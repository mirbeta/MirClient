{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressOfficeCore Library classes                        }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL     }
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

unit dxSpellCheckerCore;

{$I cxVer.inc}

interface

uses
  Types, Windows, SysUtils, Classes, Controls, Graphics, Generics.Defaults, Generics.Collections, SyncObjs,
  dxCore, dxCoreClasses, cxClasses, dxCultureInfo, dxXMLDoc;

type
  TdxSpellCheckerAutoCorrectExceptions = class;
  TdxSpellCheckerCustomAutoCorrectOptions = class;

  TdxSpellingError = (seUnknown, seMisspelling, seRepeating, seSyntax);

  { IdxSpellCheckerPosition }

  IdxSpellCheckerPosition = interface
  ['{BB627A9B-8F90-4725-BA2B-070D426638E5}']
    function Add(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
    function Compare(APosition: IdxSpellCheckerPosition): Integer;
    function Equals(AObj: TObject): Boolean;
    function MoveBackward: IdxSpellCheckerPosition;
    function MoveForward: IdxSpellCheckerPosition;
    function Subtract(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
    function ToInteger: Integer;
  end;

  { IdxSpellCheckerIgnoreList }

  IdxSpellCheckerIgnoreList = interface
  ['{2A154D04-E45A-4604-88FB-4251FF09AA1A}']
    procedure Add(const AWord: string); overload;
    procedure Add(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string); overload;
    procedure Clear;
    function Contains(const AWord: string): Boolean; overload;
    function Contains(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string): Boolean; overload;
    procedure Remove(const AWord: string); overload;
    procedure Remove(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string); overload;
  end;

  { IdxSpellChecker }

  IdxSpellChecker = interface
  ['{4515FCEE-2D09-4709-A170-E29C556355BF}']
    procedure CheckFinish;
    procedure CheckStart(AControl: TWinControl);
    procedure DrawMisspellings(AControl: TWinControl);
    function IsSpellCheckerDialogControl(AWnd: THandle): Boolean;
    procedure KeyPress(AKey: Char);
    procedure LayoutChanged(AControl: TWinControl);
    function QueryPopup(APopup: TComponent; const P: TPoint): Boolean;
    procedure SelectionChanged(AControl: TWinControl);
    procedure TextChanged(AControl: TWinControl);
    procedure Undo;
  end;

  { IdxSpellChecker2 }

  IdxSpellChecker2 = interface
  ['{0480D25C-94DA-449D-BBE6-B771C76D0BB1}']
    procedure KeyDown(AKey: Word; Shift: TShiftState);
    procedure KeyUp(AKey: Word; Shift: TShiftState);
  end;

  { IdxSpellCheckTextController }

  IdxSpellCheckTextController = interface
  ['{3FD54262-F944-467D-8067-FC644916AF3F}']
    function CreatePosition(ALogPosition: Integer): IdxSpellCheckerPosition;
    function GetNextWord(var AStart, AFinish: IdxSpellCheckerPosition): Boolean;
    function GetSentenceFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
    function GetSentenceStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
    function GetWordFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
    function GetWordStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
    function GetWord(const AStart, AFinish: IdxSpellCheckerPosition): string;
    function IsRangeEditable(const AStart, AFinish: IdxSpellCheckerPosition): Boolean;
    procedure SetSpellingArea(const AStart, AFinish: IdxSpellCheckerPosition);
  end;

  { IdxDisabledSpellChecking }

  IdxDisabledSpellChecking = interface
  ['{5578E450-1237-41F5-8085-1372A492F13B}']
  end;

  { IdxSpellCheckerAdapter }

  IdxSpellCheckerAdapter = interface
  ['{77231A5B-7883-4119-B6E1-D8A9884036AB}']
    function CreateController: IdxSpellCheckTextController;
    function GetEdit: TWinControl;
    function GetEditorHandle: THandle;
    function GetHideSelection: Boolean;
    function GetReadOnly: Boolean;
    procedure GetSelection(out AStart, AFinish: IdxSpellCheckerPosition);
    procedure GetSpellingBounds(out AStart, AFinish: IdxSpellCheckerPosition);
    procedure Post(AUpdateValue: Boolean = True);
    procedure Replace(var AStart, AFinish: IdxSpellCheckerPosition; const AWord: string;
      var ASpellingStart, ASpellingFinish: IdxSpellCheckerPosition);
    procedure SetHideSelection(AValue: Boolean);
    procedure SetSelection(const AStart, AFinish: IdxSpellCheckerPosition);
    procedure UpdateController(AController: IdxSpellCheckTextController);

    property Edit: TWinControl read GetEdit;
    property EditorHandle: THandle read GetEditorHandle;
    property HideSelection: Boolean read GetHideSelection write SetHideSelection;
    property ReadOnly: Boolean read GetReadOnly;
  end;

  { IdxSpellCheckerAutoCorrectCustomRule }

  IdxSpellCheckerAutoCorrectCustomRule = interface
  ['{6B517ADE-8B34-4FBF-91B6-BD14146A995F}']
    procedure AfterCorrect;
    procedure Undo;
  end;

  { TdxSpellCheckerAutoCorrectWordRange }

  PdxSpellCheckerAutoCorrectWordRange = ^TdxSpellCheckerAutoCorrectWordRange;
  TdxSpellCheckerAutoCorrectWordRange = record
    Replacement: string;
    SelStart: Integer;
    SelLength: Integer;
    NewSelStart: Integer;

    class operator Equal(const R1, R2: TdxSpellCheckerAutoCorrectWordRange): Boolean;
    class operator NotEqual(const R1, R2: TdxSpellCheckerAutoCorrectWordRange): Boolean;
  end;

  { TdxSpellCheckerAutoCorrectWordInfo }

  TdxSpellCheckerAutoCorrectWordInfo = record
    LastKey: Char;
    CursorLogPosition: Integer;
    SpellingAreaFinish: IdxSpellCheckerPosition;
    SpellingAreaStart: IdxSpellCheckerPosition;
    Word: string;
    WordPositionFinish: IdxSpellCheckerPosition;
    WordPositionStart: IdxSpellCheckerPosition;
    Rule: IdxSpellCheckerAutoCorrectCustomRule;

    class function Create: TdxSpellCheckerAutoCorrectWordInfo; overload; static;
    class function Create(const AWordRange: TdxSpellCheckerAutoCorrectWordRange;
      const ATextController: IdxSpellCheckTextController): TdxSpellCheckerAutoCorrectWordInfo; overload; static;
    procedure Reset;
    function ToWordRange: TdxSpellCheckerAutoCorrectWordRange;
  end;

  { TdxSpellCheckerPosition<T> }

  TdxSpellCheckerPosition<T> = class abstract(TInterfacedObject, IdxSpellCheckerPosition)
  strict private
    class function GetNull: IdxSpellCheckerPosition; static;
    class function GetUndefined: IdxSpellCheckerPosition; static;
    function GetIsZero: Boolean;
    function GetIsNegative: Boolean;
    function GetIsPositive: Boolean;
  protected
    function GetActualPosition: T; virtual; abstract;
    function GetActualPositionHashCode: Integer; virtual; abstract;
    procedure SetActualPosition(const Value: T); virtual; abstract;

    function GetZero: IdxSpellCheckerPosition; virtual;
    function Compare(APosition: IdxSpellCheckerPosition): Integer; overload; virtual; abstract;

    function MoveBackward: IdxSpellCheckerPosition; virtual; abstract;
    function MoveForward: IdxSpellCheckerPosition; virtual; abstract;

    property ActualPosition: T read GetActualPosition write SetActualPosition;
    property Zero: IdxSpellCheckerPosition read GetZero;
  public
    constructor Create(const AActualPosition: T); overload; virtual;

    function Add(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition; virtual; abstract;
    function Clone: IdxSpellCheckerPosition; virtual; abstract;
    function Subtract(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition; virtual; abstract;

    function ToInteger: Integer; virtual; abstract;
    function Equals(AObj: TObject): Boolean; overload; override;
    function GetHashCode: Integer; override;

    class function Compare(const APosition1, APosition2: IdxSpellCheckerPosition): Integer; overload; static;
    class function IsGreater(const APosition1, APosition2: IdxSpellCheckerPosition): Boolean; static;
    class function IsGreaterOrEqual(const APosition1, APosition2: IdxSpellCheckerPosition): Boolean; static;
    class function IsLess(const APosition1, APosition2: IdxSpellCheckerPosition): Boolean; static;
    class function IsLessOrEqual(const APosition1, APosition2: IdxSpellCheckerPosition): Boolean; static;

    property IsZero: Boolean read GetIsZero;
    property IsNegative: Boolean read GetIsNegative;
    property IsPositive: Boolean read GetIsPositive;

    class property Null: IdxSpellCheckerPosition read GetNull;
    class property Undefined: IdxSpellCheckerPosition read GetUndefined;
  end;

  { TdxSpellCheckerIntegerPosition }

  TdxSpellCheckerIntegerPosition = class(TdxSpellCheckerPosition<Integer>)
  protected
    FValue: Integer;

    function GetActualPosition: Integer; override;
    function GetActualPositionHashCode: Integer; override;
    procedure SetActualPosition(const Value: Integer); override;

    function Compare(APosition: IdxSpellCheckerPosition): Integer; overload; override;

    function MoveBackward: IdxSpellCheckerPosition; override;
    function MoveForward: IdxSpellCheckerPosition; override;
  public
    function Add(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition; override;
    function Clone: IdxSpellCheckerPosition; override;
    function Subtract(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition; override;
    function ToInteger: Integer; override;
  end;

  { TdxSpellCheckCustomSimpleTextController }

  TdxSpellCheckCustomSimpleTextController = class(TInterfacedObject, IdxSpellCheckTextController)
  protected
    FSpellingAreaFinish: Integer;
    FSpellingAreaStart: Integer;
    FText: string;

    function CreatePosition(AValue: Integer): IdxSpellCheckerPosition; inline;
    function DecodePosition(const AValue: IdxSpellCheckerPosition): Integer; inline;
    function DecodePositions(const AStart, AFinish: IdxSpellCheckerPosition; var AIndex, ACount: Integer): Boolean;
    function EncodePosition(const AValue: Integer): IdxSpellCheckerPosition; inline;
  public
    constructor Create(const AText: string);
    // IdxSpellCheckTextController
    function GetNextWord(var AStart, AFinish: IdxSpellCheckerPosition): Boolean; virtual; abstract;
    function GetSentenceFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; virtual; abstract;
    function GetSentenceStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; virtual; abstract;
    function GetWordFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; virtual; abstract;
    function GetWordStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; virtual; abstract;
    function GetWord(const AStart, AFinish: IdxSpellCheckerPosition): string; virtual;
    function IsRangeEditable(const AStart, AFinish: IdxSpellCheckerPosition): Boolean; virtual;
    procedure SetSpellingArea(const AStart, AFinish: IdxSpellCheckerPosition); virtual;
    //
    property Text: string read FText write FText;
  end;

  { TdxSpellCheckerReplacement }

  TdxSpellCheckerReplacement = class
  strict private
    FReplacement: string;
    FText: string;
  public
    constructor Create(const AText, AReplacement: string);
    procedure ChangeReplacement(const S: string);

    property Replacement: string read FReplacement;
    property Text: string read FText;
  end;

  { TdxSpellCheckerReplacementList }

  TdxSpellCheckerReplacementList = class(TcxObjectList)
  strict private
    FAllowDuplicates: Boolean;

    function GetItem(Index: Integer): TdxSpellCheckerReplacement;
  public
    constructor Create(AllowDuplicates: Boolean = False);
    procedure Add(const AText, AReplacement: string);
    function FindReplacement(const AText: string): TdxSpellCheckerReplacement;
    procedure SortByText;

    property Items[Index: Integer]: TdxSpellCheckerReplacement read GetItem; default;
  end;

  { TdxSpellCheckerCustomOptions }

  TdxSpellCheckerCustomOptions = class(TcxOwnedPersistent)
  strict private
    FOnChanged: TdxMulticastMethod<TNotifyEvent>;
  protected
    procedure InitializeOptions; virtual;
    procedure Changed;
    procedure DoChanged; virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure AddChangedHandler(AHandler: TNotifyEvent);
    procedure RemoveChangedHandler(AHandler: TNotifyEvent);
  end;

  { TdxSpellCheckerCustomCheckAsYouTypeOptions }

  TdxSpellCheckerUnderlineStyle = (usAuto, usWavyLine, usLine);

  TdxSpellCheckerPopupMenuItem = (scmiAddToDictionary, scmiAutoCorrect, scmiDelete,
    scmiIgnoreAll, scmiSpelling, scmiSuggestions, scmiIgnore);
  TdxSpellCheckerPopupMenuItems = set of TdxSpellCheckerPopupMenuItem;

  TdxSpellCheckerCustomCheckAsYouTypeOptions = class(TdxSpellCheckerCustomOptions)
  strict private const
    DefaultPopupMenuItems = [scmiAddToDictionary, scmiAutoCorrect, scmiDelete,
      scmiIgnoreAll, scmiSpelling, scmiSuggestions, scmiIgnore];
  strict private
    FActive: Boolean;
    FPopupMenuItems: TdxSpellCheckerPopupMenuItems;
    FSuggestionCount: Integer;
    FUnderlineColor: TColor;
    FUnderlineStyle: TdxSpellCheckerUnderlineStyle;

    procedure SetActive(AValue: Boolean);
    procedure SetPopupMenuItems(AValue: TdxSpellCheckerPopupMenuItems);
    procedure SetSuggestionCount(AValue: Integer);
    procedure SetUnderlineColor(AValue: TColor);
    procedure SetUnderlineStyle(AValue: TdxSpellCheckerUnderlineStyle);
  protected
    procedure DoActiveChanged; virtual; abstract;
    procedure DoAssign(Source: TPersistent); override;
    procedure InitializeOptions; override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property PopupMenuItems: TdxSpellCheckerPopupMenuItems read FPopupMenuItems write SetPopupMenuItems default DefaultPopupMenuItems;
    property SuggestionCount: Integer read FSuggestionCount write SetSuggestionCount default 5;
    property UnderlineColor: TColor read FUnderlineColor write SetUnderlineColor default clRed;
    property UnderlineStyle: TdxSpellCheckerUnderlineStyle read FUnderlineStyle write SetUnderlineStyle default usAuto;
  end;

  { TdxSpellCheckerAutoCorrectExceptions }

  TdxSpellCheckerAutoCorrectExceptionAddEvent = procedure (Sender: TdxSpellCheckerAutoCorrectExceptions;
    var AException: WideString; var AAllow: Boolean) of object;
  TdxSpellCheckerAutoCorrectExceptionDeleteEvent = procedure (Sender: TdxSpellCheckerAutoCorrectExceptions;
    const AException: WideString; var AAllow: Boolean) of object;

  TdxSpellCheckerAutoCorrectExceptions = class
  strict private
    FAutoInclude: Boolean;
    FCount: Integer;
    FList: PdxPWideCharArray;
    FLock: TRTLCriticalSection;
    FOptions: TdxSpellCheckerCustomAutoCorrectOptions;

    FOnAdd: TdxSpellCheckerAutoCorrectExceptionAddEvent;
    FOnDelete: TdxSpellCheckerAutoCorrectExceptionDeleteEvent;

    function GetException(Index: Integer): string;
  protected
    procedure DisposeException(AException: PWideChar);
    function DoAdd(var AException: string): Boolean;
    function DoDelete(const AException: string): Boolean;
    procedure SetCount(AValue: Integer); virtual;

    property OnAdd: TdxSpellCheckerAutoCorrectExceptionAddEvent read FOnAdd write FOnAdd;
    property OnDelete: TdxSpellCheckerAutoCorrectExceptionDeleteEvent read FOnDelete write FOnDelete;
  public
    constructor Create(AOptions: TdxSpellCheckerCustomAutoCorrectOptions); virtual;
    destructor Destroy; override;

    function Add(const AException: string): Integer;
    procedure Clear;
    procedure Delete(AIndex: Integer); overload;
    procedure Delete(const AException: string); overload;
    function Find(const AException: string): Integer;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure PopulateExceptions(AList: TStrings);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);

    property AutoInclude: Boolean read FAutoInclude write FAutoInclude;
    property Count: Integer read FCount;
    property Exceptions[Index: Integer]: string read GetException; default;
  end;

  { TdxSpellCheckerAutoCorrectReplacementList }

  TdxSpellCheckerAutoCorrectReplacementList = class(TdxSpellCheckerReplacementList)
  public
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
  end;

  { TdxSpellCheckerCustomAutoCorrectOptions }

  TdxSpellCheckerCustomAutoCorrectOptions = class(TdxSpellCheckerCustomOptions)
  strict private
    FActive: Boolean;
    FAutomaticallyUseSuggestions: Boolean;
    FCorrectCapsLock: Boolean;
    FCorrectInitialCaps: Boolean;
    FCorrectSentenceCaps: Boolean;
    FDisableCapsLock: Boolean;
    FFirstLetterExceptions: TdxSpellCheckerAutoCorrectExceptions;
    FInitialCapsExceptions: TdxSpellCheckerAutoCorrectExceptions;
    FReplacements: TdxSpellCheckerAutoCorrectReplacementList;
    FReplaceTextAsYouType: Boolean;

    procedure FirstLetterExceptionAddHandler(Sender: TdxSpellCheckerAutoCorrectExceptions; var AException: WideString; var AAllow: Boolean);
    procedure InitialCapsExceptionAddHandler(Sender: TdxSpellCheckerAutoCorrectExceptions; var AException: WideString; var AAllow: Boolean);
    procedure SetActive(const AValue: Boolean);
    procedure SetAutomaticallyUseSuggestions(const AValue: Boolean);
    procedure SetCorrectCapsLock(const AValue: Boolean);
    procedure SetCorrectInitialCaps(const AValue: Boolean);
    procedure SetCorrectSentenceCaps(const AValue: Boolean);
    procedure SetDisableCapsLock(const AValue: Boolean);
    procedure SetReplaceTextAsYouType(const AValue: Boolean);
  protected
    procedure DoActiveChanged; virtual; abstract;
    procedure DoAssign(Source: TPersistent); override;
    procedure InitializeOptions; override;
  public
    destructor Destroy; override;

    property FirstLetterExceptions: TdxSpellCheckerAutoCorrectExceptions read FFirstLetterExceptions;
    property InitialCapsExceptions: TdxSpellCheckerAutoCorrectExceptions read FInitialCapsExceptions;
    property Replacements: TdxSpellCheckerAutoCorrectReplacementList read FReplacements;
  published
    property Active: Boolean read FActive write SetActive default False;
    property AutomaticallyUseSuggestions: Boolean read FAutomaticallyUseSuggestions write SetAutomaticallyUseSuggestions default True;
    property CorrectCapsLock: Boolean read FCorrectCapsLock write SetCorrectCapsLock default True;
    property CorrectInitialCaps: Boolean read FCorrectInitialCaps write SetCorrectInitialCaps default True;
    property CorrectSentenceCaps: Boolean read FCorrectSentenceCaps write SetCorrectSentenceCaps default True;
    property DisableCapsLock: Boolean read FDisableCapsLock write SetDisableCapsLock default False;
    property ReplaceTextAsYouType: Boolean read FReplaceTextAsYouType write SetReplaceTextAsYouType default True;
  end;

  { IdxSpellingErrorInfo }

  IdxSpellingErrorInfo = interface
  ['{7AE6DEBF-8CE4-4AFA-BEC5-DBC52AB69015}']
    function GetError: TdxSpellingError;
    function GetWord: string;
    function GetWordEndPosition: IdxSpellCheckerPosition;
    function GetWordStartPosition: IdxSpellCheckerPosition;

    property Error: TdxSpellingError read GetError;
    property Word: string read GetWord;
    property WordEndPosition: IdxSpellCheckerPosition read GetWordEndPosition;
    property WordStartPosition: IdxSpellCheckerPosition read GetWordStartPosition;
  end;

  { IdxSpellChecker3 }

  IdxSpellChecker3CheckCallbackProc = reference to function (AErrorInfo: IdxSpellingErrorInfo): Boolean;

  IdxSpellChecker3 = interface
  ['{45807494-4396-401D-ADE2-6FC4312700D6}']
    function GetAutoCorrectOptions: TdxSpellCheckerCustomAutoCorrectOptions;
    function GetCheckAsYouTypeOptions: TdxSpellCheckerCustomCheckAsYouTypeOptions;

    function AutoCorrect(AControl: TWinControl; const AController: IdxSpellCheckTextController;
      var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean;
    function CheckAsync(AControl: TWinControl; const AController: IdxSpellCheckTextController;
      const AFrom, ATo: IdxSpellCheckerPosition; AProc: IdxSpellChecker3CheckCallbackProc;
      const APrevWord: string = ''): Boolean;
    procedure CheckControl(AControl: TWinControl);
    function GetSuggestions(const AWord: string): TArray<string>;
    function IsDialogCheckMode: Boolean;

    procedure AddToDictionary(const AWord: string);
    function CanAddToDictionary: Boolean;
    procedure Ignore(AControl: TControl; const AWord: string; const AStart, AEnd: IdxSpellCheckerPosition);
    procedure IgnoreAll(AControl: TControl; const AWord: string);

    procedure AddDialogCheckModeChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveDialogCheckModeChangeHandler(AEvent: TNotifyEvent);
    procedure AddSpellingOptionsChangedHandler(AEvent: TNotifyEvent);
    procedure RemoveSpellingOptionsChangedHandler(AEvent: TNotifyEvent);

    procedure RegisterIgnoreList(AControl: TControl; const AIgnoreList: IdxSpellCheckerIgnoreList);
    procedure UnregisterIgnoreList(AControl: TControl);

    property AutoCorrectOptions: TdxSpellCheckerCustomAutoCorrectOptions read GetAutoCorrectOptions;
    property CheckAsYouTypeOptions: TdxSpellCheckerCustomCheckAsYouTypeOptions read GetCheckAsYouTypeOptions;
  end;

  { IdxSpellCheckerControl }

  IdxSpellCheckerControl = interface
  ['{2DEA4CCA-3C6D-4283-9441-AABBD61F74F3}']
    function SupportsSpelling: Boolean;
    procedure SetSelText(const AText: string; APost: Boolean = False);
    procedure SetIsBarControl(AValue: Boolean);
    procedure SetValue(const AValue: Variant);
  end;

  { TdxSpellingErrorInfo }

  TdxSpellingErrorInfo = class(TInterfacedObject, IdxSpellingErrorInfo)
  strict private
    FError: TdxSpellingError;
    FWord: string;
    FWordEndPosition: IdxSpellCheckerPosition;
    FWordStartPosition: IdxSpellCheckerPosition;
  protected
    // IdxSpellingErrorInfo
    function GetError: TdxSpellingError;
    function GetWord: string;
    function GetWordEndPosition: IdxSpellCheckerPosition;
    function GetWordStartPosition: IdxSpellCheckerPosition;
  public
    constructor Create(const AWord: string; const AStart, AEnd: IdxSpellCheckerPosition; AError: TdxSpellingError);
  end;

  { TdxSpellCheckerInstance }

  TdxSpellCheckerInstance = class sealed
  strict private
    class var
      FISpellChecker: IdxSpellChecker;
      FISpellChecker2: IdxSpellChecker2;
      FISpellChecker3: IdxSpellChecker3;

      FSpellCheckerChanged: TdxMulticastMethod<TNotifyEvent>;
  public
    class procedure SetInstance(const AInstance: TObject);

    class property ISpellChecker: IdxSpellChecker read FISpellChecker;
    class property ISpellChecker2: IdxSpellChecker2 read FISpellChecker2;
    class property ISpellChecker3: IdxSpellChecker3 read FISpellChecker3;
    // Events
    class property SpellCheckerChanged: TdxMulticastMethod<TNotifyEvent> read FSpellCheckerChanged;
  end;

  { TdxSpellCheckerAdapters }

  TdxSpellCheckerCreateAdapterProc = function (AObject: TObject): IdxSpellCheckerAdapter of object;

  TdxSpellCheckerAdapters = class
  strict private
    class var
      FAdapters: TDictionary<TClass, TdxSpellCheckerCreateAdapterProc>;

    class function FindAdapter(AObject: TObject; out AProc: TdxSpellCheckerCreateAdapterProc): Boolean;
  public
    class function CreateAdapter(AObject: TObject; out AAdapter: IdxSpellCheckerAdapter): Boolean;
    class function IsSupported(AControl: TWinControl): Boolean;
    class procedure Register(AClass: TClass; AProc: TdxSpellCheckerCreateAdapterProc);
    class procedure Unregister(AClass: TClass);
  end;

implementation

uses
  Math, cxContainer, RTLConsts, cxControls;

function dxSpellCheckerContextPopupHook(ASender: TObject; APopup: TComponent; const P: TPoint): Boolean;
begin
  Result := Assigned(TdxSpellCheckerInstance.ISpellChecker) and TdxSpellCheckerInstance.ISpellChecker.QueryPopup(APopup, P);
end;

{ TdxSpellCheckerAutoCorrectWordRange }

class operator TdxSpellCheckerAutoCorrectWordRange.Equal(const R1, R2: TdxSpellCheckerAutoCorrectWordRange): Boolean;
begin
  Result := (R1.Replacement = R2.Replacement) and (R1.SelStart = R2.SelStart) and
    (R1.SelLength = R2.SelLength) and (R1.NewSelStart = R2.NewSelStart);
end;

class operator TdxSpellCheckerAutoCorrectWordRange.NotEqual(const R1, R2: TdxSpellCheckerAutoCorrectWordRange): Boolean;
begin
  Result := not (R1 = R2);
end;

{ TdxSpellCheckerAutoCorrectWordInfo }

class function TdxSpellCheckerAutoCorrectWordInfo.Create: TdxSpellCheckerAutoCorrectWordInfo;
begin
  Result.Reset;
end;

class function TdxSpellCheckerAutoCorrectWordInfo.Create(
  const AWordRange: TdxSpellCheckerAutoCorrectWordRange;
  const ATextController: IdxSpellCheckTextController): TdxSpellCheckerAutoCorrectWordInfo;
begin
  Result.Reset;
  Result.CursorLogPosition := AWordRange.NewSelStart;
  Result.WordPositionFinish := ATextController.CreatePosition(AWordRange.SelStart + AWordRange.SelLength);
  Result.WordPositionStart := ATextController.CreatePosition(AWordRange.SelStart);
  Result.Word := AWordRange.Replacement;
end;

procedure TdxSpellCheckerAutoCorrectWordInfo.Reset;
begin
  LastKey := #0;
  CursorLogPosition := 0;
  SpellingAreaFinish := nil;
  SpellingAreaStart := nil;
  Word := '';
  WordPositionFinish := nil;
  WordPositionStart := nil;
end;

{ TdxSpellCheckerPosition }

constructor TdxSpellCheckerPosition<T>.Create(const AActualPosition: T);
begin
  inherited Create;
  ActualPosition := AActualPosition;
end;

class function TdxSpellCheckerPosition<T>.GetNull: IdxSpellCheckerPosition;
begin
  Result := nil;
end;

class function TdxSpellCheckerPosition<T>.GetUndefined: IdxSpellCheckerPosition;
begin
  Result := nil;
end;

class function TdxSpellCheckerPosition<T>.Compare(const APosition1, APosition2: IdxSpellCheckerPosition): Integer;
begin
  if APosition1 <> nil then
    Exit(APosition1.Compare(APosition2))
  else
    if APosition2 <> nil then
      Exit(-APosition2.Compare(APosition1))
    else
      Exit(0);
end;

class function TdxSpellCheckerPosition<T>.IsGreater(const APosition1, APosition2: IdxSpellCheckerPosition): Boolean;
begin
  Result := Compare(APosition1, APosition2) > 0;
end;

class function TdxSpellCheckerPosition<T>.IsGreaterOrEqual(const APosition1, APosition2: IdxSpellCheckerPosition): Boolean;
begin
  Result := IsGreater(APosition1, APosition2) or (Compare(APosition1, APosition2) = 0);
end;

class function TdxSpellCheckerPosition<T>.IsLess(const APosition1, APosition2: IdxSpellCheckerPosition): Boolean;
begin
  Result := Compare(APosition1, APosition2) < 0;
end;

class function TdxSpellCheckerPosition<T>.IsLessOrEqual(const APosition1, APosition2: IdxSpellCheckerPosition): Boolean;
begin
  Result := IsLess(APosition1, APosition2) or (Compare(APosition1, APosition2) = 0);
end;

function TdxSpellCheckerPosition<T>.GetZero: IdxSpellCheckerPosition;
begin
  Result := Subtract(Self);
end;

function TdxSpellCheckerPosition<T>.GetIsZero: Boolean;
begin
  Result := Compare(Zero, Self) = 0;
end;

function TdxSpellCheckerPosition<T>.GetIsNegative: Boolean;
begin
  Result := IsLess(Self, Zero);
end;

function TdxSpellCheckerPosition<T>.GetIsPositive: Boolean;
begin
  Result := IsGreater(Self, Zero);
end;

function TdxSpellCheckerPosition<T>.Equals(AObj: TObject): Boolean;
var
  APosition: IdxSpellCheckerPosition;
begin
  if Self = AObj then
    Exit(True);
  Result := Supports(AObj, IdxSpellCheckerPosition, APosition) and (Compare(Self, APosition) = 0);
end;

function TdxSpellCheckerPosition<T>.GetHashCode: Integer;
begin
  Result := GetActualPositionHashCode;
end;

function TdxSpellCheckerAutoCorrectWordInfo.ToWordRange: TdxSpellCheckerAutoCorrectWordRange;
begin
  Result.Replacement := Word;
  Result.SelStart := WordPositionStart.ToInteger;
  Result.SelLength := WordPositionFinish.ToInteger - Result.SelStart;
  Result.NewSelStart := CursorLogPosition;
end;

{ TdxSpellCheckerIntegerPosition }

function TdxSpellCheckerIntegerPosition.Add(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
begin
  Result := TdxSpellCheckerIntegerPosition.Create(FValue + APosition.ToInteger);
end;

function TdxSpellCheckerIntegerPosition.Clone: IdxSpellCheckerPosition;
begin
  Result := TdxSpellCheckerIntegerPosition.Create(FValue);
end;

function TdxSpellCheckerIntegerPosition.Subtract(APosition: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
begin
  Result := TdxSpellCheckerIntegerPosition.Create(ActualPosition - APosition.ToInteger);
end;

function TdxSpellCheckerIntegerPosition.ToInteger: Integer;
begin
  Result := ActualPosition;
end;

function TdxSpellCheckerIntegerPosition.GetActualPosition: Integer;
begin
  Result := FValue;
end;

function TdxSpellCheckerIntegerPosition.GetActualPositionHashCode: Integer;
begin
  Result := FValue;
end;

procedure TdxSpellCheckerIntegerPosition.SetActualPosition(const Value: Integer);
begin
  FValue := Value;
end;

function TdxSpellCheckerIntegerPosition.Compare(APosition: IdxSpellCheckerPosition): Integer;
begin
  Result := FValue - APosition.ToInteger;
end;

function TdxSpellCheckerIntegerPosition.MoveBackward: IdxSpellCheckerPosition;
begin
  Result := TdxSpellCheckerIntegerPosition.Create(FValue - 1);
end;

function TdxSpellCheckerIntegerPosition.MoveForward: IdxSpellCheckerPosition;
begin
  Result := TdxSpellCheckerIntegerPosition.Create(FValue + 1);
end;

{ TdxSpellCheckCustomSimpleTextController }

constructor TdxSpellCheckCustomSimpleTextController.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

function TdxSpellCheckCustomSimpleTextController.GetWord(const AStart, AFinish: IdxSpellCheckerPosition): string;
var
  AIndex, ACount: Integer;
begin
  if DecodePositions(AStart, AFinish, AIndex, ACount) then
    Result := Copy(FText, AIndex, ACount)
  else
    Result := '';
end;

function TdxSpellCheckCustomSimpleTextController.IsRangeEditable(const AStart, AFinish: IdxSpellCheckerPosition): Boolean;
begin
  Result := True;
end;

procedure TdxSpellCheckCustomSimpleTextController.SetSpellingArea(const AStart, AFinish: IdxSpellCheckerPosition);
begin
  FSpellingAreaStart := Max(0, DecodePosition(AStart));
  FSpellingAreaFinish := Min(Max(FSpellingAreaStart, DecodePosition(AFinish)), Length(FText));
end;

function TdxSpellCheckCustomSimpleTextController.CreatePosition(AValue: Integer): IdxSpellCheckerPosition;
begin
  Result := TdxSpellCheckerIntegerPosition.Create(AValue);
end;

function TdxSpellCheckCustomSimpleTextController.DecodePosition(const AValue: IdxSpellCheckerPosition): Integer;
begin
  if AValue <> nil then
    Result := AValue.ToInteger + 1
  else
    Result := 1;
end;

function TdxSpellCheckCustomSimpleTextController.DecodePositions(
  const AStart, AFinish: IdxSpellCheckerPosition; var AIndex, ACount: Integer): Boolean;
var
  S, F: Integer;
begin
  S := DecodePosition(AStart);
  F := DecodePosition(AFinish);
  Result := (S >= 1) and (S <= F);
  if Result then
  begin
    AIndex := S;
    ACount := F - S;
  end;
end;

function TdxSpellCheckCustomSimpleTextController.EncodePosition(const AValue: Integer): IdxSpellCheckerPosition;
begin
  Result := CreatePosition(AValue - 1);
end;

{ TdxSpellCheckerReplacement }

constructor TdxSpellCheckerReplacement.Create(const AText, AReplacement: string);
begin
  inherited Create;
  FText := AText;
  FReplacement := AReplacement;
end;

procedure TdxSpellCheckerReplacement.ChangeReplacement(const S: string);
begin
  FReplacement := S;
end;

{ TdxSpellCheckerReplacementList }

constructor TdxSpellCheckerReplacementList.Create(AllowDuplicates: Boolean = False);
begin
  inherited Create;
  Capacity := 256;
  FAllowDuplicates := AllowDuplicates;
end;

procedure TdxSpellCheckerReplacementList.Add(const AText, AReplacement: string);
var
  AItem: TdxSpellCheckerReplacement;
begin
  if FAllowDuplicates then
    inherited Add(TdxSpellCheckerReplacement.Create(AText, AReplacement))
  else
  begin
    AItem := FindReplacement(AText);
    if AItem = nil then
    begin
      inherited Add(TdxSpellCheckerReplacement.Create(AText, AReplacement));
      SortByText;
    end
    else
      AItem.ChangeReplacement(AReplacement);
  end;
end;

function TdxSpellCheckerReplacementList.FindReplacement(const AText: string): TdxSpellCheckerReplacement;
var
  L, H, I, C: Integer;
begin
  Result := nil;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareText(TdxSpellCheckerReplacement(List[I]).Text, AText);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := TdxSpellCheckerReplacement(List[I]);
        Break;
      end;
    end;
  end;
end;

function TdxSpellCheckerReplacementList.GetItem(Index: Integer): TdxSpellCheckerReplacement;
begin
  Result := TdxSpellCheckerReplacement(inherited Items[Index]);
end;

function CompareReplacementByText(Item1, Item2: Pointer): Integer;
begin
  Result := WideCompareText(TdxSpellCheckerReplacement(Item1).Text, TdxSpellCheckerReplacement(Item2).Text);
end;

procedure TdxSpellCheckerReplacementList.SortByText;
begin
  Sort(CompareReplacementByText);
end;
{ TdxSpellCheckerCustomOptions }

constructor TdxSpellCheckerCustomOptions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  InitializeOptions;
end;

destructor TdxSpellCheckerCustomOptions.Destroy;
begin
  cxClearObjectLinks(Self);
  inherited Destroy;
end;

procedure TdxSpellCheckerCustomOptions.AddChangedHandler(AHandler: TNotifyEvent);
begin
  FOnChanged.Add(AHandler);
end;

procedure TdxSpellCheckerCustomOptions.RemoveChangedHandler(AHandler: TNotifyEvent);
begin
  FOnChanged.Remove(AHandler);
end;

procedure TdxSpellCheckerCustomOptions.InitializeOptions;
begin
end;

procedure TdxSpellCheckerCustomOptions.Changed;
begin
  DoChanged;
end;

procedure TdxSpellCheckerCustomOptions.DoChanged;
begin
  if not FOnChanged.Empty then
    FOnChanged.Invoke(Self);
end;

{ TdxSpellCheckerCustomCheckAsYouTypeOptions }

procedure TdxSpellCheckerCustomCheckAsYouTypeOptions.InitializeOptions;
begin
  FSuggestionCount := 5;
  FUnderlineColor := clRed;
  FUnderlineStyle := usAuto;
  FPopupMenuItems := DefaultPopupMenuItems;
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeOptions.DoAssign(Source: TPersistent);
begin
  if Source is TdxSpellCheckerCustomCheckAsYouTypeOptions then
  begin
    FSuggestionCount := TdxSpellCheckerCustomCheckAsYouTypeOptions(Source).FSuggestionCount;
    FUnderlineColor := TdxSpellCheckerCustomCheckAsYouTypeOptions(Source).FUnderlineColor;
    FUnderlineStyle := TdxSpellCheckerCustomCheckAsYouTypeOptions(Source).FUnderlineStyle;
    FPopupMenuItems := TdxSpellCheckerCustomCheckAsYouTypeOptions(Source).PopupMenuItems;
    Changed;
  end;
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeOptions.SetActive(AValue: Boolean);
begin
  if FActive <> AValue then
  begin
    FActive := AValue;
    DoActiveChanged;
    Changed;
  end;
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeOptions.SetPopupMenuItems(AValue: TdxSpellCheckerPopupMenuItems);
begin
  if AValue <> FPopupMenuItems then
  begin
    FPopupMenuItems := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeOptions.SetSuggestionCount(AValue: Integer);
begin
  if AValue <> FSuggestionCount then
  begin
    FSuggestionCount := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeOptions.SetUnderlineColor(AValue: TColor);
begin
  if FUnderlineColor <> AValue then
  begin
    FUnderlineColor := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeOptions.SetUnderlineStyle(AValue: TdxSpellCheckerUnderlineStyle);
begin
  if FUnderlineStyle <> AValue then
  begin
    FUnderlineStyle := AValue;
    Changed;
  end;
end;

{ TdxSpellCheckerAutoCorrectReplacementList }

procedure TdxSpellCheckerAutoCorrectReplacementList.LoadFromFile(const AFileName: string);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxSpellCheckerAutoCorrectReplacementList.LoadFromStream(AStream: TStream);
var
  ADocument: TdxXMLDocument;
  ANode, ARootNode: TdxXMLNode;
  I: Integer;
begin
  Clear;
  ADocument := TdxXMLDocument.Create(nil);
  try
    ADocument.LoadFromStream(AStream);
    if ADocument.FindChild('block-list:block-list', ARootNode) then
      for I := 0 to ARootNode.Count - 1 do
      begin
        ANode := ARootNode.Items[I];
        Add(
          ANode.Attributes.GetValueAsString('block-list:abbreviated-name'),
          ANode.Attributes.GetValueAsString('block-list:name'));
      end;
  finally
    ADocument.Free;
  end;
end;

procedure TdxSpellCheckerAutoCorrectReplacementList.SaveToFile(const AFileName: string);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxSpellCheckerAutoCorrectReplacementList.SaveToStream(AStream: TStream);
var
  ADocument: TdxXMLDocument;
  ANode, ARootNode: TdxXMLNode;
  I: Integer;
begin
  ADocument := TdxXMLDocument.Create(nil);
  try
    ADocument.AutoIndent := True;
    ARootNode := ADocument.AddChild('block-list:block-list');
    ARootNode.Attributes.SetValue('xmlns:block-list', 'http://www.devexpress.com');
    for I := 0 to Count - 1 do
    begin
      ANode := ARootNode.AddChild('block-list:block');
      ANode.Attributes.SetValueAsString('block-list:abbreviated-name', Items[I].Text);
      ANode.Attributes.SetValueAsString('block-list:name', Items[I].Replacement);
    end;
    ADocument.SaveToStream(AStream);
  finally
    FreeAndNil(ADocument);
  end;
end;
{ TdxSpellCheckerAutoCorrectExceptions }

constructor TdxSpellCheckerAutoCorrectExceptions.Create(AOptions: TdxSpellCheckerCustomAutoCorrectOptions);
begin
  inherited Create;
  InitializeCriticalSectionAndSpinCount(FLock, 512);
  FOptions := AOptions;
  FAutoInclude := True;
  FCount := 0;
end;

destructor TdxSpellCheckerAutoCorrectExceptions.Destroy;
begin
  Clear;
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

function TdxSpellCheckerAutoCorrectExceptions.Add(const AException: string): Integer;
var
  ALen: Integer;
  S: string;
begin
  S := AException;
  if not DoAdd(S) then
    Exit(-1);

  ALen := Length(S);
  if ALen = 0 then
    Exit(-1);

  Result := Find(S);
  if Result = -1 then
  begin
    EnterCriticalSection(FLock);
    try
      Result := Count;
      SetCount(Count + 1);
      FList[Result] := AllocMem((ALen + 1) * SizeOf(WideChar));
      cxCopyData(PWideChar(S), FList[Result], ALen * SizeOf(WideChar));
    finally
      LeaveCriticalSection(FLock);
    end;
  end;
end;

procedure TdxSpellCheckerAutoCorrectExceptions.Clear;
var
  I: Integer;
begin
  if Count = 0 then Exit;
  EnterCriticalSection(FLock);
  try
    for I := 0 to FCount - 1 do
      DisposeException(FList[I]);
    SetCount(0);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TdxSpellCheckerAutoCorrectExceptions.Delete(AIndex: Integer);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex >= Count) or (FList[AIndex] = nil) or not DoDelete(FList[AIndex]) then
    Exit;
  DisposeException(FList[AIndex]);
  for I := AIndex to Count - 2 do
    FList[I] := FList[I + 1];
  SetCount(Count - 1);
end;

procedure TdxSpellCheckerAutoCorrectExceptions.Delete(const AException: string);
begin
  Delete(Find(AException));
end;

function TdxSpellCheckerAutoCorrectExceptions.Find(const AException: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if WideCompareStr(AException, Exceptions[I]) = 0 then
    begin
      Result := I;
      Break;
    end;
end;

procedure TdxSpellCheckerAutoCorrectExceptions.LoadFromFile(const AFileName: string);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxSpellCheckerAutoCorrectExceptions.LoadFromStream(AStream: TStream);
var
  ADocument: TdxXMLDocument;
  ARootNode: TdxXMLNode;
  I: Integer;
begin
  Clear;
  ADocument := TdxXMLDocument.Create(nil);
  try
    ADocument.LoadFromStream(AStream);
    if ADocument.FindChild('block-list:block-list', ARootNode) then
    begin
      for I := 0 to ARootNode.Count - 1 do
        Add(ARootNode.Items[I].Attributes.GetValueAsString('block-list:abbreviated-name'));
    end;
  finally
    FreeAndNil(ADocument);
  end;
end;

procedure TdxSpellCheckerAutoCorrectExceptions.PopulateExceptions(AList: TStrings);
var
  I: Integer;
begin
  if AList = nil then
    Exit;
  for I := 0 to Count - 1 do
    AList.Add(Exceptions[I]);
end;

procedure TdxSpellCheckerAutoCorrectExceptions.SaveToFile(const AFileName: string);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxSpellCheckerAutoCorrectExceptions.SaveToStream(AStream: TStream);
var
  ADocument: TdxXMLDocument;
  ARootNode: TdxXMLNode;
  I: Integer;
begin
  ADocument := TdxXMLDocument.Create(nil);
  try
    ADocument.AutoIndent := True;
    ARootNode := ADocument.AddChild('block-list:block-list');
    ARootNode.Attributes.SetValue('xmlns:block-list', 'http://www.devexpress.com');
    for I := 0 to Count - 1 do
      ARootNode.AddChild('block-list:block').Attributes.SetValueAsString('block-list:abbreviated-name', Exceptions[I]);
    ADocument.SaveToStream(AStream);
  finally
    ADocument.Free;
  end;
end;

procedure TdxSpellCheckerAutoCorrectExceptions.DisposeException(AException: PWideChar);
begin
  if AException <> nil then
    FreeMem(AException, PCardinal(AException)^);
end;

function TdxSpellCheckerAutoCorrectExceptions.DoAdd(var AException: string): Boolean;
var
  S: WideString;
begin
  Result := True;
  if Assigned(OnAdd) then
  begin
    S := AException;
    OnAdd(Self, S, Result);
    AException := S;
  end;
end;

function TdxSpellCheckerAutoCorrectExceptions.DoDelete(const AException: string): Boolean;
begin
  Result := True;
  if Assigned(OnDelete) then
    OnDelete(Self, AException, Result);
end;

procedure TdxSpellCheckerAutoCorrectExceptions.SetCount(AValue: Integer);
begin
  FCount := AValue;
  if FCount > 0 then
    FList := ReallocMemory(FList, FCount * SizeOf(TdxNativeInt))
  else
    if FList <> nil then
    begin
      FreeMem(FList);
      FList := nil;
    end;
end;

function TdxSpellCheckerAutoCorrectExceptions.GetException(Index: Integer): string;
begin
  if (Index < 0) or (Index > Count - 1) then
    TList.Error(@SListIndexError, Index);
  Result := FList[Index];
end;

{ TdxSpellCheckerCustomAutoCorrectOptions }

destructor TdxSpellCheckerCustomAutoCorrectOptions.Destroy;
begin
  FreeAndNil(FFirstLetterExceptions);
  FreeAndNil(FInitialCapsExceptions);
  FreeAndNil(FReplacements);
  inherited Destroy;
end;

procedure TdxSpellCheckerCustomAutoCorrectOptions.DoAssign(Source: TPersistent);
begin
  if Source is TdxSpellCheckerCustomAutoCorrectOptions then
    with TdxSpellCheckerCustomAutoCorrectOptions(Source) do
    begin
      Self.FActive := Active;
      Self.FAutomaticallyUseSuggestions := AutomaticallyUseSuggestions;
      Self.FCorrectSentenceCaps := CorrectSentenceCaps;
      Self.FCorrectCapsLock := CorrectCapsLock;
      Self.FCorrectInitialCaps := CorrectInitialCaps;
      Self.FDisableCapsLock := DisableCapsLock;
      Self.FReplaceTextAsYouType := ReplaceTextAsYouType;
    end;
  inherited DoAssign(Source);
end;

procedure TdxSpellCheckerCustomAutoCorrectOptions.InitializeOptions;
begin
  FActive := False;
  FAutomaticallyUseSuggestions := True;
  FCorrectSentenceCaps := True;
  FCorrectCapsLock := True;
  FCorrectInitialCaps := True;
  FDisableCapsLock := False;
  FReplaceTextAsYouType := True;
  FFirstLetterExceptions := TdxSpellCheckerAutoCorrectExceptions.Create(Self);
  FFirstLetterExceptions.OnAdd := FirstLetterExceptionAddHandler;
  FInitialCapsExceptions := TdxSpellCheckerAutoCorrectExceptions.Create(Self);
  FInitialCapsExceptions.OnAdd := InitialCapsExceptionAddHandler;
  FReplacements := TdxSpellCheckerAutoCorrectReplacementList.Create;
end;

procedure TdxSpellCheckerCustomAutoCorrectOptions.FirstLetterExceptionAddHandler(
  Sender: TdxSpellCheckerAutoCorrectExceptions; var AException: WideString; var AAllow: Boolean);
var
  ACorrectException: Boolean;
  ALen, I: Integer;
begin
  AAllow := False;
  ALen := Length(AException);
  if ALen > 0 then
  begin
    ACorrectException := True;
    for I := 1 to ALen do
    begin
      ACorrectException := dxWideIsAlpha(AException[I]);
      if I = ALen then
        ACorrectException := ACorrectException or (AException[I] = '.');
      if not ACorrectException then
        Break;
    end;
    if ACorrectException then
    begin
      if AException[ALen] <> '.' then
        AException := AException + '.';
      AException := WideLowerCase(AException);
      AAllow := True;
    end;
  end;
end;

procedure TdxSpellCheckerCustomAutoCorrectOptions.InitialCapsExceptionAddHandler(
  Sender: TdxSpellCheckerAutoCorrectExceptions; var AException: WideString; var AAllow: Boolean);
var
  ALen, I: Integer;
  S: WideString;
begin
  AAllow := False;
  AException := WideLowerCase(AException);
  ALen := Length(AException);
  if ALen > 2 then
  begin
    for I := 1 to ALen do
    begin
      AAllow := dxWideIsAlpha(AException[I]);
      if not AAllow then
        Break;
    end;
    if AAllow then
    begin
      S := WideUpperCase(AException);
      AException[1] := S[1];
      AException[2] := S[2];
    end;
  end;
end;

procedure TdxSpellCheckerCustomAutoCorrectOptions.SetActive(const AValue: Boolean);
begin
  if FActive <> AValue then
  begin
    FActive := AValue;
    DoActiveChanged;
    Changed;
  end;
end;

procedure TdxSpellCheckerCustomAutoCorrectOptions.SetAutomaticallyUseSuggestions(const AValue: Boolean);
begin
  if FAutomaticallyUseSuggestions <> AValue then
  begin
    FAutomaticallyUseSuggestions := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerCustomAutoCorrectOptions.SetCorrectCapsLock(const AValue: Boolean);
begin
  if FCorrectCapsLock <> AValue then
  begin
    FCorrectCapsLock := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerCustomAutoCorrectOptions.SetCorrectInitialCaps(const AValue: Boolean);
begin
  if FCorrectInitialCaps <> AValue then
  begin
    FCorrectInitialCaps := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerCustomAutoCorrectOptions.SetCorrectSentenceCaps(const AValue: Boolean);
begin
  if FCorrectSentenceCaps <> AValue then
  begin
    FCorrectSentenceCaps := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerCustomAutoCorrectOptions.SetDisableCapsLock(const AValue: Boolean);
begin
  if FDisableCapsLock <> AValue then
  begin
    FDisableCapsLock := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerCustomAutoCorrectOptions.SetReplaceTextAsYouType(const AValue: Boolean);
begin
  if FReplaceTextAsYouType <> AValue then
  begin
    FReplaceTextAsYouType := AValue;
    Changed;
  end;
end;

{ TdxSpellingErrorInfo }

constructor TdxSpellingErrorInfo.Create(const AWord: string;
  const AStart, AEnd: IdxSpellCheckerPosition; AError: TdxSpellingError);
begin
  inherited Create;
  FWord := AWord;
  FWordStartPosition := AStart;
  FWordEndPosition := AEnd;
  FError := AError;
end;

function TdxSpellingErrorInfo.GetError: TdxSpellingError;
begin
  Result := FError;
end;

function TdxSpellingErrorInfo.GetWord: string;
begin
  Result := FWord;
end;

function TdxSpellingErrorInfo.GetWordEndPosition: IdxSpellCheckerPosition;
begin
  Result := FWordEndPosition;
end;

function TdxSpellingErrorInfo.GetWordStartPosition: IdxSpellCheckerPosition;
begin
  Result := FWordStartPosition;
end;

{ TdxSpellCheckerInstance }

class procedure TdxSpellCheckerInstance.SetInstance(const AInstance: TObject);
begin
  Supports(AInstance, IdxSpellChecker, FISpellChecker);
  Supports(AInstance, IdxSpellChecker2, FISpellChecker2);
  Supports(AInstance, IdxSpellChecker3, FISpellChecker3);
  if not SpellCheckerChanged.Empty then
    SpellCheckerChanged.Invoke(AInstance);
end;

{ TdxSpellCheckerAdapters }

class function TdxSpellCheckerAdapters.CreateAdapter(AObject: TObject; out AAdapter: IdxSpellCheckerAdapter): Boolean;
var
  AProc: TdxSpellCheckerCreateAdapterProc;
begin
  Result := FindAdapter(AObject, AProc);
  if Result then
    AAdapter := AProc(AObject);
end;

class function TdxSpellCheckerAdapters.IsSupported(AControl: TWinControl): Boolean;
var
  AIntf: IdxSpellCheckerControl;
  AProc: TdxSpellCheckerCreateAdapterProc;
begin
  Result := Assigned(AControl) and AControl.HandleAllocated and FindAdapter(AControl, AProc);
  if Result then
  begin
    if Supports(GetInnerControlContainer(AControl), IdxSpellCheckerControl, AIntf) then
      Result := AIntf.SupportsSpelling;
  end;
end;

class procedure TdxSpellCheckerAdapters.Register(AClass: TClass; AProc: TdxSpellCheckerCreateAdapterProc);
begin
  if FAdapters = nil then
    FAdapters := TDictionary<TClass, TdxSpellCheckerCreateAdapterProc>.Create;
  FAdapters.Add(AClass, AProc);
end;

class procedure TdxSpellCheckerAdapters.Unregister(AClass: TClass);
begin
  if FAdapters <> nil then
  begin
    FAdapters.Remove(AClass);
    if FAdapters.Count = 0 then
      FreeAndNil(FAdapters);
  end;
end;

class function TdxSpellCheckerAdapters.FindAdapter(AObject: TObject; out AProc: TdxSpellCheckerCreateAdapterProc): Boolean;
var
  AClassType: TClass;
begin
  Result := False;
  if FAdapters <> nil then
  begin
    AClassType := AObject.ClassType;
    while AClassType <> nil do
    begin
      if FAdapters.TryGetValue(AClassType, AProc) then
        Exit(True);
      AClassType := AClassType.ClassParent;
    end;
  end;
end;

initialization
  cxContextPopupHookFunc := dxSpellCheckerContextPopupHook;
finalization
  cxContextPopupHookFunc := nil;
end.
