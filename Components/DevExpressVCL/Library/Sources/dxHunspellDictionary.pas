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

unit dxHunspellDictionary;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, dxSpellCheckerUtils, dxHunspellWords, dxHunspellAffixes,
  dxSpellChecker, dxHunspellTypes;

const
  dxMaxGuessCount = 200;
  dxMaxWordStemCount  = 100;

type
  TdxSpellInfoItem = (siiCompound, siiForbidden, siiInitialCapitalization);
  TdxSpellInfo = set of TdxSpellInfoItem;

  { TdxHunspellDictionary }

  TdxHunspellDictionary = class(TdxCustomSpellCheckerDictionary)
  strict private
    FAffixManager: TdxHunspellAffixManager;
    FComplexPrefixes: Boolean;
    FDictionaryPath: TFileName;
    FDictionaryStream: TStream;
    FFreeStreams: Boolean;
    FGrammarAlphabet: string;
    FGrammarPath: TFileName;
    FGrammarStream: TStream;
    FLoadingFromStreams: Boolean;
    FNgramDistance: Integer;
    FSuggestionsLiteMode: Boolean;
    FWordStemManager: TdxHunspellWordStemManager;

    function CheckCompoundWord(const AWord: PWideChar; AWordLength: Integer; var AInfo: TdxSpellInfo): TdxHunspellWordStem;
    function CheckForbiddenAndCompoundOnly(var AWordStem: TdxHunspellWordStem; var AInfo: TdxSpellInfo): Boolean;
    function CheckWord(const AWord: PWideChar; var AInfo: TdxSpellInfo): TdxHunspellWordStem;
    function GetAbbreviationWordStem(const AWord: PWideChar; AWordLength: Integer;
      var AInfo: TdxSpellInfo; ACapitalizationType: TdxCapitalizationType = ctLower): TdxHunspellWordStem;
    function InputConverting(ACleanedWord, AWordForCompound: PWideChar; const ASource: PWideChar;
      out ACapitalizationType: TdxCapitalizationType; out AIsAbbreviation: Boolean): Integer;
    function IsNumber(AWord: PWideChar; AWordLength: Integer): Boolean;
    function LoadDictionary: Boolean;
    procedure LoadGrammar;
    function SpellSharps(ABase, APosition: PWideChar; AIndex, ANumRepeat: Integer; var AInfo: TdxSpellInfo): TdxHunspellWordStem;
    //setters
    procedure SetDictionaryPath(const AValue: TFileName);
    procedure SetGrammarPath(const AValue: TFileName);
  protected
    function CleanWord(ADest: PWideChar; ASource: PWideChar;
      out ACapitalizationType: TdxCapitalizationType; out AAbbreviationCount: Integer): Integer;

    procedure AfterLoad; override;
    procedure BeforeLoad; override;
    function CanLoad: Boolean; override;
    function CreateSuggestionBuilder: TdxSpellCheckerSuggestionBuilder; override;
    function DoLoad: Boolean; override;
    function DoUnload: Boolean; override;
    procedure FreeContent; override;
    function GetActiveAlphabet: string; override;
    function GetDictionaryStream: TStream; virtual;
    function GetDisplayName: string; override;
    function GetGrammarStream: TStream; virtual;
    function ProcessApostrophe(AWord: PWideChar;
      var AWordStem: TdxHunspellWordStem; var AInfo: TdxSpellInfo): Boolean;
    function ProcessSharps(AWord: PWideChar; AWordLength: Integer; AIsAbbreviation: Boolean;
      var AWordStem: TdxHunspellWordStem; var AInfo: TdxSpellInfo): Boolean;
    function ProcessWordBreak(const AWord: PWideChar; var AInfo: TdxSpellInfo): Boolean;
    function Spell(const AWord: PWideChar; var AInfo: TdxSpellInfo): Boolean;
    procedure UpdateWordChars(var AWordChars: string); override;

    property AffixManager: TdxHunspellAffixManager read FAffixManager;
    property WordStemManager: TdxHunspellWordStemManager read FWordStemManager;
  public
    constructor Create(ASpellChecker: TdxCustomSpellChecker); override;
    procedure Assign(Source: TPersistent); override;
    function HasWord(const AWord: string): Boolean; override;
    procedure LoadFromStreams(AGrammarStream, ADictionaryStream: TStream;
      AFreeStreams: Boolean; AMode: TdxSpellCheckerDictionaryLoadMode = dlmDefault);
  published
    property DictionaryPath: TFileName read FDictionaryPath write SetDictionaryPath;
    property Enabled;
    property GrammarPath: TFileName read FGrammarPath write SetGrammarPath;
    property NgramDistance: Integer read FNgramDistance write FNgramDistance default 0;
    property SuggestionsLiteMode: Boolean read FSuggestionsLiteMode write FSuggestionsLiteMode default True;

    property Language;
    property OnLoaded;
    property OnLoading;
  end;

  { TdxHunspellSuggestionBuilder }

  TdxNgramOption = (ngoLowering, ngoLongerWorse, ngoAnyMismatch);
  TdxNgramOptions = set of TdxNgramOption;
  //guess
  TdxGuessArray = array[0..dxMaxGuessCount - 1] of PWideChar;
  PdxGuessArray = ^TdxGuessArray;
  TdxGuessScoreArray = array [0..dxMaxGuessCount - 1] of Integer;
  //word stems
  TdxWordStemArray = array [0..dxMaxWordStemCount - 1] of TdxHunspellWordStem;
  TdxWordStemScoreArray = array [0..dxMaxWordStemCount - 1] of Integer;
  TdxPhoneticWordStemArray = array [0..dxMaxWordStemCount - 1] of PWideChar;
  TdxPhoneticWordStemScoreArray = array [0..dxMaxWordStemCount - 1] of Integer;

  TdxHunspellSuggestionBuilder = class(TdxNearMissStrategy)
  private
    FAbbreviationCount: Integer;
    FCapitalizationType: TdxCapitalizationType;
    FCheckCompound: Boolean;
    FMaxNgramSuggestionCount: Integer;
    procedure AddNgramSuggestions(AWord: PWideChar; AWordLength: Integer; const AWordStems: TdxWordStemArray);
    procedure AddPhoneticSuggestions(AWord: PWideChar; AWordLength: Integer;
      var APhoneticWordStems: TdxPhoneticWordStemArray; var APhoneticScores: TdxPhoneticWordStemScoreArray);
    procedure CalculateScores(AWord: PWideChar; AWordLength: Integer;
      out AWordStems: TdxWordStemArray; out AWordStemScores: TdxWordStemScoreArray;
      out APhoneticWordStems: TdxPhoneticWordStemArray; out APhoneticScores: TdxPhoneticWordStemScoreArray);
    procedure CheckDashedWord(AWord: PWideChar; AWordLength: Integer);
    procedure CheckHungarianCompounding(ACursor: PWideChar; const ACandidate: TdxLineBuffer;
      ACheck1, ACheck2: Integer);
    function CheckWord(const AWord: PWideChar; ALength: Integer; ACheckCompound: Boolean): Integer;
    function CommonCharacterPositions(S1, S2: PWideChar; out AHasSwap: Boolean): Integer;
    procedure GenerateNgramGuessTables(AWord: PWideChar; AWordLength: Integer;
      var AGuess, AGuessOrig: TdxGuessArray; var AGuessScore: TdxGuessScoreArray;
      const AWordStems: TdxWordStemArray);
    procedure GenerateNearMissSuggestions(AWord: PWideChar;
      var AExistCapitalizedWords, AOnlyCompoundSuggestions: Boolean);
    procedure GenerateNgramSuggestions(AWord: PWideChar;
      var AExistCapitalizedWords, AOnlyCompoundSuggestions: Boolean);
    function GetAffixManager: TdxHunspellAffixManager; inline;
    function GetDictionary: TdxHunspellDictionary; inline;
    function GetPhoneTable: TdxPhoneTable; inline;
    function GetWordStemManager: TdxHunspellWordStemManager; inline;
    function HasSuggestionWithDash: Boolean;
    procedure HungarianReplaceDash;
    function IsForbidden(const AWord: PWideChar): Boolean;
    function LeftCommonSubStringLength(AWord1, AWord2: PWideChar): Integer;
    function MakeSecondWordCapitalized(AWord: PWideChar): PWideChar;
    function Ngram(S1, S2: PWideChar; N: Integer; const AOptions: TdxNgramOptions): Integer;
    procedure NgramSuggestion(AWord: PWideChar);
    procedure ProcessMap(AWord, ACandidate: PWideChar; AWordIndex, ACandidateIndex: Integer; AMapTable: TdxMapTable);
    procedure RemoveBadSuggestions;
    procedure ReplaceTableSuggestion;
    procedure Sort(S1, S2: PdxGuessArray; AWeights: PIntegerArray; ACount: Integer);
    function Trigram(S1, S2: PWideChar; const AOptions: TdxNgramOptions): Integer;
  protected
    procedure CheckBadKey;
    procedure CheckCapitalize;
    procedure CheckCharMapping;
    procedure CheckTwoWords;
    procedure DoubleInterchangeTwoLetters;

    procedure AddSuggestion(AWord: PWideChar; ADistance: Integer = 2);
    function CanAddToSuggestions(const ATestWord: string): Boolean; override;
    procedure DoAddSuggestions; override;
    procedure DoNearMissStrategy(AWord: PWideChar; var AOnlyCompoundSuggestions: Boolean);
    procedure InsertSuggestion(AWord: PWideChar; ASuggestions: TdxSpellCheckerSuggestionList = nil);
    function IsCaseSensitive: Boolean; override;
    function PrepareWord(const AWord: string): string; override;
    function Spell(AWord: PWideChar): Boolean;
    procedure Suggest(AWord: PWideChar);
    function SuggestionExist(AWord: PWideChar): Boolean;

    property MaxNgramSuggestionCount: Integer read FMaxNgramSuggestionCount;
    property PhoneTable: TdxPhoneTable read GetPhoneTable;
  public
    constructor Create(ADictionary: TdxCustomSpellCheckerDictionary); override;
    property AffixManager: TdxHunspellAffixManager read GetAffixManager;
    property Dictionary: TdxHunspellDictionary read GetDictionary;
    property WordStemManager: TdxHunspellWordStemManager read GetWordStemManager;
  end;

implementation

uses
  Windows, Math, dxCore, cxClasses, dxSpellCheckerStrs, dxHunspellUtils;

const
  dxMaxPhoneticSuggestionCount = 2;
  dxMaxSharpCount              = 5;
  dxMaxNgramSuggestionCount    = 4;

{ TdxHunspellDictionary }

constructor TdxHunspellDictionary.Create(ASpellChecker: TdxCustomSpellChecker);
begin
  inherited Create(ASpellChecker);
  FSuggestionsLiteMode := True;
end;

procedure TdxHunspellDictionary.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxHunspellDictionary then
  begin
    DictionaryPath := TdxHunspellDictionary(Source).DictionaryPath;
    GrammarPath := TdxHunspellDictionary(Source).GrammarPath;
    NgramDistance := TdxHunspellDictionary(Source).NgramDistance;
    SuggestionsLiteMode := TdxHunspellDictionary(Source).SuggestionsLiteMode;
  end;
end;

function TdxHunspellDictionary.HasWord(const AWord: string): Boolean;
var
  AInfo: TdxSpellInfo;
begin
  Result := (WordStemManager <> nil) and (AffixManager <> nil);
  if Result then
  begin
    AInfo := [];
    Result := Spell(PWideChar(AWord), AInfo);
  end;
end;

procedure TdxHunspellDictionary.LoadFromStreams(AGrammarStream, ADictionaryStream: TStream;
  AFreeStreams: Boolean;
  AMode: TdxSpellCheckerDictionaryLoadMode = dlmDefault);
begin
  if (AGrammarStream = nil) or (ADictionaryStream = nil) then
    Exit;
  FFreeStreams := AFreeStreams;
  FGrammarStream := AGrammarStream;
  FDictionaryStream := ADictionaryStream;
  Load(AMode);
end;

procedure TdxHunspellDictionary.AfterLoad;
begin
  if FLoadingFromStreams and FFreeStreams then
  begin
    FGrammarStream.Free;
    FDictionaryStream.Free;
  end;
  FGrammarStream := nil;
  FDictionaryStream := nil;
  FLoadingFromStreams := False;
end;

procedure TdxHunspellDictionary.BeforeLoad;
begin
  FLoadingFromStreams := (FGrammarStream <> nil) and (FDictionaryStream <> nil);
end;

function TdxHunspellDictionary.CanLoad: Boolean;
begin
  Result := inherited CanLoad and
    (not FLoadingFromStreams or FileExists(DictionaryPath) and FileExists(GrammarPath));
end;

function TdxHunspellDictionary.CreateSuggestionBuilder: TdxSpellCheckerSuggestionBuilder;
begin
  Result := TdxHunspellSuggestionBuilder.Create(Self);
end;

function TdxHunspellDictionary.DoLoad: Boolean;
begin
  dxHunspellWarningFlag := False;
  FWordStemManager := TdxHunspellWordStemManager.Create(Language);
  try
    FAffixManager := TdxHunspellAffixManager.Create(WordStemManager);
    LoadGrammar;
    Result := LoadDictionary;
  except
    FreeContent;
    raise;
  end;
  if Result then
  begin
    CodePage := WordStemManager.CodePage;
    Language := WordStemManager.Language;
    AffixManager.Language := Language;
    FGrammarAlphabet := AffixManager.TryChars;
    FComplexPrefixes := AffixManager.ComplexPrefixes;
  end
  else
    FreeContent;
end;

function TdxHunspellDictionary.DoUnload: Boolean;
begin
  FreeContent;
  Result := True;
end;

procedure TdxHunspellDictionary.FreeContent;
begin
  FreeAndNil(FAffixManager);
  FreeAndNil(FWordStemManager);
end;

function TdxHunspellDictionary.GetActiveAlphabet: string;
begin
  Result := FGrammarAlphabet;
end;

function TdxHunspellDictionary.GetDictionaryStream: TStream;
begin
  Result := FDictionaryStream;
  if Result = nil then
    Result := TFileStream.Create(DictionaryPath, fmOpenRead or fmShareDenyWrite);
end;

function TdxHunspellDictionary.GetGrammarStream: TStream;
begin
  Result := FGrammarStream;
  if Result = nil then
    Result := TFileStream.Create(GrammarPath, fmOpenRead or fmShareDenyWrite);
end;

function TdxHunspellDictionary.GetDisplayName: string;
var
  AFileName: TFileName;
begin
  Result := inherited GetDisplayName;
  AFileName := SysUtils.ExtractFileName(DictionaryPath);
  if AFileName <> '' then
    Result := Format('%s (%s)', [Result, AFileName]);
end;

function TdxHunspellDictionary.Spell(const AWord: PWideChar;
  var AInfo: TdxSpellInfo): Boolean;
var
  AWordStem: TdxHunspellWordStem;
  ACleanedWord, ACandidate: TdxLineBuffer;
  AWordLength: Integer;
  ACapitalizationType: TdxCapitalizationType;
  AIsAbbreviation, ADone: Boolean;
begin
  if (WordStemManager = nil) or (AffixManager = nil) or (StrLen(AWord) >= dxMaxWordLength) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  AWordLength := InputConverting(ACleanedWord, ACandidate, AWord, ACapitalizationType, AIsAbbreviation);
  if (AWordLength = 0) or IsNumber(ACleanedWord, AWordLength) then
    Exit;
  if ACapitalizationType in [ctMixed, ctMixedCapitalized, ctLower] then
  begin
    AWordStem := CheckWord(ACleanedWord, AInfo);
    if AIsAbbreviation and (AWordStem = nil) then
      AWordStem := GetAbbreviationWordStem(ACleanedWord, AWordLength, AInfo);
  end
  else
  begin
    if ACapitalizationType = ctUpper then
    begin
      AWordStem := CheckWord(ACleanedWord, AInfo);
      if AWordStem <> nil then
        Exit;
      if AIsAbbreviation then
      begin
        AWordStem := GetAbbreviationWordStem(ACleanedWord, AWordLength, AInfo);
        if AWordStem <> nil then
          Exit;
      end;
      if ProcessApostrophe(ACleanedWord, AWordStem, AInfo) then
        Exit;
      if ProcessSharps(ACleanedWord, AWordLength, AIsAbbreviation, AWordStem, AInfo) then
        Exit;
    end;
    MakeAllSmall(ACleanedWord, Language, AWordLength);
    Move(ACleanedWord, ACandidate, (AWordLength + 1) * SizeOf(WideChar));
    MakeCapitalized(ACleanedWord, Language);
    if ACapitalizationType = ctCapitalized then
      Include(AInfo, siiInitialCapitalization);
    AWordStem := CheckWord(ACleanedWord, AInfo);
    if ACapitalizationType = ctCapitalized then
      Exclude(AInfo, siiInitialCapitalization);
    ADone := siiForbidden in AInfo;
    if ADone then
      AWordStem := nil
    else
    begin
      if (AWordStem <> nil) and (ACapitalizationType = ctUpper) and AWordStem.KeepCase then
        AWordStem := nil;
      if AWordStem <> nil then
        Exit;
      AWordStem := CheckWord(ACandidate, AInfo);
      if AIsAbbreviation and (AWordStem = nil) then
      begin
        AWordStem := GetAbbreviationWordStem(ACandidate, AWordLength, AInfo);
        if AWordStem = nil then
        begin
          AWordStem := GetAbbreviationWordStem(ACleanedWord, AWordLength, AInfo, ACapitalizationType);
          if (AWordStem <> nil) and AWordStem.KeepCase and (ACapitalizationType = ctUpper) then
            AWordStem := nil;
          ADone := True;
        end;
      end;
      if not ADone and (AWordStem <> nil) and AWordStem.KeepCase and
        ((ACapitalizationType = ctUpper) or not (AffixManager.CheckSharps and (StrScan(ACandidate, #$00DF) <> nil))) then
          AWordStem := nil;
    end;
  end;
  Result := (AWordStem <> nil) or ProcessWordBreak(ACleanedWord, AInfo);
end;

procedure TdxHunspellDictionary.UpdateWordChars(var AWordChars: string);
var
  P: PWideChar;
begin
  P := AffixManager.WordChars;
  if P <> nil then
    while P^ <> #0 do
    begin
      if WideCharPos(P^, AWordChars) = 0 then
        AWordChars := AWordChars + P^;
      Inc(P);
    end;
end;

function TdxHunspellDictionary.ProcessApostrophe(AWord: PWideChar;
  var AWordStem: TdxHunspellWordStem; var AInfo: TdxSpellInfo): Boolean;
var
  AApostropheCursor: PWideChar;
begin
  AApostropheCursor := StrScan(AWord, '''');
  if AApostropheCursor <> nil then
  begin
    Result := True;
    MakeAllSmall(AWord, Language);
    MakeCapitalized(AApostropheCursor + 1, Language);
    AWordStem := CheckWord(AWord, AInfo);
    if AWordStem <> nil then
      Exit;
    MakeCapitalized(AWord, Language);
    AWordStem := CheckWord(AWord, AInfo);
    if AWordStem <> nil then
      Exit;
  end;
  Result := False;
end;

function TdxHunspellDictionary.ProcessSharps(AWord: PWideChar;
  AWordLength: Integer; AIsAbbreviation: Boolean; var AWordStem: TdxHunspellWordStem;
  var AInfo: TdxSpellInfo): Boolean;
var
  ACandidate: TdxLineBuffer;
begin
  if AffixManager.CheckSharps and (StrPos(AWord, 'SS') <> nil) then
  begin
    MakeAllSmall(AWord, Language, AWordLength);
    Move(AWord^, ACandidate, (AWordLength + 1) * SizeOf(WideChar));
    AWordStem := SpellSharps(ACandidate, ACandidate, 0, 0, AInfo);
    if AWordStem = nil then
    begin
      MakeCapitalized(AWord, Language);
      AWordStem := SpellSharps(AWord, AWord, 0, 0, AInfo);
    end;
    if AIsAbbreviation and (AWordStem = nil) then
    begin
      (ACandidate + AWordLength)^ := '.';
      (ACandidate + AWordLength + 1)^ := #0;
      AWordStem := SpellSharps(ACandidate, ACandidate, 0, 0, AInfo);
      if AWordStem = nil then
      begin
        Move(AWord^, ACandidate, AWordLength * SizeOf(WideChar));
        (ACandidate + AWordLength)^ := '.';
        (ACandidate + AWordLength + 1)^ := #0;
        AWordStem := SpellSharps(ACandidate, ACandidate, 0, 0, AInfo);
      end;
    end;
    Result := AWordStem <> nil;
  end
  else
    Result := False;
end;

function TdxHunspellDictionary.ProcessWordBreak(const AWord: PWideChar;
  var AInfo: TdxSpellInfo): Boolean;
var
  I, AWordLength, AWordBreakItemLength: Integer;
  ALocalInfo: TdxSpellInfo;
  ABrakePos, AWordBreakItem: PWideChar;
  ATempChar: WideChar;
  ABreakTable: TdxBreakTable;

  function IsProcessHungarianDash: Boolean;
  var
    ATempChar: WideChar;
  begin
    Result := False;
    if AffixManager.IsHungarian and StrEquals(AWordBreakItem, '-') then
    begin
      ATempChar := (ABrakePos + 1)^;
      (ABrakePos + 1)^ := #0;
      if Spell(AWord, AInfo) then
        Result := True;
      (ABrakePos + 1)^ := ATempChar;
    end;
  end;

begin
  ABreakTable := AffixManager.BreakTable;
  Result := ABreakTable.Count > 0;
  if not Result then
    Exit;
  AWordLength := StrLen(AWord);
  for I := 0 to ABreakTable.Count - 1 do
  begin
    AWordBreakItem := ABreakTable[I];
    AWordBreakItemLength := StrLen(AWordBreakItem);
    if (AWordBreakItemLength = 1) or (AWordBreakItemLength > AWordLength) then
      Continue;
    ALocalInfo := [];
    if (AWordBreakItem^ = '^') and (StrLComp(AWord, AWordBreakItem + 1, AWordBreakItemLength - 1) = 0) and
       Spell(AWord + AWordBreakItemLength - 1, ALocalInfo) then
      Exit;
    if ((AWordBreakItem + AWordBreakItemLength - 1)^ = '$') and
      (StrLComp(AWord + AWordLength - AWordBreakItemLength + 1, AWordBreakItem, AWordBreakItemLength - 1) = 0) then
    begin
      ATempChar := AWord[AWordLength - AWordBreakItemLength + 1];
      AWord[AWordLength - AWordBreakItemLength + 1] := #0;
      ALocalInfo := [];
      if Spell(AWord, ALocalInfo) then
        Exit;
      AWord[AWordLength - AWordBreakItemLength + 1] := ATempChar;
    end;
  end;
  for I := 0 to ABreakTable.Count - 1 do
  begin
    AWordBreakItem := ABreakTable[I];
    AWordBreakItemLength := StrLen(AWordBreakItem);
    ABrakePos := StrPos(AWord, AWordBreakItem);
    if (ABrakePos <> nil) and (ABrakePos > AWord) and
      (ABrakePos < AWord + AWordLength - AWordBreakItemLength) then
    begin
      ALocalInfo := [];
      if not Spell(ABrakePos + AWordBreakItemLength, ALocalInfo) then
        Continue;
      ATempChar := ABrakePos^;
      ABrakePos^ := #0;
      ALocalInfo := [];
      if Spell(AWord, ALocalInfo) then
        Exit;
      ABrakePos^ := ATempChar;
      if IsProcessHungarianDash then
        Exit;
    end;
  end;
  Result := False;
end;

function TdxHunspellDictionary.CheckCompoundWord(const AWord: PWideChar;
  AWordLength: Integer; var AInfo: TdxSpellInfo): TdxHunspellWordStem;
var
  ATempWord: TdxLineBuffer;
begin
  if AffixManager.IsCompoundWordsAvailable then
  begin
    Result := AffixManager.CompoundCheck(AWord, 0, 0, 100, 0, nil, False, False);
    if (Result = nil) and AffixManager.IsHungarian and ((AWord + AWordLength - 1)^ = '-') then
    begin
      Move(AWord^, ATempWord, AWordLength * SizeOf(WideChar));
      (ATempWord + AWordLength - 1)^ := #0;
      Result := AffixManager.CompoundCheck(ATempWord, -5, 0, 100, 0, nil, True, False);
    end;
    if Result <> nil then
      Include(AInfo, siiCompound);
  end
  else
    Result := nil;
end;

function TdxHunspellDictionary.CheckForbiddenAndCompoundOnly(var AWordStem: TdxHunspellWordStem;
  var AInfo: TdxSpellInfo): Boolean;
begin
  Result := (AWordStem <> nil) and AWordStem.Forbidden;
  if Result then
  begin
    Include(AInfo, siiForbidden);
    if AffixManager.IsHungarian and AWordStem.Compound then
      Include(AInfo, siiCompound);
  end;
end;

function TdxHunspellDictionary.CheckWord(const AWord: PWideChar;
  var AInfo: TdxSpellInfo): TdxHunspellWordStem;
var
  AWordLength: Integer;
  ACandidate: TdxLineBuffer;
  ATakenSuffix: TdxSuffix;
  ATakenPrefix: TdxPrefix;
begin
  StrCopy(ACandidate, AWord);
  RemoveIgnoredChars(ACandidate, WordStemManager.IgnoredChars);
  if FComplexPrefixes then
    StrReverse(ACandidate);
  Result := WordStemManager.Lookup(ACandidate);
  if CheckForbiddenAndCompoundOnly(Result, AInfo) then
  begin
    Result := nil;
    Exit;
  end;
  while (Result <> nil) and (Result.NeedAffix or Result.InCompoundOnly or
     ((siiInitialCapitalization in AInfo) and Result.UpcaseOnly)) do
    Result := Result.NextHomonym;
  if Result = nil then
  begin
    AWordLength := StrLen(ACandidate);
    Result := AffixManager.AffixCheck(ACandidate, AWordLength, ATakenPrefix, ATakenSuffix, 0);
    if (Result <> nil) and
      (Result.InCompoundOnly or ((siiInitialCapitalization in AInfo) and Result.UpcaseOnly)) then
      Result := nil;
    if Result <> nil then
    begin
      if Result.Forbidden then
      begin
        Include(AInfo, siiForbidden);
        Result := nil;
      end;
    end
    else
      Result := CheckCompoundWord(ACandidate, AWordLength, AInfo);
  end;
end;

function TdxHunspellDictionary.SpellSharps(ABase, APosition: PWideChar; AIndex, ANumRepeat: Integer;
  var AInfo: TdxSpellInfo): TdxHunspellWordStem;
var
  ATestWord: TdxLineBuffer;
  AOffset: Integer;
begin
  Result := nil;
  APosition := StrPos(APosition, 'ss');
  if (APosition <> nil) and (AIndex < dxMaxSharpCount) then
  begin
    AOffset := APosition - ABase;
    StrLCopy(ATestWord, ABase, AOffset);
    ATestWord[AOffset] := #$00DF;
    StrCopy(ATestWord + AOffset + 1, APosition + 2);
    Result := SpellSharps(ATestWord, ATestWord + AOffset + 1, AIndex + 1, ANumRepeat + 1, AInfo);
    if Result = nil then
      Result := SpellSharps(ABase, APosition + 2, AIndex + 1, ANumRepeat, AInfo);
  end
  else
    if ANumRepeat > 0 then
      Result := CheckWord(ABase, AInfo);
end;

function TdxHunspellDictionary.InputConverting(ACleanedWord, AWordForCompound: PWideChar;
  const ASource: PWideChar; out ACapitalizationType: TdxCapitalizationType;
  out AIsAbbreviation: Boolean): Integer;
var
  AAbbreviationCount: Integer;
begin
  if AffixManager.InputConvertTable.Convert(ASource, AWordForCompound) then
    Result := CleanWord(ACleanedWord, AWordForCompound, ACapitalizationType, AAbbreviationCount)
  else
    Result := CleanWord(ACleanedWord, ASource, ACapitalizationType, AAbbreviationCount);
  AIsAbbreviation := AAbbreviationCount <> 0;
end;

function TdxHunspellDictionary.IsNumber(AWord: PWideChar; AWordLength: Integer): Boolean;
type
  TNumberSeparatorState = (nssNone, nssNumber, nssSeparatedNumber);
var
  I: Integer;
  ASeparatorState: TNumberSeparatorState;
begin
  Result := True;
  ASeparatorState := nssNone;
  for I := 0 to AWordLength - 1 do
  begin
    if IsDigit(AWord[I]) then
      ASeparatorState := nssNumber
    else
      if (AWord[I] = ',') or (AWord[I] = '.') or (AWord[I] = '-') then
      begin
        if (ASeparatorState = nssSeparatedNumber) or (I = 0) then
        begin
          Result := False;
          Break;
        end;
        ASeparatorState := nssSeparatedNumber;
      end
      else
      begin
        Result := False;
        Break;
      end;
  end;
  Result := Result and (ASeparatorState = nssNumber);
end;

function TdxHunspellDictionary.LoadDictionary: Boolean;
var
  ADictionaryStream: TStream;
begin
  ADictionaryStream := GetDictionaryStream;
  try
    Result := (ADictionaryStream <> nil) and
      FWordStemManager.Load(ADictionaryStream);
  finally
    if FDictionaryStream <> ADictionaryStream then
      FreeAndNil(ADictionaryStream);
  end;
  if Result then
    FAffixManager.UpdateWordStemOptions;
end;

procedure TdxHunspellDictionary.LoadGrammar;
var
  AGrammarStream: TStream;
begin
  AGrammarStream := GetGrammarStream;
  try
    FAffixManager.Load(AGrammarStream);
  finally
    if FGrammarStream <> AGrammarStream then
      FreeAndNil(AGrammarStream);
  end;
end;

function TdxHunspellDictionary.CleanWord(ADest: PWideChar; ASource: PWideChar;
  out ACapitalizationType: TdxCapitalizationType; out AAbbreviationCount: Integer): Integer;
begin
  while (ASource^ <> #0) and IsSpace(ASource^) do
    Inc(ASource);
  Result := StrLen(ASource);
  while (Result > 0) and IsSpace((ASource + Result - 1)^) do
    Dec(Result);
  AAbbreviationCount := 0;
  while (Result > 0) and ((ASource + Result - 1)^ = '.') do
  begin
    Dec(Result);
    Inc(AAbbreviationCount);
  end;
  if Result = 0 then
  begin
    ACapitalizationType := ctLower;
    ADest^ := #0;
    Exit;
  end;
  Move(ASource^, ADest^, Result * SizeOf(WideChar));
  (ADest + Result)^ := #0;
  ACapitalizationType := GetWordCapitalizationType(ADest, Result, AffixManager.Language);
end;

function TdxHunspellDictionary.GetAbbreviationWordStem(const AWord: PWideChar; AWordLength: Integer;
  var AInfo: TdxSpellInfo; ACapitalizationType: TdxCapitalizationType = ctLower): TdxHunspellWordStem;
var
  ACandidate: TdxLineBuffer;
begin
  Move(AWord^, ACandidate, AWordLength * SizeOf(WideChar));
  (ACandidate + AWordLength)^ := '.';
  (ACandidate + AWordLength + 1)^ := #0;
  if ACapitalizationType = ctCapitalized then
    Include(AInfo, siiInitialCapitalization);
  Result := CheckWord(ACandidate, AInfo);
  if ACapitalizationType = ctCapitalized then
    Exclude(AInfo, siiInitialCapitalization);
end;

procedure TdxHunspellDictionary.SetDictionaryPath(const AValue: TFileName);
begin
  if FDictionaryPath <> AValue then
  begin
    Unload;
    FDictionaryPath := AValue;
  end;
end;

procedure TdxHunspellDictionary.SetGrammarPath(const AValue: TFileName);
begin
  if FGrammarPath <> AValue then
  begin
    Unload;
    FGrammarPath := AValue;
  end;
end;

{ TdxHunspellSuggestionBuilder }

constructor TdxHunspellSuggestionBuilder.Create(ADictionary: TdxCustomSpellCheckerDictionary);
begin
  inherited Create(ADictionary);
  FMaxNgramSuggestionCount := dxMaxNgramSuggestionCount;
  if AffixManager.MaxNgramSuggestionCount >= 0 then
    FMaxNgramSuggestionCount := AffixManager.MaxNgramSuggestionCount;
end;

procedure TdxHunspellSuggestionBuilder.RemoveBadSuggestions;

  function SkipSuggestion(ASuggestion: TdxSpellCheckerSuggestion): Boolean;
  begin
    Result := (ASuggestion.Dictionary <> Dictionary) or
      (StrScan(PWideChar(ASuggestion.Word), ' ') <> nil);
  end;

var
  ACandidate: TdxLineBuffer;
  ASuggestion: TdxSpellCheckerSuggestion;
  I: Integer;
begin
  Suggestions.RemoveDuplicates;
  if (AffixManager.KeepCase <> 0) or (AffixManager.ForbiddenWordFlag <> 0) then
    if FCapitalizationType in [ctCapitalized, ctUpper] then
    begin
      I := 0;
      while I < Suggestions.Count do
      begin
        ASuggestion := Suggestions[I];
        if SkipSuggestion(ASuggestion) then
        begin
          Inc(I);
          Continue;
        end;
        if not Spell(PWideChar(ASuggestion.Word)) then
        begin
          //check other capitalized forms
          StrCopy(ACandidate, PWideChar(ASuggestion.Word));
          MakeAllSmall(ACandidate, AffixManager.Language);
          if Spell(ACandidate) then
            ASuggestion.Word := PWideChar(@ACandidate)
          else
          begin
            MakeCapitalized(ACandidate, AffixManager.Language);
            if Spell(ACandidate) then
              ASuggestion.Word := PWideChar(@ACandidate)
            else
            begin
              Suggestions.Delete(I);
              Dec(I);
            end;
          end;
        end;
        Inc(I);
      end;
    end;
end;

procedure TdxHunspellSuggestionBuilder.CheckBadKey;
var
  ACandidate: TdxLineBuffer;
  AChar, AUpChar: WideChar;
  APosition: PWideChar;
  I: Integer;
begin
  Move(Word[1], ACandidate, (Length(Word) + 1) * SizeOf(WideChar));
  for I := 0 to Length(Word) - 1 do
  begin
    AChar := Word[I + 1];
    AUpChar := WideUpCase(AChar, AffixManager.Language);
    if AChar <> AUpChar then
    begin
      ACandidate[I] := AUpChar;
      CheckAddToSuggestions(PWideChar(@ACandidate));
      ACandidate[I] := AChar;
    end;
    APosition := StrScan(AffixManager.KeyboardString, AChar);
    while APosition <> nil do
    begin
      if (APosition > AffixManager.KeyboardString) and ((APosition - 1)^ <> '|') then
      begin
        ACandidate[I] := (APosition - 1)^;
        CheckAddToSuggestions(PWideChar(@ACandidate));
      end;
      if ((APosition + 1)^ <> '|') and ((APosition + 1)^ <> #0) then
      begin
        ACandidate[I] := (APosition + 1)^;
        CheckAddToSuggestions(PWideChar(@ACandidate));
      end;
      APosition := StrScan(APosition + 1, AChar);
    end;
    ACandidate[I] := AChar;
  end;
end;

procedure TdxHunspellSuggestionBuilder.CheckCapitalize;
var
  ACandidate: TdxLineBuffer;
begin
  Move(Word[1], ACandidate, (Length(Word) + 1) * SizeOf(WideChar));
  MakeAllCapital(ACandidate, AffixManager.Language);
  CheckAddToSuggestions(ACandidate);
end;

procedure TdxHunspellSuggestionBuilder.CheckCharMapping;
var
  ACandidate: TdxLineBuffer;
begin
  if (Length(Word) < 2) or (AffixManager.MapTable.Count = 0) then Exit;
  StrCopy(ACandidate, PWideChar(Word));
  ProcessMap(PWideChar(Word), ACandidate, 0, 0, AffixManager.MapTable);
end;

procedure TdxHunspellSuggestionBuilder.ProcessMap(AWord, ACandidate: PWideChar;
  AWordIndex, ACandidateIndex: Integer; AMapTable: TdxMapTable);
var
  I, J, K, ACharSetLength : Integer;
  AChar: WideChar;
  AInMap: Boolean;
  ACharSet: PWideChar;
begin
  AChar := (AWord + AWordIndex)^;
  if AChar = #0 then
  begin
    ACandidate[ACandidateIndex] := #0;
    CheckAddToSuggestions(ACandidate);
  end
  else
  begin
    AInMap := False;
    for I := 0 to AMapTable.Count - 1 do
    begin
      ACharSet := AMapTable[I].CharacterSet;
      ACharSetLength := StrLen(ACharSet);
      for J := 0 to ACharSetLength - 1 do
      begin
        if AChar = (ACharSet + J)^ then
        begin
          AInMap := True;
          for K := 0 to ACharSetLength - 1 do
          begin
            ACandidate[ACandidateIndex] := (ACharSet + K)^;
            ACandidate[ACandidateIndex + 1] := #0;
            ProcessMap(AWord, ACandidate, AWordIndex + 1, StrLen(ACandidate), AMapTable);
            if IsTimeOut then
              Exit;
          end;
        end;
      end;
    end;
    if not AInMap then
    begin
      ACandidate[ACandidateIndex] := (AWord + AWordIndex)^;
      ProcessMap(AWord, ACandidate, AWordIndex + 1, ACandidateIndex + 1, AMapTable);
    end;
  end;
end;

procedure TdxHunspellSuggestionBuilder.CheckTwoWords;
var
  ACandidate: TdxLineBuffer;
  AWordLength: Integer;
  ABreakPos: PWideChar;
  AForbidden: Boolean;
  ACheck1, ACheck2: Integer;
begin
  AWordLength := Length(Word);
  if AWordLength < 3 then Exit;
  AForbidden := True;
  if AffixManager.IsHungarian then
    AForbidden := IsForbidden(PWideChar(Word));
  StrCopy(PWideChar(@ACandidate) + 1, PWideChar(Word));
  ABreakPos := @ACandidate;
  Inc(ABreakPos);
  while (ABreakPos + 1)^ <> #0 do
  begin
    (ABreakPos - 1)^ := ABreakPos^;
    ABreakPos^ := #0;
    ACheck1 := CheckWord(ACandidate, StrLen(ACandidate), FCheckCompound);
    if ACheck1 <> 0 then
    begin
      ACheck2 := CheckWord(ABreakPos + 1, StrLen(ABreakPos + 1), FCheckCompound);
      if ACheck2 <> 0 then
      begin
        ABreakPos^ := ' ';
        if not AForbidden then
          CheckHungarianCompounding(ABreakPos, ACandidate, ACheck1, ACheck2);
        if not SuggestionExist(ACandidate) then
          AddSuggestion(ACandidate);
        if (AffixManager.TryChars <> nil) and ((StrScan(AffixManager.TryChars, 'a') <> nil) or
          (StrScan(AffixManager.TryChars, '-') <> nil)) and
          (StrLen(ABreakPos + 1) > 1) and (StrLen(ACandidate) - StrLen(ABreakPos) > 1) then
        begin
          ABreakPos^ := '-';
          if not SuggestionExist(ACandidate) then
            AddSuggestion(ACandidate);
        end;
      end;
    end;
    Inc(ABreakPos);
  end;
end;

procedure TdxHunspellSuggestionBuilder.DoubleInterchangeTwoLetters;
var
  ACandidate: TdxLineBuffer;
  AWordLength: Integer;
begin
  AWordLength := Length(Word);
  if AWordLength in [4, 5] then
  begin
    ACandidate[0] := Word[2];
    ACandidate[1] := Word[1];
    ACandidate[2] := Word[3];
    ACandidate[AWordLength - 2] := Word[AWordLength];
    ACandidate[AWordLength - 1] := Word[AWordLength - 1];
    ACandidate[AWordLength] := #0;
    CheckAddToSuggestions(ACandidate);
    if AWordLength = 5 then
    begin
      ACandidate[0] := Word[1];
      ACandidate[1] := Word[3];
      ACandidate[2] := Word[2];
      CheckAddToSuggestions(ACandidate);
    end;
  end;
end;

procedure TdxHunspellSuggestionBuilder.AddSuggestion(AWord: PWideChar; ADistance: Integer = 2);
begin
  Suggestions.Add(AWord, Dictionary, ADistance);
end;

function TdxHunspellSuggestionBuilder.CanAddToSuggestions(const ATestWord: string): Boolean;
begin
  Result := not SuggestionExist(PWideChar(ATestWord)) and
    (CheckWord(PWideChar(ATestWord), Length(ATestWord), FCheckCompound) <> 0);
end;

procedure TdxHunspellSuggestionBuilder.DoAddSuggestions;
var
  AWord: TdxLineBuffer;
begin
  Move(Word[1], AWord, (Length(Word) + 1) * SizeOf(WideChar));
  Suggest(AWord);
end;

procedure TdxHunspellSuggestionBuilder.DoNearMissStrategy(AWord: PWideChar; var AOnlyCompoundSuggestions: Boolean);
var
  I: Boolean;
  ASavedWord: string;
  ANoCompoundTwoWords: Boolean;
begin
  ASavedWord := Word;
  Word := AWord;
  ANoCompoundTwoWords := False;
  for I := False to True do
  begin
    if ANoCompoundTwoWords then
      Break;
    FCheckCompound := I;
    CheckCapitalize;
    ReplaceTableSuggestion;
    CheckCharMapping;
    InterchangeTwoLetters;
    DoubleInterchangeTwoLetters;
    LongInterchangeTwoLetters;
    CheckBadKey;
    if not FCheckCompound and (Suggestions.Count > 0) then
      ANoCompoundTwoWords := True;
    CheckDeleteLetter;
    CheckInsertLetter;
    CheckMoveChar;
    CheckChangeOneLetter;
    CheckDoubleTwoChars;
    CheckTwoWords;
  end;
  if not ANoCompoundTwoWords and (Suggestions.Count > 0) then
    AOnlyCompoundSuggestions := True;
  Word := ASavedWord;
  FCheckCompound := False;
end;

procedure TdxHunspellSuggestionBuilder.InsertSuggestion(AWord: PWideChar; ASuggestions: TdxSpellCheckerSuggestionList = nil);
begin
  if ASuggestions = nil then
    ASuggestions := Suggestions;
  ASuggestions.Insert(0, AWord, Dictionary, 2);
end;

function TdxHunspellSuggestionBuilder.IsCaseSensitive: Boolean;
begin
  Result := True;
end;

function TdxHunspellSuggestionBuilder.PrepareWord(const AWord: string): string;
var
  ACleanedWord: TdxLineBuffer;
  P: PWideChar;
begin
  Result := AWord;
  UniqueString(Result);
  P := PWideChar(Result);
  if AffixManager.InputConvertTable.Convert(P, ACleanedWord) then
    Dictionary.CleanWord(P, ACleanedWord, FCapitalizationType, FAbbreviationCount)
  else
    Dictionary.CleanWord(P, P, FCapitalizationType, FAbbreviationCount);
end;

procedure TdxHunspellSuggestionBuilder.Suggest(AWord: PWideChar);
var
  AOnlyCompoundSuggestions, AExistCapitalizedWords: Boolean;
  I, AWordLength: Integer;
  ACandidate: TdxLineBuffer;
begin
  AWordLength := StrLen(AWord);
  if (AWordLength = 0) or (AWordLength >= dxMaxWordLength) then
    Exit;
  AOnlyCompoundSuggestions := False;
  AExistCapitalizedWords := False;
  GenerateNearMissSuggestions(AWord, AExistCapitalizedWords, AOnlyCompoundSuggestions);
  GenerateNgramSuggestions(AWord, AExistCapitalizedWords, AOnlyCompoundSuggestions);
  StrCopy(ACandidate, AWord);
  CheckDashedWord(ACandidate, AWordLength);
  if AffixManager.ComplexPrefixes then
    for I := 0 to Suggestions.Count - 1 do
      StrReverse(PWideChar(Suggestions[I].Word));
  if AExistCapitalizedWords then
    for I := 0 to Suggestions.Count - 1 do
      MakeCapitalized(PWideChar(Suggestions[I].Word), AffixManager.Language);
  if (FAbbreviationCount <> 0) and AffixManager.SuggestionsWithDots then
    for I := 0 to Suggestions.Count - 1 do
      StrCat(PWideChar(Suggestions[I].Word), AWord + AWordLength - FAbbreviationCount);
  RemoveBadSuggestions;
  if AffixManager.OutputConvertTable.Count > 0 then
    for I := 0 to Suggestions.Count - 1 do
      if AffixManager.OutputConvertTable.Convert(PWideChar(Suggestions[I].Word), ACandidate) then
        Suggestions[I].Word := PWideChar(@ACandidate);
end;

function TdxHunspellSuggestionBuilder.SuggestionExist(AWord: PWideChar): Boolean;
var
  I: Integer;
begin
  for I := 0 to Suggestions.Count - 1 do
    if StrEquals(AWord, PWideChar(Suggestions[I].Word)) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

procedure TdxHunspellSuggestionBuilder.Sort(S1, S2: PdxGuessArray;
  AWeights: PIntegerArray; ACount: Integer);
var
  I, J, AWeight: Integer;
  ATempWord: PWideChar;
begin
  I := 1;
  while I < ACount do
  begin
    J := I;
    while J > 0 do
    begin
      if AWeights[J - 1] < AWeights[J] then
      begin
        AWeight := AWeights[J - 1];
        ATempWord := S1[J - 1];
        AWeights[J - 1] := AWeights[J];
        S1[J - 1] := S1[J];
        AWeights[J] := AWeight;
        S1[J] := ATempWord;
        if S2 <> nil then
        begin
          ATempWord := S2[J - 1];
          S2[J - 1] := S2[J];
          S2[J] := ATempWord;
        end;
        Dec(J);
      end
      else
        Break;
    end;
    Inc(I);
  end;
end;

function TdxHunspellSuggestionBuilder.Trigram(S1, S2: PWideChar;
  const AOptions: TdxNgramOptions): Integer;
begin
  Result := Ngram(S1, S2, 3, AOptions);
end;

procedure TdxHunspellSuggestionBuilder.CheckDashedWord(AWord: PWideChar;
  AWordLength: Integer);
var
  I, ASuggestionCount: Integer;
  ATempWord: TdxLineBuffer;
  ADashPos, AWordPos: PWideChar;
  ADone: Boolean;
  ASaveSuggestions: TdxSpellCheckerSuggestionList;
begin
  ADashPos := StrScan(AWord, '-');
  if (ADashPos <> nil) and not HasSuggestionWithDash then
  begin
    ASaveSuggestions := Suggestions;
    Suggestions := TdxSpellCheckerSuggestionList.Create;
    try
      AWordPos := AWord;
      repeat
        ADone := ADashPos^ = #0;
        ADashPos^ := #0;
        ASuggestionCount := Suggestions.Count;
        if not Spell(AWordPos) then
        begin
          Suggest(AWordPos);
          for I := Suggestions.Count - 1 downto ASuggestionCount do
          begin
            StrLCopy(ATempWord, AWord, AWordPos - AWord);
            StrCopy(ATempWord + Integer(AWordPos - AWord), PWideChar(Suggestions[I].Word));
            if not ADone then
            begin
              StrCat(ATempWord, '-');
              StrCat(ATempWord, ADashPos + 1);
            end;
            InsertSuggestion(ATempWord, ASaveSuggestions);
            Suggestions.Delete(I);
          end;
          Break;
        end;
        if not ADone then
        begin
          ADashPos^ := '-';
          AWordPos := ADashPos + 1;
          ADashPos := StrScan(AWordPos, '-');
          if ADashPos = nil then
            ADashPos := AWord + AWordLength;
        end;
      until ADone;
    finally
      Suggestions.Free;
      Suggestions := ASaveSuggestions;
    end;
  end;
end;

procedure TdxHunspellSuggestionBuilder.CheckHungarianCompounding(
  ACursor: PWideChar; const ACandidate: TdxLineBuffer; ACheck1, ACheck2: Integer);

  function MultiplyCompounding: Boolean;
  begin
    Result := (ACheck1 = 3) and (ACheck2 >= 2);
  end;

begin
  if AffixManager.IsHungarian and
     (((ACursor[-1] = ACursor[1]) and (((ACursor > PWideChar(@ACandidate) + 1) and
     (ACursor[-1] = ACursor[-2])) or (ACursor[-1] = ACursor[2]))) or MultiplyCompounding) then
    ACursor^ := '-';
end;

function TdxHunspellSuggestionBuilder.CheckWord(const AWord: PWideChar;
  ALength: Integer; ACheckCompound: Boolean): Integer;
var
  AWordStem: TdxHunspellWordStem;
  ANoSuffix: Boolean;
  ATempPrefix: TdxPrefix;
  ATempSuffix: TdxSuffix;
begin
  Result := 0;
  ANoSuffix := False;
  if ACheckCompound then
  begin
    if AffixManager.IsCompoundWordsAvailable and
       (AffixManager.CompoundCheck(AWord, 0, 0, 100, 0, nil, False, True) <> nil) then
      Result := 3;
    Exit;
  end;
  AWordStem := AffixManager.Lookup(AWord);
  if AWordStem <> nil then
  begin
    if AffixManager.IsForbiddenOrNoSuggestWord(AWordStem, True) then
      Exit;
    while AWordStem <> nil do
    begin
      if AWordStem.NeedAffix or AWordStem.UpcaseOnly or AWordStem.InCompoundOnly then
        AWordStem := AWordStem.NextHomonym
      else
        Break;
    end;
  end
  else
    AWordStem := AffixManager.ProcessPrefixCheck(AWord, ALength, ATempPrefix, ATempSuffix, cwpNone);
  if AWordStem <> nil then
    ANoSuffix := True
  else
    AWordStem := AffixManager.ProcessSuffixCheck(AWord, ALength, [], nil, ATempSuffix);
  if (AWordStem = nil) and AffixManager.CompatibleFlagsExist then
  begin
    AWordStem := AffixManager.ProcessTwoSuffixCheck(AWord, ALength, [], nil, ATempSuffix);
    if AWordStem = nil then
      AWordStem := AffixManager.ProcessPrefixWithSuffixesCheck(AWord, ALength,
        ATempPrefix, ATempSuffix, cwpFirst);
  end;
  if (AWordStem <> nil) and (AWordStem.UpcaseOnly or
     AffixManager.IsForbiddenOrNoSuggestWord(AWordStem, True) or AWordStem.InCompoundOnly) then
    Exit;
  if AWordStem <> nil then
  begin
    if AWordStem.Compound then
      Result := 2 + Ord(ANoSuffix)
    else
      Result := 1;
  end;
end;

function TdxHunspellSuggestionBuilder.CommonCharacterPositions(S1, S2: PWideChar;
  out AHasSwap: Boolean): Integer;
var
  ADiffCount, L: Integer;
  ADiffChars: array[0..1, 0..1] of WideChar;
  ATempWord: TdxLineBuffer;
  P2: PWideChar;
begin
  Result := 0;
  ADiffCount := 0;
  StrCopy(ATempWord, S2);
  if AffixManager.ComplexPrefixes then
  begin
    L := StrLen(ATempWord);
    ATempWord[L - 1] := WideLowCase(ATempWord[L - 1], WordStemManager.Language);
  end
  else
    MakeAllSmall(ATempWord, WordStemManager.Language);
  P2 := @ATempWord;
  while (S1^ <> #0) and (P2^ <> #0) do
  begin
    if S1^ = P2^ then
      Inc(Result)
    else
    begin
      if ADiffCount < 2 then
      begin
        ADiffChars[ADiffCount, 0] := S1^;
        ADiffChars[ADiffCount, 1] := P2^;
      end;
      Inc(ADiffCount);
    end;
    Inc(S1);
    Inc(P2);
  end;
  AHasSwap := (ADiffCount = 2) and (S1^ = #0) and (P2^ = #0) and
    (ADiffChars[0, 0] = ADiffChars[1, 1]) and (ADiffChars[0, 1] = ADiffChars[1, 0]);
end;

procedure TdxHunspellSuggestionBuilder.GenerateNgramGuessTables(AWord: PWideChar;
  AWordLength: Integer; var AGuess, AGuessOrig: TdxGuessArray; var AGuessScore: TdxGuessScoreArray;
  const AWordStems: TdxWordStemArray);
var
  AMorphology: PWideChar;
  AWordStemList: PdxGuessWordArray;
  AIsSwap: Boolean;
  I, J, K, AThreshold, AIndex, ALength, ASwapCharacterWeight, ACount, AScore,
  ALongestCommonSubsequenceLength, AMinScore, AEqualCharacterPositionsWeight: Integer;
  ACandidate, AField: TdxLineBuffer;
begin
  AThreshold := 0;
  StrCopy(ACandidate, AWord);
  for I := 1 to 3 do
  begin
    J := I;
    while J < AWordLength do
    begin
      (ACandidate + J)^ := '*';
      Inc(J, 4);
    end;
    AThreshold := AThreshold + Ngram(AWord, ACandidate, AWordLength, [ngoAnyMismatch, ngoLowering]);
  end;
  AThreshold := (AThreshold div 3) - 1;
  for I := 0 to dxMaxGuessCount - 1 do
    AGuessScore[I] := -100 * I;
  FillChar(AGuess, SizeOf(AGuess), 0);
  FillChar(AGuessOrig, SizeOf(AGuessOrig), 0);
  AIndex := dxMaxGuessCount - 1;
  AWordStemList := AllocMem(SizeOf(TdxGuessWord) * dxMaxWordStemCount);
  try
    for I := 0 to dxMaxWordStemCount - 1 do
    begin
      if AWordStems[I] <> nil then
      begin
        AMorphology := nil;
        if AWordStems[I].MorphologyPhone then
          AMorphology := CopyMorphology(AField, AWordStems[I].MorphologicalDescription, mlPhon);
        ACount := AffixManager.ExpandWordStem(AWordStemList, dxMaxWordStemCount, AWordStems[I].WordStem,
          AWordStems[I].WordStemLength, AWordStems[I].AffixFlags, AWord, AWordLength, AMorphology);
        for J := 0 to ACount - 1 do
        begin
          AScore := Ngram(AWord, AWordStemList[J].Word, AWordLength, [ngoAnyMismatch, ngoLowering]) +
            LeftCommonSubStringLength(AWord, AWordStemList[J].Word);
          if (AScore > AThreshold) and (AScore > AGuessScore[AIndex]) then
          begin
            if AGuess[AIndex] <> nil then
            begin
              StrFreeAndNil(AGuess[AIndex]);
              StrFreeAndNil(AGuessOrig[AIndex]);
            end;
            AGuessScore[AIndex] := AScore;
            AGuess[AIndex] := AWordStemList[J].Word;
            AGuessOrig[AIndex] := AWordStemList[J].Orig;
            AMinScore := AScore;
            for K := 0 to dxMaxGuessCount - 1 do
              if AGuessScore[K] < AMinScore then
              begin
                AIndex := K;
                AMinScore := AGuessScore[K];
              end;
            Continue;
          end;
          StrFreeAndNil(AWordStemList[J].Word);
          StrFreeAndNil(AWordStemList[J].Orig);
        end;
      end;
      if IsTimeOut then
        Break;
    end;
  finally
    FreeMem(AWordStemList, SizeOf(TdxGuessWord) * dxMaxWordStemCount);
  end;
  Sort(@AGuess, @AGuessOrig, @AGuessScore, dxMaxGuessCount);
  for I := 0 to dxMaxGuessCount - 1 do
  begin
    if AGuess[I] <> nil then
    begin
      StrCopy(ACandidate, AGuess[I]);
      MakeAllSmall(ACandidate, WordStemManager.Language, AWordLength);
      ALength := StrLen(AGuess[I]);
      ALongestCommonSubsequenceLength := Similarity.GetLongestCommonSubsequenceLength(AWord, ACandidate);
      if (AWordLength = ALength) and (AWordLength = ALongestCommonSubsequenceLength) then
      begin
        Inc(AGuessScore[I], 2000);
        Break;
      end;
      AEqualCharacterPositionsWeight := 0;
      if ALongestCommonSubsequenceLength = CommonCharacterPositions(AWord, ACandidate, AIsSwap) then
        AEqualCharacterPositionsWeight := 1;
      ASwapCharacterWeight := 0;
      if AIsSwap then
        ASwapCharacterWeight := 1000;
      Inc(AGuessScore[I], 2 * ALongestCommonSubsequenceLength - Abs(AWordLength - ALength) +
        LeftCommonSubStringLength(AWord, ACandidate) + AEqualCharacterPositionsWeight + ASwapCharacterWeight);
    end;
  end;
  Sort(@AGuess, @AGuessOrig, @AGuessScore, dxMaxGuessCount);
end;

procedure TdxHunspellSuggestionBuilder.GenerateNearMissSuggestions(AWord: PWideChar;
  var AExistCapitalizedWords, AOnlyCompoundSuggestions: Boolean);
var
  I, AShift, ASuggestionCount: Integer;
  ACandidate: TdxLineBuffer;
  APosition, ATempSuggestion: PWideChar;
  ADotCapitalizationType: TdxCapitalizationType;
begin
  StrCopy(ACandidate, AWord);
  case FCapitalizationType of
    ctLower:
      DoNearMissStrategy(AWord, AOnlyCompoundSuggestions);
    ctCapitalized:
      begin
        AExistCapitalizedWords := True;
        DoNearMissStrategy(AWord, AOnlyCompoundSuggestions);
        MakeAllSmall(ACandidate, AffixManager.Language);
        DoNearMissStrategy(ACandidate, AOnlyCompoundSuggestions);
      end;
    ctMixedCapitalized, ctMixed:
      begin
        if FCapitalizationType = ctMixedCapitalized then
          AExistCapitalizedWords := True;
        DoNearMissStrategy(AWord, AOnlyCompoundSuggestions);
        APosition := StrScan(AWord, '.');
        if (APosition <> nil) and (APosition > AWord) then
        begin
          ADotCapitalizationType := GetWordCapitalizationType(APosition + 1,
            StrLen(APosition + 1), AffixManager.Language);
          if ADotCapitalizationType = ctCapitalized then
          begin
            ACandidate[APosition - AWord + 1] := ' ';
            StrCopy(@ACandidate[APosition - AWord + 2], APosition + 1);
            InsertSuggestion(ACandidate);
            StrCopy(ACandidate, AWord);
          end;
        end;
        if FCapitalizationType = ctMixedCapitalized then
        begin
          ACandidate[0] := WideLowCase(ACandidate[0], AffixManager.Language);
          DoNearMissStrategy(ACandidate, AOnlyCompoundSuggestions);
          StrCopy(ACandidate, AWord);
        end;
        MakeAllSmall(ACandidate, AffixManager.Language);
        if Spell(ACandidate) then
          InsertSuggestion(ACandidate);
        ASuggestionCount := Suggestions.Count;
        DoNearMissStrategy(ACandidate, AOnlyCompoundSuggestions);
        if FCapitalizationType = ctMixedCapitalized then
        begin
          MakeCapitalized(ACandidate, AffixManager.Language);
          if Spell(ACandidate) then
            InsertSuggestion(ACandidate);
          DoNearMissStrategy(ACandidate, AOnlyCompoundSuggestions);
        end;
        if ASuggestionCount < Suggestions.Count then
        begin
          ATempSuggestion := MakeSecondWordCapitalized(PWideChar(Suggestions[ASuggestionCount].Word));
          Suggestions[ASuggestionCount].Word := ATempSuggestion;
          StrDispose(ATempSuggestion);
        end;
      end;
    ctUpper:
      begin
        MakeAllSmall(ACandidate, AffixManager.Language);
        DoNearMissStrategy(ACandidate, AOnlyCompoundSuggestions);
        if (AffixManager.KeepCase <> 0) and Spell(ACandidate) then
          InsertSuggestion(ACandidate);
        MakeCapitalized(ACandidate, AffixManager.Language);
        DoNearMissStrategy(ACandidate, AOnlyCompoundSuggestions);
        for I := 0 to Suggestions.Count - 1 do
        begin
          MakeAllCapital(PWideChar(Suggestions[I].Word), AffixManager.Language);
          if AffixManager.CheckSharps then
          begin
            APosition := StrScan(PWideChar(Suggestions[I].Word), #$00DF);
            while APosition <> nil do
            begin
              AShift := APosition - PWideChar(Suggestions[I].Word);
              APosition^ := #0;
              StrCopy(ACandidate, PWideChar(Suggestions[I].Word));
              ACandidate[AShift] := 'S';
              ACandidate[AShift + 1] := 'S';
              ACandidate[AShift + 2] := #0;
              StrCat(ACandidate, APosition + 1);
              Suggestions[I].Word := ACandidate;
              APosition := StrScan(PWideChar(Suggestions[I].Word), #$00DF);
            end;
          end;
        end;
      end;
  end;
  HungarianReplaceDash;
end;

procedure TdxHunspellSuggestionBuilder.GenerateNgramSuggestions(AWord: PWideChar;
  var AExistCapitalizedWords, AOnlyCompoundSuggestions: Boolean);
var
  ACandidate: TdxLineBuffer;
  ASuggestionCount: Integer;
begin
  if ((Suggestions.Count = 0) or (not Dictionary.SuggestionsLiteMode and AOnlyCompoundSuggestions)) and
    (MaxNgramSuggestionCount > 0) then
  begin
    case FCapitalizationType of
      ctLower:
        NgramSuggestion(AWord);
      ctCapitalized, ctMixedCapitalized, ctMixed:
        begin
          if FCapitalizationType in [ctCapitalized, ctMixedCapitalized] then
            AExistCapitalizedWords := True;
          StrCopy(ACandidate, AWord);
          MakeAllSmall(ACandidate, AffixManager.Language);
          NgramSuggestion(ACandidate);
        end;
      ctUpper:
        begin
          StrCopy(ACandidate, AWord);
          MakeAllSmall(ACandidate, AffixManager.Language);
          ASuggestionCount := Suggestions.Count;
          NgramSuggestion(ACandidate);
          if ASuggestionCount < Suggestions.Count then
            MakeAllCapital(PWideChar(Suggestions[ASuggestionCount].Word), AffixManager.Language);
        end
      end;
    end;
end;

function TdxHunspellSuggestionBuilder.GetAffixManager: TdxHunspellAffixManager;
begin
  Result := Dictionary.AffixManager;
end;

function TdxHunspellSuggestionBuilder.GetDictionary: TdxHunspellDictionary;
begin
  Result := TdxHunspellDictionary(inherited Dictionary);
end;

function TdxHunspellSuggestionBuilder.GetPhoneTable: TdxPhoneTable;
begin
  Result := AffixManager.PhoneTable;
end;

function TdxHunspellSuggestionBuilder.GetWordStemManager: TdxHunspellWordStemManager;
begin
  Result := Dictionary.WordStemManager;
end;

function TdxHunspellSuggestionBuilder.HasSuggestionWithDash: Boolean;
var
  I: Integer;
begin
  for I := 0 to Suggestions.Count - 1 do
    if StrScan(PWideChar(Suggestions.Items[I].Word), '-') <> nil then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

procedure TdxHunspellSuggestionBuilder.HungarianReplaceDash;
var
  I: Integer;
  APosition: PWideChar;
  AInfo: TdxSpellInfo;
  AWord: TdxLineBuffer;
begin
  if AffixManager.IsHungarian then
    for I := 0 to Suggestions.Count - 1 do
    begin
      APosition := StrScan(PWideChar(Suggestions[I].Word), '-');
      if APosition <> nil then
      begin
        AInfo := [];
        APosition^ := #0;
        StrCopy(AWord, PWideChar(Suggestions[I].Word));
        StrCat(AWord, APosition + 1);
        Dictionary.Spell(AWord, AInfo);
        if (siiCompound in AInfo) and (siiForbidden in AInfo) then
          APosition^ := ' '
        else
          APosition^ := '-';
      end;
    end;
end;

function TdxHunspellSuggestionBuilder.IsForbidden(const AWord: PWideChar): Boolean;
var
  ALength: Integer;
  AWordStem: TdxHunspellWordStem;
  ATempPrefix: TdxPrefix;
  ATempSuffix: TdxSuffix;
begin
  ATempPrefix := nil;
  ATempSuffix := nil;
  ALength := StrLen(AWord);
  AWordStem := AffixManager.Lookup(AWord);
  if (AWordStem <> nil) and (AWordStem.NeedAffix or AWordStem.InCompoundOnly) then
    AWordStem := nil;
  if (AffixManager.ProcessPrefixCheck(AWord, ALength, ATempPrefix, ATempSuffix, cwpFirst) = nil) then
    AWordStem := AffixManager.ProcessSuffixCheck(AWord, ALength, [], nil, ATempSuffix);
  Result := (AWordStem <> nil) and AWordStem.Forbidden;
end;

function TdxHunspellSuggestionBuilder.LeftCommonSubStringLength(AWord1, AWord2: PWideChar): Integer;
var
  AStartWord1: PWideChar;
begin
  Result := 0;
  if AffixManager.ComplexPrefixes then
  begin
    if (AWord1 + StrLen(AWord1) - 1)^ = (AWord2 + StrLen(AWord2) - 1)^ then
      Result := 1;
  end
  else
  begin
    if (AWord1^ = AWord2^) or (AWord1^ = WideLowCase(AWord2^, AffixManager.Language)) then
    begin
      AStartWord1 := AWord1;
      repeat
        Inc(AWord1);
        Inc(AWord2);
      until (AWord1^ <> AWord2^) or (AWord1^ = #0);
      Result := AWord1 - AStartWord1;
    end;
  end;
end;

function TdxHunspellSuggestionBuilder.MakeSecondWordCapitalized(AWord: PWideChar): PWideChar;
var
  ASpace, ACleanedWord: PWideChar;
  AWordLength, ASpaceLength: Integer;
begin
  Result := StrNew(AWord);
  ASpace := StrScan(Result, ' ');
  if ASpace <> nil then
  begin
    AWordLength := StrLen(Result);
    ASpaceLength := StrLen(ASpace + 1);
    ACleanedWord := PWideChar(Word);
    if (ASpaceLength < AWordLength) and
      not StrEquals(ACleanedWord + AWordLength - ASpaceLength, ASpace + 1) then
      (ASpace + 1)^ := WideUpCase((ASpace + 1)^, AffixManager.Language);
  end;
end;

function TdxHunspellSuggestionBuilder.Ngram(S1, S2: PWideChar; N: Integer;
  const AOptions: TdxNgramOptions): Integer;
var
  I, J, AMatchCount, ALength1, ALength2: Integer;
  ATempWord: TdxLineBuffer;
  AStore: WideChar;
  P: PWideChar;
begin
  Result := 0;
  ALength1 := StrLen(S1);
  ALength2 := StrLen(S2);
  if ALength2 = 0 then
    Exit;
  Move(S2^, ATempWord, (ALength2 + 1) * SizeOf(WideChar));
  if ngoLowering in AOptions then
    MakeAllSmall(ATempWord, WordStemManager.Language, ALength2);
  for I := 1 to N do
  begin
    AMatchCount := 0;
    P := S1 + I;
    for J := 0 to ALength1 - I do
    begin
      AStore := P^;
      P^ := #0;
      if StrPos(ATempWord, S1 + J) <> nil then
        Inc(AMatchCount);
      P^ := AStore;
      Inc(P);
    end;
    Inc(Result, AMatchCount);
    if AMatchCount < 2 then
      Break;
  end;
  AMatchCount := 0;
  if ngoLongerWorse in AOptions then
    AMatchCount := ALength2 - ALength1 - 2;
  if ngoAnyMismatch in AOptions then
    AMatchCount := Abs(ALength2 - ALength1) - 2;
  Dec(Result,  Max(AMatchCount, 0));
end;

procedure TdxHunspellSuggestionBuilder.NgramSuggestion(AWord: PWideChar);
var
  AWordLength: Integer;
  AWordStems: TdxWordStemArray;
  APhoneticWordStems: TdxPhoneticWordStemArray;
  AScores: TdxWordStemScoreArray;
  APhoneticScores: TdxPhoneticWordStemScoreArray;
  AReverseWord: TdxLineBuffer;
begin
  FCheckCompound := False;
  if AffixManager.ComplexPrefixes then
  begin
    StrCopy(AReverseWord, AWord);
    StrReverse(AReverseWord);
    AWord := @AReverseWord;
  end;
  AWordLength := StrLen(AWord);
  CalculateScores(AWord, AWordLength, AWordStems, AScores, APhoneticWordStems, APhoneticScores);
  AddNgramSuggestions(AWord, AWordLength, AWordStems);
  AddPhoneticSuggestions(AWord, AWordLength, APhoneticWordStems, APhoneticScores);
end;

procedure TdxHunspellSuggestionBuilder.AddNgramSuggestions(AWord: PWideChar;
  AWordLength: Integer; const AWordStems: TdxWordStemArray);

  function CheckDistance(ACandidate: PWideChar; var ADistance: Integer): Boolean;
  var
    AMaxDistance: Integer;
  begin
    AMaxDistance := Dictionary.NgramDistance;
    if AMaxDistance = 0 then
    begin
      ADistance := 2;
      Result := True;
    end
    else
    begin
      ADistance := Similarity.GetDistance(ACandidate, StrLen(ACandidate), AWord, AWordLength);
      Result := ADistance <= AMaxDistance;
    end;
  end;

  function Unique(const AGuess, AGuessOrig: PWideChar): Boolean;
  var
    ASuggestion: PWideChar;
    I: Integer;
  begin
    Result := True;
    for I := 0 to Suggestions.Count - 1 do
    begin
      ASuggestion := PWideChar(Suggestions[I].Word);
      if ((AGuessOrig = nil) and (StrPos(AGuess, ASuggestion) <> nil)) or
         ((AGuessOrig <> nil) and (StrPos(AGuessOrig, ASuggestion) <> nil)) or
         (CheckWord(AGuess, StrLen(AGuess), False) = 0) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

var
  I, AAddedSuggestionCount, ADistance: Integer;
  ACandidate: PWideChar;
  ASame: Boolean;
  AGuessScores: TdxGuessScoreArray;
  AGuess, AGuessOrig: TdxGuessArray;
begin
  GenerateNgramGuessTables(AWord, AWordLength, AGuess, AGuessOrig, AGuessScores, AWordStems);
  ASame := False;
  AAddedSuggestionCount := 0;
  for I := 0 to dxMaxGuessCount - 1 do
  begin
    if AGuess[I] <> nil then
    begin
      if (AAddedSuggestionCount < MaxNgramSuggestionCount) and
        (not ASame or (AGuessScores[I] > 1000)) then
      begin
        if AGuessScores[I] > 1000 then
          ASame := True;
        if Unique(AGuess[I], AGuessOrig[I]) then
        begin
          if AGuessOrig[I] <> nil then
            ACandidate := AGuessOrig[I]
          else
            ACandidate := AGuess[I];
          if CheckDistance(ACandidate, ADistance) then
          begin
            AddSuggestion(ACandidate);
            Inc(AAddedSuggestionCount);
          end;
        end;
      end;
    end;
    StrFreeAndNil(AGuess[I]);
    StrFreeAndNil(AGuessOrig[I]);
  end;
end;

procedure TdxHunspellSuggestionBuilder.AddPhoneticSuggestions(AWord: PWideChar;
  AWordLength: Integer; var APhoneticWordStems: TdxPhoneticWordStemArray;
  var APhoneticScores: TdxPhoneticWordStemScoreArray);
var
  I, ALength, AAddedSuggestionCount: Integer;
  ACandidate: TdxLineBuffer;
begin
  if PhoneTable.Count = 0 then Exit;
  Sort(@APhoneticWordStems, nil, @APhoneticScores, dxMaxWordStemCount);
  ALength := 0;
  for I := 0 to dxMaxWordStemCount - 1 do
  begin
    if APhoneticWordStems[I] <> nil then
    begin
      StrCopy(ACandidate, APhoneticWordStems[I]);
      ALength := StrLen(ACandidate);
      MakeAllSmall(ACandidate, AffixManager.Language, ALength);
    end;
    Inc(APhoneticScores[I], 2 * Similarity.GetLongestCommonSubsequenceLength(AWord, ACandidate) -
      Abs(AWordLength - ALength) + LeftCommonSubStringLength(AWord, ACandidate));
  end;
  Sort(@APhoneticWordStems, nil, @APhoneticScores, dxMaxWordStemCount);
  AAddedSuggestionCount := 0;
  for I := 0 to dxMaxWordStemCount - 1 do
    if (AAddedSuggestionCount < dxMaxPhoneticSuggestionCount) and
      (APhoneticWordStems[I] <> nil) and not SuggestionExist(APhoneticWordStems[I]) and
      (CheckWord(APhoneticWordStems[I], StrLen(APhoneticWordStems[I]), False) <> 0) then
    begin
      CheckAddToSuggestions(APhoneticWordStems[I]);
      Inc(AAddedSuggestionCount);
    end;
end;

procedure TdxHunspellSuggestionBuilder.CalculateScores(AWord: PWideChar;
  AWordLength: Integer; out AWordStems: TdxWordStemArray;
  out AWordStemScores: TdxWordStemScoreArray;
  out APhoneticWordStems: TdxPhoneticWordStemArray;
  out APhoneticScores: TdxPhoneticWordStemScoreArray);
var
  AField, ACandidate, ATarget, ATarget2: TdxLineBuffer;
  AWordStem: TdxHunspellWordStem;
  I, AIndex, AWordStemScore, AWordStemScore2, ATableIndex, AMinScore, APhoneticScore, APhoneticIndex: Integer;
begin
  FillChar(AWordStems, SizeOf(AWordStems), 0);
  FillChar(APhoneticWordStems, SizeOf(APhoneticWordStems), 0);
  for I := 0 to dxMaxWordStemCount - 1 do
  begin
    AWordStemScore := -100 * I;
    AWordStemScores[I] := AWordStemScore;
    APhoneticScores[I] := AWordStemScore;
  end;
  AIndex := dxMaxWordStemCount - 1;
  APhoneticIndex := dxMaxWordStemCount - 1;
  APhoneticScore := APhoneticScores[dxMaxWordStemCount - 1];
  if PhoneTable.Count > 0 then
  begin
    StrCopy(ACandidate, AWord);
    MakeAllCapital(ACandidate, AffixManager.Language);
    PhoneTable.Phonetic(ACandidate, ATarget, AWordLength);
  end;
  AWordStem := WordStemManager.GetFirstItem(ATableIndex);
  while AWordStem <> nil do
  begin
    if (AffixManager.IsForbiddenOrNoSuggestWord(AWordStem, True) or
      AWordStem.UpcaseOnly or AWordStem.InCompoundOnly) then
    begin
      AWordStem := WordStemManager.GetNextItem(ATableIndex, AWordStem);
      Continue;
    end;
    AWordStemScore := Trigram(AWord, AWordStem.WordStem, [ngoLongerWorse, ngoLowering]) +
      LeftCommonSubStringLength(AWord, AWordStem.WordStem);
    if AWordStem.MorphologyPhone and
      (CopyMorphology(AField, AWordStem.MorphologicalDescription, mlPhon) <> nil) then
    begin
      AWordStemScore2 := Trigram(AWord, AField, [ngoLongerWorse, ngoLowering]) + LeftCommonSubStringLength(AWord, AField);
      AWordStemScore := Max(AWordStemScore, AWordStemScore2);
    end;
    if (PhoneTable.Count > 0) and (AWordStemScore > 2) and (Abs(AWordLength - AWordStem.WordStemLength) <= 3) then
    begin
      StrCopy(ACandidate, AWordStem.WordStem);
      MakeAllCapital(ACandidate, AffixManager.Language);
      PhoneTable.Phonetic(ACandidate, ATarget2, -1);
      APhoneticScore := 2 * Trigram(ATarget, ATarget2, [ngoLongerWorse]);
    end;
    if (AWordStemScore > 0) and (AWordStemScore > AWordStemScores[AIndex]) then
    begin
      AWordStemScores[AIndex] := AWordStemScore;
      AWordStems[AIndex] := AWordStem;
      AMinScore := AWordStemScore;
      for I := 0 to dxMaxWordStemCount - 1 do
        if AWordStemScores[I] < AMinScore then
        begin
          AIndex := I;
          AMinScore := AWordStemScores[I];
        end;
    end;
    if (APhoneticScore > 0) and (APhoneticScore > APhoneticScores[APhoneticIndex]) then
    begin
      APhoneticScores[APhoneticIndex] := APhoneticScore;
      APhoneticWordStems[APhoneticIndex] := AWordStem.WordStem;
      AMinScore := APhoneticScore;
      for I := 0 to dxMaxWordStemCount - 1 do
        if APhoneticScores[I] < AMinScore then
        begin
          APhoneticIndex := I;
          AMinScore := APhoneticScores[I];
        end;
    end;
    AWordStem := WordStemManager.GetNextItem(ATableIndex, AWordStem);
  end;
end;

procedure TdxHunspellSuggestionBuilder.ReplaceTableSuggestion;
var
  AReplaceTable: TdxReplaceTable;
  ACandidate: TdxLineBuffer;
  ATextPos, AWordStart, ASeparatorPos: PWideChar;
  I, AReplacementLen, ATextLen, AWordLength, ACount, ASavedSuggestionCount: Integer;
begin
  AWordLength := Length(Word);
  if AWordLength < 2 then
    Exit;
  AReplaceTable := AffixManager.ReplaceTable;
  if AReplaceTable = nil then
    Exit;
  ACount := AReplaceTable.Count;
  for I := 0 to ACount - 1 do
  begin
    AWordStart := PWideChar(Word);
    ATextLen := StrLen(AReplaceTable[I].Text);
    AReplacementLen := StrLen(AReplaceTable[I].Replacement);
    ATextPos := StrPos(AWordStart, AReplaceTable[I].Text);
    while ATextPos <> nil do
    begin
      StrCopy(ACandidate, AWordStart);
      if ATextPos - AWordStart + AReplacementLen + Integer(StrLen(ATextPos + ATextLen)) >= dxMaxWordLength then
        Break;
      StrCopy(@ACandidate[(ATextPos - AWordStart)], AReplaceTable[I].Replacement);
      StrCopy(@ACandidate[(ATextPos - AWordStart) + AReplacementLen], ATextPos + ATextLen);
      CheckAddToSuggestions(PWideChar(@ACandidate));

      ASeparatorPos := StrScan(ACandidate, ' ');
      if ASeparatorPos <> nil then
      begin
        ASeparatorPos^ := #0;
        if CheckWord(ACandidate, StrLen(ACandidate), False) <> 0 then
        begin
          ASeparatorPos^ := ' ';
          ASavedSuggestionCount := Suggestions.Count;
          CheckAddToSuggestions((ASeparatorPos + 1));
          if ASavedSuggestionCount < Suggestions.Count then
            Suggestions[ASavedSuggestionCount].Word := PWideChar(@ACandidate);
        end;
        ASeparatorPos^ := ' ';
      end;
      Inc(ATextPos);
      ATextPos := StrPos(ATextPos, AReplaceTable[I].Text);
    end;
  end;
end;

function TdxHunspellSuggestionBuilder.Spell(AWord: PWideChar): Boolean;
var
  AInfo: TdxSpellInfo;
begin
  AInfo := [];
  Result := Dictionary.Spell(AWord, AInfo);
end;

initialization
  GetRegisteredDictionaryTypes.Register(TdxHunspellDictionary, cxGetResourceString(@sdxSpellCheckerHunspellDictionary));

end.
