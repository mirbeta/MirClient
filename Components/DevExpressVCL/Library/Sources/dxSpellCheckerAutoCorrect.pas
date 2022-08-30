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

unit dxSpellCheckerAutoCorrect;

{$I cxVer.inc}

interface

uses
  Types, Windows, SysUtils, Messages, Classes, Graphics, Generics.Defaults, Generics.Collections, Controls, StdCtrls,
  dxCore, cxClasses, dxMessages, cxControls, cxLookAndFeels, dxSpellChecker, dxSpellCheckerCore, dxSpellCheckerAdapters;

type

  { TdxSpellCheckerAutoCorrectEngine }

  TdxSpellCheckerAutoCorrectEngineCheckEvent = function (
    ARule: IdxSpellCheckerAutoCorrectRule; var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean of object;

  TdxSpellCheckerAutoCorrectEngine = class(TdxSpellCheckerPersistent)
  strict private
    FIgnoreCaretReturn: Boolean;
    FRules: TList<IdxSpellCheckerAutoCorrectRule>;
    FTextController: IdxSpellCheckTextController;

    FOnCheckEvent: TdxSpellCheckerAutoCorrectEngineCheckEvent;
  protected
    function CheckRule(var AInfo: TdxSpellCheckerAutoCorrectWordInfo; ARule: IdxSpellCheckerAutoCorrectRule): Boolean;
    function DoAutoCorrect(ARule: IdxSpellCheckerAutoCorrectRule; var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean; virtual;
    procedure InitializeRules; virtual;
  public
    constructor Create(ASpellChecker: TdxCustomSpellChecker; ATextController: IdxSpellCheckTextController); reintroduce;
    destructor Destroy; override;
    procedure Check(var AInfo: TdxSpellCheckerAutoCorrectWordInfo);
    //
    property IgnoreCaretReturn: Boolean read FIgnoreCaretReturn write FIgnoreCaretReturn;
    property TextController: IdxSpellCheckTextController read FTextController;
    //
    property OnCheckEvent: TdxSpellCheckerAutoCorrectEngineCheckEvent read FOnCheckEvent write FOnCheckEvent;
  end;

  { TdxSpellCheckerAutoCorrectManager }

  TdxSpellCheckerAutoCorrectManager = class(TdxSpellCheckerCustomAutoCorrectManager)
  strict private
    FEngine: TdxSpellCheckerAutoCorrectEngine;
    FLastApplyRule: IdxSpellCheckerAutoCorrectCustomRule;
    FPrevWord: string;

    function CheckHandler(ARule: IdxSpellCheckerAutoCorrectRule; var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean;
    function GetAdapter: IdxSpellCheckerAutoCorrectAdapter;
    function GetAutoCorrectWordRange: TdxSpellCheckerAutoCorrectWordInfo;
    function GetEngine: TdxSpellCheckerAutoCorrectEngine;
  protected
    procedure DoActiveChanged; override;
    procedure DoOptionsChanged; override;
    procedure Initialize; virtual;
    procedure Undo; override;
  public
    destructor Destroy; override;
    procedure KeyPress(AKey: Char); override;
    //
    property Adapter: IdxSpellCheckerAutoCorrectAdapter read GetAdapter;
    property Engine: TdxSpellCheckerAutoCorrectEngine read GetEngine;
  end;

  { TdxSpellCheckerCorrectSentenceCapsRule }

  TdxSpellCheckerCorrectSentenceCapsRule = class(TdxSpellCheckerAutoCorrectCustomRule)
  strict private
    FPrevWord: string;
    FSentenceDelimiters: string;
  protected
    function GetActive: Boolean; override;
    procedure InitializeDelimiters; override;
    function IsFirstWordOfSentence(const ATextController: IdxSpellCheckTextController;
      var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean;
    function IsSentenceDelimiter(ACh: WideChar): Boolean;
  public
    function IsCheckWord(const ATextController: IdxSpellCheckTextController;
      var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean; override;
    procedure Undo; override;

    property SentenceDelimiters: string read FSentenceDelimiters;
  end;

  { TdxSpellCheckerCorrectCapsLockRule }

  TdxSpellCheckerCorrectCapsLockRule = class(TdxSpellCheckerAutoCorrectCustomRule)
  protected
    function GetActive: Boolean; override;
  public
    procedure AfterCorrect; override;
    function IsCheckWord(const ATextController: IdxSpellCheckTextController;
      var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean; override;
  end;

  { TdxSpellCheckerCorrectInitialCapsRule }

  TdxSpellCheckerCorrectInitialCapsRule = class(TdxSpellCheckerAutoCorrectCustomRule)
  strict private
    FPrevWord: string;
  protected
    function GetActive: Boolean; override;
  public
    function IsCheckWord(const ATextController: IdxSpellCheckTextController;
      var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean; override;
    procedure Undo; override;
  end;

  { TdxSpellCheckerCorrectReplaceTextAsYouTypeRule }

  TdxSpellCheckerCorrectReplaceTextAsYouTypeRule = class(TdxSpellCheckerAutoCorrectCustomRule)
  protected
    function GetActive: Boolean; override;
    procedure InitializeDelimiters; override;
  public
    function IsCheckWord(const ATextController: IdxSpellCheckTextController;
      var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean; override;
    function IsTerminating: Boolean; override;
  end;

implementation

uses
  Character, dxCharacters, dxSpellCheckerUtils, cxTextEdit, Math, cxContainer;

const
  dxVK_UNDO = 26;

function ExtractChar(const AController: IdxSpellCheckTextController; const APosition: IdxSpellCheckerPosition): Char;
var
  AText: string;
begin
  AText := AController.GetWord(APosition, APosition.MoveForward);
  if AText <> '' then
    Result := AText[1]
  else
    Result := #0;
end;

{ TdxSpellCheckerAutoCorrectEngine }

constructor TdxSpellCheckerAutoCorrectEngine.Create(
  ASpellChecker: TdxCustomSpellChecker; ATextController: IdxSpellCheckTextController);
begin
  inherited Create(ASpellChecker);
  FTextController := ATextController;
  FRules := TList<IdxSpellCheckerAutoCorrectRule>.Create;
  InitializeRules;
end;

destructor TdxSpellCheckerAutoCorrectEngine.Destroy;
begin
  FreeAndNil(FRules);
  inherited Destroy;
end;

procedure TdxSpellCheckerAutoCorrectEngine.Check(var AInfo: TdxSpellCheckerAutoCorrectWordInfo);
var
  ARule: IdxSpellCheckerAutoCorrectRule;
  I: Integer;
begin
  AInfo.Rule := nil;
  for I := 0 to FRules.Count - 1 do
  begin
    ARule := FRules[I];
    if CheckRule(AInfo, ARule) then
    begin
      AInfo.Rule := ARule;
      if ARule.IsTerminating then
        Break;
    end;
  end;
end;

function TdxSpellCheckerAutoCorrectEngine.CheckRule(
  var AInfo: TdxSpellCheckerAutoCorrectWordInfo; ARule: IdxSpellCheckerAutoCorrectRule): Boolean;

  function IsBeginningOfWord: Boolean;
  begin
    Result := (AInfo.WordPositionStart.Compare(AInfo.SpellingAreaStart) <= 0) or
      TdxCharacters.IsWhiteSpace(ExtractChar(TextController, AInfo.WordPositionStart.MoveBackward));
  end;

  function GetSelStartDelta: Integer;
  begin
    if AInfo.LastKey = #13 then
      Result := IfThen(IgnoreCaretReturn, -1, 1)
    else
      Result := 0;
  end;

var
  ATempInfo: TdxSpellCheckerAutoCorrectWordInfo;
begin
  Result := (ARule <> nil) and ARule.Active and ((AInfo.LastKey = #0) or ARule.IsWordDelimiter(AInfo.LastKey));
  if Result then
  begin
    ATempInfo := AInfo;
    Result := (Length(ATempInfo.Word) > 0) and IsBeginningOfWord and ARule.IsCheckWord(TextController, ATempInfo);
    if Result then
    begin
      ATempInfo.CursorLogPosition := ATempInfo.CursorLogPosition -
        Length(TextController.GetWord(ATempInfo.WordPositionStart, ATempInfo.WordPositionFinish)) +
        Length(ATempInfo.Word) + 1 + GetSelStartDelta;
      Result := DoAutoCorrect(ARule, ATempInfo);
    end;
    if Result then
      AInfo := ATempInfo;
  end;
end;

function TdxSpellCheckerAutoCorrectEngine.DoAutoCorrect(
  ARule: IdxSpellCheckerAutoCorrectRule; var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean;
begin
  Result := not Assigned(OnCheckEvent) or OnCheckEvent(ARule, AInfo);
end;

procedure TdxSpellCheckerAutoCorrectEngine.InitializeRules;
begin
  FRules.Clear;
  FRules.Add(TdxSpellCheckerCorrectReplaceTextAsYouTypeRule.Create(SpellChecker.AutoCorrectOptions));
  FRules.Add(TdxSpellCheckerCorrectCapsLockRule.Create(SpellChecker.AutoCorrectOptions));
  FRules.Add(TdxSpellCheckerCorrectInitialCapsRule.Create(SpellChecker.AutoCorrectOptions));
  FRules.Add(TdxSpellCheckerCorrectSentenceCapsRule.Create(SpellChecker.AutoCorrectOptions));
end;

{ TdxSpellCheckerAutoCorrectManager }

destructor TdxSpellCheckerAutoCorrectManager.Destroy;
begin
  FreeAndNil(FEngine);
  inherited Destroy;
end;

procedure TdxSpellCheckerAutoCorrectManager.KeyPress(AKey: Char);
var
  AInfo: TdxSpellCheckerAutoCorrectWordInfo;
begin
  inherited KeyPress(AKey);

  if Active and (Adapter <> nil) then
  begin
    if Ord(AKey) = dxVK_UNDO then
    begin
      Undo;
      Exit;
    end;

    AInfo := GetAutoCorrectWordRange;
    FPrevWord := AInfo.Word;
    Engine.Check(AInfo);
    FLastApplyRule := AInfo.Rule;
    if FLastApplyRule <> nil then
    try
      Adapter.ApplyChanges(AInfo);
    finally
      FLastApplyRule.AfterCorrect;
    end;
  end;
end;

procedure TdxSpellCheckerAutoCorrectManager.DoActiveChanged;
begin
  Initialize;
end;

procedure TdxSpellCheckerAutoCorrectManager.DoOptionsChanged;
begin
  Initialize;
end;

procedure TdxSpellCheckerAutoCorrectManager.Initialize;
begin
  FreeAndNil(FEngine);
  FLastApplyRule := nil;
  FPrevWord := '';
end;

procedure TdxSpellCheckerAutoCorrectManager.Undo;
begin
  if FLastApplyRule <> nil then
  begin
    FLastApplyRule.Undo;
    FLastApplyRule := nil;
  end;
end;

function TdxSpellCheckerAutoCorrectManager.CheckHandler(
  ARule: IdxSpellCheckerAutoCorrectRule; var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean;
var
  AWordRange: TdxSpellCheckerAutoCorrectWordRange;
begin
  AWordRange := AInfo.ToWordRange;
  Result := DoAutoCorrect(ARule as TdxSpellCheckerAutoCorrectCustomRule, AWordRange);
  if Result and (AWordRange <> AInfo.ToWordRange) then
    AInfo := TdxSpellCheckerAutoCorrectWordInfo.Create(AWordRange, Engine.TextController);
end;

function TdxSpellCheckerAutoCorrectManager.GetAdapter: IdxSpellCheckerAutoCorrectAdapter;
begin
  Result := inherited Adapter as IdxSpellCheckerAutoCorrectAdapter;
end;

function TdxSpellCheckerAutoCorrectManager.GetAutoCorrectWordRange: TdxSpellCheckerAutoCorrectWordInfo;
var
  AChar: Char;
begin
  Result := TdxSpellCheckerAutoCorrectWordInfo.Create;
  Adapter.GetSpellingBounds(Result.SpellingAreaStart, Result.SpellingAreaFinish);
  Adapter.UpdateController(Engine.TextController);
  Engine.TextController.SetSpellingArea(Result.SpellingAreaStart, Result.SpellingAreaFinish);
  Result.LastKey := LastKey;

  Result.WordPositionStart := Adapter.GetCursorPosition;
  Result.CursorLogPosition := Result.WordPositionStart.ToInteger;
  if Engine.IgnoreCaretReturn and (LastKey = #13) then
  begin
    if Result.WordPositionStart.Compare(Result.SpellingAreaStart) > 0 then
      Result.WordPositionStart := Result.WordPositionStart.MoveBackward;
  end;
  Result.WordPositionFinish := Result.WordPositionStart;

  while Result.WordPositionStart.Compare(Result.SpellingAreaStart) > 0 do
  begin
    AChar := ExtractChar(Engine.TextController, Result.WordPositionStart.MoveBackward);
    if TdxCharacters.IsWhiteSpace(AChar) then
      Break;
    Result.Word := AChar + Result.Word;
    Result.WordPositionStart := Result.WordPositionStart.MoveBackward;
  end;
end;

function TdxSpellCheckerAutoCorrectManager.GetEngine: TdxSpellCheckerAutoCorrectEngine;
begin
  if FEngine = nil then
  begin
    FEngine := TdxSpellCheckerAutoCorrectEngine.Create(SpellChecker, Adapter.CreateController);
    FEngine.IgnoreCaretReturn := Adapter.IgnoreCaretReturn;
    FEngine.OnCheckEvent := CheckHandler;
  end;
  Result := FEngine;
end;

{ TdxSpellCheckerCorrectSentenceCapsRule }

function TdxSpellCheckerCorrectSentenceCapsRule.IsCheckWord(
  const ATextController: IdxSpellCheckTextController; var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean;
var
  I: Integer;
begin
  FPrevWord := '';
  Result := (AInfo.LastKey <> '.') or (Options.FirstLetterExceptions.Find(AInfo.Word + '.') = -1);
  Result := Result and IsFirstWordOfSentence(ATextController, AInfo);
  if Result then
  begin
    for I := 1 to Length(AInfo.Word) do
    begin
      Result := WideIsLoCase(AInfo.Word[I]);
      if not Result then
        Break;
    end;
    if Result then
      AInfo.Word := WideCapitalizeCase(AInfo.Word);
  end;
end;

procedure TdxSpellCheckerCorrectSentenceCapsRule.Undo;
begin
  if Options <> nil then
  begin
    if Options.FirstLetterExceptions.AutoInclude then
      Options.FirstLetterExceptions.Add(FPrevWord);
  end;
end;

function TdxSpellCheckerCorrectSentenceCapsRule.GetActive: Boolean;
begin
  Result := (Options <> nil) and Options.CorrectSentenceCaps;
end;

procedure TdxSpellCheckerCorrectSentenceCapsRule.InitializeDelimiters;
begin
  inherited InitializeDelimiters;
  FSentenceDelimiters := #13#10'.';
end;

function TdxSpellCheckerCorrectSentenceCapsRule.IsFirstWordOfSentence(
  const ATextController: IdxSpellCheckTextController; var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean;
var
  AChar: Char;
  APosition: IdxSpellCheckerPosition;
begin
  APosition := AInfo.WordPositionStart;
  while (APosition.Compare(AInfo.SpellingAreaStart) > 0) and (ExtractChar(ATextController, APosition.MoveBackward) = ' ') do
    APosition := APosition.MoveBackward;

  Result := APosition.Compare(AInfo.SpellingAreaStart) = 0;
  if not Result then
  begin
    AChar := ExtractChar(ATextController, APosition.MoveBackward);
    Result := IsSentenceDelimiter(AChar);
    if AChar = '.' then
    begin
      FPrevWord := '.';
      APosition := APosition.MoveBackward;
      while APosition.Compare(AInfo.SpellingAreaStart) > 0 do
      begin
        AChar := ExtractChar(ATextController, APosition.MoveBackward);
        if TdxCharacters.IsWhiteSpace(AChar) then
          Break;
        FPrevWord := AChar + FPrevWord;
        APosition := APosition.MoveBackward;
      end;
      Result := Options.FirstLetterExceptions.Find(FPrevWord) = -1;
    end;
  end;
end;

function TdxSpellCheckerCorrectSentenceCapsRule.IsSentenceDelimiter(ACh: WideChar): Boolean;
begin
  Result := WideCharPos(ACh, SentenceDelimiters) > 0;
end;

{ TdxSpellCheckerCorrectCapsLockRule }

procedure TdxSpellCheckerCorrectCapsLockRule.AfterCorrect;
begin
  if Options.DisableCapsLock and (GetKeyState(VK_CAPITAL) = 1) then
  begin
    keybd_event(VK_CAPITAL, 0, KEYEVENTF_EXTENDEDKEY, 0);
    keybd_event(VK_CAPITAL, 0, KEYEVENTF_KEYUP, 0);
  end;
end;

function TdxSpellCheckerCorrectCapsLockRule.IsCheckWord(
  const ATextController: IdxSpellCheckTextController; var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean;
var
  I: Integer;
begin
  Result := Active and (Length(AInfo.Word) > 1) and (GetKeyState(VK_CAPITAL) = 1);
  if Result then
  begin
    for I := 1 to Length(AInfo.Word) do
    begin
      if I = 1 then
        Result := WideIsLoCase(AInfo.Word[I])
      else
        Result := WideIsUpCase(AInfo.Word[I]);

      if not Result then
        Break;
    end;
    if Result then
      AInfo.Word := WideCapitalizeCase(AInfo.Word);
  end;
end;

function TdxSpellCheckerCorrectCapsLockRule.GetActive: Boolean;
begin
  Result := (Options <> nil) and Options.CorrectCapsLock;
end;

{ TdxSpellCheckerCorrectInitialCapsRule }

function TdxSpellCheckerCorrectInitialCapsRule.IsCheckWord(
  const ATextController: IdxSpellCheckTextController; var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean;
var
  I: Integer;
begin
  Result := Active and (Length(AInfo.Word) > 2);
  if Result then
  begin
    for I := 1 to Length(AInfo.Word) do
    begin
      if I < 3 then
        Result := WideIsUpCase(AInfo.Word[I])
      else
        Result := WideIsLoCase(AInfo.Word[I]);

      if not Result then
        Break;
    end;

    Result := Result and (Options.InitialCapsExceptions.Find(AInfo.Word) = -1);
    if Result then
      AInfo.Word := WideCapitalizeCase(AInfo.Word);
  end;
  FPrevWord := AInfo.Word;
end;

procedure TdxSpellCheckerCorrectInitialCapsRule.Undo;
begin
  if Options <> nil then
  begin
    if Options.InitialCapsExceptions.AutoInclude then
      Options.InitialCapsExceptions.Add(FPrevWord);
  end;
end;

function TdxSpellCheckerCorrectInitialCapsRule.GetActive: Boolean;
begin
  Result := (Options <> nil) and Options.CorrectInitialCaps;
end;

{ TdxSpellCheckerCorrectReplaceTextAsYouTypeRule }

function TdxSpellCheckerCorrectReplaceTextAsYouTypeRule.IsCheckWord(
  const ATextController: IdxSpellCheckTextController; var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean;
var
  AReplacement: string;
  AReplacementCapitalization, AWordCapitalization: TdxCapitalizationType;
begin
  Result := Options.GetReplacement(AInfo.Word, AReplacement);
  if Result then
  begin
    AReplacementCapitalization := GetWordCapitalizationType(AReplacement);
    AWordCapitalization := GetWordCapitalizationType(AInfo.Word);
    case AReplacementCapitalization of
      ctLower:
        AInfo.Word := WidePatternCase(AInfo.Word, AReplacement);

      ctMixed:
        if AWordCapitalization <> ctLower then
          AInfo.Word := WidePatternCase(AInfo.Word, AReplacement)
        else
          AInfo.Word := AReplacement;

      ctCapitalized, ctMixedCapitalized:
        if AWordCapitalization = ctUpper then
          AInfo.Word := WidePatternCase(AInfo.Word, AReplacement)
        else
          AInfo.Word := AReplacement;

      ctUpper:
        AInfo.Word := AReplacement;
    end;
  end
  else
    if not WideIsAlphaNumeric(AInfo.LastKey) and Options.GetReplacement(AInfo.Word + AInfo.LastKey, AReplacement) then
    begin
      AInfo.Word := WidePatternCase(AInfo.Word, AReplacement);
      AInfo.WordPositionFinish := ATextController.CreatePosition(AInfo.WordPositionFinish.ToInteger + 1);
      Result := True;
    end;
end;

function TdxSpellCheckerCorrectReplaceTextAsYouTypeRule.IsTerminating: Boolean;
begin
  Result := False;
end;

function TdxSpellCheckerCorrectReplaceTextAsYouTypeRule.GetActive: Boolean;
begin
  Result := (Options <> nil) and Options.ReplaceTextAsYouType;
end;

procedure TdxSpellCheckerCorrectReplaceTextAsYouTypeRule.InitializeDelimiters;
begin
  SetWordDelimiters(#9#13' .,<>=!?:;''"()[]{}+|-/\+');
end;

end.
