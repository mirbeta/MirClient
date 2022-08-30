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

unit dxRichEdit.Commands.SpellChecker;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Controls,
  dxSpellCheckerCore,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.View.Core,
  dxRichEdit.Commands,
  dxRichEdit.Commands.IDs;

type
  { TdxSpellCheckerCommandBase }

  TdxSpellCheckerCommandBase = class abstract(TdxRichEditMenuItemSimpleCommand)
  strict private
    function GetSpellChecker: IdxSpellChecker3;
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function GetMisspelledInterval: TdxMisspelledInterval;
    function GetMisspelledWord(AInterval: TdxMisspelledInterval): string;
    function CanExecuteCommand: Boolean; virtual; abstract;

    property SpellChecker: IdxSpellChecker3 read GetSpellChecker;
  end;

  { TdxReplaceMisspellingCommand }

  TdxReplaceMisspellingCommand = class(TdxSpellCheckerCommandBase)
  strict private
    FSuggestion: string;
  protected
    procedure ExecuteCore; override;
    function CanExecuteCommand: Boolean; override;
    function GetSuggestions(AInterval: TdxMisspelledInterval): TArray<string>;
    function HasSuggestions(AInterval: TdxMisspelledInterval): Boolean;
  public
    constructor Create(const AControl: IdxRichEditControl; const ASuggestion: string); reintroduce;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    function GetObjectMenuCaption: string; override;

    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    class function Id: TdxRichEditCommandId; override;

    property Suggestion: string read FSuggestion write FSuggestion;
  end;
  TdxReplaceMisspellingCommandClass = class of TdxReplaceMisspellingCommand;

  { TdxAutoCorrectMisspellingCommand }

  TdxAutoCorrectMisspellingCommand = class(TdxReplaceMisspellingCommand)
  protected
    procedure ExecuteCore; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxAutoCorrectPlaceholderMenuCommand }

  TdxAutoCorrectPlaceholderMenuCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDeleteRepeatedWordCommand }

  TdxDeleteRepeatedWordCommand = class(TdxSpellCheckerCommandBase)
  protected
    function CanExecuteCommand: Boolean; override;
    procedure ExecuteCore; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxIgnoreMisspellingCommand }

  TdxIgnoreMisspellingCommand = class(TdxSpellCheckerCommandBase)
  protected
    procedure ExecuteCore; override;
    function CanExecuteCommand: Boolean; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxIgnoreAllMisspellingsCommand }

  TdxIgnoreAllMisspellingsCommand = class(TdxSpellCheckerCommandBase)
  protected
    procedure ExecuteCore; override;
    function CanExecuteCommand: Boolean; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxAddWordToDictionaryCommand }

  TdxAddWordToDictionaryCommand = class(TdxSpellCheckerCommandBase)
  protected
    procedure ExecuteCore; override;
    function CanExecuteCommand: Boolean; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxCheckSpellingCommand }

  TdxCheckSpellingCommand = class(TdxSpellCheckerCommandBase)
  protected
    procedure ExecuteCore; override;
    function CanExecuteCommand: Boolean; override;
  public
    class function GetDescription: string; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function Id: TdxRichEditCommandId; override;
  end;

implementation

uses
  dxCore,
  dxRichEdit.InnerControl,
  dxRichEdit.InnerControl.SpellCheckerController,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.Commands.Images,
  dxRichEdit.Commands.Strs;

{ TdxSpellCheckerCommandBase }

function TdxSpellCheckerCommandBase.GetSpellChecker: IdxSpellChecker3;
begin
  Result := TdxSpellCheckerInstance.ISpellChecker3;
end;

procedure TdxSpellCheckerCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := (IsContentEditable and (SpellChecker <> nil)) and CanExecuteCommand;
  AState.Visible := True;
end;

function TdxSpellCheckerCommandBase.GetMisspelledInterval: TdxMisspelledInterval;
var
  ALogPosition: TdxDocumentLogPosition;
  AIntervals: TdxMisspelledIntervalCollection;
begin
  ALogPosition := ActivePieceTable.DocumentModel.Selection.Interval.NormalizedStart.LogPosition;
  AIntervals := ActivePieceTable.SpellCheckerManager.MisspelledIntervals;
  Result := AIntervals.FindInterval(ALogPosition);
end;

function TdxSpellCheckerCommandBase.GetMisspelledWord(AInterval: TdxMisspelledInterval): string;
begin
  Result := ActivePieceTable.GetFilteredPlainText(AInterval.Interval.Start, AInterval.Interval.&End, ActivePieceTable.VisibleTextFilter.IsRunVisible);
end;

{ TdxReplaceMisspellingCommand }

constructor TdxReplaceMisspellingCommand.Create(const AControl: IdxRichEditControl; const ASuggestion: string);
begin
  inherited Create(AControl);
  Suggestion := ASuggestion;
end;

class function TdxReplaceMisspellingCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeMistakenWordDescription);
end;

class function TdxReplaceMisspellingCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeMistakenWordMenuCaption);
end;

class function TdxReplaceMisspellingCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ReplaceMisspelling;
end;

function TdxReplaceMisspellingCommand.GetObjectMenuCaption: string;
begin
  if Suggestion <> '' then
    Exit(Suggestion);
  Result := inherited GetObjectMenuCaption;
end;

procedure TdxReplaceMisspellingCommand.ExecuteCore;
var
  AInterval: TdxMisspelledInterval;
  AController: TdxRichEditTextController;
  AStart, AEnd: IdxSpellCheckerPosition;
  ALogPositon: TdxDocumentLogPosition;
begin
  DocumentModel.BeginUpdate;
  try
    AInterval := GetMisspelledInterval;
    AController := TdxRichEditTextController.Create(InnerControl);
    try
      AStart := TdxDocumentPosition.Create(AInterval.Interval.Start);
      AEnd := TdxDocumentPosition.Create(AInterval.Interval.&End);
      ALogPositon := AInterval.Start;
      AController.ReplaceWord(AStart, AEnd, Suggestion);

      DocumentModel.Selection.Start := ALogPositon + Length(Suggestion);
      DocumentModel.Selection.&End := DocumentModel.Selection.Start;
    finally
      AController.Free;
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxReplaceMisspellingCommand.ForceExecute(const AState: IdxCommandUIState);
var
  ASuggestions: TArray<string>;
  AValueState: TdxDefaultValueBasedCommandUIState<string>;
begin
  AValueState := Safe<TdxDefaultValueBasedCommandUIState<string>>.Cast(TObject(AState));
  if AValueState <> nil then
    if AValueState.Value <> '' then
      Suggestion := AValueState.Value;
  if Suggestion = '' then
  begin
    ASuggestions := GetSuggestions(GetMisspelledInterval);
    if Length(ASuggestions) > 0 then
      Suggestion := ASuggestions[0];
  end;
  inherited ForceExecute(AState);
end;

function TdxReplaceMisspellingCommand.CanExecuteCommand: Boolean;
var
  AInterval: TdxMisspelledInterval;
begin
  AInterval := GetMisspelledInterval;
  if (AInterval = nil) or (AInterval.ErrorType <> seMisspelling) or
      (not ActivePieceTable.CanEditRange(AInterval.Start, AInterval.&End)) then
    Exit(False);
  Result := (Suggestion <> '') or HasSuggestions(AInterval);
end;

function TdxReplaceMisspellingCommand.GetSuggestions(AInterval: TdxMisspelledInterval): TArray<string>;
var
  AMisspellingWord: string;
begin
  AMisspellingWord := GetMisspelledWord(AInterval);
  Result := SpellChecker.GetSuggestions(AMisspellingWord);
end;

function TdxReplaceMisspellingCommand.HasSuggestions(AInterval: TdxMisspelledInterval): Boolean;
var
  ASuggestions: TArray<string>;
begin
  ASuggestions := GetSuggestions(AInterval);
  Result := Length(ASuggestions) > 0;
end;

function TdxReplaceMisspellingCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultValueBasedCommandUIState<string>.Create;
end;

{ TdxAutoCorrectMisspellingCommand }

class function TdxAutoCorrectMisspellingCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.AutoCorrectMisspelling;
end;

procedure TdxAutoCorrectMisspellingCommand.ExecuteCore;
var
  AInterval: TdxMisspelledInterval;
  AController: TdxRichEditTextController;
  AStart, AEnd: IdxSpellCheckerPosition;
  AWord: string;
  ASpellChecker: IdxSpellChecker3;
begin
  AInterval := GetMisspelledInterval;
  AController := TdxRichEditTextController.Create(InnerControl);
  try
    AStart := TdxDocumentPosition.Create(AInterval.Interval.Start);
    AEnd := TdxDocumentPosition.Create(AInterval.Interval.&End);
    AWord := AController.GetWord(AStart, AEnd);
  finally
    AController.Free;
  end;
  inherited ExecuteCore;
  ASpellChecker := TdxSpellCheckerInstance.ISpellChecker3;
  if ASpellChecker = nil then
    Exit;
  ASpellChecker.AutoCorrectOptions.Replacements.Add(AWord, Suggestion);
end;

{ TdxAutoCorrectPlaceholderMenuCommand }

class function TdxAutoCorrectPlaceholderMenuCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.AutoCorrectPlaceholder;
end;

class function TdxAutoCorrectPlaceholderMenuCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandAutoCorrectPlaceholderDescription);
end;

class function TdxAutoCorrectPlaceholderMenuCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandAutoCorrectPlaceholderMenuCaption);
end;

procedure TdxAutoCorrectPlaceholderMenuCommand.ExecuteCore;
begin
end;

procedure TdxAutoCorrectPlaceholderMenuCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
end;

{ TdxDeleteRepeatedWordCommand }

class function TdxDeleteRepeatedWordCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteRepeatedWordDescription);
end;

class function TdxDeleteRepeatedWordCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteRepeatedWordMenuCaption);
end;

class function TdxDeleteRepeatedWordCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DeleteRepeatedWord;
end;

function TdxDeleteRepeatedWordCommand.CanExecuteCommand: Boolean;
var
  AInterval: TdxMisspelledInterval;
begin
  AInterval := GetMisspelledInterval;
  Result := ((AInterval <> nil) and (AInterval.ErrorType = seRepeating)) and
    ActivePieceTable.CanEditRange(AInterval.Start, AInterval.&End);
end;

procedure TdxDeleteRepeatedWordCommand.ExecuteCore;
var
  AInterval: TdxMisspelledInterval;
  AController: TdxRichEditTextController;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  DocumentModel.BeginUpdate;
  try
    AInterval := GetMisspelledInterval;
    AController := TdxRichEditTextController.Create(InnerControl);
    try
      AStart := AInterval.Interval.Start;
      AEnd := AInterval.Interval.&End;
      AController.PreparePositionsForDelete(AStart, AEnd);
      ActivePieceTable.DeleteContent(AStart.LogPosition, AEnd.LogPosition - AStart.LogPosition, False);
    finally
      AController.Free;
    end;
  finally
    DocumentModel.EndUpdate;
  end;
  ActiveView.EnforceFormattingCompleteForVisibleArea;
end;

{ TdxIgnoreMisspellingCommand }

class function TdxIgnoreMisspellingCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIgnoreMistakenWordDescription);
end;

class function TdxIgnoreMisspellingCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIgnoreMistakenWordMenuCaption);
end;

class function TdxIgnoreMisspellingCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.IgnoreMisspelling;
end;

procedure TdxIgnoreMisspellingCommand.ExecuteCore;
var
  AInterval: TdxMisspelledInterval;
  AWord: string;
begin
  AInterval := GetMisspelledInterval;

  AWord := GetMisspelledWord(AInterval);
  SpellChecker.Ignore(Control, AWord, TdxDocumentPosition.Create(AInterval.Interval.Start), TdxDocumentPosition.Create(AInterval.Interval.&End));
end;

function TdxIgnoreMisspellingCommand.CanExecuteCommand: Boolean;
begin
  Result := GetMisspelledInterval <> nil;
end;

{ TdxIgnoreAllMisspellingsCommand }

class function TdxIgnoreAllMisspellingsCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIgnoreAllMistakenWordsDescription);
end;

class function TdxIgnoreAllMisspellingsCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIgnoreAllMistakenWordsMenuCaption);
end;

class function TdxIgnoreAllMisspellingsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.IgnoreAllMisspellings;
end;

procedure TdxIgnoreAllMisspellingsCommand.ExecuteCore;
var
  AInterval: TdxMisspelledInterval;
  AWord: string;
begin
  AInterval := GetMisspelledInterval;
  try
    ActivePieceTable.SpellCheckerManager.MisspelledIntervals.Extract(AInterval);
    AWord := GetMisspelledWord(AInterval);
    SpellChecker.IgnoreAll(Control, AWord);
  finally
    AInterval.Free;
  end;
end;

function TdxIgnoreAllMisspellingsCommand.CanExecuteCommand: Boolean;
var
  AInterval: TdxMisspelledInterval;
begin
  AInterval := GetMisspelledInterval;
  Result := (AInterval <> nil) and (AInterval.ErrorType = seMisspelling);
end;

{ TdxAddWordToDictionaryCommand }

class function TdxAddWordToDictionaryCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandAddWordToDictionaryDescription);
end;

class function TdxAddWordToDictionaryCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandAddWordToDictionaryMenuCaption);
end;

class function TdxAddWordToDictionaryCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.AddWordToDictionary;
end;

procedure TdxAddWordToDictionaryCommand.ExecuteCore;
var
  AInterval: TdxMisspelledInterval;
  AWord: string;
begin
  AInterval := GetMisspelledInterval;
  AWord := GetMisspelledWord(AInterval);
  SpellChecker.AddToDictionary(AWord{, GetActualCulture(AInterval)});
end;

function TdxAddWordToDictionaryCommand.CanExecuteCommand: Boolean;
var
  AInterval: TdxMisspelledInterval;
begin
  AInterval := GetMisspelledInterval;
  Result := ((AInterval <> nil) and (AInterval.ErrorType = seMisspelling)) and
    SpellChecker.CanAddToDictionary{(GetActualCulture(AInterval))};
end;

{ TdxCheckSpellingCommand }

class function TdxCheckSpellingCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowSpellingDialogDescription);
end;

class function TdxCheckSpellingCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.CheckSpelling;
end;

class function TdxCheckSpellingCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandShowSpellingDialogMenuCaption);
end;

class function TdxCheckSpellingCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.CheckSpelling;
end;

function TdxCheckSpellingCommand.CanExecuteCommand: Boolean;
begin
  Result := SpellChecker <> nil;
end;

procedure TdxCheckSpellingCommand.ExecuteCore;
begin
  SpellChecker.CheckControl(Control);
end;

end.
