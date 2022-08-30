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

unit dxSpellCheckerWordDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, cxLookAndFeelPainters, StdCtrls,
  dxCore, cxButtons, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMemo, dxSpellChecker, dxSpellCheckerBaseForm,
  cxListBox, dxSpellCheckerOutlookDialog, cxRichEdit, cxGraphics,
  cxCheckBox, cxMaskEdit, cxDropDownEdit, cxLookAndFeels, dxLayoutcxEditAdapters, dxLayoutContainer,
  dxLayoutControlAdapters, dxLayoutLookAndFeels, cxClasses, dxLayoutControl;

type

  { TfmSpellCheckerWordForm }

  TfmSpellCheckerWordForm = class(TdxCustomSpellCheckerForm)
    btnAdd: TcxButton;
    btnCancel: TcxButton;
    btnChange: TcxButton;
    btnChangeAll: TcxButton;
    btnIgnore: TcxButton;
    btnIgnoreAll: TcxButton;
    btnOptions: TcxButton;
    btnUndo: TcxButton;
    btnUndoEdit: TcxButton;
    lbxSuggestions: TcxListBox;
    reMisspelledText: TcxRichEdit;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup2: TdxLayoutGroup;
    lbMisspelled: TdxLayoutItem;
    lbSuggestions: TdxLayoutItem;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    liUndoEdit: TdxLayoutItem;
    liIgnore: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;

    procedure btnAddClick(Sender: TObject);
    procedure btnChangeAllClick(Sender: TObject);
    procedure btnChangeClick(Sender: TObject);
    procedure btnIgnoreAllClick(Sender: TObject);
    procedure btnIgnoreClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure btnUndoClick(Sender: TObject);
    procedure btnUndoEditClick(Sender: TObject);
    procedure lbxSuggestionsClick(Sender: TObject);
    procedure reMisspelledPropertiesChange(Sender: TObject);
  strict private
    FChangeAll: Boolean;
    FHasSuggestions: Boolean;
    FMisspelledSentence: TdxSpellCheckerSentence;
    FMisspelledWord: string;

    function GetCheckMode: TdxSpellCheckerWordCheckMode;
    function GetIsChanged: Boolean;
    procedure InnerChange(const AWord: string);
    function IsValidSuggestion: Boolean;
    procedure ConfirmUseUnknownWord(const AWord: string);
    procedure SetUndoEditVisible(AVisible: Boolean);
    procedure UpdateMisspelledText;
    procedure ValidateButtonsCaption;
    procedure ValidateChange;
  protected
    procedure CheckNextMisspelledWord;
    procedure Initialize; override;
    function IsDeleteMisspelled: Boolean;
    function IsModifyMisspelledWordOnly(out ANewWord: string): Boolean;
    procedure Localize; override;
    procedure SetMisspelledWord(const AValue: string); virtual;
    procedure SetSuggestions(ASuggestions: TdxSpellCheckerSuggestionList);
    procedure SetMisspelledInfo;
    procedure ValidateButtonsState;

    property CheckMode: TdxSpellCheckerWordCheckMode read GetCheckMode;
    property IsChanged: Boolean read GetIsChanged;
    property HasSuggestions: Boolean read FHasSuggestions;
    property MisspelledSentence: TdxSpellCheckerSentence read FMisspelledSentence;
    property MisspelledWord: string read FMisspelledWord write SetMisspelledWord;
  end;

implementation

{$R *.dfm}

uses
  dxSpellCheckerStrs, RichEdit, dxSpellCheckerDialogs, dxSpellCheckerRules;

const
  ObjectMark = #8;

function DecodeObjectMarks(const S: string): string;
begin
  Result := StringReplace(S, Char(WCH_EMBEDDING), ObjectMark, [rfReplaceAll]);
end;

function EncodeObjectMarks(const S: string): string;
begin
  Result := StringReplace(S, ObjectMark, Char(WCH_EMBEDDING), [rfReplaceAll]);
end;

{ TfmSpellCheckerWordForm }

procedure TfmSpellCheckerWordForm.btnAddClick(Sender: TObject);
begin
  SpellChecker.AddWordToUserDictionary(MisspelledWord);
  CheckNextMisspelledWord;
end;

procedure TfmSpellCheckerWordForm.btnChangeAllClick(Sender: TObject);
begin
  if IsDeleteMisspelled then
  begin
    CheckMode.DeleteAll;
    CheckNextMisspelledWord;
  end
  else
  begin
    FChangeAll := True;
    ValidateChange;
  end;
end;

procedure TfmSpellCheckerWordForm.btnChangeClick(Sender: TObject);
begin
  if IsDeleteMisspelled then
  begin
    CheckMode.Delete;
    CheckNextMisspelledWord;
  end
  else
  begin
    FChangeAll := False;
    ValidateChange;
  end;
end;

procedure TfmSpellCheckerWordForm.btnIgnoreAllClick(Sender: TObject);
begin
  CheckMode.IgnoreAll;
  CheckNextMisspelledWord;
end;

procedure TfmSpellCheckerWordForm.btnIgnoreClick(Sender: TObject);
begin
  CheckMode.Ignore;
  CheckNextMisspelledWord;
end;

procedure TfmSpellCheckerWordForm.btnUndoClick(Sender: TObject);
begin
  CheckMode.UndoLast;
  CheckNextMisspelledWord;
end;

procedure TfmSpellCheckerWordForm.CheckNextMisspelledWord;
begin
  FMisspelledWord := '';
  if CheckMode.GetNextMisspelledWord then
  begin
    SetMisspelledInfo;
    SetUndoEditVisible(False);
  end
  else
    ModalResult := mrOk;
end;

procedure TfmSpellCheckerWordForm.Initialize;
begin
  SetMisspelledInfo;
  SetUndoEditVisible(False);
end;

function TfmSpellCheckerWordForm.IsDeleteMisspelled: Boolean;
begin
  Result := (CheckMode.LastCode = scRepeatedWords) and not IsChanged;
end;

function TfmSpellCheckerWordForm.IsModifyMisspelledWordOnly(out ANewWord: string): Boolean;
var
  APrevPart, ANextPart: string;
  ANewPrevPart, ANewNextPart: string;
  ANextPartStart: Integer;
  ANewLength: Integer;
begin
  APrevPart := Copy(MisspelledSentence.Text, 1, MisspelledSentence.MisspelledWordPositionInText);
  ANextPartStart := MisspelledSentence.MisspelledWordPositionInText + Length(MisspelledWord) + 1;
  ANextPart := Copy(MisspelledSentence.Text, ANextPartStart, MaxInt);

  ANewLength := Length(reMisspelledText.Text);
  ANewPrevPart := Copy(reMisspelledText.Text, 1, Length(APrevPart));
  ANextPartStart := ANewLength - Length(ANextPart) + 1;
  ANewNextPart := Copy(reMisspelledText.Text, ANextPartStart, ANewLength - ANextPartStart + 1);

  Result := (Length(APrevPart) + Length(ANextPart) <= ANewLength) and
    WideSameStr(APrevPart, ANewPrevPart) and WideSameStr(ANextPart, ANewNextPart);

  if Result then
  begin
    ANewWord := Copy(reMisspelledText.Text,
      MisspelledSentence.MisspelledWordPositionInText + 1,
      ANextPartStart - MisspelledSentence.MisspelledWordPositionInText - 1);
  end
  else
    ANewWord := '';
end;

procedure TfmSpellCheckerWordForm.lbxSuggestionsClick(Sender: TObject);
begin
  if (lbxSuggestions.ItemIndex <> -1) and FHasSuggestions then
  begin
    btnIgnore.Default := False;
    btnChange.Default := True;
  end;
end;

procedure TfmSpellCheckerWordForm.Localize;
begin
  inherited Localize;
  lbMisspelled.Caption := cxGetResourceString(@sdxSpellCheckerNotInDictionary);
  lbSuggestions.Caption := cxGetResourceString(@sdxSpellCheckerSuggestions);
  btnIgnore.Caption := cxGetResourceString(@sdxSpellCheckerIgnoreButton);
  btnIgnoreAll.Caption := cxGetResourceString(@sdxSpellCheckerIgnoreAllButton);
  btnChange.Caption := cxGetResourceString(@sdxSpellCheckerChangeButton);
  btnChangeAll.Caption := cxGetResourceString(@sdxSpellCheckerChangeAllButton);
  btnAdd.Caption := cxGetResourceString(@sdxSpellCheckerAddButton);
  btnOptions.Caption := cxGetResourceString(@sdxSpellCheckerOptionsButton);
  btnUndo.Caption := cxGetResourceString(@sdxSpellCheckerUndoButton);
  btnUndoEdit.Caption := cxGetResourceString(@sdxSpellCheckerUndoEditButton);
  btnCancel.Caption := cxGetResourceString(@sdxSpellCheckerCancelButton);
end;

procedure TfmSpellCheckerWordForm.SetMisspelledWord(const AValue: string);
begin
  FMisspelledWord := AValue;
  SetSuggestions(CheckMode.GetSuggestions(AValue));
  UpdateMisspelledText;
end;

procedure TfmSpellCheckerWordForm.SetSuggestions(ASuggestions: TdxSpellCheckerSuggestionList);
begin
  FHasSuggestions := ASuggestions.Count > 0;
  lbxSuggestions.Items.BeginUpdate;
  lbxSuggestions.Items.Clear;
  try
    lbxSuggestions.Enabled := FHasSuggestions;
    if FHasSuggestions then
    begin
      ASuggestions.SaveToStrings(lbxSuggestions.Items);
      lbxSuggestions.ItemIndex := 0;
      btnIgnore.Default := False;
      btnChange.Default := True;
    end
    else
    begin
      lbxSuggestions.Items.Add(cxGetResourceString(@sdxSpellCheckerNoSuggestions));
      btnChange.Default := False;
      btnIgnore.Default := True;
    end;
  finally
    lbxSuggestions.Items.EndUpdate;
    ASuggestions.Free;
  end;
end;

procedure TfmSpellCheckerWordForm.SetMisspelledInfo;
begin
  case CheckMode.LastCode of
    scMisspelled:
      begin
        lbMisspelled.Caption := cxGetResourceString(@sdxSpellCheckerNotInDictionary);
        SetMisspelledWord(CheckMode.MisspelledWord);
      end;
    scRepeatedWords:
      begin
        lbMisspelled.Caption := cxGetResourceString(@sdxSpellCheckerRepeatedWord);
        SetMisspelledWord(CheckMode.MisspelledWord);
      end;
  end;
  ValidateButtonsState;
  ValidateButtonsCaption;
end;

procedure TfmSpellCheckerWordForm.ValidateButtonsState;
begin
  btnChange.Enabled := HasSuggestions or IsDeleteMisspelled or IsChanged;
  btnChangeAll.Enabled := (HasSuggestions or IsDeleteMisspelled or IsChanged) and not IsDeleteMisspelled;
  lbxSuggestions.Enabled := not liUndoEdit.Visible;
  liIgnore.Visible := not liUndoEdit.Visible;
  btnIgnoreAll.Enabled := not liUndoEdit.Visible and not (CheckMode.LastCode = scRepeatedWords);
  btnAdd.Enabled := not liUndoEdit.Visible and (CheckMode.LastCode <> scRepeatedWords) and SpellChecker.HasEnabledUserDictionary;
  btnUndo.Enabled := CheckMode.CanUndo;
end;

procedure TfmSpellCheckerWordForm.InnerChange(const AWord: string);
begin
  if FChangeAll then
    CheckMode.ChangeAll(AWord)
  else
    CheckMode.Change(AWord);
  CheckNextMisspelledWord;
end;

function TfmSpellCheckerWordForm.IsValidSuggestion: Boolean;
begin
  Result := FHasSuggestions and (lbxSuggestions.ItemIndex <> -1);
end;

function TfmSpellCheckerWordForm.GetCheckMode: TdxSpellCheckerWordCheckMode;
begin
  Result := TdxSpellCheckerWordCheckMode(inherited CheckMode);
end;

function TfmSpellCheckerWordForm.GetIsChanged: Boolean;
begin
  Result := liUndoEdit.Visible;
end;

procedure TfmSpellCheckerWordForm.SetUndoEditVisible(AVisible: Boolean);
begin
  liUndoEdit.Visible := AVisible;
  ValidateButtonsState;
  ValidateButtonsCaption;
end;

procedure TfmSpellCheckerWordForm.UpdateMisspelledText;
begin
  FMisspelledSentence := CheckMode.MisspelledSentence;
  reMisspelledText.SelAttributes.Color := clDefault;
  reMisspelledText.SelAttributes.Style := [];
  reMisspelledText.Clear;
  reMisspelledText.Text := DecodeObjectMarks(MisspelledSentence.Text);
  reMisspelledText.Properties.HideSelection := True;
  try
    reMisspelledText.SetSelection(MisspelledSentence.MisspelledWordPositionInText, Length(MisspelledWord));
    reMisspelledText.SelAttributes.Color := clRed;
    reMisspelledText.SelAttributes.Style := [fsBold];
    reMisspelledText.SetSelection(MisspelledSentence.MisspelledWordPositionInText + Length(MisspelledWord), 0);
    reMisspelledText.SetFocus;
  finally
    reMisspelledText.Properties.HideSelection := False;
  end;
end;

procedure TfmSpellCheckerWordForm.ValidateButtonsCaption;
begin
  if IsDeleteMisspelled then
  begin
    btnChange.Caption := cxGetResourceString(@sdxSpellCheckerDeleteButton);
    btnChangeAll.Caption := cxGetResourceString(@sdxSpellCheckerDeleteAllButton);
  end
  else
  begin
    btnChange.Caption := cxGetResourceString(@sdxSpellCheckerChangeButton);
    btnChangeAll.Caption := cxGetResourceString(@sdxSpellCheckerChangeAllButton);
  end;
end;

procedure TfmSpellCheckerWordForm.ValidateChange;
var
  ANewWord: string;
begin
  if IsChanged then
  begin
    if IsModifyMisspelledWordOnly(ANewWord) then
    begin
      if SpellChecker.HasWordInDictionaries(ANewWord) then
        InnerChange(ANewWord)
      else
        ConfirmUseUnknownWord(ANewWord);
    end
    else
    begin
      if FChangeAll then
      begin
        ShowMessage(cxGetResourceString(@sdxSpellCheckerNotUseChangeAll));
        reMisspelledText.SetFocus;
      end
      else
      begin
        CheckMode.ChangeSentence(EncodeObjectMarks(reMisspelledText.Text));
        CheckNextMisspelledWord;
      end;
    end;
  end
  else
  begin
    if IsValidSuggestion then
      InnerChange(lbxSuggestions.Items[lbxSuggestions.ItemIndex]);
  end;
end;

procedure TfmSpellCheckerWordForm.ConfirmUseUnknownWord(const AWord: string);
begin
  if MessageDlg(cxGetResourceString(@sdxSpellCheckerConfirmUseUnknownWord), mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    CheckMode.Change(AWord);
    CheckNextMisspelledWord;
  end
  else
    reMisspelledText.SetFocus;
end;

procedure TfmSpellCheckerWordForm.reMisspelledPropertiesChange(Sender: TObject);
begin
  SetUndoEditVisible(True);
end;

procedure TfmSpellCheckerWordForm.btnUndoEditClick(Sender: TObject);
begin
  UpdateMisspelledText;
  SetUndoEditVisible(False);
end;

procedure TfmSpellCheckerWordForm.btnOptionsClick(Sender: TObject);
begin
  ShowSpellingOptionsDialog;
  CheckNextMisspelledWord;
end;

end.
