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

unit dxSpellCheckerOutlookDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, dxCore,
  cxLookAndFeelPainters, cxButtons, cxControls, cxContainer, cxEdit, cxTextEdit, cxMemo, dxSpellChecker, cxClasses,
  dxSpellCheckerBaseForm, cxListBox, cxGraphics, cxLookAndFeels, dxLayoutcxEditAdapters, dxLayoutContainer,
  dxLayoutControlAdapters, dxLayoutControl, dxLayoutLookAndFeels;

type

  { TdxCustomSpellCheckerForm }

  TdxCustomSpellCheckerFormClass = class of TdxCustomSpellCheckerForm;
  TdxCustomSpellCheckerForm = class(TfmSpellCheckerBaseForm)
  private
    FInitialized: Boolean;
    FSpellChecker: TdxCustomSpellChecker;
    function GetCheckMode: TdxSpellCheckerDialogCheckMode;
  protected
    procedure Activate; override;
    procedure Initialize; virtual;
    procedure Localize; override;
    procedure ShowSpellingOptionsDialog; virtual;

    property CheckMode: TdxSpellCheckerDialogCheckMode read GetCheckMode;
    property SpellChecker: TdxCustomSpellChecker read FSpellChecker;
    property Initialized: Boolean read FInitialized;
  public
    constructor CreateEx(ASpellChecker: TdxCustomSpellChecker); virtual;
  end;

  { TfmSpellCheckerOutlookForm }

  TfmSpellCheckerOutlookForm = class(TdxCustomSpellCheckerForm)
    btnAdd: TcxButton;
    btnCancel: TcxButton;
    btnChange: TcxButton;
    btnChangeAll: TcxButton;
    btnIgnore: TcxButton;
    btnIgnoreAll: TcxButton;
    btnOptions: TcxButton;
    btnSuggest: TcxButton;
    btnUndoLast: TcxButton;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    lbChangeTo: TdxLayoutItem;
    lbMisspelled: TdxLayoutItem;
    lbSuggestions: TdxLayoutItem;
    lbxSuggestions: TcxListBox;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    teMisspelledWord: TcxTextEdit;
    teSuggestion: TcxTextEdit;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;

    procedure btnAddClick(Sender: TObject);
    procedure btnChangeAllClick(Sender: TObject);
    procedure btnChangeClick(Sender: TObject);
    procedure btnIgnoreAllClick(Sender: TObject);
    procedure btnIgnoreClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure btnSuggestClick(Sender: TObject);
    procedure btnUndoLastClick(Sender: TObject);
    procedure lbxSuggestionsClick(Sender: TObject);
    procedure teSuggestionPropertiesChange(Sender: TObject);
  strict private
    FHasSuggestions: Boolean;
    FMisspelledWord: string;
    FSuggestionLockChange: Boolean;

    procedure ValidateCaptions;
  protected
    procedure CheckNextMisspelledWord;
    procedure Initialize; override;
    function IsDeleteMisspelled: Boolean;
    procedure Localize; override;
    procedure SetMisspelledWord(const AValue: string); virtual;
    procedure SetSuggestions(ASuggestions: TdxSpellCheckerSuggestionList);
    procedure SetSuggestionText(const AText: string);
    procedure SetMisspelledInfo;

    property HasSuggestions: Boolean read FHasSuggestions;
    property MisspelledWord: string read FMisspelledWord write SetMisspelledWord;
  end;

implementation

{$R *.dfm}

uses
  dxSpellCheckerStrs, dxSpellCheckerDialogs, dxSpellCheckerRules;

{ TdxCustomSpellCheckerForm }

constructor TdxCustomSpellCheckerForm.CreateEx(ASpellChecker: TdxCustomSpellChecker);
begin
  FSpellChecker := ASpellChecker;
  inherited CreateEx;
  Localize;
end;

procedure TdxCustomSpellCheckerForm.Activate;
begin
  FInitialized := False;
  inherited Activate;
  Update;
  ShowHourglassCursor;
  Enabled := False;
  try
    Initialize;
    FInitialized := True;
  finally
    Enabled := True;
    HideHourglassCursor;
  end;
end;

procedure TdxCustomSpellCheckerForm.Initialize;
begin
end;

procedure TdxCustomSpellCheckerForm.Localize;
begin
  Caption := cxGetResourceString(@sdxSpellCheckerSpellingFormCaption);
end;

procedure TdxCustomSpellCheckerForm.ShowSpellingOptionsDialog;
begin
  dxShowSpellingOptionsDialog(SpellChecker, SpellChecker.DialogLookAndFeel);
end;

function TdxCustomSpellCheckerForm.GetCheckMode: TdxSpellCheckerDialogCheckMode;
begin
  Result := TdxSpellCheckerDialogCheckMode(SpellChecker.CheckMode);
end;

{ TfmSpellCheckerOutlookForm }

procedure TfmSpellCheckerOutlookForm.btnAddClick(Sender: TObject);
begin
  SpellChecker.AddWordToUserDictionary(MisspelledWord);
  CheckNextMisspelledWord;
end;

procedure TfmSpellCheckerOutlookForm.btnChangeAllClick(Sender: TObject);
begin
  if IsDeleteMisspelled then
    CheckMode.DeleteAll
  else
  begin
    if not btnSuggest.Enabled and not FHasSuggestions then
      if MessageDlg(cxGetResourceString(@sdxSpellCheckerConfirmUseUnknownWord), mtWarning, [mbYes, mbNo], 0) <> mrYes then
      begin
        teSuggestion.SetFocus;
        Exit;
      end;
    CheckMode.ChangeAll(teSuggestion.Text);
  end;
  CheckNextMisspelledWord;
end;

procedure TfmSpellCheckerOutlookForm.btnChangeClick(Sender: TObject);
begin
  if IsDeleteMisspelled then
    CheckMode.Delete
  else
  begin
    if not btnSuggest.Enabled and not FHasSuggestions then
      if MessageDlg(cxGetResourceString(@sdxSpellCheckerConfirmUseUnknownWord), mtWarning, [mbYes, mbNo], 0) <> mrYes then
      begin
        teSuggestion.SetFocus;
        Exit;
      end;
    CheckMode.Change(teSuggestion.Text);
  end;
  CheckNextMisspelledWord;
end;

procedure TfmSpellCheckerOutlookForm.btnIgnoreAllClick(Sender: TObject);
begin
  CheckMode.IgnoreAll;
  CheckNextMisspelledWord;
end;

procedure TfmSpellCheckerOutlookForm.btnIgnoreClick(Sender: TObject);
begin
  CheckMode.Ignore;
  CheckNextMisspelledWord;
end;

procedure TfmSpellCheckerOutlookForm.btnSuggestClick(Sender: TObject);
begin
  SetSuggestions(CheckMode.GetSuggestions(teSuggestion.Text));
end;

procedure TfmSpellCheckerOutlookForm.btnUndoLastClick(Sender: TObject);
begin
  CheckMode.UndoLast;
  btnUndoLast.Enabled := CheckMode.CanUndo;
end;

procedure TfmSpellCheckerOutlookForm.btnOptionsClick(Sender: TObject);
begin
  ShowSpellingOptionsDialog;
  CheckNextMisspelledWord;
end;

procedure TfmSpellCheckerOutlookForm.CheckNextMisspelledWord;
begin
  if (CheckMode = nil) or not CheckMode.GetNextMisspelledWord then
    ModalResult := mrOk
  else
    SetMisspelledInfo;
end;

procedure TfmSpellCheckerOutlookForm.Initialize;
begin
  SetMisspelledInfo;
  btnAdd.Enabled := SpellChecker.HasEnabledUserDictionary and (Trim(teSuggestion.Text) <> '');
end;

function TfmSpellCheckerOutlookForm.IsDeleteMisspelled: Boolean;
begin
  Result := Length(teSuggestion.Text) = 0;
end;

procedure TfmSpellCheckerOutlookForm.lbxSuggestionsClick(Sender: TObject);
begin
  if lbxSuggestions.ItemIndex <> -1 then
  begin
    teSuggestion.Text := lbxSuggestions.Items[lbxSuggestions.ItemIndex];
    btnSuggest.Enabled := False;
  end;
end;

procedure TfmSpellCheckerOutlookForm.Localize;
begin
  inherited Localize;
  lbMisspelled.Caption := cxGetResourceString(@sdxSpellCheckerNotInDictionary);
  lbChangeTo.Caption := cxGetResourceString(@sdxSpellCheckerChangeTo);
  lbSuggestions.Caption := cxGetResourceString(@sdxSpellCheckerSuggestions);
  btnIgnore.Caption := cxGetResourceString(@sdxSpellCheckerIgnoreButton);
  btnIgnoreAll.Caption := cxGetResourceString(@sdxSpellCheckerIgnoreAllButton);
  btnChange.Caption := cxGetResourceString(@sdxSpellCheckerChangeButton);
  btnChangeAll.Caption := cxGetResourceString(@sdxSpellCheckerChangeAllButton);
  btnAdd.Caption := cxGetResourceString(@sdxSpellCheckerAddButton);
  btnSuggest.Caption := cxGetResourceString(@sdxSpellCheckerSuggestButton);
  btnOptions.Caption := cxGetResourceString(@sdxSpellCheckerOptionsButton);
  btnUndoLast.Caption := cxGetResourceString(@sdxSpellCheckerUndoLastButton);
  btnCancel.Caption := cxGetResourceString(@sdxSpellCheckerCancelButton);
end;

procedure TfmSpellCheckerOutlookForm.SetMisspelledWord(const AValue: string);
begin
  FMisspelledWord := AValue;
  teMisspelledWord.Text := AValue;
  if Initialized then
    SetSuggestionText(AValue);
  SetSuggestions(CheckMode.GetSuggestions(AValue))
end;

procedure TfmSpellCheckerOutlookForm.SetSuggestions(ASuggestions: TdxSpellCheckerSuggestionList);
begin
  FHasSuggestions := ASuggestions.Count > 0;
  lbxSuggestions.Items.BeginUpdate;
  lbxSuggestions.Items.Clear;
  try
    lbxSuggestions.Enabled := FHasSuggestions;
    if FHasSuggestions then
    begin
      ASuggestions.SaveToStrings(lbxSuggestions.Items);
      SetSuggestionText(lbxSuggestions.Items[0]);
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
  ValidateCaptions;
  teSuggestion.SetFocus;
  btnSuggest.Enabled := False;
end;

procedure TfmSpellCheckerOutlookForm.teSuggestionPropertiesChange(Sender: TObject);
begin
  if FSuggestionLockChange then Exit;
  btnSuggest.Enabled := True;
  btnAdd.Enabled := SpellChecker.HasEnabledUserDictionary and (Trim(teSuggestion.Text) <> '');
  ValidateCaptions;
end;

procedure TfmSpellCheckerOutlookForm.SetSuggestionText(const AText: string);
begin
  FSuggestionLockChange := True;
  teSuggestion.Text := AText;
  teSuggestion.SelectAll;
  FSuggestionLockChange := False;
end;

procedure TfmSpellCheckerOutlookForm.SetMisspelledInfo;
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
        teSuggestion.Text := '';
      end;
  end;
  btnUndoLast.Enabled := CheckMode.CanUndo;
end;

procedure TfmSpellCheckerOutlookForm.ValidateCaptions;
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

end.
