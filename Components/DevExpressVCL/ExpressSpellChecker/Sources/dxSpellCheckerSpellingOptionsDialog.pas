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

unit dxSpellCheckerSpellingOptionsDialog;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,
  dxCore, cxLookAndFeelPainters, cxButtons, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMemo, dxSpellChecker, dxSpellCheckerBaseForm,
  cxListBox, dxSpellCheckerOutlookDialog, cxRichEdit, cxGraphics,
  cxCheckBox, cxMaskEdit, cxDropDownEdit, cxGroupBox, cxLabel,
  dxGDIPlusClasses, cxImage, cxCheckComboBox, cxLookAndFeels, dxLayoutcxEditAdapters, dxLayoutControlAdapters,
  dxLayoutContainer, cxClasses, dxLayoutControl, dxLayoutLookAndFeels;

type
  { TfmSpellCheckerSpellingOptionsForm }

  TfmSpellCheckerSpellingOptionsForm = class(TdxCustomSpellCheckerForm)
    btnApply: TcxButton;
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cbIgnoreEmails: TcxCheckBox;
    cbIgnoreMixedCaseWords: TcxCheckBox;
    cbIgnoreUpperCaseWords: TcxCheckBox;
    cbIgnoreUrls: TcxCheckBox;
    cbIgnoreWordsWithNumbers: TcxCheckBox;
    cbIgnoreRepeatedWords: TcxCheckBox;
    ccbLanguages: TcxCheckComboBox;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutGroup1: TdxLayoutGroup;
    gbGeneralOptions: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    gbCustomDictionary: TdxLayoutGroup;
    gbInternationalDictionaries: TdxLayoutGroup;
    lbLanguage: TdxLayoutItem;
    dxLayoutGroup8: TdxLayoutGroup;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutImageItem1: TdxLayoutImageItem;
    dxLayoutImageItem2: TdxLayoutImageItem;
    dxLayoutImageItem3: TdxLayoutImageItem;
    lbInternationalDictionaries: TdxLayoutLabeledItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    btnEdit: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    lbEditCustomDictionary: TdxLayoutLabeledItem;
    dxLayoutGroup2: TdxLayoutGroup;
    procedure cbChange(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
  private
    FIsModified: Boolean;
    function GetDictionaryLanguage(ADictionary: TdxCustomSpellCheckerDictionary): Integer;
    function GetOptions: TdxSpellCheckerSpellingOptions;
    procedure ValidateButtonsState;
  protected
    procedure Initialize; override;
    procedure Localize; override;
    procedure PopulateLanguages;
    procedure Save; virtual;
    procedure SynchronizeDialogWithOptions; virtual;
    procedure SynchronizeOptionsWithDialog; virtual;
  public
    property Options: TdxSpellCheckerSpellingOptions read GetOptions;
  end;

  TfmSpellCheckerSpellingOptionsFormClass = class of TfmSpellCheckerSpellingOptionsForm;

implementation

{$R *.dfm}

uses
  dxSpellCheckerStrs, dxSpellCheckerDialogs, Math;

{ TfmSpellCheckerSpellingOptionsForm }

procedure TfmSpellCheckerSpellingOptionsForm.Initialize;
begin
  inherited Initialize;
  PopulateLanguages;
  SynchronizeDialogWithOptions;
  FIsModified := False;
  ValidateButtonsState;
end;

procedure TfmSpellCheckerSpellingOptionsForm.Localize;
begin
  Caption := cxGetResourceString(@sdxSpellCheckerSpellingOptionsFormCaption);
  gbGeneralOptions.Caption := cxGetResourceString(@sdxSpellCheckerSpellingOptionsGeneralOptionsGroupBox);
  gbCustomDictionary.Caption := cxGetResourceString(@sdxSpellCheckerSpellingOptionsEditCustomDictionaryGroupBox);
  gbInternationalDictionaries.Caption := cxGetResourceString(@sdxSpellCheckerSpellingOptionsInternationalDictionariesGroupBox);
  lbInternationalDictionaries.Caption := cxGetResourceString(@sdxSpellCheckerSpellingOptionsInternationalDictionariesText);
  lbEditCustomDictionary.Caption := cxGetResourceString(@sdxSpellCheckerSpellingOptionsEditCustomDictionaryText);
  lbLanguage.Caption := cxGetResourceString(@sdxSpellCheckerSpellingLanguage);
  cbIgnoreEmails.Caption := cxGetResourceString(@sdxSpellCheckerIgnoreEmails);
  cbIgnoreMixedCaseWords.Caption := cxGetResourceString(@sdxSpellCheckerIgnoreMixedCaseWords);
  cbIgnoreUpperCaseWords.Caption := cxGetResourceString(@sdxSpellCheckerIgnoreUpperCaseWords);
  cbIgnoreUrls.Caption := cxGetResourceString(@sdxSpellCheckerIgnoreUrls);
  cbIgnoreWordsWithNumbers.Caption := cxGetResourceString(@sdxSpellCheckerIgnoreWordsWithNumbers);
  cbIgnoreRepeatedWords.Caption := cxGetResourceString(@sdxSpellCheckerIgnoreRepeatedWords);
  btnOk.Caption := cxGetResourceString(@sdxSpellCheckerOkButton);
  btnCancel.Caption := cxGetResourceString(@sdxSpellCheckerCancelButton);
  btnApply.Caption := cxGetResourceString(@sdxSpellCheckerApplylButton);
  btnEdit.Caption := cxGetResourceString(@sdxSpellCheckerEditButton);
end;

procedure TfmSpellCheckerSpellingOptionsForm.PopulateLanguages;
var
  I: Integer;
  AList: TStringList;
begin
  AList := TStringList.Create;
  try
    SpellChecker.PopulateLanguages(AList);
    with ccbLanguages.Properties.Items do
    begin
      BeginUpdate;
      try
        Clear;
        for I := 0 to AList.Count - 1 do
          with Add do
          begin
            Description := AList[I];
            Tag := Cardinal(AList.Objects[I]);
          end;
      finally
        EndUpdate;
      end;
    end;
  finally
    FreeAndNil(AList);
  end;
end;

procedure TfmSpellCheckerSpellingOptionsForm.Save;
begin
  SynchronizeOptionsWithDialog;
  FIsModified := False;
  ValidateButtonsState;
end;

procedure TfmSpellCheckerSpellingOptionsForm.SynchronizeDialogWithOptions;
var
  I, J: Integer;
  ADictionary: TdxCustomSpellCheckerDictionary;
begin
  with Options do
  begin
    cbIgnoreEmails.Checked := IgnoreEmails;
    cbIgnoreMixedCaseWords.Checked := IgnoreMixedCaseWords;
    cbIgnoreUpperCaseWords.Checked := IgnoreUpperCaseWords;
    cbIgnoreUrls.Checked := IgnoreUrls;
    cbIgnoreWordsWithNumbers.Checked := IgnoreWordsWithNumbers;
    cbIgnoreRepeatedWords.Checked := IgnoreRepeatedWords;
  end;
  ccbLanguages.Value := 0;
  for I := 0 to ccbLanguages.Properties.Items.Count - 1 do
  begin
    for J := 0 to SpellChecker.DictionaryCount - 1 do
    begin
      ADictionary := SpellChecker.Dictionaries[J];
      if (ADictionary is TdxUserSpellCheckerDictionary) or not ADictionary.Enabled then
        Continue;
      if GetDictionaryLanguage(ADictionary) = ccbLanguages.Properties.Items[I].Tag then
      begin
        ccbLanguages.States[I] := cbsChecked;
        Break;
      end;
    end;
  end;
end;

procedure TfmSpellCheckerSpellingOptionsForm.SynchronizeOptionsWithDialog;
var
  I, J: Integer;
  ADictionary: TdxCustomSpellCheckerDictionary;
begin
  with Options do
  begin
    IgnoreEmails := cbIgnoreEmails.Checked;
    IgnoreMixedCaseWords := cbIgnoreMixedCaseWords.Checked;
    IgnoreUpperCaseWords := cbIgnoreUpperCaseWords.Checked;
    IgnoreUrls := cbIgnoreUrls.Checked;
    IgnoreWordsWithNumbers := cbIgnoreWordsWithNumbers.Checked;
    IgnoreRepeatedWords := cbIgnoreRepeatedWords.Checked;
  end;
  for I := 0 to ccbLanguages.Properties.Items.Count - 1 do
  begin
    for J := 0 to SpellChecker.DictionaryCount - 1 do
    begin
      ADictionary := SpellChecker.Dictionaries[J];
      if ADictionary is TdxUserSpellCheckerDictionary then
        Continue;
      if GetDictionaryLanguage(ADictionary) = ccbLanguages.Properties.Items[I].Tag then
        ADictionary.Enabled := ccbLanguages.States[I] = cbsChecked;
    end;
  end;
  SpellChecker.LoadDictionaries(True);
end;

function TfmSpellCheckerSpellingOptionsForm.GetDictionaryLanguage(
  ADictionary: TdxCustomSpellCheckerDictionary): Integer;
begin
  Result := ADictionary.Language;
  if Result = 0 then
    Result := dxLanguages.GetDefaultLanguageLCID;
end;

function TfmSpellCheckerSpellingOptionsForm.GetOptions: TdxSpellCheckerSpellingOptions;
begin
  Result := SpellChecker.SpellingOptions;
end;

procedure TfmSpellCheckerSpellingOptionsForm.ValidateButtonsState;
begin
  btnApply.Enabled := FIsModified;
  btnEdit.Enabled := SpellChecker.FindFirstEnabledUserDictionary <> nil;
end;

procedure TfmSpellCheckerSpellingOptionsForm.cbChange(Sender: TObject);
begin
  FIsModified := True;
  ValidateButtonsState;
end;

procedure TfmSpellCheckerSpellingOptionsForm.btnEditClick(Sender: TObject);
begin
  dxShowCustomDictionaryDialog(SpellChecker.FindFirstEnabledUserDictionary);
end;

procedure TfmSpellCheckerSpellingOptionsForm.btnApplyClick(
  Sender: TObject);
begin
  Save;
end;

end.
