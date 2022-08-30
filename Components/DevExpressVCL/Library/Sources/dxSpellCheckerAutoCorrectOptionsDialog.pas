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

unit dxSpellCheckerAutoCorrectOptionsDialog;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, cxButtons,
  dxCore, dxSpellCheckerBaseForm, dxSpellChecker, cxLookAndFeelPainters, cxControls, cxContainer, cxEdit, cxGroupBox,
  cxCheckBox, cxLabel, cxTextEdit, ComCtrls, cxListView, cxGraphics, cxLookAndFeels, dxLayoutLookAndFeels, cxClasses,
  dxLayoutContainer, dxLayoutControl, dxLayoutcxEditAdapters, dxLayoutControlAdapters;

type

  { TdxCustomSpellCheckerAutoCorrectForm }

  TdxCustomSpellCheckerAutoCorrectFormClass = class of TdxCustomSpellCheckerAutoCorrectForm;
  TdxCustomSpellCheckerAutoCorrectForm = class(TfmSpellCheckerBaseForm)
  private
    FOptions: TdxSpellCheckerAutoCorrectOptions;
  protected
    procedure ApplyChanges; virtual;

    property Options: TdxSpellCheckerAutoCorrectOptions read FOptions;
  public
    constructor CreateEx(AOptions: TdxSpellCheckerAutoCorrectOptions); virtual;

    function ShowModal: Integer; override;
  end;

  { TfmSpellCheckerAutoCorrectExceptionsForm }

  TfmSpellCheckerAutoCorrectOptionsForm = class(TdxCustomSpellCheckerAutoCorrectForm)
    btnCancel: TcxButton;
    btnDelete: TcxButton;
    btnExceptions: TcxButton;
    btnOk: TcxButton;
    btnOperation: TcxButton;
    cbAutomaticallyUseSuggestions: TcxCheckBox;
    cbCorrectCapsLock: TcxCheckBox;
    cbCorrectInitialCaps: TcxCheckBox;
    cbCorrectSentenceCaps: TcxCheckBox;
    cbDisableCapsLock: TcxCheckBox;
    cbReplaceTextAsYouType: TcxCheckBox;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    gbCapitalize: TdxLayoutGroup;
    gbReplaceTextAsYouType: TdxLayoutGroup;
    lbReplace: TdxLayoutItem;
    lbWith: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lvReplacements: TcxListView;
    teReplace: TcxTextEdit;
    teWith: TcxTextEdit;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;

    procedure btnDeleteClick(Sender: TObject);
    procedure btnExceptionsClick(Sender: TObject);
    procedure btnOperationClick(Sender: TObject);
    procedure lvReplacementsClick(Sender: TObject);
    procedure teReplacePropertiesChange(Sender: TObject);
  private
    function IsAddOperation: Boolean;
    procedure PopulateReplaces;
    procedure SelectItem;
    procedure SynchronizeDialogWithOptions;
    procedure SynchronizeOptionsWithDialog;
    procedure UpdateButtonsState;
    procedure UpdateButtonsCaption;
  protected
    procedure ApplyChanges; override;
    procedure Initialize; virtual;
    procedure Localize; override;
  public
    constructor CreateEx(AOptions: TdxSpellCheckerAutoCorrectOptions); override;
  end;

implementation

uses
  dxSpellCheckerStrs, dxSpellCheckerUtils, dxSpellCheckerDialogs, DateUtils, dxSpellCheckerCore;

{$R *.dfm}

{ TdxCustomSpellCheckerAutoCorrectForm }

constructor TdxCustomSpellCheckerAutoCorrectForm.CreateEx(AOptions: TdxSpellCheckerAutoCorrectOptions);
begin
  inherited Create(Application);
  FOptions := AOptions;
  Localize;
end;

function TdxCustomSpellCheckerAutoCorrectForm.ShowModal: Integer;
begin
  Result := inherited ShowModal;
  if Result = mrOk then
    ApplyChanges;
end;

procedure TdxCustomSpellCheckerAutoCorrectForm.ApplyChanges;
begin
end;

{ TfmSpellCheckerAutoCorrectExceptionsForm }

constructor TfmSpellCheckerAutoCorrectOptionsForm.CreateEx(AOptions: TdxSpellCheckerAutoCorrectOptions);
begin
  inherited CreateEx(AOptions);
  Initialize;
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.ApplyChanges;
begin
  SynchronizeOptionsWithDialog;
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.Initialize;
begin
  SynchronizeDialogWithOptions;
  PopulateReplaces;
  UpdateButtonsState;
  UpdateButtonsCaption;
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.Localize;
begin
  Caption := cxGetResourceString(@sdxSpellCheckerAutoCorrectOptionsFormCaption);
  btnOk.Caption := cxGetResourceString(@sdxSpellCheckerOkButton);
  btnCancel.Caption := cxGetResourceString(@sdxSpellCheckerCancelButton);
  gbCapitalize.Caption := cxGetResourceString(@sdxSpellCheckerAutoCorrectCapitalize);
  cbCorrectSentenceCaps.Caption := cxGetResourceString(@sdxSpellCheckerAutoCorrectCorrectSentenceCaps);
  cbCorrectInitialCaps.Caption := cxGetResourceString(@sdxSpellCheckerAutoCorrectCorrectInitialCaps);
  cbCorrectCapsLock.Caption := cxGetResourceString(@sdxSpellCheckerAutoCorrectCorrectCapsLock);
  cbDisableCapsLock.Caption := cxGetResourceString(@sdxSpellCheckerAutoCorrectDisableCapsLock);
  cbAutomaticallyUseSuggestions.Caption := cxGetResourceString(@sdxSpellCheckerAutoCorrectAutomaticallyUseSuggestions);
  gbReplaceTextAsYouType.Caption := cxGetResourceString(@sdxSpellCheckerAutoCorrectReplaceTextAsYouType);
  cbReplaceTextAsYouType.Caption := cxGetResourceString(@sdxSpellCheckerActive);
  btnExceptions.Caption := cxGetResourceString(@sdxSpellCheckerExceptionsButton);
  lbReplace.Caption := cxGetResourceString(@sdxSpellCheckerAutoCorrectReplace);
  lbWith.Caption := cxGetResourceString(@sdxSpellCheckerAutoCorrectWith);
  btnOperation.Caption := cxGetResourceString(@sdxSpellCheckerAddButton);
  btnDelete.Caption := cxGetResourceString(@sdxSpellCheckerDeleteButton);
end;

function TfmSpellCheckerAutoCorrectOptionsForm.IsAddOperation: Boolean;
begin
  Result := Options.Replacements.FindReplacement(teReplace.Text) = nil;
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.PopulateReplaces;
var
  I: Integer;
begin
  lvReplacements.Items.BeginUpdate;
  try
    lvReplacements.Items.Clear;
    for I := 0 to Options.Replacements.Count - 1 do
      with lvReplacements.Items.Add do
      begin
        Caption := Options.Replacements[I].Text;
        SubItems.Add(Options.Replacements[I].Replacement);
      end;
  finally
    lvReplacements.Items.EndUpdate;
  end;
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.SelectItem;
var
  I: Integer;
begin
  for I := 0 to lvReplacements.Items.Count - 1 do
    if WideCompareText(teReplace.Text, lvReplacements.Items[I].Caption) = 0 then
      lvReplacements.ItemIndex := I;
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.SynchronizeDialogWithOptions;
begin
  with Options do
  begin
    cbCorrectSentenceCaps.Checked := CorrectSentenceCaps;
    cbCorrectCapsLock.Checked := CorrectCapsLock;
    cbCorrectInitialCaps.Checked := CorrectInitialCaps;
    cbDisableCapsLock.Checked := DisableCapsLock;
    cbReplaceTextAsYouType.Checked := ReplaceTextAsYouType;
    cbAutomaticallyUseSuggestions.Checked := AutomaticallyUseSuggestions;
  end;
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.SynchronizeOptionsWithDialog;
begin
  with Options do
  begin
    CorrectSentenceCaps := cbCorrectSentenceCaps.Checked;
    CorrectCapsLock := cbCorrectCapsLock.Checked;
    CorrectInitialCaps := cbCorrectInitialCaps.Checked;
    DisableCapsLock := cbDisableCapsLock.Checked;
    ReplaceTextAsYouType := cbReplaceTextAsYouType.Checked;
    AutomaticallyUseSuggestions := cbAutomaticallyUseSuggestions.Checked;
  end;
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.UpdateButtonsState;
var
  AItem: TdxSpellCheckerReplacement;
begin
  AItem := Options.Replacements.FindReplacement(teReplace.Text);
  btnDelete.Enabled := AItem <> nil;
  btnOperation.Enabled := (Length(teReplace.Text) > 0) and
    (not btnDelete.Enabled or (WideCompareStr(AItem.Replacement, teWith.Text) <> 0));
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.UpdateButtonsCaption;
begin
  if IsAddOperation then
    btnOperation.Caption := cxGetResourceString(@sdxSpellCheckerAddButton)
  else
    btnOperation.Caption := cxGetResourceString(@sdxSpellCheckerReplaceButton);
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.btnExceptionsClick(Sender: TObject);
begin
  dxShowAutoCorrectExceptionsDialog(Options.SpellChecker);
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.teReplacePropertiesChange(Sender: TObject);
begin
  SelectItem;
  UpdateButtonsState;
  UpdateButtonsCaption;
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.lvReplacementsClick(Sender: TObject);
var
  AItem: TListItem;
begin
  AItem := lvReplacements.ItemFocused;
  if AItem <> nil then
  begin
    teReplace.Text := AItem.Caption;
    if AItem.SubItems.Count > 0 then
      teWith.Text := AItem.SubItems[0];
    UpdateButtonsState;
    UpdateButtonsCaption;
  end;
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.btnOperationClick(Sender: TObject);
var
  AItem: TdxSpellCheckerReplacement;
begin
  AItem := Options.Replacements.FindReplacement(teReplace.Text);
  if AItem <> nil then
    AItem.ChangeReplacement(teWith.Text)
  else
    Options.Replacements.Add(teReplace.Text, teWith.Text);
  PopulateReplaces;
  UpdateButtonsState;
  UpdateButtonsCaption;
end;

procedure TfmSpellCheckerAutoCorrectOptionsForm.btnDeleteClick(Sender: TObject);
var
  AItem: TdxSpellCheckerReplacement;
begin
  AItem := Options.Replacements.FindReplacement(teReplace.Text);
  if AItem <> nil then
  begin
    Options.Replacements.Delete(Options.Replacements.IndexOf(AItem));
    AItem.Free;
  end;
  PopulateReplaces;
  UpdateButtonsState;
  UpdateButtonsCaption;
end;

end.
