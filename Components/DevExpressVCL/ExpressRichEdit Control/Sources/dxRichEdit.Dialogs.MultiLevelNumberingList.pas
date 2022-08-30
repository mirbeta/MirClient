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

unit dxRichEdit.Dialogs.MultiLevelNumberingList;

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,
  dxCore, cxClasses, cxGraphics, cxControls, cxLookAndFeels, cxLabel, cxButtons, dxLayoutControl, cxListBox,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutControlAdapters, dxLayoutcxEditAdapters,
  dxLayoutContainer, cxSpinEdit, dxMeasurementUnitEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, dxLayoutLookAndFeels,
  dxRichEditDialogsSimpleControl,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.Control,
  dxRichEdit.Control.Core,
  dxRichEdit.Dialogs.CustomNumberingList,
  dxRichEdit.Dialogs.NumberingFormController;

type
  TdxRichEditMultiLevelNumberingListDialogForm = class(TdxRichEditCustomSimpleNumberingListForm)
    dxLayoutControl1Group1: TdxLayoutGroup;
    dxLayoutControl1Group6: TdxLayoutGroup;
    lbBoxLevel: TcxListBox;
    lciLevel: TdxLayoutItem;
    cmbFollowNumber: TcxComboBox;
    lcilFollowNumberWith: TdxLayoutItem;
  private
    function GetController: TdxMultiLevelNumberingListFormController; inline;
    procedure PopulateFollowNumber;
  protected
    procedure ApplyLocalization; override;
    procedure ChangeFocus(AForward: Boolean = True); override;
    procedure FollowNumberSelectedIndexChanged(Sender: TObject);
    procedure LevelSelectedIndexChanged(Sender: TObject);
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    procedure UpdateFormCore; override;
    procedure UpdateSeparator;
  public
    property Controller: TdxMultiLevelNumberingListFormController read GetController;
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs.Utils,
  dxCharacters;

{$R *.dfm}

{ TdxRichEditMultiLevelNumberingListDialogForm }

function TdxRichEditMultiLevelNumberingListDialogForm.GetController: TdxMultiLevelNumberingListFormController;
begin
  Result := TdxMultiLevelNumberingListFormController(inherited Controller);
end;

procedure TdxRichEditMultiLevelNumberingListDialogForm.PopulateFollowNumber;
begin
  Populate(cmbFollowNumber, procedure(ACombobox: TcxCustomComboBox)
    var
      I: Integer;
    begin
      for I := Low(dxListFollowNumberNames) to High(dxListFollowNumberNames) do
        ACombobox.Properties.Items.Add(cxGetResourceString(dxListFollowNumberNames[I]));
    end);
end;

procedure TdxRichEditMultiLevelNumberingListDialogForm.ApplyLocalization;
begin
  inherited ApplyLocalization;
  Caption := cxGetResourceString(@sdxRichEditMultiLevelNumberingListDialogForm);
  lblNumberFormat.Caption := StripHotkey(cxGetResourceString(@sdxRichEditCustomNumberingListNumberFormat));
  lciDisplayFormat.CaptionOptions.Text := cxGetResourceString(@sdxRichEditCustomNumberingListDisplayFormat);
  lciLevel.CaptionOptions.Text := cxGetResourceString(@sdxRichEditMultiLevelNumberingListDialogLevel);
  lcilFollowNumberWith.CaptionOptions.Text := cxGetResourceString(@sdxRichEditMultiLevelNumberingListDialogFollowNumberWith);

  PopulateFollowNumber;
end;

procedure TdxRichEditMultiLevelNumberingListDialogForm.ChangeFocus(AForward: Boolean = True);
begin
  if AForward then
    cmbDisplayFormat.SetFocus
  else
    lbBoxLevel.SetFocus;
end;

procedure TdxRichEditMultiLevelNumberingListDialogForm.FollowNumberSelectedIndexChanged(Sender: TObject);
begin
  case cmbFollowNumber.ItemIndex of
    0: Controller.Separator := TdxCharacters.TabMark;
    1: Controller.Separator := ' ';
    2: Controller.Separator := #$0000;
  end;
end;

procedure TdxRichEditMultiLevelNumberingListDialogForm.LevelSelectedIndexChanged(Sender: TObject);
var
  AIndex: Integer;
begin
  AIndex := lbBoxLevel.ItemIndex;
  Controller.ApplyChanges;
  Controller.EditedLevelIndex := AIndex;
  UpdateForm;
end;

procedure TdxRichEditMultiLevelNumberingListDialogForm.SubscribeControlsEvents;
begin
  inherited SubscribeControlsEvents;
  lbBoxLevel.OnClick := LevelSelectedIndexChanged;
  cmbFollowNumber.Properties.OnChange := FollowNumberSelectedIndexChanged;
end;

procedure TdxRichEditMultiLevelNumberingListDialogForm.UnsubscribeControlsEvents;
begin
  inherited UnsubscribeControlsEvents;
  lbBoxLevel.OnClick := nil;
  cmbFollowNumber.Properties.OnChange := nil;
end;

procedure TdxRichEditMultiLevelNumberingListDialogForm.UpdateFormCore;
begin
  inherited UpdateFormCore;
  lbBoxLevel.ItemIndex := Controller.EditedLevelIndex;
  UpdateSeparator;
end;

procedure TdxRichEditMultiLevelNumberingListDialogForm.UpdateSeparator;
begin
  case Controller.Separator of
    TdxCharacters.TabMark:
      cmbFollowNumber.ItemIndex := 0;
    ' ':
      cmbFollowNumber.ItemIndex := 1;
    else
      cmbFollowNumber.ItemIndex := 2;
  end;
end;

end.
