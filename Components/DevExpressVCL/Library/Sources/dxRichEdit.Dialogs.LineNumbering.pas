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

unit dxRichEdit.Dialogs.LineNumbering;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutContainer, dxLayoutControl, Menus, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, cxContainer, cxEdit, StdCtrls, cxRadioGroup, cxLabel, cxTextEdit, cxMaskEdit, cxSpinEdit,
  dxMeasurementUnitEdit, cxCheckBox, cxButtons, dxLayoutLookAndFeels, cxClasses,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Dialogs.LineNumberingController,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.Dialogs.Core;

type
  TdxRichEditLineNumberingDialogForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cbAddLineNumbering: TcxCheckBox;
    lcgControlsGroup: TdxLayoutGroup;
    dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item10: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item3: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    dxLayoutControl1Item9: TdxLayoutItem;
    edtCountBy: TcxSpinEdit;
    edtFromText: TdxMeasurementUnitEdit;
    edtStartAt: TcxSpinEdit;
    lciCountBy: TdxLayoutItem;
    lciFromText: TdxLayoutItem;
    lciStartAt: TdxLayoutItem;
    rbNumberingRestartContinuous: TcxRadioButton;
    rbNumberingRestartEachPage: TcxRadioButton;
    rbNumberingRestartEachSection: TcxRadioButton;
    lblNumbering: TdxLayoutLabeledItem;
    procedure cbAddLineNumberingClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure edtStartAtPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
  private
    function GetController: TdxLineNumberingFormController;
    function GetLineNumberingRestart: TdxLineNumberingRestart;
    procedure SetLineNumberingRestart(const Value: TdxLineNumberingRestart);
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure InitializeForm; override;
    procedure UpdateFormCore; override;

    procedure EnableControls(AEnable: Boolean);
    procedure CommitValuesToController;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;

    property LineNumberingRestart: TdxLineNumberingRestart read GetLineNumberingRestart write SetLineNumberingRestart;
  public

    property Controller: TdxLineNumberingFormController read GetController;
  end;

implementation

uses
  Math,
  dxRichEdit.Dialogs.Strs;

{$R *.dfm}

{ TdxRichEditLineNumberingDialogForm }

procedure TdxRichEditLineNumberingDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditLineNumberingDialogForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  cbAddLineNumbering.Caption := cxGetResourceString(@sdxRichEditLineNumberingDialogAddLineNumbering);
  lblNumbering.Caption := cxGetResourceString(@sdxRichEditLineNumberingDialogNumbering);
  rbNumberingRestartEachPage.Caption := cxGetResourceString(@sdxRichEditLineNumberingDialogNumberingRestartEachPage);
  rbNumberingRestartEachSection.Caption := cxGetResourceString(@sdxRichEditLineNumberingDialogNumberingRestartEachSection);
  rbNumberingRestartContinuous.Caption := cxGetResourceString(@sdxRichEditLineNumberingDialogNumberingRestartContinuous);
  lciStartAt.CaptionOptions.Text := cxGetResourceString(@sdxRichEditLineNumberingDialogStartAt);
  lciCountBy.CaptionOptions.Text := cxGetResourceString(@sdxRichEditLineNumberingDialogCountBy);
  lciFromText.CaptionOptions.Text := cxGetResourceString(@sdxRichEditLineNumberingDialogFromText);
end;

procedure TdxRichEditLineNumberingDialogForm.cbAddLineNumberingClick(Sender: TObject);
begin
  EnableControls(cbAddLineNumbering.Checked);
  if cbAddLineNumbering.Checked then
  begin
    if (edtCountBy.EditValue) <= 0 then
      edtCountBy.EditValue := 1;
    CommitValuesToController;
  end;
end;

procedure TdxRichEditLineNumberingDialogForm.CommitValuesToController;
var
  AFromTextValue: Variant;
begin
  if not cbAddLineNumbering.Checked then
    Controller.Step := 0
  else
    Controller.Step := edtCountBy.EditValue;
  Controller.NumberingRestartType := LineNumberingRestart;
  Controller.StartingLineNumber := edtStartAt.EditValue;

  AFromTextValue := GetValueFromEditor(edtFromText);
  if not VarIsNull(AFromTextValue) then
    Controller.Distance := AFromTextValue
  else
    Controller.Distance := 0;
end;

function TdxRichEditLineNumberingDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxLineNumberingFormController.Create(AControllerParameters as TdxLineNumberingFormControllerParameters);
end;

procedure TdxRichEditLineNumberingDialogForm.edtStartAtPropertiesValidate(Sender: TObject; var DisplayValue: Variant;
  var ErrorText: TCaption; var Error: Boolean);
begin
  Error := DoValidate(Sender as TcxSpinEdit);
end;

procedure TdxRichEditLineNumberingDialogForm.EnableControls(AEnable: Boolean);
begin
  lcgControlsGroup.Enabled := AEnable;
end;

procedure TdxRichEditLineNumberingDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then
    Exit;
  CanClose := DoValidate(edtStartAt) and DoValidate(edtCountBy) and edtFromText.ValidateEdit(True);
  if CanClose then
  begin
    CommitValuesToController;
    Controller.ApplyChanges;
  end;
end;

function TdxRichEditLineNumberingDialogForm.GetController: TdxLineNumberingFormController;
begin
  Result := TdxLineNumberingFormController(inherited Controller);
end;

function TdxRichEditLineNumberingDialogForm.GetLineNumberingRestart: TdxLineNumberingRestart;
begin
  if rbNumberingRestartContinuous.Checked then
    Result := TdxLineNumberingRestart.Continuous
  else
    if rbNumberingRestartEachSection.Checked then
      Result := TdxLineNumberingRestart.NewSection
    else
      Result := TdxLineNumberingRestart.NewPage;
end;

procedure TdxRichEditLineNumberingDialogForm.InitializeForm;
begin
  InitializeMeasurementUnitEdit(edtFromText, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2, 0, ModelUnitsToUIUnit(22 * 1440)));
end;

procedure TdxRichEditLineNumberingDialogForm.SetLineNumberingRestart(const Value: TdxLineNumberingRestart);
begin
  case Value of
    TdxLineNumberingRestart.NewPage:
      rbNumberingRestartEachPage.Checked := True;
    TdxLineNumberingRestart.NewSection:
      rbNumberingRestartEachSection.Checked := True;
    TdxLineNumberingRestart.Continuous:
      rbNumberingRestartContinuous.Checked := True;
  end;
end;

procedure TdxRichEditLineNumberingDialogForm.SubscribeControlsEvents;
begin
  cbAddLineNumbering.OnClick := cbAddLineNumberingClick;
end;

procedure TdxRichEditLineNumberingDialogForm.UnsubscribeControlsEvents;
begin
  cbAddLineNumbering.OnClick := nil;
end;

procedure TdxRichEditLineNumberingDialogForm.UpdateFormCore;
begin
  cbAddLineNumbering.Checked := Controller.Step > 0;
  edtStartAt.EditValue := Controller.StartingLineNumber;
  edtCountBy.EditValue := IfThen(Controller.Step > 0, Controller.Step, 1);
  SetValueToEditor(edtFromText, Controller.Distance);
  LineNumberingRestart := Controller.NumberingRestartType;

  EnableControls(cbAddLineNumbering.Checked);
end;

end.
