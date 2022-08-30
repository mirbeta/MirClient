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

unit dxRichEdit.Dialogs.TableCellOptions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, dxRichEdit.Dialogs.CustomTableOptions, dxCoreClasses, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, Menus, cxContainer, cxEdit, dxLayoutControlAdapters, dxLayoutcxEditAdapters,
  dxLayoutContainer, cxTextEdit, cxMaskEdit, cxSpinEdit, dxMeasurementUnitEdit, cxLabel, StdCtrls, cxButtons,
  dxLayoutControl, cxCheckBox,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.TableCellOptionsFormController,
  dxRichEdit.DocumentModel.UnitConverter, dxLayoutLookAndFeels, cxClasses;

type
  TdxRichEditTableCellOptionsDialogForm = class(TdxRichEditCustomTableOptionsDialogForm)
    cbFitText: TcxCheckBox;
    cbSameAsWholeTable: TcxCheckBox;
    cbWrapText: TcxCheckBox;
    dxLayoutControl1Item4: TdxLayoutItem;
    dxLayoutControl1Item6: TdxLayoutItem;
    dxLayoutControl1Item7: TdxLayoutItem;
    lblOptions: TdxLayoutSeparatorItem;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function GetController: TdxTableCellOptionsFormController;
    procedure BottomMarginValueChanged(Sender: TObject);
    procedure LeftMarginValueChanged(Sender: TObject);
    procedure RightMarginValueChanged(Sender: TObject);
    procedure SameAsWholeTableChange(Sender: TObject);
    procedure TopMarginValueChanged(Sender: TObject);
    procedure FitTextChanged(Sender: TObject);
    procedure WrapTextChanged(Sender: TObject);
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    procedure UpdateFormCore; override;
  public
    property Controller: TdxTableCellOptionsFormController read GetController;
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Utils.Types,
  dxRichEdit.Dialogs.TablePropertiesFormController;

{$R *.dfm}

{ TdxRichEditTableCellOptionsDialogForm }

procedure TdxRichEditTableCellOptionsDialogForm.ApplyLocalization;
begin
  inherited ApplyLocalization;
  Caption := cxGetResourceString(@sdxRichEditTableCellOptionsDialogForm);
  lblMargins.Caption := cxGetResourceString(@sdxRichEditTableCellOptionsDialogMargins);
  cbSameAsWholeTable.Caption := cxGetResourceString(@sdxRichEditTableCellOptionsDialogSameAsWholeTable);
  lblOptions.Caption := cxGetResourceString(@sdxRichEditTableCellOptionsDialogOptions);
  cbWrapText.Caption := cxGetResourceString(@sdxRichEditTableCellOptionsDialogWrapText);
  cbFitText.Caption := cxGetResourceString(@sdxRichEditTableCellOptionsDialogFitText);
end;

procedure TdxRichEditTableCellOptionsDialogForm.BottomMarginValueChanged(Sender: TObject);
var
  AValue: Integer;
begin
  if TryGetValueFromEditor(edtBottomMargin, AValue) then
    Controller.BottomMargin := AValue;
end;

procedure TdxRichEditTableCellOptionsDialogForm.SameAsWholeTableChange(Sender: TObject);
var
  ACheck: Boolean;
begin
  ACheck := cbSameAsWholeTable.Checked;
  Controller.CellMarginsSameAsTable := ACheck;
  SetMarginControlsEnabled(not ACheck);
end;

function TdxRichEditTableCellOptionsDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxTableCellOptionsFormController.Create(AControllerParameters as TdxTableCellOptionsFormControllerParameters);
end;

procedure TdxRichEditTableCellOptionsDialogForm.FitTextChanged(Sender: TObject);
begin
  Controller.FitText := CheckStateToNullableBool(cbFitText.State);
end;

procedure TdxRichEditTableCellOptionsDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  function IsValidEdit(AEdit: TcxCustomEdit): Boolean;
  begin
    if AEdit.Enabled then
      Result := AEdit.ValidateEdit(True)
    else
      Result := True;
  end;

begin
  if ModalResult <> mrOk then
    Exit;
  CanClose := IsValidEdit(edtTopMargin) and IsValidEdit(edtBottomMargin) and
    IsValidEdit(edtLeftMargin) and IsValidEdit(edtRightMargin);
  if CanClose then
    Controller.ApplyChanges;
end;

function TdxRichEditTableCellOptionsDialogForm.GetController: TdxTableCellOptionsFormController;
begin
  Result := TdxTableCellOptionsFormController(inherited Controller);
end;

procedure TdxRichEditTableCellOptionsDialogForm.LeftMarginValueChanged(Sender: TObject);
var
  AValue: Integer;
begin
  if TryGetValueFromEditor(edtLeftMargin, AValue) then
    Controller.LeftMargin := AValue;
end;

procedure TdxRichEditTableCellOptionsDialogForm.RightMarginValueChanged(Sender: TObject);
var
  AValue: Integer;
begin
  if TryGetValueFromEditor(edtRightMargin, AValue) then
    Controller.RightMargin := AValue;
end;

procedure TdxRichEditTableCellOptionsDialogForm.SubscribeControlsEvents;
begin
  edtTopMargin.Properties.OnChange := TopMarginValueChanged;
  edtRightMargin.Properties.OnChange := RightMarginValueChanged;
  edtBottomMargin.Properties.OnChange := BottomMarginValueChanged;
  edtLeftMargin.Properties.OnChange := LeftMarginValueChanged;
  cbSameAsWholeTable.Properties.OnChange := SameAsWholeTableChange;
  cbWrapText.Properties.OnChange := WrapTextChanged;
  cbFitText.Properties.OnChange := FitTextChanged;
end;

procedure TdxRichEditTableCellOptionsDialogForm.TopMarginValueChanged(Sender: TObject);
var
  AValue: Integer;
begin
  if TryGetValueFromEditor(edtTopMargin, AValue) then
    Controller.TopMargin := AValue;
end;

procedure TdxRichEditTableCellOptionsDialogForm.UnsubscribeControlsEvents;
begin
  edtTopMargin.Properties.OnChange := nil;
  edtRightMargin.Properties.OnChange := nil;
  edtBottomMargin.Properties.OnChange := nil;
  edtLeftMargin.Properties.OnChange := nil;
  cbSameAsWholeTable.Properties.OnChange := nil;
  cbWrapText.Properties.OnChange := nil;
  cbFitText.Properties.OnChange := nil;
end;

procedure TdxRichEditTableCellOptionsDialogForm.UpdateFormCore;
var
  ACellMarginsSameAsTable: Boolean;
begin
  SetValueToEditor(edtTopMargin, Controller.TopMargin);
  SetValueToEditor(edtRightMargin, Controller.RightMargin);
  SetValueToEditor(edtBottomMargin, Controller.BottomMargin);
  SetValueToEditor(edtLeftMargin, Controller.LeftMargin);

  ACellMarginsSameAsTable := Controller.CellMarginsSameAsTable;
  cbSameAsWholeTable.Checked := ACellMarginsSameAsTable;
  SetMarginControlsEnabled(not ACellMarginsSameAsTable);

  UpdateCheckEdit(cbWrapText, not Controller.NoWrap);
  UpdateCheckEdit(cbFitText, Controller.FitText);
end;

procedure TdxRichEditTableCellOptionsDialogForm.WrapTextChanged(Sender: TObject);
begin
  Controller.NoWrap := not CheckStateToNullableBool(cbWrapText.State);
end;

end.
