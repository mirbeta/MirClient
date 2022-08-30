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

unit dxRichEdit.Dialogs.TableOptions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, dxRichEdit.Dialogs.CustomTableOptions, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, Menus, cxContainer, cxEdit, dxLayoutControlAdapters, dxLayoutcxEditAdapters,
  dxLayoutContainer, cxTextEdit, cxMaskEdit, cxSpinEdit, dxMeasurementUnitEdit, cxLabel, StdCtrls, cxButtons,
  dxLayoutControl, cxCheckBox,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Dialogs.TableOptionsController, dxLayoutLookAndFeels, cxClasses;

type
  TdxRichEditTableOptionsDialogForm = class(TdxRichEditCustomTableOptionsDialogForm)
    cbAllowCellSpacing: TcxCheckBox;
    cbResizeToFitContent: TcxCheckBox;
    dxLayoutControl1Group1: TdxLayoutGroup;
    dxLayoutControl1Item5: TdxLayoutItem;
    dxLayoutControl1Item6: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    edtSpacingBetweenCells: TdxMeasurementUnitEdit;
    lblDefaultCellSpacing: TdxLayoutSeparatorItem;
    lblOptions: TdxLayoutSeparatorItem;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    procedure edtTopMarginPropertiesChange(Sender: TObject);
    procedure edtBottomMarginPropertiesChange(Sender: TObject);
    procedure edtLeftMarginPropertiesChange(Sender: TObject);
    procedure edtRightMarginPropertiesChange(Sender: TObject);
    procedure edtSpacingBetweenCellsPropertiesChange(Sender: TObject);
    procedure cbAllowCellSpacingClick(Sender: TObject);
    procedure cbResizeToFitContentClick(Sender: TObject);
    function GetController: TdxTableOptionsFormController; inline;
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure InitializeForm; override;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    procedure UpdateFormCore; override;
  public
    property Controller: TdxTableOptionsFormController read GetController;
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs.CustomDialog;

{$R *.dfm}

{ TdxRichEditTableOptionsDialogForm }

procedure TdxRichEditTableOptionsDialogForm.ApplyLocalization;
begin
  inherited ApplyLocalization;
  Caption := cxGetResourceString(@sdxRichEditTableOptionsDialogForm);
  lblMargins.Caption := cxGetResourceString(@sdxRichEditTableOptionsDialogMargins);
  lblDefaultCellSpacing.Caption := cxGetResourceString(@sdxRichEditTableOptionsDialogDefaultCellSpacing);
  cbAllowCellSpacing.Caption := cxGetResourceString(@sdxRichEditTableOptionsDialogAllowCellSpacing);
  lblOptions.Caption := cxGetResourceString(@sdxRichEditTableOptionsDialogOptions);
  cbResizeToFitContent.Caption := cxGetResourceString(@sdxRichEditTableOptionsDialogResizeToFitContent);
end;

procedure TdxRichEditTableOptionsDialogForm.cbAllowCellSpacingClick(Sender: TObject);
var
  ACellSpacingChecked: Boolean;
begin
  ACellSpacingChecked := cbAllowCellSpacing.Checked;
  if ACellSpacingChecked and (GetValueFromEditor(edtSpacingBetweenCells) = 0) then
    SetValueToEditor(edtSpacingBetweenCells, TdxTableOptionsFormHelper.MinCellSpacing);

  Controller.AllowCellSpacing := ACellSpacingChecked;
  edtSpacingBetweenCells.Enabled := ACellSpacingChecked;
end;

procedure TdxRichEditTableOptionsDialogForm.cbResizeToFitContentClick(Sender: TObject);
begin
  Controller.ResizeToFitContent := cbResizeToFitContent.Checked;
end;

function TdxRichEditTableOptionsDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxTableOptionsFormController.Create(AControllerParameters as TdxTableOptionsFormControllerParameters);
end;

procedure TdxRichEditTableOptionsDialogForm.edtBottomMarginPropertiesChange(Sender: TObject);
var
  AValue: Integer;
begin
  if TryGetValueFromEditor(edtBottomMargin, AValue) then
    Controller.BottomMargin := AValue;
end;

procedure TdxRichEditTableOptionsDialogForm.edtLeftMarginPropertiesChange(Sender: TObject);
var
  AValue: Integer;
begin
  if TryGetValueFromEditor(edtLeftMargin, AValue) then
    Controller.LeftMargin := AVAlue;
end;

procedure TdxRichEditTableOptionsDialogForm.edtRightMarginPropertiesChange(Sender: TObject);
var
  AValue: Integer;
begin
  if TryGetValueFromEditor(edtRightMargin, AValue) then
    Controller.RightMargin := AValue;
end;

procedure TdxRichEditTableOptionsDialogForm.edtSpacingBetweenCellsPropertiesChange(Sender: TObject);
var
  AValue: Integer;
begin
  if TryGetValueFromEditor(edtSpacingBetweenCells, AValue) then
    Controller.CellSpacing := AValue;
end;

procedure TdxRichEditTableOptionsDialogForm.edtTopMarginPropertiesChange(Sender: TObject);
var
  AValue: Integer;
begin
  if TryGetValueFromEditor(edtTopMargin, AValue) then
    Controller.TopMargin := AValue;
end;

procedure TdxRichEditTableOptionsDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

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
   IsValidEdit(edtLeftMargin) and IsValidEdit(edtRightMargin) and IsValidEdit(edtSpacingBetweenCells);
  if CanClose then
    Controller.ApplyChanges;
end;

function TdxRichEditTableOptionsDialogForm.GetController: TdxTableOptionsFormController;
begin
  Result := TdxTableOptionsFormController(inherited Controller);
end;

procedure TdxRichEditTableOptionsDialogForm.InitializeForm;
begin
  inherited InitializeForm;
  InitializeMeasurementUnitEdit(edtSpacingBetweenCells, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      ModelUnitsToUIUnit(TdxTableOptionsFormHelper.MinCellSpacingByDefault), ModelUnitsToUIUnit(TdxTableOptionsFormHelper.MaxCellSpacingByDefault)));
end;

procedure TdxRichEditTableOptionsDialogForm.SubscribeControlsEvents;
begin
  edtTopMargin.Properties.OnChange := edtTopMarginPropertiesChange;
  edtRightMargin.Properties.OnChange := edtRightMarginPropertiesChange;
  edtBottomMargin.Properties.OnChange := edtBottomMarginPropertiesChange;
  edtLeftMargin.Properties.OnChange := edtLeftMarginPropertiesChange;
  edtSpacingBetweenCells.Properties.OnChange := edtSpacingBetweenCellsPropertiesChange;
  cbAllowCellSpacing.OnClick := cbAllowCellSpacingClick;
  cbResizeToFitContent.OnClick := cbResizeToFitContentClick;
end;

procedure TdxRichEditTableOptionsDialogForm.UnsubscribeControlsEvents;
begin
  edtTopMargin.Properties.OnChange := nil;
  edtRightMargin.Properties.OnChange := nil;
  edtBottomMargin.Properties.OnChange := nil;
  edtLeftMargin.Properties.OnChange := nil;
  edtSpacingBetweenCells.Properties.OnChange := nil;
  cbAllowCellSpacing.OnClick := nil;
  cbResizeToFitContent.OnClick := nil;
end;

procedure TdxRichEditTableOptionsDialogForm.UpdateFormCore;
var
  AAllowCellSpacing: Boolean;
begin
  SetValueToEditor(edtTopMargin, Controller.TopMargin);
  SetValueToEditor(edtRightMargin, Controller.RightMargin);
  SetValueToEditor(edtBottomMargin, Controller.BottomMargin);
  SetValueToEditor(edtLeftMargin, Controller.LeftMargin);
  SetValueToEditor(edtSpacingBetweenCells, Controller.CellSpacing);
  AAllowCellSpacing := Controller.AllowCellSpacing = True;
  edtSpacingBetweenCells.Enabled := AAllowCellSpacing;
  cbAllowCellSpacing.Checked := AAllowCellSpacing;
  cbResizeToFitContent.Checked := Controller.ResizeToFitContent = True;
end;

end.
