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

unit dxRichEdit.Dialogs.CustomTableOptions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, dxCoreClasses,
  Controls, Forms, Dialogs, dxRichEdit.Dialogs.CustomDialog, cxGraphics, cxControls, cxLookAndFeels, cxCheckBox,
  cxLookAndFeelPainters, dxLayoutContainer, dxLayoutControl, Menus, dxLayoutControlAdapters, StdCtrls, cxButtons,
  dxLayoutcxEditAdapters, cxContainer, cxEdit, cxLabel, cxTextEdit, cxMaskEdit, cxSpinEdit, dxMeasurementUnitEdit,
  dxRichEdit.Utils.Types, dxLayoutLookAndFeels, cxClasses;

type

  { TdxTableOptionsFormHelper }

  TdxTableOptionsFormHelper = class abstract
  public const
    MaxTopMarginByDefault = 22 * 1440;
    MinTopMarginByDefault = 0;
    MaxRightMarginByDefault = 22 * 1440;
    MinRightMarginByDefault = 0;
    MaxBottomMarginByDefault = 22 * 1440;
    MinBottomMarginByDefault = 0;
    MaxLeftMarginByDefault = 22 * 1440;
    MinLeftMarginByDefault = 0;
    MaxCellSpacingByDefault = 7920;
    MinCellSpacingByDefault = 0;
    MinCellSpacing = 20;
  end;

  { TdxRichEditCustomTableOptionsDialogForm }

  TdxRichEditCustomTableOptionsDialogForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControlGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutControlGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutControlGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutControlMainGroup: TdxLayoutGroup;
    dxLayoutControlMarginsGroup: TdxLayoutGroup;
    edtBottomMargin: TdxMeasurementUnitEdit;
    edtLeftMargin: TdxMeasurementUnitEdit;
    edtRightMargin: TdxMeasurementUnitEdit;
    edtTopMargin: TdxMeasurementUnitEdit;
    lciBottomMargin: TdxLayoutItem;
    lciLeftMargin: TdxLayoutItem;
    lciRightMargin: TdxLayoutItem;
    lciTopMargin: TdxLayoutItem;
    lblMargins: TdxLayoutSeparatorItem;
  private
  protected
    procedure ApplyLocalization; override;
    procedure InitializeForm; override;
    procedure SetMarginControlsEnabled(AValue: Boolean);
    class procedure UpdateCheckEdit(ACheckEdit: TcxCheckBox; const ACheck: TdxNullableBoolean); static;
    class function CheckStateToNullableBool(ACheckState: TcxCheckBoxState): TdxNullableValue<Boolean>; static;
  public
  end;

var
  dxRichEditCustomTableOptionsDialogForm: TdxRichEditCustomTableOptionsDialogForm;

implementation

uses
  dxCore, dxRichEdit.Dialogs.Strs;

{$R *.dfm}

{ TdxRichEditCustomTableOptionsDialogForm }

procedure TdxRichEditCustomTableOptionsDialogForm.ApplyLocalization;
begin
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  lciTopMargin.CaptionOptions.Text := cxGetResourceString(@sdxRichEditCustomTableOptionsDialogTopMargin);
  lciLeftMargin.CaptionOptions.Text := cxGetResourceString(@sdxRichEditCustomTableOptionsDialogLeftMargin);
  lciRightMargin.CaptionOptions.Text := cxGetResourceString(@sdxRichEditCustomTableOptionsDialogRightMargin);
  lciBottomMargin.CaptionOptions.Text := cxGetResourceString(@sdxRichEditCustomTableOptionsDialogBottomMargin);
end;

class function TdxRichEditCustomTableOptionsDialogForm.CheckStateToNullableBool(
  ACheckState: TcxCheckBoxState): TdxNullableValue<Boolean>;
begin
  case ACheckState of
  cbsUnchecked:
    Result := False;
  cbsChecked:
    Result := True;
  cbsGrayed:
    Result := TdxNullableBoolean.Null;
  end;
end;

procedure TdxRichEditCustomTableOptionsDialogForm.InitializeForm;
var
  AMeasurementType: TdxMeasurementType;
begin
  AMeasurementType := ToMeasurementType(UnitType);
  InitializeMeasurementUnitEdit(edtTopMargin, AMeasurementType,
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      ModelUnitsToUIUnit(TdxTableOptionsFormHelper.MinTopMarginByDefault), ModelUnitsToUIUnit(TdxTableOptionsFormHelper.MaxTopMarginByDefault)));
  InitializeMeasurementUnitEdit(edtRightMargin, AMeasurementType,
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      ModelUnitsToUIUnit(TdxTableOptionsFormHelper.MinRightMarginByDefault), ModelUnitsToUIUnit(TdxTableOptionsFormHelper.MaxRightMarginByDefault)));
  InitializeMeasurementUnitEdit(edtBottomMargin, AMeasurementType,
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      ModelUnitsToUIUnit(TdxTableOptionsFormHelper.MinBottomMarginByDefault), ModelUnitsToUIUnit(TdxTableOptionsFormHelper.MaxBottomMarginByDefault)));
  InitializeMeasurementUnitEdit(edtLeftMargin, AMeasurementType,
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      ModelUnitsToUIUnit(TdxTableOptionsFormHelper.MinLeftMarginByDefault), ModelUnitsToUIUnit(TdxTableOptionsFormHelper.MaxLeftMarginByDefault)));
end;

procedure TdxRichEditCustomTableOptionsDialogForm.SetMarginControlsEnabled(AValue: Boolean);
begin
  edtTopMargin.Enabled := AValue;
  edtRightMargin.Enabled := AValue;
  edtBottomMargin.Enabled := AValue;
  edtLeftMargin.Enabled := AValue;
end;

class procedure TdxRichEditCustomTableOptionsDialogForm.UpdateCheckEdit(ACheckEdit: TcxCheckBox;
  const ACheck: TdxNullableBoolean);
begin
  if ACheck.IsNull then
  begin
    ACheckEdit.Properties.AllowGrayed := True;
    ACheckEdit.State := cbsGrayed;
  end
  else
    if ACheck.Value then
      ACheckEdit.State := cbsChecked
    else
      ACheckEdit.State := cbsUnchecked;
end;

end.
