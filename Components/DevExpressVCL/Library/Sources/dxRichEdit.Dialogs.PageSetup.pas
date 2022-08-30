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

unit dxRichEdit.Dialogs.PageSetup;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, Generics.Defaults, Generics.Collections,
  dxCore, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxLayoutContainer, dxLayoutControl,
  dxLayoutControlAdapters, cxButtons, dxLayoutcxEditAdapters, cxContainer, cxEdit, cxTextEdit, cxMaskEdit,
  cxSpinEdit, dxMeasurementUnitEdit, cxLabel, cxRadioGroup, cxDropDownEdit, cxCheckBox, dxLayoutLookAndFeels, cxClasses,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Dialogs.PageSetupController,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Utils.Types;

type
  TdxRichEditPageSetupDialogForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cbDifferentFirstPage: TcxCheckBox;
    cbDifferentOddAndEvenPage: TcxCheckBox;
    cmbLayoutApplyTo: TcxComboBox;
    cmbMarginsApplyTo: TcxComboBox;
    cmbPaperApplyTo: TcxComboBox;
    cmbPaperSize: TcxComboBox;
    cmbSectionStart: TcxComboBox;
    dxLayoutControl1Group1: TdxLayoutGroup;
    dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group5: TdxLayoutGroup;
    dxLayoutControl1Group6: TdxLayoutGroup;
    dxLayoutControl1Group7: TdxLayoutGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item13: TdxLayoutItem;
    dxLayoutControl1Item14: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item5: TdxLayoutItem;
    dxLayoutControl1Item6: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    edtMarginBottom: TdxMeasurementUnitEdit;
    edtMarginLeft: TdxMeasurementUnitEdit;
    edtMarginRight: TdxMeasurementUnitEdit;
    edtMarginTop: TdxMeasurementUnitEdit;
    edtPaperHeight: TdxMeasurementUnitEdit;
    edtPaperWidth: TdxMeasurementUnitEdit;
    lcgPageLayout: TdxLayoutGroup;
    lcgPageMargins: TdxLayoutGroup;
    lcgPagePaper: TdxLayoutGroup;
    lcgTabControl: TdxLayoutGroup;
    lciLayoutApplyTo: TdxLayoutItem;
    lciMarginBottom: TdxLayoutItem;
    lciMarginLeft: TdxLayoutItem;
    lciMarginRight: TdxLayoutItem;
    lciMarginsApplyTo: TdxLayoutItem;
    lciMarginTop: TdxLayoutItem;
    lciPaperApplyTo: TdxLayoutItem;
    lciPaperHeight: TdxLayoutItem;
    lciPaperWidth: TdxLayoutItem;
    lciSectionStart: TdxLayoutItem;
    rbLandscape: TcxRadioButton;
    rbPortrait: TcxRadioButton;
    lblMargins: TdxLayoutSeparatorItem;
    lblOrientation: TdxLayoutSeparatorItem;
    lblPaperSize: TdxLayoutSeparatorItem;
    lblSection: TdxLayoutSeparatorItem;
    lblHeadersAndFooters: TdxLayoutSeparatorItem;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    procedure btnOkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure edtPaperWidthPropertiesEditValueChanged(Sender: TObject);
    procedure edtPaperHeightPropertiesEditValueChanged(Sender: TObject);
    procedure edtMarginLeftPropertiesEditValueChanged(Sender: TObject);
    procedure edtMarginRightPropertiesEditValueChanged(Sender: TObject);
    procedure edtMarginTopPropertiesEditValueChanged(Sender: TObject);
    procedure edtMarginBottomPropertiesEditValueChanged(Sender: TObject);
    procedure rbLandscapeChanged(Sender: TObject);
    procedure cmbPaperSizePropertiesChange(Sender: TObject);
    procedure cmbApplyToPropertiesEditValueChanged(Sender: TObject);
  private
    procedure PopulateApplyTo;
    procedure PopulateApplyToComboBox(AComboBox: TcxCustomComboBox);
    procedure PopulatePaperSize;
    procedure PopulateSectionStart;

    procedure AddApplyToComboItem(AItems: TStrings; AApplyType: TdxSectionPropertiesApplyType);
    procedure SyncApplyToCombos(ASelectedIndex: Integer);
    procedure SyncApplyToCombosCore(ASelectedIndex: Integer);
    function GetController: TdxPageSetupFormController; inline;
  protected
    procedure ApplyLocalization; override;
    procedure InitializeForm; override;
    function GetUnitConverter: TdxDocumentModelUnitConverter; override;
    procedure UpdateFormCore; override;

    procedure CommitValuesToController;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
  public
    property Controller: TdxPageSetupFormController read GetController;
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs.Utils,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.Utils.Exceptions.Strs;

{$R *.dfm}

{ TdxRichEditPageSetupDialogForm }

function TdxRichEditPageSetupDialogForm.GetController: TdxPageSetupFormController;
begin
  Result := TdxPageSetupFormController(inherited Controller);
end;

function TdxRichEditPageSetupDialogForm.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := Controller.ValueUnitConverter;
end;

procedure TdxRichEditPageSetupDialogForm.AddApplyToComboItem(AItems: TStrings; AApplyType: TdxSectionPropertiesApplyType);
begin
  if AApplyType in Controller.AvailableApplyType then
    AddItemValue(AItems, cxGetResourceString(dxSectionPropertiesApplyToNames[AApplyType]), Ord(AApplyType));
end;

procedure TdxRichEditPageSetupDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditPageSetupDialogForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  lblMargins.Caption := cxGetResourceString(@sdxRichEditPageSetupDialogMargins);
  lblOrientation.Caption := cxGetResourceString(@sdxRichEditPageSetupDialogOrientation);
  rbPortrait.Caption := cxGetResourceString(@sdxRichEditPageSetupDialogPortrait);
  rbLandscape.Caption := cxGetResourceString(@sdxRichEditPageSetupDialogLandscape);
  lblPaperSize.Caption := cxGetResourceString(@sdxRichEditPageSetupDialogPaperSize);
  lblSection.Caption := cxGetResourceString(@sdxRichEditPageSetupDialogSection);
  lblHeadersAndFooters.Caption := cxGetResourceString(@sdxRichEditPageSetupDialogHeadersAndFooters);
  cbDifferentOddAndEvenPage.Caption := cxGetResourceString(@sdxRichEditPageSetupDialogDifferentOddAndEvenPage);
  cbDifferentFirstPage.Caption := cxGetResourceString(@sdxRichEditPageSetupDialogDifferentFirstPage);
  lcgPageMargins.CaptionOptions.Text := cxGetResourceString(@sdxRichEditPageSetupDialogPageMargins);
  lcgPagePaper.CaptionOptions.Text := cxGetResourceString(@sdxRichEditPageSetupDialogPagePaper);
  lciMarginLeft.CaptionOptions.Text := cxGetResourceString(@sdxRichEditPageSetupDialogMarginLeft);
  lciMarginTop.CaptionOptions.Text := cxGetResourceString(@sdxRichEditPageSetupDialogMarginTop);
  lciMarginBottom.CaptionOptions.Text := cxGetResourceString(@sdxRichEditPageSetupDialogMarginBottom);
  lciMarginRight.CaptionOptions.Text := cxGetResourceString(@sdxRichEditPageSetupDialogMarginRight);
  lciMarginsApplyTo.CaptionOptions.Text := cxGetResourceString(@sdxRichEditPageSetupDialogApplyTo);
  lciPaperWidth.CaptionOptions.Text := cxGetResourceString(@sdxRichEditPageSetupDialogPaperWidth);
  lciPaperHeight.CaptionOptions.Text := cxGetResourceString(@sdxRichEditPageSetupDialogPaperHeight);
  lciPaperApplyTo.CaptionOptions.Text := cxGetResourceString(@sdxRichEditPageSetupDialogApplyTo);
  lcgPageLayout.CaptionOptions.Text := cxGetResourceString(@sdxRichEditPageSetupDialogPageLayout);
  lciSectionStart.CaptionOptions.Text := cxGetResourceString(@sdxRichEditPageSetupDialogSectionStart);
  lciLayoutApplyTo.CaptionOptions.Text := cxGetResourceString(@sdxRichEditPageSetupDialogApplyTo);
end;

procedure TdxRichEditPageSetupDialogForm.btnOkClick(Sender: TObject);
begin
  CommitValuesToController;
  Controller.ApplyChanges;
end;

procedure TdxRichEditPageSetupDialogForm.cmbApplyToPropertiesEditValueChanged(Sender: TObject);
begin
  SyncApplyToCombos(TcxComboBox(Sender).ItemIndex);
end;

procedure TdxRichEditPageSetupDialogForm.cmbPaperSizePropertiesChange(Sender: TObject);
var
  AValue: Integer;
  APaperKind: TdxPaperKind;
begin
  if TryGetItemValue(cmbPaperSize, AValue) then
    APaperKind := TdxPaperKind(AValue)
  else
    APaperKind := TdxPaperKind.Custom;
  Controller.PaperKind := APaperKind;
  UpdateForm;
end;

procedure TdxRichEditPageSetupDialogForm.CommitValuesToController;
var
  AApplyType: Integer;
  ASectionStartType: Integer;
begin
  if TryGetItemValue(cmbMarginsApplyTo, AApplyType) then
    Controller.ApplyType := TdxSectionPropertiesApplyType(AApplyType);

  if TryGetItemValue(cmbSectionStart, ASectionStartType) then
    Controller.SectionStartType := TdxSectionStartType(ASectionStartType);

  if cbDifferentFirstPage.Checked then
    Controller.DifferentFirstPage := True
  else
    if Controller.DifferentFirstPage.HasValue then
      Controller.DifferentFirstPage := False;
  if cbDifferentOddAndEvenPage.Checked then
    Controller.DifferentOddAndEvenPages := True
  else
    if Controller.DifferentOddAndEvenPages.HasValue then
      Controller.DifferentOddAndEvenPages := False;
end;

function TdxRichEditPageSetupDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxPageSetupFormController.Create(AControllerParameters as TdxPageSetupFormControllerParameters);
end;

procedure TdxRichEditPageSetupDialogForm.edtMarginBottomPropertiesEditValueChanged(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := GetValueFromEditor(edtMarginBottom);
  if not VarIsNull(AValue) then
    Controller.BottomMargin := AValue;
end;

procedure TdxRichEditPageSetupDialogForm.edtMarginLeftPropertiesEditValueChanged(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := GetValueFromEditor(edtMarginLeft);
  if not VarIsNull(AValue) then
    Controller.LeftMargin := AValue;
end;

procedure TdxRichEditPageSetupDialogForm.edtMarginRightPropertiesEditValueChanged(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := GetValueFromEditor(edtMarginRight);
  if not VarIsNull(AValue) then
    Controller.RightMargin := AValue;
end;

procedure TdxRichEditPageSetupDialogForm.edtMarginTopPropertiesEditValueChanged(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := GetValueFromEditor(edtMarginTop);
  if not VarIsNull(AValue) then
    Controller.TopMargin := AValue;
end;

procedure TdxRichEditPageSetupDialogForm.edtPaperHeightPropertiesEditValueChanged(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := GetValueFromEditor(edtPaperHeight);
  if not VarIsNull(AValue) then
  begin
    Controller.CustomHeight := AValue;
    Controller.PaperHeight := AValue;
    Controller.UpdatePaperKind;
  end;
end;

procedure TdxRichEditPageSetupDialogForm.edtPaperWidthPropertiesEditValueChanged(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := GetValueFromEditor(edtPaperWidth);
  if not VarIsNull(AValue) then
  begin
    Controller.CustomWidth := AValue;
    Controller.PaperWidth := AValue;
    Controller.UpdatePaperKind;
  end;
end;

procedure TdxRichEditPageSetupDialogForm.InitializeForm;
begin
  PopulateApplyTo;
  UpdateSelectedIndex(cmbMarginsApplyTo, Ord(Controller.ApplyType));
  SyncApplyToCombosCore(cmbMarginsApplyTo.ItemIndex);
  PopulatePaperSize;
  PopulateSectionStart;
  if Controller.SectionStartType.HasValue then
    UpdateSelectedIndex(cmbSectionStart, Ord(Controller.SectionStartType.Value))
  else
    UpdateSelectedIndex(cmbSectionStart, -1);

  InitializeMeasurementUnitEdit(edtMarginLeft, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      ModelUnitsToUIUnit(TdxPageSetupFormDefaults.MinLeftAndRightMarginByDefault),
      ModelUnitsToUIUnit(TdxPageSetupFormDefaults.MaxLeftAndRightMarginByDefault)));
  InitializeMeasurementUnitEdit(edtMarginRight, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      ModelUnitsToUIUnit(TdxPageSetupFormDefaults.MinLeftAndRightMarginByDefault),
      ModelUnitsToUIUnit(TdxPageSetupFormDefaults.MaxLeftAndRightMarginByDefault)));
  InitializeMeasurementUnitEdit(edtMarginTop, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      ModelUnitsToUIUnit(TdxPageSetupFormDefaults.MinTopAndBottomMarginByDefault),
      ModelUnitsToUIUnit(TdxPageSetupFormDefaults.MaxTopAndBottomMarginByDefault)));
  InitializeMeasurementUnitEdit(edtMarginBottom, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      ModelUnitsToUIUnit(TdxPageSetupFormDefaults.MinTopAndBottomMarginByDefault),
      ModelUnitsToUIUnit(TdxPageSetupFormDefaults.MaxTopAndBottomMarginByDefault)));
  InitializeMeasurementUnitEdit(edtPaperWidth, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      ModelUnitsToUIUnit(TdxPageSetupFormDefaults.MinPaperWidthAndHeightByDefault),
      ModelUnitsToUIUnit(TdxPageSetupFormDefaults.MaxPaperWidthAndHeightByDefault)));
  InitializeMeasurementUnitEdit(edtPaperHeight, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      ModelUnitsToUIUnit(TdxPageSetupFormDefaults.MinPaperWidthAndHeightByDefault),
      ModelUnitsToUIUnit(TdxPageSetupFormDefaults.MaxPaperWidthAndHeightByDefault)));

  cbDifferentFirstPage.Checked := Controller.DifferentFirstPage.HasValue and Controller.DifferentFirstPage.Value;
  cbDifferentOddAndEvenPage.Checked := Controller.DifferentOddAndEvenPages.HasValue and Controller.DifferentOddAndEvenPages.Value;

  lcgTabControl.ItemIndex := Ord(Controller.InitialTabPage);
end;

procedure TdxRichEditPageSetupDialogForm.PopulateApplyTo;
begin
  Populate(cmbMarginsApplyTo, PopulateApplyToComboBox);
  cmbPaperApplyTo.Properties.Items.Assign(cmbMarginsApplyTo.Properties.Items);
  cmbLayoutApplyTo.Properties.Items.Assign(cmbMarginsApplyTo.Properties.Items);
end;

procedure TdxRichEditPageSetupDialogForm.PopulateApplyToComboBox(AComboBox: TcxCustomComboBox);
var
  AItems: TStrings;
begin
  AItems := AComboBox.Properties.Items;
  AItems.BeginUpdate;
  try
    AddApplyToComboItem(AItems, TdxSectionPropertiesApplyType.WholeDocument);
    AddApplyToComboItem(AItems, TdxSectionPropertiesApplyType.CurrentSection);
    AddApplyToComboItem(AItems, TdxSectionPropertiesApplyType.SelectedSections);
    AddApplyToComboItem(AItems, TdxSectionPropertiesApplyType.ThisPointForward);
  finally
    AItems.EndUpdate;
  end;
end;

procedure TdxRichEditPageSetupDialogForm.PopulatePaperSize;
begin
  Populate(cmbPaperSize, procedure(AComboBox: TcxCustomComboBox)
    var
      AItems: TStrings;
      APaperKind: TdxPaperKind;
      APaperKindList: TdxPaperKindList;
      ADisplayName: string;
      I: Integer;
    begin
      APaperKindList := Controller.FullPaperKindList;
      AItems := AComboBox.Properties.Items;
      for I := 0 to APaperKindList.Count - 1 do
      begin
        APaperKind := APaperKindList[I];
        ADisplayName := cxGetResourceString(dxPaperKindNames[APaperKind]);
        AddItemValue(AItems, ADisplayName, Ord(APaperKind));
      end;
      ADisplayName := cxGetResourceString(dxPaperKindNames[TdxPaperKind.Custom]);
      AddItemValue(AItems, ADisplayName, Ord(TdxPaperKind.Custom));
    end);
end;

procedure TdxRichEditPageSetupDialogForm.PopulateSectionStart;
begin
  Populate(cmbSectionStart, procedure(AComboBox: TcxCustomComboBox)
  begin
    AddItemValue(AComboBox.Properties.Items, cxGetResourceString(@sdxRichEditPageSetupSectionStartContinuous),
      Ord(TdxSectionStartType.Continuous));
    AddItemValue(AComboBox.Properties.Items, cxGetResourceString(@sdxRichEditPageSetupSectionStartColumn),
      Ord(TdxSectionStartType.Column));
    AddItemValue(AComboBox.Properties.Items, cxGetResourceString(@sdxRichEditPageSetupSectionStartNextPage),
      Ord(TdxSectionStartType.NextPage));
    AddItemValue(AComboBox.Properties.Items, cxGetResourceString(@sdxRichEditPageSetupSectionStartEvenPage),
      Ord(TdxSectionStartType.EvenPage));
    AddItemValue(AComboBox.Properties.Items, cxGetResourceString(@sdxRichEditPageSetupSectionStartOddPage),
      Ord(TdxSectionStartType.OddPage));
  end);
end;

procedure TdxRichEditPageSetupDialogForm.rbLandscapeChanged(Sender: TObject);
begin
  Controller.Landscape := rbLandscape.Checked;
  UpdateForm;
end;

procedure TdxRichEditPageSetupDialogForm.SubscribeControlsEvents;
begin
  edtPaperWidth.Properties.OnChange := edtPaperWidthPropertiesEditValueChanged;
  edtPaperHeight.Properties.OnChange := edtPaperHeightPropertiesEditValueChanged;
  edtMarginLeft.Properties.OnChange := edtMarginLeftPropertiesEditValueChanged;
  edtMarginRight.Properties.OnChange := edtMarginRightPropertiesEditValueChanged;
  edtMarginTop.Properties.OnChange := edtMarginTopPropertiesEditValueChanged;
  edtMarginBottom.Properties.OnChange := edtMarginBottomPropertiesEditValueChanged;
  rbPortrait.OnClick := rbLandscapeChanged;
  rbLandscape.OnClick := rbLandscapeChanged;
  cmbPaperSize.Properties.OnChange := cmbPaperSizePropertiesChange;
  cmbMarginsApplyTo.Properties.OnChange := cmbApplyToPropertiesEditValueChanged;
  cmbPaperApplyTo.Properties.OnChange := cmbApplyToPropertiesEditValueChanged;
  cmbLayoutApplyTo.Properties.OnChange := cmbApplyToPropertiesEditValueChanged;
end;

procedure TdxRichEditPageSetupDialogForm.SyncApplyToCombos(ASelectedIndex: Integer);
begin
  UnsubscribeControlsEvents;
  try
    SyncApplyToCombosCore(ASelectedIndex);
  finally
    SubscribeControlsEvents;
  end;
end;

procedure TdxRichEditPageSetupDialogForm.SyncApplyToCombosCore(ASelectedIndex: Integer);
begin
  cmbMarginsApplyTo.ItemIndex := ASelectedIndex;
  cmbPaperApplyTo.ItemIndex := ASelectedIndex;
  cmbLayoutApplyTo.ItemIndex := ASelectedIndex;
end;

procedure TdxRichEditPageSetupDialogForm.UnsubscribeControlsEvents;
begin
  edtPaperWidth.Properties.OnChange := nil;
  edtPaperHeight.Properties.OnChange := nil;
  edtMarginLeft.Properties.OnChange := nil;
  edtMarginRight.Properties.OnChange := nil;
  edtMarginTop.Properties.OnChange := nil;
  edtMarginBottom.Properties.OnChange := nil;
  rbPortrait.OnClick := nil;
  rbLandscape.OnClick := nil;
  cmbPaperSize.Properties.OnChange := nil;
  cmbMarginsApplyTo.Properties.OnChange := nil;
  cmbPaperApplyTo.Properties.OnChange := nil;
  cmbLayoutApplyTo.Properties.OnChange := nil;
end;

procedure TdxRichEditPageSetupDialogForm.UpdateFormCore;
begin
  rbPortrait.Checked := Controller.Landscape.HasValue and not Controller.Landscape.Value;
  rbLandscape.Checked := Controller.Landscape.HasValue and Controller.Landscape.Value;

  SetValueToEditor(edtMarginLeft, Controller.LeftMargin);
  SetValueToEditor(edtMarginRight, Controller.RightMargin);
  SetValueToEditor(edtMarginTop, Controller.TopMargin);
  SetValueToEditor(edtMarginBottom, Controller.BottomMargin);
  SetValueToEditor(edtPaperWidth, Controller.PaperWidth);
  SetValueToEditor(edtPaperHeight, Controller.PaperHeight);

  if Controller.PaperKind.HasValue then
    UpdateSelectedIndex(cmbPaperSize, Ord(Controller.PaperKind.Value))
  else
    UpdateSelectedIndex(cmbPaperSize, -1);
end;

procedure TdxRichEditPageSetupDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  AText: string;
begin
  if ModalResult <> mrOk then
    Exit;

  if not (edtMarginLeft.ValidateEdit(True) and edtMarginRight.ValidateEdit(True) and
     edtMarginTop.ValidateEdit(True) and edtMarginBottom.ValidateEdit(True) and
     edtPaperWidth.ValidateEdit(True) and edtPaperHeight.ValidateEdit(True)) then
    CanClose := False
  else
    if not Controller.IsTopBottomMarginsValid then
    begin
      AText := cxGetResourceString(@sdxRichEditExceptionTopBottomSectionMarginsTooLarge);
      Application.MessageBox(PChar(AText), PChar(Application.Title), MB_OK + MB_ICONWARNING);
      CanClose := False;
    end
    else
      if not Controller.IsLeftRightMarginsValid then
      begin
        AText := cxGetResourceString(@sdxRichEditExceptionLeftRightSectionMarginsTooLarge);
        Application.MessageBox(PChar(AText), PChar(Application.Title), MB_OK + MB_ICONWARNING);
        CanClose := False;
      end;
end;

end.
