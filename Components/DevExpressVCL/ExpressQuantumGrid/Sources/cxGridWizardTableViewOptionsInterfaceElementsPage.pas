{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridWizardTableViewOptionsInterfaceElementsPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters,
  dxLayoutContainer, cxGroupBox, ExtCtrls, cxTextEdit, cxMaskEdit, cxClasses, cxDropDownEdit, cxCheckBox, Math,
  dxLayoutControl, cxGridWizardCustomPage, cxGridWizardStrs, cxGridTableView, dxLayoutLookAndFeels, cxLabel,
  cxGridWizardCustomHelper, cxGridInplaceEditForm, cxCheckComboBox, cxGridCustomTableView, cxNavigator;

type
  { TcxGridWizardTableViewOptionsInterfaceElementsPageFrame }

  TcxGridWizardTableViewOptionsInterfaceElementsPageFrame = class(TcxGridWizardCustomPageFrame)
    cbGroupFooterMode: TcxComboBox;
    cbPreviewColumn: TcxComboBox;
    chbBandsQuickCustomization: TcxCheckBox;
    chbColumnsQuickCustomization: TcxCheckBox;
    chbFilterRow: TcxCheckBox;
    chbFooter: TcxCheckBox;
    chbGroupBox: TcxCheckBox;
    chbGroupFooter: TcxCheckBox;
    chbHeader: TcxCheckBox;
    chbHideCurrentRow: TcxCheckBox;
    chbIndicator: TcxCheckBox;
    chbInplaceEditForm: TcxCheckBox;
    chbNavigator: TcxCheckBox;
    chbNewItemRow: TcxCheckBox;
    chbPreview: TcxCheckBox;
    chcbNavigatorButtons: TcxCheckComboBox;
    lbInterfaceElements: TcxLabel;
    lciBandsQuickCustomization: TdxLayoutItem;
    lciColumnsQuickCustomization: TdxLayoutItem;
    lciFilterRow: TdxLayoutItem;
    lciFooter: TdxLayoutItem;
    lciGroupBox: TdxLayoutItem;
    lciGroupFooter: TdxLayoutItem;
    lciHeader: TdxLayoutItem;
    lciHideCurrentRow: TdxLayoutItem;
    lciIndicator: TdxLayoutItem;
    lciInplaceEditForm: TdxLayoutItem;
    lciInterfaceElements: TdxLayoutItem;
    lciNavigator: TdxLayoutItem;
    lciNavigatorButtons: TdxLayoutItem;
    lciNewItemRow: TdxLayoutItem;
    lcInterfaceElementsPageGroup1: TdxLayoutGroup;
    lcInterfaceElementsPageGroup3: TdxLayoutGroup;
    lcInterfaceElementsPageGroup5: TdxLayoutGroup;
    lcInterfaceElementsPageSeparatorItem1: TdxLayoutSeparatorItem;
    lciPreview: TdxLayoutItem;
    lciPreviewColumn: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lcMainGroup1: TdxLayoutGroup;
    lcMainItem1: TdxLayoutItem;
    pnPreviewGrid: TPanel;
    procedure RefreshPreviewGrid(Sender: TObject);
    procedure chbGroupFooterClick(Sender: TObject);
    procedure chbIndicatorClick(Sender: TObject);
    procedure chbPreviewClick(Sender: TObject);
  protected
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
    procedure PopulateColumns;
    procedure PopulateGroupFooterModes;
    procedure PopulateNavigatorButtons(AButtons: TcxGridViewNavigatorButtons);
    procedure SaveNavigatorButtons(AButtons: TcxGridViewNavigatorButtons);
    procedure UpdateControlsState;
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

uses
  cxGridWizardBandedTableViewHelper;

{$R *.dfm}

{ TcxGridWizardTableViewOptionsInterfaceElementsPageFrame }

procedure TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.ApplyLocalization;
begin
  lbInterfaceElements.Caption := cxGetResourceString(@scxgwUIElementsPageGroupInterfaceElements);

  chbFooter.Caption := cxGetResourceString(@scxgwUIElementsPageFooter);
  chbFooter.Hint := cxGetResourceString(@scxgwUIElementsPageFooterHint);

  chbHeader.Caption := cxGetResourceString(@scxgwUIElementsPageHeader);
  chbHeader.Hint := cxGetResourceString(@scxgwUIElementsPageHeaderHint);

  chbGroupFooter.Caption := cxGetResourceString(@scxgwUIElementsPageGroupFooter);
  chbGroupFooter.Hint := cxGetResourceString(@scxgwUIElementsPageGroupFooterHint);

  chbGroupBox.Caption := cxGetResourceString(@scxgwUIElementsPageGroupBox);
  chbGroupBox.Hint := cxGetResourceString(@scxgwUIElementsPageGroupBoxHint);

  chbFilterRow.Caption := cxGetResourceString(@scxgwUIElementsPageFilterRowVisible);
  chbFilterRow.Hint := cxGetResourceString(@scxgwUIElementsPageFilterRowVisibleHint);

  chbIndicator.Caption := cxGetResourceString(@scxgwUIElementsPageIndicator);
  chbIndicator.Hint := cxGetResourceString(@scxgwUIElementsPageIndicatorHint);

  chbBandsQuickCustomization.Caption := cxGetResourceString(@scxgwUIElementsPageBandsQuickCustomization);
  chbBandsQuickCustomization.Hint := cxGetResourceString(@scxgwUIElementsPageBandsQuickCustomizationHint);

  chbColumnsQuickCustomization.Caption := cxGetResourceString(@scxgwUIElementsPageColumnsQuickCustomization);
  chbColumnsQuickCustomization.Hint := cxGetResourceString(@scxgwUIElementsPageColumnsQuickCustomizationHint);

  chbPreview.Caption := cxGetResourceString(@scxgwUIElementsPagePreview);
  chbPreview.Hint := cxGetResourceString(@scxgwUIElementsPagePreviewHint);

  chbNavigator.Caption := cxGetResourceString(@scxgwUIElementsPageNavigator);
  chbNavigator.Hint := cxGetResourceString(@scxgwUIElementsPageNavigatorHint);

  lciNavigatorButtons.Caption := cxGetResourceString(@scxgwUIElementsPageNavigatorButtons);
  chcbNavigatorButtons.Hint := cxGetResourceString(@scxgwUIElementsPageNavigatorButtonsHint);

  chbNewItemRow.Caption := cxGetResourceString(@scxgwUIElementsPageNewItemRow);
  chbNewItemRow.Hint := cxGetResourceString(@scxgwUIElementsPageNewItemRowHint);

  chbInplaceEditForm.Caption := cxGetResourceString(@scxgwUIElementsPageInplaceEditForm);
  chbInplaceEditForm.Hint := cxGetResourceString(@scxgwUIElementsPageInplaceEditFormHint);

  chbHideCurrentRow.Caption := cxGetResourceString(@scxgwUIElementsPageHideCurrentRow);
  chbHideCurrentRow.Hint := cxGetResourceString(@scxgwUIElementsPageHideCurrentRowHint);
end;

procedure TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.ApplySettings;
var
  ABandedTableViewInterface: IcxGridWizardBandedTableView;
  APrevPreviewColumn: TcxGridColumn;
  AView: TcxGridTableView;
begin
  Helper.Assign(PreviewGrid.ActiveView);

  AView := TcxGridTableView(Helper.GridView);
  AView.OptionsView.Footer := chbFooter.Checked;
  AView.OptionsView.GroupByBox := chbGroupBox.Checked;
  AView.OptionsView.Header := chbHeader.Checked;
  if Helper.GetGridViewType <> gvtServerMode then
    AView.NewItemRow.Visible := chbNewItemRow.Checked;
  AView.FilterRow.Visible := chbFilterRow.Checked;
  AView.OptionsView.Indicator := chbIndicator.Checked;
  AView.OptionsCustomize.ColumnsQuickCustomization := chbColumnsQuickCustomization.Checked;
  if Supports(Helper, IcxGridWizardBandedTableView, ABandedTableViewInterface) then
    ABandedTableViewInterface.SetBandsQuickCustomization(chbBandsQuickCustomization.Checked);
  AView.Navigator.Visible := chbNavigator.Checked;
  SaveNavigatorButtons(AView.Navigator.Buttons);
  AView.Preview.Visible := chbPreview.Checked;

  if not chbGroupFooter.Checked then
    AView.OptionsView.GroupFooters := gfInvisible
  else
    if cbGroupFooterMode.ItemIndex = 0 then
      AView.OptionsView.GroupFooters := gfVisibleWhenExpanded
    else
      AView.OptionsView.GroupFooters := gfAlwaysVisible;

  APrevPreviewColumn := AView.Preview.Column;
  if chbPreview.Checked and (cbPreviewColumn.ItemIndex > -1) then
    AView.Preview.Column := AView.Columns[cbPreviewColumn.ItemIndex]
  else
    AView.Preview.Column := nil;
  if APrevPreviewColumn <> nil then
    APrevPreviewColumn.Visible := True;

  if chbInplaceEditForm.Checked then
    if chbHideCurrentRow.Checked then
      AView.OptionsBehavior.EditMode := emInplaceEditFormHideCurrentRow
    else
      AView.OptionsBehavior.EditMode := emInplaceEditForm
  else
    AView.OptionsBehavior.EditMode := emInplace;
end;

procedure TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.LoadSettings;
var
  AView: TcxGridTableView;
  ABandedTableViewInterface: IcxGridWizardBandedTableView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;

  PopulateGroupFooterModes;
  PopulateColumns;

  lciBandsQuickCustomization.Visible := Supports(Helper, IcxGridWizardBandedTableView, ABandedTableViewInterface);
  lciNewItemRow.Visible := Helper.GetGridViewType <> gvtServerMode;

  AView := TcxGridTableView(Helper.GridView);
  chbFooter.Checked := AView.OptionsView.Footer;
  chbGroupBox.Checked := AView.OptionsView.GroupByBox;
  chbHeader.Checked := AView.OptionsView.Header;
  if lciNewItemRow.Visible then
    chbNewItemRow.Checked := AView.NewItemRow.Visible;
  chbColumnsQuickCustomization.Checked := AView.OptionsCustomize.ColumnsQuickCustomization;
  if lciBandsQuickCustomization.Visible then
    chbBandsQuickCustomization.Checked := ABandedTableViewInterface.GetBandsQuickCustomization;
  chbFilterRow.Checked := AView.FilterRow.Visible;
  chbIndicator.Checked := AView.OptionsView.Indicator or chbColumnsQuickCustomization.Checked or
    chbBandsQuickCustomization.Checked;
  chbNavigator.Checked := AView.Navigator.Visible;
  PopulateNavigatorButtons(AView.Navigator.Buttons);
  chbPreview.Checked := AView.Preview.Visible and (AView.Preview.Column <> nil);
  chbGroupFooter.Checked := AView.OptionsView.GroupFooters <> gfInvisible;
  cbGroupFooterMode.ItemIndex := Ord(AView.OptionsView.GroupFooters = gfAlwaysVisible);
  chbInplaceEditForm.Checked := AView.OptionsBehavior.IsInplaceEditFormMode;
  chbHideCurrentRow.Checked := AView.OptionsBehavior.NeedHideCurrentRow;

  UpdateControlsState;
end;

function TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwUIElementsPageDescription);
end;

function TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwUIElementsPageTitle);
end;

procedure TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.PopulateColumns;
var
  I: Integer;
begin
  cbPreviewColumn.Properties.Items.BeginUpdate;
  try
    cbPreviewColumn.Properties.Items.Clear;
    for I := 0 to Helper.ItemsCount-1 do
      cbPreviewColumn.Properties.Items.Add(Helper.ItemCaption[I]);
  finally
    cbPreviewColumn.Properties.Items.EndUpdate;
  end;
end;

procedure TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.PopulateGroupFooterModes;
begin
  cbGroupFooterMode.Properties.Items.BeginUpdate;
  try
    cbGroupFooterMode.Properties.Items.Clear;
    cbGroupFooterMode.Properties.Items.Add(cxGetResourceString(@scxgwUIElementsPageGroupFooterModeVisibleWhenExpanded));
    cbGroupFooterMode.Properties.Items.Add(cxGetResourceString(@scxgwUIElementsPageGroupFooterModeAlwaysVisible));
  finally
    cbGroupFooterMode.Properties.Items.EndUpdate;
  end;
end;

procedure TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.PopulateNavigatorButtons(AButtons: TcxGridViewNavigatorButtons);
const
  CheckBoxStateMap: array [Boolean] of TcxCheckBoxState = (cbsUnchecked, cbsChecked);
var
  I: Integer;
begin
  chcbNavigatorButtons.Properties.Items.BeginUpdate;
  try
    chcbNavigatorButtons.Properties.Items.Clear;
    for I := 0 to NavigatorButtonCount - 1 do
    begin
      chcbNavigatorButtons.Properties.Items.Add.Description := scxGridWizardNavigatorButtonsNames[I];
      chcbNavigatorButtons.States[I] := CheckBoxStateMap[AButtons.Buttons[I].Visible];
    end;
  finally
    chcbNavigatorButtons.Properties.Items.EndUpdate;
  end;
end;

procedure TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.SaveNavigatorButtons(AButtons: TcxGridViewNavigatorButtons);
const
  CheckBoxStateMap: array [TcxCheckBoxState] of Boolean = (False, True, False);
var
  I: Integer;
begin
  for I := 0 to NavigatorButtonCount - 1 do
    AButtons.Buttons[I].Visible := CheckBoxStateMap[chcbNavigatorButtons.States[I]];
end;

procedure TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.UpdateControlsState;
begin
  lciBandsQuickCustomization.Enabled := chbIndicator.Checked;
  lciColumnsQuickCustomization.Enabled := chbIndicator.Checked;
  lciGroupFooter.Enabled := chbGroupFooter.Checked;
  cbPreviewColumn.Enabled := chbPreview.Checked;
  lciHideCurrentRow.Enabled := chbInplaceEditForm.Checked;
  chcbNavigatorButtons.Enabled := chbNavigator.Checked;
end;

{ Events }

procedure TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
end;

procedure TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.chbGroupFooterClick(Sender: TObject);
begin
  UpdateControlsState;
  RefreshPreviewGrid(Sender);
end;

procedure TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.chbIndicatorClick(Sender: TObject);
begin
  UpdateControlsState;
  if not chbIndicator.Checked then
  begin
    chbBandsQuickCustomization.Checked := False;
    chbColumnsQuickCustomization.Checked := False;
  end;
  RefreshPreviewGrid(Sender);
end;

procedure TcxGridWizardTableViewOptionsInterfaceElementsPageFrame.chbPreviewClick(Sender: TObject);
var
  ATableView: TcxGridTableView;
begin
  ATableView := TcxGridTableView(Helper.GridView);
  UpdateControlsState;
  if cbPreviewColumn.Enabled then
  begin
    if ATableView.Preview.Column <> nil then
      cbPreviewColumn.ItemIndex := cbPreviewColumn.Properties.Items.IndexOf(ATableView.Preview.Column.Caption)
    else
      cbPreviewColumn.ItemIndex := 0;
    if cbPreviewColumn.Tag <> -1 then
    begin
      cbPreviewColumn.ItemIndex := cbPreviewColumn.Tag;
      cbPreviewColumn.Tag := -1;
    end;
  end
  else
  begin
    cbPreviewColumn.Tag := cbPreviewColumn.ItemIndex;
    cbPreviewColumn.ItemIndex := -1;
  end;
end;

end.
