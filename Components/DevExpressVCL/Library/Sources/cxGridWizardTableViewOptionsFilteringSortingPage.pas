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

unit cxGridWizardTableViewOptionsFilteringSortingPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxClasses, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, dxLayoutContainer, ExtCtrls, StdCtrls, cxRadioGroup, cxLabel, cxCheckBox, dxLayoutControl,
  cxGridWizardCustomPage, cxGridWizardStrs, cxGridTableView, cxGridCustomTableView, dxLayoutLookAndFeels, cxTextEdit,
  cxMaskEdit, cxDropDownEdit;

type
  { TcxGridWizardTableViewOptionsFilteringSortingPageFrame }

  TcxGridWizardTableViewOptionsFilteringSortingPageFrame = class(TcxGridWizardCustomPageFrame)
    chbColumnFiltering: TcxCheckBox;
    chbColumnSorting: TcxCheckBox;
    chbIncSearch: TcxCheckBox;
    lbFilterBox: TcxCheckBox;
    lbHeaderFilterButtonShowMode: TcxLabel;
    lbOther: TcxLabel;
    lbShowColumnFilterButtons: TcxLabel;
    lcFilteringSortingPageGroup1: TdxLayoutGroup;
    lcFilteringSortingPageGroup2: TdxLayoutGroup;
    lcFilteringSortingPageGroup3: TdxLayoutGroup;
    lcFilteringSortingPageGroup4: TdxLayoutGroup;
    lcFilteringSortingPageGroup5: TdxLayoutGroup;
    lcFilteringSortingPageGroup6: TdxLayoutGroup;
    lcFilteringSortingPageGroup7: TdxLayoutGroup;
    lcFilteringSortingPageGroup8: TdxLayoutGroup;
    lcFilteringSortingPageGroup9: TdxLayoutGroup;
    lcFilteringSortingPageSeparatorItem: TdxLayoutSeparatorItem;
    lcFilteringSortingPageSeparatorItem1: TdxLayoutSeparatorItem;
    lcFilteringSortingPageSeparatorItem2: TdxLayoutSeparatorItem;
    lciAlways: TdxLayoutItem;
    lciColumnFiltering: TdxLayoutItem;
    lciColumnFilteringGroup: TdxLayoutGroup;
    lciColumnSorting: TdxLayoutItem;
    lciFilterBox: TdxLayoutItem;
    lciHeaderFilterButtonShowMode: TdxLayoutItem;
    lciIncSearch: TdxLayoutItem;
    lciNonEmpty: TdxLayoutItem;
    lciOther: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lciShowAlways: TdxLayoutItem;
    lciShowColumnFilterButtons: TdxLayoutItem;
    lciShowModeButton: TdxLayoutItem;
    lciShowModeSmartTag: TdxLayoutItem;
    lciShowWhenSelected: TdxLayoutItem;
    pnPreviewGrid: TPanel;
    rbAlways: TcxRadioButton;
    rbNonEmpty: TcxRadioButton;
    rbShowAlways: TcxRadioButton;
    rbShowModeButton: TcxRadioButton;
    rbShowModeSmartTag: TcxRadioButton;
    rbShowWhenSelected: TcxRadioButton;
    procedure RefreshPreviewGrid(Sender: TObject);
  protected
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
    procedure UpdateControlsState;
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

uses
  Math;

{$R *.dfm}

{ TcxGridWizardTableViewOptionsFilteringSortingPageFrame }

procedure TcxGridWizardTableViewOptionsFilteringSortingPageFrame.ApplyLocalization;
begin
  chbColumnFiltering.Caption := cxGetResourceString(@scxgwFilteringSortingPageColumnFiltering);
  chbColumnFiltering.Hint := cxGetResourceString(@scxgwFilteringSortingPageColumnFilteringHint);

  lbHeaderFilterButtonShowMode.Caption := cxGetResourceString(@scxgwFilteringSortingPageHeaderFilterButtonShowMode);
  lbHeaderFilterButtonShowMode.Hint := cxGetResourceString(@scxgwFilteringSortingPageHeaderFilterButtonShowModeHint);

  rbShowModeButton.Caption := cxGetResourceString(@scxgwFilteringSortingPageHeaderFilterButtonShowModeButton);
  rbShowModeButton.Hint := cxGetResourceString(@scxgwFilteringSortingPageHeaderFilterButtonShowModeButtonHint);

  rbShowModeSmartTag.Caption := cxGetResourceString(@scxgwFilteringSortingPageHeaderFilterButtonShowModeSmartTag);
  rbShowModeSmartTag.Hint := cxGetResourceString(@scxgwFilteringSortingPageHeaderFilterButtonShowModeSmartTagHint);

  lbShowColumnFilterButtons.Caption := cxGetResourceString(@scxgwFilteringSortingPageShowColumnFilterButton);

  rbShowAlways.Caption := cxGetResourceString(@scxgwFilteringSortingPageShowColumnFilterButtonAlways);
  rbShowAlways.Hint := cxGetResourceString(@scxgwFilteringSortingPageShowColumnFilterButtonAlwaysHint);

  rbShowWhenSelected.Caption := cxGetResourceString(@scxgwFilteringSortingPageShowColumnFilterButtonWhenSelected);
  rbShowWhenSelected.Hint := cxGetResourceString(@scxgwFilteringSortingPageShowColumnFilterButtonWhenSelectedHint);

  lbOther.Caption := cxGetResourceString(@scxgwCommonGroupCaptionOthers);
  chbColumnSorting.Caption := cxGetResourceString(@scxgwFilteringSortingPageColumnSorting);
  chbColumnSorting.Hint := cxGetResourceString(@scxgwFilteringSortingPageColumnSortingHint);

  chbIncSearch.Caption := cxGetResourceString(@scxgwFilteringSortingPageIncSearch);
  chbIncSearch.Hint := cxGetResourceString(@scxgwFilteringSortingPageIncSearchHint);

  lbFilterBox.Caption := cxGetResourceString(@scxgwFilteringSortingPageFilterBoxVisible);
  lbFilterBox.Hint := cxGetResourceString(@scxgwFilteringSortingPageFilterBoxVisibleHint);

  rbAlways.Caption := cxGetResourceString(@scxgwFilteringSortingPageFilterBoxVisibleAlways);
  rbAlways.Hint := cxGetResourceString(@scxgwFilteringSortingPageFilterBoxVisibleAlwaysHint);

  rbNonEmpty.Caption := cxGetResourceString(@scxgwFilteringSortingPageFilterBoxVisibleNonEmpty);
  rbNonEmpty.Hint := cxGetResourceString(@scxgwFilteringSortingPageFilterBoxVisibleNonEmptyHint);
end;

procedure TcxGridWizardTableViewOptionsFilteringSortingPageFrame.ApplySettings;
var
  AView: TcxGridTableView;
begin
  Helper.Assign(PreviewGrid.ActiveView);

  AView := TcxGridTableView(Helper.GridView);
  AView.OptionsBehavior.IncSearch := chbIncSearch.Checked;
  AView.OptionsCustomize.ColumnSorting := chbColumnSorting.Checked;
  AView.OptionsCustomize.ColumnFiltering := chbColumnFiltering.Checked;
  if rbShowModeButton.Checked then
    AView.OptionsView.HeaderFilterButtonShowMode := fbmButton
  else
    AView.OptionsView.HeaderFilterButtonShowMode := fbmSmartTag;

  if rbShowAlways.Checked then
    AView.OptionsView.ShowColumnFilterButtons := sfbAlways
  else
    AView.OptionsView.ShowColumnFilterButtons := sfbWhenSelected;

  if not lbFilterBox.Checked then
    AView.FilterBox.Visible := fvNever
  else
    if rbAlways.Checked then
      AView.FilterBox.Visible := fvAlways
    else
      AView.FilterBox.Visible := fvNonEmpty;
end;

procedure TcxGridWizardTableViewOptionsFilteringSortingPageFrame.LoadSettings;
var
  AView: TcxGridTableView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;

  AView := TcxGridTableView(Helper.GridView);
  chbColumnFiltering.Checked := AView.OptionsCustomize.ColumnFiltering;
  rbShowModeButton.Checked := AView.OptionsView.HeaderFilterButtonShowMode = fbmButton;
  rbShowModeSmartTag.Checked := not rbShowModeButton.Checked;
  rbShowAlways.Checked := AView.OptionsView.ShowColumnFilterButtons = sfbAlways;
  rbShowWhenSelected.Checked := not rbShowAlways.Checked;
  lbFilterBox.Checked := AView.FilterBox.Visible <> fvNever;
  rbAlways.Checked := AView.FilterBox.Visible = fvAlways;
  rbNonEmpty.Checked := AView.FilterBox.Visible = fvNonEmpty;
  chbColumnSorting.Checked := AView.OptionsCustomize.ColumnSorting;
  chbIncSearch.Checked := AView.OptionsBehavior.IncSearch;
  lciColumnFilteringGroup.Enabled := chbColumnFiltering.Checked;
  UpdateControlsState;
end;

function TcxGridWizardTableViewOptionsFilteringSortingPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwFilteringSortingPageDescription);
end;

function TcxGridWizardTableViewOptionsFilteringSortingPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwFilteringSortingPageTitle);
end;

procedure TcxGridWizardTableViewOptionsFilteringSortingPageFrame.UpdateControlsState;
begin
  lciColumnFilteringGroup.Enabled := chbColumnFiltering.Checked;
  lciAlways.Enabled := lbFilterBox.Checked;
  lciNonEmpty.Enabled := lbFilterBox.Checked;
end;

{ Events }

procedure TcxGridWizardTableViewOptionsFilteringSortingPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
  UpdateControlsState;
end;

end.
