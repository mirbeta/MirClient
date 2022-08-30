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

unit cxGridWizardCommonOptionsFilteringSortingPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxClasses, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, dxLayoutContainer, StdCtrls, cxRadioGroup, cxLabel, cxCheckBox, ExtCtrls, dxLayoutControl,
  cxGridWizardCustomPage, cxGridWizardStrs, cxGridCustomTableView, cxGroupBox, dxLayoutLookAndFeels;

type
  { TcxGridWizardCommonOptionsFilteringSortingPageFrame }

  TcxGridWizardCommonOptionsFilteringSortingPageFrame = class(TcxGridWizardCustomPageFrame)
    chbFilterBox: TcxCheckBox;
    chbFilterButton: TcxCheckBox;
    lcFilteringSortingPageGroup1: TdxLayoutGroup;
    lcFilteringSortingPageGroup3: TdxLayoutGroup;
    lcFilteringSortingPageSeparatorItem: TdxLayoutSeparatorItem;
    lcgRowFilteringGroup: TdxLayoutGroup;
    lciPreviewGrid: TdxLayoutItem;
    lciRowFiltering: TdxLayoutItem;
    lciShowButtonAlways: TdxLayoutItem;
    lciShowButtonWhenSelected: TdxLayoutItem;
    lcMainGroup1: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutGroup;
    lcMainGroup3: TdxLayoutGroup;
    lcMainGroup5: TdxLayoutGroup;
    lcMainItem1: TdxLayoutItem;
    lcMainItem3: TdxLayoutItem;
    lcMainItem4: TdxLayoutItem;
    lcMainSeparatorItem1: TdxLayoutSeparatorItem;
    pnPreviewGrid: TPanel;
    rbShowAlways: TcxRadioButton;
    rbShowButtonAlways: TcxRadioButton;
    rbShowButtonWhenSelected: TcxRadioButton;
    rbShowNonEmpty: TcxRadioButton;
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

{$R *.dfm}

type
  TcxCustomGridTableOptionsCustomizeAccess = class(TcxCustomGridTableOptionsCustomize);
  TcxCustomGridTableOptionsViewAccess = class(TcxCustomGridTableOptionsView);

{ TcxGridWizardCommonOptionsFilteringSortingPageFrame }

procedure TcxGridWizardCommonOptionsFilteringSortingPageFrame.ApplyLocalization;
begin
  chbFilterBox.Caption := cxGetResourceString(@scxgwFilteringSortingPageFilterBoxVisible);
  chbFilterBox.Hint := cxGetResourceString(@scxgwFilteringSortingPageFilterBoxVisibleHint);

  rbShowAlways.Caption := cxGetResourceString(@scxgwFilteringSortingPageFilterBoxVisibleAlways);
  rbShowAlways.Hint := cxGetResourceString(@scxgwFilteringSortingPageFilterBoxVisibleAlwaysHint);
  rbShowNonEmpty.Caption := cxGetResourceString(@scxgwFilteringSortingPageFilterBoxVisibleNonEmpty);
  rbShowNonEmpty.Hint := cxGetResourceString(@scxgwFilteringSortingPageFilterBoxVisibleNonEmptyHint);

  chbFilterButton.Caption := cxGetResourceString(@scxgwUIElementsPageFilterButton);
  chbFilterButton.Hint := cxGetResourceString(@scxgwUIElementsPageFilterButtonHint);

  rbShowButtonAlways.Caption := cxGetResourceString(@scxgwFilteringSortingPageShowColumnFilterButtonAlways);
  rbShowButtonAlways.Hint := cxGetResourceString(@scxgwFilteringSortingPageShowColumnFilterButtonAlwaysHint);

  rbShowButtonWhenSelected.Caption := cxGetResourceString(@scxgwFilteringSortingPageShowColumnFilterButtonWhenSelected);
  rbShowButtonWhenSelected.Hint := cxGetResourceString(@scxgwFilteringSortingPageShowColumnFilterButtonWhenSelectedHint);
end;

procedure TcxGridWizardCommonOptionsFilteringSortingPageFrame.ApplySettings;
var
  AView: TcxCustomGridTableView;
begin
  Helper.Assign(PreviewGrid.ActiveView);

  AView := TcxCustomGridTableView(Helper.GridView);

  TcxCustomGridTableOptionsCustomizeAccess(AView.OptionsCustomize).ItemFiltering := chbFilterButton.Checked;
  if rbShowButtonAlways.Checked then
    TcxCustomGridTableOptionsViewAccess(AView.OptionsView).ShowItemFilterButtons := sfbAlways
  else
    TcxCustomGridTableOptionsViewAccess(AView.OptionsView).ShowItemFilterButtons := sfbWhenSelected;

  if not chbFilterBox.Checked then
    AView.FilterBox.Visible := fvNever
  else
    if rbShowAlways.Checked then
      AView.FilterBox.Visible := fvAlways
    else
      AView.FilterBox.Visible := fvNonEmpty;
end;

procedure TcxGridWizardCommonOptionsFilteringSortingPageFrame.LoadSettings;
var
  AView: TcxCustomGridTableView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;

  AView := TcxCustomGridTableView(Helper.GridView);
  chbFilterButton.Checked := TcxCustomGridTableOptionsCustomizeAccess(AView.OptionsCustomize).ItemFiltering;
  rbShowButtonAlways.Checked := TcxCustomGridTableOptionsViewAccess(AView.OptionsView).ShowItemFilterButtons = sfbAlways;
  rbShowButtonWhenSelected.Checked := TcxCustomGridTableOptionsViewAccess(AView.OptionsView).ShowItemFilterButtons = sfbWhenSelected;
  rbShowAlways.Checked := AView.FilterBox.Visible = fvAlways;
  rbShowNonEmpty.Checked := AView.FilterBox.Visible = fvNonEmpty;
  chbFilterBox.Checked := AView.FilterBox.Visible <> fvNever;

  UpdateControlsState;
end;

function TcxGridWizardCommonOptionsFilteringSortingPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwFilteringSortingPageDescription);
end;

function TcxGridWizardCommonOptionsFilteringSortingPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwFilteringSortingPageTitle);
end;

procedure TcxGridWizardCommonOptionsFilteringSortingPageFrame.UpdateControlsState;
begin
  lcgRowFilteringGroup.Enabled := chbFilterButton.Checked;
  lcMainItem1.Enabled := chbFilterBox.Checked;
  lcMainItem3.Enabled := chbFilterBox.Checked;
end;

{ Events }

procedure TcxGridWizardCommonOptionsFilteringSortingPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
  UpdateControlsState;
end;

end.
