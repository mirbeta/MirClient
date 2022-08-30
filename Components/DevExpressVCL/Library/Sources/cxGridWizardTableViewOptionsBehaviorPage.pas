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

unit cxGridWizardTableViewOptionsBehaviorPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxClasses, cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, dxLayoutContainer, ExtCtrls, dxLayoutControl, cxGridWizardCustomPage, cxGridWizardStrs,
  cxGridTableView, dxLayoutLookAndFeels, cxCheckBox, cxLabel;

type
  { TcxGridWizardTableViewOptionsBehaviorPageFrame }

  TcxGridWizardTableViewOptionsBehaviorPageFrame = class(TcxGridWizardCustomPageFrame)
    chbCellMultiSelect: TcxCheckBox;
    chbCellSelect: TcxCheckBox;
    chbFocusCellOnCycle: TcxCheckBox;
    chbFocusCellOnTab: TcxCheckBox;
    chbGoToNextCellOnEnter: TcxCheckBox;
    chbHideSelection: TcxCheckBox;
    chbHorizontal: TcxCheckBox;
    chbRecordMultiSelect: TcxCheckBox;
    chbVertical: TcxCheckBox;
    lbFocusingOptions: TcxLabel;
    lbGridLines: TcxLabel;
    lbSelectionOptions: TcxLabel;
    lciCellMultiSelect: TdxLayoutItem;
    lciCellSelect: TdxLayoutItem;
    lciFocusCellOnCycle: TdxLayoutItem;
    lciFocusCellOnTab: TdxLayoutItem;
    lciFocusingOptions: TdxLayoutItem;
    lciGoToNextCellOnEnter: TdxLayoutItem;
    lciGridLines: TdxLayoutItem;
    lciHorizontal: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lciRecordMultiSelect: TdxLayoutItem;
    lciSelectionOptions: TdxLayoutItem;
    lciVertical: TdxLayoutItem;
    lcMainGroup1: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutGroup;
    lcMainGroup3: TdxLayoutGroup;
    lcMainItem2: TdxLayoutItem;
    lcMainSeparatorItem1: TdxLayoutSeparatorItem;
    lcSelectionPageGroup1: TdxLayoutGroup;
    lcSelectionPageGroup2: TdxLayoutGroup;
    lcSelectionPageGroup3: TdxLayoutGroup;
    lcSelectionPageGroup5: TdxLayoutGroup;
    lcSelectionPageSeparatorItem1: TdxLayoutSeparatorItem;
    lcSelectionPageSeparatorItem2: TdxLayoutSeparatorItem;
    pnPreviewGrid: TPanel;
    procedure RefreshPreviewGrid(Sender: TObject);
  protected
    function GetGridLines(AHorizontalVisible, AVerticalVisible: Boolean): TcxGridLines;
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

{ TcxGridWizardTableViewOptionsBehaviorPageFrame }

procedure TcxGridWizardTableViewOptionsBehaviorPageFrame.ApplyLocalization;
begin
  lbFocusingOptions.Caption := cxGetResourceString(@scxgwBehaviorPageFocusingOptions);

  chbFocusCellOnCycle.Caption := cxGetResourceString(@scxgwBehaviorPageFocusCellOnCycle);
  chbFocusCellOnCycle.Hint := cxGetResourceString(@scxgwBehaviorPageFocusCellOnCycleHint);

  chbFocusCellOnTab.Caption := cxGetResourceString(@scxgwBehaviorPageFocusCellOnTab);
  chbFocusCellOnTab.Hint := cxGetResourceString(@scxgwBehaviorPageFocusCellOnTabHint);

  chbGoToNextCellOnEnter.Caption := cxGetResourceString(@scxgwBehaviorPageGoToNextCellOnEnter);
  chbGoToNextCellOnEnter.Hint := cxGetResourceString(@scxgwBehaviorPageGoToNextCellOnEnterHint);

  lbSelectionOptions.Caption := cxGetResourceString(@scxgwBehaviorPageSelectionOptions);
  chbCellSelect.Caption := cxGetResourceString(@scxgwBehaviorPageCellSelect);
  chbCellSelect.Hint := cxGetResourceString(@scxgwBehaviorPageCellSelectHint);

  chbCellMultiSelect.Caption := cxGetResourceString(@scxgwBehaviorPageCellMultiSelect);
  chbCellMultiSelect.Hint := cxGetResourceString(@scxgwBehaviorPageCellMultiSelectHint);

  chbRecordMultiSelect.Caption := cxGetResourceString(@scxgwBehaviorPageRowMultiSelect);
  chbRecordMultiSelect.Hint := cxGetResourceString(@scxgwBehaviorPageRowMultiSelectHint);

  chbHideSelection.Caption := cxGetResourceString(@scxgwBehaviorPageHideSelection);
  chbHideSelection.Hint := cxGetResourceString(@scxgwBehaviorPageHideSelectionHint);

  lbGridLines.Caption := cxGetResourceString(@scxgwBehaviorPageGridLines);

  chbHorizontal.Caption := cxGetResourceString(@scxgwBehaviorPageGridLinesHorizontal);
  chbHorizontal.Hint := cxGetResourceString(@scxgwBehaviorPageGridLinesHorizontalHint);

  chbVertical.Caption := cxGetResourceString(@scxgwBehaviorPageGridLinesVertical);
  chbVertical.Hint := cxGetResourceString(@scxgwBehaviorPageGridLinesVerticalHint);
end;

procedure TcxGridWizardTableViewOptionsBehaviorPageFrame.ApplySettings;
var
  AView: TcxGridTableView;
begin
  Helper.Assign(PreviewGrid.ActiveView);

  AView := TcxGridTableView(Helper.GridView);
  AView.OptionsBehavior.FocusCellOnCycle := chbFocusCellOnCycle.Checked;
  AView.OptionsBehavior.FocusCellOnTab := chbFocusCellOnTab.Checked;
  AView.OptionsBehavior.GoToNextCellOnEnter := chbGoToNextCellOnEnter.Checked;
  AView.OptionsSelection.CellSelect := chbCellSelect.Checked;
  AView.OptionsSelection.CellMultiSelect := chbCellMultiSelect.Checked;
  AView.OptionsSelection.MultiSelect := chbRecordMultiSelect.Checked;
  AView.OptionsSelection.HideSelection := chbHideSelection.Checked;
  AView.OptionsView.GridLines := GetGridLines(chbHorizontal.Checked, chbVertical.Checked);
  UpdateControlsState;
end;

procedure TcxGridWizardTableViewOptionsBehaviorPageFrame.LoadSettings;
var
  AView: TcxGridTableView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;

  AView := TcxGridTableView(Helper.GridView);
  chbFocusCellOnCycle.Checked := AView.OptionsBehavior.FocusCellOnCycle;
  chbFocusCellOnTab.Checked := AView.OptionsBehavior.FocusCellOnTab;
  chbGoToNextCellOnEnter.Checked := AView.OptionsBehavior.GoToNextCellOnEnter;
  chbCellSelect.Checked := AView.OptionsSelection.CellSelect;
  chbCellMultiSelect.Checked := AView.OptionsSelection.CellMultiSelect;
  chbRecordMultiSelect.Checked := AView.OptionsSelection.MultiSelect;
  chbHideSelection.Checked := AView.OptionsSelection.HideSelection;
  chbHorizontal.Checked := (AView.OptionsView.GridLines = glHorizontal) or (AView.OptionsView.GridLines = glBoth);
  chbVertical.Checked := (AView.OptionsView.GridLines = glVertical) or (AView.OptionsView.GridLines = glBoth);
  UpdateControlsState;
end;

procedure TcxGridWizardTableViewOptionsBehaviorPageFrame.UpdateControlsState;
begin
  if chbCellMultiSelect.Checked then
    chbCellSelect.Checked := True;
  lciCellSelect.Enabled := not chbCellMultiSelect.Checked;
end;

function TcxGridWizardTableViewOptionsBehaviorPageFrame.GetGridLines(AHorizontalVisible, AVerticalVisible: Boolean): TcxGridLines;
begin
  if AHorizontalVisible and AVerticalVisible then
    Result := glBoth
  else
    if AHorizontalVisible and not AVerticalVisible then
      Result := glHorizontal
    else
      if AVerticalVisible and not AHorizontalVisible then
        Result := glVertical
      else
        Result := glNone;
end;

function TcxGridWizardTableViewOptionsBehaviorPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwBehaviorPageDescription);
end;

function TcxGridWizardTableViewOptionsBehaviorPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwBehaviorPageTitle);
end;

{ Events }

procedure TcxGridWizardTableViewOptionsBehaviorPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
end;

end.
