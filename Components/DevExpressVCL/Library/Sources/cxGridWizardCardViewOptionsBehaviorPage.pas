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

unit cxGridWizardCardViewOptionsBehaviorPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxClasses, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, dxLayoutContainer, cxCheckBox, dxLayoutControl, cxGridWizardCustomPage,
  cxGridWizardStrs, cxGridCardView, dxLayoutLookAndFeels, cxLabel;

type
  { TcxGridWizardCardViewOptionsBehaviorPageFrame }

  TcxGridWizardCardViewOptionsBehaviorPageFrame = class(TcxGridWizardCustomPageFrame)
    cbIncSearch: TcxCheckBox;
    chbCardExpanding: TcxCheckBox;
    chbCellSelect: TcxCheckBox;
    chbExpandRowOnDblClick: TcxCheckBox;
    chbFocusCellOnTab: TcxCheckBox;
    chbFocusFirstCellOnNewRecord: TcxCheckBox;
    chbGoToNextCellOnEnter: TcxCheckBox;
    chbHideSelection: TcxCheckBox;
    chbMultiSelect: TcxCheckBox;
    chbRowExpanding: TcxCheckBox;
    chbRowHiding: TcxCheckBox;
    chbRowMoving: TcxCheckBox;
    lbExpandingOptions: TcxLabel;
    lbFocusingOptions: TcxLabel;
    lbRowOptions: TcxLabel;
    lbSelectionOptions: TcxLabel;
    lciCardExpanding: TdxLayoutItem;
    lciCellSelect: TdxLayoutItem;
    lciExpandingOptions: TdxLayoutItem;
    lciExpandRowOnDblClick: TdxLayoutItem;
    lciFocusCellOnTab: TdxLayoutItem;
    lciFocusFirstCellOnNewRecord: TdxLayoutItem;
    lciFocusingOptions: TdxLayoutItem;
    lciGoToNextCellOnEnter: TdxLayoutItem;
    lciHideSelection: TdxLayoutItem;
    lciMultiSelect: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lciRowExpanding: TdxLayoutItem;
    lciRowHiding: TdxLayoutItem;
    lciRowMoving: TdxLayoutItem;
    lciRowOptions: TdxLayoutItem;
    lciSelectionOptions: TdxLayoutItem;
    lcMainItem1: TdxLayoutItem;
    lcSelectionPageGroup1: TdxLayoutGroup;
    lcSelectionPageGroup10: TdxLayoutGroup;
    lcSelectionPageGroup11: TdxLayoutGroup;
    lcSelectionPageGroup12: TdxLayoutGroup;
    lcSelectionPageGroup13: TdxLayoutGroup;
    lcSelectionPageGroup2: TdxLayoutGroup;
    lcSelectionPageGroup3: TdxLayoutGroup;
    lcSelectionPageGroup4: TdxLayoutGroup;
    lcSelectionPageGroup5: TdxLayoutGroup;
    lcSelectionPageGroup6: TdxLayoutGroup;
    lcSelectionPageGroup7: TdxLayoutGroup;
    lcSelectionPageGroup8: TdxLayoutGroup;
    lcSelectionPageGroup9: TdxLayoutGroup;
    lcSelectionPageSeparatorItem1: TdxLayoutSeparatorItem;
    lcSelectionPageSeparatorItem2: TdxLayoutSeparatorItem;
    lcSelectionPageSeparatorItem3: TdxLayoutSeparatorItem;
    lcSelectionPageSeparatorItem4: TdxLayoutSeparatorItem;
    pnPreviewGrid: TPanel;
    procedure RefreshPreviewGrid(Sender: TObject);
  protected
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

{$R *.dfm}

{ TcxGridWizardCardViewOptionsBehaviorPageFrame }

procedure TcxGridWizardCardViewOptionsBehaviorPageFrame.ApplyLocalization;
begin
  lbExpandingOptions.Caption := cxGetResourceString(@scxgwBehaviorPageExpandingOptions);
  lbFocusingOptions.Caption := cxGetResourceString(@scxgwBehaviorPageFocusingOptions);
  lbSelectionOptions.Caption := cxGetResourceString(@scxgwBehaviorPageSelectionOptions);
  lbRowOptions.Caption := cxGetResourceString(@scxgwBehaviorPageCardViewRowOptions);

  chbFocusFirstCellOnNewRecord.Caption := cxGetResourceString(@scxgwBehaviorPageFocusFirstCellOnNewRecord);
  chbFocusFirstCellOnNewRecord.Hint := cxGetResourceString(@scxgwBehaviorPageFocusFirstCellOnNewRecordHint);

  chbFocusCellOnTab.Caption := cxGetResourceString(@scxgwBehaviorPageFocusCellOnTab);
  chbFocusCellOnTab.Hint := cxGetResourceString(@scxgwBehaviorPageFocusCellOnTabHint);

  chbGoToNextCellOnEnter.Caption := cxGetResourceString(@scxgwBehaviorPageGoToNextCellOnEnter);
  chbGoToNextCellOnEnter.Hint := cxGetResourceString(@scxgwBehaviorPageGoToNextCellOnEnterHint);

  chbCellSelect.Caption := cxGetResourceString(@scxgwBehaviorPageCellSelect);
  chbCellSelect.Hint := cxGetResourceString(@scxgwBehaviorPageCellSelectHint);

  chbMultiSelect.Caption := cxGetResourceString(@scxgwBehaviorPageRecordMultiSelect);
  chbMultiSelect.Hint := cxGetResourceString(@scxgwBehaviorPageRecordMultiSelectHint);

  chbCardExpanding.Caption := cxGetResourceString(@scxgwBehaviorPageCardViewCardExpanding);
  chbCardExpanding.Hint := cxGetResourceString(@scxgwBehaviorPageCardViewCardExpandingHint);

  chbRowExpanding.Caption := cxGetResourceString(@scxgwBehaviorPageCardViewRowExpanding);
  chbRowExpanding.Hint := cxGetResourceString(@scxgwBehaviorPageCardViewRowExpandingHint);

  chbExpandRowOnDblClick.Caption := cxGetResourceString(@scxgwBehaviorPageCardViewExpandRowOnDblClick);
  chbExpandRowOnDblClick.Hint := cxGetResourceString(@scxgwBehaviorPageCardViewExpandRowOnDblClickHint);

  chbHideSelection.Caption := cxGetResourceString(@scxgwBehaviorPageHideSelection);
  chbHideSelection.Hint := cxGetResourceString(@scxgwBehaviorPageHideSelectionHint);

  chbRowHiding.Caption := cxGetResourceString(@scxgwBehaviorPageCardViewRowHiding);
  chbRowHiding.Hint := cxGetResourceString(@scxgwBehaviorPageCardViewRowHidingHint);

  chbRowMoving.Caption := cxGetResourceString(@scxgwBehaviorPageCardViewRowMoving);
  chbRowMoving.Hint := cxGetResourceString(@scxgwBehaviorPageCardViewRowMovingHint);

  cbIncSearch.Caption :=  cxGetResourceString(@scxgwFilteringSortingPageIncSearch);
  cbIncSearch.Hint :=  cxGetResourceString(@scxgwFilteringSortingPageIncSearchHint);
end;

procedure TcxGridWizardCardViewOptionsBehaviorPageFrame.ApplySettings;
var
  AView: TcxGridCardView;
begin
  Helper.Assign(PreviewGrid.ActiveView);

  AView := TcxGridCardView(Helper.GridView);
  AView.OptionsCustomize.CardExpanding := chbCardExpanding.Checked;
  AView.OptionsCustomize.RowExpanding := chbRowExpanding.Checked;
  AView.OptionsBehavior.ExpandRowOnDblClick := chbExpandRowOnDblClick.Checked;
  AView.OptionsBehavior.FocusCellOnTab := chbFocusCellOnTab.Checked;
  AView.OptionsBehavior.FocusFirstCellOnNewRecord := chbFocusFirstCellOnNewRecord.Checked;
  AView.OptionsBehavior.GoToNextCellOnEnter := chbGoToNextCellOnEnter.Checked;
  AView.OptionsSelection.CellSelect := chbCellSelect.Checked;
  AView.OptionsSelection.MultiSelect := chbMultiSelect.Checked;
  AView.OptionsSelection.HideSelection := chbHideSelection.Checked;
  AView.OptionsCustomize.RowHiding := chbRowHiding.Checked;
  AView.OptionsCustomize.RowMoving := chbRowMoving.Checked;
end;

procedure TcxGridWizardCardViewOptionsBehaviorPageFrame.LoadSettings;
var
  AView: TcxGridCardView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;

  AView := TcxGridCardView(Helper.GridView);
  chbCardExpanding.Checked := AView.OptionsCustomize.CardExpanding;
  chbRowExpanding.Checked := AView.OptionsCustomize.RowExpanding;
  chbExpandRowOnDblClick.Checked := AView.OptionsBehavior.ExpandRowOnDblClick;
  chbFocusCellOnTab.Checked := AView.OptionsBehavior.FocusCellOnTab;
  chbFocusFirstCellOnNewRecord.Checked := AView.OptionsBehavior.FocusFirstCellOnNewRecord;
  chbGoToNextCellOnEnter.Checked := AView.OptionsBehavior.GoToNextCellOnEnter;
  chbCellSelect.Checked := AView.OptionsSelection.CellSelect;
  chbMultiSelect.Checked := AView.OptionsSelection.MultiSelect;
  chbHideSelection.Checked := AView.OptionsSelection.HideSelection;
  chbRowHiding.Checked := AView.OptionsCustomize.RowHiding;
  chbRowMoving.Checked := AView.OptionsCustomize.RowMoving;
end;

function TcxGridWizardCardViewOptionsBehaviorPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwBehaviorPageDescription);
end;

function TcxGridWizardCardViewOptionsBehaviorPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwBehaviorPageTitle);
end;

{ Events }

procedure TcxGridWizardCardViewOptionsBehaviorPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
end;

end.
