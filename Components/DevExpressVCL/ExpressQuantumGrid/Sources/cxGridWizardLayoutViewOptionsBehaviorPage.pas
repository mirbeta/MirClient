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

unit cxGridWizardLayoutViewOptionsBehaviorPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxClasses, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, dxLayoutContainer, cxCheckBox, ExtCtrls, dxLayoutControl, cxGridWizardCustomPage,
  cxGridWizardStrs, cxGridLayoutView, dxLayoutLookAndFeels, cxLabel, cxGridCustomLayoutView;

type
  { TcxGridWizardLayoutViewOptionsBehaviorPageFrame }

  TcxGridWizardLayoutViewOptionsBehaviorPageFrame = class(TcxGridWizardCustomPageFrame)
    chbCellEndEllipsis: TcxCheckBox;
    chbCellSelect: TcxCheckBox;
    chbExpandRecordOnDblClick: TcxCheckBox;
    chbFocusCellOnTab: TcxCheckBox;
    chbFocusFirstCellOnNewRecord: TcxCheckBox;
    chbGoToNextCellOnEnter: TcxCheckBox;
    chbGroupExpanding: TcxCheckBox;
    chbHideSelection: TcxCheckBox;
    chbIncSearch: TcxCheckBox;
    chbItemHotTrack: TcxCheckBox;
    chbMultiSelect: TcxCheckBox;
    chbRecordExpanding: TcxCheckBox;
    lbExpandingOptions: TcxLabel;
    lbFocusingOptions: TcxLabel;
    lbOther: TcxLabel;
    lbSelectionOptions: TcxLabel;
    lciCellSelect: TdxLayoutItem;
    lciExpandingOptions: TdxLayoutItem;
    lciExpandRecordOnDblClick: TdxLayoutItem;
    lciFocusCellOnTab: TdxLayoutItem;
    lciFocusFirstCellOnNewRecord: TdxLayoutItem;
    lciFocusingOptions: TdxLayoutItem;
    lciGoToNextCellOnEnter: TdxLayoutItem;
    lciGroupExpanding: TdxLayoutItem;
    lciItemHotTrack: TdxLayoutItem;
    lciMultiSelect: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lciRecordExpanding: TdxLayoutItem;
    lciSelectionOptions: TdxLayoutItem;
    lcMainGroup2: TdxLayoutGroup;
    lcMainGroup3: TdxLayoutGroup;
    lcMainGroup4: TdxLayoutGroup;
    lcMainGroup5: TdxLayoutGroup;
    lcMainGroup7: TdxLayoutGroup;
    lcMainItem1: TdxLayoutItem;
    lcMainItem2: TdxLayoutItem;
    lcMainItem3: TdxLayoutItem;
    lcMainItem4: TdxLayoutItem;
    lcMainSeparatorItem1: TdxLayoutSeparatorItem;
    lcSelectionPageGroup10: TdxLayoutGroup;
    lcSelectionPageGroup2: TdxLayoutGroup;
    lcSelectionPageGroup3: TdxLayoutGroup;
    lcSelectionPageGroup4: TdxLayoutGroup;
    lcSelectionPageGroup6: TdxLayoutGroup;
    lcSelectionPageGroup7: TdxLayoutGroup;
    lcSelectionPageGroup8: TdxLayoutGroup;
    lcSelectionPageGroup9: TdxLayoutGroup;
    lcSelectionPageSeparatorItem1: TdxLayoutSeparatorItem;
    lcSelectionPageSeparatorItem2: TdxLayoutSeparatorItem;
    lcSelectionPageSeparatorItem3: TdxLayoutSeparatorItem;
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

{ TcxGridWizardLayoutViewOptionsBehaviorPageFrame }

procedure TcxGridWizardLayoutViewOptionsBehaviorPageFrame.ApplyLocalization;
begin
  lbExpandingOptions.Caption := cxGetResourceString(@scxgwBehaviorPageExpandingOptions);
  lbFocusingOptions.Caption := cxGetResourceString(@scxgwBehaviorPageFocusingOptions);
  lbOther.Caption := cxGetResourceString(@scxgwCommonGroupCaptionOthers);
  lbSelectionOptions.Caption := cxGetResourceString(@scxgwBehaviorPageSelectionOptions);

  chbFocusCellOnTab.Caption := cxGetResourceString(@scxgwBehaviorPageFocusCellOnTab);
  chbFocusCellOnTab.Hint := cxGetResourceString(@scxgwBehaviorPageFocusCellOnTabHint);

  chbGoToNextCellOnEnter.Caption := cxGetResourceString(@scxgwBehaviorPageGoToNextCellOnEnter);
  chbGoToNextCellOnEnter.Hint := cxGetResourceString(@scxgwBehaviorPageGoToNextCellOnEnterHint);

  chbCellSelect.Caption := cxGetResourceString(@scxgwBehaviorPageCellSelect);
  chbCellSelect.Hint := cxGetResourceString(@scxgwBehaviorPageCellSelectHint);

  chbRecordExpanding.Caption := cxGetResourceString(@scxgwBehaviorPageRecordExpanding);
  chbRecordExpanding.Hint := cxGetResourceString(@scxgwBehaviorPageRecordExpandingHint);

  chbGroupExpanding.Caption := cxGetResourceString(@scxgwBehaviorPageGroupExpanding);
  chbGroupExpanding.Hint := cxGetResourceString(@scxgwBehaviorPageGroupExpandingHint);

  chbExpandRecordOnDblClick.Caption := cxGetResourceString(@scxgwBehaviorPageExpandRecordOnDblClick);
  chbExpandRecordOnDblClick.Hint := cxGetResourceString(@scxgwBehaviorPageExpandRecordOnDblClickHint);

  chbFocusFirstCellOnNewRecord.Caption := cxGetResourceString(@scxgwBehaviorPageFocusFirstCellOnNewRecord);
  chbFocusFirstCellOnNewRecord.Hint := cxGetResourceString(@scxgwBehaviorPageFocusFirstCellOnNewRecordHint);

  chbItemHotTrack.Caption := cxGetResourceString(@scxgwBehaviorPageItemHotTrack);
  chbItemHotTrack.Hint := cxGetResourceString(@scxgwBehaviorPageItemHotTrackHint);

  chbMultiSelect.Caption := cxGetResourceString(@scxgwBehaviorPageRecordMultiSelect);
  chbMultiSelect.Hint := cxGetResourceString(@scxgwBehaviorPageRecordMultiSelectHint);

  chbIncSearch.Caption :=  cxGetResourceString(@scxgwFilteringSortingPageIncSearch);
  chbIncSearch.Hint :=  cxGetResourceString(@scxgwFilteringSortingPageIncSearchHint);

  chbCellEndEllipsis.Caption := cxGetResourceString(@scxgwSizingPageCellEndEllipsis);
  chbCellEndEllipsis.Hint := cxGetResourceString(@scxgwSizingPageCellEndEllipsis);

  chbHideSelection.Caption := cxGetResourceString(@scxgwBehaviorPageHideSelection);
  chbHideSelection.Hint := cxGetResourceString(@scxgwBehaviorPageHideSelectionHint);
end;

procedure TcxGridWizardLayoutViewOptionsBehaviorPageFrame.ApplySettings;
var
  AView: TcxGridLayoutView;
begin
  Helper.Assign(PreviewGrid.ActiveView);

  AView := TcxGridLayoutView(Helper.GridView);
  AView.OptionsCustomize.RecordExpanding := chbRecordExpanding.Checked;
  AView.OptionsCustomize.GroupExpanding := chbGroupExpanding.Checked;
  AView.OptionsBehavior.ExpandRecordOnDblClick := chbExpandRecordOnDblClick.Checked;
  AView.OptionsBehavior.FocusCellOnTab := chbFocusCellOnTab.Checked;
  AView.OptionsBehavior.FocusFirstCellOnNewRecord := chbFocusFirstCellOnNewRecord.Checked;
  AView.OptionsBehavior.GoToNextCellOnEnter := chbGoToNextCellOnEnter.Checked;
  AView.OptionsSelection.CellSelect := chbCellSelect.Checked;
  AView.OptionsSelection.MultiSelect := chbMultiSelect.Checked;
  AView.OptionsBehavior.ItemHotTrack := chbItemHotTrack.Checked;
  AView.OptionsView.CellEndEllipsis := chbCellEndEllipsis.Checked;
  AView.OptionsBehavior.IncSearch := chbIncSearch.Checked;
  AView.OptionsSelection.HideSelection := chbHideSelection.Checked;
end;

procedure TcxGridWizardLayoutViewOptionsBehaviorPageFrame.LoadSettings;
var
  AView: TcxGridLayoutView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;

  AView := TcxGridLayoutView(Helper.GridView);
  chbRecordExpanding.Checked := AView.OptionsCustomize.RecordExpanding;
  chbGroupExpanding.Checked := AView.OptionsCustomize.GroupExpanding;
  chbExpandRecordOnDblClick.Checked := AView.OptionsBehavior.ExpandRecordOnDblClick;
  chbFocusCellOnTab.Checked := AView.OptionsBehavior.FocusCellOnTab;
  chbFocusFirstCellOnNewRecord.Checked := AView.OptionsBehavior.FocusFirstCellOnNewRecord;
  chbGoToNextCellOnEnter.Checked := AView.OptionsBehavior.GoToNextCellOnEnter;
  chbCellSelect.Checked := AView.OptionsSelection.CellSelect;
  chbMultiSelect.Checked := AView.OptionsSelection.MultiSelect;
  chbItemHotTrack.Checked := AView.OptionsBehavior.ItemHotTrack;
  chbCellEndEllipsis.Checked := AView.OptionsView.CellEndEllipsis;
  chbIncSearch.Checked := AView.OptionsBehavior.IncSearch;
  chbHideSelection.Checked := AView.OptionsSelection.HideSelection;

  lciExpandRecordOnDblClick.Visible := AView.OptionsView.RecordCaption.Visible;
  lciRecordExpanding.Visible := AView.OptionsView.RecordCaption.Visible;
end;

function TcxGridWizardLayoutViewOptionsBehaviorPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwBehaviorPageDescription);
end;

function TcxGridWizardLayoutViewOptionsBehaviorPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwBehaviorPageTitle);
end;

{ Events }

procedure TcxGridWizardLayoutViewOptionsBehaviorPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
end;

end.
