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

unit cxGridStrs;

{$I cxVer.inc}

interface

resourcestring
  scxGridRecursiveLevels = 'You cannot create recursive levels';

  scxGridDeletingFocusedConfirmationText = 'Delete record?';
  scxGridDeletingSelectedConfirmationText = 'Delete all selected records?';

  scxGridNoDataInfoText = '<No data to display>';

  scxGridFilterRowInfoText = 'Click here to define a filter';
  scxGridNewItemRowInfoText = 'Click here to add a new row';

  scxGridFindPanelClearButtonCaption = 'Clear';
  scxGridFindPanelFindButtonCaption = 'Find';
  scxGridFindPanelInfoText = 'Enter text to search...';

  scxGridFilterIsEmpty = '<Filter is Empty>';

  scxGridCustomizationFormCaption = 'Customization';
  scxGridCustomizationFormColumnsPageCaption = 'Columns';
  scxGridGroupByBoxCaption = 'Drag a column header here to group by that column';
  scxGridFilterApplyButtonCaption = 'Apply Filter';
  scxGridFilterCustomizeButtonCaption = 'Customize...';
  scxGridColumnsQuickCustomizationHint = 'Click here to show/hide/move columns';

  scxGridCustomizationFormBandsPageCaption = 'Bands';
  scxGridBandsQuickCustomizationHint = 'Click here to show/hide/move bands';

  scxGridCustomizationFormRowsPageCaption = 'Rows';

  scxGridConverterIntermediaryMissing = 'Missing an intermediary component!'#13#10'Please add a %s component to the form.';
  scxGridConverterNotExistGrid = 'cxGrid does not exist';
  scxGridConverterNotExistComponent = 'Component does not exist';
  scxImportErrorCaption = 'Import error';

  scxNotExistGridView = 'Grid view does not exist';

  cxSEditRepositoryExtLookupComboBoxItem = 'ExtLookupComboBox|Represents an ultra-advanced lookup using the QuantumGrid as its drop down control';

  // date ranges

  scxGridYesterday = 'Yesterday';
  scxGridToday = 'Today';
  scxGridTomorrow = 'Tomorrow';
  scxGridLast30Days = 'Last 30 days';
  scxGridLast14Days = 'Last 14 days';
  scxGridLast7Days = 'Last 7 days';
  scxGridNext7Days = 'Next 7 days';
  scxGridNext14Days = 'Next 14 days';
  scxGridNext30Days = 'Next 30 days';
  scxGridLastTwoWeeks = 'Last two weeks';
  scxGridLastWeek = 'Last week';
  scxGridThisWeek = 'This week';
  scxGridNextWeek = 'Next week';
  scxGridNextTwoWeeks = 'Next two weeks';
  scxGridLastMonth = 'Last month';
  scxGridThisMonth = 'This month';
  scxGridNextMonth = 'Next month';
  scxGridLastYear = 'Last year';
  scxGridThisYear = 'This year';
  scxGridNextYear = 'Next year';
  scxGridPast = 'Past';
  scxGridFuture = 'Future';

  scxGridMonthFormat = 'mmmm yyyy';
  scxGridYearFormat = 'yyyy';

  // ChartView

  scxGridChartCategoriesDisplayText = 'Data';

  scxGridChartValueHintFormat = '%s for %s is %s';  // series display text, category, value
  scxGridChartPercentValueTickMarkLabelFormat = '0%';

  scxGridChartToolBoxDataLevels = 'Data Levels:';
  scxGridChartToolBoxDataLevelSelectValue = 'select value';
  scxGridChartToolBoxCustomizeButtonCaption = 'Customize Chart';

  scxGridChartNoneDiagramDisplayText = 'No diagram';
  scxGridChartColumnDiagramDisplayText = 'Column diagram';
  scxGridChartBarDiagramDisplayText = 'Bar diagram';
  scxGridChartLineDiagramDisplayText = 'Line diagram';
  scxGridChartAreaDiagramDisplayText = 'Area diagram';
  scxGridChartPieDiagramDisplayText = 'Pie diagram';
  scxGridChartStackedBarDiagramDisplayText = 'Stacked Bars diagram';
  scxGridChartStackedColumnDiagramDisplayText = 'Stacked Columns diagram';
  scxGridChartStackedAreaDiagramDisplayText = 'Stacked Area diagram';

  scxGridChartCustomizationFormSeriesPageCaption = 'Series';
  scxGridChartCustomizationFormSortBySeries = 'Sort by:';
  scxGridChartCustomizationFormNoSortedSeries = '<none series>';
  scxGridChartCustomizationFormDataGroupsPageCaption = 'Data Groups';
  scxGridChartCustomizationFormOptionsPageCaption = 'Options';

  scxGridChartLegend = 'Legend';
  scxGridChartLegendKeyBorder = 'Key Border';
  scxGridChartPosition = 'Position';
  scxGridChartPositionDefault = 'Default';
  scxGridChartPositionNone = 'None';
  scxGridChartPositionLeft = 'Left';
  scxGridChartPositionTop = 'Top';
  scxGridChartPositionRight = 'Right';
  scxGridChartPositionBottom = 'Bottom';
  scxGridChartAlignment = 'Alignment';
  scxGridChartAlignmentDefault = 'Default';
  scxGridChartAlignmentStart = 'Start';
  scxGridChartAlignmentCenter = 'Center';
  scxGridChartAlignmentEnd = 'End';
  scxGridChartOrientation = 'Orientation';
  scxGridChartOrientationDefault = 'Default';
  scxGridChartOrientationHorizontal = 'Horizontal';
  scxGridChartOrientationVertical = 'Vertical';
  scxGridChartBorder = 'Border';
  scxGridChartTitle = 'Title';
  scxGridChartToolBox = 'ToolBox';
  scxGridChartDiagramSelector = 'Diagram Selector';
  scxGridChartOther = 'Other';
  scxGridChartValueHints = 'Value Hints';

  scxGridLayoutViewCustomizeFormOk = 'OK';
  scxGridLayoutViewCustomizeFormCancel = 'Cancel';
  scxGridLayoutViewCustomizeFormApply = 'Apply';
  scxGridLayoutViewCustomizeWarningDialogCaption = 'Warning';
  scxGridLayoutViewCustomizeWarningDialogMessage = 'The layout has been changed. Do you want to save changes?';
  scxGridLayoutViewCustomizeLayoutButtonCaption = 'Layout Editor';
  scxGridLayoutViewCustomizeFormTemplateCard = 'Template Card';
  scxGridLayoutViewCustomizeFormViewLayout = 'View Layout';
  scxGridLayoutViewRecordCaptionDefaultMask = '[RecordIndex] of [RecordCount]';

  scxGridLockedStateImageText = 'Please wait...';

  scxGridInplaceEditFormButtonCancel = 'Cancel';
  scxGridInplaceEditFormButtonClose = 'Close';
  scxGridInplaceEditFormButtonUpdate = 'Update';
  scxGridInplaceEditFormSaveChangesQuery = 'Your data is modified. Do you want to save the changes?';

  scxGridDataRowFixingPopupCommandUnfix = 'Unfix';
  scxGridDataRowFixingPopupCommandFixToTop = 'Fix to Top';
  scxGridDataRowFixingPopupCommandFixToBottom = 'Fix to Bottom';

implementation

uses
  dxCore;

procedure AddcxGridResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAddress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAddress);
  end;

begin
  InternalAdd('scxGridRecursiveLevels', @scxGridRecursiveLevels);
  InternalAdd('scxGridDeletingFocusedConfirmationText', @scxGridDeletingFocusedConfirmationText);
  InternalAdd('scxGridDeletingSelectedConfirmationText', @scxGridDeletingSelectedConfirmationText);
  InternalAdd('scxGridNoDataInfoText', @scxGridNoDataInfoText);
  InternalAdd('scxGridFilterRowInfoText', @scxGridFilterRowInfoText);
  InternalAdd('scxGridNewItemRowInfoText', @scxGridNewItemRowInfoText);
  InternalAdd('scxGridFindPanelClearButtonCaption', @scxGridFindPanelClearButtonCaption);
  InternalAdd('scxGridFindPanelFindButtonCaption', @scxGridFindPanelFindButtonCaption);
  InternalAdd('scxGridFindPanelInfoText', @scxGridFindPanelInfoText);
  InternalAdd('scxGridFilterIsEmpty', @scxGridFilterIsEmpty);
  InternalAdd('scxGridCustomizationFormCaption', @scxGridCustomizationFormCaption);
  InternalAdd('scxGridCustomizationFormColumnsPageCaption', @scxGridCustomizationFormColumnsPageCaption);
  InternalAdd('scxGridGroupByBoxCaption', @scxGridGroupByBoxCaption);
  InternalAdd('scxGridFilterApplyButtonCaption', @scxGridFilterApplyButtonCaption);
  InternalAdd('scxGridFilterCustomizeButtonCaption', @scxGridFilterCustomizeButtonCaption);
  InternalAdd('scxGridColumnsQuickCustomizationHint', @scxGridColumnsQuickCustomizationHint);
  InternalAdd('scxGridCustomizationFormBandsPageCaption', @scxGridCustomizationFormBandsPageCaption);
  InternalAdd('scxGridBandsQuickCustomizationHint', @scxGridBandsQuickCustomizationHint);
  InternalAdd('scxGridCustomizationFormRowsPageCaption', @scxGridCustomizationFormRowsPageCaption);
  InternalAdd('scxGridConverterIntermediaryMissing', @scxGridConverterIntermediaryMissing);
  InternalAdd('scxGridConverterNotExistGrid', @scxGridConverterNotExistGrid);
  InternalAdd('scxGridConverterNotExistComponent', @scxGridConverterNotExistComponent);
  InternalAdd('scxImportErrorCaption', @scxImportErrorCaption);
  InternalAdd('scxNotExistGridView', @scxNotExistGridView);
  InternalAdd('cxSEditRepositoryExtLookupComboBoxItem', @cxSEditRepositoryExtLookupComboBoxItem);
  InternalAdd('scxGridYesterday', @scxGridYesterday);
  InternalAdd('scxGridToday', @scxGridToday);
  InternalAdd('scxGridTomorrow', @scxGridTomorrow);
  InternalAdd('scxGridLast30Days', @scxGridLast30Days);
  InternalAdd('scxGridLast14Days', @scxGridLast14Days);
  InternalAdd('scxGridLast7Days', @scxGridLast7Days);
  InternalAdd('scxGridNext7Days', @scxGridNext7Days);
  InternalAdd('scxGridNext14Days', @scxGridNext14Days);
  InternalAdd('scxGridNext30Days', @scxGridNext30Days);
  InternalAdd('scxGridLastTwoWeeks', @scxGridLastTwoWeeks);
  InternalAdd('scxGridLastWeek', @scxGridLastWeek);
  InternalAdd('scxGridThisWeek', @scxGridThisWeek);
  InternalAdd('scxGridNextWeek', @scxGridNextWeek);
  InternalAdd('scxGridNextTwoWeeks', @scxGridNextTwoWeeks);
  InternalAdd('scxGridLastMonth', @scxGridLastMonth);
  InternalAdd('scxGridThisMonth', @scxGridThisMonth);
  InternalAdd('scxGridNextMonth', @scxGridNextMonth);
  InternalAdd('scxGridLastYear', @scxGridLastYear);
  InternalAdd('scxGridThisYear', @scxGridThisYear);
  InternalAdd('scxGridNextYear', @scxGridNextYear);
  InternalAdd('scxGridPast', @scxGridPast);
  InternalAdd('scxGridFuture', @scxGridFuture);
  InternalAdd('scxGridMonthFormat', @scxGridMonthFormat);
  InternalAdd('scxGridYearFormat', @scxGridYearFormat);
  InternalAdd('scxGridChartCategoriesDisplayText', @scxGridChartCategoriesDisplayText);
  InternalAdd('scxGridChartValueHintFormat', @scxGridChartValueHintFormat);
  InternalAdd('scxGridChartPercentValueTickMarkLabelFormat', @scxGridChartPercentValueTickMarkLabelFormat);
  InternalAdd('scxGridChartToolBoxDataLevels', @scxGridChartToolBoxDataLevels);
  InternalAdd('scxGridChartToolBoxDataLevelSelectValue', @scxGridChartToolBoxDataLevelSelectValue);
  InternalAdd('scxGridChartToolBoxCustomizeButtonCaption', @scxGridChartToolBoxCustomizeButtonCaption);
  InternalAdd('scxGridChartNoneDiagramDisplayText', @scxGridChartNoneDiagramDisplayText);
  InternalAdd('scxGridChartColumnDiagramDisplayText', @scxGridChartColumnDiagramDisplayText);
  InternalAdd('scxGridChartBarDiagramDisplayText', @scxGridChartBarDiagramDisplayText);
  InternalAdd('scxGridChartLineDiagramDisplayText', @scxGridChartLineDiagramDisplayText);
  InternalAdd('scxGridChartAreaDiagramDisplayText', @scxGridChartAreaDiagramDisplayText);
  InternalAdd('scxGridChartPieDiagramDisplayText', @scxGridChartPieDiagramDisplayText);
  InternalAdd('scxGridChartStackedBarDiagramDisplayText', @scxGridChartStackedBarDiagramDisplayText);
  InternalAdd('scxGridChartStackedColumnDiagramDisplayText', @scxGridChartStackedColumnDiagramDisplayText);
  InternalAdd('scxGridChartStackedAreaDiagramDisplayText', @scxGridChartStackedAreaDiagramDisplayText);
  InternalAdd('scxGridChartCustomizationFormSeriesPageCaption', @scxGridChartCustomizationFormSeriesPageCaption);
  InternalAdd('scxGridChartCustomizationFormSortBySeries', @scxGridChartCustomizationFormSortBySeries);
  InternalAdd('scxGridChartCustomizationFormNoSortedSeries', @scxGridChartCustomizationFormNoSortedSeries);
  InternalAdd('scxGridChartCustomizationFormDataGroupsPageCaption', @scxGridChartCustomizationFormDataGroupsPageCaption);
  InternalAdd('scxGridChartCustomizationFormOptionsPageCaption', @scxGridChartCustomizationFormOptionsPageCaption);
  InternalAdd('scxGridChartLegend', @scxGridChartLegend);
  InternalAdd('scxGridChartLegendKeyBorder', @scxGridChartLegendKeyBorder);
  InternalAdd('scxGridChartPosition', @scxGridChartPosition);
  InternalAdd('scxGridChartPositionDefault', @scxGridChartPositionDefault);
  InternalAdd('scxGridChartPositionNone', @scxGridChartPositionNone);
  InternalAdd('scxGridChartPositionLeft', @scxGridChartPositionLeft);
  InternalAdd('scxGridChartPositionTop', @scxGridChartPositionTop);
  InternalAdd('scxGridChartPositionRight', @scxGridChartPositionRight);
  InternalAdd('scxGridChartPositionBottom', @scxGridChartPositionBottom);
  InternalAdd('scxGridChartAlignment', @scxGridChartAlignment);
  InternalAdd('scxGridChartAlignmentDefault', @scxGridChartAlignmentDefault);
  InternalAdd('scxGridChartAlignmentStart', @scxGridChartAlignmentStart);
  InternalAdd('scxGridChartAlignmentCenter', @scxGridChartAlignmentCenter);
  InternalAdd('scxGridChartAlignmentEnd', @scxGridChartAlignmentEnd);
  InternalAdd('scxGridChartOrientation', @scxGridChartOrientation);
  InternalAdd('scxGridChartOrientationDefault', @scxGridChartOrientationDefault);
  InternalAdd('scxGridChartOrientationHorizontal', @scxGridChartOrientationHorizontal);
  InternalAdd('scxGridChartOrientationVertical', @scxGridChartOrientationVertical);
  InternalAdd('scxGridChartBorder', @scxGridChartBorder);
  InternalAdd('scxGridChartTitle', @scxGridChartTitle);
  InternalAdd('scxGridChartToolBox', @scxGridChartToolBox);
  InternalAdd('scxGridChartDiagramSelector', @scxGridChartDiagramSelector);
  InternalAdd('scxGridChartOther', @scxGridChartOther);
  InternalAdd('scxGridChartValueHints', @scxGridChartValueHints);

  InternalAdd('scxGridLayoutViewCustomizeFormOk', @scxGridLayoutViewCustomizeFormOk);
  InternalAdd('scxGridLayoutViewCustomizeFormCancel', @scxGridLayoutViewCustomizeFormCancel);
  InternalAdd('scxGridLayoutViewCustomizeFormApply', @scxGridLayoutViewCustomizeFormApply);
  InternalAdd('scxGridLayoutViewCustomizeWarningDialogCaption', @scxGridLayoutViewCustomizeWarningDialogCaption);
  InternalAdd('scxGridLayoutViewCustomizeWarningDialogMessage', @scxGridLayoutViewCustomizeWarningDialogMessage);
  InternalAdd('scxGridLayoutViewCustomizeLayoutButtonCaption', @scxGridLayoutViewCustomizeLayoutButtonCaption);
  InternalAdd('scxGridLayoutViewCustomizeFormTemplateCard', @scxGridLayoutViewCustomizeFormTemplateCard);
  InternalAdd('scxGridLayoutViewCustomizeFormViewLayout', @scxGridLayoutViewCustomizeFormViewLayout);
  InternalAdd('scxGridLayoutViewRecordCaptionDefaultMask', @scxGridLayoutViewRecordCaptionDefaultMask);

  InternalAdd('scxGridLockedStateImageText', @scxGridLockedStateImageText);

  InternalAdd('scxGridInplaceEditFormButtonClose', @scxGridInplaceEditFormButtonClose);
  InternalAdd('scxGridInplaceEditFormButtonCancel', @scxGridInplaceEditFormButtonCancel);
  InternalAdd('scxGridInplaceEditFormButtonUpdate', @scxGridInplaceEditFormButtonUpdate);
  InternalAdd('scxGridInplaceEditFormSaveChangesQuery', @scxGridInplaceEditFormSaveChangesQuery);

  InternalAdd('scxGridDataRowFixingPopupCommandUnfix', @scxGridDataRowFixingPopupCommandUnfix);
  InternalAdd('scxGridDataRowFixingPopupCommandFixToTop', @scxGridDataRowFixingPopupCommandFixToTop);
  InternalAdd('scxGridDataRowFixingPopupCommandFixToBottom', @scxGridDataRowFixingPopupCommandFixToBottom);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressQuantumGrid', @AddcxGridResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressQuantumGrid');

end.
