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

unit cxGridWizardStrs;

{$I cxVer.inc}

interface

uses
  dxCore, cxNavigator;

const
  scxGridWizardDefaultBooleanCaptions: array [0..2] of string = ('Disabled', 'Enabled', 'Default');
  scxGridWizardNavigatorButtonsNames: array [0 .. NavigatorButtonCount - 1] of string = ('First', 'PriorPage', 'Prior',
    'Next', 'NextPage', 'Last', 'Insert', 'Append', 'Delete', 'Edit', 'Post', 'Cancel', 'Refresh', 'SaveBookmark',
    'GotoBookmark', 'Filter');

resourcestring
  // Common
  scxgwCommonAdd = '&Add';
  scxgwCommonAddAll = 'Add A&ll';
  scxgwCommonAddSelected = 'Add &Selected';
  scxgwCommonCancel = '&Cancel';
  scxgwCommonCaptionPrompt = 'Caption:';
  scxgwCommonCheckSelected = 'Check Selected';
  scxgwCommonCloseQuery = 'You are about to terminate the DevExpress Grid Configuration Wizard. ' +
    'If you close the wizard, you will lose all grid configurations. Do you wish to continue?';
  scxgwCommonCreateDetailLevelQuery = 'Do you want to create a new detail view?';
  scxgwCommonDataSource = 'Data Source:';
  scxgwCommonDataSourceHint = ''; //todo:
  scxgwCommonDeleteAll = 'D&elete All';
  scxgwCommonDeleteSelected = '&Delete';
  scxgwCommonDeselectAll = 'Clear Selection';
  scxgwCommonEdit = 'Ed&it';
  scxgwCommonGroupCaptionCommon = 'Common';
  scxgwCommonGroupCaptionOthers = 'Other';
  scxgwCommonKeyFields = 'Key Fields:';
  scxgwCommonKeyFieldsHint =
    'Key Fields specify a semicolon-separated list of key' + #13#10 +
    'field names that uniquely identify each record';
  scxgwCommonKind = 'Kind:';
  scxgwCommonMoveDown = 'Move Do&wn';
  scxgwCommonMoveSelectedDown = 'Move Selected Down';
  scxgwCommonMoveSelectedUp = 'Move Selected Up';
  scxgwCommonMoveUp = 'Move &Up';
  scxgwCommonNoneSelected = '<None Selected>';
  scxgwCommonOK = '&OK';
  scxgwCommonProperties = 'Properties:';
  scxgwCommonSelectAll = 'Select All';
  scxgwCommonUncheckSelected = 'Uncheck Selected';
  scxgwCommonWizardCaption = 'Grid Wizard';
  scxgwCommonWizardCaptionEditing = 'Grid Wizard - %s';

  // Wizard Mode Page
  scxgwWizardModePageDescription = 'Choose the most appropriate data presentation for your task';
  scxgwWizardModePageTitle = 'Data presentation';
  scxgwWizardModePageDeletePresentStructure = 'Delete the present level structure';
  scxgwWizardModePageMultiLevelStructure = 'Master-detail';
  scxgwWizardModePageSingleLevelStructure = 'Simple table';

  // Select View Page
  scxgwSelectViewPageDescription = 'Choose the most appropriate Grid View for your data source';
  scxgwSelectViewPageTitle = 'Grid View';
  scxgwSelectViewPageGroupHeaderDBViews = 'DB Views';
  scxgwSelectViewPageGroupHeaderServerModeViews = 'Server Mode Views';
  scxgwSelectViewPageGroupHeaderUnboundViews = 'Unbound Views';

  // Finish Page
  scxgwFinishPageDescription = 'This is your customized Grid View. If you need to make some corrections, ' +
    'use the Back button, otherwise click Finish to save the changes';
  scxgwFinishPageTitle = 'Finish';

  // Customize Items Page
  scxgwCustomizeItemsPageDescriptionDB = 'Provide the most convenient layout for your interface elements';
  scxgwCustomizeItemsPageDescriptionUnbound =
    'Provide the most convenient layout for your interface elements and populate the Grid View with data';
  scxgwCustomizeItemsPageTitleDB = 'Customize layout';
  scxgwCustomizeItemsPageTitleUnbound = 'Customize layout and data';

  // Card View Tree View Frame
  scxgwCardViewTreeViewFrameAddCaptionRow = 'Add Ca&ption Row';
  scxgwCardViewTreeViewFrameAddCaptionRowHint = 'Add Caption Row (Alt+P)';
  scxgwCardViewTreeViewFrameAddCategoryRow = 'Add Ca&tegory Row';
  scxgwCardViewTreeViewFrameAddCategoryRowHint = 'Add Category Row (Alt+T)';
  scxgwCardViewTreeViewFrameEditRow = '&Edit Row';
  scxgwCardViewTreeViewFrameEditRowHint = 'Edit Row (Alt+E)';
  scxgwCardViewTreeViewFrameDefaultCaptionRowCaption = 'Caption';
  scxgwCardViewTreeViewFrameDefaultCategoryRowCaption = 'Category';
  scxgwCardViewTreeViewFrameInputQueryCaptionAddCaptionRow = 'Add Caption Row';
  scxgwCardViewTreeViewFrameInputQueryCaptionAddCategoryRow = 'Add Category Row';
  scxgwCardViewTreeViewFrameInputQueryCaptionEditRow = 'Edit Row';

  // Customize Bands Page
  scxgwBandsPageDescription = 'Create bands, name them, and specify their order';
  scxgwBandsPageTitle = 'Bands';
  scxgwBandsPageInputQueryCaptionAdd = 'Add Band';
  scxgwBandsPageInputQueryCaptionEdit = 'Edit Band';

  // Unbound Select Items For Display Page
  scxgwUnboundsSelectColumnsForDisplayPageDescription = 'Create columns, name them, and specify their order';
  scxgwUnboundsSelectColumnsForDisplayPageTitle = 'Columns';
  scxgsUnboundsSelectColumnsForDisplayPageInputQueryCaptionAdd = 'Add Column';
  scxgsUnboundsSelectColumnsForDisplayPageInputQueryCaptionEdit = 'Edit Column';
  scxgwUnboundsSelectItemsForDisplayPageDescription = 'Create items, name them, and specify their order';
  scxgwUnboundsSelectItemsForDisplayPageTitle = 'Items';
  scxgsUnboundsSelectItemsForDisplayPageInputQueryCaptionAdd = 'Add Item';
  scxgsUnboundsSelectItemsForDisplayPageInputQueryCaptionEdit = 'Edit Item';
  scxgwUnboundsSelectRowsForDisplayPageDescription = 'Create rows, name them, and specify their order';
  scxgwUnboundsSelectRowsForDisplayPageTitle = 'Rows';
  scxgsUnboundsSelectRowsForDisplayPageInputQueryCaptionAdd = 'Add Row';
  scxgsUnboundsSelectRowsForDisplayPageInputQueryCaptionEdit = 'Edit Row';

  // DBViews Data Source Page
  scxgwDataSourcePageDescription = 'Specify a data source and configure its settings. ' +
    'Make sure that your data source is linked to a dataset';
  scxgwDataSourcePageTitle = 'Data Source';
  scxgwDataSourcePageIsDetailView = 'Is detail View';
  scxgwDataSourcePageMasterView = 'Master View';
  scxgwDataSourcePageMasterViewHint = '';
  scxgwDataSourcePageMasterViewKeyFieldNames = 'Master Key Fields: ';
  scxgwDataSourcePageMasterViewKeyFieldNamesHint =
    'Master Key Fields specify a list of fields in a master dataset' + #13#10 +
    'that establish a master-detail relationship between two datasets.';
  scxgwDataSourcePageDetailKeyFieldNames = 'Detail Key Fields: ';
  scxgwDataSourcePageDetailKeyFieldNamesHint =
    'Detail Key Fields specify one or more field names' + #13#10 +
    'in the details dataset that uniquely identify each detail record';

  // Server Mode Data Source Page
  scxgwServerModeDataSourcePageDescription =
    'Specify a data source. Make sure that it is properly configured and linked to a valid connection';
  scxgwServerModeDataSourcePageTitle = 'Data Source';
  scxgwServerModeDataSourcePageActivate = 'Activate';

  // Select Items For Display Page
  scxgwSelectItemsForDisplayPageDescription = 'Select fields to display in the Grid View and specify their order';
  scxgwSelectItemsForDisplayPageTitle = 'Fields for Display';
  scxgwSelectItemsForDisplayDataSetFields = 'Remaining fields in the DataSet';
  scxgwSelectItemsForDisplayGridViewFields = 'Fields in the Grid View';

  // Data Loading Settings Page
  scxgwDataSettingsPageDescription = 'Choose the most appropriate settings for your data processing';
  scxgwDataSettingsPageTitle = 'Data Processing';
  scxgwDataSettingsPageGridMode = 'Grid Mode';
  scxgwDataSettingsPageGridModeBufferCount = 'Grid mode buffer count';
  scxgwDataSettingsPageSmartRefresh = 'Smart refresh';
  scxgwDataSettingsPageSmartRefreshHint = '';
  scxgwDataSettingsPageSyncMode = 'Sync mode';
  scxgwDataSettingsPageSyncModeHint =
    'Sync mode specifies whether data synchronization between' + #13#10 +
    'the grid control and the corresponding TDataSet is enabled';
  scxgwDataSettingsPageSynchronization = 'Detail Views synchronization';
  scxgwDataSettingsPageSynchronizationHint =
    'The Detail Views synchronization determines whether all' + #13#10 +
    'the "clone" Views in the current detail View should be' + #13#10 +
    'synchronized automatically with the "pattern" View';
  scxgwDataSettingsPageMultiThreadedOptionsFiltering = 'Multi-threaded Filtering';
  scxgwDataSettingsPageMultiThreadedOptionsSorting = 'Multi-threaded Sorting';

  // Table View Interface Elements Page
  scxgwUIElementsPageDescription = 'Specify interface elements that will be accessible for end-users';
  scxgwUIElementsPageTitle = 'Interface Elements';
  scxgwUIElementsPageGroupInterfaceElements = 'Interface Elements';
  scxgwUIElementsPageFooter = 'Footer';
  scxgwUIElementsPageFooterHint =
    'The Footer panel is the area used for displaying footer summaries';
  scxgwUIElementsPageHeader = 'Header';
  scxgwUIElementsPageHeaderHint =
    'This option determines whether column headers are displayed by the Grid View' + #13#10 +
    'and enable runtime column operations such as sorting, grouping and moving';
  scxgwUIElementsPageGroupBox = 'Group By Box';
  scxgwUIElementsPageGroupBoxHint =
    'The Group By Box enables runtime grouping and determines whether the grouping panel is visible';
  scxgwUIElementsPageIndicator = 'Indicator';
  scxgwUIElementsPageIndicatorHint =
    'The Indicator determines whether the row indicator is displayed';
  scxgwUIElementsPageBandsQuickCustomization = 'Quick Customization of Bands';
  scxgwUIElementsPageBandsQuickCustomizationHint =
    'The Quick Bands Customization button invokes a dropdown' + #13#10 +
    'that provides the capability to switch band visibility' + #13#10 +
    'and reorder bands by dragging items in the dropdown';
  scxgwUIElementsPageColumnsQuickCustomization = 'Quick Customization of Columns';
  scxgwUIElementsPageColumnsQuickCustomizationHint =
    'The Columns Bands Customization button invokes a dropdown' + #13#10 +
    'that provides the capability to switch column visibility' + #13#10 +
    'and reorder columns by dragging items in the dropdown';
  scxgwUIElementsPageGroupFooter = 'Group Footers';
  scxgwUIElementsPageGroupFooterHint =
    'The group footer is a panel at the bottom of the group for displaying group summaries';
  scxgwUIElementsPageGroupFooterModeAlwaysVisible = 'Always Visible';
  scxgwUIElementsPageGroupFooterModeVisibleWhenExpanded = 'Visible When Expanded';
  scxgwUIElementsPagePreview = 'Preview';
  scxgwUIElementsPagePreviewHint =
    'MS-Outlook style preview sections allow you to display a memo field’s content' + #13#10 +
    'or custom memo information for records in an elegant manner. Such sections are' + #13#10 +
    'displayed above or under each data row and their width matches the total column width';
  scxgwUIElementsPageNavigator = 'Navigator';
  scxgwUIElementsPageNavigatorHint = 'The navigator provides record navigation and management';
  scxgwUIElementsPageNavigatorButtons = 'Buttons'; //todo:
  scxgwUIElementsPageNavigatorButtonsHint = ''; //todo:
  scxgwUIElementsPageNewItemRow = 'New Item Row';
  scxgwUIElementsPageNewItemRowHint =
    'A new item row is labeled with the ‘Click here to add a new row’ description' + #13#10 +
    'and provides the capability to enter a new record into the dataset';
  scxgwUIElementsPageRecordCaption = 'Record Caption';
  scxgwUIElementsPageRecordCaptionHint = 'Specifies the visibility of the card caption';
  scxgwUIElementsPageFilterButton = 'Filter Button Visibility';
  scxgwUIElementsPageFilterButtonHint = 'Specifies whether to display filter dropdown buttons';
  scxgwUIElementsPageFilterRowVisible = 'Filter Row';
  scxgwUIElementsPageFilterRowVisibleHint =
    'A filter row is an extra row in the View that provides a simpler alternative to filter dropdowns';
  scxgwUIElementsPageInplaceEditForm = 'In-place Edit Form';
  scxgwUIElementsPageInplaceEditFormHint = 'Enables record editing using an in-place Edit Form';
  scxgwUIElementsPageHideCurrentRow = 'Hide Current Row';
  scxgwUIElementsPageHideCurrentRowHint = 'Hide the row being edited when the in-place Edit Form is active';

  // Table View Options Inplace Edit Form Page
  scxgwInplaceEditFormPageDescription = 'Configure the in-place Edit Form';
  scxgwInplaceEditFormPageTitle = 'In-place Edit Form Settings';
  scxgwInplaceEditFormPageDefaultStretch = 'Form Stretching';
  scxgwInplaceEditFormPageClient = 'Client'; //todo:
  scxgwInplaceEditFormPageClientHint = ''; //todo:
  scxgwInplaceEditFormPageHorizontal = 'Horizontal'; //todo:
  scxgwInplaceEditFormPageHorizontalHint = ''; //todo:
  scxgwInplaceEditFormPageNone = 'None'; //todo:
  scxgwInplaceEditFormPageNoneHint = ''; //todo:
  scxgwInplaceEditFormPageVertical = 'Vertical'; //todo:
  scxgwInplaceEditFormPageVerticalHint = ''; //todo:
  scxgwInplaceEditFormPageMasterRowDblClickAction = 'Master Row Double Click Action';
  scxgwInplaceEditFormPageShowEditForm = 'Show Edit Form';
  scxgwInplaceEditFormPageShowEditFormHint = ''; //todo:
  scxgwInplaceEditFormPageSwitchExpandedState = 'Switch Expanded State';
  scxgwInplaceEditFormPageSwitchExpandedStateHint = ''; //todo:
  scxgwInplaceEditFormPageUseDefaultLayout = 'Use Default Layout';
  scxgwInplaceEditFormPageUseDefaultLayoutHint = 'Use auto-generated layout within the Edit Form or customize it';
  scxgwInplaceEditFormPageDefaultColumnCount = 'Column Count';
  scxgwInplaceEditFormPageDefaultColumnCountHint = 'Form editors are arranged across the specified number of columns';

  // Table View Options Inplace Edit Form Layout Page
  scxgwInplaceEditFormLayoutPageDescription = 'Customize the Edit Form’s layout';
  scxgwInplaceEditFormLayoutPageTitle = 'In-place Edit Form Layout';

  // Table View Options Filtering Sorting Page
  scxgwFilteringSortingPageDescription = 'Specify capabilities and elements for filtering and sorting';
  scxgwFilteringSortingPageTitle = 'Filtering and Sorting';
  scxgwFilteringSortingPageColumnFiltering = 'Column Filtering';
  scxgwFilteringSortingPageColumnFilteringHint =
    'Specifies whether columns can display filter dropdown buttons';
  scxgwFilteringSortingPageHeaderFilterButtonShowMode = 'Filter Button Show Mode';
  scxgwFilteringSortingPageHeaderFilterButtonShowModeHint =
    'Specifies the display mode for filter dropdown buttons';
  scxgwFilteringSortingPageHeaderFilterButtonShowModeButton = 'Button';
  scxgwFilteringSortingPageHeaderFilterButtonShowModeButtonHint =
    'The normal filter dropdown button. It provides no specific' + #13#10 +
    'indication when the grid is filtered by a column`s values';
  scxgwFilteringSortingPageHeaderFilterButtonShowModeSmartTag = 'Smart Tag';
  scxgwFilteringSortingPageHeaderFilterButtonShowModeSmartTagHint =
    'The filter dropdown button is replaced with a small specially-designed' + #13#10 +
    'image imitating a smart tag. The image is highlighted when the grid is' + #13#10 +
    'filtered by a column`s values to help end-users easily discern which' + #13#10 +
    'columns have a filter applied.';
  scxgwFilteringSortingPageShowColumnFilterButton = 'Filter Button Visibility';
  scxgwFilteringSortingPageShowColumnFilterButtonAlways = 'Always';
  scxgwFilteringSortingPageShowColumnFilterButtonAlwaysHint =
    'Filter buttons are permanently visible in all rows (or columns)';
  scxgwFilteringSortingPageShowColumnFilterButtonWhenSelected = 'When Selected';
  scxgwFilteringSortingPageShowColumnFilterButtonWhenSelectedHint =
    'A filter button is shown only in the selected row (or column)';
  scxgwFilteringSortingPageColumnSorting = 'Column Sorting';
  scxgwFilteringSortingPageColumnSortingHint =
    'Determines whether records can be sorted according to column values';
  scxgwFilteringSortingPageFilterBoxVisible = 'Filter Box Visibility';
  scxgwFilteringSortingPageFilterBoxVisibleHint =
    'Specifies when the filter panel is visible';
  scxgwFilteringSortingPageFilterBoxVisibleAlways = 'Always';
  scxgwFilteringSortingPageFilterBoxVisibleAlwaysHint =
    'The filter panel is always visible';
  scxgwFilteringSortingPageFilterBoxVisibleNonEmpty = 'When Filter is Applied';
  scxgwFilteringSortingPageFilterBoxVisibleNonEmptyHint =
    'The filter panel is visible when filter conditions are applied to the View';
  scxgwFilteringSortingPageIncSearch = 'Incremental Search';
  scxgwFilteringSortingPageIncSearchHint =
    'Incremental search allows a user to locate records in a grid control' + #13#10 +
    'by matching the initial characters of a record field';

  // Behavior Page
  scxgwBehaviorPageDescription = 'Customize Grid View behavior';
  scxgwBehaviorPageTitle = 'Behavior';
  scxgwBehaviorPageFocusingOptions = 'Focusing Options';
  scxgwBehaviorPageCellMultiSelect = 'Cell Multiselect';
  scxgwBehaviorPageCellMultiSelectHint =
    'Specifies whether multiple cells can be selected within a View';
  scxgwBehaviorPageRowMultiSelect = 'Row Multiselect';
  scxgwBehaviorPageRowMultiSelectHint =
    'Determines whether multiple rows can be selected within a View';
  scxgwBehaviorPageRecordMultiSelect = 'Record Multiselect';
  scxgwBehaviorPageRecordMultiSelectHint =
    'Determines whether multiple records can be selected within a View';
  scxgwBehaviorPageFocusCellOnCycle = 'Focus Cell on Cycle';
  scxgwBehaviorPageFocusCellOnCycleHint =
    'Determines whether focus moves to the next/previous row after' + #13#10 +
    'passing the rightmost/leftmost cell within the current row';
  scxgwBehaviorPageFocusCellOnTab = 'Focus Cell on Tab';
  scxgwBehaviorPageFocusCellOnTabHint =
    'Determines whether the Tab key is used to navigate through the current View’s cells';
  scxgwBehaviorPageGoToNextCellOnEnter = 'Go to Next Cell on Enter';
  scxgwBehaviorPageGoToNextCellOnEnterHint =
    'Determines whether the current View columns can be navigated by using the Enter key';
  scxgwBehaviorPageSelectionOptions = 'Selection Options';
  scxgwBehaviorPageCellSelect = 'Cell Select';
  scxgwBehaviorPageCellSelectHint =
    'Determines whether individual cells can be selected within a View instead of whole rows';
  scxgwBehaviorPageExpandingOptions = 'Expanding Options';
  scxgwBehaviorPageRecordExpanding = 'Record Expanding';
  scxgwBehaviorPageRecordExpandingHint =
    'Specifies whether to display expand buttons within card captions';
  scxgwBehaviorPageGroupExpanding = 'Group Expanding';
  scxgwBehaviorPageGroupExpandingHint =
    'Specifies whether to display the Expand Button command' + #13#10 +
    'in the layout group’s context menu';
  scxgwBehaviorPageExpandRecordOnDblClick = 'Expand Record on Double Click';
  scxgwBehaviorPageExpandRecordOnDblClickHint =
    'Specifies whether to expand or collapse cards on double clicking the card caption';
  scxgwBehaviorPageFocusFirstCellOnNewRecord = 'Focus First Cell on New Record';
  scxgwBehaviorPageFocusFirstCellOnNewRecordHint =
    'Determines whether the focus moves to the first cell of a newly created row';
  scxgwBehaviorPageItemHotTrack = 'Item Hot-track';
  scxgwBehaviorPageItemHotTrackHint =
    'Specifies whether to hot-track data items';
  scxgwBehaviorPageHideSelection = 'Hide Selection';
  scxgwBehaviorPageHideSelectionHint =
    'Determines whether the selected rows are rendered' + #13#10 +
    'like other rows if focus leaves the grid control';
  scxgwBehaviorPageGridLines = 'Grid Lines';
  scxgwBehaviorPageGridLinesHorizontal = 'Horizontal';
  scxgwBehaviorPageGridLinesHorizontalHint =
    'Specifies whether horizontal grid lines are visible';
  scxgwBehaviorPageGridLinesVertical = 'Vertical';
  scxgwBehaviorPageGridLinesVerticalHint =
    'Specifies whether vertical grid lines are visible';

  // Summary Options Page
  scxgwSummaryPageDescription = 'Click the right mouse button on Footer or Group Footer to specify a summary for the required column';
  scxgwSummaryPageTitle = 'Summary';
  scxgwSummaryPageNullIgnore = 'Ignore Null Values';
  scxgwSummaryPageNullIgnoreHint = 'Determines whether NULL values are ignored when calculating summaries';
  scxgwSummaryPageAllRecords = 'For All Records';
  scxgwSummaryPageAllRecordsHint =
    'Determines whether summaries are calculated for all records';
  scxgwSummaryPageSelectedRecords = 'For Selected Records';
  scxgwSummaryPageSelectedRecordsHint =
    'Determines whether summaries are calculated for selected records only';
  scxgwSummaryPageMultipleSelectedRecords = 'For Two or More Selected Records';
  scxgwSummaryPageMultipleSelectedRecordsHint =
    'Determines whether summaries are calculated for ' + #13#10 +
    'selected records only when two or more records are selected';

  // Sizing Options Page
  scxgwSizingPageTitle = 'Sizing';
  scxgwSizingPageDescription = 'Customize sizing options';
  scxgwSizingPageCellAutoHeight = 'Cell Auto Height';
  scxgwSizingPageCellAutoHeightHint =
    'Determines whether the height of item cells is automatically' + #13#10 +
    'adjusted to ensure their contents is fully displayed';
  scxgwSizingPageColumnAutoWidth = 'Column Auto Width';
  scxgwSizingPageColumnAutoWidthHint =
    'Determines whether column widths are automatically adjusted' + #13#10 +
    'in order to display all columns without the need for a horizontal scrollbar';
  scxgwSizingPageDataRowSizing = 'Data Row Sizing';
  scxgwSizingPageDataRowSizingHint =
    'Indicates whether the end-user can resize data rows';
  scxgwSizingPageFooterAutoHeight = 'Footer Auto Height';
  scxgwSizingPageFooterAutoHeightHint =
    'Determines whether the View’s footer is automatically' + #13#10 +
    'stretched vertically to ensure its content is fully displayed';
  scxgwSizingPageGroupAutoSizingOptions = 'Auto Sizing Options';
  scxgwSizingPageGroupManualSizingOptions = 'Manual Sizing Options';
  scxgwSizingPageGroupRowSizing = 'Group Row Sizing';
  scxgwSizingPageGroupRowSizingHint =
    'Indicates whether the end-user can resize group rows';
  scxgwSizingPageHeaderAutoHeight = 'Header Auto Height';
  scxgwSizingPageHeaderAutoHeightHint =
    'Determines whether the column header’s height' + #13#10 +
    'automatically changes to display the full caption text';
  scxgwSizingPageCellEndEllipsis = 'Cell End Ellipsis';
  scxgwSizingPageCellEndEllipsisHint =
    'Specifies whether to display an ellipsis when cell text is clipped';

  // Layout View Customize Items Page
  scxgwLayoutViewCustomizeItemsPageDescription = 'Specify the most appropriate layout for your items';
  scxgwLayoutViewCustomizeItemsPageTitle = 'Customize Items';

  // Layout View Options View Page
  scxgwLayoutViewOptionsViewPageDescription = 'Specify how cards are arranged and stretched in the View';
  scxgwLayoutViewOptionsViewPageTitle = 'View';
  scxgwLayoutViewOptionsViewPageSingleRecordStretch = 'Card Stretching';
  scxgwLayoutViewOptionsViewPageSingleRecordStretchHint =
    'Specifies a card stretch option for a single card display mode';
  scxgwLayoutViewOptionsViewPageSingleRecordStretchNone = 'None';
  scxgwLayoutViewOptionsViewPageSingleRecordStretchNoneHint =
    'Cards are not stretched even if needed.';
  scxgwLayoutViewOptionsViewPageSingleRecordStretchClient = 'Client';
  scxgwLayoutViewOptionsViewPageSingleRecordStretchClientHint =
    'The card is stretched vertically to occupy the entire View’s client area';
  scxgwLayoutViewOptionsViewPageSingleRecordStretchHorizontal = 'Horizontal';
  scxgwLayoutViewOptionsViewPageSingleRecordStretchHorizontalHint =
    'The card is stretched horizontally to occupy the entire View’s width';
  scxgwLayoutViewOptionsViewPageSingleRecordStretchVertical = 'Vertical';
  scxgwLayoutViewOptionsViewPageSingleRecordStretchVerticalHint =
    'The card is stretched vertically to occupy the entire View’s height';
  scxgwLayoutViewOptionsViewPageCenterRecords = 'Center cards';
  scxgwLayoutViewOptionsViewPageCenterRecordsHint =
    'Specifies whether cards are displayed centered within the View';
  scxgwLayoutViewOptionsViewPageViewMode = 'View Mode';
  scxgwLayoutViewOptionsViewPageViewModeHint = 'Specifies card display modes';
  scxgwLayoutViewOptionsViewPageViewModeSingleRecord = 'Single card';
  scxgwLayoutViewOptionsViewPageViewModeSingleRecordHint =
    'A single card is displayed at a time';
  scxgwLayoutViewOptionsViewPageViewModeSingleRow = 'Single Row';
  scxgwLayoutViewOptionsViewPageViewModeSingleRowHint =
    'Cards are arranged in a single row';
  scxgwLayoutViewOptionsViewPageViewModeMultiRow = 'Multi Row';
  scxgwLayoutViewOptionsViewPageViewModeMultiRowHint =
    'Cards are arranged in multiple rows';
  scxgwLayoutViewOptionsViewPageViewModeSingleColumn = 'Single Column';
  scxgwLayoutViewOptionsViewPageViewModeSingleColumnHint =
    'Cards are arranged in a single column';
  scxgwLayoutViewOptionsViewPageViewModeMultiColumn = 'Multi Column';
  scxgwLayoutViewOptionsViewPageViewModeMultiColumnHint =
    'Cards are arranged in multiple columns';
  scxgwLayoutViewOptionsViewPageViewModeCarousel = 'Carousel';
  scxgwLayoutViewOptionsViewPageViewModeCarouselHint =
    'Cards are arranged in an ellipse with transparency,' + #13#10 +
    'scale, and animation effects that mimic a rolling carousel';

  // Layout View Carousel Mode Page
  scxgwLayoutViewCarouselPageDescription = 'Configure the carousel mode';
  scxgwLayoutViewCarouselPageTitle = 'Carousel Mode';
  scxgwLayoutViewCarouselAnimationInterval = 'Animation Interval:';
  scxgwLayoutViewCarouselAnimationIntervalHint =
    'Specifies the duration of the carousel rolling animation, in milliseconds';
  scxgwLayoutViewCarouselRadius = 'Radius:';
  scxgwLayoutViewCarouselRadiusHint =
    'Specifies the longest radius of the ellipse, in pixels';
  scxgwLayoutViewCarouselRecordCount = 'Record Count:';
  scxgwLayoutViewCarouselRecordCountHint =
    'Specifies the number of simultaneously displayed' + #13#10 +
    'cards (records) within the Layout View';
  scxgwLayoutViewCarouselBackgroundRecordOptions = 'Background Record Options';
  scxgwLayoutViewCarouselBackgroundRecordAlphaLevel = 'Alpha Level:';
  scxgwLayoutViewCarouselBackgroundRecordAlphaLevelHint =
    'Specifies the transparency level of the background cards that are furthest away';
  scxgwLayoutViewCarouselBackgroundRecordStartScale = 'Start Scale:';
  scxgwLayoutViewCarouselBackgroundRecordStartScaleHint =
    'Specifies the starting scale factor of background cards, as a percentage';
  scxgwLayoutViewCarouselBackgroundRecordEndScale = 'End Scale:';
  scxgwLayoutViewCarouselBackgroundRecordEndScaleHint =
    'Specifies the final scale factor of background cards, as a percentage';
  scxgwLayoutViewCarouselAngleOptions = 'Angle Options';
  scxgwLayoutViewCarouselRollAngle = 'Roll Angle';
  scxgwLayoutViewCarouselRollAngleHint =
    'Specifies the carousel’s roll angle, in degrees';
  scxgwLayoutViewCarouselPitchAngle = 'Pitch Angle';
  scxgwLayoutViewCarouselPitchAngleHint =
    'Specifies the carousel’s pitch angle, in degrees';
  scxgwLayoutViewCarouselAutoPitchAngle = 'Auto';
  scxgwLayoutViewCarouselAutoPitchAngleHint =
    'Ensures the pitch angle is automatically set so that' + #13#10 +
    'the carousel occupies the entire View height';

  // Card View UI Settings Page
  scxgwUIElementsCardViewExpandButtonAlignment = 'Expand Button Alignment';
  scxgwUIElementsCardViewExpandButtonAlignmentHint =
    'Specifies the horizontal alignment of the card expand button';
  scxgwUIElementsCardViewExpandButtonAlignmentLeft = 'Left';
  scxgwUIElementsCardViewExpandButtonAlignmentLeftHint =
    'Card expand buttons are left aligned within caption rows';
  scxgwUIElementsCardViewExpandButtonAlignmentRight = 'Right';
  scxgwUIElementsCardViewExpandButtonAlignmentRightHint =
    'Card expand buttons are right aligned within caption rows';
  scxgwUIElementsCardViewEmptyRows = 'Empty Rows';
  scxgwUIElementsCardViewEmptyRowsHint =
    'Specifies whether empty rows should be displayed';
  scxgwUIElementsCardViewRowLayout = 'Row Layout';
  scxgwUIElementsCardViewRowLayoutHint =
    'Specifies the card row layout';
  scxgwUIElementsCardViewRowLayoutHorizontal = 'Horizontal';
  scxgwUIElementsCardViewRowLayoutHorizontalHint =
    'Card rows are collected in a row, from left to right';
  scxgwUIElementsCardViewRowLayoutVertical = 'Vertical';
  scxgwUIElementsCardViewRowLayoutVerticalHint =
    'Card rows are collected in a column, from top to bottom';

  // Card View Sizing Page
  scxgwSizingPageCardAutoWidth = 'Card Auto Width';
  scxgwSizingPageCardAutoWidthHint =
    'Specifies whether the card width is automatically adjusted while resizing the View';
  scxgwSizingPageCardSizing = 'Card Sizing';
  scxgwSizingPageCardSizingHint =
    'Specifies whether end-users can resize cards at runtime';
  scxgwSizingPageRowCaptionAutoHeight = 'Row Caption Auto Height';
  scxgwSizingPageRowCaptionAutoHeightHint =
    'Determines whether the caption height of Card View rows is' + #13#10 +
    'automatically changed in order to entirely display the caption text';
  scxgwSizingPageRowCaptionEndEllipsis = 'Row Caption End Ellipsis';
  scxgwSizingPageRowCaptionEndEllipsisHint =
    'Specifies whether to display an ellipsis when the row caption text is clipped';

  // Card View Behavior Page
  scxgwBehaviorPageCardViewRowOptions = 'Row Options';
  scxgwBehaviorPageCardViewCardExpanding = 'Card Expanding';
  scxgwBehaviorPageCardViewCardExpandingHint =
    'Specifies whether cards can be collapsed and expanded';
  scxgwBehaviorPageCardViewRowExpanding = 'Row Expanding';
  scxgwBehaviorPageCardViewRowExpandingHint =
    'Specifies whether expand buttons will be drawn in category rows';
  scxgwBehaviorPageCardViewExpandRowOnDblClick = 'Expand Row on Double Click';
  scxgwBehaviorPageCardViewExpandRowOnDblClickHint =
    'Specifies whether a category row can be expanded' + #13#10 +
    'or collapsed when its caption is double-clicked';
  scxgwBehaviorPageCardViewRowHiding = 'Row Hiding';
  scxgwBehaviorPageCardViewRowHidingHint =
    'Determines the manner in which the Card View’s rows can be hidden from a View';
  scxgwBehaviorPageCardViewRowMoving = 'Row Moving';
  scxgwBehaviorPageCardViewRowMovingHint =
    'Determines whether the Card View’s rows can be moved by dragging their captions';

  // CustomizationForm Tab Captions
  scxgwCustomizationFormBandsTab = 'Hidden Bands';
  scxgwCustomizationFormColumnsTab = 'Hidden Columns';
  scxgwCustomizationFormRowsTab = 'Hidden Rows';
  scxgwCustomizationFormCardTreeViewTab = 'Card Tree View';

  // Chart View Settings Page
  scxGridWizardChartViewOptionsViewSettingsPageDescription = 'ChartViewOptionsViewSettingsPage description';
  scxGridWizardChartViewOptionsViewSettingsPageTitle = 'View settings';
  scxGridWizardOptionCaptionDataDrillDown = 'Data drill down';
  scxGridWizardOptionCaptionDataGroupOptions = 'Data group options';
  scxGridWizardOptionCaptionDataGroupHiding = 'Data group hiding';
  scxGridWizardOptionCaptionDataGroupMoving = 'Data group moving';
  scxGridWizardOptionCaptionCustomizationOptions = 'Customization options';
  scxGridWizardOptionCaptionOptionsCustomization = 'Options customization';
  scxGridWizardOptionCaptionSeriesCustomization = 'Series customization';
  scxGridWizardOptionCaptionToolBoxCustomizeButton = 'Customize button';
  scxGridWizardOptionCaptionAntialiasing = 'Antialiasing';
  scxGridWizardOptionCaptionTransparentCaption = 'Transparent caption';

implementation

end.
