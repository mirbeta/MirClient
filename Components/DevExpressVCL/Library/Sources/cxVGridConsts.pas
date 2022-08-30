{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressVerticalGrid                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSVERTICALGRID AND ALL           }
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
unit cxVGridConsts;

{$I cxVer.inc}

interface

resourcestring
  cxSvgIndexError = 'Index error';
  cxSvgInvalidRowClass = 'Can''t create row';
  cxSvgAssignRowsError = 'Can''t assign rows';
  cxSvgDeletingFocusedConfirmationText = 'Delete record?';
  cxSvgDeletingConfirmationCaption = 'Confirm';

  cxSvgOKCaption = 'OK';
  cxSvgCancelCaption = 'Cancel';

  scxSvgFindPanelClearButtonCaption = 'Clear';
  scxSvgFindPanelInfoText = 'Enter text to search...';
  scxSvgFindPanelFindButtonCaption = 'Find';

  cxSvgCustomizeCaption = 'Customize';
  cxSvgCustomizeCategoriesCaption = 'Categories';
  cxSvgCustomizeDeleteCategory = '&Delete';
  cxSvgCustomizeNewCategory = '&New...';
  cxSvgCustomizeRowsCaption = 'Rows';

  cxSvgNewCategoryCaption = 'New Category';
  cxSvgNewCategoryLabelCaption = '&Category:';

  cxSvgLayoutEditorCaption = 'Layout editor';
  cxSvgLayoutEditorCustomize = '&Customize';

  cxSvgFilterApplyButtonCaption = 'Apply Filter';

  cxSvgFilterIsEmpty = '<Filter is Empty>';
  cxSvgFilterCustomizeButtonCaption = 'Customize...';

  cxSvgMonthFormat = 'mmmm yyyy';
  cxSvgYearFormat = 'yyyy';

  cxSvgYesterday = 'Yesterday';
  cxSvgToday = 'Today';
  cxSvgTomorrow = 'Tomorrow';
  cxSvgLast30Days = 'Last 30 days';
  cxSvgLast14Days = 'Last 14 days';
  cxSvgLast7Days = 'Last 7 days';
  cxSvgNext7Days = 'Next 7 days';
  cxSvgNext14Days = 'Next 14 days';
  cxSvgNext30Days = 'Next 30 days';
  cxSvgLastTwoWeeks = 'Last two weeks';
  cxSvgLastWeek = 'Last week';
  cxSvgThisWeek = 'This week';
  cxSvgNextWeek = 'Next week';
  cxSvgNextTwoWeeks = 'Next two weeks';
  cxSvgLastMonth = 'Last month';
  cxSvgThisMonth = 'This month';
  cxSvgNextMonth = 'Next month';
  cxSvgLastYear = 'Last year';
  cxSvgThisYear = 'This year';
  cxSvgNextYear = 'Next year';
  cxSvgPast = 'Past';
  cxSvgFuture = 'Future';

  //cxRTTIInspector
  cxSvgRTTIInspectorEmptyGlyph = '(None)';
  cxSvgUnknown = '(Unknown)';
  cxSvgExportNotVisibleControl = 'Can''t export invisible control';
  cxSvgRTTICollectionEditCaption = 'Editing %s%s%s';
  cxSvgRTTICollectionAdd = '&Add';
  cxSvgRTTICollectionAddHint = 'Add New';
  cxSvgRTTICollectionDelete = '&Delete';
  cxSvgRTTICollectionDeleteHint = 'Delete Selected';
  cxSvgRTTICollectionSelectAll = '&Select All';
  cxSvgRTTICollectionToolbar = '&Toolbar';
  cxSvgRTTICollectionTextLabel = 'Text &Labels';
  cxSvgRTTICollectionMoveUp = 'Move &Up';
  cxSvgRTTICollectionMoveUpHint = 'Move Selected Up';
  cxSvgRTTICollectionMoveDown = 'Move Dow&n';
  cxSvgRTTICollectionMoveDownHint = 'Move Selected Down';

implementation

uses
  dxCore;

procedure AddExpressVerticalGridResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAddress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAddress);
  end;

begin
  InternalAdd('cxSvgIndexError', @cxSvgIndexError);
  InternalAdd('cxSvgInvalidRowClass', @cxSvgInvalidRowClass);
  InternalAdd('cxSvgAssignRowsError', @cxSvgAssignRowsError);
  InternalAdd('cxSvgDeletingFocusedConfirmationText', @cxSvgDeletingFocusedConfirmationText);
  InternalAdd('cxSvgDeletingConfirmationCaption', @cxSvgDeletingConfirmationCaption);
  InternalAdd('cxSvgOKCaption', @cxSvgOKCaption);
  InternalAdd('cxSvgCancelCaption', @cxSvgCancelCaption);
  InternalAdd('cxSvgCustomizeCaption', @cxSvgCustomizeCaption);
  InternalAdd('cxSvgCustomizeCategoriesCaption', @cxSvgCustomizeCategoriesCaption);
  InternalAdd('cxSvgCustomizeDeleteCategory', @cxSvgCustomizeDeleteCategory);
  InternalAdd('cxSvgCustomizeNewCategory', @cxSvgCustomizeNewCategory);
  InternalAdd('cxSvgCustomizeRowsCaption', @cxSvgCustomizeRowsCaption);
  InternalAdd('cxSvgNewCategoryCaption', @cxSvgNewCategoryCaption);
  InternalAdd('cxSvgNewCategoryLabelCaption', @cxSvgNewCategoryLabelCaption);
  InternalAdd('cxSvgLayoutEditorCaption', @cxSvgLayoutEditorCaption);
  InternalAdd('cxSvgLayoutEditorCustomize', @cxSvgLayoutEditorCustomize);
  InternalAdd('cxSvgRTTIInspectorEmptyGlyph', @cxSvgRTTIInspectorEmptyGlyph);
  InternalAdd('cxSvgUnknown', @cxSvgUnknown);
  InternalAdd('cxSvgExportNotVisibleControl', @cxSvgExportNotVisibleControl);
  InternalAdd('cxSvgRTTICollectionEditCaption', @cxSvgRTTICollectionEditCaption);
  InternalAdd('cxSvgRTTICollectionAdd', @cxSvgRTTICollectionAdd);
  InternalAdd('cxSvgRTTICollectionAddHint', @cxSvgRTTICollectionAddHint);
  InternalAdd('cxSvgRTTICollectionDelete', @cxSvgRTTICollectionDelete);
  InternalAdd('cxSvgRTTICollectionDeleteHint', @cxSvgRTTICollectionDeleteHint);
  InternalAdd('cxSvgRTTICollectionSelectAll', @cxSvgRTTICollectionSelectAll);
  InternalAdd('cxSvgRTTICollectionToolbar', @cxSvgRTTICollectionToolbar);
  InternalAdd('cxSvgRTTICollectionTextLabel', @cxSvgRTTICollectionTextLabel);
  InternalAdd('cxSvgRTTICollectionMoveUp', @cxSvgRTTICollectionMoveUp);
  InternalAdd('cxSvgRTTICollectionMoveUpHint', @cxSvgRTTICollectionMoveUpHint);
  InternalAdd('cxSvgRTTICollectionMoveDown', @cxSvgRTTICollectionMoveDown);
  InternalAdd('cxSvgRTTICollectionMoveDownHint', @cxSvgRTTICollectionMoveDownHint);
  InternalAdd('scxSvgFindPanelClearButtonCaption', @scxSvgFindPanelClearButtonCaption);
  InternalAdd('scxSvgFindPanelInfoText', @scxSvgFindPanelInfoText);
  InternalAdd('scxSvgFindPanelFindButtonCaption', @scxSvgFindPanelFindButtonCaption);
  InternalAdd('cxSvgFilterApplyButtonCaption', @cxSvgFilterApplyButtonCaption);
  InternalAdd('cxSvgFilterCustomizeButtonCaption', @cxSvgFilterCustomizeButtonCaption);
  InternalAdd('cxSvgFilterIsEmpty', @cxSvgFilterIsEmpty);
  InternalAdd('cxSvgMonthFormat', @cxSvgMonthFormat);
  InternalAdd('cxSvgYearFormat', @cxSvgYearFormat);
  InternalAdd('cxSvgYesterday', @cxSvgYesterday);
  InternalAdd('cxSvgToday', @cxSvgToday);
  InternalAdd('cxSvgTomorrow', @cxSvgTomorrow);
  InternalAdd('cxSvgLast30Days', @cxSvgLast30Days);
  InternalAdd('cxSvgLast14Days', @cxSvgLast14Days);
  InternalAdd('cxSvgLast7Days', @cxSvgLast7Days);
  InternalAdd('cxSvgNext7Days', @cxSvgNext7Days);
  InternalAdd('cxSvgNext14Days', @cxSvgNext14Days);
  InternalAdd('cxSvgNext30Days', @cxSvgNext30Days);
  InternalAdd('cxSvgLastTwoWeeks', @cxSvgLastTwoWeeks);
  InternalAdd('cxSvgLastWeek', @cxSvgLastWeek);
  InternalAdd('cxSvgThisWeek', @cxSvgThisWeek);
  InternalAdd('cxSvgNextWeek', @cxSvgNextWeek);
  InternalAdd('cxSvgNextTwoWeeks', @cxSvgNextTwoWeeks);
  InternalAdd('cxSvgLastMonth', @cxSvgLastMonth);
  InternalAdd('cxSvgThisMonth', @cxSvgThisMonth);
  InternalAdd('cxSvgNextMonth', @cxSvgNextMonth);
  InternalAdd('cxSvgLastYear', @cxSvgLastYear);
  InternalAdd('cxSvgThisYear', @cxSvgThisYear);
  InternalAdd('cxSvgNextYear', @cxSvgNextYear);
  InternalAdd('cxSvgPast', @cxSvgPast);
  InternalAdd('cxSvgFuture', @cxSvgFuture);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressVerticalGrid', @AddExpressVerticalGridResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressVerticalGrid');

end.

