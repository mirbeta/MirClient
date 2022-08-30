{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetUIGeneratorScheme;

{$I cxVer.Inc}

interface

const
  sdxSpreadSheetActionsCategoryName = 'DevExpress ExpressSpreadSheet';

const
  sdxSpreadSheetTabFile = 'File';
  sdxSpreadSheetBarFileCommon = 'Common';
  sdxSpreadSheetBarFilePrint = 'Print';

  sdxSpreadSheetTabHome = 'Home';
  sdxSpreadSheetBarHomeClipboard = 'Clipboard';
  sdxSpreadSheetBarHomeFont = 'Font';
  sdxSpreadSheetSubItemHomeFontBorders = 'Borders';
  sdxSpreadSheetBarHomeAlignment = 'Alignment';
  sdxSpreadSheetSubItemHomeAlignmentMergeCells = 'Merge Cells';
  sdxSpreadSheetBarHomeConditionalFormatting = 'Conditional Formatting';
  sdxSpreadSheetSubItemHomeConditionalFormattingConditionalFormatting = 'Conditional Formatting';
  sdxSpreadSheetSubItemHomeConditionalFormattingConditionalFormattingClearRules = 'Clear Rules';
  sdxSpreadSheetBarHomeCells = 'Cells';
  sdxSpreadSheetSubItemHomeCellsInsert = 'Insert';
  sdxSpreadSheetSubItemHomeCellsDelete = 'Delete';
  sdxSpreadSheetSubItemHomeCellsFormat = 'Format';
  sdxSpreadSheetSubItemHomeCellsFormatHideAndUnhide = 'Hide and Unhide';
  sdxSpreadSheetBarHomeEditing = 'Editing';
  sdxSpreadSheetSubItemHomeEditingClear = 'Clear';

  sdxSpreadSheetTabInsert = 'Insert';
  sdxSpreadSheetBarInsertIllustrations = 'Illustrations';
  sdxSpreadSheetBarInsertLinks = 'Links';

  sdxSpreadSheetTabPageLayout = 'Page Layout';
  sdxSpreadSheetBarPageSetup = 'Page Setup';
  sdxSpreadSheetSubItemPageBreaks = 'Breaks';
  sdxSpreadSheetSubItemPageOrientation = 'Orientation';
  sdxSpreadSheetSubItemPrintArea = 'Print Area';

  sdxSpreadSheetTabFormulas = 'Formulas';
  sdxSpreadSheetBarFormulasFunctionLibrary = 'Function Library';
  sdxSpreadSheetSubItemFormulasFunctionLibraryMore = 'More';

  sdxSpreadSheetTabData = 'Data';
  sdxSpreadSheetBarDataSortFilter = 'Sort Filter';
  sdxSpreadSheetBarDataGrouping = 'Grouping';
  sdxSpreadSheetSubItemDataGroupingGroup = 'Group';
  sdxSpreadSheetSubItemDataGroupingUngroup = 'Ungroup';

  sdxSpreadSheetTabReview = 'Review';
  sdxSpreadSheetBarReviewComments = 'Comments';
  sdxSpreadSheetBarReviewChanges = 'Changes';

  sdxSpreadSheetTabView = 'View';
  sdxSpreadSheetBarViewZoom = 'Zoom';
  sdxSpreadSheetBarViewFreezePanes = 'Freeze Panes';
  sdxSpreadSheetSubItemViewFreezePanesFreezePanes = 'Freeze Panes';

procedure RegisterSpreadSheetUIGeneratorScheme;

implementation

uses
  dxUIGenerator, dxSpreadSheetCore, dxSpreadSheet, dxSpreadSheetActions, cxClasses;

procedure RegisterCategoryCommon(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSpreadSheetNewDocument);
  ACategory.Add(TdxSpreadSheetOpenDocument);
  ACategory.Add(TdxSpreadSheetSaveDocumentAs);
end;

procedure RegisterCategoryPrinting(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSpreadSheetShowPrintForm);
  ACategory.Add(TdxSpreadSheetShowPrintPreviewForm);
  ACategory.Add(TdxSpreadSheetShowPageSetupForm);
end;

procedure RegisterCategoryClipboard(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSpreadSheetPasteSelection);
  ACategory.Add(TdxSpreadSheetCutSelection, [ugivlSmallIcon, ugivlText], ugigpNone, ugipBeginsNewColumn);
  ACategory.Add(TdxSpreadSheetCopySelection, [ugivlSmallIcon, ugivlText]);
end;

procedure RegisterCategoryFont(ACategory: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACategory.Add(TdxSpreadSheetChangeFontName, [ugivlSmallIcon]);
  ACategory.Add(TdxSpreadSheetChangeFontSize, [ugivlSmallIcon], ugigpNone, ugipContinuesRow);
  ACategory.Add(TdxSpreadSheetIncreaseFontSize, [ugivlSmallIcon], ugigpStart, ugipContinuesRow);
  ACategory.Add(TdxSpreadSheetDecreaseFontSize, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);

  ACategory.Add(TdxSpreadSheetToggleFontBold, [ugivlSmallIcon], ugigpStart);
  ACategory.Add(TdxSpreadSheetToggleFontItalic, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategory.Add(TdxSpreadSheetToggleFontUnderline, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategory.Add(TdxSpreadSheetToggleFontStrikeout, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);

  ACommand := ACategory.Add('Rich Edit\BordersAll.png', sdxSpreadSheetSubItemHomeFontBorders, [ugivlSmallIcon],
    ugigpStart, ugipContinuesRow);
  ACommand.Add(TdxSpreadSheetBordersBottom);
  ACommand.Add(TdxSpreadSheetBordersTop);
  ACommand.Add(TdxSpreadSheetBordersLeft);
  ACommand.Add(TdxSpreadSheetBordersRight);
  ACommand.Add(TdxSpreadSheetBordersNone, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);
  ACommand.Add(TdxSpreadSheetBordersAll);
  ACommand.Add(TdxSpreadSheetBordersOutside);
  ACommand.Add(TdxSpreadSheetBordersOutsideThick);
  ACommand.Add(TdxSpreadSheetBordersBottomDouble, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow,
    True);
  ACommand.Add(TdxSpreadSheetBordersBottomThick);
  ACommand.Add(TdxSpreadSheetBordersTopAndBottom);
  ACommand.Add(TdxSpreadSheetBordersTopAndBottomThick);
  ACommand.Add(TdxSpreadSheetBordersTopAndBottomDouble);
  ACommand.Add(TdxSpreadSheetBordersMore, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);

  ACategory.Add(TdxSpreadSheetChangeFillColor, [ugivlSmallIcon], ugigpStart, ugipContinuesRow);
  ACategory.Add(TdxSpreadSheetChangeFontColor, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
end;

procedure RegisterCategoryAlignment(ACategory: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACategory.Add(TdxSpreadSheetAlignVerticalTop, [ugivlSmallIcon], ugigpStart);
  ACategory.Add(TdxSpreadSheetAlignVerticalCenter, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategory.Add(TdxSpreadSheetAlignVerticalBottom, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);

  ACategory.Add(TdxSpreadSheetAlignHorizontalLeft, [ugivlSmallIcon], ugigpStart);
  ACategory.Add(TdxSpreadSheetAlignHorizontalCenter, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategory.Add(TdxSpreadSheetAlignHorizontalRight, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);

  ACategory.Add(TdxSpreadSheetTextIndentDecrease, [ugivlSmallIcon], ugigpStart, ugipContinuesRow);
  ACategory.Add(TdxSpreadSheetTextIndentIncrease, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);

  ACategory.Add(TdxSpreadSheetTextWrap, [ugivlSmallIcon, ugivlText], ugigpNone, ugipBeginsNewColumn);

  ACommand := ACategory.Add('Alignment\MergeCenter.png', sdxSpreadSheetSubItemHomeAlignmentMergeCells,
    [ugivlSmallIcon, ugivlText]);
  ACommand.Add(TdxSpreadSheetMergeCellsAndCenter);
  ACommand.Add(TdxSpreadSheetMergeCellsAcross);
  ACommand.Add(TdxSpreadSheetMergeCells);
  ACommand.Add(TdxSpreadSheetUnmergeCells);
end;

procedure RegisterCategoryCells(ACategory: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategory.Add('Spreadsheet\InsertCells.png', sdxSpreadSheetSubItemHomeCellsInsert);
  ACommand.Add(TdxSpreadSheetInsertRows);
  ACommand.Add(TdxSpreadSheetInsertColumns);
  ACommand.Add(TdxSpreadSheetInsertSheet, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);

  ACommand := ACategory.Add('Spreadsheet\DeleteSheetCells.png', sdxSpreadSheetSubItemHomeCellsDelete);
  ACommand.Add(TdxSpreadSheetDeleteRows);
  ACommand.Add(TdxSpreadSheetDeleteColumns);
  ACommand.Add(TdxSpreadSheetDeleteSheet, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);

  ACommand := ACategory.Add('Spreadsheet\Format.png', sdxSpreadSheetSubItemHomeCellsFormat);
  ACommand.Add(TdxSpreadSheetAutoFitRowHeight);
  ACommand.Add(TdxSpreadSheetAutoFitColumnWidth);

  ACommand := ACommand.Add('',  sdxSpreadSheetSubItemHomeCellsFormatHideAndUnhide);
  ACommand.Add(TdxSpreadSheetHideRows);
  ACommand.Add(TdxSpreadSheetHideColumns);
  ACommand.Add(TdxSpreadSheetHideSheet);
  ACommand.Add(TdxSpreadSheetUnhideRows, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);
  ACommand.Add(TdxSpreadSheetUnhideColumns);
  ACommand.Add(TdxSpreadSheetUnhideSheet);
end;

procedure RegisterCategoryEditing(ACategory: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategory.Add('Actions\Clear.png', sdxSpreadSheetSubItemHomeEditingClear);
  ACommand.Add(TdxSpreadSheetClearAll);
  ACommand.Add(TdxSpreadSheetClearFormats);
  ACommand.Add(TdxSpreadSheetClearContents);

  ACategory.Add(TdxSpreadSheetUndo);
  ACategory.Add(TdxSpreadSheetRedo);

  ACategory.Add(TdxSpreadSheetFindAndReplace);
end;

procedure RegisterCategoryIllustrations(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSpreadSheetInsertPicture);
end;

procedure RegisterCategoryLinks(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSpreadSheetShowHyperlinkEditor);
end;

procedure RegisterCategoryPageSetup(ACategory: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
  AGallery: TdxUIGeneratorGalleryInfo;
begin
  AGallery := ACategory.Add(TdxSpreadSheetPageMarginsGallery, True, True, True, False, 1, 3);
  AGallery.Add(TdxSpreadSheetMorePageMargins);

  ACategory.Add(TdxSpreadSheetPageOrientationGallery, True, False, True, False, 1, 2, True, vaCenter);

  AGallery := ACategory.Add(TdxSpreadSheetPaperSizeGallery, True, True, True, False, 1, 1);
  AGallery.Add(TdxSpreadSheetMorePaperSizes);

  ACommand := ACategory.Add('Print\PrintArea.png', sdxSpreadSheetSubItemPrintArea);
  ACommand.Add(TdxSpreadSheetSetPrintArea);
  ACommand.Add(TdxSpreadSheetClearPrintArea);

  ACommand := ACategory.Add('Pages\InsertPageBreak.png', sdxSpreadSheetSubItemPageBreaks);
  ACommand.Add(TdxSpreadSheetInsertPageBreak);
  ACommand.Add(TdxSpreadSheetRemovePageBreak);
  ACommand.Add(TdxSpreadSheetResetAllPageBreaks, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);

  ACategory.Add(TdxSpreadSheetPrintTitles);
end;

procedure RegisterCategoryFunctionLibrary(ACategory: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACategory.Add(TdxSpreadSheetAutoSumGallery, True, False, True, False, 1, 2, False);

  ACategory.Add(TdxSpreadSheetFinancialFormulasGallery, True, False, True, False, 1, 2, False);

  ACategory.Add(TdxSpreadSheetLogicalFormulasGallery, True, False, True, False, 1, 2, False);

  ACategory.Add(TdxSpreadSheetTextFormulasGallery, True, False, True, False, 1, 2, False);

  ACategory.Add(TdxSpreadSheetDateAndTimeFormulasGallery, True, False, True, False, 1, 2, False);

  ACategory.Add(TdxSpreadSheetLookupAndReferenceFormulasGallery, True, False, True, False, 1, 2, False);

  ACategory.Add(TdxSpreadSheetMathAndTrigFormulasGallery, True, False, True, True, 1, 2, False);

  ACommand := ACategory.Add('Function Library\MoreFunctions.png', sdxSpreadSheetSubItemFormulasFunctionLibraryMore);
  ACommand.Add(TdxSpreadSheetStatisticalFormulasGallery, True, False, True, True, 1, 2, False);
  ACommand.Add(TdxSpreadSheetInformationFormulasGallery, True, False, True, False, 1, 2, False);
  ACommand.Add(TdxSpreadSheetCompatibilityFormulasGallery, True, False, True, False, 1, 2, False);
end;

procedure RegisterCategorySortFilter(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSpreadSheetSortAscending);
  ACategory.Add(TdxSpreadSheetSortDescending);
end;

procedure RegisterCategoryGrouping(ACategory: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategory.Add('Spreadsheet\GroupRows.png', sdxSpreadSheetSubItemDataGroupingGroup);
  ACommand.Add(TdxSpreadSheetGroupColumns);
  ACommand.Add(TdxSpreadSheetGroupRows);

  ACommand := ACategory.Add('Spreadsheet\UngroupRows.png', sdxSpreadSheetSubItemDataGroupingUngroup);
  ACommand.Add(TdxSpreadSheetUngroupColumns);
  ACommand.Add(TdxSpreadSheetUngroupRows);
end;

procedure RegisterCategoryComments(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSpreadSheetNewComment);
  ACategory.Add(TdxSpreadSheetEditComment);
  ACategory.Add(TdxSpreadSheetDeleteComments);
  ACategory.Add(TdxSpreadSheetPreviousComment, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);
  ACategory.Add(TdxSpreadSheetNextComment);
  ACategory.Add(TdxSpreadSheetShowHideComments, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);
end;

procedure RegisterCategoryChanges(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSpreadSheetProtectSheet);
  ACategory.Add(TdxSpreadSheetProtectWorkbook);
end;

procedure RegisterCategoryZoom(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSpreadSheetZoomOut);
  ACategory.Add(TdxSpreadSheetZoomIn);
  ACategory.Add(TdxSpreadSheetZoomDefault);
end;

procedure RegisterCategoryFreezePanes(ACategory: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategory.Add('Spreadsheet\FreezePanes.png', sdxSpreadSheetSubItemViewFreezePanesFreezePanes);
  ACommand.Add(TdxSpreadSheetFreezePanes);
  ACommand.Add(TdxSpreadSheetUnfreezePanes);
  ACommand.Add(TdxSpreadSheetFreezeTopRow);
  ACommand.Add(TdxSpreadSheetFreezeFirstColumn);
end;

procedure RegisterSpreadSheetUIGeneratorScheme;
var
  AComponent: TdxUIGeneratorComponentInfo;
begin
  AComponent := TdxUIGenerator.RegisterComponent(TdxCustomSpreadSheet, sdxSpreadSheetActionsCategoryName);

  RegisterCategoryCommon(AComponent.Add(sdxSpreadSheetTabFile, sdxSpreadSheetBarFileCommon, 'Actions\New_16x16.png', 0));
  RegisterCategoryPrinting(AComponent.Add(sdxSpreadSheetTabFile, sdxSpreadSheetBarFilePrint, 'Print\Print_16x16.png', 0));

  RegisterCategoryClipboard(AComponent.Add(sdxSpreadSheetTabHome, sdxSpreadSheetBarHomeClipboard, 'Edit\Paste_16x16.png', 0));
  RegisterCategoryFont(AComponent.Add(sdxSpreadSheetTabHome, sdxSpreadSheetBarHomeFont, 'Rich Edit\FontColor_16x16.png', 1));
  RegisterCategoryAlignment(AComponent.Add(sdxSpreadSheetTabHome, sdxSpreadSheetBarHomeAlignment, 'Format\AlignCenter_16x16.png', 1));
  RegisterCategoryCells(AComponent.Add(sdxSpreadSheetTabHome, sdxSpreadSheetBarHomeCells, 'Spreadsheet\Format_16x16.png', 2));
  RegisterCategoryEditing(AComponent.Add(sdxSpreadSheetTabHome, sdxSpreadSheetBarHomeEditing, 'Find\Find_16x16.png', 0));

  RegisterCategoryIllustrations(AComponent.Add(sdxSpreadSheetTabInsert, sdxSpreadSheetBarInsertIllustrations, 'Content\Image_16x16.png', 0));
  RegisterCategoryLinks(AComponent.Add(sdxSpreadSheetTabInsert, sdxSpreadSheetBarInsertLinks, 'Rich Edit\Hyperlink_16x16.png', 0));

  RegisterCategoryPageSetup(AComponent.Add(sdxSpreadSheetTabPageLayout, sdxSpreadSheetBarPageSetup, 'Pages\PaperSize_16x16.png', 0));

  RegisterCategoryFunctionLibrary(AComponent.Add(sdxSpreadSheetTabFormulas, sdxSpreadSheetBarFormulasFunctionLibrary, 'Rich Edit\InsertEquationCaption_16x16.png', 0));

  RegisterCategorySortFilter(AComponent.Add(sdxSpreadSheetTabData, sdxSpreadSheetBarDataSortFilter, 'Data\SortAsc_16x16.png', 0));
  RegisterCategoryGrouping(AComponent.Add(sdxSpreadSheetTabData, sdxSpreadSheetBarDataGrouping, 'Spreadsheet\GroupRows_16x16.png', 0));

  RegisterCategoryComments(AComponent.Add(sdxSpreadSheetTabReview, sdxSpreadSheetBarReviewComments, 'Comments\InsertComment_16x16.png', 2));
  RegisterCategoryChanges(AComponent.Add(sdxSpreadSheetTabReview, sdxSpreadSheetBarReviewChanges, 'SpreadSheet\ProtectSheet_16x16.png', 2));

  RegisterCategoryZoom(AComponent.Add(sdxSpreadSheetTabView, sdxSpreadSheetBarViewZoom, 'Zoom\Zoom_16x16.png', 0));
  RegisterCategoryFreezePanes(AComponent.Add(sdxSpreadSheetTabView, sdxSpreadSheetBarViewFreezePanes, 'Spreadsheet\FreezePanes_16x16.png', 2));
end;

end.
