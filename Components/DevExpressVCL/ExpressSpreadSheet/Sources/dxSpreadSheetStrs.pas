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

unit dxSpreadSheetStrs;

{$I cxVer.Inc}

interface

uses
  dxCore, cxClasses, dxSpreadSheetTypes, dxSpreadSheetGraphics, Graphics;

resourcestring
  // Popup Menu
  sdxBuiltInPopupMenuBringToFront = 'Bring to F&ront';
  sdxBuiltInPopupMenuClearContents = 'Clear Co&ntents';
  sdxBuiltInPopupMenuCopy = '&Copy';
  sdxBuiltInPopupMenuCustomizeObject = 'C&ustomize Object...';
  sdxBuiltInPopupMenuCut = 'Cu&t';
  sdxBuiltInPopupMenuDelete = '&Delete';
  sdxBuiltInPopupMenuDeleteDialog = '&Delete...';
  sdxBuiltInPopupMenuFormatCells = '&Format Cells...';
  sdxBuiltInPopupMenuHide = '&Hide';
  sdxBuiltInPopupMenuInsert = '&Insert';
  sdxBuiltInPopupMenuInsertDialog = '&Insert...';
  sdxBuiltInPopupMenuMergeCells = '&Merge Cells';
  sdxBuiltInPopupMenuPaste = '&Paste';
  sdxBuiltInPopupMenuPasteSpecial = 'Paste Special';
  sdxBuiltInPopupMenuPasteSpecialAll = '&Paste';
  sdxBuiltInPopupMenuPasteSpecialFormulas = '&Formulas';
  sdxBuiltInPopupMenuPasteSpecialFormulasAndColumnWidths = 'Keep Source Column &Widths';
  sdxBuiltInPopupMenuPasteSpecialFormulasAndFormatting = 'F&ormulas and Number Formatting';
  sdxBuiltInPopupMenuPasteSpecialFormulasAndStyles = '&Keep Source Formatting';
  sdxBuiltInPopupMenuPasteSpecialShowDialog = 'Paste Special...';
  sdxBuiltInPopupMenuPasteSpecialValues = '&Values';
  sdxBuiltInPopupMenuPasteSpecialValuesAndFormatting = 'V&alues and Number Formatting';
  sdxBuiltInPopupMenuPasteSpecialValuesAndStyles = 'Valu&es and Source Formatting';

  sdxBuiltInPopupMenuRename = '&Rename...';
  sdxBuiltInPopupMenuSendToBack = 'Send to Bac&k';
  sdxBuiltInPopupMenuSplitCells = 'U&nmerge Cells';
  sdxBuiltInPopupMenuUnhide = '&Unhide';
  sdxBuiltInPopupMenuUnhideDialog = '&Unhide...';
  sdxBuiltInPopupMenuCreateHyperlink = '&Hyperlink...';
  sdxBuiltInPopupMenuEditHyperlink = 'Edit &Hyperlink...';
  sdxBuiltInPopupMenuOpenHyperlink = '&Open Hyperlink';
  sdxBuiltInPopupMenuRemoveHyperlink = '&Remove Hyperlink';
  sdxBuiltInPopupMenuDeleteComment = 'Delete Co&mment';
  sdxBuiltInPopupMenuEditComment = '&Edit Comment...';
  sdxBuiltInPopupMenuHideComment = '&Hide Comment';
  sdxBuiltInPopupMenuInsertComment = 'Insert Co&mment...';
  sdxBuiltInPopupMenuShowComment = 'Sh&ow Comment';
  sdxBuiltInPopupMenuProtectSheet = '&Protect Sheet...';
  sdxBuiltInPopupMenuUnprotectSheet = 'Un&protect Sheet...';

  // Rename Sheet Dialog
  sdxRenameDialogCaption = 'Rename Sheet';
  sdxRenameDialogSheetName = 'Sheet Name:';

  // File Dialog
  sdxFileDialogAllSupported = 'All Supported';

  // Actions
  sdxActionAddGroup = 'Group';
  sdxActionAutoFill = 'Auto Fill';
  sdxActionCellEditing = 'Cell Editing';
  sdxActionCellsMerge = 'Cells Merge';
  sdxActionChangeConditionalFormatting = 'Change Conditional Formatting';
  sdxActionChangePrintingOptions = 'Change Printing Options';
  sdxActionCreateDefinedName = 'Create Defined Name';
  sdxActionChangeContainer = 'Change Container';
  sdxActionChangeGroup = 'Change Group';
  sdxActionChangeHyperlink = 'Change Hyperlink';
  sdxActionChangeRowColumn = 'Change Row/Column';
  sdxActionClearCells = 'Clear Cell(s)';
  sdxActionCutCells = 'Cut Cells';
  sdxActionDeleteCells = 'Delete Cells';
  sdxActionDeleteComment = 'Delete Comment';
  sdxActionDeleteGroup = 'Ungroup';
  sdxActionDragAndDrop = 'Drag and Drop';
  sdxActionEditComment = 'Edit Comment';
  sdxActionExpandCollapseGroup = 'Show/Hide Details';
  sdxActionFillCells = 'Fill Cells';
  sdxActionFormatCells = 'Format Cells';
  sdxActionInsertCells = 'Insert Cells';
  sdxActionMoveCells = 'Move Cells';
  sdxActionPasteCells = 'Paste Cells';
  sdxActionReplace = 'Replace';
  sdxActionSortCells = 'Sort Cells';

  // Hyperlink
  sdxDefaultHyperlinkScreenTip = '%s - Click once to follow.'#13#10'Click and hold to select this cell.';
  sdxDefaultHyperlinkShortScreenTip = '%s - Click once to follow.';
  scxSelectionInDocument = '<< Selection in Document >>';
  sdxHyperlinkExecuteError = '"%s" cannot be opened.';

  // Clipboard
  sdxClipboardFormatHTML = 'HTML Format';
  sdxClipboardFormatImage = 'Picture';
  sdxClipboardFormatText = 'Text';

  // Printing
  sdxSetSingleCellAsPrintAreaConfirmation =
    'You have selected a single cell for print area.' + #13#10#13#10 +
    'If this is correct, click OK.' + #13#10 +
    'If you selected a single cell by mistake, click Cancel, select the cells you want to include, and then click "Set Print Area" again';
  sdxCell = 'Cell: ';
  sdxComment = 'Comment: ';

  // FormulaBar
  sdxFormulaBarCancelHint = 'Cancel';
  sdxFormulaBarEnterHint = 'Enter';
  sdxFormulaBarFormulaBarHint = 'Formula Bar';
  sdxFormulaBarInsertFunctionHint = 'Insert Function';
  sdxFormulaBarNameBoxHint = 'Name Box';
  sdxFormulaBarSelectionInfo = '%dR x %dC';

implementation

procedure AddSpreadSheetResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxBuiltInPopupMenuBringToFront', @sdxBuiltInPopupMenuBringToFront);
  AProduct.Add('sdxBuiltInPopupMenuClearContents', @sdxBuiltInPopupMenuClearContents);
  AProduct.Add('sdxBuiltInPopupMenuCopy', @sdxBuiltInPopupMenuCopy);
  AProduct.Add('sdxBuiltInPopupMenuCustomizeObject', @sdxBuiltInPopupMenuCustomizeObject);
  AProduct.Add('sdxBuiltInPopupMenuCut', @sdxBuiltInPopupMenuCut);
  AProduct.Add('sdxBuiltInPopupMenuDelete', @sdxBuiltInPopupMenuDelete);
  AProduct.Add('sdxBuiltInPopupMenuDeleteDialog', @sdxBuiltInPopupMenuDeleteDialog);
  AProduct.Add('sdxBuiltInPopupMenuFormatCells', @sdxBuiltInPopupMenuFormatCells);
  AProduct.Add('sdxBuiltInPopupMenuHide', @sdxBuiltInPopupMenuHide);
  AProduct.Add('sdxBuiltInPopupMenuInsert', @sdxBuiltInPopupMenuInsert);
  AProduct.Add('sdxBuiltInPopupMenuInsertDialog', @sdxBuiltInPopupMenuInsertDialog);
  AProduct.Add('sdxBuiltInPopupMenuMergeCells', @sdxBuiltInPopupMenuMergeCells);
  AProduct.Add('sdxBuiltInPopupMenuPaste', @sdxBuiltInPopupMenuPaste);
  AProduct.Add('sdxBuiltInPopupMenuPasteSpecial', @sdxBuiltInPopupMenuPasteSpecial);
  AProduct.Add('sdxBuiltInPopupMenuPasteSpecialAll', @sdxBuiltInPopupMenuPasteSpecialAll);
  AProduct.Add('sdxBuiltInPopupMenuPasteSpecialFormulas', @sdxBuiltInPopupMenuPasteSpecialFormulas);
  AProduct.Add('sdxBuiltInPopupMenuPasteSpecialFormulasAndColumnWidths', @sdxBuiltInPopupMenuPasteSpecialFormulasAndColumnWidths);
  AProduct.Add('sdxBuiltInPopupMenuPasteSpecialFormulasAndFormatting', @sdxBuiltInPopupMenuPasteSpecialFormulasAndFormatting);
  AProduct.Add('sdxBuiltInPopupMenuPasteSpecialFormulasAndStyles', @sdxBuiltInPopupMenuPasteSpecialFormulasAndStyles);
  AProduct.Add('sdxBuiltInPopupMenuPasteSpecialShowDialog', @sdxBuiltInPopupMenuPasteSpecialShowDialog);
  AProduct.Add('sdxBuiltInPopupMenuPasteSpecialValues', @sdxBuiltInPopupMenuPasteSpecialValues);
  AProduct.Add('sdxBuiltInPopupMenuPasteSpecialValuesAndFormatting', @sdxBuiltInPopupMenuPasteSpecialValuesAndFormatting);
  AProduct.Add('sdxBuiltInPopupMenuPasteSpecialValuesAndStyles', @sdxBuiltInPopupMenuPasteSpecialValuesAndStyles);

  AProduct.Add('sdxBuiltInPopupMenuRename', @sdxBuiltInPopupMenuRename);
  AProduct.Add('sdxBuiltInPopupMenuSendToBack', @sdxBuiltInPopupMenuSendToBack);
  AProduct.Add('sdxBuiltInPopupMenuSplitCells', @sdxBuiltInPopupMenuSplitCells);
  AProduct.Add('sdxBuiltInPopupMenuUnhide', @sdxBuiltInPopupMenuUnhide);
  AProduct.Add('sdxBuiltInPopupMenuUnhideDialog', @sdxBuiltInPopupMenuUnhideDialog);
  AProduct.Add('sdxBuiltInPopupMenuCreateHyperlink', @sdxBuiltInPopupMenuCreateHyperlink);
  AProduct.Add('sdxBuiltInPopupMenuEditHyperlink', @sdxBuiltInPopupMenuEditHyperlink);
  AProduct.Add('sdxBuiltInPopupMenuOpenHyperlink', @sdxBuiltInPopupMenuOpenHyperlink);
  AProduct.Add('sdxBuiltInPopupMenuRemoveHyperlink', @sdxBuiltInPopupMenuRemoveHyperlink);
  AProduct.Add('sdxBuiltInPopupMenuInsertComment', @sdxBuiltInPopupMenuInsertComment);
  AProduct.Add('sdxBuiltInPopupMenuDeleteComment', @sdxBuiltInPopupMenuDeleteComment);
  AProduct.Add('sdxBuiltInPopupMenuEditComment', @sdxBuiltInPopupMenuEditComment);
  AProduct.Add('sdxBuiltInPopupMenuHideComment', @sdxBuiltInPopupMenuHideComment);
  AProduct.Add('sdxBuiltInPopupMenuShowComment', @sdxBuiltInPopupMenuShowComment);
  AProduct.Add('sdxBuiltInPopupMenuProtectSheet', @sdxBuiltInPopupMenuProtectSheet);
  AProduct.Add('sdxBuiltInPopupMenuUnprotectSheet', @sdxBuiltInPopupMenuUnprotectSheet);

  AProduct.Add('sdxRenameDialogCaption', @sdxRenameDialogCaption);
  AProduct.Add('sdxRenameDialogSheetName', @sdxRenameDialogSheetName);

  AProduct.Add('sdxFileDialogAllSupported', @sdxFileDialogAllSupported);

  AProduct.Add('sdxActionAutoFill', @sdxActionAutoFill);
  AProduct.Add('sdxActionAddGroup', @sdxActionAddGroup);
  AProduct.Add('sdxActionCellEditing', @sdxActionCellEditing);
  AProduct.Add('sdxActionCellsMerge', @sdxActionCellsMerge);
  AProduct.Add('sdxActionChangeConditionalFormatting', @sdxActionChangeConditionalFormatting);
  AProduct.Add('sdxActionChangePrintingOptions', @sdxActionChangePrintingOptions);
  AProduct.Add('sdxActionCreateDefinedName', @sdxActionCreateDefinedName);
  AProduct.Add('sdxActionChangeContainer', @sdxActionChangeContainer);
  AProduct.Add('sdxActionChangeGroup', @sdxActionChangeGroup);
  AProduct.Add('sdxActionChangeHyperlink', @sdxActionChangeHyperlink);
  AProduct.Add('sdxActionChangeRowColumn', @sdxActionChangeRowColumn);
  AProduct.Add('sdxActionClearCells', @sdxActionClearCells);
  AProduct.Add('sdxActionCutCells', @sdxActionCutCells);
  AProduct.Add('sdxActionDeleteCells', @sdxActionDeleteCells);
  AProduct.Add('sdxActionDeleteComment', @sdxActionDeleteComment);
  AProduct.Add('sdxActionDeleteGroup', @sdxActionDeleteGroup);
  AProduct.Add('sdxActionDragAndDrop', @sdxActionDragAndDrop);
  AProduct.Add('sdxActionMoveCells', @sdxActionMoveCells);
  AProduct.Add('sdxActionEditComment', @sdxActionEditComment);
  AProduct.Add('sdxActionExpandCollapseGroup', @sdxActionExpandCollapseGroup);
  AProduct.Add('sdxActionFillCells', @sdxActionFillCells);
  AProduct.Add('sdxActionFormatCells', @sdxActionFormatCells);
  AProduct.Add('sdxActionInsertCells', @sdxActionInsertCells);
  AProduct.Add('sdxActionPasteCells', @sdxActionPasteCells);
  AProduct.Add('sdxActionReplace', @sdxActionReplace);
  AProduct.Add('sdxActionSortCells', @sdxActionSortCells);

  AProduct.Add('sdxDefaultHyperlinkScreenTip', @sdxDefaultHyperlinkScreenTip);
  AProduct.Add('sdxDefaultHyperlinkShortScreenTip', @sdxDefaultHyperlinkShortScreenTip);
  AProduct.Add('scxSelectionInDocument', @scxSelectionInDocument);
  AProduct.Add('sdxHyperlinkExecuteError', @sdxHyperlinkExecuteError);

  AProduct.Add('sdxClipboardFormatImage', @sdxClipboardFormatImage);
  AProduct.Add('sdxClipboardFormatHTML', @sdxClipboardFormatHTML);
  AProduct.Add('sdxClipboardFormatText', @sdxClipboardFormatText);

  AProduct.Add('sdxSetSingleCellAsPrintAreaConfirmation', @sdxSetSingleCellAsPrintAreaConfirmation);
  AProduct.Add('sdxComment', @sdxComment);
  AProduct.Add('sdxCell', @sdxCell);

  AProduct.Add('sdxFormulaBarCancelHint', @sdxFormulaBarCancelHint);
  AProduct.Add('sdxFormulaBarEnterHint', @sdxFormulaBarEnterHint);
  AProduct.Add('sdxFormulaBarFormulaBarHint', @sdxFormulaBarFormulaBarHint);
  AProduct.Add('sdxFormulaBarInsertFunctionHint', @sdxFormulaBarInsertFunctionHint);
  AProduct.Add('sdxFormulaBarNameBoxHint', @sdxFormulaBarNameBoxHint);
  AProduct.Add('sdxFormulaBarSelectionInfo', @sdxFormulaBarSelectionInfo);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressSpreadSheet 2', @AddSpreadSheetResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressSpreadSheet 2', @AddSpreadSheetResourceStringNames);
end.
