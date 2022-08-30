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

unit dxSpreadSheetDialogStrs;

{$I cxVer.Inc}

interface

uses
  Classes, dxCore, dxGDIPlusClasses, dxSpreadSheetPrinting;

const
  sdxErrorIndicationBlank = '';
  sdxErrorIndicationDash = '--';
  sdxErrorIndicationNA = '#N/A';

resourcestring
  // Cells Shifting
  sdxShiftCellsDown = 'Shift cells &down';
  sdxShiftCellsLeft = 'Shift cells &left';
  sdxShiftCellsRight = 'Shift cells r&ight';
  sdxShiftCellsUp = 'Shift cells &up';
  sdxShiftColumn = 'Entire &column';
  sdxShiftRow = 'Entire &row';

  sdxGradientModeBackwardDiagonal = 'Backward Diagonal';
  sdxGradientModeForwardDiagonal = 'Forward Diagonal';
  sdxGradientModeHorizontal = 'Horizontal';
  sdxGradientModeVertical = 'Vertical';

  sdxPenStyleDash = 'Dash';
  sdxPenStyleDashDot = 'Dash Dot';
  sdxPenStyleDashDotDot = 'Dash Dot Dot';
  sdxPenStyleDot = 'Dot';
  sdxPenStyleSolid = 'Solid';

  // Cells Modification Dialog
  sdxCellsModificationDialogInsertCaption = 'Insert';
  sdxCellsModificationDialogDeleteCaption = 'Delete';
  sdxCellsModificationDialogButtonCancel = 'Cancel';
  sdxCellsModificationDialogButtonOK = 'OK';

  // Container Customization Dialog
  sdxContainerCustomizationDialogCaption = 'Customize Object';
  sdxContainerCustomizationDialogButtonAdd = '&Add';
  sdxContainerCustomizationDialogButtonCancel = 'Cancel';
  sdxContainerCustomizationDialogButtonColor = '&Color';
  sdxContainerCustomizationDialogButtonLoad = '&Load';
  sdxContainerCustomizationDialogButtonOK = 'OK';
  sdxContainerCustomizationDialogButtonRemove = 'Remo&ve';
  sdxContainerCustomizationDialogButtonSave = '&Save';
  sdxContainerCustomizationDialogReset = 'Re&set';

  // Container Customization Dialog - Fill
  sdxContainerCustomizationDialogDirection = '&Direction:';
  sdxContainerCustomizationDialogGradientFill = '&Gradient fill';
  sdxContainerCustomizationDialogGroupFill = 'Fill';
  sdxContainerCustomizationDialogNoFill = '&No fill';
  sdxContainerCustomizationDialogSolidFill = '&Solid fill';
  sdxContainerCustomizationDialogStops = 'Stops:';
  sdxContainerCustomizationDialogTextureFill = '&Texture fill';

  // Container Customization Dialog - Line
  sdxContainerCustomizationDialogGradientLine = '&Gradient line';
  sdxContainerCustomizationDialogLine = 'Line';
  sdxContainerCustomizationDialogLineStyle = '&Style:';
  sdxContainerCustomizationDialogLineWidth = '&Width:';
  sdxContainerCustomizationDialogNoLine = '&No line';
  sdxContainerCustomizationDialogSolidLine = '&Solid line';

  // Container Customization Dialog - Properties
  sdxContainerCustomizationDialogGroupProperties = 'Properties';
  sdxContainerCustomizationDialogPositioning = 'Positioning';
  sdxContainerCustomizationDialogAbsolute = '&Don''t move or size with cells';
  sdxContainerCustomizationDialogOneCells = '&Move but don''t size with cells';
  sdxContainerCustomizationDialogTwoCells = 'Move and &size with cells';

  // Container Customization Dialog - Size
  sdxContainerCustomizationDialogCropBottom = 'Botto&m:';
  sdxContainerCustomizationDialogCropFrom = 'Crop from';
  sdxContainerCustomizationDialogCropLeft = '&Left:';
  sdxContainerCustomizationDialogCropRight = 'Ri&ght:';
  sdxContainerCustomizationDialogCropTop = 'To&p:';
  sdxContainerCustomizationDialogGroupSize = 'Size';
  sdxContainerCustomizationDialogHeight = 'H&eight:';
  sdxContainerCustomizationDialogLockAspectRatio = 'Lock &aspect ratio';
  sdxContainerCustomizationDialogOriginalSize = 'Original size';
  sdxContainerCustomizationDialogOriginalSizeFormatString = 'Height: %d, Width: %d';
  sdxContainerCustomizationDialogRelativeToPictureSize = '&Relative to original picture size';
  sdxContainerCustomizationDialogRotation = 'Ro&tation:';
  sdxContainerCustomizationDialogScale = 'Scale';
  sdxContainerCustomizationDialogScaleHeight = '&Height:';
  sdxContainerCustomizationDialogScaleWidth = '&Width:';
  sdxContainerCustomizationDialogSize = 'Size';
  sdxContainerCustomizationDialogSizeAndRotate = 'Size and rotate';
  sdxContainerCustomizationDialogWidth = 'Wi&dth:';

  // Container Customization Dialog - Text
  sdxContainerCustomizationDialogTextButtonFont = '&Font';
  sdxContainerCustomizationDialogTextCaption = 'Text';

  // Container Customization Dialog - Text Box
  sdxContainerCustomizationDialogTextBoxCaption = 'Text Box';
  sdxContainerCustomizationDialogTextBoxAlignment = 'Alignment';
  sdxContainerCustomizationDialogTextBoxAlignmentBottom = 'Bottom';
  sdxContainerCustomizationDialogTextBoxAlignmentCenter = 'Center';
  sdxContainerCustomizationDialogTextBoxAlignmentLeft = 'Left';
  sdxContainerCustomizationDialogTextBoxAlignmentRight = 'Right';
  sdxContainerCustomizationDialogTextBoxAlignmentTop = 'Top';
  sdxContainerCustomizationDialogTextBoxHorizontal = '&Horizontal:';
  sdxContainerCustomizationDialogTextBoxVertical = '&Vertical:';
  sdxContainerCustomizationDialogTextBoxPadding = 'Padding';
  sdxContainerCustomizationDialogTextBoxPaddingLeft = '&Left:';
  sdxContainerCustomizationDialogTextBoxPaddingRight = 'Ri&ght:';
  sdxContainerCustomizationDialogTextBoxPaddingTop = 'To&p:';
  sdxContainerCustomizationDialogTextBoxPaddingBottom = 'Botto&m:';
  sdxContainerCustomizationDialogTextBoxAutoSize = 'Resize shape to &fit text';
  sdxContainerCustomizationDialogTextBoxWordWrap = '&Wrap text in shape';

  // Find And Replace Dialog
  sdxFindAndReplaceDialogButtonClose = 'Close';
  sdxFindAndReplaceDialogButtonFindAll = 'F&ind all';
  sdxFindAndReplaceDialogButtonFindNext = '&Find next';
  sdxFindAndReplaceDialogButtonReplace = '&Replace';
  sdxFindAndReplaceDialogButtonReplaceAll = 'Replace &All';
  sdxFindAndReplaceDialogCaption = 'Find and Replace';
  sdxFindAndReplaceDialogColumnCellCaption = 'Cell';
  sdxFindAndReplaceDialogColumnFormulaCaption = 'Formula';
  sdxFindAndReplaceDialogColumnNameCaption = 'Name';
  sdxFindAndReplaceDialogColumnSheetCaption = 'Sheet';
  sdxFindAndReplaceDialogColumnValueCaption = 'Value';
  sdxFindAndReplaceDialogFindMatchNotFound = 'A match cannot be found. Click Options to customize your search criteria.';
  sdxFindAndReplaceDialogFindTabCaption = 'Find';
  sdxFindAndReplaceDialogFindWhat = 'Fi&nd what:';
  sdxFindAndReplaceDialogLessOptions = 'Op&tions <<';
  sdxFindAndReplaceDialogLookIn = '&Look in:';
  sdxFindAndReplaceDialogLookInFormulas = 'Formulas';
  sdxFindAndReplaceDialogLookInValues = 'Values';
  sdxFindAndReplaceDialogMatchCase = 'Match &case';
  sdxFindAndReplaceDialogMatchEntireCell = 'Match entire cell c&ontents';
  sdxFindAndReplaceDialogMoreOptions = 'Op&tions >>';
  sdxFindAndReplaceDialogReplaceMatchNotFound = 'A match for replacement cannot be found. Click Options to customize your search criteria.';
  sdxFindAndReplaceDialogReplacementResult = 'All done. %d replacement(s) were made.';
  sdxFindAndReplaceDialogReplaceTabCaption = 'Replace';
  sdxFindAndReplaceDialogReplaceWith = 'R&eplace with:';
  sdxFindAndReplaceDialogSearchMode = '&Search:';
  sdxFindAndReplaceDialogSearchModeByColumns = 'By Columns';
  sdxFindAndReplaceDialogSearchModeByRows = 'By Rows';
  sdxFindAndReplaceDialogStatusBarCellsFound = ' %d cell(s) found';
  sdxFindAndReplaceDialogWithinRange = 'Wit&hin:';
  sdxFindAndReplaceDialogWithinRangeSheet = 'Sheet';
  sdxFindAndReplaceDialogWithinRangeSpreadsheet = 'Spreadsheet';

  // Unhide sheet dialog
  sdxUnhideSheetDialogCaption = 'Unhide';
  sdxUnhideSheetDialogHiddenSheets = '&Unhide sheet:';

  // Password Dialog
  sdxPasswordDialogCaption = 'Password Protection';
  sdxPasswordDialogCaptionConfirm = 'Confirm Password';
  sdxPasswordDialogPassword = '&Enter password:';
  sdxPasswordDialogPasswordConfirmation = 'Reenter &password to proceed:';
  sdxPasswordDialogPasswordNotMatch = 'The password confirmation doesn''t match.';
  sdxPasswordDialogPasswordNotes = 'Caution: If you lose or forget the password, it cannot be recovered. It''s recommended that you keep it in a safe place.';
  sdxPasswordDialogButtonCancel = 'Cancel';
  sdxPasswordDialogButtonOK = 'OK';

  // Paste Special Dialog
  sdxPasteSpecialDialogCaption = 'Paste Special';
  sdxPasteSpecialDialogButtonCancel = 'Cancel';
  sdxPasteSpecialDialogButtonOK = 'OK';
  sdxPasteSpecialDialogPasteFormat = 'Paste &As:';
  sdxPasteSpecialDialogPasteOptions = 'Paste';
  sdxPasteSpecialDialogPasteValues = '&Values';
  sdxPasteSpecialDialogPasteFormulas = '&Formulas';
  sdxPasteSpecialDialogPasteComments = 'Co&mments';
  sdxPasteSpecialDialogPasteStyles = '&Styles';
  sdxPasteSpecialDialogPasteStylesAll = '&All';
  sdxPasteSpecialDialogPasteStylesNumberFormatting = '&Number Formatting';
  sdxPasteSpecialDialogPasteColumnWidths = 'Column &Widths';
  sdxPasteSpecialDialogPasteSkinBlanks = '&Skip Blanks';

  // Protect Sheet Dialog
  sdxProtectSheetDialogCaption = 'Protect Sheet';
  sdxProtectSheetDialogButtonCancel = 'Cancel';
  sdxProtectSheetDialogButtonOK = 'OK';
  sdxProtectSheetDialogPassword = '&Password to unprotect sheet:';
  sdxProtectSheetDialogPermissions = 'All&ow all users of this worksheet to:';
  sdxProtectSheetDialogAllowDeleteColumns = 'Delete columns';
  sdxProtectSheetDialogAllowDeleteRows = 'Delete rows';
  sdxProtectSheetDialogAllowEditContainers = 'Edit containers';
  sdxProtectSheetDialogAllowEditHyperlinks = 'Edit hyperlinks';
  sdxProtectSheetDialogAllowFormatCells = 'Format cells';
  sdxProtectSheetDialogAllowInsertColumns = 'Insert columns';
  sdxProtectSheetDialogAllowInsertRows = 'Insert rows';
  sdxProtectSheetDialogAllowResizeColumns = 'Resize columns';
  sdxProtectSheetDialogAllowResizeRows = 'Resize rows';
  sdxProtectSheetDialogAllowSelectLockedCells = 'Select locked cells';
  sdxProtectSheetDialogAllowSelectUnlockedCells = 'Select unlocked cells';
  sdxProtectSheetDialogAllowSort = 'Sort';
  sdxProtectSheetDialogProtect = 'Protect worksheet and &contents of locked cells';

  // Protect Workbook Dialog
  sdxProtectWorkbookDialogCaption = 'Protect Structure';
  sdxProtectWorkbookDialogButtonOK = 'OK';
  sdxProtectWorkbookDialogButtonCancel = 'Cancel';
  sdxProtectWorkbookDialogProtectionOptions = 'Protect workbook for';
  sdxProtectWorkbookDialogProtectStructure = '&Structure';
  sdxProtectWorkbookDialogPassword = '&Password (optional):';

  // Hyperlink editor
  sdxHyperlinkEditorAddress = 'Address:';
  sdxHyperlinkEditorCancel = 'Cancel';
  sdxHyperlinkEditorCellReference = 'Type the cell reference:';
  sdxHyperlinkEditorCellReferenceNode = 'Cell Reference';
  sdxHyperlinkEditorDefinedNamesNode  = 'Defined Names';
  sdxHyperlinkEditorEditCaption = 'Edit Hyperlink';
  sdxHyperlinkEditorEmailAddress = 'E-mail address:';
  sdxHyperlinkEditorEmailAddressLink = 'E-mail Address';
  sdxHyperlinkEditorFileOrWebPageLink = 'Existing File or'#13#10'Web Page';
  sdxHyperlinkEditorInsertCaption = 'Insert Hyperlink';
  sdxHyperlinkEditorLinkTo = 'Link to:';
  sdxHyperlinkEditorOK = 'OK';
  sdxHyperlinkEditorPlaceInThisDocumentLink = 'Place in This'#13#10'Document';
  sdxHyperlinkEditorRemoveLink = 'Remove link';
  sdxHyperlinkEditorScreenTip = 'ScreenTip:';
  sdxHyperlinkEditorSelectPlace = 'Or select a place in this document:';
  sdxHyperlinkEditorSubject = 'Subject:';
  sdxHyperlinkEditorTextToDisplay = 'Text to display:';

  // Page Setup
  sdxPageSetupDialogAlignWithMargins = 'Align with page &margins';
  sdxPageSetupDialogBlackAndWhite = '&Black and white';
  sdxPageSetupDialogButtonCancel = 'Cancel';
  sdxPageSetupDialogButtonCustomHeaderFooter = 'Custom Header/Footer';
  sdxPageSetupDialogButtonOK = 'OK';
  sdxPageSetupDialogButtonPrint = '&Print...';
  sdxPageSetupDialogButtonPrintPreview = 'Print Previe&w...';
  sdxPageSetupDialogCaption = 'Page Setup';
  sdxPageSetupDialogCenterHorizontally = 'Hori&zontally';
  sdxPageSetupDialogCenterOnPage = 'Center on page';
  sdxPageSetupDialogCenterVertically = '&Vertically';
  sdxPageSetupDialogDownThenOver = '&Down, then over';
  sdxPageSetupDialogFooter = 'Footer:';
  sdxPageSetupDialogHeader = 'Header:';
  sdxPageSetupDialogHeaderFooter = 'Header/Footer';
  sdxPageSetupDialogMarginBottom = '&Bottom:';
  sdxPageSetupDialogMarginFooter = '&Footer:';
  sdxPageSetupDialogMarginHeader = '&Header:';
  sdxPageSetupDialogMarginLeft = '&Left:';
  sdxPageSetupDialogMarginRight = '&Right:';
  sdxPageSetupDialogMargins = 'Margins';
  sdxPageSetupDialogMarginTop = '&Top:';
  sdxPageSetupDialogOverThenDown = 'O&ver, then down';
  sdxPageSetupDialogPage = 'Page';
  sdxPageSetupDialogPageFirstPageNumber = 'Fi&rst page number:';
  sdxPageSetupDialogPageOrder = 'Page order';
  sdxPageSetupDialogPageOrientation = 'Orientation';
  sdxPageSetupDialogPageOrientationLandscape = '&Landscape';
  sdxPageSetupDialogPageOrientationPortrait = 'Por&trait';
  sdxPageSetupDialogPaperSize = 'Paper si&ze:';
  sdxPageSetupDialogPrint = 'Print';
  sdxPageSetupDialogPrintArea = 'Print &area:';
  sdxPageSetupDialogPrintCellErrorsMode = 'Cell &errors as:';
  sdxPageSetupDialogPrintCellErrorsModeBlank = '<blank>';
  sdxPageSetupDialogPrintCellErrorsModeDash = '--';
  sdxPageSetupDialogPrintCellErrorsModeDisplayed = 'displayed';
  sdxPageSetupDialogPrintCellErrorsModeNA = '#N/A';
  sdxPageSetupDialogPrintCommentsMode = 'Co&mments:';
  sdxPageSetupDialogPrintCommentsModeAsDisplayed = 'as displayed on sheet';
  sdxPageSetupDialogPrintCommentsModeAtEnd = 'at end of sheet';
  sdxPageSetupDialogPrintCommentsModeNode = '(none)';
  sdxPageSetupDialogPrintDraftQuality = 'Draft &Quality';
  sdxPageSetupDialogPrintGridlines = '&Gridlines';
  sdxPageSetupDialogPrintRowAndColumnHeadings = 'Row and co&lumn headings';
  sdxPageSetupDialogPrintTitles = 'Print titles';
  sdxPageSetupDialogPrintTitlesColumnsToRepeat = '&Columns to repeat at left:';
  sdxPageSetupDialogPrintTitlesRowsToRepeat = '&Rows to repeat at top:';
  sdxPageSetupDialogScaleWithDocument = 'Sca&le with document';
  sdxPageSetupDialogScaling = 'Scaling';
  sdxPageSetupDialogScalingAdjustTo = '&Adjust to:';
  sdxPageSetupDialogScalingAdjustToSuffix = '% normal size';
  sdxPageSetupDialogScalingFitTo = '&Fit to:';
  sdxPageSetupDialogScalingFitToPageTall = 'tall';
  sdxPageSetupDialogScalingFitToPageWide = 'page(s) wide by';
  sdxPageSetupDialogTabSheetCaption = 'Sheet';
  sdxPageSetupDialogUnitsInches = 'in';
  sdxPageSetupDialogUnitsMillimeters = 'mm';

  // Insert Function Dialog
  sdxInsertFunctionDialogCaption = 'Insert Function';
  sdxInsertFunctionDialogCategory = 'Category:';
  sdxInsertFunctionDialogCategoryAll = 'All';
  sdxInsertFunctionDialogFunctions = 'Select a function:';

  sdxPageSetupHeaderFooterDialogCaption = 'Header/Footer';
  sdxPageSetupHeaderFooterDialogDescription =
    'To insert a page number, date, time or tab name: position the insertion' +
    ' point in the edit box, then choose the appropriate button.';
  sdxPageSetupHeaderFooterDialogLeftHeader = 'Left header:';
  sdxPageSetupHeaderFooterDialogLeftFooter = 'Left footer:';
  sdxPageSetupHeaderFooterDialogCenterHeader = 'Center header:';
  sdxPageSetupHeaderFooterDialogCenterFooter = 'Center footer:';
  sdxPageSetupHeaderFooterDialogRightHeader = 'Right header:';
  sdxPageSetupHeaderFooterDialogRightFooter = 'Right footer:';
  sdxPageSetupHeaderFooterDialogButtonCancel = 'Cancel';
  sdxPageSetupHeaderFooterDialogButtonOK = 'OK';
  sdxPageSetupHeaderFooterDialogHintInsertDate = 'Insert Date';
  sdxPageSetupHeaderFooterDialogHintInsertPageNumber = 'Insert Page Number';
  sdxPageSetupHeaderFooterDialogHintInsertPageTotal = 'Insert Page Total';
  sdxPageSetupHeaderFooterDialogHintInsertSheetName = 'Insert Sheet Name';
  sdxPageSetupHeaderFooterDialogHintInsertTime = 'Insert Time';

const
  dxGradientModeNames: array[TdxGPBrushGradientMode] of Pointer = (
    @sdxGradientModeHorizontal, @sdxGradientModeVertical, @sdxGradientModeForwardDiagonal, @sdxGradientModeBackwardDiagonal
  );
  dxPenStyleNames: array[TdxGPPenStyle] of Pointer = (
    @sdxPenStyleSolid, @sdxPenStyleDash, @sdxPenStyleDot, @sdxPenStyleDashDot, @sdxPenStyleDashDotDot
  );
  dxPrintCellErrorsModesNames: array[TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication] of Pointer = (
    nil,
    @sdxPageSetupDialogPrintCellErrorsModeBlank,
    @sdxPageSetupDialogPrintCellErrorsModeDash,
    @sdxPageSetupDialogPrintCellErrorsModeDisplayed,
    @sdxPageSetupDialogPrintCellErrorsModeNA
  );
  dxPrintCommentsModesNames: array[TdxSpreadSheetTableViewOptionsPrintSourceCellComments] of Pointer = (
    @sdxPageSetupDialogPrintCommentsModeAsDisplayed,
    @sdxPageSetupDialogPrintCommentsModeAtEnd,
    @sdxPageSetupDialogPrintCommentsModeNode
  );
implementation

procedure AddSpreadSheetDialogsResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxPenStyleDash', @sdxPenStyleDash);
  AProduct.Add('sdxPenStyleDashDot', @sdxPenStyleDashDot);
  AProduct.Add('sdxPenStyleDashDotDot', @sdxPenStyleDashDotDot);
  AProduct.Add('sdxPenStyleDot', @sdxPenStyleDot);
  AProduct.Add('sdxPenStyleSolid', @sdxPenStyleSolid);

  AProduct.Add('sdxCellsModificationDialogButtonOK', @sdxCellsModificationDialogButtonOK);
  AProduct.Add('sdxCellsModificationDialogButtonCancel', @sdxCellsModificationDialogButtonCancel);
  AProduct.Add('sdxCellsModificationDialogInsertCaption', @sdxCellsModificationDialogInsertCaption);
  AProduct.Add('sdxCellsModificationDialogDeleteCaption', @sdxCellsModificationDialogDeleteCaption);
  AProduct.Add('sdxShiftCellsDown', @sdxShiftCellsDown);
  AProduct.Add('sdxShiftCellsLeft', @sdxShiftCellsLeft);
  AProduct.Add('sdxShiftCellsRight', @sdxShiftCellsRight);
  AProduct.Add('sdxShiftCellsUp', @sdxShiftCellsUp);
  AProduct.Add('sdxShiftColumn', @sdxShiftColumn);
  AProduct.Add('sdxShiftRow', @sdxShiftRow);

  AProduct.Add('sdxContainerCustomizationDialogCaption', @sdxContainerCustomizationDialogCaption);
  AProduct.Add('sdxContainerCustomizationDialogButtonAdd', @sdxContainerCustomizationDialogButtonAdd);
  AProduct.Add('sdxContainerCustomizationDialogButtonCancel', @sdxContainerCustomizationDialogButtonCancel);
  AProduct.Add('sdxContainerCustomizationDialogButtonColor', @sdxContainerCustomizationDialogButtonColor);
  AProduct.Add('sdxContainerCustomizationDialogButtonLoad', @sdxContainerCustomizationDialogButtonLoad);
  AProduct.Add('sdxContainerCustomizationDialogButtonOK', @sdxContainerCustomizationDialogButtonOK);
  AProduct.Add('sdxContainerCustomizationDialogButtonRemove', @sdxContainerCustomizationDialogButtonRemove);
  AProduct.Add('sdxContainerCustomizationDialogButtonSave', @sdxContainerCustomizationDialogButtonSave);
  AProduct.Add('sdxContainerCustomizationDialogReset', @sdxContainerCustomizationDialogReset);
  AProduct.Add('sdxContainerCustomizationDialogDirection', @sdxContainerCustomizationDialogDirection);
  AProduct.Add('sdxContainerCustomizationDialogGradientFill', @sdxContainerCustomizationDialogGradientFill);
  AProduct.Add('sdxContainerCustomizationDialogGroupFill', @sdxContainerCustomizationDialogGroupFill);
  AProduct.Add('sdxContainerCustomizationDialogNoFill', @sdxContainerCustomizationDialogNoFill);
  AProduct.Add('sdxContainerCustomizationDialogSolidFill', @sdxContainerCustomizationDialogSolidFill);
  AProduct.Add('sdxContainerCustomizationDialogStops', @sdxContainerCustomizationDialogStops);
  AProduct.Add('sdxContainerCustomizationDialogTextureFill', @sdxContainerCustomizationDialogTextureFill);
  AProduct.Add('sdxContainerCustomizationDialogGradientLine', @sdxContainerCustomizationDialogGradientLine);
  AProduct.Add('sdxContainerCustomizationDialogLine', @sdxContainerCustomizationDialogLine);
  AProduct.Add('sdxContainerCustomizationDialogLineStyle', @sdxContainerCustomizationDialogLineStyle);
  AProduct.Add('sdxContainerCustomizationDialogLineWidth', @sdxContainerCustomizationDialogLineWidth);
  AProduct.Add('sdxContainerCustomizationDialogNoLine', @sdxContainerCustomizationDialogNoLine);
  AProduct.Add('sdxContainerCustomizationDialogSolidLine', @sdxContainerCustomizationDialogSolidLine);
  AProduct.Add('sdxContainerCustomizationDialogGroupProperties', @sdxContainerCustomizationDialogGroupProperties);
  AProduct.Add('sdxContainerCustomizationDialogPositioning', @sdxContainerCustomizationDialogPositioning);
  AProduct.Add('sdxContainerCustomizationDialogAbsolute', @sdxContainerCustomizationDialogAbsolute);
  AProduct.Add('sdxContainerCustomizationDialogOneCells', @sdxContainerCustomizationDialogOneCells);
  AProduct.Add('sdxContainerCustomizationDialogTwoCells', @sdxContainerCustomizationDialogTwoCells);
  AProduct.Add('sdxContainerCustomizationDialogCropBottom', @sdxContainerCustomizationDialogCropBottom);
  AProduct.Add('sdxContainerCustomizationDialogCropFrom', @sdxContainerCustomizationDialogCropFrom);
  AProduct.Add('sdxContainerCustomizationDialogCropLeft', @sdxContainerCustomizationDialogCropLeft);
  AProduct.Add('sdxContainerCustomizationDialogCropRight', @sdxContainerCustomizationDialogCropRight);
  AProduct.Add('sdxContainerCustomizationDialogCropTop', @sdxContainerCustomizationDialogCropTop);
  AProduct.Add('sdxContainerCustomizationDialogGroupSize', @sdxContainerCustomizationDialogGroupSize);
  AProduct.Add('sdxContainerCustomizationDialogHeight', @sdxContainerCustomizationDialogHeight);
  AProduct.Add('sdxContainerCustomizationDialogLockAspectRatio', @sdxContainerCustomizationDialogLockAspectRatio);
  AProduct.Add('sdxContainerCustomizationDialogOriginalSize', @sdxContainerCustomizationDialogOriginalSize);
  AProduct.Add('sdxContainerCustomizationDialogOriginalSizeFormatString', @sdxContainerCustomizationDialogOriginalSizeFormatString);
  AProduct.Add('sdxContainerCustomizationDialogRelativeToPictureSize', @sdxContainerCustomizationDialogRelativeToPictureSize);
  AProduct.Add('sdxContainerCustomizationDialogRotation', @sdxContainerCustomizationDialogRotation);
  AProduct.Add('sdxContainerCustomizationDialogScale', @sdxContainerCustomizationDialogScale);
  AProduct.Add('sdxContainerCustomizationDialogScaleHeight', @sdxContainerCustomizationDialogScaleHeight);
  AProduct.Add('sdxContainerCustomizationDialogScaleWidth', @sdxContainerCustomizationDialogScaleWidth);
  AProduct.Add('sdxContainerCustomizationDialogSize', @sdxContainerCustomizationDialogSize);
  AProduct.Add('sdxContainerCustomizationDialogSizeAndRotate', @sdxContainerCustomizationDialogSizeAndRotate);
  AProduct.Add('sdxContainerCustomizationDialogWidth', @sdxContainerCustomizationDialogWidth);

  AProduct.Add('sdxContainerCustomizationDialogTextButtonFont', @sdxContainerCustomizationDialogTextButtonFont);
  AProduct.Add('sdxContainerCustomizationDialogTextCaption', @sdxContainerCustomizationDialogTextCaption);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxCaption', @sdxContainerCustomizationDialogTextBoxCaption);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxAlignment', @sdxContainerCustomizationDialogTextBoxAlignment);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxAlignmentBottom', @sdxContainerCustomizationDialogTextBoxAlignmentBottom);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxAlignmentCenter', @sdxContainerCustomizationDialogTextBoxAlignmentCenter);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxAlignmentLeft', @sdxContainerCustomizationDialogTextBoxAlignmentLeft);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxAlignmentRight', @sdxContainerCustomizationDialogTextBoxAlignmentRight);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxAlignmentTop', @sdxContainerCustomizationDialogTextBoxAlignmentTop);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxHorizontal', @sdxContainerCustomizationDialogTextBoxHorizontal);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxVertical', @sdxContainerCustomizationDialogTextBoxVertical);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxPadding', @sdxContainerCustomizationDialogTextBoxPadding);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxPaddingLeft', @sdxContainerCustomizationDialogTextBoxPaddingLeft);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxPaddingRight', @sdxContainerCustomizationDialogTextBoxPaddingRight);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxPaddingTop', @sdxContainerCustomizationDialogTextBoxPaddingTop);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxPaddingBottom', @sdxContainerCustomizationDialogTextBoxPaddingBottom);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxAutoSize', @sdxContainerCustomizationDialogTextBoxAutoSize);
  AProduct.Add('sdxContainerCustomizationDialogTextBoxWordWrap', @sdxContainerCustomizationDialogTextBoxWordWrap);

  AProduct.Add('sdxFindAndReplaceDialogButtonClose', @sdxFindAndReplaceDialogButtonClose);
  AProduct.Add('sdxFindAndReplaceDialogButtonFindAll', @sdxFindAndReplaceDialogButtonFindAll);
  AProduct.Add('sdxFindAndReplaceDialogButtonFindNext', @sdxFindAndReplaceDialogButtonFindNext);
  AProduct.Add('sdxFindAndReplaceDialogButtonReplace', @sdxFindAndReplaceDialogButtonReplace);
  AProduct.Add('sdxFindAndReplaceDialogButtonReplaceAll', @sdxFindAndReplaceDialogButtonReplaceAll);
  AProduct.Add('sdxFindAndReplaceDialogCaption', @sdxFindAndReplaceDialogCaption);
  AProduct.Add('sdxFindAndReplaceDialogColumnCellCaption', @sdxFindAndReplaceDialogColumnCellCaption);
  AProduct.Add('sdxFindAndReplaceDialogColumnFormulaCaption', @sdxFindAndReplaceDialogColumnFormulaCaption);
  AProduct.Add('sdxFindAndReplaceDialogColumnNameCaption', @sdxFindAndReplaceDialogColumnNameCaption);
  AProduct.Add('sdxFindAndReplaceDialogColumnSheetCaption', @sdxFindAndReplaceDialogColumnSheetCaption);
  AProduct.Add('sdxFindAndReplaceDialogColumnValueCaption', @sdxFindAndReplaceDialogColumnValueCaption);
  AProduct.Add('sdxFindAndReplaceDialogFindMatchNotFound', @sdxFindAndReplaceDialogFindMatchNotFound);
  AProduct.Add('sdxFindAndReplaceDialogFindTabCaption', @sdxFindAndReplaceDialogFindTabCaption);
  AProduct.Add('sdxFindAndReplaceDialogFindWhat', @sdxFindAndReplaceDialogFindWhat);
  AProduct.Add('sdxFindAndReplaceDialogLessOptions', @sdxFindAndReplaceDialogLessOptions);
  AProduct.Add('sdxFindAndReplaceDialogLookIn', @sdxFindAndReplaceDialogLookIn);
  AProduct.Add('sdxFindAndReplaceDialogLookInFormulas', @sdxFindAndReplaceDialogLookInFormulas);
  AProduct.Add('sdxFindAndReplaceDialogLookInValues', @sdxFindAndReplaceDialogLookInValues);
  AProduct.Add('sdxFindAndReplaceDialogMatchCase', @sdxFindAndReplaceDialogMatchCase);
  AProduct.Add('sdxFindAndReplaceDialogMatchEntireCell', @sdxFindAndReplaceDialogMatchEntireCell);
  AProduct.Add('sdxFindAndReplaceDialogMoreOptions', @sdxFindAndReplaceDialogMoreOptions);
  AProduct.Add('sdxFindAndReplaceDialogReplaceMatchNotFound', @sdxFindAndReplaceDialogReplaceMatchNotFound);
  AProduct.Add('sdxFindAndReplaceDialogReplacementResult', @sdxFindAndReplaceDialogReplacementResult);
  AProduct.Add('sdxFindAndReplaceDialogReplaceTabCaption', @sdxFindAndReplaceDialogReplaceTabCaption);
  AProduct.Add('sdxFindAndReplaceDialogReplaceWith', @sdxFindAndReplaceDialogReplaceWith);
  AProduct.Add('sdxFindAndReplaceDialogSearchMode', @sdxFindAndReplaceDialogSearchMode);
  AProduct.Add('sdxFindAndReplaceDialogSearchModeByColumns', @sdxFindAndReplaceDialogSearchModeByColumns);
  AProduct.Add('sdxFindAndReplaceDialogSearchModeByRows', @sdxFindAndReplaceDialogSearchModeByRows);
  AProduct.Add('sdxFindAndReplaceDialogStatusBarCellsFound', @sdxFindAndReplaceDialogStatusBarCellsFound);
  AProduct.Add('sdxFindAndReplaceDialogWithinRange', @sdxFindAndReplaceDialogWithinRange);
  AProduct.Add('sdxFindAndReplaceDialogWithinRangeSheet', @sdxFindAndReplaceDialogWithinRangeSheet);
  AProduct.Add('sdxFindAndReplaceDialogWithinRangeSpreadsheet', @sdxFindAndReplaceDialogWithinRangeSpreadsheet);

  AProduct.Add('sdxGradientModeBackwardDiagonal', @sdxGradientModeBackwardDiagonal);
  AProduct.Add('sdxGradientModeForwardDiagonal', @sdxGradientModeForwardDiagonal);
  AProduct.Add('sdxGradientModeHorizontal', @sdxGradientModeHorizontal);
  AProduct.Add('sdxGradientModeVertical', @sdxGradientModeVertical);

  AProduct.Add('sdxPasteSpecialDialogCaption', @sdxPasteSpecialDialogCaption);
  AProduct.Add('sdxPasteSpecialDialogButtonCancel', @sdxPasteSpecialDialogButtonCancel);
  AProduct.Add('sdxPasteSpecialDialogButtonOK', @sdxPasteSpecialDialogButtonOK);
  AProduct.Add('sdxPasteSpecialDialogPasteFormat', @sdxPasteSpecialDialogPasteFormat);
  AProduct.Add('sdxPasteSpecialDialogPasteOptions', @sdxPasteSpecialDialogPasteOptions);
  AProduct.Add('sdxPasteSpecialDialogPasteValues', @sdxPasteSpecialDialogPasteValues);
  AProduct.Add('sdxPasteSpecialDialogPasteFormulas', @sdxPasteSpecialDialogPasteFormulas);
  AProduct.Add('sdxPasteSpecialDialogPasteComments', @sdxPasteSpecialDialogPasteComments);
  AProduct.Add('sdxPasteSpecialDialogPasteStyles', @sdxPasteSpecialDialogPasteStyles);
  AProduct.Add('sdxPasteSpecialDialogPasteStylesAll', @sdxPasteSpecialDialogPasteStylesAll);
  AProduct.Add('sdxPasteSpecialDialogPasteStylesNumberFormatting', @sdxPasteSpecialDialogPasteStylesNumberFormatting);
  AProduct.Add('sdxPasteSpecialDialogPasteColumnWidths', @sdxPasteSpecialDialogPasteColumnWidths);
  AProduct.Add('sdxPasteSpecialDialogPasteSkinBlanks', @sdxPasteSpecialDialogPasteSkinBlanks);

  AProduct.Add('sdxHyperlinkEditorInsertCaption', @sdxHyperlinkEditorInsertCaption);
  AProduct.Add('sdxHyperlinkEditorEditCaption', @sdxHyperlinkEditorEditCaption);
  AProduct.Add('sdxHyperlinkEditorCancel', @sdxHyperlinkEditorCancel);
  AProduct.Add('sdxHyperlinkEditorOK', @sdxHyperlinkEditorOK);
  AProduct.Add('sdxHyperlinkEditorRemoveLink', @sdxHyperlinkEditorRemoveLink);
  AProduct.Add('sdxHyperlinkEditorLinkTo', @sdxHyperlinkEditorLinkTo);
  AProduct.Add('sdxHyperlinkEditorFileOrWebPageLink', @sdxHyperlinkEditorFileOrWebPageLink);
  AProduct.Add('sdxHyperlinkEditorPlaceInThisDocumentLink', @sdxHyperlinkEditorPlaceInThisDocumentLink);
  AProduct.Add('sdxHyperlinkEditorEmailAddressLink', @sdxHyperlinkEditorEmailAddressLink);
  AProduct.Add('sdxHyperlinkEditorTextToDisplay', @sdxHyperlinkEditorTextToDisplay);
  AProduct.Add('sdxHyperlinkEditorScreenTip', @sdxHyperlinkEditorScreenTip);
  AProduct.Add('sdxHyperlinkEditorEmailAddress', @sdxHyperlinkEditorEmailAddress);
  AProduct.Add('sdxHyperlinkEditorAddress', @sdxHyperlinkEditorAddress);
  AProduct.Add('sdxHyperlinkEditorSubject', @sdxHyperlinkEditorSubject);
  AProduct.Add('sdxHyperlinkEditorCellReference', @sdxHyperlinkEditorCellReference);
  AProduct.Add('sdxHyperlinkEditorSelectPlace', @sdxHyperlinkEditorSelectPlace);
  AProduct.Add('sdxHyperlinkEditorCellReferenceNode', @sdxHyperlinkEditorCellReferenceNode);
  AProduct.Add('sdxHyperlinkEditorDefinedNamesNode', @sdxHyperlinkEditorDefinedNamesNode);

  AProduct.Add('sdxUnhideSheetDialogCaption', @sdxUnhideSheetDialogCaption);
  AProduct.Add('sdxUnhideSheetDialogHiddenSheets', @sdxUnhideSheetDialogHiddenSheets);

  AProduct.Add('sdxPasswordDialogButtonCancel', @sdxPasswordDialogButtonCancel);
  AProduct.Add('sdxPasswordDialogButtonOK', @sdxPasswordDialogButtonOK);
  AProduct.Add('sdxPasswordDialogCaption', @sdxPasswordDialogCaption);
  AProduct.Add('sdxPasswordDialogCaptionConfirm', @sdxPasswordDialogCaptionConfirm);
  AProduct.Add('sdxPasswordDialogPasswordNotes', @sdxPasswordDialogPasswordNotes);
  AProduct.Add('sdxPasswordDialogPassword', @sdxPasswordDialogPassword);
  AProduct.Add('sdxPasswordDialogPasswordConfirmation', @sdxPasswordDialogPasswordConfirmation);
  AProduct.Add('sdxPasswordDialogPasswordNotMatch', @sdxPasswordDialogPasswordNotMatch);

  AProduct.Add('sdxProtectSheetDialogCaption', @sdxProtectSheetDialogCaption);
  AProduct.Add('sdxProtectSheetDialogButtonCancel', @sdxProtectSheetDialogButtonCancel);
  AProduct.Add('sdxProtectSheetDialogButtonOK', @sdxProtectSheetDialogButtonOK);
  AProduct.Add('sdxProtectSheetDialogPassword', @sdxProtectSheetDialogPassword);
  AProduct.Add('sdxProtectSheetDialogPermissions', @sdxProtectSheetDialogPermissions);
  AProduct.Add('sdxProtectSheetDialogProtect', @sdxProtectSheetDialogProtect);
  AProduct.Add('sdxProtectSheetDialogAllowDeleteColumns', @sdxProtectSheetDialogAllowDeleteColumns);
  AProduct.Add('sdxProtectSheetDialogAllowDeleteRows', @sdxProtectSheetDialogAllowDeleteRows);
  AProduct.Add('sdxProtectSheetDialogAllowResizeColumns', @sdxProtectSheetDialogAllowResizeColumns);
  AProduct.Add('sdxProtectSheetDialogAllowEditContainers', @sdxProtectSheetDialogAllowEditContainers);
  AProduct.Add('sdxProtectSheetDialogAllowEditHyperlinks', @sdxProtectSheetDialogAllowEditHyperlinks);
  AProduct.Add('sdxProtectSheetDialogAllowResizeRows', @sdxProtectSheetDialogAllowResizeRows);
  AProduct.Add('sdxProtectSheetDialogAllowFormatCells', @sdxProtectSheetDialogAllowFormatCells);
  AProduct.Add('sdxProtectSheetDialogAllowInsertColumns', @sdxProtectSheetDialogAllowInsertColumns);
  AProduct.Add('sdxProtectSheetDialogAllowInsertRows', @sdxProtectSheetDialogAllowInsertRows);
  AProduct.Add('sdxProtectSheetDialogAllowSelectLockedCells', @sdxProtectSheetDialogAllowSelectLockedCells);
  AProduct.Add('sdxProtectSheetDialogAllowSelectUnlockedCells', @sdxProtectSheetDialogAllowSelectUnlockedCells);
  AProduct.Add('sdxProtectSheetDialogAllowSort', @sdxProtectSheetDialogAllowSort);

  AProduct.Add('sdxProtectWorkbookDialogCaption', @sdxProtectWorkbookDialogCaption);
  AProduct.Add('sdxProtectWorkbookDialogButtonOK', @sdxProtectWorkbookDialogButtonOK);
  AProduct.Add('sdxProtectWorkbookDialogButtonCancel', @sdxProtectWorkbookDialogButtonCancel);
  AProduct.Add('sdxProtectWorkbookDialogProtectionOptions', @sdxProtectWorkbookDialogProtectionOptions);
  AProduct.Add('sdxProtectWorkbookDialogProtectStructure', @sdxProtectWorkbookDialogProtectStructure);
  AProduct.Add('sdxProtectWorkbookDialogPassword', @sdxProtectWorkbookDialogPassword);

  AProduct.Add('sdxPageSetupDialogAlignWithMargins', @sdxPageSetupDialogAlignWithMargins);
  AProduct.Add('sdxPageSetupDialogBlackAndWhite', @sdxPageSetupDialogBlackAndWhite);
  AProduct.Add('sdxPageSetupDialogButtonCancel', @sdxPageSetupDialogButtonCancel);
  AProduct.Add('sdxPageSetupDialogButtonCustomHeaderFooter', @sdxPageSetupDialogButtonCustomHeaderFooter);
  AProduct.Add('sdxPageSetupDialogButtonOK', @sdxPageSetupDialogButtonOK);
  AProduct.Add('sdxPageSetupDialogButtonPrint', @sdxPageSetupDialogButtonPrint);
  AProduct.Add('sdxPageSetupDialogButtonPrintPreview', @sdxPageSetupDialogButtonPrintPreview);
  AProduct.Add('sdxPageSetupDialogCaption', @sdxPageSetupDialogCaption);
  AProduct.Add('sdxPageSetupDialogCenterHorizontally', @sdxPageSetupDialogCenterHorizontally);
  AProduct.Add('sdxPageSetupDialogCenterOnPage', @sdxPageSetupDialogCenterOnPage);
  AProduct.Add('sdxPageSetupDialogCenterVertically', @sdxPageSetupDialogCenterVertically);
  AProduct.Add('sdxPageSetupDialogDownThenOver', @sdxPageSetupDialogDownThenOver);
  AProduct.Add('sdxPageSetupDialogFooter', @sdxPageSetupDialogFooter);
  AProduct.Add('sdxPageSetupDialogHeader', @sdxPageSetupDialogHeader);
  AProduct.Add('sdxPageSetupDialogHeaderFooter', @sdxPageSetupDialogHeaderFooter);
  AProduct.Add('sdxPageSetupDialogMarginBottom', @sdxPageSetupDialogMarginBottom);
  AProduct.Add('sdxPageSetupDialogMarginFooter', @sdxPageSetupDialogMarginFooter);
  AProduct.Add('sdxPageSetupDialogMarginHeader', @sdxPageSetupDialogMarginHeader);
  AProduct.Add('sdxPageSetupDialogMarginLeft', @sdxPageSetupDialogMarginLeft);
  AProduct.Add('sdxPageSetupDialogMarginRight', @sdxPageSetupDialogMarginRight);
  AProduct.Add('sdxPageSetupDialogMargins', @sdxPageSetupDialogMargins);
  AProduct.Add('sdxPageSetupDialogMarginTop', @sdxPageSetupDialogMarginTop);
  AProduct.Add('sdxPageSetupDialogOverThenDown', @sdxPageSetupDialogOverThenDown);
  AProduct.Add('sdxPageSetupDialogPage', @sdxPageSetupDialogPage);
  AProduct.Add('sdxPageSetupDialogPageFirstPageNumber', @sdxPageSetupDialogPageFirstPageNumber);
  AProduct.Add('sdxPageSetupDialogPageOrder', @sdxPageSetupDialogPageOrder);
  AProduct.Add('sdxPageSetupDialogPageOrientation', @sdxPageSetupDialogPageOrientation);
  AProduct.Add('sdxPageSetupDialogPageOrientationLandscape', @sdxPageSetupDialogPageOrientationLandscape);
  AProduct.Add('sdxPageSetupDialogPageOrientationPortrait', @sdxPageSetupDialogPageOrientationPortrait);
  AProduct.Add('sdxPageSetupDialogPaperSize', @sdxPageSetupDialogPaperSize);
  AProduct.Add('sdxPageSetupDialogPrint', @sdxPageSetupDialogPrint);
  AProduct.Add('sdxPageSetupDialogPrintArea', @sdxPageSetupDialogPrintArea);
  AProduct.Add('sdxPageSetupDialogPrintCellErrorsMode', @sdxPageSetupDialogPrintCellErrorsMode);
  AProduct.Add('sdxPageSetupDialogPrintCellErrorsModeBlank', @sdxPageSetupDialogPrintCellErrorsModeBlank);
  AProduct.Add('sdxPageSetupDialogPrintCellErrorsModeDash', @sdxPageSetupDialogPrintCellErrorsModeDash);
  AProduct.Add('sdxPageSetupDialogPrintCellErrorsModeDisplayed', @sdxPageSetupDialogPrintCellErrorsModeDisplayed);
  AProduct.Add('sdxPageSetupDialogPrintCellErrorsModeNA', @sdxPageSetupDialogPrintCellErrorsModeNA);
  AProduct.Add('sdxPageSetupDialogPrintCommentsMode', @sdxPageSetupDialogPrintCommentsMode);
  AProduct.Add('sdxPageSetupDialogPrintCommentsModeAsDisplayed', @sdxPageSetupDialogPrintCommentsModeAsDisplayed);
  AProduct.Add('sdxPageSetupDialogPrintCommentsModeAtEnd', @sdxPageSetupDialogPrintCommentsModeAtEnd);
  AProduct.Add('sdxPageSetupDialogPrintCommentsModeNode', @sdxPageSetupDialogPrintCommentsModeNode);
  AProduct.Add('sdxPageSetupDialogPrintDraftQuality', @sdxPageSetupDialogPrintDraftQuality);
  AProduct.Add('sdxPageSetupDialogPrintGridlines', @sdxPageSetupDialogPrintGridlines);
  AProduct.Add('sdxPageSetupDialogPrintRowAndColumnHeadings', @sdxPageSetupDialogPrintRowAndColumnHeadings);
  AProduct.Add('sdxPageSetupDialogPrintTitles', @sdxPageSetupDialogPrintTitles);
  AProduct.Add('sdxPageSetupDialogPrintTitlesColumnsToRepeat', @sdxPageSetupDialogPrintTitlesColumnsToRepeat);
  AProduct.Add('sdxPageSetupDialogPrintTitlesRowsToRepeat', @sdxPageSetupDialogPrintTitlesRowsToRepeat);
  AProduct.Add('sdxPageSetupDialogScaleWithDocument', @sdxPageSetupDialogScaleWithDocument);
  AProduct.Add('sdxPageSetupDialogScaling', @sdxPageSetupDialogScaling);
  AProduct.Add('sdxPageSetupDialogScalingAdjustTo', @sdxPageSetupDialogScalingAdjustTo);
  AProduct.Add('sdxPageSetupDialogScalingAdjustToSuffix', @sdxPageSetupDialogScalingAdjustToSuffix);
  AProduct.Add('sdxPageSetupDialogScalingFitTo', @sdxPageSetupDialogScalingFitTo);
  AProduct.Add('sdxPageSetupDialogScalingFitToPageTall', @sdxPageSetupDialogScalingFitToPageTall);
  AProduct.Add('sdxPageSetupDialogScalingFitToPageWide', @sdxPageSetupDialogScalingFitToPageWide);
  AProduct.Add('sdxPageSetupDialogTabSheetCaption', @sdxPageSetupDialogTabSheetCaption);
  AProduct.Add('sdxPageSetupDialogUnitsInches', @sdxPageSetupDialogUnitsInches);
  AProduct.Add('sdxPageSetupDialogUnitsMillimeters', @sdxPageSetupDialogUnitsMillimeters);

  AProduct.Add('sdxPageSetupHeaderFooterDialogButtonCancel', @sdxPageSetupHeaderFooterDialogButtonCancel);
  AProduct.Add('sdxPageSetupHeaderFooterDialogButtonOK', @sdxPageSetupHeaderFooterDialogButtonOK);
  AProduct.Add('sdxPageSetupHeaderFooterDialogCaption', @sdxPageSetupHeaderFooterDialogCaption);
  AProduct.Add('sdxPageSetupHeaderFooterDialogCenterFooter', @sdxPageSetupHeaderFooterDialogCenterFooter);
  AProduct.Add('sdxPageSetupHeaderFooterDialogCenterHeader', @sdxPageSetupHeaderFooterDialogCenterHeader);
  AProduct.Add('sdxPageSetupHeaderFooterDialogDescription', @sdxPageSetupHeaderFooterDialogDescription);
  AProduct.Add('sdxPageSetupHeaderFooterDialogLeftFooter', @sdxPageSetupHeaderFooterDialogLeftFooter);
  AProduct.Add('sdxPageSetupHeaderFooterDialogLeftHeader', @sdxPageSetupHeaderFooterDialogLeftHeader);
  AProduct.Add('sdxPageSetupHeaderFooterDialogRightFooter', @sdxPageSetupHeaderFooterDialogRightFooter);
  AProduct.Add('sdxPageSetupHeaderFooterDialogRightHeader', @sdxPageSetupHeaderFooterDialogRightHeader);
  AProduct.Add('sdxPageSetupHeaderFooterDialogHintInsertDate', @sdxPageSetupHeaderFooterDialogHintInsertDate);
  AProduct.Add('sdxPageSetupHeaderFooterDialogHintInsertPageNumber', @sdxPageSetupHeaderFooterDialogHintInsertPageNumber);
  AProduct.Add('sdxPageSetupHeaderFooterDialogHintInsertPageTotal', @sdxPageSetupHeaderFooterDialogHintInsertPageTotal);
  AProduct.Add('sdxPageSetupHeaderFooterDialogHintInsertSheetName', @sdxPageSetupHeaderFooterDialogHintInsertSheetName);
  AProduct.Add('sdxPageSetupHeaderFooterDialogHintInsertTime', @sdxPageSetupHeaderFooterDialogHintInsertTime);

  AProduct.Add('sdxInsertFunctionDialogCaption', @sdxInsertFunctionDialogCaption);
  AProduct.Add('sdxInsertFunctionDialogCategory', @sdxInsertFunctionDialogCategory);
  AProduct.Add('sdxInsertFunctionDialogCategoryAll', @sdxInsertFunctionDialogCategoryAll);
  AProduct.Add('sdxInsertFunctionDialogFunctions', @sdxInsertFunctionDialogFunctions);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressSpreadSheet 2', @AddSpreadSheetDialogsResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressSpreadSheet 2', @AddSpreadSheetDialogsResourceStringNames);
end.
