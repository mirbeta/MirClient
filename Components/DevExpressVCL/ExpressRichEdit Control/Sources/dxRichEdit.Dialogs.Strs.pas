{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Dialogs.Strs;

{$I cxVer.inc}

interface

uses
  dxCore, cxClasses;

resourcestring
  sdxRichEditTabForm_All = 'All';


  sdxRichEditInvalidSize = 'The number must be between %d and %d.';
  sdxRichEditInvalidMeasurementValue = 'The measurement must be between %s and %s.';
  sdxRichEditInvalidNumber = 'This is not a valid number.';
  sdxRichEditInvalidMeasurement = 'This is not a valid measurement';


  sdxSectionPropertiesApplyToWholeDocument = 'Whole document';
  sdxSectionPropertiesApplyToCurrentSection = 'Current section';
  sdxSectionPropertiesApplyToSelectedSections = 'Selected sections';
  sdxSectionPropertiesApplyThisPointForward = 'This point forward';

  sdxRichEditDialogButtonOK = 'OK';
  sdxRichEditDialogButtonCancel = 'Cancel';


  sdxRichEditParagraphDialogAfter = 'Aft&er:';
  sdxRichEditParagraphDialogAlignment = 'Ali&gnment:';
  sdxRichEditParagraphDialogAt = '&At:';
  sdxRichEditParagraphDialogBefore = '&Before:';
  sdxRichEditParagraphDialogButtonTabs = '&Tabs...';
  sdxRichEditParagraphDialogBy = 'B&y:';
  sdxRichEditParagraphDialogDontAddSpace = 'Don''t add spa&ce between paragraphs of the same style';
  sdxRichEditParagraphDialogForm = 'Paragraph';
  sdxRichEditParagraphDialogGeneral = 'General';
  sdxRichEditParagraphDialogIndentation = 'Indentation';
  sdxRichEditParagraphDialogIndentsAndSpacing = '&Indents and Spacing';
  sdxRichEditParagraphDialogKeepLinesTogether = '&Keep lines together';
  sdxRichEditParagraphDialogLeft = '&Left:';
  sdxRichEditParagraphDialogLineAndPageBreaks = 'Line and &Page Breaks';
  sdxRichEditParagraphDialogLineSpacing = 'Li&ne Spacing:';
  sdxRichEditParagraphDialogOutlinelevel = '&Outline level:';
  sdxRichEditParagraphDialogPageBreakBefore = 'Page &break before';
  sdxRichEditParagraphDialogPagination = 'Pagination';
  sdxRichEditParagraphDialogRight = '&Right:';
  sdxRichEditParagraphDialogSpacing = 'Spacing';
  sdxRichEditParagraphDialogSpecial = '&Special:';


  sdxParagraphAlignmentLeft = 'Left';
  sdxParagraphAlignmentRight = 'Right';
  sdxParagraphAlignmentCenter = 'Center';
  sdxParagraphAlignmentJustify = 'Justify';

  sdxParagraphLineSpacingSingle = 'Single';
  sdxParagraphLineSpacingSesquialteral = '1.5 lines';
  sdxParagraphLineSpacingDouble = 'Double';
  sdxParagraphLineSpacingMultiple = 'Multiple';
  sdxParagraphLineSpacingExactly = 'Exactly';
  sdxParagraphLineSpacingAtLeast = 'AtLeast';

  sdxParagraphFirstLineIndentNone = '(none)';
  sdxParagraphFirstLineIndentIndented = 'First line';
  sdxParagraphFirstLineIndentHanging = 'Hanging';

  sdxParagraphOutlineLeve0 = 'Body Text';
  sdxParagraphOutlineLeve1 = 'Level 1';
  sdxParagraphOutlineLeve2 = 'Level 2';
  sdxParagraphOutlineLeve3 = 'Level 3';
  sdxParagraphOutlineLeve4 = 'Level 4';
  sdxParagraphOutlineLeve5 = 'Level 5';
  sdxParagraphOutlineLeve6 = 'Level 6';
  sdxParagraphOutlineLeve7 = 'Level 7';
  sdxParagraphOutlineLeve8 = 'Level 8';
  sdxParagraphOutlineLeve9 = 'Level 9';


  sdxRichEditTabsDialogAlignment = 'Alignment';
  sdxRichEditTabsDialogButtonClear = 'Cl&ear';
  sdxRichEditTabsDialogButtonClearAll = 'Clear &All';
  sdxRichEditTabsDialogButtonSet = '&Set';
  sdxRichEditTabsDialogCenter = '&Center';
  sdxRichEditTabsDialogDecimal = '&Decimal';
  sdxRichEditTabsDialogDefaultTabStops = 'De&fault tab stops:';
  sdxRichEditTabsDialogDots = 'D&ots';
  sdxRichEditTabsDialogEqualSign = 'E&qualSign';
  sdxRichEditTabsDialogForm = 'Tabs';
  sdxRichEditTabsDialogHyphens = '&Hyphens';
  sdxRichEditTabsDialogLeader = 'Leader';
  sdxRichEditTabsDialogLeft = '&Left';
  sdxRichEditTabsDialogMiddleDots = '&MiddleDots';
  sdxRichEditTabsDialogNone = '(&None)';
  sdxRichEditTabsDialogRight = '&Right';
  sdxRichEditTabsDialogTabStopPosition = '&Tab stop position:';
  sdxRichEditTabsDialogTabStopsToBeCleared = 'Tab stops to be cleared:';
  sdxRichEditTabsDialogThickLine = 'Th&ickLine';
  sdxRichEditTabsDialogUnderline = '&Underline';


  sdxRichEditNumberingListDialogBulleted = '&Bulleted';
  sdxRichEditNumberingListDialogButtonCustomize = 'Customize...';
  sdxRichEditNumberingListDialogForm = 'Bullets and Numbering';
  sdxRichEditNumberingListDialogNumbered = '&Numbered';
  sdxRichEditNumberingListDialogOutlineNumbered = 'O&utline Numbered';
  sdxRichEditNumberingListDialogRestartNumbering = 'Restart numbering';
  sdxRichEditNumberingListDialogContinuePreviousList = 'Continue previous list';


  sdxRichEditBulletedListDialogAlignedAt = '&Aligned at:';
  sdxRichEditBulletedListDialogBulletCharacter = 'B&ullet character';
  sdxRichEditBulletedListDialogBulletPosition = 'Bullet position';
  sdxRichEditBulletedListDialogButtonCharacter = '&Character...';
  sdxRichEditBulletedListDialogForm = 'Customize Bulleted List';
  sdxRichEditBulletedListDialogIndentAt = '&Indent at:';
  sdxRichEditBulletedListDialogTextPosition = 'Text position';


  sdxRichEditFontDialogAllCaps = '&All caps';
  sdxRichEditFontDialogDoubleStrikeout = 'Double strikethrou&gh';
  sdxRichEditFontDialogEffects = 'Effects';
  sdxRichEditFontDialogFontColor = 'Font Color:';
  sdxRichEditFontDialogFontName = 'Font:';
  sdxRichEditFontDialogFontSize = 'Size:';
  sdxRichEditFontDialogFontStyle = 'Font style:';
  sdxRichEditFontDialogForm = 'Font';
  sdxRichEditFontDialogHidden = '&Hidden';
  sdxRichEditFontDialogPreview = 'Preview';
  sdxRichEditFontDialogStrikeout = 'Stri&kethrough';
  sdxRichEditFontDialogSubscript = 'Su&bscript';
  sdxRichEditFontDialogSuperscript = 'Su&perscript';
  sdxRichEditFontDialogUnderlineColor = 'Underline color:';
  sdxRichEditFontDialogUnderlineStyle = 'Underline style:';
  sdxRichEditFontDialogUnderlineWordsOnly = '&Underline words only';

  sdxRichEditFontDialogFontStyleRegular = 'Regular';
  sdxRichEditFontDialogFontStyleItalic = 'Italic';
  sdxRichEditFontDialogFontStyleBold = 'Bold';
  sdxRichEditFontDialogFontStyleBoldItalic = 'Bold Italic';

  sdxRichEditFontDialogUnderlineStyleNone = '(none)';
  sdxRichEditFontDialogUnderlineStyleSingle = 'Single';
  sdxRichEditFontDialogUnderlineStyleDouble = 'Double';

  sdxRichEditFontDialogButtonColorAuto = 'Auto';

  sdxRichEditFontDialogFontNotInstalled =
    'This font is not installed on the system. The closest available font will be used for printing.';
  sdxRichEditFontDialogPrintNotes =
    'This is a TrueType font. The same font will be used on both your printer and your screen.';
  sdxRichEditFontDialogFontStyleImitated =
    'This font style is imitated for display. The closest matching style will be printed.';


  sdxRichEditNumberingListBoxNone = 'None';

  sdxRichEditCustomNumberingListAlignedAt = '&Aligned at:';
  sdxRichEditCustomNumberingListButtonFont = '&Font...';
  sdxRichEditCustomNumberingListDisplayFormat = 'Number f&ormat:';
  sdxRichEditCustomNumberingListIndentAt = '&Indent at:';
  sdxRichEditCustomNumberingListNumberFormat = 'Number f&ormat';
  sdxRichEditCustomNumberingListNumberPosition = 'N&umber position';
  sdxRichEditCustomNumberingListNumberStyle = '&Number style:';
  sdxRichEditCustomNumberingListStartAt = '&Start at:';
  sdxRichEditCustomNumberingListTextPosition = 'Text position';


  sdxRichEditSimpleNumberingListDialogForm = 'Customize Numbered List';


  sdxRichEditMultiLevelNumberingListDialogFollowNumberWith = 'Follo&w Number With:';
  sdxRichEditMultiLevelNumberingListDialogForm = 'Customize Outline Numbered List';
  sdxRichEditMultiLevelNumberingListDialogLevel = 'Le&vel';

  sdxRichEditMultiLevelNumberingListFollowNumberTabCharacter = 'Tab character';
  sdxRichEditMultiLevelNumberingListFollowNumberSpace = 'Space';
  sdxRichEditMultiLevelNumberingListFollowNumberNothing = 'Nothing';


  sdxRichEditSearchTextDialogButtonFindNext = '&Find Next';
  sdxRichEditSearchTextDialogButtonReplaceAll = 'Replace &All';
  sdxRichEditSearchTextDialogButtonReplaceNext = '&Replace';
  sdxRichEditSearchTextDialogFind = 'Fin&d';
  sdxRichEditSearchTextDialogForm = 'Find and Replace';
  sdxRichEditSearchTextDialogReplace = 'Re&place';
  sdxRichEditSearchTextDialogRplReplaceString = 'replace w&ith:';
  sdxRichEditSearchTextDialogSearchString = 'Fi&nd:';
  sdxRichEditSearchTextDialogDirection = 'Search&:';
  sdxRichEditSearchTextDialogMatchCase = 'Matc&h case';
  sdxRichEditSearchTextDialogFindWholeWord = 'Find whole words onl&y';
  sdxRichEditSearchTextDialogRegex = 'Re&gular expression';

  sdxRichEditSearchTextDialogDirectionAll = 'All';
  sdxRichEditSearchTextDialogDirectionDown = 'Down';
  sdxRichEditSearchTextDialogDirectionUp = 'Up';

  sdxRichEditSearchTextDialogAnySingleCharacter = 'Any single character';
  sdxRichEditSearchTextDialogZeroOrMore = 'Zero or more';
  sdxRichEditSearchTextDialogOneOrMore = 'One or more';
  sdxRichEditSearchTextDialogBeginningOfLine = 'Beginning of paragraph';
  sdxRichEditSearchTextDialogEndOfLine = 'End of paragraph';
  sdxRichEditSearchTextDialogBeginningOfWord = 'Beginning of word';
  sdxRichEditSearchTextDialogEndOfWord = 'End of word';
  sdxRichEditSearchTextDialogAnyOneCharacterInTheSet = 'Any one character in the set';
  sdxRichEditSearchTextDialogAnyOneCharacterNotInTheSet = 'Any one character not in the set';
  sdxRichEditSearchTextDialogOr = 'Or';
  sdxRichEditSearchTextDialogEscapeSpecialCharacter = 'Escape special character';
  sdxRichEditSearchTextDialogTagExpression = 'Tag expression';
  sdxRichEditSearchTextDialogWordCharacter = 'Word character';
  sdxRichEditSearchTextDialogSpaceOrTab = 'Space or tab';
  sdxRichEditSearchTextDialogInteger = 'Integer';

  sdxRichEditSearchTextDialogTaggedExpression = 'Tagged expression';


  sdxRichEditSplitTableCellsDialogForm = 'Split Cells';
  sdxRichEditSplitTableCellsDialogMergeBeforeSplit = 'Merge cells before split';
  sdxRichEditSplitTableCellsDialogNumberOfColumns = 'Number of &columns:';
  sdxRichEditSplitTableCellsDialogNumberOfRows = 'Number of &rows:';


  sdxRichEditInsertTableCellsDialogCellOperationDeleteColumn = 'Insert entire &column';
  sdxRichEditInsertTableCellsDialogCellOperationDeleteRow = 'Insert entire &row';
  sdxRichEditInsertTableCellsDialogCellOperationShiftLeft = 'Shift cells r&ight';
  sdxRichEditInsertTableCellsDialogCellOperationShiftUp = 'Shift cells &down';
  sdxRichEditInsertTableCellsDialogForm = 'Insert Cells';


  sdxRichEditDeleteTableCellsDialogCellOperationDeleteColumn = 'Delete entire &column';
  sdxRichEditDeleteTableCellsDialogCellOperationDeleteRow = 'Delete entire &row';
  sdxRichEditDeleteTableCellsDialogCellOperationShiftLeft = 'Shift cells &left';
  sdxRichEditDeleteTableCellsDialogCellOperationShiftUp = 'Shift cells &up';
  sdxRichEditDeleteTableCellsDialogForm = 'Delete Cells';


  sdxRichEditTablePropertiesDialogButtonBorder = 'Borders and Shading...';
  sdxRichEditTablePropertiesDialogButtonCellOptions = '&Options...';
  sdxRichEditTablePropertiesDialogButtonNextColumn = '&Next Column';
  sdxRichEditTablePropertiesDialogButtonNextRow = '&Next Row';
  sdxRichEditTablePropertiesDialogButtonPreviousColumn = '&Previous Column';
  sdxRichEditTablePropertiesDialogButtonPreviousRow = '&Previous Row';
  sdxRichEditTablePropertiesDialogButtonTableOptions = '&Options...';
  sdxRichEditTablePropertiesDialogCantSplit = 'Allow row to brea&k across pages';
  sdxRichEditTablePropertiesDialogCell = 'C&ell';
  sdxRichEditTablePropertiesDialogCellVerticalAlighment = 'Vertical alignment';
  sdxRichEditTablePropertiesDialogColumn = 'Col&umn';
  sdxRichEditTablePropertiesDialogColumnNumber = 'Column';
  sdxRichEditTablePropertiesDialogForm = 'Table Properties';
  sdxRichEditTablePropertiesDialogHeader = 'Repeat as &header row at the top of each page';
  sdxRichEditTablePropertiesDialogIndentFromLeft = '&Indent from left:';
  sdxRichEditTablePropertiesDialogPreferredWidth = 'Preferred &width:';
  sdxRichEditTablePropertiesDialogCellVerticalAlignmentBottom = '&Bottom';
  sdxRichEditTablePropertiesDialogCellVerticalAlignmentCenter = '&Center';
  sdxRichEditTablePropertiesDialogCellVerticalAlignmentTop = 'To&p';
  sdxRichEditTablePropertiesDialogRow = '&Row';
  sdxRichEditTablePropertiesDialogRowHeightType = 'Row height &is:';
  sdxRichEditTablePropertiesDialogRowNumber = 'Row';
  sdxRichEditTablePropertiesDialogRowOptions = '&Options';
  sdxRichEditTablePropertiesDialogSize = 'Size';
  sdxRichEditTablePropertiesDialogSpecifyHeight = '&Specify height:';
  sdxRichEditTablePropertiesDialogTable = '&Table';
  sdxRichEditTablePropertiesDialogTableAlignmenCenter = '&Center';
  sdxRichEditTablePropertiesDialogTableAlignmenRight = 'Rig&ht';
  sdxRichEditTablePropertiesDialogTableAlignment = 'Alignment';
  sdxRichEditTablePropertiesDialogTableAlignmentLeft = '&Left';
  sdxRichEditTablePropertiesDialogWidthType = '&Measure in:';

  sdxRichEditTablePropertiesHeightTypeExact = 'Exactly';
  sdxRichEditTablePropertiesHeightTypeMinimum = 'At least';


  sdxRichEditCustomTableOptionsDialogBottomMargin = '&Bottom:';
  sdxRichEditCustomTableOptionsDialogLeftMargin = '&Left:';
  sdxRichEditCustomTableOptionsDialogRightMargin = '&Right:';
  sdxRichEditCustomTableOptionsDialogTopMargin = '&Top:';


  sdxRichEditTableOptionsDialogAllowCellSpacing = 'Allow &spacing between cells';
  sdxRichEditTableOptionsDialogDefaultCellSpacing = 'Default cell spacing';
  sdxRichEditTableOptionsDialogForm = 'Table Options';
  sdxRichEditTableOptionsDialogMargins = 'Default cell margins';
  sdxRichEditTableOptionsDialogOptions = 'Options';
  sdxRichEditTableOptionsDialogResizeToFitContent = 'Automatically resi&ze to fit contents';


  sdxRichEditTableCellOptionsDialogFitText = '&Fit text';
  sdxRichEditTableCellOptionsDialogForm = 'Cell Options';
  sdxRichEditTableCellOptionsDialogMargins = 'Cell margins';
  sdxRichEditTableCellOptionsDialogOptions = 'Options';
  sdxRichEditTableCellOptionsDialogSameAsWholeTable = '&Same as the whole table';
  sdxRichEditTableCellOptionsDialogWrapText = '&Wrap text';


  sdxRichEditBorderShadingDialogAll = '&All';
  sdxRichEditBorderShadingDialogApplyTo = 'App&ly to:';
  sdxRichEditBorderShadingDialogBorderLineColor = '&Color:';
  sdxRichEditBorderShadingDialogBorderLineStyle = 'St&yle:';
  sdxRichEditBorderShadingDialogBorderLineWeight = '&Width:';
  sdxRichEditBorderShadingDialogBorders = '&Borders';
  sdxRichEditBorderShadingDialogPreview = 'Preview';
  sdxRichEditBorderShadingDialogPreviewTxt = 'Click on diagram below or use buttons to apply  borders';
  sdxRichEditBorderShadingDialogBox = 'Bo&x';
  sdxRichEditBorderShadingDialogButtonOptions = '&Options...';
  sdxRichEditBorderShadingDialogCustom = 'C&ustom';
  sdxRichEditBorderShadingDialogForm = 'Borders and Shading';
  sdxRichEditBorderShadingDialogGrid = 'Gri&d';
  sdxRichEditBorderShadingDialogNone = '&None';
  sdxRichEditBorderShadingDialogShading = '&Shading';
  sdxRichEditBorderShadingDialogShadingFill = 'Fill';

  sdxRichEditBorderShadingDialogApplyToCell = 'Cell';
  sdxRichEditBorderShadingDialogApplyToTable = 'Table';

  sdxRichEditBorderLineStyleNone = 'None';


  sdxRichEditLineNumberingDialogAddLineNumbering = 'Add &line numbering';
  sdxRichEditLineNumberingDialogCountBy = 'Count &by:';
  sdxRichEditLineNumberingDialogForm = 'Line Numbers';
  sdxRichEditLineNumberingDialogFromText = 'From &text:';
  sdxRichEditLineNumberingDialogNumbering = 'Numbering:';
  sdxRichEditLineNumberingDialogNumberingRestartContinuous = '&Continuous';
  sdxRichEditLineNumberingDialogNumberingRestartEachPage = 'Restart each &page';
  sdxRichEditLineNumberingDialogNumberingRestartEachSection = 'Restart each &section';
  sdxRichEditLineNumberingDialogStartAt = 'Start &at:';


  sdxRichEditColumnsSetupDialogApplyTo = '&Apply to:';
  sdxRichEditColumnsSetupDialogColumnCount = '&Number of columns:';
  sdxRichEditColumnsSetupDialogColumnNumber = '&Col #:';
  sdxRichEditColumnsSetupDialogColumnsPresetLeft = 'Left';
  sdxRichEditColumnsSetupDialogColumnsPresetOne = '&One';
  sdxRichEditColumnsSetupDialogColumnsPresetRight = '&Right';
  sdxRichEditColumnsSetupDialogColumnsPresetThree = '&Three';
  sdxRichEditColumnsSetupDialogColumnsPresetTwo = 'T&wo';
  sdxRichEditColumnsSetupDialogEqualColumnWidth = '&Equal column width';
  sdxRichEditColumnsSetupDialogForm = 'Columns';
  sdxRichEditColumnsSetupDialogLineBetween = 'Line &between';
  sdxRichEditColumnsSetupDialogPresets = 'Presets';
  sdxRichEditColumnsSetupDialogSpacing = '&Spacing';
  sdxRichEditColumnsSetupDialogStartNewColumn = 'Start new col&umn';
  sdxRichEditColumnsSetupDialogWidth = 'W&idth';
  sdxRichEditColumnsSetupDialogWidthSpacing = 'Width and spacing';


  sdxRichEditPageSetupDialogApplyTo = 'Appl&y to:';
  sdxRichEditPageSetupDialogDifferentFirstPage = 'Different first &page';
  sdxRichEditPageSetupDialogDifferentOddAndEvenPage = 'Different &odd and even';
  sdxRichEditPageSetupDialogForm = 'Page Setup';
  sdxRichEditPageSetupDialogHeadersAndFooters = 'Headers and footers';
  sdxRichEditPageSetupDialogLandscape = 'Land&scape';
  sdxRichEditPageSetupDialogMarginBottom = '&Bottom:';
  sdxRichEditPageSetupDialogMarginLeft = '&Left:';
  sdxRichEditPageSetupDialogMarginRight = '&Right:';
  sdxRichEditPageSetupDialogMargins = 'Margins';
  sdxRichEditPageSetupDialogMarginTop = '&Top:';
  sdxRichEditPageSetupDialogOrientation = 'Orientation';
  sdxRichEditPageSetupDialogPageLayout = 'Layout';
  sdxRichEditPageSetupDialogPageMargins = 'Margins';
  sdxRichEditPageSetupDialogPagePaper = 'Paper';
  sdxRichEditPageSetupDialogPaperHeight = '&Height:';
  sdxRichEditPageSetupDialogPaperSize = 'Pape&r size';
  sdxRichEditPageSetupDialogPaperWidth = '&Width:';
  sdxRichEditPageSetupDialogPortrait = '&Portrait';
  sdxRichEditPageSetupDialogSection = 'Section';
  sdxRichEditPageSetupDialogSectionStart = 'Section sta&rt:';

  sdxRichEditPageSetupSectionStartContinuous = 'Continuous';
  sdxRichEditPageSetupSectionStartColumn = 'New column';
  sdxRichEditPageSetupSectionStartNextPage = 'New page';
  sdxRichEditPageSetupSectionStartOddPage = 'Odd page';
  sdxRichEditPageSetupSectionStartEvenPage = 'Even page';


  sdxRichEditSymbolDialogFont = '&Font:';
  sdxRichEditSymbolDialogForm = 'Symbol';
  sdxRichEditSymbolDialogCharacterCode = '&Character code:';


  sdxRichEditBookmarkDialogBookmarkName = '&Bookmark Name:';
  sdxRichEditBookmarkDialogButtonAdd = '&Add';
  sdxRichEditBookmarkDialogButtonCancel = 'Close';
  sdxRichEditBookmarkDialogButtonDelete = '&Delete';
  sdxRichEditBookmarkDialogButtonGoTo = '&Go To';
  sdxRichEditBookmarkDialogForm = 'Bookmark';
  sdxRichEditBookmarkDialogSortBy = 'Sort By:';
  sdxRichEditBookmarkDialogSortByLocation = '&Location';
  sdxRichEditBookmarkDialogSortByName = '&Name';


  sdxRichEditEditStyleDialogButtonFormat = 'F&ormat';
  sdxRichEditEditStyleDialogCurrentStyle = 'Current style:';
  sdxRichEditEditStyleDialogDecrementIndent = 'Decrease Indent';
  sdxRichEditEditStyleDialogDecrementIndentHint = 'Decrease Indent';
  sdxRichEditEditStyleDialogFontDialog = 'Font...';
  sdxRichEditEditStyleDialogForm = 'Modify Style';
  sdxRichEditEditStyleDialogFormatting = 'Formatting';
  sdxRichEditEditStyleDialogIncrementIndent = 'Increase Indent';
  sdxRichEditEditStyleDialogIncrementIndentHint = 'Increase Indent';
  sdxRichEditEditStyleDialogName = '&Name:';
  sdxRichEditEditStyleDialogParagraphDialog = 'Paragraph...';
  sdxRichEditEditStyleDialogProperties = 'Properties';
  sdxRichEditEditStyleDialogSelectedStyle = 'Select Style';
  sdxRichEditEditStyleDialogStyleBasedOn = 'Style &based on:';
  sdxRichEditEditStyleDialogStyleForFollowingParagraph = '&Style for following paragraph:';
  sdxRichEditEditStyleDialogTabsDialog = 'Tabs...';
  sdxRichEditEditStyleDialogToggleFontBold = '&Bold';
  sdxRichEditEditStyleDialogToggleFontBoldHint = 'Bold';
  sdxRichEditEditStyleDialogToggleFontItalic = '&Italic';
  sdxRichEditEditStyleDialogToggleFontItalicHint = 'Italic';
  sdxRichEditEditStyleDialogToggleFontUnderline = '&Underline';
  sdxRichEditEditStyleDialogToggleFontUnderlineHint = 'Underline';
  sdxRichEditEditStyleDialogToggleParagraphAlignmentCenter = '&Center';
  sdxRichEditEditStyleDialogToggleParagraphAlignmentCenterHint = 'Center';
  sdxRichEditEditStyleDialogToggleParagraphAlignmentJustify = '&Justify';
  sdxRichEditEditStyleDialogToggleParagraphAlignmentJustifyHint = 'Justify';
  sdxRichEditEditStyleDialogToggleParagraphAlignmentLeft = 'Align Text &Left';
  sdxRichEditEditStyleDialogToggleParagraphAlignmentLeftHint = 'Align Left';
  sdxRichEditEditStyleDialogToggleParagraphAlignmentRight = 'Align Text &Right';
  sdxRichEditEditStyleDialogToggleParagraphAlignmentRightHint = 'Align Right';

  sdxRichEditEditStyleDialogPreviousParagraphText = 'Previous Paragraph ';
  sdxRichEditEditStyleDialogCurrentParagraphText = 'Sample Text ';
  sdxRichEditEditStyleDialogFollowingParagraphText = 'Following Paragraph ';
  sdxRichEditEditStyleDialogEmptyParentStyle = '(underlying properties)';


  sdxRichEditInsertTableColumns = 'Number of &columns:';
  sdxRichEditInsertTableForm = 'Insert Table';
  sdxRichEditInsertTableRows = 'Number of &rows:';
  sdxRichEditInsertTableTableSize = 'Table Size';


  sdxRichEditHyperlinkSelectionInDocument = 'Selection in document';
  sdxRichEditHyperlinkSelectedBookmarkNone = '<None>';
  sdxRichEditHyperlinkDialogAddress = 'Address:';
  sdxRichEditHyperlinkDialogForm = 'Hyperlink';
  sdxRichEditHyperlinkDialogBookmark = 'B&ookmark:';
  sdxRichEditHyperlinkDialogLinkTo = 'Link to:';
  sdxRichEditHyperlinkDialogLinkToDocument = 'Place in this document';
  sdxRichEditHyperlinkDialogLinkToWebPage = 'Existing file or web page';
  sdxRichEditHyperlinkDialogTarget = 'Tar&get frame:';
  sdxRichEditHyperlinkDialogText = '&Text to display:';
  sdxRichEditHyperlinkDialogTooltip = 'ScreenTi&p:';

  sdxRichEditHyperlinkDialogSelectionInDocument = 'Selection in document';
  sdxRichEditHyperlinkDialogTargetFrameDescription_Blank = 'New window';
  sdxRichEditHyperlinkDialogTargetFrameDescription_Parent = 'Parent frame';
  sdxRichEditHyperlinkDialogTargetFrameDescription_Self = 'Same frame';
  sdxRichEditHyperlinkDialogTargetFrameDescription_Top = 'Whole page';


  sdxFloatingObjectLayoutFormDialog = 'Layout';
  sdxFloatingObjectLayoutFormDialogBottom = 'Botto&m';
  sdxFloatingObjectLayoutFormDialogButtonReset = 'Re&set';
  sdxFloatingObjectLayoutFormDialogDistance = 'Distance from text';
  sdxFloatingObjectLayoutFormDialogHeight = 'Height';
  sdxFloatingObjectLayoutFormDialogHeightAbsolute = 'Absolut&e:';
  sdxFloatingObjectLayoutFormDialogHorizontal = 'Horizontal';
  sdxFloatingObjectLayoutFormDialogHorizontalAbsolutePosition = '&to the right of';
  sdxFloatingObjectLayoutFormDialogHorizontalAbsolutePositionItem = 'Absolute &position';
  sdxFloatingObjectLayoutFormDialogHorizontalAlignmentItem = '&Alignment';
  sdxFloatingObjectLayoutFormDialogHorizontalPositionType = '&relative to';
  sdxFloatingObjectLayoutFormDialogLeft = 'L&eft';
  sdxFloatingObjectLayoutFormDialogLock = '&Lock anchor';
  sdxFloatingObjectLayoutFormDialogLockAspectRatio = 'Lock &aspect ratio';
  sdxFloatingObjectLayoutFormDialogOptions = 'Options';
  sdxFloatingObjectLayoutFormDialogOriginalSize = 'Original size';
  sdxFloatingObjectLayoutFormDialogOriginalSizeHeight = 'Height:';
  sdxFloatingObjectLayoutFormDialogOriginalSizeWidth = 'Width:';
  sdxFloatingObjectLayoutFormDialogPresetControlBehind = '&Behind text';
  sdxFloatingObjectLayoutFormDialogPresetControlInFrontOf = 'In &front of text';
  sdxFloatingObjectLayoutFormDialogPresetControlSquare = 'S&quare';
  sdxFloatingObjectLayoutFormDialogPresetControlThought = 'T&hrough';
  sdxFloatingObjectLayoutFormDialogPresetControlTight = '&Tight';
  sdxFloatingObjectLayoutFormDialogPresetControlTopAndBottom = 'T&op and bottom';
  sdxFloatingObjectLayoutFormDialogRight = 'Ri&ght';
  sdxFloatingObjectLayoutFormDialogRotate = 'Rotate';
  sdxFloatingObjectLayoutFormDialogRotation = 'Ro&tation:';
  sdxFloatingObjectLayoutFormDialogScale = 'Scale';
  sdxFloatingObjectLayoutFormDialogTabPagePosition = 'Position';
  sdxFloatingObjectLayoutFormDialogTabPageSize = 'Size';
  sdxFloatingObjectLayoutFormDialogTabPageTextWrapping = 'Text Wrapping';
  sdxFloatingObjectLayoutFormDialogTextWrapSideBothSides = 'Both &sides';
  sdxFloatingObjectLayoutFormDialogTextWrapSideLargestOnly = 'L&argest only';
  sdxFloatingObjectLayoutFormDialogTextWrapSideLeftOnly = '&Left only';
  sdxFloatingObjectLayoutFormDialogTextWrapSideRightOnly = '&Right only';
  sdxFloatingObjectLayoutFormDialogTop = 'To&p';
  sdxFloatingObjectLayoutFormDialogVertical = 'Vertical';
  sdxFloatingObjectLayoutFormDialogVerticalAbsolutePosition = 'belo&w';
  sdxFloatingObjectLayoutFormDialogVerticalAbsolutePositionItem = 'Absolute po&sition';
  sdxFloatingObjectLayoutFormDialogVerticalAlignmentItem = 'Ali&gnment';
  sdxFloatingObjectLayoutFormDialogVerticalPositionType = 'r&elative to';
  sdxFloatingObjectLayoutFormDialogWidth = 'Width';
  sdxFloatingObjectLayoutFormDialogWidthAbsolute = 'A&bsolute:';
  sdxFloatingObjectLayoutFormDialogWrappingStyle = 'Text Wrapping';
  sdxFloatingObjectLayoutFormDialogWrapText = 'Wrap text';

  sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeMargin = 'Margin';
  sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeCharacter = 'Character';
  sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeColumn = 'Column';
  sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeInsideMargin = 'Inside Margin';
  sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeLeftMargin = 'Left Margin';
  sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeOutsideMargin = 'Outside Margin';
  sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypePage = 'Page';
  sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeRightMargin = 'Right Margin';
  sdxFloatingObjectLayoutOptionsForm_HorizontalPositionAlignmentCenter = 'Centered';
  sdxFloatingObjectLayoutOptionsForm_HorizontalPositionAlignmentLeft = 'Left';
  sdxFloatingObjectLayoutOptionsForm_HorizontalPositionAlignmentRight = 'Right';

  sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeMargin = 'Margin';
  sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypePage = 'Page';
  sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeLine = 'Line';
  sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeTopMargin = 'Top Margin';
  sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeBottomMargin = 'Bottom Margin';
  sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeInsideMargin = 'Inside Margin';
  sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeOutsideMargin = 'Outside Margin';
  sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeParagraph = 'Paragraph';

  sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentTop = 'Top';
  sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentCenter = 'Center';
  sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentBottom = 'Bottom';
  sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentInside = 'Inside';
  sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentOutside = 'Outside';


  sdxRichEditPaperKindCustom = 'Custom';
  sdxRichEditPaperKindLetter = 'Letter';
  sdxRichEditPaperKindLetterSmall = 'Letter Small';
  sdxRichEditPaperKindTabloid = 'Tabloid';
  sdxRichEditPaperKindLedger = 'Ledger';
  sdxRichEditPaperKindLegal = 'Legal';
  sdxRichEditPaperKindStatement = 'Statement';
  sdxRichEditPaperKindExecutive = 'Executive';
  sdxRichEditPaperKindA3 = 'A3';
  sdxRichEditPaperKindA4 = 'A4';
  sdxRichEditPaperKindA4Small = 'A4 Small';
  sdxRichEditPaperKindA5 = 'A5';
  sdxRichEditPaperKindB4 = 'B4';
  sdxRichEditPaperKindB5 = 'B5';
  sdxRichEditPaperKindFolio = 'Folio';
  sdxRichEditPaperKindQuarto = 'Quarto';
  sdxRichEditPaperKindStandard10x14 = 'Standard 10x14';
  sdxRichEditPaperKindStandard11x17 = 'Standard 11x17';
  sdxRichEditPaperKindNote = 'Note';
  sdxRichEditPaperKindNumber9Envelope = 'Number 9 Envelope';
  sdxRichEditPaperKindNumber10Envelope = 'Number 10 Envelope';
  sdxRichEditPaperKindNumber11Envelope = 'Number 11 Envelope';
  sdxRichEditPaperKindNumber12Envelope = 'Number 12 Envelope';
  sdxRichEditPaperKindNumber14Envelope = 'Number 14 Envelope';
  sdxRichEditPaperKindCSheet = 'C Sheet';
  sdxRichEditPaperKindDSheet = 'D Sheet';
  sdxRichEditPaperKindESheet = 'E Sheet';
  sdxRichEditPaperKindDLEnvelope = 'DL Envelope';
  sdxRichEditPaperKindC5Envelope = 'C5 Envelope';
  sdxRichEditPaperKindC3Envelope = 'C3 Envelope';
  sdxRichEditPaperKindC4Envelope = 'C4 Envelope';
  sdxRichEditPaperKindC6Envelope = 'C6 Envelope';
  sdxRichEditPaperKindC65Envelope = 'C65 Envelope';
  sdxRichEditPaperKindB4Envelope = 'B4 Envelope';
  sdxRichEditPaperKindB5Envelope = 'B5 Envelope';
  sdxRichEditPaperKindB6Envelope = 'B6 Envelope';
  sdxRichEditPaperKindItalyEnvelope = 'Italy Envelope';
  sdxRichEditPaperKindMonarchEnvelope = 'Monarch Envelope';
  sdxRichEditPaperKindPersonalEnvelope = 'Personal Envelope (6 3/4)';
  sdxRichEditPaperKindUSStandardFanfold = 'US Standard Fanfold';
  sdxRichEditPaperKindGermanStandardFanfold = 'German Standard Fanfold';
  sdxRichEditPaperKindGermanLegalFanfold = 'German Legal Fanfold';
  sdxRichEditPaperKindIsoB4 = 'Iso B4';
  sdxRichEditPaperKindJapanesePostcard = 'Japanese Postcard';
  sdxRichEditPaperKindStandard9x11 = 'Standard 9x11';
  sdxRichEditPaperKindStandard10x11 = 'Standard 10x11';
  sdxRichEditPaperKindStandard15x11 = 'Standard 15x11';
  sdxRichEditPaperKindInviteEnvelope = 'Invite Envelope';
  sdxRichEditPaperKindLetterExtra = 'Letter Extra';
  sdxRichEditPaperKindLegalExtra = 'Legal Extra';
  sdxRichEditPaperKindTabloidExtra = 'Tabloid Extra';
  sdxRichEditPaperKindA4Extra = 'A4 Extra';
  sdxRichEditPaperKindLetterTransverse = 'Letter Transverse';
  sdxRichEditPaperKindA4Transverse = 'A4 Transverse';
  sdxRichEditPaperKindLetterExtraTransverse = 'Letter Extra Transverse';
  sdxRichEditPaperKindAPlus = 'SuperA/SuperA/A4';
  sdxRichEditPaperKindBPlus = 'SuperB/SuperB/A3';
  sdxRichEditPaperKindLetterPlus = 'Letter Plus';
  sdxRichEditPaperKindA4Plus = 'A4 Plus';
  sdxRichEditPaperKindA5Transverse = 'A5 Transverse';
  sdxRichEditPaperKindB5Transverse = 'JIS B5 Transverse';
  sdxRichEditPaperKindA3Extra = 'A3 Extra';
  sdxRichEditPaperKindA5Extra = 'A5 Extra';
  sdxRichEditPaperKindB5Extra = 'ISO B5 Extra';
  sdxRichEditPaperKindA2 = 'A2';
  sdxRichEditPaperKindA3Transverse = 'A3 Transverse';
  sdxRichEditPaperKindA3ExtraTransverse = 'A3 Extra Transverse';
  sdxRichEditPaperKindJapaneseDoublePostcard = 'Japanese Double Postcard';
  sdxRichEditPaperKindA6 = 'A6';
  sdxRichEditPaperKindJapaneseEnvelopeKakuNumber2 = 'Japanese Envelope Kaku Number 2';
  sdxRichEditPaperKindJapaneseEnvelopeKakuNumber3 = 'Japanese Envelope Kaku Number 3';
  sdxRichEditPaperKindJapaneseEnvelopeChouNumber3 = 'Japanese Envelope Chou Number 3';
  sdxRichEditPaperKindJapaneseEnvelopeChouNumber4 = 'Japanese Envelope Chou Number 4';
  sdxRichEditPaperKindLetterRotated = 'Letter Rotated';
  sdxRichEditPaperKindA3Rotated = 'A3 Rotated';
  sdxRichEditPaperKindA4Rotated = 'A4 Rotated';
  sdxRichEditPaperKindA5Rotated = 'A5 Rotated';
  sdxRichEditPaperKindB4JisRotated = 'JIS B4 Rotated';
  sdxRichEditPaperKindB5JisRotated = 'JIS B5 Rotated';
  sdxRichEditPaperKindJapanesePostcardRotated = 'Japanese Postcard Rotated';
  sdxRichEditPaperKindJapaneseDoublePostcardRotated = 'Japanese Double Postcard Rotated';
  sdxRichEditPaperKindA6Rotated = 'A6 Rotated';
  sdxRichEditPaperKindJapaneseEnvelopeKakuNumber2Rotated = 'Japanese Envelope Kaku Number 2 Rotated';
  sdxRichEditPaperKindJapaneseEnvelopeKakuNumber3Rotated = 'Japanese Envelope Kaku Number 3 Rotated';
  sdxRichEditPaperKindJapaneseEnvelopeChouNumber3Rotated = 'Japanese Envelope Chou Number 3 Rotated';
  sdxRichEditPaperKindJapaneseEnvelopeChouNumber4Rotated = 'Japanese Envelope Chou Number 4 Rotated';
  sdxRichEditPaperKindB6Jis = 'JIS B6';
  sdxRichEditPaperKindB6JisRotated = 'JIS B6 Rotated';
  sdxRichEditPaperKindStandard12x11 = 'Standard 12x11';
  sdxRichEditPaperKindJapaneseEnvelopeYouNumber4 = 'Japanese Envelope You Number 4';
  sdxRichEditPaperKindJapaneseEnvelopeYouNumber4Rotated = 'Japanese Envelope You Number 4 Rotated';
  sdxRichEditPaperKindPrc16K = 'Prc 16K';
  sdxRichEditPaperKindPrc32K = 'Prc 32K';
  sdxRichEditPaperKindPrc32KBig = 'Prc 32K Big';
  sdxRichEditPaperKindPrcEnvelopeNumber1 = 'Prc Envelope Number 1';
  sdxRichEditPaperKindPrcEnvelopeNumber2 = 'Prc Envelope Number 2';
  sdxRichEditPaperKindPrcEnvelopeNumber3 = 'Prc Envelope Number 3';
  sdxRichEditPaperKindPrcEnvelopeNumber4 = 'Prc Envelope Number 4';
  sdxRichEditPaperKindPrcEnvelopeNumber5 = 'Prc Envelope Number 5';
  sdxRichEditPaperKindPrcEnvelopeNumber6 = 'Prc Envelope Number 6';
  sdxRichEditPaperKindPrcEnvelopeNumber7 = 'Prc Envelope Number 7';
  sdxRichEditPaperKindPrcEnvelopeNumber8 = 'Prc Envelope Number 8';
  sdxRichEditPaperKindPrcEnvelopeNumber9 = 'Prc Envelope Number 9';
  sdxRichEditPaperKindPrcEnvelopeNumber10 = 'Prc Envelope Number 10';
  sdxRichEditPaperKindPrc16KRotated = 'Prc 16K Rotated';
  sdxRichEditPaperKindPrc32KRotated = 'Prc 32K Rotated';
  sdxRichEditPaperKindPrc32KBigRotated = 'Prc 32K Big Rotated';
  sdxRichEditPaperKindPrcEnvelopeNumber1Rotated = 'Prc Envelope Number 1 Rotated';
  sdxRichEditPaperKindPrcEnvelopeNumber2Rotated = 'Prc Envelope Number 2 Rotated';
  sdxRichEditPaperKindPrcEnvelopeNumber3Rotated = 'Prc Envelope Number 3 Rotated';
  sdxRichEditPaperKindPrcEnvelopeNumber4Rotated = 'Prc Envelope Number 4 Rotated';
  sdxRichEditPaperKindPrcEnvelopeNumber5Rotated = 'Prc Envelope Number 5 Rotated';
  sdxRichEditPaperKindPrcEnvelopeNumber6Rotated = 'Prc Envelope Number 6 Rotated';
  sdxRichEditPaperKindPrcEnvelopeNumber7Rotated = 'Prc Envelope Number 7 Rotated';
  sdxRichEditPaperKindPrcEnvelopeNumber8Rotated = 'Prc Envelope Number 8 Rotated';
  sdxRichEditPaperKindPrcEnvelopeNumber9Rotated = 'Prc Envelope Number 9 Rotated';
  sdxRichEditPaperKindPrcEnvelopeNumber10Rotated = 'Prc Envelope Number 10 Rotated';


  sdxRichEditInsertMergeFieldAddressFieldsSource = '&Address Fields';
  sdxRichEditInsertMergeFieldButtonCancel = 'Close';
  sdxRichEditInsertMergeFieldButtonInsert = '&Insert';
  sdxRichEditInsertMergeFieldDatabaseFieldsSource = '&Database Fields';
  sdxRichEditInsertMergeFieldFields = 'Fields:';
  sdxRichEditInsertMergeFieldForm = 'Insert Merge Field';
  sdxRichEditInsertMergeFieldInsert = 'Insert:';


  sdxRichEditMergeOptionsDialogForm = 'Merge Options';
  sdxRichEditMergeOptionsDialogMergeAllRecords = '&All';
  sdxRichEditMergeOptionsDialogMergeRecords = 'Merge records';
  sdxRichEditMergeOptionsDialogMergeSelectedRecords = '&Selected';
  sdxRichEditMergeOptionsDialogMergeTo = 'Merge to';
  sdxRichEditMergeOptionsDialogMergeToFile = '&File';
  sdxRichEditMergeOptionsDialogMergeToWindow = '&Window';


  sdxRichEditTableOfContentsEditShowLevels = 'Show levels:';
  sdxRichEditTableOfContentsForm = 'Table of Contents';
  sdxRichEditTableOfContentsPrintPreview = 'Print Preview';
  sdxRichEditTableOfContentsRightAlignPageNumbers = 'Right Align Page Numbers';
  sdxRichEditTableOfContentsShowPageNumbers = 'Show page numbers';
  sdxRichEditTableOfContentsUseHyperlinks = 'Use hyperlinks instead of page numbers';
  sdxRichEditTableOfContentsListParagraphContent = 'Heading';


  sdxRichEditTableStyleDialogApplyFormattingTo = '&Apply formatting to:';
  sdxRichEditTableStyleDialogButtonFormat = 'F&ormat';
  sdxRichEditTableStyleDialogCurrentStyle = 'Current style:';
  sdxRichEditTableStyleDialogFontDialog = 'Font...';
  sdxRichEditTableStyleDialogForm = 'Modify Style';
  sdxRichEditTableStyleDialogFormatting = 'Formatting';
  sdxRichEditTableStyleDialogName = '&Name:';
  sdxRichEditTableStyleDialogParagraphDialog = 'Paragraph...';
  sdxRichEditTableStyleDialogProperties = 'Properties';
  sdxRichEditTableStyleDialogResetTableCellsBorders = '&No Border';
  sdxRichEditTableStyleDialogSelectedStyle = 'Select Style';
  sdxRichEditTableStyleDialogStyleBasedOn = 'Style &based on:';
  sdxRichEditTableStyleDialogTabsDialog = 'Tabs...';
  sdxRichEditTableStyleDialogToggleFontBoldHint = 'Bold';
  sdxRichEditTableStyleDialogToggleFontItalicHint = 'Italic';
  sdxRichEditTableStyleDialogToggleFontUnderlineHint = 'Underline';
  sdxRichEditTableStyleDialogToggleTableCellsAllBorders = '&All Borders';
  sdxRichEditTableStyleDialogToggleTableCellsBottomBorder = '&Bottom Border';
  sdxRichEditTableStyleDialogToggleTableCellsBottomCenterAlignment = 'Align Bottom Center';
  sdxRichEditTableStyleDialogToggleTableCellsBottomLeftAlignment = 'Align Bottom Left';
  sdxRichEditTableStyleDialogToggleTableCellsBottomRightAlignment = 'Align Bottom Right';
  sdxRichEditTableStyleDialogToggleTableCellsInsideBorder = '&Inside Borders';
  sdxRichEditTableStyleDialogToggleTableCellsInsideHorizontalBorder = 'Inside &Horizontal Border';
  sdxRichEditTableStyleDialogToggleTableCellsInsideVerticalBorder = 'Inside &Vertical Border';
  sdxRichEditTableStyleDialogToggleTableCellsLeftBorder = '&Left Border';
  sdxRichEditTableStyleDialogToggleTableCellsMiddleCenterAlignment = 'Align Center';
  sdxRichEditTableStyleDialogToggleTableCellsMiddleLeftAlignment = 'Align Center Left';
  sdxRichEditTableStyleDialogToggleTableCellsMiddleRightAlignment = 'Align Center Right';
  sdxRichEditTableStyleDialogToggleTableCellsOutsideBorder = 'Out&side Borders';
  sdxRichEditTableStyleDialogToggleTableCellsRightBorder = '&Right Border';
  sdxRichEditTableStyleDialogToggleTableCellsTopBorder = 'To&p Border';
  sdxRichEditTableStyleDialogToggleTableCellsTopCenterAlignment = 'Align Top Center';
  sdxRichEditTableStyleDialogToggleTableCellsTopLeftAlignment = 'Align Top Left';
  sdxRichEditTableStyleDialogToggleTableCellsTopRightAlignment = 'Align Top Right';
  sdxRichEditTableStyleDialogPreviewTableColumn1 = 'Jan';
  sdxRichEditTableStyleDialogPreviewTableColumn2 = 'Feb';
  sdxRichEditTableStyleDialogPreviewTableColumn3 = 'Mar';
  sdxRichEditTableStyleDialogPreviewTableRow1 = 'East';
  sdxRichEditTableStyleDialogPreviewTableRow2 = 'West';
  sdxRichEditTableStyleDialogPreviewTableRow3 = 'South';
  sdxRichEditTableStyleDialogPreviewTableTotal = 'Total';

  sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_WholeTable = 'Whole table';
  sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_FirstRow = 'Header row';
  sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_LastRow = 'Total row';
  sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_FirstColumn = 'First column';
  sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_LastColumn = 'Last column';
  sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_OddRowBanding = 'Odd banded rows';
  sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_EvenRowBanding = 'Even banded rows';
  sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_OddColumnBanding = 'Odd banded columns';
  sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_EvenColumnBanding = 'Even banded columns';
  sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_TopLeftCell = 'Top left cell';
  sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_TopRightCell = 'Top right cell';
  sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_BottomLeftCell = 'Bottom left cell';
  sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_BottomRightCell = 'Bottom right cell';


  sdxDocumentProtectionQueryPasswordForm = 'Unprotect Document';


  sdxDocumentProtectionQueryNewPasswordForm = 'Start Enforcing Protection';

  sdxDocumentEncryptionQueryNewPasswordForm = 'Encrypt the contents of this file';


  sdxRangeEditingPermissionsButtonApply = 'Apply';
  sdxRangeEditingPermissionsForm = 'Editing Permissions';
  sdxRangeEditingPermissionsGroups = 'Groups:';
  sdxRangeEditingPermissionsUsers = 'Users:';

  sdxRangeEditingPermissionsMoreUsers = 'More users...';
  sdxRangeEditingPermissionsAddUsers = 'Add users';
  sdxRangeEditingPermissionsEnterUserNames = 'Enter user names, separated by semicolons:';
  sdxRangeEditingPermissionsInvalidUserNames = 'Some of the users you have entered could not be added to the list because their names could not be verified.';


  sdxQueryPasswordForm = 'Enter password';
  sdxQueryPasswordPassword = 'Password:';

  sdxQueryNewPasswordForm = 'Enter password';
  sdxQueryNewPasswordPassword = '&Enter new password (optional):';
  sdxQueryNewPasswordRepeatPassword = 'Reenter &password to confirm:';
  sdxQueryNewPasswordInvalidPasswordConfirmation = 'The passwords do not match.';

procedure AddRichEditDialogsResourceStringNames(AProduct: TdxProductResourceStrings);

implementation

uses
  dxRichEdit.Strs;

procedure AddRichEditDialogsResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxRichEditTabForm_All', @sdxRichEditTabForm_All);

  AProduct.Add('sdxRichEditDialogButtonOK', @sdxRichEditDialogButtonOK);
  AProduct.Add('sdxRichEditDialogButtonCancel', @sdxRichEditDialogButtonCancel);


  AProduct.Add('sdxRichEditInvalidSize', @sdxRichEditInvalidSize);
  AProduct.Add('sdxRichEditInvalidMeasurementValue', @sdxRichEditInvalidMeasurementValue);
  AProduct.Add('sdxRichEditInvalidNumber', @sdxRichEditInvalidNumber);
  AProduct.Add('sdxRichEditInvalidMeasurement', @sdxRichEditInvalidMeasurement);


  AProduct.Add('sdxSectionPropertiesApplyToWholeDocument', @sdxSectionPropertiesApplyToWholeDocument);
  AProduct.Add('sdxSectionPropertiesApplyToCurrentSection', @sdxSectionPropertiesApplyToCurrentSection);
  AProduct.Add('sdxSectionPropertiesApplyToSelectedSections', @sdxSectionPropertiesApplyToSelectedSections);
  AProduct.Add('sdxSectionPropertiesApplyThisPointForward', @sdxSectionPropertiesApplyThisPointForward);


  AProduct.Add('sdxRichEditParagraphDialogAfter', @sdxRichEditParagraphDialogAfter);
  AProduct.Add('sdxRichEditParagraphDialogAlignment', @sdxRichEditParagraphDialogAlignment);
  AProduct.Add('sdxRichEditParagraphDialogAt', @sdxRichEditParagraphDialogAt);
  AProduct.Add('sdxRichEditParagraphDialogBefore', @sdxRichEditParagraphDialogBefore);
  AProduct.Add('sdxRichEditParagraphDialogButtonTabs', @sdxRichEditParagraphDialogButtonTabs);
  AProduct.Add('sdxRichEditParagraphDialogBy', @sdxRichEditParagraphDialogBy);
  AProduct.Add('sdxRichEditParagraphDialogDontAddSpace', @sdxRichEditParagraphDialogDontAddSpace);
  AProduct.Add('sdxRichEditParagraphDialogForm', @sdxRichEditParagraphDialogForm);
  AProduct.Add('sdxRichEditParagraphDialogGeneral', @sdxRichEditParagraphDialogGeneral);
  AProduct.Add('sdxRichEditParagraphDialogIndentation', @sdxRichEditParagraphDialogIndentation);
  AProduct.Add('sdxRichEditParagraphDialogIndentsAndSpacing', @sdxRichEditParagraphDialogIndentsAndSpacing);
  AProduct.Add('sdxRichEditParagraphDialogKeepLinesTogether', @sdxRichEditParagraphDialogKeepLinesTogether);
  AProduct.Add('sdxRichEditParagraphDialogLeft', @sdxRichEditParagraphDialogLeft);
  AProduct.Add('sdxRichEditParagraphDialogLineAndPageBreaks', @sdxRichEditParagraphDialogLineAndPageBreaks);
  AProduct.Add('sdxRichEditParagraphDialogLineSpacing', @sdxRichEditParagraphDialogLineSpacing);
  AProduct.Add('sdxRichEditParagraphDialogOutlinelevel', @sdxRichEditParagraphDialogOutlinelevel);
  AProduct.Add('sdxRichEditParagraphDialogPageBreakBefore', @sdxRichEditParagraphDialogPageBreakBefore);
  AProduct.Add('sdxRichEditParagraphDialogPagination', @sdxRichEditParagraphDialogPagination);
  AProduct.Add('sdxRichEditParagraphDialogRight', @sdxRichEditParagraphDialogRight);
  AProduct.Add('sdxRichEditParagraphDialogSpacing', @sdxRichEditParagraphDialogSpacing);
  AProduct.Add('sdxRichEditParagraphDialogSpecial', @sdxRichEditParagraphDialogSpecial);


  AProduct.Add('sdxParagraphAlignmentLeft', @sdxParagraphAlignmentLeft);
  AProduct.Add('sdxParagraphAlignmentRight', @sdxParagraphAlignmentRight);
  AProduct.Add('sdxParagraphAlignmentCenter', @sdxParagraphAlignmentCenter);
  AProduct.Add('sdxParagraphAlignmentJustify', @sdxParagraphAlignmentJustify);

  AProduct.Add('sdxParagraphLineSpacingSingle', @sdxParagraphLineSpacingSingle);
  AProduct.Add('sdxParagraphLineSpacingSesquialteral', @sdxParagraphLineSpacingSesquialteral);
  AProduct.Add('sdxParagraphLineSpacingDouble', @sdxParagraphLineSpacingDouble);
  AProduct.Add('sdxParagraphLineSpacingMultiple', @sdxParagraphLineSpacingMultiple);
  AProduct.Add('sdxParagraphLineSpacingExactly', @sdxParagraphLineSpacingExactly);
  AProduct.Add('sdxParagraphLineSpacingAtLeast', @sdxParagraphLineSpacingAtLeast);

  AProduct.Add('sdxParagraphFirstLineIndentNone', @sdxParagraphFirstLineIndentNone);
  AProduct.Add('sdxParagraphFirstLineIndentIndented', @sdxParagraphFirstLineIndentIndented);
  AProduct.Add('sdxParagraphFirstLineIndentHanging', @sdxParagraphFirstLineIndentHanging);

  AProduct.Add('sdxParagraphOutlineLeve0', @sdxParagraphOutlineLeve0);
  AProduct.Add('sdxParagraphOutlineLeve1', @sdxParagraphOutlineLeve1);
  AProduct.Add('sdxParagraphOutlineLeve2', @sdxParagraphOutlineLeve2);
  AProduct.Add('sdxParagraphOutlineLeve3', @sdxParagraphOutlineLeve3);
  AProduct.Add('sdxParagraphOutlineLeve4', @sdxParagraphOutlineLeve4);
  AProduct.Add('sdxParagraphOutlineLeve5', @sdxParagraphOutlineLeve5);
  AProduct.Add('sdxParagraphOutlineLeve6', @sdxParagraphOutlineLeve6);
  AProduct.Add('sdxParagraphOutlineLeve7', @sdxParagraphOutlineLeve7);
  AProduct.Add('sdxParagraphOutlineLeve8', @sdxParagraphOutlineLeve8);
  AProduct.Add('sdxParagraphOutlineLeve9', @sdxParagraphOutlineLeve9);

  AProduct.Add('sdxRichEditTabsDialogAlignment', @sdxRichEditTabsDialogAlignment);
  AProduct.Add('sdxRichEditTabsDialogButtonClear', @sdxRichEditTabsDialogButtonClear);
  AProduct.Add('sdxRichEditTabsDialogButtonClearAll', @sdxRichEditTabsDialogButtonClearAll);
  AProduct.Add('sdxRichEditTabsDialogButtonSet', @sdxRichEditTabsDialogButtonSet);
  AProduct.Add('sdxRichEditTabsDialogCenter', @sdxRichEditTabsDialogCenter);
  AProduct.Add('sdxRichEditTabsDialogDecimal', @sdxRichEditTabsDialogDecimal);
  AProduct.Add('sdxRichEditTabsDialogDefaultTabStops', @sdxRichEditTabsDialogDefaultTabStops);
  AProduct.Add('sdxRichEditTabsDialogDots', @sdxRichEditTabsDialogDots);
  AProduct.Add('sdxRichEditTabsDialogEqualSign', @sdxRichEditTabsDialogEqualSign);
  AProduct.Add('sdxRichEditTabsDialogForm', @sdxRichEditTabsDialogForm);
  AProduct.Add('sdxRichEditTabsDialogHyphens', @sdxRichEditTabsDialogHyphens);
  AProduct.Add('sdxRichEditTabsDialogLeader', @sdxRichEditTabsDialogLeader);
  AProduct.Add('sdxRichEditTabsDialogLeft', @sdxRichEditTabsDialogLeft);
  AProduct.Add('sdxRichEditTabsDialogMiddleDots', @sdxRichEditTabsDialogMiddleDots);
  AProduct.Add('sdxRichEditTabsDialogNone', @sdxRichEditTabsDialogNone);
  AProduct.Add('sdxRichEditTabsDialogRight', @sdxRichEditTabsDialogRight);
  AProduct.Add('sdxRichEditTabsDialogTabStopPosition', @sdxRichEditTabsDialogTabStopPosition);
  AProduct.Add('sdxRichEditTabsDialogTabStopsToBeCleared', @sdxRichEditTabsDialogTabStopsToBeCleared);
  AProduct.Add('sdxRichEditTabsDialogThickLine', @sdxRichEditTabsDialogThickLine);
  AProduct.Add('sdxRichEditTabsDialogUnderline', @sdxRichEditTabsDialogUnderline);


  AProduct.Add('sdxRichEditNumberingListDialogBulleted', @sdxRichEditNumberingListDialogBulleted);
  AProduct.Add('sdxRichEditNumberingListDialogButtonCustomize', @sdxRichEditNumberingListDialogButtonCustomize);
  AProduct.Add('sdxRichEditNumberingListDialogForm', @sdxRichEditNumberingListDialogForm);
  AProduct.Add('sdxRichEditNumberingListDialogNumbered', @sdxRichEditNumberingListDialogNumbered);
  AProduct.Add('sdxRichEditNumberingListDialogOutlineNumbered', @sdxRichEditNumberingListDialogOutlineNumbered);
  AProduct.Add('sdxRichEditNumberingListDialogContinuePreviousList', @sdxRichEditNumberingListDialogContinuePreviousList);
  AProduct.Add('sdxRichEditNumberingListDialogRestartNumbering', @sdxRichEditNumberingListDialogRestartNumbering);


  AProduct.Add('sdxRichEditBulletedListDialogAlignedAt', @sdxRichEditBulletedListDialogAlignedAt);
  AProduct.Add('sdxRichEditBulletedListDialogBulletCharacter', @sdxRichEditBulletedListDialogBulletCharacter);
  AProduct.Add('sdxRichEditBulletedListDialogBulletPosition', @sdxRichEditBulletedListDialogBulletPosition);
  AProduct.Add('sdxRichEditBulletedListDialogButtonCharacter', @sdxRichEditBulletedListDialogButtonCharacter);
  AProduct.Add('sdxRichEditBulletedListDialogForm', @sdxRichEditBulletedListDialogForm);
  AProduct.Add('sdxRichEditBulletedListDialogIndentAt', @sdxRichEditBulletedListDialogIndentAt);
  AProduct.Add('sdxRichEditBulletedListDialogTextPosition', @sdxRichEditBulletedListDialogTextPosition);


  AProduct.Add('sdxRichEditFontDialogAllCaps', @sdxRichEditFontDialogAllCaps);
  AProduct.Add('sdxRichEditFontDialogDoubleStrikeout', @sdxRichEditFontDialogDoubleStrikeout);
  AProduct.Add('sdxRichEditFontDialogEffects', @sdxRichEditFontDialogEffects);
  AProduct.Add('sdxRichEditFontDialogFontColor', @sdxRichEditFontDialogFontColor);
  AProduct.Add('sdxRichEditFontDialogFontName', @sdxRichEditFontDialogFontName);
  AProduct.Add('sdxRichEditFontDialogFontSize', @sdxRichEditFontDialogFontSize);
  AProduct.Add('sdxRichEditFontDialogFontStyle', @sdxRichEditFontDialogFontStyle);
  AProduct.Add('sdxRichEditFontDialogForm', @sdxRichEditFontDialogForm);
  AProduct.Add('sdxRichEditFontDialogHidden', @sdxRichEditFontDialogHidden);
  AProduct.Add('sdxRichEditFontDialogPreview', @sdxRichEditFontDialogPreview);
  AProduct.Add('sdxRichEditFontDialogStrikeout', @sdxRichEditFontDialogStrikeout);
  AProduct.Add('sdxRichEditFontDialogSubscript', @sdxRichEditFontDialogSubscript);
  AProduct.Add('sdxRichEditFontDialogSuperscript', @sdxRichEditFontDialogSuperscript);
  AProduct.Add('sdxRichEditFontDialogUnderlineColor', @sdxRichEditFontDialogUnderlineColor);
  AProduct.Add('sdxRichEditFontDialogUnderlineStyle', @sdxRichEditFontDialogUnderlineStyle);
  AProduct.Add('sdxRichEditFontDialogUnderlineWordsOnly', @sdxRichEditFontDialogUnderlineWordsOnly);

  AProduct.Add('sdxRichEditFontDialogFontStyleRegular', @sdxRichEditFontDialogFontStyleRegular);
  AProduct.Add('sdxRichEditFontDialogFontStyleItalic', @sdxRichEditFontDialogFontStyleItalic);
  AProduct.Add('sdxRichEditFontDialogFontStyleBold', @sdxRichEditFontDialogFontStyleBold);
  AProduct.Add('sdxRichEditFontDialogFontStyleBoldItalic', @sdxRichEditFontDialogFontStyleBoldItalic);

  AProduct.Add('sdxRichEditFontDialogUnderlineStyleNone', @sdxRichEditFontDialogUnderlineStyleNone);
  AProduct.Add('sdxRichEditFontDialogUnderlineStyleSingle', @sdxRichEditFontDialogUnderlineStyleSingle);
  AProduct.Add('sdxRichEditFontDialogUnderlineStyleDouble', @sdxRichEditFontDialogUnderlineStyleDouble);

  AProduct.Add('sdxRichEditFontDialogButtonColorAuto', @sdxRichEditFontDialogButtonColorAuto);

  AProduct.Add('sdxRichEditFontDialogFontNotInstalled', @sdxRichEditFontDialogFontNotInstalled);
  AProduct.Add('sdxRichEditFontDialogPrintNotes', @sdxRichEditFontDialogPrintNotes);
  AProduct.Add('sdxRichEditFontDialogFontStyleImitated', @sdxRichEditFontDialogFontStyleImitated);


  AProduct.Add('sdxRichEditNumberingListBoxNone', @sdxRichEditNumberingListBoxNone);

  AProduct.Add('sdxRichEditCustomNumberingListAlignedAt', @sdxRichEditCustomNumberingListAlignedAt);
  AProduct.Add('sdxRichEditCustomNumberingListButtonFont', @sdxRichEditCustomNumberingListButtonFont);
  AProduct.Add('sdxRichEditCustomNumberingListDisplayFormat', @sdxRichEditCustomNumberingListDisplayFormat);
  AProduct.Add('sdxRichEditCustomNumberingListIndentAt', @sdxRichEditCustomNumberingListIndentAt);
  AProduct.Add('sdxRichEditCustomNumberingListNumberFormat', @sdxRichEditCustomNumberingListNumberFormat);
  AProduct.Add('sdxRichEditCustomNumberingListNumberPosition', @sdxRichEditCustomNumberingListNumberPosition);
  AProduct.Add('sdxRichEditCustomNumberingListNumberStyle', @sdxRichEditCustomNumberingListNumberStyle);
  AProduct.Add('sdxRichEditCustomNumberingListStartAt', @sdxRichEditCustomNumberingListStartAt);
  AProduct.Add('sdxRichEditCustomNumberingListTextPosition', @sdxRichEditCustomNumberingListTextPosition);


  AProduct.Add('sdxRichEditSimpleNumberingListDialogForm', @sdxRichEditSimpleNumberingListDialogForm);


  AProduct.Add('sdxRichEditMultiLevelNumberingListDialogFollowNumberWith', @sdxRichEditMultiLevelNumberingListDialogFollowNumberWith);
  AProduct.Add('sdxRichEditMultiLevelNumberingListDialogForm', @sdxRichEditMultiLevelNumberingListDialogForm);
  AProduct.Add('sdxRichEditMultiLevelNumberingListDialogLevel', @sdxRichEditMultiLevelNumberingListDialogLevel);

  AProduct.Add('sdxRichEditMultiLevelNumberingListFollowNumberTabCharacter', @sdxRichEditMultiLevelNumberingListFollowNumberTabCharacter);
  AProduct.Add('sdxRichEditMultiLevelNumberingListFollowNumberSpace', @sdxRichEditMultiLevelNumberingListFollowNumberSpace);
  AProduct.Add('sdxRichEditMultiLevelNumberingListFollowNumberNothing', @sdxRichEditMultiLevelNumberingListFollowNumberNothing);


  AProduct.Add('sdxRichEditSearchTextDialogButtonFindNext', @sdxRichEditSearchTextDialogButtonFindNext);
  AProduct.Add('sdxRichEditSearchTextDialogButtonReplaceAll', @sdxRichEditSearchTextDialogButtonReplaceAll);
  AProduct.Add('sdxRichEditSearchTextDialogButtonReplaceNext', @sdxRichEditSearchTextDialogButtonReplaceNext);
  AProduct.Add('sdxRichEditSearchTextDialogFind', @sdxRichEditSearchTextDialogFind);
  AProduct.Add('sdxRichEditSearchTextDialogForm', @sdxRichEditSearchTextDialogForm);
  AProduct.Add('sdxRichEditSearchTextDialogReplace', @sdxRichEditSearchTextDialogReplace);
  AProduct.Add('sdxRichEditSearchTextDialogRplReplaceString', @sdxRichEditSearchTextDialogRplReplaceString);
  AProduct.Add('sdxRichEditSearchTextDialogSearchString', @sdxRichEditSearchTextDialogSearchString);
  AProduct.Add('sdxRichEditSearchTextDialogDirection', @sdxRichEditSearchTextDialogDirection);
  AProduct.Add('sdxRichEditSearchTextDialogMatchCase', @sdxRichEditSearchTextDialogMatchCase);
  AProduct.Add('sdxRichEditSearchTextDialogFindWholeWord', @sdxRichEditSearchTextDialogFindWholeWord);
  AProduct.Add('sdxRichEditSearchTextDialogRegex', @sdxRichEditSearchTextDialogRegex);

  AProduct.Add('sdxRichEditSearchTextDialogDirectionAll', @sdxRichEditSearchTextDialogDirectionAll);
  AProduct.Add('sdxRichEditSearchTextDialogDirectionDown', @sdxRichEditSearchTextDialogDirectionDown);
  AProduct.Add('sdxRichEditSearchTextDialogDirectionUp', @sdxRichEditSearchTextDialogDirectionUp);

  AProduct.Add('sdxRichEditSearchTextDialogAnySingleCharacter', @sdxRichEditSearchTextDialogAnySingleCharacter);
  AProduct.Add('sdxRichEditSearchTextDialogZeroOrMore', @sdxRichEditSearchTextDialogZeroOrMore);
  AProduct.Add('sdxRichEditSearchTextDialogOneOrMore', @sdxRichEditSearchTextDialogOneOrMore);
  AProduct.Add('sdxRichEditSearchTextDialogBeginningOfLine', @sdxRichEditSearchTextDialogBeginningOfLine);
  AProduct.Add('sdxRichEditSearchTextDialogEndOfLine', @sdxRichEditSearchTextDialogEndOfLine);
  AProduct.Add('sdxRichEditSearchTextDialogBeginningOfWord', @sdxRichEditSearchTextDialogBeginningOfWord);
  AProduct.Add('sdxRichEditSearchTextDialogEndOfWord', @sdxRichEditSearchTextDialogEndOfWord);
  AProduct.Add('sdxRichEditSearchTextDialogAnyOneCharacterInTheSet', @sdxRichEditSearchTextDialogAnyOneCharacterInTheSet);
  AProduct.Add('sdxRichEditSearchTextDialogAnyOneCharacterNotInTheSet', @sdxRichEditSearchTextDialogAnyOneCharacterNotInTheSet);
  AProduct.Add('sdxRichEditSearchTextDialogOr', @sdxRichEditSearchTextDialogOr);
  AProduct.Add('sdxRichEditSearchTextDialogEscapeSpecialCharacter', @sdxRichEditSearchTextDialogEscapeSpecialCharacter);
  AProduct.Add('sdxRichEditSearchTextDialogTagExpression', @sdxRichEditSearchTextDialogTagExpression);
  AProduct.Add('sdxRichEditSearchTextDialogWordCharacter', @sdxRichEditSearchTextDialogWordCharacter);
  AProduct.Add('sdxRichEditSearchTextDialogSpaceOrTab', @sdxRichEditSearchTextDialogSpaceOrTab);
  AProduct.Add('sdxRichEditSearchTextDialogInteger', @sdxRichEditSearchTextDialogInteger);

  AProduct.Add('sdxRichEditSearchTextDialogTaggedExpression', @sdxRichEditSearchTextDialogTaggedExpression);


  AProduct.Add('sdxRichEditSplitTableCellsDialogForm', @sdxRichEditSplitTableCellsDialogForm);
  AProduct.Add('sdxRichEditSplitTableCellsDialogMergeBeforeSplit', @sdxRichEditSplitTableCellsDialogMergeBeforeSplit);
  AProduct.Add('sdxRichEditSplitTableCellsDialogNumberOfColumns', @sdxRichEditSplitTableCellsDialogNumberOfColumns);
  AProduct.Add('sdxRichEditSplitTableCellsDialogNumberOfRows', @sdxRichEditSplitTableCellsDialogNumberOfRows);


  AProduct.Add('sdxRichEditInsertTableCellsDialogCellOperationDeleteColumn', @sdxRichEditInsertTableCellsDialogCellOperationDeleteColumn);
  AProduct.Add('sdxRichEditInsertTableCellsDialogCellOperationDeleteRow', @sdxRichEditInsertTableCellsDialogCellOperationDeleteRow);
  AProduct.Add('sdxRichEditInsertTableCellsDialogCellOperationShiftLeft', @sdxRichEditInsertTableCellsDialogCellOperationShiftLeft);
  AProduct.Add('sdxRichEditInsertTableCellsDialogCellOperationShiftUp', @sdxRichEditInsertTableCellsDialogCellOperationShiftUp);
  AProduct.Add('sdxRichEditInsertTableCellsDialogForm', @sdxRichEditInsertTableCellsDialogForm);


  AProduct.Add('sdxRichEditDeleteTableCellsDialogCellOperationDeleteColumn', @sdxRichEditDeleteTableCellsDialogCellOperationDeleteColumn);
  AProduct.Add('sdxRichEditDeleteTableCellsDialogCellOperationDeleteRow', @sdxRichEditDeleteTableCellsDialogCellOperationDeleteRow);
  AProduct.Add('sdxRichEditDeleteTableCellsDialogCellOperationShiftLeft', @sdxRichEditDeleteTableCellsDialogCellOperationShiftLeft);
  AProduct.Add('sdxRichEditDeleteTableCellsDialogCellOperationShiftUp', @sdxRichEditDeleteTableCellsDialogCellOperationShiftUp);
  AProduct.Add('sdxRichEditDeleteTableCellsDialogForm', @sdxRichEditDeleteTableCellsDialogForm);


  AProduct.Add('sdxRichEditTablePropertiesDialogButtonBorder', @sdxRichEditTablePropertiesDialogButtonBorder);
  AProduct.Add('sdxRichEditTablePropertiesDialogButtonCellOptions', @sdxRichEditTablePropertiesDialogButtonCellOptions);
  AProduct.Add('sdxRichEditTablePropertiesDialogButtonNextColumn', @sdxRichEditTablePropertiesDialogButtonNextColumn);
  AProduct.Add('sdxRichEditTablePropertiesDialogButtonNextRow', @sdxRichEditTablePropertiesDialogButtonNextRow);
  AProduct.Add('sdxRichEditTablePropertiesDialogButtonPreviousColumn', @sdxRichEditTablePropertiesDialogButtonPreviousColumn);
  AProduct.Add('sdxRichEditTablePropertiesDialogButtonPreviousRow', @sdxRichEditTablePropertiesDialogButtonPreviousRow);
  AProduct.Add('sdxRichEditTablePropertiesDialogButtonTableOptions', @sdxRichEditTablePropertiesDialogButtonTableOptions);
  AProduct.Add('sdxRichEditTablePropertiesDialogCantSplit', @sdxRichEditTablePropertiesDialogCantSplit);
  AProduct.Add('sdxRichEditTablePropertiesDialogCell', @sdxRichEditTablePropertiesDialogCell);
  AProduct.Add('sdxRichEditTablePropertiesDialogPreferredWidth', @sdxRichEditTablePropertiesDialogPreferredWidth);
  AProduct.Add('sdxRichEditTablePropertiesDialogSize', @sdxRichEditTablePropertiesDialogSize);
  AProduct.Add('sdxRichEditTablePropertiesDialogCellVerticalAlighment', @sdxRichEditTablePropertiesDialogCellVerticalAlighment);
  AProduct.Add('sdxRichEditTablePropertiesDialogCellVerticalAlignmentBottom', @sdxRichEditTablePropertiesDialogCellVerticalAlignmentBottom);
  AProduct.Add('sdxRichEditTablePropertiesDialogCellVerticalAlignmentCenter', @sdxRichEditTablePropertiesDialogCellVerticalAlignmentCenter);
  AProduct.Add('sdxRichEditTablePropertiesDialogCellVerticalAlignmentTop', @sdxRichEditTablePropertiesDialogCellVerticalAlignmentTop);
  AProduct.Add('sdxRichEditTablePropertiesDialogWidthType', @sdxRichEditTablePropertiesDialogWidthType);
  AProduct.Add('sdxRichEditTablePropertiesDialogColumn', @sdxRichEditTablePropertiesDialogColumn);
  AProduct.Add('sdxRichEditTablePropertiesDialogColumnNumber', @sdxRichEditTablePropertiesDialogColumnNumber);
  AProduct.Add('sdxRichEditTablePropertiesDialogForm', @sdxRichEditTablePropertiesDialogForm);
  AProduct.Add('sdxRichEditTablePropertiesDialogHeader', @sdxRichEditTablePropertiesDialogHeader);
  AProduct.Add('sdxRichEditTablePropertiesDialogIndentFromLeft', @sdxRichEditTablePropertiesDialogIndentFromLeft);
  AProduct.Add('sdxRichEditTablePropertiesDialogRow', @sdxRichEditTablePropertiesDialogRow);
  AProduct.Add('sdxRichEditTablePropertiesDialogRowHeightType', @sdxRichEditTablePropertiesDialogRowHeightType);
  AProduct.Add('sdxRichEditTablePropertiesDialogRowNumber', @sdxRichEditTablePropertiesDialogRowNumber);
  AProduct.Add('sdxRichEditTablePropertiesDialogRowOptions', @sdxRichEditTablePropertiesDialogRowOptions);
  AProduct.Add('sdxRichEditTablePropertiesDialogSize', @sdxRichEditTablePropertiesDialogSize);
  AProduct.Add('sdxRichEditTablePropertiesDialogSpecifyHeight', @sdxRichEditTablePropertiesDialogSpecifyHeight);
  AProduct.Add('sdxRichEditTablePropertiesDialogTable', @sdxRichEditTablePropertiesDialogTable);
  AProduct.Add('sdxRichEditTablePropertiesDialogTableAlignmenCenter', @sdxRichEditTablePropertiesDialogTableAlignmenCenter);
  AProduct.Add('sdxRichEditTablePropertiesDialogTableAlignmenRight', @sdxRichEditTablePropertiesDialogTableAlignmenRight);
  AProduct.Add('sdxRichEditTablePropertiesDialogTableAlignment', @sdxRichEditTablePropertiesDialogTableAlignment);
  AProduct.Add('sdxRichEditTablePropertiesDialogTableAlignmentLeft', @sdxRichEditTablePropertiesDialogTableAlignmentLeft);

  AProduct.Add('sdxRichEditTablePropertiesHeightTypeExact', @sdxRichEditTablePropertiesHeightTypeExact);
  AProduct.Add('sdxRichEditTablePropertiesHeightTypeMinimum', @sdxRichEditTablePropertiesHeightTypeMinimum);


  AProduct.Add('sdxRichEditCustomTableOptionsDialogBottomMargin', @sdxRichEditCustomTableOptionsDialogBottomMargin);
  AProduct.Add('sdxRichEditCustomTableOptionsDialogLeftMargin', @sdxRichEditCustomTableOptionsDialogLeftMargin);
  AProduct.Add('sdxRichEditCustomTableOptionsDialogRightMargin', @sdxRichEditCustomTableOptionsDialogRightMargin);
  AProduct.Add('sdxRichEditCustomTableOptionsDialogTopMargin', @sdxRichEditCustomTableOptionsDialogTopMargin);


  AProduct.Add('sdxRichEditTableOptionsDialogAllowCellSpacing', @sdxRichEditTableOptionsDialogAllowCellSpacing);
  AProduct.Add('sdxRichEditTableOptionsDialogDefaultCellSpacing', @sdxRichEditTableOptionsDialogDefaultCellSpacing);
  AProduct.Add('sdxRichEditTableOptionsDialogForm', @sdxRichEditTableOptionsDialogForm);
  AProduct.Add('sdxRichEditTableOptionsDialogMargins', @sdxRichEditTableOptionsDialogMargins);
  AProduct.Add('sdxRichEditTableOptionsDialogOptions', @sdxRichEditTableOptionsDialogOptions);
  AProduct.Add('sdxRichEditTableOptionsDialogResizeToFitContent', @sdxRichEditTableOptionsDialogResizeToFitContent);


  AProduct.Add('sdxRichEditTableCellOptionsDialogFitText', @sdxRichEditTableCellOptionsDialogFitText);
  AProduct.Add('sdxRichEditTableCellOptionsDialogForm', @sdxRichEditTableCellOptionsDialogForm);
  AProduct.Add('sdxRichEditTableCellOptionsDialogMargins', @sdxRichEditTableCellOptionsDialogMargins);
  AProduct.Add('sdxRichEditTableCellOptionsDialogOptions', @sdxRichEditTableCellOptionsDialogOptions);
  AProduct.Add('sdxRichEditTableCellOptionsDialogSameAsWholeTable', @sdxRichEditTableCellOptionsDialogSameAsWholeTable);
  AProduct.Add('sdxRichEditTableCellOptionsDialogWrapText', @sdxRichEditTableCellOptionsDialogWrapText);


  AProduct.Add('sdxRichEditBorderShadingDialogAll', @sdxRichEditBorderShadingDialogAll);
  AProduct.Add('sdxRichEditBorderShadingDialogApplyTo', @sdxRichEditBorderShadingDialogApplyTo);
  AProduct.Add('sdxRichEditBorderShadingDialogBorderLineColor', @sdxRichEditBorderShadingDialogBorderLineColor);
  AProduct.Add('sdxRichEditBorderShadingDialogBorderLineStyle', @sdxRichEditBorderShadingDialogBorderLineStyle);
  AProduct.Add('sdxRichEditBorderShadingDialogBorderLineWeight', @sdxRichEditBorderShadingDialogBorderLineWeight);
  AProduct.Add('sdxRichEditBorderShadingDialogBorders', @sdxRichEditBorderShadingDialogBorders);
  AProduct.Add('sdxRichEditBorderShadingDialogBox', @sdxRichEditBorderShadingDialogBox);
  AProduct.Add('sdxRichEditBorderShadingDialogButtonOptions', @sdxRichEditBorderShadingDialogButtonOptions);
  AProduct.Add('sdxRichEditBorderShadingDialogCustom', @sdxRichEditBorderShadingDialogCustom);
  AProduct.Add('sdxRichEditBorderShadingDialogForm', @sdxRichEditBorderShadingDialogForm);
  AProduct.Add('sdxRichEditBorderShadingDialogGrid', @sdxRichEditBorderShadingDialogGrid);
  AProduct.Add('sdxRichEditBorderShadingDialogNone', @sdxRichEditBorderShadingDialogNone);
  AProduct.Add('sdxRichEditBorderShadingDialogShading', @sdxRichEditBorderShadingDialogShading);
  AProduct.Add('sdxRichEditBorderShadingDialogShadingFill', @sdxRichEditBorderShadingDialogShadingFill);
  AProduct.Add('sdxRichEditBorderShadingDialogPreviewTxt', @sdxRichEditBorderShadingDialogPreviewTxt);
  AProduct.Add('sdxRichEditBorderShadingDialogPreview', @sdxRichEditBorderShadingDialogPreview);

  AProduct.Add('sdxRichEditBorderShadingDialogApplyToCell', @sdxRichEditBorderShadingDialogApplyToCell);
  AProduct.Add('sdxRichEditBorderShadingDialogApplyToTable', @sdxRichEditBorderShadingDialogApplyToTable);

  AProduct.Add('sdxRichEditBorderLineStyleNone', @sdxRichEditBorderLineStyleNone);


  AProduct.Add('sdxRichEditLineNumberingDialogAddLineNumbering', @sdxRichEditLineNumberingDialogAddLineNumbering);
  AProduct.Add('sdxRichEditLineNumberingDialogCountBy', @sdxRichEditLineNumberingDialogCountBy);
  AProduct.Add('sdxRichEditLineNumberingDialogForm', @sdxRichEditLineNumberingDialogForm);
  AProduct.Add('sdxRichEditLineNumberingDialogFromText', @sdxRichEditLineNumberingDialogFromText);
  AProduct.Add('sdxRichEditLineNumberingDialogNumbering', @sdxRichEditLineNumberingDialogNumbering);
  AProduct.Add('sdxRichEditLineNumberingDialogNumberingRestartContinuous', @sdxRichEditLineNumberingDialogNumberingRestartContinuous);
  AProduct.Add('sdxRichEditLineNumberingDialogNumberingRestartEachPage', @sdxRichEditLineNumberingDialogNumberingRestartEachPage);
  AProduct.Add('sdxRichEditLineNumberingDialogNumberingRestartEachSection', @sdxRichEditLineNumberingDialogNumberingRestartEachSection);
  AProduct.Add('sdxRichEditLineNumberingDialogStartAt', @sdxRichEditLineNumberingDialogStartAt);


  AProduct.Add('sdxRichEditColumnsSetupDialogApplyTo', @sdxRichEditColumnsSetupDialogApplyTo);
  AProduct.Add('sdxRichEditColumnsSetupDialogColumnCount', @sdxRichEditColumnsSetupDialogColumnCount);
  AProduct.Add('sdxRichEditColumnsSetupDialogColumnNumber', @sdxRichEditColumnsSetupDialogColumnNumber);
  AProduct.Add('sdxRichEditColumnsSetupDialogColumnsPresetLeft', @sdxRichEditColumnsSetupDialogColumnsPresetLeft);
  AProduct.Add('sdxRichEditColumnsSetupDialogColumnsPresetOne', @sdxRichEditColumnsSetupDialogColumnsPresetOne);
  AProduct.Add('sdxRichEditColumnsSetupDialogColumnsPresetRight', @sdxRichEditColumnsSetupDialogColumnsPresetRight);
  AProduct.Add('sdxRichEditColumnsSetupDialogColumnsPresetThree', @sdxRichEditColumnsSetupDialogColumnsPresetThree);
  AProduct.Add('sdxRichEditColumnsSetupDialogColumnsPresetTwo', @sdxRichEditColumnsSetupDialogColumnsPresetTwo);
  AProduct.Add('sdxRichEditColumnsSetupDialogEqualColumnWidth', @sdxRichEditColumnsSetupDialogEqualColumnWidth);
  AProduct.Add('sdxRichEditColumnsSetupDialogForm', @sdxRichEditColumnsSetupDialogForm);
  AProduct.Add('sdxRichEditColumnsSetupDialogLineBetween', @sdxRichEditColumnsSetupDialogLineBetween);
  AProduct.Add('sdxRichEditColumnsSetupDialogPresets', @sdxRichEditColumnsSetupDialogPresets);
  AProduct.Add('sdxRichEditColumnsSetupDialogSpacing', @sdxRichEditColumnsSetupDialogSpacing);
  AProduct.Add('sdxRichEditColumnsSetupDialogStartNewColumn', @sdxRichEditColumnsSetupDialogStartNewColumn);
  AProduct.Add('sdxRichEditColumnsSetupDialogWidth', @sdxRichEditColumnsSetupDialogWidth);
  AProduct.Add('sdxRichEditColumnsSetupDialogWidthSpacing', @sdxRichEditColumnsSetupDialogWidthSpacing);


  AProduct.Add('sdxRichEditPageSetupDialogApplyTo', @sdxRichEditPageSetupDialogApplyTo);
  AProduct.Add('sdxRichEditPageSetupDialogDifferentFirstPage', @sdxRichEditPageSetupDialogDifferentFirstPage);
  AProduct.Add('sdxRichEditPageSetupDialogDifferentOddAndEvenPage', @sdxRichEditPageSetupDialogDifferentOddAndEvenPage);
  AProduct.Add('sdxRichEditPageSetupDialogForm', @sdxRichEditPageSetupDialogForm);
  AProduct.Add('sdxRichEditPageSetupDialogHeadersAndFooters', @sdxRichEditPageSetupDialogHeadersAndFooters);
  AProduct.Add('sdxRichEditPageSetupDialogLandscape', @sdxRichEditPageSetupDialogLandscape);
  AProduct.Add('sdxRichEditPageSetupDialogMarginBottom', @sdxRichEditPageSetupDialogMarginBottom);
  AProduct.Add('sdxRichEditPageSetupDialogMarginLeft', @sdxRichEditPageSetupDialogMarginLeft);
  AProduct.Add('sdxRichEditPageSetupDialogMarginRight', @sdxRichEditPageSetupDialogMarginRight);
  AProduct.Add('sdxRichEditPageSetupDialogMargins', @sdxRichEditPageSetupDialogMargins);
  AProduct.Add('sdxRichEditPageSetupDialogMarginTop', @sdxRichEditPageSetupDialogMarginTop);
  AProduct.Add('sdxRichEditPageSetupDialogOrientation', @sdxRichEditPageSetupDialogOrientation);
  AProduct.Add('sdxRichEditPageSetupDialogPageLayout', @sdxRichEditPageSetupDialogPageLayout);
  AProduct.Add('sdxRichEditPageSetupDialogPageMargins', @sdxRichEditPageSetupDialogPageMargins);
  AProduct.Add('sdxRichEditPageSetupDialogPagePaper', @sdxRichEditPageSetupDialogPagePaper);
  AProduct.Add('sdxRichEditPageSetupDialogPaperHeight', @sdxRichEditPageSetupDialogPaperHeight);
  AProduct.Add('sdxRichEditPageSetupDialogPaperSize', @sdxRichEditPageSetupDialogPaperSize);
  AProduct.Add('sdxRichEditPageSetupDialogPaperWidth', @sdxRichEditPageSetupDialogPaperWidth);
  AProduct.Add('sdxRichEditPageSetupDialogPortrait', @sdxRichEditPageSetupDialogPortrait);
  AProduct.Add('sdxRichEditPageSetupDialogSection', @sdxRichEditPageSetupDialogSection);
  AProduct.Add('sdxRichEditPageSetupDialogSectionStart', @sdxRichEditPageSetupDialogSectionStart);

  AProduct.Add('sdxRichEditPageSetupSectionStartContinuous', @sdxRichEditPageSetupSectionStartContinuous);
  AProduct.Add('sdxRichEditPageSetupSectionStartColumn', @sdxRichEditPageSetupSectionStartColumn);
  AProduct.Add('sdxRichEditPageSetupSectionStartNextPage', @sdxRichEditPageSetupSectionStartNextPage);
  AProduct.Add('sdxRichEditPageSetupSectionStartOddPage', @sdxRichEditPageSetupSectionStartOddPage);
  AProduct.Add('sdxRichEditPageSetupSectionStartEvenPage', @sdxRichEditPageSetupSectionStartEvenPage);


  AProduct.Add('sdxRichEditSymbolDialogFont', @sdxRichEditSymbolDialogFont);
  AProduct.Add('sdxRichEditSymbolDialogForm', @sdxRichEditSymbolDialogForm);
  AProduct.Add('sdxRichEditSymbolDialogCharacterCode', @sdxRichEditSymbolDialogCharacterCode);


  AProduct.Add('sdxRichEditBookmarkDialogBookmarkName', @sdxRichEditBookmarkDialogBookmarkName);
  AProduct.Add('sdxRichEditBookmarkDialogButtonAdd', @sdxRichEditBookmarkDialogButtonAdd);
  AProduct.Add('sdxRichEditBookmarkDialogButtonCancel', @sdxRichEditBookmarkDialogButtonCancel);
  AProduct.Add('sdxRichEditBookmarkDialogButtonDelete', @sdxRichEditBookmarkDialogButtonDelete);
  AProduct.Add('sdxRichEditBookmarkDialogButtonGoTo', @sdxRichEditBookmarkDialogButtonGoTo);
  AProduct.Add('sdxRichEditBookmarkDialogForm', @sdxRichEditBookmarkDialogForm);
  AProduct.Add('sdxRichEditBookmarkDialogSortBy', @sdxRichEditBookmarkDialogSortBy);
  AProduct.Add('sdxRichEditBookmarkDialogSortByLocation', @sdxRichEditBookmarkDialogSortByLocation);
  AProduct.Add('sdxRichEditBookmarkDialogSortByName', @sdxRichEditBookmarkDialogSortByName);


  AProduct.Add('sdxRichEditEditStyleDialogButtonFormat', @sdxRichEditEditStyleDialogButtonFormat);
  AProduct.Add('sdxRichEditEditStyleDialogCurrentStyle', @sdxRichEditEditStyleDialogCurrentStyle);
  AProduct.Add('sdxRichEditEditStyleDialogDecrementIndent', @sdxRichEditEditStyleDialogDecrementIndent);
  AProduct.Add('sdxRichEditEditStyleDialogDecrementIndentHint', @sdxRichEditEditStyleDialogDecrementIndentHint);
  AProduct.Add('sdxRichEditEditStyleDialogFontDialog', @sdxRichEditEditStyleDialogFontDialog);
  AProduct.Add('sdxRichEditEditStyleDialogForm', @sdxRichEditEditStyleDialogForm);
  AProduct.Add('sdxRichEditEditStyleDialogFormatting', @sdxRichEditEditStyleDialogFormatting);
  AProduct.Add('sdxRichEditEditStyleDialogIncrementIndent', @sdxRichEditEditStyleDialogIncrementIndent);
  AProduct.Add('sdxRichEditEditStyleDialogIncrementIndentHint', @sdxRichEditEditStyleDialogIncrementIndentHint);
  AProduct.Add('sdxRichEditEditStyleDialogName', @sdxRichEditEditStyleDialogName);
  AProduct.Add('sdxRichEditEditStyleDialogParagraphDialog', @sdxRichEditEditStyleDialogParagraphDialog);
  AProduct.Add('sdxRichEditEditStyleDialogProperties', @sdxRichEditEditStyleDialogProperties);
  AProduct.Add('sdxRichEditEditStyleDialogSelectedStyle', @sdxRichEditEditStyleDialogSelectedStyle);
  AProduct.Add('sdxRichEditEditStyleDialogStyleBasedOn', @sdxRichEditEditStyleDialogStyleBasedOn);
  AProduct.Add('sdxRichEditEditStyleDialogStyleForFollowingParagraph', @sdxRichEditEditStyleDialogStyleForFollowingParagraph);
  AProduct.Add('sdxRichEditEditStyleDialogTabsDialog', @sdxRichEditEditStyleDialogTabsDialog);
  AProduct.Add('sdxRichEditEditStyleDialogToggleFontBold', @sdxRichEditEditStyleDialogToggleFontBold);
  AProduct.Add('sdxRichEditEditStyleDialogToggleFontBoldHint', @sdxRichEditEditStyleDialogToggleFontBoldHint);
  AProduct.Add('sdxRichEditEditStyleDialogToggleFontItalic', @sdxRichEditEditStyleDialogToggleFontItalic);
  AProduct.Add('sdxRichEditEditStyleDialogToggleFontItalicHint', @sdxRichEditEditStyleDialogToggleFontItalicHint);
  AProduct.Add('sdxRichEditEditStyleDialogToggleFontUnderline', @sdxRichEditEditStyleDialogToggleFontUnderline);
  AProduct.Add('sdxRichEditEditStyleDialogToggleFontUnderlineHint', @sdxRichEditEditStyleDialogToggleFontUnderlineHint);
  AProduct.Add('sdxRichEditEditStyleDialogToggleParagraphAlignmentCenter', @sdxRichEditEditStyleDialogToggleParagraphAlignmentCenter);
  AProduct.Add('sdxRichEditEditStyleDialogToggleParagraphAlignmentCenterHint', @sdxRichEditEditStyleDialogToggleParagraphAlignmentCenterHint);
  AProduct.Add('sdxRichEditEditStyleDialogToggleParagraphAlignmentJustify', @sdxRichEditEditStyleDialogToggleParagraphAlignmentJustify);
  AProduct.Add('sdxRichEditEditStyleDialogToggleParagraphAlignmentJustifyHint', @sdxRichEditEditStyleDialogToggleParagraphAlignmentJustifyHint);
  AProduct.Add('sdxRichEditEditStyleDialogToggleParagraphAlignmentLeft', @sdxRichEditEditStyleDialogToggleParagraphAlignmentLeft);
  AProduct.Add('sdxRichEditEditStyleDialogToggleParagraphAlignmentLeftHint', @sdxRichEditEditStyleDialogToggleParagraphAlignmentLeftHint);
  AProduct.Add('sdxRichEditEditStyleDialogToggleParagraphAlignmentRight', @sdxRichEditEditStyleDialogToggleParagraphAlignmentRight);
  AProduct.Add('sdxRichEditEditStyleDialogToggleParagraphAlignmentRightHint', @sdxRichEditEditStyleDialogToggleParagraphAlignmentRightHint);

  AProduct.Add('sdxRichEditEditStyleDialogPreviousParagraphText', @sdxRichEditEditStyleDialogPreviousParagraphText);
  AProduct.Add('sdxRichEditEditStyleDialogCurrentParagraphText', @sdxRichEditEditStyleDialogCurrentParagraphText);
  AProduct.Add('sdxRichEditEditStyleDialogFollowingParagraphText', @sdxRichEditEditStyleDialogFollowingParagraphText);
  AProduct.Add('sdxRichEditEditStyleDialogEmptyParentStyle', @sdxRichEditEditStyleDialogEmptyParentStyle);


  AProduct.Add('sdxRichEditInsertTableColumns', @sdxRichEditInsertTableColumns);
  AProduct.Add('sdxRichEditInsertTableForm', @sdxRichEditInsertTableForm);
  AProduct.Add('sdxRichEditInsertTableRows', @sdxRichEditInsertTableRows);
  AProduct.Add('sdxRichEditInsertTableTableSize', @sdxRichEditInsertTableTableSize);


  AProduct.Add('sdxRichEditHyperlinkSelectionInDocument', @sdxRichEditHyperlinkSelectionInDocument);
  AProduct.Add('sdxRichEditHyperlinkSelectedBookmarkNone', @sdxRichEditHyperlinkSelectedBookmarkNone);
  AProduct.Add('sdxRichEditHyperlinkDialogAddress', @sdxRichEditHyperlinkDialogAddress);
  AProduct.Add('sdxRichEditHyperlinkDialogForm', @sdxRichEditHyperlinkDialogForm);
  AProduct.Add('sdxRichEditHyperlinkDialogBookmark', @sdxRichEditHyperlinkDialogBookmark);
  AProduct.Add('sdxRichEditHyperlinkDialogLinkTo', @sdxRichEditHyperlinkDialogLinkTo);
  AProduct.Add('sdxRichEditHyperlinkDialogLinkToDocument', @sdxRichEditHyperlinkDialogLinkToDocument);
  AProduct.Add('sdxRichEditHyperlinkDialogLinkToWebPage', @sdxRichEditHyperlinkDialogLinkToWebPage);
  AProduct.Add('sdxRichEditHyperlinkDialogTarget', @sdxRichEditHyperlinkDialogTarget);
  AProduct.Add('sdxRichEditHyperlinkDialogText', @sdxRichEditHyperlinkDialogText);
  AProduct.Add('sdxRichEditHyperlinkDialogTooltip', @sdxRichEditHyperlinkDialogTooltip);

  AProduct.Add('sdxRichEditHyperlinkDialogSelectionInDocument', @sdxRichEditHyperlinkDialogSelectionInDocument);
  AProduct.Add('sdxRichEditHyperlinkDialogTargetFrameDescription_Blank', @sdxRichEditHyperlinkDialogTargetFrameDescription_Blank);
  AProduct.Add('sdxRichEditHyperlinkDialogTargetFrameDescription_Parent', @sdxRichEditHyperlinkDialogTargetFrameDescription_Parent);
  AProduct.Add('sdxRichEditHyperlinkDialogTargetFrameDescription_Self', @sdxRichEditHyperlinkDialogTargetFrameDescription_Self);
  AProduct.Add('sdxRichEditHyperlinkDialogTargetFrameDescription_Top', @sdxRichEditHyperlinkDialogTargetFrameDescription_Top);


  AProduct.Add('sdxFloatingObjectLayoutFormDialog', @sdxFloatingObjectLayoutFormDialog);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogBottom', @sdxFloatingObjectLayoutFormDialogBottom);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogButtonReset', @sdxFloatingObjectLayoutFormDialogButtonReset);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogDistance', @sdxFloatingObjectLayoutFormDialogDistance);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogHeight', @sdxFloatingObjectLayoutFormDialogHeight);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogHeightAbsolute', @sdxFloatingObjectLayoutFormDialogHeightAbsolute);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogHorizontal', @sdxFloatingObjectLayoutFormDialogHorizontal);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogHorizontalAbsolutePosition', @sdxFloatingObjectLayoutFormDialogHorizontalAbsolutePosition);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogHorizontalAbsolutePositionItem', @sdxFloatingObjectLayoutFormDialogHorizontalAbsolutePositionItem);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogHorizontalAlignmentItem', @sdxFloatingObjectLayoutFormDialogHorizontalAlignmentItem);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogHorizontalPositionType', @sdxFloatingObjectLayoutFormDialogHorizontalPositionType);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogLeft', @sdxFloatingObjectLayoutFormDialogLeft);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogLock', @sdxFloatingObjectLayoutFormDialogLock);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogLockAspectRatio', @sdxFloatingObjectLayoutFormDialogLockAspectRatio);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogOptions', @sdxFloatingObjectLayoutFormDialogOptions);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogOriginalSize', @sdxFloatingObjectLayoutFormDialogOriginalSize);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogOriginalSizeHeight', @sdxFloatingObjectLayoutFormDialogOriginalSizeHeight);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogOriginalSizeWidth', @sdxFloatingObjectLayoutFormDialogOriginalSizeWidth);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogPresetControlBehind', @sdxFloatingObjectLayoutFormDialogPresetControlBehind);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogPresetControlInFrontOf', @sdxFloatingObjectLayoutFormDialogPresetControlInFrontOf);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogPresetControlSquare', @sdxFloatingObjectLayoutFormDialogPresetControlSquare);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogPresetControlThought', @sdxFloatingObjectLayoutFormDialogPresetControlThought);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogPresetControlTight', @sdxFloatingObjectLayoutFormDialogPresetControlTight);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogPresetControlTopAndBottom', @sdxFloatingObjectLayoutFormDialogPresetControlTopAndBottom);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogRight', @sdxFloatingObjectLayoutFormDialogRight);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogRotate', @sdxFloatingObjectLayoutFormDialogRotate);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogRotation', @sdxFloatingObjectLayoutFormDialogRotation);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogScale', @sdxFloatingObjectLayoutFormDialogScale);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogTabPagePosition', @sdxFloatingObjectLayoutFormDialogTabPagePosition);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogTabPageSize', @sdxFloatingObjectLayoutFormDialogTabPageSize);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogTabPageTextWrapping', @sdxFloatingObjectLayoutFormDialogTabPageTextWrapping);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogTextWrapSideBothSides', @sdxFloatingObjectLayoutFormDialogTextWrapSideBothSides);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogTextWrapSideLargestOnly', @sdxFloatingObjectLayoutFormDialogTextWrapSideLargestOnly);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogTextWrapSideLeftOnly', @sdxFloatingObjectLayoutFormDialogTextWrapSideLeftOnly);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogTextWrapSideRightOnly', @sdxFloatingObjectLayoutFormDialogTextWrapSideRightOnly);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogTop', @sdxFloatingObjectLayoutFormDialogTop);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogVertical', @sdxFloatingObjectLayoutFormDialogVertical);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogVerticalAbsolutePosition', @sdxFloatingObjectLayoutFormDialogVerticalAbsolutePosition);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogVerticalAbsolutePositionItem', @sdxFloatingObjectLayoutFormDialogVerticalAbsolutePositionItem);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogVerticalAlignmentItem', @sdxFloatingObjectLayoutFormDialogVerticalAlignmentItem);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogVerticalPositionType', @sdxFloatingObjectLayoutFormDialogVerticalPositionType);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogWidth', @sdxFloatingObjectLayoutFormDialogWidth);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogWidthAbsolute', @sdxFloatingObjectLayoutFormDialogWidthAbsolute);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogWrappingStyle', @sdxFloatingObjectLayoutFormDialogWrappingStyle);
  AProduct.Add('sdxFloatingObjectLayoutFormDialogWrapText', @sdxFloatingObjectLayoutFormDialogWrapText);

  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeMargin', @sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeMargin);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeCharacter', @sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeCharacter);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeColumn', @sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeColumn);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeInsideMargin', @sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeInsideMargin);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeLeftMargin', @sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeLeftMargin);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeOutsideMargin', @sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeOutsideMargin);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypePage', @sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypePage);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeRightMargin', @sdxFloatingObjectLayoutOptionsForm_HorizontalPositionTypeRightMargin);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_HorizontalPositionAlignmentCenter', @sdxFloatingObjectLayoutOptionsForm_HorizontalPositionAlignmentCenter);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_HorizontalPositionAlignmentLeft', @sdxFloatingObjectLayoutOptionsForm_HorizontalPositionAlignmentLeft);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_HorizontalPositionAlignmentRight', @sdxFloatingObjectLayoutOptionsForm_HorizontalPositionAlignmentRight);

  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeMargin', @sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeMargin);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypePage', @sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypePage);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeLine', @sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeLine);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeTopMargin', @sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeTopMargin);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeBottomMargin', @sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeBottomMargin);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeInsideMargin', @sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeInsideMargin);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeOutsideMargin', @sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeOutsideMargin);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeParagraph', @sdxFloatingObjectLayoutOptionsForm_VerticalPositionTypeParagraph);

  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentTop', @sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentTop);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentCenter', @sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentCenter);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentBottom', @sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentBottom);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentInside', @sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentInside);
  AProduct.Add('sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentOutside', @sdxFloatingObjectLayoutOptionsForm_VerticalPositionAlignmentOutside);


  AProduct.Add('sdxRichEditPaperKindCustom', @sdxRichEditPaperKindCustom);
  AProduct.Add('sdxRichEditPaperKindLetter', @sdxRichEditPaperKindLetter);
  AProduct.Add('sdxRichEditPaperKindLetterSmall', @sdxRichEditPaperKindLetterSmall);
  AProduct.Add('sdxRichEditPaperKindTabloid', @sdxRichEditPaperKindTabloid);
  AProduct.Add('sdxRichEditPaperKindLedger', @sdxRichEditPaperKindLedger);
  AProduct.Add('sdxRichEditPaperKindLegal', @sdxRichEditPaperKindLegal);
  AProduct.Add('sdxRichEditPaperKindStatement', @sdxRichEditPaperKindStatement);
  AProduct.Add('sdxRichEditPaperKindExecutive', @sdxRichEditPaperKindExecutive);
  AProduct.Add('sdxRichEditPaperKindA3', @sdxRichEditPaperKindA3);
  AProduct.Add('sdxRichEditPaperKindA4', @sdxRichEditPaperKindA4);
  AProduct.Add('sdxRichEditPaperKindA4Small', @sdxRichEditPaperKindA4Small);
  AProduct.Add('sdxRichEditPaperKindA5', @sdxRichEditPaperKindA5);
  AProduct.Add('sdxRichEditPaperKindB4', @sdxRichEditPaperKindB4);
  AProduct.Add('sdxRichEditPaperKindB5', @sdxRichEditPaperKindB5);
  AProduct.Add('sdxRichEditPaperKindFolio', @sdxRichEditPaperKindFolio);
  AProduct.Add('sdxRichEditPaperKindQuarto', @sdxRichEditPaperKindQuarto);
  AProduct.Add('sdxRichEditPaperKindStandard10x14', @sdxRichEditPaperKindStandard10x14);
  AProduct.Add('sdxRichEditPaperKindStandard11x17', @sdxRichEditPaperKindStandard11x17);
  AProduct.Add('sdxRichEditPaperKindNote', @sdxRichEditPaperKindNote);
  AProduct.Add('sdxRichEditPaperKindNumber9Envelope', @sdxRichEditPaperKindNumber9Envelope);
  AProduct.Add('sdxRichEditPaperKindNumber10Envelope', @sdxRichEditPaperKindNumber10Envelope);
  AProduct.Add('sdxRichEditPaperKindNumber11Envelope', @sdxRichEditPaperKindNumber11Envelope);
  AProduct.Add('sdxRichEditPaperKindNumber12Envelope', @sdxRichEditPaperKindNumber12Envelope);
  AProduct.Add('sdxRichEditPaperKindNumber14Envelope', @sdxRichEditPaperKindNumber14Envelope);
  AProduct.Add('sdxRichEditPaperKindCSheet', @sdxRichEditPaperKindCSheet);
  AProduct.Add('sdxRichEditPaperKindDSheet', @sdxRichEditPaperKindDSheet);
  AProduct.Add('sdxRichEditPaperKindESheet', @sdxRichEditPaperKindESheet);
  AProduct.Add('sdxRichEditPaperKindDLEnvelope', @sdxRichEditPaperKindDLEnvelope);
  AProduct.Add('sdxRichEditPaperKindC5Envelope', @sdxRichEditPaperKindC5Envelope);
  AProduct.Add('sdxRichEditPaperKindC3Envelope', @sdxRichEditPaperKindC3Envelope);
  AProduct.Add('sdxRichEditPaperKindC4Envelope', @sdxRichEditPaperKindC4Envelope);
  AProduct.Add('sdxRichEditPaperKindC6Envelope', @sdxRichEditPaperKindC6Envelope);
  AProduct.Add('sdxRichEditPaperKindC65Envelope', @sdxRichEditPaperKindC65Envelope);
  AProduct.Add('sdxRichEditPaperKindB4Envelope', @sdxRichEditPaperKindB4Envelope);
  AProduct.Add('sdxRichEditPaperKindB5Envelope', @sdxRichEditPaperKindB5Envelope);
  AProduct.Add('sdxRichEditPaperKindB6Envelope', @sdxRichEditPaperKindB6Envelope);
  AProduct.Add('sdxRichEditPaperKindItalyEnvelope', @sdxRichEditPaperKindItalyEnvelope);
  AProduct.Add('sdxRichEditPaperKindMonarchEnvelope', @sdxRichEditPaperKindMonarchEnvelope);
  AProduct.Add('sdxRichEditPaperKindPersonalEnvelope', @sdxRichEditPaperKindPersonalEnvelope);
  AProduct.Add('sdxRichEditPaperKindUSStandardFanfold', @sdxRichEditPaperKindUSStandardFanfold);
  AProduct.Add('sdxRichEditPaperKindGermanStandardFanfold', @sdxRichEditPaperKindGermanStandardFanfold);
  AProduct.Add('sdxRichEditPaperKindGermanLegalFanfold', @sdxRichEditPaperKindGermanLegalFanfold);
  AProduct.Add('sdxRichEditPaperKindIsoB4', @sdxRichEditPaperKindIsoB4);
  AProduct.Add('sdxRichEditPaperKindJapanesePostcard', @sdxRichEditPaperKindJapanesePostcard);
  AProduct.Add('sdxRichEditPaperKindStandard9x11', @sdxRichEditPaperKindStandard9x11);
  AProduct.Add('sdxRichEditPaperKindStandard10x11', @sdxRichEditPaperKindStandard10x11);
  AProduct.Add('sdxRichEditPaperKindStandard15x11', @sdxRichEditPaperKindStandard15x11);
  AProduct.Add('sdxRichEditPaperKindInviteEnvelope', @sdxRichEditPaperKindInviteEnvelope);
  AProduct.Add('sdxRichEditPaperKindLetterExtra', @sdxRichEditPaperKindLetterExtra);
  AProduct.Add('sdxRichEditPaperKindLegalExtra', @sdxRichEditPaperKindLegalExtra);
  AProduct.Add('sdxRichEditPaperKindTabloidExtra', @sdxRichEditPaperKindTabloidExtra);
  AProduct.Add('sdxRichEditPaperKindA4Extra', @sdxRichEditPaperKindA4Extra);
  AProduct.Add('sdxRichEditPaperKindLetterTransverse', @sdxRichEditPaperKindLetterTransverse);
  AProduct.Add('sdxRichEditPaperKindA4Transverse', @sdxRichEditPaperKindA4Transverse);
  AProduct.Add('sdxRichEditPaperKindLetterExtraTransverse', @sdxRichEditPaperKindLetterExtraTransverse);
  AProduct.Add('sdxRichEditPaperKindAPlus', @sdxRichEditPaperKindAPlus);
  AProduct.Add('sdxRichEditPaperKindBPlus', @sdxRichEditPaperKindBPlus);
  AProduct.Add('sdxRichEditPaperKindLetterPlus', @sdxRichEditPaperKindLetterPlus);
  AProduct.Add('sdxRichEditPaperKindA4Plus', @sdxRichEditPaperKindA4Plus);
  AProduct.Add('sdxRichEditPaperKindA5Transverse', @sdxRichEditPaperKindA5Transverse);
  AProduct.Add('sdxRichEditPaperKindB5Transverse', @sdxRichEditPaperKindB5Transverse);
  AProduct.Add('sdxRichEditPaperKindA3Extra', @sdxRichEditPaperKindA3Extra);
  AProduct.Add('sdxRichEditPaperKindA5Extra', @sdxRichEditPaperKindA5Extra);
  AProduct.Add('sdxRichEditPaperKindB5Extra', @sdxRichEditPaperKindB5Extra);
  AProduct.Add('sdxRichEditPaperKindA2', @sdxRichEditPaperKindA2);
  AProduct.Add('sdxRichEditPaperKindA3Transverse', @sdxRichEditPaperKindA3Transverse);
  AProduct.Add('sdxRichEditPaperKindA3ExtraTransverse', @sdxRichEditPaperKindA3ExtraTransverse);
  AProduct.Add('sdxRichEditPaperKindJapaneseDoublePostcard', @sdxRichEditPaperKindJapaneseDoublePostcard);
  AProduct.Add('sdxRichEditPaperKindA6', @sdxRichEditPaperKindA6);
  AProduct.Add('sdxRichEditPaperKindJapaneseEnvelopeKakuNumber2', @sdxRichEditPaperKindJapaneseEnvelopeKakuNumber2);
  AProduct.Add('sdxRichEditPaperKindJapaneseEnvelopeKakuNumber3', @sdxRichEditPaperKindJapaneseEnvelopeKakuNumber3);
  AProduct.Add('sdxRichEditPaperKindJapaneseEnvelopeChouNumber3', @sdxRichEditPaperKindJapaneseEnvelopeChouNumber3);
  AProduct.Add('sdxRichEditPaperKindJapaneseEnvelopeChouNumber4', @sdxRichEditPaperKindJapaneseEnvelopeChouNumber4);
  AProduct.Add('sdxRichEditPaperKindLetterRotated', @sdxRichEditPaperKindLetterRotated);
  AProduct.Add('sdxRichEditPaperKindA3Rotated', @sdxRichEditPaperKindA3Rotated);
  AProduct.Add('sdxRichEditPaperKindA4Rotated', @sdxRichEditPaperKindA4Rotated);
  AProduct.Add('sdxRichEditPaperKindA5Rotated', @sdxRichEditPaperKindA5Rotated);
  AProduct.Add('sdxRichEditPaperKindB4JisRotated', @sdxRichEditPaperKindB4JisRotated);
  AProduct.Add('sdxRichEditPaperKindB5JisRotated', @sdxRichEditPaperKindB5JisRotated);
  AProduct.Add('sdxRichEditPaperKindJapanesePostcardRotated', @sdxRichEditPaperKindJapanesePostcardRotated);
  AProduct.Add('sdxRichEditPaperKindJapaneseDoublePostcardRotated', @sdxRichEditPaperKindJapaneseDoublePostcardRotated);
  AProduct.Add('sdxRichEditPaperKindA6Rotated', @sdxRichEditPaperKindA6Rotated);
  AProduct.Add('sdxRichEditPaperKindJapaneseEnvelopeKakuNumber2Rotated', @sdxRichEditPaperKindJapaneseEnvelopeKakuNumber2Rotated);
  AProduct.Add('sdxRichEditPaperKindJapaneseEnvelopeKakuNumber3Rotated', @sdxRichEditPaperKindJapaneseEnvelopeKakuNumber3Rotated);
  AProduct.Add('sdxRichEditPaperKindJapaneseEnvelopeChouNumber3Rotated', @sdxRichEditPaperKindJapaneseEnvelopeChouNumber3Rotated);
  AProduct.Add('sdxRichEditPaperKindJapaneseEnvelopeChouNumber4Rotated', @sdxRichEditPaperKindJapaneseEnvelopeChouNumber4Rotated);
  AProduct.Add('sdxRichEditPaperKindB6Jis', @sdxRichEditPaperKindB6Jis);
  AProduct.Add('sdxRichEditPaperKindB6JisRotated', @sdxRichEditPaperKindB6JisRotated);
  AProduct.Add('sdxRichEditPaperKindStandard12x11', @sdxRichEditPaperKindStandard12x11);
  AProduct.Add('sdxRichEditPaperKindJapaneseEnvelopeYouNumber4', @sdxRichEditPaperKindJapaneseEnvelopeYouNumber4);
  AProduct.Add('sdxRichEditPaperKindJapaneseEnvelopeYouNumber4Rotated', @sdxRichEditPaperKindJapaneseEnvelopeYouNumber4Rotated);
  AProduct.Add('sdxRichEditPaperKindPrc16K', @sdxRichEditPaperKindPrc16K);
  AProduct.Add('sdxRichEditPaperKindPrc32K', @sdxRichEditPaperKindPrc32K);
  AProduct.Add('sdxRichEditPaperKindPrc32KBig', @sdxRichEditPaperKindPrc32KBig);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber1', @sdxRichEditPaperKindPrcEnvelopeNumber1);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber2', @sdxRichEditPaperKindPrcEnvelopeNumber2);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber3', @sdxRichEditPaperKindPrcEnvelopeNumber3);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber4', @sdxRichEditPaperKindPrcEnvelopeNumber4);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber5', @sdxRichEditPaperKindPrcEnvelopeNumber5);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber6', @sdxRichEditPaperKindPrcEnvelopeNumber6);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber7', @sdxRichEditPaperKindPrcEnvelopeNumber7);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber8', @sdxRichEditPaperKindPrcEnvelopeNumber8);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber9', @sdxRichEditPaperKindPrcEnvelopeNumber9);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber10', @sdxRichEditPaperKindPrcEnvelopeNumber10);
  AProduct.Add('sdxRichEditPaperKindPrc16KRotated', @sdxRichEditPaperKindPrc16KRotated);
  AProduct.Add('sdxRichEditPaperKindPrc32KRotated', @sdxRichEditPaperKindPrc32KRotated);
  AProduct.Add('sdxRichEditPaperKindPrc32KBigRotated', @sdxRichEditPaperKindPrc32KBigRotated);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber1Rotated', @sdxRichEditPaperKindPrcEnvelopeNumber1Rotated);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber2Rotated', @sdxRichEditPaperKindPrcEnvelopeNumber2Rotated);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber3Rotated', @sdxRichEditPaperKindPrcEnvelopeNumber3Rotated);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber4Rotated', @sdxRichEditPaperKindPrcEnvelopeNumber4Rotated);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber5Rotated', @sdxRichEditPaperKindPrcEnvelopeNumber5Rotated);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber6Rotated', @sdxRichEditPaperKindPrcEnvelopeNumber6Rotated);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber7Rotated', @sdxRichEditPaperKindPrcEnvelopeNumber7Rotated);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber8Rotated', @sdxRichEditPaperKindPrcEnvelopeNumber8Rotated);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber9Rotated', @sdxRichEditPaperKindPrcEnvelopeNumber9Rotated);
  AProduct.Add('sdxRichEditPaperKindPrcEnvelopeNumber10Rotated', @sdxRichEditPaperKindPrcEnvelopeNumber10Rotated);


  AProduct.Add('sdxRichEditInsertMergeFieldAddressFieldsSource', @sdxRichEditInsertMergeFieldAddressFieldsSource);
  AProduct.Add('sdxRichEditInsertMergeFieldButtonCancel', @sdxRichEditInsertMergeFieldButtonCancel);
  AProduct.Add('sdxRichEditInsertMergeFieldButtonInsert', @sdxRichEditInsertMergeFieldButtonInsert);
  AProduct.Add('sdxRichEditInsertMergeFieldDatabaseFieldsSource', @sdxRichEditInsertMergeFieldDatabaseFieldsSource);
  AProduct.Add('sdxRichEditInsertMergeFieldFields', @sdxRichEditInsertMergeFieldFields);
  AProduct.Add('sdxRichEditInsertMergeFieldForm', @sdxRichEditInsertMergeFieldForm);
  AProduct.Add('sdxRichEditInsertMergeFieldInsert', @sdxRichEditInsertMergeFieldInsert);


  AProduct.Add('sdxRichEditMergeOptionsDialogForm', @sdxRichEditMergeOptionsDialogForm);
  AProduct.Add('sdxRichEditMergeOptionsDialogMergeAllRecords', @sdxRichEditMergeOptionsDialogMergeAllRecords);
  AProduct.Add('sdxRichEditMergeOptionsDialogMergeRecords', @sdxRichEditMergeOptionsDialogMergeRecords);
  AProduct.Add('sdxRichEditMergeOptionsDialogMergeSelectedRecords', @sdxRichEditMergeOptionsDialogMergeSelectedRecords);
  AProduct.Add('sdxRichEditMergeOptionsDialogMergeTo', @sdxRichEditMergeOptionsDialogMergeTo);
  AProduct.Add('sdxRichEditMergeOptionsDialogMergeToFile', @sdxRichEditMergeOptionsDialogMergeToFile);
  AProduct.Add('sdxRichEditMergeOptionsDialogMergeToWindow', @sdxRichEditMergeOptionsDialogMergeToWindow);


  AProduct.Add('sdxRichEditTableOfContentsEditShowLevels', @sdxRichEditTableOfContentsEditShowLevels);
  AProduct.Add('sdxRichEditTableOfContentsForm', @sdxRichEditTableOfContentsForm);
  AProduct.Add('sdxRichEditTableOfContentsPrintPreview', @sdxRichEditTableOfContentsPrintPreview);
  AProduct.Add('sdxRichEditTableOfContentsRightAlignPageNumbers', @sdxRichEditTableOfContentsRightAlignPageNumbers);
  AProduct.Add('sdxRichEditTableOfContentsShowPageNumbers', @sdxRichEditTableOfContentsShowPageNumbers);
  AProduct.Add('sdxRichEditTableOfContentsUseHyperlinks', @sdxRichEditTableOfContentsUseHyperlinks);
  AProduct.Add('sdxRichEditTableOfContentsListParagraphContent', @sdxRichEditTableOfContentsListParagraphContent);


  AProduct.Add('sdxRichEditTableStyleDialogApplyFormattingTo', @sdxRichEditTableStyleDialogApplyFormattingTo);
  AProduct.Add('sdxRichEditTableStyleDialogButtonFormat', @sdxRichEditTableStyleDialogButtonFormat);
  AProduct.Add('sdxRichEditTableStyleDialogCurrentStyle', @sdxRichEditTableStyleDialogCurrentStyle);
  AProduct.Add('sdxRichEditTableStyleDialogFontDialog', @sdxRichEditTableStyleDialogFontDialog);
  AProduct.Add('sdxRichEditTableStyleDialogForm', @sdxRichEditTableStyleDialogForm);
  AProduct.Add('sdxRichEditTableStyleDialogFormatting', @sdxRichEditTableStyleDialogFormatting);
  AProduct.Add('sdxRichEditTableStyleDialogName', @sdxRichEditTableStyleDialogName);
  AProduct.Add('sdxRichEditTableStyleDialogParagraphDialog', @sdxRichEditTableStyleDialogParagraphDialog);
  AProduct.Add('sdxRichEditTableStyleDialogProperties', @sdxRichEditTableStyleDialogProperties);
  AProduct.Add('sdxRichEditTableStyleDialogResetTableCellsBorders', @sdxRichEditTableStyleDialogResetTableCellsBorders);
  AProduct.Add('sdxRichEditTableStyleDialogSelectedStyle', @sdxRichEditTableStyleDialogSelectedStyle);
  AProduct.Add('sdxRichEditTableStyleDialogStyleBasedOn', @sdxRichEditTableStyleDialogStyleBasedOn);
  AProduct.Add('sdxRichEditTableStyleDialogTabsDialog', @sdxRichEditTableStyleDialogTabsDialog);
  AProduct.Add('sdxRichEditTableStyleDialogToggleFontBoldHint', @sdxRichEditTableStyleDialogToggleFontBoldHint);
  AProduct.Add('sdxRichEditTableStyleDialogToggleFontItalicHint', @sdxRichEditTableStyleDialogToggleFontItalicHint);
  AProduct.Add('sdxRichEditTableStyleDialogToggleFontUnderlineHint', @sdxRichEditTableStyleDialogToggleFontUnderlineHint);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsAllBorders', @sdxRichEditTableStyleDialogToggleTableCellsAllBorders);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsBottomBorder', @sdxRichEditTableStyleDialogToggleTableCellsBottomBorder);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsBottomCenterAlignment', @sdxRichEditTableStyleDialogToggleTableCellsBottomCenterAlignment);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsBottomLeftAlignment', @sdxRichEditTableStyleDialogToggleTableCellsBottomLeftAlignment);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsBottomRightAlignment', @sdxRichEditTableStyleDialogToggleTableCellsBottomRightAlignment);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsInsideBorder', @sdxRichEditTableStyleDialogToggleTableCellsInsideBorder);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsInsideHorizontalBorder', @sdxRichEditTableStyleDialogToggleTableCellsInsideHorizontalBorder);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsInsideVerticalBorder', @sdxRichEditTableStyleDialogToggleTableCellsInsideVerticalBorder);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsLeftBorder', @sdxRichEditTableStyleDialogToggleTableCellsLeftBorder);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsMiddleCenterAlignment', @sdxRichEditTableStyleDialogToggleTableCellsMiddleCenterAlignment);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsMiddleLeftAlignment', @sdxRichEditTableStyleDialogToggleTableCellsMiddleLeftAlignment);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsMiddleRightAlignment', @sdxRichEditTableStyleDialogToggleTableCellsMiddleRightAlignment);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsOutsideBorder', @sdxRichEditTableStyleDialogToggleTableCellsOutsideBorder);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsRightBorder', @sdxRichEditTableStyleDialogToggleTableCellsRightBorder);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsTopBorder', @sdxRichEditTableStyleDialogToggleTableCellsTopBorder);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsTopCenterAlignment', @sdxRichEditTableStyleDialogToggleTableCellsTopCenterAlignment);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsTopLeftAlignment', @sdxRichEditTableStyleDialogToggleTableCellsTopLeftAlignment);
  AProduct.Add('sdxRichEditTableStyleDialogToggleTableCellsTopRightAlignment', @sdxRichEditTableStyleDialogToggleTableCellsTopRightAlignment);
  AProduct.Add('sdxRichEditTableStyleDialogPreviewTableColumn1', @sdxRichEditTableStyleDialogPreviewTableColumn1);
  AProduct.Add('sdxRichEditTableStyleDialogPreviewTableColumn2', @sdxRichEditTableStyleDialogPreviewTableColumn2);
  AProduct.Add('sdxRichEditTableStyleDialogPreviewTableColumn3', @sdxRichEditTableStyleDialogPreviewTableColumn3);
  AProduct.Add('sdxRichEditTableStyleDialogPreviewTableRow1', @sdxRichEditTableStyleDialogPreviewTableRow1);
  AProduct.Add('sdxRichEditTableStyleDialogPreviewTableRow2', @sdxRichEditTableStyleDialogPreviewTableRow2);
  AProduct.Add('sdxRichEditTableStyleDialogPreviewTableRow3', @sdxRichEditTableStyleDialogPreviewTableRow3);
  AProduct.Add('sdxRichEditTableStyleDialogPreviewTableTotal', @sdxRichEditTableStyleDialogPreviewTableTotal);

  AProduct.Add('sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_WholeTable', @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_WholeTable);
  AProduct.Add('sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_FirstRow', @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_FirstRow);
  AProduct.Add('sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_LastRow', @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_LastRow);
  AProduct.Add('sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_FirstColumn', @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_FirstColumn);
  AProduct.Add('sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_LastColumn', @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_LastColumn);
  AProduct.Add('sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_OddRowBanding', @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_OddRowBanding);
  AProduct.Add('sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_EvenRowBanding', @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_EvenRowBanding);
  AProduct.Add('sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_OddColumnBanding', @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_OddColumnBanding);
  AProduct.Add('sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_EvenColumnBanding', @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_EvenColumnBanding);
  AProduct.Add('sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_TopLeftCell', @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_TopLeftCell);
  AProduct.Add('sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_TopRightCell', @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_TopRightCell);
  AProduct.Add('sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_BottomLeftCell', @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_BottomLeftCell);
  AProduct.Add('sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_BottomRightCell', @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_BottomRightCell);


  AProduct.Add('sdxDocumentProtectionQueryPasswordForm', @sdxDocumentProtectionQueryPasswordForm);


  AProduct.Add('sdxDocumentProtectionQueryNewPasswordForm', @sdxDocumentProtectionQueryNewPasswordForm);

  AProduct.Add('sdxDocumentEncryptionQueryNewPasswordForm', @sdxDocumentEncryptionQueryNewPasswordForm);


  AProduct.Add('sdxRangeEditingPermissionsButtonApply', @sdxRangeEditingPermissionsButtonApply);
  AProduct.Add('sdxRangeEditingPermissionsForm', @sdxRangeEditingPermissionsForm);
  AProduct.Add('sdxRangeEditingPermissionsGroups', @sdxRangeEditingPermissionsGroups);
  AProduct.Add('sdxRangeEditingPermissionsUsers', @sdxRangeEditingPermissionsUsers);

  AProduct.Add('sdxRangeEditingPermissionsMoreUsers', @sdxRangeEditingPermissionsMoreUsers);
  AProduct.Add('sdxRangeEditingPermissionsAddUsers', @sdxRangeEditingPermissionsAddUsers);
  AProduct.Add('sdxRangeEditingPermissionsEnterUserNames', @sdxRangeEditingPermissionsEnterUserNames);
  AProduct.Add('sdxRangeEditingPermissionsInvalidUserNames', @sdxRangeEditingPermissionsInvalidUserNames);



  AProduct.Add('sdxQueryPasswordForm', @sdxQueryPasswordForm);
  AProduct.Add('sdxQueryPasswordPassword', @sdxQueryPasswordPassword);

  AProduct.Add('sdxQueryNewPasswordForm', @sdxQueryNewPasswordForm);
  AProduct.Add('sdxQueryNewPasswordPassword', @sdxQueryNewPasswordPassword);
  AProduct.Add('sdxQueryNewPasswordRepeatPassword', @sdxQueryNewPasswordRepeatPassword);
  AProduct.Add('sdxQueryNewPasswordInvalidPasswordConfirmation', @sdxQueryNewPasswordInvalidPasswordConfirmation);
end;

initialization
  dxResourceStringsRepository.RegisterProduct(dxRichEditProductName, @AddRichEditDialogsResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct(dxRichEditProductName, @AddRichEditDialogsResourceStringNames);

end.
