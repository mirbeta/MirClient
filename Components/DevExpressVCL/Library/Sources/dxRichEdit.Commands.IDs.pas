﻿{********************************************************************}
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

unit dxRichEdit.Commands.IDs;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

type
  TdxRichEditCommandId = (
    None,
    ToggleShowWhitespace,
    PreviousCharacter,
    NextCharacter,
    ExtendPreviousCharacter,
    ExtendNextCharacter,
    PreviousWord,
    NextWord,
    ExtendPreviousWord,
    ExtendNextWord,
    PreviousParagraph,
    NextParagraph,
    ExtendPreviousParagraph,
    ExtendNextParagraph,
    PreviousPage,
    NextPage,
    ExtendPreviousPage,
    ExtendNextPage,
    PreviousScreen,
    NextScreen,
    ExtendPreviousScreen,
    ExtendNextScreen,
    StartOfLine,
    EndOfLine,
    ExtendStartOfLine,
    ExtendEndOfLine,
    StartOfDocument,
    EndOfDocument,
    ExtendStartOfDocument,
    ExtendEndOfDocument,
    PreviousLine,
    NextLine,
    ExtendPreviousLine,
    ExtendNextLine,
    SelectAll,
    Undo,
    Redo,
    InsertParagraph,
    InsertLineBreak,
    InsertPageBreak,
    InsertColumnBreak,
    InsertNonBreakingSpace,
    InsertEnDash,
    InsertEmDash,
    InsertCopyrightSymbol,
    InsertRegisteredTrademarkSymbol,
    InsertTrademarkSymbol,
    InsertEllipsis,
    ToggleFontBold,
    ToggleFontItalic,
    ToggleFontUnderline,
    ToggleFontDoubleUnderline,
    IncreaseFontSize,
    DecreaseFontSize,
    IncrementFontSize,
    DecrementFontSize,
    ToggleFontSuperscript,
    ToggleFontSubscript,
    ShowFontForm,
    ShowParagraphForm,
    ToggleParagraphAlignmentLeft,
    ToggleParagraphAlignmentCenter,
    ToggleParagraphAlignmentRight,
    ToggleParagraphAlignmentJustify,
    SetSingleParagraphSpacing,
    SetDoubleParagraphSpacing,
    SetSesquialteralParagraphSpacing,
    Delete,
    BackSpaceKey,
    DeleteWord,
    DeleteWordBack,
    CopySelection,
    PasteSelection,
    CutSelection,
    IncrementNumerationFromParagraph,
    DecrementNumerationFromParagraph,
    FindNext,
    FindPrev,
    Find,
    Replace,
    TabKey,
    ShiftTabKey,
    FileNew,
    FileOpen,
    FileSave,
    FileSaveAs,
    IncreaseIndent,
    DecreaseIndent,
    ZoomIn,
    ZoomOut,
    InsertPicture,
    ToggleFontStrikeout,
    ToggleFontDoubleStrikeout,
    ToggleNumberingListItem,
    ToggleMultilevelListItem,
    ToggleBulletedListItem,
    Print,
    QuickPrint,
    PrintPreview,
    ClearFormatting,
    ToggleHiddenText,
    CreateField,
    UpdateField,
    ToggleFieldCodes,
    ShowInsertMergeFieldForm,
    ToggleViewMergedData,
    ShowAllFieldCodes,
    ShowAllFieldResults,
    ChangeFontName,
    ChangeFontSize,
    ChangeFontStyle,
    ShowNumberingListForm,
    InsertTab,
    ShowSymbolForm,
    SwitchToDraftView,
    SwitchToPrintLayoutView,
    SwitchToSimpleView,
    ShowBookmarkForm,
    ShowHyperlinkForm,
    EnterKey,
    EditPageHeader,
    EditPageFooter,
    ClosePageHeaderFooter,
    GoToPageHeader,
    GoToPageFooter,
    ToggleHeaderFooterLinkToPrevious,
    GoToPreviousHeaderFooter,
    GoToNextHeaderFooter,
    ToggleDifferentFirstPage,
    ToggleDifferentOddAndEvenPages,
    ChangeParagraphLineSpacing,
    ShowLineSpacingForm,
    AddSpacingBeforeParagraph,
    AddSpacingAfterParagraph,
    RemoveSpacingBeforeParagraph,
    RemoveSpacingAfterParagraph,
    SetPortraitPageOrientation,
    SetLandscapePageOrientation,
    ChangeSectionPageOrientation,
    ChangeSectionPageMargins,
    SetNormalSectionPageMargins,
    SetNarrowSectionPageMargins,
    SetModerateSectionPageMargins,
    SetWideSectionPageMargins,
    InsertTable,
    InsertTableRowBelow,
    InsertTableRowAbove,
    MergeTableElement,
    MergeTableCells,
    InsertPageNumberField,
    InsertPageCountField,
    DeleteTableRowsMenuItem,
    ToggleSpellCheckAsYouType,
    CheckSpelling,
    NextDataRecord,
    PreviousDataRecord,
    MailMergeSaveDocumentAs,
    InsertMailMergeField,
    InsertMailMergeFieldPlaceholder,
    SplitTable,
    ChangeTableBordersPlaceholder,
    ToggleTableCellsAllBorders,
    ResetTableCellsAllBorders,
    ToggleTableCellsOutsideBorder,
    ToggleTableCellsInsideBorder,
    ToggleTableCellsLeftBorder,
    ToggleTableCellsRightBorder,
    ToggleTableCellsTopBorder,
    ToggleTableCellsBottomBorder,
    ToggleTableCellsInsideHorizontalBorder,
    ToggleTableCellsInsideVerticalBorder,
    InsertTableColumnToTheLeft,
    InsertTableColumnToTheRight,
    ChangeFontForeColor,
    ChangeFontBackColor,
    DeleteTable,
    ChangeTableCellsBorderColor,
    ChangeTableCellsShading,
    ShowInsertTableCellsForm,
    Zoom,
    GoToPage,
    ShowDeleteTableCellsForm,
    DeleteTableColumns,
    DeleteTableRows,
    DeleteTablePlaceholder,
    ChangeTableCellsContentAlignmentPlaceholder,
    ToggleTableCellsTopLeftAlignment,
    ToggleTableCellsTopCenterAlignment,
    ToggleTableCellsTopRightAlignment,
    ToggleTableCellsMiddleLeftAlignment,
    ToggleTableCellsMiddleCenterAlignment,
    ToggleTableCellsMiddleRightAlignment,
    ToggleTableCellsBottomLeftAlignment,
    ToggleTableCellsBottomCenterAlignment,
    ToggleTableCellsBottomRightAlignment,
    ZoomPercent,
    ShowSplitTableCellsForm,
    SetSectionOneColumn,
    SetSectionTwoColumns,
    SetSectionThreeColumns,
    SetSectionColumnsPlaceholder,
    ShowSplitTableCellsFormMenuItem,
    SelectTableColumns,
    SelectTableCell,
    SelectTableRow,
    LastDataRecord,
    FirstDataRecord,
    ShowRangeEditingPermissionsForm,
    ChangeSectionPaperKind,
    ChangeSectionPaperKindPlaceholder,
    ProtectDocument,
    UnprotectDocument,
    MakeTextUpperCase,
    MakeTextLowerCase,
    ToggleTextCase,
    ChangeTextCasePlaceholder,
    SelectTable,
    SelectTablePlaceholder,
    ShowDeleteTableCellsFormMenuItem,
    DeleteTableColumnsMenuItem,
    ClearUndo,
    DeselectAll,
    ToggleShowTableGridLines,
    ToggleShowHorizontalRuler,
    ToggleShowVerticalRuler,
    ChangeTableCellsBorderLineStyle,
    InsertBreak,
    InsertSectionBreakNextPage,
    InsertSectionBreakOddPage,
    InsertSectionBreakEvenPage,
    InsertSectionBreakContinuous,
    ChangeSectionLineNumbering,
    SetSectionLineNumberingNone,
    SetSectionLineNumberingContinuous,
    SetSectionLineNumberingRestartNewPage,
    SetSectionLineNumberingRestartNewSection,
    ToggleParagraphSuppressLineNumbers,
    ToggleParagraphSuppressHyphenation,
    ShowLineNumberingForm,
    ShowPageSetupForm,
    ShowPasteSpecialForm,
    CreateHyperlink,
    EditHyperlink,
    CreateBookmark,
    ShowTablePropertiesForm,
    ShowTablePropertiesFormMenuItem,
    IncrementParagraphOutlineLevel,
    DecrementParagraphOutlineLevel,
    SetParagraphBodyTextLevel,
    SetParagraphHeading1Level,
    SetParagraphHeading2Level,
    SetParagraphHeading3Level,
    SetParagraphHeading4Level,
    SetParagraphHeading5Level,
    SetParagraphHeading6Level,
    SetParagraphHeading7Level,
    SetParagraphHeading8Level,
    SetParagraphHeading9Level,
    AddParagraphsToTableOfContents,
    InsertTableOfContents,
    InsertTableOfEquations,
    InsertTableOfFigures,
    InsertTableOfTables,
    InsertTableOfFiguresPlaceholder,
    InsertEquationCaption,
    InsertFigureCaption,
    InsertTableCaption,
    InsertCaptionPlaceholder,
    UpdateTableOfContents,
    FindForward,
    FindBackward,
    ReplaceForward,
    ReplaceAllForward,
    ShowColumnsSetupForm,
    ToggleTableAutoFitPlaceholder,
    ToggleTableAutoFitContents,
    ToggleTableAutoFitWindow,
    ToggleTableFixedColumnWidth,
    UpdateFields,
    ToggleTableAutoFitMenuPlaceholder,
    SetFloatingObjectSquareTextWrapType,
    SetFloatingObjectBehindTextWrapType,
    SetFloatingObjectInFrontOfTextWrapType,
    SetFloatingObjectThroughTextWrapType,
    SetFloatingObjectTightTextWrapType,
    SetFloatingObjectTopAndBottomTextWrapType,
    SetFloatingObjectTopLeftAlignment,
    SetFloatingObjectTopCenterAlignment,
    SetFloatingObjectTopRightAlignment,
    SetFloatingObjectMiddleLeftAlignment,
    SetFloatingObjectMiddleCenterAlignment,
    SetFloatingObjectMiddleRightAlignment,
    SetFloatingObjectBottomLeftAlignment,
    SetFloatingObjectBottomCenterAlignment,
    SetFloatingObjectBottomRightAlignment,
    ChangeFloatingObjectTextWrapType,
    ChangeFloatingObjectAlignment,
    FloatingObjectBringForwardPlaceholder,
    FloatingObjectBringForward,
    FloatingObjectBringToFront,
    FloatingObjectBringInFrontOfText,
    FloatingObjectSendBackward,
    FloatingObjectSendToBack,
    FloatingObjectSendBehindText,
    FloatingObjectSendBackwardPlaceholder,
    SelectUpperLevelObject,
    ChangeFloatingObjectFillColor,
    ChangeFloatingObjectOutlineColor,
    ChangeFloatingObjectOutlineWeight,
    ShowPageMarginsSetupForm,
    ShowPagePaperSetupForm,
    ShowEditStyleForm,
    InsertTextBox,
    ShowFloatingObjectLayoutOptionsForm,
    InsertFloatingPicture,
    ChangeParagraphBackColor,
    ShowTableOptionsForm,
    ChangePageColor,
    ChangeTableCellsBorderLineWeight,
    ChangeTableStyle,
    UpdateTableOfFigures,
    ShowTOCForm,
    ToggleOvertype,
    InsertText,
    OvertypeText,
    OpenHyperlink,
    RemoveHyperlink,

    ReplaceMisspelling,
    IgnoreMisspelling,
    IgnoreAllMisspellings,
    AddWordToDictionary,
    DeleteRepeatedWord,
    ChangeCase,
    AutoCorrectMisspelling,
    AutoCorrectPlaceholder,

    ShowMergeDatabaseRecordsForm,
    EncryptDocument
  );

implementation

end.
