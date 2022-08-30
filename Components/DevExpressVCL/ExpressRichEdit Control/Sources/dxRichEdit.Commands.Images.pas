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

unit dxRichEdit.Commands.Images;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

type
  TdxRichEditControlCommandsImages = class sealed
  public const
    //Actions
    Bookmark                        = 'Actions\Bookmark.png';
    Close                           = 'Actions\Close.png';
    LoadDocument                    = 'Actions\Open.png';
    NewDocument                     = 'Actions\New.png';
    SelectAll                       = 'Actions\SelectAll2.png';
    ShowHyperlinkForm               = 'Rich Edit\Hyperlink.png';
    TableStyles                     = 'Actions\FormatAsTable.png';
    //Alignment
    MergeCells                      = 'Alignment\MergeCells.png';
    //Arrange
    BringForward                    = 'Arrange\BringForward.png';
    BringInFrontOfText              = 'Arrange\BringToFrontOfText.png';
    BringToFront                    = 'Arrange\BringToFront.png';
    ImagePositionBottomCenter       = 'Arrange\WithTextWrapping_BottomCenter.png';
    ImagePositionBottomLeft         = 'Arrange\WithTextWrapping_BottomLeft.png';
    ImagePositionBottomRight        = 'Arrange\WithTextWrapping_BottomRight.png';
    ImagePositionMiddleCenter       = 'Arrange\WithTextWrapping_CenterCenter.png';
    ImagePositionMiddleLeft         = 'Arrange\WithTextWrapping_CenterLeft.png';
    ImagePositionMiddleRight        = 'Arrange\WithTextWrapping_CenterRight.png';
    ImagePositionTopCenter          = 'Arrange\WithTextWrapping_TopCenter.png';
    ImagePositionTopLeft            = 'Arrange\WithTextWrapping_TopLeft.png';
    ImagePositionTopRight           = 'Arrange\WithTextWrapping_TopRight.png';
    SendBackward                    = 'Arrange\SendBackward.png';
    SendBehindText                  = 'Arrange\SendBehindText.png';
    SendToBack                      = 'Arrange\SendToBack.png';
    WrapTextBehindText              = 'Arrange\BehindText.png';
    WrapTextInFrontOfText           = 'Arrange\InFrontOfText.png';
    WrapTextSquare                  = 'Arrange\Square.png';
    WrapTextThrough                 = 'Arrange\Through.png';
    WrapTextTight                   = 'Arrange\Tight.png';
    WrapTextTopAndBottom            = 'Arrange\TopAndBottom.png';
    //Content
    InsertPicture                   = 'Content\Image.png';
    //Edit
    CopySelection                   = 'Edit\Copy.png';
    CutSelection                    = 'Edit\Cut.png';
    PasteSelection                  = 'Edit\Paste.png';
    //Find
    SearchFind                      = 'Find\Find.png';
    //Format
    ChangeFontColor                 = 'Rich Edit\FontColor.png';
    ChangeFontStyle                 = 'Format\ChangeFontStyle.png';
    DecreaseFontSize                = 'Format\FontSizeDecrease.png';
    DecrementIndent                 = 'Format\IndentDecrease.png';
    Font                            = 'Format\Font.png';
    FillBackground                  = 'Format\FillBackground.png';
    FontSize                        = 'Format\FontSize.png';
    IncreaseFontSize                = 'Format\FontSizeIncrease.png';
    IncrementIndent                 = 'Format\IndentIncrease.png';
    SearchReplace                   = 'Format\Replace.png';
    ShowParagraphForm               = 'Format\LineSpacingOptions.png';
    ShowSymbolForm                  = 'Rich Edit\Symbol.png';
    TextHighlight                   = 'Rich Edit\Highlight.png';
    ToggleBulletedList              = 'Format\ListBullets.png';
    ToggleFontBold                  = 'Format\Bold.png';
    ToggleFontDoubleStrikeout       = 'Format\StrikeoutDouble.png';
    ToggleFontDoubleUnderline       = 'Format\UnderlineDouble.png';
    ToggleFontItalic                = 'Format\Italic.png';
    ToggleFontStrikeout             = 'Format\Strikeout.png';
    ToggleFontSubscript             = 'Format\Subscript.png';
    ToggleFontSuperscript           = 'Format\Superscript.png';
    ToggleFontUnderline             = 'Format\Underline.png';
    ToggleMultiLevelList            = 'Format\ListMultilevel.png';
    ToggleParagraphAlignmentCenter  = 'Format\AlignCenter.png';
    ToggleParagraphAlignmentJustify = 'Format\AlignJustify.png';
    ToggleParagraphAlignmentLeft    = 'Format\AlignLeft.png';
    ToggleParagraphAlignmentRight   = 'Format\AlignRight.png';
    ToggleSimpleNumberingList       = 'Format\ListNumbers.png';
    ToggleShowWhitespace            = 'Format\ShowHidden.png';
    //Grid
    ShowInsertTableForm             = 'Grid\Grid.png';
    //History
    Undo                            = 'History\Undo.png';
    Redo                            = 'History\Redo.png';
    //Pages
    InsertColumnBreak               = 'Rich Edit\InsertColumnBreak.png';
    InsertPageBreak                 = 'Pages\InsertPageBreak.png';
    InsertSectionBreakEvenPage      = 'Rich Edit\InsertSectionBreakEvenPage.png';
    InsertSectionBreakNextPage      = 'Rich Edit\InsertSectionBreakNextPage.png';
    InsertSectionBreakOddPage       = 'Rich Edit\InsertSectionBreakOddPage.png';
    LineNumbers                     = 'Rich Edit\LineNumbering.png';
    Margins                         = 'Pages\PageMargins.png';
    PaperSize                       = 'Pages\PaperSize.png';
    SetSectionOneColumn             = 'Rich Edit\ColumnsOne.png';
    SetSectionTwoColumns            = 'Rich Edit\ColumnsTwo.png';
    SetSectionThreeColumns          = 'Rich Edit\ColumnsThree.png';
    ShowColumnsSetupForm            = 'Format\Columns.png';
    ToggleShowHorizontalRuler       = 'Rich Edit\RulerHorizontal.png';
    ToggleShowVerticalRuler         = 'Rich Edit\RulerVertical.png';
    PageOrientationLandscape        = 'Pages\PageOrientationLandscape.png';
    PageOrientationPortrait         = 'Pages\PageOrientationPortrait.png';
    //Print
    ShowPageSetupForm               = 'Setup\PageSetup.png';
    ShowPrintForm                   = 'Print\PrintDialog.png';
    ShowPrintPreviewForm            = 'Print\Preview.png';
    //Save
    SaveDocument                    = 'Save\Save.png';
    SaveDocumentAs                  = 'Save\SaveAs.png';
    //Reports
    SwitchToDraftView               = 'Rich Edit\DraftView.png';
    SwitchToPrintLayoutView         = 'Rich Edit\PrintLayoutView.png';
    SwitchToSimpleView              = 'Rich Edit\SimpleView.png';
    //Rich Edit
    AddParagraphToTableOfContents   = 'Rich Edit\AddParagraphToTableOfContents.png';
    AutoFitContents                 = 'Rich Edit\TableAutoFitContents.png';
    AutoFitWindow                   = 'Rich Edit\TableAutoFitWindow.png';
    BorderBottom                    = 'Rich Edit\BorderBottom.png';
    BorderInsideHorizontal          = 'Rich Edit\BorderInsideHorizontal.png';
    BorderInsideVertical            = 'Rich Edit\BorderInsideVertical.png';
    BorderLeft                      = 'Rich Edit\BorderLeft.png';
    BorderNone                      = 'Rich Edit\BorderNone.png';
    BorderRight                     = 'Rich Edit\BorderRight.png';
    BordersAll                      = 'Rich Edit\BordersAll.png';
    BordersInside                   = 'Rich Edit\BordersInside.png';
    BordersOutside                  = 'Rich Edit\BordersOutside.png';
    BorderTop                       = 'Rich Edit\BorderTop.png';
    CellsAlignmentBottomLeft        = 'Rich Edit\AlignBottomLeft.png';
    CellsAlignmentBottomCenter      = 'Rich Edit\AlignBottomCenter.png';
    CellsAlignmentBottomRight       = 'Rich Edit\AlignBottomRight.png';
    CellsAlignmentMiddleLeft        = 'Rich Edit\AlignMiddleLeft.png';
    CellsAlignmentMiddleCenter      = 'Rich Edit\AlignMiddleCenter.png';
    CellsAlignmentMiddleRight       = 'Rich Edit\AlignMiddleRight.png';
    CellsAlignmentTopLeft           = 'Rich Edit\AlignTopLeft.png';
    CellsAlignmentTopCenter         = 'Rich Edit\AlignTopCenter.png';
    CellsAlignmentTopRight          = 'Rich Edit\AlignTopRight.png';
    DeleteCells                     = 'Rich Edit\DeleteTableCells.png';
    DeleteColumns                   = 'Rich Edit\DeleteTableColumns.png';
    DeleteHyperlink                 = 'SpreadSheet\DeleteHyperlink.png';
    DeleteRows                      = 'Rich Edit\DeleteTableRows.png';
    DeleteTable                     = 'Rich Edit\DeleteTable.png';
    DifferentFirstPage              = 'Rich Edit\DifferentFirstPage.png';
    DifferentOddAndEvenPages        = 'Rich Edit\DifferentOddEvenPages.png';
    FixedColumnWidth                = 'Rich Edit\TableFixedColumnWidth.png';
    FloatingObjectFillColor         = 'Rich Edit\FloatingObjectFillColor.png';
    FloatingObjectLayoutOptions     = 'Rich Edit\FloatingObjectLayoutOptions.png';
    FloatingObjectOutlineColor      = 'Rich Edit\FloatingObjectOutlineColor.png';
    Footer                          = 'Rich Edit\Footer.png';
    GoToFooter                      = 'Rich Edit\GoToFooter.png';
    GoToHeader                      = 'Rich Edit\GoToHeader.png';
    Header                          = 'Rich Edit\Header.png';
    InsertCaption                   = 'Rich Edit\InsertCaption.png';
    InsertColumnsToTheLeft          = 'Rich Edit\InsertTableColumnsToTheLeft.png';
    InsertColumnsToTheRight         = 'Rich Edit\InsertTableColumnsToTheRight.png';
    InsertDataField                 = 'Rich Edit\InsertDataField.png';
    InsertEquationCaption           = 'Rich Edit\InsertEquationCaption.png';
    InsertFigureCaption             = 'Rich Edit\InsertFigureCaption.png';
    InsertRowsAbove                 = 'Rich Edit\InsertTableRowsAbove.png';
    InsertRowsBelow                 = 'Rich Edit\InsertTableRowsBelow.png';
    InsertTableCaption              = 'Rich Edit\InsertTableCaption.png';
    InsertTableCells                = 'Rich Edit\InsertTableCells.png';
    InsertTableOfCaptions           = 'Rich Edit\InsertTableOfCaptions.png';
    InsertTableOfContents           = 'Rich Edit\InsertTableOfContents.png';
    InsertTableOfEquations          = 'Rich Edit\InsertTableOfEquations.png';
    InsertTableOfFigures            = 'Rich Edit\InsertTableOfFigures.png';
    LinkToPrevious                  = 'Rich Edit\LinkToPrevious.png';
    MailMerge                       = 'Rich Edit\MailMerge.png';
    PageCount                       = 'Rich Edit\InsertPageCount.png';
    PageNumber                      = 'Rich Edit\InsertPageNumber.png';
    ShowAllFieldCodes               = 'Rich Edit\ShowAllFieldCodes.png';
    ShowAllFieldResults             = 'Rich Edit\ShowAllFieldResults.png';
    ShowNextHeaderFooter            = 'Rich Edit\GoToNextHeaderFooter.png';
    ShowPreviousHeaderFooter        = 'Rich Edit\GoToPreviousHeaderFooter.png';
    SplitCells                      = 'Rich Edit\SplitTableCells.png';
    SplitTable                      = 'Rich Edit\SplitTable.png';
    TableProperties                 = 'Rich Edit\TableProperties.png';
    TextBox                         = 'Rich Edit\InsertTextBox.png';
    ToggleFieldCodes                = 'Rich Edit\ToggleFieldCodes.png';
    UpdateField                     = 'Rich Edit\UpdateField.png';
    UpdateTableOfContents           = 'Rich Edit\UpdateTableOfContents.png';
    ViewMergedData                  = 'Rich Edit\ViewMergedData.png';
    ViewTableGridlines              = 'Rich Edit\ViewTableGridlines.png';
    //Zoom
    ZoomIn                          = 'Zoom\ZoomIn.png';
    ZoomOut                         = 'Zoom\ZoomOut.png';
    // Review
    CheckSpelling                   = 'Rich Edit\SpellCheck.png';
    EditRangePermission             = 'Rich Edit\EditRangePermission.png';
    ProtectDocument                 = 'Rich Edit\ProtectDocument.png';
    UnprotectDocument               = 'Rich Edit\UnprotectDocument.png';
    EncryptDocument                 = 'SpreadSheet\Encrypt.png';
  end;

implementation

end.
