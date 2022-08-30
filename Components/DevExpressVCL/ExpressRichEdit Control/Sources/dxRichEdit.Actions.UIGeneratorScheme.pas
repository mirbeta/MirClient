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

unit dxRichEdit.Actions.UIGeneratorScheme;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxUIGenerator;

const
  sdxRichEditControlCategory = 'DevExpress RichEdit Control';

type

  { TdxRichEditControlUIGenerator }

  TdxRichEditControlUIGenerator = class
  strict private
    FComponentInfo: TdxUIGeneratorComponentInfo;
  protected
    //Commands
    procedure AddCommandAutoFit(ACategoryInfo: TdxUIGeneratorCategoryInfo);
    procedure AddCommandBorders(ACategoryInfo: TdxUIGeneratorCategoryInfo);
    procedure AddCommandBreaks(ACategoryInfo: TdxUIGeneratorCategoryInfo);
    procedure AddCommandBringToFront(ACategoryInfo: TdxUIGeneratorCategoryInfo);
    procedure AddCommandChangeCase(ACategoryInfo: TdxUIGeneratorCategoryInfo);
    procedure AddCommandColumns(ACategoryInfo: TdxUIGeneratorCategoryInfo);
    procedure AddCommandDelete(ACategoryInfo: TdxUIGeneratorCategoryInfo);
    procedure AddCommandLineNumbers(ACategoryInfo: TdxUIGeneratorCategoryInfo);
    procedure AddCommandLineSpacing(ACategoryInfo: TdxUIGeneratorCategoryInfo);
    procedure AddCommandOrientation(ACategoryInfo: TdxUIGeneratorCategoryInfo);
    procedure AddCommandPosition(ACategoryInfo: TdxUIGeneratorCategoryInfo);
    procedure AddCommandSendToBack(ACategoryInfo: TdxUIGeneratorCategoryInfo);
    procedure AddCommandWrapText(ACategoryInfo: TdxUIGeneratorCategoryInfo);
    //Categories
    procedure AddCategoryAlignment;
    procedure AddCategoryArrange;
    procedure AddCategoryBackground;
    procedure AddCategoryCaptions;
    procedure AddCategoryCellSize;
    procedure AddCategoryClipboard;
    procedure AddCategoryClose;
    procedure AddCategoryCommon;
    procedure AddCategoryDocumentViews;
    procedure AddCategoryEditing;
    procedure AddCategoryFont;
    procedure AddCategoryHeaderAndFooter;
    procedure AddCategoryIllustrations;
    procedure AddCategoryLinks;
    procedure AddCategoryMailMerge;
    procedure AddCategoryMerge;
    procedure AddCategoryNavigation;
    procedure AddCategoryOptions;
    procedure AddCategoryPages;
    procedure AddCategoryPageSetup;
    procedure AddCategoryParagraph;
    procedure AddCategoryProtect;
    procedure AddCategoryRowAndColumns;
    procedure AddCategoryShapeStyles;
    procedure AddCategoryShow;
    procedure AddCategorySpelling;
    procedure AddCategoryStyles;
    procedure AddCategorySymbols;
    procedure AddCategoryTable;
    procedure AddCategoryTableOfContents;
    procedure AddCategoryTables;
    procedure AddCategoryTableStyles;
    procedure AddCategoryText;
    procedure AddCategoryZoom;
    //Tabs
    procedure AddTabFile; virtual;
    procedure AddTabHeaderAndFooterDesign; virtual;
    procedure AddTabHome; virtual;
    procedure AddTabInsert; virtual;
    procedure AddTabMailMerge; virtual;
    procedure AddTabPageLayout; virtual;
    procedure AddTabReferences; virtual;
    procedure AddTabPictureFormat; virtual;
    procedure AddTabs; virtual;
    procedure AddTabTableDesign; virtual;
    procedure AddTabTableLayout; virtual;
    procedure AddTabReview; virtual;
    procedure AddTabView; virtual;

    property ComponentInfo: TdxUIGeneratorComponentInfo read FComponentInfo;
  public
    constructor Create(AComponentInfo: TdxUIGeneratorComponentInfo); virtual;
  end;

procedure RegisterRichEditUIGeneratorScheme;

implementation

uses
  dxRichEdit.Control, dxRichEdit.Actions, dxRichEdit.Commands.Images;

const
  // tabs name
  sdxRichEditTabFile = 'File';
  sdxRichEditTabHome = 'Home';
  sdxRichEditTabInsert = 'Insert';
  sdxRichEditTabPageLayout = 'Page Layout';
  sdxRichEditTabMailMerge = 'Mail Merge';
  sdxRichEditTabReferences = 'References';
  sdxRichEditTabReview = 'Review';
  sdxRichEditTabView = 'View';
  sdxRichEditTabHeaderAndFooterDesign = 'Header & Footer Design';
  sdxRichEditTabTableLayout = 'Table Layout';
  sdxRichEditTabTableDesign = 'Table Design';
  sdxRichEditTabPictureFormat = 'Format';

  // bars
  sdxRichEditBarCommon = 'Common';

  sdxRichEditBarClipboard = 'Clipboard';
  sdxRichEditBarFont = 'Font';
  sdxRichEditBarParagraph = 'Paragraph';
  sdxRichEditBarStyles = 'Styles';
  sdxRichEditBarEditing = 'Editing';

  sdxRichEditBarPages = 'Pages';
  sdxRichEditBarTables = 'Tables';
  sdxRichEditBarIllustrations = 'Illustrations';
  sdxRichEditBarLinks = 'Links';
  sdxRichEditBarHeaderAndFooter = 'Header & Footer';
  sdxRichEditBarText = 'Text';
  sdxRichEditBarProofing = 'Proofing';
  sdxRichEditBarProtect = 'Protect';
  sdxRichEditBarSymbols = 'Symbols';

  sdxRichEditBarPageSetup = 'Page Setup';
  sdxRichEditBarPageBackground = 'Background';

  sdxRichEditBarTableOfContents = 'Table of Contents';
  sdxRichEditBarCaptions = 'Captions';

  sdxRichEditBarMailMerge = 'Mail Merge';

  sdxRichEditBarDocumentViews = 'Document Views';
  sdxRichEditBarShow = 'Show';
  sdxRichEditBarZoom = 'Zoom';

  sdxRichEditBarNavigation = 'Navigation';
  sdxRichEditBarOptions = 'Options';
  sdxRichEditBarClose = 'Close';

  sdxRichEditBarTable = 'Table';
  sdxRichEditBarRowAndColumns = 'Row & Columns';
  sdxRichEditBarMerge = 'Merge';
  sdxRichEditBarCellSize = 'Cell Size';
  sdxRichEditBarAlignment = 'Alignment';

  sdxRichEditBarTableStyles = 'Table Styles';

  sdxRichEditBarShapeStyles = 'Shape Styles';
  sdxRichEditBarArrange = 'Arrange';

  // sub items

  sdxRichEditSubItemChangeCase = 'Change Case';
  sdxRichEditSubItemLineSpacing = 'Line Spacing';

  sdxRichEditSubItemOrientation = 'Orientation';
  sdxRichEditSubItemColumns = 'Columns';
  sdxRichEditSubItemBreaks = 'Breaks';
  sdxRichEditSubItemLineNumbers = 'Line Numbers';

  sdxRichEditSubItemDelete = 'Delete';
  sdxRichEditSubItemAutoFit = 'AutoFit';

  sdxRichEditSubItemBorders = 'Borders';

  sdxRichEditSubItemWrapText = 'Wrap Text';
  sdxRichEditSubItemPosition = 'Position';
  sdxRichEditSubItemBringToFront = 'Bring To Front';
  sdxRichEditSubItemSendToBack = 'Send To Back';

  // pictures

  dxRichEditBarLinksPictureName            = 'Rich Edit\Hyperlink_16x16.png';
  dxRichEditBarShapeStylesPictureName      = 'Rich Edit\FloatingObjectOutlineColor_16x16.png';
  dxRichEditBarArrangePictureName          = 'Arrange\BringToFront_16x16.png';
  dxRichEditBarPagesPictureName            = 'Content\TextBox_16x16.png';
  dxRichEditBarClipboardPictureName        = 'Edit\Paste_16x16.png';
  dxRichEditBarEditingPictureName          = 'Find\Find_16x16.png';
  dxRichEditBarEditRangePermission         = 'Rich Edit\EditRangePermission_16x16.png';
  dxRichEditBarFontPictureName             = 'Rich Edit\FontColor_16x16.png';
  dxRichEditBarParagraphPictureName        = 'Format\AlignCenter_16x16.png';
  dxRichEditBarSymbolsPictureName          = 'Rich Edit\Symbol_16x16.png';
  dxRichEditBarSpellCheckPictureName       = 'Rich Edit\SpellCheck_16x16.png';
  dxRichEditBarTablesPictureName           = 'Grid\Grid_16x16.png';
  dxRichEditBarPageSetupPictureName        = 'Pages\PaperSize_16x16.png';
  dxRichEditBarDocumentViewsPictureName    = 'Rich Edit\SimpleView_16x16.png';
  dxRichEditBarHeaderAndFooterPictureName  = 'Reports\EditPageHF_16x16.png';
  dxRichEditBarIllustrationsPictureName    = 'Toolbox Items\Shape_16x16.png';
  dxRichEditBarTextPictureName             = 'Toolbox Items\Label_16x16.png';
  dxRichEditBarZoomPictureName             = 'Zoom\Zoom_16x16.png';
  dxRichEditBarPageBackgroundPictureName   = 'Format\FillBackground_16x16.png';
  dxRichEditTabMailMergePictureName        = 'Rich Edit\MailMerge_16x16.png';
  dxRichEditBarShowPictureName             = 'Rich Edit\RulerHorizontal_16x16.png';
  dxRichEditBarNavigationPictureName       = 'Rich Edit\GoToFooter_16x16.png';
  dxRichEditBarOptionsPictureName          = 'Rich Edit\DifferentFirstPage_16x16.png';
  dxRichEditBarClosePictureName            = 'Actions\Close_16x16.png';
  dxRichEditBarTablePictureName            = 'Rich Edit\TableProperties_16x16.png';
  dxRichEditBarRowAndColumnsPictureName    = 'Rich Edit\InsertTableRowsAbove_16x16.png';
  dxRichEditBarMergePictureName            = 'Alignment\MergeCells_16x16.png';
  dxRichEditBarCellSizePictureName         = 'Rich Edit\TableAutoFitContents_16x16.png';
  dxRichEditBarAlignmentPictureName        = 'Rich Edit\AlignMiddleCenter_16x16.png';
  dxRichEditBarCommonPictureName           = 'Save\Save_16x16.png';
  dxRichEditBarStylesPictureName           = 'Format\ChangeFontStyle_16x16.png';
  dxRichEditBarTableStylesPictureName      = 'Actions\FormatAsTable_16x16.png';

  dxRichEditSubItemChangeCasePictureName   = 'Rich Edit\ChangeTextCase.png';
  dxRichEditSubItemLineSpacingPictureName  = 'Rich Edit\LineSpacing.png';
  dxRichEditSubItemOrientationPictureName  = 'Pages\PageOrientation.png';

{ TdxRichEditControlUIGenerator }

constructor TdxRichEditControlUIGenerator.Create(AComponentInfo: TdxUIGeneratorComponentInfo);
begin
  inherited Create;
  FComponentInfo := AComponentInfo;
end;

procedure TdxRichEditControlUIGenerator.AddCommandAutoFit(ACategoryInfo: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategoryInfo.Add(TdxRichEditControlCommandsImages.AutoFitContents,
    sdxRichEditSubItemAutoFit, [ugivlLargeIcon]);
  ACommand.Add(TdxRichEditControlToggleTableAutoFitContents);
  ACommand.Add(TdxRichEditControlToggleTableAutoFitWindow);
  ACommand.Add(TdxRichEditControlToggleTableFixedColumnWidth);
end;

procedure TdxRichEditControlUIGenerator.AddCommandBorders(ACategoryInfo: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategoryInfo.Add(TdxRichEditControlCommandsImages.BordersOutside,
    sdxRichEditSubItemBorders, [ugivlLargeIcon]);
  ACommand.Add(TdxRichEditControlToggleTableCellsBottomBorder);
  ACommand.Add(TdxRichEditControlToggleTableCellsTopBorder);
  ACommand.Add(TdxRichEditControlToggleTableCellsLeftBorder);
  ACommand.Add(TdxRichEditControlToggleTableCellsRightBorder);
  ACommand.Add(TdxRichEditControlResetTableCellsBorders);
  ACommand.Add(TdxRichEditControlToggleTableCellsAllBorders);
  ACommand.Add(TdxRichEditControlToggleTableCellsOutsideBorder);
  ACommand.Add(TdxRichEditControlToggleTableCellsInsideBorder);
  ACommand.Add(TdxRichEditControlToggleTableCellsInsideHorizontalBorder);
  ACommand.Add(TdxRichEditControlToggleTableCellsInsideVerticalBorder);
end;

procedure TdxRichEditControlUIGenerator.AddCommandBreaks(ACategoryInfo: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategoryInfo.Add(TdxRichEditControlCommandsImages.InsertPageBreak,
    sdxRichEditSubItemBreaks, [ugivlLargeIcon]);
  ACommand.Add(TdxRichEditControlInsertPageBreak);
  ACommand.Add(TdxRichEditControlInsertColumnBreak);
  ACommand.Add(TdxRichEditControlInsertSectionBreakNextPage);
  ACommand.Add(TdxRichEditControlInsertSectionBreakEvenPage);
  ACommand.Add(TdxRichEditControlInsertSectionBreakOddPage);
end;

procedure TdxRichEditControlUIGenerator.AddCommandBringToFront(ACategoryInfo: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategoryInfo.Add(TdxRichEditControlCommandsImages.BringForward,
    sdxRichEditSubItemBringToFront, [ugivlLargeIcon]);
  ACommand.Add(TdxRichEditControlFloatingObjectBringForward);
  ACommand.Add(TdxRichEditControlFloatingObjectBringToFront);
  ACommand.Add(TdxRichEditControlFloatingObjectBringInFrontOfText);
end;

procedure TdxRichEditControlUIGenerator.AddCommandChangeCase(ACategoryInfo: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategoryInfo.Add(dxRichEditSubItemChangeCasePictureName,
    sdxRichEditSubItemChangeCase, [ugivlSmallIcon], ugigpStart, ugipContinuesRow);
  ACommand.Add(TdxRichEditControlTextUpperCase);
  ACommand.Add(TdxRichEditControlTextLowerCase);
  ACommand.Add(TdxRichEditControlToggleTextCase);
end;

procedure TdxRichEditControlUIGenerator.AddCommandColumns(ACategoryInfo: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategoryInfo.Add(TdxRichEditControlCommandsImages.SetSectionTwoColumns,
    sdxRichEditSubItemColumns, [ugivlLargeIcon]);
  ACommand.Add(TdxRichEditControlSetSectionOneColumn);
  ACommand.Add(TdxRichEditControlSetSectionTwoColumns);
  ACommand.Add(TdxRichEditControlSetSectionThreeColumns);
  ACommand.Add(TdxRichEditControlShowColumnsSetupForm,
    dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);
end;

procedure TdxRichEditControlUIGenerator.AddCommandDelete(ACategoryInfo: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategoryInfo.Add(TdxRichEditControlCommandsImages.DeleteTable,
    sdxRichEditSubItemDelete, [ugivlLargeIcon]);
  ACommand.Add(TdxRichEditControlShowDeleteTableCellsForm);
  ACommand.Add(TdxRichEditControlDeleteTableColumns);
  ACommand.Add(TdxRichEditControlDeleteTableRows);
  ACommand.Add(TdxRichEditControlDeleteTable);
end;

procedure TdxRichEditControlUIGenerator.AddCommandLineNumbers(ACategoryInfo: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategoryInfo.Add(TdxRichEditControlCommandsImages.LineNumbers,
    sdxRichEditSubItemLineNumbers, [ugivlLargeIcon]);
  ACommand.Add(TdxRichEditControlSetSectionLineNumberingNone);
  ACommand.Add(TdxRichEditControlSetSectionLineNumberingContinuous);
  ACommand.Add(TdxRichEditControlSetSectionLineNumberingRestartNewPage);
  ACommand.Add(TdxRichEditControlSetSectionLineNumberingRestartNewSection);
  ACommand.Add(TdxRichEditControlShowLineNumberingForm, dxUIGeneratorItemDefaultViewLevels,
    ugigpNone, ugipBeginsNewRow, True)
end;

procedure TdxRichEditControlUIGenerator.AddCommandLineSpacing(ACategoryInfo: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategoryInfo.Add(dxRichEditSubItemLineSpacingPictureName,
    sdxRichEditSubItemLineSpacing, [ugivlSmallIcon], ugigpStart, ugipContinuesRow);
  ACommand.Add(TdxRichEditControlSetSingleParagraphSpacing);
  ACommand.Add(TdxRichEditControlSetSesquialteralParagraphSpacing);
  ACommand.Add(TdxRichEditControlSetDoubleParagraphSpacing);
  ACommand.Add(TdxRichEditControlShowParagraphForm,
    dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);
end;

procedure TdxRichEditControlUIGenerator.AddCommandOrientation(ACategoryInfo: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategoryInfo.Add(dxRichEditSubItemOrientationPictureName,
    sdxRichEditSubItemOrientation, [ugivlLargeIcon]);
  ACommand.Add(TdxRichEditControlSetPortraitPageOrientation);
  ACommand.Add(TdxRichEditControlSetLandscapePageOrientation);
end;

procedure TdxRichEditControlUIGenerator.AddCommandPosition(ACategoryInfo: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategoryInfo.Add(TdxRichEditControlCommandsImages.ImagePositionTopRight,
    sdxRichEditSubItemPosition, [ugivlLargeIcon]);
  ACommand.Add(TdxRichEditControlSetFloatingObjectTopLeftAlignment);
  ACommand.Add(TdxRichEditControlSetFloatingObjectTopCenterAlignment);
  ACommand.Add(TdxRichEditControlSetFloatingObjectTopRightAlignment);
  ACommand.Add(TdxRichEditControlSetFloatingObjectMiddleLeftAlignment);
  ACommand.Add(TdxRichEditControlSetFloatingObjectMiddleCenterAlignment);
  ACommand.Add(TdxRichEditControlSetFloatingObjectMiddleRightAlignment);
  ACommand.Add(TdxRichEditControlSetFloatingObjectBottomLeftAlignment);
  ACommand.Add(TdxRichEditControlSetFloatingObjectBottomCenterAlignment);
  ACommand.Add(TdxRichEditControlSetFloatingObjectBottomRightAlignment);
end;

procedure TdxRichEditControlUIGenerator.AddCommandSendToBack(ACategoryInfo: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategoryInfo.Add(TdxRichEditControlCommandsImages.SendBackward,
    sdxRichEditSubItemSendToBack, [ugivlLargeIcon]);
  ACommand.Add(TdxRichEditControlFloatingObjectSendBackward);
  ACommand.Add(TdxRichEditControlFloatingObjectSendToBack);
  ACommand.Add(TdxRichEditControlFloatingObjectSendBehindText);
end;

procedure TdxRichEditControlUIGenerator.AddCommandWrapText(ACategoryInfo: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategoryInfo.Add(TdxRichEditControlCommandsImages.WrapTextTight,
    sdxRichEditSubItemWrapText, [ugivlLargeIcon]);
  ACommand.Add(TdxRichEditControlSetFloatingObjectSquareTextWrapType);
  ACommand.Add(TdxRichEditControlSetFloatingObjectTightTextWrapType);
  ACommand.Add(TdxRichEditControlSetFloatingObjectThroughTextWrapType);
  ACommand.Add(TdxRichEditControlSetFloatingObjectTopAndBottomTextWrapType);
  ACommand.Add(TdxRichEditControlSetFloatingObjectBehindTextWrapType);
  ACommand.Add(TdxRichEditControlSetFloatingObjectInFrontOfTextWrapType);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryAlignment;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabTableLayout,
    sdxRichEditBarAlignment, dxRichEditBarAlignmentPictureName);
  ACategoryInfo.Add(TdxRichEditControlToggleTableCellsTopLeftAlignment, [ugivlSmallIcon]);
  ACategoryInfo.Add(TdxRichEditControlToggleTableCellsTopCenterAlignment, [ugivlSmallIcon],
    ugigpNone, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleTableCellsTopRightAlignment, [ugivlSmallIcon],
    ugigpNone, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleTableCellsMiddleLeftAlignment, [ugivlSmallIcon]);
  ACategoryInfo.Add(TdxRichEditControlToggleTableCellsMiddleCenterAlignment, [ugivlSmallIcon],
    ugigpNone, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleTableCellsMiddleRightAlignment, [ugivlSmallIcon],
    ugigpNone, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleTableCellsBottomLeftAlignment, [ugivlSmallIcon]);
  ACategoryInfo.Add(TdxRichEditControlToggleTableCellsBottomCenterAlignment, [ugivlSmallIcon],
    ugigpNone, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleTableCellsBottomRightAlignment, [ugivlSmallIcon],
    ugigpNone, ugipContinuesRow);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryArrange;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabPictureFormat, sdxRichEditBarArrange,
    dxRichEditBarArrangePictureName);
  AddCommandWrapText(ACategoryInfo);
  AddCommandPosition(ACategoryInfo);
  AddCommandBringToFront(ACategoryInfo);
  AddCommandSendToBack(ACategoryInfo);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryBackground;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabPageLayout, sdxRichEditBarPageBackground,
    dxRichEditBarPageBackgroundPictureName);
  ACategoryInfo.Add(TdxRichEditControlChangePageColor);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryCaptions;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabReferences, sdxRichEditBarCaptions,
    '');
  ACommand := ACategoryInfo.Add(TdxRichEditControlInsertCaptionPlaceholder);
  ACommand.Add(TdxRichEditControlInsertFigureCaption);
  ACommand.Add(TdxRichEditControlInsertTableCaption);
  ACommand.Add(TdxRichEditControlInsertEquationCaption);
  ACommand := ACategoryInfo.Add(TdxRichEditControlInsertTableOfFiguresPlaceholder);
  ACommand.Add(TdxRichEditControlInsertTableOfFigures);
  ACommand.Add(TdxRichEditControlInsertTableOfTables);
  ACommand.Add(TdxRichEditControlInsertTableOfEquations);
  ACategoryInfo.Add(TdxRichEditControlUpdateTableOfFigures);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryCellSize;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabTableLayout,
    sdxRichEditBarCellSize, dxRichEditBarCellSizePictureName);
  AddCommandAutoFit(ACategoryInfo);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryClipboard;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabHome, sdxRichEditBarClipboard,
    dxRichEditBarClipboardPictureName);
  ACategoryInfo.Add(TdxRichEditControlPasteSelection);
  ACategoryInfo.Add(TdxRichEditControlCutSelection, [ugivlSmallIcon, ugivlText]);
  ACategoryInfo.Add(TdxRichEditControlCopySelection, [ugivlSmallIcon, ugivlText]);
  ACategoryInfo.Add(TdxRichEditControlSelectAll, [ugivlSmallIcon, ugivlText]);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryClose;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabHeaderAndFooterDesign,
    sdxRichEditBarClose, dxRichEditBarClosePictureName);
  ACategoryInfo.Add(TdxRichEditControlClosePageHeaderFooter);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryCommon;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabFile, sdxRichEditBarCommon,
    dxRichEditBarCommonPictureName);
  ACategoryInfo.Add(TdxRichEditControlNewDocument);
  ACategoryInfo.Add(TdxRichEditControlLoadDocument);
  ACategoryInfo.Add(TdxRichEditControlSaveDocument);
  ACategoryInfo.Add(TdxRichEditControlSaveDocumentAs);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryDocumentViews;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabView, sdxRichEditBarDocumentViews, dxRichEditBarDocumentViewsPictureName);
  ACategoryInfo.Add(TdxRichEditControlSwitchToSimpleView);
  ACategoryInfo.Add(TdxRichEditControlSwitchToDraftView);
  ACategoryInfo.Add(TdxRichEditControlSwitchToPrintLayoutView);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryEditing;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabHome, sdxRichEditBarEditing,
    dxRichEditBarEditingPictureName);
  ACategoryInfo.Add(TdxRichEditControlSearchFind, [ugivlSmallIcon, ugivlText]);
  ACategoryInfo.Add(TdxRichEditControlSearchReplace, [ugivlSmallIcon, ugivlText]);
  ACategoryInfo.Add(TdxRichEditControlUndo, [ugivlLargeIcon]);
  ACategoryInfo.Add(TdxRichEditControlRedo, [ugivlLargeIcon]);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryFont;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabHome, sdxRichEditBarFont, dxRichEditBarFontPictureName);
  ACategoryInfo.Add(TdxRichEditControlChangeFontName, [ugivlText]);
  ACategoryInfo.Add(TdxRichEditControlChangeFontSize, [ugivlText], ugigpNone, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlIncreaseFontSize, [ugivlSmallIcon], ugigpStart, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlDecreaseFontSize, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  AddCommandChangeCase(ACategoryInfo);
  ACategoryInfo.Add(TdxRichEditControlToggleFontBold, [ugivlSmallIcon], ugigpStart);
  ACategoryInfo.Add(TdxRichEditControlToggleFontItalic, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleFontUnderline, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleFontDoubleUnderline, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleFontStrikeout, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleFontDoubleStrikeout, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleFontSubscript, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleFontSuperscript, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlTextHighlight, [ugivlSmallIcon], ugigpStart, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlChangeFontColor, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryHeaderAndFooter;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabInsert, sdxRichEditBarHeaderAndFooter,
    dxRichEditBarHeaderAndFooterPictureName);
  ACategoryInfo.Add(TdxRichEditControlEditPageHeader);
  ACategoryInfo.Add(TdxRichEditControlEditPageFooter);
  ACategoryInfo.Add(TdxRichEditControlInsertPageNumberField);
  ACategoryInfo.Add(TdxRichEditControlInsertPageCountField);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryIllustrations;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabInsert, sdxRichEditBarIllustrations,
    dxRichEditBarIllustrationsPictureName);
  ACategoryInfo.Add(TdxRichEditControlInsertPicture);
  ACategoryInfo.Add(TdxRichEditControlInsertFloatingObjectPicture);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryLinks;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabInsert, sdxRichEditBarLinks,
    dxRichEditBarLinksPictureName);
  ACategoryInfo.Add(TdxRichEditControlShowBookmarkForm);
  ACategoryInfo.Add(TdxRichEditControlShowHyperlinkForm);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryMailMerge;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabMailMerge, sdxRichEditBarMailMerge,
    dxRichEditTabMailMergePictureName);
  ACategoryInfo.Add(TdxRichEditControlShowInsertMergeFieldForm);
  ACategoryInfo.Add(TdxRichEditControlShowAllFieldCodes);
  ACategoryInfo.Add(TdxRichEditControlShowAllFieldResults);
  ACategoryInfo.Add(TdxRichEditControlToggleViewMergedData);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryMerge;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabTableLayout,
    sdxRichEditBarMerge, dxRichEditBarMergePictureName);
  ACategoryInfo.Add(TdxRichEditControlMergeTableCells);
  ACategoryInfo.Add(TdxRichEditControlShowSplitTableCellsForm);
  ACategoryInfo.Add(TdxRichEditControlSplitTable);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryNavigation;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabHeaderAndFooterDesign,
    sdxRichEditBarNavigation, dxRichEditBarNavigationPictureName);
  ACategoryInfo.Add(TdxRichEditControlGoToPageHeader);
  ACategoryInfo.Add(TdxRichEditControlGoToPageFooter);
  ACategoryInfo.Add(TdxRichEditControlGoToNextPageHeaderFooter);
  ACategoryInfo.Add(TdxRichEditControlGoToPreviousPageHeaderFooter);
  ACategoryInfo.Add(TdxRichEditControlToggleHeaderFooterLinkToPrevious);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryOptions;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabHeaderAndFooterDesign,
    sdxRichEditBarOptions, dxRichEditBarOptionsPictureName);
  ACategoryInfo.Add(TdxRichEditControlToggleDifferentFirstPage);
  ACategoryInfo.Add(TdxRichEditControlToggleDifferentOddAndEvenPages);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryPages;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabInsert, sdxRichEditBarPages,
    dxRichEditBarPagesPictureName);
  ACategoryInfo.Add(TdxRichEditControlInsertPageBreak);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryPageSetup;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
  AGallery: TdxUIGeneratorGalleryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabPageLayout, sdxRichEditBarPageSetup,
    dxRichEditBarPageSetupPictureName);
  AGallery := ACategoryInfo.Add(TdxRichEditControlPageMarginsGallery, True, True, True, False, 1, 2);
  AGallery.Add(TdxRichEditControlShowPageMarginsSetupForm);
  AddCommandOrientation(ACategoryInfo);
  AGallery := ACategoryInfo.Add(TdxRichEditControlPaperSizeGallery, True, True, True, False, 1, 1);
  AGallery.Add(TdxRichEditControlShowPagePaperSetupForm);
  AddCommandColumns(ACategoryInfo);
  AddCommandBreaks(ACategoryInfo);
  AddCommandLineNumbers(ACategoryInfo);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryParagraph;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabHome, sdxRichEditBarParagraph, dxRichEditBarParagraphPictureName);
  ACategoryInfo.Add(TdxRichEditControlToggleBulletedList, [ugivlSmallIcon], ugigpStart);
  ACategoryInfo.Add(TdxRichEditControlToggleSimpleNumberingList, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleMultiLevelList, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlDecrementIndent, [ugivlSmallIcon], ugigpStart, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlIncrementIndent, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleShowWhitespace, [ugivlSmallIcon], ugigpStart, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleParagraphAlignmentLeft, [ugivlSmallIcon], ugigpStart);
  ACategoryInfo.Add(TdxRichEditControlToggleParagraphAlignmentCenter, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleParagraphAlignmentRight, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  ACategoryInfo.Add(TdxRichEditControlToggleParagraphAlignmentJustify, [ugivlSmallIcon], ugigpMember, ugipContinuesRow);
  AddCommandLineSpacing(ACategoryInfo);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryProtect;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabReview, sdxRichEditBarProtect, dxRichEditBarEditRangePermission);
  ACategoryInfo.Add(TdxRichEditControlProtectDocument);
  ACategoryInfo.Add(TdxRichEditControlUnprotectDocument);
  ACategoryInfo.Add(TdxRichEditControlShowRangeEditingPermissions);
  ACategoryInfo.Add(TdxRichEditControlEncryptDocument);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryRowAndColumns;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabTableLayout,
    sdxRichEditBarRowAndColumns, dxRichEditBarRowAndColumnsPictureName);
  AddCommandDelete(ACategoryInfo);
  ACategoryInfo.Add(TdxRichEditControlInsertTableRowAbove);
  ACategoryInfo.Add(TdxRichEditControlInsertTableRowBelow);
  ACategoryInfo.Add(TdxRichEditControlInsertTableColumnToTheLeft);
  ACategoryInfo.Add(TdxRichEditControlInsertTableColumnToTheRight);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryShapeStyles;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabPictureFormat, sdxRichEditBarShapeStyles,
    dxRichEditBarShapeStylesPictureName);
  ACategoryInfo.Add(TdxRichEditControlChangeFloatingObjectFillColor, [ugivlSmallIcon, ugivlText]);
  ACategoryInfo.Add(TdxRichEditControlChangeFloatingObjectOutlineColor, [ugivlSmallIcon, ugivlText]);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryShow;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabView, sdxRichEditBarShow, dxRichEditBarShowPictureName);
  ACategoryInfo.Add(TdxRichEditControlToggleShowHorizontalRuler);
  ACategoryInfo.Add(TdxRichEditControlToggleShowVerticalRuler);
end;

procedure TdxRichEditControlUIGenerator.AddCategorySpelling;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabReview, sdxRichEditBarProofing, dxRichEditBarSpellCheckPictureName);
  ACategoryInfo.Add(TdxRichEditControlCheckSpelling);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryStyles;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabHome, sdxRichEditBarStyles, dxRichEditBarStylesPictureName);
  ACategoryInfo.Add(TdxRichEditControlQuickStylesGallery, False, False, False, True, 4);
end;

procedure TdxRichEditControlUIGenerator.AddCategorySymbols;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabInsert, sdxRichEditBarSymbols,  dxRichEditBarSymbolsPictureName);
  ACategoryInfo.Add(TdxRichEditControlShowSymbolForm);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryTable;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabTableLayout, sdxRichEditBarTable, dxRichEditBarTablePictureName);
  ACategoryInfo.Add(TdxRichEditControlToggleShowTableGridLines);
  ACategoryInfo.Add(TdxRichEditControlShowTablePropertiesForm);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryTableOfContents;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabReferences, sdxRichEditBarTableOfContents,
    '');
  ACategoryInfo.Add(TdxRichEditControlInsertTableOfContents);
  ACategoryInfo.Add(TdxRichEditControlUpdateTableOfContents);
  ACommand := ACategoryInfo.Add(TdxRichEditControlAddParagraphsToTableOfContentsPlaceholder);
  ACommand.Add(TdxRichEditControlTableOfContentsSetParagraphBodyTextLevel);
  ACommand.Add(TdxRichEditControlTableOfContentsSetParagraphHeading1Level);
  ACommand.Add(TdxRichEditControlTableOfContentsSetParagraphHeading2Level);
  ACommand.Add(TdxRichEditControlTableOfContentsSetParagraphHeading3Level);
  ACommand.Add(TdxRichEditControlTableOfContentsSetParagraphHeading4Level);
  ACommand.Add(TdxRichEditControlTableOfContentsSetParagraphHeading5Level);
  ACommand.Add(TdxRichEditControlTableOfContentsSetParagraphHeading6Level);
  ACommand.Add(TdxRichEditControlTableOfContentsSetParagraphHeading7Level);
  ACommand.Add(TdxRichEditControlTableOfContentsSetParagraphHeading8Level);
  ACommand.Add(TdxRichEditControlTableOfContentsSetParagraphHeading9Level);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryTables;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabInsert, sdxRichEditBarTables, dxRichEditBarTablesPictureName);
  ACategoryInfo.Add(TdxRichEditControlShowInsertTableForm);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryTableStyles;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabTableDesign, sdxRichEditBarTableStyles,
    dxRichEditBarTableStylesPictureName);
  ACategoryInfo.Add(TdxRichEditControlTableStylesGallery, False, False, False, True, 7);
  AddCommandBorders(ACategoryInfo);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryText;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabInsert, sdxRichEditBarText, dxRichEditBarTextPictureName);
  ACategoryInfo.Add(TdxRichEditControlInsertTextBox);
end;

procedure TdxRichEditControlUIGenerator.AddCategoryZoom;
var
  ACategoryInfo: TdxUIGeneratorCategoryInfo;
begin
  ACategoryInfo := ComponentInfo.Add(sdxRichEditTabView, sdxRichEditBarZoom,
    dxRichEditBarZoomPictureName);
  ACategoryInfo.Add(TdxRichEditControlZoomOut);
  ACategoryInfo.Add(TdxRichEditControlZoomIn);
end;

procedure TdxRichEditControlUIGenerator.AddTabs;
begin
  AddTabFile;
  AddTabHome;
  AddTabInsert;
  AddTabPageLayout;
  AddTabReferences;
  AddTabMailMerge;
  AddTabReview;
  AddTabView;
  AddTabHeaderAndFooterDesign;
  AddTabTableLayout;
  AddTabTableDesign;
  AddTabPictureFormat;
end;

procedure TdxRichEditControlUIGenerator.AddTabFile;
begin
  AddCategoryCommon;
end;

procedure TdxRichEditControlUIGenerator.AddTabHeaderAndFooterDesign;
begin
  AddCategoryNavigation;
  AddCategoryOptions;
  AddCategoryClose;
end;

procedure TdxRichEditControlUIGenerator.AddTabHome;
begin
  AddCategoryClipboard;
  AddCategoryFont;
  AddCategoryParagraph;
  AddCategoryStyles;
  AddCategoryEditing;
end;

procedure TdxRichEditControlUIGenerator.AddTabInsert;
begin
  AddCategoryPages;
  AddCategoryTables;
  AddCategoryIllustrations;
  AddCategoryLinks;
  AddCategoryHeaderAndFooter;
  AddCategoryText;
  AddCategorySymbols;
end;

procedure TdxRichEditControlUIGenerator.AddTabMailMerge;
begin
  AddCategoryMailMerge;
end;

procedure TdxRichEditControlUIGenerator.AddTabPageLayout;
begin
  AddCategoryPageSetup;
  AddCategoryBackground;
end;

procedure TdxRichEditControlUIGenerator.AddTabReferences;
begin
  AddCategoryTableOfContents;
  AddCategoryCaptions;
end;

procedure TdxRichEditControlUIGenerator.AddTabPictureFormat;
begin
  AddCategoryShapeStyles;
  AddCategoryArrange;
end;

procedure TdxRichEditControlUIGenerator.AddTabTableDesign;
begin
  AddCategoryTableStyles;
end;

procedure TdxRichEditControlUIGenerator.AddTabTableLayout;
begin
  AddCategoryTable;
  AddCategoryRowAndColumns;
  AddCategoryMerge;
  AddCategoryCellSize;
  AddCategoryAlignment;
end;

procedure TdxRichEditControlUIGenerator.AddTabReview;
begin
  AddCategorySpelling;
  AddCategoryProtect;
end;

procedure TdxRichEditControlUIGenerator.AddTabView;
begin
  AddCategoryDocumentViews;
  AddCategoryShow;
  AddCategoryZoom;
end;

procedure GenerateUI(AComponentInfo: TdxUIGeneratorComponentInfo);
var
  AGenerator: TdxRichEditControlUIGenerator;
begin
  AGenerator := TdxRichEditControlUIGenerator.Create(AComponentInfo);
  try
    AGenerator.AddTabs;
  finally
    AGenerator.Free;
  end;
end;

procedure RegisterRichEditUIGeneratorScheme;
begin
  GenerateUI(TdxUIGenerator.RegisterComponent(TdxRichEditControl, sdxRichEditControlCategory));
end;

end.
