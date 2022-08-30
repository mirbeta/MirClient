{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSRes;

interface

{$I cxVer.inc}

resourcestring
  sdxBtnOK = 'OK';
  sdxBtnOKAccelerated = '&OK';
  sdxBtnCancel = 'Cancel';
  sdxBtnClose = 'Close';
  sdxBtnApply = '&Apply';
  sdxBtnHelp = '&Help';
  sdxBtnFix = '&Fix';
  sdxBtnNew = '&New...';
  sdxBtnIgnore = '&Ignore';
  sdxBtnYes = '&Yes';
  sdxBtnNo = '&No';
  sdxBtnEdit = '&Edit...';
  sdxBtnReset = '&Reset';
  sdxBtnAdd = '&Add';
  sdxBtnAddComposition = 'Add &Composition';
  sdxBtnDefault = '&Default...';
  sdxBtnDelete = '&Delete...';
  sdxBtnDescription = '&Description...';
  sdxBtnCopy = '&Copy...';
  sdxBtnYesToAll = 'Yes To &All';
  sdxBtnFootnoteProperties = 'Footnote Properties...';
  sdxBtnRestoreDefaults = '&Restore Defaults';
  sdxBtnRestoreOriginal = 'Restore &Original';
  sdxBtnTitleProperties = 'Title Properties...';
  sdxBtnProperties = 'P&roperties...';
  sdxBtnNetwork = 'Net&work...';
  sdxBtnBrowse = '&Browse...';
  sdxBtnPageSetup = 'Pa&ge Setup...';
  sdxBtnPrintPreview = 'Print Pre&view...';
  sdxBtnPreview = 'Pre&view...';
  sdxBtnPrint = 'Print...';
  sdxBtnOptions = '&Options...';
  sdxBtnStyleOptions = 'Style Options...';
  sdxBtnDefinePrintStyles = '&Define Styles...';
  sdxBtnPrintStyles = 'Print Styles';
  sdxBtnBackground = 'Background';
  sdxBtnShowToolBar = 'Show &ToolBar';
  sdxBtnDesign = 'D&esign...';
  sdxBtnMoveUp = 'Move &Up';
  sdxBtnMoveDown = 'Move Dow&n';

  sdxBtnMoreColors = '&More Colors...';
  sdxBtnFillEffects = '&Fill Effects...';
  sdxBtnNoFill = '&No Fill';
  sdxBtnAutomatic = '&Automatic';
  sdxBtnNone = '&None';

  sdxBtnOtherTexture = 'Other Te&xture...';
  sdxBtnInvertColors = 'I&nvert Colors';
  sdxBtnSelectPicture = 'Se&lect Picture...';

  sdxEditReports = 'Edit Reports';
  sdxComposition = 'Composition';
  sdxReportFootnotesDlgCaption = 'Report Footnotes';
  sdxReportTitleDlgCaption = 'Report Title';
  sdxMode = '&Mode:';
  sdxText = '&Text';
  sdxProperties = '&Properties';
  sdxAdjustOnScale = '&Adjust on Scale';

  // Report Title mode
  sdxTitleModeNone = 'None';
  sdxTitleModeOnEveryTopPage = 'On Every Top Page';
  sdxTitleModeOnFirstPage = 'On First Page';

  // Report Footnotes mode
  sdxFootnotesModeNone = 'None';
  sdxFootnotesModeOnEveryBottomPage = 'On Every Bottom Page';
  sdxFootnotesModeOnLastPage = 'On Last Page';

  sdxEditDescription = 'Edit Description';
  sdxRename = 'Rena&me';
  sdxSelectAll = '&Select All';

  sdxAddReport = 'Add Report';
  sdxAddAndDesignReport = 'Add and D&esign Report...';
  sdxNewCompositionCaption = 'New Composition';
  sdxName = '&Name:';
  sdxCaption = '&Caption:';
  sdxAvailableSources = '&Available Source(s)';
  sdxOnlyComponentsInActiveForm = 'Only Components in Active &Form';
  sdxOnlyComponentsWithoutLinks = 'Only Components &without Existing ReportLinks';
  sdxItemName = 'Name';
  sdxItemDescription = 'Description';

  sdxConfirmDeleteItem = 'Do you want to delete next items: %s ?';
  sdxAddItemsToComposition = 'Add Items to Composition';
  sdxHideAlreadyIncludedItems = 'Hide Already &Included Items';
  sdxAvailableItems = 'A&vailable Items';
  sdxItems = '&Items';
  sdxEnable = '&Enable';
  sdxOptions = 'Options';
  sdxGroupImages = 'Images';
  sdxGroupView = 'View';
  sdxShow = 'Show';
  sdxPaintItemsGraphics = '&Paint Item Graphics';
  sdxDescription = '&Description:';

  sdxNewReport = 'NewReport';

  sdxOnlySelected = 'Only &Selected';
  sdxExtendedSelect = '&Extended Select';
  sdxIncludeFixed = '&Include Fixed';

  sdxFonts = 'Fonts';
  sdxBtnFont = 'Fo&nt...';
  sdxBtnEvenFont = 'E&ven Font...';
  sdxBtnOddFont = 'Odd Fo&nt...';
  sdxBtnFixedFont = 'F&ixed Font...';
  sdxBtnGroupFont = 'Grou&p Font...';
  sdxBtnChangeFont = 'Change Fo&nt...';

  sdxFont = 'Font';
  sdxOddFont = 'Odd Font';
  sdxEvenFont = 'Even Font';
  sdxPreviewFont = 'Preview Font';
  sdxCaptionNodeFont = 'Level Caption Font';
  sdxGroupNodeFont = 'Group Node Font';
  sdxGroupFooterFont = 'Group Footer Font';
  sdxHeaderFont = 'Header Font';
  sdxFooterFont = 'Footer Font';
  sdxBandFont = 'Band Font';

  sdxTransparent = '&Transparent';
  sdxFixedTransparent = 'Fi&xed Transparent';
  sdxCaptionTransparent = 'Caption Transparent';
  sdxGroupTransparent = 'Group Transparent';

  sdxGraphicAsTextValue = '(GRAPHIC)';
  sdxColors = 'Colors';
  sdxColor = 'Co&lor:';
  sdxOddColor = 'Odd Co&lor:';
  sdxEvenColor = 'E&ven Color:';
  sdxPreviewColor = '&Preview Color:';
  sdxBandColor = '&Band Color:';
  sdxLevelCaptionColor = 'Le&vel Caption Color:';
  sdxHeaderColor = 'H&eader Color:';
  sdxGroupNodeColor = 'Group &Node Color:';
  sdxGroupFooterColor = '&Group Footer Color:';
  sdxFooterColor = 'Foo&ter Color:';
  sdxFixedColor = 'F&ixed Color:';
  sdxGroupColor = 'Grou&p Color:';
  sdxCaptionColor = 'Caption Color:';
  sdxGridLinesColor = 'Gri&d Line Color:';

  sdxBands = '&Bands';
  sdxLevelCaptions = 'Levels &Caption';
  sdxHeaders = 'H&eaders';
  sdxFooters = 'Foote&rs';
  sdxGroupFooters = '&Group Footers';
  sdxPreview = 'Previe&w';
  sdxPreviewLineCount = 'Preview Line Coun&t:';
  sdxAutoCalcPreviewLineCount = 'A&uto Calculate Preview Lines';

  sdxGrid = 'Grid Lines';
  sdxNodesGrid = 'Node Grid Lines';
  sdxGroupFooterGrid = 'GroupFooter Grid Lines';

  sdxStateImages = '&State Images';
  sdxImages = '&Images';

  sdxTextAlign = 'Text&Align';
  sdxTextAlignHorz = 'Hori&zontally';
  sdxTextAlignVert = '&Vertically';
  sdxTextAlignLeft = 'Left';
  sdxTextAlignCenter = 'Center';
  sdxTextAlignRight = 'Right';
  sdxTextAlignTop = 'Top';
  sdxTextAlignVCenter = 'Center';
  sdxTextAlignBottom = 'Bottom';
  sdxBorderLines = '&Border';
  sdxHorzLines = 'Hori&zontal Lines';
  sdxVertLines = '&Vertical Lines';
  sdxFixedHorzLines = 'Fi&xed Horizontal Lines';
  sdxFixedVertLines = 'Fixe&d Vertical Lines';
  sdxFlatCheckMarks = 'F&lat CheckMarks';
  sdxCheckMarksAsText = '&Display CheckMarks as Text';

  sdxRowAutoHeight = 'Ro&w AutoHeight';
  sdxEndEllipsis = '&EndEllipsis';

  sdxDrawBorder = '&Draw Border';
  sdxFullExpand = 'Full &Expand';
  sdxBorderColor = '&Border Color:';
  sdxAutoNodesExpand = 'A&uto Nodes Expand';
  sdxExpandLevel = 'Expand &Level:';
  sdxFixedRowOnEveryPage = 'Fixed Rows';

  sdxDrawMode = 'Draw &Mode:';
  sdxDrawModeStrict = 'Strict';
  sdxDrawModeOddEven = 'Odd/Even Rows Mode';
  sdxDrawModeChess = 'Chess Mode';
  sdxDrawModeBorrow = 'Borrow From Source';

  sdx3DEffects = '3D Effects';
  sdxUse3DEffects = 'Use &3D Effects';
  sdxSoft3D = 'Sof&t3D';

  sdxBehaviors = 'Behaviors';
  sdxMiscellaneous = 'Miscellaneous';
  sdxOnEveryPage = 'On Every Page';
  sdxNodeExpanding = 'Node Expanding';
  sdxSelection = 'Selection';
  sdxNodeAutoHeight = '&Node Auto Height';
  sdxTransparentGraphics = '&Transparent Graphics';
  sdxAutoWidth = 'Auto &Width';

  sdxDisplayGraphicsAsText = 'Display Graphic As &Text';
  sdxTransparentColumnGraphics = 'Transparent &Graphics';
  sdxDisplayTrackBarsAsText = 'Display Track &Bars As Text';

  sdxBandsOnEveryPage = 'Bands';
  sdxHeadersOnEveryPage = 'Headers';
  sdxFootersOnEveryPage = 'Footers';
  sdxGraphics = '&Graphics';

  { Common messages }

  sdxOutOfResources = 'Out of Resources';
  sdxFileAlreadyExists = 'File "%s" Already Exists.';
  sdxConfirmOverWrite = 'File "%s" already exists. Overwrite ?';
  sdxInvalidFileName = 'Invalid File Name "%s"';
  sdxRequiredFileName = 'Enter file name.';
  sdxOutsideMarginsMessage =
    'One or more margins are set outside the printable area of the page.' + #13#10 +
    'Do you want to continue ?';
  sdxOutsideMarginsMessage2 =
    'One or more margins are set outside the printable area of the page.' + #13#10 +
    'Choose the Fix button to increase the appropriate margins.';
  sdxInvalidMarginsMessage =
    'One or more margins are set to the invalid values.' + #13#10 +
    'Choose the Fix button to correct this problem.' + #13#10 +
    'Choose the Restore button to restore original values.';
  sdxInvalidMargins = 'One or more margins have invalid values';
  sdxOutsideMargins = 'One or more margins are set outside the printable area of the page';
  sdxReportCellClassNotRegistered = '%s class is not registered. ' +
    'Make sure that the corresponding report link unit is added to the application';
  sdxThereAreNowItemsForShow = 'There are no items in this view';

  { Color palette }

  sdxPageBackground = ' Page Background';
  sdxPenColor = 'Pen Color';
  sdxFontColor = 'Font Color';
  sdxBrushColor = 'Brush Color';
  sdxHighLight = 'HighLight';

  { Color names }

  sdxColorBlack = 'Black';
  sdxColorDarkRed = 'Dark Red';
  sdxColorRed = 'Red';
  sdxColorPink = 'Pink';
  sdxColorRose = 'Rose';
  sdxColorBrown = 'Brown';
  sdxColorOrange = 'Orange';
  sdxColorLightOrange = 'Light Orange';
  sdxColorGold = 'Gold';
  sdxColorTan = 'Tan';
  sdxColorOliveGreen = 'Olive Green';
  sdxColorDrakYellow = 'Dark Yellow';
  sdxColorLime = 'Lime';
  sdxColorYellow = 'Yellow';
  sdxColorLightYellow = 'Light Yellow';
  sdxColorDarkGreen = 'Dark Green';
  sdxColorGreen = 'Green';
  sdxColorSeaGreen = 'Sea Green';
  sdxColorBrighthGreen = 'Bright Green';
  sdxColorLightGreen = 'Light Green';
  sdxColorDarkTeal = 'Dark Teal';
  sdxColorTeal = 'Teal';
  sdxColorAqua = 'Aqua';
  sdxColorTurquoise = 'Turquoise';
  sdxColorLightTurquoise = 'Light Turquoise';
  sdxColorDarkBlue = 'Dark Blue';
  sdxColorBlue = 'Blue';
  sdxColorLightBlue = 'Light Blue';
  sdxColorSkyBlue = 'Sky Blue';
  sdxColorPaleBlue = 'Pale Blue';
  sdxColorIndigo = 'Indigo';
  sdxColorBlueGray = 'Blue Gray';
  sdxColorViolet = 'Violet';
  sdxColorPlum = 'Plum';
  sdxColorLavender = 'Lavender';
  sdxColorGray80 = 'Gray-80%';
  sdxColorGray50 = 'Gray-50%';
  sdxColorGray40 = 'Gray-40%';
  sdxColorGray25 = 'Gray-25%';
  sdxColorWhite = 'White';

  { FEF Dialog }

  sdxTexture = '&Texture';
  sdxPattern = '&Pattern';
  sdxPicture = 'P&icture';
  sdxForeground = '&Foreground';
  sdxBackground = '&Background';
  sdxSample = 'Sample:';

  sdxFEFCaption = 'Fill Effects';
  sdxPaintMode = 'Paint &Mode';
  sdxPaintModeCenter = 'Center';
  sdxPaintModeStretch = 'Stretch';
  sdxPaintModeTile = 'Tile';
  sdxPaintModeProportional = 'Proportional';

  { Pattern names }

  sdxPatternGray5 = '5%';
  sdxPatternGray10 = '10%';
  sdxPatternGray20 = '20%';
  sdxPatternGray25 = '25%';
  sdxPatternGray30 = '30%';
  sdxPatternGray40 = '40%';
  sdxPatternGray50 = '50%';
  sdxPatternGray60 = '60%';
  sdxPatternGray70 = '70%';
  sdxPatternGray75 = '75%';
  sdxPatternGray80 = '80%';
  sdxPatternGray90 = '90%';
  sdxPatternLightDownwardDiagonal = 'Light downward diagonal';
  sdxPatternLightUpwardDiagonal = 'Light upward diagonal';
  sdxPatternDarkDownwardDiagonal = 'Dark downward diagonal';
  sdxPatternDarkUpwardDiagonal = 'Dark upward diagonal';
  sdxPatternWideDownwardDiagonal = 'Wide downward diagonal';
  sdxPatternWideUpwardDiagonal = 'Wide upward diagonal';
  sdxPatternLightVertical = 'Light vertical';
  sdxPatternLightHorizontal = 'Light horizontal';
  sdxPatternNarrowVertical = 'Narrow vertical';
  sdxPatternNarrowHorizontal = 'Narrow horizontal';
  sdxPatternDarkVertical = 'Dark vertical';
  sdxPatternDarkHorizontal = 'Dark horizontal';
  sdxPatternDashedDownward = 'Dashed downward';
  sdxPatternDashedUpward = 'Dashed upward';
  sdxPatternDashedVertical = 'Dashed vertical';
  sdxPatternDashedHorizontal = 'Dashed horizontal';
  sdxPatternSmallConfetti = 'Small confetti';
  sdxPatternLargeConfetti = 'Large confetti';
  sdxPatternZigZag = 'Zig zag';
  sdxPatternWave = 'Wave';
  sdxPatternDiagonalBrick = 'Diagonal brick';
  sdxPatternHorizantalBrick = 'Horizontal brick';
  sdxPatternWeave = 'Weave';
  sdxPatternPlaid = 'Plaid';
  sdxPatternDivot = 'Divot';
  sdxPatternDottedGrid = 'Dottedgrid';
  sdxPatternDottedDiamond = 'Dotted diamond';
  sdxPatternShingle = 'Shingle';
  sdxPatternTrellis = 'Trellis';
  sdxPatternSphere = 'Sphere';
  sdxPatternSmallGrid = 'Small grid';
  sdxPatternLargeGrid = 'Large grid';
  sdxPatternSmallCheckedBoard = 'Small checked board';
  sdxPatternLargeCheckedBoard = 'Large checked board';
  sdxPatternOutlinedDiamond = 'Outlined diamond';
  sdxPatternSolidDiamond = 'Solid diamond';

  { Texture names }

  sdxTextureNewSprint = 'Newsprint';
  sdxTextureGreenMarble = 'Green marble';
  sdxTextureBlueTissuePaper = 'Blue tissue paper';
  sdxTexturePapyrus = 'Papyrus';
  sdxTextureWaterDroplets = 'Water droplets';
  sdxTextureCork = 'Cork';
  sdxTextureRecycledPaper = 'Recycled paper';
  sdxTextureWhiteMarble = 'White marble';
  sdxTexturePinkMarble = 'Pink marble';
  sdxTextureCanvas = 'Canvas';
  sdxTexturePaperBag = 'Paper bag';
  sdxTextureWalnut = 'Walnut';
  sdxTextureParchment = 'Parchment';
  sdxTextureBrownMarble = 'Brown marble';
  sdxTexturePurpleMesh = 'Purple mesh';
  sdxTextureDenim = 'Denim';
  sdxTextureFishFossil = 'Fish fossil';
  sdxTextureOak = 'Oak';
  sdxTextureStationary = 'Stationary';
  sdxTextureGranite = 'Granite';
  sdxTextureBouquet = 'Bouquet';
  sdxTextureWonenMat = 'Woven mat';
  sdxTextureSand = 'Sand';
  sdxTextureMediumWood = 'Medium wood';

  sdxFSPCaption = 'Picture Preview';
  sdxWidth = 'Width';
  sdxHeight = 'Height';

  { Brush Dialog }

  sdxBrushDlgCaption = 'Brush properties';
  sdxStyle = '&Style:';

  { Enter New File Name dialog }

  sdxENFNCaption = 'Choose New File Name';
  sdxEnterNewFileName = 'Enter New File Name';

  { Define styles dialog }

  sdxDefinePrintStylesCaption = 'Define Print Styles';
  sdxDefinePrintStylesTitle = 'Print &Styles:';
  sdxDefinePrintStylesWarningDelete = 'Do you want to delete "%s" ?';
  sdxDefinePrintStylesWarningClear = 'Do you want to delete all not built-in styles ?';
  sdxClear = 'C&lear...';

  { Print device }

  sdxCustomSize = 'Custom Size';
  sdxDefaultTray = 'Default Tray';
  sdxInvalidPrintDevice = 'Printer selected is not valid';
  sdxNotPrinting = 'Printer is not currently printing';
  sdxPrinting = 'Printing in progress';
  sdxDeviceOnPort = '%s on %s';
  sdxPrinterIndexError = 'Printer index out of range';
  sdxNoDefaultPrintDevice = 'There is no default printer selected';

  { Edit AutoText Entries Dialog }

  sdxAutoTextDialogCaption = 'Edit AutoText Entries';
  sdxEnterAutoTextEntriesHere = ' Enter A&utoText Entries Here: ';

  { Print dialog }

  sdxPrintDialogCaption = 'Print';
  sdxPrintDialogPrinter = ' Printer ';
  sdxPrintDialogName = '&Name:';
  sdxPrintDialogStatus = 'Status:';
  sdxPrintDialogType = 'Type:';
  sdxPrintDialogWhere = 'Where:';
  sdxPrintDialogComment = 'Comment:';
  sdxPrintDialogPrintToFile = 'Print to &File';
  sdxPrintDialogPageRange = ' Page range ';
  sdxPrintDialogAll = '&All';
  sdxPrintDialogCurrentPage = 'Curr&ent Page';
  sdxPrintDialogSelection = '&Selection';
  sdxPrintDialogPages = '&Pages:';
  sdxPrintDialogRangeLegend =
    'Enter page number and/or page ranges' + #13#10 +
    'separated by commas. For example: 1,3,5-12.';
  sdxPrintDialogCopies = ' Copies ';
  sdxPrintDialogNumberOfPages = 'N&umber of Pages:';
  sdxPrintDialogNumberOfCopies = 'Number of &Copies:';
  sdxPrintDialogCollateCopies = 'Colla&te Copies';
  sdxPrintDialogAllPages = 'All';
  sdxPrintDialogEvenPages = 'Even';
  sdxPrintDialogOddPages = 'Odd';
  sdxPrintDialogPrintStyles = ' Print St&yles ';

  { PrintToFile Dialog }

  sdxPrintDialogOpenDlgTitle = 'Choose File Name';
  sdxPrintDialogOpenDlgAllFiles = 'All Files';
  sdxPrintDialogOpenDlgPrinterFiles = 'Printer Files';
  sdxPrintDialogPageNumbersOutOfRange = 'Page numbers out of range (%d - %d)';
  sdxPrintDialogInvalidPageRanges = 'Invalid page ranges';
  sdxPrintDialogRequiredPageNumbers = 'Enter page numbers';
  sdxPrintDialogNoPrinters =
    'No printers are installed. To install a printer, ' +
    'point to Settings on the Windows Start menu, click Printers, and then double-click Add Printer. ' +
    'Follow the instructions in the wizard.';
  sdxPrintDialogInPrintingState = 'Printer is currently printing.' + #13#10 +
    'Please wait.';

  { Printer State }

  sdxPrintDialogPSPaused = 'Paused';
  sdxPrintDialogPSPendingDeletion = 'Pending Deletion';
  sdxPrintDialogPSBusy = 'Busy';
  sdxPrintDialogPSDoorOpen = 'Door Open';
  sdxPrintDialogPSError = 'Error';
  sdxPrintDialogPSInitializing = 'Initializing';
  sdxPrintDialogPSIOActive = 'IO Active';
  sdxPrintDialogPSManualFeed = 'Manual Feed';
  sdxPrintDialogPSNoToner = 'No Toner';
  sdxPrintDialogPSNotAvailable = 'Not Available';
  sdxPrintDialogPSOFFLine = 'Offline';
  sdxPrintDialogPSOutOfMemory = 'Out of Memory';
  sdxPrintDialogPSOutBinFull = 'Output Bin Full';
  sdxPrintDialogPSPagePunt = 'Page Punt';
  sdxPrintDialogPSPaperJam = 'Paper Jam';
  sdxPrintDialogPSPaperOut = 'Paper Out';
  sdxPrintDialogPSPaperProblem = 'Paper Problem';
  sdxPrintDialogPSPrinting = 'Printing';
  sdxPrintDialogPSProcessing = 'Processing';
  sdxPrintDialogPSTonerLow = 'Toner Low';
  sdxPrintDialogPSUserIntervention = 'User Intervention';
  sdxPrintDialogPSWaiting = 'Waiting';
  sdxPrintDialogPSWarningUp = 'Warming Up';
  sdxPrintDialogPSReady = 'Ready';
  sdxPrintDialogPSPrintingAndWaiting = 'Printing: %d document(s) waiting';

  sdxLeftMargin = 'Left Margin';
  sdxTopMargin = 'Top Margin';
  sdxRightMargin = 'Right Margin';
  sdxBottomMargin = 'Bottom Margin';
  sdxGutterMargin = 'Gutter';
  sdxHeaderMargin = 'Header';
  sdxFooterMargin = 'Footer';

  sdxUnitsInches = 'in';
  sdxUnitsCentimeters = 'cm';
  sdxUnitsMillimeters = 'mm';
  sdxUnitsPoints = 'pt';
  sdxUnitsPicas = 'pi';

  sdxUnitsDefaultName = 'Default';
  sdxUnitsInchesName = 'Inches';
  sdxUnitsCentimetersName = 'Centimeters';
  sdxUnitsMillimetersName = 'Millimeters';
  sdxUnitsPointsName = 'Points';
  sdxUnitsPicasName = 'Picas';

  sdxPrintPreview = 'Print Preview';
  sdxReportDesignerCaption = 'Format Report';
  sdxCompositionDesignerCaption = 'Composition Editor';
  sdxCompositionStartEachItemFromNewPage = '&Start each item from new page';

  sdxComponentNotSupportedByLink = 'Component "%s" not supported by TdxComponentPrinter';
  sdxComponentNotSupported = 'Component "%s" not supported by TdxComponentPrinter';
  sdxPrintDeviceNotReady = 'Printer has not been installed or is not ready';
  sdxUnableToGenerateReport = 'Unable to generate report';
  sdxPreviewNotRegistered = 'There is no registered preview form';
  sdxComponentNotAssigned = '%s' + #13#10 + 'Not assigned "Component" property';
  sdxPrintDeviceIsBusy = 'Printer is busy';
  sdxPrintDeviceError = 'Printer has encountered error !';
  sdxMissingComponent = 'Missing "Component" property';
  sdxDataProviderDontPresent = 'There are no Links with Assigned Component in Composition';
  sdxBuildingReport = 'Building report: Completed %d%%';                            // obsolete
  sdxPrintingReport = 'Printing report: Completed %d page(s). Press Esc to cancel'; // obsolete
  sdxDefinePrintStylesMenuItem = 'Define Print &Styles...';
  sdxAbortPrinting = 'Abort printing ?';
  sdxStandardStyle = 'Standard Style';

  sdxFontStyleBold = 'Bold';
  sdxFontStyleItalic = 'Italic';
  sdxFontStyleUnderline = 'Underline';
  sdxFontStyleStrikeOut = 'StrikeOut';
  sdxPt = 'pt.';

  sdxPageWidth = 'Page Width';
  sdxWholePage = 'Whole Page';
  sdxTwoPages = 'Two Pages';
  sdxFourPages = 'Four Pages';
  sdxWidenToSourceWidth = 'Widen to Source Width';

  sdxMenuBar = 'MenuBar';
  sdxStandardBar = 'Standard';
  sdxHeaderFooterBar = 'Header and Footer';
  sdxShortcutMenusBar = 'Shortcut Menus';
  sdxAutoTextBar = 'AutoText';

  sdxMenuFile = '&File';
  sdxMenuFileDesign = '&Design...';
  sdxMenuFilePrint = '&Print...';
  sdxMenuFilePrintDialog = 'Print Dialog';
  sdxMenuFilePageSetup = 'Page Set&up...';
  sdxMenuPrintStyles = 'Print Styles';
  sdxMenuFileExit = '&Close';
  sdxMenuExportToPDF = 'Export To PDF';
  sdxMenuFileOptions = 'Options';

  sdxMenuEdit = '&Edit';
  sdxMenuEditCut = 'Cu&t';
  sdxMenuEditCopy = '&Copy';
  sdxMenuEditPaste = '&Paste';
  sdxMenuEditDelete = '&Delete';
  sdxMenuEditFind = '&Find...';
  sdxMenuEditFindNext = 'Find Ne&xt';
  sdxMenuEditReplace = '&Replace...';

  sdxMenuLoad = '&Load...';
  sdxMenuPreview = 'Pre&view...';

  sdxMenuInsert = '&Insert';
  sdxMenuInsertAutoText = '&AutoText';
  sdxMenuInsertEditAutoTextEntries = 'AutoTe&xt...';
  sdxMenuInsertAutoTextEntries = 'List of AutoText Entries';
  sdxMenuInsertAutoTextEntriesSubItem = 'In&sert AutoText';
  sdxMenuInsertPageNumber = '&Page Number';
  sdxMenuInsertTotalPages = '&Number of Pages';
  sdxMenuInsertPageOfPages = 'Pa&ge Number of Pages';
  sdxMenuInsertDateTime = 'Date and Time';
  sdxMenuInsertDate = '&Date';
  sdxMenuInsertTime = '&Time';
  sdxMenuInsertUserName = '&User Name';
  sdxMenuInsertMachineName = '&Machine Name';

  sdxMenuView = '&View';
  sdxMenuViewMargins = '&Margins';
  sdxMenuViewFlatToolBarButtons = '&Flat ToolBar Buttons';
  sdxMenuViewLargeToolBarButtons = '&Large ToolBar Buttons';
  sdxMenuViewMarginsStatusBar = 'M&argins Bar';
  sdxMenuViewPagesStatusBar = '&Status Bar';
  sdxMenuViewToolBars = '&Toolbars';
  sdxMenuViewPagesHeaders = 'Page &Headers';
  sdxMenuViewPagesFooters = 'Page Foote&rs';
  sdxMenuViewSwitchToLeftPart = 'Switch to Left Part';
  sdxMenuViewSwitchToRightPart = 'Switch to Right Part';
  sdxMenuViewSwitchToCenterPart = 'Switch to Center Part';
  sdxMenuViewHFSwitchHeaderFooter = '&Show Header/Footer';
  sdxMenuViewSwitchToFooter = 'Footer';
  sdxMenuViewSwitchToHeader = 'Header';
  sdxMenuViewHFClose = '&Close';

  sdxMenuZoom = '&Zoom';
  sdxMenuZoomPercent100 = 'Percent &100';
  sdxMenuZoomPageWidth = 'Page &Width';
  sdxMenuZoomWholePage = 'W&hole Page';
  sdxMenuZoomTwoPages = '&Two Pages';
  sdxMenuZoomFourPages = '&Four Pages';
  sdxMenuZoomMultiplyPages = '&Multiple Pages';
  sdxMenuZoomWidenToSourceWidth = 'Widen To S&ource Width';
  sdxMenuZoomSetup = '&Setup...';

  sdxMenuPages = '&Pages';

  sdxMenuGotoPage = '&Go';
  sdxMenuGotoPageFirst = '&First Page';
  sdxMenuGotoPagePrev = '&Previous Page';
  sdxMenuGotoPageNext = '&Next Page';
  sdxMenuGotoPageLast = '&Last Page';
  sdxMenuActivePage = '&Active Page:';

  sdxMenuFormat = 'F&ormat';
  sdxMenuFormatHeaderAndFooter = '&Header and Footer';
  sdxMenuFormatAutoTextEntries = '&Auto Text Entries...';
  sdxMenuFormatDateTime = 'Date And &Time...';
  sdxMenuFormatPageNumbering = 'Page &Numbering...';
  sdxMenuFormatPageBackground = 'Bac&kground...';
  sdxMenuFormatShrinkToPage = '&Fit to Page Width';
  sdxMenuFormatHFBackground = 'Header/Footer Background...';
  sdxMenuFormatHFClear = 'Clear Text';

  sdxMenuTools = '&Tools';
  sdxMenuToolsCustomize = '&Customize...';
  sdxMenuToolsOptions = '&Options...';

  sdxMenuHelp = '&Help';
  sdxMenuHelpTopics = 'Help &Topics...';
  sdxMenuHelpAbout = '&About...';

  sdxMenuShortcutPreview = 'Preview';
  sdxMenuShortcutAutoText = 'AutoText';

  sdxMenuBuiltInMenus = 'Built-in Menus';
  sdxMenuShortCutMenus = 'Shortcut Menus';
  sdxMenuNewMenu = 'New Menu';

  { Hints }

  sdxHintFileDesign = 'Design Report';
  sdxHintFilePrint = 'Print';
  sdxHintFilePrintDialog = 'Print Dialog';
  sdxHintFilePageSetup = 'Page Setup';
  sdxHintFileExit = 'Close Preview';
  sdxHintExportToPDF = 'Export To PDF';

  sdxHintEditFind = 'Find';
  sdxHintEditFindNext = 'Find Next';
  sdxHintEditReplace = 'Replace';

  sdxHintInsertEditAutoTextEntries = 'Edit AutoText Entries';
  sdxHintInsertPageNumber = 'Insert Page Number';
  sdxHintInsertTotalPages = 'Insert Number of Pages';
  sdxHintInsertPageOfPages = 'Insert Page Number of Pages';
  sdxHintInsertDateTime = 'Insert Date and Time';
  sdxHintInsertDate = 'Insert Date';
  sdxHintInsertTime = 'Insert Time';
  sdxHintInsertUserName = 'Insert User Name';
  sdxHintInsertMachineName = 'Insert Machine Name';

  sdxHintViewMargins = 'View Margins';
  sdxHintViewLargeButtons = 'View Large Buttons';
  sdxHintViewMarginsStatusBar = 'View Margins Status Bar';
  sdxHintViewPagesStatusBar = 'View Page Status Bar';
  sdxHintViewPagesHeaders = 'View Page Header';
  sdxHintViewPagesFooters = 'View Page Footer';
  sdxHintViewSwitchToLeftPart = 'Switch to Left Header/Footer Part';
  sdxHintViewSwitchToRightPart = 'Switch to Right Header/Footer Part';
  sdxHintViewSwitchToCenterPart = 'Switch to Center Header/Footer Part';
  sdxHintViewHFSwitchHeaderFooter = 'Switch Between Header and Footer';
  sdxHintViewSwitchToFooter = 'Switch to Footer';
  sdxHintViewSwitchToHeader = 'Switch to Header';
  sdxHintViewHFClose = 'Close';

  sdxHintViewZoom = 'Zoom';
  sdxHintZoomPercent100 = 'Zoom 100%';
  sdxHintZoomPageWidth = 'Zoom Page Width';
  sdxHintZoomWholePage = 'Whole Page';
  sdxHintZoomTwoPages = 'Two Pages';
  sdxHintZoomFourPages = 'Four Pages';
  sdxHintZoomMultiplyPages = 'Multiple Pages';
  sdxHintZoomWidenToSourceWidth = 'Widen To Source Width';
  sdxHintZoomSetup = 'Setup Zoom Factor';

  sdxHintFormatDateTime = 'Format Date and Time';
  sdxHintFormatPageNumbering = 'Format Page Number';
  sdxHintFormatPageBackground = 'Background';
  sdxHintFormatShrinkToPage = 'Fit to Page Width';
  sdxHintFormatHFBackground = 'Header/Footer Background';
  sdxHintFormatHFClear = 'Clear Header/Footer Text';

  sdxHintGotoPageFirst = 'First Page';
  sdxHintGotoPagePrev = 'Previous Page';
  sdxHintGotoPageNext = 'Next Page';
  sdxHintGotoPageLast = 'Last Page';
  sdxHintActivePage = 'Active Page';

  sdxHintToolsCustomize = 'Customize Toolbars';
  sdxHintToolsOptions = 'Options';

  sdxHintHelpTopics = 'Help Topics';
  sdxHintHelpAbout = 'About';

  sdxPopupMenuLargeButtons = '&Large Buttons';
  sdxPopupMenuFlatButtons = '&Flat Buttons';

  sdxPaperSize = 'Paper Size:';
  sdxStatus = 'Status:';
  sdxStatusReady = 'Ready';
  sdxStatusPrinting = 'Printing. Completed %d page(s)';
  sdxStatusGenerateReport = 'Generating Report. Completed %d%%';

  sdxHintDoubleClickForChangePaperSize = 'Double Click for Change Paper Size';
  sdxHintDoubleClickForChangeMargins = 'Double Click for Change Margins';

  { Date&Time Formats Dialog }

  sdxDTFormatsCaption = 'Date and Time';
  sdxDTFormatsAvailableDateFormats = '&Available Date Formats:';
  sdxDTFormatsAvailableTimeFormats = 'Available &Time Formats:';
  sdxDTFormatsAutoUpdate = '&Update Automatically';
  sdxDTFormatsChangeDefaultFormat =
    'Do you want to change the default date and time formats to match "%s"  - "%s" ?';

  { PageNumber Formats Dialog }

  sdxPNFormatsCaption = 'Page Number Format';
  sdxPageNumbering = 'Page Numbering';
  sdxPNFormatsNumberFormat = 'Number &Format:';
  sdxPNFormatsContinueFromPrevious = '&Continue from Previous Section';
  sdxPNFormatsStartAt = 'Start &At:';
  sdxPNFormatsChangeDefaultFormat =
    'Do you want to change the default Page numbering format to match "%s" ?';

  { Zoom Dialog }

  sdxZoomDlgCaption = 'Zoom';
  sdxZoomDlgZoomTo = ' Zoom To ';
  sdxZoomDlgPageWidth = 'Page &Width';
  sdxZoomDlgWholePage = 'W&hole Page';
  sdxZoomDlgTwoPages = '&Two Pages';
  sdxZoomDlgFourPages = '&Four Pages';
  sdxZoomDlgManyPages = '&Many Pages:';
  sdxZoomDlgPercent = 'P&ercent:';
  sdxZoomDlgPreview = ' Preview ';
  sdxZoomDlgFontPreview = ' 12pt Times New Roman ';
  sdxZoomDlgFontPreviewString = 'AaBbCcDdEeXxYyZz';

  { Select page X x Y }

  sdxPages = 'Pages';
  sdxCancel = 'Cancel';

  { Preferences dialog }

  sdxPreferenceDlgCaption = 'Options';
  sdxPreferenceDlgTab1 = '&General';
  sdxPreferenceDlgTab2 = '';
  sdxPreferenceDlgTab3 = '';
  sdxPreferenceDlgTab4 = '';
  sdxPreferenceDlgTab5 = '';
  sdxPreferenceDlgTab6 = '';
  sdxPreferenceDlgTab7 = '';
  sdxPreferenceDlgTab8 = '';
  sdxPreferenceDlgTab9 = '';
  sdxPreferenceDlgTab10 = '';
  sdxPreferenceDlgShow = ' &Show ';
  sdxPreferenceDlgMargins = '&Margins ';
  sdxPreferenceDlgMarginsHints = 'Margins &Hints';
  sdxPreferenceDlgMargingWhileDragging = 'Margins Hints While &Dragging';
  sdxPreferenceDlgLargeBtns = '&Large Toolbar Buttons';
  sdxPreferenceDlgFlatBtns = '&Flat Toolbar Buttons';
  sdxPreferenceDlgMarginsColor = 'Margins &Color:';
  sdxPreferenceDlgMeasurementUnits = 'Measurement &Units:';
  sdxPreferenceDlgSaveForRunTimeToo = 'Save for &RunTime too';
  sdxPreferenceDlgZoomScroll = '&Zoom on roll with IntelliMouse';
  sdxPreferenceDlgZoomStep = 'Zoom Ste&p:';

  { Page Setup }

  sdxCloneStyleCaptionPrefix = 'Copy (%d) of ';
  sdxInvalideStyleCaption = 'The style name "%s" already exists. Please supply another name.';
  sdxHintMoreHFFunctions = 'More Functions';

  sdxPageSetupCaption = 'Page Setup';
  sdxStyleName = 'Style &Name:';

  sdxAutomatic = 'Automatic';
  sdxFitToPageHorizontally = 'Width:';
  sdxFitToPageVertically = 'Height:';
  sdxHeaderFooter = '&Header / Footer';
  sdxMargins = '&Margins';
  sdxPage = '&Page';
  sdxPagesSuffix = ' page(s)';
  sdxScaleTo = 'Scale:';
  sdxScaling = '&Scaling';

  sdxPaper = ' Paper ';
  sdxPaperType = 'T&ype';
  sdxPaperDimension = 'Dimension';
  sdxPaperWidth = '&Width:';
  sdxPaperHeight = 'H&eight:';
  sdxPaperSource = 'Paper so&urce:';

  sdxOrientation = ' Orientation ';
  sdxPortrait = 'P&ortrait';
  sdxLandscape = '&Landscape';
  sdxAutoOrientation = 'Au&to';
  sdxPrintOrder = ' Print Order ';
  sdxDownThenOver = '&Down, then over';
  sdxOverThenDown = 'O&ver, then down';
  sdxShading = ' Shading ';
  sdxPrintUsingGrayShading = 'Print using &gray shading';

  sdxCenterOnPage = 'Center on page';
  sdxHorizontally = 'Hori&zontally';
  sdxVertically = '&Vertically';

  sdxHeader = 'Header ';
  sdxBtnHeaderFont = '&Font...';
  sdxBtnHeaderBackground = '&Background';
  sdxFooter = 'Footer ';
  sdxBtnFooterFont = 'Fo&nt...';
  sdxBtnFooterBackground = 'Back&ground';

  sdxTop = '&Top:';
  sdxLeft = '&Left:';
  sdxRight = 'Ri&ght:';
  sdxBottom = '&Bottom:';
  sdxHeader2 = 'H&eader:';
  sdxFooter2 = 'Foote&r:';

  sdxAlignment = 'Alignment';
  sdxVertAlignment = ' Vertical Alignment ';
  sdxReverseOnEvenPages = '&Reverse on even pages';

  sdxAdjustTo = '&Adjust To:';
  sdxFitTo = '&Fit To:';
  sdxPercentOfNormalSize = '% normal size';
  sdxPagesWideBy = 'page(s) &wide by';
  sdxTall = '&tall';

  sdxOf = 'Of';
  sdxLastPrinted = 'Last Printed ';
  sdxFileName = 'Filename ';
  sdxFileNameAndPath = 'Filename and path ';
  sdxPrintedBy = 'Printed By ';
  sdxPrintedOn = 'Printed On ';
  sdxCreatedBy = 'Created By ';
  sdxCreatedOn = 'Created On ';
  sdxConfidential = 'Confidential';

  { HF function }

  sdxHFFunctionNameDate = 'Date';
  sdxHFFunctionNameDateTime = 'Date and Time';
  sdxHFFunctionNameImage = 'Image';
  sdxHFFunctionNameMachineName = 'Machine Name';
  sdxHFFunctionNamePageNumber = 'Page Number';
  sdxHFFunctionNamePageOfPages = 'Page # of Pages #';
  sdxHFFunctionNameTime = 'Time';
  sdxHFFunctionNameTotalPages = 'Total Pages';
  sdxHFFunctionNameUnknown = 'Unknown';
  sdxHFFunctionNameUserName = 'User Name';

  sdxHFFunctionHintDate = 'Date Printed';
  sdxHFFunctionHintDateTime = 'Date and Time Printed';
  sdxHFFunctionHintImage = 'Image';
  sdxHFFunctionHintMachineName = 'Machine Name';
  sdxHFFunctionHintPageNumber = 'Page Number';
  sdxHFFunctionHintPageOfPages = 'Page # of Pages #';
  sdxHFFunctionHintTime = 'Time Printed';
  sdxHFFunctionHintTotalPages = 'Total Pages';
  sdxHFFunctionHintUserName = 'User Name';

  sdxHFFunctionTemplateDate = 'Date Printed';
  sdxHFFunctionTemplateDateTime = 'Date & Time Printed';
  sdxHFFunctionTemplateImage = 'Image';
  sdxHFFunctionTemplateMachineName = 'Machine Name';
  sdxHFFunctionTemplatePageNumber = 'Page #';
  sdxHFFunctionTemplatePageOfPages = 'Page # of Pages #';
  sdxHFFunctionTemplateTime = 'Time Printed';
  sdxHFFunctionTemplateTotalPages = 'Total Pages';
  sdxHFFunctionTemplateUserName = 'User Name';

  { PDF Export Dialog }

  sdxPDFDialogAuthor = 'Author';
  sdxPDFDialogCaption = 'PDF Export Options';
  sdxPDFDialogCompressed = 'Compressed';
  sdxPDFDialogCreator = 'Creator';
  sdxPDFDialogDocumentInfoTabSheet = '&Document Info';
  sdxPDFDialogEmbedFonts = 'Embed Fonts';
  sdxPDFDialogExportSettings = 'Export Settings';
  sdxPDFDialogExportTabSheet = '&Export';
  sdxPDFDialogKeywords = 'Keywords';
  sdxPDFDialogMaxCompression = 'Max Compression';
  sdxPDFDialogMaxQuality = 'Max Quality';
  sdxPDFDialogOpenAfterExport = 'Open After Export';
  sdxPDFDialogPageRageTabSheet = '&Pages';
  sdxPDFDialogSecurityAllowChanging = 'Allow Changing the Document';
  sdxPDFDialogSecurityAllowComments = 'Allow Comments';
  sdxPDFDialogSecurityAllowCopy = 'Allow Content Copying and Extraction';
  sdxPDFDialogSecurityAllowDocumentAssemble = 'Allow Document Assembly';
  sdxPDFDialogSecurityAllowPrint = 'Allow Print';
  sdxPDFDialogSecurityAllowPrintHiResolution = 'Allow Printing with High Resolution';
  sdxPDFDialogSecurityEnabled = 'Enabled';
  sdxPDFDialogSecurityMethod = 'Method:';
  sdxPDFDialogSecurityOwnerPassword = 'Owner Password:';
  sdxPDFDialogSecuritySettings = 'Security Settings';
  sdxPDFDialogSecurityUserPassword = 'User Password:';
  sdxPDFDialogSubject = 'Subject';
  sdxPDFDialogTitle = 'Title';
  sdxPDFDialogUseCIDFonts = 'Use CID Fonts';
  sdxPDFDialogUseJPEGCompression = 'Use JPEG Compression for Images';
  sdxPDFDialogTabDocInfo = '&Document Information';
  sdxPDFDialogTabExport = '&Export';
  sdxPDFDialogTabPages = '&Pages';
  sdxPDFDialogTabSecurity = '&Security';

  { Designer strings }

  { Months }

  sdxJanuary = 'January';
  sdxFebruary = 'February';
  sdxMarch = 'March';
  sdxApril = 'April';
  sdxMay = 'May';
  sdxJune = 'June';
  sdxJuly = 'July';
  sdxAugust = 'August';
  sdxSeptember = 'September';
  sdxOctober = 'October';
  sdxNovember = 'November';
  sdxDecember = 'December';

  sdxEast = 'East';
  sdxWest = 'West';
  sdxSouth = 'South';
  sdxNorth = 'North';

  sdxTotal = 'Total';

  { dxFlowChart }

  sdxPlan = 'Plan';
  sdxSwimmingPool = 'Swimming-pool';
  sdxAdministration = 'Administration';
  sdxPark = 'Park';
  sdxCarParking = 'Car-Parking';

  { dxOrgChart }

  sdxCorporateHeadquarters = 'Corporate' + #13#10 + 'Headquarters';
  sdxSalesAndMarketing = 'Sales and' + #13#10 + 'Marketing';
  sdxEngineering = 'Engineering';
  sdxFieldOfficeCanada = 'Field Office:' + #13#10 + 'Canada';

  { dxMasterView }

  sdxOrderNoCaption = 'OrderNo';
  sdxNameCaption = 'Name';
  sdxCountCaption = 'Count';
  sdxCompanyCaption = 'Company';
  sdxAddressCaption = 'Address';
  sdxPriceCaption = 'Price';
  sdxCashCaption = 'Cash';

  sdxName1 = 'Jennie Valentine';
  sdxName2 = 'Sam Hill';
  sdxCompany1 = 'Jennie Inc.';
  sdxCompany2 = 'Daimler-Chrysler AG';
  sdxAddress1 = '123 Home Lane';
  sdxAddress2 = '9333 Holmes Dr.';

  { dxTreeList }

  sdxCountIs = 'Count is: %d';
  sdxRegular = 'Regular';
  sdxIrregular = 'Irregular';

  sdxTLBand = 'Item Data';
  sdxTLColumnName = 'Name';
  sdxTLColumnAxisymmetric = 'Axisymmetric';
  sdxTLColumnItemShape = 'Shape';

  sdxItemShapeAsText = '(Graphic)';

  sdxItem1Name = 'Cylinder';
  sdxItem2Name = 'Cone';
  sdxItem3Name = 'Pyramid';
  sdxItem4Name = 'Box';
  sdxItem5Name = 'Free Surface';

  sdxItem1Description = '';
  sdxItem2Description = 'Axisymmetric geometry figure';
  sdxItem3Description = 'Axisymmetric geometry figure';
  sdxItem4Description = 'Acute-angled geometry figure';
  sdxItem5Description = '';
  sdxItem6Description = '';
  sdxItem7Description = 'Simple extrusion surface';

  { PS 2.3 }

  { Patterns common }

  sdxPatternIsNotRegistered = 'Pattern "%s" is not registered';

  { Excel edge patterns }

  sdxSolidEdgePattern = 'Solid';
  sdxThinSolidEdgePattern = 'Medium Solid';
  sdxMediumSolidEdgePattern = 'Medium Solid';
  sdxThickSolidEdgePattern = 'Thick Solid';
  sdxDottedEdgePattern = 'Dotted';
  sdxDashedEdgePattern = 'Dashed';
  sdxDashDotDotEdgePattern = 'Dash Dot Dot';
  sdxDashDotEdgePattern = 'Dash Dot';
  sdxSlantedDashDotEdgePattern = 'Slanted Dash Dot';
  sdxMediumDashDotDotEdgePattern = 'Medium Dash Dot Dot';
  sdxHairEdgePattern = 'Hair';
  sdxMediumDashDotEdgePattern = 'Medium Dash Dot';
  sdxMediumDashedEdgePattern = 'Medium Dashed';
  sdxDoubleLineEdgePattern = 'Double Line';

  { Excel fill patterns names}

  sdxSolidFillPattern = 'Solid';
  sdxGray75FillPattern = '75% Gray';
  sdxGray50FillPattern = '50% Gray';
  sdxGray25FillPattern = '25% Gray';
  sdxGray125FillPattern = '12.5% Gray';
  sdxGray625FillPattern = '6.25% Gray';
  sdxHorizontalStripeFillPattern = 'Horizontal Stripe';
  sdxVerticalStripeFillPattern = 'Vertical Stripe';
  sdxReverseDiagonalStripeFillPattern = 'Reverse Diagonal Stripe';
  sdxDiagonalStripeFillPattern = 'Diagonal Stripe';
  sdxDiagonalCrossHatchFillPattern = 'Diagonal Cross Hatch';
  sdxThickCrossHatchFillPattern = 'Thick Cross Hatch';
  sdxThinHorizontalStripeFillPattern = 'Thin Horizontal Stripe';
  sdxThinVerticalStripeFillPattern = 'Thin Vertical Stripe';
  sdxThinReverseDiagonalStripeFillPattern = 'Thin Reverse Diagonal Stripe';
  sdxThinDiagonalStripeFillPattern = 'Thin Diagonal Stripe';
  sdxThinHorizontalCrossHatchFillPattern = 'Thin Horizontal Cross Hatch';
  sdxThinDiagonalCrossHatchFillPattern = 'Thin Diagonal Cross Hatch';

  { cxSpreadSheet }

  sdxShowRowAndColumnHeadings = '&Row and Column Headings';
  sdxShowGridLines = 'GridLines';
  sdxSuppressSourceFormats = '&Suppress Source Formats';
  sdxRepeatHeaderRowAtTop = 'Repeat Header Row at Top';
  sdxDataToPrintDoesNotExist =
    'Cannot activate ReportLink because PrintingSystem did not find anything to print.';

  { Designer strings }

  { Short names of month }

  sdxJanuaryShort = 'Jan';
  sdxFebruaryShort = 'Feb';
  sdxMarchShort = 'March';
  sdxAprilShort = 'April';
  sdxMayShort = 'May';
  sdxJuneShort = 'June';
  sdxJulyShort = 'July';
  sdxAugustShort = 'Aug';
  sdxSeptemberShort = 'Sept';
  sdxOctoberShort = 'Oct';
  sdxNovemberShort = 'Nov';
  sdxDecemberShort = 'Dec';

  { TreeView }

  sdxTechnicalDepartment = 'Technical Department';
  sdxSoftwareDepartment = 'Software Department';
  sdxSystemProgrammers = 'Core Developers';
  sdxEndUserProgrammers = 'GUI Developers';
  sdxBetaTesters = 'Beta Testers';
  sdxHumanResourceDepartment = 'Human Resource Department';

  { misc. }

  sdxTreeLines = '&TreeLines';
  sdxTreeLinesColor = 'T&ree Line Color:';
  sdxExpandButtons = 'E&xpand Buttons';
  sdxCheckMarks = 'Check Marks';
  sdxTreeEffects = 'Tree Effects';
  sdxAppearance = 'Appearance';

  { Designer previews }

  { Localize if you want (they are used inside FormatReport dialog -> ReportPreview) }

  sdxCarLevelCaption = 'Cars';

  sdxManufacturerBandCaption = 'Manufacturer Data';
  sdxModelBandCaption = 'Car Data';

  sdxManufacturerNameColumnCaption = 'Name';
  sdxManufacturerLogoColumnCaption = 'Logo';
  sdxManufacturerCountryColumnCaption = 'Country';
  sdxCarModelColumnCaption = 'Model';
  sdxCarIsSUVColumnCaption = 'SUV';
  sdxCarPhotoColumnCaption = 'Photo';
  sdxCarSpeedCountColumnCaption = 'Speed Count';

  sdxCarManufacturerName1 = 'BMW';
  sdxCarManufacturerName2 = 'Ford';
  sdxCarManufacturerName3 = 'Audi';
  sdxCarManufacturerName4 = 'Land Rover';

  sdxCarManufacturerCountry1 = 'Germany';
  sdxCarManufacturerCountry2 = 'United States';
  sdxCarManufacturerCountry3 = 'Germany';
  sdxCarManufacturerCountry4 = 'United Kingdom';

  sdxCarModel1 = 'X5 4.8is';
  sdxCarModel2 = 'Excursion';
  sdxCarModel3 = 'S8 Quattro';
  sdxCarModel4 = 'G4 Challenge';

  sdxTrue = 'True';
  sdxFalse = 'False';

  { PS 2.4 }

  { dxPrnDev.pas }

  sdxAuto = 'Auto';
  sdxCustom = 'Custom';
  sdxEnv = 'Env';

  { Grid 4 }

  sdxLookAndFeelFlat = 'Flat';
  sdxLookAndFeelStandard = 'Standard';
  sdxLookAndFeelUltraFlat = 'UltraFlat';

  sdxViewTab = 'View';
  sdxBehaviorsTab = 'Behaviors';
  sdxPreviewTab = 'Preview';
  sdxCardsTab = 'Cards';

  sdxFormatting = 'Formatting';
  sdxLookAndFeel = 'Look and Feel';
  sdxLevelCaption = '&Caption';
  sdxFilterBar = '&Filter Bar';
  sdxRefinements = 'Refinements';
  sdxProcessSelection = 'Process &Selection';
  sdxProcessExactSelection = 'Process E&xact Selection';
  sdxExpanding = 'Expanding';
  sdxGroups = '&Groups';
  sdxDetails = '&Details';
  sdxStartFromActiveDetails = 'Start from Active Details';
  sdxOnlyActiveDetails = 'Only Active Details';
  sdxVisible = '&Visible';
  sdxPreviewAutoHeight = 'A&uto Height';
  sdxPreviewMaxLineCount = '&Max Line Count: ';
  sdxSizes = 'Sizes';
  sdxKeepSameWidth = '&Keep Same Width';
  sdxKeepSameHeight = 'Keep Same &Height';
  sdxFraming = 'Framing';
  sdxSpacing = 'Spacing';
  sdxShadow = 'Shadow';
  sdxDepth = '&Depth:';
  sdxPosition = '&Position';
  sdxPositioning = 'Positioning';
  sdxHorizontal = 'H&orizontal:';
  sdxVertical = 'V&ertical:';

  sdxSummaryFormat = 'Count = 0';

  sdxCannotUseOnEveryPageMode =
    'Cannot Use OnEveryPage Mode'+ #13#10 +
    #13#10 +
    'You should or(and)' + #13#10 +
    '  - Collapse all Master Records' + #13#10 +
    '  - Toggle "Unwrap" Option off on "Behaviors" Tab';

  sdxIncorrectBandHeadersState =
    'Cannot Use BandHeaders OnEveryPage Mode' + #13#10 +
    #13#10 +
    'You should either:' + #13#10 +
    '  - Set Caption OnEveryPage Option On' + #13#10 +
    '  - Set Caption Visible Option Off';
  sdxIncorrectHeadersState =
    'Cannot Use Headers OnEveryPage Mode' + #13#10 +
    #13#10 +
    'You should either:' + #13#10 +
    '  - Set Caption and Band OnEveryPage Option On' + #13#10 +
    '  - Set Caption and Band Visible Option Off';
  sdxIncorrectFootersState =
   'Cannot Use Footers OnEveryPage Mode' + #13#10 +
    #13#10 +
    'You should either:' + #13#10 +
    '  - Set FilterBar OnEveryPage Option On' + #13#10 +
    '  - Set FilterBar Visible Option Off';

  sdxCharts = 'Charts';

  { PS 3 }

  sdxTPicture = 'TPicture';
  sdxCopy = '&Copy';
  sdxSave = '&Save...';
  sdxBaseStyle = 'Base Style';

  sdxComponentAlreadyExists = 'Component named "%s" already exists';
  sdxInvalidComponentName = '"%s" is not a valid component name';

  { shapes }

  sdxRectangle = 'Rectangle';
  sdxSquare = 'Square';
  sdxEllipse = 'Ellipse';
  sdxCircle = 'Circle';
  sdxRoundRect = 'RoundRect';
  sdxRoundSquare = 'RoundSquare';

  { standard pattern names}

  sdxHorizontalFillPattern = 'Horizontal';
  sdxVerticalFillPattern = 'Vertical';
  sdxFDiagonalFillPattern = 'FDiagonal';
  sdxBDiagonalFillPattern = 'BDiagonal';
  sdxCrossFillPattern = 'Cross';
  sdxDiagCrossFillPattern = 'DiagCros';

  { explorer }

  sdxCyclicIDReferences = 'Cyclic ID references %s and %s';
  sdxLoadReportDataToFileTitle = 'Load Report';
  sdxSaveReportDataToFileTitle = 'Save Report As';
  sdxInvalidExternalStorage = 'Invalid External Storage';
  sdxLinkIsNotIncludedInUsesClause =
    'ReportFile contains ReportLink "%0:s"' + #13#10 +
    'Unit with declaration of "%0:s" must be included in uses clause';
  sdxInvalidStorageVersion = 'Invalid Storage Verison: %d';
  sdxPSReportFiles = 'Report Files';
  sdxReportFileLoadError =
    'Cannot load Report File "%s".' + #13#10 +
    'File is corrupted or is locked by another User or Application.' + #13#10 +
    #13#10 +
    'Original Report will be restored.';

  sdxNone = '(None)';
  sdxReportDocumentIsCorrupted = '(File is not a ReportDocument or Corrupted)';

  sdxCloseExplorerHint = 'Close Explorer';
  sdxExplorerCaption = 'Explorer';
  sdxExplorerRootFolderCaption = 'Root';
  sdxNewExplorerFolderItem = 'New Folder';
  sdxCopyOfItem = 'Copy of ';
  sdxReportExplorer = 'Report Explorer';

  sdxDataLoadErrorText = 'Cannot load Report Data';
  sdxDBBasedExplorerItemDataLoadError =
    'Cannot load Report Data.' + #13#10 +
    'Data are corrupted or are locked';
  sdxFileBasedExplorerItemDataLoadError =
    'Cannot load Report Data.' + #13#10 +
    'File is corrupted or is locked by another User or Application';
  sdxDeleteNonEmptyFolderMessageText = 'Folder "%s" is not Empty. Delete anyway?';
  sdxDeleteFolderMessageText = 'Delete Folder "%s" ?';
  sdxDeleteItemMessageText = 'Delete Item "%s" ?';
  sdxCannotRenameFolderText = 'Cannot rename folder "%s". A folder with name "%s" already exists. Specify a different name.';
  sdxCannotRenameItemText = 'Cannot rename item "%s". An item with name "%s" already exists. Specify a different name.';
  sdxOverwriteFolderMessageText =
    'This folder "%s" already contains folder named "%s".' + #13#10 +
    #13#10 +
    'If the items in existing folder have the same name as items in the' + #13#10 +
    'folder you are moving or copying, they will be replaced. Do you still?' + #13#10 +
    'want to move or copy the folder?';
  sdxOverwriteItemMessageText =
    'This Folder "%s" already contains item named "%s".' + #13#10 +
    #13#10 +
    'Would you like to overwrite existing item?';
  sdxSelectNewRoot = 'Select new Root Directory where the Reports will be stored';
  sdxInvalidFolderName = 'Invalid Folder Name "%s"';
  sdxInvalidReportName = 'Invalid Report Name "%s"';

  sdxExplorerBar = 'Explorer';

  sdxMenuFileSave = '&Save';
  sdxMenuFileSaveAs = 'Save &As...';
  sdxMenuFileLoad = '&Load';
  sdxMenuFileClose = 'U&nload';
  sdxHintFileSave = 'Save Report';
  sdxHintFileSaveAs = 'Save Report As';
  sdxHintFileLoad = 'Load Report';
  sdxHintFileClose = 'Unload Report';

  sdxMenuExplorer = 'E&xplorer';
  sdxMenuExplorerCreateFolder = 'Create &Folder';
  sdxMenuExplorerDelete = '&Delete...';
  sdxMenuExplorerRename = 'Rena&me';
  sdxMenuExplorerProperties = '&Properties...';
  sdxMenuExplorerRefresh = 'Refresh';
  sdxMenuExplorerChangeRootPath = 'Change Root...';
  sdxMenuExplorerSetAsRoot = 'Set As Root';
  sdxMenuExplorerGoToUpOneLevel = 'Up One Level';

  sdxHintExplorerCreateFolder = 'Create New Folder';
  sdxHintExplorerDelete = 'Delete';
  sdxHintExplorerRename = 'Rename';
  sdxHintExplorerProperties = 'Properties';
  sdxHintExplorerRefresh = 'Refresh';
  sdxHintExplorerChangeRootPath = 'Change Root';
  sdxHintExplorerSetAsRoot = 'Set Current Folder as Root';
  sdxHintExplorerGoToUpOneLevel = 'Up One Level';

  sdxMenuViewExplorer = 'E&xplorer';
  sdxHintViewExplorer = 'Show Explorer';

  sdxSummary = 'Summary';
  sdxCreator = 'Creato&r:';
  sdxCreationDate = 'Create&d:';

  sdxMenuViewThumbnails = 'Th&umbnails';
  sdxMenuThumbnailsLarge = '&Large Thumbnails';
  sdxMenuThumbnailsSmall = '&Small Thumbnails';

  sdxHintViewThumbnails = 'Show Thumbnails';
  sdxHintThumbnailsLarge = 'Switch to large thumbnails';
  sdxHintThumbnailsSmall = 'Switch to small thumbnails';

  sdxMenuFormatTitle = 'T&itle...';
  sdxHintFormatTitle = 'Format Report Title';
  sdxMenuFormatFootnotes = 'Foot&notes...';
  sdxHintFormatFootnotes = 'Format Report Footnotes...';

  sdxHalf = 'Half';
  sdxPredefinedFunctions = ' Predefined Functions '; // dxPgsDlg.pas
  sdxZoomParameters = ' Zoom &Parameters ';          // dxPSPrvwOpt.pas

  sdxWrapData = '&Wrap Data';

  sdxMenuShortcutExplorer = 'Explorer';
  sdxExplorerToolBar = 'Explorer';

  sdxMenuShortcutThumbnails = 'Thumbnails';

  { Ribbon Print Preview Window }

  sdxRibbonPrintPreviewClosePrintPreview = 'Close Print Preview';
  sdxRibbonPrintPreviewGroupFormat = 'Format';
  sdxRibbonPrintPreviewGroupInsertName = 'Name';
  sdxRibbonPrintPreviewGroupInsertPageNumber = 'Page Number';
  sdxRibbonPrintPreviewGroupNavigation = 'Navigation';
  sdxRibbonPrintPreviewGroupOutput = 'Output';
  sdxRibbonPrintPreviewGroupParts = 'Parts';
  sdxRibbonPrintPreviewGroupReport = 'Report';
  sdxRibbonPrintPreviewGroupScaleToFit = 'Scale to Fit';
  sdxRibbonPrintPreviewGroupZoom = 'Zoom';
  sdxRibbonPrintPreviewPagesSubItem = 'Pages';

  { TreeView New}

  sdxButtons = 'Buttons';

  { ListView }

  sdxBtnHeadersFont = '&Headers Font...';
  sdxHeadersTransparent = 'Transparent &Headers';
  sdxHintListViewDesignerMessage = ' Most Options Are Being Taken Into Account Only In Detailed View';
  sdxColumnHeaders = '&Column Headers';

  { Group LookAndFeel Names }

  sdxReportGroupNullLookAndFeel = 'Null';
  sdxReportGroupStandardLookAndFeel = 'Standard';
  sdxReportGroupOfficeLookAndFeel = 'Office';
  sdxReportGroupWebLookAndFeel = 'Web';

  { Layout }

  sdxLayoutGroupDefaultCaption = 'Layout Group';
  sdxLayoutItemDefaultCaption = 'Layout Item';
  sdxTabs = 'Tabs';
  sdxUnwrapTabs = '&Unwrap Tabs';
  sdxActiveTabToTop = 'Display Active Tab on Top';
  sdxBehaviorsGroups = 'Groups';
  sdxSkipEmptyGroups = 'Skip Empty Groups';
  sdxExpandedGroups = 'Expand Groups';

  { Designer Previews}

  { Localize if you want (they are used inside FormatReport dialog -> ReportPreview) }

  sdxCarManufacturerName5 = 'DaimlerChrysler AG';
  sdxCarManufacturerCountry5 = 'Germany';
  sdxCarModel5 = 'Maybach 62';

  sdxLuxurySedans = 'Luxury Sedans';
  sdxCarManufacturer = 'Manufacturer';
  sdxCarModel = 'Model';
  sdxCarEngine = 'Engine';
  sdxCarTransmission = 'Transmission';
  sdxCarTires = 'Tires';
  sdx760V12Manufacturer = 'BMW';
  sdx760V12Model = '760Li V12';
  sdx760V12Engine = '6.0L DOHC V12 438 HP 48V DI Valvetronic 12-cylinder engine with 6.0-liter displacement, dual overhead cam valvetrain';
  sdx760V12Transmission = 'Elec 6-Speed Automatic w/Steptronic';
  sdx760V12Tires = 'P245/45R19 Fr - P275/40R19 Rr Performance. Low Profile tires with 245mm width, 19.0" rim';

  { Styles }

  sdxBandBackgroundStyle = 'BandBackground';
  sdxBandHeaderStyle = 'BandHeader';
  sdxCaptionStyle = 'Caption';
  sdxCardCaptionRowStyle = 'Card Caption Row';
  sdxCardRowCaptionStyle = 'Card Row Caption';
  sdxCategoryStyle = 'Category';
  sdxContentStyle = 'Content';
  sdxContentEvenStyle = 'Content Even Rows';
  sdxContentOddStyle = 'Content Odd Rows';
  sdxFilterBarStyle = 'Filter Bar';
  sdxFooterStyle = 'Footer';
  sdxFooterRowStyle = 'Footer Row';
  sdxGroupStyle = 'Group';
  sdxHeaderStyle = 'Header';
  sdxIndentStyle = 'Indent';
  sdxPreviewStyle = 'Preview';
  sdxSelectionStyle = 'Selection';

  sdxStyles = 'Styles';
  sdxStyleSheets = 'Style Sheets';
  sdxBtnTexture = '&Texture...';
  sdxBtnTextureClear = 'Cl&ear';
  sdxBtnColor = 'Co&lor...';
  sdxBtnSaveAs = 'Save &As...';
  sdxBtnRename = 'Rena&me...';

  sdxLoadBitmapDlgTitle = 'Load Texture';

  sdxDeleteStyleSheet = 'Delete StyleSheet Named "%s"?';
  sdxUnnamedStyleSheet = 'Unnamed';
  sdxCreateNewStyleQueryNamePrompt = 'Enter New StyleSheet Name: ';
  sdxStyleSheetNameAlreadyExists = 'StyleSheet named "%s" Already Exists';

  sdxCannotLoadImage = 'Cannot Load Image "%s"';
  sdxUseNativeStyles = '&Use Native Styles';
  sdxSuppressBackgroundBitmaps = '&Suppress Background Textures';
  sdxConsumeSelectionStyle = 'Consume Selection Style';

  { Grid4 new }

  sdxSize = 'Size';
  sdxLevels = 'Levels';
  sdxUnwrap = '&Unwrap';
  sdxUnwrapTopLevel = 'Un&wrap Top Level';
  sdxRiseActiveToTop = 'Rise Active Level onto Top';
  sdxCannotUseOnEveryPageModeInAggregatedState =
    'Cannot Use OnEveryPage Mode'+ #13#10 +
    'While Performing in Aggregated State';

  sdxPagination = 'Pagination';
  sdxByBands = 'By Bands';
  sdxByColumns = 'By Columns';
  sdxByRows = 'By &Rows';
  sdxByTopLevelGroups = 'By TopLevel Groups';
  sdxOneGroupPerPage = 'One Group Per Page';

  sdxSkipEmptyViews = 'Skip Empty Views';

  {* For those who will translate *}
  {* You should also check "sdxCannotUseOnEveryPageMode" resource string - see above *}
  {* It was changed to "- Toggle "Unwrap" Option off on "Behaviors" Tab"*}

  { TL 4 }
  sdxBorders = 'Borders';
  sdxExplicitlyExpandNodes = 'Explicitly Expand Nodes';
  sdxNodes = '&Nodes';
  sdxSeparators = 'Separators';
  sdxThickness = 'Thickness:';
  sdxTLIncorrectHeadersState =
    'Cannot Use Headers OnEveryPage Mode' + #13#10 +
    #13#10 +
    'You should either:' + #13#10 +
    '  - Set Band OnEveryPage Option On' + #13#10 +
    '  - Set Band Visible Option Off';

  { cxVerticalGrid }

  sdxRows = '&Rows';

  sdxMultipleRecords = '&Multiple Records';
  sdxBestFit = '&Best Fit';
  sdxKeepSameRecordWidths = '&Keep Same Record Widths';
  sdxWrapRecords = '&Wrap Records';

  sdxByWrapping = 'By &Wrapping';
  sdxOneWrappingPerPage = '&One Wrapping Per Page';

  {new in 3.01}
  sdxCurrentRecord = 'Current Record';
  sdxLoadedRecords = 'Loaded Records';
  sdxAllRecords = 'All Records';

  { Container Designer }

  sdxPaginateByControlDetails = 'Control Details';
  sdxPaginateByControls = 'Controls';
  sdxPaginateByGroups = 'Groups';
  sdxPaginateByItems = 'Items';

  sdxControlsPlace = 'Controls Place';
  sdxExpandHeight = 'Expand Height';
  sdxExpandWidth = 'Expand Width';
  sdxShrinkHeight = 'Shrink Height';
  sdxShrinkWidth = 'Shrink Width';

  sdxCheckAll = 'Check &All';
  sdxCheckAllChildren = 'Check All &Children';
  sdxControlsTab = 'Controls';
  sdxExpandAll = 'E&xpand All';
  sdxHiddenControlsTab = 'Available Controls';
  sdxReportLinksTab = 'Aggregated Designers';
  sdxAvailableLinks = '&Available Links:';
  sdxAggregatedLinks = 'A&ggregated Links:';
  sdxTransparents = 'Transparents';
  sdxUncheckAllChildren = 'Uncheck &All Children';

  sdxRoot = '&Root';
  sdxRootBorders = 'Root &Borders';
  sdxControls = '&Controls';
  sdxContainers = 'C&ontainers';

  sdxHideCustomContainers = '&Hide Custom Containers';

  { General }

  // FileSize abbreviation

  sdxBytes = 'Bytes';
  sdxKiloBytes = 'KB';
  sdxMegaBytes = 'MB';
  sdxGigaBytes = 'GB';

  // Misc.

  sdxThereIsNoPictureToDisplay = 'There is no picture to display';
  sdxInvalidRootDirectory = 'Directory "%s" does not exists. Continue selection ?';
  sdxPressEscToCancel = 'Press Esc to cancel';
  sdxMenuFileRebuild = '&Rebuild';
  sdxBuildingReportStatusText = 'Building report - Press Esc to cancel';
  sdxPrintingReportStatusText = 'Printing report - Press Esc to cancel';

  sdxBuiltIn = '[BuiltIn]';
  sdxUserDefined = '[User Defined]';
  sdxNewStyleRepositoryWasCreated = 'New StyleRepository "%s" was created and assigned';

  { new in PS 3.1}
  sdxLineSpacing = '&Line spacing:';
  sdxTextAlignJustified = 'Justified';
  sdxSampleText = 'Sample Text Sample Text';

  sdxCardsRows = '&Cards';
  sdxTransparentRichEdits = 'Transparent &RichEdit Content';

  sdxIncorrectFilterBarState =
    'Cannot Use FilterBar OnEveryPage Mode' + #13#10 +
    #13#10 +
    'You should either:' + #13#10 +
    '  - Set Caption OnEveryPage Option On' + #13#10 +
    '  - Set Caption Visible Option Off';
  sdxIncorrectBandHeadersState2 =
    'Cannot Use BandHeaders OnEveryPage Mode' + #13#10 +
    #13#10 +
    'You should either:' + #13#10 +
    '  - Set Caption and FilterBar OnEveryPage Option On' + #13#10 +
    '  - Set Caption and FilterBar Visible Option Off';
  sdxIncorrectHeadersState2 =
    'Cannot Use Headers OnEveryPage Mode' + #13#10 +
    #13#10 +
    'You should either:' + #13#10 +
    '  - Set Caption, FilterBar and Band OnEveryPage Option On' + #13#10 +
    '  - Set Caption, FilterBar and Band Visible Option Off';

 { new in PS 3.2}
  sdxAvailableReportLinks = 'Available ReportLinks';
  sdxBtnRemoveInconsistents = 'Remove Unneeded';
  sdxColumnHeadersOnEveryPage = 'Column &Headers';
  sdxRowHeadersOnEveryPage = 'Row Headers';

 { Scheduler }

  sdxNotes = 'Notes';
  sdxTaskPad = 'TaskPad';
  sdxPrimaryTimeZone = 'Primary';
  sdxSecondaryTimeZone = 'Secondary';

  sdxDay = 'Day';
  sdxWeek = 'Week';
  sdxMonth = 'Month';

  sdxSchedulerSchedulerHeader = 'Scheduler Header';
  sdxSchedulerContent = 'Content';
  sdxSchedulerDateNavigatorContent = 'DateNavigator Content';
  sdxSchedulerDateNavigatorHeader = 'DateNavigator Header';
  sdxSchedulerDayHeader = 'Day Header';
  sdxSchedulerEvent = 'Event';
  sdxSchedulerResourceHeader = 'Resource Header';
  sdxSchedulerNotesAreaBlank = 'Notes Area (Blank)';
  sdxSchedulerNotesAreaLined = 'Notes Area (Lined)';
  sdxSchedulerTaskPad = 'TaskPad';
  sdxSchedulerTimeRuler = 'Time Ruler';

  sdxPrintStyleNameDaily = 'Daily';
  sdxPrintStyleNameWeekly = 'Weekly';
  sdxPrintStyleNameMonthly = 'Monthly';
  sdxPrintStyleNameDetails = 'Details';
  sdxPrintStyleNameMemo = 'Memo';
  sdxPrintStyleNameTrifold = 'Trifold';

  sdxPrintStyleCaptionAgenda = 'Agenda Style';
  sdxPrintStyleCaptionDaily = 'Daily Style';
  sdxPrintStyleCaptionWeekly = 'Weekly Style';
  sdxPrintStyleCaptionMonthly = 'Monthly Style';
  sdxPrintStyleCaptionDetails = 'Calendar Details Style';
  sdxPrintStyleCaptionMemo = 'Memo Style';
  sdxPrintStyleCaptionTimeLine = 'TimeLine Style';
  sdxPrintStyleCaptionTrifold = 'Tri-fold Style';
  sdxPrintStyleCaptionYearly = 'Yearly Style';
  sdxPrintStyleShowEventImages = 'Show Event Images';
  sdxPrintStyleShowResourceImages = 'Show Resource Images';

  sdxTabPrintStyles = 'Print Styles';

  sdxPrintStyleDontPrintWeekEnds = '&Don''t Print Weekends';
  sdxPrintStyleWorkTimeOnly = '&Work Time Only';

  sdxPrintStyleInclude = 'Include:';
  sdxPrintStyleIncludeTaskPad = 'Task&Pad';
  sdxPrintStyleIncludeNotesAreaBlank = 'Notes Area (&Blank)';
  sdxPrintStyleIncludeNotesAreaLined = 'Notes Area (&Lined)';
  sdxPrintStyleLayout = '&Layout:';
  sdxPrintStylePrintFrom = 'Print &From:';
  sdxPrintStylePrintTo = 'Print &To:';

  sdxPrintStyleDailyLayout1PPD = '1 page/day';
  sdxPrintStyleDailyLayout2PPD = '2 pages/day';

  sdxPrintStyleWeeklyArrange = '&Arrange:';
  sdxPrintStyleWeeklyArrangeT2B = 'Top to bottom';
  sdxPrintStyleWeeklyArrangeL2R = 'Left to right';
  sdxPrintStyleWeeklyLayout1PPW = '1 page/week';
  sdxPrintStyleWeeklyLayout2PPW = '2 pages/week';
  sdxPrintStyleWeeklyDaysLayout   = '&Days layout:';
  sdxPrintStyleWeeklyDaysLayoutTC = 'Two columns';
  sdxPrintStyleWeeklyDaysLayoutOC = 'One column';

  sdxPrintStyleMemoStartEachItemOnNewPage = 'Start Each Item On New Page';
  sdxPrintStyleMemoPrintOnlySelectedEvents = 'Print Only Selected Events';

  sdxPrintStyleMonthlyLayout1PPM = '1 page/month';
  sdxPrintStyleMonthlyLayout2PPM = '2 pages/month';
  sdxPrintStyleMonthlyPrintExactly1MPP = 'Print &Exactly One Month Per Page';

  sdxPrintStyleTrifoldSectionModeDailyCalendar = 'Daily Calendar';
  sdxPrintStyleTrifoldSectionModeWeeklyCalendar = 'Weekly Calendar';
  sdxPrintStyleTrifoldSectionModeMonthlyCalendar = 'Monthly Calendar';
  sdxPrintStyleTrifoldSectionModeTaskPad = 'TaskPad';
  sdxPrintStyleTrifoldSectionModeNotesBlank = 'Notes (Blank)';
  sdxPrintStyleTrifoldSectionModeNotesLined = 'Notes (Lined)';
  sdxPrintStyleTrifoldSectionLeft = '&Left Section:';
  sdxPrintStyleTrifoldSectionMiddle = '&Middle Section:';
  sdxPrintStyleTrifoldSectionRight = '&Right Section:';

  sdxPrintStyleMonthPerPage = '&Months/Page:';
  sdxPrintStyleYearly1MPP  = '1 month/page';
  sdxPrintStyleYearly2MPP  = '2 months/page';
  sdxPrintStyleYearly3MPP  = '3 months/page';
  sdxPrintStyleYearly4MPP  = '4 months/page';
  sdxPrintStyleYearly6MPP  = '6 months/page';
  sdxPrintStyleYearly12MPP = '12 months/page';

  sdxPrintStylePrimaryPageScalesOnly = 'Primary Page Scales Only';
  sdxPrintStylePrimaryPageHeadersOnly = 'Primary Page Headers Only';

  sdxPrintStyleDetailsStartNewPageEach = 'Start a New Page Each:';

  sdxSuppressContentColoration = 'Suppress &Content Coloration';
  sdxOneResourcePerPage = 'One &Resource Per Page';

  sdxPrintRanges = 'Print Ranges';
  sdxPrintRangeStart = '&Start:';
  sdxPrintRangeEnd = '&End:';
  sdxHideDetailsOfPrivateAppointments = '&Hide Details of Private Appointments';
  sdxResourceCountPerPage = '&Resources/Page:';

  sdxSubjectLabelCaption = 'Subject:';
  sdxLocationLabelCaption = 'Location:';
  sdxStartLabelCaption = 'Start:';
  sdxFinishLabelCaption = 'End:';
  sdxShowTimeAsLabelCaption = 'Show Time As:';
  sdxRecurrenceLabelCaption = 'Recurrence:';
  sdxRecurrencePatternLabelCaption = 'Recurrence Pattern:';

  //messages
  sdxSeeAboveMessage = 'Please See Above';
  sdxAllDayMessage = 'All Day';
  sdxContinuedMessage = 'Continued';
  sdxShowTimeAsFreeMessage = 'Free';
  sdxShowTimeAsTentativeMessage = 'Tentative';
  sdxShowTimeAsOutOfOfficeMessage = 'Out of Office';

  sdxRecurrenceNoneMessage = '(none)';
  scxRecurrenceDailyMessage = 'Daily';
  scxRecurrenceWeeklyMessage = 'Weekly';
  scxRecurrenceMonthlyMessage = 'Monthly';
  scxRecurrenceYearlyMessage = 'Yearly';

  //error messages
  sdxInconsistentTrifoldStyle = 'The Tri-fold style requires at least one calendar section. ' +
    'Select Daily, Weekly, or Monthly Calendar for one of section under Options.';
  sdxBadTimePrintRange = 'The hours to print are not valid. The start time must precede the end time.';
  sdxBadDatePrintRange = 'The date in the End box cannot be prior to the date in the Start box.';
  sdxCannotPrintNoSelectedItems = 'Cannot print unless an item is selected. Select an item, and then try to print again.';
  sdxCannotPrintNoItemsAvailable = 'No items available within the specified print range.';

  { PivotGrid }

  sdxColumnFields = '&Column Fields';
  sdxDataFields   = '&Data Fields';
  sdxFiterFields  = '&Filter Fields';
  sdxPrefilter = '&Prefilter';
  sdxRowFields    = '&Row Fields';

  sdxAutoColumnsExpand = 'A&uto Columns Expand';
  sdxAutoRowsExpand = 'Auto &Rows Expand';

  // styles
  sdxPivotGridColumnHeader = 'Column Header';
  sdxPivotGridContent = 'Content';
  sdxPivotGridFieldHeader  = 'Field Header';
  sdxPivotGridHeaderBackground = 'Header Background';
  sdxPivotGridRowHeader = 'Row Header';
  sdxPivotGridPrefilter = 'Prefilter';

  // PivotPreview fields
  sdxUnitPrice = 'Unit Price';
  sdxCarName = 'Car Name';
  sdxQuantity = 'Quantity';
  sdxPaymentAmount = 'Payment Amount';
  sdxPurchaseQuarter = 'Purchase Quarter';
  sdxPurchaseMonth = 'Purchase Month';
  sdxPaymentType   = 'Payment Type';
  sdxCompanyName   = 'Company Name';

implementation

uses
  dxCore;

procedure AddResourceStringsPart1(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxBtnOK', @sdxBtnOK);
  AProduct.Add('sdxBtnOKAccelerated', @sdxBtnOKAccelerated);
  AProduct.Add('sdxBtnCancel', @sdxBtnCancel);
  AProduct.Add('sdxBtnClose', @sdxBtnClose);
  AProduct.Add('sdxBtnApply', @sdxBtnApply);
  AProduct.Add('sdxBtnHelp', @sdxBtnHelp);
  AProduct.Add('sdxBtnFix', @sdxBtnFix);
  AProduct.Add('sdxBtnNew', @sdxBtnNew);
  AProduct.Add('sdxBtnIgnore', @sdxBtnIgnore);
  AProduct.Add('sdxBtnYes', @sdxBtnYes);
  AProduct.Add('sdxBtnNo', @sdxBtnNo);
  AProduct.Add('sdxBtnEdit', @sdxBtnEdit);
  AProduct.Add('sdxBtnReset', @sdxBtnReset);
  AProduct.Add('sdxBtnAdd', @sdxBtnAdd);
  AProduct.Add('sdxBtnAddComposition', @sdxBtnAddComposition);
  AProduct.Add('sdxBtnDefault', @sdxBtnDefault);
  AProduct.Add('sdxBtnDelete', @sdxBtnDelete);
  AProduct.Add('sdxBtnDescription', @sdxBtnDescription);
  AProduct.Add('sdxBtnCopy', @sdxBtnCopy);
  AProduct.Add('sdxBtnYesToAll', @sdxBtnYesToAll);
  AProduct.Add('sdxBtnRestoreDefaults', @sdxBtnRestoreDefaults);
  AProduct.Add('sdxBtnRestoreOriginal', @sdxBtnRestoreOriginal);
  AProduct.Add('sdxBtnTitleProperties', @sdxBtnTitleProperties);
  AProduct.Add('sdxBtnFootnoteProperties', @sdxBtnFootnoteProperties);
  AProduct.Add('sdxBtnProperties', @sdxBtnProperties);
  AProduct.Add('sdxBtnNetwork', @sdxBtnNetwork);
  AProduct.Add('sdxBtnBrowse', @sdxBtnBrowse);
  AProduct.Add('sdxBtnPageSetup', @sdxBtnPageSetup);
  AProduct.Add('sdxBtnPrintPreview', @sdxBtnPrintPreview);
  AProduct.Add('sdxBtnPreview', @sdxBtnPreview);
  AProduct.Add('sdxBtnPrint', @sdxBtnPrint);
  AProduct.Add('sdxBtnOptions', @sdxBtnOptions);
  AProduct.Add('sdxBtnStyleOptions', @sdxBtnStyleOptions);
  AProduct.Add('sdxBtnDefinePrintStyles', @sdxBtnDefinePrintStyles);
  AProduct.Add('sdxBtnPrintStyles', @sdxBtnPrintStyles);
  AProduct.Add('sdxBtnBackground', @sdxBtnBackground);
  AProduct.Add('sdxBtnShowToolBar', @sdxBtnShowToolBar);
  AProduct.Add('sdxBtnDesign', @sdxBtnDesign);
  AProduct.Add('sdxBtnMoveUp', @sdxBtnMoveUp);
  AProduct.Add('sdxBtnMoveDown', @sdxBtnMoveDown);
  AProduct.Add('sdxBtnMoreColors', @sdxBtnMoreColors);
  AProduct.Add('sdxBtnFillEffects', @sdxBtnFillEffects);
  AProduct.Add('sdxBtnNoFill', @sdxBtnNoFill);
  AProduct.Add('sdxBtnAutomatic', @sdxBtnAutomatic);
  AProduct.Add('sdxBtnNone', @sdxBtnNone);
  AProduct.Add('sdxBtnOtherTexture', @sdxBtnOtherTexture);
  AProduct.Add('sdxBtnInvertColors', @sdxBtnInvertColors);
  AProduct.Add('sdxBtnSelectPicture', @sdxBtnSelectPicture);
  AProduct.Add('sdxEditReports', @sdxEditReports);
  AProduct.Add('sdxComposition', @sdxComposition);
  AProduct.Add('sdxReportTitleDlgCaption', @sdxReportTitleDlgCaption);
  AProduct.Add('sdxReportFootnotesDlgCaption', @sdxReportFootnotesDlgCaption);
  AProduct.Add('sdxMode', @sdxMode);
  AProduct.Add('sdxText', @sdxText);
  AProduct.Add('sdxProperties', @sdxProperties);
  AProduct.Add('sdxAdjustOnScale', @sdxAdjustOnScale);
  AProduct.Add('sdxTitleModeNone', @sdxTitleModeNone);
  AProduct.Add('sdxTitleModeOnEveryTopPage', @sdxTitleModeOnEveryTopPage);
  AProduct.Add('sdxTitleModeOnFirstPage', @sdxTitleModeOnFirstPage);
  AProduct.Add('sdxFootnotesModeNone', @sdxFootnotesModeNone);
  AProduct.Add('sdxFootnotesModeOnEveryBottomPage', @sdxFootnotesModeOnEveryBottomPage);
  AProduct.Add('sdxFootnotesModeOnLastPage', @sdxFootnotesModeOnLastPage);
  AProduct.Add('sdxEditDescription', @sdxEditDescription);
  AProduct.Add('sdxRename', @sdxRename);
  AProduct.Add('sdxSelectAll', @sdxSelectAll);
  AProduct.Add('sdxAddReport', @sdxAddReport);
  AProduct.Add('sdxAddAndDesignReport', @sdxAddAndDesignReport);
  AProduct.Add('sdxNewCompositionCaption', @sdxNewCompositionCaption);
  AProduct.Add('sdxName', @sdxName);
  AProduct.Add('sdxCaption', @sdxCaption);
  AProduct.Add('sdxAvailableSources', @sdxAvailableSources);
  AProduct.Add('sdxOnlyComponentsInActiveForm', @sdxOnlyComponentsInActiveForm);
  AProduct.Add('sdxOnlyComponentsWithoutLinks', @sdxOnlyComponentsWithoutLinks);
  AProduct.Add('sdxItemName', @sdxItemName);
  AProduct.Add('sdxItemDescription', @sdxItemDescription);
  AProduct.Add('sdxConfirmDeleteItem', @sdxConfirmDeleteItem);
  AProduct.Add('sdxAddItemsToComposition', @sdxAddItemsToComposition);
  AProduct.Add('sdxHideAlreadyIncludedItems', @sdxHideAlreadyIncludedItems);
  AProduct.Add('sdxAvailableItems', @sdxAvailableItems);
  AProduct.Add('sdxItems', @sdxItems);
  AProduct.Add('sdxEnable', @sdxEnable);
  AProduct.Add('sdxGroupImages', @sdxGroupImages);
  AProduct.Add('sdxGroupView', @sdxGroupView);
  AProduct.Add('sdxOptions', @sdxOptions);
  AProduct.Add('sdxShow', @sdxShow);
  AProduct.Add('sdxPaintItemsGraphics', @sdxPaintItemsGraphics);
  AProduct.Add('sdxDescription', @sdxDescription);
  AProduct.Add('sdxNewReport', @sdxNewReport);
  AProduct.Add('sdxOnlySelected', @sdxOnlySelected);
  AProduct.Add('sdxExtendedSelect', @sdxExtendedSelect);
  AProduct.Add('sdxIncludeFixed', @sdxIncludeFixed);
  AProduct.Add('sdxFonts', @sdxFonts);
  AProduct.Add('sdxBtnFont', @sdxBtnFont);
  AProduct.Add('sdxBtnEvenFont', @sdxBtnEvenFont);
  AProduct.Add('sdxBtnOddFont', @sdxBtnOddFont);
  AProduct.Add('sdxBtnFixedFont', @sdxBtnFixedFont);
  AProduct.Add('sdxBtnGroupFont', @sdxBtnGroupFont);
  AProduct.Add('sdxBtnChangeFont', @sdxBtnChangeFont);
  AProduct.Add('sdxFont', @sdxFont);
  AProduct.Add('sdxOddFont', @sdxOddFont);
  AProduct.Add('sdxEvenFont', @sdxEvenFont);
  AProduct.Add('sdxPreviewFont', @sdxPreviewFont);
  AProduct.Add('sdxCaptionNodeFont', @sdxCaptionNodeFont);
  AProduct.Add('sdxGroupNodeFont', @sdxGroupNodeFont);
  AProduct.Add('sdxGroupFooterFont', @sdxGroupFooterFont);
  AProduct.Add('sdxHeaderFont', @sdxHeaderFont);
  AProduct.Add('sdxFooterFont', @sdxFooterFont);
  AProduct.Add('sdxBandFont', @sdxBandFont);
  AProduct.Add('sdxTransparent', @sdxTransparent);
  AProduct.Add('sdxFixedTransparent', @sdxFixedTransparent);
  AProduct.Add('sdxCaptionTransparent', @sdxCaptionTransparent);
  AProduct.Add('sdxGroupTransparent', @sdxGroupTransparent);
  AProduct.Add('sdxGraphicAsTextValue', @sdxGraphicAsTextValue);
  AProduct.Add('sdxColors', @sdxColors);
  AProduct.Add('sdxColor', @sdxColor);
  AProduct.Add('sdxOddColor', @sdxOddColor);
  AProduct.Add('sdxEvenColor', @sdxEvenColor);
  AProduct.Add('sdxPreviewColor', @sdxPreviewColor);
  AProduct.Add('sdxBandColor', @sdxBandColor);
  AProduct.Add('sdxLevelCaptionColor', @sdxLevelCaptionColor);
  AProduct.Add('sdxHeaderColor', @sdxHeaderColor);
  AProduct.Add('sdxGroupNodeColor', @sdxGroupNodeColor);
  AProduct.Add('sdxGroupFooterColor', @sdxGroupFooterColor);
  AProduct.Add('sdxFooterColor', @sdxFooterColor);
  AProduct.Add('sdxFixedColor', @sdxFixedColor);
  AProduct.Add('sdxGroupColor', @sdxGroupColor);
  AProduct.Add('sdxCaptionColor', @sdxCaptionColor);
  AProduct.Add('sdxGridLinesColor', @sdxGridLinesColor);
  AProduct.Add('sdxBands', @sdxBands);
  AProduct.Add('sdxLevelCaptions', @sdxLevelCaptions);
  AProduct.Add('sdxHeaders', @sdxHeaders);
  AProduct.Add('sdxFooters', @sdxFooters);
  AProduct.Add('sdxGroupFooters', @sdxGroupFooters);
  AProduct.Add('sdxPreview', @sdxPreview);
  AProduct.Add('sdxPreviewLineCount', @sdxPreviewLineCount);
  AProduct.Add('sdxAutoCalcPreviewLineCount', @sdxAutoCalcPreviewLineCount);
  AProduct.Add('sdxGrid', @sdxGrid);
  AProduct.Add('sdxNodesGrid', @sdxNodesGrid);
  AProduct.Add('sdxGroupFooterGrid', @sdxGroupFooterGrid);
  AProduct.Add('sdxStateImages', @sdxStateImages);
  AProduct.Add('sdxImages', @sdxImages);
  AProduct.Add('sdxTextAlign', @sdxTextAlign);
  AProduct.Add('sdxTextAlignHorz', @sdxTextAlignHorz);
  AProduct.Add('sdxTextAlignVert', @sdxTextAlignVert);
  AProduct.Add('sdxTextAlignLeft', @sdxTextAlignLeft);
  AProduct.Add('sdxTextAlignCenter', @sdxTextAlignCenter);
  AProduct.Add('sdxTextAlignRight', @sdxTextAlignRight);
  AProduct.Add('sdxTextAlignTop', @sdxTextAlignTop);
  AProduct.Add('sdxTextAlignVCenter', @sdxTextAlignVCenter);
  AProduct.Add('sdxTextAlignBottom', @sdxTextAlignBottom);
  AProduct.Add('sdxBorderLines', @sdxBorderLines);
  AProduct.Add('sdxHorzLines', @sdxHorzLines);
  AProduct.Add('sdxVertLines', @sdxVertLines);
  AProduct.Add('sdxFixedHorzLines', @sdxFixedHorzLines);
  AProduct.Add('sdxFixedVertLines', @sdxFixedVertLines);
  AProduct.Add('sdxFlatCheckMarks', @sdxFlatCheckMarks);
  AProduct.Add('sdxCheckMarksAsText', @sdxCheckMarksAsText);
  AProduct.Add('sdxRowAutoHeight', @sdxRowAutoHeight);
  AProduct.Add('sdxEndEllipsis', @sdxEndEllipsis);
  AProduct.Add('sdxDrawBorder', @sdxDrawBorder);
  AProduct.Add('sdxFullExpand', @sdxFullExpand);
  AProduct.Add('sdxBorderColor', @sdxBorderColor);
  AProduct.Add('sdxAutoNodesExpand', @sdxAutoNodesExpand);
  AProduct.Add('sdxExpandLevel', @sdxExpandLevel);
  AProduct.Add('sdxFixedRowOnEveryPage', @sdxFixedRowOnEveryPage);
  AProduct.Add('sdxDrawMode', @sdxDrawMode);
  AProduct.Add('sdxDrawModeStrict', @sdxDrawModeStrict);
  AProduct.Add('sdxDrawModeOddEven', @sdxDrawModeOddEven);
  AProduct.Add('sdxDrawModeChess', @sdxDrawModeChess);
  AProduct.Add('sdxDrawModeBorrow', @sdxDrawModeBorrow);
  AProduct.Add('sdx3DEffects', @sdx3DEffects);
  AProduct.Add('sdxUse3DEffects', @sdxUse3DEffects);
  AProduct.Add('sdxSoft3D', @sdxSoft3D);
  AProduct.Add('sdxBehaviors', @sdxBehaviors);
  AProduct.Add('sdxMiscellaneous', @sdxMiscellaneous);
  AProduct.Add('sdxOnEveryPage', @sdxOnEveryPage);
  AProduct.Add('sdxNodeExpanding', @sdxNodeExpanding);
  AProduct.Add('sdxSelection', @sdxSelection);
  AProduct.Add('sdxNodeAutoHeight', @sdxNodeAutoHeight);
  AProduct.Add('sdxTransparentGraphics', @sdxTransparentGraphics);
  AProduct.Add('sdxAutoWidth', @sdxAutoWidth);
  AProduct.Add('sdxDisplayGraphicsAsText', @sdxDisplayGraphicsAsText);
  AProduct.Add('sdxDisplayTrackBarsAsText', @sdxDisplayTrackBarsAsText);
  AProduct.Add('sdxTransparentColumnGraphics', @sdxTransparentColumnGraphics);
  AProduct.Add('sdxBandsOnEveryPage', @sdxBandsOnEveryPage);
  AProduct.Add('sdxHeadersOnEveryPage', @sdxHeadersOnEveryPage);
  AProduct.Add('sdxFootersOnEveryPage', @sdxFootersOnEveryPage);
  AProduct.Add('sdxGraphics', @sdxGraphics);
  AProduct.Add('sdxOutOfResources', @sdxOutOfResources);
  AProduct.Add('sdxFileAlreadyExists', @sdxFileAlreadyExists);
  AProduct.Add('sdxConfirmOverWrite', @sdxConfirmOverWrite);
  AProduct.Add('sdxInvalidFileName', @sdxInvalidFileName);
  AProduct.Add('sdxRequiredFileName', @sdxRequiredFileName);
  AProduct.Add('sdxOutsideMarginsMessage', @sdxOutsideMarginsMessage);
  AProduct.Add('sdxOutsideMarginsMessage2', @sdxOutsideMarginsMessage2);
  AProduct.Add('sdxInvalidMarginsMessage', @sdxInvalidMarginsMessage);
  AProduct.Add('sdxInvalidMargins', @sdxInvalidMargins);
  AProduct.Add('sdxOutsideMargins', @sdxOutsideMargins);
  AProduct.Add('sdxThereAreNowItemsForShow', @sdxThereAreNowItemsForShow);
  AProduct.Add('sdxReportCellClassNotRegistered', @sdxReportCellClassNotRegistered);
  AProduct.Add('sdxPageBackground', @sdxPageBackground);
  AProduct.Add('sdxPenColor', @sdxPenColor);
  AProduct.Add('sdxFontColor', @sdxFontColor);
  AProduct.Add('sdxBrushColor', @sdxBrushColor);
  AProduct.Add('sdxHighLight', @sdxHighLight);
  AProduct.Add('sdxTexture', @sdxTexture);
  AProduct.Add('sdxPattern', @sdxPattern);
  AProduct.Add('sdxPicture', @sdxPicture);
  AProduct.Add('sdxForeground', @sdxForeground);
  AProduct.Add('sdxBackground', @sdxBackground);
  AProduct.Add('sdxSample', @sdxSample);
  AProduct.Add('sdxFEFCaption', @sdxFEFCaption);
  AProduct.Add('sdxPaintMode', @sdxPaintMode);
  AProduct.Add('sdxPaintModeCenter', @sdxPaintModeCenter);
  AProduct.Add('sdxPaintModeStretch', @sdxPaintModeStretch);
  AProduct.Add('sdxPaintModeTile', @sdxPaintModeTile);
  AProduct.Add('sdxPaintModeProportional', @sdxPaintModeProportional);
  AProduct.Add('sdxPatternGray5', @sdxPatternGray5);
  AProduct.Add('sdxPatternGray10', @sdxPatternGray10);
  AProduct.Add('sdxPatternGray20', @sdxPatternGray20);
  AProduct.Add('sdxPatternGray25', @sdxPatternGray25);
  AProduct.Add('sdxPatternGray30', @sdxPatternGray30);
  AProduct.Add('sdxPatternGray40', @sdxPatternGray40);
  AProduct.Add('sdxPatternGray50', @sdxPatternGray50);
  AProduct.Add('sdxPatternGray60', @sdxPatternGray60);
  AProduct.Add('sdxPatternGray70', @sdxPatternGray70);
  AProduct.Add('sdxPatternGray75', @sdxPatternGray75);
  AProduct.Add('sdxPatternGray80', @sdxPatternGray80);
  AProduct.Add('sdxPatternGray90', @sdxPatternGray90);
  AProduct.Add('sdxPatternLightDownwardDiagonal', @sdxPatternLightDownwardDiagonal);
  AProduct.Add('sdxPatternLightUpwardDiagonal', @sdxPatternLightUpwardDiagonal);
  AProduct.Add('sdxPatternDarkDownwardDiagonal', @sdxPatternDarkDownwardDiagonal);
  AProduct.Add('sdxPatternDarkUpwardDiagonal', @sdxPatternDarkUpwardDiagonal);
  AProduct.Add('sdxPatternWideDownwardDiagonal', @sdxPatternWideDownwardDiagonal);
  AProduct.Add('sdxPatternWideUpwardDiagonal', @sdxPatternWideUpwardDiagonal);
  AProduct.Add('sdxPatternLightVertical', @sdxPatternLightVertical);
  AProduct.Add('sdxPatternLightHorizontal', @sdxPatternLightHorizontal);
  AProduct.Add('sdxPatternNarrowVertical', @sdxPatternNarrowVertical);
  AProduct.Add('sdxPatternNarrowHorizontal', @sdxPatternNarrowHorizontal);
  AProduct.Add('sdxPatternDarkVertical', @sdxPatternDarkVertical);
  AProduct.Add('sdxPatternDarkHorizontal', @sdxPatternDarkHorizontal);
  AProduct.Add('sdxPatternDashedDownward', @sdxPatternDashedDownward);
  AProduct.Add('sdxPatternDashedUpward', @sdxPatternDashedUpward);
  AProduct.Add('sdxPatternDashedVertical', @sdxPatternDashedVertical);
  AProduct.Add('sdxPatternDashedHorizontal', @sdxPatternDashedHorizontal);
  AProduct.Add('sdxPatternSmallConfetti', @sdxPatternSmallConfetti);
  AProduct.Add('sdxPatternLargeConfetti', @sdxPatternLargeConfetti);
  AProduct.Add('sdxPatternZigZag', @sdxPatternZigZag);
  AProduct.Add('sdxPatternWave', @sdxPatternWave);
  AProduct.Add('sdxPatternDiagonalBrick', @sdxPatternDiagonalBrick);
  AProduct.Add('sdxPatternHorizantalBrick', @sdxPatternHorizantalBrick);
  AProduct.Add('sdxPatternWeave', @sdxPatternWeave);
  AProduct.Add('sdxPatternPlaid', @sdxPatternPlaid);
  AProduct.Add('sdxPatternDivot', @sdxPatternDivot);
  AProduct.Add('sdxPatternDottedGrid', @sdxPatternDottedGrid);
  AProduct.Add('sdxPatternDottedDiamond', @sdxPatternDottedDiamond);
  AProduct.Add('sdxPatternShingle', @sdxPatternShingle);
  AProduct.Add('sdxPatternTrellis', @sdxPatternTrellis);
  AProduct.Add('sdxPatternSphere', @sdxPatternSphere);
  AProduct.Add('sdxPatternSmallGrid', @sdxPatternSmallGrid);
  AProduct.Add('sdxPatternLargeGrid', @sdxPatternLargeGrid);
  AProduct.Add('sdxPatternSmallCheckedBoard', @sdxPatternSmallCheckedBoard);
  AProduct.Add('sdxPatternLargeCheckedBoard', @sdxPatternLargeCheckedBoard);
  AProduct.Add('sdxPatternOutlinedDiamond', @sdxPatternOutlinedDiamond);
  AProduct.Add('sdxPatternSolidDiamond', @sdxPatternSolidDiamond);
  AProduct.Add('sdxTextureNewSprint', @sdxTextureNewSprint);
  AProduct.Add('sdxTextureGreenMarble', @sdxTextureGreenMarble);
  AProduct.Add('sdxTextureBlueTissuePaper', @sdxTextureBlueTissuePaper);
  AProduct.Add('sdxTexturePapyrus', @sdxTexturePapyrus);
  AProduct.Add('sdxTextureWaterDroplets', @sdxTextureWaterDroplets);
  AProduct.Add('sdxTextureCork', @sdxTextureCork);
  AProduct.Add('sdxTextureRecycledPaper', @sdxTextureRecycledPaper);
  AProduct.Add('sdxTextureWhiteMarble', @sdxTextureWhiteMarble);
  AProduct.Add('sdxTexturePinkMarble', @sdxTexturePinkMarble);
  AProduct.Add('sdxTextureCanvas', @sdxTextureCanvas);
  AProduct.Add('sdxTexturePaperBag', @sdxTexturePaperBag);
  AProduct.Add('sdxTextureWalnut', @sdxTextureWalnut);
  AProduct.Add('sdxTextureParchment', @sdxTextureParchment);
  AProduct.Add('sdxTextureBrownMarble', @sdxTextureBrownMarble);
  AProduct.Add('sdxTexturePurpleMesh', @sdxTexturePurpleMesh);
  AProduct.Add('sdxTextureDenim', @sdxTextureDenim);
  AProduct.Add('sdxTextureFishFossil', @sdxTextureFishFossil);
  AProduct.Add('sdxTextureOak', @sdxTextureOak);
  AProduct.Add('sdxTextureStationary', @sdxTextureStationary);
  AProduct.Add('sdxTextureGranite', @sdxTextureGranite);
  AProduct.Add('sdxTextureBouquet', @sdxTextureBouquet);
  AProduct.Add('sdxTextureWonenMat', @sdxTextureWonenMat);
  AProduct.Add('sdxTextureSand', @sdxTextureSand);
  AProduct.Add('sdxTextureMediumWood', @sdxTextureMediumWood);
  AProduct.Add('sdxFSPCaption', @sdxFSPCaption);
  AProduct.Add('sdxWidth', @sdxWidth);
  AProduct.Add('sdxHeight', @sdxHeight);
  AProduct.Add('sdxBrushDlgCaption', @sdxBrushDlgCaption);
  AProduct.Add('sdxStyle', @sdxStyle);
  AProduct.Add('sdxENFNCaption', @sdxENFNCaption);
  AProduct.Add('sdxEnterNewFileName', @sdxEnterNewFileName);
  AProduct.Add('sdxDefinePrintStylesCaption', @sdxDefinePrintStylesCaption);
  AProduct.Add('sdxDefinePrintStylesTitle', @sdxDefinePrintStylesTitle);
  AProduct.Add('sdxDefinePrintStylesWarningDelete', @sdxDefinePrintStylesWarningDelete);
  AProduct.Add('sdxDefinePrintStylesWarningClear', @sdxDefinePrintStylesWarningClear);
  AProduct.Add('sdxClear', @sdxClear);
  AProduct.Add('sdxCustomSize', @sdxCustomSize);
  AProduct.Add('sdxDefaultTray', @sdxDefaultTray);
  AProduct.Add('sdxInvalidPrintDevice', @sdxInvalidPrintDevice);
  AProduct.Add('sdxNotPrinting', @sdxNotPrinting);
  AProduct.Add('sdxPrinting', @sdxPrinting);
  AProduct.Add('sdxDeviceOnPort', @sdxDeviceOnPort);
  AProduct.Add('sdxPrinterIndexError', @sdxPrinterIndexError);
  AProduct.Add('sdxNoDefaultPrintDevice', @sdxNoDefaultPrintDevice);
  AProduct.Add('sdxAutoTextDialogCaption', @sdxAutoTextDialogCaption);
  AProduct.Add('sdxEnterAutoTextEntriesHere', @sdxEnterAutoTextEntriesHere);
  AProduct.Add('sdxPrintDialogCaption', @sdxPrintDialogCaption);
  AProduct.Add('sdxPrintDialogPrinter', @sdxPrintDialogPrinter);
  AProduct.Add('sdxPrintDialogName', @sdxPrintDialogName);
  AProduct.Add('sdxPrintDialogStatus', @sdxPrintDialogStatus);
  AProduct.Add('sdxPrintDialogType', @sdxPrintDialogType);
  AProduct.Add('sdxPrintDialogWhere', @sdxPrintDialogWhere);
  AProduct.Add('sdxPrintDialogComment', @sdxPrintDialogComment);
  AProduct.Add('sdxPrintDialogPrintToFile', @sdxPrintDialogPrintToFile);
  AProduct.Add('sdxPrintDialogPageRange', @sdxPrintDialogPageRange);
  AProduct.Add('sdxPrintDialogAll', @sdxPrintDialogAll);
  AProduct.Add('sdxPrintDialogCurrentPage', @sdxPrintDialogCurrentPage);
  AProduct.Add('sdxPrintDialogSelection', @sdxPrintDialogSelection);
  AProduct.Add('sdxPrintDialogPages', @sdxPrintDialogPages);
  AProduct.Add('sdxPrintDialogRangeLegend', @sdxPrintDialogRangeLegend);
  AProduct.Add('sdxPrintDialogCopies', @sdxPrintDialogCopies);
  AProduct.Add('sdxPrintDialogNumberOfPages', @sdxPrintDialogNumberOfPages);
  AProduct.Add('sdxPrintDialogNumberOfCopies', @sdxPrintDialogNumberOfCopies);
  AProduct.Add('sdxPrintDialogCollateCopies', @sdxPrintDialogCollateCopies);
  AProduct.Add('sdxPrintDialogAllPages', @sdxPrintDialogAllPages);
  AProduct.Add('sdxPrintDialogEvenPages', @sdxPrintDialogEvenPages);
  AProduct.Add('sdxPrintDialogOddPages', @sdxPrintDialogOddPages);
  AProduct.Add('sdxPrintDialogPrintStyles', @sdxPrintDialogPrintStyles);
  AProduct.Add('sdxPrintDialogOpenDlgTitle', @sdxPrintDialogOpenDlgTitle);
  AProduct.Add('sdxPrintDialogOpenDlgAllFiles', @sdxPrintDialogOpenDlgAllFiles);
  AProduct.Add('sdxPrintDialogOpenDlgPrinterFiles', @sdxPrintDialogOpenDlgPrinterFiles);
  AProduct.Add('sdxPrintDialogPageNumbersOutOfRange', @sdxPrintDialogPageNumbersOutOfRange);
  AProduct.Add('sdxPrintDialogInvalidPageRanges', @sdxPrintDialogInvalidPageRanges);
  AProduct.Add('sdxPrintDialogRequiredPageNumbers', @sdxPrintDialogRequiredPageNumbers);
  AProduct.Add('sdxPrintDialogNoPrinters', @sdxPrintDialogNoPrinters);
  AProduct.Add('sdxPrintDialogInPrintingState', @sdxPrintDialogInPrintingState);
  AProduct.Add('sdxPrintDialogPSPaused', @sdxPrintDialogPSPaused);
  AProduct.Add('sdxPrintDialogPSPendingDeletion', @sdxPrintDialogPSPendingDeletion);
  AProduct.Add('sdxPrintDialogPSBusy', @sdxPrintDialogPSBusy);
  AProduct.Add('sdxPrintDialogPSDoorOpen', @sdxPrintDialogPSDoorOpen);
  AProduct.Add('sdxPrintDialogPSError', @sdxPrintDialogPSError);
  AProduct.Add('sdxPrintDialogPSInitializing', @sdxPrintDialogPSInitializing);
  AProduct.Add('sdxPrintDialogPSIOActive', @sdxPrintDialogPSIOActive);
  AProduct.Add('sdxPrintDialogPSManualFeed', @sdxPrintDialogPSManualFeed);
  AProduct.Add('sdxPrintDialogPSNoToner', @sdxPrintDialogPSNoToner);
  AProduct.Add('sdxPrintDialogPSNotAvailable', @sdxPrintDialogPSNotAvailable);
  AProduct.Add('sdxPrintDialogPSOFFLine', @sdxPrintDialogPSOFFLine);
  AProduct.Add('sdxPrintDialogPSOutOfMemory', @sdxPrintDialogPSOutOfMemory);
  AProduct.Add('sdxPrintDialogPSOutBinFull', @sdxPrintDialogPSOutBinFull);
  AProduct.Add('sdxPrintDialogPSPagePunt', @sdxPrintDialogPSPagePunt);
  AProduct.Add('sdxPrintDialogPSPaperJam', @sdxPrintDialogPSPaperJam);
  AProduct.Add('sdxPrintDialogPSPaperOut', @sdxPrintDialogPSPaperOut);
  AProduct.Add('sdxPrintDialogPSPaperProblem', @sdxPrintDialogPSPaperProblem);
  AProduct.Add('sdxPrintDialogPSPrinting', @sdxPrintDialogPSPrinting);
  AProduct.Add('sdxPrintDialogPSProcessing', @sdxPrintDialogPSProcessing);
  AProduct.Add('sdxPrintDialogPSTonerLow', @sdxPrintDialogPSTonerLow);
  AProduct.Add('sdxPrintDialogPSUserIntervention', @sdxPrintDialogPSUserIntervention);
  AProduct.Add('sdxPrintDialogPSWaiting', @sdxPrintDialogPSWaiting);
  AProduct.Add('sdxPrintDialogPSWarningUp', @sdxPrintDialogPSWarningUp);
  AProduct.Add('sdxPrintDialogPSReady', @sdxPrintDialogPSReady);
  AProduct.Add('sdxPrintDialogPSPrintingAndWaiting', @sdxPrintDialogPSPrintingAndWaiting);
  AProduct.Add('sdxLeftMargin', @sdxLeftMargin);
  AProduct.Add('sdxTopMargin', @sdxTopMargin);
  AProduct.Add('sdxRightMargin', @sdxRightMargin);
  AProduct.Add('sdxBottomMargin', @sdxBottomMargin);
  AProduct.Add('sdxGutterMargin', @sdxGutterMargin);
  AProduct.Add('sdxHeaderMargin', @sdxHeaderMargin);
  AProduct.Add('sdxFooterMargin', @sdxFooterMargin);
  AProduct.Add('sdxUnitsInches', @sdxUnitsInches);
  AProduct.Add('sdxUnitsCentimeters', @sdxUnitsCentimeters);
  AProduct.Add('sdxUnitsMillimeters', @sdxUnitsMillimeters);
  AProduct.Add('sdxUnitsPoints', @sdxUnitsPoints);
  AProduct.Add('sdxUnitsPicas', @sdxUnitsPicas);
  AProduct.Add('sdxUnitsDefaultName', @sdxUnitsDefaultName);
  AProduct.Add('sdxUnitsInchesName', @sdxUnitsInchesName);
  AProduct.Add('sdxUnitsCentimetersName', @sdxUnitsCentimetersName);
  AProduct.Add('sdxUnitsMillimetersName', @sdxUnitsMillimetersName);
  AProduct.Add('sdxUnitsPointsName', @sdxUnitsPointsName);
  AProduct.Add('sdxUnitsPicasName', @sdxUnitsPicasName);
  AProduct.Add('sdxPrintPreview', @sdxPrintPreview);
  AProduct.Add('sdxReportDesignerCaption', @sdxReportDesignerCaption);
  AProduct.Add('sdxCompositionDesignerCaption', @sdxCompositionDesignerCaption);
  AProduct.Add('sdxCompositionStartEachItemFromNewPage', @sdxCompositionStartEachItemFromNewPage);
  AProduct.Add('sdxComponentNotSupportedByLink', @sdxComponentNotSupportedByLink);
end;

procedure AddResourceStringsPart2(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxComponentNotSupported', @sdxComponentNotSupported);
  AProduct.Add('sdxPrintDeviceNotReady', @sdxPrintDeviceNotReady);
  AProduct.Add('sdxUnableToGenerateReport', @sdxUnableToGenerateReport);
  AProduct.Add('sdxPreviewNotRegistered', @sdxPreviewNotRegistered);
  AProduct.Add('sdxComponentNotAssigned', @sdxComponentNotAssigned);
  AProduct.Add('sdxPrintDeviceIsBusy', @sdxPrintDeviceIsBusy);
  AProduct.Add('sdxPrintDeviceError', @sdxPrintDeviceError);
  AProduct.Add('sdxMissingComponent', @sdxMissingComponent);
  AProduct.Add('sdxDataProviderDontPresent', @sdxDataProviderDontPresent);
  AProduct.Add('sdxBuildingReport', @sdxBuildingReport);
  AProduct.Add('sdxPrintingReport', @sdxPrintingReport);
  AProduct.Add('sdxDefinePrintStylesMenuItem', @sdxDefinePrintStylesMenuItem);
  AProduct.Add('sdxAbortPrinting', @sdxAbortPrinting);
  AProduct.Add('sdxStandardStyle', @sdxStandardStyle);
  AProduct.Add('sdxFontStyleBold', @sdxFontStyleBold);
  AProduct.Add('sdxFontStyleItalic', @sdxFontStyleItalic);
  AProduct.Add('sdxFontStyleUnderline', @sdxFontStyleUnderline);
  AProduct.Add('sdxFontStyleStrikeOut', @sdxFontStyleStrikeOut);
  AProduct.Add('sdxPt', @sdxPt);
  AProduct.Add('sdxPagesSuffix', @sdxPagesSuffix);
  AProduct.Add('sdxPageWidth', @sdxPageWidth);
  AProduct.Add('sdxWholePage', @sdxWholePage);
  AProduct.Add('sdxTwoPages', @sdxTwoPages);
  AProduct.Add('sdxFourPages', @sdxFourPages);
  AProduct.Add('sdxWidenToSourceWidth', @sdxWidenToSourceWidth);
  AProduct.Add('sdxMenuBar', @sdxMenuBar);
  AProduct.Add('sdxStandardBar', @sdxStandardBar);
  AProduct.Add('sdxHeaderFooterBar', @sdxHeaderFooterBar);
  AProduct.Add('sdxShortcutMenusBar', @sdxShortcutMenusBar);
  AProduct.Add('sdxAutoTextBar', @sdxAutoTextBar);
  AProduct.Add('sdxMenuFile', @sdxMenuFile);
  AProduct.Add('sdxMenuFileDesign', @sdxMenuFileDesign);
  AProduct.Add('sdxMenuFilePrint', @sdxMenuFilePrint);
  AProduct.Add('sdxMenuFilePrintDialog', @sdxMenuFilePrintDialog);
  AProduct.Add('sdxMenuFilePageSetup', @sdxMenuFilePageSetup);
  AProduct.Add('sdxMenuPrintStyles', @sdxMenuPrintStyles);
  AProduct.Add('sdxMenuFileExit', @sdxMenuFileExit);
  AProduct.Add('sdxMenuExportToPDF', @sdxMenuExportToPDF);
  AProduct.Add('sdxMenuFileOptions', @sdxMenuFileOptions);
  AProduct.Add('sdxMenuEdit', @sdxMenuEdit);
  AProduct.Add('sdxMenuEditCut', @sdxMenuEditCut);
  AProduct.Add('sdxMenuEditCopy', @sdxMenuEditCopy);
  AProduct.Add('sdxMenuEditPaste', @sdxMenuEditPaste);
  AProduct.Add('sdxMenuEditDelete', @sdxMenuEditDelete);
  AProduct.Add('sdxMenuEditFind', @sdxMenuEditFind);
  AProduct.Add('sdxMenuEditFindNext', @sdxMenuEditFindNext);
  AProduct.Add('sdxMenuEditReplace', @sdxMenuEditReplace);
  AProduct.Add('sdxMenuLoad', @sdxMenuLoad);
  AProduct.Add('sdxMenuPreview', @sdxMenuPreview);
  AProduct.Add('sdxMenuInsert', @sdxMenuInsert);
  AProduct.Add('sdxMenuInsertAutoText', @sdxMenuInsertAutoText);
  AProduct.Add('sdxMenuInsertEditAutoTextEntries', @sdxMenuInsertEditAutoTextEntries);
  AProduct.Add('sdxMenuInsertAutoTextEntries', @sdxMenuInsertAutoTextEntries);
  AProduct.Add('sdxMenuInsertAutoTextEntriesSubItem', @sdxMenuInsertAutoTextEntriesSubItem);
  AProduct.Add('sdxMenuInsertPageNumber', @sdxMenuInsertPageNumber);
  AProduct.Add('sdxMenuInsertTotalPages', @sdxMenuInsertTotalPages);
  AProduct.Add('sdxMenuInsertPageOfPages', @sdxMenuInsertPageOfPages);
  AProduct.Add('sdxMenuInsertDateTime', @sdxMenuInsertDateTime);
  AProduct.Add('sdxMenuInsertDate', @sdxMenuInsertDate);
  AProduct.Add('sdxMenuInsertTime', @sdxMenuInsertTime);
  AProduct.Add('sdxMenuInsertUserName', @sdxMenuInsertUserName);
  AProduct.Add('sdxMenuInsertMachineName', @sdxMenuInsertMachineName);
  AProduct.Add('sdxMenuView', @sdxMenuView);
  AProduct.Add('sdxMenuViewMargins', @sdxMenuViewMargins);
  AProduct.Add('sdxMenuViewFlatToolBarButtons', @sdxMenuViewFlatToolBarButtons);
  AProduct.Add('sdxMenuViewLargeToolBarButtons', @sdxMenuViewLargeToolBarButtons);
  AProduct.Add('sdxMenuViewMarginsStatusBar', @sdxMenuViewMarginsStatusBar);
  AProduct.Add('sdxMenuViewPagesStatusBar', @sdxMenuViewPagesStatusBar);
  AProduct.Add('sdxMenuViewToolBars', @sdxMenuViewToolBars);
  AProduct.Add('sdxMenuViewPagesHeaders', @sdxMenuViewPagesHeaders);
  AProduct.Add('sdxMenuViewPagesFooters', @sdxMenuViewPagesFooters);
  AProduct.Add('sdxMenuViewSwitchToLeftPart', @sdxMenuViewSwitchToLeftPart);
  AProduct.Add('sdxMenuViewSwitchToRightPart', @sdxMenuViewSwitchToRightPart);
  AProduct.Add('sdxMenuViewSwitchToCenterPart', @sdxMenuViewSwitchToCenterPart);
  AProduct.Add('sdxMenuViewHFSwitchHeaderFooter', @sdxMenuViewHFSwitchHeaderFooter);
  AProduct.Add('sdxMenuViewSwitchToFooter', @sdxMenuViewSwitchToFooter);
  AProduct.Add('sdxMenuViewSwitchToHeader', @sdxMenuViewSwitchToHeader);
  AProduct.Add('sdxMenuViewHFClose', @sdxMenuViewHFClose);
  AProduct.Add('sdxMenuZoom', @sdxMenuZoom);
  AProduct.Add('sdxMenuZoomPercent100', @sdxMenuZoomPercent100);
  AProduct.Add('sdxMenuZoomPageWidth', @sdxMenuZoomPageWidth);
  AProduct.Add('sdxMenuZoomWholePage', @sdxMenuZoomWholePage);
  AProduct.Add('sdxMenuZoomTwoPages', @sdxMenuZoomTwoPages);
  AProduct.Add('sdxMenuZoomFourPages', @sdxMenuZoomFourPages);
  AProduct.Add('sdxMenuZoomMultiplyPages', @sdxMenuZoomMultiplyPages);
  AProduct.Add('sdxMenuZoomWidenToSourceWidth', @sdxMenuZoomWidenToSourceWidth);
  AProduct.Add('sdxMenuZoomSetup', @sdxMenuZoomSetup);
  AProduct.Add('sdxMenuPages', @sdxMenuPages);
  AProduct.Add('sdxMenuGotoPage', @sdxMenuGotoPage);
  AProduct.Add('sdxMenuGotoPageFirst', @sdxMenuGotoPageFirst);
  AProduct.Add('sdxMenuGotoPagePrev', @sdxMenuGotoPagePrev);
  AProduct.Add('sdxMenuGotoPageNext', @sdxMenuGotoPageNext);
  AProduct.Add('sdxMenuGotoPageLast', @sdxMenuGotoPageLast);
  AProduct.Add('sdxMenuActivePage', @sdxMenuActivePage);
  AProduct.Add('sdxMenuFormat', @sdxMenuFormat);
  AProduct.Add('sdxMenuFormatHeaderAndFooter', @sdxMenuFormatHeaderAndFooter);
  AProduct.Add('sdxMenuFormatAutoTextEntries', @sdxMenuFormatAutoTextEntries);
  AProduct.Add('sdxMenuFormatDateTime', @sdxMenuFormatDateTime);
  AProduct.Add('sdxMenuFormatPageNumbering', @sdxMenuFormatPageNumbering);
  AProduct.Add('sdxMenuFormatPageBackground', @sdxMenuFormatPageBackground);
  AProduct.Add('sdxMenuFormatShrinkToPage', @sdxMenuFormatShrinkToPage);
  AProduct.Add('sdxMenuFormatHFBackground', @sdxMenuFormatHFBackground);
  AProduct.Add('sdxMenuFormatHFClear', @sdxMenuFormatHFClear);
  AProduct.Add('sdxMenuTools', @sdxMenuTools);
  AProduct.Add('sdxMenuToolsCustomize', @sdxMenuToolsCustomize);
  AProduct.Add('sdxMenuToolsOptions', @sdxMenuToolsOptions);
  AProduct.Add('sdxMenuHelp', @sdxMenuHelp);
  AProduct.Add('sdxMenuHelpTopics', @sdxMenuHelpTopics);
  AProduct.Add('sdxMenuHelpAbout', @sdxMenuHelpAbout);
  AProduct.Add('sdxMenuShortcutPreview', @sdxMenuShortcutPreview);
  AProduct.Add('sdxMenuShortcutAutoText', @sdxMenuShortcutAutoText);
  AProduct.Add('sdxMenuBuiltInMenus', @sdxMenuBuiltInMenus);
  AProduct.Add('sdxMenuShortCutMenus', @sdxMenuShortCutMenus);
  AProduct.Add('sdxMenuNewMenu', @sdxMenuNewMenu);
  AProduct.Add('sdxHintFileDesign', @sdxHintFileDesign);
  AProduct.Add('sdxHintFilePrint', @sdxHintFilePrint);
  AProduct.Add('sdxHintFilePrintDialog', @sdxHintFilePrintDialog);
  AProduct.Add('sdxHintFilePageSetup', @sdxHintFilePageSetup);
  AProduct.Add('sdxHintFileExit', @sdxHintFileExit);
  AProduct.Add('sdxHintExportToPDF', @sdxHintExportToPDF);
  AProduct.Add('sdxHintEditFind', @sdxHintEditFind);
  AProduct.Add('sdxHintEditFindNext', @sdxHintEditFindNext);
  AProduct.Add('sdxHintEditReplace', @sdxHintEditReplace);
  AProduct.Add('sdxHintInsertEditAutoTextEntries', @sdxHintInsertEditAutoTextEntries);
  AProduct.Add('sdxHintInsertPageNumber', @sdxHintInsertPageNumber);
  AProduct.Add('sdxHintInsertTotalPages', @sdxHintInsertTotalPages);
  AProduct.Add('sdxHintInsertPageOfPages', @sdxHintInsertPageOfPages);
  AProduct.Add('sdxHintInsertDateTime', @sdxHintInsertDateTime);
  AProduct.Add('sdxHintInsertDate', @sdxHintInsertDate);
  AProduct.Add('sdxHintInsertTime', @sdxHintInsertTime);
  AProduct.Add('sdxHintInsertUserName', @sdxHintInsertUserName);
  AProduct.Add('sdxHintInsertMachineName', @sdxHintInsertMachineName);
  AProduct.Add('sdxHintViewMargins', @sdxHintViewMargins);
  AProduct.Add('sdxHintViewLargeButtons', @sdxHintViewLargeButtons);
  AProduct.Add('sdxHintViewMarginsStatusBar', @sdxHintViewMarginsStatusBar);
  AProduct.Add('sdxHintViewPagesStatusBar', @sdxHintViewPagesStatusBar);
  AProduct.Add('sdxHintViewPagesHeaders', @sdxHintViewPagesHeaders);
  AProduct.Add('sdxHintViewPagesFooters', @sdxHintViewPagesFooters);
  AProduct.Add('sdxHintViewSwitchToLeftPart', @sdxHintViewSwitchToLeftPart);
  AProduct.Add('sdxHintViewSwitchToRightPart', @sdxHintViewSwitchToRightPart);
  AProduct.Add('sdxHintViewSwitchToCenterPart', @sdxHintViewSwitchToCenterPart);
  AProduct.Add('sdxHintViewHFSwitchHeaderFooter', @sdxHintViewHFSwitchHeaderFooter);
  AProduct.Add('sdxHintViewSwitchToHeader', @sdxHintViewSwitchToHeader);
  AProduct.Add('sdxHintViewSwitchToFooter', @sdxHintViewSwitchToFooter);
  AProduct.Add('sdxHintViewHFClose', @sdxHintViewHFClose);
  AProduct.Add('sdxHintViewZoom', @sdxHintViewZoom);
  AProduct.Add('sdxHintZoomPercent100', @sdxHintZoomPercent100);
  AProduct.Add('sdxHintZoomPageWidth', @sdxHintZoomPageWidth);
  AProduct.Add('sdxHintZoomWholePage', @sdxHintZoomWholePage);
  AProduct.Add('sdxHintZoomTwoPages', @sdxHintZoomTwoPages);
  AProduct.Add('sdxHintZoomFourPages', @sdxHintZoomFourPages);
  AProduct.Add('sdxHintZoomMultiplyPages', @sdxHintZoomMultiplyPages);
  AProduct.Add('sdxHintZoomWidenToSourceWidth', @sdxHintZoomWidenToSourceWidth);
  AProduct.Add('sdxHintZoomSetup', @sdxHintZoomSetup);
  AProduct.Add('sdxHintFormatDateTime', @sdxHintFormatDateTime);
  AProduct.Add('sdxHintFormatPageNumbering', @sdxHintFormatPageNumbering);
  AProduct.Add('sdxHintFormatPageBackground', @sdxHintFormatPageBackground);
  AProduct.Add('sdxHintFormatShrinkToPage', @sdxHintFormatShrinkToPage);
  AProduct.Add('sdxHintFormatHFBackground', @sdxHintFormatHFBackground);
  AProduct.Add('sdxHintFormatHFClear', @sdxHintFormatHFClear);
  AProduct.Add('sdxHintGotoPageFirst', @sdxHintGotoPageFirst);
  AProduct.Add('sdxHintGotoPagePrev', @sdxHintGotoPagePrev);
  AProduct.Add('sdxHintGotoPageNext', @sdxHintGotoPageNext);
  AProduct.Add('sdxHintGotoPageLast', @sdxHintGotoPageLast);
  AProduct.Add('sdxHintActivePage', @sdxHintActivePage);
  AProduct.Add('sdxHintToolsCustomize', @sdxHintToolsCustomize);
  AProduct.Add('sdxHintToolsOptions', @sdxHintToolsOptions);
  AProduct.Add('sdxHintHelpTopics', @sdxHintHelpTopics);
  AProduct.Add('sdxHintHelpAbout', @sdxHintHelpAbout);
  AProduct.Add('sdxPopupMenuLargeButtons', @sdxPopupMenuLargeButtons);
  AProduct.Add('sdxPopupMenuFlatButtons', @sdxPopupMenuFlatButtons);
  AProduct.Add('sdxPaperSize', @sdxPaperSize);
  AProduct.Add('sdxStatus', @sdxStatus);
  AProduct.Add('sdxStatusReady', @sdxStatusReady);
  AProduct.Add('sdxStatusPrinting', @sdxStatusPrinting);
  AProduct.Add('sdxStatusGenerateReport', @sdxStatusGenerateReport);
  AProduct.Add('sdxHintDoubleClickForChangePaperSize', @sdxHintDoubleClickForChangePaperSize);
  AProduct.Add('sdxHintDoubleClickForChangeMargins', @sdxHintDoubleClickForChangeMargins);
  AProduct.Add('sdxDTFormatsCaption', @sdxDTFormatsCaption);
  AProduct.Add('sdxDTFormatsAvailableDateFormats', @sdxDTFormatsAvailableDateFormats);
  AProduct.Add('sdxDTFormatsAvailableTimeFormats', @sdxDTFormatsAvailableTimeFormats);
  AProduct.Add('sdxDTFormatsAutoUpdate', @sdxDTFormatsAutoUpdate);
  AProduct.Add('sdxDTFormatsChangeDefaultFormat', @sdxDTFormatsChangeDefaultFormat);
  AProduct.Add('sdxPNFormatsCaption', @sdxPNFormatsCaption);
  AProduct.Add('sdxPageNumbering', @sdxPageNumbering);
  AProduct.Add('sdxPNFormatsNumberFormat', @sdxPNFormatsNumberFormat);
  AProduct.Add('sdxPNFormatsContinueFromPrevious', @sdxPNFormatsContinueFromPrevious);
  AProduct.Add('sdxPNFormatsStartAt', @sdxPNFormatsStartAt);
  AProduct.Add('sdxPNFormatsChangeDefaultFormat', @sdxPNFormatsChangeDefaultFormat);
  AProduct.Add('sdxZoomDlgCaption', @sdxZoomDlgCaption);
  AProduct.Add('sdxZoomDlgZoomTo', @sdxZoomDlgZoomTo);
  AProduct.Add('sdxZoomDlgPageWidth', @sdxZoomDlgPageWidth);
  AProduct.Add('sdxZoomDlgWholePage', @sdxZoomDlgWholePage);
  AProduct.Add('sdxZoomDlgTwoPages', @sdxZoomDlgTwoPages);
  AProduct.Add('sdxZoomDlgFourPages', @sdxZoomDlgFourPages);
  AProduct.Add('sdxZoomDlgManyPages', @sdxZoomDlgManyPages);
  AProduct.Add('sdxZoomDlgPercent', @sdxZoomDlgPercent);
  AProduct.Add('sdxZoomDlgPreview', @sdxZoomDlgPreview);
  AProduct.Add('sdxZoomDlgFontPreview', @sdxZoomDlgFontPreview);
  AProduct.Add('sdxZoomDlgFontPreviewString', @sdxZoomDlgFontPreviewString);
  AProduct.Add('sdxPages', @sdxPages);
  AProduct.Add('sdxCancel', @sdxCancel);
  AProduct.Add('sdxPreferenceDlgCaption', @sdxPreferenceDlgCaption);
  AProduct.Add('sdxPreferenceDlgTab1', @sdxPreferenceDlgTab1);
  AProduct.Add('sdxPreferenceDlgTab2', @sdxPreferenceDlgTab2);
  AProduct.Add('sdxPreferenceDlgTab3', @sdxPreferenceDlgTab3);
  AProduct.Add('sdxPreferenceDlgTab4', @sdxPreferenceDlgTab4);
  AProduct.Add('sdxPreferenceDlgTab5', @sdxPreferenceDlgTab5);
  AProduct.Add('sdxPreferenceDlgTab6', @sdxPreferenceDlgTab6);
  AProduct.Add('sdxPreferenceDlgTab7', @sdxPreferenceDlgTab7);
  AProduct.Add('sdxPreferenceDlgTab8', @sdxPreferenceDlgTab8);
  AProduct.Add('sdxPreferenceDlgTab9', @sdxPreferenceDlgTab9);
  AProduct.Add('sdxPreferenceDlgTab10', @sdxPreferenceDlgTab10);
  AProduct.Add('sdxPreferenceDlgShow', @sdxPreferenceDlgShow);
  AProduct.Add('sdxPreferenceDlgMargins', @sdxPreferenceDlgMargins);
  AProduct.Add('sdxPreferenceDlgMarginsHints', @sdxPreferenceDlgMarginsHints);
  AProduct.Add('sdxPreferenceDlgMargingWhileDragging', @sdxPreferenceDlgMargingWhileDragging);
  AProduct.Add('sdxPreferenceDlgLargeBtns', @sdxPreferenceDlgLargeBtns);
  AProduct.Add('sdxPreferenceDlgFlatBtns', @sdxPreferenceDlgFlatBtns);
  AProduct.Add('sdxPreferenceDlgMarginsColor', @sdxPreferenceDlgMarginsColor);
  AProduct.Add('sdxPreferenceDlgMeasurementUnits', @sdxPreferenceDlgMeasurementUnits);
  AProduct.Add('sdxPreferenceDlgSaveForRunTimeToo', @sdxPreferenceDlgSaveForRunTimeToo);
  AProduct.Add('sdxPreferenceDlgZoomScroll', @sdxPreferenceDlgZoomScroll);
  AProduct.Add('sdxPreferenceDlgZoomStep', @sdxPreferenceDlgZoomStep);
  AProduct.Add('sdxCloneStyleCaptionPrefix', @sdxCloneStyleCaptionPrefix);
  AProduct.Add('sdxInvalideStyleCaption', @sdxInvalideStyleCaption);
  AProduct.Add('sdxHintMoreHFFunctions', @sdxHintMoreHFFunctions);
  AProduct.Add('sdxPageSetupCaption', @sdxPageSetupCaption);
  AProduct.Add('sdxStyleName', @sdxStyleName);
  AProduct.Add('sdxPage', @sdxPage);
  AProduct.Add('sdxMargins', @sdxMargins);
  AProduct.Add('sdxHeaderFooter', @sdxHeaderFooter);
  AProduct.Add('sdxScaling', @sdxScaling);
  AProduct.Add('sdxScaleTo', @sdxScaleTo);
  AProduct.Add('sdxAutomatic', @sdxAutomatic);
  AProduct.Add('sdxFitToPageHorizontally', @sdxFitToPageHorizontally);
  AProduct.Add('sdxFitToPageVertically', @sdxFitToPageVertically);
  AProduct.Add('sdxPaper', @sdxPaper);
  AProduct.Add('sdxPaperType', @sdxPaperType);
  AProduct.Add('sdxPaperDimension', @sdxPaperDimension);
  AProduct.Add('sdxPaperWidth', @sdxPaperWidth);
  AProduct.Add('sdxPaperHeight', @sdxPaperHeight);
  AProduct.Add('sdxPaperSource', @sdxPaperSource);
  AProduct.Add('sdxOrientation', @sdxOrientation);
  AProduct.Add('sdxPortrait', @sdxPortrait);
  AProduct.Add('sdxLandscape', @sdxLandscape);
  AProduct.Add('sdxAutoOrientation', @sdxAutoOrientation);
  AProduct.Add('sdxPrintOrder', @sdxPrintOrder);
  AProduct.Add('sdxDownThenOver', @sdxDownThenOver);
  AProduct.Add('sdxOverThenDown', @sdxOverThenDown);
  AProduct.Add('sdxShading', @sdxShading);
  AProduct.Add('sdxPrintUsingGrayShading', @sdxPrintUsingGrayShading);
  AProduct.Add('sdxCenterOnPage', @sdxCenterOnPage);
  AProduct.Add('sdxHorizontally', @sdxHorizontally);
  AProduct.Add('sdxVertically', @sdxVertically);
  AProduct.Add('sdxHeader', @sdxHeader);
  AProduct.Add('sdxBtnHeaderFont', @sdxBtnHeaderFont);
  AProduct.Add('sdxBtnHeaderBackground', @sdxBtnHeaderBackground);
  AProduct.Add('sdxFooter', @sdxFooter);
  AProduct.Add('sdxBtnFooterFont', @sdxBtnFooterFont);
  AProduct.Add('sdxBtnFooterBackground', @sdxBtnFooterBackground);
  AProduct.Add('sdxTop', @sdxTop);
  AProduct.Add('sdxLeft', @sdxLeft);
  AProduct.Add('sdxRight', @sdxRight);
  AProduct.Add('sdxBottom', @sdxBottom);
  AProduct.Add('sdxHeader2', @sdxHeader2);
  AProduct.Add('sdxFooter2', @sdxFooter2);
  AProduct.Add('sdxAlignment', @sdxAlignment);
  AProduct.Add('sdxVertAlignment', @sdxVertAlignment);
  AProduct.Add('sdxReverseOnEvenPages', @sdxReverseOnEvenPages);
  AProduct.Add('sdxAdjustTo', @sdxAdjustTo);
  AProduct.Add('sdxFitTo', @sdxFitTo);
  AProduct.Add('sdxPercentOfNormalSize', @sdxPercentOfNormalSize);
  AProduct.Add('sdxPagesWideBy', @sdxPagesWideBy);
  AProduct.Add('sdxTall', @sdxTall);
  AProduct.Add('sdxOf', @sdxOf);
  AProduct.Add('sdxLastPrinted', @sdxLastPrinted);
  AProduct.Add('sdxFileName', @sdxFileName);
  AProduct.Add('sdxFileNameAndPath', @sdxFileNameAndPath);
  AProduct.Add('sdxPrintedBy', @sdxPrintedBy);
  AProduct.Add('sdxPrintedOn', @sdxPrintedOn);
  AProduct.Add('sdxCreatedBy', @sdxCreatedBy);
  AProduct.Add('sdxCreatedOn', @sdxCreatedOn);
  AProduct.Add('sdxConfidential', @sdxConfidential);
  AProduct.Add('sdxHFFunctionNameUnknown', @sdxHFFunctionNameUnknown);
  AProduct.Add('sdxHFFunctionNamePageNumber', @sdxHFFunctionNamePageNumber);
  AProduct.Add('sdxHFFunctionNameTotalPages', @sdxHFFunctionNameTotalPages);
  AProduct.Add('sdxHFFunctionNamePageOfPages', @sdxHFFunctionNamePageOfPages);
  AProduct.Add('sdxHFFunctionNameDateTime', @sdxHFFunctionNameDateTime);
  AProduct.Add('sdxHFFunctionNameImage', @sdxHFFunctionNameImage);
  AProduct.Add('sdxHFFunctionNameDate', @sdxHFFunctionNameDate);
  AProduct.Add('sdxHFFunctionNameTime', @sdxHFFunctionNameTime);
  AProduct.Add('sdxHFFunctionNameUserName', @sdxHFFunctionNameUserName);
  AProduct.Add('sdxHFFunctionNameMachineName', @sdxHFFunctionNameMachineName);
  AProduct.Add('sdxHFFunctionHintPageNumber', @sdxHFFunctionHintPageNumber);
  AProduct.Add('sdxHFFunctionHintTotalPages', @sdxHFFunctionHintTotalPages);
  AProduct.Add('sdxHFFunctionHintPageOfPages', @sdxHFFunctionHintPageOfPages);
  AProduct.Add('sdxHFFunctionHintDateTime', @sdxHFFunctionHintDateTime);
  AProduct.Add('sdxHFFunctionHintImage', @sdxHFFunctionHintImage);
  AProduct.Add('sdxHFFunctionHintDate', @sdxHFFunctionHintDate);
  AProduct.Add('sdxHFFunctionHintTime', @sdxHFFunctionHintTime);
  AProduct.Add('sdxHFFunctionHintUserName', @sdxHFFunctionHintUserName);
  AProduct.Add('sdxHFFunctionHintMachineName', @sdxHFFunctionHintMachineName);
  AProduct.Add('sdxHFFunctionTemplatePageNumber', @sdxHFFunctionTemplatePageNumber);
  AProduct.Add('sdxHFFunctionTemplateTotalPages', @sdxHFFunctionTemplateTotalPages);
  AProduct.Add('sdxHFFunctionTemplatePageOfPages', @sdxHFFunctionTemplatePageOfPages);
  AProduct.Add('sdxHFFunctionTemplateDateTime', @sdxHFFunctionTemplateDateTime);
  AProduct.Add('sdxHFFunctionTemplateImage', @sdxHFFunctionTemplateImage);
  AProduct.Add('sdxHFFunctionTemplateDate', @sdxHFFunctionTemplateDate);
  AProduct.Add('sdxHFFunctionTemplateTime', @sdxHFFunctionTemplateTime);
  AProduct.Add('sdxHFFunctionTemplateUserName', @sdxHFFunctionTemplateUserName);
  AProduct.Add('sdxHFFunctionTemplateMachineName', @sdxHFFunctionTemplateMachineName);
  AProduct.Add('sdxJanuary', @sdxJanuary);
  AProduct.Add('sdxFebruary', @sdxFebruary);
  AProduct.Add('sdxMarch', @sdxMarch);
  AProduct.Add('sdxApril', @sdxApril);
  AProduct.Add('sdxMay', @sdxMay);
  AProduct.Add('sdxJune', @sdxJune);
  AProduct.Add('sdxJuly', @sdxJuly);
  AProduct.Add('sdxAugust', @sdxAugust);
  AProduct.Add('sdxSeptember', @sdxSeptember);
  AProduct.Add('sdxOctober', @sdxOctober);
  AProduct.Add('sdxNovember', @sdxNovember);
  AProduct.Add('sdxDecember', @sdxDecember);
  AProduct.Add('sdxEast', @sdxEast);
  AProduct.Add('sdxWest', @sdxWest);
  AProduct.Add('sdxSouth', @sdxSouth);
  AProduct.Add('sdxNorth', @sdxNorth);
  AProduct.Add('sdxTotal', @sdxTotal);
  AProduct.Add('sdxPlan', @sdxPlan);
  AProduct.Add('sdxSwimmingPool', @sdxSwimmingPool);
  AProduct.Add('sdxAdministration', @sdxAdministration);
  AProduct.Add('sdxPark', @sdxPark);
  AProduct.Add('sdxCarParking', @sdxCarParking);
  AProduct.Add('sdxCorporateHeadquarters', @sdxCorporateHeadquarters);
  AProduct.Add('sdxSalesAndMarketing', @sdxSalesAndMarketing);
  AProduct.Add('sdxEngineering', @sdxEngineering);
  AProduct.Add('sdxFieldOfficeCanada', @sdxFieldOfficeCanada);
  AProduct.Add('sdxOrderNoCaption', @sdxOrderNoCaption);
  AProduct.Add('sdxNameCaption', @sdxNameCaption);
  AProduct.Add('sdxCountCaption', @sdxCountCaption);
  AProduct.Add('sdxCompanyCaption', @sdxCompanyCaption);
  AProduct.Add('sdxAddressCaption', @sdxAddressCaption);
  AProduct.Add('sdxPriceCaption', @sdxPriceCaption);
  AProduct.Add('sdxCashCaption', @sdxCashCaption);
  AProduct.Add('sdxName1', @sdxName1);
  AProduct.Add('sdxName2', @sdxName2);
  AProduct.Add('sdxCompany1', @sdxCompany1);
  AProduct.Add('sdxCompany2', @sdxCompany2);
  AProduct.Add('sdxAddress1', @sdxAddress1);
  AProduct.Add('sdxAddress2', @sdxAddress2);
  AProduct.Add('sdxCountIs', @sdxCountIs);
  AProduct.Add('sdxRegular', @sdxRegular);
  AProduct.Add('sdxIrregular', @sdxIrregular);
  AProduct.Add('sdxTLBand', @sdxTLBand);
  AProduct.Add('sdxTLColumnName', @sdxTLColumnName);
  AProduct.Add('sdxTLColumnAxisymmetric', @sdxTLColumnAxisymmetric);
  AProduct.Add('sdxTLColumnItemShape', @sdxTLColumnItemShape);
  AProduct.Add('sdxItemShapeAsText', @sdxItemShapeAsText);
  AProduct.Add('sdxItem1Name', @sdxItem1Name);
  AProduct.Add('sdxItem2Name', @sdxItem2Name);
  AProduct.Add('sdxItem3Name', @sdxItem3Name);
  AProduct.Add('sdxItem4Name', @sdxItem4Name);
  AProduct.Add('sdxItem5Name', @sdxItem5Name);
  AProduct.Add('sdxItem1Description', @sdxItem1Description);
  AProduct.Add('sdxItem2Description', @sdxItem2Description);
  AProduct.Add('sdxItem3Description', @sdxItem3Description);
  AProduct.Add('sdxItem4Description', @sdxItem4Description);
  AProduct.Add('sdxItem5Description', @sdxItem5Description);
  AProduct.Add('sdxItem6Description', @sdxItem6Description);
  AProduct.Add('sdxItem7Description', @sdxItem7Description);
  AProduct.Add('sdxPatternIsNotRegistered', @sdxPatternIsNotRegistered);
  AProduct.Add('sdxSolidEdgePattern', @sdxSolidEdgePattern);
  AProduct.Add('sdxThinSolidEdgePattern', @sdxThinSolidEdgePattern);
  AProduct.Add('sdxMediumSolidEdgePattern', @sdxMediumSolidEdgePattern);
  AProduct.Add('sdxThickSolidEdgePattern', @sdxThickSolidEdgePattern);
  AProduct.Add('sdxDottedEdgePattern', @sdxDottedEdgePattern);
  AProduct.Add('sdxDashedEdgePattern', @sdxDashedEdgePattern);
  AProduct.Add('sdxDashDotDotEdgePattern', @sdxDashDotDotEdgePattern);
  AProduct.Add('sdxDashDotEdgePattern', @sdxDashDotEdgePattern);
  AProduct.Add('sdxSlantedDashDotEdgePattern', @sdxSlantedDashDotEdgePattern);
  AProduct.Add('sdxMediumDashDotDotEdgePattern', @sdxMediumDashDotDotEdgePattern);
  AProduct.Add('sdxHairEdgePattern', @sdxHairEdgePattern);
  AProduct.Add('sdxMediumDashDotEdgePattern', @sdxMediumDashDotEdgePattern);
  AProduct.Add('sdxMediumDashedEdgePattern', @sdxMediumDashedEdgePattern);
  AProduct.Add('sdxDoubleLineEdgePattern', @sdxDoubleLineEdgePattern);
  AProduct.Add('sdxSolidFillPattern', @sdxSolidFillPattern);
  AProduct.Add('sdxGray75FillPattern', @sdxGray75FillPattern);
  AProduct.Add('sdxGray50FillPattern', @sdxGray50FillPattern);
  AProduct.Add('sdxGray25FillPattern', @sdxGray25FillPattern);
  AProduct.Add('sdxGray125FillPattern', @sdxGray125FillPattern);
  AProduct.Add('sdxGray625FillPattern', @sdxGray625FillPattern);
  AProduct.Add('sdxHorizontalStripeFillPattern', @sdxHorizontalStripeFillPattern);
  AProduct.Add('sdxVerticalStripeFillPattern', @sdxVerticalStripeFillPattern);
  AProduct.Add('sdxReverseDiagonalStripeFillPattern', @sdxReverseDiagonalStripeFillPattern);
  AProduct.Add('sdxDiagonalStripeFillPattern', @sdxDiagonalStripeFillPattern);
  AProduct.Add('sdxDiagonalCrossHatchFillPattern', @sdxDiagonalCrossHatchFillPattern);
  AProduct.Add('sdxThickCrossHatchFillPattern', @sdxThickCrossHatchFillPattern);
  AProduct.Add('sdxThinHorizontalStripeFillPattern', @sdxThinHorizontalStripeFillPattern);
  AProduct.Add('sdxThinVerticalStripeFillPattern', @sdxThinVerticalStripeFillPattern);
  AProduct.Add('sdxThinReverseDiagonalStripeFillPattern', @sdxThinReverseDiagonalStripeFillPattern);
  AProduct.Add('sdxThinDiagonalStripeFillPattern', @sdxThinDiagonalStripeFillPattern);
  AProduct.Add('sdxThinHorizontalCrossHatchFillPattern', @sdxThinHorizontalCrossHatchFillPattern);
  AProduct.Add('sdxThinDiagonalCrossHatchFillPattern', @sdxThinDiagonalCrossHatchFillPattern);
  AProduct.Add('sdxShowRowAndColumnHeadings', @sdxShowRowAndColumnHeadings);
  AProduct.Add('sdxShowGridLines', @sdxShowGridLines);
  AProduct.Add('sdxSuppressSourceFormats', @sdxSuppressSourceFormats);
  AProduct.Add('sdxRepeatHeaderRowAtTop', @sdxRepeatHeaderRowAtTop);
  AProduct.Add('sdxDataToPrintDoesNotExist', @sdxDataToPrintDoesNotExist);
  AProduct.Add('sdxJanuaryShort', @sdxJanuaryShort);
  AProduct.Add('sdxFebruaryShort', @sdxFebruaryShort);
  AProduct.Add('sdxMarchShort', @sdxMarchShort);
  AProduct.Add('sdxAprilShort', @sdxAprilShort);
  AProduct.Add('sdxMayShort', @sdxMayShort);
  AProduct.Add('sdxJuneShort', @sdxJuneShort);
  AProduct.Add('sdxJulyShort', @sdxJulyShort);
  AProduct.Add('sdxAugustShort', @sdxAugustShort);
  AProduct.Add('sdxSeptemberShort', @sdxSeptemberShort);
  AProduct.Add('sdxOctoberShort', @sdxOctoberShort);
  AProduct.Add('sdxNovemberShort', @sdxNovemberShort);
  AProduct.Add('sdxDecemberShort', @sdxDecemberShort);
  AProduct.Add('sdxTechnicalDepartment', @sdxTechnicalDepartment);
  AProduct.Add('sdxSoftwareDepartment', @sdxSoftwareDepartment);
  AProduct.Add('sdxSystemProgrammers', @sdxSystemProgrammers);
  AProduct.Add('sdxEndUserProgrammers', @sdxEndUserProgrammers);
  AProduct.Add('sdxBetaTesters', @sdxBetaTesters);
  AProduct.Add('sdxHumanResourceDepartment', @sdxHumanResourceDepartment);
  AProduct.Add('sdxTreeLines', @sdxTreeLines);
  AProduct.Add('sdxTreeLinesColor', @sdxTreeLinesColor);
  AProduct.Add('sdxExpandButtons', @sdxExpandButtons);
  AProduct.Add('sdxCheckMarks', @sdxCheckMarks);
  AProduct.Add('sdxTreeEffects', @sdxTreeEffects);
  AProduct.Add('sdxAppearance', @sdxAppearance);
  AProduct.Add('sdxCarLevelCaption', @sdxCarLevelCaption);
  AProduct.Add('sdxManufacturerBandCaption', @sdxManufacturerBandCaption);
  AProduct.Add('sdxModelBandCaption', @sdxModelBandCaption);
  AProduct.Add('sdxManufacturerNameColumnCaption', @sdxManufacturerNameColumnCaption);
  AProduct.Add('sdxManufacturerLogoColumnCaption', @sdxManufacturerLogoColumnCaption);
  AProduct.Add('sdxManufacturerCountryColumnCaption', @sdxManufacturerCountryColumnCaption);
  AProduct.Add('sdxCarModelColumnCaption', @sdxCarModelColumnCaption);
  AProduct.Add('sdxCarIsSUVColumnCaption', @sdxCarIsSUVColumnCaption);
  AProduct.Add('sdxCarPhotoColumnCaption', @sdxCarPhotoColumnCaption);
  AProduct.Add('sdxCarManufacturerName1', @sdxCarManufacturerName1);
  AProduct.Add('sdxCarManufacturerName2', @sdxCarManufacturerName2);
  AProduct.Add('sdxCarManufacturerName3', @sdxCarManufacturerName3);
  AProduct.Add('sdxCarManufacturerName4', @sdxCarManufacturerName4);
  AProduct.Add('sdxCarManufacturerCountry1', @sdxCarManufacturerCountry1);
  AProduct.Add('sdxCarManufacturerCountry2', @sdxCarManufacturerCountry2);
  AProduct.Add('sdxCarManufacturerCountry3', @sdxCarManufacturerCountry3);
  AProduct.Add('sdxCarManufacturerCountry4', @sdxCarManufacturerCountry4);
  AProduct.Add('sdxCarModel1', @sdxCarModel1);
  AProduct.Add('sdxCarModel2', @sdxCarModel2);
  AProduct.Add('sdxCarModel3', @sdxCarModel3);
  AProduct.Add('sdxCarModel4', @sdxCarModel4);
  AProduct.Add('sdxTrue', @sdxTrue);
  AProduct.Add('sdxFalse', @sdxFalse);
  AProduct.Add('sdxAuto', @sdxAuto);
  AProduct.Add('sdxCustom', @sdxCustom);
  AProduct.Add('sdxEnv', @sdxEnv);
  AProduct.Add('sdxLookAndFeelFlat', @sdxLookAndFeelFlat);
  AProduct.Add('sdxLookAndFeelStandard', @sdxLookAndFeelStandard);
  AProduct.Add('sdxLookAndFeelUltraFlat', @sdxLookAndFeelUltraFlat);
  AProduct.Add('sdxViewTab', @sdxViewTab);
  AProduct.Add('sdxBehaviorsTab', @sdxBehaviorsTab);
  AProduct.Add('sdxPreviewTab', @sdxPreviewTab);
  AProduct.Add('sdxCardsTab', @sdxCardsTab);
  AProduct.Add('sdxFormatting', @sdxFormatting);
  AProduct.Add('sdxLookAndFeel', @sdxLookAndFeel);
  AProduct.Add('sdxLevelCaption', @sdxLevelCaption);
  AProduct.Add('sdxFilterBar', @sdxFilterBar);
  AProduct.Add('sdxRefinements', @sdxRefinements);
  AProduct.Add('sdxProcessSelection', @sdxProcessSelection);
  AProduct.Add('sdxProcessExactSelection', @sdxProcessExactSelection);
  AProduct.Add('sdxExpanding', @sdxExpanding);
  AProduct.Add('sdxGroups', @sdxGroups);
  AProduct.Add('sdxDetails', @sdxDetails);
  AProduct.Add('sdxStartFromActiveDetails', @sdxStartFromActiveDetails);
  AProduct.Add('sdxOnlyActiveDetails', @sdxOnlyActiveDetails);
  AProduct.Add('sdxVisible', @sdxVisible);
  AProduct.Add('sdxPreviewAutoHeight', @sdxPreviewAutoHeight);
  AProduct.Add('sdxPreviewMaxLineCount', @sdxPreviewMaxLineCount);
  AProduct.Add('sdxSizes', @sdxSizes);
  AProduct.Add('sdxKeepSameWidth', @sdxKeepSameWidth);
  AProduct.Add('sdxKeepSameHeight', @sdxKeepSameHeight);
  AProduct.Add('sdxFraming', @sdxFraming);
  AProduct.Add('sdxSpacing', @sdxSpacing);
  AProduct.Add('sdxShadow', @sdxShadow);
  AProduct.Add('sdxDepth', @sdxDepth);
  AProduct.Add('sdxPosition', @sdxPosition);
  AProduct.Add('sdxPositioning', @sdxPositioning);
  AProduct.Add('sdxHorizontal', @sdxHorizontal);
  AProduct.Add('sdxVertical', @sdxVertical);
  AProduct.Add('sdxSummaryFormat', @sdxSummaryFormat);
  AProduct.Add('sdxCannotUseOnEveryPageMode', @sdxCannotUseOnEveryPageMode);
  AProduct.Add('sdxIncorrectBandHeadersState', @sdxIncorrectBandHeadersState);
  AProduct.Add('sdxIncorrectHeadersState', @sdxIncorrectHeadersState);
  AProduct.Add('sdxIncorrectFootersState', @sdxIncorrectFootersState);
  AProduct.Add('sdxCharts', @sdxCharts);
  AProduct.Add('sdxTPicture', @sdxTPicture);
  AProduct.Add('sdxCopy', @sdxCopy);
  AProduct.Add('sdxSave', @sdxSave);
  AProduct.Add('sdxBaseStyle', @sdxBaseStyle);
  AProduct.Add('sdxCarSpeedCountColumnCaption', @sdxCarSpeedCountColumnCaption);
end;

procedure AddResourceStringsPart3(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxComponentAlreadyExists', @sdxComponentAlreadyExists);
  AProduct.Add('sdxInvalidComponentName', @sdxInvalidComponentName);
  AProduct.Add('sdxRectangle', @sdxRectangle);
  AProduct.Add('sdxSquare', @sdxSquare);
  AProduct.Add('sdxEllipse', @sdxEllipse);
  AProduct.Add('sdxCircle', @sdxCircle);
  AProduct.Add('sdxRoundRect', @sdxRoundRect);
  AProduct.Add('sdxRoundSquare', @sdxRoundSquare);
  AProduct.Add('sdxHorizontalFillPattern', @sdxHorizontalFillPattern);
  AProduct.Add('sdxVerticalFillPattern', @sdxVerticalFillPattern);
  AProduct.Add('sdxFDiagonalFillPattern', @sdxFDiagonalFillPattern);
  AProduct.Add('sdxBDiagonalFillPattern', @sdxBDiagonalFillPattern);
  AProduct.Add('sdxCrossFillPattern', @sdxCrossFillPattern);
  AProduct.Add('sdxDiagCrossFillPattern', @sdxDiagCrossFillPattern);
  AProduct.Add('sdxCyclicIDReferences', @sdxCyclicIDReferences);
  AProduct.Add('sdxLoadReportDataToFileTitle', @sdxLoadReportDataToFileTitle);
  AProduct.Add('sdxSaveReportDataToFileTitle', @sdxSaveReportDataToFileTitle);
  AProduct.Add('sdxInvalidExternalStorage', @sdxInvalidExternalStorage);
  AProduct.Add('sdxLinkIsNotIncludedInUsesClause', @sdxLinkIsNotIncludedInUsesClause);
  AProduct.Add('sdxInvalidStorageVersion', @sdxInvalidStorageVersion);
  AProduct.Add('sdxPSReportFiles', @sdxPSReportFiles);
  AProduct.Add('sdxReportFileLoadError', @sdxReportFileLoadError);
  AProduct.Add('sdxNone', @sdxNone);
  AProduct.Add('sdxReportDocumentIsCorrupted', @sdxReportDocumentIsCorrupted);
  AProduct.Add('sdxCloseExplorerHint', @sdxCloseExplorerHint);
  AProduct.Add('sdxExplorerCaption', @sdxExplorerCaption);
  AProduct.Add('sdxExplorerRootFolderCaption', @sdxExplorerRootFolderCaption);
  AProduct.Add('sdxNewExplorerFolderItem', @sdxNewExplorerFolderItem);
  AProduct.Add('sdxCopyOfItem', @sdxCopyOfItem);
  AProduct.Add('sdxReportExplorer', @sdxReportExplorer);
  AProduct.Add('sdxDataLoadErrorText', @sdxDataLoadErrorText);
  AProduct.Add('sdxDBBasedExplorerItemDataLoadError', @sdxDBBasedExplorerItemDataLoadError);
  AProduct.Add('sdxFileBasedExplorerItemDataLoadError', @sdxFileBasedExplorerItemDataLoadError);
  AProduct.Add('sdxDeleteNonEmptyFolderMessageText', @sdxDeleteNonEmptyFolderMessageText);
  AProduct.Add('sdxDeleteFolderMessageText', @sdxDeleteFolderMessageText);
  AProduct.Add('sdxDeleteItemMessageText', @sdxDeleteItemMessageText);
  AProduct.Add('sdxCannotRenameFolderText', @sdxCannotRenameFolderText);
  AProduct.Add('sdxCannotRenameItemText', @sdxCannotRenameItemText);
  AProduct.Add('sdxOverwriteFolderMessageText', @sdxOverwriteFolderMessageText);
  AProduct.Add('sdxOverwriteItemMessageText', @sdxOverwriteItemMessageText);
  AProduct.Add('sdxSelectNewRoot', @sdxSelectNewRoot);
  AProduct.Add('sdxInvalidFolderName', @sdxInvalidFolderName);
  AProduct.Add('sdxInvalidReportName', @sdxInvalidReportName);
  AProduct.Add('sdxExplorerBar', @sdxExplorerBar);
  AProduct.Add('sdxMenuFileSave', @sdxMenuFileSave);
  AProduct.Add('sdxMenuFileSaveAs', @sdxMenuFileSaveAs);
  AProduct.Add('sdxMenuFileLoad', @sdxMenuFileLoad);
  AProduct.Add('sdxMenuFileClose', @sdxMenuFileClose);
  AProduct.Add('sdxHintFileSave', @sdxHintFileSave);
  AProduct.Add('sdxHintFileSaveAs', @sdxHintFileSaveAs);
  AProduct.Add('sdxHintFileLoad', @sdxHintFileLoad);
  AProduct.Add('sdxHintFileClose', @sdxHintFileClose);
  AProduct.Add('sdxMenuExplorer', @sdxMenuExplorer);
  AProduct.Add('sdxMenuExplorerCreateFolder', @sdxMenuExplorerCreateFolder);
  AProduct.Add('sdxMenuExplorerDelete', @sdxMenuExplorerDelete);
  AProduct.Add('sdxMenuExplorerRename', @sdxMenuExplorerRename);
  AProduct.Add('sdxMenuExplorerProperties', @sdxMenuExplorerProperties);
  AProduct.Add('sdxMenuExplorerRefresh', @sdxMenuExplorerRefresh);
  AProduct.Add('sdxMenuExplorerChangeRootPath', @sdxMenuExplorerChangeRootPath);
  AProduct.Add('sdxMenuExplorerSetAsRoot', @sdxMenuExplorerSetAsRoot);
  AProduct.Add('sdxMenuExplorerGoToUpOneLevel', @sdxMenuExplorerGoToUpOneLevel);
  AProduct.Add('sdxHintExplorerCreateFolder', @sdxHintExplorerCreateFolder);
  AProduct.Add('sdxHintExplorerDelete', @sdxHintExplorerDelete);
  AProduct.Add('sdxHintExplorerRename', @sdxHintExplorerRename);
  AProduct.Add('sdxHintExplorerProperties', @sdxHintExplorerProperties);
  AProduct.Add('sdxHintExplorerRefresh', @sdxHintExplorerRefresh);
  AProduct.Add('sdxHintExplorerChangeRootPath', @sdxHintExplorerChangeRootPath);
  AProduct.Add('sdxHintExplorerSetAsRoot', @sdxHintExplorerSetAsRoot);
  AProduct.Add('sdxHintExplorerGoToUpOneLevel', @sdxHintExplorerGoToUpOneLevel);
  AProduct.Add('sdxMenuViewExplorer', @sdxMenuViewExplorer);
  AProduct.Add('sdxHintViewExplorer', @sdxHintViewExplorer);
  AProduct.Add('sdxSummary', @sdxSummary);
  AProduct.Add('sdxCreator', @sdxCreator);
  AProduct.Add('sdxCreationDate', @sdxCreationDate);
  AProduct.Add('sdxMenuViewThumbnails', @sdxMenuViewThumbnails);
  AProduct.Add('sdxMenuThumbnailsLarge', @sdxMenuThumbnailsLarge);
  AProduct.Add('sdxMenuThumbnailsSmall', @sdxMenuThumbnailsSmall);
  AProduct.Add('sdxHintViewThumbnails', @sdxHintViewThumbnails);
  AProduct.Add('sdxHintThumbnailsLarge', @sdxHintThumbnailsLarge);
  AProduct.Add('sdxHintThumbnailsSmall', @sdxHintThumbnailsSmall);
  AProduct.Add('sdxMenuFormatTitle', @sdxMenuFormatTitle);
  AProduct.Add('sdxHintFormatTitle', @sdxHintFormatTitle);
  AProduct.Add('sdxMenuFormatFootnotes', @sdxMenuFormatFootnotes);
  AProduct.Add('sdxHintFormatFootnotes', @sdxHintFormatFootnotes);
  AProduct.Add('sdxHalf', @sdxHalf);
  AProduct.Add('sdxPredefinedFunctions', @sdxPredefinedFunctions);
  AProduct.Add('sdxZoomParameters', @sdxZoomParameters);
  AProduct.Add('sdxWrapData', @sdxWrapData);
  AProduct.Add('sdxMenuShortcutExplorer', @sdxMenuShortcutExplorer);
  AProduct.Add('sdxExplorerToolBar', @sdxExplorerToolBar);
  AProduct.Add('sdxMenuShortcutThumbnails', @sdxMenuShortcutThumbnails);
  AProduct.Add('sdxButtons', @sdxButtons);
  AProduct.Add('sdxBtnHeadersFont', @sdxBtnHeadersFont);
  AProduct.Add('sdxHeadersTransparent', @sdxHeadersTransparent);
  AProduct.Add('sdxHintListViewDesignerMessage', @sdxHintListViewDesignerMessage);
  AProduct.Add('sdxColumnHeaders', @sdxColumnHeaders);
  AProduct.Add('sdxReportGroupNullLookAndFeel', @sdxReportGroupNullLookAndFeel);
  AProduct.Add('sdxReportGroupStandardLookAndFeel', @sdxReportGroupStandardLookAndFeel);
  AProduct.Add('sdxReportGroupOfficeLookAndFeel', @sdxReportGroupOfficeLookAndFeel);
  AProduct.Add('sdxReportGroupWebLookAndFeel', @sdxReportGroupWebLookAndFeel);
  AProduct.Add('sdxLayoutGroupDefaultCaption', @sdxLayoutGroupDefaultCaption);
  AProduct.Add('sdxLayoutItemDefaultCaption', @sdxLayoutItemDefaultCaption);
  AProduct.Add('sdxTabs', @sdxTabs);
  AProduct.Add('sdxUnwrapTabs', @sdxUnwrapTabs);
  AProduct.Add('sdxActiveTabToTop', @sdxActiveTabToTop);
  AProduct.Add('sdxBehaviorsGroups', @sdxBehaviorsGroups);
  AProduct.Add('sdxSkipEmptyGroups', @sdxSkipEmptyGroups);
  AProduct.Add('sdxExpandedGroups', @sdxExpandedGroups);
  AProduct.Add('sdxCarManufacturerName5', @sdxCarManufacturerName5);
  AProduct.Add('sdxCarManufacturerCountry5', @sdxCarManufacturerCountry5);
  AProduct.Add('sdxCarModel5', @sdxCarModel5);
  AProduct.Add('sdxLuxurySedans', @sdxLuxurySedans);
  AProduct.Add('sdxCarManufacturer', @sdxCarManufacturer);
  AProduct.Add('sdxCarModel', @sdxCarModel);
  AProduct.Add('sdxCarEngine', @sdxCarEngine);
  AProduct.Add('sdxCarTransmission', @sdxCarTransmission);
  AProduct.Add('sdxCarTires', @sdxCarTires);
  AProduct.Add('sdx760V12Manufacturer', @sdx760V12Manufacturer);
  AProduct.Add('sdx760V12Model', @sdx760V12Model);
  AProduct.Add('sdx760V12Engine', @sdx760V12Engine);
  AProduct.Add('sdx760V12Transmission', @sdx760V12Transmission);
  AProduct.Add('sdx760V12Tires', @sdx760V12Tires);
  AProduct.Add('sdxBandBackgroundStyle', @sdxBandBackgroundStyle);
  AProduct.Add('sdxBandHeaderStyle', @sdxBandHeaderStyle);
  AProduct.Add('sdxCaptionStyle', @sdxCaptionStyle);
  AProduct.Add('sdxCardCaptionRowStyle', @sdxCardCaptionRowStyle);
  AProduct.Add('sdxCardRowCaptionStyle', @sdxCardRowCaptionStyle);
  AProduct.Add('sdxCategoryStyle', @sdxCategoryStyle);
  AProduct.Add('sdxContentStyle', @sdxContentStyle);
  AProduct.Add('sdxContentEvenStyle', @sdxContentEvenStyle);
  AProduct.Add('sdxContentOddStyle', @sdxContentOddStyle);
  AProduct.Add('sdxFilterBarStyle', @sdxFilterBarStyle);
  AProduct.Add('sdxFooterStyle', @sdxFooterStyle);
  AProduct.Add('sdxFooterRowStyle', @sdxFooterRowStyle);
  AProduct.Add('sdxGroupStyle', @sdxGroupStyle);
  AProduct.Add('sdxHeaderStyle', @sdxHeaderStyle);
  AProduct.Add('sdxIndentStyle', @sdxIndentStyle);
  AProduct.Add('sdxPreviewStyle', @sdxPreviewStyle);
  AProduct.Add('sdxSelectionStyle', @sdxSelectionStyle);
  AProduct.Add('sdxStyles', @sdxStyles);
  AProduct.Add('sdxStyleSheets', @sdxStyleSheets);
  AProduct.Add('sdxBtnTexture', @sdxBtnTexture);
  AProduct.Add('sdxBtnTextureClear', @sdxBtnTextureClear);
  AProduct.Add('sdxBtnColor', @sdxBtnColor);
  AProduct.Add('sdxBtnSaveAs', @sdxBtnSaveAs);
  AProduct.Add('sdxBtnRename', @sdxBtnRename);
  AProduct.Add('sdxLoadBitmapDlgTitle', @sdxLoadBitmapDlgTitle);
  AProduct.Add('sdxDeleteStyleSheet', @sdxDeleteStyleSheet);
  AProduct.Add('sdxUnnamedStyleSheet', @sdxUnnamedStyleSheet);
  AProduct.Add('sdxCreateNewStyleQueryNamePrompt', @sdxCreateNewStyleQueryNamePrompt);
  AProduct.Add('sdxStyleSheetNameAlreadyExists', @sdxStyleSheetNameAlreadyExists);
  AProduct.Add('sdxCannotLoadImage', @sdxCannotLoadImage);
  AProduct.Add('sdxUseNativeStyles', @sdxUseNativeStyles);
  AProduct.Add('sdxSuppressBackgroundBitmaps', @sdxSuppressBackgroundBitmaps);
  AProduct.Add('sdxConsumeSelectionStyle', @sdxConsumeSelectionStyle);
  AProduct.Add('sdxSize', @sdxSize);
  AProduct.Add('sdxLevels', @sdxLevels);
  AProduct.Add('sdxUnwrap', @sdxUnwrap);
  AProduct.Add('sdxUnwrapTopLevel', @sdxUnwrapTopLevel);
  AProduct.Add('sdxRiseActiveToTop', @sdxRiseActiveToTop);
  AProduct.Add('sdxCannotUseOnEveryPageModeInAggregatedState', @sdxCannotUseOnEveryPageModeInAggregatedState);
  AProduct.Add('sdxPagination', @sdxPagination);
  AProduct.Add('sdxByBands', @sdxByBands);
  AProduct.Add('sdxByColumns', @sdxByColumns);
  AProduct.Add('sdxByRows', @sdxByRows);
  AProduct.Add('sdxByTopLevelGroups', @sdxByTopLevelGroups);
  AProduct.Add('sdxOneGroupPerPage', @sdxOneGroupPerPage);
  AProduct.Add('sdxSkipEmptyViews', @sdxSkipEmptyViews);
  AProduct.Add('sdxBorders', @sdxBorders);
  AProduct.Add('sdxExplicitlyExpandNodes', @sdxExplicitlyExpandNodes);
  AProduct.Add('sdxNodes', @sdxNodes);
  AProduct.Add('sdxSeparators', @sdxSeparators);
  AProduct.Add('sdxThickness', @sdxThickness);
  AProduct.Add('sdxTLIncorrectHeadersState', @sdxTLIncorrectHeadersState);
  AProduct.Add('sdxRows', @sdxRows);
  AProduct.Add('sdxMultipleRecords', @sdxMultipleRecords);
  AProduct.Add('sdxBestFit', @sdxBestFit);
  AProduct.Add('sdxKeepSameRecordWidths', @sdxKeepSameRecordWidths);
  AProduct.Add('sdxWrapRecords', @sdxWrapRecords);
  AProduct.Add('sdxByWrapping', @sdxByWrapping);
  AProduct.Add('sdxOneWrappingPerPage', @sdxOneWrappingPerPage);
  AProduct.Add('sdxCurrentRecord', @sdxCurrentRecord);
  AProduct.Add('sdxLoadedRecords', @sdxLoadedRecords);
  AProduct.Add('sdxAllRecords', @sdxAllRecords);
  AProduct.Add('sdxPaginateByControlDetails', @sdxPaginateByControlDetails);
  AProduct.Add('sdxPaginateByControls', @sdxPaginateByControls);
  AProduct.Add('sdxPaginateByGroups', @sdxPaginateByGroups);
  AProduct.Add('sdxPaginateByItems', @sdxPaginateByItems);
  AProduct.Add('sdxControlsPlace', @sdxControlsPlace);
  AProduct.Add('sdxExpandHeight', @sdxExpandHeight);
  AProduct.Add('sdxExpandWidth', @sdxExpandWidth);
  AProduct.Add('sdxShrinkHeight', @sdxShrinkHeight);
  AProduct.Add('sdxShrinkWidth', @sdxShrinkWidth);
  AProduct.Add('sdxCheckAll', @sdxCheckAll);
  AProduct.Add('sdxCheckAllChildren', @sdxCheckAllChildren);
  AProduct.Add('sdxControlsTab', @sdxControlsTab);
  AProduct.Add('sdxExpandAll', @sdxExpandAll);
  AProduct.Add('sdxHiddenControlsTab', @sdxHiddenControlsTab);
  AProduct.Add('sdxReportLinksTab', @sdxReportLinksTab);
  AProduct.Add('sdxAvailableLinks', @sdxAvailableLinks);
  AProduct.Add('sdxAggregatedLinks', @sdxAggregatedLinks);
  AProduct.Add('sdxTransparents', @sdxTransparents);
  AProduct.Add('sdxUncheckAllChildren', @sdxUncheckAllChildren);
  AProduct.Add('sdxRoot', @sdxRoot);
  AProduct.Add('sdxRootBorders', @sdxRootBorders);
  AProduct.Add('sdxControls', @sdxControls);
  AProduct.Add('sdxContainers', @sdxContainers);
  AProduct.Add('sdxHideCustomContainers', @sdxHideCustomContainers);
  AProduct.Add('sdxBytes', @sdxBytes);
  AProduct.Add('sdxKiloBytes', @sdxKiloBytes);
  AProduct.Add('sdxMegaBytes', @sdxMegaBytes);
  AProduct.Add('sdxGigaBytes', @sdxGigaBytes);
  AProduct.Add('sdxThereIsNoPictureToDisplay', @sdxThereIsNoPictureToDisplay);
  AProduct.Add('sdxInvalidRootDirectory', @sdxInvalidRootDirectory);
  AProduct.Add('sdxPressEscToCancel', @sdxPressEscToCancel);
  AProduct.Add('sdxMenuFileRebuild', @sdxMenuFileRebuild);
  AProduct.Add('sdxBuildingReportStatusText', @sdxBuildingReportStatusText);
  AProduct.Add('sdxPrintingReportStatusText', @sdxPrintingReportStatusText);
  AProduct.Add('sdxBuiltIn', @sdxBuiltIn);
  AProduct.Add('sdxUserDefined', @sdxUserDefined);
  AProduct.Add('sdxNewStyleRepositoryWasCreated', @sdxNewStyleRepositoryWasCreated);
  AProduct.Add('sdxLineSpacing', @sdxLineSpacing);
  AProduct.Add('sdxTextAlignJustified', @sdxTextAlignJustified);
  AProduct.Add('sdxSampleText', @sdxSampleText);
  AProduct.Add('sdxCardsRows', @sdxCardsRows);
  AProduct.Add('sdxTransparentRichEdits', @sdxTransparentRichEdits);
  AProduct.Add('sdxIncorrectFilterBarState', @sdxIncorrectFilterBarState);
  AProduct.Add('sdxIncorrectBandHeadersState2', @sdxIncorrectBandHeadersState2);
  AProduct.Add('sdxIncorrectHeadersState2', @sdxIncorrectHeadersState2);
  AProduct.Add('sdxAvailableReportLinks', @sdxAvailableReportLinks);
  AProduct.Add('sdxBtnRemoveInconsistents', @sdxBtnRemoveInconsistents);
  AProduct.Add('sdxRowHeadersOnEveryPage', @sdxRowHeadersOnEveryPage);
  AProduct.Add('sdxColumnHeadersOnEveryPage', @sdxColumnHeadersOnEveryPage);
  AProduct.Add('sdxNotes', @sdxNotes);
  AProduct.Add('sdxTaskPad', @sdxTaskPad);
  AProduct.Add('sdxPrimaryTimeZone', @sdxPrimaryTimeZone);
  AProduct.Add('sdxSecondaryTimeZone', @sdxSecondaryTimeZone);
  AProduct.Add('sdxDay', @sdxDay);
  AProduct.Add('sdxWeek', @sdxWeek);
  AProduct.Add('sdxMonth', @sdxMonth);
  AProduct.Add('sdxSchedulerSchedulerHeader', @sdxSchedulerSchedulerHeader);
  AProduct.Add('sdxSchedulerContent', @sdxSchedulerContent);
  AProduct.Add('sdxSchedulerDateNavigatorContent', @sdxSchedulerDateNavigatorContent);
  AProduct.Add('sdxSchedulerDateNavigatorHeader', @sdxSchedulerDateNavigatorHeader);
  AProduct.Add('sdxSchedulerDayHeader', @sdxSchedulerDayHeader);
  AProduct.Add('sdxSchedulerEvent', @sdxSchedulerEvent);
  AProduct.Add('sdxSchedulerResourceHeader', @sdxSchedulerResourceHeader);
  AProduct.Add('sdxSchedulerNotesAreaBlank', @sdxSchedulerNotesAreaBlank);
  AProduct.Add('sdxSchedulerNotesAreaLined', @sdxSchedulerNotesAreaLined);
  AProduct.Add('sdxSchedulerTaskPad', @sdxSchedulerTaskPad);
  AProduct.Add('sdxSchedulerTimeRuler', @sdxSchedulerTimeRuler);
  AProduct.Add('sdxPrintStyleNameDaily', @sdxPrintStyleNameDaily);
  AProduct.Add('sdxPrintStyleNameWeekly', @sdxPrintStyleNameWeekly);
  AProduct.Add('sdxPrintStyleNameMonthly', @sdxPrintStyleNameMonthly);
  AProduct.Add('sdxPrintStyleNameDetails', @sdxPrintStyleNameDetails);
  AProduct.Add('sdxPrintStyleNameMemo', @sdxPrintStyleNameMemo);
  AProduct.Add('sdxPrintStyleNameTrifold', @sdxPrintStyleNameTrifold);
  AProduct.Add('sdxPrintStyleCaptionAgenda', @sdxPrintStyleCaptionAgenda);
  AProduct.Add('sdxPrintStyleCaptionDaily', @sdxPrintStyleCaptionDaily);
  AProduct.Add('sdxPrintStyleCaptionWeekly', @sdxPrintStyleCaptionWeekly);
  AProduct.Add('sdxPrintStyleCaptionMonthly', @sdxPrintStyleCaptionMonthly);
  AProduct.Add('sdxPrintStyleCaptionDetails', @sdxPrintStyleCaptionDetails);
  AProduct.Add('sdxPrintStyleCaptionMemo', @sdxPrintStyleCaptionMemo);
  AProduct.Add('sdxPrintStyleCaptionTimeLine', @sdxPrintStyleCaptionTimeLine);
  AProduct.Add('sdxPrintStyleCaptionTrifold', @sdxPrintStyleCaptionTrifold);
  AProduct.Add('sdxPrintStyleCaptionYearly', @sdxPrintStyleCaptionYearly);
  AProduct.Add('sdxPrintStyleShowEventImages', @sdxPrintStyleShowEventImages);
  AProduct.Add('sdxPrintStyleShowResourceImages', @sdxPrintStyleShowResourceImages);
  AProduct.Add('sdxTabPrintStyles', @sdxTabPrintStyles);
  AProduct.Add('sdxPrintStyleDontPrintWeekEnds', @sdxPrintStyleDontPrintWeekEnds);
  AProduct.Add('sdxPrintStyleWorkTimeOnly', @sdxPrintStyleWorkTimeOnly);
  AProduct.Add('sdxPrintStyleInclude', @sdxPrintStyleInclude);
  AProduct.Add('sdxPrintStyleIncludeTaskPad', @sdxPrintStyleIncludeTaskPad);
  AProduct.Add('sdxPrintStyleIncludeNotesAreaBlank', @sdxPrintStyleIncludeNotesAreaBlank);
  AProduct.Add('sdxPrintStyleIncludeNotesAreaLined', @sdxPrintStyleIncludeNotesAreaLined);
  AProduct.Add('sdxPrintStyleLayout', @sdxPrintStyleLayout);
  AProduct.Add('sdxPrintStylePrintFrom', @sdxPrintStylePrintFrom);
  AProduct.Add('sdxPrintStylePrintTo', @sdxPrintStylePrintTo);
  AProduct.Add('sdxPrintStyleDailyLayout1PPD', @sdxPrintStyleDailyLayout1PPD);
  AProduct.Add('sdxPrintStyleDailyLayout2PPD', @sdxPrintStyleDailyLayout2PPD);
  AProduct.Add('sdxPrintStyleWeeklyArrange', @sdxPrintStyleWeeklyArrange);
  AProduct.Add('sdxPrintStyleWeeklyArrangeT2B', @sdxPrintStyleWeeklyArrangeT2B);
  AProduct.Add('sdxPrintStyleWeeklyArrangeL2R', @sdxPrintStyleWeeklyArrangeL2R);
  AProduct.Add('sdxPrintStyleWeeklyLayout1PPW', @sdxPrintStyleWeeklyLayout1PPW);
  AProduct.Add('sdxPrintStyleWeeklyLayout2PPW', @sdxPrintStyleWeeklyLayout2PPW);
  AProduct.Add('sdxPrintStyleWeeklyDaysLayout', @sdxPrintStyleWeeklyDaysLayout);
  AProduct.Add('sdxPrintStyleWeeklyDaysLayoutTC', @sdxPrintStyleWeeklyDaysLayoutTC);
  AProduct.Add('sdxPrintStyleWeeklyDaysLayoutOC', @sdxPrintStyleWeeklyDaysLayoutOC);
  AProduct.Add('sdxPrintStyleMemoStartEachItemOnNewPage', @sdxPrintStyleMemoStartEachItemOnNewPage);
  AProduct.Add('sdxPrintStyleMemoPrintOnlySelectedEvents', @sdxPrintStyleMemoPrintOnlySelectedEvents);
  AProduct.Add('sdxPrintStyleMonthlyLayout1PPM', @sdxPrintStyleMonthlyLayout1PPM);
  AProduct.Add('sdxPrintStyleMonthlyLayout2PPM', @sdxPrintStyleMonthlyLayout2PPM);
  AProduct.Add('sdxPrintStyleMonthlyPrintExactly1MPP', @sdxPrintStyleMonthlyPrintExactly1MPP);
  AProduct.Add('sdxPrintStyleTrifoldSectionModeDailyCalendar', @sdxPrintStyleTrifoldSectionModeDailyCalendar);
  AProduct.Add('sdxPrintStyleTrifoldSectionModeWeeklyCalendar', @sdxPrintStyleTrifoldSectionModeWeeklyCalendar);
  AProduct.Add('sdxPrintStyleTrifoldSectionModeMonthlyCalendar', @sdxPrintStyleTrifoldSectionModeMonthlyCalendar);
  AProduct.Add('sdxPrintStyleTrifoldSectionModeTaskPad', @sdxPrintStyleTrifoldSectionModeTaskPad);
  AProduct.Add('sdxPrintStyleTrifoldSectionModeNotesBlank', @sdxPrintStyleTrifoldSectionModeNotesBlank);
  AProduct.Add('sdxPrintStyleTrifoldSectionModeNotesLined', @sdxPrintStyleTrifoldSectionModeNotesLined);
  AProduct.Add('sdxPrintStyleTrifoldSectionLeft', @sdxPrintStyleTrifoldSectionLeft);
  AProduct.Add('sdxPrintStyleTrifoldSectionMiddle', @sdxPrintStyleTrifoldSectionMiddle);
  AProduct.Add('sdxPrintStyleTrifoldSectionRight', @sdxPrintStyleTrifoldSectionRight);
  AProduct.Add('sdxPrintStyleMonthPerPage', @sdxPrintStyleMonthPerPage);
  AProduct.Add('sdxPrintStyleYearly1MPP', @sdxPrintStyleYearly1MPP);
  AProduct.Add('sdxPrintStyleYearly2MPP', @sdxPrintStyleYearly2MPP);
  AProduct.Add('sdxPrintStyleYearly3MPP', @sdxPrintStyleYearly3MPP);
  AProduct.Add('sdxPrintStyleYearly4MPP', @sdxPrintStyleYearly4MPP);
  AProduct.Add('sdxPrintStyleYearly6MPP', @sdxPrintStyleYearly6MPP);
  AProduct.Add('sdxPrintStyleYearly12MPP', @sdxPrintStyleYearly12MPP);
  AProduct.Add('sdxPrintStylePrimaryPageScalesOnly', @sdxPrintStylePrimaryPageScalesOnly);
  AProduct.Add('sdxPrintStylePrimaryPageHeadersOnly', @sdxPrintStylePrimaryPageHeadersOnly);
  AProduct.Add('sdxPrintStyleDetailsStartNewPageEach', @sdxPrintStyleDetailsStartNewPageEach);
  AProduct.Add('sdxSuppressContentColoration', @sdxSuppressContentColoration);
  AProduct.Add('sdxOneResourcePerPage', @sdxOneResourcePerPage);
  AProduct.Add('sdxPrintRanges', @sdxPrintRanges);
  AProduct.Add('sdxPrintRangeStart', @sdxPrintRangeStart);
  AProduct.Add('sdxPrintRangeEnd', @sdxPrintRangeEnd);
  AProduct.Add('sdxHideDetailsOfPrivateAppointments', @sdxHideDetailsOfPrivateAppointments);
  AProduct.Add('sdxResourceCountPerPage', @sdxResourceCountPerPage);
  AProduct.Add('sdxSubjectLabelCaption', @sdxSubjectLabelCaption);
  AProduct.Add('sdxLocationLabelCaption', @sdxLocationLabelCaption);
  AProduct.Add('sdxStartLabelCaption', @sdxStartLabelCaption);
  AProduct.Add('sdxFinishLabelCaption', @sdxFinishLabelCaption);
  AProduct.Add('sdxShowTimeAsLabelCaption', @sdxShowTimeAsLabelCaption);
  AProduct.Add('sdxRecurrenceLabelCaption', @sdxRecurrenceLabelCaption);
  AProduct.Add('sdxRecurrencePatternLabelCaption', @sdxRecurrencePatternLabelCaption);
  AProduct.Add('sdxSeeAboveMessage', @sdxSeeAboveMessage);
  AProduct.Add('sdxAllDayMessage', @sdxAllDayMessage);
  AProduct.Add('sdxContinuedMessage', @sdxContinuedMessage);
  AProduct.Add('sdxShowTimeAsFreeMessage', @sdxShowTimeAsFreeMessage);
  AProduct.Add('sdxShowTimeAsTentativeMessage', @sdxShowTimeAsTentativeMessage);
  AProduct.Add('sdxShowTimeAsOutOfOfficeMessage', @sdxShowTimeAsOutOfOfficeMessage);
  AProduct.Add('sdxRecurrenceNoneMessage', @sdxRecurrenceNoneMessage);
  AProduct.Add('scxRecurrenceDailyMessage', @scxRecurrenceDailyMessage);
  AProduct.Add('scxRecurrenceWeeklyMessage', @scxRecurrenceWeeklyMessage);
  AProduct.Add('scxRecurrenceMonthlyMessage', @scxRecurrenceMonthlyMessage);
  AProduct.Add('scxRecurrenceYearlyMessage', @scxRecurrenceYearlyMessage);
  AProduct.Add('sdxInconsistentTrifoldStyle', @sdxInconsistentTrifoldStyle);
  AProduct.Add('sdxBadTimePrintRange', @sdxBadTimePrintRange);
  AProduct.Add('sdxBadDatePrintRange', @sdxBadDatePrintRange);
  AProduct.Add('sdxCannotPrintNoSelectedItems', @sdxCannotPrintNoSelectedItems);
  AProduct.Add('sdxCannotPrintNoItemsAvailable', @sdxCannotPrintNoItemsAvailable);
  AProduct.Add('sdxColumnFields', @sdxColumnFields);
  AProduct.Add('sdxDataFields', @sdxDataFields);
  AProduct.Add('sdxFiterFields', @sdxFiterFields);
  AProduct.Add('sdxPrefilter', @sdxPrefilter);
  AProduct.Add('sdxRowFields', @sdxRowFields);
  AProduct.Add('sdxAutoColumnsExpand', @sdxAutoColumnsExpand);
  AProduct.Add('sdxAutoRowsExpand', @sdxAutoRowsExpand);
  AProduct.Add('sdxPivotGridColumnHeader', @sdxPivotGridColumnHeader);
  AProduct.Add('sdxPivotGridContent', @sdxPivotGridContent);
  AProduct.Add('sdxPivotGridFieldHeader', @sdxPivotGridFieldHeader);
  AProduct.Add('sdxPivotGridHeaderBackground', @sdxPivotGridHeaderBackground);
  AProduct.Add('sdxPivotGridRowHeader', @sdxPivotGridRowHeader);
  AProduct.Add('sdxPivotGridPrefilter', @sdxPivotGridPrefilter);
  AProduct.Add('sdxUnitPrice', @sdxUnitPrice);
  AProduct.Add('sdxCarName', @sdxCarName);
  AProduct.Add('sdxQuantity', @sdxQuantity);
  AProduct.Add('sdxPaymentAmount', @sdxPaymentAmount);
  AProduct.Add('sdxPurchaseQuarter', @sdxPurchaseQuarter);
  AProduct.Add('sdxPurchaseMonth', @sdxPurchaseMonth);
  AProduct.Add('sdxPaymentType', @sdxPaymentType);
  AProduct.Add('sdxCompanyName', @sdxCompanyName);
end;

procedure AddResourceStringsForPDFDialog(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxPDFDialogAuthor', @sdxPDFDialogAuthor);
  AProduct.Add('sdxPDFDialogCaption', @sdxPDFDialogCaption);
  AProduct.Add('sdxPDFDialogCompressed', @sdxPDFDialogCompressed);
  AProduct.Add('sdxPDFDialogCreator', @sdxPDFDialogCreator);
  AProduct.Add('sdxPDFDialogDocumentInfoTabSheet', @sdxPDFDialogDocumentInfoTabSheet);
  AProduct.Add('sdxPDFDialogEmbedFonts', @sdxPDFDialogEmbedFonts);
  AProduct.Add('sdxPDFDialogExportSettings', @sdxPDFDialogExportSettings);
  AProduct.Add('sdxPDFDialogExportTabSheet', @sdxPDFDialogExportTabSheet);
  AProduct.Add('sdxPDFDialogKeywords', @sdxPDFDialogKeywords);
  AProduct.Add('sdxPDFDialogMaxCompression', @sdxPDFDialogMaxCompression);
  AProduct.Add('sdxPDFDialogMaxQuality', @sdxPDFDialogMaxQuality);
  AProduct.Add('sdxPDFDialogOpenAfterExport', @sdxPDFDialogOpenAfterExport);
  AProduct.Add('sdxPDFDialogPageRageTabSheet', @sdxPDFDialogPageRageTabSheet);
  AProduct.Add('sdxPDFDialogSecurityAllowChanging', @sdxPDFDialogSecurityAllowChanging);
  AProduct.Add('sdxPDFDialogSecurityAllowComments', @sdxPDFDialogSecurityAllowComments);
  AProduct.Add('sdxPDFDialogSecurityAllowCopy', @sdxPDFDialogSecurityAllowCopy);
  AProduct.Add('sdxPDFDialogSecurityAllowDocumentAssemble', @sdxPDFDialogSecurityAllowDocumentAssemble);
  AProduct.Add('sdxPDFDialogSecurityAllowPrint', @sdxPDFDialogSecurityAllowPrint);
  AProduct.Add('sdxPDFDialogSecurityAllowPrintHiResolution', @sdxPDFDialogSecurityAllowPrintHiResolution);
  AProduct.Add('sdxPDFDialogSecurityEnabled', @sdxPDFDialogSecurityEnabled);
  AProduct.Add('sdxPDFDialogSecurityMethod', @sdxPDFDialogSecurityMethod);
  AProduct.Add('sdxPDFDialogSecurityOwnerPassword', @sdxPDFDialogSecurityOwnerPassword);
  AProduct.Add('sdxPDFDialogSecuritySettings', @sdxPDFDialogSecuritySettings);
  AProduct.Add('sdxPDFDialogSecurityUserPassword', @sdxPDFDialogSecurityUserPassword);
  AProduct.Add('sdxPDFDialogSubject', @sdxPDFDialogSubject);
  AProduct.Add('sdxPDFDialogTabDocInfo', @sdxPDFDialogTabDocInfo);
  AProduct.Add('sdxPDFDialogTabExport', @sdxPDFDialogTabExport);
  AProduct.Add('sdxPDFDialogTabPages', @sdxPDFDialogTabPages);
  AProduct.Add('sdxPDFDialogTabSecurity', @sdxPDFDialogTabSecurity);
  AProduct.Add('sdxPDFDialogTitle', @sdxPDFDialogTitle);
  AProduct.Add('sdxPDFDialogUseCIDFonts', @sdxPDFDialogUseCIDFonts);
  AProduct.Add('sdxPDFDialogUseJPEGCompression', @sdxPDFDialogUseJPEGCompression);
end;

procedure AddResourceStringsForRibbonPreview(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxRibbonPrintPreviewClosePrintPreview', @sdxRibbonPrintPreviewClosePrintPreview);
  AProduct.Add('sdxRibbonPrintPreviewGroupFormat', @sdxRibbonPrintPreviewGroupFormat);
  AProduct.Add('sdxRibbonPrintPreviewGroupInsertName', @sdxRibbonPrintPreviewGroupInsertName);
  AProduct.Add('sdxRibbonPrintPreviewGroupInsertPageNumber', @sdxRibbonPrintPreviewGroupInsertPageNumber);
  AProduct.Add('sdxRibbonPrintPreviewGroupNavigation', @sdxRibbonPrintPreviewGroupNavigation);
  AProduct.Add('sdxRibbonPrintPreviewGroupOutput', @sdxRibbonPrintPreviewGroupOutput);
  AProduct.Add('sdxRibbonPrintPreviewGroupParts', @sdxRibbonPrintPreviewGroupParts);
  AProduct.Add('sdxRibbonPrintPreviewGroupReport', @sdxRibbonPrintPreviewGroupReport);
  AProduct.Add('sdxRibbonPrintPreviewGroupScaleToFit', @sdxRibbonPrintPreviewGroupScaleToFit);
  AProduct.Add('sdxRibbonPrintPreviewGroupZoom', @sdxRibbonPrintPreviewGroupZoom);
  AProduct.Add('sdxRibbonPrintPreviewPagesSubItem', @sdxRibbonPrintPreviewPagesSubItem);
end;

procedure AddExpressPrintingSystemResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  // Split into parts because D12 compiler error
  AddResourceStringsPart1(AProduct);
  AddResourceStringsPart2(AProduct);
  AddResourceStringsPart3(AProduct);
  AddResourceStringsForPDFDialog(AProduct);
  AddResourceStringsForRibbonPreview(AProduct);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressPrinting System', @AddExpressPrintingSystemResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressPrinting System');

end.

