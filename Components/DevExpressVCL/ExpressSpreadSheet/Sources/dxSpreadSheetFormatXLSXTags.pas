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

unit dxSpreadSheetFormatXLSXTags;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Graphics, Classes, cxGeometry, cxGraphics, dxGDIPlusClasses, dxSpreadSheetTypes, dxSpreadSheetClasses, dxCore,
  dxSpreadSheetCore, dxSpreadSheetGraphics, dxSpreadSheetPrinting, dxSpreadSheetConditionalFormattingRules,
  dxSpreadSheetContainers, dxProtectionUtils, dxSpreadSheetCoreStyles;

type
  TdxSpreadSheetXLSXCellType = (sxctUnknown, sxctBoolean, sxctError,
    sxctFloat, sxctFormula, sxctSharedString, sxctString, sxctRichText);

const
  sdxXLSXCommonContentType = AnsiString('application/vnd.openxmlformats-officedocument.spreadsheetml');
  sdxXLSXCommonRelationshipPath = AnsiString('http://schemas.openxmlformats.org/officeDocument/2006/relationships');

  sdxXLSXChartsheetRelationship = sdxXLSXCommonRelationshipPath + AnsiString('/chartsheet');
  sdxXLSXCommentsRelationship = sdxXLSXCommonRelationshipPath + AnsiString('/comments');
  sdxXLSXDrawingRelationship = sdxXLSXCommonRelationshipPath + AnsiString('/drawing');
  sdxXLSXExternalLinkPathRelationship = sdxXLSXCommonRelationshipPath + AnsiString('/externalLinkPath');
  sdxXLSXExternalLinkRelationship = sdxXLSXCommonRelationshipPath + AnsiString('/externalLink');
  sdxXLSXHyperlinkRelationship = sdxXLSXCommonRelationshipPath + AnsiString('/hyperlink');
  sdxXLSXImageRelationship = sdxXLSXCommonRelationshipPath + AnsiString('/image');
  sdxXLSXSharedStringRelationship = sdxXLSXCommonRelationshipPath + AnsiString('/sharedStrings');
  sdxXLSXStyleRelationship = sdxXLSXCommonRelationshipPath + AnsiString('/styles');
  sdxXLSXThemeRelationship = sdxXLSXCommonRelationshipPath + AnsiString('/theme');
  sdxXLSXVMLDrawingRelationship = sdxXLSXCommonRelationshipPath + AnsiString('/vmlDrawing');
  sdxXLSXWorkbookRelationship = sdxXLSXCommonRelationshipPath + AnsiString('/officeDocument');
  sdxXLSXWorksheetRelationship = sdxXLSXCommonRelationshipPath + AnsiString('/worksheet');

  sdxXLSXCommentsContentType = sdxXLSXCommonContentType + AnsiString('.comments+xml');
  sdxXLSXDrawingContentType = AnsiString('application/vnd.openxmlformats-officedocument.drawing+xml');
  sdxXLSXExternalLinkContentType = sdxXLSXCommonContentType + AnsiString('.externalLink+xml');
  sdxXLSXRelsContentType = AnsiString('application/vnd.openxmlformats-package.relationships+xml');
  sdxXLSXSharedStringsContentType = sdxXLSXCommonContentType + AnsiString('.sharedStrings+xml');
  sdxXLSXStylesContentType = sdxXLSXCommonContentType + AnsiString('.styles+xml');
  sdxXLSXWorkbookContentType = sdxXLSXCommonContentType + AnsiString('.sheet.main+xml');
  sdxXLSXWorkbookTemplateContentType = sdxXLSXCommonContentType + AnsiString('.template.main+xml');
  sdxXLSXWorksheetContentType = sdxXLSXCommonContentType + AnsiString('.worksheet+xml');

  sdxXLSXContentTypeNamespace = AnsiString('http://schemas.openxmlformats.org/package/2006/content-types');
  sdxXLSXDrawingNamespace = AnsiString('http://schemas.openxmlformats.org/drawingml/2006/main');
  sdxXLSXDrawingNamespaceXDR = AnsiString('http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing');

  sdxXLSXRelsNamespace = AnsiString('http://schemas.openxmlformats.org/package/2006/relationships');
  sdxXLSXWorkbookNamespace = AnsiString('http://schemas.openxmlformats.org/spreadsheetml/2006/main');

  // FileName templates
  sdxXLSXFileTemplateComments = AnsiString('xl/comments%d.xml');
  sdxXLSXFileTemplateDrawing = AnsiString('xl/drawings/drawing%d.xml');
  sdxXLSXFileTemplateExternalLink = AnsiString('xl/externalLinks/externalLink%d.xml');
  sdxXLSXFileTemplateImage = AnsiString('xl/media/image%d.%s');
  sdxXLSXFileTemplateVMLDrawing = AnsiString('xl/drawings/vmlDrawing%d.vml');
  sdxXLSXFileTemplateWorksheet = AnsiString('xl/worksheets/sheet%d.xml');

  sdxXLSXContentTypeFileName = '[Content_Types].xml';
  sdxXLSXSharedStringsFileName = 'xl/sharedStrings.xml';
  sdxXLSXStylesFileName = 'xl/styles.xml';
  sdxXLSXWorkbookFileName = 'xl/workbook.xml';

const
  // Namespaces
  sdxXLSXNamespaceX14 = AnsiString('x14');

  // Attributes
  sdxXLSXAttrAboveAverage = AnsiString('aboveAverage');
  sdxXLSXAttrActiveCell = AnsiString('activeCell');
  sdxXLSXAttrActivePane = AnsiString('activePane');
  sdxXLSXAttrActiveTab = AnsiString('activeTab');
  sdxXLSXAttrAlgorithmName = AnsiString('algorithmName');
  sdxXLSXAttrAlign = AnsiString('algn');
  sdxXLSXAttrAlternateText = AnsiString('descr');
  sdxXLSXAttrAnchor = AnsiString('anchor');
  sdxXLSXAttrAng = AnsiString('ang');
  sdxXLSXAttrAngle = AnsiString('angle');
  sdxXLSXAttrApplyAlignment = AnsiString('applyAlignment');
  sdxXLSXAttrApplyBorder = AnsiString('applyBorder');
  sdxXLSXAttrApplyFill = AnsiString('applyFill');
  sdxXLSXAttrApplyFont = AnsiString('applyFont');
  sdxXLSXAttrApplyNumberFormat = AnsiString('applyNumberFormat');
  sdxXLSXAttrApplyProtection = AnsiString('applyProtection');
  sdxXLSXAttrAuthorId = AnsiString('authorId');
  sdxXLSXAttrAuto = AnsiString('auto');
  sdxXLSXAttrAutoFilter = AnsiString('autoFilter');
  sdxXLSXAttrAxisPosition = AnsiString('axisPosition');
  sdxXLSXAttrBaseColumnWidth = AnsiString('baseColWidth');
  sdxXLSXAttrBestFit = AnsiString('bestFit');
  sdxXLSXAttrBorder = AnsiString('border');
  sdxXLSXAttrBorderId = AnsiString('borderId');
  sdxXLSXAttrBottom = AnsiString('bottom');
  sdxXLSXAttrBottomInset = AnsiString('bIns');
  sdxXLSXAttrBreakID = AnsiString('id');
  sdxXLSXAttrBreakManual = AnsiString('man');
  sdxXLSXAttrBreaksCount = AnsiString('count');
  sdxXLSXAttrBreaksManualBreakCount = AnsiString('manualBreakCount');
  sdxXLSXAttrCellColumn = AnsiString('r');
  sdxXLSXAttrCellType = AnsiString('t');
  sdxXLSXAttrCollapsed = AnsiString('collapsed');
  sdxXLSXAttrColor2 = AnsiString('color2');
  sdxXLSXAttrContentType = AnsiString('ContentType');
  sdxXLSXAttrCoordExtX = AnsiString('cx');
  sdxXLSXAttrCoordExtY = AnsiString('cy');
  sdxXLSXAttrCoordX = AnsiString('x');
  sdxXLSXAttrCoordY = AnsiString('y');
  sdxXLSXAttrCount = AnsiString('count');
  sdxXLSXAttrCustom = AnsiString('custom');
  sdxXLSXAttrCustomFormat = AnsiString('customFormat');
  sdxXLSXAttrCustomHeight = AnsiString('customHeight');
  sdxXLSXAttrCustomWidth = AnsiString('customWidth');
  sdxXLSXAttrDashStyle = AnsiString('dashstyle');
  sdxXLSXAttrDate1904 = AnsiString('date1904');
  sdxXLSXAttrDefaultColumnWidth = AnsiString('defaultColWidth');
  sdxXLSXAttrDefaultRowHeight = AnsiString('defaultRowHeight');
  sdxXLSXAttrDeleteColumns = AnsiString('deleteColumns');
  sdxXLSXAttrDeleteRows = AnsiString('deleteRows');
  sdxXLSXAttrDfxID = AnsiString('dxfId');
  sdxXLSXAttrDirection = AnsiString('direction');
  sdxXLSXAttrDisplay = AnsiString('display');
  sdxXLSXAttrDrawingResourceEmbed = AnsiString('r:embed');
  sdxXLSXAttrDrawingResourceLink = AnsiString('r:link');
  sdxXLSXAttrEditAs = AnsiString('editAs');
  sdxXLSXAttrEqualAverage = AnsiString('equalAverage');
  sdxXLSXAttrExtension = AnsiString('Extension');
  sdxXLSXAttrFillColor = AnsiString('fillcolor');
  sdxXLSXAttrFillId = AnsiString('fillId');
  sdxXLSXAttrFitToPage = AnsiString('fitToPage');
  sdxXLSXAttrFlip = AnsiString('flip');
  sdxXLSXAttrFlipH = AnsiString('flipH');
  sdxXLSXAttrFlipV = AnsiString('flipV');
  sdxXLSXAttrFocus = AnsiString('focus');
  sdxXLSXAttrFontId = AnsiString('fontId');
  sdxXLSXAttrFormatCells = AnsiString('formatCells');
  sdxXLSXAttrFormatCode = AnsiString('formatCode');
  sdxXLSXAttrFormatColumns = AnsiString('formatColumns');
  sdxXLSXAttrFormatRows = AnsiString('formatRows');
  sdxXLSXAttrGradient = AnsiString('gradient');
  sdxXLSXAttrGradientPointPos = AnsiString('pos');
  sdxXLSXAttrGridLines = 'showGridLines';
  sdxXLSXAttrGTE = AnsiString('gte');
  sdxXLSXAttrHashValue = AnsiString('hashValue');
  sdxXLSXAttrHeaderFooterAlignWithMargins = AnsiString('alignWithMargins');
  sdxXLSXAttrHeaderFooterDifferentFirst = AnsiString('differentFirst');
  sdxXLSXAttrHeaderFooterDifferentOddEven = AnsiString('differentOddEven');
  sdxXLSXAttrHeaderFooterScaleWithDocument = AnsiString('scaleWithDoc');
  sdxXLSXAttrHidden = AnsiString('hidden');
  sdxXLSXAttrHorizontal = AnsiString('horizontal');
  sdxXLSXAttrIconId = AnsiString('iconId');
  sdxXLSXAttrIconSet = AnsiString('iconSet');
  sdxXLSXAttrId = AnsiString('Id');
  sdxXLSXAttrIdLC = AnsiString('id');
  sdxXLSXAttrIndent = 'indent';
  sdxXLSXAttrIndexed = AnsiString('indexed');
  sdxXLSXAttrInsertColumns = AnsiString('insertColumns');
  sdxXLSXAttrInsertHyperlinks = AnsiString('insertHyperlinks');
  sdxXLSXAttrInsertRows = AnsiString('insertRows');
  sdxXLSXAttrInset = AnsiString('inset');
  sdxXLSXAttrIterate = AnsiString('iterate');
  sdxXLSXAttrIterateCount = AnsiString('iterateCount');
  sdxXLSXAttrLastColor = AnsiString('lastClr');
  sdxXLSXAttrLeftInset = AnsiString('lIns');
  sdxXLSXAttrLineWidth = AnsiString('w');
  sdxXLSXAttrLocalSheetId = AnsiString('localSheetId');
  sdxXLSXAttrLocation = AnsiString('location');
  sdxXLSXAttrLocked = AnsiString('locked');
  sdxXLSXAttrLockStructure = AnsiString('lockStructure');
  sdxXLSXAttrMacro = AnsiString('macro');
  sdxXLSXAttrMax = AnsiString('max');
  sdxXLSXAttrMin = AnsiString('min');
  sdxXLSXAttrName = AnsiString('name');
  sdxXLSXAttrNegativeBarBorderColorSameAsPositive = AnsiString('negativeBarBorderColorSameAsPositive');
  sdxXLSXAttrNegativeBarColorSameAsPositive = AnsiString('negativeBarColorSameAsPositive');
  sdxXLSXAttrNumFmtId = AnsiString('numFmtId');
  sdxXLSXAttrObjects = AnsiString('objects');
  sdxXLSXAttrOInsetMode = AnsiString('o:insetmode');
  sdxXLSXAttrOOpacity2 = AnsiString('o:opacity2');
  sdxXLSXAttrOpacity = AnsiString('opacity');
  sdxXLSXAttrOperator = AnsiString('operator');
  sdxXLSXAttrORelID = AnsiString('o:relid');
  sdxXLSXAttrOutlineLevel = AnsiString('outlineLevel');
  sdxXLSXAttrPageMarginsBottom = AnsiString('bottom');
  sdxXLSXAttrPageMarginsFooter = AnsiString('footer');
  sdxXLSXAttrPageMarginsHeader = AnsiString('header');
  sdxXLSXAttrPageMarginsLeft = AnsiString('left');
  sdxXLSXAttrPageMarginsRight = AnsiString('right');
  sdxXLSXAttrPageMarginsTop = AnsiString('top');
  sdxXLSXAttrPageSetupBlackAndWhite = AnsiString('blackAndWhite');
  sdxXLSXAttrPageSetupCellComments = AnsiString('cellComments');
  sdxXLSXAttrPageSetupCopies = AnsiString('copies');
  sdxXLSXAttrPageSetupDraft = AnsiString('draft');
  sdxXLSXAttrPageSetupErrors = AnsiString('errors');
  sdxXLSXAttrPageSetupFirstPageNumber = AnsiString('firstPageNumber');
  sdxXLSXAttrPageSetupFitToHeight = AnsiString('fitToHeight');
  sdxXLSXAttrPageSetupFitToWidth = AnsiString('fitToWidth');
  sdxXLSXAttrPageSetupHorizontalDPI = AnsiString('horizontalDpi');
  sdxXLSXAttrPageSetupOrientation = AnsiString('orientation');
  sdxXLSXAttrPageSetupPageOrder = AnsiString('pageOrder');
  sdxXLSXAttrPageSetupPaperHeight = AnsiString('paperHeight');
  sdxXLSXAttrPageSetupPaperSize = AnsiString('paperSize');
  sdxXLSXAttrPageSetupPaperWidth = AnsiString('paperWidth');
  sdxXLSXAttrPageSetupScale = AnsiString('scale');
  sdxXLSXAttrPageSetupUseFirstPageNumber = AnsiString('useFirstPageNumber');
  sdxXLSXAttrPageSetupVerticalDPI = AnsiString('verticalDpi');
  sdxXLSXAttrPane = AnsiString('pane');
  sdxXLSXAttrPartName = AnsiString('PartName');
  sdxXLSXAttrPassword = AnsiString('password');
  sdxXLSXAttrPatternType = AnsiString('patternType');
  sdxXLSXAttrPercent = AnsiString('percent');
  sdxXLSXAttrPivotTables = 'pivotTables';
  sdxXLSXAttrPosition = AnsiString('position');
  sdxXLSXAttrPreferRelativeResize = AnsiString('preferRelativeResize');
  sdxXLSXAttrPreset = AnsiString('prst');
  sdxXLSXAttrPrintOptionsGridLines = AnsiString('gridLines');
  sdxXLSXAttrPrintOptionsGridLinesSet = AnsiString('gridLinesSet');
  sdxXLSXAttrPrintOptionsHeadings = AnsiString('headings');
  sdxXLSXAttrPrintOptionsHorzCenter = AnsiString('horizontalCentered');
  sdxXLSXAttrPrintOptionsVertCenter = AnsiString('verticalCentered');
  sdxXLSXAttrPriority = AnsiString('priority');
  sdxXLSXAttrRank = AnsiString('rank');
  sdxXLSXAttrRef = AnsiString('ref');
  sdxXLSXAttrRefIndex = AnsiString('idx');
  sdxXLSXAttrRefMode = AnsiString('refMode');
  sdxXLSXAttrReverse = AnsiString('reverse');
  sdxXLSXAttrRGB = AnsiString('rgb');
  sdxXLSXAttrRId = AnsiString('r:id');
  sdxXLSXAttrRightInset = AnsiString('rIns');
  sdxXLSXAttrRot = AnsiString('rot');
  sdxXLSXAttrRotateWithShape = AnsiString('rotWithShape');
  sdxXLSXAttrRowHeight = AnsiString('ht');
  sdxXLSXAttrRowIndex = AnsiString('r');
  sdxXLSXAttrSaltValue = AnsiString('saltValue');
  sdxXLSXAttrScaled = AnsiString('scaled');
  sdxXLSXAttrSelectLockedCells = AnsiString('selectLockedCells');
  sdxXLSXAttrSelectUnlockedCell = AnsiString('selectUnlockedCell');
  sdxXLSXAttrShapeId = AnsiString('shapeId');
  sdxXLSXAttrSharedIndex = AnsiString('si');
  sdxXLSXAttrSheet = AnsiString('sheet');
  sdxXLSXAttrSheetId = AnsiString('sheetId');
  sdxXLSXAttrShowFormulas = 'showFormulas';
  sdxXLSXAttrShowHorizontalScroll = AnsiString('showHorizontalScroll');
  sdxXLSXAttrShowRowColHeaders = AnsiString('showRowColHeaders');
  sdxXLSXAttrShowSheetTabs = AnsiString('showSheetTabs');
  sdxXLSXAttrShowValue = AnsiString('showValue');
  sdxXLSXAttrShowVerticalScroll = AnsiString('showVerticalScroll');
  sdxXLSXAttrShrinkToFit = AnsiString('shrinkToFit');
  sdxXLSXAttrSort = AnsiString('sort');
  sdxXLSXAttrSpinCount = AnsiString('spinCount');
  sdxXLSXAttrSplitX = AnsiString('xSplit');
  sdxXLSXAttrSplitY = AnsiString('ySplit');
  sdxXLSXAttrSqRef = AnsiString('sqref');
  sdxXLSXAttrState = AnsiString('state');
  sdxXLSXAttrStdDev = AnsiString('stdDev');
  sdxXLSXAttrStopIfTrue = AnsiString('stopIfTrue');
  sdxXLSXAttrStrokeColor = AnsiString('strokecolor');
  sdxXLSXAttrStrokeWeight = AnsiString('strokeweight');
  sdxXLSXAttrStyle = AnsiString('style');
  sdxXLSXAttrStyleIndex = AnsiString('s');
  sdxXLSXAttrSummaryBelow = AnsiString('summaryBelow');
  sdxXLSXAttrSummaryRight = AnsiString('summaryRight');
  sdxXLSXAttrTabSelected = AnsiString('tabSelected');
  sdxXLSXAttrTarget = AnsiString('Target');
  sdxXLSXAttrTargetMode = AnsiString('TargetMode');
  sdxXLSXAttrTextBox = AnsiString('txBox');
  sdxXLSXAttrTextLink = AnsiString('textlink');
  sdxXLSXAttrTextRotation = AnsiString('textRotation');
  sdxXLSXAttrTheme = AnsiString('theme');
  sdxXLSXAttrThemeTint = AnsiString('tint');
  sdxXLSXAttrTileAlign = 'algn';
  sdxXLSXAttrTileSX = 'sx';
  sdxXLSXAttrTileSY = 'sy';
  sdxXLSXAttrTileTX = 'tx';
  sdxXLSXAttrTileTY = 'ty';
  sdxXLSXAttrTitle = AnsiString('title');
  sdxXLSXAttrTooltip = AnsiString('tooltip');
  sdxXLSXAttrTopInset = AnsiString('tIns');
  sdxXLSXAttrTopLeftCell = AnsiString('topLeftCell');
  sdxXLSXAttrType = AnsiString('Type');
  sdxXLSXAttrTypeface = AnsiString('typeface');
  sdxXLSXAttrTypeLC = AnsiString('type');
  sdxXLSXAttrUniqueCount = AnsiString('uniqueCount');
  sdxXLSXAttrVal = AnsiString('val');
  sdxXLSXAttrVertical = AnsiString('vertical');
  sdxXLSXAttrVisibility = AnsiString('visibility');
  sdxXLSXAttrWidth = AnsiString('width');
  sdxXLSXAttrWorkbookAlgorithmName = AnsiString('workbookAlgorithmName');
  sdxXLSXAttrWorkbookHashValue = AnsiString('workbookHashValue');
  sdxXLSXAttrWorkbookPassword = AnsiString('workbookPassword');
  sdxXLSXAttrWorkbookSaltValue = AnsiString('workbookSaltValue');
  sdxXLSXAttrWorkbookSpinCount = AnsiString('workbookSpinCount');
  sdxXLSXAttrWorkbookViewId = AnsiString('workbookViewId');
  sdxXLSXAttrWrap = AnsiString('wrap');
  sdxXLSXAttrWrapText = AnsiString('wrapText');
  sdxXLSXAttrXFId = AnsiString('xfId');
  sdxXLSXAttrXMLNS = AnsiString('xmlns');
  sdxXLSXAttrXMLNSA = AnsiString('xmlns:a');
  sdxXLSXAttrXMLNSR = AnsiString('xmlns:r');
  sdxXLSXAttrXMLNSXDR = AnsiString('xmlns:xdr');
  sdxXLSXAttrZeroValues = 'showZeros';
  sdxXLSXAttrZoomScale = AnsiString('zoomScale');
  sdxXLSXAttrZoomScaleNormal = AnsiString('zoomScaleNormal');

  // Nodes
  sdxXLSXNodeAlignment = AnsiString('alignment');
  sdxXLSXNodeAnchorAbsolute = AnsiString('xdr:absoluteAnchor');
  sdxXLSXNodeAnchorFrom = AnsiString('xdr:from');
  sdxXLSXNodeAnchorOneCell = AnsiString('xdr:oneCellAnchor');
  sdxXLSXNodeAnchorTo = AnsiString('xdr:to');
  sdxXLSXNodeAnchorTwoCell = AnsiString('xdr:twoCellAnchor');
  sdxXLSXNodeAuthor = AnsiString('author');
  sdxXLSXNodeAuthors = AnsiString('authors');
  sdxXLSXNodeAVList = AnsiString('a:avLst');
  sdxXLSXNodeAxisColor = AnsiString('axisColor');
  sdxXLSXNodeBackgroundColor = AnsiString('bgColor');
  sdxXLSXNodeBodyProperties = AnsiString('a:bodyPr');
  sdxXLSXNodeBookViews = AnsiString('bookViews');
  sdxXLSXNodeBorderColor = AnsiString('borderColor');
  sdxXLSXNodeBreak = AnsiString('brk');
  sdxXLSXNodeCalcPr = AnsiString('calcPr');
  sdxXLSXNodeCellFunction = AnsiString('f');
  sdxXLSXNodeCellRichText = AnsiString('is');
  sdxXLSXNodeCellStylePatternFill = AnsiString('patternFill');
  sdxXLSXNodeCellValue = AnsiString('v');
  sdxXLSXNodeCFIcon = AnsiString('cfIcon');
  sdxXLSXNodeCFVO = AnsiString('cfvo');
  sdxXLSXNodeCharset = AnsiString('charset');
  sdxXLSXNodeClientData = AnsiString('xdr:clientData');
  sdxXLSXNodeColBreaks = AnsiString('colBreaks');
  sdxXLSXNodeColor = AnsiString('color');
  sdxXLSXNodeColorAlpha = AnsiString('a:alpha');
  sdxXLSXNodeColorScale = AnsiString('colorScale');
  sdxXLSXNodeColumn = AnsiString('col');
  sdxXLSXNodeColumns = AnsiString('cols');
  sdxXLSXNodeComment = AnsiString('comment');
  sdxXLSXNodeCommentList = AnsiString('commentList');
  sdxXLSXNodeComments = AnsiString('comments');
  sdxXLSXNodeConditionalFormattings = AnsiString('conditionalFormattings');
  sdxXLSXNodeConditionalFormatting = 'conditionalFormatting';
  sdxXLSXNodeConditionalFormattingRule = 'cfRule';
  sdxXLSXNodeCoordExt = AnsiString('a:ext');
  sdxXLSXNodeCoordOff = AnsiString('a:off');
  sdxXLSXNodeDataBar = AnsiString('dataBar');
  sdxXLSXNodeDefault = AnsiString('Default');
  sdxXLSXNodeDefinedName = AnsiString('definedName');
  sdxXLSXNodeDefinedNames = AnsiString('definedNames');
  sdxXLSXNodeDiagonal = AnsiString('diagonal');
  sdxXLSXNodeDimension = AnsiString('dimension');
  sdxXLSXNodeDrawing = AnsiString('drawing');
  sdxXLSXNodeDrawingAttributeSourceRect = AnsiString('a:srcRect');
  sdxXLSXNodeDrawingBlip = AnsiString('a:blip');
  sdxXLSXNodeDrawingBlipFill = AnsiString('xdr:blipFill');
  sdxXLSXNodeDrawingDescription = AnsiString('xdr:cNvPr');
  sdxXLSXNodeDrawingHeader = AnsiString('xdr:wsDr');
  sdxXLSXNodeDrawingPatternBackgroundColor = AnsiString('a:bgClr');
  sdxXLSXNodeDrawingPatternForegroundColor = AnsiString('a:fgClr');
  sdxXLSXNodeDrawingPictureAttributes = AnsiString('xdr:cNvPicPr');
  sdxXLSXNodeDrawingPictureContainer = AnsiString('xdr:pic');
  sdxXLSXNodeDrawingPictureDescription = AnsiString('xdr:nvPicPr');
  sdxXLSXNodeDrawingPictureLocks = AnsiString('a:picLocks');
  sdxXLSXNodeDrawingPosMarkerColumn = AnsiString('xdr:col');
  sdxXLSXNodeDrawingPosMarkerColumnOffset = AnsiString('xdr:colOff');
  sdxXLSXNodeDrawingPosMarkerRow = AnsiString('xdr:row');
  sdxXLSXNodeDrawingPosMarkerRowOffset = AnsiString('xdr:rowOff');
  sdxXLSXNodeDrawingShapeAttributesEx = AnsiString('xdr:cNvSpPr');
  sdxXLSXNodeDrawingShapeContainer = AnsiString('xdr:sp');
  sdxXLSXNodeDrawingShapeDescription = AnsiString('xdr:nvSpPr');
  sdxXLSXNodeDrawingShapeGeometry = AnsiString('a:prstGeom');
  sdxXLSXNodeDrawingShapeGroup = AnsiString('xdr:grpSp');
  sdxXLSXNodeDrawingShapeHLink = AnsiString('a:hlinkClick');
  sdxXLSXNodeDrawingShapeLocks = AnsiString('a:spLocks');
  sdxXLSXNodeDrawingShapeProperties = AnsiString('xdr:spPr');
  sdxXLSXNodeDrawingStyle = AnsiString('xdr:style');
  sdxXLSXNodeDrawingTextBody = AnsiString('xdr:txBody');
  sdxXLSXNodeDrawingXForm = AnsiString('a:xfrm');
  sdxXLSXNodeDXF = AnsiString('dxf');
  sdxXLSXNodeDXFS = AnsiString('dxfs');
  sdxXLSXNodeEvenFooter = AnsiString('evenFooter');
  sdxXLSXNodeEvenHeader = AnsiString('evenHeader');
  sdxXLSXNodeExt = AnsiString('ext');
  sdxXLSXNodeExternalBook = AnsiString('externalBook');
  sdxXLSXNodeExternalLink = AnsiString('externalLink');
  sdxXLSXNodeExternalReference = AnsiString('externalReference');
  sdxXLSXNodeExternalReferences = AnsiString('externalReferences');
  sdxXLSXNodeExtList = AnsiString('extLst');
  sdxXLSXNodeFillColor = AnsiString('fillColor');
  sdxXLSXNodeFillRect = AnsiString('a:fillRect');
  sdxXLSXNodeFillRef = AnsiString('a:fillRef');
  sdxXLSXNodeFirstFooter = AnsiString('firstFooter');
  sdxXLSXNodeFirstHeader = AnsiString('firstHeader');
  sdxXLSXNodeFontName = AnsiString('rFont');
  sdxXLSXNodeForegroundColor = AnsiString('fgColor');
  sdxXLSXNodeFormula = AnsiString('formula');
  sdxXLSXNodeGradientFill = AnsiString('a:gradFill');
  sdxXLSXNodeGradientPoint = AnsiString('a:gs');
  sdxXLSXNodeGradientPoints = AnsiString('a:gsLst');
  sdxXLSXNodeHeaderFooter = AnsiString('headerFooter');
  sdxXLSXNodeHyperlink = AnsiString('hyperlink');
  sdxXLSXNodeHyperlinks = AnsiString('hyperlinks');
  sdxXLSXNodeIconSet = AnsiString('iconSet');
  sdxXLSXNodeLatin = AnsiString('a:latin');
  sdxXLSXNodeLegacyDrawing = AnsiString('legacyDrawing');
  sdxXLSXNodeLine = AnsiString('a:ln');
  sdxXLSXNodeLinearGradientFill = AnsiString('a:lin');
  sdxXLSXNodeLineDash = AnsiString('a:prstDash');
  sdxXLSXNodeLineRef = AnsiString('a:lnRef');
  sdxXLSXNodeListStyle = AnsiString('a:lstStyle');
  sdxXLSXNodeLumMod = AnsiString('a:lumMod');
  sdxXLSXNodeLumOff = AnsiString('a:lumOff');
  sdxXLSXNodeMergeCell = AnsiString('mergeCell');
  sdxXLSXNodeMergeCells = AnsiString('mergeCells');
  sdxXLSXNodeName = AnsiString('name');
  sdxXLSXNodeNegativeBorderColor = AnsiString('negativeBorderColor');
  sdxXLSXNodeNegativeFillColor = AnsiString('negativeFillColor');
  sdxXLSXNodeNoAutoFit = AnsiString('noAutofit');
  sdxXLSXNodeNoFill = AnsiString('a:noFill');
  sdxXLSXNodeOddFooter = AnsiString('oddFooter');
  sdxXLSXNodeOddHeader = AnsiString('oddHeader');
  sdxXLSXNodeOutlinePr = AnsiString('outlinePr');
  sdxXLSXNodeOverride = AnsiString('Override');
  sdxXLSXNodePageMargins = AnsiString('pageMargins');
  sdxXLSXNodePageSetup = AnsiString('pageSetup');
  sdxXLSXNodePageSetUpPr = AnsiString('pageSetUpPr');
  sdxXLSXNodePane = AnsiString('pane');
  sdxXLSXNodeParagraph = AnsiString('a:p');
  sdxXLSXNodePatternFill = AnsiString('a:pattFill');
  sdxXLSXNodePrintOptions = AnsiString('printOptions');
  sdxXLSXNodeProtection = AnsiString('protection');
  sdxXLSXNodeRelationship = AnsiString('Relationship');
  sdxXLSXNodeRelationships = AnsiString('Relationships');
  sdxXLSXNodeRichTextEndParagraphRunProperties = AnsiString('endParaRPr');
  sdxXLSXNodeRichTextParagraphProperties = AnsiString('pPr');
  sdxXLSXNodeRichTextRun = AnsiString('r');
  sdxXLSXNodeRichTextRunParagraph = AnsiString('rPr');
  sdxXLSXNodeRow = AnsiString('row');
  sdxXLSXNodeRowBreaks = AnsiString('rowBreaks');
  sdxXLSXNodeSchemeColor = AnsiString('a:schemeClr');
  sdxXLSXNodeSelection = AnsiString('selection');
  sdxXLSXNodeShade = AnsiString('a:shade');
  sdxXLSXNodeSheet = AnsiString('sheet');
  sdxXLSXNodeSheetData = AnsiString('sheetData');
  sdxXLSXNodeSheetFormatPr = AnsiString('sheetFormatPr');
  sdxXLSXNodeSheetPr = AnsiString('sheetPr');
  sdxXLSXNodeSheetProtection = AnsiString('sheetProtection');
  sdxXLSXNodeSheets = AnsiString('sheets');
  sdxXLSXNodeSheetsView = AnsiString('sheetViews');
  sdxXLSXNodeSheetView = AnsiString('sheetView');
  sdxXLSXNodeSI = AnsiString('si');
  sdxXLSXNodeSolidFill = AnsiString('a:solidFill');
  sdxXLSXNodeSpAutoFit = AnsiString('spAutoFit');
  sdxXLSXNodeSST = AnsiString('sst');
  sdxXLSXNodeStretch = AnsiString('a:stretch');
  sdxXLSXNodeStyleBorder = AnsiString('border');
  sdxXLSXNodeStyleBorders = AnsiString('borders');
  sdxXLSXNodeStyleCellStyleXfs = AnsiString('cellStyleXfs');
  sdxXLSXNodeStyleCellXf = AnsiString('xf');
  sdxXLSXNodeStyleCellXfs = AnsiString('cellXfs');
  sdxXLSXNodeStyleFill = AnsiString('fill');
  sdxXLSXNodeStyleFills = AnsiString('fills');
  sdxXLSXNodeStyleFont = AnsiString('font');
  sdxXLSXNodeStyleFonts = AnsiString('fonts');
  sdxXLSXNodeStyleNumberFormat = AnsiString('numFmt');
  sdxXLSXNodeStyleNumberFormats = AnsiString('numFmts');
  sdxXLSXNodeStyleSheet = AnsiString('styleSheet');
  sdxXLSXNodeSystemColor = AnsiString('a:sysClr');
  sdxXLSXNodeSZ = AnsiString('sz');
  sdxXLSXNodeText = AnsiString('t');
  sdxXLSXNodeTextFull = AnsiString('text');
  sdxXLSXNodeTexturedFill = AnsiString('a:blipFill');
  sdxXLSXNodeThemesColorScheme = AnsiString('a:clrScheme');
  sdxXLSXNodeThemesCustomColor = AnsiString('a:srgbClr');
  sdxXLSXNodeThemesElements = AnsiString('a:themeElements');
  sdxXLSXNodeThemesFormatScheme = AnsiString('a:fmtScheme');
  sdxXLSXNodeThemesFormatSchemeFillStyleList = AnsiString('a:fillStyleLst');
  sdxXLSXNodeThemesFormatSchemeLineStyleList = AnsiString('a:lnStyleLst');
  sdxXLSXNodeThemesSystemColor = AnsiString('a:sysClr');
  sdxXLSXNodeTile = 'a:tile';
  sdxXLSXNodeVMLFill = AnsiString('v:fill');
  sdxXLSXNodeVMLShape = AnsiString('v:shape');
  sdxXLSXNodeVMLStroke = AnsiString('v:stroke');
  sdxXLSXNodeVMLTextBox = AnsiString('v:textbox');
  sdxXLSXNodeWorkbook = AnsiString('workbook');
  sdxXLSXNodeWorkbookPr = AnsiString('workbookPr');
  sdxXLSXNodeWorkbookProtection = AnsiString('workbookProtection');
  sdxXLSXNodeWorkBookView = AnsiString('workbookView');
  sdxXLSXNodeWorksheet = AnsiString('worksheet');
  sdxXLSXNodeX14ID = sdxXLSXNamespaceX14 + ':' + AnsiString('id');
  sdxXLSXNodeXAnchor = AnsiString('x:Anchor');
  sdxXLSXNodeXAutoFill = AnsiString('x:AutoFill');
  sdxXLSXNodeXClientData = AnsiString('x:ClientData');
  sdxXLSXNodeXColumn = AnsiString('x:Column');
  sdxXLSXNodeXDRExt = AnsiString('xdr:ext');
  sdxXLSXNodeXDRPos = AnsiString('xdr:pos');
  sdxXLSXNodeXMFunc = AnsiString('xm:f');
  sdxXLSXNodeXML = AnsiString('xml');
  sdxXLSXNodeXMoveWithCells = AnsiString('x:MoveWithCells');
  sdxXLSXNodeXMSqRef = AnsiString('xm:sqref');
  sdxXLSXNodeXRow = AnsiString('x:Row');
  sdxXLSXNodeXSizeWithCells = AnsiString('x:SizeWithCells');
  sdxXLSXNodeXTextHAlign = AnsiString('x:TextHAlign');
  sdxXLSXNodeXTextVAlign = AnsiString('x:TextVAlign');
  sdxXLSXNodeXVisible = AnsiString('x:Visible');
  sdxXLSXNodeVertAlign = AnsiString('vertAlign');


  // Mime Types
  sdxXLSXMimeTypeJPG = 'image/jpeg';
  sdxXLSXMimeTypeJPGExt = 'jpeg';
  sdxXLSXMimeTypePNG = 'image/png';
  sdxXLSXMimeTypePNGExt = 'png';
  sdxXLSXMimeTypeRELS = sdxXLSXRelsContentType;
  sdxXLSXMimeTypeRELSExt = 'rels';
  sdxXLSXMimeTypeXML = 'application/xml';
  sdxXLSXMimeTypeXMLExt = 'xml';
  sdxXLSXMimeTypeVML = 'application/vnd.openxmlformats-officedocument.vmlDrawing';
  sdxXLSXMimeTypeVMLExt = 'vml';

  // Values
  sdxXLSXValueA1 = AnsiString('A1');
  sdxXLSXValueAboveAverage = AnsiString('aboveAverage');
  sdxXLSXValueArray = AnsiString('array');
  sdxXLSXValueAxisAuto = AnsiString('automatic');
  sdxXLSXValueAxisMiddle = AnsiString('middle');
  sdxXLSXValueAxisNone = AnsiString('none');
  sdxXLSXValueCellIs = AnsiString('cellIs');
  sdxXLSXValueColorScale = AnsiString('colorScale');
  sdxXLSXValueDuplicateValues = AnsiString('duplicateValues');
  sdxXLSXValueEditAsAbsolute = AnsiString('absolute');
  sdxXLSXValueEditAsOneCell = AnsiString('oneCell');
  sdxXLSXValueEditAsTwoCell = AnsiString('twoCell');
  sdxXLSXValueExpression = AnsiString('expression');
  sdxXLSXValueFrame = AnsiString('frame');
  sdxXLSXValueFrozen = AnsiString('frozen');
  sdxXLSXValueGradient = AnsiString('gradient');
  sdxXLSXValueHidden = AnsiString('hidden');
  sdxXLSXValueNone = AnsiString('none');
  sdxXLSXValuePaneBottomLeft = AnsiString('bottomLeft');
  sdxXLSXValuePaneBottomRight = AnsiString('bottomRight');
  sdxXLSXValuePaneTopRight = AnsiString('topRight');
  sdxXLSXValuePattern = AnsiString('pattern');
  sdxXLSXValueR1C1 = AnsiString('R1C1');
  sdxXLSXValueShared = AnsiString('shared');
  sdxXLSXValueSingle = AnsiString('sng');
  sdxXLSXValueSolid = AnsiString('solid');
  sdxXLSXValueSquare = AnsiString('square');
  sdxXLSXValueSubscript = AnsiString('subscript');
  sdxXLSXValueSuperscript = AnsiString('superscript');
  sdxXLSXValueTargetModeExternal = AnsiString('External');
  sdxXLSXValueTile = AnsiString('tile');
  sdxXLSXValueTop10 = AnsiString('top10');
  sdxXLSXValueTypeDataBar = AnsiString('dataBar');
  sdxXLSXValueTypeFormula = AnsiString('formula');
  sdxXLSXValueTypeIconSet = AnsiString('iconSet');
  sdxXLSXValueTypeMax = AnsiString('max');
  sdxXLSXValueTypeMin = AnsiString('min');
  sdxXLSXValueTypeNum = AnsiString('num');
  sdxXLSXValueTypePercent = AnsiString('percent');
  sdxXLSXValueTypePercentile = AnsiString('percentile');
  sdxXLSXValueUniqueValues = AnsiString('uniqueValues');
  sdxXLSXValueVisible = AnsiString('visible');

  sdxXLSXMSOFitShapeToText = 'mso-fit-shape-to-text';

  sdxXLSXPrintAreaDefinedName = '_xlnm.Print_Area';
  sdxXLSXPrintTitlesDefinedName = '_xlnm.Print_Titles';

const
  dxXLSXAlignHorzNames: array[TdxSpreadSheetDataAlignHorz] of AnsiString = (
    'general', 'left', 'center', 'right', 'fill', 'justify', 'distributed'
  );
  dxXLSXAlignVertNames: array[TdxSpreadSheetDataAlignVert] of AnsiString = (
    'top', 'center', 'bottom', 'justify', 'distributed'
  );
  dxXLSXBorderNames: array [TcxBorder] of AnsiString = (
    'left', 'top', 'right', 'bottom'
  );

  dxXLSXCellBorderStyleNames: array [TdxSpreadSheetCellBorderStyle] of AnsiString = (
    '', 'hair', 'dotted', 'dashDotDot', 'dashDot', 'dashed', 'thin', 'mediumDashDotDot', 'slantDashDot',
    'mediumDashDot', 'mediumDashed', 'medium', 'thick', 'double', 'none'
  );

  dxXLSXCellDataTypeNames: array[TdxSpreadSheetXLSXCellType] of AnsiString = (
    '?', 'b', 'e', 'n', 'str', 's', '', 'inlineStr'
  );
  dxXLSXCellFillStyleNames: array [TdxSpreadSheetCellFillStyle] of AnsiString = (
    'solid', 'darkGray', 'mediumGray', 'lightGray', 'gray125', 'gray0625',
    'darkHorizontal', 'darkVertical', 'darkDown', 'darkUp', 'darkGrid', 'darkTrellis',
    'lightHorizontal', 'lightVertical', 'lightDown', 'lightUp', 'lightTrellis', 'lightGrid'
  );
  dxXLSXFontStyles: array[TFontStyle] of AnsiString = ('b', 'i', 'u', 'strike');

  dxXLSXRestrictionNames: array[TdxSpreadSheetContainerRestriction] of AnsiString = (
    'noCrop', 'noMove', 'noResize', 'noRot', 'noChangeAspect'
  );

  dxXLSXShapeTypeMap: array[TdxSpreadSheetShapeType] of AnsiString = (
    'rect', 'roundRect', 'ellipse'
  );

  dxXLSXPenStyleMap: array[TdxGPPenStyle] of AnsiString = (
    'solid', 'dash', 'dot', 'dashDot', 'lgDashDotDot'
  );

  dxXLSXPrintCellComments: array[TdxSpreadSheetTableViewOptionsPrintSourceCellComments] of AnsiString = (
    'asDisplayed', 'atEnd', 'none'
  );

  dxXLSXPrintErrorIndication: array[TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication] of AnsiString = (
    '', 'blank', 'dash', 'displayed', 'NA'
  );

  dxXLSXPrintPageOrientation: array[TdxSpreadSheetTableViewOptionsPrintPageOrientation] of AnsiString = (
    'default', 'landscape', 'portrait'
  );

  dxXLSXPrintPageOrder: array[TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder] of AnsiString = (
    '', 'downThenOver', 'overThenDown'
  );

  dxXLSXCfCellIsRuleOperator: array[TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator] of AnsiString = (
    'between', 'equal', 'greaterThan', 'greaterThanOrEqual', 'lessThan', 'lessThanOrEqual', 'notBetween', 'notEqual'
  );

  dxXLSXScaleStopValueTypeMap: array[TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType] of AnsiString = (
    '', sdxXLSXValueTypeNum, sdxXLSXValueTypePercent, sdxXLSXValueTypeFormula, sdxXLSXValueTypePercentile
  );

  dxXLSXAxisPosition: array[TdxSpreadSheetConditionalFormattingRuleDataBarAxisPosition] of AnsiString = (
    sdxXLSXValueAxisAuto, sdxXLSXValueAxisMiddle, sdxXLSXValueAxisNone
  );

  dxXLSXDataBarDirectionMap: array[TdxSpreadSheetConditionalFormattingRuleDataBarDirection] of AnsiString = (
    'context', 'leftToRight', 'rightToLeft'
  );

  dxXLSXAlignmentMap: array[TAlignment] of AnsiString = (
    'l', 'r', 'ctr'
  );

  dxXLSXVerticalAlignmentMap: array[TVerticalAlignment] of AnsiString = (
    't', 'b', 'ctr'
  );

  dxXLSXVMLDashStyle: array [TdxGPPenStyle] of string = (
    'solid', 'dash', 'dot', 'dashdot', 'dashdotdot'
  );

  dxXLSXVMLTextAlignHorzMap: array[TAlignment] of string = (
    'Left', 'Right', 'Center'
  );

  dxXLSXVMLTextAlignVertMap: array[TVerticalAlignment] of string = (
    'Top', 'Bottom', 'Center'
  );

  dxXLSXHashAlgorithmTypeNames: array[TdxHashAlgorithmType] of string = (
    '', 'MD2', 'MD4', 'MD5',' SHA-1', '', 'RIPEMD-128', 'RIPEMD-160', '', '', '', '', 'SHA-256', 'SHA-384', 'SHA-512'
  );


type

  { TdxSpreadSheetXLSXHelper }

  TdxSpreadSheetXLSXHelper = class
  public
    class function AlignHorzToString(const S: TdxSpreadSheetDataAlignHorz): AnsiString;
    class function AlignVertToString(const S: TdxSpreadSheetDataAlignVert): AnsiString;
    class function BorderStyleToString(const S: TdxSpreadSheetCellBorderStyle): AnsiString;
    class function FillStyleToString(const S: TdxSpreadSheetBrushHandle): AnsiString;
    class function PenStyleToString(const S: TdxGPPenStyle): AnsiString;
    class function ShapeTypeToString(const S: TdxSpreadSheetShapeType): AnsiString;

    class function StringToAlignHorz(const S: AnsiString): TdxSpreadSheetDataAlignHorz;
    class function StringToAlignment(const S: AnsiString): TAlignment;
    class function StringToAlignVert(const S: AnsiString): TdxSpreadSheetDataAlignVert;
    class function StringToAxisPosition(const S: AnsiString): TdxSpreadSheetConditionalFormattingRuleDataBarAxisPosition;
    class function StringToBorderStyle(const S: AnsiString): TdxSpreadSheetCellBorderStyle;
    class function StringToCellType(const S: AnsiString): TdxSpreadSheetXLSXCellType;
    class function StringToCfCellIsRuleOperator(const S: AnsiString): TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator;
    class function StringToColorScaleStopValueType(const S: AnsiString): TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
    class function StringToDataBarDirection(const S: AnsiString): TdxSpreadSheetConditionalFormattingRuleDataBarDirection;
    class function StringToFillStyle(const S: AnsiString): TdxSpreadSheetCellFillStyle;
    class function StringToHashAlgorithm(const S: string): TdxHashAlgorithmType;
    class function StringToPenStyle(const S: AnsiString): TdxGPPenStyle;
    class function StringToPrintCellComments(const S: AnsiString): TdxSpreadSheetTableViewOptionsPrintSourceCellComments;
    class function StringToPrintErrorIndication(const S: AnsiString): TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication;
    class function StringToPrintPageOrder(const S: AnsiString): TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder;
    class function StringToPrintPageOrientation(const S: AnsiString): TdxSpreadSheetTableViewOptionsPrintPageOrientation;
    class function StringToShapeType(const S: AnsiString): TdxSpreadSheetShapeType;
    class function StringToVerticalAlignment(const S: AnsiString): TVerticalAlignment;
    class function StringToVMLDashStyle(S: AnsiString): TdxGPPenStyle;
    class function StringToVMLTextAlignHorz(const S: string): TAlignment;
    class function StringToVMLTextAlignVert(const S: string): TVerticalAlignment;
  end;

implementation

uses
  AnsiStrings, SysUtils;

{ TdxSpreadSheetXLSXHelper }

class function TdxSpreadSheetXLSXHelper.AlignHorzToString(const S: TdxSpreadSheetDataAlignHorz): AnsiString;
begin
  Result := dxXLSXAlignHorzNames[S];
end;

class function TdxSpreadSheetXLSXHelper.AlignVertToString(const S: TdxSpreadSheetDataAlignVert): AnsiString;
begin
  Result := dxXLSXAlignVertNames[S];
end;

class function TdxSpreadSheetXLSXHelper.BorderStyleToString(const S: TdxSpreadSheetCellBorderStyle): AnsiString;
begin
  Result := dxXLSXCellBorderStyleNames[S];
end;

class function TdxSpreadSheetXLSXHelper.FillStyleToString(const S: TdxSpreadSheetBrushHandle): AnsiString;
begin
  if cxColorIsValid(S.BackgroundColor) or (S.Style <> sscfsSolid) then
    Result := dxXLSXCellFillStyleNames[S.Style]
  else
    Result := 'none';
end;

class function TdxSpreadSheetXLSXHelper.PenStyleToString(const S: TdxGPPenStyle): AnsiString;
begin
  Result := dxXLSXPenStyleMap[S];
end;

class function TdxSpreadSheetXLSXHelper.ShapeTypeToString(const S: TdxSpreadSheetShapeType): AnsiString;
begin
  Result := dxXLSXShapeTypeMap[S];
end;

class function TdxSpreadSheetXLSXHelper.StringToAlignHorz(const S: AnsiString): TdxSpreadSheetDataAlignHorz;
var
  I: TdxSpreadSheetDataAlignHorz;
begin
  Result := ssahGeneral;
  for I := Low(TdxSpreadSheetDataAlignHorz) to High(TdxSpreadSheetDataAlignHorz) do
    if SameText(S, dxXLSXAlignHorzNames[I]) then
    begin
      Result := I;
      Break;
    end;
end;

class function TdxSpreadSheetXLSXHelper.StringToAlignment(const S: AnsiString): TAlignment;
var
  I: TAlignment;
begin
  for I := Low(TAlignment) to High(TAlignment) do
  begin
    if SameText(S, dxXLSXAlignmentMap[I]) then
      Exit(I);
  end;
  Result := taLeftJustify;
end;

class function TdxSpreadSheetXLSXHelper.StringToAlignVert(const S: AnsiString): TdxSpreadSheetDataAlignVert;
var
  I: TdxSpreadSheetDataAlignVert;
begin
  Result := ssavBottom;
  for I := Low(TdxSpreadSheetDataAlignVert) to High(TdxSpreadSheetDataAlignVert) do
    if SameText(S, dxXLSXAlignVertNames[I]) then
    begin
      Result := I;
      Break;
    end;
end;

class function TdxSpreadSheetXLSXHelper.StringToAxisPosition(
  const S: AnsiString): TdxSpreadSheetConditionalFormattingRuleDataBarAxisPosition;
var
  I: TdxSpreadSheetConditionalFormattingRuleDataBarAxisPosition;
begin
  for I := Low(I) to High(I) do
  begin
    if SameText(S, dxXLSXAxisPosition[I]) then
      Exit(I);
  end;
  Result := dbapAuto;
end;

class function TdxSpreadSheetXLSXHelper.StringToBorderStyle(const S: AnsiString): TdxSpreadSheetCellBorderStyle;
var
  I: TdxSpreadSheetCellBorderStyle;
begin
  for I := Low(TdxSpreadSheetCellBorderStyle) to High(TdxSpreadSheetCellBorderStyle) do
  begin
    if SameText(S, dxXLSXCellBorderStyleNames[I]) then
      Exit(I);
  end;
  Result := sscbsDefault;
end;

class function TdxSpreadSheetXLSXHelper.StringToCellType(const S: AnsiString): TdxSpreadSheetXLSXCellType;
var
  I: TdxSpreadSheetXLSXCellType;
begin
  for I := Low(TdxSpreadSheetXLSXCellType) to High(TdxSpreadSheetXLSXCellType) do
  begin
    if SameText(S, dxXLSXCellDataTypeNames[I]) then
      Exit(I);
  end;
  Result := sxctUnknown;
end;

class function TdxSpreadSheetXLSXHelper.StringToCfCellIsRuleOperator(
  const S: AnsiString): TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator;
var
  I: TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator;
begin
  for I := Low(TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator) to High(TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator) do
  begin
    if SameText(S, dxXLSXCfCellIsRuleOperator[I]) then
      Exit(I);
  end;
  Result := cicoEqual;
end;

class function TdxSpreadSheetXLSXHelper.StringToColorScaleStopValueType(
  const S: AnsiString): TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
var
  I: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
begin
  if (S = sdxXLSXValueTypeMax) or (S = sdxXLSXValueTypeMin) then
    Exit(cssvtLimitValue);
  for I := Low(I) to High(I) do
  begin
    if dxXLSXScaleStopValueTypeMap[I] = S then
      Exit(I);
  end;
  Result := cssvtLimitValue;
end;

class function TdxSpreadSheetXLSXHelper.StringToDataBarDirection(
  const S: AnsiString): TdxSpreadSheetConditionalFormattingRuleDataBarDirection;
var
  I: TdxSpreadSheetConditionalFormattingRuleDataBarDirection;
begin
  for I := Low(I) to High(I) do
  begin
    if S = dxXLSXDataBarDirectionMap[I] then
      Exit(I);
  end;
  Result := dbdAuto;
end;

class function TdxSpreadSheetXLSXHelper.StringToFillStyle(const S: AnsiString): TdxSpreadSheetCellFillStyle;
var
  I: TdxSpreadSheetCellFillStyle;
begin
  Result := sscfsSolid;
  for I := Low(TdxSpreadSheetCellFillStyle) to High(TdxSpreadSheetCellFillStyle) do
    if SameText(S, dxXLSXCellFillStyleNames[I]) then
    begin
      Result := I;
      Break;
    end;
end;

class function TdxSpreadSheetXLSXHelper.StringToHashAlgorithm(const S: string): TdxHashAlgorithmType;
var
  I: TdxHashAlgorithmType;
begin
  for I := Low(I) to High(I) do
  begin
    if S = dxXLSXHashAlgorithmTypeNames[I] then
      Exit(I);
  end;
  Result := TdxHashAlgorithmType.None;
end;

class function TdxSpreadSheetXLSXHelper.StringToPenStyle(const S: AnsiString): TdxGPPenStyle;
const
  sLongDash = AnsiString('lgDash');
  sLongDashDot = AnsiString('lgDashDot');
  sLongDashDotDot = AnsiString('lgDashDotDot');
  sSystemDash = AnsiString('sysDash');
  sSystemDashDot = AnsiString('sysDashDot');
  sSystemDashDotDot = AnsiString('sysDashDotDot');
  sSystemDot = AnsiString('sysDot');
var
  I: TdxGPPenStyle;
begin
  Result := gppsSolid;
  for I := Low(TdxGPPenStyle) to High(TdxGPPenStyle) do
    if SameText(S, dxXLSXPenStyleMap[I]) then
    begin
      Result := I;
      Exit;
    end;

  if SameText(S, sLongDash) or SameText(S, sSystemDash) then
    Result := gppsDash
  else

  if SameText(S, sLongDashDot) or SameText(S, sSystemDashDot) then
    Result := gppsDashDot
  else

  if SameText(S, sLongDashDotDot) or SameText(S, sSystemDashDotDot) then
    Result := gppsDashDotDot
  else

  if SameText(S, sSystemDot) then
    Result := gppsDot;
end;

class function TdxSpreadSheetXLSXHelper.StringToPrintCellComments(
  const S: AnsiString): TdxSpreadSheetTableViewOptionsPrintSourceCellComments;
var
  I: TdxSpreadSheetTableViewOptionsPrintSourceCellComments;
begin
  for I := Low(TdxSpreadSheetTableViewOptionsPrintSourceCellComments) to High(TdxSpreadSheetTableViewOptionsPrintSourceCellComments) do
  begin
    if SameText(S, dxXLSXPrintCellComments[I]) then
      Exit(I);
  end;
  Result := psccNone;
end;

class function TdxSpreadSheetXLSXHelper.StringToPrintErrorIndication(
  const S: AnsiString): TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication;
var
  I: TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication;
begin
  for I := Low(TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication) to High(TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication) do
  begin
    if SameText(S, dxXLSXPrintErrorIndication[I]) then
      Exit(I);
  end;
  Result := pseiDefault;
end;

class function TdxSpreadSheetXLSXHelper.StringToPrintPageOrder(const S: AnsiString): TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder;
var
  I: TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder;
begin
  for I := Low(TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder) to High(TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder) do
  begin
    if SameText(S, dxXLSXPrintPageOrder[I]) then
      Exit(I);
  end;
  Result := opppDefault;
end;

class function TdxSpreadSheetXLSXHelper.StringToPrintPageOrientation(
  const S: AnsiString): TdxSpreadSheetTableViewOptionsPrintPageOrientation;
var
  I: TdxSpreadSheetTableViewOptionsPrintPageOrientation;
begin
  for I := Low(TdxSpreadSheetTableViewOptionsPrintPageOrientation) to High(TdxSpreadSheetTableViewOptionsPrintPageOrientation) do
  begin
    if SameText(S, dxXLSXPrintPageOrientation[I]) then
      Exit(I);
  end;
  Result := oppoDefault;
end;

class function TdxSpreadSheetXLSXHelper.StringToShapeType(const S: AnsiString): TdxSpreadSheetShapeType;
var
  I: TdxSpreadSheetShapeType;
begin
  for I := Low(TdxSpreadSheetShapeType) to High(TdxSpreadSheetShapeType) do
  begin
    if SameText(S, dxXLSXShapeTypeMap[I]) then
      Exit(I);
  end;
  Result := stRect;
end;

class function TdxSpreadSheetXLSXHelper.StringToVerticalAlignment(const S: AnsiString): TVerticalAlignment;
var
  I: TVerticalAlignment;
begin
  for I := Low(TVerticalAlignment) to High(TVerticalAlignment) do
  begin
    if SameText(S, dxXLSXVerticalAlignmentMap[I]) then
      Exit(I);
  end;
  Result := taAlignTop;
end;

class function TdxSpreadSheetXLSXHelper.StringToVMLDashStyle(S: AnsiString): TdxGPPenStyle;
begin
  S := UpperCase(S);
  if Pos(AnsiString('DASHDOTDOT'), S) > 0 then
    Exit(gppsDashDotDot);
  if Pos(AnsiString('DASHDOT'), S) > 0 then
    Exit(gppsDashDot);
  if Pos(AnsiString('DASH'), S) > 0 then
    Exit(gppsDash);
  if Pos(AnsiString('DOT'), S) > 0 then
    Exit(gppsDot);
  Result := gppsSolid;
end;

class function TdxSpreadSheetXLSXHelper.StringToVMLTextAlignHorz(const S: string): TAlignment;
var
  I: TAlignment;
begin
  for I := Low(TAlignment) to High(TAlignment) do
  begin
    if SameText(S, dxXLSXVMLTextAlignHorzMap[I]) then
      Exit(I);
  end;
  Result := taLeftJustify;
end;

class function TdxSpreadSheetXLSXHelper.StringToVMLTextAlignVert(const S: string): TVerticalAlignment;
var
  I: TVerticalAlignment;
begin
  for I := Low(TVerticalAlignment) to High(TVerticalAlignment) do
  begin
    if SameText(S, dxXLSXVMLTextAlignVertMap[I]) then
      Exit(I);
  end;
  Result := taAlignTop;
end;

end.
