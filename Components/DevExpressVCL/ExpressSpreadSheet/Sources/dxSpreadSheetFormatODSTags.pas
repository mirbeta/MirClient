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

unit dxSpreadSheetFormatODSTags;

{$I cxVer.Inc}

interface

const
  sdxODSMimeTypeFile = 'mimetype';
  sdxODSMimeTypeSpreadSheet = 'application/vnd.oasis.opendocument.spreadsheet';
  sdxODSMimeTypeSpreadSheetTemplate = 'application/vnd.oasis.opendocument.spreadsheet-template';

  sdxODSContentFile = 'content.xml';
  sdxODSSettingsFile = 'settings.xml';
  sdxODSStylesFile = 'styles.xml';

  sdxODSNodeCalcExtColorScale = AnsiString('calcext:color-scale');
  sdxODSNodeCalcExtCondition = AnsiString('calcext:condition');
  sdxODSNodeCalcExtConditionalFormat = AnsiString('calcext:conditional-format');
  sdxODSNodeCalcExtConditionalFormats = AnsiString('calcext:conditional-formats');
  sdxODSNodeCalcExtDataBar = AnsiString('calcext:data-bar');
  sdxODSNodeCalcExtIconSet = AnsiString('calcext:icon-set');
  sdxODSNodeConfigConfigItem = AnsiString('config:config-item');
  sdxODSNodeConfigConfigItemMapIndexed = AnsiString('config:config-item-map-indexed');
  sdxODSNodeConfigConfigItemMapNamed = AnsiString('config:config-item-map-named');
  sdxODSNodeConfigConfigItemSet = AnsiString('config:config-item-set');
  sdxODSNodeDrawAnnotation = AnsiString('office:annotation');
  sdxODSNodeDrawCustomShape = AnsiString('draw:custom-shape');
  sdxODSNodeDrawFillImage = AnsiString('draw:fill-image');
  sdxODSNodeDrawFrame = AnsiString('draw:frame');
  sdxODSNodeDrawGradient = AnsiString('draw:gradient');
  sdxODSNodeDrawHatch = AnsiString('draw:hatch');
  sdxODSNodeDrawImage = AnsiString('draw:image');
  sdxODSNodeDrawStrokeDash = AnsiString('draw:stroke-dash');
  sdxODSNodeDrawURI = AnsiString('draw');
  sdxODSNodeFill = AnsiString('draw:fill');
  sdxODSNodeNumberDay = AnsiString('number:day');
  sdxODSNodeNumberDayOfWeek = AnsiString('number:day-of-week');
  sdxODSNodeNumberMonth = AnsiString('number:month');
  sdxODSNodeNumberNumber = AnsiString('number:number');
  sdxODSNodeNumberNumberStyle = AnsiString('number:number-style');
  sdxODSNodeNumberPercentageStyle = AnsiString('number:percentage-style');
  sdxODSNodeNumberText = AnsiString('number:text');
  sdxODSNodeNumberNameScope = AnsiString('number:');
  sdxODSNodeNumberYear = AnsiString('number:year');
  sdxODSNodeOfficeAutomaticStyles = AnsiString('office:automatic-styles');
  sdxODSNodeOfficeBody = AnsiString('office:body');
  sdxODSNodeOfficeDocumentContent = AnsiString('office:document-content');
  sdxODSNodeOfficeDocumentStyles = AnsiString('office:document-styles');
  sdxODSNodeOfficeMasterStyles = AnsiString('office:master-styles');
  sdxODSNodeOfficeSpreadSheet = AnsiString('office:spreadsheet');
  sdxODSNodeOfficeStyles = AnsiString('office:styles');
  sdxODSNodeStyle = AnsiString('style:style');
  sdxODSNodeStyleFooter = AnsiString('style:footer');
  sdxODSNodeStyleFooterLeft = AnsiString('style:footer-left');
  sdxODSNodeStyleFooterStyle = AnsiString('style:footer-style');
  sdxODSNodeStyleGraphicProperties = AnsiString('style:graphic-properties');
  sdxODSNodeStyleHeader = AnsiString('style:header');
  sdxODSNodeStyleHeaderLeft = AnsiString('style:header-left');
  sdxODSNodeStyleHeaderStyle = AnsiString('style:header-style');
  sdxODSNodeStyleMap = AnsiString('style:map');
  sdxODSNodeStyleMasterPage = AnsiString('style:master-page');
  sdxODSNodeStylePageLayout = AnsiString('style:page-layout');
  sdxODSNodeStylePageLayoutProperties = AnsiString('style:page-layout-properties');
  sdxODSNodeStyleParagraph = AnsiString('style:paragraph-properties');
  sdxODSNodeStyleRegionCenter = AnsiString('style:region-center');
  sdxODSNodeStyleRegionLeft = AnsiString('style:region-left');
  sdxODSNodeStyleRegionRight = AnsiString('style:region-right');
  sdxODSNodeStyleTableCellProperties = AnsiString('style:table-cell-properties');
  sdxODSNodeStyleTableColumnProperties = AnsiString('style:table-column-properties');
  sdxODSNodeStyleTableProperties = AnsiString('style:table-properties');
  sdxODSNodeStyleTableRowProperties = AnsiString('style:table-row-properties');
  sdxODSNodeStyleTextProperties = AnsiString('style:text-properties');
  sdxODSNodeSvgDescription = AnsiString('svg:desc');
  sdxODSNodeSvgTitle = AnsiString('svg:title');
  sdxODSNodeTable = AnsiString('table:table');
  sdxODSNodeTableCalculationSettings = AnsiString('table:calculation-settings');
  sdxODSNodeTableCell = AnsiString('table:table-cell');
  sdxODSNodeTableColumn = AnsiString('table:table-column');
  sdxODSNodeTableColumnGroup = AnsiString('table:table-column-group');
  sdxODSNodeTableDataBaseRange = AnsiString('table:database-range');
  sdxODSNodeTableDataBaseRanges = AnsiString('table:database-ranges');
  sdxODSNodeTableIteration = AnsiString('table:iteration');
  sdxODSNodeTableNamedExpression = AnsiString('table:named-expression');
  sdxODSNodeTableNamedExpressions = AnsiString('table:named-expressions');
  sdxODSNodeTableNamedRange = AnsiString('table:named-range');
  sdxODSNodeTableNullDate = AnsiString('table:null-date');
  sdxODSNodeTableRow = AnsiString('table:table-row');
  sdxODSNodeTableRowGroup = AnsiString('table:table-row-group');
  sdxODSNodeTableShapes = AnsiString('table:shapes');
  sdxODSNodeTableSource = AnsiString('table:table-source');
  sdxODSNodeText = AnsiString('text:p');
  sdxODSNodeTextLineBreak = AnsiString('text:line-break');
  sdxODSNodeTextSpace = AnsiString('text:s');
  sdxODSNodeTextSpan = AnsiString('text:span');
  sdxODSNodeTextTab = AnsiString('text:tab');
  sdxODSNodeTextXLink = AnsiString('text:a');

  sdxODSAttrCalcExtAxisColor = AnsiString('calcext:axis-color');
  sdxODSAttrCalcExtAxisPosition = AnsiString('calcext:axis-position');
  sdxODSAttrCalcExtColor = AnsiString('calcext:color');
  sdxODSAttrCalcExtIconSetType = AnsiString('calcext:icon-set-type');
  sdxODSAttrCalcExtNegativeColor = AnsiString('calcext:negative-color');
  sdxODSAttrCalcExtPositiveColor = AnsiString('calcext:positive-color');
  sdxODSAttrCalcExtStyleName = AnsiString('calcext:apply-style-name');
  sdxODSAttrCalcExtTargetRangeAddress = AnsiString('calcext:target-range-address');
  sdxODSAttrCalcExtType = AnsiString('calcext:type');
  sdxODSAttrCalcExtValue = AnsiString('calcext:value');
  sdxODSAttrConfigName = AnsiString('config:name');
  sdxODSAttrDrawAngle = AnsiString('draw:angle');
  sdxODSAttrDrawAutoGrowHeight = AnsiString('draw:auto-grow-height');
  sdxODSAttrDrawColor = AnsiString('draw:color');
  sdxODSAttrDrawDistance = AnsiString('draw:distance');
  sdxODSAttrDrawDots1 = AnsiString('draw:dots1');
  sdxODSAttrDrawDots1Length = AnsiString('draw:dots1-length');
  sdxODSAttrDrawDots2 = AnsiString('draw:dots2');
  sdxODSAttrDrawDots2Length = AnsiString('draw:dots2-length');
  sdxODSAttrDrawEndColor = AnsiString('draw:end-color');
  sdxODSAttrDrawEndIntensity = AnsiString('draw:end-intensity');
  sdxODSAttrDrawFillColor = AnsiString('draw:fill-color');
  sdxODSAttrDrawFillGradientName = AnsiString('draw:fill-gradient-name');
  sdxODSAttrDrawFillHatchSolid = AnsiString('draw:fill-hatch-solid');
  sdxODSAttrDrawFillImageName = AnsiString('draw:fill-image-name');
  sdxODSAttrDrawHatchName = AnsiString('draw:fill-hatch-name');
  sdxODSAttrDrawName = AnsiString('draw:name');
  sdxODSAttrDrawRotation = AnsiString('draw:rotation');
  sdxODSAttrDrawStartColor = AnsiString('draw:start-color');
  sdxODSAttrDrawStartIntensity = AnsiString('draw:start-intensity');
  sdxODSAttrDrawStroke = AnsiString('draw:stroke');
  sdxODSAttrDrawStrokeDash = AnsiString('draw:stroke-dash');
  sdxODSAttrDrawStyle = AnsiString('draw:style');
  sdxODSAttrDrawStyleName = AnsiString('draw:style-name');
  sdxODSAttrDrawTextAreaHorizontalAlign = AnsiString('draw:textarea-horizontal-align');
  sdxODSAttrDrawTextAreaVerticalAlign = AnsiString('draw:textarea-vertical-align');
  sdxODSAttrDrawTransform = AnsiString('draw:transform');
  sdxODSAttrDrawZIndex = AnsiString('draw:z-index');
  sdxODSAttrFOBackground = AnsiString('fo:background');
  sdxODSAttrFOBackgroundColor = AnsiString('fo:background-color');
  sdxODSAttrFOBorder = AnsiString('fo:border');
  sdxODSAttrFOBorderBottom = AnsiString('fo:border-bottom');
  sdxODSAttrFOBorderLeft = AnsiString('fo:border-left');
  sdxODSAttrFOBorderRight = AnsiString('fo:border-right');
  sdxODSAttrFOBorderTop = AnsiString('fo:border-top');
  sdxODSAttrFOColor = AnsiString('fo:color');
  sdxODSAttrFOFontFamily = AnsiString('fo:font-family');
  sdxODSAttrFOFontSize = AnsiString('fo:font-size');
  sdxODSAttrFOFontStyle = AnsiString('fo:font-style');
  sdxODSAttrFOFontVariant = AnsiString('fo:font-variant');
  sdxODSAttrFOFontWeight = AnsiString('fo:font-weight');
  sdxODSAttrFOMarginBottom = AnsiString('fo:margin-bottom');
  sdxODSAttrFOMarginLeft = AnsiString('fo:margin-left');
  sdxODSAttrFOMarginRight = AnsiString('fo:margin-right');
  sdxODSAttrFOMarginTop = AnsiString('fo:margin-top');
  sdxODSAttrFOMinHeight = AnsiString('fo:min-height');
  sdxODSAttrFOPaddingBottom = AnsiString('fo:padding-bottom');
  sdxODSAttrFOPaddingLeft = AnsiString('fo:padding-left');
  sdxODSAttrFOPaddingRight = AnsiString('fo:padding-right');
  sdxODSAttrFOPaddingTop = AnsiString('fo:padding-top');
  sdxODSAttrFOPageHeight = AnsiString('fo:page-height');
  sdxODSAttrFOPageWidth = AnsiString('fo:page-width');
  sdxODSAttrFOTextAlign = AnsiString('fo:text-align');
  sdxODSAttrFOWrapOption = AnsiString('fo:wrap-option');
  sdxODSAttrNumberCurrencySymbol = AnsiString('number:currency-symbol');
  sdxODSAttrNumberDecimalPlaces = AnsiString('number:decimal-places');
  sdxODSAttrNumberDecimalReplacement = AnsiString('number:decimal-replacement');
  sdxODSAttrNumberDisplayFactor = AnsiString('number:display-factor');
  sdxODSAttrNumberHours = AnsiString('number:hours');
  sdxODSAttrNumberMinIntegerDigits = AnsiString('number:min-integer-digits');
  sdxODSAttrNumberMinutes = AnsiString('number:minutes');
  sdxODSAttrNumberSeconds = AnsiString('number:seconds');
  sdxODSAttrNumberStyle = AnsiString('number:style');
  sdxODSAttrNumberTextual = AnsiString('number:textual');
  sdxODSAttrOfficeBooleanValue = AnsiString('office:boolean-value');
  sdxODSAttrOfficeCurrency = AnsiString('office:currency');
  sdxODSAttrOfficeDateValue = AnsiString('office:date-value');
  sdxODSAttrOfficeDisplay = AnsiString('office:display');
  sdxODSAttrOfficeStringValue = AnsiString('office:string-value');
  sdxODSAttrOfficeTimeValue = AnsiString('office:time-value');
  sdxODSAttrOfficeValue = AnsiString('office:value');
  sdxODSAttrOfficeValueType = AnsiString('office:value-type');
  sdxODSAttrStyleApplyStyleName = AnsiString('style:apply-style-name');
  sdxODSAttrStyleCellProtect = AnsiString('style:cell-protect');
  sdxODSAttrStyleColumnWidth = AnsiString('style:column-width');
  sdxODSAttrStyleCondition = AnsiString('style:condition');
  sdxODSAttrStyleDataStyleName = AnsiString('style:data-style-name');
  sdxODSAttrStyleDisplay = AnsiString('style:display');
  sdxODSAttrStyleDisplayName = AnsiString('style:display-name');
  sdxODSAttrStyleFirstPageNumber = AnsiString('style:first-page-number');
  sdxODSAttrStyleFontCharset = AnsiString('style:font-charset');
  sdxODSAttrStyleFontName = AnsiString('style:font-name');
  sdxODSAttrStyleFontPitch = AnsiString('style:font-pitch');
  sdxODSAttrStyleMasterPageName = AnsiString('style:master-page-name');
  sdxODSAttrStyleName = AnsiString('style:name');
  sdxODSAttrStylePageLayoutName = AnsiString('style:page-layout-name');
  sdxODSAttrStyleParentStyleName = AnsiString('style:parent-style-name');
  sdxODSAttrStylePrint = AnsiString('style:print');
  sdxODSAttrStylePrintOrientation = AnsiString('style:print-orientation');
  sdxODSAttrStylePrintPageOrder = AnsiString('style:print-page-order');
  sdxODSAttrStyleProtect = AnsiString('style:protect');
  sdxODSAttrStyleRowAutoHeight = AnsiString('style:use-optimal-row-height');
  sdxODSAttrStyleRowHeight = AnsiString('style:row-height');
  sdxODSAttrStyleScaleTo = AnsiString('style:scale-to');
  sdxODSAttrStyleScaleToPages = AnsiString('style:scale-to-pages');
  sdxODSAttrStyleShrinkToFit = AnsiString('style:shrink-to-fit');
  sdxODSAttrStyleTableCentering = AnsiString('style:table-centering');
  sdxODSAttrStyleTextLineThroughStyle = AnsiString('style:text-line-through-style');
  sdxODSAttrStyleTextUnderlineStyle = AnsiString('style:text-underline-style');
  sdxODSAttrStyleVerticalAlign = AnsiString('style:vertical-align');
  sdxODSAttrSvgHeight = AnsiString('svg:height');
  sdxODSAttrSvgStrokeColor = AnsiString('svg:stroke-color');
  sdxODSAttrSvgStrokeOpacity = AnsiString('svg:stroke-opacity');
  sdxODSAttrSvgStrokeWidth = AnsiString('svg:stroke-width');
  sdxODSAttrSvgWidth = AnsiString('svg:width');
  sdxODSAttrSvgX = AnsiString('svg:x');
  sdxODSAttrSvgY = AnsiString('svg:y');
  sdxODSAttrTableCellRangeAddress = AnsiString('table:cell-range-address');
  sdxODSAttrTableDateValue = AnsiString('table:date-value');
  sdxODSAttrTableDefaultCellStyleName = AnsiString('table:default-cell-style-name');
  sdxODSAttrTableDisplay =  AnsiString('table:display');
  sdxODSAttrTableEndCellAddress = AnsiString('table:end-cell-address');
  sdxODSAttrTableEndX = AnsiString('table:end-x');
  sdxODSAttrTableEndY = AnsiString('table:end-y');
  sdxODSAttrTableExpression = AnsiString('table:expression');
  sdxODSAttrTableFormula = AnsiString('table:formula');
  sdxODSAttrTableName = AnsiString('table:name');
  sdxODSAttrTableNumberColumnsRepeated = AnsiString('table:number-columns-repeated');
  sdxODSAttrTableNumberColumnsSpanned = AnsiString('table:number-columns-spanned');
  sdxODSAttrTableNumberMatrixColumnsSpanned = AnsiString('table:number-matrix-columns-spanned');
  sdxODSAttrTableNumberMatrixRowsSpanned = AnsiString('table:number-matrix-rows-spanned');
  sdxODSAttrTableNumberRowsRepeated = AnsiString('table:number-rows-repeated');
  sdxODSAttrTableNumberRowsSpanned = AnsiString('table:number-rows-spanned');
  sdxODSAttrTablePrintRanges = AnsiString('table:print-ranges');
  sdxODSAttrTableProtected = AnsiString('table:protected');
  sdxODSAttrTableStatus = AnsiString('table:status');
  sdxODSAttrTableSteps = AnsiString('table:steps');
  sdxODSAttrTableStructureProtected = AnsiString('table:structure-protected');
  sdxODSAttrTableStyleName = AnsiString('table:style-name');
  sdxODSAttrTableTargetRangeAddress = AnsiString('table:target-range-address');
  sdxODSAttrTableVisibility = AnsiString('table:visibility');
  sdxODSAttrTextStyleName = AnsiString('text:style-name');
  sdxODSAttrVersion = AnsiString('office:version');
  sdxODSAttrXLinkHRef = AnsiString('xlink:href');

  sdxODSValueBorderSizeMedium = AnsiString('medium');
  sdxODSValueBorderSizeThick = AnsiString('thick');
  sdxODSValueBorderSizeThin = AnsiString('thin');
  sdxODSValueBorderStyleDashed = AnsiString('dashed');
  sdxODSValueBorderStyleDotted = AnsiString('dotted');
  sdxODSValueBorderStyleDouble = AnsiString('double');
  sdxODSValueBorderStyleHidden = AnsiString('hidden');
  sdxODSValueBorderStyleNone = AnsiString('none');
  sdxODSValueBorderStyleSolid = AnsiString('solid');
  sdxODSValueCellProtectionFormulaHidden = AnsiString('formula-hidden');
  sdxODSValueCellProtectionHiddenAndProtected = AnsiString('hidden-and-protected');
  sdxODSValueCellProtectionProtected = AnsiString('protected');
  sdxODSValueColorTransparent = AnsiString('transparent');
  sdxODSValueDrawStrokeDash = AnsiString('dash');
  sdxODSValueDrawStrokeNone = AnsiString('none');
  sdxODSValueDrawStrokeSolid = AnsiString('solid');
  sdxODSValueDrawStyleDouble = AnsiString('double');
  sdxODSValueDrawStyleTriple = AnsiString('triple');
  sdxODSValueFillBitmap = AnsiString('bitmap');
  sdxODSValueFillGradient = AnsiString('gradient');
  sdxODSValueFillHatch = AnsiString('hatch');
  sdxODSValueFillNone = AnsiString('none');
  sdxODSValueFillSolid = AnsiString('solid');
  sdxODSValueFontPitchFixed = AnsiString('fixed');
  sdxODSValueFontPitchVariable = AnsiString('variable');
  sdxODSValueFontStyleItalic = AnsiString('italic');
  sdxODSValueFontWeightBold = AnsiString('bold');
  sdxODSValueFontWeightBolder = AnsiString('bolder');
  sdxODSValueNumberStyleLong = AnsiString('long');
  sdxODSValueNumberStyleShort = AnsiString('short');
  sdxODSValuePrintArea = AnsiString('Print_Area');
  sdxODSValuePrintArea2 = AnsiString('_xlnm.Print_Area');
  sdxODSValueStyleDefault = AnsiString('Default');
  sdxODSValueStyleProtectPosition = AnsiString('position');
  sdxODSValueStyleProtectSize = AnsiString('size');
  sdxODSValueTableVisibilityHidden = AnsiString('collapse');
  sdxODSValueTableVisibilityVisible = AnsiString('visible');
  sdxODSValueTextAlignCenter = AnsiString('center');
  sdxODSValueTextAlignEnd = AnsiString('end');
  sdxODSValueTextAlignInside = AnsiString('inside');
  sdxODSValueTextAlignJustify = AnsiString('justify');
  sdxODSValueTextAlignLeft = AnsiString('left');
  sdxODSValueTextAlignOutside = AnsiString('outside');
  sdxODSValueTextAlignRight = AnsiString('right');
  sdxODSValueTextAlignStart = AnsiString('start');
  sdxODSValueTypeFormula = AnsiString('formula');
  sdxODSValueTypeMaximum = AnsiString('maximum');
  sdxODSValueTypeMinimum = AnsiString('minimum');
  sdxODSValueTypeNumber = AnsiString('number');
  sdxODSValueTypePercent = AnsiString('percent');
  sdxODSValueTypePercentile = AnsiString('percentile');
  sdxODSValueValueTypeBoolean = AnsiString('boolean');
  sdxODSValueValueTypeCurrency = AnsiString('currency');
  sdxODSValueValueTypeDate = AnsiString('date');
  sdxODSValueValueTypeFloat = AnsiString('float');
  sdxODSValueValueTypePercentage = AnsiString('percentage');
  sdxODSValueValueTypeString = AnsiString('string');
  sdxODSValueValueTypeTime = AnsiString('time');
  sdxODSValueVerticalAlignAutomatic = AnsiString('automatic');
  sdxODSValueVerticalAlignBottom = AnsiString('bottom');
  sdxODSValueVerticalAlignMiddle = AnsiString('middle');
  sdxODSValueVerticalAlignTop = AnsiString('top');

  sdxODSValueConditionAbove = 'value()>0';
  sdxODSValueConditionAboveOrEqual = 'value()>=0';
  sdxODSValueConditionBelow = 'value()<0';
  sdxODSValueConditionBelowOrEqual = 'value()<=0';
  sdxODSValueConditionEqual = 'value()=0';

  sdxODSValueConfigActiveTable = AnsiString('ActiveTable');
  sdxODSValueConfigCursorPositionX = AnsiString('CursorPositionX');
  sdxODSValueConfigCursorPositionY = AnsiString('CursorPositionY');
  sdxODSValueConfigHasColumnRowHeaders = AnsiString('HasColumnRowHeaders');
  sdxODSValueConfigHasSheetTabs = AnsiString('HasSheetTabs');
  sdxODSValueConfigHorizontalSplitPosition = AnsiString('HorizontalSplitPosition');
  sdxODSValueConfigPositionBottom = AnsiString('PositionBottom');
  sdxODSValueConfigPositionLeft = AnsiString('PositionLeft');
  sdxODSValueConfigPositionRight = AnsiString('PositionRight');
  sdxODSValueConfigPositionTop = AnsiString('PositionTop');
  sdxODSValueConfigShowGrid = AnsiString('ShowGrid');
  sdxODSValueConfigShowZeroValues = AnsiString('ShowZeroValues');
  sdxODSValueConfigTables = AnsiString('Tables');
  sdxODSValueConfigVerticalSplitPosition = AnsiString('VerticalSplitPosition');
  sdxODSValueConfigViews = AnsiString('Views');
  sdxODSValueConfigViewSettings = AnsiString('ooo:view-settings');
  sdxODSValueConfigZoomValue = AnsiString('ZoomValue');

  sdxODSValueEnable = AnsiString('enable');
  sdxODSValueNullDate1904 = AnsiString('1904-01-01');

  sdxODSValuePrintObjectCharts = 'charts';
  sdxODSValuePrintObjectDrawings = 'drawing';
  sdxODSValuePrintObjectFormulas = 'formulas';
  sdxODSValuePrintObjectGridLines = 'grid';
  sdxODSValuePrintObjectHeaders = 'headers';
  sdxODSValuePrintObjectObjects = 'objects';
  sdxODSValuePrintObjectZeroValues = 'zero-values';

  sdxODSValuePrintOrientationLandscape = 'landscape';
  sdxODSValuePrintOrientationPortrait = 'portrait';

  sdxODSValuePrintPageOrderOverThenDown = AnsiString('ltr');
  sdxODSValuePrintPageOrderDownThenOver = AnsiString('ttb');

  sdxODSTableCenteringBoth = AnsiString('both');
  sdxODSTableCenteringHorz = AnsiString('horizontal');
  sdxODSTableCenteringNone = AnsiString('none');
  sdxODSTableCenteringVert = AnsiString('vertical');

implementation

end.
