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

unit dxSpreadSheetCoreDialogsStrs;

{$I cxVer.Inc}

interface

uses
  dxCore, dxSpreadSheetGraphics, Graphics, dxSpreadSheetNumberFormat;

resourcestring
  sdxFormatCellsDialogAuto = 'Automatic';
  sdxFormatCellsDialogButtonCancel = 'Cancel';
  sdxFormatCellsDialogButtonColorAuto = 'Auto';
  sdxFormatCellsDialogButtonOK = 'OK';
  sdxFormatCellsDialogCaption = 'Format Cells';
  sdxFormatCellsDialogNone = 'None';
  sdxFormatCellsDialogSample = 'Sample';

  // Number Page
  sdxFormatCellsDialogGroupNumber = 'Number';
  sdxFormatCellsDialogCategory = '&Category:';
  sdxFormatCellsDialogCategoryAccounting = 'Accounting';
  sdxFormatCellsDialogCategoryAccountingDescription =
    'Accounting formats line up the currency symbols and decimal points in a column.';
  sdxFormatCellsDialogCategoryCurrency = 'Currency';
  sdxFormatCellsDialogCategoryCurrencyDescription =
    'Currency formats are used for general monetary values. ' +
    'Use Accounting formats to align decimal points in a column.';
  sdxFormatCellsDialogCategoryCustom = 'Custom';
  sdxFormatCellsDialogCategoryCustomDescription =
    'Type the number format code, using one of the existing codes as a starting point.';
  sdxFormatCellsDialogCategoryDate = 'Date';
  sdxFormatCellsDialogCategoryDateDescription =
    'Date formats display date and time serial numbers as date values.';
  sdxFormatCellsDialogCategoryDateNote =
    'Date formats that begin with an asterisk (*) respond to changes in regional date and time settings that are specified for the operation system.';
  sdxFormatCellsDialogCategoryFraction = 'Fraction';
  sdxFormatCellsDialogCategoryGeneral = 'General';
  sdxFormatCellsDialogCategoryGeneralNotes = 'General format cells have no specific number format.';
  sdxFormatCellsDialogCategoryNumber = 'Number';
  sdxFormatCellsDialogCategoryNumberDescription =
    'Number is used for general display of numbers. ' +
    'Currency and Accounting offer specialized formatting for monetary values.';
  sdxFormatCellsDialogCategoryPercentage = 'Percentage';
  sdxFormatCellsDialogCategoryPercentageDescription =
    'Percentage formats multiply the cell value by 100 and display the result with a percent symbol.';
  sdxFormatCellsDialogCategoryScientific = 'Scientific';
  sdxFormatCellsDialogCategoryText = 'Text';
  sdxFormatCellsDialogCategoryTextNotes =
    'Text format cells are treated as text even when a number '+
    'is in the cell. The cell is displayed exactly as entered.';
  sdxFormatCellsDialogCategoryTime = 'Time';
  sdxFormatCellsDialogCategoryTimeDescription =
    'Time formats display date and time serial numbers as date values.';
  sdxFormatCellsDialogCustomCode = '&Type:';
  sdxFormatCellsDialogDecimalPlaces = '&Decimal places:';
  sdxFormatCellsDialogNumberFormatTemplates = '&Type:';
  sdxFormatCellsDialogUseThousandSeparator = '&Use 1000 Separator (%s)';

  // Alignment Page
  sdxFormatCellsDialogGroupTextAlignment = 'Alignment';
  sdxFormatCellsDialogMergeCells = '&Merge cells';
  sdxFormatCellsDialogShrinkToFit = 'Shrin&k to fit';
  sdxFormatCellsDialogTextAlignHorz = '&Horizontal:';
  sdxFormatCellsDialogTextAlignHorzIndent = '&Indent:';
  sdxFormatCellsDialogTextAlignment = 'Text alignment';
  sdxFormatCellsDialogTextAlignVert = '&Vertical:';
  sdxFormatCellsDialogTextControl = 'Text control';
  sdxFormatCellsDialogWrapText = '&Wrap text';

  // Font Page
  sdxFormatCellsDialogFont = 'Font';
  sdxFormatCellsDialogButtonResetFont = '&Reset';
  sdxFormatCellsDialogFontColor = '&Color:';
  sdxFormatCellsDialogFontName = '&Font:';
  sdxFormatCellsDialogFontNotInstalled =
    'This font is not installed on the system. The closest available font will be used for printing.';
  sdxFormatCellsDialogFontPreview = 'Preview';
  sdxFormatCellsDialogFontPrintNotes =
    'This is a TrueType font. The same font will be used on both your printer and your screen.';
  sdxFormatCellsDialogFontSize = '&Size:';
  sdxFormatCellsDialogFontStyle = 'F&ont style:';
  sdxFormatCellsDialogFontStrikethrough = 'Stri&kethrough';
  sdxFormatCellsDialogFontUnderline = '&Underline:';
  sdxFormatCellsDialogGroupFontEffects = 'Effects';
  sdxFormatCellsDialogUnderlineNone = 'None';
  sdxFormatCellsDialogUnderlineSingle = 'Single';

  // Border Page
  sdxFormatCellsDialogBorder = 'Border';
  sdxFormatCellsDialogBorderInside = '&Inside';
  sdxFormatCellsDialogBorderLine = 'Line';
  sdxFormatCellsDialogBorderLineColor = '&Color:';
  sdxFormatCellsDialogBorderLineStyle = '&Style:';
  sdxFormatCellsDialogBorderNone = '&None';
  sdxFormatCellsDialogBorderOutline = '&Outline';
  sdxFormatCellsDialogBorderPresets = 'Presets';
  sdxFormatCellsDialogBordersHint =
    'The selected border style can be applied by clicking the presets, preview diagram or the buttons above.';
  sdxFormatCellsDialogPreviewText = 'Text';

  // Fill Page
  sdxFormatCellsDialogBackgroundColor = 'Background &color:';
  sdxFormatCellsDialogFill = 'Fill';
  sdxFormatCellsDialogFillSample = 'Sample';
  sdxFormatCellsDialogMoreColors = '&More colors...';
  sdxFormatCellsDialogNoColor = 'No color';
  sdxFormatCellsDialogPatternColor = 'P&attern color:';
  sdxFormatCellsDialogPatternStyle = '&Pattern style:';

  // Protection Page
  sdxFormatCellsDialogHidden = 'H&idden';
  sdxFormatCellsDialogLocked = '&Locked';
  sdxFormatCellsDialogProtection = 'Protection';
  sdxFormatCellsDialogProtectionNotes =
    'Locking cells or hiding formulas has no effect until you protect the worksheet.';

  // Font Style
  sdxFontStyleBold = 'Bold';
  sdxFontStyleBoldItalic = 'Bold Italic';
  sdxFontStyleItalic = 'Italic';
  sdxFontStyleRegular = 'Regular';
  sdxFontStyleStrikeOut = 'Strike Out';
  sdxFontStyleUnderline = 'Underline';

  // Horizontal Alignment
  sdxHorzAlignCenter = 'Center';
  sdxHorzAlignDistributed = 'Distributed (Indent)';
  sdxHorzAlignFill = 'Fill';
  sdxHorzAlignGeneral = 'General';
  sdxHorzAlignJustify = 'Justify';
  sdxHorzAlignLeft = 'Left (Indent)';
  sdxHorzAlignRight = 'Right (Indent)';

  // Vertical Alignment
  sdxVertAlignBottom = 'Bottom';
  sdxVertAlignCenter = 'Center';
  sdxVertAlignDistributed = 'Distributed';
  sdxVertAlignJustify = 'Justify';
  sdxVertAlignTop = 'Top';

  // Cell Fill Styles
  sdxCellFillStyleSolid = 'Solid';
  sdxCellFillStyleGray75 = 'Gray 75%';
  sdxCellFillStyleGray50 = 'Gray 50%';
  sdxCellFillStyleGray25 = 'Gray 25%';
  sdxCellFillStyleGray12 = 'Gray 12%';
  sdxCellFillStyleGray6 = 'Gray 6%';
  sdxCellFillStyleHorzStrip = 'Horizontal Strip';
  sdxCellFillStyleVertStrip = 'Vertical Strip';
  sdxCellFillStyleRevDiagonalStrip = 'Reverse Diagonal Strip';
  sdxCellFillStyleDiagonalStrip = 'Diagonal Strip';
  sdxCellFillStyleDiagCrossHatch = 'Diagonal Crosshatch';
  sdxCellFillStyleThickDiagonalCrossHatch = 'Thick Diagonal Crosshatch';
  sdxCellFillStyleThinHorzStrip = 'Thin Horizontal Strip';
  sdxCellFillStyleThinVertStrip = 'Thin Vertical Strip';
  sdxCellFillStyleThinRevDiagonalStrip = 'Thin Reverse Diagonal Strip';
  sdxCellFillStyleThinDiagonalStrip = 'Thin Diagonal Strip';
  sdxCellFillStyleThinDiagCrossHatch = 'Thin Diagonal Crosshatch';
  sdxCellFillStyleThinHorzCrossHatch = 'Thin Horizontal Crosshatch';

  // Conditional Formatting Rules Manager Dialog
  sdxConditionalFormattingRulesManagerDialogCaption = 'Conditional Formatting Rules Manager';
  sdxConditionalFormattingRulesManagerDialogDisplayMode = '&Show formatting rules for:';
  sdxConditionalFormattingRulesManagerDialogDisplayModeSelectedArea = 'Current Selection';
  sdxConditionalFormattingRulesManagerDialogDisplayModeSheet = 'Sheet';
  sdxConditionalFormattingRulesManagerDialogDisplayModeAll = 'All';
  sdxConditionalFormattingRulesManagerDialogActionClone = '&Clone Rule';
  sdxConditionalFormattingRulesManagerDialogActionCreate = '&New Rule...';
  sdxConditionalFormattingRulesManagerDialogActionDelete = '&Delete Rule';
  sdxConditionalFormattingRulesManagerDialogActionEdit = '&Edit Rule...';
  sdxConditionalFormattingRulesManagerDialogButtonApply = '&Apply';
  sdxConditionalFormattingRulesManagerDialogButtonCancel = 'Cancel';
  sdxConditionalFormattingRulesManagerDialogButtonOk = 'OK';
  sdxConditionalFormattingRulesManagerDialogColumnApplyToTheRecord = 'Apply to the record';
  sdxConditionalFormattingRulesManagerDialogColumnArea = 'Applies to';
  sdxConditionalFormattingRulesManagerDialogColumnFormat = 'Format';
  sdxConditionalFormattingRulesManagerDialogColumnName = 'Rule (applied in order shown)';
  sdxConditionalFormattingRulesManagerDialogColumnStopIfTrue = 'Stop If True';

  // Conditional Formatting DataBar Rule Style Edit Dialog
  sdxConditionalFormattingDataBarRuleStyleEditDialogAxis = 'Axis Settings';
  sdxConditionalFormattingDataBarRuleStyleEditDialogAxisAuto = 'A&utomatic (display at variable position based on negative values)';
  sdxConditionalFormattingDataBarRuleStyleEditDialogAxisColor = 'A&xis color:';
  sdxConditionalFormattingDataBarRuleStyleEditDialogAxisMidpoint = 'Cell &midpoint';
  sdxConditionalFormattingDataBarRuleStyleEditDialogAxisNone = 'Non&e (show negative value bars in same direction as positive)';
  sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirection = 'Bar &direction:';
  sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirectionAuto = 'Context';
  sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirectionLeftToRight = 'Left To Right';
  sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirectionRightToLeft = 'Right To Left';
  sdxConditionalFormattingDataBarRuleStyleEditDialogBordersNone = 'None';
  sdxConditionalFormattingDataBarRuleStyleEditDialogBordersSolid = 'Solid';
  sdxConditionalFormattingDataBarRuleStyleEditDialogButtonCancel = 'Cancel';
  sdxConditionalFormattingDataBarRuleStyleEditDialogButtonOk = 'OK';
  sdxConditionalFormattingDataBarRuleStyleEditDialogCaption = 'Data Bar Rule Style Settings';
  sdxConditionalFormattingDataBarRuleStyleEditDialogCommon = 'Common';
  sdxConditionalFormattingDataBarRuleStyleEditDialogFillMode = '&Fill mode:';
  sdxConditionalFormattingDataBarRuleStyleEditDialogFillModeGradient = 'Gradient';
  sdxConditionalFormattingDataBarRuleStyleEditDialogFillModeSolid = 'Solid';
  sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBar = 'Negative Bar Appearance';
  sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarBorderColor = 'Bo&rder color:';
  sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarBorderColorAuto = 'A&pply same border color as positive bar';
  sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarColor = 'Co&lor:';
  sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarColorAuto = '&Apply same fill color as positive bar';
  sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBar = 'Positive Bar Appearance';
  sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBarBorderColor = 'Bor&der color:';
  sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBarBorderStyle = '&Border style:';
  sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBarColor = '&Color:';

  // Conditional Formatting Rule Edit Dialog
  sdxConditionalFormattingRuleEditDialogAnd = 'and';
  sdxConditionalFormattingRuleEditDialogButtonCancel = 'Cancel';
  sdxConditionalFormattingRuleEditDialogButtonFormat = '&Format...';
  sdxConditionalFormattingRuleEditDialogButtonOk = 'OK';
  sdxConditionalFormattingRuleEditDialogCaption = 'Conditional Formatting Rule';
  sdxConditionalFormattingRuleEditDialogErrorIncorrectStopsOrder = 'The order of stops is incorrect';
  sdxConditionalFormattingRuleEditDialogErrorInvalidExpression = 'One or more expressions are incorrect';
  sdxConditionalFormattingRuleEditDialogIconStyle = 'I&con Style:';
  sdxConditionalFormattingRuleEditDialogPercentsOfSelectedRange = '% of selected range';
  sdxConditionalFormattingRuleEditDialogPreview = 'Preview:';
  sdxConditionalFormattingRuleEditDialogReverseIconOrder = 'Reverse Icon Or&der';
  sdxConditionalFormattingRuleEditDialogRuleDescriptionAboveOrBelowAverage = 'Format values that are:';
  sdxConditionalFormattingRuleEditDialogRuleDescriptionCellIs = 'Format only cells with value:';
  sdxConditionalFormattingRuleEditDialogRuleDescriptionExpression = 'Format values where this formula is true:';
  sdxConditionalFormattingRuleEditDialogRuleDescriptionIconSet = 'Display each icon according to these rules:';
  sdxConditionalFormattingRuleEditDialogRuleDescriptionTopBottomValues = 'Format values that rank in the:';
  sdxConditionalFormattingRuleEditDialogRuleNameAboveOrBelowAverage = 'Format only values that are above or below average';
  sdxConditionalFormattingRuleEditDialogRuleNameCellIs = 'Format only cells that contain';
  sdxConditionalFormattingRuleEditDialogRuleNameDataBar = 'Format all cells based on their values via data bar';
  sdxConditionalFormattingRuleEditDialogRuleNameDuplicateValues = 'Format only duplicate values';
  sdxConditionalFormattingRuleEditDialogRuleNameExpression = 'Use a formula to determine which cells to format';
  sdxConditionalFormattingRuleEditDialogRuleNameIconSet = 'Format all cells based on their values via icon set';
  sdxConditionalFormattingRuleEditDialogRuleNameThreeColorScale = 'Format all cells based on their values via three color scale';
  sdxConditionalFormattingRuleEditDialogRuleNameTopBottomValues = 'Format only top or bottom ranked values';
  sdxConditionalFormattingRuleEditDialogRuleNameTwoColorScale = 'Format all cells based on their values via two color scale';
  sdxConditionalFormattingRuleEditDialogRuleNameUniqueValues = 'Format only unique values';
  sdxConditionalFormattingRuleEditDialogRuleType = '&Select a Rule Type:';
  sdxConditionalFormattingRuleEditDialogScaleMaxStop = 'Maximum';
  sdxConditionalFormattingRuleEditDialogScaleMidStop = 'Midpoint';
  sdxConditionalFormattingRuleEditDialogScaleMinStop = 'Minimum';
  sdxConditionalFormattingRuleEditDialogScaleValueTypeFormula = 'Formula';
  sdxConditionalFormattingRuleEditDialogScaleValueTypeHighestValue = 'Highest Value';
  sdxConditionalFormattingRuleEditDialogScaleValueTypeLowestValue = 'Lowest Value';
  sdxConditionalFormattingRuleEditDialogScaleValueTypePercent = 'Percent';
  sdxConditionalFormattingRuleEditDialogScaleValueTypePercentile = 'Percentile';
  sdxConditionalFormattingRuleEditDialogScaleValueTypeValue = 'Value';
  sdxConditionalFormattingRuleEditDialogShowIconOnly = 'Show &Icon Only';
  sdxConditionalFormattingRuleEditDialogShowBarOnly = 'Show &Bar Only';
  sdxConditionalFormattingRuleEditDialogWhenValueIs = 'when value is';
  sdxConditionalFormattingRuleEditDialogWhenValueLess = 'when value < "%s"';
  sdxConditionalFormattingRuleEditDialogWhenValueLessAnd = 'when value < "%s" and';
  sdxConditionalFormattingRuleEditDialogWhenValueLessOrEqual = 'when value <= "%s"';
  sdxConditionalFormattingRuleEditDialogWhenValueLessOrEqualAnd = 'when value <= "%s" and';

const
  dxCellFillStyleNames: array[TdxSpreadSheetCellFillStyle] of Pointer = (
    @sdxCellFillStyleSolid, @sdxCellFillStyleGray75, @sdxCellFillStyleGray50, @sdxCellFillStyleGray25,
    @sdxCellFillStyleGray12, @sdxCellFillStyleGray6, @sdxCellFillStyleHorzStrip, @sdxCellFillStyleVertStrip,
    @sdxCellFillStyleRevDiagonalStrip, @sdxCellFillStyleDiagonalStrip, @sdxCellFillStyleDiagCrossHatch,
    @sdxCellFillStyleThickDiagonalCrossHatch, @sdxCellFillStyleThinHorzStrip, @sdxCellFillStyleThinVertStrip,
    @sdxCellFillStyleThinRevDiagonalStrip, @sdxCellFillStyleThinDiagonalStrip, @sdxCellFillStyleThinDiagCrossHatch,
    @sdxCellFillStyleThinHorzCrossHatch
  );

  dxFontStyleNames: array[TFontStyle] of Pointer = (
    @sdxFontStyleBold, @sdxFontStyleItalic, @sdxFontStyleUnderline, @sdxFontStyleStrikeOut
  );

  dxSpreadSheetDataHorzAlignNames: array [TdxSpreadSheetDataAlignHorz] of Pointer = (
    @sdxHorzAlignGeneral, @sdxHorzAlignLeft, @sdxHorzAlignCenter, @sdxHorzAlignRight, @sdxHorzAlignFill,
    @sdxHorzAlignJustify, @sdxHorzAlignDistributed
  );

  dxSpreadSheetDataVertAlignNames: array [TdxSpreadSheetDataAlignVert] of Pointer = (
    @sdxVertAlignTop, @sdxVertAlignCenter, @sdxVertAlignBottom, @sdxVertAlignJustify, @sdxVertAlignDistributed
  );

  dxSpreadSheetNumberFormatCategoryNames: array[TdxSpreadSheetNumberFormatCategory] of Pointer = (
    @sdxFormatCellsDialogCategoryGeneral, @sdxFormatCellsDialogCategoryNumber, @sdxFormatCellsDialogCategoryCurrency,
    @sdxFormatCellsDialogCategoryAccounting, @sdxFormatCellsDialogCategoryDate, @sdxFormatCellsDialogCategoryTime,
    @sdxFormatCellsDialogCategoryPercentage, @sdxFormatCellsDialogCategoryFraction, @sdxFormatCellsDialogCategoryScientific,
    @sdxFormatCellsDialogCategoryText, @sdxFormatCellsDialogCategoryCustom
  );

implementation

procedure AddSpreadSheetDialogsResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxCellFillStyleDiagCrossHatch', @sdxCellFillStyleDiagCrossHatch);
  AProduct.Add('sdxCellFillStyleDiagonalStrip', @sdxCellFillStyleDiagonalStrip);
  AProduct.Add('sdxCellFillStyleGray12', @sdxCellFillStyleGray12);
  AProduct.Add('sdxCellFillStyleGray25', @sdxCellFillStyleGray25);
  AProduct.Add('sdxCellFillStyleGray50', @sdxCellFillStyleGray50);
  AProduct.Add('sdxCellFillStyleGray6', @sdxCellFillStyleGray6);
  AProduct.Add('sdxCellFillStyleGray75', @sdxCellFillStyleGray75);
  AProduct.Add('sdxCellFillStyleHorzStrip', @sdxCellFillStyleHorzStrip);
  AProduct.Add('sdxCellFillStyleRevDiagonalStrip', @sdxCellFillStyleRevDiagonalStrip);
  AProduct.Add('sdxCellFillStyleSolid', @sdxCellFillStyleSolid);
  AProduct.Add('sdxCellFillStyleThickDiagonalCrossHatch', @sdxCellFillStyleThickDiagonalCrossHatch);
  AProduct.Add('sdxCellFillStyleThinDiagCrossHatch', @sdxCellFillStyleThinDiagCrossHatch);
  AProduct.Add('sdxCellFillStyleThinDiagonalStrip', @sdxCellFillStyleThinDiagonalStrip);
  AProduct.Add('sdxCellFillStyleThinHorzCrossHatch', @sdxCellFillStyleThinHorzCrossHatch);
  AProduct.Add('sdxCellFillStyleThinHorzStrip', @sdxCellFillStyleThinHorzStrip);
  AProduct.Add('sdxCellFillStyleThinRevDiagonalStrip', @sdxCellFillStyleThinRevDiagonalStrip);
  AProduct.Add('sdxCellFillStyleThinVertStrip', @sdxCellFillStyleThinVertStrip);
  AProduct.Add('sdxCellFillStyleVertStrip', @sdxCellFillStyleVertStrip);
  AProduct.Add('sdxFontStyleBold', @sdxFontStyleBold);
  AProduct.Add('sdxFontStyleBoldItalic', @sdxFontStyleBoldItalic);
  AProduct.Add('sdxFontStyleItalic', @sdxFontStyleItalic);
  AProduct.Add('sdxFontStyleRegular', @sdxFontStyleRegular);
  AProduct.Add('sdxFontStyleStrikeOut', @sdxFontStyleStrikeOut);
  AProduct.Add('sdxFontStyleUnderline', @sdxFontStyleUnderline);
  AProduct.Add('sdxFormatCellsDialogAuto', @sdxFormatCellsDialogAuto);
  AProduct.Add('sdxFormatCellsDialogBackgroundColor', @sdxFormatCellsDialogBackgroundColor);
  AProduct.Add('sdxFormatCellsDialogBorder', @sdxFormatCellsDialogBorder);
  AProduct.Add('sdxFormatCellsDialogBorderInside', @sdxFormatCellsDialogBorderInside);
  AProduct.Add('sdxFormatCellsDialogBorderLine', @sdxFormatCellsDialogBorderLine);
  AProduct.Add('sdxFormatCellsDialogBorderLineColor', @sdxFormatCellsDialogBorderLineColor);
  AProduct.Add('sdxFormatCellsDialogBorderLineStyle', @sdxFormatCellsDialogBorderLineStyle);
  AProduct.Add('sdxFormatCellsDialogBorderNone', @sdxFormatCellsDialogBorderNone);
  AProduct.Add('sdxFormatCellsDialogBorderOutline', @sdxFormatCellsDialogBorderOutline);
  AProduct.Add('sdxFormatCellsDialogBorderPresets', @sdxFormatCellsDialogBorderPresets);
  AProduct.Add('sdxFormatCellsDialogBordersHint', @sdxFormatCellsDialogBordersHint);
  AProduct.Add('sdxFormatCellsDialogButtonCancel', @sdxFormatCellsDialogButtonCancel);
  AProduct.Add('sdxFormatCellsDialogButtonColorAuto', @sdxFormatCellsDialogButtonColorAuto);
  AProduct.Add('sdxFormatCellsDialogButtonOK', @sdxFormatCellsDialogButtonOK);
  AProduct.Add('sdxFormatCellsDialogFont', @sdxFormatCellsDialogFont);
  AProduct.Add('sdxFormatCellsDialogButtonResetFont', @sdxFormatCellsDialogButtonResetFont);
  AProduct.Add('sdxFormatCellsDialogCaption', @sdxFormatCellsDialogCaption);
  AProduct.Add('sdxFormatCellsDialogCategory', @sdxFormatCellsDialogCategory);
  AProduct.Add('sdxFormatCellsDialogCategoryAccounting', @sdxFormatCellsDialogCategoryAccounting);
  AProduct.Add('sdxFormatCellsDialogCategoryAccountingDescription', @sdxFormatCellsDialogCategoryAccountingDescription);
  AProduct.Add('sdxFormatCellsDialogCategoryCurrency', @sdxFormatCellsDialogCategoryCurrency);
  AProduct.Add('sdxFormatCellsDialogCategoryCurrencyDescription', @sdxFormatCellsDialogCategoryCurrencyDescription);
  AProduct.Add('sdxFormatCellsDialogCategoryCustom', @sdxFormatCellsDialogCategoryCustom);
  AProduct.Add('sdxFormatCellsDialogCategoryCustomDescription', @sdxFormatCellsDialogCategoryCustomDescription);
  AProduct.Add('sdxFormatCellsDialogCategoryDate', @sdxFormatCellsDialogCategoryDate);
  AProduct.Add('sdxFormatCellsDialogCategoryDateDescription', @sdxFormatCellsDialogCategoryDateDescription);
  AProduct.Add('sdxFormatCellsDialogCategoryDateNote', @sdxFormatCellsDialogCategoryDateNote);
  AProduct.Add('sdxFormatCellsDialogCategoryFraction', @sdxFormatCellsDialogCategoryFraction);
  AProduct.Add('sdxFormatCellsDialogCategoryGeneral', @sdxFormatCellsDialogCategoryGeneral);
  AProduct.Add('sdxFormatCellsDialogCategoryGeneralNotes', @sdxFormatCellsDialogCategoryGeneralNotes);
  AProduct.Add('sdxFormatCellsDialogCategoryNumber', @sdxFormatCellsDialogCategoryNumber);
  AProduct.Add('sdxFormatCellsDialogCategoryNumberDescription', @sdxFormatCellsDialogCategoryNumberDescription);
  AProduct.Add('sdxFormatCellsDialogCategoryPercentage', @sdxFormatCellsDialogCategoryPercentage);
  AProduct.Add('sdxFormatCellsDialogCategoryPercentageDescription', @sdxFormatCellsDialogCategoryPercentageDescription);
  AProduct.Add('sdxFormatCellsDialogCategoryScientific', @sdxFormatCellsDialogCategoryScientific);
  AProduct.Add('sdxFormatCellsDialogCategoryText', @sdxFormatCellsDialogCategoryText);
  AProduct.Add('sdxFormatCellsDialogCategoryTextNotes', @sdxFormatCellsDialogCategoryTextNotes);
  AProduct.Add('sdxFormatCellsDialogCategoryTime', @sdxFormatCellsDialogCategoryTime);
  AProduct.Add('sdxFormatCellsDialogCategoryTimeDescription', @sdxFormatCellsDialogCategoryTimeDescription);
  AProduct.Add('sdxFormatCellsDialogCustomCode', @sdxFormatCellsDialogCustomCode);
  AProduct.Add('sdxFormatCellsDialogDecimalPlaces', @sdxFormatCellsDialogDecimalPlaces);
  AProduct.Add('sdxFormatCellsDialogFill', @sdxFormatCellsDialogFill);
  AProduct.Add('sdxFormatCellsDialogFillSample', @sdxFormatCellsDialogFillSample);
  AProduct.Add('sdxFormatCellsDialogFontColor', @sdxFormatCellsDialogFontColor);
  AProduct.Add('sdxFormatCellsDialogFontName', @sdxFormatCellsDialogFontName);
  AProduct.Add('sdxFormatCellsDialogFontNotInstalled', @sdxFormatCellsDialogFontNotInstalled);
  AProduct.Add('sdxFormatCellsDialogFontPreview', @sdxFormatCellsDialogFontPreview);
  AProduct.Add('sdxFormatCellsDialogFontPrintNotes', @sdxFormatCellsDialogFontPrintNotes);
  AProduct.Add('sdxFormatCellsDialogFontSize', @sdxFormatCellsDialogFontSize);
  AProduct.Add('sdxFormatCellsDialogFontStrikethrough', @sdxFormatCellsDialogFontStrikethrough);
  AProduct.Add('sdxFormatCellsDialogFontStyle', @sdxFormatCellsDialogFontStyle);
  AProduct.Add('sdxFormatCellsDialogFontUnderline', @sdxFormatCellsDialogFontUnderline);
  AProduct.Add('sdxFormatCellsDialogGroupFontEffects', @sdxFormatCellsDialogGroupFontEffects);
  AProduct.Add('sdxFormatCellsDialogGroupNumber', @sdxFormatCellsDialogGroupNumber);
  AProduct.Add('sdxFormatCellsDialogGroupTextAlignment', @sdxFormatCellsDialogGroupTextAlignment);
  AProduct.Add('sdxFormatCellsDialogHidden', @sdxFormatCellsDialogHidden);
  AProduct.Add('sdxFormatCellsDialogLocked', @sdxFormatCellsDialogLocked);
  AProduct.Add('sdxFormatCellsDialogMergeCells', @sdxFormatCellsDialogMergeCells);
  AProduct.Add('sdxFormatCellsDialogMoreColors', @sdxFormatCellsDialogMoreColors);
  AProduct.Add('sdxFormatCellsDialogNoColor', @sdxFormatCellsDialogNoColor);
  AProduct.Add('sdxFormatCellsDialogNone', @sdxFormatCellsDialogNone);
  AProduct.Add('sdxFormatCellsDialogNumberFormatTemplates', @sdxFormatCellsDialogNumberFormatTemplates);
  AProduct.Add('sdxFormatCellsDialogPatternColor', @sdxFormatCellsDialogPatternColor);
  AProduct.Add('sdxFormatCellsDialogPatternStyle', @sdxFormatCellsDialogPatternStyle);
  AProduct.Add('sdxFormatCellsDialogPreviewText', @sdxFormatCellsDialogPreviewText);
  AProduct.Add('sdxFormatCellsDialogProtection', @sdxFormatCellsDialogProtection);
  AProduct.Add('sdxFormatCellsDialogProtectionNotes', @sdxFormatCellsDialogProtectionNotes);
  AProduct.Add('sdxFormatCellsDialogSample', @sdxFormatCellsDialogSample);
  AProduct.Add('sdxFormatCellsDialogShrinkToFit', @sdxFormatCellsDialogShrinkToFit);
  AProduct.Add('sdxFormatCellsDialogTextAlignHorz', @sdxFormatCellsDialogTextAlignHorz);
  AProduct.Add('sdxFormatCellsDialogTextAlignHorzIndent', @sdxFormatCellsDialogTextAlignHorzIndent);
  AProduct.Add('sdxFormatCellsDialogTextAlignment', @sdxFormatCellsDialogTextAlignment);
  AProduct.Add('sdxFormatCellsDialogTextAlignVert', @sdxFormatCellsDialogTextAlignVert);
  AProduct.Add('sdxFormatCellsDialogTextControl', @sdxFormatCellsDialogTextControl);
  AProduct.Add('sdxFormatCellsDialogUnderlineNone', @sdxFormatCellsDialogUnderlineNone);
  AProduct.Add('sdxFormatCellsDialogUnderlineSingle', @sdxFormatCellsDialogUnderlineSingle);
  AProduct.Add('sdxFormatCellsDialogUseThousandSeparator', @sdxFormatCellsDialogUseThousandSeparator);
  AProduct.Add('sdxFormatCellsDialogWrapText', @sdxFormatCellsDialogWrapText);
  AProduct.Add('sdxHorzAlignCenter', @sdxHorzAlignCenter);
  AProduct.Add('sdxHorzAlignDistributed', @sdxHorzAlignDistributed);
  AProduct.Add('sdxHorzAlignFill', @sdxHorzAlignFill);
  AProduct.Add('sdxHorzAlignGeneral', @sdxHorzAlignGeneral);
  AProduct.Add('sdxHorzAlignJustify', @sdxHorzAlignJustify);
  AProduct.Add('sdxHorzAlignLeft', @sdxHorzAlignLeft);
  AProduct.Add('sdxHorzAlignRight', @sdxHorzAlignRight);
  AProduct.Add('sdxVertAlignBottom', @sdxVertAlignBottom);
  AProduct.Add('sdxVertAlignCenter', @sdxVertAlignCenter);
  AProduct.Add('sdxVertAlignDistributed', @sdxVertAlignDistributed);
  AProduct.Add('sdxVertAlignJustify', @sdxVertAlignJustify);
  AProduct.Add('sdxVertAlignTop', @sdxVertAlignTop);

  AProduct.Add('sdxConditionalFormattingRulesManagerDialogCaption',
    @sdxConditionalFormattingRulesManagerDialogCaption);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogDisplayMode',
    @sdxConditionalFormattingRulesManagerDialogDisplayMode);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogDisplayModeSelectedArea',
    @sdxConditionalFormattingRulesManagerDialogDisplayModeSelectedArea);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogDisplayModeSheet',
    @sdxConditionalFormattingRulesManagerDialogDisplayModeSheet);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogDisplayModeAll',
    @sdxConditionalFormattingRulesManagerDialogDisplayModeAll);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogActionClone',
    @sdxConditionalFormattingRulesManagerDialogActionClone);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogActionCreate',
    @sdxConditionalFormattingRulesManagerDialogActionCreate);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogActionDelete',
    @sdxConditionalFormattingRulesManagerDialogActionDelete);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogActionEdit',
    @sdxConditionalFormattingRulesManagerDialogActionEdit);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogColumnArea',
    @sdxConditionalFormattingRulesManagerDialogColumnArea);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogColumnFormat',
    @sdxConditionalFormattingRulesManagerDialogColumnFormat);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogColumnName',
    @sdxConditionalFormattingRulesManagerDialogColumnName);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogColumnStopIfTrue',
    @sdxConditionalFormattingRulesManagerDialogColumnStopIfTrue);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogColumnApplyToTheRecord',
    @sdxConditionalFormattingRulesManagerDialogColumnApplyToTheRecord);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogButtonApply',
    @sdxConditionalFormattingRulesManagerDialogButtonApply);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogButtonCancel',
    @sdxConditionalFormattingRulesManagerDialogButtonCancel);
  AProduct.Add('sdxConditionalFormattingRulesManagerDialogButtonOk',
    @sdxConditionalFormattingRulesManagerDialogButtonOk);

  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogAxis',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogAxis);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogAxisAuto',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogAxisAuto);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogAxisColor',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogAxisColor);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogAxisMidpoint',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogAxisMidpoint);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogAxisNone',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogAxisNone);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirection',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirection);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirectionAuto',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirectionAuto);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirectionLeftToRight',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirectionLeftToRight);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirectionRightToLeft',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirectionRightToLeft);

  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogBordersNone',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogBordersNone);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogBordersSolid',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogBordersSolid);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogButtonCancel',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogButtonCancel);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogButtonOk',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogButtonOk);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogCaption',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogCaption);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogCommon',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogCommon);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogFillModeGradient',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogFillModeGradient);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogFillModeSolid',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogFillModeSolid);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogFillMode',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogFillMode);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBar',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBar);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarBorderColor',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarBorderColor);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarBorderColorAuto',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarBorderColorAuto);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarColor',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarColor);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarColorAuto',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarColorAuto);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBar',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBar);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBarBorderColor',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBarBorderColor);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBarBorderStyle',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBarBorderStyle);
  AProduct.Add('sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBarColor',
    @sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBarColor);

  AProduct.Add('sdxConditionalFormattingRuleEditDialogReverseIconOrder',
    @sdxConditionalFormattingRuleEditDialogReverseIconOrder);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogShowBarOnly',
    @sdxConditionalFormattingRuleEditDialogShowBarOnly);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogShowIconOnly',
    @sdxConditionalFormattingRuleEditDialogShowIconOnly);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogAnd',
    @sdxConditionalFormattingRuleEditDialogAnd);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogButtonCancel',
    @sdxConditionalFormattingRuleEditDialogButtonCancel);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogButtonFormat',
    @sdxConditionalFormattingRuleEditDialogButtonFormat);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogButtonOk',
    @sdxConditionalFormattingRuleEditDialogButtonOk);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogCaption',
    @sdxConditionalFormattingRuleEditDialogCaption);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogWhenValueIs',
    @sdxConditionalFormattingRuleEditDialogWhenValueIs);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogWhenValueLess',
    @sdxConditionalFormattingRuleEditDialogWhenValueLess);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogWhenValueLessAnd',
    @sdxConditionalFormattingRuleEditDialogWhenValueLessAnd);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogWhenValueLessOrEqual',
    @sdxConditionalFormattingRuleEditDialogWhenValueLessOrEqual);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogWhenValueLessOrEqualAnd',
    @sdxConditionalFormattingRuleEditDialogWhenValueLessOrEqualAnd);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogIconStyle',
    @sdxConditionalFormattingRuleEditDialogIconStyle);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogPercentsOfSelectedRange',
    @sdxConditionalFormattingRuleEditDialogPercentsOfSelectedRange);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogPreview',
    @sdxConditionalFormattingRuleEditDialogPreview);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleType',
    @sdxConditionalFormattingRuleEditDialogRuleType);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleDescriptionAboveOrBelowAverage',
    @sdxConditionalFormattingRuleEditDialogRuleDescriptionAboveOrBelowAverage);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleDescriptionCellIs',
    @sdxConditionalFormattingRuleEditDialogRuleDescriptionCellIs);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleDescriptionExpression',
    @sdxConditionalFormattingRuleEditDialogRuleDescriptionExpression);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleDescriptionIconSet',
    @sdxConditionalFormattingRuleEditDialogRuleDescriptionIconSet);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleDescriptionTopBottomValues',
    @sdxConditionalFormattingRuleEditDialogRuleDescriptionTopBottomValues);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleNameAboveOrBelowAverage',
    @sdxConditionalFormattingRuleEditDialogRuleNameAboveOrBelowAverage);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleNameCellIs',
    @sdxConditionalFormattingRuleEditDialogRuleNameCellIs);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleNameDataBar',
    @sdxConditionalFormattingRuleEditDialogRuleNameDataBar);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleNameDuplicateValues',
    @sdxConditionalFormattingRuleEditDialogRuleNameDuplicateValues);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleNameExpression',
    @sdxConditionalFormattingRuleEditDialogRuleNameExpression);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleNameUniqueValues',
    @sdxConditionalFormattingRuleEditDialogRuleNameUniqueValues);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleNameTopBottomValues',
    @sdxConditionalFormattingRuleEditDialogRuleNameTopBottomValues);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleNameIconSet',
    @sdxConditionalFormattingRuleEditDialogRuleNameIconSet);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleNameThreeColorScale',
    @sdxConditionalFormattingRuleEditDialogRuleNameThreeColorScale);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogRuleNameTwoColorScale',
    @sdxConditionalFormattingRuleEditDialogRuleNameTwoColorScale);

  AProduct.Add('sdxConditionalFormattingRuleEditDialogScaleMaxStop', @sdxConditionalFormattingRuleEditDialogScaleMaxStop);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogScaleMidStop', @sdxConditionalFormattingRuleEditDialogScaleMidStop);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogScaleMinStop', @sdxConditionalFormattingRuleEditDialogScaleMinStop);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogScaleValueTypeFormula',
    @sdxConditionalFormattingRuleEditDialogScaleValueTypeFormula);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogScaleValueTypeHighestValue',
    @sdxConditionalFormattingRuleEditDialogScaleValueTypeHighestValue);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogScaleValueTypeLowestValue',
    @sdxConditionalFormattingRuleEditDialogScaleValueTypeLowestValue);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogScaleValueTypePercent',
    @sdxConditionalFormattingRuleEditDialogScaleValueTypePercent);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogScaleValueTypePercentile',
    @sdxConditionalFormattingRuleEditDialogScaleValueTypePercentile);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogScaleValueTypeValue',
    @sdxConditionalFormattingRuleEditDialogScaleValueTypeValue);

  AProduct.Add('sdxConditionalFormattingRuleEditDialogErrorInvalidExpression',
    @sdxConditionalFormattingRuleEditDialogErrorInvalidExpression);
  AProduct.Add('sdxConditionalFormattingRuleEditDialogErrorIncorrectStopsOrder',
    @sdxConditionalFormattingRuleEditDialogErrorIncorrectStopsOrder);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressSpreadSheet 2', @AddSpreadSheetDialogsResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressSpreadSheet 2', @AddSpreadSheetDialogsResourceStringNames);
end.
