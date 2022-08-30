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

unit dxSpreadSheetCoreStrs;

{$I cxVer.Inc}

interface

uses
  dxCore, cxClasses, dxSpreadSheetTypes, dxSpreadSheetGraphics, Graphics;

resourcestring
  //
  sdxTrue   = 'True';
  sdxFalse  = 'False';

  // string errors
  serNameError    = '#NAME?';
  serNAError      = '#N/A';
  // all following messages must end with the "!" character
  serNullError    = '#NULL!';
  serDivZeroError = '#DIV/0!';
  serValueError   = '#VALUE!';
  serNumError     = '#NUM!';
  serRefError     = '#REF!';

  sdxDefaultSheetCaption = 'Sheet%d';

  sdxErrorCannotChangePartOfArray = 'You cannot change part of an array.';
  sdxErrorCannotRenameSheet = 'Cannot rename a sheet to the same name as existing sheet.';
  sdxErrorCellProtected = 'The cell that you are trying to change is protected and therefore read-only.';
  sdxErrorColorValueIsNotSpecified = 'Color value is not specified';
  sdxErrorCommentAlreadyExists = 'Comment container for the "%s" cell already exists';
  sdxErrorDefinedNameAlreadyExists = 'Duplicate name "%s" is found';
  sdxErrorDocumentIsCorrupted = 'Document is corrupted';
  sdxErrorExternalLinkAlreadyExists = 'An external link named "%s" already exists';
  sdxErrorFieldNotFound = 'The %s field was not found';
  sdxErrorFileCannotBeFoundInPackage = 'File "%s" cannot be found in the package';
  sdxErrorFileIsCorrupted = 'File "%s" is corrupted';
  sdxErrorInternal = 'Internal error: "%s"';
  sdxErrorInvalidAnchorCell = 'The "%s" cell cannot be used as an anchor';
  sdxErrorInvalidAnchorDefinition = 'Invalid anchor definition';
  sdxErrorInvalidCellReference = 'The "%s" cell reference is invalid';
  sdxErrorInvalidCellsReference = 'The "%s" cells reference is invalid';
  sdxErrorInvalidColor = 'The "%s" color value is not supported';
  sdxErrorInvalidColorIndex = 'The "%d" color index is invalid';
  sdxErrorInvalidColumnIndex = 'The "%s" column index is invalid';
  sdxErrorInvalidDocumentType = 'Unsupported document type';
  sdxErrorInvalidFormatCodeID = 'The "%d" format code ID is invalid';
  sdxErrorInvalidFormula = 'The "%s" formula is invalid';
  sdxErrorInvalidReference = 'The "%s" reference at position %d is invalid';
  sdxErrorInvalidRelationshipId = 'The "%s" relationship ID is invalid';
  sdxErrorInvalidSelection = 'This command cannot be used on multiple selections or an empty selection';
  sdxErrorInvalidPasteArea = 'Cannot paste into selected area because its size is not the same as the copied area''s size';
  sdxErrorInvalidSharedStringIndex = 'The "%d" shared string index is invalid';
  sdxErrorInvalidSheetId = 'Sheet with ID="%s" cannot be found';
  sdxErrorInvalidStyleIndex = 'The "%d" style index is invalid';
  sdxErrorMaxOutlineLevel = 'The number of outline levels for both rows and columns cannot be more than 8.';
  sdxErrorPictureCannotBeFound = 'Picture "%s" cannot be found';
  sdxErrorCannotExecuteActionOnProtectedSheet = 'You cannot use this command on a protected sheet. To use this command you must first unprotect the sheet.';
  sdxErrorUnsupportedDocumentFormat = 'Unsupported document format';
  sdxErrorUnsupportedSheetType = 'Unsupported sheet type';

  sdxErrorCellAlreadyExists = 'Cell with "%d" index already exists';
  sdxErrorPossibleDataLoss =
    'To prevent possible loss of data, shifting non-blank cells off the worksheet has been canceled. ' +
    'Select another location in which to insert new cells, or delete data from the end of your worksheet.';
  sdxErrorCannotMoveBecauseOfMergedCells = 'This operation will cause some merged cells to unmerge.';
  sdxReplaceCellsDataConfirmation = 'There''s already data here. Do you want to replace it?';
  sdxUnmergeCellsConfirmation = 'This operation will cause some merged cells to unmerge. Do you wish to continue?';

  //
  sdxErrorCircularMessage = 'Warning' + #13#10 +
    'One or more formulas contain a circular reference and may not calculate correctly. Circular references are any ' +
    'references within a formula that depend upon the results of that same formula. For example, a cell that refers ' +
    'to its own value or a cell that refers to another cell which depends on the original cell''s value both contain ' +
    'circular references.' +
    #13#10#13#10 +
    'Click OK to create a circular reference and continue.';

  sdxErrorCircularPathPrefix = 'The ';

  // Common
  sdxPreviewText = 'AaBbCcYyZz';

  // Conditional Formatting Rules Details
  sdxConditionalFormattingCellIsRuleDetailsBetween = 'Cell Value between "%s" and "%s"';
  sdxConditionalFormattingCellIsRuleDetailsEqual = 'Cell Value equal to "%s"';
  sdxConditionalFormattingCellIsRuleDetailsGreaterThan = 'Cell Value greater than "%s"';
  sdxConditionalFormattingCellIsRuleDetailsGreaterThanOrEqual = 'Cell Value greater than or equal to "%s"';
  sdxConditionalFormattingCellIsRuleDetailsLessThan = 'Cell Value less than "%s"';
  sdxConditionalFormattingCellIsRuleDetailsLessThanOrEqual = 'Cell Value less than or equal to "%s"';
  sdxConditionalFormattingCellIsRuleDetailsNotBetween = 'Cell Value not between "%s" and "%s"';
  sdxConditionalFormattingCellIsRuleDetailsNotEqual = 'Cell Value not equal to "%s"';
  sdxConditionalFormattingDuplicateValuesRuleDetails = 'Duplicate Values';
  sdxConditionalFormattingExpressionRuleDetails = 'Formula: %s';
  sdxConditionalFormattingUniqueValuesRuleDetails = 'Unique Values';

  // Conditional Formatting - CellIs Rule Comparison Operators
  sdxConditionalFormattingCellIsRuleComparisonOperatorBetween = 'between';
  sdxConditionalFormattingCellIsRuleComparisonOperatorEqual = 'equal';
  sdxConditionalFormattingCellIsRuleComparisonOperatorGreaterThan = 'greater than';
  sdxConditionalFormattingCellIsRuleComparisonOperatorGreaterThanOrEqual = 'greater than or equal';
  sdxConditionalFormattingCellIsRuleComparisonOperatorLessThan = 'less than';
  sdxConditionalFormattingCellIsRuleComparisonOperatorLessThanOrEqual = 'less than or equal';
  sdxConditionalFormattingCellIsRuleComparisonOperatorNotBetween = 'not between';
  sdxConditionalFormattingCellIsRuleComparisonOperatorNotEqual = 'not equal';

  // Conditional Formatting - AboveOrBelowAverage Rule
  sdxConditionalFormattingAboveAverage = 'Above average for the selected range';
  sdxConditionalFormattingAboveAverageOnStandardDeviation = '%d standard deviation above average for the selected range';
  sdxConditionalFormattingAboveOrEqualAverage = 'Above or equal average for the selected range';
  sdxConditionalFormattingBelowAverage = 'Below average for the selected range';
  sdxConditionalFormattingBelowAverageOnStandardDeviation = '%d standard deviation below average for the selected range';
  sdxConditionalFormattingBelowOrEqualAverage = 'Below or equal average for the selected range';

  // Conditional Formatting - TopBottom Rule
  sdxConditionalFormattingBottomValues = 'Bottom %s';
  sdxConditionalFormattingTopValues = 'Top %s';

  // Conditional Formatting - ColorScale Rules
  sdxConditionalFormattingColorScale = 'Color Scale';

  // Conditional Formatting - IconSet Rule
  sdxConditionalFormattingIconSet = 'Icon Set';

  // Conditional Formatting - DataBar Rule
  sdxConditionalFormattingDataBar = 'Data Bar';

  // Quarter (for AutoFilling)
  sdxQuarter = 'Quarter';
  sdxQuarterAbbreviation1 = 'Qtr';
  sdxQuarterAbbreviation2 = 'Q';

const
  dxSpreadSheetGeneralNumberFormat = 'GENERAL'; // don't localize
  dxAbsoluteReferenceChar: string = '$';
  dxReferenceLeftParenthesis: string = '[';
  dxReferenceRightParenthesis: string = ']';
  dxRCRowReferenceChar: string = 'R';
  dxRCColumnReferenceChar: string = 'C';

  dxRefSeparator: string = '!';
  dxAreaSeparator: string = ':';

  dxStringMarkChar: string = '"';
  dxStringMarkChar2: string = '''';

  dxExponentChar:  string = 'E';

  dxLeftParenthesis: string = '(';
  dxRightParenthesis: string = ')';

  dxLeftArrayParenthesis: string = '{';
  dxRightArrayParenthesis: string = '}';

  dxErrorPrefix: string = '#';

  dxDefaultOperations: TdxSpreadSheetOperationStrings = (
    '+', '-', '*', '/', '^', '&', '<', '<=', '=', '>=', '>', '<>', ' ', ',', ':', '', '-', '%', ''
  );

  dxBoolToString: array[Boolean] of string = ('FALSE', 'TRUE');

implementation

procedure AddSpreadSheetResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxTrue', @sdxTrue);
  AProduct.Add('sdxFalse', @sdxFalse);
  AProduct.Add('serNameError', @serNameError);
  AProduct.Add('serNAError', @serNAError);
  AProduct.Add('serNullError', @serNullError);
  AProduct.Add('serDivZeroError', @serDivZeroError);
  AProduct.Add('serValueError', @serValueError);
  AProduct.Add('serNumError', @serNumError);
  AProduct.Add('serRefError', @serRefError);

  AProduct.Add('sdxDefaultSheetCaption', @sdxDefaultSheetCaption);

  AProduct.Add('sdxErrorCannotRenameSheet', @sdxErrorCannotRenameSheet);
  AProduct.Add('sdxErrorCannotChangePartOfArray', @sdxErrorCannotChangePartOfArray);
  AProduct.Add('sdxErrorCellProtected', @sdxErrorCellProtected);
  AProduct.Add('sdxErrorColorValueIsNotSpecified', @sdxErrorColorValueIsNotSpecified);
  AProduct.Add('sdxErrorCommentAlreadyExists', @sdxErrorCommentAlreadyExists);
  AProduct.Add('sdxErrorDefinedNameAlreadyExists', @sdxErrorDefinedNameAlreadyExists);
  AProduct.Add('sdxErrorDocumentIsCorrupted', @sdxErrorDocumentIsCorrupted);
  AProduct.Add('sdxErrorExternalLinkAlreadyExists', @sdxErrorExternalLinkAlreadyExists);
  AProduct.Add('sdxErrorFieldNotFound', @sdxErrorFieldNotFound);
  AProduct.Add('sdxErrorFileCannotBeFoundInPackage', @sdxErrorFileCannotBeFoundInPackage);
  AProduct.Add('sdxErrorFileIsCorrupted', @sdxErrorFileIsCorrupted);
  AProduct.Add('sdxErrorInternal', @sdxErrorInternal);
  AProduct.Add('sdxErrorInvalidAnchorCell', @sdxErrorInvalidAnchorCell);
  AProduct.Add('sdxErrorInvalidAnchorDefinition', @sdxErrorInvalidAnchorDefinition);
  AProduct.Add('sdxErrorInvalidCellReference', @sdxErrorInvalidCellReference);
  AProduct.Add('sdxErrorInvalidCellsReference', @sdxErrorInvalidCellsReference);
  AProduct.Add('sdxErrorInvalidColor', @sdxErrorInvalidColor);
  AProduct.Add('sdxErrorInvalidColorIndex', @sdxErrorInvalidColorIndex);
  AProduct.Add('sdxErrorInvalidColumnIndex', @sdxErrorInvalidColumnIndex);
  AProduct.Add('sdxErrorInvalidDocumentType', @sdxErrorInvalidDocumentType);
  AProduct.Add('sdxErrorInvalidFormatCodeID', @sdxErrorInvalidFormatCodeID);
  AProduct.Add('sdxErrorInvalidFormula', @sdxErrorInvalidFormula);
  AProduct.Add('sdxErrorInvalidReference', @sdxErrorInvalidReference);
  AProduct.Add('sdxErrorInvalidRelationshipId', @sdxErrorInvalidRelationshipId);
  AProduct.Add('sdxErrorInvalidSelection', @sdxErrorInvalidSelection);
  AProduct.Add('sdxErrorInvalidPasteArea', @sdxErrorInvalidPasteArea);
  AProduct.Add('sdxErrorInvalidSharedStringIndex', @sdxErrorInvalidSharedStringIndex);
  AProduct.Add('sdxErrorInvalidSheetId', @sdxErrorInvalidSheetId);
  AProduct.Add('sdxErrorInvalidStyleIndex', @sdxErrorInvalidStyleIndex);
  AProduct.Add('sdxErrorMaxOutlineLevel', @sdxErrorMaxOutlineLevel);
  AProduct.Add('sdxErrorPictureCannotBeFound', @sdxErrorPictureCannotBeFound);
  AProduct.Add('sdxErrorCannotExecuteActionOnProtectedSheet', @sdxErrorCannotExecuteActionOnProtectedSheet);
  AProduct.Add('sdxErrorUnsupportedDocumentFormat', @sdxErrorUnsupportedDocumentFormat);
  AProduct.Add('sdxErrorUnsupportedSheetType', @sdxErrorUnsupportedSheetType);
  AProduct.Add('sdxErrorCellAlreadyExists', @sdxErrorCellAlreadyExists);
  AProduct.Add('sdxErrorPossibleDataLoss', @sdxErrorPossibleDataLoss);
  AProduct.Add('sdxErrorCannotMoveBecauseOfMergedCells', @sdxErrorCannotMoveBecauseOfMergedCells);
  AProduct.Add('sdxReplaceCellsDataConfirmation', @sdxReplaceCellsDataConfirmation);
  AProduct.Add('sdxUnmergeCellsConfirmation', @sdxUnmergeCellsConfirmation);
  AProduct.Add('sdxErrorCircularMessage', @sdxErrorCircularMessage);
  AProduct.Add('sdxErrorCircularPathPrefix', @sdxErrorCircularPathPrefix);

  AProduct.Add('sdxPreviewText', @sdxPreviewText);

  AProduct.Add('sdxConditionalFormattingCellIsRuleDetailsBetween', @sdxConditionalFormattingCellIsRuleDetailsBetween);
  AProduct.Add('sdxConditionalFormattingCellIsRuleDetailsEqual', @sdxConditionalFormattingCellIsRuleDetailsEqual);
  AProduct.Add('sdxConditionalFormattingCellIsRuleDetailsGreaterThan', @sdxConditionalFormattingCellIsRuleDetailsGreaterThan);
  AProduct.Add('sdxConditionalFormattingCellIsRuleDetailsGreaterThanOrEqual', @sdxConditionalFormattingCellIsRuleDetailsGreaterThanOrEqual);
  AProduct.Add('sdxConditionalFormattingCellIsRuleDetailsLessThan', @sdxConditionalFormattingCellIsRuleDetailsLessThan);
  AProduct.Add('sdxConditionalFormattingCellIsRuleDetailsLessThanOrEqual', @sdxConditionalFormattingCellIsRuleDetailsLessThanOrEqual);
  AProduct.Add('sdxConditionalFormattingCellIsRuleDetailsNotBetween', @sdxConditionalFormattingCellIsRuleDetailsNotBetween);
  AProduct.Add('sdxConditionalFormattingCellIsRuleDetailsNotEqual', @sdxConditionalFormattingCellIsRuleDetailsNotEqual);
  AProduct.Add('sdxConditionalFormattingDuplicateValuesRuleDetails', @sdxConditionalFormattingDuplicateValuesRuleDetails);
  AProduct.Add('sdxConditionalFormattingExpressionRuleDetails', @sdxConditionalFormattingExpressionRuleDetails);
  AProduct.Add('sdxConditionalFormattingUniqueValuesRuleDetails', @sdxConditionalFormattingUniqueValuesRuleDetails);

  AProduct.Add('sdxConditionalFormattingCellIsRuleComparisonOperatorBetween', @sdxConditionalFormattingCellIsRuleComparisonOperatorBetween);
  AProduct.Add('sdxConditionalFormattingCellIsRuleComparisonOperatorEqual', @sdxConditionalFormattingCellIsRuleComparisonOperatorEqual);
  AProduct.Add('sdxConditionalFormattingCellIsRuleComparisonOperatorGreaterThan', @sdxConditionalFormattingCellIsRuleComparisonOperatorGreaterThan);
  AProduct.Add('sdxConditionalFormattingCellIsRuleComparisonOperatorGreaterThanOrEqual', @sdxConditionalFormattingCellIsRuleComparisonOperatorGreaterThanOrEqual);
  AProduct.Add('sdxConditionalFormattingCellIsRuleComparisonOperatorLessThan', @sdxConditionalFormattingCellIsRuleComparisonOperatorLessThan);
  AProduct.Add('sdxConditionalFormattingCellIsRuleComparisonOperatorLessThanOrEqual', @sdxConditionalFormattingCellIsRuleComparisonOperatorLessThanOrEqual);
  AProduct.Add('sdxConditionalFormattingCellIsRuleComparisonOperatorNotBetween', @sdxConditionalFormattingCellIsRuleComparisonOperatorNotBetween);
  AProduct.Add('sdxConditionalFormattingCellIsRuleComparisonOperatorNotEqual', @sdxConditionalFormattingCellIsRuleComparisonOperatorNotEqual);

  AProduct.Add('sdxConditionalFormattingAboveAverage', @sdxConditionalFormattingAboveAverage);
  AProduct.Add('sdxConditionalFormattingAboveAverageOnStandardDeviation', @sdxConditionalFormattingAboveAverageOnStandardDeviation);
  AProduct.Add('sdxConditionalFormattingAboveOrEqualAverage', @sdxConditionalFormattingAboveOrEqualAverage);
  AProduct.Add('sdxConditionalFormattingBelowAverage', @sdxConditionalFormattingBelowAverage);
  AProduct.Add('sdxConditionalFormattingBelowAverageOnStandardDeviation', @sdxConditionalFormattingBelowAverageOnStandardDeviation);
  AProduct.Add('sdxConditionalFormattingBelowOrEqualAverage', @sdxConditionalFormattingBelowOrEqualAverage);

  AProduct.Add('sdxConditionalFormattingBottomValues', @sdxConditionalFormattingBottomValues);
  AProduct.Add('sdxConditionalFormattingTopValues', @sdxConditionalFormattingTopValues);
  AProduct.Add('sdxConditionalFormattingColorScale', @sdxConditionalFormattingColorScale);
  AProduct.Add('sdxConditionalFormattingIconSet', @sdxConditionalFormattingIconSet);
  AProduct.Add('sdxConditionalFormattingDataBar', @sdxConditionalFormattingDataBar);

  AProduct.Add('sdxQuarter', @sdxQuarter);
  AProduct.Add('sdxQuarterAbbreviation1', @sdxQuarterAbbreviation1);
  AProduct.Add('sdxQuarterAbbreviation2', @sdxQuarterAbbreviation1);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressSpreadSheet 2', @AddSpreadSheetResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressSpreadSheet 2', @AddSpreadSheetResourceStringNames);
end.
