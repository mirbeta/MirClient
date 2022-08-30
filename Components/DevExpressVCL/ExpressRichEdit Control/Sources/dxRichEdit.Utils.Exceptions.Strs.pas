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

unit dxRichEdit.Utils.Exceptions.Strs;

{$I cxVer.inc}

interface

uses
  dxCore, cxClasses;

resourcestring
  sdxRichEditExceptionThrowInternalException = 'An internal error occurred';
  sdxRichEditExceptionIsNotValid = '''%s'' is not a valid value for ''%s''';
  sdxRichEditExceptionUnsupportedDocVersion = 'Only MS Word 97 and later versions are supported';
  sdxRichEditExceptionEncryptedFile = 'Encrypted files are not currently supported';
  sdxRichEditExceptionMagicNumberNotFound = 'The file you are trying to open is in different format than specified by the file extension.';
  sdxRichEditExceptionUseDeletedStyleError = 'Error: using deleted style';
  sdxRichEditExceptionUseDeletedParagraphError = 'Error: using deleted paragraph';
  sdxRichEditExceptionUseDeletedFieldError = 'Error: using field paragraph';
  sdxRichEditExceptionUseDeletedSectionError = 'Error: using deleted section';
  sdxRichEditExceptionUseDeletedBookmarkError = 'Error: using deleted bookmark';
  sdxRichEditExceptionUseDeletedHyperlinkError = 'Error: using deleted hyperlink';
  sdxRichEditExceptionUseDeletedTableError = 'Error: using deleted table';
  sdxRichEditExceptionUseDeletedTableRowError = 'Error: using deleted table row';
  sdxRichEditExceptionUseDeletedTableCellError = 'Error: using deleted table cell';
  sdxRichEditExceptionUseInvalidParagraphProperties = 'Error: paragraph properties are no longer valid';
  sdxRichEditExceptionUseInvalidCharacterProperties = 'Error: character properties are no longer valid';
  sdxRichEditExceptionDocumentPositionDoesntMatchDocument = 'Error: specified document position or range belongs to other document or subdocument';
  sdxRichEditExceptionUseInvalidDocument = 'Error: this document is no longer valid';
  sdxRichEditExceptionUnsupportedFormatException = 'File format is not supported';
  sdxRichEditExceptionInvalidBeginInit = 'Error: call to BeginInit inside BeginUpdate';
  sdxRichEditExceptionInvalidEndInit = 'Error: call to EndInit or CancelInit without BeginInit or inside BeginUpdate';
  sdxRichEditExceptionInvalidBeginUpdate = 'Error: call to BeginUpdate inside BeginInit';
  sdxRichEditExceptionInvalidEndUpdate = 'Error: call to EndUpdate or CancelUpate without BeginUpdate or inside BeginInit';
  sdxRichEditExceptionInvalidSetCharacterProperties = 'Error: cannot set properties without BeginInit or without adding object to document';
  sdxRichEditExceptionInvalidSetParagraphProperties = 'Error: cannot set properties without BeginInit or without adding object to document';
  sdxRichEditExceptionInvalidParentStyle = 'Error: invalid parent style assignment caused circular reference';
  sdxRichEditExceptionInvalidDocumentModel = 'Error: document models are different';
  sdxRichEditExceptionInvalidParagraphContainNumbering = 'Error: paragraph already contains numbering';
  sdxRichEditExceptionTableIntegrityError = 'Error: broken table integrity';
  sdxRichEditExceptionInvalidCopyFromDocumentModel = 'Error: source and destination document models are different';
  sdxRichEditExceptionInvalidNumber = 'This is not a valid number.';
  sdxRichEditExceptionInvalidFontSize = 'The number must be between %s and %s.';
  sdxRichEditExceptionInvalidValueRange = 'The value must be between %s and %s.';
  sdxRichEditExceptionInvalidDivisor = 'The number must be a divisor of %s.';
  sdxRichEditExceptionUsedWrongUnit = 'The unit of measurement is incorrectly specified.';
  sdxRichEditExceptionInvalidTabStop = 'This is not a valid tab stop.';
  sdxRichEditExceptionVariableDeletedOrMissed = 'Error: document variable is either missing or deleted (from Variables collection)';
  sdxRichEditExceptionTopBottomSectionMarginsTooLarge = 'The top/bottom margins are too large for the page height in some sections.';
  sdxRichEditExceptionLeftRightSectionMarginsTooLarge = 'The left/right margins are too large for the page height in some sections.';
  sdxRichEditExceptionInvalidNumberingListIndex = 'NumberingListIndex refers to a list that does not exist.';
  sdxRichEditExceptionInvalidImageFile = 'The specified image is not valid.';
  sdxRichEditExceptionSearchComplete = 'The search is complete.';
  sdxRichEditExceptionSearchInForwardDirectionComplete = 'The end of the document has been reached.';
  sdxRichEditExceptionSearchInBackwardDirectionComplete = 'The beginning of the document has been reached.';
  sdxRichEditExceptionSearchInSelectionComplete = 'The search in the selection is finished.';
  sdxRichEditExceptionContinueSearchFromBeginningQuestion = 'Do you want to start the search from the beginning of the document?';
  sdxRichEditExceptionContinueSearchFromEndQuestion = 'Do you want to start the search from the end of the document?';
  sdxRichEditExceptionContinueSearchInRemainderQuestion = 'Do you want to search the remainder of the document?';
  sdxRichEditExceptionSearchItemNotFound = 'The search item was not found.';
  sdxRichEditExceptionReplacementsCount = '%s replacements were made.';
  sdxRichEditExceptionStyleAlreadyLinked = 'Error: style already contains a linked style';
  sdxRichEditExceptionErrorLinkDeletedStyle = 'Error: cannot link deleted style';
  sdxRichEditExceptionInvalidStyleName = 'Invalid style name';
  sdxRichEditExceptionIncorrectNumericFieldFormat = 'Error: number cannot be represented in the specified format.';
  sdxRichEditExceptionSyntaxErrorInFieldPattern = 'Syntax Error, %s.';
  sdxRichEditExceptionUnmatchedQuotesInFieldPattern = 'Error: pattern string contains unmatched quotes.';
  sdxRichEditExceptionUnknownSwitchArgument = 'Error! Unknown switch argument.';
  sdxRichEditExceptionUnexpectedEndOfFormula = '!Unexpected end of formula.';
  sdxRichEditExceptionMissingOperator = '!Missing operator.';
  sdxRichEditExceptionZeroDivide = '!Zero divide.';
  sdxRichEditExceptionCreateHyperlinkError = 'Cannot create a hyperlink. The hyperlink in the specified range already exists.';
  sdxRichEditExceptionClickToFollowHyperlink = 'Click to follow link';
  sdxRichEditExceptionInvalidNumberingListStartAtValue = '''Start At'' must be between %s and %s for this format';
  sdxRichEditExceptionBookmarkCreationFailing = 'Bookmark with the same name already exists. Replace?';
  sdxRichEditExceptionIncorrectBookmarkName = 'Bookmark name should start with a letter and contain only alphanumeric characters and underscore.';
  sdxRichEditExceptionDuplicateBookmark = 'Bookmark with that name already exists in the document';
  sdxRichEditExceptionInvalidNavigateUri = 'The address of this site is not valid. Check the address and try again.';
  sdxRichEditExceptionIncorrectPattern = 'Incorrect pattern.';
  sdxRichEditExceptionDocumentProtectionInvalidPassword = 'The password is incorrect!';
  sdxRichEditExceptionSelectBookmarkError = 'Cannot select a bookmark of inactive SubDocument.';
  sdxRichEditExceptionNoDefaultTabs = 'Default tab stops cannot be set.';
  sdxRichEditExceptionCantResetDefaultProperties = 'Can not reset default style settings.';
  sdxRichEditExceptionCantDeleteDefaultStyle = 'Cannot delete default style.';
  sdxRichEditExceptionNoTocEntriesFound = 'No table of contents entries found.';
  sdxRichEditExceptionNumberingListNotInListCollection = 'Cannot use a numbering List. The numbering list must be added to Document.NumberingLists collection';
  sdxRichEditExceptionParagraphStyleNameAlreadyExists = 'This style name already exists.';
  sdxRichEditExceptionDeleteTableStyleQuestion = 'Do you want to delete style %s from the document?';
  sdxRichEditExceptionCannotInsertShapeIntoTextBox = 'Shape can'#$27't be inserted into TextBox.Document.';
  sdxRichEditExceptionSelectionShouldContainAtLeastOneCharacter = 'The selection should contain at least one character.';
  sdxRichEditExceptionCurrentSelectionAndSpecifiedSelectionIntersect = 'Current selection and the specified selection intersect.';
  sdxRichEditExceptionSelectionShouldIncludeNotMoreThanOneRow = 'A selection range should include not more than one row';
  sdxRichEditExceptionFirstCellContinuesVerticalMerge = 'The first cell in the selected range continues the vertical merge, which is not allowed in a selection collection.';
  sdxRichEditExceptionPartiallySelectedCells = 'Partially selected cells are not allowed.';
  sdxRichEditExceptionLastCellContinuesVerticalMerge = 'The last cell in the selected range continues the vertical merge, which is not allowed in a selection collection.';
  sdxRichEditExceptionSelectionExtendsOutsideTable = 'The selection extends outside the table, so the entire row must be selected.';
  sdxRichEditExceptionEmptyCollection = 'Cannot add an empty collection.';
  sdxRichEditExceptionSpecifiedSelectionsIntersect = 'Specified selections intersect.';
  sdxRichEditExceptionRangeCannotBeEmpty = 'Range cannot be empty.';
  sdxRichEditExceptionOutOfRange = 'Index was out of range. Must be non-negative and less than the size of the selection collection.';
  sdxRichEditExceptionCannotRemoveCaret = 'Cannot remove caret.';

implementation

uses
  dxRichEdit.Strs;

procedure AddRichEditExceptionResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxRichEditExceptionThrowInternalException', @sdxRichEditExceptionThrowInternalException);
  AProduct.Add('sdxRichEditExceptionIsNotValid', @sdxRichEditExceptionIsNotValid);
  AProduct.Add('sdxRichEditExceptionUnsupportedDocVersion', @sdxRichEditExceptionUnsupportedDocVersion);
  AProduct.Add('sdxRichEditExceptionEncryptedFile', @sdxRichEditExceptionEncryptedFile);
  AProduct.Add('sdxRichEditExceptionMagicNumberNotFound', @sdxRichEditExceptionMagicNumberNotFound);
  AProduct.Add('sdxRichEditExceptionUseDeletedStyleError', @sdxRichEditExceptionUseDeletedStyleError);
  AProduct.Add('sdxRichEditExceptionUseDeletedParagraphError', @sdxRichEditExceptionUseDeletedParagraphError);
  AProduct.Add('sdxRichEditExceptionUseDeletedFieldError', @sdxRichEditExceptionUseDeletedFieldError);
  AProduct.Add('sdxRichEditExceptionUseDeletedSectionError', @sdxRichEditExceptionUseDeletedSectionError);
  AProduct.Add('sdxRichEditExceptionUseDeletedBookmarkError', @sdxRichEditExceptionUseDeletedBookmarkError);
  AProduct.Add('sdxRichEditExceptionUseDeletedHyperlinkError', @sdxRichEditExceptionUseDeletedHyperlinkError);
  AProduct.Add('sdxRichEditExceptionUseDeletedTableError', @sdxRichEditExceptionUseDeletedTableError);
  AProduct.Add('sdxRichEditExceptionUseDeletedTableRowError', @sdxRichEditExceptionUseDeletedTableRowError);
  AProduct.Add('sdxRichEditExceptionUseDeletedTableCellError', @sdxRichEditExceptionUseDeletedTableCellError);
  AProduct.Add('sdxRichEditExceptionUseInvalidParagraphProperties', @sdxRichEditExceptionUseInvalidParagraphProperties);
  AProduct.Add('sdxRichEditExceptionUseInvalidCharacterProperties', @sdxRichEditExceptionUseInvalidCharacterProperties);
  AProduct.Add('sdxRichEditExceptionDocumentPositionDoesntMatchDocument', @sdxRichEditExceptionDocumentPositionDoesntMatchDocument);
  AProduct.Add('sdxRichEditExceptionUseInvalidDocument', @sdxRichEditExceptionUseInvalidDocument);
  AProduct.Add('sdxRichEditExceptionUnsupportedFormatException', @sdxRichEditExceptionUnsupportedFormatException);
  AProduct.Add('sdxRichEditExceptionInvalidBeginInit', @sdxRichEditExceptionInvalidBeginInit);
  AProduct.Add('sdxRichEditExceptionInvalidEndInit', @sdxRichEditExceptionInvalidEndInit);
  AProduct.Add('sdxRichEditExceptionInvalidBeginUpdate', @sdxRichEditExceptionInvalidBeginUpdate);
  AProduct.Add('sdxRichEditExceptionInvalidEndUpdate', @sdxRichEditExceptionInvalidEndUpdate);
  AProduct.Add('sdxRichEditExceptionInvalidSetCharacterProperties', @sdxRichEditExceptionInvalidSetCharacterProperties);
  AProduct.Add('sdxRichEditExceptionInvalidSetParagraphProperties', @sdxRichEditExceptionInvalidSetParagraphProperties);
  AProduct.Add('sdxRichEditExceptionInvalidParentStyle', @sdxRichEditExceptionInvalidParentStyle);
  AProduct.Add('sdxRichEditExceptionInvalidDocumentModel', @sdxRichEditExceptionInvalidDocumentModel);
  AProduct.Add('sdxRichEditExceptionInvalidParagraphContainNumbering', @sdxRichEditExceptionInvalidParagraphContainNumbering);
  AProduct.Add('sdxRichEditExceptionTableIntegrityError', @sdxRichEditExceptionTableIntegrityError);
  AProduct.Add('sdxRichEditExceptionInvalidCopyFromDocumentModel', @sdxRichEditExceptionInvalidCopyFromDocumentModel);
  AProduct.Add('sdxRichEditExceptionInvalidNumber', @sdxRichEditExceptionInvalidNumber);
  AProduct.Add('sdxRichEditExceptionInvalidFontSize', @sdxRichEditExceptionInvalidFontSize);
  AProduct.Add('sdxRichEditExceptionInvalidValueRange', @sdxRichEditExceptionInvalidValueRange);
  AProduct.Add('sdxRichEditExceptionInvalidDivisor', @sdxRichEditExceptionInvalidDivisor);
  AProduct.Add('sdxRichEditExceptionUsedWrongUnit', @sdxRichEditExceptionUsedWrongUnit);
  AProduct.Add('sdxRichEditExceptionInvalidTabStop', @sdxRichEditExceptionInvalidTabStop);
  AProduct.Add('sdxRichEditExceptionVariableDeletedOrMissed', @sdxRichEditExceptionVariableDeletedOrMissed);
  AProduct.Add('sdxRichEditExceptionTopBottomSectionMarginsTooLarge', @sdxRichEditExceptionTopBottomSectionMarginsTooLarge);
  AProduct.Add('sdxRichEditExceptionLeftRightSectionMarginsTooLarge', @sdxRichEditExceptionLeftRightSectionMarginsTooLarge);
  AProduct.Add('sdxRichEditExceptionInvalidNumberingListIndex', @sdxRichEditExceptionInvalidNumberingListIndex);
  AProduct.Add('sdxRichEditExceptionInvalidImageFile', @sdxRichEditExceptionInvalidImageFile);
  AProduct.Add('sdxRichEditExceptionSearchComplete', @sdxRichEditExceptionSearchComplete);
  AProduct.Add('sdxRichEditExceptionSearchInForwardDirectionComplete', @sdxRichEditExceptionSearchInForwardDirectionComplete);
  AProduct.Add('sdxRichEditExceptionSearchInBackwardDirectionComplete', @sdxRichEditExceptionSearchInBackwardDirectionComplete);
  AProduct.Add('sdxRichEditExceptionSearchInSelectionComplete', @sdxRichEditExceptionSearchInSelectionComplete);
  AProduct.Add('sdxRichEditExceptionContinueSearchFromBeginningQuestion', @sdxRichEditExceptionContinueSearchFromBeginningQuestion);
  AProduct.Add('sdxRichEditExceptionContinueSearchFromEndQuestion', @sdxRichEditExceptionContinueSearchFromEndQuestion);
  AProduct.Add('sdxRichEditExceptionContinueSearchInRemainderQuestion', @sdxRichEditExceptionContinueSearchInRemainderQuestion);
  AProduct.Add('sdxRichEditExceptionSearchItemNotFound', @sdxRichEditExceptionSearchItemNotFound);
  AProduct.Add('sdxRichEditExceptionReplacementsCount', @sdxRichEditExceptionReplacementsCount);
  AProduct.Add('sdxRichEditExceptionStyleAlreadyLinked', @sdxRichEditExceptionStyleAlreadyLinked);
  AProduct.Add('sdxRichEditExceptionErrorLinkDeletedStyle', @sdxRichEditExceptionErrorLinkDeletedStyle);
  AProduct.Add('sdxRichEditExceptionInvalidStyleName', @sdxRichEditExceptionInvalidStyleName);
  AProduct.Add('sdxRichEditExceptionIncorrectNumericFieldFormat', @sdxRichEditExceptionIncorrectNumericFieldFormat);
  AProduct.Add('sdxRichEditExceptionSyntaxErrorInFieldPattern', @sdxRichEditExceptionSyntaxErrorInFieldPattern);
  AProduct.Add('sdxRichEditExceptionUnmatchedQuotesInFieldPattern', @sdxRichEditExceptionUnmatchedQuotesInFieldPattern);
  AProduct.Add('sdxRichEditExceptionUnknownSwitchArgument', @sdxRichEditExceptionUnknownSwitchArgument);
  AProduct.Add('sdxRichEditExceptionUnexpectedEndOfFormula', @sdxRichEditExceptionUnexpectedEndOfFormula);
  AProduct.Add('sdxRichEditExceptionMissingOperator', @sdxRichEditExceptionMissingOperator);
  AProduct.Add('sdxRichEditExceptionZeroDivide', @sdxRichEditExceptionZeroDivide);
  AProduct.Add('sdxRichEditExceptionCreateHyperlinkError', @sdxRichEditExceptionCreateHyperlinkError);
  AProduct.Add('sdxRichEditExceptionClickToFollowHyperlink', @sdxRichEditExceptionClickToFollowHyperlink);
  AProduct.Add('sdxRichEditExceptionInvalidNumberingListStartAtValue', @sdxRichEditExceptionInvalidNumberingListStartAtValue);
  AProduct.Add('sdxRichEditExceptionBookmarkCreationFailing', @sdxRichEditExceptionBookmarkCreationFailing);
  AProduct.Add('sdxRichEditExceptionIncorrectBookmarkName', @sdxRichEditExceptionIncorrectBookmarkName);
  AProduct.Add('sdxRichEditExceptionDuplicateBookmark', @sdxRichEditExceptionDuplicateBookmark);
  AProduct.Add('sdxRichEditExceptionInvalidNavigateUri', @sdxRichEditExceptionInvalidNavigateUri);
  AProduct.Add('sdxRichEditExceptionIncorrectPattern', @sdxRichEditExceptionIncorrectPattern);
  AProduct.Add('sdxRichEditExceptionDocumentProtectionInvalidPassword', @sdxRichEditExceptionDocumentProtectionInvalidPassword);
  AProduct.Add('sdxRichEditExceptionSelectBookmarkError', @sdxRichEditExceptionSelectBookmarkError);
  AProduct.Add('sdxRichEditExceptionNoDefaultTabs', @sdxRichEditExceptionNoDefaultTabs);
  AProduct.Add('sdxRichEditExceptionCantResetDefaultProperties', @sdxRichEditExceptionCantResetDefaultProperties);
  AProduct.Add('sdxRichEditExceptionCantDeleteDefaultStyle', @sdxRichEditExceptionCantDeleteDefaultStyle);
  AProduct.Add('sdxRichEditExceptionNoTocEntriesFound', @sdxRichEditExceptionNoTocEntriesFound);
  AProduct.Add('sdxRichEditExceptionNumberingListNotInListCollection', @sdxRichEditExceptionNumberingListNotInListCollection);
  AProduct.Add('sdxRichEditExceptionParagraphStyleNameAlreadyExists', @sdxRichEditExceptionParagraphStyleNameAlreadyExists);
  AProduct.Add('sdxRichEditExceptionDeleteTableStyleQuestion', @sdxRichEditExceptionDeleteTableStyleQuestion);
  AProduct.Add('sdxRichEditExceptionCannotInsertShapeIntoTextBox', @sdxRichEditExceptionCannotInsertShapeIntoTextBox);
  AProduct.Add('sdxRichEditExceptionSelectionShouldContainAtLeastOneCharacter', @sdxRichEditExceptionSelectionShouldContainAtLeastOneCharacter);
  AProduct.Add('sdxRichEditExceptionCurrentSelectionAndSpecifiedSelectionIntersect', @sdxRichEditExceptionCurrentSelectionAndSpecifiedSelectionIntersect);
  AProduct.Add('sdxRichEditExceptionSelectionShouldIncludeNotMoreThanOneRow', @sdxRichEditExceptionSelectionShouldIncludeNotMoreThanOneRow);
  AProduct.Add('sdxRichEditExceptionFirstCellContinuesVerticalMerge', @sdxRichEditExceptionFirstCellContinuesVerticalMerge);
  AProduct.Add('sdxRichEditExceptionPartiallySelectedCells', @sdxRichEditExceptionPartiallySelectedCells);
  AProduct.Add('sdxRichEditExceptionLastCellContinuesVerticalMerge', @sdxRichEditExceptionLastCellContinuesVerticalMerge);
  AProduct.Add('sdxRichEditExceptionSelectionExtendsOutsideTable', @sdxRichEditExceptionSelectionExtendsOutsideTable);
  AProduct.Add('sdxRichEditExceptionEmptyCollection', @sdxRichEditExceptionEmptyCollection);
  AProduct.Add('sdxRichEditExceptionSpecifiedSelectionsIntersect', @sdxRichEditExceptionSpecifiedSelectionsIntersect);
  AProduct.Add('sdxRichEditExceptionRangeCannotBeEmpty', @sdxRichEditExceptionRangeCannotBeEmpty);
  AProduct.Add('sdxRichEditExceptionOutOfRange', @sdxRichEditExceptionOutOfRange);
  AProduct.Add('sdxRichEditExceptionCannotRemoveCaret', @sdxRichEditExceptionCannotRemoveCaret);
end;

initialization
  dxResourceStringsRepository.RegisterProduct(dxRichEditProductName, @AddRichEditExceptionResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct(dxRichEditProductName, @AddRichEditExceptionResourceStringNames);

end.
