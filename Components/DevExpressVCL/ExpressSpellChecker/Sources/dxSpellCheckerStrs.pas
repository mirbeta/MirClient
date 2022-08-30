{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpellChecker                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPELLCHECKER AND ALL           }
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

unit dxSpellCheckerStrs;

{$I cxVer.inc}

interface

resourcestring

  sdxSpellCheckerMoreThanOne = 'An application should contain only a single TdxSpellChecker';
  sdxSpellCheckerFileFormatMismatch = 'File format mismatch';
  sdxSpellCheckerISpellDictionary = 'ISpell';
  sdxSpellCheckerOpenOfficeDictionary = 'OpenOffice';
  sdxSpellCheckerUserDictionary = 'User';
  sdxSpellCheckerHunspellDictionary = 'Hunspell';

  sdxSpellCheckerApplylButton = '&Apply';
  sdxSpellCheckerOkButton = '&OK';
  sdxSpellCheckerCancelButton = 'Cancel';
  sdxSpellCheckerAddButton = '&Add';
  sdxSpellCheckerAddButton1 = 'A&dd';
  sdxSpellCheckerChangeButton = '&Change';
  sdxSpellCheckerEditButton = 'Edit...';
  sdxSpellCheckerChangeAllButton = 'Change A&ll';
  sdxSpellCheckerCloseButton = 'Close';
  sdxSpellCheckerDeleteButton = '&Delete';
  sdxSpellCheckerDeleteButton1 = 'D&elete';
  sdxSpellCheckerDeleteButton2 = 'De&lete';
  sdxSpellCheckerDeleteAllButton = 'Delete A&ll';
  sdxSpellCheckerIgnoreButton = '&Ignore';
  sdxSpellCheckerIgnoreAllButton = 'I&gnore All';
  sdxSpellCheckerOptionsButton = '&Options...';
  sdxSpellCheckerSuggestButton = '&Suggest';
  sdxSpellCheckerUndoButton = '&Undo';
  sdxSpellCheckerUndoEditButton = '&Undo Edit';
  sdxSpellCheckerUndoLastButton = '&Undo Last';
  sdxSpellCheckerChangeTo = 'Change &to:';
  sdxSpellCheckerRepeatedWord = 'Repeated Word:';
  sdxSpellCheckerSuggestions = 'Suggestio&ns:';
  sdxSpellCheckerNotInDictionary = 'Not in Dictionary:';
  sdxSpellCheckerNoSuggestions = '(No suggestions)';

  sdxSpellCheckerCustomDictionaryFormCaption = 'Custom Dictionary';
  sdxSpellCheckerSpellingFormCaption = 'Spelling';
  sdxSpellCheckerSpellingComplete = 'The spelling check is complete.';
  sdxSpellCheckerNoActiveDictionaries = 'Dictionaries are not available';
  sdxSpellCheckerNotUseChangeAll = 'Change All option is not available because you modified ' +
    'text other than the actual spelling error. Choose Change to modify only ' +
    'this sentence, or Undo Edit to restore original sentence';
  sdxSpellCheckerConfirmUseUnknownWord = 'You have chosen a word that is not found in the ' +
    'main or custom dictionaries. Do you want to use this word and continue checking?';
  sdxSpellCheckerSelectionCheckIsFinished = 'Selection check is finished. Do you want to continue checking the remainder of the document?';

  // Options
  sdxSpellCheckerSpellingOptionsFormCaption = 'Spelling Options';
  sdxSpellCheckerSpellingOptionsMainGroupBox = 'Spelling';
  sdxSpellCheckerSpellingLanguage = 'Language:';
  sdxSpellCheckerSpellingOptionsInternationalDictionariesGroupBox = 'International dictionaries';
  sdxSpellCheckerSpellingOptionsInternationalDictionariesText = 'Choose which dictionaries to use when checking your spelling.';
  sdxSpellCheckerSpellingOptionsEditCustomDictionaryGroupBox = 'Edit custom dictionary';
  sdxSpellCheckerSpellingOptionsEditCustomDictionaryText = 'Add, change or remove words from your custom dictionary.';
  sdxSpellCheckerSpellingOptionsGeneralOptionsGroupBox = 'General options';
  sdxSpellCheckerIgnoreUpperCaseWords = 'Ignore words in UPPERCASE';
  sdxSpellCheckerIgnoreMixedCaseWords = 'Ignore words in MiXeDcASe';
  sdxSpellCheckerIgnoreWordsWithNumbers = 'Ignore words with numbers';
  sdxSpellCheckerIgnoreRepeatedWords = 'Ignore repeated words';
  sdxSpellCheckerIgnoreEmails = 'Ignore e-mails';
  sdxSpellCheckerIgnoreUrls = 'Ignore web sites';

  // AutoCorrect
  sdxSpellCheckerAutoCorrect = 'AutoCorrect';
  sdxSpellCheckerAutoCorrectReplacementExistMessageFormat = 'An AutoCorrect entry for %s already exists. Do you want to redefine it?';
  sdxSpellCheckerAutoCorrectOptionsFormCaption = 'AutoCorrect Options';
  sdxSpellCheckerExceptionsButton = '&Exceptions';
  sdxSpellCheckerActive = '&Active';
  sdxSpellCheckerAutoInclude = 'A&utoInclude';
  sdxSpellCheckerAutoInclude1 = 'Au&toInclude';
  sdxSpellCheckerAutoCorrectCapitalize = 'Capitalize';
  sdxSpellCheckerAutoCorrectCorrectSentenceCaps = 'Capitalize first letter of &sentences';
  sdxSpellCheckerAutoCorrectCorrectInitialCaps = 'Correct TWo &INitial CApitals';
  sdxSpellCheckerAutoCorrectCorrectCapsLock = 'Correct accidental usage of cAPS &LOCK key';
  sdxSpellCheckerAutoCorrectDisableCapsLock = 'Disable Caps Lock';
  sdxSpellCheckerAutoCorrectAutomaticallyUseSuggestions = 'Automatically use suggestions from the spelling checker';
  sdxSpellCheckerAutoCorrectReplaceTextAsYouType = 'Replace text as you type';
  sdxSpellCheckerAutoCorrectReplace = '&Replace:';
  sdxSpellCheckerAutoCorrectWith = '&With:';
  sdxSpellCheckerReplaceButton = 'Repl&ace';
  sdxSpellCheckerAutoCorrectExceptionsFormCaption = 'Exceptions';
  sdxSpellCheckerFirstLetterExceptions = 'Abbreviations (no subsequent capital)';
  sdxSpellCheckerInitialCapsExceptions = 'Words with TWo INitial CApitals';

implementation

uses
  dxCore;

procedure AddExpressSpellCheckerResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAddress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAddress);
  end;

begin
  InternalAdd('sdxSpellCheckerMoreThanOne', @sdxSpellCheckerMoreThanOne);
  InternalAdd('sdxSpellCheckerFileFormatMismatch', @sdxSpellCheckerFileFormatMismatch);
  InternalAdd('sdxSpellCheckerISpellDictionary', @sdxSpellCheckerISpellDictionary);
  InternalAdd('sdxSpellCheckerOpenOfficeDictionary', @sdxSpellCheckerOpenOfficeDictionary);
  InternalAdd('sdxSpellCheckerUserDictionary', @sdxSpellCheckerUserDictionary);
  InternalAdd('sdxSpellCheckerHunspellDictionary', @sdxSpellCheckerHunspellDictionary);
  InternalAdd('sdxSpellCheckerApplylButton', @sdxSpellCheckerApplylButton);
  InternalAdd('sdxSpellCheckerOkButton', @sdxSpellCheckerOkButton);
  InternalAdd('sdxSpellCheckerCancelButton', @sdxSpellCheckerCancelButton);
  InternalAdd('sdxSpellCheckerAddButton', @sdxSpellCheckerAddButton);
  InternalAdd('sdxSpellCheckerChangeButton', @sdxSpellCheckerChangeButton);
  InternalAdd('sdxSpellCheckerEditButton', @sdxSpellCheckerEditButton);
  InternalAdd('sdxSpellCheckerChangeAllButton', @sdxSpellCheckerChangeAllButton);
  InternalAdd('sdxSpellCheckerCloseButton', @sdxSpellCheckerCloseButton);
  InternalAdd('sdxSpellCheckerDeleteButton', @sdxSpellCheckerDeleteButton);
  InternalAdd('sdxSpellCheckerDeleteAllButton', @sdxSpellCheckerDeleteAllButton);
  InternalAdd('sdxSpellCheckerIgnoreButton', @sdxSpellCheckerIgnoreButton);
  InternalAdd('sdxSpellCheckerIgnoreAllButton', @sdxSpellCheckerIgnoreAllButton);
  InternalAdd('sdxSpellCheckerOptionsButton', @sdxSpellCheckerOptionsButton);
  InternalAdd('sdxSpellCheckerSuggestButton', @sdxSpellCheckerSuggestButton);
  InternalAdd('sdxSpellCheckerUndoButton', @sdxSpellCheckerUndoButton);
  InternalAdd('sdxSpellCheckerUndoEditButton', @sdxSpellCheckerUndoEditButton);
  InternalAdd('sdxSpellCheckerUndoLastButton', @sdxSpellCheckerUndoLastButton);
  InternalAdd('sdxSpellCheckerChangeTo', @sdxSpellCheckerChangeTo);
  InternalAdd('sdxSpellCheckerRepeatedWord', @sdxSpellCheckerRepeatedWord);
  InternalAdd('sdxSpellCheckerSuggestions', @sdxSpellCheckerSuggestions);
  InternalAdd('sdxSpellCheckerNotInDictionary', @sdxSpellCheckerNotInDictionary);
  InternalAdd('sdxSpellCheckerNoSuggestions', @sdxSpellCheckerNoSuggestions);
  InternalAdd('sdxSpellCheckerCustomDictionaryFormCaption', @sdxSpellCheckerCustomDictionaryFormCaption);
  InternalAdd('sdxSpellCheckerSpellingFormCaption', @sdxSpellCheckerSpellingFormCaption);
  InternalAdd('sdxSpellCheckerSpellingComplete', @sdxSpellCheckerSpellingComplete);
  InternalAdd('sdxSpellCheckerNoActiveDictionaries', @sdxSpellCheckerNoActiveDictionaries);
  InternalAdd('sdxSpellCheckerNotUseChangeAll', @sdxSpellCheckerNotUseChangeAll);
  InternalAdd('sdxSpellCheckerConfirmUseUnknownWord', @sdxSpellCheckerConfirmUseUnknownWord);
  InternalAdd('sdxSpellCheckerSelectionCheckIsFinished', @sdxSpellCheckerSelectionCheckIsFinished);
  InternalAdd('sdxSpellCheckerSpellingOptionsFormCaption', @sdxSpellCheckerSpellingOptionsFormCaption);
  InternalAdd('sdxSpellCheckerSpellingOptionsMainGroupBox', @sdxSpellCheckerSpellingOptionsMainGroupBox);
  InternalAdd('sdxSpellCheckerSpellingLanguage', @sdxSpellCheckerSpellingLanguage);
  InternalAdd('sdxSpellCheckerSpellingOptionsInternationalDictionariesGroupBox', @sdxSpellCheckerSpellingOptionsInternationalDictionariesGroupBox);
  InternalAdd('sdxSpellCheckerSpellingOptionsInternationalDictionariesText', @sdxSpellCheckerSpellingOptionsInternationalDictionariesText);
  InternalAdd('sdxSpellCheckerSpellingOptionsEditCustomDictionaryGroupBox', @sdxSpellCheckerSpellingOptionsEditCustomDictionaryGroupBox);
  InternalAdd('sdxSpellCheckerSpellingOptionsEditCustomDictionaryText', @sdxSpellCheckerSpellingOptionsEditCustomDictionaryText);
  InternalAdd('sdxSpellCheckerSpellingOptionsGeneralOptionsGroupBox', @sdxSpellCheckerSpellingOptionsGeneralOptionsGroupBox);
  InternalAdd('sdxSpellCheckerIgnoreUpperCaseWords', @sdxSpellCheckerIgnoreUpperCaseWords);
  InternalAdd('sdxSpellCheckerIgnoreMixedCaseWords', @sdxSpellCheckerIgnoreMixedCaseWords);
  InternalAdd('sdxSpellCheckerIgnoreWordsWithNumbers', @sdxSpellCheckerIgnoreWordsWithNumbers);
  InternalAdd('sdxSpellCheckerIgnoreRepeatedWords', @sdxSpellCheckerIgnoreRepeatedWords);
  InternalAdd('sdxSpellCheckerIgnoreEmails', @sdxSpellCheckerIgnoreEmails);
  InternalAdd('sdxSpellCheckerIgnoreUrls', @sdxSpellCheckerIgnoreUrls);
  InternalAdd('sdxSpellCheckerAutoCorrect', @sdxSpellCheckerAutoCorrect);
  InternalAdd('sdxSpellCheckerAutoCorrectOptionsFormCaption', @sdxSpellCheckerAutoCorrectOptionsFormCaption);
  InternalAdd('sdxSpellCheckerExceptionsButton', @sdxSpellCheckerExceptionsButton);
  InternalAdd('sdxSpellCheckerActive', @sdxSpellCheckerActive);
  InternalAdd('sdxSpellCheckerAutoInclude', @sdxSpellCheckerAutoInclude);
  InternalAdd('sdxSpellCheckerAutoCorrectReplacementExistMessageFormat', @sdxSpellCheckerAutoCorrectReplacementExistMessageFormat);
  InternalAdd('sdxSpellCheckerAutoCorrectCapitalize', @sdxSpellCheckerAutoCorrectCapitalize);
  InternalAdd('sdxSpellCheckerAutoCorrectCorrectSentenceCaps', @sdxSpellCheckerAutoCorrectCorrectSentenceCaps);
  InternalAdd('sdxSpellCheckerAutoCorrectCorrectInitialCaps', @sdxSpellCheckerAutoCorrectCorrectInitialCaps);
  InternalAdd('sdxSpellCheckerAutoCorrectCorrectCapsLock', @sdxSpellCheckerAutoCorrectCorrectCapsLock);
  InternalAdd('sdxSpellCheckerAutoCorrectDisableCapsLock', @sdxSpellCheckerAutoCorrectDisableCapsLock);
  InternalAdd('sdxSpellCheckerAutoCorrectReplaceTextAsYouType', @sdxSpellCheckerAutoCorrectReplaceTextAsYouType);
  InternalAdd('sdxSpellCheckerAutoCorrectAutomaticallyUseSuggestions', @sdxSpellCheckerAutoCorrectAutomaticallyUseSuggestions);
  InternalAdd('sdxSpellCheckerAutoCorrectReplace', @sdxSpellCheckerAutoCorrectReplace);
  InternalAdd('sdxSpellCheckerAutoCorrectWith', @sdxSpellCheckerAutoCorrectWith);
  InternalAdd('sdxSpellCheckerReplaceButton', @sdxSpellCheckerReplaceButton);
  InternalAdd('sdxSpellCheckerAutoCorrectExceptionsFormCaption', @sdxSpellCheckerAutoCorrectExceptionsFormCaption);
  InternalAdd('sdxSpellCheckerFirstLetterExceptions', @sdxSpellCheckerFirstLetterExceptions);
  InternalAdd('sdxSpellCheckerInitialCapsExceptions', @sdxSpellCheckerInitialCapsExceptions);
  InternalAdd('sdxSpellCheckerAutoInclude1', @sdxSpellCheckerAutoInclude1);
  InternalAdd('sdxSpellCheckerDeleteButton1', @sdxSpellCheckerDeleteButton1);
  InternalAdd('sdxSpellCheckerDeleteButton2', @sdxSpellCheckerDeleteButton2);
  InternalAdd('sdxSpellCheckerAddButton1', @sdxSpellCheckerAddButton1);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressSpellChecker', @AddExpressSpellCheckerResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressSpellChecker');

end.
