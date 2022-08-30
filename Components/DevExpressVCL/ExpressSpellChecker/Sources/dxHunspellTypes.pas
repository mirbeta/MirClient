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

unit dxHunspellTypes;

{$I cxVer.inc}

interface

uses
  SysUtils, dxCore;

const
  dxMaxWordLength = 100;
  dxMaxLineLength = 16000;

  LanguageNone = 0;
  LanguageHungarian = 1038;

type
  PPWideChar = ^PWideChar;

  PWord = ^Word;
  PPWord = ^PWord;

  TWordArray = array[0..0] of Word;
  PWordArray = ^TWordArray;

  TIntegerArray = array[0..0] of Integer;
  PIntegerArray = ^TIntegerArray;

  TdxLineBuffer = array[0..dxMaxLineLength - 1] of WideChar;

  PdxAffixFlagsData = ^TdxAffixFlagsData;
  TdxAffixFlagsData = array[0..0] of Word;

const
  diAf = 'AF';
  diAm = 'AM';
  diBreak = 'BREAK';
  diCheckCompoundCase = 'CHECKCOMPOUNDCASE';
  diCheckCompoundDup = 'CHECKCOMPOUNDDUP';
  diCheckCompoundPattern = 'CHECKCOMPOUNDPATTERN';
  diCheckCompoundRep = 'CHECKCOMPOUNDREP';
  diCheckCompoundTriple = 'CHECKCOMPOUNDTRIPLE';
  diCheckSharps = 'CHECKSHARPS';
  diCircumfix = 'CIRCUMFIX';
  diComplexPrefixes = 'COMPLEXPREFIXES';
  diCompoundFlag = 'COMPOUNDFLAG';
  diCompoundBegin = 'COMPOUNDBEGIN';
  diCompoundEnd = 'COMPOUNDEND';
  diCompoundMiddle = 'COMPOUNDMIDDLE';
  diCompoundWordMax = 'COMPOUNDWORDMAX';
  diCompoundRoot = 'COMPOUNDROOT';
  diCompoundPermitFlag = 'COMPOUNDPERMITFLAG';
  diCompoundForbidFlag = 'COMPOUNDFORBIDFLAG';
  diCompoundMin = 'COMPOUNDMIN';
  diCompoundRule = 'COMPOUNDRULE';
  diCompoundSyllable = 'COMPOUNDSYLLABLE';
  diFlag = 'FLAG';
  diForbiddenWord = 'FORBIDDENWORD';
  diFullStrip = 'FULLSTRIP';
  diIConv = 'ICONV';
  diIgnore = 'IGNORE';
  diKeepCase = 'KEEPCASE';
  diKey = 'KEY';
  diLang = 'LANG';
  diLemmaPresent = 'LEMMA_PRESENT';
  diMap = 'MAP';
  diNeedAffix = 'NEEDAFFIX';
  diMaxNgramSuggestions = 'MAXNGRAMSUGS';
  diNoSplitSuggestions = 'NOSPLITSUGS';
  diNoSuggest = 'NOSUGGEST';
  diOConv = 'OCONV';
  diInCompoundOnly = 'ONLYINCOMPOUND';
  diPhone = 'PHONE';
  diPrefix = 'PFX';
  diPseudoRoot = 'PSEUDOROOT';
  diRep = 'REP';
  diSet = 'SET';
  diSimplifiedTriple = 'SIMPLIFIEDTRIPLE';
  diSubStandard = 'SUBSTANDARD';
  diSuffix = 'SFX';
  diSuggestionsWithDots = 'SUGSWITHDOTS';
  diSyllableNum = 'SYLLABLENUM';
  diTry = 'TRY';
  diVersion = 'VERSION';
  diWordChars = 'WORDCHARS';

implementation

end.
