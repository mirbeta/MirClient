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

unit dxRichEdit.Import.Rtf.DestinationDefault;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Generics.Defaults, Generics.Collections, dxCoreClasses,

  dxGenerics,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Import.Rtf.DestinationPieceTable;

type
  TdxDefaultDestination = class(TdxDestinationPieceTable)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  private
    procedure ApplyStyleLinks;

    class procedure AddDocumentProtectionKeywords(ATable: TdxKeywordTranslatorTable); static;
    class procedure AddDocumentPropertiesKeywords(ATable: TdxKeywordTranslatorTable); static;
    class procedure AddSectionKeywords(ATable: TdxKeywordTranslatorTable); static;
    class procedure AddFootNoteAndEndNoteKeywords(ATable: TdxKeywordTranslatorTable); static;
    class procedure AddCommentKeywords(ATable: TdxKeywordTranslatorTable); static;

    class procedure AnnotationProtectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure BottomMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ColumnBreakKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure CommentAnnotationKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure CommentAuthorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure CommentEndPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure CommentIdKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure CommentStartPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DefaultCharacterPropertiesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DefaultParagraphPropertiesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DefaultTabKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DeffKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DisplayBackgroundShapeHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DocumentProtectionLevelHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNoteNumberingChicagoHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNoteNumberingChosungHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNoteNumberingDecimalEnclosedCircleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNoteNumberingDecimalFullWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNoteNumberingDecimalHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNoteNumberingGanadaHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNoteNumberingLowerCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNoteNumberingLowerCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNoteNumberingRestartContinuousHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNoteNumberingRestartEachSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNoteNumberingStartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNoteNumberingUpperCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNoteNumberingUpperCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNotePlacementBelowTextHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNotePlacementEndOfDocumentHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNotePlacementEndOfSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EndNotePlacementPageBottomHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure EnforceDocumentProtectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FooterForFirstPageKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FooterForLeftPagesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FooterForRightPagesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FooterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingChicagoHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingChosungHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingDecimalEnclosedCircleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingDecimalFullWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingDecimalHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingGanadaHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingLowerCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingLowerCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingRestartContinuousHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingRestartEachPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingRestartEachSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingStartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingUpperCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNoteNumberingUpperCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNotePlacementBelowTextHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNotePlacementEndOfDocumentHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNotePlacementEndOfSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FootNotePlacementPageBottomHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure GutterAtRightHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure GutterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure HeaderForFirstPageKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure HeaderForLeftPagesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure HeaderForRightPagesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure HeaderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure InfoKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure LandscapeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure LeftMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure LineNumberingContinuousHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure LineNumberingDistanceHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure LineNumberingRestartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure LineNumberingRestartOnEachPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure LineNumberingStartingLineNumberHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure LineNumberingStepHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListOverrideTableKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListTableKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure NewSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure OnlyAllowEditingOfFormFieldsHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageBackgroundHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageBreakKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageFacingHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingArabicAbjadHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingArabicAlphaHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingChapterHeaderStyleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingChapterSeparatorColonHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingChapterSeparatorEmDashHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingChapterSeparatorEnDashHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingChapterSeparatorHyphenHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingChapterSeparatorPeriodHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingChosungHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingDecimalEnclosedCircleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingDecimalFullWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingDecimalHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingGanadaHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingHindiConsonantsHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingHindiDescriptiveHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingHindiNumbersHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingHindiVowelsHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingLowerLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingLowerRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingStartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingThaiDescriptiveHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingThaiLettersHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingThaiNumbersHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingUpperLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingUpperRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PageNumberingVietnameseDescriptiveHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PaperHeightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PaperSizeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PaperWidthKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ReadOnlyProtectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure RightMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure RtfKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionBottomMarginHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionBreakColumnHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionBreakEvenPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionBreakNoneHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionBreakOddPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionBreakPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionColumnCountHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionColumnSpaceHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionCurrentColumnIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionCurrentColumnSpaceHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionCurrentColumnWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionDefaultHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionDifferentFirstPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionDrawVerticalSeparatorHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionEndNoteNumberingChicagoHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionEndNoteNumberingChosungHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionEndNoteNumberingDecimalEnclosedCircleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionEndNoteNumberingDecimalFullWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionEndNoteNumberingDecimalHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionEndNoteNumberingGanadaHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionEndNoteNumberingLowerCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionEndNoteNumberingLowerCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionEndNoteNumberingRestartContinuousHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionEndNoteNumberingRestartEachSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionEndNoteNumberingStartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionEndNoteNumberingUpperCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionEndNoteNumberingUpperCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFirstPagePaperSourceHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFooterOffsetHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingChicagoHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingChosungHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingDecimalEnclosedCircleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingDecimalFullWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingDecimalHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingGanadaHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingLowerCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingLowerCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingRestartContinuousHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingRestartEachPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingRestartEachSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingStartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingUpperCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNoteNumberingUpperCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNotePlacementBelowTextHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionFootNotePlacementPageBottomHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionGutterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionHeaderOffsetHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionLandscapeHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionLeftMarginHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionOtherPagePaperSourceHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionPageNumberingContinuousHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionPageNumberingRestartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionPageNumberingStartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionPaperHeightHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionPaperWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionRightMarginHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionTextFlowHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SectionTopMarginHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure StyleSheetKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure TopMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure VerticalTextAlignmentBottomHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure VerticalTextAlignmentCenterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure VerticalTextAlignmentJustifyHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure VerticalTextAlignmentTopHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    procedure FixLastParagraph; override;
  public
    procedure NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase); override;
  end;

implementation

uses
  dxRichEdit.Import.Rtf.DestinationHeaderFooter,
  dxRichEdit.Import.Rtf.DestinationInfo,
  dxRichEdit.Import.Rtf.DestinationStyleSheet,
  dxRichEdit.Import.Rtf.DestinationListTable,
  dxRichEdit.Import.Rtf.DestinationSkip,
  dxRichEdit.Import.Rtf.ListConverter,
  dxRichEdit.Import.Rtf.DestinationsDefaultPropertes,
  dxRichEdit.Import.Rtf.DestinationPageBackground,
  dxRichEdit.Import.Rtf.DestinationFootNote,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxCharacters,
  dxRichEdit.Platform.Font,
  dxRichEdit.Utils.Types,
  dxRichEdit.Types;

{ TdxDefaultDestination }

procedure TdxDefaultDestination.NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase);
var
  AConverter: TdxRtfListConverter;
begin
  inherited NestedGroupFinished(ADestination);

  if ADestination is TdxListTableDestination then
    Importer.DocumentProperties.ListTableComplete := True;

  if ADestination is TdxListOverrideTableDestination then
    Importer.DocumentProperties.ListOverrideTableComplete := True;

  if Importer.DocumentProperties.ListOverrideTableComplete and Importer.DocumentProperties.ListTableComplete then
  begin
    AConverter := TdxRtfListConverter.Create(Importer);
    try
      AConverter.Convert(Importer.DocumentProperties.ListTable, Importer.DocumentProperties.ListOverrideTable);
    finally
      AConverter.Free;
    end;
    Importer.DocumentProperties.ListTableComplete := False;
    Importer.DocumentProperties.ListOverrideTableComplete := False;
  end;

  if ADestination is TdxStyleSheetDestination then
    ApplyStyleLinks;

  if ADestination is TdxDestinationPieceTable then
    if ADestination.PieceTable <> PieceTable then
      TdxDestinationPieceTable(ADestination).FinalizePieceTableCreation;
end;

procedure TdxDefaultDestination.FixLastParagraph;
begin
  Importer.ApplySectionFormatting(True);
  if not Importer.Options.SuppressLastParagraphDelete and PieceTable.ShouldFixLastParagraph then
    PieceTable.FixLastParagraphCore
  else
    Importer.ApplyFormattingToLastParagraph;

  if not PieceTable.DocumentModel.DocumentCapabilities.ParagraphsAllowed then
    PieceTable.UnsafeRemoveLastSpaceSymbolRun;

  PieceTable.FixTables;
end;

procedure TdxDefaultDestination.ApplyStyleLinks;
var
  AStyleLinks: TdxIntegersDictionary;
  ADocumentModel: TdxDocumentModel;
  ACharacterStyles: TdxCharacterStyleCollection;
  AParagraphStyles: TdxParagraphStyleCollection;
  ARtfParagraphStyleIndex: Integer;
  ARtfCharacterStyleIndex: Integer;
  AParagraphStyleIndex, ACharacterStyleIndex: Integer;
  ACharacterStyle: TdxCharacterStyle;
  AParagraphStyle: TdxParagraphStyle;
  ARtfNextStyleIndex, ANextStyleIndex: Integer;
begin
  AStyleLinks := Importer.LinkParagraphStyleIndexToCharacterStyleIndex;
  ADocumentModel := Importer.DocumentModel;
  ACharacterStyles := ADocumentModel.CharacterStyles;
  AParagraphStyles := ADocumentModel.ParagraphStyles;
  for ARtfParagraphStyleIndex in AStyleLinks.Keys do
  begin
    ARtfCharacterStyleIndex := AStyleLinks[ARtfParagraphStyleIndex];
    if Importer.ParagraphStyleCollectionIndex.TryGetValue(ARtfParagraphStyleIndex, AParagraphStyleIndex) and
      Importer.CharacterStyleCollectionIndex.TryGetValue(ARtfCharacterStyleIndex, ACharacterStyleIndex) then
    begin
      ACharacterStyle := ACharacterStyles[ACharacterStyleIndex];
      AParagraphStyle := AParagraphStyles[AParagraphStyleIndex];
      ADocumentModel.StyleLinkManager.CreateLinkCore(AParagraphStyle, ACharacterStyle);
    end;
  end;
  AStyleLinks := Importer.NextParagraphStyleIndexTable;
  for ARtfParagraphStyleIndex in AStyleLinks.Keys do
  begin
    ARtfNextStyleIndex := AStyleLinks[ARtfParagraphStyleIndex];
    if Importer.ParagraphStyleCollectionIndex.TryGetValue(ARtfParagraphStyleIndex, AParagraphStyleIndex) then
    begin
      if Importer.ParagraphStyleCollectionIndex.TryGetValue(ARtfNextStyleIndex, ANextStyleIndex) then
        AParagraphStyles[AParagraphStyleIndex].NextParagraphStyle := AParagraphStyles[ANextStyleIndex];
    end;
  end;
end;

class procedure TdxDefaultDestination.AddDocumentProtectionKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('enforceprot', EnforceDocumentProtectionHandler);
  ATable.Add('annotprot', AnnotationProtectionHandler);
  ATable.Add('readprot', ReadOnlyProtectionHandler);
  ATable.Add('protlevel', DocumentProtectionLevelHandler);
end;

class procedure TdxDefaultDestination.AddDocumentPropertiesKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('paperw', PaperWidthKeywordHandler);
  ATable.Add('paperh', PaperHeightKeywordHandler);
  ATable.Add('psz', PaperSizeKeywordHandler);
  ATable.Add('landscape', LandscapeKeywordHandler);
  ATable.Add('gutter', GutterKeywordHandler);
  ATable.Add('margl', LeftMarginKeywordHandler);
  ATable.Add('margr', RightMarginKeywordHandler);
  ATable.Add('margt', TopMarginKeywordHandler);
  ATable.Add('margb', BottomMarginKeywordHandler);
  ATable.Add('rtlgutter', GutterAtRightHandler);
  ATable.Add('pgnstart', PageNumberingStartHandler);
  ATable.Add('facingp', PageFacingHandler);
  ATable.Add('endnotes', FootNotePlacementEndOfSectionHandler);
  ATable.Add('enddoc', FootNotePlacementEndOfDocumentHandler);
  ATable.Add('ftntj', FootNotePlacementBelowTextHandler);
  ATable.Add('ftnbj', FootNotePlacementPageBottomHandler);
  ATable.Add('ftnstart', FootNoteNumberingStartHandler);
  ATable.Add('ftnrstpg', FootNoteNumberingRestartEachPageHandler);
  ATable.Add('ftnrestart', FootNoteNumberingRestartEachSectionHandler);
  ATable.Add('ftnrstcont', FootNoteNumberingRestartContinuousHandler);
  ATable.Add('ftnnar', FootNoteNumberingDecimalHandler);
  ATable.Add('ftnnalc', FootNoteNumberingLowerCaseLetterHandler);
  ATable.Add('ftnnauc', FootNoteNumberingUpperCaseLetterHandler);
  ATable.Add('ftnnrlc', FootNoteNumberingLowerCaseRomanHandler);
  ATable.Add('ftnnruc', FootNoteNumberingUpperCaseRomanHandler);
  ATable.Add('ftnnchi', FootNoteNumberingChicagoHandler);
  ATable.Add('ftnnchosung', FootNoteNumberingChosungHandler);
  ATable.Add('ftnncnum', FootNoteNumberingDecimalEnclosedCircleHandler);
  ATable.Add('ftnndbar', FootNoteNumberingDecimalFullWidthHandler);
  ATable.Add('ftnnganada', FootNoteNumberingGanadaHandler);
  ATable.Add('aendnotes', EndNotePlacementEndOfSectionHandler);
  ATable.Add('aenddoc', EndNotePlacementEndOfDocumentHandler);
  ATable.Add('aftntj', EndNotePlacementBelowTextHandler);
  ATable.Add('aftnbj', EndNotePlacementPageBottomHandler);
  ATable.Add('aftnstart', EndNoteNumberingStartHandler);
  ATable.Add('aftnrestart', EndNoteNumberingRestartEachSectionHandler);
  ATable.Add('aftnrstcont', EndNoteNumberingRestartContinuousHandler);
  ATable.Add('aftnnar', EndNoteNumberingDecimalHandler);
  ATable.Add('aftnnalc', EndNoteNumberingLowerCaseLetterHandler);
  ATable.Add('aftnnauc', EndNoteNumberingUpperCaseLetterHandler);
  ATable.Add('aftnnrlc', EndNoteNumberingLowerCaseRomanHandler);
  ATable.Add('aftnnruc', EndNoteNumberingUpperCaseRomanHandler);
  ATable.Add('aftnnchi', EndNoteNumberingChicagoHandler);
  ATable.Add('aftnnchosung', EndNoteNumberingChosungHandler);
  ATable.Add('aftnncnum', EndNoteNumberingDecimalEnclosedCircleHandler);
  ATable.Add('aftnndbar', EndNoteNumberingDecimalFullWidthHandler);
  ATable.Add('aftnnganada', EndNoteNumberingGanadaHandler);
  ATable.Add('viewbksp', DisplayBackgroundShapeHandler);
end;

class procedure TdxDefaultDestination.AddSectionKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('pgndec', PageNumberingDecimalHandler);
  ATable.Add('pgnucrm', PageNumberingUpperRomanHandler);
  ATable.Add('pgnlcrm', PageNumberingLowerRomanHandler);
  ATable.Add('pgnucltr', PageNumberingUpperLetterHandler);
  ATable.Add('pgnlcltr', PageNumberingLowerLetterHandler);
  ATable.Add('pgnbidia', PageNumberingArabicAbjadHandler);
  ATable.Add('pgnbidib', PageNumberingArabicAlphaHandler);
  ATable.Add('pgnchosung', PageNumberingChosungHandler);
  ATable.Add('pgncnum', PageNumberingDecimalEnclosedCircleHandler);
  ATable.Add('pgndecd', PageNumberingDecimalFullWidthHandler);
  ATable.Add('pgnganada', PageNumberingGanadaHandler);
  ATable.Add('pgnhindia', PageNumberingHindiVowelsHandler);
  ATable.Add('pgnhindib', PageNumberingHindiConsonantsHandler);
  ATable.Add('pgnhindic', PageNumberingHindiNumbersHandler);
  ATable.Add('pgnhindid', PageNumberingHindiDescriptiveHandler);
  ATable.Add('pgnthaia', PageNumberingThaiLettersHandler);
  ATable.Add('pgnthaib', PageNumberingThaiNumbersHandler);
  ATable.Add('pgnthaic', PageNumberingThaiDescriptiveHandler);
  ATable.Add('pgnvieta', PageNumberingVietnameseDescriptiveHandler);
  ATable.Add('pgnhn', PageNumberingChapterHeaderStyleHandler);
  ATable.Add('pgnhnsh', PageNumberingChapterSeparatorHyphenHandler);
  ATable.Add('pgnhnsp', PageNumberingChapterSeparatorPeriodHandler);
  ATable.Add('pgnhnsc', PageNumberingChapterSeparatorColonHandler);
  ATable.Add('pgnhnsm', PageNumberingChapterSeparatorEmDashHandler);
  ATable.Add('pgnhnsn', PageNumberingChapterSeparatorEnDashHandler);
  ATable.Add('vertal', VerticalTextAlignmentBottomHandler);
  ATable.Add('vertalb', VerticalTextAlignmentBottomHandler);
  ATable.Add('vertalt', VerticalTextAlignmentTopHandler);
  ATable.Add('vertalc', VerticalTextAlignmentCenterHandler);
  ATable.Add('vertalj', VerticalTextAlignmentJustifyHandler);
  ATable.Add('pgnstarts', SectionPageNumberingStartHandler);
  ATable.Add('pgncont', SectionPageNumberingContinuousHandler);
  ATable.Add('pgnrestart', SectionPageNumberingRestartHandler);
  ATable.Add('stextflow', SectionTextFlowHandler);
  ATable.Add('linemod', LineNumberingStepHandler);
  ATable.Add('linex', LineNumberingDistanceHandler);
  ATable.Add('linestarts', LineNumberingStartingLineNumberHandler);
  ATable.Add('linerestart', LineNumberingRestartHandler);
  ATable.Add('lineppage', LineNumberingRestartOnEachPageHandler);
  ATable.Add('linecont', LineNumberingContinuousHandler);
  ATable.Add('pgwsxn', SectionPaperWidthHandler);
  ATable.Add('pghsxn', SectionPaperHeightHandler);
  ATable.Add('lndscpsxn', SectionLandscapeHandler);
  ATable.Add('marglsxn', SectionLeftMarginHandler);
  ATable.Add('margrsxn', SectionRightMarginHandler);
  ATable.Add('margtsxn', SectionTopMarginHandler);
  ATable.Add('margbsxn', SectionBottomMarginHandler);
  ATable.Add('guttersxn', SectionGutterHandler);
  ATable.Add('headery', SectionHeaderOffsetHandler);
  ATable.Add('footery', SectionFooterOffsetHandler);
  ATable.Add('binfsxn', SectionFirstPagePaperSourceHandler);
  ATable.Add('binsxn', SectionOtherPagePaperSourceHandler);
  ATable.Add('sectunlocked', OnlyAllowEditingOfFormFieldsHandler);
  ATable.Add('cols', SectionColumnCountHandler);
  ATable.Add('colsx', SectionColumnSpaceHandler);
  ATable.Add('colno', SectionCurrentColumnIndexHandler);
  ATable.Add('colsr', SectionCurrentColumnSpaceHandler);
  ATable.Add('colw', SectionCurrentColumnWidthHandler);
  ATable.Add('linebetcol', SectionDrawVerticalSeparatorHandler);
  ATable.Add('sbknone', SectionBreakNoneHandler);
  ATable.Add('sbkcol', SectionBreakColumnHandler);
  ATable.Add('sbkpage', SectionBreakPageHandler);
  ATable.Add('sbkeven', SectionBreakEvenPageHandler);
  ATable.Add('sbkodd', SectionBreakOddPageHandler);
  ATable.Add('sectd', SectionDefaultHandler);
  ATable.Add('sect', NewSectionHandler);
  ATable.Add('titlepg', SectionDifferentFirstPageHandler);
  ATable.Add('header', HeaderKeywordHandler);
  ATable.Add('headerl', HeaderForLeftPagesKeywordHandler);
  ATable.Add('headerr', HeaderForRightPagesKeywordHandler);
  ATable.Add('headerf', HeaderForFirstPageKeywordHandler);
  ATable.Add('footer', FooterKeywordHandler);
  ATable.Add('footerl', FooterForLeftPagesKeywordHandler);
  ATable.Add('footerr', FooterForRightPagesKeywordHandler);
  ATable.Add('footerf', FooterForFirstPageKeywordHandler);
  ATable.Add('sftntj', SectionFootNotePlacementBelowTextHandler);
  ATable.Add('sftnbj', SectionFootNotePlacementPageBottomHandler);
  ATable.Add('sftnstart', SectionFootNoteNumberingStartHandler);
  ATable.Add('sftnrstpg', SectionFootNoteNumberingRestartEachPageHandler);
  ATable.Add('sftnrestart', SectionFootNoteNumberingRestartEachSectionHandler);
  ATable.Add('sftnrstcont', SectionFootNoteNumberingRestartContinuousHandler);
  ATable.Add('sftnnar', SectionFootNoteNumberingDecimalHandler);
  ATable.Add('sftnnalc', SectionFootNoteNumberingLowerCaseLetterHandler);
  ATable.Add('sftnnauc', SectionFootNoteNumberingUpperCaseLetterHandler);
  ATable.Add('sftnnrlc', SectionFootNoteNumberingLowerCaseRomanHandler);
  ATable.Add('sftnnruc', SectionFootNoteNumberingUpperCaseRomanHandler);
  ATable.Add('sftnnchi', SectionFootNoteNumberingChicagoHandler);
  ATable.Add('sftnnchosung', SectionFootNoteNumberingChosungHandler);
  ATable.Add('sftnncnum', SectionFootNoteNumberingDecimalEnclosedCircleHandler);
  ATable.Add('sftnndbar', SectionFootNoteNumberingDecimalFullWidthHandler);
  ATable.Add('sftnnganada', SectionFootNoteNumberingGanadaHandler);
  ATable.Add('saftnstart', SectionEndNoteNumberingStartHandler);
  ATable.Add('saftnrestart', SectionEndNoteNumberingRestartEachSectionHandler);
  ATable.Add('saftnrstcont', SectionEndNoteNumberingRestartContinuousHandler);
  ATable.Add('saftnnar', SectionEndNoteNumberingDecimalHandler);
  ATable.Add('saftnnalc', SectionEndNoteNumberingLowerCaseLetterHandler);
  ATable.Add('saftnnauc', SectionEndNoteNumberingUpperCaseLetterHandler);
  ATable.Add('saftnnrlc', SectionEndNoteNumberingLowerCaseRomanHandler);
  ATable.Add('saftnnruc', SectionEndNoteNumberingUpperCaseRomanHandler);
  ATable.Add('saftnnchi', SectionEndNoteNumberingChicagoHandler);
  ATable.Add('saftnnchosung', SectionEndNoteNumberingChosungHandler);
  ATable.Add('saftnncnum', SectionEndNoteNumberingDecimalEnclosedCircleHandler);
  ATable.Add('saftnndbar', SectionEndNoteNumberingDecimalFullWidthHandler);
  ATable.Add('saftnnganada', SectionEndNoteNumberingGanadaHandler);
end;

class procedure TdxDefaultDestination.AddFootNoteAndEndNoteKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('footnote', FootNoteKeywordHandler);
end;

class procedure TdxDefaultDestination.AddCommentKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('atrfstart', CommentStartPositionKeywordHandler);
  ATable.Add('atrfend', CommentEndPositionKeywordHandler);
  ATable.Add('atnid', CommentIdKeywordHandler);
  ATable.Add('atnauthor', CommentAuthorKeywordHandler);
  ATable.Add('annotation', CommentAnnotationKeywordHandler);
end;

class procedure TdxDefaultDestination.AnnotationProtectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
//do nothing
end;

class procedure TdxDefaultDestination.BottomMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 1440;
  AParameterValue := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
  AImporter.DocumentProperties.DefaultSectionProperties.Margins.Bottom := AParameterValue;
  AImporter.Position.SectionFormattingInfo.Margins.Bottom := AParameterValue;
end;

class procedure TdxDefaultDestination.ColumnBreakKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.ParseCharWithoutDecoding(TdxCharacters.ColumnBreak);
end;

class procedure TdxDefaultDestination.CommentAnnotationKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.CommentAuthorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.CommentEndPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.CommentIdKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.CommentStartPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

function TdxDefaultDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxDefaultDestination.Create(Importer, PieceTable);
end;

class function TdxDefaultDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class constructor TdxDefaultDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxDefaultDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxDefaultDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('rtf', RtfKeywordHandler);
  Result.Add('deff', DeffKeywordHandler);
  Result.Add('info', InfoKeywordHandler);
  Result.Add('stylesheet', StyleSheetKeywordHandler);
  Result.Add('defchp', DefaultCharacterPropertiesKeywordHandler);
  Result.Add('defpap', DefaultParagraphPropertiesKeywordHandler);
  AddCommonCharacterKeywords(Result);
  AddCommonParagraphKeywords(Result);
  AddCommonSymbolsAndObjectsKeywords(Result);
  Result.Add('page', PageBreakKeywordHandler);
  Result.Add('column', ColumnBreakKeywordHandler);
  AddCommonTabKeywords(Result);
  Result.Add('deftab', DefaultTabKeywordHandler);
  AddDocumentProtectionKeywords(Result);
  AddCommonNumberingListsKeywords(Result);
  Result.Add('listtable', ListTableKeywordHandler);
  Result.Add('listoverridetable', ListOverrideTableKeywordHandler);
  Result.Add('background', PageBackgroundHandler);
  AppendTableKeywords(Result);
  AddDocumentPropertiesKeywords(Result);
  AddSectionKeywords(Result);
  AddFootNoteAndEndNoteKeywords(Result);
  AddCommentKeywords(Result);
end;

class procedure TdxDefaultDestination.DefaultCharacterPropertiesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxDefaultCharacterPropertiesDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.DefaultParagraphPropertiesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxDefaultParagraphPropertiesDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.DefaultTabKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter or (AParameterValue <= 0) then
    AParameterValue := 720;
  AImporter.DocumentModel.DocumentProperties.DefaultTabWidth := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDefaultDestination.DeffKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultFontNumber := AParameterValue;
end;

class procedure TdxDefaultDestination.DisplayBackgroundShapeHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentModel.DocumentProperties.DisplayBackgroundShape := AParameterValue = 1;
end;

class procedure TdxDefaultDestination.DocumentProtectionLevelHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AProperties: TdxDocumentProtectionProperties;
begin
  if AHasParameter and (AParameterValue = 3) then
  begin
    AProperties := AImporter.DocumentModel.ProtectionProperties;
    AProperties.BeginInit;
    try
      AProperties.ProtectionType := TdxDocumentProtectionType.ReadOnly;
    finally
      AProperties.EndInit;
    end;
  end;
end;

class procedure TdxDefaultDestination.EndNoteNumberingChicagoHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.NumberingFormat := TdxNumberingFormat.Chicago;
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.Chicago;
end;

class procedure TdxDefaultDestination.EndNoteNumberingChosungHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.NumberingFormat := TdxNumberingFormat.Chosung;
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.Chosung;
end;

class procedure TdxDefaultDestination.EndNoteNumberingDecimalEnclosedCircleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.NumberingFormat := TdxNumberingFormat.DecimalEnclosedCircle;
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.DecimalEnclosedCircle;
end;

class procedure TdxDefaultDestination.EndNoteNumberingDecimalFullWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.NumberingFormat := TdxNumberingFormat.DecimalFullWidth;
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.DecimalFullWidth;
end;

class procedure TdxDefaultDestination.EndNoteNumberingDecimalHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.NumberingFormat := TdxNumberingFormat.Decimal;
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.Decimal;
end;

class procedure TdxDefaultDestination.EndNoteNumberingGanadaHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.NumberingFormat := TdxNumberingFormat.Ganada;
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.Ganada;
end;

class procedure TdxDefaultDestination.EndNoteNumberingLowerCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.NumberingFormat := TdxNumberingFormat.LowerLetter;
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.LowerLetter;
end;

class procedure TdxDefaultDestination.EndNoteNumberingLowerCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.NumberingFormat := TdxNumberingFormat.LowerRoman;
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.LowerRoman;
end;

class procedure TdxDefaultDestination.EndNoteNumberingRestartContinuousHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.NumberingRestartType := TdxLineNumberingRestart.Continuous;
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingRestartType := TdxLineNumberingRestart.Continuous;
end;

class procedure TdxDefaultDestination.EndNoteNumberingRestartEachSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.NumberingRestartType := TdxLineNumberingRestart.NewSection;
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingRestartType := TdxLineNumberingRestart.NewSection;
end;

class procedure TdxDefaultDestination.EndNoteNumberingStartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter or (AParameterValue <= 0) then
    AParameterValue := 1;
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.StartingNumber := AParameterValue;
  AImporter.Position.SectionFormattingInfo.EndNote.StartingNumber := AParameterValue;
end;

class procedure TdxDefaultDestination.EndNoteNumberingUpperCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.NumberingFormat := TdxNumberingFormat.UpperLetter;
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.UpperLetter;
end;

class procedure TdxDefaultDestination.EndNoteNumberingUpperCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.NumberingFormat := TdxNumberingFormat.UpperRoman;
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.UpperRoman;
end;

class procedure TdxDefaultDestination.EndNotePlacementBelowTextHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.Position := TdxFootNotePosition.BelowText;
  AImporter.Position.SectionFormattingInfo.EndNote.Position := TdxFootNotePosition.BelowText;
end;

class procedure TdxDefaultDestination.EndNotePlacementEndOfDocumentHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.Position := TdxFootNotePosition.EndOfDocument;
  AImporter.Position.SectionFormattingInfo.EndNote.Position := TdxFootNotePosition.EndOfDocument;
end;

class procedure TdxDefaultDestination.EndNotePlacementEndOfSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.Position := TdxFootNotePosition.EndOfSection;
  AImporter.Position.SectionFormattingInfo.EndNote.Position := TdxFootNotePosition.EndOfSection;
end;

class procedure TdxDefaultDestination.EndNotePlacementPageBottomHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.EndNote.Position := TdxFootNotePosition.BottomOfPage;
  AImporter.Position.SectionFormattingInfo.EndNote.Position := TdxFootNotePosition.BottomOfPage;
end;

class procedure TdxDefaultDestination.EnforceDocumentProtectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AProperties: TdxDocumentProtectionProperties;
begin
  AProperties := AImporter.DocumentModel.ProtectionProperties;
  AProperties.BeginInit;
  try
    AProperties.EnforceProtection := True;
  finally
    AProperties.EndInit;
  end;
end;

class procedure TdxDefaultDestination.FooterForFirstPageKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ASection: TdxSection;
begin
  if AImporter.DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
  begin
    ASection := AImporter.DocumentModel.Sections.Last;
    ASection.Footers.Add(TdxHeaderFooterType.First);
    AImporter.Destination := TdxSectionPageFooterDestination.Create(AImporter, ASection, ASection.InnerFirstPageFooter);
  end
  else
    AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.FooterForLeftPagesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ASection: TdxSection;
begin
  if AImporter.DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
  begin
    ASection := AImporter.DocumentModel.Sections.Last;
    ASection.Footers.Add(TdxHeaderFooterType.Even);
    AImporter.Destination := TdxSectionPageFooterDestination.Create(AImporter, ASection, ASection.InnerEvenPageFooter);
  end
  else
    AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.FooterForRightPagesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ASection: TdxSection;
begin
  if AImporter.DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
  begin
    ASection := AImporter.DocumentModel.Sections.Last;
    ASection.Footers.Add(TdxHeaderFooterType.Odd);
    AImporter.Destination := TdxSectionPageFooterDestination.Create(AImporter, ASection, ASection.InnerOddPageFooter);
  end
  else
    AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.FooterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ASection: TdxSection;
begin
  if AImporter.DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
  begin
    ASection := AImporter.DocumentModel.Sections.Last;
    ASection.Footers.Add(TdxHeaderFooterType.Odd);
    AImporter.Destination := TdxSectionPageFooterDestination.Create(AImporter, ASection, ASection.InnerOddPageFooter);
  end
  else
    AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.FootNoteKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ANote: TdxFootNote;
begin
  if not AImporter.DocumentModel.DocumentCapabilities.FootNotesAllowed then
    Exit;
  if not AImporter.DocumentModel.DocumentCapabilities.EndNotesAllowed then
    Exit;
  ANote := TdxFootNote.Create(AImporter.DocumentModel);
  AImporter.DocumentModel.UnsafeEditor.InsertFirstParagraph(TdxPieceTable(ANote.PieceTable));
  AImporter.Destination := TdxFootNoteDestination.Create(AImporter, ANote);
end;

class procedure TdxDefaultDestination.FootNoteNumberingChicagoHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.NumberingFormat := TdxNumberingFormat.Chicago;
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.Chicago;
end;

class procedure TdxDefaultDestination.FootNoteNumberingChosungHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.NumberingFormat := TdxNumberingFormat.Chosung;
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.Chosung;
end;

class procedure TdxDefaultDestination.FootNoteNumberingDecimalEnclosedCircleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.NumberingFormat := TdxNumberingFormat.DecimalEnclosedCircle;
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.DecimalEnclosedCircle;
end;

class procedure TdxDefaultDestination.FootNoteNumberingDecimalFullWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.NumberingFormat := TdxNumberingFormat.DecimalFullWidth;
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.DecimalFullWidth;
end;

class procedure TdxDefaultDestination.FootNoteNumberingDecimalHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.NumberingFormat := TdxNumberingFormat.Decimal;
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.Decimal;
end;

class procedure TdxDefaultDestination.FootNoteNumberingGanadaHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.NumberingFormat := TdxNumberingFormat.Ganada;
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.Ganada;
end;

class procedure TdxDefaultDestination.FootNoteNumberingLowerCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.NumberingFormat := TdxNumberingFormat.LowerLetter;
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.LowerLetter;
end;

class procedure TdxDefaultDestination.FootNoteNumberingLowerCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.NumberingFormat := TdxNumberingFormat.LowerRoman;
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.LowerRoman;
end;

class procedure TdxDefaultDestination.FootNoteNumberingRestartContinuousHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.NumberingRestartType := TdxLineNumberingRestart.Continuous;
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingRestartType := TdxLineNumberingRestart.Continuous;
end;

class procedure TdxDefaultDestination.FootNoteNumberingRestartEachPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.NumberingRestartType := TdxLineNumberingRestart.NewPage;
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingRestartType := TdxLineNumberingRestart.NewPage;
end;

class procedure TdxDefaultDestination.FootNoteNumberingRestartEachSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.NumberingRestartType := TdxLineNumberingRestart.NewSection;
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingRestartType := TdxLineNumberingRestart.NewSection;
end;

class procedure TdxDefaultDestination.FootNoteNumberingStartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter or (AParameterValue <= 0) then
    AParameterValue := 1;
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.StartingNumber := AParameterValue;
  AImporter.Position.SectionFormattingInfo.FootNote.StartingNumber := AParameterValue;
end;

class procedure TdxDefaultDestination.FootNoteNumberingUpperCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.NumberingFormat := TdxNumberingFormat.UpperLetter;
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.UpperLetter;
end;

class procedure TdxDefaultDestination.FootNoteNumberingUpperCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.NumberingFormat := TdxNumberingFormat.UpperRoman;
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.UpperRoman;
end;

class procedure TdxDefaultDestination.FootNotePlacementBelowTextHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.Position := TdxFootNotePosition.BelowText;
  AImporter.Position.SectionFormattingInfo.FootNote.Position := TdxFootNotePosition.BelowText;
end;

class procedure TdxDefaultDestination.FootNotePlacementEndOfDocumentHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.Position := TdxFootNotePosition.EndOfDocument;
  AImporter.Position.SectionFormattingInfo.FootNote.Position := TdxFootNotePosition.EndOfDocument;
end;

class procedure TdxDefaultDestination.FootNotePlacementEndOfSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.Position := TdxFootNotePosition.EndOfSection;
  AImporter.Position.SectionFormattingInfo.FootNote.Position := TdxFootNotePosition.EndOfSection;
end;

class procedure TdxDefaultDestination.FootNotePlacementPageBottomHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.FootNote.Position := TdxFootNotePosition.BottomOfPage;
  AImporter.Position.SectionFormattingInfo.FootNote.Position := TdxFootNotePosition.BottomOfPage;
end;

class procedure TdxDefaultDestination.GutterAtRightHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.Margins.GutterAlignment := TdxSectionGutterAlignment.Right;
  AImporter.Position.SectionFormattingInfo.Margins.GutterAlignment := TdxSectionGutterAlignment.Right;
end;

class procedure TdxDefaultDestination.GutterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AValue: Integer;
begin
  if not AHasParameter then
    AParameterValue := 0;
  AValue := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
  AImporter.DocumentProperties.DefaultSectionProperties.Margins.Gutter := AValue;
  AImporter.Position.SectionFormattingInfo.Margins.Gutter := AValue;
end;

class procedure TdxDefaultDestination.HeaderForFirstPageKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ASection: TdxSection;
begin
  if AImporter.DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
  begin
    ASection := AImporter.DocumentModel.Sections.Last;
    ASection.Headers.Add(TdxHeaderFooterType.First);
    AImporter.Destination := TdxSectionPageHeaderDestination.Create(AImporter, ASection, ASection.InnerFirstPageHeader);
  end
  else
    AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.HeaderForLeftPagesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ASection: TdxSection;
begin
  if AImporter.DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
  begin
    ASection := AImporter.DocumentModel.Sections.Last;
    ASection.Headers.Add(TdxHeaderFooterType.Even);
    AImporter.Destination := TdxSectionPageHeaderDestination.Create(AImporter, ASection, ASection.InnerEvenPageHeader);
  end
  else
    AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.HeaderForRightPagesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ASection: TdxSection;
begin
  if AImporter.DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
  begin
    ASection := AImporter.DocumentModel.Sections.Last;
    ASection.Headers.Add(TdxHeaderFooterType.Odd);
    AImporter.Destination := TdxSectionPageHeaderDestination.Create(AImporter, ASection, ASection.InnerOddPageHeader);
  end
  else
    AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.HeaderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ASection: TdxSection;
begin
  if AImporter.DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
  begin
    ASection := AImporter.DocumentModel.Sections.Last;
    ASection.Headers.Add(TdxHeaderFooterType.Odd);
    AImporter.Destination := TdxSectionPageHeaderDestination.Create(AImporter, ASection, ASection.InnerOddPageHeader);
  end
  else
    AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.InfoKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxInfoDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.LandscapeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultSectionProperties.Page.Landscape := True;
  AImporter.Position.SectionFormattingInfo.Page.Landscape := True;
end;

class procedure TdxDefaultDestination.LeftMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 1800;
  AParameterValue := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
  AImporter.DocumentProperties.DefaultSectionProperties.Margins.Left := AParameterValue;
  AImporter.Position.SectionFormattingInfo.Margins.Left := AParameterValue;
end;

class procedure TdxDefaultDestination.LineNumberingContinuousHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.LineNumbering.NumberingRestartType :=
    TdxLineNumberingRestart.Continuous;
end;

class procedure TdxDefaultDestination.LineNumberingDistanceHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 360;
  AImporter.Position.SectionFormattingInfo.LineNumbering.Distance := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDefaultDestination.LineNumberingRestartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.LineNumbering.NumberingRestartType :=
    TdxLineNumberingRestart.NewSection;
end;

class procedure TdxDefaultDestination.LineNumberingRestartOnEachPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.LineNumbering.NumberingRestartType :=
    TdxLineNumberingRestart.NewPage;
end;

class procedure TdxDefaultDestination.LineNumberingStartingLineNumberHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 1;
  AImporter.Position.SectionFormattingInfo.LineNumbering.StartingLineNumber := AParameterValue;
end;

class procedure TdxDefaultDestination.LineNumberingStepHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 0;
  AImporter.Position.SectionFormattingInfo.LineNumbering.Step := AParameterValue;
end;

class procedure TdxDefaultDestination.ListOverrideTableKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxListOverrideTableDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.ListTableKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxListTableDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.NewSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.ApplySectionFormatting;
  AImporter.InsertSection;
  AImporter.TableReader.ResetState;
end;

class procedure TdxDefaultDestination.OnlyAllowEditingOfFormFieldsHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.OnlyAllowEditingOfFormFields := True;
end;

class procedure TdxDefaultDestination.PageBackgroundHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxPageBackgroundDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.PageBreakKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.ParseCharWithoutDecoding(TdxCharacters.PageBreak);
end;

class procedure TdxDefaultDestination.PageFacingHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentModel.DocumentProperties.DifferentOddAndEvenPages := True;
end;

class procedure TdxDefaultDestination.PageNumberingArabicAbjadHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.ArabicAbjad;
end;

class procedure TdxDefaultDestination.PageNumberingArabicAlphaHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.ArabicAlpha;
end;

class procedure TdxDefaultDestination.PageNumberingChapterHeaderStyleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 0;
  AImporter.Position.SectionFormattingInfo.PageNumbering.ChapterHeaderStyle := AParameterValue;
end;

class procedure TdxDefaultDestination.PageNumberingChapterSeparatorColonHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.ChapterSeparator := ':';
end;

class procedure TdxDefaultDestination.PageNumberingChapterSeparatorEmDashHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.ChapterSeparator := TdxCharacters.EmDash;
end;

class procedure TdxDefaultDestination.PageNumberingChapterSeparatorEnDashHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.ChapterSeparator := TdxCharacters.EnDash;
end;

class procedure TdxDefaultDestination.PageNumberingChapterSeparatorHyphenHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.ChapterSeparator := TdxCharacters.Hyphen;
end;

class procedure TdxDefaultDestination.PageNumberingChapterSeparatorPeriodHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.ChapterSeparator := '.';
end;

class procedure TdxDefaultDestination.PageNumberingChosungHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.Chosung;
end;

class procedure TdxDefaultDestination.PageNumberingDecimalEnclosedCircleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.DecimalEnclosedCircle;
end;

class procedure TdxDefaultDestination.PageNumberingDecimalFullWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.DecimalFullWidth;
end;

class procedure TdxDefaultDestination.PageNumberingDecimalHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.Decimal;
end;

class procedure TdxDefaultDestination.PageNumberingGanadaHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.Ganada;
end;

class procedure TdxDefaultDestination.PageNumberingHindiConsonantsHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.HindiConsonants;
end;

class procedure TdxDefaultDestination.PageNumberingHindiDescriptiveHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.HindiDescriptive;
end;

class procedure TdxDefaultDestination.PageNumberingHindiNumbersHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.HindiNumbers;
end;

class procedure TdxDefaultDestination.PageNumberingHindiVowelsHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.HindiVowels;
end;

class procedure TdxDefaultDestination.PageNumberingLowerLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.LowerLetter;
end;

class procedure TdxDefaultDestination.PageNumberingLowerRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.LowerRoman;
end;

class procedure TdxDefaultDestination.PageNumberingStartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 1;
  AImporter.DocumentProperties.DefaultSectionProperties.PageNumbering.FirstPageNumber := AParameterValue;
  AImporter.DocumentProperties.DefaultSectionProperties.PageNumbering.ContinueNumbering := False;
  AImporter.Position.SectionFormattingInfo.PageNumbering.FirstPageNumber := AParameterValue;
  AImporter.Position.SectionFormattingInfo.PageNumbering.ContinueNumbering := False;
end;

class procedure TdxDefaultDestination.PageNumberingThaiDescriptiveHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.ThaiDescriptive;
end;

class procedure TdxDefaultDestination.PageNumberingThaiLettersHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.ThaiLetters;
end;

class procedure TdxDefaultDestination.PageNumberingThaiNumbersHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.ThaiNumbers;
end;

class procedure TdxDefaultDestination.PageNumberingUpperLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.UpperLetter;
end;

class procedure TdxDefaultDestination.PageNumberingUpperRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.UpperRoman;
end;

class procedure TdxDefaultDestination.PageNumberingVietnameseDescriptiveHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.PageNumbering.NumberingFormat := TdxNumberingFormat.VietnameseDescriptive;
end;

class procedure TdxDefaultDestination.PaperHeightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AValue: Integer;
begin
  if not AHasParameter then
    AParameterValue := 15840;
  AValue := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
  AImporter.DocumentProperties.DefaultSectionProperties.Page.Height := AValue;
  AImporter.Position.SectionFormattingInfo.Page.Height := AValue;
end;

class procedure TdxDefaultDestination.PaperSizeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AValue: TdxPaperKind;
begin
  if AHasParameter then
  begin
    AValue := TdxPaperKind(AParameterValue);
    AImporter.DocumentProperties.DefaultSectionProperties.Page.PaperKind := AValue;
    AImporter.Position.SectionFormattingInfo.Page.PaperKind := AValue;
  end;
end;

class procedure TdxDefaultDestination.PaperWidthKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AValue: Integer;
begin
  if not AHasParameter then
    AParameterValue := 12240;
  AValue := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
  AImporter.DocumentProperties.DefaultSectionProperties.Page.Width := AValue;
  AImporter.Position.SectionFormattingInfo.Page.Width := AValue;
end;

class procedure TdxDefaultDestination.ReadOnlyProtectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AProperties: TdxDocumentProtectionProperties;
begin
  AProperties := AImporter.DocumentModel.ProtectionProperties;
  AProperties.BeginInit;
  try
    AProperties.ProtectionType := TdxDocumentProtectionType.ReadOnly;
  finally
    AProperties.EndInit;
  end;
end;

class procedure TdxDefaultDestination.RightMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 1800;
  AParameterValue := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
  AImporter.DocumentProperties.DefaultSectionProperties.Margins.Right := AParameterValue;
  AImporter.Position.SectionFormattingInfo.Margins.Right := AParameterValue;
end;

class procedure TdxDefaultDestination.RtfKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
end;

class procedure TdxDefaultDestination.SectionBottomMarginHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.Margins.Bottom := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDefaultDestination.SectionBreakColumnHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.StartType := TdxSectionStartType.Column;
end;

class procedure TdxDefaultDestination.SectionBreakEvenPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.StartType := TdxSectionStartType.EvenPage;
end;

class procedure TdxDefaultDestination.SectionBreakNoneHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.StartType := TdxSectionStartType.Continuous;
end;

class procedure TdxDefaultDestination.SectionBreakOddPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.StartType := TdxSectionStartType.OddPage;
end;

class procedure TdxDefaultDestination.SectionBreakPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.StartType := TdxSectionStartType.NextPage;
end;

class procedure TdxDefaultDestination.SectionColumnCountHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 1;
  AImporter.Position.SectionFormattingInfo.Columns.ColumnCount := AParameterValue;
  AImporter.Position.SectionFormattingInfo.Columns.EqualWidthColumns := True;
end;

class procedure TdxDefaultDestination.SectionColumnSpaceHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 720;
  AImporter.Position.SectionFormattingInfo.Columns.Space :=
    AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
  AImporter.Position.SectionFormattingInfo.Columns.EqualWidthColumns := True;
end;

class procedure TdxDefaultDestination.SectionCurrentColumnIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.CurrentColumnIndex := AParameterValue - 1;
  AImporter.Position.SectionFormattingInfo.Columns.EqualWidthColumns := False;
end;

class procedure TdxDefaultDestination.SectionCurrentColumnSpaceHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.SetCurrentColumnSpace(AImporter.UnitConverter.TwipsToModelUnits(AParameterValue));
  AImporter.Position.SectionFormattingInfo.Columns.EqualWidthColumns := False;
end;

class procedure TdxDefaultDestination.SectionCurrentColumnWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.SetCurrentColumnWidth(AImporter.UnitConverter.TwipsToModelUnits(AParameterValue));
  AImporter.Position.SectionFormattingInfo.Columns.EqualWidthColumns := False;
end;

class procedure TdxDefaultDestination.SectionDefaultHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.Assign(AImporter.DocumentProperties.DefaultSectionProperties);
end;

class procedure TdxDefaultDestination.SectionDifferentFirstPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.DifferentFirstPage := True;
end;

class procedure TdxDefaultDestination.SectionDrawVerticalSeparatorHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.Columns.DrawVerticalSeparator := True;
end;

class procedure TdxDefaultDestination.SectionEndNoteNumberingChicagoHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.Chicago;
end;

class procedure TdxDefaultDestination.SectionEndNoteNumberingChosungHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.Chosung;
end;

class procedure TdxDefaultDestination.SectionEndNoteNumberingDecimalEnclosedCircleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.DecimalEnclosedCircle;
end;

class procedure TdxDefaultDestination.SectionEndNoteNumberingDecimalFullWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.DecimalFullWidth;
end;

class procedure TdxDefaultDestination.SectionEndNoteNumberingDecimalHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.Decimal;
end;

class procedure TdxDefaultDestination.SectionEndNoteNumberingGanadaHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.Ganada;
end;

class procedure TdxDefaultDestination.SectionEndNoteNumberingLowerCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.LowerLetter;
end;

class procedure TdxDefaultDestination.SectionEndNoteNumberingLowerCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.LowerRoman;
end;

class procedure TdxDefaultDestination.SectionEndNoteNumberingRestartContinuousHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingRestartType := TdxLineNumberingRestart.Continuous;
end;

class procedure TdxDefaultDestination.SectionEndNoteNumberingRestartEachSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingRestartType := TdxLineNumberingRestart.NewSection;
end;

class procedure TdxDefaultDestination.SectionEndNoteNumberingStartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter or (AParameterValue <= 0) then
    AParameterValue := 1;
  AImporter.Position.SectionFormattingInfo.EndNote.StartingNumber := AParameterValue;
end;

class procedure TdxDefaultDestination.SectionEndNoteNumberingUpperCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.UpperLetter;
end;

class procedure TdxDefaultDestination.SectionEndNoteNumberingUpperCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.EndNote.NumberingFormat := TdxNumberingFormat.UpperRoman;
end;

class procedure TdxDefaultDestination.SectionFirstPagePaperSourceHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.FirstPagePaperSource := AParameterValue;
end;

class procedure TdxDefaultDestination.SectionFooterOffsetHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 720;
  AImporter.Position.SectionFormattingInfo.Margins.FooterOffset := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingChicagoHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.Chicago;
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingChosungHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.Chosung;
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingDecimalEnclosedCircleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.DecimalEnclosedCircle;
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingDecimalFullWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.DecimalFullWidth;
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingDecimalHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.Decimal;
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingGanadaHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.Ganada;
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingLowerCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.LowerLetter;
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingLowerCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.LowerRoman;
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingRestartContinuousHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingRestartType := TdxLineNumberingRestart.Continuous;
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingRestartEachPageHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingRestartType := TdxLineNumberingRestart.NewPage;
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingRestartEachSectionHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingRestartType := TdxLineNumberingRestart.NewSection;
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingStartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter or (AParameterValue <= 0) then
    AParameterValue := 1;
  AImporter.Position.SectionFormattingInfo.FootNote.StartingNumber := AParameterValue;
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingUpperCaseLetterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.UpperLetter;
end;

class procedure TdxDefaultDestination.SectionFootNoteNumberingUpperCaseRomanHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.NumberingFormat := TdxNumberingFormat.UpperRoman;
end;

class procedure TdxDefaultDestination.SectionFootNotePlacementBelowTextHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.Position := TdxFootNotePosition.BelowText;
end;

class procedure TdxDefaultDestination.SectionFootNotePlacementPageBottomHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.FootNote.Position := TdxFootNotePosition.BottomOfPage;
end;

class procedure TdxDefaultDestination.SectionGutterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.Margins.Gutter := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDefaultDestination.SectionHeaderOffsetHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 720;
  AImporter.Position.SectionFormattingInfo.Margins.HeaderOffset := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDefaultDestination.SectionLandscapeHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.Page.Landscape := True;
end;

class procedure TdxDefaultDestination.SectionLeftMarginHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.Margins.Left := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDefaultDestination.SectionOtherPagePaperSourceHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.OtherPagePaperSource := AParameterValue;
end;

class procedure TdxDefaultDestination.SectionPageNumberingContinuousHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.RestartPageNumbering := False;
end;

class procedure TdxDefaultDestination.SectionPageNumberingRestartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.RestartPageNumbering := True;
end;

class procedure TdxDefaultDestination.SectionPageNumberingStartHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 1;
  AImporter.Position.SectionFormattingInfo.PageNumbering.FirstPageNumber := AParameterValue;
  AImporter.Position.SectionFormattingInfo.PageNumbering.ContinueNumbering := False;
end;

class procedure TdxDefaultDestination.SectionPaperHeightHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.Page.Height := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDefaultDestination.SectionPaperWidthHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.Page.Width := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDefaultDestination.SectionRightMarginHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.Margins.Right := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDefaultDestination.SectionTextFlowHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AFlow: TdxTextDirection;
begin
  if not AHasParameter then
    AParameterValue := 0;
  AFlow := TdxTextDirection(AParameterValue);
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.TextDirection := AFlow;
end;

class procedure TdxDefaultDestination.SectionTopMarginHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.Margins.Top := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDefaultDestination.StyleSheetKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxStyleSheetDestination.Create(AImporter);
end;

class procedure TdxDefaultDestination.TopMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 1440;
  AParameterValue := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
  AImporter.DocumentProperties.DefaultSectionProperties.Margins.Top := AParameterValue;
  AImporter.Position.SectionFormattingInfo.Margins.Top := AParameterValue;
end;

class procedure TdxDefaultDestination.VerticalTextAlignmentBottomHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.VerticalTextAlignment := TdxVerticalAlignment.Bottom;
end;

class procedure TdxDefaultDestination.VerticalTextAlignmentCenterHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.VerticalTextAlignment := TdxVerticalAlignment.Center;
end;

class procedure TdxDefaultDestination.VerticalTextAlignmentJustifyHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.VerticalTextAlignment := TdxVerticalAlignment.Both;
end;

class procedure TdxDefaultDestination.VerticalTextAlignmentTopHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.SectionFormattingInfo.GeneralSectionInfo.VerticalTextAlignment := TdxVerticalAlignment.Top;
end;

end.
