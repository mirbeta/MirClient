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

unit dxRichEdit.Dialogs.Utils;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, SysUtils, Windows, Generics.Defaults, Generics.Collections,
  dxRichEdit.Dialogs.FindAndReplaceFormHelpers,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.TableFormatting;

const
  MinOutlineLevel = 0;
  MaxOutlineLevel = 9;

  MinFollowNumber = 0;
  MaxFollowNumber = 2;

const
  dxParagraphAlignmentNames: array[TdxParagraphAlignment] of Pointer =
    (@sdxParagraphAlignmentLeft, @sdxParagraphAlignmentRight, @sdxParagraphAlignmentCenter, @sdxParagraphAlignmentJustify);

  dxParagraphLineSpacingNames: array[TdxParagraphLineSpacing] of Pointer =
    (@sdxParagraphLineSpacingSingle, @sdxParagraphLineSpacingSesquialteral, @sdxParagraphLineSpacingDouble,
     @sdxParagraphLineSpacingMultiple, @sdxParagraphLineSpacingExactly, @sdxParagraphLineSpacingAtLeast);

  dxParagraphFirstLineIndentNames: array[TdxParagraphFirstLineIndent] of Pointer =
    (@sdxParagraphFirstLineIndentNone, @sdxParagraphFirstLineIndentIndented, @sdxParagraphFirstLineIndentHanging);

  dxParagraphOutlineLevelNames: array[MinOutlineLevel..MaxOutlineLevel] of Pointer =
    (@sdxParagraphOutlineLeve0, @sdxParagraphOutlineLeve1, @sdxParagraphOutlineLeve2, @sdxParagraphOutlineLeve3,
     @sdxParagraphOutlineLeve4, @sdxParagraphOutlineLeve5, @sdxParagraphOutlineLeve6, @sdxParagraphOutlineLeve7,
     @sdxParagraphOutlineLeve8, @sdxParagraphOutlineLeve9);

  dxListNumberAlignmentNames: array [TdxListNumberAlignment] of Pointer =
    (@sdxParagraphAlignmentLeft, @sdxParagraphAlignmentCenter, @sdxParagraphAlignmentRight);

  dxListFollowNumberNames: array[MinFollowNumber..MaxFollowNumber] of Pointer =
    (@sdxRichEditMultiLevelNumberingListFollowNumberTabCharacter, @sdxRichEditMultiLevelNumberingListFollowNumberSpace,
     @sdxRichEditMultiLevelNumberingListFollowNumberNothing);

  dxSearchTextDirectionNames: array[TdxTextSearchDirection] of Pointer =
    (@sdxRichEditSearchTextDialogDirectionAll, @sdxRichEditSearchTextDialogDirectionDown,
     @sdxRichEditSearchTextDialogDirectionUp);

  dxShadingApplyToNames: array[0..1] of Pointer =
    (@sdxRichEditBorderShadingDialogApplyToCell, @sdxRichEditBorderShadingDialogApplyToTable);

  dxSectionPropertiesApplyToNames: array[TdxSectionPropertiesApplyType] of Pointer =
    (@sdxSectionPropertiesApplyToWholeDocument, @sdxSectionPropertiesApplyToCurrentSection,
     @sdxSectionPropertiesApplyToSelectedSections,
     @sdxSectionPropertiesApplyThisPointForward);

  dxPaperKindNames: array[TdxPaperKind] of pointer =
   (@sdxRichEditPaperKindCustom,
    @sdxRichEditPaperKindLetter,
    @sdxRichEditPaperKindLetterSmall,
    @sdxRichEditPaperKindTabloid,
    @sdxRichEditPaperKindLedger,
    @sdxRichEditPaperKindLegal,
    @sdxRichEditPaperKindStatement,
    @sdxRichEditPaperKindExecutive,
    @sdxRichEditPaperKindA3,
    @sdxRichEditPaperKindA4,
    @sdxRichEditPaperKindA4Small,
    @sdxRichEditPaperKindA5,
    @sdxRichEditPaperKindB4,
    @sdxRichEditPaperKindB5,
    @sdxRichEditPaperKindFolio,
    @sdxRichEditPaperKindQuarto,
    @sdxRichEditPaperKindStandard10x14,
    @sdxRichEditPaperKindStandard11x17,
    @sdxRichEditPaperKindNote,
    @sdxRichEditPaperKindNumber9Envelope,
    @sdxRichEditPaperKindNumber10Envelope,
    @sdxRichEditPaperKindNumber11Envelope,
    @sdxRichEditPaperKindNumber12Envelope,
    @sdxRichEditPaperKindNumber14Envelope,
    @sdxRichEditPaperKindCSheet,
    @sdxRichEditPaperKindDSheet,
    @sdxRichEditPaperKindESheet,
    @sdxRichEditPaperKindDLEnvelope,
    @sdxRichEditPaperKindC5Envelope,
    @sdxRichEditPaperKindC3Envelope,
    @sdxRichEditPaperKindC4Envelope,
    @sdxRichEditPaperKindC6Envelope,
    @sdxRichEditPaperKindC65Envelope,
    @sdxRichEditPaperKindB4Envelope,
    @sdxRichEditPaperKindB5Envelope,
    @sdxRichEditPaperKindB6Envelope,
    @sdxRichEditPaperKindItalyEnvelope,
    @sdxRichEditPaperKindMonarchEnvelope,
    @sdxRichEditPaperKindPersonalEnvelope,
    @sdxRichEditPaperKindUSStandardFanfold,
    @sdxRichEditPaperKindGermanStandardFanfold,
    @sdxRichEditPaperKindGermanLegalFanfold,
    @sdxRichEditPaperKindIsoB4,
    @sdxRichEditPaperKindJapanesePostcard,
    @sdxRichEditPaperKindStandard9x11,
    @sdxRichEditPaperKindStandard10x11,
    @sdxRichEditPaperKindStandard15x11,
    @sdxRichEditPaperKindInviteEnvelope,
    nil,
    nil,
    @sdxRichEditPaperKindLetterExtra,
    @sdxRichEditPaperKindLegalExtra,
    @sdxRichEditPaperKindTabloidExtra,
    @sdxRichEditPaperKindA4Extra,
    @sdxRichEditPaperKindLetterTransverse,
    @sdxRichEditPaperKindA4Transverse,
    @sdxRichEditPaperKindLetterExtraTransverse,
    @sdxRichEditPaperKindAPlus,
    @sdxRichEditPaperKindBPlus,
    @sdxRichEditPaperKindLetterPlus,
    @sdxRichEditPaperKindA4Plus,
    @sdxRichEditPaperKindA5Transverse,
    @sdxRichEditPaperKindB5Transverse,
    @sdxRichEditPaperKindA3Extra,
    @sdxRichEditPaperKindA5Extra,
    @sdxRichEditPaperKindB5Extra,
    @sdxRichEditPaperKindA2,
    @sdxRichEditPaperKindA3Transverse,
    @sdxRichEditPaperKindA3ExtraTransverse,
    @sdxRichEditPaperKindJapaneseDoublePostcard,
    @sdxRichEditPaperKindA6,
    @sdxRichEditPaperKindJapaneseEnvelopeKakuNumber2,
    @sdxRichEditPaperKindJapaneseEnvelopeKakuNumber3,
    @sdxRichEditPaperKindJapaneseEnvelopeChouNumber3,
    @sdxRichEditPaperKindJapaneseEnvelopeChouNumber4,
    @sdxRichEditPaperKindLetterRotated,
    @sdxRichEditPaperKindA3Rotated,
    @sdxRichEditPaperKindA4Rotated,
    @sdxRichEditPaperKindA5Rotated,
    @sdxRichEditPaperKindB4JisRotated,
    @sdxRichEditPaperKindB5JisRotated,
    @sdxRichEditPaperKindJapanesePostcardRotated,
    @sdxRichEditPaperKindJapaneseDoublePostcardRotated,
    @sdxRichEditPaperKindA6Rotated,
    @sdxRichEditPaperKindJapaneseEnvelopeKakuNumber2Rotated,
    @sdxRichEditPaperKindJapaneseEnvelopeKakuNumber3Rotated,
    @sdxRichEditPaperKindJapaneseEnvelopeChouNumber3Rotated,
    @sdxRichEditPaperKindJapaneseEnvelopeChouNumber4Rotated,
    @sdxRichEditPaperKindB6Jis,
    @sdxRichEditPaperKindB6JisRotated,
    @sdxRichEditPaperKindStandard12x11,
    @sdxRichEditPaperKindJapaneseEnvelopeYouNumber4,
    @sdxRichEditPaperKindJapaneseEnvelopeYouNumber4Rotated,
    @sdxRichEditPaperKindPrc16K,
    @sdxRichEditPaperKindPrc32K,
    @sdxRichEditPaperKindPrc32KBig,
    @sdxRichEditPaperKindPrcEnvelopeNumber1,
    @sdxRichEditPaperKindPrcEnvelopeNumber2,
    @sdxRichEditPaperKindPrcEnvelopeNumber3,
    @sdxRichEditPaperKindPrcEnvelopeNumber4,
    @sdxRichEditPaperKindPrcEnvelopeNumber5,
    @sdxRichEditPaperKindPrcEnvelopeNumber6,
    @sdxRichEditPaperKindPrcEnvelopeNumber7,
    @sdxRichEditPaperKindPrcEnvelopeNumber8,
    @sdxRichEditPaperKindPrcEnvelopeNumber9,
    @sdxRichEditPaperKindPrcEnvelopeNumber10,
    @sdxRichEditPaperKindPrc16KRotated,
    @sdxRichEditPaperKindPrc32KRotated,
    @sdxRichEditPaperKindPrc32KBigRotated,
    @sdxRichEditPaperKindPrcEnvelopeNumber1Rotated,
    @sdxRichEditPaperKindPrcEnvelopeNumber2Rotated,
    @sdxRichEditPaperKindPrcEnvelopeNumber3Rotated,
    @sdxRichEditPaperKindPrcEnvelopeNumber4Rotated,
    @sdxRichEditPaperKindPrcEnvelopeNumber5Rotated,
    @sdxRichEditPaperKindPrcEnvelopeNumber6Rotated,
    @sdxRichEditPaperKindPrcEnvelopeNumber7Rotated,
    @sdxRichEditPaperKindPrcEnvelopeNumber8Rotated,
    @sdxRichEditPaperKindPrcEnvelopeNumber9Rotated,
    @sdxRichEditPaperKindPrcEnvelopeNumber10Rotated);

  dxConditionalTableStyleFormattingTypeNames: array[TdxConditionalTableStyleFormattingType] of pointer = (
    @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_BottomLeftCell,
    @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_BottomRightCell,
    @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_TopLeftCell,
    @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_TopRightCell,
    @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_EvenRowBanding,
    @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_OddRowBanding,
    @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_EvenColumnBanding,
    @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_OddColumnBanding,
    @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_LastColumn,
    @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_FirstColumn,
    @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_LastRow,
    @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_FirstRow,
    @sdxRichEditTableStyleDialogConditionalTableStyleFormattingType_WholeTable);

implementation

end.
