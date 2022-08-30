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

unit dxRichEdit.Import.Rtf.DestinationPieceTable;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore,
  dxCoreClasses,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.Import,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Import.Rtf.ParagraphFormatting;

type

  { TdxCustomMergedParagraphPropertiesCachedResult }

  TdxCustomMergedParagraphPropertiesCachedResult = class(TdxParagraphMergedParagraphPropertiesCachedResult)
  private
    FInitialParagraphPropertiesIndex: Integer;
    FFinalParagraphPropertiesIndex: Integer;
  public
    constructor Create;

    property InitialParagraphPropertiesIndex: Integer read FInitialParagraphPropertiesIndex write FInitialParagraphPropertiesIndex;
    property FinalParagraphPropertiesIndex: Integer read FFinalParagraphPropertiesIndex write FFinalParagraphPropertiesIndex;
  end;

  { TdxCustomMergedCharacterPropertiesCachedResult }

  TdxCustomMergedCharacterPropertiesCachedResult = class(TdxRunMergedCharacterPropertiesCachedResult)
  strict private
    FInitialRunCharacterPropertiesIndex: Integer;
    FFinalRunCharacterPropertiesIndex: Integer;
    FTableStyleIndex: Integer;
  public
    constructor Create; override;
    property InitialRunCharacterPropertiesIndex: Integer read FInitialRunCharacterPropertiesIndex
      write FInitialRunCharacterPropertiesIndex;
    property FinalRunCharacterPropertiesIndex: Integer read FFinalRunCharacterPropertiesIndex
      write FFinalRunCharacterPropertiesIndex;
    property TableStyleIndex: Integer read FTableStyleIndex write FTableStyleIndex;
  end;

  { TdxDestinationPieceTable }

  TdxDestinationPieceTable = class(TdxRichEditRtfDestinationBase)
  public type
    TCharType = (LowAnsiCharacter, DoubleByteCharacter, HighAnsiCharacter);
  public const
    HighestCyrillic = 1279;
    LowLatinExtendedAdditional = 7680;
    HighLatinExtendedAdditional = 7929;
    LowGeneralPunctuation = 8192;
    HighGeneralPunctuation = 8303;
    LowCurrencySymbols = 8352;
    HighCurrencySymbols = 8367;
    LowLetterlikeSymbols = 8448;
    HighLetterlikeSymbols = 8506;
    LowNumberForms = 8531;
    HighNumberForms = 8579;
    LowMathematicalOperations = 8704;
    HighMathematicalOperations = 8945;
    LowAnsiCharactersStart = $00;
    LowAnsiCharactersEnd = $7F;
    HighAnsiCharactersStart = $80;
    HighAnsiCharactersEnd = $FF;
  strict private
    class var
      FKeywordHT: TdxKeywordTranslatorTable;
      FControlCharHT: TdxControlCharTranslatorTable;
      FShadingPatterns: TDictionary<Integer, TdxShadingPattern>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateControlCharHT: TdxControlCharTranslatorTable; static;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
    class function CreateShadingPatterns: TDictionary<Integer, TdxShadingPattern>; static;
  strict private
    FPieceTable: TdxPieceTable;
  private
    procedure AppendChar(AChar: Char);
    function CalculateCharType(AChar: Char): TCharType;
    function GetDocumentModel: TdxDocumentModel; inline;
    function GetFormattingSourceRun(AField: TdxField): TdxTextRunBase;
    function GetNormalizedNumberingListIndex(AOwnNumberingListIndex, AParentNumberingListIndex: TdxNumberingListIndex): TdxNumberingListIndex;
    function GetTableStyleToMerge(ATableStyleIndex: Integer): TdxTableStyle;
    class procedure InsertSpace(AImporter: TdxRtfImporter); static;
    function IsDoubleByteChar(AChar: Char): Boolean;
    function IsLowAnsiCharacter(AChar: Char): Boolean;

    procedure InsertBookmarks;
    procedure InsertRangePermissions;

    procedure NormalizeProperties;
    procedure NormalizeParagraphsProperties;
    procedure NormalizeTabs(AParagraph: TdxParagraph);
    procedure NormalizeParagraphStyleNumbering; overload;
    procedure NormalizeParagraphStyleNumbering(AParagraphStyle: TdxParagraphStyle); overload;

    procedure NormalizeParagraphProperties(AParagraph: TdxParagraph;
      ACachedResult: TdxCustomMergedParagraphPropertiesCachedResult; ATableStyleIndex: Integer);
    procedure NormalizeRunProperties(ARun: TdxTextRunBase;
      ACachedResult: TdxCustomMergedCharacterPropertiesCachedResult; ATableStyleIndex: Integer);
    function NormalizeRunsProperties(AParagraph: TdxParagraph; ARunIndex, ATableStyleIndex: Integer;
      ACachedResults: TObjectDictionary<TClass, TdxCustomMergedCharacterPropertiesCachedResult>): TdxRunIndex;
    procedure NormalizeTableProperties(const ATable: TdxTable);
    procedure NormalizeTablesProperties;
    procedure NormalizeTableRowProperties(const ARow: TdxTableRow);
    procedure NormalizeTableRowsProperties(ATable: TdxTable);
    procedure NormalizeTableCellProperties(const ACell: TdxTableCell);
    procedure NormalizeTableCellsProperties(ARow: TdxTableRow);

    class procedure AssignDefaultBorderWidth(ABorder: TdxBorderBase); static;
    class procedure AssignWidthUnitInfo(AUnitInfo: TdxWidthUnit; Value: Integer); static;
    class function EnsureFramePropertiesExists(AImporter: TdxRtfImporter): Boolean;
    class function ShouldApplyParagraphStyle(AImporter: TdxRtfImporter): Boolean; static;
    class function GetWidthUnitType(AParameterValue: Integer): TdxWidthUnitType; static;
    class procedure SetBorderType(AImporter: TdxRtfImporter; AStyle: TdxBorderLineStyle); static;

    class procedure NonBreakingSpaceCharHandler(AImporter: TdxRtfImporter; var AChar: Char); static;
    class procedure NonBreakingHyphenCharHandler(AImporter: TdxRtfImporter; var AChar: Char); static;
    class procedure OptionalHyphenCharHandler(AImporter: TdxRtfImporter; var AChar: Char); static;

    class procedure AlignLeftKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure AlignCenterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure AlignRightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure AlignJustifyKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure AssociatedFontNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BackColorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BarTabKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BoldKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BookmarkEndKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BookmarkStartKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BulletKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CapsKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CharacterStyleIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DocumentVariableKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DoubleByteCharactersKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LowAnsiFontNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure HighAnsiFontNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DoubleStrikeoutKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure EmDashKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure EmSpaceKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure EnDashKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure EnSpaceKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FieldStartKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FirstLineIndentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FontNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FontSizeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ForeColorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure HiddenTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ItalicKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LeftDoubleQuoteKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LeftIndentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LeftSingleQuoteKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LineBreakKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ListLevelHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ListOverrideHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LineSpacingTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LineSpacingMultiplierKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure HyphenateParagraphKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure SuppressLineNumbersKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ContextualSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure PageBreakBeforeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BeforeAutoSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure AfterAutoSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure KeepWithNextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure KeepLinesTogetherKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ListTextHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure MailMergeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure NonShapePictureKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure NoSuperAndSubScriptKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LanguageKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LanguageNpKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure NoProofKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure OldParagraphNumberingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure OldParagraphNumberingTextAfterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure OldParagraphNumberingTextBeforeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ParagraphStyleIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableStyleIndexForRowOrCellHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ParKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure PictureKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure PlainKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure QmSpaceKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure RangePermissionEndKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure RangePermissionStartKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure RightDoubleQuoteKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ParCharHandler(AImporter: TdxRtfImporter; var AChar: Char); static;
    class procedure RightIndentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure RightSingleQuoteKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure SectionLevelNumberingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ShapeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ShapePictureKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure StrikeoutKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure SpacingAfterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure SpacingBeforeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure SubscriptKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure SuperscriptKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TabCenterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TabDecimalKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TabKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TabLeaderDotsKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TabLeaderEqualSignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TabLeaderHyphensKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TabLeaderMiddleDotsKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TabLeaderThickLineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TabLeaderUnderlineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TabPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TabRightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineColorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineDashDotDottedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineDashDottedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineDashedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineDottedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineDoubleKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineDoubleWaveKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineHeavyWaveKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineKeywordHandleCore(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean; AType: TdxUnderlineType); static;
    class procedure UnderlineLongDashedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineNoneKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineSingleKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineThickDashDotDottedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineThickDashDottedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineThickDashedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineThickDottedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineThickLongDashedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineThickSingleKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineWaveKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnderlineWordsOnlyKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UnicodeCountKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure WidowOrphanControlOffKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure OutlineLevelKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ParagraphBackgroundKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure WidowOrphanControlOnKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;

    class procedure InTableParagraphKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure RowKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure NestedCellKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure NestedRowKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure NestedTablePropertiesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure NoNestedTablesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ItapKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;

    class procedure TableRowDefaultsKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableStyleKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellxKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellPreferredWidthKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure WidthUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FirstHorizontalMergedCellKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure NextHorizontalMergedCellKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FirstVerticalMergedCellKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure NextVerticalMergedCellKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellFitTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellNoWrapKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellVerticalAlignmentTopKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellVerticalAlignmentCenterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellVerticalAlignmentBottomKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellHideMarkKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellBottomCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellLeftCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellRightCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellTopCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellBottomCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellLeftCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellRightCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellTopCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellTextTopAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellTextCenterVerticalAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellTextBottomAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellLeftToRightTopToBottomTextDirectionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellTopToBottomRightToLeftTextDirectionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellBottomToTopLeftToRightTextDirectionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellLeftToRightTopToBottomVerticalTextDirectionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellTopToBottomRightToLeftVerticalTextDirectionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellBackgroundColorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellForegroundColorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CellShadingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;

    class procedure NoTableBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BottomCellBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TopCellBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LeftCellBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure RightCellBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UpperLeftToLowerRightBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UpperRightToLowerLeftBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;

    class procedure RowLeftKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure RowHeaderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure RowHeightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure RowKeepKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableRightAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableLeftAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableCenterAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure WidthBeforeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure WidthBeforeUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure WidthAfterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure WidthAfterUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;

    class procedure SpaceBetweenCellsKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableBottomCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableLeftCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableRightCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableTopCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableBottomCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableLeftCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableRightCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableTopCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;

    class procedure TableBottomCellSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableLeftCellSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableRightCellSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableTopCellSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableBottomCellSpacingUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableLeftCellSpacingUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableRightCellSpacingUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableTopCellSpacingUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;

    class procedure TablePreferredWidthKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TablePreferredWidthUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableIndentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableIndentUnitTypeHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableOverlapKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;

    class procedure TableLeftFromTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableRightFromTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableTopFromTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableBottomFromTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ColHorizontalAnchorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure MarginHorizontalAnchorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure PageHorizontalAnchorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure MarginVerticalAnchorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ParagraphVerticalAnchorKewordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure PageVerticalAnchorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableHorizontalPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableVerticalPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CenterTableHorizontalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure InsideTableHorizontalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LeftTableHorizontalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure OutsideTableHorizontalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure RightTableHorizontalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BottomTableVerticalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure CenterTableVerticalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure InlineTableVerticalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure InsideTableVerticalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure OutsideTableVerticalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TopTableVerticalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TableAutoFitKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;

    class procedure RowBandSizeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ColumnBandSizeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;

    class procedure TopTableBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LeftTableBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BottomTableBorderKewordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure RightTableBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure HorizontalTableBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure VerticalTableBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;

    class procedure ApplyFirstRowConditionalFormattingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ApplyLastRowConditionalFormattingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ApplyFirstColumnContitionalFormattingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ApplyLastColumnConditionalFormattingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DoNotApplyRowBandingConditionalFormattingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DoNotApplyColumnBandingConditionalFormattingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;

    class procedure SetNoBorderType(AImporter: TdxRtfImporter; AStyle: TdxBorderLineStyle); static;
    class procedure NoBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BorderWidthKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BorderColorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FrameBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BorderSpaceKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure SingleThicknessBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DoubleThicknessBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ShadowedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DoubleBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DottedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DashedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure HairlineBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure SmallDashedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DotDashedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DotDotDashedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure InsetBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure NoneBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure OutsetBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TripletBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure SmallThickThinBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure SmallThinThickBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure SmallThinThickThinBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure MediumThickThinBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure MediumThinThickBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure MediumThinThickThinBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LargeThickThinBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LargeThinThickBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LargeThinThickThinBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure WavyBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DoubleWavyBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure StripedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure EmbossedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure EngravedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BorderArtIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TopParagraphBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BottomParagraphBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LeftParagraphBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure RightParagraphBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FrameHorizontalPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FrameVerticalPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FrameWidthKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FrameHeightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ParagraphHorizontalPositionTypeMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ParagraphHorizontalPositionTypePageKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ParagraphHorizontalPositionTypeColumnKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ParagraphVerticalPositionTypeMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ParagraphVerticalPositionTypePageKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ParagraphVerticalPositionTypeLineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FrameNoWrapKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FrameWrapDefaultKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FrameWrapOverlayKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FrameWrapAroundKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FrameWrapTightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FrameWrapThroughKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
  protected
    function CanAppendText: Boolean; override;
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetControlCharHT: TdxControlCharTranslatorTable; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    function GetPieceTable: TdxPieceTable; override;
    procedure FixFieldCodeRunsProperties;
    procedure FixLastParagraph; virtual;
    procedure InsertTextCore(const AText: string);
    procedure ProcessCharCore(AChar: Char); override;
    procedure ProcessTextCore(const AText: string); override;
    procedure SetFieldRunFormatting(AFieldCodeStart, ASourceRun: TdxTextRunBase);
    procedure StartNewField; virtual;

    class procedure AddCommonCharacterKeywords(ATable: TdxKeywordTranslatorTable); static;
    class procedure AddCommonParagraphKeywords(ATable: TdxKeywordTranslatorTable); static;
    class procedure AddCommonSymbolsAndObjectsKeywords(ATable: TdxKeywordTranslatorTable); static;
    class procedure AddCommonTabKeywords(ATable: TdxKeywordTranslatorTable); static;
    class procedure AddCommonNumberingListsKeywords(ATable: TdxKeywordTranslatorTable); static;
    class procedure AppendTableKeywords(ATable: TdxKeywordTranslatorTable); static;
    class procedure AppendTablePropertiesKeywords(ATable: TdxKeywordTranslatorTable); static;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  public
    constructor Create(AImporter: TdxRtfImporter; ATargetPieceTable: TdxPieceTable); reintroduce; virtual;
    destructor Destroy; override;

    class procedure AddCharacterPropertiesKeywords(ATable: TdxKeywordTranslatorTable); static;
    class procedure AddParagraphPropertiesKeywords(ATable: TdxKeywordTranslatorTable); static;

    //for internal use
    class function GetShadingPattern(const Value: Integer): TdxShadingPattern; static;
    class function GetShadingValue(const APattern: TdxShadingPattern): Integer; static;
    class procedure ResetParagraphPropertiesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure InsertImage(AImporter: TdxRtfImporter; AImageInfo: TdxRtfImageInfo); static;

    procedure NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase); override;

    procedure FinalizePieceTableCreation; virtual;
  end;

  { TdxStringValueDestination }

  TdxStringValueDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class var FControlCharHT: TdxControlCharTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateControlCharHT: TdxControlCharTranslatorTable; static;
  private
    FValue: string;
  protected
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
    class function GetControlCharHT: TdxControlCharTranslatorTable; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;

    function CreateClone: TdxRichEditRtfDestinationBase; override;
    function CreateEmptyClone: TdxStringValueDestination; virtual; abstract;
    function GetValue: string; virtual;
    procedure ProcessCharCore(AChar: Char); override;
  public
    property Value: string read GetValue;
  end;

  { TdxBookmarkDestinationBase }

  TdxBookmarkDestinationBase = class abstract(TdxStringValueDestination)
  protected
    procedure AssignBookmarkPosition(ABookmark: TdxImportBookmarkInfo); virtual; abstract;
  public
    procedure AfterPopRtfState; override;
  end;

  { TdxBookmarkStartDestination }

  TdxBookmarkStartDestination = class(TdxBookmarkDestinationBase)
  protected
    procedure AssignBookmarkPosition(ABookmark: TdxImportBookmarkInfo); override;
    function CreateEmptyClone: TdxStringValueDestination; override;
  end;

  { TdxBookmarkEndDestination }

  TdxBookmarkEndDestination = class(TdxBookmarkDestinationBase)
  protected
    procedure AssignBookmarkPosition(ABookmark: TdxImportBookmarkInfo); override;
    function CreateEmptyClone: TdxStringValueDestination; override;
  end;

  { TdxRangePermissionDestinationBase }

  TdxRangePermissionDestinationBase = class abstract(TdxStringValueDestination)
  protected
    function ObtainUserName(const AData: string): string; virtual;
    function ObtainGroupName(const AData: string): string; virtual;
    function ObtainUserId(const AData: string): Integer;
    procedure AssignRangePermissionPosition(ARangePermission: TdxImportRangePermissionInfo); virtual; abstract;
  public
    procedure AfterPopRtfState; override;
  end;

  { TdxRangePermissionStartDestination }

  TdxRangePermissionStartDestination = class(TdxRangePermissionDestinationBase)
  protected
    function CreateEmptyClone: TdxStringValueDestination; override;
    procedure AssignRangePermissionPosition(ARangePermission: TdxImportRangePermissionInfo); override;
  end;

  { TdxRangePermissionEndDestination }

  TdxRangePermissionEndDestination = class(TdxRangePermissionDestinationBase)
  protected
    function CreateEmptyClone: TdxStringValueDestination; override;
    procedure AssignRangePermissionPosition(ARangePermission: TdxImportRangePermissionInfo); override;
  end;

  { TdxUserTableDestination }

  TdxUserTableDestination = class(TdxStringValueDestination)
  protected
    function CreateEmptyClone: TdxStringValueDestination; override;
  public
    procedure NestedGroupFinished(ANestedDestination: TdxRichEditRtfDestinationBase); override;
  end;

implementation

uses
  Contnrs, Windows, dxCoreGraphics, dxTypeHelpers,
  dxRichEdit.Utils.BatchUpdateHelper, RTLConsts,
  Math, Graphics, dxCultureInfo,
  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxCharacters,
  dxGenerics,
  dxRichEdit.Utils.NumberParser,
  dxRichEdit.Import.Rtf.DestinationPicture,
  dxRichEdit.Import.Rtf.DestinationSkip,
  dxRichEdit.Import.Rtf.DestinationOldParagraphNumbering,
  dxRichEdit.Import.Rtf.DestinationColorTable,
  dxRichEdit.Import.Rtf.DestinationShape,
  dxRichEdit.Import.Rtf.TableReader,
  dxRichEdit.Import.Rtf.DestinationField,
  dxRichEdit.Import.Rtf.DestinationDocumentVariable,
  dxRichEdit.Export.Rtf,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.Import.Rtf.DestinationMailMerge;

{ TdxCustomMergedParagraphPropertiesCachedResult }

constructor TdxCustomMergedParagraphPropertiesCachedResult.Create;
begin
  inherited Create;
  FInitialParagraphPropertiesIndex := -1;
  FFinalParagraphPropertiesIndex := -1;
end;

{ TdxCustomMergedCharacterPropertiesCachedResult }

constructor TdxCustomMergedCharacterPropertiesCachedResult.Create;
begin
  inherited Create;
  FInitialRunCharacterPropertiesIndex := -1;
  FFinalRunCharacterPropertiesIndex := -1;
  FTableStyleIndex := -1;
end;

{ TdxDestinationPieceTable }

constructor TdxDestinationPieceTable.Create(
  AImporter: TdxRtfImporter; ATargetPieceTable: TdxPieceTable);
begin
  inherited Create(AImporter);
  FPieceTable := ATargetPieceTable;
end;

function TdxDestinationPieceTable.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxSkipNestedTableDestination.Create(Importer);
end;

class function TdxDestinationPieceTable.GetControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := FControlCharHT;
end;

class function TdxDestinationPieceTable.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class constructor TdxDestinationPieceTable.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
  FControlCharHT := CreateControlCharHT;
  FShadingPatterns := CreateShadingPatterns;
end;

class destructor TdxDestinationPieceTable.Finalize;
begin
  FreeAndNil(FShadingPatterns);
  FreeAndNil(FControlCharHT);
  FreeAndNil(FKeywordHT);
end;

class function TdxDestinationPieceTable.CreateControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := TdxControlCharTranslatorTable.Create;
  Result.Add(#10, ParCharHandler);
  Result.Add(#13, ParCharHandler);
  Result.Add('\', EscapedCharHandler);
  Result.Add('{', EscapedCharHandler);
  Result.Add('}', EscapedCharHandler);
  Result.Add('~', NonBreakingSpaceCharHandler);
  Result.Add('_', NonBreakingHyphenCharHandler);
  Result.Add('-', OptionalHyphenCharHandler);
end;

class function TdxDestinationPieceTable.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  AddCommonCharacterKeywords(Result);
  AddCommonParagraphKeywords(Result);
  AddCommonSymbolsAndObjectsKeywords(Result);
  AddCommonTabKeywords(Result);
  AddCommonNumberingListsKeywords(Result);
  AppendTableKeywords(Result);
end;

class function TdxDestinationPieceTable.CreateShadingPatterns: TDictionary<Integer, TdxShadingPattern>;
begin
  Result := TDictionary<Integer, TdxShadingPattern>.Create;
  Result.Add(0, TdxShadingPattern.Clear);
  Result.Add(500, TdxShadingPattern.Pct5);
  Result.Add(1000, TdxShadingPattern.Pct10);
  Result.Add(1250, TdxShadingPattern.Pct12);
  Result.Add(1500, TdxShadingPattern.Pct15);
  Result.Add(2000, TdxShadingPattern.Pct20);
  Result.Add(2500, TdxShadingPattern.Pct25);
  Result.Add(3000, TdxShadingPattern.Pct30);
  Result.Add(3500, TdxShadingPattern.Pct35);
  Result.Add(3750, TdxShadingPattern.Pct37);
  Result.Add(4000, TdxShadingPattern.Pct40);
  Result.Add(4500, TdxShadingPattern.Pct45);
  Result.Add(5000, TdxShadingPattern.Pct50);
  Result.Add(5500, TdxShadingPattern.Pct55);
  Result.Add(6000, TdxShadingPattern.Pct60);
  Result.Add(6250, TdxShadingPattern.Pct62);
  Result.Add(6500, TdxShadingPattern.Pct65);
  Result.Add(7000, TdxShadingPattern.Pct70);
  Result.Add(7500, TdxShadingPattern.Pct75);
  Result.Add(8000, TdxShadingPattern.Pct80);
  Result.Add(8500, TdxShadingPattern.Pct85);
  Result.Add(8750, TdxShadingPattern.Pct87);
  Result.Add(9000, TdxShadingPattern.Pct90);
  Result.Add(9500, TdxShadingPattern.Pct95);
  Result.Add(10000, TdxShadingPattern.Solid);
end;

class function TdxDestinationPieceTable.GetShadingPattern(const Value: Integer): TdxShadingPattern;
begin
  if not FShadingPatterns.TryGetValue(Value, Result) then
    Result := TdxShadingPattern.Clear;
end;

class function TdxDestinationPieceTable.GetShadingValue(const APattern: TdxShadingPattern): Integer;
begin
  for Result in FShadingPatterns.Keys do
    if FShadingPatterns[Result] = APattern then
      Exit;
  Result := 0;
end;

function TdxDestinationPieceTable.GetPieceTable: TdxPieceTable;
begin
  Result := FPieceTable;
end;

destructor TdxDestinationPieceTable.Destroy;
begin
  inherited;
end;

function TdxDestinationPieceTable.GetDocumentModel: TdxDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

procedure TdxDestinationPieceTable.NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase);
var
  AOldListIndex: TdxNumberingListIndex;
  ALevelNumber: Integer;
  AListLevel: IdxListLevel;
  APictureDestination: TdxPictureDestination;
  AImageInfo: TdxRtfImageInfo;
begin
  if ADestination is TdxPictureDestination then
  begin
    APictureDestination := TdxPictureDestination(ADestination);
    AImageInfo := APictureDestination.GetImageInfo;
    try
      InsertImage(Importer, AImageInfo);
    finally
      AImageInfo.Free;
    end;
  end;

  AOldListIndex := IfThen(Importer.Position.CurrentOldSimpleList, Importer.Position.CurrentOldSimpleListIndex,
    Importer.Position.CurrentOldMultiLevelListIndex);
  if AOldListIndex >= NumberingListIndexMinValue then
  begin
    ALevelNumber := IfThen(Importer.Position.CurrentOldSimpleList, 0, Importer.Position.CurrentOldListLevelNumber);
    if ADestination is TdxTextBeforeDestination then
    begin
      Supports(PieceTable.DocumentModel.NumberingLists[AOldListIndex].Levels[ALevelNumber], IdxListLevel, AListLevel);
      AListLevel.ListLevelProperties.DisplayFormatString := TdxTextBeforeDestination(ADestination).Value + AListLevel.ListLevelProperties.DisplayFormatString
    end
    else
      if ADestination is TdxTextAfterDestination then
      begin
        Supports(PieceTable.DocumentModel.NumberingLists[AOldListIndex].Levels[ALevelNumber], IdxListLevel, AListLevel);
        AListLevel.ListLevelProperties.DisplayFormatString := AListLevel.ListLevelProperties.DisplayFormatString + TdxTextAfterDestination(ADestination).Value;
      end;
  end;
end;

procedure TdxDestinationPieceTable.FinalizePieceTableCreation;
begin
  Importer.TableReader.InsertTables;
  FixLastParagraph;
  if Importer.IsContainsParagraphFrame then
    PieceTable.FixParagraphFramesInTables;
  InsertBookmarks;
  InsertRangePermissions;
  Importer.LinkParagraphStylesWithNumberingLists;
  NormalizeProperties;
  FixFieldCodeRunsProperties;
end;

function TdxDestinationPieceTable.CanAppendText: Boolean;
begin
  Result := True;
end;

procedure TdxDestinationPieceTable.FixFieldCodeRunsProperties;
var
  AFields: TdxFieldCollectionBase;
  ARuns: TdxTextRunCollection;
  ACount, I: Integer;
  ASourceRun, AFieldCodeStart, AFieldCodeEnd: TdxTextRunBase;
begin
  AFields := PieceTable.Fields;
  ARuns := PieceTable.Runs;
  ACount := AFields.Count;
  for I := 0 to ACount - 1 do
  begin
    ASourceRun := GetFormattingSourceRun(AFields[I]);
    if ASourceRun = nil then
      Continue;
    AFieldCodeStart := ARuns[AFields[I].Code.Start];
    AFieldCodeEnd := ARuns[AFields[I].Code.&End];

    SetFieldRunFormatting(AFieldCodeStart, ASourceRun);
    SetFieldRunFormatting(AFieldCodeEnd, ASourceRun);
  end;
end;

procedure TdxDestinationPieceTable.FixLastParagraph;
begin
  if PieceTable.ShouldFixLastParagraph then
    PieceTable.FixLastParagraphCore
  else
    Importer.ApplyFormattingToLastParagraph;
  PieceTable.FixTables;
end;

procedure TdxDestinationPieceTable.ProcessCharCore(AChar: Char);
var
  APos: TdxRtfInputPosition;
begin
  APos := Importer.Position;
  if APos.RtfFormattingInfo.Deleted and Importer.Options.IgnoreDeletedText then
    Exit;
  if not APos.UseDoubleByteCharactersFontName and not APos.UseLowAnsiCharactersFontName and not APos.UseHighAnsiCharactersFontName then
    PieceTable.AppendText(APos, AChar)
  else
    AppendChar(AChar);
end;

procedure TdxDestinationPieceTable.ProcessTextCore(const AText: string);
begin
  InsertTextCore(AText);
end;

procedure TdxDestinationPieceTable.InsertTextCore(const AText: string);
var
  APos: TdxRtfInputPosition;
  AChar: Char;
begin
  APos := Importer.Position;
  if APos.RtfFormattingInfo.Deleted and Importer.Options.IgnoreDeletedText then
    Exit;
  if not APos.UseDoubleByteCharactersFontName and not APos.UseLowAnsiCharactersFontName and not APos.UseHighAnsiCharactersFontName then
    PieceTable.AppendText(APos, AText)
  else
    for AChar in AText do
      AppendChar(AChar);
end;

procedure TdxDestinationPieceTable.SetFieldRunFormatting(AFieldCodeStart, ASourceRun: TdxTextRunBase);
begin
  AFieldCodeStart.CharacterProperties.CopyFrom(ASourceRun.CharacterProperties);
  AFieldCodeStart.CharacterStyleIndex := ASourceRun.CharacterStyleIndex;
end;

procedure TdxDestinationPieceTable.StartNewField;
begin
  Importer.Fields.Push(TdxRtfFieldInfo.Create);
  Importer.Destination := TdxFieldDestination.Create(Importer);
end;

class procedure TdxDestinationPieceTable.AddCommonCharacterKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('plain', PlainKeywordHandler);
  ATable.Add('cs', CharacterStyleIndexHandler);
  AddCharacterPropertiesKeywords(ATable);
end;

class procedure TdxDestinationPieceTable.AddCharacterPropertiesKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('b', BoldKeywordHandler);
  ATable.Add('i', ItalicKeywordHandler);
  ATable.Add('ul', UnderlineSingleKeywordHandler);
  ATable.Add('uld', UnderlineDottedKeywordHandler);
  ATable.Add('uldash', UnderlineDashedKeywordHandler);
  ATable.Add('uldashd', UnderlineDashDottedKeywordHandler);
  ATable.Add('uldashdd', UnderlineDashDotDottedKeywordHandler);
  ATable.Add('uldb', UnderlineDoubleKeywordHandler);
  ATable.Add('ulhwave', UnderlineHeavyWaveKeywordHandler);
  ATable.Add('ulldash', UnderlineLongDashedKeywordHandler);
  ATable.Add('ulth', UnderlineThickSingleKeywordHandler);
  ATable.Add('ulthd', UnderlineThickDottedKeywordHandler);
  ATable.Add('ulthdash', UnderlineThickDashedKeywordHandler);
  ATable.Add('ulthdashd', UnderlineThickDashDottedKeywordHandler);
  ATable.Add('ulthdashdd', UnderlineThickDashDotDottedKeywordHandler);
  ATable.Add('ulthldash', UnderlineThickLongDashedKeywordHandler);
  ATable.Add('ululdbwave', UnderlineDoubleWaveKeywordHandler);
  ATable.Add('ulwave', UnderlineWaveKeywordHandler);
  ATable.Add('ulw', UnderlineWordsOnlyKeywordHandler);
  ATable.Add('ulnone', UnderlineNoneKeywordHandler);
  ATable.Add('ulc', UnderlineColorKeywordHandler);
  ATable.Add('strike', StrikeoutKeywordHandler);
  ATable.Add('striked', DoubleStrikeoutKeywordHandler);
  ATable.Add('sub', SubscriptKeywordHandler);
  ATable.Add('super', SuperscriptKeywordHandler);
  ATable.Add('nosupersub', NoSuperAndSubScriptKeywordHandler);
  ATable.Add('lang', LanguageKeywordHandler);
  ATable.Add('langfe', LanguageKeywordHandler);
  ATable.Add('langfenp', LanguageNpKeywordHandler);
  ATable.Add('langnp', LanguageNpKeywordHandler);
  ATable.Add('noproof', NoProofKeywordHandler);
  ATable.Add('caps', CapsKeywordHandler);
  ATable.Add('v', HiddenTextKeywordHandler);
  ATable.Add('fs', FontSizeKeywordHandler);
  ATable.Add('f', FontNameKeywordHandler);
  ATable.Add('af', AssociatedFontNameKeywordHandler);
  ATable.Add('cf', ForeColorKeywordHandler);
  ATable.Add('cb', BackColorKeywordHandler);
  ATable.Add('highlight', BackColorKeywordHandler);
  ATable.Add('chcbpat', BackColorKeywordHandler);
  ATable.Add('dbch', DoubleByteCharactersKeywordHandler);
  ATable.Add('loch', LowAnsiFontNameKeywordHandler);
  ATable.Add('hich', HighAnsiFontNameKeywordHandler);
end;

class procedure TdxDestinationPieceTable.AddCommonParagraphKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('par', ParKeywordHandler);
  ATable.Add('pard', ResetParagraphPropertiesKeywordHandler);
  ATable.Add('s', ParagraphStyleIndexHandler);
  ATable.Add('yts', TableStyleIndexForRowOrCellHandler);
  AddParagraphPropertiesKeywords(ATable);
end;

class procedure TdxDestinationPieceTable.AddCommonSymbolsAndObjectsKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('pict', PictureKeywordHandler);
  ATable.Add('shp', ShapeKeywordHandler);
  ATable.Add('shppict', ShapePictureKeywordHandler);
  ATable.Add('nonshppict', NonShapePictureKeywordHandler);
  ATable.Add('line', LineBreakKeywordHandler);
  ATable.Add('uc', UnicodeCountKeywordHandler);
  ATable.Add('u', UnicodeKeywordHandler);
  ATable.Add('tab', TabKeywordHandler);
  ATable.Add('emdash', EmDashKeywordHandler);
  ATable.Add('endash', EnDashKeywordHandler);
  ATable.Add('lquote', LeftSingleQuoteKeywordHandler);
  ATable.Add('rquote', RightSingleQuoteKeywordHandler);
  ATable.Add('ldblquote', LeftDoubleQuoteKeywordHandler);
  ATable.Add('rdblquote', RightDoubleQuoteKeywordHandler);
  ATable.Add('bullet', BulletKeywordHandler);
  ATable.Add('emspace', EmSpaceKeywordHandler);
  ATable.Add('enspace', EnSpaceKeywordHandler);
  ATable.Add('qmspace', QmSpaceKeywordHandler);
  ATable.Add('field', FieldStartKeywordHandler);
  ATable.Add('mailmerge', MailMergeKeywordHandler);
  ATable.Add('bkmkstart', BookmarkStartKeywordHandler);
  ATable.Add('bkmkend', BookmarkEndKeywordHandler);
  ATable.Add('protstart', RangePermissionStartKeywordHandler);
  ATable.Add('protend', RangePermissionEndKeywordHandler);
  ATable.Add('docvar', DocumentVariableKeywordHandler);
end;

class procedure TdxDestinationPieceTable.AddCommonTabKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('tqr', TabRightKeywordHandler);
  ATable.Add('tqc', TabCenterKeywordHandler);
  ATable.Add('tqdec', TabDecimalKeywordHandler);
  ATable.Add('tldot', TabLeaderDotsKeywordHandler);
  ATable.Add('tlmdot', TabLeaderMiddleDotsKeywordHandler);
  ATable.Add('tlhyph', TabLeaderHyphensKeywordHandler);
  ATable.Add('tlul', TabLeaderUnderlineKeywordHandler);
  ATable.Add('tlth', TabLeaderThickLineKeywordHandler);
  ATable.Add('tleq', TabLeaderEqualSignKeywordHandler);
  ATable.Add('tx', TabPositionKeywordHandler);
  ATable.Add('tb', BarTabKeywordHandler);
end;

class procedure TdxDestinationPieceTable.AddCommonNumberingListsKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('ls', ListOverrideHandler);
  ATable.Add('ilvl', ListLevelHandler);
  ATable.Add('listtext', ListTextHandler);
  ATable.Add('pntext', ListTextHandler);
  ATable.Add('pnseclvl', SectionLevelNumberingKeywordHandler);
  ATable.Add('pn', OldParagraphNumberingKeywordHandler);
  ATable.Add('pntxta', OldParagraphNumberingTextAfterKeywordHandler);
  ATable.Add('pntxtb', OldParagraphNumberingTextBeforeKeywordHandler);
end;

class procedure TdxDestinationPieceTable.AddParagraphPropertiesKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('brdrt', TopParagraphBorderKeywordHandler);
  ATable.Add('brdrb', BottomParagraphBorderKeywordHandler);
  ATable.Add('brdrl', LeftParagraphBorderKeywordHandler);
  ATable.Add('brdrr', RightParagraphBorderKeywordHandler);
  ATable.Add('posx', FrameHorizontalPositionKeywordHandler);
  ATable.Add('posy', FrameVerticalPositionKeywordHandler);
  ATable.Add('absw', FrameWidthKeywordHandler);
  ATable.Add('absh', FrameHeightKeywordHandler);
  ATable.Add('phmrg', ParagraphHorizontalPositionTypeMarginKeywordHandler);
  ATable.Add('phpg', ParagraphHorizontalPositionTypePageKeywordHandler);
  ATable.Add('phcol', ParagraphHorizontalPositionTypeColumnKeywordHandler);
  ATable.Add('pvmrg', ParagraphVerticalPositionTypeMarginKeywordHandler);
  ATable.Add('pvpg', ParagraphVerticalPositionTypePageKeywordHandler);
  ATable.Add('pvpara', ParagraphVerticalPositionTypeLineKeywordHandler);
  ATable.Add('nowrap', FrameNoWrapKeywordHandler);
  ATable.Add('overlay', FrameWrapOverlayKeywordHandler);
  ATable.Add('wrapdefault', FrameWrapDefaultKeywordHandler);
  ATable.Add('wraparound', FrameWrapAroundKeywordHandler);
  ATable.Add('wraptight', FrameWrapTightKeywordHandler);
  ATable.Add('wrapthrough', FrameWrapThroughKeywordHandler);
  ATable.Add('ql', AlignLeftKeywordHandler);
  ATable.Add('qc', AlignCenterKeywordHandler);
  ATable.Add('qr', AlignRightKeywordHandler);
  ATable.Add('qd', AlignJustifyKeywordHandler);
  ATable.Add('qj', AlignJustifyKeywordHandler);
  ATable.Add('li', LeftIndentKeywordHandler);
  ATable.Add('lin', LeftIndentKeywordHandler);
  ATable.Add('ri', RightIndentKeywordHandler);
  ATable.Add('fi', FirstLineIndentKeywordHandler);
  ATable.Add('rin', RightIndentKeywordHandler);
  ATable.Add('sb', SpacingBeforeKeywordHandler);
  ATable.Add('sa', SpacingAfterKeywordHandler);
  ATable.Add('sl', LineSpacingTypeKeywordHandler);
  ATable.Add('slmult', LineSpacingMultiplierKeywordHandler);
  ATable.Add('hyphpar', HyphenateParagraphKeywordHandler);
  ATable.Add('noline', SuppressLineNumbersKeywordHandler);
  ATable.Add('contextualspace', ContextualSpacingKeywordHandler);
  ATable.Add('pagebb', PageBreakBeforeKeywordHandler);
  ATable.Add('sbauto', BeforeAutoSpacingKeywordHandler);
  ATable.Add('saauto', AfterAutoSpacingKeywordHandler);
  ATable.Add('keepn', KeepWithNextKeywordHandler);
  ATable.Add('keep', KeepLinesTogetherKeywordHandler);
  ATable.Add('widctlpar', WidowOrphanControlOnKeywordHandler);
  ATable.Add('nowidctlpar', WidowOrphanControlOffKeywordHandler);
  ATable.Add('outlinelevel', OutlineLevelKeywordHandler);
  ATable.Add('cbpat', ParagraphBackgroundKeywordHandler);
end;

class procedure TdxDestinationPieceTable.AppendTableKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('intbl', InTableParagraphKeywordHandler);
  ATable.Add('row', RowKeywordHandler);
  ATable.Add('cell', CellKeywordHandler);
  ATable.Add('nestcell', NestedCellKeywordHandler);
  ATable.Add('nestrow', NestedRowKeywordHandler);
  ATable.Add('nesttableprops', NestedTablePropertiesKeywordHandler);
  ATable.Add('nonesttables', NoNestedTablesKeywordHandler);
  ATable.Add('itap', ItapKeywordHandler);
  AppendTablePropertiesKeywords(ATable);
end;

class procedure TdxDestinationPieceTable.AppendTablePropertiesKeywords(ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('trowd', TableRowDefaultsKeywordHandler);
  ATable.Add('ts', TableStyleKeywordHandler);
  ATable.Add('cellx', CellxKeywordHandler);
  ATable.Add('clwWidth', CellPreferredWidthKeywordHandler);
  ATable.Add('clftsWidth', WidthUnitTypeKeywordHandler);
  ATable.Add('clmgf', FirstHorizontalMergedCellKeywordHandler);
  ATable.Add('clmrg', NextHorizontalMergedCellKeywordHandler);
  ATable.Add('clvmgf', FirstVerticalMergedCellKeywordHandler);
  ATable.Add('clvmrg', NextVerticalMergedCellKeywordHandler);
  ATable.Add('clFitText', CellFitTextKeywordHandler);
  ATable.Add('clNoWrap', CellNoWrapKeywordHandler);
  ATable.Add('tsvertalt', CellVerticalAlignmentTopKeywordHandler);
  ATable.Add('tsvertalc', CellVerticalAlignmentCenterKeywordHandler);
  ATable.Add('tsvertalb', CellVerticalAlignmentBottomKeywordHandler);
  ATable.Add('clhidemark', CellHideMarkKeywordHandler);
  ATable.Add('clpadb', CellBottomCellMarginKeywordHandler);
  ATable.Add('clpadl', CellLeftCellMarginKeywordHandler);
  ATable.Add('clpadr', CellRightCellMarginKeywordHandler);
  ATable.Add('clpadt', CellTopCellMarginKeywordHandler);
  ATable.Add('clpadfb', CellBottomCellMarginUnitTypeKeywordHandler);
  ATable.Add('clpadfl', CellLeftCellMarginUnitTypeKeywordHandler);
  ATable.Add('clpadfr', CellRightCellMarginUnitTypeKeywordHandler);
  ATable.Add('clpadft', CellTopCellMarginUnitTypeKeywordHandler);
  ATable.Add('clvertalt', CellTextTopAlignmentKeywordHandler);
  ATable.Add('clvertalc', CellTextCenterVerticalAlignmentKeywordHandler);
  ATable.Add('clvertalb', CellTextBottomAlignmentKeywordHandler);
  ATable.Add('cltxlrtb', CellLeftToRightTopToBottomTextDirectionKeywordHandler);
  ATable.Add('cltxtbrl', CellTopToBottomRightToLeftTextDirectionKeywordHandler);
  ATable.Add('cltxbtlr', CellBottomToTopLeftToRightTextDirectionKeywordHandler);
  ATable.Add('cltxlrtbv', CellLeftToRightTopToBottomVerticalTextDirectionKeywordHandler);
  ATable.Add('cltxtbrlv', CellTopToBottomRightToLeftVerticalTextDirectionKeywordHandler);

  ATable.Add('clcbpat', CellBackgroundColorKeywordHandler);
  ATable.Add('clcbpatraw', CellBackgroundColorKeywordHandler);
  ATable.Add('tscellcbpat', CellBackgroundColorKeywordHandler);

  ATable.Add('clcfpat', CellForegroundColorKeywordHandler);
  ATable.Add('clcfpatraw', CellForegroundColorKeywordHandler);
  ATable.Add('clshdng', CellShadingKeywordHandler);
  ATable.Add('clshdngraw', CellShadingKeywordHandler);

  ATable.Add('brdrtbl', NoTableBorderKeywordHandler);
  ATable.Add('clbrdrb', BottomCellBorderKeywordHandler);
  ATable.Add('clbrdrt', TopCellBorderKeywordHandler);
  ATable.Add('clbrdrl', LeftCellBorderKeywordHandler);
  ATable.Add('clbrdrr', RightCellBorderKeywordHandler);
  ATable.Add('cldglu', UpperLeftToLowerRightBorderKeywordHandler);
  ATable.Add('cldgll', UpperRightToLowerLeftBorderKeywordHandler);

  ATable.Add('trleft', RowLeftKeywordHandler);
  ATable.Add('trhdr', RowHeaderKeywordHandler);
  ATable.Add('trrh', RowHeightKeywordHandler);
  ATable.Add('trkeep', RowKeepKeywordHandler);
  ATable.Add('trqr', TableRightAlignmentKeywordHandler);
  ATable.Add('trql', TableLeftAlignmentKeywordHandler);
  ATable.Add('trqc', TableCenterAlignmentKeywordHandler);
  ATable.Add('trwWidthB', WidthBeforeKeywordHandler);
  ATable.Add('trftsWidthB', WidthBeforeUnitTypeKeywordHandler);
  ATable.Add('trwWidthA', WidthAfterKeywordHandler);
  ATable.Add('trftsWidthA', WidthAfterUnitTypeKeywordHandler);

  ATable.Add('trgaph', SpaceBetweenCellsKeywordHandler);
  ATable.Add('trpaddb', TableBottomCellMarginKeywordHandler);
  ATable.Add('trpaddl', TableLeftCellMarginKeywordHandler);
  ATable.Add('trpaddr', TableRightCellMarginKeywordHandler);
  ATable.Add('trpaddt', TableTopCellMarginKeywordHandler);
  ATable.Add('trpaddfb', TableBottomCellMarginUnitTypeKeywordHandler);
  ATable.Add('trpaddfl', TableLeftCellMarginUnitTypeKeywordHandler);
  ATable.Add('trpaddfr', TableRightCellMarginUnitTypeKeywordHandler);
  ATable.Add('trpaddft', TableTopCellMarginUnitTypeKeywordHandler);

  ATable.Add('trspdb', TableBottomCellSpacingKeywordHandler);
  ATable.Add('trspdl', TableLeftCellSpacingKeywordHandler);
  ATable.Add('trspdr', TableRightCellSpacingKeywordHandler);
  ATable.Add('trspdt', TableTopCellSpacingKeywordHandler);
  ATable.Add('trspdfb', TableBottomCellSpacingUnitTypeKeywordHandler);
  ATable.Add('trspdfl', TableLeftCellSpacingUnitTypeKeywordHandler);
  ATable.Add('trspdfr', TableRightCellSpacingUnitTypeKeywordHandler);
  ATable.Add('trspdft', TableTopCellSpacingUnitTypeKeywordHandler);

  ATable.Add('trwWidth', TablePreferredWidthKeywordHandler);
  ATable.Add('trftsWidth', TablePreferredWidthUnitTypeKeywordHandler);
  ATable.Add('tblind', TableIndentKeywordHandler);
  ATable.Add('tblindtype', TableIndentUnitTypeHandler);
  ATable.Add('tabsnoovrlp', TableOverlapKeywordHandler);

  ATable.Add('tdfrmtxtLeft', TableLeftFromTextKeywordHandler);
  ATable.Add('tdfrmtxtRight', TableRightFromTextKeywordHandler);
  ATable.Add('tdfrmtxtTop', TableTopFromTextKeywordHandler);
  ATable.Add('tdfrmtxtBottom', TableBottomFromTextKeywordHandler);
  ATable.Add('tphcol', ColHorizontalAnchorKeywordHandler);
  ATable.Add('tphmrg', MarginHorizontalAnchorKeywordHandler);
  ATable.Add('tphpg', PageHorizontalAnchorKeywordHandler);
  ATable.Add('tpvmrg', MarginVerticalAnchorKeywordHandler);
  ATable.Add('tpvpara', ParagraphVerticalAnchorKewordHandler);
  ATable.Add('tpvpg', PageVerticalAnchorKeywordHandler);
  ATable.Add('tposx', TableHorizontalPositionKeywordHandler);
  ATable.Add('tposnegx', TableHorizontalPositionKeywordHandler);
  ATable.Add('tposy', TableVerticalPositionKeywordHandler);
  ATable.Add('tposnegy', TableVerticalPositionKeywordHandler);
  ATable.Add('tposxc', CenterTableHorizontalAlignKeywordHandler);
  ATable.Add('tposxi', InsideTableHorizontalAlignKeywordHandler);
  ATable.Add('tposxl', LeftTableHorizontalAlignKeywordHandler);
  ATable.Add('tposxo', OutsideTableHorizontalAlignKeywordHandler);
  ATable.Add('tposxr', RightTableHorizontalAlignKeywordHandler);
  ATable.Add('tposyb', BottomTableVerticalAlignKeywordHandler);
  ATable.Add('tposyc', CenterTableVerticalAlignKeywordHandler);
  ATable.Add('tposyil', InlineTableVerticalAlignKeywordHandler);
  ATable.Add('tposyin', InsideTableVerticalAlignKeywordHandler);
  ATable.Add('tposyout', OutsideTableVerticalAlignKeywordHandler);
  ATable.Add('tposyt', TopTableVerticalAlignKeywordHandler);
  ATable.Add('trautofit', TableAutoFitKeywordHandler);

  ATable.Add('tscbandsh', RowBandSizeKeywordHandler);
  ATable.Add('tscbandsv', ColumnBandSizeKeywordHandler);
  ATable.Add('tsbrdrt', TopCellBorderKeywordHandler);
  ATable.Add('tsbrdrl', LeftCellBorderKeywordHandler);
  ATable.Add('tsbrdrb', BottomCellBorderKeywordHandler);
  ATable.Add('tsbrdrr', RightCellBorderKeywordHandler);
  ATable.Add('tsnowrap', CellNoWrapKeywordHandler);
  ATable.Add('tscellpaddb', TableBottomCellMarginKeywordHandler);
  ATable.Add('tscellpaddl', TableLeftCellMarginKeywordHandler);
  ATable.Add('tscellpaddr', TableRightCellMarginKeywordHandler);
  ATable.Add('tscellpaddt', TableTopCellMarginKeywordHandler);
  ATable.Add('tscellpaddfb', TableBottomCellMarginUnitTypeKeywordHandler);
  ATable.Add('tscellpaddfl', TableLeftCellMarginUnitTypeKeywordHandler);
  ATable.Add('tscellpaddfr', TableRightCellMarginUnitTypeKeywordHandler);
  ATable.Add('tscellpaddft', TableTopCellMarginUnitTypeKeywordHandler);
  ATable.Add('tsbrdrdgl', UpperLeftToLowerRightBorderKeywordHandler);
  ATable.Add('tsbrdrdgr', UpperRightToLowerLeftBorderKeywordHandler);
  ATable.Add('tsrowd', TableRowDefaultsKeywordHandler);


  ATable.Add('trbrdrt', TopTableBorderKeywordHandler);
  ATable.Add('trbrdrl', LeftTableBorderKeywordHandler);
  ATable.Add('trbrdrb', BottomTableBorderKewordHandler);
  ATable.Add('trbrdrr', RightTableBorderKeywordHandler);
  ATable.Add('trbrdrh', HorizontalTableBorderKeywordHandler);
  ATable.Add('trbrdrv', VerticalTableBorderKeywordHandler);

  ATable.Add('tbllkhdrrows', ApplyFirstRowConditionalFormattingKeywordHandler);
  ATable.Add('tbllklastrow', ApplyLastRowConditionalFormattingKeywordHandler);
  ATable.Add('tbllkhdrcols', ApplyFirstColumnContitionalFormattingKeywordHandler);
  ATable.Add('tbllklastcol', ApplyLastColumnConditionalFormattingKeywordHandler);
  ATable.Add('tbllknorowband', DoNotApplyRowBandingConditionalFormattingKeywordHandler);
  ATable.Add('tbllknocolband', DoNotApplyColumnBandingConditionalFormattingKeywordHandler);

  ATable.Add('brdrnil', NoBorderKeywordHandler);
  ATable.Add('brdrw', BorderWidthKeywordHandler);
  ATable.Add('brdrcf', BorderColorKeywordHandler);
  ATable.Add('brdrframe', FrameBorderKeywordHandler);
  ATable.Add('brsp', BorderSpaceKeywordHandler);
  ATable.Add('brdrs', SingleThicknessBorderTypeKeywordHandler);
  ATable.Add('brdrth', DoubleThicknessBorderTypeKeywordHandler);
  ATable.Add('brdrsh', ShadowedBorderTypeKeywordHandler);
  ATable.Add('brdrdb', DoubleBorderTypeKeywordHandler);
  ATable.Add('brdrdot', DottedBorderTypeKeywordHandler);
  ATable.Add('brdrdash', DashedBorderTypeKeywordHandler);
  ATable.Add('brdrhair', HairlineBorderTypeKeywordHandler);
  ATable.Add('brdrdashsm', SmallDashedBorderTypeKeywordHandler);
  ATable.Add('brdrdashd', DotDashedBorderTypeKeywordHandler);
  ATable.Add('brdrdashdd', DotDotDashedBorderTypeKeywordHandler);
  ATable.Add('brdrdashdot', DotDashedBorderTypeKeywordHandler);
  ATable.Add('brdrdashdotdot', DotDotDashedBorderTypeKeywordHandler);
  ATable.Add('brdrinset', InsetBorderTypeKeywordHandler);
  ATable.Add('brdrnone', NoneBorderTypeKeywordHandler);
  ATable.Add('brdroutset', OutsetBorderTypeKeywordHandler);
  ATable.Add('brdrtriple', TripletBorderTypeKeywordHandler);
  ATable.Add('brdrtnthsg', SmallThickThinBorderTypeKeywordHandler);
  ATable.Add('brdrthtnsg', SmallThinThickBorderTypeKeywordHandler);
  ATable.Add('brdrtnthtnsg', SmallThinThickThinBorderTypeKeywordHandler);
  ATable.Add('brdrtnthmg', MediumThickThinBorderTypeKeywordHandler);
  ATable.Add('brdrthtnmg', MediumThinThickBorderTypeKeywordHandler);
  ATable.Add('brdrtnthtnmg', MediumThinThickThinBorderTypeKeywordHandler);
  ATable.Add('brdrtnthlg', LargeThickThinBorderTypeKeywordHandler);
  ATable.Add('brdrthtnlg', LargeThinThickBorderTypeKeywordHandler);
  ATable.Add('brdrtnthtnlg', LargeThinThickThinBorderTypeKeywordHandler);
  ATable.Add('brdrwavy', WavyBorderTypeKeywordHandler);
  ATable.Add('brdrwavydb', DoubleWavyBorderTypeKeywordHandler);
  ATable.Add('brdrdashdotstr', StripedBorderTypeKeywordHandler);
  ATable.Add('brdremboss', EmbossedBorderTypeKeywordHandler);
  ATable.Add('brdrengrave', EngravedBorderTypeKeywordHandler);
  ATable.Add('brdrart', BorderArtIndexHandler);
end;

function TdxDestinationPieceTable.CalculateCharType(AChar: Char): TCharType;
begin
  if IsLowAnsiCharacter(AChar) then
    Exit(TCharType.LowAnsiCharacter);
  if IsDoubleByteChar(AChar) then
    Exit(TCharType.DoubleByteCharacter);
  Result := TCharType.HighAnsiCharacter;
end;

procedure TdxDestinationPieceTable.AppendChar(AChar: Char);
var
  APos: TdxRtfInputPosition;
  AOldFontName: string;
  ACharType: TCharType;
begin
  APos := Importer.Position;
  AOldFontName := APos.CharacterFormatting.FontName;
  if APos.UseDoubleByteCharactersFontName or APos.UseLowAnsiCharactersFontName or APos.UseHighAnsiCharactersFontName then
  begin
    ACharType := CalculateCharType(AChar);
    if APos.UseDoubleByteCharactersFontName and (ACharType = TCharType.DoubleByteCharacter) then
      APos.CharacterFormatting.FontName := APos.DoubleByteCharactersFontName
    else
      if APos.UseLowAnsiCharactersFontName and (ACharType = TCharType.LowAnsiCharacter) then
        APos.CharacterFormatting.FontName := APos.LowAnsiCharactersFontName
      else
        if APos.UseHighAnsiCharactersFontName and (ACharType = TCharType.HighAnsiCharacter) then
          APos.CharacterFormatting.FontName := APos.HighAnsiCharactersFontName;
  end;
  PieceTable.AppendText(APos, AChar);
  APos.CharacterFormatting.FontName := AOldFontName;
end;

function TdxDestinationPieceTable.GetFormattingSourceRun(AField: TdxField): TdxTextRunBase;
var
  ARuns: TdxTextRunCollection;
  ARunIndex: TdxRunIndex;
  ARun: TdxTextRunBase;
begin
  ARuns := PieceTable.Runs;
  for ARunIndex := AField.FirstRunIndex + 1 to AField.Code.&End - 1 do
  begin
    ARun := ARuns[ARunIndex];
    if not (ARun is TdxFieldCodeRunBase) and not (ARun is TdxFieldResultEndRun) then
      Exit(ARun);
  end;
  Result := nil;
end;

function TdxDestinationPieceTable.GetNormalizedNumberingListIndex(AOwnNumberingListIndex, AParentNumberingListIndex: TdxNumberingListIndex): TdxNumberingListIndex;
begin
  Result := AOwnNumberingListIndex;
  if AOwnNumberingListIndex = AParentNumberingListIndex then
    Result := NumberingListIndexListIndexNotSetted
  else
    if (AOwnNumberingListIndex = NumberingListIndexNoNumberingList) or (AOwnNumberingListIndex = NumberingListIndexListIndexNotSetted) then
    begin
      if AParentNumberingListIndex >= NumberingListIndexMinValue then
        Result := NumberingListIndexNoNumberingList
      else
        Result := NumberingListIndexListIndexNotSetted;
    end;
end;

function TdxDestinationPieceTable.GetTableStyleToMerge(ATableStyleIndex: Integer): TdxTableStyle;
var
  ATableStyle: TdxTableStyle;
begin
  if ATableStyleIndex >= 0 then
  begin
    ATableStyle := DocumentModel.TableStyles[ATableStyleIndex];
    if ATableStyle.StyleName <> TdxTableStyleCollection.DefaultTableStyleName then
      Exit(ATableStyle);
  end;
  Result := nil;
end;

class function TdxDestinationPieceTable.GetWidthUnitType(
  AParameterValue: Integer): TdxWidthUnitType;
begin
  case AParameterValue of
    0: Result := TdxWidthUnitType.Nil;
    1: Result := TdxWidthUnitType.Auto;
    2: Result := TdxWidthUnitType.FiftiethsOfPercent;
    3: Result := TdxWidthUnitType.ModelUnits;
  else
    Result := TdxWidthUnitType.Nil;
  end;
end;

class procedure TdxDestinationPieceTable.SetBorderType(AImporter: TdxRtfImporter;
  AStyle: TdxBorderLineStyle);
var
  ABorder: TdxBorderBase;
  AParagraphBorder: TdxBorderInfo;
begin
  ABorder := AImporter.TableReader.ProcessedBorder;
  if ABorder <> nil then
  begin
    AssignDefaultBorderWidth(ABorder);
    ABorder.Style := AStyle;
  end;
  AParagraphBorder := AImporter.Position.ParagraphFormattingInfo.ProcessedBorder;
  if AParagraphBorder <> nil then
    AParagraphBorder.Style := AStyle;
end;

type
  TdxInlinePictureRunAccess = class(TdxInlinePictureRun);

class procedure TdxDestinationPieceTable.InsertImage(
  AImporter: TdxRtfImporter; AImageInfo: TdxRtfImageInfo);
var
  ATable: TdxPieceTable;
  AScaleX, AScaleY: Integer;
  ARun: TdxInlinePictureRunAccess;
begin
  if not TdxDocumentFormatsHelper.ShouldInsertPicture(AImporter.DocumentModel) or
    (AImageInfo = nil) or (AImageInfo.RtfImage = nil) then
  begin
    InsertSpace(AImporter);
    Exit;
  end;
  ATable := AImporter.PieceTable;

  AScaleX := Max(1, AImageInfo.ScaleX);
  AScaleY := Max(1, AImageInfo.ScaleY);
  ATable.AppendImage(AImporter.Position, AImageInfo.RtfImage, AScaleX, AScaleY);
  ARun := TdxInlinePictureRunAccess(ATable.LastInsertedInlinePictureRunInfo.Run);
  ARun.SetOriginalSize(AImageInfo.SizeInModelUnits);
  ARun.Properties.PseudoInline := AImageInfo.PseudoInline;
end;

class procedure TdxDestinationPieceTable.InsertSpace(AImporter: TdxRtfImporter);
begin
  AImporter.PieceTable.InsertTextCore(AImporter.Position, ' ');
end;

function TdxDestinationPieceTable.IsDoubleByteChar(AChar: Char): Boolean;
var
  AValue: Integer;
begin
  AValue := Ord(AChar);
  if AValue < LowLatinExtendedAdditional then
    Exit(False);
  if (AValue >= LowLatinExtendedAdditional) and (AValue <= HighLatinExtendedAdditional) then
    Exit(False);
  if (AValue >= LowGeneralPunctuation) and (AValue <= HighGeneralPunctuation) then
    Exit(False);
  if (AValue >= LowCurrencySymbols) and (AValue <= HighCurrencySymbols) then
    Exit(False);
  if (AValue >= LowLetterlikeSymbols) and (AValue <= HighLetterlikeSymbols) then
    Exit(False);
  if (AValue >= LowNumberForms) and (AValue <= HighNumberForms) then
    Exit(False);
  if (AValue >= LowMathematicalOperations) and (AValue <= HighMathematicalOperations) then
    Exit(False);
  Result := True;
end;

function TdxDestinationPieceTable.IsLowAnsiCharacter(AChar: Char): Boolean;
var
  AValue: Integer;
begin
  AValue := Ord(AChar);
  Result := (AValue >= LowAnsiCharactersStart) and (AValue <= LowAnsiCharactersEnd);
end;

procedure TdxDestinationPieceTable.InsertBookmarks;
var
  ABookmarks: TdxImportBookmarkInfos;
  ABookmark: TdxImportBookmarkInfo;
  AName: string;
begin
  if not DocumentModel.DocumentCapabilities.BookmarksAllowed then
    Exit;

  ABookmarks := Importer.Bookmarks;
  for AName in ABookmarks.Keys do
  begin
    ABookmark := ABookmarks[AName];
    if ABookmark.Validate(PieceTable) then
      PieceTable.CreateBookmarkCore(ABookmark.Start, ABookmark.&End - ABookmark.Start, AName);
  end;
end;

procedure TdxDestinationPieceTable.InsertRangePermissions;
var
  ARangePermissions: TdxNamedObjectDictionary<TdxImportRangePermissionInfo>;
  AName: string;
  ARangePermission: TdxImportRangePermissionInfo;
begin
  ARangePermissions := Importer.RangePermissions;
  for AName in ARangePermissions.Keys do
  begin
    ARangePermission := ARangePermissions[AName];
    if ARangePermission.Validate(PieceTable) then
      PieceTable.ApplyDocumentPermission(ARangePermission.Start, ARangePermission.&End, ARangePermission.PermissionInfo);
  end;
end;

procedure TdxDestinationPieceTable.NormalizeProperties;
begin
  NormalizeParagraphStyleNumbering;
  NormalizeParagraphsProperties;
  NormalizeTablesProperties;
end;

procedure TdxDestinationPieceTable.NormalizeParagraphsProperties;
var
  ACachedResult: TdxCustomMergedParagraphPropertiesCachedResult;
  AParagraphs: TdxParagraphCollection;
  AParagraph: TdxParagraph;
  ACount: TdxParagraphIndex;
  I, ARunIndex: Integer;
  AParagraphStyles: TdxIntegersDictionary;
  ATableStyleIndex: Integer;
  ATextRunsCachedResults: TObjectDictionary<TClass, TdxCustomMergedCharacterPropertiesCachedResult>;
begin
  ATextRunsCachedResults := TObjectDictionary<TClass, TdxCustomMergedCharacterPropertiesCachedResult>.Create([doOwnsValues]);
  try
    ACachedResult := TdxCustomMergedParagraphPropertiesCachedResult.Create;
    try
      AParagraphStyles := Importer.PieceTableInfo.ParagraphTableStyles;
      AParagraphs := PieceTable.Paragraphs;
      ACount := AParagraphs.Count;
      ARunIndex := 0;
      for I := 0 to ACount - 1 do
      begin
        AParagraph := AParagraphs[I];
        if AParagraphStyles.ContainsKey(I) then
          ATableStyleIndex := Importer.GetTableStyleIndex(AParagraphStyles[I], -1)
        else
          ATableStyleIndex := -1;
        NormalizeParagraphProperties(AParagraph, ACachedResult, ATableStyleIndex);
        ARunIndex := NormalizeRunsProperties(AParagraph, ARunIndex, ATableStyleIndex, ATextRunsCachedResults);
        NormalizeTabs(AParagraph);
      end;
    finally
      ACachedResult.Free;
    end;
  finally
    ATextRunsCachedResults.Free;
  end;
end;

procedure TdxDestinationPieceTable.NormalizeTabs(AParagraph: TdxParagraph);
var
  AParentTabs, AParagraphTabs, ANormalizedTabs: TdxTabFormattingInfo;
begin
  AParagraphTabs := AParagraph.Tabs.Info;
  AParentTabs := AParagraph.GetParentTabs;
  try
    ANormalizedTabs := TdxTabFormattingInfo.NormalizeTabs(AParagraphTabs, AParentTabs);
  finally
    AParentTabs.Free;
  end;
  try
    if ANormalizedTabs <> nil then
      AParagraph.SetOwnTabs(ANormalizedTabs);
  finally
    ANormalizedTabs.Free;
  end;
end;

procedure TdxDestinationPieceTable.NormalizeParagraphStyleNumbering(
  AParagraphStyle: TdxParagraphStyle);
var
  AOwnNumberingListIndex: Integer;
  AParentNumberingListIndex: Integer;
begin
  AOwnNumberingListIndex := AParagraphStyle.GetOwnNumberingListIndex;
  if AParagraphStyle.Parent = nil then
  begin
    if AOwnNumberingListIndex = NumberingListIndexNoNumberingList then
      AOwnNumberingListIndex := NumberingListIndexListIndexNotSetted;
    AParagraphStyle.SetNumberingListIndex(AOwnNumberingListIndex);
  end
  else
  begin
    AParentNumberingListIndex := AParagraphStyle.Parent.GetNumberingListIndex;
    AParagraphStyle.SetNumberingListIndex(GetNormalizedNumberingListIndex(AOwnNumberingListIndex, AParentNumberingListIndex));
  end;
end;

procedure TdxDestinationPieceTable.NormalizeParagraphStyleNumbering;
var
  AStyles: TdxParagraphStyleCollection;
  ACount, I: Integer;
begin
  AStyles := DocumentModel.ParagraphStyles;
  ACount := AStyles.Count;
  for I := 0 to ACount - 1 do
    NormalizeParagraphStyleNumbering(AStyles[I]);
end;

procedure TdxDestinationPieceTable.NormalizeParagraphProperties(AParagraph: TdxParagraph;
  ACachedResult: TdxCustomMergedParagraphPropertiesCachedResult; ATableStyleIndex: Integer);
var
  AProperties: TdxParagraphProperties;
  AParentProperties: TdxMergedParagraphProperties;
  ATableStyleToMerge: TdxTableStyle;
begin
  AProperties := AParagraph.ParagraphProperties;
  if (ACachedResult.InitialParagraphPropertiesIndex = AProperties.Index) and
      AParagraph.TryUseParentMergedCachedResult(ACachedResult, ATableStyleIndex) then
    AProperties.SetIndexInitial(ACachedResult.FinalParagraphPropertiesIndex)
  else
  begin
    ACachedResult.InitialParagraphPropertiesIndex := AProperties.Index;
    ATableStyleToMerge := GetTableStyleToMerge(ATableStyleIndex);
    AParentProperties := AParagraph.GetParentMergedWithTableStyleParagraphProperties(True, ATableStyleToMerge);
    ACachedResult.MergedParagraphProperties := AParentProperties;
    Importer.ApplyParagraphProperties(AProperties, AProperties.Info.Info, AParentProperties);
    ACachedResult.FinalParagraphPropertiesIndex := AProperties.Index;
  end;
  if AParagraph.GetListLevelIndex = 0 then
    AParagraph.SetNumberingListIndex(GetNormalizedNumberingListIndex(AParagraph.GetOwnNumberingListIndex, AParagraph.ParagraphStyle.GetNumberingListIndex));
end;

procedure TdxDestinationPieceTable.NormalizeRunProperties(
  ARun: TdxTextRunBase;
  ACachedResult: TdxCustomMergedCharacterPropertiesCachedResult; ATableStyleIndex: Integer);

  function GetParentMergedCharacterProperties(ARun: TdxTextRunBase; AUseSpecialTableStyle: Boolean; ATableStyle: TdxTableStyle): TdxMergedCharacterProperties;
  var
    ACharacterStyleProperties, ACharacterProperties: TdxMergedCharacterProperties;
  begin
    ACharacterStyleProperties := ARun.CharacterStyle.GetMergedCharacterProperties;
    try
      ACharacterProperties := TdxParagraph(ARun.Paragraph).GetMergedCharacterProperties(AUseSpecialTableStyle, ATableStyle);
      try
        Result := TdxMergedCharacterProperties.Create(ACharacterStyleProperties);
        Result.Merge(ACharacterProperties);
      finally
        ACharacterProperties.Free;
      end;
    finally
      ACharacterStyleProperties.Free;
    end
  end;

var
  AProperties: TdxCharacterProperties;
  AParentProperties: TdxMergedCharacterProperties;
  ATableStyle: TdxTableStyle;
  ACanSetIndexInitial: Boolean;
begin
  AProperties := ARun.CharacterProperties;
  ACanSetIndexInitial := ARun.TryUseParentMergedCachedResult(ACachedResult);
  if ACanSetIndexInitial and
    (ACachedResult.InitialRunCharacterPropertiesIndex = AProperties.Index) and
    (ACachedResult.TableStyleIndex = ATableStyleIndex) then
  begin
    AProperties.SetIndexInitial(ACachedResult.FinalRunCharacterPropertiesIndex);
    Exit;
  end;
  ACachedResult.InitialRunCharacterPropertiesIndex := AProperties.Index;
  ACachedResult.TableStyleIndex := ATableStyleIndex;
  ATableStyle := GetTableStyleToMerge(ATableStyleIndex);
  AParentProperties := GetParentMergedCharacterProperties(ARun, True, ATableStyle);
  ACachedResult.MergedCharacterProperties := AParentProperties;
  Importer.ApplyCharacterProperties(AProperties, AProperties.Info.Info, AParentProperties);
  ACachedResult.FinalRunCharacterPropertiesIndex := AProperties.Index;
end;

function TdxDestinationPieceTable.NormalizeRunsProperties(AParagraph: TdxParagraph;
  ARunIndex, ATableStyleIndex: Integer;
  ACachedResults: TObjectDictionary<TClass, TdxCustomMergedCharacterPropertiesCachedResult>): TdxRunIndex;
var
  ARun: TdxTextRunBase;
  ARuns: TdxTextRunCollection;
  ACount: TdxRunIndex;
  ACachedResult: TdxCustomMergedCharacterPropertiesCachedResult;
begin
  ARuns := PieceTable.Runs;
  ACount := ARuns.Count;
  while ARunIndex < ACount do
  begin
    ARun := ARuns[ARunIndex];
    if AParagraph <> ARun.Paragraph then
      Break;
    if not ACachedResults.TryGetValue(ARun.ClassType, ACachedResult) then
    begin
        ACachedResult := TdxCustomMergedCharacterPropertiesCachedResult.Create;
        ACachedResults.Add(ARun.ClassType, ACachedResult);
    end;
    NormalizeRunProperties(ARun, ACachedResult, ATableStyleIndex);
    Inc(ARunIndex);
  end;
  Result := ARunIndex;
end;

procedure TdxDestinationPieceTable.NormalizeTableCellsProperties(
  ARow: TdxTableRow);
begin
  ARow.Cells.ForEach(NormalizeTableCellProperties);
end;

procedure TdxDestinationPieceTable.NormalizeTableCellProperties(
  const ACell: TdxTableCell);
var
  ACellProperties: TdxTableCellProperties;
  AParentProperties: TdxMergedTableCellProperties;
begin
  ACell.UnsubscribeCellPropertiesEvents;
  try
    ACellProperties := ACell.Properties;
    AParentProperties := ACell.GetParentMergedTableCellProperties;
    try
      Importer.ApplyTableCellProperties(ACellProperties, AParentProperties);
    finally
      AParentProperties.Free;
    end;
  finally
    ACell.SubscribeCellPropertiesEvents;
  end;
end;

procedure TdxDestinationPieceTable.NormalizeTableProperties(const ATable: TdxTable);
var
  ATableProperties: TdxTableProperties;
  AParentProperties: TdxMergedTableProperties;
begin
  ATableProperties := ATable.TableProperties;
  AParentProperties := ATable.GetParentMergedTableProperties;
  try
    Importer.ApplyTableProperties(ATableProperties, AParentProperties);
    NormalizeTableRowsProperties(ATable);
  finally
    AParentProperties.Free;
  end;
end;

procedure TdxDestinationPieceTable.NormalizeTableRowProperties(
  const ARow: TdxTableRow);
var
  ARowProperties: TdxTableRowProperties;
  AParentProperties: TdxMergedTableRowProperties;
begin
  ARow.UnsubscribeRowPropertiesEvents;
  try
    ARowProperties := ARow.Properties;
    AParentProperties := ARow.GetParentMergedTableRowProperties;
    try
      Importer.ApplyTableRowProperties(ARowProperties, AParentProperties);
      NormalizeTableCellsProperties(ARow);
    finally
      AParentProperties.Free;
    end;
  finally
    ARow.SubscribeRowPropertiesEvents;
  end;
end;

procedure TdxDestinationPieceTable.NormalizeTableRowsProperties(
  ATable: TdxTable);
begin
  ATable.Rows.ForEach(NormalizeTableRowProperties);
end;

procedure TdxDestinationPieceTable.NormalizeTablesProperties;
begin
  PieceTable.Tables.ForEach(NormalizeTableProperties);
  PieceTable.FixTables;
end;

class procedure TdxDestinationPieceTable.AssignDefaultBorderWidth(
  ABorder: TdxBorderBase);
begin
//do nothing
end;

class procedure TdxDestinationPieceTable.AssignWidthUnitInfo(AUnitInfo: TdxWidthUnit; Value: Integer);
begin
  if Value = 3 then
    AUnitInfo.&Type := TdxWidthUnitType.ModelUnits
  else
    if Value = 0 then
      AUnitInfo.&Type := TdxWidthUnitType.Nil;
end;

class function TdxDestinationPieceTable.ShouldApplyParagraphStyle(AImporter: TdxRtfImporter): Boolean;
begin
  Result := AImporter.DocumentModel.DocumentCapabilities.ParagraphStyleAllowed;
end;

class procedure TdxDestinationPieceTable.NonBreakingSpaceCharHandler(AImporter: TdxRtfImporter; var AChar: Char);
begin
  AImporter.FlushDecoder;
  AImporter.ParseCharWithoutDecoding(TdxCharacters.NonBreakingSpace);
end;

class procedure TdxDestinationPieceTable.NonBreakingHyphenCharHandler(AImporter: TdxRtfImporter; var AChar: Char);
begin
  AImporter.FlushDecoder;
  AImporter.ParseCharWithoutDecoding('-');
end;

class procedure TdxDestinationPieceTable.OptionalHyphenCharHandler(AImporter: TdxRtfImporter; var AChar: Char);
begin
//do nothing
end;

class procedure TdxDestinationPieceTable.AlignLeftKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.Alignment := TdxParagraphAlignment.Left;
end;

class procedure TdxDestinationPieceTable.AlignCenterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.Alignment := TdxParagraphAlignment.Center;
end;

class procedure TdxDestinationPieceTable.AlignRightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.Alignment := TdxParagraphAlignment.Right;
end;

class procedure TdxDestinationPieceTable.AlignJustifyKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.Alignment := TdxParagraphAlignment.Justify;
end;

class procedure TdxDestinationPieceTable.AssociatedFontNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AImporter.Position.FontType = TdxRtfFontType.Undefined then
    Exit;
  FontNameKeywordHandler(AImporter, AParameterValue, AHasParameter);
end;

class procedure TdxDestinationPieceTable.BackColorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AProperties: TdxRtfDocumentProperties;
  AColor: TdxAlphaColor;
begin
  if not AHasParameter then
    AParameterValue := 0;
  AProperties := AImporter.DocumentProperties;
  AColor := AProperties.Colors.GetRtfColorById(AParameterValue);
  AImporter.Position.CharacterFormatting.BackColor := AColor;
end;

class procedure TdxDestinationPieceTable.BarTabKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AInfo: TdxRtfParagraphFormattingInfo;
begin
  AInfo := AImporter.Position.ParagraphFormattingInfo;
  AInfo.TabAlignment := TdxTabInfo.DefaultAlignment;
  AInfo.TabLeader := TdxTabInfo.DefaultLeader;
end;

class procedure TdxDestinationPieceTable.BoldKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.CharacterFormatting.FontBold := not AHasParameter or (AParameterValue <> 0);
end;

class procedure TdxDestinationPieceTable.BookmarkEndKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxBookmarkEndDestination.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.BookmarkStartKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxBookmarkStartDestination.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.BulletKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.ParseCharWithoutDecoding(TdxCharacters.Bullet);
end;

class procedure TdxDestinationPieceTable.CapsKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.CharacterFormatting.AllCaps := not AHasParameter or (AParameterValue > 0);
end;

class procedure TdxDestinationPieceTable.CharacterStyleIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AImporter.DocumentModel.DocumentCapabilities.CharacterStyleAllowed then
    AImporter.Position.CharacterStyleIndex := AImporter.GetCharacterStyleIndex(AParameterValue);
end;

class procedure TdxDestinationPieceTable.DocumentVariableKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxDocumentVariableDestination.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.DoubleByteCharactersKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.FontType := TdxRtfFontType.DoubleByteCharactersFont;
end;

class procedure TdxDestinationPieceTable.LowAnsiFontNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.FontType := TdxRtfFontType.LowAnsiCharactersFont;
end;

class procedure TdxDestinationPieceTable.HighAnsiFontNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.FontType := TdxRtfFontType.HighAnsiCharactersFont;
end;

class procedure TdxDestinationPieceTable.DoubleStrikeoutKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
const
  AValueMap: array[Boolean] of TdxStrikeoutType = (TdxStrikeoutType.None, TdxStrikeoutType.Double);
begin
  AImporter.Position.CharacterFormatting.FontStrikeoutType := AValueMap[not AHasParameter or (AParameterValue > 0)];
end;

class procedure TdxDestinationPieceTable.EmDashKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.ParseCharWithoutDecoding(TdxCharacters.EmDash);
end;

class procedure TdxDestinationPieceTable.EmSpaceKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.ParseCharWithoutDecoding(TdxCharacters.EmSpace);
end;

class procedure TdxDestinationPieceTable.EnDashKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.ParseCharWithoutDecoding(TdxCharacters.EnDash);
end;

class procedure TdxDestinationPieceTable.EnSpaceKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.ParseCharWithoutDecoding(TdxCharacters.EnSpace);
end;

class function TdxDestinationPieceTable.EnsureFramePropertiesExists(
  AImporter: TdxRtfImporter): Boolean;
begin
  Result := not TdxPieceTable(AImporter.Position.PieceTable).IsTextBox;
  if Result then
  begin
    if AImporter.Position.ParagraphFrameFormattingInfo = nil then
      AImporter.Position.ParagraphFrameFormattingInfo := TdxParagraphFrameFormattingInfo.Create;
  end;
end;

class procedure TdxDestinationPieceTable.FieldStartKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxDestinationPieceTable;
begin
  ADestination := TdxDestinationPieceTable(AImporter.Destination);
  ADestination.StartNewField;
end;

class procedure TdxDestinationPieceTable.FirstLineIndentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AInfo: TdxParagraphFormattingInfo;
  AIndent: Integer;
begin
  AInfo := AImporter.Position.ParagraphFormattingInfo;
  AIndent := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
  if not ShouldApplyParagraphStyle(AImporter) or (AIndent = 0) then
  begin
    AInfo.FirstLineIndentType := TdxParagraphFirstLineIndent.None;
    AInfo.FirstLineIndent := 0;
  end
  else
  begin
    if AIndent > 0 then
    begin
      AInfo.FirstLineIndentType := TdxParagraphFirstLineIndent.Indented;
      AInfo.FirstLineIndent := AIndent;
    end
    else
      if AIndent < 0 then
      begin
        AInfo.FirstLineIndentType := TdxParagraphFirstLineIndent.Hanging;
        AInfo.FirstLineIndent := -AIndent;
      end;
  end;
end;

class procedure TdxDestinationPieceTable.FontNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := AImporter.DocumentProperties.DefaultFontNumber;
  AImporter.SetFont(AImporter.DocumentProperties.Fonts.GetRtfFontInfoById(AParameterValue));
end;

class procedure TdxDestinationPieceTable.FontSizeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 24;
  AImporter.Position.CharacterFormatting.DoubleFontSize := Max(TdxPredefinedFontSizeCollection.MinFontSize, AParameterValue);
end;

class procedure TdxDestinationPieceTable.ForeColorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AProperties: TdxRtfDocumentProperties;
  AForeColor: TdxAlphaColor;
begin
  if not AHasParameter then
    AParameterValue := 0;
  AProperties := AImporter.DocumentProperties;
  AForeColor := AProperties.Colors.GetRtfColorById(AParameterValue);
  AImporter.Position.CharacterFormatting.ForeColor := AForeColor;
end;

class procedure TdxDestinationPieceTable.HiddenTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.CharacterFormatting.Hidden := not AHasParameter or (AParameterValue <> 0);
end;

class procedure TdxDestinationPieceTable.ItalicKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.CharacterFormatting.FontItalic := not AHasParameter or (AParameterValue <> 0);
end;

class procedure TdxDestinationPieceTable.LeftDoubleQuoteKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.ParseCharWithoutDecoding(TdxCharacters.LeftDoubleQuote);
end;

class procedure TdxDestinationPieceTable.LeftIndentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.LeftIndent := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDestinationPieceTable.LeftSingleQuoteKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.ParseCharWithoutDecoding(TdxCharacters.LeftSingleQuote);
end;

class procedure TdxDestinationPieceTable.LineBreakKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  case AImporter.Options.LineBreakSubstitute of
    TdxLineBreakSubstitute.Space:
      AImporter.ParseCharWithoutDecoding(TdxCharacters.Space);
    TdxLineBreakSubstitute.Paragraph:
      ParKeywordHandler(AImporter, 0, False);
  else
    AImporter.ParseCharWithoutDecoding(TdxCharacters.LineBreak);
  end;
end;

class procedure TdxDestinationPieceTable.ListLevelHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.ListLevelIndex := AParameterValue;
end;

class procedure TdxDestinationPieceTable.ListOverrideHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AIndex: TdxNumberingListIndex;
begin
  if AImporter.ListOverrideIndexToNumberingListIndexMap.TryGetValue(AParameterValue, AIndex) then
    AImporter.Position.ParagraphFormattingInfo.NumberingListIndex := AIndex;
end;

class procedure TdxDestinationPieceTable.LineSpacingTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
  begin
    AImporter.Position.ParagraphFormattingInfo.RtfLineSpacingType := AParameterValue;
    AImporter.Position.ParagraphFormattingInfo.RtfLineSpacingMultiplier := 0;
    AImporter.Position.ParagraphFormattingInfo.UseLineSpacingMultiplier := False;
  end;
end;

class procedure TdxDestinationPieceTable.LineSpacingMultiplierKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
  begin
    AImporter.Position.ParagraphFormattingInfo.RtfLineSpacingMultiplier := Max(0, AParameterValue);
    AImporter.Position.ParagraphFormattingInfo.UseLineSpacingMultiplier := True;
  end;
end;

class procedure TdxDestinationPieceTable.HyphenateParagraphKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not ShouldApplyParagraphStyle(AImporter) then
    Exit;
  AImporter.Position.ParagraphFormattingInfo.SuppressHyphenation := AHasParameter and (AParameterValue = 0);
end;

class procedure TdxDestinationPieceTable.SuppressLineNumbersKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.SuppressLineNumbers := True;
end;

class procedure TdxDestinationPieceTable.ContextualSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.ContextualSpacing := True;
end;

class procedure TdxDestinationPieceTable.PageBreakBeforeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not ShouldApplyParagraphStyle(AImporter) then
    Exit;

  if not AHasParameter then
    AParameterValue := 1;
  AImporter.Position.ParagraphFormattingInfo.PageBreakBefore := AParameterValue <> 0;
end;

class procedure TdxDestinationPieceTable.BeforeAutoSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not ShouldApplyParagraphStyle(AImporter) then
    Exit;
  AImporter.Position.ParagraphFormattingInfo.BeforeAutoSpacing := AParameterValue <> 0;
  if AParameterValue <> 0 then
    AImporter.Position.ParagraphFormattingInfo.SpacingBefore := 0;
end;

class procedure TdxDestinationPieceTable.AfterAutoSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not ShouldApplyParagraphStyle(AImporter) then
    Exit;
  AImporter.Position.ParagraphFormattingInfo.AfterAutoSpacing := AParameterValue <> 0;
  if AParameterValue <> 0 then
    AImporter.Position.ParagraphFormattingInfo.SpacingAfter := 0;
end;

class procedure TdxDestinationPieceTable.KeepWithNextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not ShouldApplyParagraphStyle(AImporter) then
    Exit;

  if not AHasParameter then
    AParameterValue := 1;
  AImporter.Position.ParagraphFormattingInfo.KeepWithNext := AParameterValue <> 0;
end;

class procedure TdxDestinationPieceTable.KeepLinesTogetherKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not ShouldApplyParagraphStyle(AImporter) then
    Exit;

  if not AHasParameter then
    AParameterValue := 1;
  AImporter.Position.ParagraphFormattingInfo.KeepLinesTogether := AParameterValue <> 0;
end;

class procedure TdxDestinationPieceTable.ListTextHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.MailMergeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxMailMergeDestination.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.NonShapePictureKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxSkipDestination.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.NoProofKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AValue: Boolean;
begin
  AValue := not AHasParameter or (AParameterValue <> 0);
  AImporter.Position.CharacterFormatting.NoProof := AValue;
end;

class procedure TdxDestinationPieceTable.NoSuperAndSubScriptKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.CharacterFormatting.Script := TdxCharacterFormattingScript.Normal;
end;

class procedure TdxDestinationPieceTable.LanguageKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  LanguageNpKeywordHandler(AImporter, AParameterValue, AHasParameter);
  AImporter.Position.CharacterFormatting.NoProof := False;
end;

class procedure TdxDestinationPieceTable.LanguageNpKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
end;

class procedure TdxDestinationPieceTable.OldParagraphNumberingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxDestinationOldParagraphNumbering.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.OldParagraphNumberingTextAfterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxTextAfterDestination.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.OldParagraphNumberingTextBeforeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxTextBeforeDestination.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.ParagraphStyleIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.StyleIndex := AImporter.GetParagraphStyleIndex(AParameterValue);
end;

class procedure TdxDestinationPieceTable.TableStyleIndexForRowOrCellHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.RtfTableStyleIndexForRowOrCell := AParameterValue;
end;

class procedure TdxDestinationPieceTable.ParKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.OnEndParagraph;
  if AImporter.DocumentModel.DocumentCapabilities.ParagraphsAllowed then
    AImporter.InsertParagraph
  else
    InsertSpace(AImporter);
end;

class procedure TdxDestinationPieceTable.PictureKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxPictureDestination.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.PlainKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADefaultFormatting: TdxCharacterFormattingInfo;
  ACurrentPositionFormatting: TdxCharacterFormattingBase;
begin
  ADefaultFormatting := AImporter.DocumentModel.Cache.CharacterFormattingInfoCache.DefaultItem;
  ACurrentPositionFormatting := AImporter.Position.CharacterFormatting;
  ACurrentPositionFormatting.FontName := AImporter.DocumentProperties.Fonts.GetRtfFontInfoById(AImporter.DocumentProperties.DefaultFontNumber).Name;
  AImporter.Position.ResetUseAssociatedProperties;
  ACurrentPositionFormatting.DoubleFontSize := 24;
  ACurrentPositionFormatting.FontBold := ADefaultFormatting.FontBold;
  ACurrentPositionFormatting.FontItalic := ADefaultFormatting.FontItalic;
  ACurrentPositionFormatting.FontStrikeoutType := ADefaultFormatting.FontStrikeoutType;
  ACurrentPositionFormatting.FontUnderlineType := ADefaultFormatting.FontUnderlineType;
  ACurrentPositionFormatting.AllCaps := ADefaultFormatting.AllCaps;
  ACurrentPositionFormatting.Hidden := ADefaultFormatting.Hidden;
  ACurrentPositionFormatting.UnderlineWordsOnly := ADefaultFormatting.UnderlineWordsOnly;
  ACurrentPositionFormatting.StrikeoutWordsOnly := ADefaultFormatting.StrikeoutWordsOnly;
  ACurrentPositionFormatting.ForeColor := ADefaultFormatting.ForeColor;
  ACurrentPositionFormatting.BackColor := ADefaultFormatting.BackColor;
  ACurrentPositionFormatting.StrikeoutColor := ADefaultFormatting.StrikeoutColor;
  ACurrentPositionFormatting.UnderlineColor := ADefaultFormatting.UnderlineColor;
  ACurrentPositionFormatting.Script := ADefaultFormatting.Script;
  AImporter.SetCodePage(AImporter.DocumentProperties.DefaultCodePage);
end;

class procedure TdxDestinationPieceTable.QmSpaceKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.ParseCharWithoutDecoding(TdxCharacters.QmSpace);
end;

class procedure TdxDestinationPieceTable.RangePermissionEndKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxRangePermissionEndDestination.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.RangePermissionStartKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxRangePermissionStartDestination.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.ResetParagraphPropertiesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AInfo: TdxRtfParagraphFormattingInfo;
  AOldTableStyleIndex: Integer;
begin
  if ShouldApplyParagraphStyle(AImporter) then
  begin
    AOldTableStyleIndex := AImporter.Position.ParagraphFormattingInfo.RtfTableStyleIndexForRowOrCell;
    AInfo := TdxRtfParagraphFormattingInfo.Create;
    try
      AImporter.Position.ParagraphFormattingInfo := AInfo;
    finally
      AInfo.Free;
    end;
    AImporter.Position.ParagraphFormattingInfo.RtfTableStyleIndexForRowOrCell := AOldTableStyleIndex;
  end
  else
  begin
    AImporter.Position.ParagraphFormattingInfo.NumberingListIndex := NumberingListIndexListIndexNotSetted;
    AImporter.Position.ParagraphFormattingInfo.ListLevelIndex := 0;
    AImporter.Position.ParagraphFormattingInfo.FirstLineIndent := 0;
    AImporter.Position.ParagraphFormattingInfo.FirstLineIndentType := TdxParagraphFirstLineIndent.None;
    AImporter.Position.ParagraphFormattingInfo.LeftIndent := 0;
  end;
  AImporter.Position.ParagraphFrameFormattingInfo := nil;
end;

class procedure TdxDestinationPieceTable.RightDoubleQuoteKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.ParseCharWithoutDecoding(TdxCharacters.RightDoubleQuote);
end;

class procedure TdxDestinationPieceTable.ParCharHandler(AImporter: TdxRtfImporter; var AChar: Char);
begin
  AImporter.FlushDecoder;
  AImporter.TableReader.OnEndParagraph;
  AImporter.InsertParagraph;
end;

class procedure TdxDestinationPieceTable.RightIndentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.RightIndent := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDestinationPieceTable.RightSingleQuoteKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.ParseCharWithoutDecoding(TdxCharacters.RightSingleQuote);
end;

class procedure TdxDestinationPieceTable.SectionLevelNumberingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxDestinationOldSectionNumberingLevel.Create(AImporter, AParameterValue);
end;

class procedure TdxDestinationPieceTable.ShapeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxShapeDestination.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.ShapePictureKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
end;

class procedure TdxDestinationPieceTable.StrikeoutKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
const
  AValueMap: array[Boolean] of TdxStrikeoutType = (TdxStrikeoutType.None, TdxStrikeoutType.Single);
begin
  AImporter.Position.CharacterFormatting.FontStrikeoutType := AValueMap[not AHasParameter or (AParameterValue > 0)];
end;

class procedure TdxDestinationPieceTable.SpacingAfterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.SpacingAfter := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDestinationPieceTable.SpacingBeforeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.SpacingBefore := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDestinationPieceTable.SubscriptKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
const
  AValueMap: array[Boolean] of TdxCharacterFormattingScript = (TdxCharacterFormattingScript.Normal, TdxCharacterFormattingScript.Subscript);
begin
  AImporter.Position.CharacterFormatting.Script := AValueMap[not AHasParameter or (AParameterValue <> 0)];
end;

class procedure TdxDestinationPieceTable.SuperscriptKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
const
  AValueMap: array[Boolean] of TdxCharacterFormattingScript = (TdxCharacterFormattingScript.Normal, TdxCharacterFormattingScript.Superscript);
begin
  AImporter.Position.CharacterFormatting.Script := AValueMap[not AHasParameter or (AParameterValue <> 0)];
end;

class procedure TdxDestinationPieceTable.TabCenterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.TabAlignment := TdxTabAlignmentType.Center;
end;

class procedure TdxDestinationPieceTable.TabDecimalKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.TabAlignment := TdxTabAlignmentType.Decimal;
end;

class procedure TdxDestinationPieceTable.TabKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AImporter.DocumentModel.DocumentCapabilities.TabSymbolAllowed then
    AImporter.ParseCharWithoutDecoding(TdxCharacters.TabMark)
  else
	  AImporter.ParseCharWithoutDecoding(' ');
end;

class procedure TdxDestinationPieceTable.TabLeaderDotsKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.TabLeader := TdxTabLeaderType.Dots;
end;

class procedure TdxDestinationPieceTable.TabLeaderEqualSignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.TabLeader := TdxTabLeaderType.EqualSign;
end;

class procedure TdxDestinationPieceTable.TabLeaderHyphensKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.TabLeader := TdxTabLeaderType.Hyphens;
end;

class procedure TdxDestinationPieceTable.TabLeaderMiddleDotsKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.TabLeader := TdxTabLeaderType.MiddleDots;
end;

class procedure TdxDestinationPieceTable.TabLeaderThickLineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.TabLeader := TdxTabLeaderType.ThickLine;
end;

class procedure TdxDestinationPieceTable.TabLeaderUnderlineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.TabLeader := TdxTabLeaderType.Underline;
end;

class procedure TdxDestinationPieceTable.TabPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AInfo: TdxRtfParagraphFormattingInfo;
  ATab: TdxTabInfo;
begin
  AInfo := AImporter.Position.ParagraphFormattingInfo;
  if AHasParameter then
  begin
    ATab := TdxTabInfo.Create(AImporter.UnitConverter.TwipsToModelUnits(AParameterValue), AInfo.TabAlignment, AInfo.TabLeader, False, False);
    AInfo.Tabs.Add(ATab);
  end;
  AInfo.TabAlignment := TdxTabAlignmentType.Left;
  AInfo.TabLeader := TdxTabLeaderType.None;
end;

class procedure TdxDestinationPieceTable.TabRightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.TabAlignment := TdxTabAlignmentType.Right;
end;

class procedure TdxDestinationPieceTable.UnderlineColorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AProperties: TdxRtfDocumentProperties;
begin
  if not AHasParameter then
    AParameterValue := 0;
  AProperties := AImporter.DocumentProperties;
  AImporter.Position.CharacterFormatting.UnderlineColor := AProperties.Colors.GetRtfColorById(AParameterValue);
end;

class procedure TdxDestinationPieceTable.UnderlineDashDotDottedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.DashDotDotted);
end;

class procedure TdxDestinationPieceTable.UnderlineDashDottedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.DashDotted);
end;

class procedure TdxDestinationPieceTable.UnderlineDashedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.Dashed);
end;

class procedure TdxDestinationPieceTable.UnderlineDottedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.Dotted);
end;

class procedure TdxDestinationPieceTable.UnderlineDoubleKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.Double);
end;

class procedure TdxDestinationPieceTable.UnderlineDoubleWaveKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.DoubleWave);
end;

class procedure TdxDestinationPieceTable.UnderlineHeavyWaveKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.HeavyWave);
end;

class procedure TdxDestinationPieceTable.UnderlineKeywordHandleCore(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean; AType: TdxUnderlineType);
begin
  if AHasParameter and (AParameterValue = 0) then
    AType := TdxUnderlineType.None;
  AImporter.Position.CharacterFormatting.FontUnderlineType := AType;
end;

class procedure TdxDestinationPieceTable.UnderlineLongDashedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.LongDashed);
end;

class procedure TdxDestinationPieceTable.UnderlineNoneKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.None);
end;

class procedure TdxDestinationPieceTable.UnderlineSingleKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.Single);
end;

class procedure TdxDestinationPieceTable.UnderlineThickDashDotDottedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.ThickDashDotDotted);
end;

class procedure TdxDestinationPieceTable.UnderlineThickDashDottedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.ThickDashDotted);
end;

class procedure TdxDestinationPieceTable.UnderlineThickDashedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.ThickDashed);
end;

class procedure TdxDestinationPieceTable.UnderlineThickDottedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.ThickDotted);
end;

class procedure TdxDestinationPieceTable.UnderlineThickLongDashedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.ThickLongDashed);
end;

class procedure TdxDestinationPieceTable.UnderlineThickSingleKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.ThickSingle);
end;

class procedure TdxDestinationPieceTable.UnderlineWaveKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  UnderlineKeywordHandleCore(AImporter, AParameterValue, AHasParameter, TdxUnderlineType.Wave);
end;

class procedure TdxDestinationPieceTable.UnderlineWordsOnlyKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AValue: Boolean;
begin
  if AHasParameter then
    AValue := AParameterValue <> 0
  else
     AValue := True;
  if AValue then
    AImporter.Position.CharacterFormatting.FontUnderlineType := TdxUnderlineType.Single
  else
    AImporter.Position.CharacterFormatting.FontUnderlineType := TdxUnderlineType.None;
  AImporter.Position.CharacterFormatting.UnderlineWordsOnly := AValue;
end;

class procedure TdxDestinationPieceTable.UnicodeCountKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.RtfFormattingInfo.UnicodeCharacterByteCount := AParameterValue;
end;

class procedure TdxDestinationPieceTable.WidowOrphanControlOffKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.WidowOrphanControl := False;
end;

class procedure TdxDestinationPieceTable.OutlineLevelKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ALevel: Integer;
begin
  if not ShouldApplyParagraphStyle(AImporter) then
    Exit;

  if not AHasParameter then
    ALevel := 0
  else
  begin
    ALevel := AParameterValue;
    if (ALevel < 0) or (ALevel > 8) then
      ALevel := 0
    else
      Inc(ALevel);
  end;
  AImporter.Position.ParagraphFormattingInfo.OutlineLevel := ALevel;
end;

class procedure TdxDestinationPieceTable.ParagraphBackgroundKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AProperties: TdxRtfDocumentProperties;
  AColor: TdxAlphaColor;
begin
  if not ShouldApplyParagraphStyle(AImporter) then
    Exit;

  AProperties := AImporter.DocumentProperties;
  AColor := AProperties.Colors.GetRtfColorById(AParameterValue);
  AImporter.Position.ParagraphFormattingInfo.BackColor := AColor;
end;

class procedure TdxDestinationPieceTable.WidowOrphanControlOnKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) then
    AImporter.Position.ParagraphFormattingInfo.WidowOrphanControl := True;
end;

class procedure TdxDestinationPieceTable.InTableParagraphKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.InTableParagraph := True;
end;

class procedure TdxDestinationPieceTable.RowKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.OnEndRow;
end;

class procedure TdxDestinationPieceTable.CellKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.OnEndCell;
  AImporter.InsertParagraph;
end;

class procedure TdxDestinationPieceTable.NestedCellKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.OnEndNestedCell;
  AImporter.InsertParagraph;
end;

class procedure TdxDestinationPieceTable.NestedRowKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.OnEndNestedRow;
end;

class procedure TdxDestinationPieceTable.NestedTablePropertiesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.OnStartNestedTableProperties;
end;

class procedure TdxDestinationPieceTable.NoNestedTablesKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxSkipNestedTableDestination.Create(AImporter);
end;

class procedure TdxDestinationPieceTable.ItapKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter or (AParameterValue < 0) then
    Exit;
  AImporter.Position.ParagraphFormattingInfo.NestingLevel := AParameterValue;
end;

class procedure TdxDestinationPieceTable.TableRowDefaultsKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.OnTableRowDefaults;
end;

class procedure TdxDestinationPieceTable.TableStyleKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AImporter.DocumentModel.DocumentCapabilities.TableStyleAllowed then
    AImporter.TableReader.TableProperties.TableStyleIndex := AImporter.GetTableStyleIndex(AParameterValue);
end;

class procedure TdxDestinationPieceTable.CellxKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.OnCellxProperty(AParameterValue);
end;

class procedure TdxDestinationPieceTable.CellPreferredWidthKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.CellProperties.PreferredWidth.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.WidthUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AImporter.TableReader.CellProperties.PreferredWidth.&Type := GetWidthUnitType(AParameterValue);
end;

class procedure TdxDestinationPieceTable.FirstHorizontalMergedCellKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AImporter.TableReader.CellProperties.HorizontalMerging = TdxMergingState.None then
    AImporter.TableReader.CellProperties.HorizontalMerging := TdxMergingState.Restart;
end;

class procedure TdxDestinationPieceTable.NextHorizontalMergedCellKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.HorizontalMerging := TdxMergingState.Continue;
end;

class procedure TdxDestinationPieceTable.FirstVerticalMergedCellKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.VerticalMerging := TdxMergingState.Restart;
end;

class procedure TdxDestinationPieceTable.NextVerticalMergedCellKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.VerticalMerging := TdxMergingState.Continue;
end;

class procedure TdxDestinationPieceTable.CellFitTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter or (AParameterValue <> 0) then
    AImporter.TableReader.CellProperties.FitText := True;
end;

class procedure TdxDestinationPieceTable.CellNoWrapKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter or (AParameterValue <> 0)then
    AImporter.TableReader.CellProperties.NoWrap := True;
end;

class procedure TdxDestinationPieceTable.CellVerticalAlignmentTopKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.VerticalAlignment := TdxVerticalAlignment.Top;
end;

class procedure TdxDestinationPieceTable.CellVerticalAlignmentCenterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.VerticalAlignment := TdxVerticalAlignment.Center;
end;

class procedure TdxDestinationPieceTable.CellVerticalAlignmentBottomKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.VerticalAlignment := TdxVerticalAlignment.Bottom;
end;

class procedure TdxDestinationPieceTable.CellHideMarkKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter or (AParameterValue <> 0) then
    AImporter.TableReader.CellProperties.HideCellMark := True;
end;

class procedure TdxDestinationPieceTable.CellBottomCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue >= 0) and AHasParameter then
    AImporter.TableReader.CellProperties.CellMargins.Bottom.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.CellLeftCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue >= 0) and AHasParameter then
    AImporter.TableReader.CellProperties.CellMargins.Top.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.CellRightCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.CellProperties.CellMargins.Right.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.CellTopCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.CellProperties.CellMargins.Left.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.CellBottomCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AssignWidthUnitInfo(AImporter.TableReader.CellProperties.CellMargins.Bottom, AParameterValue);
end;

class procedure TdxDestinationPieceTable.CellLeftCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
    AssignWidthUnitInfo(AImporter.TableReader.CellProperties.CellMargins.Top, AParameterValue);
end;

class procedure TdxDestinationPieceTable.CellRightCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AssignWidthUnitInfo(AImporter.TableReader.CellProperties.CellMargins.Right, AParameterValue);
end;

class procedure TdxDestinationPieceTable.CellTopCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AssignWidthUnitInfo(AImporter.TableReader.CellProperties.CellMargins.Left, AParameterValue);
end;

class procedure TdxDestinationPieceTable.CellTextTopAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.VerticalAlignment := TdxVerticalAlignment.Top;
end;

class procedure TdxDestinationPieceTable.CellTextCenterVerticalAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.VerticalAlignment := TdxVerticalAlignment.Center;
end;

class procedure TdxDestinationPieceTable.CellTextBottomAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.VerticalAlignment := TdxVerticalAlignment.Bottom;
end;

class procedure TdxDestinationPieceTable.CellLeftToRightTopToBottomTextDirectionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.TextDirection := TdxTextDirection.LeftToRightTopToBottom;
end;

class procedure TdxDestinationPieceTable.CellTopToBottomRightToLeftTextDirectionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.TextDirection := TdxTextDirection.TopToBottomRightToLeft;
end;

class procedure TdxDestinationPieceTable.CellBottomToTopLeftToRightTextDirectionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.TextDirection := TdxTextDirection.BottomToTopLeftToRight;
end;

class procedure TdxDestinationPieceTable.CellLeftToRightTopToBottomVerticalTextDirectionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.TextDirection := TdxTextDirection.LeftToRightTopToBottomRotated;
end;

class procedure TdxDestinationPieceTable.CellTopToBottomRightToLeftVerticalTextDirectionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.CellProperties.TextDirection := TdxTextDirection.TopToBottomRightToLeftRotated;
end;

class procedure TdxDestinationPieceTable.CellBackgroundColorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AColorTable: TdxRtfColorCollection;
begin
  AColorTable := AImporter.DocumentProperties.Colors;
  if not AHasParameter or (AParameterValue > AColorTable.Count - 1) then
    Exit;
  AImporter.TableReader.CellProperties.BackgroundColor := AColorTable.GetRtfColorById(AParameterValue);
end;

class procedure TdxDestinationPieceTable.CellForegroundColorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AColorTable: TdxRtfColorCollection;
begin
  AColorTable := AImporter.DocumentProperties.Colors;
  if not AHasParameter or (AParameterValue > AColorTable.Count - 1) then
    Exit;
  AImporter.TableReader.CellProperties.ForegroundColor := AColorTable.GetRtfColorById(AParameterValue);
end;

class procedure TdxDestinationPieceTable.CellShadingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AImporter.TableReader.CellProperties.Shading := GetShadingPattern(AParameterValue);
end;

class procedure TdxDestinationPieceTable.NoTableBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AImporter.TableReader.ProcessedBorder = nil then
    Exit;
  AImporter.TableReader.ProcessedBorder.Style := TdxBorderLineStyle.None;
  AImporter.TableReader.ProcessedBorder := nil;
end;

class procedure TdxDestinationPieceTable.BottomCellBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.ProcessedBorder := AImporter.TableReader.CellProperties.Borders.BottomBorder;
end;

class procedure TdxDestinationPieceTable.TopCellBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.ProcessedBorder := AImporter.TableReader.CellProperties.Borders.TopBorder;
end;

class procedure TdxDestinationPieceTable.LeftCellBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.ProcessedBorder := AImporter.TableReader.CellProperties.Borders.LeftBorder;
end;

class procedure TdxDestinationPieceTable.RightCellBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.ProcessedBorder := AImporter.TableReader.CellProperties.Borders.RightBorder;
end;

class procedure TdxDestinationPieceTable.UpperLeftToLowerRightBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.ProcessedBorder := AImporter.TableReader.CellProperties.Borders.TopLeftDiagonalBorder;
end;

class procedure TdxDestinationPieceTable.UpperRightToLowerLeftBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.ProcessedBorder := AImporter.TableReader.CellProperties.Borders.TopRightDiagonalBorder;
end;

class procedure TdxDestinationPieceTable.RowLeftKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.Left := AParameterValue;
end;

class procedure TdxDestinationPieceTable.RowHeaderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter or (AParameterValue <> 0) then
    AImporter.TableReader.RowProperties.Header := True;
end;

class procedure TdxDestinationPieceTable.RowHeightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AVal: Integer;
  ARowProperties: TdxRtfTableRowProperties;
begin
  if not AHasParameter then
    Exit;
  AVal := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);

  ARowProperties := AImporter.TableReader.RowProperties;
  ARowProperties.Height.Value := Abs(AVal);
  if AParameterValue < 0 then
    ARowProperties.Height.&Type := TdxHeightUnitType.Exact
  else
    if AParameterValue > 0 then
      ARowProperties.Height.&Type := TdxHeightUnitType.Minimum
    else
      ARowProperties.Height.&Type := TdxHeightUnitType.Auto;
end;

class procedure TdxDestinationPieceTable.RowKeepKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter or (AParameterValue <> 0) then
    AImporter.TableReader.RowProperties.CantSplit := True;
end;

class procedure TdxDestinationPieceTable.TableRightAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.TableRowAlignment := TdxTableRowAlignment.Right;
end;

class procedure TdxDestinationPieceTable.TableLeftAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.TableRowAlignment := TdxTableRowAlignment.Left;
end;

class procedure TdxDestinationPieceTable.TableCenterAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.TableRowAlignment := TdxTableRowAlignment.Center;
end;

class procedure TdxDestinationPieceTable.WidthBeforeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.RowProperties.WidthBefore.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.WidthBeforeUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.WidthBefore.&Type := GetWidthUnitType(AParameterValue);
end;

class procedure TdxDestinationPieceTable.WidthAfterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.RowProperties.WidthAfter.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.WidthAfterUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.WidthAfter.&Type := GetWidthUnitType(AParameterValue);
end;

class procedure TdxDestinationPieceTable.SpaceBetweenCellsKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;

  AImporter.TableReader.TableProperties.HalfSpace := AParameterValue;
end;

class procedure TdxDestinationPieceTable.TableBottomCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.TableProperties.CellMargins.Bottom.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.TableLeftCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.TableProperties.CellMargins.Left.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.TableRightCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.TableProperties.CellMargins.Right.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.TableTopCellMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.TableProperties.CellMargins.Top.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.TableBottomCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AssignWidthUnitInfo(AImporter.TableReader.TableProperties.CellMargins.Bottom, AParameterValue);
end;

class procedure TdxDestinationPieceTable.TableLeftCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AssignWidthUnitInfo(AImporter.TableReader.TableProperties.CellMargins.Left, AParameterValue);
end;

class procedure TdxDestinationPieceTable.TableRightCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AssignWidthUnitInfo(AImporter.TableReader.TableProperties.CellMargins.Right, AParameterValue);
end;

class procedure TdxDestinationPieceTable.TableTopCellMarginUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AssignWidthUnitInfo(AImporter.TableReader.TableProperties.CellMargins.Top, AParameterValue);
end;

class procedure TdxDestinationPieceTable.TableBottomCellSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.TableProperties.CellSpacing.Bottom.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.TableLeftCellSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.TableProperties.CellSpacing.Left.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.TableRightCellSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.TableProperties.CellSpacing.Right.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.TableTopCellSpacingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.TableProperties.CellSpacing.Top.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.TableBottomCellSpacingUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AssignWidthUnitInfo(AImporter.TableReader.TableProperties.CellSpacing.Bottom, AParameterValue);
end;

class procedure TdxDestinationPieceTable.TableLeftCellSpacingUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AssignWidthUnitInfo(AImporter.TableReader.TableProperties.CellSpacing.Left, AParameterValue);
end;

class procedure TdxDestinationPieceTable.TableRightCellSpacingUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AssignWidthUnitInfo(AImporter.TableReader.TableProperties.CellSpacing.Right, AParameterValue);
end;

class procedure TdxDestinationPieceTable.TableTopCellSpacingUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AssignWidthUnitInfo(AImporter.TableReader.TableProperties.CellSpacing.Top, AParameterValue);
end;

class procedure TdxDestinationPieceTable.TablePreferredWidthKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.TableProperties.PreferredWidth.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.TablePreferredWidthUnitTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.TableProperties.PreferredWidth.&Type := GetWidthUnitType(AParameterValue);
end;

class procedure TdxDestinationPieceTable.TableIndentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AImporter.TableReader.TableProperties.TableIndent.Value := AParameterValue;
end;

class procedure TdxDestinationPieceTable.TableIndentUnitTypeHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AImporter.TableReader.TableProperties.TableIndent.&Type := GetWidthUnitType(AParameterValue);
end;

class procedure TdxDestinationPieceTable.TableOverlapKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter or (AParameterValue <> 0) then
    AImporter.TableReader.TableProperties.IsTableOverlap := False;
end;

class procedure TdxDestinationPieceTable.TableLeftFromTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AImporter.TableReader.RowProperties.FloatingPosition.LeftFromText := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDestinationPieceTable.TableRightFromTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AImporter.TableReader.RowProperties.FloatingPosition.RightFromText := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDestinationPieceTable.TableTopFromTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AImporter.TableReader.RowProperties.FloatingPosition.TopFromText := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDestinationPieceTable.TableBottomFromTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AImporter.TableReader.RowProperties.FloatingPosition.BottomFromText := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDestinationPieceTable.ColHorizontalAnchorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.HorizontalAnchor := TdxHorizontalAnchorTypes.Column;
end;

class procedure TdxDestinationPieceTable.MarginHorizontalAnchorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.TextWrapping := TdxTextWrapping.Around;
  AImporter.TableReader.RowProperties.FloatingPosition.HorizontalAnchor := TdxHorizontalAnchorTypes.Margin;
end;

class procedure TdxDestinationPieceTable.PageHorizontalAnchorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.TextWrapping := TdxTextWrapping.Around;
  AImporter.TableReader.RowProperties.FloatingPosition.HorizontalAnchor := TdxHorizontalAnchorTypes.Page;
end;

class procedure TdxDestinationPieceTable.MarginVerticalAnchorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.VerticalAnchor := TdxVerticalAnchorTypes.Margin;
end;

class procedure TdxDestinationPieceTable.ParagraphVerticalAnchorKewordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.TextWrapping := TdxTextWrapping.Around;
  AImporter.TableReader.RowProperties.FloatingPosition.VerticalAnchor := TdxVerticalAnchorTypes.Paragraph;
end;

class procedure TdxDestinationPieceTable.PageVerticalAnchorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.TextWrapping := TdxTextWrapping.Around;
  AImporter.TableReader.RowProperties.FloatingPosition.VerticalAnchor := TdxVerticalAnchorTypes.Page;
end;

class procedure TdxDestinationPieceTable.TableHorizontalPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  AImporter.TableReader.RowProperties.FloatingPosition.TableHorizontalPosition :=
    AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
  if AParameterValue <> 0 then
    AImporter.TableReader.RowProperties.FloatingPosition.TextWrapping := TdxTextWrapping.Around;
end;

class procedure TdxDestinationPieceTable.TableVerticalPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    Exit;
  if AParameterValue <> 0 then
    AImporter.TableReader.RowProperties.FloatingPosition.TextWrapping := TdxTextWrapping.Around;
  AImporter.TableReader.RowProperties.FloatingPosition.TableVerticalPosition :=
    AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDestinationPieceTable.CenterTableHorizontalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.HorizontalAlign := TdxHorizontalAlignMode.Center;
end;

class procedure TdxDestinationPieceTable.InsideTableHorizontalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.HorizontalAlign := TdxHorizontalAlignMode.Inside;
end;

class procedure TdxDestinationPieceTable.LeftTableHorizontalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.HorizontalAlign := TdxHorizontalAlignMode.Left;
end;

class procedure TdxDestinationPieceTable.OutsideTableHorizontalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.HorizontalAlign := TdxHorizontalAlignMode.Outside;
end;

class procedure TdxDestinationPieceTable.RightTableHorizontalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.HorizontalAlign := TdxHorizontalAlignMode.Right;
end;

class procedure TdxDestinationPieceTable.BottomTableVerticalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.VerticalAlign := TdxVerticalAlignMode.Bottom;
end;

class procedure TdxDestinationPieceTable.CenterTableVerticalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.VerticalAlign := TdxVerticalAlignMode.Center;
end;

class procedure TdxDestinationPieceTable.InlineTableVerticalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.VerticalAlign := TdxVerticalAlignMode.&Inline;
end;

class procedure TdxDestinationPieceTable.InsideTableVerticalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.VerticalAlign := TdxVerticalAlignMode.Inside;
end;

class procedure TdxDestinationPieceTable.OutsideTableVerticalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.VerticalAlign := TdxVerticalAlignMode.Outside;
end;

class procedure TdxDestinationPieceTable.TopTableVerticalAlignKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.RowProperties.FloatingPosition.VerticalAlign := TdxVerticalAlignMode.Top;
end;

class procedure TdxDestinationPieceTable.TableAutoFitKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AValue: Integer;
begin
  AValue := Abs(AParameterValue);
  if AValue = 1 then
    AImporter.TableReader.TableProperties.TableLayout := TdxTableLayoutType.Autofit
  else
    if AValue < 1 then
      AImporter.TableReader.TableProperties.TableLayout := TdxTableLayoutType.Fixed;
end;

class procedure TdxDestinationPieceTable.RowBandSizeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.TableProperties.TableStyleRowBandSize := AParameterValue;
end;

class procedure TdxDestinationPieceTable.ColumnBandSizeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if (AParameterValue < 0) or not AHasParameter then
    Exit;
  AImporter.TableReader.TableProperties.TableStyleColBandSize := AParameterValue;
end;

class procedure TdxDestinationPieceTable.TopTableBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.ProcessedBorder := AImporter.TableReader.TableProperties.Borders.TopBorder;
end;

class procedure TdxDestinationPieceTable.LeftTableBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.ProcessedBorder := AImporter.TableReader.TableProperties.Borders.LeftBorder;
end;

class procedure TdxDestinationPieceTable.BottomTableBorderKewordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.ProcessedBorder := AImporter.TableReader.TableProperties.Borders.BottomBorder;
end;

class procedure TdxDestinationPieceTable.RightTableBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.ProcessedBorder := AImporter.TableReader.TableProperties.Borders.RightBorder;
end;

class procedure TdxDestinationPieceTable.HorizontalTableBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.ProcessedBorder := AImporter.TableReader.TableProperties.Borders.InsideHorizontalBorder;
end;

class procedure TdxDestinationPieceTable.VerticalTableBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.ProcessedBorder := AImporter.TableReader.TableProperties.Borders.InsideVerticalBorder;
end;

class procedure TdxDestinationPieceTable.ApplyFirstRowConditionalFormattingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.TableProperties.TableLook := AImporter.TableReader.TableProperties.TableLook + [TdxTableLookType.ApplyFirstRow];
end;

class procedure TdxDestinationPieceTable.ApplyLastRowConditionalFormattingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.TableProperties.TableLook := AImporter.TableReader.TableProperties.TableLook + [TdxTableLookType.ApplyLastRow];
end;

class procedure TdxDestinationPieceTable.ApplyFirstColumnContitionalFormattingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.TableProperties.TableLook := AImporter.TableReader.TableProperties.TableLook + [TdxTableLookType.ApplyFirstColumn];
end;

class procedure TdxDestinationPieceTable.ApplyLastColumnConditionalFormattingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.TableProperties.TableLook := AImporter.TableReader.TableProperties.TableLook + [TdxTableLookType.ApplyLastColumn];
end;

class procedure TdxDestinationPieceTable.DoNotApplyRowBandingConditionalFormattingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.TableProperties.TableLook := AImporter.TableReader.TableProperties.TableLook + [TdxTableLookType.DoNotApplyRowBanding];
end;

class procedure TdxDestinationPieceTable.DoNotApplyColumnBandingConditionalFormattingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.TableReader.TableProperties.TableLook := AImporter.TableReader.TableProperties.TableLook + [TdxTableLookType.DoNotApplyColumnBanding];
end;

class procedure TdxDestinationPieceTable.SetNoBorderType(AImporter: TdxRtfImporter; AStyle: TdxBorderLineStyle);
var
  ABorder: TdxBorderBase;
  AParagraphBorder: TdxBorderInfo;
begin
  ABorder := AImporter.TableReader.ProcessedBorder;
  if ABorder <> nil then
    ABorder.Style := AStyle;
  AImporter.TableReader.ProcessedBorder := nil;
  AParagraphBorder := AImporter.Position.ParagraphFormattingInfo.ProcessedBorder;
  if AParagraphBorder <> nil then
    AParagraphBorder.Style := AStyle;
  AImporter.Position.ParagraphFormattingInfo.ProcessedBorder := nil;
end;

class procedure TdxDestinationPieceTable.NoBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetNoBorderType(AImporter, TdxBorderLineStyle.Nil);
end;

class procedure TdxDestinationPieceTable.BorderWidthKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
const
  AMaxBorderWidth = 255;
var
  ABorder: TdxBorderBase;
  AParagraphBorder: TdxBorderInfo;
  AValue: Integer;
begin
  ABorder := AImporter.TableReader.ProcessedBorder;
  AParagraphBorder := AImporter.Position.ParagraphFormattingInfo.ProcessedBorder;
  if (ABorder = nil) and (AParagraphBorder = nil) then
    Exit;
  if AParameterValue > 0 then
    AValue := IfThen(AParameterValue > AMaxBorderWidth, AMaxBorderWidth, AParameterValue)
  else
    AValue := 0;
  AValue := AImporter.UnitConverter.TwipsToModelUnits(AValue);
  AValue := IfThen(AValue > 0, AValue, 1);
  if ABorder <> nil then
    ABorder.Width := AValue;
  if AParagraphBorder <> nil then
    AParagraphBorder.Width := AValue;
end;

class procedure TdxDestinationPieceTable.BorderColorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ABorder: TdxBorderBase;
  AParagraphBorder: TdxBorderInfo;
  AColorTable: TdxRtfColorCollection;
begin
  if not AHasParameter or (AParameterValue < 0) then
    Exit;
  ABorder := AImporter.TableReader.ProcessedBorder;
  AParagraphBorder := AImporter.Position.ParagraphFormattingInfo.ProcessedBorder;
  if (ABorder = nil) and (AParagraphBorder = nil) then
    Exit;
  AColorTable := AImporter.DocumentProperties.Colors;
  if AParameterValue > AColorTable.Count - 1 then
    Exit;
  if ABorder <> nil then
    ABorder.Color := AColorTable.GetRtfColorById(AParameterValue);
  if AParagraphBorder <> nil then
    AParagraphBorder.Color := AColorTable.GetRtfColorById(AParameterValue);
end;

class procedure TdxDestinationPieceTable.FrameBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ABorder: TdxBorderBase;
  AParagraphBorder: TdxBorderInfo;
begin
  ABorder := AImporter.TableReader.ProcessedBorder;
  AParagraphBorder := AImporter.Position.ParagraphFormattingInfo.ProcessedBorder;
  if (ABorder = nil) and (AParagraphBorder = nil) then
    Exit;
  if not AHasParameter or (AParameterValue <> 0) then
  begin
    if ABorder <> nil then
      ABorder.Frame := True;
    if AParagraphBorder <> nil then
      AParagraphBorder.Frame := True;
  end;
end;

class procedure TdxDestinationPieceTable.BorderSpaceKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ABorder: TdxBorderBase;
  AParagraphBorder: TdxBorderInfo;
begin
  if not AHasParameter or (AParameterValue < 0) then
    Exit;
  ABorder := AImporter.TableReader.ProcessedBorder;
  if ABorder <> nil then
    ABorder.Offset := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
  AParagraphBorder := AImporter.Position.ParagraphFormattingInfo.ProcessedBorder;
  if AParagraphBorder <> nil then
    AParagraphBorder.Offset := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
end;

class procedure TdxDestinationPieceTable.SingleThicknessBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.Single);
end;

class procedure TdxDestinationPieceTable.DoubleThicknessBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ABorder: TdxBorderBase;
  AParagraphBorder: TdxBorderInfo;
begin
  ABorder := AImporter.TableReader.ProcessedBorder;
  if ABorder <> nil then
  begin
    AssignDefaultBorderWidth(ABorder);
    ABorder.Width := ABorder.Width * 2;
    ABorder.Style := TdxBorderLineStyle.Single;
  end;
  AParagraphBorder := AImporter.Position.ParagraphFormattingInfo.ProcessedBorder;
  if AParagraphBorder <> nil then
  begin
    AParagraphBorder.Width := AParagraphBorder.Width * 2;
    AParagraphBorder.Style := TdxBorderLineStyle.Single;
  end;
end;

class procedure TdxDestinationPieceTable.ShadowedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ABorder: TdxBorderBase;
  AParagraphBorder: TdxBorderInfo;
begin
  ABorder := AImporter.TableReader.ProcessedBorder;
  AParagraphBorder := AImporter.Position.ParagraphFormattingInfo.ProcessedBorder;
  if (ABorder = nil) and (AParagraphBorder = nil) then
    Exit;
  if not AHasParameter or (AParameterValue <> 0) then
  begin
    if ABorder <> nil then
      ABorder.Shadow := True;
    if AParagraphBorder <> nil then
      AParagraphBorder.Shadow := True;
  end;
end;

class procedure TdxDestinationPieceTable.DoubleBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.Double);
end;

class procedure TdxDestinationPieceTable.DottedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.Dotted);
end;

class procedure TdxDestinationPieceTable.DashedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.Dashed);
end;

class procedure TdxDestinationPieceTable.HairlineBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ABorder: TdxBorderBase;
  AParagraphBorder: TdxBorderInfo;
begin
  ABorder := AImporter.TableReader.ProcessedBorder;
  if ABorder <> nil then
  begin
    ABorder.Style := TdxBorderLineStyle.Single;
    ABorder.Width := 1;
  end;
  AParagraphBorder := AImporter.Position.ParagraphFormattingInfo.ProcessedBorder;
  if AParagraphBorder <> nil then
  begin
    AParagraphBorder.Style := TdxBorderLineStyle.Single;
    AParagraphBorder.Width := 1;
  end;
end;

class procedure TdxDestinationPieceTable.SmallDashedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.DashSmallGap);
end;

class procedure TdxDestinationPieceTable.DotDashedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.DotDash);
end;

class procedure TdxDestinationPieceTable.DotDotDashedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.DotDotDash);
end;

class procedure TdxDestinationPieceTable.InsetBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.Inset);
end;

class procedure TdxDestinationPieceTable.NoneBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.None);
end;

class procedure TdxDestinationPieceTable.OutsetBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.Outset);
end;

class procedure TdxDestinationPieceTable.TripletBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.Triple);
end;

class procedure TdxDestinationPieceTable.SmallThickThinBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.ThickThinSmallGap);
end;

class procedure TdxDestinationPieceTable.SmallThinThickBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.ThinThickSmallGap);
end;

class procedure TdxDestinationPieceTable.SmallThinThickThinBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.ThinThickThinSmallGap);
end;

class procedure TdxDestinationPieceTable.MediumThickThinBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.ThickThinMediumGap);
end;

class procedure TdxDestinationPieceTable.MediumThinThickBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.ThinThickMediumGap);
end;

class procedure TdxDestinationPieceTable.MediumThinThickThinBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.ThinThickThinMediumGap);
end;

class procedure TdxDestinationPieceTable.LargeThickThinBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.ThickThinLargeGap);
end;

class procedure TdxDestinationPieceTable.LargeThinThickBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.ThinThickLargeGap);
end;

class procedure TdxDestinationPieceTable.LargeThinThickThinBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.ThinThickThinLargeGap);
end;

class procedure TdxDestinationPieceTable.WavyBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.Wave);
end;

class procedure TdxDestinationPieceTable.DoubleWavyBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.DoubleWave);
end;

class procedure TdxDestinationPieceTable.StripedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.DashDotStroked);
end;

class procedure TdxDestinationPieceTable.EmbossedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.ThreeDEmboss);
end;

class procedure TdxDestinationPieceTable.EngravedBorderTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxBorderLineStyle.ThreeDEngrave);
end;

class procedure TdxDestinationPieceTable.BorderArtIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  SetBorderType(AImporter, TdxRtfArtBorderConverter.GetBorderLineStyle(AParameterValue));
end;

class procedure TdxDestinationPieceTable.TopParagraphBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.TopBorder := TdxBorderInfo.Empty;
  AImporter.Position.ParagraphFormattingInfo.ProcessedBorder := AImporter.Position.ParagraphFormattingInfo.TopBorder;
end;

class procedure TdxDestinationPieceTable.BottomParagraphBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.BottomBorder := TdxBorderInfo.Empty;
  AImporter.Position.ParagraphFormattingInfo.ProcessedBorder := AImporter.Position.ParagraphFormattingInfo.BottomBorder;
end;

class procedure TdxDestinationPieceTable.LeftParagraphBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.LeftBorder := TdxBorderInfo.Empty;
  AImporter.Position.ParagraphFormattingInfo.ProcessedBorder := AImporter.Position.ParagraphFormattingInfo.LeftBorder;
end;

class procedure TdxDestinationPieceTable.RightParagraphBorderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.RightBorder := TdxBorderInfo.Empty;
  AImporter.Position.ParagraphFormattingInfo.ProcessedBorder := AImporter.Position.ParagraphFormattingInfo.RightBorder;
end;

class procedure TdxDestinationPieceTable.FrameHorizontalPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
  begin
    AImporter.Position.ParagraphFrameFormattingInfo.HorizontalPosition := AParameterValue;
    AImporter.Position.ParagraphFrameFormattingInfo.X := Abs(AImporter.UnitConverter.TwipsToModelUnits(AParameterValue));
  end;
end;

class procedure TdxDestinationPieceTable.FrameVerticalPositionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
  begin
    AImporter.Position.ParagraphFrameFormattingInfo.VerticalPosition := AParameterValue;
    AImporter.Position.ParagraphFrameFormattingInfo.Y := Abs(AImporter.UnitConverter.TwipsToModelUnits(AParameterValue));
  end;
end;

class procedure TdxDestinationPieceTable.FrameWidthKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
    AImporter.Position.ParagraphFrameFormattingInfo.Width := Abs(AImporter.UnitConverter.TwipsToModelUnits(AParameterValue));
end;

class procedure TdxDestinationPieceTable.FrameHeightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
  begin
    AImporter.Position.ParagraphFrameFormattingInfo.Height := Abs(AImporter.UnitConverter.TwipsToModelUnits(AParameterValue));
    if AParameterValue < 0 then
      AImporter.Position.ParagraphFrameFormattingInfo.HorizontalRule := TdxParagraphFrameHorizontalRule.Exact;
    if AParameterValue > 0 then
      AImporter.Position.ParagraphFrameFormattingInfo.HorizontalRule := TdxParagraphFrameHorizontalRule.AtLeast;
  end;
end;

class procedure TdxDestinationPieceTable.ParagraphHorizontalPositionTypeMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
    AImporter.Position.ParagraphFrameFormattingInfo.HorizontalPositionType := TdxParagraphFrameHorizontalPositionType.Margin;
end;

class procedure TdxDestinationPieceTable.ParagraphHorizontalPositionTypePageKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
    AImporter.Position.ParagraphFrameFormattingInfo.HorizontalPositionType := TdxParagraphFrameHorizontalPositionType.Page;
end;

class procedure TdxDestinationPieceTable.ParagraphHorizontalPositionTypeColumnKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
    AImporter.Position.ParagraphFrameFormattingInfo.HorizontalPositionType := TdxParagraphFrameHorizontalPositionType.Column;
end;

class procedure TdxDestinationPieceTable.ParagraphVerticalPositionTypeMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
    AImporter.Position.ParagraphFrameFormattingInfo.VerticalPositionType := TdxParagraphFrameVerticalPositionType.Margin;
end;

class procedure TdxDestinationPieceTable.ParagraphVerticalPositionTypePageKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
    AImporter.Position.ParagraphFrameFormattingInfo.VerticalPositionType := TdxParagraphFrameVerticalPositionType.Page;
end;

class procedure TdxDestinationPieceTable.ParagraphVerticalPositionTypeLineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
    AImporter.Position.ParagraphFrameFormattingInfo.VerticalPositionType := TdxParagraphFrameVerticalPositionType.Paragraph;
end;

class procedure TdxDestinationPieceTable.FrameNoWrapKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
    AImporter.Position.ParagraphFrameFormattingInfo.TextWrapType := TdxParagraphFrameTextWrapType.NotBeside;
end;

class procedure TdxDestinationPieceTable.FrameWrapDefaultKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
end;

class procedure TdxDestinationPieceTable.FrameWrapOverlayKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
    AImporter.Position.ParagraphFrameFormattingInfo.TextWrapType := TdxParagraphFrameTextWrapType.None;
end;

class procedure TdxDestinationPieceTable.FrameWrapAroundKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
    AImporter.Position.ParagraphFrameFormattingInfo.TextWrapType := TdxParagraphFrameTextWrapType.Around;
end;

class procedure TdxDestinationPieceTable.FrameWrapTightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
    AImporter.Position.ParagraphFrameFormattingInfo.TextWrapType := TdxParagraphFrameTextWrapType.Tight;
end;

class procedure TdxDestinationPieceTable.FrameWrapThroughKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if ShouldApplyParagraphStyle(AImporter) and EnsureFramePropertiesExists(AImporter) then
    AImporter.Position.ParagraphFrameFormattingInfo.TextWrapType := TdxParagraphFrameTextWrapType.Through;
end;

{ TdxStringValueDestination }

procedure TdxStringValueDestination.ProcessCharCore(AChar: Char);
begin
  FValue := FValue + AChar;
end;

function TdxStringValueDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := CreateEmptyClone;
  TdxStringValueDestination(Result).FValue := FValue;
end;

class constructor TdxStringValueDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
  FControlCharHT := CreateControlCharHT;
end;

class destructor TdxStringValueDestination.Finalize;
begin
  FreeAndNil(FControlCharHT);
  FreeAndNil(FKeywordHT);
end;

class function TdxStringValueDestination.CreateControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := TdxControlCharTranslatorTable.Create;
  Result.Add('''', SwitchToHexCharHandler);
  Result.Add('\', EscapedCharHandler);
end;

class function TdxStringValueDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('u', UnicodeKeywordHandler);
end;

class function TdxStringValueDestination.GetControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := FControlCharHT;
end;

class function TdxStringValueDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

function TdxStringValueDestination.GetValue: string;
var
  APos: Integer;
begin
  APos := Pos(';', FValue);
  if APos > 0 then
    Result := Copy(FValue, 1, APos - 1)
  else
    Result := FValue;
end;

{ TdxBookmarkDestinationBase }

procedure TdxBookmarkDestinationBase.AfterPopRtfState;
var
  ABookmarkName: string;
  ABookmark: TdxImportBookmarkInfo;
begin
  ABookmarkName := Trim(Value);
  if ABookmarkName <> '' then
  begin
    if not Importer.Bookmarks.TryGetValue(ABookmarkName, ABookmark) then
    begin
      ABookmark := TdxImportBookmarkInfo.Create;
      ABookmark.Name := ABookmarkName;
      Importer.Bookmarks.Add(ABookmarkName, ABookmark);
    end;
    AssignBookmarkPosition(ABookmark);
  end;
end;

{ TdxBookmarkStartDestination }

procedure TdxBookmarkStartDestination.AssignBookmarkPosition(ABookmark: TdxImportBookmarkInfo);
begin
  ABookmark.Start := Importer.Position.LogPosition;
end;

function TdxBookmarkStartDestination.CreateEmptyClone: TdxStringValueDestination;
begin
  Result := TdxBookmarkStartDestination.Create(Importer);
end;

{ TdxBookmarkEndDestination }

procedure TdxBookmarkEndDestination.AssignBookmarkPosition(ABookmark: TdxImportBookmarkInfo);
begin
  ABookmark.&End := Importer.Position.LogPosition;
end;

function TdxBookmarkEndDestination.CreateEmptyClone: TdxStringValueDestination;
begin
  Result := TdxBookmarkEndDestination.Create(Importer);
end;

{ TdxRangePermissionDestinationBase }

procedure TdxRangePermissionDestinationBase.AfterPopRtfState;
var
  AData: string;
  ARangePermission: TdxImportRangePermissionInfo;
begin
  AData := Trim(Value);
  if Length(AData) = 16 then
  begin
    if not Importer.RangePermissions.TryGetValue(AData, ARangePermission) then
    begin
      ARangePermission := TdxImportRangePermissionInfo.Create;
      ARangePermission.PermissionInfo.UserName := ObtainUserName(AData);
      ARangePermission.PermissionInfo.Group := ObtainGroupName(AData);
      Importer.RangePermissions.Add(AData, ARangePermission);
    end;
    AssignRangePermissionPosition(ARangePermission);
  end;
end;

function TdxRangePermissionDestinationBase.ObtainUserName(const AData: string): string;
var
  AValue: Integer;
begin
  AValue := ObtainUserId(AData);
  Result := Importer.GetUserNameById(AValue);
end;

function TdxRangePermissionDestinationBase.ObtainGroupName(const AData: string): string;
var
  AValue: Integer;
begin
  AValue := ObtainUserId(AData);
  if not TdxRtfContentExporter.PredefinedUserGroups.TryGetValue(AValue, Result) then
    Result := '';
end;

function TdxRangePermissionDestinationBase.ObtainUserId(const AData: string): Integer;
var
  AValueLow, AValueHigh: Integer;
begin
  if not TdxNumber.TryParse(Copy(AData, 1, 2), TdxNumberStyles.HexNumber, AValueLow) then
    Exit(MinInt);
  if not TdxNumber.TryParse(Copy(AData, 3, 2), TdxNumberStyles.HexNumber, AValueHigh) then
    Exit(MinInt);

  Result := (AValueHigh shl 8) or AValueLow;
end;

{ TdxRangePermissionStartDestination }

function TdxRangePermissionStartDestination.CreateEmptyClone: TdxStringValueDestination;
begin
  Result := TdxRangePermissionStartDestination.Create(Importer);
end;

procedure TdxRangePermissionStartDestination.AssignRangePermissionPosition(ARangePermission: TdxImportRangePermissionInfo);
begin
  ARangePermission.Start := Importer.Position.LogPosition;
end;

{ TdxRangePermissionEndDestination }

function TdxRangePermissionEndDestination.CreateEmptyClone: TdxStringValueDestination;
begin
  Result := TdxRangePermissionEndDestination.Create(Importer);
end;

procedure TdxRangePermissionEndDestination.AssignRangePermissionPosition(ARangePermission: TdxImportRangePermissionInfo);
begin
  ARangePermission.&End := Importer.Position.LogPosition;
end;

{ TdxUserTableDestination }

procedure TdxUserTableDestination.NestedGroupFinished(ANestedDestination: TdxRichEditRtfDestinationBase);
var
  ANested: TdxUserTableDestination;
  AUserName: string;
begin
  inherited NestedGroupFinished(ANestedDestination);

  ANested := Safe<TdxUserTableDestination>.Cast(ANestedDestination);
  if ANested <> nil then
  begin
    AUserName := Trim(ANested.Value);
    Importer.DocumentProperties.UserNames.Add(AUserName);
  end;
end;

function TdxUserTableDestination.CreateEmptyClone: TdxStringValueDestination;
begin
  Result := TdxUserTableDestination.Create(Importer);
end;

end.
