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

unit dxRichEdit.DocumentModel.Simple;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, Contnrs,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.Utils.OfficeImage,
  dxGenerics,
  dxRichEdit.Platform.Font,
  dxRichEdit.Options.Core,
  dxRichEdit.Options.Simple,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.CopyManager.Core,
  dxRichEdit.DocumentModel.PieceTableModifiers.Core,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Selections.Core,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.InlineObjectFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.Tables.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Paragraphs,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Cache.Simple,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem;

type
  TdxTextRunBase = class;
  TdxSimpleParagraph = class;
  TdxSimplePieceTable = class;
  TdxSimpleDocumentModel = class;

  { IdxSimpleDocumentModelExporter }

  IdxSimpleDocumentModelExporter = interface
  ['{AFF2F3F0-4C16-493C-BA79-72639C444E7C}']
    procedure Export(ARun: TdxTextRunBase);
  end;

  { TdxRunPropertyModifierBase }

  TdxRunPropertyModifierBase = class abstract
  public
    procedure ModifyTextRun(ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); virtual; abstract;
  end;

  { TdxRichEditHistoryItem }

  TdxRichEditHistoryItem = class abstract(TdxHistoryItem)
  private
    function GetPieceTable: TdxSimplePieceTable;
    function GetDocumentModel: TdxSimpleDocumentModel;
  public
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
  end;

  { TdxInputPosition }

  TdxInputPosition = class
  strict private
    FPieceTable: TdxSimplePieceTable;
    FLogPosition: TdxDocumentLogPosition;
    FParagraphIndex: TdxParagraphIndex;
    FCharacterFormatting: TdxCharacterFormattingBase;
    FMergedCharacterFormatting: TdxCharacterFormattingInfo;
    FCharacterStyleIndex: Integer;
    function GetDocumentModel: TdxSimpleDocumentModel;
    procedure SetLogPosition(const Value: TdxDocumentLogPosition);
    procedure SetParagraphIndex(const Value: TdxParagraphIndex);
  public
    constructor Create(APieceTable: TdxSimplePieceTable); virtual;
    destructor Destroy; override;

    function IsHidden: Boolean;
    function IsValidLogPosition(AValue: TdxDocumentLogPosition): Boolean;
    function IsValidParagraphIndex(Value: TdxParagraphIndex): Boolean;
    function Clone: TdxInputPosition;
    procedure CopyFrom(AValue: TdxInputPosition);
    procedure CopyFormattingFrom(AValue: TdxInputPosition);
    procedure CopyCharacterFormattingFrom(AValue: TdxInputPosition);

    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property PieceTable: TdxSimplePieceTable read FPieceTable;
    property LogPosition: TdxDocumentLogPosition read FLogPosition write SetLogPosition;
    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex write SetParagraphIndex;
    property CharacterFormatting: TdxCharacterFormattingBase read FCharacterFormatting;
    property MergedCharacterFormatting: TdxCharacterFormattingInfo read FMergedCharacterFormatting;
    property CharacterStyleIndex: Integer read FCharacterStyleIndex write FCharacterStyleIndex;
  end;

  { TdxTextRunBase }

  TdxTextRunBase = class(TdxRunBase,
    IdxCharacterProperties,
    IdxCharacterPropertiesContainer,
    IdxObtainAffectedRangeListener)
  strict private
    FCharacterProperties: TdxCharacterProperties;
    FMergedCharacterFormattingCacheIndex: Integer;
    FCharacterStyleIndex: Integer;
    FRowProcessingFlags: TdxRowProcessingFlags;

    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetFontStrikeoutType: TdxStrikeoutType;
    function GetFontUnderlineType: TdxUnderlineType;
    function GetMergedCharacterFormatting: TdxCharacterFormattingInfo;
    function GetMergedCharacterFormattingCacheIndex: Integer;
    function GetParagraph: TdxSimpleParagraph;
    function GetPieceTable: TdxSimplePieceTable;
    function GetCharacterStyle: TdxCharacterStyle;
    procedure SetCharacterStyleIndex(const Value: Integer);
    procedure SetFontStrikeoutType(const Value: TdxStrikeoutType);
    procedure SetFontUnderlineType(const Value: TdxUnderlineType);
    procedure SetParagraph(const Value: TdxSimpleParagraph);
    function TryUseMergedCachedResult(ACachedResult: TdxRunMergedCharacterPropertiesCachedResult): Boolean;
  protected
    function CalculateFontIndexCore: Integer; overload; override;
    function GetRowProcessingFlags: TdxRowProcessingFlags; virtual;
    procedure SetRowProcessingFlags(const AValue: TdxRowProcessingFlags); virtual;

    procedure BeginInit;
    procedure EndInit;
    procedure CancelInit;

    function GetIsUpdateLocked: Boolean;
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;

    function IdxCharacterPropertiesContainer.GetPieceTable = GetPropertiesContainerPieceTable;
    function GetPropertiesContainerPieceTable: TdxCustomPieceTable;
    function CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnCharacterPropertiesChanged;

    procedure NotifyObtainAffectedRange(AArgs: TdxObtainAffectedRangeEventArgs);
    procedure OnCharacterPropertiesObtainAffectedRange(Sender: TObject; E: TdxObtainAffectedRangeEventArgs);

    function CalculateRowProcessingFlags: TdxRowProcessingFlags; virtual;
    procedure InheritRowProcessgFlags(ARun: TdxTextRunBase); virtual;
    procedure ResetCachedIndicesCore; override;

    function GetRawTextFast(ABuffer: TdxChunkedStringBuilder): string; virtual;
    function GetText: string; overload; virtual;

    function CalculateRunIndex: TdxRunIndex;
  {$REGION 'IdxCharacterProperties'}
    function GetAllCaps: Boolean;
    function GetBackColor: TdxAlphaColor;
    function GetDoubleFontSize: Integer;
    function GetFontBold: Boolean;
    function GetFontItalic: Boolean;
    function GetFontName: string;
    function GetForeColor: TdxAlphaColor;
    function GetHidden: Boolean;
    function GetNoProof: Boolean;
    function GetScript: TdxCharacterFormattingScript;
    function GetStrikeoutColor: TdxAlphaColor;
    function GetStrikeoutWordsOnly: Boolean;
    function GetUnderlineColor: TdxAlphaColor;
    function GetUnderlineWordsOnly: Boolean;
    procedure SetAllCaps(const Value: Boolean);
    procedure SetBackColor(const Value: TdxAlphaColor);
    procedure SetDoubleFontSize(const Value: Integer);
    procedure SetFontBold(const Value: Boolean);
    procedure SetFontItalic(const Value: Boolean);
    procedure SetFontName(const Value: string);
    procedure SetForeColor(const Value: TdxAlphaColor);
    procedure SetHidden(const Value: Boolean);
    procedure SetNoProof(const Value: Boolean);
    procedure SetScript(const Value: TdxCharacterFormattingScript);
    procedure SetStrikeoutColor(const Value: TdxAlphaColor);
    procedure SetStrikeoutWordsOnly(const Value: Boolean);
    procedure SetUnderlineColor(const Value: TdxAlphaColor);
    procedure SetUnderlineWordsOnly(const Value: Boolean);
  {$ENDREGION}

    property BatchUpdateHelper: TdxBatchUpdateHelper read GetBatchUpdateHelper;
    property InnerMergedCharacterFormattingCacheIndex: Integer read FMergedCharacterFormattingCacheIndex write FMergedCharacterFormattingCacheIndex;
    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
    property MergedCharacterFormattingCacheIndex: Integer read GetMergedCharacterFormattingCacheIndex;
  public
    constructor Create(AParagraph: TdxParagraphBase; AStartIndex: Integer = 0; ALength: Integer = 1); override;
    destructor Destroy; override;

    function CalculateFontIndexCore(const AFontName: string): Integer; overload;
    function GetMergedCharacterProperties: TdxMergedCharacterProperties; overload; virtual;
    function GetMergedCharacterProperties(ACachedResult: TdxRunMergedCharacterPropertiesCachedResult): TdxMergedCharacterProperties; overload; virtual;

    procedure AfterRunInserted; virtual;
    procedure ApplyFormatting(APos: TdxInputPosition; AForceVisible: Boolean = False); overload;
    procedure ApplyFormatting(const AInfo: TdxCharacterFormattingInfo; const AOptions: TdxCharacterFormattingOptions;
      AStyleIndex: Integer; AForceVisible: Boolean); overload;
    procedure BeforeRunRemoved; virtual;
    function CanJoinWith(ARun: TdxTextRunBase): Boolean; virtual; abstract;
    function CanPlaceCaretBefore: Boolean; virtual; abstract;
    function Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase; virtual; abstract;
    procedure EnsureMergedCharacterFormattingCacheIndexCalculated(ACachedResult: TdxRunMergedCharacterPropertiesCachedResult);
    procedure Export(const AExporter: IdxSimpleDocumentModelExporter); virtual;
    function GetNonEmptyText(ABuffer: TdxChunkedStringBuilder): string;
    function GetParentMergedCharacterProperties: TdxMergedCharacterProperties; virtual;
    function GetPlainText(ABuffer: TdxChunkedStringBuilder): string; overload; virtual;
    function GetRectangularObject: IdxRectangularObject; virtual;
    function GetText(ABuffer: TdxChunkedStringBuilder; AFrom, ATo: Integer): string; overload; virtual;
    procedure InheritStyleAndFormattingFromCore(ARun: TdxTextRunBase; AForceVisible: Boolean = False); virtual;
    procedure ResetCharacterProperties;
    procedure ResetMergedCharacterFormattingIndex;
    function MatchFormatting(AFormatting: TdxCharacterFormattingInfo;
      const AOptions: TdxCharacterFormattingOptions; AStyleIndex: Integer): Boolean; virtual;
    procedure Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer); virtual; abstract;
    function TryUseParentMergedCachedResult(ACachedResult: TdxRunMergedCharacterPropertiesCachedResult): Boolean;
    // for internal use
    procedure CopyCharacterPropertiesFrom(ACopyManager: TdxCustomDocumentModelCopyManager; ASourceRun: TdxTextRunBase);
    function GetPlainText(ABuffer: TdxChunkedStringBuilder; AFrom, ATo: Integer): string; overload; virtual;
    function GetTextFast(ABuffer: TdxChunkedStringBuilder): string; virtual;
    procedure ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType);
    procedure SetCharacterStyleIndexCore(Value: Integer);
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean; virtual; abstract;
    procedure WriteDebugVisualizerData(AWriter: TWriter);

    property CharacterProperties: TdxCharacterProperties read FCharacterProperties;
    property CharacterStyleIndex: Integer read FCharacterStyleIndex write SetCharacterStyleIndex;
    property MergedCharacterFormatting: TdxCharacterFormattingInfo read GetMergedCharacterFormatting;
    property RowProcessingFlags: TdxRowProcessingFlags read GetRowProcessingFlags write SetRowProcessingFlags;
  {$REGION 'IdxCharacterProperties'}
    property AllCaps: Boolean read GetAllCaps write SetAllCaps;
    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property DoubleFontSize: Integer read GetDoubleFontSize write SetDoubleFontSize;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property ForeColor: TdxAlphaColor read GetForeColor write SetForeColor;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;
    property FontName: string read GetFontName write SetFontName;
    property FontStrikeoutType: TdxStrikeoutType read GetFontStrikeoutType write SetFontStrikeoutType;
    property FontUnderlineType: TdxUnderlineType read GetFontUnderlineType write SetFontUnderlineType;
    property Hidden: Boolean read GetHidden write SetHidden;
    property NoProof: Boolean read GetNoProof write SetNoProof;
    property Script: TdxCharacterFormattingScript read GetScript write SetScript;
    property StrikeoutColor: TdxAlphaColor read GetStrikeoutColor write SetStrikeoutColor;
    property StrikeoutWordsOnly: Boolean read GetStrikeoutWordsOnly write SetStrikeoutWordsOnly;
    property Text: string read GetText;
    property UnderlineColor: TdxAlphaColor read GetUnderlineColor write SetUnderlineColor;
    property UnderlineWordsOnly: Boolean read GetUnderlineWordsOnly write SetUnderlineWordsOnly;
  {$ENDREGION}

    property CharacterStyle: TdxCharacterStyle read GetCharacterStyle;
    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property Paragraph: TdxSimpleParagraph read GetParagraph write SetParagraph;
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
  end;

  { TdxTextRunCollection }

  TdxTextRunCollection = class(TdxRunCollection)
  strict private
    function GetItem(Index: TdxRunIndex): TdxTextRunBase;
    procedure SetItem(Index: TdxRunIndex; const Value: TdxTextRunBase);
  public
    function Extract(AIndex: Integer): TdxTextRunBase; reintroduce;
    function First: TdxTextRunBase; reintroduce;
    function Last: TdxTextRunBase; reintroduce;

    property Items[Index: TdxRunIndex]: TdxTextRunBase read GetItem write SetItem; default;
  end;

  { TdxSimpleParagraph }

  TdxSimpleParagraph = class(TdxCustomParagraph,
    IdxParagraphProperties,
    IdxObtainAffectedRangeListener)
  strict private
  {$REGION 'Interfaces'}
    function GetAfterAutoSpacing: Boolean;
    function GetAlignment: TdxParagraphAlignment;
    function GetBackColor: TdxAlphaColor;
    function GetBeforeAutoSpacing: Boolean;
    function GetContextualSpacing: Boolean;
    function GetFirstLineIndent: Integer;
    function GetFirstLineIndentType: TdxParagraphFirstLineIndent;
    function GetKeepLinesTogether: Boolean;
    function GetKeepWithNext: Boolean;
    function GetLeftIndent: Integer;
    function GetLineSpacing: Single;
    function GetLineSpacingType: TdxParagraphLineSpacing;
    function GetOutlineLevel: Integer;
    function GetPageBreakBefore: Boolean;
    function GetRightIndent: Integer;
    function GetSpacingAfter: Integer;
    function GetSpacingBefore: Integer;
    function GetSuppressHyphenation: Boolean;
    function GetSuppressLineNumbers: Boolean;
    function GetWidowOrphanControl: Boolean;
    function GetTopBorder: TdxBorderInfo;
    function GetBottomBorder: TdxBorderInfo;
    function GetLeftBorder: TdxBorderInfo;
    function GetRightBorder: TdxBorderInfo;
    procedure SetAfterAutoSpacing(const Value: Boolean);
    procedure SetAlignment(const Value: TdxParagraphAlignment);
    procedure SetBackColor(const Value: TdxAlphaColor);
    procedure SetBeforeAutoSpacing(const Value: Boolean);
    procedure SetContextualSpacing(const Value: Boolean);
    procedure SetFirstLineIndent(const Value: Integer);
    procedure SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
    procedure SetKeepLinesTogether(const Value: Boolean);
    procedure SetKeepWithNext(const Value: Boolean);
    procedure SetLeftIndent(const Value: Integer);
    procedure SetLineSpacing(const Value: Single);
    procedure SetLineSpacingType(const Value: TdxParagraphLineSpacing);
    procedure SetOutlineLevel(const Value: Integer);
    procedure SetPageBreakBefore(const Value: Boolean);
    procedure SetRightIndent(const Value: Integer);
    procedure SetSpacingAfter(const Value: Integer);
    procedure SetSpacingBefore(const Value: Integer);
    procedure SetSuppressHyphenation(const Value: Boolean);
    procedure SetSuppressLineNumbers(const Value: Boolean);
    procedure SetWidowOrphanControl(const Value: Boolean);
    procedure SetTopBorder(const AValue: TdxBorderInfo);
    procedure SetBottomBorder(const AValue: TdxBorderInfo);
    procedure SetLeftBorder(const AValue: TdxBorderInfo);
    procedure SetRightBorder(const AValue: TdxBorderInfo);
    procedure NotifyObtainAffectedRange(AArgs: TdxObtainAffectedRangeEventArgs);
  {$ENDREGION}
  strict private
    FBoxCollection: TdxSimpleParagraphBoxCollection;
    FFrameProperties: TdxFrameProperties;
    FParagraphStyleIndex: Integer;
    FListLevelIndex: Integer;
    FMergedParagraphFormattingCacheIndex: Integer;
    FNumberingListIndex: TdxNumberingListIndex;
    FTabs: TdxTabProperties;
    function GetDocumentModel: TdxSimpleDocumentModel;
    procedure SetFrameProperties(const Value: TdxFrameProperties);
    function GetMergedParagraphFormatting: TdxParagraphFormattingInfo;
    function GetMergedParagraphFormattingCacheIndex: Integer;
    function GetParagraphStyle: TdxParagraphStyle;
    function GetPieceTable: TdxSimplePieceTable;
    procedure SetOwnNumberingListIndex(const Value: TdxNumberingListIndex);
    procedure SetParagraphStyleIndex(const Value: Integer);
    procedure OnParagraphPropertiesObtainAffectedRange(Sender: TObject; E: TdxObtainAffectedRangeEventArgs);
    procedure ResetCharacterFormattingProperties(AProperties: TdxCharacterProperties; const AStyleFormattingOptions: TdxCharacterFormattingOptions);
  strict protected
    function CreateBoxCollection: TdxSimpleParagraphBoxCollection; virtual;
    function CreateParagraphPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore; override;
    procedure OnParagraphPropertiesChanged; override;

    function EqualsMergedCharacterCachedResult(ACachedResult: TdxParagraphMergedCharacterPropertiesCachedResult): Boolean; virtual;
    procedure DoUseMergedCharacterCachedResult(ACachedResult: TdxParagraphMergedCharacterPropertiesCachedResult); virtual;

    procedure ResetOwnCachedIndices;

    property InnerMergedParagraphFormattingCacheIndex: Integer read FMergedParagraphFormattingCacheIndex write FMergedParagraphFormattingCacheIndex;
    property MergedParagraphFormatting: TdxParagraphFormattingInfo read GetMergedParagraphFormatting;
    property MergedParagraphFormattingCacheIndex: Integer read GetMergedParagraphFormattingCacheIndex;
    property OwnListLevelIndex: Integer read FListLevelIndex write FListLevelIndex;
    property OwnNumberingListIndex: Integer read FNumberingListIndex write FNumberingListIndex;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    destructor Destroy; override;

    function GetCellCore: TdxCustomTableCell; virtual;
    function IsHidden: Boolean;
    function IsInCell: Boolean; virtual;

    function GetTabs: TdxTabFormattingInfo; virtual;
    function GetOwnTabs: TdxTabFormattingInfo;
    procedure SetOwnTabs(ATabs: TdxTabFormattingInfo);

    procedure CreateFrameProperties;
    function GetActualFrameProperties(AIgnoreTableFloatingPosition: Boolean): TdxMergedFrameProperties;
    function GetMergedFrameProperties: TdxMergedFrameProperties;
    function HasFrameProperties: Boolean; inline;
    function HasMergedFrameProperties: Boolean;
    function HasParagraphFrame: Boolean;
    procedure SetDefaultFrameProperties;

    function IsInNonStyleList: Boolean;
    function GetAbstractNumberingListIndex: TdxAbstractNumberingListIndex; virtual;
    function GetListLevelIndex: Integer; override;
    function GetNumberingListIndex: TdxNumberingListIndex; virtual;
    function GetNumberingListText: string; overload; virtual;
    function GetOwnListLevelIndex: Integer;
    function GetOwnNumberingListIndex: TdxNumberingListIndex;

    procedure ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType);
    procedure SetParagraphStyleIndexCore(const ANewStyleIndex: Integer);
    procedure InheritStyleAndFormattingFrom(AParagraph: TdxSimpleParagraph);
    procedure InheritStyleAndFormattingFromCore(AParagraph: TdxSimpleParagraph); virtual;
    function GetParentMergedParagraphProperties: TdxMergedParagraphProperties; virtual;
    function GetMergedCharacterProperties: TdxMergedCharacterProperties; overload; virtual;
    function GetMergedParagraphProperties: TdxMergedParagraphProperties;
    procedure ResetRunsCharacterFormatting;
    function TryUseMergedCharacterCachedResult(ACachedResult: TdxParagraphMergedCharacterPropertiesCachedResult): Boolean;

    property BoxCollection: TdxSimpleParagraphBoxCollection read FBoxCollection;
    property FrameProperties: TdxFrameProperties read FFrameProperties write SetFrameProperties;
    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property ListLevelIndex: Integer read FListLevelIndex;
    property NumberingListIndex: TdxNumberingListIndex read FNumberingListIndex write SetOwnNumberingListIndex;
    property ParagraphStyle: TdxParagraphStyle read GetParagraphStyle;
    property ParagraphStyleIndex: Integer read FParagraphStyleIndex write SetParagraphStyleIndex;
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
    property Tabs: TdxTabProperties read FTabs;
  {$REGION 'IdxParagraphProperties'}
    property AfterAutoSpacing: Boolean read GetAfterAutoSpacing write SetAfterAutoSpacing;
    property Alignment: TdxParagraphAlignment read GetAlignment write SetAlignment;
    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property BeforeAutoSpacing: Boolean read GetBeforeAutoSpacing write SetBeforeAutoSpacing;
    property ContextualSpacing: Boolean read GetContextualSpacing write SetContextualSpacing;
    property FirstLineIndent: Integer read GetFirstLineIndent write SetFirstLineIndent;
    property FirstLineIndentType: TdxParagraphFirstLineIndent read GetFirstLineIndentType write SetFirstLineIndentType;
    property KeepLinesTogether: Boolean read GetKeepLinesTogether write SetKeepLinesTogether;
    property KeepWithNext: Boolean read GetKeepWithNext write SetKeepWithNext;
    property LeftIndent: Integer read GetLeftIndent write SetLeftIndent;
    property LineSpacing: Single read GetLineSpacing write SetLineSpacing;
    property LineSpacingType: TdxParagraphLineSpacing read GetLineSpacingType write SetLineSpacingType;
    property OutlineLevel: Integer read GetOutlineLevel write SetOutlineLevel;
    property PageBreakBefore: Boolean read GetPageBreakBefore write SetPageBreakBefore;
    property RightIndent: Integer read GetRightIndent write SetRightIndent;
    property SpacingAfter: Integer read GetSpacingAfter write SetSpacingAfter;
    property SpacingBefore: Integer read GetSpacingBefore write SetSpacingBefore;
    property SuppressHyphenation: Boolean read GetSuppressHyphenation write SetSuppressHyphenation;
    property SuppressLineNumbers: Boolean read GetSuppressLineNumbers write SetSuppressLineNumbers;
    property WidowOrphanControl: Boolean read GetWidowOrphanControl write SetWidowOrphanControl;
    property TopBorder: TdxBorderInfo read GetTopBorder write SetTopBorder;
    property BottomBorder: TdxBorderInfo read GetBottomBorder write SetBottomBorder;
    property LeftBorder: TdxBorderInfo read GetLeftBorder write SetLeftBorder;
    property RightBorder: TdxBorderInfo read GetRightBorder write SetRightBorder;
  {$ENDREGION}
  end;

  { TdxSimpleParagraphList }

  TdxSimpleParagraphList = class(TdxCustomParagraphList)
  strict private
    function GetItem(Index: Integer): TdxSimpleParagraph;
    procedure SetItem(Index: Integer; const Value: TdxSimpleParagraph);
  public
    function First: TdxSimpleParagraph; reintroduce;
    function Last: TdxSimpleParagraph; reintroduce;
    property Items[Index: Integer]: TdxSimpleParagraph read GetItem write SetItem; default;
  end;

  { TdxSimpleParagraphCollection }

  TdxSimpleParagraphCollection = class(TdxCustomParagraphCollection)
  strict private
  private
    function GetItem(Index: Integer): TdxSimpleParagraph;
  public
    function First: TdxSimpleParagraph; reintroduce;
    function Last: TdxSimpleParagraph; reintroduce;

    property Items[Index: Integer]: TdxSimpleParagraph read GetItem; default;
  end;

  { TdxSimpleContentType }

  TdxSimpleMainContentType =  class(TdxContentTypeBase)
  public
    procedure FixLastParagraphOfLastSection(AOriginalParagraphCount: Integer); override;
  end;

  { TdxLastInsertedRunInfoBase }

  TdxLastInsertedRunInfoBase = class abstract
  strict private
    FHistoryItem: TdxHistoryItem;
    FPieceTable: TdxSimplePieceTable;
    FRun: TdxTextRunBase;
    FRunIndex: TdxRunIndex;
    procedure SetHistoryItem(AValue: TdxHistoryItem);
  public
    constructor Create;
    procedure Reset(APieceTable: TdxSimplePieceTable); virtual;

    property PieceTable: TdxSimplePieceTable read FPieceTable write FPieceTable;
    property Run: TdxTextRunBase read FRun write FRun;
    property RunIndex: TdxRunIndex read FRunIndex write FRunIndex;
    property HistoryItem: TdxHistoryItem read FHistoryItem write SetHistoryItem;
  end;
  TdxLastInsertedInlinePictureRunInfo = class(TdxLastInsertedRunInfoBase);
  TdxLastInsertedSeparatorRunInfo = class(TdxLastInsertedRunInfoBase);

  { TdxLastInsertedRunInfo }

  TdxLastInsertedRunInfo = class(TdxLastInsertedRunInfoBase)
  strict private
    FLogPosition: Integer;
  public
    procedure Reset(APieceTable: TdxSimplePieceTable); override;

    property LogPosition: Integer read FLogPosition write FLogPosition;
  end;

  { TdxSimplePieceTable }

  TdxSimplePieceTable = class(TdxCustomPieceTable,
    IdxDocumentModelStructureChangedListener)
  strict private
    FFields: TdxFieldCollectionBase;
    FHyperlinkInfos: TdxHyperlinkInfoCollection;
    FLastDeltaLength: Integer;
    FLastDeltaRunIndex: Integer;
    FLastShiftedParagraphIndex: TdxParagraphIndex;
    FMultipleRunSplitCount: Integer;
    FTextBuffer: TdxChunkedStringBuilder;
    FVisibleTextFilter: TdxVisibleTextFilterBase;

    FTextInserter: TdxObjectInserter;
    FParagraphInserter: TdxObjectInserter;

    procedure RecalcParagraphsPositionsCore(AFrom: TdxParagraphIndex; ADeltaLength, ADeltaRunIndex: Integer); overload;
    procedure RecalcParagraphsPositionsCore(AFrom: TdxParagraphIndex; ATo: TdxParagraphIndex; ADeltaLength, ADeltaRunIndex: Integer); overload;
    procedure RecalcFieldPosition(AField: TdxField; ARunIndex: TdxRunIndex; ADeltaRunIndex: Integer);
    procedure RecalcFieldsPositions(ARunIndex: TdxRunIndex; ADeltaRunIndex: Integer);
    procedure RecalcFieldsIndices(AFrom, ADeltaIndex: Integer);

    procedure ChangeFieldCode(AField: TdxField; const ACode: string);
    function CreateField(AStartCode, AEndCode: TdxDocumentLogPosition; AResultLength: Integer;
      AForceVisible: Boolean): TdxField; overload;
    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetLastInsertedRunInfo: TdxLastInsertedRunInfo;
    function GetLastInsertedInlinePictureRunInfo: TdxLastInsertedInlinePictureRunInfo;
    function GetLastInsertedSeparatorRunInfo: TdxLastInsertedSeparatorRunInfo;
    function GetParagraphs: TdxSimpleParagraphCollection;
    function GetRuns: TdxTextRunCollection;
    function GetFirstSplitChar(AFontCharacterSet: TdxFontCharacterSet; const AText: string): Integer;
  strict protected
    function CreateParagraphCollection: TdxParagraphBaseCollection; override;
    function CreateRuns: TdxRunCollection; override; final;
    function CreateFieldCollection: TdxFieldCollectionBase; virtual;
    function CreateObjectInserter: TdxObjectInserter; virtual;
    procedure ChangeFieldResult(AField: TdxField; const AResult: string);

    property TextInserter: TdxObjectInserter read FTextInserter;
  protected
    function CreateTextRunsDeletedHistoryItem: TdxRichEditHistoryItem; virtual;
    procedure ChangeCharacterStyle(ARunInfo: TdxRunInfo; AModifier: TdxRunPropertyModifierBase); overload;
    procedure ChangeCharacterStyle(ARunInfo: TdxRunInfo; AParagraph: TdxParagraphBase; AModifier: TdxRunPropertyModifierBase); overload;
    procedure TryToJoinRuns(ARunInfo: TdxRunInfo); overload;
    procedure TryToJoinRuns(AParagraph: TdxParagraphBase; ARunInfo: TdxRunInfo); overload;

    procedure ParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
    procedure DoParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); virtual;
    procedure ParagraphInsertedCore(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); virtual;
    procedure ParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure DoParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); virtual;
    procedure ParagraphRemovedCore(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); virtual;
    procedure ParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure DoParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); virtual;
    procedure ParagraphMergedCore(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); virtual;
    procedure RunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength,
      AHistoryNotificationId: Integer);
    procedure DoRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength,
      AHistoryNotificationId: Integer); virtual;
    procedure RunInsertedCore(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer); virtual;
    procedure RunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer);
    procedure DoRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer); virtual;
    procedure RunRemovedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer); virtual;
    procedure RunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
    procedure DoRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); virtual;
    procedure RunMergedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); virtual;
    procedure RunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
    procedure DoRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); virtual;
    procedure RunUnmergedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); virtual;
    procedure RunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
    procedure DoRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); virtual;
    procedure RunSplitCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); virtual;
    procedure RunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset,
      ATailRunLength: Integer);
    procedure DoRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset,
      ATailRunLength: Integer); virtual;
    procedure RunJoinedCore(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset,
      ATailRunLength: Integer); virtual;
    procedure FieldInserted(AFieldIndex: Integer);
    procedure DoFieldInserted(AFieldIndex: Integer); virtual;
    procedure FieldRemoved(AFieldIndex: Integer);
    procedure DoFieldRemoved(AFieldIndex: Integer); virtual;
    procedure BeginMultipleRunSplit;
    procedure EndMultipleRunSplit;

    procedure InitializeUncheckedInterval; virtual;

  {$REGION 'IdxDocumentModelStructureChangedListener'}
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); virtual;
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); virtual;
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); virtual;
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); virtual;
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer); virtual;
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ASplitOffset: Integer); virtual;
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer); virtual;
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ADeltaRunLength: Integer); virtual;
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); virtual;
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer); virtual;
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer); virtual;
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable); virtual;
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable); virtual;
  {$ENDREGION}
    function CreateVisibleTextFilter(AShowHiddenText: Boolean): TdxVisibleTextFilterBase;
    procedure RecalcParagraphsPositions(AFrom: TdxParagraphIndex; ADeltaLength, ADeltaRunIndex: Integer);
  public
    constructor Create(const ADocumentModel: TdxCustomDocumentModel; const AContentType: TdxContentTypeBase); override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure ResetFormattingCaches(AResetFormattingCacheType: TdxResetFormattingCacheType);
    procedure SetShowHiddenText(AValue: Boolean); virtual;

    function GetChildFieldIndexes(AField: TdxField): TArray<Integer>;
    function GetTextFromSingleRun(const AStartPos, AEndPos: TdxFormatterPosition): string;

    procedure AddParagraphToList(AParagraphIndex: TdxParagraphIndex; ANumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer); virtual;
    procedure ApplyEmptyStyle(ASource: TdxMergedCharacterProperties; ARunInfo: TdxRunInfo);
    procedure ApplyNumberingToInsertedParagraph(AParagraphIndex: TdxParagraphIndex); virtual;
    procedure RemoveNumberingFromParagraph(AParagraph: TdxSimpleParagraph); virtual;

    function GetRunText(ARunIndex: TdxRunIndex): string;
    function GetRunNonEmptyText(ARunIndex: TdxRunIndex): string;
    function GetRunPlainText(ARunIndex: TdxRunIndex): string;
    function GetTableCore(AIndex: Integer): TdxCustomTable; virtual;

    function InsertObjectCore(AInserter: TdxObjectInserter; AParagraphIndex: TdxParagraphIndex;
      ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean = False): TdxRunIndex;

    procedure InsertParagraphCore(APos: TdxInputPosition); overload; virtual;
    function InsertParagraphCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition;
      AForceVisible: Boolean = False): TdxRunIndex; overload;

    procedure InsertTextCore(APos: TdxInputPosition; const AText: string; AForceVisible: Boolean = False); overload;
    procedure InsertTextCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; const AText: string; AForceVisible: Boolean = False); overload;
    procedure InsertTextCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; const AText: Char; AForceVisible: Boolean = False); overload;
    procedure InsertTextCoreWithoutSplit(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; const AText: string; AForceVisible: Boolean);

    procedure InsertText(ALogPosition: TdxDocumentLogPosition; const AText: string; AForceVisible: Boolean = False); overload;
    procedure InsertText(APosition: TdxInputPosition; const AText: string; AForceVisible: Boolean = False); overload;
    procedure InsertPlainText(ALogPosition: TdxDocumentLogPosition; const AText: string; AForceVisible: Boolean = False) overload;
    procedure InsertPlainText(APosition: TdxInputPosition; const AText: string); overload;
    function InsertParagraph(ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean = False): TdxSimpleParagraph; overload;
    function InsertParagraph(AInputPosition: TdxInputPosition; ALogPosition: TdxDocumentLogPosition;
      AForceVisible: Boolean): TdxSimpleParagraph; overload;
    function InsertInlineImageCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition;
      AImage: TdxOfficeImageReference; AScaleX, AScaleY: Single; AUseScreenDpi: Boolean = False; AForceVisible: Boolean = False): TdxTextRunBase; overload;
    function InsertInlineImageCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition;
      AImage: TdxOfficeImageReference; AScaleX, AScaleY: Single; AFillColor: TdxAlphaColor; AUseScreenDpi: Boolean = False; AForceVisible: Boolean = False): TdxTextRunBase; overload;
    function InsertInlinePicture(ALogPosition: TdxDocumentLogPosition; AImage: TdxOfficeImageReference;
      AScaleX: Integer = 100; AScaleY: Integer = 100; AForceVisible: Boolean = False): TdxTextRunBase; overload;
    function InsertInlinePicture(ALogPosition: TdxDocumentLogPosition; AImage: TdxOfficeImage;
      AScaleX: Integer = 100; AScaleY: Integer = 100; AForceVisible: Boolean = False): TdxTextRunBase; overload;
    procedure InsertInlineCustomObjectCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition;
      const ACustomObject: IdxInlineCustomObject; AScaleX, AScaleY: Single; AForceVisible: Boolean = False);
    procedure InsertInlineCustomObject(ALogPosition: TdxDocumentLogPosition; const ACustomObject: IdxInlineCustomObject;
      AScaleX: Integer = 100; AScaleY: Integer = 100; AForceVisible: Boolean = False);
    procedure InsertSeparatorTextRunCore(APos: TdxInputPosition); overload;
    function InsertSeparatorTextRunCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition): TdxRunIndex; overload;

    procedure JoinTextRuns(AParagraphIndex: TdxParagraphIndex; AFirstRunIndex: TdxRunIndex);
    procedure SplitTextRunByCharset(ARunIndex: TdxRunIndex);
    procedure SplitTextRunByCharsetCore(ARunIndex: TdxRunIndex);
    procedure ReplaceText(APosition: TdxDocumentLogPosition; ALength: Integer; const AText: string);

    function CreateField(ALogPosition: TdxDocumentLogPosition; ALength: Integer; AForceVisible: Boolean = False): TdxField; overload;
    procedure DeleteFieldWithoutResult(AField: TdxField);
    function InsertFieldResultEndRunCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean = False): TdxRunIndex; overload;
    function InsertFieldResultEndRunCore(APos: TdxInputPosition): TdxRunIndex; overload;
    function InsertFieldCodeStartRunCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean = False): TdxRunIndex; overload;
    function InsertFieldCodeStartRunCore(APos: TdxInputPosition): TdxRunIndex; overload;
    function InsertFieldCodeEndRunCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean = False): TdxRunIndex; overload;
    function InsertFieldCodeEndRunCore(APos: TdxInputPosition): TdxRunIndex; overload;
    procedure InsertFieldSymbolResult(ALogPosition: TdxDocumentLogPosition; const ASymbol: Char); virtual;
    procedure RemoveField(AField: TdxField);

    procedure ApplyHyperlinkStyle(ARunInfo: TdxRunInfo; AApplyDefaultHyperlinkStyle: Boolean); overload;
    procedure ApplyHyperlinkStyle(AHyperlink: TdxField; AApplyDefaultHyperlinkStyle: Boolean); overload;
    procedure CreateHyperlink(APosition: TdxDocumentLogPosition;
      ALength: Integer; AInfo: TdxHyperlinkInfo; AForceVisible: Boolean = False);
    function CreateHyperlinkField(AStart: TdxDocumentLogPosition; ALength: Integer; AInfo: TdxHyperlinkInfo; AForceVisible: Boolean = False): TdxField;
    procedure DeleteHyperlink(AField: TdxField);
    function IsHyperlinkField(const AField: TdxField): Boolean;
    function GetHyperlinkInfo(AField: TdxField): TdxHyperlinkInfo;
    function GetHyperlinkField(ARunIndex: TdxRunIndex): TdxField;
    function GetHyperlinkFields(AStart: TdxRunIndex; AEnd: TdxRunIndex): TdxFieldList;
    function InsertHyperlinkInfo(AFieldIndex: Integer; AInfo: TdxHyperlinkInfo): TdxHyperlinkInfo;
    procedure ModifyHyperlinkCode(AField: TdxField; AInfo: TdxHyperlinkInfo);
    procedure ModifyHyperlinkResult(AField: TdxField; const AResult: string);
    procedure RemoveHyperlinkInfo(AFieldIndex: Integer);
    procedure ReplaceHyperlinkInfo(AFieldIndex: Integer; ANewInfo: TdxHyperlinkInfo);
    procedure UpdateHyperlinkFieldCode(AField: TdxField);

    procedure SplitTextRun(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AOffset: Integer);
    function ObtainAffectedRunInfo(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): TdxRunInfo;

    function CreateParagraph: TdxSimpleParagraph; virtual;

    function GetFieldResultRunInfo(AField: TdxField): TdxRunInfo;
    function GetFieldRunInfo(AField: TdxField): TdxRunInfo;
    function GetInsertIndex(AField: TdxField): Integer;
    function FindFieldByRunIndex(ARunIndex: TdxRunIndex): TdxField;
    function FindFieldIndexByRunIndex(ARunIndex: TdxRunIndex): Integer; overload;
    function FindFieldIndexByRunIndex(ARunIndex: TdxRunIndex; const APredicate: TdxPredicate<TdxField>): Integer; overload;
    function FindFieldIndexByRunIndexCore(ARunIndex: TdxRunIndex): Integer;
    function SupportFieldCommonStringFormat: Boolean; virtual;
    procedure ToggleFieldCodes(AField: TdxField);
    procedure ToggleAllFieldCodes(AShowCodes: Boolean);
    procedure ToggleFieldLocked(AField: TdxField);

    function CreateDeleteContentOperation: TdxCustomDeleteContentOperation; virtual;

    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property Fields: TdxFieldCollectionBase read FFields;
    property HyperlinkInfos: TdxHyperlinkInfoCollection read FHyperlinkInfos;
    property LastInsertedRunInfo: TdxLastInsertedRunInfo read GetLastInsertedRunInfo;
    property LastInsertedInlinePictureRunInfo: TdxLastInsertedInlinePictureRunInfo read GetLastInsertedInlinePictureRunInfo;
    property LastInsertedSeparatorRunInfo: TdxLastInsertedSeparatorRunInfo read GetLastInsertedSeparatorRunInfo;
    property Paragraphs: TdxSimpleParagraphCollection read GetParagraphs;
    property Runs: TdxTextRunCollection read GetRuns;
    property TextBuffer: TdxChunkedStringBuilder read FTextBuffer;
    property VisibleTextFilter: TdxVisibleTextFilterBase read FVisibleTextFilter;
  end;

  { TdxSimpleSelection }

  TdxSimpleSelection = class(TdxCustomSelection)
  strict protected
    function GetNormalizedStart: TdxDocumentLogPosition; virtual;
    function GetNormalizedEnd: TdxDocumentLogPosition; virtual;
    function GetEnd: TdxDocumentLogPosition; virtual;
    function GetLength: Integer; virtual;
    function GetStart: TdxDocumentLogPosition; virtual;
    procedure SetEnd(const AValue: TdxDocumentLogPosition); virtual;
    procedure SetStart(const AValue: TdxDocumentLogPosition); virtual;
  public
    procedure ClearMultiSelection(AClearFrom: Integer = 0); virtual;
    function GetSelectionPersistentInfo: TdxSelectionPersistentInfo;
    procedure RestoreSelection(AInfo: TdxSelectionPersistentInfo);

    property NormalizedStart: TdxDocumentLogPosition read GetNormalizedStart;
    property NormalizedEnd: TdxDocumentLogPosition read GetNormalizedEnd;
    property Start: TdxDocumentLogPosition read GetStart write SetStart;
    property &End: TdxDocumentLogPosition read GetEnd write SetEnd;
    property Length: Integer read GetLength;
  end;

  { TdxSortedRunIndexCollection }

  TdxSortedRunIndexCollection = class(TcxIUnknownObject, IdxDocumentModelStructureChangedListener)
  private
    FPieceTable: TdxCustomPieceTable;
    FIndices: TdxIntegerList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxRunIndex;
  protected
    procedure ShiftRunIndex(AStartIndex: Integer; ADelta: Integer);
  public
    constructor Create(APieceTable: TdxCustomPieceTable);
    destructor Destroy; override;
    procedure Add(ARunIndex: TdxRunIndex);
  {$REGION 'IdxDocumentModelStructureChangedListener'}
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer);
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ASplitOffset: Integer);
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer);
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ADeltaRunLength: Integer);
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
  {$ENDREGION}
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxRunIndex read GetItem; default;
  end;

  { TdxUnsafeDocumentModelEditor }

  TdxUnsafeDocumentModelEditor = class
  strict private
    FDocumentModel: TdxSimpleDocumentModel;
  public
    constructor Create(ADocumentModel: TdxSimpleDocumentModel);
    procedure InsertFirstSection;
    procedure InsertFirstParagraph(APieceTable: TdxSimplePieceTable);
    procedure DeleteAllRunsInParagraph(APieceTable: TdxSimplePieceTable; AParagraphIndex: TdxParagraphIndex);
    procedure DeleteSections(AStartSectionIndex: TdxSectionIndex; ACount: Integer);
    procedure DeleteParagraphs(APieceTable: TdxSimplePieceTable; AStartParagraphIndex: TdxParagraphIndex; ACount: Integer; ACell: TdxCustomTableCell);
    procedure DeleteRuns(APieceTable: TdxSimplePieceTable; AStartRunIndex: TdxRunIndex; ACount: Integer);
    procedure MergeParagraphs(APieceTable: TdxSimplePieceTable; AFirstParagraph, ASecondParagraph: TdxSimpleParagraph; AUseFirstParagraphStyle: Boolean; ACell: TdxCustomTableCell);
    procedure ReplaceSectionRunWithParagraphRun(APieceTable: TdxSimplePieceTable; ASectionRun: TdxTextRunBase; ARunIndex: TdxRunIndex);
    procedure ReplaceParagraphRunWithSectionRun(APieceTable: TdxSimplePieceTable; ASectionRun: TdxTextRunBase; ARunIndex: TdxRunIndex);

    property DocumentModel: TdxSimpleDocumentModel read FDocumentModel;
  end;

  { TdxSimpleDocumentModelDeferredChanges }

  TdxSimpleDocumentModelDeferredChanges = class(TdxCustomDocumentModelDeferredChanges)
  strict private
    FOriginalSelectionLength: Integer;
    FRunIndicesForSplit: TObjectDictionary<TdxCustomPieceTable, TdxSortedRunIndexCollection>;
    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetSelectionLengthChanged: Boolean;
  protected
    function GetRunIndicesForSplit(APieceTable: TdxCustomPieceTable): TdxSortedRunIndexCollection;
    procedure ResetSelectionChanged; virtual;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); override;
    destructor Destroy; override;

    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property RunIndicesForSplit: TObjectDictionary<TdxCustomPieceTable, TdxSortedRunIndexCollection> read FRunIndicesForSplit;
    property SelectionLengthChanged: Boolean read GetSelectionLengthChanged;
  end;

  { TdxSimpleSection }

  TdxSimpleSection = class(TdxCustomSection)
  public
    procedure SubscribeInnerObjectsEvents; override;
    procedure UnsubscribeInnerObjectsEvents; override;
    procedure SubscribeHeadersFootersEvents; override;
    procedure UnsubscribeHeadersFootersEvents; override;
  end;
  TdxSimpleSectionCollection = class(TdxCustomSectionCollection);

  { TdxSimpleDocumentModel }

  TdxSimpleDocumentModel = class(TdxCustomDocumentModel,
    IdxParagraphPropertiesContainer,
    IdxCharacterPropertiesContainer,
    IdxStylesContainer,
    IdxDocumentModelStructureChangedListener)
  strict private
    FContentType: TdxContentTypeBase;
    FForceNotifyStructureChanged: Boolean;
    FImageCache: TdxImageCache;
    FModelForExport: Boolean;
    FUnsafeEditor: TdxUnsafeDocumentModelEditor;

    FCharacterStyles: TdxCharacterStyleCollection;
    FParagraphStyles: TdxParagraphStyleCollection;
    FStyleLinkManager: TdxStyleLinkManager;

    FDefaultCharacterProperties: TdxCharacterProperties;
    FDefaultParagraphProperties: TdxParagraphProperties;
    FSelection: TdxSimpleSelection;

    FFormattingMarkVisibilityOptions: TdxSimpleFormattingMarkVisibilityOptions;

    FLastInsertedRunInfo: TdxLastInsertedRunInfo;
    FLastInsertedInlinePictureRunInfo: TdxLastInsertedInlinePictureRunInfo;
    FLastInsertedSeparatorRunInfo: TdxLastInsertedSeparatorRunInfo;

    FOnInnerSelectionChanged: TdxNotifyEventHandler;
    FOnSelectionChanged: TdxNotifyEventHandler;

  {$REGION 'IdxStylesContainer'}
    function GetCharacterStyles: TdxCharacterStyleCollection;
    function GetDefaultCharacterProperties: TdxCharacterProperties;
    function GetDefaultParagraphProperties: TdxParagraphProperties;
    function GetParagraphStyles: TdxParagraphStyleCollection;
    function GetStyleLinkManager: TdxStyleLinkManager;
  {$ENDREGION}

    function GetCache: TdxSimpleDocumentCache;
    function GetDeferredChanges: TdxSimpleDocumentModelDeferredChanges;
    function GetDocumentCapabilities: TdxSimpleDocumentCapabilitiesOptions;
    function GetPieceTable: TdxSimplePieceTable;
    function GetSections: TdxSimpleSectionCollection;
    procedure SetDocumentCapabilities(const Value: TdxSimpleDocumentCapabilitiesOptions);

    procedure SubscribeFormattingMarkVisibilityOptions;

    procedure OnFormattingMarkVisibilityOptionsChanged(ASender: TObject; E: TdxRichEditNotificationOptionsChangedArgs);
    procedure OnDefaultCharacterPropertiesObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
    procedure OnDefaultParagraphPropertiesObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
  protected
    function CreateDeferredChanges: TdxCustomDocumentModelDeferredChanges; override;
    function CreateFormattingMarkVisibilityOptions: TdxSimpleFormattingMarkVisibilityOptions; virtual;
    procedure CreateDocumentObjects; override;
    function CreateSectionCollection: TdxCustomSectionCollection; override;
    procedure DestroyDocumentObjects; override;
    function GetIsDocumentProtectionEnabled: Boolean; virtual;

    procedure ClearDocumentContent; override;
    procedure ClearDocumentDefaultPropertiesCore; virtual;
    procedure ClearDocumentDefaultProperties;
    procedure ClearDocumentStyles; virtual;
    procedure ClearDocumentProperties; virtual;
    procedure ClearCore; override;
    procedure InitializeDefaultProperties; virtual;

    function GetLastInsertedRunInfo(APieceTable: TdxSimplePieceTable): TdxLastInsertedRunInfo;
    function GetLastInsertedInlinePictureRunInfo(APieceTable: TdxSimplePieceTable): TdxLastInsertedInlinePictureRunInfo;
    function GetLastInsertedSeparatorRunInfo(APieceTable: TdxSimplePieceTable): TdxLastInsertedSeparatorRunInfo;

    procedure SubscribeCharacterPropertiesEvents;
    procedure UnsubscribeCharacterPropertiesEvents;
    procedure SubscribeParagraphPropertiesEvents;
    procedure UnsubscribeParagraphPropertiesEvents;
    procedure SubscribeSelectionEvents;
    procedure UnsubscribeSelectionEvents;
    procedure SubscribeOptionsEvents; override;

    procedure RaiseInnerSelectionChanged;
    procedure RaiseSelectionChanged;

    procedure OnShowHiddenTextOptionsChanged;

    procedure OnSelectionChanged(ASender: TObject; E: TdxEventArgs);

    procedure BeginClearDocument; override;
    procedure EndClearDocument; override;
    function CreateDocumentCache: TdxCustomDocumentCache; override;
    function CreateDocumentCapabilitiesOptions: TdxCustomDocumentCapabilitiesOptions; override;

    function UseFontSubstitution: Boolean; virtual;

  {$REGION 'IdxDocumentModelStructureChangedListener'}
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); virtual;
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); virtual;
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); virtual;
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); virtual;
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer); virtual;
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ASplitOffset: Integer); virtual;
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer); virtual;
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ADeltaRunLength: Integer); virtual;
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); virtual;
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer); virtual;
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer); virtual;
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable); virtual;
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable); virtual;
  {$ENDREGION}

    function IdxCharacterPropertiesContainer.GetPieceTable = GetPropertiesContainerPieceTable;
    function IdxParagraphPropertiesContainer.GetPieceTable = GetPropertiesContainerPieceTable;
    function GetPropertiesContainerPieceTable: TdxCustomPieceTable;
    function CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    function CreateParagraphPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnCharacterPropertiesChanged;
    procedure OnParagraphPropertiesChanged;
    procedure CreateOptions; override;

    property InnerLastInsertedRunInfo: TdxLastInsertedRunInfo read FLastInsertedRunInfo;
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
  public
    function ValidateActivePieceTable: Boolean;

    function CreateSection: TdxCustomSection; override;
    function CreateMainContentType: TdxSimpleMainContentType; virtual;
    function GetPieceTables(AIncludeUnreferenced: Boolean): TdxFastList; override;
    function GetNumberingListIndex(ATarget: TdxCustomDocumentModel; ASourceListIndex, AMaxNumberingListIndex: TdxNumberingListIndex): TdxNumberingListIndex; virtual;
    procedure ResetDocumentFormattingCaches(AResetFormattingCacheType: TdxResetFormattingCacheType); override;

    function CreateImage(ANativeImage: TdxOfficeImage): TdxOfficeImageReference; overload;
    function CreateImage(AStream: TStream): TdxOfficeImageReference; overload;

    procedure ClearDocumentCore; override;
    procedure ResetUncheckedSpellIntervals;
    procedure ResetSpellCheck(APieceTable: TdxCustomPieceTable; AStartRunIndex, AEndRunIndex: TdxRunIndex; AFormattingOnly: Boolean); overload;
    procedure ResetSpellCheck(AStartRunIndex, AEndRunIndex: TdxRunIndex; AFormattingOnly: Boolean = False); overload;

    procedure NotifyNumberingListParagraphAdded(AIndex: TdxNumberingListIndex); virtual;
    procedure NotifyNumberingListParagraphRemoved(AIndex: TdxNumberingListIndex); virtual;

    procedure ResetMerging; override; final;
    procedure SwitchToSelectionCore(ANewSelection: TdxSimpleSelection); virtual;

    procedure OnSectionInserted(ASectionIndex: TdxSectionIndex); virtual;
    procedure OnSectionRemoved(ASectionIndex: TdxSectionIndex); virtual;

    procedure RaiseHyperlinkInfoInserted(APieceTable: TdxSimplePieceTable; AFieldIndex: Integer); virtual;
    procedure RaiseHyperlinkInfoDeleted(APieceTable: TdxSimplePieceTable; AFieldIndex: Integer); virtual;

    function CreateEmptyCharacterFormatting: TdxCharacterFormattingBase;
    function CreateEmptyParagraphFormatting: TdxParagraphFormattingBase;
    procedure ResetParagraphFormatting(AParagraphFormatting: TdxParagraphFormattingBase);
    procedure ResetCharacterFormatting(ACharacterFormatting: TdxCharacterFormattingBase);

    property Cache: TdxSimpleDocumentCache read GetCache;
    property CharacterStyles: TdxCharacterStyleCollection read FCharacterStyles;
    property DefaultCharacterProperties: TdxCharacterProperties read FDefaultCharacterProperties;
    property DefaultParagraphProperties: TdxParagraphProperties read FDefaultParagraphProperties;
    property DeferredChanges: TdxSimpleDocumentModelDeferredChanges read GetDeferredChanges;
    property DocumentCapabilities: TdxSimpleDocumentCapabilitiesOptions read GetDocumentCapabilities write SetDocumentCapabilities;
    property ForceNotifyStructureChanged: Boolean read FForceNotifyStructureChanged write FForceNotifyStructureChanged;
    property FormattingMarkVisibilityOptions: TdxSimpleFormattingMarkVisibilityOptions read FFormattingMarkVisibilityOptions;
    property ImageCache: TdxImageCache read FImageCache;
    property IsDocumentProtectionEnabled: Boolean read GetIsDocumentProtectionEnabled;
    property MainPieceTable: TdxSimplePieceTable read GetPieceTable;
    property ModelForExport: Boolean read FModelForExport write FModelForExport;
    property ParagraphStyles: TdxParagraphStyleCollection read FParagraphStyles;
    property Sections: TdxSimpleSectionCollection read GetSections;
    property Selection: TdxSimpleSelection read FSelection;
    property StyleLinkManager: TdxStyleLinkManager read FStyleLinkManager;
    property UnsafeEditor: TdxUnsafeDocumentModelEditor read FUnsafeEditor;

    property InnerSelectionChanged: TdxNotifyEventHandler read FOnInnerSelectionChanged;
    property SelectionChanged: TdxNotifyEventHandler read FOnSelectionChanged;
  end;

implementation

uses
  Math, RTLConsts,
  dxCharacters,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.DocumentModel.Commands.Core,
  dxRichEdit.DocumentModel.Commands.Simple,
  dxRichEdit.DocumentModel.VisibleTextFilter.Simple,
  dxRichEdit.DocumentModel.History.Simple,
  dxRichEdit.DocumentModel.History.Style,
  dxRichEdit.DocumentModel.History.FieldHistory,
  dxRichEdit.DocumentModel.History.Section,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.SectionRange,
  dxRichEdit.DocumentModel.PieceTableModifiers.Simple,
  dxRichEdit.DocumentModel.CopyManager.Simple;

{ TdxRichEditHistoryItem }

function TdxRichEditHistoryItem.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := TdxSimpleDocumentModel(inherited DocumentModel);
end;

function TdxRichEditHistoryItem.GetPieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(inherited PieceTable);
end;

{ TdxInputPosition }

constructor TdxInputPosition.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create;
  Assert(APieceTable <> nil);
  FPieceTable := APieceTable;
  FCharacterFormatting := DocumentModel.CreateEmptyCharacterFormatting;
  FMergedCharacterFormatting := TdxCharacterFormattingInfo.Create;
end;

destructor TdxInputPosition.Destroy;
begin
  FreeAndNil(FMergedCharacterFormatting);
  FreeAndNil(FCharacterFormatting);
  inherited Destroy;
end;

function TdxInputPosition.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := FPieceTable.DocumentModel;
end;

function TdxInputPosition.Clone: TdxInputPosition;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxInputPosition.Create(FPieceTable);
  Result.CopyFrom(Self);
end;

procedure TdxInputPosition.CopyCharacterFormattingFrom(AValue: TdxInputPosition);
begin
  CharacterStyleIndex := AValue.CharacterStyleIndex;
  CharacterFormatting.CopyFrom(AValue.CharacterFormatting);
end;

procedure TdxInputPosition.CopyFormattingFrom(AValue: TdxInputPosition);
begin
  CopyCharacterFormattingFrom(AValue);
  MergedCharacterFormatting.CopyFrom(AValue.MergedCharacterFormatting);
end;

procedure TdxInputPosition.CopyFrom(AValue: TdxInputPosition);
begin
  LogPosition := AValue.LogPosition;
  ParagraphIndex := AValue.ParagraphIndex;
  CopyFormattingFrom(AValue);
end;

function TdxInputPosition.IsHidden: Boolean;
begin
  Result := CharacterFormatting.Options.UseHidden and CharacterFormatting.Hidden;
end;

function TdxInputPosition.IsValidLogPosition(AValue: TdxDocumentLogPosition): Boolean;
var
  ALastParagraph: TdxParagraphBase;
begin
  if AValue < 0 then
    Exit(False);
  ALastParagraph := FPieceTable.Paragraphs.Last;
  Result := AValue < (ALastParagraph.LogPosition + ALastParagraph.Length);
end;

function TdxInputPosition.IsValidParagraphIndex(Value: TdxParagraphIndex): Boolean;
begin
  Result := (Value >= 0) and (Value < PieceTable.Paragraphs.Count);
end;

procedure TdxInputPosition.SetLogPosition(const Value: TdxDocumentLogPosition);
begin
  if Value = FLogPosition then
    Exit;
  if not IsValidLogPosition(Value) then
    raise Exception.Create('Bad LogPosition');
  FLogPosition := Value;
end;

procedure TdxInputPosition.SetParagraphIndex(const Value: TdxParagraphIndex);
begin
  if Value = FParagraphIndex then
    Exit;
  if not IsValidParagraphIndex(Value) then
    raise Exception.Create('Bad ParagraphIndex');
  FParagraphIndex := Value;
end;

{ TdxTextRunBase }

constructor TdxTextRunBase.Create(AParagraph: TdxParagraphBase; AStartIndex: Integer = 0; ALength: Integer = 1);
begin
  inherited Create(AParagraph, AStartIndex, ALength);
  FMergedCharacterFormattingCacheIndex := -1;
  FCharacterProperties := TdxCharacterProperties.Create(Self);
end;

destructor TdxTextRunBase.Destroy;
begin
  FreeAndNil(FCharacterProperties);
  inherited Destroy;
end;

function TdxTextRunBase.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := Paragraph.DocumentModel;
end;

function TdxTextRunBase.GetPieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(inherited PieceTable);
end;

function TdxTextRunBase.GetCharacterStyle: TdxCharacterStyle;
begin
  Result := PieceTable.DocumentModel.CharacterStyles[CharacterStyleIndex];
end;

procedure TdxTextRunBase.AfterRunInserted;
begin
end;

procedure TdxTextRunBase.ApplyFormatting(APos: TdxInputPosition; AForceVisible: Boolean = False);
var
  AFormatting: TdxCharacterFormattingBase;
  AShouldResetUseHidden: Boolean;
begin
  AFormatting := APos.CharacterFormatting;
  AShouldResetUseHidden := AForceVisible and AFormatting.Options.UseHidden and AFormatting.Info.Hidden;
  if not AShouldResetUseHidden and not IsUpdateLocked then
  begin
    CharacterProperties.ReplaceInfo(AFormatting, CharacterProperties.BatchUpdateChangeActions);
    CharacterStyleIndex := APos.CharacterStyleIndex;
  end
  else
  begin
    BeginUpdate;
    try
      CharacterProperties.CopyFrom(AFormatting);
      CharacterStyleIndex := APos.CharacterStyleIndex;
      if AShouldResetUseHidden then
        CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseHidden);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxTextRunBase.ApplyFormatting(const AInfo: TdxCharacterFormattingInfo;
  const AOptions: TdxCharacterFormattingOptions; AStyleIndex: Integer; AForceVisible: Boolean);
var
  AShouldResetUseHidden: Boolean;
  AFormatting: TdxCharacterFormattingBase;
begin
  AShouldResetUseHidden := AForceVisible and AOptions.UseHidden and AInfo.Hidden;
  AFormatting := TdxCharacterFormattingBase.Create(PieceTable, DocumentModel, AInfo,
    AOptions);
  try
    if not AShouldResetUseHidden and not IsUpdateLocked then
    begin
      CharacterProperties.ReplaceInfo(AFormatting, CharacterProperties.BatchUpdateChangeActions);
      CharacterStyleIndex := AStyleIndex;
    end
    else
    begin
      BeginUpdate;
      try
        CharacterProperties.CopyFrom(AFormatting);
        CharacterStyleIndex := AStyleIndex;
        if AShouldResetUseHidden then
          CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseHidden);
      finally
        EndUpdate;
      end;
    end;
  finally
    AFormatting.Free;
  end;
end;

procedure TdxTextRunBase.BeforeRunRemoved;
begin
end;

procedure TdxTextRunBase.EnsureMergedCharacterFormattingCacheIndexCalculated(
  ACachedResult: TdxRunMergedCharacterPropertiesCachedResult);
var
  AMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  if FMergedCharacterFormattingCacheIndex < 0 then
  begin
    AMergedCharacterProperties := GetMergedCharacterProperties(ACachedResult);
    FMergedCharacterFormattingCacheIndex := Paragraph.DocumentModel.Cache.MergedCharacterFormattingInfoCache.GetItemIndex(AMergedCharacterProperties.Info);
    FRowProcessingFlags := CalculateRowProcessingFlags;
  end;
end;

procedure TdxTextRunBase.Export(const AExporter: IdxSimpleDocumentModelExporter);
begin
  AExporter.Export(Self);
end;

function TdxTextRunBase.GetNonEmptyText(ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := ABuffer.ToString(StartIndex, Length);
end;

function TdxTextRunBase.GetPlainText(ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := GetTextFast(ABuffer);
end;

function TdxTextRunBase.GetRectangularObject: IdxRectangularObject;
begin
  if not Supports(Self, IdxRectangularObject, Result) then
    Result := nil;
end;

function TdxTextRunBase.GetText(ABuffer: TdxChunkedStringBuilder; AFrom, ATo: Integer): string;
begin
  Result := ABuffer.ToString(StartIndex + AFrom, ATo - AFrom + 1);
end;

procedure TdxTextRunBase.InheritStyleAndFormattingFromCore(ARun: TdxTextRunBase; AForceVisible: Boolean = False);
begin
  SetCharacterStyleIndexCore(ARun.CharacterStyleIndex);
  CharacterProperties.CopyFromCore(ARun.CharacterProperties);
  InnerFontCacheIndex := ARun.InnerFontCacheIndex;
  InnerMergedCharacterFormattingCacheIndex := ARun.InnerMergedCharacterFormattingCacheIndex;
  InheritRowProcessgFlags(ARun);
  if AForceVisible and CharacterProperties.UseHidden and CharacterProperties.Hidden then
  begin
    CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseHidden);
    ResetMergedCharacterFormattingIndex;
  end;
end;

function TdxTextRunBase.MatchFormatting(AFormatting: TdxCharacterFormattingInfo;
  const AOptions: TdxCharacterFormattingOptions; AStyleIndex: Integer): Boolean;
begin
  Result := False;
end;

function TdxTextRunBase.TryUseParentMergedCachedResult(ACachedResult: TdxRunMergedCharacterPropertiesCachedResult): Boolean;
begin
  Result :=
    (ACachedResult.CharacterStyleIndex = CharacterStyleIndex) and
    (ACachedResult.CharacterPropertiesIndex < 0) and
    Paragraph.TryUseMergedCharacterCachedResult(ACachedResult);
  if not Result then
  begin
    ACachedResult.CharacterStyleIndex := CharacterStyleIndex;
    ACachedResult.CharacterPropertiesIndex := -1;
  end;
end;

function TdxTextRunBase.CalculateFontIndexCore(const AFontName: string): Integer;
begin
  Result := Paragraph.PieceTable.DocumentModel.FontCache.CalcFontIndex(AFontName, DoubleFontSize,
    SmallFontStylesMap[FontBold, FontItalic], Script);
end;

function TdxTextRunBase.CalculateFontIndexCore: Integer;
begin
  Result := CalculateFontIndexCore(FontName);
end;

function TdxTextRunBase.GetRowProcessingFlags: TdxRowProcessingFlags;
begin
  Result := FRowProcessingFlags;
end;

procedure TdxTextRunBase.SetRowProcessingFlags(const AValue: TdxRowProcessingFlags);
begin
  FRowProcessingFlags := AValue;
end;

function TdxTextRunBase.GetMergedCharacterProperties: TdxMergedCharacterProperties;
var
  AProperties: TdxMergedCharacterProperties;
begin
  AProperties := GetParentMergedCharacterProperties;
  try
    Result := TdxMergedCharacterProperties.Create(CharacterProperties);
    Result.Merge(AProperties);
  finally
    AProperties.Free;
  end;
end;

function TdxTextRunBase.GetMergedCharacterProperties(ACachedResult: TdxRunMergedCharacterPropertiesCachedResult): TdxMergedCharacterProperties;
begin
  if TryUseMergedCachedResult(ACachedResult) then
    Exit(ACachedResult.MergedCharacterProperties);

  Result := GetMergedCharacterProperties;
  ACachedResult.MergedCharacterProperties := Result;
end;

function TdxTextRunBase.GetParentMergedCharacterProperties: TdxMergedCharacterProperties;
var
  ACharacterProperties: TdxMergedCharacterProperties;
begin
  Result := CharacterStyle.GetMergedCharacterProperties;
  ACharacterProperties := Paragraph.GetMergedCharacterProperties;
  try
    Result.Merge(ACharacterProperties);
  finally
    ACharacterProperties.Free;
  end;
end;

procedure TdxTextRunBase.BeginInit;
begin
  (CharacterProperties as IdxBatchInit).BeginInit;
end;

procedure TdxTextRunBase.EndInit;
begin
  (CharacterProperties as IdxBatchInit).EndInit;
end;

procedure TdxTextRunBase.CancelInit;
begin
  (CharacterProperties as IdxBatchInit).CancelInit;
end;

function TdxTextRunBase.GetIsUpdateLocked: Boolean;
begin
  Result := CharacterProperties.IsUpdateLocked;
end;

function TdxTextRunBase.GetBatchUpdateHelper: TdxBatchUpdateHelper;
begin
  Result := CharacterProperties.BatchUpdateHelper;
end;

procedure TdxTextRunBase.BeginUpdate;
begin
  CharacterProperties.BeginUpdate;
end;

procedure TdxTextRunBase.EndUpdate;
begin
  CharacterProperties.EndUpdate;
end;

procedure TdxTextRunBase.CancelUpdate;
begin
  CharacterProperties.CancelUpdate;
end;

function TdxTextRunBase.GetPropertiesContainerPieceTable: TdxCustomPieceTable;
begin
  Result := GetPieceTable;
end;

function TdxTextRunBase.CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
var
  ARunIndex: TdxRunIndex;
begin
  ARunIndex := GetRunIndex;
  Result := TdxRunCharacterPropertiesChangedHistoryItem.Create(Paragraph.PieceTable, ARunIndex);
end;

procedure TdxTextRunBase.OnCharacterPropertiesChanged;
begin
  ResetCachedIndicesCore;
end;

procedure TdxTextRunBase.NotifyObtainAffectedRange(AArgs: TdxObtainAffectedRangeEventArgs);
begin
  OnCharacterPropertiesObtainAffectedRange(FCharacterProperties, AArgs);
end;

procedure TdxTextRunBase.OnCharacterPropertiesObtainAffectedRange(Sender: TObject; E: TdxObtainAffectedRangeEventArgs);
var
  AThisRunIndex: TdxRunIndex;
begin
  AThisRunIndex := PieceTable.CalculateRunIndex(Self);
  E.Start := AThisRunIndex;
  E.&End := AThisRunIndex;
end;

procedure TdxTextRunBase.InheritRowProcessgFlags(ARun: TdxTextRunBase);
begin
  RowProcessingFlags := ARun.RowProcessingFlags - [TdxRowProcessingFlag.ProcessLayoutDependentText];
end;

procedure TdxTextRunBase.ResetCachedIndicesCore;
begin
  inherited ResetCachedIndicesCore;
  ResetMergedCharacterFormattingIndex;
end;

procedure TdxTextRunBase.ResetCharacterProperties;
begin
  CharacterProperties.Reset;
end;

procedure TdxTextRunBase.ResetMergedCharacterFormattingIndex;
begin
  FMergedCharacterFormattingCacheIndex := -1;
  FRowProcessingFlags := [];
end;

procedure TdxTextRunBase.ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType);
begin
  if AResetFormattingCacheType in [TdxResetFormattingCacheType.Character, TdxResetFormattingCacheType.All] then
    ResetCachedIndicesCore;
end;

procedure TdxTextRunBase.CopyCharacterPropertiesFrom(ACopyManager: TdxCustomDocumentModelCopyManager; ASourceRun: TdxTextRunBase);
begin
  CharacterProperties.CopyFrom(ASourceRun.CharacterProperties.Info);
  CharacterStyleIndex := ASourceRun.CharacterStyle.Copy(ACopyManager.TargetModel);
end;

function TdxTextRunBase.GetPlainText(ABuffer: TdxChunkedStringBuilder; AFrom, ATo: Integer): string;
begin
  Result := GetText(ABuffer, AFrom, ATo);
end;

function TdxTextRunBase.GetRawTextFast(ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := GetTextFast(ABuffer);
end;

function TdxTextRunBase.GetText: string;
begin
	Result := GetTextFast(PieceTable.TextBuffer);
end;

function TdxTextRunBase.GetTextFast(ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := ABuffer.ToString(StartIndex, Length);
end;

function TdxTextRunBase.CalculateRunIndex: TdxRunIndex;
var
  I: TdxRunIndex;
begin
  Result := -1;
  for I := Paragraph.FirstRunIndex to Paragraph.LastRunIndex do
    if PieceTable.Runs[I] = Self then
    begin
      Result := I;
      Break;
    end;
end;

procedure TdxTextRunBase.SetCharacterStyleIndexCore(Value: Integer);
var
  ARunIndex: TdxRunIndex;
begin
  if CharacterStyleIndex <> Value then
  begin
    FCharacterStyleIndex := Value;
    ResetCachedIndicesCore;
    ARunIndex := CalculateRunIndex;
    if ARunIndex >= 0 then
      PieceTable.ApplyChangesCore(TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.CharacterStyle),
        ARunIndex, ARunIndex);
  end;
end;

procedure TdxTextRunBase.WriteDebugVisualizerData(AWriter: TWriter);

  function GetColor(AColor: TdxAlphaColor): string;
  const
    AlphaShift  = 24;
    RedShift    = 16;
    GreenShift  = 8;
    BlueShift   = 0;

    function A(AColor: Cardinal): Byte;
    begin
      Result := Byte(AColor shr AlphaShift);
    end;

    function R(AColor: Cardinal): Byte;
    begin
      Result := Byte(AColor shr RedShift);
    end;

    function G(AColor: Cardinal): Byte;
    begin
      Result := Byte(AColor shr GreenShift);
    end;

    function B(AColor: Cardinal): Byte;
    begin
      Result := Byte(AColor shr BlueShift);
    end;

  begin
    Result := Format('A:%d;R:%d;G:%d;B:%d', [A(AColor), R(AColor), G(AColor), B(AColor)]);
  end;

begin
  AWriter.WriteString(ClassName);
  AWriter.WriteInteger(StartIndex);
  AWriter.WriteInteger(Length);
  AWriter.WriteString(FontName);
  AWriter.WriteString(GetColor(ForeColor));
  AWriter.WriteString(GetColor(BackColor));
  AWriter.WriteBoolean(Hidden);
  AWriter.WriteString(Text);
end;

function TdxTextRunBase.GetAllCaps: Boolean;
begin
  Result := MergedCharacterFormatting.AllCaps;
end;

function TdxTextRunBase.GetBackColor: TdxAlphaColor;
begin
  Result := MergedCharacterFormatting.BackColor;
end;

function TdxTextRunBase.GetDoubleFontSize: Integer;
begin
  Result := MergedCharacterFormatting.DoubleFontSize;
end;

function TdxTextRunBase.GetFontBold: Boolean;
begin
  Result := MergedCharacterFormatting.FontBold;
end;

function TdxTextRunBase.GetFontItalic: Boolean;
begin
  Result := MergedCharacterFormatting.FontItalic;
end;

function TdxTextRunBase.GetFontName: string;
begin
  Result := MergedCharacterFormatting.FontName;
end;

function TdxTextRunBase.GetFontStrikeoutType: TdxStrikeoutType;
begin
  Result := MergedCharacterFormatting.FontStrikeoutType;
end;

function TdxTextRunBase.GetFontUnderlineType: TdxUnderlineType;
begin
  Result := MergedCharacterFormatting.FontUnderlineType;
end;

function TdxTextRunBase.GetMergedCharacterFormatting: TdxCharacterFormattingInfo;
begin
  Result := Paragraph.DocumentModel.Cache.MergedCharacterFormattingInfoCache[MergedCharacterFormattingCacheIndex];
end;

function TdxTextRunBase.GetForeColor: TdxAlphaColor;
begin
  Result := MergedCharacterFormatting.ForeColor;
end;

function TdxTextRunBase.GetHidden: Boolean;
begin
  Result := MergedCharacterFormatting.Hidden;
end;


function TdxTextRunBase.GetNoProof: Boolean;
begin
  Result := MergedCharacterFormatting.NoProof;
end;

function TdxTextRunBase.GetScript: TdxCharacterFormattingScript;
begin
  Result := MergedCharacterFormatting.Script;
end;

function TdxTextRunBase.GetStrikeoutColor: TdxAlphaColor;
begin
  Result := CharacterProperties.StrikeoutColor;
end;

function TdxTextRunBase.GetStrikeoutWordsOnly: Boolean;
begin
  Result := MergedCharacterFormatting.StrikeoutWordsOnly;
end;

function TdxTextRunBase.GetUnderlineColor: TdxAlphaColor;
begin
  Result := MergedCharacterFormatting.UnderlineColor;
end;

function TdxTextRunBase.GetUnderlineWordsOnly: Boolean;
begin
  Result := MergedCharacterFormatting.UnderlineWordsOnly;
end;

procedure TdxTextRunBase.SetAllCaps(const Value: Boolean);
begin
  CharacterProperties.AllCaps := Value;
end;

procedure TdxTextRunBase.SetBackColor(const Value: TdxAlphaColor);
begin
  CharacterProperties.BackColor := Value;
end;

procedure TdxTextRunBase.SetDoubleFontSize(const Value: Integer);
begin
  CharacterProperties.DoubleFontSize := Value;
end;

procedure TdxTextRunBase.SetFontBold(const Value: Boolean);
begin
  CharacterProperties.FontBold := Value;
end;

procedure TdxTextRunBase.SetFontItalic(const Value: Boolean);
begin
  CharacterProperties.FontItalic := Value;
end;

procedure TdxTextRunBase.SetFontName(const Value: string);
begin
  CharacterProperties.FontName := Value;
end;

procedure TdxTextRunBase.SetFontStrikeoutType(const Value: TdxStrikeoutType);
begin
  CharacterProperties.FontStrikeoutType := Value;
end;

procedure TdxTextRunBase.SetFontUnderlineType(const Value: TdxUnderlineType);
begin
  CharacterProperties.FontUnderlineType := Value;
end;

procedure TdxTextRunBase.SetParagraph(const Value: TdxSimpleParagraph);
begin
  inherited Paragraph := Value;
end;

function TdxTextRunBase.TryUseMergedCachedResult(ACachedResult: TdxRunMergedCharacterPropertiesCachedResult): Boolean;
begin
  Result :=
    Paragraph.TryUseMergedCharacterCachedResult(ACachedResult) and
    (ACachedResult.CharacterStyleIndex = CharacterStyleIndex) and
    (ACachedResult.CharacterPropertiesIndex = CharacterProperties.Index);
  if not Result then
  begin
    ACachedResult.CharacterStyleIndex := CharacterStyleIndex;
    ACachedResult.CharacterPropertiesIndex := CharacterProperties.Index;
  end;
end;

procedure TdxTextRunBase.SetForeColor(const Value: TdxAlphaColor);
begin
  CharacterProperties.ForeColor := Value;
end;

procedure TdxTextRunBase.SetHidden(const Value: Boolean);
begin
  CharacterProperties.Hidden := Value;
end;

procedure TdxTextRunBase.SetNoProof(const Value: Boolean);
begin
  CharacterProperties.NoProof := Value;
end;

procedure TdxTextRunBase.SetScript(const Value: TdxCharacterFormattingScript);
begin
  CharacterProperties.Script := Value;
end;

procedure TdxTextRunBase.SetStrikeoutColor(const Value: TdxAlphaColor);
begin
  CharacterProperties.StrikeoutColor := Value;
end;

procedure TdxTextRunBase.SetStrikeoutWordsOnly(const Value: Boolean);
begin
  CharacterProperties.StrikeoutWordsOnly := Value;
end;

procedure TdxTextRunBase.SetUnderlineColor(const Value: TdxAlphaColor);
begin
  CharacterProperties.UnderlineColor := Value;
end;

procedure TdxTextRunBase.SetUnderlineWordsOnly(const Value: Boolean);
begin
  CharacterProperties.UnderlineWordsOnly := Value;
end;

function TdxTextRunBase.GetMergedCharacterFormattingCacheIndex: Integer;
var
  AMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  if FMergedCharacterFormattingCacheIndex < 0 then
  begin
    AMergedCharacterProperties := GetMergedCharacterProperties;
    try
      FMergedCharacterFormattingCacheIndex := Paragraph.DocumentModel.Cache.MergedCharacterFormattingInfoCache.GetItemIndex(AMergedCharacterProperties.Info);
    finally
      AMergedCharacterProperties.Free;
    end;
    FRowProcessingFlags := CalculateRowProcessingFlags;
  end;
  Result := FMergedCharacterFormattingCacheIndex;
end;

function TdxTextRunBase.GetParagraph: TdxSimpleParagraph;
begin
  Result := TdxSimpleParagraph(inherited Paragraph);
end;

function TdxTextRunBase.CalculateRowProcessingFlags: TdxRowProcessingFlags;
var
  AFormatting: TdxCharacterFormattingInfo;
begin
  Result := [];
  AFormatting := MergedCharacterFormatting;
  if (AFormatting.FontUnderlineType <> TdxUnderlineType.None) or (AFormatting.FontStrikeoutType <> TdxStrikeoutType.None) then
    Include(Result, TdxRowProcessingFlag.ProcessCharacterLines);
  if AFormatting.Hidden then
    Include(Result, TdxRowProcessingFlag.ProcessHiddenText);
  if not TdxAlphaColors.IsTransparentOrEmpty(AFormatting.BackColor) then
    Include(Result, TdxRowProcessingFlag.ProcessTextHighlight);
end;

procedure TdxTextRunBase.SetCharacterStyleIndex(const Value: Integer);
var
  AItem: TdxChangeCharacterStyleIndexHistoryItem;
begin
  Assert(Value >= 0);
  if CharacterStyleIndex <> Value then
  begin
    DocumentModel.BeginUpdate;
    try
      AItem := TdxChangeCharacterStyleIndexHistoryItem.Create(PieceTable, GetRunIndex, FCharacterStyleIndex, Value);
      DocumentModel.History.Add(AItem);
      AItem.Execute;
    finally
      DocumentModel.EndUpdate;
    end;
  end;
end;

{ TdxTextRunCollection }

function TdxTextRunCollection.First: TdxTextRunBase;
begin
  Result := TdxTextRunBase(inherited First);
end;

function TdxTextRunCollection.Last: TdxTextRunBase;
begin
  Result := TdxTextRunBase(inherited Last);
end;

procedure TdxTextRunCollection.SetItem(Index: TdxRunIndex; const Value: TdxTextRunBase);
begin
  inherited Items[Index] := Value;
end;

function TdxTextRunCollection.GetItem(Index: TdxRunIndex): TdxTextRunBase;
begin
  Result := TdxTextRunBase(inherited Items[Index]);
end;

function TdxTextRunCollection.Extract(AIndex: Integer): TdxTextRunBase;
begin
  Result := TdxTextRunBase(inherited Extract(AIndex));
end;

{ TdxSimpleParagraph }

constructor TdxSimpleParagraph.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FMergedParagraphFormattingCacheIndex := -1;
  FBoxCollection := CreateBoxCollection;
  FNumberingListIndex := NumberingListIndexListIndexNotSetted;
  FTabs := TdxTabProperties.Create(Self);
end;

destructor TdxSimpleParagraph.Destroy;
begin
  FreeAndNil(FBoxCollection);
  FreeAndNil(FFrameProperties);
  FreeAndNil(FTabs);
  inherited Destroy;
end;

function TdxSimpleParagraph.GetCellCore: TdxCustomTableCell;
begin
  Result := nil;
end;

function TdxSimpleParagraph.IsHidden: Boolean;
var
  ARuns: TdxTextRunCollection;
  I: Integer;
begin
  ARuns := PieceTable.Runs;
  Result := False;
  for I := FirstRunIndex to LastRunIndex do
    if not ARuns[I].Hidden then
      Exit;
  Result := True;
end;

function TdxSimpleParagraph.IsInCell: Boolean;
begin
  Result := False;
end;

function TdxSimpleParagraph.GetTabs: TdxTabFormattingInfo;
var
  ATabs, AStyleTabs: TdxTabFormattingInfo;
begin
  ATabs := Tabs.GetTabs;
  try
    AStyleTabs := ParagraphStyle.GetTabs;
    try
      Result := TdxTabFormattingInfo.Merge(ATabs, AStyleTabs);
    finally
      AStyleTabs.Free;
    end;
  finally
    ATabs.Free;
  end;
end;

function TdxSimpleParagraph.GetOwnTabs: TdxTabFormattingInfo;
begin
  Result := Tabs.GetTabs;
end;

procedure TdxSimpleParagraph.SetOwnTabs(ATabs: TdxTabFormattingInfo);
begin
  Tabs.SetTabs(ATabs);
end;

procedure TdxSimpleParagraph.CreateFrameProperties;
begin
  FFrameProperties.Free;
  FFrameProperties := TdxFrameProperties.Create(Self);
end;

function TdxSimpleParagraph.GetActualFrameProperties(AIgnoreTableFloatingPosition: Boolean): TdxMergedFrameProperties;
var
  ACell: TdxCustomTableCell;
begin
  Result := nil;
  if PieceTable.ContentType.IsTextBox then
    Exit;
  ACell := GetCellCore;
  if not AIgnoreTableFloatingPosition and (ACell <> nil) and
    ACell.GetTableCore.UseFloatingPosition then
    Exit;
  Result := GetMergedFrameProperties;
end;

procedure TdxSimpleParagraph.SetDefaultFrameProperties;
begin
  if FFrameProperties = nil then
    FFrameProperties := TdxFrameProperties.Create(Self);
  FFrameProperties.MakeDefault;
end;

function TdxSimpleParagraph.GetMergedFrameProperties: TdxMergedFrameProperties;
var
  AStyleFrameProperties: TdxMergedFrameProperties;
  ACell: TdxCustomTableCell;
begin
  if FrameProperties <> nil then
  begin
    if FrameProperties.Index = TdxParagraphFrameFormattingCache.DefaultParagraphFrameFormattingIndex then
      Exit(nil);
    Result := TdxMergedFrameProperties.Create(FrameProperties);
    AStyleFrameProperties := ParagraphStyle.GetMergedFrameProperties;
    try
      Result.Merge(AStyleFrameProperties);
    finally
      AStyleFrameProperties.Free;
    end;
    Exit;
  end;
  AStyleFrameProperties := ParagraphStyle.GetMergedFrameProperties;
  if (AStyleFrameProperties <> nil) and AStyleFrameProperties.IsParagraphFrame then
  begin
    ACell := GetCellCore;
    if (ACell <> nil) and ACell.GetTableCore.IsContainsFrame then
    begin
      AStyleFrameProperties.Free;
      Exit(nil);
    end;
  end;
  Result := AStyleFrameProperties;
end;

function TdxSimpleParagraph.HasFrameProperties: Boolean;
begin
  Result := FFrameProperties <> nil;
end;

function TdxSimpleParagraph.HasMergedFrameProperties: Boolean;
var
  AMergedFrameProperties: TdxMergedFrameProperties;
begin
  AMergedFrameProperties := GetMergedFrameProperties;
  try
    Result := AMergedFrameProperties <> nil;
  finally
    AMergedFrameProperties.Free;
  end;
end;

function TdxSimpleParagraph.HasParagraphFrame: Boolean;
var
  AMergedFrameProperties: TdxMergedFrameProperties;
begin
  AMergedFrameProperties := GetMergedFrameProperties;
  try
    Result := (AMergedFrameProperties <> nil) and AMergedFrameProperties.IsParagraphFrame;
  finally
    AMergedFrameProperties.Free;
  end;
end;

function TdxSimpleParagraph.IsInNonStyleList: Boolean;
begin
  Result := GetOwnNumberingListIndex >= NumberingListIndexMinValue;
end;

function TdxSimpleParagraph.GetAbstractNumberingListIndex: TdxAbstractNumberingListIndex;
begin
  Result := -1;
end;

function TdxSimpleParagraph.GetListLevelIndex: Integer;
begin
  Result := FListLevelIndex;
end;

function TdxSimpleParagraph.GetNumberingListIndex: TdxNumberingListIndex;
begin
  Result := FNumberingListIndex;
end;

function TdxSimpleParagraph.GetNumberingListText: string;
begin
  Result := '';
end;

function TdxSimpleParagraph.GetOwnListLevelIndex: Integer;
begin
  Result := FListLevelIndex;
end;

function TdxSimpleParagraph.GetOwnNumberingListIndex: TdxNumberingListIndex;
begin
  Result := FNumberingListIndex;
end;

procedure TdxSimpleParagraph.ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType);
var
  AIndex: TdxRunIndex;
begin
  if AResetFormattingCacheType in [TdxResetFormattingCacheType.All, TdxResetFormattingCacheType.Paragraph] then
    ResetOwnCachedIndices;
  BoxCollection.InvalidateBoxes;
  if Parent = nil then
      Exit;
  if AResetFormattingCacheType in  [TdxResetFormattingCacheType.All, TdxResetFormattingCacheType.Character] then
  begin
    for AIndex := FirstRunIndex to LastRunIndex do
      PieceTable.Runs[AIndex].ResetCachedIndices(AResetFormattingCacheType);
  end;
end;

procedure TdxSimpleParagraph.SetParagraphStyleIndexCore(const ANewStyleIndex: Integer);
begin
  FParagraphStyleIndex := ANewStyleIndex;
  ResetCachedIndices(TdxResetFormattingCacheType.All);
  if Parent <> nil then
    PieceTable.ApplyChangesCore(TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(
      TdxParagraphFormattingChangeType.ParagraphStyle), FirstRunIndex, LastRunIndex);
end;

procedure TdxSimpleParagraph.InheritStyleAndFormattingFrom(AParagraph: TdxSimpleParagraph);
var
  AParagraphTabs: TdxTabFormattingInfo;
begin
  if AParagraph = nil then
    TdxRichEditExceptions.ThrowArgumentException('paragraph', AParagraph);

  ParagraphStyleIndex := AParagraph.ParagraphStyleIndex;
  ParagraphProperties.CopyFrom(AParagraph.ParagraphProperties);
  InnerMergedParagraphFormattingCacheIndex := AParagraph.InnerMergedParagraphFormattingCacheIndex;
  AParagraphTabs := AParagraph.GetOwnTabs;
  try
    Tabs.SetTabs(AParagraphTabs);
  finally
    AParagraphTabs.Free;
  end;
end;

procedure TdxSimpleParagraph.InheritStyleAndFormattingFromCore(AParagraph: TdxSimpleParagraph);
begin
  if AParagraph = nil then
    TdxRichEditExceptions.ThrowArgumentException('paragraph', AParagraph);

  ParagraphStyleIndex := AParagraph.ParagraphStyleIndex;
  ParagraphProperties.CopyFromCore(AParagraph.ParagraphProperties);
  InnerMergedParagraphFormattingCacheIndex := AParagraph.InnerMergedParagraphFormattingCacheIndex;
  Tabs.CopyFromCore(AParagraph.Tabs);
end;

procedure TdxSimpleParagraph.OnParagraphPropertiesChanged;
begin
  ResetOwnCachedIndices;
end;

function TdxSimpleParagraph.EqualsMergedCharacterCachedResult(
  ACachedResult: TdxParagraphMergedCharacterPropertiesCachedResult): Boolean;
begin
  Result := ACachedResult.ParagraphStyleIndex = ParagraphStyleIndex;
end;

procedure TdxSimpleParagraph.DoUseMergedCharacterCachedResult(
  ACachedResult: TdxParagraphMergedCharacterPropertiesCachedResult);
begin
  ACachedResult.ParagraphStyleIndex := ParagraphStyleIndex;
end;

function TdxSimpleParagraph.GetParentMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  Result := TdxMergedParagraphProperties.Create(DocumentModel.DefaultParagraphProperties);
end;

function TdxSimpleParagraph.GetMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  Result := TdxMergedCharacterProperties.Create(DocumentModel.DefaultCharacterProperties);
end;

function TdxSimpleParagraph.GetMergedParagraphProperties: TdxMergedParagraphProperties;
var
  AProperties: TdxMergedParagraphProperties;
begin
  AProperties := GetParentMergedParagraphProperties;
  try
    Result := TdxMergedParagraphProperties.Create(ParagraphProperties);
    Result.Merge(AProperties);
  finally
    AProperties.Free;
  end;
end;

function TdxSimpleParagraph.TryUseMergedCharacterCachedResult(ACachedResult: TdxParagraphMergedCharacterPropertiesCachedResult): Boolean;
begin
  Result := EqualsMergedCharacterCachedResult(ACachedResult);
  if not Result then
    DoUseMergedCharacterCachedResult(ACachedResult);
end;

procedure TdxSimpleParagraph.ResetRunsCharacterFormatting;
var
  ARun: TdxTextRunBase;
  ARuns: TdxTextRunCollection;
  AProperties: TdxCharacterProperties;
  AIndex, AStartIndex, AEndIndex: TdxRunIndex;
  AStyleProperties: TdxMergedCharacterProperties;
begin
  AStyleProperties := DocumentModel.ParagraphStyles[ParagraphStyleIndex].GetMergedCharacterProperties;
  try
    AStartIndex := FirstRunIndex;
    AEndIndex := LastRunIndex;
    ARuns := PieceTable.Runs;
    for AIndex := AStartIndex to AEndIndex do
    begin
      ARun := ARuns[AIndex];
      AProperties := ARun.CharacterProperties;
      ResetCharacterFormattingProperties(AProperties, AStyleProperties.Options);
    end;
  finally
    AStyleProperties.Free;
  end;
end;

procedure TdxSimpleParagraph.ResetCharacterFormattingProperties(AProperties: TdxCharacterProperties; const AStyleFormattingOptions: TdxCharacterFormattingOptions);
begin
  if AProperties.UseAllCaps and AStyleFormattingOptions.UseAllCaps then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseAllCaps);
  if AProperties.UseHidden and AStyleFormattingOptions.UseHidden then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseHidden);
  if AProperties.UseFontBold and AStyleFormattingOptions.UseFontBold then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseFontBold);
  if AProperties.UseFontItalic and AStyleFormattingOptions.UseFontItalic then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseFontItalic);
  if AProperties.UseFontName and AStyleFormattingOptions.UseFontName then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseFontName);
  if AProperties.UseDoubleFontSize and AStyleFormattingOptions.UseDoubleFontSize then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseDoubleFontSize);
  if AProperties.UseFontUnderlineType and AStyleFormattingOptions.UseFontUnderlineType then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseFontUnderlineType);
  if AProperties.UseForeColor and AStyleFormattingOptions.UseForeColor then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseForeColor);
  if AProperties.UseBackColor and AStyleFormattingOptions.UseBackColor then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseBackColor);
  if AProperties.UseScript and AStyleFormattingOptions.UseScript then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseScript);
  if AProperties.UseStrikeoutColor and AStyleFormattingOptions.UseStrikeoutColor then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseStrikeoutColor);
  if AProperties.UseFontStrikeoutType and AStyleFormattingOptions.UseFontStrikeoutType then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseFontStrikeoutType);
  if AProperties.UseStrikeoutWordsOnly and AStyleFormattingOptions.UseStrikeoutWordsOnly then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseStrikeoutWordsOnly);
  if AProperties.UseUnderlineColor and AStyleFormattingOptions.UseUnderlineColor then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseUnderlineColor);
  if AProperties.UseUnderlineWordsOnly and AStyleFormattingOptions.UseUnderlineWordsOnly then
    AProperties.ResetUse(TdxUsedCharacterFormattingOption.UseUnderlineWordsOnly);
end;

procedure TdxSimpleParagraph.ResetOwnCachedIndices;
begin
  InnerMergedParagraphFormattingCacheIndex := -1;
end;

function TdxSimpleParagraph.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := TdxSimpleDocumentModel(inherited DocumentModel);
end;

procedure TdxSimpleParagraph.SetFrameProperties(const Value: TdxFrameProperties);
begin
  if Value = nil then
    FreeAndNil(FFrameProperties)
  else
  begin
    if FFrameProperties = nil then
      FFrameProperties := TdxFrameProperties.Create(Self);
    FFrameProperties.CopyFrom(Value);
  end;
end;

function TdxSimpleParagraph.GetMergedParagraphFormatting: TdxParagraphFormattingInfo;
begin
  Result := DocumentModel.Cache.MergedParagraphFormattingInfoCache[MergedParagraphFormattingCacheIndex];
end;

function TdxSimpleParagraph.GetMergedParagraphFormattingCacheIndex: Integer;
var
  AMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  if FMergedParagraphFormattingCacheIndex < 0 then
  begin
    AMergedParagraphProperties := GetMergedParagraphProperties;
    try
      FMergedParagraphFormattingCacheIndex := DocumentModel.Cache.MergedParagraphFormattingInfoCache.GetItemIndex(AMergedParagraphProperties.Info);
    finally
      AMergedParagraphProperties.Free;
    end;
  end;
  Result := FMergedParagraphFormattingCacheIndex;
end;

function TdxSimpleParagraph.GetParagraphStyle: TdxParagraphStyle;
begin
  Result := DocumentModel.ParagraphStyles[ParagraphStyleIndex];
end;

function TdxSimpleParagraph.GetPieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(inherited PieceTable);
end;

procedure TdxSimpleParagraph.SetOwnNumberingListIndex(const Value: TdxNumberingListIndex);
begin
  if NumberingListIndex <> Value then
  begin
    DocumentModel.BeginUpdate;
    try
      PieceTable.AddParagraphToList(Index, Value, GetListLevelIndex);
    finally
      DocumentModel.EndUpdate;
    end;
  end;
end;

procedure TdxSimpleParagraph.SetParagraphStyleIndex(const Value: Integer);
var
  AItem: TdxChangeParagraphStyleIndexHistoryItem;
begin
  Assert(Value >= 0);
  if FParagraphStyleIndex <> Value then
  begin
    DocumentModel.BeginUpdate;
    try
      AItem := TdxChangeParagraphStyleIndexHistoryItem.Create(PieceTable, Index, FParagraphStyleIndex, Value);
      DocumentModel.History.Add(AItem);
      AItem.Execute;
    finally
      DocumentModel.EndUpdate;
    end;
  end;
end;

procedure TdxSimpleParagraph.OnParagraphPropertiesObtainAffectedRange(Sender: TObject; E: TdxObtainAffectedRangeEventArgs);
begin
  E.Start := FirstRunIndex;
  E.&End := LastRunIndex;
end;

function TdxSimpleParagraph.CreateBoxCollection: TdxSimpleParagraphBoxCollection;
begin
  Result := TdxSimpleParagraphBoxCollection.Create;
end;

function TdxSimpleParagraph.CreateParagraphPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxParagraphParagraphPropertiesChangedHistoryItem.Create(PieceTable, Index);
end;

function TdxSimpleParagraph.GetTopBorder: TdxBorderInfo;
begin
  Result := MergedParagraphFormatting.TopBorder;
end;

function TdxSimpleParagraph.GetBottomBorder: TdxBorderInfo;
begin
  Result := MergedParagraphFormatting.BottomBorder;
end;

function TdxSimpleParagraph.GetLeftBorder: TdxBorderInfo;
begin
  Result := MergedParagraphFormatting.LeftBorder;
end;

function TdxSimpleParagraph.GetRightBorder: TdxBorderInfo;
begin
  Result := MergedParagraphFormatting.RightBorder;
end;

function TdxSimpleParagraph.GetAfterAutoSpacing: Boolean;
begin
  Result := MergedParagraphFormatting.AfterAutoSpacing;
end;

function TdxSimpleParagraph.GetAlignment: TdxParagraphAlignment;
begin
  Result := MergedParagraphFormatting.Alignment;
end;

function TdxSimpleParagraph.GetBackColor: TdxAlphaColor;
begin
  Result := MergedParagraphFormatting.BackColor;
end;

function TdxSimpleParagraph.GetBeforeAutoSpacing: Boolean;
begin
  Result := MergedParagraphFormatting.BeforeAutoSpacing;
end;

function TdxSimpleParagraph.GetContextualSpacing: Boolean;
begin
  Result := MergedParagraphFormatting.ContextualSpacing;
end;

function TdxSimpleParagraph.GetFirstLineIndent: Integer;
begin
  Result := MergedParagraphFormatting.FirstLineIndent;
end;

function TdxSimpleParagraph.GetFirstLineIndentType: TdxParagraphFirstLineIndent;
begin
  Result := MergedParagraphFormatting.FirstLineIndentType;
end;

function TdxSimpleParagraph.GetKeepLinesTogether: Boolean;
begin
  Result := MergedParagraphFormatting.KeepLinesTogether;
end;

function TdxSimpleParagraph.GetKeepWithNext: Boolean;
begin
  Result := MergedParagraphFormatting.KeepWithNext;
end;

function TdxSimpleParagraph.GetLeftIndent: Integer;
begin
  Result := MergedParagraphFormatting.LeftIndent;
end;

function TdxSimpleParagraph.GetLineSpacing: Single;
begin
  Result := MergedParagraphFormatting.LineSpacing;
end;

function TdxSimpleParagraph.GetLineSpacingType: TdxParagraphLineSpacing;
begin
  Result := MergedParagraphFormatting.LineSpacingType;
end;

function TdxSimpleParagraph.GetOutlineLevel: Integer;
begin
  Result := MergedParagraphFormatting.OutlineLevel;
end;

function TdxSimpleParagraph.GetPageBreakBefore: Boolean;
begin
  Result := MergedParagraphFormatting.PageBreakBefore;
end;

function TdxSimpleParagraph.GetRightIndent: Integer;
begin
  Result := MergedParagraphFormatting.RightIndent;
end;

function TdxSimpleParagraph.GetSpacingAfter: Integer;
begin
  Result := MergedParagraphFormatting.SpacingAfter;
end;

function TdxSimpleParagraph.GetSpacingBefore: Integer;
begin
  Result := MergedParagraphFormatting.SpacingBefore;
end;

function TdxSimpleParagraph.GetSuppressHyphenation: Boolean;
begin
  Result := MergedParagraphFormatting.SuppressHyphenation;
end;

function TdxSimpleParagraph.GetSuppressLineNumbers: Boolean;
begin
  Result := MergedParagraphFormatting.SuppressLineNumbers;
end;

function TdxSimpleParagraph.GetWidowOrphanControl: Boolean;
begin
  Result := MergedParagraphFormatting.WidowOrphanControl;
end;

procedure TdxSimpleParagraph.SetAfterAutoSpacing(const Value: Boolean);
begin
  ParagraphProperties.AfterAutoSpacing := Value;
end;

procedure TdxSimpleParagraph.SetAlignment(const Value: TdxParagraphAlignment);
begin
  ParagraphProperties.Alignment := Value;
end;

procedure TdxSimpleParagraph.SetBackColor(const Value: TdxAlphaColor);
begin
  ParagraphProperties.BackColor := Value;
end;

procedure TdxSimpleParagraph.SetBeforeAutoSpacing(const Value: Boolean);
begin
  ParagraphProperties.BeforeAutoSpacing := Value;
end;

procedure TdxSimpleParagraph.SetContextualSpacing(const Value: Boolean);
begin
  ParagraphProperties.ContextualSpacing := Value;
end;

procedure TdxSimpleParagraph.SetFirstLineIndent(const Value: Integer);
begin
  ParagraphProperties.FirstLineIndent := Value;
end;

procedure TdxSimpleParagraph.SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
begin
  ParagraphProperties.FirstLineIndentType := Value;
end;

procedure TdxSimpleParagraph.SetKeepLinesTogether(const Value: Boolean);
begin
  ParagraphProperties.KeepLinesTogether := Value;
end;

procedure TdxSimpleParagraph.SetKeepWithNext(const Value: Boolean);
begin
  ParagraphProperties.KeepWithNext := Value;
end;

procedure TdxSimpleParagraph.SetLeftIndent(const Value: Integer);
begin
  ParagraphProperties.LeftIndent := Value;
end;

procedure TdxSimpleParagraph.SetLineSpacing(const Value: Single);
begin
  ParagraphProperties.LineSpacing := Value;
end;

procedure TdxSimpleParagraph.SetLineSpacingType(const Value: TdxParagraphLineSpacing);
begin
  ParagraphProperties.LineSpacingType := Value;
end;

procedure TdxSimpleParagraph.SetOutlineLevel(const Value: Integer);
begin
  ParagraphProperties.OutlineLevel := Value;
end;

procedure TdxSimpleParagraph.SetPageBreakBefore(const Value: Boolean);
begin
  ParagraphProperties.PageBreakBefore := Value;
end;

procedure TdxSimpleParagraph.SetRightIndent(const Value: Integer);
begin
  ParagraphProperties.RightIndent := Value;
end;

procedure TdxSimpleParagraph.SetSpacingAfter(const Value: Integer);
begin
  ParagraphProperties.SpacingAfter := Value;
end;

procedure TdxSimpleParagraph.SetSpacingBefore(const Value: Integer);
begin
  ParagraphProperties.SpacingBefore := Value;
end;

procedure TdxSimpleParagraph.SetSuppressHyphenation(const Value: Boolean);
begin
  ParagraphProperties.SuppressHyphenation := Value;
end;

procedure TdxSimpleParagraph.SetSuppressLineNumbers(const Value: Boolean);
begin
  ParagraphProperties.SuppressLineNumbers := Value;
end;

procedure TdxSimpleParagraph.SetWidowOrphanControl(const Value: Boolean);
begin
  ParagraphProperties.WidowOrphanControl := Value;
end;

procedure TdxSimpleParagraph.SetTopBorder(const AValue: TdxBorderInfo);
begin
  ParagraphProperties.TopBorder := AValue;
end;

procedure TdxSimpleParagraph.SetBottomBorder(const AValue: TdxBorderInfo);
begin
  ParagraphProperties.BottomBorder := AValue;
end;

procedure TdxSimpleParagraph.SetLeftBorder(const AValue: TdxBorderInfo);
begin
  ParagraphProperties.LeftBorder := AValue;
end;

procedure TdxSimpleParagraph.SetRightBorder(const AValue: TdxBorderInfo);
begin
  ParagraphProperties.RightBorder := AValue;
end;

procedure TdxSimpleParagraph.NotifyObtainAffectedRange(AArgs: TdxObtainAffectedRangeEventArgs);
begin
  OnParagraphPropertiesObtainAffectedRange(Self, AArgs);
end;

{ TdxSimpleParagraphList }

function TdxSimpleParagraphList.GetItem(Index: Integer): TdxSimpleParagraph;
begin
  Result := TdxSimpleParagraph(inherited Items[Index]);
end;

procedure TdxSimpleParagraphList.SetItem(Index: Integer; const Value: TdxSimpleParagraph);
begin
  inherited Items[Index] := Value;
end;

function TdxSimpleParagraphList.First: TdxSimpleParagraph;
begin
  Result := TdxSimpleParagraph(inherited First);
end;

function TdxSimpleParagraphList.Last: TdxSimpleParagraph;
begin
  Result := TdxSimpleParagraph(inherited Last);
end;

{ TdxSimpleParagraphCollection }

function TdxSimpleParagraphCollection.First: TdxSimpleParagraph;
begin
  Result := TdxSimpleParagraph(inherited First);
end;

function TdxSimpleParagraphCollection.Last: TdxSimpleParagraph;
begin
  Result := TdxSimpleParagraph(inherited Last);
end;

function TdxSimpleParagraphCollection.GetItem(
  Index: Integer): TdxSimpleParagraph;
begin
  Result := TdxSimpleParagraph(inherited Items[Index]);
end;

{ TdxSimpleMainContentType }

procedure TdxSimpleMainContentType.FixLastParagraphOfLastSection(AOriginalParagraphCount: Integer);
begin
end;

{ TdxLastInsertedRunInfoBase }

constructor TdxLastInsertedRunInfoBase.Create;
begin
  inherited Create;
  Reset(nil);
end;

procedure TdxLastInsertedRunInfoBase.Reset(APieceTable: TdxSimplePieceTable);
begin
  FPieceTable := APieceTable;
  FRun := nil;
  FRunIndex := -1;
end;

procedure TdxLastInsertedRunInfoBase.SetHistoryItem(AValue: TdxHistoryItem);
begin
  FHistoryItem := AValue;
end;

{ TdxLastInsertedRunInfo }

procedure TdxLastInsertedRunInfo.Reset(APieceTable: TdxSimplePieceTable);
begin
  inherited Reset(APieceTable);
  FLogPosition := -1;
end;

{ TdxSimplePieceTable }

constructor TdxSimplePieceTable.Create(const ADocumentModel: TdxCustomDocumentModel; const AContentType: TdxContentTypeBase);
begin
  inherited Create(ADocumentModel, AContentType);
  FTextBuffer := TdxChunkedStringBuilder.Create;
  FFields := CreateFieldCollection;
  FTextInserter := CreateObjectInserter;
  FHyperlinkInfos := TdxHyperlinkInfoCollection.Create;
  FParagraphInserter := TdxParagraphInserter.Create(Self);
end;

destructor TdxSimplePieceTable.Destroy;
begin
  FreeAndNil(FTextBuffer);
  FreeAndNil(FFields);
  FreeAndNil(FHyperlinkInfos);
  FreeAndNil(FParagraphInserter);
  FreeAndNil(FTextInserter);
  FreeAndNil(FVisibleTextFilter);
  inherited Destroy;
end;

procedure TdxSimplePieceTable.Clear;
begin
  inherited Clear;
  FFields.Clear;
  FHyperlinkInfos.Clear;
  FTextBuffer.Clear;
end;

procedure TdxSimplePieceTable.ResetFormattingCaches(
  AResetFormattingCacheType: TdxResetFormattingCacheType);
var
  I: Integer;
begin
  for I := 0 to Paragraphs.Count - 1 do
    Paragraphs[I].ResetCachedIndices(AResetFormattingCacheType);
end;

procedure TdxSimplePieceTable.SetShowHiddenText(AValue: Boolean);
begin
  FreeAndNil(FVisibleTextFilter);
  FVisibleTextFilter := CreateVisibleTextFilter(AValue);
end;

function TdxSimplePieceTable.GetChildFieldIndexes(AField: TdxField): TArray<Integer>;
var
  AList: TdxIntegerList;
  I, ACount: Integer;
begin
  AList := TdxIntegerList.Create;
  try
    ACount := Fields.Count;
    for I := 0 to ACount - 1 do
    begin
      if AField.FirstRunIndex < Fields[I].FirstRunIndex then
      begin
        if AField.LastRunIndex > Fields[I].LastRunIndex then
          AList.Add(I)
        else
          Break;
      end;
    end;
    Result := AList.ToArray;
  finally
    AList.Free;
  end;
end;

function TdxSimplePieceTable.GetTextFromSingleRun(const AStartPos, AEndPos: TdxFormatterPosition): string;
var
  ARun: TdxTextRunBase;
begin
  ARun := Runs[AStartPos.RunIndex];
  Result := ARun.GetText(FTextBuffer, AStartPos.Offset, AEndPos.Offset);
end;

procedure TdxSimplePieceTable.AddParagraphToList(AParagraphIndex: TdxParagraphIndex;
  ANumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer);
begin
end;

procedure TdxSimplePieceTable.ApplyEmptyStyle(ASource: TdxMergedCharacterProperties; ARunInfo: TdxRunInfo);
var
  AStyleIndex: Integer;
  ATransaction: TdxHistoryTransaction;
  AModifier: TdxReplaceRunCharacterStylePropertiesModifier;
begin
  AStyleIndex := DocumentModel.CharacterStyles.GetStyleIndexByName(TdxCharacterStyleCollection.HyperlinkStyleName);
  if (ARunInfo.Start.RunIndex < 0) or (ARunInfo.&End.RunIndex < ARunInfo.Start.RunIndex) then
    Exit;
  AModifier := TdxReplaceRunCharacterStylePropertiesModifier.Create(AStyleIndex, ASource);
  try
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      ChangeCharacterStyle(ARunInfo, AModifier);
      TryToJoinRuns(ARunInfo);
    finally
      FreeAndNil(ATransaction);
    end;
  finally
    AModifier.Free;
  end;
end;

procedure TdxSimplePieceTable.ApplyNumberingToInsertedParagraph(AParagraphIndex: TdxParagraphIndex);
begin
end;

procedure TdxSimplePieceTable.RemoveNumberingFromParagraph(AParagraph: TdxSimpleParagraph);
begin
end;

function TdxSimplePieceTable.GetRunText(ARunIndex: TdxRunIndex): string;
var
  ARun: TdxTextRunBase;
begin
  ARun := Runs[ARunIndex];
  Result := ARun.GetTextFast(TextBuffer);
end;

function TdxSimplePieceTable.GetRunNonEmptyText(ARunIndex: TdxRunIndex): string;
var
  ARun: TdxTextRunBase;
begin
  ARun := Runs[ARunIndex];
  Result := ARun.GetNonEmptyText(TextBuffer);
end;

function TdxSimplePieceTable.GetRunPlainText(ARunIndex: TdxRunIndex): string;
var
  ARun: TdxTextRunBase;
begin
  ARun := Runs[ARunIndex];
  Result := ARun.GetPlainText(TextBuffer);
end;

function TdxSimplePieceTable.GetTableCore(AIndex: Integer): TdxCustomTable;
begin
  Result := nil;
end;

function TdxSimplePieceTable.InsertObjectCore(AInserter: TdxObjectInserter; AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean = False): TdxRunIndex;
var
  AParagraph: TdxParagraphBase;
  ARunIndex: TdxRunIndex;
  APos: TdxDocumentLogPosition;
begin
  ARunIndex := 0;
  if AInserter.CanMerge(ALogPosition) then
  begin
    AInserter.Merge(ALogPosition, AParagraphIndex);
    Exit(LastInsertedRunInfo.RunIndex);
  end;

  AParagraph := Paragraphs[AParagraphIndex];
  APos := FindRunStartLogPosition(AParagraph, ALogPosition, ARunIndex);
  if ALogPosition = APos then
    Result := ARunIndex
  else
  begin
    SplitTextRun(AParagraph.Index, ARunIndex, ALogPosition - APos);
    Result := ARunIndex + 1;
  end;
  AInserter.PerformInsert(AParagraph, Result, ALogPosition, AForceVisible);
end;

procedure TdxSimplePieceTable.InsertParagraphCore(APos: TdxInputPosition);
begin
  Assert(APos.PieceTable = Self);
  InsertParagraphCore(APos.ParagraphIndex, APos.LogPosition, False);
end;

function TdxSimplePieceTable.InsertParagraphCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition;
  AForceVisible: Boolean = False): TdxRunIndex;
begin
  TextBuffer.Append(TdxCharacters.ParagraphMark);
  Result := InsertObjectCore(FParagraphInserter, AParagraphIndex, ALogPosition, AForceVisible);
end;

procedure TdxSimplePieceTable.InsertTextCore(APos: TdxInputPosition; const AText: string; AForceVisible: Boolean = False);
var
  ALastInsertedRunInfo: TdxLastInsertedRunInfo;
begin
  Assert(APos.PieceTable = Self);
  if TextInserter.CanMerge(APos.LogPosition) then
  begin
    if not LastInsertedRunInfo.Run.MatchFormatting(APos.CharacterFormatting.Info, APos.CharacterFormatting.Options, APos.CharacterStyleIndex) or
        (AForceVisible and APos.IsHidden) then
      DocumentModel.ResetMerging;
  end;

  InsertTextCoreWithoutSplit(APos.ParagraphIndex, APos.LogPosition, AText, AForceVisible);

  ALastInsertedRunInfo := LastInsertedRunInfo;
  ALastInsertedRunInfo.Run.ApplyFormatting(APos, AForceVisible);
  if not DocumentModel.DeferredChanges.IsSetContentMode then
    SplitTextRunByCharset(ALastInsertedRunInfo.RunIndex);

  APos.LogPosition := APos.LogPosition + Length(AText);
end;

procedure TdxSimplePieceTable.InsertTextCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; const AText: string; AForceVisible: Boolean = False);
begin
  InsertTextCoreWithoutSplit(AParagraphIndex, ALogPosition, AText, AForceVisible);
  if not DocumentModel.DeferredChanges.IsSetContentMode then
    if DocumentModel.IsUpdateLocked then
      DocumentModel.DeferredChanges.GetRunIndicesForSplit(Self).Add(LastInsertedRunInfo.RunIndex)
    else
      SplitTextRunByCharset(LastInsertedRunInfo.RunIndex);
end;

procedure TdxSimplePieceTable.InsertTextCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; const AText: Char; AForceVisible: Boolean = False);
begin
  TextBuffer.Append(AText);
  TextInserter.TextLength := 1;
  InsertObjectCore(TextInserter, AParagraphIndex, ALogPosition, AForceVisible);
  if not DocumentModel.DeferredChanges.IsSetContentMode then
  begin
    if DocumentModel.IsUpdateLocked then
      DocumentModel.DeferredChanges.GetRunIndicesForSplit(Self).Add(LastInsertedRunInfo.RunIndex)
    else
      SplitTextRunByCharset(LastInsertedRunInfo.RunIndex);
  end;
end;

procedure TdxSimplePieceTable.InsertTextCoreWithoutSplit(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition; const AText: string; AForceVisible: Boolean);
begin
  TextBuffer.Append(AText);
  TextInserter.TextLength := Length(AText);
  InsertObjectCore(TextInserter, AParagraphIndex, ALogPosition, AForceVisible);
end;

function TdxSimplePieceTable.CreateField(ALogPosition: TdxDocumentLogPosition; ALength: Integer; AForceVisible: Boolean = False): TdxField;
var
  ACommand: TdxPieceTableCreateFieldCommand;
begin
  ACommand := TdxPieceTableCreateFieldCommand.Create(Self, ALogPosition, ALength, AForceVisible);
  try
    ACommand.Execute;
    Result := ACommand.InsertedField;
  finally
    ACommand.Free;
  end;
end;

procedure TdxSimplePieceTable.DeleteFieldWithoutResult(AField: TdxField);
var
  ACommand: TdxPieceTableDeleteFieldWithoutResultCommand;
begin
  ACommand := TdxPieceTableDeleteFieldWithoutResultCommand.Create(Self, AField);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxSimplePieceTable.InsertFieldResultEndRunCore(AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean = False): TdxRunIndex;
const
  AFieldResultEndRunMark = #$203A;
var
  AInserter: TdxFieldResultEndRunInserter;
begin
  FTextBuffer.Append(AFieldResultEndRunMark);
  AInserter := TdxFieldResultEndRunInserter.Create(Self);
  try
    Result := InsertObjectCore(AInserter, AParagraphIndex, ALogPosition, AForceVisible);
  finally
    AInserter.Free;
  end;
end;

function TdxSimplePieceTable.InsertFieldCodeStartRunCore(AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean = False): TdxRunIndex;
var
  AInserter: TdxFieldCodeStartRunInserter;
begin
  FTextBuffer.Append('{');
  AInserter := TdxFieldCodeStartRunInserter.Create(Self);
  try
    Result := InsertObjectCore(AInserter, AParagraphIndex, ALogPosition, AForceVisible);
  finally
    AInserter.Free;
  end;
end;

function TdxSimplePieceTable.InsertFieldCodeEndRunCore(AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean = False): TdxRunIndex;
var
  AInserter: TdxFieldCodeEndRunInserter;
begin
  FTextBuffer.Append('}');
  AInserter := TdxFieldCodeEndRunInserter.Create(Self);
  try
    Result := InsertObjectCore(AInserter, AParagraphIndex, ALogPosition, AForceVisible);
  finally
    AInserter.Free;
  end;
end;

procedure TdxSimplePieceTable.InitializeUncheckedInterval;
begin
end;

function TdxSimplePieceTable.InsertFieldCodeEndRunCore(APos: TdxInputPosition): TdxRunIndex;
begin
  Assert(APos.PieceTable = Self);
  Result := InsertFieldCodeEndRunCore(APos.ParagraphIndex, APos.LogPosition);
  Runs[Result].ApplyFormatting(APos);
  APos.LogPosition := APos.LogPosition + 1;
end;

procedure TdxSimplePieceTable.InsertFieldSymbolResult(ALogPosition: TdxDocumentLogPosition; const ASymbol: Char);
begin
end;

procedure TdxSimplePieceTable.RemoveField(AField: TdxField);
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxRemoveFieldHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    if HyperlinkInfos.IsHyperlink(AField.Index) then
      RemoveHyperlinkInfo(AField.Index);
    AItem := TdxRemoveFieldHistoryItem.Create(Self);
    AItem.RemovedFieldIndex := AField.Index;
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    ATransaction.Free;
  end;
end;

function TdxSimplePieceTable.InsertFieldCodeStartRunCore(APos: TdxInputPosition): TdxRunIndex;
begin
  Assert(APos.PieceTable = Self);
  Result := InsertFieldCodeStartRunCore(APos.ParagraphIndex, APos.LogPosition);
  Runs[Result].ApplyFormatting(APos);
  APos.LogPosition := APos.LogPosition + 1;
end;

function TdxSimplePieceTable.InsertFieldResultEndRunCore(APos: TdxInputPosition): TdxRunIndex;
begin
  Assert(APos.PieceTable = Self);
  Result := InsertFieldResultEndRunCore(APos.ParagraphIndex, APos.LogPosition);
  Runs[Result].ApplyFormatting(APos);
  APos.LogPosition := APos.LogPosition + 1;
end;

procedure TdxSimplePieceTable.CreateHyperlink(APosition: TdxDocumentLogPosition;
  ALength: Integer; AInfo: TdxHyperlinkInfo; AForceVisible: Boolean = False);
var
  AField: TdxField;
begin
  DocumentModel.BeginUpdate;
  try
    AField := CreateHyperlinkField(APosition, ALength, AInfo, AForceVisible);
    ApplyHyperlinkStyle(AField, True);
    ToggleFieldCodes(AField);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxSimplePieceTable.DeleteHyperlink(AField: TdxField);
var
  ARunInfo: TdxRunInfo;
  ASourceRun: TdxTextRunBase;
  AMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  if not IsHyperlinkField(AField) then
    TdxRichEditExceptions.ThrowArgumentException('AField', AField);
  DocumentModel.BeginUpdate;
  try
    ASourceRun := Runs[AField.Code.Start];
    ARunInfo := GetFieldResultRunInfo(AField);
    try
      AMergedCharacterProperties := ASourceRun.GetMergedCharacterProperties;
      try
        ApplyEmptyStyle(AMergedCharacterProperties, ARunInfo);
      finally
        AMergedCharacterProperties.Free;
      end;
    finally
      ARunInfo.Free;
    end;
    DeleteFieldWithoutResult(AField);
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxSimplePieceTable.IsHyperlinkField(const AField: TdxField): Boolean;
begin
  Result := HyperlinkInfos.IsHyperlink(AField.Index);
end;

function TdxSimplePieceTable.GetHyperlinkInfo(AField: TdxField): TdxHyperlinkInfo;
begin
  if not HyperlinkInfos.IsHyperlink(AField.Index) then
    Result := TdxHyperlinkInfo.Empty
  else
    Result := HyperlinkInfos[AField.Index];
end;

function TdxSimplePieceTable.GetHyperlinkField(ARunIndex: TdxRunIndex): TdxField;
begin
  Result := FindFieldByRunIndex(ARunIndex);
  if Result <> nil then
  begin
    if not HyperlinkInfos.IsHyperlink(Result.Index) then
      Result := nil;
  end;
end;

function TdxSimplePieceTable.GetHyperlinkFields(AStart: TdxRunIndex; AEnd: TdxRunIndex): TdxFieldList;
var
  AStartIndex, AEndIndex: Integer;
  I: Integer;
  AField: TdxField;
begin
  Result := TdxFieldList.Create;
  AStartIndex := FindFieldIndexByRunIndex(AStart, IsHyperlinkField);
  if AStartIndex < 0 then
    AStartIndex := not AStartIndex;
  if AStartIndex > Fields.Count then
    Exit;
  AEndIndex := FindFieldIndexByRunIndex(AEnd, IsHyperlinkField);
  if AEndIndex < 0 then
    AEndIndex := not AEndIndex - 1;
  if AEndIndex < AStartIndex then
    Exit;
  for I := AStartIndex to AEndIndex do
  begin
    AField := Fields[I];
    if IsHyperlinkField(AField) then
      Result.Add(AField);
  end;
end;

function TdxSimplePieceTable.InsertHyperlinkInfo(AFieldIndex: Integer; AInfo: TdxHyperlinkInfo): TdxHyperlinkInfo;
var
  AItem: TdxInsertHyperlinkInfoHistoryItem;
begin
  AItem := TdxInsertHyperlinkInfoHistoryItem.Create(Self);
  AItem.FieldIndex := AFieldIndex;
  AItem.HyperlinkInfo := AInfo;
  DocumentModel.History.Add(AItem);
  AItem.Redo;
  Result := HyperlinkInfos[AFieldIndex];
end;

procedure TdxSimplePieceTable.ModifyHyperlinkCode(AField: TdxField; AInfo: TdxHyperlinkInfo);
var
  ATransaction: TdxHistoryTransaction;
begin
  DocumentModel.BeginUpdate;
  try
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      ReplaceHyperlinkInfo(AField.Index, AInfo);
      UpdateHyperlinkFieldCode(AField);
    finally
      FreeAndNil(ATransaction);
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxSimplePieceTable.ModifyHyperlinkResult(AField: TdxField; const AResult: string);
var
  ARunInfo: TdxRunInfo;
  AStart: TdxDocumentLogPosition;
  ATransaction: TdxHistoryTransaction;
begin
  DocumentModel.BeginUpdate;
  try
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      ChangeFieldResult(AField, AResult);
      AStart := TdxDocumentModelPosition.FromRunStart(Self, AField.Result.Start).LogPosition;
      ARunInfo := FindRunInfo(AStart, Length(AResult));
      try
        ApplyHyperlinkStyle(ARunInfo, True);
      finally
        ARunInfo.Free;
      end;
    finally
      ATransaction.Free;
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxSimplePieceTable.RemoveHyperlinkInfo(AFieldIndex: Integer);
var
  AItem: TdxDeleteHyperlinkInfoHistoryItem;
begin
  AItem := TdxDeleteHyperlinkInfoHistoryItem.Create(Self);
  AItem.FieldIndex := AFieldIndex;
  DocumentModel.History.Add(AItem);
  AItem.Redo;
end;

procedure TdxSimplePieceTable.ReplaceHyperlinkInfo(AFieldIndex: Integer; ANewInfo: TdxHyperlinkInfo);
var
  AItem: TdxReplaceHyperlinkInfoHistoryItem;
begin
  AItem := TdxReplaceHyperlinkInfoHistoryItem.Create(Self);
  AItem.NewInfo := ANewInfo;
  AItem.FieldIndex := AFieldIndex;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

function TdxSimplePieceTable.CreateHyperlinkField(AStart: TdxDocumentLogPosition;
  ALength: Integer; AInfo: TdxHyperlinkInfo; AForceVisible: Boolean = False): TdxField;
var
  ABuilder: TdxHyperlinkInstructionBuilder;
  AInstruction: string;
  ATransaction: TdxHistoryTransaction;
  AEndCodePosition: TdxDocumentLogPosition;
  AField: TdxField;
begin
  ABuilder := TdxHyperlinkInstructionBuilder.Create(AInfo);
  try
    AInstruction := ABuilder.GetFieldInstruction;
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      InsertText(AStart, AInstruction, AForceVisible);
      AEndCodePosition := AStart + Length(AInstruction);
      AField := CreateField(AStart, AEndCodePosition, ALength, AForceVisible);
      InsertHyperlinkInfo(AField.Index, AInfo);
      Result := AField;
    finally
      ATransaction.Free;
    end;
  finally
    ABuilder.Free;
  end;
end;

procedure TdxSimplePieceTable.UpdateHyperlinkFieldCode(AField: TdxField);
var
  AInfo: TdxHyperlinkInfo;
  ABuilder: TdxHyperlinkInstructionBuilder;
begin
  AInfo := HyperlinkInfos[AField.Index];
  ABuilder := TdxHyperlinkInstructionBuilder.Create(AInfo);
  try
    ChangeFieldCode(AField, ABuilder.GetFieldInstruction);
  finally
    ABuilder.Free;
  end;
end;

procedure TdxSimplePieceTable.ApplyHyperlinkStyle(ARunInfo: TdxRunInfo; AApplyDefaultHyperlinkStyle: Boolean);
var
  AStyleIndex: Integer;
  AModifier: TdxRunCharacterStyleKeepOldStylePropertiesModifier;
begin
  if (ARunInfo = nil) or (ARunInfo.NormalizedStart.LogPosition > ARunInfo.NormalizedEnd.LogPosition) then
    TdxRichEditExceptions.ThrowArgumentException('runInfo', ARunInfo);
  AStyleIndex := DocumentModel.CharacterStyles.GetStyleIndexByName(TdxCharacterStyleCollection.HyperlinkStyleName);
  if AStyleIndex < 0 then
    Exit;
  AModifier := TdxRunCharacterStyleKeepOldStylePropertiesModifier.Create(AStyleIndex, AApplyDefaultHyperlinkStyle);
  try
    ChangeCharacterStyle(ARunInfo, AModifier);
  finally
    AModifier.Free;
  end;
end;

procedure TdxSimplePieceTable.ApplyHyperlinkStyle(AHyperlink: TdxField; AApplyDefaultHyperlinkStyle: Boolean);
var
  ARunInfo: TdxRunInfo;
begin
  if AHyperlink.Result.&End = AHyperlink.Result.Start then
    Exit;
  ARunInfo := GetFieldResultRunInfo(AHyperlink);
  try
    ApplyHyperlinkStyle(ARunInfo, AApplyDefaultHyperlinkStyle);
  finally
    ARunInfo.Free;
  end;
end;

procedure TdxSimplePieceTable.InsertText(ALogPosition: TdxDocumentLogPosition; const AText: string; AForceVisible: Boolean = False);
var
  ACommand: TdxPieceTableInsertTextAtLogPositionCommand;
begin
  ACommand := TdxPieceTableInsertTextAtLogPositionCommand.Create(Self, ALogPosition, AText, AForceVisible);
  try
    ACommand.Execute;
  finally
    FreeAndNil(ACommand);
  end;
end;

procedure TdxSimplePieceTable.InsertText(APosition: TdxInputPosition; const AText: string; AForceVisible: Boolean = False);
var
  ACommand: TdxPieceTableInsertTextAtInputPositionCommand;
begin
  Assert(APosition.PieceTable = Self);
  ACommand := TdxPieceTableInsertTextAtInputPositionCommand.Create(Self, APosition, AText, AForceVisible);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxSimplePieceTable.InsertPlainText(ALogPosition: TdxDocumentLogPosition; const AText: string; AForceVisible: Boolean = False);
var
  ACommand: TdxPieceTableInsertPlainTextAtLogPositionCommand;
begin
  ACommand := TdxPieceTableInsertPlainTextAtLogPositionCommand.Create(Self, ALogPosition, AText, AForceVisible);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxSimplePieceTable.InsertPlainText(APosition: TdxInputPosition; const AText: string);
var
  ACommand: TdxPieceTableInsertPlainTextAtInputPositionCommand;
begin
  ACommand := TdxPieceTableInsertPlainTextAtInputPositionCommand.Create(Self, APosition, AText, False);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxSimplePieceTable.InsertParagraph(ALogPosition: TdxDocumentLogPosition;
  AForceVisible: Boolean = False): TdxSimpleParagraph;
var
  ACommand: TdxPieceTableInsertParagraphAtLogPositionCommand;
begin
  ACommand := TdxPieceTableInsertParagraphAtLogPositionCommand.Create(Self, ALogPosition, AForceVisible);
  try
    ACommand.Execute;
    Result := Paragraphs[ACommand.ParagraphIndex];
  finally
    ACommand.Free;
  end;
end;

function TdxSimplePieceTable.InsertParagraph(AInputPosition: TdxInputPosition; ALogPosition: TdxDocumentLogPosition;
  AForceVisible: Boolean): TdxSimpleParagraph;
var
  ACommand: TdxPieceTableInsertParagraphAtInputPositionCommand;
begin
  ACommand := TdxPieceTableInsertParagraphAtInputPositionCommand.Create(Self, ALogPosition, AForceVisible, AInputPosition);
  try
    ACommand.Execute;
    Result := Paragraphs[ACommand.ParagraphIndex];
  finally
    ACommand.Free;
  end;
end;

function TdxSimplePieceTable.InsertInlineImageCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition;
  AImage: TdxOfficeImageReference; AScaleX, AScaleY: Single;
  AUseScreenDpi: Boolean = False; AForceVisible: Boolean = False): TdxTextRunBase;
begin
  Result := InsertInlineImageCore(AParagraphIndex, ALogPosition, AImage, AScaleX, AScaleY, TdxAlphaColors.Empty, AUseScreenDpi, AForceVisible);
end;

function TdxSimplePieceTable.InsertInlineImageCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition;
  AImage: TdxOfficeImageReference; AScaleX, AScaleY: Single; AFillColor: TdxAlphaColor;
  AUseScreenDpi: Boolean = False; AForceVisible: Boolean = False): TdxTextRunBase;
var
  AInserter: TdxInlinePictureInserter;
  ANewRunIndex: TdxRunIndex;
begin
  TextBuffer.Append(TdxCharacters.ObjectMark);
  AInserter := TdxInlinePictureInserter.Create(Self, AImage, AScaleX, AScaleY, AFillColor, AUseScreenDpi);
  try
    ANewRunIndex := InsertObjectCore(AInserter, AParagraphIndex, ALogPosition, AForceVisible);
    Result := Runs[ANewRunIndex];
  finally
    AInserter.Free;
  end;
end;

function TdxSimplePieceTable.InsertInlinePicture(
  ALogPosition: TdxDocumentLogPosition; AImage: TdxOfficeImageReference;
  AScaleX: Integer = 100; AScaleY: Integer = 100; AForceVisible: Boolean = False): TdxTextRunBase;
var
  ACommand: TdxPieceTableInsertInlinePictureAtLogPositionCommand;
begin
  ACommand := TdxPieceTableInsertInlinePictureAtLogPositionCommand.Create(Self, ALogPosition,
    AImage, AScaleX, AScaleY, AForceVisible);
  try
    ACommand.Execute;
    Result := TdxInlinePictureRun(ACommand.Result);
  finally
    ACommand.Free;
  end;
end;

function TdxSimplePieceTable.InsertInlinePicture(ALogPosition: TdxDocumentLogPosition; AImage: TdxOfficeImage;
  AScaleX: Integer = 100; AScaleY: Integer = 100; AForceVisible: Boolean = False): TdxTextRunBase;
var
  AImageReference: TdxOfficeImageReference;
begin
  AImageReference := DocumentModel.CreateImage(AImage);
  try
    Result := InsertInlinePicture(ALogPosition, AImageReference, AScaleX, AScaleY, AForceVisible);
  finally
    AImageReference.Free;
  end;
end;

procedure TdxSimplePieceTable.InsertInlineCustomObjectCore(AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition;
  const ACustomObject: IdxInlineCustomObject; AScaleX, AScaleY: Single; AForceVisible: Boolean = False);
var
  AInserter: TdxInlineCustomObjectInserter;
begin
  FTextBuffer.Append(TdxCharacters.ObjectMark);
  AInserter := TdxInlineCustomObjectInserter.Create(Self, ACustomObject, AScaleX, AScaleY);
  try
    InsertObjectCore(AInserter, AParagraphIndex, ALogPosition, AForceVisible);
  finally
    AInserter.Free;
  end;
end;

procedure TdxSimplePieceTable.InsertInlineCustomObject(
  ALogPosition: TdxDocumentLogPosition; const ACustomObject: IdxInlineCustomObject;
  AScaleX: Integer = 100; AScaleY: Integer = 100; AForceVisible: Boolean = False);
var
  ACommand: TdxPieceTableInsertInlineCustomObjectAtLogPositionCommand;
begin
  ACommand := TdxPieceTableInsertInlineCustomObjectAtLogPositionCommand.Create(Self, ALogPosition, ACustomObject,
    AScaleX, AScaleY, AForceVisible);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxSimplePieceTable.InsertSeparatorTextRunCore(APos: TdxInputPosition);
begin
  Assert(APos.PieceTable = Self);
  InsertSeparatorTextRunCore(APos.ParagraphIndex, APos.LogPosition);
  LastInsertedSeparatorRunInfo.Run.ApplyFormatting(APos);
  APos.LogPosition := APos.LogPosition + 1;
end;

function TdxSimplePieceTable.InsertSeparatorTextRunCore(AParagraphIndex: TdxParagraphIndex; ALogPosition: TdxDocumentLogPosition): TdxRunIndex;
var
  AInserter: TdxSeparatorTextRunInserter;
begin
  TextBuffer.Append(TdxSeparatorTextRun.SeparatorText);
  AInserter := TdxSeparatorTextRunInserter.Create(Self);
  try
    Result := InsertObjectCore(AInserter, AParagraphIndex, ALogPosition, False);
  finally
    AInserter.Free;
  end;
end;

procedure TdxSimplePieceTable.JoinTextRuns(AParagraphIndex: TdxParagraphIndex; AFirstRunIndex: TdxRunIndex);
var
  AItem: TdxTextRunsJoinedHistoryItem;
begin
  DocumentModel.History.BeginTransaction;
  try
    AItem := TdxTextRunsJoinedHistoryItem.Create(Self);
    AItem.RunIndex := AFirstRunIndex;
    AItem.ParagraphIndex := AParagraphIndex;
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    DocumentModel.History.EndTransaction;
  end;
end;

procedure TdxSimplePieceTable.SplitTextRunByCharset(ARunIndex: TdxRunIndex);
begin
  if not DocumentModel.UseFontSubstitution then
    Exit;
  SplitTextRunByCharsetCore(ARunIndex);
end;

procedure TdxSimplePieceTable.SplitTextRunByCharsetCore(ARunIndex: TdxRunIndex);
var
  AChar: Char;
  ARun: TdxTextRunBase;
  AFontCache: TdxFontCache;
  ASourceRunFontName, AText, APrevRunFontName, ASubsFontName: string;
  ASourceFontCharacterSet: TdxFontCharacterSet;
  ASplitOffset, AParagraphIndex, ATextLength, ARunStartOffset: Integer;
  ACapabilities: TdxSimpleDocumentCapabilitiesOptions;
  ACharacterFormattingCapability: TdxDocumentCapability;
begin
  ARun := Runs[ARunIndex];
  AFontCache := DocumentModel.FontCache;
  ASourceRunFontName := ARun.FontName;
  ASourceFontCharacterSet := AFontCache.GetFontCharacterSet(ASourceRunFontName);
  if ASourceFontCharacterSet = nil then
    Exit;
  AText := ARun.GetTextFast(TextBuffer);
  ASplitOffset := GetFirstSplitChar(ASourceFontCharacterSet, AText);
  if ASplitOffset < 0 then
    Exit;
  AParagraphIndex := ARun.Paragraph.Index;
  ATextLength := Length(AText);
  ARunStartOffset := ASplitOffset;
  if ASplitOffset > 0 then
  begin
    SplitTextRun(AParagraphIndex, ARunIndex, ASplitOffset);
    Inc(ARunIndex);
  end;
  ACapabilities := DocumentModel.DocumentCapabilities;
  ACharacterFormattingCapability := ACapabilities.CharacterFormatting;
  ACapabilities.CharacterFormatting := TdxDocumentCapability.Default;
  try
    APrevRunFontName := ASourceRunFontName;
    while ASplitOffset < ATextLength do
    begin
      AChar := AText[ASplitOffset + 1];
      if ASourceFontCharacterSet.ContainsChar(AChar) then
        ASubsFontName := ASourceRunFontName
      else
        ASubsFontName := AFontCache.FindSubstituteFont(ASourceRunFontName, AChar, ASourceFontCharacterSet);
      if ASubsFontName <> APrevRunFontName then
      begin
        if ASplitOffset - ARunStartOffset > 0 then
        begin
          SplitTextRun(AParagraphIndex, ARunIndex, ASplitOffset - ARunStartOffset);
          if APrevRunFontName <> ASourceRunFontName then
            Runs[ARunIndex].FontName := APrevRunFontName;
          ARunStartOffset := ASplitOffset;
          Inc(ARunIndex);
        end;
        APrevRunFontName := ASubsFontName;
      end;
      Inc(ASplitOffset);
    end;
    if (ASplitOffset - ARunStartOffset > 0) and (APrevRunFontName <> ASourceRunFontName) then
      Runs[ARunIndex].FontName := APrevRunFontName;
  finally
    ACapabilities.CharacterFormatting := ACharacterFormattingCapability;
  end;
end;

procedure TdxSimplePieceTable.ReplaceText(APosition: TdxDocumentLogPosition; ALength: Integer; const AText: string);
var
  AIsLastParagraphSelected: Boolean;
  ATransaction: TdxHistoryTransaction;
  APositionToInsert: TdxDocumentLogPosition;
  ACommand: TdxPieceTableDeleteTextCommand;
begin
  if APosition < 0 then
		raise Exception.CreateFmt('position %d', [APosition]);
  Assert(ALength >= 0, 'length');
  AIsLastParagraphSelected := (APosition + ALength) > DocumentEndLogPosition;
  DocumentModel.BeginUpdate;
  try
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      APositionToInsert := APosition + ALength;
      if AText <> '' then
        InsertPlainText(APositionToInsert, AText);
        ACommand := TdxPieceTableDeleteTextCommand.Create(Self, APosition, ALength);
        try
          ACommand.DocumentLastParagraphSelected := AIsLastParagraphSelected;
          ACommand.LeaveFieldIfResultIsRemoved := True;
					ACommand.Execute;
        finally
          ACommand.Free;
        end;
    finally
      ATransaction.Free;
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxSimplePieceTable.SplitTextRun(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AOffset: Integer);
var
  AItem: TdxTextRunSplitHistoryItem;
begin
  DocumentModel.History.BeginTransaction;
  try
    AItem := TdxTextRunSplitHistoryItem.Create(Self);
    AItem.RunIndex := ARunIndex;
    AItem.SplitOffset := AOffset;
    AItem.ParagraphIndex := AParagraphIndex;
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    DocumentModel.History.EndTransaction;
  end;
end;

function TdxSimplePieceTable.SupportFieldCommonStringFormat: Boolean;
begin
  Result := False;
end;

procedure TdxSimplePieceTable.ToggleFieldCodes(AField: TdxField);
var
  AItem: TdxToggleFieldCodesHistoryItem;
begin
  DocumentModel.BeginUpdate;
  try
    AItem := TdxToggleFieldCodesHistoryItem.Create(Self, Fields.IndexOf(AField));
    DocumentModel.History.Add(AItem);
    AItem.Redo;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxSimplePieceTable.ToggleAllFieldCodes(AShowCodes: Boolean);
var
  I, ACount: Integer;
  AField: TdxField;
begin
  DocumentModel.BeginUpdate;
  try
    ACount := Fields.Count;
    for I := 0 to ACount - 1 do
    begin
      AField := Fields[I];
      if AField.IsCodeView <> AShowCodes then
        ToggleFieldCodes(AField);
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxSimplePieceTable.ToggleFieldLocked(AField: TdxField);
var
  AItem: TdxToggleFieldLockedHistoryItem;
begin
  DocumentModel.BeginUpdate;
  try
    AItem := TdxToggleFieldLockedHistoryItem.Create(Self, Fields.IndexOf(AField));
    DocumentModel.History.Add(AItem);
    AItem.Redo;
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxSimplePieceTable.CreateDeleteContentOperation: TdxCustomDeleteContentOperation;
begin
  Result := NotImplemented;
end;

function TdxSimplePieceTable.ObtainAffectedRunInfo(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer): TdxRunInfo;
begin
  Assert(DocumentModel.IsUpdateLockedOrOverlapped);
  Result := FindRunInfo(ALogPositionStart, ALength);
  if Result.&End.RunEndLogPosition <> Result.&End.LogPosition then
    SplitTextRun(Result.&End.ParagraphIndex, Result.&End.RunIndex, Result.&End.RunOffset + 1);
  if Result.Start.RunStartLogPosition <> ALogPositionStart then
  begin
    SplitTextRun(Result.Start.ParagraphIndex, Result.Start.RunIndex, Result.Start.RunOffset);
    Result.Start.RunStartLogPosition := Result.Start.LogPosition;
    Result.Start.RunIndex := Result.Start.RunIndex + 1;
    Result.&End.RunIndex := Result.&End.RunIndex + 1;
  end;
end;

function TdxSimplePieceTable.CreateParagraph: TdxSimpleParagraph;
begin
  Result := TdxSimpleParagraph.Create(Self);
end;

function TdxSimplePieceTable.GetFieldResultRunInfo(AField: TdxField): TdxRunInfo;
var
  APos: TdxDocumentModelPosition;
begin
  Result := TdxRunInfo.Create(Self);
  APos := Result.Start;
  TdxDocumentModelPosition.SetRunStart(APos, AField.Result.Start);
  Result.Start.CopyFrom(APos);
  APos := Result.&End;
  TdxDocumentModelPosition.SetRunStart(APos, AField.Result.&End - 1);
  Result.&End.CopyFrom(APos);
  Result.&End.LogPosition := Result.&End.RunEndLogPosition;
end;

function TdxSimplePieceTable.GetFieldRunInfo(AField: TdxField): TdxRunInfo;
var
  APos: TdxDocumentModelPosition;
begin
  Result := TdxRunInfo.Create(Self);
  APos := Result.Start;
  TdxDocumentModelPosition.SetRunStart(APos, AField.Code.Start);
  Result.Start.CopyFrom(APos);
  APos := Result.&End;
  TdxDocumentModelPosition.SetRunStart(APos, AField.Result.&End);
  Result.&End.CopyFrom(APos);
end;

function TdxSimplePieceTable.GetInsertIndex(AField: TdxField): Integer;
var
  ACount: Integer;
  I: Integer;
begin
  ACount := Fields.Count;
  for I := 0 to ACount - 1 do
  begin
    if AField.FirstRunIndex > Fields[I].FirstRunIndex then
    begin
      if AField.LastRunIndex < Fields[I].LastRunIndex then
        Exit(I);
    end
    else
      Exit(not I);
  end;
  Result := not ACount;
end;

function TdxSimplePieceTable.FindFieldByRunIndex(ARunIndex: TdxRunIndex): TdxField;
var
  AIndex: Integer;
begin
  AIndex := FindFieldIndexByRunIndex(ARunIndex);
  if AIndex >= 0 then
    Result := Fields[AIndex]
  else
    Result := nil;
end;

function TdxSimplePieceTable.FindFieldIndexByRunIndex(ARunIndex: TdxRunIndex): Integer;
begin
  Result := FindFieldIndexByRunIndex(ARunIndex,
    function(const AField: TdxField): Boolean
    begin
      Result := True;
    end);
end;

function TdxSimplePieceTable.FindFieldIndexByRunIndex(ARunIndex: TdxRunIndex; const APredicate: TdxPredicate<TdxField>): Integer;
var
  AIndex: Integer;
  AField: TdxField;
begin
  AIndex := FindFieldIndexByRunIndexCore(ARunIndex);
  if AIndex < 0 then
    Exit(AIndex);
  AField := Fields[AIndex];
  repeat
    if (ARunIndex >= AField.FirstRunIndex) and APredicate(AField) then
    begin
      Exit(AField.Index);
    end;
    AField := AField.Parent;
  until (AField = nil);
  Result := not AIndex;
end;

function TdxSimplePieceTable.FindFieldIndexByRunIndexCore(ARunIndex: TdxRunIndex): Integer;
var
  ACount: Integer;
  AComparator: TdxFieldRunIndexComparable;
begin
  ACount := Fields.Count;
  AComparator := TdxFieldRunIndexComparable.Create(ARunIndex);
  try
    if not TdxAlgorithms1<TdxField>.BinarySearch(Fields.InnerList, AComparator, Result) then
    begin
      if Result >= ACount then
        Exit(not Result);
    end;
  finally
    AComparator.Free;
  end;
end;

function TdxSimplePieceTable.CreateParagraphCollection: TdxParagraphBaseCollection;
begin
  Result := TdxSimpleParagraphCollection.Create;
end;

function TdxSimplePieceTable.CreateRuns: TdxRunCollection;
begin
  Result := TdxTextRunCollection.Create;
end;

function TdxSimplePieceTable.CreateTextRunsDeletedHistoryItem: TdxRichEditHistoryItem;
begin
  Result := TdxSimpleTextRunsDeletedHistoryItem.Create(Self);
end;

function TdxSimplePieceTable.CreateFieldCollection: TdxFieldCollectionBase;
begin
  Result := TdxFieldCollectionBase.Create(Self);
end;

function TdxSimplePieceTable.CreateObjectInserter: TdxObjectInserter;
begin
  Result := TdxSimpleTextInserter.Create(Self);
end;

procedure TdxSimplePieceTable.ChangeCharacterStyle(ARunInfo: TdxRunInfo; AModifier: TdxRunPropertyModifierBase);
var
  AEndParagraphIndex: TdxParagraphIndex;
  I: Integer;
begin
  AEndParagraphIndex := ARunInfo.&End.ParagraphIndex;
  for I := ARunInfo.Start.ParagraphIndex to AEndParagraphIndex do
    ChangeCharacterStyle(ARunInfo, Paragraphs[I], AModifier);
end;

procedure TdxSimplePieceTable.ChangeCharacterStyle(ARunInfo: TdxRunInfo; AParagraph: TdxParagraphBase; AModifier: TdxRunPropertyModifierBase);
var
  ALastRunIndex, AFirstRunIndex: TdxRunIndex;
  I: Integer;
begin
  ALastRunIndex := Min(ARunInfo.&End.RunIndex, AParagraph.LastRunIndex);
  AFirstRunIndex := Max(ARunInfo.Start.RunIndex, AParagraph.FirstRunIndex);
  for I := AFirstRunIndex to ALastRunIndex do
    AModifier.ModifyTextRun(Runs[I], I);
end;

procedure TdxSimplePieceTable.TryToJoinRuns(ARunInfo: TdxRunInfo);
var
  I, AEndParagraphIndex: TdxParagraphIndex;
begin
  Assert(DocumentModel.IsUpdateLockedOrOverlapped);
  AEndParagraphIndex := ARunInfo.&End.ParagraphIndex;
  for I := ARunInfo.Start.ParagraphIndex to AEndParagraphIndex do
    TryToJoinRuns(Paragraphs[I], ARunInfo);
end;

procedure TdxSimplePieceTable.TryToJoinRuns(AParagraph: TdxParagraphBase; ARunInfo: TdxRunInfo);
var
  I, ALastRunIndex, AFirstRunIndex: TdxRunIndex;
begin
  Assert(DocumentModel.IsUpdateLockedOrOverlapped);
  ALastRunIndex := Min(ARunInfo.&End.RunIndex, AParagraph.LastRunIndex - 1);
  AFirstRunIndex := Max(ARunInfo.Start.RunIndex, AParagraph.FirstRunIndex);
  for I := ALastRunIndex downto AFirstRunIndex + 1 do
    if Runs[I - 1].CanJoinWith(Runs[I]) then
      JoinTextRuns(AParagraph.Index, I - 1);
end;

function TdxSimplePieceTable.CreateVisibleTextFilter(AShowHiddenText: Boolean): TdxVisibleTextFilterBase;
begin
  if AShowHiddenText then
    Result := TdxEmptyTextFilter.Create(Self)
  else
    Result := TdxVisibleTextFilter.Create(Self);
end;

procedure TdxSimplePieceTable.RecalcParagraphsPositions(AFrom: TdxParagraphIndex; ADeltaLength, ADeltaRunIndex: Integer);
var
  AParagraph: TdxSimpleParagraph;
  ANewLastRunIndex: TdxRunIndex;
begin
  if FMultipleRunSplitCount > 0 then
    RecalcParagraphsPositionsCore(FLastShiftedParagraphIndex + 1, AFrom, FLastDeltaLength, FLastDeltaRunIndex);
  AParagraph := Paragraphs[AFrom];
  AParagraph.Length := AParagraph.Length + ADeltaLength;
  ANewLastRunIndex := AParagraph.LastRunIndex + ADeltaRunIndex;
  AParagraph.SetRelativeLastRunIndexWithoutCheck(ANewLastRunIndex);
  if FMultipleRunSplitCount = 0 then
    RecalcParagraphsPositionsCore(AFrom + 1, ADeltaLength, ADeltaRunIndex)
  else
  begin
    FLastShiftedParagraphIndex := AFrom;
    FLastDeltaLength := FLastDeltaLength + ADeltaLength;
    FLastDeltaRunIndex := FLastDeltaRunIndex + ADeltaRunIndex;
  end;
end;

procedure TdxSimplePieceTable.RecalcParagraphsPositionsCore(AFrom: TdxParagraphIndex; ADeltaLength, ADeltaRunIndex: Integer);
var
  ATo: TdxParagraphIndex;
begin
  ATo := Paragraphs.Count - 1;
  RecalcParagraphsPositionsCore(AFrom, ATo, ADeltaLength, ADeltaRunIndex);
end;

procedure TdxSimplePieceTable.RecalcParagraphsPositionsCore(AFrom: TdxParagraphIndex; ATo: TdxParagraphIndex; ADeltaLength, ADeltaRunIndex: Integer);
begin
  Paragraphs.RecalcParagraphsPositionsCore(AFrom, ATo, ADeltaLength, ADeltaRunIndex);
end;

procedure TdxSimplePieceTable.RecalcFieldPosition(AField: TdxField; ARunIndex: TdxRunIndex; ADeltaRunIndex: Integer);
begin
  if AField.LastRunIndex < ARunIndex then
    Exit;
  if (AField.Code.Start < ARunIndex) and (AField.Code.&End >= ARunIndex) then
  begin
    AField.Code.ShiftEndRunIndex(ADeltaRunIndex);
    AField.&Result.ShiftRunIndex(ADeltaRunIndex);
    Fields.ClearCounters;
  end
  else
    if (AField.&Result.Start <= ARunIndex) and (AField.&Result.&End >= ARunIndex) then
      AField.&Result.ShiftEndRunIndex(ADeltaRunIndex)
    else
    begin
      AField.Code.ShiftRunIndex(ADeltaRunIndex);
      AField.&Result.ShiftRunIndex(ADeltaRunIndex);
    end;
end;

procedure TdxSimplePieceTable.RecalcFieldsPositions(ARunIndex: TdxRunIndex; ADeltaRunIndex: Integer);
var
  ACount: Integer;
  I: Integer;
begin
  ACount := Fields.Count;
  if (ACount = 0) or (Fields.Last.LastRunIndex < ARunIndex) then
    Exit;
  for I := 0 to ACount - 1 do
    RecalcFieldPosition(Fields[I], ARunIndex, ADeltaRunIndex);
end;

procedure TdxSimplePieceTable.RecalcFieldsIndices(AFrom, ADeltaIndex: Integer);
var
  I, ACount: Integer;
begin
  ACount := Fields.Count;
  for I := AFrom to ACount - 1 do
    Fields[I].Index := Fields[I].Index + ADeltaIndex;
end;

procedure TdxSimplePieceTable.ChangeFieldCode(AField: TdxField; const ACode: string);
var
  ALength: Integer;
  AIsCodeView: Boolean;
  AStartCode, AEndCode: TdxDocumentModelPosition;
begin
  AIsCodeView := AField.IsCodeView;
  AField.IsCodeView := True;
  try
    AStartCode := TdxDocumentModelPosition.FromRunEnd(Self, AField.Code.Start);
    AEndCode := TdxDocumentModelPosition.FromRunStart(Self, AField.Code.&End);
    ALength := AEndCode.LogPosition - AStartCode.LogPosition;
    ReplaceText(AStartCode.LogPosition, ALength, ACode);
  finally
    AField.IsCodeView := AIsCodeView;
  end;
end;

procedure TdxSimplePieceTable.ChangeFieldResult(AField: TdxField; const AResult: string);
var
  AIsCodeView: Boolean;
  AStart, AEnd: TdxDocumentLogPosition;
begin
  AIsCodeView := AField.IsCodeView;
  AField.IsCodeView := False;
  try
    AStart := TdxDocumentModelPosition.FromRunStart(Self, AField.Result.Start).LogPosition;
    AEnd := TdxDocumentModelPosition.FromRunStart(Self, AField.Result.&End).LogPosition;
    ReplaceText(AStart, AEnd - AStart, AResult);
  finally
    AField.IsCodeView := AIsCodeView;
  end;
end;

function TdxSimplePieceTable.CreateField(AStartCode, AEndCode: TdxDocumentLogPosition; AResultLength: Integer;
  AForceVisible: Boolean): TdxField;
var
  ACommand: TdxPieceTableCreateFieldWithResultCommand;
begin
  ACommand := TdxPieceTableCreateFieldWithResultCommand.Create(Self, AStartCode, AEndCode, AResultLength, AForceVisible);
  try
    ACommand.Execute;
    Result := ACommand.InsertedField;
  finally
    ACommand.Free;
  end;
end;

function TdxSimplePieceTable.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := TdxSimpleDocumentModel(inherited DocumentModel);
end;

function TdxSimplePieceTable.GetLastInsertedRunInfo: TdxLastInsertedRunInfo;
begin
  Result := DocumentModel.GetLastInsertedRunInfo(Self);
end;

function TdxSimplePieceTable.GetLastInsertedInlinePictureRunInfo: TdxLastInsertedInlinePictureRunInfo;
begin
  Result := DocumentModel.GetLastInsertedInlinePictureRunInfo(Self);
end;

function TdxSimplePieceTable.GetLastInsertedSeparatorRunInfo: TdxLastInsertedSeparatorRunInfo;
begin
  Result := DocumentModel.GetLastInsertedSeparatorRunInfo(Self);
end;

function TdxSimplePieceTable.GetParagraphs: TdxSimpleParagraphCollection;
begin
  Result := TdxSimpleParagraphCollection(inherited Paragraphs);
end;

function TdxSimplePieceTable.GetRuns: TdxTextRunCollection;
begin
  Result := TdxTextRunCollection(inherited Runs);
end;

function TdxSimplePieceTable.GetFirstSplitChar(AFontCharacterSet: TdxFontCharacterSet; const AText: string): Integer;
var
  I, ALength: Integer;
begin
  Result := -1;
  ALength := Length(AText);
  for I := 1 to ALength do
    if not AFontCharacterSet.ContainsChar(AText[I]) then
    begin
      Result := I - 1;
      Break;
    end;
end;

procedure TdxSimplePieceTable.ParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
  AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
  if not DocumentModel.DeferredChanges.IsSetContentMode or DocumentModel.ForceNotifyStructureChanged then
    ParagraphInsertedCore(ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged,
      AActualParagraphIndex, AHistoryNotificationId);
  DoParagraphInserted(ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged,
    AActualParagraphIndex, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(DocumentModel, Self, ASectionIndex,
    AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
end;

procedure TdxSimplePieceTable.DoParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
  AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxSimplePieceTable.ParagraphInsertedCore(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
  AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxSimplePieceTable.ParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  if not DocumentModel.DeferredChanges.IsSetContentMode or DocumentModel.ForceNotifyStructureChanged then
    ParagraphRemovedCore(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  DoParagraphRemoved(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(DocumentModel, Self, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxSimplePieceTable.DoParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxSimplePieceTable.ParagraphRemovedCore(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxSimplePieceTable.ParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  if not DocumentModel.DeferredChanges.IsSetContentMode or DocumentModel.ForceNotifyStructureChanged then
    ParagraphMergedCore(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  DoParagraphMerged(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(DocumentModel, Self, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxSimplePieceTable.DoParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxSimplePieceTable.ParagraphMergedCore(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxSimplePieceTable.RunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer);
begin
  Assert(ALength > 0);
  RecalcParagraphsPositions(AParagraphIndex, ALength, 1);
  RecalcFieldsPositions(ANewRunIndex, 1);
  if not DocumentModel.DeferredChanges.IsSetContentMode or DocumentModel.ForceNotifyStructureChanged then
    RunInsertedCore(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
  DoRunInserted(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(DocumentModel, Self, AParagraphIndex, ANewRunIndex,
    ALength, AHistoryNotificationId);
end;

procedure TdxSimplePieceTable.DoRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer);
begin
end;

procedure TdxSimplePieceTable.RunInsertedCore(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
end;

procedure TdxSimplePieceTable.RunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
  Assert(ALength > 0);
  RecalcParagraphsPositions(AParagraphIndex, -ALength, -1);
  RecalcFieldsPositions(ARunIndex, -1);
  if not DocumentModel.DeferredChanges.IsSetContentMode or DocumentModel.ForceNotifyStructureChanged then
    RunRemovedCore(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
  DoRunRemoved(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(DocumentModel, Self, AParagraphIndex, ARunIndex,
    ALength, AHistoryNotificationId);
end;

procedure TdxSimplePieceTable.DoRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
end;

procedure TdxSimplePieceTable.RunRemovedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
end;

procedure TdxSimplePieceTable.RunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  RecalcParagraphsPositions(AParagraphIndex, ADeltaRunLength, 0);
  TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(DocumentModel, Self, AParagraphIndex, ARunIndex, ADeltaRunLength);
  if not DocumentModel.DeferredChanges.IsSetContentMode or DocumentModel.ForceNotifyStructureChanged then
    RunMergedCore(AParagraphIndex, ARunIndex, ADeltaRunLength);
  DoRunMerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
  Assert(Runs[ARunIndex].Paragraph.Index = AParagraphIndex);
end;

procedure TdxSimplePieceTable.DoRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
end;

procedure TdxSimplePieceTable.RunMergedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
end;

procedure TdxSimplePieceTable.RunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  RecalcParagraphsPositions(AParagraphIndex, ADeltaRunLength, 0);
  TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(DocumentModel, Self, AParagraphIndex,
    ARunIndex, ADeltaRunLength);
  if not DocumentModel.DeferredChanges.IsSetContentMode or DocumentModel.ForceNotifyStructureChanged then
    RunUnmergedCore(AParagraphIndex, ARunIndex, ADeltaRunLength);
  DoRunUnmerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
  Assert(Runs[ARunIndex].Paragraph.Index = AParagraphIndex);
end;

procedure TdxSimplePieceTable.DoRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
end;

procedure TdxSimplePieceTable.RunUnmergedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
end;

procedure TdxSimplePieceTable.RunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
  Assert(ASplitOffset > 0);
  RecalcParagraphsPositions(AParagraphIndex, 0, 1);
  RecalcFieldsPositions(ARunIndex, 1);
  if not DocumentModel.DeferredChanges.IsSetContentMode or DocumentModel.ForceNotifyStructureChanged then
    RunSplitCore(AParagraphIndex, ARunIndex, ASplitOffset);
  DoRunSplit(AParagraphIndex, ARunIndex, ASplitOffset);
  TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(DocumentModel, Self, AParagraphIndex, ARunIndex, ASplitOffset);
end;

procedure TdxSimplePieceTable.DoRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
end;

procedure TdxSimplePieceTable.RunSplitCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
end;

procedure TdxSimplePieceTable.RunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset,
  ATailRunLength: Integer);
begin
  Assert(ASplitOffset > 0);
  Assert(ATailRunLength > 0);
  RecalcParagraphsPositions(AParagraphIndex, 0, -1);
  RecalcFieldsPositions(AJoinedRunIndex, -1);
  if not DocumentModel.DeferredChanges.IsSetContentMode or DocumentModel.ForceNotifyStructureChanged then
    RunJoinedCore(AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
  DoRunJoined(AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
  TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(DocumentModel, Self, AParagraphIndex, AJoinedRunIndex,
    ASplitOffset, ATailRunLength);
end;

procedure TdxSimplePieceTable.DoRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset,
  ATailRunLength: Integer);
begin
end;

procedure TdxSimplePieceTable.RunJoinedCore(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset,
  ATailRunLength: Integer);
begin
end;

procedure TdxSimplePieceTable.FieldInserted(AFieldIndex: Integer);
begin
  RecalcFieldsIndices(AFieldIndex + 1, 1);
  HyperlinkInfos.RecalcKeys(AFieldIndex, 1);
  DoFieldInserted(AFieldIndex);
  TdxDocumentModelStructureChangedNotifier.NotifyFieldInserted(DocumentModel, Self, AFieldIndex);
end;

procedure TdxSimplePieceTable.DoFieldInserted(AFieldIndex: Integer);
begin
end;

procedure TdxSimplePieceTable.FieldRemoved(AFieldIndex: Integer);
begin
  RecalcFieldsIndices(AFieldIndex, -1);
  HyperlinkInfos.RecalcKeys(AFieldIndex, -1);
  DoFieldRemoved(AFieldIndex);
  TdxDocumentModelStructureChangedNotifier.NotifyFieldRemoved(DocumentModel, Self, AFieldIndex);
end;

procedure TdxSimplePieceTable.DoFieldRemoved(AFieldIndex: Integer);
begin
end;

procedure TdxSimplePieceTable.BeginMultipleRunSplit;
begin
  Inc(FMultipleRunSplitCount);
  if FMultipleRunSplitCount = 1 then
  begin
    FLastShiftedParagraphIndex := -1;
    FLastDeltaLength := 0;
    FLastDeltaRunIndex := 0;
  end;
  Paragraphs.OnBeginMultipleRunSplit;
  TdxDocumentModelStructureChangedNotifier.NotifyBeginMultipleRunSplit(DocumentModel, Self);
end;

procedure TdxSimplePieceTable.EndMultipleRunSplit;
begin
  Dec(FMultipleRunSplitCount);
  if FMultipleRunSplitCount = 0 then
    RecalcParagraphsPositionsCore(FLastShiftedParagraphIndex + 1, FLastDeltaLength, FLastDeltaRunIndex);
  Paragraphs.OnEndMultipleRunSplit;
  TdxDocumentModelStructureChangedNotifier.NotifyEndMultipleRunSplit(DocumentModel, Self);
end;

procedure TdxSimplePieceTable.OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
  AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
  Assert(Self = APieceTable);
  ParagraphInserted(ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex,
    AHistoryNotificationId);
end;

procedure TdxSimplePieceTable.OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
	Assert(Self = APieceTable);
	ParagraphRemoved(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxSimplePieceTable.OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  Assert(Self = APieceTable);
  ParagraphMerged(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxSimplePieceTable.OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
begin
  Assert(Self = APieceTable);
  RunInserted(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxSimplePieceTable.OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
  Assert(Self = APieceTable);
  RunRemoved(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxSimplePieceTable.OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ASplitOffset: Integer);
begin
  Assert(Self = APieceTable);
  RunSplit(AParagraphIndex, ARunIndex, ASplitOffset);
end;

procedure TdxSimplePieceTable.OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer);
begin
  Assert(Self = APieceTable);
  RunJoined(AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
end;

procedure TdxSimplePieceTable.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ADeltaRunLength: Integer);
begin
  Assert(Self = APieceTable);
  RunMerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxSimplePieceTable.OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  Assert(Self = APieceTable);
  RunUnmerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxSimplePieceTable.OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  Assert(Self = APieceTable);
  FieldRemoved(AFieldIndex);
end;

procedure TdxSimplePieceTable.OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  Assert(Self = APieceTable);
  FieldInserted(AFieldIndex);
end;

procedure TdxSimplePieceTable.OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
  Assert(Self = APieceTable);
  BeginMultipleRunSplit;
end;

procedure TdxSimplePieceTable.OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
  Assert(Self = APieceTable);
  EndMultipleRunSplit;
end;

{ TdxSimpleSelection }

function TdxSimpleSelection.GetLength: Integer;
begin
  Result := 0;
end;

procedure TdxSimpleSelection.ClearMultiSelection(AClearFrom: Integer = 0);
begin
end;

function TdxSimpleSelection.GetSelectionPersistentInfo: TdxSelectionPersistentInfo;
begin
  Result := TdxSelectionPersistentInfo.Create(PieceTable);
  Result.Start := Start;
  Result.&End := &End;
end;

procedure TdxSimpleSelection.RestoreSelection(AInfo: TdxSelectionPersistentInfo);
begin
  BeginUpdate;
  try
    ClearMultiSelection;
    Start := AInfo.Start;
    &End := AInfo.&End;
  finally
    EndUpdate;
  end;
end;

function TdxSimpleSelection.GetStart: TdxDocumentLogPosition;
begin
  Result := 0;
end;

procedure TdxSimpleSelection.SetStart(const AValue: TdxDocumentLogPosition);
begin
end;

function TdxSimpleSelection.GetNormalizedStart: TdxDocumentLogPosition;
begin
  Result := 0;
end;

function TdxSimpleSelection.GetNormalizedEnd: TdxDocumentLogPosition;
begin
  Result := 0;
end;

function TdxSimpleSelection.GetEnd: TdxDocumentLogPosition;
begin
  Result := 0;
end;

procedure TdxSimpleSelection.SetEnd(const AValue: TdxDocumentLogPosition);
begin
end;

{ TdxSortedRunIndexCollection }

constructor TdxSortedRunIndexCollection.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  Assert(APieceTable <> nil, 'APieceTable = nil');
  FPieceTable := APieceTable;
  FIndices := TdxIntegerList.Create;
end;

destructor TdxSortedRunIndexCollection.Destroy;
begin
  FreeAndNil(FIndices);
  inherited Destroy;
end;

procedure TdxSortedRunIndexCollection.Add(ARunIndex: TdxRunIndex);
var
  AIndex: Integer;
begin
  if not FIndices.BinarySearch(ARunIndex, AIndex) then
    FIndices.Insert(AIndex, ARunIndex);
end;

function TdxSortedRunIndexCollection.GetCount: Integer;
begin
  Result := FIndices.Count;
end;

function TdxSortedRunIndexCollection.GetItem(Index: Integer): TdxRunIndex;
begin
  Result := FIndices[Index];
end;

procedure TdxSortedRunIndexCollection.OnParagraphInserted(APieceTable: TdxCustomPieceTable;
  ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex;
  AHistoryNotificationId: Integer);
begin
end;

procedure TdxSortedRunIndexCollection.OnParagraphRemoved(APieceTable: TdxCustomPieceTable;
  ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  AHistoryNotificationId: Integer);
begin
end;

procedure TdxSortedRunIndexCollection.OnParagraphMerged(APieceTable: TdxCustomPieceTable;
  ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  AHistoryNotificationId: Integer);
begin
end;

procedure TdxSortedRunIndexCollection.OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
var
  AIndex: Integer;
begin
  Assert(FPieceTable = APieceTable, 'FPieceTable <> APieceTable');
  FIndices.BinarySearch(ANewRunIndex, AIndex);
  ShiftRunIndex(AIndex, 1);
end;

procedure TdxSortedRunIndexCollection.OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
var
  AIndex: Integer;
begin
  Assert(FPieceTable = APieceTable, 'FPieceTable <> APieceTable');
  if FIndices.BinarySearch(ARunIndex, AIndex) then
    FIndices.Delete(AIndex);
  ShiftRunIndex(AIndex, -1);
end;

procedure TdxSortedRunIndexCollection.OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ASplitOffset: Integer);
var
  AIndex: Integer;
begin
  Assert(FPieceTable = APieceTable, 'FPieceTable <> APieceTable');
  if FIndices.BinarySearch(ARunIndex, AIndex) then
  begin
    FIndices.Insert(AIndex + 1, ARunIndex + 1);
    Inc(AIndex, 2);
  end;
  ShiftRunIndex(AIndex, 1);
end;

procedure TdxSortedRunIndexCollection.OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer);
var
  AIndex: Integer;
begin
  Assert(FPieceTable = APieceTable, 'FPieceTable <> APieceTable');
  if FIndices.BinarySearch(AJoinedRunIndex, AIndex) then
  begin
    Inc(AIndex);
    if (AIndex < FIndices.Count) and (FIndices[AIndex] = (AJoinedRunIndex + 1)) then
      FIndices.Delete(AIndex);
  end;
  ShiftRunIndex(AIndex, -1);
end;

procedure TdxSortedRunIndexCollection.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
end;

procedure TdxSortedRunIndexCollection.OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
end;

procedure TdxSortedRunIndexCollection.OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
end;

procedure TdxSortedRunIndexCollection.OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
end;

procedure TdxSortedRunIndexCollection.OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxSortedRunIndexCollection.OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxSortedRunIndexCollection.ShiftRunIndex(AStartIndex: Integer; ADelta: Integer);
var
  I: Integer;
  ACount: Integer;
begin
  ACount := FIndices.Count;
  for I := AStartIndex to ACount - 1 do
    FIndices[I] := FIndices[I] + ADelta;
end;

{ TdxUnsafeDocumentModelEditor }

constructor TdxUnsafeDocumentModelEditor.Create(ADocumentModel: TdxSimpleDocumentModel);
begin
  inherited Create;
  Assert(ADocumentModel <> nil);
  FDocumentModel := ADocumentModel;
end;

procedure TdxUnsafeDocumentModelEditor.DeleteAllRunsInParagraph(APieceTable: TdxSimplePieceTable;
  AParagraphIndex: TdxParagraphIndex);
var
  AParagraphs: TdxSimpleParagraphCollection;
  AItem: TdxSimpleTextRunsDeletedHistoryItem;
begin
  AParagraphs := APieceTable.Paragraphs;
  AItem := TdxSimpleTextRunsDeletedHistoryItem(APieceTable.CreateTextRunsDeletedHistoryItem);
  AItem.ParagraphIndex := AParagraphIndex;
  AItem.RunIndex := AParagraphs[AParagraphIndex].FirstRunIndex;
  AItem.DeletedRunCount := AParagraphs[AParagraphIndex].LastRunIndex - AParagraphs[AParagraphIndex].FirstRunIndex + 1;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxUnsafeDocumentModelEditor.DeleteParagraphs(APieceTable: TdxSimplePieceTable;
  AStartParagraphIndex: TdxParagraphIndex; ACount: Integer; ACell: TdxCustomTableCell);
var
  AItem: TdxParagraphsDeletedHistoryItem;
begin
  AItem := TdxParagraphsDeletedHistoryItem.Create(APieceTable);
  AItem.SectionIndex := APieceTable.LookupSectionIndexByParagraphIndex(AStartParagraphIndex);
  AItem.ParagraphIndex := AStartParagraphIndex;
  AItem.DeletedParagraphsCount := ACount;
  AItem.SetTableCell(ACell);
  AItem.Execute;
  DocumentModel.History.Add(AItem);
end;

procedure TdxUnsafeDocumentModelEditor.DeleteRuns(APieceTable: TdxSimplePieceTable; AStartRunIndex: TdxRunIndex;
  ACount: Integer);
var
  AItem: TdxSimpleTextRunsDeletedHistoryItem;
begin
  AItem := TdxSimpleTextRunsDeletedHistoryItem(APieceTable.CreateTextRunsDeletedHistoryItem);
  AItem.ParagraphIndex := APieceTable.Runs[AStartRunIndex].Paragraph.Index;
  AItem.RunIndex := AStartRunIndex;
  AItem.DeletedRunCount := ACount;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxUnsafeDocumentModelEditor.DeleteSections(AStartSectionIndex: TdxSectionIndex; ACount: Integer);
var
  AItem: TdxSectionsDeletedHistoryItem;
begin
  AItem := TdxSectionsDeletedHistoryItem.Create(DocumentModel);
  AItem.SectionIndex := AStartSectionIndex;
  AItem.DeletedSectionsCount := ACount;
  AItem.Execute;
  DocumentModel.History.Add(AItem);
end;

procedure TdxUnsafeDocumentModelEditor.InsertFirstParagraph(APieceTable: TdxSimplePieceTable);
var
  AParagraph: TdxSimpleParagraph;
  AParagraphMarkRun: TdxParagraphRun;
begin
  APieceTable.TextBuffer.Append(TdxCharacters.ParagraphMark);
  AParagraph := APieceTable.CreateParagraph;

  AParagraph.Length := 1;
  APieceTable.Paragraphs.Add(AParagraph);
  AParagraph.RelativeFirstRunIndex := 0;
  AParagraph.RelativeLastRunIndex := 0;
  AParagraph.RelativeLogPosition := 0;
  AParagraphMarkRun := TdxParagraphRun.Create(AParagraph);
  AParagraphMarkRun.StartIndex := APieceTable.TextBuffer.Length - 1;
  APieceTable.Runs.Add(AParagraphMarkRun);
end;

procedure TdxUnsafeDocumentModelEditor.InsertFirstSection;
var
  ASection: TdxCustomSection;
begin
  ASection := DocumentModel.CreateSection;
  ASection.FirstParagraphIndex := 0;
  ASection.LastParagraphIndex := 0;
  DocumentModel.Sections.Add(ASection);
end;

procedure TdxUnsafeDocumentModelEditor.MergeParagraphs(APieceTable: TdxSimplePieceTable; AFirstParagraph,
  ASecondParagraph: TdxSimpleParagraph; AUseFirstParagraphStyle: Boolean; ACell: TdxCustomTableCell);
var
  AItem: TdxMergeParagraphsHistoryItem;
begin
  AItem := TdxMergeParagraphsHistoryItem.Create(APieceTable);
  AItem.StartParagraph := AFirstParagraph;
  AItem.EndParagraph := ASecondParagraph;
  AItem.UseFirstParagraphStyle := AUseFirstParagraphStyle;
  AItem.SetTableCell(ACell);
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxUnsafeDocumentModelEditor.ReplaceParagraphRunWithSectionRun(APieceTable: TdxSimplePieceTable;
  ASectionRun: TdxTextRunBase; ARunIndex: TdxRunIndex);
var
  AParagraphRun: TdxParagraphRun;
begin
  AParagraphRun := Safe<TdxParagraphRun>.Cast(APieceTable.Runs[ARunIndex]);
  if AParagraphRun <> nil then
  begin
    APieceTable.Runs[ARunIndex] := ASectionRun;
    APieceTable.TextBuffer[AParagraphRun.StartIndex] := TdxCharacters.SectionMark;
  end;
end;

procedure TdxUnsafeDocumentModelEditor.ReplaceSectionRunWithParagraphRun(APieceTable: TdxSimplePieceTable;
  ASectionRun: TdxTextRunBase; ARunIndex: TdxRunIndex);
var
  AParagraphRun: TdxParagraphRun;
begin
  AParagraphRun := TdxSectionRun(ASectionRun).CreateParagraphRun;
  APieceTable.Runs[ARunIndex] := AParagraphRun;
  APieceTable.TextBuffer[AParagraphRun.StartIndex] := TdxCharacters.ParagraphMark;
end;

{ TdxSimpleDocumentModelDeferredChanges }

constructor TdxSimpleDocumentModelDeferredChanges.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel);
  FRunIndicesForSplit := TObjectDictionary<TdxCustomPieceTable, TdxSortedRunIndexCollection>.Create([doOwnsValues]);
end;

destructor TdxSimpleDocumentModelDeferredChanges.Destroy;
begin
  FreeAndNil(FRunIndicesForSplit);
  inherited Destroy;
end;

function TdxSimpleDocumentModelDeferredChanges.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := TdxSimpleDocumentModel(inherited DocumentModel);
end;

function TdxSimpleDocumentModelDeferredChanges.GetSelectionLengthChanged: Boolean;
begin
  Result := FOriginalSelectionLength <> DocumentModel.Selection.Length
end;

function TdxSimpleDocumentModelDeferredChanges.GetRunIndicesForSplit(APieceTable: TdxCustomPieceTable): TdxSortedRunIndexCollection;
begin
  Result := nil;
  if not FRunIndicesForSplit.TryGetValue(APieceTable, Result) then
  begin
    Result := TdxSortedRunIndexCollection.Create(APieceTable);
    FRunIndicesForSplit.Add(APieceTable, Result);
  end;
end;

procedure TdxSimpleDocumentModelDeferredChanges.ResetSelectionChanged;
begin
  FOriginalSelectionLength := DocumentModel.Selection.Length;
end;

{ TdxSimpleSection }

procedure TdxSimpleSection.SubscribeInnerObjectsEvents;
begin
end;

procedure TdxSimpleSection.UnsubscribeInnerObjectsEvents;
begin
end;

procedure TdxSimpleSection.SubscribeHeadersFootersEvents;
begin
end;

procedure TdxSimpleSection.UnsubscribeHeadersFootersEvents;
begin
end;

{ TdxSimpleDocumentModel }

function TdxSimpleDocumentModel.ValidateActivePieceTable: Boolean;
var
  I: Integer;
  APieceTables: TdxFastList;
begin
  APieceTables := GetPieceTables(False);
  try
    for I := 0 to APieceTables.Count - 1 do
      if GetActivePieceTableCore = APieceTables[I] then
        Exit(True);
    Result := False;
  finally
    APieceTables.Free;
  end;
end;

function TdxSimpleDocumentModel.CreateSection: TdxCustomSection;
begin
  Result := TdxSimpleSection.Create(Self);
end;

function TdxSimpleDocumentModel.CreateMainContentType: TdxSimpleMainContentType;
begin
  Result := TdxSimpleMainContentType.Create(Self);
end;

procedure TdxSimpleDocumentModel.CreateOptions;
begin
  inherited CreateOptions;
  FFormattingMarkVisibilityOptions := CreateFormattingMarkVisibilityOptions;
end;

function TdxSimpleDocumentModel.GetPieceTables(AIncludeUnreferenced: Boolean): TdxFastList;
begin
  Result := inherited GetPieceTables(AIncludeUnreferenced);
  MainPieceTable.AddPieceTables(Result, AIncludeUnreferenced);
end;

function TdxSimpleDocumentModel.GetNumberingListIndex(ATarget: TdxCustomDocumentModel; ASourceListIndex, AMaxNumberingListIndex: TdxNumberingListIndex): TdxNumberingListIndex;
begin
  Result := -1;
end;

procedure TdxSimpleDocumentModel.ResetDocumentFormattingCaches(AResetFormattingCacheType: TdxResetFormattingCacheType);
var
  I: Integer;
  APieceTable: TdxSimplePieceTable;
  APieceTables: TdxFastList;
begin
  APieceTables := GetPieceTables(True);
  try
    for I := 0 to APieceTables.Count - 1 do
    begin
      APieceTable := TdxSimplePieceTable(APieceTables[I]);
      APieceTable.ResetFormattingCaches(AResetFormattingCacheType);
    end;
  finally
    APieceTables.Free;
  end;
end;

procedure TdxSimpleDocumentModel.ResetMerging;
begin
  FLastInsertedRunInfo.Reset(nil);
end;

procedure TdxSimpleDocumentModel.ResetCharacterFormatting(ACharacterFormatting: TdxCharacterFormattingBase);
begin
  ACharacterFormatting.ReplaceInfo(Cache.CharacterFormattingCache.DefaultItem.Info,
    TdxCharacterFormattingOptions.EmptyCharacterFormattingOption);
end;

function TdxSimpleDocumentModel.CreateEmptyCharacterFormatting: TdxCharacterFormattingBase;
begin
  Result := TdxCharacterFormattingBase.Create(MainPart, Self,
    Cache.CharacterFormattingInfoCache.DefaultItem,
    TdxCharacterFormattingOptions.EmptyCharacterFormattingOption);
end;

function TdxSimpleDocumentModel.CreateEmptyParagraphFormatting: TdxParagraphFormattingBase;
begin
  Result := TdxParagraphFormattingBase.Create(MainPart, Self,
    Cache.ParagraphFormattingCache.DefaultInfo,
    TdxParagraphFormattingOptions.EmptyParagraphFormattingOption);
end;

procedure TdxSimpleDocumentModel.ResetParagraphFormatting(AParagraphFormatting: TdxParagraphFormattingBase);
begin
  AParagraphFormatting.ReplaceInfo(Cache.ParagraphFormattingCache.DefaultInfo,
    TdxParagraphFormattingOptions.EmptyParagraphFormattingOption);
end;

procedure TdxSimpleDocumentModel.SwitchToSelectionCore(ANewSelection: TdxSimpleSelection);
begin
  UnsubscribeSelectionEvents;
  FreeAndNil(FSelection);
  FSelection := ANewSelection;
  SubscribeSelectionEvents;
end;

procedure TdxSimpleDocumentModel.OnSectionInserted(ASectionIndex: TdxSectionIndex);
begin
end;

procedure TdxSimpleDocumentModel.OnSectionRemoved(ASectionIndex: TdxSectionIndex);
begin
end;

function TdxSimpleDocumentModel.CreateImage(ANativeImage: TdxOfficeImage): TdxOfficeImageReference;
var
  AImage: TdxOfficeImage;
  ACrc32: Integer;
begin
  ACrc32 := ANativeImage.CalculateCrc32;
  AImage := ImageCache.GetImage(ACrc32);
  if AImage = nil then
  begin
    AImage := ANativeImage;
    ImageCache.AddImage(AImage, ACrc32);
  end;
  Result := TdxOfficeImageReference.Create(ImageCache, AImage);
end;

function TdxSimpleDocumentModel.CreateImage(AStream: TStream): TdxOfficeImageReference;
var
  AImage: TdxOfficeImage;
begin
  Result := nil;
  AImage := TdxOfficeImage.CreateFromStream(AStream);
  try
    Result := CreateImage(AImage);
  finally
    if Result.Image <> AImage then
      AImage.Free;
  end;
end;

function TdxSimpleDocumentModel.CreateDeferredChanges: TdxCustomDocumentModelDeferredChanges;
begin
  Result := TdxSimpleDocumentModelDeferredChanges.Create(Self);
end;

function TdxSimpleDocumentModel.CreateFormattingMarkVisibilityOptions: TdxSimpleFormattingMarkVisibilityOptions;
begin
  Result := TdxSimpleFormattingMarkVisibilityOptions.Create;
end;

procedure TdxSimpleDocumentModel.CreateDocumentObjects;
begin
  inherited CreateDocumentObjects;
  FContentType := CreateMainContentType;
  FUnsafeEditor := TdxUnsafeDocumentModelEditor.Create(Self);
  FLastInsertedRunInfo := TdxLastInsertedRunInfo.Create;
  FLastInsertedInlinePictureRunInfo := TdxLastInsertedInlinePictureRunInfo.Create;
  FLastInsertedSeparatorRunInfo := TdxLastInsertedSeparatorRunInfo.Create;
  FLastInsertedRunInfo.PieceTable := MainPieceTable;
  FLastInsertedInlinePictureRunInfo.PieceTable := MainPieceTable;
  FLastInsertedSeparatorRunInfo.PieceTable := MainPieceTable;
  FImageCache := TdxImageCache.Create;
end;

function TdxSimpleDocumentModel.CreateSectionCollection: TdxCustomSectionCollection;
begin
  Result := TdxSimpleSectionCollection.Create;
end;

procedure TdxSimpleDocumentModel.ClearDocumentStyles;
begin
  FCharacterStyles.Free;
  FCharacterStyles := TdxCharacterStyleCollection.Create(Self);
  FParagraphStyles.Free;
  FParagraphStyles := TdxParagraphStyleCollection.Create(Self);
  FStyleLinkManager.Free;
  FStyleLinkManager := TdxStyleLinkManager.Create(Self);
end;

procedure TdxSimpleDocumentModel.ClearDocumentProperties;
begin
end;

procedure TdxSimpleDocumentModel.ClearCore;
begin
  inherited ClearCore;
  if FDefaultCharacterProperties <> nil then
  begin
    UnsubscribeCharacterPropertiesEvents;
    FreeAndNil(FDefaultCharacterProperties);
  end;
  if FDefaultParagraphProperties <> nil then
  begin
    UnsubscribeParagraphPropertiesEvents;
    FreeAndNil(FDefaultParagraphProperties);
  end;
  if FSelection <> nil then
  begin
    UnsubscribeSelectionEvents;
    FreeAndNil(FSelection);
  end;
  FreeAndNil(FParagraphStyles);
  FreeAndNil(FCharacterStyles);
end;

procedure TdxSimpleDocumentModel.ClearDocumentCore;
begin
  inherited ClearDocumentCore;
  ClearDocumentDefaultProperties;
  ClearDocumentProperties;
  ClearDocumentStyles;
end;

procedure TdxSimpleDocumentModel.DestroyDocumentObjects;
begin
  FreeAndNil(FUnsafeEditor);
  FreeAndNil(FParagraphStyles);
  FreeAndNil(FCharacterStyles);
  FreeAndNil(FStyleLinkManager);

  FreeAndNil(FLastInsertedRunInfo);
  FreeAndNil(FLastInsertedInlinePictureRunInfo);
  FreeAndNil(FLastInsertedSeparatorRunInfo);
  FreeAndNil(FImageCache);
  FreeAndNil(FFormattingMarkVisibilityOptions);
  inherited DestroyDocumentObjects;
end;

function TdxSimpleDocumentModel.GetIsDocumentProtectionEnabled: Boolean;
begin
  Result := False;
end;

procedure TdxSimpleDocumentModel.ClearDocumentContent;
begin
  PieceTable.Clear;
  inherited ClearDocumentContent;
  ResetMerging;
  ImageCache.Clear;
end;

procedure TdxSimpleDocumentModel.ClearDocumentDefaultPropertiesCore;
begin
  FDefaultParagraphProperties.Free;
  FDefaultParagraphProperties := TdxParagraphProperties.Create(Self);
  FDefaultCharacterProperties.Free;
  FDefaultCharacterProperties := TdxCharacterProperties.Create(Self);
end;

procedure TdxSimpleDocumentModel.ClearDocumentDefaultProperties;
begin
  ClearDocumentDefaultPropertiesCore;
  InitializeDefaultProperties;
end;

procedure TdxSimpleDocumentModel.InitializeDefaultProperties;
begin
  FDefaultParagraphProperties.SetIndexInitial(TdxParagraphFormattingCache.RootParagraphFormattingIndex);
  FDefaultCharacterProperties.SetIndexInitial(TdxCharacterFormattingCache.RootCharacterFormattingIndex);
end;

function TdxSimpleDocumentModel.GetLastInsertedRunInfo(APieceTable: TdxSimplePieceTable): TdxLastInsertedRunInfo;
begin
  if FLastInsertedRunInfo.PieceTable <> APieceTable then
    FLastInsertedRunInfo.Reset(APieceTable);
  Result := FLastInsertedRunInfo;
end;

function TdxSimpleDocumentModel.GetLastInsertedInlinePictureRunInfo(APieceTable: TdxSimplePieceTable): TdxLastInsertedInlinePictureRunInfo;
begin
  if APieceTable <> FLastInsertedInlinePictureRunInfo.PieceTable then
    FLastInsertedInlinePictureRunInfo.Reset(APieceTable);
  Result := FLastInsertedInlinePictureRunInfo;
end;

function TdxSimpleDocumentModel.GetLastInsertedSeparatorRunInfo(APieceTable: TdxSimplePieceTable): TdxLastInsertedSeparatorRunInfo;
begin
  if APieceTable <> FLastInsertedSeparatorRunInfo.PieceTable then
    FLastInsertedSeparatorRunInfo.Reset(APieceTable);
  Result := FLastInsertedSeparatorRunInfo;
end;

procedure TdxSimpleDocumentModel.SubscribeCharacterPropertiesEvents;
begin
  if FDefaultCharacterProperties <> nil then
    FDefaultCharacterProperties.OnObtainAffectedRange.Add(OnDefaultCharacterPropertiesObtainAffectedRange);
end;

procedure TdxSimpleDocumentModel.UnsubscribeCharacterPropertiesEvents;
begin
  if FDefaultCharacterProperties <> nil then
    FDefaultCharacterProperties.OnObtainAffectedRange.Remove(OnDefaultCharacterPropertiesObtainAffectedRange);
end;

procedure TdxSimpleDocumentModel.SubscribeParagraphPropertiesEvents;
begin
  if FDefaultParagraphProperties <> nil then
    FDefaultParagraphProperties.OnObtainAffectedRange.Add(OnDefaultParagraphPropertiesObtainAffectedRange);
end;

procedure TdxSimpleDocumentModel.UnsubscribeParagraphPropertiesEvents;
begin
  if FDefaultCharacterProperties <> nil then
    FDefaultParagraphProperties.OnObtainAffectedRange.Remove(OnDefaultParagraphPropertiesObtainAffectedRange);
end;

procedure TdxSimpleDocumentModel.SubscribeSelectionEvents;
begin
  if Selection <> nil then
    Selection.Changed.Add(OnSelectionChanged);
end;

procedure TdxSimpleDocumentModel.UnsubscribeSelectionEvents;
begin
  if Selection <> nil then
    Selection.Changed.Remove(OnSelectionChanged);
end;

procedure TdxSimpleDocumentModel.SubscribeOptionsEvents;
begin
  inherited SubscribeOptionsEvents;
  SubscribeFormattingMarkVisibilityOptions;
end;

procedure TdxSimpleDocumentModel.RaiseInnerSelectionChanged;
begin
  if not FOnInnerSelectionChanged.Empty then
    FOnInnerSelectionChanged.Invoke(Self);
end;

procedure TdxSimpleDocumentModel.RaiseSelectionChanged;
begin
  if not FOnSelectionChanged.Empty then
    FOnSelectionChanged.Invoke(Self);
end;

procedure TdxSimpleDocumentModel.OnShowHiddenTextOptionsChanged;
const
  ChangeActions = [
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
    TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
    TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
    TdxDocumentModelChangeAction.ForceResetVerticalRuler];
var
  I: Integer;
  AShowHiddenText: Boolean;
  APieceTable: TdxSimplePieceTable;
  APieceTables: TdxFastList;
begin
  BeginUpdate;
  try
    AShowHiddenText := FormattingMarkVisibilityOptions.ShowHiddenText;
    APieceTables := GetPieceTables(True);
    try
      for I := 0 to APieceTables.Count - 1 do
      begin
        APieceTable := TdxSimplePieceTable(APieceTables[I]);
        APieceTable.SetShowHiddenText(AShowHiddenText);
      end;
      MainPieceTable.ApplyChangesCore(ChangeActions, 0, MaxInt);
    finally
      APieceTables.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxSimpleDocumentModel.OnSelectionChanged(ASender: TObject; E: TdxEventArgs);
var
  AActions: TdxDocumentModelChangeActions;
begin
  RaiseInnerSelectionChanged;
  if IsUpdateLocked then
  begin
    AActions := [TdxDocumentModelChangeAction.RaiseSelectionChanged, TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
      TdxDocumentModelChangeAction.ResetSelectionLayout, TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
      TdxDocumentModelChangeAction.ForceResetVerticalRuler];
    if DeferredChanges.SelectionLengthChanged or (Selection.Length > 0) then
      AActions := AActions + [TdxDocumentModelChangeAction.Redraw];
    GetActivePieceTableCore.ApplyChangesCore(AActions, -1, -1);
  end
  else
    RaiseSelectionChanged;
end;

procedure TdxSimpleDocumentModel.BeginClearDocument;
begin
  UnsubscribeCharacterPropertiesEvents;
  UnsubscribeParagraphPropertiesEvents;
end;

procedure TdxSimpleDocumentModel.EndClearDocument;
begin
  SubscribeCharacterPropertiesEvents;
  SubscribeParagraphPropertiesEvents;
end;

function TdxSimpleDocumentModel.CreateDocumentCache: TdxCustomDocumentCache;
begin
  Result := TdxSimpleDocumentCache.Create;
end;

function TdxSimpleDocumentModel.CreateDocumentCapabilitiesOptions: TdxCustomDocumentCapabilitiesOptions;
begin
  Result := TdxSimpleDocumentCapabilitiesOptions.Create;
end;

function TdxSimpleDocumentModel.UseFontSubstitution: Boolean;
begin
  Result := False;
end;

procedure TdxSimpleDocumentModel.ResetUncheckedSpellIntervals;
var
  APieceTables: TdxFastList;
  APieceTable: TdxSimplePieceTable;
  I: Integer;
begin
  APieceTables := GetPieceTables(False);
  try
    for I := 0 to APieceTables.Count - 1 do
    begin
      APieceTable := TdxSimplePieceTable(APieceTables[I]);
      APieceTable.InitializeUncheckedInterval;
    end;
  finally
    APieceTables.Free;
  end;
end;

procedure TdxSimpleDocumentModel.ResetSpellCheck(APieceTable: TdxCustomPieceTable; AStartRunIndex, AEndRunIndex: TdxRunIndex; AFormattingOnly: Boolean);
var
  AActions: TdxDocumentModelChangeActions;
begin
  AActions := [
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetSecondaryLayout];

  if not AFormattingOnly then
    Include(AActions, TdxDocumentModelChangeAction.ResetUncheckedIntervals);

  BeginUpdate;
  try
    APieceTable.ApplyChangesCore(AActions, AStartRunIndex, AEndRunIndex);
  finally
    EndUpdate;
  end;
end;

procedure TdxSimpleDocumentModel.ResetSpellCheck(AStartRunIndex, AEndRunIndex: TdxRunIndex; AFormattingOnly: Boolean = False);
begin
  ResetSpellCheck(GetActivePieceTableCore, AStartRunIndex, AEndRunIndex, AFormattingOnly);
end;

procedure TdxSimpleDocumentModel.NotifyNumberingListParagraphAdded(AIndex: TdxNumberingListIndex);
begin
end;

procedure TdxSimpleDocumentModel.NotifyNumberingListParagraphRemoved(AIndex: TdxNumberingListIndex);
begin
end;

procedure TdxSimpleDocumentModel.RaiseHyperlinkInfoInserted(APieceTable: TdxSimplePieceTable; AFieldIndex: Integer);
begin
end;

procedure TdxSimpleDocumentModel.RaiseHyperlinkInfoDeleted(APieceTable: TdxSimplePieceTable; AFieldIndex: Integer);
begin
end;

function TdxSimpleDocumentModel.GetPropertiesContainerPieceTable: TdxCustomPieceTable;
begin
  Result := PieceTable;
end;

function TdxSimpleDocumentModel.CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(MainPieceTable, DefaultCharacterProperties);
end;

function TdxSimpleDocumentModel.CreateParagraphPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(MainPieceTable, DefaultParagraphProperties);
end;

procedure TdxSimpleDocumentModel.OnCharacterPropertiesChanged;
begin
  ResetDocumentFormattingCaches(TdxResetFormattingCacheType.Character);
end;

procedure TdxSimpleDocumentModel.OnParagraphPropertiesChanged;
begin
  ResetDocumentFormattingCaches(TdxResetFormattingCacheType.Paragraph);
end;

function TdxSimpleDocumentModel.GetCharacterStyles: TdxCharacterStyleCollection;
begin
  Result := FCharacterStyles;
end;

function TdxSimpleDocumentModel.GetDefaultCharacterProperties: TdxCharacterProperties;
begin
  Result := FDefaultCharacterProperties;
end;

function TdxSimpleDocumentModel.GetDefaultParagraphProperties: TdxParagraphProperties;
begin
  Result := FDefaultParagraphProperties;
end;

function TdxSimpleDocumentModel.GetParagraphStyles: TdxParagraphStyleCollection;
begin
  Result := FParagraphStyles;
end;

function TdxSimpleDocumentModel.GetStyleLinkManager: TdxStyleLinkManager;
begin
  Result := FStyleLinkManager;
end;

function TdxSimpleDocumentModel.GetCache: TdxSimpleDocumentCache;
begin
  Result := TdxSimpleDocumentCache(inherited Cache);
end;

function TdxSimpleDocumentModel.GetDeferredChanges: TdxSimpleDocumentModelDeferredChanges;
begin
  Result := TdxSimpleDocumentModelDeferredChanges(inherited DeferredChanges);
end;

function TdxSimpleDocumentModel.GetDocumentCapabilities: TdxSimpleDocumentCapabilitiesOptions;
begin
  Result := TdxSimpleDocumentCapabilitiesOptions(inherited DocumentCapabilities);
end;

function TdxSimpleDocumentModel.GetPieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(FContentType.PieceTable);
end;

function TdxSimpleDocumentModel.GetSections: TdxSimpleSectionCollection;
begin
  Result := TdxSimpleSectionCollection(inherited Sections);
end;

procedure TdxSimpleDocumentModel.SetDocumentCapabilities(const Value: TdxSimpleDocumentCapabilitiesOptions);
begin
  inherited DocumentCapabilities := Value;
end;

procedure TdxSimpleDocumentModel.SubscribeFormattingMarkVisibilityOptions;
begin
  if FFormattingMarkVisibilityOptions <> nil then
    FFormattingMarkVisibilityOptions.Changed.Add(OnFormattingMarkVisibilityOptionsChanged);
end;

procedure TdxSimpleDocumentModel.OnFormattingMarkVisibilityOptionsChanged(ASender: TObject; E: TdxRichEditNotificationOptionsChangedArgs);
begin
  if (TdxSimpleFormattingMarkVisibilityOptions.TAction.ShowHiddenText in E.Actions) or
      (TdxSimpleFormattingMarkVisibilityOptions.TAction.HiddenText in E.Actions)  then
    OnShowHiddenTextOptionsChanged;
end;

procedure TdxSimpleDocumentModel.OnDefaultCharacterPropertiesObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
begin
  E.Start := 0;
  E.&End := MaxInt;
end;

procedure TdxSimpleDocumentModel.OnDefaultParagraphPropertiesObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
begin
  E.Start := 0;
  E.&End := MaxInt;
end;

procedure TdxSimpleDocumentModel.OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
  AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
NotImplemented;
end;

procedure TdxSimpleDocumentModel.OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
NotImplemented;
end;

procedure TdxSimpleDocumentModel.OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
NotImplemented;
end;

procedure TdxSimpleDocumentModel.OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
begin
NotImplemented;
end;

procedure TdxSimpleDocumentModel.OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
NotImplemented;
end;

procedure TdxSimpleDocumentModel.OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ASplitOffset: Integer);
begin
NotImplemented;
end;

procedure TdxSimpleDocumentModel.OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer);
begin
NotImplemented;
end;

procedure TdxSimpleDocumentModel.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ADeltaRunLength: Integer);
begin
NotImplemented;
end;

procedure TdxSimpleDocumentModel.OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
NotImplemented;
end;

procedure TdxSimpleDocumentModel.OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
NotImplemented;
end;

procedure TdxSimpleDocumentModel.OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
NotImplemented;
end;

procedure TdxSimpleDocumentModel.OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
NotImplemented;
end;

procedure TdxSimpleDocumentModel.OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
NotImplemented;
end;

end.

