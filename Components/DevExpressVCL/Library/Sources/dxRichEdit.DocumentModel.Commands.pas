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

unit dxRichEdit.DocumentModel.Commands;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxSpellCheckerCore,

  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Options,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.WidthsContentInfo,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.History.Simple,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.InlineObjectFormatting,
  dxRichEdit.DocumentModel.Commands.Core,
  dxRichEdit.DocumentModel.Commands.Simple,
  dxRichEdit.DocumentModel.CopyManager,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableCalculator,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Selections.Core,
  dxRichEdit.DocumentModel.History.Paragraph,
  dxGenerics;

type
  TdxPieceTableInsertContentConvertedToDocumentModelCommand = class;

  { TdxPieceTableCommand }

  TdxPieceTableCommand = class abstract(TdxSimplePieceTableCommand)
  strict private
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
  public
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxPieceTableInsertFieldSymbolResultAtInputPositionCommand }

  TdxPieceTableInsertFieldSymbolResultAtInputPositionCommand = class(TdxPieceTableInsertTextAtLogPositionCommand)
  strict private
    FSymbol: Char;
  protected
    procedure ExecuteCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition; const ASymbol: Char); reintroduce;

    property Symbol: Char read FSymbol;
  end;

  { TdxDocumentModelCommand }

  TdxDocumentModelCommand = class abstract
  private
    FDocumentModel: TdxDocumentModel;
    FTransaction: TdxHistoryTransaction;
    function GetPieceTable: TdxPieceTable;
  protected
    procedure BeginExecute; virtual;
    procedure EndExecute; virtual;

    procedure ExecuteCore; virtual; abstract;
    procedure CalculateExecutionParameters; virtual; abstract;
    procedure CalculateApplyChangesParameters; virtual; abstract;
    procedure ApplyChanges; virtual; abstract;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    procedure Execute;

    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property Transaction: TdxHistoryTransaction read FTransaction;
  end;

  { TdxDocumentModelInsertObjectCommand }

  TdxDocumentModelInsertObjectCommand = class abstract (TdxDocumentModelCommand)
  private
    FParagraphIndex: TdxParagraphIndex;
    FApplyChangesParagraphIndex: TdxParagraphIndex;
  protected
    function GetChangeType: TdxDocumentModelChangeType; virtual; abstract;
    procedure CalculateExecutionParameters; override;
    procedure CalculateApplyChangesParameters; override;
    procedure ApplyChanges; override;

    function CalculateInsertionParagraphIndex: TdxParagraphIndex; virtual; abstract;

    property ChangeType: TdxDocumentModelChangeType read GetChangeType;
  public
    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex;
  end;

  { TdxDocumentModelInsertObjectAtLogPositionCommand }

  TdxDocumentModelInsertObjectAtLogPositionCommand = class abstract (TdxDocumentModelInsertObjectCommand)
  private
    FForceVisible: Boolean;
    FLogPosition: TdxDocumentLogPosition;
  protected
    function CalculateInsertionParagraphIndex: TdxParagraphIndex; override;
  public
    constructor Create(ADocumentModel: TdxDocumentModel; ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean);

    property LogPosition: TdxDocumentLogPosition read FLogPosition;
    property ForceVisible: Boolean read FForceVisible;
  end;

  { TdxDocumentModelInsertObjectAtInputPositionCommand }

  TdxDocumentModelInsertObjectAtInputPositionCommand = class abstract(TdxDocumentModelInsertObjectCommand)
  private
    FPosition: TdxInputPosition;
  protected
    function CalculateInsertionParagraphIndex: TdxParagraphIndex; override;
  public
    constructor Create(ADocumentModel: TdxDocumentModel; APosition: TdxInputPosition);

    property Position: TdxInputPosition read FPosition;
  end;

  { TdxPieceTablePasteContentCommand }

  TdxPieceTablePasteContentCommand = class abstract(TdxPieceTableCommand)
  private
    FPasteSource: IdxPasteSource;
  protected
    function GetFormat: TdxRichEditDocumentFormat; virtual; abstract;
  public
    constructor Create(APieceTable: TdxPieceTable);

    function IsDataAvailable: Boolean; virtual; abstract;

    property Format: TdxRichEditDocumentFormat read GetFormat;
    property PasteSource: IdxPasteSource read FPasteSource write FPasteSource;
  end;

  { TdxPieceTablePasteContentConvertedToDocumentModelCommandBase }

  TdxPieceTablePasteContentConvertedToDocumentModelCommandBase = class abstract(TdxPieceTablePasteContentCommand)
  private
    FCopyBetweenInternalModels: Boolean;
    FForceInsertFloatingObjectAtParagraphStart: Boolean;
    FInsertOptions: TdxInsertOptions;
    FPasteFromIE: Boolean;
  protected
    procedure CalculateExecutionParameters; override;
    procedure ExecuteCore; override;
    procedure CalculateApplyChangesParameters; override;
    procedure ApplyChanges; override;

    function CreateSourceDocumentModel(const ASizeCollection: string): TdxDocumentModel; virtual; abstract;
    function CreatePasteDocumentModelCommand(APos: TdxDocumentLogPosition; ASource: TdxDocumentModel;
      ASuppressFieldsUpdate, APasteFromIE: Boolean): TdxPieceTableInsertContentConvertedToDocumentModelCommand; virtual;
    function GetAdditionalContentString: string; virtual;
    function GetDocumentModelSingleFloatingObjectAnchorRun(AModel: TdxDocumentModel): TdxFloatingObjectAnchorRun;
    procedure OffsetNewlyInsertedFloatingObjectIfNeed(ANewRun: TdxFloatingObjectAnchorRun; AParagraph: TdxParagraph);
    procedure OffsetNewlyInsertedFloatingObject(ANewRun: TdxFloatingObjectAnchorRun; ARun: TdxFloatingObjectAnchorRun);
    procedure PasteContent(ASource: TdxDocumentModel; APos: TdxDocumentLogPosition; const ASizeCollection: string); virtual;
    procedure PasteDocumentModel(APos: TdxDocumentLogPosition; ASource: TdxDocumentModel; APasteFromIE: Boolean; ASuppressFieldsUpdate: Boolean = False);
    function PreprocessSingleFloatingObjectInsertion(ARun: TdxFloatingObjectAnchorRun): TdxParagraph;
    procedure SetSuppressStoreImageSize(ADocumentModel: TdxDocumentModel; const ASizeCollection: string); virtual;
    function ShouldOffsetNewRun(ANewRun: TdxFloatingObjectAnchorRun; ARun: TdxFloatingObjectAnchorRun): Boolean;
    function SuppressCopySectionProperties(ASource: TdxDocumentModel): Boolean; virtual;
  public
    constructor Create(APieceTable: TdxPieceTable); overload;
    constructor Create(APieceTable: TdxPieceTable; AInsertOptions: TdxInsertOptions); overload;

    property CopyBetweenInternalModels: Boolean read FCopyBetweenInternalModels write FCopyBetweenInternalModels;
    property ForceInsertFloatingObjectAtParagraphStart: Boolean read FForceInsertFloatingObjectAtParagraphStart write FForceInsertFloatingObjectAtParagraphStart;
    property InsertOptions: TdxInsertOptions read FInsertOptions;
    property PasteFromIE: Boolean read FPasteFromIE write FPasteFromIE;
  end;

  { TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBase }

  TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBase = class abstract(TdxPieceTablePasteContentConvertedToDocumentModelCommandBase)
  protected
    function CreateSourceDocumentModel(const ASizeCollection: string): TdxDocumentModel; override;
    function CreateDocumentModelFromContentString(AContent: TdxClipboardStringContent; const ASizeCollection: string): TdxDocumentModel; virtual;
    function GetContent: TdxClipboardStringContent; virtual; abstract;
    procedure PopulateDocumentModelFromContentStringCore(ADocumentModel: TdxDocumentModel; AContent: TdxClipboardStringContent; const ASizeCollection: string); virtual; abstract;
    procedure PasteContent(AContent: TdxClipboardStringContent;
      APos: TdxDocumentLogPosition; const ASizeCollection: string); reintroduce; overload; virtual;
  end;

  { TdxPieceTablePasteRtfTextCommand }

  TdxPieceTablePasteRtfTextCommand = class(TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBase)
  private
    function GetIsPasteFromIe: Boolean;
  protected
    function GetAdditionalContentString: string; override;
    function GetFormat: TdxRichEditDocumentFormat; override;
    function GetContent: TdxClipboardStringContent; override;
    procedure PopulateDocumentModelFromContentStringCore(ADocumentModel: TdxDocumentModel;
      AContent: TdxClipboardStringContent; const ASizeCollection: string); override;
    function PrepareInputStream(const AStr: string): TStream;
  public
    function IsDataAvailable: Boolean; override;
  end;

  { TdxPieceTableApplyAutoCorrectCommand }

  TdxPieceTableApplyAutoCorrectCommand = class(TdxPieceTableCommand)
  strict private
    FLength: Integer;
    FLogPosition: TdxDocumentLogPosition;
    FText: string;
    FRule: IdxSpellCheckerAutoCorrectCustomRule;
    procedure AddUndoHistoryItem;
    procedure DeleteContent;
    procedure InsertText;
  protected
    procedure ExecuteCore; override;
    procedure CalculateExecutionParameters; override;
    procedure CalculateApplyChangesParameters; override;
    procedure ApplyChanges; override;
  public
    constructor Create(APieceTable: TdxPieceTable;
      ALogPosition: TdxDocumentLogPosition; ALength: Integer; const AText: string;
      const ARule: IdxSpellCheckerAutoCorrectCustomRule); reintroduce;
  end;

  { TdxPieceTableInsertFloatingObjectAnchorAtLogPositionCommand }

  TdxPieceTableInsertFloatingObjectAnchorAtLogPositionCommand = class sealed(TdxPieceTableInsertObjectAtLogPositionCommand)
  protected
    function GetChangeType: TdxDocumentModelChangeType; override;
    procedure ExecuteCore; override;
  end;

  { TdxPieceTableInsertContentConvertedToDocumentModelCommand }

  TdxPieceTableInsertContentConvertedToDocumentModelCommand = class(TdxPieceTableInsertObjectAtLogPositionCommand)
  private
    FPasteFromIE: Boolean;
    FSuppressParentFieldsUpdate: Boolean;
    FCopyLastParagraph: Boolean;
    FInsertOptions: TdxInsertOptions;
    FSuppressFieldsUpdate: Boolean;
    FSuppressCopySectionProperties: Boolean;
    FCopyBetweenInternalModels: Boolean;
    FSourceModel: TdxDocumentModel;
    FIsMergingTableCell: Boolean;
    FRemoveLeadingPageBreak: Boolean;
  protected
    function GetChangeType: TdxDocumentModelChangeType; override;
    procedure ExecuteCore; override;

    property SourceModel: TdxDocumentModel read FSourceModel;
    property IsMergingTableCell: Boolean read FIsMergingTableCell write FIsMergingTableCell;
  public
    constructor Create(ATargetPieceTable: TdxPieceTable; ASourceModel: TdxDocumentModel;
      ALogPosition: TdxDocumentLogPosition; AInsertOptions: TdxInsertOptions; AForceVisible: Boolean); overload;
    constructor Create(ATargetPieceTable: TdxPieceTable; ASourceModel: TdxDocumentModel;
      ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean); overload;
    procedure CopyDocumentModelContent(ACopyManager: TdxDocumentModelCopyManager; ALength: Integer);
    function GetDocumentLength(ADocumentModel: TdxDocumentModel): Integer;

    property InsertOptions: TdxInsertOptions read FInsertOptions;
    property SuppressParentFieldsUpdate: Boolean read FSuppressParentFieldsUpdate write FSuppressParentFieldsUpdate;
    property SuppressFieldsUpdate: Boolean read FSuppressFieldsUpdate write FSuppressFieldsUpdate;
    property CopyBetweenInternalModels: Boolean read FCopyBetweenInternalModels write FCopyBetweenInternalModels;
    property CopyLastParagraph: Boolean read FCopyLastParagraph write FCopyLastParagraph;
    property PasteFromIE: Boolean read FPasteFromIE write FPasteFromIE;
    property SuppressCopySectionProperties: Boolean read FSuppressCopySectionProperties write FSuppressCopySectionProperties;
    property RemoveLeadingPageBreak: Boolean read FRemoveLeadingPageBreak write FRemoveLeadingPageBreak;
  end;

  { TdxBookmarkCopyOperationBase }

  TdxBookmarkCopyOperationBase = class abstract
  public
    function GetEntireBookmarks(ASourcePieceTable: TdxPieceTable; AStart: TdxDocumentLogPosition;
      ALength: Integer): TdxBookmarkBaseList; virtual; abstract;
    procedure InsertBookmark(ATargetPieceTable: TdxPieceTable; AStart: TdxDocumentLogPosition; ALength: Integer;
      ABookmark: TdxBookmarkBase; APositionOffset: Integer); virtual; abstract;
  end;

  { TdxBookmarkCopyOperation }

  TdxBookmarkCopyOperation = class(TdxBookmarkCopyOperationBase)
  strict private
    FForceUpdateInterval: Boolean;
  public
    function GetEntireBookmarks(ASourcePieceTable: TdxPieceTable; AStart: TdxDocumentLogPosition;
      ALength: Integer): TdxBookmarkBaseList; override;
    procedure InsertBookmark(ATargetPieceTable: TdxPieceTable; AStart: TdxDocumentLogPosition;
      ALength: Integer; ABookmark: TdxBookmarkBase; APositionOffset: Integer); override;

    property ForceUpdateInterval: Boolean read FForceUpdateInterval write FForceUpdateInterval;
  end;

  { TdxCopyBookmarksOperation }

  TdxCopyBookmarksOperation = class(TdxCopySectionOperation)
  strict private
    FTargetBookmarksPosition: TdxDocumentModelPosition;
  protected
    procedure CopyBookmarksToTargetModelCore(ARunInfo: TdxRunInfo; const APositionToInsert: TdxDocumentModelPosition); overload; virtual;
    procedure CopyBookmarksToTargetModelCore(ARunInfo: TdxRunInfo; const APositionToInsert: TdxDocumentLogPosition;
      AIsAllowed: Boolean; ABookmarkCopyOperation: TdxBookmarkCopyOperationBase); overload;
  public
    procedure CopyBookmarksToTargetModel(ARunInfo: TdxRunInfo; const APositionToInsert: TdxDocumentModelPosition); overload; virtual;
    function ExecuteCore(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Boolean; override;

    property TargetBookmarksPosition: TdxDocumentModelPosition read FTargetBookmarksPosition write FTargetBookmarksPosition;
  end;

  { TdxRangePermissionCopyOperation }

  TdxRangePermissionCopyOperation = class(TdxBookmarkCopyOperationBase)
  public
    function GetEntireBookmarks(ASourcePieceTable: TdxPieceTable; AStart: TdxDocumentLogPosition;
      ALength: Integer): TdxBookmarkBaseList; override;
    procedure InsertBookmark(ATargetPieceTable: TdxPieceTable; AStart: TdxDocumentLogPosition;
      ALength: Integer; ABookmark: TdxBookmarkBase; APositionOffset: Integer); override;
  end;

  { TdxDocumentModelCopyCommand }

  TdxDocumentModelCopyCommand = class(TdxPieceTableCommand)
  private
    FTableCopyFromNestedLevel: Integer;
    FAllowCopyWholeFieldResult: Boolean;
    FFixLastParagraph: Boolean;
    FSuppressFieldsUpdate: Boolean;
    FTargetModel: TdxDocumentModel;
    FOptions: TdxDocumentModelCopyOptions;
    FRemoveLeadingPageBreak: Boolean;
    FUpdateFieldOperationType: TdxUpdateFieldOperationType;
    FUpdateIntervals: Boolean;
  protected
    procedure ExecuteCore; override;
    function CreateCopyBookmarkOperation(ACopyManager: TdxDocumentModelCopyManager): TdxCopyBookmarksOperation; virtual;
    procedure NormalizeTables(APieceTable: TdxPieceTable);
    class procedure CopyStyles(ATargetModel, ASourceModel: TdxDocumentModel; AWithId: Boolean = False); static;
    procedure CopyDocumentVariables(ATargetModel, ASourceModel: TdxDocumentModel);
    class procedure ReplaceStylesCore(ATargetStyles, ASourceStyles: TdxStyleCollectionBase); static;
    class procedure CopyStylesCore(ATargetStyles, ASourceStyles: TdxStyleCollectionBase; AWithId: Boolean); static;
    procedure CalculateExecutionParameters; override;
    procedure ApplyChanges; override;
    procedure CalculateApplyChangesParameters; override;

    property TargetModel: TdxDocumentModel read FTargetModel;
  public
    constructor Create(ASourcePieceTable: TdxPieceTable; ATargetModel: TdxDocumentModel; AOptions: TdxDocumentModelCopyOptions); reintroduce;

    class procedure ReplaceDefaultProperties(ATargetModel: TdxDocumentModel; ASourceModel: TdxDocumentModel); static;
    class procedure ReplaceDefaultStyles(ATargetModel, ASourceModel: TdxDocumentModel); static;

    property FixLastParagraph: Boolean read FFixLastParagraph write FFixLastParagraph;
    property TableCopyFromNestedLevel: Integer read FTableCopyFromNestedLevel write FTableCopyFromNestedLevel;
    property AllowCopyWholeFieldResult: Boolean read FAllowCopyWholeFieldResult write FAllowCopyWholeFieldResult;
    property SuppressFieldsUpdate: Boolean read FSuppressFieldsUpdate write FSuppressFieldsUpdate;
    property RemoveLeadingPageBreak: Boolean read FRemoveLeadingPageBreak write FRemoveLeadingPageBreak;
    property UpdateFieldOperationType: TdxUpdateFieldOperationType read FUpdateFieldOperationType write FUpdateFieldOperationType;
    property UpdateIntervals: Boolean read FUpdateIntervals write FUpdateIntervals;
  end;

  { TdxPieceTableInsertSeparatorAtLogPositionCommand }

  TdxPieceTableInsertSeparatorAtLogPositionCommand = class(TdxPieceTableInsertObjectAtLogPositionCommand)
  protected
    function GetChangeType: TdxDocumentModelChangeType; override;
    procedure ExecuteCore; override;
  end;

  { TdxDocumentModelInsertSectionAtLogPositionCommand }

  TdxDocumentModelInsertSectionAtLogPositionCommand = class(TdxDocumentModelInsertObjectAtLogPositionCommand)
  protected
    function GetChangeType: TdxDocumentModelChangeType; override;
    procedure ExecuteCore; override;
  end;

  { TdxPieceTableTableBaseCommand }

  TdxPieceTableTableBaseCommand = class abstract(TdxPieceTableCommand)
  protected
    procedure CalculateExecutionParameters; override;
    procedure CalculateApplyChangesParameters; override;
    procedure ApplyChanges; override;
  end;

  { TdxPieceTableTableDocumentServerOwnerCommand }

  TdxPieceTableTableDocumentServerOwnerCommand = class abstract(TdxPieceTableTableBaseCommand)
  private
    FServer: IdxInnerRichEditDocumentServerOwner;
  public
    constructor Create(APieceTable: TdxPieceTable; const AServer: IdxInnerRichEditDocumentServerOwner); reintroduce;

    property DocumentServer: IdxInnerRichEditDocumentServerOwner read FServer;
  end;

  { TdxPieceTableCreateEmptyTableCommand }

  TdxPieceTableCreateEmptyTableCommand = class(TdxPieceTableTableBaseCommand)
  private
    FInsertedTable: TdxTable;
    FSourceCell: TdxTableCell;
  protected
    procedure ExecuteCore; override;
  public
    constructor Create(APieceTable: TdxPieceTable; ASourceCell: TdxTableCell); reintroduce;

    property NewTable: TdxTable read FInsertedTable;
  end;

  { TdxPieceTableCreateRowEmptyCommand }

  TdxPieceTableCreateRowEmptyCommand = class(TdxPieceTableTableBaseCommand)
  private
    FInsertedTableRow: TdxTableRow;
    FTable: TdxTable;
    FIndex: Integer;
  protected
    procedure ExecuteCore; override;
  public
    constructor Create(APieceTable: TdxPieceTable; ATable: TdxTable; AIndex: Integer); reintroduce;

    property InsertedRow: TdxTableRow read FInsertedTableRow;
  end;

  { TdxPieceTableCreateCellEmptyCommand }

  TdxPieceTableCreateCellEmptyCommand = class(TdxPieceTableTableBaseCommand)
  private
    FInsertedIndex: Integer;
    FStartParagraphIndex: TdxParagraphIndex;
    FRow: TdxTableRow;
    FEndParagraphIndex: TdxParagraphIndex;
    FInsertedTableCell: TdxTableCell;
  protected
    procedure ExecuteCore; override;
  public
    constructor Create(APieceTable: TdxPieceTable; ARow: TdxTableRow; AInsertedIndex: Integer;
      AStart, AEnd: TdxParagraphIndex); reintroduce;

    property InsertedCell: TdxTableCell read FInsertedTableCell;
    property StartParagraphIndex: TdxParagraphIndex  read FStartParagraphIndex;
    property EndParagraphIndex: TdxParagraphIndex read FEndParagraphIndex;
    property Row: TdxTableRow read FRow;
    property InsertedIndex: Integer read FInsertedIndex;
  end;

  { TdxPieceTableInsertTableRowCommand }

  TdxPieceTableInsertTableRowCommand = class abstract(TdxPieceTableCommand)
  strict private
    FNewRowStartParagraphIndex: TdxParagraphIndex;
    FPositionToParagraphsInsert: TdxDocumentLogPosition;
    FNewRowIndex: Integer;
    FPatternRow: TdxTableRow;
    FForceVisible: Boolean;
  protected
    procedure ExecuteCore; override;
    procedure CorrectVerticalMerging(ARow: TdxTableRow; I: Integer; ASourceCell: TdxTableCell; ATargetCell: TdxTableCell); virtual; abstract;
    procedure CopyPropertiesFromPatternCell(ASource: TdxTableCell; ATarget: TdxTableCell); virtual; abstract;
    procedure CopyCharacterAndParagraphFormattingFromPatternCell(ASource: TdxTableCell; ATarget: TdxTableCell); virtual;
    procedure InsertParagraphs(ALogPosition: TdxDocumentLogPosition; ACells: TdxTableCellCollection);
    procedure CalculateApplyChangesParameters; override;
    procedure ApplyChanges; override;
    procedure AfterParagraphsInserted; virtual; abstract;
    function GetNextRow(ARow: TdxTableRow): TdxTableRow; virtual; abstract;

    property NewRowStartParagraphIndex: TdxParagraphIndex read FNewRowStartParagraphIndex write FNewRowStartParagraphIndex;
    property PositionToParagraphsInsert: TdxDocumentLogPosition read FPositionToParagraphsInsert write FPositionToParagraphsInsert;
    property NewRowIndex: Integer read FNewRowIndex write FNewRowIndex;
  public
    constructor Create(APieceTable: TdxPieceTable; APatternRow: TdxTableRow; AForceVisible: Boolean); reintroduce;

    property PatternRow: TdxTableRow read FPatternRow;
  end;

  { TdxPieceTableInsertTableRowBelowCommand }

  TdxPieceTableInsertTableRowBelowCommand = class(TdxPieceTableInsertTableRowCommand)
  protected
    procedure CalculateExecutionParameters; override;
    procedure AfterParagraphsInserted; override;
    function GetNextRow(ARow: TdxTableRow): TdxTableRow; override;
    procedure CorrectVerticalMerging(ACreatedRow: TdxTableRow; I: Integer; ASourceCell, ATargetCell: TdxTableCell); override;
    procedure CopyPropertiesFromPatternCell(ASourceCell, ATargetCell: TdxTableCell); override;
  end;

  { TdxPieceTableInsertTableRowAboveCommand }

  TdxPieceTableInsertTableRowAboveCommand = class(TdxPieceTableInsertTableRowCommand)
  protected
    procedure CalculateExecutionParameters; override;
    procedure AfterParagraphsInserted; override;
    function GetNextRow(ARow: TdxTableRow): TdxTableRow; override;
    procedure CorrectVerticalMerging(ARow: TdxTableRow; I: Integer; ASourceCell, ATargetCell: TdxTableCell); override;
    procedure CopyPropertiesFromPatternCell(ASourceCell, ATargetCell: TdxTableCell); override;
  end;

  { TdxPieceTableMergeTableCellsCommandBase }

  TdxPieceTableMergeTableCellsCommandBase = class abstract(TdxPieceTableTableBaseCommand)
  private
    FCell: TdxTableCell;
    FNeedDeleteNextTableCell: Boolean;
    FCopyManager: TdxDocumentModelCopyManager;
    FSuppressNormalizeTableRows: Boolean;
    FCopyDocumentModel: TdxDocumentModel;
    function GetCopyPieceTable: TdxPieceTable;
  protected
    procedure ExecuteCore; override;
    function CalculateNextCell: TdxTableCell; virtual; abstract;
    procedure UpdateProperties(ANextCell: TdxTableCell); virtual; abstract;
    function CalculateSelectionRange(ACell: TdxTableCell): TdxSelectionRange; virtual;
    function IsEmptyCell(ACell: TdxTableCell): Boolean; virtual;
    procedure CopyToCopyPieceTable(ACopyingRange: TdxSelectionRange); virtual;
    procedure DeleteTableCellWithContent(ANextCell: TdxTableCell; ADeletingRange: TdxSelectionRange); virtual;
    procedure CopyToPieceTable; virtual;
    procedure FixParagraphsInPatternCell(ANeedDeleteFirstParagraphInCell, ANeedDeleteLastParagraphInCell: Boolean); virtual;

    property CopyPieceTable: TdxPieceTable read GetCopyPieceTable;
    property CopyManager: TdxDocumentModelCopyManager read FCopyManager write FCopyManager;
    property NeedDeleteNextTableCell: Boolean read FNeedDeleteNextTableCell write FNeedDeleteNextTableCell;
    property PatternCell: TdxTableCell read FCell write FCell;
    property SuppressNormalizeTableRows: Boolean read FSuppressNormalizeTableRows write FSuppressNormalizeTableRows;
  public
		constructor Create(APieceTable: TdxPieceTable; ACell: TdxTableCell); reintroduce;
    destructor Destroy; override;
  end;

  { TdxPieceTableMergeTwoTableCellsHorizontallyCommand }

  TdxPieceTableMergeTwoTableCellsHorizontallyCommand = class(TdxPieceTableMergeTableCellsCommandBase)
  protected
    function CalculateNextCell: TdxTableCell; override;
    procedure UpdateProperties(ANextCell: TdxTableCell); override;
    procedure DeleteTableCellWithContent(ANextCell: TdxTableCell; ADeletingRange: TdxSelectionRange); override;
  end;

  { TdxPieceTableMergeTwoTableCellsVerticallyCommand }

  TdxPieceTableMergeTwoTableCellsVerticallyCommand = class(TdxPieceTableMergeTableCellsCommandBase)
  protected
    function CalculateNextCell: TdxTableCell; override;
    procedure UpdateProperties(ANextCell: TdxTableCell); override;
  end;

  { TdxPieceTableMergeTableCellsHorizontallyCommand }

  TdxPieceTableMergeTableCellsHorizontallyCommand = class(TdxPieceTableTableBaseCommand)
  private
    FCell: TdxTableCell;
    FCount: Integer;
  protected
    function CalculateCellIndex(ARow: TdxTableRow; AStartCellIndex, AColumnSpan: Integer): Integer; virtual;
    procedure ExecuteCore; override;
  public
    constructor Create(APieceTable: TdxPieceTable; ACell: TdxTableCell; ACount: Integer); reintroduce;
  end;

  { TdxPieceTableMergeTableCellsVerticallyCommand }

  TdxPieceTableMergeTableCellsVerticallyCommand = class(TdxPieceTableTableBaseCommand)
  private
    FCell: TdxTableCell;
    FCount: Integer;
  protected
    procedure ExecuteCore; override;
  public
    constructor Create(APieceTable: TdxPieceTable; ACell: TdxTableCell; ACount: Integer); reintroduce;
  end;

  { TdxPieceTableDeleteTableCellWithNestedTablesCommand }

  TdxPieceTableDeleteTableCellWithNestedTablesCommand = class(TdxPieceTableTableBaseCommand)
  private
    FTableIndex: Integer;
    FRowIndex: Integer;
    FCellIndex: Integer;
  protected
    procedure ExecuteCore; override;
  public
    constructor Create(APieceTable: TdxPieceTable; ATableIndex, ARowIndex, ACellIndex: Integer); reintroduce;
  end;

  { TdxPieceTableSplitTableCommand }

  TdxPieceTableSplitTableCommand = class(TdxPieceTableTableBaseCommand)
  private
    FCopyManager: TdxDocumentModelCopyManager;
    FCopyDocumentModel: TdxDocumentModel;
    FTableIndex: Integer;
    FRowIndex: Integer;
    FForceVisible: Boolean;
    function GetCopyPieceTable: TdxPieceTable;
    function GetTable: TdxTable;
    function GetParagraphs: TdxParagraphCollection;
  protected
    procedure ExecuteCore; override;
    procedure InsertParagraphBeforeTable; virtual;
    function CalculateSelectionRange: TdxSelectionRange; virtual;
    procedure CopyToCopyPieceTable(ACopyingRange: TdxSelectionRange); virtual;
    procedure DeleteContent(ADeletingRange: TdxSelectionRange); virtual;
    procedure CopyToPieceTable; virtual;
    procedure InsertParagraphWithDefaultProperties(APosition: TdxDocumentLogPosition); virtual;

    property CopyPieceTable: TdxPieceTable read GetCopyPieceTable;
    property CopyManager: TdxDocumentModelCopyManager read FCopyManager write FCopyManager;
    property Table: TdxTable read  GetTable;
    property Paragraphs: TdxParagraphCollection read GetParagraphs;
  public
    constructor Create(APieceTable: TdxPieceTable; ATableIndex, ARowIndex: Integer; AForceVisible: Boolean); reintroduce;
    destructor Destroy; override;
  end;

  { TdxPieceTablePieceTableInsertColumnBase }

  TdxPieceTablePieceTableInsertColumnBase = class abstract(TdxPieceTableTableBaseCommand)
  private
    FForceVisible: Boolean;
    FPatternCell: TdxTableCell;
    procedure NormalizeTableCellWidth(ATable: TdxTable);
  protected
    procedure ExecuteCore; override;
    procedure InsertColumnToTheLeft(ACurrentRow: TdxTableRow; ACurrentCell: TdxTableCell); virtual;
    procedure ChangeStartParagraphIndexInCells(ACells: TdxTableCellList);
    procedure InsertColumnToTheRight(ACurrentRow: TdxTableRow; ACurrentCell: TdxTableCell); virtual;
    function InsertColumnCore(ARow: TdxTableRow; AInsertedIndex: Integer; AStart, AEnd: TdxParagraphIndex): TdxTableCell; virtual;
    function GetColumnIndex: Integer; virtual; abstract;
    function GetCurrentCell(AColumnIndex: Integer; ACurrentRow: TdxTableRow): TdxTableCell; virtual; abstract;
    procedure Modify(ACurrentRow: TdxTableRow; ACurrentCell: TdxTableCell); virtual; abstract;

    property ForceVisible: Boolean read FForceVisible;
  public
    constructor Create(APieceTable: TdxPieceTable; APatternCell: TdxTableCell; AForceVisible: Boolean); reintroduce;

    property PatternCell: TdxTableCell read FPatternCell;
  end;

  { TdxPieceTableInsertColumnToTheLeft }

  TdxPieceTableInsertColumnToTheLeft = class(TdxPieceTablePieceTableInsertColumnBase)
  protected
    function GetColumnIndex: Integer; override;
    function GetCurrentCell(AColumnIndex: Integer; ACurrentRow: TdxTableRow): TdxTableCell; override;
    procedure Modify(ACurrentRow: TdxTableRow; ACurrentCell: TdxTableCell); override;
  end;

  TdxPieceTableInsertColumnToTheRight = class(TdxPieceTablePieceTableInsertColumnBase)
  protected
    function GetColumnIndex: Integer; override;
    function GetCurrentCell(AColumnIndex: Integer; ACurrentRow: TdxTableRow): TdxTableCell; override;
    procedure Modify(ACurrentRow: TdxTableRow; ACurrentCell: TdxTableCell); override;
  end;

  { TdxPieceTableDeleteTableColumnsCommand }

  TdxPieceTableDeleteTableColumnsCommand = class(TdxPieceTableTableDocumentServerOwnerCommand)
  private
    FSelectedCells: TdxSelectedCellsCollection;
    FEndColumnIndex: Integer;
    FStartColumnIndex: Integer;
    function CalculateTableGridIntervals(
      ATableGridColumnCollection: TdxTableGridColumnCollection): TdxTableGridIntervalList;
  protected
    procedure ExecuteCore; override;
    procedure DeleteCellsWithContent(ACells: TdxTableCellList; AContainer: TdxTableWidthsContainer);
    function IsSelectedEntireRow(ACells: TdxTableCellList): Boolean;
    procedure CalculateExecutionParameters; override;
    function GetStartColumnIndex(AFirstCell: TdxTableCell): Integer;
    function GetEndColumnIndex(ALastCell: TdxTableCell): Integer;
    procedure NormalizeCellVerticalMerging(ATable: TdxTable); virtual;
    function IsAllCellsVerticalMergingContinue(ACells: TdxTableCellCollection): Boolean; virtual;

    property StartColumnIndex: Integer read FStartColumnIndex write FStartColumnIndex;
    property EndColumnIndex: Integer read FEndColumnIndex write FEndColumnIndex;
  public
    constructor Create(APieceTable: TdxPieceTable; ASelectedCells: TdxSelectedCellsCollection; const AServer: IdxInnerRichEditDocumentServerOwner);
  end;

  { TdxPieceTableDeleteTableCellsWithShiftToTheUpCommand }

  TdxPieceTableDeleteTableCellsWithShiftToTheUpCommand = class(TdxPieceTableTableBaseCommand)
  private
    FSelectedCells: TdxSelectedCellsCollection;
  protected
    procedure ExecuteCore; override;
    procedure DeleteSelectedCells(ASelectedCells: TdxSelectedCellsIntervalInRow);
  public
    constructor Create(APieceTable: TdxPieceTable; ASelectedCells: TdxSelectedCellsCollection); reintroduce;
  end;

  { TdxPieceTableDeleteTableCellWithShiftToTheUpCoreCommand }

  TdxPieceTableDeleteTableCellWithShiftToTheUpCoreCommand = class(TdxPieceTableTableBaseCommand)
  private
    FCell: TdxTableCell;
  protected
    procedure ExecuteCore; override;
  public
    constructor Create(APieceTable: TdxPieceTable; ACell: TdxTableCell); reintroduce;
  end;

  { TdxPieceTableDeleteOneTableCellWithShiftToTheUpCommand }

  TdxPieceTableDeleteOneTableCellWithShiftToTheUpCommand = class(TdxPieceTableMergeTwoTableCellsVerticallyCommand)
  private
    FRunInfo: TdxRunInfo;
  protected
    procedure ExecuteCore; override;
    procedure FixParagraphsInPatternCell(ANeedDeleteFirstParagraphInCell,
      ANeedDeleteLastParagraphInCell: Boolean); override;
    procedure DeleteContentInCell; virtual;
    procedure UpdateProperties(ANextCell: TdxTableCell); override;
  public
    constructor Create(APieceTable: TdxPieceTable; ACell: TdxTableCell);
    destructor Destroy; override;
  end;

  { TdxPieceTableDeleteTableCellWithContentCommand }

  TdxPieceTableDeleteTableCellWithContentCommand = class(TdxPieceTableTableDocumentServerOwnerCommand)
  private
    FDeletedCell: TdxTableCell;
    FCanNormalizeCellVerticalMerging: Boolean;
    FCollectVerticalSpanCells: Boolean;
    FUseDeltaBetweenColumnsUpdate: Boolean;
  protected
    procedure ExecuteCore; override;
    function CreateTableColumnWidthCalculator(ATable: TdxTable): TdxCustomTableColumnWidthCalculator; virtual;
  public
    constructor Create(APieceTable: TdxPieceTable; ADeletedCell: TdxTableCell;
      const AServer: IdxInnerRichEditDocumentServerOwner);

    property CanNormalizeCellVerticalMerging: Boolean read FCanNormalizeCellVerticalMerging
      write FCanNormalizeCellVerticalMerging;
    property CollectVerticalSpanCells: Boolean read FCollectVerticalSpanCells write FCollectVerticalSpanCells;
    property UseDeltaBetweenColumnsUpdate: Boolean read FUseDeltaBetweenColumnsUpdate write FUseDeltaBetweenColumnsUpdate;
  end;

  { TdxPieceTableDeleteTableCellWithContentKnownWidthsCommand }

  TdxPieceTableDeleteTableCellWithContentKnownWidthsCommand = class(TdxPieceTableDeleteTableCellWithContentCommand)
  strict private
    FContainer: TdxTableWidthsContainer;
  protected
    function CreateTableColumnWidthCalculator(ATable: TdxTable): TdxCustomTableColumnWidthCalculator; override;
  public
    constructor Create(APieceTable: TdxPieceTable; ADeletedCell: TdxTableCell;
      const AServer: IdxInnerRichEditDocumentServerOwner; const AContainer: TdxTableWidthsContainer); reintroduce;
  end;

  { TdxPieceTableInsertTableCellsBase }

  TdxPieceTableInsertTableCellsBase = class abstract(TdxPieceTableTableDocumentServerOwnerCommand)
  private
    FPatternCell: TdxTableCell;
    FCanCopyProperties: Boolean;
    FCanNormalizeTable: Boolean;
    FForceVisible: Boolean;
    FCanNormalizeVerticalMerging: Boolean;
  protected
    function Modify(ACell: TdxTableCell): TdxTableCell; virtual; abstract;
    procedure ExecuteCore; override;
    procedure NormalizeTableGridAfter(ATable: TdxTable); virtual;

    property CanNormalizeTable: Boolean read FCanNormalizeTable write FCanNormalizeTable;
    property CanCopyProperties: Boolean read FCanCopyProperties write FCanCopyProperties;
    property CanNormalizeVerticalMerging: Boolean read FCanNormalizeVerticalMerging
      write FCanNormalizeVerticalMerging;
    property ForceVisible: Boolean read FForceVisible;
  public
    constructor Create(APieceTable: TdxPieceTable; APatternCell: TdxTableCell;
      AForceVisible: Boolean; const AServer: IdxInnerRichEditDocumentServerOwner);

		property PatternCell: TdxTableCell read FPatternCell;
  end;

  { TdxPieceTableInsertTableCellToTheLeft }

  TdxPieceTableInsertTableCellToTheLeft = class(TdxPieceTableInsertTableCellsBase)
  protected
    function Modify(ACell: TdxTableCell): TdxTableCell; override;
    procedure ChangeStartParagraphIndexInCells(ACells: TdxTableCellList);
  end;

  { TdxPieceTableInsertTableCellToTheRight }

  TdxPieceTableInsertTableCellToTheRight = class(TdxPieceTableInsertTableCellsBase)
  protected
    function Modify(ACell: TdxTableCell): TdxTableCell; override;
  end;

  { TdxPieceTableInsertTableCellWithShiftToTheDownCommand }

  TdxPieceTableInsertTableCellWithShiftToTheDownCommand = class(TdxPieceTableTableDocumentServerOwnerCommand)
  private
    FPatternCell: TdxTableCell;
    FForceVisible: Boolean;
  protected
    procedure ExecuteCore; override;
    procedure InsertTableCells(APatternCell: TdxTableCell; AInsertedCellsCount: Integer;
      AWidth: TdxPreferredWidth); virtual;
    procedure DeleteContentInPatternCell; virtual;
  public
    constructor Create(APieceTable: TdxPieceTable; APatternCell: TdxTableCell;
      AForceVisible: Boolean; const AServer: IdxInnerRichEditDocumentServerOwner);
  end;

  { TdxPieceTableInsertTableCellWithShiftToTheDownCoreCommand }

  TdxPieceTableInsertTableCellWithShiftToTheDownCoreCommand = class(TdxPieceTableMergeTableCellsCommandBase)
  protected
    function CalculateNextCell: TdxTableCell; override;
    procedure UpdateProperties(ANextCell: TdxTableCell); override;
    procedure DeleteTableCellWithContent(ANextCell: TdxTableCell; ADeletingRange: TdxSelectionRange); override;
    procedure FixParagraphsInPatternCell(ANeedDeleteFirstParagraphInCell, ANeedDeleteLastParagraphInCell: Boolean); override;
  end;

  { TdxPieceTableSplitTableCellsHorizontally }

  TdxPieceTableSplitTableCellsHorizontally = class(TdxPieceTableTableDocumentServerOwnerCommand)
  private
    FPatternCell: TdxTableCell;
    FPartsCount: Integer;
    FForceVisible: Boolean;
  protected
    procedure ExecuteCore; override;
    procedure NormalizeTableCellsWidth(AOldWidth: Integer);
  public
    constructor Create(APieceTable: TdxPieceTable; APatternCell: TdxTableCell;
      APartsCount: Integer; AForceVisible: Boolean; const AServer: IdxInnerRichEditDocumentServerOwner);
  end;

  { TdxPieceTableSplitTableCellsVertically }

  TdxPieceTableSplitTableCellsVertically = class(TdxPieceTableTableBaseCommand)
  private
    FPatternCell: TdxTableCell;
    FPartsCount: Integer;
    FColumnsCount: Integer;
    FForceVisible: Boolean;
  protected
    procedure ExecuteCore; override;
    procedure InsertRows(ARow: TdxTableRow); virtual;
    procedure SplitMergedCellsVertically(ACellsInRow: TdxTableCellCollection); virtual;
    function GetSelectedCellsEndIndex: Integer; virtual;
    procedure SplitMergedCellsVerticallyCore(ARestartCell: TdxTableCell); virtual;
  public
    constructor Create(APieceTable: TdxPieceTable; APatternCell: TdxTableCell;
      APartsCount, AColumnsCount: Integer; AForceVisible: Boolean); reintroduce;
  end;

  TdxTableArray = TArray<TdxTable>;

  { TdxPositionInfo }

  TdxPositionInfo = record
  strict private
    FPosition: Integer;
    FMinLeft: Integer;
    FMaxRight: Integer;
  public
    constructor Create(APosition, AMinLeft, AMaxRight: Integer);

    property Position: Integer read FPosition write FPosition;
    property MinLeft: Integer read FMinLeft write FMinLeft;
    property MaxRight: Integer read FMaxRight write FMaxRight;
  end;

  { TdxTableGridMerger }

  TdxTableGridMerger = class
  public const
    Delta = 5;
  strict private
    FTables: array of TdxTable;
  protected
    function GetPositions(const APositionInfo: TArray<TdxPositionInfo>): TArray<Integer>;
    function CalculateMergedPositions(const ATables: array of TdxTable; const ATableGrids: array of TdxTableGrid;
      ATotalPositions: TdxIntegerList; const ASourceTableRowsCount: array of Integer): TList<TArray<Integer>>;
    function CalculateAbsolutePositions(ATable: TdxTable; AColumns: TdxTableGridColumnCollection;
      ARowCount: Integer): TArray<TdxPositionInfo>;
    procedure AdjustNearestPositions(const AAbsolutePositions: TList<TArray<TdxPositionInfo>>; ALayoutDelta: Integer;
      const ATotalPositions: TdxIntegerList);
    function AdjustRight(ATotalPositions: TdxIntegerList; ATotalPositionIndex: Integer;
      const APositions: TArray<TdxPositionInfo>; APositionIndex: Integer; ALayoutDelta: Integer; APos: TdxPositionInfo): Boolean;
    function AdjustLeft(ATotalPositions: TdxIntegerList; ATotalPositionIndex: Integer;
      const APositions: TArray<TdxPositionInfo>; APositionIndex: Integer; ALayoutDelta: Integer; APos: TdxPositionInfo): Boolean;
    procedure AdjustPosition(const ATotalPositions: TdxIntegerList; ATotalPositionIndex: Integer;
      const APositions: TArray<TdxPositionInfo>; APositionIndex: Integer; ALayoutDelta: Integer);
    procedure MergedGridCore(ATable: TdxTable; AStartRowIndex: Integer; AEndRowIndex: Integer;
      const ATablePositions: TArray<Integer>; ATotalPositions: TdxIntegerList);
    function CalculateColumnSpan(const ATotalPositions: TdxIntegerList; AStartPos: Integer; AEndPos: Integer): Integer;
  public
    constructor Create(const ATables: array of TdxTable);
    procedure MergeGrids(const ATableGrids: array of TdxTableGrid; const ASourceTableRowsCount: array of Integer);
  end;

  { TdxPieceTableJoinSeveralTablesCommand }

  TdxPieceTableJoinSeveralTablesCommand = class(TdxPieceTableTableBaseCommand)
  strict private
    FTables: TdxTableArray;
    FJoinPosition: TdxDocumentLogPosition;
  protected
    procedure ExecuteCore; override;
    function CalculateColumnWidth: Integer;
    function GetTableGrid(ATable: TdxTable; APercentWidthBase: Integer): TdxTableGrid;
    function CalculateColumnIndex(APosition: Integer; AColumnWidthCollection: TdxIntegerList;
      var ACorrection: Integer): Integer; virtual;
  public
    constructor Create(APieceTable: TdxPieceTable; const ATables: TdxTableArray);
  end;

  { TdxPieceTableJoinTablesCommand }

  TdxPieceTableJoinTablesCommand = class(TdxPieceTableJoinSeveralTablesCommand)
  public
    constructor Create(APieceTable: TdxPieceTable; ATopTable, ABottomTable: TdxTable); reintroduce;
  end;

  { TdxColumnIndexParameters }

  TdxColumnIndexParameters = record
    ColumnIndex: Integer;
    IsExactly: Boolean;
    constructor Create(AColumnIndex: Integer; AIsExactly: Boolean);
  end;

  { TdxTableCellParameters }

  TdxTableCellParameters = record
    IndexInRow: Integer;
    RowIndex: Integer;
    StartColumnIndex: Integer;
    EndColumnIndex: Integer;
    constructor Create(AIndexInRow, ARowIndex, AStartColumnIndex, AEndColumnIndex: Integer);
  end;

  { TdxDistanceCollection }

  TdxDistanceCollection = class(TdxIntegerList);

  { TdxNormalizeTableGridHelperBase }

  TdxNormalizeTableGridHelperBase = class abstract
  protected const
    DeltaBetweenColumns = 4;
  strict private
    FDeltaBetweenColumnsUpdate: Integer;
    FTable: TdxTable;
    FPatternCellsParameters: TList<TdxTableCellParameters>;
    FGridColumns: TdxTableGridColumnCollection;
    function GetDocumentModel: TdxDocumentModel;
  protected
    function CalculatePatternCellsParameters(ACells: TdxTableCellList): TList<TdxTableCellParameters>; virtual;
    procedure NormalizeColumnSpans;
    procedure SnapDistanceToColumnGrid(AMergedDistances: TdxIntegerList; ADistancesToNewCells: TdxList<TdxDistanceCollection>); virtual;
    function CalculateDistancesToNewCells: TdxList<TdxDistanceCollection>; virtual;
    function CalculateDistancesToNewCellsCore(ACellParameters: TdxTableCellParameters): TdxDistanceCollection; virtual;
    function GetColumnIndex(AIndex: Integer; ACell: TdxTableCell): Integer; virtual;
    function CalculateDistance(ADistanceToCell: Integer; ACellWidth: Integer): Integer; virtual;
    function MergeDistances(ADistances: TdxList<TdxDistanceCollection>): TdxIntegerList;
    function GetRowContainsPatternCellIndexes: TdxIntegerList; virtual;
    procedure NormalizeColumnSpansCore(ADistances: TdxIntegerList; ATableRow: TdxTableRow);
    function GetWidthConsiderUnitType(AWidthUnit: TdxWidthUnit): Integer; virtual;
    procedure RecalculateGridIntervals(AMergedDistances: TdxIntegerList);
    procedure NormalizeColumnSpansInRowContainingPatternCell(ADistances: TdxList<TdxDistanceCollection>);
    procedure NormalizeColumnSpansInRowContainingPatternCellCore(AColumnSpans: TdxIntegerList; ACellParameters: TdxTableCellParameters);
    function GetIndex(AIndex: Integer): Integer; virtual;
    function CalculateColumnSpansForNewCells(ADistances: TdxIntegerList; ACellParameters: TdxTableCellParameters): TdxIntegerList; virtual;
    function CalculateColumnIndexParameters(ADistanceToCell: Integer): TdxColumnIndexParameters;
    procedure NormalizeWidthAfter(ATable: TdxTable);
    function GetActualWidthValue(AWidthUnit: TdxWidthUnit): Integer; virtual;
    procedure ResetWidthAfter(ARowProperties: TdxTableRowProperties); virtual;
    procedure CheckTableWidthTypeIntegrity(ATable: TdxTable);
    procedure NormalizeVerticalMerging(ACell: TdxTableCell); virtual;

    property DeltaBetweenColumnsUpdate: Integer read FDeltaBetweenColumnsUpdate;
  public
    constructor Create(ATable: TdxTable; ACells: TdxTableCellList; AGridColumns: TdxTableGridColumnCollection; AUseDeltaBetweenColumns: Boolean);
    destructor Destroy; override;

    property Table: TdxTable read FTable;
    property PatternCellsParameters: TList<TdxTableCellParameters> read FPatternCellsParameters;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property GridColumns: TdxTableGridColumnCollection read FGridColumns;
  end;

  { TdxNormalizeTableGridAfterSplitCellHelper }

  TdxNormalizeTableGridAfterSplitCellHelper = class(TdxNormalizeTableGridHelperBase)
  strict private
    FPartsCount: Integer;
  protected
    function CalculateDistancesToNewCellsCore(ACellParameters: TdxTableCellParameters): TdxDistanceCollection; override;
    procedure SnapDistanceToColumnGrid(AMergedDistances: TdxIntegerList;
      ADistancesToNewCells: TdxList<TdxDistanceCollection>); override;
    procedure ChangeDistance(ADistancesToNewCells: TdxList<TdxDistanceCollection>; AOldDistance: Integer;
      ANewDistance: Integer; const AStartIndices: TArray<Integer>);
  public
    constructor Create(ATable: TdxTable; ACells: TdxTableCellList; APartsCount: Integer;
      AGridColumns: TdxTableGridColumnCollection; AUseDeltaBetweenColumns: Boolean);
  end;

  { TdxNormalizeTableGridAfterInsertCellHelper }

  TdxNormalizeTableGridAfterInsertCellHelper = class(TdxNormalizeTableGridHelperBase)
  protected
    function GetIndex(AIndex: Integer): Integer; override;
    function CalculateColumnSpansForNewCells(ADistances: TdxIntegerList; ACellParameters: TdxTableCellParameters): TdxIntegerList; override;
  end;

  { TdxNormalizeTableGridAfterDeleteCellHelper }

  TdxNormalizeTableGridAfterDeleteCellHelper = class(TdxNormalizeTableGridHelperBase)
  protected
    function GetColumnIndex(AIndex: Integer; ACell: TdxTableCell): Integer; override;
    function CalculateDistance(ADistanceToCell: Integer; ACellWidth: Integer): Integer; override;
  end;

  { TdxParagraphListHistoryItemBase }

  TdxParagraphListHistoryItemBase = class abstract(TdxParagraphBaseHistoryItem)
  private
    FListLevelIndex: Integer;
    FNumberingListIndex: TdxNumberingListIndex;
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
  public
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property ListLevelIndex: Integer read FListLevelIndex write FListLevelIndex;
    property NumberingListIndex: TdxNumberingListIndex read FNumberingListIndex write FNumberingListIndex;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxAddParagraphToListHistoryItem }

  TdxAddParagraphToListHistoryItem = class(TdxParagraphListHistoryItemBase)
  private
    FOldOwnNumberingListIndex: TdxNumberingListIndex;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;

    property OldOwnNumberingListIndex: TdxNumberingListIndex read FOldOwnNumberingListIndex
      write FOldOwnNumberingListIndex;
  end;

  { TdxNumberingListNotifier }

  TdxNumberingListNotifier = class
  public
    class procedure NotifyParagraphAdded(ADocumentModel: TdxDocumentModel; AIndex: TdxNumberingListIndex); static;
    class procedure NotifyParagraphRemoved(ADocumentModel: TdxDocumentModel; AIndex: TdxNumberingListIndex); static;
  end;

  { TdxRemoveParagraphFromListHistoryItem }

  TdxRemoveParagraphFromListHistoryItem = class(TdxParagraphListHistoryItemBase)
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  end;

implementation

uses
  dxRichEdit.Utils.BatchUpdateHelper, Contnrs, RTLConsts,
  Math,

  dxTypeHelpers,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.History.Table,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.TwipsToLayoutDocumentsConverter,
  dxRichEdit.DocumentModel.History.FieldHistory,
  dxRichEdit.Import.Core,
  dxRichEdit.Import.Formats,
  dxRichEdit.Control.AutoCorrect,
  dxRichEdit.Utils.Exceptions;

type
  TdxHistoryItemAccess = class(TdxHistoryItem);

{ TdxPieceTableCommand }

function TdxPieceTableCommand.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxPieceTableCommand.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

{ TdxPieceTableInsertFieldSymbolResultAtInputPositionCommand }

constructor TdxPieceTableInsertFieldSymbolResultAtInputPositionCommand.Create(APieceTable: TdxCustomPieceTable;
  ALogPosition: TdxDocumentLogPosition; const ASymbol: Char);
begin
  inherited Create(APieceTable, ALogPosition, ASymbol, False);
  FSymbol := ASymbol;
end;

procedure TdxPieceTableInsertFieldSymbolResultAtInputPositionCommand.ExecuteCore;
begin
  TdxPieceTable(PieceTable).InsertInsertFieldSymbolResultCore(ParagraphIndex, LogPosition, Symbol, ForceVisible);
end;

{ TdxDocumentModelCommand }

constructor TdxDocumentModelCommand.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  Assert(ADocumentModel <> nil);
  FDocumentModel := ADocumentModel;
end;

procedure TdxDocumentModelCommand.BeginExecute;
begin
  DocumentModel.BeginUpdate;
  FTransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  CalculateExecutionParameters;
  CalculateApplyChangesParameters;
end;

procedure TdxDocumentModelCommand.EndExecute;
begin
  ApplyChanges;
  FreeAndNil(FTransaction);
  DocumentModel.EndUpdate;
end;

procedure TdxDocumentModelCommand.Execute;
begin
  BeginExecute;
  ExecuteCore;
  EndExecute;
end;

function TdxDocumentModelCommand.GetPieceTable: TdxPieceTable;
begin
  Result := FDocumentModel.MainPieceTable;
end;

{ TdxDocumentModelInsertObjectCommand }

procedure TdxDocumentModelInsertObjectCommand.ApplyChanges;
var
  AParagraph: TdxParagraph;
begin
  AParagraph := PieceTable.Paragraphs[FApplyChangesParagraphIndex];
  PieceTable.ApplyChanges(ChangeType, AParagraph.FirstRunIndex, AParagraph.LastRunIndex);
end;

procedure TdxDocumentModelInsertObjectCommand.CalculateApplyChangesParameters;
var
  ASectionIndex: TdxSectionIndex;
  ASection: TdxSection;
  AParagraph: TdxParagraph;
begin
  ASectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(ParagraphIndex);
  ASection := DocumentModel.Sections[ASectionIndex];
  if (ASection.LastParagraphIndex = ParagraphIndex) and (ParagraphIndex > 0) then
    FApplyChangesParagraphIndex := ParagraphIndex - 1
  else
    FApplyChangesParagraphIndex := ParagraphIndex;
  AParagraph := PieceTable.Paragraphs[FApplyChangesParagraphIndex];
  PieceTable.ApplyChanges(ChangeType, AParagraph.FirstRunIndex, AParagraph.LastRunIndex + 1);
end;

procedure TdxDocumentModelInsertObjectCommand.CalculateExecutionParameters;
begin
  FParagraphIndex := CalculateInsertionParagraphIndex;
end;

{ TdxDocumentModelInsertObjectAtLogPositionCommand }

constructor TdxDocumentModelInsertObjectAtLogPositionCommand.Create(ADocumentModel: TdxDocumentModel;
  ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean);
begin
  inherited Create(ADocumentModel);
  if ALogPosition < 0 then
    TdxRichEditExceptions.ThrowArgumentException('logPosition', logPosition);
  FLogPosition := ALogPosition;
  FForceVisible := AForceVisible;
end;

function TdxDocumentModelInsertObjectAtLogPositionCommand.CalculateInsertionParagraphIndex: TdxParagraphIndex;
begin
  Result := PieceTable.FindParagraphIndex(LogPosition);
end;

{ TdxDocumentModelInsertObjectAtInputPositionCommand }

constructor TdxDocumentModelInsertObjectAtInputPositionCommand.Create(ADocumentModel: TdxDocumentModel;
  APosition: TdxInputPosition);
begin
  inherited Create(ADocumentModel);
  if APosition.LogPosition < 0 then
    TdxRichEditExceptions.ThrowArgumentException('logPosition', APosition.LogPosition);
  FPosition := APosition;
end;

function TdxDocumentModelInsertObjectAtInputPositionCommand.CalculateInsertionParagraphIndex: TdxParagraphIndex;
begin
  Result := Position.ParagraphIndex;
end;

{ TdxPieceTableApplyAutoCorrectCommand }

constructor TdxPieceTableApplyAutoCorrectCommand.Create(APieceTable: TdxPieceTable;
  ALogPosition: TdxDocumentLogPosition; ALength: Integer; const AText: string;
  const ARule: IdxSpellCheckerAutoCorrectCustomRule);
begin
  inherited Create(APieceTable);
  FLogPosition := ALogPosition;
  FLength := ALength;
  FText := AText;
  FRule := ARule;
end;

procedure TdxPieceTableApplyAutoCorrectCommand.AddUndoHistoryItem;
var
  AItem: TdxRichEditAutoCorrectHistoryItem;
begin
  AItem := TdxRichEditAutoCorrectHistoryItem.Create(PieceTable, FRule);
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxPieceTableApplyAutoCorrectCommand.DeleteContent;
var
  ACommand: TdxPieceTableDeleteTextCommand;
begin
  ACommand := TdxPieceTableDeleteTextCommand.Create(PieceTable, FLogPosition, FLength);
  try
    ACommand.AllowPartiallyDeletingField := False;
    ACommand.DocumentLastParagraphSelected := False;
    ACommand.ForceRemoveInnerFields := False;
    ACommand.LeaveFieldIfResultIsRemoved := False;
    ACommand.BackspacePressed := False;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTableApplyAutoCorrectCommand.InsertText;
var
  ACommand: TdxPieceTableInsertPlainTextAtLogPositionCommand;
begin
  ACommand := TdxPieceTableInsertPlainTextAtLogPositionCommand.Create(PieceTable, FLogPosition,
    FText, False);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTableApplyAutoCorrectCommand.ExecuteCore;
begin
  AddUndoHistoryItem;
  DeleteContent;
  InsertText;
  DocumentModel.Selection.SetStartCell(DocumentModel.Selection.Start);
end;

procedure TdxPieceTableApplyAutoCorrectCommand.CalculateExecutionParameters;
begin
end;

procedure TdxPieceTableApplyAutoCorrectCommand.CalculateApplyChangesParameters;
begin
end;

procedure TdxPieceTableApplyAutoCorrectCommand.ApplyChanges;
begin
end;

{ TdxPieceTableInsertFloatingObjectAnchorAtLogPositionCommand }

procedure TdxPieceTableInsertFloatingObjectAnchorAtLogPositionCommand.ExecuteCore;
begin
  TdxPieceTable(PieceTable).InsertFloatingObjectAnchorCore(ParagraphIndex, LogPosition, ForceVisible);
end;

function TdxPieceTableInsertFloatingObjectAnchorAtLogPositionCommand.GetChangeType: TdxDocumentModelChangeType;
begin
  Result := TdxDocumentModelChangeType.InsertFloatingObjectAnchor;
end;

{ TdxPieceTableInsertContentConvertedToDocumentModelCommand }

constructor TdxPieceTableInsertContentConvertedToDocumentModelCommand.Create(ATargetPieceTable: TdxPieceTable;
  ASourceModel: TdxDocumentModel; ALogPosition: TdxDocumentLogPosition; AInsertOptions: TdxInsertOptions;
  AForceVisible: Boolean);
begin
  inherited Create(ATargetPieceTable, ALogPosition, AForceVisible);
  Assert(ASourceModel <> nil);
  FSourceModel := ASourceModel;
  FInsertOptions := AInsertOptions;
end;

constructor TdxPieceTableInsertContentConvertedToDocumentModelCommand.Create(ATargetPieceTable: TdxPieceTable;
  ASourceModel: TdxDocumentModel; ALogPosition: TdxDocumentLogPosition; AForceVisible: Boolean);
begin
  Create(ATargetPieceTable, ASourceModel, ALogPosition, TdxInsertOptions.MatchDestinationFormatting, AForceVisible);
end;

procedure TdxPieceTableInsertContentConvertedToDocumentModelCommand.CopyDocumentModelContent(
  ACopyManager: TdxDocumentModelCopyManager; ALength: Integer);
var
  AOperation: TdxCopySectionOperation;
begin
  ACopyManager.TableCopyHelper.TargetStartParagraphIndex := ACopyManager.TargetPosition.ParagraphIndex;
  ACopyManager.TableCopyHelper.SuppressCopyTables := True;
  AOperation := TdxCopySectionOperation(SourceModel.CreateCopySectionOperation(ACopyManager));
  try
    AOperation.SuppressParentFieldsUpdate := SuppressParentFieldsUpdate;
    AOperation.SuppressFieldsUpdate := SuppressFieldsUpdate;
    AOperation.IsMergingTableCell := IsMergingTableCell;
    AOperation.SuppressCopySectionProperties := SuppressCopySectionProperties;
    AOperation.RemoveLeadingPageBreak := RemoveLeadingPageBreak;
    if FPasteFromIE then
      AOperation.UpdateFieldOperationType := TdxUpdateFieldOperationType.PasteFromIE;
    AOperation.Execute(0, ALength - IfThen(FCopyLastParagraph, 0, 1), True);
  finally
    AOperation.Free;
  end;
end;

procedure TdxPieceTableInsertContentConvertedToDocumentModelCommand.ExecuteCore;
var
  ALength: Integer;
  ATargetPieceTable: TdxPieceTable;
  ACopyManager: TdxDocumentModelCopyManager;
  ARunIndex: TdxRunIndex;
  ARangeStartLogPosition: TdxDocumentLogPosition;
begin
  ALength := GetDocumentLength(SourceModel);
  SourceModel.Selection.Start := 0;
  if SourceModel.MailMergeOptions.KeepLastParagraph then
    FCopyLastParagraph := True;
  SourceModel.Selection.&End := ALength - IfThen(FCopyLastParagraph, 0, 1);
  ATargetPieceTable := TdxPieceTable(PieceTable);
  ACopyManager := TdxDocumentModelCopyManager(ATargetPieceTable.GetCopyManager(SourceModel.MainPieceTable, InsertOptions));
  try
    ACopyManager.IsInserted := True;
    ACopyManager.TargetPosition.LogPosition := LogPosition;
    ACopyManager.TargetPosition.ParagraphIndex := ATargetPieceTable.FindParagraphIndex(LogPosition);
    ARangeStartLogPosition := ATargetPieceTable.FindRunStartLogPosition(ATargetPieceTable.Paragraphs[ACopyManager.TargetPosition.ParagraphIndex], LogPosition, ARunIndex);
    if ARangeStartLogPosition <> LogPosition then
    begin
      ATargetPieceTable.SplitTextRun(ACopyManager.TargetPosition.ParagraphIndex, ARunIndex, LogPosition - ARangeStartLogPosition);
      ARangeStartLogPosition := LogPosition;
      Inc(ARunIndex);
    end;
    ACopyManager.TargetPosition.RunStartLogPosition := ARangeStartLogPosition;
    ACopyManager.TargetPosition.RunIndex := ARunIndex;
    ACopyManager.CopyAdditionalInfo(CopyBetweenInternalModels);
    CopyDocumentModelContent(ACopyManager, ALength);
  finally
    ACopyManager.Free;
  end;
end;

function TdxPieceTableInsertContentConvertedToDocumentModelCommand.GetChangeType: TdxDocumentModelChangeType;
begin
  Result := TdxDocumentModelChangeType.None;
end;

function TdxPieceTableInsertContentConvertedToDocumentModelCommand.GetDocumentLength(
  ADocumentModel: TdxDocumentModel): Integer;
var
  ALastParagraph: TdxParagraph;
begin
  ALastParagraph := ADocumentModel.MainPieceTable.Paragraphs.Last;
  Result := ALastParagraph.LogPosition + ALastParagraph.Length;
end;

{ TdxBookmarkCopyOperation }

function TdxBookmarkCopyOperation.GetEntireBookmarks(ASourcePieceTable: TdxPieceTable;
  AStart: TdxDocumentLogPosition; ALength: Integer): TdxBookmarkBaseList;
begin
  Result := ASourcePieceTable.GetEntireBookmarks(AStart, ALength);
end;

procedure TdxBookmarkCopyOperation.InsertBookmark(ATargetPieceTable: TdxPieceTable; AStart: TdxDocumentLogPosition;
  ALength: Integer; ABookmark: TdxBookmarkBase; APositionOffset: Integer);
begin
  ATargetPieceTable.CreateBookmarkCore(AStart, ALength, TdxBookmark(ABookmark).Name, FForceUpdateInterval);
end;

{ TdxCopyBookmarksOperation }

function TdxCopyBookmarksOperation.ExecuteCore(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Boolean;
begin
  CopyBookmarksToTargetModel(AInfo, TargetBookmarksPosition);
  Result := True;
end;

procedure TdxCopyBookmarksOperation.CopyBookmarksToTargetModel(ARunInfo: TdxRunInfo;
  const APositionToInsert: TdxDocumentModelPosition);
begin
  SourceModel.BeginUpdate;
  try
    TargetModel.BeginUpdate;
    try
      CopyBookmarksToTargetModelCore(ARunInfo, APositionToInsert);
    finally
      TargetModel.EndUpdate;
    end;
  finally
    SourceModel.EndUpdate;
  end;
end;

procedure TdxCopyBookmarksOperation.CopyBookmarksToTargetModelCore(ARunInfo: TdxRunInfo;
  const APositionToInsert: TdxDocumentModelPosition);
var
  ABookmarksAllowed: Boolean;
  ABookmarkCopyOperation: TdxBookmarkCopyOperation;
  ARangePermissionCopyOperation: TdxRangePermissionCopyOperation;
begin
  ABookmarksAllowed := TargetModel.DocumentCapabilities.BookmarksAllowed;
  ABookmarkCopyOperation := TdxBookmarkCopyOperation.Create;
  try
    ABookmarkCopyOperation.ForceUpdateInterval := True;
    CopyBookmarksToTargetModelCore(ARunInfo, APositionToInsert.LogPosition, ABookmarksAllowed, ABookmarkCopyOperation);
  finally
    ABookmarkCopyOperation.Free;
  end;
  ARangePermissionCopyOperation := TdxRangePermissionCopyOperation.Create;
  try
    CopyBookmarksToTargetModelCore(ARunInfo, APositionToInsert.LogPosition, ABookmarksAllowed, ARangePermissionCopyOperation);
  finally
    ARangePermissionCopyOperation.Free;
  end;
end;

procedure TdxCopyBookmarksOperation.CopyBookmarksToTargetModelCore(ARunInfo: TdxRunInfo;
  const APositionToInsert: TdxDocumentLogPosition; AIsAllowed: Boolean;
  ABookmarkCopyOperation: TdxBookmarkCopyOperationBase);
var
  AStartPos: TdxDocumentLogPosition;
  ALength, AOffset, ACount, I: Integer;
  ABookmarks: TdxBookmarkBaseList;
begin
  if not AIsAllowed then
    Exit;
  AStartPos := ARunInfo.Start.LogPosition;
  ALength := ARunInfo.&End.LogPosition - AStartPos + 1;

  ABookmarks := ABookmarkCopyOperation.GetEntireBookmarks(SourcePieceTable, AStartPos, ALength);
  try
    if ABookmarks.Count = 0 then
      Exit;

    AOffset := APositionToInsert - AStartPos;
    ACount := ABookmarks.Count;
    for I := 0 to ACount - 1 do
      ABookmarkCopyOperation.InsertBookmark(TargetPieceTable, ABookmarks[I].Start + AOffset, ABookmarks[I].Length,
        ABookmarks[I], AOffset);
  finally
    ABookmarks.Free;
  end;
end;

{ TdxRangePermissionCopyOperation }

function TdxRangePermissionCopyOperation.GetEntireBookmarks(ASourcePieceTable: TdxPieceTable;
  AStart: TdxDocumentLogPosition; ALength: Integer): TdxBookmarkBaseList;
begin
  Result := ASourcePieceTable.GetEntireRangePermissions(AStart, ALength);
end;

procedure TdxRangePermissionCopyOperation.InsertBookmark(ATargetPieceTable: TdxPieceTable; AStart: TdxDocumentLogPosition;
  ALength: Integer; ABookmark: TdxBookmarkBase; APositionOffset: Integer);
var
  ATargetModel, ASourceModel: TdxDocumentModel;
begin
  ATargetModel := ATargetPieceTable.DocumentModel;
  ASourceModel := TdxDocumentModel(ABookmark.PieceTable.DocumentModel);
  if (ATargetModel.ModelForExport) or ATargetModel.IntermediateModel or not ASourceModel.IsDocumentProtectionEnabled then
    ATargetPieceTable.ApplyDocumentPermission(AStart, AStart + ALength, TdxRangePermission(ABookmark).Properties.Info);
end;

{ TdxDocumentModelCopyCommand }

constructor TdxDocumentModelCopyCommand.Create(ASourcePieceTable: TdxPieceTable; ATargetModel: TdxDocumentModel;
  AOptions: TdxDocumentModelCopyOptions);
begin
  inherited Create(ASourcePieceTable);
  Assert(ATargetModel <> nil);
  Assert(AOptions <> nil);
  FTargetModel := ATargetModel;
  FOptions := AOptions;
  FTableCopyFromNestedLevel := -1;
  UpdateFieldOperationType := TdxUpdateFieldOperationType.Copy;
end;

procedure TdxDocumentModelCopyCommand.ApplyChanges;
begin
end;

procedure TdxDocumentModelCopyCommand.CalculateApplyChangesParameters;
begin
end;

procedure TdxDocumentModelCopyCommand.CalculateExecutionParameters;
begin
end;

procedure TdxDocumentModelCopyCommand.CopyDocumentVariables(ATargetModel, ASourceModel: TdxDocumentModel);
var
  ASourceVariables: TdxDocumentVariableCollection;
  ATargetVariables: TdxDocumentVariableCollection;
  AKey: string;
begin
  ASourceVariables := ASourceModel.Variables;
  ATargetVariables := ATargetModel.Variables;
  for AKey in ASourceVariables.GetVariableNames do
    ATargetVariables.Add(AKey, ASourceVariables[AKey]);
end;

class procedure TdxDocumentModelCopyCommand.CopyStyles(ATargetModel, ASourceModel: TdxDocumentModel; AWithId: Boolean = False);
begin
  ATargetModel.BeginUpdate;
  CopyStylesCore(ATargetModel.CharacterStyles, ASourceModel.CharacterStyles, AWithId);
  CopyStylesCore(ATargetModel.ParagraphStyles, ASourceModel.ParagraphStyles, AWithId);
  CopyStylesCore(ATargetModel.TableStyles, ASourceModel.TableStyles, AWithId);
  CopyStylesCore(ATargetModel.TableCellStyles, ASourceModel.TableCellStyles, AWithId);
  CopyStylesCore(ATargetModel.NumberingListStyles, ASourceModel.NumberingListStyles, AWithId);
  ATargetModel.EndUpdate;
end;

function TdxDocumentModelCopyCommand.CreateCopyBookmarkOperation(
  ACopyManager: TdxDocumentModelCopyManager): TdxCopyBookmarksOperation;
begin
  Result := TdxCopyBookmarksOperation.Create(ACopyManager);
end;

procedure TdxDocumentModelCopyCommand.ExecuteCore;
var
  I: Integer;
  AOldValue: Boolean;
  ACopyManager: TdxDocumentModelCopyManager;
  AOperation: TdxCopySectionOperation;
  ASelectionRanges: TdxSelectionRangeCollection;
  ATargetBookmarksPositions: TList<TdxDocumentModelPosition>;
  ACurrentRange: TdxSelectionRange;
  ACopyBookmarksOperation: TdxCopyBookmarksOperation;
  APieceTables: TdxFastList;
  APieceTable: TdxPieceTable;
begin
  if FOptions.DefaultPropertiesCopyOptions = TdxDefaultPropertiesCopyOptions.Always then
    ReplaceDefaultProperties(TargetModel, DocumentModel);
  ReplaceDefaultStyles(TargetModel, DocumentModel);
  if FOptions.CopyDocumentVariables then
    CopyDocumentVariables(TargetModel, DocumentModel);
  TargetModel.DeleteDefaultNumberingList(TargetModel.NumberingLists);

  ACopyManager := TdxDocumentModelCopyManager.Create(PieceTable, TargetModel.MainPieceTable,
    FOptions.ParagraphNumerationCopyOptions, FOptions.FormattingCopyOptions);
  try
    ACopyManager.TableCopyHelper.CopyFromNestedLevel := TableCopyFromNestedLevel;
    AOperation := TdxCopySectionOperation(DocumentModel.CreateCopySectionOperation(ACopyManager));
    try
      AOperation.ShouldCopyBookmarks := False;
      AOperation.SuppressFieldsUpdate := SuppressFieldsUpdate;
      AOperation.AllowCopyWholeFieldResult := AllowCopyWholeFieldResult;
      AOperation.RemoveLeadingPageBreak := RemoveLeadingPageBreak;
      AOperation.UpdateFieldOperationType := UpdateFieldOperationType;
      ASelectionRanges := FOptions.SelectionRanges;
      ATargetBookmarksPositions := TList<TdxDocumentModelPosition>.Create;
      try
        TargetModel.ActivePieceTable.SuppressTableIntegrityCheck := True;
        for I := 0 to ASelectionRanges.Count - 1 do
        begin
          ACurrentRange := ASelectionRanges[I];
          ATargetBookmarksPositions.Add(ACopyManager.TargetPosition);
          AOperation.Execute(ACurrentRange.From, ACurrentRange.Length, False);
        end;
        NormalizeTables(TargetModel.ActivePieceTable);
        TargetModel.ActivePieceTable.SuppressTableIntegrityCheck := False;
        for I := 0 to ASelectionRanges.Count - 1 do
        begin
          ACurrentRange := ASelectionRanges[I];
          ACopyBookmarksOperation := CreateCopyBookmarkOperation(ACopyManager);
          try
            ACopyBookmarksOperation.TargetBookmarksPosition := ATargetBookmarksPositions[I];
            ACopyBookmarksOperation.Execute(ACurrentRange.From, ACurrentRange.Length, False);
          finally
            ACopyBookmarksOperation.Free;
          end;
        end;
      finally
        ATargetBookmarksPositions.Free;
      end;
      if UpdateIntervals then
      begin
        APieceTables := TargetModel.GetPieceTables(True);
        try
          for I := 0 to APieceTables.Count - 1 do
          begin
            APieceTable := APieceTables[I];
            APieceTable.UpdateIntervals;
          end;
        finally
          APieceTables.Free;
        end;
      end;
      AOldValue := TargetModel.ForceNotifyStructureChanged;
      TargetModel.ForceNotifyStructureChanged := True;
      try
        AOperation.AfterBookmarkCopied;
      finally
        TargetModel.ForceNotifyStructureChanged := AOldValue;
      end;
    finally
      AOperation.Free;
    end;
  finally
    ACopyManager.Free;
  end;

  if FixLastParagraph then
    TargetModel.MainPieceTable.FixLastParagraph;
end;

procedure TdxDocumentModelCopyCommand.NormalizeTables(APieceTable: TdxPieceTable);
var
  ATables: TdxTableCollection;
  ACurrentTable: TdxTable;
  ARow: TdxTableRow;
  I, AMaxCells, ARowIndex, ARowsCount, ACount, ANewCellsCount, ACells: Integer;
begin
  ATables := APieceTable.Tables;
  ACount := ATables.Count;
  APieceTable.DocumentModel.BeginUpdate;
  try
    for I := 0 to ACount - 1 do
    begin
      ACurrentTable := ATables[I];
      AMaxCells := ACurrentTable.FindTotalColumnsCountInTable;
      ARowsCount := ACurrentTable.Rows.Count;
      for ARowIndex := 0 to ARowsCount - 1 do
      begin
        ARow := ACurrentTable.Rows[ARowIndex];
        ACells := ACurrentTable.GetTotalCellsInRowConsiderGrid(ARow);
        if ACells < AMaxCells then
        begin
          ANewCellsCount := AMaxCells - ACells;
          ARow.GridAfter := ARow.GridAfter + ANewCellsCount;
        end;
      end;
      ACurrentTable.Normalize;
      ACurrentTable.NormalizeCellColumnSpans;
      ACurrentTable.NormalizeTableCellVerticalMerging;
    end;
  finally
    APieceTable.DocumentModel.EndUpdate;
  end;
end;

class procedure TdxDocumentModelCopyCommand.ReplaceDefaultProperties(ATargetModel, ASourceModel: TdxDocumentModel);
begin
  ATargetModel.DefaultCharacterProperties.Info.CopyFrom(ASourceModel.DefaultCharacterProperties.Info);
  ATargetModel.DefaultParagraphProperties.Info.CopyFrom(ASourceModel.DefaultParagraphProperties.Info);
  ATargetModel.DefaultTableCellProperties.Info.CopyFrom(ASourceModel.DefaultTableCellProperties.Info);
  ATargetModel.DefaultTableProperties.Info.CopyFrom(ASourceModel.DefaultTableProperties.Info);
  ATargetModel.DefaultTableRowProperties.Info.CopyFrom(ASourceModel.DefaultTableRowProperties.Info);
  ATargetModel.DocumentProperties.Info.CopyFrom(ASourceModel.DocumentProperties.Info);
end;

class procedure TdxDocumentModelCopyCommand.ReplaceDefaultStyles(ATargetModel, ASourceModel: TdxDocumentModel);
begin
  ReplaceStylesCore(ATargetModel.CharacterStyles, ASourceModel.CharacterStyles);
  ReplaceStylesCore(ATargetModel.ParagraphStyles, ASourceModel.ParagraphStyles);
  ReplaceStylesCore(ATargetModel.TableStyles, ASourceModel.TableStyles);
  ReplaceStylesCore(ATargetModel.TableCellStyles, ASourceModel.TableCellStyles);
  ReplaceStylesCore(ATargetModel.NumberingListStyles, ASourceModel.NumberingListStyles);
end;

class procedure TdxDocumentModelCopyCommand.ReplaceStylesCore(ATargetStyles, ASourceStyles: TdxStyleCollectionBase);
var
  I: Integer;
  ATargetStyle, ASourceStyle: TdxStyleBase;
begin
  for I := 0 to ATargetStyles.Count - 1 do
  begin
    ATargetStyle := ATargetStyles[I];
    ASourceStyle := ASourceStyles.GetStyleByName(ATargetStyle.StyleName);
    if ASourceStyle <> nil then
      ATargetStyle.CopyProperties(ASourceStyle);
  end;
end;

class procedure TdxDocumentModelCopyCommand.CopyStylesCore(ATargetStyles, ASourceStyles: TdxStyleCollectionBase;  AWithId: Boolean);
var
  I: Integer;
  ATargetStyle, ASourceStyle: TdxStyleBase;
  AStyleIndex: Integer;
begin
  for I := 0 to ASourceStyles.Count - 1 do
  begin
    ASourceStyle := ASourceStyles[I];
    ATargetStyle := ATargetStyles.GetStyleByName(ASourceStyle.StyleName);
    if ATargetStyle <> nil then
      ATargetStyle.CopyProperties(ASourceStyle)
    else
    begin
      AStyleIndex := ASourceStyle.Copy(ATargetStyles.DocumentModel);
      ATargetStyle := ATargetStyles[AStyleIndex];
    end;
    if AWithId then
      ATargetStyle.SetId(ASourceStyle.Id);
  end;
end;

{ TdxPieceTableInsertSeparatorAtLogPositionCommand }

procedure TdxPieceTableInsertSeparatorAtLogPositionCommand.ExecuteCore;
begin
  TdxPieceTable(PieceTable).InsertSeparatorTextRunCore(ParagraphIndex, LogPosition);
end;

function TdxPieceTableInsertSeparatorAtLogPositionCommand.GetChangeType: TdxDocumentModelChangeType;
begin
  Result := TdxDocumentModelChangeType.InsertText;
end;

{ TdxDocumentModelInsertSectionAtLogPositionCommand }

procedure TdxDocumentModelInsertSectionAtLogPositionCommand.ExecuteCore;
begin
  if DocumentModel.DocumentCapabilities.SectionsAllowed then
    DocumentModel.SafeEditor.InsertSectionCore(ParagraphIndex, LogPosition, ForceVisible)
  else
    PieceTable.InsertParagraphCore(ParagraphIndex, LogPosition, ForceVisible);
  DocumentModel.MainPieceTable.ApplyNumberingToInsertedParagraph(ParagraphIndex);
end;

function TdxDocumentModelInsertSectionAtLogPositionCommand.GetChangeType: TdxDocumentModelChangeType;
begin
  Result := TdxDocumentModelChangeType.InsertSection;
end;

{ TdxPieceTableTableBaseCommand }

procedure TdxPieceTableTableBaseCommand.ApplyChanges;
begin
end;

procedure TdxPieceTableTableBaseCommand.CalculateApplyChangesParameters;
begin
end;

procedure TdxPieceTableTableBaseCommand.CalculateExecutionParameters;
begin
end;

{ TdxPieceTableTableDocumentServerOwnerCommand }

constructor TdxPieceTableTableDocumentServerOwnerCommand.Create(APieceTable: TdxPieceTable;
  const AServer: IdxInnerRichEditDocumentServerOwner);
begin
  inherited Create(APieceTable);
  Assert(AServer <> nil);
  FServer := AServer;
end;

{ TdxPieceTableCreateEmptyTableCommand }

constructor TdxPieceTableCreateEmptyTableCommand.Create(APieceTable: TdxPieceTable; ASourceCell: TdxTableCell);
begin
  inherited Create(APieceTable);
  FSourceCell := ASourceCell;
end;

procedure TdxPieceTableCreateEmptyTableCommand.ExecuteCore;
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxCreateEmptyTableHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AItem := TdxCreateEmptyTableHistoryItem.Create(PieceTable, FSourceCell);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
    FInsertedTable := PieceTable.Tables[AItem.InsertedTableIndex];
  finally
    ATransaction.Free;
  end;
end;

{ TdxPieceTableCreateRowEmptyCommand }

constructor TdxPieceTableCreateRowEmptyCommand.Create(APieceTable: TdxPieceTable; ATable: TdxTable; AIndex: Integer);
begin
  inherited Create(APieceTable);
  Assert(ATable <> nil);
  Assert(AIndex >= 0);
  FTable := ATable;
  FIndex := AIndex;
end;

procedure TdxPieceTableCreateRowEmptyCommand.ExecuteCore;
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxInsertEmptyTableRowHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AItem := TdxInsertEmptyTableRowHistoryItem.Create(PieceTable, FTable, FIndex);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
    FInsertedTableRow := FTable.Rows[AItem.InsertedRowIndex];
  finally
    ATransaction.Free;
  end;
end;

{ TdxPieceTableCreateCellEmptyCommand }

constructor TdxPieceTableCreateCellEmptyCommand.Create(APieceTable: TdxPieceTable; ARow: TdxTableRow;
  AInsertedIndex: Integer; AStart, AEnd: TdxParagraphIndex);
begin
  inherited Create(APieceTable);
  Assert(ARow <> nil);
  Assert(AStart >= 0);
  Assert(AEnd >= 0);
  Assert((AInsertedIndex >= 0) and (AInsertedIndex <= ARow.Cells.Count));
  FRow := ARow;
  FStartParagraphIndex := AStart;
  FEndParagraphIndex := AEnd;
  FInsertedIndex := AInsertedIndex;
end;

procedure TdxPieceTableCreateCellEmptyCommand.ExecuteCore;
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxInsertEmptyTableCellHistoryItem;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AItem := TdxInsertEmptyTableCellHistoryItem.Create(PieceTable, Row, InsertedIndex, StartParagraphIndex, EndParagraphIndex);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
    FInsertedTableCell := AItem.InsertedCell;
  finally
    ATransaction.Free;
  end;
end;

{ TdxPieceTableInsertTableRowCommand }

constructor TdxPieceTableInsertTableRowCommand.Create(APieceTable: TdxPieceTable; APatternRow: TdxTableRow;
  AForceVisible: Boolean);
begin
  inherited Create(APieceTable);
  Assert(APatternRow <> nil);
  FPatternRow := APatternRow;
  FForceVisible := AForceVisible;
end;

procedure TdxPieceTableInsertTableRowCommand.ExecuteCore;
var
  ARow: TdxTableRow;
  ACellCount, I: Integer;
  ASourceCell, ATargetCell: TdxTableCell;
begin
  InsertParagraphs(PositionToParagraphsInsert, FPatternRow.Cells);

  ARow := PieceTable.CreateTableRowCore(FPatternRow.Table, NewRowIndex);
  ARow.Properties.CopyFrom(FPatternRow.Properties);

  ACellCount := FPatternRow.Cells.Count;
  PieceTable.ConvertParagraphsIntoTableRow(ARow, NewRowStartParagraphIndex, ACellCount);

  for I := 0 to ACellCount - 1 do
  begin
    ASourceCell := FPatternRow.Cells[I];
    ATargetCell := ARow.Cells[I];
    ATargetCell.StyleIndex := ASourceCell.StyleIndex;
    CopyPropertiesFromPatternCell(ASourceCell, ATargetCell);
    CopyCharacterAndParagraphFormattingFromPatternCell(ASourceCell, ATargetCell);
    CorrectVerticalMerging(ARow, I, ASourceCell, ATargetCell);
  end;
end;

procedure TdxPieceTableInsertTableRowCommand.CopyCharacterAndParagraphFormattingFromPatternCell(ASource: TdxTableCell; ATarget: TdxTableCell);
var
  APatternParagraph, ATargetParagraph: TdxParagraph;
  ARunIndex: TdxRunIndex;
  AFirstRunFromPatternParagraph: TdxTextRunBase;
  AFirstRunFromTargetParagraph: TdxParagraphRun;
  AOptions: TdxDocumentCapabilitiesOptions;
begin
  APatternParagraph := PieceTable.Paragraphs[ASource.StartParagraphIndex];
  ATargetParagraph := PieceTable.Paragraphs[ATarget.StartParagraphIndex];
  APatternParagraph.CopyFrom(DocumentModel, ATargetParagraph);
  ARunIndex := APatternParagraph.FirstRunIndex;
  AFirstRunFromPatternParagraph := PieceTable.Runs[ARunIndex];
  if AFirstRunFromPatternParagraph is TdxSeparatorTextRun then
    AFirstRunFromPatternParagraph := PieceTable.Runs[ARunIndex + 1];
  if PieceTable.Runs[ATargetParagraph.FirstRunIndex] is TdxParagraphRun then
    AFirstRunFromTargetParagraph := TdxParagraphRun(PieceTable.Runs[ATargetParagraph.FirstRunIndex])
  else
    AFirstRunFromTargetParagraph := nil;
  if (AFirstRunFromPatternParagraph <> nil) and (AFirstRunFromTargetParagraph <> nil) then
  begin
    AOptions := DocumentModel.DocumentCapabilities;
    if AOptions.CharacterFormattingAllowed then
      AFirstRunFromTargetParagraph.CharacterProperties.CopyFrom(AFirstRunFromPatternParagraph.CharacterProperties.Info);
    if AOptions.CharacterStyleAllowed then
      AFirstRunFromTargetParagraph.CharacterStyleIndex := AFirstRunFromPatternParagraph.CharacterStyle.Copy(DocumentModel);
  end;
end;

procedure TdxPieceTableInsertTableRowCommand.InsertParagraphs(ALogPosition: TdxDocumentLogPosition; ACells: TdxTableCellCollection);
var
  ACount, I: Integer;
  ASourceParagraph, ATargetParagraph: TdxParagraph;
begin
  ACount := ACells.Count;
  for I := 0 to ACount - 1 do
    PieceTable.InsertParagraph(ALogPosition, FForceVisible);
  for I := 0 to ACount - 1 do
  begin
    ASourceParagraph := PieceTable.Paragraphs[ACells[I].StartParagraphIndex];
    ATargetParagraph := PieceTable.Paragraphs[NewRowStartParagraphIndex + I];
    ATargetParagraph.InheritStyleAndFormattingFrom(ASourceParagraph);
  end;
  AfterParagraphsInserted;
end;

procedure TdxPieceTableInsertTableRowCommand.CalculateApplyChangesParameters;
begin
end;

procedure TdxPieceTableInsertTableRowCommand.ApplyChanges;
begin
end;

{ TdxPieceTableInsertTableRowBelowCommand }

procedure TdxPieceTableInsertTableRowBelowCommand.AfterParagraphsInserted;
var
  AIndex: TdxParagraphIndex;
begin
	AIndex := PatternRow.LastCell.EndParagraphIndex;
  PieceTable.ChangeCellEndParagraphIndex(PatternRow.LastCell, AIndex - PatternRow.Cells.Count);
end;

procedure TdxPieceTableInsertTableRowBelowCommand.CalculateExecutionParameters;
var
  AIndex: TdxParagraphIndex;
  AParagraph: TdxParagraph;
begin
  AIndex := PatternRow.LastCell.EndParagraphIndex;
  AParagraph := PieceTable.Paragraphs[AIndex];
  PositionToParagraphsInsert := AParagraph.LogPosition + AParagraph.Length - 1;
  NewRowStartParagraphIndex := AIndex + 1;
  NewRowIndex := PatternRow.Table.Rows.IndexOf(PatternRow) + 1;
end;

procedure TdxPieceTableInsertTableRowBelowCommand.CopyPropertiesFromPatternCell(ASourceCell, ATargetCell: TdxTableCell);
var
  APatternCell, AFirstCellInVerticalMergeGroup: TdxTableCell;
begin
  APatternCell := ASourceCell;
  if ASourceCell.VerticalMerging = TdxMergingState.Continue then
  begin
    AFirstCellInVerticalMergeGroup := ASourceCell.Table.GetFirstCellInVerticalMergingGroup(ASourceCell);
    if AFirstCellInVerticalMergeGroup <> nil then
      APatternCell := AFirstCellInVerticalMergeGroup;
  end;
  ATargetCell.Properties.CopyFrom(APatternCell.Properties);
end;

procedure TdxPieceTableInsertTableRowBelowCommand.CorrectVerticalMerging(ACreatedRow: TdxTableRow; I: Integer;
  ASourceCell, ATargetCell: TdxTableCell);
var
  ANextRow: TdxTableRow;
  ASourceCellStartColumnIndex, AIndexInNextRow: Integer;
begin
  if ASourceCell.VerticalMerging = TdxMergingState.Continue then
  begin
    ANextRow := GetNextRow(ACreatedRow);
    if ANextRow = nil then
      ATargetCell.VerticalMerging := TdxMergingState.None
    else
    begin
      ASourceCellStartColumnIndex := ASourceCell.GetStartColumnIndexConsiderRowGrid;
      AIndexInNextRow := ANextRow.Table.GetAbsoluteCellIndexInRow(ANextRow, ASourceCellStartColumnIndex, False);
      if ANextRow.Cells[AIndexInNextRow].VerticalMerging <> TdxMergingState.Continue then
        ATargetCell.VerticalMerging := TdxMergingState.None;
    end;
  end
  else
    if ASourceCell.VerticalMerging = TdxMergingState.Restart then
      ATargetCell.VerticalMerging := TdxMergingState.Continue;
end;

function TdxPieceTableInsertTableRowBelowCommand.GetNextRow(ARow: TdxTableRow): TdxTableRow;
begin
  Result := ARow.Next;
end;

{ TdxPieceTableInsertTableRowAboveCommand }

procedure TdxPieceTableInsertTableRowAboveCommand.AfterParagraphsInserted;
var
  AIndex: TdxParagraphIndex;
  AMostNestedCell: TdxTableCell;
begin
  AIndex := PatternRow.FirstCell.StartParagraphIndex;
  AMostNestedCell := PieceTable.Paragraphs[AIndex].GetCell;
  PieceTable.ChangeCellStartParagraphIndex(PatternRow.FirstCell, AIndex + PatternRow.Cells.Count);
  while AMostNestedCell.Table.ParentCell <> nil do
  begin
    PieceTable.ChangeCellStartParagraphIndex(AMostNestedCell, AIndex + PatternRow.Cells.Count);
    AMostNestedCell := AMostNestedCell.Table.ParentCell;
  end;
end;

procedure TdxPieceTableInsertTableRowAboveCommand.CalculateExecutionParameters;
var
  AIndex: TdxParagraphIndex;
  AParagraph: TdxParagraph;
begin
  AIndex := PatternRow.FirstCell.StartParagraphIndex;
  AParagraph := PieceTable.Paragraphs[AIndex];
  PositionToParagraphsInsert := AParagraph.LogPosition;
  NewRowStartParagraphIndex := AIndex;
  NewRowIndex := PatternRow.Table.Rows.IndexOf(PatternRow);
end;

procedure TdxPieceTableInsertTableRowAboveCommand.CopyPropertiesFromPatternCell(ASourceCell, ATargetCell: TdxTableCell);
begin
  ATargetCell.Properties.CopyFrom(ASourceCell.Properties);
end;

procedure TdxPieceTableInsertTableRowAboveCommand.CorrectVerticalMerging(ARow: TdxTableRow; I: Integer; ASourceCell,
  ATargetCell: TdxTableCell);
begin
  if ATargetCell.Properties.VerticalMerging = TdxMergingState.Restart then
    ATargetCell.Properties.VerticalMerging := TdxMergingState.None;
end;

function TdxPieceTableInsertTableRowAboveCommand.GetNextRow(ARow: TdxTableRow): TdxTableRow;
begin
  Result := ARow.Previous;
end;

{ TdxPieceTableMergeTableCellsCommandBase }

constructor TdxPieceTableMergeTableCellsCommandBase.Create(APieceTable: TdxPieceTable; ACell: TdxTableCell);
begin
  inherited Create(APieceTable);
  FNeedDeleteNextTableCell := False;
  Assert(ACell <> nil);
  FCell := ACell;
end;

destructor TdxPieceTableMergeTableCellsCommandBase.Destroy;
begin
  FreeAndNil(FCopyDocumentModel);
  inherited Destroy;
end;

function TdxPieceTableMergeTableCellsCommandBase.GetCopyPieceTable: TdxPieceTable;
begin
  Result := FCopyDocumentModel.ActivePieceTable;
end;

procedure TdxPieceTableMergeTableCellsCommandBase.ExecuteCore;
var
  ATable: TdxTable;
  ANextCell: TdxTableCell;
  ATransaction: TdxHistoryTransaction;
  AIsEmptyCell, AIsEmptyNextCell: Boolean;
  ASelectionRangeForNextCell: TdxSelectionRange;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ATable := FCell.Table;
    ANextCell := CalculateNextCell;
    UpdateProperties(ANextCell);

    ASelectionRangeForNextCell := CalculateSelectionRange(ANextCell);
    try
      AIsEmptyCell := IsEmptyCell(FCell);
      AIsEmptyNextCell := IsEmptyCell(ANextCell);

      CopyToCopyPieceTable(ASelectionRangeForNextCell);
      DeleteTableCellWithContent(ANextCell, ASelectionRangeForNextCell);
    finally
      ASelectionRangeForNextCell.Free;
    end;
    CopyToPieceTable;
    FixParagraphsInPatternCell(AIsEmptyCell, AIsEmptyNextCell);

    ATable.NormalizeCellColumnSpans;
    if not FSuppressNormalizeTableRows then
      ATable.NormalizeRows;
  finally
    ATransaction.Free;
  end;
end;

function TdxPieceTableMergeTableCellsCommandBase.CalculateSelectionRange(ACell: TdxTableCell): TdxSelectionRange;
var
  AStartLogPosition, AEndLogPosition: TdxDocumentLogPosition;
  ALength: Integer;
begin
  AStartLogPosition := PieceTable.Paragraphs[ACell.StartParagraphIndex].LogPosition;
  AEndLogPosition := PieceTable.Paragraphs[ACell.EndParagraphIndex].EndLogPosition;
  ALength := AEndLogPosition - AStartLogPosition + 1;
  Result := TdxSelectionRange.Create(AStartLogPosition, ALength);
end;

function TdxPieceTableMergeTableCellsCommandBase.IsEmptyCell(ACell: TdxTableCell): Boolean;
var
  AStartParagraph: TdxParagraph;
begin
  AStartParagraph := PieceTable.Paragraphs[ACell.StartParagraphIndex];
  Result := (ACell.StartParagraphIndex = ACell.EndParagraphIndex) and (AStartParagraph.Length = 1);
end;

procedure TdxPieceTableMergeTableCellsCommandBase.CopyToCopyPieceTable(ACopyingRange: TdxSelectionRange);
var
  AOptions: TdxDocumentModelCopyOptions;
  ACopyCommand: TdxDocumentModelCopyCommand;
begin
  FCopyDocumentModel := PieceTable.DocumentModel.CreateNew;
  FCopyDocumentModel.IntermediateModel := True;
  FCopyDocumentModel.BeginUpdate;
  try
    AOptions := TdxDocumentModelCopyOptions.Create(ACopyingRange.Start, ACopyingRange.Length);
    try
      ACopyCommand := TdxDocumentModelCopyCommand(
        PieceTable.DocumentModel.CreateDocumentModelCopyCommand(PieceTable, FCopyDocumentModel, AOptions));
      try
        ACopyCommand.TableCopyFromNestedLevel := FCell.Table.NestedLevel + 1;
        ACopyCommand.Execute;
      finally
        ACopyCommand.Free;
      end;
    finally
      AOptions.Free;
    end;
  finally
    FCopyDocumentModel.EndUpdate;
  end;
end;

procedure TdxPieceTableMergeTableCellsCommandBase.DeleteTableCellWithContent(ANextCell: TdxTableCell;
  ADeletingRange: TdxSelectionRange);
var
  ATable: TdxTable;
begin
  ATable := ANextCell.Table;
  if NeedDeleteNextTableCell then
    PieceTable.DeleteTableCellWithNestedTables(ATable.Index, ANextCell.RowIndex, ANextCell.IndexInRow);
  PieceTable.DeleteContent(ADeletingRange.Start, ADeletingRange.Length, False);
end;

procedure TdxPieceTableMergeTableCellsCommandBase.CopyToPieceTable;
var
  APosition: TdxDocumentLogPosition;
  ACommand: TdxPieceTableInsertContentConvertedToDocumentModelCommand;
begin
  APosition := PieceTable.Paragraphs[PatternCell.EndParagraphIndex].EndLogPosition;
  ACommand := TdxPieceTableInsertContentConvertedToDocumentModelCommand.Create(PieceTable, FCopyDocumentModel, APosition, False);
  try
    ACommand.IsMergingTableCell := True;
    ACommand.CopyBetweenInternalModels := True;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTableMergeTableCellsCommandBase.FixParagraphsInPatternCell(ANeedDeleteFirstParagraphInCell: Boolean; ANeedDeleteLastParagraphInCell: Boolean);
var
  AOperation: TdxDeleteParagraphOperation;
  AParagraphs: TdxParagraphCollection;
begin
  if not ANeedDeleteFirstParagraphInCell and not ANeedDeleteLastParagraphInCell then
    Exit;

  AOperation := TdxDeleteParagraphOperation.Create(PieceTable);
  try
    AOperation.AllowedDeleteLastParagraphInTableCell := True;
    AParagraphs := PieceTable.Paragraphs;
    if ANeedDeleteFirstParagraphInCell then
    begin
      AOperation.Execute(AParagraphs[PatternCell.StartParagraphIndex].LogPosition, 1, False);
      Exit;
    end;
    if ANeedDeleteLastParagraphInCell then
      AOperation.Execute(AParagraphs[PatternCell.EndParagraphIndex].LogPosition, 1, False);
  finally
    AOperation.Free;
  end;
end;

{ TdxPieceTableMergeTwoTableCellsHorizontallyCommand }

function TdxPieceTableMergeTwoTableCellsHorizontallyCommand.CalculateNextCell: TdxTableCell;
var
  ARow: TdxTableRow;
  ACells: TdxTableCellCollection;
  AIndex, ANextCellIndex: Integer;
begin
  ARow := PatternCell.Row;
  AIndex := PatternCell.IndexInRow;
  ANextCellIndex := AIndex + 1;
  ACells := ARow.Cells;
  if ANextCellIndex < ACells.Count then
    Result := ACells[ANextCellIndex]
  else
    Result := nil;
end;

procedure TdxPieceTableMergeTwoTableCellsHorizontallyCommand.DeleteTableCellWithContent(ANextCell: TdxTableCell;
  ADeletingRange: TdxSelectionRange);
begin
  NeedDeleteNextTableCell := True;
  inherited DeleteTableCellWithContent(ANextCell, ADeletingRange);
end;

procedure TdxPieceTableMergeTwoTableCellsHorizontallyCommand.UpdateProperties(ANextCell: TdxTableCell);
begin
  PatternCell.ColumnSpan := PatternCell.ColumnSpan + ANextCell.ColumnSpan;
  PatternCell.PreferredWidth.Value := PatternCell.PreferredWidth.Value + ANextCell.PreferredWidth.Value;
end;

{ TdxPieceTableMergeTwoTableCellsVerticallyCommand }

function TdxPieceTableMergeTwoTableCellsVerticallyCommand.CalculateNextCell: TdxTableCell;
var
  ATable: TdxTable;
  AColumnIndex: Integer;
begin
  ATable := PatternCell.Table;
  AColumnIndex := PatternCell.GetStartColumnIndexConsiderRowGrid;
  Result := ATable.GetCell(PatternCell.RowIndex + 1, AColumnIndex);
end;

procedure TdxPieceTableMergeTwoTableCellsVerticallyCommand.UpdateProperties(ANextCell: TdxTableCell);
begin
  PatternCell.VerticalMerging := TdxMergingState.Restart;
  ANextCell.VerticalMerging := TdxMergingState.Continue;
end;

{ TdxPieceTableMergeTableCellsHorizontallyCommand }

constructor TdxPieceTableMergeTableCellsHorizontallyCommand.Create(APieceTable: TdxPieceTable; ACell: TdxTableCell;
  ACount: Integer);
begin
  inherited Create(APieceTable);
  Assert(ACell <> nil);
  Assert(ACount > 0);
  FCell := ACell;
  FCount := ACount;
end;

function TdxPieceTableMergeTableCellsHorizontallyCommand.CalculateCellIndex(ARow: TdxTableRow; AStartCellIndex,
  AColumnSpan: Integer): Integer;
var
  ACells: TdxTableCellCollection;
  ACount: Integer;
begin
  ACells := ARow.Cells;
  ACount := ACells.Count;
  Result := AStartCellIndex;
  while Result < ACount - 1 do
  begin
    Dec(AColumnSpan, ACells[Result].ColumnSpan);
    if AColumnSpan <= 0 then
      Break;
    Inc(Result);
  end;
end;

procedure TdxPieceTableMergeTableCellsHorizontallyCommand.ExecuteCore;
var
  ARow: TdxTableRow;
  AMergeCell: TdxTableCell;
  ATransaction: TdxHistoryTransaction;
  AStartCellIndex, AEndCellIndex, I: Integer;
  ACommand: TdxPieceTableMergeTwoTableCellsHorizontallyCommand;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ARow := FCell.Row;
    AStartCellIndex := ARow.Cells.IndexOf(FCell);
    AEndCellIndex := CalculateCellIndex(ARow, AStartCellIndex, FCount) - 1;

    for I := AEndCellIndex downto AStartCellIndex do
    begin
      AMergeCell := ARow.Cells[I];
      ACommand := TdxPieceTableMergeTwoTableCellsHorizontallyCommand.Create(PieceTable, AMergeCell);
      try
        ACommand.SuppressNormalizeTableRows := True;
        ACommand.Execute;
      finally
        ACommand.Free;
      end;
    end;
  finally
    ATransaction.Free;
  end;
end;

{ TdxPieceTableMergeTableCellsVerticallyCommand }

constructor TdxPieceTableMergeTableCellsVerticallyCommand.Create(APieceTable: TdxPieceTable; ACell: TdxTableCell;
  ACount: Integer);
begin
  inherited Create(APieceTable);
  Assert(ACell <> nil);
  Assert(ACount > 0);
  FCell := ACell;
  FCount := ACount;
end;

procedure TdxPieceTableMergeTableCellsVerticallyCommand.ExecuteCore;
var
  ATable: TdxTable;
  AMergeCell: TdxTableCell;
  ATransaction: TdxHistoryTransaction;
  AColumnIndex, ARestartRowIndex, AContinueRowIndex, I: Integer;
  ACommand: TdxPieceTableMergeTwoTableCellsVerticallyCommand;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ATable := FCell.Table;
    AColumnIndex := ATable.GetCellColumnIndexConsiderRowGrid(FCell);

    ARestartRowIndex := FCell.RowIndex;
    AContinueRowIndex := ARestartRowIndex + FCount - 2;
    for I := AContinueRowIndex downto ARestartRowIndex do
    begin
      AMergeCell := ATable.GetCell(I, AColumnIndex);
      ACommand := TdxPieceTableMergeTwoTableCellsVerticallyCommand.Create(PieceTable, AMergeCell);
      try
        ACommand.SuppressNormalizeTableRows := True;
        ACommand.Execute;
      finally
        ACommand.Free;
      end;
    end;
    ATable.NormalizeRows;
  finally
    ATransaction.Free;
  end;
end;

{ TdxPieceTableDeleteTableCellWithNestedTablesCommand }

constructor TdxPieceTableDeleteTableCellWithNestedTablesCommand.Create(APieceTable: TdxPieceTable; ATableIndex,
  ARowIndex, ACellIndex: Integer);
begin
  inherited Create(APieceTable);
  Assert(ATableIndex >= 0);
  Assert(ARowIndex >= 0);
  Assert(ACellIndex >= 0);
  FTableIndex := ATableIndex;
  FRowIndex := ARowIndex;
  FCellIndex := ACellIndex;
end;

procedure TdxPieceTableDeleteTableCellWithNestedTablesCommand.ExecuteCore;
var
  ATransaction: TdxHistoryTransaction;
  ADeletedCell: TdxTableCell;
  ARunInfo: TdxRunInfo;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ADeletedCell := PieceTable.Tables[FTableIndex].Rows[FRowIndex].Cells[FCellIndex];
    ARunInfo := PieceTable.GetRunInfoByTableCell(ADeletedCell);
    try
      PieceTable.DeleteSelectedTables(ARunInfo, False);
    finally
      ARunInfo.Free;
    end;
    PieceTable.DeleteEmptyTableCellCore(FTableIndex, FRowIndex, FCellIndex);
  finally
    ATransaction.Free;
  end;
end;

{ TdxPieceTableSplitTableCommand }

constructor TdxPieceTableSplitTableCommand.Create(APieceTable: TdxPieceTable; ATableIndex, ARowIndex: Integer;
  AForceVisible: Boolean);
begin
  inherited Create(APieceTable);
  Assert(ATableIndex >= 0);
  Assert(ARowIndex >= 0);
  FTableIndex := ATableIndex;
  FRowIndex := ARowIndex;
  FForceVisible := AForceVisible;
end;

destructor TdxPieceTableSplitTableCommand.Destroy;
begin
  FreeAndNil(FCopyDocumentModel);
  inherited Destroy;
end;

function TdxPieceTableSplitTableCommand.CalculateSelectionRange: TdxSelectionRange;
var
  AStartCell, AEndCell: TdxTableCell;
  AStartLogPosition, AEndLogPosition: TdxDocumentLogPosition;
  ALength: Integer;
begin
  AStartCell := Table.Rows[FRowIndex].FirstCell;
  AEndCell := Table.Rows.Last.LastCell;

  AStartLogPosition := Paragraphs[AStartCell.StartParagraphIndex].LogPosition;
  AEndLogPosition := Paragraphs[AEndCell.EndParagraphIndex].EndLogPosition;
  ALength := AEndLogPosition - AStartLogPosition + 1;
  Result := TdxSelectionRange.Create(AStartLogPosition, ALength);
end;

procedure TdxPieceTableSplitTableCommand.CopyToCopyPieceTable(ACopyingRange: TdxSelectionRange);
var
  AOptions: TdxDocumentModelCopyOptions;
  ACopyCommand: TdxDocumentModelCopyCommand;
begin
  FCopyDocumentModel := PieceTable.DocumentModel.CreateNew;
  FCopyDocumentModel.IntermediateModel := True;
  FCopyDocumentModel.BeginUpdate;
  try
    AOptions := TdxDocumentModelCopyOptions.Create(ACopyingRange.Start, ACopyingRange.Length);
    try
      ACopyCommand := PieceTable.DocumentModel.CreateDocumentModelCopyCommand(PieceTable,
        FCopyDocumentModel, AOptions) as TdxDocumentModelCopyCommand;
      try
        ACopyCommand.Execute;
      finally
        ACopyCommand.Free;
      end;
    finally
      AOptions.Free;
    end;
  finally
    FCopyDocumentModel.EndUpdate;
  end;
end;

procedure TdxPieceTableSplitTableCommand.CopyToPieceTable;
var
  ARow: TdxTableRow;
  APosition: TdxDocumentLogPosition;
  ACommand: TdxPieceTableInsertContentConvertedToDocumentModelCommand;
begin
  ARow := Table.Rows[FRowIndex - 1];
  APosition := Paragraphs[ARow.LastCell.EndParagraphIndex].EndLogPosition;
  InsertParagraphWithDefaultProperties(APosition + 1);

  ACommand := TdxPieceTableInsertContentConvertedToDocumentModelCommand.Create(PieceTable, FCopyDocumentModel,
    APosition + 2, FForceVisible);
  try
    ACommand.CopyBetweenInternalModels := True;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxPieceTableSplitTableCommand.DeleteContent(ADeletingRange: TdxSelectionRange);
var
  ARows: TdxTableRowCollection;
  ALastRowIndex, I: Integer;
begin
  ARows := Table.Rows;
  ALastRowIndex := ARows.IndexOf(ARows.Last);

  for I := ALastRowIndex downto FRowIndex do
    PieceTable.DeleteEmptyTableRowCore(FTableIndex, I);
  PieceTable.DeleteContent(ADeletingRange.Start, ADeletingRange.Length, False);
end;

procedure TdxPieceTableSplitTableCommand.ExecuteCore;
var
  ATransaction: TdxHistoryTransaction;
  ASelectionRange: TdxSelectionRange;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    if FRowIndex = 0 then
      InsertParagraphBeforeTable
    else
    begin
      ASelectionRange := CalculateSelectionRange;
      try
        CopyToCopyPieceTable(ASelectionRange);
        DeleteContent(ASelectionRange);
        CopyToPieceTable;
      finally
        ASelectionRange.Free;
      end;
    end;
    Table.NormalizeCellColumnSpans;
    Table.NormalizeTableCellVerticalMerging;
  finally
    ATransaction.Free;
  end;
end;

function TdxPieceTableSplitTableCommand.GetCopyPieceTable: TdxPieceTable;
begin
  Result := FCopyDocumentModel.ActivePieceTable;
end;

function TdxPieceTableSplitTableCommand.GetParagraphs: TdxParagraphCollection;
begin
  Result := PieceTable.Paragraphs;
end;

function TdxPieceTableSplitTableCommand.GetTable: TdxTable;
begin
  Result := PieceTable.Tables[FTableIndex];
end;

procedure TdxPieceTableSplitTableCommand.InsertParagraphBeforeTable;
var
  AFirstCell, ANestedCell: TdxTableCell;
  AStartParagraphIndex: TdxParagraphIndex;
  APosition: TdxDocumentLogPosition;
begin
  AFirstCell := Table.Rows.First.Cells.First;
  AStartParagraphIndex := AFirstCell.StartParagraphIndex;
  APosition := Paragraphs[AStartParagraphIndex].LogPosition;
  InsertParagraphWithDefaultProperties(APosition);

  ANestedCell := PieceTable.Paragraphs[AStartParagraphIndex].GetCell;
  while ANestedCell <> AFirstCell do
  begin
    PieceTable.ChangeCellStartParagraphIndex(ANestedCell, ANestedCell.StartParagraphIndex + 1);
    ANestedCell := PieceTable.Paragraphs[AStartParagraphIndex].GetCell;
  end;
  PieceTable.ChangeCellStartParagraphIndex(AFirstCell, AStartParagraphIndex + 1);
end;

procedure TdxPieceTableSplitTableCommand.InsertParagraphWithDefaultProperties(APosition: TdxDocumentLogPosition);
var
  AInsertedParagraphIndex: TdxParagraphIndex;
  AInsertedParagraph: TdxParagraph;
  AParagraphStales: TdxParagraphStyleCollection;
  ARun: TdxTextRunBase;
begin
  PieceTable.InsertParagraph(APosition);
  AInsertedParagraphIndex := PieceTable.FindParagraphIndex(APosition);
  AInsertedParagraph := Paragraphs[AInsertedParagraphIndex];
  AInsertedParagraph.ParagraphProperties.Reset;
  AParagraphStales := DocumentModel.ParagraphStyles;
  AInsertedParagraph.ParagraphStyleIndex := AParagraphStales.IndexOf(AParagraphStales.DefaultItem);
  ARun := PieceTable.Runs[AInsertedParagraph.FirstRunIndex];
  ARun.CharacterProperties.Reset;
end;

{ TdxPieceTablePieceTableInsertColumnBase }

constructor TdxPieceTablePieceTableInsertColumnBase.Create(APieceTable: TdxPieceTable; APatternCell: TdxTableCell;
  AForceVisible: Boolean);
begin
  inherited Create(APieceTable);
  FPatternCell := APatternCell;
  FForceVisible := AForceVisible;
end;

procedure TdxPieceTablePieceTableInsertColumnBase.ChangeStartParagraphIndexInCells(ACells: TdxTableCellList);
var
  I, ACellsCount: Integer;
  ACurrentCell: TdxTableCell;
begin
  ACellsCount := ACells.Count;
  for I := 0 to ACellsCount - 1 do
  begin
    ACurrentCell := ACells[I];
    PieceTable.ChangeCellStartParagraphIndex(ACurrentCell, ACurrentCell.StartParagraphIndex + 1);
  end;
end;

procedure TdxPieceTablePieceTableInsertColumnBase.ExecuteCore;
var
  ATransaction: TdxHistoryTransaction;
  AColumnIndex, ARowsCount, I: Integer;
  ATable: TdxTable;
  ARows: TdxTableRowCollection;
  ACurrentRow: TdxTableRow;
  ACurrentCell: TdxTableCell;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AColumnIndex := GetColumnIndex;
    ATable := PatternCell.Table;

    ARows := ATable.Rows;
    ARowsCount := ARows.Count;
    for I := 0 to ARowsCount - 1 do
    begin
      ACurrentRow := ARows[I];
      ACurrentCell := GetCurrentCell(AColumnIndex, ACurrentRow);

      Modify(ACurrentRow, ACurrentCell);
    end;

    ATable.NormalizeTableGrid;
    ATable.NormalizeCellColumnSpans;
    NormalizeTableCellWidth(ATable);
  finally
    ATransaction.Free;
  end;
end;

function TdxPieceTablePieceTableInsertColumnBase.InsertColumnCore(ARow: TdxTableRow; AInsertedIndex: Integer; AStart,
  AEnd: TdxParagraphIndex): TdxTableCell;
begin
  Result := PieceTable.CreateTableCellCore(ARow, AInsertedIndex, AStart, AEnd);
end;

procedure TdxPieceTablePieceTableInsertColumnBase.InsertColumnToTheLeft(ACurrentRow: TdxTableRow;
  ACurrentCell: TdxTableCell);
var
  AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition;
  ACells: TdxTableCellList;
  ANewCell: TdxTableCell;
begin
  AParagraphIndex := ACurrentCell.StartParagraphIndex;
  ALogPosition := PieceTable.Paragraphs[AParagraphIndex].LogPosition;
  ACells := PieceTable.TableCellsManager.GetCellsByParagraphIndex(AParagraphIndex, ACurrentCell.Table.NestedLevel);
  try
    PieceTable.InsertParagraph(ALogPosition, ForceVisible);
    ChangeStartParagraphIndexInCells(ACells);
  finally
    ACells.Free;
  end;

  ANewCell := InsertColumnCore(ACurrentRow, ACurrentCell.IndexInRow, AParagraphIndex, AParagraphIndex);
  ANewCell.Properties.CopyFrom(ACurrentCell.Properties);
  ANewCell.StyleIndex := ACurrentCell.StyleIndex;
  ANewCell.Properties.PreferredWidth.CopyFrom(PatternCell.PreferredWidth);
  ANewCell.ColumnSpan := 1;
  ANewCell.VerticalMerging := TdxMergingState.None;
end;

procedure TdxPieceTablePieceTableInsertColumnBase.InsertColumnToTheRight(ACurrentRow: TdxTableRow;
  ACurrentCell: TdxTableCell);
var
  AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition;
  ANextCell, ANewCell: TdxTableCell;
begin
  AParagraphIndex := ACurrentCell.EndParagraphIndex;
  ALogPosition := PieceTable.Paragraphs[AParagraphIndex].EndLogPosition;
  PieceTable.InsertParagraph(ALogPosition, ForceVisible);
  PieceTable.ChangeCellEndParagraphIndex(ACurrentCell, ACurrentCell.EndParagraphIndex - 1);

  Inc(AParagraphIndex, 1);
  ANextCell := ACurrentCell.NextCellInRow;
  ANewCell := InsertColumnCore(ACurrentRow, ACurrentCell.IndexInRow + 1, AParagraphIndex, AParagraphIndex);
  ANewCell.StyleIndex := ACurrentCell.StyleIndex;
  if ANextCell <> nil then
    ANewCell.Properties.CopyFrom(ANextCell.Properties)
  else
    ANewCell.Properties.CopyFrom(ACurrentCell.Properties);
  ANewCell.Properties.PreferredWidth.CopyFrom(PatternCell.PreferredWidth);
  ANewCell.ColumnSpan := 1;
  ANewCell.VerticalMerging := TdxMergingState.None;
end;

procedure TdxPieceTablePieceTableInsertColumnBase.NormalizeTableCellWidth(ATable: TdxTable);
var
  I, J, AMaxWidth, ARowsCount, ACellsCount, ATotalWidth, ANewValue: Integer;
  ACurrentRow: TdxTableRow;
  ACell: TdxTableCell;
begin
  AMaxWidth := 5000;
  ARowsCount := ATable.Rows.Count;
  for J := 0 to ARowsCount - 1 do
  begin
    ACurrentRow := ATable.Rows[J];
    ACellsCount := ACurrentRow.Cells.Count;
    ATotalWidth := 0;
    for I := 0 to ACellsCount - 1 do
    begin
      ACell := ACurrentRow.Cells[I];
      if ACell.PreferredWidth.&Type = TdxWidthUnitType.FiftiethsOfPercent then
        Inc(ATotalWidth, ACell.PreferredWidth.Value)
      else
      begin
        ATotalWidth := 0;
        Break;
      end;
    end;
    if ATotalWidth <= AMaxWidth then
      Continue;

    for I := 0 to ACellsCount - 1 do
    begin
      ACell := ACurrentRow.Cells[I];
      ANewValue := (ACell.PreferredWidth.Value * AMaxWidth) div ATotalWidth;
      ACell.PreferredWidth.Value := ANewValue;
    end;
  end;
end;

{ TdxPieceTableInsertColumnToTheLeft }

function TdxPieceTableInsertColumnToTheLeft.GetColumnIndex: Integer;
begin
  Result := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(PatternCell, false);
end;

function TdxPieceTableInsertColumnToTheLeft.GetCurrentCell(AColumnIndex: Integer;
  ACurrentRow: TdxTableRow): TdxTableCell;
begin
  Result := TdxTableCellVerticalBorderCalculator.GetCellByColumnIndex(ACurrentRow, AColumnIndex);
end;

procedure TdxPieceTableInsertColumnToTheLeft.Modify(ACurrentRow: TdxTableRow; ACurrentCell: TdxTableCell);
begin
  if ACurrentCell = nil then
    ACurrentCell := ACurrentRow.Cells.Last;
  InsertColumnToTheLeft(ACurrentRow, ACurrentCell);
end;

{ TdxPieceTableInsertColumnToTheRight }

function TdxPieceTableInsertColumnToTheRight.GetColumnIndex: Integer;
begin
  Result := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(PatternCell, False) + PatternCell.ColumnSpan - 1;
end;

function TdxPieceTableInsertColumnToTheRight.GetCurrentCell(AColumnIndex: Integer;
  ACurrentRow: TdxTableRow): TdxTableCell;
begin
  Result := TdxTableCellVerticalBorderCalculator.GetCellByEndColumnIndex(ACurrentRow, AColumnIndex);
end;

procedure TdxPieceTableInsertColumnToTheRight.Modify(ACurrentRow: TdxTableRow; ACurrentCell: TdxTableCell);
begin
  if ACurrentCell = nil then
    InsertColumnToTheLeft(ACurrentRow, ACurrentRow.Cells.First)
  else
    InsertColumnToTheRight(ACurrentRow, ACurrentCell);
end;

{ TdxPieceTableDeleteTableColumnsCommand }

constructor TdxPieceTableDeleteTableColumnsCommand.Create(APieceTable: TdxPieceTable;
  ASelectedCells: TdxSelectedCellsCollection; const AServer: IdxInnerRichEditDocumentServerOwner);
begin
  inherited Create(APieceTable, AServer);
  Assert(ASelectedCells <> nil);
  FSelectedCells := ASelectedCells;
end;

procedure TdxPieceTableDeleteTableColumnsCommand.CalculateExecutionParameters;
var
  I, ABottomRowIndex: Integer;
  AFirstCell, ALastCell: TdxTableCell;
begin
  AFirstCell := FSelectedCells.NormalizedFirst.NormalizedStartCell;
  ALastCell := FSelectedCells.NormalizedFirst.NormalizedEndCell;
  StartColumnIndex := GetStartColumnIndex(AFirstCell);
  EndColumnIndex := GetEndColumnIndex(ALastCell);

  ABottomRowIndex := FSelectedCells.GetBottomRowIndex;
  for I := FSelectedCells.GetTopRowIndex to ABottomRowIndex do
  begin
    AFirstCell := FSelectedCells[I].NormalizedStartCell;
    ALastCell := FSelectedCells[I].NormalizedEndCell;
    StartColumnIndex := Max(StartColumnIndex, GetStartColumnIndex(AFirstCell));
    EndColumnIndex := Min(EndColumnIndex, GetEndColumnIndex(ALastCell));
  end;
end;

procedure TdxPieceTableDeleteTableColumnsCommand.DeleteCellsWithContent(ACells: TdxTableCellList;
  AContainer: TdxTableWidthsContainer);
var
  ACellsCount, I: Integer;
  ARow: TdxTableRow;
  ACurrentCell: TdxTableCell;
  ACommand: TdxPieceTableDeleteTableCellWithContentKnownWidthsCommand;
begin
  ACellsCount := ACells.Count;
  if ACellsCount = 0 then
    Exit;

  if IsSelectedEntireRow(ACells) then
  begin
    ARow := ACells[0].Row;
    if ARow.Table.Rows.Count = 1 then
      PieceTable.DeleteTableWithContent(ARow.Table)
    else
      PieceTable.DeleteTableRowWithContent(ARow);
    Exit;
  end;

  for I := ACellsCount - 1 downto 0 do
  begin
    ACurrentCell := ACells[I];
    if (ACurrentCell.VerticalMerging = TdxMergingState.Continue) and (ACurrentCell.IndexInRow = -1) then
      Continue;

    ACommand := TdxPieceTableDeleteTableCellWithContentKnownWidthsCommand.Create(PieceTable, ACurrentCell,
      DocumentServer, AContainer);
    try
      ACommand.CanNormalizeCellVerticalMerging := False;
      ACommand.UseDeltaBetweenColumnsUpdate := False;
      ACommand.CollectVerticalSpanCells := False;
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  end;
end;

procedure TdxPieceTableDeleteTableColumnsCommand.ExecuteCore;
var
  ATable: TdxTable;
  I, ARowsCount: Integer;
  ACurrentRow: TdxTableRow;
  ATableGrid: TdxTableGrid;
  AMerger: TdxTableGridMerger;
  ARows: TdxTableRowCollection;
  ATransaction: TdxHistoryTransaction;
  AContainer: TdxTableWidthsContainer;
  ACells, ACurrentCells: TdxTableCellList;
  ACalculator: TdxTableColumnWidthCalculator;
  AColumnCollection: TdxTableGridColumnCollection;
  ATableGridIntervals: TdxTableGridIntervalList;
  ADeletedCellsCollection: TdxObjectList<TdxTableCellList>;
  ANewCalculator: TdxTableColumnKnownWidthCalculator;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ATable := FSelectedCells.NormalizedFirst.Table;
    if FSelectedCells.IsSelectedEntireTable then
    begin
      TdxPieceTable(ATable.PieceTable).DeleteTableWithContent(ATable);
      Exit;
    end;
    ARows := ATable.Rows;
    ARowsCount := ARows.Count;
    ADeletedCellsCollection := TdxObjectList<TdxTableCellList>.Create;
    try
      ACalculator := TdxTableColumnWidthCalculator.Create(ATable, DocumentServer);
      try
        AContainer := ACalculator.CalculateWidths;
        try
          for I := ARowsCount - 1 downto 0 do
          begin
            ACurrentRow := ARows[I];
            ACells := TdxTableCellVerticalBorderCalculator.GetCellsByIntervalColumnIndex(ACurrentRow,
              StartColumnIndex, EndColumnIndex);
            ADeletedCellsCollection.Add(ACells);
          end;
          for I := 0 to ARowsCount - 1 do
          begin
            ACurrentCells := ADeletedCellsCollection[I];
            DeleteCellsWithContent(ACurrentCells, AContainer);
          end;

          ANewCalculator := TdxTableColumnKnownWidthCalculator.Create(ATable, AContainer);
          try
            AMerger := TdxTableGridMerger.Create([ATable]);
            try
              AColumnCollection := ANewCalculator.GetTableGridColumns;
              try
                ATableGridIntervals := CalculateTableGridIntervals(AColumnCollection);
                try
                  ATableGrid := TdxTableGrid.Create(ATableGridIntervals);
                  try
                    AMerger.MergeGrids([ATableGrid], [ATable.Rows.Count]);
                  finally
                    ATableGrid.Free;
                  end;
                finally
                  ATableGridIntervals.Free;
                end;
              finally
                AColumnCollection.Free;
              end;
            finally
              AMerger.Free;
            end;
          finally
            ANewCalculator.Free;
          end;
        finally
          AContainer.Clear;
        end;
      finally
        ACalculator.Free;
      end;
    finally
      ADeletedCellsCollection.Free;
    end;
    NormalizeCellVerticalMerging(ATable);
    ATable.NormalizeCellColumnSpans;
  finally
    ATransaction.Free;
  end;
end;

function TdxPieceTableDeleteTableColumnsCommand.GetEndColumnIndex(ALastCell: TdxTableCell): Integer;
begin
  Result := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(ALastCell, False) + ALastCell.ColumnSpan - 1;
end;

function TdxPieceTableDeleteTableColumnsCommand.GetStartColumnIndex(AFirstCell: TdxTableCell): Integer;
begin
  Result := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(AFirstCell, False);
end;

function TdxPieceTableDeleteTableColumnsCommand.IsAllCellsVerticalMergingContinue(
  ACells: TdxTableCellCollection): Boolean;
var
  I, ACellsCount: Integer;
begin
  ACellsCount := ACells.Count;
  for I := 0 to ACellsCount - 1 do
  begin
    if ACells[I].VerticalMerging <> TdxMergingState.Continue then
      Exit(False);
  end;
  PieceTable.DeleteTableRowWithContent(ACells.First.Row);
  Result := True;
end;

function TdxPieceTableDeleteTableColumnsCommand.IsSelectedEntireRow(ACells: TdxTableCellList): Boolean;
var
  ARow: TdxTableRow;
  AFirstCell, ALastCell: TdxTableCell;
begin
  AFirstCell := ACells[0];
  ALastCell := ACells[ACells.Count - 1];
  ARow := AFirstCell.Row;
  Result := (ARow.FirstCell = AFirstCell) and (ARow.LastCell = ALastCell);
end;

procedure TdxPieceTableDeleteTableColumnsCommand.NormalizeCellVerticalMerging(ATable: TdxTable);
var
  ARows: TdxTableRowCollection;
  I, J, ARowsCount, ACellsCount, AColumnIndex: Integer;
  ACurrentRow, ANextRow, APrevRow: TdxTableRow;
  ACells: TdxTableCellCollection;
  ACurrentCell, ANextCell, APrevCell: TdxTableCell;
  AVerticalMerging: TdxMergingState;
begin
  ARows := ATable.Rows;
  ARowsCount := ARows.Count - 1;
  for I := ARowsCount downto 0 do
  begin
    ACurrentRow := ARows[I];
    ACells := ACurrentRow.Cells;
    ACellsCount := ACells.Count;

    if IsAllCellsVerticalMergingContinue(ACells) then
      Continue;

    for J := 0 to ACellsCount - 1 do
    begin
      ACurrentCell := ACells[J];

      AVerticalMerging := ACurrentCell.VerticalMerging;
      if AVerticalMerging = TdxMergingState.None then
        Continue;

      AColumnIndex := ACurrentCell.GetStartColumnIndexConsiderRowGrid;
      ANextRow := ACurrentRow.Next;
      if AVerticalMerging = TdxMergingState.Restart then
      begin
        if I = ARowsCount then
        begin
          ACurrentCell.Properties.VerticalMerging := TdxMergingState.None;
          Continue;
        end;
        ANextCell := TdxTableCellVerticalBorderCalculator.GetCellByStartColumnIndex(ANextRow, AColumnIndex, False);
        if (ANextCell = nil) or (ANextCell.VerticalMerging <> TdxMergingState.Continue) then
          ACurrentCell.Properties.VerticalMerging := TdxMergingState.None;
      end
      else
      begin
        APrevRow := ACurrentRow.Previous;
        if APrevRow <> nil then
          APrevCell := TdxTableCellVerticalBorderCalculator.GetCellByStartColumnIndex(APrevRow, AColumnIndex, False)
        else
          APrevCell := nil;
        if ANextRow <> nil then
          ANextCell := TdxTableCellVerticalBorderCalculator.GetCellByStartColumnIndex(ANextRow, AColumnIndex, False)
        else
          ANextCell := nil;

        if ((APrevCell = nil) or (APrevCell.VerticalMerging = TdxMergingState.None)) and
          ((ANextCell = nil) or (ANextCell.VerticalMerging <> TdxMergingState.Continue)) then
        begin
          ACurrentCell.Properties.VerticalMerging := TdxMergingState.None;
          Continue;
        end;
        if ((APrevCell = nil) or (APrevCell.VerticalMerging = TdxMergingState.None)) and ((ANextCell <> nil) and
          (ANextCell.VerticalMerging = TdxMergingState.Continue)) then
          ACurrentCell.Properties.VerticalMerging := TdxMergingState.Restart;
      end;
    end;
  end;
end;

function TdxPieceTableDeleteTableColumnsCommand.CalculateTableGridIntervals(
  ATableGridColumnCollection: TdxTableGridColumnCollection): TdxTableGridIntervalList;
var
  ACount, I: Integer;
begin
  Result := TdxTableGridIntervalList.Create;
  ACount := ATableGridColumnCollection.Count;
  for I := 0 to ACount - 1 do
    Result.Add(TdxTableGridInterval.Create(ATableGridColumnCollection[I].Width, 1, TdxTableGridIntervalType.ModelUnit));
end;

{ TdxPieceTableDeleteTableCellsWithShiftToTheUpCommand }

constructor TdxPieceTableDeleteTableCellsWithShiftToTheUpCommand.Create(APieceTable: TdxPieceTable;
  ASelectedCells: TdxSelectedCellsCollection);
begin
  inherited Create(APieceTable);
  Assert(ASelectedCells <> nil);
  FSelectedCells := ASelectedCells;
end;

procedure TdxPieceTableDeleteTableCellsWithShiftToTheUpCommand.ExecuteCore;
var
  ATransaction: TdxHistoryTransaction;
  ASelectedRowsCount, I: Integer;
  ACurrentSelectedCells: TdxSelectedCellsIntervalInRow;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ASelectedRowsCount := FSelectedCells.RowsCount;
    for I := ASelectedRowsCount - 1 downto 0 do
    begin
      ACurrentSelectedCells := FSelectedCells[I];
      DeleteSelectedCells(ACurrentSelectedCells);
    end;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTableDeleteTableCellsWithShiftToTheUpCommand.DeleteSelectedCells(ASelectedCells: TdxSelectedCellsIntervalInRow);
var
  ALeftCellIndex, I: Integer;
  ADeletedCell: TdxTableCell;
  ACommand: TdxPieceTableDeleteTableCellWithShiftToTheUpCoreCommand;
begin
  ALeftCellIndex := ASelectedCells.NormalizedStartCellIndex;
  for I := ASelectedCells.NormalizedEndCellIndex downto ALeftCellIndex do
  begin
    ADeletedCell := ASelectedCells.Row.Cells[I];
    ACommand := TdxPieceTableDeleteTableCellWithShiftToTheUpCoreCommand.Create(PieceTable, ADeletedCell);
    try
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  end;
end;

{ TdxPieceTableDeleteTableCellWithShiftToTheUpCoreCommand }

constructor TdxPieceTableDeleteTableCellWithShiftToTheUpCoreCommand.Create(APieceTable: TdxPieceTable;
  ACell: TdxTableCell);
begin
  inherited Create(APieceTable);
  FCell := ACell;
end;

procedure TdxPieceTableDeleteTableCellWithShiftToTheUpCoreCommand.ExecuteCore;
var
  ATable: TdxTable;
  AColumnIndex, ARowsCount, I: Integer;
  ADeletedCell: TdxTableCell;
  ACommand: TdxPieceTableDeleteOneTableCellWithShiftToTheUpCommand;
begin
  ATable := FCell.Table;
  AColumnIndex := ATable.GetCellColumnIndexConsiderRowGrid(FCell);

  ARowsCount := ATable.Rows.Count;
  for I := FCell.RowIndex to ARowsCount - 1 do
  begin
    ADeletedCell := ATable.GetCell(I, AColumnIndex);
    ACommand := TdxPieceTableDeleteOneTableCellWithShiftToTheUpCommand.Create(PieceTable, ADeletedCell);
    try
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  end;
end;

{ TdxPieceTableDeleteOneTableCellWithShiftToTheUpCommand }

constructor TdxPieceTableDeleteOneTableCellWithShiftToTheUpCommand.Create(APieceTable: TdxPieceTable;
  ACell: TdxTableCell);
begin
  inherited Create(APieceTable, ACell);
  FRunInfo := PieceTable.GetRunInfoByTableCell(PatternCell);
end;

destructor TdxPieceTableDeleteOneTableCellWithShiftToTheUpCommand.Destroy;
begin
  FreeAndNil(FRunInfo);
  inherited Destroy;
end;

procedure TdxPieceTableDeleteOneTableCellWithShiftToTheUpCommand.DeleteContentInCell;
var
  AStartLogPosition: TdxDocumentLogPosition;
begin
  AStartLogPosition := FRunInfo.Start.LogPosition;
  PieceTable.DeleteContent(AStartLogPosition, FRunInfo.&End.LogPosition - AStartLogPosition + 1, False);
end;

procedure TdxPieceTableDeleteOneTableCellWithShiftToTheUpCommand.ExecuteCore;
begin
  if PatternCell.Row.IsLastRowInTable then
    DeleteContentInCell
  else
    inherited ExecuteCore;
end;

procedure TdxPieceTableDeleteOneTableCellWithShiftToTheUpCommand.FixParagraphsInPatternCell(
  ANeedDeleteFirstParagraphInCell, ANeedDeleteLastParagraphInCell: Boolean);
begin
  DeleteContentInCell;
end;

procedure TdxPieceTableDeleteOneTableCellWithShiftToTheUpCommand.UpdateProperties(ANextCell: TdxTableCell);
begin
end;

{ TdxPieceTableDeleteTableCellWithContentCommand }

constructor TdxPieceTableDeleteTableCellWithContentCommand.Create(APieceTable: TdxPieceTable;
  ADeletedCell: TdxTableCell; const AServer: IdxInnerRichEditDocumentServerOwner);
begin
  inherited Create(APieceTable, AServer);
  FDeletedCell := ADeletedCell;
  FCanNormalizeCellVerticalMerging := True;
  FCollectVerticalSpanCells := True;
  FUseDeltaBetweenColumnsUpdate := True;
end;

function TdxPieceTableDeleteTableCellWithContentCommand.CreateTableColumnWidthCalculator(
  ATable: TdxTable): TdxCustomTableColumnWidthCalculator;
begin
  Result := TdxTableColumnWidthCalculator.Create(ATable, DocumentServer);
end;

procedure TdxPieceTableDeleteTableCellWithContentCommand.ExecuteCore;
var
  ATransaction: TdxHistoryTransaction;
  ATable: TdxTable;
  APatternCellStartColumnIndex: Integer;
  AVerticalSpanCells: TdxTableCellList;
  ACalculator: TdxCustomTableColumnWidthCalculator;
  ANormalizeHelper: TdxNormalizeTableGridAfterDeleteCellHelper;
  I: Integer;
  ACurrentCell: TdxTableCell;
  ARunInfo: TdxRunInfo;
  AStartLogPosition: TdxDocumentLogPosition;
  AColumns: TdxTableGridColumnCollection;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ATable := FDeletedCell.Table;

    APatternCellStartColumnIndex := FDeletedCell.GetStartColumnIndexConsiderRowGrid;
    if CollectVerticalSpanCells then
      AVerticalSpanCells := TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(FDeletedCell, APatternCellStartColumnIndex, False)
    else
    begin
      AVerticalSpanCells := TdxTableCellList.Create;
      AVerticalSpanCells.Add(FDeletedCell);
    end;
    try
      ACalculator := CreateTableColumnWidthCalculator(ATable);
      try
        AColumns := ACalculator.GetTableGridColumns;
        ANormalizeHelper := TdxNormalizeTableGridAfterDeleteCellHelper.Create(ATable, AVerticalSpanCells,
          AColumns,  UseDeltaBetweenColumnsUpdate);
        try
          for I := AVerticalSpanCells.Count - 1 downto 0 do
          begin
            ACurrentCell := AVerticalSpanCells[I];
            if CanNormalizeCellVerticalMerging then
              ANormalizeHelper.NormalizeVerticalMerging(ACurrentCell);
            PieceTable.DeleteTableCellWithNestedTables(ATable.Index,
              ACurrentCell.RowIndex, ACurrentCell.IndexInRow);

            ARunInfo := PieceTable.GetRunInfoByTableCell(ACurrentCell);
            try
              AStartLogPosition := ARunInfo.Start.LogPosition;
              PieceTable.DeleteContent(AStartLogPosition, ARunInfo.&End.LogPosition - AStartLogPosition + 1,
                False, True);
            finally
              ARunInfo.Free;
            end;
          end;
          ANormalizeHelper.NormalizeColumnSpans;
          if CanNormalizeCellVerticalMerging then
            ATable.NormalizeRows;
          ATable.NormalizeTableGrid;
          ATable.NormalizeCellColumnSpans(False);
          ANormalizeHelper.NormalizeWidthAfter(ATable);
        finally
          ANormalizeHelper.Free;
        end;
      finally
        ACalculator.Free;
      end;
    finally
      AVerticalSpanCells.Free;
    end;
  finally
    ATransaction.Free;
  end;
end;

{ TdxPieceTableDeleteTableCellWithContentKnownWidthsCommand }

constructor TdxPieceTableDeleteTableCellWithContentKnownWidthsCommand.Create(
  APieceTable: TdxPieceTable; ADeletedCell: TdxTableCell;
  const AServer: IdxInnerRichEditDocumentServerOwner; const AContainer: TdxTableWidthsContainer);
begin
  inherited Create(APieceTable, ADeletedCell, AServer);
  FContainer := AContainer;
end;

function TdxPieceTableDeleteTableCellWithContentKnownWidthsCommand.CreateTableColumnWidthCalculator(
  ATable: TdxTable): TdxCustomTableColumnWidthCalculator;
begin
  Result := TdxTableColumnKnownWidthCalculator.Create(ATable, FContainer);
end;

{ TdxPieceTableInsertTableCellsBase }

constructor TdxPieceTableInsertTableCellsBase.Create(APieceTable: TdxPieceTable; APatternCell: TdxTableCell;
  AForceVisible: Boolean; const AServer: IdxInnerRichEditDocumentServerOwner);
begin
  inherited Create(APieceTable, AServer);
  Assert(APatternCell <> nil);
  FPatternCell := APatternCell;
  FCanNormalizeTable := True;
  FCanCopyProperties := True;
  FCanNormalizeVerticalMerging := True;
  FForceVisible := AForceVisible;
end;

procedure TdxPieceTableInsertTableCellsBase.ExecuteCore;
var
  ATransaction: TdxHistoryTransaction;
  ATable: TdxTable;
  APatternCellStartColumnIndex, I: Integer;
  ARestartCell, ACurrentCell, AInsertedCell: TdxTableCell;
  ACells: TdxTableCellList;
  ANormalizeHelper: TdxNormalizeTableGridAfterInsertCellHelper;
  ACalculator: TdxTableColumnWidthCalculator;
  ATableGridColumns: TdxTableGridColumnCollection;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ATable := PatternCell.Table;

    APatternCellStartColumnIndex := ATable.GetCellColumnIndexConsiderRowGrid(PatternCell);
    ARestartCell := ATable.GetFirstCellInVerticalMergingGroup(PatternCell);
    ACells := TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(ARestartCell, APatternCellStartColumnIndex, False);
    try

      ANormalizeHelper := nil;
      try
        if CanNormalizeTable then
        begin
          ACalculator := TdxTableColumnWidthCalculator.Create(ATable, DocumentServer);
          try
            ATableGridColumns := ACalculator.GetTableGridColumns;
          finally
            ACalculator.Free;
          end;
          ANormalizeHelper := TdxNormalizeTableGridAfterInsertCellHelper.Create(ATable, ACells,
            ATableGridColumns, True);
        end;

        for I := ACells.Count - 1 downto 0 do
        begin
          ACurrentCell := ACells[I];
          if CanNormalizeVerticalMerging then
            ANormalizeHelper.NormalizeVerticalMerging(ACurrentCell);
          AInsertedCell := Modify(ACurrentCell);
          if CanCopyProperties then
          begin
            AInsertedCell.Properties.CopyFrom(ACurrentCell.Properties);
            AInsertedCell.StyleIndex := ACurrentCell.StyleIndex;
          end;
        end;

        if CanNormalizeTable then
        begin
          ANormalizeHelper.NormalizeColumnSpans;

          NormalizeTableGridAfter(ATable);
          ATable.NormalizeCellColumnSpans;
          ANormalizeHelper.NormalizeWidthAfter(ATable);
        end;
      finally
        FreeAndNil(ANormalizeHelper);
      end;
    finally
      ACells.Free;
    end;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTableInsertTableCellsBase.NormalizeTableGridAfter(ATable: TdxTable);
var
  I, AMinEndColumnIndex, ARowsCount, ACurrentEndColumnIndex: Integer;
  ARows: TdxTableRowCollection;
  ACurrentRow: TdxTableRow;
begin
  AMinEndColumnIndex := MaxInt;
  ARows := ATable.Rows;
  ARowsCount := ARows.Count;
  for I := 0 to ARowsCount - 1 do
  begin
    ACurrentRow := ARows[I];
    ACurrentEndColumnIndex := ACurrentRow.LastCell.GetEndColumnIndexConsiderRowGrid + ACurrentRow.GridAfter;
    AMinEndColumnIndex := Min(AMinEndColumnIndex, ACurrentEndColumnIndex);
  end;

  for I := 0 to ARowsCount - 1 do
  begin
    ACurrentRow := ARows[I];
    ACurrentEndColumnIndex := ACurrentRow.LastCell.GetEndColumnIndexConsiderRowGrid + ACurrentRow.GridAfter;
    ACurrentRow.GridAfter := ACurrentRow.GridAfter - (ACurrentEndColumnIndex - AMinEndColumnIndex);
  end;
end;

{ TdxPieceTableInsertTableCellToTheLeft }

function TdxPieceTableInsertTableCellToTheLeft.Modify(ACell: TdxTableCell): TdxTableCell;
var
  AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition;
  ACells: TdxTableCellList;
begin
  AParagraphIndex := ACell.StartParagraphIndex;
  ALogPosition := PieceTable.Paragraphs[AParagraphIndex].LogPosition;

  ACells := PieceTable.TableCellsManager.GetCellsByParagraphIndex(AParagraphIndex, ACell.Table.NestedLevel);
  try
    PieceTable.InsertParagraph(ALogPosition, ForceVisible);
    ChangeStartParagraphIndexInCells(ACells);
    Result := PieceTable.CreateTableCellCore(ACell.Row, ACell.IndexInRow, AParagraphIndex, AParagraphIndex);
  finally
    ACells.Free;
  end;
end;

procedure TdxPieceTableInsertTableCellToTheLeft.ChangeStartParagraphIndexInCells(ACells: TdxTableCellList);
var
  I, ACellsCount: Integer;
  ACurrentCell: TdxTableCell;
begin
  ACellsCount := ACells.Count;
  for I := 0 to ACellsCount - 1 do
  begin
    ACurrentCell := ACells[I];
    PieceTable.ChangeCellStartParagraphIndex(ACurrentCell, ACurrentCell.StartParagraphIndex + 1);
  end;
end;

{ TdxPieceTableInsertTableCellToTheRight }

function TdxPieceTableInsertTableCellToTheRight.Modify(ACell: TdxTableCell): TdxTableCell;
var
  AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition;
  AInsertedIndex: Integer;
begin
  AParagraphIndex := ACell.EndParagraphIndex;
  ALogPosition := PieceTable.Paragraphs[AParagraphIndex].EndLogPosition;
  PieceTable.InsertParagraph(ALogPosition, ForceVisible);
  PieceTable.ChangeCellEndParagraphIndex(ACell, ACell.EndParagraphIndex - 1);

  AInsertedIndex := ACell.IndexInRow + 1;
  Result := PieceTable.CreateTableCellCore(ACell.Row, AInsertedIndex, AParagraphIndex + 1, AParagraphIndex + 1);
end;

{ TdxPieceTableInsertTableCellWithShiftToTheDownCommand }

constructor TdxPieceTableInsertTableCellWithShiftToTheDownCommand.Create(APieceTable: TdxPieceTable;
  APatternCell: TdxTableCell; AForceVisible: Boolean; const AServer: IdxInnerRichEditDocumentServerOwner);
begin
  inherited Create(APieceTable, AServer);
  Assert(APatternCell <> nil);
  FPatternCell := APatternCell;
  FForceVisible := AForceVisible;
end;

procedure TdxPieceTableInsertTableCellWithShiftToTheDownCommand.DeleteContentInPatternCell;
var
  ARunInfo: TdxRunInfo;
  AStartLogPosition: TdxDocumentLogPosition;
begin
  ARunInfo := PieceTable.GetRunInfoByTableCell(FPatternCell);
  try
    AStartLogPosition := ARunInfo.Start.LogPosition;
    PieceTable.DeleteContent(AStartLogPosition, ARunInfo.&End.LogPosition - AStartLogPosition + 1, False);
  finally
    ARunInfo.Free;
  end;
end;

procedure TdxPieceTableInsertTableCellWithShiftToTheDownCommand.ExecuteCore;
var
  ATransaction: TdxHistoryTransaction;
  ARows: TdxTableRowCollection;
  I, AStartRowIndex, AEndRowIndex, APatternCellIndexInRow, ACellsCountInCurrentRow: Integer;
  ACurrentRow, APreviousRow: TdxTableRow;
  ACurrentCell: TdxTableCell;
  ACommand: TdxPieceTableInsertTableCellWithShiftToTheDownCoreCommand;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    ARows := FPatternCell.Table.Rows;
    AStartRowIndex := FPatternCell.Row.IndexInTable;
    AEndRowIndex := ARows.Count - 1;
    APatternCellIndexInRow := FPatternCell.IndexInRow;
    for I := AEndRowIndex downto AStartRowIndex + 1 do
    begin
      ACurrentRow := ARows[I];
      ACellsCountInCurrentRow := ACurrentRow.Cells.Count;

      APreviousRow := ACurrentRow.Previous;
      if APatternCellIndexInRow >= APreviousRow.Cells.Count then
        Continue;

      if APatternCellIndexInRow >= ACellsCountInCurrentRow then
        InsertTableCells(ACurrentRow.LastCell, APatternCellIndexInRow - ACellsCountInCurrentRow + 1,
          APreviousRow.LastCell.PreferredWidth);

      ACurrentCell := ACurrentRow.Cells[APatternCellIndexInRow];
      ACommand := TdxPieceTableInsertTableCellWithShiftToTheDownCoreCommand.Create(PieceTable, ACurrentCell);
      try
        ACommand.Execute;
      finally
        ACommand.Free;
      end;
    end;
    DeleteContentInPatternCell;

    FPatternCell.Table.NormalizeTableGrid;
    FPatternCell.Table.NormalizeCellColumnSpans;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTableInsertTableCellWithShiftToTheDownCommand.InsertTableCells(APatternCell: TdxTableCell;
  AInsertedCellsCount: Integer; AWidth: TdxPreferredWidth);
var
  APatternCellIndexInRow, I, AIndexLastCell: Integer;
  ACellsInRow: TdxTableCellCollection;
  ACommand: TdxPieceTableInsertTableCellToTheRight;
begin
  APatternCellIndexInRow := APatternCell.IndexInRow;
  ACellsInRow := APatternCell.Row.Cells;
  for I := 0 to AInsertedCellsCount - 1 do
  begin
    AIndexLastCell := APatternCellIndexInRow + I;
    ACommand := TdxPieceTableInsertTableCellToTheRight.Create(PieceTable, ACellsInRow[AIndexLastCell],
      FForceVisible, DocumentServer);
    try
      ACommand.CanNormalizeTable := False;
      ACommand.CanCopyProperties := False;
      ACommand.Execute;
    finally
      ACommand.Free;
    end;

    ACellsInRow[AIndexLastCell + 1].Properties.PreferredWidth.CopyFrom(AWidth);
  end;
end;

{ TdxPieceTableInsertTableCellWithShiftToTheDownCoreCommand }

function TdxPieceTableInsertTableCellWithShiftToTheDownCoreCommand.CalculateNextCell: TdxTableCell;
begin
  Result := PatternCell.Table.Rows[PatternCell.Row.IndexInTable - 1].Cells[PatternCell.IndexInRow];
end;

procedure TdxPieceTableInsertTableCellWithShiftToTheDownCoreCommand.DeleteTableCellWithContent(ANextCell: TdxTableCell;
  ADeletingRange: TdxSelectionRange);
var
  ASelectionRangeForPatternCell: TdxSelectionRange;
begin
  ASelectionRangeForPatternCell := CalculateSelectionRange(PatternCell);
  try
    PieceTable.DeleteContent(ASelectionRangeForPatternCell.Start, ASelectionRangeForPatternCell.Length, False);
  finally
    ASelectionRangeForPatternCell.Free;
  end;
end;

procedure TdxPieceTableInsertTableCellWithShiftToTheDownCoreCommand.FixParagraphsInPatternCell(
  ANeedDeleteFirstParagraphInCell, ANeedDeleteLastParagraphInCell: Boolean);
begin
  inherited FixParagraphsInPatternCell(True, False);
end;

procedure TdxPieceTableInsertTableCellWithShiftToTheDownCoreCommand.UpdateProperties(ANextCell: TdxTableCell);
begin
  PatternCell.Properties.Borders.CopyFrom(ANextCell.Properties.Borders);
  PatternCell.Properties.BackgroundColor := ANextCell.BackgroundColor;
  PatternCell.Properties.VerticalAlignment := ANextCell.VerticalAlignment;
end;

{ TdxPieceTableSplitTableCellsHorizontally }

constructor TdxPieceTableSplitTableCellsHorizontally.Create(APieceTable: TdxPieceTable; APatternCell: TdxTableCell;
  APartsCount: Integer; AForceVisible: Boolean; const AServer: IdxInnerRichEditDocumentServerOwner);
begin
  inherited Create(APieceTable, AServer);
  FPatternCell := APatternCell;
  FPartsCount := APartsCount;
  FForceVisible := AForceVisible;
end;

procedure TdxPieceTableSplitTableCellsHorizontally.ExecuteCore;
var
  ATransaction: TdxHistoryTransaction;
  ATableGridColumns: TdxTableGridColumnCollection;
  APatternCellStartColumnIndex, AOldWidthPatternCell, ALastCellIndex, I: Integer;
  AVerticalSpanCells: TdxTableCellList;
  ACalculator: TdxTableColumnWidthCalculator;
  ANormalizeHelper: TdxNormalizeTableGridAfterSplitCellHelper;
  ACommand: TdxPieceTableInsertTableCellToTheRight;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    APatternCellStartColumnIndex := FPatternCell.GetStartColumnIndexConsiderRowGrid;
    AVerticalSpanCells := TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(FPatternCell, APatternCellStartColumnIndex, False);
    try
      ACalculator := TdxTableColumnWidthCalculator.Create(FPatternCell.Table, DocumentServer);
      try
        ATableGridColumns := ACalculator.GetTableGridColumns;
        ANormalizeHelper := TdxNormalizeTableGridAfterSplitCellHelper.Create(FPatternCell.Table, AVerticalSpanCells,
          FPartsCount, ATableGridColumns, True);
        try
          AOldWidthPatternCell := FPatternCell.PreferredWidth.Value;
          ALastCellIndex := AVerticalSpanCells.Count - 1;
          for I := ALastCellIndex downto 0 do
            AVerticalSpanCells[I].PreferredWidth.Value := AVerticalSpanCells[I].PreferredWidth.Value div FPartsCount;

          for I := 0 to FPartsCount - 1 - 1 do
          begin
            ACommand := TdxPieceTableInsertTableCellToTheRight.Create(PieceTable, FPatternCell, FForceVisible, DocumentServer);
            try
              ACommand.CanNormalizeTable := False;
              ACommand.CanNormalizeVerticalMerging := False;
              ACommand.ExecuteCore;
            finally
              ACommand.Free;
            end;
          end;
          NormalizeTableCellsWidth(AOldWidthPatternCell);
          ANormalizeHelper.NormalizeColumnSpans;
        finally
          ANormalizeHelper.Free;
        end;
      finally
        ACalculator.Free;
      end;
    finally
      AVerticalSpanCells.Free;
    end;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxPieceTableSplitTableCellsHorizontally.NormalizeTableCellsWidth(AOldWidth: Integer);
var
  ACells: TdxTableCellCollection;
  ACurrentCell: TdxTableCell;
  AWidthAllNewCells, AStartIndex, AEndIndex, I, ADelta: Integer;
begin
  AWidthAllNewCells := 0;
  AStartIndex := FPatternCell.IndexInRow;
  AEndIndex := AStartIndex + FPartsCount;
  ACells := FPatternCell.Row.Cells;
  for I := AStartIndex to AEndIndex - 1 do
  begin
    ACurrentCell := ACells[I];
    Inc(AWidthAllNewCells, ACurrentCell.PreferredWidth.Value);
  end;
  ADelta := AOldWidth - AWidthAllNewCells;
  Assert(AStartIndex + ADelta <= AEndIndex);

  AEndIndex := AStartIndex + ADelta;
  for I := AStartIndex to AEndIndex - 1 do
  begin
    ACurrentCell := ACells[I];
    ACurrentCell.PreferredWidth.Value := ACurrentCell.PreferredWidth.Value + 1;
  end;
end;

{ TdxPieceTableSplitTableCellsVertically }

constructor TdxPieceTableSplitTableCellsVertically.Create(APieceTable: TdxPieceTable; APatternCell: TdxTableCell;
  APartsCount, AColumnsCount: Integer; AForceVisible: Boolean);
begin
  inherited Create(APieceTable);
  Assert(APatternCell <> nil);
  FPatternCell := APatternCell;
  FPartsCount := APartsCount;
  FColumnsCount := AColumnsCount;
  FForceVisible := AForceVisible;
end;

procedure TdxPieceTableSplitTableCellsVertically.ExecuteCore;
var
  ATransaction: TdxHistoryTransaction;
  ARow: TdxTableRow;
  ANormStartIndex, ACellsCount, AEndIndex, ADelta, I: Integer;
  ACellsInRow: TdxTableCellCollection;
  ACurrentCell, AMergeCell: TdxTableCell;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    if FPartsCount = 1 then
      Exit;

    ARow := FPatternCell.Row;
    ANormStartIndex := FPatternCell.IndexInRow;
    ACellsInRow := ARow.Cells;
    if ACellsInRow[ANormStartIndex].VerticalMerging = TdxMergingState.Restart then
    begin
      SplitMergedCellsVertically(ACellsInRow);
      Exit;
    end;

    InsertRows(ARow);
    ACellsCount := ACellsInRow.Count;

    AEndIndex := GetSelectedCellsEndIndex;
    for I := 0 to ACellsCount - 1 do
      if (I < ANormStartIndex) or (I > AEndIndex) then
      begin
        ACurrentCell := ACellsInRow[I];
        AMergeCell := ACurrentCell.Table.GetFirstCellInVerticalMergingGroup(ACurrentCell);
        ADelta := ACurrentCell.RowIndex - AMergeCell.RowIndex;
        PieceTable.MergeTableCellsVertically(AMergeCell, FPartsCount + ADelta);
      end;
  finally
    ATransaction.Free;
  end;
end;

function TdxPieceTableSplitTableCellsVertically.GetSelectedCellsEndIndex: Integer;
begin
	Result := FPatternCell.IndexInRow + (FColumnsCount - 1);
end;

procedure TdxPieceTableSplitTableCellsVertically.InsertRows(ARow: TdxTableRow);
var
  I: Integer;
begin
  ARow.Height.Value := ARow.Height.Value div FPartsCount;
  for I := 0 to FPartsCount - 1 - 1 do
    PieceTable.InsertTableRowBelow(ARow, FForceVisible);
end;

procedure TdxPieceTableSplitTableCellsVertically.SplitMergedCellsVertically(ACellsInRow: TdxTableCellCollection);
var
  AEndIndex, I: Integer;
begin
  AEndIndex := GetSelectedCellsEndIndex;
  for I := FPatternCell.IndexInRow to AEndIndex do
    SplitMergedCellsVerticallyCore(ACellsInRow[I]);
end;

procedure TdxPieceTableSplitTableCellsVertically.SplitMergedCellsVerticallyCore(ARestartCell: TdxTableCell);
var
  AMergedCells: TdxTableCellList;
  I, AColumnIndex, AMergedCellsCount, ATotalRowsCount: Integer;
begin
  AColumnIndex := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(ARestartCell, False);
  AMergedCells := TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(ARestartCell, AColumnIndex, False);
  try
    AMergedCellsCount := AMergedCells.Count;
    if AMergedCellsCount = FPartsCount then
    begin
      for I := 0 to AMergedCellsCount - 1 do
        AMergedCells[I].VerticalMerging := TdxMergingState.None;
      Exit;
    end;
    ATotalRowsCount := AMergedCellsCount div FPartsCount;
    for I := 0 to AMergedCellsCount - 1 do
      if I mod ATotalRowsCount = 0 then
        AMergedCells[I].VerticalMerging := TdxMergingState.Restart;
  finally
    AMergedCells.Free;
  end;
end;

{ TdxPositionInfo }

constructor TdxPositionInfo.Create(APosition, AMinLeft, AMaxRight: Integer);
begin
  FPosition := APosition;
  FMinLeft := AMinLeft;
  FMaxRight := AMaxRight;
end;

{ TdxTableGridMerger }

constructor TdxTableGridMerger.Create(const ATables: array of TdxTable);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FTables, Length(ATables));
  for I := Low(ATables) to High(ATables) do
    FTables[I] := ATables[I];
end;

function TdxTableGridMerger.GetPositions(const APositionInfo: TArray<TdxPositionInfo>): TArray<Integer>;
var
  I: Integer;
begin
  SetLength(Result, Length(APositionInfo));
  for I := 0 to Length(APositionInfo) - 1 do
    Result[I] := APositionInfo[I].Position;
end;

procedure TdxTableGridMerger.MergeGrids(const ATableGrids: array of TdxTableGrid;
  const ASourceTableRowsCount: array of Integer);
var
  ATotalPosition: TdxIntegerList;
  AAbsolutePositions: TList<TArray<Integer>>;
  AResultTable: TdxTable;
  ACount, ARowIndex, I: Integer;
begin
  ATotalPosition := TdxIntegerList.Create;
  try
    AAbsolutePositions := CalculateMergedPositions(FTables, ATableGrids, ATotalPosition, ASourceTableRowsCount);
    try
      AResultTable := FTables[0];
      ACount := Length(FTables);
      ARowIndex := 0;
      for I := 0 to ACount - 1 do
      begin
        MergedGridCore(AResultTable, ARowIndex, ARowIndex + ASourceTableRowsCount[I] - 1,
          AAbsolutePositions[I], ATotalPosition);
        Inc(ARowIndex, ASourceTableRowsCount[I]);
      end;
    finally
      AAbsolutePositions.Free;
    end;
  finally
    ATotalPosition.Free;
  end;
  AResultTable.NormalizeTableGrid;
  AResultTable.NormalizeCellColumnSpans;
end;

function TdxTableGridMerger.CalculateMergedPositions(const ATables: array of TdxTable; const ATableGrids: array of TdxTableGrid;
   ATotalPositions: TdxIntegerList; const ASourceTableRowsCount: array of Integer): TList<TArray<Integer>>;
var
  AAbsolutePositions: TList<TArray<TdxPositionInfo>>;
  ACount, I: Integer;
begin
  ACount := Length(ATableGrids);
  AAbsolutePositions := TList<TArray<TdxPositionInfo>>.Create;
  try
    AAbsolutePositions.Capacity := ACount;
    for I := 0 to ACount - 1 do
      AAbsolutePositions.Add(CalculateAbsolutePositions(ATables[I], ATableGrids[I].Columns, ASourceTableRowsCount[I]));
    AdjustNearestPositions(AAbsolutePositions, Delta, ATotalPositions);
    Result := TList<TArray<Integer>>.Create;
    Result.Capacity := ACount;
    for I := 0 to ACount - 1 do
      Result.Add(GetPositions(AAbsolutePositions[I]));
  finally
    AAbsolutePositions.Free;
  end;
end;

function TdxTableGridMerger.CalculateAbsolutePositions(ATable: TdxTable; AColumns: TdxTableGridColumnCollection;
  ARowCount: Integer): TArray<TdxPositionInfo>;
var
  AColumnCount, ACurrentPosition, AStartIndex, AEndIndex, I, J: Integer;
  APositions: TArray<TdxPositionInfo>;
  ARows: TdxTableRowCollection;
  ARow: TdxTableRow;
  ACells: TdxTableCellCollection;
begin
  AColumnCount := AColumns.Count;
  SetLength(APositions, AColumnCount + 1);

  ACurrentPosition := 0;
  APositions[0] := TdxPositionInfo.Create(0, 0, MaxInt);
  for I := 0 to AColumnCount - 1 do
  begin
    Inc(ACurrentPosition, AColumns[I].Width);
    APositions[I + 1] := TdxPositionInfo.Create(ACurrentPosition, MaxInt, MaxInt);
  end;
  ARows := ATable.Rows;
  for I := 0 to ARowCount - 1 do
  begin
    ARow := ARows[I];
    ACells := ARow.Cells;
    AStartIndex := ARow.GridBefore;
    for J := 0 to ACells.Count - 1 do
    begin
      AEndIndex := AStartIndex + ACells[J].ColumnSpan;
      APositions[AStartIndex].MaxRight := AEndIndex;
      APositions[AEndIndex].MinLeft := AStartIndex;
      AStartIndex := AEndIndex;
    end;
  end;
  Result := APositions;
end;

function TdxTableGridMerger.AdjustLeft(ATotalPositions: TdxIntegerList; ATotalPositionIndex: Integer;
  const APositions: TArray<TdxPositionInfo>; APositionIndex, ALayoutDelta: Integer; APos: TdxPositionInfo): Boolean;
var
  APrevPosition: Integer;
begin
  if ATotalPositionIndex <= 0 then
    Exit(False);
  APrevPosition := ATotalPositions[ATotalPositionIndex - 1];
  if ((APos.Position - APrevPosition < ALayoutDelta) and
    ((APositionIndex = 0) or (APrevPosition >= APositions[APositionIndex - 1].Position))) and
    ((APos.MinLeft < 0) or (APrevPosition > APositions[APos.MinLeft].Position)) then
  begin
    APositions[APositionIndex].Position := APrevPosition;
    Result := True;
  end
  else
    Result := False;
end;

procedure TdxTableGridMerger.AdjustNearestPositions(const AAbsolutePositions: TList<TArray<TdxPositionInfo>>;
  ALayoutDelta: Integer; const ATotalPositions: TdxIntegerList);
var
  ACount, I, J, APos, AIndex: Integer;
  APositions: TArray<TdxPositionInfo>;
begin
  ATotalPositions.Add(0);
  ACount := AAbsolutePositions.Count;
  for I := 0 to ACount - 1 do
  begin
    APositions := AAbsolutePositions[I];
    for J := Low(APositions) to High(APositions) do
    begin
      APos := APositions[J].Position;
      if ATotalPositions.BinarySearch(APos, AIndex) then
        Continue;
      AdjustPosition(ATotalPositions, AIndex, APositions, J, ALayoutDelta);
    end;
  end;
end;

procedure TdxTableGridMerger.AdjustPosition(const ATotalPositions: TdxIntegerList; ATotalPositionIndex: Integer;
  const APositions: TArray<TdxPositionInfo>; APositionIndex: Integer; ALayoutDelta: Integer);
var
  APos: TdxPositionInfo;
begin
  APos := APositions[APositionIndex];
  if AdjustRight(ATotalPositions, ATotalPositionIndex, APositions, APositionIndex, ALayoutDelta, APos) then
    Exit;
  if AdjustLeft(ATotalPositions, ATotalPositionIndex, APositions, APositionIndex, ALayoutDelta, APos) then
    Exit;
  ATotalPositions.Insert(ATotalPositionIndex, APos.Position);
end;

function TdxTableGridMerger.AdjustRight(ATotalPositions: TdxIntegerList; ATotalPositionIndex: Integer;
  const APositions: TArray<TdxPositionInfo>; APositionIndex, ALayoutDelta: Integer; APos: TdxPositionInfo): Boolean;
var
  ANextPosition: Integer;
begin
  if ATotalPositionIndex >= ATotalPositions.Count then
    Exit(False);
  ANextPosition := ATotalPositions[ATotalPositionIndex];
  if ((ANextPosition - APos.Position < ALayoutDelta) and
    ((APositionIndex + 1 >= Length(APositions)) or (ANextPosition < APositions[APositionIndex + 1].Position))) and
    ((APos.MaxRight >= Length(APositions)) or (ANextPosition < APositions[APos.MaxRight].Position)) then
  begin
    APositions[APositionIndex].Position := ANextPosition;
    Result := True;
  end
  else
    Result := False;
end;

procedure TdxTableGridMerger.MergedGridCore(ATable: TdxTable; AStartRowIndex: Integer; AEndRowIndex: Integer;
  const ATablePositions: TArray<Integer>; ATotalPositions: TdxIntegerList);
var
  ARowIndex, AStartIndex, ARowGridBefore, AEndIndex, ACellsCount, ACellIndex, ARowGridAfter: Integer;
  ARow: TdxTableRow;
  ACells: TdxTableCellCollection;
  ACell: TdxTableCell;
begin
  for ARowIndex := AStartRowIndex to AEndRowIndex do
  begin
    ARow := ATable.Rows[ARowIndex];
    AStartIndex := 0;

    ARowGridBefore := ARow.GridBefore;
    if ARowGridBefore <> 0 then
    begin
      AEndIndex := AStartIndex + ARowGridBefore;
      ARow.GridBefore := CalculateColumnSpan(ATotalPositions, ATablePositions[0], ATablePositions[AEndIndex]);
      AStartIndex := AEndIndex;
    end;
    ACells := ARow.Cells;
    ACellsCount := ARow.Cells.Count;

    for ACellIndex := 0 to ACellsCount - 1 do
    begin
      ACell := ACells[ACellIndex];
      AEndIndex := AStartIndex + ACell.ColumnSpan;
      ACell.ColumnSpan := CalculateColumnSpan(ATotalPositions, ATablePositions[AStartIndex], ATablePositions[AEndIndex]);
      AStartIndex := AEndIndex;
    end;
    ARowGridAfter := ARow.GridAfter;
    if ARowGridAfter <> 0 then
    begin
      AEndIndex := Min(AStartIndex + ARowGridAfter, High(ATablePositions));
      ARow.GridAfter := CalculateColumnSpan(ATotalPositions, ATablePositions[AStartIndex], ATablePositions[AEndIndex]);
    end;
  end;
end;

function TdxTableGridMerger.CalculateColumnSpan(const ATotalPositions: TdxIntegerList; AStartPos: Integer; AEndPos: Integer): Integer;
var
  AStartIndex, AEndIndex: Integer;
begin
  if ATotalPositions.BinarySearch(AEndPos, AEndIndex) and ATotalPositions.BinarySearch(AStartPos, AStartIndex) then
    Result := AEndIndex - AStartIndex
  else
    raise TdxInternalException.Create;
end;

{ TdxPieceTableJoinSeveralTablesCommand }

constructor TdxPieceTableJoinSeveralTablesCommand.Create(APieceTable: TdxPieceTable; const ATables: TdxTableArray);
var
  AFirstTableLastEndParagraphIndex: TdxParagraphIndex;
begin
  inherited Create(APieceTable);
  Assert(Length(ATables) >= 2);
  FTables := ATables;
  AFirstTableLastEndParagraphIndex := ATables[0].LastRow.LastCell.EndParagraphIndex;
  FJoinPosition := PieceTable.Paragraphs[AFirstTableLastEndParagraphIndex].EndLogPosition;
end;

procedure TdxPieceTableJoinSeveralTablesCommand.ExecuteCore;
var
  AColumnWidth, ACount, ATableIndex, ARowCountInBottomTable, I: Integer;
  ATableGrids: TdxObjectList<TdxTableGrid>;
  ATransaction: TdxHistoryTransaction;
  ASourceTablesRowCount: TIntegerDynArray;
  AResultTable, ATable: TdxTable;
  ARowsInBottomTable: TdxTableRowCollection;
  ACurrentBottomRow: TdxTableRow;
  AMerger: TdxTableGridMerger;
begin
  AColumnWidth := CalculateColumnWidth;
  ACount := Length(FTables);
  ATableGrids := TdxObjectList<TdxTableGrid>.Create;
  try
    for I := 0 to ACount - 1 do
      ATableGrids.Add(nil);
    for ATableIndex := 0 to ACount - 1 do
      ATableGrids[ATableIndex] := GetTableGrid(FTables[ATableIndex], AColumnWidth);

    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      SetLength(ASourceTablesRowCount, ACount);
      AResultTable := FTables[0];
      ASourceTablesRowCount[0] := AResultTable.Rows.Count;

      for ATableIndex := 1 to ACount - 1 do
      begin
        ATable := FTables[ATableIndex];
        ASourceTablesRowCount[ATableIndex] := ATable.Rows.Count;
        ARowsInBottomTable := ATable.Rows;
        ARowCountInBottomTable := ARowsInBottomTable.Count;
        for I := 0 to ARowCountInBottomTable - 1 do
        begin
          ACurrentBottomRow := ATable.Rows[I];
          PieceTable.MoveTableRowToOtherTable(AResultTable, ACurrentBottomRow);
        end;
        PieceTable.DeleteTableFromTableCollection(ATable);
      end;
      AMerger := TdxTableGridMerger.Create(FTables);
      try
        AMerger.MergeGrids(ATableGrids.ToArray, ASourceTablesRowCount);
      finally
        AMerger.Free;
      end;
    finally
      ATransaction.Free;
    end;
  finally
    ATableGrids.Free;
  end;
end;

function TdxPieceTableJoinSeveralTablesCommand.CalculateColumnWidth: Integer;
var
  ASection: TdxSection;
  ABounds: TdxRectList;
  ASectionIndex: TdxSectionIndex;
  AConverter: TdxDocumentModelTwipsToLayoutDocumentsConverter;
  APageBoundsCalculator: TdxPageBoundsCalculator;
  AColumnsBoundsCalculator: TdxColumnsBoundsCalculator;
begin
  ASectionIndex := DocumentModel.FindSectionIndex(FJoinPosition);
  ASection := DocumentModel.Sections[ASectionIndex];

  AConverter := TdxDocumentModelTwipsToLayoutDocumentsConverter.Create;
  try
    APageBoundsCalculator := TdxPageBoundsCalculator.Create(AConverter);
    try
      AColumnsBoundsCalculator := TdxColumnsBoundsCalculator.Create(AConverter);
      try
        ABounds := AColumnsBoundsCalculator.Calculate(ASection, APageBoundsCalculator.CalculatePageClientBounds(ASection));
        try
          Result := ABounds[0].Width;
        finally
          ABounds.Free;
        end;
      finally
        AColumnsBoundsCalculator.Free;
      end;
    finally
      APageBoundsCalculator.Free;
    end;
  finally
    AConverter.Free;
  end;
end;

function TdxPieceTableJoinSeveralTablesCommand.GetTableGrid(ATable: TdxTable; APercentWidthBase: Integer): TdxTableGrid;
var
  AWidthsCalculator: TdxJoinTableWidthsCalculator;
  ACalculator: TdxTableGridCalculator;
begin
  AWidthsCalculator := TdxJoinTableWidthsCalculator.Create(DocumentModel.ToDocumentLayoutUnitConverter, APercentWidthBase);
  try
    ACalculator := TdxTableGridCalculator.Create(AWidthsCalculator, APercentWidthBase);
    try
      Result := ACalculator.CalculateTableGrid(ATable, APercentWidthBase);
    finally
      ACalculator.Free;
    end;
  finally
    AWidthsCalculator.Free;
  end;
end;

function TdxPieceTableJoinSeveralTablesCommand.CalculateColumnIndex(APosition: Integer;
  AColumnWidthCollection: TdxIntegerList; var ACorrection: Integer): Integer;
var
  AIndex, AColumnWidthCount, AKey, AValue: Integer;
begin
  AIndex := AColumnWidthCollection.IndexOf(APosition);
  if AIndex >= 0 then
    Exit(AIndex);

  AColumnWidthCount := AColumnWidthCollection.Count;
  for AKey := 0 to AColumnWidthCount - 1 do
  begin
    AValue := AColumnWidthCollection[AKey] - APosition;
    if Abs(AValue) < TdxTableGridMerger.Delta then
    begin
      Inc(ACorrection, AValue);
      Exit(AKey);
    end;
  end;
  raise TdxInternalException.Create;
end;

{ TdxPieceTableJoinTablesCommand }

constructor TdxPieceTableJoinTablesCommand.Create(APieceTable: TdxPieceTable; ATopTable, ABottomTable: TdxTable);
var
  ATables: TdxTableArray;
begin
  SetLength(ATables, 2);
  ATables[0] := ATopTable;
  ATables[1] := ABottomTable;
  inherited Create(APieceTable, ATables);
  Assert(ATopTable <> nil);
  Assert(ABottomTable <> nil);
end;

{ TdxColumnIndexParameters }

constructor TdxColumnIndexParameters.Create(AColumnIndex: Integer; AIsExactly: Boolean);
begin
  ColumnIndex := AColumnIndex;
  IsExactly := AIsExactly;
end;

{ TdxTableCellParameters }

constructor TdxTableCellParameters.Create(AIndexInRow, ARowIndex, AStartColumnIndex, AEndColumnIndex: Integer);
begin
  IndexInRow := AIndexInRow;
  RowIndex := ARowIndex;
  StartColumnIndex := AStartColumnIndex;
  EndColumnIndex := AEndColumnIndex;
end;

{ TdxNormalizeTableGridHelperBase }

constructor TdxNormalizeTableGridHelperBase.Create(ATable: TdxTable; ACells: TdxTableCellList;
  AGridColumns: TdxTableGridColumnCollection; AUseDeltaBetweenColumns: Boolean);
begin
  inherited Create;
  Assert(ACells <> nil);
  Assert(ATable <> nil);
  FTable := ATable;
  FPatternCellsParameters := CalculatePatternCellsParameters(ACells);
  FGridColumns := AGridColumns;
  if AUseDeltaBetweenColumns then
    FDeltaBetweenColumnsUpdate := DeltaBetweenColumns
  else
    FDeltaBetweenColumnsUpdate := 0;
end;

destructor TdxNormalizeTableGridHelperBase.Destroy;
begin
  FreeAndNil(FPatternCellsParameters);
  inherited Destroy;
end;

function TdxNormalizeTableGridHelperBase.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(Table.DocumentModel);
end;

function TdxNormalizeTableGridHelperBase.CalculatePatternCellsParameters(ACells: TdxTableCellList): TList<TdxTableCellParameters>;
var
  ACell: TdxTableCell;
  AParameters: TdxTableCellParameters;
  ACellsCount, I, AStartColumnIndex, AEndColumnIndex: Integer;
begin
  Result := TList<TdxTableCellParameters>.Create;
  ACellsCount := ACells.Count;
  for I := 0 to ACellsCount - 1 do
  begin
    ACell := ACells[I];
    AStartColumnIndex := ACell.GetStartColumnIndexConsiderRowGrid;
    AEndColumnIndex := ACell.GetEndColumnIndexConsiderRowGrid(AStartColumnIndex);
    AParameters := TdxTableCellParameters.Create(ACell.IndexInRow, ACell.RowIndex, AStartColumnIndex, AEndColumnIndex);
    Result.Add(AParameters);
  end;
end;

procedure TdxNormalizeTableGridHelperBase.NormalizeColumnSpans;
var
  ARowContainsPatternCellIndexes, AMergedDistances: TdxIntegerList;
  ADistancesToNewCells: TdxList<TdxDistanceCollection>;
  ARows: TdxTableRowCollection;
  ARowsCount, I: Integer;
begin
  ARowContainsPatternCellIndexes := GetRowContainsPatternCellIndexes;
  try
    ADistancesToNewCells := CalculateDistancesToNewCells;
    try
      AMergedDistances := MergeDistances(ADistancesToNewCells);
      try
        SnapDistanceToColumnGrid(AMergedDistances, ADistancesToNewCells);
        ARows := Table.Rows;
        ARowsCount := ARows.Count;
        for I := 0 to ARowsCount - 1 do
          if not ARowContainsPatternCellIndexes.Contains(I) then
            NormalizeColumnSpansCore(AMergedDistances, ARows[I]);
        RecalculateGridIntervals(AMergedDistances);
        NormalizeColumnSpansInRowContainingPatternCell(ADistancesToNewCells);
        FTable.ResetCachedLayoutInfo;
      finally
        AMergedDistances.Free;
      end;
    finally
      ADistancesToNewCells.Free;
    end;
  finally
    ARowContainsPatternCellIndexes.Free;
  end;
end;

procedure TdxNormalizeTableGridHelperBase.SnapDistanceToColumnGrid(AMergedDistances: TdxIntegerList;
  ADistancesToNewCells: TdxList<TdxDistanceCollection>);
begin
end;

function TdxNormalizeTableGridHelperBase.CalculateDistancesToNewCells: TdxList<TdxDistanceCollection>;
var
  I, ACellsCount: Integer;
begin
  Result := TdxObjectList<TdxDistanceCollection>.Create;
  ACellsCount := PatternCellsParameters.Count;
  for I := 0 to ACellsCount - 1 do
    Result.Add(CalculateDistancesToNewCellsCore(PatternCellsParameters[I]));
end;

function TdxNormalizeTableGridHelperBase.CalculateDistancesToNewCellsCore(
  ACellParameters: TdxTableCellParameters): TdxDistanceCollection;
var
  ACell: TdxTableCell;
  ACells: TdxTableCellCollection;
  I, AIndexInRow, APatternCellWidth, ADistanceToCell, AStartColumnIndex, AEndColumnIndex, AColumnIndex,
    AGridIntervalsCount, ACurrentWidth: Integer;
begin
  Result := TdxDistanceCollection.Create;
  ACells := Table.Rows[ACellParameters.RowIndex].Cells;
  AIndexInRow := ACellParameters.IndexInRow;
  if AIndexInRow = ACells.Count then
    Exit;

  APatternCellWidth := 0;
  ADistanceToCell := 0;
  AStartColumnIndex := ACellParameters.StartColumnIndex;
  AEndColumnIndex := ACellParameters.EndColumnIndex;

  ACell := ACells[GetIndex(AIndexInRow)];
  AColumnIndex := GetColumnIndex(AEndColumnIndex, ACell);
  AGridIntervalsCount := GridColumns.Count;
  for I := 0 to AGridIntervalsCount - 1 do
  begin
    ACurrentWidth := GridColumns[I].Width;
    Inc(ADistanceToCell, ACurrentWidth);
    if (I >= AStartColumnIndex) and (I <= AEndColumnIndex) then
      Inc(APatternCellWidth, ACurrentWidth);

    if (I >= AStartColumnIndex) and (AColumnIndex = I) then
    begin
      Result.Add(CalculateDistance(ADistanceToCell, APatternCellWidth));
      if ACell.IsLastCellInRow then
        Break;
      ACell := ACell.Next;
      Inc(AColumnIndex, ACell.ColumnSpan);
    end;
  end;
end;

function TdxNormalizeTableGridHelperBase.GetColumnIndex(AIndex: Integer; ACell: TdxTableCell): Integer;
begin
  Result := AIndex;
end;

function TdxNormalizeTableGridHelperBase.CalculateDistance(ADistanceToCell: Integer; ACellWidth: Integer): Integer;
begin
  Result := ADistanceToCell + ACellWidth;
end;

function TdxNormalizeTableGridHelperBase.MergeDistances(ADistances: TdxList<TdxDistanceCollection>): TdxIntegerList;
var
  ACurrentDistances: TdxDistanceCollection;
  ACount, I, ACurrentDistancesCount, J, ADistance: Integer;
begin
  Result := TdxIntegerList.Create;
  ACount := ADistances.Count;

  if ACount = 0 then
    Exit(Result);
  Result.AddRange(ADistances[0]);

  for I := 1 to ACount - 1 do
  begin
    ACurrentDistances := ADistances[I];
    ACurrentDistancesCount := ACurrentDistances.Count;
    for J := 0 to ACurrentDistancesCount - 1 do
    begin
      ADistance := ACurrentDistances[J];
      if not Result.Contains(ADistance) then
        Result.Add(ADistance);
    end;
  end;
  Result.Sort;
end;

function TdxNormalizeTableGridHelperBase.GetRowContainsPatternCellIndexes: TdxIntegerList;
var
  ACount, I: Integer;
begin
  Result := TdxIntegerList.Create;
  ACount := PatternCellsParameters.Count;
  for I := 0 to ACount - 1 do
    Result.Add(PatternCellsParameters[I].RowIndex);
end;

procedure TdxNormalizeTableGridHelperBase.NormalizeColumnSpansCore(ADistances: TdxIntegerList; ATableRow: TdxTableRow);
var
  ATableRowGridBefore, ATableRowGridAfter, I, AColumnIndex: Integer;
  AParameters: TdxColumnIndexParameters;
  ACell: TdxTableCell;
begin
  ATableRowGridBefore := ATableRow.GridBefore;
  ATableRowGridAfter := ATableRow.GridAfter;
  for I := ADistances.Count - 1 downto 0 do
  begin
    AParameters := CalculateColumnIndexParameters(ADistances[I]);
    if AParameters.ColumnIndex < 0 then
    begin
      ATableRow.GridAfter := ATableRow.GridAfter + 1;
      Continue;
    end;
    if AParameters.IsExactly then
      Continue;
    AColumnIndex := AParameters.ColumnIndex;
    if AColumnIndex < ATableRowGridBefore then
    begin
      ATableRow.GridBefore := ATableRow.GridBefore + 1;
      Continue;
    end;
    if AColumnIndex > GridColumns.Count - ATableRowGridAfter - 1 then
    begin
      ATableRow.GridAfter := ATableRow.GridAfter + 1;
      Continue;
    end;
    ACell := TdxTableCellVerticalBorderCalculator.GetCellByColumnIndex(ATableRow, AColumnIndex);
    if ACell = nil then
      Continue;
    ACell.ColumnSpan := ACell.ColumnSpan + 1;
  end;
end;

function TdxNormalizeTableGridHelperBase.GetWidthConsiderUnitType(AWidthUnit: TdxWidthUnit): Integer;
begin
  Result := AWidthUnit.Value;
  if AWidthUnit.&Type = TdxWidthUnitType.ModelUnits then
    Result := Table.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(Result);
end;

procedure TdxNormalizeTableGridHelperBase.RecalculateGridIntervals(AMergedDistances: TdxIntegerList);
var
  ACurrentColumn, ANewInterval: TdxTableGridColumn;
  I, J, AGridIntervalsCount, ATotalLength, ACurrentColumnWidth, AColumnWidth, APreviousNewColumnWidth, ADistancesCount,
    ADistance, ADelta, APreviousTotalWidth, ADeltaPrevious, ANewWidth: Integer;
begin
  AGridIntervalsCount := GridColumns.Count;
  ATotalLength := 0;
  I := 0;
  while I < AGridIntervalsCount do
  begin
    ACurrentColumn := GridColumns[I];
    ACurrentColumnWidth := ACurrentColumn.Width;
    Inc(ATotalLength, ACurrentColumnWidth);
    AColumnWidth := ACurrentColumnWidth;
    APreviousNewColumnWidth := 0;
    ADistancesCount := AMergedDistances.Count;
    for J := 0 to ADistancesCount - 1 do
    begin
      ADistance := AMergedDistances[J];
      ADelta := Abs(ATotalLength - ADistance);
      APreviousTotalWidth := ATotalLength - AColumnWidth;
      ADeltaPrevious := Abs(APreviousTotalWidth - ADistance);
      if (ADelta <= FDeltaBetweenColumnsUpdate) or (ADeltaPrevious <= FDeltaBetweenColumnsUpdate) or (ADistance > ATotalLength) or (ADistance < APreviousTotalWidth) then
        Continue;

      ANewWidth := ADistance - APreviousTotalWidth - APreviousNewColumnWidth;
      ACurrentColumn.Width := AColumnWidth - ANewWidth - APreviousNewColumnWidth;

      ANewInterval := TdxTableGridColumn.Create(ANewWidth, False);
      GridColumns.Insert(I, ANewInterval);

      Inc(I);
      Inc(AGridIntervalsCount);
      Inc(APreviousNewColumnWidth, ANewWidth);
    end;
    Inc(I);
  end;
end;

procedure TdxNormalizeTableGridHelperBase.NormalizeColumnSpansInRowContainingPatternCell(ADistances: TdxList<TdxDistanceCollection>);
var
  ACount, I: Integer;
  AColumnSpans: TdxIntegerList;
begin
  ACount := PatternCellsParameters.Count;
  for I := 0 to ACount - 1 do
  begin
    AColumnSpans := CalculateColumnSpansForNewCells(ADistances[I], PatternCellsParameters[I]);
    try
      NormalizeColumnSpansInRowContainingPatternCellCore(AColumnSpans, PatternCellsParameters[I]);
    finally
      AColumnSpans.Free;
    end;
  end;
end;

procedure TdxNormalizeTableGridHelperBase.NormalizeColumnSpansInRowContainingPatternCellCore(
  AColumnSpans: TdxIntegerList; ACellParameters: TdxTableCellParameters);
var
  ACells: TdxTableCellCollection;
  AStartIndex, ASpanCount, I, AColumnSpan: Integer;
begin
  AStartIndex := GetIndex(ACellParameters.IndexInRow);
  ACells := Table.Rows[ACellParameters.RowIndex].Cells;
  ASpanCount := AColumnSpans.Count;
  for I := 0 to ASpanCount - 1 do
  begin
    AColumnSpan := AColumnSpans[I];
    if AColumnSpan <= 0 then
      ACells[AStartIndex + I].ColumnSpan := 1
    else
      ACells[AStartIndex + I].ColumnSpan := AColumnSpan;
  end;
end;

function TdxNormalizeTableGridHelperBase.GetIndex(AIndex: Integer): Integer;
begin
  Result := AIndex;
end;

function TdxNormalizeTableGridHelperBase.CalculateColumnSpansForNewCells(ADistances: TdxIntegerList;
  ACellParameters: TdxTableCellParameters): TdxIntegerList;
var
  AStartColumnIndex, AIntervalsCount, I, AColumnSpan: Integer;
  AParameters: TdxColumnIndexParameters;
begin
  Result := TdxIntegerList.Create;
  AStartColumnIndex := ACellParameters.StartColumnIndex;
  AIntervalsCount := ADistances.Count;
  for I := 0 to AIntervalsCount - 1 do
  begin
    AParameters := CalculateColumnIndexParameters(ADistances[I]);
    AColumnSpan := AParameters.ColumnIndex - AStartColumnIndex + 1;
    Result.Add(AColumnSpan);
    AStartColumnIndex := AParameters.ColumnIndex;
    if AParameters.IsExactly then
      Inc(AStartColumnIndex);
  end;
end;

function TdxNormalizeTableGridHelperBase.CalculateColumnIndexParameters(
  ADistanceToCell: Integer): TdxColumnIndexParameters;
var
  AIsExactly, AIsPrevousExactly: Boolean;
  I, AGridIntervalsCount, ATotalLength, APreviousTotalLength, APrveiousDelta, AColumnIndex: Integer;
begin
  Result := TdxColumnIndexParameters.Create(-1, False);
  AGridIntervalsCount := GridColumns.Count;
  ATotalLength := 0;
  APreviousTotalLength := 0;
  for I := 0 to AGridIntervalsCount - 1 do
  begin
    Inc(ATotalLength, GridColumns[I].Width);
    APrveiousDelta := Abs(ADistanceToCell - APreviousTotalLength);
    AIsExactly := ADistanceToCell = ATotalLength;
    AIsPrevousExactly := APrveiousDelta <= FDeltaBetweenColumnsUpdate;
    if ADistanceToCell <= ATotalLength then
    begin
      if (not AIsExactly and AIsPrevousExactly) and (APreviousTotalLength > 0) then
        AColumnIndex := I - 1
      else
        AColumnIndex := I;
      Result.ColumnIndex := AColumnIndex;
      Result.IsExactly := (AIsExactly) or ((AIsPrevousExactly and (APreviousTotalLength > 0)));
      Exit;
    end;
    APreviousTotalLength := ATotalLength;
  end;
end;

procedure TdxNormalizeTableGridHelperBase.NormalizeWidthAfter(ATable: TdxTable);
var
  AWidthsCollection: TdxIntegerList;
  I, J, AMaxWidth, ARowsCount, AWidth, ACellsCount, ANewWidth: Integer;
  ARows: TdxTableRowCollection;
  ACurrentRow: TdxTableRow;
  ACells: TdxTableCellCollection;
  ACurrentCell: TdxTableCell;
  AWidthAfter: TdxWidthUnit;
  ACurrentRowProperties: TdxTableRowProperties;
  AUnitType: TdxWidthUnitType;
  AWidthUnitInfo: TdxWidthUnitInfo;
begin
  AWidthsCollection := TdxIntegerList.Create;
  try
    AMaxWidth := 0;

    ARows := ATable.Rows;
    ARowsCount := ARows.Count;
    for I := 0 to ARowsCount - 1 do
    begin
      ACurrentRow := ARows[I];
      AWidth := GetActualWidthValue(ACurrentRow.WidthBefore);

      ACells := ACurrentRow.Cells;
      ACellsCount := ACells.Count;
      for J := 0 to ACellsCount - 1 do
      begin
        ACurrentCell := ACells[J];
        Inc(AWidth, GetActualWidthValue(ACurrentCell.PreferredWidth));
      end;

      AMaxWidth := Math.Max(AMaxWidth, AWidth);
      AWidthsCollection.Add(AWidth);
    end;

    for I := 0 to ARowsCount - 1 do
    begin
      ACurrentRow := ARows[I];
      AWidthAfter := ACurrentRow.WidthAfter;
      ACurrentRowProperties := ACurrentRow.Properties;
      if ACurrentRow.GridAfter = 0 then
      begin
        if (AWidthAfter.&Type <> TdxWidthUnitType.&Nil) or (AWidthAfter.Value <> 0) then
          ResetWidthAfter(ACurrentRowProperties);
        Continue;
      end;
      AUnitType := ARows.First.Cells.First.PreferredWidth.&Type;
      if AUnitType = TdxWidthUnitType.Auto then
      begin
        ResetWidthAfter(ACurrentRowProperties);
        Continue;
      end;

      ANewWidth := AMaxWidth - AWidthsCollection[I];
      Assert((ANewWidth > 0) or (AUnitType = TdxWidthUnitType.&Nil));
      AWidthUnitInfo := TdxWidthUnitInfo.Create(AUnitType, ANewWidth);
      try
        ACurrentRowProperties.WidthAfter.CopyFrom(AWidthUnitInfo);
      finally
        AWidthUnitInfo.Free;
      end;
    end;
  finally
    AWidthsCollection.Free;
  end;
end;

function TdxNormalizeTableGridHelperBase.GetActualWidthValue(AWidthUnit: TdxWidthUnit): Integer;
var
  AUnitType: TdxWidthUnitType;
begin
  AUnitType := AWidthUnit.&Type;
  if (AUnitType = TdxWidthUnitType.ModelUnits) or (AUnitType = TdxWidthUnitType.FiftiethsOfPercent) then
    Result := AWidthUnit.Value
  else
    Result := 0;
end;

procedure TdxNormalizeTableGridHelperBase.ResetWidthAfter(ARowProperties: TdxTableRowProperties);
var
  AWidthUnitInfo: TdxWidthUnitInfo;
begin
  AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.&Nil, 0);
  try
    ARowProperties.WidthAfter.CopyFrom(AWidthUnitInfo);
  finally
    AWidthUnitInfo.Free;
  end;
  ARowProperties.ResetUse(TdxTableRowPropertiesOptions.MaskUseWidthAfter);
end;

procedure TdxNormalizeTableGridHelperBase.CheckTableWidthTypeIntegrity(ATable: TdxTable);
begin
end;

procedure TdxNormalizeTableGridHelperBase.NormalizeVerticalMerging(ACell: TdxTableCell);
var
  AVerticalSpanCells: TdxTableCellList;
  ACurrentCellVerticalMerging: TdxMergingState;
  ACurrentCell, AFirstCellInMergingGroup: TdxTableCell;
  I, ACellsCount, AStartColumnIndex, AVerticalSpanCellsCount, ACurrentCellIndexInMergingGroup: Integer;
begin
  ACellsCount := ACell.Row.Cells.Count;
  for I := ACell.IndexInRow + 1 to ACellsCount - 1 do
  begin
    ACurrentCell := ACell.Row.Cells[I];
    ACurrentCellVerticalMerging := ACurrentCell.VerticalMerging;
    if ACurrentCellVerticalMerging = TdxMergingState.None then
      Continue;

    AFirstCellInMergingGroup := ACurrentCell.Table.GetFirstCellInVerticalMergingGroup(ACurrentCell);
    AStartColumnIndex := AFirstCellInMergingGroup.GetStartColumnIndexConsiderRowGrid;
    AVerticalSpanCells := TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(AFirstCellInMergingGroup,
      AStartColumnIndex, False);
    try
      AVerticalSpanCellsCount := AVerticalSpanCells.Count;
      Assert(AVerticalSpanCellsCount > 1);
      ACurrentCell.Properties.VerticalMerging := TdxMergingState.None;
      if ACurrentCellVerticalMerging = TdxMergingState.Restart then
      begin
        if AVerticalSpanCellsCount > 2 then
          AVerticalSpanCells[1].Properties.VerticalMerging := TdxMergingState.Restart
        else
        begin
          AVerticalSpanCells[0].Properties.VerticalMerging := TdxMergingState.None;
          AVerticalSpanCells[1].Properties.VerticalMerging := TdxMergingState.None;
        end;
      end
      else
      begin
        ACurrentCellIndexInMergingGroup := AVerticalSpanCells.IndexOf(ACurrentCell);
        if ACurrentCellIndexInMergingGroup = 1 then
        begin
          AVerticalSpanCells[0].Properties.VerticalMerging := TdxMergingState.None;
        end;
        if AVerticalSpanCellsCount - 2 = ACurrentCellIndexInMergingGroup then
          AVerticalSpanCells[AVerticalSpanCellsCount - 1].Properties.VerticalMerging := TdxMergingState.None
        else
          if AVerticalSpanCellsCount - 1 <> ACurrentCellIndexInMergingGroup then
            AVerticalSpanCells[ACurrentCellIndexInMergingGroup + 1].Properties.VerticalMerging :=
              TdxMergingState.Restart;
      end;
    finally
      AVerticalSpanCells.Free;
    end;
  end;
end;

{ TdxNormalizeTableGridAfterSplitCellHelper }

constructor TdxNormalizeTableGridAfterSplitCellHelper.Create(ATable: TdxTable; ACells: TdxTableCellList;
  APartsCount: Integer; AGridColumns: TdxTableGridColumnCollection; AUseDeltaBetweenColumns: Boolean);
begin
  inherited Create(ATable, ACells, AGridColumns, AUseDeltaBetweenColumns);
  FPartsCount := APartsCount;
end;

function TdxNormalizeTableGridAfterSplitCellHelper.CalculateDistancesToNewCellsCore(
  ACellParameters: TdxTableCellParameters): TdxDistanceCollection;
var
  ACells: TdxTableCellCollection;
  I, APatternCellWidth, ADistanceToPatternCell, AStartColumnIndex, AEndColumnIndex,
    ACurrentWidth, APartWidth, ADistanceToCell: Integer;
begin
  Result := TdxDistanceCollection.Create;
  ACells := Table.Rows[ACellParameters.RowIndex].Cells;
  if ACellParameters.IndexInRow = ACells.Count then
    Exit;

  APatternCellWidth := 0;
  ADistanceToPatternCell := 0;
  AStartColumnIndex := ACellParameters.StartColumnIndex;
  AEndColumnIndex := ACellParameters.EndColumnIndex;

  for I := 0 to AEndColumnIndex do
  begin
    ACurrentWidth := GridColumns[I].Width;
    if I < AStartColumnIndex then
      Inc(ADistanceToPatternCell, ACurrentWidth);
    if I >= AStartColumnIndex then
      Inc(APatternCellWidth, ACurrentWidth);
  end;

  APartWidth := APatternCellWidth div FPartsCount;
  ADistanceToCell := ADistanceToPatternCell;
  for I := 0 to FPartsCount - 1 - 1 do
  begin
    Inc(ADistanceToCell, APartWidth);
    Result.Add(ADistanceToCell);
  end;
  Result.Add(ADistanceToPatternCell + APatternCellWidth);
end;

procedure TdxNormalizeTableGridAfterSplitCellHelper.SnapDistanceToColumnGrid(AMergedDistances: TdxIntegerList;
  ADistancesToNewCells: TdxList<TdxDistanceCollection>);
var
  I, AGridColumnsCount, ADistanceCount, AGridColumnIndex, AColumnDistance, ANextAllowedDistance,
    ADistance, AOldDistance: Integer;
  AStartIndices: TArray<Integer>;
begin
  AGridColumnsCount := GridColumns.Count;
  ADistanceCount := AMergedDistances.Count;

  SetLength(AStartIndices, ADistanceCount);
  AGridColumnIndex := 0;
  AColumnDistance := 0;
  while (AGridColumnIndex < AGridColumnsCount) and (AColumnDistance + GridColumns[AGridColumnIndex].Width < AMergedDistances[0]) do
  begin
    Inc(AColumnDistance, GridColumns[AGridColumnIndex].Width);
    Inc(AGridColumnIndex);
  end;
  ANextAllowedDistance := AColumnDistance + DeltaBetweenColumnsUpdate + 1;

  for I := 0 to ADistanceCount - 1 do
  begin
    while (AGridColumnIndex < AGridColumnsCount) and (AColumnDistance < ANextAllowedDistance) do
    begin
      Inc(AColumnDistance, GridColumns[AGridColumnIndex].Width);
      Inc(AGridColumnIndex);
    end;

    ADistance := AMergedDistances[I];
    AOldDistance := ADistance;
    if ADistance < ANextAllowedDistance then
      ADistance := ANextAllowedDistance;
    if (AGridColumnIndex < AGridColumnsCount) and (Abs(ADistance - AColumnDistance) <= DeltaBetweenColumnsUpdate) then
      ADistance := AColumnDistance;
    if AOldDistance <> ADistance then
    begin
      ChangeDistance(ADistancesToNewCells, AOldDistance, ADistance, AStartIndices);
      AMergedDistances[I] := ADistance;
    end;
    ANextAllowedDistance := ADistance + DeltaBetweenColumnsUpdate + 1;
  end;
end;

procedure TdxNormalizeTableGridAfterSplitCellHelper.ChangeDistance(ADistancesToNewCells: TdxList<TdxDistanceCollection>;
  AOldDistance: Integer; ANewDistance: Integer; const AStartIndices: TArray<Integer>);
var
  I, J, ACount, AIndex: Integer;
  ADistances: TdxIntegerList;
begin
  ACount := ADistancesToNewCells.Count;
  for I := 0 to ACount - 1 do
  begin
    ADistances := ADistancesToNewCells[I];
    AIndex := -1;
    for J := AStartIndices[I] to ADistances.Count - 1 do
      if ADistances[J] = AOldDistance then
      begin
        AIndex := J;
        Break;
      end;
    if AIndex >= 0 then
    begin
      ADistances[AIndex] := ANewDistance;
      AStartIndices[I] := AIndex + 1;
      Exit;
    end;
  end;
end;

{ TdxNormalizeTableGridAfterInsertCellHelper }

function TdxNormalizeTableGridAfterInsertCellHelper.GetIndex(AIndex: Integer): Integer;
begin
  Result := AIndex + 1;
end;

function TdxNormalizeTableGridAfterInsertCellHelper.CalculateColumnSpansForNewCells(ADistances: TdxIntegerList; ACellParameters: TdxTableCellParameters): TdxIntegerList;
var
  AIntervalsCount, AStartColumnIndex, I, AColumnSpan: Integer;
  AParameters: TdxColumnIndexParameters;
begin
  Result := TdxIntegerList.Create;
  AIntervalsCount := ADistances.Count;
  AStartColumnIndex := ACellParameters.EndColumnIndex + 1;
  for I := 0 to AIntervalsCount - 1 do
  begin
    AParameters := CalculateColumnIndexParameters(ADistances[I]);
    if (AParameters.ColumnIndex < 0) and (AStartColumnIndex > 0) then
      AColumnSpan := (GridColumns.Count + 1) - AStartColumnIndex
    else
      AColumnSpan := AParameters.ColumnIndex - (AStartColumnIndex) + 1;
    Result.Add(AColumnSpan);
    AStartColumnIndex := AParameters.ColumnIndex;
    if AParameters.IsExactly then
      Inc(AStartColumnIndex);
  end;
end;

{ TdxNormalizeTableGridAfterDeleteCellHelper }

function TdxNormalizeTableGridAfterDeleteCellHelper.GetColumnIndex(AIndex: Integer; ACell: TdxTableCell): Integer;
begin
  Result := AIndex + ACell.ColumnSpan;
end;

function TdxNormalizeTableGridAfterDeleteCellHelper.CalculateDistance(ADistanceToCell: Integer; ACellWidth: Integer): Integer;
begin
  Result := ADistanceToCell - ACellWidth;
end;

{ TdxParagraphListHistoryItemBase }

function TdxParagraphListHistoryItemBase.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxParagraphListHistoryItemBase.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

{ TdxAddParagraphToListHistoryItem }

constructor TdxAddParagraphToListHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FOldOwnNumberingListIndex := NumberingListIndexListIndexNotSetted;
end;

procedure TdxAddParagraphToListHistoryItem.RedoCore;
var
  AParagraph: TdxParagraph;
begin
  AParagraph := PieceTable.Paragraphs[ParagraphIndex];
  OldOwnNumberingListIndex := AParagraph.GetOwnNumberingListIndex;
  AParagraph.SetNumberingListIndex(NumberingListIndex);
  AParagraph.SetListLevelIndex(ListLevelIndex);
  TdxNumberingListNotifier.NotifyParagraphAdded(DocumentModel, NumberingListIndex);
  TdxNumberingListNotifier.NotifyParagraphRemoved(DocumentModel, OldOwnNumberingListIndex);
end;

procedure TdxAddParagraphToListHistoryItem.UndoCore;
var
  AParagraph: TdxParagraph;
begin
  AParagraph := PieceTable.Paragraphs[ParagraphIndex];
  AParagraph.ResetNumberingListIndex(OldOwnNumberingListIndex);
  AParagraph.ResetListLevelIndex;
  TdxNumberingListNotifier.NotifyParagraphRemoved(DocumentModel, NumberingListIndex);
  TdxNumberingListNotifier.NotifyParagraphAdded(DocumentModel, OldOwnNumberingListIndex);
end;

{ TdxNumberingListNotifier }

class procedure TdxNumberingListNotifier.NotifyParagraphAdded(ADocumentModel: TdxDocumentModel;
  AIndex: TdxNumberingListIndex);
begin
  if AIndex >= NumberingListIndexMinValue then
    ADocumentModel.NumberingLists[AIndex].OnParagraphAdded;
end;

class procedure TdxNumberingListNotifier.NotifyParagraphRemoved(ADocumentModel: TdxDocumentModel;
  AIndex: TdxNumberingListIndex);
begin
  if AIndex >= NumberingListIndexMinValue then
    ADocumentModel.NumberingLists[AIndex].OnParagraphRemoved;
end;

{ TdxRemoveParagraphFromListHistoryItem }

procedure TdxRemoveParagraphFromListHistoryItem.RedoCore;
var
  AParagraph: TdxParagraph;
  ANewValue: TdxNumberingListIndex;
begin
  AParagraph := PieceTable.Paragraphs[ParagraphIndex];
  NumberingListIndex := AParagraph.GetOwnNumberingListIndex;
  ListLevelIndex := AParagraph.GetOwnListLevelIndex;
  ANewValue := NumberingListIndexListIndexNotSetted;
  if not AParagraph.IsInNonStyleList then
    ANewValue := NumberingListIndexNoNumberingList;
  AParagraph.ResetNumberingListIndex(ANewValue);
  AParagraph.ResetListLevelIndex;
  TdxNumberingListNotifier.NotifyParagraphRemoved(DocumentModel, NumberingListIndex);
end;

procedure TdxRemoveParagraphFromListHistoryItem.UndoCore;
var
  AParagraph: TdxParagraph;
begin
  AParagraph := PieceTable.Paragraphs[ParagraphIndex];
  AParagraph.SetNumberingListIndex(NumberingListIndex);
  AParagraph.SetListLevelIndex(ListLevelIndex);
  TdxNumberingListNotifier.NotifyParagraphAdded(DocumentModel, NumberingListIndex);
end;

{ TdxPieceTablePasteContentCommand }

constructor TdxPieceTablePasteContentCommand.Create(APieceTable: TdxPieceTable);
begin
  inherited Create(APieceTable);
  FPasteSource := TdxEmptyPasteSource.Create;
end;

{ TdxPieceTablePasteContentConvertedToDocumentModelCommandBase }

constructor TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.Create(
  APieceTable: TdxPieceTable);
begin
  inherited Create(APieceTable);
  FInsertOptions := DocumentModel.CopyPasteOptions.InsertOptions;
end;

procedure TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.ApplyChanges;
begin
end;

procedure TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.CalculateApplyChangesParameters;
begin
end;

procedure TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.CalculateExecutionParameters;
begin
end;

constructor TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.Create(
  APieceTable: TdxPieceTable; AInsertOptions: TdxInsertOptions);
begin
  inherited Create(APieceTable);
  FInsertOptions := AInsertOptions;
end;

function TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.CreatePasteDocumentModelCommand(
  APos: TdxDocumentLogPosition; ASource: TdxDocumentModel;
  ASuppressFieldsUpdate,
  APasteFromIE: Boolean): TdxPieceTableInsertContentConvertedToDocumentModelCommand;
begin
  Result := TdxPieceTableInsertContentConvertedToDocumentModelCommand.Create(PieceTable, ASource, APos,
    InsertOptions, False);
  Result.SuppressFieldsUpdate := ASuppressFieldsUpdate;
  Result.CopyBetweenInternalModels := CopyBetweenInternalModels;
  Result.PasteFromIE := APasteFromIE;
  Result.SuppressCopySectionProperties := SuppressCopySectionProperties(ASource);
end;

procedure TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.ExecuteCore;
var
  ASizeCollection: string;
  ASource: TdxDocumentModel;
begin
  ASizeCollection := GetAdditionalContentString;
  ASource := CreateSourceDocumentModel(ASizeCollection);
  try
    PasteContent(ASource, DocumentModel.Selection.&End, ASizeCollection);
  finally
    ASource.Free;
  end;
end;

function TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.GetAdditionalContentString: string;
begin
  Result := '';
end;

function TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.GetDocumentModelSingleFloatingObjectAnchorRun(AModel: TdxDocumentModel): TdxFloatingObjectAnchorRun;
var
  APieceTable: TdxPieceTable;
begin
  APieceTable := AModel.MainPieceTable;
  if (APieceTable.Runs.Count = 2) and (APieceTable.Runs[0] is TdxFloatingObjectAnchorRun) then
    Result := TdxFloatingObjectAnchorRun(APieceTable.Runs[0])
  else
    Result := nil;
end;

procedure TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.OffsetNewlyInsertedFloatingObjectIfNeed(
  ANewRun: TdxFloatingObjectAnchorRun; AParagraph: TdxParagraph);
var
  ALastRunIndex, I: TdxRunIndex;
  ARuns: TdxTextRunCollection;
  ARun: TdxFloatingObjectAnchorRun;
begin
  ALastRunIndex := AParagraph.LastRunIndex;
  ARuns := AParagraph.PieceTable.Runs;
  I := AParagraph.FirstRunIndex;
  while (I < ALastRunIndex) and (I >= 0) do
  begin
    if ARuns[I] is TdxFloatingObjectAnchorRun then
    begin
      ARun := TdxFloatingObjectAnchorRun(ARuns[I]);
      if ShouldOffsetNewRun(ANewRun, ARun) then
      begin
        OffsetNewlyInsertedFloatingObject(ANewRun, ARun);
        I := AParagraph.FirstRunIndex - 1;
      end
      else
        Inc(I);
    end
    else
      Break;
  end;
end;

procedure TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.OffsetNewlyInsertedFloatingObject(ANewRun: TdxFloatingObjectAnchorRun; ARun: TdxFloatingObjectAnchorRun);
var
  AProperties: TdxFloatingObjectProperties;
  AOffset: TPoint;
  AShift: Integer;
begin
  AProperties := ANewRun.FloatingObjectProperties;
  AOffset := AProperties.Offset;
  AShift := DocumentModel.UnitConverter.DocumentsToModelUnits(50);
  Inc(AOffset.X, AShift);
  Inc(AOffset.Y, AShift);
  AProperties.Offset := AOffset;
  AProperties.ZOrder := Math.Max(AProperties.ZOrder, ARun.FloatingObjectProperties.ZOrder + 1);
end;

procedure TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.PasteContent(
  ASource: TdxDocumentModel; APos: TdxDocumentLogPosition;
  const ASizeCollection: string);
var
  AAnchorRunParagraph: TdxParagraph;
  AAnchorRun: TdxFloatingObjectAnchorRun;
begin
  if ASource = nil then
    Exit;

  if FForceInsertFloatingObjectAtParagraphStart then
  begin
    AAnchorRun := GetDocumentModelSingleFloatingObjectAnchorRun(ASource);
    if AAnchorRun <> nil then
    begin
      AAnchorRunParagraph := PreprocessSingleFloatingObjectInsertion(AAnchorRun);
      APos := AAnchorRunParagraph.LogPosition;
    end;
  end;
  ASource.PreprocessContentBeforeInsert(PieceTable, APos);
  PasteDocumentModel(APos, ASource, PasteFromIE);
end;

procedure TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.PasteDocumentModel(APos: TdxDocumentLogPosition;
  ASource: TdxDocumentModel; APasteFromIE: Boolean; ASuppressFieldsUpdate: Boolean = False);
var
  ACommand: TdxPieceTableInsertContentConvertedToDocumentModelCommand;
begin
  ACommand := CreatePasteDocumentModelCommand(APos, ASource, ASuppressFieldsUpdate, APasteFromIE);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.PreprocessSingleFloatingObjectInsertion(
  ARun: TdxFloatingObjectAnchorRun): TdxParagraph;
var
  AModelPosition: TdxDocumentModelPosition;
  APieceTable: TdxPieceTable;
begin
  AModelPosition := DocumentModel.Selection.Interval.&End;
  APieceTable := TdxPieceTable(AModelPosition.PieceTable);
  Result := APieceTable.Paragraphs[AModelPosition.ParagraphIndex];
  OffsetNewlyInsertedFloatingObjectIfNeed(ARun, Result);
end;

procedure TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.SetSuppressStoreImageSize(
  ADocumentModel: TdxDocumentModel; const ASizeCollection: string);
var
  ARuns: TdxTextRunCollection;
  ASizeIndex: Integer;
  I: Integer;
  APictureRun: TdxInlinePictureRun;
  ASizes: TStrings;
begin
  if ASizeCollection <> '' then
  begin
    ARuns := PieceTable.Runs;
    ASizeIndex := 0;
    ASizes := TStringList.Create;
    try
      ExtractStrings([',', ';'], [' '], PChar(ASizeCollection), ASizes);
      for I := 0 to ARuns.Count - 1 do
      begin
        if ARuns[I] is TdxInlinePictureRun then
        begin
          APictureRun := TdxInlinePictureRun(ARuns[I]);
          if APictureRun.Image.SuppressStore then
          begin
            APictureRun.ScaleX := StrToIntDef(ASizes[ASizeIndex], 1);
            APictureRun.ScaleY := StrToIntDef(ASizes[ASizeIndex + 1], 1);
            Inc(ASizeIndex);
          end;
        end;
      end;
    finally
      ASizes.Free;
    end;
  end;
end;

function TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.ShouldOffsetNewRun(
  ANewRun, ARun: TdxFloatingObjectAnchorRun): Boolean;
var
  ANewFloatingObjectProperties: TdxFloatingObjectProperties;
  AFloatingObjectProperties: TdxFloatingObjectProperties;
begin
  ANewFloatingObjectProperties := ANewRun.FloatingObjectProperties;
  AFloatingObjectProperties := ARun.FloatingObjectProperties;
  Result := PointsEqual(ANewFloatingObjectProperties.Offset, AFloatingObjectProperties.Offset) and
    (ANewFloatingObjectProperties.HorizontalPositionType = AFloatingObjectProperties.HorizontalPositionType) and
    (ANewFloatingObjectProperties.VerticalPositionType = AFloatingObjectProperties.VerticalPositionType);
end;

function TdxPieceTablePasteContentConvertedToDocumentModelCommandBase.SuppressCopySectionProperties(
  ASource: TdxDocumentModel): Boolean;
var
  AParagraphs: TdxParagraphCollection;
  ALastParagraph: TdxParagraph;
  AShouldPreserveTableProperties: Boolean;
begin
  AParagraphs := ASource.MainPieceTable.Paragraphs;
  ALastParagraph := AParagraphs.Last;
  AShouldPreserveTableProperties := (AParagraphs.Count > 1) and (AParagraphs[AParagraphs.Count - 2].GetCell <> nil);
  Result := not ALastParagraph.IsEmpty or
    AShouldPreserveTableProperties or DocumentModel.CopyPasteOptions.MaintainDocumentSectionSettings;
end;

{ TdxPieceTablePasteRtfTextCommand }

function TdxPieceTablePasteRtfTextCommand.GetAdditionalContentString: string;
begin
  Result := PasteSource.GetDataAsText(TdxOfficeDataFormats.SuppressStoreImageSize);
end;

function TdxPieceTablePasteRtfTextCommand.GetContent: TdxClipboardStringContent;
var
  AContent: string;
begin
  AContent := PasteSource.GetDataAsText(TdxOfficeDataFormats.Rtf);
  Result := TdxClipboardStringContent.Create(AContent);
end;

function TdxPieceTablePasteRtfTextCommand.GetFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Rtf;
end;

function TdxPieceTablePasteRtfTextCommand.GetIsPasteFromIe: Boolean;
begin
  Result := PasteSource.ContainsData(TdxOfficeDataFormats.MsSourceUrl);
end;

function TdxPieceTablePasteRtfTextCommand.IsDataAvailable: Boolean;
begin
  Result := PasteSource.ContainsData(TdxOfficeDataFormats.Rtf);
end;

procedure TdxPieceTablePasteRtfTextCommand.PopulateDocumentModelFromContentStringCore(
  ADocumentModel: TdxDocumentModel; AContent: TdxClipboardStringContent;
  const ASizeCollection: string);
var
  AOptions: TdxRtfDocumentImporterOptions;
  AImporter: TdxDocumentModelImporter;
  AStream: TStream;
begin
  ADocumentModel.DeleteDefaultNumberingList(ADocumentModel.NumberingLists);
  AOptions := TdxRtfDocumentImporterOptions.Create;
  try
    AOptions.SuppressLastParagraphDelete := True;
    AOptions.CopySingleCellAsText := DocumentModel.BehaviorOptions.PasteSingleCellAsText;
    AOptions.LineBreakSubstitute := DocumentModel.BehaviorOptions.PasteLineBreakSubstitution;
    AOptions.PasteFromIE := GetIsPasteFromIe;
    PasteFromIE := AOptions.PasteFromIE;
    AImporter := TdxImportFileFormats.GetImporter(TdxRichEditDocumentFormat.Rtf, ADocumentModel, AOptions);
    try
      AStream := PrepareInputStream(AContent.StringContent);
      try
        AImporter.UpdateFields := ADocumentModel.FieldOptions.UpdateFieldsOnPaste;
        AImporter.Import(AStream);
      finally
        AStream.Free;
      end;
    finally
      AImporter.Free;
    end;
    SetSuppressStoreImageSize(ADocumentModel, ASizeCollection);
  finally
    AOptions.Free;
  end;
end;

function TdxPieceTablePasteRtfTextCommand.PrepareInputStream(
  const AStr: string): TStream;
begin
  Result := TStringStream.Create(AStr);
end;

{ TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBase }

function TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBase.CreateDocumentModelFromContentString(
  AContent: TdxClipboardStringContent;
  const ASizeCollection: string): TdxDocumentModel;
begin
  if (AContent = nil) or (AContent.StringContent = '') then
    Result := nil
  else
  begin
    Result := PieceTable.DocumentModel.CreateNew;
    Result.FieldOptions.CopyFrom(PieceTable.DocumentModel.FieldOptions);
    Result.IntermediateModel := True;
    if not PieceTable.IsMain then
    begin
      Result.DocumentCapabilities.HeadersFooters := TdxDocumentCapability.Disabled;
      Result.DocumentCapabilities.Sections := TdxDocumentCapability.Disabled;
    end;
    PopulateDocumentModelFromContentStringCore(Result, AContent, ASizeCollection);
  end;
end;

function TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBase.CreateSourceDocumentModel(
  const ASizeCollection: string): TdxDocumentModel;
var
  AContent: TdxClipboardStringContent;
begin
  AContent := GetContent;
  try
    Result := CreateDocumentModelFromContentString(AContent, ASizeCollection);
  finally
    AContent.Free;
  end;
end;

procedure TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBase.PasteContent(
  AContent: TdxClipboardStringContent; APos: TdxDocumentLogPosition;
  const ASizeCollection: string);
var
  ASource: TdxDocumentModel;
begin
  ASource := CreateDocumentModelFromContentString(AContent, ASizeCollection);
  try
    PasteContent(ASource, APos, ASizeCollection);
  finally
    ASource.Free;
  end;
end;

end.
