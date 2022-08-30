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

unit dxRichEdit.Commands;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Graphics, Classes, Controls, Generics.Defaults, Generics.Collections, ImgList,
  dxCoreClasses, dxCoreGraphics,
  dxRichEdit.Types,
  dxRichEdit.Options,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.FastComparer,
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.View.PageViewInfoGenerator,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.DocumentModel.Selection;

type
  { TdxRichEditMenuItemSimpleCommand }

  TdxRichEditMenuItemSimpleCommand = class(TdxRichEditCommand)
  protected
    procedure ExecuteCore; virtual; abstract;
    function GetForceVisible: Boolean;
    function GetSelectedCellsCollection: TdxSelectedCellsCollection;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
  end;

  { TdxHistoryCommandBase }

  TdxHistoryCommandBase = class abstract(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    function CanPerformHistoryOperation(AHistory: TdxDocumentHistory): Boolean; virtual; abstract;
    procedure InvalidateDocumentLayout;
    procedure PerformHistoryOperation(AHistory: TdxDocumentHistory); virtual; abstract;
  public
    procedure ExecuteCore; override;
  end;

  { TdxUndoCommand }

  TdxUndoCommand = class(TdxHistoryCommandBase)
  protected
    function CanPerformHistoryOperation(AHistory: TdxDocumentHistory): Boolean; override;
    procedure PerformHistoryOperation(AHistory: TdxDocumentHistory); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxClearUndoCommand }

  TdxClearUndoCommand = class(TdxHistoryCommandBase)
  protected
    procedure PerformHistoryOperation(AHistory: TdxDocumentHistory); override;
    function CanPerformHistoryOperation(AHistory: TdxDocumentHistory): Boolean; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxRedoCommand }

  TdxRedoCommand = class(TdxHistoryCommandBase)
  protected
    function CanPerformHistoryOperation(AHistory: TdxDocumentHistory): Boolean; override;
    procedure PerformHistoryOperation(AHistory: TdxDocumentHistory); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxRichEditCaretBasedCommand }

  TdxRichEditCaretBasedCommand = class(TdxRichEditMenuItemSimpleCommand)
  strict private
    function GetCaretPosition: TdxCaretPosition;
  protected
    procedure ApplyLayoutPreferredPageIndex(APreferredPageIndex: Integer); virtual;
    function UpdateCaretPosition(ADetailLevel: TdxDocumentLayoutDetailsLevel): Boolean; virtual;

    property CaretPosition: TdxCaretPosition read GetCaretPosition;
  end;

  { TdxEnsureCaretVisibleHorizontallyCommand }

  TdxEnsureCaretVisibleHorizontallyCommand = class(TdxRichEditCaretBasedCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    function CalculateOptimalCaretHorizontalPosition(const ALogicalVisibleBounds, ACaretBounds: TRect): Integer;
    function IsCaretVisible(const ALogicalVisibleBounds, ACaretBounds: TRect): Boolean;
    procedure ScrollToMakeCaretVisible(const ALogicalVisibleBounds, ACaretBounds: TRect);
  public
    procedure ExecuteCore; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxEnsureCaretVisibleVerticallyCommand }

  TdxSelectionMovementVerticalDirection = (Up, Down);

  TdxCaretScrollDirection = (None, Unknown, Up, Down);

  TdxCurrentRowInfo = record
    Row: TdxRow;
    BoundsRelativeToPage: TRect;
    constructor Create(ARow: TdxRow; const ABoundsRelativeToPage: TRect);
    class function Empty: TdxCurrentRowInfo; static;
  end;

  TdxEnsureCaretVisibleVerticallyCommand = class(TdxRichEditCaretBasedCommand)
  strict private
    FCurrentRow: TdxCurrentRowInfo;
    FCurrentPage: TdxPage;
    FCurrentPageViewInfo: TdxPageViewInfo;
    FRelativeCaretPosition: Single;
    procedure SetRelativeCaretPosition(Value: Single);
    procedure SetCurrentPage(const Value: TdxPage);
    procedure SetCurrentPageViewInfo(const Value: TdxPageViewInfo);
    procedure SetCurrentRow(const Value: TdxCurrentRowInfo);
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    procedure AdjustAndSaveCaretPosition;
    function BothPositionsWillValid(ACurrentCaretPosition, AAdjustedCaretPosition: TdxCaretPosition): Boolean;
    function CalculateActualInvisiblePageBounds(APageViewInfo: TdxPageViewInfo; ADirection: TdxSelectionMovementVerticalDirection): TRect;
    function CalculateDirectionToInvisibleCaret: TdxSelectionMovementVerticalDirection;
    function CalculateOffsetToVisibleArea(AInitialCaretPhysicalTop: Integer): Integer;
    function CalculatePhysicalOffsetToCaret: Integer;
    function CalculatePhysicalOffsetToCaretAtVisiblePage: Integer;
    function CalculatePhysicalOffsetToCaretAtInvisiblePage: Integer;
    function CalculatePhysicalOffsetForCaretPositionRowVisibilityCore(APageViewInfo: TdxPageViewInfo; const ARow: TdxCurrentRowInfo): Integer;
    function CalculatePhysicalOffsetForRowBoundsVisibility(const AViewBounds, APhysicalRowBounds: TRect): Integer; virtual;
    function CalculateScrollDirectionToCaret(APos: TdxCaretPosition): TdxCaretScrollDirection;
    function CalculateTargetRowLogicalBounds(APageViewInfo: TdxPageViewInfo; const ARow: TdxCurrentRowInfo): TRect; virtual;
    function CreateAdjustedCaretPosition: TdxCaretPosition;
    function IsCaretVisible: Boolean;
    function LookupInvisiblePageWithCaret(ADirection: TdxSelectionMovementVerticalDirection): TdxPageViewInfo;
    procedure SaveCaretPosition(APos: TdxCaretPosition);
    procedure ScrollToAlreadyFormattedContentCore(AVerticalOffset: Integer);
    procedure ScrollToMakeCaretVisible;
    procedure ScrollToSetRelativeCaretPosition;
    procedure ScrollVerticallyByPhysicalOffset(AOffset: Integer);
    function ShouldAdjustCaretPosition: Boolean;
    function ValidateRowBounds(APageViewInfo: TdxPageViewInfo; const ARowBounds: TRect): TRect;

    property CurrentRow: TdxCurrentRowInfo read FCurrentRow write SetCurrentRow;
    property CurrentPage: TdxPage read FCurrentPage write SetCurrentPage;
    property CurrentPageViewInfo: TdxPageViewInfo read FCurrentPageViewInfo write SetCurrentPageViewInfo;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); override;
    destructor Destroy; override;
    procedure ExecuteCore; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property RelativeCaretPosition: Single read FRelativeCaretPosition write SetRelativeCaretPosition;
  end;

  TdxEnsureCaretVisibleVerticallyForMovePrevNextPageCommand = class(TdxEnsureCaretVisibleVerticallyCommand)
  private
    FPhysicalOffsetAboveTargetRow: Integer;
  protected
    function CalculatePhysicalOffsetForRowBoundsVisibility(const AViewBounds: TRect;
      const APhysicalRowBounds: TRect): Integer; override;
    function CalculateTargetRowLogicalBounds(APageViewInfo: TdxPageViewInfo;
      const ARow: TdxCurrentRowInfo): TRect; override;
    function CalculateTopmostRow(APage: TdxPage): TdxRow;
  public
    property PhysicalOffsetAboveTargetRow: Integer read FPhysicalOffsetAboveTargetRow write FPhysicalOffsetAboveTargetRow;
  end;

  { TdxPlaceCaretToPhysicalPointCommand }

  TdxPlaceCaretToPhysicalPointCommand = class(TdxRichEditCaretBasedCommand)
  private
    FPhysicalPoint: TPoint;
    FSuppressClearOutdatedSelectionItems: Boolean;
    FUpdateCaretX: Boolean;
    function CalculatePreferredPageIndex(AHitTestResult: TdxRichEditHitTestResult): Integer;
  protected
    procedure ChangeSelection(ASelection: TdxSelection;
      ALogPosition: TdxDocumentLogPosition; AHitTestResult: TdxRichEditHitTestResult); virtual;
    function ChangeSelectionEnd(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition;
      AHitTestResult: TdxRichEditHitTestResult): Boolean; virtual;
    procedure ChangeSelectionStart(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition;
      AHitTestResult: TdxRichEditHitTestResult; ASelectionItemCountBeforeChangeEnd: Integer); virtual;
    function SelectToTheEndOfRow(ASelection: TdxSelection; ARow: TdxRow; AExtendSelection: Boolean): Boolean;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

		function ExtendSelection: Boolean; virtual;
    function HitTestOnlyInPageClientBounds: Boolean; virtual;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); override;
    procedure ExecuteCore; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ValidateSelection; virtual;

    property PhysicalPoint: TPoint read FPhysicalPoint write FPhysicalPoint;
    property SuppressClearOutdatedSelectionItems: Boolean read FSuppressClearOutdatedSelectionItems write
      FSuppressClearOutdatedSelectionItems;
    property UpdateCaretX: Boolean read FUpdateCaretX write FUpdateCaretX;
  end;

  { TdxPlaceCaretToPhysicalPointCommand2 }

  TdxPlaceCaretToPhysicalPointCommand2 = class(TdxPlaceCaretToPhysicalPointCommand)
  protected
    function ChangeSelectionEnd(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition;
      AHitTestResult: TdxRichEditHitTestResult): Boolean; override;
  end;

  { TdxScrollVerticallyByPhysicalOffsetCommandBase }

  TdxScrollVerticallyByPhysicalOffsetCommandBase = class(TdxRichEditMenuItemSimpleCommand)
  private
    FUpdateScrollBarBeforeExecution: Boolean;
    FExecutedSuccessfully: Boolean;
  protected
    function GetAbsolutePhysicalVerticalOffset: Integer; virtual; abstract;
    procedure GeneratePagesToEnsureScrollingSuccessfull(ADelta: Int64); virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function CreateScrollDeltaCalculator(AScrollUp: Boolean): TdxScrollByPhysicalHeightCalculator; virtual;

    property View: TdxRichEditView read GetActiveView;
    property ExecutedSuccessfully: Boolean read FExecutedSuccessfully;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    procedure ExecuteCore; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property AbsolutePhysicalVerticalOffset: Integer read GetAbsolutePhysicalVerticalOffset;
    property UpdateScrollBarBeforeExecution: Boolean read FUpdateScrollBarBeforeExecution write FUpdateScrollBarBeforeExecution;
  end;

  { TdxScrollVerticallyByLogicalOffsetCommand }

  TdxScrollVerticallyByLogicalOffsetCommand = class(TdxScrollVerticallyByPhysicalOffsetCommandBase)
  private
    FLogicalOffset: Integer;
  protected
    function GetAbsolutePhysicalVerticalOffset: Integer; override;
  public
    constructor Create(const AControl: IdxRichEditControl); override;

    property LogicalOffset: Integer read FLogicalOffset write FLogicalOffset;
  end;

  { TdxScrollVerticallyByPhysicalOffsetCommand }

  TdxScrollVerticallyByPhysicalOffsetCommand = class(TdxScrollVerticallyByPhysicalOffsetCommandBase)
  private
    FPhysicalOffset: Integer;
  protected
    function GetAbsolutePhysicalVerticalOffset: Integer; override;
  public
    constructor Create(const AControl: IdxRichEditControl); override;

    property PhysicalOffset: Integer read FPhysicalOffset write FPhysicalOffset;
  end;

  { TdxScrollVerticallyByPhysicalOffsetEnsurePageGenerationCommand }

  TdxScrollVerticallyByPhysicalOffsetEnsurePageGenerationCommand = class(TdxScrollVerticallyByPhysicalOffsetCommand)
  protected
    procedure GeneratePagesToEnsureScrollingSuccessfull(ADelta: Int64); override;
    function CalculateNewTopInvisibleHeight(ADelta: Int64): Int64;
  end;

  { TdxScrollHorizontallyByPhysicalOffsetCommand }

  TdxScrollHorizontallyByPhysicalOffsetCommand = class(TdxRichEditMenuItemSimpleCommand)
  strict private
    FPhysicalOffset: Integer;
    FExecutedSuccessfully: Boolean;
    function GetActiveView: TdxRichEditView;
  protected
    procedure ExecuteCore; override;
    procedure PerformScroll;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property ExecutedSuccessfully: Boolean read FExecutedSuccessfully;
    property View: TdxRichEditView read GetActiveView;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    property PhysicalOffset: Integer read FPhysicalOffset write FPhysicalOffset;
  end;

  { TdxSelectionValidator }

  TdxSelectionValidator = class abstract
  strict private
    FPieceTable: TdxPieceTable;
  private
    function GetDocumentModel: TdxDocumentModel;
  protected
    function CalculateOffset(AParagraphIndex: TdxParagraphIndex; AIncrementIndex: Boolean): Integer;
    procedure ChangeFieldSelection(ASelection: TdxSelection; ASelectedInterval: TdxRunInfo; AField: TdxField);
    procedure ChangeFieldSelectionCore(ASelection: TdxSelection; AField: TdxField); virtual; abstract;
    function IsSelectedItemCovered(AItem: TdxSelectionItem): Boolean;
    function ShouldChangeInterval(AInterval: TdxRunInfo; AStart, AEnd: TdxRunIndex): Boolean; virtual;
    function ShouldExtendInterval(AInterval: TdxRunInfo; AField: TdxField): Boolean; virtual;
    procedure ValidateTableActiveSelection(ASelection: TdxSelection); virtual;
    procedure ValidateFieldSelectionCore(ASelection: TdxSelection); virtual;
    procedure ValidateTableSelectionCore(ASelection: TdxSelection); virtual;
  public
    constructor Create(APieceTable: TdxPieceTable); virtual;

    procedure ValidateSelection(ASelection: TdxSelection); virtual;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read FPieceTable;
  end;

  { TdxFieldIsSelectValidator }

  TdxFieldIsSelectValidator = class(TdxSelectionValidator)
  protected
    procedure ChangeFieldSelectionCore(ASelection: TdxSelection;
      AField: TdxField); override;
  end;

  { TdxFieldIsUnselectValidator }

  TdxFieldIsUnselectValidator = class(TdxSelectionValidator)
  protected
    procedure ChangeFieldSelectionCore(ASelection: TdxSelection;
      AField: TdxField); override;
  end;

  { TdxNextCaretPositionVerticalDirectionCalculator }

  TdxNextCaretPositionVerticalDirectionCalculator = class abstract
  strict private
    FControl: IdxRichEditControl;
    FFixedDirection: Boolean;
  private
    function FindTableCellViewInfoInTableRowByX(ATableRow: TdxTableRowViewInfoBase; X: Integer): TdxTableCellViewInfo;
    function FindMostNestedTableCell(ACell: TdxTableCellViewInfo;
      AColumn: TdxColumn; ACaretX: Integer): TdxTableCellViewInfo;
    function GetTargetCellViewInfoFromNextTableRow(ACell: TdxTableCellViewInfo; X: Integer): TdxTableCellViewInfo;
    function GetNextCellViewInfoInTargetRowWithNonZeroRows(AColumn: TdxColumn; ATargetRow: TdxTableRowViewInfoBase;
      ACellWithZeroRows: TdxTableCellViewInfo): TdxTableCellViewInfo;
    function IsCellFromPreviousRowIsInNestedTableInSourceCellViewInfo(ASourceCellViewInfo, ACellFromPreviousLayoutRow: TdxTableCellViewInfo): Boolean;
    function ShouldJumpToCellInNestedTable(ARows: TdxRowCollection;
      ACurrentCellNestedLevel, ASourceRowIndexInColumn, ACaretX: Integer): Boolean;

    function GetActivePieceTable: TdxPieceTable;
    function GetView: TdxRichEditView;
  protected
    function CalculateFirstInvisiblePageIndex: Integer; virtual; abstract;
    function CalculateFirstInvalidPageIndex: Integer; virtual; abstract;
    function CreateInvisiblePageRowsGenerator: TdxInvisiblePageRowsGenerator;
    procedure CorrectRowAndCellViewInfo(APos: TdxCaretPosition; var ARowIndexInColumn: Integer;
      var ASourceCellViewInfo: TdxTableCellViewInfo);
    procedure CorrectGeneratedRowPagesHorizontalLocation(ALayoutManager: TdxPageGeneratorLayoutManager;
      ARow: TdxPageViewInfoRow); virtual; abstract;
    procedure CorrectCaretXOnExtendSelection(ASourceCellViewInfo: TdxTableCellViewInfo; var ACaretX: Integer); virtual;
    function GenerateTargetPageViewInfoRow: TdxPageViewInfoRow;
    function GetDefaultPosition: TdxDocumentModelPosition; virtual;
    function GetPageViewInfoRowIndex(APageRows: TdxPageViewInfoRowCollection; APageViewInfo: TdxPageViewInfo): Integer;
    function GetTargetLayoutRowObtainTargetPageViewInfo(APos: TdxCaretPosition; var ACaretX: Integer): TdxRow;
    function GetTargetLayoutRowObtainTargetPageViewInfoCore(APageViewInfo: TdxPageViewInfo;
      var ACaretX: Integer; AColumn: TdxColumn; ARowIndexInColumn: Integer; ASourceCellViewInfo: TdxTableCellViewInfo): TdxRow;
    function GetNextLayoutRowByDirection(ARows: TdxRowCollection; ASourceRowIndex: Integer): TdxRow;
    function IsBoundaryTableCellRowOnCurrentPage(ACell: TdxTableCellViewInfo; AColumn: TdxColumn;
      ASourceRowIndexInColumn, ACaretX: Integer): Boolean;
    function IsCaretAtTheBeginningOfAnotherCell(APos: TdxCaretPosition; ADocumentModel: TdxDocumentModel): Boolean;
    function IsBoundaryRowInCell(AColumn: TdxColumn; ACell: TdxTableCellViewInfo; ASourceRow: TdxRow): Boolean; virtual; abstract;
    function IsCellInBoundaryRow(AColumn: TdxColumn; ACell: TdxTableCellViewInfo; ACaretX: Integer): Boolean; virtual; abstract;
    function ObtainTargetLayoutRow(ARowIndexInColumn, ACaretX: Integer; AColumn: TdxColumn;
      ASourceCell: TdxTableCellViewInfo): TdxRow;
    function ObtainTargetLayoutRowFromNextCellInCurrentTable(ARowIndexInColumn: Integer; ACaretX: Integer;
      AColumn: TdxColumn; ASourceCellViewInfo: TdxTableCellViewInfo): TdxRow;
    function ObtainExistingTargetPageViewInfoRow(APageRows: TdxPageViewInfoRowCollection; ACurrentRowIndex: Integer): TdxPageViewInfoRow; virtual; abstract;
    function ShouldObtainTargetPageViewInfo(ASourceRowIndex, ASourceRowCount: Integer): Boolean; virtual; abstract;
    function ObtainTargetPageViewInfo(ACurrentPageViewInfo: TdxPageViewInfo; APhysicalCaretX: Integer): TdxPageViewInfo; virtual;
    function ObtainTargetLayoutRowFromNextCellInCurrentTableCore(ACaretX: Integer; AColumn: TdxColumn;
      ASourceCellViewInfo: TdxTableCellViewInfo; ANextCell: TdxTableCellViewInfo): TdxRow; virtual;
    function ObtainLayoutRowFromValidCellInFirstRowNestedTable(ARowIndex, ACaretX: Integer;
      AColumn: TdxColumn; ASourceNestedLevel: Integer): TdxRow; virtual;
    function ObtainRowFromOuterTextRowOrOuterTableWhenCellsInInnerTable(AColumn: TdxColumn;
      AInnerSourceCellViewInfo: TdxTableCellViewInfo; ACaretX: Integer; AFirstOrLastCellInRow: TdxTableCellViewInfo): TdxRow; virtual;
    function GetTargetPageArea(APage: TdxPage): TdxPageArea; virtual; abstract;
    function GetTargetLayoutRowCore(ARows: TdxRowCollection): TdxRow; virtual; abstract;
    function GetNextLayoutRowIndexInDirection(ASourceRowIndex: Integer): Integer; virtual; abstract;
    function GetTargetLayoutRowInCell(ARows: TdxRowCollection): TdxRow; virtual; abstract;
    function GetBoundaryRowInColumn(AColumn: TdxColumn): TdxRow; virtual; abstract;
    function GetCellBoundaryRowIndex(AColumn: TdxColumn; AParentCell: TdxTableCellViewInfo): Integer; virtual; abstract;
    function GetLastTableRowInDirection(ACell: TdxTableCellViewInfo; X: Integer): TdxTableRowViewInfoBase; virtual;
    function GetLastNonCoveredByVerticalMergingCell(ARow: TdxTableRowViewInfoBase): TdxTableCellViewInfo; virtual;
    function GetPreviousNonVerticalMergedCellInRow(ASourceCellViewInfo: TdxTableCellViewInfo): TdxTableCellViewInfo; virtual;
    function GetTargetTableRowCore(ARow: TdxTableRowViewInfoBase): TdxTableRowViewInfoBase; virtual; abstract;
    function GetFirstOrLastCellViewInfoInRow(ARow: TdxTableRowViewInfoBase): TdxTableCellViewInfo; virtual; abstract;
    function GetTargetCellViewInfoInNestedTable(ANextLayoutRow: TdxRow;
      ACaretX, ACurrentNestedLevel: Integer): TdxTableCellViewInfo; virtual; abstract;
    function GetFirstOrLastLayoutRowInCell(ACellRows: TdxRowCollection): TdxRow; virtual; abstract;
    function IsCellViewInfoContinuesOnAnotherPage(ACell: TdxTableCellViewInfo): Boolean; virtual; abstract;
    function IsLastOrFirstLayoutRowInCell(ARowIndexInCell, ACellsRowsCount: Integer): Boolean;
    function MoveFromInnerIntoOuterTable(AInnerSourceCellViewInfo: TdxTableCellViewInfo;
      AColumn: TdxColumn; AFirstOrLastCellInRow: TdxTableCellViewInfo; ACaretX: Integer): TdxRow; virtual; abstract;
    function MoveBetweenRowsInsideCell(ACellRows: TdxRowCollection; ARowIndexInCell: Integer): TdxRow; virtual;
    function ShouldJumpToNextCellInCurrentTable(AShouldJumpToCellInNestedTable: Boolean; ANextCell: TdxTableCellViewInfo;
      AIsLastOrFirstLayoutRowInCell: Boolean): Boolean; virtual;
    function ShouldObtainNextLayoutRowInsideCell(AIsLastOrFirstLayoutRowInCell: Boolean;
      ANextCell: TdxTableCellViewInfo): Boolean; virtual;
    function FindMostParentTableRowViewInfo(ATargetTable: TdxTable; AFirstColumnRowCell: TdxTableCellViewInfo): TdxTableRowViewInfoBase; virtual;

    property View: TdxRichEditView read GetView;
  public
    constructor Create(const AControl: IdxRichEditControl);

    function CalculateNextPosition(ACaretPosition: TdxCaretPosition): TdxDocumentModelPosition;
    function GetTargetCharacter(ARow: TdxRow; ACaretX: Integer): TdxCharacterBox;
    function GetTargetColumn(APage: TdxPage; ACaretX: Integer): TdxColumn;
    function ObtainTargetRowFromCurrentPageViewInfoAndCaretX(APageViewInfo: TdxPageViewInfo;
      ACellViewInfoFromPrevoiusTableViewInfo: TdxTableCellViewInfo; var ACaretX: Integer): TdxRow;

    property ActivePieceTable: TdxPieceTable read GetActivePieceTable;
    property Control: IdxRichEditControl read FControl;
    property FixedDirection: Boolean read FFixedDirection write FFixedDirection;
  end;

  { TdxNextCaretPositionUpDirectionCalculator }

  TdxNextCaretPositionUpDirectionCalculator = class abstract(TdxNextCaretPositionVerticalDirectionCalculator)
  private
    function ShouldJumpUpFromMultipleInnerTableToOuter(AFirstCellInTableRow: TdxTableCellViewInfo): Boolean;
    function ObtainRowInNextCellWhenFirstCellInTableRowIsInMultipleInnerTable(AColumn: TdxColumn; ACaretX: Integer;
      ATableViewInfo: TdxTableViewInfo): TdxRow;
  protected
    function GetBottomRightTableCellViewInfoByNestedLevel(ANextLayoutRow: TdxRow; ADesiredNestedLevel: Integer): TdxTableCellViewInfo; virtual;
    function GetBoundaryRowInColumn(AColumn: TdxColumn): TdxRow; override;
    function IsBoundaryRowInCell(AColumn: TdxColumn; ACell: TdxTableCellViewInfo; ASourceRow: TdxRow): Boolean; override;
    function GetCellBoundaryRowIndex(AColumn: TdxColumn; AParentCell: TdxTableCellViewInfo): Integer; override;

    function IsCellInBoundaryRow(AColumn: TdxColumn;
      ACell: TdxTableCellViewInfo; ACaretX: Integer): Boolean; override;
    procedure CorrectGeneratedRowPagesHorizontalLocation(ALayoutManager: TdxPageGeneratorLayoutManager;
      ARow: TdxPageViewInfoRow); override;
    function CalculateFirstInvalidPageIndex: Integer; override;
    function CalculateFirstInvisiblePageIndex: Integer; override;
    function GetTargetPageArea(APage: TdxPage): TdxPageArea; override;
    function GetTargetLayoutRowCore(ARows: TdxRowCollection): TdxRow; override;
    function GetFirstOrLastCellViewInfoInRow(ARow: TdxTableRowViewInfoBase): TdxTableCellViewInfo; override;
    function GetTargetTableRowCore(ARow: TdxTableRowViewInfoBase): TdxTableRowViewInfoBase; override;
    function GetTargetCellViewInfoInNestedTable(ANextLayoutRow: TdxRow;
      ACaretX: Integer; ACurrentNestedLevel: Integer): TdxTableCellViewInfo; override;
    function GetFirstOrLastLayoutRowInCell(ACellRows: TdxRowCollection): TdxRow; override;
    function IsCellViewInfoContinuesOnAnotherPage(ACell: TdxTableCellViewInfo): Boolean; override;
    function MoveFromInnerIntoOuterTable(AInnerSourceCellViewInfo: TdxTableCellViewInfo;
      AColumn: TdxColumn; AFirstOrLastCellInRow: TdxTableCellViewInfo;
      ACaretX: Integer): TdxRow; override;
  end;

  TdxNextCaretPositionDownDirectionCalculator = class abstract(TdxNextCaretPositionVerticalDirectionCalculator)
  private
    function GetTopLeftTableCellViewInfoByNestedLevel(ANextLayoutRow: TdxRow; ADesiredNestedLevel: Integer): TdxTableCellViewInfo;
  protected
    function GetBoundaryRowInColumn(AColumn: TdxColumn): TdxRow; override;
    function IsBoundaryRowInCell(AColumn: TdxColumn; ACell: TdxTableCellViewInfo; ASourceRow: TdxRow): Boolean; override;
    function GetCellBoundaryRowIndex(AColumn: TdxColumn; AParentCell: TdxTableCellViewInfo): Integer; override;
    function GetTargetPageArea(APage: TdxPage): TdxPageArea; override;
    function GetTargetLayoutRowCore(ARows: TdxRowCollection): TdxRow; override;
    function GetTargetLayoutRowInCell(ARows: TdxRowCollection): TdxRow; override;
    function IsCellInBoundaryRow(AColumn: TdxColumn;
      ACell: TdxTableCellViewInfo; ACaretX: Integer): Boolean; override;
    function CalculateFirstInvalidPageIndex: Integer; override;
    function CalculateFirstInvisiblePageIndex: Integer; override;
    procedure CorrectGeneratedRowPagesHorizontalLocation(ALayoutManager: TdxPageGeneratorLayoutManager;
      ARow: TdxPageViewInfoRow); override;
    function GetFirstOrLastCellViewInfoInRow(ARow: TdxTableRowViewInfoBase): TdxTableCellViewInfo; override;
    function GetTargetCellViewInfoInNestedTable(ANextLayoutRow: TdxRow;
      ACaretX: Integer; ACurrentNestedLevel: Integer): TdxTableCellViewInfo; override;
    function GetTargetTableRowCore(ARow: TdxTableRowViewInfoBase): TdxTableRowViewInfoBase; override;
    function GetFirstOrLastLayoutRowInCell(ACellRows: TdxRowCollection): TdxRow; override;
    function IsCellViewInfoContinuesOnAnotherPage(ACell: TdxTableCellViewInfo): Boolean; override;
    function MoveFromInnerIntoOuterTable(AInnerSourceCellViewInfo: TdxTableCellViewInfo;
      AColumn: TdxColumn; ALastCellInTableRow: TdxTableCellViewInfo;
      ACaretX: Integer): TdxRow; override;
  end;

  { TdxNextCaretPositionLineUpCalculator }

  TdxNextCaretPositionLineUpCalculator = class(TdxNextCaretPositionUpDirectionCalculator)
  protected
    function ShouldObtainTargetPageViewInfo(ASourceRowIndex: Integer;
      ASourceRowCount: Integer): Boolean; override;
    function GetNextLayoutRowIndexInDirection(ASourceRowIndex: Integer): Integer; override;
    function GetTargetLayoutRowInCell(ARows: TdxRowCollection): TdxRow; override;
    function ObtainExistingTargetPageViewInfoRow(APageRows: TdxPageViewInfoRowCollection;
      ACurrentRowIndex: Integer): TdxPageViewInfoRow; override;
  end;

  { TdxExtendNextCaretPositionLineUpCalculator }

  TdxExtendNextCaretPositionLineUpCalculator = class(TdxNextCaretPositionLineUpCalculator)
  private
    function GetSelection: TdxSelection;
    function GetSelectedCells: TdxSelectedTableStructureBase;
    procedure SetSelectedCells(const Value: TdxSelectedTableStructureBase);
  protected
    function GetDefaultPosition: TdxDocumentModelPosition; override;
    function ShouldJumpToNextCellInCurrentTable(AShouldJumpToCellInNestedTable: Boolean;
      ANextCell: TdxTableCellViewInfo;
      AIsLastOrFirstLayoutRowInCell: Boolean): Boolean; override;
    property Selection: TdxSelection read GetSelection;
    property SelectedCells: TdxSelectedTableStructureBase read GetSelectedCells write SetSelectedCells;
  end;

  { TdxNextCaretPositionLineDownCalculator }

  TdxNextCaretPositionLineDownCalculator = class(TdxNextCaretPositionDownDirectionCalculator)
  protected
    function GetNextLayoutRowIndexInDirection(ASourceRowIndex: Integer): Integer; override;
    function ShouldObtainTargetPageViewInfo(ASourceRowIndex: Integer;
      ASourceRowCount: Integer): Boolean; override;
    function ObtainExistingTargetPageViewInfoRow(APageRows: TdxPageViewInfoRowCollection;
      ACurrentRowIndex: Integer): TdxPageViewInfoRow; override;
  end;

  { TdxExtendNextCaretPositionLineDownCalculator }

  TdxExtendNextCaretPositionLineDownCalculator = class(TdxNextCaretPositionLineDownCalculator)
  private
    function GetSelection: TdxSelection;
    function GetSelectedCells: TdxSelectedTableStructureBase;
    procedure SetSelectedCells(const Value: TdxSelectedTableStructureBase);
  protected
    procedure CorrectCaretXOnExtendSelection(ASourceCellViewInfo: TdxTableCellViewInfo;
      var ACaretX: Integer); override;
    function GetDefaultPosition: TdxDocumentModelPosition; override;
    function MoveFromInnerIntoOuterTable(AInnerSourceCellViewInfo: TdxTableCellViewInfo;
      AColumn: TdxColumn; ALastCellInTableRow: TdxTableCellViewInfo;
      ACaretX: Integer): TdxRow; override;
    function ShouldJumpToNextCellInCurrentTable(AShouldJumpToCellInNestedTable: Boolean;
      ANextCell: TdxTableCellViewInfo;
      AIsLastOrFirstLayoutRowInCell: Boolean): Boolean; override;
    property Selection: TdxSelection read GetSelection;
    property SelectedCells: TdxSelectedTableStructureBase read GetSelectedCells write SetSelectedCells;
  end;

  { TdxExtendKeyboardSelectionHorizontalDirectionCalculator }

  TdxExtendKeyboardSelectionHorizontalDirectionCalculator = class
  strict private
    FDocumentModel: TdxDocumentModel;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);

    function CalculateNextPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
    function CalculatePrevPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;

    property DocumentModel: TdxDocumentModel read FDocumentModel;
  end;

  { TdxToggleShowWhitespaceCommand }

  TdxToggleShowWhitespaceCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure EnsureSelectionVisible; virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxChangeIndentCommand }

  TdxChangeIndentCommand = class abstract(TdxRichEditMenuItemSimpleCommand)
  private
    function GetStartIndex: TdxParagraphIndex;
    function GetEndIndex: TdxParagraphIndex;
  protected
    function GetEndParagraphIndex: TdxParagraphIndex;
    function SelectedOnlyParagraphWithNumeration: Boolean; virtual;
    function SelectedFirstParagraphInList: Boolean; virtual;
    function GetStartParagraphLayoutPosition: TdxDocumentLayoutPosition;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property StartIndex: TdxParagraphIndex read GetStartIndex;
    property EndIndex: TdxParagraphIndex read GetEndIndex;
  public
    function SelectionBeginFirstRowStartPos: Boolean; virtual;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxIncrementIndentCommand }

  TdxIncrementIndentCommand = class(TdxChangeIndentCommand)
  protected
    procedure ExecuteCore; override;
    procedure ProcessNumerationParagraph; virtual;
    procedure IncrementNumerationParagraphIndent; virtual;
    procedure IncrementNumerationFromParagraph; virtual;
    procedure IncrementParagraphIndent; virtual;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDecrementIndentCommand }

  TdxDecrementIndentCommand = class(TdxChangeIndentCommand)
  protected
    procedure ExecuteCore; override;
    procedure ProcessNumerationParagraph; virtual;
    procedure DecrementNumerationParagraphIndent; virtual;
    procedure DecrementNumerationFromParagraph; virtual;
    procedure DecrementParagraphIndent; virtual;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleShowRulersCommandBase }

  TdxToggleShowRulersCommandBase = class abstract(TdxRichEditMenuItemSimpleCommand)
  protected
    function GetShowRulerByDefault: Boolean; virtual; abstract;
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function IsRulerVisible(AVisibility: TdxRichEditRulerVisibility): Boolean; virtual;
    function GetModifyOptions: TdxRulerOptions; virtual; abstract;
  public
    property ShowRulerByDefault: Boolean read GetShowRulerByDefault;
  end;

  { TdxToggleShowHorizontalRulerCommand }

  TdxToggleShowHorizontalRulerCommand = class(TdxToggleShowRulersCommandBase)
  protected
    function GetShowRulerByDefault: Boolean; override;
    function GetModifyOptions: TdxRulerOptions; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleShowVerticalRulerCommand }

  TdxToggleShowVerticalRulerCommand = class(TdxToggleShowRulersCommandBase)
  protected
    function GetShowRulerByDefault: Boolean; override;
    function GetModifyOptions: TdxRulerOptions; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxZoomCommandBase }

  TdxZoomCommandBase = class abstract(TdxRichEditMenuItemSimpleCommand)
  public const
    DefaultZoomFactorDelta = 0.1;
  strict private
    FDelta: TdxNullableValue<Single>;
    function GetZoomFactor: Single;
    function GetActualDelta: Single;
    procedure SetZoomFactor(const Value: Single);
  protected
    function CalculateNewZoomFactor(AOldZoomFactor: Single): Single; virtual; abstract;
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    property ZoomFactor: Single read GetZoomFactor write SetZoomFactor;
    property Delta: TdxNullableValue<Single> read FDelta write FDelta;
    property ActualDelta: Single read GetActualDelta;
  end;

  { TdxZoomInCommand }

  TdxZoomInCommand = class(TdxZoomCommandBase)
  protected
    function CalculateNewZoomFactor(AOldZoomFactor: Single): Single; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxZoomOutCommand }

  TdxZoomOutCommand = class(TdxZoomCommandBase)
  protected
    function CalculateNewZoomFactor(AOldZoomFactor: Single): Single; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxZoomCommand }

  TdxZoomCommand = class(TdxZoomCommandBase)
  strict private
    FZoomFactor: Single;
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function CalculateNewZoomFactor(AOldZoomFactor: Single): Single; override;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxZoomPercentCommand }

  TdxZoomPercentCommand = class(TdxZoomCommandBase)
  strict private
    FZoomPercent: Single;
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function CalculateNewZoomFactor(AOldZoomFactor: Single): Single; override;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSwitchActiveViewCommand }

  TdxSwitchActiveViewCommand = class abstract(TdxRichEditMenuItemSimpleCommand)
  protected
    function GetViewType: TdxRichEditViewType; virtual; abstract;
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property ViewType: TdxRichEditViewType read GetViewType;
  end;

  { TdxSwitchToDraftViewCommand }

  TdxSwitchToDraftViewCommand = class(TdxSwitchActiveViewCommand)
  protected
    function GetViewType: TdxRichEditViewType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSwitchToSimpleViewCommand }

  TdxSwitchToSimpleViewCommand = class(TdxSwitchActiveViewCommand)
  protected
    function GetViewType: TdxRichEditViewType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSwitchToPrintLayoutViewCommand }

  TdxSwitchToPrintLayoutViewCommand = class(TdxSwitchActiveViewCommand)
  protected
    function GetViewType: TdxRichEditViewType; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxChangePageColorCommand }

  TdxChangePageColorCommand = class(TdxRichEditMenuItemSimpleCommand)
  private
    FColor: TdxAlphaColor;
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property Color: TdxAlphaColor read FColor write FColor;
  end;

implementation

uses
  Contnrs, Math, dxTypeHelpers, dxCore,
  dxRichEdit.Commands.Images,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxCharacters,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.Commands.Selection,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.InnerControl,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.Commands.Numbering;

{ TdxRichEditCaretBasedCommand }

procedure TdxRichEditCaretBasedCommand.ApplyLayoutPreferredPageIndex(
  APreferredPageIndex: Integer);
const
  ChangeActions = [
    TdxDocumentModelChangeAction.RaiseSelectionChanged,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
    TdxDocumentModelChangeAction.Redraw];
begin
  if APreferredPageIndex < 0 then
    Exit;
  CaretPosition.PreferredPageIndex := APreferredPageIndex;
  if CaretPosition is TdxHeaderFooterCaretPosition then
  begin
    if DocumentModel.IsUpdateLockedOrOverlapped then
      TdxHeaderFooterCaretPosition(CaretPosition).LayoutPosition.PieceTable.ApplyChangesCore(ChangeActions, -1, -1);
  end;
  ActiveView.SelectionLayout.PreferredPageIndex := APreferredPageIndex;
end;

function TdxRichEditCaretBasedCommand.GetCaretPosition: TdxCaretPosition;
begin
  Result := ActiveView.CaretPosition;
end;

function TdxRichEditCaretBasedCommand.UpdateCaretPosition(
  ADetailLevel: TdxDocumentLayoutDetailsLevel): Boolean;
begin
  Result := CaretPosition.Update(ADetailLevel);
end;

{ TdxPlaceCaretToPhysicalPointCommand }

function TdxPlaceCaretToPhysicalPointCommand.CalculatePreferredPageIndex(
  AHitTestResult: TdxRichEditHitTestResult): Integer;
var
  AHitTestRequest: TdxRichEditHitTestRequest;
  ANewHitTestResult: TdxRichEditHitTestResult;
begin
  if AHitTestResult.PieceTable.IsMain and AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Page) then
    Exit(AHitTestResult.Page.PageIndex);

  AHitTestRequest := TdxRichEditHitTestRequest.Create(DocumentModel.MainPieceTable);
  AHitTestRequest.PhysicalPoint := PhysicalPoint;
  AHitTestRequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Page;
  AHitTestRequest.Accuracy := NearestPage or NearestPageArea or NearestColumn or NearestTableRow or
    NearestTableCell or NearestRow or NearestBox or NearestCharacter;
  if HitTestOnlyInPageClientBounds then
    AHitTestRequest.Accuracy := AHitTestRequest.Accuracy or ActiveView.DefaultHitTestPageAccuracy;

  ANewHitTestResult := ActiveView.HitTestCore(AHitTestRequest, HitTestOnlyInPageClientBounds);
  try
    if not ANewHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Page) then
      Result := 0
    else
      Result := ANewHitTestResult.Page.PageIndex;
  finally
    ANewHitTestResult.Free;
  end;
end;

procedure TdxPlaceCaretToPhysicalPointCommand.ChangeSelection(
  ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition;
  AHitTestResult: TdxRichEditHitTestResult);
var
  AUsePreviousBoxBounds: Boolean;
  ASelectionItemCountBeforeChangeEnd: Integer;
begin
  ASelectionItemCountBeforeChangeEnd := ASelection.Items.Count;
  AUsePreviousBoxBounds := ChangeSelectionEnd(ASelection, ALogPosition, AHitTestResult);

  ChangeSelectionStart(ASelection, ALogPosition, AHitTestResult, ASelectionItemCountBeforeChangeEnd);

  ASelection.UsePreviousBoxBounds := AUsePreviousBoxBounds;
  if ExtendSelection then
    ValidateSelection;
end;

function TdxPlaceCaretToPhysicalPointCommand.ChangeSelectionEnd(
  ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition;
  AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  ASelectionManager: TdxEnhancedSelectionManager;
begin
  ASelectionManager := TdxEnhancedSelectionManager.Create(ActivePieceTable);
  try
    Result := ASelectionManager.ShouldSelectToTheEndOfRow(AHitTestResult) and
      SelectToTheEndOfRow(ASelection, AHitTestResult.Row, ExtendSelection);
    if not Result then
    begin
      ASelection.&End := ALogPosition;
      if ExtendSelection then
        ASelection.UpdateTableSelectionEnd(ALogPosition);
    end;
  finally
    ASelectionManager.Free;
  end;
end;

procedure TdxPlaceCaretToPhysicalPointCommand.ChangeSelectionStart(
  ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition;
  AHitTestResult: TdxRichEditHitTestResult; ASelectionItemCountBeforeChangeEnd: Integer);
var
  ASelectionManager: TdxEnhancedSelectionManager;
begin
  if not ExtendSelection then
  begin
    ASelection.Start := ASelection.&End;
    if ASelection.Items.Count > ASelectionItemCountBeforeChangeEnd then
      ASelection.ClearMultiSelection(ASelectionItemCountBeforeChangeEnd - 1);
    ASelection.SetStartCell(ALogPosition);
  end
  else
  begin
    ASelectionManager := TdxEnhancedSelectionManager.Create(ActivePieceTable);
    try
      ASelection.Start := ASelectionManager.ExtendSelectionStartToParagraphMark(ASelection, ALogPosition);
      ASelection.UpdateTableSelectionStart(ALogPosition);
    finally
      ASelectionManager.Free;
    end;
  end;
end;

constructor TdxPlaceCaretToPhysicalPointCommand.Create(
  const ARichEditControl: IdxRichEditControl);
begin
  inherited Create(ARichEditControl);
  FUpdateCaretX := True;
end;

procedure TdxPlaceCaretToPhysicalPointCommand.ExecuteCore;
var
  AHitTestRequest: TdxRichEditHitTestRequest;
  AHitTestResult: TdxRichEditHitTestResult;
  ALogPosition: TdxDocumentLogPosition;
  ASelection: TdxSelection;
  APageViewInfo: TdxPageViewInfo;
  ACell: TdxTableCellViewInfo;
begin
  CheckExecutedAtUIThread;

  AHitTestRequest := TdxRichEditHitTestRequest.Create(DocumentModel.ActivePieceTable);
  AHitTestRequest.PhysicalPoint := PhysicalPoint;
  AHitTestRequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Character;
  AHitTestRequest.Accuracy := NearestPage or NearestPageArea or NearestColumn or NearestTableRow or
    NearestTableCell or NearestRow or NearestBox or NearestCharacter;
  if HitTestOnlyInPageClientBounds then
    AHitTestRequest.Accuracy := AHitTestRequest.Accuracy or ActiveView.DefaultHitTestPageAccuracy;

  AHitTestResult := ActiveView.HitTestCore(AHitTestRequest, HitTestOnlyInPageClientBounds);
  try
    if not AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Character) then
      FreeAndNil(AHitTestResult)
    else
    begin
      ALogPosition := AHitTestResult.Character.GetFirstPosition(AHitTestResult.PieceTable).LogPosition;
      ASelection := DocumentModel.Selection;
      DocumentModel.BeginUpdate;
      try
        DocumentModel.DeferredChanges.SuppressClearOutdatedSelectionItems := SuppressClearOutdatedSelectionItems;
        ChangeSelection(ASelection, ALogPosition, AHitTestResult);
        if UpdateCaretX then
          ApplyLayoutPreferredPageIndex(CalculatePreferredPageIndex(AHitTestResult));
      finally
        DocumentModel.EndUpdate;
      end;
      if UpdateCaretX then
      begin
        CaretPosition.Update(TdxDocumentLayoutDetailsLevel.Character);
        Assert(CaretPosition.LayoutPosition.IsValid(TdxDocumentLayoutDetailsLevel.Character));
        APageViewInfo := CaretPosition.PageViewInfo;
        if APageViewInfo <> nil then
        begin
          CaretPosition.X := ActiveView.CreateLogicalPoint(APageViewInfo.ClientBounds, PhysicalPoint).X;
          ACell := CaretPosition.LayoutPosition.TableCell;
          if ACell <> nil then
            CaretPosition.TableCellTopAnchorIndex := ACell.TopAnchorIndex
          else
            CaretPosition.TableCellTopAnchorIndex := -1;
        end;
      end;
    end;
  finally
    AHitTestResult.Free;
  end;
end;

function TdxPlaceCaretToPhysicalPointCommand.ExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxPlaceCaretToPhysicalPointCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPlaceCaretToPhysicalPointDescription);
end;

class function TdxPlaceCaretToPhysicalPointCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPlaceCaretToPhysicalPointMenuCaption);
end;

function TdxPlaceCaretToPhysicalPointCommand.HitTestOnlyInPageClientBounds: Boolean;
begin
  Result := True;
end;

function TdxPlaceCaretToPhysicalPointCommand.SelectToTheEndOfRow(
  ASelection: TdxSelection; ARow: TdxRow; AExtendSelection: Boolean): Boolean;
var
  ALogPosition: TdxDocumentLogPosition;
  APos: TdxDocumentModelPosition;
  ARun: TdxRunBase;
  ALineBreak: Boolean;
begin
  if AExtendSelection then
  begin
    ALogPosition := ARow.GetLastPosition(ActivePieceTable).LogPosition;
    ASelection.&End := ALogPosition + 1;
    ASelection.UpdateTableSelectionEnd(ALogPosition);
    Result := True;
  end
  else
  begin
    ARun := ARow.Boxes.Last.GetRun(ActivePieceTable);
    Result := not (ARun is TdxParagraphRun);
    if Result then
    begin
      APos := ARow.GetLastPosition(ActivePieceTable);
      ALineBreak := ActivePieceTable.TextBuffer[ARun.StartIndex + APos.RunOffset] = TdxCharacters.LineBreak;
      ALogPosition := APos.LogPosition;
      ASelection.&End := ALogPosition;
      if not ALineBreak then
        ASelection.&End := ASelection.&End + 1;
      ASelection.UpdateTableSelectionEnd(ALogPosition);
      Result := not ALineBreak;
    end;
  end;
end;

procedure TdxPlaceCaretToPhysicalPointCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := True;
  AState.Visible := True;
end;

procedure TdxPlaceCaretToPhysicalPointCommand.ValidateSelection;
var
  AValidator: TdxFieldIsSelectValidator;
begin
  AValidator := TdxFieldIsSelectValidator.Create(ActivePieceTable);
  try
    AValidator.ValidateSelection(DocumentModel.Selection);
  finally
    AValidator.Free;
  end;
end;

{ TdxPlaceCaretToPhysicalPointCommand2 }

function TdxPlaceCaretToPhysicalPointCommand2.ChangeSelectionEnd(
  ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition;
  AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  ASelectionManager: TdxEnhancedSelectionManager;
begin
  ASelectionManager := TdxEnhancedSelectionManager.Create(AHitTestResult.PieceTable);
  try
    Result := (ASelectionManager.ShouldSelectEntireRow(AHitTestResult) and
      SelectToTheEndOfRow(ASelection, AHitTestResult.Row, true)) or
      inherited ChangeSelectionEnd(ASelection, ALogPosition, AHitTestResult)
  finally
    ASelectionManager.Free;
  end;
end;

{ TdxScrollVerticallyByPhysicalOffsetCommand }

constructor TdxScrollVerticallyByPhysicalOffsetCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FPhysicalOffset := DocumentModel.LayoutUnitConverter.DocumentsToLayoutUnits(50);
end;

function TdxScrollVerticallyByPhysicalOffsetCommand.GetAbsolutePhysicalVerticalOffset: Integer;
begin
  Result := PhysicalOffset;
end;

{ TdxScrollVerticallyByPhysicalOffsetCommandBase }

constructor TdxScrollVerticallyByPhysicalOffsetCommandBase.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FUpdateScrollBarBeforeExecution := True;
end;

function TdxScrollVerticallyByPhysicalOffsetCommandBase.CreateScrollDeltaCalculator(
  AScrollUp: Boolean): TdxScrollByPhysicalHeightCalculator;
begin
  if AScrollUp then
    Result := TdxScrollUpByPhysicalHeightCalculator.Create(View)
  else
    Result := TdxScrollDownByPhysicalHeightCalculator.Create(View);
end;

procedure TdxScrollVerticallyByPhysicalOffsetCommandBase.ExecuteCore;

  function Sign(Value: Int64): Integer;
  begin
    if Value = 0 then
      Result := 0
    else
      if Value < 0 then
        Result := -1
      else
        Result := 1;
  end;

var
  AOffset: Integer;
  ACalculator: TdxScrollByPhysicalHeightCalculator;
  ADelta, APreviousValue: Int64;
begin
  View.CheckExecutedAtUIThread;
  if UpdateScrollBarBeforeExecution then
    View.UpdateVerticalScrollBar;
  AOffset := AbsolutePhysicalVerticalOffset;
  ACalculator := CreateScrollDeltaCalculator(AOffset < 0);
  try
    ADelta := ACalculator.CalculateScrollDelta(Abs(AOffset));
    if ADelta = 0 then
      Exit;
    APreviousValue := View.VerticalScrollController.ScrollBarAdapter.Value;
    GeneratePagesToEnsureScrollingSuccessfull(ADelta * Sign(AOffset));
    View.VerticalScrollController.ScrollByTopInvisibleHeightDelta(ADelta * Sign(AOffset));
    View.OnVerticalScroll;
    FExecutedSuccessfully := APreviousValue <> View.VerticalScrollController.ScrollBarAdapter.Value;
  finally
    ACalculator.Free;
  end;
end;

procedure TdxScrollVerticallyByPhysicalOffsetCommandBase.GeneratePagesToEnsureScrollingSuccessfull(ADelta: Int64);
begin
end;

class function TdxScrollVerticallyByPhysicalOffsetCommandBase.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

class function TdxScrollVerticallyByPhysicalOffsetCommandBase.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

procedure TdxScrollVerticallyByPhysicalOffsetCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := True;
  AState.Visible := True;
end;

{ TdxScrollVerticallyByLogicalOffsetCommand }

constructor TdxScrollVerticallyByLogicalOffsetCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FLogicalOffset := DocumentModel.UnitConverter.DocumentsToModelUnits(150);
end;

function TdxScrollVerticallyByLogicalOffsetCommand.GetAbsolutePhysicalVerticalOffset: Integer;
begin
  Result := Round(LogicalOffset * ActiveView.ScaleFactor);
end;

{ TdxRichEditMenuItemSimpleCommand }

procedure TdxRichEditMenuItemSimpleCommand.ForceExecute(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try
    ExecuteCore;
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

function TdxRichEditMenuItemSimpleCommand.GetForceVisible: Boolean;
begin
  Result := ((CommandSourceType = TdxCommandSourceType.Keyboard) or
    (CommandSourceType = TdxCommandSourceType.Menu) or
    (CommandSourceType = TdxCommandSourceType.Mouse)) and
    not DocumentModel.FormattingMarkVisibilityOptions.ShowHiddenText;
end;

function TdxRichEditMenuItemSimpleCommand.GetSelectedCellsCollection: TdxSelectedCellsCollection;
var
  ACells: TdxSelectedCellsCollection;
  ASelectedCells: TdxSelectedCellsCollection;
  ASelection: TdxSelection;
  ASelectionPieceTable: TdxPieceTable;
  AStartPos: TdxDocumentModelPosition;
  AStartCell: TdxTableCell;
  AEndPos: TdxDocumentModelPosition;
  AEndCell: TdxTableCell;
  ASelectedCell: TdxSelectedCellsIntervalInRow;
  I: Integer;
begin
  ACells := TdxSelectedCellsCollection.Create;
  if DocumentModel.Selection.Items.Count > 1 then
  begin
    ASelection := DocumentModel.Selection;
    ASelectionPieceTable := ASelection.PieceTable;
    for I := 0 to ASelection.Items.Count - 1 do
    begin
      AStartPos := TdxPositionConverter.ToDocumentModelPosition(ASelectionPieceTable, ASelection.Items[I].NormalizedStart);
      AStartCell := ASelectionPieceTable.Paragraphs[AStartPos.ParagraphIndex].GetCell;
      AEndPos := TdxPositionConverter.ToDocumentModelPosition(ASelectionPieceTable, ASelection.Items[I].NormalizedEnd - 1);
      AEndCell := ASelectionPieceTable.Paragraphs[AEndPos.ParagraphIndex].GetCell;
      if (AStartCell = nil) or (AEndCell = nil) then
      begin
        ACells.Free;
        Exit(nil);
      end;
      ACells.AddSelectedCells(AStartCell.Row, AStartCell.IndexInRow, AEndCell.IndexInRow);
    end;
  end
  else
  begin
    ASelectedCells := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
    ACells.OriginalStartLogPosition := ASelectedCells.OriginalStartLogPosition;
    for I := 0 to ASelectedCells.RowsCount - 1 do
    begin
      ASelectedCell := ASelectedCells.Items[I];
      ACells.AddSelectedCells(ASelectedCell.Row, ASelectedCell.NormalizedStartCellIndex, ASelectedCell.NormalizedEndCellIndex);
    end;
  end;
  Result := ACells;
end;

{ TdxScrollHorizontallyByPhysicalOffsetCommand }

constructor TdxScrollHorizontallyByPhysicalOffsetCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FPhysicalOffset := DocumentModel.LayoutUnitConverter.DocumentsToLayoutUnits(50);
end;

procedure TdxScrollHorizontallyByPhysicalOffsetCommand.ExecuteCore;
begin
  View.CheckExecutedAtUIThread;
  PerformScroll;
  View.OnHorizontalScroll;
end;

function TdxScrollHorizontallyByPhysicalOffsetCommand.GetActiveView: TdxRichEditView;
begin
  Result := InnerControl.ActiveView;
end;

class function TdxScrollHorizontallyByPhysicalOffsetCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

class function TdxScrollHorizontallyByPhysicalOffsetCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

procedure TdxScrollHorizontallyByPhysicalOffsetCommand.PerformScroll;
var
  APreviousValue: Int64;
begin
  APreviousValue := View.HorizontalScrollController.ScrollBarAdapter.Value;
  View.HorizontalScrollController.ScrollByLeftInvisibleWidthDelta(PhysicalOffset);
  FExecutedSuccessfully := APreviousValue <> View.HorizontalScrollController.ScrollBarAdapter.Value;
end;

procedure TdxScrollHorizontallyByPhysicalOffsetCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := True;
  AState.Visible := True;
end;

{ TdxSelectionValidator }

constructor TdxSelectionValidator.Create(APieceTable: TdxPieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
end;

function TdxSelectionValidator.CalculateOffset(
  AParagraphIndex: TdxParagraphIndex; AIncrementIndex: Boolean): Integer;
var
  ACell: TdxTableCell;
begin
  Result := 0;
  while True do
  begin
    ACell := PieceTable.Paragraphs[AParagraphIndex].GetCell;
    if ACell = nil then
      Break;
    if ACell.VerticalMerging <> TdxMergingState.Continue then
      Break;
    if AIncrementIndex then
      Inc(AParagraphIndex)
    else
      Dec(AParagraphIndex);
    Inc(Result);
  end;
end;

procedure TdxSelectionValidator.ChangeFieldSelection(ASelection: TdxSelection;
  ASelectedInterval: TdxRunInfo; AField: TdxField);
begin
  if ShouldExtendInterval(ASelectedInterval, AField) then
    ChangeFieldSelectionCore(ASelection, AField);
  if AField.Parent <> nil then
    ChangeFieldSelection(ASelection, ASelectedInterval, AField.Parent);
end;

function TdxSelectionValidator.IsSelectedItemCovered(AItem: TdxSelectionItem): Boolean;
var
  I: TdxDocumentLogPosition;
  ACell: TdxTableCell;
begin
  for I := AItem.NormalizedStart to AItem.NormalizedEnd - 1 do
  begin
    ACell := PieceTable.FindParagraph(I).GetCell;
    if (ACell = nil) or (ACell.VerticalMerging <> TdxMergingState.Continue) then
      Exit(False);
  end;
  Result := True;
end;

function TdxSelectionValidator.ShouldChangeInterval(AInterval: TdxRunInfo;
  AStart, AEnd: TdxRunIndex): Boolean;
begin
  Result := ((AInterval.Start.RunIndex > AStart) and (AInterval.&End.RunIndex >= AEnd)) or
    ((AInterval.Start.RunIndex <= AStart) and (AInterval.&End.RunIndex < AEnd));
end;

function TdxSelectionValidator.ShouldExtendInterval(AInterval: TdxRunInfo;
  AField: TdxField): Boolean;
begin
  if AField.IsCodeView then
    Result := ShouldChangeInterval(AInterval, AField.Code.Start, AField.Code.&End)
  else
    Result := ShouldChangeInterval(AInterval, AField.Code.&End, AField.Result.&End);
end;

function TdxSelectionValidator.GetDocumentModel: TdxDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

procedure TdxSelectionValidator.ValidateFieldSelectionCore(
  ASelection: TdxSelection);
var
  ASelectedInterval: TdxRunInfo;
  ALastField, AFirstField: TdxField;
begin
  ASelectedInterval := PieceTable.FindRunInfo(ASelection.NormalizedStart, ASelection.Length);
  try
    ALastField := PieceTable.FindFieldByRunIndex(ASelectedInterval.&End.RunIndex);
    if ALastField <> nil then
      ChangeFieldSelection(ASelection, ASelectedInterval, ALastField);
    AFirstField := PieceTable.FindFieldByRunIndex(ASelectedInterval.Start.RunIndex);
    if (AFirstField <> ALastField) and (AFirstField <> nil) then
      ChangeFieldSelection(ASelection, ASelectedInterval, AFirstField);
  finally
    ASelectedInterval.Free;
  end;
end;

procedure TdxSelectionValidator.ValidateSelection(ASelection: TdxSelection);
begin
  if ASelection.Length = 0 then
    ValidateTableActiveSelection(ASelection)
  else
  begin
    ValidateFieldSelectionCore(ASelection);
    ValidateTableSelectionCore(ASelection);
  end;
end;

procedure TdxSelectionValidator.ValidateTableActiveSelection(
  ASelection: TdxSelection);
var
  AActiveSelection: TdxSelectionItem;
  AParagraphIndex: TdxParagraphIndex;
  AOffset: Integer;
begin
  if ASelection.Items.Count <> 1 then
    Exit;
  AActiveSelection := ASelection.ActiveSelection;
  AParagraphIndex := PieceTable.FindParagraphIndex(AActiveSelection.Start);
  AOffset := CalculateOffset(AParagraphIndex, True);
  AActiveSelection.LeftOffset := AOffset;
  AActiveSelection.RightOffset := -AOffset;
end;

procedure TdxSelectionValidator.ValidateTableSelectionCore(
  ASelection: TdxSelection);
var
  ASelectionsCount: Integer;
  I: Integer;
  ASelectionItem: TdxSelectionItem;
  AParagraphIndex: TdxParagraphIndex;
  ALeftOffset, ARightOffset: Integer;
begin
  ASelectionsCount := ASelection.Items.Count;
  for I := 0 to ASelectionsCount - 1 do
  begin
    ASelectionItem := ASelection.Items[i];
    ASelectionItem.IsCovered := False;
    if (ASelectionsCount > 1) and IsSelectedItemCovered(ASelectionItem) then
    begin
      ASelectionItem.IsCovered := True;
      Continue;
    end;
    AParagraphIndex := PieceTable.FindParagraphIndex(ASelectionItem.NormalizedStart);
    ALeftOffset := CalculateOffset(AParagraphIndex, True);
    AParagraphIndex := PieceTable.FindParagraphIndex(ASelectionItem.NormalizedEnd);
    ARightOffset := CalculateOffset(AParagraphIndex, False);
    ASelectionItem.LeftOffset := ALeftOffset;
    ASelectionItem.RightOffset := ARightOffset;
  end;
end;

{ TdxFieldIsSelectValidator }

procedure TdxFieldIsSelectValidator.ChangeFieldSelectionCore(
  ASelection: TdxSelection; AField: TdxField);
var
  AStartPos, AEndPos: TdxDocumentModelPosition;
begin
  AStartPos := TdxDocumentModelPosition.FromRunStart(PieceTable, AField.FirstRunIndex);
  AEndPos := TdxDocumentModelPosition.FromRunEnd(PieceTable, AField.LastRunIndex);
  if ASelection.&End > ASelection.Start then
  begin
    ASelection.Start := Min(ASelection.Start, AStartPos.LogPosition);
    ASelection.&End := Max(ASelection.&End, AEndPos.LogPosition);
  end
  else
    if ASelection.&End < ASelection.Start then
    begin
      ASelection.&End := Min(ASelection.&End, AStartPos.LogPosition);
      ASelection.Start := Max(ASelection.Start, AEndPos.LogPosition);
    end;
end;

{ TdxFieldIsUnselectValidator }

procedure TdxFieldIsUnselectValidator.ChangeFieldSelectionCore(
  ASelection: TdxSelection; AField: TdxField);
var
  AStartPos, AEndPos: TdxDocumentModelPosition;
begin
  if ASelection.&End > ASelection.Start then
  begin
    AStartPos := TdxDocumentModelPosition.FromRunStart(PieceTable, AField.FirstRunIndex);
    ASelection.&End := Min(ASelection.&End, AStartPos.LogPosition);
  end
  else
    if ASelection.&End < ASelection.Start then
    begin
      AEndPos := TdxDocumentModelPosition.FromRunEnd(PieceTable, AField.LastRunIndex);
      ASelection.&End := Max(ASelection.&End, AEndPos.LogPosition);
    end;
end;

{ TdxEnsureCaretVisibleHorizontallyCommand }

function TdxEnsureCaretVisibleHorizontallyCommand.CalculateOptimalCaretHorizontalPosition(
  const ALogicalVisibleBounds, ACaretBounds: TRect): Integer;
var
  AOptimalLogicalVisibleBounds: TRect;
  AColumnBounds: TRect;
  APageBounds: TRect;
begin
  AOptimalLogicalVisibleBounds := ALogicalVisibleBounds;
  AOptimalLogicalVisibleBounds.MoveToLeft(ACaretBounds.Left - 2 * ALogicalVisibleBounds.Width div 3);
  AColumnBounds := CaretPosition.LayoutPosition.Column.Bounds;
  if AOptimalLogicalVisibleBounds.Left < AColumnBounds.Left then
    AOptimalLogicalVisibleBounds.MoveToLeft(AColumnBounds.Left);
  APageBounds := CaretPosition.LayoutPosition.Page.Bounds;
  if AOptimalLogicalVisibleBounds.Right > APageBounds.Right then
    AOptimalLogicalVisibleBounds.MoveToRight(APageBounds.Right);
  Result := AOptimalLogicalVisibleBounds.Left;
end;

procedure TdxEnsureCaretVisibleHorizontallyCommand.ExecuteCore;
var
  ALogicalVisibleBounds: TRect;
  ACaretBounds: TRect;
begin
  CheckExecutedAtUIThread;
  UpdateCaretPosition(TdxDocumentLayoutDetailsLevel.Character);
  Assert(CaretPosition.PageViewInfo <> nil);
  if CaretPosition.PageViewInfo = nil then
    Exit;
  ALogicalVisibleBounds := ActiveView.CreateLogicalRectangle(CaretPosition.PageViewInfo, ActiveView.Bounds);
  if CaretPosition.LayoutPosition.DetailsLevel >= TdxDocumentLayoutDetailsLevel.Character then
  begin
    ACaretBounds := CaretPosition.CalculateCaretBounds;
    if not IsCaretVisible(ALogicalVisibleBounds, ACaretBounds) then
      ScrollToMakeCaretVisible(ALogicalVisibleBounds, ACaretBounds);
  end;
end;

class function TdxEnsureCaretVisibleHorizontallyCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEnsureCaretVisibleHorizontallyDescription);
end;

class function TdxEnsureCaretVisibleHorizontallyCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEnsureCaretVisibleHorizontallyMenuCaption);
end;

function TdxEnsureCaretVisibleHorizontallyCommand.IsCaretVisible(
  const ALogicalVisibleBounds, ACaretBounds: TRect): Boolean;
begin
  Assert(CaretPosition.PageViewInfo <> nil);
  Result := ALogicalVisibleBounds.Contains(ACaretBounds);
end;

procedure TdxEnsureCaretVisibleHorizontallyCommand.ScrollToMakeCaretVisible(
  const ALogicalVisibleBounds, ACaretBounds: TRect);
var
  AOptimalHorizontalPosition: Integer;
  AScrollBarAdapter: TdxScrollBarAdapter;
begin
  AOptimalHorizontalPosition := CalculateOptimalCaretHorizontalPosition(ALogicalVisibleBounds, ACaretBounds);
  AScrollBarAdapter := ActiveView.HorizontalScrollController.ScrollBarAdapter;
  if AScrollBarAdapter.Value <> AOptimalHorizontalPosition then
  begin
    AScrollBarAdapter.Value := AOptimalHorizontalPosition;
    AScrollBarAdapter.ApplyValuesToScrollBar;
    ActiveView.PageViewInfoGenerator.LeftInvisibleWidth := ActiveView.HorizontalScrollController.GetLeftInvisibleWidth;
    ActiveView.OnHorizontalScroll;
  end;
end;

procedure TdxEnsureCaretVisibleHorizontallyCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := True;
  AState.Visible := True;
  AState.Checked := False;
end;

{ TdxCurrentRowInfo }

constructor TdxCurrentRowInfo.Create(ARow: TdxRow; const ABoundsRelativeToPage: TRect);
begin
  Row := ARow;
  BoundsRelativeToPage := ABoundsRelativeToPage;
end;

class function TdxCurrentRowInfo.Empty: TdxCurrentRowInfo;
begin
  Result := TdxCurrentRowInfo.Create(nil, TRect.Null);
end;

{ TdxEnsureCaretVisibleVerticallyCommand }

constructor TdxEnsureCaretVisibleVerticallyCommand.Create(
  const ARichEditControl: IdxRichEditControl);
begin
  inherited Create(ARichEditControl);
  FRelativeCaretPosition := -1;
end;

destructor TdxEnsureCaretVisibleVerticallyCommand.Destroy;
begin
  CurrentRow := TdxCurrentRowInfo.Empty;
  CurrentPageViewInfo := nil;
  CurrentPage := nil;
  inherited Destroy;
end;

procedure TdxEnsureCaretVisibleVerticallyCommand.AdjustAndSaveCaretPosition;
var
  APos: TdxCaretPosition;
begin
  if ShouldAdjustCaretPosition then
  begin
    APos := CreateAdjustedCaretPosition;
    SaveCaretPosition(APos);
  end
  else
    SaveCaretPosition(CaretPosition);
end;

function TdxEnsureCaretVisibleVerticallyCommand.BothPositionsWillValid(
  ACurrentCaretPosition, AAdjustedCaretPosition: TdxCaretPosition): Boolean;
var
  APhysicalCaretRowBounds, APhysicalAdjustedCaretRowBounds, AViewBounds: TRect;
begin
  if AAdjustedCaretPosition.PageViewInfo = nil then
    Result := False
  else
  begin
    APhysicalCaretRowBounds := ActiveView.CreatePhysicalRectangle(ACurrentCaretPosition.PageViewInfo, ACurrentCaretPosition.LayoutPosition.Row.Bounds);
    APhysicalAdjustedCaretRowBounds := ActiveView.CreatePhysicalRectangle(AAdjustedCaretPosition.PageViewInfo, AAdjustedCaretPosition.LayoutPosition.Row.Bounds);
    AViewBounds := ActiveView.Bounds;
    Assert(APhysicalAdjustedCaretRowBounds.Top < AViewBounds.Top);
    Result := APhysicalCaretRowBounds.Top + AViewBounds.Top - APhysicalAdjustedCaretRowBounds.Top <= AViewBounds.Bottom;
  end;
end;

function TdxEnsureCaretVisibleVerticallyCommand.CalculateActualInvisiblePageBounds(
  APageViewInfo: TdxPageViewInfo;
  ADirection: TdxSelectionMovementVerticalDirection): TRect;
var
  APageOffset: Integer;
begin
  if ADirection = TdxSelectionMovementVerticalDirection.Down then
    APageOffset := ActiveView.PageViewInfoGenerator.ActiveGenerator.PageRows.Last.Bounds.Bottom
  else
    APageOffset := ActiveView.PageViewInfoGenerator.ActiveGenerator.PageRows.First.Bounds.Top -
      2 * APageViewInfo.Bounds.Bottom + APageViewInfo.Bounds.Height;
  Result := ActiveView.PageViewInfoGenerator.OffsetRectangle(APageViewInfo.Bounds, 0, APageOffset);
end;

function TdxEnsureCaretVisibleVerticallyCommand.CalculateDirectionToInvisibleCaret: TdxSelectionMovementVerticalDirection;
var
  APages: TdxPageCollection;
  ACaretPageIndex: Integer;
  ACalculator: TdxNextCaretPositionLineUpCalculator;
  AFirstVisiblePageIndex: Integer;
begin
  APages := ActiveView.FormattingController.PageController.Pages;
  ACaretPageIndex := APages.IndexOf(CurrentPage);
  ACalculator := TdxNextCaretPositionLineUpCalculator.Create(RichEditControl);
  try
    ACalculator.FixedDirection := False;
    AFirstVisiblePageIndex := ACalculator.CalculateFirstInvisiblePageIndex + 1;
    Assert(ACaretPageIndex >= 0);
    Assert(AFirstVisiblePageIndex >= 0);
    Assert(ACaretPageIndex <> AFirstVisiblePageIndex);
    if ACaretPageIndex < AFirstVisiblePageIndex then
      Result := TdxSelectionMovementVerticalDirection.Up
    else
      Result := TdxSelectionMovementVerticalDirection.Down;
  finally
    ACalculator.Free;
  end;
end;

function TdxEnsureCaretVisibleVerticallyCommand.CalculateOffsetToVisibleArea(
  AInitialCaretPhysicalTop: Integer): Integer;
var
  AViewBounds, APhysicalCaretRowBounds: TRect;
  ATargetY: Integer;
begin
  if RelativeCaretPosition < 0 then
    Result := 0
  else
  begin
    AViewBounds := ActiveView.Bounds;
    APhysicalCaretRowBounds := ActiveView.CreatePhysicalRectangle(TRect.CreateSize(0, 0, 100, 100), CurrentRow.BoundsRelativeToPage);
    AViewBounds.Height := AViewBounds.Height - APhysicalCaretRowBounds.Height;
    if AViewBounds.Height <= 0 then
      Result := 0
    else
    begin
      ATargetY := Round(AViewBounds.Height * RelativeCaretPosition);
			Result := AInitialCaretPhysicalTop - ATargetY;
    end;
  end;
end;

function TdxEnsureCaretVisibleVerticallyCommand.CalculatePhysicalOffsetForCaretPositionRowVisibilityCore(
  APageViewInfo: TdxPageViewInfo; const ARow: TdxCurrentRowInfo): Integer;
var
  ALogicalRowBounds, APhysicalRowBounds: TRect;
begin
  ALogicalRowBounds := CalculateTargetRowLogicalBounds(APageViewInfo, ARow);
  APhysicalRowBounds := ActiveView.CreatePhysicalRectangle(APageViewInfo, ALogicalRowBounds);
  Result := CalculatePhysicalOffsetForRowBoundsVisibility(ActiveView.Bounds, APhysicalRowBounds);
end;

function TdxEnsureCaretVisibleVerticallyCommand.CalculatePhysicalOffsetForRowBoundsVisibility(
  const AViewBounds, APhysicalRowBounds: TRect): Integer;
begin
  if (AViewBounds.Top <= APhysicalRowBounds.Top) and (APhysicalRowBounds.Bottom <= AViewBounds.Bottom) then
    Result := 0
  else
  begin
    if APhysicalRowBounds.Top < AViewBounds.Top then
      Result := APhysicalRowBounds.Top - AViewBounds.Top
    else
      Result := APhysicalRowBounds.Bottom - AViewBounds.Bottom;
  end;
end;

function TdxEnsureCaretVisibleVerticallyCommand.CalculatePhysicalOffsetToCaret: Integer;
begin
  if CurrentPageViewInfo <> nil then
    Result := CalculatePhysicalOffsetToCaretAtVisiblePage
  else
    Result := CalculatePhysicalOffsetToCaretAtInvisiblePage;
end;

function TdxEnsureCaretVisibleVerticallyCommand.CalculatePhysicalOffsetToCaretAtInvisiblePage: Integer;
var
  APageViewInfo: TdxPageViewInfo;
  ADirection: TdxSelectionMovementVerticalDirection;
begin
  ADirection := CalculateDirectionToInvisibleCaret;
  APageViewInfo := LookupInvisiblePageWithCaret(ADirection);
  APageViewInfo.Bounds := CalculateActualInvisiblePageBounds(APageViewInfo, ADirection);
  ActiveView.PageViewInfoGenerator.UpdatePageClientBounds(APageViewInfo);
  Result := CalculatePhysicalOffsetForCaretPositionRowVisibilityCore(APageViewInfo, CurrentRow);
end;

function TdxEnsureCaretVisibleVerticallyCommand.CalculatePhysicalOffsetToCaretAtVisiblePage: Integer;
begin
  Result := CalculatePhysicalOffsetForCaretPositionRowVisibilityCore(CurrentPageViewInfo, CurrentRow);
end;

function TdxEnsureCaretVisibleVerticallyCommand.CalculateScrollDirectionToCaret(
  APos: TdxCaretPosition): TdxCaretScrollDirection;
var
  APhysicalCaretRowBounds, AViewBounds: TRect;

begin
  if APos.PageViewInfo = nil then
    Result := TdxCaretScrollDirection.Unknown
  else
  begin
    APhysicalCaretRowBounds := ActiveView.CreatePhysicalRectangle(APos.PageViewInfo,
      APos.LayoutPosition.Row.Bounds);
    AViewBounds := ActiveView.Bounds;
    if APhysicalCaretRowBounds.Top < AViewBounds.Top then
      Result := TdxCaretScrollDirection.Up
    else
			if APhysicalCaretRowBounds.Bottom > AViewBounds.Bottom then
        Result := TdxCaretScrollDirection.Down
      else
        Result := TdxCaretScrollDirection.None;
  end;
end;

function TdxEnsureCaretVisibleVerticallyCommand.CalculateTargetRowLogicalBounds(
  APageViewInfo: TdxPageViewInfo; const ARow: TdxCurrentRowInfo): TRect;
begin
  Result := ValidateRowBounds(APageViewInfo, ARow.BoundsRelativeToPage);
end;

function TdxEnsureCaretVisibleVerticallyCommand.CreateAdjustedCaretPosition: TdxCaretPosition;
var
  ALogPosition: TdxDocumentLogPosition;
begin
  ALogPosition := ActivePieceTable.NavigationVisibleTextFilter.GetPrevVisibleLogPosition(CaretPosition.LogPosition, False);
  Result := CaretPosition.CreateExplicitCaretPosition(ALogPosition);
  Result.Update(TdxDocumentLayoutDetailsLevel.Row);
end;

procedure TdxEnsureCaretVisibleVerticallyCommand.ExecuteCore;
var
  ARedraw: Boolean;
begin
  CheckExecutedAtUIThread;
  RichEditControl.UpdateControlAutoSize;
  UpdateCaretPosition(TdxDocumentLayoutDetailsLevel.Row);
  if CaretPosition.LayoutPosition.DetailsLevel >= TdxDocumentLayoutDetailsLevel.Row then
  begin
    ARedraw := False;
    InnerControl.BeginDocumentRendering;
    try
      if CaretPosition.PageViewInfo = nil then
        UpdateCaretPosition(TdxDocumentLayoutDetailsLevel.Row);

      if not IsCaretVisible then
      begin
        ScrollToMakeCaretVisible;
        ARedraw := False;
      end
      else
      begin
        if RelativeCaretPosition >= 0 then
        begin
          ScrollToSetRelativeCaretPosition;
          ARedraw := True;
        end;
      end;
    finally
      InnerControl.EndDocumentRendering;
    end;
    UpdateCaretPosition(TdxDocumentLayoutDetailsLevel.Row);
    Assert(CaretPosition.LayoutPosition.IsValid(TdxDocumentLayoutDetailsLevel.Row));
    Assert(CaretPosition.PageViewInfo <> nil);
    if ARedraw then
      (RichEditControl.Control as IdxInnerRichEditControlOwner).Redraw;
//      InnerControl.Owner.Redraw;
  end;
  Assert(CaretPosition.PageViewInfo <> nil);
end;

class function TdxEnsureCaretVisibleVerticallyCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEnsureCaretVisibleVerticallyDescription);
end;

class function TdxEnsureCaretVisibleVerticallyCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEnsureCaretVisibleVerticallyMenuCaption);
end;

function TdxEnsureCaretVisibleVerticallyCommand.IsCaretVisible: Boolean;
var
  ACaretScrollDirection: TdxCaretScrollDirection;
  APos: TdxCaretPosition;
begin
  ACaretScrollDirection := CalculateScrollDirectionToCaret(CaretPosition);
  if (ACaretScrollDirection = TdxCaretScrollDirection.Down) or (ACaretScrollDirection = TdxCaretScrollDirection.Unknown) then
  begin
    SaveCaretPosition(CaretPosition);
    Exit(False);
  end;
  if ACaretScrollDirection = TdxCaretScrollDirection.Up then
  begin
    AdjustAndSaveCaretPosition;
    Exit(False);
  end;
  Assert(ACaretScrollDirection = TdxCaretScrollDirection.None);
  Assert(CaretPosition.PageViewInfo <> nil);
  if not ShouldAdjustCaretPosition then
  begin
    SaveCaretPosition(CaretPosition);
    Exit(True);
  end;
  APos := CreateAdjustedCaretPosition;
  try
    ACaretScrollDirection := CalculateScrollDirectionToCaret(APos);
    if ACaretScrollDirection = TdxCaretScrollDirection.None then
    begin
      SaveCaretPosition(CaretPosition);
      Exit(True);
    end
    else
    begin
      if (ACaretScrollDirection = TdxCaretScrollDirection.Up) or (ACaretScrollDirection = TdxCaretScrollDirection.Down) then
      begin
        if BothPositionsWillValid(CaretPosition, APos) then
          SaveCaretPosition(APos)
        else
          SaveCaretPosition(CaretPosition);
      end
      else
        SaveCaretPosition(CaretPosition);
        Result := False;
    end;
  finally
    APos.Free;
  end;
end;

function TdxEnsureCaretVisibleVerticallyCommand.LookupInvisiblePageWithCaret(
  ADirection: TdxSelectionMovementVerticalDirection): TdxPageViewInfo;
var
  ACalculator: TdxNextCaretPositionVerticalDirectionCalculator;
  AGenerator: TdxInvisiblePageRowsGenerator;
  ARow: TdxPageViewInfoRow;
  I: Integer;
  APageViewInfo: TdxPageViewInfo;
begin
  if ADirection = TdxSelectionMovementVerticalDirection.Up then
    ACalculator := TdxNextCaretPositionLineUpCalculator.Create(RichEditControl)
  else
    ACalculator := TdxNextCaretPositionLineDownCalculator.Create(RichEditControl);
  try
    ACalculator.FixedDirection := False;
    AGenerator := ACalculator.CreateInvisiblePageRowsGenerator;
    try
      while True do
      begin
        ARow := AGenerator.GenerateNextRow;
        Assert(ARow <> nil);
        for I := 0 to ARow.Count - 1 do
        begin
          APageViewInfo := ARow[I];
          if APageViewInfo.Page = CurrentPage then
            Exit(APageViewInfo);
        end;
      end;
    finally
      AGenerator.Free;
    end;
  finally
    ACalculator.Free;
  end;
end;

procedure TdxEnsureCaretVisibleVerticallyCommand.SaveCaretPosition(APos: TdxCaretPosition);
begin
  CurrentPage := APos.LayoutPosition.Page;
  CurrentRow := TdxCurrentRowInfo.Create(APos.LayoutPosition.Row, APos.GetRowBoundsRelativeToPage);
  CurrentPageViewInfo := APos.PageViewInfo;
end;

procedure TdxEnsureCaretVisibleVerticallyCommand.ScrollToAlreadyFormattedContentCore(
  AVerticalOffset: Integer);
var
  APhysicalCaretRowBounds: TRect;
  APageIndex, AOffset: Integer;
  APageViewInfoGenerator: TdxPageViewInfoGenerator;
begin
  ActiveView.ResetPages(TdxPageGenerationStrategyType.FirstPageOffset);
  if CurrentPageViewInfo <> nil then
  begin
    APhysicalCaretRowBounds := ActiveView.CreatePhysicalRectangle(CurrentPageViewInfo, TRect.CreateSize(0, AVerticalOffset, 100, 100));

    APageIndex := ActiveView.FormattingController.PageController.Pages.IndexOf(CurrentPage);
    Assert(APageIndex >= 0);
    APageViewInfoGenerator := ActiveView.PageViewInfoGenerator;
    APageViewInfoGenerator.FirstPageOffsetAnchor.PageIndex := APageIndex;
    AOffset := CalculateOffsetToVisibleArea(APhysicalCaretRowBounds.Y);
    APageViewInfoGenerator.FirstPageOffsetAnchor.VerticalOffset := Math.Max(0, APageViewInfoGenerator.FirstPageOffsetAnchor.VerticalOffset + AOffset);
  end;
  ActiveView.GeneratePages;
end;

procedure TdxEnsureCaretVisibleVerticallyCommand.ScrollToMakeCaretVisible;
var
  APageViewInfoGenerator: TdxPageViewInfoGenerator;
  AOffset: Integer;
begin
  ActiveView.UpdateVerticalScrollBar;
  APageViewInfoGenerator := ActiveView.PageViewInfoGenerator;
  if APageViewInfoGenerator.TopInvisibleHeight >= APageViewInfoGenerator.TotalHeight then
    ScrollToAlreadyFormattedContentCore(CurrentRow.BoundsRelativeToPage.Top)
  else
  begin
    AOffset := CalculatePhysicalOffsetToCaret;
    if AOffset < 0 then
      Inc(AOffset, CalculateOffsetToVisibleArea(0))
    else
      Inc(AOffset, CalculateOffsetToVisibleArea(ActiveView.Bounds.Height -
        ActiveView.CreatePhysicalRectangle(TRect.CreateSize(0, 0, 100, 100), CurrentRow.BoundsRelativeToPage).Height));

    ScrollVerticallyByPhysicalOffset(AOffset);
  end;
end;

procedure TdxEnsureCaretVisibleVerticallyCommand.ScrollToSetRelativeCaretPosition;
var
  APhysicalCaretRowBounds: TRect;
  AOffset: Integer;
begin
  if CurrentPageViewInfo = nil then
    Exit;
  APhysicalCaretRowBounds := ActiveView.CreatePhysicalRectangle(CurrentPageViewInfo, CurrentRow.BoundsRelativeToPage);
  AOffset := CalculateOffsetToVisibleArea(APhysicalCaretRowBounds.Top);
  if AOffset = 0 then
    Exit;
  ActiveView.UpdateVerticalScrollBar;
  ScrollVerticallyByPhysicalOffset(AOffset);
end;

procedure TdxEnsureCaretVisibleVerticallyCommand.ScrollVerticallyByPhysicalOffset(
  AOffset: Integer);
var
  ACommand: TdxScrollVerticallyByPhysicalOffsetEnsurePageGenerationCommand;
begin
  if AOffset <> 0 then
  begin
    ACommand := TdxScrollVerticallyByPhysicalOffsetEnsurePageGenerationCommand.Create(RichEditControl);
    try
      ACommand.PhysicalOffset := AOffset;
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  end;
end;

procedure TdxEnsureCaretVisibleVerticallyCommand.SetCurrentPage(const Value: TdxPage);
begin
  if FCurrentPage <> Value then
  begin
    TdxPage.Release(FCurrentPage);
    FCurrentPage := Value;
    TdxPage.AddReference(FCurrentPage);
  end;
end;

procedure TdxEnsureCaretVisibleVerticallyCommand.SetCurrentPageViewInfo(const Value: TdxPageViewInfo);
begin
  if FCurrentPageViewInfo <> Value then
  begin
    TdxPageViewInfo.Release(FCurrentPageViewInfo);
    FCurrentPageViewInfo := Value;
    TdxPageViewInfo.AddReference(FCurrentPageViewInfo);
  end;
end;

procedure TdxEnsureCaretVisibleVerticallyCommand.SetCurrentRow(const Value: TdxCurrentRowInfo);
begin
  if FCurrentRow.Row <> Value.Row then
  begin
    TdxRow.Release(FCurrentRow.Row);
    TdxRow.AddReference(Value.Row);
  end;
  FCurrentRow := Value;
end;

procedure TdxEnsureCaretVisibleVerticallyCommand.SetRelativeCaretPosition(Value: Single);
begin
  if FRelativeCaretPosition <> Value then
  begin
    if Value >= 0 then
      Value := Min(1, Value);
    FRelativeCaretPosition := Value;
  end;
end;

function TdxEnsureCaretVisibleVerticallyCommand.ShouldAdjustCaretPosition: Boolean;
begin
  if DocumentModel.Selection.&End <= DocumentModel.Selection.Start then
    Result := False
  else
    Result := CaretPosition.LogPosition = CaretPosition.LayoutPosition.Row.GetFirstPosition(ActivePieceTable).LogPosition;
end;

procedure TdxEnsureCaretVisibleVerticallyCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := True;
  AState.Visible := True;
  AState.Checked := False;
end;

function TdxEnsureCaretVisibleVerticallyCommand.ValidateRowBounds(
  APageViewInfo: TdxPageViewInfo; const ARowBounds: TRect): TRect;
var
  APageBottom: Integer;
begin
  Result := ARowBounds;
  APageBottom := APageViewInfo.Page.Bounds.Bottom;
  if ARowBounds.Bottom > APageBottom then
    Result.Height := APageBottom - ARowBounds.Top;
end;

{ TdxNextCaretPositionVerticalDirectionCalculator }

procedure TdxNextCaretPositionVerticalDirectionCalculator.CorrectCaretXOnExtendSelection(
  ASourceCellViewInfo: TdxTableCellViewInfo; var ACaretX: Integer);
begin
  if ASourceCellViewInfo = nil then
    Exit;
  ACaretX := Max(ACaretX, ASourceCellViewInfo.TextLeft);
  ACaretX := Min(ACaretX, ASourceCellViewInfo.TextLeft + ASourceCellViewInfo.TextWidth);
end;

procedure TdxNextCaretPositionVerticalDirectionCalculator.CorrectRowAndCellViewInfo(
  APos: TdxCaretPosition; var ARowIndexInColumn: Integer;
  var ASourceCellViewInfo: TdxTableCellViewInfo);
var
  ARows: TdxRowCollection;
  ADocumentModel: TdxDocumentModel;
  ASelection: TdxSelection;
  ACellFromPreviousLayoutRow: TdxTableCellViewInfo;
  ATableRow: TdxTableRowViewInfoBase;
  ACellsInDifferentTables: Boolean;
  ANewRowIndex: Integer;
begin
  ADocumentModel := APos.DocumentModel;
  ASelection := ADocumentModel.Selection;
  if IsCaretAtTheBeginningOfAnotherCell(APos, ADocumentModel) then
  begin
    if ASourceCellViewInfo = nil then
      Exit;
    ARowIndexInColumn := Math.Max(0, ARowIndexInColumn - 1);
    ACellFromPreviousLayoutRow := APos.LayoutPosition.Column.Rows[ARowIndexInColumn].GetCellViewInfo;
    ATableRow := ASourceCellViewInfo.GetTableRow;
    ACellsInDifferentTables := (ACellFromPreviousLayoutRow <> nil) and
      not (ASourceCellViewInfo.TableViewInfo.Table = ACellFromPreviousLayoutRow.TableViewInfo.Table);
    if ACellsInDifferentTables and ASelection.FirstAndSecondCellHaveCommonTableButSecondCellNotParentForFirstCell(
      ASourceCellViewInfo.Cell, ACellFromPreviousLayoutRow.Cell) then
    begin
      ATableRow := ACellFromPreviousLayoutRow.GetTableRow;
      ASourceCellViewInfo := GetLastNonCoveredByVerticalMergingCell(ATableRow);
    end
    else
      if IsCellFromPreviousRowIsInNestedTableInSourceCellViewInfo(ASourceCellViewInfo, ACellFromPreviousLayoutRow) then
      begin
        ASourceCellViewInfo := ACellFromPreviousLayoutRow;
      end
      else
        if ASourceCellViewInfo.Cell.IsFirstCellInRow and (ATableRow.Previous <> nil) then
        begin
          ATableRow := ATableRow.Previous;
          ASourceCellViewInfo := GetLastNonCoveredByVerticalMergingCell(ATableRow);
        end
        else
          if ACellFromPreviousLayoutRow = nil then
            ASourceCellViewInfo := nil
          else
            ASourceCellViewInfo := GetPreviousNonVerticalMergedCellInRow(ASourceCellViewInfo);
    if ASourceCellViewInfo <> nil then
    begin
      ARows := ASourceCellViewInfo.GetRows(APos.LayoutPosition.Column);
      try
        ANewRowIndex := APos.LayoutPosition.Column.Rows.IndexOf(ARows.Last);
      finally
        ARows.Free;
      end;
      ARowIndexInColumn := ANewRowIndex;
    end;
  end;
end;

constructor TdxNextCaretPositionVerticalDirectionCalculator.Create(const AControl: IdxRichEditControl);
begin
  inherited Create;
  FControl := AControl;
  FFixedDirection := True;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.CalculateNextPosition(
  ACaretPosition: TdxCaretPosition): TdxDocumentModelPosition;
var
  X: Integer;
  ATargetRow: TdxRow;
  ACharacter: TdxCharacterBox;
  ACenter: Integer;
begin
  View.CheckExecutedAtUIThread;
  if ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.Character) then
    X := ACaretPosition.LayoutPosition.Character.ActualSizeBounds.X
  else
    X := ACaretPosition.X;
  Assert(ACaretPosition.PageViewInfo <> nil);
  ATargetRow := GetTargetLayoutRowObtainTargetPageViewInfo(ACaretPosition, X);
  if ATargetRow = nil then
    Exit(GetDefaultPosition);
  if ActivePieceTable <> ATargetRow.Paragraph.PieceTable then
    Exit(TdxDocumentModelPosition.Null);
  ACharacter := GetTargetCharacter(ATargetRow, X);
  try
    Result := ACharacter.GetFirstPosition(ActivePieceTable);
    if (Result.LogPosition < ATargetRow.GetLastPosition(ActivePieceTable).LogPosition) and
      (ACharacter.Bounds.Right > X) and (ACharacter.Bounds.Left <= X) then
    begin
      ACenter := (ACharacter.Bounds.Right + ACharacter.Bounds.Left) div 2;
      if ACenter < X then
        Result.LogPosition := Result.LogPosition + 1;
    end;
  finally
    ACharacter.Free;
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.CreateInvisiblePageRowsGenerator: TdxInvisiblePageRowsGenerator;
var
  ABounds: TRect;
begin
  Result := TdxInvisiblePageRowsGenerator.Create(View.FormattingController.PageController.Pages, View.PageViewInfoGenerator);
  Result.FirstPageIndex := CalculateFirstInvisiblePageIndex;
  if (Result.FirstPageIndex < 0) and not FixedDirection then
  begin
    Result.FirstPageIndex := 0;
    Result.FirstInvalidPageIndex := View.FormattingController.PageController.Pages.Count;
  end
  else
    Result.FirstInvalidPageIndex := CalculateFirstInvalidPageIndex;
  ABounds := Result.LayoutManager.ViewPortBounds;
  ABounds.MoveToTop(0);
  Result.LayoutManager.ViewPortBounds := ABounds;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.FindMostNestedTableCell(
  ACell: TdxTableCellViewInfo; AColumn: TdxColumn;
  ACaretX: Integer): TdxTableCellViewInfo;
var
  ARowsFromNextCell: TdxRowCollection;
  ARowIndexInNextCell: Integer;
  ANextLayoutRow: TdxRow;
begin
  Result := ACell;
  while (Result.InnerTables.Count > 0) and (Result.InnerTables[0].FirstContentInParentCell) do
  begin
    ARowsFromNextCell := Result.GetRows(AColumn);
    try
      ARowIndexInNextCell := AColumn.Rows.IndexOf(ARowsFromNextCell.First);
    finally
      ARowsFromNextCell.Free;
    end;
    ANextLayoutRow := AColumn.Rows[ARowIndexInNextCell];
    Result := GetTargetCellViewInfoInNestedTable(ANextLayoutRow, ACaretX,
      Result.TableViewInfo.Table.NestedLevel);
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.FindMostParentTableRowViewInfo(
  ATargetTable: TdxTable;
  AFirstColumnRowCell: TdxTableCellViewInfo): TdxTableRowViewInfoBase;
var
  AParentCellViewInfo: TdxTableCellViewInfo;
begin
  Result := nil;
  if AFirstColumnRowCell = nil then
    Exit;

  Result := AFirstColumnRowCell.GetTableRow;
  AParentCellViewInfo := AFirstColumnRowCell.TableViewInfo.ParentTableCellViewInfo;
  while AParentCellViewInfo <> nil do
  begin
    if (ATargetTable = AParentCellViewInfo.TableViewInfo.Table) or
      (AParentCellViewInfo.TableViewInfo.Table.NestedLevel = 0) then
    begin
      Result := AParentCellViewInfo.GetTableRow;
      break;
    end;
    AParentCellViewInfo := AParentCellViewInfo.TableViewInfo.ParentTableCellViewInfo;
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.FindTableCellViewInfoInTableRowByX(
  ATableRow: TdxTableRowViewInfoBase; X: Integer): TdxTableCellViewInfo;
var
  ACells: TdxTableCellViewInfoCollection;
  APredicate: TdxTableCellAnchorComparable;
  AIndex: Integer;
begin
  ACells := ATableRow.Cells;
  APredicate := TdxTableCellAnchorComparable.Create(X);
  try
    if TdxAlgorithms1<TdxTableCellViewInfo>.BinarySearch(ACells, APredicate, AIndex) then
      Exit(ACells[AIndex]);
    if AIndex = ACells.Count then
    begin
      if APredicate.CompareTo(ACells.First) > 0 then
        Result := ACells.First
      else
        Result := ACells.Last;
    end
    else
      Result := ACells[AIndex];
  finally
    APredicate.Free;
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GenerateTargetPageViewInfoRow: TdxPageViewInfoRow;
var
  AGenerator: TdxInvisiblePageRowsGenerator;
begin
  AGenerator := CreateInvisiblePageRowsGenerator;
  try
    Result := AGenerator.GenerateNextRow;
    if Result <> nil then
      CorrectGeneratedRowPagesHorizontalLocation(AGenerator.LayoutManager, Result);
  finally
    AGenerator.Free;
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetActivePieceTable: TdxPieceTable;
begin
  Result := Control.InnerControl.DocumentModel.ActivePieceTable;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetDefaultPosition: TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.Null;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetLastNonCoveredByVerticalMergingCell(
  ARow: TdxTableRowViewInfoBase): TdxTableCellViewInfo;
var
  ARowCellsCount: Integer;
  I: Integer;
begin
  ARowCellsCount := ARow.Cells.Count;
  Result := ARow.Cells.Last;
  for I := ARowCellsCount - 1 downto 0 do
  begin
    if Result.Cell.VerticalMerging <> TdxMergingState.Restart then
      Break;
    Result := ARow.Cells[I];
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetLastTableRowInDirection(
  ACell: TdxTableCellViewInfo; X: Integer): TdxTableRowViewInfoBase;
var
  ATableRow: TdxTableRowViewInfoBase;
  ANextTableRow: TdxTableRowViewInfoBase;
  ARowSpan: Integer;
  I: Integer;
begin
  Result := nil;
  ATableRow := ACell.GetTableRow;
  ARowSpan := ACell.RowSpan;
  I := 0;
  while (I < ARowSpan) and (ATableRow <> nil) do
  begin
    ANextTableRow := GetTargetTableRowCore(ATableRow);
    if ANextTableRow = nil then
      Exit(ATableRow);
    ATableRow := ANextTableRow;
    Inc(I);
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetNextCellViewInfoInTargetRowWithNonZeroRows(
  AColumn: TdxColumn; ATargetRow: TdxTableRowViewInfoBase;
  ACellWithZeroRows: TdxTableCellViewInfo): TdxTableCellViewInfo;
var
  ACellsCount, I: Integer;
  ARows: TdxRowCollection;
begin
  Result := ACellWithZeroRows;
  ACellsCount := ATargetRow.Cells.Count;
  I := ATargetRow.Cells.IndexOf(ACellWithZeroRows);
  ARows := Result.GetRows(AColumn);
  try
    while (ARows.Count = 0) and (I < ACellsCount) do
    begin
      Result := ATargetRow.Cells[I];
      ARows.Free;
      ARows := Result.GetRows(AColumn);
      Inc(I);
    end;
  finally
    ARows.Free;
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetNextLayoutRowByDirection(
  ARows: TdxRowCollection; ASourceRowIndex: Integer): TdxRow;
var
  ANewIndex: Integer;
begin
  ANewIndex := GetNextLayoutRowIndexInDirection(ASourceRowIndex);
  Assert(ANewIndex < ARows.Count);
  Result := ARows[ANewIndex];
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetPageViewInfoRowIndex(
  APageRows: TdxPageViewInfoRowCollection;
  APageViewInfo: TdxPageViewInfo): Integer;
begin
  for Result := 0 to APageRows.Count - 1 do
  begin
    if APageRows[Result].IndexOf(APageViewInfo) >= 0 then
      Exit;
  end;
  Result := -1;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetPreviousNonVerticalMergedCellInRow(
  ASourceCellViewInfo: TdxTableCellViewInfo): TdxTableCellViewInfo;
var
  ARow: TdxTableRowViewInfoBase;
  ASourceCellViewInfoIndexInRow: Integer;
  I: Integer;
begin
  ARow := ASourceCellViewInfo.GetTableRow;
  ASourceCellViewInfoIndexInRow := ARow.Cells.IndexOf(ASourceCellViewInfo);
  Result := ARow.Cells[Max(0, ASourceCellViewInfoIndexInRow - 1)];
  for I := ASourceCellViewInfoIndexInRow - 1 downto 0 do
  begin
    if Result.Cell.VerticalMerging = TdxMergingState.None then
      Break;
    Result := ARow.Cells[I];
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetTargetCellViewInfoFromNextTableRow(
  ACell: TdxTableCellViewInfo; X: Integer): TdxTableCellViewInfo;
var
  ARow, ANextRow: TdxTableRowViewInfoBase;
  ANextCell: TdxTableCellViewInfo;
begin
  ARow := ACell.GetTableRow;
  Result := nil;
  ANextRow := GetTargetTableRowCore(ARow);
  while (Result = nil) and (ANextRow <> nil) do
  begin
    ANextCell := FindTableCellViewInfoInTableRowByX(ANextRow, X);
    if ANextCell <> ACell then
    begin
      Result := ANextCell;
      Break;
    end;
    ANextRow := GetTargetTableRowCore(ANextRow);
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetTargetCharacter(
  ARow: TdxRow; ACaretX: Integer): TdxCharacterBox;
var
  AHitTestRequest: TdxRichEditHitTestRequest;
  AHitTestResult: TdxRichEditHitTestResult;
  AHitTestCalculator: TdxBoxHitTestCalculator;
  ABoxIndex: Integer;
  AComparable: TdxBoxAndPointXComparable;
  ADetailRow: TdxDetailRow;
begin
  AHitTestRequest := TdxRichEditHitTestRequest.Create(ActivePieceTable);
  AHitTestRequest.LogicalPoint := Point(ACaretX, ARow.Bounds.Top + ARow.BaseLineOffset);

  AHitTestResult := TdxRichEditHitTestResult.Create(View.DocumentLayout, ActivePieceTable);
  try
    AHitTestCalculator := View.CreateHitTestCalculator(AHitTestRequest, AHitTestResult);
    try
      AComparable := TdxBoxAndPointXComparable.Create(AHitTestRequest.LogicalPoint);
      try
        ABoxIndex := AHitTestCalculator.FastHitTestIndexCore(ARow.Boxes, AComparable, False);
      finally
        AComparable.Free;
      end;
      Assert(ABoxIndex >= 0);
      ADetailRow := View.DocumentLayout.CreateDetailRowForBox(ARow, ARow.Boxes[ABoxIndex], False);
      try
        AHitTestCalculator.FastHitTestCharacter(ADetailRow.Characters, False);
        Assert(AHitTestResult.IsValid(TdxDocumentLayoutDetailsLevel.Character));
        Result := AHitTestResult.Character.Clone;
      finally
        ADetailRow.Free;
      end;
    finally
      AHitTestCalculator.Free;
    end;
  finally
    AHitTestResult.Free;
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetTargetColumn(
  APage: TdxPage; ACaretX: Integer): TdxColumn;
var
  AHitTestRequest: TdxRichEditHitTestRequest;
  AHitTestResult: TdxRichEditHitTestResult;
  APageArea: TdxPageArea;
  ACalculator: TdxBoxHitTestCalculator;
begin
  AHitTestRequest := TdxRichEditHitTestRequest.Create(ActivePieceTable);
  AHitTestRequest.LogicalPoint := Point(ACaretX, 0);
  AHitTestRequest.DetailsLevel := TdxDocumentLayoutDetailsLevel.Column;
  AHitTestRequest.Accuracy := NearestColumn;

  APageArea := GetTargetPageArea(APage);
  AHitTestResult := TdxRichEditHitTestResult.Create(View.DocumentLayout, ActivePieceTable);
  try
    ACalculator := View.CreateHitTestCalculator(AHitTestRequest, AHitTestResult);
    try
      ACalculator.FastHitTestAssumingArrangedHorizontally(APageArea.Columns, False);
    finally
      ACalculator.Free;
    end;
    Assert(AHitTestResult.Column <> nil);
    Result := AHitTestResult.Column;
  finally
    AHitTestResult.Free;
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetTargetLayoutRowObtainTargetPageViewInfo(
  APos: TdxCaretPosition; var ACaretX: Integer): TdxRow;
var
  ALayoutPosition: TdxDocumentLayoutPosition;
  ALayoutPositionRow: TdxRow;
  ARowIndexInColumn: Integer;
  ASourceCellViewInfo: TdxTableCellViewInfo;
begin
  ALayoutPosition := APos.LayoutPosition;
  ALayoutPositionRow := ALayoutPosition.Row;
  ARowIndexInColumn := ALayoutPosition.Column.Rows.IndexOf(ALayoutPositionRow);
  Assert(ARowIndexInColumn >= 0);
  ASourceCellViewInfo := ALayoutPosition.TableCell;
  CorrectRowAndCellViewInfo(APos, ARowIndexInColumn, ASourceCellViewInfo);
  CorrectCaretXOnExtendSelection(ASourceCellViewInfo, ACaretX);
  Result := GetTargetLayoutRowObtainTargetPageViewInfoCore(APos.PageViewInfo, ACaretX, ALayoutPosition.Column,
    ARowIndexInColumn, ASourceCellViewInfo);
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetTargetLayoutRowObtainTargetPageViewInfoCore(
  APageViewInfo: TdxPageViewInfo; var ACaretX: Integer; AColumn: TdxColumn;
  ARowIndexInColumn: Integer;
  ASourceCellViewInfo: TdxTableCellViewInfo): TdxRow;
begin
  if ShouldObtainTargetPageViewInfo(ARowIndexInColumn, AColumn.Rows.Count) or
      IsBoundaryTableCellRowOnCurrentPage(ASourceCellViewInfo, AColumn, ARowIndexInColumn, ACaretX) then
    Result := ObtainTargetRowFromCurrentPageViewInfoAndCaretX(APageViewInfo, ASourceCellViewInfo, ACaretX)
  else
    Result := ObtainTargetLayoutRow(ARowIndexInColumn, ACaretX, AColumn, ASourceCellViewInfo);
end;

function TdxNextCaretPositionVerticalDirectionCalculator.GetView: TdxRichEditView;
begin
  Result := Control.InnerControl.ActiveView;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.IsBoundaryTableCellRowOnCurrentPage(
  ACell: TdxTableCellViewInfo; AColumn: TdxColumn; ASourceRowIndexInColumn,
  ACaretX: Integer): Boolean;
var
  ARows: TdxRowCollection;
  ASourceRowIsLastInCurrentCell: Boolean;
  ASourceRow, ALastRowInCurrentCell, ARow: TdxRow;
  ACurrentCell, AParentCell, ALastRowCellViewInfo: TdxTableCellViewInfo;
  ABoundaryRowInColumn, ALastRowInParentCell: TdxTableCellRow;
  AOuterTableViewInfo, ABoundaryOuterTableViewInfo: TdxTableViewInfo;
begin
  if not IsCellInBoundaryRow(AColumn, ACell, ACaretX) then
    Exit(False);
  ASourceRow := AColumn.Rows[ASourceRowIndexInColumn];
  if not IsBoundaryRowInCell(AColumn, ACell, ASourceRow) then
    Exit(False);
  if not (GetBoundaryRowInColumn(AColumn) is TdxTableCellRow) then
    Exit(False);
  ABoundaryRowInColumn := TdxTableCellRow(GetBoundaryRowInColumn(AColumn));

  ACurrentCell := ACell;
  while True do
  begin
    AParentCell := ACurrentCell.TableViewInfo.ParentTableCellViewInfo;
    if AParentCell = nil then
      Break;

    ARows := ACurrentCell.GetRows(AColumn);
    try
      ALastRowInCurrentCell := ARows.Last;
    finally
      ARows.Free;
    end;
    ASourceRowIsLastInCurrentCell := ALastRowInCurrentCell = ASourceRow;
    if not ASourceRowIsLastInCurrentCell and (GetCellBoundaryRowIndex(AColumn, AParentCell) <> ASourceRowIndexInColumn) then
      Exit(False);
    if not IsCellInBoundaryRow(AColumn, AParentCell, ACaretX) then
      Exit(False)
    else
      if ASourceRowIsLastInCurrentCell then
      begin
        ARows := AParentCell.GetRows(AColumn);
        try
          ARow := ARows.Last;
        finally
          ARows.Free;
        end;
        if ARow is TdxTableCellRow then
        begin
          ALastRowInParentCell := TdxTableCellRow(ARow);
          ALastRowCellViewInfo := ALastRowInParentCell.CellViewInfo;
          while (ALastRowCellViewInfo.TableViewInfo <> AParentCell.TableViewInfo) and (ALastRowCellViewInfo.TableViewInfo <> ACurrentCell.TableViewInfo) do
            ALastRowCellViewInfo := ALastRowCellViewInfo.TableViewInfo.ParentTableCellViewInfo;
          if ALastRowCellViewInfo.TableViewInfo = AParentCell.TableViewInfo then
            Exit(False);
        end;
      end;
    ACurrentCell := AParentCell;
  end;
  AOuterTableViewInfo := ACurrentCell.TableViewInfo;
  ABoundaryOuterTableViewInfo := ABoundaryRowInColumn.CellViewInfo.TableViewInfo;
  while ABoundaryOuterTableViewInfo.ParentTableCellViewInfo <> nil do
    ABoundaryOuterTableViewInfo := ABoundaryOuterTableViewInfo.ParentTableCellViewInfo.TableViewInfo;
  Result := AOuterTableViewInfo = ABoundaryOuterTableViewInfo;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.IsCaretAtTheBeginningOfAnotherCell(
  APos: TdxCaretPosition; ADocumentModel: TdxDocumentModel): Boolean;
var
  ASelection: TdxSelection;
begin
  ASelection := ADocumentModel.Selection;
  Result := (ASelection.Length > 0) and (ASelection.&End > ASelection.Start) and
    (APos.LayoutPosition.Row.GetFirstPosition(ADocumentModel.ActivePieceTable).LogPosition = APos.LogPosition);
end;

function TdxNextCaretPositionVerticalDirectionCalculator.IsCellFromPreviousRowIsInNestedTableInSourceCellViewInfo(
  ASourceCellViewInfo,
  ACellFromPreviousLayoutRow: TdxTableCellViewInfo): Boolean;
begin
  Result := False;
  if ACellFromPreviousLayoutRow = nil then
    Exit;
  Result := TdxDocumentModel(ASourceCellViewInfo.Cell.DocumentModel).Selection.FirstCellIsParentCellForSecondCellsTable(ASourceCellViewInfo.Cell, ACellFromPreviousLayoutRow.Cell);
end;

function TdxNextCaretPositionVerticalDirectionCalculator.IsLastOrFirstLayoutRowInCell(
  ARowIndexInCell, ACellsRowsCount: Integer): Boolean;
begin
  Result := ShouldObtainTargetPageViewInfo(ARowIndexInCell, ACellsRowsCount);
end;

function TdxNextCaretPositionVerticalDirectionCalculator.MoveBetweenRowsInsideCell(
  ACellRows: TdxRowCollection; ARowIndexInCell: Integer): TdxRow;
begin
  Result := GetNextLayoutRowByDirection(ACellRows, ARowIndexInCell);
end;

function TdxNextCaretPositionVerticalDirectionCalculator.ObtainLayoutRowFromValidCellInFirstRowNestedTable(
  ARowIndex, ACaretX: Integer; AColumn: TdxColumn;
  ASourceNestedLevel: Integer): TdxRow;
var
  ANextLayoutRow: TdxRow;
  ARows: TdxRowCollection;
  AFirstCellInInnerTable: TdxTableCellViewInfo;
  AFirstLayoutRowInInnerTableCell: TdxRow;
  ACandidateCell: TdxTableCellViewInfo;
begin
  Result := nil;
  ANextLayoutRow := GetNextLayoutRowByDirection(AColumn.Rows, ARowIndex);
  AFirstCellInInnerTable := GetTargetCellViewInfoInNestedTable(ANextLayoutRow, ACaretX, ASourceNestedLevel);
  if AFirstCellInInnerTable = nil then
    Exit;
  ARows := AFirstCellInInnerTable.GetRows(AColumn);
  try
    AFirstLayoutRowInInnerTableCell := GetFirstOrLastLayoutRowInCell(ARows);
  finally
    ARows.Free;
  end;
  ACandidateCell := AFirstLayoutRowInInnerTableCell.GetCellViewInfo;
  while AFirstCellInInnerTable.TableViewInfo.Table.NestedLevel <> ACandidateCell.TableViewInfo.Table.NestedLevel do
  begin
    AFirstCellInInnerTable := FindTableCellViewInfoInTableRowByX(ACandidateCell.GetTableRow, ACaretX);
    ARows := AFirstCellInInnerTable.GetRows(AColumn);
    try
      AFirstLayoutRowInInnerTableCell := GetFirstOrLastLayoutRowInCell(ARows);
    finally
      ARows.Free;
    end;
    ACandidateCell := AFirstLayoutRowInInnerTableCell.GetCellViewInfo;
  end;
  Result := AFirstLayoutRowInInnerTableCell;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.ObtainRowFromOuterTextRowOrOuterTableWhenCellsInInnerTable(
  AColumn: TdxColumn; AInnerSourceCellViewInfo: TdxTableCellViewInfo;
  ACaretX: Integer; AFirstOrLastCellInRow: TdxTableCellViewInfo): TdxRow;
var
  AFirstLastTextRowsCollection: TdxRowCollection;
  AFirstOrLastTextRowInCell: TdxRow;
  ARowIndexInColumn: Integer;
begin
  AFirstLastTextRowsCollection := AFirstOrLastCellInRow.GetRows(AColumn);
  try
    AFirstOrLastTextRowInCell := GetTargetLayoutRowInCell(AFirstLastTextRowsCollection);
  finally
    AFirstLastTextRowsCollection.Free;
  end;
  ARowIndexInColumn := AColumn.Rows.IndexOf(AFirstOrLastTextRowInCell);
  Result := GetNextLayoutRowByDirection(AColumn.Rows, ARowIndexInColumn);
end;

function TdxNextCaretPositionVerticalDirectionCalculator.ObtainTargetLayoutRow(
  ARowIndexInColumn, ACaretX: Integer; AColumn: TdxColumn;
  ASourceCell: TdxTableCellViewInfo): TdxRow;
var
  ANextCell: TdxTableCellViewInfo;
  ATargetCell: TdxTableCellViewInfo;
  ATargetCellLayoutRows: TdxRowCollection;
  ANextCellWithNonZeroRows: TdxTableCellViewInfo;
begin
  ANextCell := ASourceCell;
  if ASourceCell = nil then
  begin
    Result := GetNextLayoutRowByDirection(AColumn.Rows, ARowIndexInColumn);
    ANextCell := Result.GetCellViewInfo;
    if ANextCell <> nil then
    begin
      ATargetCell := FindTableCellViewInfoInTableRowByX(ANextCell.GetTableRow, ACaretX);
      ATargetCellLayoutRows := ATargetCell.GetRows(AColumn);
      try
        if ATargetCellLayoutRows.Count = 0 then
        begin
          ANextCellWithNonZeroRows := GetNextCellViewInfoInTargetRowWithNonZeroRows(AColumn,
            ATargetCell.GetTableRow, ATargetCell);
          if ANextCellWithNonZeroRows <> nil then
          begin
            ATargetCellLayoutRows.Free;
            ATargetCellLayoutRows := ANextCellWithNonZeroRows.GetRows(AColumn);
          end;
        end;
        Result := GetTargetLayoutRowCore(ATargetCellLayoutRows);
      finally
        ATargetCellLayoutRows.Free;
      end;
    end;
  end
  else
    Result := ObtainTargetLayoutRowFromNextCellInCurrentTable(ARowIndexInColumn, ACaretX, AColumn, ANextCell);
end;

function TdxNextCaretPositionVerticalDirectionCalculator.ObtainTargetLayoutRowFromNextCellInCurrentTable(
  ARowIndexInColumn, ACaretX: Integer; AColumn: TdxColumn;
  ASourceCellViewInfo: TdxTableCellViewInfo): TdxRow;
var
  ANextCell: TdxTableCellViewInfo;
  ACellRows: TdxRowCollection;
  ARowIndexInCell: Integer;
  AShouldJumpToCellInNestedTable: Boolean;
  AIsLastOrFirstLayoutRowInCell: Boolean;
  ATargetLayoutRow: TdxRow;
  ALastTableRow: TdxTableRowViewInfoBase;
  AFirstOrLastCellInRow: TdxTableCellViewInfo;
begin
  ANextCell := GetTargetCellViewInfoFromNextTableRow(ASourceCellViewInfo, ACaretX);
  ACellRows := ASourceCellViewInfo.GetRows(AColumn);
  try
    ARowIndexInCell := ACellRows.IndexOf(AColumn.Rows[ARowIndexInColumn]);
    AShouldJumpToCellInNestedTable := ShouldJumpToCellInNestedTable(AColumn.Rows,
      ASourceCellViewInfo.Cell.Table.NestedLevel, ARowIndexInColumn, ACaretX);
    AIsLastOrFirstLayoutRowInCell := IsLastOrFirstLayoutRowInCell(ARowIndexInCell, ACellRows.Count);
    if ShouldJumpToNextCellInCurrentTable(AShouldJumpToCellInNestedTable, ANextCell,
      AIsLastOrFirstLayoutRowInCell) then
      Exit(ObtainTargetLayoutRowFromNextCellInCurrentTableCore(ACaretX, AColumn, ASourceCellViewInfo, ANextCell));
    if AShouldJumpToCellInNestedTable then
      Exit(ObtainLayoutRowFromValidCellInFirstRowNestedTable(ARowIndexInColumn, ACaretX, AColumn,
        ASourceCellViewInfo.TableViewInfo.Table.NestedLevel));
    if ShouldObtainNextLayoutRowInsideCell(AIsLastOrFirstLayoutRowInCell, ANextCell) then
    begin
      ATargetLayoutRow := MoveBetweenRowsInsideCell(ACellRows, ARowIndexInCell);
      Result := ATargetLayoutRow;
    end
    else
    begin
      ALastTableRow := GetLastTableRowInDirection(ASourceCellViewInfo, ACaretX);
      AFirstOrLastCellInRow := GetFirstOrLastCellViewInfoInRow(ALastTableRow);
      Result := MoveFromInnerIntoOuterTable(ASourceCellViewInfo, AColumn, AFirstOrLastCellInRow, ACaretX);
    end;
  finally
    ACellRows.Free;
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.ObtainTargetLayoutRowFromNextCellInCurrentTableCore(
  ACaretX: Integer; AColumn: TdxColumn; ASourceCellViewInfo,
  ANextCell: TdxTableCellViewInfo): TdxRow;
var
  ACellRows: TdxRowCollection;
  AFirstOrLastRowInCell: TdxRow;
  ANextCellViewInfoNestedLevel: Integer;
  AIsLayoutRowInNestedTable: Boolean;
begin
  ACellRows := ANextCell.GetRows(AColumn);
  try
    if ACellRows.Count = 0 then
      Exit(AColumn.Rows.First);
    AFirstOrLastRowInCell := GetTargetLayoutRowCore(ACellRows);
  finally
    ACellRows.Free;
  end;
  ANextCellViewInfoNestedLevel := AFirstOrLastRowInCell.GetCellViewInfo.TableViewInfo.Table.NestedLevel;
  AIsLayoutRowInNestedTable := ANextCellViewInfoNestedLevel <> ASourceCellViewInfo.Cell.Table.NestedLevel;
  if AIsLayoutRowInNestedTable then
    ANextCell := FindMostNestedTableCell(ANextCell, AColumn, ACaretX);
  ACellRows := ANextCell.GetRows(AColumn);
  try
    Result := GetTargetLayoutRowCore(ACellRows);
  finally
    ACellRows.Free;
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.ObtainTargetPageViewInfo(
  ACurrentPageViewInfo: TdxPageViewInfo;
  APhysicalCaretX: Integer): TdxPageViewInfo;
var
  APageRows: TdxPageViewInfoRowCollection;
  ARowIndex: Integer;
  APageRow: TdxPageViewInfoRow;
begin
  Assert(ACurrentPageViewInfo <> nil);
  APageRows := View.PageViewInfoGenerator.ActiveGenerator.PageRows;
  ARowIndex := GetPageViewInfoRowIndex(APageRows, ACurrentPageViewInfo);
  Assert(ARowIndex >= 0);
  if ShouldObtainTargetPageViewInfo(ARowIndex, APageRows.Count) then
    APageRow := GenerateTargetPageViewInfoRow
  else
    APageRow := ObtainExistingTargetPageViewInfoRow(APageRows, ARowIndex);
  if APageRow <> nil then
  begin
    Result := APageRow.GetPageAtPoint(Point(APhysicalCaretX, 0), False);
    Assert(Result <> nil);
  end
  else
    Result := nil;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.ObtainTargetRowFromCurrentPageViewInfoAndCaretX(
  APageViewInfo: TdxPageViewInfo;
  ACellViewInfoFromPrevoiusTableViewInfo: TdxTableCellViewInfo;
  var ACaretX: Integer): TdxRow;
var
  APhysicalCaretPoint, ALogicalCaretPoint: TPoint;
  ATargetPageViewInfo: TdxPageViewInfo;
  AColumn: TdxColumn;
  ARows: TdxRowCollection;
  AFirstColumnRow: TdxRow;
  AFirstColumnRowCell, ACellFromNextPage, ANextCell, AMostNestedTableCell: TdxTableCellViewInfo;
  ATargetTable, AParentTargetTable: TdxTable;
  ATargetTableFromPreviousPageNestedLevel, AFirstColumnRowCellTableNestedLevel: Integer;
  AIsExpectedTheNestedTableEndsOnPreviousPageAndOuterTableContinuesOnTheNextPage: Boolean;
  ATargetRow: TdxTableRowViewInfoBase;
begin
  APhysicalCaretPoint := View.CreatePhysicalPoint(APageViewInfo, TPoint.Create(ACaretX, 0));
  ATargetPageViewInfo := ObtainTargetPageViewInfo(APageViewInfo, APhysicalCaretPoint.X);
  if ATargetPageViewInfo = nil then
    Exit(nil);
  ALogicalCaretPoint := View.CreateLogicalPoint(ATargetPageViewInfo.ClientBounds, TPoint.Create(APhysicalCaretPoint.X, 0));
  ACaretX := ALogicalCaretPoint.X;
  AColumn := GetTargetColumn(ATargetPageViewInfo.Page, ALogicalCaretPoint.X);

  AFirstColumnRow := GetTargetLayoutRowCore(AColumn.Rows);
  if AFirstColumnRow = nil then
    Exit(nil);

  AFirstColumnRowCell := AFirstColumnRow.GetCellViewInfo;
  if AFirstColumnRowCell = nil then
    Exit(GetTargetLayoutRowCore(AColumn.Rows));
  if ACellViewInfoFromPrevoiusTableViewInfo <> nil then
    ATargetTable := ACellViewInfoFromPrevoiusTableViewInfo.TableViewInfo.Table
  else
    ATargetTable := AFirstColumnRowCell.TableViewInfo.Table;
  ATargetTableFromPreviousPageNestedLevel := ATargetTable.NestedLevel;
  AFirstColumnRowCellTableNestedLevel := AFirstColumnRowCell.TableViewInfo.Table.NestedLevel;
  AIsExpectedTheNestedTableEndsOnPreviousPageAndOuterTableContinuesOnTheNextPage := ATargetTableFromPreviousPageNestedLevel - 1 = AFirstColumnRowCellTableNestedLevel;
  if AIsExpectedTheNestedTableEndsOnPreviousPageAndOuterTableContinuesOnTheNextPage then
  begin
    AParentTargetTable := ATargetTable.ParentCell.Table;
    ATargetTable := AParentTargetTable;
  end;
  ATargetRow := FindMostParentTableRowViewInfo(ATargetTable, AFirstColumnRowCell);
  ACellFromNextPage := FindTableCellViewInfoInTableRowByX(ATargetRow, ACaretX);
  ARows := ACellFromNextPage.GetRows(AColumn);
  try
    if ACellFromNextPage.IsStartOnPreviousTableViewInfo and (ARows.Count = 0) then
    begin
      ANextCell := GetTargetCellViewInfoFromNextTableRow(ACellFromNextPage, ACaretX);
      if ANextCell = nil then
      begin
        ANextCell := GetNextCellViewInfoInTargetRowWithNonZeroRows(AColumn, ATargetRow, ACellFromNextPage);
        Exit(ObtainTargetLayoutRowFromNextCellInCurrentTableCore(ACaretX, AColumn, ACellFromNextPage, ANextCell));
      end;
      Exit(ObtainTargetLayoutRowFromNextCellInCurrentTableCore(ACaretX, AColumn, ACellFromNextPage, ANextCell));
    end;
  finally
    ARows.Free;
  end;
  AMostNestedTableCell := FindMostNestedTableCell(ACellFromNextPage, AColumn, ACaretX);
  ARows := AMostNestedTableCell.GetRows(AColumn);
  try
    Result := GetTargetLayoutRowCore(ARows);
  finally
    ARows.Free;
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.ShouldJumpToCellInNestedTable(
  ARows: TdxRowCollection; ACurrentCellNestedLevel, ASourceRowIndexInColumn,
  ACaretX: Integer): Boolean;
var
  ASourceCellRow: TdxRow;
  ASourceCell: TdxTableCellViewInfo;
  ANextLayoutRow: TdxRow;
  ATargetCell: TdxTableCellViewInfo;
  ANextRowTableNestedLevel: Integer;
  ANextCellIsNested: Boolean;
  AParentCellViewInfoForTargetCell: TdxTableCellViewInfo;
begin
  ASourceCellRow := ARows[ASourceRowIndexInColumn];
  ASourceCell := ASourceCellRow.GetCellViewInfo;
  Result := False;
  if (ASourceCell = nil) or (ASourceCell.InnerTables.Count = 0) then
    Exit;
  ANextLayoutRow := GetNextLayoutRowByDirection(ARows, ASourceRowIndexInColumn);
  ATargetCell := GetTargetCellViewInfoInNestedTable(ANextLayoutRow, ACaretX, ACurrentCellNestedLevel);
  if ATargetCell = nil then
    Exit;
  ANextRowTableNestedLevel := ATargetCell.Cell.Table.NestedLevel;
  ANextCellIsNested := ANextRowTableNestedLevel > ACurrentCellNestedLevel;
  if not ANextCellIsNested then
    Exit;
  AParentCellViewInfoForTargetCell := ATargetCell.TableViewInfo.ParentTableCellViewInfo;
  while AParentCellViewInfoForTargetCell <> nil do
  begin
    Result := ASourceCell = AParentCellViewInfoForTargetCell;
    if Result then
      Break;
    AParentCellViewInfoForTargetCell := AParentCellViewInfoForTargetCell.TableViewInfo.ParentTableCellViewInfo;
  end;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.ShouldJumpToNextCellInCurrentTable(
  AShouldJumpToCellInNestedTable: Boolean; ANextCell: TdxTableCellViewInfo;
  AIsLastOrFirstLayoutRowInCell: Boolean): Boolean;
var
  AShouldJumpToNextCellInCurrentTable: Boolean;
begin
  AShouldJumpToNextCellInCurrentTable := (ANextCell <> nil) and AIsLastOrFirstLayoutRowInCell;
  Result := not AShouldJumpToCellInNestedTable and AShouldJumpToNextCellInCurrentTable;
end;

function TdxNextCaretPositionVerticalDirectionCalculator.ShouldObtainNextLayoutRowInsideCell(
  AIsLastOrFirstLayoutRowInCell: Boolean;
  ANextCell: TdxTableCellViewInfo): Boolean;
begin
  Result := not AIsLastOrFirstLayoutRowInCell;
end;

{ TdxNextCaretPositionLineUpCalculator }

function TdxNextCaretPositionLineUpCalculator.GetNextLayoutRowIndexInDirection(
  ASourceRowIndex: Integer): Integer;
begin
  Result := Max(ASourceRowIndex - 1, 0);
end;

function TdxNextCaretPositionLineUpCalculator.GetTargetLayoutRowInCell(
  ARows: TdxRowCollection): TdxRow;
begin
  Result := ARows.First;
end;

function TdxNextCaretPositionLineUpCalculator.ObtainExistingTargetPageViewInfoRow(
  APageRows: TdxPageViewInfoRowCollection;
  ACurrentRowIndex: Integer): TdxPageViewInfoRow;
begin
  Result := APageRows[ACurrentRowIndex - 1];
end;

function TdxNextCaretPositionLineUpCalculator.ShouldObtainTargetPageViewInfo(
  ASourceRowIndex, ASourceRowCount: Integer): Boolean;
begin
  Assert(ASourceRowIndex >= 0);
  Result := ASourceRowIndex = 0;
end;

{ TdxNextCaretPositionUpDirectionCalculator }

function TdxNextCaretPositionUpDirectionCalculator.CalculateFirstInvalidPageIndex: Integer;
begin
  Result := -1;
end;

function TdxNextCaretPositionUpDirectionCalculator.CalculateFirstInvisiblePageIndex: Integer;
begin
  Result := View.CalculateFirstInvisiblePageIndexBackward;
end;

procedure TdxNextCaretPositionUpDirectionCalculator.CorrectGeneratedRowPagesHorizontalLocation(
  ALayoutManager: TdxPageGeneratorLayoutManager;
  ARow: TdxPageViewInfoRow);
var
  X: Integer;
  I: Integer;
  APageViewInfo: TdxPageViewInfo;
  ABounds: TRect;
begin
  Assert(ARow.Count > 0);
  X := ARow[0].Bounds.Left;
  for I := ARow.Count - 1 downto 0 do
  begin
    APageViewInfo := ARow[I];
    ABounds := APageViewInfo.Bounds;
    ABounds.MoveToLeft(X);
    APageViewInfo.Bounds := ABounds;
    ALayoutManager.UpdatePageClientBounds(APageViewInfo);
    Inc(X, ABounds.Width);
  end;
end;

function TdxNextCaretPositionUpDirectionCalculator.GetBottomRightTableCellViewInfoByNestedLevel(
  ANextLayoutRow: TdxRow; ADesiredNestedLevel: Integer): TdxTableCellViewInfo;
var
  ANextLayoutRowTableNestedLevel: Integer;
begin
  Result := ANextLayoutRow.GetCellViewInfo;
  if Result = nil then
    Exit;
  ANextLayoutRowTableNestedLevel := Result.TableViewInfo.Table.NestedLevel;
  while ANextLayoutRowTableNestedLevel > ADesiredNestedLevel do
  begin
    Result := Result.TableViewInfo.ParentTableCellViewInfo;
    ANextLayoutRowTableNestedLevel := Result.TableViewInfo.Table.NestedLevel;
  end;
end;

function TdxNextCaretPositionUpDirectionCalculator.GetBoundaryRowInColumn(AColumn: TdxColumn): TdxRow;
begin
  Result := AColumn.Rows.First;
end;

function TdxNextCaretPositionUpDirectionCalculator.GetCellBoundaryRowIndex(AColumn: TdxColumn;
  AParentCell: TdxTableCellViewInfo): Integer;
begin
  Result := AParentCell.GetFirstRowIndex(AColumn);
end;

function TdxNextCaretPositionUpDirectionCalculator.GetFirstOrLastCellViewInfoInRow(
  ARow: TdxTableRowViewInfoBase): TdxTableCellViewInfo;
begin
  Result := ARow.Cells.First;
end;

function TdxNextCaretPositionUpDirectionCalculator.GetFirstOrLastLayoutRowInCell(
  ACellRows: TdxRowCollection): TdxRow;
begin
  Result := ACellRows.Last;
end;

function TdxNextCaretPositionUpDirectionCalculator.GetTargetCellViewInfoInNestedTable(
  ANextLayoutRow: TdxRow; ACaretX,
  ACurrentNestedLevel: Integer): TdxTableCellViewInfo;
var
  ATopLeftCell: TdxTableCellViewInfo;
begin
  ATopLeftCell := GetBottomRightTableCellViewInfoByNestedLevel(ANextLayoutRow, ACurrentNestedLevel + 1);
  if ATopLeftCell = nil then
    Result := nil
  else
    Result := FindTableCellViewInfoInTableRowByX(ATopLeftCell.GetTableRow, ACaretX);
end;

function TdxNextCaretPositionUpDirectionCalculator.GetTargetLayoutRowCore(
  ARows: TdxRowCollection): TdxRow;
begin
  Result := ARows.Last;
end;

function TdxNextCaretPositionUpDirectionCalculator.GetTargetPageArea(
  APage: TdxPage): TdxPageArea;
begin
  Result := APage.Areas.Last;
end;

function TdxNextCaretPositionUpDirectionCalculator.GetTargetTableRowCore(
  ARow: TdxTableRowViewInfoBase): TdxTableRowViewInfoBase;
begin
  Result := ARow.Previous;
end;

function TdxNextCaretPositionUpDirectionCalculator.IsBoundaryRowInCell(AColumn: TdxColumn; ACell: TdxTableCellViewInfo;
  ASourceRow: TdxRow): Boolean;
var
  ARows: TdxRowCollection;
begin
  ARows := ACell.GetRows(AColumn);
  try
    Result := ARows.First = ASourceRow;
  finally
    ARows.Free;
  end;
end;

function TdxNextCaretPositionUpDirectionCalculator.IsCellInBoundaryRow(
  AColumn: TdxColumn; ACell: TdxTableCellViewInfo; ACaretX: Integer): Boolean;
begin
  Result := ACell <> nil;
  if Result then
    Result := ACell.TopAnchorIndex = 0;
end;

function TdxNextCaretPositionUpDirectionCalculator.IsCellViewInfoContinuesOnAnotherPage(
  ACell: TdxTableCellViewInfo): Boolean;
begin
  Result := ACell.IsStartOnPreviousTableViewInfo;
end;

function TdxNextCaretPositionUpDirectionCalculator.MoveFromInnerIntoOuterTable(
  AInnerSourceCellViewInfo: TdxTableCellViewInfo; AColumn: TdxColumn;
  AFirstOrLastCellInRow: TdxTableCellViewInfo; ACaretX: Integer): TdxRow;
begin
  if ShouldJumpUpFromMultipleInnerTableToOuter(AFirstOrLastCellInRow) then
    Result := ObtainRowInNextCellWhenFirstCellInTableRowIsInMultipleInnerTable(AColumn,
      ACaretX, AFirstOrLastCellInRow.TableViewInfo)
  else
    Result := ObtainRowFromOuterTextRowOrOuterTableWhenCellsInInnerTable(AColumn,
      AInnerSourceCellViewInfo, ACaretX, AFirstOrLastCellInRow);
end;

function TdxNextCaretPositionUpDirectionCalculator.ObtainRowInNextCellWhenFirstCellInTableRowIsInMultipleInnerTable(
  AColumn: TdxColumn; ACaretX: Integer;
  ATableViewInfo: TdxTableViewInfo): TdxRow;
var
  ARow: TdxRow;
  ARows: TdxRowCollection;
  ARowIndexInColumn: Integer;
  AParentCellViewInfo: TdxTableCellViewInfo;
begin
  Result := nil;
  AParentCellViewInfo := ATableViewInfo.ParentTableCellViewInfo;
  if AParentCellViewInfo = nil then
    Exit;
  ARows := AParentCellViewInfo.GetRows(AColumn);
  try
    ARow := GetTargetLayoutRowInCell(ARows);
  finally
    ARows.Free;
  end;
  ARowIndexInColumn := AColumn.Rows.IndexOf(ARow);
  Result := ObtainTargetLayoutRowFromNextCellInCurrentTable(ARowIndexInColumn, ACaretX,
    AColumn, AParentCellViewInfo);
end;

function TdxNextCaretPositionUpDirectionCalculator.ShouldJumpUpFromMultipleInnerTableToOuter(
  AFirstCellInTableRow: TdxTableCellViewInfo): Boolean;
begin
  Result := AFirstCellInTableRow.Cell.IsFirstRow and
    AFirstCellInTableRow.TableViewInfo.FirstContentInParentCell;
end;

{ TdxNextCaretPositionLineDownCalculator }

function TdxNextCaretPositionLineDownCalculator.GetNextLayoutRowIndexInDirection(
  ASourceRowIndex: Integer): Integer;
begin
  Result := ASourceRowIndex + 1;
end;

function TdxNextCaretPositionLineDownCalculator.ObtainExistingTargetPageViewInfoRow(
  APageRows: TdxPageViewInfoRowCollection;
  ACurrentRowIndex: Integer): TdxPageViewInfoRow;
begin
  Result := APageRows[ACurrentRowIndex + 1];
end;

function TdxNextCaretPositionLineDownCalculator.ShouldObtainTargetPageViewInfo(
  ASourceRowIndex, ASourceRowCount: Integer): Boolean;
begin
  Assert(ASourceRowIndex >= 0);
  Assert(ASourceRowIndex + 1 <= ASourceRowCount);
  Result := ASourceRowIndex + 1 = ASourceRowCount;
end;

{ TdxNextCaretPositionDownDirectionCalculator }

function TdxNextCaretPositionDownDirectionCalculator.CalculateFirstInvalidPageIndex: Integer;
begin
  Result := View.FormattingController.PageController.Pages.Count;
end;

function TdxNextCaretPositionDownDirectionCalculator.CalculateFirstInvisiblePageIndex: Integer;
begin
  Result := View.CalculateFirstInvisiblePageIndexForward;
end;

procedure TdxNextCaretPositionDownDirectionCalculator.CorrectGeneratedRowPagesHorizontalLocation(
  ALayoutManager: TdxPageGeneratorLayoutManager; ARow: TdxPageViewInfoRow);
begin
end;

function TdxNextCaretPositionDownDirectionCalculator.GetBoundaryRowInColumn(AColumn: TdxColumn): TdxRow;
begin
  Result := AColumn.Rows.Last;
end;

function TdxNextCaretPositionDownDirectionCalculator.GetCellBoundaryRowIndex(AColumn: TdxColumn;
  AParentCell: TdxTableCellViewInfo): Integer;
begin
  Result := AParentCell.GetLastRowIndex(AColumn);
end;

function TdxNextCaretPositionDownDirectionCalculator.GetFirstOrLastCellViewInfoInRow(
  ARow: TdxTableRowViewInfoBase): TdxTableCellViewInfo;
begin
  if ARow = nil then
    Result := nil
  else
    Result := GetLastNonCoveredByVerticalMergingCell(ARow);
end;

function TdxNextCaretPositionDownDirectionCalculator.GetFirstOrLastLayoutRowInCell(
  ACellRows: TdxRowCollection): TdxRow;
begin
  Result := ACellRows.First;
end;

function TdxNextCaretPositionDownDirectionCalculator.GetTargetCellViewInfoInNestedTable(
  ANextLayoutRow: TdxRow; ACaretX,
  ACurrentNestedLevel: Integer): TdxTableCellViewInfo;
var
  ATopLeftCell: TdxTableCellViewInfo;
begin
  ATopLeftCell := GetTopLeftTableCellViewInfoByNestedLevel(ANextLayoutRow, ACurrentNestedLevel + 1);
  if ATopLeftCell = nil then
    Result := nil
  else
    Result := FindTableCellViewInfoInTableRowByX(ATopLeftCell.GetTableRow, ACaretX);
end;

function TdxNextCaretPositionDownDirectionCalculator.GetTargetLayoutRowCore(
  ARows: TdxRowCollection): TdxRow;
begin
 Result := ARows.First;
end;

function TdxNextCaretPositionDownDirectionCalculator.GetTargetLayoutRowInCell(
  ARows: TdxRowCollection): TdxRow;
begin
 Result := ARows.Last;
end;

function TdxNextCaretPositionDownDirectionCalculator.GetTargetPageArea(APage: TdxPage): TdxPageArea;
begin
  Result := APage.Areas.First;
end;

function TdxNextCaretPositionDownDirectionCalculator.GetTargetTableRowCore(
  ARow: TdxTableRowViewInfoBase): TdxTableRowViewInfoBase;
begin
  Result := ARow.Next;
end;

function TdxNextCaretPositionDownDirectionCalculator.GetTopLeftTableCellViewInfoByNestedLevel(
  ANextLayoutRow: TdxRow; ADesiredNestedLevel: Integer): TdxTableCellViewInfo;
var
  ANextLayoutRowTableNestedLevel: Integer;
begin
  Result := ANextLayoutRow.GetCellViewInfo;
  if Result = nil then
    Exit;
  ANextLayoutRowTableNestedLevel := Result.TableViewInfo.Table.NestedLevel;
  while ANextLayoutRowTableNestedLevel > ADesiredNestedLevel do
  begin
    Result := Result.TableViewInfo.ParentTableCellViewInfo;
    ANextLayoutRowTableNestedLevel := Result.TableViewInfo.Table.NestedLevel;
  end;
end;

function TdxNextCaretPositionDownDirectionCalculator.IsBoundaryRowInCell(AColumn: TdxColumn;
  ACell: TdxTableCellViewInfo; ASourceRow: TdxRow): Boolean;
var
  ARows: TdxRowCollection;
begin
  ARows := ACell.GetRows(AColumn);
  try
    Result := ARows.Last = ASourceRow;
  finally
    ARows.Free;
  end;
end;

function TdxNextCaretPositionDownDirectionCalculator.IsCellInBoundaryRow(
  AColumn: TdxColumn; ACell: TdxTableCellViewInfo; ACaretX: Integer): Boolean;
begin
  Result := (ACell <> nil) and (ACell.BottomAnchorIndex + 1 = ACell.TableViewInfo.Anchors.Count);
end;

function TdxNextCaretPositionDownDirectionCalculator.IsCellViewInfoContinuesOnAnotherPage(
  ACell: TdxTableCellViewInfo): Boolean;
begin
  Result := ACell.IsEndOnNextTableViewInfo;
end;

function TdxNextCaretPositionDownDirectionCalculator.MoveFromInnerIntoOuterTable(
  AInnerSourceCellViewInfo: TdxTableCellViewInfo; AColumn: TdxColumn;
  ALastCellInTableRow: TdxTableCellViewInfo; ACaretX: Integer): TdxRow;
var
  ATargetLastCellViewInfoWithNonZeroRows: TdxTableCellViewInfo;
  ARow: TdxTableRowViewInfoBase;
  ALastCellIndex: Integer;
  AFirstLastTextRowsCollection: TdxRowCollection;
begin
  ATargetLastCellViewInfoWithNonZeroRows := ALastCellInTableRow;
  ARow := ATargetLastCellViewInfoWithNonZeroRows.GetTableRow;
  ALastCellIndex := ARow.Cells.IndexOf(ATargetLastCellViewInfoWithNonZeroRows);
  AFirstLastTextRowsCollection := ATargetLastCellViewInfoWithNonZeroRows.GetRows(AColumn);
  try
    while (ALastCellIndex > 0) and (AFirstLastTextRowsCollection.Count = 0) and
      ATargetLastCellViewInfoWithNonZeroRows.IsStartOnPreviousTableViewInfo do
    begin
      Dec(ALastCellIndex);
      ATargetLastCellViewInfoWithNonZeroRows := ARow.Cells[ALastCellIndex];
      AFirstLastTextRowsCollection.Free;
      AFirstLastTextRowsCollection := ATargetLastCellViewInfoWithNonZeroRows.GetRows(AColumn);
    end;
  finally
    AFirstLastTextRowsCollection.Free;
  end;
  Result := ObtainRowFromOuterTextRowOrOuterTableWhenCellsInInnerTable(AColumn,
    AInnerSourceCellViewInfo, ACaretX, ATargetLastCellViewInfoWithNonZeroRows);
end;

{ TdxScrollVerticallyByPhysicalOffsetEnsurePageGenerationCommand }

function TdxScrollVerticallyByPhysicalOffsetEnsurePageGenerationCommand.CalculateNewTopInvisibleHeight(
  ADelta: Int64): Int64;
begin
  Result := View.PageViewInfoGenerator.TopInvisibleHeight + ADelta;
end;

procedure TdxScrollVerticallyByPhysicalOffsetEnsurePageGenerationCommand.GeneratePagesToEnsureScrollingSuccessfull(
  ADelta: Int64);
var
  ANewTopInvisibleHeight: Int64;
  APageViewInfoGenerator: TdxPageViewInfoGenerator;
begin
  ANewTopInvisibleHeight := CalculateNewTopInvisibleHeight(ADelta);
  APageViewInfoGenerator := View.PageViewInfoGenerator;
  if ANewTopInvisibleHeight <= APageViewInfoGenerator.TotalHeight - APageViewInfoGenerator.VisibleHeight + 1 then
    Exit;
  View.ResetPages(TdxPageGenerationStrategyType.RunningHeight);
  APageViewInfoGenerator.TopInvisibleHeight := ANewTopInvisibleHeight;
  View.GeneratePages;
  Assert(ANewTopInvisibleHeight < APageViewInfoGenerator.TotalHeight);
end;

{ TdxExtendKeyboardSelectionHorizontalDirectionCalculator }

constructor TdxExtendKeyboardSelectionHorizontalDirectionCalculator.Create(
  ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
end;

function TdxExtendKeyboardSelectionHorizontalDirectionCalculator.CalculateNextPosition(
  ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
begin
  Result := ALogPosition;
end;

function TdxExtendKeyboardSelectionHorizontalDirectionCalculator.CalculatePrevPosition(
  ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
begin
  Result := ALogPosition;
end;

{ TdxEnsureCaretVisibleVerticallyForMovePrevNextPageCommand }

function TdxEnsureCaretVisibleVerticallyForMovePrevNextPageCommand.CalculatePhysicalOffsetForRowBoundsVisibility(
  const AViewBounds, APhysicalRowBounds: TRect): Integer;
begin
  if (AViewBounds.Top <= APhysicalRowBounds.Top) and (APhysicalRowBounds.Bottom <= AViewBounds.Bottom) then
    Result := 0
  else
    Result := APhysicalRowBounds.Top - AViewBounds.Top;
end;

function TdxEnsureCaretVisibleVerticallyForMovePrevNextPageCommand.CalculateTargetRowLogicalBounds(
  APageViewInfo: TdxPageViewInfo; const ARow: TdxCurrentRowInfo): TRect;
var
  ALogicalRowBounds, ATopmostRowBounds: TRect;
begin
  ALogicalRowBounds := ValidateRowBounds(APageViewInfo, ARow.BoundsRelativeToPage);
  ATopmostRowBounds := ValidateRowBounds(APageViewInfo, CalculateTopmostRow(APageViewInfo.Page).Bounds);
  ALogicalRowBounds.MoveToTop(ALogicalRowBounds.Top - Round(PhysicalOffsetAboveTargetRow * ActiveView.ScaleFactor));
  ALogicalRowBounds.MoveToTop(Max(0, ALogicalRowBounds.Top));
  if ATopmostRowBounds.Top < ALogicalRowBounds.Top then
    Result := ATopmostRowBounds
  else
    Result := ALogicalRowBounds;
end;

function TdxEnsureCaretVisibleVerticallyForMovePrevNextPageCommand.CalculateTopmostRow(
  APage: TdxPage): TdxRow;
var
  AColumns: TdxColumnCollection;
  I: Integer;
  ARow: TdxRow;
begin
  AColumns := APage.Areas.First.Columns;
  Result := AColumns[0].Rows.First;
  for I := 1 to AColumns.Count - 1 do
  begin
    ARow := AColumns[I].Rows.First;
    if ARow.Bounds.Top < Result.Bounds.Top then
      Result := ARow;
  end;
end;

{ TdxHistoryCommandBase }

procedure TdxHistoryCommandBase.ExecuteCore;
var
  ADocumentModel: TdxDocumentModel;
begin
  CheckExecutedAtUIThread;
  ADocumentModel := DocumentModel;
  ADocumentModel.BeginUpdate;
  try
    PerformHistoryOperation(ADocumentModel.History);
    InvalidateDocumentLayout;
  finally
    ADocumentModel.EndUpdate;
  end;
end;

procedure TdxHistoryCommandBase.InvalidateDocumentLayout;
begin
  DocumentModel.InvalidateDocumentLayout;
  RichEditControl.EnsureCaretVisible(False);
end;

procedure TdxHistoryCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := IsContentEditable and CanPerformHistoryOperation(DocumentModel.History);
  AState.Visible := True;
end;

{ TdxUndoCommand }

function TdxUndoCommand.CanPerformHistoryOperation(
  AHistory: TdxDocumentHistory): Boolean;
begin
  Result := AHistory.CanUndo;
end;

class function TdxUndoCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandUndoDescription);
end;

class function TdxUndoCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandUndoMenuCaption);
end;

class function TdxUndoCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.Undo;
end;

class function TdxUndoCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.Undo;
end;

procedure TdxUndoCommand.PerformHistoryOperation(AHistory: TdxDocumentHistory);
begin
  AHistory.Undo;
end;

procedure TdxUndoCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Undo,
    CanPerformHistoryOperation(DocumentModel.History));
end;


{ TdxClearUndoCommand }

class function TdxClearUndoCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ClearUndo;
end;

class function TdxClearUndoCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandClearUndoMenuCaption);
end;

class function TdxClearUndoCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandClearUndoDescription);
end;

procedure TdxClearUndoCommand.PerformHistoryOperation(AHistory: TdxDocumentHistory);
begin
  AHistory.SmartClear;
end;

function TdxClearUndoCommand.CanPerformHistoryOperation(AHistory: TdxDocumentHistory): Boolean;
begin
  Result := True;
end;

procedure TdxClearUndoCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Undo,
    CanPerformHistoryOperation(DocumentModel.History));
end;

{ TdxRedoCommand }

function TdxRedoCommand.CanPerformHistoryOperation(
  AHistory: TdxDocumentHistory): Boolean;
begin
  Result := AHistory.CanRedo;
end;

class function TdxRedoCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandRedoDescription);
end;

class function TdxRedoCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandRedoMenuCaption);
end;

class function TdxRedoCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.Redo;
end;

class function TdxRedoCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.Redo;
end;

procedure TdxRedoCommand.PerformHistoryOperation(AHistory: TdxDocumentHistory);
begin
  AHistory.Redo;
end;

procedure TdxRedoCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Undo, CanPerformHistoryOperation(DocumentModel.History));
end;

{ TdxToggleShowWhitespaceCommand }

procedure TdxToggleShowWhitespaceCommand.EnsureSelectionVisible;
var
  ASelectionInterval: TdxRunInfo;
  AStart, AEnd: TdxDocumentModelPosition;
  ATextFilter: IdxVisibleTextFilter;
  AStartLogPosition, AEndLogPosition: TdxDocumentLogPosition;
begin
  ASelectionInterval := DocumentModel.Selection.Interval;
  AStart := ASelectionInterval.Start;
  AEnd := ASelectionInterval.&End;
  AStartLogPosition := AStart.LogPosition;
  AEndLogPosition := AEnd.LogPosition;

  ATextFilter := ActivePieceTable.NavigationVisibleTextFilter;
  if not ATextFilter.IsRunVisible(AStart.RunIndex) then
    AStartLogPosition := ATextFilter.GetVisibleLogPosition(AStart);
  if not ATextFilter.IsRunVisible(AEnd.RunIndex) then
    AEndLogPosition := ATextFilter.GetVisibleLogPosition(AEnd);
  DocumentModel.Selection.Start := AStartLogPosition;
  DocumentModel.Selection.&End := AEndLogPosition;
end;

procedure TdxToggleShowWhitespaceCommand.ExecuteCore;
begin
    DocumentModel.BeginUpdate;
    try
      DocumentModel.FormattingMarkVisibilityOptions.ShowHiddenText := not DocumentModel.FormattingMarkVisibilityOptions.ShowHiddenText;
      EnsureSelectionVisible;
    finally
      DocumentModel.EndUpdate;
    end;
end;

class function TdxToggleShowWhitespaceCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleWhitespaceDescription);
end;

class function TdxToggleShowWhitespaceCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleWhitespaceMenuCaption);
end;

class function TdxToggleShowWhitespaceCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleShowWhitespace;
end;

class function TdxToggleShowWhitespaceCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleShowWhitespace;
end;

procedure TdxToggleShowWhitespaceCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := True;
  AState.Visible := True;
  AState.Checked := DocumentModel.FormattingMarkVisibilityOptions.ShowHiddenText;
end;

{ TdxChangeIndentCommand }

class function TdxChangeIndentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeIndentDescription);
end;

function TdxChangeIndentCommand.GetEndIndex: TdxParagraphIndex;
begin
  Result := DocumentModel.Selection.Interval.NormalizedEnd.ParagraphIndex;
end;

function TdxChangeIndentCommand.GetEndParagraphIndex: TdxParagraphIndex;
var
  ASelection: TdxSelection;
begin
  ASelection := DocumentModel.Selection;
  if (ASelection.NormalizedEnd > 0) and (ASelection.NormalizedEnd <> ASelection.NormalizedStart) then
    Result := ActivePieceTable.FindParagraphIndex(ASelection.NormalizedEnd - 1, False)
  else
    Result := ActivePieceTable.FindParagraphIndex(ASelection.NormalizedEnd, False);
end;

class function TdxChangeIndentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeIndentMenuCaption);
end;

function TdxChangeIndentCommand.GetStartIndex: TdxParagraphIndex;
begin
  Result := DocumentModel.Selection.Interval.NormalizedStart.ParagraphIndex;
end;

function TdxChangeIndentCommand.GetStartParagraphLayoutPosition: TdxDocumentLayoutPosition;
var
  AParagraph: TdxParagraph;
begin
  AParagraph := ActivePieceTable.Paragraphs[StartIndex];
  Result := TdxDocumentLayoutPosition(ActiveView.DocumentLayout.CreateLayoutPosition(ActivePieceTable,
    AParagraph.LogPosition, 0));
  Result.Update(ActiveView.DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Row);
end;

function TdxChangeIndentCommand.SelectedFirstParagraphInList: Boolean;
var
  I: Integer;
  AParagraph: TdxParagraph;
  AListIndex: TdxNumberingListIndex;
  AParagraphs: TdxParagraphCollection;
begin
  AParagraphs := ActivePieceTable.Paragraphs;
  AListIndex := AParagraphs[StartIndex].GetNumberingListIndex;
  if StartIndex = 0 then
    Exit(True);
  for I := StartIndex - 1 downto 0 do
  begin
    AParagraph := AParagraphs[I];
    if AParagraph.IsInList and (AParagraph.GetNumberingListIndex = AListIndex) then
      Exit(False);
  end;
  Result := True;
end;

function TdxChangeIndentCommand.SelectedOnlyParagraphWithNumeration: Boolean;
var
  I: Integer;
  AEndIndex: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
begin
  AParagraphs := ActivePieceTable.Paragraphs;
  AEndIndex := GetEndParagraphIndex;
  for I := StartIndex to AEndIndex do
    if not AParagraphs[I].IsInList then
      Exit(False);
  Result := True;
end;

function TdxChangeIndentCommand.SelectionBeginFirstRowStartPos: Boolean;
var
  AParagraph: TdxParagraph;
begin
  if DocumentModel.Selection.Length = 0 then
  begin
    AParagraph := ActivePieceTable.Paragraphs[StartIndex];
    Result := DocumentModel.Selection.Start = AParagraph.LogPosition;
  end
  else
    Result := False;
end;

procedure TdxChangeIndentCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.ParagraphFormatting);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxIncrementIndentCommand }

procedure TdxIncrementIndentCommand.ExecuteCore;
begin
  if SelectedOnlyParagraphWithNumeration then
    ProcessNumerationParagraph
  else
    IncrementParagraphIndent;
end;

class function TdxIncrementIndentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementIndentDescription);
end;

class function TdxIncrementIndentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.IncrementIndent;
end;

class function TdxIncrementIndentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementIndentMenuCaption);
end;

class function TdxIncrementIndentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.IncreaseIndent;
end;

procedure TdxIncrementIndentCommand.IncrementNumerationFromParagraph;
var
  ACommand: TdxIncrementNumerationFromParagraphCommand;
begin
  ACommand := TdxIncrementNumerationFromParagraphCommand.Create(RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxIncrementIndentCommand.IncrementNumerationParagraphIndent;
var
  ACommand: TdxIncrementNumerationParagraphIndentCommand;
begin
  ACommand := TdxIncrementNumerationParagraphIndentCommand.Create(RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxIncrementIndentCommand.IncrementParagraphIndent;
var
  ACommand: TdxIncrementParagraphLeftIndentCommand;
begin
  ACommand := TdxIncrementParagraphLeftIndentCommand.Create(RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxIncrementIndentCommand.ProcessNumerationParagraph;
begin
  if not SelectedFirstParagraphInList then
    IncrementNumerationFromParagraph
  else
    IncrementNumerationParagraphIndent;
end;

{ TdxDecrementIndentCommand }

procedure TdxDecrementIndentCommand.DecrementNumerationFromParagraph;
var
  ACommand: TdxDecrementNumerationFromParagraphCommand;
begin
  ACommand := TdxDecrementNumerationFromParagraphCommand.Create(RichEditControl);
  try
    ACommand.ForceExecute(CreateDefaultCommandUIState);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDecrementIndentCommand.DecrementNumerationParagraphIndent;
var
  ACommand: TdxDecrementNumerationParagraphIndentCommand;
begin
  ACommand := TdxDecrementNumerationParagraphIndentCommand.Create(RichEditControl);
  try
    ACommand.ForceExecute(CreateDefaultCommandUIState);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDecrementIndentCommand.DecrementParagraphIndent;
var
  ACommand: TdxDecrementParagraphLeftIndentCommand;
begin
  ACommand := TdxDecrementParagraphLeftIndentCommand.Create(RichEditControl);
  try
    ACommand.ForceExecute(CreateDefaultCommandUIState);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDecrementIndentCommand.ExecuteCore;
begin
  if SelectedOnlyParagraphWithNumeration then
    ProcessNumerationParagraph
  else
    DecrementParagraphIndent;
end;

class function TdxDecrementIndentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementIndentDescription);
end;

class function TdxDecrementIndentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementIndentMenuCaption);
end;

class function TdxDecrementIndentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DecreaseIndent;
end;

class function TdxDecrementIndentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.DecrementIndent;
end;

procedure TdxDecrementIndentCommand.ProcessNumerationParagraph;
begin
  if not SelectedFirstParagraphInList then
    DecrementNumerationFromParagraph
  else
    DecrementNumerationParagraphIndent;
end;

{ TdxExtendNextCaretPositionLineUpCalculator }

function TdxExtendNextCaretPositionLineUpCalculator.GetDefaultPosition: TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.FromParagraphStart(ActivePieceTable, ActivePieceTable.Paragraphs.First.Index);
end;

function TdxExtendNextCaretPositionLineUpCalculator.GetSelectedCells: TdxSelectedTableStructureBase;
begin
  Result := Selection.SelectedCells;
end;

function TdxExtendNextCaretPositionLineUpCalculator.GetSelection: TdxSelection;
begin
  Result := Control.InnerControl.DocumentModel.Selection;
end;

procedure TdxExtendNextCaretPositionLineUpCalculator.SetSelectedCells(
  const Value: TdxSelectedTableStructureBase);
begin
  Selection.SelectedCells := Value;
end;

function TdxExtendNextCaretPositionLineUpCalculator.ShouldJumpToNextCellInCurrentTable(
  AShouldJumpToCellInNestedTable: Boolean; ANextCell: TdxTableCellViewInfo;
  AIsLastOrFirstLayoutRowInCell: Boolean): Boolean;
var
  ABaseCondition, ASelectingOneCell: Boolean;
  ASelectedCells: TdxSelectedCellsCollection;
begin
  ABaseCondition := inherited ShouldJumpToNextCellInCurrentTable(AShouldJumpToCellInNestedTable, ANextCell, AIsLastOrFirstLayoutRowInCell);

  if Selection.SelectedCells is TdxStartSelectedCellInTable then
    Exit(ABaseCondition);

  ASelectedCells := TdxSelectedCellsCollection(Selection.SelectedCells);
  ASelectingOneCell := ASelectedCells.SelectedOnlyOneCell;

  if ABaseCondition then
    Exit(True);
  Result := not ASelectingOneCell and (ANextCell <> nil);
end;

{ TdxExtendNextCaretPositionLineDownCalculator }

procedure TdxExtendNextCaretPositionLineDownCalculator.CorrectCaretXOnExtendSelection(
  ASourceCellViewInfo: TdxTableCellViewInfo; var ACaretX: Integer);
begin
  if ASourceCellViewInfo = nil then
    Exit;
  ACaretX := ASourceCellViewInfo.Left + ASourceCellViewInfo.Width - 10;
end;

function TdxExtendNextCaretPositionLineDownCalculator.GetDefaultPosition: TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.FromParagraphEnd(ActivePieceTable, ActivePieceTable.Paragraphs.Last.Index);
end;

function TdxExtendNextCaretPositionLineDownCalculator.MoveFromInnerIntoOuterTable(
  AInnerSourceCellViewInfo: TdxTableCellViewInfo; AColumn: TdxColumn;
  ALastCellInTableRow: TdxTableCellViewInfo; ACaretX: Integer): TdxRow;
begin
  if Selection.SelectedCells is TdxStartSelectedCellInTable then
  begin
    if (Selection.SelectedCells.FirstSelectedCell <> nil) and
      (TObject(ALastCellInTableRow.TableViewInfo.ParentTableCellViewInfo) = TObject(Selection.SelectedCells.FirstSelectedCell)) then
      Selection.SelectedCells := TdxSelectedCellsCollection.Create(TdxStartSelectedCellInTable(Selection.SelectedCells));
  end;
  Result := inherited MoveFromInnerIntoOuterTable(AInnerSourceCellViewInfo, AColumn, ALastCellInTableRow, ACaretX);
end;

function TdxExtendNextCaretPositionLineDownCalculator.ShouldJumpToNextCellInCurrentTable(
  AShouldJumpToCellInNestedTable: Boolean; ANextCell: TdxTableCellViewInfo;
  AIsLastOrFirstLayoutRowInCell: Boolean): Boolean;
var
  ABaseCondition, ASelectingOneCell: Boolean;
  ASelectedCells: TdxSelectedCellsCollection;
begin
  ABaseCondition := inherited ShouldJumpToNextCellInCurrentTable(AShouldJumpToCellInNestedTable,
    ANextCell, AIsLastOrFirstLayoutRowInCell);
  if TObject(Selection.SelectedCells) is TdxStartSelectedCellInTable then
    Exit(ABaseCondition);

  ASelectedCells := TdxSelectedCellsCollection(Selection.SelectedCells);
  ASelectingOneCell := ASelectedCells.SelectedOnlyOneCell;
  Result := ABaseCondition or
    (not ASelectingOneCell and (ANextCell <> nil));
end;

function TdxExtendNextCaretPositionLineDownCalculator.GetSelection: TdxSelection;
begin
  Result := Control.InnerControl.DocumentModel.Selection;
end;

function TdxExtendNextCaretPositionLineDownCalculator.GetSelectedCells: TdxSelectedTableStructureBase;
begin
  Result := Selection.SelectedCells;
end;

procedure TdxExtendNextCaretPositionLineDownCalculator.SetSelectedCells(const Value: TdxSelectedTableStructureBase);
begin
  Selection.SelectedCells := Value;
end;

{ TdxToggleShowRulersCommandBase }

procedure TdxToggleShowRulersCommandBase.ExecuteCore;
var
  ARulerOptions: TdxRulerOptions;
begin
  ARulerOptions := GetModifyOptions;
  if IsRulerVisible(ARulerOptions.Visibility) then
    ARulerOptions.Visibility := TdxRichEditRulerVisibility.Hidden
  else
    ARulerOptions.Visibility := TdxRichEditRulerVisibility.Auto;
end;

procedure TdxToggleShowRulersCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  ARulerOptions: TdxRulerOptions;
  AVisibility: TdxRichEditRulerVisibility;
begin
  AState.Visible := True;
  AState.Enabled := ShowRulerByDefault;

  ARulerOptions := GetModifyOptions;
  AVisibility := ARulerOptions.Visibility;
  AState.Checked := IsRulerVisible(AVisibility);
end;

function TdxToggleShowRulersCommandBase.IsRulerVisible(AVisibility: TdxRichEditRulerVisibility): Boolean;
begin
  Result := (AVisibility = TdxRichEditRulerVisibility.Visible) or (((AVisibility = TdxRichEditRulerVisibility.Auto) and ShowRulerByDefault));
end;

{ TdxToggleShowHorizontalRulerCommand }

class function TdxToggleShowHorizontalRulerCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleShowHorizontalRuler;
end;

class function TdxToggleShowHorizontalRulerCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleShowHorizontalRulerDescription);
end;

class function TdxToggleShowHorizontalRulerCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleShowHorizontalRulerMenuCaption);
end;

class function TdxToggleShowHorizontalRulerCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleShowHorizontalRuler;
end;

function TdxToggleShowHorizontalRulerCommand.GetModifyOptions: TdxRulerOptions;
begin
  Result := Options.HorizontalRuler;
end;

function TdxToggleShowHorizontalRulerCommand.GetShowRulerByDefault: Boolean;
begin
  Result := ActiveView.ShowHorizontalRulerByDefault;
end;

{ TdxToggleShowVerticalRulerCommand }

class function TdxToggleShowVerticalRulerCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleShowVerticalRulerDescription);
end;

class function TdxToggleShowVerticalRulerCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleShowVerticalRulerMenuCaption);
end;

class function TdxToggleShowVerticalRulerCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleShowVerticalRuler;
end;

class function TdxToggleShowVerticalRulerCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleShowVerticalRuler;
end;

function TdxToggleShowVerticalRulerCommand.GetModifyOptions: TdxRulerOptions;
begin
  Result := Options.VerticalRuler;
end;

function TdxToggleShowVerticalRulerCommand.GetShowRulerByDefault: Boolean;
begin
  Result := ActiveView.ShowVerticalRulerByDefault;
end;

{ TdxZoomCommandBase }

procedure TdxZoomCommandBase.ExecuteCore;
begin
  CheckExecutedAtUIThread;

  ZoomFactor := CalculateNewZoomFactor(ZoomFactor);
end;

function TdxZoomCommandBase.GetActualDelta: Single;
begin
  if not Delta.IsNull then
    Result := Delta.Value
  else
    Result := DefaultZoomFactorDelta;
end;

function TdxZoomCommandBase.GetZoomFactor: Single;
begin
  Result := ActiveView.ZoomFactor;
end;

procedure TdxZoomCommandBase.SetZoomFactor(const Value: Single);
begin
  ActiveView.ZoomFactor := Value;
end;

procedure TdxZoomCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandsRestriction(AState, Options.Behavior.Zooming);
end;

{ TdxZoomInCommand }

class function TdxZoomInCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ZoomIn
end;

class function TdxZoomInCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandZoomInDescription);
end;

class function TdxZoomInCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandZoomInMenuCaption);
end;

class function TdxZoomInCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ZoomIn;
end;

function TdxZoomInCommand.CalculateNewZoomFactor(AOldZoomFactor: Single): Single;
var
  AOptions: TdxRichEditBehaviorOptions;
begin
  AOptions := Options.Behavior;
  if AOptions.MaxZoomFactor = AOptions.DefaultMaxZoomFactor then
    Result := AOldZoomFactor + ActualDelta
  else
    Result := Min(AOptions.MaxZoomFactor, AOldZoomFactor + ActualDelta);
end;

{ TdxZoomOutCommand }

class function TdxZoomOutCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandZoomOutDescription);
end;

class function TdxZoomOutCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandZoomOutMenuCaption);
end;

class function TdxZoomOutCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ZoomOut;
end;

class function TdxZoomOutCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ZoomOut;
end;

function TdxZoomOutCommand.CalculateNewZoomFactor(AOldZoomFactor: Single): Single;
begin
  Result := Max(Options.Behavior.MinZoomFactor, AOldZoomFactor - ActualDelta);
end;

{ TdxZoomCommand }

class function TdxZoomCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandZoomDescription);
end;

class function TdxZoomCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandZoomMenuCaption);
end;

class function TdxZoomCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.Zoom;
end;

function TdxZoomCommand.CalculateNewZoomFactor(AOldZoomFactor: Single): Single;
begin
  Result := FZoomFactor;
end;

function TdxZoomCommand.CreateDefaultCommandUIState: IdxCommandUIState;
var
  AState: TdxDefaultValueBasedCommandUIState<Single>;
begin
  AState := TdxDefaultValueBasedCommandUIState<Single>.Create;
  AState.Value := ZoomFactor;
  Result := AState;
end;

procedure TdxZoomCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueState: IdxValueBasedCommandUIState<Single>;
begin
  AValueState := AState as IdxValueBasedCommandUIState<Single>;
  if AValueState <> nil then
    FZoomFactor := AValueState.Value;

  inherited ForceExecute(AState);
end;

procedure TdxZoomCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AValueState: IdxValueBasedCommandUIState<Single>;
begin
  inherited UpdateUIStateCore(AState);
  AValueState := AState as IdxValueBasedCommandUIState<Single>;
  if AValueState <> nil then
    AValueState.Value := ZoomFactor;
end;

{ TdxZoomPercentCommand }

class function TdxZoomPercentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandZoomDescription);
end;

class function TdxZoomPercentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandZoomMenuCaption);
end;

class function TdxZoomPercentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ZoomPercent;
end;

function TdxZoomPercentCommand.CalculateNewZoomFactor(AOldZoomFactor: Single): Single;
begin
  Result := FZoomPercent * 0.01;
end;

function TdxZoomPercentCommand.CreateDefaultCommandUIState: IdxCommandUIState;
var
  AState: TdxDefaultValueBasedCommandUIState<Single>;
begin
  AState := TdxDefaultValueBasedCommandUIState<Single>.Create;
  AState.Value := ZoomFactor * 100;
  Result := AState;
end;

procedure TdxZoomPercentCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueState: IdxValueBasedCommandUIState<Single>;
begin
  AValueState := AState as IdxValueBasedCommandUIState<Single>;
  if AValueState <> nil then
    FZoomPercent := AValueState.Value;

  inherited ForceExecute(AState);
end;

procedure TdxZoomPercentCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  AValueState: IdxValueBasedCommandUIState<Single>;
begin
  inherited UpdateUIStateCore(AState);
  AValueState := AState as IdxValueBasedCommandUIState<Single>;
  if AValueState <> nil then
    AValueState.Value := 100 * ZoomFactor;
end;

{ TdxSwitchActiveViewCommand }

procedure TdxSwitchActiveViewCommand.ExecuteCore;
begin
  InnerControl.ActiveViewType := ViewType;
end;

procedure TdxSwitchActiveViewCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Checked := ActiveViewType = ViewType;
  AState.Enabled := True;
  AState.Visible := True;
end;

{ TdxSwitchToDraftViewCommand }

class function TdxSwitchToDraftViewCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSwitchToDraftViewDescription);
end;

class function TdxSwitchToDraftViewCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSwitchToDraftViewMenuCaption);
end;

class function TdxSwitchToDraftViewCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SwitchToDraftView;
end;

class function TdxSwitchToDraftViewCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SwitchToDraftView;
end;

function TdxSwitchToDraftViewCommand.GetViewType: TdxRichEditViewType;
begin
  Result := TdxRichEditViewType.Draft;
end;

{ TdxSwitchToSimpleViewCommand }

class function TdxSwitchToSimpleViewCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SwitchToSimpleView;
end;

class function TdxSwitchToSimpleViewCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSwitchToSimpleViewDescription);
end;

class function TdxSwitchToSimpleViewCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSwitchToSimpleViewMenuCaption);
end;

class function TdxSwitchToSimpleViewCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SwitchToSimpleView;
end;

function TdxSwitchToSimpleViewCommand.GetViewType: TdxRichEditViewType;
begin
  Result := TdxRichEditViewType.Simple;
end;

{ TdxSwitchToPrintLayoutViewCommand }

class function TdxSwitchToPrintLayoutViewCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSwitchToPrintLayoutViewDescription);
end;

class function TdxSwitchToPrintLayoutViewCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSwitchToPrintLayoutViewMenuCaption);
end;

class function TdxSwitchToPrintLayoutViewCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SwitchToPrintLayoutView;
end;

class function TdxSwitchToPrintLayoutViewCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SwitchToPrintLayoutView;
end;

function TdxSwitchToPrintLayoutViewCommand.GetViewType: TdxRichEditViewType;
begin
  Result := TdxRichEditViewType.PrintLayout;
end;

{ TdxChangePageColorCommand }

function TdxChangePageColorCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultValueBasedCommandUIState<TdxAlphaColor>.Create;
end;

procedure TdxChangePageColorCommand.ExecuteCore;
var
  AProperties: TdxDocumentProperties;
begin
  AProperties := DocumentModel.DocumentProperties;
  AProperties.BeginUpdate;
  try
    AProperties.DisplayBackgroundShape := True;
    AProperties.PageBackColor := FColor;
  finally
    AProperties.EndUpdate;
  end;
end;

procedure TdxChangePageColorCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxAlphaColor>;
begin
  AValueBasedState := AState as IdxValueBasedCommandUIState<TdxAlphaColor>;
  if AValueBasedState = nil then
    Exit;

  FColor := AValueBasedState.Value;
  inherited ForceExecute(AState);
end;

class function TdxChangePageColorCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangePageColorDescription);
end;

class function TdxChangePageColorCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangePageColorMenuCaption);
end;

class function TdxChangePageColorCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangePageColor;
end;

class function TdxChangePageColorCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.FillBackground;
end;

procedure TdxChangePageColorCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable;
end;

end.
