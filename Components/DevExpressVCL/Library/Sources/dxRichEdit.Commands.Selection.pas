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

unit dxRichEdit.Commands.Selection;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses,
  dxGenerics,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.View.Core,
  dxRichEdit.Control.HitTest,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.PieceTableModifiers.Simple,
  dxRichEdit.DocumentModel.PieceTableModifiers,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.DocumentModel.VisibleTextFilter.Simple,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.Commands,
  dxRichEdit.Commands.IDs,
  dxRichEdit.DocumentModel.Section;

type

  TdxMoveNextRowDelegate = reference to function (ARow: TdxTableRow): TdxTableRow;

  TdxVirtualTableColumn = class;
  TdxTableElementAccessorBase = class;
  TdxTableElementAccessorBaseList = class;

  { TdxEnhancedSelectionManager }

  TdxEnhancedSelectionManager = class
  public const
    TextBoxHotZoneWidthInPixels = 5;
  strict private
    FDragSize: TSize;
    FPieceTable: TdxPieceTable;
    FMinDistance: TdxLayoutUnit;
    FMinBorderWidthToHitTest: TdxLayoutUnit;
    function GetDocumentModel: TdxDocumentModel;
  protected
    function GetTextBoxContentBounds(AFloatingObjectBox: TdxFloatingObjectBox; AContent: TdxTextBoxFloatingObjectContent): TRect;
    function GetNormalizedBorderBounds(const ABounds: TRect; ARowViewInfo: TdxTableRowViewInfoBase; ABottomRowViewInfo: TdxTableRowViewInfoBase): TRect; virtual;
    function ShouldAppendSelectedCellsOnly(ASelection: TdxSelection; AElementAccessor: TdxTableElementAccessorBase): Boolean; virtual;
    procedure AppendCells(ATableViewInfo: TdxTableViewInfo; AStartRow: TdxTableRow; AModelHorizontalPositionIndex: Integer;
      AResult: TdxVirtualTableColumn; AAppendSelectedCellsOnly: Boolean; AIsFirstRow: Boolean; const AMoveNextRow: TdxMoveNextRowDelegate); virtual;
    procedure ApplyMaxBorders(AResult: TdxVirtualTableColumn; ATableViewInfo: TdxTableViewInfo; AElement: TdxTableElementAccessorBase); virtual;
    function ShouldAddElement(AElement: TdxTableElementAccessorBase): Boolean;
    function FindElementByHorizontalPosition(ARow: TdxTableRow; AModelHorizontalPosition: Integer): TdxTableElementAccessorBase; virtual;
    function GetWidthConsiderUnitType(AWidthUnit: TdxWidthUnit): Integer; virtual;
    function ExtendSelectionEndToParagraphMark(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition; virtual;
    function GetSelectionEndParagraphIndex(ASelection: TdxSelection): TdxParagraphIndex;
    function ExtendSelectionStartToTableRow(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition; virtual;
    function ExtendSelectionEndToTableRow(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition; virtual;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property DragSize: TSize read FDragSize;
    property PieceTable: TdxPieceTable read FPieceTable;
  public
    constructor Create(APieceTable: TdxPieceTable);

    function CalculateTableRowToResize(AHitTestResult: TdxRichEditHitTestResult): TdxTableRowViewInfoBase; virtual;
    function CalculateTableCellsToResizeHorizontallyCore(ATable: TdxTableViewInfo; ATableRow: TdxTableRow;
      AElementAccessor: TdxTableElementAccessorBase; ABorderLayoutHorizontalPositionIndex, ABorderModelHorizontalPositionIndex: Integer): TdxVirtualTableColumn;
    function CalculateTableCellsToResizeHorizontally(AHitTestResult: TdxRichEditHitTestResult): TdxVirtualTableColumn;
    function ExtendSelectionStartToParagraphMark(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition; virtual;
    function ShouldResizeTableCellsHorizontally(const AControl: IdxRichEditControl; AHitTestResult: TdxRichEditHitTestResult; AVirtualColumn: TdxVirtualTableColumn): Boolean;
    function ShouldResizeTableRow(const AControl: IdxRichEditControl; AHitTestResult: TdxRichEditHitTestResult; ATableRow: TdxTableRowViewInfoBase): Boolean; virtual;
    function ShouldSelectFloatingObject(AHitTestResult: TdxRichEditHitTestResult): Boolean; overload; virtual;
    function ShouldSelectFloatingObject(AFloatingObjectBox: TdxFloatingObjectBox; const ALogicalPoint: TPoint): Boolean; overload;
    function ShouldSelectComment(AHitTestResult: TdxRichEditHitTestResult): Boolean; virtual;
    function ShouldSelectCommentMoreButton(AHitTestResult: TdxRichEditHitTestResult): Boolean; virtual;
    function ShouldSelectEntireTableCell(AHitTestResult: TdxRichEditHitTestResult): Boolean; virtual;
    function ShouldSelectToTheEndOfRow(AHitTestResult: TdxRichEditHitTestResult): Boolean; virtual;
    function ShouldSelectEntireTableColumn(AHitTestResult: TdxRichEditHitTestResult): Boolean; virtual;
    function ShouldSelectEntireTableRow(AHitTestResult: TdxRichEditHitTestResult): Boolean; virtual;
    function ShouldSelectEntireRow(AHitTestResult: TdxRichEditHitTestResult): Boolean; virtual;
  end;

  { TdxExtendSelectionByRangesCommandBase }

  TdxExtendSelectionByRangesCommandBase = class abstract(TdxPlaceCaretToPhysicalPointCommand)
  private
    FInitialBox: TdxBox;
    FInitialLogPosition: TdxDocumentLogPosition;
    procedure SetInitialBox(const Value: TdxBox);
  protected
    function ExtendSelection: Boolean; override;
    function HitTestOnlyInPageClientBounds: Boolean; override;
  public
    property InitialBox: TdxBox read FInitialBox write SetInitialBox;
    property InitialLogPosition: TdxDocumentLogPosition read FInitialLogPosition;
  end;

  { TdxExtendSelectionByTableRowsCommand }

  TdxExtendSelectionByTableRowsCommand = class(TdxExtendSelectionByRangesCommandBase)
  strict private
    FNestingLevel: Integer;
    FStartLogPosition: TdxDocumentLogPosition;
    FEndLogPosition: TdxDocumentLogPosition;
  protected
    function ChangeSelectionEnd(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition; AHitTestResult: TdxRichEditHitTestResult): Boolean; override;
    procedure ChangeSelectionStart(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition; AHitTestResult: TdxRichEditHitTestResult; ASelectionItemCountBeforeChangeEnd: Integer); reintroduce; virtual;
    function GetCurrentTableRow(ALogPosition: TdxDocumentLogPosition): TdxTableRow; virtual;
  public
    constructor Create(const AControl: IdxRichEditControl; ANestingLevel: Integer); reintroduce;

    property StartLogPosition: TdxDocumentLogPosition read FStartLogPosition write FStartLogPosition;
    property EndLogPosition: TdxDocumentLogPosition read FEndLogPosition write FEndLogPosition;
  end;

  { TdxExtendSelectionByStartTableRowsCommand }

  TdxExtendSelectionByStartTableRowsCommand = class(TdxExtendSelectionByTableRowsCommand)
  strict private
    FStartRowIndex: Integer;
  protected
    procedure ChangeSelection(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition; AHitTestResult: TdxRichEditHitTestResult); override;
  public
    constructor Create(const AControl: IdxRichEditControl; ANestingLevel: Integer);

    property StartRowIndex: Integer read FStartRowIndex write FStartRowIndex;
  end;

  { TdxExtendSelectionByTableColumnsCommand }

  TdxExtendSelectionByTableColumnsCommand = class(TdxExtendSelectionByRangesCommandBase)
  strict private
    FNestedLevel: Integer;
    FStartColumnIndex: Integer;
  protected
    procedure ChangeSelection(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition; AHitTestResult: TdxRichEditHitTestResult); override;
  public
    constructor Create(const AControl: IdxRichEditControl; AStartColumnIndex: Integer; ANestedLevel: Integer); reintroduce;
  end;

  { TdxExtendSelectionByCellsCommand }

  TdxExtendSelectionByCellsCommand = class(TdxExtendSelectionByRangesCommandBase)
  strict private
    FStartCell: TdxTableCell;
  protected
    procedure ChangeSelection(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition; AHitTestResult: TdxRichEditHitTestResult); override;
  public
    constructor Create(const AControl: IdxRichEditControl; AStartCell: TdxTableCell); reintroduce;
  end;

  TdxExtendSelectionByCharactersCommand = class(TdxExtendSelectionByRangesCommandBase);

  { TdxExtendSelectionByRangesCommand }

  TdxExtendSelectionByRangesCommand = class(TdxExtendSelectionByRangesCommandBase)
  protected
    function ChangeSelectionEnd(ASelection: TdxSelection; ALogPosition: Integer;
      AHitTestResult: TdxRichEditHitTestResult): Boolean; override;
    procedure ChangeSelectionStart(ASelection: TdxSelection;
      ALogPosition: Integer; AHitTestResult: TdxRichEditHitTestResult; ASelectionItemCountBeforeChangeEnd: Integer); override;

    function CreateIterator: TdxPieceTableIterator; virtual; abstract;
    function ExtendEnd1(ASelection: TdxSelection; AHitTestResult: TdxRichEditHitTestResult): Boolean; virtual;
    function ExtendEnd2(AHitTestResult: TdxRichEditHitTestResult): TdxDocumentLogPosition; virtual;
    function ExtendStart1: TdxDocumentLogPosition; virtual;
    function ExtendStart2: TdxDocumentLogPosition; virtual;
  end;

  { TdxExtendSelectionByWordsCommand }

  TdxExtendSelectionByWordsCommand = class(TdxExtendSelectionByRangesCommand)
  protected
    function CreateIterator: TdxPieceTableIterator; override;
  end;

  { TdxExtendSelectionByParagraphsCommand }

  TdxExtendSelectionByParagraphsCommand = class(TdxExtendSelectionByRangesCommand)
  protected
    function CreateIterator: TdxPieceTableIterator; override;
  end;

  { TdxExtendSelectionByLinesCommand }

  TdxExtendSelectionByLinesCommand = class(TdxExtendSelectionByRangesCommandBase)
  protected
    function ExtendEnd1(ASelection: TdxSelection; AHitTestResult: TdxRichEditHitTestResult): Boolean; virtual;
    function ExtendEnd2(AHitTestResult: TdxRichEditHitTestResult): TdxDocumentLogPosition; virtual;
    function ExtendStart1: TdxDocumentLogPosition; virtual;
    function ExtendStart2: TdxDocumentLogPosition; virtual;
    function ChangeSelectionEnd(ASelection: TdxSelection; ALogPosition: Integer;
      AHitTestResult: TdxRichEditHitTestResult): Boolean; override;
    procedure ChangeSelectionStart(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition;
      AHitTestResult: TdxRichEditHitTestResult; ASelectionItemCountBeforeChangeEnd: Integer); override;
  end;

  { TdxRichEditSelectionCommand }

  TdxRichEditSelectionCommand = class abstract (TdxRichEditCaretBasedCommand)
  private
    FShouldEnsureCaretVisibleVerticallyBeforeUpdate: Boolean;
    FShouldEnsureCaretVisibleVerticallyAfterUpdate: Boolean;
  protected
    function GetTryToKeepCaretX: Boolean; virtual; abstract;
    function GetTreatStartPositionAsCurrent: Boolean; virtual; abstract;
    function GetExtendSelection: Boolean; virtual; abstract;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; virtual; abstract;
    function GetShouldUpdateCaretY: Boolean; virtual;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; virtual; abstract;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; virtual; abstract;
    function CalculateSelectionCurrentPosition(ASelection: TdxSelection): TdxDocumentModelPosition; virtual;
    function ExtendSelectionStartToParagraphMark(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition; virtual;
    function ExtendSelectionEndToParagraphMark(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition; virtual;
    procedure UpdateTableSelectionAfterSelectionUpdated(ALogPosition: TdxDocumentLogPosition); virtual;
    procedure ValidateSelection(ASelection: TdxSelection; AIsSelectionExtended: Boolean); virtual;
    function ApplyNewPositionToSelectionEnd(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition): Boolean; virtual;
    procedure ChangeSelectionStart(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition); virtual;
    procedure BeforeUpdate; virtual;
    procedure AfterUpdate; virtual;
    function PerformChangeSelection: Boolean; virtual;
    procedure EnsureCaretVisibleVertically; virtual;
    function GetSelectionEndParagraphIndex: TdxParagraphIndex; virtual;
    function IsSelectionEndInTableCell: Boolean; virtual;
    function IsSelectionEndAfterTableCell: Boolean; virtual;
    function GetSelectionEndTableCell: TdxTableCell; virtual;
    function CreateEnsureCaretVisibleVerticallyCommand: TdxRichEditCommand; virtual;
    procedure UpdateCaretPosition; reintroduce; overload; virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function GetLeftVisibleLogPosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; virtual;
    procedure EnsureCaretVisible;
    procedure PerformModifyModel; virtual;

    property TryToKeepCaretX: Boolean read GetTryToKeepCaretX;
    property TreatStartPositionAsCurrent: Boolean read GetTreatStartPositionAsCurrent;
    property ExtendSelection: Boolean read GetExtendSelection;
    property UpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel read GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel;
    property ShouldUpdateCaretY: Boolean read GetShouldUpdateCaretY;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    procedure ChangeSelection(ASelection: TdxSelection); virtual;
    procedure ExecuteCore; override;

    property ShouldEnsureCaretVisibleVerticallyBeforeUpdate: Boolean read FShouldEnsureCaretVisibleVerticallyBeforeUpdate write FShouldEnsureCaretVisibleVerticallyBeforeUpdate;
    property ShouldEnsureCaretVisibleVerticallyAfterUpdate: Boolean read FShouldEnsureCaretVisibleVerticallyAfterUpdate write FShouldEnsureCaretVisibleVerticallyAfterUpdate;
  end;

  { TdxSelectFieldNextPrevToCaretCommand }

  TdxSelectFieldNextPrevToCaretCommand = class(TdxRichEditSelectionCommand)
  private
    function GetVisibleTextFilter: IdxVisibleTextFilter;
  protected
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;

    function GetDeletedPosition(ASelection: TdxSelection): TdxDocumentModelPosition; virtual;
    function IsFieldRun(ARun: TdxTextRunBase): Boolean;
    function TryGetVisibleStart(const AStart: TdxDocumentModelPosition; out AResult: TdxDocumentLogPosition): Boolean;
    function TryGetVisibleEnd(const AEnd: TdxDocumentModelPosition; out AResult: TdxDocumentLogPosition): Boolean;

    property VisibleTextFilter: IdxVisibleTextFilter read GetVisibleTextFilter;
  public
    procedure ChangeSelection(ASelection: TdxSelection); override;
    procedure UpdateUIState(const AState: IdxCommandUIState); override;
  end;

  { TdxShrinkSelectionToStartCommand }

  TdxShrinkSelectionToStartCommand = class(TdxRichEditSelectionCommand)
  protected
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSelectFieldPrevToCaretCommand }

  TdxSelectFieldPrevToCaretCommand = class(TdxSelectFieldNextPrevToCaretCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    function GetDeletedPosition(ASelection: TdxSelection): TdxDocumentModelPosition; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSelectFieldNextToCaretCommand }

  TdxSelectFieldNextToCaretCommand = class(TdxSelectFieldNextPrevToCaretCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSelectAllCommand }

  TdxSelectAllCommand = class(TdxRichEditSelectionCommand)
  protected
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    procedure EnsureCaretVisibleVertically; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ChangeSelection(ASelection: TdxSelection); override;
  end;

  { TdxDeselectAllCommand }

  TdxDeselectAllCommand = class(TdxRichEditSelectionCommand)
  protected
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    procedure EnsureCaretVisibleVertically; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ChangeSelection(ASelection: TdxSelection); override;
  end;

  { TdxChangeActivePieceTableCommand }

  TdxChangeActivePieceTableCommand = class(TdxRichEditSelectionCommand)
  strict private
    FNewActivePieceTable: TdxPieceTable;
    FSection: TdxSection;
    FPreferredPageIndex: Integer;
  protected
    procedure AfterUpdate; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function PerformChangeSelection: Boolean; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    procedure SetSelection(AStart: TdxDocumentLogPosition; ALength: Integer); virtual;
  public
    constructor Create(const AControl: IdxRichEditControl; ANewActivePieceTable: TdxPieceTable;
      ASection: TdxSection; APreferredPageIndex: Integer); reintroduce;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ActivatePieceTable(ANewPieceTable: TdxPieceTable; ASection: TdxSection); virtual;
  end;

  { TdxSelectUpperLevelObjectCommand }

  TdxSelectUpperLevelObjectCommand = class(TdxChangeActivePieceTableCommand)
  protected
    function PerformChangeSelection: Boolean; override;
    function ResetCurrentObjectSelection: Boolean; virtual;
    function CanResetCurrentObjectSelection: Boolean; virtual;
    function GetCurrentSectionFromCaretLayoutPosition: TdxSection;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxStartOfLineCommand }

  TdxStartOfLineCommand = class(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendStartOfLineCommand }

  TdxExtendStartOfLineCommand = class(TdxStartOfLineCommand)
  protected
    function GetExtendSelection: Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxEndOfLineCommand }

  TdxEndOfLineCommand = class(TdxRichEditSelectionCommand)
  protected
    function ApplyNewPositionToSelectionEnd(ASelection: TdxSelection;
      ALogPosition: Integer): Boolean; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function ExtendSelectionEndToParagraphMark(ASelection: TdxSelection;
      ALogPosition: Integer): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendEndOfLineCommand }

  TdxExtendEndOfLineCommand = class(TdxEndOfLineCommand)
  protected
    function GetExtendSelection: Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxPreviousCharacterCommand }

  TdxPreviousCharacterCommand = class(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;

    function GetPrevVisibleLogPosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendPreviousCharacterCommand }

  TdxExtendPreviousCharacterCommand = class(TdxPreviousCharacterCommand)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    procedure UpdateTableSelectionAfterSelectionUpdated(ALogPosition: Integer); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxPrevNextWordCommand }

  TdxPrevNextWordCommand = class abstract(TdxRichEditSelectionCommand)
  private
    function GetVisibleTextFilter: IdxVisibleTextFilter;
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;

    function GetNextVisibleLogPosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
    function GetPrevVisibleLogPosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
    function GetModelPosition(const APos: TdxDocumentLogPosition): TdxDocumentModelPosition;

    property VisibleTextFilter: IdxVisibleTextFilter read GetVisibleTextFilter;
  end;

  { TdxNextCharacterCommand }

  TdxNextCharacterCommand = class(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;

    function GetMaxPosition: TdxDocumentLogPosition; virtual;
    function GetNextVisibleLogPosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendNextCharacterCommand }

  TdxExtendNextCharacterCommand = class(TdxNextCharacterCommand)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetMaxPosition: Integer; override;
    procedure UpdateTableSelectionAfterSelectionUpdated(ALogPosition: Integer); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxPreviousWordCommand }

  TdxPreviousWordCommand = class(TdxPrevNextWordCommand)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendPreviousWordCommand }

  TdxExtendPreviousWordCommand = class(TdxPreviousWordCommand)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    procedure UpdateTableSelectionAfterSelectionUpdated(ALogPosition: Integer); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxNextWordCommand }

  TdxNextWordCommand = class(TdxPrevNextWordCommand)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendNextWordCommand }

  TdxExtendNextWordCommand = class(TdxNextWordCommand)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    procedure UpdateTableSelectionAfterSelectionUpdated(ALogPosition: Integer); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxLineUpDownCommandBase }

  TdxLineUpDownCommandBase = class(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;

    function CreateNextCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator; virtual; abstract;
  end;

  { TdxPreviousLineCommand }

  TdxPreviousLineCommand = class(TdxLineUpDownCommandBase)
  protected
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function CreateNextCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendPreviousLineCommand }

  TdxExtendPreviousLineCommand = class(TdxPreviousLineCommand)
  protected
    function CreateNextCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator; override;
    function GetExtendSelection: Boolean; override;
    procedure UpdateTableSelectionAfterSelectionUpdated(ALogPosition: Integer); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxNextLineCommand }

  TdxNextLineCommand = class(TdxLineUpDownCommandBase)
  protected
    function CreateNextCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendNextLineCommand }

  TdxExtendNextLineCommand = class(TdxNextLineCommand)
  protected
    function CreateNextCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator; override;
    function GetExtendSelection: Boolean; override;
    procedure UpdateTableSelectionAfterSelectionUpdated(ALogPosition: Integer); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxPrevNextParagraphCommandBase }

  TdxPrevNextParagraphCommandBase = class(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;

    function GetNextVisiblePosition(ARunIndex: TdxRunIndex): TdxDocumentModelPosition; overload; virtual;
    function GetNextVisiblePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; overload; virtual;
    function GetPrevVisiblePosition(ARunIndex: TdxRunIndex): TdxDocumentModelPosition; overload; virtual;
    function GetPrevVisiblePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; overload; virtual;
    function GetValidLogPosition(const ANewPos: TdxDocumentModelPosition; AParagraph: TdxParagraph): TdxDocumentLogPosition; virtual;
    function GetVisibleLogPosition(AParagraph: TdxParagraph): TdxDocumentLogPosition; virtual;
    function VisibleTextFilter: IdxVisibleTextFilter;
  end;

  TdxPreviousParagraphCommand = class(TdxPrevNextParagraphCommandBase)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetValidLogPosition(const ANewPos: TdxDocumentModelPosition;
      AParagraph: TdxParagraph): Integer; override;

    function GetMinPosition: TdxDocumentLogPosition;
    function GetPrevPosition(AParagraphIndex: TdxParagraphIndex): TdxDocumentLogPosition;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendPreviousParagraphCommand }

  TdxExtendPreviousParagraphCommand = class(TdxPreviousParagraphCommand)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    procedure UpdateTableSelectionAfterSelectionUpdated(ALogPosition: Integer); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxNextParagraphCommand }

  TdxNextParagraphCommand = class(TdxPrevNextParagraphCommandBase)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetMaxPosition: TdxDocumentLogPosition; virtual;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendNextParagraphCommand }

  TdxExtendNextParagraphCommand = class(TdxNextParagraphCommand)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetMaxPosition: Integer; override;
    procedure UpdateTableSelectionAfterSelectionUpdated(ALogPosition: Integer); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxPrevNextPageCommandBase }

  TdxPrevNextPageCommandBase = class abstract(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function CreateEnsureCaretVisibleVerticallyCommand: TdxRichEditCommand; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
  end;

  { TdxPreviousPageCommand }

  TdxPreviousPageCommand = class(TdxPrevNextPageCommandBase)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendPreviousPageCommand }

  TdxExtendPreviousPageCommand = class(TdxPreviousPageCommand)
  protected
    function GetExtendSelection: Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxNextPageCommand }

  TdxNextPageCommand = class(TdxPrevNextPageCommandBase)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;

    function GetLastPosition(APageIndex: Integer): TdxDocumentLogPosition; virtual;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendNextPageCommand }

  TdxExtendNextPageCommand = class(TdxNextPageCommand)
  protected
    function GetExtendSelection: Boolean; override;
    function GetLastPosition(APageIndex: Integer): Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxPrevNextScreenCommandBase }

  TdxPrevNextScreenCommandBase = class abstract(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;

    function CalculateCaretNewLogPosition(const APhysicalPoint: TPoint): TdxDocumentLogPosition;
    function CreateCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator; virtual; abstract;
    function CreatePlaceCaretToPhysicalPointCommand: TdxPlaceCaretToPhysicalPointCommand; virtual;
    function GetDefaultLogPosition: TdxDocumentLogPosition; virtual; abstract;
    function GetTargetRowAtColumn(AColumn: TdxColumn): TdxRow; virtual; abstract;
    function IsNewPositionInCorrectDirection(APreviousLogPosition, ACurrentLogPosition: TdxDocumentLogPosition): Boolean; virtual; abstract;
    function ScrollScreen: Boolean; virtual; abstract;
    function ShouldCalculateNewCaretLogPosition(APreviousLogPosition, ACurrentLogPosition: TdxDocumentLogPosition): Boolean; virtual; abstract;
    function ShouldUseAnotherPage(AColumn: TdxColumn; const ALogicalCaretPoint: TPoint): Boolean; virtual; abstract;
  public
    procedure ChangeSelection(ASelection: TdxSelection); override;
  end;

  { TdxPreviousScreenCommand }

  TdxPreviousScreenCommand = class(TdxPrevNextScreenCommandBase)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function CreateCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator; override;
    function GetDefaultLogPosition: Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTargetRowAtColumn(AColumn: TdxColumn): TdxRow; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function IsNewPositionInCorrectDirection(APreviousLogPosition: Integer;
      ACurrentLogPosition: Integer): Boolean; override;
    function ScrollScreen: Boolean; override;
    function ShouldCalculateNewCaretLogPosition(APreviousLogPosition: Integer;
      ACurrentLogPosition: Integer): Boolean; override;
    function ShouldUseAnotherPage(AColumn: TdxColumn; const ALogicalCaretPoint: TPoint): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendPreviousScreenCommand }

  TdxExtendPreviousScreenCommand = class(TdxPreviousScreenCommand)
  protected
    function CreatePlaceCaretToPhysicalPointCommand: TdxPlaceCaretToPhysicalPointCommand; override;
    function GetExtendSelection: Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxNextScreenCommand }

  TdxNextScreenCommand = class(TdxPrevNextScreenCommandBase)
  protected
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function CreateCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator; override;
    function GetDefaultLogPosition: Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTargetRowAtColumn(AColumn: TdxColumn): TdxRow; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function IsNewPositionInCorrectDirection(APreviousLogPosition: Integer;
      ACurrentLogPosition: Integer): Boolean; override;
    function ScrollScreen: Boolean; override;
    function ShouldUseAnotherPage(AColumn: TdxColumn;
      const ALogicalCaretPoint: TPoint): Boolean; override;
    function ShouldCalculateNewCaretLogPosition(APreviousLogPosition: Integer;
      ACurrentLogPosition: Integer): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendNextScreenCommand }

  TdxExtendNextScreenCommand = class(TdxNextScreenCommand)
  protected
    function CreatePlaceCaretToPhysicalPointCommand: TdxPlaceCaretToPhysicalPointCommand; override;
    function GetExtendSelection: Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxStartOfDocumentCommand }

  TdxStartOfDocumentCommand = class(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendStartOfDocumentCommand }

  TdxExtendStartOfDocumentCommand = class(TdxStartOfDocumentCommand)
  protected
    function GetExtendSelection: Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxEndOfDocumentCommand }

  TdxEndOfDocumentCommand = class(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxExtendEndOfDocumentCommand }

  TdxExtendEndOfDocumentCommand = class(TdxEndOfDocumentCommand)
  protected
    function GetExtendSelection: Boolean; override;
    procedure UpdateTableSelectionAfterSelectionUpdated(ALogPosition: Integer); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

    { TdxSelectionBasedCommandBase }

  TdxSelectionBasedCommandBase = class abstract(TdxRichEditCommand)
  protected
    function CalculateStartPosition(AItem: TdxSelectionItem; AllowSelectionExpanding: Boolean): TdxDocumentModelPosition; virtual;
    function CalculateEndPosition(AItem: TdxSelectionItem; AllowSelectionExpanding: Boolean): TdxDocumentModelPosition; virtual;
  end;

  { TdxExtendSelectionToPhysicalPointCommand }

  TdxExtendSelectionToPhysicalPointCommand = class(TdxPlaceCaretToPhysicalPointCommand)
  protected
    function ExtendSelection: Boolean; override;
  end;

  { TdxSelectionBasedPropertyChangeCommandBase }

  TdxSelectionBasedPropertyChangeCommandBase = class(TdxSelectionBasedCommandBase)
  protected
    function ActivePieceTableObtainParagraphsPropertyValue<T>(ALogPositionStart: TdxDocumentLogPosition;
      ALength: Integer; AModifier: TdxParagraphPropertyModifier<T>; out AValue: T): Boolean;
    function ActivePieceTableObtainRunsPropertyValue<T>(const ALogPositionStart: TdxDocumentLogPosition;
      ALength: Integer; AModifier: TdxRunPropertyModifier<T>; out AValue: T): Boolean;

    function CanEditSelection: Boolean; override;
    function ChangeProperty(const AStart, AEnd: TdxDocumentModelPosition;
      const AState: IdxCommandUIState): TdxDocumentModelChangeActions; virtual; abstract;
    function GetSelectionItems: TdxSelectionItems; virtual;
    procedure ModifyDocumentModel(const AState: IdxCommandUIState); virtual;
    procedure ModifyDocumentModelCore(const AState: IdxCommandUIState); virtual;
    function ValidateSelectionInterval(AItem: TdxSelectionItem): Boolean; virtual;
    function ValidateUIState(const AState: IdxCommandUIState): Boolean; virtual;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
  end;

  { TdxSelectLineCommand }

  TdxSelectLineCommand = class(TdxPlaceCaretToPhysicalPointCommand)
  protected
    function ExtendSelection: Boolean; override;
    function HitTestOnlyInPageClientBounds: Boolean; override;
    function ChangeSelectionEnd(ASelection: TdxSelection; ALogPosition: Integer;
      AHitTestResult: TdxRichEditHitTestResult): Boolean; override;
    procedure ChangeSelectionStart(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition;
      AHitTestResult: TdxRichEditHitTestResult; ASelectionItemCountBeforeChangeEnd: Integer); override;
    procedure ChangeSelection(ASelection: TdxSelection; ALogPosition: Integer;
      AHitTestResult: TdxRichEditHitTestResult); override;
  end;

  { TdxSimpleSetSelectionCommand }

  TdxSimpleSetSelectionCommand = class(TdxRichEditMenuItemSimpleCommand)
  strict private
    FPos: PdxDocumentModelPosition;
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    property Position: PdxDocumentModelPosition read FPos write FPos;
  end;

  { TdxTableElementAccessorBase }

  TdxTableElementAccessorBase = class abstract
  protected
    function GetRow: TdxTableRow; virtual; abstract;
    function GetLayoutColumnSpan: Integer; virtual; abstract;
    function GetModelColumnSpan: Integer; virtual; abstract;
    procedure SetModelColumnSpan(const AValue: Integer); virtual; abstract;
    function GetPreferredWidth: TdxWidthUnitInfo; virtual; abstract;
    procedure SetPreferredWidth(const AValue: TdxWidthUnitInfo); virtual; abstract;
    function IsElementSelected(ASelectedCells: TdxSelectedCellsCollection): Boolean; virtual; abstract;
    function CoercePreferredWidth(AInfo: TdxWidthUnitInfo): TdxWidthUnitInfo; virtual;
    function IsNextElementSelected(ASelectedCells: TdxSelectedCellsCollection): Boolean; virtual;
  public
    destructor Destroy; override;

    function GetNextElement: TdxTableElementAccessorBase; virtual; abstract;
    function GetStartColumnIndex: Integer; virtual; abstract;
    function GetVerticalSpanElements: TdxTableElementAccessorBaseList; virtual; abstract;
    function GetEndColumnIndex: Integer; virtual;
    function IsRightBoundarySelected(ASelectedCells: TdxSelectedCellsCollection): Boolean; virtual;
    function GetMinContentWidth: Integer; virtual; abstract;
    function GetMinRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer; virtual; abstract;
    function GetMaxRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer; virtual; abstract;
    function GetRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer; virtual; abstract;

    property Row: TdxTableRow read GetRow;
    property LayoutColumnSpan: Integer read GetLayoutColumnSpan;
    property ModelColumnSpan: Integer read GetModelColumnSpan write SetModelColumnSpan;
    property PreferredWidth: TdxWidthUnitInfo read GetPreferredWidth write SetPreferredWidth;
  end;
  TdxTableElementAccessorBaseList = class(TdxFastObjectList)
  private
    function GetItem(Index: Integer): TdxTableElementAccessorBase;
  public
    constructor Create(AOwnsObjects: Boolean = False); reintroduce;
    property Items[Index: Integer]: TdxTableElementAccessorBase read GetItem; default;
  end;

  { TdxTableRowAccessorBase }

  TdxTableRowAccessorBase = class abstract(TdxTableElementAccessorBase)
  strict private
    FRow: TdxTableRow;
  protected
    function GetRow: TdxTableRow; override;
  public
    constructor Create(ARow: TdxTableRow);
    function GetVerticalSpanElements: TdxTableElementAccessorBaseList; override;
    function GetMinContentWidth: Integer; override;
  end;

  { TdxTableCellAccessor }

  TdxTableCellAccessor = class(TdxTableElementAccessorBase)
  strict private
    FCell: TdxTableCell;
  protected
    function GetRow: TdxTableRow; override;
    function GetLayoutColumnSpan: Integer; override;
    function GetModelColumnSpan: Integer; override;
    procedure SetModelColumnSpan(const AValue: Integer); override;
    function GetPreferredWidth: TdxWidthUnitInfo; override;
    procedure SetPreferredWidth(const AValue: TdxWidthUnitInfo); override;
    function IsElementSelected(ASelectedCells: TdxSelectedCellsCollection): Boolean; override;
  public
    constructor Create(ACell: TdxTableCell);
    function GetNextElement: TdxTableElementAccessorBase; override;
    function Equals(AObj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function GetStartColumnIndex: Integer; override;
    function GetVerticalSpanElements: TdxTableElementAccessorBaseList; override;
    function GetMinContentWidth: Integer; override;
    function GetMinRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer; override;
    function GetMaxRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer; override;
    function GetRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer; override;

    property Cell: TdxTableCell read FCell;
  end;

  { TdxTableRowBeforeAccessor }

  TdxTableRowBeforeAccessor = class(TdxTableRowAccessorBase)
  protected
    function GetLayoutColumnSpan: Integer; override;
    function GetModelColumnSpan: Integer; override;
    procedure SetModelColumnSpan(const AValue: Integer); override;
    function GetPreferredWidth: TdxWidthUnitInfo; override;
    procedure SetPreferredWidth(const AValue: TdxWidthUnitInfo); override;
    function IsElementSelected(ASelectedCells: TdxSelectedCellsCollection): Boolean; override;
  public
    function GetNextElement: TdxTableElementAccessorBase; override;
    function Equals(AObj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function GetStartColumnIndex: Integer; override;
    function GetMinRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer; override;
    function GetMaxRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer; override;
    function GetRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer; override;
  end;

  { TdxTableRowAfterAccessor }

  TdxTableRowAfterAccessor = class(TdxTableRowAccessorBase)
  protected
    function GetLayoutColumnSpan: Integer; override;
    function GetModelColumnSpan: Integer; override;
    procedure SetModelColumnSpan(const AValue: Integer); override;
    function GetPreferredWidth: TdxWidthUnitInfo; override;
    procedure SetPreferredWidth(const AValue: TdxWidthUnitInfo); override;
    function IsElementSelected(ASelectedCells: TdxSelectedCellsCollection): Boolean; override;
  public
    function GetNextElement: TdxTableElementAccessorBase; override;
    function Equals(AObj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function GetStartColumnIndex: Integer; override;
    function GetMinRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer; override;
    function GetMaxRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer; override;
    function GetRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer; override;
  end;

  { TdxVirtualTableColumn }

  TdxVirtualTableColumn = class
  strict private
    FElements: TdxTableElementAccessorBaseList;
    FMaxLeftBorder: Integer;
    FPosition: Integer;
    FMaxRightBorder: Integer;
    FTableViewInfo: TdxTableViewInfo;
    procedure SetTableViewInfo(const Value: TdxTableViewInfo);
  public
    constructor Create;
    destructor Destroy; override;

    property Elements: TdxTableElementAccessorBaseList read FElements;
    property TableViewInfo: TdxTableViewInfo read FTableViewInfo write SetTableViewInfo;
    property MaxLeftBorder: Integer read FMaxLeftBorder write FMaxLeftBorder;
    property Position: Integer read FPosition write FPosition;
    property MaxRightBorder: Integer read FMaxRightBorder write FMaxRightBorder;
  end;

implementation

uses
  Windows, Contnrs, Math, Controls, RTLConsts,
  dxTypeHelpers,

  dxRichEdit.Commands.Images,
  dxRichEdit.Utils.Graphics,
  dxCharacters,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.Tables.Core,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.Selections.Core,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Commands.Tables,
  dxRichEdit.View.PageViewInfoGenerator,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.LayoutEngine.Formatter;

{ TdxEnhancedSelectionManager }

constructor TdxEnhancedSelectionManager.Create(APieceTable: TdxPieceTable);
begin
  inherited Create;
  Assert(APieceTable <> nil);
  FPieceTable := APieceTable;
  FMinDistance := DocumentModel.LayoutUnitConverter.TwipsToLayoutUnits(14);
  FMinBorderWidthToHitTest := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(4, DocumentModel.DpiX);
  FDragSize.Init(GetSystemMetrics(SM_CXDRAG), GetSystemMetrics(SM_CYDRAG));
end;

function TdxEnhancedSelectionManager.GetDocumentModel: TdxDocumentModel;
begin
  Result := FPieceTable.DocumentModel;
end;

function TdxEnhancedSelectionManager.ShouldSelectFloatingObject(AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  AFloatingObjectBox: TdxFloatingObjectBox;
begin
  if AHitTestResult.DetailsLevel < TdxDocumentLayoutDetailsLevel.Page then
    Exit(False);
  AFloatingObjectBox := AHitTestResult.FloatingObjectBox;
  if AFloatingObjectBox = nil then
    Exit(False);
  Result := ShouldSelectFloatingObject(AFloatingObjectBox, AHitTestResult.LogicalPoint);
end;

function TdxEnhancedSelectionManager.ShouldSelectFloatingObject(AFloatingObjectBox: TdxFloatingObjectBox; const ALogicalPoint: TPoint): Boolean;
var
  ARun: TdxFloatingObjectAnchorRun;
  APoint: TPoint;
begin
  ARun := AFloatingObjectBox.GetFloatingObjectRun;
  if (ARun = nil) or (AFloatingObjectBox.DocumentLayout = nil) then
    Exit(True);

  if not (ARun.Content is TdxTextBoxFloatingObjectContent) then
    Exit(True);

  APoint := AFloatingObjectBox.TransformPointBackward(ALogicalPoint);
  Result := not GetTextBoxContentBounds(AFloatingObjectBox, TdxTextBoxFloatingObjectContent(ARun.Content)).Contains(APoint);
end;

function TdxEnhancedSelectionManager.GetTextBoxContentBounds(AFloatingObjectBox: TdxFloatingObjectBox; AContent: TdxTextBoxFloatingObjectContent): TRect;
var
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  ALocation: TPoint;
  ATextBoxProperties: TdxTextBoxProperties;
  AHotZoneWidth, AHotZoneHeight, ALeftMargin, ATopMargin, ARightMargin, ABottomMargin: Integer;
  ASize: TSize;
begin
  AUnitConverter := FPieceTable.DocumentModel.ToDocumentLayoutUnitConverter;

  ALocation := AFloatingObjectBox.ContentBounds.Location;
  ATextBoxProperties := AContent.TextBoxProperties;
  AHotZoneWidth := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(TextBoxHotZoneWidthInPixels);
  AHotZoneHeight := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(TextBoxHotZoneWidthInPixels);

  ALeftMargin := Max(AHotZoneWidth, AUnitConverter.ToLayoutUnits(ATextBoxProperties.LeftMargin));
  ATopMargin := Max(AHotZoneHeight, AUnitConverter.ToLayoutUnits(ATextBoxProperties.TopMargin));
  ARightMargin := Max(AHotZoneWidth, AUnitConverter.ToLayoutUnits(ATextBoxProperties.RightMargin));
  ABottomMargin := Max(AHotZoneHeight, AUnitConverter.ToLayoutUnits(ATextBoxProperties.BottomMargin));

  Inc(ALocation.X, ALeftMargin);
  Inc(ALocation.Y, ATopMargin);

  ASize := AFloatingObjectBox.ContentBounds.Size;
  Dec(ASize.cx, ALeftMargin + ARightMargin);
  Dec(ASize.cy, ATopMargin + ABottomMargin);

  ASize.Width := Max(ASize.Width, 50);
  ASize.Height := Max(ASize.Height, 10);
  Result.InitSize(ALocation, ASize);
end;

function TdxEnhancedSelectionManager.ShouldSelectComment(AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  ACommentViewInfo: TdxCommentViewInfo;
begin
  if AHitTestResult.DetailsLevel < TdxDocumentLayoutDetailsLevel.Page then
    Exit(False);
  ACommentViewInfo := AHitTestResult.CommentViewInfo;
  Result := ACommentViewInfo <> nil;
end;

function TdxEnhancedSelectionManager.ShouldSelectCommentMoreButton(AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  ACommentLocation: TdxCommentLocationType;
begin
  if AHitTestResult.DetailsLevel < TdxDocumentLayoutDetailsLevel.Page then
    Exit(False);

  ACommentLocation := AHitTestResult.CommentLocation;
  Result := ACommentLocation = TdxCommentLocationType.CommentMoreButton;
end;

function TdxEnhancedSelectionManager.ShouldSelectToTheEndOfRow(AHitTestResult: TdxRichEditHitTestResult): Boolean;
begin
  if AHitTestResult.PieceTable <> FPieceTable then
    Exit(False);

  if (AHitTestResult.Accuracy and ExactCharacter) = 0 then
    if AHitTestResult.Character.Bounds.Right < AHitTestResult.LogicalPoint.X then
      Exit(True);

  Result := False;
end;

function TdxEnhancedSelectionManager.ShouldSelectEntireRow(AHitTestResult: TdxRichEditHitTestResult): Boolean;
begin
  if AHitTestResult.PieceTable <> FPieceTable then
    Exit(False);

  if (AHitTestResult.Accuracy and ExactCharacter) = 0 then
    if AHitTestResult.Row.Bounds.Left > AHitTestResult.LogicalPoint.X then
      Exit(True);

  Result := False;
end;

function TdxEnhancedSelectionManager.CalculateTableRowToResize(AHitTestResult: TdxRichEditHitTestResult): TdxTableRowViewInfoBase;
var
  ACell: TdxTableCellViewInfo;
  ATable: TdxTableViewInfo;
  AAnchor: TdxTableCellVerticalAnchor;
  ABounds: TRect;
begin
  if (AHitTestResult.Accuracy and ExactTableCell) = 0 then
    Exit(nil);

  ACell := AHitTestResult.TableCell;
  ATable := ACell.TableViewInfo;
  AAnchor := ATable.Anchors[ACell.TopAnchorIndex];
  ABounds := ATable.GetCellBounds(ACell);
  ABounds.Y := AAnchor.VerticalPosition;
  ABounds.Height := Math.Max(AAnchor.BottomTextIndent, FMinBorderWidthToHitTest);

  if ABounds.Contains(AHitTestResult.LogicalPoint) then
    Exit(AHitTestResult.TableRow.Previous);

  if (AHitTestResult.TableRow.Row = ATable.Table.Rows.Last) and (ATable.NextTableViewInfo = nil) then
  begin
    ABounds.Y := ATable.Anchors.Last.VerticalPosition;
    ABounds.Height := Math.Max(AAnchor.BottomTextIndent, FMinBorderWidthToHitTest);
    if ABounds.Contains(AHitTestResult.LogicalPoint) then
      Exit(AHitTestResult.TableRow);
  end;
  Result := nil;
end;

function TdxEnhancedSelectionManager.ShouldResizeTableRow(const AControl: IdxRichEditControl; AHitTestResult: TdxRichEditHitTestResult; ATableRow: TdxTableRowViewInfoBase): Boolean;
 var
   ACommand: TdxChangeTableRowHeightCommand;
begin
  if ATableRow = nil then
    Exit(False);

  if AHitTestResult.PieceTable <> FPieceTable then
    Exit(False);

  ACommand := TdxChangeTableRowHeightCommand.Create(AControl, ATableRow.Row, 100);
  try
    Result := ACommand.CanExecute;
  finally
    ACommand.Free;
  end;
end;

function TdxEnhancedSelectionManager.CalculateTableCellsToResizeHorizontally(AHitTestResult: TdxRichEditHitTestResult): TdxVirtualTableColumn;
var
  ACell, APrevCell: TdxTableCellViewInfo;
  ATable: TdxTableViewInfo;
  ATableCell: TdxTableCell;
  ATableRow: TdxTableRow;
  ATopRowViewInfoIndex, ABottomRowViewInfoIndex, AStartLayoutHorizontalPositionIndex, AStartModelHorizontalPositionIndex, AEndLayoutHorizontalPositionIndex, AEndModelHorizontalPositionIndex, ABorderLayoutHorizontalPositionIndex, ABorderModelHorizontalPositionIndex: Integer;
  ARowViewInfo, ABottomRowViewInfo: TdxTableRowViewInfoBase;
  ABounds: TRect;
  AElementAccessor: TdxTableElementAccessorBase;
  AIsFirstCellInRow: Boolean;
begin
  if (AHitTestResult.Accuracy and ExactTableCell) = 0 then
    Exit(nil);

  ACell := AHitTestResult.TableCell;
  ATable := ACell.TableViewInfo;
  ATableCell := ACell.Cell;
  ATableRow := ATableCell.Row;
  ATopRowViewInfoIndex := Math.Max(ACell.StartRowIndex - ACell.TableViewInfo.TopRowIndex, 0);
  ABottomRowViewInfoIndex := ATopRowViewInfoIndex + ACell.BottomAnchorIndex - ACell.TopAnchorIndex - 1;

  ARowViewInfo := ATable.Rows[ATopRowViewInfoIndex];
  ABottomRowViewInfo := ATable.Rows[ABottomRowViewInfoIndex];
  AStartLayoutHorizontalPositionIndex := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(ATableCell, True);
  AStartModelHorizontalPositionIndex := ATableCell.GetStartColumnIndexConsiderRowGrid;
  AEndLayoutHorizontalPositionIndex := AStartLayoutHorizontalPositionIndex + ATableCell.LayoutProperties.ColumnSpan;
  AEndModelHorizontalPositionIndex := ATableCell.GetStartColumnIndexConsiderRowGrid + ATableCell.ColumnSpan;

  ABounds := ARowViewInfo.GetVerticalBorderBounds(AEndLayoutHorizontalPositionIndex);
  ABounds := GetNormalizedBorderBounds(ABounds, ARowViewInfo, ABottomRowViewInfo);
  if not ABounds.Contains(AHitTestResult.LogicalPoint) then
  begin
    ABounds := ARowViewInfo.GetVerticalBorderBounds(AStartLayoutHorizontalPositionIndex);
    ABounds := GetNormalizedBorderBounds(ABounds, ARowViewInfo, ABottomRowViewInfo);
    if not ABounds.Contains(AHitTestResult.LogicalPoint) then
      Exit(nil);
    ABorderModelHorizontalPositionIndex := AStartModelHorizontalPositionIndex;
    ABorderLayoutHorizontalPositionIndex := AStartLayoutHorizontalPositionIndex;
    AIsFirstCellInRow := ARowViewInfo.Cells.First = ACell;
    if AIsFirstCellInRow then
      AElementAccessor := TdxTableRowBeforeAccessor.Create(ATableCell.Row)
    else
    begin
      APrevCell := ARowViewInfo.Cells[ARowViewInfo.Cells.IndexOf(ACell) - 1];
      AElementAccessor := TdxTableCellAccessor.Create(APrevCell.Cell);
    end;
  end
  else
  begin
    AElementAccessor := TdxTableCellAccessor.Create(ATableCell);
    ABorderLayoutHorizontalPositionIndex := AEndLayoutHorizontalPositionIndex;
    ABorderModelHorizontalPositionIndex := AEndModelHorizontalPositionIndex;
  end;
  Result := CalculateTableCellsToResizeHorizontallyCore(ATable, ATableRow, AElementAccessor, ABorderLayoutHorizontalPositionIndex, ABorderModelHorizontalPositionIndex);
end;

function TdxEnhancedSelectionManager.GetNormalizedBorderBounds(const ABounds: TRect; ARowViewInfo: TdxTableRowViewInfoBase; ABottomRowViewInfo: TdxTableRowViewInfoBase): TRect;
var
  ADelta: TdxLayoutUnit;
begin
  Result := ABounds;
  if ABottomRowViewInfo <> ARowViewInfo then
    Result.Height := ABottomRowViewInfo.BottomAnchor.VerticalPosition - ABounds.Top;
  if ABounds.Width < FMinBorderWidthToHitTest then
  begin
    ADelta := FMinBorderWidthToHitTest - ABounds.Width;
    Result.X := Result.X - ADelta;
    Result.Width := FMinBorderWidthToHitTest;
  end;
end;

function TdxEnhancedSelectionManager.CalculateTableCellsToResizeHorizontallyCore(
  ATable: TdxTableViewInfo; ATableRow: TdxTableRow; AElementAccessor: TdxTableElementAccessorBase;
  ABorderLayoutHorizontalPositionIndex, ABorderModelHorizontalPositionIndex: Integer): TdxVirtualTableColumn;
var
  AAppendSelectedCellsOnly, AIsFirstRow: Boolean;
  AMoveTop, AMoveBottom: TdxMoveNextRowDelegate;
begin
  Result := TdxVirtualTableColumn.Create;
  Result.Position := ATable.GetAlignedPosition(ABorderLayoutHorizontalPositionIndex);
  Result.MaxLeftBorder := MinInt;
  Result.MaxRightBorder := MaxInt;
  ApplyMaxBorders(Result, ATable, AElementAccessor);
  Result.TableViewInfo := ATable;
  Result.Elements.Add(AElementAccessor);
  AAppendSelectedCellsOnly := ShouldAppendSelectedCellsOnly(DocumentModel.Selection, AElementAccessor);
  AIsFirstRow := ATableRow.Table.Rows.First = ATableRow;
  AMoveTop :=
    function (ARow: TdxTableRow): TdxTableRow
    begin
      Result := ARow.Previous;
    end;
  AMoveBottom :=
    function (ARow: TdxTableRow): TdxTableRow
    begin
      Result := ARow.Next;
    end;
  AppendCells(ATable, ATableRow, ABorderModelHorizontalPositionIndex, Result, AAppendSelectedCellsOnly, AIsFirstRow, AMoveTop);
  AppendCells(ATable, ATableRow, ABorderModelHorizontalPositionIndex, Result, AAppendSelectedCellsOnly, AIsFirstRow, AMoveBottom);
end;

function TdxEnhancedSelectionManager.ShouldAppendSelectedCellsOnly(ASelection: TdxSelection; AElementAccessor: TdxTableElementAccessorBase): Boolean;
begin
  if not ASelection.IsWholeSelectionInOneTable then
    Exit(False);

  Result := ShouldAddElement(AElementAccessor);
end;

procedure TdxEnhancedSelectionManager.AppendCells(ATableViewInfo: TdxTableViewInfo;
  AStartRow: TdxTableRow; AModelHorizontalPositionIndex: Integer;
  AResult: TdxVirtualTableColumn; AAppendSelectedCellsOnly: Boolean;
  AIsFirstRow: Boolean; const AMoveNextRow: TdxMoveNextRowDelegate);
var
  ARowAlignment: TdxTableRowAlignment;
  ARow: TdxTableRow;
  AElement: TdxTableElementAccessorBase;
begin
  ARowAlignment := AStartRow.TableRowAlignment;
  ARow := AMoveNextRow(AStartRow);
  while ARow <> nil do
  begin
    if AResult.MaxLeftBorder = AResult.MaxRightBorder then
      Break;
    AElement := FindElementByHorizontalPosition(ARow, AModelHorizontalPositionIndex);
    if (AElement = nil) or (ARow.TableRowAlignment <> ARowAlignment) then
    begin
      AElement.Free;
      if not AAppendSelectedCellsOnly and not AIsFirstRow then
        Exit
      else
      begin
        ARow := AMoveNextRow(ARow);
        Continue;
      end;
    end;
    if not AAppendSelectedCellsOnly or ShouldAddElement(AElement) then
    begin
      AResult.Elements.Add(AElement);
      ApplyMaxBorders(AResult, ATableViewInfo, AElement);
    end
    else
      AElement.Free;
    ARow := AMoveNextRow(ARow);
  end;
end;

procedure TdxEnhancedSelectionManager.ApplyMaxBorders(AResult: TdxVirtualTableColumn; ATableViewInfo: TdxTableViewInfo;
  AElement: TdxTableElementAccessorBase);
var
  AMiddle: TdxLayoutUnit;
begin
  AResult.MaxLeftBorder := Max(AResult.MaxLeftBorder, AElement.GetMinRightBorderPosition(ATableViewInfo) + FMinDistance);
  AResult.MaxRightBorder := Min(AResult.MaxRightBorder, AElement.GetMaxRightBorderPosition(ATableViewInfo) - FMinDistance);
  if AResult.MaxLeftBorder > AResult.MaxRightBorder then
  begin
    AMiddle := (AResult.MaxRightBorder + AResult.MaxLeftBorder) div 2;
    AResult.MaxLeftBorder := AMiddle;
    AResult.MaxRightBorder := AMiddle;
  end;
end;

function TdxEnhancedSelectionManager.ShouldAddElement(AElement: TdxTableElementAccessorBase): Boolean;
var
  ASelection: TdxSelection;
  ASelectedCells: TdxSelectedCellsCollection;
begin
  ASelection := DocumentModel.Selection;
  Assert(ASelection.IsWholeSelectionInOneTable);
  ASelectedCells := Safe<TdxSelectedCellsCollection>.Cast(ASelection.SelectedCells);
  if ASelectedCells <> nil then
    Result := AElement.IsRightBoundarySelected(ASelectedCells)
  else
    Result := False;
end;

function TdxEnhancedSelectionManager.FindElementByHorizontalPosition(ARow: TdxTableRow; AModelHorizontalPosition: Integer): TdxTableElementAccessorBase;
var
  ACells: TdxTableCellCollection;
  ACount, I: Integer;
  ACell: TdxTableCell;
begin
  Dec(AModelHorizontalPosition, ARow.GridBefore);
  if AModelHorizontalPosition = 0 then
    Exit(TdxTableRowBeforeAccessor.Create(ARow));
  ACells := ARow.Cells;
  ACount := ACells.Count;
  for I := 0 to ACount - 1 do
  begin
    if AModelHorizontalPosition < 0 then
      Exit(nil);
    ACell := ACells[I];
    Dec(AModelHorizontalPosition, ACell.ColumnSpan);
    if AModelHorizontalPosition = 0 then
      Exit(TdxTableCellAccessor.Create(ACell));
  end;
  Result := nil;
end;

function TdxEnhancedSelectionManager.ShouldResizeTableCellsHorizontally(const AControl: IdxRichEditControl;
  AHitTestResult: TdxRichEditHitTestResult; AVirtualColumn: TdxVirtualTableColumn): Boolean;
var
  ACommand: TdxChangeTableVirtualColumnRightCommand;
begin
  if AVirtualColumn = nil then
    Exit(False);

  if AVirtualColumn.Elements.Count <= 0 then
    Exit(False);

  if AHitTestResult.PieceTable <> FPieceTable then
    Exit(False);

  ACommand := TdxChangeTableVirtualColumnRightCommand.Create(AControl, AVirtualColumn, 100);
  try
    Result := ACommand.CanExecute;
  finally
    ACommand.Free;
  end;
end;

function TdxEnhancedSelectionManager.ShouldSelectEntireTableColumn(AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  ARowInfo: TdxTableRowViewInfoBase;
  ALogicalY, ATableRowTopPosition, AOffset: Integer;
  ATableRowTopAnchor: TdxTableCellVerticalAnchor;
begin
  if AHitTestResult.PieceTable <> FPieceTable then
    Exit(False);

  if (AHitTestResult.Accuracy and ExactTableCell) = 0 then
    Exit(False);

  ARowInfo := AHitTestResult.TableRow;
  if ARowInfo.Row.IndexInTable > 0 then
    Exit(False);

  ALogicalY := AHitTestResult.LogicalPoint.Y;
  ATableRowTopAnchor := ARowInfo.TopAnchor;
  ATableRowTopPosition := ATableRowTopAnchor.VerticalPosition;
  AOffset := Min(ATableRowTopAnchor.BottomTextIndent + DragSize.cy, ARowInfo.BottomAnchor.VerticalPosition - ATableRowTopPosition);
  Result := (ALogicalY >= ATableRowTopPosition) and (ALogicalY <= ATableRowTopPosition + AOffset);
end;

function TdxEnhancedSelectionManager.ShouldSelectEntireTableRow(AHitTestResult: TdxRichEditHitTestResult): Boolean;
begin
  if AHitTestResult.PieceTable <> FPieceTable then
    Exit(False);

  if (AHitTestResult.Accuracy and ExactTableRow) <> 0 then
    if AHitTestResult.TableRow.Cells.First.Left > AHitTestResult.LogicalPoint.X then
      if AHitTestResult.FloatingObjectBox = nil then
        Exit(True);

  Result := False;
end;

function TdxEnhancedSelectionManager.ShouldSelectEntireTableCell(AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  AAccuracy: TdxHitTestAccuracy;
  ACellViewInfo: TdxTableCellViewInfo;
  ACellViewInfoLeft, ACellMargin, ALogicalX: Integer;
begin
  if AHitTestResult.PieceTable <> FPieceTable then
    Exit(False);

  AAccuracy := AHitTestResult.Accuracy;
  if (AAccuracy and ExactTableCell) <> 0 then
  begin
    ACellViewInfo := AHitTestResult.TableCell;
    ACellViewInfoLeft := ACellViewInfo.Left;
    ACellMargin := GetWidthConsiderUnitType(ACellViewInfo.Cell.GetActualLeftMargin);
    ALogicalX := AHitTestResult.LogicalPoint.X;
    if (ACellViewInfoLeft <= ALogicalX) and (ALogicalX <= ACellViewInfoLeft + ACellMargin) then
      Exit(True);
  end;
  Result := False;
end;

function TdxEnhancedSelectionManager.GetWidthConsiderUnitType(AWidthUnit: TdxWidthUnit): Integer;
begin
  Result := AWidthUnit.Value;
  if AWidthUnit.&Type = TdxWidthUnitType.ModelUnits then
    Result := DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(Result);
end;

function TdxEnhancedSelectionManager.ExtendSelectionStartToParagraphMark(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
var
  ASelectionEndParagraph: TdxParagraphIndex;
  AParagraph: TdxParagraph;
begin
  ASelectionEndParagraph := GetSelectionEndParagraphIndex(ASelection);
  AParagraph := PieceTable.Paragraphs[ASelectionEndParagraph];
  if (ALogPosition <= AParagraph.LogPosition) and (ASelection.Start = AParagraph.LogPosition + AParagraph.Length - 1) then
  begin
    if AParagraph.Length > 1 then
      Exit(ASelection.Start + 1);
  end;
  Result := ASelection.Start;
end;

function TdxEnhancedSelectionManager.ExtendSelectionEndToParagraphMark(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
var
  ASelectionEndParagraph: TdxParagraphIndex;
  AParagraph: TdxParagraph;
begin
  ASelectionEndParagraph := GetSelectionEndParagraphIndex(ASelection);
  AParagraph := PieceTable.Paragraphs[ASelectionEndParagraph];
  if AParagraph.LogPosition + AParagraph.Length - 1 = ALogPosition then
  begin
    if ASelection.NormalizedStart <= AParagraph.LogPosition then
    begin
      if AParagraph.Length > 1 then
      begin
        if ASelection.NormalizedEnd <= PieceTable.DocumentEndLogPosition then
          Exit(ALogPosition + 1);
      end;
    end;
  end;
  Result := ALogPosition;
end;

function TdxEnhancedSelectionManager.GetSelectionEndParagraphIndex(ASelection: TdxSelection): TdxParagraphIndex;
var
  ASelectionEndParagraph, ALastParagraphIndex: TdxParagraphIndex;
begin
  ASelectionEndParagraph := ASelection.Interval.NormalizedEnd.ParagraphIndex;
  ALastParagraphIndex := ASelection.PieceTable.Paragraphs.Count - 1;
  if ASelectionEndParagraph > ALastParagraphIndex then
    Exit(ALastParagraphIndex);
  Result := ASelectionEndParagraph;
end;

function TdxEnhancedSelectionManager.ExtendSelectionStartToTableRow(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
var
  AParagraphs: TdxParagraphCollection;
  AParagraph: TdxParagraph;
  ATableCell: TdxTableCell;
begin
  AParagraphs := PieceTable.Paragraphs;
  AParagraph := AParagraphs[ASelection.Interval.NormalizedStart.ParagraphIndex];
  ATableCell := AParagraph.GetCell;
  if ATableCell = nil then
    Exit(ASelection.Start);
  Result := AParagraphs[ATableCell.Row.FirstCell.StartParagraphIndex].LogPosition;
end;

function TdxEnhancedSelectionManager.ExtendSelectionEndToTableRow(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
var
  AParagraph: TdxParagraph;
  ATableCell: TdxTableCell;
begin
  AParagraph := PieceTable.FindParagraph(ASelection.NormalizedVirtualEnd);
  ATableCell := AParagraph.GetCell;
  if ATableCell = nil then
    Exit(ASelection.&End);

  Result := PieceTable.Paragraphs[ATableCell.Row.LastCell.EndParagraphIndex].EndLogPosition + 1;
end;

{ TdxExtendSelectionByRangesCommandBase }

function TdxExtendSelectionByRangesCommandBase.ExtendSelection: Boolean;
begin
  Result := True;
end;

function TdxExtendSelectionByRangesCommandBase.HitTestOnlyInPageClientBounds: Boolean;
begin
  Result := False;
end;

procedure TdxExtendSelectionByRangesCommandBase.SetInitialBox(
  const Value: TdxBox);
begin
  FInitialBox := Value;
  FInitialLogPosition := InitialBox.GetFirstPosition(ActivePieceTable).LogPosition;
end;

{ TdxExtendSelectionByTableRowsCommand }

constructor TdxExtendSelectionByTableRowsCommand.Create(const AControl: IdxRichEditControl; ANestingLevel: Integer);
begin
  inherited Create(AControl);
  FNestingLevel := ANestingLevel;
end;

function TdxExtendSelectionByTableRowsCommand.ChangeSelectionEnd(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition; AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  AParagraphs: TdxParagraphCollection;
  AActualTableRow: TdxTableRow;
  AStartParagraphIndexInRow, AEndParagraphIndexInRow: TdxParagraphIndex;
begin
  AParagraphs := ActivePieceTable.Paragraphs;
  AActualTableRow := GetCurrentTableRow(ALogPosition);
  if ALogPosition < StartLogPosition then
  begin
    AStartParagraphIndexInRow := AActualTableRow.FirstCell.StartParagraphIndex;
    ASelection.&End := AParagraphs[AStartParagraphIndexInRow].LogPosition;
  end
  else
  begin
    AEndParagraphIndexInRow := AActualTableRow.LastCell.EndParagraphIndex;
    ASelection.&End := AParagraphs[AEndParagraphIndexInRow].EndLogPosition + 1;
  end;
  Result := True;
end;

procedure TdxExtendSelectionByTableRowsCommand.ChangeSelectionStart(ASelection: TdxSelection;
  ALogPosition: TdxDocumentLogPosition; AHitTestResult: TdxRichEditHitTestResult;
  ASelectionItemCountBeforeChangeEnd: Integer);
begin
end;

function TdxExtendSelectionByTableRowsCommand.GetCurrentTableRow(ALogPosition: TdxDocumentLogPosition): TdxTableRow;
var
  AParagraphIndex: TdxParagraphIndex;
begin
  AParagraphIndex := ActivePieceTable.FindParagraphIndex(ALogPosition);
  Result := ActivePieceTable.TableCellsManager.GetCellByNestingLevel(AParagraphIndex, FNestingLevel).Row;
end;

{ TdxExtendSelectionByStartTableRowsCommand }

constructor TdxExtendSelectionByStartTableRowsCommand.Create(const AControl: IdxRichEditControl; ANestingLevel: Integer);
begin
  inherited Create(AControl, ANestingLevel);
  FStartRowIndex := -1;
end;

procedure TdxExtendSelectionByStartTableRowsCommand.ChangeSelection(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition; AHitTestResult: TdxRichEditHitTestResult);
var
  ACommand: TdxSelectTableRowCommand;
  ACurrentTableRow, AStartRow: TdxTableRow;
  ARows: TdxTableRowCollection;
begin
  ACommand := TdxSelectTableRowCommand.Create(RichEditControl);
  try
    ACommand.CanCalculateExecutionParameters := False;
    ACurrentTableRow := GetCurrentTableRow(ALogPosition);
    ARows := ACurrentTableRow.Table.Rows;
    AStartRow := ARows[StartRowIndex];
    if AStartRow.Table <> ACurrentTableRow.Table then
      Exit;
    ACommand.Rows := ARows;
    ACommand.StartRowIndex := StartRowIndex;
    ACommand.EndRowIndex := ACurrentTableRow.IndexInTable;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
  ASelection.SelectedCells.OriginalStartLogPosition := StartLogPosition;
end;

{ TdxExtendSelectionByTableColumnsCommand }

constructor TdxExtendSelectionByTableColumnsCommand.Create(const AControl: IdxRichEditControl; AStartColumnIndex: Integer; ANestedLevel: Integer);
begin
  inherited Create(AControl);
  FNestedLevel := ANestedLevel;
  FStartColumnIndex := AStartColumnIndex;
end;

procedure TdxExtendSelectionByTableColumnsCommand.ChangeSelection(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition; AHitTestResult: TdxRichEditHitTestResult);
var
  AParagraphIndex: TdxParagraphIndex;
  ACurrentTableCell: TdxTableCell;
  AEndColumnIndex, ATemp: Integer;
  ACommand: TdxSelectTableColumnsCommand;
begin
  AParagraphIndex := ActivePieceTable.FindParagraphIndex(ALogPosition);
  ACurrentTableCell := ActivePieceTable.TableCellsManager.GetCellByNestingLevel(AParagraphIndex, FNestedLevel);

  AEndColumnIndex := ACurrentTableCell.GetEndColumnIndexConsiderRowGrid;
  if FStartColumnIndex > AEndColumnIndex then
  begin
    ATemp := FStartColumnIndex;
    FStartColumnIndex := AEndColumnIndex;
    AEndColumnIndex := ATemp;
  end;

  ACommand := TdxSelectTableColumnsCommand.Create(RichEditControl);
  try
    ACommand.Rows := ACurrentTableCell.Table.Rows;
    ACommand.StartColumnIndex := FStartColumnIndex;
    ACommand.EndColumnIndex := AEndColumnIndex;
    ACommand.ChangeSelection(ASelection);
    ACommand.ValidateSelection(ASelection, True);
  finally
    ACommand.Free;
  end;
end;

{ TdxExtendSelectionByCellsCommand }

constructor TdxExtendSelectionByCellsCommand.Create(const AControl: IdxRichEditControl; AStartCell: TdxTableCell);
begin
  inherited Create(AControl);
  Assert(AStartCell <> nil);
  FStartCell := AStartCell;
end;

procedure TdxExtendSelectionByCellsCommand.ChangeSelection(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition; AHitTestResult: TdxRichEditHitTestResult);
var
  AParagraphIndex: TdxParagraphIndex;
  AEndCell: TdxTableCell;
  AValidator: TdxFieldIsSelectValidator;
begin
  AParagraphIndex := ActivePieceTable.FindParagraphIndex(ALogPosition);
  AEndCell := ActivePieceTable.TableCellsManager.GetCellByNestingLevel(AParagraphIndex, FStartCell.Table.NestedLevel);
  if AEndCell.Table.NestedLevel <> FStartCell.Table.NestedLevel then
    FStartCell := ActivePieceTable.TableCellsManager.GetCellByNestingLevel(FStartCell.StartParagraphIndex, AEndCell.Table.NestedLevel);

  while (FStartCell <> nil) and (AEndCell <> nil) do
  begin
    if FStartCell.Table = AEndCell.Table then
    begin
      ASelection.ManuallySetTableSelectionStructureAndChangeSelection(FStartCell, AEndCell);
      Break;
    end;
    FStartCell := FStartCell.Table.ParentCell;
    AEndCell := AEndCell.Table.ParentCell;
  end;

  AValidator := TdxFieldIsSelectValidator.Create(ActivePieceTable);
  try
    AValidator.ValidateSelection(ASelection);
  finally
    AValidator.Free;
  end;
end;

{ TdxRichEditSelectionCommand }

constructor TdxRichEditSelectionCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  ShouldEnsureCaretVisibleVerticallyBeforeUpdate := True;
  ShouldEnsureCaretVisibleVerticallyAfterUpdate := True;
end;

procedure TdxRichEditSelectionCommand.AfterUpdate;
begin
end;

function TdxRichEditSelectionCommand.ApplyNewPositionToSelectionEnd(ASelection: TdxSelection;
  ALogPosition: TdxDocumentLogPosition): Boolean;
begin
  ASelection.&End := ALogPosition;
  Result := False;
end;

procedure TdxRichEditSelectionCommand.BeforeUpdate;
begin
end;

function TdxRichEditSelectionCommand.CalculateSelectionCurrentPosition(
  ASelection: TdxSelection): TdxDocumentModelPosition;
var
  AStart, AEnd, ATemp: TdxDocumentModelPosition;
begin
  if ExtendSelection then
    Result := ASelection.Interval.&End
  else
  begin
    AStart := ASelection.Interval.Start;
    AEnd := ASelection.Interval.&End;
    if AStart.LogPosition > AEnd.LogPosition then
    begin
      ATemp := AStart;
      AStart := AEnd;
      AEnd := ATemp;
    end;
    if TreatStartPositionAsCurrent then
      Result := AStart
    else
      Result := AEnd;
  end;
end;

procedure TdxRichEditSelectionCommand.ChangeSelection(ASelection: TdxSelection);
var
  ARuns: TdxTextRunCollection;
  AOldSelectionLength: Integer;
  AUsePreviousBoxBounds: Boolean;
  APos: TdxDocumentModelPosition;
  ALogPosition: TdxDocumentLogPosition;
  AOldSelectionEndRunIndex: TdxRunIndex;
  ANewLogPosition: TdxDocumentLogPosition;
  APreviousLastCell, ACurrentLastCell: TdxCustomTableCell;
begin
  APos := CalculateSelectionCurrentPosition(ASelection);
  ALogPosition := ChangePosition(APos);
  AUsePreviousBoxBounds := False;
  if not ExtendSelection then
    ALogPosition := Min(ALogPosition, ActivePieceTable.DocumentEndLogPosition)
  else
  begin
    ALogPosition := Min(ALogPosition, ActivePieceTable.DocumentEndLogPosition + 1);
    ANewLogPosition := ExtendSelectionEndToParagraphMark(ASelection, ALogPosition);
    if ANewLogPosition <> ALogPosition then
      AUsePreviousBoxBounds := True;
  end;
  AOldSelectionEndRunIndex := ASelection.Interval.&End.RunIndex;
  AOldSelectionLength := ASelection.Length;
  AUsePreviousBoxBounds := ApplyNewPositionToSelectionEnd(ASelection, ALogPosition) or AUsePreviousBoxBounds;
  ChangeSelectionStart(ASelection, ALogPosition);
  if not ExtendSelection then
    ASelection.&End := ASelection.Start;
  ASelection.UsePreviousBoxBounds := AUsePreviousBoxBounds;
  if not ExtendSelection then
    ASelection.ClearMultiSelection;

  ARuns := DocumentModel.ActivePieceTable.Runs;
  APreviousLastCell := ARuns[AOldSelectionEndRunIndex].Paragraph.GetCellCore;
  ACurrentLastCell := ARuns[ASelection.Interval.&End.RunIndex].Paragraph.GetCellCore;

  if not ((AOldSelectionLength > ASelection.Length) and (ACurrentLastCell <> nil) and (APreviousLastCell = nil)) then
    UpdateTableSelectionAfterSelectionUpdated(ALogPosition);
  ValidateSelection(ASelection, ASelection.Length > AOldSelectionLength);
end;

procedure TdxRichEditSelectionCommand.ChangeSelectionStart(ASelection: TdxSelection;
  ALogPosition: TdxDocumentLogPosition);
begin
  if not ExtendSelection then
    ASelection.Start := ASelection.VirtualEnd
  else
    ASelection.Start := ExtendSelectionStartToParagraphMark(ASelection, ALogPosition);
end;

function TdxRichEditSelectionCommand.CreateEnsureCaretVisibleVerticallyCommand: TdxRichEditCommand;
begin
  Result := TdxEnsureCaretVisibleVerticallyCommand.Create(RichEditControl);
end;

procedure TdxRichEditSelectionCommand.EnsureCaretVisible;
begin
  ActiveView.EnsureCaretVisible;
end;

procedure TdxRichEditSelectionCommand.EnsureCaretVisibleVertically;
var
  ACommand: TdxRichEditCommand;
begin
  if DocumentModel.IsUpdateLocked then
    Exit;
  ACommand := CreateEnsureCaretVisibleVerticallyCommand;
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
  UpdateCaretPosition;
  if not TryToKeepCaretX then
  begin
    ACommand := TdxEnsureCaretVisibleHorizontallyCommand.Create(RichEditControl);
    try
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
  end;
end;

procedure TdxRichEditSelectionCommand.ExecuteCore;
var
  AShouldRedrawControl: Boolean;
  ACell: TdxTableCellViewInfo;
begin
  CheckExecutedAtUIThread;
  RichEditControl.BeginUpdate;
  try
    UpdateCaretPosition;
    if ShouldEnsureCaretVisibleVerticallyBeforeUpdate then
      EnsureCaretVisibleVertically;
    BeforeUpdate;
    DocumentModel.BeginUpdate;
    try
      PerformModifyModel;
      AShouldRedrawControl := PerformChangeSelection;
    finally
      DocumentModel.EndUpdate;
    end;
    AfterUpdate;
    if ShouldEnsureCaretVisibleVerticallyAfterUpdate and not DocumentModel.ActivePieceTable.IsComment then
      EnsureCaretVisibleVertically;
    if not TryToKeepCaretX then
    begin
      if CaretPosition.LayoutPosition.Character <> nil then
        CaretPosition.X := CaretPosition.CalculateCaretBounds.X;
    end;
    if ShouldUpdateCaretY and (CaretPosition.LayoutPosition.TableCell <> nil) then
    begin
      ACell := CaretPosition.LayoutPosition.TableCell;
      if (ACell <> nil) then
        CaretPosition.TableCellTopAnchorIndex := ACell.TopAnchorIndex
      else
        CaretPosition.TableCellTopAnchorIndex := -1;
    end;
    if DocumentModel.ActivePieceTable.IsComment then
      RichEditControl.InnerControl.ActiveView.OnActivePieceTableChanged;
    if AShouldRedrawControl then
    begin
      UpdateCaretPosition(TdxDocumentLayoutDetailsLevel.Character);
      RichEditControl.RedrawEnsureSecondaryFormattingComplete(TdxRefreshAction.Selection);
    end;
  finally
    RichEditControl.EndUpdate;
  end;
end;

function TdxRichEditSelectionCommand.ExtendSelectionEndToParagraphMark(ASelection: TdxSelection;
  ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
var
  ASelectionManager: TdxEnhancedSelectionManager;
begin
  ASelectionManager := TdxEnhancedSelectionManager.Create(ActivePieceTable);
  try
    Result := ASelectionManager.ExtendSelectionEndToParagraphMark(ASelection, ALogPosition);
  finally
    ASelectionManager.Free;
  end;
end;

function TdxRichEditSelectionCommand.ExtendSelectionStartToParagraphMark(ASelection: TdxSelection;
  ALogPosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
var
  ASelectionManager: TdxEnhancedSelectionManager;
begin
  ASelectionManager := TdxEnhancedSelectionManager.Create(ActivePieceTable);
  try
    Result := ASelectionManager.ExtendSelectionStartToParagraphMark(ASelection, ALogPosition);
  finally
    ASelectionManager.Free;
  end;
end;

function TdxRichEditSelectionCommand.GetLeftVisibleLogPosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
var
  ATextFilter: IdxVisibleTextFilter;
begin
  ATextFilter := ActivePieceTable.NavigationVisibleTextFilter;
  Result := ATextFilter.GetVisibleLogPosition(APos, True);
  if Result < ActivePieceTable.DocumentStartLogPosition then
    Result := APos.LogPosition;
end;

function TdxRichEditSelectionCommand.GetSelectionEndParagraphIndex: TdxParagraphIndex;
var
  ASelection: TdxSelectionRangeCollection;
  AEndParagraphIndex: TdxParagraphIndex;
begin
  ASelection := DocumentModel.Selection.GetSortedSelectionCollection;
  try
    AEndParagraphIndex := ActivePieceTable.FindParagraphIndex(ASelection.Last.&End);
    Result := AEndParagraphIndex;
  finally
    ASelection.Free;
  end;
end;

function TdxRichEditSelectionCommand.GetSelectionEndTableCell: TdxTableCell;
begin
  Result := ActivePieceTable.Paragraphs[GetSelectionEndParagraphIndex].GetCell;
end;

function TdxRichEditSelectionCommand.GetShouldUpdateCaretY: Boolean;
begin
  Result := True;
end;

function TdxRichEditSelectionCommand.IsSelectionEndAfterTableCell: Boolean;
var
  AIndex: TdxParagraphIndex;
begin
  AIndex := GetSelectionEndParagraphIndex;
  if AIndex = 0 then
    Exit(False);
  Result := ActivePieceTable.Paragraphs[AIndex - 1].IsInCell and not ActivePieceTable.Paragraphs[AIndex].IsInCell;
end;

function TdxRichEditSelectionCommand.IsSelectionEndInTableCell: Boolean;
var
  AIndex: TdxParagraphIndex;
begin
  AIndex := GetSelectionEndParagraphIndex;
  Result := ActivePieceTable.Paragraphs[AIndex].IsInCell;
end;

function TdxRichEditSelectionCommand.PerformChangeSelection: Boolean;
var
  ASelection: TdxSelection;
  AInitialSelectionLength, ACurrentSelectionLength: Integer;
begin
  ASelection := DocumentModel.Selection;
  AInitialSelectionLength := ASelection.Length;
  DocumentModel.BeginUpdate;
  try
    ChangeSelection(ASelection);
  finally
    DocumentModel.EndUpdate;
  end;
  ACurrentSelectionLength := ASelection.Length;
  Result := (ACurrentSelectionLength <> 0) or (AInitialSelectionLength <> 0);
end;

procedure TdxRichEditSelectionCommand.PerformModifyModel;
begin
end;

procedure TdxRichEditSelectionCommand.UpdateCaretPosition;
begin
  UpdateCaretPosition(UpdateCaretPositionBeforeChangeSelectionDetailsLevel);
end;

procedure TdxRichEditSelectionCommand.UpdateTableSelectionAfterSelectionUpdated(ALogPosition: TdxDocumentLogPosition);
begin
  DocumentModel.Selection.SetStartCell(ALogPosition);
end;

procedure TdxRichEditSelectionCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  APos: TdxDocumentModelPosition;
begin
  CheckExecutedAtUIThread;
  APos := CalculateSelectionCurrentPosition(DocumentModel.Selection);
  AState.Enabled := CanChangePosition(APos);
  AState.Visible := True;
  AState.Checked := false;
end;

procedure TdxRichEditSelectionCommand.ValidateSelection(ASelection: TdxSelection; AIsSelectionExtended: Boolean);
var
  AValidator: TdxSelectionValidator;
begin
  if AIsSelectionExtended then
    AValidator := TdxFieldIsSelectValidator.Create(ActivePieceTable)
  else
    AValidator := TdxFieldIsUnselectValidator.Create(ActivePieceTable);
  try
    AValidator.ValidateSelection(ASelection);
  finally
    AValidator.Free;
  end;
end;

{ TdxSelectFieldNextPrevToCaretCommand }

procedure TdxSelectFieldNextPrevToCaretCommand.ChangeSelection(
  ASelection: TdxSelection);
var
  ADeletedPosition, AStartSelection, AEndSelection: TdxDocumentModelPosition;
  AField: TdxField;
  AStart, AEnd: TdxDocumentLogPosition;
begin
  ADeletedPosition := GetDeletedPosition(ASelection);
  AField := ActivePieceTable.FindFieldByRunIndex(ADeletedPosition.RunIndex);
  AStartSelection := TdxDocumentModelPosition.FromRunStart(ActivePieceTable, AField.FirstRunIndex);
  AEndSelection := TdxDocumentModelPosition.FromRunEnd(ActivePieceTable, AField.LastRunIndex);
  if not AField.IsCodeView and (AField.Result.Start = AField.Result.&End) then
  begin
    if TryGetVisibleStart(AStartSelection, AStart) or TryGetVisibleEnd(AEndSelection, AEnd) then
    begin
      ASelection.Start := AStart;
      ASelection.&End := AEnd;
    end;
  end
  else
  begin
    ASelection.Start := AStartSelection.LogPosition;
    ASelection.&End := AEndSelection.LogPosition;
  end;
end;

function TdxSelectFieldNextPrevToCaretCommand.GetDeletedPosition(
  ASelection: TdxSelection): TdxDocumentModelPosition;
begin
  Result := ASelection.Interval.NormalizedEnd^;
end;

function TdxSelectFieldNextPrevToCaretCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

function TdxSelectFieldNextPrevToCaretCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxSelectFieldNextPrevToCaretCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxSelectFieldNextPrevToCaretCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

function TdxSelectFieldNextPrevToCaretCommand.GetVisibleTextFilter: IdxVisibleTextFilter;
begin
  Result := ActivePieceTable.NavigationVisibleTextFilter;
end;

function TdxSelectFieldNextPrevToCaretCommand.IsFieldRun(
  ARun: TdxTextRunBase): Boolean;
begin
  Result := (ARun is TdxFieldCodeRunBase) or (ARun is TdxFieldResultEndRun);
end;

function TdxSelectFieldNextPrevToCaretCommand.TryGetVisibleStart(const AStart: TdxDocumentModelPosition; out AResult: TdxDocumentLogPosition): Boolean;
begin
  AResult := VisibleTextFilter.GetPrevVisibleLogPosition(AStart, False);
  if AResult >= ActivePieceTable.DocumentStartLogPosition then
    Exit(True);
  AResult := ActivePieceTable.DocumentStartLogPosition;
  Result := False;
end;

function TdxSelectFieldNextPrevToCaretCommand.TryGetVisibleEnd(const AEnd: TdxDocumentModelPosition; out AResult: TdxDocumentLogPosition): Boolean;
begin
  AResult := VisibleTextFilter.GetNextVisibleLogPosition(AEnd, False);
  if AResult <= ActivePieceTable.DocumentEndLogPosition then
    Exit(True);
  AResult := ActivePieceTable.DocumentEndLogPosition;
  Result := False;
end;

procedure TdxSelectFieldNextPrevToCaretCommand.UpdateUIState(const AState: IdxCommandUIState);
var
  ARunIndex: TdxRunIndex;
begin
  ARunIndex := GetDeletedPosition(DocumentModel.Selection).RunIndex;
  AState.Enabled := (DocumentModel.Selection.Length = 0) and IsFieldRun(ActivePieceTable.Runs[ARunIndex]);
end;

{ TdxShrinkSelectionToStartCommand }

function TdxShrinkSelectionToStartCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxShrinkSelectionToStartCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := True;
end;

function TdxShrinkSelectionToStartCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxShrinkSelectionToStartCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

class function TdxShrinkSelectionToStartCommand.GetMenuCaption: string;
begin
  Result := '';
end;

class function TdxShrinkSelectionToStartCommand.GetDescription: string;
begin
  Result := '';
end;

function TdxShrinkSelectionToStartCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxShrinkSelectionToStartCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := APos.LogPosition;
end;

{ TdxSelectFieldPrevToCaretCommand }

function TdxSelectFieldPrevToCaretCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxSelectFieldPrevToCaretCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := APos.LogPosition;
end;

function TdxSelectFieldPrevToCaretCommand.GetDeletedPosition(ASelection: TdxSelection): TdxDocumentModelPosition;
begin
  if ASelection.NormalizedStart > ActivePieceTable.DocumentStartLogPosition then
    Result := TdxDocumentModelPosition.MoveBackward(ASelection.Interval.NormalizedStart^)
  else
    Result := TdxDocumentModelPosition.Create(ActivePieceTable);
end;

class function TdxSelectFieldPrevToCaretCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectFieldPrevToCaretDescription)
end;

class function TdxSelectFieldPrevToCaretCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectFieldPrevToCaretMenuCaption)
end;

{ TdxChangeActivePieceTableCommand }

procedure TdxChangeActivePieceTableCommand.ActivatePieceTable(
  ANewPieceTable: TdxPieceTable; ASection: TdxSection);
var
  APos: TdxDocumentModelPosition;
begin
  DocumentModel.SetActivePieceTable(ANewPieceTable, ASection);
  if ANewPieceTable.IsMain then
  begin
    if CaretPosition.LayoutPosition.IsValid(TdxDocumentLayoutDetailsLevel.Page) then
    begin
      APos := CaretPosition.LayoutPosition.Page.GetFirstPosition(DocumentModel.MainPieceTable);
      SetSelection(APos.LogPosition, 0);
    end;
  end
  else
    DocumentModel.Selection.SetStartCell(DocumentModel.Selection.Start);
end;

procedure TdxChangeActivePieceTableCommand.SetSelection(AStart: TdxDocumentLogPosition; ALength: Integer);
begin
  DocumentModel.BeginUpdate;
  try
    DocumentModel.Selection.Start := AStart;
    DocumentModel.Selection.&End := AStart + ALength;
    DocumentModel.Selection.SetStartCell(DocumentModel.Selection.Start);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxChangeActivePieceTableCommand.AfterUpdate;
begin
  inherited AfterUpdate;
  ApplyLayoutPreferredPageIndex(FPreferredPageIndex);
end;

function TdxChangeActivePieceTableCommand.CanChangePosition(
  const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxChangeActivePieceTableCommand.ChangePosition(
  const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

constructor TdxChangeActivePieceTableCommand.Create(const AControl: IdxRichEditControl; ANewActivePieceTable: TdxPieceTable;
  ASection: TdxSection; APreferredPageIndex: Integer);
begin
  inherited Create(AControl);
  FNewActivePieceTable := ANewActivePieceTable;
  Assert(ANewActivePieceTable.DocumentModel = DocumentModel);
  if not ANewActivePieceTable.IsMain then
    FSection := ASection;
  FPreferredPageIndex := APreferredPageIndex;
end;

class function TdxChangeActivePieceTableCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

function TdxChangeActivePieceTableCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxChangeActivePieceTableCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

function TdxChangeActivePieceTableCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxChangeActivePieceTableCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxChangeActivePieceTableCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.PageArea;
end;

function TdxChangeActivePieceTableCommand.PerformChangeSelection: Boolean;
begin
  ActivatePieceTable(FNewActivePieceTable, FSection);
  Result := True;
end;

procedure TdxChangeActivePieceTableCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Visible := True;
  AState.Checked := True;
  if FNewActivePieceTable.IsHeaderFooter then
  begin
    AState.Enabled := ActiveViewType = TdxRichEditViewType.PrintLayout;
    ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.HeadersFooters, AState.Enabled);
    ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Sections, AState.Enabled);
  end
  else
    if FNewActivePieceTable.IsTextBox then
    begin
      AState.Enabled := True;
      ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.FloatingObjects, AState.Enabled);
    end;
end;

{ TdxSelectUpperLevelObjectCommand }

class function TdxSelectUpperLevelObjectCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SelectUpperLevelObject;
end;

function TdxSelectUpperLevelObjectCommand.PerformChangeSelection: Boolean;
var
  ATextBoxPieceTable: TdxTextBoxContentType;
  AAnchorPieceTable: TdxPieceTable;
  ASection: TdxSection;
begin
  if ActivePieceTable.IsTextBox then
  begin
    ATextBoxPieceTable := Safe<TdxTextBoxContentType>.Cast(ActivePieceTable.ContentType);
    AAnchorPieceTable := TdxPieceTable(ATextBoxPieceTable.AnchorRun.PieceTable);
    ASection := nil;
    if AAnchorPieceTable.IsHeaderFooter then
      ASection := GetCurrentSectionFromCaretLayoutPosition;
    ActivatePieceTable(AAnchorPieceTable, ASection);
    SetSelection(AAnchorPieceTable.GetRunLogPosition(ATextBoxPieceTable.AnchorRun), 1);
    Exit(True);
  end
  else
  begin
    if not ActivePieceTable.IsMain then
    begin
      if ResetCurrentObjectSelection then
        Exit(True);
      ActivatePieceTable(DocumentModel.MainPieceTable, nil);
      Exit(True);
    end
    else
      Exit(ResetCurrentObjectSelection);
  end;
end;

function TdxSelectUpperLevelObjectCommand.ResetCurrentObjectSelection: Boolean;
begin
  if not CanResetCurrentObjectSelection then
    Exit(False);
  SetSelection(DocumentModel.Selection.NormalizedStart, 0);
  Result := True;
end;

function TdxSelectUpperLevelObjectCommand.CanResetCurrentObjectSelection: Boolean;
var
  ARunInfo: TdxRunInfo;
  ARun: TdxTextRunBase;
begin
  if DocumentModel.Selection.Length <> 1 then
    Exit(False);

  ARunInfo := ActivePieceTable.FindRunInfo(DocumentModel.Selection.NormalizedStart, 1);
  try
    ARun := ActivePieceTable.Runs[ARunInfo.Start.RunIndex];
  finally
    ARunInfo.Free;
  end;
  if not ((ARun is TdxFloatingObjectAnchorRun) or (ARun is TdxInlinePictureRun)) then
    Exit(False);

  Result := True;
end;

function TdxSelectUpperLevelObjectCommand.GetCurrentSectionFromCaretLayoutPosition: TdxSection;
begin
  UpdateCaretPosition;
  if not CaretPosition.LayoutPosition.IsValid(TdxDocumentLayoutDetailsLevel.PageArea) then
    Exit(nil);
  Result := CaretPosition.LayoutPosition.PageArea.Section;
end;

procedure TdxSelectUpperLevelObjectCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  ARunInfo: TdxRunInfo;
  ARun: TdxTextRunBase;
begin
  AState.Visible := True;

  if DocumentModel.Selection.Length = 1 then
  begin
    ARunInfo := ActivePieceTable.FindRunInfo(DocumentModel.Selection.NormalizedStart, 1);
    try
      ARun := ActivePieceTable.Runs[ARunInfo.Start.RunIndex];
    finally
      ARunInfo.Free;
    end;
    AState.Enabled := not ActivePieceTable.IsMain or (((ARun is TdxFloatingObjectAnchorRun) or (ARun is TdxInlinePictureRun)));
  end
  else
    AState.Enabled := not ActivePieceTable.IsMain;
  AState.Checked := True;
end;

{ TdxPreviousCharacterCommand }

function TdxPreviousCharacterCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxPreviousCharacterCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  if not ExtendSelection and (DocumentModel.Selection.Length > 0) then
  begin
    if not DocumentModel.Selection.IsSelectFieldPictureResult then
      Exit(APos.LogPosition);
  end;
  if APos.LogPosition > ActivePieceTable.DocumentStartLogPosition then
    Result := GetPrevVisibleLogPosition(APos)
  else
    Result := APos.LogPosition;
end;

class function TdxPreviousCharacterCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveBackwardDescription);
end;

function TdxPreviousCharacterCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxPreviousCharacterCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveBackwardMenuCaption);
end;

function TdxPreviousCharacterCommand.GetPrevVisibleLogPosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
var
  ATextFilter: IdxVisibleTextFilter;
begin
  ATextFilter := ActivePieceTable.NavigationVisibleTextFilter;
  Result := ATextFilter.GetPrevVisibleLogPosition(APos, True);
  if Result < ActivePieceTable.DocumentStartLogPosition then
    Result := APos.LogPosition;
end;

function TdxPreviousCharacterCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := True;
end;

function TdxPreviousCharacterCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxPreviousCharacterCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

class function TdxPreviousCharacterCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.PreviousCharacter;
end;

{ TdxPrevNextWordCommand }

function TdxPrevNextWordCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxPrevNextWordCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxPrevNextWordCommand.GetModelPosition(const APos: TdxDocumentLogPosition): TdxDocumentModelPosition;
begin
Assert(False);
end;

function TdxPrevNextWordCommand.GetNextVisibleLogPosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := VisibleTextFilter.GetNextVisibleLogPosition(APos, True);
  if Result < ActivePieceTable.DocumentEndLogPosition then
    Result := Result - 1;
end;

function TdxPrevNextWordCommand.GetPrevVisibleLogPosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := VisibleTextFilter.GetPrevVisibleLogPosition(APos, True);
  if Result > ActivePieceTable.DocumentStartLogPosition then
    Result := Result + 1;
end;

function TdxPrevNextWordCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := True;
end;

function TdxPrevNextWordCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxPrevNextWordCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

function TdxPrevNextWordCommand.GetVisibleTextFilter: IdxVisibleTextFilter;
begin
  Result := ActivePieceTable.NavigationVisibleTextFilter;
end;

{ TdxPreviousWordCommand }

function TdxPreviousWordCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  AIterator: TdxPieceTableIterator;
begin
  AIterator := TdxVisibleWordsIterator.Create(ActivePieceTable);
  try
    if not ExtendSelection and (DocumentModel.Selection.Length > 0) then
      Result := APos.LogPosition
    else
      Result := AIterator.MoveBack(APos).LogPosition;
  finally
    AIterator.Free;
  end;
end;

class function TdxPreviousWordCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMovePreviousWordDescription);
end;

class function TdxPreviousWordCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMovePreviousWordMenuCaption);
end;

class function TdxPreviousWordCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.PreviousWord;
end;

{ TdxNextCharacterCommand }

function TdxNextCharacterCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxNextCharacterCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
begin
  if not ExtendSelection and (DocumentModel.Selection.Length > 0) then
  begin
    if not DocumentModel.Selection.IsSelectFieldPictureResult then
      Exit(APos.LogPosition);
  end;
  if APos.LogPosition < GetMaxPosition then
    Result := GetNextVisibleLogPosition(APos)
  else
    Result := APos.LogPosition;
end;

class function TdxNextCharacterCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveForwardDescription);
end;

function TdxNextCharacterCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxNextCharacterCommand.GetMaxPosition: TdxDocumentLogPosition;
begin
  Result := ActivePieceTable.DocumentEndLogPosition;
end;

class function TdxNextCharacterCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveForwardMenuCaption);
end;

function TdxNextCharacterCommand.GetNextVisibleLogPosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
var
  ATextFilter: IdxVisibleTextFilter;
begin
  ATextFilter := ActivePieceTable.NavigationVisibleTextFilter;
  Result := ATextFilter.GetNextVisibleLogPosition(APos, True);
end;

function TdxNextCharacterCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxNextCharacterCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxNextCharacterCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

class function TdxNextCharacterCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.NextCharacter;
end;

{ TdxNextWordCommand }

function TdxNextWordCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  AIterator: TdxPieceTableIterator;
begin
  AIterator := TdxVisibleWordsIterator.Create(ActivePieceTable);
  try
    if not ExtendSelection and (DocumentModel.Selection.Length > 0) then
      Exit(APos.LogPosition);
    Result := AIterator.MoveForward(APos).LogPosition;
  finally
    AIterator.Free;
  end;
end;

class function TdxNextWordCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveNextWordDescription);
end;

class function TdxNextWordCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveNextWordMenuCaption);
end;

class function TdxNextWordCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.NextWord;
end;

{ TdxLineUpDownCommandBase }

function TdxLineUpDownCommandBase.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxLineUpDownCommandBase.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  ACalculator: TdxNextCaretPositionVerticalDirectionCalculator;
  ANewPosition: TdxDocumentModelPosition;
begin
  ACalculator := CreateNextCaretPositionCalculator;
  try
    ANewPosition := ACalculator.CalculateNextPosition(CaretPosition);
    if not ANewPosition.IsValid then
      Result := APos.LogPosition
    else
      Result := GetLeftVisibleLogPosition(ANewPosition);
  finally
    ACalculator.Free;
  end;
end;

function TdxLineUpDownCommandBase.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Row;
end;

{ TdxPreviousLineCommand }

function TdxPreviousLineCommand.CreateNextCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator;
begin
  Result := TdxNextCaretPositionLineUpCalculator.Create(RichEditControl)
end;

class function TdxPreviousLineCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveLineUpDescription);
end;

function TdxPreviousLineCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxPreviousLineCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveLineUpMenuCaption);
end;

function TdxPreviousLineCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := True;
end;

function TdxPreviousLineCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := True;
end;

class function TdxPreviousLineCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.PreviousLine;
end;

{ TdxNextLineCommand }

function TdxNextLineCommand.CreateNextCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator;
begin
  Result := TdxNextCaretPositionLineDownCalculator.Create(RichEditControl);
end;

class function TdxNextLineCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveLineDownDescription);
end;

function TdxNextLineCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxNextLineCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveLineDownMenuCaption);
end;

function TdxNextLineCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxNextLineCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := True;
end;

class function TdxNextLineCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.NextLine;
end;

{ TdxExtendPreviousCharacterCommand }

function TdxExtendPreviousCharacterCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  ACalculator: TdxExtendKeyboardSelectionHorizontalDirectionCalculator;
begin
  Result := inherited ChangePosition(APos);
  ACalculator := TdxExtendKeyboardSelectionHorizontalDirectionCalculator.Create(DocumentModel);
  try
    Result := ACalculator.CalculatePrevPosition(Result);
  finally
    ACalculator.Free;
  end;
end;

function TdxExtendPreviousCharacterCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxExtendPreviousCharacterCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendPreviousCharacter;
end;

procedure TdxExtendPreviousCharacterCommand.UpdateTableSelectionAfterSelectionUpdated(
  ALogPosition: Integer);
begin
  DocumentModel.Selection.UpdateTableSelectionEnd(ALogPosition);
end;

{ TdxExtendNextCharacterCommand }

function TdxExtendNextCharacterCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  ACalculator: TdxExtendKeyboardSelectionHorizontalDirectionCalculator;
begin
  Result := inherited ChangePosition(APos);
  ACalculator := TdxExtendKeyboardSelectionHorizontalDirectionCalculator.Create(DocumentModel);
  try
    Result := ACalculator.CalculateNextPosition(Result);
  finally
    ACalculator.Free;
  end;
end;

function TdxExtendNextCharacterCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

function TdxExtendNextCharacterCommand.GetMaxPosition: Integer;
begin
  Result := ActivePieceTable.DocumentEndLogPosition + 1;
end;

class function TdxExtendNextCharacterCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendNextCharacter;
end;

procedure TdxExtendNextCharacterCommand.UpdateTableSelectionAfterSelectionUpdated(
  ALogPosition: Integer);
begin
  DocumentModel.Selection.UpdateTableSelectionEnd(ALogPosition);
end;

{ TdxExtendPreviousWordCommand }

function TdxExtendPreviousWordCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  ACalculator: TdxExtendKeyboardSelectionHorizontalDirectionCalculator;
begin
  Result := inherited ChangePosition(APos);
  ACalculator := TdxExtendKeyboardSelectionHorizontalDirectionCalculator.Create(DocumentModel);
  try
    Result := ACalculator.CalculatePrevPosition(Result);
  finally
    ACalculator.Free;
  end;
end;

function TdxExtendPreviousWordCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxExtendPreviousWordCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendPreviousWord;
end;

procedure TdxExtendPreviousWordCommand.UpdateTableSelectionAfterSelectionUpdated(
  ALogPosition: Integer);
begin
  DocumentModel.Selection.UpdateTableSelectionEnd(ALogPosition);
end;

{ TdxExtendNextWordCommand }

function TdxExtendNextWordCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  ACalculator: TdxExtendKeyboardSelectionHorizontalDirectionCalculator;
begin
  Result := inherited ChangePosition(APos);
  ACalculator := TdxExtendKeyboardSelectionHorizontalDirectionCalculator.Create(DocumentModel);
  try
    Result := ACalculator.CalculateNextPosition(Result);
  finally
    ACalculator.Free;
  end;
end;

function TdxExtendNextWordCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxExtendNextWordCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendNextWord;
end;

procedure TdxExtendNextWordCommand.UpdateTableSelectionAfterSelectionUpdated(
  ALogPosition: Integer);
begin
  DocumentModel.Selection.UpdateTableSelectionEnd(ALogPosition);
end;

{ TdxStartOfLineCommand }

function TdxStartOfLineCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxStartOfLineCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  APosition: TdxDocumentModelPosition;
begin
  APosition := CaretPosition.LayoutPosition.Row.GetFirstPosition(ActivePieceTable);
  Result := GetLeftVisibleLogPosition(APosition);
end;

class function TdxStartOfLineCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveToStartOfLineDescription);
end;

function TdxStartOfLineCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxStartOfLineCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveToStartOfLineMenuCaption);
end;

function TdxStartOfLineCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := True;
end;

function TdxStartOfLineCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxStartOfLineCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Row;
end;

class function TdxStartOfLineCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.StartOfLine;
end;

{ TdxEndOfLineCommand }

function TdxEndOfLineCommand.ApplyNewPositionToSelectionEnd(ASelection: TdxSelection; ALogPosition: Integer): Boolean;
var
  ASelectionIntervalEnd: TdxDocumentModelPosition;
  ARun: TdxTextRunBase;
  AChar: Char;
begin
  inherited ApplyNewPositionToSelectionEnd(ASelection, ALogPosition);
  if ExtendSelection then
  begin
    if ASelection.&End < ActivePieceTable.DocumentEndLogPosition then
    begin
      ASelection.&End := ASelection.&End + 1;
      Result := True;
    end
    else
      Result := False;
  end
  else
  begin
    ASelectionIntervalEnd := ASelection.Interval.&End;
    ARun := ActivePieceTable.Runs[ASelectionIntervalEnd.RunIndex];
    if ARun is TdxParagraphRun then
      Result := False
    else
    begin
      AChar := ActivePieceTable.TextBuffer[ARun.StartIndex + ASelectionIntervalEnd.RunOffset];
      if AChar = TdxCharacters.LineBreak then
        Result := False
      else
      begin
        ASelection.&End := ASelection.&End + 1;
        Result := True;
      end;
    end;
  end;
end;

function TdxEndOfLineCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxEndOfLineCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
begin
  Result := CaretPosition.LayoutPosition.Row.GetLastPosition(ActivePieceTable).LogPosition;
end;

function TdxEndOfLineCommand.ExtendSelectionEndToParagraphMark(
  ASelection: TdxSelection; ALogPosition: Integer): Integer;
begin
  Result := ALogPosition;
end;

class function TdxEndOfLineCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveToEndOfLineDescription);
end;

function TdxEndOfLineCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxEndOfLineCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveToEndOfLineMenuCaption);
end;

function TdxEndOfLineCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxEndOfLineCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxEndOfLineCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Row;
end;

class function TdxEndOfLineCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.EndOfLine;
end;

{ TdxExtendStartOfLineCommand }

function TdxExtendStartOfLineCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxExtendStartOfLineCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendStartOfLine;
end;

{ TdxExtendEndOfLineCommand }

function TdxExtendEndOfLineCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxExtendEndOfLineCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendEndOfLine;
end;

{ TdxExtendPreviousLineCommand }

function TdxExtendPreviousLineCommand.CreateNextCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator;
begin
  Result := TdxExtendNextCaretPositionLineUpCalculator.Create(RichEditControl)
end;

function TdxExtendPreviousLineCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxExtendPreviousLineCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendPreviousLine;
end;

procedure TdxExtendPreviousLineCommand.UpdateTableSelectionAfterSelectionUpdated(
  ALogPosition: Integer);
var
  ASelection: TdxSelection;
begin
  ASelection := DocumentModel.Selection;
  ASelection.UpdateTableSelectionStart(ALogPosition);
  ASelection.UpdateTableSelectionEnd(ALogPosition);
end;

{ TdxExtendNextLineCommand }

function TdxExtendNextLineCommand.CreateNextCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator;
begin
  Result := TdxExtendNextCaretPositionLineDownCalculator.Create(RichEditControl);
end;

function TdxExtendNextLineCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxExtendNextLineCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendNextLine;
end;

procedure TdxExtendNextLineCommand.UpdateTableSelectionAfterSelectionUpdated(ALogPosition: Integer);
var
  ASelection: TdxSelection;
begin
  ASelection := DocumentModel.Selection;
  ASelection.UpdateTableSelectionEnd(ALogPosition);
  ASelection.UpdateTableSelectionStart(ALogPosition);
end;

{ TdxPrevNextParagraphCommandBase }

function TdxPrevNextParagraphCommandBase.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxPrevNextParagraphCommandBase.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxPrevNextParagraphCommandBase.GetNextVisiblePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := VisibleTextFilter.GetNextVisibleLogPosition(APos, False);
  if Result < ActivePieceTable.DocumentEndLogPosition then
    Result := Result - 1;
end;

function TdxPrevNextParagraphCommandBase.GetPrevVisiblePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := VisibleTextFilter.GetPrevVisibleLogPosition(APos, False);
  if Result > ActivePieceTable.DocumentStartLogPosition then
    Result := Result + 1;
end;

function TdxPrevNextParagraphCommandBase.GetNextVisiblePosition(ARunIndex: TdxRunIndex): TdxDocumentModelPosition;
var
  AIndex: TdxRunIndex;
begin
  AIndex := VisibleTextFilter.GetNextVisibleRunIndex(ARunIndex);
  Result := TdxDocumentModelPosition.FromRunStart(ActivePieceTable, AIndex);
end;

function TdxPrevNextParagraphCommandBase.GetPrevVisiblePosition(ARunIndex: TdxRunIndex): TdxDocumentModelPosition;
var
  AIndex: TdxRunIndex;
begin
  AIndex := VisibleTextFilter.GetPrevVisibleRunIndex(ARunIndex);
  Result := TdxDocumentModelPosition.FromRunStart(ActivePieceTable, AIndex);
end;

function TdxPrevNextParagraphCommandBase.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxPrevNextParagraphCommandBase.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxPrevNextParagraphCommandBase.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

function TdxPrevNextParagraphCommandBase.GetValidLogPosition(const ANewPos: TdxDocumentModelPosition;
  AParagraph: TdxParagraph): TdxDocumentLogPosition;
begin
  Result := ANewPos.LogPosition;
end;

function TdxPrevNextParagraphCommandBase.GetVisibleLogPosition(AParagraph: TdxParagraph): TdxDocumentLogPosition;
var
  ARunIndex: TdxRunIndex;
begin
  ARunIndex := AParagraph.FirstRunIndex;
  if VisibleTextFilter.IsRunVisible(ARunIndex) then
    Result := AParagraph.LogPosition
  else
    Result := GetValidLogPosition(GetNextVisiblePosition(ARunIndex), AParagraph);
end;

function TdxPrevNextParagraphCommandBase.VisibleTextFilter: IdxVisibleTextFilter;
begin
  Result := ActivePieceTable.NavigationVisibleTextFilter;
end;

{ TdxPreviousParagraphCommand }

function TdxPreviousParagraphCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  AParagraphIndex: TdxParagraphIndex;
  AParagraph: TdxParagraph;
begin
  AParagraphIndex := APos.ParagraphIndex;
  AParagraph := ActivePieceTable.Paragraphs[AParagraphIndex];
  if APos.LogPosition = AParagraph.LogPosition then
    Result := GetPrevPosition(AParagraphIndex)
  else
  begin
    Result := GetVisibleLogPosition(AParagraph);
    if Result = APos.LogPosition then
      Result := GetPrevPosition(AParagraphIndex);
  end;
end;

class function TdxPreviousParagraphCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMovePreviousParagraphDescription);
end;

class function TdxPreviousParagraphCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMovePreviousParagraphMenuCaption);
end;

function TdxPreviousParagraphCommand.GetMinPosition: TdxDocumentLogPosition;
var
  APos: TdxDocumentModelPosition;
begin
  APos := TdxDocumentModelPosition.FromParagraphStart(ActivePieceTable, 0);
  if VisibleTextFilter.IsRunVisible(APos.RunIndex) then
    Result := APos.LogPosition
  else
    Result := GetNextVisiblePosition(APos);
end;

function TdxPreviousParagraphCommand.GetPrevPosition(AParagraphIndex: TdxParagraphIndex): TdxDocumentLogPosition;
var
  APrevParagraphIndex: TdxParagraphIndex;
begin
  APrevParagraphIndex := AParagraphIndex - 1;
  if APrevParagraphIndex >= 0 then
    Result := GetVisibleLogPosition(ActivePieceTable.Paragraphs[APrevParagraphIndex])
  else
    Result := GetMinPosition;
end;

function TdxPreviousParagraphCommand.GetValidLogPosition(
  const ANewPos: TdxDocumentModelPosition; AParagraph: TdxParagraph): Integer;
begin
  if ANewPos.ParagraphIndex <= AParagraph.Index then
    Result := ANewPos.LogPosition
  else
    Result := GetPrevVisiblePosition(AParagraph.FirstRunIndex).LogPosition;
end;

class function TdxPreviousParagraphCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.PreviousParagraph;
end;

{ TdxExtendPreviousParagraphCommand }

function TdxExtendPreviousParagraphCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  APosition: TdxDocumentLogPosition;
  ACalculator: TdxExtendKeyboardSelectionHorizontalDirectionCalculator;
begin
  APosition := inherited ChangePosition(APos);
  ACalculator := TdxExtendKeyboardSelectionHorizontalDirectionCalculator.Create(DocumentModel);
  try
    Result := ACalculator.CalculatePrevPosition(APosition);
  finally
    ACalculator.Free;
  end;
end;

function TdxExtendPreviousParagraphCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxExtendPreviousParagraphCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendPreviousParagraph;
end;

procedure TdxExtendPreviousParagraphCommand.UpdateTableSelectionAfterSelectionUpdated(
  ALogPosition: Integer);
begin
  DocumentModel.Selection.UpdateTableSelectionEnd(ALogPosition);
end;

{ TdxNextParagraphCommand }

function TdxNextParagraphCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  AParagraphIndex: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
begin
  AParagraphIndex := APos.ParagraphIndex + 1;
  AParagraphs := ActivePieceTable.Paragraphs;
  if AParagraphIndex < AParagraphs.Count then
    Result := GetVisibleLogPosition(AParagraphs[AParagraphIndex])
  else
    Result := GetMaxPosition;
end;

class function TdxNextParagraphCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveNextParagraphDescription);
end;

function TdxNextParagraphCommand.GetMaxPosition: TdxDocumentLogPosition;
var
  APos: TdxDocumentModelPosition;
begin
  APos := TdxDocumentModelPosition.FromParagraphEnd(ActivePieceTable, ActivePieceTable.Paragraphs.Count - 1);
  if VisibleTextFilter.IsRunVisible(APos.RunIndex) then
    Result := APos.LogPosition
  else
    Result := GetPrevVisiblePosition(APos);
end;

class function TdxNextParagraphCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveNextParagraphMenuCaption);
end;

class function TdxNextParagraphCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.NextParagraph;
end;

{ TdxExtendNextParagraphCommand }

function TdxExtendNextParagraphCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  APosition: TdxDocumentLogPosition;
  ACalculator: TdxExtendKeyboardSelectionHorizontalDirectionCalculator;
begin
  APosition := inherited ChangePosition(APos);
  ACalculator := TdxExtendKeyboardSelectionHorizontalDirectionCalculator.Create(DocumentModel);
  try
    Result := ACalculator.CalculateNextPosition(APosition);
  finally
    ACalculator.Free;
  end;
end;

function TdxExtendNextParagraphCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

function TdxExtendNextParagraphCommand.GetMaxPosition: Integer;
begin
  Result := ActivePieceTable.DocumentEndLogPosition + 1;
end;

class function TdxExtendNextParagraphCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendNextParagraph;
end;

procedure TdxExtendNextParagraphCommand.UpdateTableSelectionAfterSelectionUpdated(
  ALogPosition: Integer);
begin
  DocumentModel.Selection.UpdateTableSelectionEnd(ALogPosition);
end;

{ TdxPrevNextPageCommandBase }

function TdxPrevNextPageCommandBase.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxPrevNextPageCommandBase.CreateEnsureCaretVisibleVerticallyCommand: TdxRichEditCommand;
var
  ACommand: TdxEnsureCaretVisibleVerticallyForMovePrevNextPageCommand;
begin
  ACommand := TdxEnsureCaretVisibleVerticallyForMovePrevNextPageCommand.Create(RichEditControl);
  ACommand.PhysicalOffsetAboveTargetRow := Round(DocumentModel.LayoutUnitConverter.PointsToFontUnitsF(72) / ActiveView.ScaleFactor);
  Result := ACommand;
end;

function TdxPrevNextPageCommandBase.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxPrevNextPageCommandBase.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Page;
end;

{ TdxPreviousPageCommand }

function TdxPreviousPageCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  APages: TdxPageCollection;
  APageIndex: Integer;
begin
  APages := ActiveView.FormattingController.PageController.Pages;
  APageIndex := APages.IndexOf(CaretPosition.LayoutPosition.Page);
  Assert(APageIndex >= 0);
  if APageIndex > 0 then
    Dec(APageIndex);
  Result := GetLeftVisibleLogPosition(APages[APageIndex].GetFirstPosition(ActivePieceTable));
end;

class function TdxPreviousPageCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMovePreviousPageDescription);
end;

function TdxPreviousPageCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxPreviousPageCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMovePreviousPageMenuCaption);
end;

function TdxPreviousPageCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := True;
end;

class function TdxPreviousPageCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.PreviousPage;
end;

{ TdxExtendPreviousPageCommand }

function TdxExtendPreviousPageCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxExtendPreviousPageCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendPreviousPage;
end;

{ TdxNextPageCommand }

function TdxNextPageCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
var
  APages: TdxPageCollection;
  APageIndex: Integer;
begin
  APages := ActiveView.FormattingController.PageController.Pages;
  APageIndex := APages.IndexOf(CaretPosition.LayoutPosition.Page);
  Assert(APageIndex >= 0);
  if APageIndex + 1 < APages.Count then
  begin
    Inc(APageIndex);
    Result := APages[APageIndex].GetFirstPosition(ActivePieceTable).LogPosition;
  end
  else
    Result := GetLastPosition(APageIndex);
end;

class function TdxNextPageCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveNextPageDescription);
end;

function TdxNextPageCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxNextPageCommand.GetLastPosition(APageIndex: Integer): TdxDocumentLogPosition;
var
  APages: TdxPageCollection;
begin
  APages := ActiveView.FormattingController.PageController.Pages;
  Result := APages[APageIndex].GetFirstPosition(ActivePieceTable).LogPosition;
end;

class function TdxNextPageCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveNextPageMenuCaption);
end;

function TdxNextPageCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

class function TdxNextPageCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.NextPage;
end;

{ TdxExtendNextPageCommand }

function TdxExtendNextPageCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

function TdxExtendNextPageCommand.GetLastPosition(APageIndex: Integer): Integer;
begin
  Result := ActivePieceTable.DocumentEndLogPosition;
end;

class function TdxExtendNextPageCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendNextPage;
end;

{ TdxPrevNextScreenCommandBase }

function TdxPrevNextScreenCommandBase.CalculateCaretNewLogPosition(const APhysicalPoint: TPoint): TdxDocumentLogPosition;
var
  APageViewInfoRow: TdxPageViewInfoRow;
  APageViewInfo: TdxPageViewInfo;
  X: Integer;
  ACalculator: TdxNextCaretPositionVerticalDirectionCalculator;
  ALogicalCaretPoint: TPoint;
  AColumn: TdxColumn;
  ATargetRow: TdxRow;
  ACharacter: TdxCharacterBox;
begin
  APageViewInfoRow := ActiveView.PageViewInfoGenerator.GetPageRowAtPoint(APhysicalPoint, True);
  if APageViewInfoRow = nil then
    Result := MaxInt
  else
  begin
    APageViewInfo := APageViewInfoRow.GetPageAtPoint(APhysicalPoint, False);
    if APageViewInfo = nil then
      Result := MaxInt
    else
    begin
      X := ActiveView.CreateLogicalPoint(APageViewInfo.ClientBounds, APhysicalPoint).X;
      ACalculator := CreateCaretPositionCalculator;
      try
        ALogicalCaretPoint := ActiveView.CreateLogicalPoint(APageViewInfo.ClientBounds, APhysicalPoint);
			  AColumn := ACalculator.GetTargetColumn(APageViewInfo.Page, ALogicalCaretPoint.X);
        if ShouldUseAnotherPage(AColumn, ALogicalCaretPoint) then
        begin
          ATargetRow := ACalculator.ObtainTargetRowFromCurrentPageViewInfoAndCaretX(APageViewInfo, nil, X);
          if ATargetRow <> nil then
          begin
            ACharacter := ACalculator.GetTargetCharacter(ATargetRow, X);
            try
              Result := ACharacter.GetFirstPosition(ActivePieceTable).LogPosition;
            finally
              ACharacter.Free;
            end;
          end
          else
            Result := GetDefaultLogPosition;
			  end
  			else
        begin
          ATargetRow := GetTargetRowAtColumn(AColumn);
          ACharacter := ACalculator.GetTargetCharacter(ATargetRow, X);
          try
            Result := ACharacter.GetFirstPosition(ActivePieceTable).LogPosition;
          finally
            ACharacter.Free;
          end;
        end;
      finally
        ACalculator.Free;
      end;
    end;
  end;
end;

function TdxPrevNextScreenCommandBase.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := APos.PieceTable.IsMain;
end;

procedure TdxPrevNextScreenCommandBase.ChangeSelection(ASelection: TdxSelection);
var
  APhysicalPoint: TPoint;
  APos, ACurrentLogPosition: TdxDocumentLogPosition;
  ACommand: TdxPlaceCaretToPhysicalPointCommand;
  ACalculator: TdxNextCaretPositionVerticalDirectionCalculator;
  ANewPosition: TdxDocumentModelPosition;
begin
  APhysicalPoint := ActiveView.CreatePhysicalPoint(CaretPosition.PageViewInfo,
    CaretPosition.CalculateCaretBounds.TopLeft);
  if ScrollScreen then
  begin
    ACurrentLogPosition := ASelection.&End;
    ACommand := CreatePlaceCaretToPhysicalPointCommand;
    try
      ACommand.UpdateCaretX := False;
      ACommand.PhysicalPoint := APhysicalPoint;
      ACommand.Execute;
    finally
      ACommand.Free;
    end;
    if ShouldCalculateNewCaretLogPosition(ACurrentLogPosition, ASelection.&End) then
    begin
      APos := CalculateCaretNewLogPosition(APhysicalPoint);
      if APos <> MaxInt then
      begin
        if not IsNewPositionInCorrectDirection(ACurrentLogPosition, APos) then
        begin
          if not UpdateCaretPosition(TdxDocumentLayoutDetailsLevel.Row) then
            Exit;
          ACalculator := CreateCaretPositionCalculator;
          try
            ANewPosition := ACalculator.CalculateNextPosition(CaretPosition);
            if ANewPosition.IsValid then
              APos := ANewPosition.LogPosition;
          finally
            ACalculator.Free;
          end;
        end;
        ASelection.&End := APos;
        if not ExtendSelection then
          ASelection.Start := APos;
      end;
    end;
  end
  else
    inherited ChangeSelection(ASelection);
end;

function TdxPrevNextScreenCommandBase.CreatePlaceCaretToPhysicalPointCommand: TdxPlaceCaretToPhysicalPointCommand;
begin
  Result := TdxPlaceCaretToPhysicalPointCommand.Create(RichEditControl);
end;

function TdxPrevNextScreenCommandBase.GetTryToKeepCaretX: Boolean;
begin
  Result := True;
end;

function TdxPrevNextScreenCommandBase.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Character;
end;

{ TdxPreviousScreenCommand }

function TdxPreviousScreenCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
begin
  Result := ActivePieceTable.DocumentStartLogPosition;
end;

function TdxPreviousScreenCommand.CreateCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator;
begin
  Result := TdxNextCaretPositionLineUpCalculator.Create(RichEditControl);
end;

function TdxPreviousScreenCommand.GetDefaultLogPosition: Integer;
begin
  Result := ActivePieceTable.DocumentStartLogPosition;
end;

class function TdxPreviousScreenCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveScreenUpDescription);
end;

function TdxPreviousScreenCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxPreviousScreenCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveScreenUpMenuCaption);
end;

function TdxPreviousScreenCommand.GetTargetRowAtColumn(AColumn: TdxColumn): TdxRow;
begin
  Result := AColumn.Rows.Last;
end;

function TdxPreviousScreenCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := True;
end;

class function TdxPreviousScreenCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.PreviousScreen;
end;

function TdxPreviousScreenCommand.IsNewPositionInCorrectDirection(
  APreviousLogPosition, ACurrentLogPosition: Integer): Boolean;
begin
  Result := APreviousLogPosition > ACurrentLogPosition;
end;

function TdxPreviousScreenCommand.ScrollScreen: Boolean;
begin
  Result := ActiveView.VerticalScrollController.ScrollPageUp;
  if Result then
    ActiveView.OnVerticalScroll;
end;

function TdxPreviousScreenCommand.ShouldCalculateNewCaretLogPosition(
  APreviousLogPosition, ACurrentLogPosition: Integer): Boolean;
begin
  Result := (APreviousLogPosition = ACurrentLogPosition) and (APreviousLogPosition <> 0);
end;

function TdxPreviousScreenCommand.ShouldUseAnotherPage(AColumn: TdxColumn;
  const ALogicalCaretPoint: TPoint): Boolean;
begin
  Result := AColumn.Rows.First.Bounds.Bottom > ALogicalCaretPoint.Y;
end;

{ TdxExtendPreviousScreenCommand }

function TdxExtendPreviousScreenCommand.CreatePlaceCaretToPhysicalPointCommand: TdxPlaceCaretToPhysicalPointCommand;
begin
  Result := TdxExtendSelectionToPhysicalPointCommand.Create(RichEditControl);
end;

function TdxExtendPreviousScreenCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxExtendPreviousScreenCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendPreviousScreen;
end;

{ TdxNextScreenCommand }

function TdxNextScreenCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
begin
  Result := ActivePieceTable.DocumentEndLogPosition;
end;

function TdxNextScreenCommand.CreateCaretPositionCalculator: TdxNextCaretPositionVerticalDirectionCalculator;
begin
  Result := TdxNextCaretPositionLineDownCalculator.Create(RichEditControl);
end;

function TdxNextScreenCommand.GetDefaultLogPosition: Integer;
begin
  Result := ActivePieceTable.DocumentEndLogPosition;
end;

class function TdxNextScreenCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveScreenDownDescription);
end;

function TdxNextScreenCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxNextScreenCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveScreenDownMenuCaption);
end;

function TdxNextScreenCommand.GetTargetRowAtColumn(AColumn: TdxColumn): TdxRow;
begin
  Result := AColumn.Rows.First;
end;

function TdxNextScreenCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := True;
end;

class function TdxNextScreenCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.NextScreen;
end;

function TdxNextScreenCommand.IsNewPositionInCorrectDirection(
  APreviousLogPosition, ACurrentLogPosition: Integer): Boolean;
begin
  Result := APreviousLogPosition < ACurrentLogPosition;
end;

function TdxNextScreenCommand.ScrollScreen: Boolean;
begin
  Result := ActiveView.VerticalScrollController.ScrollPageDown;
  if Result then
    ActiveView.OnVerticalScroll;
end;

function TdxNextScreenCommand.ShouldCalculateNewCaretLogPosition(
  APreviousLogPosition, ACurrentLogPosition: Integer): Boolean;
begin
  Result := APreviousLogPosition = ACurrentLogPosition;
end;

function TdxNextScreenCommand.ShouldUseAnotherPage(AColumn: TdxColumn; const ALogicalCaretPoint: TPoint): Boolean;
begin
  Result := AColumn.Rows.Last.Bounds.Top < ALogicalCaretPoint.Y;
end;

{ TdxExtendNextScreenCommand }

function TdxExtendNextScreenCommand.CreatePlaceCaretToPhysicalPointCommand: TdxPlaceCaretToPhysicalPointCommand;
begin
  Result := TdxExtendSelectionToPhysicalPointCommand.Create(RichEditControl);
end;

function TdxExtendNextScreenCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxExtendNextScreenCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendNextScreen;
end;

{ TdxStartOfDocumentCommand }

function TdxStartOfDocumentCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxStartOfDocumentCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
begin
  Result := ActivePieceTable.DocumentStartLogPosition;
end;

class function TdxStartOfDocumentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveToBeginOfDocumentDescription);
end;

function TdxStartOfDocumentCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxStartOfDocumentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveToBeginOfDocumentMenuCaption);
end;

function TdxStartOfDocumentCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := True;
end;

function TdxStartOfDocumentCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxStartOfDocumentCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

class function TdxStartOfDocumentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.StartOfDocument;
end;

{ TdxExtendStartOfDocumentCommand }

function TdxExtendStartOfDocumentCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxExtendStartOfDocumentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendStartOfDocument;
end;

{ TdxEndOfDocumentCommand }

function TdxEndOfDocumentCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxEndOfDocumentCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
begin
  Result := ActivePieceTable.DocumentEndLogPosition;
end;

class function TdxEndOfDocumentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveToEndOfDocumentDescription);
end;

function TdxEndOfDocumentCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxEndOfDocumentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMoveToEndOfDocumentMenuCaption);
end;

function TdxEndOfDocumentCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxEndOfDocumentCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxEndOfDocumentCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

class function TdxEndOfDocumentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.EndOfDocument;
end;

{ TdxExtendEndOfDocumentCommand }

function TdxExtendEndOfDocumentCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxExtendEndOfDocumentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ExtendEndOfDocument;
end;

procedure TdxExtendEndOfDocumentCommand.UpdateTableSelectionAfterSelectionUpdated(
  ALogPosition: Integer);
begin
  DocumentModel.Selection.UpdateTableSelectionStart(ALogPosition);
  DocumentModel.Selection.UpdateTableSelectionEnd(ALogPosition);
end;

{ TdxExtendSelectionByRangesCommand }

function TdxExtendSelectionByRangesCommand.ChangeSelectionEnd(
  ASelection: TdxSelection; ALogPosition: Integer;
  AHitTestResult: TdxRichEditHitTestResult): Boolean;
begin
  if AHitTestResult.Character.GetFirstPosition(ActivePieceTable).LogPosition >= InitialLogPosition then
    Result := ExtendEnd1(ASelection, AHitTestResult)
  else
  begin
    ASelection.&End := ExtendEnd2(AHitTestResult);
    Result := False;
  end;
end;

procedure TdxExtendSelectionByRangesCommand.ChangeSelectionStart(
  ASelection: TdxSelection; ALogPosition: Integer;
  AHitTestResult: TdxRichEditHitTestResult; ASelectionItemCountBeforeChangeEnd: Integer);
begin
  if AHitTestResult.Character.GetFirstPosition(ActivePieceTable).LogPosition >= InitialLogPosition then
    ASelection.Start := ExtendStart1
  else
    ASelection.Start := ExtendStart2;
end;

function TdxExtendSelectionByRangesCommand.ExtendEnd1(ASelection: TdxSelection;
  AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  AIterator: TdxPieceTableIterator;
  APos: TdxDocumentModelPosition;
begin
  AIterator := CreateIterator;
  try
    APos := AHitTestResult.Character.GetFirstPosition(ActivePieceTable);
    ASelection.&End := AIterator.MoveForward(APos).LogPosition;
    Result := False;
  finally
    AIterator.Free;
  end;
end;

function TdxExtendSelectionByRangesCommand.ExtendEnd2(
  AHitTestResult: TdxRichEditHitTestResult): TdxDocumentLogPosition;
var
  AIterator: TdxPieceTableIterator;
  APos: TdxDocumentModelPosition;
begin
  AIterator := CreateIterator;
  try
    APos := AHitTestResult.Character.GetFirstPosition(ActivePieceTable);
    Result := AIterator.MoveBack(APos).LogPosition;
  finally
    AIterator.Free;
  end;
end;

function TdxExtendSelectionByRangesCommand.ExtendStart1: TdxDocumentLogPosition;
var
  AIterator: TdxPieceTableIterator;
  APos: TdxDocumentModelPosition;
begin
  AIterator := CreateIterator;
  try
    APos := InitialBox.GetFirstPosition(ActivePieceTable);
    if AIterator.IsNewElement(APos) then
      Result := APos.LogPosition
    else
      Result := AIterator.MoveBack(APos).LogPosition;
  finally
    AIterator.Free;
  end;
end;

function TdxExtendSelectionByRangesCommand.ExtendStart2: TdxDocumentLogPosition;
var
  AIterator: TdxPieceTableIterator;
  APos: TdxDocumentModelPosition;
begin
  AIterator := CreateIterator;
  try
    APos := InitialBox.GetFirstPosition(ActivePieceTable);
    Result := AIterator.MoveForward(APos).LogPosition;
  finally
    AIterator.Free;
  end;
end;

{ TdxExtendSelectionByWordsCommand }

function TdxExtendSelectionByWordsCommand.CreateIterator: TdxPieceTableIterator;
begin
  Result := TdxWordsDocumentModelIterator.Create(ActivePieceTable);
end;

{ TdxSelectAllCommand }

function TdxSelectAllCommand.CanChangePosition(
  const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxSelectAllCommand.ChangePosition(
  const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

procedure TdxSelectAllCommand.ChangeSelection(ASelection: TdxSelection);
var
  AStartSelection, AEndSelection: TdxDocumentLogPosition;
begin
  DocumentModel.Selection.ClearMultiSelection;
  AStartSelection := ActivePieceTable.DocumentStartLogPosition;
  ASelection.SetStartCell(AStartSelection);
  ASelection.Start := AStartSelection;
  AEndSelection := ActivePieceTable.DocumentEndLogPosition + 1;
  ASelection.&End := AEndSelection;
  ASelection.UpdateTableSelectionEnd(AEndSelection);
end;

procedure TdxSelectAllCommand.EnsureCaretVisibleVertically;
begin
end;

class function TdxSelectAllCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectAllDescription);
end;

function TdxSelectAllCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxSelectAllCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectAllMenuCaption);
end;

class function TdxSelectAllCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SelectAll;
end;

class function TdxSelectAllCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SelectAll;
end;

function TdxSelectAllCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxSelectAllCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxSelectAllCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

{ TdxDeselectAllCommand }

function TdxDeselectAllCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxDeselectAllCommand.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

procedure TdxDeselectAllCommand.ChangeSelection(ASelection: TdxSelection);
begin
  DocumentModel.Selection.ClearMultiSelection;
  ASelection.Start := 0;
  ASelection.&End := 0;
end;

procedure TdxDeselectAllCommand.EnsureCaretVisibleVertically;
begin
end;

class function TdxDeselectAllCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeselectAllDescription);
end;

function TdxDeselectAllCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxDeselectAllCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeselectAllMenuCaption);
end;

class function TdxDeselectAllCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DeselectAll;
end;

function TdxDeselectAllCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxDeselectAllCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxDeselectAllCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

{ TdxExtendSelectionByParagraphsCommand }

function TdxExtendSelectionByParagraphsCommand.CreateIterator: TdxPieceTableIterator;
begin
  Result := TdxParagraphsDocumentModelIterator.Create(ActivePieceTable);
end;

{ TdxSelectLineCommand }

procedure TdxSelectLineCommand.ChangeSelection(ASelection: TdxSelection;
  ALogPosition: Integer; AHitTestResult: TdxRichEditHitTestResult);
var
  AValidator: TdxFieldIsSelectValidator;
begin
  DocumentModel.Selection.ClearMultiSelection;
  inherited ChangeSelection(ASelection, ALogPosition, AHitTestResult);
  AValidator := TdxFieldIsSelectValidator.Create(ActivePieceTable);
  try
    AValidator.ValidateSelection(ASelection);
  finally
    AValidator.Free;
  end;
end;

function TdxSelectLineCommand.ChangeSelectionEnd(ASelection: TdxSelection;
  ALogPosition: Integer; AHitTestResult: TdxRichEditHitTestResult): Boolean;
begin
  Result := SelectToTheEndOfRow(ASelection, AHitTestResult.Row, True);
end;

procedure TdxSelectLineCommand.ChangeSelectionStart(ASelection: TdxSelection; ALogPosition: TdxDocumentLogPosition;
  AHitTestResult: TdxRichEditHitTestResult; ASelectionItemCountBeforeChangeEnd: Integer);
var
  APos: TdxDocumentLogPosition;
begin
  APos := AHitTestResult.Row.GetFirstPosition(ActivePieceTable).LogPosition;
  if APos > 0 then
    ASelection.Start := APos - 1
  else
    ASelection.Start := APos;
  ASelection.ClearMultiSelection;
  ASelection.SetStartCell(ALogPosition);
  ASelection.Start := APos;
end;

function TdxSelectLineCommand.ExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxSelectLineCommand.HitTestOnlyInPageClientBounds: Boolean;
begin
  Result := False;
end;

{ TdxExtendSelectionByLinesCommand }

function TdxExtendSelectionByLinesCommand.ChangeSelectionEnd(
  ASelection: TdxSelection; ALogPosition: Integer;
  AHitTestResult: TdxRichEditHitTestResult): Boolean;
begin
  if AHitTestResult.Character.GetFirstPosition(ActivePieceTable).LogPosition >= InitialLogPosition then
    Result := ExtendEnd1(ASelection, AHitTestResult)
  else
  begin
    ASelection.&End := ExtendEnd2(AHitTestResult);
    Result := False;
  end;
end;

procedure TdxExtendSelectionByLinesCommand.ChangeSelectionStart(ASelection: TdxSelection;
  ALogPosition: TdxDocumentLogPosition; AHitTestResult: TdxRichEditHitTestResult;
  ASelectionItemCountBeforeChangeEnd: Integer);
begin
  if AHitTestResult.Character.GetFirstPosition(ActivePieceTable).LogPosition >= InitialLogPosition then
    ASelection.Start := ExtendStart1
  else
    ASelection.Start := ExtendStart2;
end;

function TdxExtendSelectionByLinesCommand.ExtendEnd1(ASelection: TdxSelection;
  AHitTestResult: TdxRichEditHitTestResult): Boolean;
begin
  Result := SelectToTheEndOfRow(ASelection, AHitTestResult.Row, True);
end;

function TdxExtendSelectionByLinesCommand.ExtendEnd2(
  AHitTestResult: TdxRichEditHitTestResult): TdxDocumentLogPosition;
begin
  Result := AHitTestResult.Row.GetFirstPosition(ActivePieceTable).LogPosition;
end;

function TdxExtendSelectionByLinesCommand.ExtendStart1: TdxDocumentLogPosition;
begin
  Result := InitialLogPosition;
end;

function TdxExtendSelectionByLinesCommand.ExtendStart2: TdxDocumentLogPosition;
begin
  Result := InitialBox.GetLastPosition(ActivePieceTable).LogPosition + 1;
end;

{ TdxSelectFieldNextToCaretCommand }

function TdxSelectFieldNextToCaretCommand.CanChangePosition(
  const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxSelectFieldNextToCaretCommand.ChangePosition(
  const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

class function TdxSelectFieldNextToCaretCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectFieldNextToCaretDescription)
end;

class function TdxSelectFieldNextToCaretCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectFieldNextToCaretMenuCaption)
end;

{ TdxSelectionBasedCommandBase }

function TdxSelectionBasedCommandBase.CalculateStartPosition(AItem: TdxSelectionItem; AllowSelectionExpanding: Boolean): TdxDocumentModelPosition;
begin
  Result := AItem.CalculateStartPosition(AllowSelectionExpanding);
end;

function TdxSelectionBasedCommandBase.CalculateEndPosition(AItem: TdxSelectionItem; AllowSelectionExpanding: Boolean): TdxDocumentModelPosition;
begin
  Result := AItem.CalculateEndPosition(AllowSelectionExpanding);
end;

{ TdxExtendSelectionToPhysicalPointCommand }

function TdxExtendSelectionToPhysicalPointCommand.ExtendSelection: Boolean;
begin
  Result := True;
end;

{ TdxSelectionBasedPropertyChangeCommandBase }

function TdxSelectionBasedPropertyChangeCommandBase.CanEditSelection: Boolean;
var
  AItems: TdxSelectionItems;
  I: Integer;
  AItem: TdxSelectionItem;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AItems := GetSelectionItems;
  Result := True;
  for I := 0 to AItems.Count -1 do
  begin
    AItem := AItems[I];
    AStart := CalculateStartPosition(AItem, True);
    AEnd := CalculateEndPosition(AItem, True);
    if not ActivePieceTable.CanEditRange(AStart.LogPosition, AEnd.LogPosition) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TdxSelectionBasedPropertyChangeCommandBase.ForceExecute(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  if not ValidateUIState(AState) then
    Exit;
  NotifyBeginCommandExecution(AState);
  try
    RichEditControl.BeginUpdate;
    try
      ModifyDocumentModel(AState);
      ActiveView.EnsureCaretVisible;
    finally
      RichEditControl.EndUpdate;
    end;
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

function TdxSelectionBasedPropertyChangeCommandBase.GetSelectionItems: TdxSelectionItems;
begin
  Result := DocumentModel.Selection.Items;
end;

procedure TdxSelectionBasedPropertyChangeCommandBase.ModifyDocumentModel(
  const AState: IdxCommandUIState);
var
  ADocumentModel: TdxDocumentModel;
begin
  ADocumentModel := DocumentModel;
  ADocumentModel.BeginUpdate;
  try
    ModifyDocumentModelCore(AState);
  finally
    ADocumentModel.EndUpdate;
  end;
end;

procedure TdxSelectionBasedPropertyChangeCommandBase.ModifyDocumentModelCore(
  const AState: IdxCommandUIState);
var
  AList: TdxSelectionItems;
  ACount, I: Integer;
  Actions: TdxDocumentModelChangeActions;
  AItem: TdxSelectionItem;
  ASelectionValid: Boolean;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AList := GetSelectionItems;
  ACount := AList.Count;
  Actions := [];
  for I := 0 to ACount - 1 do
  begin
    AItem := AList[I];
    ASelectionValid := ValidateSelectionInterval(AItem);
    AStart := CalculateStartPosition(AItem, True);
    AEnd := CalculateEndPosition(AItem, True);
    if ASelectionValid then
      Actions := Actions + ChangeProperty(AStart, AEnd, AState);
  end;
  ActivePieceTable.ApplyChangesCore(Actions, -1, -1);
end;

function TdxSelectionBasedPropertyChangeCommandBase.ValidateSelectionInterval(
  AItem: TdxSelectionItem): Boolean;
begin
  Result := True;
end;

function TdxSelectionBasedPropertyChangeCommandBase.ValidateUIState(
  const AState: IdxCommandUIState): Boolean;
begin
  Result := True;
end;

function TdxSelectionBasedPropertyChangeCommandBase.ActivePieceTableObtainParagraphsPropertyValue<T>(ALogPositionStart: TdxDocumentLogPosition;
  ALength: Integer; AModifier: TdxParagraphPropertyModifier<T>; out AValue: T): Boolean;
var
  AInfo: TdxRunInfo;
  I: TdxParagraphIndex;
  AParagraphValue: T;
  AActivePieceTable: TdxPieceTable;
begin
  AActivePieceTable := ActivePieceTable;
  AInfo := AActivePieceTable.FindRunInfo(ALogPositionStart, ALength);
  try
    AValue := AModifier.GetParagraphPropertyValue(AActivePieceTable.Paragraphs[AInfo.Start.ParagraphIndex]);
    Result := True;
    for I := AInfo.Start.ParagraphIndex + 1 to AInfo.&End.ParagraphIndex do
    begin
      AParagraphValue := AModifier.GetParagraphPropertyValue(AActivePieceTable.Paragraphs[I]);
      if not AModifier.IsValueEquals(AParagraphValue, AValue) then
      begin
        Result := False;
        Break;
      end;
    end;
  finally
    AInfo.Free;
  end;
end;

function TdxSelectionBasedPropertyChangeCommandBase.ActivePieceTableObtainRunsPropertyValue<T>(
  const ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxRunPropertyModifier<T>;
  out AValue: T): Boolean;
var
  AInfo: TdxRunInfo;
  I: TdxRunIndex;
  ARunValue: T;
  AActivePieceTable: TdxPieceTable;
begin
  AActivePieceTable := ActivePieceTable;
  AInfo := AActivePieceTable.FindRunInfo(ALogPositionStart, ALength);
  try
    Result := True;
    AValue := AModifier.GetRunPropertyValue(AActivePieceTable.Runs[AInfo.Start.RunIndex]);
    for I := AInfo.Start.RunIndex + 1 to AInfo.&End.RunIndex do
    begin
      ARunValue := AModifier.GetRunPropertyValue(AActivePieceTable.Runs[I]);
      Result := AModifier.IsValueEquals(AValue, ARunValue);
      if not Result then
        Break;
    end;
  finally
    AInfo.Free;
  end;
end;

{ TdxSimpleSetSelectionCommand }

procedure TdxSimpleSetSelectionCommand.ExecuteCore;
var
  ADocumentModel: TdxDocumentModel;
  ASelection: TdxSelection;
begin
  ADocumentModel := DocumentModel;
  ASelection := ADocumentModel.Selection;
  ADocumentModel.BeginUpdate;
  try
    ASelection.Start := FPos.LogPosition;
    ASelection.&End := FPos.LogPosition;
  finally
    ADocumentModel.EndUpdate;
  end;
end;

class function TdxSimpleSetSelectionCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

class function TdxSimpleSetSelectionCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

procedure TdxSimpleSetSelectionCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
  AState.Checked := False;
  AState.Enabled := True;
  AState.Visible := True;
end;

{ TdxTableElementAccessorBase }

destructor TdxTableElementAccessorBase.Destroy;
begin
  inherited Destroy;
end;

function TdxTableElementAccessorBase.GetEndColumnIndex: Integer;
begin
  Result := GetStartColumnIndex + ModelColumnSpan;
end;

function TdxTableElementAccessorBase.CoercePreferredWidth(AInfo: TdxWidthUnitInfo): TdxWidthUnitInfo;
begin
  if ModelColumnSpan > 0 then
    Result := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, Math.Max(AInfo.Value, 1))
  else
    Result := TdxWidthUnitInfo.Create(TdxWidthUnitType.Nil, 0);
end;

function TdxTableElementAccessorBase.IsRightBoundarySelected(ASelectedCells: TdxSelectedCellsCollection): Boolean;
begin
  Result := IsElementSelected(ASelectedCells) or IsNextElementSelected(ASelectedCells);
end;

function TdxTableElementAccessorBase.IsNextElementSelected(ASelectedCells: TdxSelectedCellsCollection): Boolean;
var
  ANextElement: TdxTableElementAccessorBase;
begin
  ANextElement := GetNextElement;
  try
    Result := (ANextElement <> nil) and ANextElement.IsElementSelected(ASelectedCells);
  finally
    ANextElement.Free;
  end;
end;

{ TdxTableRowAccessorBase }

constructor TdxTableRowAccessorBase.Create(ARow: TdxTableRow);
begin
  inherited Create;
  Assert(ARow <> nil);
  FRow := ARow;
end;

function TdxTableRowAccessorBase.GetRow: TdxTableRow;
begin
  Result := FRow;
end;

function TdxTableRowAccessorBase.GetVerticalSpanElements: TdxTableElementAccessorBaseList;
begin
  Result := TdxTableElementAccessorBaseList.Create;
  Result.Add(Self);
end;

function TdxTableRowAccessorBase.GetMinContentWidth: Integer;
begin
  Result := 0;
end;

{ TdxTableElementAccessorBaseList }

constructor TdxTableElementAccessorBaseList.Create(AOwnsObjects: Boolean = False);
begin
  inherited Create(AOwnsObjects);
end;

function TdxTableElementAccessorBaseList.GetItem(Index: Integer): TdxTableElementAccessorBase;
begin
  Result := TdxTableElementAccessorBase(inherited Items[Index]);
end;

{ TdxTableCellAccessor }

constructor TdxTableCellAccessor.Create(ACell: TdxTableCell);
begin
  inherited Create;
  FCell := ACell;
end;

function TdxTableCellAccessor.GetRow: TdxTableRow;
begin
  Result := Cell.Row;
end;

function TdxTableCellAccessor.GetLayoutColumnSpan: Integer;
begin
  Result := Cell.LayoutProperties.ColumnSpan;
end;

function TdxTableCellAccessor.GetModelColumnSpan: Integer;
begin
  Result := Cell.ColumnSpan;
end;

procedure TdxTableCellAccessor.SetModelColumnSpan(const AValue: Integer);
begin
  Cell.Properties.ColumnSpan := AValue;
end;

function TdxTableCellAccessor.GetPreferredWidth: TdxWidthUnitInfo;
begin
  Result := Cell.PreferredWidth.Info;
end;

procedure TdxTableCellAccessor.SetPreferredWidth(const AValue: TdxWidthUnitInfo);
var
  AInfo: TdxWidthUnitInfo;
begin
  AInfo := CoercePreferredWidth(AValue);
  try
    Cell.Properties.PreferredWidth.CopyFrom(AInfo);
  finally
    AInfo.Free;
  end;
end;

function TdxTableCellAccessor.GetNextElement: TdxTableElementAccessorBase;
var
  ARow: TdxTableRow;
  ACells: TdxTableCellCollection;
  ACellIndex: Integer;
begin
  ARow := Cell.Row;
  ACells := ARow.Cells;
  ACellIndex := ACells.IndexOf(Cell);
  if ACellIndex + 1 < ACells.Count then
    Result := TdxTableCellAccessor.Create(ACells[ACellIndex + 1])
  else
    Result := TdxTableRowAfterAccessor.Create(ARow);
end;

function TdxTableCellAccessor.Equals(AObj: TObject): Boolean;
begin
  if AObj is TdxTableCellAccessor then
    Result := Cell = TdxTableCellAccessor(AObj).Cell
  else
    Result := False;
end;

function TdxTableCellAccessor.GetHashCode: Integer;
begin
  Result := Cell.GetHashCode;
end;

function TdxTableCellAccessor.GetStartColumnIndex: Integer;
begin
  Result := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(Cell, False);
end;

function TdxTableCellAccessor.GetVerticalSpanElements: TdxTableElementAccessorBaseList;
var
  ACells: TdxTableCellList;
  ACount, I: Integer;
begin
  ACells := TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(Cell, GetStartColumnIndex, False);
  try
    Result := TdxTableElementAccessorBaseList.Create;
    ACount := ACells.Count;
    for I := 0 to ACount - 1 do
      Result.Add(TdxTableCellAccessor.Create(ACells[I]));
  finally
    ACells.Free;
  end;
end;

function TdxTableCellAccessor.IsElementSelected(ASelectedCells: TdxSelectedCellsCollection): Boolean;
begin
  Result := ASelectedCells.IsWholeCellSelected(Cell);
end;

function TdxTableCellAccessor.GetMinContentWidth: Integer;
begin
  Result := Cell.LayoutProperties.ContainerWidthsInfo.MinWidth;
end;

function TdxTableCellAccessor.GetMinRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer;
begin
  Result := ATableViewInfo.GetAlignedPosition(TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(Cell, True));
end;

function TdxTableCellAccessor.GetMaxRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer;
var
  ANext: TdxTableElementAccessorBase;
begin
  ANext := GetNextElement;
  try
    Result := ANext.GetRightBorderPosition(ATableViewInfo);
  finally
    ANext.Free;
  end;
end;

function TdxTableCellAccessor.GetRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer;
begin
  Result := ATableViewInfo.GetAlignedPosition(TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(Cell, True) + Cell.LayoutProperties.ColumnSpan);
end;

{ TdxTableRowBeforeAccessor }

function TdxTableRowBeforeAccessor.GetLayoutColumnSpan: Integer;
begin
  Result := Row.LayoutProperties.GridBefore;
end;

function TdxTableRowBeforeAccessor.GetModelColumnSpan: Integer;
begin
  Result := Row.GridBefore;
end;

procedure TdxTableRowBeforeAccessor.SetModelColumnSpan(const AValue: Integer);
begin
  Row.Properties.GridBefore := AValue;
end;

function TdxTableRowBeforeAccessor.GetPreferredWidth: TdxWidthUnitInfo;
begin
  Result := Row.WidthBefore.Info;
end;

procedure TdxTableRowBeforeAccessor.SetPreferredWidth(const AValue: TdxWidthUnitInfo);
var
  AInfo: TdxWidthUnitInfo;
begin
  AInfo := CoercePreferredWidth(AValue);
  try
    Row.Properties.WidthBefore.CopyFrom(AInfo);
  finally
    AInfo.Free;
  end;
end;

function TdxTableRowBeforeAccessor.GetNextElement: TdxTableElementAccessorBase;
begin
  Result := TdxTableCellAccessor.Create(Row.Cells.First);
end;

function TdxTableRowBeforeAccessor.Equals(AObj: TObject): Boolean;
begin
  if AObj is TdxTableRowBeforeAccessor then
    Result := Row = TdxTableRowBeforeAccessor(AObj).Row
  else
    Result := False;
end;

function TdxTableRowBeforeAccessor.GetHashCode: Integer;
begin
  Result := Row.GetHashCode;
end;

function TdxTableRowBeforeAccessor.GetStartColumnIndex: Integer;
begin
  Result := 0;
end;

function TdxTableRowBeforeAccessor.IsElementSelected(ASelectedCells: TdxSelectedCellsCollection): Boolean;
begin
  Result := False;
end;

function TdxTableRowBeforeAccessor.GetMinRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer;
begin
  Result := MinInt + 1000;
end;

function TdxTableRowBeforeAccessor.GetMaxRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer;
var
  ANext: TdxTableElementAccessorBase;
begin
  ANext := GetNextElement;
  try
    Result := ANext.GetRightBorderPosition(ATableViewInfo);
  finally
    ANext.Free;
  end;
end;

function TdxTableRowBeforeAccessor.GetRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer;
var
  ANext: TdxTableElementAccessorBase;
begin
  ANext := GetNextElement;
  try
    Result := ANext.GetMinRightBorderPosition(ATableViewInfo);
  finally
    ANext.Free;
  end;
end;

{ TdxTableRowAfterAccessor }

function TdxTableRowAfterAccessor.GetLayoutColumnSpan: Integer;
begin
  Result := Row.LayoutProperties.GridAfter;
end;

function TdxTableRowAfterAccessor.GetModelColumnSpan: Integer;
begin
  Result := Row.GridAfter;
end;

procedure TdxTableRowAfterAccessor.SetModelColumnSpan(const AValue: Integer);
begin
  Row.Properties.GridAfter := AValue;
end;

function TdxTableRowAfterAccessor.GetPreferredWidth: TdxWidthUnitInfo;
begin
  Result := Row.WidthAfter.Info;
end;

procedure TdxTableRowAfterAccessor.SetPreferredWidth(const AValue: TdxWidthUnitInfo);
var
  AInfo: TdxWidthUnitInfo;
begin
  AInfo := CoercePreferredWidth(AValue);
  try
    Row.Properties.WidthAfter.CopyFrom(AInfo);
  finally
    AInfo.Free;
  end;
end;

function TdxTableRowAfterAccessor.GetNextElement: TdxTableElementAccessorBase;
begin
  Result := nil;
end;

function TdxTableRowAfterAccessor.Equals(AObj: TObject): Boolean;
begin
  if AObj is TdxTableRowAfterAccessor then
    Result := Row = TdxTableRowAfterAccessor(AObj).Row
  else
    Result := False;
end;

function TdxTableRowAfterAccessor.GetHashCode: Integer;
begin
  Result := Row.GetHashCode;
end;

function TdxTableRowAfterAccessor.GetStartColumnIndex: Integer;
var
  ALastCell: TdxTableCell;
begin
  ALastCell := Row.Cells.Last;
  Result := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(ALastCell, False) + ALastCell.ColumnSpan;
end;

function TdxTableRowAfterAccessor.IsElementSelected(ASelectedCells: TdxSelectedCellsCollection): Boolean;
begin
  Result := False;
end;

function TdxTableRowAfterAccessor.GetMinRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer;
var
  ALastCell: TdxTableCell;
begin
  ALastCell := Row.Cells.Last;
  Result := ATableViewInfo.GetAlignedPosition(TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(ALastCell, True) + ALastCell.LayoutProperties.ColumnSpan);
end;

function TdxTableRowAfterAccessor.GetMaxRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer;
begin
  Result := MaxInt - 1000;
end;

function TdxTableRowAfterAccessor.GetRightBorderPosition(ATableViewInfo: TdxTableViewInfo): Integer;
begin
  Result := MaxInt - 1000;
end;

{ TdxVirtualTableColumn }

constructor TdxVirtualTableColumn.Create;
begin
  inherited Create;
  FElements := TdxTableElementAccessorBaseList.Create(True);
end;

destructor TdxVirtualTableColumn.Destroy;
begin
  TableViewInfo := nil;
  FreeAndNil(FElements);
  inherited Destroy;
end;

procedure TdxVirtualTableColumn.SetTableViewInfo(const Value: TdxTableViewInfo);
begin
  if FTableViewInfo <> Value then
  begin
    TdxTableViewInfo.Release(FTableViewInfo);
    FTableViewInfo := Value;
    TdxTableViewInfo.AddReference(FTableViewInfo);
  end;
end;

end.
