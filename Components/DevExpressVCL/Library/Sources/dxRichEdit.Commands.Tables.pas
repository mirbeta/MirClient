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

unit dxRichEdit.Commands.Tables;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Properties,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Selections.Core,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableCalculator,
  dxRichEdit.Commands,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.Insert,
  dxRichEdit.Commands.Selection,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting;

type

  { TdxInsertTableCoreCommand }

  TdxInsertTableCoreCommand = class(TdxInsertObjectCommandBase)
  strict private
    FRowCount: Integer;
    FColumnCount: Integer;
  protected
    procedure ModifyModel; override;
    function GetColumnWidth: Integer;
    procedure JoinTables(ATable: TdxTable);
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;

    property RowCount: Integer read FRowCount write FRowCount;
    property ColumnCount: Integer read FColumnCount write FColumnCount;
  end;

  { TdxInsertTableRowCoreCommandBase }

  TdxInsertTableRowCoreCommandBase = class abstract(TdxInsertObjectCommandBase)
  strict private
    FRow: TdxTableRow;
  protected
    function GetExtendSelection: Boolean; override;
    procedure ModifyModel; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    procedure ChangeSelection(ARowToSelect: TdxTableRow); reintroduce; virtual;
    function InsertRow(ARow: TdxTableRow): TdxTableRow; virtual; abstract;
  public
    property Row: TdxTableRow read FRow write FRow;
  end;
  TdxInsertTableRowCoreCommandBaseClass = class of TdxInsertTableRowCoreCommandBase;

  { TdxInsertTableColumnCoreCommandBase }

  TdxInsertTableColumnCoreCommandBase = class abstract(TdxInsertObjectCommandBase)
  strict private
    FPatternCell: TdxTableCell;
  protected
    function GetExtendSelection: Boolean; override;
    function InsertColumn(APatternCell: TdxTableCell): TdxTableCell; virtual; abstract;
    procedure ModifyModel; override;
    procedure ChangeSelection(APatternCell: TdxTableCell); reintroduce; virtual;

    property PatternCell: TdxTableCell read FPatternCell write FPatternCell;
  end;
  TdxInsertTableColumnCoreCommandBaseClass = class of TdxInsertTableColumnCoreCommandBase;

  { TdxInsertTableElementCommandBase }

  TdxInsertTableElementCommandBase = class abstract(TdxRichEditCaretBasedCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  end;

  { TdxInsertTableElementMenuCommand }

  TdxInsertTableElementMenuCommand = class(TdxInsertTableElementCommandBase)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxInsertTableRowCommandBase }

  TdxInsertTableRowCommandBase = class abstract(TdxInsertTableElementCommandBase)
  strict private
    FInsertRowCommand: TdxInsertTableRowCoreCommandBase;
  protected
    function GetInsertRowCommand: TdxInsertTableRowCoreCommandBase; virtual;
    class function GetInsertTableRowCommandClass: TdxInsertTableRowCoreCommandBaseClass; virtual; {$IFDEF DELPHIXE2}abstract;{$ENDIF}
    function CreateInsertTableRowCommand(const AControl: IdxRichEditControl): TdxInsertTableRowCoreCommandBase;
    procedure ExecuteCore; override;
    function FindPatternRow: TdxTableRow; virtual;
    function FindPatternRowParagraphIndex(ASelections: TdxSelectionRangeCollection): TdxParagraphIndex; virtual; abstract;

    property InsertRowCommand: TdxInsertTableRowCoreCommandBase read GetInsertRowCommand;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    destructor Destroy; override;

    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxInsertTableColumnCommandBase }

  TdxInsertTableColumnCommandBase = class abstract(TdxInsertTableElementCommandBase)
  strict private
    FInsertColumnCommand: TdxInsertTableColumnCoreCommandBase;
    function GetInsertColumnCommand: TdxInsertTableColumnCoreCommandBase;
  protected
    class function GetInsertTableColumnCommandClass: TdxInsertTableColumnCoreCommandBaseClass; virtual; {$IFDEF DELPHIXE2}abstract;{$ENDIF}
    function CreateInsertTableColumnCommand(const AControl: IdxRichEditControl): TdxInsertTableColumnCoreCommandBase;
    function FindPatternCell: TdxTableCell; virtual; abstract;
    procedure ExecuteCore; override;

    property InsertColumnCommand: TdxInsertTableColumnCoreCommandBase read GetInsertColumnCommand;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    destructor Destroy; override;
  end;

  { TdxInsertTableRowBelowCommand }

  TdxInsertTableRowBelowCommand = class(TdxInsertTableRowCommandBase)
  protected
    class function GetInsertTableRowCommandClass: TdxInsertTableRowCoreCommandBaseClass; override;
    function FindPatternRowParagraphIndex(ASelections: TdxSelectionRangeCollection): TdxParagraphIndex; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
  end;

  { TdxInsertTableRowAboveCommand }

  TdxInsertTableRowAboveCommand = class(TdxInsertTableRowCommandBase)
  protected
    class function GetInsertTableRowCommandClass: TdxInsertTableRowCoreCommandBaseClass; override;
    function FindPatternRowParagraphIndex(ASelections: TdxSelectionRangeCollection): TdxParagraphIndex; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
  end;

  { TdxInsertTableCellsWithShiftToTheHorizontallyCommand }

  TdxInsertTableCellsWithShiftToTheHorizontallyCommand = class(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
  public
    procedure PerformModifyModel; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxInsertTableCellsWithShiftToTheVerticallyCommand }

  TdxInsertTableCellsWithShiftToTheVerticallyCommand = class(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function CalculateInsertedRowsCount(const ACellsInterval: TdxSelectedCellsIntervalInRow): Integer; virtual;
    procedure DeleteContentInCells(ACell: TdxTableCell); virtual;
    procedure DeleteTextInCells(ARow: TdxTableRow; ARowsCount: Integer); virtual;
    procedure TableCellsOperationWithShiftToTheVerticallyCore(const ACellsInterval: TdxSelectedCellsIntervalInRow);
  public
    procedure PerformModifyModel; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxInsertTableColumnToTheLeftCommand }

  TdxInsertTableColumnToTheLeftCommand = class(TdxInsertTableColumnCommandBase)
  protected
    class function GetInsertTableColumnCommandClass: TdxInsertTableColumnCoreCommandBaseClass; override;
    function FindPatternCell: TdxTableCell; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxInsertTableColumnToTheRightCommand }

  TdxInsertTableColumnToTheRightCommand = class(TdxInsertTableColumnCommandBase)
  protected
    class function GetInsertTableColumnCommandClass: TdxInsertTableColumnCoreCommandBaseClass; override;
    function FindPatternCell: TdxTableCell; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxInsertTableRowBelowCoreCommand }

  TdxInsertTableRowBelowCoreCommand = class(TdxInsertTableRowCoreCommandBase)
  protected
    function InsertRow(ARow: TdxTableRow): TdxTableRow; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTableRowAboveCoreCommand }

  TdxInsertTableRowAboveCoreCommand = class(TdxInsertTableRowCoreCommandBase)
  protected
    function InsertRow(ARow: TdxTableRow): TdxTableRow; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTableColumnToTheLeftCoreCommand }

  TdxInsertTableColumnToTheLeftCoreCommand = class(TdxInsertTableColumnCoreCommandBase)
  protected
    function InsertColumn(APatternCell: TdxTableCell): TdxTableCell; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTableColumnToTheRightCoreCommand }

  TdxInsertTableColumnToTheRightCoreCommand = class(TdxInsertTableColumnCoreCommandBase)
  protected
    function InsertColumn(APatternCell: TdxTableCell): TdxTableCell; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTableCommand }

  TdxInsertTableCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
    procedure InsertTable(ARows: Integer; AColumns: Integer); virtual;
    procedure ForceExecuteCore(const AState: IdxCommandUIState); override;
    procedure ShowInsertTableFormCallback(const AParameters: TdxCreateTableParameters; ACallbackData: TObject);
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;
    procedure ShowInsertTableForm(AParameters: TdxCreateTableParameters; const ACallback: TdxShowInsertTableFormCallback; ACallbackData: TObject);
    procedure UpdateUIState(const AState: IdxCommandUIState); override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertDeleteTableCellsDispatcherCommandBase }

  TdxInsertDeleteTableCellsDispatcherCommandBase = class abstract(TdxRichEditMenuItemSimpleCommand)
  strict private
    FCellsParameters: TdxTableCellsParameters;
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    procedure TableCellsOperationWithShiftToTheHorizontally; virtual; abstract;
    procedure TableCellsOperationWithShiftToTheVertically; virtual; abstract;
    procedure TableCellsOperationWithRow; virtual; abstract;
    procedure TableCellsOperationWithColumn; virtual; abstract;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;

    property CellsParameters: TdxTableCellsParameters read FCellsParameters;
  end;

  TdxInsertTableCellsDispatcherCommand = class(TdxInsertDeleteTableCellsDispatcherCommandBase)
  protected
    procedure TableCellsOperationWithColumn; override;
    procedure TableCellsOperationWithRow; override;
    procedure TableCellsOperationWithShiftToTheHorizontally; override;
    procedure TableCellsOperationWithShiftToTheVertically; override;
  public
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxSelectTableRowCommand }

  TdxSelectTableRowCommand = class(TdxRichEditSelectionCommand)
  strict private
    FRows: TdxTableRowCollection;
    FStartRowIndex: Integer;
    FEndRowIndex: Integer;
    FCanCalculateExecutionParameters: Boolean;
  protected
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function PerformChangeSelection: Boolean; override;
    procedure CalculateExecutionParameters; virtual;
    function GetSelectedCells: TdxSelectedCellsCollection; virtual;
    procedure GetSelectedCellsCore(ASelectedCells: TdxSelectedCellsCollection; AIndex: Integer); virtual;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    procedure ChangeSelection(ASelection: TdxSelection); override;

    class function Id: TdxRichEditCommandId; override;
    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;

    property Rows: TdxTableRowCollection read FRows write FRows;
    property StartRowIndex: Integer read FStartRowIndex write FStartRowIndex;
    property EndRowIndex: Integer read FEndRowIndex write FEndRowIndex;
    property CanCalculateExecutionParameters: Boolean read FCanCalculateExecutionParameters write FCanCalculateExecutionParameters;
  end;

  { TdxSelectTableColumnsCommand }

  TdxSelectTableColumnsCommand = class(TdxRichEditSelectionCommand)
  strict private
    FRows: TdxTableRowCollection;
    FStartColumnIndex: Integer;
    FEndColumnIndex: Integer;
    FCanCalculateExecutionParameters: Boolean;
  protected
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function PerformChangeSelection: Boolean; override;
    procedure CalculateExecutionParameters; virtual;
    function GetStartCell: TdxTableCell; virtual;
    function GetCellByColumnIndex(ARow: TdxTableRow; AColumnIndex: Integer): TdxTableCell; virtual;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    procedure EnsureCaretVisibleVertically; override;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    procedure ChangeSelection(ASelection: TdxSelection); override;

    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property Rows: TdxTableRowCollection read FRows write FRows;
    property StartColumnIndex: Integer read FStartColumnIndex write FStartColumnIndex;
    property EndColumnIndex: Integer read FEndColumnIndex write FEndColumnIndex;
    property CanCalculateExecutionParameters: Boolean read FCanCalculateExecutionParameters write FCanCalculateExecutionParameters;
  end;

  { TdxSelectTableCellCommand }

  TdxSelectTableCellCommand = class(TdxRichEditSelectionCommand)
  strict private
    FCell: TdxTableCell;
    FShouldUpdateCaretY: Boolean;
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetShouldUpdateCaretY: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function PerformChangeSelection: Boolean; override;
    procedure CalculateSelectedCell;
  public
    procedure ChangeSelection(ASelection: TdxSelection); override;
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property ShouldUpdateCaretTableAnchorVerticalPositionY: Boolean read FShouldUpdateCaretY write FShouldUpdateCaretY;
    property Cell: TdxTableCell read FCell write FCell;
  end;

  { TdxSplitTableCommand }

  TdxSplitTableCommand = class(TdxRichEditSelectionCommand)
  strict private
    FFirstCellLogPosition: TdxDocumentLogPosition;
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    function CheckSelection: Boolean;
    function GetEndCell(ASelection: TdxSelection): TdxTableCell;
    function GetExtendSelection: Boolean; override;
    function GetStartCell(ASelection: TdxSelection): TdxTableCell;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); override;
    procedure PerformModifyModel; override;
    procedure PerformTableSplitBySelectionStart;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxChangeColumnSizeCommand }

  TdxChangeColumnSizeCommand = class abstract(TdxRichEditCommand)
  strict private
    FColumnIndex: Integer;
    FOffset: Integer;
    FSectionProperties: TdxSectionProperties;
    FDefaultTabWidth: Integer;
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function GetLeftMargin(AOldMargin: Integer): Integer;
    function GetRightMargin(AOldMargin: Integer): Integer;

    property DefaultTabWidth: Integer read FDefaultTabWidth;
  public
    constructor Create(const AControl: IdxRichEditControl; ASectionProperties: TdxSectionProperties; AColumnIndex: Integer; AOffset: Integer); reintroduce;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function GetNewSection(ASection: TdxSectionProperties): TdxSectionProperties; virtual; abstract;

    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;

    property Offset: Integer read FOffset;
    property ColumnIndex: Integer read FColumnIndex;
  end;

  { TdxChangeColumnWidthCommand }

  TdxChangeColumnWidthCommand = class(TdxChangeColumnSizeCommand)
  protected
    procedure ChangeColumnSize(ASectionProperties: TdxSectionProperties; AWidth: Integer);
    procedure ChangeColumnSizeBySpace(AWidth: Integer; AColumn: TdxColumnInfo);
    procedure ChangeColumnSizeByMargin(ASectionProperties: TdxSectionProperties; AWidth: Integer; AColumn: TdxColumnInfo);
  public
    function GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties; override;
  end;

  { TdxMoveColumnCommand }

  TdxMoveColumnCommand = class(TdxChangeColumnSizeCommand)
  public
    function GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties; override;
  end;

  { TdxChangeColumnSizeByLeftCommand }

  TdxChangeColumnSizeByLeftCommand = class(TdxChangeColumnSizeCommand)
  protected
    procedure ChangeColumnSize(ASectionProperties: TdxSectionProperties; AWidth: Integer);
    procedure ChangeColumnSizeByMargin(ASectionProperties: TdxSectionProperties; AColumn: TdxColumnInfo; AWidth: Integer);
    procedure ChangeColumnSizeBySpace(AColumns: TdxColumnInfoCollection; AColumn: TdxColumnInfo; AWidth: Integer);
  public
    function GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties; override;
  end;

  { TdxChangeSectionHeightByTopCommand }

  TdxChangeSectionHeightByTopCommand = class(TdxChangeColumnSizeCommand)
  public
    function GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties; override;
  end;

  { TdxChangeSectionHeightByBottomCommand }

  TdxChangeSectionHeightByBottomCommand = class(TdxChangeColumnSizeCommand)
  public
    function GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties; override;
  end;

  { TdxChangeWidthEqualWidthColumnsByLeftCommand }

  TdxChangeWidthEqualWidthColumnsByLeftCommand = class(TdxChangeColumnSizeCommand)
  public
    function GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties; override;
  end;

  { TdxChangeWidthEqualWidthColumnsByRightCommand }

  TdxChangeWidthEqualWidthColumnsByRightCommand = class(TdxChangeColumnSizeCommand)
  public
    function GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties; override;
  end;

  { TdxTableElementPair }

  TdxTableElementPair = class
  strict private
    FElement: TdxTableElementAccessorBase;
    FNextElement: TdxTableElementAccessorBase;
    FNextElementValid: Boolean;
    function GetNextElement: TdxTableElementAccessorBase;
  protected
    function DoGetNextElement: TdxTableElementAccessorBase; virtual;
  public
    constructor Create(AElement: TdxTableElementAccessorBase);
    destructor Destroy; override;

    property Element: TdxTableElementAccessorBase read FElement;
    property NextElement: TdxTableElementAccessorBase read GetNextElement;
  end;

  { TdxTableElementPairUniqueCollection }

  TdxTableElementPairUniqueCollection = class
  strict private
    FInnerList: TdxObjectList<TdxTableElementPair>;
    FExtractItems: TdxTableElementAccessorBaseList;
    FMapElementPairIndex: TObjectDictionary<TdxTableElementAccessorBase, Integer>;
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxTableElementPair;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AElement: TdxTableElementAccessorBase; AOwns: Boolean): Integer;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxTableElementPair read GetItem; default;
  end;

  { TdxFromAutoToRealWidthsTableCalculator }

  TdxFromAutoToRealWidthsTableCalculator = class
  public
    class procedure ApplyRealWidths(ATableViewInfo: TdxTableViewInfo); static;
    class procedure ApplyRealWidthToTable(ATable: TdxTable; APositions: TdxSortedList<Integer>); static;
    class function ApplyNewWidth(APairs: TdxTableElementPairUniqueCollection; AInitialPositions: TdxSortedList<Integer>;
      ANewModelPositionIndex: Integer; AConverter: TdxDocumentModelUnitToLayoutUnitConverter; AColumn: TdxVirtualTableColumn): Boolean; static;
    class function GetPreferredWidthByTotalWidth(AElement: TdxTableElementAccessorBase; ATotalWidth: TdxLayoutUnit): TdxLayoutUnit; overload; static;
    class function GetPreferredWidthByTotalWidth(ACell: TdxTableCell; ATotalWidth: TdxLayoutUnit): TdxLayoutUnit; overload; static;
    class procedure ApplyRealWidthToRow(ARow: TdxTableRow; APositions: TdxSortedList<Integer>); static;
    class procedure ApplyRealWidthToCell(ACell: TdxTableCell; APositions: TdxSortedList<Integer>); static;
    class function ShouldSetNewPreferredWidth(ATable: TdxTable; ANewPreferredWidth: TdxModelUnit): Boolean; overload; static;
    class function ShouldSetNewPreferredWidth(ACell: TdxTableCell; ANewPreferredWidth: TdxModelUnit): Boolean; overload; static;
    class function ShouldSetNewWidthBefore(ARow: TdxTableRow; ANewWidth: TdxModelUnit): Boolean; static;
    class function ShouldSetNewWidthAfter(ARow: TdxTableRow; ANewWidth: TdxModelUnit): Boolean; static;
    class function ShouldSetNewWidthCore(ACurrentWidth: TdxWidthUnit; ANewWidth: TdxModelUnit; AConverter: TdxDocumentModelUnitToLayoutUnitConverter): Boolean; static;
  end;

  { TdxChangeTableVirtualColumnRightCommand }

  TdxChangeTableVirtualColumnRightCommand = class(TdxRichEditSelectionCommand)
  strict private
    FColumn: TdxVirtualTableColumn;
    FValue: TdxLayoutUnit;
  protected
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function PerformChangeSelection: Boolean; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function CalculateModelPositionByLayoutPosition(ARow: TdxTableRow; ALayoutPosition: Integer; AInitialPositions: TdxSortedList<Integer>; AAlignmentPositions: TdxSortedList<Integer>): TdxLayoutUnit; virtual;
    function FindModelPositionIndexByLayoutPositionIndex(ATableRow: TdxTableRow; ALayoutPosition: Integer): Integer; virtual;
    function GetActualWidth(AWidth: TdxWidthUnit): TdxModelUnit; virtual;
    procedure SetTablePreferredWidth(ATable: TdxTable; AInitialPositions: TdxSortedList<Integer>);
    procedure NormalizeTable(ATable: TdxTable; AInitialPositions: TdxLayoutUnitSortedList); virtual;
    function IsPairsValid(APairs: TdxTableElementPairUniqueCollection): Boolean; virtual;
    function CreateTableElementPairUniqueCollection(AElements: TdxTableElementAccessorBaseList): TdxTableElementPairUniqueCollection; virtual;
    procedure AddCellCore(APairs: TdxTableElementPairUniqueCollection; AElement: TdxTableElementAccessorBase); virtual;
    procedure ChangeCellSpans(ATable: TdxTable; AColumnIndex: Integer); virtual;
    procedure ChangeTableLeftOffset; virtual;
  public
    constructor Create(const AControl: IdxRichEditControl; AColumn: TdxVirtualTableColumn; AValue: TdxLayoutUnit); reintroduce;
    procedure PerformModifyModel; override;

    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxChangeTableRowHeightCommand }

  TdxChangeTableRowHeightCommand = class(TdxRichEditSelectionCommand)
  strict private
    FRow: TdxTableRow;
    FValue: Integer;
  protected
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function PerformChangeSelection: Boolean; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property Row: TdxTableRow read FRow;
  public
    constructor Create(const AControl: IdxRichEditControl; ARow: TdxTableRow; AValue: Integer); reintroduce;
    procedure PerformModifyModel; override;

    class function GetMenuCaption: string; override;
    class function GetDescription: string; override;
  end;

  { TdxDeleteTableCellsDispatcherCommand }

  TdxDeleteTableCellsDispatcherCommand = class(TdxInsertDeleteTableCellsDispatcherCommandBase)
  protected
    procedure TableCellsOperationWithColumn; override;
    procedure TableCellsOperationWithRow; override;
    procedure TableCellsOperationWithShiftToTheHorizontally; override;
    procedure TableCellsOperationWithShiftToTheVertically; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDeleteTableRowsCommand }

  TdxDeleteTableRowsCommand = class(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    procedure PerformModifyModelCore(ASelectedRows: TdxTableRowList); virtual;
    procedure UpdateUIStateEnabled(const AState: IdxCommandUIState); virtual;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure PerformModifyModel; override;
  end;

  { TdxDeleteTableRowsMenuCommand }

  TdxDeleteTableRowsMenuCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDeleteTableColumnsCommand }

  TdxDeleteTableColumnsCommand = class(TdxRichEditSelectionCommand)
  strict private
    FNewLogPosition: TdxDocumentLogPosition;
    FIsNotEmptySelectedCells: Boolean;
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    function CalculateNewLogPosition(ASelectedCells: TdxSelectedCellsCollection): TdxDocumentLogPosition; virtual;
    function CanDeleteTableColumns: Boolean; virtual;
    procedure PerformModifyModelCore(ASelectedCellsCollection: TdxSelectedCellsCollection); virtual;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure PerformModifyModel; override;
  end;

  { TdxDeleteTableColumnsMenuCommand }

  TdxDeleteTableColumnsMenuCommand = class(TdxDeleteTableColumnsCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxDeleteTableCellsWithShiftToTheHorizontallyCommand }

  TdxDeleteTableCellsWithShiftToTheHorizontallyCommand = class(TdxRichEditSelectionCommand)
  strict private
    procedure DeleteTableCellWithContentKnownWidths(ADeletedCell: TdxTableCell; ACanNormalizeCellVerticalMerging: Boolean;
      const AServer: IdxInnerRichEditDocumentServerOwner; const AContainer: TdxTableWidthsContainer; AUseDeltaBetweenColumnsUpdate: Boolean);
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;

    procedure DeleteEntireRow(ADeletedRow: TdxTableRow); virtual;
    procedure DeleteEntireTable; virtual;
    function IsSelectedEntireRow(const ACellsInterval: TdxSelectedCellsIntervalInRow): Boolean; virtual;
    function IsSelectedEntireTable(ASelectedCells: TdxSelectedCellsCollection): Boolean; virtual;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure PerformModifyModel; override;
  end;

  { TdxDeleteTableCellsWithShiftToTheVerticallyCommand }

  TdxDeleteTableCellsWithShiftToTheVerticallyCommand = class(TdxRichEditSelectionCommand)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure PerformModifyModel; override;
  end;

  { TdxDeleteTableCoreCommand }

  TdxDeleteTableCoreCommand = class(TdxRichEditSelectionCommand)
  strict private
    FDeletedTable: TdxTable;
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure PerformModifyModel; override;

    property DeletedTable: TdxTable read FDeletedTable write FDeletedTable;
  end;

  { TdxDeleteTableCommand }

  TdxDeleteTableCommand = class(TdxDeleteTableCoreCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    procedure PerformModifyModel; override;
  end;

  { TdxToggleTableAutoFitPlaceholderCommand }

  TdxToggleTableAutoFitPlaceholderCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableAutoFitPlaceholderMenuCommand }

  TdxToggleTableAutoFitPlaceholderMenuCommand = class(TdxToggleTableAutoFitPlaceholderCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxToggleTableAutoFitCommandBase }

  TdxToggleTableAutoFitCommandBase = class abstract (TdxRichEditMenuItemSimpleCommand)
  protected
    function GetTable: TdxTable; virtual;
    procedure ExecuteCore; override;
    procedure ApplyRealWidths; virtual;
    function GetTableViewInfo(APosition: TdxDocumentLayoutPosition): TdxTableViewInfo;
    procedure ApplyTableProperties; virtual;
    procedure ApplyTableCellProperties; virtual;
    function GetTableWidthInfo: TdxWidthUnitInfo; virtual; abstract;
    function GetTableCellWidthInfo(ACell: TdxTableCell; ARowWidth: Double): TdxWidthUnitInfo; virtual; abstract;
    function CalculateRowWidth(ARow: TdxTableRow): Integer; virtual; abstract;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property Table: TdxTable read GetTable;
  end;

  { TdxToggleShowTableGridLinesCommand }

  TdxToggleShowTableGridLinesCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableAutoFitContentsCommand }

  TdxToggleTableAutoFitContentsCommand = class(TdxToggleTableAutoFitCommandBase)
  protected
    procedure ApplyRealWidths; override;
    function GetTableWidthInfo: TdxWidthUnitInfo; override;
    function GetTableCellWidthInfo(ACell: TdxTableCell; ARowWidth: Double): TdxWidthUnitInfo; override;
    function CalculateRowWidth(ARow: TdxTableRow): Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableAutoFitWindowCommand }

  TdxToggleTableAutoFitWindowCommand = class(TdxToggleTableAutoFitCommandBase)
  strict private
    FHundredPercent: Integer;
  protected
    function GetTableWidthInfo: TdxWidthUnitInfo; override;
    function GetTableCellWidthInfo(ACell: TdxTableCell; ARowWidth: Double): TdxWidthUnitInfo; override;
    function CalculateRowWidth(ARow: TdxTableRow): Integer; override;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableFixedColumnWidthCommand }

  TdxToggleTableFixedColumnWidthCommand = class(TdxToggleTableAutoFitCommandBase)
  protected
    procedure ApplyTableProperties; override;
    procedure ApplyTableCellProperties; override;
    function GetTableWidthInfo: TdxWidthUnitInfo; override;
    function GetTableCellWidthInfo(ACell: TdxTableCell; ARowWidth: Double): TdxWidthUnitInfo; override;
    function CalculateRowWidth(ARow: TdxTableRow): Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxTableStylesGalleryCommand }

  TdxTableStylesGalleryCommand = class(TdxRichEditCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    class function GetDescription: string; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  Math, Classes, Contnrs, dxCore, dxTypeHelpers,

  dxCoreClasses,
  dxRichEdit.Types,
  dxRichEdit.Commands.Images,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.DocumentModel.History.Table,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.Options;

{ TdxInsertTableCoreCommand }

class function TdxInsertTableCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableDescription);
end;

class function TdxInsertTableCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableMenuCaption);
end;

function TdxInsertTableCoreCommand.GetColumnWidth: Integer;
var
  ACaretPosition: TdxCaretPosition;
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  ALayoutPosition: TdxDocumentLayoutPosition;
  AWidth: Integer;
begin
  ActiveView.EnsureFormattingCompleteForSelection;
  ACaretPosition := ActiveView.CaretPosition;
  AUnitConverter := DocumentModel.ToDocumentLayoutUnitConverter;
  if ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.TableCell) then
  begin
    ALayoutPosition := CaretPosition.LayoutPosition;
    if ALayoutPosition.TableCell = nil then
      AWidth := ALayoutPosition.Column.Bounds.Width
    else
      AWidth := ALayoutPosition.TableCell.TextWidth;
    if ActiveView.MatchHorizontalTableIndentsToTextEdge then
      AWidth := Max(AWidth - 1, 1);
    Result := AUnitConverter.ToModelUnits(AWidth);
  end
  else
    Result := 0;
end;

procedure TdxInsertTableCoreCommand.JoinTables(ATable: TdxTable);
var
  AFirstParagraphInTableIndex: TdxParagraphIndex;
  APreviousParagraph: TdxParagraph;
  ACell: TdxTableCell;
begin
  AFirstParagraphInTableIndex := ATable.FirstRow.FirstCell.StartParagraphIndex;
  if AFirstParagraphInTableIndex = 0 then
    Exit;
  APreviousParagraph := ActivePieceTable.Paragraphs[AFirstParagraphInTableIndex - 1];
  ACell := APreviousParagraph.GetCell;
  if (ACell = nil) or (ATable.NestedLevel <> ACell.Table.NestedLevel) then
    Exit;

  ActivePieceTable.JoinTables(ACell.Table, ATable);
end;

procedure TdxInsertTableCoreCommand.ModifyModel;
var
  AColumnWidth, AStyleIndex: Integer;
  ALastTable: TdxTable;
begin
  if (RowCount <= 0) or (ColumnCount <= 0) then
    Exit;

  AColumnWidth := GetColumnWidth;
  ActivePieceTable.InsertTable(DocumentModel.Selection.&End, RowCount, ColumnCount, TdxTableAutoFitBehaviorType.FixedColumnWidth,
    MinInt, AColumnWidth, GetForceVisible, ActiveView.MatchHorizontalTableIndentsToTextEdge);
  ALastTable := ActivePieceTable.Tables.Last;
  AStyleIndex := DocumentModel.TableStyles.GetStyleIndexByName(TdxTableStyleCollection.TableSimpleStyleName);
  if AStyleIndex >= 0 then
    ALastTable.StyleIndex := AStyleIndex;

  JoinTables(ALastTable);
end;

procedure TdxInsertTableCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  ApplyCommandsRestriction(AState, Options.DocumentCapabilities.Tables, AState.Enabled);
end;

{ TdxInsertTableRowCoreCommandBase }

function TdxInsertTableRowCoreCommandBase.GetExtendSelection: Boolean;
begin
  Result := False;
end;

procedure TdxInsertTableRowCoreCommandBase.ModifyModel;
begin
  if Row <> nil then
    ChangeSelection(InsertRow(Row));
end;

procedure TdxInsertTableRowCoreCommandBase.ChangeSelection(ARowToSelect: TdxTableRow);
var
  AItem: TdxSelectTableRowHistoryItem;
  ARowIndex: Integer;
begin
  AItem := TdxSelectTableRowHistoryItem.Create(DocumentModel.ActivePieceTable);
  AItem.Control := RichEditControl;
  AItem.TableIndex := ARowToSelect.Table.&Index;
  ARowIndex := ARowToSelect.IndexInTable;
  AItem.StartRowIndex := ARowIndex;
  AItem.EndRowIndex := ARowIndex;
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxInsertTableRowCoreCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxInsertTableColumnCoreCommandBase }

function TdxInsertTableColumnCoreCommandBase.GetExtendSelection: Boolean;
begin
  Result := False;
end;

procedure TdxInsertTableColumnCoreCommandBase.ModifyModel;
begin
  ChangeSelection(InsertColumn(PatternCell));
end;

procedure TdxInsertTableColumnCoreCommandBase.ChangeSelection(APatternCell: TdxTableCell);
var
  AItem: TdxSelectTableColumnsHistoryItem;
  AStartColumnIndex: Integer;
begin
  AItem := TdxSelectTableColumnsHistoryItem.Create(DocumentModel.ActivePieceTable);
  AItem.Control := RichEditControl;
  AItem.TableIndex := APatternCell.Table.Index;
  AStartColumnIndex := APatternCell.GetStartColumnIndexConsiderRowGrid;
  AItem.StartColumnIndex := AStartColumnIndex;
  AItem.EndColumnIndex := APatternCell.GetEndColumnIndexConsiderRowGrid(AStartColumnIndex);
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

{ TdxInsertTableElementCommandBase }

procedure TdxInsertTableElementCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := DocumentModel.Selection.IsWholeSelectionInOneTable;
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxInsertTableElementMenuCommand }

class function TdxInsertTableElementMenuCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableElementMenuCaption);
end;

class function TdxInsertTableElementMenuCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableElementMenuCaption);
end;

procedure TdxInsertTableElementMenuCommand.ExecuteCore;
begin
end;

procedure TdxInsertTableElementMenuCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Visible := AState.Enabled;
end;

{ TdxInsertTableRowCommandBase }

constructor TdxInsertTableRowCommandBase.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FInsertRowCommand := CreateInsertTableRowCommand(AControl);
end;

function TdxInsertTableRowCommandBase.CreateInsertTableRowCommand(
  const AControl: IdxRichEditControl): TdxInsertTableRowCoreCommandBase;
begin
  Result := GetInsertTableRowCommandClass.Create(AControl);
end;

destructor TdxInsertTableRowCommandBase.Destroy;
begin
  FreeAndNil(FInsertRowCommand);
  inherited Destroy;
end;

class function TdxInsertTableRowCommandBase.GetDescription: string;
begin
  Result := GetInsertTableRowCommandClass.GetDescription;
end;

{$IFNDEF DELPHIXE2}
class function TdxInsertTableRowCommandBase.GetInsertTableRowCommandClass: TdxInsertTableRowCoreCommandBaseClass;
begin
  raise Exception.Create('for C++Builder XE');
end;
{$ENDIF}

function TdxInsertTableRowCommandBase.GetInsertRowCommand: TdxInsertTableRowCoreCommandBase;
begin
  Result := FInsertRowCommand;
end;

class function TdxInsertTableRowCommandBase.GetMenuCaption: string;
begin
  Result := GetInsertTableRowCommandClass.GetMenuCaption;
end;

procedure TdxInsertTableRowCommandBase.ExecuteCore;
var
  APatternRow: TdxTableRow;
begin
  APatternRow := FindPatternRow;
  if APatternRow = nil then
    Exit;
  InsertRowCommand.Row := APatternRow;
  InsertRowCommand.PerformModifyModel;
end;

function TdxInsertTableRowCommandBase.FindPatternRow: TdxTableRow;
var
  ASelections: TdxSelectionRangeCollection;
  AParagraphIndex: TdxParagraphIndex;
  ALastCell: TdxTableCell;
begin
  ASelections := DocumentModel.Selection.GetSortedSelectionCollection;
  try
    AParagraphIndex := FindPatternRowParagraphIndex(ASelections);
  finally
    ASelections.Free;
  end;

  ALastCell := ActivePieceTable.Paragraphs[AParagraphIndex].GetCell;
  if ALastCell = nil then
    Result := nil
  else
    Result := ALastCell.Row;
end;

{ TdxInsertTableRowBelowCoreCommand }

function TdxInsertTableRowBelowCoreCommand.ChangePosition(
  const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
var
  ARows: TdxTableRowCollection;
  ANewRowIndex: Integer;
  AStart: TdxParagraphIndex;
begin
  ARows := Row.Table.Rows;
  ANewRowIndex := ARows.IndexOf(Row) + 1;
  if ANewRowIndex = ARows.Count then
    Result := inherited ChangePosition(APos)
  else
  begin
    AStart := ARows[ANewRowIndex].FirstCell.StartParagraphIndex;
    Result := ActivePieceTable.Paragraphs[AStart].LogPosition;
  end;
end;

class function TdxInsertTableRowBelowCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableRowBelowDescription);
end;

class function TdxInsertTableRowBelowCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableRowBelowMenuCaption);
end;

function TdxInsertTableRowBelowCoreCommand.InsertRow(
  ARow: TdxTableRow): TdxTableRow;
var
  APatternRowIndex: Integer;
begin
  ActivePieceTable.InsertTableRowBelow(ARow, GetForceVisible);
  APatternRowIndex := ARow.Table.Rows.IndexOf(ARow);
  Result := ARow.Table.Rows[APatternRowIndex + 1];
end;

{ TdxInsertTableColumnCommandBase }

constructor TdxInsertTableColumnCommandBase.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FInsertColumnCommand := CreateInsertTableColumnCommand(AControl);
end;

destructor TdxInsertTableColumnCommandBase.Destroy;
begin
  FreeAndNil(FInsertColumnCommand);
  inherited Destroy;
end;

function TdxInsertTableColumnCommandBase.CreateInsertTableColumnCommand(
  const AControl: IdxRichEditControl): TdxInsertTableColumnCoreCommandBase;
begin
  Result := GetInsertTableColumnCommandClass.Create(AControl);
end;

{$IFNDEF DELPHIXE2}
class function TdxInsertTableColumnCommandBase.GetInsertTableColumnCommandClass: TdxInsertTableColumnCoreCommandBaseClass;
begin
  raise Exception.Create('for C++Builder XE');
end;
{$ENDIF}

function TdxInsertTableColumnCommandBase.GetInsertColumnCommand: TdxInsertTableColumnCoreCommandBase;
begin
  Result := FInsertColumnCommand;
end;

procedure TdxInsertTableColumnCommandBase.ExecuteCore;
var
  APatternCell: TdxTableCell;
begin
  DocumentModel.History.BeginTransaction;
  try
    APatternCell := FindPatternCell;
    if APatternCell = nil then
      Exit;
    InsertColumnCommand.PatternCell := APatternCell;
    InsertColumnCommand.PerformModifyModel;
  finally
    DocumentModel.History.EndTransaction;
  end;
end;

{ TdxInsertTableRowBelowCommand }

class function TdxInsertTableRowBelowCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertTableRowBelow;
end;

class function TdxInsertTableRowBelowCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertRowsBelow;
end;

class function TdxInsertTableRowBelowCommand.GetInsertTableRowCommandClass: TdxInsertTableRowCoreCommandBaseClass;
begin
  Result := TdxInsertTableRowBelowCoreCommand;
end;

function TdxInsertTableRowBelowCommand.FindPatternRowParagraphIndex(ASelections: TdxSelectionRangeCollection): TdxParagraphIndex;
var
  AEndParagraphIndex: TdxParagraphIndex;
begin
  AEndParagraphIndex := ActivePieceTable.FindParagraphIndex(ASelections.Last.&End);
  if (ASelections.Last.Length > 0) and (ASelections.Last.&End = ActivePieceTable.Paragraphs[AEndParagraphIndex].LogPosition) then
    Dec(AEndParagraphIndex);
  Result := AEndParagraphIndex;
end;

{ TdxInsertTableRowAboveCommand }

class function TdxInsertTableRowAboveCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertTableRowAbove;
end;

class function TdxInsertTableRowAboveCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertRowsAbove;
end;

class function TdxInsertTableRowAboveCommand.GetInsertTableRowCommandClass: TdxInsertTableRowCoreCommandBaseClass;
begin
  Result := TdxInsertTableRowAboveCoreCommand;
end;

function TdxInsertTableRowAboveCommand.FindPatternRowParagraphIndex(ASelections: TdxSelectionRangeCollection): TdxParagraphIndex;
begin
  Result := ActivePieceTable.FindParagraphIndex(ASelections.First.Start);
end;

{ TdxInsertTableCellsWithShiftToTheHorizontallyCommand }

class function TdxInsertTableCellsWithShiftToTheHorizontallyCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableCellsMenuCaption);
end;

class function TdxInsertTableCellsWithShiftToTheHorizontallyCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableCellsDescription);
end;

function TdxInsertTableCellsWithShiftToTheHorizontallyCommand.CanChangePosition(
  const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxInsertTableCellsWithShiftToTheHorizontallyCommand.ChangePosition(
  const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

function TdxInsertTableCellsWithShiftToTheHorizontallyCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxInsertTableCellsWithShiftToTheHorizontallyCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxInsertTableCellsWithShiftToTheHorizontallyCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxInsertTableCellsWithShiftToTheHorizontallyCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

procedure TdxInsertTableCellsWithShiftToTheHorizontallyCommand.PerformModifyModel;
var
  ACellsCollection: TdxSelectedCellsCollection;
  ATopRowIndex: Integer;
  AForceVisible: Boolean;
  I, J: Integer;
  ACurrentCellsInterval: TdxSelectedCellsIntervalInRow;
  AStartIndex, AEndIndex: Integer;
begin
  if not DocumentModel.Selection.IsWholeSelectionInOneTable then
    Exit;

  ACellsCollection := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
  ATopRowIndex := ACellsCollection.GetTopRowIndex;
  AForceVisible := GetForceVisible;
  for I := ACellsCollection.GetBottomRowIndex downto ATopRowIndex do
  begin
    ACurrentCellsInterval := ACellsCollection[I];
    AStartIndex := ACurrentCellsInterval.NormalizedStartCellIndex;
    AEndIndex := ACurrentCellsInterval.NormalizedEndCellIndex;
    for J := AEndIndex downto AStartIndex do
      ActivePieceTable.InsertTableCellToTheLeft(ACurrentCellsInterval.NormalizedStartCell,
        AForceVisible, RichEditControl as IdxInnerRichEditDocumentServerOwner);
  end;
end;

{ TdxInsertTableCellsWithShiftToTheVerticallyCommand }

class function TdxInsertTableCellsWithShiftToTheVerticallyCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableCellsMenuCaption);
end;

class function TdxInsertTableCellsWithShiftToTheVerticallyCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableCellsMenuCaption);
end;

function TdxInsertTableCellsWithShiftToTheVerticallyCommand.CalculateInsertedRowsCount(
  const ACellsInterval: TdxSelectedCellsIntervalInRow): Integer;
var
  I: Integer;
  AEndCellIndex: Integer;
  ACurrentCell: TdxTableCell;
  ACells: TdxTableCellCollection;
  ASpanCells: TdxTableCellList;
begin
  Result := MaxInt;
  ACells := ACellsInterval.Row.Cells;
  AEndCellIndex := ACellsInterval.EndCellIndex;
  for I := ACellsInterval.StartCellIndex to AEndCellIndex do
  begin
    ACurrentCell := ACells[I];
    ASpanCells := TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(ACurrentCell, False);
    try
      Result := Min(Result, ASpanCells.Count);
    finally
      ASpanCells.Free;
    end;
  end;
end;

function TdxInsertTableCellsWithShiftToTheVerticallyCommand.CanChangePosition(
  const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxInsertTableCellsWithShiftToTheVerticallyCommand.ChangePosition(
  const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

procedure TdxInsertTableCellsWithShiftToTheVerticallyCommand.DeleteContentInCells(ACell: TdxTableCell);
var
  ARunInfo: TdxRunInfo;
  AStartLogPosition: TdxDocumentLogPosition;
begin
  ARunInfo := ActivePieceTable.GetRunInfoByTableCell(ACell);
  try
    AStartLogPosition := ARunInfo.Start.LogPosition;
    ActivePieceTable.DeleteContent(AStartLogPosition, ARunInfo.&End.LogPosition - AStartLogPosition + 1, False);
  finally
    ARunInfo.Free;
  end;
end;

procedure TdxInsertTableCellsWithShiftToTheVerticallyCommand.DeleteTextInCells(
  ARow: TdxTableRow; ARowsCount: Integer);
var
  AStartIndex, AEndIndex: Integer;
  ARows: TdxTableRowCollection;
  ACurrentRow: TdxTableRow;
  ACurrentCell: TdxTableCell;
  ACells: TdxTableCellCollection;
  ACellsCount: Integer;
  I, J: Integer;
begin
  AStartIndex := ARow.IndexInTable + 1;
  AEndIndex := AStartIndex + ARowsCount;
  ARows := ARow.Table.Rows;
  for I := AStartIndex to AEndIndex - 1 do
  begin
    ACurrentRow := ARows[I];
    ACells := ACurrentRow.Cells;
    ACellsCount := ACells.Count;
    for J := 0 to ACellsCount - 1 do
    begin
      ACurrentCell := ACells[J];
      if ACurrentCell.VerticalMerging = TdxMergingState.Continue then
        DeleteContentInCells(ACurrentCell);
    end;
  end;
end;

function TdxInsertTableCellsWithShiftToTheVerticallyCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxInsertTableCellsWithShiftToTheVerticallyCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxInsertTableCellsWithShiftToTheVerticallyCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxInsertTableCellsWithShiftToTheVerticallyCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

procedure TdxInsertTableCellsWithShiftToTheVerticallyCommand.PerformModifyModel;
var
  ACellsCollection: TdxSelectedCellsCollection;
  ATopBottomRowIndex: Integer;
  I: Integer;
  ACurrentCellsInterval: TdxSelectedCellsIntervalInRow;
begin
  ACellsCollection := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
  ATopBottomRowIndex := ACellsCollection.GetBottomRowIndex;
  for I := ACellsCollection.GetTopRowIndex to ATopBottomRowIndex do
  begin
    ACurrentCellsInterval := ACellsCollection[I];
    TableCellsOperationWithShiftToTheVerticallyCore(ACurrentCellsInterval);
  end;
end;

procedure TdxInsertTableCellsWithShiftToTheVerticallyCommand.TableCellsOperationWithShiftToTheVerticallyCore(
  const ACellsInterval: TdxSelectedCellsIntervalInRow);
var
  AInsertedRowsCount: Integer;
  ACurrentRow: TdxTableRow;
  AForceVisible: Boolean;
  I, J: Integer;
  ACellsInCurrentRow: TdxTableCellCollection;
  AStartIndex, AEndIndex: Integer;
  ACurrentCell: TdxTableCell;
begin
  AInsertedRowsCount := CalculateInsertedRowsCount(ACellsInterval);
  ACurrentRow := ACellsInterval.Row;
  AForceVisible := GetForceVisible;
  for I := 0 to AInsertedRowsCount - 1 do
  begin
    ActivePieceTable.InsertTableRowBelow(ACurrentRow.Table.Rows.Last, AForceVisible);
    ACellsInCurrentRow := ACurrentRow.Cells;
    AStartIndex := ACellsInterval.NormalizedStartCellIndex;
    AEndIndex := ACellsInterval.NormalizedEndCellIndex;
    for J := AEndIndex downto AStartIndex do
    begin
      ACurrentCell := ACellsInCurrentRow[J];
      ActivePieceTable.InsertTableCellWithShiftToTheDown(ACurrentCell, AForceVisible, RichEditControl as IdxInnerRichEditDocumentServerOwner);
    end;
    ACurrentRow := ACurrentRow.Next;
    if ACurrentRow = nil then
      Exit;
  end;
  DeleteTextInCells(ACellsInterval.Row, AInsertedRowsCount);
end;

{ TdxInsertTableColumnToTheLeftCommand }

class function TdxInsertTableColumnToTheLeftCommand.GetDescription: string;
begin
  Result := TdxInsertTableColumnToTheLeftCoreCommand.GetDescription;
end;

class function TdxInsertTableColumnToTheLeftCommand.GetInsertTableColumnCommandClass: TdxInsertTableColumnCoreCommandBaseClass;
begin
  Result := TdxInsertTableColumnToTheLeftCoreCommand;
end;

class function TdxInsertTableColumnToTheLeftCommand.GetMenuCaption: string;
begin
  Result := TdxInsertTableColumnToTheLeftCoreCommand.GetMenuCaption;
end;

class function TdxInsertTableColumnToTheLeftCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertTableColumnToTheLeft;
end;

class function TdxInsertTableColumnToTheLeftCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertColumnsToTheLeft;
end;

function TdxInsertTableColumnToTheLeftCommand.FindPatternCell: TdxTableCell;
var
  ASelection: TdxSelection;
begin
  ASelection := DocumentModel.Selection;
  Result := ActivePieceTable.FindParagraph(ASelection.First.NormalizedStart).GetCell;
end;

{ TdxInsertTableColumnToTheRightCommand }

class function TdxInsertTableColumnToTheRightCommand.GetDescription: string;
begin
  Result := TdxInsertTableColumnToTheRightCoreCommand.GetDescription;
end;

class function TdxInsertTableColumnToTheRightCommand.GetInsertTableColumnCommandClass: TdxInsertTableColumnCoreCommandBaseClass;
begin
  Result := TdxInsertTableColumnToTheRightCoreCommand;
end;

class function TdxInsertTableColumnToTheRightCommand.GetMenuCaption: string;
begin
  Result := TdxInsertTableColumnToTheRightCoreCommand.GetMenuCaption;
end;

class function TdxInsertTableColumnToTheRightCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertTableColumnToTheRight;
end;

class function TdxInsertTableColumnToTheRightCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertColumnsToTheRight;
end;

function TdxInsertTableColumnToTheRightCommand.FindPatternCell: TdxTableCell;
var
  ASelection: TdxSelection;
begin
  ASelection := DocumentModel.Selection;
  Result := ActivePieceTable.FindParagraph(ASelection.NormalizedVirtualEnd).GetCell;
end;

{ TdxInsertTableRowAboveCoreCommand }

class function TdxInsertTableRowAboveCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableRowAboveDescription);
end;

class function TdxInsertTableRowAboveCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableRowAboveMenuCaption);
end;

function TdxInsertTableRowAboveCoreCommand.InsertRow(ARow: TdxTableRow): TdxTableRow;
var
  ARows: TdxTableRowCollection;
  APatternRowIndex: Integer;
begin
  ActivePieceTable.InsertTableRowAbove(ARow, GetForceVisible);
  ARows := ARow.Table.Rows;
  APatternRowIndex := ARows.IndexOf(ARow);
  if APatternRowIndex = 0 then
    Result := nil
  else
    Result := ARows[APatternRowIndex - 1];
end;

function TdxInsertTableRowAboveCoreCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := ActivePieceTable.Paragraphs[Row.LastCell.EndParagraphIndex].LogPosition + 1;
end;

{ TdxInsertTableColumnToTheLeftCoreCommand }

class function TdxInsertTableColumnToTheLeftCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableColumnToTheLeftDescription);
end;

class function TdxInsertTableColumnToTheLeftCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableColumnToTheLeftMenuCaption);
end;

function TdxInsertTableColumnToTheLeftCoreCommand.InsertColumn(APatternCell: TdxTableCell): TdxTableCell;
begin
  ActivePieceTable.InsertColumnToTheLeft(APatternCell, GetForceVisible);
  Result := APatternCell.Previous;
end;

{ TdxInsertTableColumnToTheRightCoreCommand }

class function TdxInsertTableColumnToTheRightCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableColumnToTheRightDescription);
end;

class function TdxInsertTableColumnToTheRightCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableColumnToTheRightMenuCaption);
end;

function TdxInsertTableColumnToTheRightCoreCommand.InsertColumn(APatternCell: TdxTableCell): TdxTableCell;
begin
  ActivePieceTable.InsertColumnToTheRight(APatternCell, GetForceVisible);
  Result := APatternCell.Next;
end;

{ TdxSelectTableRowCommand }

constructor TdxSelectTableRowCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FCanCalculateExecutionParameters := True;
end;

class function TdxSelectTableRowCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SelectTableRow;
end;

class function TdxSelectTableRowCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectTableRowMenuCaption);
end;

class function TdxSelectTableRowCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectTableRowDescription);
end;


function TdxSelectTableRowCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxSelectTableRowCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxSelectTableRowCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxSelectTableRowCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

function TdxSelectTableRowCommand.PerformChangeSelection: Boolean;
begin
  if CanCalculateExecutionParameters then
    CalculateExecutionParameters;
  Result := inherited PerformChangeSelection;
end;

procedure TdxSelectTableRowCommand.CalculateExecutionParameters;
var
  ASelectedCellsCollection: TdxSelectedCellsCollection;
  ARow: TdxTableRow;
begin
  if not DocumentModel.Selection.IsWholeSelectionInOneTable then
    Exit;
  ASelectedCellsCollection := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
  ARow := ASelectedCellsCollection.NormalizedFirst.Row;
  Rows := ARow.Table.Rows;
  StartRowIndex := ARow.IndexInTable;
  EndRowIndex := ASelectedCellsCollection.NormalizedLast.Row.IndexInTable;
end;

 procedure TdxSelectTableRowCommand.ChangeSelection(ASelection: TdxSelection);
var
  AStartRow, AEndRow: TdxTableRow;
  AStartParagraph, AEndParagraph: TdxParagraph;
  AStartSelection, AEndSelection: TdxDocumentLogPosition;
begin
  ASelection.ClearSelectionInTable;
  AStartRow := Rows[Min(StartRowIndex, EndRowIndex)];
  AEndRow := Rows[Max(StartRowIndex, EndRowIndex)];
  AStartParagraph := ActivePieceTable.Paragraphs[AStartRow.FirstCell.StartParagraphIndex];
  AEndParagraph := ActivePieceTable.Paragraphs[AEndRow.LastCell.EndParagraphIndex];
  AStartSelection := AStartParagraph.LogPosition;
  AEndSelection := AEndParagraph.LogPosition + AEndParagraph.Length;
  if StartRowIndex <= EndRowIndex then
  begin
    ASelection.Start := AStartSelection;
    ASelection.&End := AEndSelection;
  end
  else
  begin
    ASelection.Start := AEndSelection;
    ASelection.&End := AStartSelection;
  end;

  ASelection.SelectedCells := GetSelectedCells;
  ASelection.RemoveIntersectedSelectionItems(1);
  ValidateSelection(ASelection, True);
end;

function TdxSelectTableRowCommand.GetSelectedCells: TdxSelectedCellsCollection;
var
  I: Integer;
begin
  Result := TdxSelectedCellsCollection.Create;
  if StartRowIndex <= EndRowIndex then
  begin
    for I := StartRowIndex to EndRowIndex do
      GetSelectedCellsCore(Result, I);
  end
  else
  begin
    for I := StartRowIndex downto EndRowIndex do
      GetSelectedCellsCore(Result, I);
  end;
end;

procedure TdxSelectTableRowCommand.GetSelectedCellsCore(ASelectedCells: TdxSelectedCellsCollection; AIndex: Integer);
var
  ACurrentRow: TdxTableRow;
begin
  ACurrentRow := Rows[AIndex];
  ASelectedCells.AddSelectedCells(ACurrentRow, 0, ACurrentRow.Cells.Count - 1);
end;

function TdxSelectTableRowCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := APos.LogPosition;
end;

function TdxSelectTableRowCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

{ TdxSelectTableColumnsCommand }

constructor TdxSelectTableColumnsCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FCanCalculateExecutionParameters := True;
end;

class function TdxSelectTableColumnsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SelectTableColumns;
end;

class function TdxSelectTableColumnsCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectTableColumnsMenuCaption);
end;

class function TdxSelectTableColumnsCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectTableColumnsDescription);
end;


function TdxSelectTableColumnsCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxSelectTableColumnsCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxSelectTableColumnsCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxSelectTableColumnsCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

function TdxSelectTableColumnsCommand.PerformChangeSelection: Boolean;
begin
  if CanCalculateExecutionParameters then
    CalculateExecutionParameters;
  Result := inherited PerformChangeSelection;
end;

procedure TdxSelectTableColumnsCommand.CalculateExecutionParameters;
var
  ACells: TdxSelectedCellsCollection;
  I: Integer;
  AInterval: TdxSelectedCellsIntervalInRow;
begin
  if not DocumentModel.Selection.IsWholeSelectionInOneTable then
    Exit;
  ACells := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
  Rows := ACells.NormalizedFirst.Table.Rows;
  StartColumnIndex := ACells.NormalizedFirst.NormalizedStartCell.GetStartColumnIndexConsiderRowGrid;
  EndColumnIndex := ACells.NormalizedFirst.NormalizedEndCell.GetEndColumnIndexConsiderRowGrid;
  for I := ACells.GetTopRowIndex + 1 to ACells.GetBottomRowIndex do
  begin
    AInterval := ACells[I];
    StartColumnIndex := Min(StartColumnIndex, AInterval.NormalizedStartCell.GetStartColumnIndexConsiderRowGrid);
    EndColumnIndex := Max(EndColumnIndex, AInterval.NormalizedEndCell.GetEndColumnIndexConsiderRowGrid);
  end;
end;

procedure TdxSelectTableColumnsCommand.ChangeSelection(ASelection: TdxSelection);
var
  AStartCell, AEndCell: TdxTableCell;
begin
  ASelection.ClearSelectionInTable;
  AStartCell := GetStartCell;
  AEndCell := GetCellByColumnIndex(Rows.First, EndColumnIndex);
  ASelection.ManuallySetTableSelectionStructureAndChangeSelection(AStartCell, AEndCell, True);
  ValidateSelection(ASelection, True);
end;

function TdxSelectTableColumnsCommand.GetStartCell: TdxTableCell;
var
  ACount, I: Integer;
  ACell: TdxTableCell;
begin
  ACount := Rows.Count;
  for I := 0 to ACount - 1 do
  begin
    ACell := TdxTableCellVerticalBorderCalculator.GetCellByColumnIndex(Rows[I], StartColumnIndex);
    if ACell <> nil then
      Exit(ACell);
  end;
  Result := nil;
end;

function TdxSelectTableColumnsCommand.GetCellByColumnIndex(ARow: TdxTableRow; AColumnIndex: Integer): TdxTableCell;
var
  ACell: TdxTableCell;
  ACells: TdxTableCellCollection;
begin
  ACell := TdxTableCellVerticalBorderCalculator.GetCellByColumnIndex(ARow, AColumnIndex);
  if ACell <> nil then
    Exit(ACell.Table.GetFirstCellInVerticalMergingGroup(ACell));
  ACells := ARow.Cells;
  Assert(ACells.Count > 0);
  if AColumnIndex < ACells.First.GetStartColumnIndexConsiderRowGrid then
    Exit(ACells.First)
  else
    Exit(GetCellByColumnIndex(ARow.Previous, AColumnIndex));
end;

function TdxSelectTableColumnsCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxSelectTableColumnsCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := APos.LogPosition;
end;

procedure TdxSelectTableColumnsCommand.EnsureCaretVisibleVertically;
begin
end;

{ TdxSelectTableCellCommand }

procedure TdxSelectTableCellCommand.CalculateSelectedCell;
var
  ACells: TdxSelectedCellsCollection;
  AIsEmpty, ASeveralRowsSelected, AIsSquare, AIsPositiveLength: Boolean;
begin
  if not DocumentModel.Selection.IsWholeSelectionInOneTable then
    Exit;
  ACells := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
  AIsEmpty := not ACells.IsNotEmpty;
  ASeveralRowsSelected := ACells.RowsCount > 1;

  if AIsEmpty or ASeveralRowsSelected then
    Exit;

  AIsSquare := ACells.IsSquare;
  AIsPositiveLength := ACells.NormalizedFirst.NormalizedLength > 0;

  if AIsSquare and AIsPositiveLength then
    Exit;
  Cell := ACells.TopLeftCell;
end;

function TdxSelectTableCellCommand.CanChangePosition(
  const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxSelectTableCellCommand.ChangePosition(
  const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

procedure TdxSelectTableCellCommand.ChangeSelection(ASelection: TdxSelection);
var
  AStartParagraph, AEndParagraph: TdxParagraph;
begin
  if Cell = nil then
    Exit;
  AStartParagraph := ActivePieceTable.Paragraphs[Cell.StartParagraphIndex];
  AEndParagraph := ActivePieceTable.Paragraphs[Cell.EndParagraphIndex];
  ASelection.ClearSelectionInTable;
  ASelection.ActiveSelection.Start := AStartParagraph.LogPosition;
  ASelection.ActiveSelection.&End := AEndParagraph.EndLogPosition + 1;
  ASelection.TryMergeByActiveSelection;
  ASelection.SetStartCell(ASelection.Start);
  ValidateSelection(ASelection, True);
end;

class function TdxSelectTableCellCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectTableCellDescription);
end;

function TdxSelectTableCellCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxSelectTableCellCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectTableCellMenuCaption);
end;

function TdxSelectTableCellCommand.GetShouldUpdateCaretY: Boolean;
begin
  Result := ShouldUpdateCaretTableAnchorVerticalPositionY;
end;

function TdxSelectTableCellCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxSelectTableCellCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxSelectTableCellCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

class function TdxSelectTableCellCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SelectTableCell;
end;

function TdxSelectTableCellCommand.PerformChangeSelection: Boolean;
begin
  if Cell = nil then
    CalculateSelectedCell;
  Result := inherited PerformChangeSelection;
end;

{ TdxSplitTableCommand }

constructor TdxSplitTableCommand.Create(const ARichEditControl: IdxRichEditControl);
begin
  inherited Create(ARichEditControl);
  FFirstCellLogPosition := -1;
end;

class function TdxSplitTableCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SplitTable;
end;

class function TdxSplitTableCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SplitTable;
end;

class function TdxSplitTableCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSplitTableDescription);
end;

class function TdxSplitTableCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSplitTableMenuCaption);
end;

function TdxSplitTableCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxSplitTableCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxSplitTableCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

function TdxSplitTableCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

procedure TdxSplitTableCommand.PerformModifyModel;
begin
  PerformTableSplitBySelectionStart;
end;

procedure TdxSplitTableCommand.PerformTableSplitBySelectionStart;
var
  AStartCell: TdxTableCell;
  AFirstCellStartParagraphIndex: TdxParagraphIndex;
begin
  AStartCell := GetStartCell(DocumentModel.Selection);
  AFirstCellStartParagraphIndex := AStartCell.Row.FirstCell.StartParagraphIndex;
  FFirstCellLogPosition := ActivePieceTable.Paragraphs[AFirstCellStartParagraphIndex].LogPosition;
  ActivePieceTable.SplitTable(AStartCell.Table.Index, AStartCell.RowIndex, GetForceVisible);
end;

procedure TdxSplitTableCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := CheckSelection;
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

function TdxSplitTableCommand.CheckSelection: Boolean;
var
  ASelection: TdxSelection;
  AStartCell, AEndCell: TdxTableCell;
begin
  ASelection := DocumentModel.Selection;
  if not ASelection.IsWholeSelectionInOneTable then
    Exit(False);

  AStartCell := GetStartCell(ASelection);
  AEndCell := GetEndCell(ASelection);
  Result := not ((AStartCell = nil) or (AEndCell = nil) or (AStartCell.Table <> AEndCell.Table));
end;

function TdxSplitTableCommand.GetStartCell(ASelection: TdxSelection): TdxTableCell;
begin
  Result := TdxSelectedCellsCollection(ASelection.SelectedCells).NormalizedFirst.NormalizedStartCell;
end;

function TdxSplitTableCommand.GetEndCell(ASelection: TdxSelection): TdxTableCell;
begin
  Result := TdxSelectedCellsCollection(ASelection.SelectedCells).NormalizedLast.NormalizedEndCell;
end;

function TdxSplitTableCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxSplitTableCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Assert(FFirstCellLogPosition <> -1);
  Result := FFirstCellLogPosition;
end;

{ TdxChangeColumnSizeCommand }

constructor TdxChangeColumnSizeCommand.Create(const AControl: IdxRichEditControl;
  ASectionProperties: TdxSectionProperties; AColumnIndex: Integer; AOffset: Integer);
begin
  inherited Create(AControl);
  FSectionProperties := ASectionProperties;
  FColumnIndex := AColumnIndex;
  FOffset := AOffset;
  FDefaultTabWidth := DocumentModel.DocumentProperties.DefaultTabWidth;
end;

class function TdxChangeColumnSizeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeColumnSizeMenuCaption);
end;

class function TdxChangeColumnSizeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeColumnSizeDescription);
end;

procedure TdxChangeColumnSizeCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
end;

procedure TdxChangeColumnSizeCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AParagraphIndex: TdxParagraphIndex;
  ASectionIndex: TdxSectionIndex;
  ANewSectionProperties: TdxSectionProperties;
  ATargetSection: TdxSection;
begin
  AParagraphIndex := DocumentModel.Selection.Interval.&End.ParagraphIndex;
  ASectionIndex := DocumentModel.MainPieceTable.LookupSectionIndexByParagraphIndex(AParagraphIndex);
  if (FColumnIndex >= FSectionProperties.ColumnCount) or (ASectionIndex < 0) then
    Exit;
  RichEditControl.BeginUpdate;
  try
    ANewSectionProperties := GetNewSection(FSectionProperties);
    ATargetSection := DocumentModel.Sections[ASectionIndex];
    DocumentModel.BeginUpdate;
    try
      ANewSectionProperties.CopyToSection(ATargetSection);
    finally
      DocumentModel.EndUpdate;
    end;
  finally
    RichEditControl.EndUpdate;
  end;
end;

function TdxChangeColumnSizeCommand.GetLeftMargin(AOldMargin: Integer): Integer;
var
  ANewMargin: Integer;
begin
  ANewMargin := AOldMargin - Offset;
  if ANewMargin > 0 then
    Exit(ANewMargin);
  Result := AOldMargin;
end;

function TdxChangeColumnSizeCommand.GetRightMargin(AOldMargin: Integer): Integer;
var
  ANewMargin: Integer;
begin
  ANewMargin := AOldMargin + Offset;
  if ANewMargin > 0 then
    Exit(ANewMargin);
  Result := AOldMargin;
end;

{ TdxChangeColumnWidthCommand }

function TdxChangeColumnWidthCommand.GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties;
var
  AWidth: Integer;
begin
  AWidth := ASectionProperties.ColumnInfoCollection[ColumnIndex].Width - Offset;
  if AWidth > DefaultTabWidth then
    ChangeColumnSize(ASectionProperties, AWidth);
  Result := ASectionProperties;
end;

procedure TdxChangeColumnWidthCommand.ChangeColumnSize(ASectionProperties: TdxSectionProperties; AWidth: Integer);
var
  AColumns: TdxColumnInfoCollection;
  AColumn: TdxColumnInfo;
begin
  AColumns := ASectionProperties.ColumnInfoCollection;
  AColumn := AColumns[ColumnIndex].Clone;
  if ColumnIndex = AColumns.Count - 1 then
    ChangeColumnSizeByMargin(ASectionProperties, AWidth, AColumn)
  else
    ChangeColumnSizeBySpace(AWidth, AColumn);

  AColumns[ColumnIndex].Free;
  AColumns[ColumnIndex] := AColumn;
end;

procedure TdxChangeColumnWidthCommand.ChangeColumnSizeBySpace(AWidth: Integer; AColumn: TdxColumnInfo);
var
  ASpace: Integer;
begin
  ASpace := AColumn.Space + Offset;
  if (AWidth > DefaultTabWidth) and (ASpace > 0) then
  begin
    AColumn.Space := ASpace;
    AColumn.Width := AWidth;
  end;
end;

procedure TdxChangeColumnWidthCommand.ChangeColumnSizeByMargin(ASectionProperties: TdxSectionProperties; AWidth: Integer; AColumn: TdxColumnInfo);
var
  ANewMargin: Integer;
begin
  ANewMargin := ASectionProperties.RightMargin + Offset;
  if ANewMargin > 0 then
  begin
    ASectionProperties.RightMargin := ANewMargin;
    AColumn.Width := AWidth;
  end;
end;

{ TdxMoveColumnCommand }

function TdxMoveColumnCommand.GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties;
var
  AColumns: TdxColumnInfoCollection;
  AColumn, ANextColumn: TdxColumnInfo;
  AWidth, ANextWidth: Integer;
begin
  AColumns := ASectionProperties.ColumnInfoCollection;

  AColumn := AColumns[ColumnIndex].Clone;
  ANextColumn := AColumns[ColumnIndex + 1].Clone;

  AWidth := AColumn.Width + Offset;
  ANextWidth := ANextColumn.Width - Offset;
  if (AWidth > DefaultTabWidth) and (ANextWidth > DefaultTabWidth) then
  begin
    AColumn.Width := AWidth;
    ANextColumn.Width := ANextWidth;
  end;

  AColumns[ColumnIndex].Free;
  AColumns[ColumnIndex] := AColumn;

  AColumns[ColumnIndex + 1].Free;
  AColumns[ColumnIndex + 1] := ANextColumn;

  Result := ASectionProperties;
end;

{ TdxChangeColumnSizeByLeftCommand }

function TdxChangeColumnSizeByLeftCommand.GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties;
var
  AWidth: Integer;
begin
  AWidth := ASectionProperties.ColumnInfoCollection[ColumnIndex].Width + Offset;
  if AWidth > DefaultTabWidth then
    ChangeColumnSize(ASectionProperties, AWidth);
  Result := ASectionProperties;
end;

procedure TdxChangeColumnSizeByLeftCommand.ChangeColumnSize(ASectionProperties: TdxSectionProperties; AWidth: Integer);
var
  AColumns: TdxColumnInfoCollection;
  AColumn: TdxColumnInfo;
begin
  AColumns := ASectionProperties.ColumnInfoCollection;
  AColumn := AColumns[ColumnIndex].Clone;
  if ColumnIndex = 0 then
    ChangeColumnSizeByMargin(ASectionProperties, AColumn, AWidth)
  else
    ChangeColumnSizeBySpace(AColumns, AColumn, AWidth);

  AColumns[ColumnIndex].Free;
  AColumns[ColumnIndex] := AColumn;
end;

procedure TdxChangeColumnSizeByLeftCommand.ChangeColumnSizeByMargin(ASectionProperties: TdxSectionProperties; AColumn: TdxColumnInfo; AWidth: Integer);
var
  ANewMargin: Integer;
begin
  ANewMargin := ASectionProperties.LeftMargin - Offset;
  if ANewMargin > 0 then
  begin
    ASectionProperties.LeftMargin := ANewMargin;
    AColumn.Width := AWidth;
  end;
end;

procedure TdxChangeColumnSizeByLeftCommand.ChangeColumnSizeBySpace(AColumns: TdxColumnInfoCollection; AColumn: TdxColumnInfo; AWidth: Integer);
var
  APreviousColumn: TdxColumnInfo;
  ASpace: Integer;
begin
  APreviousColumn := AColumns[ColumnIndex - 1].Clone;
  ASpace := APreviousColumn.Space - Offset;
  if ASpace > 0 then
  begin
    AColumn.Width := AWidth;
    APreviousColumn.Space := ASpace;

    AColumns[ColumnIndex - 1].Free;
    AColumns[ColumnIndex - 1] := APreviousColumn;
  end;
end;

{ TdxChangeSectionHeightByTopCommand }

function TdxChangeSectionHeightByTopCommand.GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties;
var
  ANewMargin: Integer;
begin
  ANewMargin := ASectionProperties.TopMargin - Offset;
  if (ANewMargin > 0) and (ASectionProperties.BottomMargin + ANewMargin < ASectionProperties.PageHeight) then
    ASectionProperties.TopMargin := ANewMargin;
  Result := ASectionProperties;
end;

{ TdxChangeSectionHeightByBottomCommand }

function TdxChangeSectionHeightByBottomCommand.GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties;
var
  ANewMargin: Integer;
begin
  ANewMargin := ASectionProperties.BottomMargin + Offset;
  if (ANewMargin > 0) and (ASectionProperties.TopMargin + ANewMargin < ASectionProperties.PageHeight) then
    ASectionProperties.BottomMargin := ANewMargin;
  Result := ASectionProperties;
end;

{ TdxChangeWidthEqualWidthColumnsByLeftCommand }

function TdxChangeWidthEqualWidthColumnsByLeftCommand.GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties;
var
  ANewMargin, AWidth, ASpace: Integer;
begin
  if ColumnIndex = 0 then
  begin
    ANewMargin := ASectionProperties.LeftMargin - Offset;
    AWidth := Trunc((ASectionProperties.PageWidth - ANewMargin - ASectionProperties.RightMargin -
      ASectionProperties.Space * ASectionProperties.ColumnCount) / ASectionProperties.ColumnCount);
    if (ANewMargin > 0) and (AWidth > DefaultTabWidth) then
      ASectionProperties.LeftMargin := ANewMargin;
  end
  else
  begin
    ASpace := ASectionProperties.Space - Offset;
    AWidth := Trunc((ASectionProperties.PageWidth - ASectionProperties.LeftMargin - ASectionProperties.RightMargin -
      ASpace * ASectionProperties.ColumnCount) / ASectionProperties.ColumnCount);
    if (ASpace > 0) and (AWidth > DefaultTabWidth) then
      ASectionProperties.Space := ASpace;
  end;
  Result := ASectionProperties;
end;

{ TdxChangeWidthEqualWidthColumnsByRightCommand }

function TdxChangeWidthEqualWidthColumnsByRightCommand.GetNewSection(ASectionProperties: TdxSectionProperties): TdxSectionProperties;
var
  ANewMargin, AWidth, ASpace: Integer;
begin
  if ColumnIndex = ASectionProperties.ColumnCount - 1 then
  begin
    ANewMargin := ASectionProperties.RightMargin + Offset;
    AWidth := Trunc((ASectionProperties.PageWidth - ANewMargin - ASectionProperties.LeftMargin -
      ASectionProperties.Space * ASectionProperties.ColumnCount) / ASectionProperties.ColumnCount);
    if (ANewMargin > 0) and (AWidth > DefaultTabWidth) then
      ASectionProperties.RightMargin := ANewMargin;
  end
  else
  begin
    ASpace := ASectionProperties.Space + Offset;
    AWidth := Trunc((ASectionProperties.PageWidth - ASectionProperties.LeftMargin - ASectionProperties.RightMargin -
      ASpace * ASectionProperties.ColumnCount) / ASectionProperties.ColumnCount);
    if (ASpace > 0) and (AWidth > DefaultTabWidth) then
      ASectionProperties.Space := ASpace;
  end;
  Result := ASectionProperties;
end;

{ TdxTableElementPair }

constructor TdxTableElementPair.Create(AElement: TdxTableElementAccessorBase);
begin
  inherited Create;
  FElement := AElement;
end;

destructor TdxTableElementPair.Destroy;
begin
  FNextElement.Free;
  inherited Destroy;
end;

function TdxTableElementPair.GetNextElement: TdxTableElementAccessorBase;
begin
  if not FNextElementValid then
  begin
    FNextElement := DoGetNextElement;
    FNextElementValid := True;
  end;
  Result := FNextElement;
end;

function TdxTableElementPair.DoGetNextElement: TdxTableElementAccessorBase;
begin
  Result := FElement.GetNextElement;
end;

{ TdxTableElementPairUniqueCollection }

constructor TdxTableElementPairUniqueCollection.Create;
begin
  inherited Create;
  FInnerList := TdxObjectList<TdxTableElementPair>.Create;
  FMapElementPairIndex := TObjectDictionary<TdxTableElementAccessorBase, Integer>.Create([doOwnsKeys]);
  FExtractItems := TdxTableElementAccessorBaseList.Create;
end;

destructor TdxTableElementPairUniqueCollection.Destroy;
var
  I: Integer;
begin
  for I := 0 to FExtractItems.Count - 1 do
    FMapElementPairIndex.ExtractPair(FExtractItems[I]);
  FExtractItems.Free;
  FInnerList.Free;
  FMapElementPairIndex.Free;
  inherited Destroy;
end;

function TdxTableElementPairUniqueCollection.Add(AElement: TdxTableElementAccessorBase; AOwns: Boolean): Integer;
begin
  if FMapElementPairIndex.TryGetValue(AElement, Result) then
    Exit(Result);
  Result := FInnerList.Count;
  FInnerList.Add(TdxTableElementPair.Create(AElement));
  FMapElementPairIndex.Add(AElement, Result);
  if not AOwns then
    FExtractItems.Add(AElement);
end;

function TdxTableElementPairUniqueCollection.GetCount: Integer;
begin
  Result := FInnerList.Count;
end;

function TdxTableElementPairUniqueCollection.GetItem(Index: Integer): TdxTableElementPair;
begin
  Result := FInnerList[Index];
end;

{ TdxFromAutoToRealWidthsTableCalculator }

class procedure TdxFromAutoToRealWidthsTableCalculator.ApplyRealWidths(ATableViewInfo: TdxTableViewInfo);
var
  ATable: TdxTable;
begin
  ATable := ATableViewInfo.Table;
  ApplyRealWidthToTable(ATable, ATableViewInfo.VerticalBorderPositions.InitialPositions);
end;

class procedure TdxFromAutoToRealWidthsTableCalculator.ApplyRealWidthToTable(ATable: TdxTable; APositions: TdxSortedList<Integer>);
var
  AApplyRealWidth: TdxTableCellProcessorDelegate;
  AApplyRealWidthToRow: TdxTableRowProcessorDelegate;
begin
  AApplyRealWidth :=
    procedure (ACell: TdxTableCell)
    begin
      ApplyRealWidthToCell(ACell, APositions);
    end;
  ATable.ForEachCell(AApplyRealWidth);

  AApplyRealWidthToRow :=
    procedure (ARow: TdxTableRow)
    begin
      ApplyRealWidthToRow(ARow, APositions);
    end;
  ATable.ForEachRow(AApplyRealWidthToRow);
end;

class function TdxFromAutoToRealWidthsTableCalculator.ApplyNewWidth(APairs: TdxTableElementPairUniqueCollection;
  AInitialPositions: TdxSortedList<Integer>; ANewModelPositionIndex: Integer; AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AColumn: TdxVirtualTableColumn): Boolean;
var
  AShouldSetFixedLayout: Boolean;
  ACount, I, ALeftModelPositionIndex, ARightModelPositionIndex, ANextElementColumnSpan, ALastModelPositionIndex: Integer;
  APair: TdxTableElementPair;
  AElement, ANextElement: TdxTableElementAccessorBase;
  AElementWidth, AInitialElementWidth, AInitialNextElementWidth, AInitialSummaryWidth, ANextElementWidth, ANewValue, AOffset: TdxModelUnit;
  ATable: TdxTable;
  AIncreaseWidthBefore, AIncreaseWidthAfter: TdxTableRowProcessorDelegate;
  AWidthUnitInfo: TdxWidthUnitInfo;
begin
  AShouldSetFixedLayout := False;
  ACount := APairs.Count;
  for I := 0 to ACount - 1 do
  begin
    APair := APairs[I];
    AElement := APair.Element;
    ANextElement := APair.NextElement;
    ALeftModelPositionIndex := AElement.GetStartColumnIndex;
    AElementWidth := AConverter.ToModelUnits(GetPreferredWidthByTotalWidth(AElement, AInitialPositions[ANewModelPositionIndex] - AInitialPositions[ALeftModelPositionIndex]));
    if ANextElement <> nil then
    begin
      AInitialElementWidth := AElement.PreferredWidth.Value;
      AInitialNextElementWidth := ANextElement.PreferredWidth.Value;
      AInitialSummaryWidth := AInitialElementWidth + AInitialNextElementWidth;
      ARightModelPositionIndex := ANextElement.GetEndColumnIndex;
      if (ANewModelPositionIndex = ALeftModelPositionIndex) or (ANewModelPositionIndex = ARightModelPositionIndex) then
      begin
        Assert(((ANewModelPositionIndex = 0) and (ANewModelPositionIndex = ALeftModelPositionIndex)) or ((ANewModelPositionIndex = AInitialPositions.Count - 1) and (ANewModelPositionIndex = ARightModelPositionIndex)));
        ANextElementWidth := Max(AConverter.ToModelUnits(GetPreferredWidthByTotalWidth(AElement, AInitialPositions[ARightModelPositionIndex] - AInitialPositions[ALeftModelPositionIndex])), 0);
      end
      else
        ANextElementWidth := AInitialSummaryWidth - AElementWidth;
      AElement.ModelColumnSpan := ANewModelPositionIndex - ALeftModelPositionIndex;
      AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, AElementWidth);
      try
        AElement.PreferredWidth := AWidthUnitInfo;
      finally
        AWidthUnitInfo.Free;
      end;
      AShouldSetFixedLayout := AShouldSetFixedLayout or (AElementWidth < AConverter.ToModelUnits(AElement.GetMinContentWidth));
      ANextElementColumnSpan := ARightModelPositionIndex - ANewModelPositionIndex;
      ANextElement.ModelColumnSpan := ANextElementColumnSpan;
      AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, ANextElementWidth);
      try
        ANextElement.PreferredWidth := AWidthUnitInfo;
      finally
        AWidthUnitInfo.Free;
      end;
      AShouldSetFixedLayout := AShouldSetFixedLayout or (ANextElementWidth < AConverter.ToModelUnits(ANextElement.GetMinContentWidth));
    end
    else
    begin
      AElement.ModelColumnSpan := ANewModelPositionIndex - ALeftModelPositionIndex;
      AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, AElementWidth);
      try
        AElement.PreferredWidth := AWidthUnitInfo;
      finally
        AWidthUnitInfo.Free;
      end;
      AShouldSetFixedLayout := AElementWidth < AConverter.ToModelUnits(AElement.GetMinContentWidth);
    end;
  end;
  if ANewModelPositionIndex = 0 then
  begin
    ATable := APairs[0].Element.Row.Table;
    ANewValue := AConverter.ToModelUnits(AInitialPositions[0]) - AColumn.TableViewInfo.ModelRelativeIndent;
    if ATable.NestedLevel > 0 then
      ANewValue := Math.Max(ANewValue, 0);
    AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, ANewValue);
    try
      ATable.TableProperties.TableIndent.CopyFrom(AWidthUnitInfo);
    finally
      AWidthUnitInfo.Free;
    end;
    AOffset := AConverter.ToModelUnits(AInitialPositions[1] - AInitialPositions[0]);
    AIncreaseWidthBefore :=
      procedure (ARow: TdxTableRow)
      begin
        if ARow.GridBefore > 0 then
        begin
          ARow.Properties.WidthBefore.&Type := TdxWidthUnitType.ModelUnits;
          ARow.Properties.WidthBefore.Value := ARow.WidthBefore.Value + AOffset;
        end;
      end;
    ATable.ForEachRow(AIncreaseWidthBefore);
  end
  else
    if ANewModelPositionIndex = AInitialPositions.Count - 1 then
    begin
      ATable := APairs[0].Element.Row.Table;
      ALastModelPositionIndex := AInitialPositions.Count - 1;
      AOffset := AConverter.ToModelUnits(AInitialPositions[ALastModelPositionIndex] - AInitialPositions[ALastModelPositionIndex - 1]);
      AIncreaseWidthAfter :=
        procedure (ARow: TdxTableRow)
        var
          AWidthUnitInfo: TdxWidthUnitInfo;
        begin
          if ARow.GridAfter > 0 then
          begin
            AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, ARow.WidthAfter.Value + AOffset);
            try
              ARow.Properties.WidthAfter.CopyFrom(AWidthUnitInfo);
            finally
              AWidthUnitInfo.Free;
            end;
          end;
        end;
      ATable.ForEachRow(AIncreaseWidthAfter);
    end;
  Result := AShouldSetFixedLayout;
end;

class function TdxFromAutoToRealWidthsTableCalculator.GetPreferredWidthByTotalWidth(AElement: TdxTableElementAccessorBase;
  ATotalWidth: TdxLayoutUnit): TdxLayoutUnit;
begin
  Result := ATotalWidth;
end;

class function TdxFromAutoToRealWidthsTableCalculator.GetPreferredWidthByTotalWidth(ACell: TdxTableCell; ATotalWidth: TdxLayoutUnit): TdxLayoutUnit;
begin
  Result := ATotalWidth;
end;

class procedure TdxFromAutoToRealWidthsTableCalculator.ApplyRealWidthToRow(ARow: TdxTableRow; APositions: TdxSortedList<Integer>);
var
  AWidthUnitInfo: TdxWidthUnitInfo;
  AGridBefore, AStartIndex, AGridAfter, AEndIndex: Integer;
  ATotalWidth: TdxLayoutUnit;
  ANewWidthBefore, ANewWidthAfter: TdxModelUnit;
begin
  AGridBefore := ARow.GridBefore;
  if AGridBefore > 0 then
  begin
    AStartIndex := ARow.GridBefore;
    ATotalWidth := APositions[AStartIndex] - APositions[0];
    ANewWidthBefore := Max(ARow.DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(ATotalWidth), 1);
    if ShouldSetNewWidthBefore(ARow, ANewWidthBefore) then
    begin
      AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, ANewWidthBefore);
      try
        ARow.Properties.WidthBefore.CopyFrom(AWidthUnitInfo);
      finally
        AWidthUnitInfo.Free;
      end;
    end;
  end;
  AGridAfter := ARow.GridAfter;
  if AGridAfter > 0 then
  begin
    AStartIndex := ARow.LastCell.GetEndColumnIndexConsiderRowGrid + 1;
    AEndIndex := AStartIndex + AGridAfter;
    ATotalWidth := APositions[AEndIndex] - APositions[AStartIndex];
    ANewWidthAfter := Max(ARow.DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(ATotalWidth), 1);
    if ShouldSetNewWidthAfter(ARow, ANewWidthAfter) then
    begin
      AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, ANewWidthAfter);
      try
        ARow.Properties.WidthAfter.CopyFrom(AWidthUnitInfo);
      finally
        AWidthUnitInfo.Free;
      end;
    end;
  end;
end;

class procedure TdxFromAutoToRealWidthsTableCalculator.ApplyRealWidthToCell(ACell: TdxTableCell; APositions: TdxSortedList<Integer>);
var
  AStartIndex, AEndIndex: Integer;
  ATotalWidth: TdxLayoutUnit;
  ANewPreferredWidth: TdxModelUnit;
  APreferredWidth: TdxPreferredWidth;
begin
  AStartIndex := ACell.GetStartColumnIndexConsiderRowGrid;
  AEndIndex := AStartIndex + ACell.ColumnSpan;
  ATotalWidth := APositions[AEndIndex] - APositions[AStartIndex];
  ANewPreferredWidth := Math.Max(ACell.DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(GetPreferredWidthByTotalWidth(ACell, ATotalWidth)), 1);
  if ShouldSetNewPreferredWidth(ACell, ANewPreferredWidth) then
  begin
    APreferredWidth := ACell.Properties.PreferredWidth;
    APreferredWidth.&Type := TdxWidthUnitType.ModelUnits;
    APreferredWidth.Value := ANewPreferredWidth;
  end;
end;

class function TdxFromAutoToRealWidthsTableCalculator.ShouldSetNewPreferredWidth(ATable: TdxTable; ANewPreferredWidth: TdxModelUnit): Boolean;
var
  AProperties: TdxTableProperties;
begin
  AProperties := ATable.TableProperties;
  if not AProperties.UsePreferredWidth then
    Exit(True);
  Result := ShouldSetNewWidthCore(ATable.PreferredWidth, ANewPreferredWidth, ATable.DocumentModel.ToDocumentLayoutUnitConverter);
end;

class function TdxFromAutoToRealWidthsTableCalculator.ShouldSetNewPreferredWidth(ACell: TdxTableCell; ANewPreferredWidth: TdxModelUnit): Boolean;
var
  AProperties: TdxTableCellProperties;
begin
  AProperties := ACell.Properties;
  if not AProperties.UsePreferredWidth then
    Exit(True);
  Result := ShouldSetNewWidthCore(ACell.PreferredWidth, ANewPreferredWidth, ACell.DocumentModel.ToDocumentLayoutUnitConverter);
end;

class function TdxFromAutoToRealWidthsTableCalculator.ShouldSetNewWidthBefore(ARow: TdxTableRow; ANewWidth: TdxModelUnit): Boolean;
var
  AProperties: TdxTableRowProperties;
begin
  AProperties := ARow.Properties;
  if not AProperties.UseWidthBefore then
    Exit(True);
  Result := ShouldSetNewWidthCore(AProperties.WidthBefore, ANewWidth, ARow.DocumentModel.ToDocumentLayoutUnitConverter);
end;

class function TdxFromAutoToRealWidthsTableCalculator.ShouldSetNewWidthAfter(ARow: TdxTableRow; ANewWidth: TdxModelUnit): Boolean;
var
  AProperties: TdxTableRowProperties;
begin
  AProperties := ARow.Properties;
  if not AProperties.UseWidthAfter then
    Exit(True);
  Result := ShouldSetNewWidthCore(AProperties.WidthAfter, ANewWidth, ARow.DocumentModel.ToDocumentLayoutUnitConverter);
end;

class function TdxFromAutoToRealWidthsTableCalculator.ShouldSetNewWidthCore(ACurrentWidth: TdxWidthUnit; ANewWidth: TdxModelUnit; AConverter: TdxDocumentModelUnitToLayoutUnitConverter): Boolean;
var
  ADelta: TdxLayoutUnit;
begin
  if ACurrentWidth.&Type <> TdxWidthUnitType.ModelUnits then
    Exit(True);
  ADelta := AConverter.ToModelUnits(2);
  Result := Abs(ACurrentWidth.Value - ANewWidth) > ADelta;
end;

{ TdxChangeTableVirtualColumnRightCommand }

constructor TdxChangeTableVirtualColumnRightCommand.Create(const AControl: IdxRichEditControl; AColumn: TdxVirtualTableColumn; AValue: TdxLayoutUnit);
begin
  inherited Create(AControl);
  Assert(AColumn <> nil);
  FColumn := AColumn;
  FValue := AValue;
end;

function TdxChangeTableVirtualColumnRightCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxChangeTableVirtualColumnRightCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxChangeTableVirtualColumnRightCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxChangeTableVirtualColumnRightCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

class function TdxChangeTableVirtualColumnRightCommand.GetMenuCaption: string;
begin
  Result := '';
end;

class function TdxChangeTableVirtualColumnRightCommand.GetDescription: string;
begin
  Result := '';
end;

function TdxChangeTableVirtualColumnRightCommand.PerformChangeSelection: Boolean;
begin
  Result := True;
end;

function TdxChangeTableVirtualColumnRightCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := False;
end;

function TdxChangeTableVirtualColumnRightCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := APos.LogPosition;
end;

procedure TdxChangeTableVirtualColumnRightCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Tables);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
  ApplyDocumentProtectionToTable(AState, FColumn.TableViewInfo.Table);
end;

procedure TdxChangeTableVirtualColumnRightCommand.PerformModifyModel;
var
  AElements: TdxTableElementPairUniqueCollection;
  ATableViewInfo: TdxTableViewInfo;
  ATable: TdxTable;
  AAlignedPositions, AInitialPositions, AOldInitialPositions: TdxLayoutUnitSortedList;
  ANeedDestroyInitialPositions: Boolean;
  ATextAreaOffset, ANewModelPosition: TdxLayoutUnit;
  ANewLayoutPosition, ACount, I, ANewModelPositionIndex: Integer;
  AInitialPositionsArray: TArray<Integer>;
  ARow: TdxTableRow;
  AShouldSetFixedLayout: Boolean;
begin
  Assert(FColumn.Elements.Count > 0);
  AElements := CreateTableElementPairUniqueCollection(FColumn.Elements);
  try
    if not IsPairsValid(AElements) then
      Exit;

    ATableViewInfo := FColumn.TableViewInfo;
    ATable := ATableViewInfo.Table;
    TdxFromAutoToRealWidthsTableCalculator.ApplyRealWidths(ATableViewInfo);
    AAlignedPositions := ATableViewInfo.VerticalBorderPositions.AlignedPosition;
    ANeedDestroyInitialPositions := False;
    AInitialPositions := ATableViewInfo.VerticalBorderPositions.InitialPositions;
    try
      ATextAreaOffset := ATableViewInfo.TextAreaOffset;
      ANewLayoutPosition := FValue - ATableViewInfo.Column.Bounds.Left;
      if ATextAreaOffset <> 0 then
      begin
        ACount := AInitialPositions.Count;
        SetLength(AInitialPositionsArray, ACount);
        for I := 0 to ACount - 1 do
          AInitialPositionsArray[I] := AInitialPositions[I] - ATextAreaOffset;
        AInitialPositions := TdxLayoutUnitSortedList.Create;
        ANeedDestroyInitialPositions := True;
        for I := 0 to ACount - 1 do
          AInitialPositions.Add(AInitialPositionsArray[I]);
      end;
      ARow := FColumn.Elements[0].Row;
      ANewModelPosition := CalculateModelPositionByLayoutPosition(ARow, ANewLayoutPosition, AInitialPositions, AAlignedPositions);
      ANewModelPositionIndex := AInitialPositions.BinarySearch(ANewModelPosition);
      if ANewModelPositionIndex < 0 then
      begin
        Assert(ANewModelPositionIndex < 0);
        ANewModelPositionIndex := not ANewModelPositionIndex;
        ChangeCellSpans(ATable, ANewModelPositionIndex - 1);
        if ATextAreaOffset = 0 then
        begin
          AOldInitialPositions := AInitialPositions;
          AInitialPositions := TdxLayoutUnitSortedList(AOldInitialPositions.Clone);
          if ANeedDestroyInitialPositions then
            AOldInitialPositions.Free;
          ANeedDestroyInitialPositions := True;
        end;
        AInitialPositions.Add(ANewModelPosition);
      end;
      AShouldSetFixedLayout := TdxFromAutoToRealWidthsTableCalculator.ApplyNewWidth(AElements, AInitialPositions, ANewModelPositionIndex, DocumentModel.ToDocumentLayoutUnitConverter, FColumn);
      NormalizeTable(ATable, AInitialPositions);
      if AShouldSetFixedLayout and (ATable.TableLayout <> TdxTableLayoutType.Fixed) then
        ATable.TableLayout := TdxTableLayoutType.Fixed;
      SetTablePreferredWidth(ATable, AInitialPositions);
    finally
      if ANeedDestroyInitialPositions then
        AInitialPositions.Free;
    end;
  finally
    AElements.Free;
  end;
end;

function TdxChangeTableVirtualColumnRightCommand.CalculateModelPositionByLayoutPosition(ARow: TdxTableRow; ALayoutPosition: Integer; AInitialPositions: TdxSortedList<Integer>; AAlignmentPositions: TdxSortedList<Integer>): TdxLayoutUnit;
var
  ADelta: TdxLayoutUnit;
begin
  ADelta := ALayoutPosition - AAlignmentPositions[ARow.LayoutProperties.GridBefore];
  Result := AInitialPositions[ARow.GridBefore] + ADelta;
end;

function TdxChangeTableVirtualColumnRightCommand.FindModelPositionIndexByLayoutPositionIndex(ATableRow: TdxTableRow; ALayoutPosition: Integer): Integer;
var
  AModelPositionIndex, ACellIndex, ACount: Integer;
  ACells: TdxTableCellCollection;
  ACell: TdxTableCell;
begin
  AModelPositionIndex := ATableRow.GridBefore;
  Dec(ALayoutPosition, ATableRow.LayoutProperties.GridBefore);
  ACellIndex := 0;
  ACells := ATableRow.Cells;
  ACount := ACells.Count;
  while (ALayoutPosition > 0) and (ACellIndex < ACount) do
  begin
    ACell := ACells[ACellIndex];
    Inc(AModelPositionIndex, ACell.ColumnSpan);
    Dec(ALayoutPosition, ACell.LayoutProperties.ColumnSpan);
    Inc(ACellIndex);
  end;
  Result := AModelPositionIndex;
end;

function TdxChangeTableVirtualColumnRightCommand.GetActualWidth(AWidth: TdxWidthUnit): TdxModelUnit;
begin
  if AWidth.&Type <> TdxWidthUnitType.ModelUnits then
    Result := 0
  else
    Result := AWidth.Value;
end;

procedure TdxChangeTableVirtualColumnRightCommand.SetTablePreferredWidth(ATable: TdxTable; AInitialPositions: TdxSortedList<Integer>);
var
  AWidthUnitInfo: TdxWidthUnitInfo;
  ATablePreferredWidth: TdxWidthUnit;
  AFirstRow: TdxTableRow;
  ATotalWidth, ANewPreferredWidth: TdxModelUnit;
  ACells: TdxTableCellCollection;
  ACount, I: Integer;
  ATableProperties: TdxTableProperties;
begin
  ATablePreferredWidth := ATable.PreferredWidth;
  if (ATablePreferredWidth.&Type = TdxWidthUnitType.Nil) or (ATablePreferredWidth.&Type = TdxWidthUnitType.Auto) then
    Exit;

  AFirstRow := ATable.Rows.First;
  ATotalWidth := GetActualWidth(AFirstRow.WidthBefore) + GetActualWidth(AFirstRow.WidthAfter);

  ACells := AFirstRow.Cells;
  ACount := ACells.Count;
  for I := 0 to ACount - 1 do
    Inc(ATotalWidth, GetActualWidth(ACells[I].PreferredWidth));

  ANewPreferredWidth := ATotalWidth;
  if not TdxFromAutoToRealWidthsTableCalculator.ShouldSetNewPreferredWidth(ATable, ANewPreferredWidth) then
    Exit;

  ATableProperties := ATable.TableProperties;
  AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, Max(ANewPreferredWidth, 1));
  try
    ATableProperties.PreferredWidth.CopyFrom(AWidthUnitInfo);
  finally
    AWidthUnitInfo.Free;
  end;
end;

procedure TdxChangeTableVirtualColumnRightCommand.NormalizeTable(ATable: TdxTable; AInitialPositions: TdxLayoutUnitSortedList);
var
  AWidthUnitInfo: TdxWidthUnitInfo;
  AMinGridBefore, AMinGridAfter: Integer;
  AMinWidthBefore, AMinWidthAfter: TdxModelUnit;
  AFindMinGridBeforeAfter, ADecreaseWidthBeforeAfter: TdxTableRowProcessorDelegate;
  AValue: TdxLayoutUnit;
begin
  AMinGridBefore := MaxInt;
  AMinWidthBefore := 0;
  AMinGridAfter := MaxInt;
  AMinWidthAfter := 0;

  AFindMinGridBeforeAfter :=
    procedure (ARow: TdxTableRow)
    var
      AGridBefore, AGridAfter: Integer;
    begin
      AGridBefore := ARow.GridBefore;
      if AGridBefore < AMinGridBefore then
      begin
        AMinGridBefore := AGridBefore;
        if ARow.WidthBefore.&Type = TdxWidthUnitType.ModelUnits then
          AMinWidthBefore := ARow.WidthBefore.Value
        else
          AMinWidthBefore := 0;
      end;
      AGridAfter := ARow.GridAfter;
      if AGridAfter < AMinGridAfter then
      begin
        AMinGridAfter := AGridAfter;
        if ARow.WidthAfter.&Type = TdxWidthUnitType.ModelUnits then
          AMinWidthAfter := ARow.WidthAfter.Value
        else
          AMinWidthAfter := 0;
      end;
    end;
  ATable.ForEachRow(AFindMinGridBeforeAfter);

  if AMinGridBefore > 0 then
  begin
    AValue := DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(AInitialPositions[0]) + AMinWidthBefore - FColumn.TableViewInfo.ModelRelativeIndent;
    if ATable.NestedLevel > 0 then
      AValue := Math.Max(0, AValue);
    AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, AValue);
    try
      ATable.TableProperties.TableIndent.CopyFrom(AWidthUnitInfo);
    finally
      AWidthUnitInfo.Free;
    end;
  end;

  ADecreaseWidthBeforeAfter :=
    procedure (ARow: TdxTableRow)
    var
      AWidthUnitInfo: TdxWidthUnitInfo;
      ANewWidthBeforeValue, ANewWidthAfterValue: TdxModelUnit;
    begin
      if AMinGridBefore > 0 then
      begin
        ANewWidthBeforeValue := ARow.WidthBefore.Value - AMinWidthBefore;
        AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, ANewWidthBeforeValue);
        try
          ARow.Properties.WidthBefore.CopyFrom(AWidthUnitInfo);
        finally
          AWidthUnitInfo.Free;
        end;
      end;
      if AMinGridAfter > 0 then
      begin
        ANewWidthAfterValue := ARow.WidthAfter.Value - AMinWidthAfter;
        AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, ANewWidthAfterValue);
        try
          ARow.Properties.WidthAfter.CopyFrom(AWidthUnitInfo);
        finally
          AWidthUnitInfo.Free;
        end;
      end;
    end;
  ATable.ForEachRow(ADecreaseWidthBeforeAfter);

  ATable.NormalizeCellColumnSpans;
end;

function TdxChangeTableVirtualColumnRightCommand.IsPairsValid(APairs: TdxTableElementPairUniqueCollection): Boolean;
var
  AExpectedNextElements: TObjectDictionary<TdxTableElementAccessorBase, TdxTableElementPair>;
  ACount, I, AVerticalSpanElementCount, J: Integer;
  AExpectedRowAlignment: TdxTableRowAlignment;
  APair: TdxTableElementPair;
  ANextElement, AElement, AKey: TdxTableElementAccessorBase;
  AVerticalSpanElements: TdxTableElementAccessorBaseList;
begin
  AExpectedNextElements := TObjectDictionary<TdxTableElementAccessorBase, TdxTableElementPair>.Create([doOwnsKeys]);
  try
    AExpectedRowAlignment := APairs[0].Element.Row.TableRowAlignment;
    ACount := APairs.Count;
    for I := 0 to ACount - 1 do
    begin
      APair := APairs[I];
      if APair.Element.Row.TableRowAlignment <> AExpectedRowAlignment then
        Exit(False);
      ANextElement := APair.NextElement;
      if ANextElement <> nil then
      begin
        AVerticalSpanElements := ANextElement.GetVerticalSpanElements;
        try
          AVerticalSpanElementCount := AVerticalSpanElements.Count;
          for J := 0 to AVerticalSpanElementCount - 1 do
          begin
            AElement := AVerticalSpanElements[J];
            if not AExpectedNextElements.ContainsKey(AElement) then
              AExpectedNextElements.Add(AElement, APair)
            else
              AElement.Free;
          end;
        finally
          AVerticalSpanElements.Free;
        end;
      end;
    end;
    for I := 0 to ACount - 1 do
    begin
      APair := APairs[I];
      if APair.NextElement <> nil then
      begin
        AElement := nil;
        for AKey in AExpectedNextElements.Keys do
          if AKey.Equals(APair.NextElement) and (AKey <> APair.NextElement) then
          begin
            AElement := AKey;
            Break;
          end;
        AExpectedNextElements.ExtractPair(APair.NextElement);
        AElement.Free;
      end;
    end;
    Result := AExpectedNextElements.Count = 0;
  finally
    AExpectedNextElements.Free;
  end;
end;

function TdxChangeTableVirtualColumnRightCommand.CreateTableElementPairUniqueCollection(AElements: TdxTableElementAccessorBaseList): TdxTableElementPairUniqueCollection;
var
  ACount, I: Integer;
  AElement: TdxTableElementAccessorBase;
begin
  Result := TdxTableElementPairUniqueCollection.Create;
  ACount := AElements.Count;
  for I := 0 to ACount - 1 do
  begin
    AElement := AElements[I];
    AddCellCore(Result, AElement);
  end;
end;

procedure TdxChangeTableVirtualColumnRightCommand.AddCellCore(APairs: TdxTableElementPairUniqueCollection;
  AElement: TdxTableElementAccessorBase);
var
  AVerticalSpanElements: TdxTableElementAccessorBaseList;
  AVerticalSpanElementCount, I: Integer;
begin
  AVerticalSpanElements := AElement.GetVerticalSpanElements;
  try
    AVerticalSpanElementCount := AVerticalSpanElements.Count;
    for I := 0 to AVerticalSpanElementCount - 1 do
      APairs.Add(AVerticalSpanElements[I], AElement <> AVerticalSpanElements[I]);
  finally
    AVerticalSpanElements.Free;
  end;
end;

procedure TdxChangeTableVirtualColumnRightCommand.ChangeCellSpans(ATable: TdxTable; AColumnIndex: Integer);
var
  AWidthUnitInfo: TdxWidthUnitInfo;
  AChangeSpan: TdxTableCellProcessorDelegate;
  AChangeGridBefore, AChangeGridAfter: TdxTableRowProcessorDelegate;
begin
  AChangeSpan :=
    procedure (ACell: TdxTableCell)
    var
      AStartColumnIndex, AEndColumnIndex: Integer;
    begin
      AStartColumnIndex := ACell.GetStartColumnIndexConsiderRowGrid();
      if AStartColumnIndex > AColumnIndex then
        Exit;
      AEndColumnIndex := ACell.GetEndColumnIndexConsiderRowGrid();
      if AEndColumnIndex < AColumnIndex then
        Exit;
      ACell.Properties.ColumnSpan := ACell.ColumnSpan + 1;
    end;
  ATable.ForEachCell(AChangeSpan);

  AChangeGridBefore :=
    procedure (ARow: TdxTableRow)
    var
      AGridBefore: Integer;
    begin
      AGridBefore := ARow.GridBefore;
      if AColumnIndex >= AGridBefore then
        Exit;
      if AGridBefore = 0 then
      begin
        AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, 0);
        try
          ARow.Properties.WidthBefore.CopyFrom(AWidthUnitInfo);
        finally
          AWidthUnitInfo.Free;
        end;
      end;
      ARow.Properties.GridBefore := AGridBefore + 1;
    end;
  ATable.ForEachRow(AChangeGridBefore);

  AChangeGridAfter :=
    procedure (ARow: TdxTableRow)
    var
      AGridAfter, AStartColumnIndex: Integer;
    begin
      AGridAfter := ARow.GridAfter;
      AStartColumnIndex := ARow.LastCell.GetEndColumnIndexConsiderRowGrid() + 1;
      if AStartColumnIndex > AColumnIndex then
        Exit;
      if AGridAfter = 0 then
      begin
        AWidthUnitInfo := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, 0);
        try
          ARow.Properties.WidthAfter.CopyFrom(AWidthUnitInfo);
        finally
          AWidthUnitInfo.Free;
        end;
      end;
      ARow.Properties.GridAfter := AGridAfter + 1;
    end;
  ATable.ForEachRow(AChangeGridAfter);
end;

procedure TdxChangeTableVirtualColumnRightCommand.ChangeTableLeftOffset;
begin
  raise TdxNotImplementedException.Create;
end;

{ TdxChangeTableRowHeightCommand }

constructor TdxChangeTableRowHeightCommand.Create(const AControl: IdxRichEditControl; ARow: TdxTableRow; AValue: Integer);
begin
  inherited Create(AControl);
  Assert(ARow is TdxTableRow);
  FRow := ARow;
  FValue := AValue;
end;

class function TdxChangeTableRowHeightCommand.GetMenuCaption: string;
begin
  Result := '';
end;

class function TdxChangeTableRowHeightCommand.GetDescription: string;
begin
  Result := '';
end;

function TdxChangeTableRowHeightCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxChangeTableRowHeightCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxChangeTableRowHeightCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxChangeTableRowHeightCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

function TdxChangeTableRowHeightCommand.PerformChangeSelection: Boolean;
begin
  Result := True;
end;

function TdxChangeTableRowHeightCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := False;
end;

function TdxChangeTableRowHeightCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := APos.LogPosition;
end;

procedure TdxChangeTableRowHeightCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Tables);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
  ApplyDocumentProtectionToTable(AState, Row.Table);
end;

procedure TdxChangeTableRowHeightCommand.PerformModifyModel;
var
  AHeight: TdxHeightUnit;
begin
  AHeight := FRow.Properties.Height;
  AHeight.&Type := TdxHeightUnitType.Minimum;
  AHeight.Value := FValue;
end;

{ TdxInsertTableCommand }

function TdxInsertTableCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultValueBasedCommandUIState<TdxCreateTableParameters>.Create;
end;

procedure TdxInsertTableCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxCreateTableParameters>;
begin
  CheckExecutedAtUIThread;
  NotifyBeginCommandExecution(AState);
  try

    AValueBasedState := AState as IdxValueBasedCommandUIState<TdxCreateTableParameters>;
    AValueBasedState.Value := TdxCreateTableParameters.Create(2, 5);

    ShowInsertTableForm(AValueBasedState.Value, ShowInsertTableFormCallback, AState as TObject);
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

procedure TdxInsertTableCommand.ForceExecuteCore(const AState: IdxCommandUIState);
var
  AValueBasedState: IdxValueBasedCommandUIState<TdxCreateTableParameters>;
  AParameters: TdxCreateTableParameters;
  AInsertCommand: TdxInsertTableCoreCommand;
begin
  AValueBasedState := AState as IdxValueBasedCommandUIState<TdxCreateTableParameters>;
  AParameters := AValueBasedState.Value;

  AInsertCommand := TdxInsertTableCoreCommand(Commands[1]);
  AInsertCommand.RowCount := AParameters.RowCount;
  AInsertCommand.ColumnCount := AParameters.ColumnCount;

  inherited ForceExecuteCore(AState);
end;

class function TdxInsertTableCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableDescription);
end;

class function TdxInsertTableCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertTableCoreCommand;
end;

class function TdxInsertTableCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableMenuCaption);
end;

class function TdxInsertTableCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertTable;
end;

class function TdxInsertTableCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ShowInsertTableForm;
end;

procedure TdxInsertTableCommand.InsertTable(ARows, AColumns: Integer);
var
  ATransaction: TdxHistoryTransaction;
  AState: TdxDefaultValueBasedCommandUIState<TdxCreateTableParameters>;
begin
  RichEditControl.BeginUpdate;
  try
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      AState := TdxDefaultValueBasedCommandUIState<TdxCreateTableParameters>.Create;
      try
        AState.Value := TdxCreateTableParameters.Create(ARows, AColumns);
        ForceExecuteCore(AState);
      finally
        FreeAndNil(AState);
      end;
    finally
      ATransaction.Free;
    end;
  finally
    RichEditControl.EndUpdate;
  end;
end;

procedure TdxInsertTableCommand.ShowInsertTableForm(AParameters: TdxCreateTableParameters;
  const ACallback: TdxShowInsertTableFormCallback; ACallbackData: TObject);
begin
  RichEditControl.ShowInsertTableForm(AParameters, ACallback, ACallbackData);
end;

procedure TdxInsertTableCommand.ShowInsertTableFormCallback(const AParameters: TdxCreateTableParameters;
  ACallbackData: TObject);
var
  ATransaction: TdxHistoryTransaction;
  AValueBasedState: IdxValueBasedCommandUIState<TdxCreateTableParameters>;
begin
  RichEditControl.BeginUpdate;
  try
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      if not Supports(ACallbackData, IdxValueBasedCommandUIState<TdxCreateTableParameters>, AValueBasedState) then
        Exit;
      AValueBasedState.Value := AParameters;

      inherited ForceExecute(AValueBasedState);
    finally
      ATransaction.Free;
    end;
  finally
    RichEditControl.EndUpdate;
  end;
end;

procedure TdxInsertTableCommand.UpdateUIState(const AState: IdxCommandUIState);
begin
  inherited UpdateUIState(AState);
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxInsertDeleteTableCellsDispatcherCommandBase }

procedure TdxInsertDeleteTableCellsDispatcherCommandBase.ExecuteCore;
begin
  case CellsParameters.CellOperation of
    TdxTableCellOperation.ShiftToTheVertically:
      TableCellsOperationWithShiftToTheVertically;
    TdxTableCellOperation.RowOperation:
      TableCellsOperationWithRow;
    TdxTableCellOperation.ColumnOperation:
      TableCellsOperationWithColumn;
  else
    TableCellsOperationWithShiftToTheHorizontally;
  end;

end;

procedure TdxInsertDeleteTableCellsDispatcherCommandBase.ForceExecute(
  const AState: IdxCommandUIState);
var
  AValueState: IdxValueBasedCommandUIState<TdxTableCellsParameters>;
begin
  AValueState := AState as IdxValueBasedCommandUIState<TdxTableCellsParameters>;
  if AValueState = nil then
    Exit;

  FCellsParameters := AValueState.Value;
  inherited ForceExecute(AState);
end;

procedure TdxInsertDeleteTableCellsDispatcherCommandBase.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
// do nothing
end;

{ TdxInsertTableCellsDispatcherCommand }

class function TdxInsertTableCellsDispatcherCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableCellsDescription);
end;

class function TdxInsertTableCellsDispatcherCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableCellsMenuCaption);
end;

procedure TdxInsertTableCellsDispatcherCommand.TableCellsOperationWithColumn;
var
  ACommand: TdxInsertTableColumnToTheLeftCommand;
begin
  ACommand := TdxInsertTableColumnToTheLeftCommand.Create(RichEditControl);
  try
    ACommand.ExecuteCore;
  finally
    ACommand.Free;
  end;
end;

procedure TdxInsertTableCellsDispatcherCommand.TableCellsOperationWithRow;
var
  ACommand: TdxInsertTableRowAboveCommand;
begin
  ACommand := TdxInsertTableRowAboveCommand.Create(RichEditControl);
  try
    ACommand.ExecuteCore;
  finally
    ACommand.Free;
  end;
end;

procedure TdxInsertTableCellsDispatcherCommand.TableCellsOperationWithShiftToTheHorizontally;
var
  ACommand: TdxInsertTableCellsWithShiftToTheHorizontallyCommand;
begin
  ACommand := TdxInsertTableCellsWithShiftToTheHorizontallyCommand.Create(RichEditControl);
  try
    ACommand.ExecuteCore;
  finally
    ACommand.Free;
  end;
end;

procedure TdxInsertTableCellsDispatcherCommand.TableCellsOperationWithShiftToTheVertically;
var
  ACommand: TdxInsertTableCellsWithShiftToTheVerticallyCommand;
begin
  ACommand := TdxInsertTableCellsWithShiftToTheVerticallyCommand.Create(RichEditControl);
  try
    ACommand.ExecuteCore;
  finally
    ACommand.Free;
  end;
end;

{ TdxDeleteTableCellsDispatcherCommand }

class function TdxDeleteTableCellsDispatcherCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableCellsDescription);
end;

class function TdxDeleteTableCellsDispatcherCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableCellsMenuCaption);
end;

procedure TdxDeleteTableCellsDispatcherCommand.TableCellsOperationWithColumn;
var
  ACommand: TdxDeleteTableColumnsCommand;
begin
  ACommand := TdxDeleteTableColumnsCommand.Create(RichEditControl);
  try
    ACommand.ExecuteCore;
  finally
    ACommand.Free;
  end;
end;

procedure TdxDeleteTableCellsDispatcherCommand.TableCellsOperationWithRow;
var
  ACommand: TdxDeleteTableRowsCommand;
begin
  ACommand := TdxDeleteTableRowsCommand.Create(RichEditControl);
  try
    ACommand.ExecuteCore;
  finally
    ACommand.Free;
  end;
end;

procedure TdxDeleteTableCellsDispatcherCommand.TableCellsOperationWithShiftToTheHorizontally;
var
  ACommand: TdxDeleteTableCellsWithShiftToTheHorizontallyCommand;
begin
  ACommand := TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.Create(RichEditControl);
  try
    ACommand.ExecuteCore;
  finally
    ACommand.Free;
  end;
end;

procedure TdxDeleteTableCellsDispatcherCommand.TableCellsOperationWithShiftToTheVertically;
var
  ACommand: TdxDeleteTableCellsWithShiftToTheVerticallyCommand;
begin
  ACommand := TdxDeleteTableCellsWithShiftToTheVerticallyCommand.Create(RichEditControl);
  try
    ACommand.ExecuteCore;
  finally
    ACommand.Free;
  end;
end;

{ TdxDeleteTableRowsCommand }

function TdxDeleteTableRowsCommand.CanChangePosition(
  const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxDeleteTableRowsCommand.ChangePosition(
  const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

class function TdxDeleteTableRowsCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableRowsDescription);
end;

function TdxDeleteTableRowsCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxDeleteTableRowsCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableRowsMenuCaption);
end;

function TdxDeleteTableRowsCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxDeleteTableRowsCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxDeleteTableRowsCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

class function TdxDeleteTableRowsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DeleteTableRows;
end;

class function TdxDeleteTableRowsCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.DeleteRows;
end;

procedure TdxDeleteTableRowsCommand.PerformModifyModel;
var
  ASelectedRows: TdxTableRowList;
begin
  ASelectedRows := DocumentModel.Selection.GetSelectedTableRows;
  try
    PerformModifyModelCore(ASelectedRows);
  finally
    ASelectedRows.Free;
  end;
end;

procedure TdxDeleteTableRowsCommand.PerformModifyModelCore(
  ASelectedRows: TdxTableRowList);
var
  ASelectedRowsCount: Integer;
  ATable: TdxTable;
  ACurrentRow: TdxTableRow;
  I: Integer;
begin
  ASelectedRowsCount := ASelectedRows.Count;
  if ASelectedRowsCount <= 0 then
    Exit;

  ATable := ASelectedRows[0].Table;
  for I := ASelectedRowsCount - 1 downto 0 do
  begin
    ACurrentRow := ASelectedRows[I];
    ActivePieceTable.DeleteTableRowWithContent(ACurrentRow);
  end;
  ATable.NormalizeCellColumnSpans;
end;

procedure TdxDeleteTableRowsCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
var
  ASelection: TdxSelection;
begin
  inherited UpdateUIStateCore(AState);
  ASelection := DocumentModel.Selection;
  if not ASelection.IsWholeSelectionInOneTable then
  begin
    AState.Enabled := False;
    Exit;
  end;
  UpdateUIStateEnabled(AState);
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

procedure TdxDeleteTableRowsCommand.UpdateUIStateEnabled(
  const AState: IdxCommandUIState);
var
  ACells: TdxSelectedCellsCollection;
  ARowsCount: Integer;
begin
  ACells := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
  ARowsCount := ACells.RowsCount;
  AState.Enabled := (ARowsCount > 0) and (ARowsCount < ACells.First.Table.Rows.Count);
end;

{ TdxDeleteTableRowsMenuCommand }

procedure TdxDeleteTableRowsMenuCommand.ExecuteCore;
var
  ACommand: TdxDeleteTableRowsCommand;
begin
  ACommand := TdxDeleteTableRowsCommand.Create(RichEditControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

class function TdxDeleteTableRowsMenuCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableRowsDescription);
end;

class function TdxDeleteTableRowsMenuCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableRowsMenuCaption);
end;

class function TdxDeleteTableRowsMenuCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DeleteTableRowsMenuItem;
end;

procedure TdxDeleteTableRowsMenuCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  ACommand: TdxDeleteTableRowsCommand;
  ACellsCollection: TdxSelectedCellsCollection;
begin
  ACommand := TdxDeleteTableRowsCommand.Create(RichEditControl);
  try
    ACommand.UpdateUIState(AState);
    if AState.Enabled = False then
    begin
      AState.Visible := False;
      Exit;
    end;
    ACellsCollection := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
    AState.Enabled := AState.Enabled and ACellsCollection.IsSelectedEntireTableRows;
    AState.Visible := AState.Enabled;
  finally
    ACommand.Free;
  end;
end;

{ TdxDeleteTableColumnsCommand }

function TdxDeleteTableColumnsCommand.CalculateNewLogPosition(
  ASelectedCells: TdxSelectedCellsCollection): TdxDocumentLogPosition;
var
  AMinColumnIndex, ASelectedRowsCount: Integer;
  I: Integer;
  ACell: TdxTableCell;
begin
  AMinColumnIndex := MaxInt;
  ASelectedRowsCount := ASelectedCells.RowsCount;
  for I := 0 to ASelectedRowsCount - 1 do
    AMinColumnIndex := Min(AMinColumnIndex, ASelectedCells[I].StartCell.GetStartColumnIndexConsiderRowGrid);
  ACell := ASelectedCells.FirstSelectedCell.Table.GetCell(0, AMinColumnIndex);
  if ACell = nil then
    ACell := ASelectedCells.FirstSelectedCell.Table.FirstRow.FirstCell;
  Result := ActivePieceTable.Paragraphs[ACell.StartParagraphIndex].LogPosition;
end;

function TdxDeleteTableColumnsCommand.CanChangePosition(
  const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxDeleteTableColumnsCommand.CanDeleteTableColumns: Boolean;
var
  ASelectedCellsCollection: TdxSelectedCellsCollection;
begin
  Result := DocumentModel.Selection.SelectedCells is TdxSelectedCellsCollection;
  if Result then
  begin
    ASelectedCellsCollection := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
    Result := ASelectedCellsCollection.IsNotEmpty;
  end;
end;

function TdxDeleteTableColumnsCommand.ChangePosition(
  const APos: TdxDocumentModelPosition): Integer;
begin
  if FIsNotEmptySelectedCells then
    Result := FNewLogPosition
  else
    Result := APos.LogPosition;
end;

class function TdxDeleteTableColumnsCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableColumnsDescription);
end;

function TdxDeleteTableColumnsCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxDeleteTableColumnsCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableColumnsMenuCaption);
end;

function TdxDeleteTableColumnsCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxDeleteTableColumnsCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxDeleteTableColumnsCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

class function TdxDeleteTableColumnsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DeleteTableColumns;
end;

class function TdxDeleteTableColumnsCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.DeleteColumns;
end;

procedure TdxDeleteTableColumnsCommand.PerformModifyModel;
var
  ASelectedCells: TdxSelectedCellsCollection;
begin
  FIsNotEmptySelectedCells := CanDeleteTableColumns;
  if FIsNotEmptySelectedCells then
  begin
    ASelectedCells := Safe<TdxSelectedCellsCollection>.Cast(DocumentModel.Selection.SelectedCells);
    PerformModifyModelCore(ASelectedCells);
  end;
end;

procedure TdxDeleteTableColumnsCommand.PerformModifyModelCore(
  ASelectedCellsCollection: TdxSelectedCellsCollection);
begin
  FNewLogPosition := CalculateNewLogPosition(ASelectedCellsCollection);
  ActivePieceTable.DeleteTableColumns(ASelectedCellsCollection, RichEditControl as IdxInnerRichEditDocumentServerOwner);
end;

procedure TdxDeleteTableColumnsCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Enabled := CanDeleteTableColumns;
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxDeleteTableColumnsMenuCommand }

class function TdxDeleteTableColumnsMenuCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DeleteTableColumnsMenuItem;
end;

procedure TdxDeleteTableColumnsMenuCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  ACellsCollection: TdxSelectedCellsCollection;
begin
  inherited UpdateUIStateCore(AState);
  if AState.Enabled = False then
  begin
    AState.Visible := False;
    Exit;
  end;
  ACellsCollection := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
  AState.Enabled := AState.Enabled and ACellsCollection.IsSelectedEntireTableColumns;
  AState.Visible := AState.Enabled;
end;

{ TdxDeleteTableCellsWithShiftToTheHorizontallyCommand }

function TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.CanChangePosition(
  const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.ChangePosition(
  const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

procedure TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.DeleteEntireRow(
  ADeletedRow: TdxTableRow);
var
  ATable: TdxTable;
begin
  ATable := ADeletedRow.Table;
  ActivePieceTable.DeleteTableRowWithContent(ADeletedRow);
  ATable.NormalizeCellColumnSpans;
end;

procedure TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.DeleteEntireTable;
var
  ACommand: TdxDeleteTableCommand;
begin
  ACommand := TdxDeleteTableCommand.Create(RichEditControl);
  try
    ACommand.PerformModifyModel;
  finally
    ACommand.Free;
  end;
end;

class function TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableCellsDescription);
end;

function TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableCellsMenuCaption);
end;

function TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

function TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.IsSelectedEntireRow(
  const ACellsInterval: TdxSelectedCellsIntervalInRow): Boolean;
var
  ARow: TdxTableRow;
begin
  ARow := ACellsInterval.Row;
  Result := (ACellsInterval.NormalizedStartCell = ARow.FirstCell) and
    (ACellsInterval.NormalizedEndCell = ARow.LastCell);
end;

function TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.IsSelectedEntireTable(
  ASelectedCells: TdxSelectedCellsCollection): Boolean;
begin
  Result := ASelectedCells.IsSelectedEntireTable;
end;

procedure TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.PerformModifyModel;
var
  ACellsCollection: TdxSelectedCellsCollection;
  ATable: TdxTable;
  ATopRowIndex: Integer;
  ACalculator: TdxTableColumnWidthCalculator;
  AContainer: TdxTableWidthsContainer;
  I, J: Integer;
  ACurrentCellsInterval: TdxSelectedCellsIntervalInRow;
  ACurrentRow: TdxTableRow;
  ACellsInCurrentRow: TdxTableCellCollection;
  AStartIndex, AEndIndex: Integer;
  ACurrentCell: TdxTableCell;
begin
  if not DocumentModel.Selection.IsWholeSelectionInOneTable then
    Exit;
  ACellsCollection := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
  if IsSelectedEntireTable(ACellsCollection) then
  begin
    DeleteEntireTable;
    Exit;
  end;
  ATable := ACellsCollection.FirstSelectedCell.Table;
  ATopRowIndex := ACellsCollection.GetTopRowIndex;
  ACalculator := TdxTableColumnWidthCalculator.Create(ATable, RichEditControl as IdxInnerRichEditDocumentServerOwner);
  try
    AContainer := ACalculator.CalculateWidths;
    try
      for I := ACellsCollection.GetBottomRowIndex downto ATopRowIndex do
      begin
        ACurrentCellsInterval := ACellsCollection[I];
        ACurrentRow := ACurrentCellsInterval.Row;
        if IsSelectedEntireRow(ACurrentCellsInterval) then
        begin
          DeleteEntireRow(ACurrentRow);
          Continue;
        end;
        ACellsInCurrentRow := ACurrentRow.Cells;
        AStartIndex := ACurrentCellsInterval.NormalizedStartCellIndex;
        AEndIndex := ACurrentCellsInterval.NormalizedEndCellIndex;
        for J := AEndIndex downto AStartIndex do
        begin
          ACurrentCell := ACellsInCurrentRow[J];
          if ACurrentCell.VerticalMerging = TdxMergingState.Continue then
            Continue;
          DeleteTableCellWithContentKnownWidths(ACurrentCell, False,
            RichEditControl as IdxInnerRichEditDocumentServerOwner, AContainer, True);
        end;
      end;
    finally
      AContainer.Clear;
    end;
    ATable.Normalize(True);
    ATable.NormalizeRows;
  finally
    ACalculator.Free;
  end;
end;

procedure TdxDeleteTableCellsWithShiftToTheHorizontallyCommand.DeleteTableCellWithContentKnownWidths(ADeletedCell: TdxTableCell; ACanNormalizeCellVerticalMerging: Boolean;
  const AServer: IdxInnerRichEditDocumentServerOwner; const AContainer: TdxTableWidthsContainer; AUseDeltaBetweenColumnsUpdate: Boolean);
var
  ACommand: TdxPieceTableDeleteTableCellWithContentKnownWidthsCommand;
begin
  ACommand := TdxPieceTableDeleteTableCellWithContentKnownWidthsCommand.Create(ActivePieceTable,
    ADeletedCell, AServer, AContainer);
  try
    ACommand.CanNormalizeCellVerticalMerging := ACanNormalizeCellVerticalMerging;
    ACommand.UseDeltaBetweenColumnsUpdate := AUseDeltaBetweenColumnsUpdate;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

{ TdxDeleteTableCellsWithShiftToTheVerticallyCommand }

function TdxDeleteTableCellsWithShiftToTheVerticallyCommand.CanChangePosition(
  const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxDeleteTableCellsWithShiftToTheVerticallyCommand.ChangePosition(
  const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

class function TdxDeleteTableCellsWithShiftToTheVerticallyCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableCellsDescription);
end;

function TdxDeleteTableCellsWithShiftToTheVerticallyCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxDeleteTableCellsWithShiftToTheVerticallyCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableCellsMenuCaption);
end;

function TdxDeleteTableCellsWithShiftToTheVerticallyCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxDeleteTableCellsWithShiftToTheVerticallyCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxDeleteTableCellsWithShiftToTheVerticallyCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

procedure TdxDeleteTableCellsWithShiftToTheVerticallyCommand.PerformModifyModel;
begin
  if not DocumentModel.Selection.IsWholeSelectionInOneTable then
    Exit;
  ActivePieceTable.DeleteTableCellsWithShiftToTheUp(TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells));
end;

{ TdxDeleteTableCoreCommand }

function TdxDeleteTableCoreCommand.CanChangePosition(
  const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxDeleteTableCoreCommand.ChangePosition(
  const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

class function TdxDeleteTableCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableDescription);
end;

function TdxDeleteTableCoreCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxDeleteTableCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteTableMenuCaption);
end;

function TdxDeleteTableCoreCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxDeleteTableCoreCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxDeleteTableCoreCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

procedure TdxDeleteTableCoreCommand.PerformModifyModel;
begin
  ActivePieceTable.DeleteTableWithContent(DeletedTable);
end;

{ TdxDeleteTableCommand }

class function TdxDeleteTableCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DeleteTable;
end;

class function TdxDeleteTableCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.DeleteTable;
end;

procedure TdxDeleteTableCommand.PerformModifyModel;
var
  ASelectedCells: TdxSelectedCellsCollection;
begin
  ASelectedCells := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
  DeletedTable := ASelectedCells.FirstSelectedCell.Table;
  inherited PerformModifyModel;
end;

procedure TdxDeleteTableCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
var
  ASelection: TdxSelection;
begin
  inherited UpdateUIStateCore(AState);
  ASelection := DocumentModel.Selection;
  AState.Enabled := ASelection.IsWholeSelectionInOneTable;
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxToggleTableAutoFitPlaceholderCommand }

procedure TdxToggleTableAutoFitPlaceholderCommand.ExecuteCore;
begin
end;

class function TdxToggleTableAutoFitPlaceholderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableAutoFitPlaceholderDescription);
end;

class function TdxToggleTableAutoFitPlaceholderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableAutoFitPlaceholderMenuCaption);
end;

class function TdxToggleTableAutoFitPlaceholderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableAutoFitPlaceholder;
end;

procedure TdxToggleTableAutoFitPlaceholderCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := DocumentModel.Selection.IsWholeSelectionInOneTable;
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxToggleTableAutoFitPlaceholderMenuCommand }

class function TdxToggleTableAutoFitPlaceholderMenuCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableAutoFitMenuPlaceholder;
end;

procedure TdxToggleTableAutoFitPlaceholderMenuCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Visible := AState.Enabled;
end;

{ TdxToggleTableAutoFitCommandBase }

function TdxToggleTableAutoFitCommandBase.GetTable: TdxTable;
var
  ACell: TdxTableCell;
begin
  ACell := DocumentModel.Selection.SelectedCells.FirstSelectedCell;
  if (ACell = nil) and (InnerControl.ActiveView.CaretPosition.LayoutPosition <> nil) and
    (InnerControl.ActiveView.CaretPosition.LayoutPosition.TableCell <> nil) then
    ACell := InnerControl.ActiveView.CaretPosition.LayoutPosition.TableCell.Cell;
  Result := ACell.Table;
end;

procedure TdxToggleTableAutoFitCommandBase.ExecuteCore;
begin
  DocumentModel.BeginUpdate;
  try
    ApplyRealWidths;
    ApplyTableProperties;
    ApplyTableCellProperties;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxToggleTableAutoFitCommandBase.ApplyRealWidths;
var
  ATableViewInfo: TdxTableViewInfo;
begin
  ATableViewInfo := GetTableViewInfo(InnerControl.ActiveView.CaretPosition.LayoutPosition);
  if ATableViewInfo = nil then
  begin
    ATableViewInfo := GetTableViewInfo(InnerControl.ActiveView.SelectionLayout.StartLayoutPosition);
    if ATableViewInfo = nil then
    begin
      ATableViewInfo := GetTableViewInfo(InnerControl.ActiveView.SelectionLayout.EndLayoutPosition);
      if ATableViewInfo = nil then
        TdxRichEditExceptions.ThrowInternalException;
    end;
  end;
  TdxFromAutoToRealWidthsTableCalculator.ApplyRealWidths(ATableViewInfo);
end;

function TdxToggleTableAutoFitCommandBase.GetTableViewInfo(APosition: TdxDocumentLayoutPosition): TdxTableViewInfo;
var
  ATableCell: TdxTableCellViewInfo;
begin
  APosition.Update(InnerControl.ActiveView.DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.TableCell);
  ATableCell := APosition.TableCell;
  if APosition.IsValid(TdxDocumentLayoutDetailsLevel.TableCell) and (ATableCell <> nil) then
    Result := ATableCell.TableViewInfo
  else
    Result := nil;
end;

procedure TdxToggleTableAutoFitCommandBase.ApplyTableProperties;
var
  AWidthInfo: TdxWidthUnitInfo;
  ATableProperties: TdxTableProperties;
begin
  AWidthInfo := GetTableWidthInfo;
  try
    ATableProperties := Table.TableProperties;
    ATableProperties.PreferredWidth.CopyFrom(AWidthInfo);
    ATableProperties.TableLayout := TdxTableLayoutType.Autofit;
  finally
    AWidthInfo.Free;
  end;
end;

procedure TdxToggleTableAutoFitCommandBase.ApplyTableCellProperties;
var
  ARows: TdxTableRowCollection;
  ARowWidth, ARowCount, I, ACellCount, J: Integer;
  ACells: TdxTableCellCollection;
  ACurrentCell: TdxTableCell;
  AWidthInfo: TdxWidthUnitInfo;
begin
  ARows := Table.Rows;
  ARowWidth := CalculateRowWidth(ARows.First);
  ARowCount := ARows.Count;
  for I := 0 to ARowCount - 1 do
  begin
    ACells := ARows[I].Cells;
    ACellCount := ACells.Count;
    for J := 0 to ACellCount - 1 do
    begin
      ACurrentCell := ACells[J];
      AWidthInfo := GetTableCellWidthInfo(ACurrentCell, ARowWidth);
      try
        ACurrentCell.Properties.PreferredWidth.CopyFrom(AWidthInfo);
      finally
        AWidthInfo.Free;
      end;
    end;
  end;
end;

procedure TdxToggleTableAutoFitCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := DocumentModel.Selection.IsWholeSelectionInOneTable;
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxToggleShowTableGridLinesCommand }

class function TdxToggleShowTableGridLinesCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleShowTableGridLinesDescription);
end;

class function TdxToggleShowTableGridLinesCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleShowTableGridLinesMenuCaption);
end;

class function TdxToggleShowTableGridLinesCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleShowTableGridLines;
end;

class function TdxToggleShowTableGridLinesCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ViewTableGridlines;
end;

procedure TdxToggleShowTableGridLinesCommand.ExecuteCore;
var
  ATableOptions: TdxTableOptions;
begin
  ATableOptions := DocumentModel.TableOptions;
  if ATableOptions.GridLines = TdxRichEditTableGridLinesVisibility.Visible then
    ATableOptions.GridLines := TdxRichEditTableGridLinesVisibility.Hidden
  else
    ATableOptions.GridLines := TdxRichEditTableGridLinesVisibility.Visible;
end;

procedure TdxToggleShowTableGridLinesCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Visible := True;
  AState.Enabled := True;
  AState.Checked := DocumentModel.TableOptions.GridLines = TdxRichEditTableGridLinesVisibility.Visible;
end;

{ TdxToggleTableAutoFitContentsCommand }

procedure TdxToggleTableAutoFitContentsCommand.ApplyRealWidths;
begin
end;

function TdxToggleTableAutoFitContentsCommand.CalculateRowWidth(ARow: TdxTableRow): Integer;
begin
  Result := 0;
end;

class function TdxToggleTableAutoFitContentsCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableAutoFitContentsDescription);
end;

class function TdxToggleTableAutoFitContentsCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableAutoFitContentsMenuCaption);
end;

function TdxToggleTableAutoFitContentsCommand.GetTableCellWidthInfo(ACell: TdxTableCell;
  ARowWidth: Double): TdxWidthUnitInfo;
begin
  Result := GetTableWidthInfo;
end;

function TdxToggleTableAutoFitContentsCommand.GetTableWidthInfo: TdxWidthUnitInfo;
begin
  Result := TdxWidthUnitInfo.Create(TdxWidthUnitType.Auto, 0);
end;

class function TdxToggleTableAutoFitContentsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableAutoFitContents;
end;

class function TdxToggleTableAutoFitContentsCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.AutoFitContents;
end;

{ TdxToggleTableAutoFitWindowCommand }

constructor TdxToggleTableAutoFitWindowCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FHundredPercent := 5000;
end;

function TdxToggleTableAutoFitWindowCommand.CalculateRowWidth(ARow: TdxTableRow): Integer;
var
  ACellCount, I: Integer;
  ACells: TdxTableCellCollection;
begin
  Result := ARow.WidthAfter.Value + ARow.WidthBefore.Value;
  ACells := ARow.Cells;
  ACellCount := ACells.Count;
  for I := 0 to ACellCount - 1 do
    Inc(Result, ACells[I].PreferredWidth.Value);
end;

class function TdxToggleTableAutoFitWindowCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableAutoFitWindowDescription);
end;

class function TdxToggleTableAutoFitWindowCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableAutoFitWindowMenuCaption);
end;

function TdxToggleTableAutoFitWindowCommand.GetTableCellWidthInfo(ACell: TdxTableCell; ARowWidth: Double): TdxWidthUnitInfo;
var
  AValue: Integer;
begin
  AValue := Ceil(FHundredPercent * ACell.PreferredWidth.Value / ARowWidth);
  Result := TdxWidthUnitInfo.Create(TdxWidthUnitType.FiftiethsOfPercent, AValue);
end;

function TdxToggleTableAutoFitWindowCommand.GetTableWidthInfo: TdxWidthUnitInfo;
begin
  Result := TdxWidthUnitInfo.Create(TdxWidthUnitType.FiftiethsOfPercent, FHundredPercent);
end;

class function TdxToggleTableAutoFitWindowCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableAutoFitWindow;
end;

class function TdxToggleTableAutoFitWindowCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.AutoFitWindow;
end;

{ TdxToggleTableFixedColumnWidthCommand }

procedure TdxToggleTableFixedColumnWidthCommand.ApplyTableCellProperties;
begin
end;

procedure TdxToggleTableFixedColumnWidthCommand.ApplyTableProperties;
var
  AProperties: TdxTableProperties;
  APreferredWidth: TdxPreferredWidth;
begin
  AProperties := Table.TableProperties;
  AProperties.BeginUpdate;
  try
    AProperties.TableLayout := TdxTableLayoutType.Fixed;
    if Table.PreferredWidth.&Type <> TdxWidthUnitType.Nil then
    begin
      APreferredWidth := AProperties.PreferredWidth;
      APreferredWidth.BeginUpdate;
      try
        APreferredWidth.&Type := TdxWidthUnitType.Nil;
        APreferredWidth.Value := 0;
      finally
        APreferredWidth.EndUpdate;
      end;
    end;
  finally
    AProperties.EndUpdate;
  end;
end;

function TdxToggleTableFixedColumnWidthCommand.CalculateRowWidth(ARow: TdxTableRow): Integer;
begin
  Result := 0;
end;

class function TdxToggleTableFixedColumnWidthCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableFixedColumnWidthDescription);
end;

class function TdxToggleTableFixedColumnWidthCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableFixedColumnWidthMenuCaption);
end;

function TdxToggleTableFixedColumnWidthCommand.GetTableCellWidthInfo(ACell: TdxTableCell;
  ARowWidth: Double): TdxWidthUnitInfo;
begin
  Result := nil;
end;

function TdxToggleTableFixedColumnWidthCommand.GetTableWidthInfo: TdxWidthUnitInfo;
begin
  Result := nil;
end;

class function TdxToggleTableFixedColumnWidthCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableFixedColumnWidth;
end;

class function TdxToggleTableFixedColumnWidthCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.FixedColumnWidth;
end;

{ TdxTableStylesGalleryCommand }

procedure TdxTableStylesGalleryCommand.ForceExecute(const AState: IdxCommandUIState);
begin
  // do nothing
end;

class function TdxTableStylesGalleryCommand.GetDescription: string;
begin
  Result := '';
end;

class function TdxTableStylesGalleryCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.TableStyles;
end;

class function TdxTableStylesGalleryCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandTableStylesGalleryCaption);
end;

procedure TdxTableStylesGalleryCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  // do nothing
end;

end.

