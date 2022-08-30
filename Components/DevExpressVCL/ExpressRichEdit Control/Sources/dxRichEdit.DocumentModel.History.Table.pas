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

unit dxRichEdit.DocumentModel.History.Table;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes,
  Generics.Defaults, Generics.Collections, dxCore,
  dxRichEdit.Utils.Types,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem;

type
  { TdxCustomTableHistoryItem }

  TdxCustomTableHistoryItem = class(TdxRichEditHistoryItem)
  strict private
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
  public
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxCreateEmptyTableHistoryItem }

  TdxCreateEmptyTableHistoryItem = class(TdxCustomTableHistoryItem)
  private
    FInsertedTableIndex: Integer;
    FTable: TdxTable;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxSimplePieceTable; ASourceCell: TdxTableCell); reintroduce;
    destructor Destroy; override;

    property InsertedTableIndex: Integer read FInsertedTableIndex;
  end;

  { TdxCreateTableHistoryItem }

  TdxCreateTableHistoryItem = class(TdxCustomTableHistoryItem)
  private
    FFirstParagraphIndex: TdxParagraphIndex;
    FRowCount: Integer;
    FCellCount: Integer;
    FTable: TdxTable;
    FOwnTable: Boolean;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; AFirstParagraphIndex: TdxParagraphIndex; ARowCount, ACellCount: Integer); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;

    property FirstParagraphIndex: TdxParagraphIndex read FFirstParagraphIndex;
    property RowCount: Integer read FRowCount;
    property CellCount: Integer read FCellCount;
  end;

  { TdxTableConditionalFormattingController }

  TdxTableConditionalFormattingController = record
  private
    FTable: TdxTable;
    procedure ResetRowCachedProperties(ARow: TdxTableRow);
    function ShouldResetCellCachedIndexes(ACell: TdxTableCell): Boolean;
    procedure ResetParagraphsInCell(ACell: TdxTableCell);
    class procedure ResetTablesCachedProperties(APieceTable: TdxPieceTable); overload; static;
    class procedure ResetTableCachedProperties(ATable: TdxTable); static;
    function GetPieceTable: TdxPieceTable;
  public
    procedure Init(ATable: TdxTable);
    class procedure ResetTablesCachedProperties(ADocumentModel: TdxCustomDocumentModel); overload; static;
    procedure ResetCachedProperties(AStartRowIndex: Integer);
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxInsertEmptyTableRowHistoryItem }

  TdxInsertEmptyTableRowHistoryItem = class(TdxCustomTableHistoryItem)
  private
    FTable: TdxTable;
    FExtractedRow: TdxTableRow;
    FRow: TdxTableRow;
    FRowIndex: Integer;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ATable: TdxTable; ARowIndex: Integer); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;

    property InsertedRowIndex: TdxParagraphIndex read FRowIndex;
    property Table: TdxTable read FTable;
  end;

  { TdxInsertEmptyTableCellHistoryItem }

  TdxInsertEmptyTableCellHistoryItem = class(TdxCustomTableHistoryItem)
  private
    FStartParagraphIndex: TdxParagraphIndex;
    FEndParagraphIndex: TdxParagraphIndex;
    FInsertedCell: TdxTableCell;
    FExtractedCell: TdxTableCell;
    FRow: TdxTableRow;
    FInsertedIndex: Integer;
    function GetTable: TdxTable;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ARow: TdxTableRow; AInsertedIndex: Integer; AStart, AEnd: TdxParagraphIndex); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;

    property InsertedCell: TdxTableCell read FInsertedCell;
    property Row: TdxTableRow read FRow;
    property Table: TdxTable read GetTable;
  end;

  { TdxChangeTableStyleIndexHistoryItem }

  TdxChangeTableStyleIndexHistoryItem = class(TdxCustomTableHistoryItem)
  private
    FNewIndex: Integer;
    FOldIndex: Integer;
    FTableIndex: Integer;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ATableIndex, AOldStyleIndex, ANewStyleIndex: Integer); reintroduce;

    property NewIndex: Integer read FNewIndex;
    property OldIndex: Integer read FOldIndex;
    property TableIndex: Integer read FTableIndex;
  end;

  { TdxChangeTableCellStyleIndexHistoryItem }

  TdxChangeTableCellStyleIndexHistoryItem = class(TdxChangeTableStyleIndexHistoryItem)
  private
    FRowIndex: Integer;
    FColumnIndex: Integer;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ATableIndex, ARowIndex, AColumnIndex, AOldStyleIndex, ANewStyleIndex: Integer); reintroduce;

    property RowIndex: Integer read FRowIndex;
    property ColumnIndex: Integer read FColumnIndex;
  end;

  { TdxTableRowCollectionHistoryItemBase }

  TdxTableRowCollectionHistoryItemBase = class(TdxCustomTableHistoryItem)
  private
    FTableIndex: Integer;
    FRowIndex: Integer;
    function GetRow: TdxTableRow;
    function GetTable: TdxTable;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ATableIndex, ARowIndex: Integer); reintroduce;

    property TableIndex: Integer read FTableIndex;
    property RowIndex: Integer read FRowIndex;
    property Row: TdxTableRow read GetRow;
    property Table: TdxTable read GetTable;
  end;

  { TdxDeleteTableHistoryItem }

  TdxDeleteTableHistoryItem = class(TdxCustomTableHistoryItem)
  private
    FDeletedTable: TdxTable;
    FOwnDeletedTable: Boolean;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ADeletedTable: TdxTable); reintroduce;
    destructor Destroy; override;

    property DeletedTable: TdxTable read FDeletedTable;
  end;

  { TdxDeleteTableRowHistoryItem }

  TdxDeleteTableRowHistoryItem = class(TdxTableRowCollectionHistoryItemBase)
  private
    FDeletedRow: TdxTableRow;
    FRunIndex: TdxRunIndex;
    FExtractedRow: TdxTableRow;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ATableIndex, ARowIndex: Integer); reintroduce;
    destructor Destroy; override;

    property DeletedRow: TdxTableRow read FDeletedRow;
    property RunIndex: TdxRunIndex read FRunIndex write FRunIndex;
  end;

  { TdxDeleteEmptyTableCellHistoryItem }

  TdxDeleteEmptyTableCellHistoryItem = class(TdxTableRowCollectionHistoryItemBase)
  private
    FCell: TdxTableCell;
    FExtractedCell: TdxTableCell;
    FStartParagraphIndex: TdxParagraphIndex;
    FEndParagraphIndex: TdxParagraphIndex;
    FCellIndex: Integer;
  protected
    procedure NormalizeSelectedCellsInSelection;
    procedure NormalizeSelectedCellsInSelectionCore(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ASelectedCells: TdxSelectedCellsCollection);
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ATableIndex, ARowIndex, ACellIndex: Integer); reintroduce;
    destructor Destroy; override;
  end;

  { TdxConvertParagraphsIntoTableRowHistoryItem }

  TdxConvertParagraphsIntoTableRowHistoryItem = class(TdxCustomTableHistoryItem)
  private
    FParagraphIndex: TdxParagraphIndex;
    FParagraphCount: Integer;
    FRow: TdxTableRow;
    FNewCells: array of TdxTableCell;
    function GetTable: TdxTable;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ARow: TdxTableRow; AParagraphIndex: TdxParagraphIndex; AParagraphCount: Integer); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;

    property Table: TdxTable read GetTable;
    property TableRow: TdxTableRow read FRow;
    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex;
    property ParagraphCount: Integer read FParagraphCount;
  end;

  { TdxChangeCellParagraphIndexCoreHistoryItem }

  TdxChangeCellParagraphIndexCoreHistoryItem = class(TdxCustomTableHistoryItem)
  private
    FParagraphIndex: TdxParagraphIndex;
    FTableCellIndex: Integer;
    FTableRowIndex: Integer;
    FTableIndex: Integer;
    FPreviousParagraphIndex: TdxParagraphIndex;
    function GetTable: TdxTable;
    function GetTableCell: TdxTableCell;
  protected
    property PreviousParagraphIndex: TdxParagraphIndex read FPreviousParagraphIndex write FPreviousParagraphIndex;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ATableCell: TdxTableCell; AParagraphIndex: TdxParagraphIndex); reintroduce;
    procedure SetTableCell(ACell: TdxTableCell);

    property Table: TdxTable read GetTable;
    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex;
    property TableCellIndex: Integer read FTableCellIndex;
    property TableCell: TdxTableCell read GetTableCell;
    property TableIndex: Integer read FTableIndex;
  end;

  { TdxChangeCellEndParagraphIndexHistoryItem }

  TdxChangeCellEndParagraphIndexHistoryItem = class(TdxChangeCellParagraphIndexCoreHistoryItem)
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ATableCell: TdxTableCell; AParagraphIndex: TdxParagraphIndex); reintroduce;
  end;

  { TdxChangeCellEndParagraphIndexHistoryItem }

  TdxChangeCellStartParagraphIndexHistoryItem = class(TdxChangeCellParagraphIndexCoreHistoryItem)
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ATableCell: TdxTableCell; AParagraphIndex: TdxParagraphIndex); reintroduce;
  end;

  { TdxSelectTableColumnsHistoryItem }

  TdxSelectTableColumnsHistoryItem = class(TdxCustomTableHistoryItem)
  private
    FTableIndex: Integer;
    FStartColumnIndex: Integer;
    FEndColumnIndex: Integer;
    FControl: IdxRichEditControl;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    property TableIndex: Integer read FTableIndex write FTableIndex;
    property StartColumnIndex: Integer read FStartColumnIndex write FStartColumnIndex;
    property EndColumnIndex: Integer read FEndColumnIndex write FEndColumnIndex;
    property Control: IdxRichEditControl read FControl write FControl;
  end;

  { TdxSelectTableRowHistoryItem }

  TdxSelectTableRowHistoryItem = class(TdxCustomTableHistoryItem)
  private
    FTableIndex: Integer;
    FStartRowIndex: Integer;
    FEndRowIndex: Integer;
    FControl: IdxRichEditControl;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    property TableIndex: Integer read FTableIndex write FTableIndex;
    property StartRowIndex: Integer read FStartRowIndex write FStartRowIndex;
    property EndRowIndex: Integer read FEndRowIndex write FEndRowIndex;
    property Control: IdxRichEditControl read FControl write FControl;
  end;

  { TdxMoveTableRowToOtherTableHistoryItem }

  TdxMoveTableRowToOtherTableHistoryItem = class(TdxCustomTableHistoryItem)
  private
    FTargetTable: TdxTable;
    FRow: TdxTableRow;
    FPreviousTable: TdxTable;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ATargetTable: TdxTable; ARow: TdxTableRow); reintroduce;
  end;

  { TdxDeleteTableFromTableCollectionHistoryItem }

  TdxDeleteTableFromTableCollectionHistoryItem = class(TdxCustomTableHistoryItem)
  private
    FDeletedTable: TdxTable;
    FOwnDeletedTable: Boolean;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ADeletedTable: TdxTable); reintroduce;
    destructor Destroy; override;

    property DeletedTable: TdxTable read FDeletedTable;
  end;

  { TdxTableCellPropertiesChangedHistoryItem }

  TdxTableCellPropertiesChangedHistoryItem = class(TdxIndexChangedHistoryItemCore)
  private
    FFirstCellParagraphIndex: TdxParagraphIndex;
    FNestedLevel: Integer;
    function GetPieceTable: TdxPieceTable;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; AFirstCellParagraphIndex: TdxParagraphIndex; ANestedLevel: Integer); reintroduce;
    function GetObject: TdxIndexBasedObject; override;

    property FirstCellParagraphIndex: TdxParagraphIndex read FFirstCellParagraphIndex;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

implementation

uses
  Math, SysUtils, RTLConsts, dxCoreClasses,
  dxRichEdit.Commands.Tables,
  dxRichEdit.DocumentModel.Styles,
  dxGenerics,
  dxRichEdit.Utils.Exceptions;

type
  TdxSelectTableColumnsCommandAccess = class(TdxSelectTableColumnsCommand);

{ TdxCustomTableHistoryItem }

function TdxCustomTableHistoryItem.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxCustomTableHistoryItem.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

{ TdxCreateEmptyTableHistoryItem }

constructor TdxCreateEmptyTableHistoryItem.Create(APieceTable: TdxSimplePieceTable; ASourceCell: TdxTableCell);
begin
  inherited Create(APieceTable);
  FInsertedTableIndex := -1;
  FTable := TdxTable.Create(PieceTable, ASourceCell, 0, 0);
end;

destructor TdxCreateEmptyTableHistoryItem.Destroy;
begin
  if FInsertedTableIndex = - 1 then
    FTable.Free;
  inherited Destroy;
end;

procedure TdxCreateEmptyTableHistoryItem.RedoCore;
begin
  PieceTable.Tables.Add(FTable);
  FInsertedTableIndex := PieceTable.Tables.Count - 1;
end;

procedure TdxCreateEmptyTableHistoryItem.UndoCore;
begin
  PieceTable.Tables.RemoveLast;
  FInsertedTableIndex := -1;
end;

{ TdxCreateTableHistoryItem }

constructor TdxCreateTableHistoryItem.Create(APieceTable: TdxCustomPieceTable;
  AFirstParagraphIndex: TdxParagraphIndex; ARowCount, ACellCount: Integer);
begin
  inherited Create(APieceTable);
  FFirstParagraphIndex := AFirstParagraphIndex;
  FRowCount := ARowCount;
  FCellCount := ACellCount;
end;

destructor TdxCreateTableHistoryItem.Destroy;
begin
  if FOwnTable then
    FTable.Free;
  inherited Destroy;
end;

procedure TdxCreateTableHistoryItem.Execute;
begin
  FTable := PieceTable.TableCellsManager.CreateTableCore(FirstParagraphIndex, RowCount, CellCount);
  FOwnTable := False;
end;

procedure TdxCreateTableHistoryItem.RedoCore;
begin
  PieceTable.TableCellsManager.InsertTable(FTable);
  FOwnTable := False;
end;

procedure TdxCreateTableHistoryItem.UndoCore;
var
  ASelection: TdxSelection;
begin
  PieceTable.TableCellsManager.RemoveTable(FTable);
  ASelection := DocumentModel.Selection;
  ASelection.SetStartCell(ASelection.NormalizedStart);
  FOwnTable := True;
end;

{ TdxTableConditionalFormattingController }

procedure TdxTableConditionalFormattingController.Init(ATable: TdxTable);
begin
  Assert(ATable <> nil);
  FTable := ATable;
end;

class procedure TdxTableConditionalFormattingController.ResetTablesCachedProperties(ADocumentModel: TdxCustomDocumentModel);
var
  I: Integer;
  APieceTables: TdxFastList;
  APieceTable: TdxPieceTable;
begin
  APieceTables := ADocumentModel.GetPieceTables(True);
  try
    for I := 0 to APieceTables.Count - 1 do
    begin
      APieceTable := TdxPieceTable(APieceTables[I]);
      ResetTablesCachedProperties(APieceTable);
    end;
  finally
    APieceTables.Free;
  end;
end;

function TdxTableConditionalFormattingController.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(FTable.PieceTable);
end;

procedure TdxTableConditionalFormattingController.ResetCachedProperties(AStartRowIndex: Integer);
var
  I, ACount: Integer;
  ARows: TdxTableRowCollection;
begin
  AStartRowIndex := Math.Max(0, AStartRowIndex);
  ARows := FTable.Rows;
  ACount := ARows.Count;
  if (AStartRowIndex = ACount) and (AStartRowIndex > 0) then
    Dec(AStartRowIndex);
  for I := AStartRowIndex to ACount - 1 do
    ResetRowCachedProperties(ARows[I]);
end;

procedure TdxTableConditionalFormattingController.ResetParagraphsInCell(ACell: TdxTableCell);
var
  I, AEndIndex: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
begin
  AEndIndex := ACell.EndParagraphIndex;
  AParagraphs := PieceTable.Paragraphs;
  if AEndIndex < 0 then
    Exit;
  AEndIndex := Min(PieceTable.Paragraphs.Count - 1, AEndIndex);
  for I := ACell.StartParagraphIndex to AEndIndex do
    AParagraphs[I].ResetCachedIndices(TdxResetFormattingCacheType.All);
end;

procedure TdxTableConditionalFormattingController.ResetRowCachedProperties(ARow: TdxTableRow);
var
  I: Integer;
  ACell: TdxTableCell;
  ACells: TdxTableCellCollection;
begin
  ACells := ARow.Cells;
  ARow.ResetConditionalType;
  for I := 0 to ACells.Count - 1 do
  begin
    ACell := ACells[I];
    ACell.ResetConditionalType;
    if ShouldResetCellCachedIndexes(ACell) then
      ResetParagraphsInCell(ACell);
  end;
end;

class procedure TdxTableConditionalFormattingController.ResetTableCachedProperties(ATable: TdxTable);
var
  AController: TdxTableConditionalFormattingController;
begin
  AController.Init(ATable);
  AController.ResetCachedProperties(0);
end;

class procedure TdxTableConditionalFormattingController.ResetTablesCachedProperties(APieceTable: TdxPieceTable);
var
  ATable: TdxTable;
  I: Integer;
begin
  for I := 0 to APieceTable.Tables.Count - 1 do
  begin
    ATable := APieceTable.Tables[I];
    ResetTableCachedProperties(ATable);
  end;
end;

function TdxTableConditionalFormattingController.ShouldResetCellCachedIndexes(ACell: TdxTableCell): Boolean;
begin
  Result := True;
end;

{ TdxInsertEmptyTableRowHistoryItem }

constructor TdxInsertEmptyTableRowHistoryItem.Create(APieceTable: TdxCustomPieceTable; ATable: TdxTable;
  ARowIndex: Integer);
begin
  inherited Create(APieceTable);
  Assert(ATable <> nil);
  Assert(ARowIndex >= 0);
  FRowIndex := ARowIndex;
  FTable := ATable;
end;

destructor TdxInsertEmptyTableRowHistoryItem.Destroy;
begin
  FreeAndNil(FExtractedRow);
  inherited;
end;

procedure TdxInsertEmptyTableRowHistoryItem.Execute;
begin
  FRow := TdxTableRow.Create(Table, 0);
  RedoCore;
end;

procedure TdxInsertEmptyTableRowHistoryItem.RedoCore;
var
  AController: TdxTableConditionalFormattingController;
begin
  AController.Init(Table);
  AController.ResetCachedProperties(FRowIndex);
  FExtractedRow := nil;
  Table.Rows.AddRowCore(FRowIndex, FRow);
end;

procedure TdxInsertEmptyTableRowHistoryItem.UndoCore;
var
  AController: TdxTableConditionalFormattingController;
begin
  Assert(Table.Rows[FRowIndex] = FRow);
  FExtractedRow := Table.Rows.ExtractRowCore(FRowIndex);
  AController.Init(Table);
  AController.ResetCachedProperties(FRowIndex);
end;

{ TdxInsertEmptyTableCellHistoryItem }

constructor TdxInsertEmptyTableCellHistoryItem.Create(APieceTable: TdxCustomPieceTable; ARow: TdxTableRow;
  AInsertedIndex: Integer; AStart, AEnd: TdxParagraphIndex);
begin
  inherited Create(APieceTable);
  Assert(ARow <> nil);
  if AStart < 0 then
    TdxRichEditExceptions.ThrowArgumentException('start', AStart);
  if AEnd < 0 then
    TdxRichEditExceptions.ThrowArgumentException('end', AEnd);
  FRow := ARow;
  FStartParagraphIndex := AStart;
  FEndParagraphIndex := AEnd;
  FInsertedIndex := AInsertedIndex;
end;

destructor TdxInsertEmptyTableCellHistoryItem.Destroy;
begin
  FreeAndNil(FExtractedCell);
  inherited Destroy;
end;

procedure TdxInsertEmptyTableCellHistoryItem.Execute;
begin
  FInsertedCell := TdxTableCell.Create(Row);
  RedoCore;
end;

function TdxInsertEmptyTableCellHistoryItem.GetTable: TdxTable;
begin
  Result := Row.Table;
end;

procedure TdxInsertEmptyTableCellHistoryItem.RedoCore;
var
  AController: TdxTableConditionalFormattingController;
begin
  FExtractedCell := nil;
  Row.Cells.AddCellCore(FInsertedIndex, InsertedCell);
  PieceTable.TableCellsManager.InitializeTableCell(FInsertedCell, FStartParagraphIndex, FEndParagraphIndex);
  AController.Init(Table);
  AController.ResetCachedProperties(FRow.IndexInTable);
end;

procedure TdxInsertEmptyTableCellHistoryItem.UndoCore;
var
  AController: TdxTableConditionalFormattingController;
begin
  PieceTable.TableCellsManager.RemoveTableCell(InsertedCell);
  FExtractedCell := Row.Cells.ExtractInternal(InsertedCell);
  AController.Init(Table);
  AController.ResetCachedProperties(FRow.IndexInTable);
end;

{ TdxChangeTableStyleIndexHistoryItem }

constructor TdxChangeTableStyleIndexHistoryItem.Create(APieceTable: TdxCustomPieceTable;
  ATableIndex, AOldStyleIndex, ANewStyleIndex: Integer);
begin
  inherited Create(APieceTable);
  FTableIndex := ATableIndex;
  FOldIndex := AOldStyleIndex;
  FNewIndex := ANewStyleIndex;
end;

procedure TdxChangeTableStyleIndexHistoryItem.RedoCore;
begin
  PieceTable.Tables[TableIndex].SetTableStyleIndexCore(NewIndex);
end;

procedure TdxChangeTableStyleIndexHistoryItem.UndoCore;
begin
  PieceTable.Tables[TableIndex].SetTableStyleIndexCore(OldIndex);
end;

{ TdxChangeTableCellStyleIndexHistoryItem }

constructor TdxChangeTableCellStyleIndexHistoryItem.Create(APieceTable: TdxCustomPieceTable; ATableIndex, ARowIndex,
  AColumnIndex, AOldStyleIndex, ANewStyleIndex: Integer);
begin
  inherited Create(APieceTable, ATableIndex, AOldStyleIndex, ANewStyleIndex);
  FRowIndex := ARowIndex;
  FColumnIndex := AColumnIndex;
end;

procedure TdxChangeTableCellStyleIndexHistoryItem.RedoCore;
begin
  PieceTable.Tables[TableIndex].Rows[RowIndex].Cells[ColumnIndex].SetTableCellStyleIndexCore(NewIndex);
end;

procedure TdxChangeTableCellStyleIndexHistoryItem.UndoCore;
begin
  PieceTable.Tables[TableIndex].Rows[RowIndex].Cells[ColumnIndex].SetTableCellStyleIndexCore(OldIndex);
end;

{ TdxTableRowCollectionHistoryItemBase }

constructor TdxTableRowCollectionHistoryItemBase.Create(APieceTable: TdxCustomPieceTable; ATableIndex, ARowIndex: Integer);
begin
  inherited Create(APieceTable);
  Assert(ATableIndex >= 0);
  Assert(ARowIndex >= 0);
  FTableIndex := ATableIndex;
  FRowIndex := ARowIndex;
end;

function TdxTableRowCollectionHistoryItemBase.GetRow: TdxTableRow;
begin
  Result := Table.Rows[RowIndex];
end;

function TdxTableRowCollectionHistoryItemBase.GetTable: TdxTable;
begin
  Result := PieceTable.Tables[TableIndex];
end;

{ TdxDeleteTableHistoryItem }

constructor TdxDeleteTableHistoryItem.Create(APieceTable: TdxCustomPieceTable; ADeletedTable: TdxTable);
begin
  inherited Create(APieceTable);
  Assert(ADeletedTable <> nil);
  Assert(ADeletedTable.FirstRow <> nil);
  Assert(ADeletedTable.FirstRow.FirstCell <> nil);
  FDeletedTable := ADeletedTable;
end;

destructor TdxDeleteTableHistoryItem.Destroy;
begin
  if FOwnDeletedTable then
    FDeletedTable.Free;
  inherited Destroy;
end;

procedure TdxDeleteTableHistoryItem.RedoCore;
var
  AStartParagraph: TdxParagraphIndex;
  APosition: TdxDocumentLogPosition;
begin
  AStartParagraph := DeletedTable.FirstRow.FirstCell.StartParagraphIndex;
  APosition := PieceTable.Paragraphs[Min(AStartParagraph, PieceTable.Paragraphs.Count - 1)].LogPosition;
  PieceTable.TableCellsManager.RemoveTable(DeletedTable);
  FOwnDeletedTable := True;
  PieceTable.DocumentModel.Selection.SetStartCell(APosition);
end;

procedure TdxDeleteTableHistoryItem.UndoCore;
begin
  PieceTable.TableCellsManager.InsertTable(DeletedTable);
  FOwnDeletedTable := False;
end;

{ TdxDeleteTableRowHistoryItem }

constructor TdxDeleteTableRowHistoryItem.Create(APieceTable: TdxCustomPieceTable; ATableIndex, ARowIndex: Integer);
var
  ARows: TdxTableRowCollection;
begin
  inherited Create(APieceTable, ATableIndex, ARowIndex);
  ARows := Table.Rows;
  FDeletedRow := ARows[RowIndex];
end;

destructor TdxDeleteTableRowHistoryItem.Destroy;
begin
  FreeAndNil(FExtractedRow);
  inherited Destroy;
end;

procedure TdxDeleteTableRowHistoryItem.RedoCore;
var
  ARows: TdxTableRowCollection;
  AController: TdxTableConditionalFormattingController;
begin
  PieceTable.ApplyChanges(TdxDocumentModelChangeType.DeleteContent, RunIndex, MaxInt);
  ARows := Table.Rows;
  FExtractedRow := ARows.ExtractRowCore(RowIndex);
  AController.Init(Table);
  AController.ResetCachedProperties(RowIndex);
end;

procedure TdxDeleteTableRowHistoryItem.UndoCore;
var
  ARows: TdxTableRowCollection;
  AController: TdxTableConditionalFormattingController;
begin
  ARows := Table.Rows;
  ARows.AddRowCore(RowIndex, DeletedRow);
  FExtractedRow := nil;
  AController.Init(Table);
  AController.ResetCachedProperties(RowIndex);
end;

{ TdxDeleteEmptyTableCellHistoryItem }

constructor TdxDeleteEmptyTableCellHistoryItem.Create(APieceTable: TdxCustomPieceTable; ATableIndex, ARowIndex,
  ACellIndex: Integer);
begin
  inherited Create(APieceTable, ATableIndex, ARowIndex);
  FCellIndex := ACellIndex;
  FCell := Table.Rows[RowIndex].Cells[FCellIndex];
  FStartParagraphIndex := FCell.StartParagraphIndex;
  FEndParagraphIndex := FCell.EndParagraphIndex;
end;

destructor TdxDeleteEmptyTableCellHistoryItem.Destroy;
begin
  FreeAndNil(FExtractedCell);
  inherited Destroy;
end;

procedure TdxDeleteEmptyTableCellHistoryItem.NormalizeSelectedCellsInSelection;
var
  ASelectedCells: TdxSelectedCellsCollection;
  I, ASelectedRowsCount: Integer;
  ACellsInRow: TdxSelectedCellsIntervalInRow;
begin
  if DocumentModel.Selection.SelectedCells is TdxStartSelectedCellInTable then
    Exit;

  ASelectedCells := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
  ASelectedRowsCount := ASelectedCells.RowsCount;
  if (ASelectedRowsCount = 1) and ASelectedCells.IsNotEmpty and ASelectedCells.First.IsContainsOnlyOneCell then
  begin
    DocumentModel.Selection.SetStartCell(PieceTable.Paragraphs[FStartParagraphIndex].LogPosition);
    Exit;
  end;

  for I := ASelectedRowsCount - 1 downto 0 do
  begin
    ACellsInRow := ASelectedCells[I];
    if ACellsInRow.Row = FCell.Row then
    begin
      NormalizeSelectedCellsInSelectionCore(ACellsInRow, ASelectedCells);
      Break;
    end;
  end;
end;

procedure TdxDeleteEmptyTableCellHistoryItem.NormalizeSelectedCellsInSelectionCore(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ASelectedCells: TdxSelectedCellsCollection);
begin
  if ACellsInRow.StartCellIndex = FCellIndex then
  begin
    if ACellsInRow.IsContainsOnlyOneCell then
    begin
      ASelectedCells.Remove(ACellsInRow);
      Exit;
    end;

    if ACellsInRow.StartCellIndex < ACellsInRow.EndCellIndex then
      ACellsInRow.StartCellIndex := ACellsInRow.StartCellIndex + 1
    else
      ACellsInRow.StartCellIndex := ACellsInRow.StartCellIndex - 1;
    Exit;
  end;
  if ACellsInRow.EndCellIndex = FCellIndex then
    if ACellsInRow.EndCellIndex > ACellsInRow.StartCellIndex then
      ACellsInRow.EndCellIndex := ACellsInRow.EndCellIndex - 1
    else
      ACellsInRow.EndCellIndex := ACellsInRow.EndCellIndex + 1;
end;

procedure TdxDeleteEmptyTableCellHistoryItem.RedoCore;
var
  ARunIndex: TdxRunIndex;
  AController: TdxTableConditionalFormattingController;
begin
  ARunIndex := PieceTable.Paragraphs[Table.Rows.First.Cells.First.StartParagraphIndex].FirstRunIndex;
  PieceTable.ApplyChanges(TdxDocumentModelChangeType.DeleteContent, ARunIndex, MaxInt);
  PieceTable.TableCellsManager.RemoveTableCell(FCell);
  FExtractedCell := Table.Rows[RowIndex].Cells.ExtractInternal(FCell);
  NormalizeSelectedCellsInSelection;
  AController.Init(Table);
  AController.ResetCachedProperties(RowIndex);
end;

procedure TdxDeleteEmptyTableCellHistoryItem.UndoCore;
var
  AController: TdxTableConditionalFormattingController;
begin
  PieceTable.TableCellsManager.InitializeTableCell(FCell, FStartParagraphIndex, FEndParagraphIndex);
  Table.Rows[RowIndex].Cells.AddCellCore(FCellIndex, FCell);
  FExtractedCell := nil;
  AController.Init(Table);
  AController.ResetCachedProperties(RowIndex);
end;

{ TdxConvertParagraphsIntoTableRowHistoryItem }

constructor TdxConvertParagraphsIntoTableRowHistoryItem.Create(APieceTable: TdxCustomPieceTable; ARow: TdxTableRow;
  AParagraphIndex: TdxParagraphIndex; AParagraphCount: Integer);
begin
  inherited Create(APieceTable);
  Assert(ARow <> nil);
  Assert(ParagraphCount >= 0);
  FRow := ARow;
  FParagraphIndex := AParagraphIndex;
  FParagraphCount := AParagraphCount;
end;

destructor TdxConvertParagraphsIntoTableRowHistoryItem.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FNewCells) - 1 do
    FNewCells[I].Free;
  FNewCells := nil;
  inherited Destroy;
end;

procedure TdxConvertParagraphsIntoTableRowHistoryItem.Execute;
var
  I, ACount: TdxParagraphIndex;
  ACell: TdxTableCell;
begin
  SetLength(FNewCells, ParagraphCount);
  ACount := ParagraphIndex + ParagraphCount;
  for I := ParagraphIndex to ACount - 1 do
  begin
    ACell := TdxTableCell.Create(FRow);
    FNewCells[I - ParagraphIndex] := ACell;
  end;
  RedoCore;
end;

function TdxConvertParagraphsIntoTableRowHistoryItem.GetTable: TdxTable;
begin
  Result := TableRow.Table;
end;

procedure TdxConvertParagraphsIntoTableRowHistoryItem.RedoCore;
var
  I, ACount: TdxParagraphIndex;
  ACell: TdxTableCell;
begin
  ACount := ParagraphIndex + ParagraphCount;
  for I := ParagraphIndex to ACount - 1 do
  begin
    ACell := FNewCells[I - ParagraphIndex];
    FRow.Cells.AddInternal(ACell);
    PieceTable.TableCellsManager.InitializeTableCell(ACell, I, I);
  end;
  FNewCells := nil;
end;

procedure TdxConvertParagraphsIntoTableRowHistoryItem.UndoCore;
var
  I, ACount: TdxParagraphIndex;
begin
  ACount := FRow.Cells.Count;
  SetLength(FNewCells, ACount);
  for I := 0 to ACount - 1 do
    FNewCells[I] := FRow.Cells[I];
  PieceTable.TableCellsManager.RevertParagraphsFromTableCells(TableRow);
end;

{ TdxChangeCellParagraphIndexCoreHistoryItem }

constructor TdxChangeCellParagraphIndexCoreHistoryItem.Create(APieceTable: TdxCustomPieceTable; ATableCell: TdxTableCell;
  AParagraphIndex: TdxParagraphIndex);
begin
  inherited Create(APieceTable);
  Assert(ATableCell <> nil);
  FPreviousParagraphIndex := -1;
  SetTableCell(ATableCell);
  FParagraphIndex := AParagraphIndex;
end;

function TdxChangeCellParagraphIndexCoreHistoryItem.GetTable: TdxTable;
begin
  if TableIndex >= 0 then
    Result := PieceTable.Tables[TableIndex]
  else
    Result := nil;
end;

function TdxChangeCellParagraphIndexCoreHistoryItem.GetTableCell: TdxTableCell;
begin
  if (FTableRowIndex >= 0) and (FTableCellIndex >= 0) then
    Result := Table.Rows[FTableRowIndex].Cells[FTableCellIndex]
  else
    Result := nil;
end;

procedure TdxChangeCellParagraphIndexCoreHistoryItem.SetTableCell(ACell: TdxTableCell);
var
  ATable: TdxTable;
  ARow: TdxTableRow;
begin
  if ACell = nil then
  begin
    FTableCellIndex := -1;
    FTableRowIndex  := -1;
    FTableCellIndex := -1;
    Exit;
  end;
  ATable := ACell.Table;
  FTableIndex := ATable.GetIndexCore;

  ARow := ACell.Row;
  FTableRowIndex := ATable.Rows.IndexOf(ARow);
  FTableCellIndex := ARow.Cells.IndexOf(ACell);
end;

{ TdxChangeCellEndParagraphIndexHistoryItem }

constructor TdxChangeCellEndParagraphIndexHistoryItem.Create(APieceTable: TdxCustomPieceTable; ATableCell: TdxTableCell; AParagraphIndex: TdxParagraphIndex);
begin
  inherited Create(APieceTable, ATableCell, AParagraphIndex);
  PreviousParagraphIndex := TableCell.EndParagraphIndex;
end;

procedure TdxChangeCellEndParagraphIndexHistoryItem.RedoCore;
begin
  TableCell.EndParagraphIndex := ParagraphIndex;
end;

procedure TdxChangeCellEndParagraphIndexHistoryItem.UndoCore;
begin
  TableCell.EndParagraphIndex := PreviousParagraphIndex;
end;

{ TdxChangeCellStartParagraphIndexHistoryItem }

constructor TdxChangeCellStartParagraphIndexHistoryItem.Create(APieceTable: TdxCustomPieceTable; ATableCell: TdxTableCell;
  AParagraphIndex: TdxParagraphIndex);
begin
  inherited Create(APieceTable, ATableCell, AParagraphIndex);
  PreviousParagraphIndex := TableCell.StartParagraphIndex;
end;

procedure TdxChangeCellStartParagraphIndexHistoryItem.RedoCore;
begin
  TableCell.StartParagraphIndex := ParagraphIndex;
end;

procedure TdxChangeCellStartParagraphIndexHistoryItem.UndoCore;
begin
  TableCell.StartParagraphIndex := PreviousParagraphIndex;
end;

{ TdxSelectTableColumnsHistoryItem }

procedure TdxSelectTableColumnsHistoryItem.RedoCore;
var
  ACommand: TdxSelectTableColumnsCommand;
begin
  ACommand := TdxSelectTableColumnsCommand.Create(Control);
  try
    ACommand.Rows := PieceTable.Tables[TableIndex].Rows;
    ACommand.StartColumnIndex := StartColumnIndex;
    ACommand.EndColumnIndex := EndColumnIndex;
    TdxSelectTableColumnsCommandAccess(ACommand).ChangeSelection(DocumentModel.Selection);
  finally
    ACommand.Free;
  end;
end;

procedure TdxSelectTableColumnsHistoryItem.UndoCore;
begin
end;

{ TdxSelectTableRowHistoryItem }

procedure TdxSelectTableRowHistoryItem.RedoCore;
var
  ACommand: TdxSelectTableRowCommand;
begin
  ACommand := TdxSelectTableRowCommand.Create(Control);
  try
    ACommand.Rows := PieceTable.Tables[TableIndex].Rows;
    ACommand.StartRowIndex := StartRowIndex;
    ACommand.EndRowIndex := EndRowIndex;
    TdxSelectTableColumnsCommandAccess(ACommand).ChangeSelection(DocumentModel.Selection);
  finally
    ACommand.Free;
  end;
end;

procedure TdxSelectTableRowHistoryItem.UndoCore;
begin
end;

{ TdxMoveTableRowToOtherTableHistoryItem }

constructor TdxMoveTableRowToOtherTableHistoryItem.Create(APieceTable: TdxCustomPieceTable; ATargetTable: TdxTable;
  ARow: TdxTableRow);
begin
  inherited Create(APieceTable);
  FTargetTable := ATargetTable;
  FRow := ARow;
  FPreviousTable := ARow.Table;
end;

procedure TdxMoveTableRowToOtherTableHistoryItem.RedoCore;
var
  AController: TdxTableConditionalFormattingController;
begin
  FTargetTable.Rows.AddInternal(FRow);
  FRow.SetTable(FTargetTable);
  AController.Init(FTargetTable);
  AController.ResetCachedProperties(FTargetTable.Rows.Count - 1);
end;

procedure TdxMoveTableRowToOtherTableHistoryItem.UndoCore;
var
  AController: TdxTableConditionalFormattingController;
  AIndex: Integer;
begin
  AIndex := FTargetTable.Rows.IndexOf(FRow);
  FTargetTable.Rows.ExtractRowCore(AIndex);
  AController.Init(FTargetTable);
  AController.ResetCachedProperties(AIndex);
  FRow.SetTable(FPreviousTable);
end;

{ TdxDeleteTableFromTableCollectionHistoryItem }

constructor TdxDeleteTableFromTableCollectionHistoryItem.Create(APieceTable: TdxCustomPieceTable;
  ADeletedTable: TdxTable);
begin
  inherited Create(APieceTable);
  Assert(ADeletedTable <> nil);
  FDeletedTable := ADeletedTable;
end;

destructor TdxDeleteTableFromTableCollectionHistoryItem.Destroy;
begin
  if FOwnDeletedTable then
    FDeletedTable.Free;
  inherited Destroy;
end;

procedure TdxDeleteTableFromTableCollectionHistoryItem.RedoCore;
var
  AIndex: Integer;
begin
  AIndex := PieceTable.Tables.IndexOf(DeletedTable);
  PieceTable.Tables.Remove(AIndex);
  FOwnDeletedTable := True;
  PieceTable.DocumentModel.InvalidateDocumentLayout;
end;

procedure TdxDeleteTableFromTableCollectionHistoryItem.UndoCore;
begin
  PieceTable.Tables.Add(DeletedTable);
  FOwnDeletedTable := False;
  PieceTable.DocumentModel.InvalidateDocumentLayout;
end;

{ TdxTableCellPropertiesChangedHistoryItem }

constructor TdxTableCellPropertiesChangedHistoryItem.Create(APieceTable: TdxCustomPieceTable;
  AFirstCellParagraphIndex: TdxParagraphIndex; ANestedLevel: Integer);
begin
  inherited Create(APieceTable);
  if AFirstCellParagraphIndex < 0 then
    TdxRichEditExceptions.ThrowArgumentException('firstCellParagraphIndex', AFirstCellParagraphIndex);
  FFirstCellParagraphIndex := AFirstCellParagraphIndex;
  FNestedLevel := ANestedLevel;
end;

function TdxTableCellPropertiesChangedHistoryItem.GetObject: TdxIndexBasedObject;
var
  ACell: TdxTableCell;
begin
  ACell := PieceTable.Paragraphs[FirstCellParagraphIndex].GetCell;
  while ACell.Table.NestedLevel <> FNestedLevel do
    ACell := ACell.Table.ParentCell;
  Result := ACell.Properties;
end;

function TdxTableCellPropertiesChangedHistoryItem.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

end.
