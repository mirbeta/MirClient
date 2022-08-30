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

unit dxRichEdit.DocumentFormats.DocumentFormatUtils;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.Import;

type
  { TdxVerticalMergeCellProperties }

  TdxVerticalMergeCellProperties = record
    RowSpan: Integer;
    ActualBottomBorder: TdxBorderBase;
    constructor Create(ARowSpan: Integer; AActualBottomBorder: TdxBorderBase);
  end;

  { TdxVerticalMergeCellsCollection }

  TdxVerticalMergeCellsCollection = class(TDictionary<TdxTableCell, TdxVerticalMergeCellProperties>)
  public
    function GetRowSpan(ACell: TdxTableCell): Integer;
    function GetMergedCellProperties(ACell: TdxTableCell): TdxVerticalMergeCellProperties;
  end;

  { TdxTableInfo }

  TdxTableInfo = class
  private const
    RootTableNestingLevel = 0;
  strict private
    FTable: TdxTable;
    FNestedLevel: Integer;
    FVerticalMergedCellsInTable: TdxVerticalMergeCellsCollection;
    function GetDocumentModel: TdxDocumentModel;
  protected
    function GetTableGridIntervals: TdxTableGridIntervalList; virtual;
    function GetTableGrid(AParentWidth: Integer): TdxTableGrid; virtual;
    function GetParentSectionWidth: Integer; virtual;
    function GetCellRowSpan(ACell: TdxTableCell): Integer;
  public
    constructor Create(ATable: TdxTable);
    destructor Destroy; override;

    function GetNestingLevel(ATable: TdxTable): Integer;
    function GetRootTable: TdxTable;
    function GetParentTable(ANestingLevel: Integer): TdxTable;
    function GetMergedCellProperties(ACell: TdxTableCell): TdxVerticalMergeCellProperties;
    function GetVerticalMergedCells: TdxVerticalMergeCellsCollection;
    function CalculateMergeCellProperties(ACurrentCell: TdxTableCell): TdxVerticalMergeCellProperties;

    property Table: TdxTable read FTable;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property NestedLevel: Integer read FNestedLevel;
  end;

  { TdxXmlTextHelper }

  TdxXmlTextHelper = class
  public
    class function DeleteIllegalXmlCharacters(const AValue: string): string; static;
    class function IsLegalXmlChar(ACharacter: Char): Boolean; static;
  end;

 { TdxCellsRowSpanCollection }

  TdxCellsRowSpanCollection = class(TdxIntegerList)
  end;

  { TdxImportedTableInfo }

  TdxImportedTableInfo = class(TdxTableInfo)
  strict private
    FCellsRowSpanCollection: TdxCellsRowSpanCollection;
    FColumnIndex: Integer;
  public
    constructor Create(ATable: TdxTable);
    destructor Destroy; override;
    procedure MoveNextRow;
    procedure AddCellToSpanCollection(ARowSpan, AColumnSpan: Integer); virtual;
    procedure ExpandSpanCollection(ARowSpan, AColumnSpan: Integer); virtual;
    function AddCellToSpanCollectionCore(AColumnIndex, ARowSpan, AColumnSpan: Integer): Integer; virtual;

    property CellsRowSpanCollection: TdxCellsRowSpanCollection read FCellsRowSpanCollection;
    property ColumnIndex: Integer read FColumnIndex write FColumnIndex;
  end;

  { TdxTablesImportHelper }

  TdxTablesImportHelper = class abstract
  strict private
    FPieceTable: TdxPieceTable;
    FTableStack: TdxObjectStack<TdxImportedTableInfo>;
    FTopLevelTableInfo: TdxImportedTableInfo;
    function GetTable: TdxTable;
    function GetTableStackCount: Integer;
    function GetIsInTable: Boolean;
    function GetTablesAllowed: Boolean;
    function GetTableInfo: TdxImportedTableInfo;
  protected
    function GetTopLevelTableInfo: TdxImportedTableInfo; virtual;
    function CreateTableInfo(ANewTable: TdxTable): TdxImportedTableInfo; virtual; abstract;
    function GetRowBeforeLastRow(ATable: TdxTable): TdxTableRow;
    function CreateCoveredInVerticalMergeCell(AColumnIndex: Integer): Integer; virtual;
    function CreateCoveredCellWithEmptyParagraph(ALastRow: TdxTableRow): TdxTableCell; virtual; abstract;
    procedure CloseUnclosedLastRowLastCell(ATable: TdxTable; ALastParagraphIndex: TdxParagraphIndex); virtual;
    function FindLastValidCellInTable(ATable: TdxTable): TdxTableCell;
    procedure RemoveEmptyRows(ACurrentTable: TdxTable); virtual;
  public
    constructor Create(APieceTable: TdxPieceTable);
    destructor Destroy; override;

    function CreateNewRow: TdxTableRow;
    function CreateNewRowOrGetLastEmpty: TdxTableRow;
    function IsEmptyLastRow: Boolean;
    procedure MoveNextRow; virtual;
    procedure RemoveEmptyRow;
    procedure SkipFakeRow;
    procedure InsertCoveredByVerticalMergingCellsInRow;
    procedure TablesCapabilityCheck;
    procedure InitializeTableCell(ACell: TdxTableCell; AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex); virtual;
    function CreateCoveredInVerticalMergeCells(ANextColumnIndex: Integer): Integer;
    function FindPatternCellWithMergeRestartInPreviousRows(ARow: TdxTableRow; AColumnIndex: Integer): TdxTableCell;
    function FindStartParagraphIndexForCell(AInputPositionPragraphIndex: TdxParagraphIndex): TdxParagraphIndex; virtual;
    procedure UpdateFirstRowSpanCollection(ARowSpan, AColumnSpan: Integer); virtual;
    procedure UpdateFirstRowSpanCollectionCore(ARowSpan: Integer; AColumnSpan: Integer);
    function ProcessUnCoveredCell(ARowSpan, AColumnSpan, AColumnIndex: Integer): Integer;
    function ProcessLastCellInRow(ARowSpan, AColumnSpan, ALocalColumnIndex: Integer): Integer;
    procedure AddCellToSpanCollection(ARowSpan, AColumnSpan: Integer);
    procedure ExpandSpanCollection(ARowSpan, AColumnSpan: Integer);
    function CreateTable(AParent: TdxTableCell): TdxTable;
    procedure FinalizeTableCreation; virtual;
    procedure PostProcessTableCells; virtual;
    procedure FixBrokenCells(ACurrentTable: TdxTable); virtual;
    procedure PushTable(ATable: TdxTable);
    procedure PopTable;

    property PieceTable: TdxPieceTable read FPieceTable;
    property Table: TdxTable read GetTable;
    property TableInfo: TdxImportedTableInfo read GetTableInfo;
    property TopLevelTableInfo: TdxImportedTableInfo read GetTopLevelTableInfo;
    property TableStackCount: Integer read GetTableStackCount;
    property IsInTable: Boolean read GetIsInTable;
    property TablesAllowed: Boolean read GetTablesAllowed;
  end;

  { TdxImportFieldHelper }

  TdxImportFieldHelper = class
  strict private
    FPieceTable: TdxPieceTable;
  public
    constructor Create(APieceTable: TdxPieceTable);
    procedure ProcessFieldBegin(AFieldInfo: TdxImportFieldInfo; APosition: TdxInputPosition);
    procedure ProcessFieldSeparator(AFieldInfo: TdxImportFieldInfo; APosition: TdxInputPosition);
    function ProcessFieldEnd(AFieldInfo: TdxImportFieldInfo; APosition: TdxInputPosition): TdxField;
    procedure InsertHyperlinkInstruction(AInfo: TdxHyperlinkInfo; APosition: TdxInputPosition);

    property PieceTable: TdxPieceTable read FPieceTable;
  end;

implementation

uses
  Contnrs, Math,
  dxRichEdit.Options,
  dxCharacters,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.TableCalculator,
  dxRichEdit.DocumentModel.Core;

{ TdxVerticalMergeCellsCollection }

function TdxVerticalMergeCellsCollection.GetRowSpan(ACell: TdxTableCell): Integer;
begin
  if ContainsKey(ACell) then
    Exit(Self[ACell].RowSpan);
  Result := 1;
end;

function TdxVerticalMergeCellsCollection.GetMergedCellProperties(ACell: TdxTableCell): TdxVerticalMergeCellProperties;
begin
  if ContainsKey(ACell) then
    Exit(Self[ACell]);
  Result := TdxVerticalMergeCellProperties.Create(1, nil);
end;

{ TdxTableInfo }

constructor TdxTableInfo.Create(ATable: TdxTable);
begin
  inherited Create;
  FNestedLevel := -1;
  FTable := ATable;
  FNestedLevel := GetNestingLevel(ATable);
  FVerticalMergedCellsInTable := GetVerticalMergedCells;
end;

destructor TdxTableInfo.Destroy;
begin
  FreeAndNil(FVerticalMergedCellsInTable);
  inherited Destroy;
end;

function TdxTableInfo.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(FTable.DocumentModel);
end;

function TdxTableInfo.GetNestingLevel(ATable: TdxTable): Integer;
var
  AParentCell: TdxTableCell;
  AParentTable: TdxTable;
begin
  Result := 0;
  AParentCell := ATable.ParentCell;
  while AParentCell <> nil do
  begin
    AParentTable := AParentCell.Row.Table;
    AParentCell := AParentTable.ParentCell;
    Inc(Result);
  end;
end;

function TdxTableInfo.GetTableGridIntervals: TdxTableGridIntervalList;
var
  AWidthsCalculator: TdxRtfTableWidthsCalculator;
  ACalculator: TdxTableGridCalculator;
begin
  AWidthsCalculator := TdxRtfTableWidthsCalculator.Create(DocumentModel.ToDocumentLayoutUnitConverter);
  try
    ACalculator := TdxTableGridCalculator.Create(AWidthsCalculator, MaxInt);
    try
      Result := ACalculator.CalculateGridIntervals(Table, MaxInt);
    finally
      ACalculator.Free;
    end;
  finally
    AWidthsCalculator.Free;
  end;
end;

function TdxTableInfo.GetTableGrid(AParentWidth: Integer): TdxTableGrid;
var
  AWidthsCalculator: TdxRtfTableWidthsCalculator;
  ACalculator: TdxTableGridCalculator;
begin
  AWidthsCalculator := TdxRtfTableWidthsCalculator.Create(DocumentModel.ToDocumentLayoutUnitConverter, AParentWidth);
  try
    ACalculator := TdxTableGridCalculator.Create(AWidthsCalculator, MaxInt);
    try
      Result := ACalculator.CalculateTableGrid(FTable, AParentWidth);
    finally
      ACalculator.Free;
    end;
  finally
    AWidthsCalculator.Free;
  end;
end;

function TdxTableInfo.GetParentSectionWidth: Integer;
var
  APos: TdxDocumentModelPosition;
  ASectionIndex: TdxSectionIndex;
  ASection: TdxSection;
  ASectionMargins: TdxSectionMargins;
  AMargins: Integer;
begin
  APos := TdxDocumentModelPosition.FromParagraphStart(FTable.PieceTable, FTable.FirstRow.FirstCell.StartParagraphIndex);
  ASectionIndex := FTable.DocumentModel.FindSectionIndex(APos.LogPosition);
  ASection := DocumentModel.Sections[ASectionIndex];
  ASectionMargins := ASection.Margins;
  AMargins := ASectionMargins.Left + ASectionMargins.Right;
  Result := ASection.Page.Width - AMargins;
end;

function TdxTableInfo.GetRootTable: TdxTable;
begin
  Result := GetParentTable(RootTableNestingLevel);
end;

function TdxTableInfo.GetParentTable(ANestingLevel: Integer): TdxTable;
var
  ACurrentTableNestingLevel: Integer;
  AParentCell: TdxTableCell;
begin
  if ANestingLevel = NestedLevel then
    Exit(FTable);
  Result := FTable;
  ACurrentTableNestingLevel := NestedLevel;
  while ACurrentTableNestingLevel > ANestingLevel do
  begin
    AParentCell := Result.ParentCell;
    Result := AParentCell.Row.Table;
    Dec(ACurrentTableNestingLevel);
  end;
end;

function TdxTableInfo.GetCellRowSpan(ACell: TdxTableCell): Integer;
begin
  Result := FVerticalMergedCellsInTable.GetRowSpan(ACell);
end;

function TdxTableInfo.GetMergedCellProperties(ACell: TdxTableCell): TdxVerticalMergeCellProperties;
begin
  Result := FVerticalMergedCellsInTable.GetMergedCellProperties(ACell);
end;

function TdxTableInfo.GetVerticalMergedCells: TdxVerticalMergeCellsCollection;
var
  ARows: TdxTableRowCollection;
  I, J: Integer;
  ACurrentCell: TdxTableCell;
  ACurrentRow: TdxTableRow;
  AMergeCellProperties: TdxVerticalMergeCellProperties;
begin
  Result := TdxVerticalMergeCellsCollection.Create;
  ARows := FTable.Rows;
  for I := 0 to ARows.Count - 1 do
  begin
    ACurrentRow := ARows[I];
    for J := 0 to ACurrentRow.Cells.Count - 1 do
    begin
      ACurrentCell := ACurrentRow.Cells[J];
      case ACurrentCell.VerticalMerging of
        TdxMergingState.Restart:
          begin
            AMergeCellProperties := CalculateMergeCellProperties(ACurrentCell);
            Result.Add(ACurrentCell, AMergeCellProperties);
          end;
        TdxMergingState.Continue:
          Result.Add(ACurrentCell, TdxVerticalMergeCellProperties.Create(0, nil));
      end;
    end;
  end;
end;

function TdxTableInfo.CalculateMergeCellProperties(ACurrentCell: TdxTableCell): TdxVerticalMergeCellProperties;
var
  AColumnIndex, AVerticalMergeCellsCount: Integer;
  AVerticalMergedCells: TdxTableCellList;
  AActualBottomCellBorder: TdxBorderBase;
begin
  AColumnIndex := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(ACurrentCell, False);
  AVerticalMergedCells := TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(ACurrentCell, AColumnIndex, False);
  try
    AVerticalMergeCellsCount := AVerticalMergedCells.Count;
    AActualBottomCellBorder := AVerticalMergedCells[AVerticalMergeCellsCount - 1].GetActualBottomCellBorder;
  finally
    AVerticalMergedCells.Free;
  end;
  Result := TdxVerticalMergeCellProperties.Create(AVerticalMergeCellsCount, AActualBottomCellBorder);
end;

{ TdxVerticalMergeCellProperties }

constructor TdxVerticalMergeCellProperties.Create(ARowSpan: Integer;
  AActualBottomBorder: TdxBorderBase);
begin
  RowSpan := ARowSpan;
  ActualBottomBorder := AActualBottomBorder;
end;

{ TdxXmlTextHelper }

class function TdxXmlTextHelper.IsLegalXmlChar(ACharacter: Char): Boolean;
begin
  Result := ACharacter >= TdxCharacters.Space;
end;

class function TdxXmlTextHelper.DeleteIllegalXmlCharacters(const AValue: string): string;
var
  ACount, ADestLen: Integer;
  ASrc, ADest: PChar;
begin
  ACount := Length(AValue);
  if ACount = 0 then
    Exit('');

  SetLength(Result, ACount);
  ADestLen := 0;
  ASrc := PChar(AValue);
  ADest := PChar(Result);
  repeat
    if IsLegalXmlChar(ASrc^) then
    begin
      ADest^ := ASrc^;
      Inc(ADest);
      Inc(ADestLen);
    end;
    Inc(ASrc);
    Dec(ACount);
  until ACount = 0;
  SetLength(Result, ADestLen);
end;


{ TdxCellsRowSpanCollection }


{ TdxImportedTableInfo }

constructor TdxImportedTableInfo.Create(ATable: TdxTable);
begin
  inherited Create(ATable);
  FCellsRowSpanCollection := TdxCellsRowSpanCollection.Create;
end;

destructor TdxImportedTableInfo.Destroy;
begin
  FreeAndNil(FCellsRowSpanCollection);
  inherited Destroy;
end;

procedure TdxImportedTableInfo.MoveNextRow;
begin
  FColumnIndex := 0;
end;

procedure TdxImportedTableInfo.AddCellToSpanCollection(ARowSpan, AColumnSpan: Integer);
begin
  AddCellToSpanCollectionCore(FColumnIndex, ARowSpan, AColumnSpan);
end;

procedure TdxImportedTableInfo.ExpandSpanCollection(ARowSpan, AColumnSpan: Integer);
var
  AColumnIndex, ANewCount, I: Integer;
begin
  AColumnIndex := ColumnIndex;
  if AColumnIndex >= CellsRowSpanCollection.Count then
    CellsRowSpanCollection.Add(ARowSpan);
  ANewCount := ColumnIndex + AColumnSpan - CellsRowSpanCollection.Count;
  for I := 0 to ANewCount - 1 do
    CellsRowSpanCollection.Add(0);
end;

function TdxImportedTableInfo.AddCellToSpanCollectionCore(AColumnIndex, ARowSpan, AColumnSpan: Integer): Integer;
var
  I: Integer;
begin
  CellsRowSpanCollection.Add(ARowSpan);
  for I := AColumnSpan downto 1 + 1 do
  begin
    CellsRowSpanCollection.Add(0);
    Inc(AColumnIndex){POST-INCREMENT};
  end;
  Result := AColumnIndex;
end;

{ TdxTablesImportHelper }

constructor TdxTablesImportHelper.Create(APieceTable: TdxPieceTable);
begin
  Assert(APieceTable <> nil, 'pieceTable');
  FPieceTable := APieceTable;
  FTableStack := TdxObjectStack<TdxImportedTableInfo>.Create(False);
end;

destructor TdxTablesImportHelper.Destroy;
begin
  FreeAndNil(FTableStack);
  inherited Destroy;
end;

function TdxTablesImportHelper.GetTable: TdxTable;
begin
  if (FTableStack.Count = 0) then
    Result := nil
  else
    Result := FTableStack.Peek.Table;
end;

function TdxTablesImportHelper.GetTableInfo: TdxImportedTableInfo;
begin
  if (FTableStack.Count = 0) then
    Result := nil
  else
    Result := FTableStack.Peek;
end;

function TdxTablesImportHelper.GetTopLevelTableInfo: TdxImportedTableInfo;
begin
  Result := FTopLevelTableInfo;
end;

function TdxTablesImportHelper.GetTableStackCount: Integer;
begin
  Result := FTableStack.Count;
end;

function TdxTablesImportHelper.GetIsInTable: Boolean;
begin
  Result := TableStackCount > 0;
end;

function TdxTablesImportHelper.GetTablesAllowed: Boolean;
var
  AOptions: TdxDocumentCapabilitiesOptions;
begin
  AOptions := FPieceTable.DocumentModel.DocumentCapabilities;
  Result := AOptions.TablesAllowed and AOptions.ParagraphsAllowed;
end;

function TdxTablesImportHelper.CreateNewRow: TdxTableRow;
var
  ARow: TdxTableRow;
begin
  TablesCapabilityCheck;
  ARow := TdxTableRow.Create(Table);
  ARow.Table.Rows.AddInternal(ARow);
  MoveNextRow;
  InsertCoveredByVerticalMergingCellsInRow;
  Result := ARow;
end;

function TdxTablesImportHelper.CreateNewRowOrGetLastEmpty: TdxTableRow;
begin
  if IsEmptyLastRow then
    Result := Table.LastRow
  else
    Result := CreateNewRow;
end;

function TdxTablesImportHelper.IsEmptyLastRow: Boolean;
var
  ANoRows, ANoCells: Boolean;
begin
  ANoRows := Table.Rows.Count = 0;
  if ANoRows then
    Exit(False);
  ANoCells := Table.LastRow.Cells.Count = 0;
  Result := ANoCells;
end;

procedure TdxTablesImportHelper.MoveNextRow;
begin
  TableInfo.ColumnIndex := 0;
end;

procedure TdxTablesImportHelper.RemoveEmptyRow;
var
  ARow: TdxTableRow;
begin
  ARow := Table.LastRow;
  if ARow.Cells.Count = 0 then
  begin
    ARow.Table.Rows.RemoveInternal(ARow);
    SkipFakeRow;
  end;
end;

procedure TdxTablesImportHelper.SkipFakeRow;
begin
  TableInfo.ColumnIndex := 0;
end;

procedure TdxTablesImportHelper.InsertCoveredByVerticalMergingCellsInRow;
var
  ACellsInPreviousRowCount, AColumnIndex: Integer;
begin
  if (Table.Rows.Count = 1) or (TableInfo.CellsRowSpanCollection.Count = 0) then
    Exit;
  TablesCapabilityCheck;
  ACellsInPreviousRowCount := TableInfo.CellsRowSpanCollection.Count;
  AColumnIndex := TableInfo.ColumnIndex;
  while (AColumnIndex < ACellsInPreviousRowCount) and (TableInfo.CellsRowSpanCollection[AColumnIndex] > 1) do
    AColumnIndex := CreateCoveredInVerticalMergeCell(AColumnIndex);

  TableInfo.ColumnIndex := Min(AColumnIndex, ACellsInPreviousRowCount);
end;

procedure TdxTablesImportHelper.TablesCapabilityCheck;
begin
  Assert(IsInTable and (Table <> nil));
  Assert(Table.DocumentModel.DocumentCapabilities.TablesAllowed);
  Assert(Table.DocumentModel.DocumentCapabilities.ParagraphsAllowed);
end;

procedure TdxTablesImportHelper.InitializeTableCell(ACell: TdxTableCell; AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex);
var
  AInvalid: TdxParagraphIndex;
begin
  AInvalid := - 1;
  Assert((AStartParagraphIndex <> AInvalid) and (AEndParagraphIndex <> AInvalid));
  if ((ACell <> nil) and (AStartParagraphIndex <> AInvalid)) and (AEndParagraphIndex <> AInvalid) then
    PieceTable.TableCellsManager.InitializeTableCell(ACell, AStartParagraphIndex, AEndParagraphIndex);
end;

function TdxTablesImportHelper.CreateCoveredInVerticalMergeCells(ANextColumnIndex: Integer): Integer;
begin
  TablesCapabilityCheck;

  Result := ANextColumnIndex;
  while (Result < TableInfo.CellsRowSpanCollection.Count) and (TableInfo.CellsRowSpanCollection[Result] > 1) do
    Result := CreateCoveredInVerticalMergeCell(Result);
end;

function TdxTablesImportHelper.FindPatternCellWithMergeRestartInPreviousRows(ARow: TdxTableRow; AColumnIndex: Integer): TdxTableCell;
var
  AFoundCell: TdxTableCell;
begin
  AFoundCell := Table.GetCell(ARow, AColumnIndex);
  if AFoundCell = nil then
    Exit(nil);
  if AFoundCell.VerticalMerging <> TdxMergingState.Restart then
    AFoundCell := FindPatternCellWithMergeRestartInPreviousRows(ARow.Previous, AColumnIndex);
  Result := AFoundCell;
end;

function TdxTablesImportHelper.GetRowBeforeLastRow(ATable: TdxTable): TdxTableRow;
var
  ARows: TdxTableRowCollection;
  ACount: Integer;
begin
  ARows := ATable.Rows;
  ACount := ARows.Count;
  if ACount > 1 then
    Result := ARows[ACount - 2]
  else
    Result := nil;
end;

function TdxTablesImportHelper.CreateCoveredInVerticalMergeCell(AColumnIndex: Integer): Integer;
var
  ACovered, APatternCell, AHorizontalCoveredCell: TdxTableCell;
  ANextColumnIndex, AExpectedNextColumnIndex: Integer;
begin
  TablesCapabilityCheck;
  ACovered := CreateCoveredCellWithEmptyParagraph(Table.LastRow);

  Assert(AColumnIndex = Table.GetCellColumnIndexConsiderRowGrid(ACovered));
  APatternCell := FindPatternCellWithMergeRestartInPreviousRows(GetRowBeforeLastRow(Table), AColumnIndex);
  if APatternCell <> nil then
    ACovered.Properties.CopyFrom(APatternCell.Properties);

  ACovered.Properties.VerticalMerging := TdxMergingState.Continue;

  Assert(TableInfo.CellsRowSpanCollection.Count > AColumnIndex);
  if TableInfo.CellsRowSpanCollection.Count <= AColumnIndex then
    Exit(AColumnIndex);

  TableInfo.CellsRowSpanCollection[AColumnIndex] := TableInfo.CellsRowSpanCollection[AColumnIndex] - 1;
  ANextColumnIndex := AColumnIndex + 1;
  while (ANextColumnIndex < TableInfo.CellsRowSpanCollection.Count) and (TableInfo.CellsRowSpanCollection[ANextColumnIndex] = 0) do
  begin
    Inc(ANextColumnIndex);
  end;
  if ANextColumnIndex < TableInfo.CellsRowSpanCollection.Count then
  begin
    AExpectedNextColumnIndex := AColumnIndex + ACovered.ColumnSpan;
    if AExpectedNextColumnIndex < ANextColumnIndex then
    begin
      AHorizontalCoveredCell := CreateCoveredCellWithEmptyParagraph(Table.LastRow);
      AHorizontalCoveredCell.ColumnSpan := ANextColumnIndex - AExpectedNextColumnIndex;
    end;
  end;
  Result := ANextColumnIndex;
end;

procedure TdxTablesImportHelper.UpdateFirstRowSpanCollection(ARowSpan: Integer; AColumnSpan: Integer);
begin
  TablesCapabilityCheck;
  if Table.Rows.Count > 1 then
    UpdateFirstRowSpanCollectionCore(ARowSpan, AColumnSpan);
end;

procedure TdxTablesImportHelper.UpdateFirstRowSpanCollectionCore(ARowSpan, AColumnSpan: Integer);
var
  AColumnIndex, ANextColumnIndex, ACellCollectionCount, I, AIndexOfLastCell: Integer;
begin
  AColumnIndex := TableInfo.ColumnIndex;
  ANextColumnIndex := AColumnIndex + 1;

  ACellCollectionCount := TableInfo.CellsRowSpanCollection.Count;
  if ACellCollectionCount = 0 then
  begin
    TableInfo.ColumnIndex := 0;
    Exit;
  end;
  if ACellCollectionCount <= AColumnIndex then
    for I := 0 to AColumnIndex - ACellCollectionCount + 1 - 1 do
      TableInfo.CellsRowSpanCollection.Add(1);

  if TableInfo.CellsRowSpanCollection[AColumnIndex] < 2 then
    TableInfo.CellsRowSpanCollection[AColumnIndex] := ARowSpan;
  if ANextColumnIndex = TableInfo.CellsRowSpanCollection.Count then
  begin
    if AColumnSpan > 1 then
      TableInfo.ColumnIndex := ProcessLastCellInRow(ARowSpan, AColumnSpan, AColumnIndex)
    else
      TableInfo.ColumnIndex := ANextColumnIndex;
  end
  else
    if TableInfo.CellsRowSpanCollection[ANextColumnIndex] > 1 then
    begin

      AIndexOfLastCell := Table.GetCellColumnIndexConsiderRowGrid(Table.LastRow.LastCell) + Table.LastRow.LastCell.ColumnSpan;
      if AIndexOfLastCell >= ANextColumnIndex then
      begin
        Table.LastRow.LastCell.ColumnSpan := 1;
      end;
      TableInfo.ColumnIndex := CreateCoveredInVerticalMergeCells(ANextColumnIndex);
    end
    else
    begin
      TableInfo.ColumnIndex := ProcessUnCoveredCell(ARowSpan, AColumnSpan, AColumnIndex);
      TableInfo.ColumnIndex := CreateCoveredInVerticalMergeCells(TableInfo.ColumnIndex);
    end;
end;

function TdxTablesImportHelper.ProcessUnCoveredCell(ARowSpan, AColumnSpan, AColumnIndex: Integer): Integer;
var
  ALocalColumnIndex, AUpperBounds, I: Integer;
begin
  ALocalColumnIndex := AColumnIndex;
  AUpperBounds := TableInfo.CellsRowSpanCollection.Count - 1;
  I := AColumnSpan;
  while (I > 1) and (ALocalColumnIndex < AUpperBounds) do
  begin
    Inc(ALocalColumnIndex);
    TableInfo.CellsRowSpanCollection[ALocalColumnIndex] := 0;
    Dec(I);
  end;
  Result := ALocalColumnIndex + 1;
end;

function TdxTablesImportHelper.ProcessLastCellInRow(ARowSpan, AColumnSpan, ALocalColumnIndex: Integer): Integer;
var
  I: Integer;
begin
  I := AColumnSpan;
  while (I > 1) and (ALocalColumnIndex < TableInfo.CellsRowSpanCollection.Count) do
  begin
    TableInfo.CellsRowSpanCollection[ALocalColumnIndex] := 0;
    Inc(ALocalColumnIndex);
    Dec(I);
  end;
  Result := ALocalColumnIndex;
end;

procedure TdxTablesImportHelper.AddCellToSpanCollection(ARowSpan, AColumnSpan: Integer);
begin
  TablesCapabilityCheck;
  TableInfo.AddCellToSpanCollection(ARowSpan, AColumnSpan);
end;

procedure TdxTablesImportHelper.ExpandSpanCollection(ARowSpan, AColumnSpan: Integer);
begin
  TableInfo.ExpandSpanCollection(ARowSpan, AColumnSpan);
end;

function TdxTablesImportHelper.FindStartParagraphIndexForCell(AInputPositionPragraphIndex: TdxParagraphIndex): TdxParagraphIndex;
var
  ATable: TdxTable;
  ALastCellParagraphIndex: TdxParagraphIndex;
  APrevious: TdxTableRow;
begin
  ATable := Table;

  if ATable.LastRow.Cells.Count = 1 then
  begin
    APrevious := GetRowBeforeLastRow(ATable);
    if APrevious = nil then
      Exit(AInputPositionPragraphIndex);
    ALastCellParagraphIndex := APrevious.LastCell.EndParagraphIndex;
  end
  else
    ALastCellParagraphIndex := ATable.LastRow.Cells[ATable.LastRow.Cells.Count - 2].EndParagraphIndex;

  if AInputPositionPragraphIndex > ALastCellParagraphIndex + 1 then
    Result := ALastCellParagraphIndex + 1
  else
    Result := AInputPositionPragraphIndex;
end;

function TdxTablesImportHelper.CreateTable(AParent: TdxTableCell): TdxTable;
begin
  Result := TdxTable.Create(PieceTable, AParent, 0, 0);
  PushTable(Result);
  PieceTable.Tables.Add(Result);
end;

procedure TdxTablesImportHelper.FinalizeTableCreation;
begin
  TablesCapabilityCheck;
  if (Table.Rows.Count = 0) or (Table.Rows.First.Cells.Count = 0) then
    PieceTable.Tables.Remove(Table.Index)
  else
  begin
    CloseUnclosedLastRowLastCell(Table, FPieceTable.Paragraphs.Last.Index - 1);

    PostProcessTableCells;
    RemoveEmptyRows(Table);

    FixBrokenCells(Table);
  end;
  PopTable;
end;

procedure TdxTablesImportHelper.CloseUnclosedLastRowLastCell(ATable: TdxTable; ALastParagraphIndex: TdxParagraphIndex);
var
  AInvalid: TdxParagraphIndex;
  ALastCell: TdxTableCell;
begin
  AInvalid := - 1;
  ALastCell := FindLastValidCellInTable(ATable);
  if ALastCell = nil then
    Exit;
  if ALastCell.EndParagraphIndex = AInvalid then
  begin
    if ALastParagraphIndex > 0 then
    begin
      ALastCell.EndParagraphIndex := ALastParagraphIndex;
      InitializeTableCell(ALastCell, ALastCell.StartParagraphIndex, ALastCell.EndParagraphIndex);
    end;
  end;
end;

function TdxTablesImportHelper.FindLastValidCellInTable(ATable: TdxTable): TdxTableCell;
var
  ARows: TdxTableRowCollection;
  ACount, ARowIndex: Integer;
begin
  ARows := ATable.Rows;
  ACount := ARows.Count;
  ARowIndex := ACount - 1;
  while (ARowIndex >= 0) and (ARows[ARowIndex].Cells.Count = 0) do
  begin
    Dec(ARowIndex);
  end;
  if ((ARowIndex < 0) or (ARows[ARowIndex].Cells.Count = 0)) then
    Result := nil
  else
    Result := ARows[ARowIndex].LastCell;
end;

procedure TdxTablesImportHelper.RemoveEmptyRows(ACurrentTable: TdxTable);
var
  ARowsCount, ARowId: Integer;
  ARow: TdxTableRow;
begin
  ARowsCount := ACurrentTable.Rows.Count;
  for ARowId := ARowsCount - 1 downto 0 do
  begin
    ARow := ACurrentTable.Rows[ARowId];
    if ARow.Cells.Count = 0 then
    begin
      ACurrentTable.Rows.RemoveInternal(ARow);
    end;
  end;
end;

procedure TdxTablesImportHelper.PostProcessTableCells;
var
  ARows: TdxTableRowCollection;
  ARowCount, ARowIndex, ACellCount, ACellIndex: Integer;
  AInvalidParagraphIndex: TdxParagraphIndex;
  ACells: TdxTableCellCollection;
  ACell: TdxTableCell;
begin
  ARows := Table.Rows;
  ARowCount := ARows.Count;
  AInvalidParagraphIndex := -1;
  for ARowIndex := 0 to ARowCount - 1 - 1 do
  begin
    ACells := ARows[ARowIndex].Cells;
    ACellCount := ACells.Count;
    for ACellIndex := ACellCount - 1 downto 0 do
    begin
      ACell := ACells[ACellIndex];
      if (ACell.StartParagraphIndex = AInvalidParagraphIndex) or (ACell.EndParagraphIndex = AInvalidParagraphIndex) then
        ACells.DeleteInternal(ACell);
    end;
  end;
end;

procedure TdxTablesImportHelper.FixBrokenCells(ACurrentTable: TdxTable);
var
  ARowCount, AExpectedMaxColumnIndex, I, AMaxColumnIndex, ANewEndColumnListIndex, ANewEndColumnIndex: Integer;
  AExistingPositions: TdxSortedList<Integer>;
  ARow: TdxTableRow;
begin
  ARowCount := ACurrentTable.Rows.Count;
  if ARowCount < 1 then
    Exit;
  AExistingPositions := ACurrentTable.GetExistingValidColumnsPositions;
  try
    AExpectedMaxColumnIndex := AExistingPositions[AExistingPositions.Count - 1];
    for I := 0 to ARowCount - 1 do
    begin
      ARow := ACurrentTable.Rows[I];
      AMaxColumnIndex := ACurrentTable.GetTotalCellsInRowConsiderGrid(ARow);
      if AMaxColumnIndex < AExpectedMaxColumnIndex then
      begin
        ANewEndColumnListIndex := AExistingPositions.BinarySearch(AMaxColumnIndex);
        if ANewEndColumnListIndex < 0 then
          ANewEndColumnListIndex := not ANewEndColumnListIndex;
        ANewEndColumnIndex := AExistingPositions[ANewEndColumnListIndex];
        if ANewEndColumnIndex <> AMaxColumnIndex then
          ARow.LastCell.ColumnSpan := ARow.LastCell.ColumnSpan + ANewEndColumnIndex - AMaxColumnIndex;
        if ANewEndColumnIndex <> AExpectedMaxColumnIndex then
          ARow.GridAfter := ARow.GridAfter + AExpectedMaxColumnIndex - ANewEndColumnIndex;
      end;
    end;
    Table.Normalize;
  finally
    AExistingPositions.Free;
  end;
end;

procedure TdxTablesImportHelper.PushTable(ATable: TdxTable);
var
  AInfo: TdxImportedTableInfo;
begin
  AInfo := CreateTableInfo(ATable);
  FTableStack.Push(AInfo);
  if FTableStack.Count = 1 then
    FTopLevelTableInfo := AInfo;
end;

procedure TdxTablesImportHelper.PopTable;
var
  AInfo: TdxImportedTableInfo;
begin
  AInfo := FTableStack.Extract;
  AInfo.Free;
  if FTableStack.Count = 0 then
    FTopLevelTableInfo := nil;
end;

{ TdxImportFieldHelper }

constructor TdxImportFieldHelper.Create(APieceTable: TdxPieceTable);
begin
  Assert(APieceTable <> nil, 'pieceTable');
  FPieceTable := APieceTable;
end;

procedure TdxImportFieldHelper.ProcessFieldBegin(AFieldInfo: TdxImportFieldInfo; APosition: TdxInputPosition);
begin
  AFieldInfo.CodeStartIndex := PieceTable.InsertFieldCodeStartRunCore(APosition);
end;

procedure TdxImportFieldHelper.ProcessFieldSeparator(AFieldInfo: TdxImportFieldInfo; APosition: TdxInputPosition);
begin
  AFieldInfo.CodeEndIndex := PieceTable.InsertFieldCodeEndRunCore(APosition);
end;

function TdxImportFieldHelper.ProcessFieldEnd(AFieldInfo: TdxImportFieldInfo; APosition: TdxInputPosition): TdxField;
var
  AField: TdxField;
  AFields: TdxFieldCollection;
begin
  if AFieldInfo.CodeEndIndex <= AFieldInfo.CodeStartIndex then
    AFieldInfo.CodeEndIndex := PieceTable.InsertFieldCodeEndRunCore(APosition);

  AFieldInfo.ResultEndIndex := PieceTable.InsertFieldResultEndRunCore(APosition);

  AField := AFieldInfo.Field;
  AField.DisableUpdate := AFieldInfo.DisableUpdate;
  AField.HideByParent := AFieldInfo.HideByParent;
  AField.Locked := AFieldInfo.Locked;
  AField.Code.SetInterval(AFieldInfo.CodeStartIndex, AFieldInfo.CodeEndIndex);
  AField.Result.SetInterval(AFieldInfo.CodeEndIndex + 1, AFieldInfo.ResultEndIndex);
  AFields := PieceTable.Fields;
  AField.Index := AFields.Count;
  AFields.Add(AField);
  AFieldInfo.IsFieldUsed := True;
  Result := AField;
end;

procedure TdxImportFieldHelper.InsertHyperlinkInstruction(AInfo: TdxHyperlinkInfo; APosition: TdxInputPosition);
var
  ABuilder: TdxHyperlinkInstructionBuilder;
begin
  ABuilder := TdxHyperlinkInstructionBuilder.Create(AInfo);
  try
    PieceTable.InsertTextCore(APosition, ABuilder.GetFieldInstruction);
  finally
    ABuilder.Free;
  end;
end;

end.
