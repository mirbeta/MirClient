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

unit dxRichEdit.Import.Doc.DocTablesImportHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreGraphics, dxGenerics, cxClasses,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.Import.Doc.DCO;

type
  { TdxRowLeftAndCellWidths }

  TdxRowLeftAndCellWidths = class
  strict private
    FCellWidths: TdxIntegerList;
    FRowLeft: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property RowLeft: Integer read FRowLeft write FRowLeft;
    property CellWidths: TdxIntegerList read FCellWidths;
  end;

  { TdxVerticalMergingStateUpdater }

  TdxVerticalMergingStateUpdater = class
  strict private
    FVerticalMergedCells: TObjectDictionary<TdxTableRow, TdxVerticalMergeInfoList>;
  protected
    property VerticalMergedCells: TObjectDictionary<TdxTableRow, TdxVerticalMergeInfoList> read FVerticalMergedCells;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpdateCellsVerticalMergeState(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
    procedure ApplyVerticalMerging(ATable: TdxTable);
    procedure AddVerticalMergedCells(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
    procedure ApplyCellVerticalAlignmentActions(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
    function GetVerticalMergingInfo(ARow: TdxTableRow): TdxVerticalMergeInfoList;
  end;

  { TdxDocTableLayoutUpdater }

  TdxDocTableLayoutUpdater = class
  public const
    Tolerance = 32;
  strict private
    FUnitConverter: TdxDocumentModelUnitConverter;
    FVerticalMergingUpdater: TdxVerticalMergingStateUpdater;
    FTableCellPositions: TDictionary<TdxTable, TdxIntegerList>;
    FRowCellWidths: TDictionary<TdxTableRow, TdxRowLeftAndCellWidths>;
    FPreferredCellWidths: TDictionary<TdxTableRow, TdxTableCellWidthOperandList>;
    FImporter: TObject{TdxDocImporter};
  protected
    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter;
    property VerticalMergingUpdater: TdxVerticalMergingStateUpdater read FVerticalMergingUpdater;
    property TableCellPositions: TDictionary<TdxTable, TdxIntegerList> read FTableCellPositions;
    property RowCellWidths: TDictionary<TdxTableRow, TdxRowLeftAndCellWidths> read FRowCellWidths;
    property PreferredCellWidths: TDictionary<TdxTableRow, TdxTableCellWidthOperandList> read FPreferredCellWidths;
  public
    constructor Create(AImporter: TObject{TdxDocImporter}; AUnitConverter: TdxDocumentModelUnitConverter);
    destructor Destroy; override;

    procedure PerformRowActions(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
    procedure AddPreferredCellWidths(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
    procedure AddRowCellPositions(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
    function GetTableCellPositions(ATable: TdxTable): TdxIntegerList;
    procedure ProcessTableDefinition(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer; ARowCellSizeAndPosition: TdxRowLeftAndCellWidths);
    procedure ApplyInsertActions(APropertyContainer: TdxDocPropertyContainer; ARowCellWidths: TdxIntegerList);
    procedure ApplyColumnWidthsActions(APropertyContainer: TdxDocPropertyContainer; ARowCellWidths: TdxIntegerList);
    procedure AddTableCellPositions(ARow: TdxTableRow; ARowLeftAndCellsWidth: TdxRowLeftAndCellWidths; ATableCellPositions: TdxIntegerList; APropertyContainer: TdxDocPropertyContainer);
    function GetRowLeft(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer): Integer;
    procedure TryToAddCellPosition(ADestination: TdxIntegerList; APosition: Integer);
    procedure UpdateLayout(ATable: TdxTable);
    procedure ApplySpans(ATable: TdxTable);
    function CalcMinRowLeft(ATable: TdxTable; ARowLeftAndCellsWidthCollection: TdxObjectList<TdxRowLeftAndCellWidths>): Integer;
    procedure CorrectCellWidthsAndSpans(ARow: TdxTableRow; ACellPositions: TdxIntegerList; ARowLeftAndCellsWidth: TdxRowLeftAndCellWidths; AMinRowLeft: Integer);
    function ProcessRowCells(ARow: TdxTableRow; ACellPositions: TdxIntegerList; ACellWidths: TdxIntegerList; ACurrentPosition: Integer): Integer;
    function ApplyActualColumnSpans(ARow: TdxTableRow; ACellPositions: TdxIntegerList; ACellWidths: TdxIntegerList; ACurrentPosition: Integer): Integer;
    function ApplyDefaultColumnSpans(ARow: TdxTableRow; ACellPositions: TdxIntegerList; ACellWidths: TdxIntegerList; ACurrentPosition: Integer): Integer;
    function CalcColumnSpan(ACellPositions: TdxIntegerList; ACurrentPosition: Integer; ACurrentWidth: Integer): Integer;
    procedure ApplyPreferredWidths(ARow: TdxTableRow; APreferredCellWidths: TdxTableCellWidthOperandList);
    procedure ApplyPreferredWidthsCore(ARow: TdxTableRow; AOperand: TdxTableCellWidthOperand);
    procedure ApplyTablePropertiesException(ARow: TdxTableRow; AInfo: TdxTableRowInfo);
  end;

  { TdxDocTablesImporter }

  TdxDocTablesImporter = class
  strict private
    FRows: TcxObjectList;
    FImporter: TObject;
    FPieceTable: TdxPieceTable;
    FTableCells: TStack<TdxTableCell>;
    FTableLayoutUpdater: TdxDocTableLayoutUpdater;
    FLastAddedParagraphIndex: TdxParagraphIndex;
    FHorizontalAnchor: TdxHorizontalAnchorTypes;
    FVerticalAnchor: TdxVerticalAnchorTypes;
    FReferencedTables: TdxList<TdxTable>;
    function GetCurrentCell: TdxTableCell;
    function GetCurrentTableDepth: Integer;
    procedure SetLastAddedParagraphIndex(const AValue: TdxParagraphIndex);
  protected
    procedure AddToGC(AObject: TObject);

    property PieceTable: TdxPieceTable read FPieceTable;
    property TableCells: TStack<TdxTableCell> read FTableCells;
    property TableLayoutUpdater: TdxDocTableLayoutUpdater read FTableLayoutUpdater;
    property CurrentCell: TdxTableCell read GetCurrentCell;
    property LastAddedParagraphIndex: TdxParagraphIndex read FLastAddedParagraphIndex write SetLastAddedParagraphIndex;
  public
    constructor Create(AImporter: TObject{TdxDocImporter}; APieceTable: TdxPieceTable);
    destructor Destroy; override;

    procedure ParagraphAdded(AParagraph: TdxParagraph; APropertyContainer: TdxDocPropertyContainer);
    function Finish: Boolean;
    procedure BeforeSectionAdded;
    procedure CellEndReached(APropertyContainer: TdxDocPropertyContainer);
    procedure ApplyCellProperties(APropertyContainer: TdxDocPropertyContainer; ACell: TdxTableCell);
    procedure ApplyCellPropertiesCore(ACell: TdxTableCell; AProperties: TdxTableCellProperties);
    procedure ApplyTableFloatingPosition(APos: TdxTableFloatingPosition);
    procedure RowEndReached(APropertyContainer: TdxDocPropertyContainer);
    function TryToUpdateCurrentTable(ACell: TdxTableCell; APropertyContainer: TdxDocPropertyContainer): TdxTableCell;
    procedure ApplyRowProperties(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
    procedure ApplyRowPropertiesCore(ARow: TdxTableRow; AProperties: TdxTableRowProperties);
    procedure ApplyTableInfo(ATable: TdxTable; ARow: TdxTableRow; AInfo: TdxDocTableInfo);
    procedure UpdateTableProperties(ATable: TdxTable; ARow: TdxTableRow; AProperties: TdxTableProperties);
    procedure ResetTableProperties(AProperties: TdxTableProperties);
    procedure CreateNestedTable(APropertyContainer: TdxDocPropertyContainer);
    procedure StartTable;
    function ShouldFinishTable(ATable: TdxTable; APropertyContainer: TdxDocPropertyContainer): Boolean;
    procedure FinishTable(ATable: TdxTable);

    property CurrentTableDepth: Integer read GetCurrentTableDepth;
  end;

implementation

uses
  Types, Math, Contnrs,
  dxRichEdit.Import.Doc.DocImporter,
  dxRichEdit.Import.Doc.BorderDescriptor,
  dxRichEdit.Doc.Utils;

type
  { TdxTableCellPropertiesUpdater }

  TdxTableCellPropertiesUpdater = class
  public
    class procedure UpdateTableRow(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer); static;
    class procedure ApplyCellBorders(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer); static;
    class procedure ApplyBorderColors(ARow: TdxTableRow; AColors: TdxDocTableBorderColorReferenceList; AType: TdxDocTableCellBorder; ATableInfo: TdxDocTableInfo; AUnitConverter: TdxDocumentModelUnitConverter); static;
    class procedure ApplyTableCellDescriptors(ARow: TdxTableRow; AType: TdxDocTableCellBorder; ATableInfo: TdxDocTableInfo; AUnitConverter: TdxDocumentModelUnitConverter); static;
    class function GetModelBorder(ABorders: TdxTableCellBorders; AType: TdxDocTableCellBorder): TdxBorderBase; static;
    class function GetBorderDescriptor(ACell: TdxTableCellDescriptor; AType: TdxDocTableCellBorder): TdxBorderDescriptor97; static;
    class procedure ApplyCellMargins(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer); static;
    class procedure ApplyOldCellBackgroundColors(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer); static;
    class procedure ApplyOldCellBackgroundColorsCore(ARow: TdxTableRow; AList: TdxAlphaColorList; AFirstCellIndex: Integer); static;
    class procedure ApplyCellBackgroundColors(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer); static;
  end;

{ TdxDocTablesImporter }

constructor TdxDocTablesImporter.Create(AImporter: TObject{TdxDocImporter}; APieceTable: TdxPieceTable);
begin
  inherited Create;
  FImporter := AImporter;
  FPieceTable := APieceTable;
  FTableCells := TStack<TdxTableCell>.Create;
  FReferencedTables := TdxList<TdxTable>.Create;
  FTableLayoutUpdater := TdxDocTableLayoutUpdater.Create(AImporter, APieceTable.DocumentModel.UnitConverter);
  FRows := TcxObjectList.Create;
end;

destructor TdxDocTablesImporter.Destroy;
begin
  while TableCells.Count > 0 do
    TableCells.Pop.Free;
  FreeAndNil(FRows);
  FreeAndNil(FTableCells);
  FreeAndNil(FReferencedTables);
  FreeAndNil(FTableLayoutUpdater);
  inherited Destroy;
end;

procedure TdxDocTablesImporter.AddToGC(AObject: TObject);
begin
  TdxDocImporter(FImporter).ContentBuilder.AddToGC(AObject);
end;

function TdxDocTablesImporter.GetCurrentCell: TdxTableCell;
begin
  if FTableCells.Count > 0 then
    Result := FTableCells.Peek
  else
    Result := nil;
end;

function TdxDocTablesImporter.GetCurrentTableDepth: Integer;
begin
  if (CurrentCell <> nil) then
    Result := CurrentCell.Table.NestedLevel + 1
  else
    Result := 0;
end;

procedure TdxDocTablesImporter.SetLastAddedParagraphIndex(const AValue: TdxParagraphIndex);
begin
  FLastAddedParagraphIndex := AValue;
end;

procedure TdxDocTablesImporter.ParagraphAdded(AParagraph: TdxParagraph; APropertyContainer: TdxDocPropertyContainer);
var
  ACell: TdxTableCell;
begin
  LastAddedParagraphIndex := AParagraph.Index;
  if APropertyContainer.ParagraphInfo = nil then
    Exit;
  if APropertyContainer.ParagraphInfo.TableDepth = CurrentTableDepth then
    Exit;
  if APropertyContainer.ParagraphInfo.TableDepth > CurrentTableDepth then
    CreateNestedTable(APropertyContainer)
  else
  begin
    ACell := TableCells.Pop;
    FinishTable(ACell.Table);
    ACell.Free;
  end;
end;

function TdxDocTablesImporter.Finish: Boolean;
var
  ACell: TdxTableCell;
begin
  if TableCells.Count = 0 then
    Exit(False);
  while TableCells.Count > 0 do
  begin
    ACell := TableCells.Pop;
    FinishTable(ACell.Table);
    ACell.Free;
  end;
  Result := True;
end;

procedure TdxDocTablesImporter.BeforeSectionAdded;
var
  ACell: TdxTableCell;
begin
  while TableCells.Count > 0 do
  begin
    ACell := TableCells.Pop;
    if ACell.Row.Cells.Count > 0 then
      TdxDocImporter.ThrowInvalidDocFile;
    FinishTable(ACell.Table);
    ACell.Free;
  end;
end;

procedure TdxDocTablesImporter.CellEndReached(APropertyContainer: TdxDocPropertyContainer);
var
  ACell, ANextCell: TdxTableCell;
begin
  if CurrentTableDepth = 0 then
    Exit;
  ACell := TableCells.Pop;
  ApplyCellProperties(APropertyContainer, ACell);
  PieceTable.TableCellsManager.InitializeTableCell(ACell, ACell.StartParagraphIndex, ACell.EndParagraphIndex);
  ACell.Row.Cells.AddInternal(ACell);

  ANextCell := TdxTableCell.Create(ACell.Row);
  TableCells.Push(ANextCell);
  ANextCell.StartParagraphIndex := LastAddedParagraphIndex + 1;
end;

procedure TdxDocTablesImporter.ApplyCellProperties(APropertyContainer: TdxDocPropertyContainer; ACell: TdxTableCell);
begin
  if APropertyContainer.TableCellInfo <> nil then
    ApplyCellPropertiesCore(ACell, APropertyContainer.TableCellInfo.TableCellProperties);
  ACell.EndParagraphIndex := LastAddedParagraphIndex;
  if APropertyContainer.TableInfo <> nil then
    ApplyTableFloatingPosition(APropertyContainer.TableInfo.TableProperties.FloatingPosition);
end;

procedure TdxDocTablesImporter.ApplyCellPropertiesCore(ACell: TdxTableCell; AProperties: TdxTableCellProperties);
begin
  ACell.Properties.BeginInit;
  ACell.Properties.CopyFrom(AProperties);
  ACell.Properties.EndInit;
end;

procedure TdxDocTablesImporter.ApplyTableFloatingPosition(APos: TdxTableFloatingPosition);
begin
  FHorizontalAnchor := APos.HorizontalAnchor;
  FVerticalAnchor := APos.VerticalAnchor;
end;

procedure TdxDocTablesImporter.RowEndReached(APropertyContainer: TdxDocPropertyContainer);
var
  ACell, ANextCell: TdxTableCell;
  ARow, ANextRow: TdxTableRow;
begin
  if CurrentTableDepth = 0 then
    Exit;
  ACell := TableCells.Pop;
  ARow := ACell.Row;
  ACell := TryToUpdateCurrentTable(ACell, APropertyContainer);
  if ACell.Row.Cells.IndexOf(ACell) = -1 then
    ACell.Free;

  ApplyRowProperties(ARow, APropertyContainer);
  TableLayoutUpdater.PerformRowActions(ARow, APropertyContainer);
  ARow.Table.Rows.AddInternal(ARow);
  FRows.Extract(ARow);

  ANextRow := TdxTableRow.Create(ARow.Table);
  FRows.Add(ANextRow);
  ANextCell := TdxTableCell.Create(ANextRow);
  ANextCell.StartParagraphIndex := LastAddedParagraphIndex + 1;
  TableCells.Push(ANextCell);
end;

function TdxDocTablesImporter.TryToUpdateCurrentTable(ACell: TdxTableCell; APropertyContainer: TdxDocPropertyContainer): TdxTableCell;
var
  ARow: TdxTableRow;
begin
  ARow := ACell.Row;
  if ShouldFinishTable(ACell.Table, APropertyContainer) then
  begin
    FinishTable(ACell.Table);
    CreateNestedTable(APropertyContainer);
    ACell.Free;
    ACell := TableCells.Pop;
    ARow.SetTable(ACell.Table);
  end;
  Result := ACell;
end;

procedure TdxDocTablesImporter.ApplyRowProperties(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
var
  ATable: TdxTable;
begin
  ATable := ARow.Table;
  if APropertyContainer.TableRowInfo <> nil then
    ApplyRowPropertiesCore(ARow, APropertyContainer.TableRowInfo.TableRowProperties);
  if APropertyContainer.TableInfo <> nil then
    ApplyTableInfo(ATable, ARow, APropertyContainer.TableInfo);
end;

procedure TdxDocTablesImporter.ApplyRowPropertiesCore(ARow: TdxTableRow; AProperties: TdxTableRowProperties);
begin
  ARow.Properties.BeginInit;
  ARow.Properties.CopyFrom(AProperties);
  ARow.Properties.EndInit;
end;

procedure TdxDocTablesImporter.ApplyTableInfo(ATable: TdxTable; ARow: TdxTableRow; AInfo: TdxDocTableInfo);
begin
  UpdateTableProperties(ATable, ARow, AInfo.TableProperties);
  if AInfo.TableStyleIndex >= 0 then
    ATable.StyleIndex := AInfo.TableStyleIndex
  else
    ResetTableProperties(ATable.TableProperties);
end;

procedure TdxDocTablesImporter.UpdateTableProperties(ATable: TdxTable; ARow: TdxTableRow; AProperties: TdxTableProperties);
begin
  if AProperties.Info.Value = TdxTablePropertiesOptions.MaskUseNone then
    Exit;
  if ATable.Rows.Count = 0 then
  begin
    ATable.TableProperties.BeginUpdate;
    ATable.TableProperties.CopyFrom(AProperties);
    ATable.TableProperties.EndUpdate;
  end
  else
    if not ATable.TableProperties.AreSame(AProperties) then
    begin
      ARow.TablePropertiesException.BeginUpdate;
      ARow.TablePropertiesException.CopyFrom(AProperties);
      ARow.TablePropertiesException.EndUpdate;
    end;
end;

procedure TdxDocTablesImporter.ResetTableProperties(AProperties: TdxTableProperties);
begin
  AProperties.BeginUpdate;
  if not AProperties.Info.UseLeftMargin then
  begin
    AProperties.CellMargins.Left.Value := 0;
    AProperties.CellMargins.Left.&Type := TdxWidthUnitType.ModelUnits;
  end;
  if not AProperties.Info.UseRightMargin then
  begin
    AProperties.CellMargins.Right.Value := 0;
    AProperties.CellMargins.Right.&Type := TdxWidthUnitType.ModelUnits;
  end;
  if not AProperties.Info.UseTopMargin then
  begin
    AProperties.CellMargins.Top.Value := 0;
    AProperties.CellMargins.Top.&Type := TdxWidthUnitType.ModelUnits;
  end;
  if not AProperties.Info.UseBottomMargin then
  begin
    AProperties.CellMargins.Bottom.Value := 0;
    AProperties.CellMargins.Bottom.&Type := TdxWidthUnitType.ModelUnits;
  end;
  AProperties.EndUpdate;
end;

procedure TdxDocTablesImporter.CreateNestedTable(APropertyContainer: TdxDocPropertyContainer);
var
  ACount, I: Integer;
begin
  if APropertyContainer.ParagraphInfo = nil then
    Exit;
  ACount := APropertyContainer.ParagraphInfo.TableDepth - CurrentTableDepth;
  for I := 0 to ACount - 1 do
    StartTable;
end;

procedure TdxDocTablesImporter.StartTable;
var
  ATable: TdxTable;
  ACell: TdxTableCell;
  ARow: TdxTableRow;
begin
  ATable := TdxTable.Create(PieceTable, CurrentCell, 0, 0);
  PieceTable.Tables.Add(ATable);
  ARow := TdxTableRow.Create(ATable);
  FRows.Add(ARow);
  ACell := TdxTableCell.Create(ARow);
  ACell.StartParagraphIndex := LastAddedParagraphIndex;
  TableCells.Push(ACell);
end;

function TdxDocTablesImporter.ShouldFinishTable(ATable: TdxTable; APropertyContainer: TdxDocPropertyContainer): Boolean;
var
  ACurrentPosition, ANextPosition: TdxTableFloatingPosition;
begin
  if ATable.Rows.Count = 0 then
    Exit(False);
  if APropertyContainer.TableInfo = nil then
    Exit(True);
  ACurrentPosition := ATable.TableProperties.FloatingPosition;
  ANextPosition := APropertyContainer.TableInfo.TableProperties.FloatingPosition;
  Result := ((ATable.StyleIndex <> APropertyContainer.TableInfo.TableStyleIndex) and
    (APropertyContainer.TableInfo.TableStyleIndex <> -1)) or
    not ACurrentPosition.CompareTo(ANextPosition);
end;

procedure TdxDocTablesImporter.FinishTable(ATable: TdxTable);
begin
  if (ATable.Rows.Count = 0) or (ATable.Rows.First.Cells.Count = 0) then
  begin
    if FReferencedTables.Contains(ATable) then
      TdxDocImporter.ThrowInvalidDocFile;
    PieceTable.Tables.Remove(ATable.Index);
  end
  else
  begin
    TableLayoutUpdater.UpdateLayout(ATable);
    ATable.NormalizeTableGrid;
    if ATable.ParentCell <> nil then
      FReferencedTables.Add(ATable.ParentCell.Table);
  end;
end;

{ TdxRowLeftAndCellWidths }

constructor TdxRowLeftAndCellWidths.Create;
begin
  inherited Create;
  FCellWidths := TdxIntegerList.Create;
end;

destructor TdxRowLeftAndCellWidths.Destroy;
begin
  FreeAndNil(FCellWidths);
  inherited Destroy;
end;

{ TdxDocTableLayoutUpdater }

constructor TdxDocTableLayoutUpdater.Create(AImporter: TObject{TdxDocImporter}; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  inherited Create;
  FUnitConverter := AUnitConverter;
  FVerticalMergingUpdater := TdxVerticalMergingStateUpdater.Create;
  FTableCellPositions := TObjectDictionary<TdxTable, TdxIntegerList>.Create([doOwnsValues]);
  FRowCellWidths := TObjectDictionary<TdxTableRow, TdxRowLeftAndCellWidths>.Create([{doOwnsValues}]);
  FPreferredCellWidths := TObjectDictionary<TdxTableRow, TdxTableCellWidthOperandList>.Create([{doOwnsValues}]);
  FImporter := AImporter;
end;

destructor TdxDocTableLayoutUpdater.Destroy;
begin
  FreeAndNil(FVerticalMergingUpdater);
  FreeAndNil(FTableCellPositions);
  FreeAndNil(FRowCellWidths);
  FreeAndNil(FPreferredCellWidths);
  inherited Destroy;
end;

procedure TdxDocTableLayoutUpdater.PerformRowActions(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
begin
  AddPreferredCellWidths(ARow, APropertyContainer);
  AddRowCellPositions(ARow, APropertyContainer);
  TdxTableCellPropertiesUpdater.UpdateTableRow(ARow, APropertyContainer);
  VerticalMergingUpdater.UpdateCellsVerticalMergeState(ARow, APropertyContainer);
  ApplyTablePropertiesException(ARow, APropertyContainer.TableRowInfo);
end;

procedure TdxDocTableLayoutUpdater.AddPreferredCellWidths(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
var
  ATableCellWidths: TdxTableCellWidthOperandList;
begin
  if APropertyContainer.TableRowInfo = nil then
    Exit;
  ATableCellWidths := APropertyContainer.TableRowInfo.PreferredCellWidths;
  if ATableCellWidths.Count > 0 then
    PreferredCellWidths.Add(ARow, ATableCellWidths);
end;

procedure TdxDocTableLayoutUpdater.AddRowCellPositions(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
var
  ARowCellSizeAndPosition: TdxRowLeftAndCellWidths;
  ATableCellPositions: TdxIntegerList;
begin
  ARowCellSizeAndPosition := TdxRowLeftAndCellWidths.Create;
  ATableCellPositions := GetTableCellPositions(ARow.Table);

  ProcessTableDefinition(ARow, APropertyContainer, ARowCellSizeAndPosition);
  ApplyInsertActions(APropertyContainer, ARowCellSizeAndPosition.CellWidths);
  ApplyColumnWidthsActions(APropertyContainer, ARowCellSizeAndPosition.CellWidths);
  RowCellWidths.Add(ARow, ARowCellSizeAndPosition);
  AddTableCellPositions(ARow, ARowCellSizeAndPosition, ATableCellPositions, APropertyContainer);
end;

function TdxDocTableLayoutUpdater.GetTableCellPositions(ATable: TdxTable): TdxIntegerList;
begin
  if not TableCellPositions.TryGetValue(ATable, Result) then
  begin
    Result := TdxIntegerList.Create;
    TableCellPositions.Add(ATable, Result);
  end;
end;

procedure TdxDocTableLayoutUpdater.ProcessTableDefinition(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer; ARowCellSizeAndPosition: TdxRowLeftAndCellWidths);
var
  AOperand: TdxTableDefinitionOperand;
  ACount, I, ACurrentWidth, AFirstMergedCell, ASpan: Integer;
  ACells: TdxTableCellDescriptorList;
begin
  AOperand := APropertyContainer.TableInfo.TableDefinition;
  ACount := AOperand.ColumnsCount;
  if ACount > 0 then
    ARowCellSizeAndPosition.RowLeft := AOperand.Positions[0];
  for I := 0 to ACount - 1 do
  begin
    ACurrentWidth := AOperand.Positions[I + 1] - AOperand.Positions[I];
    if ACurrentWidth >= 0 then
      ARowCellSizeAndPosition.CellWidths.Add(ACurrentWidth);
  end;
  ACells := APropertyContainer.TableInfo.TableDefinition.Cells;
  AFirstMergedCell := 0;
  for I := 0 to ACells.Count - 1 do
  begin
    if ACells[I].HorizontalMerging <> TdxMergingState.Continue then
    begin
      ASpan := I - AFirstMergedCell;
      if ASpan > 1 then
        TdxDocImporter(FImporter).AddCellsHorizontalMerging(ARow, AFirstMergedCell, ASpan);
      AFirstMergedCell := I;
    end;
  end;
  ASpan := ACells.Count - AFirstMergedCell;
  if ASpan > 1 then
    TdxDocImporter(FImporter).AddCellsHorizontalMerging(ARow, AFirstMergedCell, ASpan);
end;

procedure TdxDocTableLayoutUpdater.ApplyInsertActions(APropertyContainer: TdxDocPropertyContainer; ARowCellWidths: TdxIntegerList);
var
  AInsertionsCount, AInsertionIndex, AColumnIndex: Integer;
  ACurrentInsertion: TdxInsertOperand;
  ACount: Byte;
begin
  if APropertyContainer.TableRowInfo = nil then
    Exit;
  AInsertionsCount := APropertyContainer.TableRowInfo.InsertActions.Count;
  for AInsertionIndex := 0 to AInsertionsCount - 1 do
  begin
    ACurrentInsertion := APropertyContainer.TableRowInfo.InsertActions[AInsertionIndex];
    ACount := ACurrentInsertion.Count;
    for AColumnIndex := 0 to ACount - 1 do
      ARowCellWidths.Insert(ACurrentInsertion.StartIndex + AColumnIndex, ACurrentInsertion.WidthInTwips);
  end;
end;

procedure TdxDocTableLayoutUpdater.ApplyColumnWidthsActions(APropertyContainer: TdxDocPropertyContainer; ARowCellWidths: TdxIntegerList);
var
  ACount, AWidthActionIndex, AColumnIndex: Integer;
  ACurrentWidthAction: TdxColumnWidthOperand;
begin
  if APropertyContainer.TableRowInfo = nil then
    Exit;
  ACount := APropertyContainer.TableRowInfo.ColumnWidthActions.Count;
  for AWidthActionIndex := 0 to ACount - 1 do
  begin
    ACurrentWidthAction := APropertyContainer.TableRowInfo.ColumnWidthActions[AWidthActionIndex];
    for AColumnIndex := ACurrentWidthAction.StartIndex to ACurrentWidthAction.EndIndex do
      ARowCellWidths[AColumnIndex] := ACurrentWidthAction.WidthInTwips;
  end;
end;

procedure TdxDocTableLayoutUpdater.AddTableCellPositions(ARow: TdxTableRow; ARowLeftAndCellsWidth: TdxRowLeftAndCellWidths; ATableCellPositions: TdxIntegerList; APropertyContainer: TdxDocPropertyContainer);
var
  ARowCellWidths: TdxIntegerList;
  ACurrentPosition, ACount, I: Integer;
begin
  ARowCellWidths := ARowLeftAndCellsWidth.CellWidths;

  ACurrentPosition := GetRowLeft(ARow, APropertyContainer);
  ARowLeftAndCellsWidth.RowLeft := ACurrentPosition;
  TryToAddCellPosition(ATableCellPositions, ACurrentPosition);

  ACount := ARowCellWidths.Count;
  for I := 0 to ACount - 1 do
  begin
    Inc(ACurrentPosition, ARowCellWidths[I]);
    TryToAddCellPosition(ATableCellPositions, ACurrentPosition);
  end;

  if ARow.Properties.WidthAfter.Value > 0 then
  begin
    Inc(ACurrentPosition, ARow.Properties.WidthAfter.Value);
    TryToAddCellPosition(ATableCellPositions, ACurrentPosition);
  end;
end;

function TdxDocTableLayoutUpdater.GetRowLeft(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer): Integer;
var
  ARowInfo: TdxTableRowInfo;
  ARowLeft, ALeft: Integer;
  AIndent: TdxTableIndent;
begin
  ARowInfo := APropertyContainer.TableRowInfo;
  if (ARowInfo <> nil) and (ARowInfo.RowLeftSetted or ARowInfo.RowLeftOffsetSetted) then
  begin
    ARowLeft := 0;
    if ARowInfo.RowLeftSetted then
      ARowLeft := ARowInfo.RowLeft;
    if ARowInfo.RowLeftOffsetSetted then
      Dec(ARowLeft, ARowInfo.RowLeftOffset);
    Exit(ARowLeft);
  end;

  ALeft := ARow.Properties.WidthBefore.Value;
  AIndent := ARow.Table.TableIndent;
  if AIndent.&Type = TdxWidthUnitType.ModelUnits then
    Inc(ALeft, AIndent.Value);
  Result := ALeft;
end;

procedure TdxDocTableLayoutUpdater.TryToAddCellPosition(ADestination: TdxIntegerList; APosition: Integer);
var
  APositionIndex: Integer;
begin
  if ADestination.BinarySearch(APosition, APositionIndex) then
    Exit;
  ADestination.Insert(APositionIndex, APosition);
end;

procedure TdxDocTableLayoutUpdater.UpdateLayout(ATable: TdxTable);
begin
  ApplySpans(ATable);
  VerticalMergingUpdater.ApplyVerticalMerging(ATable);
end;

procedure TdxDocTableLayoutUpdater.ApplySpans(ATable: TdxTable);
var
  ACellPositions: TdxIntegerList;
  ANeedReleaseList: Boolean;
  ARowCount, AMinRowLeft, ARowIndex: Integer;
  ARowLeftAndCellsWidthCollection: TdxObjectList<TdxRowLeftAndCellWidths>;
  ACurrentRow: TdxTableRow;
  APreferredCellWidths: TdxTableCellWidthOperandList;
begin
  if not TableCellPositions.TryGetValue(ATable, ACellPositions) then
  begin
    ACellPositions := TdxIntegerList.Create;
    ANeedReleaseList := True;
  end
  else
    ANeedReleaseList := False;
  try
    ARowCount := ATable.Rows.Count;
    ARowLeftAndCellsWidthCollection := TdxObjectList<TdxRowLeftAndCellWidths>.Create;
    try
      AMinRowLeft := CalcMinRowLeft(ATable, ARowLeftAndCellsWidthCollection);
      for ARowIndex := 0 to ARowCount - 1 do
      begin
        ACurrentRow := ATable.Rows[ARowIndex];
        CorrectCellWidthsAndSpans(ACurrentRow, ACellPositions, ARowLeftAndCellsWidthCollection[ARowIndex], AMinRowLeft);
        if PreferredCellWidths.TryGetValue(ACurrentRow, APreferredCellWidths) then
          ApplyPreferredWidths(ACurrentRow, APreferredCellWidths);
      end;
    finally
      ARowLeftAndCellsWidthCollection.Free;
    end;
  finally
    if ANeedReleaseList then
      ACellPositions.Free;
  end;
end;

function TdxDocTableLayoutUpdater.CalcMinRowLeft(ATable: TdxTable;
  ARowLeftAndCellsWidthCollection: TdxObjectList<TdxRowLeftAndCellWidths>): Integer;
var
  ACount, I: Integer;
  ARows: TdxTableRowCollection;
  ACurrentRow: TdxTableRow;
  ARowLeftAndCellsWidth: TdxRowLeftAndCellWidths;
begin
  Result := MaxInt;
  ARows := ATable.Rows;
  ACount := ARows.Count;
  for I := 0 to ACount - 1 do
  begin
    ACurrentRow := ARows[I];
    if not RowCellWidths.TryGetValue(ACurrentRow, ARowLeftAndCellsWidth) then
      ARowLeftAndCellsWidth := TdxRowLeftAndCellWidths.Create;

    Result := Math.Min(Result, ARowLeftAndCellsWidth.RowLeft);
    ARowLeftAndCellsWidthCollection.Add(ARowLeftAndCellsWidth);
  end;
end;

procedure TdxDocTableLayoutUpdater.CorrectCellWidthsAndSpans(ARow: TdxTableRow; ACellPositions: TdxIntegerList; ARowLeftAndCellsWidth: TdxRowLeftAndCellWidths; AMinRowLeft: Integer);
var
  ASpan, ACurrentPosition: Integer;
begin
  ARow.UnsubscribeRowPropertiesEvents;
  try
    ASpan := CalcColumnSpan(ACellPositions, AMinRowLeft, ARowLeftAndCellsWidth.RowLeft - AMinRowLeft);
    if ASpan <> ARow.GridBefore then
      ARow.GridBefore := ASpan;
    ACurrentPosition := ARowLeftAndCellsWidth.RowLeft;
    ACurrentPosition := ProcessRowCells(ARow, ACellPositions, ARowLeftAndCellsWidth.CellWidths, ACurrentPosition);

    ASpan := CalcColumnSpan(ACellPositions, ACurrentPosition, ACellPositions[ACellPositions.Count - 1] - ACurrentPosition);
    if ASpan <> ARow.GridAfter then
      ARow.GridAfter := ASpan;
  finally
    ARow.SubscribeRowPropertiesEvents;
  end;
end;

function TdxDocTableLayoutUpdater.ProcessRowCells(ARow: TdxTableRow; ACellPositions: TdxIntegerList; ACellWidths: TdxIntegerList; ACurrentPosition: Integer): Integer;
var
  ACount: Integer;
begin
  ACount := ARow.Cells.Count;
  if ACount = ACellWidths.Count then
    Result := ApplyActualColumnSpans(ARow, ACellPositions, ACellWidths, ACurrentPosition)
  else
    Result := ApplyDefaultColumnSpans(ARow, ACellPositions, ACellWidths, ACurrentPosition);
end;

function TdxDocTableLayoutUpdater.ApplyActualColumnSpans(ARow: TdxTableRow; ACellPositions: TdxIntegerList; ACellWidths: TdxIntegerList; ACurrentPosition: Integer): Integer;
var
  ACount, ACellIndex, ACurrentWidth: Integer;
  ACurrentCell: TdxTableCell;
begin
  ACount := ACellWidths.Count;
  for ACellIndex := 0 to ACount - 1 do
  begin
    ACurrentWidth := ACellWidths[ACellIndex];
    ACurrentCell := ARow.Cells[ACellIndex];
    ACurrentCell.UnsubscribeCellPropertiesEvents;
    try
      ACurrentCell.ColumnSpan := Math.Max(1, CalcColumnSpan(ACellPositions, ACurrentPosition, ACurrentWidth));
      ACurrentCell.Properties.PreferredWidth.Value := UnitConverter.TwipsToModelUnits(ACurrentWidth);
      ACurrentCell.Properties.PreferredWidth.&Type := TdxWidthUnitType.ModelUnits;
    finally
      ACurrentCell.SubscribeCellPropertiesEvents;
    end;
    Inc(ACurrentPosition, ACurrentWidth);
  end;
  Result := ACurrentPosition;
end;

function TdxDocTableLayoutUpdater.ApplyDefaultColumnSpans(ARow: TdxTableRow; ACellPositions: TdxIntegerList; ACellWidths: TdxIntegerList; ACurrentPosition: Integer): Integer;
var
  ACells: TdxTableCellCollection;
  ACount, I : Integer;
  ACurrentCell: TdxTableCell;
begin
  ACells := ARow.Cells;
  ACount := ACells.Count;
  for I := 0 to ACount - 1 do
  begin
    ACurrentCell := ACells[I];
    ACurrentCell.UnsubscribeCellPropertiesEvents;
    try
      ACurrentCell.ColumnSpan := 1;
      ACurrentCell.Properties.PreferredWidth.&Type := TdxWidthUnitType.Auto;
      ACurrentCell.Properties.PreferredWidth.Value := 0;
    finally
      ACurrentCell.SubscribeCellPropertiesEvents;
    end;
  end;
  ACount := ACellWidths.Count;
  for I := 0 to ACount - 1 do
    Inc(ACurrentPosition, ACellWidths[I]);

  Result := ACurrentPosition;
end;

function TdxDocTableLayoutUpdater.CalcColumnSpan(ACellPositions: TdxIntegerList; ACurrentPosition: Integer; ACurrentWidth: Integer): Integer;
var
  AStartPositionIndex, AEndPositionIndex: Integer;
begin
  if not ACellPositions.BinarySearch(ACurrentPosition, AStartPositionIndex) then
    AStartPositionIndex := not AStartPositionIndex;
  if not ACellPositions.BinarySearch(ACurrentPosition + ACurrentWidth, AEndPositionIndex) then
    AEndPositionIndex := not AEndPositionIndex;
  Result := AEndPositionIndex - AStartPositionIndex;
end;

procedure TdxDocTableLayoutUpdater.ApplyPreferredWidths(ARow: TdxTableRow; APreferredCellWidths: TdxTableCellWidthOperandList);
var
  ACount, I: Integer;
begin
  ACount := APreferredCellWidths.Count;
  for I := 0 to ACount - 1 do
    ApplyPreferredWidthsCore(ARow, APreferredCellWidths[I]);
end;

procedure TdxDocTableLayoutUpdater.ApplyPreferredWidthsCore(ARow: TdxTableRow; AOperand: TdxTableCellWidthOperand);
var
  AStart, AEnd, I: Integer;
  APreferredWidth: TdxPreferredWidth;
begin
  AStart := Max(0, Integer(AOperand.StartIndex));
  AEnd := Min(ARow.Cells.Count - 1, AOperand.EndIndex);
  for I := AStart to AEnd do
  begin
    APreferredWidth := ARow.Cells[I].Properties.PreferredWidth;
    APreferredWidth.&Type := AOperand.WidthUnit.&Type;
    APreferredWidth.Value := AOperand.WidthUnit.Value;
  end;
end;

procedure TdxDocTableLayoutUpdater.ApplyTablePropertiesException(ARow: TdxTableRow; AInfo: TdxTableRowInfo);
begin
  if (AInfo = nil) or (AInfo.TableAutoformatLookSpecifier = nil) then
    Exit;
  AInfo.TableAutoformatLookSpecifier.ApplyProperties(ARow.TablePropertiesException);
end;

{ TdxVerticalMergingStateUpdater }

constructor TdxVerticalMergingStateUpdater.Create;
begin
  inherited Create;
  FVerticalMergedCells := TObjectDictionary<TdxTableRow, TdxVerticalMergeInfoList>.Create([doOwnsValues]);
end;

destructor TdxVerticalMergingStateUpdater.Destroy;
begin
  FreeAndNil(FVerticalMergedCells);
  inherited Destroy;
end;

procedure TdxVerticalMergingStateUpdater.UpdateCellsVerticalMergeState(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
begin
  AddVerticalMergedCells(ARow, APropertyContainer);
  ApplyCellVerticalAlignmentActions(ARow, APropertyContainer);
end;

procedure TdxVerticalMergingStateUpdater.ApplyVerticalMerging(ATable: TdxTable);
var
  ARowCount, ARowIndex, AMergedCellsCount, AInfoIndex: Integer;
  ACurrentRow: TdxTableRow;
  AVerticalMergeInfo: TdxVerticalMergeInfoList;
  ACurrentInfo: TdxVerticalMergeInfo;
begin
  ARowCount := ATable.Rows.Count;
  for ARowIndex := 0 to ARowCount - 1 do
  begin
    ACurrentRow := ATable.Rows[ARowIndex];
    if not VerticalMergedCells.TryGetValue(ACurrentRow, AVerticalMergeInfo) then
      Continue;
    AMergedCellsCount := AVerticalMergeInfo.Count;
    for AInfoIndex := 0 to AMergedCellsCount - 1 do
    begin
      ACurrentInfo := AVerticalMergeInfo[AInfoIndex];
      if ACurrentInfo.CellIndex >= ACurrentRow.Cells.Count then
        Continue;
      ACurrentRow.Cells[ACurrentInfo.CellIndex].VerticalMerging := ACurrentInfo.MergingState;
    end;
  end;
end;

procedure TdxVerticalMergingStateUpdater.AddVerticalMergedCells(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
var
  ALegacyMergeInfo, AMergeInfo: TdxVerticalMergeInfoList;
  ACount, I: Integer;
begin
  if APropertyContainer.TableRowInfo = nil then
    Exit;
  ALegacyMergeInfo := GetVerticalMergingInfo(ARow);
  AMergeInfo := APropertyContainer.TableRowInfo.VerticalMerging;
  ACount := AMergeInfo.Count;
  for I := 0 to ACount - 1 do
    ALegacyMergeInfo.Add(AMergeInfo[I]);
end;

procedure TdxVerticalMergingStateUpdater.ApplyCellVerticalAlignmentActions(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
var
  ACount, I, ACellIndex: Integer;
  ACurrentVerticalAlignmentAction: TdxCellRangeVerticalAlignmentOperand;
begin
  if APropertyContainer.TableRowInfo = nil then
    Exit;
  ACount := APropertyContainer.TableRowInfo.CellRangeVerticalAlignmentActions.Count;
  for I := 0 to ACount - 1 do
  begin
    ACurrentVerticalAlignmentAction := APropertyContainer.TableRowInfo.CellRangeVerticalAlignmentActions[I];
    for ACellIndex := ACurrentVerticalAlignmentAction.StartIndex to ACurrentVerticalAlignmentAction.EndIndex do
      ARow.Cells[ACellIndex].Properties.VerticalAlignment := ACurrentVerticalAlignmentAction.VerticalAlignment;
  end;
end;

function TdxVerticalMergingStateUpdater.GetVerticalMergingInfo(ARow: TdxTableRow): TdxVerticalMergeInfoList;
begin
  if not VerticalMergedCells.TryGetValue(ARow, Result) then
  begin
    Result := TdxVerticalMergeInfoList.Create(False);
    VerticalMergedCells.Add(ARow, Result);
  end;
end;

{ TdxTableCellPropertiesUpdater }

class procedure TdxTableCellPropertiesUpdater.UpdateTableRow(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
begin
  ApplyCellMargins(ARow, APropertyContainer);
  ApplyOldCellBackgroundColors(ARow, APropertyContainer);
  ApplyCellBackgroundColors(ARow, APropertyContainer);
  ApplyCellBorders(ARow, APropertyContainer);
end;

class procedure TdxTableCellPropertiesUpdater.ApplyCellBorders(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
var
  ARowInfo: TdxTableRowInfo;
  ATableInfo: TdxDocTableInfo;
  ACount, I, AStart, AEnd, ACellIndex: Integer;
  AOperand: TdxTableBordersOverrideOperand;
begin
  ARowInfo := APropertyContainer.TableRowInfo;
  ATableInfo := APropertyContainer.TableInfo;
  if ARowInfo = nil then
    Exit;
  ACount := APropertyContainer.TableRowInfo.OverrideCellBordersActions.Count;
  for I := 0 to ACount - 1 do
  begin
    AOperand := APropertyContainer.TableRowInfo.OverrideCellBordersActions[I];
    AStart := Math.Max(AOperand.StartIndex, 0);
    AEnd := Math.Min(AOperand.EndIndex, ARow.Cells.Count - 1);
    for ACellIndex := AStart to AEnd do
      AOperand.ApplyProperties(ARow.Cells[ACellIndex].Properties.Borders, APropertyContainer.UnitConverter);
  end;
  if ACount > 0 then
    Exit;
  ApplyBorderColors(ARow, ARowInfo.TopBorders, TdxDocTableCellBorder.Top, ATableInfo, APropertyContainer.UnitConverter);
  ApplyBorderColors(ARow, ARowInfo.LeftBorders, TdxDocTableCellBorder.Left, ATableInfo, APropertyContainer.UnitConverter);
  ApplyBorderColors(ARow, ARowInfo.RightBorders, TdxDocTableCellBorder.Right, ATableInfo, APropertyContainer.UnitConverter);
  ApplyBorderColors(ARow, ARowInfo.BottomBorders, TdxDocTableCellBorder.Bottom, ATableInfo, APropertyContainer.UnitConverter);

  ApplyTableCellDescriptors(ARow, TdxDocTableCellBorder.Top, ATableInfo, APropertyContainer.UnitConverter);
  ApplyTableCellDescriptors(ARow, TdxDocTableCellBorder.Left, ATableInfo, APropertyContainer.UnitConverter);
  ApplyTableCellDescriptors(ARow, TdxDocTableCellBorder.Right, ATableInfo, APropertyContainer.UnitConverter);
  ApplyTableCellDescriptors(ARow, TdxDocTableCellBorder.Bottom, ATableInfo, APropertyContainer.UnitConverter);
end;

class procedure TdxTableCellPropertiesUpdater.ApplyBorderColors(ARow: TdxTableRow;
  AColors: TdxDocTableBorderColorReferenceList; AType: TdxDocTableCellBorder;
  ATableInfo: TdxDocTableInfo; AUnitConverter: TdxDocumentModelUnitConverter);
var
  ACount, I: Integer;
  ABorder: TdxBorderBase;
begin
  ACount := Math.Min(AColors.Count, ARow.Cells.Count);
  for I := 0 to ACount - 1 do
  begin
    ABorder := GetModelBorder(ARow.Cells[I].Properties.Borders, AType);
    ABorder.BeginUpdate;
    try
      if TdxAlphaColors.IsEmpty(AColors[I].Color) then
        Continue;
      ABorder.Color := AColors[I].Color;
    finally
      ABorder.EndUpdate;
    end;
  end;
end;

class procedure TdxTableCellPropertiesUpdater.ApplyTableCellDescriptors(ARow: TdxTableRow;
  AType: TdxDocTableCellBorder; ATableInfo: TdxDocTableInfo; AUnitConverter: TdxDocumentModelUnitConverter);
var
  ACells: TdxTableCellDescriptorList;
  I: Integer;
  ABorder: TdxBorderBase;
  ADescriptor: TdxBorderDescriptor97;
begin
  if (ATableInfo = nil) or (ATableInfo.TableDefinition = nil) then
    Exit;
  ACells := ATableInfo.TableDefinition.Cells;
  if (ACells = nil) or (ACells.Count = 0) then
    Exit;
  for I := 0 to ACells.Count - 1 do
  begin
    ABorder := GetModelBorder(ARow.Cells[I].Properties.Borders, AType);
    ABorder.BeginUpdate;
    try
      ADescriptor := GetBorderDescriptor(ACells[I], AType);
      if (ADescriptor <> nil) and not ADescriptor.NotUseBorder then
      begin
        ABorder.Style := TdxDocBorderCalculator.MapToBorderLineStyle(ADescriptor.Style);
        ABorder.Width := AUnitConverter.TwipsToModelUnits(ADescriptor.Width);

        ABorder.Color := ADescriptor.BorderColor;
      end;
    finally
      ABorder.EndUpdate;
    end;
  end;
end;

class function TdxTableCellPropertiesUpdater.GetModelBorder(ABorders: TdxTableCellBorders;
  AType: TdxDocTableCellBorder): TdxBorderBase;
begin
  case AType of
    TdxDocTableCellBorder.Top:
      Result := ABorders.TopBorder;
    TdxDocTableCellBorder.Left:
      Result := ABorders.LeftBorder;
    TdxDocTableCellBorder.Bottom:
      Result := ABorders.BottomBorder;
    TdxDocTableCellBorder.Right:
      Result := ABorders.RightBorder;
    TdxDocTableCellBorder.TopLeftToBottomRight:
      Result := ABorders.TopLeftDiagonalBorder;
    TdxDocTableCellBorder.TopRightToBottomLeft:
      Result := ABorders.TopRightDiagonalBorder;
    else
      Exit(nil);
  end;
end;

class function TdxTableCellPropertiesUpdater.GetBorderDescriptor(ACell: TdxTableCellDescriptor;
  AType: TdxDocTableCellBorder): TdxBorderDescriptor97;
begin
  case AType of
    TdxDocTableCellBorder.Top:
      Result := ACell.TopBorder;
    TdxDocTableCellBorder.Left:
      Result := ACell.LeftBorder;
    TdxDocTableCellBorder.Bottom:
      Result := ACell.BottomBorder;
    TdxDocTableCellBorder.Right:
      Result := ACell.RightBorder;
    else
      Result := nil;
  end;
end;

class procedure TdxTableCellPropertiesUpdater.ApplyCellMargins(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
var
  ACount, I, AStart, AEnd, ACellIndex: Integer;
  AOperand: TdxCellSpacingOperand;
begin
  if APropertyContainer.TableRowInfo = nil then
    Exit;
  ACount := APropertyContainer.TableRowInfo.CellMarginsActions.Count;
  for I := 0 to ACount - 1 do
  begin
    AOperand := APropertyContainer.TableRowInfo.CellMarginsActions[I];
    AStart := Math.Max(AOperand.StartIndex, 0);
    AEnd := Math.Min(AOperand.EndIndex, ARow.Cells.Count - 1);
    for ACellIndex := AStart to AEnd do
      AOperand.ApplyProperties(ARow.Cells[ACellIndex].Properties.CellMargins, APropertyContainer.UnitConverter);
  end;
end;

class procedure TdxTableCellPropertiesUpdater.ApplyOldCellBackgroundColors(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
begin
  if APropertyContainer.TableRowInfo = nil then
    Exit;
  ApplyOldCellBackgroundColorsCore(ARow, APropertyContainer.TableRowInfo.DefaultCellsShading, 0);
  ApplyOldCellBackgroundColorsCore(ARow, APropertyContainer.TableRowInfo.CellShading1, 0);
  ApplyOldCellBackgroundColorsCore(ARow, APropertyContainer.TableRowInfo.CellShading2, 22);
  ApplyOldCellBackgroundColorsCore(ARow, APropertyContainer.TableRowInfo.CellShading3, 44);
end;

class procedure TdxTableCellPropertiesUpdater.ApplyOldCellBackgroundColorsCore(ARow: TdxTableRow; AList: TdxAlphaColorList; AFirstCellIndex: Integer);
var
  ACells: TdxTableCellCollection;
  I: Integer;
begin
  ACells := ARow.Cells;
  for I := 0 to AList.Count - 1 do
  begin
    if not TdxAlphaColors.IsEmpty(AList[I]) then
      ACells[I + AFirstCellIndex].BackgroundColor := AList[I];
  end;
end;

class procedure TdxTableCellPropertiesUpdater.ApplyCellBackgroundColors(ARow: TdxTableRow; APropertyContainer: TdxDocPropertyContainer);
var
  ACount, I: Integer;
begin
  if APropertyContainer.TableCellInfo = nil then
    Exit;
  ACount := APropertyContainer.TableCellInfo.CellColors.Count;
  for I := 0 to ACount - 1 do
    if APropertyContainer.TableCellInfo.CellColors[I] <> TdxAlphaColors.Empty then
      ARow.Cells[I].BackgroundColor := APropertyContainer.TableCellInfo.CellColors[I];
end;

end.
