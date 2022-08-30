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

unit dxRichEdit.Export.Rtf.TableExporter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.Export.Rtf;

type
  TdxRtfTableExporterStateBase = class;
  TdxRtfTableExportHelper = class;

  { TdxRtfTableExporter }

  TdxRtfTableExporter = class
  private
    FExporter: TdxRtfContentExporter;
    FExportAsNestedTable: Boolean;
  protected
    function CreateExporterState(ATable: TdxTable): TdxRtfTableExporterStateBase; virtual;
    function GetRootTable(ATable: TdxTable): TdxTable;
    function GetNestingLevel(ATable: TdxTable): Integer;
  public
    constructor Create(AExporter: TdxRtfContentExporter);
    function Export(ATable: TdxTable): TdxParagraphIndex;

    property RtfExporter: TdxRtfContentExporter read FExporter;
    property ExportAsNestedTable: Boolean read FExportAsNestedTable write FExportAsNestedTable;
  end;

  { TdxRtfTableExporterStateBase }

  TdxRtfTableExporterStateBase = class abstract
  private
    FRtfExporter: TdxRtfContentExporter;
    FTableHelper: TdxRtfTableExportHelper;
    FNestingLevel: Integer;
    FTableStyleIndex: Integer;
    FTableRowPropertiesExporter: TdxRtfTableRowPropertiesExporter;
    FTableCellPropertiesExporter: TdxRtfTableCellPropertiesExporter;
    FTablePropertiesExporter: TdxRtfTablePropertiesExporter;
    function GetRtfBuilder: TdxRtfBuilder;
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
    function GetUnitConverter: TdxDocumentModelUnitConverter;
  protected
    FRowLeftOffset: Integer;
    procedure ExportBase; virtual;
    function GetTableStyleIndex: Integer;
    procedure ExportRowCells(ARow: TdxTableRow; ARowIndex: Integer);
    function IsCellEmpty(ACell: TdxTableCell): Boolean;
    procedure ExportInTableParagraph(AParIndex: TdxParagraphIndex; ATableNestingLevel: Integer; AIsEndParagraph: Boolean;
      ACondTypes: TdxConditionalTableStyleFormattingTypes); virtual;
    procedure FinishParagraph(AIsEndParagraph: Boolean);
    procedure ExportCellParagraphs(ACell: TdxTableCell; AParentRowIndex: Integer);
    function ExportNestedTable(ACell: TdxTableCell; AParIndex: TdxParagraphIndex; var ALastParIndex: TdxParagraphIndex): Boolean; overload;
    procedure ExportNestedTable(ATable: TdxTable; var ALastParIndex: TdxParagraphIndex); overload; virtual;
    function CreateTableHelper(ATable: TdxTable): TdxRtfTableExportHelper; virtual;
    procedure ExportRow(ARow: TdxTableRow; ARowIndex: Integer); virtual; abstract;
    procedure WriteParagraphEndMark; virtual; abstract;
    procedure ExportRowProperties(ARow: TdxTableRow; ARowIndex: Integer);
    function CalculateRowLeft(ARow: TdxTableRow; AIndent: TdxWidthUnit): Integer;
    function CalculateRowLeftOffset(ARow: TdxTableRow): Integer; virtual;
    function GetActualWidth(AUnit: TdxWidthUnit): Integer;
    procedure ExportOwnRowProperties(ARow: TdxTableRow; ARowIndex: Integer; ALeft: Integer);
    procedure StartNewRow(ARowIndex: Integer);
    procedure WriteTableStyleIndex;
    function CalcHalfSpaceBetweenCells: Integer;
    procedure ExportCellProperties(ACell: TdxTableCell; ACellRight: Integer);

    property RtfContentExporter: TdxRtfContentExporter read FRtfExporter;
    property RtfBuilder: TdxRtfBuilder read GetRtfBuilder;
    property TableHelper: TdxRtfTableExportHelper read FTableHelper;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;
  public
    constructor Create(ARtfExporter: TdxRtfContentExporter; ATable: TdxTable; ANestingLevel: Integer);
    destructor Destroy; override;
    procedure Export; virtual; abstract;

    property NestingLevel: Integer read FNestingLevel;
  end;

  { TdxRtfTableExportHelper }

  TdxRtfTableExportHelper = class
  public const
    RootTableNestingLevel = 1;
  private
    FTable: TdxTable;
    FWidth: Integer;
    FGrid: TdxTableGrid;
    function GetWidth: Integer;
    function GetGrid: TdxTableGrid;
  protected
    function GetTableGrid: TdxTableGrid; virtual;
    function GetTableWidth: Integer;
  public
    constructor Create(ATable: TdxTable);
    destructor Destroy; override;
    function GetCellWidth(ALeftSideIndex: Integer; AColumnSpan: Integer): Integer;
    function GetLastParagraphIndex: TdxParagraphIndex;

    property Table: TdxTable read FTable;
    property Width: Integer read GetWidth;
    property Grid: TdxTableGrid read GetGrid;
  end;

  { TdxRtfTableExporterState }

  TdxRtfTableExporterState = class(TdxRtfTableExporterStateBase)
  protected
    procedure WriteParagraphEndMark; override;
    procedure ExportRow(ARow: TdxTableRow; ARowIndex: Integer); override;
  public
    constructor Create(ARtfExporter: TdxRtfContentExporter; ATable: TdxTable);
    procedure Export; override;
  end;

  { TdxRtfTableStartingWithNestedTableExporterState }

  TdxRtfTableStartingWithNestedTableExporterState = class(TdxRtfTableExporterState)
  public
    procedure Export; override;
  end;

  { TdxRtfNestedTableExporterState }

  TdxRtfNestedTableExporterState = class(TdxRtfTableExporterStateBase)
  protected
    procedure WriteParagraphEndMark; override;
    procedure ExportRow(ARow: TdxTableRow; ARowIndex: Integer); override;
    procedure WriteNoNestedTableGroup;
  public
    procedure Export; override;
  end;

implementation

uses
  Math,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.TableCalculator,
  dxRichEdit.Export.Rtf.Keywords;

{ TdxRtfTableExporter }

constructor TdxRtfTableExporter.Create(AExporter: TdxRtfContentExporter);
begin
  inherited Create;
  FExporter := AExporter;
end;

function TdxRtfTableExporter.Export(ATable: TdxTable): TdxParagraphIndex;
var
  ATableExporterState: TdxRtfTableExporterStateBase;
begin
  ATableExporterState := CreateExporterState(ATable);
  try
    ATableExporterState.Export;
    Result := ATableExporterState.TableHelper.GetLastParagraphIndex;
  finally
    ATableExporterState.Free;
  end;
end;

function TdxRtfTableExporter.CreateExporterState(ATable: TdxTable): TdxRtfTableExporterStateBase;
var
  ARootTable: TdxTable;
begin
  if GetNestingLevel(ATable) > 1 then
  begin
    ARootTable := GetRootTable(ATable);
    if ExportAsNestedTable then
      Result := TdxRtfNestedTableExporterState.Create(RtfExporter, ARootTable, 2)
    else
      Result := TdxRtfTableStartingWithNestedTableExporterState.Create(RtfExporter, ARootTable);
  end
  else
  begin
    if ExportAsNestedTable then
      Result := TdxRtfNestedTableExporterState.Create(RtfExporter, ATable, 2)
    else
      Result := TdxRtfTableExporterState.Create(RtfExporter, ATable);
  end;
end;

function TdxRtfTableExporter.GetRootTable(ATable: TdxTable): TdxTable;
begin
  Result := ATable;
  while Result.ParentCell <> nil do
    Result := Result.ParentCell.Table;
end;

function TdxRtfTableExporter.GetNestingLevel(ATable: TdxTable): Integer;
var
  AParentCell: TdxTableCell;
  AParentTable: TdxTable;
begin
  Result := 1;
  AParentCell := ATable.ParentCell;
  while AParentCell <> nil do
  begin
    AParentTable := AParentCell.Row.Table;
    AParentCell := AParentTable.ParentCell;
    Inc(Result);
  end;
end;

{ TdxRtfTableExporterStateBase }

constructor TdxRtfTableExporterStateBase.Create(ARtfExporter: TdxRtfContentExporter;
  ATable: TdxTable; ANestingLevel: Integer);
begin
  inherited Create;
  FRtfExporter := ARtfExporter;
  FNestingLevel := ANestingLevel;
  FTableHelper := CreateTableHelper(ATable);
  FTableRowPropertiesExporter := TdxRtfTableRowPropertiesExporter.Create(DocumentModel, RtfContentExporter.RtfExportHelper, RtfBuilder);
  FTableCellPropertiesExporter := TdxRtfTableCellPropertiesExporter.Create(DocumentModel, RtfContentExporter.RtfExportHelper, RtfBuilder);
  FTablePropertiesExporter := TdxRtfTablePropertiesExporter.Create(DocumentModel, RtfContentExporter.RtfExportHelper, RtfBuilder);
  FTableStyleIndex := GetTableStyleIndex;
  FRowLeftOffset := -MaxInt;
end;

destructor TdxRtfTableExporterStateBase.Destroy;
begin
  FreeAndNil(FTableHelper);
  FreeAndNil(FTableRowPropertiesExporter);
  FreeAndNil(FTableCellPropertiesExporter);
  FreeAndNil(FTablePropertiesExporter);
  inherited Destroy;
end;

function TdxRtfTableExporterStateBase.GetRtfBuilder: TdxRtfBuilder;
begin
  Result := RtfContentExporter.RtfBuilder;
end;

function TdxRtfTableExporterStateBase.GetDocumentModel: TdxDocumentModel;
begin
  Result := RtfContentExporter.DocumentModel;
end;

function TdxRtfTableExporterStateBase.GetPieceTable: TdxPieceTable;
begin
  Result := RtfContentExporter.PieceTable;
end;

function TdxRtfTableExporterStateBase.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := DocumentModel.UnitConverter;
end;

procedure TdxRtfTableExporterStateBase.ExportBase;
var
  ARowsCount, ARowIndex: Integer;
begin
  ARowsCount := TableHelper.Table.Rows.Count;
  for ARowIndex := 0 to ARowsCount - 1 do
    ExportRow(TableHelper.Table.Rows[ARowIndex], ARowIndex);
end;

function TdxRtfTableExporterStateBase.GetTableStyleIndex: Integer;
var
  AStyleName: string;
  AStyleCollection: TdxNamedOrdinalDictionary<Integer>;
begin
  if FRtfExporter.RtfExportHelper.SupportStyle and
    (FTableHelper.Table.StyleIndex <> TdxTableStyleCollection.DefaultTableStyleIndex) then
  begin
    AStyleName := FTableHelper.Table.TableStyle.StyleName;
    AStyleCollection := FRtfExporter.RtfExportHelper.TableStylesCollectionIndex;
    if AStyleCollection.ContainsKey(AStyleName) then
      Exit(AStyleCollection[AStyleName]);
  end;
  Result := -1;
end;

procedure TdxRtfTableExporterStateBase.ExportRowCells(ARow: TdxTableRow; ARowIndex: Integer);
var
  ACellsCount, I: Integer;
begin
  ACellsCount := ARow.Cells.Count;
  for I := 0 to ACellsCount - 1 do
    ExportCellParagraphs(ARow.Cells[I], ARowIndex);
end;

function TdxRtfTableExporterStateBase.IsCellEmpty(ACell: TdxTableCell): Boolean;
var
  AParagraph: TdxParagraph;
begin
  if ACell.StartParagraphIndex <> ACell.EndParagraphIndex then
    Exit(False);
  AParagraph := PieceTable.Paragraphs[ACell.StartParagraphIndex];
  Result := AParagraph.Length = 1;
end;

procedure TdxRtfTableExporterStateBase.ExportInTableParagraph(AParIndex: TdxParagraphIndex;
  ATableNestingLevel: Integer; AIsEndParagraph: Boolean;
  ACondTypes: TdxConditionalTableStyleFormattingTypes);
var
  AParagraph: TdxParagraph;
begin
  AParagraph := PieceTable.Paragraphs[AParIndex];
  RtfContentExporter.ExportParagraphCore(AParagraph, ATableNestingLevel, ACondTypes, FTableStyleIndex);
  FinishParagraph(AIsEndParagraph);
end;

procedure TdxRtfTableExporterStateBase.FinishParagraph(AIsEndParagraph: Boolean);
begin
  if AIsEndParagraph then
    WriteParagraphEndMark
  else
    RtfBuilder.WriteCommand(TdxRtfExportSR.EndOfParagraph);
end;

procedure TdxRtfTableExporterStateBase.ExportCellParagraphs(ACell: TdxTableCell; AParentRowIndex: Integer);
var
  AStartParagraphIndex, AEndParagraphIndex, AParIndex, ANextParIndex: TdxParagraphIndex;
  AIsEndParagraph: Boolean;
  ARow: TdxTableRow;
begin
  AStartParagraphIndex := ACell.StartParagraphIndex;
  AEndParagraphIndex := ACell.EndParagraphIndex;
  AParIndex := AStartParagraphIndex;
  while AParIndex <= AEndParagraphIndex do
  begin
    if ExportNestedTable(ACell, AParIndex, ANextParIndex) then
    begin
      AParIndex := ANextParIndex;
      if NestingLevel = 1 then
      begin
        ARow := ACell.Row;
        ExportRowProperties(ARow, AParentRowIndex);
      end;
    end
    else
    begin
      AIsEndParagraph := AParIndex = AEndParagraphIndex;
      ExportInTableParagraph(AParIndex, NestingLevel, AIsEndParagraph, ACell.CellConditionalFormattingMasks);
    end;
    Inc(AParIndex);
  end;
end;

function TdxRtfTableExporterStateBase.ExportNestedTable(ACell: TdxTableCell; AParIndex: TdxParagraphIndex;
  var ALastParIndex: TdxParagraphIndex): Boolean;
var
  AParagraph: TdxParagraph;
  AParCell: TdxTableCell;
  ANestedTable: TdxTable;
begin
  AParagraph := PieceTable.Paragraphs[AParIndex];
  AParCell := AParagraph.GetCell;
  if AParCell = ACell then
  begin
    ALastParIndex := AParIndex;
    Exit(False);
  end;
  ANestedTable := AParCell.Table;
  while ACell <> ANestedTable.ParentCell do
  begin
    ANestedTable := ANestedTable.ParentCell.Table;
  end;
  ExportNestedTable(ANestedTable, ALastParIndex);
  Result := True;
end;

procedure TdxRtfTableExporterStateBase.ExportNestedTable(ATable: TdxTable; var ALastParIndex: TdxParagraphIndex);
var
  ANestedTableExporter: TdxRtfNestedTableExporterState;
begin
  ANestedTableExporter := TdxRtfNestedTableExporterState.Create(RtfContentExporter, ATable, NestingLevel + 1);
  try
    ANestedTableExporter.Export;
    ALastParIndex := ANestedTableExporter.TableHelper.GetLastParagraphIndex;
  finally
    FreeAndNil(ANestedTableExporter);
  end;
end;

function TdxRtfTableExporterStateBase.CreateTableHelper(ATable: TdxTable): TdxRtfTableExportHelper;
begin
  Result := TdxRtfTableExportHelper.Create(ATable);
end;

procedure TdxRtfTableExporterStateBase.ExportRowProperties(ARow: TdxTableRow; ARowIndex: Integer);
var
  ARowLeft, ACellsCount, ACellLeftSideIndex, ACellRight, I: Integer;
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  ACell: TdxTableCell;
  AColumnSpan, ACellWidth: Integer;
begin
  ARowLeft := 0;
  ExportOwnRowProperties(ARow, ARowIndex, ARowLeft);
  ACellsCount := ARow.Cells.Count;
  ACellLeftSideIndex := ARow.GridBefore;
  AUnitConverter := DocumentModel.ToDocumentLayoutUnitConverter;
  ACellRight := AUnitConverter.ToLayoutUnits(ARowLeft);
  for I := 0 to ACellsCount - 1 do
  begin
    ACell := ARow.Cells[I];
    AColumnSpan := ACell.ColumnSpan;
    ACellWidth := TableHelper.GetCellWidth(ACellLeftSideIndex, AColumnSpan);
    Inc(ACellLeftSideIndex, AColumnSpan);
    Inc(ACellRight, ACellWidth);
    ExportCellProperties(ACell, AUnitConverter.ToModelUnits(ACellRight));
  end;
end;

function TdxRtfTableExporterStateBase.CalculateRowLeft(ARow: TdxTableRow; AIndent: TdxWidthUnit): Integer;
var
  AWidthBefore, AOffset: Integer;
begin
  if ARow.GridBefore > 0 then
    AWidthBefore := TableHelper.GetCellWidth(0, ARow.GridBefore)
  else
    AWidthBefore := 0;
  AOffset := GetActualWidth(AIndent) + AWidthBefore;

  if FRowLeftOffset = -MaxInt then
    Result := AOffset - CalculateRowLeftOffset(ARow)
  else
    Result := AOffset - FRowLeftOffset;
end;

function TdxRtfTableExporterStateBase.CalculateRowLeftOffset(ARow: TdxTableRow): Integer;
var
  ABorderWidth: Integer;
  ALeftMargin: TdxWidthUnit;
begin
  ABorderWidth := ARow.Cells.First.GetActualLeftCellBorder.Width;
  ALeftMargin := ARow.Cells.First.GetActualLeftMargin;
  FRowLeftOffset := Max(ABorderWidth div 2, GetActualWidth(ALeftMargin));
  Result := FRowLeftOffset;
end;

function TdxRtfTableExporterStateBase.GetActualWidth(AUnit: TdxWidthUnit): Integer;
begin
  if AUnit.&Type = TdxWidthUnitType.ModelUnits then
    Result := UnitConverter.ModelUnitsToTwips(AUnit.Value)
  else
    Result := 0;
end;

procedure TdxRtfTableExporterStateBase.ExportOwnRowProperties(ARow: TdxTableRow; ARowIndex: Integer; ALeft: Integer);
var
  ATable: TdxTable;
begin
  StartNewRow(ARowIndex);
  FTableRowPropertiesExporter.WriteRowAlignment(ARow.TableRowAlignment);
  ATable := ARow.Table;
  FTablePropertiesExporter.WriteTableBorders(ATable.GetActualTopBorder.Info, ATable.GetActualLeftBorder.Info, ATable.GetActualBottomBorder.Info, ATable.GetActualRightBorder.Info, ATable.GetActualInsideHorizontalBorder.Info, ATable.GetActualInsideVerticalBorder.Info);
  FTablePropertiesExporter.WriteTableFloatingPosition(ATable.TableProperties.FloatingPosition.Info);
  ALeft := CalculateRowLeft(ARow, ATable.TableIndent);
  FTablePropertiesExporter.WriteRowLeft(ALeft);
  FTableRowPropertiesExporter.WriteRowHeight(ARow.Height.Info);
  FTableRowPropertiesExporter.WriteRowHeader(ARow.Header);
  FTableRowPropertiesExporter.WriteRowCantSplit(ARow.CantSplit);
  FTablePropertiesExporter.WriteTableWidth(ATable.PreferredWidth.Info);
  FTableRowPropertiesExporter.WriteWidthBefore(ARow.WidthBefore.Info);
  FTableRowPropertiesExporter.WriteWidthAfter(ARow.WidthAfter.Info);
  FTablePropertiesExporter.WriteTableLayout(ATable.TableLayout);
  FTableRowPropertiesExporter.WriteRowCellSpacing(ARow.CellSpacing.Info);
  FTablePropertiesExporter.WriteTableCellMargins(ATable.LeftMargin.Info, ATable.RightMargin.Info, ATable.BottomMargin.Info, ATable.TopMargin.Info);
  FTablePropertiesExporter.WriteTableLook(ATable.TableLook);
  FTablePropertiesExporter.WriteTableIndent(ATable.TableIndent.Info);
  FTablePropertiesExporter.WriteBandSizes(ATable.TableProperties.GeneralSettings.Info, ATable.TableStyle.HasRowBandingStyleProperties, ATable.TableStyle.HasColumnBandingStyleProperties);
end;

procedure TdxRtfTableExporterStateBase.StartNewRow(ARowIndex: Integer);
begin
  RtfBuilder.WriteCommand(TdxRtfExportSR.ResetTableProperties);
  RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowIndex, ARowIndex);
  if TdxTableLookType.ApplyFirstRow in TableHelper.Table.TableLook then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowBandIndex, ARowIndex - 1)
  else
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowBandIndex, ARowIndex);
  if ARowIndex = FTableHelper.Table.Rows.Count - 1 then
    FTableRowPropertiesExporter.WriteLastRowMark;
  WriteTableStyleIndex;
  FTableRowPropertiesExporter.WriteHalfSpaceBetweenCells(CalcHalfSpaceBetweenCells);
end;

procedure TdxRtfTableExporterStateBase.WriteTableStyleIndex;
begin
  if FTableStyleIndex <> -1 then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableStyleIndex, FTableStyleIndex);
end;

function TdxRtfTableExporterStateBase.CalcHalfSpaceBetweenCells: Integer;
var
  ALeftMargin, ARightMargin: TdxWidthUnit;
  ALeftMarginVal, ARightMarginVal: Integer;
begin
  ALeftMargin := FTableHelper.Table.TableProperties.CellMargins.Left;
  if ALeftMargin.&Type = TdxWidthUnitType.ModelUnits then
    ALeftMarginVal := UnitConverter.ModelUnitsToTwips(ALeftMargin.Value)
  else
    ALeftMarginVal := 0;
  ARightMargin := FTableHelper.Table.TableProperties.CellMargins.Right;
  if ARightMargin.&Type = TdxWidthUnitType.ModelUnits then
    ARightMarginVal := UnitConverter.ModelUnitsToTwips(ARightMargin.Value)
  else
    ARightMarginVal := 0;
  Result := (ALeftMarginVal + ARightMarginVal) div 2;
end;

procedure TdxRtfTableExporterStateBase.ExportCellProperties(ACell: TdxTableCell; ACellRight: Integer);
begin
  FTableCellPropertiesExporter.WriteCellMerging(ACell.VerticalMerging);
  FTableCellPropertiesExporter.WriteCellVerticalAlignment(ACell.VerticalAlignment);
  FTableCellPropertiesExporter.WriteCellBackgroundColor(ACell);
  FTableCellPropertiesExporter.WriteCellForegroundColor(ACell);
  FTableCellPropertiesExporter.WriteCellShading(ACell);
  FTableCellPropertiesExporter.WriteCellBasicBorders(ACell.GetActualTopCellBorder.Info, ACell.GetActualLeftCellBorder.Info, ACell.GetActualRightCellBorder.Info, ACell.GetActualBottomCellBorder.Info);
  FTableCellPropertiesExporter.WriteCellTextDirection(ACell.TextDirection);
  FTableCellPropertiesExporter.WriteCellFitText(ACell.FitText);
  FTableCellPropertiesExporter.WriteCellNoWrap(ACell.NoWrap);
  FTableCellPropertiesExporter.WriteCellHideCellMark(ACell.HideCellMark);
  FTableCellPropertiesExporter.WriteCellPreferredWidth(ACell.PreferredWidth.Info);
  FTableCellPropertiesExporter.WriteCellMargins(ACell.GetActualTopMargin.Info, ACell.GetActualLeftMargin.Info, ACell.GetActualRightMargin.Info, ACell.GetActualBottomMargin.Info);
  FTableCellPropertiesExporter.WriteCellRight(ACellRight);
end;

{ TdxRtfTableExportHelper }

constructor TdxRtfTableExportHelper.Create(ATable: TdxTable);
begin
  inherited Create;
  FWidth := -1;
  FTable := ATable;
end;

destructor TdxRtfTableExportHelper.Destroy;
begin
  FreeAndNil(FGrid);
  inherited Destroy;
end;

function TdxRtfTableExportHelper.GetWidth: Integer;
begin
  if FWidth = -1 then
    FWidth := GetTableWidth;
  Result := FWidth;
end;

function TdxRtfTableExportHelper.GetGrid: TdxTableGrid;
begin
  if FGrid = nil then
    FGrid := GetTableGrid;
  Result := FGrid;
end;

function TdxRtfTableExportHelper.GetCellWidth(ALeftSideIndex: Integer; AColumnSpan: Integer): Integer;
var
  I: Integer;
begin
  if (ALeftSideIndex < 0) or (AColumnSpan <= 0) then
    TdxRichEditExceptions.ThrowInternalException;
  Result := 0;
  for I := 0 to AColumnSpan - 1 do
    Inc(Result, Max(Grid[I + ALeftSideIndex].Width, 1));
end;

function TdxRtfTableExportHelper.GetLastParagraphIndex: TdxParagraphIndex;
var
  ALastCell: TdxTableCell;
begin
  ALastCell := Table.Rows.Last.Cells.Last;
  Result := ALastCell.EndParagraphIndex;
end;

function TdxRtfTableExportHelper.GetTableGrid: TdxTableGrid;
var
  ADocumentModel: TdxCustomDocumentModel;
  AWidthsCalculator: TdxRtfTableWidthsCalculator;
  ACalculator: TdxTableGridCalculator;
begin
  ADocumentModel := Table.DocumentModel;
  AWidthsCalculator := TdxRtfTableWidthsCalculator.Create(ADocumentModel.ToDocumentLayoutUnitConverter);
  try
    ACalculator := TdxTableGridCalculator.Create(AWidthsCalculator, MaxInt);
    try
      Result := ACalculator.CalculateTableGrid(Table, ADocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(TdxRtfTableWidthsCalculator.DefaultPercentBaseWidthInTwips));
    finally
      ACalculator.Free;
    end;
  finally
    AWidthsCalculator.Free;
  end;
end;

function TdxRtfTableExportHelper.GetTableWidth: Integer;
var
  I, AColCount: Integer;
  AColumns: TdxTableGridColumnCollection;
begin
  Result := 0;
  AColumns := Grid.Columns;
  AColCount := AColumns.Count;
  for I := 0 to AColCount - 1 do
    Inc(Result, AColumns[I].Width);
end;

{ TdxRtfTableExporterState }

constructor TdxRtfTableExporterState.Create(ARtfExporter: TdxRtfContentExporter; ATable: TdxTable);
begin
  inherited Create(ARtfExporter, ATable, 1);
end;

procedure TdxRtfTableExporterState.Export;
begin
  ExportRowProperties(TableHelper.Table.Rows.First, 0);
  ExportBase;
end;

procedure TdxRtfTableExporterState.WriteParagraphEndMark;
begin
  RtfBuilder.WriteCommand(TdxRtfExportSR.TableEndCell);
end;

procedure TdxRtfTableExporterState.ExportRow(ARow: TdxTableRow; ARowIndex: Integer);
begin
  ExportRowCells(ARow, ARowIndex);
  ExportRowProperties(ARow, ARowIndex);
  RtfBuilder.WriteCommand(TdxRtfExportSR.TableEndRow);
end;

{ TdxRtfTableStartingWithNestedTableExporterState }

procedure TdxRtfTableStartingWithNestedTableExporterState.Export;
begin
  ExportBase;
end;

{ TdxRtfNestedTableExporterState }

procedure TdxRtfNestedTableExporterState.Export;
begin
  ExportBase;
end;

procedure TdxRtfNestedTableExporterState.WriteParagraphEndMark;
begin
  RtfBuilder.WriteCommand(TdxRtfExportSR.NestedTableEndCell);
  WriteNoNestedTableGroup;
end;

procedure TdxRtfNestedTableExporterState.ExportRow(ARow: TdxTableRow; ARowIndex: Integer);
begin
  ExportRowCells(ARow, ARowIndex);
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.NestedTableProperties);
  ExportRowProperties(ARow, ARowIndex);
  RtfBuilder.WriteCommand(TdxRtfExportSR.NestedTableEndRow);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfNestedTableExporterState.WriteNoNestedTableGroup;
begin
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.NoNestedTable);
  RtfBuilder.WriteCommand(TdxRtfExportSR.EndOfParagraph);
  RtfBuilder.CloseGroup;
end;

end.

