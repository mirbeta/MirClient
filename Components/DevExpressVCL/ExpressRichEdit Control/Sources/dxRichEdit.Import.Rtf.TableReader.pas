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

unit dxRichEdit.Import.Rtf.TableReader;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxRichEdit.Import.Rtf.ParagraphFormatting,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.Borders,
  dxGenerics;

type
  TdxRtfTableReader = class;
  TdxRtfTableController = class;
  TdxRtfTable = class;
  TdxRtfTableCell = class;
  TdxRtfTableRowController = class;
  TdxRtfTableProperties = class;
  TdxRtfTableRowProperties = class;
  TdxRtfTableCellProperties = class;
  TdxRtfTableRow = class;

  TdxRtfCellSpacing = class;

  { TdxRtfTableCustomItemCollection }

  TdxRtfTableCustomItemCollection<T: class> = class(TdxList<T>)
  public
    function First: T; reintroduce;
    function Last: T; reintroduce;
  end;

  { TdxRtfTableCellProperties }

  TdxRtfTableCellProperties = class(TdxTableCellProperties)
  strict private
    FRight: Integer;
    FHorizontalMerging: TdxMergingState;
  public
    property Right: Integer read FRight write FRight;
    property HorizontalMerging: TdxMergingState read FHorizontalMerging write FHorizontalMerging;
  end;

  TdxRtfTableCellPropertiesCollection = class(TdxRtfTableCustomItemCollection<TdxRtfTableCellProperties>);

  { TdxRtfTableReaderStateBase }

  TdxRtfTableReaderStateBase = class abstract
  strict private
    FReader: TdxRtfTableReader;
    function GetDocumentModel: TdxDocumentModel;
    function GetImporter: TObject;
    function GetTableController: TdxRtfTableController;
  public
    constructor Create(AReader: TdxRtfTableReader);

    procedure OnStartNestedTableProperties; virtual; abstract;
    procedure OnEndParagraph(AParagraphFormattingInfo: TdxRtfParagraphFormattingInfo); virtual; abstract;
    procedure OnEndRow; virtual; abstract;
    procedure OnEndCell; virtual; abstract;
    procedure OnEndNestedRow; virtual; abstract;
    procedure OnEndNestedCell; virtual; abstract;
    procedure OnTableRowDefaults; virtual; abstract;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property Importer: TObject read GetImporter;
    property Reader: TdxRtfTableReader read FReader;
    property TableController: TdxRtfTableController read GetTableController;
  end;

  { TdxTableRtfTableManagerState }

  TdxTableRtfTableManagerState = class(TdxRtfTableReaderStateBase)
  protected
    procedure ValidateCurrentTable; virtual;
    procedure OnEndInTableParagraph(ANestingLevel: Integer); virtual;
    function IsParagraphInTable(AParagraphFormattingInfo: TdxRtfParagraphFormattingInfo): Boolean;
    function IsCurrentTableNotComplete: Boolean;
    procedure OnEndRowCore; virtual;
    procedure OnEndCellCore(ANestingLevel: Integer); virtual;
  public
    constructor Create(AReader: TdxRtfTableReader);
    procedure OnEndParagraph(AParagraphFormattingInfo: TdxRtfParagraphFormattingInfo); override;
    procedure OnEndRow; override;
    procedure OnEndCell; override;
    procedure OnEndNestedRow; override;
    procedure OnEndNestedCell; override;
    procedure OnStartNestedTableProperties; override;
    procedure OnTableRowDefaults; override;
  end;

  { TdxNoTableRtfTableReaderState }

  TdxNoTableRtfTableReaderState = class(TdxRtfTableReaderStateBase)
  public const
    DefaultNestingLevel = 1;
  protected
    function ChangeState: TdxTableRtfTableManagerState;
  public
    procedure OnStartNestedTableProperties; override;
    procedure OnEndParagraph(AParagraphFormattingInfo: TdxRtfParagraphFormattingInfo); override;
    procedure OnEndRow; override;
    procedure OnEndCell; override;
    procedure OnEndNestedRow; override;
    procedure OnEndNestedCell; override;
    procedure OnTableRowDefaults; override;
  end;

  { TdxRtfTableState }

  TdxRtfTableState = class
  strict private
    FTable: TdxRtfTable;
    FTableProperties: TdxRtfTableProperties;
    FRowProperties: TdxRtfTableRowProperties;
    FCellPropertiesCollection: TdxRtfTableCellPropertiesCollection;
  public
    constructor Create(ATable: TdxRtfTable; AReader: TdxRtfTableReader);
    property Table: TdxRtfTable read FTable;
    property TableProperties: TdxRtfTableProperties read FTableProperties;
    property RowProperties: TdxRtfTableRowProperties read FRowProperties;
    property CellPropertiesCollection: TdxRtfTableCellPropertiesCollection read FCellPropertiesCollection;
  end;

  TdxRtfTableCollection = class(TdxRtfTableCustomItemCollection<TdxRtfTable>);

  { TdxRtfTableCellController }

  TdxRtfTableCellController = class
  strict private
    FRowController: TdxRtfTableRowController;
    FCurrentCell: TdxRtfTableCell;
    function GetReader: TdxRtfTableReader;
  protected
    procedure SetParagraphIndexCore(ACell: TdxRtfTableCell; AParagraphIndex: Integer);
    procedure SetParagraphIndexesToParentCell(AParagraphIndex: Integer);

    property Reader: TdxRtfTableReader read GetReader;
  public
    constructor Create(ARowController: TdxRtfTableRowController);

    procedure AssignLastCellAsCurrent;
    procedure FinishCell;
    function IsCurrentCellNotComplete: Boolean;
    procedure Reset;
    procedure SetParagraphIndex(AParagraphIndex: Integer);
    procedure StartNewCell;

    property CurrentCell: TdxRtfTableCell read FCurrentCell;
    property RowController: TdxRtfTableRowController read FRowController;
  end;

  TdxRtfTableCellCollection = class(TdxRtfTableCustomItemCollection<TdxRtfTableCell>);

  { TdxRtfTableRow }

  TdxRtfTableRow = class
  strict private
    FCells: TdxRtfTableCellCollection;
    FOffset: Integer;
    FLeft: Integer;
    FIndex: Integer;
    FGridBefore: Integer;
    FDefineProperties: Boolean;
    FRowProperties: TdxRtfTableRowProperties;
    FTable: TdxRtfTable;
    function GetActualLeft: Integer;
    function GetRight: Integer;
  public
    constructor Create(ATable: TdxRtfTable; AImporter: TObject);
    destructor Destroy; override;
    procedure CopyFrom(ARow: TdxRtfTableRow);

    property ActualLeft: Integer read GetActualLeft;
    property Properties: TdxRtfTableRowProperties read FRowProperties;
    property Cells: TdxRtfTableCellCollection read FCells;
    property Left: Integer read FLeft write FLeft;
    property Table: TdxRtfTable read FTable write FTable;
    property Offset: Integer read FOffset write FOffset;
    property Index: Integer read FIndex write FIndex;
    property GridBefore: Integer read FGridBefore write FGridBefore;
    property DefineProperties: Boolean read FDefineProperties write FDefineProperties;
    property Right: Integer read GetRight;
  end;

  TdxRtfTableRowCollection = class(TdxRtfTableCustomItemCollection<TdxRtfTableRow>);

  { TdxRtfTableRowController }

  TdxRtfTableRowController = class
  strict private
    FTableController: TdxRtfTableController;
    FCurrentRow: TdxRtfTableRow;
    FCellController: TdxRtfTableCellController;
    function GetReader: TdxRtfTableReader;
  protected
    procedure AssignCellProperties;
    procedure AssignRowProperties;
    function CreateCellController: TdxRtfTableCellController;
    procedure FinishRowCore;

    property Reader: TdxRtfTableReader read GetReader;
    property TableController: TdxRtfTableController read FTableController;
  public
    constructor Create(ATableController: TdxRtfTableController);
    destructor Destroy; override;

    procedure AssignLastRowAsCurrent;
    procedure FinishRow;
    function IsCurrentRowNotComplete: Boolean;
    function IsCurrentRowValid: Boolean;
    procedure StartNewRow; virtual;
    procedure Reset; virtual;

    property CurrentRow: TdxRtfTableRow read FCurrentRow;
    property CellController: TdxRtfTableCellController read FCellController;
  end;

  { TdxRtfTableController }

  TdxRtfTableController = class
  strict private
    FReader: TdxRtfTableReader;
    FCurrentTable: TdxRtfTable;
    FRowController: TdxRtfTableRowController;
  protected
    function CreateRowController: TdxRtfTableRowController; virtual;
    procedure CreateCurrentTable; virtual;
    procedure CreateNestedTable(ADepth: Integer);
  public
    constructor Create(AReader: TdxRtfTableReader);
    destructor Destroy; override;

    procedure ChangeTable(ANestingLevel: Integer);
    procedure FinishTable;
    procedure PopParentTable(ADepth: Integer);
    procedure Reset; virtual;

    property Reader: TdxRtfTableReader read FReader;
    property CurrentTable: TdxRtfTable read FCurrentTable;
    property RowController: TdxRtfTableRowController read FRowController;
  end;

  { TdxRtfTable }

  TdxRtfTable = class
  strict private
    FTableProperties: TdxRtfTableProperties;
    FRows: TdxRtfTableRowCollection;
    FParentCell: TdxRtfTableCell;
    FRight: Integer;
    FIndent: Integer;
    function GetNestingLevel: Integer;
    procedure SetRight(const AValue: Integer);
  protected
    function GetNestedLevel: Integer; virtual;
  public
    constructor Create(AImporter: TObject; AParentCell: TdxRtfTableCell = nil);
    destructor Destroy; override;

    property Properties: TdxRtfTableProperties read FTableProperties;
    property Rows: TdxRtfTableRowCollection read FRows;
    property ParentCell: TdxRtfTableCell read FParentCell write FParentCell;
    property NestingLevel: Integer read GetNestingLevel;
    property Right: Integer read FRight write SetRight;
    property Indent: Integer read FIndent write FIndent;
  end;

  { TdxRtfTableProperties }

  TdxRtfTableProperties = class(TdxTableProperties)
  private
    FCellSpacing: TdxRtfCellSpacing;
    FTableStyleIndex: Integer;
    FHalfSpace: Integer;
    FUseHalfSpace: Boolean;
    procedure SetHalfSpace(const AValue: Integer);
  public
    constructor Create(APieceTable: TdxPieceTable); reintroduce;
    destructor Destroy; override;

    function IsChanged: Boolean;
    procedure CopyFrom(ANewProperties: TdxTableProperties); override;

    property CellSpacing: TdxRtfCellSpacing read FCellSpacing;
    property TableStyleIndex: Integer read FTableStyleIndex write FTableStyleIndex;
    property HalfSpace: Integer read FHalfSpace write SetHalfSpace;
    property UseHalfSpace: Boolean read FUseHalfSpace;
  end;

  { TdxRtfCellSpacing }

  TdxRtfCellSpacing = class
  strict private
    FCellSpacing: TdxWidthUnit;
  public
    constructor Create(ACellSpacing: TdxWidthUnit);

    property Left: TdxWidthUnit read FCellSpacing;
    property Top: TdxWidthUnit read FCellSpacing;
    property Right: TdxWidthUnit read FCellSpacing;
    property Bottom: TdxWidthUnit read FCellSpacing;
  end;

  { TdxRtfTableRowProperties }

  TdxRtfTableRowProperties = class(TdxTableRowProperties)
  strict private
    FLeft: Integer;
    FFloatingPosition: TdxTableFloatingPositionInfo;
    FCellSpacing: TdxRtfCellSpacing;
  public
    constructor Create(APieceTable: TdxPieceTable); reintroduce;
    destructor Destroy; override;

    property Left: Integer read FLeft write FLeft;
    property FloatingPosition: TdxTableFloatingPositionInfo read FFloatingPosition;
    property CellSpacing: TdxRtfCellSpacing read FCellSpacing;
  end;

  { TdxRtfTableCell }

  TdxRtfTableCell = class(TcxIUnknownObject, IdxCellPropertiesOwner)
  private
    FRight: Integer;
    FCellProperties: TdxRtfTableCellProperties;
    FRow: TdxRtfTableRow;
    FStartParagraphIndex: TdxParagraphIndex;
    FEndParagraphIndex: TdxParagraphIndex;
    FIndex: Integer;
    function GetActualRight: Integer;
    function GetIsEmpty: Boolean;
  public
    constructor Create(ARow: TdxRtfTableRow; AImporter: TObject; ARight: Integer = 0);
    procedure CopyFrom(ACell: TdxRtfTableCell);
    function CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;

    property ActualRight: Integer read GetActualRight;
    property Properties: TdxRtfTableCellProperties read FCellProperties;
    property StartParagraphIndex: TdxParagraphIndex read FStartParagraphIndex write FStartParagraphIndex;
    property EndParagraphIndex: TdxParagraphIndex read FEndParagraphIndex write FEndParagraphIndex;
    property Right: Integer read FRight write FRight;
    property Row: TdxRtfTableRow read FRow write FRow;
    property IsEmpty: Boolean read GetIsEmpty;
    property Index: Integer read FIndex write FIndex;
  end;

  TdxRtfParentCellMap = TObjectDictionary<TdxRtfTableCell, TdxRtfTableCollection>;

  { TdxRtfTableReader }

  TdxRtfTableReader = class(TcxIUnknownObject, IdxCellPropertiesOwner)
  strict private
    FState: TdxRtfTableReaderStateBase;
    FImporter: TObject;
    FTableStack: TdxObjectStack<TdxRtfTableState>;
    FTableController: TdxRtfTableController;
    FTables: TdxRtfTableCollection;
    FTableProperties: TdxRtfTableProperties;
    FTableObjectsCollection: TdxFastObjectList;
    FRowProperties: TdxRtfTableRowProperties;
    FCellPropertiesCollection: TdxRtfTableCellPropertiesCollection;
    FCellProperties: TdxRtfTableCellProperties;
    FIsNestedTableProperetiesReading: Boolean;
    FParentCellMap: TdxRtfParentCellMap;
    FProcessedBorder: TdxBorderBase;
  private
    function GetDocumentModel: TdxDocumentModel;
    function GetCellProperties: TdxRtfTableCellProperties;
    function GetRowProperties: TdxRtfTableRowProperties;
    function GetTableProperties: TdxRtfTableProperties;
  protected
    function CreateTableController: TdxRtfTableController; virtual;
    procedure ResetProperties;
    procedure CreateCellProperties; virtual;
    function CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
    procedure RestoreProperties(AState: TdxRtfTableState);

    procedure AddTableObject(AObject: TObject);
    procedure ExtractTableObject(AObject: TObject);

    property CellPropertiesCollection: TdxRtfTableCellPropertiesCollection read FCellPropertiesCollection;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property ParentCellMap: TdxRtfParentCellMap read FParentCellMap;
    property State: TdxRtfTableReaderStateBase read FState;
    property TableStack: TdxObjectStack<TdxRtfTableState> read FTableStack;
  public
    constructor Create(AImporter: TObject);
    destructor Destroy; override;

    procedure ChangeState(ANewState: TdxRtfTableReaderStateBase); virtual;
    procedure InsertTables; virtual;
    procedure ResetState; virtual;

    procedure OnCellxProperty(AValue: Integer); virtual;
    procedure OnStartNestedTableProperties; virtual;

    procedure OnEndCell;
    procedure OnEndNestedCell;
    procedure OnEndNestedRow;
    procedure OnEndParagraph;
    procedure OnEndRow;
    procedure OnTableRowDefaults;

    property TableProperties: TdxRtfTableProperties read GetTableProperties;
    property RowProperties: TdxRtfTableRowProperties read GetRowProperties;
    property CellProperties: TdxRtfTableCellProperties read GetCellProperties;
    property ProcessedBorder: TdxBorderBase read FProcessedBorder write FProcessedBorder;
    property IsNestedTableProperetiesReading: Boolean read FIsNestedTableProperetiesReading;
    property Importer: TObject read FImporter;
    property TableController: TdxRtfTableController read FTableController;
    property Tables: TdxRtfTableCollection read FTables;
  end;

  { TdxRtfTableGrid }

  TdxRtfTableGrid = class(TdxIntegerList)
  protected
    function InnerBinarySearch(ARight: Integer): Integer;
  public
    function BinarySearchRight(ARight: Integer): Integer;
    function BinarySearchLeft(ARight: Integer): Integer;

    function First: Integer;
    function Last: Integer;
  end;

  { TdxRtfTableConverter }

  TdxRtfTableConverter = class
  private
    FTableReader: TdxRtfTableReader;
    FRtfCellMap: TDictionary<TdxRtfTableCell, TdxTableCell>;
    FTablesQueue: TdxRtfTableCollection;
    function GetPieceTable: TdxPieceTable;
  protected
    function IsOnlySingleCellInTable: Boolean; virtual;
    procedure ConvertTable(ARtfTable: TdxRtfTable);
    function RtfTableIsValid(ARtfTable: TdxRtfTable): Boolean;
    procedure PrepareRtfTable(ATable: TdxRtfTable); virtual;
    function ShouldUseFloatingPosition(ATable: TdxRtfTable): Boolean; virtual;
    function CalculateTableLeftOffset(ATable: TdxRtfTable): Integer; virtual;
    function CalculateTableIndent(ATable: TdxRtfTable): Integer; virtual;
    function GetCellLeftMargin(ATable: TdxRtfTable): TdxWidthUnit;
    function GetActualWidth(AUnitInfo: TdxWidthUnitInfo): Integer;
    procedure ConvertTableCore(ATable: TdxTable; ARtfTable: TdxRtfTable);
    function CalculateTableGrid(ATable: TdxRtfTable): TdxRtfTableGrid; virtual;
    procedure PrepareRtfRow(ARow: TdxRtfTableRow;
      AGrid: TdxRtfTableGrid; ATableLayoutType: TdxTableLayoutType); virtual;
    function CalculateWidthAfter(ARow: TdxRtfTableRow): Integer;
    function CalculateTotalRowWidth(ARow: TdxRtfTableRow): Integer;
    procedure PrepareRtfRowCells(ARow: TdxRtfTableRow; AGrid: TdxRtfTableGrid;
      AGridBefore: Integer; ATableLayoutType: TdxTableLayoutType);
    function CalculateEquidistantCellOrder(ACells: TdxRtfTableCellCollection;
      AIndex: Integer; ALeft: Integer): Integer;
    function CalculateRowColumnSpan(ARow: TdxRtfTableRow): Integer;
    procedure ConvertRow(ARow: TdxTableRow; ARtfRow: TdxRtfTableRow);
    procedure ConvertCell(ARow: TdxTableRow; ARtfCell: TdxRtfTableCell);
    procedure MergeCells(AFirstCell: TdxRtfTableCell);
    procedure RemoveCell(ACells: TdxRtfTableCellCollection; ACell: TdxRtfTableCell);
    procedure RecalcParagraphIndexes(ARemovedCell: TdxRtfTableCell);
    procedure RecalcParagraphIndexesInRow(ARow: TdxRtfTableRow; ACellIndex: Integer; ADelta: Integer);
    procedure RecalcParagraphIndexesInTable(ATable: TdxRtfTable; ARowIndex: Integer; ADelta: Integer; AParagraphIndex: TdxParagraphIndex);
    procedure RecalcParagraphIndexesInTables(ADelta: Integer; AParagraphIndex: TdxParagraphIndex);
  public
    constructor Create(ATableReader: TdxRtfTableReader);
    destructor Destroy; override;
    procedure ConvertTables(ARtfTables: TdxRtfTableCollection; AIsCopySingleCellAsText: Boolean);

    property TableReader: TdxRtfTableReader read FTableReader;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxRtfTableColumnsCalculator }

  TdxRtfTableColumnsCalculator = class
  protected
    function Merge(ASource: TdxRtfTableGrid; ADestination: TdxRtfTableGrid): TdxRtfTableGrid; virtual;
    function GetRowColumns(ARow: TdxRtfTableRow): TdxRtfTableGrid; virtual;
  public
    function Calculate(ATable: TdxRtfTable; ATableIndent: Integer): TdxRtfTableGrid;
  end;

implementation

uses
  Contnrs, Math,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.DocumentModel.Boxes;

type
  { TdxRtfTableReaderStateBaseHelper }

  TdxRtfTableReaderStateBaseHelper = class helper for TdxRtfTableReaderStateBase
  private
    function GetImporter: TdxRtfImporter;
  public
    property Importer: TdxRtfImporter read GetImporter;
  end;

  { TdxRtfTableReaderHelper }

  TdxRtfTableReaderHelper = class helper for TdxRtfTableReader
  private
    function GetImporter: TdxRtfImporter;
  public
    property Importer: TdxRtfImporter read GetImporter;
  end;

function AdjustValueByTolerance(const Value: Integer): Integer;
const
  Tolerance = 240 div 2;
begin
  Result := Round(Value / Tolerance) * Tolerance;
end;

{ TdxRtfTableReaderStateBaseHelper }

function TdxRtfTableReaderStateBaseHelper.GetImporter: TdxRtfImporter;
begin
  Result := TdxRtfImporter(inherited Importer);
end;

{ TdxRtfTableReaderHelper }

function TdxRtfTableReaderHelper.GetImporter: TdxRtfImporter;
begin
  Result := TdxRtfImporter(inherited Importer);
end;

{ TdxRtfTableCell }

constructor TdxRtfTableCell.Create(ARow: TdxRtfTableRow; AImporter: TObject; ARight: Integer);
var
  ARtfImporter: TdxRtfImporter;
begin
  inherited Create;
  FStartParagraphIndex := -1;
  FEndParagraphIndex := -1;
  FIndex := -1;
  FRight := ARight;
  FRow := ARow;
  ARtfImporter := TdxRtfImporter(AImporter);
  FCellProperties := TdxRtfTableCellProperties.Create(ARtfImporter.PieceTable, Self);
  TdxRtfImporter(AImporter).TableReader.AddTableObject(FCellProperties);
end;

function TdxRtfTableCell.GetActualRight: Integer;
begin
  Result := AdjustValueByTolerance(Right);
end;

function TdxRtfTableCell.GetIsEmpty: Boolean;
begin
  Result := (StartParagraphIndex = -1) or (EndParagraphIndex = -1);
end;

procedure TdxRtfTableCell.CopyFrom(ACell: TdxRtfTableCell);
begin
  Right := ACell.Right;
  Properties.CopyFrom(ACell.Properties);
end;

function TdxRtfTableCell.CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(TdxPieceTable(AProperties.PieceTable), AProperties);
end;

{ TdxRtfTableReaderStateBase }

constructor TdxRtfTableReaderStateBase.Create(AReader: TdxRtfTableReader);
begin
  inherited Create;
  FReader := AReader;
end;

function TdxRtfTableReaderStateBase.GetDocumentModel: TdxDocumentModel;
begin
  Result := Importer.DocumentModel;
end;

function TdxRtfTableReaderStateBase.GetImporter: TObject;
begin
  Result := Reader.Importer;
end;

function TdxRtfTableReaderStateBase.GetTableController: TdxRtfTableController;
begin
  Result := Reader.TableController;
end;

{ TdxTableRtfTableManagerState }

constructor TdxTableRtfTableManagerState.Create(AReader: TdxRtfTableReader);
begin
  inherited Create(AReader);
  Reader.TableController.CreateCurrentTable;
end;

procedure TdxTableRtfTableManagerState.OnEndParagraph(AParagraphFormattingInfo: TdxRtfParagraphFormattingInfo);
begin
  if (IsParagraphInTable(AParagraphFormattingInfo)) or (IsCurrentTableNotComplete) then
    OnEndInTableParagraph(AParagraphFormattingInfo.NestingLevel)
  else
  begin
    ValidateCurrentTable;
    Reader.ResetState;
  end;
end;

procedure TdxTableRtfTableManagerState.ValidateCurrentTable;
var
  ACurrentTable: TdxRtfTable;
  ALastRow: TdxRtfTableRow;
begin
  ACurrentTable := TableController.CurrentTable;
  if ACurrentTable.Rows.Count = 0 then
  begin
    Reader.Tables.Remove(ACurrentTable);
    Exit;
  end;
  ALastRow := ACurrentTable.Rows.Last;
  if ALastRow.Cells.Count > 0 then
    Exit;
  ACurrentTable.Rows.Remove(ALastRow);
  ValidateCurrentTable;
end;

procedure TdxTableRtfTableManagerState.OnEndInTableParagraph(ANestingLevel: Integer);
var
  ATableController: TdxRtfTableController;
  AIndex: TdxParagraphIndex;
begin
  ATableController := Reader.TableController;
  if ATableController.CurrentTable.NestingLevel <> ANestingLevel then
    ATableController.ChangeTable(ANestingLevel);
  AIndex := Importer.Position.ParagraphIndex;
  ATableController.RowController.CellController.SetParagraphIndex(AIndex);
end;

function TdxTableRtfTableManagerState.IsParagraphInTable(AParagraphFormattingInfo: TdxRtfParagraphFormattingInfo): Boolean;
begin
  Result := AParagraphFormattingInfo.InTableParagraph or (AParagraphFormattingInfo.NestingLevel > 0);
end;

function TdxTableRtfTableManagerState.IsCurrentTableNotComplete: Boolean;
begin
  Result := Reader.TableController.RowController.IsCurrentRowNotComplete or
    (Reader.TableController.RowController.IsCurrentRowValid and (Reader.TableController.RowController.CurrentRow.Cells.Count < Reader.CellPropertiesCollection.Count));
end;

procedure TdxTableRtfTableManagerState.OnEndRow;
begin
  if TableController.CurrentTable.NestingLevel > 1 then
    TdxRtfImporter.ThrowInvalidRtfFile;
  OnEndRowCore;
end;

procedure TdxTableRtfTableManagerState.OnEndRowCore;
var
  ARowController: TdxRtfTableRowController;
begin
  ARowController := Reader.TableController.RowController;
  ARowController.FinishRow;
  ARowController.StartNewRow;
end;

procedure TdxTableRtfTableManagerState.OnEndCell;
begin
  OnEndCellCore(1);
end;

procedure TdxTableRtfTableManagerState.OnEndCellCore(ANestingLevel: Integer);
var
  ACellController: TdxRtfTableCellController;
begin
  ACellController := Reader.TableController.RowController.CellController;
  OnEndInTableParagraph(ANestingLevel);
  ACellController.FinishCell;
  ACellController.StartNewCell;
end;

procedure TdxTableRtfTableManagerState.OnEndNestedRow;
begin
  if TableController.CurrentTable.NestingLevel = 1 then
    TdxRtfImporter.ThrowInvalidRtfFile;
  OnEndRowCore;
end;

procedure TdxTableRtfTableManagerState.OnEndNestedCell;
begin
  OnEndCellCore(Importer.Position.ParagraphFormattingInfo.NestingLevel);
end;

procedure TdxTableRtfTableManagerState.OnStartNestedTableProperties;
begin
//do hothing
end;

procedure TdxTableRtfTableManagerState.OnTableRowDefaults;
var
  ACurrentTable: TdxRtfTable;
begin
  ACurrentTable := Reader.TableController.CurrentTable;
  if (not Reader.IsNestedTableProperetiesReading) and (ACurrentTable <> nil) and (ACurrentTable.NestingLevel > 1) then
    TableController.ChangeTable(1);
end;

{ TdxRtfTableReader }

constructor TdxRtfTableReader.Create(AImporter: TObject);
begin
  inherited Create;
  Assert(AImporter is TdxRtfImporter);
  FImporter := AImporter;
  FTableObjectsCollection := TdxFastObjectList.Create;
  FState := TdxNoTableRtfTableReaderState.Create(Self);
  FTableStack := TdxObjectStack<TdxRtfTableState>.Create(True);
  FTables := TdxRtfTableCollection.Create;
  FTableController := CreateTableController;
  FParentCellMap := TdxRtfParentCellMap.Create([doOwnsValues]);
  ResetProperties;
end;

destructor TdxRtfTableReader.Destroy;
begin
  FreeAndNil(FTableStack);
  FreeAndNil(FParentCellMap);
  FreeAndNil(FTables);
  FreeAndNil(FTableController);
  FreeAndNil(FState);
  FreeAndNil(FTableObjectsCollection);
  inherited Destroy;
end;

procedure TdxRtfTableReader.ChangeState(ANewState: TdxRtfTableReaderStateBase);
begin
  FreeAndNil(FState);
  FState := ANewState;
end;

function TdxRtfTableReader.CreateTableController: TdxRtfTableController;
begin
  Result := TdxRtfTableController.Create(Self);
end;

procedure TdxRtfTableReader.OnCellxProperty(AValue: Integer);
begin
  CellProperties.Right := AValue;
  if (FCellPropertiesCollection.Count = 0) or (FCellPropertiesCollection.Last <> CellProperties) then
    FCellPropertiesCollection.Add(CellProperties);
  CreateCellProperties;
  ProcessedBorder := nil;
end;

procedure TdxRtfTableReader.OnEndCell;
begin
  State.OnEndCell;
end;

procedure TdxRtfTableReader.OnEndNestedCell;
begin
  State.OnEndNestedCell;
end;

procedure TdxRtfTableReader.OnEndNestedRow;
begin
  FIsNestedTableProperetiesReading := False;
  State.OnEndNestedRow;
end;

procedure TdxRtfTableReader.OnEndParagraph;
begin
  State.OnEndParagraph(Importer.Position.ParagraphFormattingInfo);
end;

procedure TdxRtfTableReader.OnEndRow;
begin
  State.OnEndRow;
end;

procedure TdxRtfTableReader.OnStartNestedTableProperties;
begin
  FIsNestedTableProperetiesReading := True;
  State.OnStartNestedTableProperties;
end;

procedure TdxRtfTableReader.OnTableRowDefaults;
begin
  State.OnTableRowDefaults;
  ResetProperties;
end;

procedure TdxRtfTableReader.ResetProperties;
begin
  FTableProperties := nil;
  FRowProperties := nil;
  FCellPropertiesCollection := TdxRtfTableCellPropertiesCollection.Create;
  AddTableObject(FCellPropertiesCollection);
  FCellProperties := nil;
  FProcessedBorder := nil;
end;

procedure TdxRtfTableReader.ResetState;
begin
  ChangeState(TdxNoTableRtfTableReaderState.Create(Self));
  TableController.Reset;
end;

procedure TdxRtfTableReader.RestoreProperties(AState: TdxRtfTableState);
begin
  FTableProperties := AState.TableProperties;
  FRowProperties := AState.RowProperties;
  FCellPropertiesCollection := AState.CellPropertiesCollection;
  if FCellPropertiesCollection.Count > 0 then
    FCellProperties := FCellPropertiesCollection.Last
  else
    CreateCellProperties;
end;

procedure TdxRtfTableReader.AddTableObject(AObject: TObject);
begin
  FTableObjectsCollection.Add(AObject);
end;

procedure TdxRtfTableReader.ExtractTableObject(AObject: TObject);
begin
  FTAbleObjectsCollection.Extract(AObject);
end;

procedure TdxRtfTableReader.CreateCellProperties;
begin
  FCellProperties := TdxRtfTableCellProperties.Create(Importer.PieceTable, Self);
  AddTableObject(FCellProperties);
end;

function TdxRtfTableReader.CreateCellPropertiesChangedHistoryItem(
  AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(AProperties.PieceTable as TdxPieceTable, AProperties);
end;

function TdxRtfTableReader.GetCellProperties: TdxRtfTableCellProperties;
begin
  if FCellProperties = nil then
    CreateCellProperties;
  Result := FCellProperties;
end;

function TdxRtfTableReader.GetDocumentModel: TdxDocumentModel;
begin
  Result := Importer.DocumentModel;
end;

function TdxRtfTableReader.GetRowProperties: TdxRtfTableRowProperties;
begin
  if FRowProperties = nil then
  begin
    FRowProperties := TdxRtfTableRowProperties.Create(Importer.PieceTable);
    AddTableObject(FRowProperties);
  end;
  Result := FRowProperties;
end;

function TdxRtfTableReader.GetTableProperties: TdxRtfTableProperties;
begin
  if FTableProperties = nil then
  begin
    FTableProperties := TdxRtfTableProperties.Create(Importer.PieceTable);
    AddTableObject(FTableProperties);
  end;
  Result := FTableProperties;
end;

procedure TdxRtfTableReader.InsertTables;
var
  AConverter: TdxRtfTableConverter;
begin
  if Tables.Count = 0 then
    Exit;
  if DocumentModel.DocumentCapabilities.TablesAllowed then
  begin
    AConverter := TdxRtfTableConverter.Create(Self);
    try
      AConverter.ConvertTables(Tables, Importer.Options.CopySingleCellAsText);
    finally
      AConverter.Free;
    end;
  end;
  Tables.Clear;
end;

{ TdxRtfTableController }

constructor TdxRtfTableController.Create(AReader: TdxRtfTableReader);
begin
  inherited Create;
  FReader := AReader;
  FRowController := CreateRowController;
end;

destructor TdxRtfTableController.Destroy;
begin
  FreeAndNil(FRowController);
  inherited;
end;

procedure TdxRtfTableController.ChangeTable(ANestingLevel: Integer);
var
  ADepth: Integer;
begin
  ADepth := ANestingLevel - CurrentTable.NestingLevel;
  if ADepth > 0 then
    CreateNestedTable(ADepth)
  else
    if ADepth < 0 then
      PopParentTable(ADepth)
    else
    begin
      FinishTable;
      CreateCurrentTable;
    end;
end;

procedure TdxRtfTableController.CreateCurrentTable;
var
  ACurrentCell: TdxRtfTableCell;
  ATables: TdxRtfTableCollection;
begin
  FCurrentTable := TdxRtfTable.Create(Reader.Importer);
  Reader.AddTableObject(FCurrentTable);
  ACurrentCell := RowController.CellController.CurrentCell;
  CurrentTable.ParentCell := ACurrentCell;
  if ACurrentCell <> nil then
  begin
    if not Reader.ParentCellMap.TryGetValue(ACurrentCell, ATables) then
    begin
      ATables := TdxRtfTableCollection.Create;
      Reader.ParentCellMap.Add(ACurrentCell, ATables);
    end;
    ATables.Add(CurrentTable);
  end;
  Reader.Tables.Add(CurrentTable);
  RowController.StartNewRow;
end;

procedure TdxRtfTableController.CreateNestedTable(ADepth: Integer);
var
  I: Integer;
begin
  for I := 0 to ADepth - 1 do
  begin
    FinishTable;
    Reader.TableStack.Push(TdxRtfTableState.Create(CurrentTable, Reader));
    CreateCurrentTable;
  end;
end;

function TdxRtfTableController.CreateRowController: TdxRtfTableRowController;
begin
  Result := TdxRtfTableRowController.Create(Self);
end;

procedure TdxRtfTableController.FinishTable;
begin
  RowController.CellController.FinishCell;
  RowController.FinishRowCore;
end;

procedure TdxRtfTableController.PopParentTable(ADepth: Integer);
var
  ACount: Integer;
  AState: TdxRtfTableState;
  I: Integer;
begin
  ACount := Abs(ADepth);
  AState := nil;
  I := 0;
  while (I < ACount) and (Reader.TableStack.Count > 0) do
  begin
    AState := Reader.TableStack.Extract;
    Inc(I);
  end;
  if AState = nil then
    Exit;
  try
    FCurrentTable := AState.Table;
    Reader.RestoreProperties(AState);
    RowController.AssignLastRowAsCurrent;
  finally
    AState.Free;
  end;
end;

procedure TdxRtfTableController.Reset;
begin
  FCurrentTable := nil;
  RowController.Reset;
end;

{ TdxRtfTable }

constructor TdxRtfTable.Create(AImporter: TObject; AParentCell: TdxRtfTableCell);
var
  ARtfImporter: TdxRtfImporter;
begin
  inherited Create;
  ARtfImporter := TdxRtfImporter(AImporter);
  FTableProperties := TdxRtfTableProperties.Create(ARtfImporter.PieceTable);
  FRows := TdxRtfTableRowCollection.Create;
  FParentCell := AParentCell;
end;

destructor TdxRtfTable.Destroy;
begin
  FreeAndNil(FTableProperties);
  FreeAndNil(FRows);
  inherited Destroy;
end;

procedure TdxRtfTable.SetRight(const AValue: Integer);
begin
  if AValue >= 0 then
    FRight := AValue;
end;

function TdxRtfTable.GetNestedLevel: Integer;
var
  AParentCell: TdxRtfTableCell;
  AParentTable: TdxRtfTable;
begin
  Result := 1;
  AParentCell := ParentCell;
  while AParentCell <> nil do
  begin
    AParentTable := AParentCell.Row.Table;
    AParentCell := AParentTable.ParentCell;
    Inc(Result);
  end;
end;

function TdxRtfTable.GetNestingLevel: Integer;
begin
  Result := GetNestedLevel;
end;

{ TdxRtfTableProperties }

procedure TdxRtfTableProperties.CopyFrom(ANewProperties: TdxTableProperties);
var
  ASourceProperties: TdxRtfTableProperties;
begin
  inherited CopyFrom(ANewProperties);
  ASourceProperties := Safe<TdxRtfTableProperties>.Cast(ANewProperties);
  if (ASourceProperties <> nil) and ASourceProperties.UseHalfSpace then
    HalfSpace := ASourceProperties.HalfSpace;
end;

constructor TdxRtfTableProperties.Create(APieceTable: TdxPieceTable);
begin
  inherited Create(APieceTable);
  FCellSpacing := TdxRtfCellSpacing.Create(inherited CellSpacing);
  TableLayout := TdxTableLayoutType.Fixed;
end;

destructor TdxRtfTableProperties.Destroy;
begin
  FreeAndNil(FCellSpacing);
  inherited Destroy;
end;

function TdxRtfTableProperties.IsChanged: Boolean;
begin
  Result := UseHalfSpace or UseLeftMargin or UseRightMargin or
    UseTopMargin or UseBottomMargin or UseCellSpacing or UseIsTableOverlap or
    UsePreferredWidth or UseTableIndent or
    (TableLayout = TdxTableLayoutType.Autofit) or
    UseTableLook or UseTableStyleColBandSize or UseTableStyleRowBandSize or
    Borders.UseBottomBorder or Borders.UseInsideHorizontalBorder or
    Borders.UseInsideVerticalBorder or Borders.UseLeftBorder or Borders.UseRightBorder or
    Borders.UseTopBorder;
end;

procedure TdxRtfTableProperties.SetHalfSpace(const AValue: Integer);
begin
  FHalfSpace := AValue;
  FUseHalfSpace := True;
end;

{ TdxRtfTableRowController }

constructor TdxRtfTableRowController.Create(
  ATableController: TdxRtfTableController);
begin
  inherited Create;
  FTableController := ATableController;
  FCellController := CreateCellController;
end;

destructor TdxRtfTableRowController.Destroy;
begin
  FreeAndNil(FCellController);
  inherited Destroy;
end;

function TdxRtfTableRowController.GetReader: TdxRtfTableReader;
begin
  Result := TableController.Reader;
end;

procedure TdxRtfTableRowController.AssignCellProperties;
var
  AReader: TdxRtfTableReader;
  ACellPropertiesCount, ACellCount, I: Integer;
  ACells: TdxRtfTableCellCollection;
  ACell: TdxRtfTableCell;
  AProperties: TdxRtfTableCellProperties;
begin
  AReader := TableController.Reader;
  ACellPropertiesCount := AReader.CellPropertiesCollection.Count;
  if ACellPropertiesCount = 0 then
    TdxRtfImporter.ThrowInvalidRtfFile;

  ACells := FCurrentRow.Cells;
  while (ACells.Count > ACellPropertiesCount) and (ACells.Count > 1) do
  begin
    ACells[1].StartParagraphIndex := ACells[0].StartParagraphIndex;
    ACells.Delete(0);
  end;
  ACellCount := ACells.Count;
  for I := 0 to ACellCount - 1 do
  begin
    ACell := FCurrentRow.Cells[I];
    AProperties := AReader.CellPropertiesCollection[I];

    ACell.Right := AProperties.Right;
    ACell.Properties.HorizontalMerging := AProperties.HorizontalMerging;
    ACell.Properties.CopyFrom(AProperties);
  end;
end;

procedure TdxRtfTableRowController.AssignLastRowAsCurrent;
begin
  FCurrentRow := TableController.CurrentTable.Rows.Last;
  CellController.AssignLastCellAsCurrent;
end;

procedure TdxRtfTableRowController.AssignRowProperties;
var
  ATableProperties: TdxRtfTableProperties;
begin
  ATableProperties := TableController.CurrentTable.Properties;
  if not ATableProperties.IsChanged then
  begin
    ATableProperties.CopyFrom(TableController.Reader.TableProperties);
    ATableProperties.FloatingPosition.CopyFrom(TableController.Reader.TableProperties.FloatingPosition);
  end;
  ATableProperties.TableStyleIndex := TableController.Reader.TableProperties.TableStyleIndex;
  CurrentRow.Properties.CopyFrom(TableController.Reader.RowProperties);
  CurrentRow.Properties.FloatingPosition.CopyFrom(TableController.Reader.RowProperties.FloatingPosition);
  CurrentRow.Left := TableController.Reader.RowProperties.Left;
  AssignCellProperties;
end;

procedure TdxRtfTableRowController.FinishRow;
begin
  FinishRowCore;
  AssignRowProperties;
end;

procedure TdxRtfTableRowController.FinishRowCore;
var
  ARows: TdxRtfTableRowCollection;
begin
  ARows := TableController.CurrentTable.Rows;
  if (ARows.Count = 0) or (ARows.Last <> CurrentRow) then
  begin
    CurrentRow.Index := ARows.Count;
    ARows.Add(CurrentRow);
  end;
end;

function TdxRtfTableRowController.IsCurrentRowNotComplete: Boolean;
begin
  Result := not TableController.CurrentTable.Rows.Contains(CurrentRow) and
    ((CurrentRow.Cells.Count > 0) or CellController.IsCurrentCellNotComplete);
end;

function TdxRtfTableRowController.IsCurrentRowValid: Boolean;
begin
  Result := TableController.CurrentTable.Rows.Contains(CurrentRow) and (CurrentRow.Cells.Count > 0);
end;

procedure TdxRtfTableRowController.Reset;
begin
  FCurrentRow := nil;
  CellController.Reset;
end;

procedure TdxRtfTableRowController.StartNewRow;
var
  ATable: TdxRtfTable;
begin
  ATable := FTableController.CurrentTable;
  FCurrentRow := TdxRtfTableRow.Create(ATable, TableController.Reader.Importer);
  Reader.AddTableObject(FCurrentRow);
  CellController.StartNewCell;
end;

function TdxRtfTableRowController.CreateCellController: TdxRtfTableCellController;
begin
  Result := TdxRtfTableCellController.Create(Self);
end;

{ TdxRtfTableCellController }

constructor TdxRtfTableCellController.Create(
  ARowController: TdxRtfTableRowController);
begin
  inherited Create;
  FRowController := ARowController;
end;

procedure TdxRtfTableCellController.AssignLastCellAsCurrent;
begin
  FCurrentCell := RowController.CurrentRow.Cells.Last;
end;

procedure TdxRtfTableCellController.FinishCell;
var
  ACells: TdxRtfTableCellCollection;
begin
  ACells := RowController.CurrentRow.Cells;
  if (ACells.Count = 0) or (ACells.Last <> CurrentCell) then
  begin
    CurrentCell.Index := ACells.Count;
    ACells.Add(CurrentCell);
  end;
end;

function TdxRtfTableCellController.IsCurrentCellNotComplete: Boolean;
begin
  Result := not RowController.CurrentRow.Cells.Contains(CurrentCell) and not CurrentCell.IsEmpty;
end;

procedure TdxRtfTableCellController.Reset;
begin
  FCurrentCell := nil;
end;

procedure TdxRtfTableCellController.SetParagraphIndex(AParagraphIndex: Integer);
begin
  SetParagraphIndexCore(CurrentCell, AParagraphIndex);
  SetParagraphIndexesToParentCell(AParagraphIndex);
end;

procedure TdxRtfTableCellController.SetParagraphIndexCore(
  ACell: TdxRtfTableCell; AParagraphIndex: Integer);
begin
  if ACell.StartParagraphIndex < 0 then
    ACell.StartParagraphIndex := AParagraphIndex;
  ACell.EndParagraphIndex := AParagraphIndex;
end;

procedure TdxRtfTableCellController.SetParagraphIndexesToParentCell(
  AParagraphIndex: Integer);
var
  AParentCell: TdxRtfTableCell;
begin
  AParentCell := RowController.TableController.CurrentTable.ParentCell;
  while AParentCell <> nil do
  begin
    SetParagraphIndexCore(AParentCell, AParagraphIndex);
    AParentCell := AParentCell.Row.Table.ParentCell;
  end;
end;

procedure TdxRtfTableCellController.StartNewCell;
var
  ARow: TdxRtfTableRow;
begin
  ARow := RowController.CurrentRow;
  FCurrentCell := TdxRtfTableCell.Create(ARow, RowController.TableController.Reader.Importer);
  Reader.AddTableObject(FCurrentCell);
end;

function TdxRtfTableCellController.GetReader: TdxRtfTableReader;
begin
  Result := RowController.Reader;
end;

{ TdxNoTableRtfTableReaderState }

function TdxNoTableRtfTableReaderState.ChangeState: TdxTableRtfTableManagerState;
begin
  Result := TdxTableRtfTableManagerState.Create(Reader);
  Reader.ChangeState(Result);
end;

procedure TdxNoTableRtfTableReaderState.OnEndCell;
var
  ANewState: TdxTableRtfTableManagerState;
begin
  ANewState := ChangeState;
  ANewState.OnEndCell;
end;

procedure TdxNoTableRtfTableReaderState.OnEndNestedCell;
var
  ANewState: TdxTableRtfTableManagerState;
begin
  ANewState := ChangeState;
  ANewState.OnEndNestedCell;
end;

procedure TdxNoTableRtfTableReaderState.OnEndNestedRow;
begin
  Importer.ThrowInvalidRtfFile;
end;

procedure TdxNoTableRtfTableReaderState.OnEndParagraph(
  AParagraphFormattingInfo: TdxRtfParagraphFormattingInfo);
var
  AInfo: TdxRtfParagraphFormattingInfo;
  ANewState: TdxTableRtfTableManagerState;
begin
  AInfo := AParagraphFormattingInfo;
  if not AInfo.InTableParagraph then
    Exit;
  ANewState := ChangeState;
  ANewState.OnEndParagraph(AParagraphFormattingInfo);
end;

procedure TdxNoTableRtfTableReaderState.OnEndRow;
begin
  Importer.ThrowInvalidRtfFile;
end;

procedure TdxNoTableRtfTableReaderState.OnStartNestedTableProperties;
begin
  Importer.ThrowInvalidRtfFile;
end;

procedure TdxNoTableRtfTableReaderState.OnTableRowDefaults;
begin
//do nothing
end;

{ TdxRtfTableState }

constructor TdxRtfTableState.Create(ATable: TdxRtfTable;
  AReader: TdxRtfTableReader);
begin
  inherited Create;
  FTable := ATable;
  FTableProperties := AReader.TableProperties;
  FRowProperties := AReader.RowProperties;
  FCellPropertiesCollection := AReader.CellPropertiesCollection;
end;

{ TdxRtfTableRowProperties }

constructor TdxRtfTableRowProperties.Create(APieceTable: TdxPieceTable);
begin
  inherited Create(APieceTable);
  FFloatingPosition := APieceTable.DocumentModel.Cache.TableFloatingPositionInfoCache.DefaultItem.Clone;
  FFloatingPosition.HorizontalAnchor := TdxHorizontalAnchorTypes.Column;
  FCellSpacing := TdxRtfCellSpacing.Create(inherited CellSpacing);
end;

destructor TdxRtfTableRowProperties.Destroy;
begin
  FreeAndNil(FFloatingPosition);
  FreeAndNil(FCellSpacing);
  inherited Destroy;
end;

{ TdxRtfCellSpacing }

constructor TdxRtfCellSpacing.Create(ACellSpacing: TdxWidthUnit);
begin
  inherited Create;
  FCellSpacing := ACellSpacing;
end;

{ TdxRtfTableRow }

constructor TdxRtfTableRow.Create(ATable: TdxRtfTable; AImporter: TObject);
begin
  inherited Create;
  FIndex := -1;
  FRowProperties := TdxRtfTableRowProperties.Create(TdxRtfImporter(AImporter).PieceTable);
  TdxRtfImporter(AImporter).TableReader.AddTableObject(FRowProperties);
  FTable := ATable;
  FCells := TdxRtfTableCellCollection.Create;
end;

destructor TdxRtfTableRow.Destroy;
begin
  FreeAndNil(FCells);
  inherited Destroy;
end;

function TdxRtfTableRow.GetActualLeft: Integer;
begin
  Result := AdjustValueByTolerance(Left);
end;

function TdxRtfTableRow.GetRight: Integer;
var
  ACellCount: Integer;
begin
  ACellCount := Cells.Count;
  if ACellCount > 0 then
    Result := Cells[ACellCount - 1].ActualRight
  else
    Result := 0;
end;

procedure TdxRtfTableRow.CopyFrom(ARow: TdxRtfTableRow);
var
  ACellCount, I: Integer;
begin
  Offset := ARow.Offset;
  Left := ARow.Left;
  GridBefore := ARow.GridBefore;
  Properties.CopyFrom(ARow.Properties);
  ACellCount := Min(ARow.Cells.Count, Cells.Count);
  for I := 0 to ACellCount - 1 do
    Cells[I].CopyFrom(ARow.Cells[I]);
end;

{ TdxRtfTableCustomItemCollection }

function TdxRtfTableCustomItemCollection<T>.First: T;
begin
  if Count > 0 then
    Result := inherited First
  else
    Result := Default(T);
end;

function TdxRtfTableCustomItemCollection<T>.Last: T;
begin
  if Count > 0 then
    Result := inherited Last
  else
    Result := Default(T);
end;

{ TdxRtfTableGrid }

function TdxRtfTableGrid.First: Integer;
begin
  if Count > 0 then
    Result := Self[0]
  else
    Result :=  -1;
end;

function TdxRtfTableGrid.InnerBinarySearch(ARight: Integer): Integer;
begin
  if not Self.BinarySearch(ARight, Result) then
    Result := not Result;
end;

function TdxRtfTableGrid.Last: Integer;
begin
  if Count > 0 then
    Result := Self[Count - 1]
  else
    Result := -1;
end;

function TdxRtfTableGrid.BinarySearchRight(ARight: Integer): Integer;
var
  AIndex, ANextIndex: Integer;
begin
  AIndex := InnerBinarySearch(ARight);
  if AIndex <= 0 then
    Exit(AIndex);
  ANextIndex := AIndex + 1;
  while (ANextIndex < Count) and (Self[ANextIndex] = Self[AIndex]) do
  begin
    AIndex := ANextIndex;
    Inc(ANextIndex);
  end;
  Result := AIndex;
end;

function TdxRtfTableGrid.BinarySearchLeft(ARight: Integer): Integer;
var
  AIndex, APrevIndex: Integer;
begin
  AIndex := InnerBinarySearch(ARight);
  if AIndex <= 0 then
    Exit(AIndex);
  APrevIndex := AIndex - 1;
  while (APrevIndex >= 0) and (Self[APrevIndex] = Self[AIndex]) do
  begin
    AIndex := APrevIndex;
    Dec(APrevIndex);
  end;
  Result := AIndex;
end;

{ TdxRtfTableConverter }

constructor TdxRtfTableConverter.Create(ATableReader: TdxRtfTableReader);
begin
  inherited Create;
  FTableReader := ATableReader;
  FRtfCellMap := TDictionary<TdxRtfTableCell, TdxTableCell>.Create;
end;

destructor TdxRtfTableConverter.Destroy;
begin
  FreeAndNil(FRtfCellMap);
  inherited Destroy;
end;

function TdxRtfTableConverter.GetPieceTable: TdxPieceTable;
begin
  Result := TableReader.Importer.PieceTable;
end;

procedure TdxRtfTableConverter.ConvertTables(ARtfTables: TdxRtfTableCollection; AIsCopySingleCellAsText: Boolean);
var
  ATable: TdxRtfTable;
begin
  FRtfCellMap.Clear;
  FTablesQueue := TdxRtfTableCollection.Create;
  try
    FTablesQueue.AddRange(ARtfTables);
    if not (IsOnlySingleCellInTable and AIsCopySingleCellAsText) then
    begin
      while FTablesQueue.Count > 0 do
      begin
        ATable := FTablesQueue.First;
        FTablesQueue.Extract(ATable);
        ConvertTable(ATable);
      end;
    end;
  finally
    FTablesQueue.Free;
  end;
end;

function TdxRtfTableConverter.IsOnlySingleCellInTable: Boolean;
var
  ATable: TdxRtfTable;
  AFirstRow: TdxRtfTableRow;
  ACell: TdxRtfTableCell;
begin
  if FTablesQueue.Count > 1 then
    Exit(False);
  ATable := FTablesQueue.First;
  if (ATable.Rows.Count > 1) or (ATable.Rows.Count = 0) then
    Exit(False);
  AFirstRow := ATable.Rows[0];
  if AFirstRow.Cells.Count > 1 then
    Exit(False);
  ACell := AFirstRow.Cells[0];
  if (ACell.StartParagraphIndex <> 0) or (ACell.EndParagraphIndex <> PieceTable.Paragraphs.Count - 2) then
    Exit(False);
  Result := True;
end;

procedure TdxRtfTableConverter.ConvertTable(ARtfTable: TdxRtfTable);
var
  AParentCell: TdxTableCell;
  ATable: TdxTable;
begin
  if not RtfTableIsValid(ARtfTable) then
    Exit;
  AParentCell := nil;
  if ARtfTable.ParentCell <> nil then
  begin
    AParentCell := FRtfCellMap[ARtfTable.ParentCell];
  end;
  PrepareRtfTable(ARtfTable);
  ATable := TdxTable.Create(PieceTable, AParentCell, 0, 0);
  PieceTable.Tables.Add(ATable);
  ConvertTableCore(ATable, ARtfTable);
end;

function TdxRtfTableConverter.RtfTableIsValid(ARtfTable: TdxRtfTable): Boolean;
var
  ALastRow: TdxRtfTableRow;
begin
  if ARtfTable.Rows.Count = 0 then
    Exit(False);
  ALastRow := ARtfTable.Rows.Last;
  if ALastRow.Cells.Count > 0 then
    Exit(True);
  ARtfTable.Rows.Remove(ALastRow);
  Result := RtfTableIsValid(ARtfTable);
end;

procedure TdxRtfTableConverter.PrepareRtfTable(ATable: TdxRtfTable);
var
  ARow: TdxRtfTableRow;
  ATablePreferredWidth, AIndent: TdxWidthUnit;
  AMargin: Integer;
begin
  if ShouldUseFloatingPosition(ATable) then
  begin
    ARow := ATable.Rows.First;
    ATable.Properties.FloatingPosition.CopyFrom(ARow.Properties.FloatingPosition);
  end;
  ATablePreferredWidth := ATable.Properties.PreferredWidth;
  if ATablePreferredWidth.&Type = TdxWidthUnitType.Nil then
  begin
    ATablePreferredWidth.&Type := TdxWidthUnitType.Auto;
    ATablePreferredWidth.Value := 0;
  end;
  if (not ATable.Properties.UseRightMargin and not ATable.Properties.UseLeftMargin) and ATable.Properties.UseHalfSpace then
  begin
    AMargin := ATable.Properties.HalfSpace;
    ATable.Properties.CellMargins.Left.&Type := TdxWidthUnitType.ModelUnits;
    ATable.Properties.CellMargins.Left.Value := AMargin;
    ATable.Properties.CellMargins.Right.&Type := TdxWidthUnitType.ModelUnits;
    ATable.Properties.CellMargins.Right.Value := AMargin;
  end;
  ATable.Indent := CalculateTableLeftOffset(ATable);
  AIndent := ATable.Properties.TableIndent;
  if AIndent.&Type <> TdxWidthUnitType.ModelUnits then
  begin
    AIndent.Value := CalculateTableIndent(ATable);
    AIndent.&Type := TdxWidthUnitType.ModelUnits;
  end;
end;

function TdxRtfTableConverter.ShouldUseFloatingPosition(ATable: TdxRtfTable): Boolean;
var
  APrevious: TdxTableFloatingPositionInfo;
  ARow: TdxRtfTableRow;
  ACurrent: TdxTableFloatingPositionInfo;
  I: Integer;
begin
  Result := True;
  APrevious := nil;
  for I := 0 to ATable.Rows.Count - 1 do
  begin
    ARow := ATable.Rows[I];
    ACurrent := ARow.Properties.FloatingPosition;
    if APrevious <> nil then
      Result := ACurrent.Equals(APrevious) and Result;
    APrevious := ACurrent;
  end;
end;

function TdxRtfTableConverter.CalculateTableLeftOffset(ATable: TdxRtfTable): Integer;
var
  ARows: TdxRtfTableRowCollection;
  ARowsCount, I: Integer;
begin
  ARows := ATable.Rows;
  Result := ARows[0].Left;
  ARowsCount := ARows.Count;
  for I := 1 to ARowsCount - 1 do
    Result := Min(Result, ARows[I].Left);
end;

function TdxRtfTableConverter.CalculateTableIndent(ATable: TdxRtfTable): Integer;
var
  ACellProperties: TdxRtfTableCellProperties;
  ALeftBorder: TdxBorderBase;
  ALeftMargin: TdxWidthUnit;
  ACalculator: TdxTableBorderCalculator;
  ABorderWidth: Integer;
begin
  ACellProperties := ATable.Rows.First.Cells.First.Properties;
  ALeftBorder := ACellProperties.Borders.LeftBorder;
  ALeftMargin := GetCellLeftMargin(ATable);
  ACalculator := TdxTableBorderCalculator.Create;
  try
    ABorderWidth := ACalculator.GetActualWidth(ALeftBorder);
    Result := Max(ABorderWidth div 2, GetActualWidth(ALeftMargin.Info)) + ATable.Indent;
  finally
    ACalculator.Free;
  end;
end;

function TdxRtfTableConverter.GetCellLeftMargin(ATable: TdxRtfTable): TdxWidthUnit;
var
  ACellProperties: TdxRtfTableCellProperties;
begin
  ACellProperties := ATable.Rows.First.Cells.First.Properties;
  if ACellProperties.CellMargins.Left.Info.&Type = TdxWidthUnitType.ModelUnits then
    Exit(ACellProperties.CellMargins.Left);
  Result := ATable.Properties.CellMargins.Left;
end;

function TdxRtfTableConverter.GetActualWidth(AUnitInfo: TdxWidthUnitInfo): Integer;
begin
  Result := 0;
  if AUnitInfo.&Type = TdxWidthUnitType.ModelUnits then
    Exit(AUnitInfo.Value);
end;

procedure TdxRtfTableConverter.ConvertTableCore(ATable: TdxTable; ARtfTable: TdxRtfTable);
var
  ATableGrid: TdxRtfTableGrid;
  ARows: TdxTableRowCollection;
  ARtfRows: TdxRtfTableRowCollection;
  ACount, I: Integer;
  ATableLayoutType: TdxTableLayoutType;
  ARow: TdxTableRow;
begin
  ATable.TableProperties.CopyFrom(ARtfTable.Properties);
  ATable.SetTableStyleIndexCore(ARtfTable.Properties.TableStyleIndex);
  ATable.TableProperties.FloatingPosition.CopyFrom(ARtfTable.Properties.FloatingPosition);
  ATableGrid := CalculateTableGrid(ARtfTable);
  try
    ARows := ATable.Rows;
    ARtfRows := ARtfTable.Rows;
    ACount := ARtfRows.Count;
    ATableLayoutType := ATable.TableLayout;
    for I := 0 to ACount - 1 do
    begin
      PrepareRtfRow(ARtfRows[I], ATableGrid, ATableLayoutType);
      ARow := TdxTableRow.Create(ATable);
      ARows.AddInternal(ARow);
      ConvertRow(ARow, ARtfRows[I]);
    end;
  finally
    ATableGrid.Free;
  end;
end;

function TdxRtfTableConverter.CalculateTableGrid(ATable: TdxRtfTable): TdxRtfTableGrid;
var
  ACalculator: TdxRtfTableColumnsCalculator;
begin
  ACalculator := TdxRtfTableColumnsCalculator.Create;
  try
    Result := ACalculator.Calculate(ATable, ATable.Indent);
  finally
    ACalculator.Free;
  end;
end;

procedure TdxRtfTableConverter.PrepareRtfRow(ARow: TdxRtfTableRow; AGrid: TdxRtfTableGrid; ATableLayoutType: TdxTableLayoutType);
var
  AGridBefore, ALastColumnIndex, AGridAfter, AWidthAfter: Integer;
begin
  AGridBefore := AGrid.BinarySearchLeft(ARow.ActualLeft);
  ARow.Properties.GridBefore := AGridBefore;
  if (ARow.Properties.WidthBefore.Value = 0) and (AGridBefore > 0) then
  begin
    ARow.Properties.WidthBefore.Value := ARow.Offset;
    ARow.Properties.WidthBefore.&Type := TdxWidthUnitType.ModelUnits;
  end;
  PrepareRtfRowCells(ARow, AGrid, AGridBefore, ATableLayoutType);
  ALastColumnIndex := AGrid.Count - 1;
  AGridAfter := Max(0, ALastColumnIndex - CalculateRowColumnSpan(ARow) - AGridBefore);
  ARow.Properties.GridAfter := AGridAfter;
  if (ARow.Properties.WidthAfter.Value = 0) and (AGridAfter > 0) then
  begin
    if ARow.Properties.WidthAfter.&Type = TdxWidthUnitType.ModelUnits then
      AWidthAfter := CalculateWidthAfter(ARow)
    else
      AWidthAfter := AGrid[ALastColumnIndex] - AGrid[ALastColumnIndex - AGridAfter];
    ARow.Properties.WidthAfter.Value := AWidthAfter;
    ARow.Properties.WidthAfter.&Type := TdxWidthUnitType.ModelUnits;
  end;
end;

function TdxRtfTableConverter.CalculateWidthAfter(ARow: TdxRtfTableRow): Integer;
var
  ARowWidth, AMaxWidth, I: Integer;
  ARows: TdxRtfTableRowCollection;
  ACurrentRow: TdxRtfTableRow;
  ACurrentRowWidth: Integer;
begin
  ARowWidth := CalculateTotalRowWidth(ARow);
  if ARowWidth < 0 then
    Exit(1);
  ARows := ARow.Table.Rows;
  AMaxWidth := 0;
  for I := 0 to ARows.Count - 1 do
  begin
    ACurrentRow := ARows[I];
    if ACurrentRow = ARow then
      Continue;
    ACurrentRowWidth := CalculateTotalRowWidth(ACurrentRow);
    if ACurrentRowWidth > 0 then
      AMaxWidth := Max(AMaxWidth, ACurrentRowWidth);
  end;
  Result := Max(1, ARowWidth - AMaxWidth);
end;

function TdxRtfTableConverter.CalculateTotalRowWidth(ARow: TdxRtfTableRow): Integer;
var
  ATotalWidth, ACount, I: Integer;
  ACells: TdxRtfTableCellCollection;
  AWidth: TdxPreferredWidth;
begin
  ATotalWidth := 0;
  ACells := ARow.Cells;
  ACount := ACells.Count;
  for I := 0 to ACount - 1 do
  begin
    AWidth := ACells[I].Properties.PreferredWidth;
    if AWidth.&Type = TdxWidthUnitType.ModelUnits then
      Inc(ATotalWidth, AWidth.Value)
    else
      Exit(-1);
  end;
  Result := ATotalWidth;
end;

procedure TdxRtfTableConverter.PrepareRtfRowCells(ARow: TdxRtfTableRow;
  AGrid: TdxRtfTableGrid; AGridBefore: Integer; ATableLayoutType: TdxTableLayoutType);
var
  APrevBorderIndex, ALeft, I, J: Integer;
  ACell: TdxRtfTableCell;
  ABorderIndex, AColumnSpan: Integer;
  APreferredWidth: TdxWidthUnit;
  ARight, ACellRight: Integer;
begin
  APrevBorderIndex := AGridBefore;
  ALeft := AGrid[AGridBefore];
  for I := 0 to ARow.Cells.Count - 1 do
  begin
    ACell := ARow.Cells[I];
    ARight := IfThen(ACell.ActualRight > ALeft, ACell.ActualRight, ALeft);
    ABorderIndex := AGrid.BinarySearchRight(ARight) - CalculateEquidistantCellOrder(ARow.Cells, I, ARight);
    AColumnSpan := Max(1, ABorderIndex - APrevBorderIndex);
    ACell.Properties.ColumnSpan := AColumnSpan;
    APreferredWidth := ACell.Properties.PreferredWidth;
    if (APreferredWidth.&Type = TdxWidthUnitType.Nil) or
      ((APreferredWidth.&Type = TdxWidthUnitType.Auto) and (ATableLayoutType <> TdxTableLayoutType.Autofit)) then
    begin
      APreferredWidth.Value := ACell.Right;

      if I = 0 then
        APreferredWidth.Value := APreferredWidth.Value - ARow.Left
      else
      begin
        ACellRight := 0;
        for J := 0 to I - 1 do
          ACellRight := Max(ARow.Cells[J].Right, ACellRight);
        APreferredWidth.Value := APreferredWidth.Value - ACellRight;
      end;
      APreferredWidth.Value := Max(0, APreferredWidth.Value);

      APreferredWidth.&Type := TdxWidthUnitType.ModelUnits;
    end;
    APrevBorderIndex := Math.Max(ABorderIndex, APrevBorderIndex);
    ALeft := ARight;
  end;
end;

function TdxRtfTableConverter.CalculateEquidistantCellOrder(ACells: TdxRtfTableCellCollection; AIndex: Integer; ALeft: Integer): Integer;
var
  ACount, I: Integer;
begin
  Result := 0;
  ACount := ACells.Count;
  for I := AIndex + 1 to ACount - 1 do
  begin
    if ACells[I].ActualRight > ALeft then
      Break;
    Inc(Result);
  end;
end;

function TdxRtfTableConverter.CalculateRowColumnSpan(ARow: TdxRtfTableRow): Integer;
var
  ACellsCount, I: Integer;
begin
  Result := 0;
  ACellsCount := ARow.Cells.Count;
  for I := 0 to ACellsCount - 1 do
    Inc(Result, ARow.Cells[I].Properties.ColumnSpan);
end;

procedure TdxRtfTableConverter.ConvertRow(ARow: TdxTableRow; ARtfRow: TdxRtfTableRow);
var
  ARtfCells: TdxRtfTableCellCollection;
  I: Integer;
begin
  ARow.Properties.CopyFrom(ARtfRow.Properties);
  ARtfCells := ARtfRow.Cells;
  I := 0;
  while I < ARtfCells.Count do
  begin
    ConvertCell(ARow, ARtfCells[I]);
    Inc(I);
  end;
end;

procedure TdxRtfTableConverter.ConvertCell(ARow: TdxTableRow; ARtfCell: TdxRtfTableCell);
var
  ACell: TdxTableCell;
  AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
begin
  if ARtfCell.Properties.HorizontalMerging = TdxMergingState.Restart then
    MergeCells(ARtfCell);
  ACell := TdxTableCell.Create(ARow);
  ARow.Cells.AddInternal(ACell);
  ACell.Properties.BeginInit;
  try
    ACell.Properties.CopyFrom(ARtfCell.Properties);
  finally
    ACell.Properties.EndInit;
  end;
  AStartParagraphIndex := ARtfCell.StartParagraphIndex;
  AEndParagraphIndex := ARtfCell.EndParagraphIndex;
  PieceTable.TableCellsManager.InitializeTableCell(ACell, AStartParagraphIndex, AEndParagraphIndex);
  FRtfCellMap.Add(ARtfCell, ACell);
end;

procedure TdxRtfTableConverter.MergeCells(AFirstCell: TdxRtfTableCell);
var
  ANextIndex: Integer;
  ACells: TdxRtfTableCellCollection;
  ANextRtfCell: TdxRtfTableCell;
  AParentCellMap: TdxRtfParentCellMap;
  ATables: TdxRtfTableCollection;
  ATable: TdxRtfTable;
  I: Integer;
begin
  ANextIndex := AFirstCell.Index + 1;
  ACells := AFirstCell.Row.Cells;
  while (ANextIndex < ACells.Count) and (ACells[ANextIndex].Properties.HorizontalMerging = TdxMergingState.Continue) do
  begin
    ANextRtfCell := ACells[ANextIndex];

    AParentCellMap := TableReader.ParentCellMap;
    if AParentCellMap.ContainsKey(ANextRtfCell) then
    begin
      ATables := AParentCellMap[ANextRtfCell];
      for I := 0 to ATables.Count - 1 do
      begin
        ATable := ATables[I];
        FTablesQueue.Remove(ATable);
      end;
      AParentCellMap.Remove(ANextRtfCell);
    end;
    AFirstCell.Properties.ColumnSpan := AFirstCell.Properties.ColumnSpan + ANextRtfCell.Properties.ColumnSpan;
    RemoveCell(ACells, ANextRtfCell);
  end;
end;

procedure TdxRtfTableConverter.RemoveCell(ACells: TdxRtfTableCellCollection; ACell: TdxRtfTableCell);
var
  ACount, I, ALength: Integer;
  AStart, AEnd: TdxDocumentLogPosition;
begin
  ACells.Extract(ACell);
  ACount := ACells.Count;
  for I := ACell.Index to ACount - 1 do
    ACells[I].Index := ACells[I].Index - 1;
  AStart := PieceTable.Paragraphs[ACell.StartParagraphIndex].LogPosition;
  AEnd := PieceTable.Paragraphs[ACell.EndParagraphIndex].EndLogPosition;
  ALength := AEnd - AStart + 1;
  PieceTable.DeleteContent(AStart, ALength, False);
  RecalcParagraphIndexes(ACell);
end;

procedure TdxRtfTableConverter.RecalcParagraphIndexes(ARemovedCell: TdxRtfTableCell);
var
  ARemovedStartParagraphIndex: TdxParagraphIndex;
  ADelta, ARowIndex: Integer;
  ATable: TdxRtfTable;
begin
  ARemovedStartParagraphIndex := ARemovedCell.StartParagraphIndex;
  ADelta := ARemovedCell.EndParagraphIndex - ARemovedStartParagraphIndex + 1;
  RecalcParagraphIndexesInRow(ARemovedCell.Row, ARemovedCell.Index, ADelta);
  ATable := ARemovedCell.Row.Table;
  ARowIndex := ATable.Rows.IndexOf(ARemovedCell.Row);
  RecalcParagraphIndexesInTable(ATable, ARowIndex + 1, ADelta, ARemovedStartParagraphIndex);
  RecalcParagraphIndexesInTables(ADelta, ARemovedStartParagraphIndex);
end;

procedure TdxRtfTableConverter.RecalcParagraphIndexesInRow(ARow: TdxRtfTableRow; ACellIndex: Integer; ADelta: Integer);
var
  ACount, I: Integer;
begin
  ACount := ARow.Cells.Count;
  for I := ACellIndex to ACount - 1 do
  begin
    ARow.Cells[I].StartParagraphIndex := ARow.Cells[I].StartParagraphIndex - ADelta;
    ARow.Cells[I].EndParagraphIndex := ARow.Cells[I].EndParagraphIndex - ADelta;
  end;
end;

procedure TdxRtfTableConverter.RecalcParagraphIndexesInTable(ATable: TdxRtfTable; ARowIndex: Integer; ADelta: Integer; AParagraphIndex: TdxParagraphIndex);
var
  ACount, I: Integer;
begin
  if ATable.Rows.Last.Cells.Last.EndParagraphIndex <= AParagraphIndex then
    Exit;
  ACount := ATable.Rows.Count;
  for I := ARowIndex to ACount - 1 do
    RecalcParagraphIndexesInRow(ATable.Rows[I], 0, ADelta);
end;

procedure TdxRtfTableConverter.RecalcParagraphIndexesInTables(ADelta: Integer; AParagraphIndex: TdxParagraphIndex);
var
  ACount, I: Integer;
begin
  ACount := FTablesQueue.Count;
  for I := 0 to ACount - 1 do
    RecalcParagraphIndexesInTable(FTablesQueue[I], 0, ADelta, AParagraphIndex);
end;

{ TdxRtfTableColumnsCalculator }

function TdxRtfTableColumnsCalculator.Calculate(ATable: TdxRtfTable; ATableIndent: Integer): TdxRtfTableGrid;
var
  ARowsCount, I: Integer;
  ARowColumns: TdxRtfTableGrid;
  AOldResult: TdxRtfTableGrid;
begin
  ARowsCount := ATable.Rows.Count;
  if ARowsCount = 0 then
    TdxRichEditExceptions.ThrowArgumentException('table.Rows.Count', ARowsCount);

  Result := TdxRtfTableGrid.Create;
  Result.Add(ATableIndent);
  for I := 0 to ARowsCount - 1 do
  begin
    ARowColumns := GetRowColumns(ATable.Rows[I]);
    try
      AOldResult := Result;
      try
        Result := Merge(ARowColumns, AOldResult);
      finally
        AOldResult.Free;
      end;
    finally
      ARowColumns.Free;
    end;
  end;
end;

function TdxRtfTableColumnsCalculator.Merge(ASource: TdxRtfTableGrid; ADestination: TdxRtfTableGrid): TdxRtfTableGrid;
var
  ASourceIndex, ADestinationIndex: Integer;
begin
  Result := TdxRtfTableGrid.Create;
  ASourceIndex := 0;
  ADestinationIndex := 0;
  while (ASourceIndex < ASource.Count) and (ADestinationIndex < ADestination.Count) do
  begin
    if ASource[ASourceIndex] <= ADestination[ADestinationIndex] then
    begin
      Result.Add(ASource[ASourceIndex]);
      if ASource[ASourceIndex] = ADestination[ADestinationIndex] then
        Inc(ADestinationIndex);
      Inc(ASourceIndex);
    end
    else
    begin
      Result.Add(ADestination[ADestinationIndex]);
      Inc(ADestinationIndex);
    end;
  end;
  if ADestinationIndex < ADestination.Count then
  begin
    while ADestinationIndex < ADestination.Count do
    begin
      Result.Add(ADestination[ADestinationIndex]);
      Inc(ADestinationIndex);
    end;
  end
  else
    if ASourceIndex < ASource.Count then
    begin
      while ASourceIndex < ASource.Count do
      begin
        Result.Add(ASource[ASourceIndex]);
        Inc(ASourceIndex);
      end;
    end;
end;

function TdxRtfTableColumnsCalculator.GetRowColumns(ARow: TdxRtfTableRow): TdxRtfTableGrid;
var
  ALeft, ARight, ACellsCount, AIndex: Integer;
begin
  Result := TdxRtfTableGrid.Create;
  ALeft := ARow.ActualLeft;
  Result.Add(ALeft);

  ACellsCount := ARow.Cells.Count;
  if ACellsCount = 0 then
    TdxRichEditExceptions.ThrowArgumentException('row.Cells.Count', ACellsCount);

  for AIndex := 0 to ACellsCount - 1 do
  begin
    ARight := ARow.Cells[AIndex].ActualRight;
    if ARight <= ALeft then
      Result.Add(ALeft)
    else
    begin
      Result.Add(ARight);
      ALeft := ARight;
    end;
  end;
end;

end.
