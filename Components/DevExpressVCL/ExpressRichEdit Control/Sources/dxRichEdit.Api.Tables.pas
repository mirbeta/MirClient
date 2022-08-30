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

unit dxRichEdit.Api.Tables;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,
  dxRichEdit.NativeApi,
  dxRichEdit.Api.NativeDocument,
  dxRichEdit.Api.Formatting,

  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.Types,

  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.PieceTable;

type
  TdxNativeTable = class;
  TdxNativeTableRow = class;

  { TdxNativeTableCell }

  TdxNativeTableCell = class(TInterfacedObject,
    IdxRichEditTableCell)
  strict private
    FColumnIndex: Integer;
    FIsValid: Boolean;
    FRow: TdxNativeTableRow;

    FBorders: IdxRichEditTableCellBorders;

    procedure CheckValid;
    function GetDocument: TdxNativeSubDocument;
    function GetModelCell: TdxTableCell;
    function GetModelTable: TdxTable;
    function GetNativeTable: TdxNativeTable;

    function GetBackgroundColor: TdxAlphaColor;
    function GetBorders: IdxRichEditTableCellBorders;
    function GetBottomPadding: Single;
    function GetColumnSpan: Integer;
    function GetContentRange: IdxRichEditDocumentRange;
    function GetHeight: Single;
    function GetHeightType: TdxRichEditHeightType;
    function GetIndex: Integer;
    function GetLeftPadding: Single;
    function GetNestingLevel: Integer;
    function GetNext: IdxRichEditTableCell;
    function GetPreferredWidth: Single;
    function GetPreferredWidthType: TdxRichEditWidthType;
    function GetPrevious: IdxRichEditTableCell;
    function GetRange: IdxRichEditDocumentRange;
    function GetRightPadding: Single;
    function GetRow: IdxRichEditTableRow;
    function GetStyle: IdxRichEditTableCellStyle;
    function GetTable: IdxRichEditTable;
    function GetTopPadding: Single;
    function GetVerticalAlignment: TdxRichEditTableCellVerticalAlignment;
    function GetWordWrap: Boolean;
    procedure SetBackgroundColor(const Value: TdxAlphaColor);
    procedure SetBottomPadding(const Value: Single);
    procedure SetHeight(const Value: Single);
    procedure SetHeightType(const Value: TdxRichEditHeightType);
    procedure SetLeftPadding(const Value: Single);
    procedure SetPreferredWidth(const Value: Single);
    procedure SetPreferredWidthType(const Value: TdxRichEditWidthType);
    procedure SetRightPadding(const Value: Single);
    procedure SetStyle(const Value: IdxRichEditTableCellStyle);
    procedure SetTopPadding(const Value: Single);
    procedure SetVerticalAlignment(const Value: TdxRichEditTableCellVerticalAlignment);
    procedure SetWordWrap(const Value: Boolean);
  protected
    property ModelTable: TdxTable read GetModelTable;
    property NativeTable: TdxNativeTable read GetNativeTable;
    property Document: TdxNativeSubDocument read GetDocument;
  public
    constructor Create(ARow: TdxNativeTableRow; AColumnIndex: Integer);

    procedure Split(ARowCount, AColumnCount: Integer);
    procedure Delete;

    property ModelCell: TdxTableCell read GetModelCell;

    property BackgroundColor: TdxAlphaColor read GetBackgroundColor write SetBackgroundColor;
    property Borders: IdxRichEditTableCellBorders read GetBorders;
    property BottomPadding: Single read GetBottomPadding write SetBottomPadding;
    property ColumnSpan: Integer read GetColumnSpan;
    property ContentRange: IdxRichEditDocumentRange read GetContentRange;
    property Height: Single read GetHeight write SetHeight;
    property HeightType: TdxRichEditHeightType read GetHeightType write SetHeightType;
    property Index: Integer read GetIndex;
    property LeftPadding: Single read GetLeftPadding write SetLeftPadding;
    property NestingLevel: Integer read GetNestingLevel;
    property Next: IdxRichEditTableCell read GetNext;
    property PreferredWidth: Single read GetPreferredWidth write SetPreferredWidth;
    property PreferredWidthType: TdxRichEditWidthType read GetPreferredWidthType write SetPreferredWidthType;
    property Previous: IdxRichEditTableCell read GetPrevious;
    property Range: IdxRichEditDocumentRange read GetRange;
    property RightPadding: Single read GetRightPadding write SetRightPadding;
    property Row: IdxRichEditTableRow read GetRow;
    property Style: IdxRichEditTableCellStyle read GetStyle write SetStyle;
    property Table: IdxRichEditTable read GetTable;
    property TopPadding: Single read GetTopPadding write SetTopPadding;
    property VerticalAlignment: TdxRichEditTableCellVerticalAlignment read GetVerticalAlignment write SetVerticalAlignment;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
  end;

  { TdxNativeTableRow }

  TdxNativeTableRow = class(TInterfacedObject, IdxRichEditTableRow)
  strict private
    FTable: TdxNativeTable;
    FRowIndex: Integer;
    FCells: IdxRichEditTableCellCollection;
    FIsValid: Boolean;

    function GetDocument: TdxNativeSubDocument;
    function GetModelRow: TdxTableRow;
    function GetModelTable: TdxTable;

    function GetCells: IdxRichEditTableCellCollection;
    function GetFirstCell: IdxRichEditTableCell;
    function GetGridAfter: Integer;
    function GetGridBefore: Integer;
    function GetHeight: Single;
    function GetHeightType: TdxRichEditHeightType;
    function GetIndex: Integer;
    function GetIsFirst: Boolean;
    function GetIsLast: Boolean;
    function GetItem(AColumn: Integer): IdxRichEditTableCell;
    function GetLastCell: IdxRichEditTableCell;
    function GetNestingLevel: Integer;
    function GetNext: IdxRichEditTableRow;
    function GetPrevious: IdxRichEditTableRow;
    function GetRange: IdxRichEditDocumentRange;
    function GetTable: IdxRichEditTable;
    function GetTableRowAlignment: TdxRichEditTableRowAlignment;
    procedure SetHeight(const Value: Single);
    procedure SetHeightType(const Value: TdxRichEditHeightType);
    procedure SetTableRowAlignment(const Value: TdxRichEditTableRowAlignment);
  protected
    procedure CheckValid;

    property Document: TdxNativeSubDocument read GetDocument;
    property ModelRow: TdxTableRow read GetModelRow;
    property ModelTable: TdxTable read GetModelTable;
    property NativeTable: TdxNativeTable read FTable;
  public
    constructor Create(ATable: TdxNativeTable; ARowIndex: Integer);

    procedure Delete;

    property Cells: IdxRichEditTableCellCollection read GetCells;
    property FirstCell: IdxRichEditTableCell read GetFirstCell;
    property GridAfter: Integer read GetGridAfter;
    property GridBefore: Integer read GetGridBefore;
    property Height: Single read GetHeight write SetHeight;
    property HeightType: TdxRichEditHeightType read GetHeightType write SetHeightType;
    property Index: Integer read GetIndex;
    property IsFirst: Boolean read GetIsFirst;
    property IsLast: Boolean read GetIsLast;
    property Items[AColumn: Integer]: IdxRichEditTableCell read GetItem; default;
    property LastCell: IdxRichEditTableCell read GetLastCell;
    property NestingLevel: Integer read GetNestingLevel;
    property Next: IdxRichEditTableRow read GetNext;
    property Previous: IdxRichEditTableRow read GetPrevious;
    property Range: IdxRichEditDocumentRange read GetRange;
    property Table: IdxRichEditTable read GetTable;
    property TableRowAlignment: TdxRichEditTableRowAlignment read GetTableRowAlignment write SetTableRowAlignment;
  end;

  { TdxNativeTable }

  TdxNativeTable = class(TInterfacedObject,
    IdxRichEditTable, IdxBatchUpdateable)
  strict private
    type

      TBatchUpdateHandler = class(TcxIUnknownObject, IdxBatchUpdateHandler)
      strict private
        FTable: TdxNativeTable;
        procedure OnFirstBeginUpdate;
        procedure OnBeginUpdate;
        procedure OnEndUpdate;
        procedure OnLastEndUpdate;
        procedure OnCancelUpdate;
        procedure OnLastCancelUpdate;
      public
        constructor Create(ATable: TdxNativeTable);
      end;

  strict private
    FBatchUpdateHandler: TBatchUpdateHandler;
    FBatchUpdateHelper: TdxBatchUpdateHelper;
    FDocument: TdxNativeSubDocument;
    FIsValid: Boolean;
    FTable: TdxTable;

    FBorders: IdxRichEditTableBorders;
    FRows: IdxRichEditTableRowCollection;

    function GetCellsForMerging(const AMergeFrom, AMergeTo: IdxRichEditTableCell): TdxSelectedCellsCollection;
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;

    function GetBorders: IdxRichEditTableBorders;
    function GetBottomPadding: Single;
    function GetFirstRow: IdxRichEditTableRow;
    function GetIndent: Single;
    function GetItem(ARow, AColumn: Integer): IdxRichEditTableCell;
    function GetLastRow: IdxRichEditTableRow;
    function GetLeftPadding: Single;
    function GetNestingLevel: Integer;
    function GetParentCell: IdxRichEditTableCell;
    function GetPreferredWidth: Single;
    function GetPreferredWidthType: TdxRichEditWidthType;
    function GetRange: IdxRichEditDocumentRange;
    function GetRightPadding: Single;
    function GetRows: IdxRichEditTableRowCollection;
    function GetStyle: IdxRichEditTableStyle;
    function GetTableAlignment: TdxRichEditTableRowAlignment;
    function GetTableBackgroundColor: TdxAlphaColor;
    function GetTableCellSpacing: Single;
    function GetTableLayout: TdxRichEditTableLayoutType;
    function GetTableLook: TdxRichEditTableLookTypes;
    function GetTopPadding: Single;
    procedure SetBottomPadding(const Value: Single);
    procedure SetIndent(const Value: Single);
    procedure SetLeftPadding(const Value: Single);
    procedure SetPreferredWidth(const Value: Single); overload;
    procedure SetPreferredWidthType(const Value: TdxRichEditWidthType);
    procedure SetRightPadding(const Value: Single);
    procedure SetStyle(const Value: IdxRichEditTableStyle);
    procedure SetTableAlignment(const Value: TdxRichEditTableRowAlignment);
    procedure SetTableBackgroundColor(const Value: TdxAlphaColor);
    procedure SetTableCellSpacing(const Value: Single);
    procedure SetTableLayout(const Value: TdxRichEditTableLayoutType);
    procedure SetTableLook(const Value: TdxRichEditTableLookTypes);
    procedure SetTopPadding(const Value: Single);
  protected
    procedure CheckValid;

    procedure OnFirstBeginUpdate;
    procedure OnBeginUpdate;
    procedure OnEndUpdate;
    procedure OnLastEndUpdate;
    procedure OnCancelUpdate;
    procedure OnLastCancelUpdate;
    procedure OnLastEndUpdateCore;

    function GetIsUpdateLocked: Boolean;
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;

    property BatchUpdateHelper: TdxBatchUpdateHelper read FBatchUpdateHelper;
    property Document: TdxNativeSubDocument read FDocument;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  public
    constructor Create(ADocument: TdxNativeSubDocument; ATable: TdxTable);
    destructor Destroy; override;

    function Cell(ARow, AColumn: Integer): IdxRichEditTableCell;
    procedure Validate;
    procedure MergeCells(const AMergeFrom, AMergeTo: IdxRichEditTableCell);
    procedure SetPreferredWidth(AWidth: Single; AWidthType: TdxRichEditWidthType); overload;
    procedure ForEachCell(const ACellProcessor: TdxRichEditTableCellProcessorDelegate);
    procedure ForEachRow(const ARowProcessor: TdxRichEditTableRowProcessorDelegate);

    procedure Reset; overload;
    procedure Reset(const AMask: TdxRichEditTablePropertiesMask); overload;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;

    property IsValid: Boolean read FIsValid write FIsValid;
    property ModelTable: TdxTable read FTable;

    property Borders: IdxRichEditTableBorders read GetBorders;
    property BottomPadding: Single read GetBottomPadding write SetBottomPadding;
    property FirstRow: IdxRichEditTableRow read GetFirstRow;
    property Indent: Single read GetIndent write SetIndent;
    property LastRow: IdxRichEditTableRow read GetLastRow;
    property LeftPadding: Single read GetLeftPadding write SetLeftPadding;
    property NestingLevel: Integer read GetNestingLevel;
    property ParentCell: IdxRichEditTableCell read GetParentCell;
    property PreferredWidth: Single read GetPreferredWidth write SetPreferredWidth;
    property PreferredWidthType: TdxRichEditWidthType read GetPreferredWidthType write SetPreferredWidthType;
    property Range: IdxRichEditDocumentRange read GetRange;
    property RightPadding: Single read GetRightPadding write SetRightPadding;
    property Rows: IdxRichEditTableRowCollection read GetRows;
    property Style: IdxRichEditTableStyle read GetStyle write SetStyle;
    property TableAlignment: TdxRichEditTableRowAlignment read GetTableAlignment write SetTableAlignment;
    property TableBackgroundColor: TdxAlphaColor read GetTableBackgroundColor write SetTableBackgroundColor;
    property TableCellSpacing: Single read GetTableCellSpacing write SetTableCellSpacing;
    property TableLayout: TdxRichEditTableLayoutType read GetTableLayout write SetTableLayout;
    property TableLook: TdxRichEditTableLookTypes read GetTableLook write SetTableLook;
    property TopPadding: Single read GetTopPadding write SetTopPadding;
    property Self[ARow, AColumn: Integer]: IdxRichEditTableCell read GetItem; default;
  end;

  { TdxNativeTableCollection }

  TdxNativeTableCollection = class(TInterfacedObject,
    IdxRichEditTableCollection,
    IdxRichEditReadOnlyTableCollection)
  strict private
    FDocument: TdxNativeSubDocument;
    FInnerList: TList<IdxRichEditTable>;

    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;

    function GetCount: Integer;
    function GetFirst: IdxRichEditTable;
    function GetItem(Index: Integer): IdxRichEditTable;
    function GetLast: IdxRichEditTable;
  protected
    procedure RegisterTable(const ATable: TdxTable);
    procedure UnregisterTable(const ATable: TdxTable);

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  public
    constructor Create(ADocument: TdxNativeSubDocument);
    destructor Destroy; override;

    function Add(const APos: IdxRichEditDocumentPosition; ARowCount, AColumnCount: Integer;
      AAutoFitBehavior: TdxRichEditAutoFitBehaviorType = TdxRichEditAutoFitBehaviorType.AutoFitToContents;
      AFixedColumnWidths: Integer = MinInt): IdxRichEditTable;
    procedure Delete(ATableIndex: Integer);
    procedure Remove(const ATable: IdxRichEditTable);
    function IndexOf(const ATable: IdxRichEditTable): Integer;

    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyTableCollection;
    function GetTableCell(const APos: IdxRichEditDocumentPosition): IdxRichEditTableCell;

    procedure Clear;
    procedure PopulateTables;
    procedure TablesCollectionChanged(ATable: TdxTable; AAction: TListNotification);

    property Count: Integer read GetCount;
    property First: IdxRichEditTable read GetFirst;
    property Items[Index: Integer]: IdxRichEditTable read GetItem; default;
    property Last: IdxRichEditTable read GetLast;
  end;

  { TdxNativeTableRowCollection }

  TdxNativeTableRowCollection = class(TInterfacedObject,
    IdxRichEditTableRowCollection)
  strict private
    FCachedItems: TDictionary<Integer, IdxRichEditTableRow>;
    FTable: TdxNativeTable;

    function GetPieceTable: TdxPieceTable;

    function GetCount: Integer;
    function GetFirst: IdxRichEditTableRow;
    function GetItem(Index: Integer): IdxRichEditTableRow;
    function GetLast: IdxRichEditTableRow;
  protected
    property PieceTable: TdxPieceTable read GetPieceTable;
  public
    constructor Create(ATable: TdxNativeTable);
    destructor Destroy; override;

    function Append: IdxRichEditTableRow;
    procedure Delete(ARowIndex: Integer);
    function InsertBefore(ARowIndex: Integer): IdxRichEditTableRow;
    function InsertAfter(ARowIndex: Integer): IdxRichEditTableRow;

    property Count: Integer read GetCount;
    property First: IdxRichEditTableRow read GetFirst;
    property Items[Index: Integer]: IdxRichEditTableRow read GetItem; default;
    property Last: IdxRichEditTableRow read GetLast;
  end;

  { TdxNativeTableCellCollection }

  TdxNativeTableCellCollection = class(TInterfacedObject,
    IdxRichEditTableCellCollection)
  strict private
    FCachedItems: TDictionary<Integer, IdxRichEditTableCell>;
    FRow: TdxNativeTableRow;

    function GetDocument: TdxNativeSubDocument;
    function GetPieceTable: TdxPieceTable;

    function GetCount: Integer;
    function GetFirst: IdxRichEditTableCell;
    function GetItem(Index: Integer): IdxRichEditTableCell;
    function GetLast: IdxRichEditTableCell;
  protected
    property Document: TdxNativeSubDocument read GetDocument;
    property PieceTable: TdxPieceTable read GetPieceTable;
  public
    constructor Create(ARow: TdxNativeTableRow);
    destructor Destroy; override;

    function Append: IdxRichEditTableCell;
    procedure Delete(AColumnIndex: Integer);
    function InsertBefore(AColumnIndex : Integer): IdxRichEditTableCell;
    function InsertAfter(AColumnIndex: Integer): IdxRichEditTableCell;

    property Count: Integer read GetCount;
    property First: IdxRichEditTableCell read GetFirst;
    property Items[Index: Integer]: IdxRichEditTableCell read GetItem; default;
    property Last: IdxRichEditTableCell read GetLast;
  end;

  { TdxNativeTableBorderBase }

  TdxNativeTableBorderBase = class abstract(TdxNativeBorderBase)
  strict private
    type
      TBorderValueSetter = reference to procedure(const ABorder: TdxBorderBase);
  strict private
    procedure SetBorderValues(const ASetter: TBorderValueSetter);
  protected
    function GetActualBorder: TdxBorderBase; virtual; abstract;
    function GetBorder: TdxBorderBase; virtual; abstract;

    function GetLineColorCore: TdxAlphaColor; override;
    function GetLineStyleCore: TdxBorderLineStyle; override;
    function GetLineThicknessCore: Integer; override;
    procedure SetLineColorCore(const Value: TdxAlphaColor); override;
    procedure SetLineStyleCore(const Value: TdxBorderLineStyle); override;
    procedure SetLineThicknessCore(const Value: Integer); override;
  end;

  { TdxNativeTableCellBorder }

  TdxNativeTableCellBorder = class(TdxNativeTableBorderBase,
    IdxRichEditTableCellBorder)
  strict private
    FCell: TdxNativeTableCell;
  protected
    property Cell: TdxNativeTableCell read FCell;
  public
    constructor Create(ACell: TdxNativeTableCell); reintroduce;
  end;

  { TdxNativeTableBottomCellBorder }

  TdxNativeTableBottomCellBorder = class abstract(TdxNativeTableCellBorder)
  protected
    function GetActualBorder: TdxBorderBase; override;
    function GetBorder: TdxBorderBase; override;
  end;

  { TdxNativeTableLeftCellBorder }

  TdxNativeTableLeftCellBorder = class abstract(TdxNativeTableCellBorder)
  protected
    function GetActualBorder: TdxBorderBase; override;
    function GetBorder: TdxBorderBase; override;
  end;

  { TdxNativeTableRightCellBorder }

  TdxNativeTableRightCellBorder = class abstract(TdxNativeTableCellBorder)
  protected
    function GetActualBorder: TdxBorderBase; override;
    function GetBorder: TdxBorderBase; override;
  end;

  { TdxNativeTableTopCellBorder }

  TdxNativeTableTopCellBorder = class abstract(TdxNativeTableCellBorder)
  protected
    function GetActualBorder: TdxBorderBase; override;
    function GetBorder: TdxBorderBase; override;
  end;

  { TdxNativeTableCellBorders }

  TdxNativeTableCellBorders = class(TdxNativeTableCellBordersBase)
  strict private
    FCell: TdxNativeTableCell;
  protected
    function CreateBottom: IdxRichEditTableCellBorder; override;
    function CreateLeft: IdxRichEditTableCellBorder; override;
    function CreateRight: IdxRichEditTableCellBorder; override;
    function CreateTop: IdxRichEditTableCellBorder; override;

    property Cell: TdxNativeTableCell read FCell;
  public
    constructor Create(ACell: TdxNativeTableCell); reintroduce;
  end;

  { TdxNativeTableBorder }

  TdxNativeTableBorder = class abstract(TdxNativeTableBorderBase, IdxRichEditTableBorder)
  strict private
    FTable: TdxNativeTable;
  protected
    property Table: TdxNativeTable read FTable;
  public
    constructor Create(ATable: TdxNativeTable); reintroduce;
  end;

  { TdxNativeTableBottomBorder }

  TdxNativeTableBottomBorder = class abstract(TdxNativeTableBorder)
  protected
    function GetActualBorder: TdxBorderBase; override;
    function GetBorder: TdxBorderBase; override;
  end;

  { TdxNativeTableLeftBorder }

  TdxNativeTableLeftBorder = class abstract(TdxNativeTableBorder)
  protected
    function GetActualBorder: TdxBorderBase; override;
    function GetBorder: TdxBorderBase; override;
  end;

  { TdxNativeTableRightBorder }

  TdxNativeTableRightBorder = class abstract(TdxNativeTableBorder)
  protected
    function GetActualBorder: TdxBorderBase; override;
    function GetBorder: TdxBorderBase; override;
  end;

  { TdxNativeTableTopBorder }

  TdxNativeTableTopBorder = class abstract(TdxNativeTableBorder)
  protected
    function GetActualBorder: TdxBorderBase; override;
    function GetBorder: TdxBorderBase; override;
  end;

  { TdxNativeTableInsideHorizontalBorder }

  TdxNativeTableInsideHorizontalBorder = class abstract(TdxNativeTableBorder)
  protected
    function GetActualBorder: TdxBorderBase; override;
    function GetBorder: TdxBorderBase; override;
  end;

  { TdxNativeTableInsideVerticalBorder }

  TdxNativeTableInsideVerticalBorder = class abstract(TdxNativeTableBorder)
  protected
    function GetActualBorder: TdxBorderBase; override;
    function GetBorder: TdxBorderBase; override;
  end;

  { TdxNativeTableBorders }

  TdxNativeTableBorders = class(TdxNativeTableBordersBase)
  strict private
    FTable: TdxNativeTable;
  protected
    function CreateBottom: IdxRichEditTableBorder; override;
    function CreateInsideHorizontalBorder: IdxRichEditTableBorder; override;
    function CreateInsideVerticalBorder: IdxRichEditTableBorder; override;
    function CreateLeft: IdxRichEditTableBorder; override;
    function CreateRight: IdxRichEditTableBorder; override;
    function CreateTop: IdxRichEditTableBorder; override;

    property Table: TdxNativeTable read FTable;
  public
    constructor Create(ATable: TdxNativeTable); reintroduce;
  end;

implementation

uses
  RTLConsts,
  dxRichEdit.Api.Paragraphs,

  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.Platform.Font,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs;

type
  { TdxNativeTableComparer }

  TdxNativeTableComparer = class(TinterfacedObject, IComparer<IdxRichEditTable>)
  public
    function Compare(const Left, Right: IdxRichEditTable): Integer;
  end;

{ TdxNativeTableComparer }

function TdxNativeTableComparer.Compare(const Left, Right: IdxRichEditTable): Integer;
begin
  Result := TdxNativeTable(Left).ModelTable.Index - TdxNativeTable(Right).ModelTable.Index;
end;

{ TdxNativeTableCell }

constructor TdxNativeTableCell.Create(ARow: TdxNativeTableRow; AColumnIndex: Integer);
begin
  inherited Create;
  FRow := ARow;
  FColumnIndex := AColumnIndex;
  FIsValid := True;
end;

procedure TdxNativeTableCell.Split(ARowCount, AColumnCount: Integer);
var
  APieceTable: TdxPieceTable;
  ADocumentModel: TdxDocumentModel;
begin
  APieceTable := TdxPieceTable(FRow.ModelTable.PieceTable);

  ADocumentModel := Document.DocumentModel;
  ADocumentModel.BeginUpdate;
  try
    APieceTable.SplitTableCellsHorizontally(FRow.ModelRow.Cells[FColumnIndex], AColumnCount, Document.DocumentServer.Owner);
    APieceTable.SplitTableCellsVertically(FRow.ModelRow.Cells[FColumnIndex], ARowCount, AColumnCount, False);
  finally
    ADocumentModel.EndUpdate;
  end;
end;

procedure TdxNativeTableCell.Delete;
begin
  FRow.Cells.Delete(FColumnIndex);
end;

procedure TdxNativeTableCell.CheckValid;
begin
  FRow.CheckValid;
  if FIsValid then
    FIsValid := (FColumnIndex <= FRow.ModelRow.Cells.Count);
  if not FIsValid then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionUseDeletedTableCellError));
end;

function TdxNativeTableCell.GetDocument: TdxNativeSubDocument;
begin
  CheckValid;
  Result := NativeTable.Document;
end;

function TdxNativeTableCell.GetModelCell: TdxTableCell;
begin
  CheckValid;
  Result := FRow.ModelRow.Cells[FColumnIndex];
end;

function TdxNativeTableCell.GetModelTable: TdxTable;
begin
  CheckValid;
  Result := FRow.ModelTable;
end;

function TdxNativeTableCell.GetNativeTable: TdxNativeTable;
begin
  CheckValid;
  Result := FRow.NativeTable;
end;

function TdxNativeTableCell.GetBackgroundColor: TdxAlphaColor;
begin
  Result := ModelCell.BackgroundColor;
end;

function TdxNativeTableCell.GetBorders: IdxRichEditTableCellBorders;
begin
  if FBorders = nil then
    FBorders := TdxNativeTableCellBorders.Create(Self);
  Result := FBorders;
end;

function TdxNativeTableCell.GetBottomPadding: Single;
var
  AMargin: TdxMarginUnitBase;
begin
  AMargin := ModelCell.GetActualBottomMargin;
  Result := Document.GetWidthUnitFixedValue(AMargin);
end;

function TdxNativeTableCell.GetColumnSpan: Integer;
begin
  Result := ModelCell.ColumnSpan;
end;

function TdxNativeTableCell.GetContentRange: IdxRichEditDocumentRange;
var
  AStart, AEnd: TdxParagraph;
begin
  AStart := Document.PieceTable.Paragraphs[ModelCell.StartParagraphIndex];
  AEnd := Document.PieceTable.Paragraphs[ModelCell.EndParagraphIndex];
  Result := Document.CreateRange(AStart.LogPosition, AEnd.LogPosition + AEnd.Length - AStart.LogPosition - 1);
end;

function TdxNativeTableCell.GetHeight: Single;
begin
  Result := FRow.Height;
end;

function TdxNativeTableCell.GetHeightType: TdxRichEditHeightType;
begin
  Result := FRow.HeightType;
end;

function TdxNativeTableCell.GetIndex: Integer;
begin
  Result := FColumnIndex;
end;

function TdxNativeTableCell.GetLeftPadding: Single;
var
  AMargin: TdxMarginUnitBase;
begin
  AMargin := ModelCell.GetActualLeftMargin;
  Result := Document.GetWidthUnitFixedValue(AMargin);
end;

function TdxNativeTableCell.GetNestingLevel: Integer;
begin
  Result := NativeTable.NestingLevel;
end;

function TdxNativeTableCell.GetNext: IdxRichEditTableCell;
begin
  CheckValid;
  if FColumnIndex + 1 < FRow.Cells.Count then
    Result := FRow.Cells[FColumnIndex + 1]
  else
    Result := nil;
end;

function TdxNativeTableCell.GetPreferredWidth: Single;
begin
  Result := Document.GetWidthValue(ModelCell.PreferredWidth);
end;

function TdxNativeTableCell.GetPreferredWidthType: TdxRichEditWidthType;
begin
  Result := TdxRichEditWidthType(ModelCell.PreferredWidth.&Type);
end;

function TdxNativeTableCell.GetPrevious: IdxRichEditTableCell;
begin
  CheckValid;
  if FColumnIndex > 0 then
    Result := FRow.Cells[FColumnIndex - 1]
  else
    Result := nil;
end;

function TdxNativeTableCell.GetRange: IdxRichEditDocumentRange;
var
  AStart, AEnd: TdxParagraph;
begin
  AStart := Document.PieceTable.Paragraphs[ModelCell.StartParagraphIndex];
  AEnd := Document.PieceTable.Paragraphs[ModelCell.EndParagraphIndex];
  Result := Document.CreateRange(AStart.LogPosition, AEnd.LogPosition + AEnd.Length - AStart.LogPosition);
end;

function TdxNativeTableCell.GetRightPadding: Single;
var
  AMargin: TdxMarginUnitBase;
begin
  AMargin := ModelCell.GetActualRightMargin;
  Result := Document.GetWidthUnitFixedValue(AMargin);
end;

function TdxNativeTableCell.GetRow: IdxRichEditTableRow;
begin
  CheckValid;
  Result := FRow;
end;

function TdxNativeTableCell.GetStyle: IdxRichEditTableCellStyle;
begin
  CheckValid;
  Result := Document.GetTableCellStyle(ModelCell);
end;

function TdxNativeTableCell.GetTable: IdxRichEditTable;
begin
  Result := FRow.Table;
end;

function TdxNativeTableCell.GetTopPadding: Single;
var
  AMargin: TdxMarginUnitBase;
begin
  AMargin := ModelCell.GetActualTopMargin;
  Result := Document.GetWidthUnitFixedValue(AMargin);
end;

function TdxNativeTableCell.GetVerticalAlignment: TdxRichEditTableCellVerticalAlignment;
begin
  Result := TdxNativeTableCellPropertiesBase.VerticalAlignmentMap[ModelCell.VerticalAlignment];
end;

function TdxNativeTableCell.GetWordWrap: Boolean;
begin
  Result := not ModelCell.NoWrap;
end;

procedure TdxNativeTableCell.SetBackgroundColor(const Value: TdxAlphaColor);
begin
  ModelCell.Properties.BackgroundColor := Value;
end;

procedure TdxNativeTableCell.SetBottomPadding(const Value: Single);
begin
  Document.SetWidthUnitFixedValue(ModelCell.Properties.CellMargins.Bottom, Value);
end;

procedure TdxNativeTableCell.SetHeight(const Value: Single);
begin
  FRow.Height := Value;
end;

procedure TdxNativeTableCell.SetHeightType(const Value: TdxRichEditHeightType);
begin
  FRow.HeightType := Value;
end;

procedure TdxNativeTableCell.SetLeftPadding(const Value: Single);
begin
  Document.SetWidthUnitFixedValue(ModelCell.Properties.CellMargins.Left, Value);
end;

procedure TdxNativeTableCell.SetPreferredWidth(const Value: Single);
begin
  Document.SetWidthValue(ModelCell.Properties.PreferredWidth, Value);
end;

procedure TdxNativeTableCell.SetPreferredWidthType(const Value: TdxRichEditWidthType);
begin
  ModelCell.Properties.PreferredWidth.&Type := TdxWidthUnitType(Value);
end;

procedure TdxNativeTableCell.SetRightPadding(const Value: Single);
begin
  Document.SetWidthUnitFixedValue(ModelCell.Properties.CellMargins.Right, Value);
end;

procedure TdxNativeTableCell.SetStyle(const Value: IdxRichEditTableCellStyle);
var
  AStyle: TdxTableCellStyle;
begin
  CheckValid;
  if Value <> nil then
    AStyle := Document.GetInnerTableCellStyle(Value)
  else
    AStyle := nil;
  ModelCell.StyleIndex := TdxDocumentModel(ModelTable.DocumentModel).TableCellStyles.IndexOf(AStyle);
end;

procedure TdxNativeTableCell.SetTopPadding(const Value: Single);
begin
  Document.SetWidthUnitFixedValue(ModelCell.Properties.CellMargins.Top, Value);
end;

procedure TdxNativeTableCell.SetVerticalAlignment(const Value: TdxRichEditTableCellVerticalAlignment);
begin
  ModelCell.Properties.VerticalAlignment := TdxNativeTableCellPropertiesBase.ModelVerticalAlignmentMap[Value];
end;

procedure TdxNativeTableCell.SetWordWrap(const Value: Boolean);
begin
  ModelCell.Properties.NoWrap := not Value;
end;

{ TdxNativeTableRow }

constructor TdxNativeTableRow.Create(ATable: TdxNativeTable; ARowIndex: Integer);
begin
  inherited Create;
  FTable := ATable;
  FRowIndex := ARowIndex;
  FIsValid := True;
end;

procedure TdxNativeTableRow.Delete;
begin
end;

procedure TdxNativeTableRow.CheckValid;
begin
  FTable.CheckValid;
  if FIsValid then
    FIsValid := FRowIndex <= FTable.ModelTable.Rows.Count;
  if not FIsValid then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourcestring(@sdxRichEditExceptionUseDeletedTableRowError));
end;

function TdxNativeTableRow.GetDocument: TdxNativeSubDocument;
begin
  Result := NativeTable.Document;
end;

function TdxNativeTableRow.GetModelRow: TdxTableRow;
begin
  CheckValid;
  Result := FTable.ModelTable.Rows[FRowIndex];
end;

function TdxNativeTableRow.GetModelTable: TdxTable;
begin
  CheckValid;
  Result := FTable.ModelTable;
end;

function TdxNativeTableRow.GetCells: IdxRichEditTableCellCollection;
begin
  CheckValid;
  if FCells = nil then
    FCells := TdxNativeTableCellCollection.Create(Self);
  Result := FCells;
end;

function TdxNativeTableRow.GetFirstCell: IdxRichEditTableCell;
begin
  Result := Cells.First;
end;

function TdxNativeTableRow.GetGridAfter: Integer;
begin
  Result := ModelRow.GridAfter;
end;

function TdxNativeTableRow.GetGridBefore: Integer;
begin
  Result := ModelRow.GridBefore;
end;

function TdxNativeTableRow.GetHeight: Single;
begin
  Result := Document.ModelUnitsToUnits(ModelRow.Properties.Height.Value);
end;

function TdxNativeTableRow.GetHeightType: TdxRichEditHeightType;
begin
  Result := TdxRichEditHeightType(ModelRow.Height.&Type);
end;

function TdxNativeTableRow.GetIndex: Integer;
begin
  Result := FRowIndex
end;

function TdxNativeTableRow.GetIsFirst: Boolean;
begin
  Result := Index = 0;
end;

function TdxNativeTableRow.GetIsLast: Boolean;
begin
  Result := Index = NativeTable.ModelTable.Rows.Count - 1;
end;

function TdxNativeTableRow.GetItem(AColumn: Integer): IdxRichEditTableCell;
begin
  Result := Cells[AColumn];
end;

function TdxNativeTableRow.GetLastCell: IdxRichEditTableCell;
begin
  Result := Cells.Last;
end;

function TdxNativeTableRow.GetNestingLevel: Integer;
begin
  Result := Table.NestingLevel;
end;

function TdxNativeTableRow.GetNext: IdxRichEditTableRow;
begin
  CheckValid;
  if FRowIndex + 1 < NativeTable.ModelTable.Rows.Count then
    Result := Table.Rows[FRowIndex + 1]
  else
    Result := nil;
end;

function TdxNativeTableRow.GetPrevious: IdxRichEditTableRow;
begin
  CheckValid;
  if FRowIndex > 0 then
    Result := Table.Rows[FRowIndex - 1]
  else
    Result := nil;
end;

function TdxNativeTableRow.GetRange: IdxRichEditDocumentRange;
var
  AStart, AEnd: TdxParagraph;
begin
  AStart := Document.PieceTable.Paragraphs[ModelRow.FirstCell.StartParagraphIndex];
  AEnd := Document.PieceTable.Paragraphs[ModelRow.LastCell.EndParagraphIndex];
  Result := Document.CreateRange(AStart.LogPosition, AEnd.LogPosition + AEnd.Length - AStart.LogPosition);
end;

function TdxNativeTableRow.GetTable: IdxRichEditTable;
begin
  Result := FTable;
end;

function TdxNativeTableRow.GetTableRowAlignment: TdxRichEditTableRowAlignment;
begin
  Result := ModelRow.TableRowAlignment;
end;

procedure TdxNativeTableRow.SetHeight(const Value: Single);
begin
  ModelRow.Properties.Height.Value := Document.UnitsToModelUnits(Value);
end;

procedure TdxNativeTableRow.SetHeightType(const Value: TdxRichEditHeightType);
begin
  ModelRow.Properties.Height.&Type := TdxHeightUnitType(Value);
end;

procedure TdxNativeTableRow.SetTableRowAlignment(const Value: TdxRichEditTableRowAlignment);
begin
  ModelRow.Properties.TableRowAlignment := Value;
end;

{ TdxNativeTable.TBatchUpdateHandler }

constructor TdxNativeTable.TBatchUpdateHandler.Create(ATable: TdxNativeTable);
begin
  inherited Create;
  FTable := ATable;
end;

procedure TdxNativeTable.TBatchUpdateHandler.OnBeginUpdate;
begin
  FTable.OnBeginUpdate;
end;

procedure TdxNativeTable.TBatchUpdateHandler.OnCancelUpdate;
begin
  FTable.OnCancelUpdate;
end;

procedure TdxNativeTable.TBatchUpdateHandler.OnEndUpdate;
begin
  FTable.OnEndUpdate;
end;

procedure TdxNativeTable.TBatchUpdateHandler.OnFirstBeginUpdate;
begin
  FTable.OnFirstBeginUpdate;
end;

procedure TdxNativeTable.TBatchUpdateHandler.OnLastCancelUpdate;
begin
  FTable.OnLastCancelUpdate;
end;

procedure TdxNativeTable.TBatchUpdateHandler.OnLastEndUpdate;
begin
  FTable.OnLastEndUpdate;
end;

{ TdxNativeTable }

constructor TdxNativeTable.Create(ADocument: TdxNativeSubDocument; ATable: TdxTable);
begin
  inherited Create;
  FDocument := ADocument;
  FTable := ATable;
  FBatchUpdateHandler := TBatchUpdateHandler.Create(Self);
  FBatchUpdateHelper := TdxBatchUpdateHelper.Create(FBatchUpdateHandler);
  FIsValid := True;
end;

destructor TdxNativeTable.Destroy;
begin
  FreeAndNil(FBatchUpdateHelper);
  FreeAndNil(FBatchUpdateHandler);
  inherited Destroy;
end;

function TdxNativeTable.Cell(ARow, AColumn: Integer): IdxRichEditTableCell;
begin
  Result := Rows[ARow].Cells[AColumn];
end;

procedure TdxNativeTable.Validate;
begin
  CheckValid;
end;

procedure TdxNativeTable.MergeCells(const AMergeFrom, AMergeTo: IdxRichEditTableCell);
var
  ACells: TdxSelectedCellsCollection;
begin
  ACells := GetCellsForMerging(AMergeFrom, AMergeTo);
  TdxSelectedCellsCollection.AddReference(ACells);
  try
    DocumentModel.BeginUpdate;
    try
      PieceTable.MergeCells(ACells);
    finally
      DocumentModel.EndUpdate;
    end;
  finally
    TdxSelectedCellsCollection.Release(ACells);
  end;
end;

procedure TdxNativeTable.SetPreferredWidth(AWidth: Single; AWidthType: TdxRichEditWidthType);
begin
  PreferredWidthType := AWidthType;
  PreferredWidth := AWidth;
end;

procedure TdxNativeTable.ForEachCell(const ACellProcessor: TdxRichEditTableCellProcessorDelegate);
var
  ACells: IdxRichEditTableCellCollection;
  ARows: IdxRichEditTableRowCollection;
  I, ARowCount: Integer;
  J, ACellCount: Integer;
begin
  ARows := Rows;
  ARowCount := ARows.Count;
  for I := 0 to ARowCount - 1 do
  begin
    ACells := ARows[I].Cells;
    ACellCount := ACells.Count;
    for J := 0 to ACellCount - 1 do
      ACellProcessor(ACells[J], I, J);
  end;
end;

procedure TdxNativeTable.ForEachRow(const ARowProcessor: TdxRichEditTableRowProcessorDelegate);
var
  ARows: IdxRichEditTableRowCollection;
  I, ARowCount: Integer;
begin
  ARows := Rows;
  ARowCount := ARows.Count;
  for I := 0 to ARowCount - 1 do
    ARowProcessor(ARows[I], I);
end;

procedure TdxNativeTable.Reset;
begin
  ModelTable.TableProperties.ResetAllUse;
end;

procedure TdxNativeTable.Reset(const AMask: TdxRichEditTablePropertiesMask);
begin
  ModelTable.TableProperties.ResetUse(TdxNativeTablePropertiesBase.ApiMaskToModelMask(AMask));
end;

procedure TdxNativeTable.BeginUpdate;
begin
  FBatchUpdateHelper.BeginUpdate;
end;

procedure TdxNativeTable.EndUpdate;
begin
  FBatchUpdateHelper.EndUpdate;
end;

procedure TdxNativeTable.CancelUpdate;
begin
  FBatchUpdateHelper.CancelUpdate;
end;

procedure TdxNativeTable.CheckValid;
begin
  if not IsValid then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionUseDeletedTableError));
end;

procedure TdxNativeTable.OnFirstBeginUpdate;
begin
  DocumentModel.BeginUpdate;
end;

procedure TdxNativeTable.OnBeginUpdate;
begin
end;

procedure TdxNativeTable.OnEndUpdate;
begin
end;

procedure TdxNativeTable.OnLastEndUpdate;
begin
  OnLastEndUpdateCore;
end;

procedure TdxNativeTable.OnCancelUpdate;
begin
end;

procedure TdxNativeTable.OnLastCancelUpdate;
begin
  OnLastEndUpdateCore;
end;

procedure TdxNativeTable.OnLastEndUpdateCore;
begin
  Validate;
  DocumentModel.EndUpdate;
end;

function TdxNativeTable.GetIsUpdateLocked: Boolean;
begin
  Result := FBatchUpdateHelper.IsUpdateLocked;
end;

function TdxNativeTable.GetBatchUpdateHelper: TdxBatchUpdateHelper;
begin
  Result := FBatchUpdateHelper;
end;

function TdxNativeTable.GetDocumentModel: TdxDocumentModel;
begin
  Result := FDocument.DocumentModel;
end;

function TdxNativeTable.GetPieceTable: TdxPieceTable;
begin
  Result := FDocument.PieceTable;
end;

function TdxNativeTable.GetCellsForMerging(const AMergeFrom, AMergeTo: IdxRichEditTableCell): TdxSelectedCellsCollection;
var
  ASelectionCalculator: TdxTableStructureBySelectionCalculator;
begin
  ASelectionCalculator := TdxTableStructureBySelectionCalculator.Create(PieceTable);
  try
    Result := ASelectionCalculator.Calculate(TdxNativeTableCell(AMergeFrom).ModelCell, TdxNativeTableCell(AMergeTo).ModelCell);
  finally
    ASelectionCalculator.Free;
  end;
end;

function TdxNativeTable.GetBorders: IdxRichEditTableBorders;
begin
  if FBorders = nil then
    FBorders := TdxNativeTableBorders.Create(Self);
  Result := FBorders;
end;

function TdxNativeTable.GetBottomPadding: Single;
begin
  Result := Document.GetWidthUnitFixedValue(ModelTable.BottomMargin);
end;

function TdxNativeTable.GetFirstRow: IdxRichEditTableRow;
begin
  Result := Rows.First;
end;

function TdxNativeTable.GetIndent: Single;
begin
  Result := Document.GetWidthUnitFixedValue(ModelTable.TableIndent);
end;

function TdxNativeTable.GetItem(ARow, AColumn: Integer): IdxRichEditTableCell;
begin
  Result := Rows[ARow].Cells[AColumn];
end;

function TdxNativeTable.GetLastRow: IdxRichEditTableRow;
begin
  Result := Rows.Last;
end;

function TdxNativeTable.GetLeftPadding: Single;
begin
  Result := Document.GetWidthUnitFixedValue(ModelTable.LeftMargin);
end;

function TdxNativeTable.GetNestingLevel: Integer;
begin
  Result := ModelTable.NestedLevel;
end;

function TdxNativeTable.GetParentCell: IdxRichEditTableCell;
var
  AParentCell: TdxTableCell;
  I, AColumn, ARowIndexInTable, ATableCount: Integer;
begin
  Result := nil;
  AParentCell := ModelTable.ParentCell;
  if AParentCell = nil then
    Exit;
  AColumn := AParentCell.IndexInRow;
  ARowIndexInTable := AParentCell.Row.IndexInTable;
  ATableCount := FDocument.Tables.Count;
  for I := 0 to ATableCount - 1 do
  begin
    if TdxNativeTable(FDocument.Tables[I]).ModelTable = AParentCell.Table then
    begin
      Result := FDocument.Tables[I][ARowIndexInTable, AColumn];
      Break;
    end;
  end;
end;

function TdxNativeTable.GetPreferredWidth: Single;
begin
  Result := Document.GetWidthValue(ModelTable.PreferredWidth);
end;

function TdxNativeTable.GetPreferredWidthType: TdxRichEditWidthType;
begin
  Result := TdxRichEditWidthType(ModelTable.PreferredWidth.&Type);
end;

function TdxNativeTable.GetRange: IdxRichEditDocumentRange;
var
  AStart, AEnd: TdxParagraph;
begin
  AStart := Document.PieceTable.Paragraphs[FTable.StartParagraphIndex];
  AEnd := Document.PieceTable.Paragraphs[FTable.EndParagraphIndex];
  Result := Document.CreateRange(AStart.LogPosition, AEnd.LogPosition + AEnd.Length - AStart.LogPosition);
end;

function TdxNativeTable.GetRightPadding: Single;
begin
  Result := Document.GetWidthUnitFixedValue(ModelTable.RightMargin);
end;

function TdxNativeTable.GetRows: IdxRichEditTableRowCollection;
begin
  CheckValid;
  if FRows = nil then
    FRows := TdxNativeTableRowCollection.Create(Self);
  Result := FRows;
end;

function TdxNativeTable.GetStyle: IdxRichEditTableStyle;
var
  AStyles: TdxNativeTableStyleCollection;
begin
  CheckValid;
  AStyles := TdxNativeTableStyleCollection(Document.MainDocument.TableStyles);
  Result := AStyles.GetStyle(ModelTable.TableStyle);
end;

function TdxNativeTable.GetTableAlignment: TdxRichEditTableRowAlignment;
begin
  Result := ModelTable.TableAlignment;
end;

function TdxNativeTable.GetTableBackgroundColor: TdxAlphaColor;
begin
  Result := ModelTable.BackgroundColor;
end;

function TdxNativeTable.GetTableCellSpacing: Single;
begin
  Result := Document.GetWidthUnitFixedValue(ModelTable.CellSpacing);
end;

function TdxNativeTable.GetTableLayout: TdxRichEditTableLayoutType;
begin
  Result := ModelTable.TableLayout;
end;

function TdxNativeTable.GetTableLook: TdxRichEditTableLookTypes;
begin
  Result := ModelTable.TableLook;
end;

function TdxNativeTable.GetTopPadding: Single;
begin
  Result := Document.GetWidthUnitFixedValue(ModelTable.TopMargin);
end;

procedure TdxNativeTable.SetBottomPadding(const Value: Single);
begin
  Document.SetWidthUnitFixedValue(ModelTable.TableProperties.CellMargins.Bottom, Value);
end;

procedure TdxNativeTable.SetIndent(const Value: Single);
begin
  Document.SetWidthUnitFixedValue(ModelTable.TableProperties.TableIndent, Value);
end;

procedure TdxNativeTable.SetLeftPadding(const Value: Single);
begin
  Document.SetWidthUnitFixedValue(ModelTable.TableProperties.CellMargins.Left, Value);
end;

procedure TdxNativeTable.SetPreferredWidth(const Value: Single);
begin
  Document.SetWidthValue(ModelTable.TableProperties.PreferredWidth, Value);
end;

procedure TdxNativeTable.SetPreferredWidthType(const Value: TdxRichEditWidthType);
begin
  ModelTable.TableProperties.PreferredWidth.&Type := TdxWidthUnitType(Value);
end;

procedure TdxNativeTable.SetRightPadding(const Value: Single);
begin
  Document.SetWidthUnitFixedValue(ModelTable.TableProperties.CellMargins.Right, Value);
end;

procedure TdxNativeTable.SetStyle(const Value: IdxRichEditTableStyle);
var
  AStyle: TdxTableStyle;
begin
  CheckValid;
  if Value <> nil then
    AStyle := TdxNativeTableStyle(Value).InnerStyle
  else
    AStyle := nil;
  ModelTable.StyleIndex := DocumentModel.TableStyles.IndexOf(AStyle);
end;

procedure TdxNativeTable.SetTableAlignment(const Value: TdxRichEditTableRowAlignment);
begin
  ModelTable.TableProperties.TableAlignment := Value;
end;

procedure TdxNativeTable.SetTableBackgroundColor(const Value: TdxAlphaColor);
begin
  Document.BeginUpdate;
  try
    ModelTable.TableProperties.BackgroundColor := Value;
    ForEachCell(
      procedure(const ACell: IdxRichEditTableCell; ARowIndex, ACellIndex: Integer)
      begin
        ACell.BackgroundColor := Value;
      end);
  finally
    Document.EndUpdate;
  end;
end;

procedure TdxNativeTable.SetTableCellSpacing(const Value: Single);
begin
  Document.SetWidthUnitFixedValue(ModelTable.TableProperties.CellSpacing, Value);
end;

procedure TdxNativeTable.SetTableLayout(const Value: TdxRichEditTableLayoutType);
begin
  ModelTable.TableProperties.TableLayout := Value;
end;

procedure TdxNativeTable.SetTableLook(const Value: TdxRichEditTableLookTypes);
begin
  ModelTable.TableLook := Value;
end;

procedure TdxNativeTable.SetTopPadding(const Value: Single);
begin
  Document.SetWidthUnitFixedValue(ModelTable.TableProperties.CellMargins.Top, Value);
end;

{ TdxNativeTableCollection }

constructor TdxNativeTableCollection.Create(ADocument: TdxNativeSubDocument);
begin
  inherited Create;
  FDocument := ADocument;
  FInnerList := TList<IdxRichEditTable>.Create;
end;

destructor TdxNativeTableCollection.Destroy;
begin
  FreeAndNil(FInnerList);
  inherited Destroy;
end;

function TdxNativeTableCollection.Add(const APos: IdxRichEditDocumentPosition;
  ARowCount, AColumnCount: Integer;
  AAutoFitBehavior: TdxRichEditAutoFitBehaviorType;
  AFixedColumnWidths: Integer): IdxRichEditTable;
var
  ALogPosition: TdxDocumentLogPosition;
  ANewTable: TdxTable;
  AStyleIndex: Integer;
begin
  FDocument.CheckValid;
  FDocument.CheckDocumentPosition(APos);

  ALogPosition := FDocument.NormalizeLogPosition(APos.LogPosition);
  DocumentModel.BeginUpdate;
  try
    ANewTable := PieceTable.InsertTable(ALogPosition, ARowCount, AColumnCount,
      TdxTableAutoFitBehaviorType(AAutoFitBehavior), FDocument.UnitsToModelUnits(AFixedColumnWidths));
    AStyleIndex := DocumentModel.TableStyles.GetStyleIndexByName(TdxTableStyleCollection.TableSimpleStyleName);
    if AStyleIndex >= 0 then
      ANewTable.StyleIndex := AStyleIndex;
  finally
    DocumentModel.EndUpdate;
  end;
  Result := Items[ANewTable.Index];
end;

procedure TdxNativeTableCollection.Delete(ATableIndex: Integer);
begin
  if (ATableIndex < 0) or (ATableIndex >= Count) then
    Exit;
  DocumentModel.BeginUpdate;
  try
    PieceTable.DeleteTableWithContent(PieceTable.Tables[ATableIndex]);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxNativeTableCollection.Clear;
begin
  FInnerList.Clear;
end;

function TdxNativeTableCollection.Get(
  const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyTableCollection;
var
  AParagraphsRange: TdxParagraphRange;
  ANativeRange: TdxNativeDocumentRange;
  AFirstIndex, ALastIndex, ATableStartParagraphIndex, ATableEndParagraphIndex: TdxParagraphIndex;
  AResult: TdxNativeTableCollection;
  ACount, I: Integer;
  ATable: IdxRichEditTable;
begin
  FDocument.CheckValid;
  FDocument.CheckDocumentRange(ARange);

  AParagraphsRange := FDocument.CalculateParagraphsRange(ARange);
  ANativeRange := TdxNativeDocumentRange(ARange);

  AFirstIndex := TdxNativeDocumentPosition(ANativeRange.NormalizedStart).Position.ParagraphIndex;
  ALastIndex := TdxNativeDocumentPosition(ANativeRange.NormalizedEnd).Position.ParagraphIndex;

  AResult := TdxNativeTableCollection.Create(FDocument);
  ACount := PieceTable.Tables.Count;
  for I := 0 to ACount - 1 do
  begin
    ATableStartParagraphIndex := PieceTable.Tables[I].StartParagraphIndex;
    ATableEndParagraphIndex := PieceTable.Tables[I].EndParagraphIndex;
    if (ATableStartParagraphIndex >= AFirstIndex) and (ATableEndParagraphIndex <= ALastIndex) then
    begin
      ATable := TdxNativeTable.Create(FDocument, PieceTable.Tables[I]);
      AResult.FInnerList.Add(ATable);
    end;
  end;
  Result := AResult;
end;

function TdxNativeTableCollection.GetDocumentModel: TdxDocumentModel;
begin
  Result := FDocument.DocumentModel;
end;

function TdxNativeTableCollection.GetPieceTable: TdxPieceTable;
begin
  Result := FDocument.PieceTable;
end;

function TdxNativeTableCollection.GetCount: Integer;
begin
  Result := FInnerList.Count;
end;

function TdxNativeTableCollection.GetFirst: IdxRichEditTable;
begin
  Result := FInnerList.First;
end;

function TdxNativeTableCollection.GetItem(Index: Integer): IdxRichEditTable;
begin
  Result := FInnerList[Index];
end;

function TdxNativeTableCollection.GetLast: IdxRichEditTable;
begin
  Result := FInnerList.Last;
end;

function TdxNativeTableCollection.GetTableCell(
  const APos: IdxRichEditDocumentPosition): IdxRichEditTableCell;
var
  AParagraph: IdxRichEditParagraph;
  ANativeParagraph: TdxNativeParagraph;
  ACell: TdxTableCell;
begin
  AParagraph := FDocument.Paragraphs.Get(APos);
  ANativeParagraph := TdxNativeParagraph(AParagraph);
  if ANativeParagraph = nil then
    Exit(nil);
  ACell := ANativeParagraph.InnerParagraph.GetCell;

  if ACell <> nil then
    Result := Self[ACell.Table.Index][ACell.RowIndex, ACell.IndexInRow]
  else
    Result := nil;
end;

function TdxNativeTableCollection.IndexOf(
  const ATable: IdxRichEditTable): Integer;
begin
  Result := FInnerList.IndexOf(ATable);
end;

procedure TdxNativeTableCollection.PopulateTables;
begin
  Clear;
  PieceTable.Tables.ForEach(RegisterTable);
end;

procedure TdxNativeTableCollection.TablesCollectionChanged(ATable: TdxTable; AAction: TListNotification);
begin
  case AAction of
    lnAdded:
      RegisterTable(ATable);
    lnExtracted, lnDeleted:
      UnregisterTable(ATable);
  end;
end;

procedure TdxNativeTableCollection.RegisterTable(const ATable: TdxTable);
var
  AComparer: IComparer<IdxRichEditTable>;
  AItem: IdxRichEditTable;
begin
  AItem := TdxNativeTable.Create(FDocument, ATable);
  FInnerList.Add(AItem);
  AComparer := TdxNativeTableComparer.Create;
  FInnerList.Sort(AComparer);
end;

procedure TdxNativeTableCollection.Remove(const ATable: IdxRichEditTable);
begin
  Delete(TdxNativeTable(ATable).ModelTable.Index);
end;

procedure TdxNativeTableCollection.UnregisterTable(const ATable: TdxTable);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if TdxNativeTable(Items[I]).ModelTable = ATable then
    begin
      TdxNativeTable(Items[I]).IsValid := False;
      FInnerList.Remove(Items[I]);
      Break;
    end;
  end;
end;

{ TdxNativeTableRowCollection }

constructor TdxNativeTableRowCollection.Create(ATable: TdxNativeTable);
begin
  inherited Create;
  FTable := ATable;
  FCachedItems := TDictionary<Integer, IdxRichEditTableRow>.Create;
end;

destructor TdxNativeTableRowCollection.Destroy;
begin
  FreeAndNil(FCachedItems);
  inherited Destroy;
end;

function TdxNativeTableRowCollection.Append: IdxRichEditTableRow;
var
  ACount: Integer;
begin
  ACount := Count;
  if ACount <= 0 then
    Exit(nil);
  PieceTable.InsertTableRowBelow(FTable.ModelTable.Rows[ACount - 1], False);
  Result := Last;
end;

procedure TdxNativeTableRowCollection.Delete(ARowIndex: Integer);
begin
  if (ARowIndex < 0) or (ARowIndex >= Count) then
    Exit;
  PieceTable.DocumentModel.BeginUpdate;
  try
    if Count = 1 then
      PieceTable.DeleteTableWithContent(FTable.ModelTable)
    else
    begin
      PieceTable.DeleteTableRowWithContent(FTable.ModelTable.Rows[ARowIndex]);
      FTable.ModelTable.NormalizeCellColumnSpans;
    end;
  finally
    PieceTable.DocumentModel.EndUpdate;
  end;
end;

function TdxNativeTableRowCollection.InsertBefore(ARowIndex: Integer): IdxRichEditTableRow;
begin
  if (ARowIndex < 0) or (ARowIndex >= Count) then
    Exit(nil);
  PieceTable.InsertTableRowAbove(FTable.ModelTable.Rows[ARowIndex], False);
  Result := GetItem(ARowIndex);
end;

function TdxNativeTableRowCollection.InsertAfter(ARowIndex: Integer): IdxRichEditTableRow;
begin
  if (ARowIndex < 0) or (ARowIndex >= Count) then
    Exit(nil);
  PieceTable.InsertTableRowBelow(FTable.ModelTable.Rows[ARowIndex], False);
  Result := GetItem(ARowIndex + 1);
end;

function TdxNativeTableRowCollection.GetPieceTable: TdxPieceTable;
begin
  Result := FTable.Document.PieceTable;
end;

function TdxNativeTableRowCollection.GetCount: Integer;
begin
  Result := FTable.ModelTable.Rows.Count;
end;

function TdxNativeTableRowCollection.GetFirst: IdxRichEditTableRow;
begin
  if Count > 0 then
    Result := GetItem(0)
  else
    Result := nil;
end;

function TdxNativeTableRowCollection.GetItem(Index: Integer): IdxRichEditTableRow;
begin
  Result := nil;
  if Index < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Index >= Count then
  begin
    if FCachedItems.ContainsKey(Index) then
      FCachedItems.Remove(Index);
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  end;
  if not FCachedItems.TryGetValue(Index, Result) then
  begin
    Result := TdxNativeTableRow.Create(FTable, Index);
    FCachedItems.Add(Index, Result);
  end;
end;

function TdxNativeTableRowCollection.GetLast: IdxRichEditTableRow;
begin
  if Count > 0 then
    Result := GetItem(Count - 1)
  else
    Result := nil;
end;

{ TdxNativeTableCellCollection }

constructor TdxNativeTableCellCollection.Create(ARow: TdxNativeTableRow);
begin
  inherited Create;
  FRow := ARow;
  FCachedItems := TDictionary<Integer, IdxRichEditTableCell>.Create;
end;

destructor TdxNativeTableCellCollection.Destroy;
begin
  FreeAndNil(FCachedItems);
  inherited Destroy;
end;

function TdxNativeTableCellCollection.Append: IdxRichEditTableCell;
var
  ACell: TdxTableCell;
begin
  if Count <= 0 then
    Exit(nil);

  ACell := FRow.ModelRow.Cells.Last;
  PieceTable.InsertColumnToTheRight(ACell, False);
  Result := Last;
end;

procedure TdxNativeTableCellCollection.Delete(AColumnIndex: Integer);
begin
  if (AColumnIndex < 0) or (AColumnIndex >= Count) then
    Exit;
  PieceTable.DeleteTableCellWithContent(FRow.ModelRow.Cells[AColumnIndex], Document.DocumentServer.Owner);
end;

function TdxNativeTableCellCollection.InsertBefore(AColumnIndex: Integer): IdxRichEditTableCell;
var
  ACell: TdxTableCell;
begin
  if (AColumnIndex < 0) or (AColumnIndex >= Count) then
    Exit(nil);

  ACell := FRow.ModelRow.Cells[AColumnIndex];
  PieceTable.InsertColumnToTheLeft(ACell, False);
  Result := GetItem(AColumnIndex);
end;

function TdxNativeTableCellCollection.InsertAfter(AColumnIndex: Integer): IdxRichEditTableCell;
var
  ACell: TdxTableCell;
begin
  if (AColumnIndex < 0) or (AColumnIndex >= Count) then
    Exit(nil);

  ACell := FRow.ModelRow.Cells[AColumnIndex];
  PieceTable.InsertColumnToTheRight(ACell, False);
  Result := GetItem(AColumnIndex + 1);
end;

function TdxNativeTableCellCollection.GetDocument: TdxNativeSubDocument;
begin
  Result := FRow.NativeTable.Document;
end;

function TdxNativeTableCellCollection.GetPieceTable: TdxPieceTable;
begin
  Result := Document.PieceTable;
end;

function TdxNativeTableCellCollection.GetCount: Integer;
begin
  Result := FRow.ModelRow.Cells.Count;
end;

function TdxNativeTableCellCollection.GetFirst: IdxRichEditTableCell;
begin
  if Count > 0 then
    Result := GetItem(0)
  else
    Result := nil;
end;

function TdxNativeTableCellCollection.GetItem(Index: Integer): IdxRichEditTableCell;
begin
  Result := nil;
  if Index < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Index >= Count then
  begin
    if FCachedItems.ContainsKey(Index) then
      FCachedItems.Remove(Index);
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  end;
  if not FCachedItems.TryGetValue(Index, Result) then
  begin
    Result := TdxNativeTableCell.Create(FRow, Index);
    FCachedItems.Add(Index, Result);
  end;
end;

function TdxNativeTableCellCollection.GetLast: IdxRichEditTableCell;
begin
  if Count > 0 then
    Result := GetItem(Count - 1)
  else
    Result := nil;
end;

{ TdxNativeTableBorderBase }

function TdxNativeTableBorderBase.GetLineColorCore: TdxAlphaColor;
begin
  Result := GetActualBorder.Color;
end;

function TdxNativeTableBorderBase.GetLineStyleCore: TdxBorderLineStyle;
begin
  Result := GetActualBorder.Style;
end;

function TdxNativeTableBorderBase.GetLineThicknessCore: Integer;
begin
  Result := GetActualBorder.Width;
end;

procedure TdxNativeTableBorderBase.SetLineColorCore(const Value: TdxAlphaColor);
begin
  SetBorderValues(
    procedure(const ABorder: TdxBorderBase)
    begin
      ABorder.Color := Value;
    end);
end;

procedure TdxNativeTableBorderBase.SetLineStyleCore(const Value: TdxBorderLineStyle);
begin
  SetBorderValues(
    procedure(const ABorder: TdxBorderBase)
    begin
      ABorder.Style := Value;
    end);
end;

procedure TdxNativeTableBorderBase.SetLineThicknessCore(const Value: Integer);
begin
  SetBorderValues(
    procedure(const ABorder: TdxBorderBase)
    begin
      ABorder.Width := Value;
    end);
end;

procedure TdxNativeTableBorderBase.SetBorderValues(const ASetter: TBorderValueSetter);
var
  ABorder: TdxBorderBase;
begin
  ABorder := GetBorder;
  ABorder.CopyFrom(GetActualBorder);
  ASetter(ABorder);
end;

{ TdxNativeTableCellBorder }

constructor TdxNativeTableCellBorder.Create(ACell: TdxNativeTableCell);
begin
  inherited Create(ACell.Document);
  FCell := ACell;
end;

{ TdxNativeTableBottomCellBorder }

function TdxNativeTableBottomCellBorder.GetActualBorder: TdxBorderBase;
begin
  Result := Cell.ModelCell.GetActualBottomCellBorder;
end;

function TdxNativeTableBottomCellBorder.GetBorder: TdxBorderBase;
begin
  Result := Cell.ModelCell.Properties.Borders.BottomBorder;
end;

{ TdxNativeTableLeftCellBorder }

function TdxNativeTableLeftCellBorder.GetActualBorder: TdxBorderBase;
begin
  Result := Cell.ModelCell.GetActualLeftCellBorder;
end;

function TdxNativeTableLeftCellBorder.GetBorder: TdxBorderBase;
begin
  Result := Cell.ModelCell.Properties.Borders.LeftBorder;
end;

{ TdxNativeTableRightCellBorder }

function TdxNativeTableRightCellBorder.GetActualBorder: TdxBorderBase;
begin
  Result := Cell.ModelCell.GetActualRightCellBorder;
end;

function TdxNativeTableRightCellBorder.GetBorder: TdxBorderBase;
begin
  Result := Cell.ModelCell.Properties.Borders.RightBorder;
end;

{ TdxNativeTableTopCellBorder }

function TdxNativeTableTopCellBorder.GetActualBorder: TdxBorderBase;
begin
  Result := Cell.ModelCell.GetActualTopCellBorder;
end;

function TdxNativeTableTopCellBorder.GetBorder: TdxBorderBase;
begin
  Result := Cell.ModelCell.Properties.Borders.TopBorder;
end;

{ TdxNativeTableCellBorders }

constructor TdxNativeTableCellBorders.Create(ACell: TdxNativeTableCell);
begin
  inherited Create(ACell.Document);
  FCell := ACell;
end;

function TdxNativeTableCellBorders.CreateBottom: IdxRichEditTableCellBorder;
begin
  Result := TdxNativeTableBottomCellBorder.Create(Cell);
end;

function TdxNativeTableCellBorders.CreateLeft: IdxRichEditTableCellBorder;
begin
  Result := TdxNativeTableLeftCellBorder.Create(Cell);
end;

function TdxNativeTableCellBorders.CreateRight: IdxRichEditTableCellBorder;
begin
  Result := TdxNativeTableRightCellBorder.Create(Cell);
end;

function TdxNativeTableCellBorders.CreateTop: IdxRichEditTableCellBorder;
begin
  Result := TdxNativeTableTopCellBorder.Create(Cell);
end;

{ TdxNativeTableBorder }

constructor TdxNativeTableBorder.Create(ATable: TdxNativeTable);
begin
  inherited Create(ATable.Document);
  FTable := ATable;
end;

{ TdxNativeTableBottomBorder }

function TdxNativeTableBottomBorder.GetActualBorder: TdxBorderBase;
begin
  Result := Table.ModelTable.GetActualBottomBorder;
end;

function TdxNativeTableBottomBorder.GetBorder: TdxBorderBase;
begin
  Result := Table.ModelTable.TableProperties.Borders.BottomBorder;
end;

{ TdxNativeTableLeftBorder }

function TdxNativeTableLeftBorder.GetActualBorder: TdxBorderBase;
begin
  Result := Table.ModelTable.GetActualLeftBorder;
end;

function TdxNativeTableLeftBorder.GetBorder: TdxBorderBase;
begin
  Result := Table.ModelTable.TableProperties.Borders.LeftBorder;
end;

{ TdxNativeTableRightBorder }

function TdxNativeTableRightBorder.GetActualBorder: TdxBorderBase;
begin
  Result := Table.ModelTable.GetActualRightBorder;
end;

function TdxNativeTableRightBorder.GetBorder: TdxBorderBase;
begin
  Result := Table.ModelTable.TableProperties.Borders.RightBorder;
end;

{ TdxNativeTableTopBorder }

function TdxNativeTableTopBorder.GetActualBorder: TdxBorderBase;
begin
  Result := Table.ModelTable.GetActualTopBorder;
end;

function TdxNativeTableTopBorder.GetBorder: TdxBorderBase;
begin
  Result := Table.ModelTable.TableProperties.Borders.TopBorder;
end;

{ TdxNativeTableInsideHorizontalBorder }

function TdxNativeTableInsideHorizontalBorder.GetActualBorder: TdxBorderBase;
begin
  Result := Table.ModelTable.GetActualInsideHorizontalBorder;
end;

function TdxNativeTableInsideHorizontalBorder.GetBorder: TdxBorderBase;
begin
  Result := Table.ModelTable.TableProperties.Borders.InsideHorizontalBorder;
end;

{ TdxNativeTableInsideVerticalBorder }

function TdxNativeTableInsideVerticalBorder.GetActualBorder: TdxBorderBase;
begin
  Result := Table.ModelTable.GetActualInsideVerticalBorder;
end;

function TdxNativeTableInsideVerticalBorder.GetBorder: TdxBorderBase;
begin
  Result := Table.ModelTable.TableProperties.Borders.InsideVerticalBorder;
end;

{ TdxNativeTableBorders }

constructor TdxNativeTableBorders.Create(ATable: TdxNativeTable);
begin
  inherited Create(ATable.Document);
  FTable := ATable;
end;

function TdxNativeTableBorders.CreateBottom: IdxRichEditTableBorder;
begin
  Result := TdxNativeTableBottomBorder.Create(Table);
end;

function TdxNativeTableBorders.CreateInsideHorizontalBorder: IdxRichEditTableBorder;
begin
  Result := TdxNativeTableInsideHorizontalBorder.Create(Table);
end;

function TdxNativeTableBorders.CreateInsideVerticalBorder: IdxRichEditTableBorder;
begin
  Result := TdxNativeTableInsideVerticalBorder.Create(Table);
end;

function TdxNativeTableBorders.CreateLeft: IdxRichEditTableBorder;
begin
  Result := TdxNativeTableLeftBorder.Create(Table);
end;

function TdxNativeTableBorders.CreateRight: IdxRichEditTableBorder;
begin
  Result := TdxNativeTableRightBorder.Create(Table);
end;

function TdxNativeTableBorders.CreateTop: IdxRichEditTableBorder;
begin
  Result := TdxNativeTableTopBorder.Create(Table);
end;

end.
