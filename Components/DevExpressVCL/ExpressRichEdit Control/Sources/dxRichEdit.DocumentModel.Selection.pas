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

unit dxRichEdit.DocumentModel.Selection;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCoreClasses,

  dxGenerics,
  dxRichEdit.Control.HotZones,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Selections.Core,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.DocumentLayout.UnitConverter;

type
  TdxRectangularObjectSelectionLayout = class;
  TdxRowSelectionLayoutBase = class;
  TdxFloatingObjectAnchorSelectionLayout = class;
  TdxSelectedCellsCollection = class;

  { IdxSelectionPainter }

  IdxSelectionPainter = interface
  ['{5E9AA6D1-9F74-4BD6-834C-3A89C283C95C}']
    procedure Draw(AViewInfo: TdxRectangularObjectSelectionLayout); overload;
    procedure Draw(AViewInfo: TdxRowSelectionLayoutBase); overload;
    procedure Draw(AViewInfo: TdxFloatingObjectAnchorSelectionLayout); overload;
  end;

  { IdxSelectionLayoutItem }

  IdxSelectionLayoutItem = interface
  ['{0CC8B2CD-CD1B-4E94-A415-22C7F3611A14}']
    procedure Draw(const ASelectionPainter: IdxSelectionPainter);
    function Update: Boolean;
    function HitTest(ALogPosition: TdxDocumentLogPosition; const ALogicalPoint: TPoint): Boolean;
    function GetType: TClass;
  end;
  TdxSelectionLayoutItemList = class(TInterfaceList)
  private
    function GetItem(Index: Integer): IdxSelectionLayoutItem;
  public
    procedure AddRange(ARange: TdxSelectionLayoutItemList);
    function First: IdxSelectionLayoutItem; reintroduce;
    function Last: IdxSelectionLayoutItem; reintroduce;

    property Items[Index: Integer]: IdxSelectionLayoutItem read GetItem; default;
  end;
  TdxPageSelectionLayoutsCollection = TdxSelectionLayoutItemList;

  { TdxTableStructureBySelectionCalculator}

  TdxTableStructureBySelectionCalculator = class
  strict private
    FPieceTable: TdxPieceTable;
  public
    constructor Create(APieceTable: TdxPieceTable);
    function CalculateSelectedCells(AResult: TdxSelectedCellsCollection; ARow: TdxTableRow; ALeftColumnIndex: Integer; ARightColumnIndex: Integer; AIsLeftToRight: Boolean): Boolean;
    function SetStartCell(APos: TdxDocumentLogPosition): TdxSelectedTableStructureBase;
    function Calculate(AStartCell: TdxTableCell; AEndCell: TdxTableCell): TdxSelectedCellsCollection; overload;
    function Calculate(AStartCell: TdxTableCell; AEndCell: TdxTableCell; AIsColumnSelected: Boolean): TdxSelectedCellsCollection; overload;
    function CalculateCore(AStartCell: TdxTableCell; AEndCell: TdxTableCell; ATable: TdxTable; AIsColumnSelected: Boolean): TdxSelectedCellsCollection;

    property ActivePieceTable: TdxPieceTable read FPieceTable;
  end;

  { TdxStartSelectedCellInTable }

  TdxStartSelectedCellInTable = class(TdxSelectedTableStructureBase)
  private
    FFirstSelectedCell: TdxTableCell;
  protected
    function GetFirstSelectedCell: TdxTableCell; override;
    function GetIsNotEmpty: Boolean; override;
    function GetSelectedOnlyOneCell: Boolean; override;
    function GetRowsCount: Integer; override;
  public
    constructor Create(AFirstSelectedCell: TdxTableCell; AOriginalStartLogPosition: TdxDocumentLogPosition); overload;
    constructor Create(AOld: TdxSelectedTableStructureBase); overload;
    procedure SetFirstSelectedCell(AStartCell: TdxTableCell; APos: TdxDocumentLogPosition); override;

    property FirstSelectedCell: TdxTableCell read FFirstSelectedCell;
  end;

  { TdxRowSelectionLayoutBase }

  TdxRowSelectionLayoutBase = class abstract(TInterfacedObject, IdxSelectionLayoutItem)
  strict private
    FBounds: TRect;
    FPage: TdxPage;
  protected
    procedure Draw(const ASelectionPainter: IdxSelectionPainter);
    function GetType: TClass;
    function HitTest(ALogPosition: TdxDocumentLogPosition; const ALogicalPoint: TPoint): Boolean;
    function Update: Boolean; virtual; abstract;
  public
    constructor Create(APage: TdxPage);

    property Bounds: TRect read FBounds write FBounds;
    property Page: TdxPage read FPage;
  end;

  { TdxRowSelectionLayout }

  TdxRowSelectionLayout = class(TdxRowSelectionLayoutBase)
  strict private
    FEnd: TdxDocumentLayoutPosition;
    FStart: TdxDocumentLayoutPosition;
  protected
    function Update: Boolean; override;
  public
    constructor Create(AStart, AEnd: TdxDocumentLayoutPosition; APage: TdxPage);
    destructor Destroy; override;

    property &End: TdxDocumentLayoutPosition read FEnd;
    property Start: TdxDocumentLayoutPosition read FStart;
  end;

  { TdxEntireRowSelectionLayout }

  TdxEntireRowSelectionLayout = class(TdxRowSelectionLayoutBase)
  strict private
    FEnd: TdxDocumentLayoutPosition;
    FStart: TdxDocumentLayoutPosition;
  protected
    function Update: Boolean; override;
  public
    constructor Create(AStart, AEnd: TdxDocumentLayoutPosition; APage: TdxPage);
    destructor Destroy; override;

    property &End: TdxDocumentLayoutPosition read FEnd;
    property Start: TdxDocumentLayoutPosition read FStart;
  end;

  { TdxTableCellSelectionLayout }

  TdxTableCellSelectionLayout = class(TdxRowSelectionLayoutBase)
  strict private
    FCell: TdxTableCellViewInfo;
  protected
    property Cell: TdxTableCellViewInfo read FCell;
  public
    constructor Create(ACell: TdxTableCellViewInfo; APage: TdxPage);
    function Update: Boolean; override;
  end;

  { TdxRectangularObjectSelectionLayoutBase }

  TdxRectangularObjectSelectionLayoutBase = class abstract(TInterfacedObject, IdxSelectionLayoutItem)
  private
    FView: TObject;
    FBox: TdxBox;
    FLogStart: TdxDocumentLogPosition;
    FLogEnd: TdxDocumentLogPosition;
    FPieceTable: TdxPieceTable;
    FUnitConverter: TdxDocumentLayoutUnitConverter;
  protected
    procedure Draw(const ASelectionPainter: IdxSelectionPainter); virtual; abstract;
    function Update: Boolean; virtual; abstract;
    function HitTest(ALogPosition: TdxDocumentLogPosition; const ALogicalPoint: TPoint): Boolean; virtual; abstract;
    function GetType: TClass;

  public
    constructor Create(AView: TObject; ABox: TdxBox; AStart: TdxDocumentLogPosition; APieceTable: TdxPieceTable);
    destructor Destroy; override;

    property LogStart: TdxDocumentLogPosition read FLogStart;
    property LogEnd: TdxDocumentLogPosition read FLogEnd;
    property Box: TdxBox read FBox;
    property View: TObject read FView;
    property PieceTable: TdxPieceTable read FPieceTable;
    property UnitConverter: TdxDocumentLayoutUnitConverter read FUnitConverter;
  end;

  { TdxRectangularObjectSelectionLayout }

  TdxRectangularObjectSelectionLayout = class(TdxRectangularObjectSelectionLayoutBase)
  private
    FHitTestTransform: TdxTransformMatrix;
    FAnchor: TdxFloatingObjectAnchorSelectionLayout;
  protected
    function GetResizeable: Boolean; virtual;
  public
    destructor Destroy; override;
    property HitTestTransform: TdxTransformMatrix read FHitTestTransform write FHitTestTransform;
    property Anchor: TdxFloatingObjectAnchorSelectionLayout read FAnchor write FAnchor;
    property Resizeable: Boolean read GetResizeable;

    procedure Draw(const ASelectionPainter: IdxSelectionPainter); override;
    function Update: Boolean; override;
    function HitTest(ALogPosition: TdxDocumentLogPosition; const ALogicalPoint: TPoint): Boolean; override;
  end;

  { TdxResizeableRectangularObjectSelectionLayout }

  TdxResizeableRectangularObjectSelectionLayout = class(TdxRectangularObjectSelectionLayout)
  public const
    DefaultHotZoneSizeDocuments   = 24;
    DeltaRadiusForExtendedHotZone = 75;
  strict private
    FHotZoneSize: Integer;
  protected
    function GetResizeable: Boolean; override;
  public
    constructor Create(AView: TObject{TdxRichEditView}; ABox: TdxBox; AStart: TdxDocumentLogPosition; APieceTable: TdxPieceTable);
    function Update: Boolean; override;
    procedure AddHotZone(AHotZone: TdxHotZone; const ABounds: TRect);
    function TryCreateAnchorSelectionItem: TdxFloatingObjectAnchorSelectionLayout;
    function CalculateExtendedBounds(const R: TRect): TRect;
    function RectangleFromCenter(X: Integer; Y: Integer; ALargeHotZoneSize: Integer): TRect;
    function AlignSizeToPixel(ALayoutSize: Integer): Integer;
  end;

  { TdxFloatingObjectAnchorSelectionLayout }

  TdxFloatingObjectAnchorSelectionLayout = class(TdxRectangularObjectSelectionLayoutBase)
  private
    FAnchorBounds: TRect;
  public
    procedure Draw(const ASelectionPainter: IdxSelectionPainter); override;
    function Update: Boolean; override;
    function HitTest(ALogPosition: TdxDocumentLogPosition; const ALogicalPoint: TPoint): Boolean; override;

    property AnchorBounds: TRect read FAnchorBounds write FAnchorBounds;
  end;

  { TdxSelectedCellsCollection }

  TdxSelectedCellsCollection = class(TdxSelectedTableStructureBase)
  private
    FInnerList: TdxSelectedCellsIntervalInRowList;
    function GetNormalizedFirst: TdxSelectedCellsIntervalInRow;
    function GetNormalizedLast: TdxSelectedCellsIntervalInRow;
    function GetFirst: TdxSelectedCellsIntervalInRow;
    function GetLast: TdxSelectedCellsIntervalInRow;
    function GetTopLeftCell: TdxTableCell;
    function GetItem(Index: Integer): TdxSelectedCellsIntervalInRow;
    procedure SetItem(Index: Integer; const Value: TdxSelectedCellsIntervalInRow);
  protected
    function GetFirstSelectedCell: TdxTableCell; override;
    function GetSelectedOnlyOneCell: Boolean; override;
    function GetIsNotEmpty: Boolean; override;
    function GetRowsCount: Integer; override;
    function GetSelectedColumnsCountInRow(ASelectionInterval: TdxSelectedCellsIntervalInRow): Integer; virtual;
    function GetSelectedRowsCountCore(ACell: TdxTableCell): Integer; virtual;
    function GetMergedCellsCount(ACurrentCell: TdxTableCell): Integer; virtual;
    procedure Clear; virtual;
  public
    constructor Create; overload;
    constructor Create(AFirstSelectedCell: TdxTableCell;
      AOriginalStartLogPosition: TdxDocumentLogPosition); overload;
    constructor Create(AOld: TdxStartSelectedCellInTable); overload;
    destructor Destroy; override;
    procedure AddSelectedCells(ARow: TdxTableRow; AStart: Integer; AEnd: Integer); virtual;
    function IsSquare: Boolean;
    function GetTopRowIndex: Integer;
    function GetBottomRowIndex: Integer;
    function GetNormalizedTopRowIndex: Integer;
    function GetNormalizedBottomRowIndex: Integer;
    function GetSelectedRowsCount(AColumnIndex: Integer): Integer;
    procedure SetFirstSelectedCell(AStartCell: TdxTableCell; APos: TdxDocumentLogPosition); override;
    function GetSelectedTableRows: TdxTableRowList;
    function IsWholeCellSelected(ACell: TdxTableCell): Boolean; virtual;

    procedure Add(AItem: TdxSelectedCellsIntervalInRow); virtual;
    procedure Remove(AItem: TdxSelectedCellsIntervalInRow); virtual;

    function IsSelectedEntireTable: Boolean; virtual;
    property First: TdxSelectedCellsIntervalInRow read GetFirst;
    property Last: TdxSelectedCellsIntervalInRow read GetLast;

    // for internal use
    function IsSelectedEntireTableRows: Boolean; virtual;
    function IsSelectedEntireTableColumns: Boolean; virtual;

    property Items[Index: Integer]: TdxSelectedCellsIntervalInRow read GetItem write SetItem; default;
    property NormalizedFirst: TdxSelectedCellsIntervalInRow read GetNormalizedFirst;
    property NormalizedLast: TdxSelectedCellsIntervalInRow read GetNormalizedLast;
    property TopLeftCell: TdxTableCell read GetTopLeftCell;
    property FirstSelectedCell: TdxTableCell read GetFirstSelectedCell;
    property SelectedOnlyOneCell: Boolean read GetSelectedOnlyOneCell;
    property IsNotEmpty: Boolean read GetIsNotEmpty;
    property RowsCount: Integer read GetRowsCount;
  end;

implementation

uses
  Contnrs, Math, dxCore, dxTypeHelpers,

  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.View.Core,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.TableFormatting;

type

  { TdxRectangularObjectSelectionLayoutBaseHelper }

  TdxRectangularObjectSelectionLayoutBaseHelper = class helper for TdxRectangularObjectSelectionLayoutBase
  private
    function GetView: TdxRichEditView;
  public
    property View: TdxRichEditView read GetView;
  end;

  function TdxRectangularObjectSelectionLayoutBaseHelper.GetView: TdxRichEditView;
  begin
    Result := TdxRichEditView(inherited View);
  end;

{ TdxSelectionLayoutItemList }

function TdxSelectionLayoutItemList.GetItem(Index: Integer): IdxSelectionLayoutItem;
begin
  Result := inherited Items[Index] as IdxSelectionLayoutItem;
end;

procedure TdxSelectionLayoutItemList.AddRange(ARange: TdxSelectionLayoutItemList);
var
  I: Integer;
begin
  for I := 0 to ARange.Count - 1 do
    Add(ARange[I]);
end;

function TdxSelectionLayoutItemList.First: IdxSelectionLayoutItem;
begin
  Result := inherited First as IdxSelectionLayoutItem;
end;

function TdxSelectionLayoutItemList.Last: IdxSelectionLayoutItem;
begin
  Result := inherited Last as IdxSelectionLayoutItem;
end;

{ TdxTableStructureBySelectionCalculator }

constructor TdxTableStructureBySelectionCalculator.Create(
  APieceTable: TdxPieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
end;

function TdxTableStructureBySelectionCalculator.CalculateSelectedCells(AResult: TdxSelectedCellsCollection; ARow: TdxTableRow; ALeftColumnIndex: Integer; ARightColumnIndex: Integer; AIsLeftToRight: Boolean): Boolean;
var
  AColumnIndex, AStart, AEnd, ACount, ACellIndex: Integer;
begin
  AColumnIndex := ARow.GridBefore;
  AStart := MaxInt;
  AEnd   := MinInt;
  ACount := ARow.Cells.Count;
  for ACellIndex := 0 to ACount - 1 do
  begin
    Inc(AColumnIndex, ARow.Cells[ACellIndex].ColumnSpan);
    if ALeftColumnIndex < AColumnIndex then
      AStart := Min(AStart, ACellIndex);
    if ARightColumnIndex < AColumnIndex then
    begin
      AEnd := Max(AEnd, ACellIndex);
      Break;
    end;
    AEnd := Max(AEnd, ACellIndex);
  end;

  if (AStart = MaxInt) or (AEnd = MinInt) then
    Exit(False);

  if AIsLeftToRight then
    AResult.AddSelectedCells(ARow, AStart, AEnd)
  else
    AResult.AddSelectedCells(ARow, AEnd, AStart);

  Result := True;
end;

function TdxTableStructureBySelectionCalculator.SetStartCell(
  APos: TdxDocumentLogPosition): TdxSelectedTableStructureBase;
var
  AStartCell: TdxTableCell;
begin
  AStartCell := ActivePieceTable.FindParagraph(APos).GetCell;
  if AStartCell = nil then
    Result := TdxStartSelectedCellInTable.Create(nil, APos)
  else
  begin
    Result := TdxSelectedCellsCollection.Create;
    Result.SetFirstSelectedCell(AStartCell, APos);
  end;
end;

function TdxTableStructureBySelectionCalculator.Calculate(AStartCell: TdxTableCell; AEndCell: TdxTableCell): TdxSelectedCellsCollection;
begin
  Result := Calculate(AStartCell, AEndCell, False);
end;

function TdxTableStructureBySelectionCalculator.Calculate(AStartCell: TdxTableCell; AEndCell: TdxTableCell; AIsColumnSelected: Boolean): TdxSelectedCellsCollection;
var
  ATable: TdxTable;
begin
  if AStartCell.Table = AEndCell.Table then
    ATable := AStartCell.Table
  else
    raise TdxInternalException.Create;

  Result := CalculateCore(AStartCell, AEndCell, ATable, AIsColumnSelected);
end;

function TdxTableStructureBySelectionCalculator.CalculateCore(AStartCell: TdxTableCell; AEndCell: TdxTableCell; ATable: TdxTable; AIsColumnSelected: Boolean): TdxSelectedCellsCollection;
var
  AStartCellColumnIndex, AEndCellColumnIndex, AStartCellRowIndex, AEndCellRowIndex, ANormalizedLeft, ANormalizedRight, ARightColumnIndex, I {INDENTIFIER REDECLARED} {INDENTIFIER REDECLARED}: Integer;
  AIsLeftToRightDirection: Boolean;
begin
  AStartCellColumnIndex := AStartCell.GetStartColumnIndexConsiderRowGrid;
  AEndCellColumnIndex := AEndCell.GetStartColumnIndexConsiderRowGrid;
  AStartCellRowIndex := AStartCell.RowIndex;
  AEndCellRowIndex := AEndCell.RowIndex;
  if AEndCellRowIndex <> AStartCellRowIndex then
    AIsLeftToRightDirection := AEndCellRowIndex >= AStartCellRowIndex
  else
    AIsLeftToRightDirection := AEndCellColumnIndex >= AStartCellColumnIndex;

  ANormalizedLeft := Min(AStartCellColumnIndex, AEndCellColumnIndex);
  ANormalizedRight := Max(AStartCellColumnIndex, AEndCellColumnIndex);
  if AIsColumnSelected then
    AEndCellRowIndex := AStartCell.Table.Rows.Count - 1;

  Result := TdxSelectedCellsCollection.Create;

  if AIsLeftToRightDirection then
  begin
    if AIsColumnSelected then
      ARightColumnIndex := ANormalizedRight + AStartCell.ColumnSpan - 1
    else
      ARightColumnIndex := ANormalizedRight + AEndCell.ColumnSpan - 1;
    for I := AStartCellRowIndex to AEndCellRowIndex do
      if not CalculateSelectedCells(Result, ATable.Rows[I], ANormalizedLeft, ARightColumnIndex, AIsLeftToRightDirection) then
        Break;
  end
  else
  begin
    ARightColumnIndex := ANormalizedRight + AStartCell.ColumnSpan - 1;
    for I := AStartCellRowIndex downto AEndCellRowIndex do
      if not CalculateSelectedCells(Result, ATable.Rows[I], ANormalizedLeft, ARightColumnIndex, AIsLeftToRightDirection) then
        Break;
  end;
end;

{ TdxStartSelectedCellInTable }

constructor TdxStartSelectedCellInTable.Create(AFirstSelectedCell: TdxTableCell;
  AOriginalStartLogPosition: TdxDocumentLogPosition);
begin
  inherited Create(AOriginalStartLogPosition);
  FFirstSelectedCell := AFirstSelectedCell;
end;

constructor TdxStartSelectedCellInTable.Create(AOld: TdxSelectedTableStructureBase);
begin
  Create(AOld.FirstSelectedCell, AOld.OriginalStartLogPosition);
end;

function TdxStartSelectedCellInTable.GetFirstSelectedCell: TdxTableCell;
begin
  Result := FFirstSelectedCell;
end;

function TdxStartSelectedCellInTable.GetIsNotEmpty: Boolean;
begin
  Result := FFirstSelectedCell <> nil;
end;

function TdxStartSelectedCellInTable.GetRowsCount: Integer;
begin
  if FirstSelectedCell = nil then
    Result := 0
  else
    Result := 1;
end;

function TdxStartSelectedCellInTable.GetSelectedOnlyOneCell: Boolean;
begin
  Result := True;
end;

procedure TdxStartSelectedCellInTable.SetFirstSelectedCell(
  AStartCell: TdxTableCell; APos: TdxDocumentLogPosition);
begin
  FFirstSelectedCell := AStartCell;
  OriginalStartLogPosition := APos;
end;

{ TdxRowSelectionLayoutBase }

constructor TdxRowSelectionLayoutBase.Create(APage: TdxPage);
begin
  inherited Create;
  FPage := APage;
end;

procedure TdxRowSelectionLayoutBase.Draw(
  const ASelectionPainter: IdxSelectionPainter);
begin
  ASelectionPainter.Draw(Self);
end;

function TdxRowSelectionLayoutBase.GetType: TClass;
begin
  Result := ClassType;
end;

function TdxRowSelectionLayoutBase.HitTest(ALogPosition: TdxDocumentLogPosition;
  const ALogicalPoint: TPoint): Boolean;
begin
  Result := Bounds.Contains(ALogicalPoint);
end;

{ TdxRectangularObjectSelectionLayoutBase }

constructor TdxRectangularObjectSelectionLayoutBase.Create(AView: TObject; ABox: TdxBox; AStart: TdxDocumentLogPosition;
  APieceTable: TdxPieceTable);
begin
  inherited Create;
  Assert(AView <> nil);
  Assert(ABox <> nil);
  FUnitConverter := TdxRichEditView(AView).DocumentModel.LayoutUnitConverter;
  FView := AView;
  FBox := ABox;
  TdxBox.AddReference(FBox);
  FLogStart := AStart;
  FLogEnd := AStart + 1;
  FPieceTable := APieceTable;
end;

destructor TdxRectangularObjectSelectionLayoutBase.Destroy;
begin
  TdxBox.Release(FBox);
  inherited Destroy;
end;

function TdxRectangularObjectSelectionLayoutBase.GetType: TClass;
begin
  Result := ClassType;
end;

{ TdxRowSelectionLayout }

constructor TdxRowSelectionLayout.Create(AStart,
  AEnd: TdxDocumentLayoutPosition; APage: TdxPage);
begin
  inherited Create(APage);
  Assert(AStart.IsValid(TdxDocumentLayoutDetailsLevel.Character));
  Assert(AEnd.IsValid(TdxDocumentLayoutDetailsLevel.Character));
  FStart := TdxDocumentLayoutPosition(AStart.Clone);
  FEnd := TdxDocumentLayoutPosition(AEnd.Clone);
end;

destructor TdxRowSelectionLayout.Destroy;
begin
  FreeAndNil(FStart);
  FreeAndNil(FEnd);
  inherited Destroy;
end;

function TdxRowSelectionLayout.Update: Boolean;
var
  AStartCharacterBounds, AEndCharacterBounds: TRect;
begin
  AStartCharacterBounds := Start.Character.Bounds;
  AEndCharacterBounds := &End.Character.Bounds;
  Bounds := TRect.Create(AStartCharacterBounds.Left, AStartCharacterBounds.Top,
    AEndCharacterBounds.Right, AEndCharacterBounds.Bottom);
  Result := True;
end;

{ TdxEntireRowSelectionLayout }

constructor TdxEntireRowSelectionLayout.Create(AStart,
  AEnd: TdxDocumentLayoutPosition; APage: TdxPage);
begin
  inherited Create(APage);
  Assert(AStart.IsValid(TdxDocumentLayoutDetailsLevel.Row));
  Assert(AEnd.IsValid(TdxDocumentLayoutDetailsLevel.Row));
  FStart := TdxDocumentLayoutPosition(AStart.Clone);
  FEnd := TdxDocumentLayoutPosition(AEnd.Clone);
end;

destructor TdxEntireRowSelectionLayout.Destroy;
begin
  FreeAndNil(FStart);
  FreeAndNil(FEnd);
  inherited Destroy;
end;

function TdxEntireRowSelectionLayout.Update: Boolean;
var
  AStartCharacterBounds, AEndCharacterBounds: TRect;
  ARowBoxes: TdxBoxCollection;
  ARowBounds: TRect;
begin
  ARowBoxes := Start.Row.Boxes;
  AStartCharacterBounds := ARowBoxes.First.Bounds;
  AEndCharacterBounds := ARowBoxes.Last.Bounds;
  ARowBounds := Start.Row.Bounds;
  Bounds := TRect.CreateSize(AStartCharacterBounds.Left, ARowBounds.Top,
    AEndCharacterBounds.Right - AStartCharacterBounds.Left, ARowBounds.Height);
  Result := True;
end;

{ TdxTableCellSelectionLayout }

constructor TdxTableCellSelectionLayout.Create(ACell: TdxTableCellViewInfo; APage: TdxPage);
begin
  inherited Create(APage);
  FCell := ACell;
end;

function TdxTableCellSelectionLayout.Update: Boolean;
begin
  Bounds := Cell.TableViewInfo.GetCellBounds(Cell);
  Result := True;
end;

{ TdxRectangularObjectSelectionLayout }

destructor TdxRectangularObjectSelectionLayout.Destroy;
begin
  FreeAndNil(FAnchor);
  FreeAndNil(FHitTestTransform);
  inherited Destroy;
end;

procedure TdxRectangularObjectSelectionLayout.Draw(const ASelectionPainter: IdxSelectionPainter);
begin
  ASelectionPainter.Draw(Self);
  if FAnchor <> nil then
    FAnchor.Draw(ASelectionPainter);
end;

function TdxRectangularObjectSelectionLayout.GetResizeable: Boolean;
begin
  Result := False;
end;

function TdxRectangularObjectSelectionLayout.HitTest(ALogPosition: TdxDocumentLogPosition;
  const ALogicalPoint: TPoint): Boolean;
begin
  raise TdxNotImplementedException.Create;
end;

function TdxRectangularObjectSelectionLayout.Update: Boolean;
begin
  FreeAndNil(FAnchor);
  Result := True;
end;

{ TdxResizeableRectangularObjectSelectionLayout }

constructor TdxResizeableRectangularObjectSelectionLayout.Create(AView: TObject{TdxRichEditView}; ABox: TdxBox; AStart: TdxDocumentLogPosition; APieceTable: TdxPieceTable);
begin
  inherited Create(AView, ABox, AStart, APieceTable);
  FHotZoneSize := AlignSizeToPixel(UnitConverter.DocumentsToLayoutUnits(DefaultHotZoneSizeDocuments));
end;

function TdxResizeableRectangularObjectSelectionLayout.GetResizeable: Boolean;
begin
  Result := True;
end;

function TdxResizeableRectangularObjectSelectionLayout.Update: Boolean;
var
  APictureBounds: TRect;
  ADelta, ALargeHotZoneSize: Integer;
  AGestureIndicator: IdxGestureStateIndicator;
  ARotationHotZone: TdxRectangularObjectRotationHotZone;
begin
  if not inherited Update then
    Exit(False);
  APictureBounds := Box.ActualSizeBounds;
  Anchor := TryCreateAnchorSelectionItem;
  if Anchor <> nil then
    Anchor.Update;

  ADelta := FHotZoneSize * 3 - APictureBounds.Width;
  if ADelta > 0 then
  begin
    APictureBounds.X := APictureBounds.X - ADelta div 2;
    APictureBounds.Width := APictureBounds.Width + ADelta;
  end;
  ADelta := FHotZoneSize * 3 - APictureBounds.Height;
  if ADelta > 0 then
  begin
    APictureBounds.Y := APictureBounds.Y - ADelta div 2;
    APictureBounds.Height := APictureBounds.Height + ADelta;
  end;

  ALargeHotZoneSize := AlignSizeToPixel(6 * FHotZoneSize div 5);

  Supports(View.Control.InnerControl, IdxGestureStateIndicator, AGestureIndicator);

  if Box is TdxFloatingObjectBox then
  begin
    ARotationHotZone := TdxRectangularObjectRotationHotZone.Create(Box, PieceTable, AGestureIndicator);
    ARotationHotZone.LineEnd := TPoint.Create(APictureBounds.Left + APictureBounds.Width div 2, APictureBounds.Top);
    AddHotZone(ARotationHotZone, RectangleFromCenter(APictureBounds.Left + APictureBounds.Width div 2, APictureBounds.Top - 2 * ALargeHotZoneSize, ALargeHotZoneSize));
  end;

  AddHotZone(TdxRectangularObjectTopLeftHotZone.Create(Box, PieceTable, AGestureIndicator),
    RectangleFromCenter(APictureBounds.Left, APictureBounds.Top, ALargeHotZoneSize));
  AddHotZone(TdxRectangularObjectTopRightHotZone.Create(Box, PieceTable, AGestureIndicator),
    RectangleFromCenter(APictureBounds.Right - 1, APictureBounds.Top, ALargeHotZoneSize));
  AddHotZone(TdxRectangularObjectBottomLeftHotZone.Create(Box, PieceTable, AGestureIndicator),
    RectangleFromCenter(APictureBounds.Left, APictureBounds.Bottom - 1, ALargeHotZoneSize));
  AddHotZone(TdxRectangularObjectBottomRightHotZone.Create(Box, PieceTable, AGestureIndicator),
    RectangleFromCenter(APictureBounds.Right - 1, APictureBounds.Bottom - 1, ALargeHotZoneSize));
  AddHotZone(TdxRectangularObjectTopMiddleHotZone.Create(Box, PieceTable, AGestureIndicator),
    RectangleFromCenter(APictureBounds.Left + APictureBounds.Width div 2, APictureBounds.Top, ALargeHotZoneSize));
  AddHotZone(TdxRectangularObjectBottomMiddleHotZone.Create(Box, PieceTable, AGestureIndicator),
    RectangleFromCenter(APictureBounds.Left + APictureBounds.Width div 2, APictureBounds.Bottom - 1, ALargeHotZoneSize));
  AddHotZone(TdxRectangularObjectMiddleLeftHotZone.Create(Box, PieceTable, AGestureIndicator),
    RectangleFromCenter(APictureBounds.Left, APictureBounds.Top + APictureBounds.Height div 2, ALargeHotZoneSize));
  AddHotZone(TdxRectangularObjectMiddleRightHotZone.Create(Box, PieceTable, AGestureIndicator),
    RectangleFromCenter(APictureBounds.Right - 1, APictureBounds.Top + APictureBounds.Height div 2, ALargeHotZoneSize));
  Result := True;
end;

procedure TdxResizeableRectangularObjectSelectionLayout.AddHotZone(AHotZone: TdxHotZone; const ABounds: TRect);
var
  ASelectionLayout: TdxDocumentSelectionLayout;
  AHotZones: TdxHotZoneCollection;
  AIndicator: IdxGestureStateIndicator;
begin
  ASelectionLayout := View.SelectionLayout.LastDocumentSelectionLayout;
  AHotZones := ASelectionLayout.HotZones;

  AHotZone.Bounds := ABounds;
  AHotZone.ExtendedBounds := CalculateExtendedBounds(ABounds);
  AHotZone.HitTestTransform := HitTestTransform;

  Supports(View.Control.InnerControl, IdxGestureStateIndicator, AIndicator);
  AHotZone.GestureStateIndicator := AIndicator;
  AHotZones.Add(AHotZone);
end;

function TdxResizeableRectangularObjectSelectionLayout.TryCreateAnchorSelectionItem: TdxFloatingObjectAnchorSelectionLayout;
var
  AFloatingObjectBox: TdxFloatingObjectBox;
  AAnchorPieceTable: TdxPieceTable;
  ALayoutPosition: TdxDocumentLayoutPosition;
  ARow: TdxRow;
  ABounds: TRect;
begin
  AFloatingObjectBox := Safe<TdxFloatingObjectBox>.Cast(Box);
  if AFloatingObjectBox = nil then
    Exit(nil);

  AAnchorPieceTable := TdxPieceTable(AFloatingObjectBox.PieceTable);
  ALayoutPosition := TdxDocumentLayoutPosition(View.DocumentLayout.CreateLayoutPosition(AAnchorPieceTable, LogStart, 0));
  try
    if not ALayoutPosition.Update(View.DocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Row) then
      Exit(nil);

    ARow := ALayoutPosition.Row;
    if ARow = nil then
      Exit(nil);

    Result := TdxFloatingObjectAnchorSelectionLayout.Create(View, Box, LogStart, AAnchorPieceTable);
    if ARow.Boxes.Count <= 0 then
      ABounds := ARow.Bounds
    else
      ABounds := ARow.Boxes.First.Bounds;
  finally
    FreeAndNil(ALayoutPosition);
  end;
  ABounds.Width := 30;
  ABounds.Height := 30;
  Result.AnchorBounds := ABounds;
end;

function TdxResizeableRectangularObjectSelectionLayout.CalculateExtendedBounds(const R: TRect): TRect;
begin
  Result.InitSize(R.X - DeltaRadiusForExtendedHotZone, R.Y - DeltaRadiusForExtendedHotZone,
    R.Width + 2 * DeltaRadiusForExtendedHotZone, R.Height + 2 * DeltaRadiusForExtendedHotZone);
end;

function TdxResizeableRectangularObjectSelectionLayout.RectangleFromCenter(X: Integer; Y: Integer; ALargeHotZoneSize: Integer): TRect;
var
  AHalfLargeHotZoneSize: Integer;
begin
  AHalfLargeHotZoneSize := ALargeHotZoneSize div 2;
  Result.InitSize(X - AHalfLargeHotZoneSize, Y - AHalfLargeHotZoneSize, ALargeHotZoneSize, ALargeHotZoneSize);
end;

function TdxResizeableRectangularObjectSelectionLayout.AlignSizeToPixel(ALayoutSize: Integer): Integer;
begin
  Result := UnitConverter.LayoutUnitsToPixels(ALayoutSize, TdxDocumentModel.Dpi);
  if (Result mod 2) = 0 then
    Inc(Result);
  Result := UnitConverter.PixelsToLayoutUnits(Result, TdxDocumentModel.Dpi);
end;

{ TdxFloatingObjectAnchorSelectionLayout }

procedure TdxFloatingObjectAnchorSelectionLayout.Draw(const ASelectionPainter: IdxSelectionPainter);
begin
  ASelectionPainter.Draw(Self);
end;

function TdxFloatingObjectAnchorSelectionLayout.HitTest(ALogPosition: TdxDocumentLogPosition;
  const ALogicalPoint: TPoint): Boolean;
begin
  Result := AnchorBounds.Contains(ALogicalPoint);
end;

function TdxFloatingObjectAnchorSelectionLayout.Update: Boolean;
begin
  Result := True;
end;

{ TdxSelectedCellsCollection }

constructor TdxSelectedCellsCollection.Create;
begin
  inherited Create(0);
  FInnerList := TdxSelectedCellsIntervalInRowList.Create;
end;

constructor TdxSelectedCellsCollection.Create(AOld: TdxStartSelectedCellInTable);
begin
  Create(AOld.FirstSelectedCell, AOld.OriginalStartLogPosition);
end;

constructor TdxSelectedCellsCollection.Create(AFirstSelectedCell: TdxTableCell; AOriginalStartLogPosition: TdxDocumentLogPosition);
begin
  inherited Create(AOriginalStartLogPosition);
  FInnerList := TdxSelectedCellsIntervalInRowList.Create;
  SetFirstSelectedCell(AFirstSelectedCell, AOriginalStartLogPosition);
end;

destructor TdxSelectedCellsCollection.Destroy;
begin
  FreeAndNil(FInnerList);
  inherited Destroy;
end;

function TdxSelectedCellsCollection.GetNormalizedFirst: TdxSelectedCellsIntervalInRow;
begin
  if RowsCount > 0 then
    Result :=  Self[GetNormalizedTopRowIndex]
  else
    Result := nil;
end;

function TdxSelectedCellsCollection.GetNormalizedLast: TdxSelectedCellsIntervalInRow;
begin
  Result := Self[GetNormalizedBottomRowIndex];
end;

function TdxSelectedCellsCollection.GetFirst: TdxSelectedCellsIntervalInRow;
begin
  if RowsCount > 0 then
    Result :=  Self[0]
  else
    Result := nil;
end;

function TdxSelectedCellsCollection.GetLast: TdxSelectedCellsIntervalInRow;
begin
  if RowsCount > 0 then
    Result := Self[RowsCount - 1]
  else
    Result := nil;
end;

function TdxSelectedCellsCollection.GetTopLeftCell: TdxTableCell;
begin
  if NormalizedFirst <> nil then
    Result := NormalizedFirst.NormalizedStartCell
  else
    Result := nil;
end;

function TdxSelectedCellsCollection.GetFirstSelectedCell: TdxTableCell;
begin
  if First <> nil then
    Result := First.StartCell
  else
    Result := nil;
end;

function TdxSelectedCellsCollection.GetSelectedOnlyOneCell: Boolean;
begin
  Result := (RowsCount = 1) and (First.NormalizedLength = 0);
end;

function TdxSelectedCellsCollection.GetIsNotEmpty: Boolean;
var
  I: Integer;
begin
  if RowsCount = 0 then
    Result := False
  else
  begin
    Result := True;
    for I := 0 to RowsCount - 1 do
      if (Self[I].StartCell = nil) or (Self[I].EndCell = nil) then
      begin
        Result := False;
        Break;
      end;
  end;
end;

function TdxSelectedCellsCollection.GetItem(
  Index: Integer): TdxSelectedCellsIntervalInRow;
begin
  Result := FInnerList[Index];
end;

function TdxSelectedCellsCollection.GetRowsCount: Integer;
begin
  Result := FInnerList.Count;
end;

function TdxSelectedCellsCollection.IsSquare: Boolean;
var
  AStartColumnIndexInFirstInterval, AEndColumnIndexInFirstInterval: Integer;
  ABottomRowIndex, AIndex: Integer;
  ASelectedRowsCountInFirstColumn, I: Integer;
  AStartColumnIndex, AEndColumnIndex: Integer;
  ACurrentInterval: TdxSelectedCellsIntervalInRow;
begin
  if not IsNotEmpty then
    Exit(False);
  AStartColumnIndexInFirstInterval := TopLeftCell.GetStartColumnIndexConsiderRowGrid;
  AEndColumnIndexInFirstInterval := NormalizedFirst.NormalizedEndCell.GetEndColumnIndexConsiderRowGrid;
  ABottomRowIndex := GetBottomRowIndex;
  for AIndex := GetTopRowIndex to ABottomRowIndex do
  begin
    ACurrentInterval := Self[AIndex];
    AStartColumnIndex := ACurrentInterval.NormalizedStartCell.GetStartColumnIndexConsiderRowGrid;
    AEndColumnIndex := ACurrentInterval.NormalizedEndCell.GetEndColumnIndexConsiderRowGrid;
    if (AStartColumnIndexInFirstInterval <> AStartColumnIndex) or (AEndColumnIndexInFirstInterval <> AEndColumnIndex) then
      Exit(False);
  end;
  ASelectedRowsCountInFirstColumn := GetSelectedRowsCount(AStartColumnIndexInFirstInterval);
  for I := AStartColumnIndexInFirstInterval + 1 to AEndColumnIndexInFirstInterval do
  begin
    if ASelectedRowsCountInFirstColumn <> GetSelectedRowsCount(I) then
      Exit(False);
  end;
  Result := True;
end;

function TdxSelectedCellsCollection.GetTopRowIndex: Integer;
begin
  Result := Min(FInnerList.IndexOf(First), FInnerList.IndexOf(Last));
end;

function TdxSelectedCellsCollection.GetNormalizedTopRowIndex: Integer;
begin
  if First.Row.IndexInTable < Last.Row.IndexInTable then
    Exit(0);
  Result := RowsCount - 1;
end;

function TdxSelectedCellsCollection.GetBottomRowIndex: Integer;
begin
  Result := Max(FInnerList.IndexOf(First), FInnerList.IndexOf(Last));
end;

function TdxSelectedCellsCollection.GetNormalizedBottomRowIndex: Integer;
begin
  if First.Row.IndexInTable < Last.Row.IndexInTable then
    Exit(RowsCount - 1);
  Result := 0;
end;

function TdxSelectedCellsCollection.GetSelectedColumnsCountInRow(ASelectionInterval: TdxSelectedCellsIntervalInRow): Integer;
var
  ACellId: Integer;
  ACells: TdxTableCellCollection;
begin
  Result := 0;
  ACells := ASelectionInterval.Row.Cells;
  for ACellId := ASelectionInterval.NormalizedStartCellIndex to ASelectionInterval.NormalizedEndCellIndex do
    Inc(Result, ACells[ACellId].ColumnSpan);
end;

function TdxSelectedCellsCollection.GetSelectedRowsCount(AColumnIndex: Integer): Integer;
var
  ABottomRowIndex, I: Integer;
  ACurrentCell: TdxTableCell;
begin
  Result := 0;
  ABottomRowIndex := GetBottomRowIndex;
  for I := GetTopRowIndex to ABottomRowIndex do
  begin
    ACurrentCell := TdxTableCellVerticalBorderCalculator.GetCellByColumnIndex(Self[I].Row, AColumnIndex);
    if ACurrentCell = nil then
      Continue;
    Inc(Result, GetSelectedRowsCountCore(ACurrentCell));
  end;
end;

function TdxSelectedCellsCollection.GetSelectedRowsCountCore(ACell: TdxTableCell): Integer;
begin
  case ACell.VerticalMerging of
    TdxMergingState.Restart:
      Result := GetMergedCellsCount(ACell);
    TdxMergingState.Continue:
      Result := 0;
  else
    Result := 1;
  end;
end;

function TdxSelectedCellsCollection.GetMergedCellsCount(ACurrentCell: TdxTableCell): Integer;
var
  AStartColumnIndex: Integer;
  ACells: TdxTableCellList;
begin
  AStartColumnIndex := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(ACurrentCell, False);
  ACells := TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(ACurrentCell, AStartColumnIndex, False);
  try
    Result := ACells.Count;
  finally
    ACells.Free;
  end;
end;

procedure TdxSelectedCellsCollection.AddSelectedCells(ARow: TdxTableRow; AStart: Integer; AEnd: Integer);
begin
  FInnerList.Add(TdxSelectedCellsIntervalInRow.Create(ARow, AStart, AEnd));
end;

procedure TdxSelectedCellsCollection.Clear;
begin
  FInnerList.Clear;
end;

procedure TdxSelectedCellsCollection.Add(AItem: TdxSelectedCellsIntervalInRow);
begin
  FInnerList.Add(AItem);
end;

procedure TdxSelectedCellsCollection.Remove(AItem: TdxSelectedCellsIntervalInRow);
begin
  FInnerList.Remove(AItem);
end;

procedure TdxSelectedCellsCollection.SetFirstSelectedCell(AStartCell: TdxTableCell; APos: TdxDocumentLogPosition);
var
  AStartCellIndex: Integer;
begin
  AStartCellIndex := AStartCell.IndexInRow;
  Add(TdxSelectedCellsIntervalInRow.Create(AStartCell.Row, AStartCellIndex, AStartCellIndex));
  OriginalStartLogPosition := APos;
end;

procedure TdxSelectedCellsCollection.SetItem(Index: Integer;
  const Value: TdxSelectedCellsIntervalInRow);
begin
  FInnerList[Index] := Value;
end;

function TdxSelectedCellsCollection.GetSelectedTableRows: TdxTableRowList;
var
  ABottomRowIndex, I: Integer;
begin
  Result := TdxTableRowList.Create;
  ABottomRowIndex := GetBottomRowIndex;
  for I := GetTopRowIndex to ABottomRowIndex do
    Result.Add(Self[I].Row);
end;

function TdxSelectedCellsCollection.IsSelectedEntireTable: Boolean;
begin
  Result := (RowsCount > 0) and
    (NormalizedFirst.NormalizedStartCell.IsFirstCellInTable) and (NormalizedLast.NormalizedEndCell.IsLastCellInTable);
end;

function TdxSelectedCellsCollection.IsSelectedEntireTableRows: Boolean;
var
  ABottomRowIndex, I: Integer;
  ACurrentInterval: TdxSelectedCellsIntervalInRow;
begin
  if RowsCount = 0 then
    Exit(False);
  ABottomRowIndex := GetBottomRowIndex;
  Result := True;
  for I := GetTopRowIndex to ABottomRowIndex do
  begin
    ACurrentInterval := FInnerList[I];
    if (not ACurrentInterval.NormalizedStartCell.IsFirstCellInRow) or (not ACurrentInterval.NormalizedEndCell.IsLastCellInRow) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TdxSelectedCellsCollection.IsSelectedEntireTableColumns: Boolean;
begin
  Result := (RowsCount > 0) and
    (RowsCount = FirstSelectedCell.Table.Rows.Count);
end;

function TdxSelectedCellsCollection.IsWholeCellSelected(ACell: TdxTableCell): Boolean;
var
  ASelectedCell: TdxTableCell;
  ADocumentModel: TdxDocumentModel;
  ASelection: TdxSelection;
  APieceTable: TdxSimplePieceTable;
  AParagraphs: TdxSimpleParagraphCollection;
  AStartParagraph, AEndParagraph: TdxSimpleParagraph;
  ACellContentLength, ACount, I: Integer;
  ASelectedCellsInRow: TdxSelectedCellsIntervalInRow;
  ARow: TdxTableRow;
begin
  if First.Table <> ACell.Table then
    Exit(False);
  if SelectedOnlyOneCell then
  begin
    ASelectedCell := First.NormalizedStartCell;
    if ASelectedCell <> ACell then
      Exit(False);
    ADocumentModel := TdxDocumentModel(ASelectedCell.DocumentModel);
    ASelection := ADocumentModel.Selection;
    APieceTable := ACell.PieceTable;
    if ASelection.PieceTable <> APieceTable then
      Exit(False);
    AParagraphs := APieceTable.Paragraphs;
    AStartParagraph := AParagraphs[ACell.StartParagraphIndex];
    AEndParagraph := AParagraphs[ACell.EndParagraphIndex];
    ACellContentLength := AEndParagraph.EndLogPosition - AStartParagraph.LogPosition + 1;
    Exit((ASelection.NormalizedStart = AStartParagraph.LogPosition) and (ASelection.Length = ACellContentLength));
  end;
  ARow := ACell.Row;
  ACount := RowsCount;
  for I := 0 to ACount - 1 do
  begin
    ASelectedCellsInRow := Self[I];
    if ASelectedCellsInRow.Row <> ARow then
      Continue;
    Exit(ASelectedCellsInRow.ContainsCell(ACell));
  end;
  Result := False;
end;

end.
