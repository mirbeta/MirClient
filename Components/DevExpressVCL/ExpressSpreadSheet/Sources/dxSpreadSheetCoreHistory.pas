{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetCoreHistory;

{$I cxVer.Inc}

interface

uses
  Types, Classes, Generics.Defaults, Generics.Collections, cxVariants, dxHashUtils,
  dxSpreadSheetCore, dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetCoreStyles, dxSpreadSheetCoreFormulas,
  dxSpreadSheetStyles, dxSpreadSheetConditionalFormatting, dxSpreadSheetPrinting;

type

  { TdxSpreadSheetHistoryCellCommand }

  TdxSpreadSheetHistoryCellCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private
    FColumn: Integer;
    FRow: Integer;
    FSheet: TdxSpreadSheetTableView;

    function GetCell: TdxSpreadSheetCell;
  public
    constructor Create(ASheet: TdxSpreadSheetTableView; ARow, AColumn: Integer); virtual;
    constructor CreateEx(ACell: TdxSpreadSheetCell); virtual;
    class function ActionClass: TdxSpreadSheetHistoryActionClass; override;
    function CompatibleWith(ACommand: TdxSpreadSheetHistoryCustomCommand): Boolean; override;
    //
    property Cell: TdxSpreadSheetCell read GetCell;
    property Column: Integer read FColumn;
    property Row: Integer read FRow;
    property Sheet: TdxSpreadSheetTableView read FSheet;
  end;

  { TdxSpreadSheetHistoryCreateCellCommand }

  TdxSpreadSheetHistoryCreateCellCommand = class(TdxSpreadSheetHistoryCellCommand)
  strict private
    FHasCell: Boolean;
    FHasColumn: Boolean;
    FHasRow: Boolean;
  protected
    procedure Initialize; override;
  public
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryChangeCellCommand }

  TdxSpreadSheetHistoryChangeCellCommand = class(TdxSpreadSheetHistoryCellCommand)
  protected
    FData: TMemoryStream;

    procedure Store(ACell: TdxSpreadSheetCell); virtual;
    procedure Restore; virtual;
  public
    constructor CreateEx(ACell: TdxSpreadSheetCell); override;
    destructor Destroy; override;
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryChangeCellStyleCommand }

  TdxSpreadSheetHistoryChangeCellStyleCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private
    FHandle: TdxSpreadSheetCellStyleHandle;

    function GetStyleHandle: TdxSpreadSheetCellStyleHandle;
    procedure SetStyleHandle(AValue: TdxSpreadSheetCellStyleHandle);
    procedure SetHandle(AValue: TdxSpreadSheetCellStyleHandle);
  protected
    FOwner: TObject;
    FPoint: TPoint;

    procedure Store(AStyleHandle: TdxSpreadSheetCellStyleHandle);
    procedure Restore;
    //
    property Handle: TdxSpreadSheetCellStyleHandle read FHandle write SetHandle;
    property StyleHandle: TdxSpreadSheetCellStyleHandle read GetStyleHandle write SetStyleHandle;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    class function ActionClass: TdxSpreadSheetHistoryActionClass; override;
    function CompatibleWith(ACommand: TdxSpreadSheetHistoryCustomCommand): Boolean; override;
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryDeleteCellCommand }

  TdxSpreadSheetHistoryDeleteCellCommand = class(TdxSpreadSheetHistoryChangeCellStyleCommand)
  public
    procedure Redo; override;
  end;

  { TdxSpreadSheetHistoryMoveCellCommand }

  TdxSpreadSheetHistoryMoveCellCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private
    FSourceArea: TRect;
    FTargetArea: TRect;
  public
    constructor Create(const ASourceArea, ATargetArea: TRect);
    class function ActionClass: TdxSpreadSheetHistoryActionClass; override;
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryMergeCellsCommand }

  TdxSpreadSheetHistoryMergeCellsCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private
    function GetMergedCells: TdxSpreadSheetMergedCellList;
  protected
    FArea: TRect;
    FCell: TdxSpreadSheetMergedCell;
    FPrevArea: TRect;

    function FindCell: TdxSpreadSheetMergedCell;
    procedure UndoRedo; virtual;
  public
    constructor Create(ACell: TdxSpreadSheetMergedCell; const AArea: TRect);
    class function ActionClass: TdxSpreadSheetHistoryActionClass; override;
    procedure Redo; override;
    procedure Undo; override;

    property MergedCells: TdxSpreadSheetMergedCellList read GetMergedCells;
  end;

  { TdxSpreadSheetHistoryChangeItemCommand }

  TdxSpreadSheetHistoryChangeItemCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private const
    FLAGS_SIZECUSTOM  = 1;
    FLAGS_SIZEDEFAULT = 2;
    FLAGS_VISIBLE = 4;
  strict private
    FItemIndex: Integer;
    FItems: TdxSpreadSheetTableItems;
    FStyleHandle: TdxSpreadSheetCellStyleHandle;

    function GetItem: TdxSpreadSheetTableItem;
    procedure SetStyleHandle(AValue: TdxSpreadSheetCellStyleHandle);
  protected
    FFlags: Byte;
    FSize: Integer;

    procedure Restore; virtual;
    procedure RestoreItemParams(ASize, AFlags: Integer; AHandle: TdxSpreadSheetCellStyleHandle);
    procedure Store; virtual;
  public
    constructor Create(AItem: TdxSpreadSheetTableItem);
    destructor Destroy; override;
    procedure Redo; override;
    procedure Undo; override;

    property Item: TdxSpreadSheetTableItem read GetItem;
    property StyleHandle: TdxSpreadSheetCellStyleHandle read FStyleHandle write SetStyleHandle;
  end;

  { TdxSpreadSheetHistoryDeleteItemCommand }

  TdxSpreadSheetHistoryDeleteItemCommand = class(TdxSpreadSheetHistoryChangeItemCommand)
  public
    procedure Restore; override;
  public
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryCustomGroupCommand }

  TdxSpreadSheetHistoryCustomGroupCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private
    FGroups: TdxSpreadSheetTableItemGroups;

    function GetRoot: TdxSpreadSheetTableItemGroup;
  protected
    FIndexes: TList<Integer>;

    procedure RedoUndo; virtual; abstract;
    procedure StoreIndexes(AIndexes: TList<Integer>; AGroup: TdxSpreadSheetTableItemGroup);
  public
    constructor Create(AGroup: TdxSpreadSheetTableItemGroup);
    destructor Destroy; override;
    procedure Redo; override;
    procedure Undo; override;
    //
    property Groups: TdxSpreadSheetTableItemGroups read FGroups;
    property Root: TdxSpreadSheetTableItemGroup read GetRoot;
  end;

  { TdxSpreadSheetHistoryChangeGroupRangeCommand }

  TdxSpreadSheetHistoryChangeGroupRangeCommand = class(TdxSpreadSheetHistoryCustomGroupCommand)
  strict private
    FFinishIndex: Integer;
    FStartIndex: Integer;

    function GetGroup: TdxSpreadSheetTableItemGroup;
  protected
    procedure RedoUndo; override;
  public
    constructor Create(AGroup: TdxSpreadSheetTableItemGroup);
    //
    property FinishIndex: Integer read FFinishIndex;
    property Group: TdxSpreadSheetTableItemGroup read GetGroup;
    property StartIndex: Integer read FStartIndex;
  end;

  { TdxSpreadSheetHistoryChangeGroupParentCommand }

  TdxSpreadSheetHistoryChangeGroupParentCommand = class(TdxSpreadSheetHistoryChangeGroupRangeCommand)
  strict private
    FPrevIndexes: TList<Integer>;

    function GetPrevParent: TdxSpreadSheetTableItemGroup;
  protected
    procedure RedoUndo; override;
  public
    constructor Create(AGroup: TdxSpreadSheetTableItemGroup);
    destructor Destroy; override;
    procedure Changed(AGroup: TdxSpreadSheetTableItemGroup);
    procedure Changing(AGroup: TdxSpreadSheetTableItemGroup);
  end;

  { TdxSpreadSheetHistoryChangeDefaultSizeCommand }

  TdxSpreadSheetHistoryChangeDefaultSizeCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private
    FSize: Integer;
    FItems: TdxSpreadSheetTableItems;
  public
    constructor Create(AItems: TdxSpreadSheetTableItems);
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryCustomContainerCommand }

  TdxSpreadSheetHistoryCustomContainerCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private
    function GetContainer: TdxSpreadSheetContainer;
  protected
    FContainerClass: TdxSpreadSheetContainerClass;
    FContainerIndex: Integer;

    procedure CreateContainer;
    procedure Store(AContainer: TdxSpreadSheetContainer); virtual;
  public
    constructor Create(AContainer: TdxSpreadSheetContainer);
    //
    property Container: TdxSpreadSheetContainer read GetContainer;
  end;

  { TdxSpreadSheetHistoryCreateContainerCommand }

  TdxSpreadSheetHistoryCreateContainerCommand = class(TdxSpreadSheetHistoryCustomContainerCommand)
  public
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryChangeContainerCommand }

  TdxSpreadSheetHistoryChangeContainerCommand = class(TdxSpreadSheetHistoryCustomContainerCommand)
  strict private
    FCanSaveContent: Boolean;
    FData: TMemoryStream;
  protected
    procedure Restore; virtual;
    procedure RestoreData(AReader: TcxReader); virtual;
    procedure Store(AContainer: TdxSpreadSheetContainer); override;
    procedure StoreData(AContainer: TdxSpreadSheetContainer; AWriter: TcxWriter); virtual;
  public
    constructor Create(AContainer: TdxSpreadSheetContainer; ACanSaveContent: Boolean = False);
    destructor Destroy; override;
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryChangeContainerIndexCommand }

  TdxSpreadSheetHistoryChangeContainerIndexCommand = class(TdxSpreadSheetHistoryCustomContainerCommand)
  strict private
    FOldIndex: Integer;
  public
    constructor Create(AContainer: TdxSpreadSheetContainer; ANewIndex: Integer);
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryDeleteContainerCommand }

  TdxSpreadSheetHistoryDeleteContainerCommand = class(TdxSpreadSheetHistoryChangeContainerCommand)
  protected
    procedure RestoreData(AReader: TcxReader); override;
    procedure StoreData(AContainer: TdxSpreadSheetContainer; AWriter: TcxWriter); override;
  public
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryFormulaChangedCommand }

  TdxSpreadSheetHistoryFormulaChangedCommand = class(TdxSpreadSheetHistoryCustomCommand)
  private
    FColumnIndex: Integer;
    FName: TdxSpreadSheetDefinedName;
    FRowIndex: Integer;
    FSheet: TdxSpreadSheetTableView;
    FSourceText: string;
  public
    constructor Create(AFormula: TdxSpreadSheetCustomFormula; const ASourceText: string);
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryConditionalFormattingRuleCustomCommand }

  TdxSpreadSheetHistoryConditionalFormattingRuleCustomCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private
    function GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
    function GetRule: TdxSpreadSheetCustomConditionalFormattingRule;
  protected
    FData: TMemoryStream;
    FRuleIndex: Integer;

    procedure Restore; virtual;
    procedure Store(ARule: TdxSpreadSheetCustomConditionalFormattingRule); virtual;
  public
    constructor Create(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
    destructor Destroy; override;
    class function ActionClass: TdxSpreadSheetHistoryActionClass; override;

    property ConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting read GetConditionalFormatting;
    property Rule: TdxSpreadSheetCustomConditionalFormattingRule read GetRule;
  end;

  { TdxSpreadSheetHistoryCreateConditionalFormattingRuleCommand }

  TdxSpreadSheetHistoryCreateConditionalFormattingRuleCommand = class(TdxSpreadSheetHistoryConditionalFormattingRuleCustomCommand)
  protected
    FRuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass;
  public
    constructor Create(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryChangeConditionalFormattingRuleCommand }

  TdxSpreadSheetHistoryChangeConditionalFormattingRuleCommand = class(TdxSpreadSheetHistoryConditionalFormattingRuleCustomCommand)
  protected
    procedure Initialize; override;
    procedure Restore; override;
  public
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryChangeConditionalFormattingRuleIndexCommand }

  TdxSpreadSheetHistoryChangeConditionalFormattingRuleIndexCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private
    FNewIndex: Integer;
    FOldIndex: Integer;

    function GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
  public
    constructor Create(AOldIndex, ANewIndex: Integer);
    class function ActionClass: TdxSpreadSheetHistoryActionClass; override;
    procedure Redo; override;
    procedure Undo; override;
    //
    property ConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting read GetConditionalFormatting;
  end;

  { TdxSpreadSheetHistoryDeleteConditionalFormattingRuleCommand }

  TdxSpreadSheetHistoryDeleteConditionalFormattingRuleCommand = class(TdxSpreadSheetHistoryCreateConditionalFormattingRuleCommand)
  protected
    procedure Initialize; override;
  public
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryChangePrintingOptionsCommand }

  TdxSpreadSheetHistoryChangePrintingOptionsCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private
    FSource: TdxSpreadSheetTableViewOptionsPrint;

    function GetView: TdxSpreadSheetTableView;
  protected
    procedure Initialize; override;
  public
    destructor Destroy; override;
    class function ActionClass: TdxSpreadSheetHistoryActionClass; override;
    procedure Redo; override;
    procedure Undo; override;
    //
    property View: TdxSpreadSheetTableView read GetView;
  end;

  { TdxSpreadSheetHistoryCreateDefinedNameCommand }

  TdxSpreadSheetHistoryCreateDefinedNameCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private
    FCaption: string;
    FReference: string;
    FScope: TdxSpreadSheetCustomView;
  public
    constructor Create(ADefinedName: TdxSpreadSheetDefinedName);
    class function ActionClass: TdxSpreadSheetHistoryActionClass; override;
    procedure Redo; override;
    procedure Undo; override;
  end;

implementation

uses
  Math, SysUtils, cxGeometry, dxCore, dxSpreadSheetCoreHelpers, dxSpreadSheetFormatBinary;

type
  TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);
  TdxSpreadSheetCustomConditionalFormattingAccess = class(TdxSpreadSheetCustomConditionalFormatting);
  TdxSpreadSheetDefinedNamesAccess = class(TdxSpreadSheetDefinedNames);
  TdxSpreadSheetMergedCellAccess = class(TdxSpreadSheetMergedCell);
  TdxSpreadSheetMergedCellListAccess = class(TdxSpreadSheetMergedCellList);
  TdxSpreadSheetTableItemAccess = class(TdxSpreadSheetTableItem);
  TdxSpreadSheetTableItemGroupAccess = class(TdxSpreadSheetTableItemGroup);
  TdxSpreadSheetTableItemGroupsAccess = class(TdxSpreadSheetTableItemGroups);

{ TdxSpreadSheetHistoryCellCommand }

constructor TdxSpreadSheetHistoryCellCommand.Create(ASheet: TdxSpreadSheetTableView; ARow, AColumn: Integer);
begin
  FRow := ARow;
  FColumn := AColumn;
  FSheet := ASheet;
end;

constructor TdxSpreadSheetHistoryCellCommand.CreateEx(ACell: TdxSpreadSheetCell);
begin
  Create(ACell.View, ACell.RowIndex, ACell.ColumnIndex);
end;

class function TdxSpreadSheetHistoryCellCommand.ActionClass: TdxSpreadSheetHistoryActionClass;
begin
  Result := TdxSpreadSheetHistoryEditCellAction;
end;

function TdxSpreadSheetHistoryCellCommand.CompatibleWith(ACommand: TdxSpreadSheetHistoryCustomCommand): Boolean;
begin
  Result :=
    (ACommand is TdxSpreadSheetHistoryCellCommand) and
      (TdxSpreadSheetHistoryCellCommand(ACommand).Sheet = Sheet) and
      (TdxSpreadSheetHistoryCellCommand(ACommand).Row = Row) and
      (TdxSpreadSheetHistoryCellCommand(ACommand).Column = Column) or

    (ACommand is TdxSpreadSheetHistoryChangeCellStyleCommand) and
      (TdxSpreadSheetHistoryChangeCellStyleCommand(ACommand).FOwner = Sheet) and
      (TdxSpreadSheetHistoryChangeCellStyleCommand(ACommand).FPoint.X = Column) and
      (TdxSpreadSheetHistoryChangeCellStyleCommand(ACommand).FPoint.Y = Row);
end;

function TdxSpreadSheetHistoryCellCommand.GetCell: TdxSpreadSheetCell;
begin
  Result := FSheet.CreateCell(Row, Column);
end;

{ TdxSpreadSheetHistoryCreateCellCommand }

procedure TdxSpreadSheetHistoryCreateCellCommand.Redo;
begin
  if not FHasCell then
    Sheet.CreateCell(Row, Column);
end;

procedure TdxSpreadSheetHistoryCreateCellCommand.Undo;
begin
  if not (FHasCell or View.Containers.IsCellUsed(Cell)) then
  begin
    Sheet.Cells[Row, Column].Free;
    if not FHasRow then
      Sheet.Rows[Row].Free;
    if not FHasColumn then
      Sheet.Columns[Column].Free;
  end;
end;

procedure TdxSpreadSheetHistoryCreateCellCommand.Initialize;
begin
  FHasCell := Sheet.Cells[Row, Column] <> nil;
  if not FHasCell then
  begin
    FHasColumn := Sheet.Columns[Column] <> nil;
    FHasRow := Sheet.Rows[Row] <> nil;
  end;
end;

{ TdxSpreadSheetHistoryChangeCellCommand }

constructor TdxSpreadSheetHistoryChangeCellCommand.CreateEx(ACell: TdxSpreadSheetCell);
begin
  inherited CreateEx(ACell);
  Store(ACell);
end;

destructor TdxSpreadSheetHistoryChangeCellCommand.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

procedure TdxSpreadSheetHistoryChangeCellCommand.Store(ACell: TdxSpreadSheetCell);
var
  AWriter: TcxWriter;
begin
  FData := TMemoryStream.Create;
  AWriter := TcxWriter.Create(FData, dxSpreadSheetBinaryFormatVersion);
  try
    TdxSpreadSheetCellAccess(ACell).SaveToStream(AWriter);
  finally
    AWriter.Free;
  end;
end;

procedure TdxSpreadSheetHistoryChangeCellCommand.Restore;
var
  ACell: TdxSpreadSheetCell;
  AFormulaRef: TdxSpreadSheetFormulaAsTextInfoList;
  AReader: TcxReader;
begin
  FData.Position := 0;
  AReader := TcxReader.Create(FData, dxSpreadSheetBinaryFormatVersion);
  try
    ACell := Cell;
    Store(ACell);
    AFormulaRef := TdxSpreadSheetFormulaAsTextInfoList.Create(Sheet.SpreadSheet);
    try
      TdxSpreadSheetCellAccess(ACell).LoadFromStream(AReader, AFormulaRef);
      if ACell.DataType = cdtFormula then
        AFormulaRef.ResolveReferences;
    finally
      AFormulaRef.Free;
    end;
  finally
    AReader.Stream.Free;
    AReader.Free;
  end;
end;

procedure TdxSpreadSheetHistoryChangeCellCommand.Redo;
begin
  Restore;
end;

procedure TdxSpreadSheetHistoryChangeCellCommand.Undo;
begin
  Restore;
end;

{ TdxSpreadSheetHistoryChangeCellStyleCommand }

constructor TdxSpreadSheetHistoryChangeCellStyleCommand.Create(AOwner: TObject);
begin
  inherited Create;
  FPoint := cxInvalidPoint;

  if AOwner is TdxSpreadSheetCell then
  begin
    FOwner := TdxSpreadSheetCell(AOwner).View;
    FPoint.X := TdxSpreadSheetCell(AOwner).ColumnIndex;
    FPoint.Y := TdxSpreadSheetCell(AOwner).RowIndex;
    Store(TdxSpreadSheetCell(AOwner).StyleHandle);
  end
  else

  if AOwner is TdxSpreadSheetTableItem then
  begin
    FOwner := TdxSpreadSheetTableItem(AOwner).Owner;
    FPoint.X := TdxSpreadSheetTableItem(AOwner).Index;
    Store(TdxSpreadSheetTableItem(AOwner).Style.Handle);
  end;
end;

destructor TdxSpreadSheetHistoryChangeCellStyleCommand.Destroy;
begin
  Handle := nil;
  inherited Destroy;
end;

class function TdxSpreadSheetHistoryChangeCellStyleCommand.ActionClass: TdxSpreadSheetHistoryActionClass;
begin
  Result := TdxSpreadSheetHistoryEditCellAction;
end;

function TdxSpreadSheetHistoryChangeCellStyleCommand.CompatibleWith(ACommand: TdxSpreadSheetHistoryCustomCommand): Boolean;
begin
  Result :=
    (ACommand is TdxSpreadSheetHistoryCellCommand) and TdxSpreadSheetHistoryCellCommand(ACommand).CompatibleWith(Self) or
    (ACommand is TdxSpreadSheetHistoryChangeCellStyleCommand) and
      (TdxSpreadSheetHistoryChangeCellStyleCommand(ACommand).FOwner = FOwner) and
      (cxPointIsEqual(TdxSpreadSheetHistoryChangeCellStyleCommand(ACommand).FPoint, FPoint));
end;

procedure TdxSpreadSheetHistoryChangeCellStyleCommand.Redo;
begin
  Restore;
end;

procedure TdxSpreadSheetHistoryChangeCellStyleCommand.Undo;
begin
  Restore;
end;

procedure TdxSpreadSheetHistoryChangeCellStyleCommand.Store(AStyleHandle: TdxSpreadSheetCellStyleHandle);
begin
  Handle := AStyleHandle;
end;

procedure TdxSpreadSheetHistoryChangeCellStyleCommand.Restore;
var
  AHandle: TdxHashTableItem;
  AStyleHandle: TdxSpreadSheetCellStyleHandle;
begin
  AHandle := nil;
  dxChangeHandle(AHandle, Handle);
  try
    AStyleHandle := StyleHandle;
    Store(AStyleHandle);
    StyleHandle := TdxSpreadSheetCellStyleHandle(AHandle);
  finally
    dxChangeHandle(AHandle, nil);
  end;
end;

function TdxSpreadSheetHistoryChangeCellStyleCommand.GetStyleHandle: TdxSpreadSheetCellStyleHandle;
begin
  if FPoint.Y >= 0 then
    Result := TdxSpreadSheetTableView(FOwner).CreateCell(FPoint.Y, FPoint.X).StyleHandle
  else
    Result := TdxSpreadSheetTableItems(FOwner).CreateItem(FPoint.X).Style.Handle
end;

procedure TdxSpreadSheetHistoryChangeCellStyleCommand.SetHandle(AValue: TdxSpreadSheetCellStyleHandle);
begin
  dxChangeHandle(TdxHashTableItem(FHandle), AValue);
end;

procedure TdxSpreadSheetHistoryChangeCellStyleCommand.SetStyleHandle(AValue: TdxSpreadSheetCellStyleHandle);
begin
  if FPoint.Y >= 0 then
    TdxSpreadSheetTableView(FOwner).CreateCell(FPoint.Y, FPoint.X).StyleHandle := AValue
  else
    TdxSpreadSheetTableItems(FOwner).CreateItem(FPoint.X).Style.Handle := AValue
end;

{ TdxSpreadSheetHistoryDeleteCellCommand }

procedure TdxSpreadSheetHistoryDeleteCellCommand.Redo;
begin
  inherited Redo;
  TdxSpreadSheetTableView(FOwner).Cells[FPoint.Y, FPoint.X].Free;
end;

{ TdxSpreadSheetHistoryMoveCellCommand }

constructor TdxSpreadSheetHistoryMoveCellCommand.Create(const ASourceArea, ATargetArea: TRect);
begin
  inherited Create;
  FSourceArea := ASourceArea;
  FTargetArea := ATargetArea;
end;

class function TdxSpreadSheetHistoryMoveCellCommand.ActionClass: TdxSpreadSheetHistoryActionClass;
begin
  Result := TdxSpreadSheetHistoryMoveCellsAction;
end;

procedure TdxSpreadSheetHistoryMoveCellCommand.Redo;
begin
  Undo;
end;

procedure TdxSpreadSheetHistoryMoveCellCommand.Undo;
var
  AHelper: TdxSpreadSheetTableViewMoveCellsModificationHelper;
  ATempRect: TRect;
begin
  AHelper := TdxSpreadSheetTableViewMoveCellsModificationHelper.Create(View as TdxSpreadSheetTableView);
  try
    AHelper.Process(FTargetArea, FSourceArea.TopLeft);
    ATempRect := FTargetArea;
    FTargetArea := FSourceArea;
    FSourceArea := ATempRect;
  finally
    AHelper.Free;
  end;
end;

{ TdxSpreadSheetHistoryMergeCellsCommand }

constructor TdxSpreadSheetHistoryMergeCellsCommand.Create(ACell: TdxSpreadSheetMergedCell; const AArea: TRect);
begin
  if ACell <> nil then
  begin
    FPrevArea := ACell.Area;
    FArea := AArea;
  end
  else
    FPrevArea := AArea;
end;

class function TdxSpreadSheetHistoryMergeCellsCommand.ActionClass: TdxSpreadSheetHistoryActionClass;
begin
  Result := TdxSpreadSheetHistoryMergeCellsAction;
end;

procedure TdxSpreadSheetHistoryMergeCellsCommand.Redo;
begin
  UndoRedo;
end;

procedure TdxSpreadSheetHistoryMergeCellsCommand.Undo;
begin
  UndoRedo;
end;

function TdxSpreadSheetHistoryMergeCellsCommand.GetMergedCells: TdxSpreadSheetMergedCellList;
begin
   Result := TdxSpreadSheetTableView(View).MergedCells;
end;

function TdxSpreadSheetHistoryMergeCellsCommand.FindCell: TdxSpreadSheetMergedCell;
var
  ACells: TdxSpreadSheetMergedCellListAccess;
begin
  ACells := TdxSpreadSheetMergedCellListAccess(TdxSpreadSheetTableView(View).MergedCells);
  Result := FCell;
  if ACells.Find(FCell) >= 0 then
    Exit;
  Result := ACells.First;
  while (Result <> nil) and not cxRectIsEqual(Result.Area, FArea) do
    Result := Result.Next;
end;

procedure TdxSpreadSheetHistoryMergeCellsCommand.UndoRedo;
var
  ACell: TdxSpreadSheetMergedCellAccess;
  R: TRect;
begin
  ACell := TdxSpreadSheetMergedCellAccess(FindCell);
  if cxRectIsNull(FPrevArea) then
  begin
    ACell.Free;
    FCell := nil;
  end
  else
    if ACell <> nil then
      ACell.Initialize(ACell.Owner, FPrevArea)
    else
    begin
      MergedCells.Add(FPrevArea);
      FCell := MergedCells.Last;
    end;

  R := FArea;
  FArea := FPrevArea;
  FPrevArea := R;
end;

{ TdxSpreadSheetHistoryChangeItemCommand }

constructor TdxSpreadSheetHistoryChangeItemCommand.Create(AItem: TdxSpreadSheetTableItem);
begin
  inherited Create;
  FItemIndex := AItem.Index;
  FItems := AItem.Owner;
  Store;
end;

destructor TdxSpreadSheetHistoryChangeItemCommand.Destroy;
begin
  StyleHandle := nil;
  inherited Destroy;
end;

procedure TdxSpreadSheetHistoryChangeItemCommand.Redo;
begin
  Restore;
end;

procedure TdxSpreadSheetHistoryChangeItemCommand.Undo;
begin
  Restore;
end;

procedure TdxSpreadSheetHistoryChangeItemCommand.Restore;
var
  AFlags: Integer;
  AHandle: TdxHashTableItem;
  ASize: Integer;
begin
  AHandle := nil;
  dxChangeHandle(AHandle, StyleHandle);
  try
    AFlags := FFlags;
    ASize := FSize;
    Store;
    RestoreItemParams(ASize, AFlags, TdxSpreadSheetCellStyleHandle(AHandle));
  finally
    dxChangeHandle(AHandle, nil);
  end;
end;

procedure TdxSpreadSheetHistoryChangeItemCommand.RestoreItemParams(
  ASize, AFlags: Integer; AHandle: TdxSpreadSheetCellStyleHandle);
begin
  TdxSpreadSheetTableItemAccess(Item).BeforeResize;
  try
    TdxSpreadSheetTableItemAccess(Item).Size := ASize;
    TdxSpreadSheetTableItemAccess(Item).DefaultSize := AFlags and FLAGS_SIZEDEFAULT <> 0;
    TdxSpreadSheetTableItemAccess(Item).IsCustomSize := AFlags and FLAGS_SIZECUSTOM <> 0;
    TdxSpreadSheetTableItemAccess(Item).Visible := AFlags and FLAGS_VISIBLE <> 0;
    TdxSpreadSheetTableItemAccess(Item).Style.Handle := AHandle;
  finally
    TdxSpreadSheetTableItemAccess(Item).AfterResize;
  end;
end;

procedure TdxSpreadSheetHistoryChangeItemCommand.Store;
begin
  StyleHandle := Item.Style.Handle;
  FFlags :=
    IfThen(TdxSpreadSheetTableItemAccess(Item).DefaultSize, FLAGS_SIZEDEFAULT) or
    IfThen(TdxSpreadSheetTableItemAccess(Item).IsCustomSize, FLAGS_SIZECUSTOM) or
    IfThen(TdxSpreadSheetTableItemAccess(Item).Visible, FLAGS_VISIBLE);
  FSize := Item.CustomSize;
end;

function TdxSpreadSheetHistoryChangeItemCommand.GetItem: TdxSpreadSheetTableItem;
begin
  Result := FItems.CreateItem(FItemIndex);
end;

procedure TdxSpreadSheetHistoryChangeItemCommand.SetStyleHandle(AValue: TdxSpreadSheetCellStyleHandle);
begin
  dxChangeHandle(TdxHashTableItem(FStyleHandle), AValue);
end;

{ TdxSpreadSheetHistoryDeleteItemCommand }

procedure TdxSpreadSheetHistoryDeleteItemCommand.Redo;
begin
  Item.Free;
end;

procedure TdxSpreadSheetHistoryDeleteItemCommand.Undo;
begin
  Restore;
end;

procedure TdxSpreadSheetHistoryDeleteItemCommand.Restore;
begin
  RestoreItemParams(FSize, FFlags, StyleHandle);
end;

{ TdxSpreadSheetHistoryCustomGroupCommand }

constructor TdxSpreadSheetHistoryCustomGroupCommand.Create(AGroup: TdxSpreadSheetTableItemGroup);
begin
  inherited Create;
  FGroups := AGroup.Owner.Groups;
  FIndexes := TList<Integer>.Create;
  StoreIndexes(FIndexes, AGroup);
end;

destructor TdxSpreadSheetHistoryCustomGroupCommand.Destroy;
begin
  FreeAndNil(FIndexes);
  inherited Destroy;
end;

procedure TdxSpreadSheetHistoryCustomGroupCommand.Redo;
begin
  RedoUndo;
end;

procedure TdxSpreadSheetHistoryCustomGroupCommand.Undo;
begin
  RedoUndo;
end;

procedure TdxSpreadSheetHistoryCustomGroupCommand.StoreIndexes(AIndexes: TList<Integer>; AGroup: TdxSpreadSheetTableItemGroup);
var
  AIndex: Integer;
begin
  AIndexes.Capacity := Max(AGroup.Level, 0);
  repeat
    AIndex := AGroup.Index;
    if AIndex < 0 then
      Break;
    AIndexes.Insert(0, AIndex);
    AGroup := AGroup.Parent;
  until False;
end;

function TdxSpreadSheetHistoryCustomGroupCommand.GetRoot: TdxSpreadSheetTableItemGroup;
begin
  Result := TdxSpreadSheetTableItemGroupsAccess(Groups).Root;
end;

{ TdxSpreadSheetHistoryChangeGroupRangeCommand }

constructor TdxSpreadSheetHistoryChangeGroupRangeCommand.Create(AGroup: TdxSpreadSheetTableItemGroup);
begin
  inherited Create(AGroup);
  FFinishIndex := AGroup.FinishIndex;
  FStartIndex := AGroup.StartIndex;
end;

procedure TdxSpreadSheetHistoryChangeGroupRangeCommand.RedoUndo;

  function Exchange(var AStoreValue: Integer; const ACurrentValue: Integer): Integer;
  begin
    Result := AStoreValue;
    AStoreValue := ACurrentValue;
  end;

var
  AGroup: TdxSpreadSheetTableItemGroup;
begin
  AGroup := Group;
  AGroup.BeginUpdate;
  try
    AGroup.StartIndex := Exchange(FStartIndex, AGroup.StartIndex);
    AGroup.FinishIndex := Exchange(FFinishIndex, AGroup.FinishIndex);
  finally
    AGroup.EndUpdate;
  end;
end;

function TdxSpreadSheetHistoryChangeGroupRangeCommand.GetGroup: TdxSpreadSheetTableItemGroup;
var
  AGroup: TdxSpreadSheetTableItemGroup;
  I: Integer;
begin
  AGroup := Root;
  for I := 0 to FIndexes.Count - 1 do
    AGroup := AGroup.Items[FIndexes[I]];
  Result := AGroup as TdxSpreadSheetTableItemGroup;
end;

{ TdxSpreadSheetHistoryChangeGroupParentCommand }

constructor TdxSpreadSheetHistoryChangeGroupParentCommand.Create(AGroup: TdxSpreadSheetTableItemGroup);
begin
  inherited Create(AGroup);
  FPrevIndexes := TList<Integer>.Create;
end;

destructor TdxSpreadSheetHistoryChangeGroupParentCommand.Destroy;
begin
  FreeAndNil(FPrevIndexes);
  inherited Destroy;
end;

procedure TdxSpreadSheetHistoryChangeGroupParentCommand.RedoUndo;
begin
  if (FPrevIndexes.Count > 0) and (FIndexes.Count > 0) then
    TdxSpreadSheetTableItemGroupAccess(Group).SetParent(GetPrevParent)
  else
    if FIndexes.Count = 0 then
      TdxSpreadSheetTableItemGroup.Create(Root.Owner, GetPrevParent, StartIndex, FinishIndex)
    else
      Group.Free;

  ExchangePointers(FPrevIndexes, FIndexes);
end;

procedure TdxSpreadSheetHistoryChangeGroupParentCommand.Changing(AGroup: TdxSpreadSheetTableItemGroup);
begin
  // do nothing
end;

procedure TdxSpreadSheetHistoryChangeGroupParentCommand.Changed(AGroup: TdxSpreadSheetTableItemGroup);
begin
  StoreIndexes(FPrevIndexes, AGroup);
  ExchangePointers(FPrevIndexes, FIndexes);
end;

function TdxSpreadSheetHistoryChangeGroupParentCommand.GetPrevParent: TdxSpreadSheetTableItemGroup;
var
  I: Integer;
begin
  Result := Root;
  for I := 0 to FPrevIndexes.Count - 2 do
    Result := Result.Items[FPrevIndexes[I]];
end;

{ TdxSpreadSheetHistoryChangeDefaultSizeCommand }

constructor TdxSpreadSheetHistoryChangeDefaultSizeCommand.Create(AItems: TdxSpreadSheetTableItems);
begin
  FItems := AItems;
  FSize := FItems.DefaultSize;
end;

procedure TdxSpreadSheetHistoryChangeDefaultSizeCommand.Redo;
begin
  Undo;
end;

procedure TdxSpreadSheetHistoryChangeDefaultSizeCommand.Undo;
var
  ASize: Integer;
begin
  ASize := FItems.DefaultSize;
  FItems.DefaultSize := FSize;
  FSize := ASize;
end;

{ TdxSpreadSheetHistoryCustomContainerCommand }

constructor TdxSpreadSheetHistoryCustomContainerCommand.Create(AContainer: TdxSpreadSheetContainer);
begin
  inherited Create;
  Store(AContainer);
end;

procedure TdxSpreadSheetHistoryCustomContainerCommand.CreateContainer;
begin
  View.Containers.Add(FContainerClass).Index := FContainerIndex;
end;

procedure TdxSpreadSheetHistoryCustomContainerCommand.Store(AContainer: TdxSpreadSheetContainer);
begin
  FContainerIndex := AContainer.Index;
  FContainerClass := TdxSpreadSheetContainerClass(AContainer.ClassType);
end;

function TdxSpreadSheetHistoryCustomContainerCommand.GetContainer: TdxSpreadSheetContainer;
begin
  Result := View.Containers[FContainerIndex];
end;

{ TdxSpreadSheetHistoryCreateContainerCommand }

procedure TdxSpreadSheetHistoryCreateContainerCommand.Redo;
begin
  CreateContainer;
end;

procedure TdxSpreadSheetHistoryCreateContainerCommand.Undo;
begin
  Container.Free;
end;

{ TdxSpreadSheetHistoryChangeContainerCommand }

constructor TdxSpreadSheetHistoryChangeContainerCommand.Create(
  AContainer: TdxSpreadSheetContainer; ACanSaveContent: Boolean = False);
begin
  FCanSaveContent := ACanSaveContent;
  inherited Create(AContainer);
end;

destructor TdxSpreadSheetHistoryChangeContainerCommand.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

procedure TdxSpreadSheetHistoryChangeContainerCommand.Redo;
begin
  Restore;
end;

procedure TdxSpreadSheetHistoryChangeContainerCommand.Undo;
begin
  Restore;
end;

procedure TdxSpreadSheetHistoryChangeContainerCommand.Restore;
var
  AReader: TcxReader;
begin
  FData.Position := 0;
  AReader := TcxReader.Create(FData, dxSpreadSheetBinaryFormatVersion);
  try
    RestoreData(AReader);
  finally
    if FData <> AReader.Stream then
      AReader.Stream.Free;
    AReader.Free;
  end;
end;

procedure TdxSpreadSheetHistoryChangeContainerCommand.RestoreData(AReader: TcxReader);
begin
  Store(Container);
  TdxSpreadSheetContainerAccess(Container).LoadFromStream(AReader, FCanSaveContent);
end;

procedure TdxSpreadSheetHistoryChangeContainerCommand.Store(AContainer: TdxSpreadSheetContainer);
var
  AWriter: TcxWriter;
begin
  inherited Store(AContainer);

  FData := TMemoryStream.Create;
  AWriter := TcxWriter.Create(FData, dxSpreadSheetBinaryFormatVersion);
  try
    StoreData(AContainer, AWriter);
  finally
    AWriter.Free;
  end;
end;

procedure TdxSpreadSheetHistoryChangeContainerCommand.StoreData(AContainer: TdxSpreadSheetContainer; AWriter: TcxWriter);
begin
  TdxSpreadSheetContainerAccess(AContainer).SaveToStream(AWriter, FCanSaveContent);
end;

{ TdxSpreadSheetHistoryChangeContainerIndexCommand }

constructor TdxSpreadSheetHistoryChangeContainerIndexCommand.Create(
  AContainer: TdxSpreadSheetContainer; ANewIndex: Integer);
begin
  inherited Create(AContainer);
  FOldIndex := FContainerIndex;
  FContainerIndex := ANewIndex;
end;

procedure TdxSpreadSheetHistoryChangeContainerIndexCommand.Redo;
begin
  Undo;
end;

procedure TdxSpreadSheetHistoryChangeContainerIndexCommand.Undo;
begin
  Container.Index := FOldIndex;
  ExchangeLongWords(FContainerIndex, FOldIndex);
end;

{ TdxSpreadSheetHistoryDeleteContainerCommand }

procedure TdxSpreadSheetHistoryDeleteContainerCommand.Redo;
begin
  Container.Free;
end;

procedure TdxSpreadSheetHistoryDeleteContainerCommand.Undo;
begin
  Restore;
end;

procedure TdxSpreadSheetHistoryDeleteContainerCommand.RestoreData(AReader: TcxReader);
begin
  CreateContainer;
  TdxSpreadSheetContainerAccess(Container).LoadFromStream(AReader);
end;

procedure TdxSpreadSheetHistoryDeleteContainerCommand.StoreData(
  AContainer: TdxSpreadSheetContainer; AWriter: TcxWriter);
begin
  TdxSpreadSheetContainerAccess(AContainer).SaveToStream(AWriter);
end;

{ TdxSpreadSheetHistoryFormulaChangedCommand }

constructor TdxSpreadSheetHistoryFormulaChangedCommand.Create(AFormula: TdxSpreadSheetCustomFormula; const ASourceText: string);
begin
  FColumnIndex := AFormula.AnchorColumn;
  FRowIndex := AFormula.AnchorRow;
  FSheet := AFormula.View as TdxSpreadSheetTableView;
  if AFormula is TdxSpreadSheetDefinedNameFormula then
    FName := TdxSpreadSheetDefinedNameFormula(AFormula).Owner;
  FSourceText := ASourceText;
end;

procedure TdxSpreadSheetHistoryFormulaChangedCommand.Redo;
begin
  // todo: when insert all reference will be valid
end;

procedure TdxSpreadSheetHistoryFormulaChangedCommand.Undo;
begin
  if (FName = nil) and (FSheet <> nil) then
    FSheet.CreateCell(FRowIndex, FColumnIndex).SetText(FSourceText, True)
  else
    if TdxSpreadSheetDefinedNamesAccess(View.SpreadSheet.DefinedNames).ItemList.IndexOf(FName) >= 0 then
      FName.Reference := FSourceText;
end;

{ TdxSpreadSheetHistoryConditionalFormattingRuleCustomCommand }

constructor TdxSpreadSheetHistoryConditionalFormattingRuleCustomCommand.Create(
  ARule: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  inherited Create;
  FRuleIndex := ARule.Index;
end;

destructor TdxSpreadSheetHistoryConditionalFormattingRuleCustomCommand.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

class function TdxSpreadSheetHistoryConditionalFormattingRuleCustomCommand.ActionClass: TdxSpreadSheetHistoryActionClass;
begin
  Result := TdxSpreadSheetHistoryChangeConditionalFormattingAction;
end;

procedure TdxSpreadSheetHistoryConditionalFormattingRuleCustomCommand.Restore;
var
  AReader: TcxReader;
begin
  FData.Position := 0;
  AReader := TcxReader.Create(FData, dxSpreadSheetBinaryFormatVersion);
  try
    Rule.LoadFromStream(AReader);
  finally
    AReader.Free;
  end;
end;

procedure TdxSpreadSheetHistoryConditionalFormattingRuleCustomCommand.Store(
  ARule: TdxSpreadSheetCustomConditionalFormattingRule);
var
  AWriter: TcxWriter;
begin
  FData := TMemoryStream.Create;
  AWriter := TcxWriter.Create(FData, dxSpreadSheetBinaryFormatVersion);
  try
    ARule.SaveToStream(AWriter);
  finally
    AWriter.Free;
  end;
end;

function TdxSpreadSheetHistoryConditionalFormattingRuleCustomCommand.GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
begin
  Result := TdxSpreadSheetTableView(View).ConditionalFormatting;
end;

function TdxSpreadSheetHistoryConditionalFormattingRuleCustomCommand.GetRule: TdxSpreadSheetCustomConditionalFormattingRule;
begin
  Result := ConditionalFormatting.Rules[FRuleIndex];
end;

{ TdxSpreadSheetHistoryCreateConditionalFormattingRuleCommand }

constructor TdxSpreadSheetHistoryCreateConditionalFormattingRuleCommand.Create(
  ARule: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  inherited Create(ARule);
  FRuleClass := TdxSpreadSheetCustomConditionalFormattingRuleClass(ARule.ClassType);
end;

procedure TdxSpreadSheetHistoryCreateConditionalFormattingRuleCommand.Redo;
begin
  FRuleClass.Create(TdxSpreadSheetTableView(View).ConditionalFormatting).Index := FRuleIndex;
end;

procedure TdxSpreadSheetHistoryCreateConditionalFormattingRuleCommand.Undo;
begin
  Rule.Free;
end;

{ TdxSpreadSheetHistoryChangeConditionalFormattingRuleCommand }

procedure TdxSpreadSheetHistoryChangeConditionalFormattingRuleCommand.Redo;
begin
  Restore;
end;

procedure TdxSpreadSheetHistoryChangeConditionalFormattingRuleCommand.Undo;
begin
  Restore;
end;

procedure TdxSpreadSheetHistoryChangeConditionalFormattingRuleCommand.Initialize;
begin
  inherited Initialize;
  Store(Rule);
end;

procedure TdxSpreadSheetHistoryChangeConditionalFormattingRuleCommand.Restore;
var
  AData: TMemoryStream;
begin
  AData := nil;
  ExchangePointers(AData, FData);
  try
    Store(Rule);
    ExchangePointers(AData, FData);
    inherited Restore;
    ExchangePointers(AData, FData);
  finally
    AData.Free;
  end;
end;

{ TdxSpreadSheetHistoryChangeConditionalFormattingRuleIndexCommand }

constructor TdxSpreadSheetHistoryChangeConditionalFormattingRuleIndexCommand.Create(AOldIndex, ANewIndex: Integer);
begin
  inherited Create;
  FOldIndex := AOldIndex;
  FNewIndex := ANewIndex;
end;

class function TdxSpreadSheetHistoryChangeConditionalFormattingRuleIndexCommand.ActionClass: TdxSpreadSheetHistoryActionClass;
begin
  Result := TdxSpreadSheetHistoryChangeConditionalFormattingAction;
end;

procedure TdxSpreadSheetHistoryChangeConditionalFormattingRuleIndexCommand.Redo;
begin
  ConditionalFormatting.Rules[FOldIndex].Index := FNewIndex;
end;

procedure TdxSpreadSheetHistoryChangeConditionalFormattingRuleIndexCommand.Undo;
begin
  ConditionalFormatting.Rules[FNewIndex].Index := FOldIndex;
end;

function TdxSpreadSheetHistoryChangeConditionalFormattingRuleIndexCommand.GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
begin
  Result := TdxSpreadSheetTableView(View).ConditionalFormatting;
end;

{ TdxSpreadSheetHistoryDeleteConditionalFormattingRuleCommand }

procedure TdxSpreadSheetHistoryDeleteConditionalFormattingRuleCommand.Redo;
begin
  inherited Undo;
end;

procedure TdxSpreadSheetHistoryDeleteConditionalFormattingRuleCommand.Undo;
begin
  inherited Redo;
  Restore;
end;

procedure TdxSpreadSheetHistoryDeleteConditionalFormattingRuleCommand.Initialize;
begin
  inherited Initialize;
  Store(Rule);
end;

{ TdxSpreadSheetHistoryChangePrintingOptionsCommand }

destructor TdxSpreadSheetHistoryChangePrintingOptionsCommand.Destroy;
begin
  FreeAndNil(FSource);
  inherited;
end;

class function TdxSpreadSheetHistoryChangePrintingOptionsCommand.ActionClass: TdxSpreadSheetHistoryActionClass;
begin
  Result := TdxSpreadSheetHistoryChangePrintingOptionsAction;
end;

procedure TdxSpreadSheetHistoryChangePrintingOptionsCommand.Redo;
begin
  Undo;
end;

procedure TdxSpreadSheetHistoryChangePrintingOptionsCommand.Undo;
var
  ASource: TdxSpreadSheetTableViewOptionsPrint;
begin
  ASource := TdxSpreadSheetTableViewOptionsPrint.Create(nil);
  try
    ASource.Assign(View.OptionsPrint);
    View.OptionsPrint.Assign(FSource);
    ExchangePointers(ASource, FSource);
  finally
    ASource.Free;
  end;
end;

procedure TdxSpreadSheetHistoryChangePrintingOptionsCommand.Initialize;
begin
  inherited;
  FSource := TdxSpreadSheetTableViewOptionsPrint.Create(nil);
  FSource.Assign(View.OptionsPrint);
end;

function TdxSpreadSheetHistoryChangePrintingOptionsCommand.GetView: TdxSpreadSheetTableView;
begin
  Result := inherited View as TdxSpreadSheetTableView;
end;

{ TdxSpreadSheetHistoryCreateDefinedNameCommand }

constructor TdxSpreadSheetHistoryCreateDefinedNameCommand.Create(ADefinedName: TdxSpreadSheetDefinedName);
begin
  FCaption := ADefinedName.Caption;
  FReference := ADefinedName.Reference;
  FScope := ADefinedName.Scope;
end;

class function TdxSpreadSheetHistoryCreateDefinedNameCommand.ActionClass: TdxSpreadSheetHistoryActionClass;
begin
  Result := TdxSpreadSheetHistoryCreateDefinedNameAction;
end;

procedure TdxSpreadSheetHistoryCreateDefinedNameCommand.Redo;
begin
  View.SpreadSheet.DefinedNames.Add(FCaption, FReference, FScope);
end;

procedure TdxSpreadSheetHistoryCreateDefinedNameCommand.Undo;
begin
  View.SpreadSheet.DefinedNames.GetItemByName(FCaption, FScope).Free;
end;

end.
