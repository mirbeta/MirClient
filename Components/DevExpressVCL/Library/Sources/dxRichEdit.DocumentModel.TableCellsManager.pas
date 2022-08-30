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

unit dxRichEdit.DocumentModel.TableCellsManager;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Paragraphs,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.Tables.Core,
  dxRichEdit.DocumentModel.Tables;

type
  TdxTableCellNodeSortedList = class;

  { TdxTableCellNode }

  TdxTableCellNode = class
  strict private
    FCell: TdxTableCell;
    FChildNodes: TdxTableCellNodeSortedList;
    function GetIsRootNode: Boolean;
  strict private
    function CreateChildNodes(AFirstCell: TdxTableCell): TdxTableCellNodeSortedList;
    function CreateChildNodesCore: TdxTableCellNodeSortedList;
    procedure AddChildAtPosition(ACell: TdxTableCell; APosition: Integer);
    procedure ResetCachedTableLayoutInfoRecursive;
    procedure GetCellsByParagraphIndexCore(AParagraphIndex: TdxParagraphIndex; ANestedLevel: Integer; ACells: TdxTableCellList);
    procedure ShiftParagraphIndex(ADelta: Integer);
    procedure RemoveCore(ANodeIndex: Integer);
    procedure RecalcNestedLevelRecursive(ACellNode: TdxTableCellNode);
  protected
    function GetChildNodes: TdxTableCellNodeSortedList; virtual;
    procedure SetChildNodes(const Value: TdxTableCellNodeSortedList); virtual;

    function GetCellsByParagraphIndex(AParagraphIndex: TdxParagraphIndex; ANestedLevel: Integer): TdxTableCellList; virtual;
    function GetSubTree(AStart, AEnd: TdxParagraphIndex; AFromNestedLevel: Integer): TdxTableCellNode; virtual;
    procedure RemoveCellNode(ATargetCellNode, ASourceCellNode: TdxTableCellNode; AStart, AEnd: TdxParagraphIndex); virtual;

    property IsRootNode: Boolean read GetIsRootNode;
  public
    constructor Create(ACell: TdxTableCell = nil);
    destructor Destroy; override;
    procedure Add(ACell: TdxTableCell);
    function GetCellByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxTableCell;
    function GetReference: TdxTableCellNode;
    procedure ResetCachedTableLayoutInfo(AFrom, ATo: TdxParagraphIndex);
    function IsParagraphInCell(AParagraphIndex: TdxParagraphIndex): Boolean;
    function GetCellByNestingLevel(AParagraphIndex: TdxParagraphIndex; ANestingLevel: Integer; ACellFromPreviousLevel: TdxTableCell): TdxTableCell;
    procedure OnParagraphRemove(AIndex: TdxParagraphIndex);
    function OnParagraphInserted(AIndex: TdxParagraphIndex; ACellToInsert: Boolean): TdxTableCellNode;
    procedure Remove(ACell: TdxTableCell);
    property Cell: TdxTableCell read FCell;
    property ChildNodes: TdxTableCellNodeSortedList read GetChildNodes write SetChildNodes;
  end;

  TdxTableCellNodeSortedList = class(TdxObjectSortedList<TdxTableCellNode>);

  { TdxTableCellTree }

  TdxTableCellTree = class
  strict private
    FRoot: TdxTableCellNode;
    function OnParagraphInsertedRecursive(AIndex: TdxParagraphIndex; ATargetCell: TdxTableCell): TdxTableCellNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ACell: TdxTableCell);
    function GetCellByParagraphIndex(AIndex: TdxParagraphIndex): TdxTableCell;
    procedure ResetCachedTableLayoutInfo(AFrom, ATo: TdxParagraphIndex);
    function IsParagraphInCell(AIndex: TdxParagraphIndex): Boolean;
    procedure OnParagraphRemove(AIndex: TdxParagraphIndex);
    procedure OnParagraphInserted(AIndex: TdxParagraphIndex; ACell: TdxTableCell);
    procedure Clear;
    procedure Remove(ACell: TdxTableCell);
    function GetCellByNestingLevel(AParagraphIndex: TdxParagraphIndex; ANestingLevel: Integer): TdxTableCell;
    function GetCellsByParagraphIndex(AParagraphIndex: TdxParagraphIndex; ANestedLevel: Integer): TdxTableCellList;
    function GetCellSubTree(AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex; AFromNestedLevel: Integer): TdxTableCellNode;
  end;

  { TdxTableCellsManager }

  TdxTableCellsManager = class(TcxIUnknownObject, IdxDocumentModelStructureChangedListener)
  strict private
    FCellTree: TdxTableCellTree;
    FPieceTable: TdxSimplePieceTable;
  protected
  {$REGION 'IdxDocumentModelStructureChangedListener'}
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); overload;
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); overload;
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
      ALength: Integer; AHistoryNotificationId: Integer); overload;
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength: Integer; AHistoryNotificationId: Integer); overload;
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); overload;
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex;
      ASplitOffset: Integer; ATailRunLength: Integer); overload;
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); overload;
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); overload;
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer); overload;
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer); overload;
  {$ENDREGION}
    procedure OnFieldInserted(AFieldIndex: Integer); overload; virtual;
    procedure OnFieldRemoved(AFieldIndex: Integer); overload; virtual;
    procedure OnRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); overload; virtual;
    procedure OnRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); overload; virtual;
    procedure OnRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer); overload; virtual;
    procedure OnRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); overload; virtual;
    procedure OnRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); overload; virtual;
    procedure OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); overload; virtual;
    procedure OnParagraphRemove(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      AHistoryNotificationId: Integer); virtual;
    procedure OnParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      AHistoryNotificationId: Integer); overload; virtual;
    procedure OnParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); overload; virtual;

    property PieceTable: TdxSimplePieceTable read FPieceTable;
    property CellTree: TdxTableCellTree read FCellTree;
  public
    constructor Create(APieceTable: TdxSimplePieceTable);
    destructor Destroy; override;
    function IsInCell(AParagraph: TdxParagraphBase): Boolean; virtual;
    function GetCell(AParagraph: TdxParagraphBase): TdxTableCell; virtual;
    procedure Clear;
    procedure ResetCachedTableLayoutInfo(AFrom, ATo: TdxParagraphIndex); virtual;
    function GetCellByNestingLevel(AParagraphIndex: TdxParagraphIndex; ANestingLevel: Integer): TdxTableCell;
    function GetCellsByParagraphIndex(AParagraphIndex: TdxParagraphIndex; ANestedLevel: Integer): TdxTableCellList;
    function GetCellSubTree(AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex; AFromNestedLevel: Integer): TdxTableCellNode;
    procedure ConvertParagraphsIntoTableCells(ARow: TdxTableRow; AParagraphIndex: TdxParagraphIndex; AParagraphCount: Integer);
    procedure RevertParagraphsFromTableCells(ARow: TdxTableRow);
    function CreateTableCore(AFirstParagraphIndex: TdxParagraphIndex; ARowCount, ACellCount: Integer): TdxTable;
    procedure RemoveTable(ATable: TdxTable);
    procedure InsertTable(ATable: TdxTable);
    procedure InitializeTableCell(ACell: TdxTableCell; AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex);
    procedure RemoveTableCell(ACell: TdxTableCell);
  end;

implementation

uses
  Contnrs, Math,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.DocumentModel.PieceTable;

type
  { TdxTableCellNodeAndCellComparer }

  TdxTableCellNodeAndCellComparer = class(TcxIUnknownObject, IdxComparable<TdxTableCellNode>)
  strict private
    FCellToInsert: TdxTableCell;
    function IsNestedCell(ANestedCell, AOuterCell: TdxTableCell): Boolean;
  public
    constructor Create(ACellToInsert: TdxTableCell);
    function CompareTo(const AOther: TdxTableCellNode): Integer;
  end;

  { TdxTableCellNodeReference }

  TdxTableCellNodeReference = class(TdxTableCellNode)
  strict private
    FNode: TdxTableCellNode;
  protected
    function GetChildNodes: TdxTableCellNodeSortedList; override;
    procedure SetChildNodes(const Value: TdxTableCellNodeSortedList); override;
  public
    constructor Create(ANode: TdxTableCellNode);
  end;

  { TdxTableCellNodeComparer }

  TdxTableCellNodeComparer = class(TComparer<TdxTableCellNode>)
  public
    function Compare(const X, Y: TdxTableCellNode): Integer; override;
  end;

  { TdxTableCellNodeAndParagraphIndexComparer }

  TdxTableCellNodeAndParagraphIndexComparer = class(TcxIUnknownObject, IdxComparable<TdxTableCellNode>)
  strict private
    FIndex: TdxParagraphIndex;
  public
    constructor Create(AIndex: TdxParagraphIndex);
    function CompareTo(const AOther: TdxTableCellNode): Integer;
  end;

{ TdxTableCellNodeAndCellComparer }

constructor TdxTableCellNodeAndCellComparer.Create(ACellToInsert: TdxTableCell);
begin
  inherited Create;
  FCellToInsert := ACellToInsert;
end;

function TdxTableCellNodeAndCellComparer.CompareTo(const AOther: TdxTableCellNode): Integer;
var
  AOtherCell: TdxTableCell;
begin
  AOtherCell := AOther.Cell;
  if IsNestedCell(FCellToInsert, AOtherCell) then
    Exit(0);
  if IsNestedCell(AOtherCell, FCellToInsert) then
    Result := 1
  else
    Result := AOtherCell.StartParagraphIndex - FCellToInsert.StartParagraphIndex;
end;

function TdxTableCellNodeAndCellComparer.IsNestedCell(ANestedCell,
  AOuterCell: TdxTableCell): Boolean;
begin
  Result := (ANestedCell.StartParagraphIndex >= AOuterCell.StartParagraphIndex) and
    (ANestedCell.EndParagraphIndex <= AOuterCell.EndParagraphIndex);
end;

{ TdxTableCellNodeReference }

constructor TdxTableCellNodeReference.Create(ANode: TdxTableCellNode);
begin
  inherited Create(ANode.Cell);
  FNode := ANode;
end;

function TdxTableCellNodeReference.GetChildNodes: TdxTableCellNodeSortedList;
begin
  Result := FNode.ChildNodes;
end;

procedure TdxTableCellNodeReference.SetChildNodes(
  const Value: TdxTableCellNodeSortedList);
begin
  FNode.ChildNodes := Value;
end;

{ TdxTableCellNodeComparer }

function TdxTableCellNodeComparer.Compare(const X, Y: TdxTableCellNode): Integer;
begin
  Result := X.Cell.StartParagraphIndex - Y.Cell.StartParagraphIndex;
end;

{ TdxTableCellNodeAndParagraphIndexComparer }

constructor TdxTableCellNodeAndParagraphIndexComparer.Create(AIndex: TdxParagraphIndex);
begin
  inherited Create;
  FIndex := AIndex;
end;

function TdxTableCellNodeAndParagraphIndexComparer.CompareTo(
  const AOther: TdxTableCellNode): Integer;
begin
  if FIndex < AOther.Cell.StartParagraphIndex then
    Result := 1
  else
    if FIndex > AOther.Cell.EndParagraphIndex then
      Result := -1
    else
      Result := 0;
end;

{ TdxTableCellNode }

constructor TdxTableCellNode.Create(ACell: TdxTableCell);
begin
  inherited Create;
  FCell := ACell;
end;

destructor TdxTableCellNode.Destroy;
begin
  FChildNodes.Free;
  inherited Destroy;
end;

procedure TdxTableCellNode.Add(ACell: TdxTableCell);
var
  ANextCell: TdxTableCell;
  APrevTable, ANextTable: TdxTable;
  AChildIndex, ANextCellIndex: Integer;
  ANextNode, ANewNode: TdxTableCellNode;
  AComparer: TdxTableCellNodeAndCellComparer;
begin
  if ChildNodes = nil then
  begin
    ChildNodes := CreateChildNodes(ACell);
    Exit;
  end;

  AComparer := TdxTableCellNodeAndCellComparer.Create(ACell);
  try
    AChildIndex := ChildNodes.BinarySearch(AComparer);
  finally
    AComparer.Free;
  end;

  if AChildIndex >= 0 then
  begin
    AddChildAtPosition(ACell, AChildIndex);
    Exit;
  end;

  ANextCellIndex := not AChildIndex;
  if ANextCellIndex < ChildNodes.Count then
  begin
    ANextNode := ChildNodes[ANextCellIndex];
    ANextCell := ANextNode.Cell;
    if ANextCell.EndParagraphIndex < ACell.EndParagraphIndex then
    begin
      ANewNode := TdxTableCellNode.Create(ACell);
      ANewNode.ChildNodes := ANewNode.CreateChildNodesCore;
      APrevTable := nil;
      while (ANextCellIndex < ChildNodes.Count) and
        ((ChildNodes[ANextCellIndex].Cell.EndParagraphIndex < ACell.EndParagraphIndex) or
          (ChildNodes[ANextCellIndex].Cell.EndParagraphIndex = ACell.EndParagraphIndex) and
          (ChildNodes[ANextCellIndex].Cell.Table.NestedLevel > ACell.Table.NestedLevel)) do
      begin
        ANextNode := ChildNodes[ANextCellIndex];
        Assert(not (ANextNode is TdxTableCellNodeReference));
        ChildNodes.ExtractByIndex(ANextCellIndex);
        ANewNode.ChildNodes.Add(ANextNode);
        ANextNode.Cell.Table.SetParentCell(ANewNode.Cell);
        ANextTable := ANextNode.Cell.Table;
        if ANextTable <> APrevTable then
        begin
          ANextTable.SetParentCell(ANewNode.Cell);
          APrevTable := ANextTable;
        end;
      end;
      ChildNodes.Add(ANewNode);
    end
    else
      ChildNodes.Insert(ANextCellIndex, TdxTableCellNode.Create(ACell));
  end
  else
    ChildNodes.Insert(ChildNodes.Count, TdxTableCellNode.Create(ACell));
end;

procedure TdxTableCellNode.AddChildAtPosition(ACell: TdxTableCell; APosition: Integer);
var
  AOtherCellNode: TdxTableCellNode;
  AOtherCell: TdxTableCell;
  ANewNode: TdxTableCellNode;
begin
  AOtherCellNode := ChildNodes[APosition];
  AOtherCell := AOtherCellNode.Cell;
  if AOtherCell.Table.NestedLevel < ACell.Table.NestedLevel then
  begin
    AOtherCellNode.Add(ACell);
    Exit;
  end;

  Assert((AOtherCell.Table.NestedLevel > ACell.Table.NestedLevel) and
    (ACell.StartParagraphIndex = AOtherCell.StartParagraphIndex) and
    (ACell.EndParagraphIndex = AOtherCell.EndParagraphIndex));

  Assert(not (AOtherCellNode is TdxTableCellNodeReference));
  ChildNodes.ExtractByIndex(APosition);
  ANewNode := TdxTableCellNode.Create(ACell);
  ANewNode.ChildNodes := ANewNode.CreateChildNodesCore;
  ANewNode.ChildNodes.Add(AOtherCellNode);
  ChildNodes.Add(ANewNode);
end;

function TdxTableCellNode.CreateChildNodes(
  AFirstCell: TdxTableCell): TdxTableCellNodeSortedList;
begin
  Result := CreateChildNodesCore;
  Result.Add(TdxTableCellNode.Create(AFirstCell));
end;

function TdxTableCellNode.CreateChildNodesCore: TdxTableCellNodeSortedList;
begin
  Result := TdxTableCellNodeSortedList.Create(TdxTableCellNodeComparer.Create);
end;

function TdxTableCellNode.GetCellByNestingLevel(AParagraphIndex: TdxParagraphIndex;
  ANestingLevel: Integer; ACellFromPreviousLevel: TdxTableCell): TdxTableCell;
var
  AIndex: Integer;
  ACellNode: TdxTableCell;
  ANode: TdxTableCellNode;
  AComparer: TdxTableCellNodeAndParagraphIndexComparer;
begin
  if ChildNodes = nil then
    Exit(ACellFromPreviousLevel);

  AComparer := TdxTableCellNodeAndParagraphIndexComparer.Create(AParagraphIndex);
  try
    AIndex := ChildNodes.BinarySearch(AComparer);
  finally
    AComparer.Free;
  end;
  if AIndex < 0 then
    Exit(ACellFromPreviousLevel);

  ANode := ChildNodes[AIndex];
  ACellNode := ANode.Cell;
  if ACellNode.Table.NestedLevel = ANestingLevel then
    Result := ACellNode
  else
    Result := ANode.GetCellByNestingLevel(AParagraphIndex, ANestingLevel, ACellNode);
end;

function TdxTableCellNode.GetCellByParagraphIndex(
  AParagraphIndex: TdxParagraphIndex): TdxTableCell;
var
  AIndex: Integer;
  AComparer: TdxTableCellNodeAndParagraphIndexComparer;
begin
  if ChildNodes = nil then
    Exit(FCell);
  AComparer := TdxTableCellNodeAndParagraphIndexComparer.Create(AParagraphIndex);
  try
    AIndex := ChildNodes.BinarySearch(AComparer);
  finally
    AComparer.Free;
  end;
  if AIndex < 0 then
    Result := FCell
  else
    Result := ChildNodes[AIndex].GetCellByParagraphIndex(AParagraphIndex);
end;

function TdxTableCellNode.GetReference: TdxTableCellNode;
begin
  Result := TdxTableCellNodeReference.Create(Self);
end;

function TdxTableCellNode.GetCellsByParagraphIndex(AParagraphIndex: TdxParagraphIndex;
  ANestedLevel: Integer): TdxTableCellList;
begin
  Result := TdxTableCellList.Create;
  GetCellsByParagraphIndexCore(AParagraphIndex, ANestedLevel, Result);
end;

procedure TdxTableCellNode.GetCellsByParagraphIndexCore(AParagraphIndex: TdxParagraphIndex;
  ANestedLevel: Integer; ACells: TdxTableCellList);
var
  AIndex: Integer;
  ANode: TdxTableCellNode;
  ACurrentCell: TdxTableCell;
  AComparer: TdxTableCellNodeAndParagraphIndexComparer;
begin
  if ChildNodes = nil then
    Exit;

  AComparer := TdxTableCellNodeAndParagraphIndexComparer.Create(AParagraphIndex);
  try
    AIndex := ChildNodes.BinarySearch(AComparer);
  finally
    AComparer.Free;
  end;
  if AIndex < 0 then
    Exit;

  ANode := ChildNodes[AIndex];
  ACurrentCell := ANode.Cell;
  if ACurrentCell.Table.NestedLevel >= ANestedLevel then
    ACells.Add(ACurrentCell);
  ANode.GetCellsByParagraphIndexCore(AParagraphIndex, ANestedLevel, ACells);
end;

function TdxTableCellNode.GetChildNodes: TdxTableCellNodeSortedList;
begin
  Result := FChildNodes;
end;

function TdxTableCellNode.GetIsRootNode: Boolean;
begin
  Result := Cell <> nil;
end;

function TdxTableCellNode.GetSubTree(AStart, AEnd: TdxParagraphIndex;
  AFromNestedLevel: Integer): TdxTableCellNode;
var
  I, AStartIndex, AEndIndex: Integer;
  ACurrentNode, AAnother: TdxTableCellNode;
  AComparer: TdxTableCellNodeAndParagraphIndexComparer;
begin
  if ChildNodes = nil then
    Exit(nil);

  Result := TdxTableCellNode.Create;
  if Result.ChildNodes = nil then
    Result.ChildNodes := CreateChildNodesCore;

  AComparer := TdxTableCellNodeAndParagraphIndexComparer.Create(AStart);
  try
    AStartIndex := ChildNodes.BinarySearch(AComparer);
  finally
    AComparer.Free;
  end;

  if AStartIndex < 0 then
  begin
    AComparer := TdxTableCellNodeAndParagraphIndexComparer.Create(AEnd);
    try
      AEndIndex := ChildNodes.BinarySearch(AComparer);
    finally
      AComparer.Free;
    end;
    if AEndIndex < 0 then
    begin
      if (not AEndIndex) < 0 then
        TdxRichEditExceptions.ThrowInternalException;
      AEndIndex := Math.Min(not AEndIndex, ChildNodes.Count - 1);
    end;
    for I := not AStartIndex to AEndIndex do
      if (ChildNodes[I].Cell.StartParagraphIndex <= AEnd) and (ChildNodes[I].Cell.EndParagraphIndex <= AEnd) then
        Result.ChildNodes.Add(ChildNodes[I].GetReference);
    Exit;
  end;
  ACurrentNode := ChildNodes[AStartIndex];

  if ACurrentNode.Cell.Table.NestedLevel < AFromNestedLevel then
  begin
    AComparer := TdxTableCellNodeAndParagraphIndexComparer.Create(AEnd);
    try
      AEndIndex := ChildNodes.BinarySearch(AComparer);
    finally
      AComparer.Free;
    end;
    if AEndIndex < 0 then
    begin
      AEndIndex := AStartIndex;
      AAnother := ChildNodes[AEndIndex].GetSubTree(AStart, AEnd, AFromNestedLevel);
      if AAnother = nil then
        Exit;
    end;

    for I := AStartIndex to AEndIndex do
    begin
      ACurrentNode := ChildNodes[I];
      AAnother := ACurrentNode.GetSubTree(AStart, AEnd, AFromNestedLevel);
      if AAnother <> nil then
        Result.ChildNodes.Add(AAnother);
    end;
    Exit;
  end;
  if ACurrentNode.Cell.EndParagraphIndex = AEnd then
  begin
    Result.ChildNodes.Add(ACurrentNode.GetReference);
    Exit;
  end;
  if (ACurrentNode.Cell.StartParagraphIndex >= AStart) and (ACurrentNode.Cell.EndParagraphIndex <= AEnd) then
  begin
    Result.ChildNodes.Add(ACurrentNode.GetReference);

    AComparer := TdxTableCellNodeAndParagraphIndexComparer.Create(AEnd);
    try
      AEndIndex := ChildNodes.BinarySearch(AComparer);
    finally
      AComparer.Free;
    end;
    if AEndIndex < 0 then
      AEndIndex := ChildNodes.Count - 1;
    for I := AStartIndex + 1 to AEndIndex do
      if (ChildNodes[I].Cell.StartParagraphIndex <= AEnd) and (ChildNodes[I].Cell.EndParagraphIndex <= AEnd) then
        Result.ChildNodes.Add(ChildNodes[I].GetReference);
  end
  else
  begin
    FreeAndNil(Result);
    Exit(ACurrentNode.GetSubTree(AStart, AEnd, AFromNestedLevel));
  end;
end;

function TdxTableCellNode.IsParagraphInCell(AParagraphIndex: TdxParagraphIndex): Boolean;
var
  AIndex: Integer;
  AComparer: TdxTableCellNodeAndParagraphIndexComparer;
begin
  if ChildNodes = nil then
    Exit(False);
  AComparer := TdxTableCellNodeAndParagraphIndexComparer.Create(AParagraphIndex);
  try
    AIndex := ChildNodes.BinarySearch(AComparer);
  finally
    AComparer.Free;
  end;
  Result := AIndex >= 0;
end;

function TdxTableCellNode.OnParagraphInserted(AIndex: TdxParagraphIndex;
  ACellToInsert: Boolean): TdxTableCellNode;
var
  I, ACellIndex, ANextCellIndex, AChildCount: Integer;
  AComparer: TdxTableCellNodeAndParagraphIndexComparer;
begin
  if FCell <> nil then
  begin
    Assert((FCell.StartParagraphIndex <= AIndex) and (AIndex <= FCell.EndParagraphIndex));
    FCell.EndParagraphIndex := FCell.EndParagraphIndex + 1;
  end;
  if ChildNodes = nil then
    Exit(nil);
  AComparer := TdxTableCellNodeAndParagraphIndexComparer.Create(AIndex);
  try
    ACellIndex := ChildNodes.BinarySearch(AComparer);
  finally
    AComparer.Free;
  end;
  ANextCellIndex := ACellIndex;
  if ANextCellIndex < 0 then
    ANextCellIndex := not ANextCellIndex
  else
    if not ACellToInsert then
      Inc(ANextCellIndex);
  AChildCount := ChildNodes.Count;
  for I := ANextCellIndex to AChildCount - 1 do
    ChildNodes[I].ShiftParagraphIndex(1);
  if (ACellIndex >= 0) and not ACellToInsert then
    Result := ChildNodes[ACellIndex]
  else
    Result := nil;
end;

procedure TdxTableCellNode.OnParagraphRemove(AIndex: TdxParagraphIndex);
var
  I, ACellIndex: Integer;
  AComparer: TdxTableCellNodeAndParagraphIndexComparer;
begin
  if FCell <> nil then
    FCell.EndParagraphIndex := FCell.EndParagraphIndex - 1;
  if ChildNodes = nil then
    Exit;
  AComparer := TdxTableCellNodeAndParagraphIndexComparer.Create(AIndex);
  try
    ACellIndex := ChildNodes.BinarySearch(AComparer);
  finally
    AComparer.Free;
  end;
  if ACellIndex >= 0 then
  begin
    ChildNodes[ACellIndex].OnParagraphRemove(AIndex);
    Inc(ACellIndex);
  end
  else
    ACellIndex := not ACellIndex;
  for I := ACellIndex to ChildNodes.Count - 1 do
    ChildNodes[I].ShiftParagraphIndex(-1);
end;

procedure TdxTableCellNode.RecalcNestedLevelRecursive(ACellNode: TdxTableCellNode);
var
  I: Integer;
  AChildNodes: TdxSortedList<TdxTableCellNode>;
begin
  AChildNodes := ACellNode.ChildNodes;
  if AChildNodes = nil then
    Exit;
  for I := 0 to AChildNodes.Count - 1 do
  begin
    AChildNodes[I].Cell.Table.RecalNestedLevel;
    RecalcNestedLevelRecursive(AChildNodes[I]);
  end;
end;

procedure TdxTableCellNode.Remove(ACell: TdxTableCell);
var
  I: Integer;
  ANestedCell: TdxTableCell;
begin
  if ChildNodes = nil then
    Exit;
  for I := 0 to ChildNodes.Count - 1 do
  begin
    ANestedCell := ChildNodes[I].Cell;
    if ACell = ANestedCell then
    begin
      RemoveCore(I);
      Exit;
    end
    else
      if (ACell.StartParagraphIndex >= ANestedCell.StartParagraphIndex) and
        (ACell.EndParagraphIndex <= ANestedCell.EndParagraphIndex) and
        (ACell.Table.NestedLevel > ANestedCell.Table.NestedLevel) then
      begin
        ChildNodes[I].Remove(ACell);
        Exit;
      end;
  end;
end;

procedure TdxTableCellNode.RemoveCellNode(ATargetCellNode, ASourceCellNode: TdxTableCellNode;
  AStart, AEnd: TdxParagraphIndex);
begin
end;

procedure TdxTableCellNode.RemoveCore(ANodeIndex: Integer);
var
  I: Integer;
  ARemovedNode: TdxTableCellNode;
begin
  ARemovedNode := ChildNodes[ANodeIndex];
  Assert(not (ARemovedNode is TdxTableCellNodeReference));
  ChildNodes.ExtractByIndex(ANodeIndex);
  try
    if ARemovedNode.ChildNodes = nil then
      Exit;
    for I := 0 to ARemovedNode.ChildNodes.Count - 1 do
    begin
      ChildNodes.Add(ARemovedNode.ChildNodes[I]);
      ARemovedNode.ChildNodes[I].Cell.Table.SetParentCell(Cell);
      RecalcNestedLevelRecursive(ARemovedNode.ChildNodes[I]);
    end;
  finally
    ARemovedNode.Free;
  end;
end;

procedure TdxTableCellNode.ResetCachedTableLayoutInfo(AFrom, ATo: TdxParagraphIndex);
var
  I, AFromIndex, AToIndex: Integer;
  AComparer: TdxTableCellNodeAndParagraphIndexComparer;
begin
  if ChildNodes = nil then
    Exit;
  AComparer := TdxTableCellNodeAndParagraphIndexComparer.Create(AFrom);
  try
    AFromIndex := ChildNodes.BinarySearch(AComparer);
  finally
    AComparer.Free;
  end;
  if AFromIndex >= 0 then
  begin
    ChildNodes[AFromIndex].Cell.Table.ResetCachedLayoutInfo;
    ChildNodes[AFromIndex].ResetCachedTableLayoutInfo(AFrom, ATo);
    Inc(AFromIndex);
  end
  else
    AFromIndex := not AFromIndex;
  AComparer := TdxTableCellNodeAndParagraphIndexComparer.Create(ATo);
  try
    AToIndex := ChildNodes.BinarySearch(AComparer);
  finally
    AComparer.Free;
  end;
  if AToIndex >= 0 then
  begin
    ChildNodes[AToIndex].Cell.Table.ResetCachedLayoutInfo;
    ChildNodes[AToIndex].ResetCachedTableLayoutInfo(AFrom, ATo);
  end
  else
    AToIndex := not AToIndex;
  Dec(AToIndex);
  for I := AFromIndex to AToIndex do
  begin
    ChildNodes[I].Cell.Table.ResetCachedLayoutInfo;
    ChildNodes[I].ResetCachedTableLayoutInfoRecursive;
  end;
end;

procedure TdxTableCellNode.ResetCachedTableLayoutInfoRecursive;
var
  I: Integer;
begin
  if ChildNodes = nil then
    Exit;
  for I := 0 to ChildNodes.Count - 1 do
  begin
    ChildNodes[I].Cell.Table.ResetCachedLayoutInfo;
    ChildNodes[I].ResetCachedTableLayoutInfoRecursive;
  end;
end;

procedure TdxTableCellNode.SetChildNodes(const Value: TdxTableCellNodeSortedList);
begin
  FChildNodes := Value;
end;

procedure TdxTableCellNode.ShiftParagraphIndex(ADelta: Integer);
var
  I: Integer;
begin
  FCell.StartParagraphIndex := FCell.StartParagraphIndex + ADelta;
  FCell.EndParagraphIndex := FCell.EndParagraphIndex + ADelta;
  if ChildNodes = nil then
    Exit;
  for I := 0 to ChildNodes.Count - 1 do
    ChildNodes[I].ShiftParagraphIndex(ADelta);
end;

{ TdxTableCellTree }

procedure TdxTableCellTree.Add(ACell: TdxTableCell);
begin
  FRoot.Add(ACell);
end;

procedure TdxTableCellTree.Clear;
begin
  FRoot.Free;
  FRoot := TdxTableCellNode.Create;
end;

constructor TdxTableCellTree.Create;
begin
  inherited Create;
  FRoot := TdxTableCellNode.Create;
end;

destructor TdxTableCellTree.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

function TdxTableCellTree.GetCellByNestingLevel(AParagraphIndex: TdxParagraphIndex;
  ANestingLevel: Integer): TdxTableCell;
begin
  Result := FRoot.GetCellByNestingLevel(AParagraphIndex, ANestingLevel, nil);
end;

function TdxTableCellTree.GetCellByParagraphIndex(AIndex: TdxParagraphIndex): TdxTableCell;
begin
  Result := FRoot.GetCellByParagraphIndex(AIndex);
end;

function TdxTableCellTree.GetCellsByParagraphIndex(AParagraphIndex: TdxParagraphIndex;
  ANestedLevel: Integer): TdxTableCellList;
begin
  Result := FRoot.GetCellsByParagraphIndex(AParagraphIndex, ANestedLevel);
end;

function TdxTableCellTree.GetCellSubTree(AStartParagraphIndex,
  AEndParagraphIndex: TdxParagraphIndex; AFromNestedLevel: Integer): TdxTableCellNode;
begin
  Result := FRoot.GetSubTree(AStartParagraphIndex, AEndParagraphIndex, AFromNestedLevel);
end;

function TdxTableCellTree.IsParagraphInCell(AIndex: TdxParagraphIndex): Boolean;
begin
  Result := FRoot.IsParagraphInCell(AIndex);
end;

procedure TdxTableCellTree.OnParagraphInserted(AIndex: TdxParagraphIndex; ACell: TdxTableCell);
var
  ANode: TdxTableCellNode;
begin
  ANode := OnParagraphInsertedRecursive(AIndex, ACell);
  ANode.OnParagraphInserted(AIndex, True);
end;

function TdxTableCellTree.OnParagraphInsertedRecursive(AIndex: TdxParagraphIndex;
  ATargetCell: TdxTableCell): TdxTableCellNode;
var
  AParentNode: TdxTableCellNode;
begin
  if ATargetCell <> nil then
  begin
    AParentNode := OnParagraphInsertedRecursive(AIndex, ATargetCell.Table.ParentCell);
    Result := AParentNode.OnParagraphInserted(AIndex, False);
    Assert(Result <> nil);
  end
  else
    Result := FRoot;
end;

procedure TdxTableCellTree.OnParagraphRemove(AIndex: TdxParagraphIndex);
begin
  FRoot.OnParagraphRemove(AIndex);
end;

procedure TdxTableCellTree.Remove(ACell: TdxTableCell);
begin
  FRoot.Remove(ACell);
end;

procedure TdxTableCellTree.ResetCachedTableLayoutInfo(AFrom, ATo: TdxParagraphIndex);
begin
  FRoot.ResetCachedTableLayoutInfo(AFrom, ATo);
end;

{ TdxTableCellsManager }

procedure TdxTableCellsManager.Clear;
begin
  FCellTree.Clear;
end;

procedure TdxTableCellsManager.ConvertParagraphsIntoTableCells(ARow: TdxTableRow; AParagraphIndex: TdxParagraphIndex;
  AParagraphCount: Integer);
var
  I: Integer;
  ACell: TdxTableCell;
begin
  for I := AParagraphIndex to AParagraphIndex + AParagraphCount - 1 do
  begin
    ACell := TdxTableCell.Create(ARow);
    ARow.Cells.AddInternal(ACell);
    InitializeTableCell(ACell, I, I);
  end;
end;

constructor TdxTableCellsManager.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create;
  Assert(APieceTable <> nil);
  FPieceTable := APieceTable;
  FCellTree := TdxTableCellTree.Create;
end;

function TdxTableCellsManager.CreateTableCore(AFirstParagraphIndex: TdxParagraphIndex; ARowCount,
  ACellCount: Integer): TdxTable;
var
  ACell: TdxTableCell;
  AParentCell: TdxTableCell;
  ARows: TdxTableRowCollection;
  ARowIndex, ACellIndex: Integer;
  ACells: TdxTableCellCollection;
  AParagraphIndex: TdxParagraphIndex;
  AParagraphs: TdxParagraphBaseCollection;
begin
  AParagraphs := PieceTable.Paragraphs;
  AParentCell := GetCell(AParagraphs[AFirstParagraphIndex]);
  Result := TdxTable.Create(PieceTable, AParentCell, ARowCount, ACellCount);
  AParagraphIndex := AFirstParagraphIndex;
  ARows := Result.Rows;
  for ARowIndex := 0 to ARowCount - 1 do
  begin
    ACells := ARows[ARowIndex].Cells;
    for ACellIndex := 0 to ACellCount - 1 do
    begin
      ACell := ACells[ACellIndex];
      InitializeTableCell(ACell, AParagraphIndex, AParagraphIndex);
      Inc(AParagraphIndex);
    end;
  end;
  TdxPieceTable(PieceTable).Tables.Add(Result);
end;

destructor TdxTableCellsManager.Destroy;
begin
  FCellTree.Free;
  inherited Destroy;
end;

procedure TdxTableCellsManager.InitializeTableCell(ACell: TdxTableCell; AStartParagraphIndex,
  AEndParagraphIndex: TdxParagraphIndex);
begin
  ACell.StartParagraphIndex := AStartParagraphIndex;
  ACell.EndParagraphIndex := AEndParagraphIndex;
  CellTree.Add(ACell);
end;

procedure TdxTableCellsManager.InsertTable(ATable: TdxTable);
var
  ACell: TdxTableCell;
  ARows: TdxTableRowCollection;
  ACells: TdxTableCellCollection;
  ARowIndex, ACellIndex: Integer;
begin
  ARows := ATable.Rows;
  for ARowIndex := 0 to ARows.Count - 1 do
  begin
    ACells := ARows[ARowIndex].Cells;
    for ACellIndex := 0 to ACells.Count - 1 do
    begin
      ACell := ACells[ACellIndex];
      CellTree.Add(ACell);
    end;
  end;
  TdxPieceTable(PieceTable).Tables.Add(ATable);
end;

function TdxTableCellsManager.IsInCell(AParagraph: TdxParagraphBase): Boolean;
begin
  Result := CellTree.IsParagraphInCell(AParagraph.Index);
end;

procedure TdxTableCellsManager.OnParagraphInserted(APieceTable: TdxCustomPieceTable;
  ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
  Assert(PieceTable = APieceTable);
  OnParagraphInserted(ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
end;

procedure TdxTableCellsManager.OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  Assert(PieceTable = APieceTable);
  OnParagraphRemove(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxTableCellsManager.OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  Assert(PieceTable = APieceTable);
  OnParagraphMerged(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxTableCellsManager.OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
begin
  Assert(PieceTable = APieceTable);
  OnRunInserted(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxTableCellsManager.OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
begin
  Assert(PieceTable = APieceTable);
  OnRunRemoved(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxTableCellsManager.OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxTableCellsManager.OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxTableCellsManager.OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
  Assert(PieceTable = APieceTable);
  OnRunSplit(AParagraphIndex, ARunIndex, ASplitOffset);
end;

procedure TdxTableCellsManager.OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer);
begin
  Assert(PieceTable = APieceTable);
  OnRunJoined(AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
end;

procedure TdxTableCellsManager.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  Assert(PieceTable = APieceTable);
  OnRunMerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxTableCellsManager.OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  Assert(PieceTable = APieceTable);
  OnRunUnmerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxTableCellsManager.OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  Assert(PieceTable = APieceTable);
  OnFieldRemoved(AFieldIndex);
end;

procedure TdxTableCellsManager.OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  Assert(PieceTable = APieceTable);
  OnFieldInserted(AFieldIndex);
end;

procedure TdxTableCellsManager.OnFieldInserted(AFieldIndex: Integer);
begin
end;

procedure TdxTableCellsManager.OnFieldRemoved(AFieldIndex: Integer);
begin
end;

procedure TdxTableCellsManager.OnParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex;
  AHistoryNotificationId: Integer);
var
  ATableCell: TdxTableCell absolute ACell;
begin
  if AIsParagraphMerged then
    Dec(AParagraphIndex);
  if ATableCell <> nil then
  begin
    if AParagraphIndex > TdxTableCell(ATableCell).EndParagraphIndex then
      AParagraphIndex := ATableCell.EndParagraphIndex;
    if AParagraphIndex < ATableCell.StartParagraphIndex then
      AParagraphIndex := ATableCell.StartParagraphIndex;
  end;
  CellTree.OnParagraphInserted(AParagraphIndex, ATableCell);
end;

procedure TdxTableCellsManager.OnParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  CellTree.OnParagraphRemove(AParagraphIndex - 1);
end;

procedure TdxTableCellsManager.OnParagraphRemove(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  CellTree.OnParagraphRemove(AParagraphIndex);
end;

procedure TdxTableCellsManager.OnRunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer);
begin
end;

procedure TdxTableCellsManager.OnRunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex;
  ASplitOffset, ATailRunLength: Integer);
begin
end;

procedure TdxTableCellsManager.OnRunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ADeltaRunLength: Integer);
begin
end;

procedure TdxTableCellsManager.OnRunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer);
begin
end;

procedure TdxTableCellsManager.OnRunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ASplitOffset: Integer);
begin
end;

procedure TdxTableCellsManager.OnRunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ADeltaRunLength: Integer);
begin
end;

procedure TdxTableCellsManager.RemoveTable(ATable: TdxTable);
var
  ACell: TdxTableCell;
  ARows: TdxTableRowCollection;
  ARowIndex, ACellIndex: Integer;
  ACells: TdxTableCellCollection;
begin
  ARows := ATable.Rows;
  for ARowIndex := 0 to ARows.Count - 1 do
  begin
    ACells := ARows[ARowIndex].Cells;
    for ACellIndex := 0 to ACells.Count - 1 do
    begin
      ACell := ACells[ACellIndex];
      CellTree.Remove(ACell);
    end;
  end;
  TdxPieceTable(PieceTable).Tables.Remove(ATable.Index);
end;

procedure TdxTableCellsManager.RemoveTableCell(ACell: TdxTableCell);
begin
  CellTree.Remove(ACell);
end;

procedure TdxTableCellsManager.ResetCachedTableLayoutInfo(AFrom, ATo: TdxParagraphIndex);
var
  I: Integer;
  ATables: TdxTableCollection;
begin
  if (AFrom <= 0) and (ATo >= FPieceTable.Paragraphs.Count - 1) then
  begin
    ATables := TdxPieceTable(FPieceTable).Tables;
    for I := 0 to ATables.Count - 1 do
      ATables[I].ResetCachedLayoutInfo;
  end
  else
    CellTree.ResetCachedTableLayoutInfo(AFrom, ATo);
end;

procedure TdxTableCellsManager.RevertParagraphsFromTableCells(ARow: TdxTableRow);
var
  ACell: TdxTableCell;
  ACellIndex: Integer;
  ACells: TdxTableCellCollection;
begin
  ACells := ARow.Cells;
  for ACellIndex := ACells.Count - 1 downto 0 do
  begin
    ACell := ACells[ACellIndex];
    CellTree.Remove(ACell);
    ARow.Cells.ExtractInternal(ACell);
  end;
end;

function TdxTableCellsManager.GetCell(AParagraph: TdxParagraphBase): TdxTableCell;
begin
  Result := CellTree.GetCellByParagraphIndex(AParagraph.Index);
end;

function TdxTableCellsManager.GetCellByNestingLevel(AParagraphIndex: TdxParagraphIndex;
  ANestingLevel: Integer): TdxTableCell;
begin
  Result := CellTree.GetCellByNestingLevel(AParagraphIndex, ANestingLevel);
end;

function TdxTableCellsManager.GetCellsByParagraphIndex(AParagraphIndex: TdxParagraphIndex;
  ANestedLevel: Integer): TdxTableCellList;
begin
  Result := CellTree.GetCellsByParagraphIndex(AParagraphIndex, ANestedLevel);
end;

function TdxTableCellsManager.GetCellSubTree(AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
  AFromNestedLevel: Integer): TdxTableCellNode;
begin
  Result := CellTree.GetCellSubTree(AStartParagraphIndex, AEndParagraphIndex, AFromNestedLevel);
end;

end.
