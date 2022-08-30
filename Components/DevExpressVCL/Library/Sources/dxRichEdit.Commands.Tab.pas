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

unit dxRichEdit.Commands.Tab;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Generics.Defaults, Generics.Collections,
  dxRichEdit.Utils.Types,
  dxRichEdit.View.Core,
  dxRichEdit.Commands,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.Keyboard,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Core;

type
  { TdxTabKeyCommand }

  TdxTabKeyCommand = class(TdxRichEditCaretBasedCommand)
  private
    function GetCurrentCell(ACell: TdxTableCell): TdxTableCell;
    function GetNextUnmergedCellFromCoveredCell(ACell: TdxTableCell): TdxTableCell;
    function GetStartLogPosition: TdxDocumentLogPosition;
    function GetEndLogPosition: TdxDocumentLogPosition;
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    procedure SelectEntireCell(ACell: TdxTableCell; AShouldUpdateCaretY: Boolean); virtual;
    procedure AppendTableRowAtBottom(ATable: TdxTable); virtual;

    procedure ChangeIndent; virtual;
    function CreateIncrementIndentByTheTabCommand: TdxIncrementIndentByTheTabCommand; virtual;
    function CreateInsertTabCommand: TdxInsertTabCommand; virtual;
    function GetNextCell(ACurrentCell: TdxTableCell): TdxTableCell; virtual;
    function IsSelectedOneParagraphWithTab: Boolean;
    function IsSelectionStartFromBeginRow: Boolean;
    function IsLastCellInDirection(ACell: TdxTableCell): Boolean; virtual;
    procedure ModifyCaretPositionCellTopAnchor; virtual;
    procedure ProcessSingleCell(ACell: TdxTableCell); virtual;
    function SelectedWholeRow: Boolean;
    function ShouldAppendTableRowAtBottom(ACurrentCell: TdxTableCell): Boolean; virtual;

    property EndLogPosition: TdxDocumentLogPosition read GetEndLogPosition;
    property StartLogPosition: TdxDocumentLogPosition read GetStartLogPosition;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxShiftTabKeyCommand }

  TdxShiftTabKeyCommand = class(TdxTabKeyCommand)
  protected
    procedure ChangeIndent; override;

    function CreateDecrementIndentByTheTabCommand: TdxDecrementIndentByTheTabCommand; virtual;
    function GetNextCell(ACurrentCell: TdxTableCell): TdxTableCell; override;
    function IsLastCellInDirection(ACell: TdxTableCell): Boolean; override;
    procedure ModifyCaretPositionCellTopAnchor; override;
    procedure ProcessSingleCell(ACell: TdxTableCell); override;
    function ShouldAppendTableRowAtBottom(ACurrentCell: TdxTableCell): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  RTLConsts, dxCore, dxCoreClasses,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.Commands.Tables,
  dxRichEdit.Commands.Strs,
  dxRichEdit.DocumentModel.Selection;

{ TdxTabKeyCommand }

procedure TdxTabKeyCommand.AppendTableRowAtBottom(ATable: TdxTable);
var
  ACommand: TdxInsertTableRowBelowCoreCommand;
  AState: IdxCommandUIState;
begin
  ACommand := TdxInsertTableRowBelowCoreCommand.Create(RichEditControl);
  try
    ACommand.Row := ATable.Rows.Last;
    AState := ACommand.CreateDefaultCommandUIState;
    ACommand.UpdateUIState(AState);
    if AState.Enabled and AState.Visible then
      ACommand.ForceExecute(AState);
  finally
    ACommand.Free;
  end;
end;

procedure TdxTabKeyCommand.ChangeIndent;
var
  AOldInputPosition: TdxInputPosition;
  AIncrementCommand: TdxIncrementIndentByTheTabCommand;
  AState: IdxCommandUIState;
  ANewInputPosition: TdxInputPosition;
begin
  if DocumentModel.Selection.Length = 0 then
    AOldInputPosition := CaretPosition.GetInputPosition.Clone
  else
    AOldInputPosition := nil;
  try
    AIncrementCommand := CreateIncrementIndentByTheTabCommand;
    try
      AState := CreateDefaultCommandUIState;
      AIncrementCommand.ForceExecute(AState);
      if AOldInputPosition <> nil then
      begin
        UpdateCaretPosition(TdxDocumentLayoutDetailsLevel.Character);
        ANewInputPosition := CaretPosition.GetInputPosition;
        ANewInputPosition.CopyFormattingFrom(AOldInputPosition);
      end;
    finally
      AIncrementCommand.Free;
    end;
  finally
    AOldInputPosition.Free;
  end;
end;

function TdxTabKeyCommand.CreateIncrementIndentByTheTabCommand: TdxIncrementIndentByTheTabCommand;
begin
  Result := TdxIncrementIndentByTheTabCommand.Create(RichEditControl);
end;

function TdxTabKeyCommand.CreateInsertTabCommand: TdxInsertTabCommand;
begin
  Result := TdxInsertTabCommand.Create(RichEditControl);
end;

procedure TdxTabKeyCommand.ExecuteCore;
var
  ASelection: TdxSelection;
  ACells: TdxSelectedCellsCollection;
  AInsertCommand: TdxInsertTabCommand;
  AState: IdxCommandUIState;
begin
  ASelection := DocumentModel.Selection;
  if ASelection.IsWholeSelectionInOneTable then
  begin
    ACells := TdxSelectedCellsCollection(ASelection.SelectedCells);
    ProcessSingleCell(ACells.TopLeftCell);
    ASelection.ClearMultiSelection;
    ASelection.SetStartCell(ASelection.NormalizedStart);
  end
  else
  begin
    if IsSelectionStartFromBeginRow and SelectedWholeRow and
      not IsSelectedOneParagraphWithTab and DocumentModel.DocumentCapabilities.ParagraphFormattingAllowed then
        ChangeIndent
    else
    begin
      AInsertCommand := CreateInsertTabCommand;
      try
        AState := CreateDefaultCommandUIState;
        AInsertCommand.ForceExecute(AState);
      finally
        AInsertCommand.Free;
      end;
    end;
  end;
end;

function TdxTabKeyCommand.GetCurrentCell(ACell: TdxTableCell): TdxTableCell;
var
  AAnchors: TdxTableCellVerticalAnchorCollection;
  AItems: TdxTableCellVerticalAnchorList;
  AIndex: Integer;
  ATargetRow: TdxTableRow;
  AStartColumnIndex: Integer;
  ACellIndexInTargetRow: Integer;
begin
  if IsLastCellInDirection(ACell) and (ACell.VerticalMerging = TdxMergingState.Restart) then
  begin
    AAnchors := CaretPosition.LayoutPosition.TableCell.TableViewInfo.Anchors;
    AItems := AAnchors.Items;
    AIndex := CaretPosition.TableCellTopAnchorIndex;
    ATargetRow := ACell.Row;
    if AIndex >= 0 then
      ATargetRow := ACell.Table.Rows[AIndex]
    else
    begin
      AIndex := -AIndex;
      if AIndex >= AItems.Count then
        ATargetRow := ACell.Table.LastRow;
    end;
    AStartColumnIndex := ACell.GetStartColumnIndexConsiderRowGrid;
    ACellIndexInTargetRow := ACell.Table.GetAbsoluteCellIndexInRow(ATargetRow, AStartColumnIndex, False);
    Result := ATargetRow.Cells[ACellIndexInTargetRow];
  end
  else
    Result := ACell;
end;

class function TdxTabKeyCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandTabKeyDescription);
end;

function TdxTabKeyCommand.GetEndLogPosition: TdxDocumentLogPosition;
begin
  Result := DocumentModel.Selection.Interval.NormalizedEnd.LogPosition;
end;

class function TdxTabKeyCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandTabKeyMenuCaption);
end;

function TdxTabKeyCommand.GetNextCell(ACurrentCell: TdxTableCell): TdxTableCell;
begin
  Result := ACurrentCell.Next;
end;

function TdxTabKeyCommand.GetNextUnmergedCellFromCoveredCell(
  ACell: TdxTableCell): TdxTableCell;
var
  AAnchors: TdxTableCellVerticalAnchorCollection;
  AItems: TdxTableCellVerticalAnchorList;
  AIndex: Integer;
  ATargetRow: TdxTableRow;
  AStartColumnIndex: Integer;
  ACellIndexInTargetRow: Integer;
begin
  AAnchors := CaretPosition.LayoutPosition.TableCell.TableViewInfo.Anchors;
  AItems := AAnchors.Items;
  AIndex := CaretPosition.TableCellTopAnchorIndex;
  ATargetRow := ACell.Row;
  if AIndex >= ACell.Table.Rows.Count then
    ATargetRow := ACell.Table.Rows.Last
  else
    if AIndex >= 0 then
      ATargetRow := ACell.Table.Rows[AIndex]
    else
    begin
      AIndex := -AIndex;
      if AIndex >= AItems.Count then
        ATargetRow := ACell.Table.LastRow;
    end;
  AStartColumnIndex := ACell.GetStartColumnIndexConsiderRowGrid;
  ACellIndexInTargetRow := ACell.Table.GetAbsoluteCellIndexInRow(ATargetRow, AStartColumnIndex, False);
  Result := ATargetRow.Cells[ACellIndexInTargetRow];
end;

function TdxTabKeyCommand.GetStartLogPosition: TdxDocumentLogPosition;
begin
  Result := DocumentModel.Selection.Interval.NormalizedStart.LogPosition;
end;

class function TdxTabKeyCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.TabKey;
end;

function TdxTabKeyCommand.IsLastCellInDirection(ACell: TdxTableCell): Boolean;
begin
  Result := ACell.IsLastCellInRow;
end;

function TdxTabKeyCommand.IsSelectedOneParagraphWithTab: Boolean;
var
  AStartParagraphIndex: TdxParagraphIndex;
  AEndParagraphIndex: TdxParagraphIndex;
  AStartParagraph: TdxParagraph;
begin
  AStartParagraphIndex := DocumentModel.Selection.Interval.Start.ParagraphIndex;
  AEndParagraphIndex := DocumentModel.Selection.Interval.&End.ParagraphIndex;
  AStartParagraph := ActivePieceTable.Paragraphs[AStartParagraphIndex];
  Result := (AStartParagraphIndex = AEndParagraphIndex) and
    (AStartParagraph.Tabs.Info.Count > 0) and not AStartParagraph.IsInList;
end;

function TdxTabKeyCommand.IsSelectionStartFromBeginRow: Boolean;
var
  ASelectionLayout: TdxSelectionLayout;
begin
  ASelectionLayout := ActiveView.SelectionLayout;
  Result := ASelectionLayout.IsSelectionStartFromBeginRow;
end;

procedure TdxTabKeyCommand.ModifyCaretPositionCellTopAnchor;
begin
  CaretPosition.TableCellTopAnchorIndex := CaretPosition.TableCellTopAnchorIndex + 1;
end;

procedure TdxTabKeyCommand.ProcessSingleCell(ACell: TdxTableCell);
var
  ACurrentCell: TdxTableCell;
  ANextCellInNewRow: Boolean;
  ANextCell: TdxTableCell;
begin
  ACurrentCell := GetCurrentCell(ACell);
  if ShouldAppendTableRowAtBottom(ACurrentCell) then
    AppendTableRowAtBottom(ACurrentCell.Table)
  else
  begin
    ANextCell := GetNextCell(ACurrentCell);
    if ANextCell = nil then
      Exit;
    ANextCellInNewRow := ACurrentCell.RowIndex <> ANextCell.RowIndex;

    if (ACurrentCell.VerticalMerging = TdxMergingState.Continue) or
      (ACurrentCell.VerticalMerging = TdxMergingState.Restart) and
      (ANextCell.VerticalMerging = TdxMergingState.None) then
    begin
      ANextCell := GetNextUnmergedCellFromCoveredCell(ACurrentCell);
      ANextCell := GetNextCell(ANextCell);
      if ANextCell = nil then
        Exit;
    end;
    if ANextCell.VerticalMerging = TdxMergingState.Continue then
    begin
      ANextCell := ANextCell.Table.GetFirstCellInVerticalMergingGroup(ANextCell);
      if ANextCellInNewRow and ANextCell.IsFirstCellInRow then
      begin
        ModifyCaretPositionCellTopAnchor;
        ANextCellInNewRow := False;
      end;
    end;
    if ANextCellInNewRow and ACurrentCell.IsFirstCellInRow then
    begin
      ModifyCaretPositionCellTopAnchor;
      ANextCellInNewRow := False;
    end;
    SelectEntireCell(ANextCell, ANextCellInNewRow);
  end;
end;

function TdxTabKeyCommand.SelectedWholeRow: Boolean;
var
  ASelectionLayout: TdxSelectionLayout;
  AStartParagraph: TdxParagraph;
  AEqualsStartAndEndRow: Boolean;
  ACharacter: TdxCharacterBox;
  ASelectedUpToEndRow: Boolean;
begin
  ASelectionLayout := ActiveView.SelectionLayout;
  AStartParagraph := ActivePieceTable.Paragraphs[DocumentModel.Selection.Interval.Start.ParagraphIndex];
  if DocumentModel.Selection.Length = 0 then
    Result := (AStartParagraph.Length > 1) or AStartParagraph.IsInList
  else
  begin
    AEqualsStartAndEndRow := ASelectionLayout.StartLayoutPosition.Row.Equals(ASelectionLayout.EndLayoutPosition.Row);
    ACharacter := ASelectionLayout.EndLayoutPosition.Character;
    ASelectedUpToEndRow := (ACharacter <> nil) and
      (ACharacter.EndPos = ASelectionLayout.EndLayoutPosition.Row.Boxes.Last.EndPos);
    Result := not AEqualsStartAndEndRow or ASelectedUpToEndRow;
  end;
end;

procedure TdxTabKeyCommand.SelectEntireCell(ACell: TdxTableCell;
  AShouldUpdateCaretY: Boolean);
var
  ACommand: TdxSelectTableCellCommand;
  AState: IdxCommandUIState;
begin
  ACommand := TdxSelectTableCellCommand.Create(RichEditControl);
  try
    ACommand.Cell := ACell;
    ACommand.ShouldUpdateCaretTableAnchorVerticalPositionY := AShouldUpdateCaretY;
    AState := ACommand.CreateDefaultCommandUIState;
    ACommand.ForceExecute(AState);
  finally
    ACommand.Free;
  end;
end;

function TdxTabKeyCommand.ShouldAppendTableRowAtBottom(
  ACurrentCell: TdxTableCell): Boolean;
begin
  Result := ACurrentCell.IsLastCellInTable;
end;

procedure TdxTabKeyCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
end;

{ TdxShiftTabKeyCommand }

procedure TdxShiftTabKeyCommand.ChangeIndent;
var
  ADecrementCommand: TdxDecrementIndentByTheTabCommand;
begin
  ADecrementCommand := CreateDecrementIndentByTheTabCommand;
  try
    ADecrementCommand.ForceExecute(CreateDefaultCommandUIState);
  finally
    ADecrementCommand.Free;
  end;
end;

function TdxShiftTabKeyCommand.CreateDecrementIndentByTheTabCommand: TdxDecrementIndentByTheTabCommand;
begin
  Result := TdxDecrementIndentByTheTabCommand.Create(RichEditControl);
end;

class function TdxShiftTabKeyCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandTabKeyDescription);
end;

class function TdxShiftTabKeyCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandTabKeyMenuCaption);
end;

function TdxShiftTabKeyCommand.GetNextCell(
  ACurrentCell: TdxTableCell): TdxTableCell;
begin
  Result := ACurrentCell.Previous;
end;

class function TdxShiftTabKeyCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShiftTabKey;
end;

function TdxShiftTabKeyCommand.IsLastCellInDirection(
  ACell: TdxTableCell): Boolean;
begin
  Result := ACell.IsFirstCellInRow;
end;

procedure TdxShiftTabKeyCommand.ModifyCaretPositionCellTopAnchor;
begin
  CaretPosition.TableCellTopAnchorIndex := CaretPosition.TableCellTopAnchorIndex - 1;
end;

procedure TdxShiftTabKeyCommand.ProcessSingleCell(ACell: TdxTableCell);
begin
  if ACell.IsFirstCellInTable and (CaretPosition.TableCellTopAnchorIndex = 0) then
    Exit;
  inherited ProcessSingleCell(ACell);
end;

function TdxShiftTabKeyCommand.ShouldAppendTableRowAtBottom(
  ACurrentCell: TdxTableCell): Boolean;
begin
  Result := False;
end;

end.
