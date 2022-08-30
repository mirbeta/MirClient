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

unit dxRichEdit.Commands.Delete;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Control.HitTest,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.Selections.Core,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.View.Core,
  dxRichEdit.Commands,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.MultiCommand,
  dxRichEdit.Commands.ChangeProperties;

type
  { TdxDeleteCommandBase }

  TdxDeleteCommandBase = class abstract(TdxRichEditSelectionCommand)
  protected
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;

    procedure DeleteContentCore(ASelectionStart: TdxDocumentLogPosition; ASelectionLength: Integer;
      ADocumentLastParagraphSelected: Boolean); overload; virtual;
    procedure DeleteContentCore(ASelectionStart: TdxDocumentLogPosition; ASelectionLength: Integer;
      ADocumentLastParagraphSelected: Boolean; ABackspacePressed: Boolean); overload; virtual;
    procedure ModifyModel; virtual; abstract;
    function ValidateSelectionRanges(ASorted: TdxSelectionRangeCollection): Boolean; virtual;
  public
    procedure PerformModifyModel; override;
  end;

  { TdxDeleteBackCoreCommand }

  TdxDeleteBackCoreCommand = class(TdxDeleteCommandBase)
  private
    function GetSelectionStartBySelectionRange(ARange: TdxSelectionRange): TdxDocumentModelPosition;
  protected
    function GetExtendSelection: Boolean; override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    procedure ModifyModel; override;

    procedure DeleteBackProcess(const ASelectionStart: TdxDocumentModelPosition; ASelectionStartPos: TdxDocumentLogPosition);
    function IsDeletedNumeration(const ASelectionStart: TdxDocumentModelPosition; AParagraph: TdxParagraph): Boolean; virtual;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDeleteCoreCommand }

  TdxDeleteCoreCommand = class(TdxDeleteCommandBase)
  protected
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function GetExtendSelection: Boolean; override;
    procedure ModifyModel; override;

    function IsResetLastParagraph(ASelectionRange: TdxSelectionRange): Boolean;
    function IsWholeDocumentSelected(ASelectionRange: TdxSelectionRange): Boolean;
    procedure ResetLastParagraphProperties;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDeleteWordCoreCommand }

  TdxDeleteWordCoreCommand = class(TdxDeleteCommandBase)
  protected
    procedure ModifyModel; override;

    function CalculateSelectionEnd(const AStart: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function TryCalculateSelectionEndAssumingSelectionStartAtTheEndOfWord(
      AIterator: TdxWordsDocumentModelIterator; const AStart: TdxDocumentModelPosition): TdxDocumentModelPosition;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDeleteNonEmptySelectionCommand }

  TdxDeleteNonEmptySelectionCommand = class(TdxDeleteCoreCommand)
  strict private
    FRestoreInputPositionFormatting: Boolean;
    FInitialInputPosition: TdxInputPosition;
    FSelectedTable: TdxTable;
    function GetAllowDeleteTableRows: Boolean;
  protected
    function GetSelectedTable: TdxTable;
    procedure AfterUpdate; override;
    procedure BeforeUpdate; override;
    procedure DeleteContentCore(ASelectionStart: TdxDocumentLogPosition; ASelectionLength: Integer;
      ADocumentLastParagraphSelected: Boolean); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property AllowDeleteTableRows: Boolean read GetAllowDeleteTableRows;
  public
    destructor Destroy; override;

    property RestoreInputPositionFormatting: Boolean read FRestoreInputPositionFormatting write FRestoreInputPositionFormatting;
  end;

  { TdxDeleteNonEmptySelectionCommandKeepSingleImage }

  TdxDeleteNonEmptySelectionCommandKeepSingleImage = class(TdxDeleteNonEmptySelectionCommand)
  protected
    function IsSingleImageSelected: Boolean;
  public
    procedure ExecuteCore; override;
  end;

  { TdxDeleteSelectionKeepLastParagraphCommand }

  TdxDeleteSelectionKeepLastParagraphCommand = class(TdxDeleteNonEmptySelectionCommandKeepSingleImage)
  protected
    procedure DeleteContentCore(ASelectionStart: TdxDocumentLogPosition; ASelectionLength: Integer;
      ADocumentLastParagraphSelected: Boolean); override;
  end;

  { TdxBackSpaceKeyCommand }

  TdxDeleteBackCommand = class;

  TdxBackSpaceKeyCommand = class(TdxRichEditCaretBasedCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    function CanDecrementParagraphIndent: Boolean;
    function CreateDeleteBackCommand: TdxDeleteBackCommand; virtual;
    function CreateDecrementParagraphIndentCommand: TdxDecrementParagraphIndentCommand; virtual;
    function IsForbidDeleting: Boolean;
  public
    class function Id: TdxRichEditCommandId; override;
    procedure ExecuteCore; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDeleteBackCommand }

  TdxDeleteBackCommand = class(TdxMultiCommand)
  protected
    procedure CreateCommands; override;
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDeleteCommand }

  TdxDeleteCommand = class(TdxMultiCommand)
  protected
    procedure CreateCommands; override;
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDeleteWordCommand }

  TdxDeleteWordCommand = class(TdxMultiCommand)
  protected
    procedure CreateCommands; override;
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDeleteWordBackCoreCommand }

  TdxDeleteWordBackCoreCommand = class(TdxDeleteCommandBase)
  protected
    procedure ModifyModel; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDeleteWordBackCommand }

  TdxDeleteWordBackCommand = class(TdxMultiCommand)
  protected
    procedure CreateCommands; override;
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDeleteTabAtParagraphCommand }

  TdxDeleteTabAtParagraphCommand = class(TdxRichEditCommand)
  strict private
    FTab: TdxTabInfo;
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    constructor Create(const AControl: IdxRichEditControl; const ATab: TdxTabInfo); reintroduce; virtual;
    procedure ForceExecute(const AState: IdxCommandUIState); override;

    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  Math, Classes, RTLConsts, Contnrs, dxCore,
  dxRichEdit.Commands.Strs,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxCoreClasses;

{ TdxDeleteCommandBase }

function TdxDeleteCommandBase.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := IsContentEditable and CanEditSelection;
end;

function TdxDeleteCommandBase.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;


procedure TdxDeleteCommandBase.DeleteContentCore(ASelectionStart: TdxDocumentLogPosition; ASelectionLength: Integer; ADocumentLastParagraphSelected: Boolean);
begin
  DeleteContentCore(ASelectionStart, ASelectionLength, ADocumentLastParagraphSelected, False);
end;

procedure TdxDeleteCommandBase.DeleteContentCore(ASelectionStart: TdxDocumentLogPosition; ASelectionLength: Integer; ADocumentLastParagraphSelected: Boolean; ABackspacePressed: Boolean);
begin
  if ABackspacePressed then
    ActivePieceTable.DeleteBackContent(ASelectionStart, ASelectionLength, ADocumentLastParagraphSelected)
  else
    ActivePieceTable.DeleteContent(ASelectionStart, ASelectionLength, ADocumentLastParagraphSelected);
  DocumentModel.Selection.Start := ASelectionStart;
  DocumentModel.Selection.&End := ASelectionStart;
end;

function TdxDeleteCommandBase.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxDeleteCommandBase.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxDeleteCommandBase.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxDeleteCommandBase.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

procedure TdxDeleteCommandBase.PerformModifyModel;
begin
  DocumentModel.BeginUpdate;
  try
    ModifyModel;
  finally
    DocumentModel.EndUpdate;
  end;
  ActiveView.EnforceFormattingCompleteForVisibleArea;
end;

function TdxDeleteCommandBase.ValidateSelectionRanges(
  ASorted: TdxSelectionRangeCollection): Boolean;
begin
  Result := True;
end;

{ TdxDeleteBackCoreCommand }

function TdxDeleteBackCoreCommand.CanChangePosition(
  const APos: TdxDocumentModelPosition): Boolean;
var
  ASelection: TdxSelection;
  AStart: TdxDocumentLogPosition;
begin
  ASelection := DocumentModel.Selection;
  if (ASelection.Items.Count = 1) and (ASelection.Items[0].Length = 0) then
  begin
    AStart := Max(ActivePieceTable.DocumentStartLogPosition, ASelection.Items[0].Start - 1);
    Result := IsContentEditable and ActivePieceTable.CanEditRangeLength(AStart, 1);
  end
  else
    Result := inherited CanChangePosition(APos);
end;

procedure TdxDeleteBackCoreCommand.DeleteBackProcess(
  const ASelectionStart: TdxDocumentModelPosition;
  ASelectionStartPos: TdxDocumentLogPosition);
var
  AParagraph: TdxParagraph;
begin
  AParagraph := ActivePieceTable.Paragraphs[ASelectionStart.ParagraphIndex];
  ASelectionStartPos := ASelectionStartPos - 1;
  if ASelectionStartPos >= 0 then
  begin
    if IsDeletedNumeration(ASelectionStart, AParagraph) then
      ActivePieceTable.RemoveNumberingFromParagraph(AParagraph)
    else
      DeleteContentCore(ASelectionStartPos, 1, False);
  end
  else
  begin
    if AParagraph.IsInList then
      ActivePieceTable.RemoveNumberingFromParagraph(AParagraph);
  end;
end;

class function TdxDeleteBackCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteBackCoreDescription);
end;

function TdxDeleteBackCoreCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxDeleteBackCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteBackCoreMenuCaption);
end;

function TdxDeleteBackCoreCommand.GetSelectionStartBySelectionRange(
  ARange: TdxSelectionRange): TdxDocumentModelPosition;
var
  AItems: TdxSelectionItems;
  I: Integer;
  AItem: TdxSelectionItem;
begin
  AItems := DocumentModel.Selection.Items;
  for I := 0 to AItems.Count - 1 do
  begin
    AItem := AItems[I];
    if (AItem.NormalizedStart = ARange.From) and (AItem.Length = ARange.Length) then
      Exit(AItem.Interval.NormalizedStart^);
  end;
  Result := TdxDocumentModelPosition.Null;
end;

function TdxDeleteBackCoreCommand.IsDeletedNumeration(
  const ASelectionStart: TdxDocumentModelPosition; AParagraph: TdxParagraph): Boolean;
begin
  Result := AParagraph.IsInList and (AParagraph.LogPosition = ASelectionStart.LogPosition);
end;

procedure TdxDeleteBackCoreCommand.ModifyModel;
var
  ASelection: TdxSelection;
  ASelectedRows: TdxTableRowList;
  ASelectedTable: TdxTable;
  ASorted: TdxSelectionRangeCollection;
  ASelectionRange: TdxSelectionRange;
  I: Integer;
  ASelectionLength: Integer;
  ASelectionStartPos: TdxDocumentLogPosition;
  ADocumentLastParagraphSelected: Boolean;
  ASelectionStart: TdxDocumentModelPosition;
  ARunIndex: TdxRunIndex;
begin
  ASelection := DocumentModel.Selection;
  ASelectedRows := ASelection.GetSelectedTableRows;
  try
    if ASelectedRows.Count > 0 then
      ASelectedTable := ASelectedRows[0].Table
    else
      ASelectedTable := nil;
  finally
    ASelectedRows.Free;
  end;
  ASorted := ASelection.GetSortedSelectionCollection;
  try
    if not ValidateSelectionRanges(ASorted) then
      Exit;
    for I := ASorted.Count - 1 downto 0 do
    begin
      ASelectionRange := ASorted[I];
      ASelectionStartPos := ASelectionRange.From;
      ASelectionLength := ASelectionRange.Length;
      ADocumentLastParagraphSelected := False;
      if ASelectionStartPos + ASelectionLength > ActivePieceTable.DocumentEndLogPosition then
      begin
        ASelectionLength := ActivePieceTable.DocumentEndLogPosition - ASelectionStartPos;
        ADocumentLastParagraphSelected := True;
      end;
      if ASelectionLength > 0 then
        DeleteContentCore(ASelectionStartPos, ASelectionLength, ADocumentLastParagraphSelected, True)
      else
      begin
        ASelectionStart := GetSelectionStartBySelectionRange(ASelectionRange);
        if ASelectionStart.IsValid then
          DeleteBackProcess(ASelectionStart, ASelectionStartPos);
      end;
    end;
  finally
    ASorted.Free;
  end;
  if (ASelectedTable <> nil) and (ASelectedTable.Rows.Count = 0) then
    ActivePieceTable.DeleteTableFromTableCollection(ASelectedTable);
  ASelection.ClearMultiSelection;
  ARunIndex := ASelection.Interval.NormalizedStart.RunIndex;
  ActivePieceTable.ApplyChangesCore([TdxDocumentModelChangeAction.ValidateSelectionInterval], ARunIndex, ARunIndex);
end;

{ TdxDeleteCoreCommand }

function TdxDeleteCoreCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
var
  ASelection: TdxSelection;
begin
  ASelection := DocumentModel.Selection;
  if (ASelection.Items.Count = 1) and (ASelection.Items[0].Length = 0) then
    Result := IsContentEditable and ActivePieceTable.CanEditRangeLength(ASelection.Items[0].Start, 1)
  else
    Result := inherited CanChangePosition(APos);
end;

class function TdxDeleteCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteDescription);
end;

function TdxDeleteCoreCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

class function TdxDeleteCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteMenuCaption);
end;

function TdxDeleteCoreCommand.IsResetLastParagraph(ASelectionRange: TdxSelectionRange): Boolean;
begin
  Result := IsWholeDocumentSelected(ASelectionRange) or
    ((ASelectionRange.Length = 0) and DocumentModel.ActivePieceTable.IsEmpty);
end;

function TdxDeleteCoreCommand.IsWholeDocumentSelected(ASelectionRange: TdxSelectionRange): Boolean;
var
  AStartLogPosition, AEndLogPosition: TdxDocumentLogPosition;
begin
  AStartLogPosition := ASelectionRange.From;
  AEndLogPosition := ASelectionRange.From + ASelectionRange.Length;
  Result := (AStartLogPosition = ActivePieceTable.DocumentStartLogPosition) and
    (AEndLogPosition = ActivePieceTable.DocumentEndLogPosition + 1);
end;

procedure TdxDeleteCoreCommand.ModifyModel;
var
  ASelection: TdxSelection;
  ASorted: TdxSelectionRangeCollection;
  I: Integer;
  ARunIndex: TdxRunIndex;
  ASelectionRange: TdxSelectionRange;
  ASelectionLength: Integer;
  ASelectionStart: TdxDocumentLogPosition;
  AResetLastParagraphProperties: Boolean;
  ADocumentLastParagraphSelected : Boolean;
begin
  ASelection := DocumentModel.Selection;
  ASorted := ASelection.GetSortedSelectionCollection;
  try
    if not ValidateSelectionRanges(ASorted) then
      Exit;
    for I := ASorted.Count - 1 downto 0 do
    begin
      ASelectionRange := ASorted[I];
      ASelectionLength := Max(1, Abs(ASelectionRange.Length));
      ASelectionStart := ASelectionRange.From;
      AResetLastParagraphProperties := IsResetLastParagraph(ASelectionRange);
      ADocumentLastParagraphSelected := False;
      if ASelectionStart + ASelectionLength > ActivePieceTable.DocumentEndLogPosition then
      begin
        ASelectionLength := ActivePieceTable.DocumentEndLogPosition - ASelectionStart;
        ADocumentLastParagraphSelected := True;
      end;
      if ASelectionLength > 0 then
        DeleteContentCore(ASelectionStart, ASelectionLength, ADocumentLastParagraphSelected);
      if AResetLastParagraphProperties then
        ResetLastParagraphProperties;
    end;
  finally
    ASorted.Free;
  end;
  ASelection.ClearMultiSelection;
  ARunIndex := ASelection.Interval.NormalizedStart.RunIndex;
  ActivePieceTable.ApplyChangesCore([TdxDocumentModelChangeAction.ValidateSelectionInterval], ARunIndex, ARunIndex);
end;

procedure TdxDeleteCoreCommand.ResetLastParagraphProperties;
var
  ARun: TdxTextRunBase;
  AParagraph: TdxParagraph;
  ATabs: TdxTabFormattingInfo;
begin
  ARun := ActivePieceTable.Runs.Last;
  ARun.CharacterStyleIndex := 0;
  ARun.CharacterProperties.Reset;
  AParagraph := ActivePieceTable.Paragraphs.Last;
  AParagraph.ParagraphStyleIndex := 0;
  AParagraph.ParagraphProperties.Reset;
  ATabs := AParagraph.Tabs.GetTabs;
  try
    ATabs.Clear;
    AParagraph.Tabs.SetTabs(ATabs);
    if AParagraph.IsInList then
      ActivePieceTable.RemoveNumberingFromParagraph(AParagraph);
  finally
    ATabs.Free;
  end;
end;

{ TdxDeleteNonEmptySelectionCommand }

destructor TdxDeleteNonEmptySelectionCommand.Destroy;
begin
  FreeAndNil(FInitialInputPosition);
  inherited Destroy;
end;

procedure TdxDeleteNonEmptySelectionCommand.AfterUpdate;
var
  AInputPosition: TdxInputPosition;
begin
  inherited AfterUpdate;

  if (FSelectedTable <> nil) and (FSelectedTable.Rows.Count = 0) then
    ActivePieceTable.DeleteTableFromTableCollection(FSelectedTable);
  DocumentModel.EndUpdate;
  if RestoreInputPositionFormatting and (FInitialInputPosition <> nil) then
  begin
    AInputPosition := CaretPosition.GetInputPosition;
    AInputPosition.CopyFormattingFrom(FInitialInputPosition);
  end;
end;

function TdxDeleteNonEmptySelectionCommand.GetAllowDeleteTableRows: Boolean;
begin
  Result := FSelectedTable <> nil;
end;

procedure TdxDeleteNonEmptySelectionCommand.BeforeUpdate;
var
  APosition: TdxDocumentLogPosition;
begin
  DocumentModel.BeginUpdate;
  inherited BeforeUpdate;
  if FRestoreInputPositionFormatting then
  begin
    APosition := Min(DocumentModel.Selection.NormalizedStart + 1, ActivePieceTable.DocumentEndLogPosition);
    FInitialInputPosition := CaretPosition.CreateInputPosition(APosition);
  end;
  FSelectedTable := GetSelectedTable;
end;

function TdxDeleteNonEmptySelectionCommand.GetSelectedTable: TdxTable;
var
  ASelection: TdxSelection;
  ASelectedCells: TdxSelectedCellsCollection;
  AFirstCell, ALastCell: TdxTableCell;
  ATable: TdxTable;
begin
  ASelection := DocumentModel.Selection;
  ASelectedCells := Safe<TdxSelectedCellsCollection>.Cast(ASelection.SelectedCells);
  if (ASelectedCells = nil) or not ASelectedCells.IsNotEmpty then
    Exit(nil);

  AFirstCell := ASelectedCells.NormalizedFirst.StartCell;
  ALastCell := ASelectedCells.NormalizedLast.EndCell;
  if (AFirstCell = nil) or (ALastCell = nil) or (AFirstCell.Table <> ALastCell.Table) then
    Exit(nil);

  ATable := AFirstCell.Table;
  if (AFirstCell = ATable.FirstRow.FirstCell) and (ALastCell = ATable.LastRow.LastCell) then
    Exit(ATable);
  Result := nil;
end;

procedure TdxDeleteNonEmptySelectionCommand.DeleteContentCore(ASelectionStart: TdxDocumentLogPosition;
  ASelectionLength: Integer; ADocumentLastParagraphSelected: Boolean);
begin
  DeleteContentCore(ASelectionStart, ASelectionLength, ADocumentLastParagraphSelected, AllowDeleteTableRows);
end;

procedure TdxDeleteNonEmptySelectionCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable and (DocumentModel.Selection.Length > 0);
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxDeleteNonEmptySelectionCommandKeepSingleImage }

procedure TdxDeleteNonEmptySelectionCommandKeepSingleImage.ExecuteCore;
var
  AState: IdxCommandUIState;
  ACommand: TdxShrinkSelectionToStartCommand;
begin
  if IsSingleImageSelected then
  begin
    ACommand := TdxShrinkSelectionToStartCommand.Create(RichEditControl);
    try
      AState := ACommand.CreateDefaultCommandUIState;
      try
        ACommand.UpdateUIState(AState);
        if AState.Enabled then
          ACommand.ExecuteCore;
      finally
        AState := nil;
      end;
    finally
      ACommand.Free;
    end;
  end
  else
    inherited ExecuteCore;
end;

function TdxDeleteNonEmptySelectionCommandKeepSingleImage.IsSingleImageSelected: Boolean;
var
  ARunInfo: TdxRunInfo;
  ATextRun: TdxTextRunBase;
  APieceTable: TdxPieceTable;
  AAnchorRun: TdxFloatingObjectAnchorRun;
begin
  if DocumentModel.Selection.Length <> 1 then
    Exit(False);
  APieceTable := DocumentModel.Selection.PieceTable;
  ARunInfo := TdxRunInfo.Create(APieceTable);
  try
    APieceTable.CalculateRunInfoStart(DocumentModel.Selection.Start, ARunInfo);
    ATextRun := APieceTable.Runs[ARunInfo.Start.RunIndex];
  finally
    ARunInfo.Free;
  end;
  if ATextRun is TdxInlinePictureRun then
    Exit(True);
  AAnchorRun := Safe<TdxFloatingObjectAnchorRun>.Cast(ATextRun);
  if (AAnchorRun <> nil) and (AAnchorRun.PictureContent <> nil) then
    Result := True
  else
    Result := False;
end;

{ TdxDeleteSelectionKeepLastParagraphCommand }

procedure TdxDeleteSelectionKeepLastParagraphCommand.DeleteContentCore(
  ASelectionStart, ASelectionLength: Integer; ADocumentLastParagraphSelected: Boolean);
var
  APieceTable: TdxPieceTable;
  AEndSelectionLogPosition: TdxDocumentLogPosition;
  AParagraphIndex: TdxParagraphIndex;
  AParagraph: TdxParagraph;
  ACell: TdxTableCell;
begin
  APieceTable := ActivePieceTable;
  AEndSelectionLogPosition := ASelectionStart + ASelectionLength - 1;
  AParagraphIndex := APieceTable.FindParagraphIndex(AEndSelectionLogPosition);
  AParagraph := APieceTable.Paragraphs[AParagraphIndex];
  ACell := AParagraph.GetCell;
  if (AParagraph.EndLogPosition = AEndSelectionLogPosition) and (ACell = nil) then
    if not ADocumentLastParagraphSelected or not APieceTable.Paragraphs.Last.IsEmpty then
      ASelectionLength := Max(0, ASelectionLength - 1);
  inherited DeleteContentCore(ASelectionStart, ASelectionLength, ADocumentLastParagraphSelected);
end;

{ TdxBackSpaceKeyCommand }

function TdxBackSpaceKeyCommand.CanDecrementParagraphIndent: Boolean;
var
  ASelection: TdxSelection;
  AParagraphIndex: TdxParagraphIndex;
  AParagraph: TdxParagraph;
begin
  ASelection := DocumentModel.Selection;
  AParagraphIndex := ASelection.Interval.NormalizedStart.ParagraphIndex;
  AParagraph := ActivePieceTable.Paragraphs[AParagraphIndex];
  Result := not AParagraph.IsInList and ((AParagraph.LeftIndent <> 0) or (AParagraph.FirstLineIndent <> 0));
end;

function TdxBackSpaceKeyCommand.CreateDecrementParagraphIndentCommand: TdxDecrementParagraphIndentCommand;
begin
  Result := TdxDecrementParagraphIndentCommand.Create(RichEditControl);
end;

function TdxBackSpaceKeyCommand.CreateDeleteBackCommand: TdxDeleteBackCommand;
begin
  Result := TdxDeleteBackCommand.Create(RichEditControl);
end;

procedure TdxBackSpaceKeyCommand.ExecuteCore;
var
  ADeleteBackCommand: TdxDeleteBackCommand;
  ADecrementIndentCommand: TdxDecrementIndentCommand;
  ACommand: TdxDecrementParagraphIndentCommand;
begin
  ADecrementIndentCommand := TdxDecrementIndentCommand.Create(RichEditControl);
  try
    if ADecrementIndentCommand.SelectionBeginFirstRowStartPos and CanDecrementParagraphIndent then
    begin
      ACommand := CreateDecrementParagraphIndentCommand;
      try
        ACommand.ForceExecute(CreateDefaultCommandUIState);
      finally
        ACommand.Free;
      end;
      Exit;
    end;
  finally
    ADecrementIndentCommand.Free;
  end;
  if IsForbidDeleting then
    Exit;
  ADeleteBackCommand := CreateDeleteBackCommand;
  try
    ADeleteBackCommand.ForceExecute(CreateDefaultCommandUIState);
  finally
    ADeleteBackCommand.Free;
  end;
end;

class function TdxBackSpaceKeyCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandBackSpaceKeyDescription);
end;

class function TdxBackSpaceKeyCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandBackSpaceKeyMenuCaption);
end;

class function TdxBackSpaceKeyCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.BackSpaceKey;
end;

function TdxBackSpaceKeyCommand.IsForbidDeleting: Boolean;
var
  ASelection: TdxSelection;
  ACell, APreviousCell: TdxTableCell;
  APreviousPosition: TdxDocumentLogPosition;
begin
  ASelection := DocumentModel.Selection;
  if ASelection.Length > 0 then
    Exit(False);
  ACell := ActivePieceTable.FindParagraph(ASelection.Start).GetCell;
  Result := (ACell <> nil) and
    (ASelection.Start = ActivePieceTable.Paragraphs[ACell.StartParagraphIndex].LogPosition);
  if not Result then
  begin
    APreviousPosition := Max(0, ASelection.Start - 1);
    APreviousCell := ActivePieceTable.FindParagraph(APreviousPosition).GetCell;
    Result := ACell <> APreviousCell;
  end;
end;

procedure TdxBackSpaceKeyCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
end;

{ TdxDeleteBackCommand }

procedure TdxDeleteBackCommand.CreateCommands;
begin
  Commands.Add(TdxSelectFieldPrevToCaretCommand.Create(RichEditControl));
  Commands.Add(TdxDeleteBackCoreCommand.Create(RichEditControl));
end;

function TdxDeleteBackCommand.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := TdxMultiCommandExecutionMode.ExecuteFirstAvailable;
end;

class function TdxDeleteBackCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteBackDescription);
end;

class function TdxDeleteBackCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteBackMenuCaption);
end;

function TdxDeleteBackCommand.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := TdxMultiCommandUpdateUIStateMode.EnableIfAnyAvailable;
end;

{ TdxDeleteWordCoreCommand }

function TdxDeleteWordCoreCommand.CalculateSelectionEnd(const AStart: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  AIterator: TdxWordsDocumentModelIterator;
begin
  AIterator := TdxWordsDocumentModelIterator.Create(ActivePieceTable);
  try
    Result := TryCalculateSelectionEndAssumingSelectionStartAtTheEndOfWord(AIterator, AStart);
    if not Result.IsValid then
      Result := AIterator.MoveForward(AStart);
  finally
    AIterator.Free;
  end;
end;

class function TdxDeleteWordCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteWordCoreDescription);
end;

class function TdxDeleteWordCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteWordCoreMenuCaption);
end;

procedure TdxDeleteWordCoreCommand.ModifyModel;
var
  AStart, AEnd: TdxDocumentModelPosition;
  ALength: Integer;
begin
  AStart := DocumentModel.Selection.Interval.NormalizedStart^;
  AEnd := CalculateSelectionEnd(AStart);
  ALength := AEnd.LogPosition - AStart.LogPosition;
  if ALength > 0 then
  begin
    if IsContentEditable and ActivePieceTable.CanEditRange(AStart.LogPosition, AEnd.LogPosition) then
      DeleteContentCore(AStart.LogPosition, ALength, False);
  end;
end;

function TdxDeleteWordCoreCommand.TryCalculateSelectionEndAssumingSelectionStartAtTheEndOfWord(
  AIterator: TdxWordsDocumentModelIterator;
  const AStart: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  ACharacterIterator: TdxCharactersDocumentModelIterator;
  APreviousChar: TdxDocumentModelPosition;
begin
  if AIterator.IsStartOfDocument(AStart) or AIterator.IsEndOfDocument(AStart) then
    Exit(TdxDocumentModelPosition.Null);
  if AIterator.IsInsideWord(AStart) then
  begin
    ACharacterIterator := TdxCharactersDocumentModelIterator.Create(ActivePieceTable);
    try
      Result := AStart;
      AIterator.SkipForward(ACharacterIterator, Result, AIterator.IsNotNonWordsSymbols);
    finally
      ACharacterIterator.Free;
    end;
  end
  else
  begin
    if not AIterator.IsSpace(AIterator.GetCharacter(AStart)) then
      Exit(TdxDocumentModelPosition.Null);
    ACharacterIterator := TdxCharactersDocumentModelIterator.Create(ActivePieceTable);
    try
      APreviousChar := ACharacterIterator.MoveBack(AStart);
      if not AIterator.IsNotNonWordsSymbols(AIterator.GetCharacter(APreviousChar)) then
        Exit(TdxDocumentModelPosition.Null);
      Result := AStart;
      AIterator.SkipForward(ACharacterIterator, Result, AIterator.IsSpace);
      AIterator.SkipForward(ACharacterIterator, Result, AIterator.IsNotNonWordsSymbols);
    finally
      ACharacterIterator.Free;
    end;
  end;
end;

{ TdxDeleteWordBackCoreCommand }

class function TdxDeleteWordBackCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteWordBackCoreDescription);
end;

class function TdxDeleteWordBackCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteWordBackCoreMenuCaption);
end;

procedure TdxDeleteWordBackCoreCommand.ModifyModel;
var
  AStart, ANewStart: TdxDocumentModelPosition;
  AIterator: TdxWordsDocumentModelIterator;
  ALength: Integer;
begin
  AStart := DocumentModel.Selection.Interval.NormalizedStart^;
  AIterator := TdxWordsDocumentModelIterator.Create(ActivePieceTable);
  try
    ANewStart := AIterator.MoveBack(AStart);
    ALength := AStart.LogPosition - ANewStart.LogPosition;
    if ALength > 0 then
    begin
      if IsContentEditable and ActivePieceTable.CanEditRange(ANewStart.LogPosition, AStart.LogPosition) then
        DeleteContentCore(ANewStart.LogPosition, ALength, False);
    end;
  finally
    AIterator.Free;
  end;
end;

{ TdxDeleteWordBackCommand }

procedure TdxDeleteWordBackCommand.CreateCommands;
begin
  Commands.Add(TdxSelectFieldPrevToCaretCommand.Create(RichEditControl));
  Commands.Add(TdxDeleteWordBackCoreCommand.Create(RichEditControl));
end;

function TdxDeleteWordBackCommand.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := TdxMultiCommandExecutionMode.ExecuteFirstAvailable;
end;

class function TdxDeleteWordBackCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteWordBackDescription);
end;

class function TdxDeleteWordBackCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteWordBackMenuCaption);
end;

class function TdxDeleteWordBackCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DeleteWordBack;
end;

function TdxDeleteWordBackCommand.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := TdxMultiCommandUpdateUIStateMode.EnableIfAnyAvailable;
end;

{ TdxDeleteCommand }

procedure TdxDeleteCommand.CreateCommands;
begin
  Commands.Add(TdxSelectFieldNextToCaretCommand.Create(RichEditControl));
  Commands.Add(TdxDeleteCoreCommand.Create(RichEditControl));
end;

function TdxDeleteCommand.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := TdxMultiCommandExecutionMode.ExecuteFirstAvailable;
end;

class function TdxDeleteCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteDescription);
end;

class function TdxDeleteCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteMenuCaption);
end;

class function TdxDeleteCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.Delete;
end;

function TdxDeleteCommand.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := TdxMultiCommandUpdateUIStateMode.EnableIfAnyAvailable;
end;

{ TdxDeleteWordCommand }

procedure TdxDeleteWordCommand.CreateCommands;
begin
  Commands.Add(TdxSelectFieldNextToCaretCommand.Create(RichEditControl));
  Commands.Add(TdxDeleteWordCoreCommand.Create(RichEditControl));
end;

function TdxDeleteWordCommand.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := TdxMultiCommandExecutionMode.ExecuteFirstAvailable;
end;

class function TdxDeleteWordCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteWordDescription);
end;

class function TdxDeleteWordCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteWordMenuCaption);
end;

class function TdxDeleteWordCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DeleteWord;
end;

function TdxDeleteWordCommand.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := TdxMultiCommandUpdateUIStateMode.EnableIfAnyAvailable;
end;

{ TdxDeleteTabAtParagraphCommand }

constructor TdxDeleteTabAtParagraphCommand.Create(const AControl: IdxRichEditControl; const ATab: TdxTabInfo);
begin
  inherited Create(AControl);
  FTab := ATab;
end;

class function TdxDeleteTabAtParagraphCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTabToParagraphMenuCaption);
end;

class function TdxDeleteTabAtParagraphCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTabToParagraphDescription);
end;

procedure TdxDeleteTabAtParagraphCommand.ForceExecute(const AState: IdxCommandUIState);
var
  ASelection: TdxSelection;
  AParagraphs: TdxParagraphList;
  AParagraph: TdxParagraph;
  ATabs, AParagraphTabs, AStyleTabs: TdxTabFormattingInfo;
  I: Integer;
begin
  ASelection := DocumentModel.Selection;
  AParagraphs := ASelection.GetSelectedParagraphs;
  try
  for I := 0 to AParagraphs.Count - 1 do
  begin
    AParagraph := AParagraphs[I];
    ATabs := AParagraph.GetTabs;
    try
      if ATabs.Contains(FTab) then
      begin
        AParagraphTabs := AParagraph.Tabs.GetTabs;
        try
          AParagraphTabs.Remove(FTab);
          AStyleTabs := AParagraph.ParagraphStyle.GetTabs;
          try
            if AStyleTabs.Contains(FTab) then
            begin
              FTab := TdxTabInfo.Create(FTab.Position, FTab.Alignment, FTab.Leader, True, False);
              AParagraphTabs.Add(FTab);
            end;
          finally
            AStyleTabs.Free;
          end;
          AParagraph.Tabs.SetTabs(AParagraphTabs);
        finally
          AParagraphTabs.Free;
        end;
      end;
    finally
      ATabs.Free;
    end;
  end;
  finally
    AParagraphs.Free;
  end;
end;

procedure TdxDeleteTabAtParagraphCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.ParagraphTabs);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

end.
