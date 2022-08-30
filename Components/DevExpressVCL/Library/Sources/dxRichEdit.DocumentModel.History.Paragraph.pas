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

unit dxRichEdit.DocumentModel.History.Paragraph;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Paragraphs,
  dxRichEdit.DocumentModel.History.Simple,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.SectionRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Notes,
  dxGenerics;

type
  { TdxFieldSymbolResultInsertedHistoryItem }

  TdxFieldSymbolResultInsertedHistoryItem = class(TdxTextRunInsertedHistoryItem)
  protected
    function CreateTextRun(AParagraph: TdxParagraphBase): TdxTextRunBase; override;
  end;

  { TdxLayoutDependentTextRunInsertedHistoryItem }

  TdxLayoutDependentTextRunInsertedHistoryItem = class(TdxTextRunInsertedHistoryItem)
  private
    FFieldResultFormatting: TdxFieldResultFormatting;
    function GetDocumentModel: TdxDocumentModel;
    procedure SetFieldResultFormatting(const Value: TdxFieldResultFormatting);
  protected
    function CreateTextRun(AParagraph: TdxParagraphBase): TdxTextRunBase; override;
    function CreateTextRunCore(AParagraph: TdxParagraphBase): TdxTextRunBase; virtual;
  public
    destructor Destroy; override;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property FieldResultFormatting: TdxFieldResultFormatting read FFieldResultFormatting write SetFieldResultFormatting;
  end;

  { TdxFloatingObjectAnchorRunInsertedHistoryItem }

  TdxFloatingObjectAnchorRunInsertedHistoryItem = class(TdxTextRunInsertedBaseHistoryItem)
  private
    FNewRun: TdxFloatingObjectAnchorRun;
    FNotificationId: Integer;
    FOwnsNewRun: Boolean;
    procedure AfterInsertRun(ADocumentModel: TdxDocumentModel; ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    destructor Destroy; override;
    procedure Execute; override;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxFootNoteRunInsertedHistoryItemBase }

  TdxFootNoteRunInsertedHistoryItemBase = class abstract(TdxLayoutDependentTextRunInsertedHistoryItem)
  strict private
    FNoteIndex: Integer;
  protected
    procedure UndoCore; override;
    function CreateTextRun(AParagraph: TdxParagraphBase): TdxTextRunBase; override;
    function GetNote(ANoteIndex: Integer): TdxFootNoteBase; virtual; abstract;
  public
    property NoteIndex: Integer read FNoteIndex write FNoteIndex;
  end;

  { TdxFootNoteRunInsertedHistoryItem }

  TdxFootNoteRunInsertedHistoryItem = class(TdxFootNoteRunInsertedHistoryItemBase)
  protected
    function CreateTextRunCore(AParagraph: TdxParagraphBase): TdxTextRunBase; override;
    function GetNote(ANoteIndex: Integer): TdxFootNoteBase; override;
  end;

  { TdxEndNoteRunInsertedHistoryItem }

  TdxEndNoteRunInsertedHistoryItem = class(TdxFootNoteRunInsertedHistoryItemBase)
  protected
    function CreateTextRunCore(AParagraph: TdxParagraphBase): TdxTextRunBase; override;
    function GetNote(ANoteIndex: Integer): TdxFootNoteBase; override;
  end;

  { TdxRemoveLastParagraphHistoryItem }

  TdxRemoveLastParagraphHistoryItem = class(TdxParagraphBaseHistoryItem)
  private
    FParagraph: TdxCustomParagraph;
    FRun: TdxTextRunBase;
    FOriginalParagraphCount: Integer;
    FNotificationId: Integer;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    destructor Destroy; override;

    property OriginalParagraphCount: Integer read FOriginalParagraphCount write FOriginalParagraphCount;
  end;

  { TdxFixLastParagraphOfLastSectionHistoryItem }

  TdxFixLastParagraphOfLastSectionHistoryItem = class(TdxParagraphBaseHistoryItem)
  private
    FSection: TdxSection;
    FLastRun: TdxSectionRun;
    FOriginalParagraphCount: Integer;
    function GetDocumentModel: TdxDocumentModel;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property OriginalParagraphCount: Integer read FOriginalParagraphCount write FOriginalParagraphCount;
  end;

implementation

uses
  Contnrs,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentModel.Commands;

{ TdxRemoveLastParagraphHistoryItem }

destructor TdxRemoveLastParagraphHistoryItem.Destroy;
var
  ADocumentModel: TdxDocumentModel;
begin
  ADocumentModel := TdxDocumentModel(PieceTable.DocumentModel);
  if ADocumentModel.LineNumberRun.Paragraph = FParagraph then
    ADocumentModel.RecreateLineNumberRun;

  FreeAndNil(FRun);
  FreeAndNil(FParagraph);
  inherited Destroy;
end;

procedure TdxRemoveLastParagraphHistoryItem.RedoCore;
var
  ARunIndex: TdxRunIndex;
  AParagraphIndex: TdxParagraphIndex;
begin
  AParagraphIndex := OriginalParagraphCount - 1;
  FParagraph := PieceTable.Paragraphs[AParagraphIndex];
  PieceTable.Paragraphs.RemoveAt(AParagraphIndex);
  ARunIndex := PieceTable.Runs.Count - 1;
  FRun := PieceTable.Runs[ARunIndex];
  PieceTable.Runs.Extract(ARunIndex);
  if FNotificationId = TdxNotificationIdGenerator.EmptyId then
    FNotificationId := DocumentModel.History.GetNotificationId;
  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(DocumentModel, PieceTable, AParagraphIndex,
    ARunIndex, 1, FNotificationId);
end;

procedure TdxRemoveLastParagraphHistoryItem.UndoCore;
var
  ARunIndex: TdxRunIndex;
  AParagraphIndex: TdxParagraphIndex;
begin
  PieceTable.Runs.Add(FRun);
  ARunIndex := PieceTable.Runs.Count - 1;
  FRun := nil;
  PieceTable.Paragraphs.Add(FParagraph);
  FParagraph := nil;
  AParagraphIndex := OriginalParagraphCount - 1;
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(DocumentModel, PieceTable,
    AParagraphIndex, ARunIndex, 1, FNotificationId);
end;

{ TdxFixLastParagraphOfLastSectionHistoryItem }

constructor TdxFixLastParagraphOfLastSectionHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  Assert(APieceTable is TdxPieceTable);
  inherited Create(APieceTable);
end;

procedure TdxFixLastParagraphOfLastSectionHistoryItem.RedoCore;
var
  ARuns: TdxTextRunCollection;
  ASections: TdxSectionCollection;
  ASectionIndex: TdxSectionIndex;
begin
  ASections := DocumentModel.Sections;
  FSection := ASections.Last;
  if FSection.FirstParagraphIndex = OriginalParagraphCount - 1 then
  begin
    FSection.UnsubscribeHeadersFootersEvents;
    ASectionIndex := ASections.Count - 1;
    ASections.Delete(ASectionIndex);
  end
  else
    FSection.LastParagraphIndex := FSection.LastParagraphIndex - 1;
  ARuns := PieceTable.Runs;
  if ARuns.Last is TdxSectionRun then
  begin
    FLastRun := TdxSectionRun(ARuns.Last);
    DocumentModel.UnsafeEditor.ReplaceSectionRunWithParagraphRun(PieceTable, FLastRun, ARuns.Count - 1);
  end;
end;

procedure TdxFixLastParagraphOfLastSectionHistoryItem.UndoCore;
begin
  if FSection.FirstParagraphIndex = OriginalParagraphCount - 1 then
  begin
    FSection.SubscribeHeadersFootersEvents;
    DocumentModel.Sections.Add(FSection);
  end
  else
    FSection.LastParagraphIndex := FSection.LastParagraphIndex + 1;
  if FLastRun <> nil then
    DocumentModel.UnsafeEditor.ReplaceParagraphRunWithSectionRun(PieceTable, FLastRun, PieceTable.Runs.Count - 1);
end;

function TdxFixLastParagraphOfLastSectionHistoryItem.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

{ TdxFootNoteRunInsertedHistoryItemBase }

procedure TdxFootNoteRunInsertedHistoryItemBase.UndoCore;
var
  ANote: TdxFootNoteBase;
begin
  ANote := GetNote(NoteIndex);
  Assert(ANote <> nil);
  ANote.ReferenceRun.Free;
  ANote.ReferenceRun := nil;
  inherited UndoCore;
end;

function TdxFootNoteRunInsertedHistoryItemBase.CreateTextRun(AParagraph: TdxParagraphBase): TdxTextRunBase;
var
  AFootNoteRun: TdxFootNoteRunBase;
  ANote: TdxFootNoteBase;
begin
  AFootNoteRun := TdxFootNoteRunBase(inherited CreateTextRun(AParagraph));
  AFootNoteRun.NoteIndex := NoteIndex;
  if not PieceTable.ContentType.IsNote then
  begin
    ANote := GetNote(NoteIndex);
    if ANote <> nil then
      ANote.ReferenceRun := AFootNoteRun;
  end;
  Result := AFootNoteRun;
end;

{ TdxFootNoteRunInsertedHistoryItem }

function TdxFootNoteRunInsertedHistoryItem.CreateTextRunCore(AParagraph: TdxParagraphBase): TdxTextRunBase;
begin
  Result := TdxFootNoteRun.Create(AParagraph, StartIndex, NewLength);
end;

function TdxFootNoteRunInsertedHistoryItem.GetNote(ANoteIndex: Integer): TdxFootNoteBase;
begin
  if ANoteIndex < 0 then
    Exit(nil);
  Result := DocumentModel.FootNotes[ANoteIndex];
end;

{ TdxEndNoteRunInsertedHistoryItem }

function TdxEndNoteRunInsertedHistoryItem.CreateTextRunCore(AParagraph: TdxParagraphBase): TdxTextRunBase;
begin
  Result := TdxEndNoteRun.Create(AParagraph, StartIndex, NewLength);
end;

function TdxEndNoteRunInsertedHistoryItem.GetNote(ANoteIndex: Integer): TdxFootNoteBase;
begin
  if ANoteIndex < 0 then
    Exit(nil);
  Result := DocumentModel.EndNotes[ANoteIndex];
end;

{ TdxLayoutDependentTextRunInsertedHistoryItem }

destructor TdxLayoutDependentTextRunInsertedHistoryItem.Destroy;
begin
  FreeAndNil(FFieldResultFormatting);
  inherited Destroy;
end;

function TdxLayoutDependentTextRunInsertedHistoryItem.CreateTextRun(AParagraph: TdxParagraphBase): TdxTextRunBase;
begin
  Result := CreateTextRunCore(AParagraph);
  TdxLayoutDependentTextRun(Result).FieldResultFormatting := FFieldResultFormatting.Clone;
end;

function TdxLayoutDependentTextRunInsertedHistoryItem.CreateTextRunCore(AParagraph: TdxParagraphBase): TdxTextRunBase;
begin
  Result := TdxLayoutDependentTextRun.Create(AParagraph, StartIndex, NewLength);
end;

function TdxLayoutDependentTextRunInsertedHistoryItem.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

procedure TdxLayoutDependentTextRunInsertedHistoryItem.SetFieldResultFormatting(const Value: TdxFieldResultFormatting);
begin
  FFieldResultFormatting.Free;
  FFieldResultFormatting := Value.Clone;
end;

{ TdxFieldSymbolResultInsertedHistoryItem }

function TdxFieldSymbolResultInsertedHistoryItem.CreateTextRun(AParagraph: TdxParagraphBase): TdxTextRunBase;
begin
  Result := TdxFieldSymbolRun.Create(AParagraph, StartIndex, NewLength);
end;

{ TdxFloatingObjectAnchorRunInsertedHistoryItem }

destructor TdxFloatingObjectAnchorRunInsertedHistoryItem.Destroy;
begin
  if FOwnsNewRun then
    FreeAndNil(FNewRun);
  inherited Destroy;
end;

procedure TdxFloatingObjectAnchorRunInsertedHistoryItem.AfterInsertRun(ADocumentModel: TdxDocumentModel;
  ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
var
  ALastInsertedRunInfo: TdxLastInsertedFloatingObjectAnchorRunInfo;
begin
  ALastInsertedRunInfo := PieceTable.LastInsertedFloatingObjectAnchorRunInfo;
  ALastInsertedRunInfo.Run := TdxFloatingObjectAnchorRun(ARun);
  ALastInsertedRunInfo.HistoryItem := Self;
  ALastInsertedRunInfo.RunIndex := ARunIndex;
end;

function TdxFloatingObjectAnchorRunInsertedHistoryItem.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxFloatingObjectAnchorRunInsertedHistoryItem.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

procedure TdxFloatingObjectAnchorRunInsertedHistoryItem.Execute;
begin
  FNewRun := TdxFloatingObjectAnchorRun.Create(PieceTable.Runs[RunIndex].Paragraph);
  FNewRun.StartIndex := StartIndex;

  inherited Execute;
end;

procedure TdxFloatingObjectAnchorRunInsertedHistoryItem.RedoCore;
begin
  PieceTable.Runs.Insert(RunIndex, FNewRun);
  FOwnsNewRun := False;
  DocumentModel.ResetMerging;
  if FNotificationId = TdxNotificationIdGenerator.EmptyId then
    FNotificationId := DocumentModel.History.GetNotificationId;
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(PieceTable, PieceTable, ParagraphIndex, RunIndex, 1, FNotificationId);
  AfterInsertRun(DocumentModel, FNewRun, RunIndex);
  PieceTable.Runs[RunIndex].AfterRunInserted;
end;

procedure TdxFloatingObjectAnchorRunInsertedHistoryItem.UndoCore;
begin
  PieceTable.Runs[RunIndex].BeforeRunRemoved;
  PieceTable.Runs.Extract(RunIndex);
  FOwnsNewRun := True;
  DocumentModel.ResetMerging;

  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(PieceTable, PieceTable, ParagraphIndex, RunIndex, 1, FNotificationId);
end;

end.
