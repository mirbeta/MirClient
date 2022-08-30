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

unit dxRichEdit.DocumentModel.History.Simple;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, Contnrs,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.Tables.Core,
  dxRichEdit.DocumentModel.Selections.Core,
  dxRichEdit.DocumentModel.InlineObjectFormatting,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem;

type
  TdxSyntaxHighlightHistoryItem = class;

  { TdxParagraphBaseHistoryItem }

  TdxParagraphBaseHistoryItem = class abstract(TdxRichEditHistoryItem)
  private
    FParagraphIndex: TdxParagraphIndex;
    FSectionIndex: TdxSectionIndex;
    FTableCellIndex: Integer;
    FTableRowIndex: Integer;
    FTableIndex: Integer;
  protected
    function GetTable: TdxCustomTable; virtual;
    function GetCell: TdxCustomTableCell; virtual;

    property TableCellIndex: Integer read FTableCellIndex;
    property TableIndex: Integer read FTableIndex;
    property Table: TdxCustomTable read GetTable;
    property Cell: TdxCustomTableCell read GetCell;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    procedure SetTableCell(ACell: TdxCustomTableCell); virtual;

    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex write FParagraphIndex;
    property SectionIndex: TdxSectionIndex read FSectionIndex write FSectionIndex;
  end;

  { TdxTextRunBaseHistoryItem }

  TdxTextRunBaseHistoryItem = class abstract(TdxParagraphBaseHistoryItem)
  private
    FRunIndex: TdxRunIndex;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;

    property RunIndex: TdxRunIndex read FRunIndex write FRunIndex;
  end;

  { TdxTextRunInsertedBaseHistoryItem }

  TdxTextRunInsertedBaseHistoryItem = class(TdxTextRunBaseHistoryItem)
  private
    FStartIndex: Integer;
    FForceVisible: Boolean;
  protected
    procedure ApplyFormattingToNewRun(ARuns: TdxTextRunCollection; ARun: TdxTextRunBase); virtual;
    function CalculateFormattingRunIndex(ARuns: TdxTextRunCollection): TdxRunIndex;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;

    property StartIndex: Integer read FStartIndex write FStartIndex;
    property ForceVisible: Boolean read FForceVisible write FForceVisible;
  end;

  { TdxTextRunInsertedHistoryItem }

  TdxTextRunInsertedHistoryItem = class(TdxTextRunInsertedBaseHistoryItem)
  private
    FNewLength: Integer;
    FNotificationId: Integer;
    procedure AfterInsertRun(ADocumentModel: TdxSimpleDocumentModel; ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
    function CreateTextRun(AParagraph: TdxParagraphBase): TdxTextRunBase; virtual;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;

    property NewLength: Integer read FNewLength write FNewLength;
  end;

  { TdxTextRunAppendTextHistoryItem }

  TdxTextRunAppendTextHistoryItem = class(TdxTextRunBaseHistoryItem)
  private
    FTextLength: Integer;
    FLogPosition: TdxDocumentLogPosition;
    FHistoryItem: TdxTextRunInsertedHistoryItem;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    property TextLength: Integer read FTextLength write FTextLength;
    property LogPosition: TdxDocumentLogPosition read FLogPosition write FLogPosition;
  end;

  { TdxRichEditSelectionHistoryItem }

  TdxRichEditSelectionHistoryItem = class(TdxRichEditHistoryItem)
  strict private
    FInfo: TdxSelectionPersistentInfo;
    procedure StoreSelection;
    procedure RestoreSelection;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    destructor Destroy; override;
  end;

  { TdxRichEditCompositeHistoryItem }

  TdxRichEditCompositeHistoryItem = class(TdxCompositeHistoryItem)
  private
    FFirstSelection: TdxRichEditSelectionHistoryItem;
    FLastSelection: TdxRichEditSelectionHistoryItem;
    FSyntaxHighlightTransaction: TdxSyntaxHighlightHistoryItem;
    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetFirstSelection: TdxRichEditSelectionHistoryItem;
    function GetLastSelection: TdxRichEditSelectionHistoryItem;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;

    property FirstSelection: TdxRichEditSelectionHistoryItem read GetFirstSelection;
    property LastSelection: TdxRichEditSelectionHistoryItem read GetLastSelection;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;
    destructor Destroy; override;

    function NeedStoreSelection: Boolean; override;

    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property SyntaxHighlightTransaction: TdxSyntaxHighlightHistoryItem
      read FSyntaxHighlightTransaction write FSyntaxHighlightTransaction;
  end;

  { TdxSyntaxHighlightHistoryItem }

  TdxSyntaxHighlightHistoryItem = class(TdxRichEditCompositeHistoryItem)
  private
    FParent: TdxRichEditCompositeHistoryItem;
    FStartTransactionLevel: Integer;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel;
      AParent: TdxRichEditCompositeHistoryItem; AStartTransactionLevel: Integer); reintroduce;

    property Parent: TdxRichEditCompositeHistoryItem read FParent;
    property StartTransactionLevel: Integer read FStartTransactionLevel;
  end;

  { TdxRichEditDocumentHistory }

  TdxRichEditDocumentHistory = class(TdxDocumentHistory)
  private
    function GetDocumentModel: TdxSimpleDocumentModel;
  protected
    procedure OnEndUndoCore; override;
    function CommitAsSingleItemCore(ASingleItem: TdxHistoryItem): TdxHistoryItem; override;
    function CreateCompositeHistoryItem: TdxCompositeHistoryItem; override;
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    function Add(AItem: TdxHistoryItem): TdxHistoryItem; override;
    procedure AddRangeTextAppendedHistoryItem(AItem: TdxTextRunAppendTextHistoryItem); virtual;

    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
  end;

  { TdxMarkRunInsertedHistoryItemBase }

  TdxMarkRunInsertedHistoryItemBase = class(TdxTextRunInsertedBaseHistoryItem)
  private
    FNotificationId: Integer;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
    procedure AfterInsertRun(ADocumentModel: TdxSimpleDocumentModel; ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); virtual;
  public
    function CreateRun(AParagraph: TdxParagraphBase): TdxTextRunBase; virtual; abstract;
  end;

  { TdxParagraphRunInsertedHistoryItem }

  TdxParagraphRunInsertedHistoryItem = class(TdxMarkRunInsertedHistoryItemBase)
  public
    function CreateRun(AParagraph: TdxParagraphBase): TdxTextRunBase; override;
  end;

  { TdxParagraphInsertedBaseHistoryItem }

  TdxParagraphInsertedBaseHistoryItem = class(TdxParagraphBaseHistoryItem)
  private
    FLogPosition: TdxDocumentLogPosition;
    FParagraphMarkRunIndex: TdxRunIndex;
    FNotificationId: Integer;
    FNewParagraph: TdxSimpleParagraph;
    class procedure ChangeRangesParagraph(ARunIndex: TdxRunIndex; ARuns: TdxTextRunCollection; AOldParagraph, ANewParagraph: TdxSimpleParagraph); static;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    destructor Destroy; override;
    procedure Execute; override;

    property ParagraphMarkRunIndex: TdxRunIndex read FParagraphMarkRunIndex write FParagraphMarkRunIndex;
    property LogPosition: TdxDocumentLogPosition read FLogPosition write FLogPosition;
  end;

  { TdxInlineCustomObjectRunInsertedHistoryItem }

  TdxInlineCustomObjectRunInsertedHistoryItem = class(TdxTextRunInsertedBaseHistoryItem)
  private
    FCustomObject: IdxInlineCustomObject;
    FScaleX: Single;
    FScaleY: Single;
    FNotificationId: Integer;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    property CustomObject: IdxInlineCustomObject read FCustomObject write FCustomObject;
    property ScaleX: Single read FScaleX write FScaleX;
    property ScaleY: Single read FScaleY write FScaleY;
  end;

  { TdxInlinePictureRunInsertedHistoryItem }

  TdxInlinePictureRunInsertedHistoryItem = class(TdxTextRunInsertedBaseHistoryItem)
  private
    FImage: TdxOfficeImageReference;
    FNotificationId: Integer;
    procedure SetImage(const Value: TdxOfficeImageReference);
  protected
    function CreateInlinePictureRun(AParagraph: TdxSimpleParagraph; AImage: TdxOfficeImageReference): TdxInlinePictureRun; virtual;
    procedure UndoCore; override;
    procedure RedoCore; override;
    procedure AfterInsertRun(ADocumentModel: TdxSimpleDocumentModel; ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); virtual;
  public
    destructor Destroy; override;
    property Image: TdxOfficeImageReference read FImage write SetImage;
  end;

  { TdxRunPropertiesChangedHistoryItemBase }

  TdxRunPropertiesChangedHistoryItemBase = class(TdxIndexChangedHistoryItemCore)
  private
    FRunIndex: TdxRunIndex;
  protected
    function GetProperties(ARun: TdxRunBase): TdxIndexBasedObject; virtual; abstract;
  public
    constructor Create(const APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex); reintroduce;
    function GetObject: TdxIndexBasedObject; override;

    property RunIndex: TdxRunIndex read FRunIndex;
  end;

  { TdxRunInlinePicturePropertiesChangedHistoryItem }

  TdxRunInlinePicturePropertiesChangedHistoryItem = class(TdxRunPropertiesChangedHistoryItemBase)
  protected
    function GetProperties(ATextRunBase: TdxRunBase): TdxIndexBasedObject; override;
  end;

  { TdxRunCharacterPropertiesChangedHistoryItem }

  TdxRunCharacterPropertiesChangedHistoryItem = class(TdxRunPropertiesChangedHistoryItemBase)
  protected
    function GetProperties(ARun: TdxRunBase): TdxIndexBasedObject; override;
  end;

  { TdxFieldCodeStartRunInsertedHistoryItem }

  TdxFieldCodeStartRunInsertedHistoryItem = class(TdxMarkRunInsertedHistoryItemBase)
  public
    function CreateRun(AParagraph: TdxParagraphBase): TdxTextRunBase; override;
  end;

  { TdxFieldResultEndRunInsertedHistoryItem }

  TdxFieldResultEndRunInsertedHistoryItem = class(TdxMarkRunInsertedHistoryItemBase)
  public
    function CreateRun(AParagraph: TdxParagraphBase): TdxTextRunBase; override;
  end;

  { TdxFieldCodeEndRunInsertedHistoryItem }

  TdxFieldCodeEndRunInsertedHistoryItem = class(TdxMarkRunInsertedHistoryItemBase)
  public
    function CreateRun(AParagraph: TdxParagraphBase): TdxTextRunBase; override;
  end;

  { TdxInsertHyperlinkInfoHistoryItem }

  TdxInsertHyperlinkInfoHistoryItem = class(TdxRichEditHistoryItem)
  strict private
    FFieldIndex: Integer;
    FHyperlinkInfo: TdxHyperlinkInfo;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    property FieldIndex: Integer read FFieldIndex write FFieldIndex;
    property HyperlinkInfo: TdxHyperlinkInfo read FHyperlinkInfo write FHyperlinkInfo;
  end;

  { TdxSeparatorRunInsertedHistoryItem }

  TdxSeparatorRunInsertedHistoryItem = class(TdxMarkRunInsertedHistoryItemBase)
  protected
    procedure AfterInsertRun(ADocumentModel: TdxSimpleDocumentModel; ARun: TdxTextRunBase; ARunIndex: TdxRunIndex); override;
  public
    function CreateRun(AParagraph: TdxParagraphBase): TdxTextRunBase; override;
  end;

  { TdxTextRunSplitHistoryItem }

  TdxTextRunSplitHistoryItem = class(TdxTextRunBaseHistoryItem)
  private
    FSplitOffset: Integer;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;

    property SplitOffset: Integer read FSplitOffset write FSplitOffset;
  end;

  { TdxTextRunsJoinedHistoryItem }

  TdxTextRunsJoinedHistoryItem = class(TdxTextRunBaseHistoryItem)
  private
    FSplitOffset: Integer;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
  end;

  { TdxSimpleTextRunsDeletedHistoryItem }

  TdxSimpleTextRunsDeletedHistoryItem = class(TdxTextRunBaseHistoryItem)
  private
    FDeletedRunCount: Integer;
    FDeletedRuns: TdxObjectList<TdxTextRunBase>;
    FDeltaLength: Integer;
    FNotificationIds: TdxIntegerList;
  strict protected
    procedure AfterUndoCore; virtual;
    procedure BeforeRedoCore; virtual;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    destructor Destroy; override;

    property DeletedRunCount: Integer read FDeletedRunCount write FDeletedRunCount;
  end;

  { TdxParagraphsDeletedHistoryItem }

  TdxParagraphsDeletedHistoryItem = class(TdxParagraphBaseHistoryItem)
  strict private
    FDeletedParagraphsCount: Integer;
    FDeletedParagraphs: TdxObjectList<TdxSimpleParagraph>;
    FNotificationIds: TdxIntegerList;
    function GetDocumentModel: TdxSimpleDocumentModel;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    destructor Destroy; override;

    property DeletedParagraphsCount: Integer read FDeletedParagraphsCount write FDeletedParagraphsCount;
    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
  end;

  { TdxMergeParagraphsHistoryItem }

  TdxMergeParagraphsHistoryItem = class(TdxParagraphBaseHistoryItem)
  strict private
    FStartParagraph: TdxSimpleParagraph;
    FEndParagraph: TdxSimpleParagraph;
    FIndex: Integer;
    FEndParagraphIndex: TdxParagraphIndex;
    FParagraphStyleIndex: Integer;
    FUseFirstParagraphStyle: Boolean;
    FNotificationId: Integer;
    FOwnsParagraph: Boolean;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    destructor Destroy; override;

    property StartParagraph: TdxSimpleParagraph read FStartParagraph write FStartParagraph;
    property EndParagraph: TdxSimpleParagraph read FEndParagraph write FEndParagraph;
    property UseFirstParagraphStyle: Boolean read FUseFirstParagraphStyle write FUseFirstParagraphStyle;
  end;

implementation

uses
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.ParagraphRange;

{ TdxParagraphBaseHistoryItem }

constructor TdxParagraphBaseHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FParagraphIndex := -1;
  FSectionIndex := -1;
end;

function TdxParagraphBaseHistoryItem.GetCell: TdxCustomTableCell;
begin
  if Table <> nil then
    Result := Table.GetCellCore(FTableRowIndex, FTableCellIndex)
  else
    Result := nil;
end;

function TdxParagraphBaseHistoryItem.GetTable: TdxCustomTable;
begin
  Result := PieceTable.GetTableCore(TableIndex);
end;

procedure TdxParagraphBaseHistoryItem.SetTableCell(ACell: TdxCustomTableCell);
var
  ATable: TdxCustomTable;
  ARow: TdxCustomTableRow;
begin
  if ACell = nil then
  begin
    FTableCellIndex := -1;
    FTableRowIndex := -1;
    FTableIndex := -1;
  end
  else
  begin
    ATable := ACell.GetTableCore;
    FTableIndex := ATable.GetIndexCore;
    ARow := ACell.GetRowCore;
    FTableRowIndex := ARow.GetIndexCore;
    FTableCellIndex := ACell.GetIndexCore;
  end;
end;

{ TdxTextRunBaseHistoryItem }

constructor TdxTextRunBaseHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FRunIndex := -1;
end;

{ TdxTextRunInsertedBaseHistoryItem }

constructor TdxTextRunInsertedBaseHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FStartIndex := -1;
end;

procedure TdxTextRunInsertedBaseHistoryItem.ApplyFormattingToNewRun(ARuns: TdxTextRunCollection; ARun: TdxTextRunBase);
var
  AFormattingRunIndex: TdxRunIndex;
  ARunWithFormatting: TdxTextRunBase;
begin
  AFormattingRunIndex := CalculateFormattingRunIndex(ARuns);
  if AFormattingRunIndex < ARuns.Count then
  begin
    ARunWithFormatting := ARuns[AFormattingRunIndex];
    ARun.InheritStyleAndFormattingFromCore(ARunWithFormatting, FForceVisible);
  end;
end;

function TdxTextRunInsertedBaseHistoryItem.CalculateFormattingRunIndex(ARuns: TdxTextRunCollection): TdxRunIndex;
var
  APrevRunIndex, ANextRunIndex: TdxRunIndex;
begin
  APrevRunIndex := RunIndex - 1;
  ANextRunIndex := RunIndex + 1;
  while True do
  begin
    if (APrevRunIndex < 0) or (ARuns[APrevRunIndex] is TdxParagraphRun) then
    begin
      if ARuns[ANextRunIndex] is TdxSeparatorTextRun then
        Inc(ANextRunIndex)
      else
        Exit(ANextRunIndex);
    end
    else
    begin
      if ARuns[APrevRunIndex] is TdxSeparatorTextRun then
        Dec(APrevRunIndex)
      else
        Exit(APrevRunIndex);
    end;
  end;
end;

{ TdxTextRunInsertedHistoryItem }

constructor TdxTextRunInsertedHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FNewLength := -1;
end;

procedure TdxTextRunInsertedHistoryItem.AfterInsertRun(ADocumentModel: TdxSimpleDocumentModel; ARun: TdxTextRunBase;
  ARunIndex: TdxRunIndex);
var
  ALastInsertedRunInfo: TdxLastInsertedRunInfo;
begin
  ALastInsertedRunInfo := PieceTable.LastInsertedRunInfo;
  Assert(ARun is TdxTextRun);
  ALastInsertedRunInfo.Run := TdxTextRun(ARun);
  ALastInsertedRunInfo.HistoryItem := Self;
  ALastInsertedRunInfo.RunIndex := ARunIndex;
end;

function TdxTextRunInsertedHistoryItem.CreateTextRun(AParagraph: TdxParagraphBase): TdxTextRunBase;
begin
  Result := TdxTextRun.Create(AParagraph, StartIndex, NewLength);
end;

procedure TdxTextRunInsertedHistoryItem.RedoCore;
var
  ARuns: TdxTextRunCollection;
  AParagraph: TdxParagraphBase;
  ARun: TdxTextRun;
begin
  Assert(NewLength > 0);
  ARuns := PieceTable.Runs;
  AParagraph := PieceTable.Paragraphs[ParagraphIndex];
  ARun := TdxTextRun(CreateTextRun(AParagraph));
  ARuns.Insert(RunIndex, ARun);
  ApplyFormattingToNewRun(ARuns, ARun);
  if FNotificationId = TdxNotificationIdGenerator.EmptyId then
    FNotificationId := DocumentModel.History.GetNotificationId;
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(PieceTable, PieceTable, ParagraphIndex, RunIndex,
    NewLength, FNotificationId);
  AfterInsertRun(DocumentModel, ARun, RunIndex);
  ARuns[RunIndex].AfterRunInserted;
  PieceTable.ApplyChangesCore([TdxDocumentModelChangeAction.RaiseContentChanged], dxRunIndexDontCare, dxRunIndexDontCare);
end;

procedure TdxTextRunInsertedHistoryItem.UndoCore;
begin
  PieceTable.Runs[RunIndex].BeforeRunRemoved;
  PieceTable.Runs.Delete(RunIndex);
  DocumentModel.ResetMerging;
  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(PieceTable, PieceTable, ParagraphIndex, RunIndex, NewLength, FNotificationId);
  PieceTable.ApplyChangesCore([TdxDocumentModelChangeAction.RaiseContentChanged], dxRunIndexDontCare, dxRunIndexDontCare);
end;

{ TdxTextRunAppendTextHistoryItem }

procedure TdxTextRunAppendTextHistoryItem.RedoCore;
var
  ALastInsertedRunInfo: TdxLastInsertedRunInfo;
begin
  ALastInsertedRunInfo := PieceTable.LastInsertedRunInfo;
  Assert(ALastInsertedRunInfo <> nil);
  Assert(ALastInsertedRunInfo.Run <> nil);
  Assert(ALastInsertedRunInfo.HistoryItem <> nil);
  Assert(ALastInsertedRunInfo.RunIndex = RunIndex);
  FHistoryItem := TdxTextRunInsertedHistoryItem(ALastInsertedRunInfo.HistoryItem);
  ALastInsertedRunInfo.LogPosition := LogPosition + TextLength;
  ALastInsertedRunInfo.Run.Length := ALastInsertedRunInfo.Run.Length + TextLength;
  TdxTextRunInsertedHistoryItem(ALastInsertedRunInfo.HistoryItem).NewLength := TdxTextRunInsertedHistoryItem(ALastInsertedRunInfo.HistoryItem).NewLength + TextLength;
  DocumentModel.History.RaiseOperationCompleted;
  TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(PieceTable, PieceTable, ParagraphIndex, RunIndex, TextLength);
end;

procedure TdxTextRunAppendTextHistoryItem.UndoCore;
var
  ALastInsertedRunInfo: TdxLastInsertedRunInfo;
begin
  ALastInsertedRunInfo := PieceTable.LastInsertedRunInfo;
  ALastInsertedRunInfo.RunIndex := RunIndex;
  ALastInsertedRunInfo.Run := PieceTable.Runs[RunIndex] as TdxTextRun;
  ALastInsertedRunInfo.HistoryItem := FHistoryItem;
  ALastInsertedRunInfo.LogPosition := FLogPosition;
  ALastInsertedRunInfo.Run.Length := ALastInsertedRunInfo.Run.Length - TextLength;
  TdxTextRunInsertedHistoryItem(ALastInsertedRunInfo.HistoryItem).NewLength := TdxTextRunInsertedHistoryItem(ALastInsertedRunInfo.HistoryItem).NewLength - TextLength;
  DocumentModel.History.RaiseOperationCompleted;
  TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(PieceTable, PieceTable, ParagraphIndex, RunIndex, -TextLength);
end;

{ TdxRichEditSelectionHistoryItem }

destructor TdxRichEditSelectionHistoryItem.Destroy;
begin
  FreeAndNil(FInfo);
  inherited Destroy;
end;

procedure TdxRichEditSelectionHistoryItem.UndoCore;
begin
  if FInfo <> nil then
    RestoreSelection;
end;

procedure TdxRichEditSelectionHistoryItem.RedoCore;
begin
  if FInfo = nil then
    StoreSelection
  else
    RestoreSelection;
end;

procedure TdxRichEditSelectionHistoryItem.StoreSelection;
begin
  Assert(FInfo = nil);
  FInfo := DocumentModel.Selection.GetSelectionPersistentInfo;
end;

procedure TdxRichEditSelectionHistoryItem.RestoreSelection;
begin
  if DocumentModel.GetActivePieceTableCore = FInfo.PieceTable then
    DocumentModel.Selection.RestoreSelection(FInfo);
end;

{ TdxSyntaxHighlightHistoryItem }

constructor TdxSyntaxHighlightHistoryItem.Create(ADocumentModel: TdxCustomDocumentModel;
  AParent: TdxRichEditCompositeHistoryItem; AStartTransactionLevel: Integer);
begin
  inherited Create(ADocumentModel);
  FParent := AParent;
  FStartTransactionLevel := AStartTransactionLevel;
end;

{ TdxRichEditCompositeHistoryItem }

constructor TdxRichEditCompositeHistoryItem.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel.MainPart);
  FirstSelection.Execute;
end;

destructor TdxRichEditCompositeHistoryItem.Destroy;
begin
  FreeAndNil(FFirstSelection);
  FreeAndNil(FLastSelection);
  inherited Destroy;
end;

function TdxRichEditCompositeHistoryItem.NeedStoreSelection: Boolean;
var
  I: Integer;
begin
  Result := Count > 0;
  if Result then
  begin
    Result := False;
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].NeedStoreSelection;
      if Result then
        Break;
    end;
  end;
end;

procedure TdxRichEditCompositeHistoryItem.RedoCore;
begin
  if NeedStoreSelection then
    FirstSelection.Redo;
  inherited RedoCore;
  if FSyntaxHighlightTransaction <> nil then
    FSyntaxHighlightTransaction.Redo;
  if NeedStoreSelection then
    LastSelection.Redo;
end;

procedure TdxRichEditCompositeHistoryItem.UndoCore;
begin
  if NeedStoreSelection then
    LastSelection.Undo;
  if FSyntaxHighlightTransaction <> nil then
    FSyntaxHighlightTransaction.Undo;
  inherited UndoCore;
  if NeedStoreSelection then
    FirstSelection.Undo;
end;

function TdxRichEditCompositeHistoryItem.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := TdxSimpleDocumentModel(inherited DocumentModel);
end;

function TdxRichEditCompositeHistoryItem.GetFirstSelection: TdxRichEditSelectionHistoryItem;
begin
  if FFirstSelection = nil then
    FFirstSelection := TdxRichEditSelectionHistoryItem.Create(DocumentModel.GetActivePieceTableCore);
  Result := FFirstSelection;
end;

function TdxRichEditCompositeHistoryItem.GetLastSelection: TdxRichEditSelectionHistoryItem;
begin
  if FLastSelection = nil then
    FLastSelection := TdxRichEditSelectionHistoryItem.Create(DocumentModel.GetActivePieceTableCore);
  Result := FLastSelection;
end;

{ TdxRichEditDocumentHistory }

function TdxRichEditDocumentHistory.Add(AItem: TdxHistoryItem): TdxHistoryItem;
begin
  if TransactionLevel <> 0  then
  begin
    if Transaction is TdxRichEditCompositeHistoryItem then
      Assert(TdxRichEditCompositeHistoryItem(Transaction).SyntaxHighlightTransaction = nil);
  end;
  Result := inherited Add(AItem);
end;

procedure TdxRichEditDocumentHistory.AddRangeTextAppendedHistoryItem(AItem: TdxTextRunAppendTextHistoryItem);
begin
  if TransactionLevel <> 0 then
    Transaction.AddItem(AItem)
  else
    SetModifiedTextAppended(True);
end;

function TdxRichEditDocumentHistory.CommitAsSingleItemCore(ASingleItem: TdxHistoryItem): TdxHistoryItem;
var
  ATransaction: TdxRichEditCompositeHistoryItem;
begin
  if Transaction is TdxRichEditCompositeHistoryItem then
  begin
    ATransaction := TdxRichEditCompositeHistoryItem(Transaction);
    if ATransaction.SyntaxHighlightTransaction <> nil then
      Exit(nil);
  end;
  if ASingleItem is TdxTextRunAppendTextHistoryItem then
  begin
    Transaction.Items.Delete(0);
    SetModifiedTextAppended(False);
    Result := nil;
  end
  else
  begin
    if ASingleItem.NeedStoreSelection then
      Result := inherited CommitAsSingleItemCore(Transaction)
    else
      Result := inherited CommitAsSingleItemCore(ASingleItem);
  end;
end;

function TdxRichEditDocumentHistory.CreateCompositeHistoryItem: TdxCompositeHistoryItem;
begin
  Result := TdxRichEditCompositeHistoryItem.Create(DocumentModel);
end;

function TdxRichEditDocumentHistory.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := TdxSimpleDocumentModel(inherited DocumentModel);
end;

procedure TdxRichEditDocumentHistory.OnEndUndoCore;
begin
  if not DocumentModel.ValidateActivePieceTable then
    DocumentModel.SetActivePieceTable(DocumentModel.MainPieceTable);
end;

procedure TdxRichEditDocumentHistory.RedoCore;
begin
  DocumentModel.DeferredChanges.SuppressSyntaxHighlight := True;
  inherited RedoCore;
end;

procedure TdxRichEditDocumentHistory.UndoCore;
begin
  DocumentModel.DeferredChanges.SuppressSyntaxHighlight := True;
  inherited UndoCore;
end;

{ TdxMarkRunInsertedHistoryItemBase }

procedure TdxMarkRunInsertedHistoryItemBase.AfterInsertRun(ADocumentModel: TdxSimpleDocumentModel; ARun: TdxTextRunBase;
  ARunIndex: TdxRunIndex);
begin
end;

procedure TdxMarkRunInsertedHistoryItemBase.RedoCore;
var
  ARuns: TdxTextRunCollection;
  AParagraph: TdxParagraphBase;
  ANewRun: TdxTextRunBase;
begin
  ARuns := PieceTable.Runs;
  AParagraph := ARuns[RunIndex].Paragraph;
  ANewRun := CreateRun(AParagraph);
  ANewRun.StartIndex := StartIndex;
  ARuns.Insert(RunIndex, ANewRun);
  ApplyFormattingToNewRun(ARuns, ANewRun);
  DocumentModel.ResetMerging;
  if FNotificationId = TdxNotificationIdGenerator.EmptyId then
    FNotificationId := DocumentModel.History.GetNotificationId;
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(PieceTable, PieceTable, ParagraphIndex, RunIndex, 1, FNotificationId);
  AfterInsertRun(DocumentModel, ANewRun, RunIndex);
  ARuns[RunIndex].AfterRunInserted;
end;

procedure TdxMarkRunInsertedHistoryItemBase.UndoCore;
begin
  PieceTable.Runs[RunIndex].BeforeRunRemoved;
  PieceTable.Runs.Delete(RunIndex);
  DocumentModel.ResetMerging;
  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(PieceTable, PieceTable, ParagraphIndex, RunIndex, 1, FNotificationId);
end;

{ TdxParagraphRunInsertedHistoryItem }

function TdxParagraphRunInsertedHistoryItem.CreateRun(AParagraph: TdxParagraphBase): TdxTextRunBase;
begin
  Result := TdxParagraphRun.Create(AParagraph);
end;

{ TdxParagraphInsertedBaseHistoryItem }

constructor TdxParagraphInsertedBaseHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FLogPosition := -1;
  FParagraphMarkRunIndex := -1;
end;

destructor TdxParagraphInsertedBaseHistoryItem.Destroy;
begin
  FreeAndNil(FNewParagraph);
  inherited Destroy;
end;

procedure TdxParagraphInsertedBaseHistoryItem.Execute;
begin
  Assert(FNewParagraph = nil);
  FNewParagraph := PieceTable.CreateParagraph;
  inherited Execute;
end;

procedure TdxParagraphInsertedBaseHistoryItem.RedoCore;
var
  AParagraph: TdxSimpleParagraph;
  ASectionIndex: TdxSectionIndex;
  AParagraphLastRunIndex: TdxRunIndex;
  AParagraphFirstRunIndex: TdxRunIndex;
  AParagraphLogPosition: TdxDocumentLogPosition;
begin
  AParagraph := PieceTable.Paragraphs[ParagraphIndex];
  ASectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(ParagraphIndex);
  Assert((ASectionIndex >= 0) or ((ASectionIndex = dxRunIndexDontCare) and
    (PieceTable.ContentType.IsNote or PieceTable.ContentType.IsTextBox or PieceTable.ContentType.IsComment)));
  FNewParagraph.SetParagraphStyleIndexCore(AParagraph.ParagraphStyleIndex);
  AParagraphLastRunIndex := ParagraphMarkRunIndex;
  AParagraphFirstRunIndex := AParagraph.FirstRunIndex;
  AParagraphLogPosition := AParagraph.LogPosition;
  FNewParagraph.Length := LogPosition - AParagraphLogPosition + 1;
  AParagraph.RelativeFirstRunIndex := FParagraphMarkRunIndex + 1;
  AParagraph.Length := AParagraph.Length - FNewParagraph.Length;
  AParagraph.ShiftLogPosition(FNewParagraph.Length);
  PieceTable.Paragraphs.Insert(ParagraphIndex, FNewParagraph);
  FNewParagraph.RelativeFirstRunIndex := AParagraphFirstRunIndex;
  FNewParagraph.RelativeLastRunIndex := AParagraphLastRunIndex;
  FNewParagraph.RelativeLogPosition := AParagraphLogPosition;
  ChangeRangesParagraph(AParagraphLastRunIndex, PieceTable.Runs, AParagraph, FNewParagraph);
  if FNotificationId = TdxNotificationIdGenerator.EmptyId then
    FNotificationId := DocumentModel.History.GetNotificationId;
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(PieceTable, PieceTable, ASectionIndex, ParagraphIndex, ParagraphMarkRunIndex + 1, Cell, false, ParagraphIndex, FNotificationId);
  FNewParagraph.InheritStyleAndFormattingFromCore(AParagraph);
  FNewParagraph := nil;
end;

procedure TdxParagraphInsertedBaseHistoryItem.UndoCore;
var
  AParagraph: TdxSimpleParagraph;
  ASectionIndex: TdxSectionIndex;
  ARunIndex: TdxRunIndex;
begin
  Assert(FNewParagraph = nil);
  AParagraph := PieceTable.Paragraphs[ParagraphIndex + 1];
  ASectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(ParagraphIndex);
  Assert((ASectionIndex >= 0) or
    ((ASectionIndex = dxSectionIndexDontCare) and
    (PieceTable.ContentType.IsNote or PieceTable.ContentType.IsTextBox or PieceTable.ContentType.IsComment)));
  FNewParagraph := PieceTable.Paragraphs[ParagraphIndex];
  ARunIndex := AParagraph.FirstRunIndex;
  AParagraph.RelativeFirstRunIndex := FNewParagraph.FirstRunIndex;
  AParagraph.Length := AParagraph.Length + FNewParagraph.Length;
  AParagraph.RelativeLogPosition := FNewParagraph.LogPosition;
  ChangeRangesParagraph(FNewParagraph.LastRunIndex, PieceTable.Runs, FNewParagraph, AParagraph);
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(PieceTable, PieceTable, ASectionIndex, ParagraphIndex, ARunIndex, FNotificationId);
  PieceTable.Paragraphs.RemoveAt(ParagraphIndex);
end;

class procedure TdxParagraphInsertedBaseHistoryItem.ChangeRangesParagraph(ARunIndex: TdxRunIndex; ARuns: TdxTextRunCollection;
  AOldParagraph, ANewParagraph: TdxSimpleParagraph);
var
  I: Integer;
  ARange: TdxTextRunBase;
begin
  for I := ARunIndex downto 0 do
  begin
    ARange := ARuns[I];
    if ARange.Paragraph <> AOldParagraph then
      Break;
    ARange.Paragraph := ANewParagraph;
  end;
end;

{ TdxInlineCustomObjectRunInsertedHistoryItem }

procedure TdxInlineCustomObjectRunInsertedHistoryItem.UndoCore;
begin
  PieceTable.Runs[RunIndex].BeforeRunRemoved;
  PieceTable.Runs.Delete(RunIndex);
  DocumentModel.ResetMerging;

  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(PieceTable, PieceTable, ParagraphIndex, RunIndex, 1, FNotificationId);
end;

procedure TdxInlineCustomObjectRunInsertedHistoryItem.RedoCore;
var
  ARuns: TdxTextRunCollection;
  ANewRange: TdxInlineCustomObjectRun;
begin
  ARuns := PieceTable.Runs;
  ANewRange := TdxInlineCustomObjectRun.Create(ARuns[RunIndex].Paragraph, FCustomObject);
  ANewRange.StartIndex := StartIndex;
  ANewRange.ScaleX := ScaleX;
  ANewRange.ScaleY := ScaleY;
  ARuns.Insert(RunIndex, ANewRange);
  DocumentModel.ResetMerging;
  if FNotificationId = TdxNotificationIdGenerator.EmptyId then
    FNotificationId := DocumentModel.History.GetNotificationId;
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(PieceTable, PieceTable, ParagraphIndex, RunIndex, 1, FNotificationId);
  ARuns[RunIndex].AfterRunInserted;
end;

{ TdxInlinePictureRunInsertedHistoryItem }

destructor TdxInlinePictureRunInsertedHistoryItem.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxInlinePictureRunInsertedHistoryItem.AfterInsertRun(ADocumentModel: TdxSimpleDocumentModel; ARun: TdxTextRunBase;
  ARunIndex: TdxRunIndex);
var
  ALastInsertedRunInfo: TdxLastInsertedInlinePictureRunInfo;
begin
  ALastInsertedRunInfo := PieceTable.LastInsertedInlinePictureRunInfo;
  ALastInsertedRunInfo.Run := TdxInlinePictureRun(ARun);
  ALastInsertedRunInfo.HistoryItem := Self;
  ALastInsertedRunInfo.RunIndex := ARunIndex;
end;

function TdxInlinePictureRunInsertedHistoryItem.CreateInlinePictureRun(AParagraph: TdxSimpleParagraph; AImage: TdxOfficeImageReference): TdxInlinePictureRun;
begin
  Result := TdxInlinePictureRun.Create(AParagraph, AImage);
end;

procedure TdxInlinePictureRunInsertedHistoryItem.RedoCore;
var
  ANewRun: TdxInlinePictureRun;
  ARuns: TdxTextRunCollection;
begin
  ARuns := PieceTable.Runs;
  ANewRun := CreateInlinePictureRun(ARuns[RunIndex].Paragraph, FImage);
  ANewRun.StartIndex := StartIndex;
  ARuns.Insert(RunIndex, ANewRun);
  DocumentModel.ResetMerging;
  if FNotificationId = TdxNotificationIdGenerator.EmptyId then
    FNotificationId := DocumentModel.History.GetNotificationId;
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(PieceTable, PieceTable, ParagraphIndex, RunIndex, 1, FNotificationId);
  AfterInsertRun(DocumentModel, ANewRun, RunIndex);
  ARuns[RunIndex].AfterRunInserted;
end;

procedure TdxInlinePictureRunInsertedHistoryItem.UndoCore;
begin
  PieceTable.Runs[RunIndex].BeforeRunRemoved;
  PieceTable.Runs.Delete(RunIndex);
  DocumentModel.ResetMerging;
  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(PieceTable, PieceTable, ParagraphIndex, RunIndex, 1, FNotificationId);
end;

procedure TdxInlinePictureRunInsertedHistoryItem.SetImage(const Value: TdxOfficeImageReference);
begin
  FImage.Free;
  FImage := Value.Clone(DocumentModel.ImageCache);
end;

{ TdxRunPropertiesChangedHistoryItemBase }

constructor TdxRunPropertiesChangedHistoryItemBase.Create(const APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex);
begin
  inherited Create(APieceTable);
  Assert(ARunIndex >= 0);
  FRunIndex := ARunIndex;
end;

function TdxRunPropertiesChangedHistoryItemBase.GetObject: TdxIndexBasedObject;
begin
  Result := GetProperties(PieceTable.Runs[RunIndex]);
end;

{ TdxRunInlinePicturePropertiesChangedHistoryItem }

function TdxRunInlinePicturePropertiesChangedHistoryItem.GetProperties(
  ATextRunBase: TdxRunBase): TdxIndexBasedObject;
begin
  Result := TdxInlinePictureRun(ATextRunBase).PictureProperties;
end;

{ TdxRunCharacterPropertiesChangedHistoryItem }

function TdxRunCharacterPropertiesChangedHistoryItem.GetProperties(ARun: TdxRunBase): TdxIndexBasedObject;
begin
  Result := TdxTextRunBase(ARun).CharacterProperties;
end;

{ TdxFieldCodeStartRunInsertedHistoryItem }

function TdxFieldCodeStartRunInsertedHistoryItem.CreateRun(
  AParagraph: TdxParagraphBase): TdxTextRunBase;
begin
  Result := TdxFieldCodeStartRun.Create(AParagraph);
end;

{ TdxFieldResultEndRunInsertedHistoryItem }

function TdxFieldResultEndRunInsertedHistoryItem.CreateRun(
  AParagraph: TdxParagraphBase): TdxTextRunBase;
begin
  Result := TdxFieldResultEndRun.Create(AParagraph);
end;

{ TdxFieldCodeEndRunInsertedHistoryItem }

function TdxFieldCodeEndRunInsertedHistoryItem.CreateRun(
  AParagraph: TdxParagraphBase): TdxTextRunBase;
begin
  Result := TdxFieldCodeEndRun.Create(AParagraph);
end;

{ TdxInsertHyperlinkInfoHistoryItem }

procedure TdxInsertHyperlinkInfoHistoryItem.RedoCore;
begin
  PieceTable.HyperlinkInfos.Add(FFieldIndex, FHyperlinkInfo);
  DocumentModel.RaiseHyperlinkInfoInserted(PieceTable, FFieldIndex);
end;

procedure TdxInsertHyperlinkInfoHistoryItem.UndoCore;
begin
  PieceTable.HyperlinkInfos[FFieldIndex] := nil;
  DocumentModel.RaiseHyperlinkInfoDeleted(PieceTable, FFieldIndex);
end;

{ TdxSeparatorRunInsertedHistoryItem }

procedure TdxSeparatorRunInsertedHistoryItem.AfterInsertRun(ADocumentModel: TdxSimpleDocumentModel;
  ARun: TdxTextRunBase; ARunIndex: TdxRunIndex);
var
  ALastInsertedSeparatorRunInfo: TdxLastInsertedSeparatorRunInfo;
begin
  ALastInsertedSeparatorRunInfo := PieceTable.LastInsertedSeparatorRunInfo;
  ALastInsertedSeparatorRunInfo.Run := ARun as TdxSeparatorTextRun;
  ALastInsertedSeparatorRunInfo.HistoryItem := Self;
  ALastInsertedSeparatorRunInfo.RunIndex := ARunIndex;
end;

function TdxSeparatorRunInsertedHistoryItem.CreateRun(AParagraph: TdxParagraphBase): TdxTextRunBase;
begin
  Result := TdxSeparatorTextRun.Create(AParagraph);
end;

{ TdxTextRunSplitHistoryItem }

constructor TdxTextRunSplitHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FSplitOffset := -1;
end;

procedure TdxTextRunSplitHistoryItem.RedoCore;
var
  ARuns: TdxTextRunCollection;
  ARun, ATailRun: TdxTextRun;
begin
  ARuns := PieceTable.Runs;
  ARun := TdxTextRun(ARuns[RunIndex]);
  ATailRun := ARun.CreateRun(ARun.Paragraph, ARun.StartIndex + SplitOffset, ARun.Length - SplitOffset);
  ARuns.Insert(RunIndex + 1, ATailRun);
  ARun.Length := SplitOffset;
  ATailRun.InheritStyleAndFormattingFromCore(ARun, False);
  TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(PieceTable, PieceTable, ParagraphIndex, RunIndex, SplitOffset);
  PieceTable.ApplyChanges(TdxDocumentModelChangeType.SplitRun, RunIndex, RunIndex + 1);
  DocumentModel.ResetMerging;
end;

procedure TdxTextRunSplitHistoryItem.UndoCore;
var
  ARuns: TdxTextRunCollection;
  ARun: TdxTextRunBase;
  ATailRun: TdxTextRunBase;
begin
  ARuns := PieceTable.Runs;
  ARun := ARuns[RunIndex];
  ATailRun := ARuns.Extract(RunIndex + 1);
  ARun.Length := ARun.Length + ATailRun.Length;
  try
    TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(PieceTable, PieceTable, ParagraphIndex, RunIndex, SplitOffset, ATailRun.Length);
    PieceTable.ApplyChanges(TdxDocumentModelChangeType.JoinRun, RunIndex, RunIndex);
    DocumentModel.ResetMerging;
  finally
    FreeAndNil(ATailRun);
  end;
end;

{ TdxTextRunsJoinedHistoryItem }

constructor TdxTextRunsJoinedHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FSplitOffset := -1;
end;

procedure TdxTextRunsJoinedHistoryItem.RedoCore;
var
  ARuns: TdxTextRunCollection;
  ARun, ANextRun: TdxTextRun;
  ANextRunLength: Integer;
begin
  ARuns := PieceTable.Runs;
  ARun := ARuns[RunIndex] as TdxTextRun;
  ANextRun := ARuns[RunIndex + 1] as TdxTextRun;
  FSplitOffset := ARun.Length;
  Assert(ARun.CanJoinWith(ANextRun));
  ANextRunLength := ANextRun.Length;
  ARuns.Delete(RunIndex + 1);
  ARun.Length := ARun.Length + ANextRunLength;
  TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(PieceTable, PieceTable, ParagraphIndex, RunIndex,
    FSplitOffset, ANextRunLength);
  PieceTable.ApplyChanges(TdxDocumentModelChangeType.JoinRun, RunIndex, RunIndex);
  DocumentModel.ResetMerging;
end;

procedure TdxTextRunsJoinedHistoryItem.UndoCore;
var
  ARuns: TdxTextRunCollection;
  ARun, ATailRun: TdxTextRun;
begin
  ARuns := PieceTable.Runs;
  ARun := TdxTextRun(ARuns[RunIndex]);
  ATailRun := TdxTextRun.Create(ARun.Paragraph, ARun.StartIndex + FSplitOffset, ARun.Length - FSplitOffset);
  ARuns.Insert(RunIndex + 1, ATailRun);
  ARun.Length := FSplitOffset;
  ATailRun.InheritStyleAndFormattingFromCore(ARun, False);
  TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(PieceTable, PieceTable, ParagraphIndex, RunIndex, FSplitOffset);
  PieceTable.ApplyChanges(TdxDocumentModelChangeType.SplitRun, RunIndex, RunIndex + 1);
  DocumentModel.ResetMerging;
end;

{ TdxSimpleTextRunsDeletedHistoryItem }

constructor TdxSimpleTextRunsDeletedHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FDeletedRunCount := -1;
end;

destructor TdxSimpleTextRunsDeletedHistoryItem.Destroy;
begin
  FreeAndNil(FNotificationIds);
  FreeAndNil(FDeletedRuns);
  inherited Destroy;
end;

procedure TdxSimpleTextRunsDeletedHistoryItem.AfterUndoCore;
begin
end;

procedure TdxSimpleTextRunsDeletedHistoryItem.BeforeRedoCore;
begin
end;

procedure TdxSimpleTextRunsDeletedHistoryItem.RedoCore;
var
  ACount: TdxRunIndex;
  ASaveUndoRedoInfo: Boolean;
  ARuns: TdxTextRunCollection;
  I, ALength, ANotificationId: Integer;
begin
  DocumentModel.ResetMerging;
  ARuns := PieceTable.Runs;
  ACount := RunIndex + DeletedRunCount;
  ASaveUndoRedoInfo := not DocumentModel.ModelForExport;
  if ASaveUndoRedoInfo then
  begin
    BeforeRedoCore;
    FDeletedRuns.Free;
    FDeletedRuns := TdxObjectList<TdxTextRunBase>.Create;
    FDeltaLength := 0;

    if FNotificationIds = nil then
    begin
      FNotificationIds := TdxIntegerList.Create;
      for I := 0 to DeletedRunCount - 1 do
        FNotificationIds.Add(DocumentModel.History.GetNotificationId);
    end;
  end;
  for I := ACount - 1 downto RunIndex do
  begin
    ALength := ARuns[I].Length;
    ARuns[I].BeforeRunRemoved;
    if ASaveUndoRedoInfo then
    begin
      FDeletedRuns.Add(ARuns[I]);
      FDeltaLength := FDeltaLength + ALength;
      ARuns.Extract(I);
    end
    else
      ARuns.Delete(I);
    if FNotificationIds <> nil then
      ANotificationId := FNotificationIds[I - RunIndex]
    else
      ANotificationId := TdxNotificationIdGenerator.EmptyId;
    TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(PieceTable, PieceTable, ParagraphIndex,
      I, ALength, ANotificationId);
  end;
end;

procedure TdxSimpleTextRunsDeletedHistoryItem.UndoCore;
var
  ARuns: TdxTextRunCollection;
  ACount, I: TdxRunIndex;
  ARun: TdxTextRunBase;
begin
  ARuns := PieceTable.Runs;
  ACount := RunIndex + DeletedRunCount;
  FDeletedRuns.OwnsObjects := False;
  for I := RunIndex to ACount - 1 do
  begin
    ARun := FDeletedRuns[ACount - I - 1];
    ARuns.Insert(I, ARun);
    TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(PieceTable, PieceTable,
      ParagraphIndex, I, ARun.Length, FNotificationIds[I - RunIndex]);
    ARuns[I].AfterRunInserted;
  end;
  AfterUndoCore;
end;

{ TdxParagraphsDeletedHistoryItem }

constructor TdxParagraphsDeletedHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FDeletedParagraphsCount := -1;
end;

destructor TdxParagraphsDeletedHistoryItem.Destroy;
begin
  FreeAndNil(FDeletedParagraphs);
  FreeAndNil(FNotificationIds);
  inherited Destroy;
end;

procedure TdxParagraphsDeletedHistoryItem.RedoCore;
var
  I: Integer;
  ACount: TdxParagraphIndex;
  AParagraphs: TdxSimpleParagraphCollection;
  AParagraph: TdxSimpleParagraph;
  AFirstRunIndex, ALastRunIndex: TdxRunIndex;
  ALogPosition: TdxDocumentLogPosition;
begin
  FDeletedParagraphs.Free;
  FDeletedParagraphs := TdxObjectList<TdxSimpleParagraph>.Create;
  if FNotificationIds = nil then
  begin
    FNotificationIds := TdxIntegerList.Create;
    for I := 0 to DeletedParagraphsCount - 1 do
      FNotificationIds.Add(DocumentModel.History.GetNotificationId);
  end;
  AParagraphs := PieceTable.Paragraphs;
  ACount := ParagraphIndex + DeletedParagraphsCount;
  for I := ACount - 1 downto ParagraphIndex do
  begin
    AParagraph := AParagraphs[I];
    FDeletedParagraphs.Add(AParagraph);
    AFirstRunIndex := AParagraph.FirstRunIndex;
    ALastRunIndex := AParagraph.LastRunIndex;
    ALogPosition := AParagraph.LogPosition;
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(PieceTable, PieceTable, SectionIndex, I,
      AFirstRunIndex, FNotificationIds[I - ParagraphIndex]);
    AParagraphs.RemoveAt(I);
    AParagraph.AfterRemove(AFirstRunIndex, ALastRunIndex, ALogPosition);
    DocumentModel.NotifyNumberingListParagraphRemoved(AParagraph.GetOwnNumberingListIndex);
  end;
end;

procedure TdxParagraphsDeletedHistoryItem.UndoCore;
var
  AParagraphs: TdxSimpleParagraphCollection;
  ACount, I: TdxParagraphIndex;
  AParagraph: TdxSimpleParagraph;
begin
  AParagraphs := PieceTable.Paragraphs;
  ACount := ParagraphIndex + DeletedParagraphsCount;
  FDeletedParagraphs.OwnsObjects := False;
  for I := ParagraphIndex to ACount - 1 do
  begin
    AParagraph := FDeletedParagraphs[ACount - I - 1];
    AParagraphs.Insert(I, AParagraph);
    AParagraph.AfterUndoRemove;
    TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(PieceTable, PieceTable, SectionIndex,
      I, AParagraphs[I].FirstRunIndex, Cell, False, I, FNotificationIds[I - ParagraphIndex]);
    DocumentModel.NotifyNumberingListParagraphAdded(AParagraph.GetOwnNumberingListIndex);
  end;
end;

function TdxParagraphsDeletedHistoryItem.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := TdxSimpleDocumentModel(inherited DocumentModel);
end;

{ TdxMergeParagraphsHistoryItem }

destructor TdxMergeParagraphsHistoryItem.Destroy;
begin
  if FOwnsParagraph then
    FreeAndNil(FEndParagraph);
  inherited Destroy;
end;

procedure TdxMergeParagraphsHistoryItem.RedoCore;
var
  ARuns: TdxTextRunCollection;
  I: TdxRunIndex;
  ASectionIndex: TdxSectionIndex;
  AFirstRunIndex, ALastRunIndex: TdxRunIndex;
  ALogPosition: TdxDocumentLogPosition;
begin
  ARuns := PieceTable.Runs;
  FIndex := EndParagraph.LastRunIndex + 1 - EndParagraph.FirstRunIndex;
  for I := EndParagraph.FirstRunIndex to EndParagraph.LastRunIndex do
  begin
    ARuns[I].Paragraph := StartParagraph;
    StartParagraph.Length := StartParagraph.Length + ARuns[I].Length;
  end;
  if not UseFirstParagraphStyle then
  begin
    FParagraphStyleIndex := StartParagraph.ParagraphStyleIndex;
    StartParagraph.SetParagraphStyleIndexCore(EndParagraph.ParagraphStyleIndex);
    for I := StartParagraph.FirstRunIndex to StartParagraph.LastRunIndex do
      ARuns[I].ResetMergedCharacterFormattingIndex;
  end;
  StartParagraph.RelativeLastRunIndex := EndParagraph.LastRunIndex;
  ASectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(EndParagraph.Index);
  if FNotificationId = TdxNotificationIdGenerator.EmptyId then
    FNotificationId := DocumentModel.History.GetNotificationId;
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(PieceTable, PieceTable,
    ASectionIndex, EndParagraph.Index, EndParagraph.FirstRunIndex, FNotificationId);
  FEndParagraphIndex := EndParagraph.Index;
  AFirstRunIndex := EndParagraph.FirstRunIndex;
  ALastRunIndex := EndParagraph.LastRunIndex;
  ALogPosition := EndParagraph.LogPosition;
  PieceTable.Paragraphs.RemoveAt(FEndParagraphIndex);
  FOwnsParagraph := True;
  EndParagraph.AfterRemove(AFirstRunIndex, ALastRunIndex, ALogPosition);
end;

procedure TdxMergeParagraphsHistoryItem.UndoCore;
var
  ARuns: TdxTextRunCollection;
  I: TdxRunIndex;
  ASectionIndex: TdxSectionIndex;
begin
  ARuns := PieceTable.Runs;
  FOwnsParagraph := False;
  PieceTable.Paragraphs.Insert(FEndParagraphIndex, EndParagraph);
  EndParagraph.AfterUndoRemove;
  ASectionIndex := PieceTable.LookupSectionIndexByParagraphIndex(EndParagraph.Index - 1);
  TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(PieceTable, PieceTable, ASectionIndex, EndParagraph.Index, EndParagraph.FirstRunIndex, Cell, true, EndParagraph.Index, FNotificationId);
  if not UseFirstParagraphStyle then
  begin
    EndParagraph.SetParagraphStyleIndexCore(StartParagraph.ParagraphStyleIndex);
    StartParagraph.SetParagraphStyleIndexCore(FParagraphStyleIndex);
    for I := StartParagraph.FirstRunIndex to StartParagraph.LastRunIndex do
      ARuns[I].ResetMergedCharacterFormattingIndex;
  end;
  I := StartParagraph.LastRunIndex;
  while I > StartParagraph.LastRunIndex - FIndex do
  begin
    ARuns[I].Paragraph := EndParagraph;
    StartParagraph.Length := StartParagraph.Length - ARuns[I].Length;
    Dec(I);
  end;
  StartParagraph.RelativeLastRunIndex := StartParagraph.LastRunIndex - FIndex;
end;

end.
