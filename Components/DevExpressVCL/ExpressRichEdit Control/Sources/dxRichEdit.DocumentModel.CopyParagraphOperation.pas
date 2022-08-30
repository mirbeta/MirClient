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

unit dxRichEdit.DocumentModel.CopyParagraphOperation;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.CopyManager.Simple,
  dxRichEdit.DocumentModel.CopyManager,
  dxRichEdit.DocumentModel.PieceTable;

type
  { TdxCopyParagraphOperation }

  TdxCopyParagraphOperation = class(TdxSelectionBasedOperation)
  strict private
    FCopyManager: TdxDocumentModelCopyManager;
    FTransactionItemCountBeforeExecute: Integer;
    FIsMergingTableCell: Boolean;
    FHistoryDisabled: Boolean;
    FOldUndoValue: TdxDocumentCapability;
    function GetTargetModel: TdxDocumentModel;
    function GetSourceModel: TdxDocumentModel;
    function GetTargetPieceTable: TdxPieceTable;
    function GetSourcePieceTable: TdxPieceTable;
    function GetTargetPosition: PdxDocumentModelPosition;
  private
    function GetTableCopyManager: TdxTableCopyHelper;
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
    function ShouldProcessContentInSameParent(AInfo: TdxRunInfo): Boolean; override;
    function ShouldProcessRunParent(AInfo: TdxRunInfo): Boolean; override;
    procedure ProcessRunParent(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean); override;
    procedure ProcessContentInsideParent(AInfo: TdxRunInfo; AAllowMergeWithNextParagraph, ADocumentLastParagraphSelected: Boolean); override;
    function ProcessHead(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer; override;
    function ProcessMiddle(AInfo: TdxRunInfo; AParagraphCount: Integer; ADocumentLastParagraphSelected: Boolean): Boolean; override;
    function ProcessTail(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer; override;

    procedure DeleteLastParagraphInTableCell; virtual;
    function NeedInsertParagraphBeforeTable(AInfo: TdxRunInfo): Boolean;
    procedure ProcessParagraph(ARunIndex: TdxRunIndex; AParagraphIndex: TdxParagraphIndex);
    procedure ProcessRuns(AFirstRunIndex, ALastRunIndex: TdxRunIndex);
    procedure ProcessMiddleCore(AStartIndex, AEndParagraphIndex: TdxParagraphIndex );
    procedure InsertParagraphBeforeTable;
    procedure InsertSpaceInsteadParagraph;
  public
    constructor Create(ACopyManager: TdxDocumentModelCopyManager);

    function ExecuteCore(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Boolean; override;

    property IsMergingTableCell: Boolean read FIsMergingTableCell write FIsMergingTableCell;
    property CopyManager: TdxDocumentModelCopyManager read FCopyManager;
    property TargetModel: TdxDocumentModel read GetTargetModel;
    property SourceModel: TdxDocumentModel read GetSourceModel;
    property TargetPieceTable: TdxPieceTable read GetTargetPieceTable;
    property SourcePieceTable: TdxPieceTable read GetSourcePieceTable;
    property TargetPosition: PdxDocumentModelPosition read GetTargetPosition;

    property TableCopyManager: TdxTableCopyHelper read GetTableCopyManager;
  end;

implementation

uses
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.Options;

{ TdxCopyParagraphOperation }

constructor TdxCopyParagraphOperation.Create(ACopyManager: TdxDocumentModelCopyManager);
begin
  inherited Create(ACopyManager.SourcePieceTable);
  FCopyManager := ACopyManager;
end;

procedure TdxCopyParagraphOperation.DeleteLastParagraphInTableCell;
var
  AOperation: TdxDeleteParagraphOperation;
begin
  AOperation := TdxDeleteParagraphOperation.Create(TargetPieceTable);
  try
    AOperation.AllowedDeleteLastParagraphInTableCell := True;
    AOperation.Execute(TargetPosition.LogPosition, 1, False);
  finally
    AOperation.Free;
  end;
end;

function TdxCopyParagraphOperation.ExecuteCore(AInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean): Boolean;
var
  ATransaction: TdxHistoryTransaction;
begin
  ATransaction := TdxHistoryTransaction.Create(TargetModel.History);
  try
    Result := inherited ExecuteCore(AInfo, False);
  finally
    ATransaction.Free;
  end;
end;

procedure TdxCopyParagraphOperation.AfterExecute;
var
  ATransaction: TdxCompositeHistoryItem;
  AItem: TdxHistoryItem;
  I: Integer;
begin
  ATransaction := SourceModel.History.Transaction;
  for I := ATransaction.Items.Count - 1 downto FTransactionItemCountBeforeExecute do
  begin
    AItem := ATransaction[I];
    if AItem.PieceTable = SourcePieceTable then
    begin
      AItem.Undo;
      ATransaction.Items.Delete(I);
    end;
  end;
  if FHistoryDisabled then
  begin
    SourceModel.History.EndTransaction;
    SourceModel.DocumentCapabilities.Undo := FOldUndoValue;
  end;
  DocumentModel.EndSuppressPerformLayout;
end;

procedure TdxCopyParagraphOperation.BeforeExecute;
var
  ATransaction: TdxCompositeHistoryItem;
begin
  DocumentModel.BeginSuppressPerformLayout;
  FHistoryDisabled := SourceModel.History is TdxDisabledHistory;
  if FHistoryDisabled then
  begin
    FOldUndoValue := SourceModel.DocumentCapabilities.Undo;
    SourceModel.DocumentCapabilities.Undo := TdxDocumentCapability.Enabled;
    SourceModel.History.BeginTransaction;
  end;
  ATransaction := SourceModel.History.Transaction;
  FTransactionItemCountBeforeExecute := ATransaction.Items.Count;
end;

function TdxCopyParagraphOperation.ShouldProcessContentInSameParent(AInfo: TdxRunInfo): Boolean;
var
  AStart, AEnd: TdxDocumentModelPosition;
  AParagraphs: TdxParagraphCollection;
begin
  AStart := AInfo.Start;
  AEnd := AInfo.&End;
  AParagraphs := SourcePieceTable.Paragraphs;
  Result := (AStart.ParagraphIndex = AEnd.ParagraphIndex) and
    ((AStart.RunIndex <> AParagraphs[AStart.ParagraphIndex].FirstRunIndex) or
    (AEnd.RunIndex <> AParagraphs[AStart.ParagraphIndex].LastRunIndex));
end;

function TdxCopyParagraphOperation.ShouldProcessRunParent(AInfo: TdxRunInfo): Boolean;
var
  AEndRun: TdxTextRunBase;
begin
  AEndRun := SourcePieceTable.Runs[AInfo.&End.RunIndex];
  Result := AEndRun is TdxParagraphRun;
end;

procedure TdxCopyParagraphOperation.ProcessRunParent(AInfo: TdxRunInfo;
  ADocumentLastParagraphSelected: Boolean);
begin
  ProcessParagraph(AInfo.Start.RunIndex, AInfo.&End.ParagraphIndex);
end;

procedure TdxCopyParagraphOperation.ProcessContentInsideParent(AInfo: TdxRunInfo;
  AAllowMergeWithNextParagraph, ADocumentLastParagraphSelected: Boolean);
var
  AParagraph: TdxParagraph;
  AStartRunIndex: TdxRunIndex;
begin
  AParagraph := SourcePieceTable.Paragraphs[AInfo.Start.ParagraphIndex];
  AStartRunIndex := AInfo.Start.RunIndex;
  ProcessRuns(AStartRunIndex, AInfo.&End.RunIndex);

  if (AParagraph.IsInNonStyleList or (AParagraph.GetOwnNumberingListIndex = NumberingListIndexNoNumberingList)) and
      (AStartRunIndex = AParagraph.FirstRunIndex) and
      (CopyManager.ParagraphNumerationCopyOptions = TdxParagraphNumerationCopyOptions.CopyAlways) then
    AParagraph.CopyNumberingListProperties(CopyManager.TargetPieceTable.Paragraphs[CopyManager.TargetPosition.ParagraphIndex]);
end;

function TdxCopyParagraphOperation.ProcessHead(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer;
var
  AParagraph: TdxParagraph;
  AParagraphCount: Integer;
begin
  AParagraph := SourcePieceTable.Paragraphs[AInfo.Start.ParagraphIndex];
  AParagraphCount := AInfo.&End.ParagraphIndex - AParagraph.Index + 1;
  if AParagraph.FirstRunIndex = AInfo.Start.RunIndex then
    Result := AParagraphCount
  else
  begin
    ProcessParagraph(AInfo.Start.RunIndex, AParagraph.Index);
    Result := AParagraphCount - 1;
  end;
end;

function TdxCopyParagraphOperation.ProcessMiddle(AInfo: TdxRunInfo;
  AParagraphCount: Integer; ADocumentLastParagraphSelected: Boolean): Boolean;
var
  AEndParagraphIndex, AStartIndex: TdxParagraphIndex;
  ALastRunIndex: TdxRunIndex;
begin
  if NeedInsertParagraphBeforeTable(AInfo) or IsMergingTableCell then
    InsertParagraphBeforeTable;

  AEndParagraphIndex := AInfo.&End.ParagraphIndex;
  AStartIndex := AEndParagraphIndex - AParagraphCount + 1;
  ALastRunIndex := SourcePieceTable.Paragraphs[AEndParagraphIndex].LastRunIndex;
  if AInfo.&End.RunIndex = ALastRunIndex then
  begin
    ProcessMiddleCore(AStartIndex, AEndParagraphIndex);
    if IsMergingTableCell then
      DeleteLastParagraphInTableCell;
    Result := False;
  end
  else
  begin
    ProcessMiddleCore(AStartIndex, AEndParagraphIndex - 1);
    Result := True;
  end;
end;

function TdxCopyParagraphOperation.ProcessTail(AInfo: TdxRunInfo; ADocumentLastParagraphSelected: Boolean): Integer;
var
  AParagraph, ATargetParagraph: TdxParagraph;
  ATargetPieceTable: TdxPieceTable;
  ATargetPosition: TdxDocumentModelPosition;
  AOptions: TdxDocumentCapabilitiesOptions;
  AFirstRunIndex: TdxRunIndex;
begin
  AParagraph := SourcePieceTable.Paragraphs[AInfo.&End.ParagraphIndex];

  ATargetPieceTable := CopyManager.TargetPieceTable;
  ATargetPosition := CopyManager.TargetPosition;
  ATargetParagraph := ATargetPieceTable.Paragraphs[ATargetPosition.ParagraphIndex];

  AOptions := ATargetParagraph.DocumentModel.DocumentCapabilities;
  if AOptions.ParagraphStyleAllowed then
    ATargetParagraph.ParagraphStyleIndex := AParagraph.ParagraphStyle.Copy(CopyManager.TargetModel);
  ATargetParagraph.ParagraphStyle.ParagraphProperties.Alignment := TdxParagraphAlignment.Left;

  AFirstRunIndex := AParagraph.FirstRunIndex;

  ProcessRuns(AFirstRunIndex, AInfo.&End.RunIndex);

  if ((AParagraph.IsInNonStyleList) or (AParagraph.GetOwnNumberingListIndex = NumberingListIndexNoNumberingList)) and
    (CopyManager.ParagraphNumerationCopyOptions = TdxParagraphNumerationCopyOptions.CopyAlways) then
    AParagraph.CopyNumberingListProperties(ATargetParagraph);
  Result := 0;
end;


function TdxCopyParagraphOperation.NeedInsertParagraphBeforeTable(AInfo: TdxRunInfo): Boolean;
var
  ASourceStartCell: TdxTableCell;
  ATargetParagraphLogPosition: TdxDocumentLogPosition;
begin
  ASourceStartCell := SourcePieceTable.Paragraphs[AInfo.Start.ParagraphIndex].GetCell;
  ATargetParagraphLogPosition := TargetPieceTable.Paragraphs[TargetPosition.ParagraphIndex].LogPosition;
  Result := (ASourceStartCell <> nil) and (ATargetParagraphLogPosition <> TargetPosition.LogPosition);
end;

procedure TdxCopyParagraphOperation.ProcessParagraph(ARunIndex: TdxRunIndex;
  AParagraphIndex: TdxParagraphIndex);
var
  ASourceParagraph, ATargetParagraph: TdxParagraph;
  ATargetRun, ASourceRun: TdxTextRunBase;
begin
  ASourceParagraph := SourcePieceTable.Paragraphs[AParagraphIndex];
  ATargetParagraph := ASourceParagraph.Copy(CopyManager);
  ATargetRun := TargetPieceTable.Runs[ATargetParagraph.LastRunIndex];
  ASourceRun := SourcePieceTable.Runs[ASourceParagraph.LastRunIndex];

  if TargetModel.DocumentCapabilities.CharacterFormattingAllowed then
    ATargetRun.CharacterProperties.CopyFrom(ASourceRun.CharacterProperties.Info);

  if TargetModel.DocumentCapabilities.CharacterStyleAllowed then
    ATargetRun.CharacterStyleIndex := ASourceRun.CharacterStyle.Copy(CopyManager.TargetModel);

  ProcessRuns(ARunIndex, ASourceParagraph.LastRunIndex - 1);
  if TargetPieceTable.DocumentModel.DocumentCapabilities.ParagraphsAllowed then
    CopyManager.OnTargetParagraphInserted(ASourceParagraph, ATargetParagraph, ASourceRun, ATargetRun)
  else
    InsertSpaceInsteadParagraph;
end;

procedure TdxCopyParagraphOperation.ProcessRuns(AFirstRunIndex, ALastRunIndex: TdxRunIndex);
var
  I: Integer;
  ASourceRun, ATargetRun: TdxTextRunBase;
begin
  TargetModel.ResetMerging;
  for I := AFirstRunIndex to ALastRunIndex do
  begin
    ASourceRun := SourcePieceTable.Runs[I];
    ATargetRun := ASourceRun.Copy(CopyManager);
    TargetModel.ResetMerging;
    CopyManager.OnTargetRunInserted(ASourceRun, ATargetRun);
  end;
end;

procedure TdxCopyParagraphOperation.ProcessMiddleCore(AStartIndex, AEndParagraphIndex: TdxParagraphIndex );
var
  AParagraphs: TdxParagraphCollection;
  I: Integer;
begin
  AParagraphs := SourcePieceTable.Paragraphs;
  for I := AStartIndex to AEndParagraphIndex do
    ProcessParagraph(AParagraphs[i].FirstRunIndex, I);
end;

procedure TdxCopyParagraphOperation.InsertParagraphBeforeTable;
begin
  TargetPieceTable.InsertParagraph(TargetPosition.LogPosition);
  CopyManager.TableCopyHelper.TargetStartParagraphIndex := CopyManager.TableCopyHelper.TargetStartParagraphIndex + 1;
  CopyManager.ParagraphWasInsertedBeforeTable := True;
  TargetModel.ResetMerging;
  TargetPosition.ParagraphIndex := TargetPosition.ParagraphIndex + 1;
  TargetPosition.LogPosition := TargetPosition.LogPosition + 1;
  TargetPosition.RunStartLogPosition := TargetPosition.RunStartLogPosition + 1;
  TargetPosition.RunIndex := TargetPosition.RunIndex + 1;
end;

procedure TdxCopyParagraphOperation.InsertSpaceInsteadParagraph;
begin
  TargetPieceTable.InsertPlainText(TargetPosition.LogPosition, ' ');
  TargetModel.ResetMerging;
  TargetPosition.LogPosition := TargetPosition.LogPosition + 1;
  TargetPosition.RunStartLogPosition := TargetPosition.RunStartLogPosition + 1;
  TargetPosition.RunIndex := TargetPosition.RunIndex + 1;
end;

function TdxCopyParagraphOperation.GetTableCopyManager: TdxTableCopyHelper;
begin
  Result := CopyManager.TableCopyHelper;
end;

function TdxCopyParagraphOperation.GetTargetModel: TdxDocumentModel;
begin
  Result := FCopyManager.TargetModel;
end;

function TdxCopyParagraphOperation.GetSourceModel: TdxDocumentModel;
begin
  Result := FCopyManager.SourceModel;
end;

function TdxCopyParagraphOperation.GetTargetPieceTable: TdxPieceTable;
begin
  Result := FCopyManager.TargetPieceTable;
end;

function TdxCopyParagraphOperation.GetSourcePieceTable: TdxPieceTable;
begin
  Result := FCopyManager.SourcePieceTable;
end;

function TdxCopyParagraphOperation.GetTargetPosition: PdxDocumentModelPosition;
begin
  Result := @FCopyManager.TargetPosition;
end;

end.
