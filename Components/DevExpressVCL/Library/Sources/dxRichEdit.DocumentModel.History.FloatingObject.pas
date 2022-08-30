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

unit dxRichEdit.DocumentModel.History.FloatingObject;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.History.Simple,
  dxRichEdit.DocumentModel.History.Paragraph;

type
  { TdxFloatingObjectAnchorRunMovedHistoryItem }

  TdxFloatingObjectAnchorRunMovedHistoryItem = class(TdxTextRunInsertedBaseHistoryItem)
  strict private
    FNewRunIndex: TdxRunIndex;
    FNewParagraphIndex: TdxParagraphIndex;
    FNotificationId1: Integer;
    FNotificationId2: Integer;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    property NewRunIndex: TdxRunIndex read FNewRunIndex write FNewRunIndex;
    property NewParagraphIndex: TdxParagraphIndex read FNewParagraphIndex write FNewParagraphIndex;
  end;

implementation

uses
  dxRichEdit.DocumentModel.Intervals.Core;

{ TdxFloatingObjectAnchorRunMovedHistoryItem }

procedure TdxFloatingObjectAnchorRunMovedHistoryItem.RedoCore;
var
  ARun: TdxTextRunBase;
  AIndex: TdxRunIndex;
begin
  ARun := PieceTable.Runs.Extract(RunIndex);
  DocumentModel.ResetMerging;
  if FNotificationId1 = TdxNotificationIdGenerator.EmptyId then
  begin
    FNotificationId1 := DocumentModel.History.GetNotificationId;
    FNotificationId2 := DocumentModel.History.GetNotificationId;
  end;
  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(PieceTable, PieceTable, ParagraphIndex, RunIndex, 1, FNotificationId1);
  AIndex := NewRunIndex;
  if AIndex >= RunIndex then
    Dec(AIndex);
  PieceTable.Runs.Insert(AIndex, ARun);
  ARun.Paragraph := PieceTable.Paragraphs[NewParagraphIndex];
  DocumentModel.ResetMerging;
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(PieceTable, PieceTable, NewParagraphIndex, AIndex, 1, FNotificationId2);
end;

procedure TdxFloatingObjectAnchorRunMovedHistoryItem.UndoCore;
var
  AIndex: TdxRunIndex;
  ARun: TdxTextRunBase;
begin
  AIndex := NewRunIndex;
  if AIndex >= RunIndex then
    Dec(AIndex);
  ARun := PieceTable.Runs.Extract(AIndex);
  DocumentModel.ResetMerging;
  TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(PieceTable, PieceTable, NewParagraphIndex, AIndex, 1, FNotificationId2);
  PieceTable.Runs.Insert(RunIndex, ARun);
  ARun.Paragraph := PieceTable.Paragraphs[ParagraphIndex];
  DocumentModel.ResetMerging;
  TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(PieceTable, PieceTable, ParagraphIndex, RunIndex, 1, FNotificationId1);
end;

end.
