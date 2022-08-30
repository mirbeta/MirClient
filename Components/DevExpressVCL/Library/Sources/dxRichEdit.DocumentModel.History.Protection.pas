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

unit dxRichEdit.DocumentModel.History.Protection;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.Utils.Types,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.History.BookmarkHistory;

type
  { TdxInsertRangePermissionHistoryItem }

  TdxInsertRangePermissionHistoryItem = class(TdxInsertDocumentIntervalHistoryItem)
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  end;

  { TdxRemoveRangePermissionHistoryItem }

  TdxRemoveRangePermissionHistoryItem = class(TdxInsertDocumentIntervalHistoryItem)
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  end;

  { TdxDeleteRangePermissionHistoryItem }

  TdxDeleteRangePermissionHistoryItem = class(TdxBookmarkBaseHistoryItem)
  strict private
    FDeletedRangePermission: TdxRangePermission;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
  public
    destructor Destroy; override;
    property DeletedRangePermission: TdxRangePermission read FDeletedRangePermission write FDeletedRangePermission;
  end;

implementation

uses
  Math;

{ TdxInsertRangePermissionHistoryItem }

procedure TdxInsertRangePermissionHistoryItem.RedoCore;
var
  AStart, AEnd: TdxDocumentLogPosition;
  ARunInfo: TdxRunInfo;
  AStartRunIndex, AEndRunIndex: TdxRunIndex;
begin
  AStart := Min(Position, PieceTable.DocumentEndLogPosition);
  AEnd := Min(Position + Length, PieceTable.DocumentEndLogPosition);
  ARunInfo := PieceTable.ApplyDocumentPermissionCore(AStart, AEnd, IndexToInsert);
  if ARunInfo <> nil then
    AStartRunIndex := ARunInfo.NormalizedStart.RunIndex
  else
    AStartRunIndex := dxRunIndexDontCare;
  if ARunInfo <> nil then
    AEndRunIndex := ARunInfo.NormalizedEnd.RunIndex
  else
    AEndRunIndex := dxRunIndexDontCare;
  PieceTable.ApplyChangesCore([
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.Redraw], AStartRunIndex, AEndRunIndex);
end;

procedure TdxInsertRangePermissionHistoryItem.UndoCore;
var
  AStart, AEnd: TdxDocumentLogPosition;
  ARunInfo: TdxRunInfo;
  AStartRunIndex, AEndRunIndex: TdxRunIndex;
begin
  AStart := Min(Position, PieceTable.DocumentEndLogPosition);
  AEnd := Min(Position + Length, PieceTable.DocumentEndLogPosition);
  ARunInfo := PieceTable.RemoveDocumentPermissionCore(AStart, AEnd, IndexToInsert);
  if ARunInfo = nil then
    Exit;
  try
    AStartRunIndex := ARunInfo.NormalizedStart.RunIndex;
    AEndRunIndex := ARunInfo.NormalizedEnd.RunIndex;
    PieceTable.ApplyChangesCore([
      TdxDocumentModelChangeAction.RaiseContentChanged,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.Redraw], AStartRunIndex, AEndRunIndex);
  finally
    ARunInfo.Free;
  end;
end;

{ TdxRemoveRangePermissionHistoryItem }

procedure TdxRemoveRangePermissionHistoryItem.RedoCore;
var
  AStart, AEnd: TdxDocumentLogPosition;
  ARunInfo: TdxRunInfo;
  AStartRunIndex, AEndRunIndex: TdxRunIndex;
begin
  AStart := Min(Position, PieceTable.DocumentEndLogPosition);
  AEnd := Min(Position + Length, PieceTable.DocumentEndLogPosition);
  ARunInfo := PieceTable.RemoveDocumentPermissionCore(AStart, AEnd, IndexToInsert);
  try
    if ARunInfo <> nil then
    begin
      AStartRunIndex := ARunInfo.NormalizedStart.RunIndex;
      AEndRunIndex := ARunInfo.NormalizedEnd.RunIndex;
    end
    else
    begin
      AStartRunIndex := dxRunIndexDontCare;
      AEndRunIndex := dxRunIndexDontCare;
    end;
    PieceTable.ApplyChangesCore([
      TdxDocumentModelChangeAction.RaiseContentChanged,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.Redraw], AStartRunIndex, AEndRunIndex);
  finally
    ARunInfo.Free;
  end;
end;

procedure TdxRemoveRangePermissionHistoryItem.UndoCore;
var
  AStart, AEnd: TdxDocumentLogPosition;
  ARunInfo: TdxRunInfo;
  AStartRunIndex, AEndRunIndex: TdxRunIndex;
begin
  AStart := Min(Position, PieceTable.DocumentEndLogPosition);
  AEnd := Min(Position + Length, PieceTable.DocumentEndLogPosition);
  ARunInfo := PieceTable.ApplyDocumentPermissionCore(AStart, AEnd, IndexToInsert);
  if ARunInfo <> nil then
    AStartRunIndex := ARunInfo.NormalizedStart.RunIndex
  else
    AStartRunIndex := dxRunIndexDontCare;
  if ARunInfo <> nil then
    AEndRunIndex := ARunInfo.NormalizedEnd.RunIndex
  else
    AEndRunIndex := dxRunIndexDontCare;
  PieceTable.ApplyChangesCore([
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.Redraw], AStartRunIndex, AEndRunIndex);
end;

{ TdxDeleteRangePermissionHistoryItem }

destructor TdxDeleteRangePermissionHistoryItem.Destroy;
begin
  FDeletedRangePermission.Free;
  inherited Destroy;
end;

procedure TdxDeleteRangePermissionHistoryItem.RedoCore;
var
  ARangePermissions: TdxRangePermissionCollection;
  ACount, I: Integer;
  APermission: TdxRangePermission;
begin
  ARangePermissions := PieceTable.RangePermissions;
  ACount := ARangePermissions.Count;
  for I := 0 to ACount - 1 do
  begin
    APermission := ARangePermissions[I];
    if (APermission.Properties.Index = FDeletedRangePermission.Properties.Index) and
       (APermission.NormalizedStart = FDeletedRangePermission.NormalizedStart) and
       (APermission.NormalizedEnd = FDeletedRangePermission.NormalizedEnd) then
    begin
      ARangePermissions.Delete(I);
      Break;
    end;
  end;

  Assert(FDeletedRangePermission <> nil);
  PieceTable.ApplyChangesCore([
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.Redraw],
    FDeletedRangePermission.Interval.NormalizedStart.RunIndex, FDeletedRangePermission.Interval.NormalizedEnd.RunIndex);
end;

procedure TdxDeleteRangePermissionHistoryItem.UndoCore;
begin
  Assert(FDeletedRangePermission <> nil);
  PieceTable.RangePermissions.Add(FDeletedRangePermission.Clone);
  PieceTable.ApplyChangesCore([
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.Redraw],
    FDeletedRangePermission.Interval.NormalizedStart.RunIndex, FDeletedRangePermission.Interval.NormalizedEnd.RunIndex);
end;

end.
