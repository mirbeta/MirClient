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

unit dxRichEdit.DocumentModel.Intervals.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.DocumentModel.Tables.Core,
  dxRichEdit.DocumentModel.Core;

type
  { IdxDocumentModelStructureChangedListener }

  IdxDocumentModelStructureChangedListener = interface
  ['{DB6E685A-82D6-4C79-9D81-35B71035350C}']
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer);
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ASplitOffset: Integer);
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer);
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ADeltaRunLength: Integer);
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
  end;

  { TdxCustomDocumentInterval }

  TdxCustomDocumentInterval = class abstract(TcxIUnknownObject,
    IdxDocumentModelStructureChangedListener)
  strict private
    FPieceTable: TdxCustomPieceTable;
    FInterval: TdxRunInfo;
    FNotChangedStartIds: TdxIntegerList;
    FNotChangedEndIds: TdxIntegerList;
    function CreateRunInfo(APieceTable: TdxCustomPieceTable): TdxRunInfo;
    function GetDocumentModel: TdxCustomDocumentModel;
    function GetLength: Integer;
    function GetNormalizedStart: TdxDocumentLogPosition;
    function GetNormalizedEnd: TdxDocumentLogPosition;
    procedure SetNotChangedEndIds(const Value: TdxIntegerList);
    procedure SetNotChangedStartIds(const Value: TdxIntegerList);
  protected
    function GetStart: TdxDocumentLogPosition; virtual;
    function GetEnd: TdxDocumentLogPosition; virtual;
    procedure SetStartCore(AValue: TdxDocumentLogPosition); overload;
    procedure SetEndCore(const AValue: TdxDocumentLogPosition); overload;
    function SetPositionCore(const APos, AValue: TdxDocumentModelPosition): Boolean;
    procedure OnChanged(AStartChanged, AEndChanged: Boolean); virtual;
    procedure UpdateStartPosition; virtual;
  {$REGION 'IdxDocumentModelStructureChangedListener'}
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer);
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ASplitOffset: Integer);
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer);
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ADeltaRunLength: Integer);
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
  {$ENDREGION}

    procedure ParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); virtual;
    procedure ParagraphRemoved(ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); virtual;
    procedure ParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); virtual;
    procedure RunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength,
      AHistoryNotificationId: Integer); overload; virtual;
    procedure RunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex;
      ASplitOffset, ATailRunLength: Integer); virtual;
    procedure RunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer); overload; virtual;
    procedure RunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); virtual;
    procedure RunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); virtual;
    procedure RunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); virtual;

    property NotChangedStartIds: TdxIntegerList read FNotChangedStartIds write SetNotChangedStartIds;
    property NotChangedEndIds: TdxIntegerList read FNotChangedEndIds write SetNotChangedEndIds;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); virtual;
    destructor Destroy; override;

    procedure UpdateEndPosition; virtual;
    function Contains(AStart, AEnd: TdxDocumentLogPosition): Boolean; overload;
    function Contains(AInterval: TdxCustomDocumentInterval): Boolean; overload;
    function IntersectsWith(AStart, AEnd: TdxDocumentLogPosition): Boolean; overload;
    function IntersectsWith(AInterval: TdxCustomDocumentInterval): Boolean; overload;
    function IntersectsWithExcludingBounds(AInterval: TdxCustomDocumentInterval): Boolean;
    procedure OnChangedCore; virtual; abstract;
    procedure SetStartCore(const APos: TdxDocumentModelPosition); overload;
    procedure SetEndCore(const APos: TdxDocumentModelPosition); overload;

    procedure FieldInserted(AFieldIndex: Integer);
    procedure FieldRemoved(AFieldIndex: Integer);

    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
    property PieceTable: TdxCustomPieceTable read FPieceTable;
    property Interval: TdxRunInfo read FInterval;
    property Length: Integer read GetLength;
    property Start: TdxDocumentLogPosition read GetStart;
    property &End: TdxDocumentLogPosition read GetEnd;
    property NormalizedStart: TdxDocumentLogPosition read GetNormalizedStart;
    property NormalizedEnd: TdxDocumentLogPosition read GetNormalizedEnd;
  end;

  { TdxDocumentModelStructureChangedNotifier }

  TdxDocumentModelStructureChangedNotifier = class
  public
    class procedure NotifyParagraphInserted(const AListener: IdxDocumentModelStructureChangedListener;
      APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex;
      AHistoryNotificationId: Integer); static;
    class procedure NotifyParagraphRemoved(const AListener: IdxDocumentModelStructureChangedListener;
      APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); static;
    class procedure NotifyParagraphMerged(const AListener: IdxDocumentModelStructureChangedListener;
      APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); static;
    class procedure NotifyRunInserted(const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable;
      AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); static;
    class procedure NotifyRunRemoved(const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer); static;
    class procedure NotifyRunSplit(const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); static;
    class procedure NotifyRunJoined(const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable;
      AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer); static;
    class procedure NotifyRunMerged(const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); static;
    class procedure NotifyRunUnmerged(const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); static;
    class procedure NotifyFieldRemoved(const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable;
      AFieldIndex: Integer); static;
    class procedure NotifyFieldInserted(const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable;
      AFieldIndex: Integer); static;
    class procedure NotifyBeginMultipleRunSplit(const AListener: IdxDocumentModelStructureChangedListener;
      APieceTable: TdxCustomPieceTable); static;
    class procedure NotifyEndMultipleRunSplit(const AListener: IdxDocumentModelStructureChangedListener;
      APieceTable: TdxCustomPieceTable); static;
  end;

implementation

constructor TdxCustomDocumentInterval.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  Assert(APieceTable <> nil, 'APieceTable = nil');
  FPieceTable := APieceTable;
  FInterval := CreateRunInfo(APieceTable);
end;

destructor TdxCustomDocumentInterval.Destroy;
begin
  FreeAndNil(FInterval);
  FreeAndNil(FNotChangedStartIds);
  FreeAndNil(FNotChangedEndIds);
  inherited Destroy;
end;

function TdxCustomDocumentInterval.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := FPieceTable.DocumentModel;
end;

function TdxCustomDocumentInterval.CreateRunInfo(APieceTable: TdxCustomPieceTable): TdxRunInfo;
begin
  Result := TdxRunInfo.Create(APieceTable);
end;

function TdxCustomDocumentInterval.GetEnd: TdxDocumentLogPosition;
begin
  Result := FInterval.&End.LogPosition;
end;

function TdxCustomDocumentInterval.GetLength: Integer;
begin
  Result := Abs(&End - Start);
end;

function TdxCustomDocumentInterval.GetNormalizedEnd: TdxDocumentLogPosition;
begin
  Result := Interval.NormalizedEnd.LogPosition;
end;

function TdxCustomDocumentInterval.GetNormalizedStart: TdxDocumentLogPosition;
begin
  Result := Interval.NormalizedStart.LogPosition;
end;

function TdxCustomDocumentInterval.GetStart: TdxDocumentLogPosition;
begin
  Result := FInterval.Start.LogPosition;
end;

procedure TdxCustomDocumentInterval.SetEndCore(const AValue: TdxDocumentLogPosition);
begin
  if &End <> AValue then
  begin
    FInterval.&End.LogPosition := AValue;
    UpdateEndPosition;
    OnChanged(False, True);
  end;
end;

procedure TdxCustomDocumentInterval.SetEndCore(const APos: TdxDocumentModelPosition);
begin
  if SetPositionCore(Interval.&End, APos) then
    OnChanged(False, True);
end;

procedure TdxCustomDocumentInterval.FieldInserted(AFieldIndex: Integer);
begin
end;

procedure TdxCustomDocumentInterval.FieldRemoved(AFieldIndex: Integer);
begin
end;

procedure TdxCustomDocumentInterval.ParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
  AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxCustomDocumentInterval.ParagraphRemoved(ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxCustomDocumentInterval.ParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
end;

procedure TdxCustomDocumentInterval.RunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer);
begin
end;

procedure TdxCustomDocumentInterval.RunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex;
  ASplitOffset, ATailRunLength: Integer);
begin
end;

procedure TdxCustomDocumentInterval.RunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
end;

procedure TdxCustomDocumentInterval.RunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
end;

procedure TdxCustomDocumentInterval.RunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
end;

procedure TdxCustomDocumentInterval.RunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
end;

procedure TdxCustomDocumentInterval.SetNotChangedEndIds(const Value: TdxIntegerList);
begin
  if FNotChangedEndIds <> Value then
    FreeAndNil(FNotChangedEndIds);
  FNotChangedEndIds := Value;
end;

procedure TdxCustomDocumentInterval.SetNotChangedStartIds(const Value: TdxIntegerList);
begin
  if FNotChangedStartIds <> Value then
    FreeAndNil(FNotChangedStartIds);
  FNotChangedStartIds := Value;
end;

function TdxCustomDocumentInterval.SetPositionCore(const APos, AValue: TdxDocumentModelPosition): Boolean;
begin
  if APos = AValue then
    Exit(False);
  APos.CopyFrom(AValue);
  Result := True;
end;

procedure TdxCustomDocumentInterval.OnChanged(AStartChanged, AEndChanged: Boolean);
begin
  if AStartChanged or AEndChanged then
    OnChangedCore;
end;

procedure TdxCustomDocumentInterval.SetStartCore(const APos: TdxDocumentModelPosition);
begin
  if SetPositionCore(FInterval.Start, APos) then
    OnChanged(True, False);
end;

procedure TdxCustomDocumentInterval.UpdateEndPosition;
begin
  PieceTable.CalculateRunInfoEnd(&End, FInterval);
end;

function TdxCustomDocumentInterval.Contains(AStart, AEnd: TdxDocumentLogPosition): Boolean;
begin
  Result := (AStart >= NormalizedStart) and (AEnd <= NormalizedEnd);
end;

function TdxCustomDocumentInterval.Contains(AInterval: TdxCustomDocumentInterval): Boolean;
begin
  if AInterval = nil then
    Result := False
  else
    Result := (AInterval.NormalizedStart >= NormalizedStart) and (AInterval.NormalizedEnd <= NormalizedEnd);
end;

function TdxCustomDocumentInterval.IntersectsWith(AInterval: TdxCustomDocumentInterval): Boolean;
begin
  if AInterval = nil then
    Exit(False);
  Result := (AInterval.NormalizedEnd >= NormalizedStart) and (AInterval.NormalizedStart <= NormalizedEnd);
end;

function TdxCustomDocumentInterval.IntersectsWithExcludingBounds(AInterval: TdxCustomDocumentInterval): Boolean;
begin
  if AInterval = nil then
    Exit(False);
  if (Length = 0) and (AInterval.Length = 0) and (AInterval.NormalizedStart = NormalizedStart) then
    Exit(True);
  Result := (AInterval.NormalizedEnd > NormalizedStart) and (AInterval.NormalizedStart < NormalizedEnd);
end;

function TdxCustomDocumentInterval.IntersectsWith(AStart, AEnd: TdxDocumentLogPosition): Boolean;
begin
  Result := (AEnd >= NormalizedStart) and (AStart <= NormalizedEnd);
end;

procedure TdxCustomDocumentInterval.UpdateStartPosition;
begin
  PieceTable.CalculateRunInfoStart(Start, FInterval);
end;

procedure TdxCustomDocumentInterval.SetStartCore(AValue: TdxDocumentLogPosition);
begin
  if Start <> AValue then
  begin
    FInterval.Start.LogPosition := AValue;
    UpdateStartPosition;
    OnChanged(True, False);
  end;
end;

procedure TdxCustomDocumentInterval.OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
  AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
  if PieceTable = APieceTable then
    ParagraphInserted(ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
end;

procedure TdxCustomDocumentInterval.OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  if PieceTable = APieceTable then
    ParagraphRemoved(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxCustomDocumentInterval.OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  if PieceTable = APieceTable then
    ParagraphMerged(ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxCustomDocumentInterval.OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
begin
  if PieceTable = APieceTable then
    RunInserted(AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxCustomDocumentInterval.OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
  if PieceTable = APieceTable then
    RunRemoved(AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxCustomDocumentInterval.OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ASplitOffset: Integer);
begin
  if PieceTable = APieceTable then
    RunSplit(AParagraphIndex, ARunIndex, ASplitOffset);
end;

procedure TdxCustomDocumentInterval.OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer);
begin
  if PieceTable = APieceTable then
    RunJoined(AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
end;

procedure TdxCustomDocumentInterval.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ADeltaRunLength: Integer);
begin
  if PieceTable = APieceTable then
    RunMerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxCustomDocumentInterval.OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  if PieceTable = APieceTable then
    RunUnmerged(AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxCustomDocumentInterval.OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  if PieceTable = APieceTable then
    FieldRemoved(AFieldIndex);
end;

procedure TdxCustomDocumentInterval.OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  if PieceTable = APieceTable then
    FieldInserted(AFieldIndex);
end;

procedure TdxCustomDocumentInterval.OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxCustomDocumentInterval.OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

{ TdxDocumentModelStructureChangedNotifier }

class procedure TdxDocumentModelStructureChangedNotifier.NotifyBeginMultipleRunSplit(
  const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable);
begin
  AListener.OnBeginMultipleRunSplit(APieceTable);
end;

class procedure TdxDocumentModelStructureChangedNotifier.NotifyEndMultipleRunSplit(
  const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable);
begin
  AListener.OnEndMultipleRunSplit(APieceTable);
end;

class procedure TdxDocumentModelStructureChangedNotifier.NotifyFieldInserted(
  const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  AListener.OnFieldInserted(APieceTable, AFieldIndex);
end;

class procedure TdxDocumentModelStructureChangedNotifier.NotifyFieldRemoved(
  const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  AListener.OnFieldRemoved(APieceTable, AFieldIndex);
end;

class procedure TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(
  const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
  AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
  AListener.OnParagraphInserted(APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged,
    AActualParagraphIndex, AHistoryNotificationId);
end;

class procedure TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(
  const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  AListener.OnParagraphMerged(APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

class procedure TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(
  const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  AListener.OnParagraphRemoved(APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

class procedure TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(
  const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
begin
  AListener.OnRunInserted(APieceTable, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

class procedure TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(
  const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer);
begin
  AListener.OnRunJoined(APieceTable, AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
end;

class procedure TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(
  const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  AListener.OnRunMerged(APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

class procedure TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(
  const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
begin
  AListener.OnRunRemoved(APieceTable, AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
end;

class procedure TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(
  const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
  AListener.OnRunSplit(APieceTable, AParagraphIndex, ARunIndex, ASplitOffset);
end;

class procedure TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(
  const AListener: IdxDocumentModelStructureChangedListener; APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  AListener.OnRunUnmerged(APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

end.
