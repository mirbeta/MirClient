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

unit dxRichEdit.DocumentModel.Intervals;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Contnrs, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.Tables.Core;

type
  TdxVisitableDocumentInterval = class;

  { TdxDocumentInterval }

  TdxDocumentInterval = class(TdxCustomDocumentInterval)
  protected
    procedure ParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer); override;
    procedure ParagraphRemoved(ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); override;
    procedure ParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer); override;
    procedure RunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength,
      AHistoryNotificationId: Integer); override;
    procedure RunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex;
      ASplitOffset, ATailRunLength: Integer); override;
    procedure RunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer); override;
    procedure RunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer); override;
    procedure RunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); override;
    procedure RunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); override;
  end;

  { TdxDocumentModelPositionAnchor }

  TdxDocumentModelPositionAnchor = class(TcxIUnknownObject, IdxDocumentModelStructureChangedListener)
  strict private
    FPositionChanged: Boolean;
    FPos: PdxDocumentModelPosition;
    FNotChangeNotificationIds: TdxIntegerList;
    FIsOwnerOfNotChangeNotificationIds: Boolean;
    function GetPos: TdxDocumentModelPosition;
  protected
    function ShouldChange(AHistoryNotificationId: Integer): Boolean;
    procedure AddNotChangeNotificationIds(AHistoryNotificationId: Integer);
    function OnParagraphInsertedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      AHistoryNotificationId: Integer): Boolean;
    function OnParagraphRemovedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      AHistoryNotificationId: Integer): Boolean;
    function OnParagraphMergedCore(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      AHistoryNotificationId: Integer): Boolean;
    function OnRunInsertedCore(ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer): Boolean;
    function OnRunRemovedCore(ARunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer): Boolean;
    function OnRunSplitCore(ARunIndex: TdxRunIndex; ASplitOffset: Integer): Boolean;
    function OnRunJoinedCore(AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer): Boolean;
    function OnRunMergedCore(ARunIndex: TdxRunIndex; ADeltaRunLength: Integer): Boolean;
    function OnRunUnmergedCore(ARunIndex: TdxRunIndex; ADeltaRunLength: Integer): Boolean;
    procedure OnFieldInsertedCore(AFieldIndex: Integer);
    procedure OnFieldRemovedCore(AFieldIndex: Integer);

    property NotChangeNotificationIds: TdxIntegerList read FNotChangeNotificationIds;
    property IsOwnerOfNotChangeNotificationIds: Boolean read FIsOwnerOfNotChangeNotificationIds write FIsOwnerOfNotChangeNotificationIds;
  public
    constructor Create(APos: PdxDocumentModelPosition); overload;
    constructor Create(APos: PdxDocumentModelPosition; ANotChangeNotificationIds: TdxIntegerList); overload;
    destructor Destroy; override;

  {$REGION 'IdxDocumentModelStructureChangedListener'}
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer);
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer);
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ASplitOffset: Integer);
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex;
      ASplitOffset, ATailRunLength: Integer);
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ADeltaRunLength: Integer);
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ADeltaRunLength: Integer);
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
  {$ENDREGION}

    property PositionChanged: Boolean read FPositionChanged;
    property Position: TdxDocumentModelPosition read GetPos;
  end;

  { IdxDocumentIntervalVisitor }

  IdxDocumentIntervalVisitor = interface
  ['{6FB1B05D-AE01-463A-9AFC-8AEB426B854F}']
    procedure Visit(AInterval: TdxVisitableDocumentInterval);
// todo: refactor
//    procedure Visit(ABookmark: TdxBookmark); overload;
//    procedure Visit(ARangePermission: TdxRangePermission); overload;
//  {$IFDEF RICH_COMMENT}
//    procedure Visit(AComment: TdxComment);
//  {$ENDIF}
  end;

  { TdxVisitableDocumentInterval }

  TdxVisitableDocumentInterval = class abstract (TdxDocumentInterval)
  public
    procedure Visit(const AVisitor: IdxDocumentIntervalVisitor); virtual; abstract;
  end;

  { TdxDocumentIntervalEventArgs }

  TdxDocumentIntervalEventArgs = class(TdxEventArgs)
  strict private
    FIndex: Integer;
  public
    constructor Create(AIndex: Integer);

    property Index: Integer read FIndex;
  end;

  TdxDocumentIntervalEvent = procedure(ASender: TObject; E: TdxDocumentIntervalEventArgs) of object;
  TdxDocumentIntervalEventHandler = TdxMulticastMethod<TdxDocumentIntervalEvent>;

  { TdxDocumentIntervalComparer }

  TdxDocumentIntervalComparer = class(TdxComparer<TdxDocumentInterval>)
  public
    function Compare(const X, Y: TdxDocumentInterval): Integer; override;
  end;

  TdxDocumentIntervalList = class(TdxList<TdxDocumentInterval>);

  { TdxDocumentIntervalCollection }

  TdxDocumentIntervalCollection = class(TcxIUnknownObject, IdxDocumentModelStructureChangedListener)
  strict private
    FPieceTable: TdxSimplePieceTable;
    FInnerList: TdxDocumentIntervalList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxDocumentInterval;
  protected
    FOnInserted: TdxDocumentIntervalEventHandler;
    FOnRemoved: TdxDocumentIntervalEventHandler;
  {$REGION IdxDocumentModelStructureChangedListener}
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure ListenerOnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
    procedure IdxDocumentModelStructureChangedListener.OnRunInserted = ListenerOnRunInserted;
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer);
    procedure ListenerOnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
    procedure IdxDocumentModelStructureChangedListener.OnRunMerged = ListenerOnRunMerged;
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
  {$ENDREGION}

    function CreateInnerList: TdxDocumentIntervalList; virtual;
    procedure RaiseDocumentIntervalInserted(AIndex: Integer);
    procedure RaiseDocumentIntervalRemoved(AIndex: Integer);
    function AddCore(AValue: TdxDocumentInterval): Integer; virtual;
    function CreateDocumentIntervalComparer: TdxDocumentIntervalComparer; virtual;
    procedure InsertCore(AIndex: Integer; AValue: TdxDocumentInterval); virtual;
    procedure OnDocumentIntervalInserted(AIndex: Integer); virtual;
    procedure OnDocumentIntervalRemoved(AIndex: Integer); virtual;
    procedure DeleteCore(AIndex: Integer; AOwns: Boolean); virtual;
    procedure ForEachCore(const AAction: TdxAction<TdxDocumentInterval>); virtual;
    function IndexOfCore(AItem: TdxDocumentInterval): Integer; virtual;
    function BinarySearchCore(const APredicate: IdxComparable<TdxDocumentInterval>): Integer; virtual;
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer); virtual;
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer); virtual;
  public
    constructor Create(APieceTable: TdxSimplePieceTable);
    destructor Destroy; override;
    function Add(AValue: TdxDocumentInterval): Integer; virtual;
    procedure Insert(AIndex: Integer; AValue: TdxDocumentInterval);
    procedure Delete(AIndex: Integer; AOwns: Boolean = True);
    procedure ForEach(const AAction: TdxAction<TdxDocumentInterval>);
    function IndexOf(AItem: TdxDocumentInterval): Integer;
    function BinarySearch(const APredicate: IdxComparable<TdxDocumentInterval>): Integer;

    property PieceTable: TdxSimplePieceTable read FPieceTable;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxDocumentInterval read GetItem; default;

    property InnerList: TdxDocumentIntervalList read FInnerList;

    property Inserted: TdxDocumentIntervalEventHandler read FOnInserted;
    property Removed: TdxDocumentIntervalEventHandler read FOnRemoved;
  end;

implementation

{ TdxDocumentIntervalComparer }

function TdxDocumentIntervalComparer.Compare(const X, Y: TdxDocumentInterval): Integer;
begin
  if X.Start = Y.Start then
  begin
    if X.&End = Y.&End then
      Exit(0);
    if X.&End < Y.&End then
      Exit(-1)
    else
      Exit(1);
  end;
  if X.Start < Y.Start then
    Exit(-1)
  else
    Exit(1);
end;

{ TdxDocumentInterval }

procedure TdxDocumentInterval.ParagraphInserted(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex;
  AHistoryNotificationId: Integer);
var
  AStartAnchor, AEndAnchor: TdxDocumentModelPositionAnchor;
begin
  AStartAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.Start, NotChangedStartIds);
  try
    AEndAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.&End, NotChangedEndIds);
    try
      AStartAnchor.OnParagraphInserted(PieceTable, ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
      AEndAnchor.OnParagraphInserted(PieceTable, ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged, AActualParagraphIndex, AHistoryNotificationId);
      if AStartAnchor.NotChangeNotificationIds <> nil then
        NotChangedStartIds := TdxIntegerList.Create(AStartAnchor.NotChangeNotificationIds)
      else
        NotChangedStartIds := nil;
      if AEndAnchor.NotChangeNotificationIds <> nil then
        NotChangedEndIds := TdxIntegerList.Create(AEndAnchor.NotChangeNotificationIds)
      else
        NotChangedEndIds := nil;
      OnChanged(AStartAnchor.PositionChanged, AEndAnchor.PositionChanged);
    finally
      AEndAnchor.Free;
    end;
  finally
    AStartAnchor.Free;
  end;
end;

procedure TdxDocumentInterval.ParagraphMerged(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
var
  AStartAnchor, AEndAnchor: TdxDocumentModelPositionAnchor;
begin
  AStartAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.Start, NotChangedStartIds);
  try
    AEndAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.&End, NotChangedEndIds);
    try
      AStartAnchor.OnParagraphMerged(PieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
      AEndAnchor.OnParagraphMerged(PieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
      if AStartAnchor.NotChangeNotificationIds <> nil then
        NotChangedStartIds := TdxIntegerList.Create(AStartAnchor.NotChangeNotificationIds)
      else
        NotChangedStartIds := nil;
      if AEndAnchor.NotChangeNotificationIds <> nil then
        NotChangedEndIds := TdxIntegerList.Create(AEndAnchor.NotChangeNotificationIds)
      else
        NotChangedEndIds := nil;
      OnChanged(AStartAnchor.PositionChanged, AEndAnchor.PositionChanged);
    finally
      AEndAnchor.Free;
    end;
  finally
    AStartAnchor.Free;
  end;
end;

procedure TdxDocumentInterval.ParagraphRemoved(ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
var
  AStartAnchor, AEndAnchor: TdxDocumentModelPositionAnchor;
begin
  AStartAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.Start, NotChangedStartIds);
  try
    AEndAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.&End, NotChangedEndIds);
    try
      AStartAnchor.OnParagraphRemoved(PieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
      AEndAnchor.OnParagraphRemoved(PieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
      if AStartAnchor.NotChangeNotificationIds <> nil then
        NotChangedStartIds := TdxIntegerList.Create(AStartAnchor.NotChangeNotificationIds)
      else
        NotChangedStartIds := nil;
      if AEndAnchor.NotChangeNotificationIds <> nil then
        NotChangedEndIds := TdxIntegerList.Create(AEndAnchor.NotChangeNotificationIds)
      else
        NotChangedEndIds := nil;
      OnChanged(AStartAnchor.PositionChanged, AEndAnchor.PositionChanged);
    finally
      AEndAnchor.Free;
    end;
  finally
    AStartAnchor.Free;
  end;
end;

procedure TdxDocumentInterval.RunInserted(AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer);
var
  AStartAnchor, AEndAnchor: TdxDocumentModelPositionAnchor;
begin
  AStartAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.Start, NotChangedStartIds);
  try
    AEndAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.&End, NotChangedEndIds);
    try
      AStartAnchor.OnRunInserted(PieceTable, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
      AEndAnchor.OnRunInserted(PieceTable, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
      if AStartAnchor.NotChangeNotificationIds <> nil then
        NotChangedStartIds := TdxIntegerList.Create(AStartAnchor.NotChangeNotificationIds)
      else
        NotChangedStartIds := nil;
      if AEndAnchor.NotChangeNotificationIds <> nil then
        NotChangedEndIds := TdxIntegerList.Create(AEndAnchor.NotChangeNotificationIds)
      else
        NotChangedEndIds := nil;
      OnChanged(AStartAnchor.PositionChanged, AEndAnchor.PositionChanged);
    finally
      AEndAnchor.Free;
    end;
  finally
    AStartAnchor.Free;
  end;
end;

procedure TdxDocumentInterval.RunJoined(AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex;
  ASplitOffset, ATailRunLength: Integer);
var
  AStartAnchor, AEndAnchor: TdxDocumentModelPositionAnchor;
begin
  AStartAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.Start);
  try
    AEndAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.&End);
    try
      AStartAnchor.OnRunJoined(PieceTable, AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
      AEndAnchor.OnRunJoined(PieceTable, AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
      OnChanged(AStartAnchor.PositionChanged, AEndAnchor.PositionChanged);
    finally
      AEndAnchor.Free;
    end;
  finally
    AStartAnchor.Free;
  end;
end;

procedure TdxDocumentInterval.RunMerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ADeltaRunLength: Integer);
var
  AStartAnchor, AEndAnchor: TdxDocumentModelPositionAnchor;
begin
  AStartAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.Start);
  try
    AEndAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.&End);
    try
      AStartAnchor.OnRunMerged(PieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
      AEndAnchor.OnRunMerged(PieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
      OnChanged(AStartAnchor.PositionChanged, AEndAnchor.PositionChanged);
    finally
      AEndAnchor.Free;
    end;
  finally
    AStartAnchor.Free;
  end;
end;

procedure TdxDocumentInterval.RunRemoved(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer);
var
  AStartAnchor, AEndAnchor: TdxDocumentModelPositionAnchor;
begin
  AStartAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.Start, NotChangedStartIds);
  try
    AEndAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.&End, NotChangedEndIds);
    try
      AStartAnchor.OnRunRemoved(PieceTable, AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
      AEndAnchor.OnRunRemoved(PieceTable, AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
      if AStartAnchor.NotChangeNotificationIds <> nil then
        NotChangedStartIds := TdxIntegerList.Create(AStartAnchor.NotChangeNotificationIds)
      else
        NotChangedStartIds := nil;
      if AEndAnchor.NotChangeNotificationIds <> nil then
        NotChangedEndIds := TdxIntegerList.Create(AEndAnchor.NotChangeNotificationIds)
      else
        NotChangedEndIds := nil;
      OnChanged(AStartAnchor.PositionChanged, AEndAnchor.PositionChanged);
    finally
      AEndAnchor.Free;
    end;
  finally
    AStartAnchor.Free;
  end;
end;

procedure TdxDocumentInterval.RunSplit(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ASplitOffset: Integer);
var
  AStartAnchor, AEndAnchor: TdxDocumentModelPositionAnchor;
begin
  AStartAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.Start);
  try
    AEndAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.&End);
    try
      AStartAnchor.OnRunSplit(PieceTable, AParagraphIndex, ARunIndex, ASplitOffset);
      AEndAnchor.OnRunSplit(PieceTable, AParagraphIndex, ARunIndex, ASplitOffset);
      OnChanged(AStartAnchor.PositionChanged, AEndAnchor.PositionChanged);
    finally
      AEndAnchor.Free;
    end;
  finally
    AStartAnchor.Free;
  end;
end;

procedure TdxDocumentInterval.RunUnmerged(AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ADeltaRunLength: Integer);
var
  AStartAnchor, AEndAnchor: TdxDocumentModelPositionAnchor;
begin
  AStartAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.Start);
  try
    AEndAnchor := TdxDocumentModelPositionAnchor.Create(@Interval.&End);
    try
      AStartAnchor.OnRunUnmerged(PieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
      AEndAnchor.OnRunUnmerged(PieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
      OnChanged(AStartAnchor.PositionChanged, AEndAnchor.PositionChanged);
    finally
      AEndAnchor.Free;
    end;
  finally
    AStartAnchor.Free;
  end;
end;

{ TdxDocumentModelPositionAnchor }

constructor TdxDocumentModelPositionAnchor.Create(APos: PdxDocumentModelPosition; ANotChangeNotificationIds: TdxIntegerList);
begin
  inherited Create;
  FNotChangeNotificationIds := ANotChangeNotificationIds;
  FPos := APos;
end;

constructor TdxDocumentModelPositionAnchor.Create(APos: PdxDocumentModelPosition);
begin
  inherited Create;
  FPos := APos;
end;

destructor TdxDocumentModelPositionAnchor.Destroy;
begin
  if FIsOwnerOfNotChangeNotificationIds then
    FreeAndNil(FNotChangeNotificationIds);
  inherited Destroy;
end;

function TdxDocumentModelPositionAnchor.GetPos: TdxDocumentModelPosition;
begin
  Result := FPos^;
end;

procedure TdxDocumentModelPositionAnchor.AddNotChangeNotificationIds(AHistoryNotificationId: Integer);
var
  AIndex: Integer;
begin
  if AHistoryNotificationId = TdxNotificationIdGenerator.EmptyId then
    Exit;
  if FNotChangeNotificationIds = nil then
  begin
    FIsOwnerOfNotChangeNotificationIds := True;
    FNotChangeNotificationIds := TdxIntegerList.Create;
    FNotChangeNotificationIds.Add(AHistoryNotificationId);
  end
  else
  begin
    if FNotChangeNotificationIds.BinarySearch(AHistoryNotificationId, AIndex) then
      Exit;
    FNotChangeNotificationIds.Insert(AIndex, AHistoryNotificationId);
  end;
end;

procedure TdxDocumentModelPositionAnchor.OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxDocumentModelPositionAnchor.OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxDocumentModelPositionAnchor.OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  if FPos.PieceTable = APieceTable then
    OnFieldInsertedCore(AFieldIndex);
end;

procedure TdxDocumentModelPositionAnchor.OnFieldInsertedCore(AFieldIndex: Integer);
begin
end;

procedure TdxDocumentModelPositionAnchor.OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  if FPos.PieceTable = APieceTable then
    OnFieldRemovedCore(AFieldIndex);
end;

procedure TdxDocumentModelPositionAnchor.OnFieldRemovedCore(AFieldIndex: Integer);
begin
end;

procedure TdxDocumentModelPositionAnchor.OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
  AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
  if FPos.PieceTable = APieceTable then
    FPositionChanged := OnParagraphInsertedCore(AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

function TdxDocumentModelPositionAnchor.OnParagraphInsertedCore(AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer): Boolean;
begin
  if AParagraphIndex >= FPos.ParagraphIndex then
  begin
    if (ARunIndex < FPos.RunIndex) or (ARunIndex = FPos.RunIndex) and ShouldChange(AHistoryNotificationId) then
    begin
      FPos.ParagraphIndex := FPos.ParagraphIndex + 1;
      Result := True;
    end
    else
      Result := False;
  end
  else
    if AParagraphIndex < FPos.ParagraphIndex then
    begin
      FPos.ParagraphIndex := FPos.ParagraphIndex + 1;
      Result := True;
    end
    else
      Result := False;
end;

procedure TdxDocumentModelPositionAnchor.OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  if FPos.PieceTable = APieceTable then
    FPositionChanged := OnParagraphMergedCore(AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

function TdxDocumentModelPositionAnchor.OnParagraphMergedCore(AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer): Boolean;
begin
  Result := False;
  if AParagraphIndex = FPos.ParagraphIndex then
  begin
    if ARunIndex <= FPos.RunIndex then
      if FPos.ParagraphIndex > 0 then
      begin
        FPos.ParagraphIndex := FPos.ParagraphIndex - 1;
        Exit(True);
      end;
  end
  else
    if AParagraphIndex < FPos.ParagraphIndex then
    begin
      if FPos.ParagraphIndex > 0 then
      begin
        FPos.ParagraphIndex := FPos.ParagraphIndex - 1;
        Exit(True);
      end;
    end
    else
      if AParagraphIndex = FPos.ParagraphIndex + 1 then
        if ARunIndex = FPos.RunIndex then
          AddNotChangeNotificationIds(AHistoryNotificationId);
end;

procedure TdxDocumentModelPositionAnchor.OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  if FPos.PieceTable = APieceTable then
    FPositionChanged := OnParagraphRemovedCore(AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

function TdxDocumentModelPositionAnchor.OnParagraphRemovedCore(AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer): Boolean;
begin
  Result := False;
  if AParagraphIndex < FPos.ParagraphIndex then
  begin
    if FPos.ParagraphIndex > 0 then
    begin
      FPos.ParagraphIndex := FPos.ParagraphIndex - 1;
      Result := True;
    end;
  end
  else
    if AParagraphIndex = FPos.ParagraphIndex then
      AddNotChangeNotificationIds(AHistoryNotificationId);
end;

procedure TdxDocumentModelPositionAnchor.OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
begin
  if FPos.PieceTable = APieceTable then
    FPositionChanged := OnRunInsertedCore(ANewRunIndex, ALength, AHistoryNotificationId);
end;

function TdxDocumentModelPositionAnchor.OnRunInsertedCore(ANewRunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer): Boolean;
begin
  Result := False;
  if (ANewRunIndex < FPos.RunIndex) or (ANewRunIndex = FPos.RunIndex) and ShouldChange(AHistoryNotificationId) then
  begin
    FPos.RunIndex := FPos.RunIndex + 1;
    FPos.RunStartLogPosition := FPos.RunStartLogPosition + ALength;
    FPos.LogPosition := FPos.LogPosition + ALength;
    Result := True;
  end;
end;

procedure TdxDocumentModelPositionAnchor.OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer);
begin
  if FPos.PieceTable = APieceTable then
    FPositionChanged := OnRunJoinedCore(AJoinedRunIndex, ASplitOffset, ATailRunLength);
end;

function TdxDocumentModelPositionAnchor.OnRunJoinedCore(AJoinedRunIndex: TdxRunIndex; ASplitOffset,
  ATailRunLength: Integer): Boolean;
begin
  Result := False;
  if AJoinedRunIndex + 1 = FPos.RunIndex then
  begin
    FPos.RunIndex := FPos.RunIndex - 1;
    FPos.RunStartLogPosition := FPos.RunStartLogPosition - (FPos.PieceTable.Runs[AJoinedRunIndex].Length - ATailRunLength);
    Result := True
  end
  else
    if AJoinedRunIndex < FPos.RunIndex  then
    begin
      FPos.RunIndex := FPos.RunIndex - 1;
      Result := True;
    end;
end;

procedure TdxDocumentModelPositionAnchor.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  if FPos.PieceTable = APieceTable then
    FPositionChanged := OnRunMergedCore(ARunIndex, ADeltaRunLength);
end;

function TdxDocumentModelPositionAnchor.OnRunMergedCore(ARunIndex: TdxRunIndex; ADeltaRunLength: Integer): Boolean;
begin
  Result := ARunIndex < FPos.RunIndex;
  if Result then
  begin
    FPos.LogPosition := FPos.LogPosition + ADeltaRunLength;
    FPos.RunStartLogPosition := FPos.RunStartLogPosition + ADeltaRunLength;
  end;
end;

procedure TdxDocumentModelPositionAnchor.OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
begin
  if FPos.PieceTable = APieceTable then
    FPositionChanged := OnRunRemovedCore(ARunIndex, ALength, AHistoryNotificationId);
end;

function TdxDocumentModelPositionAnchor.OnRunRemovedCore(ARunIndex: TdxRunIndex; ALength,
  AHistoryNotificationId: Integer): Boolean;
begin
  Result := False;
  if ARunIndex = FPos.RunIndex then
  begin
    if FPos.RunOffset = 0 then
      AddNotChangeNotificationIds(AHistoryNotificationId)
    else
      FPos.LogPosition := FPos.LogPosition - FPos.RunOffset;
    Result := True;
  end
  else
    if ARunIndex < FPos.RunIndex then
    begin
      FPos.RunIndex := FPos.RunIndex - 1;
      FPos.RunStartLogPosition := FPos.RunStartLogPosition - ALength;
      FPos.LogPosition := FPos.LogPosition - ALength;
      Result := True;
    end;
end;

procedure TdxDocumentModelPositionAnchor.OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ASplitOffset: Integer);
begin
  if FPos.PieceTable = APieceTable then
    FPositionChanged := OnRunSplitCore(ARunIndex, ASplitOffset);
end;

function TdxDocumentModelPositionAnchor.OnRunSplitCore(ARunIndex: TdxRunIndex; ASplitOffset: Integer): Boolean;
begin
  Result := False;
  if ARunIndex < FPos.RunIndex then
  begin
    FPos.RunIndex := FPos.RunIndex + 1;
    Result := True;
  end
  else
    if ARunIndex = FPos.RunIndex then
      if ASplitOffset <= FPos.RunOffset then
      begin
        FPos.RunStartLogPosition := FPos.RunStartLogPosition + FPos.PieceTable.Runs[ARunIndex].Length;
        FPos.RunIndex := FPos.RunIndex + 1;
        Result := True;
      end;
end;

procedure TdxDocumentModelPositionAnchor.OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  if FPos.PieceTable = APieceTable then
    FPositionChanged := OnRunUnmergedCore(ARunIndex, ADeltaRunLength);
end;

function TdxDocumentModelPositionAnchor.OnRunUnmergedCore(ARunIndex: TdxRunIndex; ADeltaRunLength: Integer): Boolean;
begin
  Result := ARunIndex < FPos.RunIndex;
  if Result then
  begin
    FPos.LogPosition := FPos.LogPosition + ADeltaRunLength;
    FPos.RunStartLogPosition := FPos.LogPosition + ADeltaRunLength;
  end;
end;

function TdxDocumentModelPositionAnchor.ShouldChange(AHistoryNotificationId: Integer): Boolean;
begin
  Result := (FNotChangeNotificationIds = nil) or (AHistoryNotificationId = TdxNotificationIdGenerator.EmptyId);
  if not Result then
    Result := FNotChangeNotificationIds.IndexOf(AHistoryNotificationId) < 0;
end;

{ TdxDocumentIntervalEventArgs }

constructor TdxDocumentIntervalEventArgs.Create(AIndex: Integer);
begin
  inherited Create;
  FIndex := AIndex;
end;

{ TdxDocumentIntervalCollection }

constructor TdxDocumentIntervalCollection.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create;
  Assert(APieceTable <> nil);
  FPieceTable := APieceTable;
  FInnerList := CreateInnerList;
end;

destructor TdxDocumentIntervalCollection.Destroy;
begin
  FreeAndNil(FInnerList);
  inherited Destroy;
end;

function TdxDocumentIntervalCollection.GetCount: Integer;
begin
  Result := FInnerList.Count;
end;

function TdxDocumentIntervalCollection.GetItem(Index: Integer): TdxDocumentInterval;
begin
  Result := FInnerList[Index];
end;

function TdxDocumentIntervalCollection.CreateInnerList: TdxDocumentIntervalList;
begin
  Result := TdxDocumentIntervalList.Create(True);
end;

procedure TdxDocumentIntervalCollection.RaiseDocumentIntervalInserted(AIndex: Integer);
var
  Args: TdxDocumentIntervalEventArgs;
begin
  if not FOnInserted.Empty then
  begin
    Args := TdxDocumentIntervalEventArgs.Create(AIndex);
    try
      FOnInserted.Invoke(Self, Args);
    finally
      Args.Free;
    end;
  end;
end;

procedure TdxDocumentIntervalCollection.RaiseDocumentIntervalRemoved(AIndex: Integer);
var
  Args: TdxDocumentIntervalEventArgs;
begin
  if not FOnRemoved.Empty then
  begin
    Args := TdxDocumentIntervalEventArgs.Create(AIndex);
    try
      FOnRemoved.Invoke(Self, Args);
    finally
      Args.Free;
    end;
  end;
end;

function TdxDocumentIntervalCollection.Add(AValue: TdxDocumentInterval): Integer;
begin
  Result := AddCore(AValue);
end;

function TdxDocumentIntervalCollection.AddCore(AValue: TdxDocumentInterval): Integer;
var
  AIndex: Integer;
  AComparer: TdxDocumentIntervalComparer;
begin
  Assert(AValue.PieceTable = PieceTable);
  AComparer := CreateDocumentIntervalComparer;
  try
    if FInnerList.BinarySearch(AValue, AIndex, AComparer) then
    begin
      Inc(AIndex);
      while (FInnerList.Count > AIndex) and (AComparer.Compare(AValue, FInnerList[AIndex]) = 0) do
        Inc(AIndex)
    end;
  finally
    AComparer.Free;
  end;
  Insert(AIndex, AValue);
  Result := AIndex;
end;

function TdxDocumentIntervalCollection.CreateDocumentIntervalComparer: TdxDocumentIntervalComparer;
begin
  Result := TdxDocumentIntervalComparer.Create;
end;

procedure TdxDocumentIntervalCollection.Insert(AIndex: Integer; AValue: TdxDocumentInterval);
begin
  InsertCore(AIndex, AValue);
  OnDocumentIntervalInserted(AIndex);
end;

procedure TdxDocumentIntervalCollection.InsertCore(AIndex: Integer; AValue: TdxDocumentInterval);
begin
  Assert(AValue.PieceTable = PieceTable);
  FInnerList.Insert(AIndex, AValue);
end;

procedure TdxDocumentIntervalCollection.Delete(AIndex: Integer; AOwns: Boolean = True);
begin
  DeleteCore(AIndex, AOwns);
  OnDocumentIntervalRemoved(AIndex);
end;

procedure TdxDocumentIntervalCollection.OnDocumentIntervalInserted(AIndex: Integer);
begin
  RaiseDocumentIntervalInserted(AIndex);
end;

procedure TdxDocumentIntervalCollection.OnDocumentIntervalRemoved(AIndex: Integer);
begin
  RaiseDocumentIntervalRemoved(AIndex);
end;

procedure TdxDocumentIntervalCollection.DeleteCore(AIndex: Integer; AOwns: Boolean);
begin
  if AOwns then
    FInnerList.Delete(AIndex)
  else
    FInnerList.Extract(FInnerList[AIndex]);
end;

procedure TdxDocumentIntervalCollection.ForEach(const AAction: TdxAction<TdxDocumentInterval>);
begin
  ForEachCore(AAction);
end;

procedure TdxDocumentIntervalCollection.ForEachCore(const AAction: TdxAction<TdxDocumentInterval>);
var
  I: Integer;
begin
  for I := 0 to FInnerList.Count - 1 do
    AAction(FInnerList[I]);
end;

function TdxDocumentIntervalCollection.IndexOf(AItem: TdxDocumentInterval): Integer;
begin
  Result := IndexOfCore(AItem);
end;

function TdxDocumentIntervalCollection.IndexOfCore(AItem: TdxDocumentInterval): Integer;
begin
  Result := FInnerList.IndexOf(AItem);
end;

function TdxDocumentIntervalCollection.BinarySearch(const APredicate: IdxComparable<TdxDocumentInterval>): Integer;
begin
  Result := BinarySearchCore(APredicate);
end;

function TdxDocumentIntervalCollection.BinarySearchCore(const APredicate: IdxComparable<TdxDocumentInterval>): Integer;
begin
  if not TdxAlgorithms1<TdxDocumentInterval>.BinarySearch(FInnerList, APredicate, Result) then
    Result := not Result;
end;

procedure TdxDocumentIntervalCollection.OnParagraphInserted(APieceTable: TdxCustomPieceTable;
  ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean; AActualParagraphIndex: TdxParagraphIndex;
  AHistoryNotificationId: Integer);
var
  I: Integer;
begin
  if PieceTable <> APieceTable then
    Exit;
  for I := 0 to Count - 1 do
    Self[I].OnParagraphInserted(APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged,
      AActualParagraphIndex, AHistoryNotificationId);
end;

procedure TdxDocumentIntervalCollection.OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
var
  I: Integer;
begin
  if PieceTable <> APieceTable then
    Exit;
  for I := 0 to Count - 1 do
    Self[I].OnParagraphRemoved(APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxDocumentIntervalCollection.OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
var
  I: Integer;
begin
  if PieceTable <> APieceTable then
    Exit;
  for I := 0 to Count - 1 do
    Self[I].OnParagraphMerged(APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
end;

procedure TdxDocumentIntervalCollection.ListenerOnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
begin
  if PieceTable <> APieceTable then
    Exit;
  OnRunInserted(APieceTable, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxDocumentIntervalCollection.OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ANewRunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Self[I].OnRunInserted(APieceTable, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxDocumentIntervalCollection.OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ALength: Integer; AHistoryNotificationId: Integer);
var
  I: Integer;
begin
  if PieceTable <> APieceTable then
    Exit;
  for I := 0 to Count - 1 do
    Self[I].OnRunRemoved(APieceTable, AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
end;

procedure TdxDocumentIntervalCollection.OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxDocumentIntervalCollection.OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxDocumentIntervalCollection.OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ASplitOffset: Integer);
var
  I: Integer;
begin
  if PieceTable <> APieceTable then
    Exit;
  for I := 0 to Count - 1 do
    Self[I].OnRunSplit(APieceTable, AParagraphIndex, ARunIndex, ASplitOffset);
end;

procedure TdxDocumentIntervalCollection.OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; AJoinedRunIndex: TdxRunIndex; ASplitOffset: Integer; ATailRunLength: Integer);
var
  I: Integer;
begin
  if PieceTable <> APieceTable then
    Exit;

  for I := 0 to Count - 1 do
    Self[I].OnRunJoined(APieceTable, AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
end;

procedure TdxDocumentIntervalCollection.ListenerOnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  if PieceTable <> APieceTable then
    Exit;

  OnRunMerged(APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxDocumentIntervalCollection.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Self[I].OnRunMerged(APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxDocumentIntervalCollection.OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
var
  I: Integer;
begin
  if PieceTable <> APieceTable then
    Exit;

  for I := 0 to Count - 1 do
    Self[I].OnRunUnmerged(APieceTable, AParagraphIndex, ARunIndex, ADeltaRunLength);
end;

procedure TdxDocumentIntervalCollection.OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
end;

procedure TdxDocumentIntervalCollection.OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
end;

end.
