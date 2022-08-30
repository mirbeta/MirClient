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

unit dxRichEdit.DocumentModel.History.Run;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxCore,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.History.Simple,
  dxRichEdit.DocumentModel.History.Paragraph,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.InlineObjectFormatting,
  dxRichEdit.DocumentModel.Simple,
  dxGenerics;

type
  { TextRunChangeCaseHistoryItem }

  TdxTextRunChangeCaseHistoryItem = class abstract(TdxRichEditHistoryItem)
  public const
    ChangeActions = [
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetRuler,
      TdxDocumentModelChangeAction.RaiseContentChanged,
      TdxDocumentModelChangeAction.RaiseModifiedChanged,
      TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetSelectionLayout];
  private
    FRunIndex: TdxRunIndex;
		FOriginalText: string;
  protected
    procedure ApplyChangeActions;
    procedure UndoCore; override;
    procedure RedoCore; override;
    function ChangeCase(const AValue: Char; AAllCaps: Boolean): Char; virtual; abstract;
  public
    constructor Create(APieceTable: TdxPieceTable; ARunIndex: TdxRunIndex); reintroduce;

    procedure Execute; override;

    property RunIndex: TdxRunIndex read FRunIndex;
  end;

  { TdxTextRunMakeLowerCaseHistoryItem }

  TdxTextRunMakeLowerCaseHistoryItem = class(TdxTextRunChangeCaseHistoryItem)
  protected
    function ChangeCase(const AValue: Char; AAllCaps: Boolean): Char; override;
  end;

  { TdxTextRunMakeUpperCaseHistoryItem }

  TdxTextRunMakeUpperCaseHistoryItem = class(TdxTextRunChangeCaseHistoryItem)
  protected
    function ChangeCase(const AValue: Char; AAllCaps: Boolean): Char; override;
  end;

  { TdxTextRunToggleCaseHistoryItem }

  TdxTextRunToggleCaseHistoryItem = class(TdxTextRunChangeCaseHistoryItem)
  protected
    function ChangeCase(const AValue: Char; AAllCaps: Boolean): Char; override;
  end;

  { TdxSectionRunInsertedHistoryItem }

  TdxSectionRunInsertedHistoryItem = class(TdxParagraphRunInsertedHistoryItem)
  public
    function CreateRun(AParagraph: TdxParagraphBase): TdxTextRunBase; override;
  end;

  { TdxAddAbstractNumberingListHistoryItem }

  TdxAddAbstractNumberingListHistoryItem = class(TdxRichEditHistoryItem)
  private
    FAbstractList: TdxAbstractNumberingList;
    FOwnedAbstractList: Boolean;
    function GetDocumentModel: TdxDocumentModel;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    destructor Destroy; override;

    property AbstractList: TdxAbstractNumberingList read FAbstractList write FAbstractList;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  end;

  { TdxAddNumberingListHistoryItem }

  TdxAddNumberingListHistoryItem = class(TdxRichEditHistoryItem)
  private
    FNumberingList: TdxNumberingList;
    FOwnedNumberingList: Boolean;
    function GetDocumentModel: TdxDocumentModel;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    destructor Destroy; override;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property NumberingList: TdxNumberingList read FNumberingList write FNumberingList;
  end;

  { TdxTextRunsDeletedHistoryItem }

  TdxTextRunsDeletedHistoryItem = class(TdxSimpleTextRunsDeletedHistoryItem)
  strict private
    FAffectedBookmarks: TdxBookmarkInfoList;
    FAffectedRangePermissions: TdxBookmarkInfoList;
    FAffectedComments: TdxList<TdxBookmarkInfo>;
    function IsBookmarkStartPositionAffected(ABookmark: TdxBookmarkBase; AStartRunIndex: TdxRunIndex;
      AEndRunIndex: TdxRunIndex): Boolean;
    function IsBookmarkEndPositionAffected(ABookmark: TdxBookmarkBase; AStartRunIndex: TdxRunIndex;
      AEndRunIndex: TdxRunIndex): Boolean;
    function CalculateAffectedBookmarkList(ABookmarks: TdxBookmarkBaseCollection): TdxBookmarkInfoList;
    procedure RepairBookmarks(AAffectedBookmarks: TdxBookmarkInfoList);

    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
  strict protected
    procedure AfterUndoCore; override;
    procedure BeforeRedoCore; override;
  public
    destructor Destroy; override;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

implementation

uses
  Classes, Contnrs, Character, dxCoreClasses,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.SectionRange;

{ TdxTextRunChangeCaseHistoryItem }

constructor TdxTextRunChangeCaseHistoryItem.Create(APieceTable: TdxPieceTable; ARunIndex: TdxRunIndex);
begin
  inherited Create(APieceTable);
  FRunIndex := ARunIndex;
end;

procedure TdxTextRunChangeCaseHistoryItem.Execute;
var
  ARun: TdxTextRun;
begin
  ARun := TdxTextRun(PieceTable.Runs[RunIndex]);
  FOriginalText := ARun.GetPlainText(PieceTable.TextBuffer);
  inherited Execute;
end;

procedure TdxTextRunChangeCaseHistoryItem.ApplyChangeActions;
begin
  PieceTable.ApplyChangesCore(ChangeActions, RunIndex, RunIndex);
end;

procedure TdxTextRunChangeCaseHistoryItem.RedoCore;
var
  ARun: TdxTextRunBase;
  ATextBuffer: TdxChunkedStringBuilder;
  I, ACount: Integer;
  AAllCaps: Boolean;
begin
  ACount := Length(FOriginalText);
  if ACount = 0 then
    Exit;
  ATextBuffer := PieceTable.TextBuffer;
  ARun := PieceTable.Runs[RunIndex];
  AAllCaps := ARun.AllCaps;
  for I := ARun.StartIndex + ARun.Length - 1 downto ARun.StartIndex do
    ATextBuffer[I] := ChangeCase(ATextBuffer[I], AAllCaps);
  ApplyChangeActions;
end;

procedure TdxTextRunChangeCaseHistoryItem.UndoCore;
var
  ARun: TdxTextRunBase;
  ATextBuffer: TdxChunkedStringBuilder;
  I, ACount: Integer;
begin
  ACount := Length(FOriginalText);
  if ACount = 0 then
    Exit;
  ARun := PieceTable.Runs[RunIndex];
  Assert(ARun.Length = ACount);
  ATextBuffer := PieceTable.TextBuffer;
  for I := 0 to ACount - 1 do
    ATextBuffer[I + ARun.StartIndex] := FOriginalText[I + 1];
  ApplyChangeActions;
end;

{ TdxTextRunMakeLowerCaseHistoryItem }

function TdxTextRunMakeLowerCaseHistoryItem.ChangeCase(const AValue: Char; AAllCaps: Boolean): Char;
begin
{$IFDEF DELPHIXE4}
  Result := AValue.ToLower;
{$ELSE}
  Result := TCharacter.ToLower(AValue);
{$ENDIF}
end;

{ TdxTextRunMakeUpperCaseHistoryItem }

function TdxTextRunMakeUpperCaseHistoryItem.ChangeCase(const AValue: Char; AAllCaps: Boolean): Char;
begin
{$IFDEF DELPHIXE4}
  Result := AValue.ToUpper;
{$ELSE}
  Result := TCharacter.ToUpper(AValue);
{$ENDIF}
end;

{ TdxTextRunToggleCaseHistoryItem }

function TdxTextRunToggleCaseHistoryItem.ChangeCase(const AValue: Char; AAllCaps: Boolean): Char;
begin
{$IFDEF DELPHIXE4}
  if AAllCaps then
  begin
    if AValue.IsUpper then
      Exit(AValue.ToLower);
  end
  else
  begin
    if AValue.IsLower then
      Exit(AValue.ToUpper);
    if AValue.IsUpper then
      Exit(AValue.ToLower);
  end;
{$ELSE}
  if AAllCaps then
  begin
    if TCharacter.IsUpper(AValue) then
      Exit(TCharacter.ToLower(AValue));
  end
  else
  begin
    if TCharacter.IsLower(AValue) then
      Exit(TCharacter.ToUpper(AValue));
    if TCharacter.IsUpper(AValue) then
      Exit(TCharacter.ToLower(AValue));
  end;
{$ENDIF}
  Result := AValue;
end;

{ TdxSectionRunInsertedHistoryItem }

function TdxSectionRunInsertedHistoryItem.CreateRun(AParagraph: TdxParagraphBase): TdxTextRunBase;
begin
  Result := TdxSectionRun.Create(AParagraph);
end;

{ TdxAddAbstractNumberingListHistoryItem }

destructor TdxAddAbstractNumberingListHistoryItem.Destroy;
begin
  if FOwnedAbstractList then
    FreeAndNil(FAbstractList);
  inherited Destroy;
end;

procedure TdxAddAbstractNumberingListHistoryItem.UndoCore;
var
  ALists: TdxAbstractNumberingListCollection;
begin
  ALists := DocumentModel.AbstractNumberingLists;
  Assert(ALists.IndexOf(FAbstractList) = ALists.Count - 1);
  FAbstractList.Deleted := True;
  ALists.Extract(FAbstractList);
  FOwnedAbstractList := True;
end;

procedure TdxAddAbstractNumberingListHistoryItem.RedoCore;
begin
  FAbstractList.Deleted := False;
  DocumentModel.AbstractNumberingLists.Add(FAbstractList);
  FOwnedAbstractList := False;
end;

function TdxAddAbstractNumberingListHistoryItem.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

{ TdxAddNumberingListHistoryItem }

destructor TdxAddNumberingListHistoryItem.Destroy;
begin
  if FOwnedNumberingList then
    FreeAndNil(FNumberingList);
  inherited Destroy;
end;

procedure TdxAddNumberingListHistoryItem.UndoCore;
var
  ALists: TdxNumberingListCollection;
begin
  ALists := DocumentModel.NumberingLists;
  Assert(ALists.IndexOf(FNumberingList) = ALists.Count - 1);
  ALists.Extract(FNumberingList);
  FOwnedNumberingList := True;
end;

function TdxAddNumberingListHistoryItem.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

procedure TdxAddNumberingListHistoryItem.RedoCore;
begin
  DocumentModel.NumberingLists.Add(FNumberingList);
  FOwnedNumberingList := False;
end;

{ TdxTextRunsDeletedHistoryItem }

destructor TdxTextRunsDeletedHistoryItem.Destroy;
begin
  FreeAndNil(FAffectedBookmarks);
  FreeAndNil(FAffectedRangePermissions);
  FreeAndNil(FAffectedComments);
  inherited Destroy;
end;

procedure TdxTextRunsDeletedHistoryItem.AfterUndoCore;
begin
  RepairBookmarks(FAffectedBookmarks);
  RepairBookmarks(FAffectedRangePermissions);
end;

procedure TdxTextRunsDeletedHistoryItem.BeforeRedoCore;
begin
  FreeAndNil(FAffectedBookmarks);
  FAffectedBookmarks := CalculateAffectedBookmarkList(PieceTable.Bookmarks);
  FreeAndNil(FAffectedRangePermissions);
  FAffectedRangePermissions := CalculateAffectedBookmarkList(PieceTable.RangePermissions);
end;

function TdxTextRunsDeletedHistoryItem.CalculateAffectedBookmarkList(
  ABookmarks: TdxBookmarkBaseCollection): TdxBookmarkInfoList;
var
  I: Integer;
  ABookmark: TdxBookmarkBase;
  AInfo: TdxBookmarkInfo;
  AStartDeletedIndex, AEndDeletedIndex: TdxRunIndex;
begin
  Result := TdxBookmarkInfoList.Create;
  AStartDeletedIndex := RunIndex;
  AEndDeletedIndex := AStartDeletedIndex + DeletedRunCount - 1;
  for I := 0 to ABookmarks.Count - 1 do
  begin
    ABookmark := ABookmarks[I];
    AInfo := nil;
    if IsBookmarkStartPositionAffected(ABookmark, AStartDeletedIndex, AEndDeletedIndex) then
    begin
      AInfo := TdxBookmarkInfo.Create(ABookmark);
      AInfo.StartPosition := ABookmark.Interval.Start;
      AInfo.EndPosition.Invalidate;
    end;
    if IsBookmarkEndPositionAffected(ABookmark, AStartDeletedIndex, AEndDeletedIndex) then
    begin
      if AInfo = nil then
      begin
        AInfo := TdxBookmarkInfo.Create(ABookmark);
        AInfo.StartPosition.Invalidate;
      end;
      AInfo.EndPosition := ABookmark.Interval.&End;
    end;
    if AInfo <> nil then
      Result.Add(AInfo);
  end;
end;

function TdxTextRunsDeletedHistoryItem.IsBookmarkEndPositionAffected(
  ABookmark: TdxBookmarkBase; AStartRunIndex,
  AEndRunIndex: TdxRunIndex): Boolean;
var
  AEndBookmarkIndex: TdxRunIndex;
begin
  AEndBookmarkIndex := ABookmark.Interval.&End.RunIndex;
  Result := (AStartRunIndex <= AEndBookmarkIndex) and (AEndRunIndex >= AEndBookmarkIndex);
end;

function TdxTextRunsDeletedHistoryItem.IsBookmarkStartPositionAffected(
  ABookmark: TdxBookmarkBase; AStartRunIndex,
  AEndRunIndex: TdxRunIndex): Boolean;
var
  AStartBookmarkIndex: TdxRunIndex;
begin
  AStartBookmarkIndex := ABookmark.Interval.Start.RunIndex;
  Result := (AStartRunIndex <= AStartBookmarkIndex) and (AEndRunIndex >= AStartBookmarkIndex);
end;

procedure TdxTextRunsDeletedHistoryItem.RepairBookmarks(
  AAffectedBookmarks: TdxBookmarkInfoList);
var
  I: Integer;
  AInfo: TdxBookmarkInfo;
begin
  for I := 0 to AAffectedBookmarks.Count - 1 do
  begin
    AInfo := AAffectedBookmarks[I];
    if AInfo.StartPosition.IsValid then
      AInfo.Bookmark.Interval.Start.CopyFrom(AInfo.StartPosition);
    if AInfo.EndPosition.IsValid then
      AInfo.Bookmark.Interval.&End.CopyFrom(AInfo.EndPosition);
  end;
end;

function TdxTextRunsDeletedHistoryItem.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxTextRunsDeletedHistoryItem.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

end.
