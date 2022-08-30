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

unit dxRichEdit.DocumentModel.History.BookmarkHistory;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.SectionRange,
  dxRichEdit.DocumentModel.Hyperlink;

type
  { TdxBookmarkBaseHistoryItem }

  TdxBookmarkBaseHistoryItem = class abstract(TdxRichEditHistoryItem)
  strict private
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
  public
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxDeleteBookmarkBaseHistoryItem }

  TdxDeleteBookmarkBaseHistoryItem = class abstract(TdxBookmarkBaseHistoryItem)
  strict private
    FDeletedBookmark: TdxBookmarkBase;
    FDeletedBookmarkIndex: Integer;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
    function GetBookmarkCollection: TdxBookmarkBaseCollection; virtual; abstract;

    property DeletedBookmark: TdxBookmarkBase read FDeletedBookmark write FDeletedBookmark;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    destructor Destroy; override;

    property DeletedBookmarkIndex: Integer read FDeletedBookmarkIndex write FDeletedBookmarkIndex;
  end;

  { TdxDeleteBookmarkHistoryItem }

  TdxDeleteBookmarkHistoryItem = class(TdxDeleteBookmarkBaseHistoryItem)
  protected
    function GetBookmarkCollection: TdxBookmarkBaseCollection; override;
  end;

  { TdxInsertDocumentIntervalHistoryItem }

  TdxInsertDocumentIntervalHistoryItem = class abstract(TdxBookmarkBaseHistoryItem)
  strict private
    FLength: Integer;
    FPosition: TdxDocumentLogPosition;
    FIndexToInsert: Integer;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;

    property Length: Integer read FLength write FLength;
    property Position: TdxDocumentLogPosition read FPosition write FPosition;
    property IndexToInsert: Integer read FIndexToInsert write FIndexToInsert;
  end;

  { TdxInsertBookmarkBaseHistoryItem }

  TdxInsertBookmarkBaseHistoryItem = class abstract(TdxInsertDocumentIntervalHistoryItem)
  strict private
    FBookmark: TdxBookmarkBase;
    FOwnsBookmark: Boolean;
  protected
    procedure ExecuteCore; virtual;
    procedure RedoCore; override;
    procedure UndoCore; override;
    function CreateBookmark(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition): TdxBookmarkBase; virtual; abstract;
    function GetBookmarkCollection: TdxBookmarkBaseCollection; virtual; abstract;

    property Bookmark: TdxBookmarkBase read FBookmark write FBookmark;
  public
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TdxInsertBookmarkHistoryItem }

  TdxInsertBookmarkHistoryItem = class(TdxInsertBookmarkBaseHistoryItem)
  strict private
    FBookmarkName: string;
    FForceUpdateInterval: Boolean;
  protected
    function CreateBookmark(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition): TdxBookmarkBase; override;
    function GetBookmarkCollection: TdxBookmarkBaseCollection; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;

    property BookmarkName: string read FBookmarkName write FBookmarkName;
    property ForceUpdateInterval: Boolean read FForceUpdateInterval write FForceUpdateInterval;
  end;

implementation

uses
  Math;

{ TdxBookmarkBaseHistoryItem }

function TdxBookmarkBaseHistoryItem.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxBookmarkBaseHistoryItem.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(inherited PieceTable);
end;

{ TdxDeleteBookmarkBaseHistoryItem }

constructor TdxDeleteBookmarkBaseHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FDeletedBookmarkIndex := -1;
end;

destructor TdxDeleteBookmarkBaseHistoryItem.Destroy;
begin
  FreeAndNil(FDeletedBookmark);
  inherited Destroy;
end;

procedure TdxDeleteBookmarkBaseHistoryItem.RedoCore;
var
  ABookmarks: TdxBookmarkBaseCollection;
begin
  ABookmarks := GetBookmarkCollection;
  FDeletedBookmark := ABookmarks[FDeletedBookmarkIndex];
  ABookmarks.Delete(FDeletedBookmarkIndex, False);
end;

procedure TdxDeleteBookmarkBaseHistoryItem.UndoCore;
begin
  GetBookmarkCollection.Insert(FDeletedBookmarkIndex, FDeletedBookmark);
  FDeletedBookmark := nil;
end;

{ TdxDeleteBookmarkHistoryItem }

function TdxDeleteBookmarkHistoryItem.GetBookmarkCollection: TdxBookmarkBaseCollection;
begin
  Result := PieceTable.Bookmarks;
end;

{ TdxInsertDocumentIntervalHistoryItem }

constructor TdxInsertDocumentIntervalHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FIndexToInsert := -1;
end;

{ TdxInsertBookmarkBaseHistoryItem }

destructor TdxInsertBookmarkBaseHistoryItem.Destroy;
begin
  if FOwnsBookmark then
    FreeAndNil(FBookmark);
  inherited Destroy;
end;

procedure TdxInsertBookmarkBaseHistoryItem.Execute;
begin
  ExecuteCore;
  inherited Execute;
end;

procedure TdxInsertBookmarkBaseHistoryItem.ExecuteCore;
begin
  FOwnsBookmark := True;
  FBookmark := CreateBookmark(Min(Position, PieceTable.DocumentEndLogPosition), Min(Position + Length, PieceTable.DocumentEndLogPosition));
  if Length = 0 then
    FBookmark.CanExpand := False;
end;

procedure TdxInsertBookmarkBaseHistoryItem.RedoCore;
begin
  IndexToInsert := GetBookmarkCollection.Add(FBookmark);
  FOwnsBookmark := False;
end;

procedure TdxInsertBookmarkBaseHistoryItem.UndoCore;
begin
  GetBookmarkCollection.Delete(IndexToInsert, False);
  FOwnsBookmark := True;
end;

{ TdxInsertBookmarkHistoryItem }

constructor TdxInsertBookmarkHistoryItem.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FBookmarkName := '';
end;

function TdxInsertBookmarkHistoryItem.CreateBookmark(AStart: TdxDocumentLogPosition; AEnd: TdxDocumentLogPosition): TdxBookmarkBase;
begin
  Result := TdxBookmark.Create(PieceTable, AStart, AEnd, FForceUpdateInterval);
  TdxBookmark(Result).Name := BookmarkName;
end;

function TdxInsertBookmarkHistoryItem.GetBookmarkCollection: TdxBookmarkBaseCollection;
begin
  Result := PieceTable.Bookmarks;
end;

end.
