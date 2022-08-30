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

unit dxRichEdit.Api.Hyperlinks;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils,
  dxCore, dxCoreClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.Api.NativeDocument,
  dxRichEdit.Utils.Types,

  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields;

type
  { TdxNativeHyperlink }

  TdxNativeHyperlink = class(TInterfacedObject, IdxRichEditHyperlink)
  strict private
    type
      TUpdateHyperlinkInfoProc = reference to procedure(AInfo: TdxHyperlinkInfo);
  strict private
    FIsValid: Boolean;
    FDocument: TdxNativeSubDocument;
    FPieceTable: TdxPieceTable;
    FField: TdxField;
    FNativeField: IdxRichEditField;

    function GetHyperlinkInfo: TdxHyperlinkInfo;
    procedure UpdateHyperlinkInfo(const Action: TUpdateHyperlinkInfoProc);

    function GetAnchor: string;
    function GetNavigateUri: string;
    function GetRange: IdxRichEditDocumentRange;
    function GetTarget: string;
    function GetToolTip: string;
    function GetVisited: Boolean;
    procedure SetAnchor(const Value: string);
    procedure SetNavigateUri(const Value: string);
    procedure SetTarget(const Value: string);
    procedure SetToolTip(const Value: string);
    procedure SetVisited(const Value: Boolean);
  protected
    procedure CheckValid;
    function FindNativeField(AField: TdxField): IdxRichEditField;

    property Document: TdxNativeSubDocument read FDocument;
    property HyperlinkInfo: TdxHyperlinkInfo read GetHyperlinkInfo;
    property Field: TdxField read FField;
    property PieceTable: TdxPieceTable read FPieceTable;
  public
    constructor Create(ADocument: TdxNativeSubDocument; APieceTable: TdxPieceTable; AField: TdxField);

    property IsValid: Boolean read FIsValid write FIsValid;

    property Anchor: string read GetAnchor write SetAnchor;
    property NavigateUri: string read GetNavigateUri write SetNavigateUri;
    property Range: IdxRichEditDocumentRange read GetRange;
    property Target: string read GetTarget write SetTarget;
    property ToolTip: string read GetToolTip write SetToolTip;
    property Visited: Boolean read GetVisited write SetVisited;
  end;

  { TdxNativeHyperlinkCollection }

  TdxNativeHyperlinkCollection = class(TdxIUnknownList<IdxRichEditHyperlink>,
    IdxRichEditHyperlinkCollection,
    IdxRichEditReadOnlyHyperlinkCollection)
  strict private
    FDocument: TdxNativeSubDocument;
    function GetCount: Integer;
  protected
    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyHyperlinkCollection;

    function CreateHyperlink(const AStart: IdxRichEditDocumentPosition; ALength: Integer): IdxRichEditHyperlink; overload;
    function CreateHyperlink(const ARange: IdxRichEditDocumentRange): IdxRichEditHyperlink; overload;
    function FindHyperlink(AFieldIndex: Integer; out AHyperlink: IdxRichEditHyperlink): Boolean;
    procedure RemoveHyperlink(const AHyperlink: IdxRichEditHyperlink);
  public
    constructor Create(ADocument: TdxNativeSubDocument);

    procedure PopulateHyperlinks;
  end;

  { TdxNativeBookmark }

  TdxNativeBookmark = class(TInterfacedObject, IdxRichEditBookmark)
  strict private
    FBookmark: TdxBookmark;
    FDocument: TdxNativeSubDocument;
    FIsValid: Boolean;

    function GetName: string;
    function GetRange: IdxRichEditDocumentRange;
  protected
    procedure CheckValid;
  public
    constructor Create(ADocument: TdxNativeSubDocument; ABookmark: TdxBookmark);

    property IsValid: Boolean read FIsValid write FIsValid;

    property Name: string read GetName;
    property Range: IdxRichEditDocumentRange read GetRange;
  end;

  { TdxNativeBookmarkCollection }

  TdxNativeBookmarkCollection = class(TdxIUnknownList<IdxRichEditBookmark>,
    IdxRichEditBookmarkCollection,
    IdxRichEditReadOnlyBookmarkCollection)
  strict private
    FDocument: TdxNativeSubDocument;
    FLastInsertedBookmarkIndex: Integer;

    procedure CheckValid;
    function FindByName(const AName: string): IdxRichEditBookmark;
    procedure RegisterBookmark(const ABookmark: TdxDocumentInterval);

    procedure OnBookmarkInserted(ASender: TObject; E: TdxDocumentIntervalEventArgs);
    procedure OnBookmarkRemoved(ASender: TObject; E: TdxDocumentIntervalEventArgs);

    function GetCount: Integer;
    function GetItem(const Name: string): IdxRichEditBookmark;
    function GetPieceTable: TdxPieceTable;
  protected
    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyBookmarkCollection;

    function CreateBookmark(const AStart: IdxRichEditDocumentPosition; ALength: Integer; const AName: string): IdxRichEditBookmark; overload;
    function CreateBookmark(const ARange: IdxRichEditDocumentRange; const AName: string): IdxRichEditBookmark; overload;
    procedure SelectBookmark(const ABookmark: IdxRichEditBookmark); overload;
    procedure RemoveBookmark(const ABookmark: IdxRichEditBookmark); overload;

    property PieceTable: TdxPieceTable read GetPieceTable;
  public
    constructor Create(ADocument: TdxNativeSubDocument);
    destructor Destroy; override;

    procedure SubscribeEvents;
    procedure UnsubscribeEvents;

    procedure PopulateBookmarks;
  end;

implementation

uses
  dxRichEdit.Api.Fields,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs;

{ TdxNativeHyperlink }

procedure TdxNativeHyperlink.CheckValid;
begin
  if not IsValid then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionUseDeletedHyperlinkError));
end;

function TdxNativeHyperlink.FindNativeField(AField: TdxField): IdxRichEditField;
var
  ANativeFields: TdxNativeFieldCollection;
  I, ACount: Integer;
begin
  ANativeFields := TdxNativeFieldCollection(Document.Fields);
  ACount := ANativeFields.Count;
  for I := 0 to ACount - 1 do
  begin
    Result := ANativeFields[I];
    if TdxNativeField(Result).Field = AField then
      Exit;
  end;
  Result := TdxNativeField.Create(FDocument, AField);
end;

constructor TdxNativeHyperlink.Create(ADocument: TdxNativeSubDocument;
  APieceTable: TdxPieceTable; AField: TdxField);
begin
  inherited Create;
  FDocument := ADocument;
  FPieceTable := APieceTable;
  FField := AField;
  FNativeField := FindNativeField(FField);
  FIsValid := True;
end;

function TdxNativeHyperlink.GetAnchor: string;
begin
  CheckValid;
  Result := HyperlinkInfo.Anchor;
end;

function TdxNativeHyperlink.GetHyperlinkInfo: TdxHyperlinkInfo;
begin
  CheckValid;
  Result := FPieceTable.HyperlinkInfos[FField.Index];
end;

procedure TdxNativeHyperlink.UpdateHyperlinkInfo(const Action: TUpdateHyperlinkInfoProc);
var
  AInfo: TdxHyperlinkInfo;
begin
  AInfo := HyperlinkInfo.Clone;
  Action(AInfo);
  PieceTable.ModifyHyperlinkCode(Field, AInfo);
end;

function TdxNativeHyperlink.GetNavigateUri: string;
begin
  CheckValid;
  Result := HyperlinkInfo.NavigateUri;
end;

function TdxNativeHyperlink.GetRange: IdxRichEditDocumentRange;
begin
  CheckValid;
  Result := FNativeField.Range;
end;

function TdxNativeHyperlink.GetTarget: string;
begin
  CheckValid;
  Result := HyperlinkInfo.Target;
end;

function TdxNativeHyperlink.GetToolTip: string;
begin
  CheckValid;
  Result := HyperlinkInfo.ToolTip;
end;

function TdxNativeHyperlink.GetVisited: Boolean;
begin
  CheckValid;
  Result := HyperlinkInfo.Visited;
end;

procedure TdxNativeHyperlink.SetAnchor(const Value: string);
begin
  CheckValid;
  UpdateHyperlinkInfo(procedure(AInfo: TdxHyperlinkInfo)
    begin
      AInfo.Anchor := Value;
    end);
end;

procedure TdxNativeHyperlink.SetNavigateUri(const Value: string);
begin
  CheckValid;
  UpdateHyperlinkInfo(procedure(AInfo: TdxHyperlinkInfo)
    begin
      AInfo.NavigateUri := Value;
    end);
end;

procedure TdxNativeHyperlink.SetTarget(const Value: string);
begin
  CheckValid;
  UpdateHyperlinkInfo(procedure(AInfo: TdxHyperlinkInfo)
    begin
      AInfo.Target := Value;
    end);
end;

procedure TdxNativeHyperlink.SetToolTip(const Value: string);
begin
  CheckValid;
  UpdateHyperlinkInfo(procedure(AInfo: TdxHyperlinkInfo)
    begin
      AInfo.ToolTip := Value;
    end);
end;

procedure TdxNativeHyperlink.SetVisited(const Value: Boolean);
begin
  CheckValid;
  UpdateHyperlinkInfo(procedure(AInfo: TdxHyperlinkInfo)
    begin
      AInfo.Visited := Value;
    end);
end;

{ TdxNativeHyperlinkCollection }

constructor TdxNativeHyperlinkCollection.Create(
  ADocument: TdxNativeSubDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

function TdxNativeHyperlinkCollection.CreateHyperlink(
  const ARange: IdxRichEditDocumentRange): IdxRichEditHyperlink;
begin
  Result := CreateHyperlink(ARange.Start, ARange.Length);
end;

function TdxNativeHyperlinkCollection.CreateHyperlink(
  const AStart: IdxRichEditDocumentPosition;
  ALength: Integer): IdxRichEditHyperlink;
var
  APrevCount: Integer;
  AInfo: TdxHyperlinkInfo;
begin
  Result := nil;
  if FDocument <> nil then
  begin
    FDocument.CheckValid;
    FDocument.CheckDocumentPosition(AStart);
    APrevCount := Count;
    AInfo := TdxHyperlinkInfo.Create;
    try
      FDocument.PieceTable.CreateHyperlink(AStart.LogPosition, ALength, AInfo);
    finally
      if Count > APrevCount then
      begin
        Result := Self[APrevCount];
      end
      else
        AInfo.Free;
    end;
  end;
end;

function TdxNativeHyperlinkCollection.FindHyperlink(AFieldIndex: Integer;
  out AHyperlink: IdxRichEditHyperlink): Boolean;
var
  I: Integer;
begin
  AHyperlink := nil;
  Result := False;
  for I := 0 to Count - 1 do
  begin
    if TdxNativeHyperlink(Self[I]).Field.Index = AFieldIndex then
    begin
      AHyperlink := Self[I];
      Result := True;
      Break;
    end;
  end;
end;

function TdxNativeHyperlinkCollection.Get(
  const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyHyperlinkCollection;
var
  AResult: TdxNativeHyperlinkCollection;
  I: Integer;
  AHyperlink: IdxRichEditHyperlink;
begin
  AResult := TdxNativeHyperlinkCollection.Create(FDocument);
  for I in FDocument.PieceTable.HyperlinkInfos do
  begin
    AHyperlink := TdxNativeHyperlink.Create(FDocument, FDocument.PieceTable, FDocument.PieceTable.Fields[I]);
    if ARange.Contains(AHyperlink.Range.Start) then
      AResult.Add(AHyperlink);
  end;
  Result := AResult;
end;

function TdxNativeHyperlinkCollection.GetCount: Integer;
begin
  Result := inherited Count;
end;

procedure TdxNativeHyperlinkCollection.PopulateHyperlinks;
var
  I: Integer;
  AHyperlink: IdxRichEditHyperlink;
begin
  Clear;
  for I in FDocument.PieceTable.HyperlinkInfos do
  begin
    AHyperlink := TdxNativeHyperlink.Create(FDocument, FDocument.PieceTable, FDocument.PieceTable.Fields[I]);
    Add(AHyperlink);
  end;
end;

procedure TdxNativeHyperlinkCollection.RemoveHyperlink(
  const AHyperlink: IdxRichEditHyperlink);
begin
  if FDocument <> nil then
  begin
    FDocument.CheckValid;
    FDocument.PieceTable.DeleteHyperlink(TdxNativeHyperlink(AHyperlink).Field);
    TdxNativeHyperlink(AHyperlink).IsValid := False;
  end;
end;

{ TdxNativeBookmark }

procedure TdxNativeBookmark.CheckValid;
begin
  if not IsValid then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionUseDeletedBookmarkError));
end;

constructor TdxNativeBookmark.Create(ADocument: TdxNativeSubDocument;
  ABookmark: TdxBookmark);
begin
  inherited Create;
  FDocument := ADocument;
  FBookmark := ABookmark;
  FIsValid := True;
end;

function TdxNativeBookmark.GetName: string;
begin
  CheckValid;
  Result := FBookmark.Name;
end;

function TdxNativeBookmark.GetRange: IdxRichEditDocumentRange;
var
  AStart, AEnd: TdxDocumentModelPosition;
begin
  CheckValid;
  AStart := FBookmark.Interval.Start;
  AEnd := FBookmark.Interval.&End;
  Result := TdxNativeDocumentRange.Create(FDocument, AStart, AEnd);
end;

{ TdxNativeBookmarkCollection }

constructor TdxNativeBookmarkCollection.Create(ADocument: TdxNativeSubDocument);
begin
  inherited Create;
  FDocument := ADocument;
  FLastInsertedBookmarkIndex := -1;
end;

destructor TdxNativeBookmarkCollection.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TdxNativeBookmarkCollection.CheckValid;
begin
  FDocument.CheckValid;
end;

function TdxNativeBookmarkCollection.FindByName(const AName: string): IdxRichEditBookmark;
var
  AResult: IdxRichEditBookmark;
begin
  Result := nil;
  for AResult in Self  do
    if CompareText(AResult.Name, AName) = 0 then
      Exit(AResult);
  Result := nil;
end;

procedure TdxNativeBookmarkCollection.RegisterBookmark(const ABookmark: TdxDocumentInterval);
begin
  Add(TdxNativeBookmark.Create(FDocument, TdxBookmark(ABookmark)));
end;

function TdxNativeBookmarkCollection.CreateBookmark(
  const ARange: IdxRichEditDocumentRange;
  const AName: string): IdxRichEditBookmark;
begin
  Result := CreateBookmark(ARange.Start, ARange.Length, AName);
end;

function TdxNativeBookmarkCollection.CreateBookmark(
  const AStart: IdxRichEditDocumentPosition; ALength: Integer;
  const AName: string): IdxRichEditBookmark;
begin
  FDocument.CheckValid;
  FDocument.CheckDocumentPosition(AStart);
  if FDocument.DocumentModel.ActivePieceTable.Bookmarks.FindByName(AName) <> nil then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionDuplicateBookmark));
  PieceTable.CreateBookmark(FDocument.NormalizeLogPosition(AStart.LogPosition), ALength, Trim(AName));
  Result := Self[FLastInsertedBookmarkIndex];
end;

function TdxNativeBookmarkCollection.Get(
  const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyBookmarkCollection;
var
  AResult: TdxNativeBookmarkCollection;
  ABookmark: IdxRichEditBookmark;
begin
  CheckValid;
  FDocument.CheckDocumentRange(ARange);
  AResult := TdxNativeBookmarkCollection.Create(FDocument);
  AResult.UnsubscribeEvents;
  for ABookmark in Self do
    if ARange.Contains(ABookmark.Range.Start) then
      AResult.Add(ABookmark);
  Result := AResult;
end;

function TdxNativeBookmarkCollection.GetCount: Integer;
begin
  Result := inherited Count;
end;

function TdxNativeBookmarkCollection.GetItem(
  const Name: string): IdxRichEditBookmark;
begin
  Result := FindByName(Name);
end;

function TdxNativeBookmarkCollection.GetPieceTable: TdxPieceTable;
begin
  Result := FDocument.PieceTable;
end;

procedure TdxNativeBookmarkCollection.OnBookmarkInserted(ASender: TObject;
  E: TdxDocumentIntervalEventArgs);
var
  ABookmark: TdxBookmark;
begin
  ABookmark := PieceTable.Bookmarks[E.Index];
  Insert(E.Index, TdxNativeBookmark.Create(FDocument, ABookmark));
  FLastInsertedBookmarkIndex := E.Index;
end;

procedure TdxNativeBookmarkCollection.OnBookmarkRemoved(ASender: TObject;
  E: TdxDocumentIntervalEventArgs);
begin
  TdxNativeBookmark(Self[E.Index]).IsValid := False;
  Delete(E.Index);
end;

procedure TdxNativeBookmarkCollection.PopulateBookmarks;
begin
  Clear;
  PieceTable.Bookmarks.ForEach(RegisterBookmark);
end;

procedure TdxNativeBookmarkCollection.RemoveBookmark(
  const ABookmark: IdxRichEditBookmark);
var
  I: Integer;
begin
  FDocument.CheckValid;
  for I := 0 to Count - 1 do
  begin
    if CompareText(Self[I].Name, ABookmark.Name) = 0 then
    begin
      PieceTable.DeleteBookmark(I);
      Break;
    end;
  end;
end;

procedure TdxNativeBookmarkCollection.SelectBookmark(
  const ABookmark: IdxRichEditBookmark);
var
  ARange: IdxRichEditDocumentRange;
begin
  FDocument.CheckValid;
  if PieceTable <> FDocument.DocumentModel.ActivePieceTable then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionSelectBookmarkError));
  ARange := ABookmark.Range;
  FDocument.CheckDocumentRange(ARange);
  FDocument.SetSelectionCore(ARange.Start.LogPosition, ARange.&End.LogPosition, False);
end;

procedure TdxNativeBookmarkCollection.SubscribeEvents;
var
  ABookmarks: TdxBookmarkCollection;
begin
  ABookmarks := PieceTable.Bookmarks;
  ABookmarks.Inserted.Add(OnBookmarkInserted);
  ABookmarks.Removed.Add(OnBookmarkRemoved);
end;

procedure TdxNativeBookmarkCollection.UnsubscribeEvents;
var
  ABookmarks: TdxBookmarkCollection;
begin
  ABookmarks := PieceTable.Bookmarks;
  ABookmarks.Inserted.Remove(OnBookmarkInserted);
  ABookmarks.Removed.Remove(OnBookmarkRemoved);
end;

end.
