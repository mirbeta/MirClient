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

unit dxRichEdit.Dialogs.BookmarkFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.PieceTable,
  dxGenerics,
  dxRichEdit.Utils.Types;

type

  { TdxPredicate }

  TdxPredicate = reference to function: Boolean;

  { TdxBookmarkComparer }

  TdxBookmarkNameComparer = class(TdxDocumentIntervalComparer)
    function Compare(const Left, Right: TdxDocumentInterval): Integer; override;
  end;

  { TdxBookmarkFormControllerParameters }

  TdxBookmarkFormControllerParameters = class(TdxFormControllerParameters);

  { TdxBookmarkFormController }

  TdxBookmarkFormController = class(TdxFormController)
  strict private
    FControl: IdxRichEditControl;
    function GetPieceTable: TdxPieceTable;
  protected
    function GetBookmarks(AIncludeHiddenBookmarks: Boolean): TdxBookmarkList;
    function FindBookmarkByName(ABookmarks: TdxBookmarkList; const AName: string): TdxBookmark;
    procedure CreateBookmarkCore(const AName: string);

    property PieceTable: TdxPieceTable read GetPieceTable;
  public
    constructor Create(AControllerParameters: TdxBookmarkFormControllerParameters);
    procedure ApplyChanges; override;
    function GetBookmarksSortedByName(AIncludeHiddenBookmarks: Boolean = True): TdxBookmarkList;
    function GetBookmarksSortedByLocation(AIncludeHiddenBookmarks: Boolean = True): TdxBookmarkList;
    function ValidateName(const AName: string): Boolean; inline;
    procedure CreateBookmark(const AName: string; const ARecreate: TdxPredicate);
    procedure DeleteBookmark(ABookmark: TdxBookmark);
    procedure SelectBookmark(ABookmark: TdxBookmark);
    function CanSelectBookmark(ABookmark: TdxBookmark): Boolean;
    function GetCurrentBookmark: TdxBookmark;

    property Control: IdxRichEditControl read FControl;
  end;


implementation

uses
  dxRichEdit.DocumentModel.Exporter,
  dxRichEdit.Commands.Bookmarks;

{ TdxBookmarkComparer }

function TdxBookmarkNameComparer.Compare(const Left, Right: TdxDocumentInterval): Integer;
begin
  Result := AnsiCompareStr(TdxBookmark(Left).Name, TdxBookmark(Right).Name);
end;

{ TdxBookmarkFormController }

constructor TdxBookmarkFormController.Create(AControllerParameters: TdxBookmarkFormControllerParameters);
begin
  inherited Create;
  Assert(AControllerParameters <> nil, 'ControllerParameters');
  FControl := AControllerParameters.Control;
end;

function TdxBookmarkFormController.GetPieceTable: TdxPieceTable;
begin
  Result := Control.InnerControl.DocumentModel.Selection.PieceTable;
end;

function TdxBookmarkFormController.GetBookmarks(AIncludeHiddenBookmarks: Boolean): TdxBookmarkList;
begin
  Result := Control.InnerControl.DocumentModel.GetBookmarks(AIncludeHiddenBookmarks);
end;

procedure TdxBookmarkFormController.ApplyChanges;
begin
end;

function TdxBookmarkFormController.GetBookmarksSortedByName(AIncludeHiddenBookmarks: Boolean): TdxBookmarkList;
var
  AComparer: TdxBookmarkNameComparer;
begin
  Result := GetBookmarks(AIncludeHiddenBookmarks);
  AComparer := TdxBookmarkNameComparer.Create;
  try
    Result.Sort(AComparer);
  finally
    AComparer.Free;
  end;
end;

function TdxBookmarkFormController.GetBookmarksSortedByLocation(AIncludeHiddenBookmarks: Boolean): TdxBookmarkList;
begin
  Result := GetBookmarks(AIncludeHiddenBookmarks);
end;

procedure TdxBookmarkFormController.CreateBookmark(const AName: string; const ARecreate: TdxPredicate);
var
  ABookmarks: TdxBookmarkList;
  ABookmark: TdxBookmark;
begin
  ABookmarks := GetBookmarks(True);
  try
    ABookmark := FindBookmarkByName(ABookmarks, AName);
  finally
    ABookmarks.Free;
  end;
  if ABookmark <> nil then
  begin
    if not ARecreate then
      Exit;
    DeleteBookmark(ABookmark);
  end;
  CreateBookmarkCore(AName);
end;

function TdxBookmarkFormController.FindBookmarkByName(ABookmarks: TdxBookmarkList; const AName: string): TdxBookmark;
var
  ABookmark: TdxBookmark;
  I: Integer;
begin
  for I := 0 to ABookmarks.Count - 1 do
  begin
    ABookmark := ABookmarks[I];
    if ABookmark.Name = AName then
      Exit(ABookmark);
  end;
  Result := nil;
end;

function TdxBookmarkFormController.ValidateName(const AName: string): Boolean;
begin
  Result := TdxBookmark.IsNameValid(AName);
end;

procedure TdxBookmarkFormController.CreateBookmarkCore(const AName: string);
var
  ACommand: TdxCreateBookmarkCommand;
begin
  ACommand := TdxCreateBookmarkCommand.Create(Control, Trim(AName));
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxBookmarkFormController.DeleteBookmark(ABookmark: TdxBookmark);
var
  ACommand: TdxDeleteBookmarkCommand;
begin
  ACommand := TdxDeleteBookmarkCommand.Create(Control, ABookmark);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxBookmarkFormController.SelectBookmark(ABookmark: TdxBookmark);
var
  ACommand: TdxSelectBookmarkCommand;
begin
  if not CanSelectBookmark(ABookmark) then
    Exit;
  ACommand := TdxSelectBookmarkCommand.Create(Control, ABookmark);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxBookmarkFormController.CanSelectBookmark(ABookmark: TdxBookmark): Boolean;
begin
  Result := PieceTable = ABookmark.PieceTable;
end;

function TdxBookmarkFormController.GetCurrentBookmark: TdxBookmark;
var
  ALogPosition: TdxDocumentLogPosition;
  AIterator: TdxVisitableDocumentIntervalBoundaryIterator;
  ABoundaries: TdxVisitableDocumentIntervalBoundaryCollection;
  ACount, I: Integer;
  ABoundary: TdxVisitableDocumentIntervalBoundary;
begin
  Result := nil;
  ALogPosition := Control.InnerControl.ActiveView.CaretPosition.LogPosition;
  AIterator := TdxVisitableDocumentIntervalBoundaryIterator.Create(PieceTable);
  try
    ABoundaries := AIterator.Boundaries;
    ACount := ABoundaries.Count;
    for I := 0 to ACount - 1 do
    begin
      ABoundary := ABoundaries[I];
      if ALogPosition > ABoundary.Position.LogPosition then
      begin
        if (ABoundary.Order = TdxBookmarkBoundaryOrder.Start) and (ALogPosition <= ABoundary.VisitableInterval.&End) then
          Result := TdxBookmark(ABoundary.VisitableInterval);
        Continue;
      end;
      if ABoundary.Order = TdxBookmarkBoundaryOrder.Start then
      begin
        if Result <> nil then
          Exit
        else
          if I > 0 then
            Exit(TdxBookmark(ABoundaries[I - 1].VisitableInterval))
          else
            Exit(TdxBookmark(ABoundaries[ACount - 1].VisitableInterval));
      end;
    end;
    if not ((Result <> nil) or (ACount = 0)) then
      Result := TdxBookmark(ABoundaries[ACount - 1].VisitableInterval);
  finally
    AIterator.Free;
  end;
end;

end.
