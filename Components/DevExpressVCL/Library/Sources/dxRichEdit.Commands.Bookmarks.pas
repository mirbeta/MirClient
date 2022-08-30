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

unit dxRichEdit.Commands.Bookmarks;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Commands,
  dxRichEdit.Commands.MultiCommand,
  dxRichEdit.Commands.IDs,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Utils.Types;

type

  { TdxCreateBookmarkCommand }

  TdxCreateBookmarkCommand = class(TdxRichEditMenuItemSimpleCommand)
  strict private
    FBookmarkName: string;
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    constructor Create(const AControl: IdxRichEditControl; const ABookmarkName: string); reintroduce;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property BookmarkName: string read FBookmarkName;
  end;

  { TdxSelectBookmarkCommand }

  TdxSelectBookmarkCommand = class(TdxRichEditSelectionCommand)
  strict private
    FBookmark: TdxBookmark;
  protected
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    function CreateEnsureCaretVisibleVerticallyCommand: TdxRichEditCommand; override;
  public
    constructor Create(const AControl: IdxRichEditControl; ABookmark: TdxBookmark); reintroduce;
    procedure ChangeSelection(ASelection: TdxSelection); override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property Bookmark: TdxBookmark read FBookmark;
  end;

  { TdxDeleteBookmarkCommand }

  TdxDeleteBookmarkCommand = class(TdxRichEditMenuItemSimpleCommand)
  strict private
    FBookmark: TdxBookmark;
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    constructor Create(const AControl: IdxRichEditControl; ABookmark: TdxBookmark); reintroduce;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property Bookmark: TdxBookmark read FBookmark;
  end;

  { TdxCreateBookmarkCoreCommand }

  TdxCreateBookmarkCoreCommand = class(TdxRichEditMenuItemSimpleCommand)
  strict private
    FPosition: TdxDocumentLogPosition;
    FBookmarkName: string;
    FLength: Integer;
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    constructor Create(const AControl: IdxRichEditControl; APosition: TdxDocumentLogPosition; ALength: Integer;
      const ABookmarkName: string); reintroduce;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property BookmarkName: string read FBookmarkName;
  end;

  { TdxShowBookmarkFormCommand }

  TdxShowBookmarkFormCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    function GetShowsModalDialog: Boolean; override;
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxCreateBookmarkContextMenuItemCommand }

  TdxCreateBookmarkContextMenuItemCommand = class(TdxShowBookmarkFormCommand)
  public
    class function Id: TdxRichEditCommandId; override;
  end;

implementation

uses
  dxCore,
  dxRichEdit.Commands.Images,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Utils.Exceptions;

{ TdxCreateBookmarkCommand }

constructor TdxCreateBookmarkCommand.Create(const AControl: IdxRichEditControl; const ABookmarkName: string);
begin
  inherited Create(AControl);
  Assert(ABookmarkName <> '', 'BookmarkName');
  FBookmarkName := ABookmarkName;
end;

procedure TdxCreateBookmarkCommand.ExecuteCore;
var
  ASelectionItems: TdxSelectionItemList;
  AStart, AEnd: TdxDocumentLogPosition;
  ALength: Integer;
  ACommand: TdxCreateBookmarkCoreCommand;
begin
  ASelectionItems := DocumentModel.Selection.Items;
  AStart := ASelectionItems[0].NormalizedStart;
  AEnd := ASelectionItems[ASelectionItems.Count - 1].NormalizedEnd;
  ALength := AEnd - AStart;
  ACommand := TdxCreateBookmarkCoreCommand.Create(RichEditControl, AStart, ALength, FBookmarkName);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

class function TdxCreateBookmarkCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandCreateBookmarkDescription);
end;

class function TdxCreateBookmarkCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandCreateBookmarkMenuCaption);
end;

procedure TdxCreateBookmarkCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Bookmarks);
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxSelectBookmarkCommand }

constructor TdxSelectBookmarkCommand.Create(const AControl: IdxRichEditControl; ABookmark: TdxBookmark);
begin
  inherited Create(AControl);
  Assert(ABookmark <> nil, 'Bookmark');
  FBookmark := ABookmark;
end;

function TdxSelectBookmarkCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxSelectBookmarkCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

class function TdxSelectBookmarkCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectBookmarkDescription);
end;

function TdxSelectBookmarkCommand.GetExtendSelection: Boolean;
begin
  Result := True;
end;

class function TdxSelectBookmarkCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSelectBookmarkMenuCaption);
end;

function TdxSelectBookmarkCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.Box;
end;

procedure TdxSelectBookmarkCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandsRestriction(AState, Options.DocumentCapabilities.Bookmarks);
  AState.Visible := True;
end;

function TdxSelectBookmarkCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxSelectBookmarkCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := APos.LogPosition;
end;

procedure TdxSelectBookmarkCommand.ChangeSelection(ASelection: TdxSelection);
begin
  ASelection.ClearMultiSelection;
  ASelection.Start := Bookmark.Start;
  ASelection.&End := FBookmark.&End;
end;

function TdxSelectBookmarkCommand.CreateEnsureCaretVisibleVerticallyCommand: TdxRichEditCommand;
var
  ACommand: TdxEnsureCaretVisibleVerticallyCommand;
begin
  Result := inherited CreateEnsureCaretVisibleVerticallyCommand;
  ACommand := Safe<TdxEnsureCaretVisibleVerticallyCommand>.Cast(Result);
  if ACommand <> nil then
    ACommand.RelativeCaretPosition := 0.3;
end;

{ TdxDeleteBookmarkCommand }

constructor TdxDeleteBookmarkCommand.Create(const AControl: IdxRichEditControl; ABookmark: TdxBookmark);
begin
  inherited Create(AControl);
  Assert(ABookmark <> nil, 'Bookmark');
  FBookmark := ABookmark;
end;

procedure TdxDeleteBookmarkCommand.ExecuteCore;
var
  APieceTable: TdxPieceTable;
  AIndex: Integer;
begin
  APieceTable := TdxPieceTable(FBookmark.PieceTable);
  AIndex := APieceTable.Bookmarks.IndexOf(FBookmark);
  if AIndex < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Bookmark', FBookmark);
  APieceTable.DeleteBookmark(AIndex);
end;

class function TdxDeleteBookmarkCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteBookmarkDescription);
end;

class function TdxDeleteBookmarkCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteBookmarkMenuCaption);
end;

procedure TdxDeleteBookmarkCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Bookmarks);
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxCreateBookmarkCoreCommand }

constructor TdxCreateBookmarkCoreCommand.Create(const AControl: IdxRichEditControl; APosition: TdxDocumentLogPosition;
  ALength: Integer; const ABookmarkName: string);
begin
  inherited Create(AControl);
  Assert(ABookmarkName <> '', 'BookmarkName');
  FBookmarkName := ABookmarkName;
  FPosition := APosition;
  FLength := ALength;
end;

class function TdxCreateBookmarkCoreCommand.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

class function TdxCreateBookmarkCoreCommand.GetDescription: string;
begin
  Result := 'InternalError';
end;

procedure TdxCreateBookmarkCoreCommand.ExecuteCore;
begin
  ActivePieceTable.CreateBookmark(FPosition, FLength, FBookmarkName);
end;

procedure TdxCreateBookmarkCoreCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Bookmarks);
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxShowBookmarkFormCommand }

procedure TdxShowBookmarkFormCommand.ExecuteCore;
begin
  RichEditControl.ShowBookmarkForm;
end;

class function TdxShowBookmarkFormCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandBookmarkDescription);
end;

class function TdxShowBookmarkFormCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandBookmarkMenuCaption);
end;

function TdxShowBookmarkFormCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

class function TdxShowBookmarkFormCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ShowBookmarkForm;
end;

class function TdxShowBookmarkFormCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.Bookmark;
end;

procedure TdxShowBookmarkFormCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Bookmarks);
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxCreateBookmarkContextMenuItemCommand }

class function TdxCreateBookmarkContextMenuItemCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.CreateBookmark;
end;

end.
