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

unit dxRichEdit.Commands.DragAndDrop;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Contnrs,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.View.Core,
  dxRichEdit.Commands.MultiCommand,
  dxRichEdit.Commands.CopyAndPaste,
  dxRichEdit.Commands.Selection;

type
  { TdxDragMoveContentCommandBase }

  TdxDragMoveContentCommandBase = class abstract(TdxTransactedMultiCommand)
  strict private
    FCopyCommand: TdxCopyAndSaveContentCommand;
    FSetSelectionCommand: TdxSimpleSetSelectionCommand;
    FPasteContentCommand: TdxCommand;
    FAnchor: TdxDocumentModelPositionAnchor;
  protected
    procedure CreateCommands; override;
    procedure ForceExecuteCore(const AState: IdxCommandUIState); override;
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;

    function CopyContent: Boolean; virtual; abstract;
    function CreateCopyContentCommand: TdxCopyAndSaveContentCommand; virtual;
    function CreatePasteContentCommand: TdxCommand; virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState; ACanDropOnSelection: Boolean); reintroduce;

    property PasteContentCommand: TdxCommand read FPasteContentCommand;
    property Anchor: TdxDocumentModelPositionAnchor read FAnchor;
  public
    constructor Create(const AControl: IdxRichEditControl; ATargetPosition: PdxDocumentModelPosition); reintroduce; virtual;
    destructor Destroy; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure UpdateUIState(const AState: IdxCommandUIState); override;
  end;

  { TdxDragCopyContentCommand }

  TdxDragCopyContentCommand = class(TdxDragMoveContentCommandBase)
  protected
    function CopyContent: Boolean; override;
  end;

  { TdxDragMoveContentCommand }

  TdxDragMoveContentCommand = class(TdxDragMoveContentCommandBase)
  protected
    function CopyContent: Boolean; override;
  end;

  { TdxDragCopyExternalContentCommand }

  TdxDragCopyExternalContentCommand = class(TdxDragCopyContentCommand)
  protected
    function CreateCopyContentCommand: TdxCopyAndSaveContentCommand; override;
    function CreatePasteContentCommand: TdxCommand; override;
    procedure SetSelection(ASelection: TdxSelection; const AStart: TdxDocumentLogPosition);
  public
    constructor Create(const AControl: IdxRichEditControl;
      ATargetPosition: PdxDocumentModelPosition; const ADataObject: IdxDataObject); reintroduce;

    procedure ForceExecute(const AState: IdxCommandUIState); override;
    procedure UpdateUIState(const AState: IdxCommandUIState); override;
  end;

implementation

uses
  dxCore,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Commands.Delete;

{ TdxDragMoveContentCommandBase }

constructor TdxDragMoveContentCommandBase.Create(
  const AControl: IdxRichEditControl;
  ATargetPosition: PdxDocumentModelPosition);
begin
  inherited Create(AControl);
  FSetSelectionCommand.Position := ATargetPosition;
  FAnchor := TdxDocumentModelPositionAnchor.Create(ATargetPosition);
end;

destructor TdxDragMoveContentCommandBase.Destroy;
begin
  FreeAndNil(FAnchor);
  inherited Destroy;
end;

procedure TdxDragMoveContentCommandBase.CreateCommands;
begin
  FCopyCommand := CreateCopyContentCommand;
  if FCopyCommand <> nil then
    Commands.Add(FCopyCommand);
  if not CopyContent then
    Commands.Add(TdxDeleteNonEmptySelectionCommand.Create(RichEditControl));
  FSetSelectionCommand := TdxSimpleSetSelectionCommand.Create(RichEditControl);
  Commands.Add(FSetSelectionCommand);
  FPasteContentCommand := CreatePasteContentCommand;
  Commands.Add(FPasteContentCommand);
end;

function TdxDragMoveContentCommandBase.CreateCopyContentCommand: TdxCopyAndSaveContentCommand;
begin
  Result := TdxCopyAndSaveContentCommand.Create(RichEditControl);
end;

function TdxDragMoveContentCommandBase.CreatePasteContentCommand: TdxCommand;
begin
  Result := TdxPasteSavedContentCommand.Create(RichEditControl, FCopyCommand);
end;

function TdxDragMoveContentCommandBase.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := TdxMultiCommandExecutionMode.ExecuteAllAvailable;
end;

procedure TdxDragMoveContentCommandBase.ForceExecuteCore(
  const AState: IdxCommandUIState);
begin
  DocumentModel.InternalAPI.RegisterAnchor(FAnchor);
  try
    inherited ForceExecuteCore(AState);
  finally
    DocumentModel.InternalAPI.UnregisterAnchor(FAnchor);
  end;
end;

class function TdxDragMoveContentCommandBase.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

class function TdxDragMoveContentCommandBase.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

procedure TdxDragMoveContentCommandBase.UpdateUIState(
  const AState: IdxCommandUIState);
begin
  UpdateUIStateCore(AState, False);
end;

procedure TdxDragMoveContentCommandBase.UpdateUIStateCore(
  const AState: IdxCommandUIState; ACanDropOnSelection: Boolean);
var
  ASelection: TdxSelection;
  AOffset: Integer;
  ACanExecute: Boolean;
begin
  if not ACanDropOnSelection then
  begin
    ASelection := DocumentModel.Selection;
    AOffset := Anchor.Position.LogPosition - ASelection.NormalizedStart;
    ACanExecute := (AOffset < 0 ) or (AOffset > ASelection.Length) or (ASelection.Length <= 0);
    if not ACanExecute then
    begin
      AState.Enabled := False;
      AState.Visible := True;
      Exit;
    end;
  end;
  inherited UpdateUIState(AState);
end;

function TdxDragMoveContentCommandBase.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := TdxMultiCommandUpdateUIStateMode.EnableIfAnyAvailable;
end;

{ TdxDragCopyContentCommand }

function TdxDragCopyContentCommand.CopyContent: Boolean;
begin
  Result := True;
end;

{ TdxDragMoveContentCommand }

function TdxDragMoveContentCommand.CopyContent: Boolean;
begin
  Result := False;
end;

{ TdxDragCopyExternalContentCommand }

constructor TdxDragCopyExternalContentCommand.Create(
  const AControl: IdxRichEditControl; ATargetPosition: PdxDocumentModelPosition;
  const ADataObject: IdxDataObject);
var
  ACommand: TdxPasteDataObjectCoreCommand;
begin
  inherited Create(AControl, ATargetPosition);
  ACommand := TdxPasteDataObjectCoreCommand(PasteContentCommand);
  ACommand.DataObject := ADataObject;
end;

function TdxDragCopyExternalContentCommand.CreateCopyContentCommand: TdxCopyAndSaveContentCommand;
begin
  Result := nil;
end;

function TdxDragCopyExternalContentCommand.CreatePasteContentCommand: TdxCommand;
begin
  Result := TdxPasteDataObjectCoreCommand.Create(RichEditControl, nil);
end;

procedure TdxDragCopyExternalContentCommand.ForceExecute(
  const AState: IdxCommandUIState);
var
  ADocumentModel: TdxDocumentModel;
  AStart: TdxDocumentLogPosition;
begin
  ADocumentModel := DocumentModel;
  AStart := Anchor.Position.LogPosition;
  inherited ForceExecute(AState);
  ADocumentModel.BeginUpdate;
  try
    SetSelection(ADocumentModel.Selection, AStart);
  finally
    ADocumentModel.EndUpdate;
  end;
end;

procedure TdxDragCopyExternalContentCommand.SetSelection(
  ASelection: TdxSelection; const AStart: TdxDocumentLogPosition);
begin
  ASelection.Start := AStart;
  ASelection.&End := Anchor.Position.LogPosition;
end;

procedure TdxDragCopyExternalContentCommand.UpdateUIState(
  const AState: IdxCommandUIState);
begin
  UpdateUIStateCore(AState, True);
end;

end.
