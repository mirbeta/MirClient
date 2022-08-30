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

unit dxRichEdit.Commands.FileOperations;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Graphics, Windows, Controls, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.View.Core,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Options,
  dxRichEdit.Commands,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.Insert,
  dxRichEdit.Commands.MultiCommand;

type
  { TdxSaveDocumentCommand }

  TdxSaveDocumentCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSaveDocumentAsCommand }

  TdxSaveDocumentAsCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    function GetShowsModalDialog: Boolean; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxLoadDocumentCommand }

  TdxLoadDocumentCommand = class(TdxRichEditMenuItemSimpleCommand)
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

  { TdxCreateEmptyDocumentCommand }

  TdxCreateEmptyDocumentCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  dxCore,
  dxRichEdit.Commands.Images,
  dxRichEdit.Commands.Strs;

{ TdxSaveDocumentCommand }

procedure TdxSaveDocumentCommand.ExecuteCore;
begin
  CheckExecutedAtUIThread;
  if InnerControl.SaveDocument then
    InnerControl.Modified := False;
end;

class function TdxSaveDocumentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSaveDocumentDescription)
end;

class function TdxSaveDocumentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSaveDocumentMenuCaption)
end;

class function TdxSaveDocumentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FileSave;
end;

class function TdxSaveDocumentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SaveDocument;
end;

procedure TdxSaveDocumentCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandsRestriction(AState, Options.Behavior.Save, InnerControl.Modified);
end;

{ TdxSaveDocumentAsCommand }

procedure TdxSaveDocumentAsCommand.ExecuteCore;
begin
  CheckExecutedAtUIThread;
  if InnerControl.SaveDocumentAs then
    InnerControl.Modified := False;
end;

class function TdxSaveDocumentAsCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSaveDocumentAsDescription)
end;

class function TdxSaveDocumentAsCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSaveDocumentAsMenuCaption)
end;

class function TdxSaveDocumentAsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FileSaveAs;
end;

class function TdxSaveDocumentAsCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SaveDocumentAs;
end;

function TdxSaveDocumentAsCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

procedure TdxSaveDocumentAsCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandsRestriction(AState, Options.Behavior.SaveAs);
end;

{ TdxLoadDocumentCommand }

procedure TdxLoadDocumentCommand.ExecuteCore;
begin
  CheckExecutedAtUIThread;
  if InnerControl.CanCloseExistingDocument then
    InnerControl.LoadDocument;
end;

class function TdxLoadDocumentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandLoadDocumentDescription);
end;

class function TdxLoadDocumentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandLoadDocumentMenuCaption);
end;

class function TdxLoadDocumentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FileOpen;
end;

class function TdxLoadDocumentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.LoadDocument;
end;

function TdxLoadDocumentCommand.GetShowsModalDialog: Boolean;
begin
  Result := True;
end;

procedure TdxLoadDocumentCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandsRestriction(AState, Options.Behavior.Open);
end;

{ TdxCreateEmptyDocumentCommand }

procedure TdxCreateEmptyDocumentCommand.ExecuteCore;
begin
  CheckExecutedAtUIThread;
  DocumentModel.ClearDataSources;
  if InnerControl.CanCloseExistingDocument then
  begin
    DocumentModel.BeginUpdate;
    try
      DocumentModel.Modified := False;
      DocumentModel.InternalAPI.CreateNewDocument;
    finally
      DocumentModel.EndUpdate;
    end;
  end;
end;

class function TdxCreateEmptyDocumentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandNewEmptyDocumentDescription);
end;

class function TdxCreateEmptyDocumentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandNewEmptyDocumentMenuCaption);
end;

class function TdxCreateEmptyDocumentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.FileNew;
end;

class function TdxCreateEmptyDocumentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.NewDocument;
end;

procedure TdxCreateEmptyDocumentCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  ApplyCommandRestrictionOnEditableControl(AState, Options.Behavior.CreateNew);
end;

end.
