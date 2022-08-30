{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxShellEditorsReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  DesignEditors, DesignIntf, Windows, Classes, Forms, cxDBShellComboBox, cxEditPropEditors,
  cxEditRepositoryEditor, cxExtEditConsts, cxShellBrowserDialog, cxShellComboBox,
  cxShellCommon, cxShellEditRepositoryItems, cxShellListView, cxShellTreeView,
  cxLookAndFeels, dxShellBreadcrumbEdit;

const
  cxShellBrowserEditorVerb = 'Test Browser...';

type
  { TcxShellEditorSelectionEditor }

  TcxShellEditorSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TcxShellBrowserEditor }

  TcxShellBrowserEditor = class(TcxEditorsLibraryComponentEditorEx)
  protected
    function GetEditItemCaption: string; override;
    procedure ExecuteEditAction; override;
  end;

{ TcxShellEditorSelectionEditor }

procedure TcxShellEditorSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
{$IFDEF DELPHI16}
  Proc('Vcl.ComCtrls');
  Proc('Winapi.ShlObj');
{$ELSE}
  Proc('ComCtrls');
  Proc('ShlObj');
{$ENDIF}
  Proc('cxShellCommon');
end;

{ TcxShellBrowserEditor }

function TcxShellBrowserEditor.GetEditItemCaption: string;
begin
  Result := cxShellBrowserEditorVerb;
end;

procedure TcxShellBrowserEditor.ExecuteEditAction;
var
  ADialog: TcxShellBrowserDialog;
begin
ADialog := Component as TcxShellBrowserDialog;
with TcxShellBrowserDialog.Create(Application) do
  try
    if Length(ADialog.Title) > 0 then
      Title := ADialog.Title;
    if Length(ADialog.FolderLabelCaption) > 0 then
      FolderLabelCaption := ADialog.FolderLabelCaption;
    Options.ShowFolders := ADialog.Options.ShowFolders;
    Options.ShowToolTip := ADialog.Options.ShowToolTip;
    Options.TrackShellChanges := ADialog.Options.TrackShellChanges;
    Options.ContextMenus := ADialog.Options.ContextMenus;
    Options.ShowNonFolders := ADialog.Options.ShowNonFolders;
    Options.ShowHidden := ADialog.Options.ShowHidden;
    Options.ShowZipFilesWithFolders := ADialog.Options.ShowZipFilesWithFolders;
    Root.BrowseFolder := ADialog.Root.BrowseFolder;
    Root.CustomPath := ADialog.Root.CustomPath;
    LookAndFeel.Kind := ADialog.LookAndFeel.Kind;
    LookAndFeel.NativeStyle := ADialog.LookAndFeel.NativeStyle;
    LookAndFeel.SkinName := ADialog.LookAndFeel.SkinName;
    ShowButtons := ADialog.ShowButtons;
    ShowInfoTips := ADialog.ShowInfoTips;
    ShowLines := ADialog.ShowLines;
    ShowRoot := ADialog.ShowRoot;
    Path := ADialog.Path;
    Execute;
  finally
    Free;
  end;
end;

procedure Register;
begin
  RegisterComponents(cxEditorsLibraryProductPage, [TcxShellComboBox]);
  RegisterComponents(cxEditorsDBLibraryProductPage, [TcxDBShellComboBox]);
  RegisterComponents('Express Utilities', [TcxShellListView, TcxShellTreeView,
    TcxShellBrowserDialog, TdxShellBreadcrumbEdit]);
  RegisterSelectionEditor(TdxCustomShellBreadcrumbEdit, TcxShellEditorSelectionEditor);
  RegisterSelectionEditor(TcxCustomShellComboBox, TcxShellEditorSelectionEditor);
  RegisterSelectionEditor(TcxCustomShellListView, TcxShellEditorSelectionEditor);
  RegisterSelectionEditor(TcxCustomShellTreeView, TcxShellEditorSelectionEditor);
  RegisterComponentEditor(TcxShellBrowserDialog, TcxShellBrowserEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TcxDragDropSettings, 'Scroll', nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TcxCustomShellTreeView, 'RightClickSelect', nil);
end;

initialization
  RegisterEditRepositoryItem(TcxEditRepositoryShellComboBoxItem,
    scxSEditRepositoryShellComboBoxItem);

finalization
  UnregisterEditRepositoryItem(TcxEditRepositoryShellComboBoxItem);

end.
