{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxWinExplorerViewEditor;

{$I cxVer.inc}

interface

uses
  Variants, Types, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, Menus, StdCtrls,
  cxViewEditor, cxCustomTableViewEditor, cxGridCustomView, cxGridWinExplorerView,
  cxLookAndFeelPainters, cxButtons, cxPC, cxControls, cxGraphics, cxLookAndFeels, dxLayoutContainer,
  dxLayoutControlAdapters, dxLayoutLookAndFeels, cxClasses, cxContainer, cxEdit, cxListBox, dxLayoutControl;

type
  { TcxWinExplorerViewEditor }

  TcxWinExplorerViewEditor = class(TcxCustomTableViewEditor);

  { TcxGridWinExplorerViewStorage }

  TcxGridWinExplorerViewStorage = class(TcxCustomGridViewStorage)
  private
    function GetOptionsBehavior: TcxGridWinExplorerViewOptionsBehavior;
    function GetOptionsData: TcxGridWinExplorerViewOptionsData;
    function GetOptionsSelection: TcxGridWinExplorerViewOptionsSelection;
    function GetOptionsView: TcxGridWinExplorerViewOptionsView;
  protected
    function WinExplorerView: TcxGridWinExplorerView;
  public
    class function GetViewClass: TcxCustomGridViewClass; override;
  published
    property OptionsBehavior: TcxGridWinExplorerViewOptionsBehavior read GetOptionsBehavior;
    property OptionsData: TcxGridWinExplorerViewOptionsData read GetOptionsData;
    property OptionsSelection: TcxGridWinExplorerViewOptionsSelection read GetOptionsSelection;
    property OptionsView: TcxGridWinExplorerViewOptionsView read GetOptionsView;
  end;

implementation

uses
  cxGridCustomTableView;

{$R *.dfm}

{ TcxGridWinExplorerViewStorage }

function TcxGridWinExplorerViewStorage.GetOptionsBehavior: TcxGridWinExplorerViewOptionsBehavior;
begin
  Result := WinExplorerView.OptionsBehavior;
end;

function TcxGridWinExplorerViewStorage.GetOptionsData: TcxGridWinExplorerViewOptionsData;
begin
  Result := WinExplorerView.OptionsData;
end;

function TcxGridWinExplorerViewStorage.GetOptionsSelection: TcxGridWinExplorerViewOptionsSelection;
begin
  Result := WinExplorerView.OptionsSelection;
end;

function TcxGridWinExplorerViewStorage.GetOptionsView: TcxGridWinExplorerViewOptionsView;
begin
  Result := WinExplorerView.OptionsView;
end;

function TcxGridWinExplorerViewStorage.WinExplorerView: TcxGridWinExplorerView;
begin
  Result := inherited View as TcxGridWinExplorerView;
end;

class function TcxGridWinExplorerViewStorage.GetViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridWinExplorerView;
end;

initialization
  RegisterViewEditorClass(TcxGridWinExplorerView, TcxWinExplorerViewEditor);
  RegisterDefaultViewStorage(TcxGridWinExplorerViewStorage);

finalization
  UnregisterDefaultViewStorage(TcxGridWinExplorerViewStorage);
  UnregisterViewEditorClass(TcxGridWinExplorerView, TcxWinExplorerViewEditor);

end.
