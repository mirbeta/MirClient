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

unit cxLayoutViewEditor;

{$I cxVer.inc}

interface

uses
  Variants, Types, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, Menus, StdCtrls,
  cxViewEditor, cxCustomTableViewEditor, cxGridCustomView, cxGridLayoutView,
  cxLookAndFeelPainters, cxButtons, cxPC, cxControls, cxGraphics, cxLookAndFeels, dxLayoutContainer,
  dxLayoutControlAdapters, dxLayoutLookAndFeels, cxClasses, cxContainer, cxEdit, cxListBox, dxLayoutControl;

type
  TcxLayoutViewEditor = class(TcxCustomTableViewEditor);

  TcxGridLayoutViewStorage = class(TcxCustomGridViewStorage)
  private
    function GetOptionsBehavior: TcxGridLayoutViewOptionsBehavior;
    function GetOptionsData: TcxGridLayoutViewOptionsData;
    function GetOptionsSelection: TcxGridLayoutViewOptionsSelection;
    function GetOptionsView: TcxGridLayoutViewOptionsView;
  protected
    function LayoutView: TcxGridLayoutView;
  public
    class function GetViewClass: TcxCustomGridViewClass; override;
  published
    property OptionsBehavior: TcxGridLayoutViewOptionsBehavior read GetOptionsBehavior;
    property OptionsData: TcxGridLayoutViewOptionsData read GetOptionsData;
    property OptionsSelection: TcxGridLayoutViewOptionsSelection read GetOptionsSelection;
    property OptionsView: TcxGridLayoutViewOptionsView read GetOptionsView;
  end;

implementation

uses
  cxGridCustomLayoutView, cxGridCustomTableView;

{$R *.dfm}

{ TcxGridLayoutViewStorage }

function TcxGridLayoutViewStorage.GetOptionsBehavior: TcxGridLayoutViewOptionsBehavior;
begin
  Result := LayoutView.OptionsBehavior;
end;

function TcxGridLayoutViewStorage.GetOptionsData: TcxGridLayoutViewOptionsData;
begin
  Result := LayoutView.OptionsData;
end;

function TcxGridLayoutViewStorage.GetOptionsSelection: TcxGridLayoutViewOptionsSelection;
begin
  Result := LayoutView.OptionsSelection;
end;

function TcxGridLayoutViewStorage.GetOptionsView: TcxGridLayoutViewOptionsView;
begin
  Result := LayoutView.OptionsView;
end;

function TcxGridLayoutViewStorage.LayoutView: TcxGridLayoutView;
begin
  Result := inherited View as TcxGridLayoutView;
end;

class function TcxGridLayoutViewStorage.GetViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridLayoutView;
end;

initialization
  RegisterViewEditorClass(TcxGridLayoutView, TcxLayoutViewEditor);
  RegisterDefaultViewStorage(TcxGridLayoutViewStorage);

finalization
  UnregisterDefaultViewStorage(TcxGridLayoutViewStorage);
  UnregisterViewEditorClass(TcxGridLayoutView, TcxLayoutViewEditor);

end.
