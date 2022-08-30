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

unit cxCardViewEditor;

{$I cxVer.inc}

interface

uses
  Variants, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, Menus, StdCtrls,
  cxViewEditor, cxCustomTableViewEditor, cxGridCustomView, cxGridCardView,
  cxLookAndFeelPainters, cxButtons, cxPC, cxControls, cxGraphics, cxLookAndFeels, dxLayoutContainer,
  dxLayoutControlAdapters, dxLayoutLookAndFeels, cxClasses, cxContainer, cxEdit, cxListBox, dxLayoutControl;

type
  TcxCardViewEditor = class(TcxCustomTableViewEditor);

  TcxGridCardViewStorage = class(TcxCustomGridViewStorage)
  private
    function GetLayoutDirection: TcxGridCardViewLayoutDirection;
    function GetOptionsBehavior: TcxGridCardViewOptionsBehavior;
    function GetOptionsData: TcxGridCardViewOptionsData;
    function GetOptionsSelection: TcxGridCardViewOptionsSelection;
    function GetOptionsView: TcxGridCardViewOptionsView;
    procedure SetLayoutDirection(Value: TcxGridCardViewLayoutDirection);
  protected
    function CardView: TcxGridCardView;
  public
    class function GetViewClass: TcxCustomGridViewClass; override;
  published
    property LayoutDirection: TcxGridCardViewLayoutDirection read GetLayoutDirection write SetLayoutDirection;
    property OptionsBehavior: TcxGridCardViewOptionsBehavior read GetOptionsBehavior;
    property OptionsData: TcxGridCardViewOptionsData read GetOptionsData;
    property OptionsSelection: TcxGridCardViewOptionsSelection read GetOptionsSelection;
    property OptionsView: TcxGridCardViewOptionsView read GetOptionsView;
  end;

implementation

{$R *.dfm}

{ TcxGridCardViewStorage }

function TcxGridCardViewStorage.GetLayoutDirection: TcxGridCardViewLayoutDirection;
begin
  Result := CardView.LayoutDirection;
end;

function TcxGridCardViewStorage.GetOptionsBehavior: TcxGridCardViewOptionsBehavior;
begin
  Result := CardView.OptionsBehavior;
end;

function TcxGridCardViewStorage.GetOptionsData: TcxGridCardViewOptionsData;
begin
  Result := CardView.OptionsData;
end;

function TcxGridCardViewStorage.GetOptionsSelection: TcxGridCardViewOptionsSelection;
begin
  Result := CardView.OptionsSelection;
end;

function TcxGridCardViewStorage.GetOptionsView: TcxGridCardViewOptionsView;
begin
  Result := CardView.OptionsView;
end;

procedure TcxGridCardViewStorage.SetLayoutDirection(Value: TcxGridCardViewLayoutDirection);
begin
  CardView.LayoutDirection := Value;
end;

function TcxGridCardViewStorage.CardView: TcxGridCardView;
begin
  Result := inherited View as TcxGridCardView;
end;

class function TcxGridCardViewStorage.GetViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridCardView;
end;

initialization
  RegisterViewEditorClass(TcxGridCardView, TcxCardViewEditor);
  RegisterDefaultViewStorage(TcxGridCardViewStorage);

finalization
  UnregisterDefaultViewStorage(TcxGridCardViewStorage);
  UnregisterViewEditorClass(TcxGridCardView, TcxCardViewEditor);

end.
