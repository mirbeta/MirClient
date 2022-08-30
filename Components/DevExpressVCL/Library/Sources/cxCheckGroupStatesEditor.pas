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

unit cxCheckGroupStatesEditor;

interface

{$I cxVer.inc}

uses
  Variants, Windows, Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, Messages,
  StdCtrls, SysUtils, cxButtons, cxCheckListBox, cxContainer, cxControls,
  cxLookAndFeelPainters, cxGraphics, cxLookAndFeels, Menus, cxEdit;

type
  TcxCheckGroupStatesEditorDlg = class(TForm)
    clbStates: TcxCheckListBox;
    Panel1: TPanel;
    btnOK: TcxButton;
    btnCancel: TcxButton;
    procedure FormShow(Sender: TObject);
  end;

var
  cxCheckGroupStatesEditorDlg: TcxCheckGroupStatesEditorDlg;

implementation

{$R *.dfm}

procedure TcxCheckGroupStatesEditorDlg.FormShow(Sender: TObject);
var
  AItemWidth, AMaxItemWidth, I: Integer;
begin
  AMaxItemWidth := 0;
  clbStates.Canvas.Font := clbStates.Style.Font;
  for I := 0 to clbStates.Items.Count - 1 do
  begin
    AItemWidth := clbStates.Canvas.TextWidth(clbStates.Items[I].Text);
    if AMaxItemWidth < AItemWidth then
      AMaxItemWidth := AItemWidth;
  end;
  clbStates.ScrollWidth := AMaxItemWidth + 25;
end;

end.
