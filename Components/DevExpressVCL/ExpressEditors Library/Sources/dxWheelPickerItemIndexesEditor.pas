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

unit dxWheelPickerItemIndexesEditor;

interface

{$I cxVer.inc}

uses
  Variants, Windows, Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, Messages, Menus, StdCtrls, SysUtils,
  cxButtons, cxContainer, cxControls, cxLookAndFeelPainters, cxGraphics, cxLookAndFeels, cxEdit, dxWheelPicker;

type

  { TfrmWheelPickerItemIndexesEditor }

  TfrmWheelPickerItemIndexesEditor = class(TForm)
    Panel1: TPanel;
    btnOK: TcxButton;
    btnCancel: TcxButton;
    dxWheelPicker1: TdxWheelPicker;
    procedure FormShow(Sender: TObject);
  end;

procedure dxShowWheelPickerItemIndexesEditor(AWheelPicker: TdxWheelPicker);

implementation

{$R *.dfm}

procedure dxShowWheelPickerItemIndexesEditor(AWheelPicker: TdxWheelPicker);
var
  AForm: TfrmWheelPickerItemIndexesEditor;
  AOwnerCaption: string;
  I: Integer;
begin
  AForm := TfrmWheelPickerItemIndexesEditor.Create(nil);
  try
    if AWheelPicker.Owner <> nil then
      AOwnerCaption := AWheelPicker.Owner.Name + '.'
    else
      AOwnerCaption := '';
    AForm.Caption := Format('%s%s -  Item indexes editor', [AOwnerCaption, AWheelPicker.Name]);
    AForm.dxWheelPicker1.Properties.Assign(AWheelPicker.Properties);
    AForm.dxWheelPicker1.Width := AWheelPicker.Width;
    AForm.dxWheelPicker1.Height := AWheelPicker.Height;
    AForm.dxWheelPicker1.AutoSize := True;
    for I := 0 to AWheelPicker.Properties.Wheels.Count - 1 do
      AForm.dxWheelPicker1.ItemIndexes[I] := AWheelPicker.ItemIndexes[I];
    if AForm.ShowModal = mrOk then
    begin
      for I := 0 to AWheelPicker.Properties.Wheels.Count - 1 do
        AWheelPicker.ItemIndexes[I] := AForm.dxWheelPicker1.ItemIndexes[I];
      SetDesignerModified(AWheelPicker);
    end;
  finally
    AForm.Release;
  end;
end;

{ TfrmWheelPickerItemIndexesEditor }

procedure TfrmWheelPickerItemIndexesEditor.FormShow(Sender: TObject);
begin
  AutoSize := True;
  dxWheelPicker1.Align := alLeft;
  Panel1.Align := alLeft;
end;

end.
