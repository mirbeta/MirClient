{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars item editor                                  }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxBarItemEd;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Menus, dxBar, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, cxButtons, cxControls, cxContainer, cxEdit, cxClasses, cxTextEdit, cxMaskEdit,
  cxDropDownEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer, dxLayoutControl, dxLayoutLookAndFeels;

type
  TdxBarItemAddEditor = class(TForm)
    BCancel: TcxButton;
    BOk: TcxButton;
    ComboBox1: TcxComboBox;
    ComboBox2: TcxComboBox;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    Edit1: TcxTextEdit;
    Edit2: TcxTextEdit;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;

    procedure BOkClick(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    BarItem: TdxBarItem;
    BarManager: TdxBarManager;
  end;

function dxBarItemAddEditor(ABarManager: TdxBarManager; ACategoryIndex: Integer): TdxBarItem;

implementation

{$R *.DFM}

uses
  TypInfo, dxBarCustomCustomizationForm, dxBarStrs;

function dxBarItemAddEditor(ABarManager: TdxBarManager; ACategoryIndex: Integer): TdxBarItem;
var
  AForm: TdxBarItemAddEditor;
  I: Integer;
begin
  Result := nil;
  AForm := TdxBarItemAddEditor.Create(nil);
  try
    AForm.BarManager := ABarManager;
    AForm.Font := dxBarCustomizingForm.Font;

    for I := 0 to AForm.BarManager.Categories.Count - 1 do
      AForm.ComboBox2.Properties.Items.Add(AForm.BarManager.Categories[I]);
    if (0 <= ACategoryIndex) and (ACategoryIndex < AForm.BarManager.Categories.Count) then
      AForm.ComboBox2.ItemIndex := ACategoryIndex
    else
      AForm.ComboBox2.ItemIndex := 0;

    for I := 0 to RegdxItemList.VisibleItemCount - 1 do
      AForm.ComboBox1.Properties.Items.Add(RegdxItemList.VisibleItemClass[I].ClassName);
    AForm.ComboBox1.ItemIndex := 0;
    AForm.ComboBox1Click(nil);

    if AForm.ShowModal = mrOK then
      Result := AForm.BarItem;
  finally
    AForm.Free;
  end;
end;

{ TdxBarItemAddEditor }

procedure TdxBarItemAddEditor.FormShow(Sender: TObject);
begin
  Edit1.SetFocus;
end;

procedure TdxBarItemAddEditor.ComboBox1Click(Sender: TObject);
begin
  Edit1.Text := BarManager.GetUniqueItemName(RegdxItemList.VisibleItemClass[ComboBox1.ItemIndex]);
end;

procedure TdxBarItemAddEditor.BOkClick(Sender: TObject);
begin
  BarItem := RegdxItemList.VisibleItemClass[ComboBox1.ItemIndex].Create(BarManager.Owner);
  try
    BarItem.Name := Edit1.Text;
    BarItem.Category := ComboBox2.ItemIndex;
    BarItem.Caption := Edit2.Text;
  except
    FreeAndNil(BarItem);
    ComboBox1Click(Sender);
    ModalResult := mrNone;
  end;
end;

end.
