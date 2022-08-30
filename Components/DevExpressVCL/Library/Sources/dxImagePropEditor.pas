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

unit dxImagePropEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, dximctrl;

type
  TfrmdxImagePropEditor = class(TForm)
    Panel1: TPanel;
    bAdd: TButton;
    bInsert: TButton;
    bDelete: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Edit1: TEdit;
    ListBox: TdxImageListBox;
    bUp: TButton;
    bDown: TButton;
    LabelText: TLabel;
    bClear: TButton;
    LabelValue: TLabel;
    Edit3: TEdit;
    LabelImageIndex: TLabel;
    Edit2: TEdit;
    BOk: TButton;
    bCancel: TButton;
    bHelp: TButton;
    SpinImage: TdxSpinImage;
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
    procedure Edit2Exit(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure bInsertClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure bUpClick(Sender: TObject);
    procedure bDownClick(Sender: TObject);
    procedure ListBoxDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure bClearClick(Sender: TObject);
    procedure Edit3Exit(Sender: TObject);
    procedure SpinImageChange(Sender: TObject; ItemIndex: Integer);
    procedure bHelpClick(Sender: TObject);
  private
  public
  end;

function ExpressImageItemsPropEditor(Control : TWinControl) : Boolean;

implementation

uses
  cxClasses, cxEditConsts;

{$R *.DFM}

const
  dxNewItemCaption = 'NewItem';

function ExpressImageItemsPropEditor(Control : TWinControl) : Boolean;
Var
  Form : TfrmdxImagePropEditor;
begin
  Result := False;
  Form := TfrmdxImagePropEditor.Create(Nil);
  Form.Caption := Form.Caption + Control.Name;
  if(Control is TdxCustomImageListBox) then
    Form.ListBox.Assign(TdxCustomImageListBox(Control));
  if(Control is TdxImageComboBox) then
    Form.ListBox.Assign(TdxImageComboBox(Control));
  Form.Edit3.Visible := True;
  Form.LabelValue.Visible := True;
  Form.SpinImage.ImageList := Form.ListBox.ImageList;
  Form.SpinImage.ItemIndex := -1;
  if(Form.ListBox.Items.Count > 0) then
    Form.ListBox.ItemIndex := 0;
  Form.ListBoxClick(Nil);
  Form.ShowModal;
  if(Form.ModalResult = mrOk) then begin
    if(Control is TdxCustomImageListBox) then
      TdxCustomImageListBox(Control).Assign(Form.ListBox);
    if(Control is TdxImageComboBox) then
      TdxImageComboBox(Control).Assign(Form.ListBox);
    Result := True;
  end;
  Form.Free;
end;

procedure TfrmdxImagePropEditor.Edit2KeyPress(Sender: TObject; var Key: Char);
begin
  if((Key < '0') Or (Key > '9')) And (Key <> Char(VK_BACK))
  And ((Key <> '-') Or ((Edit2.Text <> '') And (Edit2.Text <> Edit2.SelText))) then begin
    Key := #0;
    MessageBeep(0);
  end;
end;

procedure TfrmdxImagePropEditor.Edit2Exit(Sender: TObject);
begin
  if(ListBox.ItemIndex > -1) and (ListBox.Items.Count > 0) then begin
    if(Edit2.Text = '-') then
      Edit2.Text := '-1';
    SpinImage.ItemIndex := StrToInt(Edit2.Text);
    ListBox.ImageIndexes[ListBox.ItemIndex] := SpinImage.ItemIndex;
  end;
end;

procedure TfrmdxImagePropEditor.bAddClick(Sender: TObject);
begin
  ListBox.AddItem(dxNewItemCaption, -1);
  ListBox.ItemIndex := ListBox.Items.Count - 1;
  ListBoxClick(Sender);
  Edit1.SetFocus;
end;

procedure TfrmdxImagePropEditor.bInsertClick(Sender: TObject);
begin
  if(ListBox.ItemIndex > -1) and (ListBox.Items.Count > 0) then begin
    ListBox.InsertItem(ListBox.ItemIndex - 1, 'New Item', -1);
    ListBoxClick(Sender);
    Edit1.SetFocus;
  end
  else bAddClick(Sender);
end;

procedure TfrmdxImagePropEditor.bDeleteClick(Sender: TObject);
Var
  index : Integer;
begin
  if(ListBox.ItemIndex > -1) and (ListBox.Items.Count > 0) then begin
    index := ListBox.ItemIndex;
    ListBox.Items.Delete(ListBox.ItemIndex);
    if(index < ListBox.Items.Count) then
      ListBox.ItemIndex := index
    else if (index > 0) then
      ListBox.ItemIndex := index - 1;
    ListBoxClick(Sender);
  end;
end;

procedure TfrmdxImagePropEditor.Edit1Exit(Sender: TObject);
Var
  index, imindex : Integer;
  St : String;
begin
  if (ListBox.Items.Count > 0)
  and (Edit1.Text <> ListBox.Items[ListBox.ItemIndex]) then begin
    St := Edit1.Text;
    imindex := ListBox.ImageIndexes[ListBox.ItemIndex];
    index := ListBox.ItemIndex;
    ListBox.Items.Delete(index);
    ListBox.InsertItem(index, St, imindex);
    ListBox.ItemIndex := index;
  end;
end;

type
TtempAutoImageListBox = class(TdxImageListBox)
public
  property Values;
end;

procedure TfrmdxImagePropEditor.ListBoxClick(Sender: TObject);
begin
  Edit1.Enabled := (ListBox.ItemIndex > -1) and (ListBox.Items.Count > 0);
  Edit2.Enabled := Edit1.Enabled;
  SpinImage.Enabled := Edit1.Enabled;
  bDelete.Enabled := Edit1.Enabled;
  bUp.Enabled := Edit1.Enabled;
  bDown.Enabled := Edit1.Enabled;
  Edit3.Enabled := Edit1.Enabled;
  if(Edit1.Enabled) then begin
    Edit1.Text := ListBox.Items[ListBox.ItemIndex];
    Edit2.Text := IntToStr(ListBox.ImageIndexes[ListBox.ItemIndex]);
    SpinImage.ItemIndex := ListBox.ImageIndexes[ListBox.ItemIndex];
    Edit3.Text := TtempAutoImageListBox(ListBox).Values[ListBox.ItemIndex];
  end else begin
    Edit1.Text := '';
    Edit2.Text := '';
    Edit3.Text := '';
    SpinImage.ItemIndex := -1;
  end;
end;

procedure TfrmdxImagePropEditor.bUpClick(Sender: TObject);
Var
  index : Integer;
begin
  if(ListBox.ItemIndex > 0) and (ListBox.Items.Count > 0) then begin
    index := ListBox.ItemIndex;
    ListBox.ExchangeItems(index, index - 1);
    ListBox.ItemIndex := index - 1;
  end;
end;

procedure TfrmdxImagePropEditor.bDownClick(Sender: TObject);
Var
  index : Integer;
begin
  if(ListBox.ItemIndex < ListBox.Items.Count - 1) then begin
    index := ListBox.ItemIndex;
    ListBox.ExchangeItems(index, index + 1);
    ListBox.ItemIndex := index + 1;
  end;
end;

procedure TfrmdxImagePropEditor.ListBoxDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
Var
 p : TPoint;
 item : Integer;
begin
  Accept := False;
  p.x := X;
  p.y := Y;
  item := ListBox.ItemAtPos(p, True);
  if(item > -1) And (item < ListBox.Items.Count) then
    Accept := Not ListBox.Selected[Item];
end;

procedure TfrmdxImagePropEditor.ListBoxDragDrop(Sender, Source: TObject; X,
  Y: Integer);
Var
 p : TPoint;
 Item, Item1 : Integer;
 flag : Boolean;
begin
  ListBoxDragOver(Sender, Source, X, Y, dsDragLeave, flag);
  if Not flag then exit;
  p.x := X;
  p.y := Y;
  item := ListBox.ItemAtPos(p, True);
  if(item > -1) And (item < ListBox.Items.Count) then begin
    if(Item > ListBox.ItemIndex) then
      Inc(Item);
    ListBox.InsertItem(Item, ListBox.Items[ListBox.ItemIndex],
                            ListBox.ImageIndexes[ListBox.ItemIndex]);
    item1 := ListBox.ItemIndex;
    ListBox.Items.Delete(Item1);
    if(Item >= Item1) then
      Dec(Item);
    ListBox.ItemIndex := Item;
  end;
end;

procedure TfrmdxImagePropEditor.bClearClick(Sender: TObject);
begin
  ListBox.Items.Clear;
  ListBoxClick(Sender);
end;

procedure TfrmdxImagePropEditor.Edit3Exit(Sender: TObject);
begin
  if(ListBox.ItemIndex > -1) and (ListBox.Items.Count > 0)
  and (Edit3.Text <> TtempAutoImageListBox(ListBox).Values[ListBox.ItemIndex]) then
    TtempAutoImageListBox(ListBox).Values[ListBox.ItemIndex] := Edit3.Text;
end;

procedure TfrmdxImagePropEditor.SpinImageChange(Sender: TObject;
  ItemIndex: Integer);
begin
  if(ListBox.ItemIndex > -1) and (ListBox.Items.Count > 0) then begin
    Edit2.Text := IntToStr(ItemIndex);
    Edit2Exit(Sender);
  end;
end;

procedure TfrmdxImagePropEditor.bHelpClick(Sender: TObject);
begin
//  Application.HelpFile := 'aob_ps.hlp';
//  Application.HelpContext(10001);
end;

end.
