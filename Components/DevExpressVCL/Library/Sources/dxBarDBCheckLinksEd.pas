{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars DB Navigator CheckLinks editor               }
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

unit dxBarDBCheckLinksEd;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, dxBarDBNav, dxBar;

type
  TdxBarDBCheckLinksEditor = class(TForm)
    GroupBox1: TGroupBox;
    ListBox: TListBox;
    GroupBox2: TGroupBox;
    ComboBox: TComboBox;
    CheckBox1: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox3: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    Button4: TButton;
    Button1: TButton;
    Button2: TButton;
    Bevel1: TBevel;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FBarDBNavigator: TdxBarDBNavigator;
    SelectedDBCheckLink: TdxBarDBCheckLink;
    procedure RefreshItems(OldIndex: Integer);
  end;

function dxBarDBCheckLinksEditor(ABarDBNavigator: TdxBarDBNavigator): Boolean;

implementation

{$R *.DFM}

function dxBarDBCheckLinksEditor(ABarDBNavigator: TdxBarDBNavigator): Boolean;
var
  AForm: TdxBarDBCheckLinksEditor;
  I: Integer;
  ADBCheckLinks: TdxBarDBCheckLinks;
begin
  AForm := TdxBarDBCheckLinksEditor.Create(nil);
  with AForm do
  begin
    FBarDBNavigator := ABarDBNavigator;
    ADBCheckLinks := TdxBarDBCheckLinks.Create(TdxBarDBCheckLink);
    ADBCheckLinks.Assign(FBarDBNavigator.DBCheckLinks);
    if FBarDBNavigator.BarManager <> nil then
      for I := 0 to FBarDBNavigator.BarManager.ItemCount - 1 do
        with FBarDBNavigator.BarManager do
          if Items[I].Category > -1 then
            ComboBox.Items.AddObject(Items[I].Name, Items[I]);
    RefreshItems(0);
  end;
  Result := AForm.ShowModal = mrOK;
  if not Result then
    ABarDBNavigator.DBCheckLinks.Assign(ADBCheckLinks);
  ADBCheckLinks.Free;
  AForm.Free;
end;

procedure TdxBarDBCheckLinksEditor.RefreshItems(OldIndex: Integer);
var
  I: Integer;
begin
  ListBox.Items.BeginUpdate;
  ListBox.Items.Clear;
  for I := 0 to FBarDBNavigator.DBCheckLinks.Count - 1 do
    ListBox.Items.AddObject(TdxBarDBCheckLink.ClassName + ' - ' + IntToStr(I),
      FBarDBNavigator.DBCheckLinks[I]);
  if OldIndex > ListBox.Items.Count - 1 then
    OldIndex := ListBox.Items.Count - 1;
  ListBox.Items.EndUpdate;
  ListBox.ItemIndex := OldIndex;
  ListBoxClick(nil);
end;

procedure TdxBarDBCheckLinksEditor.Button3Click(Sender: TObject);
begin
  ListBoxClick(nil);
  ModalResult := mrOK;
end;

procedure TdxBarDBCheckLinksEditor.Button4Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TdxBarDBCheckLinksEditor.ListBoxClick(Sender: TObject);
begin
  if (ListBox.ItemIndex = -1) and (ListBox.Items.Count > 0) then
    ListBox.ItemIndex := 0;

  if ListBox.ItemIndex > -1 then
  begin
    if SelectedDBCheckLink <> nil then
    begin
      if ComboBox.ItemIndex > -1 then
        SelectedDBCheckLink.Item := TdxBarItem(ComboBox.Items.Objects[ComboBox.ItemIndex]);
      with SelectedDBCheckLink do
      begin
        EnableTypes := [];
        if CheckBox1.Checked then EnableTypes := EnableTypes + [dxdbtCanModify];
        if CheckBox2.Checked then EnableTypes := EnableTypes + [dxdbtNotEOF];
        if CheckBox3.Checked then EnableTypes := EnableTypes + [dxdbtNotBOF];
        if CheckBox4.Checked then EnableTypes := EnableTypes + [dxdbtHasRecords];
        if CheckBox5.Checked then EnableTypes := EnableTypes + [dxdbtIsModified];
        if CheckBox6.Checked then EnableTypes := EnableTypes + [dxdbtIsNotModified];
      end;
    end;
    SelectedDBCheckLink := TdxBarDBCheckLink(ListBox.Items.Objects[ListBox.ItemIndex]);
    if SelectedDBCheckLink <> nil then
      with SelectedDBCheckLink do
      begin
        CheckBox1.Checked := dxdbtCanModify in EnableTypes;
        CheckBox2.Checked := dxdbtNotEOF in EnableTypes;
        CheckBox3.Checked := dxdbtNotBOF in EnableTypes;
        CheckBox4.Checked := dxdbtHasRecords in EnableTypes;
        CheckBox5.Checked := dxdbtIsModified in EnableTypes;
        CheckBox6.Checked := dxdbtIsNotModified in EnableTypes;
        ComboBox.ItemIndex := ComboBox.Items.IndexOfObject(Item);
      end;
  end
  else SelectedDBCheckLink := nil;
  GroupBox2.Enabled := ListBox.ItemIndex > -1;
end;

procedure TdxBarDBCheckLinksEditor.Button1Click(Sender: TObject);
var
  ABarDBCheckLink: TdxBarDBCheckLink;
begin
  ABarDBCheckLink := FBarDBNavigator.DBCheckLinks.Add;
  RefreshItems(ABarDBCheckLink.Index);
end;

procedure TdxBarDBCheckLinksEditor.Button2Click(Sender: TObject);
var
  ABarDBCheckLink: TdxBarDBCheckLink;
  PrevIndex: Integer;
begin
  if ListBox.Items.Count > 0 then
  begin
    ABarDBCheckLink := TdxBarDBCheckLink(ListBox.Items.Objects[ListBox.ItemIndex]);
    PrevIndex := ABarDBCheckLink.Index;
    ABarDBCheckLink.Free;
    SelectedDBCheckLink := nil;
    RefreshItems(PrevIndex);
  end;
end;

end.
