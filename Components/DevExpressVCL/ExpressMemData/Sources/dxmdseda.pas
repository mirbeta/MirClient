{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMemData                                           }
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
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSMEMDATA                     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
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

unit dxmdseda;

interface

{$I cxVer.inc}

uses
  DesignIntf, Windows, Classes, Controls, Forms, StdCtrls, DB, dxmdaset, ExtCtrls, Graphics;

type
  IFormDesigner = IDesigner;

  TfrmdxMemDataAddField = class(TForm)
  private
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    pnlMain: TPanel;
    gbFieldProp: TGroupBox;
    edName: TEdit;
    cbFieldType: TComboBox;
    edComponent: TEdit;
    edSize: TEdit;
    gbFieldtype: TRadioGroup;
    gbLookup: TGroupBox;
    cbLookupField: TComboBox;
    cbKeyField: TComboBox;
    cbDataSet: TComboBox;
    cbResultField: TComboBox;

    procedure cbFieldTypeChange(Sender: TObject);
    procedure gbFieldtypeClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure edSizeKeyPress(Sender: TObject; var Key: Char);
    procedure edComponentChange(Sender: TObject);
    procedure cbDataSetExit(Sender: TObject);
  private

    Data: TdxMemData;
    LookupDS: TDataSet;
    FormDesigner: IFormDesigner;
    procedure GetDataSets(const AComponentName: string);
    procedure CreateControls;
  end;

function GetMemDataNewFieldType(Data: TdxMemData; X, Y: Integer; FormDesigner: IFormDesigner): TField;

implementation

uses
  SysUtils, TypInfo, Consts, RTLConsts, dxCore;

type
  TdxMemDataAccess = class(TdxMemData);
  TDummyField = class(TField)
  published
    property DataType;
  end;

function GetMemDataNewFieldType(Data: TdxMemData; X, Y: Integer; FormDesigner: IFormDesigner): TField;
var
  AForm: TfrmdxMemDataAddField;
  TypeInfo: PPropInfo;
  i: TFieldType;
  j: Integer;
begin
  Result := nil;
  AForm := TfrmdxMemDataAddField.CreateNew(nil, 0);
  try
    AForm.CreateControls;
    AForm.Data := Data;
    AForm.FormDesigner := FormDesigner;
    TypeInfo := GetPropInfo(TDummyField.ClassInfo, 'DataType');
    if TypeInfo <> nil then
    begin
      with AForm do
      begin
        for i := Low(TFieldType) to High(TFieldType) do
          if Data.SupportedFieldType(TFieldType(i)) then
            cbFieldType.Items.Add(GetEnumName(
              TypeInfo.PropType^, Integer(i)));

        cbFieldType.ItemIndex := 0;
        with Data do
          for j := 0 to FieldCount - 1 do
            if (Fields[j].Owner = Owner) and (Fields[j].FieldName <> '') then
              cbKeyField.Items.Add(Fields[j].FieldName);

        FormDesigner.GetComponentNames(GetTypeData(TDataset.ClassInfo), GetDataSets);

        Left := X;
        Top := Y;
        if ShowModal = mrOk then
        begin
          i := TFieldType(GetEnumValue(
            TypeInfo.PropType^, cbFieldType.Text));
          Result := TdxMemDataAccess(Data).GetFieldClass(i).Create(Data.Owner);
          with Result do
          begin
            try
              FieldName := edName.Text;
              DataSet := Data;
              Name := edComponent.Text;
            except
              Result.Free;
              raise;
            end;
            try
              if edSize.Text <> '' then
                TStringField(Result).Size := StrToInt(edSize.Text);
            except
            end;
            Calculated := gbFieldtype.ItemIndex = 1;
            Lookup := gbFieldtype.ItemIndex = 2;
            if Lookup then
            begin
              KeyFields := cbKeyField.Text;
              LookupDataSet := LookupDS;
              LookupKeyFields := cbLookupField.Text;
              LookupResultField := cbResultField.Text;
            end;
            if FormDesigner <> nil then
              FormDesigner.Modified;
          end;
        end;
      end;
    end;
  finally
    AForm.Free;
  end;
end;

procedure TfrmdxMemDataAddField.cbFieldTypeChange(Sender: TObject);
begin
  edSize.Enabled := (cbFieldType.Text = 'ftString') or (cbFieldType.Text = 'ftWideString');
  if not edSize.Enabled then
    edSize.Text := '';
end;

procedure TfrmdxMemDataAddField.gbFieldtypeClick(Sender: TObject);
begin
  cbKeyField.Enabled := gbFieldtype.ItemIndex = 2;
  cbDataSet.Enabled := cbKeyField.Enabled;
  cbLookupField.Enabled := cbKeyField.Enabled;
  cbResultField.Enabled := cbKeyField.Enabled;
  if not cbResultField.Enabled then
  begin
    cbKeyField.ItemIndex := -1;
    cbDataSet.Text := '';
    cbLookupField.ItemIndex := -1;
    cbResultField.ItemIndex := -1;
    LookupDS := nil;
  end;
end;

procedure TfrmdxMemDataAddField.edNameChange(Sender: TObject);
begin
  edComponent.Text := Data.Name + edName.Text;
  btnOk.Enabled := (edComponent.Text <> '') and (edName.Text <> '');
end;

procedure TfrmdxMemDataAddField.edSizeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not dxCharInSet(Key, [#8, '0'..'9']) then
  begin
    Key := #0;
    MessageBeep(0);
  end;
end;

procedure TfrmdxMemDataAddField.edComponentChange(Sender: TObject);
begin
  btnOk.Enabled := (edComponent.Text <> '') and (edName.Text <> '');
end;

procedure TfrmdxMemDataAddField.cbDataSetExit(Sender: TObject);
var
  Component: TComponent;
  i: Integer;
begin
  LookupDS := nil;
  cbLookupField.Items.Clear;
  cbResultField.Items.Clear;
  if not (csDesigning in Data.ComponentState) then
    Exit;
  if cbDataSet.Text = '' then
    Component := nil
  else
  begin
    Component := FormDesigner.GetComponent(cbDataSet.Text);
    if not (Component is TDataSet) then
    begin
      raise EPropertyError.Create(SInvalidPropertyValue);
      Component := nil;
      cbDataSet.Text := '';
    end;
  end;
  if Component <> nil then
  begin
    LookupDS := TDataSet(Component);
    if LookupDS.Active then
    begin
      for i := 0 to LookupDS.FieldCount - 1 do
        if LookupDS.Fields[i].FieldName <> '' then
          cbLookupField.Items.Add(LookupDS.Fields[i].FieldName)
    end
    else
    begin
      LookupDS.FieldDefs.Update;
      for i := 0 to LookupDS.FieldDefs.Count - 1 do
        if LookupDS.FieldDefs[i].Name <> '' then
          cbLookupField.Items.Add(LookupDS.FieldDefs[i].Name);
    end;
    cbResultField.Items.Assign(cbLookupField.Items);
  end;
end;

procedure TfrmdxMemDataAddField.GetDataSets(const AComponentName: string);
begin
  cbDataSet.Items.Add(AComponentName);
end;

procedure TfrmdxMemDataAddField.CreateControls;

  procedure CreateLabel(AParent: TWinControl; ALeft, ATop: Integer; ACaption: String);
  var
    ALabel: TLabel;
  begin
    ALabel := TLabel.Create(self);
    with ALabel do
    begin
      Parent := AParent;
      Left := ALeft;
      Top := ATop;
      Caption := ACaption;
    end;
  end;

  procedure CreateDummyTopPanel;
  var
    APanel: TPanel;
  begin
    APanel := TPanel.Create(self);
    with APanel do
    begin
      Parent := pnlMain;
      Top := self.Height;
      Height := 4;
      Align := alTop;
      BevelOuter := bvNone;
    end;
  end;

begin
  Width := 526;
  Height := 337;
  BorderIcons := [biSystemMenu];
    BorderStyle := bsDialog;
  Caption := 'New Field';
  Color := clBtnFace;
  Position := poScreenCenter;

  pnlBottom := TPanel.Create(self);
  with pnlBottom do
  begin
    Parent := self;
    Height := 38;
    Align := alBottom;
    BevelOuter := bvNone;
  end;

  btnOK := TButton.Create(self);
  with btnOK do
  begin
    Parent := pnlBottom;
    Left := 317;
    Top := 8;
    Width := 92;
    Height := 28;
    Caption := 'OK';
    Default := True;
    ModalResult := 1;
  end;

  btnCancel := TButton.Create(self);
  with btnCancel do
  begin
    Parent := pnlBottom;
    Left := 421;
    Top := 8;
    Width := 92;
    Height := 28;
    Cancel := True;
    Caption := 'Cancel';
    ModalResult := 2;
  end;

  pnlMain := TPanel.Create(self);
  with pnlMain do
  begin
    Parent := self;
    Align := alClient;
    BevelOuter := bvNone;
    BorderWidth := 4;
  end;

  gbFieldProp := TGroupBox.Create(self);
  with gbFieldProp do
  begin
    Parent := pnlMain;
    Height := 102;
    Align := alTop;
    Caption := 'Field Properties';
  end;

  CreateLabel(gbFieldProp, 11, 25, 'Name:');
  CreateLabel(gbFieldProp, 11, 65, 'Type:');
  CreateLabel(gbFieldProp, 251, 25, 'Component:');
  CreateLabel(gbFieldProp, 251, 65, 'Size:');

  edName := TEdit.Create(self);
  with edName do
  begin
    Parent := gbFieldProp;
    Left := 67;
    Top := 23;
    Width := 173;
    Height := 24;
    MaxLength := 32767;
    TabOrder := 0;
    OnChange := edNameChange;
  end;

  cbFieldType := TComboBox.Create(self);
  with cbFieldType do
  begin
    Parent := gbFieldProp;
    Left := 67;
    Top := 61;
    Width := 173;
    Height := 24;
    Style := csDropDownList;
    ItemHeight := 16;
    TabOrder := 2;
    OnChange := cbFieldTypeChange;
  end;

  edComponent := TEdit.Create(self);
  with edComponent do
  begin
    Parent := gbFieldProp;
    Left := 328;
    Top := 23;
    Width := 172;
    Height := 24;
    MaxLength := 32767;
    TabOrder := 1;
    OnChange := edComponentChange;
  end;

  edSize := TEdit.Create(self);
  with edSize do
  begin
    Parent := gbFieldProp;
    Left := 328;
    Top := 61;
    Width := 69;
    Height := 24;
    MaxLength := 32767;
    TabOrder := 3;
    OnKeyPress := edSizeKeyPress;
  end;

  CreateDummyTopPanel;

  gbFieldtype := TRadioGroup.Create(self);
  with gbFieldtype do
  begin
    Parent := pnlMain;
    Top := self.Height;
    Align := alTop;
    Caption := 'Field Type';
    Columns := 3;
    Items.Add('Data');
    Items.Add('Calculated');
    Items.Add('Lookup');
    ItemIndex := 0;
    Height := 57;
    OnClick := gbFieldtypeClick;
  end;

  CreateDummyTopPanel;

  gbLookup := TGroupBox.Create(self);
  with gbLookup do
  begin
    Parent := pnlMain;
    Height := 91;
    Top := self.Height;
    Align := alTop;
    Caption := 'Lookup Definition';
  end;

  CreateLabel(gbLookup, 11, 30, 'Key Field:');
  CreateLabel(gbLookup, 11, 57, 'Lookup Field:');
  CreateLabel(gbLookup, 270, 30, 'Dataset:');
  CreateLabel(gbLookup, 270, 57, 'Result Field:');

  cbKeyField := TComboBox.Create(self);
  with cbKeyField do
  begin
    Parent := gbLookup;
    Left := 100;
    Top := 23;
    Width := 149;
    Height := 24;
    Style := csDropDownList;
    Enabled := False;
    ItemHeight := 16;
    TabOrder := 0;
  end;

  cbLookupField := TComboBox.Create(self);
  with cbLookupField do
  begin
    Parent := gbLookup;
    Left := 100;
    Top := 57;
    Width := 149;
    Height := 24;
    Style := csDropDownList;
    Enabled := False;
    ItemHeight := 16;
    TabOrder := 1;
  end;

  cbDataSet := TComboBox.Create(self);
  with cbDataSet do
  begin
    Parent := gbLookup;
    Left := 355;
    Top := 23;
    Width := 149;
    Height := 24;
    Enabled := False;
    ItemHeight := 16;
    TabOrder := 2;
    OnExit := cbDataSetExit;
  end;

  cbResultField := TComboBox.Create(self);
  with cbResultField do
  begin
    Parent := gbLookup;
    Left := 355;
    Top := 57;
    Width := 149;
    Height := 24;
    Style := csDropDownList;
    Enabled := False;
    ItemHeight := 16;
    TabOrder := 3;
  end;

  ActiveControl := edName;

end;

end.
