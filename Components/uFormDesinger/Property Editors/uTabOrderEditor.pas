unit uTabOrderEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  uDesigner, StdCtrls, Buttons, ExtCtrls;

type
  TfrmTabOrder = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Panel3: TPanel;
    Panel4: TPanel;
    OBJList: TListBox;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure OBJListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure OBJListClick(Sender: TObject);
  private
    FDesigner: TCustomDesigner;
  public
    constructor create(Owner: TComponent; BusinessDesigner: TCustomDesigner);
  end;

var
  frmTabOrder: TfrmTabOrder;

implementation

{$R *.dfm}

constructor TfrmTabOrder.create(Owner: TComponent;
  BusinessDesigner: TCustomDesigner);
begin
  inherited create(Owner);
  FDesigner :=  BusinessDesigner;
end;

procedure TfrmTabOrder.FormShow(Sender: TObject);
  procedure AddWinctrols(Parent: TWinControl);
  var
    i, j: integer;
    tabOrder, newtabOrder: integer;
  begin
    for i :=  0 to parent.ControlCount - 1 do
      if (parent.Controls[i] is TWinControl) and
        (parent.Controls[i].Owner = FDesigner.DesignForm {≈≈≥˝∞À∏ˆ–°µ„}) then
        OBJList.Items.AddObject(parent.Controls[i].Name, parent.Controls[i]);
    //≈≈–Ú, ∞¥TabOrder≈≈–Ú
    tabOrder  :=  -1;
    for i :=  0 to OBJList.Items.Count - 1 do
      for j :=  i + 1 to OBJList.Items.Count - 1 do
      begin
        newtabOrder :=  TWinControl(OBJList.Items.Objects[j]).TabOrder;
        if newtabOrder < TWinControl(OBJList.Items.Objects[i]).TabOrder then
          OBJList.Items.Move(j, i);
      end;
  end;
var
  i: integer;
  c: TWinControl;
begin
  OBJList.Clear;
  if FDesigner.SelectionCount = 1 then
  begin
    if FDesigner.Selections[0] is TWinControl then
    begin
      c :=  TWinControl(FDesigner.Selections[0]);
      if  (c.ControlCount = 0) and (c <> FDesigner.DesignForm) then
        AddWinctrols(c.Parent)
      else
        AddWinctrols(c);
      c :=  nil;
    end;
  end else
  begin
    for i :=  0 to FDesigner.SelectionCount - 1 do
      if FDesigner.Selections[i] is TWinControl then
      begin
        c :=  TWinControl(FDesigner.Selections[i]);
        AddWinctrols(c.Parent);
        c :=  nil;
        break;
      end;
  end;
end;

procedure TfrmTabOrder.BitBtn4Click(Sender: TObject);
var
  i: integer;
begin
  for i :=  0 to OBJList.Items.Count - 1 do
    TWinControl(OBJList.Items.Objects[i]).TabOrder  :=  i;
  Close;
  OBJList.Clear;
end;

procedure TfrmTabOrder.OBJListClick(Sender: TObject);
begin
  if OBJList.ItemIndex<>-1 then
    FDesigner.Select([OBJList.Items.Objects[OBJList.ItemIndex] as TPersistent]);
end;

procedure TfrmTabOrder.OBJListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  OBJList.Canvas.TextRect(Rect, Rect.Left + 1, Rect.Top + 1,
                            inttostr(Index)+':  '+OBJList.Items[Index]);
end;

procedure TfrmTabOrder.SpeedButton1Click(Sender: TObject);
var
  i: integer;
begin
  if OBJList.ItemIndex > 0 then
  begin
    i :=  OBJList.ItemIndex;
    OBJList.Items.Move(i, i - 1);
    OBJList.ItemIndex :=  i - 1;
  end;
end;

procedure TfrmTabOrder.SpeedButton2Click(Sender: TObject);
var
  i: integer;
begin
  if OBJList.ItemIndex < OBJList.Items.Count - 1 then
  begin
    i :=  OBJList.ItemIndex;
    OBJList.Items.Move(i, i + 1);
    OBJList.ItemIndex :=  i + 1;
  end;
end;

end.
