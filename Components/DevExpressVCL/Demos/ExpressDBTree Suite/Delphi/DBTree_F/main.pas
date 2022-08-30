unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB, ComCtrls, Mask, DBCtrls, Grids,
  DBGrids, ExtCtrls, Buttons, ShellAPI, dxtree, dxdbtree, dxmdaset;

type
  TForm1 = class(TForm)
    DBTreeView1: TdxDBTreeView;
    DataSource1: TDataSource;
    Panel1: TPanel;
    Button1: TButton;
    BitBtnAdd: TBitBtn;
    BitBtnAddChild: TBitBtn;
    BitBtnEdit: TBitBtn;
    BitBtnDel: TBitBtn;
    BitBtn2: TBitBtn;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    DBNavigator1: TDBNavigator;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    dxMemData1: TdxMemData;
    dxMemData1id: TIntegerField;
    dxMemData1parent: TIntegerField;
    dxMemData1name: TStringField;
    dxMemData1buffer: TStringField;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtnAddClick(Sender: TObject);
    procedure BitBtnAddChildClick(Sender: TObject);
    procedure BitBtnEditClick(Sender: TObject);
    procedure BitBtnDelClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure DBTreeView1CreateNewKeyValue(Sender: TObject;
      var NewKeyValue: Variant);
    procedure CheckBox3Click(Sender: TObject);
  private
    MaxValue : Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses dbview;
{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  Form2: TForm2;
begin
  Form2 := TForm2.Create(Nil);
  Form2.DBGrid1.DataSource := DataSource1;
  Form2.ShowModal;
  Form2.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
Var
  i, j, k, l, id, parent : Integer;
begin
  dxMemData1.Open;

  for i := 0 to 9 do
    for j := 0 to 9 do
      for k := 0 to 9 do
        for l := 0 to 9 do begin
          with dxMemData1 do begin
            Append;
            id := l + k*10 + j* 100 + i * 1000;
            parent := k + j* 10 + i * 100 - 1;
            FindField('id').AsInteger := id;
            FindField('parent').AsInteger := parent;
            FindField('Name').AsString := 'TreeNode item No ' + IntToStr(id) + ' parent No ' + IntToStr(Parent);
            FindField('Buffer').AsString := 'No ' + IntToStr(id);
            Post;
          end;
        end;

  dxMemData1.First;
  MaxValue := 10000;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  dxMemData1.Close;
end;

procedure TForm1.BitBtnAddClick(Sender: TObject);
begin
  if (DBTreeView1.Selected <> Nil) then
    DBTreeView1.Selected := DBTreeView1.Items.Add(DBTreeView1.Selected, 'New Item');
end;

procedure TForm1.BitBtnAddChildClick(Sender: TObject);
begin
  if (DBTreeView1.Selected <> Nil) then
    DBTreeView1.Selected := DBTreeView1.Items.AddChild(DBTreeView1.Selected,'Child of ' + DBTreeView1.Selected.Text);
end;

procedure TForm1.BitBtnEditClick(Sender: TObject);
begin
  if (DBTreeView1.Selected <> Nil) then
    DBTreeView1.Selected.EditText;
end;

procedure TForm1.BitBtnDelClick(Sender: TObject);
begin
  if (DBTreeView1.Selected <> Nil) then
    DBTreeView1.Selected.Delete;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
var
  tr : TTreeNode;
begin
  if (DBTreeView1.Selected <> Nil) then begin
    tr := DBTreeView1.Selected.Parent;
    if(tr <> nil) then begin
      if(tr.parent <> nil) then
        DBTreeView1.Selected.MoveTo(tr.Parent, naAddChild)
      else DBTreeView1.Selected.MoveTo(Nil, naAdd);
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  DBTreeView1.DataSource := DataSource1;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  DBTreeView1.DataSource := Nil;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if(CheckBox1.Checked) then
    DBTreeView1.Options := DBTreeView1.Options + [trCanDBNavigate]
  else DBTreeView1.Options := DBTreeView1.Options - [trCanDBNavigate];
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if(CheckBox2.Checked) then
    DBTreeView1.Options := DBTreeView1.Options + [trCheckHasChildren]
  else DBTreeView1.Options := DBTreeView1.Options - [trCheckHasChildren];
end;

procedure TForm1.DBTreeView1CreateNewKeyValue(Sender: TObject;
  var NewKeyValue: Variant);
begin
  NewKeyValue := MaxValue;
  Inc(MaxValue);
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  if(CheckBox3.Checked) then
    DBTreeView1.Options := DBTreeView1.Options + [trSmartRecordCopy]
  else DBTreeView1.Options := DBTreeView1.Options - [trSmartRecordCopy];
end;

end.
