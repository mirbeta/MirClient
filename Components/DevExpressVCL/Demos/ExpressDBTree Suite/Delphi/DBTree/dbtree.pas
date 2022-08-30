unit dbtree;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBCtrls, StdCtrls, Buttons, Grids, DBGrids, ComCtrls,
  ExtCtrls, DB, Menus, Mask, dxtree, dxdbtree, dxdbtrel,
  dxmdaset, ImgList;

type
  TForm1 = class(TForm)
    DS1: TDataSource;
    Panel1: TPanel;
    Panel2: TPanel;
    DBTreeView1: TdxDBTreeView;
    Panel3: TPanel;
    DBMemo1: TDBMemo;
    Label1: TLabel;
    Label2: TLabel;
    DBLookUpTreeView1: TdxDBLookUpTreeView;
    DS2: TDataSource;
    BitBtn1: TBitBtn;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    DBEdit1: TDBEdit;
    ImageList1: TImageList;
    CheckBox2: TCheckBox;
    BitBtnAdd: TBitBtn;
    BitBtnAddChild: TBitBtn;
    BitBtnEdit: TBitBtn;
    BitBtnEdit1: TBitBtn;
    BitBtnDel: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Button2: TButton;
    CheckBox5: TCheckBox;
    mdProject1: TdxMemData;
    mdProject2: TdxMemData;
    mdProject1Pr_id: TIntegerField;
    mdProject1Pr_parent: TIntegerField;
    mdProject1Pr_name: TStringField;
    mdProject1Pr_bdate: TDateField;
    mdProject1Pr_edate: TDateField;
    mdProject1Pr_info: TMemoField;
    mdProject2Pr_id: TIntegerField;
    mdProject2Pr_parent: TIntegerField;
    mdProject2Pr_name: TStringField;
    mdProject2Pr_bdate: TDateField;
    mdProject2Pr_edate: TDateField;
    mdProject2Pr_info: TMemoField;
    procedure CheckBox1Click(Sender: TObject);
    procedure BitBtnDelClick(Sender: TObject);
    procedure BitBtnEditClick(Sender: TObject);
    procedure BitBtnAddClick(Sender: TObject);
    procedure BitBtnAddChildClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure DBTreeView1SetDisplayItemText(Sender: TObject;
      var DisplayText: string);
    procedure BitBtnEdit1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure DBTreeView1DragDropTreeNode(Destination, Source: TTreeNode;
      var Accept: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure DBTreeView1CustomDraw(Sender: TObject; TreeNode: TTreeNode;
      AFont: TFont; var AColor, ABKColor: TColor);
    procedure CheckBox5Click(Sender: TObject);
    procedure DBTreeView1AddNewItem(Sender: TObject;
      var DBTreeNode: TdxDBTreeNode);
    procedure DBLookUpTreeView1AddNewItem(Sender: TObject;
      var DBTreeNode: TdxDBTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure mdProject1AfterInsert(DataSet: TDataSet);
    procedure mdProject1Pr_parentChange(Sender: TField);
    procedure mdProject1AfterPost(DataSet: TDataSet);
  private
    function IsLoop : Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  Variants,
  tree, dbgridv;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if(CheckBox1.Checked) then
    DBTreeView1.DragMode := dmAutomatic
  else DBTreeView1.DragMode := dmManual;
end;

procedure TForm1.BitBtnDelClick(Sender: TObject);
begin
  if (DBTreeView1.Selected <> Nil) then
    DBTreeView1.Selected.Delete;
end;                                    

procedure TForm1.BitBtnEditClick(Sender: TObject);
begin
  if (DBTreeView1.Selected <> Nil) then
    DBTreeView1.Selected.EditText;
end;

procedure TForm1.BitBtnAddClick(Sender: TObject);
begin
  DBTreeView1.Items.Add(DBTreeView1.Selected, 'New Item');
end;

procedure TForm1.BitBtnAddChildClick(Sender: TObject);
begin
  if (DBTreeView1.Selected <> Nil) then
    DBTreeView1.Items.AddChild(DBTreeView1.Selected,'Child of ' + DBTreeView1.Selected.Text);
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

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  Form2.Show;
end;

function TForm1.IsLoop : Boolean;
Var
  dbItemP, dbItem : TdxDBTreeNode;
begin
  Result := False;
  {Get Selected DBTreeNode}
  dbItem := DBTreeView1.DBTreeNodes.GetDBTreeNode(mdProject1.FindField('pr_id').Value);
  {Get Parent DBTreeNode}
  dbItemP := DBTreeView1.DBTreeNodes.GetDBTreeNode(mdProject1.FindField('pr_parent').Value);
  {Is it loop ?}
  if(dbItem <> Nil) and  (dbItemP <> Nil) and (dbItemP <> dbItem)
  and dbItemP.HasAsParent(dbItem) then
    Result := True;
end;

procedure TForm1.DBTreeView1SetDisplayItemText(Sender: TObject;
  var DisplayText: string);
begin
  if(mdProject1.FindField('pr_id').AsInteger = 1) then
    DisplayText := 'It is the first item. ' + DisplayText;
end;
procedure TForm1.BitBtnEdit1Click(Sender: TObject);
begin
  if (DBTreeView1.Selected <> Nil) then
    DBTreeView1.Selected.Text := DBTreeView1.Selected.Text + ' - ' + DBTreeView1.Selected.Text;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
    DBTreeView1.DisplayField := 'pr_name;pr_id'
   else DBTreeView1.DisplayField := '';
end;

procedure TForm1.DBTreeView1DragDropTreeNode(Destination,
  Source: TTreeNode; var Accept: Boolean);
begin
  if Destination = nil then
  begin
    Accept := False;
    exit;
  end;
  if Source.TreeView = Form2.dxTreeView1 then
  begin
    Accept := True;
    exit;
  end;
  if(CheckBox3.Checked) then
    Accept := Source.Level > Destination.Level;
  if Accept and (CheckBox4.Checked) then
    Accept := Not (Source.Parent = Nil);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Form3.ShowModal;
end;

procedure TForm1.DBTreeView1CustomDraw(Sender: TObject;
  TreeNode: TTreeNode; AFont: TFont; var AColor, ABKColor: TColor);
begin
  if not CheckBox5.Checked then exit;
  if(TreeNode.Index mod 2 = 0) then begin
    AFont.Style := [fsBold];
    AColor := clBlue;
  end else begin
    ABKColor := clYellow;
    AColor := clRed;
  end;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  DBTreeView1.repaint;
end;

procedure TForm1.DBTreeView1AddNewItem(Sender: TObject;
  var DBTreeNode: TdxDBTreeNode);
begin
 if VarIsNull(DBTreeNode.ParentFieldValue) then exit;
 if (DBTreeNode.KeyFieldValue >= 150)
 and (DBTreeNode.KeyFieldValue < 200) then begin
    DBTreeNode.ImageIndex := 0;
    DBTreeNode.SelectedIndex := 0;
    exit;
 end;
  if (DBTreeNode.KeyFieldValue > 200) then begin
    DBTreeNode.ImageIndex := 1;
    DBTreeNode.SelectedIndex := 1;
    exit;
 end;
 DBTreeNode.ImageIndex := 2;
 DBTreeNode.SelectedIndex := 2;
end;

procedure TForm1.DBLookUpTreeView1AddNewItem(Sender: TObject;
  var DBTreeNode: TdxDBTreeNode);
begin
   DBTreeView1AddNewItem(Sender, DBTreeNode);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  mdProject1.LoadFromBinaryFile('..\..\Data\Project.dat');
  mdProject2.LoadFromDataSet(mdProject1);
  mdProject1.Open;
  mdProject2.Open;
end;

procedure TForm1.mdProject1AfterInsert(DataSet: TDataSet);
begin
  if varIsNull(mdProject1.FindField('pr_id').Value) then begin
    if varIsNull(DBTreeView1.DBTreeNodes.MaxKeyFieldValue) then
      mdProject1.FindField('pr_id').AsInteger := 0
    else mdProject1.FindField('pr_id').Value := DBTreeView1.DBTreeNodes.MaxKeyFieldValue + 1;
  end;
end;

procedure TForm1.mdProject1Pr_parentChange(Sender: TField);
begin
  if(IsLoop) then
    ShowMessage('It is Loop');
end;

procedure TForm1.mdProject1AfterPost(DataSet: TDataSet);
var
  ABooknark: TBookmark;
begin
  ABooknark := mdProject1.Bookmark;
  mdProject2.LoadFromDataSet(mdProject1);
  mdProject1.Bookmark := ABooknark;
end;

end.
