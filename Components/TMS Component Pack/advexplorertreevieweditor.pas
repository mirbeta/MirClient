unit AdvExplorerTreeviewEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Spin, StdCtrls, ComCtrls, AdvExplorerTreeview, ExtCtrls;

type
  TExpTreeviewEditor = class(TForm)
    GroupBox1: TGroupBox;
    Treeview1: TTreeView;
    Btn_NewItem: TButton;
    Btn_NewSubItem: TButton;
    Btn_Delete: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Ed_Text: TEdit;
    Sp_ImageIndex: TSpinEdit;
    Btn_Ok: TButton;
    Btn_Cancel: TButton;
    Image1: TImage;
    procedure Btn_OkClick(Sender: TObject);
    procedure Btn_NewItemClick(Sender: TObject);
    procedure Ed_TextChange(Sender: TObject);
    procedure Sp_ImageIndexChange(Sender: TObject);
    procedure Btn_NewSubItemClick(Sender: TObject);
    procedure Btn_DeleteClick(Sender: TObject);
    procedure Treeview1Change(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
  private
    FExplorerTreeview: TAdvExplorerTreeview;
    FInternalChange: Boolean;
    procedure SetExplorerTreeview(const Value: TAdvExplorerTreeview);
    procedure UpdateImage;
    { Private declarations }
  protected
    //Add AdvExplorerNode to TreeviewNode
    procedure AddNodeChildren(var ExpNode: TAdvTreeNode; var Node: TTreeNode);
    procedure CopyItemsFromTvToExpTv;
    procedure AddExpNodeChildren(var Node: TTreeNode; var ExpNode: TAdvTreeNode);
    procedure UpdateButtons;
  public
    { Public declarations }
    property ExplorerTreeview: TAdvExplorerTreeview read FExplorerTreeview write SetExplorerTreeview;
  end;

var
  ExpTreeviewEditor: TExpTreeviewEditor;

implementation

{$R *.dfm}

{ TExpTreeviewEditor }

//------------------------------------------------------------------------------
// Add AdvExplorerNode to TreeviewNode

procedure TExpTreeviewEditor.AddNodeChildren(var ExpNode: TAdvTreeNode; var Node: TTreeNode);
var
  ExN: TAdvTreeNode;
  N: TTreeNode;
begin
  if not Assigned(ExpNode) or not Assigned(Node) then
    Exit;

  ExN := ExpNode.getFirstChild;
  if Assigned(ExN) then
  begin
    while (ExN <> nil) do
    begin
      N := Treeview1.Items.AddChildObject(Node, ExN.Text, ExN);
      N.ImageIndex := ExN.ImageIndex;
      AddNodeChildren(ExN, N);
      ExN := ExN.getNextSibling;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewEditor.SetExplorerTreeview(
  const Value: TAdvExplorerTreeview);
var
   ExpNode: TAdvTreeNode;
   Node: TTreeNode;
begin
  FExplorerTreeview := Value;
  if Assigned(ExplorerTreeview) then
  begin
    Treeview1.Items.Clear;
    ExpNode := ExplorerTreeview.Items.GetFirstNode;
    if Assigned(ExpNode) then
    begin
      while (ExpNode <> nil) do
      begin
        Node := Treeview1.Items.AddChildObject(nil, ExpNode.Text, ExpNode);
        Node.ImageIndex := ExpNode.ImageIndex;
        AddNodeChildren(ExpNode, Node);
        ExpNode := ExpNode.getNextSibling;
      end;
    end;
  end;
  Treeview1.Selected := Treeview1.Items.GetFirstNode;
  UpdateButtons;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewEditor.Btn_OkClick(Sender: TObject);
begin
// OK button
  CopyItemsFromTvToExpTv;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewEditor.CopyItemsFromTvToExpTv;
var
   ExpNode: TAdvTreeNode;
   Node: TTreeNode;
begin
  if Assigned(ExplorerTreeview) then
  begin
    ExplorerTreeview.Items.Clear;
    Node := Treeview1.Items.GetFirstNode;;
    while (Node <> nil) do
    begin
      ExpNode := ExplorerTreeview.Items.Add(Node.Text);
      ExpNode.ImageIndex := Node.ImageIndex;
      AddExpNodeChildren(Node, ExpNode);
      Node := Node.getNextSibling;
    end;
  end;
end;

//------------------------------------------------------------------------------

// Add TreeviewNode to ExplorerTreeviewNode
procedure TExpTreeviewEditor.AddExpNodeChildren(var Node: TTreeNode; var ExpNode: TAdvTreeNode);
var
  ExN: TAdvTreeNode;
  N: TTreeNode;
begin
  if not Assigned(ExpNode) or not Assigned(Node) then
    Exit;

  N := Node.getFirstChild;
  if Assigned(N) then
  begin
    while (N <> nil) do
    begin
      ExN := ExpNode.AddChild(N.Text);
      ExN.ImageIndex := N.ImageIndex;
      AddExpNodeChildren(N, ExN);
      N := N.getNextSibling;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewEditor.Btn_NewItemClick(Sender: TObject);
begin
  if Assigned(Treeview1.Selected) then
    Treeview1.Selected := Treeview1.Items.AddChild(Treeview1.Selected.Parent, '')
  else
    Treeview1.Selected := Treeview1.Items.AddChildFirst(nil, '');
  Treeview1.Selected.ImageIndex := -1;
  Sp_ImageIndex.Value := Treeview1.Selected.ImageIndex;
  UpdateImage;
  Ed_Text.SetFocus;
  UpdateButtons;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewEditor.Ed_TextChange(Sender: TObject);
begin
  if Assigned(Treeview1.Selected) then
    Treeview1.Selected.Text := Ed_Text.Text;
end;

procedure TExpTreeviewEditor.FormCreate(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

procedure TExpTreeviewEditor.UpdateImage;
begin
  if Assigned(Treeview1.Selected) and not FInternalChange then
  begin
    Image1.Picture.Bitmap.Canvas.Brush.Color := Self.Color;
    Image1.Picture.Bitmap.Canvas.Pen.Color := Self.Color;
    Image1.Picture.Bitmap.Canvas.FillRect(Image1.ClientRect);
    if Assigned(ExplorerTreeview) and Assigned(ExplorerTreeview.Images) and (Treeview1.Selected.ImageIndex >= 0) then
    begin
      ExplorerTreeview.Images.GetBitmap(Treeview1.Selected.ImageIndex, Image1.Picture.Bitmap);
      Image1.Picture.Bitmap.Transparent := True;
      Image1.Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewEditor.Sp_ImageIndexChange(Sender: TObject);
begin
  if Assigned(Treeview1.Selected) and not FInternalChange then
  begin
    Treeview1.Selected.ImageIndex := Sp_ImageIndex.Value;
    UpdateImage;
  end;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewEditor.Btn_NewSubItemClick(Sender: TObject);
begin
  if Assigned(Treeview1.Selected) then
    Treeview1.Selected := Treeview1.Items.AddChild(Treeview1.Selected, '');
  Ed_Text.SetFocus;  
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewEditor.Btn_DeleteClick(Sender: TObject);
begin
  if Assigned(Treeview1.Selected) then
    Treeview1.Items.Delete(Treeview1.Selected);
  UpdateButtons;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewEditor.UpdateButtons;
begin
  Btn_NewSubItem.Enabled := Assigned(Treeview1) and Assigned(Treeview1.Selected);
  Btn_Delete.Enabled := Assigned(Treeview1) and Assigned(Treeview1.Selected);
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewEditor.Treeview1Change(Sender: TObject;
  Node: ComCtrls.TTreeNode);
begin
  if Assigned(Node) then
  begin
    FInternalChange := True;
    Ed_Text.Text := Node.Text;
    Sp_ImageIndex.Value := Node.ImageIndex;
    FInternalChange := False;
    UpdateImage;
  end;  
end;

//------------------------------------------------------------------------------


end.
