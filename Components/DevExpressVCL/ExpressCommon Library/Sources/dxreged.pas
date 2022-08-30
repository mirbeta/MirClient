{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Registry path property editor                    }
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

unit dxRegEd;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Registry, ImgList;

type
  TfrmdxLibREgPathEditor = class(TForm)
    Panel: TPanel;
    TreeView: TTreeView;
    bOk: TButton;
    bCancel: TButton;
    bNew: TButton;
    ImageList: TImageList;
    lbRegistry: TLabel;
    bDelete: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure bNewClick(Sender: TObject);
    procedure TreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure TreeViewCollapsed(Sender: TObject; Node: TTreeNode);
    procedure bDeleteClick(Sender: TObject);
  private
    reg : TRegistry;
    function GetFullText(Node : TTreeNode) : String;
    procedure WMGetMinMaxInfo(var Message : TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
  public
  end;

function dxGetRegistryPath(Var APath : String) : Boolean;

implementation

uses
  Types, dxrgkeya;

var
  ATempPath : String;

function dxGetRegistryPath(Var APath : String) : Boolean;
var
  AForm : TfrmdxLibREgPathEditor;
begin
  if (APath <> '') and (APath[1] = '\') then
    ATempPath := Copy(APath, 2, 1000)
  else ATempPath := APath;
  AForm := TfrmdxLibREgPathEditor.Create(Nil);
  AForm.ShowModal;
  Result := AForm.ModalResult = mrOk;
  if (Result) then
    APath := AForm.lbRegistry.Caption;
  AForm.Free;
end;

{$R *.DFM}
procedure TfrmdxLibREgPathEditor.bDeleteClick(Sender: TObject);

  function DeleteNode(ANode : TTreeNode) : Boolean;
  var
    AReg : TRegistry;
  begin
    Result := True;
    while ANode.Count > 0 do
    begin
      Result := DeleteNode(ANode.Item[0]);
      if not Result then
        Break;
    end;
    if Result then
    begin
      AReg := TRegistry.Create;
      Result := AReg.DeleteKey(GetFullText(ANode));
      if Result then
        ANode.Free;
      AReg.Free;
    end;
  end;

begin
  if(TreeView.Selected = Nil) or (TreeView.Selected.Parent = Nil) then exit;
  if (MessageBox(self.Handle, 'Are you sure you want delete this key?',
    'Confirm Key Delete', MB_ICONWARNING or MB_YESNO) = IDYES)
  and (DeleteNode(TreeView.Selected)) then
    TreeViewChange(Sender, TreeView.Selected)
  else FormCreate(nil);
end;

procedure TfrmdxLibREgPathEditor.TreeViewCollapsed(Sender: TObject;
  Node: TTreeNode);
begin
  TreeView.Refresh;
end;

procedure TfrmdxLibREgPathEditor.TreeViewExpanded(Sender: TObject;
  Node: TTreeNode);
begin
  TreeView.Refresh;
end;

procedure TfrmdxLibREgPathEditor.TreeViewGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  TreeViewGetImageIndex(Sender, Node);
end;

procedure TfrmdxLibREgPathEditor.TreeViewGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.HasChildren and Node.Expanded then
    Node.ImageIndex := 1
  else Node.ImageIndex := 0;
  Node.SelectedIndex := Node.ImageIndex;
end;

function TfrmdxLibREgPathEditor.GetFullText(Node : TTreeNode) : String;
begin
  if(Node = nil) then
  begin
    Result := '';
    exit;
  end;
  Result := Node.Text;
  while Node.Parent <> nil do
  begin
    Node := Node.Parent;
    Result := Node.Text + '\' + Result;
  end;
  Result := '\' + Result;
end;

procedure TfrmdxLibREgPathEditor.FormCreate(Sender: TObject);
var
  Sts : TStrings;
  i : Integer;

  procedure FillRegistry(Node : TTreeNode; APath, ASt : String);
  var
    ANode : TTreeNode;
    i : Integer;
    ASts : TStrings;
    AReg : TRegistry;
  begin
    ASts := TStringList.Create;
    try
      AReg := TRegistry.Create;
      try
        APath := APath + '\' + ASt;
        AReg.OpenKey(APath, False);
        AReg.GetKeyNames(ASts);
        ANode := TreeView.Items.AddChild(Node, ASt);
        if CompareText(APath, ATempPath) = 0 then
          ANode.Selected := True;
      finally
        AReg.Free;
      end;
      for i := 0 to ASts.Count - 1 do
        FillRegistry(ANode, APath, ASts[i]);
    finally
      ASts.Free;
    end;
  end;

var
  ANode : TTreeNode;
begin
  PopupMode := pmAuto;
  reg := TRegistry.Create;
  reg.OpenKey('Software', False);
  Sts := TStringList.Create;
  reg.GetKeyNames(Sts);
  TreeView.Items.BeginUpdate;
  try
    TreeView.Items.Clear;
    ANode := TreeView.Items.Add(nil, 'Software');
    for i := 0 to Sts.Count - 1 do
      FillRegistry(ANode, 'Software', Sts[i]);
    if TreeView.Selected = nil then
    begin
      TreeView.Items[0].Expand(False);
      TreeView.Selected := TreeView.Items[0];
    end;
  finally
    TreeView.Items.EndUpdate;
    TreeView.Selected.MakeVisible;
    Sts.Free;
  end;
end;

procedure TfrmdxLibREgPathEditor.FormDestroy(Sender: TObject);
begin
  reg.Free;
end;

procedure TfrmdxLibREgPathEditor.TreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  if (Node <> nil) then
    lbRegistry.Caption := GetFullText(Node)
  else lbRegistry.Caption := '';
  bNew.Enabled := (TreeView.Selected <> nil);
  bDelete.Enabled := (TreeView.Selected <> nil) and (TreeView.Selected.Parent <> nil);
  bOk.Enabled := bDelete.Enabled;
end;

procedure TfrmdxLibREgPathEditor.bNewClick(Sender: TObject);
var
  AReg : TRegistry;
  St : String;
  Node : TTreeNode;
begin
  if(TreeView.Selected = Nil) then exit;
  AReg := TRegistry.Create;
  AReg.OpenKey(GetFullText(TreeView.Selected), False);
  St := dxGetNewRegistryKey;
  if(St <> '') then
  begin
    if(AReg.CreateKey(St)) then
    begin
      Node := TreeView.Items.AddChild(TreeView.Selected, St);
      Node.Selected := True;
      Node.MakeVisible;
    end;
  end;
  AReg.Free;
end;

procedure TfrmdxLibREgPathEditor.WMGetMinMaxInfo(var Message : TWMGetMinMaxInfo);
begin
  Message.MinMaxInfo^.ptMinTrackSize := Point(395, 365);
  inherited;
end;

end.
