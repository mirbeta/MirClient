unit RDMenuEditor;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     Menus, ActnList, ToolWin, ComCtrls,
     ExtCtrls, ImgList, DesignEditors, DesignIntf, DesignWindows;

type
  TUpdateState = (usTree, usList, usSetNode, usDeleting);
  TUpdateStates = set of TUpdateState;

  TMenuDesignWindow = class(TDesignWindow)
    TreeView: TTreeView;
    ToolBar1: TToolBar;
    AL: TActionList;
    IL: TImageList;
    actNewItem: TAction;
    actSeparator: TAction;
    actDelete: TAction;
    actCopy: TAction;
    actCut: TAction;
    actPaste: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    PopupMenu1: TPopupMenu;
    actNewItem1: TMenuItem;
    actSeparator1: TMenuItem;
    actSubMenu1: TMenuItem;
    Delete1: TMenuItem;
    N2: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    Splitter1: TSplitter;
    ListView: TListView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure TreeViewEnter(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actNewItemExecute(Sender: TObject);
    procedure actSeparatorExecute(Sender: TObject);
    procedure ListViewKeyPress(Sender: TObject; var Key: Char);
    procedure TreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    FRootItem, FSelParentItem: TMenuItem;
    FParentComponent: TComponent;
    FState: TUpdateStates;
    List: IDesignerSelections;
    procedure RebuildList;
    procedure RebuildTree;
    procedure CreateItem(ACaption: string);
    procedure SetSelParentItem (ASelParentItem: TMenuItem);
    procedure GetSelList;
    function GetItemTreeCaption (AItem: TMenuItem): String;
    procedure UpadteCaptions;
  protected
    procedure Copy;
    procedure Cut;
    procedure Delete;
    procedure Paste;
    procedure SetSelections;
    procedure SetEnabling;
    procedure Notification (AComponent: TComponent; Operation: TOperation); override;
    function UniqueName (Component: TComponent): String; override;
  public
    function EditAction(Action: TEditAction): Boolean; override;
    procedure ItemsModified(const ADesigner: IDesigner); override;
    function GetEditState: TEditState; override;
  end;

  TMenuEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb (Index: Integer); override;
    function GetVerb (Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

  TMenuItemsPropertyEditor = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
  end;

implementation

{$R *.DFM}

procedure ShowMenuEditForm (const AParentComponent: TComponent;
                            const ARootItem: TMenuItem;
                            const ADesigner: IDesigner);
var
  I: Integer;
  Form: TCustomForm;
  EditForm: TMenuDesignWindow;
begin
  for I := 0 to Screen.FormCount-1 do begin
    Form := Screen.Forms[I];
    if Form is TMenuDesignWindow then
      if TMenuDesignWindow(Form).FRootItem = ARootItem then
      begin
        Form.Show;
        Form.BringToFront;
        if Form.WindowState = wsMinimized then
          Form.WindowState := wsNormal;
        Exit;
      end;
  end;
  EditForm := TMenuDesignWindow.Create(Application);
  try
    EditForm.Designer := ADesigner;
    EditForm.FParentComponent := AParentComponent;
    AParentComponent.FreeNotification(EditForm);
    EditForm.FRootItem := ARootItem;
    ARootItem.FreeNotification (EditForm);
    EditForm.FSelParentItem := ARootItem;
    EditForm.Caption := 'Editing ' + AParentComponent.Name;
    EditForm.RebuildTree;
    EditForm.RebuildList;
    EditForm.Show;
  except
    EditForm.Free;
    raise;
  end;
end;


{: TMenuEditor }

procedure TMenuEditor.Edit;
begin
  if Assigned(Component) then
    ShowMenuEditForm(Component, TMenu(Component).Items, Designer);
end;

procedure TMenuEditor.ExecuteVerb (Index: Integer);
begin
  if Index = 0 then Edit;
end;

function TMenuEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TMenuEditor.GetVerb (Index: Integer): String;
begin
  if Index = 0 then Result := 'Edit...'
   else Result := '';
end;

{: TMenuItemsPropertyEditor }

function TMenuItemsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TMenuItemsPropertyEditor.GetValue: String;
begin
  Result := Format('(%s)', ['Menu']);
end;

{: TMenuDesignWindow }

function TMenuDesignWindow.GetEditState: TEditState;
begin
  Result := [];
  if ActiveControl = ListView then
  begin
    if Assigned(ListView.Selected) then
      Result := [esCanDelete, esCanCut, esCanCopy];
    if ClipboardComponents then
      Include (Result, esCanPaste);
  end;
end;

procedure TMenuDesignWindow.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and
     ((AComponent = FParentComponent) or (AComponent = FRootItem)) then
    Free;
end;

function TMenuDesignWindow.GetItemTreeCaption (AItem: TMenuItem): String;
begin
  if AItem <> FRootItem then
  begin
    Result := AItem.Caption;
    if Result = '' then  Result := '[' + AItem.Name + ']'  else
     if Result = '-' then  Result := '<------------->';
  end
  else Result := '(Root)';
end;

procedure TMenuDesignWindow.RebuildTree;

  procedure Recurse (const AParentItem: TMenuItem; const ATreeNode: TTreeNode;
    var FoundSelParentItem: TTreeNode);
  var
    I: Integer;
    NewNode: TTreeNode;
    ChildItem: TMenuItem;
  begin
    NewNode := TreeView.Items.AddChild(ATreeNode, GetItemTreeCaption(AParentItem));
    NewNode.Data := AParentItem;
    NewNode.ImageIndex := AParentItem.ImageIndex;
    NewNode.SelectedIndex := AParentItem.ImageIndex;
    if AParentItem = FSelParentItem then
      FoundSelParentItem := NewNode;
    for I := 0 to AParentItem.Count-1 do
    begin
      ChildItem := AParentItem[I];
      Recurse (ChildItem, NewNode, FoundSelParentItem);
    end;
  end;

var
  FoundSelParentItem: TTreeNode;
begin
  Include(FState, usTree);
  try
    TreeView.Items.BeginUpdate;
    try
      TreeView.Items.Clear;
      FoundSelParentItem := nil;
      Recurse(FRootItem, nil, FoundSelParentItem);
      if FoundSelParentItem = nil then
        SetSelParentItem (FRootItem)
      else
        TreeView.Selected := FoundSelParentItem;
      TreeView.Items[0].Expand (True);
    finally
      TreeView.Items.EndUpdate;
    end;
  finally
    Exclude(FState, usTree);
  end;
end;

procedure TMenuDesignWindow.RebuildList;
var ChildItem: TMenuItem;
    I: Integer;

  function AddListViewItem (const Item: TMenuItem): TListItem;
  begin
    Result := ListView.Items.Add;
    Result.Data := Item;
    Result.Caption := GetItemTreeCaption(Item);
    Result.ImageIndex := Item.ImageIndex;
    Result.StateIndex := -1;
  end;

begin
  Include(FState, usList);
  try
    ListView.Items.BeginUpdate;
    try
      ListView.Items.Clear;
      if Assigned(FSelParentItem) then
      begin
        for I := 0 to FSelParentItem.Count - 1 do begin
          ChildItem := FSelParentItem[I];
          if not (csDestroying in ChildItem.ComponentState) then
            AddListViewItem(ChildItem);
        end;
      end;
    finally
      ListView.Items.EndUpdate;
    end;
  finally
    Exclude(FState, usList);
  end;
end;

procedure TMenuDesignWindow.SetSelParentItem(ASelParentItem: TMenuItem);
var TreeNode: TTreeNode;
begin
  if usSetNode in FState then  Exit;
  Include(FState, usSetNode);
  try
    if FSelParentItem <> ASelParentItem then
     begin
      FSelParentItem := ASelParentItem;
      if not Assigned(TreeView.Selected) or (TreeView.Selected.Data <> FSelParentItem) then
       begin
        if FSelParentItem = nil then TreeView.Selected := nil
        else
         begin
          TreeNode := TreeView.Items.GetFirstNode;
          while Assigned(TreeNode) do
           begin
            if TreeNode.Data = FSelParentItem then
             begin
              TreeView.Selected := TreeNode;
              Break;
             end;
            TreeNode := TreeNode.GetNext;
           end;
        end;
      end;
      RebuildList;
    end;
    SetSelections;
  finally
    Exclude(FState, usSetNode);
  end;
end;

procedure TMenuDesignWindow.SetEnabling;
begin
  GetSelList;
  actNewItem.Enabled := Assigned(FSelParentItem);
  actSeparator.Enabled := Assigned(FSelParentItem);
  actCut.Enabled := List.Count > 0;
  actCopy.Enabled := List.Count > 0;
  actPaste.Enabled := ClipboardComponents;// (ActiveControl = ListView);
  actDelete.Enabled := List.Count > 0;
end;

procedure TMenuDesignWindow.SetSelections;
begin
  GetSelList;
  if List.Count = 0 then
   begin
    if not Assigned(FSelParentItem) or (FSelParentItem = FRootItem) then
       List.Add (FParentComponent)
    else
       List.Add (FSelParentItem);
   end;
  Designer.SetSelections(List);
  SetEnabling;
end;

procedure TMenuDesignWindow.GetSelList;
var i: integer;
begin
  List := TDesignerSelections.Create;
  if ActiveControl = ListView then
  begin
   for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Selected and Assigned(ListView.Items[i].Data) then
      List.Add(ListView.Items[i].Data);
  end else
  if ActiveControl = TreeView then
  begin
   for i := 0 to TreeView.Items.Count - 1 do
    if TreeView.Items[i].Selected and Assigned(TreeView.Items[i].Data) then
      List.Add(TreeView.Items[i].Data);
  end;
end;

function TMenuDesignWindow.UniqueName(Component: TComponent): String;
begin
  Result := Designer.UniqueName(Component.ClassName);
end;

procedure TMenuDesignWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TMenuDesignWindow.Copy;
begin
  GetSelList;
  if List.Count > 0 then
    CopyComponents(FParentComponent.Owner, List);
end;

procedure TMenuDesignWindow.Cut;
begin
  Copy;
  Delete;
end;

function FindItem(List: IDesignerSelections; Item: TPersistent): Boolean;
var i: integer;
begin
 Result := True;
 for i := 0 to List.Count - 1 do
  if List[i] = Item then  Exit;
 Result := False;
end;

procedure TMenuDesignWindow.Delete;
var i, J: integer;
begin
  GetSelList;
  if List.Count = 0 then Exit;
  // Remove Items entries
  FState := [usTree, usSetNode];
  J := -1;
  for i := ListView.Items.Count - 1 downto 0 do
   if FindItem(List, ListView.Items[i].Data) then
    begin
     ListView.Items[i].Delete;
     if (ActiveControl = ListView) and (J = -1) then
      J := i;
    end;
  for i := TreeView.Items.Count - 1 downto 0 do
   if FindItem(List, TreeView.Items[i].Data) then
    begin
     if TreeView.Items[i].Data = FSelParentItem then
        FSelParentItem := nil;
     TreeView.Items.Delete(TreeView.Items[i]);
     if (ActiveControl = TreeView) and (J = -1) then
      J := i;
    end;
  for i := 0 to List.Count - 1 do
   List[i].Free;
  if (ActiveControl = ListView) then
   begin
    if (J >= 0) and (J < ListView.Items.Count) then
     ListView.Selected := ListView.Items[J]
    else
     ListView.Selected := nil;
    SetSelections;
   end else
   begin
    if (J < 0) or (J >= TreeView.Items.Count) then  J := TreeView.Items.Count - 1;
    SetSelParentItem(TreeView.Items[J].Data)
   end;
  FState := [];
end;

procedure TMenuDesignWindow.Paste;
var
  List: IDesignerSelections;
begin
  if FSelParentItem = nil then Exit;
  List := DesignIntf.CreateSelectionList;
  PasteComponents (FParentComponent.Owner, FSelParentItem, List);
  RebuildTree;
  RebuildList;
  if List.Count <> 0 then Designer.Modified;
end;

procedure TMenuDesignWindow.TreeViewChange(Sender: TObject; Node: TTreeNode);
var
  NewSelectedParentItem: TMenuItem;
begin
  if [usTree, usSetNode]*FState <> [] then Exit;
  if Node = nil then
    NewSelectedParentItem := nil
  else
    NewSelectedParentItem := Node.Data;
  SetSelParentItem (NewSelectedParentItem);
end;

procedure TMenuDesignWindow.actCopyExecute(Sender: TObject);
begin
 Copy;
end;

procedure TMenuDesignWindow.actCutExecute(Sender: TObject);
begin
 Cut;
end;

procedure TMenuDesignWindow.actPasteExecute(Sender: TObject);
begin
 Paste;
end;

procedure TMenuDesignWindow.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 if [usList, usSetNode] * FState <> [] then Exit;
 SetSelections;
end;

procedure TMenuDesignWindow.TreeViewEnter(Sender: TObject);
begin
  ListView.Selected := nil;
  SetSelParentItem (FSelParentItem);
end;

procedure TMenuDesignWindow.actDeleteExecute(Sender: TObject);
begin
 Delete;
end;

procedure TMenuDesignWindow.actNewItemExecute(Sender: TObject);
begin
 CreateItem('');
end;

procedure TMenuDesignWindow.actSeparatorExecute(Sender: TObject);
begin
 CreateItem('-');
end;

procedure TMenuDesignWindow.CreateItem(ACaption: string);
var mi: TMenuItem;
    Index: integer;
    NewNode: TTreeNode;
begin
 if not Assigned(FSelParentItem) then Exit;
 mi := TMenuItem.Create(Designer.GetRoot);
 mi.Name := UniqueName(mi);
 if ACaption = '' then
   mi.Caption := mi.Name
 else
   mi.Caption := ACaption;
 Index := -1;
 if (ActiveControl = ListView) and Assigned(ListView.Selected) and
    Assigned(ListView.Selected.Data) then
   Index := ListView.Selected.Index;
 ActiveControl := ListView;
 if Index = -1 then
  begin
   FSelParentItem.Add(mi);
   NewNode := TreeView.Items.AddChild(TreeView.Selected, GetItemTreeCaption(mi));
   NewNode.ImageIndex := -1;
   NewNode.SelectedIndex := -1;
   Index := FSelParentItem.Count - 1;
   if Assigned(TreeView.Selected) then
     TreeView.Selected.Expanded := True;
  end
  else
  begin
   FSelParentItem.Insert(Index, mi);
   NewNode := TreeView.Items.Insert(TreeView.Selected[Index], GetItemTreeCaption(mi));
   NewNode.Data := mi;
  end;
  NewNode.Data := mi;
  RebuildList;
  ListView.Selected := ListView.Items[Index];
end;

procedure TMenuDesignWindow.UpadteCaptions;
var i: integer;
    mi: TMenuItem;
begin
 Include(FState, usTree);
 TreeView.Images := TMenu(FParentComponent).Images;
 for i := 0 to TreeView.Items.Count - 1 do
  with TreeView.Items[i] do
  begin
   mi := TMenuItem(Data);
   Text := GetItemTreeCaption(mi);
   ImageIndex := mi.ImageIndex;
   SelectedIndex := mi.ImageIndex;
  end;
 Exclude(FState, usTree);
 Include(FState, usList);
 ListView.SmallImages := TMenu(FParentComponent).Images;
 for i := 0 to ListView.Items.Count - 1 do
  with ListView.Items[i] do
  if Assigned(Data) then
  begin
   mi := TMenuItem(Data);
   Caption := GetItemTreeCaption(mi);
   ImageIndex := mi.ImageIndex;
  end;
 Exclude(FState, usList);
end;

procedure TMenuDesignWindow.ListViewKeyPress(Sender: TObject; var Key: Char);
begin
  ActivateInspector(Key);
end;

procedure TMenuDesignWindow.TreeViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var nd: TTreeNode;
  function IsChild(Prn, Item: TMenuItem): Boolean;
  var i: integer;
  begin
    if Prn = nil then Result := False else
     if (Prn = Item) or (Prn.IndexOf(Item) <> -1) then Result := True else
      begin
       for i := 0 to Prn.Count - 1 do
        if IsChild(Prn.Items[i], Item) then
         begin
           Result := True;
           Exit;
         end;
       Result := False;
      end;
  end;

  function CheckNoChild(Item: TMenuItem): boolean;
  var i: integer;
  begin
    for i := 0 to List.Count - 1 do
     if IsChild(List[i] as TMenuItem, Item) then
      begin
        Result := False;
        Exit;
      end;
    Result := True;
  end;

begin
  GetSelList;
  nd := TreeView.GetNodeAt(X, Y);
  Accept := (nd <> nil) and (List.Count > 0) and CheckNoChild(TMenuItem(nd.Data));
end;

procedure TMenuDesignWindow.TreeViewDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var nd: TTreeNode;
begin
  nd := TreeView.GetNodeAt(X, Y);
  Cut;
  TreeView.Selected := nd;
  Paste;
end;

procedure TMenuDesignWindow.ItemsModified(const ADesigner: IDesigner);
begin
 inherited;
 if ADesigner = Designer then
  UpadteCaptions;
end;

function TMenuDesignWindow.EditAction(Action: TEditAction): Boolean;
begin
  case Action of
    eaCut: Cut;
    eaCopy: Copy;
    eaPaste: Paste;
    eaDelete: Delete;
  end;
 Result := True;
end;

procedure TMenuItemsPropertyEditor.Edit;
var Editor: IComponentEditor;
begin
  if PropCount <> 1 then Exit;
  Editor := GetComponentEditor(GetComponent(0) as TComponent, Designer);
  try
    Editor.Edit;
  finally
//    Editor  :=  nil;
  end;
end;

initialization
  RegisterComponentEditor(TMenu, TMenuEditor);
  RegisterPropertyEditor(TypeInfo(TMenuItem), nil, '', TMenuItemsPropertyEditor);
  RegisterNoIcon([TMenuItem]);

end.
