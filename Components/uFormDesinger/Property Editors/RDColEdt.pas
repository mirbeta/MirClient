unit RDColEdt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DesignIntf, ActnList, Menus, PlatformDefaultStyleActnCtrls,
  ActnPopup, ToolWin, ComCtrls, ExtCtrls, ImgList, DesignEditors;

const
  AM_DeferUpdate  = WM_USER + 100;

type
  TColOption = (coAdd, coDelete, coMove);
  TColOptions = set of TColOption;

  TRDCollectionEditor = class(TForm, IDesignWindow, IDesignNotification,
    IEditHandler, IActivatable)
    ToolBar1: TToolBar;
    PopupMenu1: TPopupActionBar;
    Toolbar2: TMenuItem;
    ActionList1: TActionList;
    ToolbarCmd: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton5: TToolButton;
    ImageList1: TImageList;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    Panel1: TPanel;
    ListView1: TListView;
    AddCmd: TAction;
    DeleteCmd: TAction;
    MoveUpCmd: TAction;
    MoveDownCmd: TAction;
    SelectAllCmd: TAction;
    AddCmd1: TMenuItem;
    DeleteCmd1: TMenuItem;
    MoveDownCmd1: TMenuItem;
    MoveUpCmd1: TMenuItem;
    N1: TMenuItem;
    SelectAllCmd1: TMenuItem;
    N2: TMenuItem;
    procedure ToolbarCmdExecute(Sender: TObject);
    procedure ToolbarCmdUpdate(Sender: TObject);
    procedure AddCmdExecute(Sender: TObject);
    procedure DeleteCmdExecute(Sender: TObject);
    procedure DeleteCmdUpdate(Sender: TObject);
    procedure MoveUpCmdExecute(Sender: TObject);
    procedure MoveUpCmdUpdate(Sender: TObject);
    procedure MoveDownCmdExecute(Sender: TObject);
    procedure MoveDownCmdUpdate(Sender: TObject);
    procedure SelectAllCmdExecute(Sender: TObject);
    procedure SelectAllCmdUpdate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListView1KeyPress(Sender: TObject; var Key: Char);
  private
    FSelection: IDesignerSelections;
    FOwner: TComponent;
    FDesigner: IDesigner;
    FActive: Boolean;
    FLargeButtons: Boolean;
    FClosing: Boolean;
    FCollectionPropertyName: string;
    FStateLock: Integer;
    FItemIDList: TList;
    FAddedItemIDList: TList;
    FCollectionClassName: string;
    FSelectionError: Boolean;
    FColOptions: TColOptions;
    procedure SetCollectionPropertyName(const Value: string);
    procedure AMDeferUpdate(var Msg); message AM_DeferUpdate;
    procedure SetColOptions(Value: TColOptions);
    procedure CloseEditor;
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
    procedure SetDesigner(const Value: IDesigner);
  protected
    procedure ActivateInspector(Ch: Char);
    procedure SetSelection(const Components: IDesignerSelections); overload;
    function UniqueName(Component: TComponent): string; virtual;
    function  CanAdd(Index: Integer): Boolean; virtual;
    procedure LockState;
    procedure UnlockState;
    property StateLock: Integer read FStateLock;
    procedure SelectAll(DoUpdate: Boolean = True);
    procedure SelectNone(DoUpdate: Boolean = True);
  public
    Collection: TCollection;
    Component: TComponent;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // IEditHandler
    function GetEditState: TEditState; virtual;
    function EditAction(Action: TEditAction): Boolean; virtual;

    // IDesignNotification
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); virtual;
    procedure ItemInserted(const ADesigner: IDesigner; Item: TPersistent); virtual;
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections); virtual;
    procedure DesignerOpened(const Designer: IDesigner; AResurrecting: Boolean); virtual;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); virtual;
    procedure ItemsModified(const Designer: IDesigner); virtual;

    // IDesignWindowActions
    procedure WindowHide; virtual;
    procedure WindowShow; virtual;

    property Options: TColOptions read FColOptions write SetColOptions;
    function GetItemName(Index, ItemIndex: Integer): string;
    procedure GetSelection;
    procedure SetSelection; overload;
    procedure UpdateListbox;
    property CollectionPropertyName: string read FCollectionPropertyName
      write SetCollectionPropertyName;

    property Active: Boolean read FActive;
    property Designer: IDesigner read FDesigner write SetDesigner;
  end;

  TCollectionEditorClass = class of TRDCollectionEditor;

  TCollectionProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetEditorClass: TCollectionEditorClass; virtual;
    function GetColOptions: TColOptions; virtual;
  end;

procedure ShowCollectionEditor(ADesigner: IDesigner; AComponent: TComponent;
  ACollection: TCollection; const PropertyName: string);
function ShowCollectionEditorClass(ADesigner: IDesigner;
  CollectionEditorClass: TCollectionEditorClass; AComponent: TComponent;
  ACollection: TCollection; const PropertyName: string;
  ColOptions: TColOptions = [coAdd, coDelete, coMove]): TRDCollectionEditor;

var
  RDCollectionEditor: TRDCollectionEditor;

implementation
  uses Clipbrd, Registry, TypInfo, DesignConst;
{$R *.dfm}

type
  TAccessCollection = class(TCollection); // used for protected method access
  TPersistentCracker = class(TPersistent);

var
  CollectionEditorsList: TList = nil;

function ShowCollectionEditorClass(ADesigner: IDesigner;
  CollectionEditorClass: TCollectionEditorClass; AComponent: TComponent;
  ACollection: TCollection; const PropertyName: string;
  ColOptions: TColOptions): TRDCollectionEditor;
var
  I: Integer;
begin
  if CollectionEditorsList = nil then
    CollectionEditorsList := TList.Create;
  for I := 0 to CollectionEditorsList.Count-1 do
  begin
    Result := TRDCollectionEditor(CollectionEditorsList[I]);
    with Result do
      if (Designer = ADesigner) and (Component = AComponent)
        and (Collection = ACollection)
        and (CompareText(CollectionPropertyName, PropertyName) = 0) then
      begin
        Show;
        BringToFront;
        Exit;
      end;
  end;
  Result := CollectionEditorClass.Create(Application);
  with Result do
  try
    Options := ColOptions;
    Designer := ADesigner;
    Collection := ACollection;
    FCollectionClassName := ACollection.ClassName;
    Component := AComponent;
    CollectionPropertyName := PropertyName;
    UpdateListbox;
    Show;
  except
    Free;
  end;
end;

procedure ShowCollectionEditor(ADesigner: IDesigner; AComponent: TComponent;
  ACollection: TCollection; const PropertyName: string);
begin
  ShowCollectionEditorClass(ADesigner, TRDCollectionEditor, AComponent,
    ACollection, PropertyName);
end;

{ TRDDesignWindow }

procedure TRDCollectionEditor.ActivateInspector(Ch: Char);
begin
  Designer.ModalEdit(Ch, Self);
end;

procedure TRDCollectionEditor.AddCmdExecute(Sender: TObject);
var
  Item: TListItem;
  PrevCount: Integer;
begin
  SelectNone(False);
  Collection.BeginUpdate;
  try
    PrevCount := Collection.Count + 1;
    Collection.Add;

    if PrevCount <> Collection.Count then
      UpdateListBox
    else
    begin
      ListView1.Selected := ListView1.Items.Add;
      ListView1.Selected.Caption  :=  GetItemName(0, ListView1.Selected.Index);
    end;
  finally
    Collection.EndUpdate;
  end;

  SetSelection;
  if csAncestor in Component.ComponentState then
  begin
    if FAddedItemIDList = nil then
      FAddedItemIDList := TList.Create;
    FAddedItemIDList.Add(Pointer(Collection.Items[Collection.Count - 1].ID));
  end;
  Designer.Modified;
  { Focus last added item }
  Item := ListView1.Items[ListView1.Items.Count-1];
  Item.Focused := True;
  Item.MakeVisible(False);
end;

procedure TRDCollectionEditor.AMDeferUpdate(var Msg);
begin
  if FStateLock = 0 then
  begin
    if TMessage(Msg).WParam = 0 then
      SetSelection
    else
      ItemsModified(nil);
  end
  else
    PostMessage(Handle, AM_DeferUpdate, TMessage(Msg).WParam, TMessage(Msg).LParam);
end;

function TRDCollectionEditor.CanAdd(Index: Integer): Boolean;
begin
  Result := True;
end;

procedure TRDCollectionEditor.CloseEditor;
begin
  FClosing := True;
  Collection := nil;
  Component := nil;
  Close;
end;

constructor TRDCollectionEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RegisterDesignNotification(Self);
  FItemIdList := TList.Create;
  CollectionEditorsList.Add(Self);
end;

procedure TRDCollectionEditor.DeleteCmdExecute(Sender: TObject);
var
  I, J, K: Integer;
  Item: TCollectionItem;
  Found: Boolean;
begin
  Collection.BeginUpdate;
  try
    Designer.SetSelections(nil);
    if ListView1.Selected <> nil then
      J := ListView1.Selected.Index
    else J := -1;
    if (ListView1.SelCount = Collection.Count) and ((csAncestor in Component.ComponentState) = False) then
      Collection.Clear
    else if ListView1.SelCount > 0 then
      for I := ListView1.Items.Count - 1 downto 0 do
        if ListView1.Items[I].Selected then
        begin
          Found := False;
          if (FAddedItemIDList <> nil) and (FAddedItemIDList.Count > 0) then
          for K := 0 to FAddedItemIDList.Count -1 do
          begin
            Item := Collection.FindItemID(Integer(FAddedItemIDList[K]));
            if Item = Collection.Items[i] then
            begin
              FAddedItemIDList.Delete(K);
              Found := True;
              Break;
            end
            else Found := False;
          end;
          if (not Found) and (csAncestor in Component.ComponentState) then
            raise Exception.Create(SCantDeleteAncestor)
          else
            Collection.Items[I].Free;
        end;
  finally
    Collection.EndUpdate;
  end;
  UpdateListbox;
  if J >= ListView1.Items.Count then
    J := ListView1.Items.Count - 1;
  if (J > -1) and (J < ListView1.Items.Count) then
    ListView1.Selected := ListView1.Items[J];
  SetSelection;
  Designer.Modified;
end;

procedure TRDCollectionEditor.DeleteCmdUpdate(Sender: TObject);
var
  Enabled: Boolean;
begin
  Enabled := ListView1.Selected <> nil;
  if Enabled then
    if Sender = DeleteCmd then
      Enabled := coDelete in Options else
    if (Sender = MoveUpCmd) or (Sender = MoveDownCmd) then
      Enabled := coMove in Options;
  (Sender as TAction).Enabled := Enabled;
end;

procedure TRDCollectionEditor.DesignerClosed(const Designer: IDesigner;
  AGoingDormant: Boolean);
begin
  if Designer = FDesigner then
    CloseEditor;
end;

procedure TRDCollectionEditor.DesignerOpened(const Designer: IDesigner;
  AResurrecting: Boolean);
begin

end;

destructor TRDCollectionEditor.Destroy;
begin
  UnregisterDesignNotification(Self);
  FItemIdList.Free;
  if FAddedItemIDList <> Nil then FreeAndNil(FAddedItemIDList);
  if CollectionEditorsList <> nil then
    CollectionEditorsList.Remove(Self);
  inherited Destroy;
end;

function TRDCollectionEditor.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

procedure TRDCollectionEditor.FormClose(Sender: TObject; var Action: TCloseAction);
var
  I: Integer;
begin
  if Component <> nil then
    Designer.SelectComponent(Component);
  Action := caFree;
  LockState;
end;

function TRDCollectionEditor.GetEditState: TEditState;
begin
  Result  :=  [];
end;

function TRDCollectionEditor.GetItemName(Index, ItemIndex: Integer): string;
begin
  with TAccessCollection(Collection) do
    if GetAttrCount < 1 then
      Result := Format('%d - %s',[ItemIndex, Collection.Items[ItemIndex].DisplayName])
    else Result := GetItemAttr(Index, ItemIndex);
end;

procedure TRDCollectionEditor.GetSelection;
var
  I: Integer;
  Item: TCollectionItem;
  List: IDesignerSelections;
begin
  LockState;
  try
    ListView1.Selected := nil;
  finally
    UnlockState;
  end;

  List := CreateSelectionList;
  Designer.GetSelections(List);
  if (List.Count = 0) or (List.Count > Collection.Count) then Exit;
  if not ((List[0] = Component) or (List[0] = Collection)
    or (TRDCollectionEditor(List[0]).GetOwner = Collection)) then Exit;

  if List.Count > ListView1.Items.Count then UpdateListbox;

  LockState;
  try
    for I := FItemIDList.Count - 1 downto 0 do
    begin
      Item := Collection.FindItemID(Integer(FItemIDList[I]));
      if Item <> nil then
        ListView1.Items[Item.Index].Selected := True
      else FItemIDList.Delete(I);
    end;
  finally
    UnlockState;
  end;
end;

procedure TRDCollectionEditor.ItemDeleted(const ADesigner: IDesigner;
  Item: TPersistent);

  function IsOwnedBy(Owner, Child: TPersistent): Boolean;
  begin
    Result := False;
    if Owner = nil then Exit;
    while (Child <> nil) and (Child <> Owner) and not (Child is TComponent) do
      Child := TPersistentCracker(Child).GetOwner;
    Result := Child = Owner;
  end;

begin
  if (Item = nil) or FClosing then Exit;
  if (Component = nil) or (csDestroying in Component.ComponentState) or
     (Item = Component) or (Item = Collection) or IsOwnedBy(Item, Collection) then
    CloseEditor
  else if IsOwnedBy(Collection, Item) then
    PostMessage(Handle, AM_DeferUpdate, 1, 0);

end;

procedure TRDCollectionEditor.ItemInserted(const ADesigner: IDesigner;
  Item: TPersistent);
begin

end;

procedure TRDCollectionEditor.ItemsModified(const Designer: IDesigner);
begin
  if FClosing then exit;
  if Designer=FDesigner then
    if Collection <> nil then
    begin
      UpdateListbox;
      if (Designer = nil) then  Exit;
      GetSelection;
    end;
end;

procedure TRDCollectionEditor.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  Msg: TMsg;
begin
  if (Change = ctState) and (FStateLock = 0) then
    if not PeekMessage(Msg, Handle, AM_DeferUpdate, AM_DeferUpdate, PM_NOREMOVE) then
      PostMessage(Handle, AM_DeferUpdate, 0, 0);
end;

procedure TRDCollectionEditor.ListView1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Item: TListItem;
  I, J, InsPos: Integer;
  L: TList;
begin
  Item := ListView1.GetItemAt(X, Y);
  if Item <> nil then
    InsPos := Item.Index
  else Exit;
  L := TList.Create;
  try
    for I := 0 to ListView1.Items.Count - 1 do
      if ListView1.Items[I].Selected then
        L.Add(Collection.Items[I]);

    Collection.BeginUpdate;
    try
      for I := 0 to L.Count - 1 do
      with TCollectionItem(L[I]) do
      begin
        J := Index;
        Index := InsPos;
        if (J > InsPos) and (InsPos < Collection.Count) then
          Inc(InsPos);
      end;
    finally
      Collection.EndUpdate;
    end;
  finally
    L.Free;
  end;
  GetSelection;
  Designer.Modified;
end;

procedure TRDCollectionEditor.ListView1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Item: TListItem;
begin
  Item := ListView1.GetItemAt(X, Y);
  Accept := (Item <> nil) and (Source = ListView1) and
    (not Item.Selected);
end;

procedure TRDCollectionEditor.ListView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_PROCESSKEY then
    FDesigner.ModalEdit(#0,Self);
end;

procedure TRDCollectionEditor.ListView1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key in ['!'..'~'] then
  begin
    FDesigner.ModalEdit(Key, Self);
    Key := #0;
  end;
end;

procedure TRDCollectionEditor.LockState;
begin
  Inc(FStateLock);
end;

procedure TRDCollectionEditor.MoveDownCmdExecute(Sender: TObject);
var
  I, InsPos: Integer;
begin
  if (ListView1.SelCount = 0) or
    (ListView1.SelCount = Collection.Count) then Exit;

  InsPos := ListView1.Items.Count - 1;
  while not ListView1.Items[InsPos].Selected do
    Dec(InsPos);
  if InsPos < (ListView1.Items.Count -1) then Inc(InsPos);

  Collection.BeginUpdate;
  try
     for I := ListView1.Items.Count - 1 downto 0 do
       if ListView1.Items[I].Selected then
       begin
         Collection.Items[I].Index := InsPos;
         Dec(InsPos);
       end;
  finally
    Collection.EndUpdate;
  end;
  GetSelection;
  Designer.Modified;
end;

procedure TRDCollectionEditor.MoveDownCmdUpdate(Sender: TObject);
var
  Enabled: Boolean;
begin
  Enabled := ListView1.Selected <> nil;
  if Enabled then
    if Sender = DeleteCmd then
      Enabled := coDelete in Options else
    if (Sender = MoveUpCmd) or (Sender = MoveDownCmd) then
      Enabled := coMove in Options;
  (Sender as TAction).Enabled := Enabled;
end;

procedure TRDCollectionEditor.MoveUpCmdExecute(Sender: TObject);
var
  I, InsPos: Integer;
begin
  if (ListView1.SelCount = 0) or
    (ListView1.SelCount = Collection.Count) then Exit;

  InsPos := 0;
  while not ListView1.Items[InsPos].Selected do
    Inc(InsPos);
  if InsPos > 0 then Dec(InsPos);

  Collection.BeginUpdate;
  try
     for I := 0 to ListView1.Items.Count - 1 do
       if ListView1.Items[I].Selected then
       begin
         Collection.Items[I].Index := InsPos;
         Inc(InsPos);
       end;
  finally
    Collection.EndUpdate;
  end;
  GetSelection;
  Designer.Modified;
end;

procedure TRDCollectionEditor.MoveUpCmdUpdate(Sender: TObject);
var
  Enabled: Boolean;
begin
  Enabled := ListView1.Selected <> nil;
  if Enabled then
    if Sender = DeleteCmd then
      Enabled := coDelete in Options else
    if (Sender = MoveUpCmd) or (Sender = MoveDownCmd) then
      Enabled := coMove in Options;
  (Sender as TAction).Enabled := Enabled;
end;

procedure TRDCollectionEditor.SelectAll(DoUpdate: Boolean);
var
  I: Integer;
begin
  LockState;
  ListView1.Items.BeginUpdate;
  try
    for I := 0 to Listview1.Items.Count-1 do
      Listview1.Items[I].Selected := True;
  finally
    ListView1.Items.EndUpdate;
    UnlockState;
    if DoUpdate then SetSelection;
  end;
end;

procedure TRDCollectionEditor.SelectAllCmdExecute(Sender: TObject);
begin
  SelectAll();
end;

procedure TRDCollectionEditor.SelectAllCmdUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListView1.Items.Count > 0;
end;

procedure TRDCollectionEditor.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin

end;

procedure TRDCollectionEditor.SelectNone(DoUpdate: Boolean);
var
  I: Integer;
begin
  LockState;
  ListView1.Items.BeginUpdate;
  try
    for I := 0 to Listview1.Items.Count-1 do
      Listview1.Items[I].Selected := False;
  finally
    ListView1.Items.EndUpdate;
    UnlockState;
    if DoUpdate then SetSelection;
  end;
end;

procedure TRDCollectionEditor.SetCollectionPropertyName(const Value: string);
begin
  if Value <> FCollectionPropertyName then
  begin
    FCollectionPropertyName := Value;
    Caption := Format(sColEditCaption, [Component.Name, DotSep, Value]);
  end;
end;

procedure TRDCollectionEditor.SetColOptions(Value: TColOptions);
begin
  FColOptions := Value;
  AddCmd.Enabled := coAdd in Value;
end;

procedure TRDCollectionEditor.SetDesigner(const Value: IDesigner);
begin
  if FDesigner <> Value then
    FDesigner := Value;
end;

procedure TRDCollectionEditor.SetSelection;
var
  I: Integer;
  List: IDesignerSelections;
begin
  if csAncestor in Component.ComponentState then
    UpdateListBox;
  if FSelectionError then Exit;
  try
    if ListView1.SelCount > 0 then
    begin
      List := CreateSelectionList;
      FItemIDList.Clear;
      for I := 0 to ListView1.Items.Count - 1 do
        if ListView1.Items[I].Selected then
        begin
          List.Add(Collection.Items[I]);
          FItemIDList.Add(Pointer(Collection.Items[I].ID));
        end;
      Designer.SetSelections(List);
    end
    else
      Designer.SelectComponent(Collection);
  except
    FSelectionError := True;
    Application.HandleException(ExceptObject);
    Close;
  end;
end;

procedure TRDCollectionEditor.SetSelection(const Components: IDesignerSelections);
begin
  FDesigner.SetSelections(Components);
end;

procedure TRDCollectionEditor.ToolbarCmdExecute(Sender: TObject);
begin
  with ToolbarCmd do
    ToolBar1.Visible := not Checked;
end;

procedure TRDCollectionEditor.ToolbarCmdUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := Toolbar1.Visible;
end;

function TRDCollectionEditor.UniqueName(Component: TComponent): string;
begin
  Result := Designer.UniqueName(Component.ClassName);
end;

procedure TRDCollectionEditor.UnlockState;
begin
  Dec(FStateLock);
end;

procedure TRDCollectionEditor.UpdateListbox;
var
  I, J: Integer;

  procedure UpdateColumns;
  var
    I: Integer;
  begin
    if (Collection <> nil) and
      (((TAccessCollection(Collection).GetAttrCount > 0) and
      (ListView1.Columns.Count <> TAccessCollection(Collection).GetAttrCount)) or
      ((ListView1.Columns.Count = 0) and
      (TAccessCollection(Collection).GetAttrCount < 1))) then
    begin
      ListView1.HandleNeeded;
      with TAccessCollection(Collection) do
      begin
        if GetAttrCount >= 1 then
          for I := 0 to GetAttrCount - 1 do
            with ListView1.Columns.Add do
            begin
              Caption := GetAttr(I);
              Width := -2;
            end
        else
          with ListView1.Columns.Add do
            Width := -1;
        if GetAttrCount >= 1 then
          ListView1.ShowColumnHeaders := True
        //else
        //  ListView1.Column[0].Width := ListView1.ClientWidth;
      end;
    end;
  end;

  procedure FetchItems(List: TStrings);
  var
    I, J: Integer;
    SubList: TStringList;
  begin
    if Collection <> nil then
      for I := 0 to Collection.Count - 1 do
        if CanAdd(I) then
        begin
          SubList := TStringList.Create;
          for J := 1 to TAccessCollection(Collection).GetAttrCount - 1 do
            SubList.Add(GetItemName(J, I));
          List.AddObject(GetItemName(0, I), SubList);
        end;

  end;

  function ItemsEqual(ListItems: TListItems; Items: TStrings): Boolean;
  var
    I, J: Integer;
  begin
    Result := False;
    if ListItems.Count <> Items.Count then Exit;
    for I := 0 to ListItems.Count - 1 do
    begin
      if ListItems[I].Caption = Items[I] then
      begin
        for J := 0 to ListItems[I].SubItems.Count - 1 do
          if ListItems[I].SubItems[J] <> TStrings(Items.Objects[I])[J] then
            Exit;
      end
      else
        Exit;
    end;
    Result := True;
  end;

var
  TmpItems: TStringList;
begin
  if Collection = nil then Exit;
  LockState;
  try
    TmpItems := TStringList.Create;
    FetchItems(TmpItems);
    try
      if (TmpItems.Count = 0) or not ItemsEqual(ListView1.Items, TmpItems) then
      begin
        ListView1.Items.BeginUpdate;
        try
          UpdateColumns;
          ListView1.Items.Clear;
          for I := 0 to TmpItems.Count - 1 do
            with ListView1.Items.Add do
            begin
              Caption := TmpItems.Strings[I];
              for J := 0 to TStrings(TmpItems.Objects[I]).Count - 1 do
                SubItems.Add(TStrings(TmpItems.Objects[I])[J]);
            end;
        finally
          ListView1.Items.EndUpdate;
        end;
      end;
    finally
      for I := 0 to TmpItems.Count - 1 do
        TStrings(TmpItems.Objects[I]).Free;
      TmpItems.Free;
    end;
  finally
    UnlockState;
  end;
end;

procedure TRDCollectionEditor.WindowHide;
begin
  if Visible then
    ShowWindow(Handle, SW_HIDE);
end;

procedure TRDCollectionEditor.WindowShow;
const
  ShowCommands: array[TWindowState] of Word =
    (SW_SHOWNOACTIVATE, SW_SHOWMINNOACTIVE, SW_SHOWMAXIMIZED);
begin
  if Visible then
    ShowWindow(Handle, ShowCommands[WindowState]);
end;

procedure TRDCollectionEditor.WMActivate(var Msg: TWMActivate);
begin
  inherited;
  FActive := Msg.Active <> 0;
  if FActive then
  begin
    Designer.Activate;
    SetSelection;
  end;
end;

procedure TRDCollectionEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_PROCESSKEY then
    FDesigner.ModalEdit(Char(Key),Self);
end;

procedure TRDCollectionEditor.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    FDesigner.ModalEdit(Key,Self);
end;

procedure TRDCollectionEditor.FormShow(Sender: TObject);
begin
  inherited;
  MakeFullyVisible;
end;

{ TCollectionProperty }

procedure TCollectionProperty.Edit;
var
  Obj: TPersistent;
begin
  Obj := GetComponent(0);
  while (Obj <> nil) and not (Obj is TComponent) do
    Obj := TPersistentCracker(Obj).GetOwner;
  ShowCollectionEditorClass(Designer, GetEditorClass,
    TComponent(Obj), TCollection(GetOrdValue), GetName, GetColOptions);
end;

function TCollectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paVCL];
end;

function TCollectionProperty.GetColOptions: TColOptions;
begin
  Result := [coAdd, coDelete, coMove];
end;

function TCollectionProperty.GetEditorClass: TCollectionEditorClass;
begin
  Result := TRDCollectionEditor;
end;

initialization
  RegisterPropertyEditor(TypeInfo(TCollection), nil, '', TCollectionProperty);

finalization
  if CollectionEditorsList<>nil then
    FreeAndNil(CollectionEditorsList);

end.
