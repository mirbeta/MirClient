unit CollectionEditor;

interface

{错误原因主要在于Inspector为空，所以不能够赋值让其刷新属性}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Menus, ExtCtrls, ComCtrls, ImgList, ActnList, ToolWin,
  DesignEditors, DesignIntf;

const
  AM_DeferUpdate = WM_USER + 100;

type
  TColOption = (coAdd, coDelete, coMove);
  TColOptions = set of TColOption;

  TFrmCollectionEditor = class(TForm, IActivatable)
    acAdd: TAction;
    acDelete: TAction;
    acMoveDown: TAction;
    acMoveUp: TAction;
    acSelectAll: TAction;
    acTextLabels: TAction;
    acToolbar: TAction;
    ImageList: TImageList;
    ListView1: TListView;
    Panel: TPanel;
    Toolbar1: TToolbar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ActionList: TActionList;
    PopupMenu1: TPopupMenu;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    miMoveUp: TMenuItem;
    miMoveDown: TMenuItem;
    N1: TMenuItem;
    miToolbar: TMenuItem;
    PopupMenu2: TPopupMenu;
    miTextLabels: TMenuItem;
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acSelectAllExecute(Sender: TObject);
    procedure acToolbarExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure SelectAllCommandUpdate(Sender: TObject);
    procedure SelectionUpdate(Sender: TObject);
    procedure ListView1KeyPress(Sender: TObject; var Key: Char);
  private
    FPropPath:  String;
    FClosing: Boolean;
    FCollectionPropertyName: string;
    FStateLock: Integer;
    FItemIDList: TList;
    FCollectionClassName: string;
    FSelectionError: Boolean;
    FColOptions: TColOptions;
    FDesigner: IDesigner;
    procedure SetCollectionPropertyName(const Value: string);
    procedure AMDeferUpdate(var Msg); message AM_DeferUpdate;
    procedure SetColOptions(Value: TColOptions);
    procedure CloseEditor;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
  protected
    function  CanAdd(Index: Integer): Boolean; virtual;
    procedure LockState;
    procedure UnlockState;
    property StateLock: Integer read FStateLock;
    procedure SelectAll(DoUpdate: Boolean = True);
    procedure SelectNone(DoUpdate: Boolean = True);
  public
    Collection: TCollection;
    Component: TComponent;
    property Options: TColOptions read FColOptions write SetColOptions;
    procedure ItemsModified(ADesigner: IDesigner); virtual;
    function GetItemName(Index, ItemIndex: Integer): string;
    procedure GetSelection;
    procedure SetSelection;
    procedure UpdateListbox;
    property CollectionPropertyName: string read FCollectionPropertyName
      write SetCollectionPropertyName;
    property Designer: IDesigner read FDesigner;
  end;

  TCollectionEditorClass = class of TFrmCollectionEditor;

  { TCollectionProperty }

  TCollectionProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetEditorClass: TCollectionEditorClass; virtual;
    function GetColOptions: TColOptions; virtual;
  end;

function ShowCollectionEditorClass(ADesigner: IDesigner; Editor: TClassProperty;
  CollectionEditorClass: TCollectionEditorClass; AComponent: TComponent;
  ACollection: TCollection; const PropertyName: string;
  ColOptions: TColOptions = [coAdd, coDelete, coMove]): TFrmCollectionEditor;

implementation

{$R *.dfm}

uses
  Types,  Registry, TypInfo;

type
  TCollectionAccess = class(TCollection); // used for protected method access
  TPersistentAccess = class(TPersistent);

var
  CollectionEditorsList: TList = nil;

function ShowCollectionEditorClass(ADesigner: IDesigner; Editor: TClassProperty;
  CollectionEditorClass: TCollectionEditorClass; AComponent: TComponent;
  ACollection: TCollection; const PropertyName: string;
  ColOptions: TColOptions): TFrmCollectionEditor;
var
  I: Integer;
begin
  if CollectionEditorsList = nil then
    CollectionEditorsList := TList.Create;
  for I := 0 to CollectionEditorsList.Count-1 do
  begin
    Result := TFrmCollectionEditor(CollectionEditorsList[I]);
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
    FDesigner := ADesigner;
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

{ TCollectionProperty }

procedure TCollectionProperty.Edit;
var
  Obj: TPersistent;
begin
  Obj := GetComponent(0);
  while (Obj <> nil) and not (Obj is TComponent) do
    Obj := TPersistentAccess(Obj).GetOwner;
  ShowCollectionEditorClass(Designer, Self, GetEditorClass,
    TComponent(Obj), TCollection(GetOrdValue), GetName, GetColOptions);
end;

function TCollectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paSubProperties];
end;

function TCollectionProperty.GetEditorClass: TCollectionEditorClass;
begin
  Result := TFrmCollectionEditor;
end;

function TCollectionProperty.GetColOptions: TColOptions;
begin
  Result := [coAdd, coDelete, coMove];
end;

{ TcxCollectionEditor }

procedure TFrmCollectionEditor.acAddExecute(Sender: TObject);
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
      ListView1.Selected := ListView1.Items.Add;
  finally
    Collection.EndUpdate;
  end;
  SetSelection;
  Item := ListView1.Items[ListView1.Items.Count-1];
  Item.Focused := True;
  Item.MakeVisible(False);
end;

procedure TFrmCollectionEditor.acDeleteExecute(Sender: TObject);
var
  I, J: Integer;
begin
  Collection.BeginUpdate;
  try
    if ListView1.Selected <> nil then
      J := ListView1.Selected.Index
    else
      J := -1;
    if ListView1.SelCount = Collection.Count then
      Collection.Clear
    else if ListView1.SelCount > 0 then
      for I := ListView1.Items.Count - 1 downto 0 do
        if ListView1.Items[I].Selected then
          Collection.Items[I].Free;
  finally
    Collection.EndUpdate;
  end;
  UpdateListbox;
  if J >= ListView1.Items.Count then
    J := ListView1.Items.Count - 1;
  if (J > -1) and (J < ListView1.Items.Count) then
    ListView1.Selected := ListView1.Items[J];
  SetSelection;
end;

procedure TFrmCollectionEditor.acMoveDownExecute(Sender: TObject);
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
end;

procedure TFrmCollectionEditor.acMoveUpExecute(Sender: TObject);
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
end;

procedure TFrmCollectionEditor.acSelectAllExecute(Sender: TObject);
begin
  SelectAll;
end;

procedure TFrmCollectionEditor.acToolbarExecute(Sender: TObject);
begin
  with acToolbar do
  begin
    Checked := not Checked;
    Toolbar1.Visible := Checked;
  end;
end;

procedure TFrmCollectionEditor.SetColOptions(Value: TColOptions);
begin
  FColOptions := Value;
  acAdd.Enabled := coAdd in Value;
end;

procedure TFrmCollectionEditor.CloseEditor;
begin
  FClosing := True;
  Collection := nil;
  Component := nil;
  Close;
end;

procedure TFrmCollectionEditor.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if (Message.Active <> WA_INACTIVE) and (Collection <> nil) then
    SetSelection;
end;

procedure TFrmCollectionEditor.ItemsModified(ADesigner: IDesigner);
begin
  if FClosing then exit;
  if Collection <> nil then
  begin
    UpdateListbox;
    GetSelection;
  end;
end;

function TFrmCollectionEditor.GetItemName(Index, ItemIndex: Integer): string;
begin
  with TCollectionAccess(Collection) do
    if GetAttrCount < 1 then
      Result := Format('%d - %s',[ItemIndex, Collection.Items[ItemIndex].DisplayName])
    else Result := GetItemAttr(Index, ItemIndex);
end;

procedure TFrmCollectionEditor.GetSelection;
var
  I: Integer;
  Item: TCollectionItem;
{$IFDEF RTTIMULTISELECTION}
  List: TList;
{$ENDIF}
begin
  LockState;
  try
    ListView1.Selected := nil;
  finally
    UnlockState;
  end;
{$IFDEF RTTIMULTISELECTION}
  List := TList.Create;
  try
    Inspector.GetSelections(List);
    if (List.Count = 0) or (List.Count > Collection.Count) then Exit;
    if not ((List[0] = Component) or (List[0] = Collection)
      or (TcxCollectionEditor(List[0]).GetOwner = Collection)) then Exit;
    if List.Count > ListView1.Items.Count then
      UpdateListbox;
  finally
    List.Free;
  end;
{$ELSE}
  UpdateListbox;
{$ENDIF}
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

procedure TFrmCollectionEditor.LockState;
begin
  Inc(FStateLock);
end;

procedure TFrmCollectionEditor.SetCollectionPropertyName(const Value: string);
begin
  if Value <> FCollectionPropertyName then
  begin
    FCollectionPropertyName := Value;
    Caption := Format('编辑: %s%s%s',
      [Component.Name, DotSep, Value]);
  end;
end;

procedure TFrmCollectionEditor.SetSelection;
var
  I: Integer;
  tmp: TPersistent;
begin
  UpdateListBox;
//  if FSelectionError then Exit;
  try
    if ListView1.SelCount > 0 then
    begin
      FItemIDList.Clear;
      for I := 0 to ListView1.Items.Count - 1 do
        if ListView1.Items[I].Selected then
        begin
          FDesigner.SelectComponent(PersistentToDesignObject(Collection.Items[I]));
          FItemIDList.Add(Pointer(Collection.Items[I].ID));
        end;
    end
//    else
//      Inspector.Inspect([Collection]);
  except
    FSelectionError := True;
    Close;
  end;
end;

procedure TFrmCollectionEditor.UnlockState;
begin
  Dec(FStateLock);
end;

procedure TFrmCollectionEditor.UpdateListbox;
var
  I, J: Integer;

  procedure UpdateSizes;
  begin
      ListView1.HandleNeeded;
  end;

  procedure UpdateColumns;
  var
    I: Integer;
  begin
    if (Collection <> nil) and
      (((TCollectionAccess(Collection).GetAttrCount > 0) and
      (ListView1.Columns.Count <> TCollectionAccess(Collection).GetAttrCount)) or
      ((ListView1.Columns.Count = 0) and
      (TCollectionAccess(Collection).GetAttrCount < 1))) then
    begin
      ListView1.HandleNeeded;
      with TCollectionAccess(Collection) do
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
      end;
      UpdateSizes;
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
          for J := 1 to TCollectionAccess(Collection).GetAttrCount - 1 do
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
              Caption := TmpItems[I];
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

procedure TFrmCollectionEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  I: Integer;
begin
  try
    if Component <> nil then
      FDesigner.SelectComponent(PersistentToDesignObject(Component));
  except
    FDesigner.SelectComponent(nil);
  end;
  Action := caFree;
  LockState;
end;

procedure TFrmCollectionEditor.FormCreate(Sender: TObject);
begin
  FItemIdList := TList.Create;
  CollectionEditorsList.Add(Self);
end;

procedure TFrmCollectionEditor.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  FClosing := True;
  Collection := nil;
  Component := nil;
  FItemIdList.Free;
  if CollectionEditorsList <> nil then
    CollectionEditorsList.Remove(Self);
end;

procedure TFrmCollectionEditor.ListView1Change(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  Msg: TMsg;
begin
  if (Change = ctState) and (FStateLock = 0) then
    if not PeekMessage(Msg, Handle, AM_DeferUpdate, AM_DeferUpdate, PM_NOREMOVE) then
      PostMessage(Handle, AM_DeferUpdate, 0, 0);
end;

procedure TFrmCollectionEditor.ListView1DragDrop(Sender, Source: TObject; X,
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
end;

procedure TFrmCollectionEditor.ListView1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Item: TListItem;
begin
  Item := ListView1.GetItemAt(X, Y);
  Accept := (Item <> nil) and (Source = ListView1) and
    (not Item.Selected);
end;

procedure TFrmCollectionEditor.SelectAllCommandUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListView1.Items.Count > 0;
end;

procedure TFrmCollectionEditor.AMDeferUpdate(var Msg);
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

procedure TFrmCollectionEditor.SelectionUpdate(Sender: TObject);
var
  Enabled: Boolean;
begin
  Enabled := ListView1.Selected <> nil;
  if Enabled then
    if Sender = acDelete then
      Enabled := coDelete in Options
    else
      if (Sender = acMoveUp) or (Sender = acMoveDown) then
        Enabled := coMove in Options;
  (Sender as TAction).Enabled := Enabled;
end;

procedure TFrmCollectionEditor.SelectAll(DoUpdate: Boolean);
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

procedure TFrmCollectionEditor.SelectNone(DoUpdate: Boolean);
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

function TFrmCollectionEditor.CanAdd(Index: Integer): Boolean;
begin
  Result := True;
end;

procedure TFrmCollectionEditor.ListView1KeyPress(Sender: TObject;
  var Key: Char);
begin
  FDesigner.ModalEdit(Key, Self);
end;

initialization

finalization
  CollectionEditorsList.Free;
  CollectionEditorsList := nil;
end.
