{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{           TCollection Property Editor Dialog          }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit uColnEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  uDesignWindows, StdCtrls, Menus, ExtCtrls, uDesignIntf, ComCtrls, ImgList,
  ActnList, ToolWin, uToolWnds, uDesignEditors, ActnPopup,
  PlatformDefaultStyleActnCtrls;

const
  AM_DeferUpdate = WM_USER + 100;  // avoids break-before-make listview ugliness

type

  TColOption = (coAdd, coDelete, coMove);
  TColOptions = set of TColOption;

  TCollectionEditor = class(TToolbarDesignWindow)
    Panel3: TPanel;
    ListView1: TListView;
    ImageList1: TImageList;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    AddCmd: TAction;
    DeleteCmd: TAction;
    MoveUpCmd: TAction;
    MoveDownCmd: TAction;
    SelectAllCmd: TAction;
    N2: TMenuItem;
    procedure AddClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MoveUpClick(Sender: TObject);
    procedure ListView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MoveDownClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure SelectAll1Click(Sender: TObject);
    procedure SelectAllCommandUpdate(Sender: TObject);
    procedure SelectionUpdate(Sender: TObject);
    procedure ListView1KeyPress(Sender: TObject; var Key: Char);
    procedure ListView1KeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FClosing: Boolean;
    FCollectionPropertyName: string;
    FStateLock: Integer;
    FItemIDList: TList;
    FAddedItemIDList: TList;
    FCollectionClassName: string;
    FSelectionError: Boolean;
    FColOptions: TColOptions;
    function GetRegKey: string;
    procedure SetCollectionPropertyName(const Value: string);
    procedure AMDeferUpdate(var Msg); message AM_DeferUpdate;
    procedure SetColOptions(Value: TColOptions);
    procedure CloseEditor;
  protected
    procedure Activated; override;
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
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent); override;
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemsModified(const ADesigner: IDesigner); override;
    function GetItemName(Index, ItemIndex: Integer): string;
    procedure GetSelection;
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections); override;
    procedure SetSelection;
    procedure UpdateListbox;
    property CollectionPropertyName: string read FCollectionPropertyName
      write SetCollectionPropertyName;
  end;

  TCollectionEditorClass = class of TCollectionEditor;

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
  ColOptions: TColOptions = [coAdd, coDelete, coMove]): TCollectionEditor;

implementation

{$R *.dfm}

uses Registry, TypInfo, uDesignConst, uComponentDesigner;

type
  TAccessCollection = class(TCollection); // used for protected method access
  TPersistentCracker = class(TPersistent);

var
  CollectionEditorsList: TList = nil;

function ShowCollectionEditorClass(ADesigner: IDesigner;
  CollectionEditorClass: TCollectionEditorClass; AComponent: TComponent;
  ACollection: TCollection; const PropertyName: string;
  ColOptions: TColOptions): TCollectionEditor;
var
  I: Integer;
begin
  if CollectionEditorsList = nil then
    CollectionEditorsList := TList.Create;
  for I := 0 to CollectionEditorsList.Count-1 do
  begin
    Result := TCollectionEditor(CollectionEditorsList[I]);
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
  ShowCollectionEditorClass(ADesigner, TCollectionEditor, AComponent,
    ACollection, PropertyName);
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

function TCollectionProperty.GetEditorClass: TCollectionEditorClass;
begin
  Result := TCollectionEditor;
end;

function TCollectionProperty.GetColOptions: TColOptions;
begin
  Result := [coAdd, coDelete, coMove];
end;

{ TCollectionEditor }

procedure TCollectionEditor.Activated;
begin
  Designer.Activate;
  SetSelection;
end;

procedure TCollectionEditor.SetColOptions(Value: TColOptions);
begin
  FColOptions := Value;
  AddCmd.Enabled := coAdd in Value;
end;

procedure TCollectionEditor.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);

  function IsOwnedBy(Owner, Child: TPersistent): Boolean;
  begin
    Result := False;
    if Owner = nil then Exit;
    while (Child <> nil) and (Child <> Owner) and not (Child is TComponent) do
      Child := TPersistentCracker(Child).GetOwner;
    Result := Child = Owner;
  end;

begin
  if (AItem = nil) or FClosing then Exit;
  if (Component = nil) or (csDestroying in Component.ComponentState) or
     (AItem = Component) or (AItem = Collection) or IsOwnedBy(AItem, Collection) then
    CloseEditor
  else if IsOwnedBy(Collection, AItem) then
    PostMessage(Handle, AM_DeferUpdate, 1, 0);
end;

procedure TCollectionEditor.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
begin
  if Designer = ADesigner then
    CloseEditor;
end;

procedure TCollectionEditor.CloseEditor;
begin
  FClosing := True;
  Collection := nil;
  Component := nil;
  Close;
end;

procedure TCollectionEditor.ItemsModified(const ADesigner: IDesigner);
var
  Root: IRoot;
begin
  if FClosing then exit;
  if Collection <> nil then
  begin
    UpdateListbox;
    Root := ActiveRoot;
    if (Root = nil) or (Root.GetDesigner <> Designer) then
      Exit;
    GetSelection;
  end;
end;

function TCollectionEditor.GetItemName(Index, ItemIndex: Integer): string;
begin
  with TAccessCollection(Collection) do
    if GetAttrCount < 1 then
      Result := Format('%d - %s',[ItemIndex, Collection.Items[ItemIndex].DisplayName])
    else Result := GetItemAttr(Index, ItemIndex);
end;

function TCollectionEditor.GetRegKey: string;
begin
  Result := ComponentDesigner.Environment.GetBaseRegKey + '\' +
    sIniEditorsName + '\Collection Editor';
end;

procedure TCollectionEditor.GetSelection;
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
    or (TCollectionEditor(List[0]).GetOwner = Collection)) then Exit;

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

procedure TCollectionEditor.LockState;
begin
  Inc(FStateLock);
end;

procedure TCollectionEditor.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin
end;

procedure TCollectionEditor.SetCollectionPropertyName(const Value: string);
begin
  if Value <> FCollectionPropertyName then
  begin
    FCollectionPropertyName := Value;
    Caption := Format(sColEditCaption, [Component.Name, DotSep, Value]);
  end;
end;

procedure TCollectionEditor.SetSelection;
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

procedure TCollectionEditor.UnlockState;
begin
  Dec(FStateLock);
end;

procedure TCollectionEditor.UpdateListbox;
var
  I, J: Integer;

  procedure UpdateSizes;
  var
    I: Integer;
  begin
    with TRegIniFile.Create(GetRegKey) do
    try
      Width := ReadInteger(FCollectionClassName, 'Width', Width);
      Height := ReadInteger(FCollectionClassName, 'Height', Height);
      Splitter1.Top := Toolbar1.Top + Toolbar1.Height;
      ToolBar1.Visible := ReadBool(FCollectionClassName, 'Toolbar', True);
      Splitter1.Visible := Toolbar1.Visible;
      LargeButtons := ReadBool(FCollectionClassName, 'LargeButtons', False);
      ListView1.HandleNeeded;
      if ListView1.Columns.Count > 1 then
        for I := 0 to ListView1.Columns.Count - 1 do
          ListView1.Column[I].Width := ReadInteger(FCollectionClassName,
            Format('Column%d', [I]), ListView1.Column[I].WidthType);
    finally
      Free;
    end;
  end;

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

procedure TCollectionEditor.AddClick(Sender: TObject);
var
  Item: TListItem;
  PrevCount: Integer;
begin
  SelectNone(False);
  Collection.BeginUpdate;
  try
    PrevCount := Collection.Count + 1;
    Collection.Add;
    { Take into account collections that free items }
    if PrevCount <> Collection.Count then
      UpdateListBox
    else
      ListView1.Selected := ListView1.Items.Add;
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

procedure TCollectionEditor.DeleteClick(Sender: TObject);
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
            raise Exception.Create(uDesignConst.SCantDeleteAncestor)
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

procedure TCollectionEditor.ListView1Click(Sender: TObject);
begin
//  SetSelection;
end;

procedure TCollectionEditor.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    ComponentDesigner.Environment.ModalEdit(#0,Self);
end;

procedure TCollectionEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_PROCESSKEY then
    ComponentDesigner.Environment.ModalEdit(#0,Self);
end;

procedure TCollectionEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  I: Integer;
begin
  if Component <> nil then
    Designer.SelectComponent(Component);
  with TRegIniFile.Create(GetRegKey) do
  try
    EraseSection(FCollectionClassName);
    WriteInteger(FCollectionClassName, 'Width', Width);
    WriteInteger(FCollectionClassName, 'Height', Height);
    WriteBool(FCollectionClassName, 'LargeButtons', LargeButtons);
    WriteBool(FCollectionClassName, 'Toolbar', ToolBar1.Visible);
    for I := 0 to ListView1.Columns.Count - 1 do
      WriteInteger(FCollectionClassName, Format('Column%d', [I]),
        ListView1.Column[I].WidthType);
  finally
    Free;
  end;
  Action := caFree;
  LockState;
end;


procedure TCollectionEditor.MoveUpClick(Sender: TObject);
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

procedure TCollectionEditor.MoveDownClick(Sender: TObject);
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

procedure TCollectionEditor.ListView1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Item: TListItem;
begin
  Item := ListView1.GetItemAt(X, Y);
  Accept := (Item <> nil) and (Source = ListView1) and
    (not Item.Selected);
end;

procedure TCollectionEditor.ListView1DragDrop(Sender, Source: TObject; X,
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


procedure TCollectionEditor.FormCreate(Sender: TObject);
begin
  FItemIdList := TList.Create;
  CollectionEditorsList.Add(Self);
end;

procedure TCollectionEditor.FormDestroy(Sender: TObject);
begin
  FItemIdList.Free;
  if FAddedItemIDList <> Nil then FreeAndNil(FAddedItemIDList);
  if CollectionEditorsList <> nil then
    CollectionEditorsList.Remove(Self);
end;

procedure TCollectionEditor.FormResize(Sender: TObject);
begin
  //if not ListView1.ShowColumnHeaders then
  //  ListView1.Column[0].Width := ListView1.ClientWidth;
end;

procedure TCollectionEditor.ListView1Change(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  Msg: TMsg;
begin
  if (Change = ctState) and (FStateLock = 0) then
    if not PeekMessage(Msg, Handle, AM_DeferUpdate, AM_DeferUpdate, PM_NOREMOVE) then
      PostMessage(Handle, AM_DeferUpdate, 0, 0);
end;

procedure TCollectionEditor.AMDeferUpdate(var Msg);
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

procedure TCollectionEditor.SelectAll1Click(Sender: TObject);
begin
  SelectAll();
end;

procedure TCollectionEditor.SelectionUpdate(Sender: TObject);
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

procedure TCollectionEditor.SelectAllCommandUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListView1.Items.Count > 0;
end;

procedure TCollectionEditor.SelectAll(DoUpdate: Boolean);
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

procedure TCollectionEditor.SelectNone(DoUpdate: Boolean);
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

procedure TCollectionEditor.ListView1KeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key in ['!'..'~'] then
  begin
    ComponentDesigner.Environment.ModalEdit(Key, Self);
    Key := #0;
  end;
end;

procedure TCollectionEditor.ListView1KeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_PROCESSKEY then
    ComponentDesigner.Environment.ModalEdit(#0,Self);
end;

function TCollectionEditor.CanAdd(Index: Integer): Boolean;
begin
  Result := True;
end;

procedure TCollectionEditor.FormShow(Sender: TObject);
begin
  inherited;
  { People who use the main window at the bottom of the screen will
    have the collection editor appear at the bottom of the screen (offscreen).
    This fixes that issue }
  MakeFullyVisible;
end;

initialization
  RegisterPropertyEditor(TypeInfo(TCollection), nil, '', TCollectionProperty);

finalization
  CollectionEditorsList.Free;
  CollectionEditorsList := nil;
end.
