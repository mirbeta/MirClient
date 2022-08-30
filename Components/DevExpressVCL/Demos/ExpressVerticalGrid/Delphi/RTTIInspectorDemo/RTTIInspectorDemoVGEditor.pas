unit RTTIInspectorDemoVGEditor;

interface

uses
  Classes, Graphics, Controls, Forms,  Dialogs, cxVGrid, StdCtrls,
  ComCtrls, ExtCtrls, cxLookAndFeelPainters, cxButtons, cxDBVGrid, Menus;

type
  TcxVerticalGridEditor = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    lbRows: TListBox;
    btCategory: TcxButton;
    btEditor: TcxButton;
    btClose: TcxButton;
    btMultiEditor: TcxButton;
    btDelete: TcxButton;
    btClear: TcxButton;
    PopupMenu: TPopupMenu;
    miEditor: TMenuItem;
    miCategory: TMenuItem;
    miMultieditor: TMenuItem;
    N1: TMenuItem;
    miDelete: TMenuItem;
    miClearAll: TMenuItem;
    ShowVerticalGridLayoutEditor: TcxButton;
    procedure btCloseClick(Sender: TObject);
    procedure lbRowsClick(Sender: TObject);
    procedure btCategoryClick(Sender: TObject);
    procedure btEditorClick(Sender: TObject);
    procedure btMultiEditorClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure miEditorClick(Sender: TObject);
    procedure miCategoryClick(Sender: TObject);
    procedure miMultieditorClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miClearAllClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ShowVerticalGridLayoutEditorClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FVerticalGrid: TcxCustomVerticalGrid;
    FOnObjectSelected: TNotifyEvent;
    procedure AddRow(ARowClass: TcxCustomRowClass);
    function GetVerticalGrid: TcxCustomVerticalGrid;
    procedure UpdateButtons;
    procedure UpdateItems;
    procedure SetVerticalGrid(const Value: TcxCustomVerticalGrid);
    procedure SelectItem(AItem: Pointer);
  protected
    procedure InitFormEditor;
    procedure DoObjectSelected(Sender: TObject); virtual;
  public
    property OnObjectSelected: TNotifyEvent read FOnObjectSelected write FOnObjectSelected;
    procedure DoItemsModified;
    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid write SetVerticalGrid;
  end;

function GetVerticalGridEditor(AVerticalGrid: TcxCustomVerticalGrid; AEvent: TNotifyEvent): TcxVerticalGridEditor;

implementation

{$R *.dfm}

uses SysUtils;

type
  TcxCustomVerticalGridAccess = class(TcxCustomVerticalGrid);
  TcxVerticalGridCustomizingAccess = class(TcxVerticalGridCustomizing);

const
  SSubStr = 'Tcx';

var
  VerticalGridEditor: TcxVerticalGridEditor;

function GetVerticalGridEditor(AVerticalGrid: TcxCustomVerticalGrid; AEvent: TNotifyEvent): TcxVerticalGridEditor;
begin
  if VerticalGridEditor = nil then
  begin
    VerticalGridEditor := TcxVerticalGridEditor.Create(Application);
    with VerticalGridEditor do
      try
        VerticalGrid := AVerticalGrid;
        OnObjectSelected := AEvent;
        InitFormEditor;
      except
        Free;
      end;
  end;
   Result := VerticalGridEditor;
end;

function LockListBox(AListBox: TListBox): TNotifyEvent;
begin
  Result := AListBox.OnClick;
  AListBox.OnClick := nil;
end;

procedure UnlockListBox(AListBox: TListBox; APrevOnClick: TNotifyEvent);
begin
  AListBox.OnClick := APrevOnClick;
end;

procedure GenVerticalGridRowName(AVerticalGrid: TcxCustomVerticalGrid;
  ARow: TcxCustomRow);
var
  I: Integer;
  S, S1: string;
  ARowName: String;
  function IsComponentNameExist(ANewname: String): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i:=0 to AVerticalGrid.Parent.Parent.ComponentCount - 1 do
      if CompareText(AVerticalGrid.Parent.Parent.Components[i].Name, ANewname) = 0 then
        Exit;
    Result := False;
  end;
begin
  S1 := ARow.ClassName;
  I := Pos(SSubStr, S1);
  if I <> 0 then
    S := Copy(S1, I + Length(SSubStr), Length(S1));
  I := 0;
  repeat
    Inc(I);
    ARowName := AVerticalGrid.Name + S + IntToStr(I);
  until not IsComponentNameExist(ARowName);
  ARow.Name := ARowname;
end;

procedure TcxVerticalGridEditor.btCategoryClick(Sender: TObject);
begin
  AddRow(TcxCategoryRow);
end;

procedure TcxVerticalGridEditor.btEditorClick(Sender: TObject);
var
  AIntf: IcxVGridDesignerRows;
begin
  if VerticalGrid.GetInterface(IcxVGridDesignerRows, AIntf) then
    AddRow(AIntf.GetEditorRowClass);
end;

procedure TcxVerticalGridEditor.btMultiEditorClick(Sender: TObject);
var
  AIntf: IcxVGridDesignerRows;
begin
  if VerticalGrid.GetInterface(IcxVGridDesignerRows, AIntf) then
    AddRow(AIntf.GetMultiEditorRowClass);
end;

procedure TcxVerticalGridEditor.btDeleteClick(Sender: TObject);

  function FindItemToSelect: Pointer;
  var
    I: Integer;
  begin
    Result := nil;
    with lbRows do
    begin
      if ItemIndex = -1 then Exit;
      if not Selected[ItemIndex] then
        Result := Items.Objects[ItemIndex]
      else
      begin
        for I := ItemIndex + 1 to Items.Count - 1 do
          if not Selected[I] then
          begin
            Result := Items.Objects[I];
            Exit
          end;
        for I := ItemIndex - 1 downto 0 do
          if not Selected[I] then
          begin
            Result := Items.Objects[I];
            Exit
          end;
      end;
    end;
  end;

var
  AItem: Pointer;
begin
  if lbRows.ItemIndex <> -1 then
  begin
    DoObjectSelected(nil);
    AItem := FindItemToSelect;
    VerticalGrid.Remove(TcxCustomRow(lbRows.Items.Objects[lbRows.ItemIndex]));
    UpdateItems;
    SelectItem(AItem);
    lbRowsClick(nil);
  end;
end;

procedure TcxVerticalGridEditor.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TcxVerticalGridEditor.lbRowsClick(Sender: TObject);
begin
  if lbRows.ItemIndex <> -1 then
    DoObjectSelected(lbRows.Items.Objects[lbRows.ItemIndex]);
  UpdateButtons;
end;

procedure TcxVerticalGridEditor.AddRow(ARowClass: TcxCustomRowClass);
var
  ARow: TcxCustomRow;
begin
  if ARowClass <> nil then
  begin
    ARow := VerticalGrid.Add(ARowClass);
    GenVerticalGridRowName(VerticalGrid, ARow);
    UpdateItems;
    SelectItem(ARow);
    UpdateButtons;
  end;
end;

function TcxVerticalGridEditor.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := FVerticalGrid;
end;

procedure TcxVerticalGridEditor.UpdateButtons;
begin
  btDelete.Enabled := lbRows.ItemIndex <> -1;
  miDelete.Enabled := btDelete.Enabled;
  btClear.Enabled := lbRows.Items.Count > 0;
  miClearAll.Enabled := btClear.Enabled;
end;

procedure TcxVerticalGridEditor.UpdateItems;
var
  I, AItemIndex: Integer;
begin
  AItemIndex := lbRows.ItemIndex;
  try
    lbRows.Items.Clear;
    for I := 0 to VerticalGrid.Rows.Count - 1 do
      lbRows.Items.AddObject(VerticalGrid.Rows[I].Name, VerticalGrid.Rows.Items[I]);
  finally
    if AItemIndex <= lbRows.Items.Count - 1 then
      lbRows.ItemIndex := AItemIndex
    else
      lbRows.ItemIndex := lbRows.Items.Count - 1;
  end;
end;

procedure TcxVerticalGridEditor.InitFormEditor;
begin
  UpdateItems;
  UpdateButtons;
end;

procedure TcxVerticalGridEditor.DoItemsModified;
begin
  UpdateItems;
end;

procedure TcxVerticalGridEditor.btClearClick(Sender: TObject);
begin
  DoObjectSelected(nil);
  VerticalGrid.ClearRows;
  UpdateItems;
  UpdateButtons;
end;

procedure TcxVerticalGridEditor.FormActivate(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TcxVerticalGridEditor.miEditorClick(Sender: TObject);
begin
  btEditorClick(nil);
end;

procedure TcxVerticalGridEditor.miCategoryClick(Sender: TObject);
begin
  btCategoryClick(nil);
end;

procedure TcxVerticalGridEditor.miMultieditorClick(Sender: TObject);
begin
  btMultiEditorClick(nil);
end;

procedure TcxVerticalGridEditor.miDeleteClick(Sender: TObject);
begin
  btDeleteClick(nil);
end;

procedure TcxVerticalGridEditor.miClearAllClick(Sender: TObject);
begin
  btClearClick(nil);
end;

procedure TcxVerticalGridEditor.SetVerticalGrid(
  const Value: TcxCustomVerticalGrid);
begin
  FVerticalGrid := Value;
end;

procedure TcxVerticalGridEditor.DoObjectSelected(Sender: TObject);
begin
  if Assigned(FOnObjectSelected) then
    FOnObjectSelected(Sender);
end;

procedure TcxVerticalGridEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  VerticalGridEditor := nil;
end;

procedure TcxVerticalGridEditor.SelectItem(AItem: Pointer);
var
  ItemIndex: Integer;
begin
  ItemIndex := lbRows.Items.IndexOfObject(AItem);
  lbRows.ItemIndex := ItemIndex;
end;

procedure TcxVerticalGridEditor.ShowVerticalGridLayoutEditorClick(
  Sender: TObject);
begin
  with TcxCustomVerticalGridAccess(VerticalGrid) do
  begin
    TcxVerticalGridCustomizingAccess(Customizing).ShowCategoryButtons := True;
    Customizing.Visible := True;
  end;
end;

procedure TcxVerticalGridEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  with TcxCustomVerticalGridAccess(VerticalGrid) do
    Customizing.Visible := False;
end;

end.
