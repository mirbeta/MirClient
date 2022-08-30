unit AdvSmoothDropDownList;

{$I TMSDEFS.INC}

interface

uses
  Windows, Controls, ExtCtrls, Classes, StdCtrls,
  AdvSmoothListBox, AdvStyleIF, dialogs;

type
  TAdvSmoothCrackedListBox = class(TAdvSmoothListBox)

  end;

  TAdvSmoothDropDownListItemSelect = procedure(Sender: TObject; Value: String) of object;

  TAdvSmoothDropDownList = class(TPanel, ISmoothListBox)
  private
    FSearchItem: TAdvSmoothListBoxItem;
    DoSelect, SearchList: Boolean;
    FOwnerListBox: TAdvSmoothListBox;
    FOwnerListBoxItem: TAdvSmoothListBoxItem;
    FLst: TAdvSmoothListBox;
    FEdt: TEdit;
    FItems: TStringList;
    FOnItemSelect: TAdvSmoothDropDownListItemSelect;
    procedure SetItems(const Value: TStringList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure DoEnter; override;
  protected
    procedure KeyDownEdit(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ItemSelected(Sender: TObject; ItemIndex: integer);
    procedure DoItemSelect(Sender: TObject; Value: String);
    procedure StringListChanged(Sender: TObject);
    procedure UpdateListBox;
    procedure SetOwner(Owner: TAdvSmoothListBox; Item: TAdvSmoothListBoxItem);
    procedure Show(Item: TAdvSmoothListBoxItem);
  public
    property Items: TStringList read FItems write SetItems;
    property OnItemSelect: TAdvSmoothDropDownListItemSelect read FOnItemSelect write FOnItemSelect;
    //property listbox
  end;

implementation

{ TAdvSmoothDropDownList }

constructor TAdvSmoothDropDownList.Create(AOwner: TComponent);
begin
  inherited;
  Width := 200;
  Height := 210;
  FLst := TAdvSmoothListBox.Create(Self);
  FLst.Parent := Self;
  FEdt := TEdit.Create(Self);
  FEdt.Parent := Self;
  FEdt.OnKeyDown := KeyDownEdit;
  FEdt.Align := alTop;
  FLst.Align := alClient;
  FLst.OnItemSelected := ItemSelected;
  FItems := TStringList.Create;
  FItems.OnChange := StringListChanged;
  DoSelect := true;
end;

procedure TAdvSmoothDropDownList.CreateWnd;
begin
  inherited;
  if Assigned(FLst) then
  begin
    FLst.Header.Visible := false;
    FLst.Footer.Visible := false;
    FLst.LookupBar.Visible := false;
    FLst.SelectionMode := sPersistSelectionAlways;
    FLst.SetComponentStyle(tsOffice2007Luna);
    /// extra settings
  end;
end;

destructor TAdvSmoothDropDownList.Destroy;
begin
  FLst.Free;
  FEdt.Free;
  FItems.Free;
  inherited;
end;

procedure TAdvSmoothDropDownList.DoEnter;
begin
  inherited;
  if Assigned(FEdt) then
    FEdt.SetFocus;
end;

procedure TAdvSmoothDropDownList.DoItemSelect(Sender: TObject; Value: String);
begin
  if DoSelect then
  begin
    if Assigned(FOnItemSelect) then
      FOnItemSelect(Sender, Value);

    if Assigned(FOwnerListBox) and Assigned(FOwnerListBoxItem) then
      TAdvSmoothCrackedListBox(FOwnerListBox).DoItemDropDownSelect(FOwnerListBox, FOwnerListBoxItem, FOwnerListBoxItem.Index, Value);

    if Assigned(FLst) then
      FLst.InitState;
  end;
end;

procedure TAdvSmoothDropDownList.ItemSelected(Sender: TObject;
  itemindex: integer);
begin
  if ItemIndex <> -1 then
    DoItemSelect(Sender, Items[itemindex]);
end;

procedure TAdvSmoothDropDownList.KeyDownEdit(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    DoItemSelect(Sender, FEdt.Text);
end;

procedure TAdvSmoothDropDownList.SetItems(const Value: TStringList);
begin
  FItems.Assign(Value);
end;

procedure TAdvSmoothDropDownList.SetOwner(Owner: TAdvSmoothListBox;
  Item: TAdvSmoothListBoxItem);
begin
  FOwnerListBox := Owner;
  FOwnerListBoxItem := Item;
end;

procedure TAdvSmoothDropDownList.Show(Item: TAdvSmoothListBoxItem);
var
  i: integer;
begin
  SearchList := true;
  FSearchItem := Item;
  if SearchList then
  begin
    DoSelect := false;
    if Assigned(FEdt) and Assigned(FSearchItem) then
    begin
      FEdt.Text := '';
      for I := 0 to FLst.Items.Count - 1 do
      begin
        if FLst.Items[I].Caption = FSearchItem.Info then
        begin
          FLst.SelectedItemIndex := I;
          DoSelect := true;
          Exit;
        end;
      end;
      FEdt.Text := FSearchItem.Info;
    end;
    DoSelect := true;
    SearchList := false;
  end;
end;

procedure TAdvSmoothDropDownList.StringListChanged(Sender: TObject);
begin
  UpdateListBox;
end;

procedure TAdvSmoothDropDownList.UpdateListBox;
var
  i: integer;
begin
  if Assigned(FLst) then
  begin
    FLst.Items.BeginUpdate;
    FLst.Items.Clear;
    for I := 0 to Items.Count - 1 do
      FLst.Items.Add.Caption := Items[I];
    FLst.Items.EndUpdate;
  end;
end;

end.
