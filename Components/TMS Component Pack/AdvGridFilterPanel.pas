{*************************************************************************}
{ TMS TAdvGridFilterPanel component                                       }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2014 - 2015                                      }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit advgridfilterpanel;

interface

uses
  Classes, Windows, StdCtrls, ExtCtrls, Controls, SysUtils, Dialogs, Forms,
  AdvGrid, ComCtrls, Buttons, AdvUtil ,Spin, Graphics, AdvCustomFilterPanel;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Issue with numeric filter conditions
  // v1.0.1.0 : New : SortColumNames property added
  //          : New : DefaultOperation added
  //          : New : DefaultColumn added
  //          : Improved : Auto filter remove on clear filter
  // v1.0.2.0 : New : Property StringLookup added

type
  TAdvGridFilterPanel = class;

  TAdvGridFilterPanel = class(TAdvCustomFilterPanel)
  private
    FGrid: TAdvStringGrid;
    FStringLookup: boolean;
    procedure SetGrid(const Value: TAdvStringGrid);
  protected
    procedure FilterToPanel(grid: TAdvStringGrid);
    procedure PanelToFilter(grid: TAdvStringGrid);

    procedure AddClear(Sender: TObject);override;
    procedure AddFilterClick(Sender: TObject); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function GetColumnNames: TStringList; override;
    function GetFilterOperations: TStringList; override;
    procedure RemoveClickHandler(Sender: TObject); override;
    procedure AddClickHandler(Sender: TObject); override;
    function GetEditorType(AColumn: integer): TColumnEditor; override;
    procedure RestoreFilterClick(Sender: TObject); override;
    function AddEdit(groupbox: TCustomControl; AColumn: integer; AText: string): TControl; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init; override;
  published
    property Grid: TAdvStringGrid read FGrid write SetGrid;
    property StringLookup: boolean read FStringLookup write FStringLookup default false;
  end;

  TAdvGridFilterForm = class(TAdvCustomFilterForm)
  private
    FGrid: TAdvStringGrid;
    FFilterPanel: TAdvGridFilterPanel;
    procedure SetGrid(const Value: TAdvStringGrid);
  protected
    procedure CreateWnd; override;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: integer = 0); override;
    function CreateFilterPanel(AOwner: TComponent): TAdvCustomFilterPanel; override;
  published
    property Grid: TAdvStringGrid read FGrid write SetGrid;
  end;

  TAdvGridFilterDialog = class(TAdvCustomGridFilterDialog)
  private
    FGrid: TAdvStringGrid;
    function GetVersion: string;
    function GetVersionNr: Integer;
    procedure SetGrid(const Value: TAdvStringGrid);
    procedure SetVersion(const Value: string);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure FormShow(Sender: TObject); override;
    function CreateFilterForm: TAdvCustomFilterForm; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Grid: TAdvStringGrid read FGrid write SetGrid;
    property Version: string read GetVersion write SetVersion;
  end;

implementation

uses
  AdvFilterPanelButton;

{$R AdvGridFilter.res}

const
  ERRGRIDASSIGN = 'Please assign a TAdvStringGrid to the grid property.';
  containerX = 5;
  containerY = 85;

{ TAdvGridFilterPanel }
procedure TAdvGridFilterPanel.AddClear(Sender: TObject);
var
  I: integer;
begin
  inherited;

  if not Assigned(Grid) then
    raise Exception.Create(ERRGRIDASSIGN);

  //free the box first
  if CustomControl.ControlCount - 1 >= 0 then
  begin
    for I := CustomControl.ControlCount - 1 downto 0 do
    begin
      CustomControl.Controls[I].Free;
    end;
  end;

  Grid.Filter.Clear;
  Grid.FilterActive := false;

  AddNewRow(0, false);
end;

procedure TAdvGridFilterPanel.AddClickHandler(Sender: TObject);
var
  ctrlscnt, ccount, ctrlpos: Integer;
  grpName: string;
  grp: TAdvFilterGroupBox;
  I, J: integer;
begin
  ctrlpos := -1;
  ccount := -1; // combo counts

  // look for n° of controls in the scrollbox
  ctrlscnt := CustomControl.ControlCount;

  // find the group
  grpName := TAdvFilterGroupBox(TButton(Sender).Parent).Name;
  grp := nil;

  while ctrlpos = -1 do
  begin
    for I := 0 to CustomControl.ControlCount - 1 do
    begin
      if (CustomControl.Controls[I] is TAdvFilterGroupBox)  then
      begin
        if grpName = TAdvFilterGroupBox(CustomControl.Controls[I]).Name then
        begin
          grp := TAdvFilterGroupBox(CustomControl.Controls[I]);
          ctrlpos := I;
        end;
      end;
    end;
  end;

  ctrlpos := ctrlscnt - 1;

  //first move all other controls beneath us lower
  for J := 0 to CustomControl.ControlCount - 1 do
  begin
    if (CustomControl.Controls[J]).Top > grp.Top then
    begin
      (CustomControl.Controls[J]).Top :=  (CustomControl.Controls[J]).Top + containerY ;

      // lessen the count with every higher Y position
      if (ctrlpos - 1 >= 0) then
        Dec(ctrlpos);
    end;

    if (CustomControl.Controls[J] is TComboBox) then
    begin
      Inc(ccount);
    end;
  end;

  if (ctrlpos-ccount >= 0) and (ctrlpos = (ctrlscnt-1)) then
    ctrlpos := ctrlpos-ccount
  else
    if ctrlpos > 2 then
      ctrlpos := ctrlpos-1;

  if ctrlpos = 0 then
    AddNewRow(1,true)
  else
    AddNewRow(ctrlpos,true);

  SetCustomControlHeight;
end;

function TAdvGridFilterPanel.AddEdit(groupbox: TCustomControl; AColumn: integer;
  AText: string): TControl;
var
  ed: TControl;
begin
  if StringLookup then
    ed := TComboBox.Create(groupbox)
  else
    ed := TEdit.Create(groupbox);

  with ed do
  begin
    Left := 310;
    top := 28;
    Width := 121;
    Height := 21;
    Parent := groupbox;
    Tag := 3;
    Text := AText;
    TabStop := true;
    TabOrder := 2;
  end;

  if StringLookup then
    (ed as TComboBox).Items.Assign(grid.DistinctValues(AColumn));


  Result := ed;
end;

constructor TAdvGridFilterPanel.Create(AOwner: TComponent);
var
  lbl: TLabel;
begin
  inherited Create(AOwner);

  FGrid := nil;
  Title := 'Grid Filter';

  lbl := TLabel.Create(Self);
  lbl.Caption := ERRGRIDASSIGN;
  lbl.Parent := CustomControl;
  lbl.Left := 20;
  lbl.Top := 20;
end;

destructor TAdvGridFilterPanel.Destroy;
begin
  inherited;
end;

procedure TAdvGridFilterPanel.AddFilterClick(Sender: TObject);
begin
  inherited;

  if Assigned(Grid) then
    FilterToPanel(Grid)
  else
    raise Exception.Create(ERRGRIDASSIGN);
end;

procedure TAdvGridFilterPanel.FilterToPanel(grid: TAdvStringGrid);
var
  fd: TFilterData;
  C: TCustomControl;
  combo: TComboBox;
  I,II,idx: integer;
  nextop: AdvGrid.TFilterOperation;
  op: integer;
begin
  // first set filter inactive & clear
  Grid.FilterActive := False;
  Grid.Filter.Clear;
  nextop := foNone;

  // loop over all Controls and actions
  for I := 0 to CustomControl.controlCount - 1 do
  begin
    if CustomControl.Controls[I] is TCustomControl then
    begin
      // add a filter
      fd := Self.Grid.Filter.Add;
      fd.Operation := nextop;

      C := TCustomControl(CustomControl.Controls[I]);

      // loop over all controls inside the Containter
      for II := 0 to C.controlCount - 1 do
      begin
        // comboboxes
        if (C.Controls[II] is TComboBox) and ((C.Controls[II] as TComboBox).Style = csDropDownList) then
        begin
          // column filter
          if TComboBox(C.Controls[II]).Tag = tagColumn then
          begin
            combo := TComboBox(C.Controls[II]);

            if combo.ItemIndex >= 0 then
              fd.Column := integer((combo.Items.Objects[combo.ItemIndex]))
            else
            begin
              fd.Free;
              Break;
            end;
          end;

          // operation filter
          if TComboBox(C.Controls[II]).Tag = tagOperation then
          begin
            idx := TComboBox(C.Controls[II]).ItemIndex;
            op := integer(TComboBox(C.Controls[II]).Items.Objects[idx]);
            if op = Integer(foEqual) then
            begin
              fd.Condition := '=' + fd.Condition;
            end;

            if op = Integer(foNotEqual) then
            begin
              fd.Condition := '!' + fd.Condition;
            end;

            if op = Integer(foLargerThen) then
            begin
              fd.Condition := '>' + fd.Condition;
            end;

            if op = Integer(foSmallerThen) then
            begin
              fd.Condition := '<' + fd.Condition;
            end;

            if op = Integer(foContains) then
            begin
              fd.Condition := '**' + fd.Condition;
            end;

            if op = Integer(foEndsWith) then
            begin
              fd.Condition := '*' + fd.Condition;
            end;

            if op = Integer(foBeginsWith) then
            begin
              fd.Condition := fd.Condition + '-*';
            end;

            if op = Integer(foLargerOrEqual) then
            begin
              fd.Condition := '>=' + fd.Condition;
            end;

            if op = Integer(foSmallerOrEqual) then
            begin
              fd.Condition := '<=' + fd.Condition;
            end;
          end;
        end;

        // edits
        if C.Controls[II] is TEdit then
        begin
          if fd.Condition = '-*' then
          begin
            fd.Condition := TEdit(C.Controls[II]).Text + '*';
          end
          else
          begin
            if fd.Condition = '**' then
            begin
              fd.Condition := '*' + TEdit(C.Controls[II]).Text + '*';
            end
            else
            begin
              fd.Condition := fd.Condition + TEdit(C.Controls[II]).Text;
            end;
          end;
        end;

        // dropdown
        if (C.Controls[II] is TComboBox) and ((C.Controls[II] as TComboBox).Style = csDropDown) then
        begin
          if fd.Condition = '-*' then
          begin
            fd.Condition := TComboBox(C.Controls[II]).Text + '*';
          end
          else
          begin
            if fd.Condition = '**' then
            begin
              fd.Condition := '*' + TComboBox(C.Controls[II]).Text + '*';
            end
            else
            begin
              fd.Condition := fd.Condition + TComboBox(C.Controls[II]).Text;
            end;
          end;
        end;

        // datetimepickers
        if C.Controls[II] is TDateTimePicker then
        begin
          fd.Condition := fd.Condition +
            DateTimeToStr(TDateTimePicker(C.Controls[II]).DateTime);
        end;

        // spinedits
        if C.Controls[II] is TSpinEdit then
        begin
          fd.Condition := fd.Condition + TSpinEdit(C.Controls[II]).Text;
        end;

        // checkboxes
        if C.Controls[II] is TCheckBox then
        begin
          if C.Controls[II].Tag = tagValue then
          begin
            if TCheckBox(C.Controls[II]).Checked then
              fd.Condition := Grid.CheckTrue
            else
              fd.Condition := Grid.CheckFalse;
          end
          else
          begin
            fd.CaseSensitive := TCheckBox(C.Controls[II]).Checked;
          end;
        end;
      end; // for
    end; // if

    if CustomControl.Controls[I] is TComboBox then
    begin
      if TComboBox(CustomControl.Controls[I]).Tag = tagAction then
      begin
        if TComboBox(CustomControl.Controls[I]).ItemIndex = 0 then
          nextop := foAnd
        else
          nextop := foOR;
      end; // if
    end; // if
  end; // for

  Grid.FilterActive := true;
end;

function TAdvGridFilterPanel.GetColumnNames: TStringlist;
var
  sl: TStringList;
  I: integer;
begin
  sl := TStringList.Create;
  sl.Duplicates := dupIgnore;

  for I := 1 to grid.ColCount - 1 do
  begin
    if grid.Cells[I, 0] <> '' then
    begin
      sl.AddObject(grid.Cells[I, 0], TObject(I));
    end
    else
    begin
      sl.AddObject(IntToStr(I), TObject(I));
    end;
  end;

  Result := sl;
end;

function TAdvGridFilterPanel.GetEditorType(AColumn: integer): TColumnEditor;
var
  ct: TColumnType;
begin
  Result := ceText;

  ct := grid.ColumnType(AColumn);

  case ct of
  ctNumeric: Result := ceNumeric;
  ctFloat: Result := ceNumeric;
  ctAlpha: Result := ceText;
  ctDate: Result := ceDate;
  ctTime: Result := ceTime;
  ctBoolean: Result := ceBoolean;
  end;
end;

function TAdvGridFilterPanel.GetFilterOperations: TStringList;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.AddObject(UI.OperationEqual, TObject(foEqual));
  sl.AddObject(UI.OperationNotEqual,TObject(foNotEqual));
  sl.AddObject(UI.OperationLargerThen,TObject(foLargerThen));
  sl.AddObject(UI.OperationSmallerThen,TObject(foSmallerThen));
  sl.AddObject(UI.OperationContains,TObject(foContains));
  sl.AddObject(UI.OperationEndsWith,TObject(foEndsWith));
  sl.AddObject(UI.OperationBeginsWith,TObject(foBeginsWith));
  sl.AddObject(UI.OperationLargerOrEqual,TObject(foLargerOrEqual));
  sl.AddObject(UI.OperationSmallerOrEqual,TObject(foSmallerOrEqual));
  sl.AddObject(UI.OperationTrueFalse,TObject(foTrueFalse));
  Result := sl;
end;

procedure TAdvGridFilterPanel.Init;
begin
  AddClear(Self);
end;

procedure TAdvGridFilterPanel.Loaded;
begin
  inherited;
  Init;
end;

procedure TAdvGridFilterPanel.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited;
  if (AComponent = FGrid) and (AOperation = opRemove) then
    Grid := nil;
end;

procedure TAdvGridFilterPanel.PanelToFilter(grid: TAdvStringGrid);
var
  F : TFilter;
  I,II,Y,FC,LC: Integer;
  C: TCustomControl;
  combo, combo2: TComboBox;
  check: TCheckbox;
  cond_expr, fltr_expr,fltr_suff: string;
  celltype,celltype2: TAutoType;
begin
  if not Assigned(Self.Grid) then
    raise Exception.Create(ERRGRIDASSIGN);

  F := Self.Grid.Filter;
  FC := F.Count;
  LC := -1;

  AddClear(Self);

  //first make all necessary rows
  for I := 1 to FC -1 do
  begin
    AddNewRow(I, true);
  end;

  for I := 0 to CustomControl.ControlCount - 1 do
  begin
    if CustomControl.Controls[I] is TCustomControl then
    begin
      C := TCustomControl(CustomControl.Controls[I]);

      for II := 0 to C.ControlCount - 1 do
      begin
        if C.Controls[II] is TComboBox then
        begin
          combo2 := TComboBox(C.Controls[II]);

          if combo2.Tag = tagColumn then
          begin
            LC := LC + 1;

            if F.Count > 0 then
              combo2.ItemIndex := combo2.Items.IndexOf(grid.cells[F.Items[LC].Column,0])
            else
            begin
              break;
            end;
          end;//if tag = column

          cond_expr := F.Items[LC].Condition;

          if (combo2.Tag = tagOperation) and (cond_expr <> '') then
          begin
            fltr_expr := Copy(cond_expr,1,2);
            fltr_suff := Copy(cond_expr,Length(cond_expr), 1);

            if fltr_expr = '<=' then
            begin
              combo2.ItemIndex := Integer(foSmallerOrEqual);
              F.Items[LC].Condition := Copy(cond_expr,3,Length(cond_expr));
            end //if condition <=
            else
            if fltr_expr = '>=' then
            begin
              combo2.ItemIndex := Integer(foLargerOrEqual);
              F.Items[LC].Condition := Copy(cond_expr,3,Length(cond_expr));
            end //if condition <=
            else
            begin
              fltr_expr := Copy(cond_expr,1,1);

              if (fltr_expr = '*') and (fltr_suff = '*') then
              begin
                combo2.ItemIndex := Integer(foContains);
                F.Items[LC].Condition := Copy(cond_expr,2,Length(cond_expr) - 1);
                F.Items[LC].Condition := Copy(F.Items[LC].Condition,1,Length(F.Items[LC].Condition)-1);
              end //if condition *xxxx*
              else
              if fltr_expr = '=' then
              begin
                combo2.ItemIndex := Integer(foEqual);
                F.Items[LC].Condition := Copy(cond_expr,2,Length(cond_expr));
              end //if condition =
              else
              if fltr_expr = '<' then
              begin
                combo2.ItemIndex := Integer(foSmallerThen);
                F.Items[LC].Condition := Copy(cond_expr,2,Length(cond_expr));
              end //if condition <
              else
              if fltr_expr = '>' then
              begin
                combo2.ItemIndex := Integer(foLargerThen);
                F.Items[LC].Condition := Copy(cond_expr,2,Length(cond_expr));
              end//if condition >
              else
              if fltr_expr = '*' then
              begin
                combo2.ItemIndex := Integer(foEndsWith);
                F.Items[LC].Condition := Copy(cond_expr,2,Length(cond_expr));
              end //if condition xxx*
              else
              if fltr_suff = '*' then
              begin
                combo2.ItemIndex := Integer(foBeginsWith);
                F.Items[LC].Condition := Copy(cond_expr,1,Length(cond_expr) - 1);;
              end //if condition *xxx
              else
              if fltr_expr = '!' then
              begin
                combo2.ItemIndex := Integer(foNotEqual);
                F.Items[LC].Condition := Copy(cond_expr, 2, Length(cond_expr));
              end;//if condition !
            end;

            // Remove the informational value
            for Y := C.controlCount - 1 downto 0 do
            begin
              // comboboxes
              if (C.Controls[Y].Tag = tagValue) and not (C.Controls[Y] is TLabel)then
              begin
                 C.Controls[Y].Free;
              end;
            end;

            // make the value edit
            celltype := IsType(grid.Cells[F.Items[LC].Column, 1]);
            celltype2 := IsType(grid.Cells[F.Items[LC].Column, 2]);

            if celltype = celltype2 then
            begin
              case celltype of
                atNumeric:
                  begin
                    AddSpinEdit(C, F.Items[LC].Condition);
                  end;
                atFloat:
                  begin
                    AddSpinEdit(C, F.Items[LC].Condition);
                  end;
                atString:
                  begin
                    AddEdit(C, F.Items[LC].Column, F.Items[LC].Condition);
                  end;
                atDate:
                  begin
                    AddDate(C, F.Items[LC].Condition);
                  end;
                atTime:
                  begin
                    AddDate(C, F.Items[LC].Condition);
                  end;
              end;
            end
            else
            begin
              AddEdit(C, F.Items[LC].Column, F.Items[LC].Condition);
            end;
          end;//if tag = operation
        end;//if TComboBox

        if C.Controls[II] is TCheckBox then
        begin
          check := TCheckBox(C.Controls[II]);
          if check.Tag = tagCase then
          begin
            check.Checked := F.Items[LC].CaseSensitive;
          end;
        end;//if checkbox

      end;//for C
    end;//if C

    if CustomControl.Controls[I] is TComboBox then
    begin
      combo := TComboBox(CustomControl.Controls[I]);
      if F.Items[LC].Operation = foAND then
      begin
        combo.ItemIndex := 0;
      end
      else
      begin
        combo.ItemIndex := combo.Items.IndexOf('OR');
      end;//if else operation
    end;//if ComboBox
  end;//for scrollbox

end;

procedure TAdvGridFilterPanel.RemoveClickHandler(Sender: TObject);
var
  C: integer;
  container: TAdvFilterGroupBox;
  I: integer;
  Y: single;
  G: TControl;
  sl: TStringList;
begin
  // look for n° of controls in the scrollbox
  C := CustomControl.ControlCount;
  sl := TStringList.Create;

  container := TAdvFilterGroupBox(TButton(Sender).Parent);
  Y := container.Top;

  if C = 1 then
  begin
    CustomControl.Controls[0].Free;

    sl.Free;
    Exit;
  end;

  if C > 1 then
  begin
    if Y < containerY then
    begin
      CustomControl.Controls[1].Free;
      CustomControl.Controls[0].Free;

      //Lower the positions
      for I := 0 to C - 3 do
      begin
        G := CustomControl.Controls[I];
        G.Top := G.Top - containerY;
      end;

      sl.Free;

      Exit;
    end;

    I := C - 1;
    while (I > 0) do
    begin
      if Assigned(CustomControl.Controls[I]) then
      begin
        G := CustomControl.Controls[I];

        // All above => lower
        if G.Top > Y then
        begin
          G.Top := G.Top - containerY;
        end
        else
        begin
          if G.Top = Y then
          begin
            sl.Add(intToStr(I));
            sl.Add(intToStr(I-1));
            Dec(I);
          end;
        end;
      end;
      Dec(I);
    end;
  end;

    I := strToInt(sl[0]);
    CustomControl.Controls[I].Free;
    I := strToInt(sl[1]);
    CustomControl.Controls[I].Free;

  sl.Free;
end;

procedure TAdvGridFilterPanel.RestoreFilterClick(Sender: TObject);
begin
  inherited;
  PanelToFilter(Self.Grid);
end;

procedure TAdvGridFilterPanel.SetGrid(const Value: TAdvStringGrid);
var
  lbl: TLabel;
  I: integer;
begin
  if csDestroying in ComponentState then
    Exit;

  FGrid := Value;
  if Assigned(Value) then
    AddClear(Self)
  else
  begin
    //free the box first
    if CustomControl.ControlCount-1 >= 0 then
    begin
      for I := CustomControl.ControlCount-1 downto 0 do
      begin
        CustomControl.Controls[I].Free;
      end;
    end;

    lbl:= TLabel.Create(Self);
    lbl.Caption := ERRGRIDASSIGN;
    lbl.Parent := CustomControl;
  end;
end;

{ TAdvGridFilterDialog }

constructor TAdvGridFilterDialog.Create(AOwner: TComponent);
begin
  inherited;
end;

function TAdvGridFilterDialog.CreateFilterForm: TAdvCustomFilterForm;
begin
  Result := TAdvGridFilterForm.CreateNew(Application);
end;

destructor TAdvGridFilterDialog.Destroy;
begin
  inherited;
end;

procedure TAdvGridFilterDialog.FormShow(Sender: TObject);
begin
  inherited;
  ((Form as TAdvGridFilterForm).FilterPanel as TAdvGridFilterPanel).Grid := FGrid;
end;

function TAdvGridFilterDialog.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvGridFilterDialog.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvGridFilterDialog.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = Grid) then
    Grid := nil;
end;

procedure TAdvGridFilterDialog.SetGrid(const Value: TAdvStringGrid);
begin
  FGrid := Value;
end;

procedure TAdvGridFilterDialog.SetVersion(const Value: string);
begin

end; 

{ TAdvFilterForm }

function TAdvGridFilterForm.CreateFilterPanel(AOwner: TComponent): TAdvCustomFilterPanel;
begin
  Result := TAdvGridFilterPanel.Create(AOwner);
end;

constructor TAdvGridFilterForm.CreateNew(AOwner: TComponent; Dummy: integer);
begin
  inherited;
  if Assigned(FFilterPanel) then
    FFilterPanel.Assign(FilterPanel);
end;

procedure TAdvGridFilterForm.CreateWnd;
begin
  inherited;
  Filterpanel.Parent := Self;
end;

procedure TAdvGridFilterForm.SetGrid(const Value: TAdvStringGrid);
begin
  FGrid := Value;
end;

end.
