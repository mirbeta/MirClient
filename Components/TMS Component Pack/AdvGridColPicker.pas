{*******************************************************************}
{ TAdvGridColumnPicker component                                    }
{ for Delphi & C++Builder                                           }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{    copyright © 2015                                               }
{    Email : info@tmssoftware.com                                   }
{    Web   : http://www.tmssoftware.com                             }
{                                                                   }
{ The source code is given as is. The author is not responsible     }
{ for any possible damage done due to the use of this code.         }
{ The component can be freely used in any application. The source   }
{ code remains property of the writer and may not be distributed    }
{ freely as such.                                                   }
{*******************************************************************}

unit AdvGridColPicker;

interface

uses
  Classes, AdvGrid, BtnListB, Forms, Controls;

  // version history
  // v1.0.0.0 : First release

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

type
  TGridFieldChooser = class(TForm)
  private
    { Private declarations }
    colsource: TObject;
    FGrid: TAdvStringGrid;
    ButtonListbox1: TButtonListbox;
    procedure ButtonListbox1OleDragStart(Sender: TObject;
      DropIndex: Integer);
    procedure ButtonListbox1OleDragStop(Sender: TObject;
      OLEEffect: Integer);
    procedure ButtonListbox1OleDragOver(Sender: TObject;
      var Allow: Boolean);
  public
    { Public declarations }
    constructor CreateNew(AOwner: TComponent; Dummy: integer = 0); override;
    procedure CreateWnd; override;
    property Grid: TAdvStringGrid read FGrid write FGrid;
    procedure MoveToFieldChooser(AColumn: integer);
    procedure RemoveFromFieldChooser(AColumn, ToColumn: integer);
  end;

  TAdvGridColumnPicker = class(TComponent)
  private
    FGrid: TAdvStringGrid;
    FGridFieldChooser: TGridFieldChooser;
    FCaption: TCaption;
    FDragColumn: integer;
    FDragSource: TObject;
    FPickerHeight: integer;
    FPickerWidth: integer;
    procedure SetVersionString(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;
    function GetVersionString: string; virtual;

    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;

    procedure GridOleDrag(Sender: TObject; Arow,
      Acol: Integer; data: string; var allow: Boolean);
    procedure GridOleDragOver(Sender: TObject; Arow,
      Acol: Integer; var allow: Boolean);
    procedure GridOleDragStart(Sender: TObject; Arow,
      Acol: Integer);
    procedure GridOleDragStop(Sender: TObject;
      OLEEffect: Integer);
    procedure GridOleDropCol(Sender: TObject; Arow, Acol,
      DropCol: Integer);
    procedure EnsureFieldChooser;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddToFieldChooser(ColumnIndex: Integer);
    procedure RemoveFromFieldChooser(ColumnIndex: Integer);
    procedure Show;
    procedure Hide;
  published
    property Caption: TCaption read FCaption write FCaption;
    property Grid: TAdvStringGrid read FGrid write FGrid;
    property PickerWidth: integer read FPickerWidth write FPickerWidth default 200;
    property PickerHeight: integer read FPickerHeight write FPickerHeight default 300;
    property Version: string read GetVersionString write SetVersionString;
  end;


implementation

uses
  Windows, SysUtils;

{ TGridFieldChooser }

procedure TGridFieldChooser.ButtonListbox1OleDragOver(Sender: TObject;
  var Allow: Boolean);
begin
  Allow := Sender <> ColSource;
end;

procedure TGridFieldChooser.ButtonListbox1OleDragStart(Sender: TObject;
  DropIndex: Integer);
begin
  ColSource := Sender;
end;

procedure TGridFieldChooser.ButtonListbox1OleDragStop(Sender: TObject;
  OLEEffect: Integer);
begin
  ColSource := nil;
end;

constructor TGridFieldChooser.CreateNew(AOwner: TComponent; Dummy: integer);
begin
  inherited;
  BorderStyle := bsToolWindow;
  BorderIcons := [biMinimize, biMaximize];
  ButtonListbox1 := TButtonListBox.Create(Self);
  ButtonListbox1.Align := alClient;
  ButtonListbox1.Parent := Self;
  ButtonListbox1.ItemHeight := 23;

  ButtonListbox1.OnOleDragOver := ButtonListbox1OleDragOver;
  ButtonListbox1.OnOleDragStart := ButtonListbox1OleDragStart;
  ButtonListbox1.OnOleDragStop := ButtonListbox1OleDragStop;
end;

procedure TGridFieldChooser.CreateWnd;
begin
  inherited;
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
end;


procedure TGridFieldChooser.MoveToFieldChooser(AColumn: integer);
var
  s: string;
begin
  s := Grid.Cells[AColumn, 0];
  if s = '' then
    s := 'Column ' + inttostr(AColumn);

  ButtonListbox1.AddItem(s, TObject(AColumn));
  Grid.SuppressColumn(AColumn);
end;

procedure TGridFieldChooser.RemoveFromFieldChooser(AColumn, ToColumn: integer);
var
  i,j: integer;
  delta: integer;
begin
  Grid.UnSuppressColumn(AColumn);

  Grid.EnhRowColMove := true;

  if AColumn < ToColumn then
    dec(ToColumn);

  Grid.MoveColumn(AColumn, ToColumn);

  if ToColumn > AColumn then
    delta := -1
  else
    delta := +1;

  for i := 0 to ButtonListbox1.Items.Count - 1 do
  begin
    j := integer(ButtonListbox1.Items.Objects[i]);

    if (delta = -1) and (j > AColumn) and (j <= ToColumn) then
    begin
      j := j + delta;
      ButtonListbox1.Items.Objects[i] := TObject(j);
    end;

    if (delta = +1) and (j >= ToColumn) and (j < AColumn) then
    begin
      j := j + delta;
      ButtonListbox1.Items.Objects[i] := TObject(j);
    end;
  end;

  Grid.EnhRowColMove := false;

  ButtonListBox1.Items.Delete(ButtonListBox1.ItemIndex);
end;

{ TAdvGridColumnPicker }

procedure TAdvGridColumnPicker.AddToFieldChooser(ColumnIndex: Integer);
begin
  if not Assigned(FGrid) then
    raise Exception.Create('No grid assigned');

  EnsureFieldChooser;

  FGridFieldChooser.MoveToFieldChooser(ColumnIndex);
end;

constructor TAdvGridColumnPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGrid := nil;
  FGridFieldChooser := nil;
  FCaption := 'Columns';
  FPickerWidth := 200;
  FPickerHeight := 300;
end;

destructor TAdvGridColumnPicker.Destroy;
begin
  if Assigned(FGridFieldChooser) then
    FGridFieldChooser.Close;
  inherited;
end;

procedure TAdvGridColumnPicker.EnsureFieldChooser;
begin
  if not Assigned(FGridFieldChooser) then
    FGridFieldChooser := TGridFieldChooser.CreateNew(Application);

  FGridFieldChooser.Caption := Caption;
  FGridFieldChooser.Grid := FGrid;
  FGridFieldChooser.Width := FPickerWidth;
  FGridFieldChooser.Height := FPickerHeight;
end;

function TAdvGridColumnPicker.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvGridColumnPicker.GetVersionString: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

procedure TAdvGridColumnPicker.GridOleDrag(Sender: TObject; ARow, ACol: Integer;
  data: string; var Allow: Boolean);
begin
  Allow := (ARow = 0) and (ACol >= Grid.FixedCols);
  FDragColumn := Acol;
end;

procedure TAdvGridColumnPicker.GridOleDragOver(Sender: TObject; Arow,
  Acol: Integer; var allow: Boolean);
begin
  Allow := (FDragSource = nil) and (acol > 0) and (arow = 0);
end;

procedure TAdvGridColumnPicker.GridOleDragStart(Sender: TObject; Arow,
  Acol: Integer);
begin
  FDragSource := Sender;
end;

procedure TAdvGridColumnPicker.GridOleDragStop(Sender: TObject;
  OLEEffect: Integer);
begin
  FGridFieldChooser.MoveToFieldChooser(FDragColumn);
  FDragSource := nil;
end;

procedure TAdvGridColumnPicker.GridOleDropCol(Sender: TObject; Arow, Acol,
  DropCol: Integer);
begin
  FGridFieldChooser.RemoveFromFieldChooser(DropCol,ACol);
end;

procedure TAdvGridColumnPicker.Hide;
begin
  if Assigned(FGridFieldChooser) then
    FGridFieldChooser.Hide;
end;

procedure TAdvGridColumnPicker.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FGrid) then
    FGrid := nil;
end;

procedure TAdvGridColumnPicker.RemoveFromFieldChooser(ColumnIndex: Integer);
var
  i: integer;
begin
  if not Assigned(FGridFieldChooser) then
    Exit;

  FGridFieldChooser.RemoveFromFieldChooser(ColumnIndex, ColumnIndex);

  for i := 0 to FGridFieldChooser.ButtonListbox1.Items.Count - 1 do
  begin
    if Integer(FGridFieldChooser.ButtonListBox1.Items.Objects[i]) = ColumnIndex then
    begin
      FGridFieldChooser.ButtonListBox1.Items.Delete(i);
      break;
    end;
  end;
end;

procedure TAdvGridColumnPicker.SetVersionString(const Value: string);
begin
  //
end;

procedure TAdvGridColumnPicker.Show;
begin
  if not Assigned(FGrid) then
    raise Exception.Create('No grid assigned');

  EnsureFieldChooser;

  FGrid.EnhRowColMove := false;
  FGrid.DragDropSettings.OleDropSource := true;
  FGrid.DragDropSettings.OleDropTarget := true;
  FGrid.DragDropSettings.OleCopyAlways := true;
  FGrid.DragDropSettings.OleColumnsOnly := true;

  FGrid.OnOleDrag := GridOleDrag;
  FGrid.OnOleDragOver := GridOleDragOver;
  FGrid.OnOleDragStart := GridOleDragStart;
  FGrid.OnOleDragStop := GridOleDragStop;
  FGrid.OnOleDropCol := GridOleDropCol;

  FGridFieldChooser.Show;
end;

end.
