unit uControlPostionSize;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Buttons, uDesigner;

type
  TfrmPostionSize = class(TForm)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel2: TPanel;
    rgHeight: TRadioGroup;
    rgWidth: TRadioGroup;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    FDesigner: TCustomDesigner;
    procedure SetWidth(NewWidth: Integer);
    procedure SetHeight(NewHeight: Integer);
  public
    constructor create(Owner: TComponent; BusinessDesigner: TCustomDesigner);
  end;

var
  frmPostionSize: TfrmPostionSize;

implementation

{$R *.dfm}

procedure TfrmPostionSize.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9', #8]) then
    key :=  #0;
end;

procedure TfrmPostionSize.BitBtn2Click(Sender: TObject);
begin
  close;
end;

procedure TfrmPostionSize.BitBtn1Click(Sender: TObject);
  function getMinWidth: integer;
  var
    i: integer;
    c: TControl;
  begin
    result  :=  -1;
    for i :=  0 to FDesigner.SelectionCount - 1 do
      if FDesigner.Selections[i] is TControl then
      begin
        c :=  TControl(FDesigner.Selections[i]);
        if (result = -1) or (result > c.Width) then
          result  :=  c.Width;
        c :=  nil;
      end;
  end;
  function getMaxWidth: integer;
  var
    i: integer;
    c: TControl;
  begin
    result  :=  0;
    for i :=  0 to FDesigner.SelectionCount - 1 do
      if FDesigner.Selections[i] is TControl then
      begin
        c :=  TControl(FDesigner.Selections[i]);
        if result < c.Width then
          result  :=  c.Width;
        c :=  nil;
      end;
  end;
  function getMinHeight: integer;
  var
    i: integer;
    c: TControl;
  begin
    result  :=  -1;
    for i :=  0 to FDesigner.SelectionCount - 1 do
      if FDesigner.Selections[i] is TControl then
      begin
        c :=  TControl(FDesigner.Selections[i]);
        if (result = -1) or (result > c.Width) then
          result  :=  c.Height;
        c :=  nil;
      end;
  end;
  function getMaxHeight: integer;
  var
    i: integer;
    c: TControl;
  begin
    result  :=  0;
    for i :=  0 to FDesigner.SelectionCount - 1 do
      if FDesigner.Selections[i] is TControl then
      begin
        c :=  TControl(FDesigner.Selections[i]);
        if result < c.Height then
          result  :=  c.Height;
        c :=  nil;
      end;
  end;
begin
  case rgWidth.ItemIndex of
    1: SetWidth(getMinWidth);
    2: SetWidth(getMaxWidth);
    3: SetWidth(strToint(Edit1.Text));
  end;
  case rgHeight.ItemIndex of
    1: SetHeight(getMinHeight);
    2: SetHeight(getMaxHeight);
    3: SetHeight(strToint(Edit2.Text));
  end;
  close;
end;

constructor TfrmPostionSize.create(Owner: TComponent;
  BusinessDesigner: TCustomDesigner);
begin
  inherited Create(Owner);
  FDesigner :=  BusinessDesigner;
end;

procedure TfrmPostionSize.SetWidth(NewWidth: Integer);
var
  i: integer;
  c: TControl;
begin
  FDesigner.ShowGrabHandle(False);
  for i :=  0 to FDesigner.SelectionCount - 1 do
    if FDesigner.Selections[i] is TControl then
    begin
      c :=  TControl(FDesigner.Selections[i]);
      c.Width :=  NewWidth;
      c :=  nil;
    end;
  FDesigner.ShowGrabHandle(True);
end;

procedure TfrmPostionSize.SetHeight(NewHeight: Integer);
var
  i: integer;
  c: TControl;
begin
  FDesigner.ShowGrabHandle(False);
  for i :=  0 to FDesigner.SelectionCount - 1 do
    if FDesigner.Selections[i] is TControl then
    begin
      c :=  TControl(FDesigner.Selections[i]);
      c.Height :=  NewHeight;
      c :=  nil;
    end;
  FDesigner.ShowGrabHandle(True);
end;

end.
