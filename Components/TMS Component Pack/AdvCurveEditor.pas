{**************************************************************************}
{ TAdvCurveEditor component                                                }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2013                                                       }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}
unit AdvCurveEditor;

interface

{$I TMSDEFS.INC}

uses
  Windows, Classes, Forms, Messages, SysUtils, Variants, Graphics, Controls, Dialogs,
  AdvCurve, Buttons, ExtCtrls, StdCtrls, AdvGDIP, Math;

type
  TAdvCurveEditorDialog = class;

  TSaveMode = (notsaving, saving, cancel);

  TFrmAdvCurveEditor = class(TForm)
    CurvePanel1: TAdvCurve;
    ListBox1: TListBox;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel3: TPanel;
    Button3: TButton;
    Button4: TButton;
    Button19: TButton;
    SpeedButton2: TSpeedButton;
    SpeedButton1: TSpeedButton;
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure CurvePanel1CurvePointSelected(Sender: TObject;
      ACurvePointIndex: Integer; ACurvePoint: TCurvePointRec;
      APoint: TCurvePoint);
    procedure CurvePanel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CurvePanel1CurveDraw(Sender: TObject; AGraphics: TGPGraphics);
    procedure CurvePanel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CurvePanel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FDialog: TAdvCurveEditorDialog;
    Mode: TSaveMode;
    FX, FXTo, FY, FYTo: Integer;
    FMouseDown: Boolean;
    { Private declarations }
  protected
    { Protected declarations }
    procedure Init;
    procedure InitListBox;
    procedure SaveChanges;
    function GetCurveControl: TAdvCustomCurve;
  public
    { Public declarations }
    property Dialog: TAdvCurveEditorDialog read FDialog write FDialog;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvCurveEditorDialog = class(TCommonDialog)
  private
    FCurveComponent: TControl;
    FForm: TFrmAdvCurveEditor;
  public
    function Execute: Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Form: TFrmAdvCurveEditor read FForm;
  published
    property CurveComponent: TControl read FCurveComponent write FCurveComponent;
  end;

var
  FrmAdvCurveEditor: TFrmAdvCurveEditor;

implementation

{$R *.dfm}

{ TAdvCurveEditorDialog }

constructor TAdvCurveEditorDialog.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TAdvCurveEditorDialog.Destroy;
begin
  inherited;
end;

function TAdvCurveEditorDialog.Execute: Boolean;
begin
  FForm := TFrmAdvCurveEditor.Create(Application);

  if not Assigned(CurveComponent) then
  begin
    raise Exception.Create('The dialog does not have a curve component assigned.');
    Result := False;
    Exit;
  end;

  try
    FForm.Dialog := Self;
    FForm.Init;
    Result := FForm.ShowModal = mrOK;
  finally
    FForm.Free;
  end;
end;

{ TFrmAdvCurveEditor }

procedure TFrmAdvCurveEditor.Button19Click(Sender: TObject);
begin
  SaveChanges;
end;

procedure TFrmAdvCurveEditor.Button3Click(Sender: TObject);
begin
  Mode := cancel;
  Self.Close;
  ModalResult := mrCancel;
end;

procedure TFrmAdvCurveEditor.Button4Click(Sender: TObject);
begin
  Mode := saving;
  SaveChanges;
  Self.Close;
  ModalResult := mrOk;
end;

procedure TFrmAdvCurveEditor.CurvePanel1CurveDraw(Sender: TObject;
  AGraphics: TGPGraphics);
var
  p: TGPPen;
begin
  if FMouseDown then
  begin
    p := TGPPen.Create(MakeColor(255, clBlack));
    p.SetDashStyle(DashStyleDot);
    AGraphics.DrawLine(p, FX, FY, FXTo, FYTo);
    p.Free;
  end;
end;

procedure TFrmAdvCurveEditor.CurvePanel1CurvePointSelected(Sender: TObject;
  ACurvePointIndex: Integer; ACurvePoint: TCurvePointRec; APoint: TCurvePoint);
begin
  ListBox1.ItemIndex := ListBox1.Items.IndexOfObject(APoint);
end;

procedure TFrmAdvCurveEditor.CurvePanel1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if CurvePanel1.XYToPoint(X, Y) > -1 then
    Exit;

  FMouseDown := True;
  FX := X;
  FY := Y;
  FXTo := X;
  FYTo := Y;
  CurvePanel1.Repaint;
end;

procedure TFrmAdvCurveEditor.CurvePanel1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDown then
  begin
    FXTo := X;
    FYTo := Y;
    CurvePanel1.Repaint;
  end;
end;

procedure TFrmAdvCurveEditor.CurvePanel1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  it: TCurvePoint;
begin
  if FMouseDown and not (Button = mbRight) then
  begin
    it := CurvePanel1.Points.Add;
    it.X := FX;
    it.Y := FY;
    it.XTo := FXTo;
    it.YTo := FYTo;

    InitListBox;
    ListBox1.ItemIndex := ListBox1.Items.IndexOfObject(it);
  end;

  FMouseDown := False;
  CurvePanel1.Repaint;
end;

procedure TFrmAdvCurveEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Mode = notsaving then
  begin
    case MessageDlg('Save Changes ?',mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
      begin
        SaveChanges;
        ModalResult := mrOk ;
      end;
      mrNo: ModalResult := mrCancel;
      mrCancel: Action := caNone;
    end;
  end;
end;

procedure TFrmAdvCurveEditor.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
end;

procedure TFrmAdvCurveEditor.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  if Key = VK_RETURN then
  begin
    Mode := saving;
    SaveChanges;
    Self.Close;
    ModalResult := mrok;
  end
  else if Key = VK_ESCAPE then
  begin
    Mode := cancel;
    Self.Close;
    ModalResult := mrCancel;
  end
  else if (Key = VK_DELETE) and (ListBox1.Focused) then
  begin
    if listbox1.ItemIndex >= 0 then
    begin
      if MessageDlg('Do you want to remove the selected point?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        i := Listbox1.ItemIndex;
        CurvePanel1.Points[i].Free;
        InitListBox;
        ListBox1.ItemIndex := Min(ListBox1.Items.Count-1, Max(0, i));
        ListBox1Click(Self);
      end;
    end;
  end;
end;

function TFrmAdvCurveEditor.GetCurveControl: TAdvCustomCurve;
begin
  Result := nil;
  if Assigned(Dialog) and Assigned(Dialog.CurveComponent) and (Dialog.CurveComponent is TAdvCustomCurve) then
      Result := (Dialog.CurveComponent as TAdvCustomCurve);
end;

procedure TFrmAdvCurveEditor.Init;
var
  w, h: Integer;
  c: TAdvCustomCurve;
begin
  Mode := notsaving;
  c := GetCurveControl;
  if Assigned(c) then
  begin
    CurvePanel1.BeginUpdate;
    CurvePanel1.Assign(c);
    if c.Width > CurvePanel1.Width then
    begin
      w := c.Width - CurvePanel1.Width;
      Width := Width + w;
    end;

    if c.Height > CurvePanel1.Height then
    begin
      h := c.Height - CurvePanel1.Height;
      Height := Height + h;
    end;

    CurvePanel1.ShowLines := True;
    CurvePanel1.ShowCurve := True;
    CurvePanel1.ShowHandles := True;
    CurvePanel1.EndUpdate;
    InitListBox;
  end;
end;

procedure TFrmAdvCurveEditor.InitListBox;
var
  I: Integer;
begin
  ListBox1.Items.Clear;
  for I := 0 to CurvePanel1.Points.Count - 1 do
  begin
    ListBox1.Items.AddObject('Point ' + inttostr(I) + ' [From:'+floattostr(CurvePanel1.Points[I].X)+','+floattostr(CurvePanel1.Points[I].Y) +
      ' To:'+floattostr(CurvePanel1.Points[I].Xto)+','+floattostr(CurvePanel1.Points[I].YTo) + ']', CurvePanel1.Points[I]);
  end;
end;

procedure TFrmAdvCurveEditor.ListBox1Click(Sender: TObject);
begin
//  CurvePanel1.SelectedPoint :=
end;

procedure TFrmAdvCurveEditor.SaveChanges;
var
  c: TAdvCustomCurve;
begin
  c := GetCurveControl;
  if Assigned(c) then
  begin
    c.BeginUpdate;
    c.Points.Assign(CurvePanel1.Points);
    c.EndUpdate;
  end;
end;

procedure TFrmAdvCurveEditor.SpeedButton1Click(Sender: TObject);
var
  i: Integer;
begin
  if listbox1.ItemIndex >= 0 then
  begin
    i := Listbox1.ItemIndex;
    CurvePanel1.Points[i].Free;
    InitListBox;
    ListBox1.ItemIndex := Min(ListBox1.Items.Count-1, Max(0, i));
    ListBox1Click(Self);
  end;
end;

procedure TFrmAdvCurveEditor.SpeedButton2Click(Sender: TObject);
var
  it: TCurvePoint;
begin
  it := CurvePanel1.Points.Add;
  it.X := CurvePanel1.Width / 3;
  it.Y := CurvePanel1.Height / 2;
  it.XTo := CurvePanel1.Width / 3 * 2;
  it.YTo := CurvePanel1.Height / 2;

  InitListBox;

  ListBox1.ItemIndex := ListBox1.Items.IndexOfObject(it);

end;

end.
