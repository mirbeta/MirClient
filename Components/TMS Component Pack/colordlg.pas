{**************************************************************************}
{ TColorChooser component                                                  }
{ for Delphi & C++Builder                                                  }
{ version 1.0                                                              }
{                                                                          }
{ Copyright © 2003                                                         }
{   TMS Software                                                           }
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

unit ColorDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TColorChooser = class(TForm)
    Button1: TButton;
    Button2: TButton;
    PaintBox1: TPaintBox;
    Label1: TLabel;
    Button3: TButton;
    CheckBox1: TCheckBox;
    Button4: TButton;
    Button5: TButton;
    ColorDialog1: TColorDialog;
    CheckBox2: TCheckBox;
    procedure PaintBox1Paint(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBox2Click(Sender: TObject);
  private
    FDirection: Boolean;
    FColorFrom: TColor;
    FColorTo: TColor;
    FBorderColor: TColor;
    procedure SetDirection(const AValue: Boolean);
    procedure SetColorTo(const Value: TColor);
    { Private declarations }
  public
    { Public declarations }
    property ColorFrom: TColor read FColorFrom write FColorFrom;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property Direction: Boolean read FDirection write SetDirection;
    property BorderColor: TColor read FBorderColor write FBorderColor;
  end;

var
  ColorChooser: TColorChooser;



implementation

{$R *.dfm}

procedure DrawHTMLGradient(Canvas: TCanvas; FromColor,ToColor,BorderColor: TColor; Steps: Integer;R:TRect; Direction: Boolean);
var
  diffr,startr,endr: Integer;
  diffg,startg,endg: Integer;
  diffb,startb,endb: Integer;
  iend: Integer;
  rstepr,rstepg,rstepb,rstepw: Real;
  i,stepw: Word;
begin
  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to Steps - 1 do
    begin
      endr := startr + Round(rstepr*i);
      endg := startg + Round(rstepg*i);
      endb := startb + Round(rstepb*i);
      stepw := Round(i*rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
      begin
        iend := R.Left + stepw + Trunc(rstepw) + 1;
        if iend > R.Right then
          iend := R.Right;
        Rectangle(R.Left + stepw,R.Top,iend,R.Bottom)
      end
      else
      begin
        iend := R.Top + stepw + Trunc(rstepw)+1;
        if iend > r.Bottom then
          iend := r.Bottom;
        Rectangle(R.Left,R.Top + stepw,R.Right,iend);
      end;
    end;

    if BorderColor <> clNone then
    begin
      Brush.Style := bsClear;
      Pen.Color := BorderColor;
      Rectangle(R.Left,R.Top,R.Right,R.Bottom);
    end;
  end;
end;


procedure TColorChooser.PaintBox1Paint(Sender: TObject);
begin
  if FColorTo = clNone then
    DrawHTMLGradient(PaintBox1.Canvas,FColorFrom,FColorFrom,FBorderColor,128,PaintBox1.ClientRect,FDirection)
  else
    DrawHTMLGradient(PaintBox1.Canvas,FColorFrom,FColorTo,FBorderColor,128,PaintBox1.ClientRect,FDirection);
end;

procedure TColorChooser.Button1Click(Sender: TObject);
begin
  colordialog1.Color := FColorFrom;
  if ColorDialog1.Execute then
  begin
    FColorFrom := ColorDialog1.Color;
    PaintBox1.Repaint;
  end;

end;

procedure TColorChooser.Button2Click(Sender: TObject);
begin
  colordialog1.Color := FColorTo;
  if ColorDialog1.Execute then
  begin
    FColorTo := ColorDialog1.Color;
    PaintBox1.Repaint;
  end;

end;

procedure TColorChooser.Button3Click(Sender: TObject);
begin
  colordialog1.Color := FBorderColor;
  if ColorDialog1.Execute then
  begin
    FBorderColor := ColorDialog1.Color;
    PaintBox1.Repaint;
  end;
end;

procedure TColorChooser.CheckBox1Click(Sender: TObject);
begin
  FDirection := checkbox1.Checked;
  PaintBox1.Repaint;
end;

procedure TColorChooser.SetDirection(const AValue: Boolean);
begin
  FDirection := AValue;
  CheckBox1.Checked := AValue;
end;

procedure TColorChooser.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  cl: TColor;
begin
  cl := ColorFrom;
  ColorFrom := ColorTo;
  ColorTo := cl;
  Repaint;
end;

procedure TColorChooser.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
    FColorTo := clNone
  else
    FColorTo := clWhite;  
    
  PaintBox1.Repaint;
end;

procedure TColorChooser.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  CheckBox2.Checked := FColorTo = clNone
end;

end.
