{***************************************************************************}
{ TAdvCardList component                                                    }
{ for Delphi & C++Builder                                                   }
{ version 1.0                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2005                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvCardListGradient;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ExtCtrls, AdvCardList;

type
  TGradientEditor = class(TForm)
    PaintBox1: TPaintBox;
    RadioGroup1: TRadioGroup;
    Button1: TButton;
    Button2: TButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ColorDialog1: TColorDialog;
    procedure PaintBox1Paint(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    FColor: TAdvGradient;
    FOldColorTo: TColor;
    procedure SetColor(const Value: TAdvGradient);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
    property Color: TAdvGradient read FColor write SetColor;
  end;

var
  GradientEditor: TGradientEditor;

implementation

{$R *.dfm}

{ TForm2 }

constructor TGradientEditor.Create(Aowner: TComponent);
begin
  inherited;
  FColor := TAdvGradient.Create;
end;

destructor TGradientEditor.Destroy;
begin
  FColor.Free;
  inherited;
end;


procedure TGradientEditor.SetColor(const Value: TAdvGradient);
begin
  if Value.ColorTo = clNone then
  begin
    RadioGroup1.ItemIndex := 0;
  end
  else
  begin
    if Value.Direction = gdVertical then
      RadioGroup1.ItemIndex := 1
    else
      RadioGroup1.ItemIndex := 2;
  end;
  FOldColorTo := Value.ColorTo;
  FColor.Assign(Value);
end;

procedure TGradientEditor.PaintBox1Paint(Sender: TObject);
begin
  FColor.Draw(PaintBox1.Canvas, PaintBox1.ClientRect);
end;

procedure TGradientEditor.SpeedButton1Click(Sender: TObject);
begin
  ColorDialog1.Color := FColor.Color;
  if ColorDialog1.Execute then
  begin
    FColor.Color := ColorDialog1.Color;
    PaintBox1.Repaint;
  end;  
end;

procedure TGradientEditor.SpeedButton2Click(Sender: TObject);
begin
  ColorDialog1.Color := FColor.ColorTo;
  if ColorDialog1.Execute then
  begin
    FColor.ColorTo := ColorDialog1.Color;
    PaintBox1.Repaint;
  end;
end;

procedure TGradientEditor.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
  0:
    begin
      FOldColorTo := FColor.ColorTo;
      FColor.ColorTo := clNone;
    end;
  1:begin
      FColor.Direction := gdVertical;
      FColor.ColorTo := FOldColorTo;
    end;
  2:begin
      FColor.Direction := gdHorizontal;
      FColor.ColorTo := FOldColorTo;
    end;  
  end;
  PaintBox1.Repaint;
end;

end.
