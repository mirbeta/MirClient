{***************************************************************************}
{ TAdvMetroProgressBar component                                            }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2013                                        }
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

unit AdvMetroProgressBar;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, AdvStyleIF, Controls, Graphics
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvMetroProgressBar = class(TGraphicControl, ITMSTones)
  private
    FMax: integer;
    FMin: integer;
    FPosition: integer;
    FColor: TColor;
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure SetColor(const Value: TColor);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetColorTones(ATones: TColorTones);
  published
    property Align;
    property Anchors;
    property Color: TColor read FColor write SetColor default $00F2BC00;
    property Min: integer read FMin write SetMin default 0;
    property Max: integer read FMax write SetMax default 100;
    property Position: integer read FPosition write SetPosition default 50;
    property Visible;
  end;

implementation

{ TAdvMetroProgressBar }

constructor TAdvMetroProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMin := 0;
  FMax := 100;
  FPosition := 50;
  Width := 128;
  Height := 8;
  FColor := $00F2BC00;
end;

procedure TAdvMetroProgressBar.Paint;
var
  delta,diff: integer;

begin
  Canvas.Brush.Color := FColor;
  Canvas.Pen.Color := Canvas.Brush.Color;
  Canvas.Pen.Width := 1;

  diff := FMax - FMin;
  if diff <= 0 then
    diff := 1;

  if Width > Height then
  begin
    delta := Round(FPosition / diff * Width);
    Canvas.Rectangle(0,0,delta,Height);
  end
  else
  begin
    delta := Round(FPosition / diff * Height);
    Canvas.Rectangle(0,0,Width,delta);
  end;

  Canvas.MoveTo(0,Height - 1);
  Canvas.LineTo(Width - 1, Height - 1);
end;

procedure TAdvMetroProgressBar.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TAdvMetroProgressBar.SetColorTones(ATones: TColorTones);
begin
  FColor := ATones.Selected.BrushColor;
end;

procedure TAdvMetroProgressBar.SetMax(const Value: integer);
begin
  if (FMax <> Value) then
  begin
    FMax := Value;
    Invalidate;
  end;
end;

procedure TAdvMetroProgressBar.SetMin(const Value: integer);
begin
  if (FMin <> Value) then
  begin
    FMin := Value;
    Invalidate;
  end;
end;

procedure TAdvMetroProgressBar.SetPosition(const Value: integer);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Invalidate;
  end;
end;

end.
