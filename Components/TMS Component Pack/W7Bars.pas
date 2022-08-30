{***************************************************************************}
{ TMS W7 Controls Pack                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2011                                               } 
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

unit W7Bars;

interface
{$I TMSDEFS.INC}
uses
  Windows, Classes, Controls, W7Graphics, W7Classes;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7InformationBar = class(TW7Control)
  private
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF DELPHI2006_LVL}
     property Padding;
    {$ENDIF}
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7ToolBar = class(TW7Control)
  private
    FShowTopBorder: boolean;
    procedure SetShowTopBorder(Value: boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ShowTopBorder: boolean read FShowTopBorder write SetShowTopBorder;
    {$IFDEF DELPHI2006_LVL}
    property Padding;
    {$ENDIF}
  end;

implementation

constructor TW7InformationBar.Create(AOwner: TComponent);
begin
  inherited;
  Width := 300;
  Height := 53;
  Align := alBottom;
  ControlStyle := ControlStyle + [csAcceptsControls];
  InvalidateOnMouseEvents := False;
end;

destructor TW7InformationBar.Destroy;
begin
  inherited;
end;

procedure TW7InformationBar.Paint;
begin
  Canvas.Brush.Color := $00FBF5F1;
  Canvas.Pen.Color := $00FBF5F1;
  Canvas.Rectangle(0, 0, Width, Height);

  Canvas.Pen.Color := $00FAF4F0;
  Canvas.MoveTo(0, 4);
  Canvas.LineTo(Width, 4);

  Canvas.Pen.Color := $00F9F2ED;
  Canvas.MoveTo(0, 3);
  Canvas.LineTo(Width, 3);

  Canvas.Pen.Color := $00F7EEE8;
  Canvas.MoveTo(0, 2);
  Canvas.LineTo(Width, 2);

  Canvas.Pen.Color := $00F0E3D9;
  Canvas.MoveTo(0, 1);
  Canvas.LineTo(Width, 1);

  Canvas.Pen.Color := $00EAD9CC;
  Canvas.MoveTo(0, 0);
  Canvas.LineTo(Width, 0);
end;

constructor TW7ToolBar.Create(AOwner: TComponent);
begin
  inherited;
  Height := 31;
  Width := 300;
  Align := alTop;
  FShowTopBorder := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  InvalidateOnMouseEvents := False;
end;

destructor TW7ToolBar.Destroy;
begin
  inherited;
end;

procedure TW7ToolBar.SetShowTopBorder(Value: Boolean);
begin
  FShowTopBorder := Value;
  Repaint;
end;

procedure TW7ToolBar.Paint;
begin
  Canvas.Lock;
  DrawGradient(Canvas, $00FDFCFA, $00FAF0E6, Rect(0, 0, Width, Height div 2), True);
  DrawGradient(Canvas, $00F4E6DC, $00F7E9DD, Rect(0, Height div 2, Width, Height), True);

  Canvas.Pen.Color := $00FBEFE4;
  Canvas.MoveTo(0, Height - 3);
  Canvas.LineTo(Width, Height - 3);

  Canvas.Pen.Color := $00EADACD;
  Canvas.MoveTo(0, Height - 2);
  Canvas.LineTo(Width,Height - 2);

  Canvas.Pen.Color := $00C3AFA0;
  Canvas.MoveTo(0, Height - 1);
  Canvas.LineTo(Width, Height - 1);
///
  Canvas.Pen.Color := $00FBEFE4;
  Canvas.MoveTo(0, 0);
  Canvas.LineTo(0, Height - 3);

  Canvas.Pen.Color := $00FBEFE4;
  Canvas.MoveTo(Width - 1, 0);
  Canvas.LineTo(Width - 1, Height - 3);

  if FShowTopBorder then
  begin
    Canvas.Pen.Color := $00EADACD;
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width, 0);
  end;

  Canvas.Unlock;
end;

end.