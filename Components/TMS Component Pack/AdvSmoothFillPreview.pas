{**************************************************************************}
{ TAdvSmoothFillPreview component                                          }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2009                                                       }
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
unit AdvSmoothFillPreview;

interface

{$I TMSDEFS.INC}

uses
  Windows, SysUtils, Classes, Graphics, Controls, Math, GDIPFill,
  AdvGDIP
  ;

type
  TAdvSmoothFillPreview = class(TCustomControl)
  private
    FColor: TColor;
    FFill: TGDIPFill;
    FCellSize: integer;
    procedure SetCellSize(const Value: integer);
    procedure SetFill(const Value: TGDIPFill);
  protected
    procedure FillChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    property Fill: TGDIPFill read FFill write SetFill;    
  published
    property CellSize: integer read FCellSize write SetCellSize default 15;
  end;


implementation


{ TAdvSmoothFillPreview }

procedure TAdvSmoothFillPreview.Assign(Source: TPersistent);
begin
  inherited;
  Fill.Assign((Source as TAdvSmoothFillPreview).Fill);
  CellSize := (Source as TAdvSmoothFillPreview).CellSize;
end;

constructor TAdvSmoothFillPreview.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := true;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  Width := 200;
  Height := 100;
  FCellSize := 15;
end;

destructor TAdvSmoothFillPreview.Destroy;
begin
  FFill.Free;
  inherited;
end;

procedure TAdvSmoothFillPreview.FillChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvSmoothFillPreview.Paint;
var
  g: TGPGraphics;
  c, r, rows, cols: integer;
  b: TGPSolidBrush;
  rt, fillr: TGPRectF;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  rows := Round(Height / CellSize);
  cols := Round(Width / CellSize);

  FColor := clwhite;

  if Odd(rows) then
    Inc(Rows);

  for c := 0 to cols do
  begin
    for r := 0 to rows do
    begin
      rt := MakeRect(c * cellsize, r * cellsize, cellsize, cellsize);
      
      if FColor = clWhite then
        FColor := clGray
      else
        Fcolor := clWhite;

      b := TGPSolidBrush.Create(MakeColor(255, FColor));
      g.FillRectangle(b, rt);
      b.Free;
    end;
  end;

  if Assigned(FFill) then
  begin
    //fill preview
    if (Fill.BorderWidth > 0) and (Fill.BorderColor <> clNone) then    
      fillr := MakeRect(0, 0, ClientWidth - 1, ClientRect.Bottom - 1)
    else
      fillr := MakeRect(0, 0, ClientWidth, ClientHeight);    

    fill.Fill(g, fillr);
  end;
  g.Free;
end;

procedure TAdvSmoothFillPreview.SetCellSize(const Value: integer);
begin
  if FCellSize <> value then
  begin
    FCellSize := Max(1, Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothFillPreview.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill := Value;
    FillChanged(self);
  end;
end;

end.
