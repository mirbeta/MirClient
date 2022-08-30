{$I TMSDEFS.INC}

{***********************************************************************}
{ TPlanner component                                                    }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 1999-2013                                      }
{            Email: info@tmssoftware.com                                }
{            Web: http://www.tmssoftware.com                            }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The complete     }
{ source code remains property of the author and may not be distributed,}
{ published, given or sold in any form as such. No parts of the source  }
{ code can be included in any other component or application without    }
{ written authorization of the author.                                  }
{***********************************************************************}

unit PlanDraw;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  Planner, PlanUtil, Types;

type

  TSimpleShape = (ssCircle, ssTriangle, ssHArrow, ssVArrow, ssTriangles, ssCircles, ssSquares);

  TShapeDrawTool = class(TCustomItemDrawTool)
  private
    FShape: TSimpleShape;
    procedure SetShape(const Value: TSimpleShape);
  public
    procedure DrawItem(PlannerItem: TPlannerItem; Canvas: TCanvas; Rect: TRect;
      Selected, Print: Boolean); override;
  published
    property Shape: TSimpleShape read FShape write SetShape;

  end;


implementation


{ TShapeDrawTool }

procedure TShapeDrawTool.DrawItem(PlannerItem: TPlannerItem;
  Canvas: TCanvas; Rect: TRect; Selected, Print: Boolean);
const
  ARW = 16;
  ARH = 16;
var
  m: Integer;
  DRect: TRect;
  
begin

  case FShape of
  ssCircle:
    begin
      Canvas.Brush.Color := PlannerItem.Color;

      if Selected then
      begin
        Canvas.Pen.Color := PlannerItem.TrackColor;
        Canvas.Pen.Width := 2;
        Canvas.Brush.Color := PlannerItem.SelectColor;
      end;

      Canvas.Ellipse(Rect.Left,Rect.Top,Rect.Right,Rect.Bottom);
    end;
  ssTriangle:
    begin
      Canvas.Brush.Color := PlannerItem.Color;
      Canvas.Brush.Style := PlannerItem.BrushStyle;

      if Selected then
      begin
        Canvas.Pen.Color := PlannerItem.TrackColor;
        Canvas.Pen.Width := 2;
        Canvas.Brush.Color := PlannerItem.SelectColor;
        Canvas.Brush.Style := bsSolid;
      end;

      Canvas.PolyGon([Point(Rect.Left,Rect.Bottom),
                      Point(Rect.Right,Rect.Bottom),
                      Point(Rect.Left + (Rect.Right - Rect.Left) div 2,Rect.Top)]);
    end;
  ssHArrow:
    begin
      InflateRect(Rect,-2,-2);
      Canvas.Pen.Color := clGray;
      Canvas.Brush.Color := clGray;
      Canvas.Brush.Style := bsSolid;

      m := Rect.Top + (Rect.Bottom - Rect.Top) div 2;
      Canvas.PolyGon([Point(2 + Rect.Left,2 + m),
                      Point(2 + Rect.Left + ARW,2 + m - ARH * 2),
                      Point(2 + Rect.Left + ARW,2 + m - ARH),
                      Point(2 + Rect.Right - ARW,2 + m - ARH),
                      Point(2 + Rect.Right - ARW,2 + m - ARH * 2),
                      Point(2 + Rect.Right,2 + m),
                      Point(2 + Rect.Right - ARW,2 + m + ARH * 2),
                      Point(2 + Rect.Right - ARW,2 + m + ARH),
                      Point(2 + Rect.Left + ARW,2 + m + ARH),
                      Point(2 + Rect.Left + ARW,2 + m + ARH * 2)]);

      Canvas.Brush.Color := PlannerItem.Color;
      Canvas.Brush.Style := PlannerItem.BrushStyle;

      if Selected then
      begin
        Canvas.Pen.Color := PlannerItem.TrackColor;
        Canvas.Pen.Width := 2;
        Canvas.Brush.Color := PlannerItem.SelectColor;
        Canvas.Brush.Style := bsSolid;
      end;

      m := Rect.Top + (Rect.Bottom - Rect.Top) div 2;
      Canvas.PolyGon([Point(Rect.Left,m),
                      Point(Rect.Left + ARW,m - ARH * 2),
                      Point(Rect.Left + ARW,m - ARH),
                      Point(Rect.Right - ARW,m - ARH),
                      Point(Rect.Right - ARW,m - ARH * 2),
                      Point(Rect.Right,m),
                      Point(Rect.Right - ARW,m + ARH * 2),
                      Point(Rect.Right - ARW,m + ARH),
                      Point(Rect.Left + ARW,m + ARH),
                      Point(Rect.Left + ARW,m + ARH * 2)]);
    end;
  ssVArrow:
    begin
      InflateRect(Rect,-2,-2);
      Canvas.Pen.Color := clGray;
      Canvas.Brush.Color := clGray;
      m := Rect.Left + (Rect.Right - Rect.Left) div 2;
      Canvas.PolyGon([Point(2 + m,2 + Rect.Top),
                      Point(2 + m - ARH * 2,2 + Rect.Top + ARW),
                      Point(2 + m - ARH,2 + Rect.Top + ARW),
                      Point(2 + m - ARH,2 + Rect.Bottom - ARW),
                      Point(2 + m - ARH * 2,2 + Rect.Bottom - ARW),
                      Point(2 + m,2 + Rect.Bottom),
                      Point(2 + m + ARH * 2,2 + Rect.Bottom - ARW),
                      Point(2 + m + ARH,2 + Rect.Bottom - ARW),
                      Point(2 + m + ARH,2 + Rect.Top + ARW),
                      Point(2 + m + ARH * 2,2 + Rect.Top + ARW)]);

      Canvas.Brush.Color := PlannerItem.Color;
      Canvas.Brush.Style := PlannerItem.BrushStyle;

      if Selected then
      begin
        Canvas.Pen.Color := PlannerItem.TrackColor;
        Canvas.Pen.Width := 2;
        Canvas.Brush.Color := PlannerItem.SelectColor;
        Canvas.Brush.Style := bsSolid;
      end;

      m := Rect.Left + (Rect.Right - Rect.Left) div 2;
      Canvas.PolyGon([Point(m,Rect.Top),
                      Point(m - ARH * 2,Rect.Top + ARW),
                      Point(m - ARH,Rect.Top + ARW),
                      Point(m - ARH,Rect.Bottom - ARW),
                      Point(m - ARH * 2,Rect.Bottom - ARW),
                      Point(m,Rect.Bottom),
                      Point(m + ARH * 2,Rect.Bottom - ARW),
                      Point(m + ARH,Rect.Bottom - ARW),
                      Point(m + ARH,Rect.Top + ARW),
                      Point(m + ARH * 2,Rect.Top + ARW)]);
    end;
  ssTriangles:
    begin
      InflateRect(Rect,-4,-4);

      m := Rect.Top + (Rect.Bottom - Rect.Top) div 2;

      m := m + ARH div 2;

      Canvas.Pen.Width := 2;
      Canvas.Pen.Color := clBlack;
      Canvas.MoveTo(Rect.Left + ARW,m - ARH div 2);
      Canvas.LineTo(Rect.Right - ARW,m - ARH div 2);

      Canvas.Brush.Color := clGray;
      Canvas.Pen.Color := clGray;
      Canvas.Brush.Style := bsSolid;

      Canvas.Polygon([Point(2 + Rect.Left,2 + m),
                      Point(2 + Rect.Left + ARW * 2,2 + m),
                      Point(2 + Rect.Left + ARW,2 + m - ARH)]);

      Canvas.Polygon([Point(2 + Rect.Right,2 + m - ARH),
                      Point(2 + Rect.Right - ARW * 2,2 + m - ARH),
                      Point(2 + Rect.Right - ARW,2 + m)]);

      Canvas.Brush.Color := PlannerItem.Color;
      Canvas.Pen.Color := Canvas.Brush.Color;
      Canvas.Pen.Width := 1;
      Canvas.Brush.Style := PlannerItem.BrushStyle;

      if Selected then
      begin
        Canvas.Pen.Color := PlannerItem.TrackColor;
        Canvas.Brush.Color := PlannerItem.SelectColor;
        Canvas.Pen.Width := 2;
        Canvas.Brush.Style := bsSolid;
      end;

      Canvas.Polygon([Point(Rect.Left,m),
                      Point(Rect.Left + ARW * 2,m),
                      Point(Rect.Left + ARW,m - ARH)]);

      Canvas.Polygon([Point(Rect.Right,m - ARH),
                      Point(Rect.Right - ARW * 2,m - ARH),
                      Point(Rect.Right - ARW,m)]);

    end;
  ssCircles:
    begin
      InflateRect(Rect,-4,-4);

      m := Rect.Top + (Rect.Bottom - Rect.Top) div 2;

      Canvas.Pen.Width := 2;
      Canvas.Pen.Color := clBlack;
      Canvas.MoveTo(Rect.Left + ARW,m);
      Canvas.LineTo(Rect.Right - ARW ,m);

      m := m - ARW;



      Canvas.Pen.Color := clGray;
      Canvas.Brush.Color := clGray;
      DRect := Classes.Rect(2 + Rect.Left,2 + m,2 + Rect.Left + ARW * 2,2 + m + ARW * 2);

      Canvas.Ellipse(DRect.Left, DRect.Top, DRect.Right, DRect.Bottom);

      DRect := Classes.Rect(2 + Rect.Right - ARW * 2,2 + m,2 + Rect.Right,2 + m + ARW * 2);

      Canvas.Ellipse(DRect.Left, DRect.Top, DRect.Right, DRect.Bottom);

      Canvas.Brush.Color := PlannerItem.Color;
      Canvas.Pen.Color := Canvas.Brush.Color;
      Canvas.Pen.Width := 1;
      Canvas.Brush.Style := PlannerItem.BrushStyle;

      if Selected then
      begin
        Canvas.Pen.Color := PlannerItem.TrackColor;
        Canvas.Brush.Color := PlannerItem.SelectColor;
        Canvas.Pen.Width := 2;
        Canvas.Brush.Style := bsSolid;
      end;

      DRect := Classes.Rect(Rect.Left,m,Rect.Left + ARW * 2,m + ARW * 2);
      Canvas.Ellipse(DRect.Left, DRect.Top, DRect.Right, DRect.Bottom);

      DRect := Classes.Rect(Rect.Right - ARW * 2,m,Rect.Right,m + ARW * 2);
      Canvas.Ellipse(DRect.Left, DRect.Top, DRect.Right, DRect.Bottom);
    end;
  ssSquares:
    begin

    end;

  end;

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(PlannerItem.Font);
  Canvas.TextRect(Rect,Rect.Left + 2,Rect.Top + 2,PlannerItem.GetCaptionString);
end;

procedure TShapeDrawTool.SetShape(const Value: TSimpleShape);
begin
  FShape := Value;
end;

end.
