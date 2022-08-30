{**************************************************************************}
{ TGradientStyle                                                           }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright © 2001 - 2012                                                  }
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

unit AdvGradient;

{$I TMSDEFS.INC}

interface

uses
  Classes, Graphics, Windows, AdvStyleIF, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;


type
  TGradientStyle = class(TPersistent)
  private
    FColorTo: TColor;
    FColorFrom: TColor;
    FBorderColor: TColor;
    FOnChange: TNotifyEvent;
    FDirection: Boolean;
    FColorMirrorFrom: TColor;
    FColorMirrorTo: TColor;
    FRounded: Boolean;
    procedure SetBorderColor(const Value: TColor);
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetColorMirrorFrom(const Value: TColor);
    procedure SetColorMirrorTo(const Value: TColor);
    procedure SetDirection(const Value: Boolean);
    procedure SetRounded(const Value: Boolean);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(Canvas: TCanvas; Rect: TRect);
    procedure SetStyle(AStyle: TTMSStyle);
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor default clHighLight;
    property ColorFrom: TColor read FColorFrom write SetColorFrom default clHighlight;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property ColorMirrorFrom: TColor read FColorMirrorFrom write SetColorMirrorFrom default clNone;
    property ColorMirrorTo: TColor read FColorMirrorTo write SetColorMirrorTo default clNone;
    property Direction: Boolean read FDirection write SetDirection default False;
    property Rounded: Boolean read FRounded write SetRounded default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGradientDirection = (gdVertical, gdHorizontal);
  TGradientPart = (gpFull, gpLeft, gpRight, gpMiddle);

procedure DrawGradient(Canvas: TCanvas; FromColor,ToColor: TColor; Steps: Integer;R:TRect; Direction: Boolean);
procedure DrawVistaGradient(ACanvas: TCanvas; ARect: TRect; ColorFrom, ColorTo, ColorMirrorFrom, ColorMirrorTo: TColor;
  Direction: TGradientDirection; BorderColor: TColor; Fill: Boolean = True);
procedure DrawSelectionGradient(canvas: TCanvas; color1,color2,mircolor1,mircolor2,bordercolor,edgecolor,bkgcolor: TColor; r: TRect; part: TGradientPart);


implementation

{ TGradientStyle }

procedure TGradientStyle.Assign(Source: TPersistent);
begin
  if not (Source is TGradientStyle) then
    Exit;

  FBorderColor := (Source as TGradientStyle).BorderColor;
  FColorTo := (Source as TGradientStyle).ColorTo;
  FColorFrom := (Source as TGradientStyle).ColorFrom;
  FColorMirrorTo := (Source as TGradientStyle).ColorMirrorTo;
  FColorMirrorFrom := (Source as TGradientStyle).ColorMirrorFrom;
  Changed;
end;

procedure TGradientStyle.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TGradientStyle.Create;
begin

  FColorFrom := clHighLight;
  FColorTo := clNone;
  FColorMirrorFrom := clNone;
  FColorMirrorTo := clNone;
  FBorderColor := clHighLight;
end;

procedure TGradientStyle.Draw(Canvas: TCanvas; Rect: TRect);
begin
  if ColorTo <> clNone then
  begin
    if Rounded then
      DrawSelectionGradient(Canvas, ColorFrom, ColorTo, ColorMirrorFrom, ColorMirrorTo, BorderColor, BorderColor, clWindow, Rect, gpFull)
    else
      DrawVistaGradient(Canvas, Rect, ColorFrom, FColorTo, ColorMirrorFrom, ColorMirrorTo, gdVertical, BorderColor)
  end
  else
  begin
    Canvas.Brush.Color := ColorFrom;
    Canvas.Pen.Color := ColorFrom;
    Canvas.Rectangle(rect.Left,rect.Top,rect.Right,rect.Bottom);
  end;
end;

procedure TGradientStyle.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TGradientStyle.SetColorFrom(const Value: TColor);
begin
  if (FColorFrom <> Value) then
  begin
    FColorFrom := Value;
    Changed;
  end;
end;

procedure TGradientStyle.SetColorMirrorFrom(const Value: TColor);
begin
  if (FColorMirrorFrom <> Value) then
  begin
    FColorMirrorFrom := Value;
    Changed;
  end;
end;

procedure TGradientStyle.SetColorMirrorTo(const Value: TColor);
begin
  if (FColorMirrorTo <> Value) then
  begin
    FColorMirrorTo := Value;
    Changed;
  end;
end;

procedure TGradientStyle.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;


procedure TGradientStyle.SetDirection(const Value: Boolean);
begin
  if (FDirection <> Value) then
  begin
    FDirection := Value;
    Changed;
  end;
end;

procedure TGradientStyle.SetRounded(const Value: Boolean);
begin
  if (FRounded <> Value) then
  begin
    FRounded := Value;
    Changed;
  end;
end;

procedure TGradientStyle.SetStyle(AStyle: TTMSStyle);
begin
  Rounded := false;
  case AStyle of
   tsOffice2016Black:
      begin
        BorderColor := $6A6A6A;
        ColorFrom := $6A6A6A;
        ColorTo := $6A6A6A;
        ColorMirrorFrom := $6A6A6A;
        ColorMirrorTo := $6A6A6A;
      end;
   tsOffice2016Gray:
      begin
        BorderColor := $F2E1D5;
        ColorFrom := $F2E1D5;
        ColorTo := $F2E1D5;
        ColorMirrorFrom := $F2E1D5;
        ColorMirrorTo := $F2E1D5;
      end;
   tsOffice2016White:
      begin
        BorderColor := $F2E1D5;
        ColorFrom := $F2E1D5;
        ColorTo := $F2E1D5;
        ColorMirrorFrom := $F2E1D5;
        ColorMirrorTo := $F2E1D5;
      end;
   tsWindows8, tsWindows10:
      begin
        BorderColor := $F9CEA4;
        ColorFrom := $F7EFE8;
        ColorTo := $F7EFE8;
        ColorMirrorFrom := $F7EFE8;
        ColorMirrorTo := $F7EFE8;
      end;
    tsOffice2010Black:
      begin
        Rounded := true;
        BorderColor := $308AC2;
        ColorFrom := $7BEEFF;
        ColorTo := $6CD0FF;
        ColorMirrorFrom := $6CD0FF;
        ColorMirrorTo := $7BEEFF;
      end;
        tsOffice2010Silver:
      begin
        Rounded := true;
        BorderColor := $308AC2;
        ColorFrom := $7BEEFF;
        ColorTo := $6CD0FF;
        ColorMirrorFrom := $6CD0FF;
        ColorMirrorTo := $7BEEFF;
      end;
        tsOffice2010Blue:
      begin
       Rounded := true;
        BorderColor := $308AC2;
        ColorFrom := $7BEEFF;
        ColorTo := $6CD0FF;
        ColorMirrorFrom := $6CD0FF;
        ColorMirrorTo := $7BEEFF;
      end;
    tsTerminal:
      begin
        BorderColor := clGray;
        ColorFrom := clBtnFace;
        ColorTo := clBtnFace;
        ColorMirrorFrom := clNone;
        ColorMirrorTo := clNone;
      end;
    tsWindows7:
      begin
        Rounded := true;
        BorderColor := $CEA27D;
        ColorFrom := $FCEBDC;  // #FFEEC2
        ColorTo := $FCDBC1;
        ColorMirrorFrom := clNone;
        ColorMirrorTo := clNone;
      end;
    tsWindowsVista:
      begin
        Rounded := true;    
        BorderColor := $FDDE99;
        ColorFrom := $FDF8F1;  // #FFEEC2
        ColorTo := $FCEFD5;
        ColorMirrorFrom := clNone;
        ColorMirrorTo := clNone;
      end;
    tsOffice2003Blue, tsOffice2003Silver, tsOffice2003Olive, tsOffice2003Classic:
      begin
        BorderColor := clNone;
        ColorFrom := $C2EEFF;  // #FFEEC2
        ColorTo := clNone;
        ColorMirrorFrom := clNone;
        ColorMirrorTo := clNone;
      end;
    tsOffice2007Luna, tsOffice2007Obsidian, tsOffice2007Silver:
      begin
        Rounded := true;
        BorderColor := clSilver;
        ColorFrom := $EBFDFF;
        ColorTo := $ABEBFF;
        ColorMirrorFrom := $69D6FF;
        ColorMirrorTo := $96E4FF;
      end;
    tsWindowsXP, tsWhidbey:
      begin
        BorderColor := clNone;
        ColorFrom := clHighlight;
        ColorTo := clNone;
        ColorMirrorFrom := clNone;
        ColorMirrorTo := clNone;
      end;
    tsCustom: ;
  end;
end;

procedure DrawGradient(Canvas: TCanvas; FromColor,ToColor: TColor; Steps: Integer;R:TRect; Direction: Boolean);
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
  end;
end;

// Draw gradient in the specified rectangle (if Fill = True and ColorFrom <> clNone),
// frame it with BorderColor color.
procedure DrawVistaGradient(ACanvas: TCanvas; ARect: TRect; ColorFrom, ColorTo, ColorMirrorFrom, ColorMirrorTo: TColor;
  Direction: TGradientDirection; BorderColor: TColor; Fill: Boolean = True);
var
  r: Trect;

begin
  if Fill and (ColorFrom <> clNone) then
  begin
    if ColorMirrorFrom <> clNone then
    begin
      r := ARect;

      if Direction = gdHorizontal then
      begin
        r.Right := r.Left + ((r.Right - r.Left) div 2);
        DrawGradient(ACanvas,  ColorFrom, ColorTo, 128, r, Direction = gdVertical);
        r := ARect;
        r.Left := r.Left + ((r.Right - r.Left) div 2);
        DrawGradient(ACanvas,  ColorMirrorFrom, ColorMirrorTo, 128, r, Direction = gdVertical);
      end
      else
      begin
        r.Bottom := r.Top + ((r.Bottom - r.Top) div 2);
        DrawGradient(ACanvas,  ColorFrom, ColorTo, 128, r, Direction = gdHorizontal);
        r := ARect;
        r.Top := r.Top + ((r.Bottom - r.Top) div 2);
        DrawGradient(ACanvas,  ColorMirrorFrom, ColorMirrorTo, 128, r, Direction = gdHorizontal);
      end;
    end
    else
      DrawGradient(ACanvas, ColorFrom, ColorTo, 128, ARect, Direction = gdHorizontal);
  end;

  if BorderColor <> clNone then
  begin
    ACanvas.Brush.Color := BorderColor;
    ACanvas.FrameRect(ARect);
  end;
end;


procedure DrawSelectionGradient(canvas: TCanvas; color1,color2,mircolor1,mircolor2,bordercolor,edgecolor,bkgcolor: TColor; r: TRect; part: TGradientPart);
var
  dl,dr: integer;
begin
//  r.Bottom := r.Bottom - 1;

  r := Rect(r.Left, r.Top, r.Right, r.Bottom - 1);

  if (color2 = clNone) then
  begin
    Canvas.Brush.Color := color1;
    Canvas.Pen.Color := color1;
    Canvas.FillRect(r);
  end
  else
  begin
    if mircolor1 <> clNone then
    begin
      DrawGradient(Canvas,color1,color2,16,Rect(r.Left, r.Top, r.Right, r.Top + (r.Bottom - r.Top) div 2), false);
      DrawGradient(Canvas,mircolor1,mircolor2,16,Rect(r.Left, r.Top + (r.Bottom - r.Top) div 2, r.Right, r.Bottom), false);
    end
    else
      DrawGradient(Canvas,color1,color2,16,r, false);
  end;

  dl := 2;
  dr := 2;

  Canvas.Pen.Color := bordercolor;
  Canvas.MoveTo(r.left + dl,r.top);
  Canvas.LineTo(r.right - dr,r.top);

  Canvas.MoveTo(r.left + dl,r.bottom);
  Canvas.LineTo(r.right - dr,r.bottom);

  Canvas.MoveTo(r.right - 1,r.top + 2);
  Canvas.LineTo(r.right - 1,r.bottom - 1);
  Canvas.MoveTo(r.left,r.top + 2);
  Canvas.LineTo(r.left,r.bottom - 1);

  if (part in [gpFull, gpLeft]) then
  begin
    Canvas.Pixels[r.Left + 1,r.Top] := edgecolor;
    Canvas.Pixels[r.Left + 1,r.Top + 1] := edgecolor;
    Canvas.Pixels[r.Left,r.Top + 1] := edgecolor;
    Canvas.Pixels[r.Left + 1,r.Bottom] := edgecolor;
    Canvas.Pixels[r.Left + 1,r.Bottom - 1] := edgecolor;
    Canvas.Pixels[r.Left,r.Bottom - 1] := edgecolor;

    Canvas.Pixels[r.Left,r.Top] := bkgcolor;
    Canvas.Pixels[r.Left,r.Bottom] := bkgcolor;
  end;

  if (part in [gpFull, gpRight]) then
  begin
    Canvas.Pixels[r.right - 2,r.top] := edgecolor;
    Canvas.Pixels[r.right - 2,r.top + 1] := edgecolor;
    Canvas.Pixels[r.right - 1,r.top + 1] := edgecolor;
    Canvas.Pixels[r.right - 2,r.bottom] := edgecolor;
    Canvas.Pixels[r.right - 2,r.bottom - 1] := edgecolor;
    Canvas.Pixels[r.right - 1,r.bottom - 1] := edgecolor;

    Canvas.Pixels[r.Right - 1,r.Top] := bkgcolor;
    Canvas.Pixels[r.Right - 1,r.Bottom] := bkgcolor;
  end;

end;




end.
