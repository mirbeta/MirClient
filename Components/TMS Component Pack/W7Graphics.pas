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

unit W7Graphics;

interface

{$I TMSDEFS.INC}
{$DEFINE USE_SCANLINE}

uses
  Windows, Graphics, Classes, SysUtils, Math;

type

  {$EXTERNALSYM COLOR16}
  COLOR16 = Word;

  PTriVertex = ^TTriVertex;
  {$EXTERNALSYM _TRIVERTEX}
  _TRIVERTEX = packed record
    x: Longint;
    y: Longint;
    Red: COLOR16;
    Green: COLOR16;
    Blue: COLOR16;
    Alpha: COLOR16;
  end;
  TTriVertex = _TRIVERTEX;
  {$EXTERNALSYM TRIVERTEX}
  TRIVERTEX = _TRIVERTEX;

  TW7ArrowType = (atDown, atUp, atLeft, atRight);
  TFilterProc = function(Value: Single): Single;
  TContributor = record
    pixel: integer;		// Source pixel
    weight: single;		// Pixel weight
  end;


procedure DrawW7Arrow(Canvas: TCanvas; Color: TColor; X: integer; Y: integer; ArrowType: TW7ArrowType);
procedure DrawDownArrow(Canvas: TCanvas; Color: TColor; X: integer; Y: integer);
procedure DrawUpArrow(Canvas: TCanvas; Color: TColor; X: integer; Y: integer);
procedure DrawLeftArrow(Canvas: TCanvas; Color: TColor; X: integer; Y: integer);
procedure DrawRightArrow(Canvas: TCanvas; Color: TColor; X: integer; Y: integer);
function AlphaBlendPixel(Pixel: TColor; BackgroundPixel: TColor; Alpha: Byte): TColor;
procedure AlphaBlendBitmap(Source: TBitmap; Destination: TCanvas; DestRect: TRect; Opacity: Byte; Ignore32Bit: boolean = false); overload;
procedure AlphaBlendBitmap(Source: TBitmap; SourceRect: TRect; Destination: TCanvas; DestRect: TRect; Opacity: Byte; Ignore32Bit: boolean = false); overload;
procedure DrawGradient(ACanvas: TCanvas; AStartColor, AEndColor: TColor; ARect: TRect; Vertical: boolean);
procedure DrawEllipticGradient(Canvas: TCanvas; StartColor: TColor; EndColor: TColor; DstRect: TRect);
procedure DrawHorizontalGradient(Canvas: TCanvas; StartColor: TColor; EndColor: TColor; DstRect: TRect);
procedure DrawVerticalGradient(Canvas: TCanvas; StartColor: TColor; EndColor: TColor; DstRect: TRect);
procedure DrawAlphaGradient(Canvas: TCanvas; Source: TBitmap; StartValue: Byte; EndValue: Byte; DstRect: TRect; Vertical: boolean);

{$EXTERNALSYM GradientFill}
function GradientFill(DC: HDC; Vertex: PTriVertex; NumVertex: ULONG; Mesh: Pointer; NumMesh, Mode: ULONG): BOOL; stdcall;

implementation

function GradientFill; external msimg32 name 'GradientFill';

procedure DrawW7Arrow(Canvas: TCanvas; Color: TColor; X: integer; Y: integer; ArrowType: TW7ArrowType);
begin
  case ArrowType of
    atDown: DrawDownArrow(Canvas, Color, X, Y);
    atUp: DrawUpArrow(Canvas, Color, X, Y);
    atLeft: DrawLeftArrow(Canvas, Color, X, Y);
    atRight: DrawRightArrow(Canvas, Color, X, Y);
  end;
end;

procedure DrawDownArrow(Canvas: TCanvas; Color: TColor; X: integer; Y: integer);
var
  Ind: integer;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := Color;
  Canvas.MoveTo(X + 1, Y + 0);
  Canvas.LineTo(X + 8, Y + 0);
  Canvas.MoveTo(X + 6, Y + 1);
  Canvas.LineTo(X + 4, Y + 3);
  Canvas.LineTo(X + 1, Y + 0);
  Canvas.Pixels[X + 3, Y + 1] := Color;
  Canvas.Pixels[X + 4, Y + 1] := Color;
  Canvas.Pixels[X + 5, Y + 1] := Color;
  Canvas.Pixels[X + 4, Y + 2] := Color;
  for Ind := 0 to 4 do
  begin
    Canvas.Pixels[X + Ind, Y + Ind] := AlphaBlendPixel(Color, Canvas.Pixels[X + Ind, Y + Ind], 222);
    Canvas.Pixels[X + 8 - Ind, Y + Ind] := AlphaBlendPixel(Color, Canvas.Pixels[X + 8 - Ind, Y + Ind], 222);
  end;
end;

procedure DrawUpArrow(Canvas: TCanvas; Color: TColor; X: integer; Y: integer);
var
  Ind: integer;
  DY: integer;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := Color;
  DY := Y + 4;
  Canvas.MoveTo(X + 1, DY - 0);
  Canvas.LineTo(X + 8, DY - 0);
  Canvas.MoveTo(X + 6, DY - 1);
  Canvas.LineTo(X + 4, DY - 3);
  Canvas.LineTo(X + 1, DY - 0);
  Canvas.Pixels[X + 3, DY - 1] := Color;
  Canvas.Pixels[X + 4, DY - 1] := Color;
  Canvas.Pixels[X + 5, DY - 1] := Color;
  Canvas.Pixels[X + 4, DY - 2] := Color;
  for Ind := 0 to 4 do
  begin
    Canvas.Pixels[X + Ind, DY - Ind] := AlphaBlendPixel(Color, Canvas.Pixels[X + Ind, DY - Ind], 222);
    Canvas.Pixels[X + 8 - Ind, DY - Ind] := AlphaBlendPixel(Color, Canvas.Pixels[X + 8 - Ind, DY - Ind], 222);
  end;
end;

procedure DrawLeftArrow(Canvas: TCanvas; Color: TColor; X: integer; Y: integer);
var
  Ind: integer;
  DX: integer;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := Color;
  DX := X + 4;
  Canvas.MoveTo(DX - 0, Y + 1);
  Canvas.LineTo(DX - 0, Y + 8);
  Canvas.MoveTo(DX - 1, Y + 6);
  Canvas.LineTo(DX - 3, Y + 4);
  Canvas.LineTo(DX - 0, Y + 1);
  Canvas.Pixels[DX - 1, Y + 3] := Color;
  Canvas.Pixels[DX - 1, Y + 4] := Color;
  Canvas.Pixels[DX - 1, Y + 5] := Color;
  Canvas.Pixels[DX - 2, Y + 4] := Color;
  for Ind := 0 to 4 do
  begin
    Canvas.Pixels[DX - Ind, Y + Ind] := AlphaBlendPixel(Color, Canvas.Pixels[DX - Ind, Y + Ind], 222);
    Canvas.Pixels[DX - Ind, Y - Ind + 8] := AlphaBlendPixel(Color, Canvas.Pixels[DX - Ind, Y - Ind + 8], 222);
  end;
end;

procedure DrawRightArrow(Canvas: TCanvas; Color: TColor; X: integer; Y: integer);
var
  Ind: integer;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := Color;
  Canvas.MoveTo(X + 0, Y + 1);
  Canvas.LineTo(X + 0, Y + 8);
  Canvas.MoveTo(X + 1, Y + 6);
  Canvas.LineTo(X + 3, Y + 4);
  Canvas.LineTo(X + 0, Y + 1);
  Canvas.Pixels[X + 1, Y + 3] := Color;
  Canvas.Pixels[X + 1, Y + 4] := Color;
  Canvas.Pixels[X + 1, Y + 5] := Color;
  Canvas.Pixels[X + 2, Y + 4] := Color;
  for Ind := 0 to 4 do
  begin
    Canvas.Pixels[X + Ind, Y + Ind] := AlphaBlendPixel(Color, Canvas.Pixels[X + Ind, Y + Ind], 222);
    Canvas.Pixels[X + Ind, Y - Ind + 8] := AlphaBlendPixel(Color, Canvas.Pixels[X + Ind, Y - Ind + 8], 222);
  end;
end;


function AlphaBlendPixel(Pixel: TColor; BackgroundPixel: TColor; Alpha: Byte): TColor;
var
  DstRed: byte;
  DstGreen: byte;
  DstBlue: byte;
begin
  DstRed := Round(GetRValue(BackgroundPixel) * (Alpha/255.0) + GetRValue(Pixel) * (1.0 - (Alpha/255.0)));
  DstGreen := Round(GetGValue(BackgroundPixel) * (Alpha/255.0) + GetGValue(Pixel) * (1.0 - (Alpha/255.0)));
  DstBlue := Round(GetBValue(BackgroundPixel) * (Alpha/255.0) + GetBValue(Pixel) * (1.0 - (Alpha/255.0)));
  Result := RGB(DstRed, DstGreen, DstBlue);
end;

procedure AlphaBlendBitmap(Source: TBitmap; Destination: TCanvas; DestRect: TRect; Opacity: Byte; Ignore32Bit: boolean = false);
var
  BlendFunc: TBlendFunction;
begin
  BlendFunc.BlendOp := AC_SRC_OVER;
  BlendFunc.BlendFlags := 0;
  BlendFunc.SourceConstantAlpha := Opacity;
  if (Source.PixelFormat = pf32bit) and (not Ignore32Bit) then
    BlendFunc.AlphaFormat := AC_SRC_ALPHA
  else
    BlendFunc.AlphaFormat := 0;
  AlphaBlend(Destination.Handle, DestRect.Left, DestRect.Top,
    DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
    Source.Canvas.Handle, 0, 0, Source.Width, Source.Height, BlendFunc);
end;

procedure AlphaBlendBitmap(Source: TBitmap; SourceRect: TRect; Destination: TCanvas; DestRect: TRect; Opacity: Byte; Ignore32Bit: boolean = false); overload;
var
  BlendFunc: TBlendFunction;
begin
  BlendFunc.BlendOp := AC_SRC_OVER;
  BlendFunc.BlendFlags := 0;
  BlendFunc.SourceConstantAlpha := Opacity;

  if (Source.PixelFormat = pf32bit) and (not Ignore32Bit) then
    BlendFunc.AlphaFormat := AC_SRC_ALPHA
  else
    BlendFunc.AlphaFormat := 0;

  AlphaBlend(Destination.Handle, DestRect.Left, DestRect.Top,
    DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
    Source.Canvas.Handle, SourceRect.Left, SourceRect.Top,
    SourceRect.Right - SourceRect.Left, SourceRect.Bottom - SourceRect.Top, BlendFunc);
end;

procedure DrawGradient(ACanvas: TCanvas; AStartColor, AEndColor: TColor; ARect: TRect; Vertical: boolean);
var
  StartColor, EndColor: Cardinal;
  GradientRect: TGradientRect;
  Vertexes: array [0..1] of TTriVertex;
begin
  StartColor := ColorToRGB(AStartColor);
  EndColor := ColorToRGB(AEndColor);

  Vertexes[0].x := ARect.Left;
  Vertexes[0].y := ARect.Top;
  Vertexes[0].Red := GetRValue(StartColor) shl 8;
  Vertexes[0].Blue := GetBValue(StartColor) shl 8;
  Vertexes[0].Green := GetGValue(StartColor) shl 8;
  Vertexes[0].Alpha := 0;

  Vertexes[1].x := ARect.Right;
  Vertexes[1].y := ARect.Bottom;
  Vertexes[1].Red := GetRValue(EndColor) shl 8;
  Vertexes[1].Blue := GetBValue(EndColor) shl 8;
  Vertexes[1].Green := GetGValue(EndColor) shl 8;
  Vertexes[1].Alpha := 0;

  GradientRect.UpperLeft := 0;
  GradientRect.LowerRight := 1;
  {$IFDEF DELPHI6_LVL}
  GradientFill(ACanvas.Handle, @Vertexes[0], 2, @GradientRect, 1, integer(Vertical));
  {$ELSE}
  if Vertical then
    DrawVerticalGradient(ACanvas, StartColor, EndColor, ARect)
  else
    DrawHorizontalGradient(ACanvas, StartColor, EndColor, ARect);
  {$ENDIF}
end;

procedure DrawEllipticGradient(Canvas: TCanvas; StartColor: TColor; EndColor: TColor; DstRect: TRect);
var
  StartRed, StartGreen, StartBlue : Integer;
  DeltaRed, DeltaGreen, DeltaBlue : Integer;
  Ind: integer;
  Bmp:TBitmap;
  R,G,B:Byte;
  pW, pH : Real;
  X1, Y1, X2, Y2: Real;
begin
  Bmp := TBitmap.Create;
  Bmp.Width := DstRect.Right - DstRect.Left;
  Bmp.Height := DstRect.Bottom - DstRect.Top;
  StartRed := GetRValue(StartColor);
  StartGreen := GetGValue(StartColor);
  StartBlue := GetBValue(StartColor);
  DeltaRed := GetRValue(EndColor)-StartRed;
  DeltaGreen := GetGValue(EndColor)-StartGreen;
  DeltaBlue := GetBValue(EndColor)-StartBlue;
  Bmp.Canvas.Pen.Style := psClear;
  Bmp.Canvas.Pen.Mode := pmCopy;
  X1 := 0 - (Bmp.Width / 4);
  X2 := Bmp.Width + (Bmp.Width / 4)+4;
  Y1 := 0 - (Bmp.Height / 4);
  Y2 := Bmp.Height + (Bmp.Height / 4)+4;
  pW := ((Bmp.Width / 4) + (Bmp.Width / 2)) / 155;
  pH := ((Bmp.Height / 4) + (Bmp.Height / 2)) / 155;
  for Ind := 0 to 155 do
  begin
    X1 := X1 + pW;
    X2 := X2 - pW;
    Y1 := Y1 + pH;
    Y2 := Y2 - pH;
    R := StartRed + MulDiv(Ind, DeltaRed, 155);
    G := StartGreen + MulDiv(Ind, DeltaGreen, 155);
    B := StartBlue + MulDiv(Ind, DeltaBlue, 155);
    Bmp.Canvas.Brush.Color := R or (G shl 8) or (B shl 16);
    Bmp.Canvas.Ellipse(Trunc(X1), Trunc(Y1), Trunc(X2), Trunc(Y2));
  end;
  BitBlt(Canvas.Handle, DstRect.Left, DstRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
  Bmp.Destroy;
end;

procedure DrawHorizontalGradient(Canvas: TCanvas; StartColor: TColor; EndColor: TColor; DstRect: TRect);
var
  StartRed, StartGreen, StartBlue : Integer;
  DeltaRed, DeltaGreen, DeltaBlue : Integer;
  ColorRect:TRect;
  Ind: integer;
  Bmp: TBitmap;
  R,G,B:Byte;
begin
  Bmp := TBitmap.Create;
  Bmp.Width := DstRect.Right - DstRect.Left;
  Bmp.Height := DstRect.Bottom - DstRect.Top;
  StartRed := GetRValue(StartColor);
  StartGreen := GetGValue(StartColor);
  StartBlue := GetBValue(StartColor);
  DeltaRed := GetRValue(EndColor)-StartRed;
  DeltaGreen := GetGValue(EndColor)-StartGreen;
  DeltaBlue := GetBValue(EndColor)-StartBlue;

  ColorRect.Top:= 0;                //Set rectangle top
  ColorRect.Bottom := Bmp.Height;
  for Ind := 0 to 255 do
  begin
    ColorRect.Left:= MulDiv (Ind, Bmp.Width, 256);
    ColorRect.Right:= MulDiv (Ind + 1, Bmp.Width, 256);
    R := StartRed + MulDiv(Ind, DeltaRed, 255);
    G := StartGreen + MulDiv(Ind, DeltaGreen, 255);
    B := StartBlue + MulDiv(Ind, DeltaBlue, 255);
    Bmp.Canvas.Brush.Color := RGB(R, G, B);
    Bmp.Canvas.FillRect(ColorRect);
  end;
  BitBlt(Canvas.Handle, DstRect.Left, DstRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
  Bmp.Destroy;
end;

procedure DrawVerticalGradient(Canvas: TCanvas; StartColor: TColor; EndColor: TColor; DstRect: TRect);
var
  StartRed, StartGreen, StartBlue : Integer;
  DeltaRed, DeltaGreen, DeltaBlue : Integer;
  ColorRect:TRect;
  Ind: integer;
  Bmp: TBitmap;
  R,G,B:Byte;
begin
  Bmp := TBitmap.Create;
  Bmp.Width := DstRect.Right - DstRect.Left;
  Bmp.Height := DstRect.Bottom - DstRect.Top;
  StartRed := GetRValue(StartColor);
  StartGreen := GetGValue(StartColor);
  StartBlue := GetBValue(StartColor);
  DeltaRed := GetRValue(EndColor)-StartRed;
  DeltaGreen := GetGValue(EndColor)-StartGreen;
  DeltaBlue := GetBValue(EndColor)-StartBlue;

  ColorRect.Left:= 0;
  ColorRect.Right := Bmp.Width;
  for Ind := 0 to 255 do
  begin
    ColorRect.Top:= MulDiv (Ind, Bmp.Height, 256);
    ColorRect.Bottom:= MulDiv (Ind + 1, Bmp.Height, 256);
    R := StartRed + MulDiv(Ind, DeltaRed, 255);
    G := StartGreen + MulDiv(Ind, DeltaGreen, 255);
    B := StartBlue + MulDiv(Ind, DeltaBlue, 255);
    Bmp.Canvas.Brush.Color := RGB(R, G, B);
    Bmp.Canvas.FillRect(ColorRect);
  end;
  BitBlt(Canvas.Handle, DstRect.Left, DstRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
  Bmp.Destroy;
end;

procedure SetAlphaValue(var Color: TColor; Alpha: byte);
begin
  Color := Color and $00FFFFFF;
  Color := Color or (Alpha shl 24);
end;

procedure DrawAlphaGradient(Canvas: TCanvas; Source: TBitmap; StartValue: Byte; EndValue: Byte; DstRect: TRect; Vertical: boolean);
var
  Etalon: TBitmap;
  StartColor, EndColor: TColor;
  Ind: integer;
begin
  Etalon := TBitmap.Create;
  Etalon.PixelFormat := pf24Bit;
  StartColor := StartValue;
  EndColor := EndValue;
  Etalon.Width := 1;
  case Vertical of
    False: Etalon.Height := DstRect.Right - DstRect.Left;
    True: Etalon.Height := DstRect.Bottom - DstRect.Top;
  end;
  DrawGradient(Etalon.Canvas, StartColor, EndColor, Rect(0, 0, Etalon.Width, Etalon.Height), True);
  case Vertical of
    False:
      for Ind := 0 to Etalon.Height - 1 do
        AlphaBlendBitmap(Source, Rect(Ind, 0, Ind + 1, Source.Height), Canvas, Rect(DstRect.Left + Ind, DstRect.Top, DstRect.Left + Ind + 1, DstRect.Bottom), Etalon.Canvas.Pixels[0, Ind]);
    True:
      for Ind := 0 to Etalon.Height - 1 do
        AlphaBlendBitmap(Source, Rect(0, Ind, Source.Width, Ind + 1), Canvas, Rect(DstRect.Left, DstRect.Top + Ind, DstRect.Right, DstRect.Top + Ind + 1), Etalon.Canvas.Pixels[0, Ind]);
  end;
  Etalon.Destroy;
end;

end.
