{***************************************************************************}
{ TMS W7 Controls Pack                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2011 - 2014                                        }
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

unit W7ProgressBars;

interface
{$I TMSDEFS.INC}
uses
  Windows,  Classes, Controls, ImgList, Graphics, W7Classes, W7Common,
  W7Graphics;

type
  TW7ProgressBarStyle = (pbsBlue, pbsRed, pbsGreen, pbsCustom);

  TW7ProgressBarColors = record
    TopTopLeftColor: TColor;
    TopTopCenterColor: TColor;
    TopTopRightColor: TColor;
    TopBottomLeftColor: TColor;
    TopBottomCenterColor: TColor;
    TopBottomRightColor: TColor;
    BottomTopLeftColor: TColor;
    BottomTopCenterColor: TColor;
    BottomTopRightColor: TColor;
    BottomBottomLeftColor: TColor;
    BottomBottomCenterColor: TColor;
    BottomBottomRightColor: TColor;
    TopBorderColor: TColor;
    BottomBorderColor: TColor;
    LeftTopBorderColor: TColor;
    LeftBottomBorderColor: TColor;
    RightTopBorderColor: TColor;
    RightBottomBorderColor: TColor;
  end;

  TW7CustomProgressBar = class (TW7GraphicControl)
  private
    FMin: int64;
    FMax: int64;
    FPosition: int64;
    FBackgroundColor: TColor;
    FTransparent: boolean;
    FStyle: TW7ProgressBarStyle;
    FColors: TW7ProgressBarColors;
    FInternalColors: TW7ProgressBarColors;
    FProgressEtalon: TBitmap;
    procedure SetMin(Value: int64);
    procedure SetMax(Value: int64);
    procedure SetPosition(Value: int64);
    procedure SetBackgroundColor(Value: TColor);
    procedure SetTransparent(Value: boolean);
    procedure SetStyle(Value: TW7ProgressBarStyle);
    procedure SetColors(Value: TW7ProgressBarColors);
  protected
    procedure DrawBorder;
    procedure DrawProgress(Progress: integer);
    procedure Paint; override;
    procedure ActivateStyle(Style: TW7ProgressBarStyle);
    procedure CreateEtalon;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Min: int64 read FMin write SetMin;
    property Max: int64 read FMax write SetMax;
    property Position: int64 read FPosition write SetPosition;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property Transparent: boolean read FTransparent write SetTransparent;
    property Style: TW7ProgressBarStyle read FStyle write SetStyle;
    property Colors: TW7ProgressBarColors read FColors write SetColors;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7ProgressBar = class (TW7CustomProgressBar)
  published
    property Align;
    property Anchors;
    property Min;
    property Max;
    property Position;
    property BackgroundColor;
    property Transparent;
    property Style;
    property Colors;
  end;

implementation

constructor TW7CustomProgressBar.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
begin
  inherited;
  FMin := 0;
  FMax := 100;

  FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    FPosition := 10;

  FTransparent := True;
  FBackgroundColor := $00FFFFFF;
  Width := 250;
  Height := 13;
  FStyle := pbsBlue;
  FProgressEtalon := TBitmap.Create;
  ActivateStyle(pbsBlue);
end;

destructor TW7CustomProgressBar.Destroy;
begin
  FProgressEtalon.Destroy;
  inherited;
end;

procedure TW7CustomProgressBar.DrawBorder;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := $00B2B2B2;
  Canvas.MoveTo(2, 0);
  Canvas.LineTo(Width - 2, 0);

  Canvas.Pen.Color := $008C8C8C;
  Canvas.MoveTo(2, Height - 1);
  Canvas.LineTo(Width - 2, Height - 1);

  Canvas.Pen.Color := $00F2F2F2;
  Canvas.MoveTo(2, 1);
  Canvas.LineTo(Width - 2, 1);

  Canvas.Pen.Color := $00E1E1E1;
  Canvas.MoveTo(2, Height - 2);
  Canvas.LineTo(Width - 2, Height - 2);

  Canvas.Pixels[1, 0] := $00C8C8C8;
  Canvas.Pixels[Width - 2, 0] := $00C8C8C8;

  Canvas.Pixels[0, 1] := $00BDBDBD;
  Canvas.Pixels[Width - 1, 1] := $00BDBDBD;

  Canvas.Pixels[1, 1] := $00DADADA;
  Canvas.Pixels[Width - 2, 1] := $00DADADA;

  Canvas.Pixels[0, Height - 2] := $00A1A1A1;
  Canvas.Pixels[Width - 1, Height - 2] := $00A1A1A1;

  Canvas.Pixels[1, Height - 1] := $009E9E9E;
  Canvas.Pixels[Width - 2, Height - 1] := $009E9E9E;

  Canvas.Pixels[1, Height - 2] := $00C3C3C3;
  Canvas.Pixels[Width - 2, Height - 2] := $00C3C3C3;

  Canvas.Pen.Color := $00A5A5A5;
  Canvas.MoveTo(0, 2);
  Canvas.LineTo(0, Height div 2 + 2);

  Canvas.Pen.Color := $00999999;
  Canvas.LineTo(0, Height - 2);

  Canvas.Pen.Color := $00A5A5A5;
  Canvas.MoveTo(Width - 1, 2);
  Canvas.LineTo(Width - 1, Height div 2 + 2);

  Canvas.Pen.Color := $00999999;
  Canvas.LineTo(Width - 1, Height - 2);
     //////
  Canvas.Pen.Color := $00DDDDDD;
  Canvas.MoveTo(1, 2);
  Canvas.LineTo(1, Height div 2 + 2);

  Canvas.Pen.Color := $00D2D2D2;
  Canvas.LineTo(1, Height - 2);

  Canvas.Pen.Color := $00DDDDDD;
  Canvas.MoveTo(Width - 2, 2);
  Canvas.LineTo(Width - 2, Height div 2 + 2);

  Canvas.Pen.Color := $00D2D2D2;
  Canvas.LineTo(Width - 2, Height - 2);
end;

procedure TW7CustomProgressBar.DrawProgress(Progress: integer);
var
  Ind, PosY: integer;
begin
  if Progress < 2 then
    Exit;

  Canvas.Pen.Style := psSolid;
  if FProgressEtalon.Height <> (Height - 4) then
    CreateEtalon;

  if Progress > Width - 2 then
    Progress := Width - 2;
  ////////////////// Borders ////////////////////////////////
  Canvas.Pen.Color := FInternalColors.TopBorderColor;
  Canvas.MoveTo(2, 1);
  Canvas.LineTo(Progress, 1);

  Canvas.Pen.Color := FInternalColors.BottomBorderColor;
  Canvas.MoveTo(2, Height - 2);
  Canvas.LineTo(Progress, Height - 2);

  Canvas.Pen.Color := FInternalColors.LeftTopBorderColor;
  Canvas.MoveTo(1, 2);
  Canvas.LineTo(1, Height div 2 + 1);

  Canvas.Pen.Color := FInternalColors.LeftBottomBorderColor;
  Canvas.LineTo(1, Height - 2);

  Canvas.Pen.Color := FInternalColors.RightTopBorderColor;
  Canvas.MoveTo(Progress, 2);
  Canvas.LineTo(Progress, Height div 2 + 1);

  Canvas.Pen.Color := FInternalColors.RightBottomBorderColor;
  Canvas.LineTo(Progress, Height - 2);

  Canvas.Pixels[1, 1] := AlphaBlendPixel(FInternalColors.TopBorderColor, AlphaBlendPixel(FInternalColors.LeftTopBorderColor, Canvas.Pixels[1, 1], 150), 150);
  Canvas.Pixels[Progress, 1] := AlphaBlendPixel(FInternalColors.TopBorderColor, AlphaBlendPixel(FInternalColors.RightTopBorderColor, Canvas.Pixels[1, Height - 2], 150), 150);
  Canvas.Pixels[1, Height - 2] := AlphaBlendPixel(FInternalColors.BottomBorderColor, AlphaBlendPixel(FInternalColors.LeftBottomBorderColor, Canvas.Pixels[Progress, 1], 150), 150);
  Canvas.Pixels[Progress, Height - 2] := AlphaBlendPixel(FInternalColors.BottomBorderColor, AlphaBlendPixel(FInternalColors.RightBottomBorderColor, Canvas.Pixels[Progress, Height - 2], 150), 150);
  ///////////////////////////////////////////////////

  for Ind := 0 to FProgressEtalon.Height - 1 do
  begin
    PosY := Ind + 2;
    DrawGradient(Canvas, FProgressEtalon.Canvas.Pixels[0, Ind], FProgressEtalon.Canvas.Pixels[1, Ind], Rect(1, PosY, Progress div 2, PosY + 1), False);
    DrawGradient(Canvas, FProgressEtalon.Canvas.Pixels[1, Ind], FProgressEtalon.Canvas.Pixels[2, Ind], Rect(Progress div 2, PosY, Progress, PosY + 1), False);
  end;

end;

procedure TW7CustomProgressBar.Paint;
var
  ProgressWidth: integer;
begin
  inherited;
  Canvas.Lock;
  if not FTransparent then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := FBackgroundColor;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := FBackgroundColor;
    Canvas.Rectangle(1, 1, Width - 1, Height - 1);
  end;
  if fMax<>FMin then
    ProgressWidth := Round((Width / 100) * ((FPosition - FMin) / ((FMax - FMin) / 100)))
  else
    ProgressWidth := 0;
  DrawBorder;
  DrawProgress(ProgressWidth);
  Canvas.Unlock;
end;

procedure TW7CustomProgressBar.SetMin(Value: int64);
begin
  if Value > FMax then
    Exit;
  FMin := Value;
  if FMin > FPosition then
    FPosition := FMin;
  Invalidate;
end;

procedure TW7CustomProgressBar.SetMax(Value: int64);
begin
  if Value < FMin then
    Exit;
  FMax := Value;
  if FMax < FPosition then
    FPosition := FMax;
  Invalidate;
end;

procedure TW7CustomProgressBar.SetPosition(Value: int64);
begin
  FPosition := Value;
  if FPosition > FMax then
    FPosition := FMax;
  if FPosition < FMin then
    FPosition := FMin;
  Invalidate;
end;

procedure TW7CustomProgressBar.SetBackgroundColor(Value: TColor);
begin
  FBackgroundColor := Value;
  Invalidate;
end;

procedure TW7CustomProgressBar.SetTransparent(Value: boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TW7CustomProgressBar.SetStyle(Value: TW7ProgressBarStyle);
begin
  FStyle := Value;
  if FStyle <> pbsCustom then
    ActivateStyle(FStyle)
  else
    SetColors(FColors);
  Invalidate;
end;

procedure TW7CustomProgressBar.SetColors(Value: TW7ProgressBarColors);
begin
  FColors := Value;
  if FStyle = pbsCustom then
  begin

    FInternalColors := FColors;
    CreateEtalon;
//    Invalidate;
  end;
end;

procedure TW7CustomProgressBar.ActivateStyle(Style: TW7ProgressBarStyle);
begin
  case Style of
    pbsBlue:
    begin
      FInternalColors.TopTopLeftColor := $00EACD54;
      FInternalColors.TopTopCenterColor := $00F8E378;
      FInternalColors.TopTopRightColor := $00EACD54;
      FInternalColors.TopBottomLeftColor := $00C39F2B;
      FInternalColors.TopBottomCenterColor := $00E4CE56;
      FInternalColors.TopBottomRightColor := $00C39F2B;
      FInternalColors.BottomTopLeftColor := $006D5301;
      FInternalColors.BottomTopCenterColor := $00AF9601;
      FInternalColors.BottomTopRightColor := $006D5301;
      FInternalColors.BottomBottomLeftColor := $00815E03;
      FInternalColors.BottomBottomCenterColor := $00B6A00A;
      FInternalColors.BottomBottomRightColor := $00815E03;
      FInternalColors.TopBorderColor := $00F6D960;
      FInternalColors.BottomBorderColor := $00A5880F;
      FInternalColors.LeftTopBorderColor := $00CAA62F;
      FInternalColors.LeftBottomBorderColor := $007F5D02;
      FInternalColors.RightTopBorderColor := $00CAA62F;
      FInternalColors.RightBottomBorderColor := $007F5D02;
    end;
    pbsRed:
    begin
      FInternalColors.TopTopLeftColor := $00A3A3FF;
      FInternalColors.TopTopCenterColor := $00CDCDFF;
      FInternalColors.TopTopRightColor := $00A3A3FF;
      FInternalColors.TopBottomLeftColor := $006969FF;
      FInternalColors.TopBottomCenterColor := $008B8BFF;
      FInternalColors.TopBottomRightColor := $006969FF;
      FInternalColors.BottomTopLeftColor := $000000D0;
      FInternalColors.BottomTopCenterColor := $000000D4;
      FInternalColors.BottomTopRightColor := $000000D0;
      FInternalColors.BottomBottomLeftColor := $000000EC;
      FInternalColors.BottomBottomCenterColor := $000000EE;
      FInternalColors.BottomBottomRightColor := $000000EC;
      FInternalColors.TopBorderColor := $00CDCDFF;
      FInternalColors.BottomBorderColor := $000000FE;
      FInternalColors.LeftTopBorderColor := $007777FF;
      FInternalColors.LeftBottomBorderColor := $000101FF;
      FInternalColors.RightTopBorderColor := $007777FF;
      FInternalColors.RightBottomBorderColor := $000101FF;
    end;
    pbsGreen:
    begin

      FInternalColors.TopTopLeftColor := clLime;
      FInternalColors.TopTopCenterColor := clGreen;
      FInternalColors.TopTopRightColor := clLime;
      FInternalColors.TopBottomLeftColor := clGreen;
      FInternalColors.TopBottomCenterColor := clGreen;
      FInternalColors.TopBottomRightColor := clGreen;

      FInternalColors.BottomTopLeftColor := clGreen;
      FInternalColors.BottomTopCenterColor := clGreen;
      FInternalColors.BottomTopRightColor := clGreen;
      FInternalColors.BottomBottomLeftColor := clGreen;
      FInternalColors.BottomBottomCenterColor := clGreen;
      FInternalColors.BottomBottomRightColor := clGreen;

      FInternalColors.TopBorderColor := clLime;
      FInternalColors.BottomBorderColor := clGreen;
      FInternalColors.LeftTopBorderColor := clLime;
      FInternalColors.LeftBottomBorderColor := clLime;
      FInternalColors.RightTopBorderColor := clLime;
      FInternalColors.RightBottomBorderColor := clLime;
    end;

    pbsCustom:
      Exit;
  end;
  CreateEtalon;
  FColors := FInternalColors;
end;

procedure TW7CustomProgressBar.CreateEtalon;
begin
  with FProgressEtalon do
  begin
    Width := 3;
    Height := Self.Height - 4;
    PixelFormat := pf24Bit;
  end;
  DrawGradient(FProgressEtalon.Canvas, FInternalColors.TopTopLeftColor, FInternalColors.TopBottomLeftColor, Rect(0, 0, 1, FProgressEtalon.Height div 2 + 1), True);
  DrawGradient(FProgressEtalon.Canvas, FInternalColors.TopTopCenterColor, FInternalColors.TopBottomCenterColor, Rect(1, 0, 2, FProgressEtalon.Height div 2 + 1), True);
  DrawGradient(FProgressEtalon.Canvas, FInternalColors.TopTopRightColor, FInternalColors.TopBottomRightColor, Rect(2, 0, 3, FProgressEtalon.Height div 2 + 1), True);
  DrawGradient(FProgressEtalon.Canvas, FInternalColors.BottomTopLeftColor, FInternalColors.BottomBottomLeftColor, Rect(0, FProgressEtalon.Height div 2 + 1, 1, FProgressEtalon.Height), True);
  DrawGradient(FProgressEtalon.Canvas, FInternalColors.BottomTopCenterColor, FInternalColors.BottomBottomCenterColor, Rect(1, FProgressEtalon.Height div 2 + 1, 2, FProgressEtalon.Height), True);
  DrawGradient(FProgressEtalon.Canvas, FInternalColors.BottomTopRightColor, FInternalColors.BottomBottomRightColor, Rect(2, FProgressEtalon.Height div 2 + 1, 3, FProgressEtalon.Height), True);
end;

end.