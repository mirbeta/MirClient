{***************************************************************************}
{ TSHADER component                                                         }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by                                                                }
{       TMS Software                                                        }
{       copyright © 1998-2013                                               }
{       Email : info@tmssoftware.com                                        }
{       Web : http://www.tmssoftware.com                                    }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}


unit Shader;
{$I TMSDEFS.INC}
interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, AdvStyleIF
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF};

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.2.0.0 : added mirror gradient capability
  //          : added style interface
  // v1.2.1.0 : Added support for Office 2007 styles
  // v1.2.1.1 : Fixed : repaint issue on style change
  // v1.2.1.2 : Fixed : issue with caption drawing
  // v1.2.1.3 : Fixed : issue with Alignment <> taCenter
  // v1.3.0.0 : New : Terminal, Windows Vista & Windows 7 styles added
  // v1.4.0.0 : New : Built in support for Office 2010 colors
  // v1.4.1.0 : New : Windows 8, Office 2013 styles added
 //  v1.4.2.0 : New : Windows 10, Office 2016 styles added
  
type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TShader = class(TPanel, ITMSStyle)
  private
    FFromColor:TColor;
    FToColor:TColor;
    FFromColorMirror: TColor;
    FToColorMirror: TColor;
    FSteps:word;
    FDirection:boolean;
    procedure SetFromColor(const Value:TColor);
    procedure SetToColor(const Value:TColor);
    procedure SetFromColorMirror(const Value:TColor);
    procedure SetToColorMirror(const Value:TColor);
    procedure SetSteps(w:word);
    procedure SetDirection(b:boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;
    procedure Paint; override;
    { Protected declarations }
  public
    constructor Create(AOwner:TComponent); override;
    procedure SetComponentStyle(AStyle: TTMSStyle);    
    { Public declarations }
  published
    property FromColor:TColor read FFromColor write SetFromCOlor default clBlue;
    property ToColor:TColor read FToColor write SetToColor default clBlack;
    property FromColorMirror: TColor read FFromColorMirror write SetFromColorMirror default clNone;
    property ToColorMirror: TColor read FToColorMirror write SetToColorMirror default clNone;    
    property Steps:word read FSteps write SetSteps default 20;
    property Direction:boolean read FDirection write SetDirection;
    property Version: string read GetVersion write SetVersion;
    { Published declarations }
  end;

procedure Register;

implementation

type
  TGradientDirection = (gdVertical, gdHorizontal);

constructor TShader.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FSteps := 20;
  FDirection := False;
  FFromColor := clBlue;
  FToColor := clBlack;
  FFromColorMirror := clNone;
  FToColorMirror := clNone;
end;

procedure TShader.SetFromColor(const Value:TColor);
begin
  if (FFromColor <> Value) then
  begin
    FFromColor := Value;
    Invalidate;
  end;
end;

procedure TShader.SetToColor(const Value:TColor);
begin
  if (FToColor <> Value) then
  begin
    FToColor := Value;
    Invalidate;
  end;
end;

procedure TShader.SetFromColorMirror(const Value:TColor);
begin
  if (FFromColorMirror <> Value) then
  begin
    FFromColorMirror := Value;
    Invalidate;
  end;
end;

procedure TShader.SetToColorMirror(const Value:TColor);
begin
  if (FToColorMirror <> Value) then
  begin
    FToColorMirror := Value;
    Invalidate;
  end;

end;


procedure TShader.SetSteps(w:word);
begin
  if (FSteps <> w) then
  begin
    FSteps := w;
    Invalidate;
  end;
end;

procedure TShader.SetComponentStyle(AStyle: TTMSStyle);
var
  i: Integer;
begin
  case AStyle of
    tsOffice2003Blue:
      begin
        FromColor := $FDEADA;
        ToColor := $E4AE88;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
    tsOffice2003Silver:
      begin
        FromColor := $ECE2E1;
        ToColor := $B39698;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
    tsOffice2003Olive:
      begin
        FromColor := $CFF0EA;
        ToColor := $8CC0B1;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
    tsOffice2003Classic:
      begin
        FromColor := clWhite;
        ToColor := $00E3F0F2;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
    tsOffice2007Luna:
      begin
        FromColor := $FAF1E9;
        ToColor := $EDD8C7;
        FromColorMirror := $EDD8C7;
        ToColorMirror := $FFF2E7;
      end;
    tsOffice2007Obsidian:
      begin
        FromColor := $CFC6C1;
        ToColor := $C5BBB4;
        FromColorMirror := $C5BBB4;
        ToColorMirror := $ECECE5;
      end;
    tsOffice2007Silver:
      begin
        FromColor := $F9F5F3;
        ToColor := $E7DCD5;
        FromColorMirror := $E7DCD5;
        ToColorMirror := $FBFAF0;
      end;
    tsWindowsXP:
      begin
        FromColor := clBtnFace;
        ToColor := clBtnFace;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
    tsWhidbey:
      begin
        FromColor := clWhite;
        ToColor := $00E3F0F2;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
      tsWindowsVista:
      begin
        FromColor := $FDF8F1;
        ToColor := $FCEFD5;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
      tsWindows7:
      begin
        FromColor := $FCEBDC;
        ToColor := $FCDBC1;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
      tsTerminal:
      begin
        FromColor := clBtnFace;
        ToColor := clBtnFace;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
      tsOffice2010Blue:
      begin
        FromColor := $FDF6EF;
        ToColor := $F0DAC7;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
      tsOffice2010Silver:
      begin
        FromColor := $FFFFFF;
        ToColor := $EDE5E0;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
      tsOffice2010Black:
      begin
        FromColor := $BFBFBF;
        ToColor := $919191;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
     tsWindows8, tsWindows10:
      begin
        FromColor := $F7F6F5;
        ToColor := $F7F6F5;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
     tsOffice2013White:
      begin
        FromColor := $EEEEEE;
        ToColor := $EEEEEE;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
     tsOffice2013LightGray:
      begin
        FromColor := $F6F6F6;
        ToColor := $F6F6F6;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
     tsOffice2013Gray:
      begin
        FromColor := $E5E5E5;
        ToColor := $E5E5E5;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
     tsOffice2016White:
      begin
        FromColor := clWhite;
        ToColor := clWhite;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
     tsOffice2016Gray:
      begin
        FromColor := $B2B2B2;
        ToColor := $B2B2B2;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;
     tsOffice2016Black:
      begin
        FromColor := $363636;
        ToColor := $363636;
        FromColorMirror := clNone;
        ToColorMirror := clNone;
      end;

    tsCustom:
      begin
      end;
  end;

  for i := 0 to ControlCount - 1 do
    Controls[i].Invalidate;
end;

procedure TShader.SetDirection(b:boolean);
begin
  if (FDirection <> b) then
  begin
    FDirection := b;
    Invalidate;
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

      if Direction = gdVertical then
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
        DrawGradient(ACanvas,  ColorFrom, ColorTo, 128, r, Direction = gdVertical);
        r := ARect;
        r.Top := r.Top + ((r.Bottom - r.Top) div 2);
        DrawGradient(ACanvas,  ColorMirrorFrom, ColorMirrorTo, 128, r, Direction = gdVertical);
      end;
    end
    else
      DrawGradient(ACanvas, ColorFrom, ColorTo, 128, ARect, Direction = gdVertical);
  end;

  if BorderColor <> clNone then
  begin
    ACanvas.Brush.Color := BorderColor;
    ACanvas.FrameRect(ARect);
  end;
end;


procedure TShader.Paint;
var
  r: TRect;
  dtstyle: DWORD;

begin
  r := ClientRect;
  if Direction then
    DrawVistaGradient(Canvas, R, FromColor, ToColor, FromColorMirror, ToColorMirror, gdVertical, clNone)
  else
    DrawVistaGradient(Canvas, R, FromColor, ToColor, FromColorMirror, ToColorMirror, gdHorizontal, clNone);

  Canvas.Font.Assign(Font);

  SetBkMode(Canvas.Handle, TRANSPARENT);

  dtstyle := DT_CENTER;

  case Alignment of
  taLeftJustify: dtstyle := DT_LEFT;
  taRightJustify: dtstyle := DT_RIGHT;
  end;


  DrawText(Canvas.Handle, Pchar(Caption), Length(Caption), R, DT_SINGLELINE or dtstyle or DT_VCENTER);
end;

function TShader.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TShader.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TShader.SetVersion(const Value: string);
begin

end;

procedure Register;

begin
  RegisterComponents('TMS', [TShader]);
end;

end.
