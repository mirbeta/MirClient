{***************************************************************************}
{ TAdvScrollBox component                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2007 - 2014                                        }
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

unit AdvScrollBox;

{$I TMSDEFS.INC}

interface

uses
  Messages, Windows, SysUtils, CommCtrl, Classes, Controls, Forms,
  Graphics, ImgList, ComCtrls, Math, Dialogs;

const

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 2; // Build nr.

  // version history
  // 1.0.0.0 : first release
  // 1.1.0.0 : Added events OnHScroll, OnVScroll, OnHScrollEnd, OnVScrollEnd
  // 1.1.0.1 : Fixed : issue with transparency when scrollbox has no scrollbars
  // 1.1.0.2 : Fixed : issue with AutoScroll inherited from base class TScrollBox


type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvScrollBox = class(TScrollBox)
  private
    FCanvas: TCanvas;
    FBorderColor: TColor;
    FOnVScroll: TNotifyEvent;
    FOnHScroll: TNotifyEvent;
    FOnHScrollEnd: TNotifyEvent;
    FOnVScrollEnd: TNotifyEvent;
    Procedure WMEraseBkGnd( Var msg: TWMEraseBkGnd ); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure NCPaintProc;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetBorderColor(const Value: TColor);
  protected
    procedure Paint; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintWindow(DC: HDC); override;
    procedure WndProc(var Message: TMessage); override;
    procedure DrawBorder;
    property Canvas: TCanvas read FCanvas;
    procedure AutoScrollInView(AControl: TControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: integer;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Version: string read GetVersion write SetVersion stored false;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property OnVScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHScroll: TNotifyEvent read FOnHScroll write FOnHScroll;
    property OnVScrollEnd: TNotifyEvent read FOnVScrollEnd write FOnVScrollEnd;
    property OnHScrollEnd: TNotifyEvent read FOnHScrollEnd write FOnHScrollEnd;
  end;

implementation

//------------------------------------------------------------------------------

{TWinCtrl}

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

//------------------------------------------------------------------------------

{ TAdvScrollBox }

procedure TAdvScrollBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
   //Params.ExStyle := Params.ExStyle + WS_EX_TRANSPARENT;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.WMEraseBkGnd(var msg: TWMEraseBkGnd);
begin
  inherited;
  //SetBkMode( msg.DC, TRANSPARENT );
  //msg.result := 1;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.Paint;
var
  R: TRect;
  i: Integer;
  P: TPoint;
begin
  R := ClientRect;
  //rgn1 :=  CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
  //SelectClipRgn(Canvas.Handle, rgn1);

  i := SaveDC(Canvas.Handle);
  p := ClientOrigin;
  Windows.ScreenToClient(Parent.Handle, p);
  p.x := -p.x;
  p.y := -p.y;
  MoveWindowOrg(Canvas.Handle, p.x, p.y);

  SendMessage(Parent.Handle, WM_ERASEBKGND, Canvas.Handle, 0);
  // transparency ?
  SendMessage(Parent.Handle, WM_PAINT, Canvas.Handle, 0);

  if (Parent is TWinCtrl) then
   (Parent as TWinCtrl).PaintCtrls(Canvas.Handle, nil);

  RestoreDC(Canvas.Handle, i);

  //SelectClipRgn(Canvas.Handle, 0);
  //DeleteObject(rgn1);
  
  DrawBorder;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.DrawBorder;
var
  R: TRect;
begin
  Exit;
  R := ClientRect;
  R := Rect(R.Left-1, R.Top-1, R.Right+1, R.Bottom+1);
  Canvas.Pen.Color := clRed;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(R);
end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.WMPaint(var Message: TWMPaint);
begin
  //Include(FControlState, csCustomPaint);
  inherited;
  //Exclude(FControlState, csCustomPaint);
end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.WndProc(var Message: TMessage);
begin
  inherited;
  {if (Message.Msg = WM_PAINT) and (not Ctl3D) then
  begin
    Message.Result := 0;
    DrawBorder;
  end;}
end;

//------------------------------------------------------------------------------

constructor TAdvScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FBorderColor := clSilver;
  DoubleBuffered := True;
  ParentCtl3D := False;
  Ctl3D := false;
end;

//------------------------------------------------------------------------------

destructor TAdvScrollBox.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  Invalidate;

  if (Message.ScrollCode <> SB_ENDSCROLL) and Assigned(FOnHScroll) then
  begin
    FOnHScroll(Self);
  end;

  if (Message.ScrollCode = SB_ENDSCROLL) and Assigned(FOnHScrollEnd) then
  begin
    FOnHScrollEnd(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  Invalidate;

  if (Message.ScrollCode <> SB_ENDSCROLL) and Assigned(FOnVScroll) then
  begin
    FOnVScroll(Self);
  end;

  if (Message.ScrollCode = SB_ENDSCROLL) and Assigned(FOnVScrollEnd) then
  begin
    FOnVScrollEnd(Self);
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvScrollBox.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.NCPaintProc;
var
  DC: HDC;
  //WindowBrush:hBrush;
  Canvas: TCanvas;

begin
  if (BorderStyle = bsNone) or (Ctl3D) then
    Exit;

  DC := GetWindowDC(Handle);
  //WindowBrush := 0;
  try
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;

    //WindowBrush := CreateSolidBrush(ColorToRGB(clRed));

    Canvas.Pen.Color := BorderColor;
    Canvas.MoveTo(0,Height);
    Canvas.LineTo(0,0);
    Canvas.LineTo(Width - 1,0);
    Canvas.LineTo(Width - 1,Height - 1);
    Canvas.LineTo(0,Height-1);

    {if FIsWinXP then
      Canvas.Pen.Color := $B99D7F
    else
      Canvas.Pen.Color := clGray;

    Canvas.MoveTo(1,Height);
    Canvas.LineTo(1,1);
    Canvas.LineTo(Width - 2,1);
    Canvas.LineTo(Width - 2,Height - 2);
    Canvas.LineTo(1,Height - 2);

    if (Parent is TWinControl) then
    begin
      Canvas.Pen.Color := (Parent as TWinControl).Brush.Color;
      Canvas.MoveTo(0,Height);
      Canvas.LineTo(0,0);
      Canvas.LineTo(Width - 1,0);
      Canvas.LineTo(Width - 1,Height - 1);
      Canvas.LineTo(0,Height-1);
    end; }

    Canvas.Free;

    // FrameRect(DC, ARect, WindowBrush);
  finally
    //DeleteObject(WindowBrush);
    ReleaseDC(Handle,DC);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.WMNCPaint(var Message: TMessage);
begin
  inherited;
  //if FUpdateCount > 0 then Exit;
  NCPaintProc;
  Message.Result := 0;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    if (Handle <> 0) then
      NCPaintProc;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollBox.AutoScrollInView(AControl: TControl);
begin
  if AutoScroll then
    inherited;
end;

procedure TAdvScrollBox.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and (Message.Inserting) then
    Invalidate;
end;

end.
