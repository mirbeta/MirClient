{*************************************************************************}
{ TAsgDateTimePicker component                                            }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2012                                              }
{           Email : info@tmssoftware.com                                  }
{           Website : http://www.tmssoftware.com/                         }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AsgDateTimePicker;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, CommCtrl, ExtCtrls
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes, System.Types
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  DROPDOWNBTN_WIDTH = 21;

type

  TAsgDateTimePicker = class(TDateTimePicker)
  private
    FBorderStyle: TBorderStyle;
    FBorderColor: TColor;
    FIsThemed: Boolean;
    FButtonColorDown: TColor;
    FButtonBorderColor: TColor;
    FButtonTextColor: TColor;
    FButtonTextColorHot: TColor;
    FButtonColor: TColor;
    FButtonColorHot: TColor;
    FButtonTextColorDown: TColor;
    FButtonWidth: Integer;
    FButtonHover: Boolean;
    FMouseInControl: Boolean;
    FIsWinXP: Boolean;
    FMetroStyle: Boolean;
    FFocusBorderColor: TColor;
    FDisabledBorder: Boolean;
    FHasFocus: Boolean;
    FFocusBorder: Boolean;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure NCPaintProc;
    procedure SetBorderColor(const Value: TColor);
    procedure SetButtonWidth(const Value: integer);
    procedure SetMetroStyle(const Value: Boolean);
    function  Is3DBorderControl: Boolean;
    function  Is3DBorderButton: Boolean;
    procedure DrawButtonBorder(DC:HDC);
    procedure DrawControlBorder(DC:HDC);
    procedure DrawBorders;
    function GetMetroStyleShowing: Boolean;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure WndProc(var Message: TMessage); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoVisualStyles: Boolean;
    procedure SetBorderStyle(const Value: TBorderStyle); virtual;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth default 19;

    property FocusBorderColor: TColor read FFocusBorderColor write FFocusBorderColor default clNone;
    property FocusBorder: Boolean read FFocusBorder write FFocusBorder default False;
    property DisabledBorder: Boolean read FDisabledBorder write FDisabledBorder default true;

    property MetroStyleShowing: Boolean read GetMetroStyleShowing;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: integer;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property ButtonColor: TColor read FButtonColor write FButtonColor default clNone;
    property ButtonColorHot: TColor read FButtonColorHot write FButtonColorHot default clNone;
    property ButtonColorDown: TColor read FButtonColorDown write FButtonColorDown default clNone;
    property ButtonTextColor: TColor read FButtonTextColor write FButtonTextColor default clNone;
    property ButtonTextColorHot: TColor read FButtonTextColorHot write FButtonTextColorHot default clNone;
    property ButtonTextColorDown: TColor read FButtonTextColorDown write FButtonTextColorDown default clNone;
    property ButtonBorderColor: TColor read FButtonBorderColor write FButtonBorderColor default clNone;
    property MetroStyle: Boolean read FMetroStyle write SetMetroStyle;
  published
    property Version: string read GetVersion write SetVersion;
  end;


implementation

uses
  ComStrs
  , ADvXPVS
  ;

//------------------------------------------------------------------------------

function IsVista: boolean;
var
  hKernel32: HMODULE;
begin
  hKernel32 := GetModuleHandle('kernel32');
  if (hKernel32 > 0) then
  begin
    Result := GetProcAddress(hKernel32, 'GetLocaleInfoEx') <> nil;
  end
  else
    Result := false;
end;

//------------------------------------------------------------------------------


function GetFileVersion(FileName:string): Integer;
var
  FileHandle:dword;
  l: Integer;
  pvs: PVSFixedFileInfo;
  lptr: uint;
  querybuf: array[0..255] of char;
  buf: PChar;
begin
  Result := -1;

  StrPCopy(querybuf,FileName);
  l := GetFileVersionInfoSize(querybuf,FileHandle);
  if (l>0) then
  begin
    GetMem(buf,l);
    GetFileVersionInfo(querybuf,FileHandle,l,buf);
    if VerQueryValue(buf,'\',Pointer(pvs),lptr) then
    begin
      if (pvs^.dwSignature=$FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;

//------------------------------------------------------------------------------

function IsComCtl6: Boolean;
var
  i: Integer;
begin
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  Result := (i > 5);
end;

//------------------------------------------------------------------------------

function IsMouseButtonDown: Boolean;
{
  Returns a "True" if a Mouse button happens to be down.
}
begin
  {Note: Key state is read twice because the first time you read it,
   you learn if the bittpm has been pressed ever.
   The second time you read it you learn if
   the button is currently pressed.}
  Result := not(((GetAsyncKeyState(VK_RBUTTON)and $8000)=0) and
     ((GetAsyncKeyState(VK_LBUTTON)and $8000)=0));
end;

//------------------------------------------------------------------------------

{ TAsgDateTimePicker }

constructor TAsgDateTimePicker.Create(AOwner: TComponent);
var
  i: Integer;
  dwVersion:Dword;
  dwWindowsMajorVersion,dwWindowsMinorVersion:Dword;
begin
  inherited;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL) + 2;
  FMetroStyle := false;
  DoubleBuffered := True;
  ParentCtl3D := False;
  BorderStyle := bsSingle;
  Ctl3D := true;

  FMouseInControl := False;
  FBorderStyle := bsNone;
  FBorderColor := clNone;
  FButtonHover := False;

  FFocusBorder := False;
  FDisabledBorder := False;
  FFocusBorderColor := clNone;
  FButtonColor := clNone;
  FButtonColorHot := clNone;
  FButtonColorDown := clNone;
  FButtonTextColor := clNone;
  FButtonTextColorHot := clNone;
  FButtonTextColorDown := clNone;
  FButtonBorderColor := clNone;

  dwVersion := Windows.GetVersion;
  dwWindowsMajorVersion :=  DWORD(LOBYTE(LOWORD(dwVersion)));
  dwWindowsMinorVersion :=  DWORD(HIBYTE(LOWORD(dwVersion)));
  FIsWinXP := (dwWindowsMajorVersion > 5) OR
    ((dwWindowsMajorVersion = 5) AND (dwWindowsMinorVersion >= 1));

  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsThemed := (i > 5);
end;

//------------------------------------------------------------------------------

destructor TAsgDateTimePicker.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

function TAsgDateTimePicker.DoVisualStyles: Boolean;
begin
  if FIsThemed then
    Result := IsThemeActive
  else
    Result := False;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.DrawButtonBorder(DC: HDC);
const
  Flags: array[Boolean] of Integer = (0, BF_FLAT);
  Edge: array[Boolean] of Integer = (EDGE_RAISED,EDGE_ETCHED);

var
  ARect: TRect;
  BtnFaceBrush: HBRUSH;

begin
  ExcludeClipRect(DC, ClientWidth - FButtonWidth + 4, 4, ClientWidth - 4, ClientHeight - 4);

  GetWindowRect(Handle, ARect);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  Inc(ARect.Left, ClientWidth - FButtonWidth - 2);
  InflateRect(ARect, -2, -2);

  if Is3DBorderButton then
    DrawEdge(DC, ARect, Edge[False], BF_RECT or Flags[DroppedDown])
  else
  begin
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
    InflateRect(ARect, 0, -1);
    ARect.Right := ARect.Right - 1;
    FillRect(DC, ARect, BtnFaceBrush);
    DeleteObject(BtnFaceBrush);
  end;

  ExcludeClipRect(DC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.DrawControlBorder(DC: HDC);
var
  ARect:TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
  //OldPen: HPen;

begin
  if not Enabled and FIsThemed and not DisabledBorder then
  begin
    BtnFaceBrush := CreateSolidBrush(ColorToRGB($B99D7F));
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    FrameRect(DC, ARect, BtnFaceBrush);
    DeleteObject(BtnFaceBrush);
    Exit;
  end;


  if (FBorderColor <> clNone) then
  begin
    if not FHasFocus or (FFocusBorderColor = clNone) then
    begin
      BtnFaceBrush := CreateSolidBrush(ColorToRGB(FBorderColor));
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      DeleteObject(BtnFaceBrush);
      Exit;
    end;
  end;

  if (FFocusBorderColor <> clNone) then
  begin
    if FHasFocus then
    begin
      BtnFaceBrush := CreateSolidBrush(ColorToRGB(FFocusBorderColor));
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      DeleteObject(BtnFaceBrush);
    end;
    Exit;
  end;

  if Is3DBorderControl then
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE))
  else
    BtnFaceBrush := CreateSolidBrush(ColorToRGB((Parent as TWinControl).Brush.Color));

  WindowBrush := CreateSolidBrush(ColorToRGB(self.Color));

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    if Is3DBorderControl then
    begin
      DrawEdge(DC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, WindowBrush);
    end
    else
    begin
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);

      ARect.Right := ARect.Right - ButtonWidth; //GetSystemMetrics(SM_CXVSCROLL);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, WindowBrush);
      ARect.Right := ARect.Right + ButtonWidth; //GetSystemMetrics(SM_CXVSCROLL);
    end;

    {
    if FFlat and (FFlatLineColor <> clNone) then
    begin
      OldPen := SelectObject(DC,CreatePen( PS_SOLID,1,ColorToRGB(FFlatLineColor)));
      MovetoEx(DC,ARect.Left - 2,Height - 1,nil);
      LineTo(DC,ARect.Right - ButtonWidth - 1 ,Height - 1);
      DeleteObject(SelectObject(DC,OldPen));
    end;
    }
  finally
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  end;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.DrawBorders;
var
  DC: HDC;
begin
  if not MetroStyleShowing or (Enabled and not ((FBorderColor <> clNone) or (FFocusBorderColor <> clNone))) then
    Exit;

  DC := GetWindowDC(Handle);

  try
    DrawControlBorder(DC);
    if not (FIsWinXP and DoVisualStyles) then
        DrawButtonBorder(DC);
  finally
    ReleaseDC(Handle,DC);
  end;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FHasFocus := true;
    DrawBorders;
  end;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.CMExit(var Message: TCMExit);
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    FHasFocus := false;
    DrawBorders;
  end;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;

  if not FMouseInControl and Enabled then
    begin
     FMouseInControl := True;
     DrawBorders;
    end;

  if FIsWinXP then
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;

  if FMouseInControl and Enabled then
    begin
     FMouseInControl := False;
     DrawBorders;
    end;

  FButtonHover := False;

  if FIsWinXP then
    Invalidate;
end;

//------------------------------------------------------------------------------

function TAsgDateTimePicker.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);
end;

//------------------------------------------------------------------------------

function TAsgDateTimePicker.Is3DBorderControl: Boolean;
begin
  if csDesigning in ComponentState then
    Result := False
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);

  Result := Result and FFocusBorder;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.CreateWnd;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.NCPaintProc;
var
  DC: HDC;
  //WindowBrush:hBrush;
  Canvas: TCanvas;

begin
  if Ctl3D then
    Exit;
  if BorderColor = clNone then
    Exit;

  DC := GetWindowDC(Handle);
  //WindowBrush := 0;
  try
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;

    //WindowBrush := CreateSolidBrush(ColorToRGB(clRed));

    if (BorderStyle = bsNone) and (Parent is TWinControl) then
      Canvas.Pen.Color := (Parent as TWinControl).Brush.Color
    else
    begin
      if FIsThemed then
        Canvas.Pen.Color := $B99D7F
      else
        Canvas.Pen.Color := BorderColor;
    end;

    Canvas.MoveTo(0,Height);
    Canvas.LineTo(0,0);
    Canvas.LineTo(Width - 1,0);
    Canvas.LineTo(Width - 1,Height - 1);
    Canvas.LineTo(0,Height-1);

    if (BorderStyle = bsSingle) and (Parent is TWinControl) then
      Canvas.Pen.Color := (Parent as TWinControl).Brush.Color;

    if (BorderStyle in [bsNone, bsSingle]) and (Parent is TWinControl) then
    begin
      Canvas.MoveTo(1,Height - 2);
      Canvas.LineTo(1,1);
      Canvas.LineTo(Width - 1,1);
    end;

    Canvas.Free;

    // FrameRect(DC, ARect, WindowBrush);
  finally
    //DeleteObject(WindowBrush);
    ReleaseDC(Handle,DC);
  end;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.WMNCPaint(var Message: TMessage);
begin
  inherited;
  NCPaintProc;

  if (not Enabled and DoVisualStyles and not FDisabledBorder) then
    DrawBorders;

  if ((FFocusBorderColor <> clNone) and (GetFocus = Handle)) or (FBorderColor <> clNone) then
    DrawBorders;

  Message.Result := 0;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  ACanvas: TCanvas;

  procedure DrawButton;
  var
    ARect: TRect;
    htheme: THandle;
    pt: array of TPoint;

  begin
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    Inc(ARect.Left, ClientWidth - FButtonWidth + 2);
    InflateRect(ARect, -1, -1);

    if FButtonColor <> clNone then
    begin
      ACanvas.Brush.Color := FButtonColor;

      if FButtonHover then
        ACanvas.Brush.Color := FButtonColorHot;

      if IsMouseButtonDown and FButtonHover then
        ACanvas.Brush.Color := FButtonColorDown;

      ACanvas.Pen.Color := ACanvas.Brush.Color;

      ARect.Left := ARect.Left + 2;
      ACanvas.FillRect(ARect);
      ARect.Left := ARect.Left - 2;

      ACanvas.Pen.Color := FButtonBorderColor;
      ACanvas.MoveTo(ARect.Left, ARect.Top);
      ACanvas.LineTo(ARect.Left, ARect.Bottom);

      ACanvas.Brush.Color := FButtonTextColor;

      if IsMouseButtonDown then
        ACanvas.Brush.Color := FButtonTextColorDown;

      ACanvas.Pen.Color := ACanvas.Brush.Color;

      SetLength(pt, 3);

      pt[0].X := ARect.Left + 5;
      pt[0].y := ARect.Top + 7;

      pt[1].X := ARect.Left + 11;
      pt[1].y := ARect.Top + 7;

      pt[2].X := ARect.Left + 8;
      pt[2].y := ARect.Top + 10;

      ACanvas.Polygon(pt);
    end
    else
    begin
      if DoVisualStyles then
      begin
        htheme := OpenThemeData(Handle,'combobox');

        if not Enabled then
        begin
          DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_DISABLED,@ARect,nil)
        end
        else
        begin
          if IsMouseButtonDown and DroppedDown then
          begin
            DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_PRESSED,@ARect,nil)
          end
          else
          begin
            if not IsMouseButtonDown and FButtonHover and not DroppedDown then
              DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_HOT,@ARect,nil)
            else
            DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_NORMAL,@ARect,nil);
          end;
        end;

        CloseThemeData(htheme);
      end
      else
      begin
        if Enabled then
          DrawFrameControl(DC, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT )
        else
          DrawFrameControl(DC, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_INACTIVE )
      end;
    end;

    ExcludeClipRect(DC, ClientWidth - FButtonWidth -4 , 0, ClientWidth +2, ClientHeight);
  end;

begin
  inherited;

  if (Ctl3D and not MetroStyleShowing) or not IsVista then
    Exit;

  DC := GetWindowDC(Handle);
  ACanvas := TCanvas.Create;

  try
    ACanvas.Handle := DC;

    if MetroStyleShowing then
    begin
      //FillRect(DC, ClientRect, Brush.Handle);
      if (Kind = dtkTime) then
      begin
        //DrawSpinButton;  //do nothing
      end
      else
      begin
        if IsVista and IsComCtl6 then
        begin
          if Assigned(IsThemeActive) and IsThemeActive then
            FillRect(DC, Rect(ClientRect.Right - FButtonWidth - 15, 1, ClientRect.Right, Height), Brush.Handle)
          else
            FillRect(DC, Rect(ClientRect.Right - FButtonWidth - 2, 4, ClientRect.Right - 2, Height - 4), Brush.Handle);
        end;
        DrawButton;
      end;
    end;

  finally
    ACanvas.Free;
    ReleaseDC(Handle,DC);
  end;

  DrawBorders;
end;

//------------------------------------------------------------------------------
procedure TAsgDateTimePicker.WMSize(var Message: TWMSize);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.WndProc(var Message: TMessage);
begin
  inherited;

  if Message.Msg = WM_NCPAINT then
  begin
    if (not Enabled and DoVisualStyles and not FDisabledBorder) then
      DrawBorders;

    if ((FFocusBorderColor <> clNone) and (GetFocus = Handle)) or (FBorderColor <> clNone) then
      DrawBorders;
  end;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.KeyPress(var Key: Char);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (X > Width - FButtonWidth) and (X < Width) then
  begin
    if not FButtonHover then
    begin
      FButtonHover := True;
      Invalidate;
    end;
  end
  else
  begin
    if FButtonHover then
    begin
      FButtonHover := False;
      Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.SetBorderStyle(const Value: TBorderStyle);
begin
  if (FBorderStyle <> Value) then
  begin
    FBorderStyle := Value;
    {if (FBorderStyle = bsCtl3D) then
    begin
      ParentCtl3D := True;
      Ctl3D := True;
    end
    else if Ctl3D then
    begin
      ParentCtl3D := False;
      Ctl3D := false;
    end;
    }
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.SetButtonWidth(const Value: integer);
begin
  if (value < 14) or (value > 32) then
    Exit;

  FButtonWidth := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.SetMetroStyle(const Value: Boolean);
begin
  if (FMetroStyle <> Value) then
  begin
    FMetroStyle := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAsgDateTimePicker.GetMetroStyleShowing: Boolean;
begin
  Result := MetroStyle and not (csDesigning in ComponentState);
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

function TAsgDateTimePicker.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAsgDateTimePicker.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));

end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAsgDateTimePicker.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

end.
