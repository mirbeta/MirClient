{*************************************************************************}
{ TMS TAdvScrollControl                                                   }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvScrollControl;

{$I TMSDEFS.INC}

interface

uses
  Classes, Messages, Graphics, Windows, Forms, Controls, SysUtils, Types, uxTheme
  {$IFDEF DELPHIXE5_LVL}
  , VCL.Themes
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  {$IFDEF LCLLIB}
  , LCLType, Win32Proc, Win32Extra, JwaWinUser, LMessages
  {$ENDIF}
  ;

type
  {$IFDEF DELPHIXE8_LVL}
  TCustomScrollingStyleHook = class(TScrollingStyleHook)
  strict protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;
  {$ENDIF}


  TCustomScrollingControl = class(TScrollingWinControl)
  private
    FNHCanvas: TCanvas;
    FBorderStyle: TBorderStyle;
    FUseVCLStyles: boolean;
    FBorderColor: TColor;
    {$IFDEF LCLLIB}
    FCtl3D: boolean;
    {$ENDIF}
    {$IFDEF DELPHIXE8_LVL}
    class constructor Create;
    class destructor Destroy;
    {$ENDIF}
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Message: TWMVScroll); message WM_HSCROLL;
    function GetScrollPosition: TPoint;
    procedure SetScrollPosition(const Value: TPoint);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure DrawBorders;
    function GetHasHorizontalScrollBar: boolean;
    function GetHasVerticalScrollBar: boolean;
  protected
    procedure Paint; virtual; abstract;
    procedure ScrollHorz(const X: integer);
    procedure ScrollVert(const Y: integer);
    function ScrollUp(const Delta: integer): boolean;
    function ScrollDown(const Delta: integer): boolean;
    function PageUp: boolean;
    function PageDown: boolean;
    function ScrollHome: boolean;
    function ScrollEnd: boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    function ScrollSizeVert: integer;
    function ScrollSizeHorz: integer;
    function SetRange(ARange: TSize): boolean; virtual;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property UseVCLStyles: boolean read FUseVCLStyles write FUseVCLStyles;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintWindow(DC: HDC); override;
    property Canvas: TCanvas read FNHCanvas;
    property TopLeft: TPoint read GetScrollPosition write SetScrollPosition;
    {$IFDEF LCLLIB}
    property Ctl3D: boolean read FCtl3D write FCtl3D;
    {$ENDIF}
    property HasVerticalScrollBar: boolean read GetHasVerticalScrollBar;
    property HasHorizontalScrollBar: boolean read GetHasHorizontalScrollBar;
  published
    property Align;
    property Anchors;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Font;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

implementation

uses
  Math;

const
  SCROLL_DISTANCE = 20;

{ TCustomScrollingControl }

constructor TCustomScrollingControl.Create(AOwner: TComponent);
begin
  inherited;

  DoubleBuffered := true;

  FNHCanvas := TControlCanvas.Create;
  TControlCanvas(FNHCanvas).Control := Self;

  VertScrollBar.Tracking := true;
  HorzScrollBar.Tracking := true;
  FBorderStyle := bsSingle;
  FBorderColor := clGray;
  FUseVCLStyles := false;

  Ctl3D := true;
end;

{$IFDEF DELPHIXE8_LVL}
class constructor TCustomScrollingControl.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TCustomScrollingControl, TCustomScrollingStyleHook);
end;
{$ENDIF}

procedure TCustomScrollingControl.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);

  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      if BorderStyle = bsNone then
        Style := Style and not WS_BORDER
      else
        Style := Style or WS_BORDER;
    end;

    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

destructor TCustomScrollingControl.Destroy;
begin
  FNHCanvas.Free;
  inherited;
end;

function TCustomScrollingControl.GetHasHorizontalScrollBar: boolean;
begin
  Result := HorzScrollBar.IsScrollBarVisible;
end;

function TCustomScrollingControl.GetHasVerticalScrollBar: boolean;
begin
  Result := VertScrollBar.IsScrollBarVisible;
end;

function TCustomScrollingControl.GetScrollPosition: TPoint;
begin
  Result := Point(HorzScrollBar.ScrollPos, VertScrollBar.ScrollPos);
end;

function TCustomScrollingControl.PageDown: boolean;
var
  OldTopLeft: TPoint;
begin
  OldTopLeft := TopLeft;
  if VertScrollBar.Range > Height then
    TopLeft := Point(TopLeft.X, TopLeft.Y + Height);
  Result := TopLeft.Y <> OldTopLeft.Y;
end;

function TCustomScrollingControl.PageUp: boolean;
var
  OldTopLeft: TPoint;
begin
  OldTopLeft := TopLeft;
  TopLeft := Point(TopLeft.X, TopLeft.Y - Height);
  Result := TopLeft.Y <> OldTopLeft.Y;
end;

procedure TCustomScrollingControl.PaintWindow(DC: HDC);
begin
  FNHCanvas.Handle := DC;
  try
    Paint;
  finally
   FNHCanvas.Handle := 0;
  end;
end;

function TCustomScrollingControl.ScrollDown(const Delta: integer): boolean;
var
  OldTopLeft: TPoint;
begin
  OldTopLeft := TopLeft;
  TopLeft := Point(TopLeft.X, TopLeft.Y + Delta);
  Result:= TopLeft.Y <> OldTopLeft.Y;
end;

function TCustomScrollingControl.ScrollEnd: boolean;
var
  OldTopLeft: TPoint;
begin
  OldTopLeft := TopLeft;
  TopLeft := Point(0, VertScrollBar.Range - Height);
  Result:= TopLeft.Y <> OldTopLeft.Y;
end;

function TCustomScrollingControl.ScrollHome: boolean;
var
  OldTopLeft: TPoint;
begin
  OldTopLeft := TopLeft;
  TopLeft := Point(0,0);
  Result:= TopLeft.Y <> OldTopLeft.Y;
end;

procedure TCustomScrollingControl.ScrollHorz(const X: integer);
begin
  TopLeft := Point(X, TopLeft.Y);
end;

function TCustomScrollingControl.ScrollSizeHorz: integer;
var
  sbinfo: TSCROLLBARINFO;
begin
  Result := 0;

  if HandleAllocated then
  begin
    sbinfo.cbSize := Sizeof(TSCROLLBARINFO);
    GetScrollBarInfo(Handle, integer(OBJID_HSCROLL), sbinfo);
    if (sbinfo.rgstate[0] and STATE_SYSTEM_INVISIBLE = 0) then
      Result := GetSystemMetrics(SM_CYHSCROLL);
  end;
end;

function TCustomScrollingControl.ScrollSizeVert: integer;
var
  sbinfo: TSCROLLBARINFO;
begin
  Result := 0;
  if HandleAllocated then
  begin
    sbinfo.cbSize := Sizeof(TSCROLLBARINFO);
    GetScrollBarInfo(Handle, integer(OBJID_VSCROLL), sbinfo);
    if (sbinfo.rgstate[0] and STATE_SYSTEM_INVISIBLE = 0) then
      Result := GetSystemMetrics(SM_CXVSCROLL);
  end;
end;

function TCustomScrollingControl.ScrollUp(const Delta: integer): boolean;
var
  OldTopLeft: TPoint;
begin
  OldTopLeft := TopLeft;
  TopLeft := Point(TopLeft.X, TopLeft.Y - Delta);
  Result:= TopLeft.Y <> OldTopLeft.Y;
end;

procedure TCustomScrollingControl.ScrollVert(const Y: integer);
begin
  TopLeft := Point(TopLeft.X, Y);
end;

procedure TCustomScrollingControl.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

function TCustomScrollingControl.SetRange(ARange: TSize): boolean;
var
  vs: integer;
begin
  vs := ScrollSizeVert;
  HorzScrollBar.Range := ARange.cx;
  VertScrollBar.Range := ARange.cy;
  Result := (vs <> ScrollSizeVert);
end;

procedure TCustomScrollingControl.SetScrollPosition(const Value: TPoint);
begin
  HorzScrollBar.Position := Max(0,Min(Value.X, HorzScrollBar.Range));
  VertScrollBar.Position := Max(0,Min(Value.Y, VertScrollBar.Range));
end;

procedure TCustomScrollingControl.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

procedure TCustomScrollingControl.WMHScroll(var Message: TWMVScroll);
begin
  inherited;
  Invalidate;
end;

{$IFDEF DELPHIXE8_LVL}
class destructor TCustomScrollingControl.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TCustomScrollingControl, TCustomScrollingStyleHook);
end;
{$ENDIF}

procedure TCustomScrollingControl.DrawBorders;
var
  DC: HDC;
  OldPen: HPen;
  ARect: TRect;
  hTheme: THandle;
  clr: TColor;
  opts: TDTBGOPTS;
begin
  DC := GetWindowDC(Handle);
  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);

    {$IFDEF VCLLIB}
    if IsThemeActive and not UseVCLStyles then
    begin
      htheme := OpenThemeData(Handle,'COMBOBOX');
      opts.dwSize := Sizeof(opts);
      opts.dwFlags := DTBG_OMITCONTENT;
      DrawThemeBackgroundEx(hTheme, DC, 0, 0, ARect, @opts);
      CloseThemeData(htheme);
    end
    else
    {$ENDIF}
    begin
      clr := BorderColor;
      OldPen := SelectObject(DC,CreatePen(PS_SOLID,1,ColorToRGB(clr)));

      MovetoEx(DC,ARect.Left ,ARect.Top ,nil);
      LineTo(DC,ARect.Right -1 ,ARect.Top );
      LineTo(DC,ARect.Right -1 ,ARect.Bottom - 1);
      LineTo(DC,ARect.Left,ARect.Bottom -1 );
      LineTo(DC,ARect.Left,ARect.Top );

      DeleteObject(SelectObject(DC,OldPen));
    end;
  finally
    ReleaseDC(Handle,DC);
  end;
end;

procedure TCustomScrollingControl.WMNCPaint(var Message: TMessage);
begin
  inherited;
  if BorderStyle = bsSingle then
    DrawBorders;
end;

procedure TCustomScrollingControl.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TCustomScrollingControl.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  Invalidate;
end;

{$IFDEF DELPHIXE8_LVL}

{ TCustomScrollingStyleHook }

type
  TWinControlClassHook = class(TWinControl);

constructor TCustomScrollingStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverrideEraseBkgnd := True;
  if seClient in Control.StyleElements then
    Brush.Color := StyleServices.GetStyleColor(scPanel)
  else
    Brush.Color :=  TWinControlClassHook(Control).Color;
end;

procedure TCustomScrollingStyleHook.WndProc(var Message: TMessage);
begin
  // Reserved for potential updates
  inherited;
end;
{$ENDIF}


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.
