{*************************************************************************}
{ AdvMetroHint component                                                  }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 2012 - 2015                                      }
{            Email : info@tmssoftware.com                                 }
{            Website : http://www.tmssoftware.com/                        }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvMetroHint;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Graphics, Messages, Controls, Forms, SysUtils, AdvStyleIF,
  AppEvnts, Types;

const
  HINTROUNDING = 16;

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.0.0.0 : First release
  // 1.0.1.0 : New : support for multiline hints
  // 1.0.1.1 : Fixed : Issue with MaxWidth handling
  // 1.0.2.0 : New : Line property added to control position of lines around hint

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  THintStyle = (hsRectangle,hsRounded);

  THintLinePos = (hlTop, hlLeft, hlRight, hlBottom);

  THintLine = set of THintLinePos;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvMetroHint = class(TComponent, ITMSTones)
  private
    FHintFont: TFont;
    FHintInfo: THintInfo;
    FHintColor: TColor;
    FOnShowHint: TShowHintEvent;
    FYMargin: Integer;
    FXMargin: Integer;
    FHintStyle: THintStyle;
    FMaxWidth: Integer;
    FLineColor: TColor;
    FShadow: boolean;
    FLine: THintLine;
    {$IFDEF DELPHI6_LVL}
    FApplicationEvents : TApplicationEvents;
    {$ENDIF}
    procedure GetHintInfo(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure SetHintColor(const Value: TColor);
    procedure SetHintFont(const Value: TFont);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // ITMSTones interface
    procedure SetColorTones(ATones: TColorTones);
  published
    property HintColor: TColor read FHintColor write SetHintColor default $00F2BC00;
    property HintFont: TFont read FHintFont write SetHintFont;
    property HintStyle: THintStyle read FHintStyle write FHintStyle default hsRectangle;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 0;
    property XMargin: Integer read FXMargin write FXMargin default 4;
    property YMargin: Integer read FYMargin write FYMargin default 2;
    property LineColor: TColor read FLineColor write FLineColor default clNone;
    property Line: THintLine read FLine write FLine;
    property Shadow: boolean read FShadow write FShadow default false;
    property Version: string read GetVersion write SetVersion;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
  end;

  { TMetroHintWindow }
  TMetroHintWindow = class(THintWindow)
  private
    FHint: TAdvMetroHint;
    FTextHeight, FTextWidth: Integer;
    function FindHintControl: TAdvMetroHint;
  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
  published
  end;


implementation

uses
  Commctrl, ShellApi;

var
  HintControl: TControl; { control the tooltip belongs to }
  HintMaxWidth: Integer; { max width of the tooltip }

//------------------------------------------------------------------------------

constructor TAdvMetroHint.Create(AOwner: TComponent);
var
  I,Instances:Integer;

begin
  inherited Create(AOwner);

  if not (Owner is TForm) then
    raise Exception.Create('Control parent must be a form!');

  Instances := 0;
  for I := 0 to Owner.ComponentCount - 1 do
    if (Owner.Components[I] is TAdvMetroHint) then Inc(Instances);
  if (Instances > 1) then
    raise Exception.Create('Only one instance of TAdvMetroHint allowed on form');

  if not (csDesigning in ComponentState) then
  begin
    HintWindowClass := TMetroHintWindow;

    {$IFDEF DELPHI6_LVL}
    FApplicationEvents := TApplicationEvents.Create(self);
    with Application do
    begin
      ShowHint := not ShowHint;
      ShowHint := not ShowHint;
    end;
    FApplicationEvents.OnShowHint := GetHintInfo;
    {$ELSE}
    with Application do
    begin
      ShowHint := not ShowHint;
      ShowHint := not ShowHint;
      OnShowHint := GetHintInfo;
    end;
    {$ENDIF}
  end;

  FHintColor := $00F2BC00;
  FHintFont := TFont.Create;
  FHintFont.Style := [fsBold];
  FShadow := false;

  FHintFont.Name := GetMetroFont;
  FHintFont.Color := clWhite;
  FXMargin := 4;
  FYMargin := 2;
  FLineColor := clNone; //$00F2BC00;
  FLine := [hlTop];
  FHintStyle := hsRectangle;
end;

//------------------------------------------------------------------------------

destructor TAdvMetroHint.Destroy;
begin
  FHintFont.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroHint.GetHintInfo(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  HintInfo.HintColor := FHintColor;
  if MaxWidth <> 0 then
    HintInfo.HintMaxWidth := MaxWidth;
  if Assigned(FOnShowHint) then
    FOnShowHint(HintStr, CanShow, HintInfo);
  HintControl := HintInfo.HintControl;
  HintMaxWidth := HintInfo.HintMaxWidth;
  FHintInfo := HintInfo;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroHint.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroHint.SetColorTones(ATones: TColorTones);
begin
  FHintColor := ATones.Selected.BrushColor;
  FLineColor := ATones.BackGround.BrushColor;
  FHintFont.Name := GetMetroFont;
  FHintFont.Color := ATones.Selected.TextColor;
end;

procedure TAdvMetroHint.SetHintColor(const Value: TColor);
begin
  FHintColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvMetroHint.SetHintFont(const Value: TFont);
begin
  FHintFont.Assign(Value);
end;

//------------------------------------------------------------------------------

function TAdvMetroHint.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvMetroHint.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvMetroHint.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

{ TMetroHintWindow }

function TMetroHintWindow.FindHintControl: TAdvMetroHint;
var
  I: Integer;
begin
  Result := nil;

  if not Assigned(Application.MainForm) then
    Exit;

  with Application.MainForm do
  for I := 0 to ComponentCount-1 do
    if Components[I] is TAdvMetroHint then
    begin
      Result := TAdvMetroHint(Components[I]);
      Break;
    end;
end;

//------------------------------------------------------------------------------

procedure TMetroHintWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
var
  hnt: TAdvMetroHint;

begin
  inherited CreateParams(Params);


  hnt := FindHintControl;

  if Assigned(hnt) and not (hnt.Shadow) then
  begin
    Params.Style := Params.Style - WS_BORDER;

    if (Win32Platform = VER_PLATFORM_WIN32_NT) and
       ((Win32MajorVersion > 5) or
        ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
      Params.WindowClass.Style := Params.WindowClass.Style - CS_DROPSHADOW;
  end;
end;

//------------------------------------------------------------------------------

procedure TMetroHintWindow.Paint;
var
  DC: HDC;
  R, rd: TRect;
  Brush, SaveBrush: HBRUSH;
begin
  if not Assigned(FHint) then
    FHint := FindHintControl;

  if not Assigned(FHint) then
    Exit;

  DC := Canvas.Handle;
  R := ClientRect;
  RD := ClientRect;
  RD.Top := R.Top - 1;

  // Background
  Brush := CreateSolidBrush(ColorToRGB(FHint.FHintInfo.HintColor));
  SaveBrush := SelectObject(DC, Brush);
  FillRect(DC, R, Brush);
  SelectObject(DC, SaveBrush);
  DeleteObject(Brush);

  // Top line
  if (FHint.HintStyle = hsRectangle) and (FHint.LineColor <> clNone) then
  begin
    Canvas.Pen.Color := FHint.LineColor;

    if hlTop in FHint.Line then
    begin
      Canvas.MoveTo(R.Left, R.Top);
      Canvas.LineTo(R.Right, R.Top);
      RD.Top := R.Top + 1;
    end;

    if hlBottom in FHint.Line then
    begin
      Canvas.MoveTo(R.Left, R.Bottom - 1);
      Canvas.LineTo(R.Right, R.Bottom - 1);
      RD.Bottom := R.Bottom - 1;
    end;

    if hlLeft in FHint.Line then
    begin
      Canvas.MoveTo(R.Left, R.Top);
      Canvas.LineTo(R.Left, R.Bottom - 1);
      RD.Left := R.Left + 1;
    end;

    if hlRight in FHint.Line then
    begin
      Canvas.MoveTo(R.Right - 1, R.Top);
      Canvas.LineTo(R.Right - 1, R.Bottom - 1);
      RD.Right := R.Right - 1;
    end;
  end;

  Canvas.Font.Assign(FHint.HintFont);
  Canvas.Brush.Style := bsClear;

  if (Pos(#13,Caption) > 0) or (HintMaxWidth > 0) then
  begin
    RD.Top := RD.Top + 2;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), RD, DT_WORDBREAK or DT_TOP or DT_CENTER)
  end
  else
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), RD, DT_SINGLELINE or DT_VCENTER or DT_CENTER)
end;

//------------------------------------------------------------------------------

procedure TMetroHintWindow.ActivateHint(Rect: TRect; const AHint: string);
var
  dx, dy : Integer;
  Pnt: TPoint;
  R2: TRect;
  rgn:  THandle;
{$IFDEF DELPHI6_LVL}
  Monitor : TMonitor;
{$ENDIF}
begin
  Caption := AHint;
  FHint := FindHintControl;

  if not Assigned(FHint) then
    Exit;

  dx := 4;
  dy := 2;

  Canvas.Font.Assign(FHint.FHintFont);
  Color := FHint.FHintColor;

  with Rect do
  begin
    // Calculate width and height
    Rect.Right := {Rect.Left +} HintMaxWidth - dx;
    R2 := Classes.Rect(0, 0, Rect.Right, 100);
    DrawText(Canvas.Handle,PChar(Text),Length(Text), R2, DT_CALCRECT or DT_TOP or DT_LEFT or DT_WORDBREAK);

    FTextWidth := R2.Right + (2 * FHint.XMargin);
    Right := Left + FTextWidth + dx;

    FTextHeight := R2.Bottom + (2 * FHint.YMargin);
    Bottom := Top + FTextHeight + dy;

    // Calculate position
    Pnt := FHint.FHintinfo.HintPos;
    Left := Pnt.X;
    Top := Pnt.Y;
    Right := Right - Left + Pnt.X;
    Bottom := Bottom - Top + Pnt.Y;

    // Make sure the tooltip is completely visible

    {$IFDEF DELPHI6_LVL}
    Monitor := Screen.MonitorFromPoint(Pnt);

    if not Assigned(Monitor) then
      Monitor := Screen.Monitors[0];

    if Right - Monitor.Left > Monitor.Width then
    begin
      Left := Monitor.Left + Monitor.Width - Right + Left - 2;
      Right := Left + FTextWidth + dx;
    end;

    if Bottom - Monitor.Top > Monitor. Height then
    begin
      Bottom := Monitor.Top + Monitor. Height - 2;
      Top := Bottom - FTextHeight - dy;
    end;
    {$ELSE}
    if Right > Screen.DesktopWidth then
    begin
      Left := Screen.DesktopWidth - Right + Left -2;
      Right := Left + FTextWidth + dx;
    end;

    if Bottom > Screen.DesktopHeight then
    begin
      Bottom := Screen.DesktopHeight - 2;
      Top := Bottom - FTextHeight - dy;
    end;
    {$ENDIF}
  end;

  BoundsRect := Rect;

//  if not IsWindowVisible(Handle) then
//  begin

    if FHint.FHintStyle = hsRounded then
    begin
      rgn := CreateRoundRectRgn(0,0,Rect.Right - Rect.Left,Rect.Bottom - Rect.Top,HINTROUNDING,HINTROUNDING);
      if rgn > 0 then
      begin
        try
          SetWindowRgn(Handle,rgn,true);
        finally
          DeleteObject(rgn);
        end;
      end;
    end;

//  end;

  Pnt := ClientToScreen(Point(0, 0));
  SetWindowPos(Handle, HWND_TOPMOST, Pnt.X, Pnt.Y, 0, 0,
               SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);
end;

//------------------------------------------------------------------------------


end.
