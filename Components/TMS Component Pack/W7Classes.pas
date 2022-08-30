{***************************************************************************}
{ TMS W7 Controls Pack                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2011 - 2012                                        }
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

unit W7Classes;

interface

{$I TMSDEFS.INC}

uses
  Windows, Controls, Classes, Messages, W7Common, Graphics, Forms;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.

  // First release
  // 1.0.0.0 : First release as TMS W7 Controls

  // 1.0.1.0 : New : Enabled property for TW7TaskItem, TW7ListItem, TW7SpeedButton, TW7ToolButton
  // 1.0.1.1 : Fixed : Issue with changing Enabled property from OnClick event
  // 1.0.1.2 : Fixed : Issue with TabStop in TW7ToolButton
  // 1.0.2.0 : New : CaptionAlignment added for TW7ToolButton

type
  TW7VerticalAlignment = (wvaTop, wvaCenter, wvaBottom);
  TW7HorizontalAlignment = (whaLeft, whaCenter, whaRight);
  TW7ButtonIconSize = (is16px, is24px, is32px, is48px);

  TW7Control = class(TCustomControl)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    {$IFNDEF DELPHI2007_LVL}
    FMouseInClient: boolean;
    {$ENDIF}
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    function GetVersionNr: Integer;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    LeftButtonPressed: boolean;
    WindowsVersion: integer;
    InvalidateOnMouseEvents: boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    {$IFNDEF DELPHI2007_LVL}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFNDEF DELPHI2007_LVL}
    property MouseInClient: boolean read FMouseInClient;
    {$ENDIF}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;

  published
    property Version: string read GetVersion write SetVersion;
  end;

  TW7TransparentControl = class (TW7Control)
  protected
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure DrawBackground(Control: TControl; DC: HDC; InvalidateParent: Boolean);
    procedure Paint; override;
  end;

  TW7GraphicControl = class (TGraphicControl)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    LeftButtonPressed: boolean;
    MouseInControl: boolean;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

implementation

uses
  SysUtils;

constructor TW7Control.Create(AOwner: TComponent);
begin
  inherited;
  LeftButtonPressed := False;
  InvalidateOnMouseEvents := True;
  if GetWindowsVersion >= 6 then
  begin
    Font.Name := W7StandartFontName;
    Font.Size := W7StandartFontSize;
  end;
end;

destructor TW7Control.Destroy;
begin
  inherited;
end;

function TW7Control.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) +
    '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TW7Control.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;


procedure TW7Control.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TW7Control.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
  LeftButtonPressed := false;
end;

procedure TW7Control.CMMouseEnter(var Message: TMessage);
begin
  MouseEnter;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TW7Control.CMMouseLeave(var Message: TMessage);
begin
  MouseLeave;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TW7Control.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    LeftButtonPressed := True;
  Invalidate;
end;

procedure TW7Control.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  LeftButtonPressed := False;
  Invalidate;
end;

procedure TW7Control.SetVersion(const Value: string);
begin

end;

procedure TW7Control.MouseEnter;
begin
  if InvalidateOnMouseEvents then
    Invalidate;
end;

procedure TW7Control.MouseLeave;
begin
  if InvalidateOnMouseEvents then
    Invalidate;
{$IFNDEF DELPHI2007_LVL}
  FMouseInClient := False;
{$ENDIF}
end;

{$IFNDEF DELPHI2007_LVL}
procedure TW7Control.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  FMouseInClient := True;
end;
{$ENDIF}


procedure TW7Control.WMKillFocus(var Message: TWMKillFocus);
begin
  LeftButtonPressed := False;
  Invalidate;
end;

procedure TW7Control.WMSetFocus(var Message: TWMSetFocus);
begin
  Invalidate;
end;
///////////////////

procedure TW7TransparentControl.Paint;
begin
  DrawBackground(Self, Canvas.Handle, True);
end;

procedure TW7TransparentControl.DrawBackground(Control: TControl; DC: HDC; InvalidateParent: Boolean);
var
  SaveDCInd: Integer;
  Pos: TPoint;
begin
 if Control.Parent <> nil then
 begin
   SaveDCInd := SaveDC(DC);
   GetViewportOrgEx(DC, Pos);
   SetViewportOrgEx(DC, Pos.X - Control.Left, Pos.Y - Control.Top, nil);
   IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight);
   try
     Control.Parent.Perform(WM_EraseBkgnd, DC, 0);
     Control.Parent.Perform(WM_Paint, DC, 0);
   except
   end;
   RestoreDC(DC, SaveDCInd);
   if InvalidateParent and not (Control.Parent is TCustomControl) and not (Control.Parent is TCustomForm) and not (csDesigning in Control.ComponentState) then
     Control.Parent.Invalidate;
 end;
end;

procedure TW7TransparentControl.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

///////////////////////////////

constructor TW7GraphicControl.Create(AOwner: TComponent);
begin
  inherited;
  MouseInControl := False;
  LeftButtonPressed := False;
  if GetWindowsVersion >= 6 then
  begin
    Font.Name := W7StandartFontName;
    Font.Size := W7StandartFontSize;
  end;
end;

destructor TW7GraphicControl.Destroy;
begin
  inherited;
end;

procedure TW7GraphicControl.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TW7GraphicControl.CMMouseEnter(var Message: TMessage);
begin
  MouseEnter;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TW7GraphicControl.CMMouseLeave(var Message: TMessage);
begin
  MouseLeave;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TW7GraphicControl.MouseEnter;
begin
  MouseInControl := True;
end;

procedure TW7GraphicControl.MouseLeave;
begin
  MouseInControl := False;
end;

procedure TW7GraphicControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    LeftButtonPressed := True;
  Invalidate;
end;

procedure TW7GraphicControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  LeftButtonPressed := False;
  Invalidate;
end;
//////////////////////////////////


{$IFDEF FREEWARE}
function Scramble(s:string): string;
var
  r:string;
  i: integer;
  c: char;
  b: byte;
begin
  r := '';
  for i := 1 to length(s) do
  begin
    b := ord(s[i]);
    b := (b and $E0) + ((b and $1F) xor 5);
    c := chr(b);
    r := r + c;
  end;
  Result := r;
end;
{$ENDIF}

initialization
begin
{$IFDEF FREEWARE}
   if  (FindWindow(PChar(Scramble('QDuuilfdqljk')), nil) = 0) OR
       (FindWindow(PChar(Scramble('QDuuGplia`w')), nil) = 0) then
   begin
     MessageBox(0,PChar(Scramble('Duuilfdqljk%pv`v%qwldi%s`wvljk%jc%QHV%vjcqrdw`%fjhujk`kqv+')+#13#10+Scramble('Fjkqdfq%QHV%vjcqrdw`%mqqu?**rrr+qhvvjcqrdw`+fjh%cjw%sdila%ilf`kvlkb+')),PChar(Scramble('Rdwklkb')),MB_OK);
   end;
{$ENDIF}
end;




end.
