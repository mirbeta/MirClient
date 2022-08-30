{*************************************************************************}
{ TAdvDateTimePicker component                                            }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2007 - 2014                                       }
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

unit AdvDateTimePicker;

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
  MIN_VER = 2; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed issue with DB-aware version
  // v1.0.0.2 : Improved : force milliseconds to zero
  // v1.0.0.3 : Improved : position of internal datetimepickers with XP theming enabled
  // v1.0.0.4 : Fixed : issue with DB aware version
  // v1.0.0.5 : Fixed : issue with timeformat setting
  // v1.0.0.6 : Fixed : issue with use in VCL.NET
  // v1.0.0.7 : Fixed : issue with initializing time in older Delphi versions
  // v1.0.0.8 : Fixed : issue with setting Checked = false at design time
  // v1.0.1.0 : New : automatic handling of checkbox with DB-aware component for null dates
  // v1.0.2.0 : New : AutoTab : when true, tabbing between date & time entry is automatic
  // v1.0.2.1 : Fixed : issue with AutoTab
  // v1.0.3.0 : Improved : support to be used as timepicker only
  // v1.0.4.0 : Improved : painting for Vista / Win7 with standard theme
  // v1.1.0.0 : New : Label property added
  // v1.1.0.1 : Fixed : timepicker positioning on Windows 7
  // v1.1.0.2 : Fixed : issue with Checked property for time picker
  // v1.2.0.0 : New : support for flat style added
  // v1.2.0.1 : Fixed : issue with parent font handling for label
  // v1.2.0.2 : Fixed : issue with ShowCheckBox and ParseInput = True
  // v1.2.0.3 : Improved : small improvement for setting null dates
  // v1.2.0.4 : Fixed : Issue with dropdown & closeup event handlers
  // v1.2.0.5 : Improved : Focus handling when tabbing
  // v1.2.0.6 : Fixed : Issue with ShowCheckBox & AutoTab = true
  // v1.2.0.7 : Fixed : Issue with setting TabStop = false
  // v1.2.0.8 : Fixed : Issue with time when changing Kind at runtime
  // v1.2.1.0 : New : Label positions lpTopRight, lpBottomRight added
  //          : Improved : Label position calculation
  // v1.2.2.0 : New : AutoTabToNext property added


  DROPDOWNBTN_WIDTH = 21;

type
  TAdvDateTimeKind = (dkDate, dkTime, dkDateTime);
  TLabelPosition = (lpLeftTop, lpLeftCenter, lpLeftBottom, lpTopLeft, lpBottomLeft,
    lpLeftTopLeft, lpLeftCenterLeft, lpLeftBottomLeft, lpTopCenter, lpBottomCenter,
    lpRightTop, lpRightCenter, lpRighBottom, lpTopRight, lpBottomRight);

  {$IFNDEF DELPHI2007_LVL}
  TWMNotifyDT = packed record { TWMNotify }
    Msg: Cardinal;
    IDCtrl: Longint;
    case Integer of
      0: (NMHdr: PNMHdr);
      1: (NMDateTimeChange: PNMDateTimeChange);
      2: (NMDateTimeString: PNMDateTimeString;
          Result: LRESULT);
  end;
  {$ENDIF}

  TCustomDateTimePicker = class(TDateTimePicker)
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
    FAutoTab: Boolean;
    FAutoTabToNext: Boolean;
    Fh, Fm, Fs, Fms: Word;
    FIsTimePicker: boolean;
    FGridTabhandling: boolean;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
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
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure WndProc(var Message: TMessage); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure ShiftFocus; virtual;
    procedure SetAutoTab(const Value: Boolean); virtual;
    procedure SetAutoTabToNext(const Value: Boolean); virtual;
    function DoVisualStyles: Boolean;
    function IsInnerCtrl: Boolean; virtual;
    procedure SetBorderStyle(const Value: TBorderStyle); virtual;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth default 19;

    property ButtonColor: TColor read FButtonColor write FButtonColor default clNone;
    property ButtonColorHot: TColor read FButtonColorHot write FButtonColorHot default clNone;
    property ButtonColorDown: TColor read FButtonColorDown write FButtonColorDown default clNone;
    property ButtonTextColor: TColor read FButtonTextColor write FButtonTextColor default clNone;
    property ButtonTextColorHot: TColor read FButtonTextColorHot write FButtonTextColorHot default clNone;
    property ButtonTextColorDown: TColor read FButtonTextColorDown write FButtonTextColorDown default clNone;
    property ButtonBorderColor: TColor read FButtonBorderColor write FButtonBorderColor default clNone;
    property FocusBorderColor: TColor read FFocusBorderColor write FFocusBorderColor default clNone;
    property FocusBorder: Boolean read FFocusBorder write FFocusBorder default False;
    property DisabledBorder: Boolean read FDisabledBorder write FDisabledBorder default true;

    property MetroStyleShowing: Boolean read GetMetroStyleShowing;
    procedure TabToNext;
    property IsTimePicker: boolean read FIsTimePicker write FIsTimePicker;
    property GridTabHandling: boolean read FGridTabHandling write FGridTabHandling;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property MetroStyle: Boolean read FMetroStyle write SetMetroStyle;
  published
    property AutoTab: Boolean read FAutoTab write SetAutoTab default False;
    property AutoTabToNext: Boolean read FAutoTabToNext write SetAutoTabToNext default True;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvDateTimePicker = class(TCustomDateTimePicker)
  private
    FKind: TAdvDateTimeKind;
    FOnTimeChange: TNotifyEvent;
    FFocusTimer: TTimer;
    Fpt: TPoint;
    FTimeFormat: string;
    Fy, Fm, Fd: word;
    FLabel: TLabel;
    FLabelFont: TFont;
    FLabelPosition: TLabelPosition;
    FLabelMargin: Integer;
    FLabelTransparent: Boolean;
    FFocusLabel: Boolean;
    FLabelAlwaysEnabled: Boolean;
    FOnLabelClick: TNotifyEvent;
    FOnLabelDblClick: TNotifyEvent;
    FParentFnt: boolean;
    FIgnoreTimePickerCheck: Boolean;
    FFirstRun: Boolean;
    FNullDateIsToday: boolean;
    procedure OnTimePickerChanged(Sender: TObject);
    procedure OnTimePickerClicked(Sender: TObject);
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetKind(const Value: TAdvDateTimeKind);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CNNotify(var Message: TWMNotifyDt); message CN_NOTIFY;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure OnFocusTimerTime(Sender: TObject);
    function GetTimeEx: TTime;
    procedure SetTimeEx(const Value: TTime);
    function GetDateTimeEx: TDateTime;
    function GetFormatEx: String;
    procedure SetFormatEx(const Value: String);
    function GetTimeFormat: String;
    procedure SetTimeFormat(const Value: String);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure LabelFontChange(Sender: TObject);
    procedure SetLabelFont(const Value: TFont);
    procedure SetLabelCaption(const value: string);
    function GetLabelCaption: string;
    procedure SetLabelPosition(const value: TLabelPosition);
    procedure SetLabelMargin(const value: integer);
    procedure SetLabelTransparent(const value: boolean);
    procedure UpdateLabel;
    procedure UpdateLabelPos;
    procedure SetLabelAlwaysEnabled(const Value: Boolean);
    function GetDateEx: TDate;
    procedure SetDateEx(const Value: TDate);
    function GetTabStopEx: Boolean;
    procedure SetTabStopEx(const Value: Boolean);
  protected
    FTimePicker: TCustomDateTimePicker;
    procedure ShiftFocus; override;
    procedure SetAutoTab(const Value: Boolean); override;
    procedure SetAutoTabToNext(const Value: Boolean); override;
    procedure CreateTimePicker;
    procedure UpdateTimePicker;
    procedure Loaded; override;
    procedure SetBorderStyle(const Value: TBorderStyle); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure TimePickerChanged; virtual;
    procedure TimePickerClicked; virtual;
    procedure SetDateTimeEx(const Value: TDateTime); virtual;
    procedure TimePickerKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure TimePickerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    function CreateLabel: TLabel;
    procedure LabelClick(Sender: TObject);
    procedure LabelDblClick(Sender: TObject);
    function MsgSetDateTime(Value: TSystemTime): Boolean; override;
    procedure Change; override;
    procedure WndProc(var Message: TMessage); override;
    function IsInnerCtrl: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property BorderColor;
    function GetVersionNr: integer;

    property PickerLabel: TLabel read FLabel;
    property OnTimeChange: TNotifyEvent read FOnTimeChange write FOnTimeChange;

    property ButtonColor;
    property ButtonColorHot;
    property ButtonColorDown;
    property ButtonTextColor;
    property ButtonTextColorHot;
    property ButtonTextColorDown;
    property ButtonBorderColor;
    property NullDateIsToday: boolean read FNullDateIsToday write FNullDateIsToday;
  published
    property BorderStyle;
    property Ctl3D;
    property Date: TDate read GetDateEx write SetDateEx;
    property DateTime: TDateTime read GetDateTimeEx write SetDateTimeEx;
    property Format: String read GetFormatEx write SetFormatEx;
    property TimeFormat: String read GetTimeFormat write SetTimeFormat;
    property Kind: TAdvDateTimeKind read FKind write SetKind;
    property TabStop: Boolean read GetTabStopEx write SetTabStopEx default False;
    property Time: TTime read GetTimeEx write SetTimeEx;
    property Version: string read GetVersion write SetVersion;

    property FocusLabel: Boolean read FFocusLabel write FFocusLabel default false;
    property LabelCaption: string read GetLabelCaption write SetLabelCaption;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpLeftTop;
    property LabelMargin: Integer read FLabelMargin write SetLabelMargin default 4;
    property LabelTransparent: Boolean read FLabelTransparent write SetLabelTransparent default False;
    property LabelAlwaysEnabled: Boolean read FLabelAlwaysEnabled write SetLabelAlwaysEnabled default False;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property OnLabelClick: TNotifyEvent read FOnLabelClick write FOnLabelClick;
    property OnLabelDblClick: TNotifyEvent read FOnLabelDblClick write FOnLabelDblClick;
  end;

implementation

uses
  ComStrs
   , AdvXPVS;

var
  WM_DTPSHIFTFOCUS: Word;

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

function GetTextSize(WinCtrl: TWinControl; Text: string; font: TFont): TSize;
var
  Canvas: TCanvas;
  R: TRect;
begin
  Canvas := TCanvas.Create;
  Canvas.Handle := GetWindowDC(WinCtrl.Handle);
  Canvas.Font.Assign(font);

  R := Rect(0, 0, 1000, 200);
  DrawText(Canvas.Handle,PChar(Text),Length(Text), R, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
  Result.cx := R.Right - R.Left;
  Result.cy := R.Bottom - R.Top;
  ReleaseDC(WinCtrl.Handle, Canvas.Handle);
  Canvas.Free;
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
  Result := not(((GetAsyncKeyState(VK_RBUTTON)and $8000) = 0) and
     ((GetAsyncKeyState(VK_LBUTTON)and $8000) = 0));
end;

//------------------------------------------------------------------------------

{ TCustomDateTimePicker }

constructor TCustomDateTimePicker.Create(AOwner: TComponent);
var
  i: Integer;
  dwVersion:Dword;
  dwWindowsMajorVersion,dwWindowsMinorVersion:Dword;
begin
  inherited;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL) + 2;
  FMetroStyle := False;
  FIsTimePicker := False;
  DoubleBuffered := True;
  ParentCtl3D := False;
  Ctl3D := false;
  FMouseInControl := False;
  FBorderStyle := bsNone;
  FBorderColor := clBlack;
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

  dwVersion := GetVersion;
  dwWindowsMajorVersion :=  DWORD(LOBYTE(LOWORD(dwVersion)));
  dwWindowsMinorVersion :=  DWORD(HIBYTE(LOWORD(dwVersion)));

  FIsWinXP := (dwWindowsMajorVersion > 5) OR
    ((dwWindowsMajorVersion = 5) AND (dwWindowsMinorVersion >= 1));

  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsThemed := (i > 5);
  //CalExceptionClass := nil;
  FAutoTab := False;
  FAutoTabToNext := True;
  FGridTabhandling := False;
end;

//------------------------------------------------------------------------------

destructor TCustomDateTimePicker.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

function TCustomDateTimePicker.DoVisualStyles: Boolean;
begin
  if FIsThemed then
    Result := IsThemeActive
  else
    Result := False;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.DrawButtonBorder(DC: HDC);
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

procedure TCustomDateTimePicker.DrawControlBorder(DC: HDC);
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

procedure TCustomDateTimePicker.DrawBorders;
var
  DC: HDC;
begin
  if IsInnerCtrl or not MetroStyleShowing or (Enabled and not ((FBorderColor <> clNone) or (FFocusBorderColor <> clNone))) then
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

procedure TCustomDateTimePicker.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FHasFocus := true;
    DrawBorders;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.CMExit(var Message: TCMExit);
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    FHasFocus := false;
    DrawBorders;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.CMMouseEnter(var Message: TMessage);
begin
  inherited;

  if not FMouseInControl and Enabled then
    begin
     FMouseInControl := True;
     DrawBorders;
    end;

  if FIsWinXP then
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.CMMouseLeave(var Message: TMessage);
begin
  inherited;

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

function TCustomDateTimePicker.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);
end;

//------------------------------------------------------------------------------

function TCustomDateTimePicker.Is3DBorderControl: Boolean;
begin
  if csDesigning in ComponentState then
    Result := False
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);

  Result := Result and FFocusBorder;
end;

//------------------------------------------------------------------------------

function TCustomDateTimePicker.IsInnerCtrl: Boolean;
begin
  Result := True;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.CreateWnd;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.NCPaintProc;
var
  DC: HDC;
  //WindowBrush:hBrush;
  Canvas: TCanvas;

begin
  if Ctl3D then
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

procedure TCustomDateTimePicker.WMNCPaint(var Message: TMessage);
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

procedure TCustomDateTimePicker.WMPaint(var Message: TWMPaint);
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
            FillRect(DC, Rect(ClientRect.Right - FButtonWidth - 4, 1, ClientRect.Right, Height), Brush.Handle)
          else
            FillRect(DC, Rect(ClientRect.Right - FButtonWidth - 2, 4, ClientRect.Right - 2, Height - 4), Brush.Handle);
        end;
        DrawButton;
      end;
    end;

    if not Ctl3D then
    begin
      if (BorderStyle = bsNone) and (Parent is TWinControl) then
        ACanvas.Pen.Color := (Parent as TWinControl).Brush.Color
      else
      begin
        if FIsThemed then
          ACanvas.Pen.Color := $B99D7F
        else
          ACanvas.Pen.Color := BorderColor;
      end;

      ACanvas.MoveTo(0,Height);
      ACanvas.LineTo(0,0);
      ACanvas.LineTo(Width - 1,0);
      ACanvas.LineTo(Width - 1,Height - 1);
      ACanvas.LineTo(0,Height-1);

      {$IFDEF DELPHI7_LVL}
      if IsVista and not IsThemeActive then  // for Windows standard scheme
      begin
        ACanvas.MoveTo(1,Height);
        ACanvas.LineTo(1,1);
        ACanvas.LineTo(Width - 2, 1);
        ACanvas.LineTo(Width - 2, Height - 2);
        ACanvas.LineTo(0 ,Height-2);
      end;
      {$ENDIF}
    end;
  finally
    ACanvas.Free;
    ReleaseDC(Handle,DC);
  end;

  DrawBorders;
  end;

//------------------------------------------------------------------------------
procedure TCustomDateTimePicker.WMSize(var Message: TWMSize);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.WndProc(var Message: TMessage);
var
  lp: dword;
begin
  if (Message.Msg = WM_DTPSHIFTFOCUS) then
    ShiftFocus;

  if AutoTab and (Message.Msg = WM_SETFOCUS) then
  begin
    if ShowCheckBox then
      lp := MakeLParam(24,2)
    else
      lp := MakeLParam(2,2);
    PostMessage(Handle, WM_LBUTTONDOWN,0,lp);
  end;

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

procedure TCustomDateTimePicker.KeyPress(var Key: Char);
begin
  if (Integer(Key) in [48..57]) and AutoTab then
  begin
    if (Kind = dtkTime) then
      DecodeTime(DateTime, Fh, Fm, Fs, Fms);
    inherited;
    PostMessage(Handle, WM_DTPSHIFTFOCUS, 0, 0);
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TCustomDateTimePicker.WMGetDlgCode(var Message: TMessage);
var
  isShift: boolean;
begin
  inherited;

  if FGridTabHandling then
  begin
    isShift := (GetKeyState(VK_SHIFT) and $8000 = $8000);

    if (IsTimePicker and not isShift) or (not IsTimePicker and isShift) then
    begin
      Message.Result := Message.Result or DLGC_WANTTAB
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.ShiftFocus;
var
  h, m, s, ms: word;
begin
  if (Kind = dtkTime) then
  begin
    DecodeTime(DateTime, h, m, s, ms);

    if (h <> Fh) or (m <> Fm) or (s <> Fs) then
    begin
      if (h = Fh) and (m = Fm) and FAutoTabToNext then
      begin
        TabToNext;
      end
      else
        PostMessage(Handle, WM_KEYDOWN, VK_RIGHT, 0);
    end;
  end;
end;

procedure TCustomDateTimePicker.TabToNext;
var
  cf: TCustomForm;
begin
  if IsWindowVisible(Handle) then
  begin
    cf := GetParentForm(self);
    if Assigned(cf) then
      cf.Perform(WM_NEXTDLGCTL, 0, 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.SetBorderStyle(const Value: TBorderStyle);
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

procedure TCustomDateTimePicker.SetButtonWidth(const Value: integer);
begin
  if (value < 14) or (value > 32) then
    Exit;

  FButtonWidth := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.SetMetroStyle(const Value: Boolean);
begin
  if (FMetroStyle <> Value) then
  begin
    FMetroStyle := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TCustomDateTimePicker.GetMetroStyleShowing: Boolean;
begin
  Result := MetroStyle and not IsInnerCtrl;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.SetAutoTab(const Value: Boolean);
begin
  FAutoTab := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.SetAutoTabToNext(const Value: Boolean);
begin
  FAutoTabToNext := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomDateTimePicker.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

{ TAdvDateTimePicker }

procedure TAdvDateTimePicker.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FTimePicker) then
    FTimePicker.Enabled := Enabled;

  if Assigned(FLabel) then
    if not FLabelAlwaysEnabled then
      FLabel.Enabled := Enabled;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.CMFontChanged(var Message: TMessage);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    if (FLabel <> nil) and ParentFont then
    begin
      FLabel.Font.Assign(self.font);
    end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if (FLabel <> nil) then FLabel.Visible := Visible;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if (FLabel <> nil) then FLabel.Visible := Visible;
end;

//------------------------------------------------------------------------------

constructor TAdvDateTimePicker.Create(AOwner: TComponent);
begin
  inherited;
  {
  if (inherited Kind = dtkDate) then
    FKind := dkDateTime
  else
    FKind := dkTime;
  }
  FLabel := nil;
  FLabelMargin := 4;
  FTimePicker := nil;
  FKind := dkDateTime;
  //CreateTimePicker;
  BorderStyle := bsSingle;
  Ctl3D := true;
  FFocusTimer := TTimer.Create(Self);
  FFocusTimer.Enabled := False;
  FFocusTimer.Interval := 20;
  FFocusTimer.OnTimer := OnFocusTimerTime;
  FLabelFont := TFont.Create;
  FLabelFont.OnChange := LabelFontChange;
  FParentFnt := false;
  FFirstRun := True;
  FMetroStyle := False;
  FNullDateIsToday := false;

end;

//------------------------------------------------------------------------------

function TAdvDateTimePicker.CreateLabel: TLabel;
begin
  Result := Tlabel.Create(self);
  Result.Parent := self.Parent;
  Result.FocusControl := self;
  Result.Font.Assign(LabelFont);
  Result.OnClick := LabelClick;
  Result.OnDblClick := LabelDblClick;
  Result.ParentFont := self.ParentFont;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.CreateTimePicker;
begin
  if not Assigned(FTimePicker) and (FKind = dkDateTime) then
  begin
    FTimePicker := TCustomDateTimePicker.Create(Self);
    if not ((csLoading in ComponentState) and (csDesigning in ComponentState)) then
      FTimePicker.Parent := Self;
    FTimePicker.Width := 90;
    FTimePicker.Height := 17;
    FTimePicker.IsTimePicker := true;
    FTimePicker.Visible := False;
    FTimePicker.Enabled := Enabled;
    FTimePicker.Kind := dtkTime;
    FTimePicker.OnChange := OnTimePickerChanged;
    FTimePicker.OnClick := OnTimePickerClicked;
    FTimePicker.OnKeyPress := TimePickerKeyPress;
    FTimePicker.OnKeyDown := TimePickerKeyDown;
    FTimePicker.AutoTabToNext := FAutoTabToNext;
    FTimePicker.GridTabHandling := GridTabHandling;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.CreateWnd;
var
  oldKind: TAdvDateTimeKind;
begin
  inherited;
  oldKind := FKind;
  FKind := dkDate;
  Kind := oldKind;
  if Assigned(FLabel) then
    UpdateLabelPos;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.UpdateLabelPos;
var
  tw,brdr: Integer;
  r: TRect;
begin
  r := Rect(0,0,1000,255);
  DrawText(FLabel.Canvas.Handle, PChar(FLabel.Caption), Length(FLabel.Caption), r, DT_HIDEPREFIX or DT_CALCRECT);
  tw := r.Right;

  brdr := 0;
  if BorderStyle = bsSingle then
    brdr := 2;


  case FLabelPosition of
    lpLeftTop:
      begin
        FLabel.Top := self.Top;
        FLabel.Left := self.Left - tw - FLabelMargin;
      end;
    lpLeftCenter:
      begin
        if Self.Height > FLabel.Height then
          FLabel.Top := Top + ((Height - brdr - FLabel.Height) div 2)
        else
          FLabel.Top := Top - ((FLabel.Height - Height + brdr) div 2);

        FLabel.Left := Left - tw - FLabelMargin;
      end;
    lpLeftBottom:
      begin
        FLabel.Top := self.Top + self.Height - FLabel.Height;
        FLabel.Left := self.Left - tw - FLabelMargin;
      end;
    lpTopLeft:
      begin
        FLabel.Top := self.Top - FLabel.Height - FLabelMargin;
        FLabel.Left := self.Left;
      end;
    lpTopRight:
      begin
        FLabel.Top := self.Top - FLabel.Height - FLabelMargin;
        FLabel.Left := self.Left + self.Width - FLabel.Width;
      end;
    lpTopCenter:
      begin
        FLabel.Top := self.Top - FLabel.height - FLabelMargin;
        if self.Width - FLabel.Width > 0 then
          FLabeL.Left := self.Left + ((self.Width - FLabel.Width) div 2)
        else
          FLabeL.Left := self.Left - ((FLabel.Width - self.Width) div 2)
      end;
    lpBottomLeft:
      begin
        FLabel.top := self.top + self.height + FLabelMargin;
        FLabel.left := self.left;
      end;
    lpBottomCenter:
      begin
        FLabel.top := self.top + self.height + FLabelMargin;
        if self.Width - FLabel.Width > 0 then
          FLabeL.Left := self.Left + ((self.Width - FLabel.width) div 2)
        else
          FLabeL.Left := self.Left - ((FLabel.Width - self.width) div 2)
      end;
    lpBottomRight:
      begin
        FLabel.top := self.top + self.height + FLabelMargin;
        FLabel.Left := self.Left + self.Width - FLabel.Width;
      end;
    lpLeftTopLeft:
      begin
        FLabel.top := self.top;
        FLabel.left := self.left - FLabelMargin;
      end;
    lpLeftCenterLeft:
      begin
        if Self.Height > FLabel.Height then
          FLabel.Top := self.top + ((Height - brdr - FLabel.height) div 2)
        else
          FLabel.Top := self.Top - ((FLabel.Height - Height + brdr) div 2);
        FLabel.left := self.left - FLabelMargin;
      end;
    lpLeftBottomLeft:
      begin
        FLabel.top := self.top + self.height - FLabel.height;
        FLabel.left := self.left - FLabelMargin;
      end;
    lpRightTop:
      begin
        FLabel.Top := self.Top;
        FLabel.Left := self.Left + Self.Width + FLabelMargin;
      end;
    lpRightCenter:
      begin
        if Self.Height > FLabel.Height then
          FLabel.Top := Top + ((Height - brdr - FLabel.Height) div 2)
        else
          FLabel.Top := Top - ((FLabel.Height - Height + brdr) div 2);

        FLabel.Left := self.Left + Self.Width + FLabelMargin;
      end;
    lpRighBottom:
      begin
        FLabel.Top := self.Top + self.Height - FLabel.Height;
        FLabel.Left := self.Left + Self.Width + FLabelMargin;
      end;
  end;

  FLabel.Visible := Visible;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.UpdateLabel;
begin
  if Assigned(FLabel.Parent) then
  begin
    FLabel.Transparent := FLabeltransparent;

    if not FParentFnt then
    begin
      FLabel.Font.Assign(FLabelFont);
    end
    else
      FLabel.Font.Assign(self.Font);

    if FLabel.Parent.HandleAllocated then
      UpdateLabelPos;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.UpdateTimePicker;
var
  i,j: Integer;
  lpstr: array[0..255] of char;
begin
  if Assigned(FTimePicker) then
  begin
    if (FTimePicker.Parent <> Self) then
      FTimePicker.Parent := Self;
    FTimePicker.Color := Self.Color;
    FTimePicker.Enabled := Self.Enabled and (checked or not ShowCheckbox);
    FTimePicker.Visible := (FKind = dkDateTime);
    FTimePicker.Time := Self.Time;
    FTimePicker.DateTime := Self.DateTime;
    FTimePicker.Format := Self.TimeFormat;
    FTimePicker.Font.Assign(Self.Font);

    if not FTimePicker.Visible and (not (csLoading in ComponentState) or (csDesigning in ComponentState)) then
    begin
      FTimePicker.Free;
      FTimePicker := nil;
    end
    else
    begin
      GetWindowText(FTimepicker.Handle, lpstr,255);
      i := GetTextSize(Self, strpas(lpstr), Font).cx + DROPDOWNBTN_WIDTH + 10;

      if IsVista then
      begin
        //FTimePicker.SetBounds(Width - i - DROPDOWNBTN_WIDTH - 13, 0, i, Height)
        j := 0;
        if ((BevelInner <> bvNone) or (BevelOuter <> bvNone)) and (BevelKind <> bkNone) then
          j := 4;

        //ThemeControl(Self);

        if IsComCtl6
        {$IFDEF DELPHI7_LVl}
        and Assigned(IsThemeActive)
        {$ENDIF}
        then
        begin
          {$IFDEF DELPHI7_LVl}
          if IsThemeActive then
          begin
          {$ENDIF}
            if MetroStyleShowing then
              FTimePicker.SetBounds(Width - i - DROPDOWNBTN_WIDTH - j, 1, i, Height - 2 - j)
            else
              FTimePicker.SetBounds(Width - i - DROPDOWNBTN_WIDTH - 13 - j, 1, i, Height - 2 - j);
          {$IFDEF DELPHI7_LVl}
          end
          else  // for Windows standard scheme
            FTimePicker.SetBounds(Width - i - DROPDOWNBTN_WIDTH - j, 2, i, Height - j - 4);
          {$ENDIF}
        end
        else
          FTimePicker.SetBounds(Width - i - DROPDOWNBTN_WIDTH - 2 - j, -2, i, Height - j)
      end
      else
      begin
        if Ctl3D then
          FTimePicker.SetBounds(Width - i - DROPDOWNBTN_WIDTH, -2, i, Height)
        else
          FTimePicker.SetBounds(Width - i - DROPDOWNBTN_WIDTH, -2, i, Height);
      end;  
    end;
  end;
end;

//------------------------------------------------------------------------------

destructor TAdvDateTimePicker.Destroy;
begin
  FFocusTimer.Free;
  if (FLabel <> nil) then
  begin
    FLabel.Parent := nil;
    FLabel.Free;
    FLabel := nil;
  end;
  FLabelFont.Free;

  if Assigned(FTimePicker) then
    FTimePicker.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetKind(const Value: TAdvDateTimeKind);
var
  fdt: TDateTime;
begin
  if (FKind <> Value) then
  begin
    FKind := Value;
    if (FKind = dkTime) then
      inherited Kind := dtkTime
    else
      inherited Kind := dtkDate;

    if (FKind = dkDateTime) then
    begin
      fdt := DateTime;
      CreateTimePicker;
      Datetime := fdt;
    end;

    UpdateTimePicker;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetLabelAlwaysEnabled(const Value: Boolean);
begin
  FLabelAlwaysEnabled := Value;
  if FLabel <> nil then
    if Value then
      FLabel.Enabled := True;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetLabelCaption(const value: string);
begin
  if FLabel = nil then
    FLabel := CreateLabel;
  FLabel.Caption := Value;
  UpdateLabel;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetLabelFont(const Value: TFont);
begin
  if not ParentFont then
    FLabelFont.Assign(Value);

  if FLabel <> nil then
    UpdateLabel;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetLabelMargin(const value: integer);
begin
  FLabelMargin := Value;
  if FLabel <> nil then UpdateLabel;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetLabelPosition(const value: TLabelPosition);
begin
  FLabelPosition := Value;
  if FLabel <> nil then UpdateLabel;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetLabelTransparent(const value: boolean);
begin
  FLabelTransparent := Value;
  if FLabel <> nil then UpdateLabel;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetParent(AParent: TWinControl);
begin
  inherited;
  if FLabel <> nil then
    FLabel.Parent := AParent;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.ShiftFocus;
var
  y, m, d: word;
  c: char;
begin
  if (FKind = dkTime) then
    inherited
  else
  begin
    DecodeDate(DateTime, y, m, d);
    if (Format <> '') then
    begin
      c := Format[Length(Format)];

      {$IFDEF DELPHI_UNICODE}
      if ((d <> Fd) and (CharInSet(c,['D','d']))) or ((m <> Fm) and (CharInSet(c,['M','m']))) or ((y <> Fy) and (CharInSet(c,['Y','y']))) then
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      if ((d <> Fd) and (c in ['D','d'])) or ((m <> Fm) and (c in ['M','m'])) or ((y <> Fy) and (c in ['Y','y'])) then
      {$ENDIF}
      begin
        if Assigned(FTimePicker) then
          FTimePicker.SetFocus;
      end
      else
        if (d <> Fd) or (m <> Fm) or (y <> Fy) then
          PostMessage(Handle, WM_KEYDOWN, VK_RIGHT, 0);
    end
    else
    begin
      if (d <> Fd) or (m <> Fm) then
        PostMessage(Handle, WM_KEYDOWN, VK_RIGHT, 0)
      else if (y <> Fy) then
      begin
        if Kind = dkDate then
        begin
          TabToNext;
        end
        else
          if Assigned(FTimePicker) then
            FTimePicker.SetFocus;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.KeyPress(var Key: Char);
begin
  if (Integer(Key) in [48..57]) and AutoTab then
  begin
    DecodeDate(DateTime, Fy, Fm, Fd);
    inherited;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.WMKillFocus(var Msg: TWMKillFocus);
begin
  if (csLoading in ComponentState) then
  begin
    inherited;
    Exit;
  end;

  if fFocusLabel and (FLabel <> nil) then
    FLabel.Font.Style := FLabel.Font.Style - [fsBold];

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if FFirstRun then
  begin
    FFirstRun := False;
    UpdateTimePicker;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if (csLoading in ComponentState) then
    Exit;
  
  if FFocusLabel and (FLabel <> nil) then
    FLabel.Font.Style := FLabel.Font.Style + [fsBold];
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateTimePicker;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.WndProc(var Message: TMessage);
begin
  inherited;

  if ShowCheckbox and not (csDesigning in ComponentState) and (Message.Msg = DTM_SETSYSTEMTIME) and (Message.WParam = GDT_NONE) then
  begin
    if Assigned(FTimePicker) then  // update timepicker on check change
    begin
      FTimePicker.Enabled := Enabled and Checked;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvDateTimePicker.GetTabStopEx: Boolean;
begin
  Result := inherited TabStop;
end;

function TAdvDateTimePicker.GetTimeEx: TTime;
begin
  Result := inherited Time;
  if (FKind = dkDateTime) and Assigned(FTimePicker) then
    Result := FTimePicker.Time;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetTabStopEx(const Value: Boolean);
begin
  inherited TabStop := Value;

  if Assigned(FTimePicker) then
    FTimePicker.TabStop := Value;
end;

procedure TAdvDateTimePicker.SetTimeEx(const Value: TTime);
begin
  inherited Time := Value;
  UpdateTimePicker;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetAutoTab(const Value: Boolean);
begin
  inherited;
  if Assigned(FTimePicker) then
    FTimePicker.AutoTab := AutoTab;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetAutoTabToNext(const Value: Boolean);
begin
  inherited;
  if Assigned(FTimePicker) then
    FTimePicker.AutoTabToNext := AutoTab;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetBorderStyle(const Value: TBorderStyle);
begin
  inherited;
  UpdateTimePicker;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if Assigned(FLabel) then
  begin
    case LabelPosition of
      lpLeftTop, lpLeftCenter, lpLeftBottom:
        begin
          if (Align in [alTop, alClient, alBottom]) then
          begin
            AWidth := AWidth - (FLabel.Width + LabelMargin);
            ALeft := ALeft + (FLabel.Width + LabelMargin);
          end;
        end;
      lpRightTop, lpRightCenter, lpRighBottom:
        begin
          if (Align in [alTop, alClient, alBottom]) then
            AWidth := AWidth - (FLabel.Width + LabelMargin);
        end;
      lpTopLeft, lpTopCenter, lpTopRight:
        begin
          if (Align in [alTop, alClient, alRight, alLeft]) then
            ATop := ATop + FLabel.Height;
        end;
    end;
  end;

  inherited;

  if (FLabel <> nil) then
  begin
    if FLabel.Parent <> nil then
      UpdateLabel;
  end;  
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.OnFocusTimerTime(Sender: TObject);
var
  pt2: TPoint;
begin
  FFocusTimer.Enabled := False;
  Exit;
  if ShowCheckbox and not DroppedDown then
  begin
    //if GetCaretPos(pt) then
    begin
      FPt := ClientToScreen(FPt);
      FPt.x := Round(FPt.x * (65535 / Screen.Width));
      FPt.y := Round(FPt.y * (65535 / Screen.Height));

      // keeping old mouse pos
      GetCursorPos(pt2);

      // mouse move
      Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE, FPt.x, FPt.y, 0, 0) ;

      // left mouse button down
      Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, FPt.x, FPt.y, 0, 0);
      // left mouse button Up
      Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, FPt.x, FPt.y, 0, 0) ;

      // moving mouse to old pos
      pt2.x := Round(Pt2.x * (65535 / Screen.Width)) ;
      Pt2.y := Round(Pt2.y * (65535 / Screen.Height)) ;
      Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE, Pt2.x, Pt2.y, 0, 0);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvDateTimePicker.MsgSetDateTime(Value: TSystemTime): Boolean;
begin
  Result := inherited MsgSetDateTime(Value);
  if ShowCheckbox and Assigned(FTimePicker) then
  begin
    FTimePicker.Enabled := Enabled and Checked;
    FIgnoreTimePickerCheck := FTimePicker.Enabled;
  end;
end;

procedure TAdvDateTimePicker.CNNotify(var Message: TWMNotifyDT);
var
  y, y1, m, m1, d, d1: word;
  DT: TDateTime;
  AllowChange: Boolean;
  UserString: string;
  DateTimeString: PNMDateTimeString;

begin
  if ShowCheckbox then
  begin
    with Message, NMHdr^ do
    begin
      case code of
        DTN_CLOSEUP: inherited;        
        DTN_DROPDOWN: inherited;

        DTN_DATETIMECHANGE:
        begin
          //GetCaretPos(Fpt);  //invalid pos
          Fpt := Point(-1, -1);
          DecodeDate(DateTime, y, m, d);
          inherited;
          DecodeDate(DateTime, y1, m1, d1);
          if (y <> y1) then
            Fpt := Point(80, 5)
          else if (m <> m1) then
            Fpt := Point(46, 5)
          else if (d <> d1) then
            Fpt := Point(28, 5);
          if (Fpt.X <> -1) then
            FFocusTimer.Enabled := True;

          if Assigned(FTimePicker) and not FIgnoreTimePickerCheck then
          begin
            FTimePicker.Enabled := Enabled and Checked;
            with NMDateTimeChange{$IFNDEF CLR}^{$ENDIF} do
              if (dwFlags = GDT_NONE) then
                FTimePicker.Enabled := False;
          end;
        end;

        DTN_USERSTRINGA, DTN_USERSTRINGW:
        begin
          DateTimeString := NMDateTimeString;
          with DateTimeString^ do
          begin
            UserString := pszUserString;
            AllowChange := TryStrToDateTime(UserString, DT);
            if Assigned(OnUserInput) then
            begin
              OnUserInput(Self, UserString, DT, AllowChange);
              if AllowChange then
                dwFlags := GDT_VALID
              else
                dwFlags := Cardinal(GDT_ERROR);
            end
            else
              dwFlags := GDT_VALID;
            DateTimeToSystemTime(DT, st);
          end;
        end;

      end;
    end;
    FIgnoreTimePickerCheck := False;
  end
  else
    inherited;
end;

procedure TAdvDateTimePicker.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) and ParentFont then
  begin
    FLabel.Font.Assign(Font);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.Change;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.CMColorChanged(var Message: TMessage);
begin
  inherited;
  UpdateTimePicker;
end;

//------------------------------------------------------------------------------

function TAdvDateTimePicker.GetDateTimeEx: TDateTime;
begin
  if Assigned(FTimePicker) then
    inherited Time := FTimePicker.Time;
  Result := inherited DateTime;
end;

//------------------------------------------------------------------------------

function TAdvDateTimePicker.GetDateEx: TDate;
begin
  Result := inherited Date;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetDateEx(const Value: TDate);
var
  ho,mi,se,se100:word;
begin
  DecodeTime(DateTime, ho, mi, se, se100);
  DateTime := Int(Value) + encodetime(ho,mi,se,0);;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetDateTimeEx(const Value: TDateTime);
var
  ho,mi,se,se100:word;
begin
  DecodeTime(value, ho, mi, se, se100);

  if (int(Value) = 0) and FNullDateIsToday then
    inherited DateTime := int(Now) + + encodetime(ho,mi,se,0)
  else
    inherited DateTime := int(Value) + encodetime(ho,mi,se,0);

  CreateTimePicker;
  if Assigned(FTimePicker) then
    FTimePicker.DateTime := int(value) + encodetime(ho,mi,se,0);
end;

//------------------------------------------------------------------------------

function TAdvDateTimePicker.GetLabelCaption: string;
begin
  if FLabel <> nil then
    Result := FLabel.Caption
  else
    Result := '';
end;

//------------------------------------------------------------------------------

function TAdvDateTimePicker.GetFormatEx: String;
begin
  Result := inherited Format;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetFormatEx(const Value: String);
begin
  inherited Format := Value;
end;

//------------------------------------------------------------------------------

function TAdvDateTimePicker.GetTimeFormat: String;
begin
  (*{$IFDEF DELPHI6_LVL}
  if Assigned(FTimePicker) then
    Result := FTimePicker.Format;
  {$ELSE}
  Result := '';
  {$ENDIF}*)
  Result := FTimeFormat;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetTimeFormat(const Value: String);
begin
  if Assigned(FTimePicker) then
    FTimePicker.Format := Value;

  if (Kind = dkTime) then
    Format := Value;

  FTimeFormat := Value;
  UpdateTimePicker;
end;

//------------------------------------------------------------------------------

function TAdvDateTimePicker.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvDateTimePicker.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

function TAdvDateTimePicker.IsInnerCtrl: Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.LabelClick(Sender: TObject);
begin
  if Assigned(FOnLabelClick) then
    FOnLabelClick(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.LabelDblClick(Sender: TObject);
begin
  if Assigned(FOnLabelDblClick) then
    FOnLabelDblClick(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.LabelFontChange(Sender: TObject);
begin
  if FLabel <> nil then
  begin
    UpdateLabel;
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      ParentFont := false;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    CreateTimePicker;
    UpdateTimePicker;
    if not Checked then
      Checked := false;

    if Assigned(FTimePicker) and not Checked then
      FTimePicker.Enabled := False;
  end;

  if not LabelAlwaysEnabled and Assigned(FLabel) then
    FLabel.Enabled := Enabled;

  if (FLabel <> nil) then
    UpdateLabel;

  if Self.ParentFont and Assigned(FLabel) then
  begin
    FLabel.Font.Assign(Font);
  end;

  FParentFnt := self.ParentFont;

  if (FLabel <> nil) then
    UpdateLabel;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.OnTimePickerChanged(Sender: TObject);
begin
  TimePickerChanged;
  if Assigned(OnTimeChange) then
    OnTimeChange(self);
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.OnTimePickerClicked(Sender: TObject);
begin
  TimePickerClicked;
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.TimePickerChanged;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.TimePickerClicked;
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.TimePickerKeyPress(Sender: TObject;
  var Key: Char);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvDateTimePicker.TimePickerKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key,Shift);
end;

//------------------------------------------------------------------------------

{$IFDEF FREEWARE}
{$I TRIALINIT.INC}
{$ENDIF}

initialization
  WM_DTPSHIFTFOCUS := RegisterWindowMessage('DTPShiftFocus');

{$IFDEF FREEWARE}
  DoTrial;
{$ENDIF}

end.
