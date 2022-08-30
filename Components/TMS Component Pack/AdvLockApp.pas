{***************************************************************************}
{ TAdvLockApp component                                                     }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012                                               }
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

unit AdvLockApp;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, Graphics, Controls, Forms, Dialogs, Sysutils, Messages,
  ExtCtrls;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 7; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Issue with capability to restore from task manager
  // v1.0.0.2 : Improved : support for apps with active modal forms
  // v1.0.0.3 : Fixed : Issue with updating IdleMinutes at runtime
  // v1.0.0.4 : Fixed : Issue with OnQueryUnlock event handler
  // v1.0.0.5 : Fixed : Issue with OnQueryUnlock event handler in SubclassProc, HookProc
  // v1.0.0.6 : Fixed : Issue with opening from secondary modal dialogs
  // v1.0.0.7 : Fixed : Rare issue that could cause incorrect lock

type

  TLockDialog = class(TPersistent)
  private
    FLabelPassword: string;
    FLabelUserName: string;
    FCaption: string;
  protected
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
  published
    property Caption: string read FCaption write FCaption;
    property LabelUsername: string read FLabelUserName write FLabelUserName;
    property LabelPassword: string read FLabelPassword write FLabelPassword;
  end;

  TVerifyPasswordEvent = procedure(Sender: TObject; Password: string; var Valid: boolean) of object;
  TQueryUnlockEvent = procedure(Sender: TObject; var AllowUnlock: boolean) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvLockApp = class(TComponent)
  private
    FLocked: Boolean;
    FLockedIcon: TIcon;
    FUnLockedIcon: TIcon;
    FPassword: string;
    FUserName: string;
    FEnabled: boolean;
    FSeconds: integer;
    FOnIdle: TIdleEvent;
    FActiveForm: TForm;
    FModalActiveForm: TForm;
    Ftimer: TTimer;
    FIncorrectPW: Boolean;
    FIsWin7: Boolean;
    FFormWndProc: TWndMethod;
    FModalFormWndProc: TWndMethod;

    FAppIdle: TIdleEvent;
    FAppMessage: TMessageEvent;
    FOnLock: TNotifyEvent;
    FOnUnLock: TNotifyEvent;
    FDialog: TLockDialog;
    FOnVerifyPassword: TVerifyPasswordEvent;
    FOnIncorrectPassword: TNotifyEvent;
    FIncorrectPassword: string;
    FOnQueryUnlock: TQueryUnlockEvent;
    FActivatingHack: Boolean;
    FIsWinXP: Boolean;
    FIsD2k7Lvl: Boolean;
    FAFRect: TRect;
    procedure DoIdle (Sender: TObject; var Done: Boolean);
    procedure DoMessage(var Msg: TMsg; var Handled: Boolean);

    procedure UnlockPWDlg;
    procedure DoLockUnlock;
    procedure SetLockedIcon (Value: TIcon);
    procedure SetUnLockedIcon (Value: TIcon);
    procedure SetMinutes (Value: integer);
    function  GetMinutes: integer;
    procedure SetEnabled (Value: boolean);
    procedure OnTimer (Sender: TObject);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetActiveForm;
    procedure SetSeconds(const Value: integer);
    procedure SetDialog(const Value: TLockDialog);
  protected
    function HookProc(var Message: TMessage): Boolean;
    procedure SubClassProc(var Message: TMessage);
    procedure ModalSubClassProc(var Message: TMessage);
    procedure Loaded; override;
    function DoVerifyPassword(Password: string; var Valid: Boolean): boolean; virtual;
    procedure DoIncorrectPassword; virtual;
    function DoQueryUnlock: boolean; virtual;

    function GetPreviewBtimap(Height, Width: Integer): HBitmap;
    function DoSetIconicThumbnail(Wnd: THandle; Width: Integer; Height: Integer) : HRESULT; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: Integer; virtual;
    procedure Lock;
  published
    property Dialog: TLockDialog read FDialog write SetDialog;
    property Password: string read FPassword write FPassword;
    property UserName: string read FUserName write FUserName;
    property LockedIcon: TIcon read FLockedIcon write SetLockedIcon;
    property UnLockedIcon: TIcon read FUnLockedIcon write SetUnLockedIcon;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property IdleMinutes: integer read GetMinutes write SetMinutes default 0;
    property IdleSeconds: integer read FSeconds write SetSeconds default 10;
    property IncorrectPassword: string read FIncorrectPassword write FIncorrectPassword;
    property Version: string read GetVersion write SetVersion;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property OnQueryUnlock: TQueryUnlockEvent read FOnQueryUnlock write FOnQueryUnlock;
    property OnLock: TNotifyEvent read FOnLock write FOnLock;
    property OnUnLock: TNotifyEvent read FOnUnLock write FOnUnLock;
    property OnVerifyPassword: TVerifyPasswordEvent read FOnVerifyPassword write FOnVerifyPassword;
    property OnIncorrectPassword: TNotifyEvent read FOnIncorrectPassword write FOnIncorrectPassword;
  end;


implementation

uses
  AdvLockAppDlg, AdvDWM, Comobj;

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

constructor TAdvLockApp.Create(AOwner: TComponent);
var
  VerInfo: TOSVersioninfo;
  I,Instances:Integer;
begin
  inherited Create(AOwner);
  FIsD2k7Lvl := False;
  {$IFDEF DELPHI2007_LVL}
  FIsD2k7Lvl := True;
  {$ENDIF}

  FDialog := TLockDialog.Create;

  FIncorrectPassword := 'Password is not correct';

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);

  GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));

  FIsWin7 := (verinfo.dwMajorVersion > 6) OR
    ((verinfo.dwMajorVersion = 6) AND (verinfo.dwMinorVersion >= 1));

  FLocked := False;
  FPassword := '';
  FLockedIcon := TIcon.Create;
  FUnLockedIcon := TIcon.Create;
  FUnLockedIcon.Assign(Application.Icon);
  FSeconds := 10;
  FEnabled := False;
  //FactiveForm := Screen.ActiveForm;   // to solve hidden form when parent form is minimized
  FTimer := nil;
  FAFRect := Rect(-1, -1, -1, -1);

  Instances := 0;
  for I := 0 to Owner.ComponentCount - 1 do
    if (Owner.Components[I] is TAdvLockApp) then Inc(Instances);

  if (Instances > 1) then
    raise Exception.Create('Only one instance of TAdvLockApp allowed on form');

  if not (csDesigning in componentState) then
  begin
    FAppIdle := Application.OnIdle;
    FAppMessage := Application.OnMessage;
    Application.OnIdle := DoIdle;
    Application.OnMessage := DoMessage;
  end;
end;

//------------------------------------------------------------------------------

destructor TAdvLockApp.Destroy;
begin
  FDialog.Free;

	if Assigned(Ftimer) then
  begin
    FTimer.Enabled := False;
    FTimer.Free;
  end;

	if Assigned(FLockedIcon) then
    FLockedIcon.Free;

	if Assigned(FUnLockedIcon) then
    FUnLockedIcon.Free;

	if not (csDesigning in componentState) then
	begin
		Application.OnIdle := FAppIdle;
		Application.OnMessage := FAppMessage;
		Application.UnHookMainWindow(HookProc);
	end;

	inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.Loaded;
begin
  inherited;
  if Enabled then
    SetActiveForm;
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.SetActiveForm;
var
  OldF, OldMF: TForm;
begin
  if not (csDesigning in componentState) then
  begin
    OldF := FActiveForm;
    OldMF := FModalActiveForm;
    FModalActiveForm := nil;
    if Assigned(Application) and Assigned(Application.MainForm) and (Application.MainForm.WindowState <> wsMinimized) then
    begin
      FActiveForm := Application.MainForm;
      {$IFDEF DELPHI2006_LVL}
      if (Application.ModalLevel > 0) then
      begin
        FModalActiveForm := Screen.ActiveForm;
      end;
      {$ENDIF}
    end
    else
    begin
      FActiveForm := Screen.ActiveForm;
    end;

    if (OldF <> FActiveForm) and (IsVista or FIsWin7 or FIsD2k7Lvl) then
    begin
      if Assigned(OldF) then
      begin
        OldF.WindowProc := FFormWndProc;
        FFormWndProc := nil;
      end;

      if Assigned(FActiveForm) then
      begin
        FFormWndProc := FActiveForm.WindowProc;
        FActiveForm.WindowProc := SubClassProc;
      end;
    end;

    if (OldMF <> FModalActiveForm) and (IsVista or FIsWin7 or FIsD2k7Lvl) then
    begin
      if Assigned(OldMF) then
      begin
        OldF.WindowProc := FModalFormWndProc;
        FModalFormWndProc := nil;
      end;

      if Assigned(FModalActiveForm) then
      begin
        FModalFormWndProc := FModalActiveForm.WindowProc;
        FModalActiveForm.WindowProc := ModalSubClassProc;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.SetDialog(const Value: TLockDialog);
begin
  FDialog.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.DoIdle(Sender: TObject; var Done: Boolean);
begin
	Done := True;

	if FEnabled and not FLocked then
	begin
    if Assigned(Screen.ActiveForm) then
      FAFRect := Screen.ActiveForm.BoundsRect;

		FTimer.Enabled := True;
		if Assigned(FOnIdle) then
      FOnIdle(Sender, Done);
	end;

	if Assigned(FAppIdle) then
		FAppIdle(Sender, Done);
end;

procedure TAdvLockApp.DoIncorrectPassword;
begin
  if Assigned(FOnIncorrectPassword) then
    FOnIncorrectPassword(Self)
  else
    ShowMessage(IncorrectPassword);
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.DoMessage(var Msg: TMsg; var Handled: Boolean);
begin
  with Msg do
  begin
    if (Message = WM_MOUSEMOVE) or (Message = WM_KEYDOWN) then
    begin
       if Assigned(FTimer) then FTimer.Enabled := False;
    end;
  end;
  if Assigned(FAppMessage) then FAppMessage(Msg, Handled);
end;

//------------------------------------------------------------------------------

function TAdvLockApp.DoQueryUnlock: boolean;
begin
  Result := false;
  if Assigned(FOnQueryUnlock) then
    FOnQueryUnlock(Self, Result);
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.SetMinutes (Value: integer);
begin
  IdleSeconds := Value * 60;
end;

//------------------------------------------------------------------------------

function TAdvLockApp.GetMinutes: integer;
begin
  Result := FSeconds div 60;
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.SetLockedIcon (Value: TIcon);
begin
   FLockedIcon.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.SetUnLockedIcon (Value: TIcon);
begin
   FUnLockedIcon.Assign(Value);
end;

//------------------------------------------------------------------------------

function TAdvLockApp.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvLockApp.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.Lock;
begin
	if Assigned(FTimer) then
		FTimer.Enabled := False;

	if not Flocked then
	begin
		FLocked := True;
		DoLockUnlock;
	end;
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.OnTimer (Sender: TObject);
begin
  FTimer.Enabled := False;

  if Assigned(Screen.ActiveForm) and (FAFRect.Right - FAFRect.Left > 0) then
  begin
    if not EqualRect(FAFRect, Screen.ActiveForm.BoundsRect) then
      Exit;
  end;

  if not FLocked then
  begin
    FLocked := True;
	DoLockUnlock;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.SetEnabled(Value: boolean);
begin
  FEnabled := Value;  {and (FPassword <> '')};
  
  if (csDesigning in ComponentState) then
    Exit;

  if FEnabled then
  begin
    Application.HookMainWindow(HookProc);      

    if not Assigned(FTimer) then
      FTimer := TTimer.create(nil);

    if Assigned(Screen.ActiveForm) then
      FAFRect := Screen.ActiveForm.BoundsRect;

    FTimer.interval := FSeconds * 1000;
    FTimer.Enabled := True;
    FTimer.OnTimer := Self.OnTimer;
  end
  else
  begin
    if Assigned(FTimer) then
    begin
      FTimer.Enabled := false;
      FTimer.Free;
    end;
    FTimer := nil;
    Application.UnHookMainWindow(HookProc);
  end;
end;

//------------------------------------------------------------------------------

function GetUserFromWindows: string;
Var
   UserName : string;
   UserNameLen : Dword;
Begin
   UserNameLen := 255;
   SetLength(userName, UserNameLen) ;
   If GetUserName(PChar(UserName), UserNameLen) Then
     Result := Copy(UserName,1,UserNameLen - 1)
   Else
     Result := 'Unknown';
End;

//------------------------------------------------------------------------------

procedure TAdvLockApp.UnlockPWDlg;
var
  AdvUnlockPWD: TAdvUnLockForm;
  usr: string;
  valid: Boolean;
begin
  usr := FUserName;
  if usr = '' then
    usr := GetUserFromWindows;

  AdvUnlockPWD := TAdvUnLockForm.CreateWindow(Application, usr);
  try
    Flocked := True;

    AdvUnlockPWD.lbl_usr.Caption := FDialog.LabelUsername;
    AdvUnlockPWD.lbl_pwd.Caption := FDialog.LabelPassword;
    AdvUnlockPWD.Caption := FDialog.Caption;
    AdvUnlockPWD.ShowModal;

    if AdvUnlockPWD.ModalResult = mrOK then
    begin
      valid := (AdvUnlockPWD.me_Password.Text = FPassword);
      FLocked := not DoVerifyPassword(AdvUnlockPWD.me_Password.Text, valid);

      FIncorrectPW := Flocked;

      if Flocked then
        DoIncorrectPassword
      else
        DoLockUnlock;
    end;
  finally
    AdvUnlockPWD.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure CtrlPaintToDC(Control: TWinControl; DC: HDC; X, Y: Integer);

  procedure DrawCtrlBorder(Control: TWinControl; DC: HDC; EdgeFlags, BorderFlags: Integer);
  var
    R: TRect;
  begin
    if (BorderFlags > 0) then
    begin
      SetRect(R, 0, 0, Control.Width, Control.Height);
      DrawEdge(DC, R, EdgeFlags, BorderFlags);
      MoveWindowOrg(DC, R.Left, R.Top);
      IntersectClipRect(DC, 0, 0, R.Right - R.Left, R.Bottom - R.Top);
    end;
  end;

  procedure PaintCtrlTo(Control: TWinControl; DC: HDC; EdgeFlags, BorderFlags: Integer);
  begin
    Control.Perform(WM_ERASEBKGND, DC, 0);

    if (Control is TCustomForm) then
    begin
      DrawCtrlBorder(Control, DC, EdgeFlags, BorderFlags);
      Control.Perform(WM_PAINT, DC, 0);
    end
    else
    begin
      if BorderFlags = -1 then
        Control.Perform(WM_PRINT, DC, PRF_NONCLIENT or PRF_CLIENT)
      else
      begin
        DrawCtrlBorder(Control, DC, EdgeFlags, BorderFlags);
        Control.Perform(WM_PRINT, DC, PRF_CLIENT);
      end;
    end;
  end;

var
  I, EdgeFlags, BorderFlags, SaveIndex: Integer;
  R1, R2: TRect;
  DeltaX, DeltaY : Integer;
begin
  Control.ControlState := Control.ControlState + [csPaintCopy];
  SaveIndex := SaveDC(DC);
  MoveWindowOrg(DC, X, Y);
  IntersectClipRect(DC, 0, 0, Control.Width, Control.Height);
  BorderFlags := 0;
  EdgeFlags := 0;
  if (GetWindowLong(Control.Handle, GWL_STYLE) and (WS_VSCROLL or WS_HSCROLL) <> 0) then
    BorderFlags := -1
  else
  begin
    if (GetWindowLong(Control.Handle, GWL_EXSTYLE) and WS_EX_CLIENTEDGE <> 0) then
    begin
      EdgeFlags := EDGE_SUNKEN;
      BorderFlags := BF_RECT or BF_ADJUST
    end else
    if (GetWindowLong(Control.Handle, GWL_STYLE) and WS_BORDER <> 0) then
    begin
      EdgeFlags := BDR_OUTER;
      BorderFlags := BF_RECT or BF_ADJUST or BF_MONO;
    end;
  end;

  PaintCtrlTo(Control, DC, EdgeFlags, BorderFlags);

  if GetWindowLong(Control.Handle, GWL_STYLE) and WS_CAPTION = 0 then
  begin
    GetWindowRect(Control.Handle, R1);
    GetClientRect(Control.Handle, R2);
    R2.TopLeft := Control.ClientToScreen(R2.TopLeft);
    R2.BottomRight := Control.ClientToScreen(R2.BottomRight);
    DeltaX := R2.Left - R1.left;
    DeltaY := R2.Top - R1.Top;
  end
  else
  begin
   DeltaX := 0;
   DeltaY := 0;
  end;

  for I := 0 to Control.ControlCount - 1 do
    if Control.Controls[i] is TWinControl then
      with TWinControl(Control.Controls[I]) do
        if Visible then CtrlPaintToDC(TWinControl(Control.Controls[I]), DC, Left + DeltaX, Top + DeltaY);
  RestoreDC(DC, SaveIndex);
  Control.ControlState := Control.ControlState - [csPaintCopy];
end;

function ControlToHBitmap(Control: TWinControl; ControlRect: TRect; DestWidth: Integer; DestHeight: Integer) : HBITMAP;
var
  desdc, srcdc: HDC;
  dbmi, sbmi: BITMAPINFO;
  dBits, sBits: PInteger;
  sBitmap: HBITMAP;
  i, j: Integer;
begin
  Result := 0;
  if (ControlRect.Left = ControlRect.Right) or (ControlRect.Top = ControlRect.Bottom) or (DestWidth = 0) or (DestHeight = 0) then
    Exit;

  srcdc := CreateCompatibleDC(0);
  desdc := CreateCompatibleDC(0);
  if (srcdc <> 0) and (desdc <> 0) then
  begin
    ZeroMemory(@sbmi.bmiHeader, sizeof(BITMAPINFOHEADER));
    sbmi.bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
    sbmi.bmiHeader.biWidth := Control.ClientWidth;
    sbmi.bmiHeader.biHeight := -Control.ClientHeight;
    sbmi.bmiHeader.biPlanes := 1;
    sbmi.bmiHeader.biBitCount := 32;

    sBitmap := CreateDIBSection(srcdc, sbmi, DIB_RGB_COLORS, Pointer(sBits), 0, 0);

    ZeroMemory(@dbmi.bmiHeader, sizeof(BITMAPINFOHEADER));
    dbmi.bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
    dbmi.bmiHeader.biWidth := DestWidth;
    dbmi.bmiHeader.biHeight := -DestHeight;
    dbmi.bmiHeader.biPlanes := 1;
    dbmi.bmiHeader.biBitCount := 32;

    Result := CreateDIBSection(desdc, dbmi, DIB_RGB_COLORS, Pointer(dBits), 0, 0);

    SelectObject(srcdc, sBitmap);
    SelectObject(desdc, Result);

    CtrlPaintToDC(Control, srcdc, 0, 0);

    StretchBlt(desdc, 0, 0, DestWidth, DestHeight,
               srcdc, ControlRect.Left, ControlRect.Top,
               ControlRect.Right - ControlRect.Left, ControlRect.Bottom - ControlRect.Top,
               SRCCOPY);

    for i := 0 to DestHeight - 1 do
      for j := 0 to DestWidth - 1 do
      begin
        dBits^ := dBits^ and $00ffffff;
        Inc(dBits);
      end;

    DeleteDC(srcdc);
    DeleteDC(desdc);
    DeleteObject(sBitmap);
  end;
end;

//------------------------------------------------------------------------------

function TAdvLockApp.GetPreviewBtimap(Height, Width: Integer): HBitmap;
var
  AtaUnlockPWD: TAdvUnLockForm;
begin
  AtaUnlockPWD := TAdvUnLockForm.CreateWindow(Application, FUserName);
  Result := ControlToHBitmap(AtaUnlockPWD, AtaUnlockPWD.ClientRect, Width, Height);
  AtaUnlockPWD.Free;
end;

//------------------------------------------------------------------------------

function TAdvLockApp.DoSetIconicThumbnail(Wnd: THandle; Width,
  Height: Integer): HRESULT;
var
 Hbmp: HBitmap;
begin
  Result := -1;
  //if Assigned(FOnGetIconicBitmap) then begin
    //FOnGetIconicBitmap(Self, Width, Height, Hbmp);
  Hbmp := GetPreviewBtimap(Height, Width);
  if (Hbmp <> 0) then
  begin
    Result := DwmSetIconicThumbnail(Wnd, Hbmp, 0);
    DeleteObject(Hbmp);
  end;
end;

//------------------------------------------------------------------------------

function TAdvLockApp.DoVerifyPassword(Password: string; var Valid: Boolean): boolean;
begin
  if Assigned(FOnVerifyPassword) then
    FOnVerifyPassword(Self, Password, Valid);
  Result := Valid;
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.ModalSubClassProc(var Message: TMessage);
begin
  with Message do
  begin
    case msg of
      WM_SYSCOMMAND:
      begin
        if (wParam = SC_RESTORE) or (wParam = SC_MAXIMIZE) then
        begin
          if FLocked then
          begin
            if DoQueryUnlock then
            begin
              FLocked := false;
              DoLockUnlock;
            end
            else
              if not Assigned(FOnQueryUnlock) then
                UnlockPWDlg;
          end;

          if FLocked then
          begin
            msg := WM_NULL;
            Message.Result := 0;
          end;
        end;
      end;
      WM_QUERYOPEN:  // FF: On dbl click secondry form restores without sending SC_Restore msg
      begin
        if FLocked then
        begin
          if FLocked then
          begin
            if DoQueryUnlock then
            begin
              FLocked := false;
              DoLockUnlock;
            end
            else if not Assigned(FOnQueryUnlock) then
              UnlockPWDlg;
          end;

          if FLocked then
          begin
            Message.msg := WM_NULL;
            Message.Result := 0;
            Exit;
          end;
        end;
      end;

    end;
  end;

  if Assigned(FModalFormWndProc) then
    FModalFormWndProc(Message);

  if (Message.Msg in [WM_DESTROY, WM_CLOSE]) or (not FLocked and (Message.wParam = SC_RESTORE)) then
  begin
    if Assigned(FModalActiveForm) then
    begin
      FModalActiveForm.WindowProc := FModalFormWndProc;
      FModalActiveForm := nil;
      FModalFormWndProc := nil;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.SubClassProc(var Message: TMessage);
begin
  with Message do
  begin
    case msg of
      WM_SYSCOMMAND:
      begin
        if (wParam = SC_RESTORE) or (wParam = SC_MAXIMIZE) then
        begin
          if FLocked then
          begin
            if DoQueryUnlock then
            begin
              FLocked := false;
              DoLockUnlock;
            end
            else
              if not Assigned(FOnQueryUnlock) then
                UnlockPWDlg;
          end;

          if FLocked then
          begin
            msg := WM_NULL;
            Message.Result := 0;
          end;
        end;
      end;
      {WM_WINDOWPOSCHANGING, WM_WINDOWPOSCHANGED, WM_GETICON, WM_PAINTICON:  // FF: winXP iss that some times restores form without sending SC_Restore msg
      begin
        if FLocked then
        begin
          msg := WM_NULL;
          Message.Result := 0;
          Exit;
        end;
      end;}
     WM_DWMSENDICONICTHUMBNAIL:
     begin
       if Assigned(FActiveForm) then
         if DoSetIconicThumbnail(FActiveForm.Handle, HIWORD(Message.lParam), LOWORD(Message.lParam)) = 0 then
         begin
           Message.Result := 0;
           Exit;
         end;
     end;
    end;
  end;

  if (IsVista or FIsWin7 or FIsD2k7Lvl) then
  begin
    if not FActivatingHack and FLocked and (Message.Msg = WM_QUERYOPEN) then
      FActivatingHack := True;

    if FActivatingHack and FLocked and (Message.Msg in [WM_WINDOWPOSCHANGING, WM_WINDOWPOSCHANGED, WM_QUERYOPEN, WM_ACTIVATE, WM_ACTIVATEAPP]) then
    begin
      Message.Result := 0;
      Exit;
    end;
  end;
  //if FLocked then
    //OutputDebugString(PChar('Msg:   ' + Inttostr(Message.Msg) + ' -- FLocked: ' + BoolToStr(FLocked)));

  FFormWndProc(Message);
end;

//------------------------------------------------------------------------------

function TAdvLockApp.HookProc(var Message: TMessage): Boolean;
// force the application to stay minimized
begin
  Result := False;

  with message do
  begin
    case msg of
      WM_SYSCOMMAND:
      begin
        if (wParam = SC_RESTORE) or (wParam = SC_MAXIMIZE) then
        begin
          if FLocked then
          begin
            if DoQueryUnlock then
            begin
              FLocked := false;
              DoLockUnlock;
            end
            else
              if not Assigned(FOnQueryUnlock) then
                UnlockPWDlg;
          end;
          if FLocked then
            msg := WM_NULL;
        end;
      end;
      WM_WINDOWPOSCHANGING, WM_WINDOWPOSCHANGED:  // FF: winXP iss that some times restores form without sending SC_Restore msg
      begin
        if FIncorrectPW and FLocked then
          msg := WM_NULL;
      end;
    end;
  end;

  // hack to avoid retoring Form from windows task manager.
  if FIsWinXP and not (IsVista or FIsWin7) then
  begin
    if not FActivatingHack and FLocked and (Message.Msg = WM_QUERYOPEN) then
      FActivatingHack := True;

    if FActivatingHack and FLocked and (Message.Msg in [WM_WINDOWPOSCHANGING, WM_WINDOWPOSCHANGED, WM_QUERYOPEN, WM_ACTIVATE, WM_ACTIVATEAPP]) then
    begin
      Result := True;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.DoLockUnlock;
var
  EnableAttribute: DWORD;
  hr: HRESULT;
  WndHandle: THandle;
begin
  if (csDesigning in ComponentState) then
    Exit;

  FActivatingHack := False;

  WndHandle := 0;
  if FLocked then
  begin
    if (FLockedIcon <> nil) and not (FLockedIcon.Empty) then
     Application.Icon.Assign(FLockedIcon);

    SetActiveForm;

    //FactiveForm := Screen.ActiveForm;   // to solve hidden form when parent form is minimized

    //older Delphi versions
    //SHOWWINDOW(Application.handle, SW_MINIMIZE);

    //newer Delphi versions
    ShowWindow(FActiveForm.Handle, SW_SHOW); // so Vista can get a thumbnail at startup
    Application.ProcessMessages;  // allow time for form to be drawn

    if (IsVista or FIsWin7 or FIsD2k7Lvl) then
    begin
      if Assigned(FActiveForm) then
      begin
        ShowWindow(FActiveForm.Handle, SW_MINIMIZE);

        if Assigned(FModalActiveForm) then
          ShowWindow(FModalActiveForm.Handle, SW_MINIMIZE);

        if IsVista or FIsWin7 then
        begin
          ChangeWindowMessageFilter(WM_DWMSENDICONICTHUMBNAIL, MSGFLT_ADD);
          WndHandle := FActiveForm.Handle;
        end;
      end;
    end
    else
      ShowWindow(Application.Handle, SW_MINIMIZE);

    if Assigned(FOnLock) then
      FOnLock(Self);
  end
  else
  begin
    if (FUnLockedIcon <> nil) and not (FLockedIcon.Empty) then
      Application.Icon.Assign(FUnLockedIcon);
    ShowWindow(Application.Handle, SW_RESTORE);
    {$IFDEF DELPHI2006_LVL}
    if Assigned(FActiveForm) then
    begin
      if (Application.ModalLevel > 0) then
        ShowWindow(FActiveForm.Handle, SW_RESTORE);

      if Assigned(FModalActiveForm) then
        FModalActiveForm.BringToFront
      else
        FActiveForm.BringToFront;   // to solve hidden form when paren form is minimized
      WndHandle := FActiveForm.Handle;
    end;
    {$ENDIF}

    if Assigned(FOnUnLock) then
      FOnUnLock(Self);
  end;

  if (WndHandle <> 0) and (IsVista or FIsWin7) then
  begin
    EnableAttribute := DWORD(FLocked);
    hr := DwmSetWindowAttribute(WndHandle, DWMWA_FORCE_ICONIC_REPRESENTATION,
          @EnableAttribute, SizeOf(EnableAttribute));
    OleCheck(hr);

    if FIsWin7 then
    begin
      EnableAttribute := DWORD(FLocked);
      hr := DwmSetWindowAttribute(WndHandle, DWMWA_HAS_ICONIC_BITMAP,
            @EnableAttribute, SizeOf(EnableAttribute));
      OleCheck(hr);

      EnableAttribute := DWORD(FLocked);
      hr := DwmSetWindowAttribute(
          WndHandle,
          DWMWA_DISALLOW_PEEK,
          @EnableAttribute,
          SizeOf(EnableAttribute));
      OleCheck(hr);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvLockApp.SetSeconds(const Value: integer);
begin
  if (FSeconds <> Value) then
  begin
    FSeconds := Value;
    if not (csDesigning in ComponentState) and Assigned(FTimer) then
      FTimer.interval := FSeconds * 1000;
  end;
end;

//------------------------------------------------------------------------------

{ TLockDialog }

procedure TLockDialog.Assign(Source: TPersistent);
begin
  if (Source is TLockDialog) then
  begin
    FCaption := (Source as TLockDialog).Caption;
    FLabelPassword := (Source as TLockDialog).LabelPassword;
    FLabelUserName := (Source as TLockDialog).LabelUsername;
  end;
end;

constructor TLockDialog.Create;
begin
  inherited Create;
  FCaption := 'Enter password to reopen application';
  FLabelPassword := 'Password';
  FLabelUserName := 'Username';
end;

end.

