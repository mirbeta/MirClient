{***************************************************************************}
{ TFormSize component                                                       }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2013                                        }
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

unit FormSize;

{$I TMSDEFS.INC}

interface

uses
  SysUtils, Windows, Messages, Classes, Forms, ShellAPI, Registry, Types;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 5; // Build nr.

  // version history
  // 1.1.0.1 : Fixed issue with magnet function on left/top side
  // 1.1.0.2 : Added exception handling in LoadFromSettings, SaveFormSettings
  // 1.2.0.0 : Added support to be used on Unicode forms (TTntForm)
  // 1.3.0.0 : Added support to persist in registry
  // 1.3.1.0 : Improved : position persistence
  // 1.3.1.1 : Fixed : Issue with form position persistence & magnet function
  // 1.3.1.2 : Fixed : Issue with Win64 and Windows 8
  // 1.3.1.3 : Fixed : Issue with maximized state & multimonitor config
  // 1.3.1.4 : Fixed : Issue with restoring form position
  // 1.3.1.5 : Improved : support for multimonitor use

type
  EFormSizeError = class(Exception);

  TPersistLocation = (plIniFile, plRegistry);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TFormSize = class(TComponent)
  private
    { Private declarations }
    OldWndProc: TFarProc;
    NewWndProc: Pointer;
    FSaveMachine: boolean;
    FSaveUser: boolean;
    FSavePosition: boolean;
    FSaveSize: boolean;
    FSaveName: string;
    FSaveKey: string;
    FDragAlways: boolean;
    FMagnet: boolean;
    FMagnetDistance: integer;
    FLocation: TPersistLocation;
    function CreateKey:string;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    { Protected declarations }
    function GetVersionNr: Integer; virtual;
    procedure HookWndProc(var Msg: TMessage);
    procedure DoLoadPlacement;
    procedure DoSavePlacement;
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SaveFormSettings;
    procedure LoadFormSettings;
  published
    { Published declarations }
    property DragAlways: boolean read FDragAlways write FDragAlways default False;
    property Location: TPersistLocation read FLocation write FLocation default plIniFile;
    property Magnet: boolean read FMagnet write FMagnet default False;
    property MagnetDistance: integer read FMagnetDistance write FMagnetDistance;

    property SavePosition: boolean read FSavePosition write FSavePosition default True;
    property SaveSize: boolean read FSaveSize write FSaveSize default True;
    property SaveUser: boolean read FSaveUser write FSaveUser;
    property SaveMachine: boolean read fSaveMachine write fSaveMachine;
    property SaveName: string read FSaveName write FSaveName;
    property SaveKey: string read FSaveKey write FSaveKey;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

uses
  INIFiles, Dialogs;

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}


{$WARNINGS OFF}
constructor TFormSize.Create(AOwner:TComponent);
var
  I, Instances: Integer;
begin
  inherited Create(AOwner);
  if not (Owner is TForm) then
    raise EFormSizeError.Create('Control parent must be a form!');
  Instances := 0;
  for I := 0 to Owner.ComponentCount - 1 do
    if (Owner.Components[I] is TFormSize) then
      Inc(Instances);
  if (Instances > 1) then
    raise EFormSizeError.Create('The form already contains a TFormSize component');

  FSavePosition := True;
  FSaveSize := True;
  FSaveName := '.\FORM.INI';
  FSaveKey := owner.Name;
  FMagnetDistance := 32;
  FLocation := plInifile;

  { Hook parent }

  {$IFDEF DELPHI_UNICODE}
  OldWndProc := TFarProc(GetWindowLongPtr((Owner as TForm).Handle, GWL_WNDPROC));
  NewWndProc := MakeObjectInstance(HookWndProc);
  SetWindowLongPtr((Owner as TForm).Handle, GWL_WNDPROC, LInteger(NewWndProc));
  {$ENDIF}

  {$IFNDEF DELPHI_UNICODE}
  OldWndProc := TFarProc(GetWindowLong((Owner as TForm).Handle, GWL_WNDPROC));
  NewWndProc := MakeObjectInstance(HookWndProc);
  SetWindowLong((Owner as TForm).Handle, GWL_WNDPROC, LInteger(NewWndProc));
  {$ENDIF}
end;  { TFormSize.Create }


procedure TFormSize.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) and (FSavePosition or FSaveSize) then
    DoLoadPlacement;
end;  { TFormSize.Loaded }

destructor TFormSize.Destroy;
begin
  { Unhook parent }
  {$IFDEF DELPHI_UNICODE}
  if (Owner <> nil) and Assigned(OldWndProc) then
    SetWindowLongPtr((Owner as TForm).Handle, GWL_WNDPROC, LInteger(OldWndProc));
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  if (Owner <> nil) and Assigned(OldWndProc) then
    SetWindowLong((Owner as TForm).Handle, GWL_WNDPROC, LInteger(OldWndProc));
  {$ENDIF}

  if Assigned(NewWndProc) then
    FreeObjectInstance(NewWndProc);

  { Clean up }
  inherited Destroy;
end;  { TFormSize.Destroy }

{$WARNINGS ON}

procedure TFormSize.DoLoadPlacement;
var
  ARect: TRect;
  Maximize: Boolean;
  Settings: TIniFile;
  RegInifile: TRegInifile;
  Key:string;
  FOldMagnet: boolean;
//  Placement: TWindowPlacement;
  mon: TMonitor;
begin
  if (FSaveName = '') or (FSaveKey = '') then
    Exit;

  FOldMagnet := FMagnet;
  FMagnet := false;

  ARect := (Owner as TForm).BoundsRect;

  if Location = plInifile then
  begin
    Settings := TIniFile.Create(FSaveName);
    try
      Key := CreateKey;

      with Settings, ARect do
      begin
        if FSavePosition then
        begin
          Left := ReadInteger(Key, 'Left', Left);
          Top := ReadInteger(Key, 'Top', Top);
        end;

        if FSaveSize then
        begin
          Right := ReadInteger(Key, 'Right', Right);
          Bottom := ReadInteger(Key, 'Bottom', Bottom);
        end
        else
        begin
          Right := Left + (Owner as TForm).Width;
          Bottom := Top + (Owner as TForm).Height;
        end;

        Maximize := ReadBool(Key, 'Maximized',
          (Owner as TForm).WindowState = wsMaximized);
      end;
    finally
      Settings.Free;
    end;
  end
  else
  begin
    RegIniFile := TRegIniFile.Create(FSaveName);
    try
      ARect := (Owner as TForm).BoundsRect;
      Key := CreateKey;

      with RegInifile, ARect do
      begin
        if FSavePosition then
        begin
          Left := ReadInteger(Key, 'Left', Left);
          Top := ReadInteger(Key, 'Top', Top);
        end;

        if FSaveSize then
        begin
          Right := ReadInteger(Key, 'Right', Right);
          Bottom := ReadInteger(Key, 'Bottom', Bottom);
        end
        else
        begin
          Right := Left + (Owner as TForm).Width;
          Bottom := Top + (Owner as TForm).Height;
        end;

        Maximize := ReadBool(Key, 'Maximized',
          (Owner as TForm).WindowState = wsMaximized);
      end;
    finally
      RegInifile.Free;
    end;
  end;

  { Make sure the window is entirely visible on the screen }
  with ARect do
  begin
    if ((Right > Screen.DesktopWidth) and not (Maximize)) then
    begin
      Dec(Left, (Right - Screen.DesktopWidth));
      Right := Screen.DesktopWidth;
    end;

    if (Left < Screen.DesktopLeft) then
    begin
      Inc(Right, (Screen.DesktopLeft - Left));
      Left := Screen.DesktopLeft;
    end;

    if ((Bottom > Screen.DesktopHeight) and not(Maximize)) then
    begin
      Dec(Top, (Bottom - Screen.DesktopHeight));
      Bottom := Screen.DesktopHeight;
    end;

    if (Top < Screen.DesktopTop) then
    begin
      Inc(Bottom, (Screen.DesktopTop - Top));
      Top := Screen.DesktopTop;
    end;
  end;

  if Maximize then
  begin
    mon := Screen.MonitorFromPoint(Point(ARect.Left + ((ARect.Right - ARect.Left) div 2),
      ARect.Top + ((ARect.Bottom - ARect.Top) div 2)));

    ARect := mon.BoundsRect;
  end;

//  Placement.rcNormalPosition := ARect;
//  Placement.length := Sizeof(Placement);
//  SetWindowPlacement((Owner as TForm).Handle, @Placement);
//  MoveWindow((Owner as TForm).Handle, ARect.Left, ARect.Top, ARect.Width, ARect.Height, true);

  (Owner as TForm).BoundsRect := ARect;

  if Maximize then
    (Owner as TForm).WindowState := wsMaximized;

  FMagnet := FOldMagnet;
end;  { TFormSize.LoadPlacement }

procedure TFormSize.DoSavePlacement;
var
  Placement: TWindowPlacement;
  Settings: TIniFile;
  RegInifile: TReginifile;
  Key:string;

begin
  if (FSaveName = '') or (FSaveKey = '') then
    Exit;

  Placement.length := SizeOf(Placement);
  GetWindowPlacement((Owner as TForm).Handle, @Placement);

  if Location = plInifile then
  begin
    Settings := TIniFile.Create(FSaveName);
    try
      Key := CreateKey;

      with Settings, Placement, rcNormalPosition do
      begin
        if FSavePosition then
        begin
          WriteInteger(Key, 'Left', (Owner as TForm).Left);
          WriteInteger(Key, 'Top', (Owner as TForm).Top);
        end;
        if FSaveSize then
        begin
          WriteInteger(Key, 'Right', (Owner as TForm).Left + (Owner as TForm).Width);
          WriteInteger(Key, 'Bottom', (Owner as TForm).Top + (Owner as TForm).Height);
          WriteBool(Key, 'Maximized', showCmd = SW_SHOWMAXIMIZED);
        end;
      end;
    finally
      Settings.Free;
    end;
  end
  else
  begin
    RegInifile := TRegIniFile.Create(FSaveName);
    try
      Key := CreateKey;

      with RegInifile, Placement, rcNormalPosition do
      begin
        if FSavePosition then
        begin
          WriteInteger(Key, 'Left', (Owner as TForm).Left);
          WriteInteger(Key, 'Top', (Owner as TForm).Top);
        end;
        if FSaveSize then
        begin
          WriteInteger(Key, 'Right', (Owner as TForm).Left + (Owner as TForm).Width);
          WriteInteger(Key, 'Bottom', (Owner as TForm).Top + (Owner as TForm).Height);
          WriteBool(Key, 'Maximized', showCmd = SW_SHOWMAXIMIZED);
        end;
      end;
    finally
      RegInifile.Free;
    end;
  end;
end;  { TFormSize.DoSavePlacement }

procedure TFormSize.HookWndProc(var Msg: TMessage);
var
  xpos,ypos:word;
  pt: TPoint;
  wp: PWindowPos;
  R: TRect;
  AD : TAppBarData;
  lim_left,lim_top,lim_right,lim_bottom : integer;

begin
  with Msg do
  begin
    case Msg of
    WM_WINDOWPOSCHANGING:
      begin
        if FMagnet then
        begin
          fillchar(AD,sizeof(AD),0);
          AD.cbSize := sizeof(AD);
          SHAppBarMessage(ABM_GETTASKBARPOS,AD);

          lim_left := 0;
          lim_right := GetSystemMetrics(SM_CXSCREEN);
          lim_top := 0;
          lim_bottom := GetSystemMetrics(SM_CYSCREEN);

          case AD.uEdge of
          ABE_BOTTOM: lim_bottom := lim_bottom - (AD.rc.Bottom -AD.rc.Top);
          ABE_TOP: lim_top := lim_top + (AD.rc.Bottom -AD.rc.Top);
          ABE_LEFT: lim_left := lim_left + (AD.rc.Right -AD.rc.Left);
          ABE_RIGHT: lim_right := lim_right - (AD.rc.Right -AD.rc.Left);
          end;

          wp:=PWindowPos(lparam);

          R := (Owner as TForm).BoundsRect;
          if (wp^.x<lim_left + FMagnetDistance) or (wp^.x<lim_left) then
            wp^.x := lim_left;
          if (wp^.y<lim_top + FMagnetDistance) or (wp^.y<lim_top) then
            wp^.y := lim_top;

          if (wp^.y + (R.Bottom-R.Top) > lim_bottom-FMagnetDistance) then
             wp^.y := lim_bottom - (R.Bottom-R.Top);

          if (wp^.x + (R.Right-R.Left) > lim_right-FMagnetDistance) then
             wp^.x := lim_right - (R.Right-R.Left);
        end;
      end;
    end;

    Result := CallWindowProc(OldWndProc, (Owner as TForm).Handle, Msg,
                                wParam, lParam);

    case Msg of
    WM_DESTROY:
      if not (csDesigning in ComponentState) and FSavePosition then
        DoSavePlacement;

    WM_NCHITTEST:
      begin
        if FDragAlways and not (csDesigning in ComponentState) then
        begin
          xpos := loword(lParam);
          ypos := hiword(lParam);
          pt := (Owner as TForm).ScreenToClient(point(xpos,ypos));
          if PtInRect((Owner as TForm).ClientRect,pt) then
            Result := htCaption;
        end;
      end;
    end;
  end;
end;  { TFormSize.HookWndProc }

function TFormSize.CreateKey: string;
var
 buf:array[0..255] of char;
 bufsize:dword;
begin
 result:=SaveKey;

 bufsize:=sizeof(buf);
 GetUserName(buf,bufsize);
 if fSaveUser then result:=result+'-'+strpas(buf);
 bufsize:=sizeof(buf);
 GetComputerName(buf,bufsize);
 if fSaveMachine then result:=result+'-'+strpas(buf);

end;

procedure TFormSize.LoadFormSettings;
var
  osavepos,osavesize: Boolean;
begin
  osavesize := FSaveSize;
  osavepos := FSavePosition;
  FSaveSize := True;
  FSavePosition := True;
  try
    DoLoadPlacement;
  finally
    FSaveSize := osavesize;
    FSavePosition := osavepos;
  end;
end;

procedure TFormSize.SaveFormSettings;
var
  osavepos,osavesize: Boolean;
begin
  osavesize := FSaveSize;
  osavepos := FSavePosition;
  FSaveSize := True;
  FSavePosition := True;
  try
    DoSavePlacement;
  finally
    FSaveSize := osavesize;
    FSavePosition := osavepos;
  end;
end;

function TFormSize.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TFormSize.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TFormSize.SetVersion(const Value: string);
begin

end;

end.
