{*******************************************************************}
{ TWEBCONNECT component                                             }
{ for Delphi & C++Builder                                           }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{    copyright © 2002-2012                                          }
{    Email : info@tmssoftware.com                                   }
{    Web   : http://www.tmssoftware.com                             }
{                                                                   }
{ The source code is given as is. The author is not responsible     }
{ for any possible damage done due to the use of this code.         }
{ The component can be freely used in any application. The source   }
{ code remains property of the writer and may not be distributed    }
{ freely as such.                                                   }
{*******************************************************************}

{$I TMSDEFS.INC}

unit WebConnect;

interface

uses
  Classes, Windows, SysUtils;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.1.0.0 : New : Prompt property added

type

  TWebConnectType = (wctModem,wctLan,wctNone);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebConnect = class(TComponent)
  private
    FConnect: Boolean;
    FPrompt: Boolean;
    FActive: Boolean;
    procedure SetConnect(const Value: Boolean);
    function GetConnectedState: Boolean;
    function GetWebConnectType: TWebConnectType;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
  public
    property Connected: Boolean read GetConnectedState;
    property ConnectionType: TWebConnectType read GetWebConnectType;
  published
    property Active: Boolean read FActive write FActive;
    property Connect: Boolean read FConnect write SetConnect;
    property Prompt: Boolean read FPrompt write FPrompt;
    property Version: string read GetVersion write SetVersion;
  end;

implementation

const
  INTERNET_AUTODIAL_FORCE_ONLINE          = 1;
  INTERNET_AUTODIAL_FORCE_UNATTENDED      = 2;
  INTERNET_AUTODIAL_FAILIFSECURITYCHECK   = 4;
  INTERNET_AUTODIAL_FLAGS_MASK = INTERNET_AUTODIAL_FORCE_ONLINE or
                                 INTERNET_AUTODIAL_FORCE_UNATTENDED or
                                 INTERNET_AUTODIAL_FAILIFSECURITYCHECK;

  INTERNET_CONNECTION_MODEM        =   $1;
  INTERNET_CONNECTION_LAN          =   $2;
  INTERNET_CONNECTION_PROXY        =   $4;
  INTERNET_CONNECTION_MODEM_BUSY   =   $8;
  INTERNET_RAS_INSTALLED           =   $10;
  INTERNET_CONNECTION_OFFLINE      =   $20;
  INTERNET_CONNECTION_CONFIGURED   =   $40;

  winetdll = 'WININET.DLL';


function DynaLink_InternetAutodial(dwFlags: DWORD; dwReserved: DWORD): BOOL;
var
  WininetDLL: THandle;
  Wininet_InternetAutodial:function(dwFlags: DWORD; dwReserved: DWORD): BOOL; stdcall;
begin
  Result := TRUE;
  WininetDLL := LoadLibrary(winetdll);
  if WininetDLL > 0 then
  begin
    @Wininet_InternetAutodial := GetProcAddress(Wininetdll,'InternetAutodial');
    if Assigned(Wininet_InternetAutodial) then
    begin
      Result := Wininet_InternetAutodial(dwFlags,dwReserved);
    end;
    FreeLibrary(WininetDLL);
  end;
end;

function DynaLink_InternetAutodialHangup(dwReserved: DWORD): BOOL;
var
  WininetDLL: THandle;
  Wininet_InternetAutodialHangup:function(dwReserved: DWORD): BOOL; stdcall;
begin
  Result := TRUE;
  WininetDLL := LoadLibrary(winetdll);
  if WininetDLL > 0 then
  begin
    @Wininet_InternetAutodialHangup := GetProcAddress(WininetDLL,'InternetAutodialHangup');
    if Assigned(Wininet_InternetAutodialHangup) then
    begin
      Result := Wininet_InternetAutodialHangup(dwReserved);
    end;
    FreeLibrary(WininetDLL);
  end;
end;

function DynaLink_InternetGetConnectedState(lpdwFlags: LPDWORD;dwReserved: DWORD): BOOL;
var
  WininetDLL: THandle;
  Wininet_InternetGetConnectedState:function(lpdwFlags: LPDWORD;dwReserved: DWORD): BOOL; stdcall;
begin
  Result := TRUE;
  WininetDLL := LoadLibrary(winetdll);
  if WininetDLL > 0 then
  begin
    @Wininet_InternetGetConnectedState := GetProcAddress(WininetDLL,'InternetGetConnectedState');
    if Assigned(Wininet_InternetGetConnectedState) then
    begin
      Result := Wininet_InternetGetConnectedState(lpdwFlags,dwReserved);
    end;
    FreeLibrary(WininetDLL);
  end;
end;


{ TWebConnect }

procedure TWebConnect.SetConnect(const Value: Boolean);
var
  dwReserved: Cardinal;
begin
  FConnect := Value;
  dwReserved := 0;

  if csDesigning in ComponentState then
    Exit;

  if not FActive then
    Exit;  

  if FConnect and not Connected then
  begin
    if FPrompt then
    begin
      if not DynaLink_InternetAutodial(INTERNET_AUTODIAL_FORCE_ONLINE,dwReserved) then
        Exit;
    end;

    if not FPrompt then
    begin
      if not DynaLink_InternetAutodial(INTERNET_AUTODIAL_FORCE_UNATTENDED,dwReserved) then
        Exit;
    end;
  end
  else
    if not FConnect and Connected then
    begin
      DynaLink_InternetAutoDialHangup(dwReserved);
    end;
end;

function TWebConnect.GetConnectedState: Boolean;
var
  dwFlags: dword;
  dwReserved: Cardinal;
begin
  dwReserved := 0;
  Result := DynaLink_InternetGetConnectedState(@dwFlags,dwReserved);
end;

function TWebConnect.GetWebConnectType: TWebConnectType;
var
  dwFlags: dword;
  dwReserved: cardinal;
begin
  dwReserved := 0;
  Result := wctNone;

  if DynaLink_InternetGetConnectedState(@dwFlags,dwReserved) then
  begin
    if dwFlags and INTERNET_CONNECTION_MODEM = INTERNET_CONNECTION_MODEM then
      Result := wctModem;

    if dwFlags and INTERNET_CONNECTION_LAN = INTERNET_CONNECTION_LAN then
      Result := wctLAN;
  end;

end;

function TWebConnect.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TWebConnect.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TWebConnect.SetVersion(const Value: string);
begin

end;

end.
