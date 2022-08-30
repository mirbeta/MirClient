{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Shows how to use TWSocket from within a Windows NT/2000 service.
              This code has been tested with Delphi 5, Windows NT4 and 2000.
              All TWSocket code has been encapsulated in TTcpDaemon object.
              This is done so that you can see how the same code can be used
              inside a service or inside a normal exe program (see SrvTcp).
              To install SvcTcp, you need Windows NT or Windows 2000. At
              command prompt, enter: <SvcTcp /install> to uninstall it, just
              enter the command <SvcTcp /uninstall>. Once installed, you can
              find SvcTcp service in the "services" applet. You can start and
              stop it from that applet. You can also start and stop it from
              the command line with the command <net start "ICS Tcp Service">.
              To test for service operation, use command line telnet utility
              and connect to port 2120, then enter the command help and hit
              return key. If you wants to see what you type, turn telnet
              local echo to on.
Creation:     July 15, 2000
Version:      1.01
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2000-2010 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Jul 18, 2004 V1.01 Arno Garrels <garrels@duodata.de> made this sample
                   compatible with Delphi 4.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSvcTcp1;

{$I OVERBYTEICSDEFS.INC}
{$IFNDEF DELPHI4_UP}
  Bomb('This code require Delphi 4 or later');
{$ENDIF}

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$H+}       { Use long strings                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, SvcMgr, OverbyteIcsTcpCmd;

const
  SvcTcpVersion             = 101;
  CopyRight    : String     = ' SvcTcp (c) 2000-2010 F. Piette V1.01 ';

type
  TIcsTcpSvc = class(TService)
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
  private
    FTcpDaemon : TTcpDaemon;
    procedure Display(Msg: String);
  public
    {$IFDEF DELPHI5_UP}
    function GetServiceController: TServiceController; override;
    {$ELSE}
    function GetServiceController: PServiceController; override;
    {$ENDIF}
  end;

var
  IcsTcpSvc: TIcsTcpSvc;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ServiceController(CtrlCode: DWord); stdcall;
begin
    IcsTcpSvc.Controller(CtrlCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF DELPHI5_UP}
function TIcsTcpSvc.GetServiceController: TServiceController;
begin
    Result := ServiceController;
end;
{$ELSE}
function TIcsTcpSvc.GetServiceController: PServiceController;
begin
    Result := @ServiceController;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTcpSvc.ServiceStart(Sender: TService; var Started: Boolean);
begin
    try
        FTcpDaemon.Start;
        Started := TRUE;
    except
        Started := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTcpSvc.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
   FTcpDaemon.Stop;
   Stopped := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTcpSvc.ServiceExecute(Sender: TService);
begin
    while not Terminated do
        ServiceThread.ProcessRequests(TRUE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTcpSvc.Display(Msg : String);
begin
    // A service has no access to the GUI.
    // Simply ignore any display :-(
    // We could log messages to Windows EventLog
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTcpSvc.ServiceCreate(Sender: TObject);
begin
    FTcpDaemon           := TTcpDaemon.Create;
    FTcpDaemon.Banner    := DisplayName + ' Ready';
    FTcpDaemon.OnDisplay := Display;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTcpSvc.ServiceDestroy(Sender: TObject);
begin
    if Assigned(FTcpDaemon) then begin
        FTcpDaemon.Destroy;
        FTcpDaemon := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
