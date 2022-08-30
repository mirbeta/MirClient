{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  SSL web application server sample, no real GUI, really designed
              to be a Windows service application.
Creation:     July 2017
Updated:      Oct 2018
Version:      8.58
Support:      Use the mailing list ics-ssl@elists.org
Legal issues: Copyright (C) 2003-2018 by Fran�ois PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

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

Note this web server sample combines the functionality of these two existing samples,
OverbyteIcsWebServ1 for OverbyteIcsHttpSrv, and OverbyteIcsWebAppServerMain for
OverbyteIcsAppHttpSrv.  The main difference is OverbyteIcsAppHttpSrv contains
session support to ease sharing data between pages, and handling of received data
for POST, PUT, PATCH etc so the application does not need it.

This sample is non-interactive, web servers are normally run as windows background
servers.  All the server settings come from an INI file which will need to be
edited before the sample will successfully run.  A bare sample INI file is included
which will be copied into the ICS shared INI directory on first run, with the
actual file name shown when you start the application, and that is the file
to edit.

Unlike the other web server samples, this one uses IcsHosts to support multiple
addresses and ports and SSL certificates, but all of these must exist and not being
used by other applications, otherwise the server will not start.  To use SSL, an
SSL certificate should exist for the host name used, IP addresses don't eeally work
with SSL, the OverbyteIcsPemtool sample allows self signed SSL certificates to be
created for testing.  Up to 100 IcsHosts can be specified, you can edit the Windows
HOSTS file if necessary to create alternate host names for your PC, if you don't
have a local DNS server to do it.  Documentation for IcsHosts may be found in
OverbyteIcsWSocketS.pas.

This web server will automatically order and install SSL certificates if so required,
from various suppliers, including free certificates from Let's Encrypt, and commercial
certificates for Digicert, Comodo, Thawte and GeoTrust from CertCentre AG.  For
automated ordering, Domain Validation is used which means the web server must be
accessible from the public internet by all the host names for which an SSL
certificate is being ordered. See OverbyteIcsSslX509Certs.pas fore more info.


History:
6 July 2017  - V8.49 baseline
20 Sep 2017 - V8.50 - Close connection after sending redirection
12 Dec 2017 - V8.51 - Try and enable FIPS mode if supported by OpenSSL.
2 Oct 2018  - V8.57 - INI file now reads Options as enumerated type literals,
                        ie Options=[hoContentEncoding,hoAllowDirList,hoSendServerHdr,hoAllowPut]
                      INI file reads SslCliCertMethod, SslCertAutoOrder and CertExpireDays
                      Allow SSL certificates to be ordered and installed automatically if
                        SslCertAutoOrder=True and so specified in IcsHosts, and a
                        certificate supplier account has been created (by the
                        OverbyteIcsX509CertsTst sample application).
19 Oct 2018  V8.58 version only


Insalling note:
All web server configuration is in OverbyteIcsSslMultiWebServ.ini file, a default file
is included in the \ics\Samples\Delphi\SslInternet directory, and is copied into the
ICS temporary working directory, usually c:\Users\(login)\AppData\Local\ICS. This
INI file will need to be edited for IP addresses, SSL certificates, etc, although
some defaults may work.  Specifically Host4 has certificate ordering settings but
is disabled and will need new valid hosts, IP addresss and files names before it
will do anything useful.

Once this demo has been run once, any changes to the default INI file from new versions
 (ie auto certificate ordering) will need to copied manually into the working file.

See OverbyteIcsWSocketS.pas for documentation on TSslWSocketServer whose
properties are exposed by TSslHttpAppSrv, and for IcsHosts which allows the web
server to support multiple Hosts on multiple IP addresses and ports with SSL
support, including automatic SSL certificate ordering.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslMultiWebServ1;

{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{ If you use Delphi 7, you may wants to disable warnings for unsage type,   }
{ unsafe code and unsafe typecast in the project options. Those warning are }
{ intended for .NET programs. You may also want to turn off deprecated      }
{ symbol and platform symbol warnings.                                      }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, ComCtrls, TypInfo,
  OverbyteIcsWSocket, OverbyteIcsWSocketS, OverbyteIcsWndControl,
  OverbyteIcsUtils, OverbyteIcsSSLEAY, OverbyteIcsLIBEAY,
  OverbyteIcsLogger, OverbyteIcsSslX509Utils, OverbyteIcsFtpSrvT,
  OverbyteIcsHttpSrv, OverbyteIcsHttpAppServer, OverbyteIcsWebSession,
  OverbyteIcsFormDataDecoder, OverbyteIcsMimeUtils,
{ note following units shared with two other server samples }
  OverbyteIcsWebAppServerDataModule,
  OverbyteIcsWebAppServerSessionData,
  OverbyteIcsWebAppServerConfig,
  OverbyteIcsWebAppServerUrlDefs,
  OverbyteIcsWebAppServerHomePage,
  OverbyteIcsWebAppServerHelloWorld,
  OverbyteIcsWebAppServerCounter,
  OverbyteIcsWebAppServerLogin,
  OverbyteIcsWebAppServerCounterView,
  OverbyteIcsWebAppServerMailer,
{$IFDEF use_DWScript}
  OverbyteIcsWebAppServerDWScriptUrlHandler,
{$ENDIF}
  OverbyteIcsWebAppServerHead,
  OverbyteIcsWebAppServerUploads,
  OverbyteIcsTicks64,       { V8.57 }
  OverbyteIcsSslX509Certs,  { V8.57 }
  OverbyteIcsSslHttpRest;   { V8.57 }
const
    SrvCopyRight : String = ' OverbyteIcsSslMultiWebServ (c) 2018 Francois Piette V8.58 ';
    MaxWinChars = 800000;
    WM_STARTUP = WM_USER + 712 ;
    SimpLogName = '"webapp-"yyyymmdd".log"' ;
    WebLogFields = '#Fields: date time s-sitename s-computername s-ip cs-method cs-uri-stem cs-uri-query ' +
                   's-port cs-username c-ip cs-version cs(User-Agent) cs(Referer) cs-host sc-status ' +
                   'sc-bytes cs-bytes time-taken' ;
    ISODateMask = 'yyyy-mm-dd' ;
    ISODateTimeMask = 'yyyy-mm-dd"T"hh:nn:ss' ;
    ISOTimeMask = 'hh:nn:ss' ;
    XmitBufSize = 65536 ;
    RecvBufSize = 65536;
//  FILE_UPLOAD_URL    = '/cgi-bin/FileUpload/';
//  UPLOAD_DIR         = 'upload\';
  MAX_UPLOAD_SIZE    = 1024 * 1024 * 60; // Accept max 60MB file

type
  TMyHttpConnection = class(THttpAppSrvConnection)
  public
    CStartTick        : Longword;
    CLastRead         : Int64;
    CLastWrite        : Int64;
    FRespTimer        : TTimer;    { send a delayed response }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure TimerRespTimer(Sender: TObject);
  end ;
         
type
  TWeblServerForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    StartButton: TButton;
    StopButton: TButton;
    Timer1: TTimer;
    RecheckCertsButton: TButton;
    SslHttpAppSrv1: TSslHttpAppSrv;
    DisplayHeaderCheckBox: TCheckBox;
    IcsSslX509Certs: TSslX509Certs;
    procedure WMCMSTARTUP (var Msg : TMessage); message WM_STARTUP ;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure RecheckCertsButtonClick(Sender: TObject);
    procedure SslHttpAppSrv1ClientConnect(Sender, Client: TObject; Error: Word);
    procedure SslHttpAppSrv1ServerStarted(Sender: TObject);
    procedure SslHttpAppSrv1ServerStopped(Sender: TObject);
    procedure SslHttpAppSrv1WellKnownDir(Sender, Client: TObject;
      const Path: string; var BodyStr: string);
    procedure SslHttpAppSrv1DeleteSession(Sender: TObject;
      Session: TWebSession);
    procedure SslHttpAppSrv1GetDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1AfterAnswer(Sender, Client: TObject);
    procedure SslHttpAppSrv1BeforeProcessRequest(Sender, Client: TObject);
    procedure SslHttpAppSrv1VirtualException(Sender: TObject; E: Exception;
      Method: THttpMethod; const Path: string);
    procedure SslHttpAppSrv1SslHandshakeDone(Sender: TObject; ErrCode: Word;
      PeerCert: TX509Base; var Disconnect: Boolean);
    procedure SslHttpAppSrv1BgException(Sender: TObject; E: Exception;
      var CanClose: Boolean);
    procedure SslHttpAppSrv1Display(Sender: TObject; const Msg: string);
    procedure SslHttpAppSrv1HeadDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1HttpMimeContentType(Sender, Client: TObject;
      const FileName: string; var ContentType: string);
    procedure SslHttpAppSrv1DeleteDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1PatchDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1PutDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1AuthGetType(Sender, Client: TObject);
    procedure SslHttpAppSrv1AuthGetPassword(Sender, Client: TObject;
      var Password: string);
    procedure SslHttpAppSrv1AuthResult(Sender, Client: TObject;
      Success: Boolean);
    procedure SslHttpAppSrv1AuthNtlmBeforeValidate(Sender, Client: TObject;
      var Allow: Boolean);
    procedure SslHttpAppSrv1OptionsDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1PostDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1TraceDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure IcsSslX509CertsCertProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure IcsSslX509CertsChallengeDNS(Sender: TObject;
      ChallengeItem: TChallengeItem);
    procedure IcsSslX509CertsNewCert(Sender: TObject);
    procedure IcsSslX509CertsOAuthAuthUrl(Sender: TObject; const URL: string);
//    procedure SslHttpAppSrv1PostedData(Sender, Client: TObject; Error: Word);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FDataDir     : String;
    FSessionFile : String;
    FCountRequests : Integer;
    FUploadsDir  : String;
    procedure CreateVirtualDocument_Demo(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_DemoAuthAll(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_DemoNtlmAuth(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_DemoDigestAuth(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_DemoBasicAuth(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_Time(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_Bruno(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_Redir(Sender    : TObject;
                                             ClientCnx : TMyHttpConnection;
                                             var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_MyIP(Sender    : TObject;
                                             ClientCnx : TMyHttpConnection;
                                             var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_HeaderBug(Sender    : TObject;
                                              ClientCnx : TMyHttpConnection;
                                              var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_Template(Sender    : TObject;
                                             ClientCnx : TMyHttpConnection;
                                             var Flags : THttpGetFlag);
    procedure DisplayHeader(ClientCnx : TMyHttpConnection);
  public
    procedure Display(Msg : String);
    procedure HttpAppSrvClientBgException(Sender: TObject; E: Exception; var CanClose : Boolean);
    procedure ReportHosts;
    property IniFileName : String read FIniFileName write FIniFileName;
    property DataDir : String read FDataDir write FDataDir;
    property UploadsDir : String read FUploadsDir write FUploadsDir;
  end;

var
    WeblServerForm: TWeblServerForm;
    WinLinesBuff: string ;
    WinDispCur: Integer;
    ProgDirectory: String;
    LogDate: Integer;
    SrvCompName: string;
    StatSrvSslCert: String;
    StatSrvSslCertWeb: String;
    HouseKeepingTrg: int64;    { V8.57 }
    CertCheckTrigger: int64 ;  { V8.57 }
    LockFileAccess: TRtlCriticalSection;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ simple log file, writes Msg to text file in progam directory with FNameMask
  unless mask includes // or x:
  mask generally includes date only - "mylog-"yyyymmdd".log"
  path needed quote at start - "c:\temp\mylog-"yyyymmdd".log"  }
procedure SimpleLogging (const FNameMask, Msg: String);
var
    LogFileName, S: String;
    f: TextFile;
begin
    S := FormatDateTime (ISOTimeMask, Time) + ' ' + Msg;
    try
        LogFileName := FormatDateTime (FNameMask, Date) ;
        if (Pos ('//', LogFileName) <> 1) and (Pos (':', LogFileName) <> 2) then
            LogFileName := ExtractFileDir (ParamStr (0)) + '\' + LogFileName ;
        AssignFile (f, LogFileName);
        if FileExists (LogFileName) then
        begin
            Append (f);
        end
        else
        begin
            Rewrite (f);
        end;
        Writeln (f, S);
        CloseFile(f);
    except
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMyHttpConnection.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
  { keep alive means connection may be used for multiple requests so we must track how much
    data is sent before and after each request }
    CLastRead := 0 ;
    CLastWrite := 0 ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /delayed.html document when a timer expires }
procedure TMyHttpConnection.TimerRespTimer(Sender: TObject);
var
    Flags: THttpGetFlag;
begin
    if NOT Assigned (FRespTimer) then exit ;
    FRespTimer.Enabled := false ;
    Flags := hgWillSendMySelf;
    AnswerString(Flags,
        '',                            { Default Status '200 OK'            }
        '',                            { Default Content-Type: text/html    }
        'Pragma: no-cache' + icsCRLF +  { No client caching please           }
        'Expires: -1'      + icsCRLF,   { I said: no caching !               }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer Demo</TITLE>' +
          '</HEAD>' + icsCRLF +
          '<BODY>' +
            '<H2>This page was deliberately returned slowly</H2>' + icsCRLF +
            '<H2>Time at server side:</H2>' + icsCRLF +
            '<P>' + DateTimeToStr(Now) +'</P>' + icsCRLF +
            '<A HREF="/demo.html">Demo menu</A>' + icsCRLF +
          '</BODY>' +
        '</HTML>');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We need to override parent class destructor  }
destructor TMyHttpConnection.Destroy;
begin
    FreeAndNil (FRespTimer);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ we update DisplayMemo once a second in the timer }
procedure TWeblServerForm.Display(Msg : String);
begin
    WinLinesBuff := WinLinesBuff + Msg + icsCRLF ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.Timer1Timer(Sender: TObject);
var
    displen, removelen, newstart: integer ;
    S1: String ;
    NewFlag: Boolean;
begin

  // check SSL certificates every two hours, may order expired certs
    if IcsTestTrgTick64 (CertCheckTrigger) then  { V8.57 }
    begin
        CertCheckTrigger := IcsGetTrgMins64 (120) ;
        try
          // don't stop on first error, no exceptions
            NewFlag := SslHttpAppSrv1.RecheckSslCerts(S1, False, True);
            if NewFlag or (S1 <> '') then  begin
                if NewFlag then Display('Server Recheck Loaded New SSL Certificate(s)');
                Display('Server Recheck SSL Certificate Errors:' + icsCRLF + S1);
                ReportHosts;    // report everything again
                Display('Listen Bindings:' + icsCRLF + SslHttpAppSrv1.ListenStates);
            end;
        except
            on E:Exception do begin
               Display('Server Recheck SSL Certificate Failed - ' + E.Message);
            end;
        end;
    end;

  // rotate logs at midnight, recheck SSL certificates, log configuration again
    if LogDate <> Trunc(Date) then begin
        LogDate := Trunc(Date);
        Display('Nightly Server Recheck Starting');
        ReportHosts;    // report everything again
        Display('Listen Bindings:' + icsCRLF + SslHttpAppSrv1.ListenStates);
        CertCheckTrigger := Trigger64Immediate; { V8.57 }
    end;

  // see if updating the log window with multiple lines
    displen := Length (WinLinesBuff) ;
    if displen > 0 then begin
        try
            if WinDispCur + displen > MaxWinChars then begin
                S1 := DisplayMemo.Lines.Text ;
                removelen := MaxWinChars - displen - 20000 ;
                if removelen > 20000 then begin
                    S1 := copy (S1, removelen, 9999999) ;
                    newstart := Pos(#13, S1) ; // find start of next line
                    DisplayMemo.Text := copy (S1, newstart, 9999999) + WinLinesBuff ;
                    WinDispCur := Length (S1) - newstart + displen ;
                end
                else begin
                    DisplayMemo.Lines.Text := WinLinesBuff ;
                    WinDispCur := displen ;
                end;
            end
            else begin
                SetLength (WinLinesBuff, displen - 2) ;  // remove CRLF
                DisplayMemo.Lines.Add (WinLinesBuff) ;
                WinDispCur := WinDispCur + displen ;
                SendMessage (DisplayMemo.Handle, WM_VSCROLL, SB_BOTTOM, 0);
            end;
        except
        end ;
        WinLinesBuff := '' ;
    end;

 // house keeping every five minutes
    if IcsTestTrgTick(HouseKeepingTrg) then begin
        HouseKeepingTrg := IcsGetTrgMSecs64 (300);
        CleanupTimeStampedDir(WebAppSrvDataModule.DataDir);
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.FormCreate(Sender: TObject);
var
    mode: integer;
begin
    FIniFileName := GetIcsIniFileName;
  // ensure SSL DLLs come from program directory, and exist, else die
 // GSSLEAY_DLL_IgnoreNew := true ; // !!! TEMP TESTING
    GSSLEAY_DLL_IgnoreOld := true;
    ProgDirectory := ExtractFileDir(Lowercase (ParamStr(0)));
    GSSL_DLL_DIR := ProgDirectory + '\';
    GSSL_SignTest_Check := True;
    GSSL_SignTest_Certificate := True;
    OverbyteIcsWSocket.LoadSsl;
    LogDate := Trunc(Date);
    HouseKeepingTrg := IcsGetTrgSecs64 (300);
    CertCheckTrigger := Trigger64Disabled;  { V8.57 }

{ V8.51 see if using FIPS OpenSSL DLLs, try and set FIPS mode }
    if Pos ('fips', OpenSslVersion) > 0 then begin
        mode := f_fips_mode_set(1);
        if mode <> 0 then
            Display('OpenSSL FIPS 140-2 self test successful')
         else
            Display('OpenSSL FIPS 140-2 self test failed - ' +
                                            String(LastOpenSslErrMsg(False))) ;
    end;
    PostMessage (Handle, WM_STARTUP, 0, 0) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.FormDestroy(Sender: TObject);
begin
    OverbyteIcsWSocket.UnLoadSsl;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIcsIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        IniFile.Free;
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.RecheckCertsButtonClick(Sender: TObject);
begin
    CertCheckTrigger := Trigger64Immediate; 
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.FormClose(
    Sender     : TObject;
    var Action : TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    CertCheckTrigger := Trigger64Disabled;  { V8.57 }
    HouseKeepingTrg := Trigger64Disabled;  { V8.57 }
    StopButtonClick(Self);
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.WMCMSTARTUP (var Msg : TMessage);
begin
    StartButtonClick(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.ReportHosts;
var
    I: Integer;
begin
    StatSrvSslCert := '';
    Display('Total server hosts ' + IntToStr(SslHttpAppSrv1.IcsHosts.Count)) ;
    for I := 0 to SslHttpAppSrv1.IcsHosts.Count - 1 do begin
        with SslHttpAppSrv1.IcsHosts[I] do begin
            if CertValRes = chainFail then begin
                Display('Server SSL Certificate Errors - ' + DisplayName);
              { might want to stop here and warn user }
           //     Exit;
            end;

        // build site info for log and status page
            StatSrvSslCert := StatSrvSslCert + 'Site Host ' + IntToStr (I) +
             ' [' + DisplayName + '] ' + HostNames.CommaText + icsCRLF +
             'Bindings: ' + BindInfo + icsCRLF +
             'SSL Security Level: ' + GetEnumName(TypeInfo(TSslSrvSecurity), Ord(SslSrvSecurity)) + IcsCRLF +
                    'SSL Certificate: ' + CertErrs + IcsCRLF + CertInfo + IcsCRLF +
             'Web Pages Root: ' + WebDocDir + icsCRLF +
             'Templates Root: ' + WebTemplDir + icsCRLF;
            if WebRedirectStat <> 0 then StatSrvSslCert := StatSrvSslCert +
                                    'Redirection to: ' + WebRedirectURL + icsCRLF;
            if WellKnownPath <> '' then StatSrvSslCert := StatSrvSslCert +
                                    'Well-Known Root: ' + WellKnownPath + icsCRLF;
             StatSrvSslCert := StatSrvSslCert + icsCRLF;
        end;
    end;
    StatSrvSslCertWeb := StringReplace (StatSrvSslCert, icsCRLF, '<BR>' + icsCRLF, [rfReplaceAll]);
    Display(StatSrvSslCert);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.DisplayHeader(ClientCnx : TMyHttpConnection);
var
    I : Integer;
    S: String;
begin
    if not DisplayHeaderCheckBox.Checked then
        Exit;
    S := ClientCnx.Method + ' ' + ClientCnx.Path;
    if ClientCnx.Params <> '' then S := S + '?' + ClientCnx.Params;
    S := S + ' ' + ClientCnx.Version;
    Display('HDR0) ' + S);
    for I := 0 to ClientCnx.RequestHeader.Count - 1 do
        Display('HDR' + IntToStr(I + 1) + ') ' +
                ClientCnx.RequestHeader.Strings[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.StartButtonClick(Sender: TObject);
var
    IniFile : TIcsIniFile;
    List: TStringList;
    J: Integer;
    Errs, S, BaseDir: String;

    function BuildDemoURIs: String;
    var
        I: integer;
    begin
        Result := '' ;
        for I:= 0 to SslHttpAppSrv1.IcsHosts.Count - 1 do begin
            with SslHttpAppSrv1.IcsHosts [I] do begin
                if NOT HostEnabled then continue;
                if BindNonPort <> 0 then begin
                    Result := Result + 'http://' + SslHttpAppSrv1.IcsHosts [0].HostNames[0];
                    if BindNonPort <> 80 then Result := Result + ':' + IntToStr(BindNonPort);
                    Result := Result + '/' + WebDefDoc + icsCRLF;
                end;
                if BindSslPort <> 0 then begin
                    Result := Result + 'https://' + SslHttpAppSrv1.IcsHosts [0].HostNames[0];
                    if BindSslPort <> 443 then Result := Result + ':' + IntToStr(BindSslPort);
                    Result := Result + '/' + WebDefDoc + icsCRLF;
                end;
            end;
        end;
    end;

begin
    // In this SSL demo, we use the files of the non-SSL demo which
    // is located in ..\WebDemos
    BaseDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + '..\WebDemos\';
    BaseDir := AbsolutisePath(BaseDir);
    FDataDir := BaseDir + 'WebAppServerData\Data';
    FUploadsDir := BaseDir + 'WebAppServerData\Uploads';
    FSessionFile := FDataDir + '\Sessions.dat';

    try
      { try and open INI file in C:\Users\(me)\AppData\Local\ICS, if missing copy
        default from sample directory  }
        if NOT FileExists(FIniFileName) then begin
            S := ChangeFileExt(IcsLowercase(ParamStr(0)), '.ini');
            if FileExists(S) then begin
                List := TStringList.Create;
                try
                    List.LoadFromFile(S);
                    List.SaveToFile(FIniFileName);
                    Display('Copied default INI file to: ' + FIniFileName + ', where it should be edited');
                except
                    List.Free;
                end;
            end;
        end;
        IniFile := TIcsIniFile.Create(FIniFileName);
        Display('INI file: ' + FIniFileName);

      // tell them who we are
        Display(OverbyteIcsHttpSrv.CopyRight + icsCRLF +
           'SSL Version: ' + OpenSslVersion + ', Dir: ' + GLIBEAY_DLL_FileName + icsCRLF);

      // main proxy settings from INI file, built in CA if no file found
        IcsLoadTHttpAppSrvFromIni(IniFile, SslHttpAppSrv1, 'WebAppServer');
        if (SslHttpAppSrv1.RootCA = '') or (NOT FileExists(SslHttpAppSrv1.RootCA)) then
                                         SslHttpAppSrv1.RootCA := sslRootCACertsBundle;

      // read the server hosts from INI file and check SSL files exist
        IcsLoadIcsHostsFromIni(IniFile, SslHttpAppSrv1.IcsHosts, 'Host');
        if SslHttpAppSrv1.IcsHosts.Count <= 0 then begin
            Display('Can Not Start Server - No Source Server Hosts Configured') ;
            exit ;
        end;
        for J := 0 to SslHttpAppSrv1.IcsHosts.Count - 1 do begin
            with SslHttpAppSrv1.IcsHosts [J] do begin
                if NOT HostEnabled then continue;

            // special case, add ICS sample directory path to directories without drive letters
                if Pos ('WebAppServerData\', WebDocDir) = 1 then WebDocDir := BaseDir + WebDocDir;
                if Pos ('WebAppServerData\', WebTemplDir) = 1 then WebTemplDir := BaseDir + WebTemplDir;

            // create web logging file name, use INI directory if nothing better specified
                if WebLogDir = '' then WebLogDir := IncludeTrailingPathDelimiter(ExtractFileDir(FIniFileName)) + HostTag;
                ForceDirectories(WebLogDir);
                WebLogDir := '"' + IncludeTrailingPathDelimiter(WebLogDir) + '"' + SimpLogName;  // file name is a mask to add date
            end;
        end;

    // validate hosts and keep site certificiate information
        try
            Errs := SslHttpAppSrv1.ValidateHosts(False, True); // don't stop on first error, no exceptions
            if Errs <> '' then begin
                Display('Proxy Validation Errors:' + icsCRLF + Errs);
            end;
            ReportHosts;
            Display('Required Listen Bindings:' + icsCRLF + SslHttpAppSrv1.ListenStates);
        except
            on E:Exception do begin
                Display('Host Validation Failed, Server Stopped - ' + E.Message);
                Exit;
            end;
        end;

    // setup some web server defauls, most were done in IcsLoadTHttpAppSrvFromIni
        SslHttpAppSrv1.TemplateDir :=  SslHttpAppSrv1.IcsHosts [0].WebTemplDir;
        SslHttpAppSrv1.DocDir :=  SslHttpAppSrv1.IcsHosts [0].WebDocDir;
        SslHttpAppSrv1.DefaultDoc :=  SslHttpAppSrv1.IcsHosts [0].WebDefDoc;
        SslHttpAppSrv1.ServerHeader := DefServerHeader;  // get latest version
        SslHttpAppSrv1.ClientClass := TMyHttpConnection;
        SslHttpAppSrv1.Options := SslHttpAppSrv1.Options + [hoContentEncoding];  // compress replies
        SslHttpAppSrv1.MaxBlkSize := XmitBufSize;
        SslHttpAppSrv1.SocketErrs := wsErrFriendly ;
        SslHttpAppSrv1.ExclusiveAddr := true ;
        if SslHttpAppSrv1.SessionTimeout < 30 then SslHttpAppSrv1.SessionTimeout := 300;  // sanity check

     // Force directory creation
        ForceDirectories(FDataDir);
        ForceDirectories(FUploadsDir);
        if SslHttpAppSrv1.TemplateDir <> '' then
            ForceDirectories(SslHttpAppSrv1.TemplateDir);
        ForceDirectories(SslHttpAppSrv1.DocDir);
        ForceDirectories(SslHttpAppSrv1.DocDir + '\Js');
        ForceDirectories(SslHttpAppSrv1.DocDir + '\Styles');
        ForceDirectories(SslHttpAppSrv1.DocDir + '\Images');

        WebAppSrvDataModule.IniFileName := FIniFileName;
        WebAppSrvDataModule.OnDisplay   := SslHttpAppSrv1Display;
        WebAppSrvDataModule.DataDir     := FDataDir;
        WebAppSrvDataModule.ImagesDir   := SslHttpAppSrv1.DocDir + '\Images';
        WebAppSrvDataModule.LoadConfig;

      // note that AllowedPaths and Handlers must match a HostTag for each Host in INI file

      // simple web server, no handlers, allow access to all static files
         SslHttpAppSrv1.AddGetAllowedPath('/', afBeginBy, 'HTTP-WEB');

      // application web server, only allow access to folders where static documents are.
        SslHttpAppSrv1.AddGetAllowedPath('/',        afBeginBy, 'WEB-APP');
        SslHttpAppSrv1.AddGetAllowedPath('/js/',     afBeginBy, 'WEB-APP');
        SslHttpAppSrv1.AddGetAllowedPath('/styles/', afBeginBy, 'WEB-APP');
        SslHttpAppSrv1.AddGetAllowedPath('/images/', afBeginBy, 'WEB-APP');

      // Add all dynamic webpage handlers
//        SslHttpAppSrv1.AddGetHandler('/', TUrlHandlerDefaultDoc, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler('/appindex.html', TUrlHandlerDefaultDoc, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlLogin, TUrlHandlerLoginFormHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlDoLoginSecure, TUrlHandlerDoLoginSecureHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlCounter, TUrlHandlerCounterJpg, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlHomePage, TUrlHandlerHomePageHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlConfigForm, TUrlHandlerConfigFormHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlConfigLogoPng, TUrlHandlerConfigLogoPng, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlDoConfigConfirmSaveHtml, TUrlHandlerDoConfigConfirmSaveHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddPostHandler(UrlDoConfigHtml, TUrlHandlerDoConfigHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlCounterViewHtml, TUrlHandlerCounterViewHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlAjaxFetchCounter, TUrlHandlerAjaxFetchCounter, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlJavascriptErrorHtml, TUrlHandlerJavascriptErrorHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler('/mailer.html', TUrlHandlerMailer, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddPostHandler('/mailer.html', TUrlHandlerMailer, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlHeadForm, TUrlHandlerHead, hgWillSendMySelf, 'WEB-APP');
    {$IFDEF use_DWScript}
        SslHttpAppSrv1.AddGetHandler('/DWScripts/*', TUrlHandlerDWScript, hgWillSendMySelf, 'WEB-APP');
    {$ENDIF}

      // Just for demoing the simplest handler, let's add an "Helloworld" one
        SslHttpAppSrv1.AddGetHandler('/HelloWorld.html', TUrlHandlerHelloWorld, hgWillSendMySelf, 'WEB-APP');

      // these handlers replace the POST code in OverbyteIcsWebServ1.pas that does not work for HttpAppSrv
        SslHttpAppSrv1.AddPostHandler('/cgi-bin/FormHandler', TUrlHandlerUploadData, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler('/uploadfile.html', TUrlHandlerUploadFile, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddPostHandler('/uploadfile.html', TUrlHandlerUploadFile, hgWillSendMySelf, 'WEB-APP');

      // look for old sessions
        if FileExists(FSessionFile) then begin
            try
                SslHttpAppSrv1.WSessions.LoadFromFile(FSessionFile);
                Display(IntToStr(SslHttpAppSrv1.SessionsCount) + ' sessions loaded');
            except
                // Ignore any exception, but clear anything partially loaded
                SslHttpAppSrv1.WSessions.Clear;
                // and delete existing (corrupted) file
                DeleteFile(FSessionFile);
                Display('Unable to load existing sessions');
            end;
        end;

      // Cleanup temporary files left from a previous run
        CleanupTimeStampedDir(WebAppSrvDataModule.DataDir);

      // start logging file for each host
        SrvCompName := IcsGetCompName ;
        for J := 0 to SslHttpAppSrv1.IcsHosts.Count - 1 do begin
            with SslHttpAppSrv1.IcsHosts [J] do begin
                if NOT HostEnabled then continue;
                Display('W3C logging for host: ' + HostTag + ' to ' + FormatDateTime(WebLogDir, Date)) ;
                SimpleLogging (WebLogDir, FormatDateTime ('"#Date: "' + ISODateMask + #32 + ISOTimeMask + '"', Now)) ;
                SimpleLogging (WebLogDir, WebLogFields) ;
            end;
        end;

      // start as many hosts as possible
        Errs := SslHttpAppSrv1.Start (True) ;
        if Errs <> '' then Display('Start Web Server Error - ' + Errs) ;
        if NOT SslHttpAppSrv1.ListenAllOK then
            Display('Failed to Start, Listen Bindings:' +
                                      icsCRLF + SslHttpAppSrv1.ListenStates)
        else
            Display('Started OK, Listen Bindings:' +
                                      icsCRLF + SslHttpAppSrv1.ListenStates);
        CertCheckTrigger := IcsGetTrgSecs64 (15) ;  { V8.57 first check is early to order new certificates }
        StartButton.Enabled := false;
        StopButton.Enabled := true;

     // try and show URLSs that will access the first host
        Display('Now browse to one of these URLs:' + icsCRLF + BuildDemoURIs);

    except
        on E:Exception do begin
           Display('Failed to start web server - ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.StopButtonClick(Sender: TObject);
begin
    if NOT StopButton.Enabled then Exit;
    CertCheckTrigger := Trigger64Disabled;  { V8.57 }
    StartButton.Enabled := true;
    StopButton.Enabled := false;
    SslHttpAppSrv1.Stop;
    Display('Server stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ write W3C log, similar to Microsoft IIS, log line looks something like this:
12:34:48 2009-07-03 12:34:48 WebAppDemo PC19 0.0.0.0 GET /login/loginform.html - 20105 - 192.168.1.119 HTTP/1.1 Mozilla/5.0+(Windows;+U;+Windows+NT+5.1;+en-GB;+rv:1.9.0.11)+Gecko/2009060215+Firefox/3.0.11+(.NET+CLR+3.5.30729) http://pc19:20105/ pc19:20105 200 2101 1138 31 }
procedure TWeblServerForm.SslHttpAppSrv1AfterAnswer(Sender,
  Client: TObject);
var
    RemoteClient: TMyHttpConnection;
    newparams, newuser, info: string ;
    duration: longword ;
    curread, curwrite: int64 ;

    function SpaceToPlus (const S: String): String ;
    begin
        result := StringReplace (S, #32, '+', [rfReplaceAll]) ;
    end;

begin
    RemoteClient := TMyHttpConnection(Client) ;
    info := FormatDateTime (ISODateMask + #32 + ISOTimeMask, Now) ;
    with RemoteClient do
    begin
        try
            newparams := Params ;
            if newparams = '' then newparams := '-' ;
            newuser := SpaceToPlus (AuthUserName)  ;
            if newuser = '' then newuser := '-' ;
            curread := ReadCount - CLastRead ;
            curwrite := WriteCount - CLastWrite ;
            duration := IcsElapsedMSecs (CStartTick) ;
// #Fields: date time s-sitename s-computername s-ip cs-method
            info := info + #32 + HostTag + #32 + SrvCompName + #32 + Server.Addr + #32 + Method + #32 +
// #Fields: cs-uri-stem cs-uri-query s-port
                    SpaceToPlus (Path) + #32 + SpaceToPlus (newparams) + #32 + Server.Port + #32 +
//  #Fields: cs-username c-ip cs-version
                    newuser + #32 + GetPeerAddr + #32 + Version + #32 +
// #Fields: cs(User-Agent) cs(Referer)
                    SpaceToPlus (RequestUserAgent) + #32 + SpaceToPlus (RequestReferer) + #32 +
// #Fields: cs-host sc-status
                    RequestHost + #32 + IntToStr (FAnswerStatus) + #32 +
// #Fields: sc-bytes cs-bytes time-taken
                    IntToStr (curwrite) + #32 + IntToStr (curread) + #32 + IntToStr (duration);
            SimpleLogging (RemoteClient.WebLogDir, info) ;   // note separate file name for each host
        except
            Display ('AfterAnswer Error - ' + info) ;
        end;
    end;
    RemoteClient.CLastRead := RemoteClient.ReadCount ;   // reset read ready for next request
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1AuthGetPassword(Sender, Client: TObject;
  var Password: string);
var
    ClientCnx  : TMyHttpConnection;
begin
    { It's easyer to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);
    Display('AuthGetPassword for "' + ClientCnx.Path + '" AuthType is "' +
            AuthTypesToString(ClientCnx.AuthTypes) + '"');
    if (ClientCnx.AuthTypes     = [atDigest]) and
       (ClientCnx.AuthUserName = 'test') then
        Password := 'digest'
    else if (ClientCnx.AuthTypes     = [atBasic]) and
            (ClientCnx.AuthUserName = 'test') then
        Password := 'basic'
    else if (ClientCnx.AuthTypes = [atNtlm]) then ;
        //  nothing to do windows will validate credentials

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1AuthGetType(Sender, Client: TObject);
var
    ClientCnx  : TMyHttpConnection;
begin
    { It's easyer to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);
    if CompareText(ClientCnx.Path, '/DemoBasicAuth.html') = 0 then begin
        ClientCnx.AuthTypes  := [atBasic];
        ClientCnx.AuthRealm := 'DemoBasicAuth';
    end
    else if CompareText(ClientCnx.Path, '/DemoDigestAuth.html') = 0 then begin
        ClientCnx.AuthTypes  := [atDigest];
        ClientCnx.AuthRealm := 'DemoDigestAuth';
    end
    else if CompareText(ClientCnx.Path, '/DemoNtlmAuth.html') = 0 then begin
        ClientCnx.AuthTypes  := [atNtlm];
        ClientCnx.AuthRealm := 'DemoNtlmAuth';
    end
    else if CompareText(ClientCnx.Path, '/DemoAuthAll.html') = 0 then begin
        ClientCnx.AuthTypes  := [atBasic, atDigest, atNtlm];
        ClientCnx.AuthRealm := 'DemoAuthAll';
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1AuthNtlmBeforeValidate(Sender,
  Client: TObject; var Allow: Boolean);
var
    ClientCnx  : TMyHttpConnection;
begin
    ClientCnx := TMyHttpConnection(Client);
    Allow := (ClientCnx.AuthNtlmSession.Username <> '') and
             (ClientCnx.AuthNtlmSession.Domain <> 'SomeDomain');

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1AuthResult(Sender, Client: TObject;
  Success: Boolean);
var
    ClientCnx  : TMyHttpConnection;
const
    SuccessStr : array [Boolean] of String = ('failed', 'OK');
begin
    { It's easier to do the cast one time. Could use with clause...         }
    ClientCnx := TMyHttpConnection(Client);

    { If we always want to pop up client browser's login dialog with digest }
    { authentication when the nonce is stale we may set FAuthDigestStale    }
    { back to FALSE.  Note: Do not set this value to TRUE.                  }
    { A nonce is considered stale after AuthDigestNonceLifeTimeMin expired. }
    { Uncomment next three lines to see what changes.                       }
    {if (not Success) and (ClientCnx.AuthTypes = [atDigest]) and
       ClientCnx.FAuthDigestStale then
        ClientCnx.FAuthDigestStale := FALSE;}

    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] authentication ' +
            SuccessStr[Success] + ' for ' +
            ClientCnx.Path);

    if (not Success) and (ClientCnx.AuthTypes = [atNtlm]) and
       (ClientCnx.AuthNtlmSession <> nil) then
        Display(ClientCnx.AuthNtlmSession.AuthErrorDesc);  // just for debugging!

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1BeforeProcessRequest(Sender,
  Client: TObject);
var
    RemoteClient: TMyHttpConnection;
    I: integer ;
    CProtocol: String;
begin
    RemoteClient := TMyHttpConnection(Client) ;
    CProtocol := RemoteClient.RequestProtocol + '://';
    RemoteClient.CStartTick := IcsGetTickCountX ;
    RemoteClient.CLastWrite := RemoteClient.WriteCount ;

  // check for absolute URL, strip off protocol and host
    if Pos (CProtocol, RemoteClient.Path) = 1 then begin
        for I := (Length (CProtocol) + 1) to Length (RemoteClient.Path) do begin
            if RemoteClient.Path [I] = '/' then begin
                Display('Found and Trimmed Absolute URL: ' + RemoteClient.Path);
                RemoteClient.Path := Copy (RemoteClient.Path, I, 999) ;
                Break ;
            end;
        end;
    end;


end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1BgException(Sender: TObject;
  E: Exception; var CanClose: Boolean);
begin
    Display('Exception processing page - ' +
            E.ClassName + ': ' + E.Message);
    CanClose := FALSE;  // Let the server continue to accept connections
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1ClientConnect(Sender,
  Client: TObject; Error: Word);
var
    ClientCnx : TMyHttpConnection;
begin
    ClientCnx                := Client as TMyHttpConnection;
    ClientCnx.WSessionCookie := 'OverbyteIcsWebAppServer' + SslHttpAppSrv1.Port;
    ClientCnx.OnBgException  := HttpAppSrvClientBgException;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1DeleteDocument(Sender, Client: TObject;
  var Flags: THttpGetFlag);
var
    ClientCnx  : TMyHttpConnection;
    Dummy     : THttpGetFlag;
begin
    if Flags = hg401 then
    { Not authenticated (yet), we might be still in an authentication       }
    { session with ClientCnx.RequestContentLength = 0 which was valid,      }
    { i.e. NTLM uses a challenge/response method, anyway just exit.         }
        Exit;

    { It's easier to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);

    { Count request and display a message }
    InterlockedIncrement(FCountRequests);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
            ': ' + ClientCnx.Version + ' DELETE ' + ClientCnx.Path);
    DisplayHeader(ClientCnx);

    { path may specify a file name or just a name, something we want to delete    }

    { tell user }
    Flags := hgWillSendMySelf;
    ClientCnx.AnswerString(Dummy,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer DELETE Demo</TITLE>' +
          '</HEAD>' + icsCRLF +
          '<BODY>' +
            '<H2>Your DELETE request has been noted:</H2>' + icsCRLF +
            '<P>Command: ' + ClientCnx.Path + '</P>' + icsCRLF +
            '<A HREF="/demo.html">Back to demo menu</A><BR>' +
          '</BODY>' +
        '</HTML>');

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1DeleteSession(Sender: TObject;
  Session: TWebSession);
var
    MySessionData : TAppSrvSessionData;
begin
    MySessionData := Session.SessionData as TAppSrvSessionData;
    Display('Session for user "' + MySessionData.UserCode + '" timed out');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1Display(Sender: TObject;
  const Msg: string);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1GetDocument(Sender, Client: TObject;
  var Flags: THttpGetFlag);
var
    ClientCnx : TMyHttpConnection;
    RespStatus, S: String;
begin
    ClientCnx := Client as TMyHttpConnection;
    S := ClientCnx.PeerAddr + ' - ' + ClientCnx.HostTag + ' ' + ClientCnx.Path;
    if ClientCnx.Params = '' then
        Display(S)
    else
        Display(S + '?' + ClientCnx.Params);
    DisplayHeader(ClientCnx);

 { if starting authentication, don't process any pages yet }
    if Flags = hg401 then
        Exit;

  { some hosts are purely for redirection }
    if ClientCnx.WebRedirectStat <> 0 then begin
        Flags := hgWillSendMySelf;
         case ClientCnx.WebRedirectStat of
            301: RespStatus := '301 Moved Permanently';
            302: RespStatus := '302 Found';
            307: RespStatus := '307 Temporary Rediect';
            308: RespStatus := '308 Permanent Rediect';
         else
            RespStatus := IntToStr(ClientCnx.WebRedirectStat) + ' Unknown';
         end;
        Flags := hgWillSendMySelf ;
        ClientCnx.KeepAlive := False ;   // V8.50 must close connection for redirect
        ClientCnx.AnswerString (Flags, RespStatus, '', 'Location: ' + ClientCnx.WebRedirectURL,
            '<HTML>' +
              '<HEAD>' +
                '<TITLE>Redirection</TITLE>' +
              '</HEAD>' + icsCRLF +
              '<BODY>' +
                'You should be redirected automatically !<BR>' + icsCRLF +
                '<A HREF="' + ClientCnx.WebRedirectURL + '">Click Here</A><BR>' + icsCRLF +
              '</BODY>' +
            '</HTML>');
        Exit;
    end;

 { webapp host is the one we handle here, mostly }
    if ClientCnx.HostTag = 'WEB-APP' then begin

      // note many special pages are processed by AddGetHandlers set earlier

        { Instead of the long if/then/else below, we could use a lookup table  }
        if (CompareText(ClientCnx.Path, '/demo.html') = 0 ) or
                                               (ClientCnx.Path = '/') then   // this is out default document
            CreateVirtualDocument_Demo(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/demoBasicAuth.html') = 0 then
            CreateVirtualDocument_DemoBasicAuth(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/demoDigestAuth.html') = 0 then
            CreateVirtualDocument_DemoDigestAuth(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/demoNtlmAuth.html') = 0 then
            CreateVirtualDocument_DemoNtlmAuth(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/demoAuthAll.html') = 0 then
            CreateVirtualDocument_DemoAuthAll(Sender, ClientCnx, Flags)
        { Trap '/time.html' path to dynamically generate a dynamic answer. }
        else if CompareText(ClientCnx.Path, '/time.html') = 0 then
            CreateVirtualDocument_Time(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/bruno.html') = 0 then
            CreateVirtualDocument_Bruno(Sender, ClientCnx, Flags)
        { Trap '/myip.html' path to dynamically generate a dynamic answer. }
        else if CompareText(ClientCnx.Path, '/myip.html') = 0 then
            CreateVirtualDocument_MyIP(Sender, ClientCnx, Flags)
        { Trap '/HeaderBug.html' path to dynamically generate a dynamic answer. }
        else if CompareText(ClientCnx.Path, '/HeaderBug.html') = 0 then
            CreateVirtualDocument_HeaderBug(Sender, ClientCnx, Flags)
        { Trap '/redir.html' to dynamically generate a redirection answer }
        else if CompareText(ClientCnx.Path, '/redir.html') = 0 then
            CreateVirtualDocument_Redir(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/template.html') = 0 then
            CreateVirtualDocument_template(Sender, ClientCnx, Flags)
        { Trap '/delayed.html' path to send a page once a timer expires. }
        else if CompareText(ClientCnx.Path, '/delayed.html') = 0 then
        begin
            ClientCnx.FRespTimer := TTimer.Create (self) ;
            ClientCnx.FRespTimer.OnTimer := ClientCnx.TimerRespTimer ;
            ClientCnx.FRespTimer.Interval := 10000 ;     // 10 second delay
            ClientCnx.FRespTimer.Enabled := true ;
            Flags := hgWillSendMySelf;
        end;
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1HeadDocument(Sender, Client: TObject;
  var Flags: THttpGetFlag);
var
    ClientCnx : TMyHttpConnection;
    S: String;
begin
    ClientCnx := Client as TMyHttpConnection;
    S := ClientCnx.PeerAddr + ' - ' + ClientCnx.HostTag + ' ' + ClientCnx.Path;
    if ClientCnx.Params = '' then
        Display(S)
    else
        Display(S + '?' + ClientCnx.Params);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1HttpMimeContentType(Sender,
  Client: TObject; const FileName: string; var ContentType: string);
var
    ClientCnx  : TMyHttpConnection;
begin
    ClientCnx := TMyHttpConnection(Client);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] ' +
            ': Document: ' + FileName + ', Content-Type: ' + ContentType);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1OptionsDocument(Sender, Client: TObject;
  var Flags: THttpGetFlag);
var
    ClientCnx  : TMyHttpConnection;
begin
    { It's easier to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);

    { Count request and display a message }
    InterlockedIncrement(FCountRequests);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
            ': ' + ClientCnx.Version + ' OPTIONS ' + ClientCnx.Path);
    DisplayHeader(ClientCnx);
    Exit;  { let web server send general response }

    { path may be the resource for which OPTIONS should be checked }

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1PatchDocument(Sender, Client: TObject;
  var Flags: THttpGetFlag);
var
    ClientCnx  : TMyHttpConnection;
begin
    if Flags = hg401 then
    { Not authenticated (yet), we might be still in an authentication       }
    { session with ClientCnx.RequestContentLength = 0 which was valid,      }
    { i.e. NTLM uses a challenge/response method, anyway just exit.         }
        Exit;

    { It's easier to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);

    { Count request and display a message }
    InterlockedIncrement(FCountRequests);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
            ': ' + ClientCnx.Version + ' PATCH ' + ClientCnx.Path);
    DisplayHeader(ClientCnx);

    if (ClientCnx.RequestContentLength > MAX_UPLOAD_SIZE) or
       (ClientCnx.RequestContentLength <= 0) then begin
        if (ClientCnx.RequestContentLength > MAX_UPLOAD_SIZE) then
            Display('Upload size exceeded limit (' +
                    IntToStr(MAX_UPLOAD_SIZE) + ')');
        Flags := hg403;
        Exit;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server component receive a POST }
{ command from any client.                                                  }
{ We count the request, display a message and trap posted data.             }
{ To check for posted data, you may construct the following HTML document:  }
{ <HTML>                                                                    }
{   <HEAD>                                                                  }
{     <TITLE>Test Form 1</TITLE>                                            }
{   </HEAD>                                                                 }
{   <BODY>                                                                  }
{     <H2>Enter your first and last name</H2>                               }
{     <FORM METHOD="POST" ACTION="/cgi-bin/FormHandler">                    }
{       <TABLE BORDER="0" ALIGN="DEFAULT" WIDTH="100%">                     }
{         <TR>                                                              }
{           <TD>First name</TD>                                             }
{           <TD><INPUT TYPE="TEXT" NAME="FirstName"                         }
{                      MAXLENGTH="25" VALUE="YourFirstName"></TD>           }
{         </TR>                                                             }
{         <TR>                                                              }
{           <TD>Last name</TD>                                              }
{           <TD><INPUT TYPE="TEXT" NAME="LastName"                          }
{                      MAXLENGTH="25" VALUE="YourLastName"></TD>            }
{         </TR>                                                             }
{       </TABLE>                                                            }
{       <P><INPUT TYPE="SUBMIT" NAME="Submit" VALUE="Button"></P>           }
{     </FORM>                                                               }
{   </BODY>                                                                 }
{ </HTML>                                                                   }


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1PostDocument(Sender, Client: TObject;
  var Flags: THttpGetFlag);
var
    ClientCnx  : TMyHttpConnection;
begin
    if Flags = hg401 then
    { Not authenticated (yet), we might be still in an authentication       }
    { session with ClientCnx.RequestContentLength = 0 which was valid,      }
    { i.e. NTLM uses a challenge/response method, anyway just exit.         }
        Exit;

    { It's easier to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);

    { Count request and display a message }
    InterlockedIncrement(FCountRequests);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
            ': ' + ClientCnx.Version + ' POST ' + ClientCnx.Path);
    DisplayHeader(ClientCnx);

    if (ClientCnx.RequestContentLength > MAX_UPLOAD_SIZE) or
       (ClientCnx.RequestContentLength <= 0) then begin
        if (ClientCnx.RequestContentLength > MAX_UPLOAD_SIZE) then
            Display('Upload size exceeded limit (' +
                    IntToStr(MAX_UPLOAD_SIZE) + ')');
        Flags := hg403;
        Exit;
    end;

{  NOTE - HttpAppSrv overrides POST data processing for which
       a UrlHandler must be provided. }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1PutDocument(Sender, Client: TObject;
  var Flags: THttpGetFlag);
var
    ClientCnx  : TMyHttpConnection;
begin
    if Flags = hg401 then
    { Not authenticated (yet), we might be still in an authentication       }
    { session with ClientCnx.RequestContentLength = 0 which was valid,      }
    { i.e. NTLM uses a challenge/response method, anyway just exit.         }
        Exit;

    { It's easier to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);

    { Count request and display a message }
    InterlockedIncrement(FCountRequests);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
            ': ' + ClientCnx.Version + ' PUT ' + ClientCnx.Path);
    DisplayHeader(ClientCnx);

    if (ClientCnx.RequestContentLength > MAX_UPLOAD_SIZE) or
       (ClientCnx.RequestContentLength <= 0) then begin
        if (ClientCnx.RequestContentLength > MAX_UPLOAD_SIZE) then
            Display('Upload size exceeded limit (' +
                    IntToStr(MAX_UPLOAD_SIZE) + ')');
        Flags := hg403;
        Exit;
    end;

    { URL may specify a file name or just a name, worry about it later    }
 (*
    { Tell HTTP server that we will accept posted data                 }
    { OnPostedData event will be triggered when data comes in          }
    Flags := hgAcceptData;
    { We wants to receive any data type. So we turn line mode off on   }
    { client connection.                                               }
    ClientCnx.LineMode := FALSE;
    { We need a buffer to hold posted data. We allocate as much as the }
    { size of posted data plus one byte for terminating nul char.      }
    { We should check for ContentLength = 0 and handle that case...    }
    ReallocMem(ClientCnx.FPostedRawData,
               ClientCnx.RequestContentLength + 1);
    { Clear received length                                            }
    ClientCnx.FDataLen := 0;
   *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1ServerStarted(Sender: TObject);
begin
    Display('Server has started');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1ServerStopped(Sender: TObject);
begin
    SslHttpAppSrv1.WSessions.SaveToFile(FSessionFile);
    CleanupTimeStampedDir(WebAppSrvDataModule.DataDir);
    Display('Server is now stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1SslHandshakeDone(Sender: TObject;
  ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
var
    ClientCnx : TMyHttpConnection;
begin
    ClientCnx := Sender as TMyHttpConnection;
    Display(ClientCnx.PeerAddr + ' - ' + ClientCnx.HostTag + ' ' + ClientCnx.SslHandshakeRespMsg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1TraceDocument(Sender, Client: TObject;
  var Flags: THttpGetFlag);
var
    ClientCnx  : TMyHttpConnection;
begin
    { It's easier to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);

    { Count request and display a message }
    InterlockedIncrement(FCountRequests);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
            ': ' + ClientCnx.Version + ' TRACE ' + ClientCnx.Path);
    DisplayHeader(ClientCnx);

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1VirtualException(Sender: TObject;
  E: Exception; Method: THttpMethod; const Path: string);
begin
    Display('Exception creating virtual page: ' + Path + ' - ' + E.Message);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.SslHttpAppSrv1WellKnownDir(Sender,
  Client: TObject; const Path: string; var BodyStr: string);
var
    ClientCnx : TMyHttpConnection;
begin
    ClientCnx := Client as TMyHttpConnection;
    Display(ClientCnx.PeerAddr + ' - ' + ClientCnx.HostTag + '  Well-Known File Requested: ' + Path);
end;


procedure TWeblServerForm.IcsSslX509CertsCertProg(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    Display(Msg);
end;

procedure TWeblServerForm.IcsSslX509CertsChallengeDNS(Sender: TObject;
  ChallengeItem: TChallengeItem);
begin
//   update DNS server with TXT challenge information
end;

procedure TWeblServerForm.IcsSslX509CertsNewCert(Sender: TObject);    { V8.57 }
begin
    CertCheckTrigger := Trigger64Immediate;
 // force certiificate check to load new ones
    Display ('Trigger Recheck Certificates') ;
    Display('Web server ordered new SSL cerrtificate.' + IcsCRLF +
                                      IcsSslX509Certs.GetOrderResult);
end;

procedure TWeblServerForm.IcsSslX509CertsOAuthAuthUrl(Sender: TObject;
  const URL: string);
begin
 // ideally email this to user, if run as a service!!!!
   Display('Web server demo needs OAuth authenfication for new certificate, ' +
                'Browse to this URL: ' + URL +  ', From PC: ' + IcsGetCompName) ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.HttpAppSrvClientBgException(Sender: TObject; E: Exception; var CanClose : Boolean);
begin
    Display('Exception processing page - ' +
            E.ClassName + ': ' + E.Message);
    CanClose := TRUE;  // Shutdown client
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /demo.html document                      }
procedure TWeblServerForm.CreateVirtualDocument_Demo(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer Demo - Menu</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS WebAppServer Demo Menu</H2>' +
            '<A HREF="/time.html">Server time</A><BR>'  +
            '<A HREF="/template.html">Template demo</A><BR>'  +
            '<A HREF="/appindex.html">Application Web Server session demo</A><BR>'  +
            '<A HREF="/form.html">Data entry</A><BR>'   +
            '<A HREF="/uploadfile.html">Upload a file using POST</A><BR>' +
        {    '<A HREF="/upload">View uploaded files</A><BR>' +   }
            '<A HREF="/redir.html">Redirection</A><BR>' +
            '<A HREF="/myip.html">Show client IP</A><BR>' +
            '<A HREF="/delayed.html">Delayed Slow Response</A><BR>' +
            '<A HREF="/DemoBasicAuth.html">Password protected page</A> ' +
                     '(Basic method, usercode=test, password=basic)<BR>' +
            '<A HREF="/DemoDigestAuth.html">Password protected page</A> ' +
                     '(Digest method, usercode=test, password=digest)<BR>' +
            '<A HREF="/DemoNtlmAuth.html">Password protected page</A> ' +
                     '(NTLM method, usercode=(Windows) user, password=(Windows) password)<BR>' +
            '<A HREF="/DemoAuthAll.html">Password protected page</A> ' +
                     '(method is selected by the browser. Usercode/Password as above)<BR>' +
            '<A HREF="/">Default document</A><BR>'     +
            '<A HREF="http://www.overbyte.be">ICS Home page</A><BR>' +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /DemoBasicAuth.html document                      }
procedure TWeblServerForm.CreateVirtualDocument_DemoBasicAuth(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS WebServer Demo - Password Protected Page - Basic' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS WebServer Password Protected Page</H2>' +
            'This page is protected by a username and password.<BR>' +
            'The authentication is done using basic mode.<BR>' +
            '<A HREF="/Demo.html">Demo menu</A><BR>'  +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /DemoBasicAuth.html document            }
procedure TWeblServerForm.CreateVirtualDocument_DemoDigestAuth(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS WebServer Demo - Password Protected Page - Digest' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS WebServer Password Protected Page</H2>' +
            'This page is protected by a username and password.<BR>' +
            'The authentication is done using digest mode.<BR>' +
            '<A HREF="/Demo.html">Demo menu</A><BR>'  +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /DemoNtlmAuth.html document             }
procedure TWeblServerForm.CreateVirtualDocument_DemoNtlmAuth(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS WebServer Demo - Password Protected Page - NTLM' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS WebServer Password Protected Page</H2>' +
            'This page is protected by a username and password.<BR>' +
            'The authentication is done using NTLM mode.<BR>' +
            '<A HREF="/Demo.html">Demo menu</A><BR>'  +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /DemoAuthAll.html document             }
procedure TWeblServerForm.CreateVirtualDocument_DemoAuthAll(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS WebServer Demo - Password Protected Page - NTLM' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS WebServer Password Protected Page</H2>' +
            'This page is protected by a username and password.<BR>' +
            'Your browser selected the <B>' +
            HttpAuthTypeNames[ClientCnx.AuthType] +
            '</B> authentication mode.<BR>' +
            '<A HREF="/Demo.html">Demo menu</A><BR>'  +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.CreateVirtualDocument_Template(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerPage(
        Flags,
        '',
        NO_CACHE,
        'TemplateDemo.html',
        nil,
        ['TIME',    DateTimeToStr(Now),
         'PROGVER', SrvCopyRight,
         'SOURCE',  TextToHtmlText(ClientCnx.TemplateDir +
                                   'TemplateDemo.html')]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /redir.html document                     }
procedure TWeblServerForm.CreateVirtualDocument_Redir(
    Sender    : TObject;            { HTTP server component                 }
    ClientCnx : TMyHttpConnection;  { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    Location : String;
begin
    Location := ClientCnx.Params;
 //   if Location = '' then
 //       Location := Trim(RedirUrlEdit.text);

    ClientCnx.AnswerString(Flags,
        '302 Moved',                    { Tell the browser about relocation }
        '',                             { Default Content-Type: text/html   }
        'Location: ' + Location + icsCRLF,            { Specify new location }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer Demo - Redir</TITLE>' +
          '</HEAD>' + icsCRLF +
          '<BODY>' +
            'You should be redirected automatically !<BR>' + icsCRLF +
            '<A HREF="' + Location + '">Click Here</A><BR>' + icsCRLF +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /time.html document                     }
procedure TWeblServerForm.CreateVirtualDocument_Time(
    Sender    : TObject;            { HTTP server component                 }
    ClientCnx : TMyHttpConnection;  { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
begin
    ClientCnx.AnswerString(Flags,
        '',                            { Default Status '200 OK'            }
        '',                            { Default Content-Type: text/html    }
        'Pragma: no-cache' + icsCRLF +  { No client caching please           }
        'Expires: -1'      + icsCRLF,   { I said: no caching !               }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer Demo</TITLE>' +
          '</HEAD>' + icsCRLF +
          '<BODY>' +
            '<H2>Time at server side:</H2>' + icsCRLF +
            '<P>' + DateTimeToStr(Now) +'</P>' + icsCRLF +
            '<A HREF="/demo.html">Demo menu</A>' + icsCRLF +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.CreateVirtualDocument_Bruno(
    Sender    : TObject;            { HTTP server component                 }
    ClientCnx : TMyHttpConnection;  { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    Body      : String;
    Header : String;
    I      : Integer;
    Buf    : String;
    Count  : Integer;
begin
    Buf   := ClientCnx.Params;
    Count := StrToIntDef(Buf, 5000);
    if Count > 20000 then
        Count := 20000;
    Body := '<HTML>' + '<HEAD>' + '<TITLE>Test Page from Bruno :-)</TITLE>' +
            '</HEAD>' + icsCRLF +  '<BODY>';
    for I := 1 to Count do
        Body := Body + '<br>This is line number ' + IntToStr(I);
    Body := Body + '</BODY></HTML>';
    Header := 'Pragma: no-cache' + icsCRLF +  { No client caching please     }
              'Expires: -1'      + icsCRLF +
              'Set-Cookie: Usuario=a; path=/'+ icsCRLF +
              'Set-Cookie: Senha=a; path=/'+ icsCRLF;
//    ClientCnx.OnRequestDone := RequestDone;
    ClientCnx.AnswerString(Flags, '', '', Header, Body);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Produce a reply with a huge header line. Used to check client behaviour.  }
procedure TWeblServerForm.CreateVirtualDocument_HeaderBug(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',                            { Default Status '200 OK'            }
        '',                            { Default Content-Type: text/html    }
        'Pragma: no-cache' + icsCRLF +  { No client caching please           }
        'Expires: -1'      + icsCRLF,   { I said: no caching !               }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer Demo</TITLE>' +
          '</HEAD>' + icsCRLF +
          '<BODY>' +
            'Congratulations !' + icsCRLF +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWeblServerForm.CreateVirtualDocument_MyIP(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        'Pragma: no-cache' + icsCRLF +  { No client caching please           }
        'Expires: -1'      + icsCRLF,   { I said: no caching !               }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer Demo</TITLE>' +
          '</HEAD>' + icsCRLF +
          '<BODY>' +
            'Your IP is: ' +
            ClientCnx.PeerAddr + icsCRLF +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
