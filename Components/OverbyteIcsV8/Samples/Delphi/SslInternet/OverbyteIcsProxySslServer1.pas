{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Forward and Reverse SSL HTTP Proxy
Creation:     May 2017
Updated:      Sept 2018
Version:      8.50
Support:      Use the mailing list ics-ssl@elists.org
Legal issues: Copyright (C) 2003-2018 by François PIETTE
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

History:
24 May 2017 - 8.48 baseline
23 Jun 2017 - 8.49 -Start no longer gives exception if some binding fail, but
                        opens as many source listeners as possible.
                    Added host redirection and .well-known directory support
19 Sep 2017 - 8.50  Added body events
24 Sep 2018 - V8.57 INI file reads CertVerTar, DebugLevel and TarSecLevel to be
                      read as typed liternals as well as numeric values.
                    INI file reads SslCliCertMethod, SslCertAutoOrder and CertExpireDays
                    Allow SSL certificates to be ordered and installed automatically if
                      SslCertAutoOrder=True and so specified in IcsHosts, and a
                      certificate supplier account has been created (by the
                      OverbyteIcsX509CertsTst sample application).
                    Note certificate ordering currently only works with Proto=HTTP.


Insalling note:
All proxy server configuration is in OverbyteIcsProxySslServer.ini file, a default file
is included in the \ics\Samples\Delphi\SslInternet directory, and is copied into the
ICS temporary working directory, usually c:\Users\(login)\AppData\Local\ICS. This
INI file will need to be edited for IP addresses, SSL certificates, etc, although
some defaults may work.  Specifically Source8 has certificate ordering settings but
is disabled and will need new valid hosts, IP addresss and files names before it
will do anything useful.

Once this demo has been run once, any changes to the default INI file from new versions
 (ie auto certificate ordering) will need to copied manually into the working file.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsProxySslServer1;

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
  OverbyteIcsLogger,
  OverbyteIcsSslX509Utils,
  OverbyteIcsProxy,
  OverbyteIcsTicks64,       { V8.57 }
  OverbyteIcsSslX509Certs,  { V8.57 }
  OverbyteIcsSslHttpRest;   { V8.57 }

const
    ProxyCopyRight : String = ' OverbyteIcsProxySslServer (c) 2018 Francois Piette V8.57 ';
    MaxWinChars = 800000;
    WM_STARTUP = WM_USER + 711 ;

type
  TProxySslServerForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    StartButton: TButton;
    StopButton: TButton;
    IcsHttpProxy1: TIcsHttpProxy;
    Timer1: TTimer;
    RecheckCertsButton: TButton;
    IcsSslX509Certs: TSslX509Certs;
    procedure WMCMSTARTUP (var Msg : TMessage); message WM_STARTUP ;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IcsHttpProxy1DataRecvTar(Sender: TObject;
      ProxyClient: TProxyClient; DataPtr: Pointer; var DataLen: Integer);
    procedure IcsHttpProxy1DataSendTar(Sender: TObject;
      ProxyClient: TProxyClient; DataPtr: Pointer; var DataLen: Integer);
    procedure IcsHttpProxy1ProxyProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure IcsHttpProxy1SetTarget(Sender: TObject;
      ProxyClient: TProxyClient);
    procedure Timer1Timer(Sender: TObject);
    procedure RecheckCertsButtonClick(Sender: TObject);
    procedure IcsHttpProxy1HttpWellKnown(Sender: TObject;
      ProxyClient: THttpProxyClient; var Arg: string);
    procedure IcsHttpProxy1HttpReqBody(Sender: TObject;
      ProxyClient: THttpProxyClient; var Arg: string);
    procedure IcsHttpProxy1HttpRespBody(Sender: TObject;
      ProxyClient: THttpProxyClient; var Arg: string);
    procedure IcsSslX509CertsCertProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure IcsSslX509CertsChallengeDNS(Sender: TObject;
      ChallengeItem: TChallengeItem);
    procedure IcsSslX509CertsNewCert(Sender: TObject);
    procedure IcsSslX509CertsOAuthAuthUrl(Sender: TObject; const URL: string);
  private
    FIniFileName : String;
    FInitialized : Boolean;
  public
    procedure Display(Msg : String);
    procedure ReportHosts;
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
    ProxySslServerForm: TProxySslServerForm;
    WinLinesBuff: string ;
    WinDispCur: Integer;
    ProgDirectory: String;
    LogDate: Integer;
    CertCheckTrigger: int64 ;  { V8.57 }

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
{ we update DisplayMemo once a second in the timer }
procedure TProxySslServerForm.Display(Msg : String);
begin
    WinLinesBuff := WinLinesBuff + Msg + cCRLF ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.Timer1Timer(Sender: TObject);
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
            NewFlag := IcsHttpProxy1.RecheckSslCerts(S1, False, True);
            if NewFlag or (S1 <> '') then  begin
                if NewFlag then Display('Server Recheck Loaded New SSL Certificate(s)');
                Display('Proxy Recheck SSL Certificate Errors:' + cCRLF + S1);
                ReportHosts;    // report everything again
                Display('Listen Bindings:' + cCRLF + IcsHttpProxy1.ListenStates);
            end;
        except
            on E:Exception do begin
               Display('Proxy Recheck SSL Certificate Failed - ' + E.Message);
            end;
        end;
    end;

  // rotate logs at midnight, recheck SSL certificates, log configuration again
    if LogDate <> Trunc(Date) then begin
        LogDate := Trunc(Date);
        Display('Nightly Server Recheck Starting');
        CertCheckTrigger := Trigger64Immediate; { V8.57 }
        ReportHosts;    // report everything again
        Display('Listen Bindings:' + cCRLF + IcsHttpProxy1.ListenStates);
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
                    newstart := Pos(cCR, S1) ; // find start of next line
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
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.FormCreate(Sender: TObject);
begin
    FIniFileName := GetIcsIniFileName;
  // ensure SSL DLLs come from program directory, and exist, else die
  // GSSLEAY_DLL_IgnoreNew := true ; // !!! TEMP TESTING
    GSSLEAY_DLL_IgnoreOld := true ;
    ProgDirectory := ExtractFileDir (Lowercase (ParamStr(0))) ;
    GSSL_DLL_DIR := ProgDirectory + '\' ;
    GSSL_SignTest_Check := True;
    GSSL_SignTest_Certificate := True;
    OverbyteIcsWSocket.LoadSsl;
    LogDate := Trunc(Date);
    CertCheckTrigger := Trigger64Disabled;  { V8.57 }
    PostMessage (Handle, WM_STARTUP, 0, 0) ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.FormDestroy(Sender: TObject);
begin
    OverbyteIcsWSocket.UnLoadSsl;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.FormShow(Sender: TObject);
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
procedure TProxySslServerForm.IcsHttpProxy1DataRecvTar(Sender: TObject;
  ProxyClient: TProxyClient; DataPtr: Pointer; var DataLen: Integer);
begin
 //   Display('Data received from target, bytes ' + IntToStr(DataLen));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.IcsHttpProxy1DataSendTar(Sender: TObject;
  ProxyClient: TProxyClient; DataPtr: Pointer; var DataLen: Integer);
begin
//    Display('Data being sent to target, bytes ' + IntToStr(DataLen));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.IcsHttpProxy1HttpReqBody(Sender: TObject;
  ProxyClient: THttpProxyClient; var Arg: string);
begin
//    Display('Event: Request Body, Length ' + IntToStr(Length(Arg)));
 //   Display('Event: Request Body: ' + Copy (Arg, 1, 200));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.IcsHttpProxy1HttpRespBody(Sender: TObject;
  ProxyClient: THttpProxyClient; var Arg: string);
begin
//    Display('Event: Response Body, Length ' + IntToStr(Length(Arg)));
//    Display('Event: Response Body: ' + Copy (Arg, 1, 200));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.IcsHttpProxy1HttpWellKnown(Sender: TObject;
  ProxyClient: THttpProxyClient; var Arg: string);
begin
   Display('Event: Well-Known File Requested: ' + ProxyClient.RequestPath);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.IcsHttpProxy1ProxyProg(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    Display(Msg);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.IcsHttpProxy1SetTarget(Sender: TObject;
  ProxyClient: TProxyClient);
begin
    Display('Do we want to change target??? - ' + ProxyClient.TarHost);
end;

procedure TProxySslServerForm.IcsSslX509CertsCertProg(Sender: TObject;     { V8.57 }
  LogOption: TLogOption; const Msg: string);
begin
    Display(Msg);
end;

procedure TProxySslServerForm.IcsSslX509CertsChallengeDNS(Sender: TObject;
  ChallengeItem: TChallengeItem);                                        { V8.57 }
begin
//   update DNS server with TXT challenge information
end;

procedure TProxySslServerForm.IcsSslX509CertsNewCert(Sender: TObject);   { V8.57 }
begin
    CertCheckTrigger := Trigger64Immediate;
 // force certiificate check to load new ones
    Display ('Trigger Recheck Certificates') ;
    Display('Proxy server ordered new SSL cerrtificate.' + IcsCRLF +
                                      IcsSslX509Certs.GetOrderResult);
end;

procedure TProxySslServerForm.IcsSslX509CertsOAuthAuthUrl(Sender: TObject;
  const URL: string);                                                      { V8.57 }
begin
 // ideally email this to user, if run as a service!!!!
   Display('Proxy server demo needs OAuth authenfication for new certificate. ' +
                'Browse to this URL: ' + URL +  ', From PC: ' + IcsGetCompName) ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.RecheckCertsButtonClick(Sender: TObject);
begin
    CertCheckTrigger := Trigger64Immediate; 
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.FormClose(
    Sender     : TObject;
    var Action : TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    CertCheckTrigger := Trigger64Disabled;  { V8.57 }
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
procedure TProxySslServerForm.WMCMSTARTUP (var Msg : TMessage);
begin
    StartButtonClick(Self);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.ReportHosts;
var
    J: Integer;
    S: String;
begin
    Display('Total source server hosts ' + IntToStr(IcsHttpProxy1.IcsHosts.Count)) ;
    for J := 0 to IcsHttpProxy1.IcsHosts.Count - 1 do begin
        with IcsHttpProxy1.IcsHosts[J] do begin
            if CertValRes > chainOK then begin
                Display('Server SSL Certificate Errors: Server: ' + DisplayName +
                    cCRLF + 'SSL Certificate chain: ' + cCRLF + CertInfo + cCRLF + CertErrs) ;
            end;
            if CertValRes = chainFail then begin
                Display('Server SSL Certificate Errors') ;
                Exit;
            end;
            S := 'Source #' + IntToStr(J) + ' - ' + HostTag + ': ' + Descr + cCRLF +
              'Host: ' + HostNames [0] + ', Bindings: ' + BindInfo + cCRLF;
            if BindSslPort <> 0 then S := S + 'SSL Security Level ' +
                  GetEnumName(TypeInfo(TSslSrvSecurity), Ord(SslSrvSecurity)) +
                    ' - ' + CertErrs + cCRLF + CertInfo + cCRLF
            else
                S := S + 'No SSL' + cCRLF;
            if ForwardProxy then S := S + 'Forward Proxy' + cCRLF;
            if WebRedirectStat <> 0 then S := S + 'Redirection to: ' + WebRedirectURL + cCRLF;      { V9.49 }
            if WellKnownPath <> '' then S := S + 'Well-Known path root: ' + WellKnownPath + cCRLF;  { V9.49 }
            Display(S) ;
        end;
    end;
    Display('Total remote targets ' + IntToStr(IcsHttpProxy1.ProxyTargets.Count)) ;
    for J := 0 to IcsHttpProxy1.ProxyTargets.Count - 1 do begin
        with IcsHttpProxy1.ProxyTargets[J] do begin
            S := 'Target #' + IntToStr(J) + ' - ' + HostTag + ': ' + Descr + cCRLF +
                    'Remote ' + TarHost + ':' + IntToStr (TarPort) ;
            if TarSsl then S := S + ' with SSL' ;
            if SrcPath <> '' then S := S + ', Path ' + SrcPath ;
            S := S + cCRLF ;
           Display(S) ;
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.StartButtonClick(Sender: TObject);
var
    IniFile : TIcsIniFile;
    List: TStringList;
    S: String;
begin
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
        Display(OverbyteIcsProxy.CopyRight + cCRLF +
           'SSL Version: ' + OpenSslVersion + ', Dir: ' + GLIBEAY_DLL_FileName + cCRLF);

      // main proxy settings from INI file, built in CA if no file found
        IcsLoadTIcsHttpProxyFromIni(IniFile, IcsHttpProxy1, 'Proxy');
        if (IcsHttpProxy1.RootCA = '') or (NOT FileExists(IcsHttpProxy1.RootCA)) then
                                         IcsHttpProxy1.RootCA := sslRootCACertsBundle;

      // read the source server hosts from INI file and check SSL files exist
        IcsLoadIcsHostsFromIni(IniFile, IcsHttpProxy1.IcsHosts, 'Source');
        if IcsHttpProxy1.IcsHosts.Count <= 0 then
         begin
            Display('Can Not Start Server - No Source Server Hosts Configured') ;
            exit ;
        end;

    // read the remote proxy targets
        IcsLoadProxyTargetsFromIni(IniFile, IcsHttpProxy1.ProxyTargets, 'Target');
        if IcsHttpProxy1.ProxyTargets.Count <= 0 then  begin
            Display('Can Not Start Server - No Proxy Targets Configured');
            exit ;
        end;

    // validate hosts and keep site certificiate information
        try
            S := IcsHttpProxy1.ValidateHosts(False, True); // don't stop on first error, no exceptions
            if S <> '' then begin
                Display('Proxy Validation Errors:' + cCRLF + S);
            end;
            ReportHosts;
            Display('Required Listen Bindings:' + cCRLF + IcsHttpProxy1.ListenStates);
        except
            on E:Exception do begin
                Display('Host Validation Failed, Server Stopped - ' + E.Message);
                Exit;
            end;
        end;
        IcsHttpProxy1.Start;
        if NOT IcsHttpProxy1.ListenAllOK then
            Display('Failed to Start, Listen Bindings:' +
                                      cCRLF + IcsHttpProxy1.ListenStates);
        CertCheckTrigger := IcsGetTrgSecs64 (15) ;  { V8.57 first check is early to order new certificates }
        StartButton.Enabled := false;
        StopButton.Enabled := true;
    except
        on E:Exception do begin
           Display('Failed to start proxy - ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.StopButtonClick(Sender: TObject);
begin
    if NOT StopButton.Enabled then Exit;
    CertCheckTrigger := Trigger64Disabled;  { V8.57 }
    StartButton.Enabled := true;
    StopButton.Enabled := false;
    IcsHttpProxy1.Stop;
    Display('Server stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
