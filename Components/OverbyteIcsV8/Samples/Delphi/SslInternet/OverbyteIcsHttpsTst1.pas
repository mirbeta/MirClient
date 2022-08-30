{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Feb 15, 2003
Description:  A simple  HTTPS SSL Web Client Demo client.
              Make use of OpenSSL (http://www.openssl.org).
              Make use of freeware TSslHttpCli and TSslWSocket components
              from ICS (Internet Component Suite).
Version:      8.56
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
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
Oct 22, 2005  V1.03 Arno Garrels implemented client-side SSL session caching,
              alternate verify methode, and support for dynamicaly provided
              client certificates.
Nov 08, 2005  V1.04 Arno Garrels adjusted a few type casts in some event
              handlers. Put in OpenSSL version info.
Dec 20, 2005  V1.05 Angus Robertson added new LogOptions and GZIP decompression
                and display more log lines
Jul 18, 2008  V1.06 A. Garrels fixed an AV in SslHttpCli1SslCliCertRequest
Dec 9, 2014   V8.00 Angus added SslHandshakeRespMsg for better error handling
Mar 16 2015   V8.01 Angus added DH File (mainly for servers)
              Added SSL Version and Cipher edits to make testing easier
              Reset SSL when changing parameters to force new negotiation
Oct 26 2015   V8.02 Angus simplified certificate display
May 24 2016   V8.27 Angus testing OpenSSL 1.1.0, added SslThrdLock
              Specify minimum and maximum SSL version supported
              List SSL ciphers available and supported by protocols
              Old SSL check box to ignore OpenSSL 1.1.0 and use older versions
              Certificates and CA bundle may now be added as lines of text to
                context instead of being read from files
              If no CA file or path or lines specified, use default CA bundle
Aug 27, 2016  V8.32 set SslCipherEdit if empty
Nov 04, 2016  V8.37 report more error information
              Only report client ciphers once
Nov 23, 2016  V3.39 no longer need PostConnectionCheck or TX509Ex
              Added List Cert Store button to list common names of any
                certificates loaded from CA File or CA Path, so you know
                exactly what was found
Feb 26, 2017  V8.41 added SslSecLevel to set minimum effective bits for
                certificate key length, 128 bits and higher won't usually work!
              Simplified listing certificate chain in handshake
Sep 17, 2017  V8.50 HTML text content now converted to Delphi string with correct
                 code page according to charset in header or page, or BOM
Dec 11, 2017  V8.51 added Debug Dump tick box to log SSL dump diagnostics
              Added proxy and socks authentication, login and password
              Report SOCKS proxy events
              Try and enable FIPS mode if supported by OpenSSL.
Feb 16, 2018  V8.52 root certificates show SubjectOUName
Jul 6, 2018   V8.56 testing SSL application layer protocol negotiation, used
                 for HTTP/2 (not supported yet), set the protoools supported
                 in SslContext.SslAlpnProtocols and check what the servers
                 choose after SslHandshake.



// pending add persistent cookie support, and gzip content encoding

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpsTst1;

{$IFNDEF USE_SSL}
  {$MESSAGE FATAL 'Define conditional define "USE_SSL" in the project options'};
{$ENDIF}
{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$DEFINE USE_MODEZ}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, OverbyteIcsHttpProt, OverbyteIcsWSocket,
  OverbyteIcsLIBEAY, OverbyteIcsSsLeay, OverbyteIcsSslSessionCache,
  OverbyteIcsLogger, OverbyteIcsSslX509Utils, OverbyteIcsSslThrdLock, TypInfo,
{$IF CompilerVersion > 23}
  System.UITypes,
{$IFEND}
{$IFDEF USE_MODEZ}              { V2.102 }
  OverbyteIcsHttpCCodZLib,
{$ENDIF}
  OverbyteIcsWndControl,
  OverbyteIcsCharsetUtils;       { V8.50 }


const
     HttpsTstVersion     = 856;
     HttpsTstDate        = 'July 6, 2018';
     HttpsTstName        = 'HttpsTst';
     CopyRight : String  = ' HttpsTst (c) 2005-2018 Francois Piette V8.56 ';
     WM_SSL_NOT_TRUSTED  = WM_USER + 1;

type
  THttpsTstForm = class(TForm)
    DisplayMemo: TMemo;
    SslHttpCli1: TSslHttpCli;
    DocumentMemo: TMemo;
    Splitter1: TSplitter;
    ToolsPanel: TPanel;
    Label1: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    Label10: TLabel;
    Label6: TLabel;
    Label4: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label5: TLabel;
    Label11: TLabel;
    Label15: TLabel;
    SocksServerEdit: TEdit;
    SocksPortEdit: TEdit;
    DocEdit: TEdit;
    CertFileEdit: TEdit;
    CAFileEdit: TEdit;
    VerifyPeerCheckBox: TCheckBox;
    CAPathEdit: TEdit;
    PrivKeyFileEdit: TEdit;
    PassPhraseEdit: TEdit;
    AcceptableHostsEdit: TEdit;
    SocksLevelComboBox: TComboBox;
    GetButton: TButton;
    ClearButton: TButton;
    CloseButton: TButton;
    ProxyHostEdit: TEdit;
    ProxyPortEdit: TEdit;
    HttpVersionComboBox: TComboBox;
    SslContext1: TSslContext;
    SessCacheCheckBox: TCheckBox;
    Label17: TLabel;
    ButtonOSSLVersion: TButton;
    IcsLogger1: TIcsLogger;
    DebugEventCheckBox: TCheckBox;
    DebugOutputCheckBox: TCheckBox;
    DebugFileCheckBox: TCheckBox;
    Label18: TLabel;
    DateTimeEdit: TEdit;
    HeadButton: TButton;
    AbortButton: TButton;
    SslAvlSessionCache1: TSslAvlSessionCache;
    Label19: TLabel;
    DhParamFileEdit: TEdit;
    SslMaxVersion: TComboBox;
    Label20: TLabel;
    Label21: TLabel;
    SslCipherEdit: TEdit;
    UrlEdit: TComboBox;
    ResetButton: TButton;
    SslStaticLock1: TSslStaticLock;
    Label14: TLabel;
    SslMinVersion: TComboBox;
    OldSslCheckBox: TCheckBox;
    StoreButton: TButton;
    SslSecLevel: TComboBox;
    Label22: TLabel;
    DebugDumpCheckBox: TCheckBox;
    Label16: TLabel;
    Label23: TLabel;
    ProxyLoginEdit: TEdit;
    ProxyPwEdit: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure GetButtonClick(Sender: TObject);
    procedure SslHttpCli1SslVerifyPeer(Sender: TObject; var Ok: Integer;
      Cert : TX509Base);
    procedure FormDestroy(Sender: TObject);
    procedure SslHttpCli1RequestDone(Sender: TObject; RqType: THttpRequest;
      ErrCode: Word);
    procedure ClearButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure SslHttpCli1Command(Sender: TObject; var S: String);
    procedure SslHttpCli1Cookie(Sender: TObject; const Data: String;
      var Accept: Boolean);
    procedure SslHttpCli1DocBegin(Sender: TObject);
    procedure SslHttpCli1DocEnd(Sender: TObject);
    procedure SslHttpCli1LocationChange(Sender: TObject);
    procedure SslHttpCli1HeaderData(Sender: TObject);
    procedure SslHttpCli1SslCliNewSession(Sender: TObject;
      SslSession: Pointer; WasReused: Boolean; var IncRefCount: Boolean);
    procedure SslHttpCli1SslCliGetSession(Sender: TObject;
      var SslSession: Pointer; var FreeSession: Boolean);
    procedure SslHttpCli1SslHandshakeDone(Sender: TObject; ErrCode: Word;
      PeerCert: TX509Base; var Disconnect: Boolean);
    procedure SslHttpCli1SslCliCertRequest(Sender: TObject;
      var Cert: TX509Base);
    procedure ButtonOSSLVersionClick(Sender: TObject);
    procedure IcsLogger1IcsLogEvent(Sender: TObject;
      DebugOption: TLogOption; const Msg: String);
    procedure HeadButtonClick(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
    procedure SslHttpCli1DocData(Sender: TObject; Buffer: Pointer;
      Len: Integer);
    procedure ResetSsl(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure OldSslCheckBoxClick(Sender: TObject);
    procedure StoreButtonClick(Sender: TObject);
    procedure SslHttpCli1SocksConnected(Sender: TObject; ErrCode: Word);
    procedure SslHttpCli1SocksError(Sender: TObject; Error: Integer;
      Msg: string);
    procedure SslHttpCli1SocksAuthState(Sender: TObject;
      AuthState: TSocksAuthState);
    procedure SslHttpCli1SocketError(Sender: TObject);

  private
    FIniFileName               : String;
    FInitialized               : Boolean;
    FTrustedList               : TStringList;
    //FNotTrusted              : String;
    FClientCerts               : TX509List;
    FDocFileName               : String;
    FByteCount                 : Integer;
    FStartTime                 : Integer;
    FDispCiphDone              : Boolean;  { V8.37 }
    procedure WMSslNotTrusted(var Msg: TMessage); message WM_SSL_NOT_TRUSTED;
    procedure BackgroundException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure PrepareConnection;
    procedure SetButtonState(State: Boolean);
    procedure TransfertStats;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  HttpsTstForm: THttpsTstForm;

implementation

{$R *.DFM}

uses OverbyteIcsCliCertDlg;

const
    SectionWindow      = 'HttpTstMainWindow';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyUrl             = 'Url';
    KeySocksServer     = 'SocksServer';
    KeySocksPort       = 'SocksPort';
    KeySocksLevel      = 'SocksLevelIndex';
    KeyProxyHost       = 'ProxyHost';
    KeyProxyPort       = 'ProxyPort';
    KeyDateTime        = 'DateTime';
    KeyDoc             = 'Doc';
    KeyCertFile        = 'CertFile';
    KeyPassPhrase      = 'PassPhrase';
    KeyPrivKeyFile     = 'PrivKeyFile';
    KeyCAFile          = 'CAFile';
    KeyCAPath          = 'CAPath';
    KeyLineMode        = 'LineMode';
    KeyVerifyPeer      = 'VerifyPeer';
    KeyAcceptableHosts = 'AcceptableHosts';
    KeyHttpVer         = 'HttpVer';
    KeySessCache       = 'SessCache';
    KeyDebugEvent      = 'DebugEvent';
    KeyDebugOutput     = 'DebugOutput';
    KeyDebugFile       = 'DebugFile';
    KeyDHFile          = 'DHFile';
    KeySslMinVersion   = 'SslMinVersion';
    KeySslMaxVersion   = 'SslMaxVersion';
    KeySslCipher       = 'SslCipher';
    KeyOldSsl          = 'OldSsl';
    KeySslSecLevel     = 'SslSecLevel';
    KeyDebugDump       = 'DebugDump';
    KeyProxyLogin      = 'ProxyLogin';
    KeyProxyPw         = 'ProxyPw';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF DEBUG_OUTPUT}
procedure BigConsole(nCols, nLines : Integer);
var
    sc : TCoord;
    N  : DWord;
begin
    if not IsConsole then
        Exit;
    sc.x := nCols;
    sc.y := nLines;
    SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), sc);
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
                            BACKGROUND_BLUE or BACKGROUND_GREEN or
                            BACKGROUND_RED or BACKGROUND_INTENSITY);
    sc.x := 0;
    sc.y := 0;
    FillConsoleOutputAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
                               BACKGROUND_BLUE or BACKGROUND_GREEN or
                               BACKGROUND_RED or BACKGROUND_INTENSITY,
                               nCols * nLines, sc, N);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.FormCreate(Sender: TObject);
begin
{$IFDEF DEBUG_OUTPUT}
    BigConsole(80, 100);
{$ENDIF}
{$IF CompilerVersion > 17}
    // BDS2006 has built-in memory leak detection and display
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$IFEND}
    FIniFileName := GetIcsIniFileName;
    FTrustedList := TStringList.Create;
    FClientCerts := nil;
    SslHttpCli1.CtrlSocket.OnBgException := BackgroundException;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.FormDestroy(Sender: TObject);
begin
    if Assigned(FTrustedList) then
        FreeAndNil(FTrustedList);
    FreeAndNil(FClientCerts);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
    SL : TSslSecLevel;
    mode: integer;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

    { V8.41 set SSL security level items from TSslSecLevel }
        for SL := Low (TSslSecLevel) to High (TSslSecLevel) do
            SslSecLevel.Items.Add (GetEnumName(TypeInfo(TSslSecLevel), Ord(SL)));

        IniFile      := TIcsIniFile.Create(FIniFileName);
        try
            Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
            Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
            Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
            UrlEdit.Text := IniFile.ReadString(SectionData, KeyUrl,
                                              'https://localhost');
            SocksServerEdit.Text := IniFile.ReadString(SectionData, KeySocksServer,
                                                   '');
            SocksPortEdit.Text   := IniFile.ReadString(SectionData, KeySocksPort,
                                                       '1080');
            ProxyHostEdit.Text   := IniFile.ReadString(SectionData, KeyProxyHost,
                                                       '');
            ProxyPortEdit.Text   := IniFile.ReadString(SectionData, KeyProxyPort,
                                                      '8080');
            DateTimeEdit.Text    := IniFile.ReadString(SectionData, KeyDateTime,
                                                       '');
            DocEdit.Text         := IniFile.ReadString(SectionData, KeyDoc,
                                                      '/index.html');
            CertFileEdit.Text    := IniFile.ReadString(SectionData, KeyCertFile, '');
            PrivKeyFileEdit.Text := IniFile.ReadString(SectionData, KeyPrivKeyFile, '');
            PassPhraseEdit.Text  := IniFile.ReadString(SectionData, KeyPassPhrase,
                                                      'password');
            CAFileEdit.Text      := IniFile.ReadString(SectionData, KeyCAFile, '');
            CAPathEdit.Text      := IniFile.ReadString(SectionData, KeyCAPath, '');
            DHParamFileEdit.Text := IniFile.ReadString(SectionData, KeyDHFile, ''); { V8.01 }
            SslMinVersion.ItemIndex := IniFile.ReadInteger(SectionData,          { V8.03 }
                                                            KeySslMinVersion,
                                                            0);
            SslMaxVersion.ItemIndex := IniFile.ReadInteger(SectionData,          { V8.03 }
                                                            KeySslMaxVersion,
                                                            5);
            SslCipherEdit.Text   := IniFile.ReadString(SectionData, KeySslCipher, { V8.01 }
                                                       sslCiphersNormal);

            SocksLevelComboBox.ItemIndex := IniFile.ReadInteger(SectionData,
                                                                KeySocksLevel,
                                                                0);
            AcceptableHostsEdit.Text := IniFile.ReadString(SectionData,
                                                           KeyAcceptableHosts,
                                                           'www.overbyte.be;' +
                                                           'www.borland.com');
            VerifyPeerCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData,
                                                                  KeyVerifyPeer,
                                                                  0));
            HttpVersionComboBox.ItemIndex := IniFile.ReadInteger(SectionData,
                                                                 KeyHttpVer, 0);
            SessCacheCheckBox.Checked     := IniFile.ReadBool(SectionData,
                                                              KeySessCache,
                                                              False);
            DebugEventCheckBox.Checked    := IniFile.ReadBool(SectionData,
                                                              KeyDebugEvent,
                                                              False);
            DebugOutputCheckBox.Checked   := IniFile.ReadBool(SectionData,
                                                              KeyDebugOutput,
                                                              False);
            DebugFileCheckBox.Checked     := IniFile.ReadBool(SectionData,
                                                              KeyDebugFile,
                                                              False);
            OldSslCheckBox.Checked        := IniFile.ReadBool(SectionData,
                                                              KeyOldSsl,
                                                              False);
            SslSecLevel.ItemIndex         := IniFile.ReadInteger(SectionData,
                                                              KeySslSecLevel, 1);  { V8.41 }
            DebugDumpCheckBox.Checked     :=  IniFile.ReadBool(SectionData,
                                                              KeyDebugDump,
                                                              False);              { V8.51 }
            ProxyLoginEdit.Text           := IniFile.ReadString(SectionData,
                                                              KeyProxyLogin,
                                                              '');                 { V8.51 }
            ProxyPwEdit.Text              := IniFile.ReadString(SectionData,
                                                              KeyProxyPw,
                                                              '');                 { V8.51 }
        finally
            IniFile.Free;
        end;
        if SslCipherEdit.Text = '' then SslCipherEdit.Text := sslCiphersNormal;
        DisplayMemo.Clear;

    { V8.03 load OpenSSL, then display OpenSSL DLL name and version  }
        GSSLEAY_DLL_IgnoreNew := OldSslCheckBox.Checked;  { V8.03 ignore OpenSSL 1.1.0 and later }
        SslStaticLock1.Enabled := true ;
        FDispCiphDone := false; { V8.37 }
        if NOT FileExists (GLIBEAY_DLL_FileName) then
            DisplayMemo.Lines.Add('SSL/TLS DLL not found: ' + GLIBEAY_DLL_FileName)
        else
            DisplayMemo.Lines.Add('SSL/TLS DLL: ' + GLIBEAY_DLL_FileName +
                                                ', Version: ' + OpenSslVersion);

    { V8.51 see if using FIPS OpenSSL DLLs, try and set FIPS mode }
        if Pos ('fips', OpenSslVersion) > 0 then begin
            mode := f_fips_mode_set(1);
            if mode <> 0 then
                DisplayMemo.Lines.Add('OpenSSL FIPS 140-2 self test successful')
             else
                DisplayMemo.Lines.Add('OpenSSL FIPS 140-2 self test failed - ' +
                                                String(LastOpenSslErrMsg(False))) ;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData,    KeyUrl,         UrlEdit.Text);
    IniFile.WriteString(SectionData,    KeySocksServer, SocksServerEdit.Text);
    IniFile.WriteString(SectionData,    KeySocksPort,   SocksPortEdit.Text);
    IniFile.WriteString(SectionData,    KeyProxyHost,   ProxyHostEdit.Text);
    IniFile.WriteString(SectionData,    KeyProxyPort,   ProxyPortEdit.Text);
    IniFile.WriteString(SectionData,    KeyDateTime,    DateTimeEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyVerifyPeer,  Ord(VerifyPeerCheckBox.Checked));
    IniFile.WriteString(SectionData,    KeyDoc,         DocEdit.Text);
    IniFile.WriteString(SectionData,    KeyCertFile,    CertFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPrivKeyFile, PrivKeyFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPassPhrase,  PassPhraseEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAFile,      CAFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAPath,      CAPathEdit.Text);
    IniFile.WriteString(SectionData,    KeyDHFile,      DhParamFileEdit.Text);        { V8.01 }
    IniFile.WriteInteger(SectionData,   KeySslMinVersion,   SslMinVersion.ItemIndex);    { V8.03 }
    IniFile.WriteInteger(SectionData,   KeySslMaxVersion,   SslMaxVersion.ItemIndex);    { V8.03 }
    IniFile.WriteString(SectionData,    KeySslCipher,   SslCipherEdit.Text);          { V8.01 }
    IniFile.WriteString(SectionData,    KeyAcceptableHosts, AcceptableHostsEdit.Text);
    IniFile.WriteInteger(SectionData,   KeySocksLevel,  SocksLevelComboBox.ItemIndex);
    IniFile.WriteInteger(SectionData,   KeyHttpVer,     HttpVersionComboBox.ItemIndex);
    IniFile.WriteBool(SectionData,      KeySessCache,   SessCacheCheckBox.Checked);
    IniFile.WriteBool(SectionData,      KeyDebugEvent,  DebugEventCheckBox.Checked);
    IniFile.WriteBool(SectionData,      KeyDebugOutput, DebugOutputCheckBox.Checked);
    IniFile.WriteBool(SectionData,      KeyDebugFile,   DebugFileCheckBox.Checked);
    IniFile.WriteBool(SectionData,      KeyOldSsl,      OldSslCheckBox.Checked);     { V8.03 }
    IniFile.WriteInteger(SectionData,   KeySslSecLevel, SslSecLevel.ItemIndex);      { V8.41 }
    IniFile.WriteBool(SectionData,      KeyDebugDump,   DebugDumpCheckBox.Checked);  { V8.51 }
    IniFile.WriteString(SectionData,    KeyProxyLogin,  ProxyLoginEdit.Text);        { V8.51 }
    IniFile.WriteString(SectionData,    KeyProxyPw,     ProxyPwEdit.Text);           { V8.51 }
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.Display(Msg : String);
var
    I, J : Integer;
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 5000 then begin
            while DisplayMemo.Lines.Count > 5000 do
                DisplayMemo.Lines.Delete(0);
        end;
        // Display Msg, breaking it into separate lines
        // Line end is either LF alone or CR/LF pair
        I := 1;
        while I <= Length(Msg) do begin
            J := I;
            while (I <= Length(Msg)) and (Msg[I] <> #10) do
                Inc(I);
            if (I > 1) and (I <= Length(Msg)) and (Msg[I] = #10) and (Msg[I-1] = #13) then
                DisplayMemo.Lines.Add(Copy(Msg, J, I -J - 1))
            else
                DisplayMemo.Lines.Add(Copy(Msg, J, I - J));
            Inc(I);
        end;
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.PrepareConnection;
const
    SocksLevelValues : array [0..2] of String = ('5', '4A', '4');
var
   List: string;
begin
    IcsLogger1.LogOptions := [];
    if DebugEventCheckBox.Checked then
        IcsLogger1.LogOptions := IcsLogger1.LogOptions + [loDestEvent];
    if DebugOutputCheckBox.Checked then
        IcsLogger1.LogOptions := IcsLogger1.LogOptions + [loDestOutDebug];
    if DebugFileCheckBox.Checked then begin
        IcsLogger1.LogFileName   := 'Debug_Out_HttpsTst.txt';
        IcsLogger1.LogFileOption := lfoOverwrite;
        IcsLogger1.LogOptions    := IcsLogger1.LogOptions + [loDestFile];
    end;
    if IcsLogger1.LogOptions <> [] then begin
        IcsLogger1.LogOptions := IcsLogger1.LogOptions + LogAllOptInfo + [loAddStamp];
        if DebugDumpCheckBox.Checked then
            IcsLogger1.LogOptions := IcsLogger1.LogOptions + LogAllOptDump ; { V8.51 SSL devel dump }
    end;

    SslHttpCli1.SocksAuthentication := socksNoAuthentication;    { V8.51 }
    SslHttpCli1.ProxyAuth     := httpAuthNone;            { V8.51 }

    if SocksServerEdit.Text > '' then begin
        SslHttpCli1.SocksServer := Trim(SocksServerEdit.Text);
        SslHttpCli1.SocksPort   := Trim(SocksPortEdit.Text);
        SslHttpCli1.SocksLevel  := SocksLevelValues[SocksLevelComboBox.ItemIndex];
        if ProxyLoginEdit.Text <> '' then begin
            SslHttpCli1.SocksAuthentication := socksAuthenticateUsercode;    { V8.51 }
            SslHttpCli1.SocksUsercode := Trim(ProxyLoginEdit.Text);        { V8.51 }
            SslHttpCli1.SocksPassword := Trim(ProxyPwEdit.Text);           { V8.51 }
        end;
    end
    else begin
        SslHttpCli1.SocksServer := '';
        SslHttpCli1.SocksPort   := '';
        SslHttpCli1.SocksLevel  := '5';
    end;

    if ProxyHostEdit.Text > '' then begin
        SslHttpCli1.Proxy         := Trim(ProxyHostEdit.Text);
        SslHttpCli1.ProxyPort     := Trim(ProxyPortEdit.Text);
        if ProxyLoginEdit.Text <> '' then begin
            SslHttpCli1.ProxyAuth     := httpAuthBasic;              { V8.51 }
            SslHttpCli1.ProxyUsername := Trim(ProxyLoginEdit.Text);        { V8.51 }
            SslHttpCli1.ProxyPassword := Trim(ProxyPwEdit.Text);           { V8.51 }
        end;
    end
    else begin
        SslHttpCli1.Proxy         := '';   { V8.51 }
        SslHttpCli1.ProxyPort     := '';   { V8.51 }
    end;
    SslHttpCli1.URL            := Trim(UrlEdit.Text);
    SslHttpCli1.AcceptLanguage := 'en, fr';
    SslHttpCli1.Connection     := 'Keep-Alive';
    SslHttpCli1.RequestVer     := '1.' + IntToStr(HttpVersionComboBox.ItemIndex);

    if DateTimeEdit.Text <> '' then
        SslHttpCli1.ModifiedSince := StrToDateTime(DateTimeEdit.Text)
    else
        SslHttpCli1.ModifiedSince := 0;

    //SslHttpCli1.SetAcceptableHostsList(AcceptableHostsEdit.Text);

    { note SSL cert and priv key are only needed if the remote server requires
      a client SSL certificate to be sent for maximum security, very rare!! }
    SslContext1.SslCertFile         := Trim(CertFileEdit.Text);
    SslContext1.SslPassPhrase       := Trim(PassPhraseEdit.Text);
    SslContext1.SslPrivKeyFile      := Trim(PrivKeyFileEdit.Text);
    SslContext1.SslCAFile           := Trim(CAFileEdit.Text);
    SslContext1.SslCAPath           := Trim(CAPathEdit.Text);
   { V8.03 no CA file or path or lines, use defaults Root CA Certs Bundle }
    if (SslContext1.SslCAFile = '') and (SslContext1.SslCAPath = '') and
        (SslContext1.SslCALines.Count = 0) then
            SslContext1.SslCALines.Text := sslRootCACertsBundle;
    SslContext1.SslVerifyPeer       := VerifyPeerCheckBox.Checked;
  { V8.03 SslVersionMethod is ignored by OpenSSL 1.1.0 and later which uses SslMinVersion and SslMaxVersion instead }
    SslContext1.SslMinVersion       := TSslVerMethod (SslMinVersion.ItemIndex);  { V8.03}
    SslContext1.SslMaxVersion       := TSslVerMethod (SslMaxVersion.ItemIndex);  { V8.03}
    SslContext1.SslCipherList       := Trim(SslCipherEdit.Text);                       { V8.01 }
    SslContext1.SslDHParamFile      := Trim(DhParamFileEdit.Text);                     { V8.01 }
    SslContext1.SslSecLevel         := TSslSecLevel (SslSecLevel.ItemIndex);     { V8.41 }

    try
        SslContext1.InitContext;  { V8.01 get any error now before making request }
    except
        on E:Exception do begin
            Display('Failed to initialize SSL Context: ' + E.Message);
            Exit;
        end;
    end;

   { list SSL ciphers }
    if NOT FDispCiphDone then begin { V8.37 only once  }
        List := SslContext1.SslGetAllCiphers;
        List := StringReplace(List, #13#10, ', ', [rfReplaceAll]);
        Display('SSL Ciphers Available: ' + #13#10 + List + #13#10);

        List := SslHttpCli1.CtrlSocket.SslGetSupportedCiphers (True, False);
        List := StringReplace(List, #13#10, ', ', [rfReplaceAll]);
        Display('SSL Ciphers Supported by Protocol: ' + #13#10 + List + #13#10);
        FDispCiphDone := true;
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.GetButtonClick(Sender: TObject);
begin
    try
        PrepareConnection;
        if NOT SslContext1.IsCtxInitialized then Exit;  { V8.01 }
        Display('Connecting to: ' + SslHttpCli1.URL);
        SetButtonState(FALSE);
        DocumentMemo.Clear;
        SslHttpCli1.GetAsync;      // async method
    except
        on E:Exception do begin
            Display('Connect error. ' + E.Classname + ': ' + E.Message);
            Exit;
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.HeadButtonClick(Sender: TObject);
var
    I       : Integer;
begin
    DisplayMemo.Clear;
    DocumentMemo.Clear;
    SetButtonState(FALSE);

    try
        PrepareConnection;
        if NOT SslContext1.IsCtxInitialized then Exit;  { V8.01 }
        Display('Connecting to: ' + SslHttpCli1.URL);
        SslHttpCli1.RcvdStream       := nil;

        try
            SslHttpCli1.Head;   // sync method
        except
            Display('HEAD Failed !');
            Display('StatusCode   = ' + IntToStr(SslHttpCli1.StatusCode));
            Display('ReasonPhrase = ' + SslHttpCli1.ReasonPhrase);
            Exit;
        end;

        Display('StatusCode = ' + IntToStr(SslHttpCli1.StatusCode));

        for I := 0 to SslHttpCli1.RcvdHeader.Count - 1 do
            Display('hdr>' + SslHttpCli1.RcvdHeader.Strings[I]);
    finally
        SetButtonState(TRUE);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.AbortButtonClick(Sender: TObject);
begin
    SslHttpCli1.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.WMSslNotTrusted(var Msg: TMessage);
begin
    (*
    if Application.MessageBox(
           PChar('Do you want to trust this certificate ?' + #10 +
                 FNotTrusted),
           HttpsTstName, MB_YESNO + MB_DEFBUTTON2) = ID_YES then begin
        FTrustedList.Add(FNotTrusted);
        // Now that the user accepted the certificate, we can restart
        // the request (It was interrupted because of untrusted certificate)
     *)
        SslHttpCli1.GetAsync;
    //end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.ClearButtonClick(Sender: TObject);
begin
    DocumentMemo.Clear;
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.CloseButtonClick(Sender: TObject);
begin
    SslHttpCli1.CloseAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1Command(Sender: TObject; var S: String);
begin
    Display('cmd> ' + s);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1Cookie(
    Sender     : TObject;
    const Data : String;
    var Accept : Boolean);
begin
    Display('Cookie: "' + Data + '"');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1DocBegin(Sender: TObject);
var
    HttpCli : TSslHttpCli;
begin
    FByteCount := 0;
    FStartTime := GetTickCount;
    HttpCli := Sender as TSslHttpCli;
    Display(HttpCli.ContentType + ' => ' + HttpCli.DocName);
    Display('Document = ' + HttpCli.DocName);

    FDocFileName := HttpCli.DocName;

    if HttpCli.ContentType = 'image/gif' then
        ReplaceExt(FDocFileName, 'gif')
    else if HttpCli.ContentType = 'image/jpeg' then
        ReplaceExt(FDocFileName, 'jpg')
    else if HttpCli.ContentType = 'image/bmp' then
        ReplaceExt(FDocFileName, 'bmp');

    if FDocFileName = '' then
        FDocFileName := 'HttpTst.htm';
    try
        HttpCli.RcvdStream := TFileStream.Create(FDocFileName, fmCreate);
    except
        on E:Exception do begin
            Display('Error opening file: ' + E.Message);
            FDocFileName := 'HttpTst.htm';
            Display('Using default file name: ' + FDocFileName);
            HttpCli.RcvdStream := TFileStream.Create(FDocFileName, fmCreate);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1DocData(Sender: TObject; Buffer: Pointer;
  Len: Integer);
begin
    Inc(FByteCount, Len);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.TransfertStats;
var
    Buffer   : String;
    BytesSec : Int64 ;
    Duration : Int64 ;  { V2.100 allow wrap at 49 days, don't show 0 secs or silly bps }
    FStopTime : Integer;
begin
    FStopTime := LongInt(GetTickCount);
    Buffer    := IntToSTr(FByteCount) + ' bytes document received/sent in ';
    if DWORD (FStopTime) >= DWORD (FStartTime) then   { V2.102 fix zero duration downloads }
        Duration := DWORD (FStopTime) - DWORD (FStartTime)
    else
        Duration := ($FFFFFFFF - DWORD (FStartTime)) + DWORD (FStopTime);
    if Duration < 5000 then
        Buffer := Buffer + IntToStr(Duration) + ' milliseconds'
    else begin
        Buffer := Buffer + IntToStr(Duration div 1000) + ' seconds';
    if FStopTime <> FStartTime then begin
        if FByteCount > 32767 then
                BytesSec := 1000 * (FByteCount div Duration)
        else
                BytesSec := (1000 * FByteCount) div Duration;
        Buffer := Buffer + ' (' + IntToStr(BytesSec) + ' Bytes/sec)';
    end;
    end;
    Display('! ' + Buffer);
end;


procedure THttpsTstForm.SslHttpCli1DocEnd(Sender: TObject);
var
    HttpCli : TSslHttpCli;
begin
    TransfertStats;
    HttpCli := Sender as TSslHttpCli;
    if Assigned(HttpCli.RcvdStream) then begin
        HttpCli.RcvdStream.Free;
        HttpCli.RcvdStream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1LocationChange(Sender: TObject);
var
    SslHttpCli : TSslHttpCli;
    I          : Integer;
begin
    SslHttpCli := Sender as TSslHttpCli;
    for I := 0 to SslHttpCli.RcvdHeader.Count - 1 do
        Display('hdr>' + SslHttpCli.RcvdHeader.Strings[I]);
    Display('Location changed to "' + SslHttpCli.Location + '"');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1RequestDone(
    Sender  : TObject;
    RqType  : THttpRequest;
    ErrCode : Word);
var
    DataIn  : TStream;
    I       : Integer;
    HtmlCodepage: Integer;
    BOMSize : Integer;
    DataStr : String;
begin
    SetButtonState(TRUE);
    if ErrCode <> 0 then begin
        Display('Request done, error #' + IntToStr(ErrCode) +
              '. Status = ' + IntToStr(SslHttpCli1.StatusCode) +
                               ' - ' + SslHttpCli1.ReasonPhrase);  { V8.37 report more }
        Exit;
    end;

    Display('Request done, StatusCode #' + IntToStr(SslHttpCli1.StatusCode));

    for I := 0 to SslHttpCli1.RcvdHeader.Count - 1 do
        Display('hdr>' + SslHttpCli1.RcvdHeader.Strings[I]);

    if SslHttpCli1.DocName = '' then
        DocumentMemo.Lines.Add('*** NO DOCUMENT FILE NAME ***')
    else begin
        if not FileExists(SslHttpCli1.DocName) then
            DocumentMemo.Lines.Add('*** NO DOCUMENT FILE ***')
        else begin
           { V8.50 convert response to correct codepage }
            DataIn := TFileStream.Create(SslHttpCli1.DocName, fmOpenRead);
            try
                if Copy(SslHttpCli1.ContentType, 1, 5) = 'text/' then begin
                   BOMSize := 0;

                 // first look for codepage in HTTP charset header, rarely set
                    HtmlCodepage := IcsContentCodepage(SslHttpCli1.ContentType);

                 // if none, look for codepage in file BOM or META headers in HTML
                    if HtmlCodepage = 0 then
                        HtmlCodepage := IcsFindHtmlCodepage(DataIn, BOMSize);
                    Display('HTML Codepage: ' + CodePageToMimeCharsetString(HtmlCodepage));

                 // finally convert stream into a string with correct codepage, including entities
                    DataStr := IcsHtmlToStr(DataIn, HtmlCodepage, true);

                 // convert HTML to string, including entities (does all above steps together)
               //     DataStr := IcsHtmlToStr(DataIn, SslHttpCli1.ContentType, true);

                 // show page
                    DocumentMemo.Lines.Add(DataStr);
                end
                else begin
                    DocumentMemo.Lines.Add('Content type is ' +
                                           SslHttpCli1.ContentType);
                    DocumentMemo.Lines.Add('Document stored in ''' +
                                           SslHttpCli1.DocName +
                                           ''' Size=' + IntToStr(DataIn.Size));
                end;
            finally
                DataIn.Free;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1HeaderData(Sender: TObject);
begin
     Display(SslHttpCli1.LastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SslCliNewSession(Sender: TObject;
    SslSession      : Pointer;
    WasReused       : Boolean;
    var IncRefCount : Boolean);
var
    HttpCli : TSslHttpCli;
begin
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    if not SessCacheCheckBox.Checked then
        Exit;
    HttpCli := (Sender as TSslHttpCli);
    if (not WasReused) then begin
        SslAvlSessionCache1.CacheCliSession(SslSession,
                                            HttpCli.CtrlSocket.PeerAddr +
                                            HttpCli.CtrlSocket.PeerPort,
                                            IncRefCount);
        Display('! New SSL session');
    end
    else
        Display('! SSL Session reused');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SslCliGetSession(
    Sender          : TObject;
    var SslSession  : Pointer;
    var FreeSession : Boolean);
var
    HttpCli : TSslHttpCli;
begin
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    if not SessCacheCheckBox.Checked then Exit;
    HttpCli := (Sender as TSslHttpCli);
    SslSession  := SslAvlSessionCache1.GetCliSession(
                                      HttpCli.CtrlSocket.PeerAddr +
                                      HttpCli.CtrlSocket.PeerPort,
                                      FreeSession);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SslVerifyPeer(
    Sender  : TObject;
    var Ok  : Integer;
    Cert    : TX509Base);
//var
//    MyCert : TX509Ex; { V8.02 }
begin
    { Alternate verification takes place in event HandshakeDone, we    }
    { accept anything temporarily here. Note that the same certificate }
    { may appear multiple times in this event when we set OK to 1      }
    { overwriting the real verify result.                              }
    OK := 1;
(*    MyCert := TX509Ex (Cert);    { V8.02, V8.27 display in SslHandshakeDone }
    Display('Received certificate'#13#10 +
            MyCert.CertInfo + #13#10 +  { V8.02 four lines with parsed cert info }
         {   'Subject: "' + Cert.SubjectOneLine + '"'#13#10 +
            'Issuer:  "' + Cert.IssuerOneLine + '"'#13#10  +  }
            'Verify result: ' + MyCert.VerifyErrMsg +
            ' Verify depth: ' + IntToStr(MyCert.VerifyDepth));   *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.ResetButtonClick(Sender: TObject);    { V8.01 }
begin
    ResetSsl(Self);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.ResetSsl(Sender: TObject);   { V8.01 }
begin
    if SslContext1.IsCtxInitialized then begin
        SslHttpCli1.CloseAsync;
        SslHttpCli1.CtrlSocket.ResetSSL;
        SslHttpCli1.SslAcceptableHosts.Clear;
        SslContext1.DeInitContext;
        Display('Reset SSL');
    end;
    FDispCiphDone := false; { V8.37 }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SslHandshakeDone(
    Sender          : TObject;
    ErrCode         : Word;
    PeerCert        : TX509Base;
    var Disconnect  : Boolean);  // If TRUE will close the connection delayed!
var
    CertChain   : TX509List;
    DlgMsg      : String;
    Hash        : String;
    HttpCli     : TSslHttpCli;
    ChainInfo   : String;
begin
    HttpCli   := Sender as TSslHttpCli;
    Display('Handshake done, error #' + IntToStr (ErrCode) +
                                     ' - ' + HttpCli.CtrlSocket.SslHandshakeRespMsg);  { V8.00 }

    { V8.56 see if ALPN selected by server }
    if HttpCli.CtrlSocket.SslGetAlpnProtocol <> '' then
        Display('Application layer protocol selected: ' + HttpCli.CtrlSocket.SslGetAlpnProtocol);

    { A simple custom verification that may be unsecure!...               }
    { See also SslVerifyPeer above.                                       }

    { Ssl handshake error or session is reused or no verification wanted  }
    if (ErrCode <> 0) or (HttpCli.CtrlSocket.SslSessionReused) or
       not HttpCli.SslContext.SslVerifyPeer then
        Exit; // nothing to do, go ahead

    Hash := PeerCert.Sha1Hex;
    { Is current host already in the list of temporarily accepted hosts ? }
    if HttpCli.SslAcceptableHosts.IndexOf(HttpCli.Hostname + Hash) > -1 then
        Exit; // previously accepted, go ahead

    { Property SslCertChain contains all certificates in current verify chain }
    CertChain := HttpCli.CtrlSocket.SslCertChain;

    { Collect further information to display in the dialog.                  }
    if CertChain.Count > 0 then begin
        ChainInfo := '! ' + 'VerifyResult: ' + PeerCert.FirstVerifyErrMsg +
             ', Peer domain: ' +  HttpCli.CtrlSocket.SslCertPeerName + #13#10 +  { V8.39 }
             IntToStr(CertChain.Count) +' Certificate(s) in the verify chain.' +
             #13#10 + CertChain.AllCertInfo(True, True);    { V8.41 }
        Display(ChainInfo + #13#10);
    end;

    { Now to the PostConnectionCheck, a very important security check!
      Our application will be vulnerable if we do not check the peer
      certificate beyond verification of the chain. Nothing prevents an
      attacker from getting his own certificate signed by one of our trusted
      CAs and then hijacking all our sessions. We thward this kind of
      masquerade by tying the certificate to some information unique to the
      machine. In SSL this information is one or multiple full qualified
      domain names (FQDN) also called DNS names stored in certificate's
      commonName field(s) of the subjectName field. Since X.509v3 the
      subjectAltName extension allows to hold the FQDN as well as other
      identifying information such as the IP address.
      We use function PostConnectionCheck to perform these checks for us. }

//    if PeerCert.PostConnectionCheck(HttpCli.Hostname) then begin  { V8.39 no longer needed }

        { Now check whether chain verify result was OK as well }
     if (PeerCert.VerifyResult = X509_V_OK) then begin
         Display('! Chain verification and host check succeeded' + #13#10);
         Exit; // Everything OK, go ahead.
     end;

     DlgMsg := 'Certificate verification failed: ' + PeerCert.FirstVerifyErrMsg + #13#10 +   { V8.39 more info }
               'Certificate peer name: ' + HttpCli.CtrlSocket.SslCertPeerName + #13#10#13#10 +
                  'Do you want to trust the connection anyway?';

    { OpenSsl's chain verification and/or our PostConnectionCheck failed,   }
    { abort current connection and ask the user what to do next.            }

    HttpCli.Abort; //***

    { Note that we may not process messages in an event, calling ShowMessage }
    { and the like would call the message pump, that was fatal if we were    }
    { still connected. If you do not need user intervention it's OK to set   }
    { Disconnect to TRUE which will close the connection delayed after this  }
    { handler returned.                                                      }

    if MessageDlg(DlgMsg, mtWarning, [mbYes, mbNo], 0) = mrYes then begin
        { If FirstVerifyResult of top most cert in CertChain equals error     }
        { X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN we hit a self-signed root cert }
        { that is not in our trusted store, we should ask the user whether he }
        { wants to trust this root CA by import into the trusted store,       }
        { this was a persistant trust.                                        }
        if (CertChain.Count > 0) and
           (PeerCert.FirstVerifyResult = X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN) and
           (CaPathEdit.Text <> '') then begin
            { This looks ugly, real applications should provide a nicer dialog }
            if (MessageDlg(PeerCert.CertInfo + #13#10 +    { V8.02 }
                'Do you also want to add this root certificate to your ' +
                'trusted CA certificate store?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes) then begin
                { Certificates stored this way are being looked up by openssl   }
                { the next time a certificate is verified, w/o initializing the }
                { SslContext first.                                             }
                PeerCert.SaveToPemFile(IncludeTrailingBackSlash(CaPathEdit.Text) +
                IntToHex(f_X509_subject_name_hash(PeerCert.X509), 8) + '.0');
                { If the same file name already exists we need to increment the }
                { extension by one, skipped in this demo.                       }
                { We could append those files to our trusted CA file later on.  }
            end;
        end;
        { Add a unique host ID to our temporarily trusted host list and repeat }
        { last request.                                                        }
        HttpCli.SslAcceptableHosts.Add(HttpCli.Hostname + Hash);
        PostMessage(Handle, WM_SSL_NOT_TRUSTED, 0, Integer(Sender));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SocketError(Sender: TObject);     { V8.51 }
begin
    Display('Socket error: ' + WSocketErrorDesc(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SocksAuthState(Sender: TObject; AuthState: TSocksAuthState);   { V8.51 }
begin
    case AuthState of
        socksAuthStart:
            Display('Socks authentification start.');
        socksAuthSuccess:
            Display('Socks authentification success.');
        socksAuthFailure:
            Display('Socks authentification failure.');
        socksAuthNotRequired:
            Display('Socks authentification not required.');
        else
            Display('Unknown socks authentification state.')
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SocksConnected(Sender: TObject;  ErrCode: Word);   { V8.51 }
begin
    if ErrCode = 0 then
        Display('Session connected to socks server: ' + SslHttpCli1.SocksServer)
    else
        Display('Session failed to connect to socks server: ' +
                    SslHttpCli1.SocksServer + ' - ' + WSocketErrorDesc(ErrCode));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SocksError(Sender: TObject; Error: Integer; Msg: string);    { V8.51 }
begin
    Display('Socks error: ' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SslCliCertRequest(Sender: TObject;
    var Cert: TX509Base);
var
    X : TX509Base;
begin
    { A very simple test of the SslCliCertRequest event.               }
    { This event is triggered only if CertFileEdit.Text is empty,      }
    { the server requested a certificate from the client,              }
    { and of course only in case of the SSL session wasn't reused.     }
    if not Assigned(FClientCerts) then begin
        if not Assigned(ClientCertDlg) then
            Application.CreateForm(TClientCertDlg, ClientCertDlg);
        { Create a pool of client certs }
        ClientCertDlg.CertListBox.Clear;
        FClientCerts := TX509List.Create(Self);
        try
            X := FClientCerts.Add;
            X.LoadFromPemFile('01cert.pem');
            X.PrivateKeyLoadFromPemFile('01key.pem', 'password');
            ClientCertDlg.CertListBox.Items.Add(X.SubjectOneLine);
            X := FClientCerts.Add;
            X.LoadFromPemFile('client.pem', True, 'password');
            ClientCertDlg.CertListBox.Items.Add(X.SubjectOneLine);
        except
            FreeAndNil(FClientCerts);
            raise
        end;
    end;
    ClientCertDlg.CertListBox.ItemIndex := 0;
    if ClientCertDlg.ShowModal = mrOK then
        Cert := FClientCerts[ClientCertDlg.CertListBox.ItemIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.ButtonOSSLVersionClick(Sender: TObject);
begin
    SslContext1.InitContext; //Pre-loads OpenSSL DLL's
    Display(OpenSslVersion);
    Display(OpenSslCompilerFlags);
    Display(OpenSslBuiltOn);
    Display(OpenSslPlatForm);
    Display(OpenSslDir);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.BackgroundException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    Display('!' + E.ClassName + ': ' + E.Message);
    CanClose := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.IcsLogger1IcsLogEvent(Sender: TObject;
  DebugOption: TLogOption; const Msg: String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.OldSslCheckBoxClick(Sender: TObject);
begin
    Display('Only effective when application restarts');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SetButtonState(State : Boolean);
begin
    GetButton.Enabled   := State;
    HeadButton.Enabled  := State;
    AbortButton.Enabled := NOT State;  { V8.01 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.39 lists the common name of all root certificates in the store }

procedure THttpsTstForm.StoreButtonClick(Sender: TObject);
var
    CertList: TX509List;
    Tot, I: Integer;
    Info: string;
begin
    PrepareConnection;
    CertList := TX509List.Create (self, True);
    try
        Tot := SslContext1.SslGetAllCerts (CertList);
        if Tot > 0 then begin
            CertList.SortChain(xsrtIssuerFirst);
            Info := '! SSL context contains ' + IntToStr (Tot) +
                                            ' certificate in store' + #13#10;
            for I := 1 to Tot do begin
                Info := Info + '#' + IntToStr (I) + ' ';
                if CertList [I-1].SubAltNameDNS <> '' then
                    Info := Info + CertList [I-1].SubAltNameDNS
                else if CertList [I-1].SubjectCName <> '' then  { V8.41 some roots blank }
                    Info := Info + CertList [I-1].SubjectCName
                else
                    Info := Info + CertList [I-1].SubjectOName;
                Info := Info + ' (' + CertList [I-1].SubjectOName + ')';
                if CertList [I-1].SubjectOUName <> '' then
                    Info := Info + ' OU: ' + CertList [I-1].SubjectOUName;   { V8.52 }
                Info := Info + #13#10;
            end;
            Display(Info);
        end
        else
            Display('! SSL context certificate store empty');
    finally
        CertList.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
