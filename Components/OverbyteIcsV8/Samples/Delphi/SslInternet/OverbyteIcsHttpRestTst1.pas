{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  ICS HTTPS REST functions demo.
Creation:     Apr 2018
Updated:      Oct 2018
Version:      8.58
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
May, 8 2018  - V8.54 baseline
Jun 15, 2018 - V8.55 Update SSL client security levels from literals
                     Added https://cloudflare-dns.com/dns-query?name=magsys.co.uk&type=MX&ct=application/dns-json
Jul 9, 2018  - V8.56 Using OverbyteIcsTypes instead of OverbyteIcsLogger
Sep 25, 2018 - V8.57 Using OnSelectDns to show alternate IP addresses, changed
                      SocketFamily to sfAny so it finds both IPV4 and IPV6 addresses
Oct 27, 2018 - V8.58 Better error handling.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpRestTst1;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TypInfo, ExtCtrls, Grids, ComCtrls, ActiveX,
  OverbyteIcsWSocket,
  OverbyteIcsIniFiles,
  OverbyteIcsTypes,
  OverbyteIcsUtils,
  OverbyteIcsMimeUtils,
  OverbyteIcsURL,
  OverbyteIcsLogger,     { for TLogOption }
  OverbyteIcsSSLEAY,
  OverbyteIcsLibeay,
  OverbyteIcsSslHttpRest,
  OverbyteIcsSuperObject,
  OverbyteIcsSslJose,
  OverbyteIcsWndControl,
  OverbyteIcsHttpProt;

type
  THttpRestForm = class(TForm)
 // properties saved
    AuthBearer: TEdit;
    AuthLogin: TEdit;
    AuthPassword: TEdit;
    AuthType: TRadioGroup;
    CertVerMethod: TRadioGroup;
    DebugLogging: TRadioGroup;
    ExtraHeaders: TMemo;
    OAuthScope: TEdit;
    OAuthOptNoRedir: TCheckBox;
    OAuthAutoRefresh: TCheckBox;
    OAuthRefrMins: TEdit;
    OAuthAccToken: TEdit;
    OAuthAppUrl: TEdit;
    OAuthAuthCode: TEdit;
    OAuthAuthType: TRadioGroup;
    OAuthClientId: TEdit;
    OAuthClientSecret: TEdit;
    OAuthExpire: TEdit;
    OAuthProtoType: TRadioGroup;
    OAuthRedirectUrl: TEdit;
    OAuthRefToken: TEdit;
    OAuthTokenUrl: TEdit;
    OAuthWebIP: TComboBox;
    OAuthWebPort: TEdit;
    ParamContent: TRadioGroup;
    RawParams: TEdit;
    ReportCertChain: TCheckBox;
    ReqMode: TRadioGroup;
    ReqType: TRadioGroup;
    RestURL: TComboBox;
    SslClientCertFile: TEdit;
    SslRootBundleFile: TEdit;
    SslSecurity: TRadioGroup;

 // properties not saved
    LogWin: TMemo;
    Label1: TLabel;
    Label3: TLabel;
    doStartReq: TButton;
    Label5: TLabel;
    HttpRest1: TSslHttpRest;
    GridParams: TStringGrid;
    RespList: TListView;
    doClear: TButton;
    doAbort: TButton;
    PageControl1: TPageControl;
    TabREST: TTabSheet;
    TabSettings: TTabSheet;
    TabOAuth: TTabSheet;
    Label2: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    GroupBox1: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    GroupBox2: TGroupBox;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    doOAuthLogin: TButton;
    doOAuthToken: TButton;
    doOAuthRefresh: TButton;
    doTestRedir: TButton;
    RestOAuth1: TRestOAuth;
    doGrantCred: TButton;
    doGrantPassword: TButton;
    Label21: TLabel;
    LabelResult: TLabel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure HttpRest1HttpRestProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure HttpRest1RestRequestDone(Sender: TObject; RqType: THttpRequest;
      ErrCode: Word);
    procedure doStartReqClick(Sender: TObject);
    procedure doAbortClick(Sender: TObject);
    procedure doClearClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure doTestRedirClick(Sender: TObject);
    procedure RestOAuth1OAuthAuthUrl(Sender: TObject; const URL: string);
    procedure doOAuthLoginClick(Sender: TObject);
    procedure doOAuthTokenClick(Sender: TObject);
    procedure doOAuthRefreshClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure doGrantCredClick(Sender: TObject);
    procedure doGrantPasswordClick(Sender: TObject);
    procedure RestOAuth1OAuthNewCode(Sender: TObject);
    procedure RestOAuth1OAuthNewToken(Sender: TObject);
    procedure SettingsChange(Sender: TObject);
    procedure HttpRest1SelectDns(Sender: TObject; DnsList: TStrings;
      var NewDns: string);
  private
    { Private declarations }
  public
    { Public declarations }
    FProgDir: String;
    FIniFileName: String;
    FCookieFileName: String;
    FInitialized: Boolean;
    procedure AddLog (const S: string) ;
    procedure RestOAuthSetup;

  end;

var
  HttpRestForm: THttpRestForm;

implementation

{$R *.dfm}

const
    SectionMainWindow    = 'MainWindow';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';
    SectionData          = 'Data';
    KeyRestParams        = 'RestParams';


procedure THttpRestForm.FormCreate(Sender: TObject);
begin
    FProgDir     := ExtractFilePath(ParamStr(0));
    FIniFileName := GetIcsIniFileName;
    FCookieFileName := ChangeFileExt(FIniFileName, '.cookie');

 // Avoid dynamical loading and unloading the SSL DLLs plenty of times
    GSSLEAY_DLL_IgnoreNew := False;    { don't ignore OpenSSL 1.1.0 and later }
//  GSSLEAY_DLL_IgnoreNew := True;     { don't ignore OpenSSL 1.1.0 and later }
//  GSSLEAY_DLL_IgnoreOld := True;     { ignore OpenSSL 1.0.2 and earlier }
    GSSL_DLL_DIR := FProgDir;          { only from our directory }
    GSSL_SignTest_Check := True;       { check digitally signed }
    GSSL_SignTest_Certificate := True; { check digital certificate }
    OverbyteIcsWSocket.LoadSsl;
    GridParams.Cells[0,0] := 'Name';
    GridParams.Cells[1,0] := 'Value';
    GridParams.Cells[2,0] := 'Raw (Y/N)';
//    CoInitializeEx(nil, COINIT_MULTITHREADED OR COINIT_DISABLE_OLE1DDE); { keep COM/ActiveX happy, free it on close }
end;

procedure THttpRestForm.FormDestroy(Sender: TObject);
begin
//    CoUnInitialize;
end;

procedure THttpRestForm.FormShow(Sender: TObject);
var
    IniFile: TIcsIniFile;
    SL: TStringList;
    I, J, K, tot: Integer;
    Level: TSslCliSecurity;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

    // V8.55 update SSL client security levels
        SslSecurity.Items.Clear;
        for Level := Low(TSslCliSecurity) to High(TSslCliSecurity) do
             SslSecurity.Items.Add (SslCliSecurityNames[Level]);

        IniFile := TIcsIniFile.Create(FIniFileName);
        Width := IniFile.ReadInteger(SectionMainWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionMainWindow, KeyHeight, Height);
        Top := IniFile.ReadInteger(SectionMainWindow, KeyTop, (Screen.Height - Height) div 2);
        Left := IniFile.ReadInteger(SectionMainWindow, KeyLeft, (Screen.Width  - Width)  div 2);
        SL := TStringList.Create;
        try
             SL.Delimiter := '|';
             SL.DelimitedText := IniFile.ReadString(SectionData, KeyRestParams, '');
             tot := SL.Count;
             K := 0;
             for I := 0 to GridParams.RowCount - 1 do begin
                for J := 0 to GridParams.ColCount - 1 do begin
                    if K < tot then
                        GridParams.Cells[J, I] := SL[K];
                    K := K + 1;
                end;
             end;
        finally
             SL.Free;
        end;

       with IniFile do begin
          AuthBearer.Text := ReadString (SectionData, 'AuthBearer_Text', AuthBearer.Text) ;
          AuthLogin.Text := ReadString (SectionData, 'AuthLogin_Text', AuthLogin.Text) ;
          AuthPassword.Text := ReadString (SectionData, 'AuthPassword_Text', AuthPassword.Text) ;
          AuthType.ItemIndex := ReadInteger (SectionData, 'AuthType_ItemIndex', AuthType.ItemIndex) ;
          CertVerMethod.ItemIndex := ReadInteger (SectionData, 'CertVerMethod_ItemIndex', CertVerMethod.ItemIndex) ;
          DebugLogging.ItemIndex := ReadInteger (SectionData, 'DebugLogging_ItemIndex', DebugLogging.ItemIndex) ;
          ExtraHeaders.Lines.CommaText := ReadString (SectionData, 'ExtraHeaders_Lines', '') ;
          OAuthScope.Text := ReadString (SectionData, 'OAuthScope_Text', OAuthScope.Text) ;
          if ReadString (SectionData, 'OAuthAutoRefresh_Checked', 'False') = 'True' then OAuthAutoRefresh.Checked := true else OAuthAutoRefresh.Checked := false ;
          if ReadString (SectionData, 'OAuthOptNoRedir_Checked', 'False') = 'True' then OAuthOptNoRedir.Checked := true else OAuthOptNoRedir.Checked := false ;
          OAuthRefrMins.Text := ReadString (SectionData, 'OAuthRefrMins_Text', OAuthRefrMins.Text) ;
          OAuthAccToken.Text := ReadString (SectionData, 'OAuthAccToken_Text', OAuthAccToken.Text) ;
          OAuthAppUrl.Text := ReadString (SectionData, 'OAuthAppUrl_Text', OAuthAppUrl.Text) ;
          OAuthAuthType.ItemIndex := ReadInteger (SectionData, 'OAuthAuthType_ItemIndex', OAuthAuthType.ItemIndex) ;
          OAuthClientId.Text := ReadString (SectionData, 'OAuthClientId_Text', OAuthClientId.Text) ;
          OAuthClientSecret.Text := ReadString (SectionData, 'OAuthClientSecret_Text', OAuthClientSecret.Text) ;
          OAuthExpire.Text := ReadString (SectionData, 'OAuthExpire_Text', OAuthExpire.Text) ;
          OAuthProtoType.ItemIndex := ReadInteger (SectionData, 'OAuthProtoType_ItemIndex', OAuthProtoType.ItemIndex) ;
          OAuthRedirectUrl.Text := ReadString (SectionData, 'OAuthRedirectUrl_Text', OAuthRedirectUrl.Text) ;
          OAuthRefToken.Text := ReadString (SectionData, 'OAuthRefToken_Text', OAuthRefToken.Text) ;
          OAuthTokenUrl.Text := ReadString (SectionData, 'OAuthTokenUrl_Text', OAuthTokenUrl.Text) ;
          OAuthWebIP.Text := ReadString (SectionData, 'OAuthWebIP_Text', OAuthWebIP.Text) ;
          OAuthWebPort.Text := ReadString (SectionData, 'OAuthWebPort_Text', OAuthWebPort.Text) ;
          ParamContent.ItemIndex := ReadInteger (SectionData, 'ParamContent_ItemIndex', ParamContent.ItemIndex) ;
          RawParams.Text := ReadString (SectionData, 'RawParams_Text', RawParams.Text) ;
          if ReadString (SectionData, 'ReportCertChain_Checked', 'False') = 'True' then ReportCertChain.Checked := true else ReportCertChain.Checked := false ;
          ReqMode.ItemIndex := ReadInteger (SectionData, 'ReqMode_ItemIndex', ReqMode.ItemIndex) ;
          ReqType.ItemIndex := ReadInteger (SectionData, 'ReqType_ItemIndex', ReqType.ItemIndex) ;
          RestURL.Text := ReadString (SectionData, 'RestURL_Text', RestURL.Text) ;
          SslClientCertFile.Text := ReadString (SectionData, 'SslClientCertFile_Text', SslClientCertFile.Text) ;
          SslRootBundleFile.Text := ReadString (SectionData, 'SslRootBundleFile_Text', SslRootBundleFile.Text) ;
          SslSecurity.ItemIndex := ReadInteger (SectionData, 'SslSecurity_ItemIndex', SslSecurity.ItemIndex) ;
       end;
        IniFile.Free;
    end;

    if HttpRest1.SslRootFile = '' then
        SslRootBundleFile.Text := HttpRest1.SslRootFile;
    HttpRest1.RestCookies.LoadFromFile(FCookieFileName);
    OAuthWebIP.Items.Assign(LocalIPList); 
    OAuthWebIP.Items.Insert(0, ICS_LOCAL_HOST_V4);
end;

procedure THttpRestForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
    temp: String;
    SL: TStringList;
    I, J: Integer;
begin
    HttpRest1.RestCookies.SaveToFile(FCookieFileName);
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionMainWindow, KeyTop, Top);
    IniFile.WriteInteger(SectionMainWindow, KeyLeft, Left);
    IniFile.WriteInteger(SectionMainWindow, KeyWidth, Width);
    IniFile.WriteInteger(SectionMainWindow, KeyHeight, Height);
    SL := TStringList.Create;
    try
        SL.Delimiter := '|';
        for I := 0 to GridParams.RowCount - 1 do begin
            for J := 0 to GridParams.ColCount - 1 do begin
                SL.Add(GridParams.Cells[J,I]);
            end;
        end;
        IniFile.WriteString(SectionData, KeyRestParams, SL.DelimitedText);
    finally
        SL.Free;
    end;

    with IniFile do begin
      WriteString (SectionData, 'AuthBearer_Text', AuthBearer.Text) ;
      WriteString (SectionData, 'AuthLogin_Text', AuthLogin.Text) ;
      WriteString (SectionData, 'AuthPassword_Text', AuthPassword.Text) ;
      WriteInteger (SectionData, 'AuthType_ItemIndex', AuthType.ItemIndex) ;
      WriteInteger (SectionData, 'CertVerMethod_ItemIndex', CertVerMethod.ItemIndex) ;
      WriteInteger (SectionData, 'DebugLogging_ItemIndex', DebugLogging.ItemIndex) ;
      WriteString (SectionData, 'ExtraHeaders_Lines', ExtraHeaders.Lines.CommaText) ;
      WriteString (SectionData, 'OAuthScope_Text', OAuthScope.Text) ;
      if OAuthAutoRefresh.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'OAuthAutoRefresh_Checked', temp) ;
      if OAuthOptNoRedir.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'OAuthOptNoRedir_Checked', temp) ;
      WriteString (SectionData, 'OAuthRefrMins_Text', OAuthRefrMins.Text) ;
      WriteString (SectionData, 'OAuthAccToken_Text', OAuthAccToken.Text) ;
      WriteString (SectionData, 'OAuthAppUrl_Text', OAuthAppUrl.Text) ;
      WriteInteger (SectionData, 'OAuthAuthType_ItemIndex', OAuthAuthType.ItemIndex) ;
      WriteString (SectionData, 'OAuthClientId_Text', OAuthClientId.Text) ;
      WriteString (SectionData, 'OAuthClientSecret_Text', OAuthClientSecret.Text) ;
      WriteString (SectionData, 'OAuthExpire_Text', OAuthExpire.Text) ;
      WriteInteger (SectionData, 'OAuthProtoType_ItemIndex', OAuthProtoType.ItemIndex) ;
      WriteString (SectionData, 'OAuthRedirectUrl_Text', OAuthRedirectUrl.Text) ;
      WriteString (SectionData, 'OAuthRefToken_Text', OAuthRefToken.Text) ;
      WriteString (SectionData, 'OAuthTokenUrl_Text', OAuthTokenUrl.Text) ;
      WriteString (SectionData, 'OAuthWebIP_Text', OAuthWebIP.Text) ;
      WriteString (SectionData, 'OAuthWebPort_Text', OAuthWebPort.Text) ;
      WriteInteger (SectionData, 'ParamContent_ItemIndex', ParamContent.ItemIndex) ;
      WriteString (SectionData, 'RawParams_Text', RawParams.Text) ;
      if ReportCertChain.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ReportCertChain_Checked', temp) ;
      WriteInteger (SectionData, 'ReqMode_ItemIndex', ReqMode.ItemIndex) ;
      WriteInteger (SectionData, 'ReqType_ItemIndex', ReqType.ItemIndex) ;
      WriteString (SectionData, 'RestURL_Text', RestURL.Text) ;
      WriteString (SectionData, 'SslClientCertFile_Text', SslClientCertFile.Text) ;
      WriteString (SectionData, 'SslRootBundleFile_Text', SslRootBundleFile.Text) ;
      WriteInteger (SectionData, 'SslSecurity_ItemIndex', SslSecurity.ItemIndex) ;
    end;
    IniFile.UpdateFile;
    IniFile.Free;
end;


procedure THttpRestForm.AddLog (const S: string) ;
begin
    if Pos (IcsLF,S) > 0 then
        LogWin.Lines.Text := LogWin.Lines.Text + IcsCRLF + S
    else
        LogWin.Lines.Add (S) ;
    SendMessage(LogWin.Handle, EM_LINESCROLL, 0, 999999);
end;


procedure THttpRestForm.HttpRest1HttpRestProg(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    AddLog(Msg);
end;


procedure THttpRestForm.doAbortClick(Sender: TObject);
begin
    if (HttpRest1.State > httpReady) or HttpRest1.Connected then begin
        AddLog ('Aborting operation');
        HttpRest1.Abort;
    end;
    doStartReq.Enabled := True;
end;


procedure THttpRestForm.doClearClick(Sender: TObject);
begin
    LogWin.Lines.Clear;
    RespList.Items.Clear;
end;


procedure THttpRestForm.SettingsChange(Sender: TObject);
begin
    if HttpRest1.Connected then
        HttpRest1.Abort;  // close socket so new settings used
end;


procedure THttpRestForm.doStartReqClick(Sender: TObject);
const
    ReqList: array[0..4] of THttpRequest =
               (httpGET, httpPOST, httpHEAD, httpPUT, httpDELETE) ;
var
    StatCode, Row: Integer;
    Req: THttpRequest;
    Async: Boolean;
begin
    doStartReq.Enabled := False;
    RespList.Items.Clear;

 // optional HTTP parameters, all have defaults so can be ignored if not needed
    Req := ReqList[ReqType.ItemIndex];
    Async := (ReqMode.ItemIndex = 1);
    HttpRest1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    HttpRest1.CertVerMethod := TCertVerMethod(CertVerMethod.ItemIndex);
    HttpRest1.SslCliSecurity := TSslCliSecurity(SslSecurity.ItemIndex);
    HttpRest1.SslReportChain := ReportCertChain.Checked;
    if SslClientCertFile.Text <> '' then begin
        if FileExists (SslClientCertFile.Text) then begin
            HttpRest1.SslCliCert.LoadFromFile(SslClientCertFile.Text);
        end
        else
            AddLog ('SSL client certificate ignored, not found: ' +
                                                      SslClientCertFile.Text);
    end;
    HttpRest1.SslRootFile := SslRootBundleFile.Text;
    HttpRest1.ServerAuth := THttpAuthType(AuthType.ItemIndex);
    HttpRest1.Username := AuthLogin.Text;
    HttpRest1.Password := AuthPassword.Text;
    HttpRest1.AuthBearerToken := AuthBearer.Text;
    HttpRest1.ExtraHeaders := ExtraHeaders.Lines;
    HttpRest1.SocketFamily := sfAny;  { V8.57 IP4 and IPV6 }

  // read grid and build REST paramaters
    HttpRest1.RestParams.Clear;
    for Row := 1 to GridParams.RowCount do begin
        if (Trim(GridParams.Cells[0,Row]) <> '') then begin
            HttpRest1.RestParams.AddItem(Trim(GridParams.Cells[0,Row]),
                Trim(GridParams.Cells[1,Row]), (GridParams.Cells[2,Row] = 'Y'));
        end;
    end;
    HttpRest1.RestParams.PContent := TPContent(ParamContent.ItemIndex);

  // make HTTP request, note RestParams are ignored if RawRarams not blank
    StatCode := HttpRest1.RestRequest(Req, RestURL.Text, Async, RawParams.Text);
    if Async then
        AddLog ('Async request started')
    else begin
        AddLog ('Sync request completed, Status ' + IntToStr (StatCode));
      // for sync we can process the result here instead of HttpRest1RestRequestDone
        doStartReq.Enabled := True;
    end;
end;

procedure THttpRestForm.HttpRest1RestRequestDone(Sender: TObject;
  RqType: THttpRequest; ErrCode: Word);
var
    JsonItem: TSuperAvlEntry;
    JsonObj: ISuperObject;
    JsonEnum: TSuperAvlIterator;
    I, CWid: integer;
    FirstCol, FirstRow: Boolean;
    CVal: String;
begin
    doStartReq.Enabled := True;
    if ErrCode <> 0 then begin
        AddLog('Request failed, error #' + IntToStr(ErrCode) +
              '. Status = ' + IntToStr(HttpRest1.StatusCode) +
                               ' - ' + HttpRest1.ReasonPhrase);
        Exit;
    end;

    AddLog('Request done, StatusCode #' + IntToStr(HttpRest1.StatusCode));
    AddLog (String(HttpRest1.ResponseRaw));

    if ((Pos('{', HttpRest1.ResponseRaw) > 0) or
           (Pos('[', HttpRest1.ResponseRaw) > 0)) and
                Assigned(HttpRest1.ResponseJson) then begin
        try
            AddLog ('Json main content type: ' + GetEnumName(TypeInfo(TSuperType),
                                                Ord(HttpRest1.ResponseJson.DataType)));

         // two columns only
         // note that values containing objects are displayed as raw Json
            if HttpRest1.ResponseJson.DataType = stObject then begin
                RespList.Columns.Clear;
                with RespList.Columns.Add do begin
                    Caption := 'Name';
                    Width := 100;
                end;
                with RespList.Columns.Add do begin
                    Caption := 'Type';
                    Width := 70;
                end;
                with RespList.Columns.Add do begin
                    Caption := 'Value';
                    Width := 400;
                end;
        {        for JsonItem in HttpRest1.ResponseJson.AsObject do begin
                    with RespList.Items.Add do begin
                        Caption := JsonItem.Name;
                        SubItems.Add(GetEnumName(TypeInfo(TSuperType),
                                                Ord(JsonItem.Value.DataType)));
                        SubItems.Add(JsonItem.Value.AsString);
                    end;
                end;   }
                JsonEnum := HttpRest1.ResponseJson.AsObject.GetEnumerator;
                try
                    while JsonEnum.MoveNext do begin
                        JsonItem := JsonEnum.GetIter;
                        with RespList.Items.Add do begin
                            Caption := JsonItem.Name;
                            SubItems.Add(GetEnumName(TypeInfo(TSuperType),
                                                    Ord(JsonItem.Value.DataType)));
                            SubItems.Add(JsonItem.Value.AsString);
                        end;
                    end;
                finally
                    JsonEnum.Free;
                end;
            end;

         // one column per Value, with Name as title
            if HttpRest1.ResponseJson.DataType = stArray then begin
                RespList.Items.BeginUpdate;
                RespList.Columns.Clear;
                FirstRow := True;
                for I := 0 to  HttpRest1.ResponseJson.AsArray.Length - 1 do begin
                    JsonObj := HttpRest1.ResponseJson.AsArray[I];
                    FirstCol := True;
                    with RespList.Items.Add do begin
                        JsonEnum := JsonObj.AsObject.GetEnumerator;
                        while JsonEnum.MoveNext do begin
                            JsonItem := JsonEnum.GetIter;
                            CVal := JsonItem.Value.AsString;
                            if FirstRow then begin
                                CWid := (Length(CVal) * 5) + 20;
                                with RespList.Columns.Add do begin
                                    Caption := JsonItem.Name;
                                    Width := CWid;
                                end;
                            end;
                            if FirstCol then
                                Caption := CVal
                            else
                                SubItems.Add(CVal);
                            FirstCol := False;
                        end;
                    end;
                    FirstRow := False;
                end;
                RespList.Items.EndUpdate;
            end;

        except
            on E:Exception do
                AddLog('Error parsing Json: ' + E.Message);
        end;
    end;

end;

procedure THttpRestForm.HttpRest1SelectDns(Sender: TObject; DnsList: TStrings;
  var NewDns: string);
begin
    AddLog('Looked-up DNS: ' + NewDns);
    if DnsList.Count > 1 then begin
        AddLog(IntToStr(DnsList.Count) + ' alternate addresses: ' + DnsList.CommaText);
      { we could select an alternate now, round robin or IPV4/IPV6 }
    end;
end;


procedure THttpRestForm.RestOAuthSetup;
begin
    LabelResult.Caption := 'Result: Please Wait';
    RestOAuth1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    RestOAuth1.ProtoType := TOAuthProto(OAuthProtoType.ItemIndex);
    RestOAuth1.AuthCode := OAuthAuthCode.Text;
    RestOAuth1.AuthType := TOAuthType(OAuthAuthType.ItemIndex);
    RestOAuth1.AppUrl := Trim(OAuthAppUrl.Text);
    RestOAuth1.RedirectMsg := 'App: ' + OAuthAppUrl.Text;
    RestOAuth1.ClientId := Trim(OAuthClientId.Text);
    RestOAuth1.ClientSecret := Trim(OAuthClientSecret.Text);
    RestOAuth1.OAOptions := [];
    if OAuthOptNoRedir.Checked then
        RestOAuth1.OAOptions := RestOAuth1.OAOptions  + [OAopAuthNoRedir];
    RestOAuth1.RefreshAuto := OAuthAutoRefresh.Checked;
    RestOAuth1.RefrMinsPrior := atoi(OAuthRefrMins.Text);
    RestOAuth1.RefreshToken := OAuthRefToken.Text;
    RestOAuth1.Scope := Trim(OAuthScope.Text);
    RestOAuth1.TokenUrl := Trim(OAuthTokenUrl.Text);
    RestOAuth1.RedirectUrl := Trim(OAuthRedirectUrl.Text);
    RestOAuth1.WebSrvIP := Trim(OAuthWebIP.Text);
    RestOAuth1.WebSrvPort := Trim(OAuthWebPort.Text);
end;


{ call back for embedded browser window }
procedure THttpRestForm.RestOAuth1OAuthAuthUrl(Sender: TObject;
                                                const URL: string);
begin
    if ((Sender as TRestOAuth).AuthType = OAuthTypeMan) then begin
        AddLog('Please copy this URL and browse to it, then enter Auth Code: ' + URL);
    end
    else if ((Sender as TRestOAuth).AuthType = OAuthTypeEmbed) then begin
// display TWebBrowser window with URL
// trap redirect URL, and capture code=xxx
    end ;
end;

procedure THttpRestForm.RestOAuth1OAuthNewCode(Sender: TObject);
begin
    OAuthAuthCode.Text := (Sender as TRestOAuth).AuthCode;
    LabelResult.Caption := 'Result: Got New Auth Code OK';
  // NOTE - AuthCode usually expires in 10 minutes or less
end;

procedure THttpRestForm.RestOAuth1OAuthNewToken(Sender: TObject);
begin
    OAuthAccToken.Text := (Sender as TRestOAuth).AccToken;
    OAuthRefToken.Text := (Sender as TRestOAuth).RefreshToken;
    OAuthExpire.Text := DateTimeToStr((Sender as TRestOAuth).ExpireDT);
    AuthBearer.Text := (Sender as TRestOAuth).AccToken;
    LabelResult.Caption := 'Result: Got New Token OK';
end;

procedure THttpRestForm.doTestRedirClick(Sender: TObject);
begin
    RestOAuthSetup;
    RestOAuth1.TestRedirect;
end;

procedure THttpRestForm.doOAuthLoginClick(Sender: TObject);
begin
    RestOAuthSetup;
    if RestOAuth1.StartAuthorization then
        LabelResult.Caption := 'Result: Waiting for Auth Code'
    else
        LabelResult.Caption := 'Result: Failed - ' + RestOAuth1.LastError;
end;


procedure THttpRestForm.doOAuthTokenClick(Sender: TObject);
begin
    RestOAuthSetup;
    if RestOAuth1.GrantAuthToken then
        LabelResult.Caption := 'Result: Got New Token OK'
    else
        LabelResult.Caption := 'Result: Failed - ' + RestOAuth1.LastError;
end;

procedure THttpRestForm.doOAuthRefreshClick(Sender: TObject);
begin
    RestOAuthSetup;
    if RestOAuth1.GrantRefresh then
        LabelResult.Caption := 'Result: Got New Token OK'
    else
        LabelResult.Caption := 'Result: Failed - ' + RestOAuth1.LastError;
end;

procedure THttpRestForm.doGrantCredClick(Sender: TObject);
begin
    RestOAuthSetup;
    if RestOAuth1.GrantAppToken then
        LabelResult.Caption := 'Result: Got New Token OK'
    else
        LabelResult.Caption := 'Result: Failed - ' + RestOAuth1.LastError;
end;

procedure THttpRestForm.doGrantPasswordClick(Sender: TObject);
begin
    RestOAuthSetup;
    if RestOAuth1.GrantPasswordToken(Trim(AuthLogin.Text), Trim(AuthPassword.Text)) then
        LabelResult.Caption := 'Result: Got New Token OK'
    else
        LabelResult.Caption := 'Result: Failed - ' + RestOAuth1.LastError;
end;

end.
