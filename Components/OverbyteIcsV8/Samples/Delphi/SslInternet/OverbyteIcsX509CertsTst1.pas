{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Automatically download SSL X509 certificates from various
              issuers, including free certificates from Let's Encrypt, and
              commercial certificates from CertCentre AG and Servertastic.
              Supports and ACME V1 and V2 protocols, and REST protocols
              for specific vendors.  Domain validated certificates should
              generally be issued without internvention, other commercial
              certificates may take days to be approved.
Creation:     May 2018
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
May 22, 2018 - V8.54 baseline
Oct 5, 2018  - V8.57 Added Database tab and settings
Oct 19, 2018 - V8.58 Bug fixes

For docunentation on how to use this sample, please see a lengthy Overview in
the OverbyteIcsSslX509Certs.pas unit.

Pending - Waiting challenges list

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsX509CertsTst1;

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
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, Buttons,
  OverbyteIcsTypes,
  OverbyteIcsWSocket,
  OverbyteIcsWndControl,
  OverbyteIcsHttpProt,
  OverbyteIcsIniFiles,
  OverbyteIcsUtils,
  OverbyteIcsMimeUtils,
  OverbyteIcsURL,
  OverbyteIcsLogger,     { for TLogOption }
  OverbyteIcsSSLEAY,
  OverbyteIcsLibeay,
  OverbyteIcsSslHttpRest,
  OverbyteIcsSuperObject,
  OverbyteIcsSslJose,
  OverbyteIcsSslX509Utils,
  OverbyteIcsSslX509Certs;

type
  TX509CertsForm = class(TForm)
 // properties saved
    AccAcmeKeyV1: TComboBox;
    AccAcmeKeyV2: TComboBox;
    AcmeServerV1: TComboBox;
    AcmeServerV2: TComboBox;
    AutoOrderComplete: TCheckBox;
    CACertFile: TEdit;
    CAPkeyFile: TEdit;
    CAPkeyPw: TEdit;
    CertAddress: TEdit;
    CertCentreApprovEmail: TComboBox;
    CertCentreOrderId: TEdit;
    CertCentreOrderRef: TEdit;
    CertCentreProducts: TListBox;
    CertCentreServer: TComboBox;
    CertContactEmail: TEdit;
    CertContactFirst: TEdit;
    CertContactLast: TEdit;
    CertContactTitle: TEdit;
    CertCountry: TEdit;
    CertCsrOrigin: TRadioGroup;
    CertLocality: TEdit;
    CertOldCsrFile: TEdit;
    CertOldPrvKey: TEdit;
    CertOrganization: TEdit;
    CertOrganizationalUnit: TEdit;
    CertOutFmtBudl: TCheckBox;
    CertOutFmtP12: TCheckBox;
    CertOutFmtP7: TCheckBox;
    CertOutFmtReq: TCheckBox;
    CertOutFmtSep: TCheckBox;
    CertPhone: TEdit;
    CertPostCode: TEdit;
    CertSignDigestType: TRadioGroup;
    CertState: TEdit;
    CertValidity: TEdit;
    DebugLogging: TComboBox;
    DirAcmeConfV1: TEdit;
    DirAcmeConfV2: TEdit;
    DirCertCenConf: TEdit;
    DirDatabase: TComboBox;
    DirLogs: TEdit;
    DirPubWebCert: TEdit;
    DirWellKnown: TEdit;
    DomWebSrvIP: TComboBox;
    LogJson: TCheckBox;
    LogPkeys: TCheckBox;
    OAuthAccToken: TEdit;
    OAuthAppUrl: TEdit;
    OAuthAutoRefresh: TCheckBox;
    OAuthClientId: TEdit;
    OAuthClientSecret: TEdit;
    OAuthExpire: TEdit;
    OAuthRedirectUrl: TEdit;
    OAuthRefToken: TEdit;
    OAuthRefrMins: TEdit;
    OAuthScope: TEdit;
    OAuthTokenUrl: TEdit;
    OAuthWebIP: TComboBox;
    OAuthWebPort: TEdit;
    OldOSL: TCheckBox;
    OwnCACertDir: TEdit;
    PrivKeyCipher: TRadioGroup;
    PrivKeyPassword: TEdit;
    PrivKeyType: TRadioGroup;
    SuppCertChallenge: TRadioGroup;
    SupplierEmail: TEdit;

 // properties not saved
    X509Certs1: TSslX509Certs;
    LogWin: TMemo;
    CertSANGrid: TStringGrid;
    CertBuy: TEdit;
    OAuthAuthCode: TEdit;
    PageControl1: TPageControl;
    TabCommon: TTabSheet;
    Label3: TLabel;
    Label15: TLabel;
    doWebServer: TButton;
    TabDomain: TTabSheet;
    Label9: TLabel;
    Label2: TLabel;
    Label13: TLabel;
    Label23: TLabel;
    doTestWellKnown: TButton;
    TabInfo: TTabSheet;
    lbCountry: TLabel;
    lbState: TLabel;
    lbLocality: TLabel;
    lbOrganization: TLabel;
    lbOrganizationalUnit: TLabel;
    lbEMail: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label24: TLabel;
    TabAcme1: TTabSheet;
    Label1: TLabel;
    Label5: TLabel;
    Label28: TLabel;
    LabelAcme1Cert: TLabel;
    Label29: TLabel;
    doAcmeOrderV1: TButton;
    doAcmeAccV1: TButton;
    doAcmeCheckOrderV1: TButton;
    doAcmeGetCertV1: TButton;
    TabAcme2: TTabSheet;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    doAcmeAccV2: TButton;
    doAcmeCheckOrderV2: TButton;
    doAcmeOrderV2: TButton;
    doAcmeGetCertV2: TButton;
    TabCCOrder: TTabSheet;
    Label6: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label16: TLabel;
    LabelCertInfo: TLabel;
    Label4: TLabel;
    doCCProfile: TButton;
    doCertCentreAlways: TButton;
    doCertCentreCollect: TButton;
    doCertCentreOther: TButton;
    doCertCentreOrders: TButton;
    doCertCentreCheck: TButton;
    TabCCOAuth: TTabSheet;
    TabServtas: TTabSheet;
    BoxOAuthApp: TGroupBox;
    Label7: TLabel;
    Label11: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label34: TLabel;
    BoxOAuthTokens: TGroupBox;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    BoxOAuthWeb: TGroupBox;
    Label39: TLabel;
    Label40: TLabel;
    Label14: TLabel;
    Label22: TLabel;
    Label41: TLabel;
    doOARefreshNow: TButton;
    doOACodeToken: TButton;
    Label42: TLabel;
    CertCommonName: TEdit;
    LabelAcme2Info: TLabel;
    TabDatabase: TTabSheet;
    Label43: TLabel;
    SelDirDatabase: TBitBtn;
    OpenDirDiag: TOpenDialog;
    doOpenDatabase: TButton;
    LabelDB: TLabel;
    Label45: TLabel;
    doDBCheck: TButton;
    doDBOrder: TButton;
    LabelInfoDomain: TLabel;
    Acme2DnsUpdated: TCheckBox;
    doClearDomain: TButton;
    doCloseDatabase: TButton;
    TabOwnCA: TTabSheet;
    CABox: TGroupBox;
    Label44: TLabel;
    Label46: TLabel;
    SelCACertFile: TBitBtn;
    SelCAPkeyFile: TBitBtn;
    doSelfSigned: TButton;
    doCASignCert: TButton;
    LabelOwnCA: TLabel;
    Label48: TLabel;
    doLoadCA: TButton;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    OpenFileDlg: TOpenDialog;
    BoxCertFmts: TGroupBox;
    SelDirLogs: TBitBtn;
    SelCertOldCsrFile: TBitBtn;
    SelCertOldPrvKey: TBitBtn;
    SelDirWellKnown: TBitBtn;
    SelDirPubWebCert: TBitBtn;
    SelDirAcmeConfV1: TBitBtn;
    SelDirAcmeConfV2: TBitBtn;
    SelDirCertCenConf: TBitBtn;
    doCheckCSR: TButton;
    LabelCertOwnCA: TLabel;
    SelOwnCACertDir: TBitBtn;
    CertSerNumType: TRadioGroup;
    DatabaseDomains: TListView;
    TabChallenges: TTabSheet;
    DatabaseChallg: TListView;
    Label47: TLabel;
    Label53: TLabel;
    doDBRevoke: TButton;
    doDBCollect: TButton;
    doDBCancel: TButton;
    doDBRemove: TButton;
    CCDnsUpdated: TCheckBox;
    doAcmeSaveOrderV2: TButton;
    doCertCentreSaveOrder: TButton;
    doDBRedist: TButton;


    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doCCProfileClick(Sender: TObject);
    procedure doCertCentreCheckClick(Sender: TObject);
    procedure doCertCentreAlwaysClick(Sender: TObject);
    procedure doCertCentreOtherClick(Sender: TObject);
    procedure doCertCentreOrdersClick(Sender: TObject);
    procedure doCertCentreCollectClick(Sender: TObject);
    procedure CertCentreProductsClick(Sender: TObject);
    procedure doTestWellKnownClick(Sender: TObject);
    procedure doAcmeAccV1Click(Sender: TObject);
    procedure doAcmeCheckOrderV1Click(Sender: TObject);
    procedure doAcmeOrderV1Click(Sender: TObject);
    procedure doAcmeGetCertV1Click(Sender: TObject);
    procedure doAcmeAccV2Click(Sender: TObject);
    procedure doAcmeCheckOrderV2Click(Sender: TObject);
    procedure doAcmeOrderV2Click(Sender: TObject);
    procedure doAcmeGetCertV2Click(Sender: TObject);
    procedure doWebServerClick(Sender: TObject);
    procedure X509Certs1CertProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure X509Certs1NewCert(Sender: TObject);
    procedure X509Certs1NewToken(Sender: TObject);
    procedure doOARefreshNowClick(Sender: TObject);
    procedure doOACodeTokenClick(Sender: TObject);
    procedure X509Certs1ChallengeDNS(Sender: TObject;
      ChallengeItem: TChallengeItem);
    procedure X509Certs1ChallengeEmail(Sender: TObject;
      ChallengeItem: TChallengeItem);
    procedure X509Certs1ChallengeFTP(Sender: TObject;
      ChallengeItem: TChallengeItem);
    procedure SelDirDatabaseClick(Sender: TObject);
    procedure doOpenDatabaseClick(Sender: TObject);
    procedure DatabaseDomainsClick(Sender: TObject);
    procedure doDBCheckClick(Sender: TObject);
    procedure doDBOrderClick(Sender: TObject);
    procedure doClearDomainClick(Sender: TObject);
    procedure doCloseDatabaseClick(Sender: TObject);
    procedure doAcmeRevokeV2Click(Sender: TObject);
    procedure SelCACertFileClick(Sender: TObject);
    procedure doLoadCAClick(Sender: TObject);
    procedure SelCAPkeyFileClick(Sender: TObject);
    procedure SelDirLogsClick(Sender: TObject);
    procedure SelCertOldCsrFileClick(Sender: TObject);
    procedure SelCertOldPrvKeyClick(Sender: TObject);
    procedure SelDirWellKnownClick(Sender: TObject);
    procedure SelDirPubWebCertClick(Sender: TObject);
    procedure SelDirAcmeConfV1Click(Sender: TObject);
    procedure SelDirAcmeConfV2Click(Sender: TObject);
    procedure SelDirCertCenConfClick(Sender: TObject);
    procedure doCheckCSRClick(Sender: TObject);
    procedure doCASignCertClick(Sender: TObject);
    procedure doSelfSignedClick(Sender: TObject);
    procedure SelOwnCACertDirClick(Sender: TObject);
    procedure doDBCollectClick(Sender: TObject);
    procedure doDBCancelClick(Sender: TObject);
    procedure doDBRevokeClick(Sender: TObject);
    procedure doDBRemoveClick(Sender: TObject);
    procedure X509Certs1ChallgRefresh(Sender: TObject);
    procedure X509Certs1DomainsRefresh(Sender: TObject);
    procedure X509Certs1SuppDBRefresh(Sender: TObject);
    procedure doAcmeSaveOrderV2Click(Sender: TObject);
    procedure doCertCentreSaveOrderClick(Sender: TObject);
    procedure X509Certs1OAuthAuthUrl(Sender: TObject; const URL: string);
    procedure doDBRedistClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FProgDir: String;
    FIniFileName: String;
    FCookieFileName: String;
    FInitialized: Boolean;
    FLogFStream: TFileStream;
    FLogOpen: Boolean;
    FPendCCProfile: Boolean;
    procedure AddLog (const S: string) ;
    procedure SetOAParams;
    procedure SetCommParams;
    procedure SetCertParams;
    procedure ResetButtons;
    procedure RefreshDomains;
    procedure ResetDomButtons;
    procedure SetDomButtons;
  end;

const
    SectionMainWindow    = 'MainWindow';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';
    SectionData          = 'Data';
    KeyCertSANGrid       = 'CertSANGrid';

var
  X509CertsForm: TX509CertsForm;

implementation

{$R *.dfm}


procedure TX509CertsForm.FormCreate(Sender: TObject);
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
    CertSANGrid.Cells[0,0] := 'Domain Name';
    CertSANGrid.Cells[1,0] := 'Web Server UNC HTTP .Well-Known';
    CertSANGrid.Cells[2,0] := 'Web Server UNC Public Certs Dir';
    CertSANGrid.Cells[3,0] := 'Approval Email';
    OpenFileDlg.Filter := 'Certs *.pem;*.cer;*.crt;*.der;*.p12;*.pfx;*.p7*;*.spc|' +
                            '*.pem;*.cer;*.crt;*.der;*.p12;*.pfx;*.p7*;*.spc|' +
            'All Files *.*|*.*';
end;

procedure TX509CertsForm.FormDestroy(Sender: TObject);
begin
//
end;

procedure TX509CertsForm.FormShow(Sender: TObject);
var
    IniFile: TIcsIniFile;
    SL, SR: TStringList;
    I, J, K, tot, row: Integer;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIcsIniFile.Create(FIniFileName);
        Width := IniFile.ReadInteger(SectionMainWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionMainWindow, KeyHeight, Height);
        Top := IniFile.ReadInteger(SectionMainWindow, KeyTop, (Screen.Height - Height) div 2);
        Left := IniFile.ReadInteger(SectionMainWindow, KeyLeft, (Screen.Width  - Width)  div 2);
        SL := TStringList.Create;
        SR := TStringList.Create;
        try
             SL.Delimiter := ',';
             SR.Delimiter := '|';
             SL.DelimitedText := IniFile.ReadString(SectionData, KeyCertSANGrid, '');
             tot := SL.Count;
             CertSANGrid.RowCount := tot + 10;
             row := 1;
             for I := 0 to tot - 1 do begin
                SR.DelimitedText := SL[I];
                if SR.Count >= 4 then begin
                    if Length(Trim(SR[0])) > 2 then begin
                        CertSANGrid.Cells[0, row] := SR[0];
                        CertSANGrid.Cells[1, row] := SR[1];
                        CertSANGrid.Cells[2, row] := SR[2];
                        CertSANGrid.Cells[3, row] := SR[3];
                        row := row + 1;
                    end;
                end;
             end;
        finally
             SL.Free;
             SR.Free;
        end;

       with IniFile do begin
  AccAcmeKeyV1.ItemIndex := ReadInteger (SectionData, 'AccAcmeKeyV1_ItemIndex', AccAcmeKeyV1.ItemIndex) ;
  AccAcmeKeyV2.ItemIndex := ReadInteger (SectionData, 'AccAcmeKeyV2_ItemIndex', AccAcmeKeyV2.ItemIndex) ;
  AcmeServerV1.Text := ReadString (SectionData, 'AcmeServerV1_Text', AcmeServerV1.Text) ;
  AcmeServerV2.Text := ReadString (SectionData, 'AcmeServerV2_Text', AcmeServerV2.Text) ;
  if ReadString (SectionData, 'AutoOrderComplete_Checked', 'False') = 'True' then AutoOrderComplete.Checked := true else AutoOrderComplete.Checked := false ;
  CACertFile.Text := ReadString (SectionData, 'CACertFile_Text', CACertFile.Text) ;
  CAPkeyFile.Text := ReadString (SectionData, 'CAPkeyFile_Text', CAPkeyFile.Text) ;
  CAPkeyPw.Text := ReadString (SectionData, 'CAPkeyPw_Text', CAPkeyPw.Text) ;
  CertAddress.Text := ReadString (SectionData, 'CertAddress_Text', CertAddress.Text) ;
  CertCentreApprovEmail.Text := ReadString (SectionData, 'CertCentreApprovEmail_Text', CertCentreApprovEmail.Text) ;
  CertCentreOrderId.Text := ReadString (SectionData, 'CertCentreOrderId_Text', CertCentreOrderId.Text) ;
  CertCentreOrderRef.Text := ReadString (SectionData, 'CertCentreOrderRef_Text', CertCentreOrderRef.Text) ;
  CertCentreProducts.ItemIndex := ReadInteger (SectionData, 'CertCentreProducts_ItemIndex', CertCentreProducts.ItemIndex) ;
  CertCentreProducts.Items.CommaText := ReadString (SectionData, 'CertCentreProducts_Items', '') ;
  CertCentreServer.Text := ReadString (SectionData, 'CertCentreServer_Text', CertCentreServer.Text) ;
  CertCommonName.Text := ReadString (SectionData, 'CertCommonName_Text', CertCommonName.Text) ;
  CertContactEmail.Text := ReadString (SectionData, 'CertContactEmail_Text', CertContactEmail.Text) ;
  CertContactFirst.Text := ReadString (SectionData, 'CertContactFirst_Text', CertContactFirst.Text) ;
  CertContactLast.Text := ReadString (SectionData, 'CertContactLast_Text', CertContactLast.Text) ;
  CertContactTitle.Text := ReadString (SectionData, 'CertContactTitle_Text', CertContactTitle.Text) ;
  CertCountry.Text := ReadString (SectionData, 'CertCountry_Text', CertCountry.Text) ;
  CertCsrOrigin.ItemIndex := ReadInteger (SectionData, 'CertCsrOrigin_ItemIndex', CertCsrOrigin.ItemIndex) ;
  CertLocality.Text := ReadString (SectionData, 'CertLocality_Text', CertLocality.Text) ;
  CertOldCsrFile.Text := ReadString (SectionData, 'CertOldCsrFile_Text', CertOldCsrFile.Text) ;
  CertOldPrvKey.Text := ReadString (SectionData, 'CertOldPrvKey_Text', CertOldPrvKey.Text) ;
  CertOrganization.Text := ReadString (SectionData, 'CertOrganization_Text', CertOrganization.Text) ;
  CertOrganizationalUnit.Text := ReadString (SectionData, 'CertOrganizationalUnit_Text', CertOrganizationalUnit.Text) ;
  if ReadString (SectionData, 'CertOutFmtBudl_Checked', 'False') = 'True' then CertOutFmtBudl.Checked := true else CertOutFmtBudl.Checked := false ;
  if ReadString (SectionData, 'CertOutFmtP12_Checked', 'False') = 'True' then CertOutFmtP12.Checked := true else CertOutFmtP12.Checked := false ;
  if ReadString (SectionData, 'CertOutFmtP7_Checked', 'False') = 'True' then CertOutFmtP7.Checked := true else CertOutFmtP7.Checked := false ;
  if ReadString (SectionData, 'CertOutFmtReq_Checked', 'False') = 'True' then CertOutFmtReq.Checked := true else CertOutFmtReq.Checked := false ;
  if ReadString (SectionData, 'CertOutFmtSep_Checked', 'False') = 'True' then CertOutFmtSep.Checked := true else CertOutFmtSep.Checked := false ;
  CertPhone.Text := ReadString (SectionData, 'CertPhone_Text', CertPhone.Text) ;
  CertPostCode.Text := ReadString (SectionData, 'CertPostCode_Text', CertPostCode.Text) ;
  CertSerNumType.ItemIndex := ReadInteger (SectionData, 'CertSerNumType_ItemIndex', CertSerNumType.ItemIndex) ;
  CertSignDigestType.ItemIndex := ReadInteger (SectionData, 'CertSignDigestType_ItemIndex', CertSignDigestType.ItemIndex) ;
  CertState.Text := ReadString (SectionData, 'CertState_Text', CertState.Text) ;
  CertValidity.Text := ReadString (SectionData, 'CertValidity_Text', CertValidity.Text) ;
  DebugLogging.ItemIndex := ReadInteger (SectionData, 'DebugLogging_ItemIndex', DebugLogging.ItemIndex) ;
  DirAcmeConfV1.Text := ReadString (SectionData, 'DirAcmeConfV1_Text', DirAcmeConfV1.Text) ;
  DirAcmeConfV2.Text := ReadString (SectionData, 'DirAcmeConfV2_Text', DirAcmeConfV2.Text) ;
  DirCertCenConf.Text := ReadString (SectionData, 'DirCertCenConf_Text', DirCertCenConf.Text) ;
  DirDatabase.Items.CommaText := ReadString (SectionData, 'DirDatabase_Items', '') ;
  DirDatabase.Text := ReadString (SectionData, 'DirDatabase_Text', DirDatabase.Text) ;
  DirLogs.Text := ReadString (SectionData, 'DirLogs_Text', DirLogs.Text) ;
  DirPubWebCert.Text := ReadString (SectionData, 'DirPubWebCert_Text', DirPubWebCert.Text) ;
  DirWellKnown.Text := ReadString (SectionData, 'DirWellKnown_Text', DirWellKnown.Text) ;
  DomWebSrvIP.Text := ReadString (SectionData, 'DomWebSrvIP_Text', DomWebSrvIP.Text) ;
  if ReadString (SectionData, 'LogJson_Checked', 'False') = 'True' then LogJson.Checked := true else LogJson.Checked := false ;
  if ReadString (SectionData, 'LogPkeys_Checked', 'False') = 'True' then LogPkeys.Checked := true else LogPkeys.Checked := false ;
  OAuthAccToken.Text := ReadString (SectionData, 'OAuthAccToken_Text', OAuthAccToken.Text) ;
  OAuthAppUrl.Text := ReadString (SectionData, 'OAuthAppUrl_Text', OAuthAppUrl.Text) ;
  if ReadString (SectionData, 'OAuthAutoRefresh_Checked', 'False') = 'True' then OAuthAutoRefresh.Checked := true else OAuthAutoRefresh.Checked := false ;
  OAuthClientId.Text := ReadString (SectionData, 'OAuthClientId_Text', OAuthClientId.Text) ;
  OAuthClientSecret.Text := ReadString (SectionData, 'OAuthClientSecret_Text', OAuthClientSecret.Text) ;
  OAuthExpire.Text := ReadString (SectionData, 'OAuthExpire_Text', OAuthExpire.Text) ;
  OAuthRedirectUrl.Text := ReadString (SectionData, 'OAuthRedirectUrl_Text', OAuthRedirectUrl.Text) ;
  OAuthRefToken.Text := ReadString (SectionData, 'OAuthRefToken_Text', OAuthRefToken.Text) ;
  OAuthRefrMins.Text := ReadString (SectionData, 'OAuthRefrMins_Text', OAuthRefrMins.Text) ;
  OAuthScope.Text := ReadString (SectionData, 'OAuthScope_Text', OAuthScope.Text) ;
  OAuthTokenUrl.Text := ReadString (SectionData, 'OAuthTokenUrl_Text', OAuthTokenUrl.Text) ;
  OAuthWebIP.Text := ReadString (SectionData, 'OAuthWebIP_Text', OAuthWebIP.Text) ;
  OAuthWebPort.Text := ReadString (SectionData, 'OAuthWebPort_Text', OAuthWebPort.Text) ;
  if ReadString (SectionData, 'OldOSL_Checked', 'False') = 'True' then OldOSL.Checked := true else OldOSL.Checked := false ;
  OwnCACertDir.Text := ReadString (SectionData, 'OwnCACertDir_Text', OwnCACertDir.Text) ;
  PrivKeyCipher.ItemIndex := ReadInteger (SectionData, 'PrivKeyCipher_ItemIndex', PrivKeyCipher.ItemIndex) ;
  PrivKeyPassword.Text := ReadString (SectionData, 'PrivKeyPassword_Text', PrivKeyPassword.Text) ;
  PrivKeyType.ItemIndex := ReadInteger (SectionData, 'PrivKeyType_ItemIndex', PrivKeyType.ItemIndex) ;
  SuppCertChallenge.ItemIndex := ReadInteger (SectionData, 'SuppCertChallenge_ItemIndex', SuppCertChallenge.ItemIndex) ;
  SupplierEmail.Text := ReadString (SectionData, 'SupplierEmail_Text', SupplierEmail.Text) ;
       end;
        IniFile.Free;
    end;

    OAuthWebIP.Items.Assign(LocalIPList);
    DomWebSrvIP.Items.Assign(OAuthWebIP.Items);
    OAuthWebIP.Items.Insert(0, ICS_LOCAL_HOST_V4);
    ResetButtons;
    OverbyteIcsWSocket.LoadSsl;

// get API server URLs
    AcmeServerV1.Items.Clear;
    AcmeServerV1.Items.Add(X509Certs1.GetServerAPIUrl(SuppProtoAcmeV1, False));
    AcmeServerV1.Items.Add(X509Certs1.GetServerAPIUrl(SuppProtoAcmeV1, True));
    AcmeServerV2.Items.Clear;
    AcmeServerV2.Items.Add(X509Certs1.GetServerAPIUrl(SuppProtoAcmeV2, False));
    AcmeServerV2.Items.Add(X509Certs1.GetServerAPIUrl(SuppProtoAcmeV2, True));
    CertCentreServer.Items.Clear;
    CertCentreServer.Items.Add(X509Certs1.GetServerAPIUrl(SuppProtoCertCentre, False));
    if AcmeServerV1.Text = '' then AcmeServerV1.ItemIndex := 0;
    if AcmeServerV2.Text = '' then AcmeServerV2.ItemIndex := 0;
    if CertCentreServer.Text = '' then CertCentreServer.ItemIndex := 0;

end;

procedure TX509CertsForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
    temp: String;
    SL: TStringList;
    I: Integer;
begin
    if FLogOpen then FreeAndNil(FLogFStream);
    FLogOpen := False;
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionMainWindow, KeyTop, Top);
    IniFile.WriteInteger(SectionMainWindow, KeyLeft, Left);
    IniFile.WriteInteger(SectionMainWindow, KeyWidth, Width);
    IniFile.WriteInteger(SectionMainWindow, KeyHeight, Height);
    SL := TStringList.Create;
    try
        SL.Delimiter := ',';
        for I := 1 to CertSANGrid.RowCount - 1 do begin
            if Length(Trim(CertSANGrid.Cells[0,I])) > 2 then
                SL.Add(CertSANGrid.Cells[0,I] + '|' + CertSANGrid.Cells[1,I] +
                     '|' + CertSANGrid.Cells[2,I] + '|' + CertSANGrid.Cells[3,I] );
        end;
        IniFile.WriteString(SectionData, KeyCertSANGrid, SL.DelimitedText);
    finally
        SL.Free;
    end;

    with IniFile do begin
  WriteInteger (SectionData, 'AccAcmeKeyV1_ItemIndex', AccAcmeKeyV1.ItemIndex) ;
  WriteInteger (SectionData, 'AccAcmeKeyV2_ItemIndex', AccAcmeKeyV2.ItemIndex) ;
  WriteString (SectionData, 'AcmeServerV1_Text', AcmeServerV1.Text) ;
  WriteString (SectionData, 'AcmeServerV2_Text', AcmeServerV2.Text) ;
  if AutoOrderComplete.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'AutoOrderComplete_Checked', temp) ;
  WriteString (SectionData, 'CACertFile_Text', CACertFile.Text) ;
  WriteString (SectionData, 'CAPkeyFile_Text', CAPkeyFile.Text) ;
  WriteString (SectionData, 'CAPkeyPw_Text', CAPkeyPw.Text) ;
  WriteString (SectionData, 'CertAddress_Text', CertAddress.Text) ;
  WriteString (SectionData, 'CertCentreApprovEmail_Text', CertCentreApprovEmail.Text) ;
  WriteString (SectionData, 'CertCentreOrderId_Text', CertCentreOrderId.Text) ;
  WriteString (SectionData, 'CertCentreOrderRef_Text', CertCentreOrderRef.Text) ;
  WriteInteger (SectionData, 'CertCentreProducts_ItemIndex', CertCentreProducts.ItemIndex) ;
  WriteString (SectionData, 'CertCentreProducts_Items', CertCentreProducts.Items.CommaText) ;
  WriteString (SectionData, 'CertCentreServer_Text', CertCentreServer.Text) ;
  WriteString (SectionData, 'CertCommonName_Text', CertCommonName.Text) ;
  WriteString (SectionData, 'CertContactEmail_Text', CertContactEmail.Text) ;
  WriteString (SectionData, 'CertContactFirst_Text', CertContactFirst.Text) ;
  WriteString (SectionData, 'CertContactLast_Text', CertContactLast.Text) ;
  WriteString (SectionData, 'CertContactTitle_Text', CertContactTitle.Text) ;
  WriteString (SectionData, 'CertCountry_Text', CertCountry.Text) ;
  WriteInteger (SectionData, 'CertCsrOrigin_ItemIndex', CertCsrOrigin.ItemIndex) ;
  WriteString (SectionData, 'CertLocality_Text', CertLocality.Text) ;
  WriteString (SectionData, 'CertOldCsrFile_Text', CertOldCsrFile.Text) ;
  WriteString (SectionData, 'CertOldPrvKey_Text', CertOldPrvKey.Text) ;
  WriteString (SectionData, 'CertOrganization_Text', CertOrganization.Text) ;
  WriteString (SectionData, 'CertOrganizationalUnit_Text', CertOrganizationalUnit.Text) ;
  if CertOutFmtBudl.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertOutFmtBudl_Checked', temp) ;
  if CertOutFmtP12.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertOutFmtP12_Checked', temp) ;
  if CertOutFmtP7.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertOutFmtP7_Checked', temp) ;
  if CertOutFmtReq.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertOutFmtReq_Checked', temp) ;
  if CertOutFmtSep.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertOutFmtSep_Checked', temp) ;
  WriteString (SectionData, 'CertPhone_Text', CertPhone.Text) ;
  WriteString (SectionData, 'CertPostCode_Text', CertPostCode.Text) ;
  WriteInteger (SectionData, 'CertSerNumType_ItemIndex', CertSerNumType.ItemIndex) ;
  WriteInteger (SectionData, 'CertSignDigestType_ItemIndex', CertSignDigestType.ItemIndex) ;
  WriteString (SectionData, 'CertState_Text', CertState.Text) ;
  WriteString (SectionData, 'CertValidity_Text', CertValidity.Text) ;
  WriteInteger (SectionData, 'DebugLogging_ItemIndex', DebugLogging.ItemIndex) ;
  WriteString (SectionData, 'DirAcmeConfV1_Text', DirAcmeConfV1.Text) ;
  WriteString (SectionData, 'DirAcmeConfV2_Text', DirAcmeConfV2.Text) ;
  WriteString (SectionData, 'DirCertCenConf_Text', DirCertCenConf.Text) ;
  WriteString (SectionData, 'DirDatabase_Items', DirDatabase.Items.CommaText) ;
  WriteString (SectionData, 'DirDatabase_Text', DirDatabase.Text) ;
  WriteString (SectionData, 'DirLogs_Text', DirLogs.Text) ;
  WriteString (SectionData, 'DirPubWebCert_Text', DirPubWebCert.Text) ;
  WriteString (SectionData, 'DirWellKnown_Text', DirWellKnown.Text) ;
  WriteString (SectionData, 'DomWebSrvIP_Text', DomWebSrvIP.Text) ;
  if LogJson.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'LogJson_Checked', temp) ;
  if LogPkeys.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'LogPkeys_Checked', temp) ;
  WriteString (SectionData, 'OAuthAccToken_Text', OAuthAccToken.Text) ;
  WriteString (SectionData, 'OAuthAppUrl_Text', OAuthAppUrl.Text) ;
  if OAuthAutoRefresh.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'OAuthAutoRefresh_Checked', temp) ;
  WriteString (SectionData, 'OAuthClientId_Text', OAuthClientId.Text) ;
  WriteString (SectionData, 'OAuthClientSecret_Text', OAuthClientSecret.Text) ;
  WriteString (SectionData, 'OAuthExpire_Text', OAuthExpire.Text) ;
  WriteString (SectionData, 'OAuthRedirectUrl_Text', OAuthRedirectUrl.Text) ;
  WriteString (SectionData, 'OAuthRefToken_Text', OAuthRefToken.Text) ;
  WriteString (SectionData, 'OAuthRefrMins_Text', OAuthRefrMins.Text) ;
  WriteString (SectionData, 'OAuthScope_Text', OAuthScope.Text) ;
  WriteString (SectionData, 'OAuthTokenUrl_Text', OAuthTokenUrl.Text) ;
  WriteString (SectionData, 'OAuthWebIP_Text', OAuthWebIP.Text) ;
  WriteString (SectionData, 'OAuthWebPort_Text', OAuthWebPort.Text) ;
  if OldOSL.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'OldOSL_Checked', temp) ;
  WriteString (SectionData, 'OwnCACertDir_Text', OwnCACertDir.Text) ;
  WriteInteger (SectionData, 'PrivKeyCipher_ItemIndex', PrivKeyCipher.ItemIndex) ;
  WriteString (SectionData, 'PrivKeyPassword_Text', PrivKeyPassword.Text) ;
  WriteInteger (SectionData, 'PrivKeyType_ItemIndex', PrivKeyType.ItemIndex) ;
  WriteInteger (SectionData, 'SuppCertChallenge_ItemIndex', SuppCertChallenge.ItemIndex) ;
  WriteString (SectionData, 'SupplierEmail_Text', SupplierEmail.Text) ;
    end;
    IniFile.UpdateFile;
    IniFile.Free;
end;

procedure TX509CertsForm.AddLog(const S: string);
var
    S2: String;
begin
    if Pos (IcsLF,S) > 0 then
        LogWin.Lines.Text := LogWin.Lines.Text + IcsCRLF + S
    else
       LogWin.Lines.Add (S) ;
    SendMessage(LogWin.Handle, EM_LINESCROLL, 0, 999999);

    try
        if (DirLogs.Text = '') then Exit ;
        if NOT FLogOpen then
           FLogFStream := TFileStream.Create (IncludeTrailingPathDelimiter(DirLogs.Text) +
                    'ics-certlog-' + FormatDateTime (DateMaskPacked, Now) + '.log', fmCreate) ;
        FLogOpen := true ;
        S2 := S + IcsCRLF ;
        FLogFStream.WriteBuffer (S2 [1], Length (S2)) ;
    except
    end;
end;

procedure TX509CertsForm.X509Certs1CertProg(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    AddLog(Msg);
end;

procedure TX509CertsForm.X509Certs1ChallengeDNS(Sender: TObject;
  ChallengeItem: TChallengeItem);
var
    secswait: integer;
    Trg: LongWord;
    S1, S2: String;

begin
     S1 := 'Add DNS ' + ChallengeItem.CAuthzURL + ' Record for: ' +
        ChallengeItem.CPage + ', with: ' + IcsCRLF + ChallengeItem.CDNSValue;
     S2 := 'Waiting up to five minutes for DNS Server to be manually updated with challenge, Tick box when done';
    if ChallengeItem.CSupplierProto = SuppProtoAcmeV2 then begin
    // !!! must now wait until DNS server is updated, or challenge will fail
    // might be able to use WMI to do this automatically???
        AddLog(S1);
        AddLog(S2);
        Acme2DnsUpdated.Checked := False;
        Acme2DnsUpdated.Visible := True;
        secswait := 300;  // 5 minutes
        while NOT Acme2DnsUpdated.Checked do begin
            LabelAcme2Info.Caption := 'DNS Challenge: ' + IcsCRLF + S1 +
              IcsCRLF + 'Challenge Will Continue in ' + IntToStr(secswait) + ' seconds';
            Trg := IcsGetTrgSecs(5); // five second wait
            while True do begin
                Application.ProcessMessages;
                if Application.Terminated then Exit;
                if IcsTestTrgTick(Trg) then break ;
                Sleep(0);   // thread now stops for rest of time slice
            end ;
            secswait := secswait - 5;
            if secswait <= 0 then break;
        end;
        LabelAcme2Info.Caption := '';
        Acme2DnsUpdated.Visible := False;
    end;
    if ChallengeItem.CSupplierProto = SuppProtoCertCentre then begin
    // !!! must now wait until DNS server is updated, or challenge will fail
    // might be able to use WMI to do this automatically???
        AddLog(S1);
        AddLog(S2);
        CCDnsUpdated.Checked := False;
        CCDnsUpdated.Visible := True;
        secswait := 300;  // 5 minutes
        while NOT CCDnsUpdated.Checked do begin
            LabelCertInfo.Caption := 'DNS Challenge: ' + IcsCRLF + S1 +
              IcsCRLF + 'Challenge Will Continue in ' + IntToStr(secswait) + ' seconds';
            Trg := IcsGetTrgSecs(5); // five second wait
            while True do begin
                Application.ProcessMessages;
                if Application.Terminated then Exit;
                if IcsTestTrgTick(Trg) then break ;
                Sleep(0);   // thread now stops for rest of time slice
            end ;
            secswait := secswait - 5;
            if secswait <= 0 then break;
        end;
        CCDnsUpdated.Visible := False;
        LabelCertInfo.Caption := '';
    end;
end;

procedure TX509CertsForm.X509Certs1ChallengeEmail(Sender: TObject;
  ChallengeItem: TChallengeItem);
begin
    LabelCertInfo.Caption := 'Email Challenge: ' + icsCRLF +
        'Please collect order once email challenge has been completed for ' +
                                                         ChallengeItem.CDomain;
end;

procedure TX509CertsForm.X509Certs1ChallengeFTP(Sender: TObject;
  ChallengeItem: TChallengeItem);
var
    S: String;
begin
    S := '.Well-Known Challenge: ' + icsCRLF +
        'Use FTP to Copy file: ' + ChallengeItem.CWKFullName +
            ' to server .Well-Known directory for domain: ' + ChallengeItem.CDomain;
    if ChallengeItem.CSupplierProto = SuppProtoAcmeV2 then
        LabelAcme2Info.Caption := S;
    if ChallengeItem.CSupplierProto = SuppProtoCertCentre then
        LabelCertInfo.Caption := S;
    // need some FTP code here
    // !!! must now wait until server is updated, or challenge will fail
end;

procedure TX509CertsForm.X509Certs1ChallgRefresh(Sender: TObject);
begin
//
end;

procedure TX509CertsForm.ResetDomButtons;
begin
    doDBCheck.Enabled := False;
    doDBOrder.Enabled := False;
    doDBCollect.Enabled := False;
    doDBRevoke.Enabled := False;
    doDBCancel.Enabled := False;
    doDBRemove.Enabled := False;
    doDBRedist.Enabled := False;
    LabelInfoDomain.Caption := 'Order Information:';
end;

function MyDateToStr(DT: TDateTime): String;
begin
    if DT < 10 then
        Result := ''
    else
        Result := DateToStr(DT);
end;

function MyDateTimeToStr(DT: TDateTime): String;
begin
    if DT < 10 then
        Result := ''
    else
        Result := DateTimeToStr(DT);
end;

procedure TX509CertsForm.SetDomButtons;
var
    SelNr: Integer;
begin
    SelNr := DatabaseDomains.ItemIndex;
    ResetDomButtons;
    if SelNr < 0 then Exit;
    if SelNr > Length(X509Certs1.DomainItems) then Exit;
    with X509Certs1.DomainItems[SelNr] do begin
        doDBCheck.Enabled := True;
        doDBRemove.Enabled := True;
        if DIssueState >= IssStateChecked then doDBOrder.Enabled := True;
        if DIssueState >= IssStateChallgPend then begin
            doDBCollect.Enabled := True;
            doDBCancel.Enabled := True;
            LabelInfoDomain.Caption := 'Certificate Common Name Domain: ' + DCommonName + IcsCRLF +
              'Alternate Names: ' + DCertSANs + IcsCRLF +
              'Challenge Type: ' + ChallengeTypeLits[DSuppCertChallg] + IcsCRLF +
              'Product: ' + DProduct + IcsCRLF +
              'Issue State: ' + IssueStateLits[DIssueState] + IcsCRLF +
              'Issued: ' + MyDateTimeToStr(DStartDT) + IcsCRLF +
              'Expires: ' + MyDateTimeToStr(DEndDT) + IcsCRLF;
        end;
        if DIssueState >= IssStateCollect then begin
            doDBRevoke.Enabled := True;
            doDBRedist.Enabled := True;
        end;
    end;
end;

procedure TX509CertsForm.RefreshDomains;
var
    I, Tot: integer;
    OldDom: String;
begin
    Tot := Length(X509Certs1.DomainItems);
    if (Tot = 0) or (DatabaseDomains.Items.Count > Tot) then begin
        DatabaseDomains.Items.Clear;
        ResetDomButtons;
    end
    else begin
        if PageControl1.ActivePage = TabDatabase then
            DatabaseDomains.SetFocus;
        if DatabaseDomains.ItemIndex >= 0 then
            OldDom := DatabaseDomains.Items[DatabaseDomains.ItemIndex].Caption
        else
            OldDom := '';
        while (DatabaseDomains.Items.Count < Tot) do begin
            DatabaseDomains.Items.Add;
            ResetDomButtons;
        end;
        for I := 0 to Tot - 1 do begin
            with X509Certs1.DomainItems[I] do begin
                DatabaseDomains.Items[I].Caption := DCommonName;
                while (DatabaseDomains.Items[I].SubItems.Count) < 7 do
                    DatabaseDomains.Items[I].SubItems.Add('');
                DatabaseDomains.Items[I].SubItems[0] := IssueStateLits[DIssueState];
                DatabaseDomains.Items[I].SubItems[1] := DSuppOrderId;
                DatabaseDomains.Items[I].SubItems[2] := MyDateToStr(DStartDT);
                DatabaseDomains.Items[I].SubItems[3] := MyDateToStr(DEndDT);
                DatabaseDomains.Items[I].SubItems[4] := ChallengeTypeLits[DSuppCertChallg];
                DatabaseDomains.Items[I].SubItems[5] := DProduct;
                DatabaseDomains.Items[I].SubItems[6] := DCertSANs;
                if OldDom = DCommonName then begin
                    DatabaseDomains.Items[I].Selected := True;
                    DatabaseDomains.Items[I].Focused := True;
                end;
            end;
        end;
        SetDomButtons;
    end;
end;

procedure TX509CertsForm.X509Certs1DomainsRefresh(Sender: TObject);
begin
    RefreshDomains;
end;

procedure TX509CertsForm.X509Certs1SuppDBRefresh(Sender: TObject);
var
    S: String;
begin
    if X509Certs1.SupplierProto > SuppProtoNone then begin
        if DirDatabase.Items.IndexOf(DirDatabase.Text) < 0 then
                           DirDatabase.Items.Add(DirDatabase.Text);
        S := 'Account Database Supplier: ' + X509Certs1.SupplierTitle + IcsCRLF +
          'Supplier Protocol: ' + SupplierProtoLits [X509Certs1.SupplierProto] + IcsCRLF;
        LabelDB.Caption := S;
    end
    else begin
        LabelDB.Caption := 'No Supplier Account Database Open';
        DatabaseDomains.Items.Clear;
    end;
end;

procedure TX509CertsForm.X509Certs1OAuthAuthUrl(Sender: TObject;
  const URL: string);
begin
    { comes here for OAAuthType=OAuthTypeMan generally for windows services,
     we need to ask user to launch the URL, either by sending an email or by
     sending a message to an GUI to launch the browser }
end;

procedure TX509CertsForm.X509Certs1NewCert(Sender: TObject);
begin
    with Sender as TSslX509Certs do begin
        if SupplierProto = SuppProtoAcmeV1 then
            LabelAcme1Cert.Caption := X509Certs1.GetOrderResult;
        if SupplierProto = SuppProtoAcmeV2 then
            LabelAcme2Info.Caption :=  X509Certs1.GetOrderResult;
        if SupplierProto = SuppProtoCertCentre then
            LabelCertInfo.Caption :=  X509Certs1.GetOrderResult;
    end;
end;

procedure TX509CertsForm.X509Certs1NewToken(Sender: TObject);
begin
    OAuthAccToken.Text := X509Certs1.OAAccToken;
    OAuthRefToken.Text := X509Certs1.OARefreshToken;
    if X509Certs1.OAExpireDT > 10 then
        OAuthExpire.Text := RFC3339_DateToStr(X509Certs1.OAExpireDT)
    else
        OAuthExpire.Text := '';

  // see if repeating CC Profile command, only once
    if FPendCCProfile and (X509Certs1.OAAccToken <> '') then begin
        FPendCCProfile := False;
        doCCProfileClick(Self);
        FPendCCProfile := False;
    end;
end;

procedure TX509CertsForm.ResetButtons;
begin
    doAcmeCheckOrderV1.Enabled := False;
    doAcmeCheckOrderV2.Enabled := False;
    doAcmeGetCertV1.Enabled := False;
    doAcmeGetCertV2.Enabled := False;
    doAcmeOrderV1.Enabled := False;
    doAcmeOrderV2.Enabled := False;
    doCertCentreAlways.Enabled := False;
    doCertCentreCollect.Enabled := False;
    doCertCentreOrders.Enabled := False;
    doCertCentreCheck.Enabled := False;
end;

procedure TX509CertsForm.SetCommParams;
begin
    X509Certs1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    X509Certs1.LogJson := LogJson.Checked;
    X509Certs1.LogPkeys := LogPkeys.Checked;
    X509Certs1.DomWebSrvIP := DomWebSrvIP.Text;
    X509Certs1.AutoOrderComplete := AutoOrderComplete.Checked;
end;

procedure TX509CertsForm.SetOAParams;
begin
    X509Certs1.OAAccToken := OAuthAccToken.Text;
    X509Certs1.OAAppUrl := OAuthAppUrl.Text;
  // authenication handled internally by launching browser, only works for
  // GUIs, use OAuthTypeMan for services with OAuth1OAuthAuthUrl event
    X509Certs1.OAAuthType := OAuthTypeWeb;
    X509Certs1.OAClientId := OAuthClientId.Text;
    X509Certs1.OAClientSecret := OAuthClientSecret.Text;
    X509Certs1.OAExpireDT := RFC3339_StrToDate(OAuthExpire.Text);
    X509Certs1.OARedirectUrl := OAuthRedirectUrl.Text;
    X509Certs1.OARefrMinsPrior := atoi(OAuthRefrMins.Text);
    X509Certs1.OARefreshAuto := OAuthAutoRefresh.Checked;
    X509Certs1.OARefreshToken := OAuthRefToken.Text; ;
    X509Certs1.OAScope := OAuthScope.Text;
    X509Certs1.OATokenUrl := OAuthTokenUrl.Text;
    X509Certs1.OAWebSrvIP := OAuthWebIP.Text;
    X509Certs1.OAWebSrvPort := OAuthWebPort.Text;
end;


procedure TX509CertsForm.SelCertOldCsrFileClick(Sender: TObject);
var
    FName: String;
begin
    OpenFileDlg.InitialDir := CertOldCsrFile.Text;
    if OpenFileDlg.Execute then begin
        FName := OpenFileDlg.FileName;
        if (FName = '') or (not FileExists(FName)) then Exit;
        CertOldCsrFile.Text := FName;
    end;
end;

procedure TX509CertsForm.SelCertOldPrvKeyClick(Sender: TObject);
var
    FName: String;
begin
    OpenFileDlg.InitialDir := CertOldPrvKey.Text;
    if OpenFileDlg.Execute then begin
        FName := OpenFileDlg.FileName;
        if (FName = '') or (not FileExists(FName)) then Exit;
        CertOldPrvKey.Text := FName;
    end;
end;

procedure TX509CertsForm.SelDirDatabaseClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirDatabase.Text ;
    if OpenDirDiag.Execute then
        DirDatabase.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelDirAcmeConfV1Click(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirAcmeConfV1.Text ;
    if OpenDirDiag.Execute then
        DirAcmeConfV1.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelDirAcmeConfV2Click(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirAcmeConfV2.Text ;
    if OpenDirDiag.Execute then
        DirAcmeConfV2.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelDirCertCenConfClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirCertCenConf.Text ;
    if OpenDirDiag.Execute then
        DirCertCenConf.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelDirLogsClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirLogs.Text ;
    if OpenDirDiag.Execute then
        DirLogs.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelDirPubWebCertClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirPubWebCert.Text ;
    if OpenDirDiag.Execute then
        DirPubWebCert.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelDirWellKnownClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirWellKnown.Text ;
    if OpenDirDiag.Execute then
        DirWellKnown.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelOwnCACertDirClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := OwnCACertDir.Text ;
    if OpenDirDiag.Execute then
        OwnCACertDir.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelCACertFileClick(Sender: TObject);
var
    FName: String;
begin
    OpenFileDlg.InitialDir := CACertFile.Text;
    if OpenFileDlg.Execute then begin
        FName := OpenFileDlg.FileName;
        if (FName = '') or (not FileExists(FName)) then Exit;
        CACertFile.Text := FName;
    end;
end;

procedure TX509CertsForm.SelCAPkeyFileClick(Sender: TObject);
var
    FName: String;
begin
    OpenFileDlg.InitialDir := CAPkeyFile.Text;
    if OpenFileDlg.Execute then begin
        FName := OpenFileDlg.FileName;
        if (FName = '') or (not FileExists(FName)) then Exit;
        CAPkeyFile.Text := FName;
    end;
end;

procedure TX509CertsForm.SetCertParams;
var
    I, CommRow, NewRow: Integer;
begin
    SetCommParams;
    X509Certs1.CertCsrOrigin := TCertCsrOrigin(CertCsrOrigin.ItemIndex);
    CertCommonName.Text := IcsLowerCase(Trim(CertCommonName.Text));
    X509Certs1.CertCommonName := '';  // set later, may come from CSR
    X509Certs1.DirWellKnown := DirWellKnown.Text;
    X509Certs1.DirPubWebCert.Text := DirPubWebCert.Text;
    X509Certs1.PrivKeyPassword := PrivKeyPassword.Text;
    X509Certs1.PrivKeyType := TSslPrivKeyType (PrivKeyType.ItemIndex);
    X509Certs1.CertSignDigestType := digestlist[CertSignDigestType.ItemIndex];
    X509Certs1.SuppCertChallenge := TChallengeType(SuppCertChallenge.ItemIndex);
    X509Certs1.CertOldCsrFile := CertOldCsrFile.Text;
    X509Certs1.CertOldPrvKey := CertOldPrvKey.Text;
    X509Certs1.CertApprovEmail := CertCentreApprovEmail.Text;
    X509Certs1.CertSerNumType := TSerNumType(CertSerNumType.ItemIndex);
    X509Certs1.SuppOrderRef := '';
    X509Certs1.SuppOrderId := '';

    X509Certs1.CertOutFmts := [];
  // must have one sensible certificate output format
    if (NOT CertOutFmtBudl.Checked) and (NOT CertOutFmtSep.Checked) and
                     (NOT CertOutFmtP12.Checked) then CertOutFmtSep.Checked := True;
    if CertOutFmtBudl.Checked then X509Certs1.CertOutFmts := X509Certs1.CertOutFmts + [OutFmtBudl];
    if CertOutFmtP12.Checked then X509Certs1.CertOutFmts := X509Certs1.CertOutFmts + [OutFmtP12];
    if CertOutFmtP7.Checked then X509Certs1.CertOutFmts := X509Certs1.CertOutFmts + [OutFmtP7];
    if CertOutFmtReq.Checked then X509Certs1.CertOutFmts := X509Certs1.CertOutFmts + [OutFmtReq];
    if CertOutFmtSep.Checked then X509Certs1.CertOutFmts := X509Certs1.CertOutFmts + [OutFmtSep];

 // see if creating own CSR from properties
    if X509Certs1.CertCsrOrigin = CsrOriginProps then begin
        X509Certs1.CertCommonName := CertCommonName.Text;
        X509Certs1.CertOrganization := CertOrganization.Text;
        X509Certs1.CertOrgUnit := CertOrganizationalUnit.Text;
        X509Certs1.SuppOrderId := '';
        X509Certs1.SuppOrderRef := '';
        X509Certs1.CertAddress := CertAddress.Text;
        X509Certs1.CertLocality := CertLocality.Text;
        X509Certs1.CertState := CertState.Text;
        X509Certs1.CertPostCode := CertPostCode.Text;
        X509Certs1.CertCountry := CertCountry.Text;
    //    X509Certs1.CertDescr := CertDescr.Text;
        X509Certs1.CertContactEmail := CertContactEmail.Text;
        X509Certs1.CertContactTitle := CertContactTitle.Text;
        X509Certs1.CertContactFirst := CertContactFirst.Text;
        X509Certs1.CertContactLast := CertContactLast.Text;
        X509Certs1.CertPhone := CertPhone.Text;
        X509Certs1.CertValidity := atoi(CertValidity.Text);

    // build list of SANs from grid
        X509Certs1.CertSubAltNames.Clear;
        CommRow := -1;
        NewRow := -1;
        for I := 1 to CertSANGrid.RowCount - 1 do begin
            CertSANGrid.Cells[0,I] := IcsLowercase(Trim(CertSANGrid.Cells[0,I]));
            if (CertSANGrid.Cells[0,I] <> '') then begin
                if Trim(CertSANGrid.Cells[1,I]) = '' then
                    CertSANGrid.Cells[1,I] := X509Certs1.DirWellKnown;
            end;
            if (CertSANGrid.Cells[0,I] = CertCommonName.Text) then CommRow := I;
            if (CertSANGrid.Cells[0,I] = '') and (NewRow < 0) then NewRow := I;  // first blank row
        end;
    // add common name to SANs if missing, ensure correct workdir
        if (CommRow = -1) and (NewRow >= 1) then begin
            CertSANGrid.Cells[0,NewRow] := CertCommonName.Text;
            CertSANGrid.Cells[1,NewRow] := X509Certs1.DirWellKnown;
            CertSANGrid.Cells[2,NewRow] := X509Certs1.DirPubWebCert[0];
            CertSANGrid.Cells[3,NewRow] := X509Certs1.CertApprovEmail;
        end
        else begin
            if CertSANGrid.Cells[1,CommRow] <> X509Certs1.DirWellKnown then
                        CertSANGrid.Cells[1,CommRow] := X509Certs1.DirWellKnown;
        end;
        for I := 1 to CertSANGrid.RowCount - 1 do begin
            if (CertSANGrid.Cells[0,I] <> '') then begin
                X509Certs1.CertSubAltNames.AddItem(CertSANGrid.Cells[0,I],
                   CertSANGrid.Cells[1,I], CertSANGrid.Cells[2,I], CertSANGrid.Cells[3,I]);
            end;
        end;
    end
    else begin
        if (CertOldCsrFile.Text = '') OR (NOT FileExists(CertOldCsrFile.Text)) then
            AddLog('Must Specify CSR File Name');
    end;
end;


procedure TX509CertsForm.doAcmeAccV1Click(Sender: TObject);
begin
    ResetButtons;
    X509Certs1.CloseAccount;
    SetCommParams;
    X509Certs1.SupplierTitle := 'ACME V1 by Let''s Encrypt';
    X509Certs1.SupplierProto := SuppProtoAcmeV1;
    X509Certs1.SupplierServer := trim(AcmeServerV1.Text);
    X509Certs1.DirCertWork := IncludeTrailingPathDelimiter (DirAcmeConfV1.Text) ;
    X509Certs1.AcmeAccKeyType := TSslPrivKeyType(AccAcmeKeyV1.ItemIndex);
    X509Certs1.SupplierEmail := SupplierEmail.Text;
    if X509Certs1.SetAcmeAccount(True) then begin
        doAcmeCheckOrderV1.Enabled := True;
    end;
    X509Certs1SuppDBRefresh(Self);
end;

procedure TX509CertsForm.doAcmeAccV2Click(Sender: TObject);
begin
    ResetButtons;
    X509Certs1.CloseAccount;
    SetCommParams;
    X509Certs1.SupplierTitle := 'ACME V2 by Let''s Encrypt';
    X509Certs1.SupplierProto := SuppProtoAcmeV2;
    X509Certs1.SupplierServer := trim(AcmeServerV2.Text);
    X509Certs1.DirCertWork := IncludeTrailingPathDelimiter (DirAcmeConfV2.Text) ;
    X509Certs1.AcmeAccKeyType := TSslPrivKeyType (AccAcmeKeyV2.ItemIndex);
    X509Certs1.SupplierEmail := SupplierEmail.Text;
    if X509Certs1.SetAcmeAccount(True) then begin
        doAcmeCheckOrderV2.Enabled := True;
    end;
    X509Certs1SuppDBRefresh(Self);
 end;

procedure TX509CertsForm.doAcmeCheckOrderV1Click(Sender: TObject);
begin
    if X509Certs1.SupplierProto <> SuppProtoAcmeV1 then begin
        AddLog('Must Register ACME Account First');
        Exit;
    end;
    SetCertParams;
    if NOT X509Certs1.AcmeCheckOrder(True, True) then exit;
    LabelAcme1Cert.Caption := 'Let''s Encrypt three month SSL certificate' +
                       icsCRLF + 'Domain(s): ' + X509Certs1.CertSANs.CommaText;
    doAcmeOrderV1.Enabled := true;
    if X509Certs1.IssueState >= IssStateChallgPend then doAcmeGetCertV1.Enabled := true;
end;

procedure TX509CertsForm.doAcmeCheckOrderV2Click(Sender: TObject);
begin
    if X509Certs1.SupplierProto <> SuppProtoAcmeV2 then begin
        AddLog('Must Register ACME Account First');
        Exit;
    end;
    SetCertParams;
    if NOT X509Certs1.AcmeCheckOrder(True, True) then exit;
    LabelAcme2Info.Caption := 'Let''s Encrypt three month SSL certificate' +
                       icsCRLF + 'Domain(s): ' + X509Certs1.CertSANs.CommaText;
    doAcmeOrderV2.Enabled := true;
    if X509Certs1.IssueState >= IssStateChallgPend then doAcmeGetCertV2.Enabled := true;
end;

procedure TX509CertsForm.doAcmeOrderV1Click(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateChecked then begin
        AddLog('Must Check New Certificate First');
        Exit;
    end;
    if NOT X509Certs1.AcmeV1OrderCert then Exit;
    if X509Certs1.IssueState >= IssStateChallgPend then doAcmeGetCertV1.Enabled := true;
    if X509Certs1.IssueState = IssStateChallgOK then X509Certs1.AcmeV1GetCert;
end;

procedure TX509CertsForm.doAcmeOrderV2Click(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateChecked then begin
        AddLog('Must Check New Certificate First');
        Exit;
    end;
    if NOT X509Certs1.AcmeV2OrderCert then Exit;
    if X509Certs1.IssueState >= IssStateChallgPend then doAcmeGetCertV2.Enabled := true;
    if X509Certs1.IssueState = IssStateChallgOK then X509Certs1.AcmeV2GetCert;
end;

procedure TX509CertsForm.doAcmeRevokeV2Click(Sender: TObject);
begin
//
end;

procedure TX509CertsForm.doAcmeSaveOrderV2Click(Sender: TObject);
begin
    if X509Certs1.SupplierProto <> SuppProtoAcmeV2 then begin
        AddLog('Must Register ACME Account First');
        Exit;
    end;
    SetCertParams;
    if NOT X509Certs1.AcmeCheckOrder(False, True) then exit;
    AddLog('Saved Certificate Order');
end;

procedure TX509CertsForm.doAcmeGetCertV1Click(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateChallgPend then begin
        AddLog('Acme Challenge Not Started, Must Order First');
        Exit;
    end;
    X509Certs1.AcmeV1GetCert;
end;

procedure TX509CertsForm.doAcmeGetCertV2Click(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateChallgPend then begin
         AddLog('Acme Challenge Not Started, Must Order First');
        Exit;
    end;
    X509Certs1.AcmeV2GetCert;
end;

procedure TX509CertsForm.doCASignCertClick(Sender: TObject);
begin
    SetCommParams;
    SetCertParams;
    X509Certs1.SupplierProto := SuppProtoOwnCA;
    X509Certs1.DirCertWork := IncludeTrailingPathDelimiter(OwnCACertDir.Text);
    X509Certs1.OwnCASign;
end;

procedure TX509CertsForm.doCCProfileClick(Sender: TObject);
begin
    ResetButtons;
    X509Certs1.CloseAccount;
    SetCommParams;
    SetOAParams;
    X509Certs1.SupplierTitle := 'CertCenter AG';
    X509Certs1.SupplierProto := SuppProtoCertCentre;
    X509Certs1.SupplierServer := trim(CertCentreServer.Text);
    X509Certs1.DirCertWork := IncludeTrailingPathDelimiter (DirCertCenConf.Text) ;

    X509Certs1.SetCertCentre(True);
    if X509Certs1.CCGetProfile then begin
        doCertCentreCheck.Enabled := True;
        doCertCentreAlways.Enabled := True;
        doCertCentreOrders.Enabled := True;
        doCertCentreCollect.Enabled := True;
        if (CertCentreProducts.Items.Count <> X509Certs1.ProductList.Count) then
            CertCentreProducts.Items.Assign(X509Certs1.ProductList);
        X509Certs1SuppDBRefresh(Self);
    end
    else
        FPendCCProfile := True;  // waiting for oAuth2 response
end;

procedure TX509CertsForm.CertCentreProductsClick(Sender: TObject);
begin
    if (X509Certs1.SupplierProto <> SuppProtoCertCentre) then Exit;
    if CertCentreProducts.ItemIndex < 0 then Exit;
    if X509Certs1.CCGetOneProduct(CertCentreProducts.Items[CertCentreProducts.ItemIndex]) then begin
        LabelCertInfo.Caption := X509Certs1.ProductInfo;
        AddLog(X509Certs1.ProductInfo);
    end;
end;


procedure TX509CertsForm.doCertCentreCheckClick(Sender: TObject);
begin
    if X509Certs1.SupplierProto <> SuppProtoCertCentre then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if CertCentreProducts.ItemIndex < 0 then begin
        AddLog('Must Select a Certificate Product First');
        Exit;
    end;
    SetCertParams;
    CertCentreOrderId.Text := '';
    X509Certs1.SuppOrderId := '';
    X509Certs1.SuppOrderRef := CertCentreOrderRef.Text;
    CertCentreProductsClick(Self) ;
    X509Certs1.SuppCertProduct := CertCentreProducts.Items[CertCentreProducts.ItemIndex];
    if X509Certs1.CCCheckOrder(True, True) then begin
        LabelCertInfo.Caption := LabelCertInfo.Caption + IcsCRLF +
            'Domain: ' + X509Certs1.CertCommonName + ', quote price ' + X509Certs1.ProductQuote;
        if Pos ('AlwaysOnSSL', X509Certs1.SuppCertProduct) = 0 then begin
            CertBuy.Enabled := true;
            doCertCentreOther.Enabled := true;
            if X509Certs1.CCGetApproverEmail then begin
                if (CertCentreApprovEmail.Items.Text <> X509Certs1.ApproverEmails.Text) then
                    CertCentreApprovEmail.Items.Assign(X509Certs1.ApproverEmails);
                CertCentreApprovEmail.ItemIndex := 0;
            end;
            if X509Certs1.SuppCertChallenge = ChallEmail then begin
                CertCentreApprovEmail.ItemIndex := -1;
                CertCentreApprovEmail.Text := '';
                AddLog('Must Select a Approver Email Address');
            end;
        end;
    end;
end;

procedure TX509CertsForm.doCertCentreAlwaysClick(Sender: TObject);
begin
    if X509Certs1.SupplierProto <> SuppProtoCertCentre then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    SetCertParams;
    X509Certs1.SuppOrderId := '';
    CertCentreOrderId.Text := '';
    X509Certs1.SuppOrderRef := CertCentreOrderRef.Text;
    if X509Certs1.SuppCertChallenge = ChallEmail then begin
        AddLog('Email challenge not available');
        exit;
    end;
    X509Certs1.SuppCertProduct := 'AlwaysOnSSL.AlwaysOnSSL';
    if X509Certs1.CCOrderCert then begin
        CertCentreOrderId.Text := X509Certs1.SuppOrderId;
    end;
end;

procedure TX509CertsForm.doCertCentreOtherClick(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateChecked then begin
        AddLog('Must Check New Certificate First');
        Exit;
    end;
    if CertBuy.Text <> 'BUY' then begin
        AddLog('!! Must type BUY before ordering commercial certificates that cost money') ;
        exit ;
    end;
    SetCertParams;
    if (CertCentreApprovEmail.Text = '') then begin
         AddLog('Must Select a Approver Email Address First');
         Exit;
    end;
    X509Certs1.SuppOrderId := '';
    CertCentreOrderId.Text := '';
    X509Certs1.SuppOrderRef := CertCentreOrderRef.Text;
    CertBuy.Text := '';
    if X509Certs1.CCOrderCert then begin
        CertCentreOrderId.Text := X509Certs1.SuppOrderId;
        doCertCentreCollect.Enabled := True;
    end;
end;

procedure TX509CertsForm.doCertCentreSaveOrderClick(Sender: TObject);
begin
    if X509Certs1.SupplierProto <> SuppProtoCertCentre then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if CertCentreProducts.ItemIndex < 0 then begin
        AddLog('Must Select a Certificate Product First');
        Exit;
    end;
    SetCertParams;
    CertCentreOrderId.Text := '';
    X509Certs1.SuppOrderId := '';
    X509Certs1.SuppOrderRef := CertCentreOrderRef.Text;
    CertCentreProductsClick(Self) ;
    X509Certs1.SuppCertProduct := CertCentreProducts.Items[CertCentreProducts.ItemIndex];
    if NOT X509Certs1.CCCheckOrder(False, True) then Exit;
    AddLog('Saved Certificate Order');
end;

procedure TX509CertsForm.doCertCentreCollectClick(Sender: TObject);
begin
    if X509Certs1.SupplierProto <> SuppProtoCertCentre then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if CertCentreOrderId.Text = '' then begin
        AddLog('CertCentre Order ID is Required');
        Exit;
    end;
    SetCertParams;
    X509Certs1.SuppOrderId := CertCentreOrderId.Text;
    if X509Certs1.CCGetCert then begin
      //
    end;
end;

procedure TX509CertsForm.doCertCentreOrdersClick(Sender: TObject);
begin
    if X509Certs1.SupplierProto <> SuppProtoCertCentre then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if X509Certs1.CCListAllOrders then begin
        // should we display a grid ??
    end;
end;


procedure TX509CertsForm.doCheckCSRClick(Sender: TObject);
begin
    if (CertOldCsrFile.Text = '') OR (NOT FileExists(CertOldCsrFile.Text)) then begin
        AddLog('Can Not Find Old Certificate Signing Request File');
        Exit;
    end;
    if (CertOldPrvKey.Text <> '') and (NOT FileExists(CertOldPrvKey.Text)) then begin
        AddLog('Can Not Find Old Private Key File');
        Exit;
    end;
    X509Certs1.CertOldCsrFile := CertOldCsrFile.Text;
    X509Certs1.CertOldPrvKey := CertOldPrvKey.Text;
    X509Certs1.CheckCSR(False);
end;

procedure TX509CertsForm.doClearDomainClick(Sender: TObject);
var
    I, J: Integer;
begin
    CertCommonName.Text := '';
    DirPubWebCert.Text := '';
    DirWellKnown.Text := '';
    for I := 1 to CertSANGrid.RowCount - 1 do begin
        for J := 0 to 3 do
            CertSANGrid.Cells[0,J] := '';
    end;
end;


procedure TX509CertsForm.doOACodeTokenClick(Sender: TObject);
begin
    SetOAParams;
    if OAuthAuthCode.Text = '' then begin
        AddLog('Must Specify OAuth2 Authorization Code');
        Exit;
    end;
    X509Certs1.OAGrantAuthToken(OAuthAuthCode.Text);
end;

procedure TX509CertsForm.doOARefreshNowClick(Sender: TObject);
begin
    SetOAParams;
    if OAuthRefToken.Text = '' then begin
        AddLog('Must Specify OAuth2 Refresh Token');
        Exit;
    end;
    X509Certs1.OAGrantRefresh;
end;

procedure TX509CertsForm.doOpenDatabaseClick(Sender: TObject);
begin
    DatabaseDomains.Items.Clear;
    ResetDomButtons;
    LabelDB.Caption := '';
    DirDatabase.Text := Trim(DirDatabase.Text);
    if DirDatabase.Text = '' then Exit;
    X509Certs1.OpenAccount(DirDatabase.Text, False);
    // result checked in event
end;

procedure TX509CertsForm.doCloseDatabaseClick(Sender: TObject);
begin
    X509Certs1.CloseAccount;
    ResetDomButtons;
    DatabaseDomains.Items.Clear;
    LabelDB.Caption := '';
end;

procedure TX509CertsForm.DatabaseDomainsClick(Sender: TObject);
begin
    ResetDomButtons;
    if DatabaseDomains.Items.Count = 0 then Exit;
    if DatabaseDomains.ItemIndex < 0 then Exit;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBCheckClick(Sender: TObject);
begin
    LabelInfoDomain.Caption := '';
    if DatabaseDomains.ItemIndex < 0 then Exit;
    if NOT X509Certs1.CertCheckDomain(DatabaseDomains.Items[DatabaseDomains.ItemIndex].Caption) then Exit;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBOrderClick(Sender: TObject);
begin
    if DatabaseDomains.ItemIndex < 0 then Exit;
    if NOT X509Certs1.CertOrderDomain(DatabaseDomains.Items[DatabaseDomains.ItemIndex].Caption) then Exit;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBCollectClick(Sender: TObject);
begin
    if DatabaseDomains.ItemIndex < 0 then Exit;
    if NOT X509Certs1.CertCollectDomain(DatabaseDomains.Items[DatabaseDomains.ItemIndex].Caption) then Exit;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBCancelClick(Sender: TObject);
begin
    if DatabaseDomains.ItemIndex < 0 then Exit;
    if NOT X509Certs1.CertCancelDomain(DatabaseDomains.Items[DatabaseDomains.ItemIndex].Caption) then Exit;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBRedistClick(Sender: TObject);
begin
    if DatabaseDomains.ItemIndex < 0 then Exit;
    if NOT X509Certs1.CertRedistDomain(DatabaseDomains.Items[DatabaseDomains.ItemIndex].Caption) then Exit;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBRemoveClick(Sender: TObject);
begin
    if DatabaseDomains.ItemIndex < 0 then Exit;
    if NOT X509Certs1.CertRemoveDomain(DatabaseDomains.Items[DatabaseDomains.ItemIndex].Caption) then Exit;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBRevokeClick(Sender: TObject);
begin
    if DatabaseDomains.ItemIndex < 0 then Exit;
    if NOT X509Certs1.CertRevokeDomain(DatabaseDomains.Items[DatabaseDomains.ItemIndex].Caption) then Exit;
    SetDomButtons;
end;

procedure TX509CertsForm.doLoadCAClick(Sender: TObject);
begin
    X509Certs1.CACertFile := CACertFile.Text;
    X509Certs1.CAPkeyFile := CAPkeyFile.Text;
    X509Certs1.CAPkeyPw := CAPkeyPw.Text;
    if NOT X509Certs1.LoadOwnCA then begin
        LabelOwnCA.Caption := 'Failed to load Own CA';
    end
    else begin
        LabelOwnCA.Caption := X509Certs1.NewSslCert.CertInfo(true);
    end;
end;

procedure TX509CertsForm.doSelfSignedClick(Sender: TObject);
begin
    SetCommParams;
    X509Certs1.DirCertWork := IncludeTrailingPathDelimiter(OwnCACertDir.Text);
    X509Certs1.SelfSign;
end;

procedure TX509CertsForm.doTestWellKnownClick(Sender: TObject);
var
    I: Integer;
begin
    SetCertParams;
    if X509Certs1.CertSubAltNames.Count > 0 then begin
        for I := 0 to X509Certs1.CertSubAltNames.Count - 1 do begin
            X509Certs1.TestWellKnown(X509Certs1.CertSubAltNames[I].Domain,
                                 X509Certs1.CertSubAltNames[I].DirWellKnown);
        end;
     end;
end;

procedure TX509CertsForm.doWebServerClick(Sender: TObject);
begin
    SetCommParams;
    X509Certs1.StartDomSrv;
end;

end.
