{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     Aug 26, 2003
Description:  A small utility to export SSL certificate from IE certificate
              store to a directory using OpenSSL PEM file format.
              Make use of the ICS Delphi encapsulation for SSLEAY32.DLL &
              LIBEAY32.DLL (OpenSSL) by Francois Piette <francois.piette@overbyte.be>
              Makes use of OpenSSL (http://www.openssl.org)
              Makes use of the Jedi JwaWincrypt.pas (MPL).
Version:      8.53
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2018 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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
Aug 26, 2003 V1.01 F. Piette <francois.piette@overbyte.be> added persistance
             to export directory and windows position & size. Also added
             compiler switches and version constant.
Aug 31, 2003 V1.02 func ParseNameProp rewritten.
Sep 04, 2003 V1.03 Added LVCert sort on column header click, and a simple
             application exception handler. Fixed 'CopyCert' bug,
             and beautyfied source.
Sep 11, 2003 V1.04 Test version for new IcsOpenSsl.DLL.
Aug 07, 2007 V1.05 ICS-SSL V6 compatibility
Jun 30, 2008 V1.06 A.Garrels made some changes to prepare SSL code for Unicode.
Jun 30, 2008 V1.07 Some RSA and Blowfish crypto functions.
Jul 14, 2008 V1.08 Paul <paul.blommaerts@telenet.be> added an option to import
             Windows certificates to a single file (CA bundle).
Jul 15, 2008 V1.09 Made one change to prepare SSL code for Unicode.
Jan 29, 2009 V1.10 Removed some string cast warnings.
Dec 20, 2009 V1.11 Memory leak fixed.
Feb 13, 2014 V1.14 Angus using TX509Ex instead of TMyX509 to read PEM entries
             PEM display window now shows all major entries separately as well
                as the raw certificate content.
             ListView now always shows subject common name, and ignore errors.
             Added directory selection buttons (but using Open Dialog for ease).
             Optionally add clear text comments to PEM files to easily identify
             certifcates.
June 23, 2014 V1.15 Angus show issuer Common Name and Organisation Unit in
                    certificate comments
Mar 16, 2015 V8.00 Angus default key length now 2048
June 2015,   V8.01 Angus using new units
Oct 23, 2015 V8.02 Angus get certificate signing and encryption algorithms
Oct 18, 2016 V8.35 Angus, no longer need OverbyteIcsLibeayEx
Nov 15, 2016 V8.38 Angus, only load digitally signed OpenSSL DLLs
                   Added Check Signed button that allows a single file to be
                     selected and it's digital certificate tested
Nov 23, 2016 V8.39 Angus replaced TX509Ex with TX509Base
                   View multiple PEM certificates in a bundle file
Jan 27, 2017 V8.40 Angus display multiple certificate file formats
                   Using new TSslCertTools component to read, create and save
                     certificates, private keys, certificate requests, DHParams,
                     and to sign requests as a certificate authority.
                   This tool can now be used to convert different format certificate
                     files between formats, by reading one format and saving as
                     a different format.  Also combining keys and certificates in a file.
Feb 24, 2017 V8.41 Finished changes for TSslCertTools
                   Simplified creating bundles from Windows with new functions
Apr 20, 2017 V8.46 Force random serial for each new certificate to avoid duplicates
Jun 20, 2017 V8.49 Fixed some missing spaces after : in certificate info listing
                   Clear more certificate fields, specifically DNS names
Sep 22, 2017 V8.50 Corrected X25519 private keys to ED25519, requires OpenSSL 1.1.1
                   Alternate DNS names are now correctly added to requests and certs
                   Specify and save CA Bundle separately to certificate to avoid
                      confusion and needing to repeatedly reload pkey to sign certs
Nov 3, 2017  V8.51 Tested ED25519 keys, can now sign requests and certs
             Added RSA-PSS keys and SHA3 digest hashes, requires OpenSSL 1.1.1
Feb 14, 2018 V8.52 TX509 PublicKey now X509PublicKey
Mar 12, 2018 V8.53 Display Wsocket version in About
Jun 11, 2018 V8.55 don't load OpenSSL in Create
Oct 19, 2018 V8.58 version only


Pending
Load a windows certificate store into TX509List
Save a TX509 certificate to a windows certificate store


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsPemtool1;

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

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Buttons,
  StdCtrls, OverbyteIcsIniFiles, ComCtrls, Menus, ImgList, ExtCtrls, CommCtrl,
{$IF CompilerVersion > 23}
  System.UITypes,
{$IFEND}
  OverByteIcsMimeUtils, OverbyteIcsWSocket,
  OverbyteIcsSsleay, OverbyteIcsLibeay,
  OverbyteIcsWinCrypt, OverbyteIcsMsSslUtils,
  OverbyteIcsUtils, OverbyteIcsSslX509Utils;

const
     PemToolVersion     = 858;
     PemToolDate        = 'Oct 19, 2018';
     PemToolName        = 'PEM Certificate Tool';
     CopyRight : String = '(c) 2003-2018 by François PIETTE V8.58 ';
     CaptionMain        = 'ICS PEM Certificate Tool - ';
     WM_APPSTARTUP      = WM_USER + 1;

type
  TfrmPemTool1 = class(TForm)
// following stuff saved to INI file
    CAFilesDir: TEdit;
    CertAddComment: TCheckBox;
    CertAltDomains: TMemo;
    CertAltIPs: TMemo;
    CertCommonName: TEdit;
    CertCountry: TEdit;
    CertDays: TEdit;
    CertDescr: TEdit;
    CertEMail: TEdit;
    CertExtClient: TCheckBox;
    CertExtCodeSign: TCheckBox;
    CertExtEmail: TCheckBox;
    CertExtServer: TCheckBox;
    CertLocality: TEdit;
    CertOrganization: TEdit;
    CertOrganizationalUnit: TEdit;
    CertPassword: TEdit;
    CertSignHash: TRadioGroup;
    CertState: TEdit;
    CertUsageCRLSign: TCheckBox;
    CertUsageCertSign: TCheckBox;
    CertUsageDataEn: TCheckBox;
    CertUsageDigSign: TCheckBox;
    CertUsageKeyAgree: TCheckBox;
    CertUsageKeyEn: TCheckBox;
    CertUsageNonRepud: TCheckBox;
    CheckBoxComment: TCheckBox;
    CheckBoxEmptyDestDir: TCheckBox;
    CheckBoxOverwriteExisting: TCheckBox;
    CheckBoxWarnDestNotEmpty: TCheckBox;
    CheckBoxWriteToBundle: TCheckBox;
    CurrentCertDirEdit: TEdit;
    DHParamFile: TEdit;
    DHParamSize: TRadioGroup;
    DestDirEdit: TEdit;
    KeyEncrypt: TRadioGroup;
    KeyType: TRadioGroup;
    LoadCertFile: TEdit;
    LoadCertInters: TCheckBox;
    LoadCertPrivKey: TCheckBox;
    LoadCertPW: TEdit;
    LoadDirectory: TEdit;
    LoadInterCerts: TEdit;
    LoadPrivatetKey: TEdit;
    LoadRequestFile: TEdit;
    NewCertCopyExt: TCheckBox;
    SaveAutoReplace: TCheckBox;
    SaveCertDer: TEdit;
    SaveCertPW: TEdit;
    SaveCertPem: TEdit;
    SaveDirectory: TEdit;
    SaveInterCerts: TCheckBox;
    SavePkcs12File: TEdit;
    SavePkcs7File: TEdit;
    SavePrivateKey: TCheckBox;
    SavePrvFileFile: TEdit;
    SavePubKeyFile: TEdit;
    SaveReqCertFile: TEdit;
    LoadCaBundleFile: TEdit;  // V8.50

// following not saved
    pmLv: TPopupMenu;
    pmShowDetails: TMenuItem;
    pmDelete: TMenuItem;
    ImageList1: TImageList;
    OpenDlg: TOpenDialog;
    PageControl1: TPageControl;
    TabCertLv: TTabSheet;
    TabImport: TTabSheet;
    LvCerts: TListView;
    btnRefresh: TButton;
    Label4: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    ComboBoxStoreType: TComboBox;
    Label2: TLabel;
    btnImport: TButton;
    Bevel1: TBevel;
    Label5: TLabel;
    btnDeleteCert: TButton;
    btnCopyCert: TButton;
    pmCopy: TMenuItem;
    Bevel2: TBevel;
    Label6: TLabel;
    N1: TMenuItem;
    N2: TMenuItem;
    MainMenu1: TMainMenu;
    MMFile: TMenuItem;
    MMFileExit: TMenuItem;
    MMExtras: TMenuItem;
    MMExtrasCreateSelfSignedCert: TMenuItem;
    MMExtrasCreateCertRequest: TMenuItem;
    MMExtrasEncryptStringRSA: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    MMExtrasEncryptStringBlowfish: TMenuItem;
    MMExtrasEncryptStreamBlowfish: TMenuItem;
    ProgressBar1: TProgressBar;
    MMExtrasEncryptFileBlowfish: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    MMExtrasDecryptFileBlowfish: TMenuItem;
    OpenDirDiag: TOpenDialog;
    SelCurrDir: TBitBtn;
    SelImpDir: TBitBtn;
    btnImportPemFile: TButton;
    Bevel3: TBevel;
    Label7: TLabel;
    btnCheckSigned: TButton;
    About1: TMenuItem;
    TabViewCerts: TTabSheet;
    btnShowBundleFile: TButton;
    btnShowOneFile: TButton;
    Panel1: TPanel;
    Label8: TLabel;
    BoxLoadCert: TGroupBox;
    TabNew: TTabSheet;
    GroupBoxCertCreate: TGroupBox;
    lbCountry: TLabel;
    lbState: TLabel;
    lbLocality: TLabel;
    lbOrganization: TLabel;
    lbOrganizationalUnit: TLabel;
    lbCommonName: TLabel;
    lbEMail: TLabel;
    lbDays: TLabel;
    SelLoadDir: TBitBtn;
    CertLinesOld: TMemo;
    doLoadCert: TButton;
    BoxCertProc: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    SelCertFile: TBitBtn;
    SelPrvKeyFile: TBitBtn;
    SelReqFile: TBitBtn;
    doLoadPrvKey: TButton;
    doLoadReq: TButton;
    doLoadBase64: TButton;
    Label13: TLabel;
    LabelStateCert: TLabel;
    LabelStateReq: TLabel;
    doClearCerts: TButton;
    GroupKeys: TGroupBox;
    Label14: TLabel;
    LabelStatePrivKey: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    KeyPairLines: TMemo;
    doGenKey: TButton;
    DHParamsLines: TMemo;
    doDHParams: TButton;
    doCreateReqProps: TButton;
    doCreateReqCert: TButton;
    doCreateSelfCert: TButton;
    doCreateCACert: TButton;
    BoxCertSave: TGroupBox;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label22: TLabel;
    SelSaveDir: TBitBtn;
    doSaveCertPem: TButton;
    doSaveCertDer: TButton;
    doSaveReqCert: TButton;
    LabelStateCACert: TLabel;
    Label21: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    SelCertsDB: TBitBtn;
    Label25: TLabel;
    Label26: TLabel;
    doLoadInters: TButton;
    SelIntersFile: TBitBtn;
    Label27: TLabel;
    LabelInters: TLabel;
    doSavePkcs12: TButton;
    doSavePkcs7Cert: TButton;
    doSavePrivKey: TButton;
    doSavePubKey: TButton;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    GroupBox1: TGroupBox;
    CertIsCA: TCheckBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupDHParam: TGroupBox;
    CertLinesNew: TMemo;
    Panel2: TPanel;
    Status: TLabel;
    doCreateBundle: TButton;
    Label32: TLabel;
    doCheckBundleWin: TButton;
    doCheckBundleSelf: TButton;
    Label33: TLabel;
    SelCAFile: TBitBtn;
    doLoadCABundle: TButton;

    procedure btnImportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnRefreshClick(Sender: TObject);
    procedure LvCertsDblClick(Sender: TObject);
    procedure btnShowBundleFileClick(Sender: TObject);
    procedure CurrentCertDirEditChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure btnDeleteCertClick(Sender: TObject);
    procedure btnCopyCertClick(Sender: TObject);
    procedure DestDirEditChange(Sender: TObject);
    procedure btnImportPemFileClick(Sender: TObject);
    procedure LvCertsColumnClick(Sender: TObject; Column: TListColumn);
    procedure LvCertsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure LvCertsCustomDraw(Sender: TCustomListView;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure AppOnException(Sender: TObject; E: Exception);
    procedure MMFileExitClick(Sender: TObject);
    procedure MMExtrasCreateSelfSignedCertClick(Sender: TObject);
    procedure MMExtrasCreateCertRequestClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MMExtrasEncryptStringRSAClick(Sender: TObject);
    procedure MMExtrasEncryptStringBlowfishClick(Sender: TObject);
    procedure MMExtrasEncryptStreamBlowfishClick(Sender: TObject);
    procedure MMExtrasEncryptFileBlowfishClick(Sender: TObject);
    procedure MMExtrasDecryptFileBlowfishClick(Sender: TObject);
    procedure SelCurrDirClick(Sender: TObject);
    procedure SelImpDirClick(Sender: TObject);
    procedure btnCheckSignedClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure btnShowOneFileClick(Sender: TObject);
    procedure SelLoadDirClick(Sender: TObject);
    procedure SelCertFileClick(Sender: TObject);
    procedure SelPrvKeyFileClick(Sender: TObject);
    procedure SelReqFileClick(Sender: TObject);
    procedure doLoadCertClick(Sender: TObject);
    procedure doLoadPrvKeyClick(Sender: TObject);
    procedure doLoadReqClick(Sender: TObject);
    procedure SelIntersFileClick(Sender: TObject);
    procedure doLoadBase64Click(Sender: TObject);
    procedure doClearCertsClick(Sender: TObject);
    procedure doCreateReqPropsClick(Sender: TObject);
    procedure doCreateReqCertClick(Sender: TObject);
    procedure doCreateSelfCertClick(Sender: TObject);
    procedure doCreateCACertClick(Sender: TObject);
    procedure SelSaveDirClick(Sender: TObject);
    procedure doSaveCertPemClick(Sender: TObject);
    procedure doSaveCertDerClick(Sender: TObject);
    procedure doSaveReqCertClick(Sender: TObject);
    procedure doSavePkcs12Click(Sender: TObject);
    procedure doSavePkcs7CertClick(Sender: TObject);
    procedure doSavePrivKeyClick(Sender: TObject);
    procedure doSavePubKeyClick(Sender: TObject);
    procedure CAFilesDirClick(Sender: TObject);
    procedure SelCertsDBClick(Sender: TObject);
    procedure doGenKeyClick(Sender: TObject);
    procedure doDHParamsClick(Sender: TObject);
    procedure doLoadIntersClick(Sender: TObject);
    procedure doCreateBundleClick(Sender: TObject);
    procedure doCheckBundleWinClick(Sender: TObject);
    procedure doCheckBundleSelfClick(Sender: TObject);
    procedure SelCAFileClick(Sender: TObject);
    procedure doLoadCABundleClick(Sender: TObject);
  protected
    procedure WMAppStartup(var Msg: TMessage); message WM_APPSTARTUP;
  private
    FProgDir         : String;
    FInitialized     : Boolean;
    FCurrentCertDir  : String;
    FLVSortFlag      : Boolean;
    FSslCertTools    : TSslCertTools;
    FSslCAX09        : TX509Base;    // V8.50
    procedure AddListView(X: TX509Base; const Filename: String);
    procedure FillListView;
    procedure ShowBundleCerts(const FileName: String);
    procedure ShowOneCert(const FileName: String);
    function  BuildLoadName(const fname: string): string;
    function  BuildSaveName(const fname: string): string;
    procedure SetCertProps;
    procedure ToolsOKeyProgress(Sender: TObject);
    procedure DispError(const Err: String);
    procedure DispCert;
    procedure DispPKey;
    procedure DispReq;
    procedure DispInter;
    procedure DispCACert;
    procedure ShowLogWindow;
  public
    FIniFileName    : String;
    LogWinOpen      : Boolean;
  end;

  function  FindPemFileName(const FileName: String): String;
  function  DirectoryExists(const Name: string): Boolean;
  function  IsDirEmpty(const Path: String): Boolean;
  function  PathAddBackSlash(const Path: String): String;
  procedure EmptyDirectory(Path: String);

var
  frmPemTool1 : TfrmPemTool1;
  ColumnToSort: Integer;
  VerifyDir: String;
  StartTickCount: integer;
  MsCertChainEngine: TMsCertChainEngine;   { V8.41 }

implementation

{$R *.DFM}

uses
    OverByteIcsPemTool2, OverByteIcsPemTool3;

const
    SectionMainWindow    = 'MainWindow';
    SectionDisplayWindow = 'DisplayWindow';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';
    SectionData          = 'Data';
    KeyVerifyDir         = 'VerifyDir';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.AppOnException(Sender: TObject; E: Exception);
begin
    if MessageDlg(E.ClassName + ': ' + E.Message + #13#10
                + 'Exit PemTool now?',
                   mtError, [mbYes, mbNo], 0) = mrYes then
        Application.Terminate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DispError(const Err: String);  { V8.40 }
begin
    Status.Caption := StringReplace (Err, #13#10, ' ', [rfReplaceAll]);
    beep;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.ToolsOKeyProgress(Sender: TObject);
begin
    Application.ProcessMessages;
    if StartTickCount = 0 then Exit;
    Status.Caption := 'Generating prime numbers: ' + IntToStr(IcsCalcTickDiff
                            (StartTickCount, GetTickCount) div 1000) + ' secs';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FormCreate(Sender: TObject);
begin
    Application.OnException := AppOnException;
    FProgDir     := ExtractFilePath(ParamStr(0));
    FIniFileName := GetIcsIniFileName;
    ComboBoxStoreType.ItemIndex := 0;
 // Avoid dynamical loading and unloading the SSL DLLs plenty of times
    GSSLEAY_DLL_IgnoreNew := False;  { V8.38 don't ignore OpenSSL 1.1.0 and later }
//  GSSLEAY_DLL_IgnoreNew := True;  { V8.38 don't ignore OpenSSL 1.1.0 and later }
//  GSSLEAY_DLL_IgnoreOld := True;   { V8.38 ignore OpenSSL 1.0.2 and earlier }
    GSSL_DLL_DIR := FProgDir;        { V8.38 only from our directory }
    GSSL_SignTest_Check := True;     { V8.38 check digitally signed }
    GSSL_SignTest_Certificate := True; { V8.38 check digital certificate }
    OpenDlg.Filter := 'Certs *.pem;*.cer;*.crt;*.der;*.p12;*.pfx;*.p7*;*.spc|' +
                            '*.pem;*.cer;*.crt;*.der;*.p12;*.pfx;*.p7*;*.spc|' +
                            'All Files *.*|*.*';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FormDestroy(Sender: TObject);
begin
    FreeAndNil (MsCertChainEngine) ;
    OverbyteIcsWSocket.UnLoadSsl;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile      := TIcsIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionMainWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionMainWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionMainWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionMainWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        VerifyDir := IniFile.ReadString(SectionData, KeyVerifyDir, FProgDir); { V8.38 }
        with IniFile do begin   { V8.40 }
          CAFilesDir.Text := ReadString (SectionData, 'CAFilesDir_Text', CAFilesDir.Text) ;
          if ReadString (SectionData, 'CertAddComment_Checked', 'False') = 'True' then CertAddComment.Checked := true else CertAddComment.Checked := false ;
          CertAltDomains.Lines.CommaText := ReadString (SectionData, 'CertAltDomains_CommaText', '') ;
          CertAltIPs.Lines.CommaText := ReadString (SectionData, 'CertAltIPs_CommaText', '') ;
          CertCommonName.Text := ReadString (SectionData, 'CertCommonName_Text', CertCommonName.Text) ;
          CertCountry.Text := ReadString (SectionData, 'CertCountry_Text', CertCountry.Text) ;
          CertDays.Text := ReadString (SectionData, 'CertDays_Text', CertDays.Text) ;
          CertDescr.Text := ReadString (SectionData, 'CertDescr_Text', CertDescr.Text) ;
          CertEMail.Text := ReadString (SectionData, 'CertEMail_Text', CertEMail.Text) ;
          if ReadString (SectionData, 'CertExtClient_Checked', 'False') = 'True' then CertExtClient.Checked := true else CertExtClient.Checked := false ;
          if ReadString (SectionData, 'CertExtCodeSign_Checked', 'False') = 'True' then CertExtCodeSign.Checked := true else CertExtCodeSign.Checked := false ;
          if ReadString (SectionData, 'CertExtEmail_Checked', 'False') = 'True' then CertExtEmail.Checked := true else CertExtEmail.Checked := false ;
          if ReadString (SectionData, 'CertExtServer_Checked', 'False') = 'True' then CertExtServer.Checked := true else CertExtServer.Checked := false ;
          CertLocality.Text := ReadString (SectionData, 'CertLocality_Text', CertLocality.Text) ;
          CertOrganization.Text := ReadString (SectionData, 'CertOrganization_Text', CertOrganization.Text) ;
          CertOrganizationalUnit.Text := ReadString (SectionData, 'CertOrganizationalUnit_Text', CertOrganizationalUnit.Text) ;
          CertPassword.Text := ReadString (SectionData, 'CertPassword_Text', CertPassword.Text) ;
          CertSignHash.ItemIndex := ReadInteger (SectionData, 'CertSignHash_ItemIndex', CertSignHash.ItemIndex) ;
          CertState.Text := ReadString (SectionData, 'CertState_Text', CertState.Text) ;
          if ReadString (SectionData, 'CertUsageCRLSign_Checked', 'False') = 'True' then CertUsageCRLSign.Checked := true else CertUsageCRLSign.Checked := false ;
          if ReadString (SectionData, 'CertUsageCertSign_Checked', 'False') = 'True' then CertUsageCertSign.Checked := true else CertUsageCertSign.Checked := false ;
          if ReadString (SectionData, 'CertUsageDataEn_Checked', 'False') = 'True' then CertUsageDataEn.Checked := true else CertUsageDataEn.Checked := false ;
          if ReadString (SectionData, 'CertUsageDigSign_Checked', 'False') = 'True' then CertUsageDigSign.Checked := true else CertUsageDigSign.Checked := false ;
          if ReadString (SectionData, 'CertUsageKeyAgree_Checked', 'False') = 'True' then CertUsageKeyAgree.Checked := true else CertUsageKeyAgree.Checked := false ;
          if ReadString (SectionData, 'CertUsageKeyEn_Checked', 'False') = 'True' then CertUsageKeyEn.Checked := true else CertUsageKeyEn.Checked := false ;
          if ReadString (SectionData, 'CertUsageNonRepud_Checked', 'False') = 'True' then CertUsageNonRepud.Checked := true else CertUsageNonRepud.Checked := false ;
          if ReadString (SectionData, 'CheckBoxComment_Checked', 'False') = 'True' then CheckBoxComment.Checked := true else CheckBoxComment.Checked := false ;
          if ReadString (SectionData, 'CheckBoxEmptyDestDir_Checked', 'False') = 'True' then CheckBoxEmptyDestDir.Checked := true else CheckBoxEmptyDestDir.Checked := false ;
          if ReadString (SectionData, 'CheckBoxOverwriteExisting_Checked', 'False') = 'True' then CheckBoxOverwriteExisting.Checked := true else CheckBoxOverwriteExisting.Checked := false ;
          if ReadString (SectionData, 'CheckBoxWarnDestNotEmpty_Checked', 'False') = 'True' then CheckBoxWarnDestNotEmpty.Checked := true else CheckBoxWarnDestNotEmpty.Checked := false ;
          if ReadString (SectionData, 'CheckBoxWriteToBundle_Checked', 'False') = 'True' then CheckBoxWriteToBundle.Checked := true else CheckBoxWriteToBundle.Checked := false ;
          CurrentCertDirEdit.Text := ReadString (SectionData, 'CurrentCertDirEdit_Text', CurrentCertDirEdit.Text) ;
          DHParamFile.Text := ReadString (SectionData, 'DHParamFile_Text', DHParamFile.Text) ;
          DHParamSize.ItemIndex := ReadInteger (SectionData, 'DHParamSize_ItemIndex', DHParamSize.ItemIndex) ;
          DestDirEdit.Text := ReadString (SectionData, 'DestDirEdit_Text', DestDirEdit.Text) ;
          KeyEncrypt.ItemIndex := ReadInteger (SectionData, 'KeyEncrypt_ItemIndex', KeyEncrypt.ItemIndex) ;
          KeyType.ItemIndex := ReadInteger (SectionData, 'KeyType_ItemIndex', KeyType.ItemIndex) ;
          LoadCertFile.Text := ReadString (SectionData, 'LoadCertFile_Text', LoadCertFile.Text) ;
          if ReadString (SectionData, 'LoadCertInters_Checked', 'False') = 'True' then LoadCertInters.Checked := true else LoadCertInters.Checked := false ;
          if ReadString (SectionData, 'LoadCertPrivKey_Checked', 'False') = 'True' then LoadCertPrivKey.Checked := true else LoadCertPrivKey.Checked := false ;
          LoadCertPW.Text := ReadString (SectionData, 'LoadCertPW_Text', LoadCertPW.Text) ;
          LoadDirectory.Text := ReadString (SectionData, 'LoadDirectory_Text', LoadDirectory.Text) ;
          LoadInterCerts.Text := ReadString (SectionData, 'LoadInterCerts_Text', LoadInterCerts.Text) ;
          LoadPrivatetKey.Text := ReadString (SectionData, 'LoadPrivatetKey_Text', LoadPrivatetKey.Text) ;
          LoadRequestFile.Text := ReadString (SectionData, 'LoadRequestFile_Text', LoadRequestFile.Text) ;
          if ReadString (SectionData, 'NewCertCopyExt_Checked', 'False') = 'True' then NewCertCopyExt.Checked := true else NewCertCopyExt.Checked := false ;
          if ReadString (SectionData, 'SaveAutoReplace_Checked', 'False') = 'True' then SaveAutoReplace.Checked := true else SaveAutoReplace.Checked := false ;
          SaveCertDer.Text := ReadString (SectionData, 'SaveCertDer_Text', SaveCertDer.Text) ;
          SaveCertPem.Text := ReadString (SectionData, 'SaveCertPem_Text', SaveCertPem.Text) ;
          SaveCertPW.Text := ReadString (SectionData, 'SaveCertPW_Text', SaveCertPW.Text) ;
          SaveDirectory.Text := ReadString (SectionData, 'SaveDirectory_Text', SaveDirectory.Text) ;
          if ReadString (SectionData, 'SaveInterCerts_Checked', 'False') = 'True' then SaveInterCerts.Checked := true else SaveInterCerts.Checked := false ;
          SavePkcs12File.Text := ReadString (SectionData, 'SavePkcs12File_Text', SavePkcs12File.Text) ;
          SavePkcs7File.Text := ReadString (SectionData, 'SavePkcs7File_Text', SavePkcs7File.Text) ;
          if ReadString (SectionData, 'SavePrivateKey_Checked', 'False') = 'True' then SavePrivateKey.Checked := true else SavePrivateKey.Checked := false ;
          SavePrvFileFile.Text := ReadString (SectionData, 'SavePrvFileFile_Text', SavePrvFileFile.Text) ;
          SavePubKeyFile.Text := ReadString (SectionData, 'SavePubKeyFile_Text', SavePubKeyFile.Text) ;
          SaveReqCertFile.Text := ReadString (SectionData, 'SaveReqCertFile_Text', SaveReqCertFile.Text) ;
          LoadCaBundleFile.Text := ReadString (SectionData, 'LoadCaBundleFile_Text', LoadCaBundleFile.Text) ;
       end;
        IniFile.Free;
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.WMAppStartup(var Msg: TMessage);
begin
 // V8.55 don't load OpenSSL in create
    OverbyteIcsWSocket.LoadSsl;
    FSslCertTools := TSslCertTools.Create(self);
    FSslCertTools.OnKeyProgress := ToolsOKeyProgress;
    FSslCAX09 := TX509Base.Create(self);    // V8.50
    frmPemTool1.Caption := CaptionMain + Trim(CurrentCertDirEdit.Text);
    PageControl1.ActivePageIndex := 0;
    LvCerts.Perform(CM_RECREATEWND, 0, 0); // fix column buttons not displayed
    FillListView;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
    temp: String;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionMainWindow, KeyTop,               Top);
    IniFile.WriteInteger(SectionMainWindow, KeyLeft,              Left);
    IniFile.WriteInteger(SectionMainWindow, KeyWidth,             Width);
    IniFile.WriteInteger(SectionMainWindow, KeyHeight,            Height);
    IniFile.WriteString(SectionData,        KeyVerifyDir,         VerifyDir); { V8.38 }
    with IniFile do begin   { V8.40 }
          WriteString (SectionData, 'CAFilesDir_Text', CAFilesDir.Text) ;
          if CertAddComment.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertAddComment_Checked', temp) ;
          WriteString (SectionData, 'CertAltDomains_CommaText', CertAltDomains.Lines.CommaText) ;
          WriteString (SectionData, 'CertAltIPs_CommaText', CertAltIPs.Lines.CommaText) ;
          WriteString (SectionData, 'CertCommonName_Text', CertCommonName.Text) ;
          WriteString (SectionData, 'CertCountry_Text', CertCountry.Text) ;
          WriteString (SectionData, 'CertDays_Text', CertDays.Text) ;
          WriteString (SectionData, 'CertDescr_Text', CertDescr.Text) ;
          WriteString (SectionData, 'CertEMail_Text', CertEMail.Text) ;
          if CertExtClient.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertExtClient_Checked', temp) ;
          if CertExtCodeSign.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertExtCodeSign_Checked', temp) ;
          if CertExtEmail.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertExtEmail_Checked', temp) ;
          if CertExtServer.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertExtServer_Checked', temp) ;
          WriteString (SectionData, 'CertLocality_Text', CertLocality.Text) ;
          WriteString (SectionData, 'CertOrganization_Text', CertOrganization.Text) ;
          WriteString (SectionData, 'CertOrganizationalUnit_Text', CertOrganizationalUnit.Text) ;
          WriteString (SectionData, 'CertPassword_Text', CertPassword.Text) ;
          WriteInteger (SectionData, 'CertSignHash_ItemIndex', CertSignHash.ItemIndex) ;
          WriteString (SectionData, 'CertState_Text', CertState.Text) ;
          if CertUsageCRLSign.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageCRLSign_Checked', temp) ;
          if CertUsageCertSign.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageCertSign_Checked', temp) ;
          if CertUsageDataEn.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageDataEn_Checked', temp) ;
          if CertUsageDigSign.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageDigSign_Checked', temp) ;
          if CertUsageKeyAgree.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageKeyAgree_Checked', temp) ;
          if CertUsageKeyEn.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageKeyEn_Checked', temp) ;
          if CertUsageNonRepud.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageNonRepud_Checked', temp) ;
          if CheckBoxComment.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CheckBoxComment_Checked', temp) ;
          if CheckBoxEmptyDestDir.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CheckBoxEmptyDestDir_Checked', temp) ;
          if CheckBoxOverwriteExisting.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CheckBoxOverwriteExisting_Checked', temp) ;
          if CheckBoxWarnDestNotEmpty.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CheckBoxWarnDestNotEmpty_Checked', temp) ;
          if CheckBoxWriteToBundle.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CheckBoxWriteToBundle_Checked', temp) ;
          WriteString (SectionData, 'CurrentCertDirEdit_Text', CurrentCertDirEdit.Text) ;
          WriteString (SectionData, 'DHParamFile_Text', DHParamFile.Text) ;
          WriteInteger (SectionData, 'DHParamSize_ItemIndex', DHParamSize.ItemIndex) ;
          WriteString (SectionData, 'DestDirEdit_Text', DestDirEdit.Text) ;
          WriteInteger (SectionData, 'KeyEncrypt_ItemIndex', KeyEncrypt.ItemIndex) ;
          WriteInteger (SectionData, 'KeyType_ItemIndex', KeyType.ItemIndex) ;
          WriteString (SectionData, 'LoadCertFile_Text', LoadCertFile.Text) ;
          if LoadCertInters.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'LoadCertInters_Checked', temp) ;
          if LoadCertPrivKey.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'LoadCertPrivKey_Checked', temp) ;
          WriteString (SectionData, 'LoadCertPW_Text', LoadCertPW.Text) ;
          WriteString (SectionData, 'LoadDirectory_Text', LoadDirectory.Text) ;
          WriteString (SectionData, 'LoadInterCerts_Text', LoadInterCerts.Text) ;
          WriteString (SectionData, 'LoadPrivatetKey_Text', LoadPrivatetKey.Text) ;
          WriteString (SectionData, 'LoadRequestFile_Text', LoadRequestFile.Text) ;
          if NewCertCopyExt.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'NewCertCopyExt_Checked', temp) ;
          if SaveAutoReplace.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'SaveAutoReplace_Checked', temp) ;
          WriteString (SectionData, 'SaveCertDer_Text', SaveCertDer.Text) ;
          WriteString (SectionData, 'SaveCertPem_Text', SaveCertPem.Text) ;
          WriteString (SectionData, 'SaveCertPW_Text', SaveCertPW.Text) ;
          WriteString (SectionData, 'SaveDirectory_Text', SaveDirectory.Text) ;
          if SaveInterCerts.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'SaveInterCerts_Checked', temp) ;
          WriteString (SectionData, 'SavePkcs12File_Text', SavePkcs12File.Text) ;
          WriteString (SectionData, 'SavePkcs7File_Text', SavePkcs7File.Text) ;
          if SavePrivateKey.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'SavePrivateKey_Checked', temp) ;
          WriteString (SectionData, 'SavePrvFileFile_Text', SavePrvFileFile.Text) ;
          WriteString (SectionData, 'SavePubKeyFile_Text', SavePubKeyFile.Text) ;
          WriteString (SectionData, 'SaveReqCertFile_Text', SaveReqCertFile.Text) ;
          WriteString (SectionData, 'LoadCaBundleFile_Text', LoadCaBundleFile.Text) ;
    end;
    IniFile.UpdateFile;
    IniFile.Free;
    if Assigned (FSslCertTools) then FSslCertTools.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ListCertDetail(Cert: TX509Base): string;
begin
    if NOT Assigned (Cert) then begin
        Result := 'No certificate loaded';
        Exit;
    end;
    with Cert do begin
    { Angus added major PEM entries separately, also serves to document how
    to access all the different properties of the T509 component.  Note multiple
    items may be returned, normally separated by CRLF, but unwrapped here for display.
    Rarely does a certificate have all these entries, particuarly the personal name
    stuff which is really for email certificates  }
    { V8.49 fixed some missing spaces after : }
        Result := 'ISSUED TO (Subject)' + #13#10 +
            'Common Name (CN): ' + IcsUnwrapNames(SubjectCName) + #13#10 +
            'Alt Name (DNS): ' + IcsUnwrapNames(SubAltNameDNS) + #13#10 +
            'Alt Name (IP): ' + IcsUnwrapNames(SubAltNameIP) + #13#10 +
            'Organisation (O): ' + IcsUnwrapNames(SubjectOName) + #13#10 +
            'Organisational Unit (OU): ' + IcsUnwrapNames(SubjectOUName) + #13#10 +
            'Country (C): ' + SubjectCOName + #13#10 +
            'State/Province(ST): ' + SubjectSTName + #13#10 +
            'Locality (L): ' + SubjectLName + #13#10 +
            'Serial Number: ' + SubjectSerialName + #13#10 +
            'Title (T): ' + GetNameEntryByNid(TRUE, NID_title) + #13#10 +
            'Initials (I): ' + GetNameEntryByNid(TRUE, NID_initials) + #13#10 +
            'Given Name (G): ' + GetNameEntryByNid(TRUE, NID_givenName) + #13#10 +
            'Surname (S): ' + GetNameEntryByNid(TRUE, NID_surname) + #13#10 +
            'Description (D): ' + GetNameEntryByNid(TRUE, NID_description) + #13#10 +
            'Email (Email): ' + SubjectEmailName + #13#10 +
            '' + #13#10;
        if SelfSigned then
            Result := Result + 'SELF SIGNED' + #13#10
        else begin
            Result := Result + 'ISSUED BY' + #13#10 +
                'Common Name (CN): ' + IcsUnwrapNames(IssuerCName) + #13#10 +
                'Organisation (O): ' + IcsUnwrapNames(IssuerOName) + #13#10 +
                'Organisational Unit (OU): ' + IcsUnwrapNames(IssuerOUName) + #13#10 +
                'Country (C): ' + IssuerCOName + #13#10 +
                'State/Province(ST): ' + IssuerSTName + #13#10 +
                'Locality (L): ' + IssuerLName + #13#10 +
                'Email (Email): ' + IssuerEmailName + #13#10;
        end;
        Result := Result + '' + #13#10 +
            'GENERAL' + #13#10 +
            'Serial Number: ' + SerialNumHex + #13#10 + // Oct 2015 not always very numeric IntToStr (SerialNum));
            'Issued on: ' + DateToStr(ValidNotBefore) + #13#10 +
            'Expires on: ' + DateToStr(ValidNotAfter) + #13#10 +
            'Basic Constraints: ' + IcsUnwrapNames(BasicConstraints) + #13#10 +
            'Key Usage: ' + IcsUnwrapNames(KeyUsage) + #13#10 +
            'Extended Key Usage: ' + IcsUnwrapNames(ExKeyUsage) + #13#10 +
            'Authority Info Access: ' + IcsUnwrapNames(AuthorityInfoAccess) + #13#10 +
            'Certificate Policies: ' + IcsUnwrapNames(CertPolicies) + #13#10 +
            'CRL Distribution Points: ' + IcsUnwrapNames(CRLDistribution) + #13#10 +
            'Authority Key Identifier: ' + IcsUnwrapNames(AuthorityKeyId) + #13#10 +
            'Subject Key Identifier: ' + IcsUnwrapNames(SubjectKeyId) + #13#10 +
            'Signature Algorithm: ' + SignatureAlgorithm + #13#10 +  // Oct 2015
            'Fingerprint (sha1): ' + IcsLowerCase(Sha1Hex) + #13#10;
        if ExtendedValidation then
            Result := Result + 'Extended Validation (EV) SSL Server Certificate' + #13#10;
        Result := Result + 'Key Info: ' + KeyInfo + #13#10;                         // Oct 2015
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.ShowLogWindow;
begin
    if LogWinOpen then exit;
    frmPemTool2 := TfrmPemTool2.Create(Self);
    frmPemTool2.Show;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetCertReadOpt(value: Boolean): TCertReadOpt;
begin
    if Value then
        Result := croTry
    else
        Result := croNo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.ShowOneCert(const FileName: String);
var
    Cert: TX509Base;
begin
    if (FileName = '') or not FileExists(FileName) then
        raise Exception.Create('FileName empty or file doesn''t exist');
    Cert := TX509Base.Create(nil);
    try
        frmPemTool2 := TfrmPemTool2.Create(Self);
        try
            try
                frmPemTool2.Caption := FileName;
                frmPemTool2.Memo1.Lines.Clear;
                Cert.LoadFromFile(Filename, croTry, croTry, CertPassword.Text);
                if NOT Cert.IsCertLoaded then begin
                    frmPemTool2.Memo1.Lines.Add ('No certificate found in file ' + FileName);
                end
                else begin
                    frmPemTool2.Memo1.Lines.Add ('Certificate file ' + FileName);
                    frmPemTool2.Memo1.Lines.Add (ListCertDetail(Cert) + #13#10);
                    frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                                    'Raw Cert' + #13#10 + Cert.GetRawText + #13#10;
                    if Cert.IsPKeyLoaded then begin
                        frmPemTool2.Memo1.Lines.Add ('!! Private key available for certificate: ' +
                                                                               Cert.KeyInfo + #13#10);
                    end;
                end;
                if Cert.IsInterLoaded then begin
                     frmPemTool2.Memo1.Lines.Add ('!! Intermediate certificates: ' +
                                                                  Cert.ListInters + #13#10);
                end;
            except
                on E:Exception do
                begin
                    frmPemTool2.Memo1.Lines.Add (E.Message) ;
                end;
            end;
            frmPemTool2.ShowModal;
        finally
            frmPemTool2.free;
        end;
    finally
        Cert.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.ShowBundleCerts(const FileName: String);
var
    Certs: TX509List;      { V8.39 read multiple certificates from PEM file }
    Total, I: Integer;
    Info: String;
begin
    if (FileName = '') or not FileExists(FileName) then
        raise Exception.Create('FileName empty or file doesn''t exist');
    Certs := TX509List.Create(nil, True);
    try
        Total := Certs.LoadAllFromFile(Filename);
        frmPemTool2 := TfrmPemTool2.Create(Self);
        try
            frmPemTool2.Caption := FileName;
            frmPemTool2.Memo1.Lines.Clear;
            if Total <= 0 then begin
                frmPemTool2.Memo1.Lines.Add ('No PEM certificates found in file');
            end
            else begin
                frmPemTool2.Memo1.Lines.Add ('Certificate file ' + FileName);
                frmPemTool2.Memo1.Lines.Add ('Number of PEM certificates found in file: ' + IntToStr (Total));

                for I := 1 to Total do begin
                    Info := #13#10 + 'Certificate #' + IntToStr(I) + #13#10 + ListCertDetail(Certs [I-1]);
                    frmPemTool2.Memo1.Lines.Add (Info);
                    frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                                                     'Raw #' + IntToStr(I) + #13#10 + Certs [I-1].GetRawText;
                end;
            end;
            frmPemTool2.ShowModal;
        finally
            frmPemTool2.free;
        end;
    finally
        Certs.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnShowBundleFileClick(Sender: TObject);
var
    FileName : String;
begin
    if OpenDlg.InitialDir = '' then OpenDlg.InitialDir := DestDirEdit.Text;
    OpenDlg.Filter     := 'PEM Certs *.pem|*.pem|All Files *.*|*.*';
    if OpenDlg.Execute then
        FileName := OpenDlg.FileName;
    if (FileName = '') or not FileExists(FileName) then
        Exit;
    OpenDlg.InitialDir := ExtractFileDir(FileName);
    ShowBundleCerts(FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnShowOneFileClick(Sender: TObject);
var
    FileName : String;
begin
    if OpenDlg.InitialDir = '' then OpenDlg.InitialDir := DestDirEdit.Text;
    if OpenDlg.Execute then
        FileName := OpenDlg.FileName;
    if (FileName = '') or not FileExists(FileName) then
        Exit;
    OpenDlg.InitialDir := ExtractFileDir(FileName);
    ShowOneCert(FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.AddListView(X: TX509Base; const Filename: String);
var
    ListItem : TListItem;
    S        : String;
begin
    with LVCerts do begin
        ListItem          := Items.Add;
        ListItem.Caption  := X.SubjectCName;
        if ListItem.Caption = '' then
            ListItem.Caption := X.SubjectOName;
        S := X.SubjectOUName;
        if S = '' then
            S := X.SubjectOName;
        ListItem.SubItems.Add(S);
        S := X.IssuerCName;
        if S = '' then
            S := X.IssuerOUName;
        if S = '' then
            S := X.IssuerOName;
        ListItem.SubItems.Add(S);
        ListItem.SubItems.Add(DateToStr(X.ValidNotAfter));
        ListItem.SubItems.Add(ExtractFileName(FileName));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FillListView;
var
    SRec    : TSearchRec;
    CertDir : String;
    X       : TX509Base;
begin
    X := TX509Base.Create(nil);
    try
        LvCerts.Items.BeginUpdate;
        try
            LVCerts.Items.Clear;
            FCurrentCertDir := Trim(CurrentCertDirEdit.Text);
            if not DirectoryExists(FCurrentCertDir) then
                Exit;
            CertDir := PathAddBackSlash(FCurrentCertDir);
            if FindFirst(CertDir + '*.*', faAnyFile - faDirectory, SRec) = 0 then
            try
                try
                    X.LoadFromFile(CertDir + SRec.Name, croNo, croNo, CertPassword.Text);  { V8.40 was PemFile }
                    AddListView(X, CertDir + SRec.Name);
                except  // angus - ignore files that are not really certificates
                end;
                while FindNext(SRec) = 0 do begin
                    try
                        X.LoadFromFile(CertDir + SRec.Name, croNo, croNo, CertPassword.Text);
                        AddListView(X, CertDir + SRec.Name);
                    except
                    end;
                end;
            finally
                FindClose(SRec);
            end;
        finally
            LvCerts.Items.EndUpdate;
        end;
    finally
        X.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LvCertsDblClick(Sender: TObject);
var
    ListItem : TListItem;
    FileName : String;
begin
    with LvCerts do begin
        if Assigned(Selected) then begin
            ListItem := Items[Selected.Index];
            FileName := ListItem.SubItems[3];
            if FileName <> '' then
                FileName := PathAddBackSlash(FCurrentCertDir) + FileName;
            if FileExists(FileName) then
                ShowOneCert(FileName);
        end
        else
            MessageDlg('No item selected', mtError, [mbOK], 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LvCertsColumnClick(Sender: TObject;
    Column: TListColumn);
var
    I : Integer;
begin
    Screen.Cursor := crHourGlass;
    LVCerts.Items.BeginUpdate;
    try
        if ColumnToSort = Column.Index then
            FLVSortFlag := not FLVSortFlag;
        for I := 0 to LVCerts.Columns.Count -1 do begin
             if I <> Column.Index then
                 LVCerts.Columns[I].ImageIndex := -1
             else begin
                 if FLVSortFlag then
                     LVCerts.Columns[Column.Index].ImageIndex := 1
                 else
                     LVCerts.Columns[Column.Index].ImageIndex := 2;
             end;
        end;
        ColumnToSort := Column.Index;
        with (Sender as TCustomListView) do begin
            AlphaSort;
            if Assigned(Selected) then
                Selected.MakeVisible(TRUE);
        end;
        LVCerts.Items.EndUpdate;
        Screen.Cursor := crDefault;
    except
        LVCerts.Items.EndUpdate;
        Screen.Cursor := crDefault;
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LvCertsCompare(Sender: TObject; Item1,
    Item2: TListItem; Data: Integer; var Compare: Integer);
var
    Idx: Integer;
begin
    if not FLVSortFlag then begin
        if ColumnToSort = 0 then
            Compare := CompareText(Item1.Caption, Item2.Caption)
        else begin
            Idx := ColumnToSort - 1;
            if Idx = 1 then begin
                Compare := 0;
                if StrToDate(Item1.SubItems[Idx]) > StrToDate(Item2.SubItems[Idx]) then
                    Compare := 1
                else
                if StrToDate(Item1.SubItems[Idx]) < StrToDate(Item2.SubItems[Idx]) then
                    Compare := -1;
                Exit;
            end;
            Compare := CompareText(Item1.SubItems[Idx], Item2.SubItems[Idx]);
        end;
    end
    else begin
        if ColumnToSort = 0 then
            Compare := CompareText(Item2.Caption, Item1.Caption)
        else begin
            Idx := ColumnToSort - 1;
            if Idx = 1 then begin
                Compare := 0;
                if StrToDate(Item1.SubItems[Idx]) < StrToDate(Item2.SubItems[Idx]) then
                    Compare := 1
                else
                if StrToDate(Item1.SubItems[Idx]) > StrToDate(Item2.SubItems[Idx]) then
                    Compare := -1;
                Exit;
            end;
            Compare := CompareText(Item2.SubItems[Idx], Item1.SubItems[Idx]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LvCertsCustomDraw(Sender: TCustomListView;
    const ARect    : TRect;
    var   DefaultDraw: Boolean);
var
    I            : Integer;
    LvColumn     : TLVColumn;
    HeaderHandle : THandle;
begin
    { Display sort BMP on the right of column caption}
    { requires comctl32.dll version 4.70+            }
    HeaderHandle := GetDlgItem(LVCerts.Handle, 0);
    for I := 0 to LVCerts.Columns.Count - 1 do begin
        if (LVCerts.Columns[I].ImageIndex <> -1) then begin
            FillChar(LvColumn, SizeOf(LvColumn), #0);
            ListView_GetColumn(HeaderHandle, I, LvColumn);
            with LvColumn do begin
                iImage := LVCerts.Columns[I].ImageIndex;
                mask   := mask or LVCF_IMAGE or LVCF_FMT;
                fmt    := fmt or LVCFMT_IMAGE or LVCFMT_BITMAP_ON_RIGHT;
                case LVCerts.Columns[I].Alignment of
                    taLeftJustify  : fmt := fmt or LVCFMT_LEFT;
                    taCenter       : fmt := fmt or LVCFMT_CENTER;
                    taRightJustify : fmt := fmt or LVCFMT_RIGHT;
                end;
            end;
            ListView_SetColumn(LVCerts.Handle, I, LvColumn);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TfrmPemTool1.BuildLoadName(const fname: string): string;
begin
    Result := '';
    if fname = '' then begin
        DispError('Must specify load file name');
        exit;
    end;
    if LoadDirectory.Text = '' then begin
        DispError('Must specify load directory');
        exit;
    end;
    Result := PathAddBackSlash(LoadDirectory.Text) + fname;
    if FileExists(Result) then Exit;
    DispError('Load file not found - ' + Result);
    Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TfrmPemTool1.BuildSaveName(const fname: string): string;
begin
    Result := '';
    if fname = '' then begin
        DispError('Must specify save file name');
        exit;
    end;
    if SaveDirectory.Text = '' then begin
        DispError('Must specify save directory');
        exit;
    end;
    if NOT ForceDirectories(SaveDirectory.Text) then begin
        DispError('Failed to create save directory - ' + SaveDirectory.Text);
        exit;
    end;
    Result := PathAddBackSlash(SaveDirectory.Text) + fname;
    if NOT FileExists(Result) then exit;
    if SaveAutoReplace.Checked then begin
        if DeleteFile(Result) then exit;
        DispError('Failed to delete old save file - ' + Result);
    end
    else
        DispError('File already exists and replace not specified - ' + Result);
    Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SetCertProps;
const
    digestlist: array [0..8] of TEvpDigest =
        (Digest_sha1, Digest_sha224, Digest_sha256, Digest_sha384, Digest_sha512,
        Digest_sha3_224, Digest_sha3_256, Digest_sha3_384, Digest_sha3_512);
begin
    CertCommonName.Text := Trim(CertCommonName.Text);
    with FSslCertTools do begin
        Country           := CertCountry.Text;
        State             := CertState.Text;
        Locality          := CertLocality.Text;
        Organization      := CertOrganization.Text;
        OrgUnit           := CertOrganizationalUnit.Text;
        Descr             := CertDescr.Text;
        Email             := CertEMail.Text;
        CommonName        := CertCommonName.Text;

     // make sure alt domain contains common name
        if CertAltDomains.Lines.Count > 0 then begin
            if CertAltDomains.Lines.IndexOf(CertCommonName.Text) < 0 then
                CertAltDomains.Lines.Add(CertCommonName.Text);
        end;
        AltDNSList.Assign(CertAltDomains.Lines);
        AltIpList.Assign(CertAltIPs.Lines);
  //      AltEmailList
  //      AltIssuer
  //      CRLDistPoint
  //      AuthInfoAcc
        BasicIsCA         := CertIsCA.Checked;
        BasicPathLen      := 0;
        KeyCertSign       := CertUsageCertSign.Checked;
        KeyCRLSign        := CertUsageCRLSign.Checked;
        KeyDigiSign       := CertUsageDigSign.Checked;
        KeyDataEnc        := CertUsageDataEn.Checked;
        KeyKeyEnc         := CertUsageKeyEn.Checked;
        KeyKeyAgree       := CertUsageKeyAgree.Checked;
        KeyNonRepud       := CertUsageNonRepud.Checked;
        KeyExtClient      := CertExtClient.Checked;
        KeyExtServer      := CertExtServer.Checked;
        KeyExtEmail       := CertExtEmail.Checked;
        KeyExtCode        := CertExtCodeSign.Checked;
        ExpireDays        := atoi(CertDays.Text);
//        SerialNum
        AddComments       := CertAddComment.Checked;
        CertDigest        := digestlist[CertSignHash.ItemIndex];
        SerialNum         := 0;   { V8.46 force random serial }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelCAFileClick(Sender: TObject);
begin
    OpenDlg.InitialDir := LoadDirectory.Text;
    OpenDlg.FileName := PathAddBackSlash(LoadDirectory.Text) + LoadCaBundleFile.Text;
    if OpenDlg.FileName = '' then Exit;
    if OpenDlg.Execute then begin
        LoadCaBundleFile.Text := ExtractFileName(OpenDlg.FileName);
        LoadDirectory.Text := ExtractFilePath(OpenDlg.FileName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelCertFileClick(Sender: TObject);
begin
    OpenDlg.InitialDir := LoadDirectory.Text;
    OpenDlg.FileName := PathAddBackSlash(LoadDirectory.Text) + LoadCertFile.Text;
    if OpenDlg.FileName = '' then Exit;
    if OpenDlg.Execute then begin
        LoadCertFile.Text := ExtractFileName(OpenDlg.FileName);
        LoadDirectory.Text := ExtractFilePath(OpenDlg.FileName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelCertsDBClick(Sender: TObject);
begin
//
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelCurrDirClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := CurrentCertDirEdit.Text ;
    if OpenDirDiag.Execute then
        CurrentCertDirEdit.Text := ExtractFilePath(OpenDirDiag.FileName);
    FillListView;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelLoadDirClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := LoadDirectory.Text ;
    if OpenDirDiag.Execute then
        LoadDirectory.Text := ExtractFilePath(OpenDirDiag.FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelPrvKeyFileClick(Sender: TObject);
begin
    OpenDlg.InitialDir := LoadDirectory.Text;
    OpenDlg.FileName := PathAddBackSlash(LoadDirectory.Text) + LoadPrivatetKey.Text;
    if OpenDlg.FileName = '' then Exit;
    if OpenDlg.Execute then begin
        LoadPrivatetKey.Text := ExtractFileName(OpenDlg.FileName);
        LoadDirectory.Text := ExtractFilePath(OpenDlg.FileName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelReqFileClick(Sender: TObject);
begin
    OpenDlg.InitialDir := LoadDirectory.Text;
    OpenDlg.FileName := PathAddBackSlash(LoadDirectory.Text) + LoadRequestFile.Text;
    if OpenDlg.FileName = '' then Exit;
    if OpenDlg.Execute then begin
        LoadRequestFile.Text := ExtractFileName(OpenDlg.FileName);
        LoadDirectory.Text := ExtractFilePath(OpenDlg.FileName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelSaveDirClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := SaveDirectory.Text ;
    if OpenDirDiag.Execute then
        SaveDirectory.Text := ExtractFilePath(OpenDirDiag.FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelImpDirClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DestDirEdit.Text ;
    if OpenDirDiag.Execute then
        DestDirEdit.Text := ExtractFilePath(OpenDirDiag.FileName);
    DestDirEditChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelIntersFileClick(Sender: TObject);
begin
    OpenDlg.InitialDir := LoadDirectory.Text;
    OpenDlg.FileName := PathAddBackSlash(LoadDirectory.Text) + LoadInterCerts.Text;
    if OpenDlg.FileName = '' then Exit;
    if OpenDlg.Execute then begin
        LoadInterCerts.Text := ExtractFileName(OpenDlg.FileName);
        LoadDirectory.Text := ExtractFilePath(OpenDlg.FileName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.CAFilesDirClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := CAFilesDir.Text ;
    if OpenDirDiag.Execute then
        CAFilesDir.Text := ExtractFilePath(OpenDirDiag.FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.CurrentCertDirEditChange(Sender: TObject);
begin
    FCurrentCertDir := Trim(CurrentCertDirEdit.Text);
    frmPemTool1.Caption := CaptionMain + FCurrentCertDir;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.PageControl1Change(Sender: TObject);
begin
    case PageControl1.ActivePageIndex of
        0: frmPemTool1.Caption := CaptionMain + FCurrentCertDir;
        1: frmPemTool1.Caption := CaptionMain + Trim(DestDirEdit.Text);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DestDirEditChange(Sender: TObject);
begin
    frmPemTool1.Caption := CaptionMain + Trim(DestDirEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doClearCertsClick(Sender: TObject);
begin
    FSslCertTools.ClearAll;
    FSslCAX09.ClearAll;
    DispCert;
    DispPKey;
    DispReq;
    DispInter;
    DispCACert;
    KeyPairLines.Lines.Clear;
    DHParamsLines.Lines.Clear;
    CertLinesNew.Lines.Clear;
    CertAltDomains.Lines.Clear;  // V8.49
    CertAltIPs.Lines.Clear;      // V8.49
    CertCommonName.Text := '';   // V8.49
    if LogWinOpen then frmPemTool2.Memo1.Lines.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCreateBundleClick(Sender: TObject);
var
    certfname, pkeyfname, interfname, savefname: string;
begin
    doClearCertsClick(self);
    certfname := BuildLoadName(LoadCertFile.Text);
    if certfname = '' then Exit;
    pkeyfname := BuildLoadName(LoadPrivatetKey.Text);
    if pkeyfname = '' then Exit;
    interfname := BuildLoadName(LoadInterCerts.Text);   // optional
    savefname := BuildSaveName(SaveCertPem.Text);
    if savefname = '' then Exit;
    try
        FSslCertTools.CreateCertBundle(certfname, pkeyfname, interfname,
                  LoadCertPW.Text, savefname, SaveCertPW.Text,
                                     TSslPrivKeyCipher(KeyEncrypt.ItemIndex));
        DispError('Saved certificate bundle OK - ' + savefname);
        DispCert;
        DispPKey;
        DispInter;
    except
        on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCheckBundleSelfClick(Sender: TObject);
var
    CertStr, ErrStr: string;
    ValRes: TChainResult;
begin
    if NOT FSslCertTools.IsCertLoaded then begin
        DispError('Must load or create a certificate first');
        exit;
    end;
    if NOT FSslCertTools.IsPKeyLoaded then begin
        DispError('Must load or create a private key first');
        exit;
    end;
    FSslCertTools.LoadCATrustFromString(sslRootCACertsBundle);  { trusted root }
    ValRes := FSslCertTools.ValidateCertChain('', CertStr, ErrStr);   
    if ValRes = chainOK then
        ErrStr := 'Chain Validated OK'
    else if ValRes = chainWarn then
        ErrStr := 'Chain Warning - ' + ErrStr
    else
        ErrStr := 'Chain Failed - ' + ErrStr;
    DispError(ErrStr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCheckBundleWinClick(Sender: TObject);
var
    CertChain: TX509List;
    ChainVerifyResult: LongWord;
    VerifyInfo: String;
begin
    if NOT FSslCertTools.IsCertLoaded then begin
        DispError('Must load or create a certificate first');
        exit;
    end;
    if SaveInterCerts.Checked then begin
        if NOT FSslCertTools.IsInterLoaded then begin
            DispError('Must load intermediate certificates first');
            exit;
        end;
    end;
    if FSslCertTools.IsPKeyLoaded then begin
        if NOT FSslCertTools.CheckCertAndPKey then begin
            DispError('Mismatch certificate and private key');
            exit;
        end;
    end;

  { get intermediate chain }
    CertChain := TX509List.Create(nil);
    FSslCertTools.GetIntersList (CertChain);

  { pending use ValidateCertChain when it's written }

  { start Windows certificate engine }
    if not Assigned (MsCertChainEngine) then
        MsCertChainEngine := TMsCertChainEngine.Create;

  { see if checking revoocation, very slow!!!! }
    MsCertChainEngine.VerifyOptions := []; // [mvoRevocationCheckChainExcludeRoot];

  { Pass the certificate and the chain certificates to the engine      }
    MsCertChainEngine.VerifyCert (FSslCertTools, CertChain, ChainVerifyResult, True);

   { The MsChainVerifyErrorToStr function works on chain error codes     }
    VerifyInfo := MsChainVerifyErrorToStr (ChainVerifyResult);
    DispError('Bundle check result - ' + VerifyInfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCreateCACertClick(Sender: TObject);
begin
    SetCertProps;
    doCreateCACert.Enabled := false;
    try
        try
            if NOT FSslCAX09.IsCertLoaded then begin    { V8.50 }
                DispError('Must specify CA ertificate first');
                exit;
            end;
            FSslCertTools.X509CA := FSslCAX09.X509;     { V8.50 }
            FSslCertTools.PrivKeyCA := FSslCAX09.PrivateKey;   { V8.50 }
            DispCACert;
            FSslCertTools.DoSignCertReq(NewCertCopyExt.Checked);
            DispError('Created certificate from request signed by CA certificate OK');
            DispCert;
            CertLinesNew.Lines.Text := FSslCertTools.SaveCertToText(true);
        except
            on E:Exception do
                    DispError(E.Message);
        end;
    finally
        doCreateCACert.Enabled := true;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCreateReqCertClick(Sender: TObject);
begin
    SetCertProps;
    doCreateReqCert.Enabled := false;
    try
        try
            FSslCertTools.X509Req := Nil;
            DispCert;
            CertLinesNew.Lines.Clear;
            FSslCertTools.DoCertReqOld;
            DispError('Created certificate request from existing certificate OK');
            DispReq;
            CertLinesNew.Lines.Text := FSslCertTools.SaveReqToText(true);
        except
            on E:Exception do
                    DispError(E.Message);
        end;
    finally
        doCreateReqCert.Enabled := true;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCreateReqPropsClick(Sender: TObject);
begin
    SetCertProps;
    doCreateReqProps.Enabled := false;
    try
        try
            FSslCertTools.X509Req := Nil;
            DispCert;
            CertLinesNew.Lines.Clear;
            FSslCertTools.DoCertReqProps;
            DispError('Created certificate request from properties OK');
            DispReq;
            CertLinesNew.Lines.Text := FSslCertTools.SaveReqToText(true);
        except
            on E:Exception do
                    DispError(E.Message);
        end;
    finally
        doCreateReqProps.Enabled := true;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCreateSelfCertClick(Sender: TObject);
begin
    SetCertProps;
    doCreateSelfCert.Enabled := false;
    try
        try
            FSslCertTools.X509 := Nil;
            DispCert;
            CertLinesNew.Lines.Clear;
            FSslCertTools.DoSelfSignCert;
            DispError('Created self signed certificate OK');
            DispCert;
            CertLinesNew.Lines.Text := FSslCertTools.SaveCertToText(true);
        except
            on E:Exception do
                    DispError(E.Message);
        end;
    finally
        doCreateSelfCert.Enabled := true;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doDHParamsClick(Sender: TObject);
var
    fname: string;
const
    bittable: array [0..4] of integer = (768, 1024, 2048, 4096, 8192);
begin
    fname := BuildSaveName(DHParamFile.Text);
    if fname = '' then Exit;
    doDHParams.Enabled := false;
    try
        try
            DHParamsLines.Lines.Text := 'Generating DHParams, this may take many minutes';
            DispError(DHParamsLines.Lines.Text);
            StartTickCount := GetTickCount;
            DHParamsLines.Lines.Text := FSslCertTools.DoDHParams(FName, bittable[DHParamSize.ItemIndex]);
            DispError('Generated DHParams OK - ' + fname + ', took ' + IntToStr(IcsCalcTickDiff
                                            (StartTickCount, GetTickCount) div 1000) + ' secs');
        except
            on E:Exception do
                    DispError(E.Message);
        end;
    finally
        doDHParams.Enabled := true;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doGenKeyClick(Sender: TObject);
begin
    FSslCertTools.PrivKeyType := TSslPrivKeyType(KeyType.ItemIndex);
    doGenKey.Enabled := false;
    try
        try
            FSslCertTools.PrivateKey := Nil;
            KeyPairLines.Lines.Clear; 
            DispError('Generating private and public key pair, please wait');
            StartTickCount := GetTickCount;
            FSslCertTools.DoKeyPair;
            KeyPairLines.Lines.Text := FSslCertTools.SavePKeyToText
                        (SaveCertPW.Text, TSslPrivKeyCipher(KeyEncrypt.ItemIndex));
            DispError('Generated private and public key pair OK');
            DispPKey;
        except
            on E:Exception do
                DispError(E.Message);
        end;
    finally
        doGenKey.Enabled := true;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DispCert;
var
    info: String;
begin
    info := 'Certificate: ';
    if NOT FSslCertTools.IsCertLoaded then
        LabelStateCert.Caption := info + 'None'
    else begin
        ShowLogWindow;
        with FSslCertTools do begin
            LabelStateCert.Caption := info + CertInfo(False);
            frmPemTool2.Memo1.Lines.Add (ListCertDetail(FSslCertTools));
            frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                                       'Raw Certificate' + #13#10 + GetRawText;
            frmPemTool2.Memo1.Lines.Add ('');
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DispPKey;
begin
    LabelStatePrivKey.Caption := 'Private Key: ';
    if NOT FSslCertTools.IsPKeyLoaded then
        LabelStatePrivKey.Caption := LabelStatePrivKey.Caption + 'None'
    else begin
        ShowLogWindow;
        with FSslCertTools do begin
            LabelStatePrivKey.Caption := LabelStatePrivKey.Caption +
                                                    #13#10 + PrivateKeyInfo;
            frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                'Raw Private Key: ' + PrivateKeyInfo + #13#10 + GetPKeyRawText;
            frmPemTool2.Memo1.Lines.Add ('');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DispReq;
begin
    LabelStateReq.Caption := 'Certificate Request: ';
    if NOT FSslCertTools.IsReqLoaded then
        LabelStateReq.Caption := LabelStateReq.Caption + 'None'
    else begin
        ShowLogWindow;
        with FSslCertTools do begin
            LabelStateReq.Caption := LabelStateReq.Caption + ReqCertInfo;
            frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                                                       LabelStateReq.Caption;
            frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                          'Raw Certificate Request' + #13#10 + GetRequestRawText;
            frmPemTool2.Memo1.Lines.Add ('');
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DispInter;
var
    info: String;
begin
    info := 'Intermediate Certificates: ';
    if NOT FSslCertTools.IsInterLoaded then
        LabelInters.Caption := info + 'None'
    else begin
        ShowLogWindow;
        LabelInters.Caption := info + FSslCertTools.ListInters;
        frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                                                       LabelInters.Caption;
        frmPemTool2.Memo1.Lines.Add ('');
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doLoadBase64Click(Sender: TObject);
begin
    if (Pos ('-BEGIN CERTIFICATE-', CertLinesOld.Text) = 0) then begin
        DispError('Must paste base64 certificate into control');
        exit;
    end;
    try
        FSslCertTools.LoadFromText(CertLinesOld.Text, true, LoadCertPW.Text);
        DispCert;
        DispPKey;
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DispCACert;
begin
    LabelStateCACert.Caption := 'CA Certificate: ';
    if NOT FSslCAX09.IsCertLoaded then                   { V8.50 }
         LabelStateCACert.Caption := 'None'
    else
      { assume cert still loaded before being copied to CA }
        LabelStateCACert.Caption := LabelStateCACert.Caption +
                                                FSslCAX09.CertInfo(True);     { V8.50 } 
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doLoadCABundleClick(Sender: TObject);     { V8.50 }
var
    fname: string;
begin
    fname := BuildLoadName(LoadCaBundleFile.Text);
    if fname = '' then Exit;
    try
        FSslCAX09.LoadFromFile(fname, croTry, croNo, LoadCertPW.Text);
        if (Pos ('CA=TRUE', FSslCAX09.BasicConstraints) = 0) then begin
            DispError('Certificate does not allow CA signing');
            FSslCAX09.ClearAll;
            exit;
        end;
        if NOT FSslCAX09.CheckCertAndPKey then begin
            DispError('Need matching CA certificate amd private key');
            FSslCAX09.ClearAll;
            exit;
        end;
        DispError('Loaded CA cerfificate OK - ' + fname);
        DispCACert;
    except
        on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doLoadCertClick(Sender: TObject);
var
    fname: string;
begin
    fname := BuildLoadName(LoadCertFile.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.LoadFromFile(fname, GetCertReadOpt(LoadCertPrivKey.Checked),
                           GetCertReadOpt(LoadCertInters.Checked), LoadCertPW.Text);
        DispError('Loaded cerfificate OK - ' + fname);
        DispCert;
        if LoadCertPrivKey.Checked then begin
            DispError('Loaded cerfificate and key OK - ' + fname);
            DispPKey;
        end;
        if LoadCertInters.Checked then
            DispInter;
    except
        on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doLoadIntersClick(Sender: TObject);
var
    fname: string;
begin
    fname := BuildLoadName(LoadInterCerts.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.LoadIntersFromPemFile(fname);
        DispError('Loaded intermediates OK - ' + fname);
        DispInter;
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doLoadPrvKeyClick(Sender: TObject);
var
    fname: string;
begin
    fname := BuildLoadName(LoadPrivatetKey.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.PrivateKeyLoadFromPemFile(fname, LoadCertPW.Text);
        DispError('Loaded private key OK - ' + fname);
        DispPKey;
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doLoadReqClick(Sender: TObject);
var
    fname: string;
begin
    fname := BuildLoadName(LoadRequestFile.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.LoadReqFromFile(fname);
        DispError('Loaded request OK - ' + fname);
        DispReq;
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSaveCertDerClick(Sender: TObject);
var
    fname: string;
begin
    if NOT FSslCertTools.IsCertLoaded then begin
        DispError('Must load or create a certificate first');
        exit;
    end;
    fname := BuildSaveName(SaveCertDer.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.SaveToDERFile(fname);
        DispError('Saved certificate OK - ' + fname);
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSaveCertPemClick(Sender: TObject);
var
    fname: string;
begin
    if NOT FSslCertTools.IsCertLoaded then begin
        DispError('Must load or create a certificate first');
        exit;
    end;
    if SavePrivateKey.Checked then begin
        if NOT FSslCertTools.IsPKeyLoaded then begin
            DispError('Must load or create a private key first');
            exit;
        end;
    end;
    if SaveInterCerts.Checked then begin
        if NOT FSslCertTools.IsInterLoaded then begin
            DispError('Must load intermediate certificates first');
            exit;
        end;
    end;
    fname := BuildSaveName(SaveCertPem.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.SaveToPemFile(fname, SavePrivateKey.Checked,
            CertAddComment.Checked, SaveInterCerts.Checked, SaveCertPW.Text,
                                        TSslPrivKeyCipher(KeyEncrypt.ItemIndex));
        DispError('Saved certificate OK - ' + fname);
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSavePkcs12Click(Sender: TObject);
var
    fname: string;
begin
    if NOT FSslCertTools.IsCertLoaded then begin
        DispError('Must load or create a certificate first');
        exit;
    end;
    if NOT FSslCertTools.IsPKeyLoaded then begin
        DispError('Must load or create a private key first');
        exit;
    end;
    if SaveInterCerts.Checked then begin
        if NOT FSslCertTools.IsInterLoaded then begin
            DispError('Must load intermediate certificates first');
            exit;
        end;
    end;
    fname := BuildSaveName(SavePkcs12File.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.SaveToP12File(fname, SaveCertPW.Text,
                     SaveInterCerts.Checked, TSslPrivKeyCipher(KeyEncrypt.ItemIndex));
        DispError('Saved certificate OK - ' + fname);
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSavePkcs7CertClick(Sender: TObject);
var
    fname: string;
begin
    if NOT FSslCertTools.IsCertLoaded then begin
        DispError('Must load or create a certificate first');
        exit;
    end;
    if SaveInterCerts.Checked then begin
        if NOT FSslCertTools.IsInterLoaded then begin
            DispError('Must load intermediate certificates first');
            exit;
        end;
    end;
    fname := BuildSaveName(SavePkcs7File.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.SaveToP7BFile(fname, SaveInterCerts.Checked);
        DispError('Saved certificate OK - ' + fname);
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSavePrivKeyClick(Sender: TObject);
var
    fname: string;
begin
    if SavePrivateKey.Checked then begin
        if NOT FSslCertTools.IsPKeyLoaded then begin
            DispError('Must load or create a private key first');
            exit;
        end;
    end;
    fname := BuildSaveName(SavePrvFileFile.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.PrivateKeySaveToPemFile(fname, SaveCertPW.Text,
                                         TSslPrivKeyCipher(KeyEncrypt.ItemIndex));
        DispError('Saved perivate key OK - ' + fname);
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSavePubKeyClick(Sender: TObject);
var
    fname: string;
begin
    if SavePrivateKey.Checked then begin
        if NOT FSslCertTools.IsPKeyLoaded then begin
            DispError('Must load or create a private key first');
            exit;
        end;
    end;
    fname := BuildSaveName(SavePubKeyFile.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.PublicKeySaveToPemFile(fname);
        DispError('Saved public key OK - ' + fname);
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSaveReqCertClick(Sender: TObject);
var
    fname: string;
begin
    if NOT FSslCertTools.IsReqLoaded then begin
        DispError('Must load or create a certificate request first');
        exit;
    end;
    fname := BuildSaveName(SaveReqCertFile.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.SaveReqToFile(fname, CertAddComment.Checked);
        DispError('Saved certificate request OK - ' + fname);
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnRefreshClick(Sender: TObject);
begin
    FillListView;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnDeleteCertClick(Sender: TObject);
var
    ListItem : TListItem;
    FileName : String;
begin
    with LvCerts do begin
        if Assigned(Selected) then begin
            ListItem := Items[Selected.Index];
            FileName := ListItem.SubItems[3];
            if FileName <> '' then
                FileName := PathAddBackSlash(FCurrentCertDir) + FileName;
            if FileExists(FileName) then
                if MessageDlg('Delete Certificate ''' + ListItem.Caption + ''','
                            + #13#10
                            + 'file ''' + FileName + ''' now?',
                             mtWarning, [mbYes, mbNo], 0) <> mrYes then
                    Exit;
            if DeleteFile(FileName) then
                FillListView;
        end
        else
            MessageDlg('No item selected', mtError, [mbOK], 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.38 check digital signature on EXE and DLL files }
procedure TfrmPemTool1.btnCheckSignedClick(Sender: TObject);
var
    FileName, Info: String;
    RetCode: Integer;
begin
    OpenDlg.InitialDir := VerifyDir;
    OpenDlg.Filter     := 'Executable Files|*.exe;*.dll;*.ocx|All Files *.*|*.*';
    OpenDlg.Title      := 'Select executable file to check Digital Signature';
    if OpenDlg.Execute then
    begin
        FileName := OpenDlg.FileName;
        VerifyDir := ExtractFileDir(FileName);
        if (FileName = '') or not FileExists(FileName) then
            Exit;
        RetCode := IcsVerifyTrust (FileName, False, True, Info);
        MessageDlg('Checked Digital Signature for ' + FileName + #13#10#13#10 +
                   'Result: ' + Info + #13#10 +
                   'RetCode=' + IntToHex (RetCode, 8),
                   mtInformation, [mbOK], 0);
    end;
 end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnCopyCertClick(Sender: TObject);
var
    ListItem    : TListItem;
    FileName    : String;
    NewFileName : String;
    ClickedOK   : Boolean;
begin
    ClickedOk := FALSE;
    with LvCerts do begin
        if Assigned(Selected) then begin
            ListItem := Items[Selected.Index];
            FileName := ListItem.SubItems[3];
            if FileName <> '' then
                FileName := PathAddBackSlash(FCurrentCertDir) + FileName;
            if FileExists(FileName) then begin
                NewFileName := FileName;
                ClickedOK := InputQuery(ListItem.Caption,
                                        'Copy to: ', NewFileName);
            end;
            if ClickedOK and (CompareText(NewFileName, FileName) <> 0) then
                if CopyFile(PChar(FileName), PChar(NewFileName), TRUE) then
                    FillListView
                else
                    MessageDlg('''' + FileName + ''' ' + 'copy failed!',
                    mtError, [mbOK], 0);
        end
        else
            MessageDlg('No item selected', mtError, [mbOK], 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.About1Click(Sender: TObject);
begin
   ShowMessage(PemToolName + #13#10 +
               CopyRight + ' ' + PemToolDate + #13#10 +
               Trim(OverbyteIcsWSocket.CopyRight)  + #13#10 +   { V8.53 }
               'SSL Version: ' + OpenSslVersion + #13#10 +
               'Dir: ' + GLIBEAY_DLL_FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnImportClick(Sender: TObject);
var
     hSystemStore    : HCERTSTORE;
     pCertContext    : PCCERT_CONTEXT;
     pwszSystemName  : LPCWSTR;
     X               : TX509Base;
     Subject_Hash    : Cardinal;
     BundleBio       : PBIO;          // added
     FileName        : String;
     Path            : String;
     BundleFilename  : String;        // added
     BundlePath      : String;        // added
     Count           : Integer;
     xTmp            : PX509;
begin
    LoadSsl; // Need to load the libraries here since it may be required for the call of f_d2i_X509()
    Count := 0;
    pCertContext := nil;
    BundleBio    := nil;
    Path  := Trim(DestDirEdit.Text);

    if (Path = '') or (not DirectoryExists(Path)) then begin
        ForceDirectories(Path); // Angus
      //  ShowMessage('Invalid destination ''' + Path + '''!');
      //  Exit;
    end;

    if CheckBoxWarnDestNotEmpty.Checked then
        if not isDirEmpty(Path) then
            if MessageDlg('Directory ''' + Path + ''' is not empty. Continue?',
                          mtWarning,
                          [mbYes, mbNo],
                          0) <> mrYes then
                Exit;

    if CheckBoxEmptyDestDir.Checked then begin
        if MessageDlg('Any file in destination ''' + Path
                     + ''' will be deleted. Continue?',
                      mtWarning,
                      [mbYes, mbNo],
                      0) <> mrYes then
            Exit;
        EmptyDirectory(Path);
    end;

    case ComboBoxStoreType.ItemIndex of
        0 : pwszSystemName := 'CA';
        1 : pwszSystemName := 'ROOT';
        2 : pwszSystemName := 'MY';
        3 : pwszSystemName := 'TRUST';        { V8.41 }
        4 : pwszSystemName := 'ADDRESSBOOK';  { V8.41 }
      else
        pwszSystemName := nil;
    end;

    { Open the Windows certificate system store }
    hSystemStore := CertOpenStore(CERT_STORE_PROV_SYSTEM,
                                  0,
                                  0,
                                  CERT_SYSTEM_STORE_CURRENT_USER
                                  or CERT_STORE_READONLY_FLAG,
                                  pwszSystemName);

    if hSystemStore <> nil  then
        ShowMessage('Opened the '''
                   + ComboBoxStoreType.Items[ComboBoxStoreType.ItemIndex]
                   + ''' system store.')
    else begin
        ShowMessage('Could not open the '''
                   + ComboBoxStoreType.Items[ComboBoxStoreType.ItemIndex]
                   + ''' system store.');
        Exit;
    end;

    if CheckBoxWriteToBundle.Checked then begin
         BundlePath:= IncludeTrailingPathDelimiter(Path) + 'Bundled certs';
         ForceDirectories(BundlePath);
         BundlePath:= IncludeTrailingPathDelimiter(BundlePath);
         case ComboBoxStoreType.ItemIndex of
             0 : BundleFilename := BundlePath + 'CaCertsBundle.pem';
             1 : BundleFilename := BundlePath + 'RootCaCertsBundle.pem';
             2 : BundleFilename := BundlePath + 'MyCertsBundle.pem';
             3 : BundleFilename := BundlePath + 'TrustCertsBundle.pem';  { V8.41 }
             4 : BundleFilename := BundlePath + 'EmailCertsBundle.pem';  { V8.41 }
         end;
       { opens text file, adds CR to LF }
         BundleBio := f_BIO_new_file(Pointer(AnsiString(BundleFilename)), PAnsiChar('w+'));
     end;

    { Enum all the certs in the store and store them in PEM format }
    pCertContext := CertEnumCertificatesInStore(hSystemStore, pCertContext);
    X := TX509Base.Create(nil);
    try
        while pCertContext <> nil do begin
            xTmp := f_d2i_X509(nil, @pCertContext.pbCertEncoded, pCertContext.cbCertEncoded);
            if Assigned(xTmp) then begin
                X.X509 := xTmp;
                f_X509_free(xTmp);
                Subject_Hash := f_X509_subject_name_hash(X.X509);
                FileName := PathAddBackSlash(Path) + IntToHex(Subject_Hash, 8) + '.0';
                if not CheckBoxOverwriteExisting.Checked then
                    if FileExists(FileName) then
                        FileName := FindPemFileName(FileName);
                X.SaveToPemFile(FileName, False, CheckBoxComment.Checked);  { V8.41 does it all now }
                Inc(Count);
                  // save to bundle also
                if (Assigned(BundleBio)) and CheckBoxWriteToBundle.Checked then begin
                    X.WriteCertToBio(BundleBio, CheckBoxComment.Checked, BundleFilename);   { V8.41 does it all now }
                    X.WriteStrBio(BundleBio, #10#10);  { blank lines between certs }
                end;
            end;
            pCertContext := CertEnumCertificatesInStore(hSystemStore, pCertContext);
        end;
        ShowMessage(IntToStr(Count) + ' Certificates exported.');
    finally
        if Assigned(BundleBio) then
            f_BIO_free(BundleBio);
        X.Free;
        UnloadSsl;
        if pCertContext <> nil then
            CertFreeCertificateContext(pCertContext);
        if hSystemStore <> nil then
            CertCloseStore(hSystemStore, CERT_CLOSE_STORE_CHECK_FLAG);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnImportPemFileClick(Sender: TObject);
var
    X               : TX509Base;
    Subject_Hash    : Cardinal;
    FileName        : String;
begin
    OpenDlg.InitialDir := DestDirEdit.Text;
    OpenDlg.Filter     := 'PEM Certs *.pem|*.pem|All Files *.*|*.*';
    if OpenDlg.Execute then
        FileName := OpenDlg.FileName;
    if (FileName = '') or not FileExists(FileName) then
        raise Exception.Create('FileName empty or file doesn''t exist');
    X := TX509Base.Create(nil);
    try
        X.LoadFromFile(FileName, croNo, croNo, CertPassword.Text);   { V8.40 was pemfile }
        Subject_Hash := f_X509_subject_name_hash(X.X509);
        FileName     := PathAddBackSlash(Trim(DestDirEdit.Text))
                        + IntToHex(Subject_Hash, 8) + '.0';

        if FileExists(FileName) then
            if MessageDlg('A certificate with the same subject already '
                           + 'exists in folder ''' + Trim(DestDirEdit.Text)
                           + ''' . Change file extension?' + #13#10
                           +'Click ''Yes'' to change file extension.' + #13#10
                           +'Click ''No'' to overwrite existing file.' + #13#10
                           +'Click ''Cancel'' to abort.',
                           mtWarning, [mbYes, mbNo, mbCancel], 0) = mrYES then

                FileName := FindPemFileName(FileName)
            else
                Exit;

            X.SaveToPemFile(FileName);
            ShowMessage('Certificate has been stored to ''' + FileName + '''');
    finally
        X.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FindPemFileName(const FileName: String): String;
var
    I         : Integer;
    FFileName : String;
    FExt      : String;
begin
    { If more than one CA certificate with the same name hash value exist, the }
    { extension must be different (e.g. 9d66eef0.0, 9d66eef0.1 etc). }
    FExt      := ExtractFileExt(FileName);
    FFileName := Copy(FileName, 1, length(FileName) -Length(FExt));
    I         := StrToInt(Copy(FExt, 2, MaxInt));
    Result    := FFileName + '.' + IntToStr(I);
    while FileExists(Result) do
    begin
       Inc(I);
       Result := FFileName + '.' + IntToStr(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DirectoryExists(const Name: string): Boolean; {from D5 FileCtrl.pas}
var
    Code: Integer;
begin
 {$R-}
    Code := GetFileAttributes(PChar(Name));
    Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
 {$R+}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsDirEmpty(const Path: String): Boolean;
var
    SRec : TSearchRec;
begin
    Result := FindFirst(PathAddBackSlash(Path) + '*.*',
                        FaAnyFile - faDirectory,
                        SRec) <> 0;
    if not Result then
        FindClose(SRec);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure EmptyDirectory(Path: String);
var
    SRec : TSearchRec;
begin
    Path := PathAddBackSlash(Path);
    if FindFirst(Path + '*.*', faAnyFile - faDirectory, SRec) = 0 then begin
        try
            DeleteFile(Path + SRec.Name);
            while FindNext(SRec) = 0 do
                DeleteFile(Path + SRec.Name);
        finally
            FindClose(SRec);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function PathAddBackSlash(const Path: String): String;
begin
    Result := Path;
    if Path[Length(Path)] <> '\' then
         Result := Path + '\';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMFileExitClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasCreateSelfSignedCertClick(Sender: TObject);
begin
    frmPemTool3.ToDo := tdCreateSelfSignedCert;
    frmPemTool3.Showmodal;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasCreateCertRequestClick(Sender: TObject);
begin
    frmPemTool3.ToDo := tdCreateCertRequest;
    frmPemTool3.Showmodal;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasEncryptStringRSAClick(Sender: TObject);
var
    Cert        : TX509Base;
    PemFileName : String;
    OldTitle    : String;
    Password    : String;
    S           : String;
begin
    S := 'This the plain text This the plain text This the plain text';
    if not InputQuery(Application.Title, 'Enter a string to encrypt:', S) then
        Exit; //***
    OldTitle := OpenDlg.Title;
    OpenDlg.Title := 'Select a PEM file containing both private and public key!';
    try
        OpenDlg.InitialDir := ExtractFileDir(Application.ExeName);
        if FileExists(OpenDlg.InitialDir + '\client.pem') then
            OpenDlg.FileName := 'client.pem';
        if OpenDlg.Execute then begin
            Password := 'password';
            if not InputQuery(Application.Title, 'Private key password:', Password) then
                Exit;
            PemFileName := OpenDlg.FileName;
        end
        else
            Exit; //***
    finally
        OpenDlg.Title := OldTitle;
    end;
    { We encrypt using the public key. }
    { Could also load a PEM file containing both private and public key. }
    Cert := TX509Base.Create(nil);
    try
        { Load a certificate (public key) from PEM file, private key must not exist }
        Cert.LoadFromPemFile(PemFileName);
        { Encrypted string is Base64 encoded }
        S := String(StrEncRsa(Cert.X509PublicKey, AnsiString(S), TRUE));   { V8.52 was PublicKey }
        ShowMessage('RSA encryted and Base64 encoded:'#13#10 + S);
    finally
        Cert.Free;
    end;
    { Decrypt using the private key. }
    Cert := TX509Base.Create(nil);
    try
        { Load a private key from PEM file }
        Cert.PrivateKeyLoadFromPemFile(PemFileName, Password);
        { Decrypt the Base64 encoded string }
        S := String(StrDecRsa(Cert.PrivateKey, AnsiString(S), TRUE));
        ShowMessage('Back to plain text again:'#13#10 + S);
    finally
        Cert.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasEncryptStringBlowfishClick(Sender: TObject);
var
    IV : TIVector;
    S  : AnsiString;
begin
  { if not LibeayExLoaded then
        LoadLibeayEx;  }
    S := 'This the plain text This the plain text This the plain text';
    f_RAND_bytes(@IV, SizeOf(IV));
    S := StrEncBF(S, 'password', @IV, cklDefault, TRUE);
    ShowMessage(String(S));
    S := StrDecBF(S, 'password', @IV, cklDefault, TRUE);
    ShowMessage(String(S));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasEncryptStreamBlowfishClick(Sender: TObject);
var
    S : AnsiString;
    Src, Dest : TStream;
    EncCtx, DecCtx : TCiphContext;
begin
    //SetLength(S, 2099);
    //FillChar(S[1], 2099, 'x');
    { Write NULLs, required!! }
    FillChar(EncCtx, SizeOf(EncCtx), #0);
    FillChar(DecCtx, SizeOf(DecCtx), #0);
    { We use one context for encryption and another one for decryption,    }
    { IV will be calculated from the password, key size default = 128-bits }
    CiphInitialize(EncCtx, 'password', nil, nil, {ctBfEcb ctBfOfb} ctBfCbc, cklDefault, True);
    CiphInitialize(DecCtx, 'password', nil, nil, {ctBfEcb ctBfOfb} ctBfCbc, cklDefault, False);
    try
        S := 'This the plain text This the plain text This the plain text';
        Src  := TMemoryStream.Create;
        Dest := TMemoryStream.Create;
        try
            { Populate the source stream }
            Src.WriteBuffer(S[1], Length(S));
            { Encrytion takes place here }
            StreamEncrypt(Src, Dest, 1024, EncCtx, False);
            { Just to display cipher text }
            SetLength(S, Dest.Size);
            Dest.Position := 0;
            Dest.Read(S[1], Length(S));
            ShowMessage(String(Base64Encode(S)));
            { Decrytion takes place here }
            StreamDecrypt(Dest, Src, 1024, DecCtx, False);
            { Just to display decrypted plain result }
            SetLength(S, Src.Size);
            Src.Position := 0;
            Src.Read(S[1], Length(S));
            ShowMessage(String(S));
        finally
            Src.Free;
            Dest.Free;
        end;
    finally
        CiphFinalize(EncCtx);
        CiphFinalize(DecCtx);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Progress(Obj: TObject; Count: Int64; var Cancel: Boolean);
begin
    TProgressBar(Obj).Position := Count;
    Application.ProcessMessages;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasEncryptFileBlowfishClick(Sender: TObject);
var
    Src, Dest   : TStream;
    EncCtx      : TCiphContext;
    OldTitle    : String;
    SrcFileName : String;
    DestFileName: String;
    Password    : String;
    OldFilter   : String;
begin
    OldFilter := OpenDlg.Filter;
    OldTitle := OpenDlg.Title;
    OpenDlg.Filter := 'All Files *.*|*.*';
    OpenDlg.Title := 'Select a file to encrypt!';
    try
        OpenDlg.InitialDir := ExtractFileDir(Application.ExeName);
        if OpenDlg.Execute then begin
            SrcFileName := OpenDlg.FileName;
            DestFileName := ChangeFileExt(SrcFileName, '_ENC' + ExtractFileExt(SrcFileName));
            if MessageDlg('Save encrypted file as "' + DestFileName + '" ?',
                          mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
                Exit; //***
        end
        else
            Exit; //***
        Password := 'password';
        if not InputQuery(Application.Title, 'Password:', Password) then
            Exit; //***
    finally
        OpenDlg.Filter := OldFilter;
        OpenDlg.Title := OldTitle;
    end;
    FillChar(EncCtx, SizeOf(EncCtx), #0);

    CiphInitialize(EncCtx, AnsiString(Password), nil, nil, ctBfCbc, cklDefault, True);
    try
        Src  := TFileStream.Create(SrcFileName, fmOpenRead or fmShareDenyWrite);
        Dest := TFileStream.Create(DestFileName, fmCreate);
        try
            ProgressBar1.Max := Src.Size;
            ProgressBar1.Position := 0;
            ProgressBar1.Visible := TRUE;
            StreamEncrypt(Src, Dest, 1024 * 4, EncCtx, False, ProgressBar1, Progress);
            ShowMessage('File encrypted');
        finally
            Src.Free;
            Dest.Free;
        end;
    finally
        CiphFinalize(EncCtx);
        ProgressBar1.Visible := False;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasDecryptFileBlowfishClick(Sender: TObject);
var
    Src, Dest : TStream;
    DecCtx : TCiphContext;
    OldTitle    : String;
    SrcFileName : String;
    DestFileName: String;
    Password    : String;
    OldFilter   : String;
begin
    OldFilter := OpenDlg.Filter;
    OldTitle := OpenDlg.Title;
    OpenDlg.Filter := 'All Files *.*|*.*';
    OpenDlg.Title := 'Select a file to decrypt!';
    try
        OpenDlg.InitialDir := ExtractFileDir(Application.ExeName);
        if OpenDlg.Execute then begin
            SrcFileName := OpenDlg.FileName;
            DestFileName := ChangeFileExt(SrcFileName, '_DEC' + ExtractFileExt(SrcFileName));
            if MessageDlg('Save decrypted file as "' + DestFileName + '" ?',
                          mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
                Exit; //***
        end
        else
            Exit; //***
        Password := 'password';
        if not InputQuery(Application.Title, 'Password:', Password) then
            Exit; //***
    finally
        OpenDlg.Filter := OldFilter;
        OpenDlg.Title := OldTitle;
    end;
    FillChar(DecCtx, SizeOf(DecCtx), #0);
    CiphInitialize(DecCtx, AnsiString(Password), nil, nil, ctBfCbc, cklDefault, False);
    try
        Src  := TFileStream.Create(SrcFileName, fmOpenRead or fmShareDenyWrite);
        Dest := TFileStream.Create(DestFileName, fmCreate);
        try
            ProgressBar1.Max := Src.Size;
            ProgressBar1.Position := 0;
            ProgressBar1.Visible := TRUE;
            StreamDecrypt(Src, Dest, 1024 * 4, DecCtx, False, ProgressBar1, Progress);
            ShowMessage('File decrypted');
        finally
            Src.Free;
            Dest.Free;
        end;
    finally
        CiphFinalize(DecCtx);
        ProgressBar1.Visible := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
