unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls;

type
  TwndMain = class(TForm)
    Label1: TLabel;
    eFile: TEdit;
    bOpen: TButton;
    od: TOpenDialog;
    Memo: TMemo;
    bView: TButton;
    bProps: TButton;
    Image: TImage;
    procedure bOpenClick(Sender: TObject);
    procedure bViewClick(Sender: TObject);
    procedure bPropsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  wndMain: TwndMain;

implementation

uses MiTeC_Cert, MiTeC_Routines, MiTeC_Dialogs;

{$R *.dfm}

procedure TwndMain.bOpenClick(Sender: TObject);
var
  p,e: string;
  ci: TCertInfo;
  vi: TVersionInfo;
  r: integer;
begin
  if not od.Execute then
    Exit;
  bProps.Enabled:=True;
  Memo.Clear;
  bView.Enabled:=False;
  eFile.Text:=od.Filename;
  e:=LowerCase(ExtractFileExt(od.Filename));
  if Pos(e,'.exe.dll.sys.ocx.msi')>0 then begin
    r:=VerifyFile(od.Filename,ci.Subject,ci.ValidFrom);
    if (r=0) or (ci.Subject<>'') then begin
      Memo.Lines.Add('Publisher: '+ci.Subject);
      if ci.ValidFrom>0 then
        Memo.Lines.Add('Signing date: '+DateTimeToStr(ci.ValidFrom));
      bView.Enabled:=True;
      if r<>0 then
        Memo.Lines.Add('ERROR: '+GetCertErrorText(r));
    end else
      Memo.Lines.Add('File is not signed');
    GetFileVerInfo(od.Filename,vi);
    Memo.Lines.Add('Company: '+vi.CompanyName);
    Memo.Lines.Add('Description: '+vi.Description);
    Memo.Lines.Add('Product: '+vi.ProductName);
    Memo.Lines.Add('Prod version: '+vi.ProductVersion);
    Memo.Lines.Add('File version: '+vi.FileVersion);
    Memo.Lines.Add('Copyright: '+vi.Copyright);
  end else if Pos(e,'.pem.p7c.p7b.cer.crt.der')>0 then begin
    if GetCertInfo(od.Filename,ci) then begin
      Memo.Lines.Add('Version: '+IntToStr(ci.Version));
      Memo.Lines.Add('Issuer: '+ci.Issuer);
      Memo.Lines.Add('Subject: '+ci.Subject);
      Memo.Lines.Add('Serial: '+ci.Serial);
      Memo.Lines.Add('SignatureAlgorithm: '+ci.SignatureAlgorithm);
      Memo.Lines.Add('PublicKeyAlgorithm: '+ci.PublicKeyAlgorithm);
      Memo.Lines.Add('PublicKeyBits: '+IntToStr(ci.PublicKeyBits));
      Memo.Lines.Add('ValidFrom: '+DateTimeToStr(ci.ValidFrom));
      Memo.Lines.Add('ValidTo: '+DateTimeToStr(ci.ValidTo));
      bView.Enabled:=True;
    end else
      Memo.Lines.Add('File is not certificate');
  end else if Pos(e,'.pfx.p12')>0 then begin
    if InputQuery('Password','',p) and GetPFXInfo(od.Filename,p,ci) then begin
      Memo.Lines.Add('Version: '+IntToStr(ci.Version));
      Memo.Lines.Add('Issuer: '+ci.Issuer);
      Memo.Lines.Add('Subject: '+ci.Subject);
      Memo.Lines.Add('Serial: '+ci.Serial);
      Memo.Lines.Add('SignatureAlgorithm: '+ci.SignatureAlgorithm);
      Memo.Lines.Add('PublicKeyAlgorithm: '+ci.PublicKeyAlgorithm);
      Memo.Lines.Add('PublicKeyBits: '+IntToStr(ci.PublicKeyBits));
      Memo.Lines.Add('ValidFrom: '+DateTimeToStr(ci.ValidFrom));
      Memo.Lines.Add('ValidTo: '+DateTimeToStr(ci.ValidTo));
      bView.Enabled:=True;
    end else
      Memo.Lines.Add('File is not certificate');
  end;
end;

procedure TwndMain.bPropsClick(Sender: TObject);
begin
 ShellPropDlg(Handle,eFile.Text);
end;

procedure TwndMain.bViewClick(Sender: TObject);
var
  e,s: string;
  dt: TDatetime;
begin
  e:=LowerCase(ExtractFileExt(eFile.Text));
  if Pos(e,'.exe.dll.sys.ocx.msi')>0 then
    VerifyFile(eFile.text,s,dt,Handle)
  else if Pos(e,'.p7b.cer.der')>0 then
    ViewCertificate(Handle,eFile.text)
  else if Pos(e,'.pfx.p12')>0 then
    if InputQuery('Password','',s) then
     ViewPFXCertificate(Handle,eFile.text,s);
end;

procedure TwndMain.FormCreate(Sender: TObject);
begin
  Image.Picture.Icon.Handle:=Application.Icon.Handle;
end;

end.
