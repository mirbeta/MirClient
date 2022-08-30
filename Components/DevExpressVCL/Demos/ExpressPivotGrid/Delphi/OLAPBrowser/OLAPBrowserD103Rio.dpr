program OLAPBrowserD103Rio;

uses
  Forms,
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {frmDemoBasicMain},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  OLAPBrowserMain in 'OLAPBrowserMain.pas' {frmOlapBrowser},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TfrmOlapBrowser, frmOlapBrowser);
  Application.Run;
end.
