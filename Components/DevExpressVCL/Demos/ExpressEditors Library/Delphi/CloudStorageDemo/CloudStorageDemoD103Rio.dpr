program CloudStorageDemoD103Rio;

uses
  Forms,
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  DemoUtils in '..\DemoUtils.pas',
  uCloudStorageDemoMain in 'uCloudStorageDemoMain.pas' {fmCloudStorageDemoForm},
  uCloudSetupForm in 'uCloudSetupForm.pas' {fmCloudSetupWizard};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmCloudStorageDemoForm, fmCloudStorageDemoForm);
  Application.Run;
end.
