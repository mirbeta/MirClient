program WebServiceDemoD103Rio;

uses
  Forms,
  WebServiceDemoMain in 'WebServiceDemoMain.pas' {WebServiceDemoMainForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  WebServiceDemoSetupForm in 'WebServiceDemoSetupForm.pas' {WebServiceDemoSetupWizard};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressScheduler WebServiceDemoD103Rio';
  Application.CreateForm(TWebServiceDemoMainForm, WebServiceDemoMainForm);
  Application.Run;
end.
