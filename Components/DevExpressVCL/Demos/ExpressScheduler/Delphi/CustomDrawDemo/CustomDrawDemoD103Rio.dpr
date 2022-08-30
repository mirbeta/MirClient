program CustomDrawDemoD103Rio;

uses
  Forms,
  CustomDrawDemoMain in 'CustomDrawDemoMain.pas' {CustomDrawDemoMainForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas';


  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressScheduler CustomDrawDemoD103Rio';
  Application.CreateForm(TCustomDrawDemoMainForm, CustomDrawDemoMainForm);
  Application.Run;
end.
