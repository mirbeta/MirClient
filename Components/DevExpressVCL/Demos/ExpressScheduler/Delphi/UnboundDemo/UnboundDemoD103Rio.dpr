program UnboundDemoD103Rio;

uses
  Forms,
  UnboundDemoMain in 'UnboundDemoMain.pas' {UnboundDemoMainForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressScheduler UnboundDemoD103Rio';
  Application.CreateForm(TUnboundDemoMainForm, UnboundDemoMainForm);
  Application.Run;
end.
