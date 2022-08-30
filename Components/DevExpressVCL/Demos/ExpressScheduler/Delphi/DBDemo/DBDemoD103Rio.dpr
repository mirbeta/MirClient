program DBDemoD103Rio;

uses
  Forms,
  DBDemoMainUnit in 'DBDemoMainUnit.pas' {DBDemoMainForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressScheduler DBDemoD103Rio';
  Application.CreateForm(TDBDemoMainForm, DBDemoMainForm);
  Application.Run;
end.
