program StylesDemoD103Rio;

uses
  Forms,
  StylesMainUnit in 'StylesMainUnit.pas' {StylesMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressScheduler StylesDemoD103Rio';
  Application.CreateForm(TStylesMainForm, StylesMainForm);
  Application.Run;
end.
