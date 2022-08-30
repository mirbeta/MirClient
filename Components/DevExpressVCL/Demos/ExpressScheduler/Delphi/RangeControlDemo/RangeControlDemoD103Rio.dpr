program RangeControlDemoD103Rio;

uses
  Forms,
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  RangeControlDemoMain in 'RangeControlDemoMain.pas' {RangeControlDemoMainForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TRangeControlDemoMainForm, RangeControlDemoMainForm);
  Application.Run;
end.
