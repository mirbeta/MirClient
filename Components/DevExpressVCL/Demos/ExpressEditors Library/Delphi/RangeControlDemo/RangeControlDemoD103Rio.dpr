program RangeControlDemoD103Rio;

uses
  Forms,
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  DemoUtils in '..\DemoUtils.pas',
  RangeControlDemoMain in 'RangeControlDemoMain.pas' {dxRangeControlDemoForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdxRangeControlDemoForm, dxRangeControlDemoForm);
  Application.Run;
end.
