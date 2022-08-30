program AlertWindowDemoD103Rio;

uses
  Forms,
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  DemoUtils in '..\DemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas',
  AlertWindowDemoMain in 'AlertWindowDemoMain.pas' {AlertWindowDemoForm},
  AlertWindowDemoOptions in 'AlertWindowDemoOptions.pas' {FormOptions};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TAlertWindowDemoForm, AlertWindowDemoForm);
  Application.CreateForm(TFormOptions, FormOptions);
  Application.Run;
end.
