program BreadcrumbEditDemoD103Rio;

uses
  Forms,
  BaseForm in '..\BaseForm.pas',
  DemoUtils in '..\DemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas',
  BreadcrumbEditDemoMain in 'BreadcrumbEditDemoMain.pas' {dxBreadcrumbEditDemoForm},
  BreadcrumbEditDemoRecentPaths in 'BreadcrumbEditDemoRecentPaths.pas' {dxBreadcrumbEditDemoRecentPathsForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TdxBreadcrumbEditDemoForm, dxBreadcrumbEditDemoForm);
  Application.Run;
end.
