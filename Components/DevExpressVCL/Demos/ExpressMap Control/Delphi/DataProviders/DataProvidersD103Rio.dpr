program DataProvidersD103Rio;

uses
  Forms,
  DemoUtils in '..\Common\DemoUtils.pas',
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  BasicDemoMain in '..\Common\BasicDemoMain.pas' {frmBasicDemoMain},
  DataProvidersDemoMain in 'DataProvidersDemoMain.pas' {DataProvidersDemoMainForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataProvidersDemoMainForm, DataProvidersDemoMainForm);
  Application.Run;
end.
