program BingServicesD103Rio;

uses
  Forms,
  BasicDemoMain in '..\Common\BasicDemoMain.pas' {frmBasicDemoMain},
  DemoUtils in '..\Common\DemoUtils.pas',
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  BingServicesDemoMain in 'BingServicesDemoMain.pas' {BingServicesDemoMainForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Title := 'Bing Services';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TBingServicesDemoMainForm, BingServicesDemoMainForm);
  Application.Run;
end.
