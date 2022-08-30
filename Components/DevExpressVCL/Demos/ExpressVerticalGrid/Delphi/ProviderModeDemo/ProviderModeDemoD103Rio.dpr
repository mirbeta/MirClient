program ProviderModeDemoD103Rio;

uses
  Forms,
  ProviderModeDemoMain in 'ProviderModeDemoMain.pas' {ProviderModeDemoMainForm},
  ProviderModeDemoClasses in 'ProviderModeDemoClasses.pas',
  DemoBasicAbout in '..\Common\DemoBasicAbout.pas' {DemoBasicAboutForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressVerticalGrid ProviderMode Demo';
  Application.CreateForm(TProviderModeDemoMainForm, ProviderModeDemoMainForm);
  Application.CreateForm(TDemoBasicAboutForm, DemoBasicAboutForm);
  Application.CreateForm(TDemoBasicMainForm, DemoBasicMainForm);
  Application.CreateForm(TDemoRatingForm, DemoRatingForm);
  Application.Run;
end.
