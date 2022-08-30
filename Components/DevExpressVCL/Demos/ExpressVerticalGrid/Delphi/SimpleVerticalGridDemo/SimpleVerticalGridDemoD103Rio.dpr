program SimpleVerticalGridDemoD103Rio;

uses
  Forms,
  SimpleVerticalGridDemoMain in 'SimpleVerticalGridDemoMain.pas' {SimpleVerticalGridDemoMainForm},
  CarsData in '..\Common\CarsData.pas' {DemoRatingForm},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  SimpleVerticalGridDemoData in 'SimpleVerticalGridDemoData.pas' {SimpleVerticalGridDemoMainDM: TDataModule},
  DemoBasicAbout in '..\Common\DemoBasicAbout.pas' {DemoBasicAboutForm},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressVerticalGrid SimpleVerticalGrid Demo';
  Application.CreateForm(TdmCars, dmCars);
  Application.CreateForm(TSimpleVerticalGridDemoMainDM, SimpleVerticalGridDemoMainDM);
  Application.CreateForm(TSimpleVerticalGridDemoMainForm, SimpleVerticalGridDemoMainForm);
  Application.Run;
end.
