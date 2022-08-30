program EditorsInPlaceDemoD103Rio;

uses
  Forms,
  CarsData in '..\Common\CarsData.pas',
  EditorsInPlaceDemoMain in 'EditorsInPlaceDemoMain.pas' {EditorsInPlaceDemoMainForm},
  EditorsInPlaceDemoData in 'EditorsInPlaceDemoData.pas' {EditorsInPlaceDemoDataDM: TDataModule},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicAbout in '..\Common\DemoBasicAbout.pas' {DemoBasicAboutForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  EditorsInPlaceDemoCarInfo in 'EditorsInPlaceDemoCarInfo.pas' {EditorsInPlaceDemoCarInfoForm},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressVerticalGrid EditorsInPlace Demo';
  Application.CreateForm(TdmCars, dmCars);
  Application.CreateForm(TEditorsInPlaceDemoDataDM, EditorsInPlaceDemoDataDM);
  Application.CreateForm(TEditorsInPlaceDemoMainForm, EditorsInPlaceDemoMainForm);
  Application.CreateForm(TEditorsInPlaceDemoCarInfoForm, EditorsInPlaceDemoCarInfoForm);
  Application.Run;
end.
