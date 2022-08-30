program EditorsInPlaceDemoD103Rio;

uses
  Forms,
  EditorsInPlaceDemoMain in 'EditorsInPlaceDemoMain.pas' {EditorsInPlaceDemoMainForm},
  EditorsInPlaceDemoData in 'EditorsInPlaceDemoData.pas' {EditorsInPlaceDemoDataDM: TDataModule},
  CarsData in '..\Common\CarsData.pas',
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  EditorsInPlaceDemoCarInfo in 'EditorsInPlaceDemoCarInfo.pas' {EditorsInPlaceDemoCarInfoForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas',
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumTreeList EditorsInPlaceDemoD103Rio';
  Application.HelpFile := '..\..\Help\ExpressQuantumTreeList.hlp';
  Application.CreateForm(TdmCars, dmCars);
  Application.CreateForm(TEditorsInPlaceDemoMainForm, EditorsInPlaceDemoMainForm);
  Application.CreateForm(TEditorsInPlaceDemoDataDM, EditorsInPlaceDemoDataDM);
  Application.CreateForm(TEditorsInPlaceDemoCarInfoForm, EditorsInPlaceDemoCarInfoForm);
  Application.Run;
end.
