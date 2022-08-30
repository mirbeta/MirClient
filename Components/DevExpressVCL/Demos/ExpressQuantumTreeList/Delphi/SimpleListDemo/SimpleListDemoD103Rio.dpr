program SimpleListDemoD103Rio;

uses
  Forms,
  SimpleListDemoMain in 'SimpleListDemoMain.pas' {SimpleListDemoMainForm},
  SimpleListDemoData in 'SimpleListDemoData.pas' {SimpleListDemoDataDM: TDataModule},
  CarsData in '..\Common\CarsData.pas',
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumTreeList SimpleList Demo';
  Application.HelpFile := '..\..\Help\ExpressQuantumTreeList.hlp';
  Application.CreateForm(TdmCars, dmCars);
  Application.CreateForm(TSimpleListDemoDataDM, SimpleListDemoDataDM);
  Application.CreateForm(TSimpleListDemoMainForm, SimpleListDemoMainForm);
  Application.Run;
end.
