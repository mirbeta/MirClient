program FilterDropDownDemoD103Rio;

uses
  Forms,
  CarsData in '..\Common\CarsData.pas' {dmCars: TDataModule},
  FilterDropDownDemoMain in 'FilterDropDownDemoMain.pas' {frmMain},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas',
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumTreeList Filter dropdown Demo';
  Application.CreateForm(TdmCars, dmCars);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
