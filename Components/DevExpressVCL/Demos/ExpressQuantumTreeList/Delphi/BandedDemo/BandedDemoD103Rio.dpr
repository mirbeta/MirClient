program BandedDemoD103Rio;

uses
  Forms,
  BandedDemoMain in 'BandedDemoMain.pas' {BandedDemoMainForm},
  BandedDemoData in 'BandedDemoData.pas' {BandedDemoDataDM: TDataModule},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  BandedDemoBands in 'BandedDemoBands.pas' {BandedDemoBandsForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumTreeList BandedDemoD103Rio';
  Application.HelpFile := '..\..\Help\ExpressQuantumTreeList.hlp';
  Application.CreateForm(TBandedDemoMainForm, BandedDemoMainForm);
  Application.CreateForm(TBandedDemoDataDM, BandedDemoDataDM);
  Application.Run;
end.
