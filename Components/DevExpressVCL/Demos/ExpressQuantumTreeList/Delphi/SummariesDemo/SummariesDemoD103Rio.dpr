program SummariesDemoD103Rio;

uses
  Forms,
  SummariesDemoMain in 'SummariesDemoMain.pas' {SummariesDemoMainForm},
  SummariesDemoData in 'SummariesDemoData.pas' {SummariesDemoDataDM: TDataModule},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumTreeList SummariesDemoD103Rio';
  Application.HelpFile := '..\..\Help\ExpressQuantumTreeList.hlp';
  Application.CreateForm(TSummariesDemoDataDM, SummariesDemoDataDM);
  Application.CreateForm(TSummariesDemoMainForm, SummariesDemoMainForm);
  Application.Run;
end.
