program CheckGroupsDemoD103Rio;

uses
  Forms,
  CheckGroupsDemoMain in 'CheckGroupsDemoMain.pas' {fmGheckGroupsDemo},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumTreeList CheckGroupsDemoD103Rio ';
  Application.HelpFile := '..\..\Help\ExpressQuantumTreeList.hlp';
  Application.CreateForm(TfmGheckGroupsDemo, fmGheckGroupsDemo);
  Application.Run;
end.
