program FilterDropDownDemoD103Rio;

uses
  Forms,
  FilterDropDownDemoMain in 'FilterDropDownDemoMain.pas' {frmMain},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicAbout in '..\Common\DemoBasicAbout.pas' {DemoBasicAboutForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid Filter Dropdown Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
