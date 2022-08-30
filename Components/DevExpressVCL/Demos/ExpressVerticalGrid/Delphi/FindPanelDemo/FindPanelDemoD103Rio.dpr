program FindPanelDemoD103Rio;

uses
  Forms,
  FindPanelDemoMain in 'FindPanelDemoMain.pas' {frmMain},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicAbout in '..\Common\DemoBasicAbout.pas' {DemoBasicAboutForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid FindPanel Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
