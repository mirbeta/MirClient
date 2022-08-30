program SchedulerLinkD103Rio;

uses
  Forms,
  SchedulerRLMain in 'SchedulerRLMain.pas' {SchedulerRLMainForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Title := 'Report Link Demo - ExpressScheduler';
  Application.Initialize;
  Application.CreateForm(TSchedulerRLMainForm, SchedulerRLMainForm);
  Application.Run;
end.
