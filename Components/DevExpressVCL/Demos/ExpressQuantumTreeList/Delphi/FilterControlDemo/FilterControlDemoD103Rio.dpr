program FilterControlDemoD103Rio;

uses
  Forms,
  FilterControlDemoMain in 'FilterControlDemoMain.pas' {frmMain},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas',
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumTreeList FindPanel Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
