program SpellCheckerSimpleDemoD103Rio;

uses
  Forms,
  SimpleDemoMain in 'SimpleDemoMain.pas' {fmCV},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TfmCV, fmCV);
  Application.Run;
end.
