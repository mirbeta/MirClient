program UnboundModeDemoD103Rio;

uses
  Forms,
  UnboundModeDemoMain in 'UnboundModeDemoMain.pas' {UnboundModeDemoMainForm},
  UnboundModeDemoData in 'UnboundModeDemoData.pas' {UnboundModeDemoDataDM: TDataModule},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumTreeList UnboundModeDemoD103Rio ';
  Application.HelpFile := '..\..\Help\ExpressQuantumTreeList.hlp';
  Application.CreateForm(TUnboundModeDemoMainForm, UnboundModeDemoMainForm);
  Application.CreateForm(TUnboundModeDemoDataDM, UnboundModeDemoDataDM);
  Application.Run;
end.
