program SimpleTreeDemoD103Rio;

uses
  Forms,
  SimpleTreeDemoMain in 'SimpleTreeDemoMain.pas' {SimpleTreeDemoMainForm},
  SimpleTreeDemoData in 'SimpleTreeDemoData.pas' {SimpleTreeDemoDataDM: TDataModule},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumTreeList SimpleTreeDemoD103Rio';
  Application.HelpFile := '..\..\Help\ExpressQuantumTreeList.hlp';
  Application.CreateForm(TSimpleTreeDemoMainForm, SimpleTreeDemoMainForm);
  Application.CreateForm(TSimpleTreeDemoDataDM, SimpleTreeDemoDataDM);
  Application.Run;
end.
