program CustomDrawDemoD103Rio;

{$R 'CustomDrawDemoImages.res' 'CustomDrawDemoImages.rc'}

uses
  Forms,
  CustomDrawDemoMain in 'CustomDrawDemoMain.pas' {CustomDrawDemoMainForm},
  CustomDrawDemoData in 'CustomDrawDemoData.pas' {CustomDrawDemoDataDM: TDataModule},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicAbout in '..\Common\DemoBasicAbout.pas' {DemoBasicAboutForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  CustomDrawDemoEditor in 'CustomDrawDemoEditor.pas' {CustomDrawDemoEditorForm},
  CustomDrawDemoUtils in 'CustomDrawDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressVerticalGrid CustomDraw Demo';
  Application.CreateForm(TCustomDrawDemoDataDM, CustomDrawDemoDataDM);
  Application.CreateForm(TCustomDrawDemoMainForm, CustomDrawDemoMainForm);
  Application.CreateForm(TCustomDrawDemoEditorForm, CustomDrawDemoEditorForm);
  Application.Run;
end.
