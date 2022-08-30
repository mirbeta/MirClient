program StylesMultiDemoD103Rio;

uses
  Forms,
  StylesMultiDemoMain in 'StylesMultiDemoMain.pas' {StylesMultiDemoMainForm},
  StylesMultiDemoData in 'StylesMultiDemoData.pas' {StylesMultiDemoDataDM: TDataModule},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumTreeList StylesMultiDemoD103Rio';
  Application.HelpFile := '..\..\Help\ExpressQuantumTreeList.hlp';
  Application.CreateForm(TStylesMultiDemoDataDM, StylesMultiDemoDataDM);
  Application.CreateForm(TStylesMultiDemoMainForm, StylesMultiDemoMainForm);
  Application.Run;
end.
