program StylesSimpleDemoD103Rio;

uses
  Forms,
  StylesSimpleDemoMain in 'StylesSimpleDemoMain.pas' {StylesSimpleDemoMainForm},
  StylesSimpleDemoEdit in 'StylesSimpleDemoEdit.pas' {StylesSimpleDemoEditForm},
  StylesSimpleDemoData in 'StylesSimpleDemoData.pas' {StylesSimpleDemoDataDM: TDataModule},
  StylesSimpleDemoStylesDialog in 'StylesSimpleDemoStylesDialog.pas' {StylesSimpleDemoStylesDialogForm},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumTreeList StylesSimple Demo';
  Application.HelpFile := '..\..\Help\ExpressQuantumTreeList.hlp';
  Application.CreateForm(TStylesSimpleDemoDataDM, StylesSimpleDemoDataDM);
  Application.CreateForm(TStylesSimpleDemoMainForm, StylesSimpleDemoMainForm);
  Application.CreateForm(TStylesSimpleDemoStylesDialogForm, StylesSimpleDemoStylesDialogForm);
  Application.Run;
end.
