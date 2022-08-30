program StylesMultiDemoD103Rio;

uses
  Forms,
  StylesMultiDemoMain in 'StylesMultiDemoMain.pas' {StylesMultiDemoMainForm},
  StylesMultiDemoData in 'StylesMultiDemoData.pas' {StylesMultiDemoMainDM: TDataModule},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  DemoUtils in '..\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid StylesMulti Demo';
  Application.CreateForm(TStylesMultiDemoMainDM, StylesMultiDemoMainDM);
  Application.CreateForm(TStylesMultiDemoMainForm, StylesMultiDemoMainForm);
  Application.Run;
end.
