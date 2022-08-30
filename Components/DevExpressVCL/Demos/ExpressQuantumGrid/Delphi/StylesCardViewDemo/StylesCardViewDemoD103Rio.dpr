program StylesCardViewDemoD103Rio;

uses
  Forms,
  StylesCardViewDemoMain in 'StylesCardViewDemoMain.pas' {StylesCardViewDemoMainForm},
  StylesCardViewDemoData in 'StylesCardViewDemoData.pas' {StylesCardViewDemoMainDM: TDataModule},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  DemoUtils in '..\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid StylesCardView Demo';
  Application.CreateForm(TStylesCardViewDemoMainForm, StylesCardViewDemoMainForm);
  Application.CreateForm(TStylesCardViewDemoMainDM, StylesCardViewDemoMainDM);
  Application.Run;
end.
