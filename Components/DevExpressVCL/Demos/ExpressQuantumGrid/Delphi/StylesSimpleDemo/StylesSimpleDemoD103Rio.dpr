program StylesSimpleDemoD103Rio;

uses
  Forms,
  StylesSimpleDemoMain in 'StylesSimpleDemoMain.pas' {StylesSimpleDemoMainForm},
  StylesSimpleDemoEdit in 'StylesSimpleDemoEdit.pas' {StylesSimpleDemoEditForm},
  StylesSimpleDemoAssign in 'StylesSimpleDemoAssign.pas' {StylesSimpleDemoAssignForm},
  StylesSimpleDemoData in 'StylesSimpleDemoData.pas' {StylesSimpleDemoMainDM: TDataModule},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid StylesSimple Demo';
  Application.CreateForm(TStylesSimpleDemoMainDM, StylesSimpleDemoMainDM);
  Application.CreateForm(TStylesSimpleDemoMainForm, StylesSimpleDemoMainForm);
  Application.Run;
end.
