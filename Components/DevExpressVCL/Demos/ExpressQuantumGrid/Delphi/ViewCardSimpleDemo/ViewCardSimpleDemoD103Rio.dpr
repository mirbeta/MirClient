program ViewCardSimpleDemoD103Rio;

uses
  Forms,
  ViewCardSimpleDemoMain in 'ViewCardSimpleDemoMain.pas' {ViewCardSimpleDemoMainForm},
  ViewCardSimpleDemoData in 'ViewCardSimpleDemoData.pas' {ViewCardSimpleDemoMainDM: TDataModule},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid ViewCardSimple Demo';
  Application.CreateForm(TViewCardSimpleDemoMainForm, ViewCardSimpleDemoMainForm);
  Application.CreateForm(TViewCardSimpleDemoMainDM, ViewCardSimpleDemoMainDM);
  Application.Run;
end.
