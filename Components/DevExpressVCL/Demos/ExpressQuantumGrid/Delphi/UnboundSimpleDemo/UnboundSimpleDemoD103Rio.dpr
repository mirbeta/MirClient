program UnboundSimpleDemoD103Rio;

uses
  Forms,
  UnboundSimpleDemoMain in 'UnboundSimpleDemoMain.pas' {UnboundSimpleDemoMainForm},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid UnboundSimple Demo';
  Application.CreateForm(TUnboundSimpleDemoMainForm, UnboundSimpleDemoMainForm);
  Application.Run;
end.
