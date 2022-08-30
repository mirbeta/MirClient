program OutlineDemoD103Rio;

uses
  Forms,
  OutlineDemoMain in 'OutlineDemoMain.pas' {frmOutline},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\Common\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmOutline, frmOutline);
  Application.Run;
end.
