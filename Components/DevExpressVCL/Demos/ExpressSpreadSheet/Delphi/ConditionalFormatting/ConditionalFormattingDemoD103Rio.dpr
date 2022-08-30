program ConditionalFormattingDemoD103Rio;

uses
  Forms,
  ConditionalFormattingDemoMain in 'ConditionalFormattingDemoMain.pas' {frmConditionalFormatting},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\Common\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmConditionalFormatting, frmConditionalFormatting);
  Application.Run;
end.
