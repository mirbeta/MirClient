program SimpleReportDemoD103Rio;

uses
  Forms,
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\Common\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  ReportPreviewUnit in '..\Common\ReportPreviewUnit.pas' {frmPreview},
  ReportDesignerBaseForm in '..\Common\ReportDesignerBaseForm.pas' {frmReportDesignerBase},
  SimpleReportDemoMain in 'SimpleReportDemoMain.pas' {frmSimpleReport};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSimpleReport, frmSimpleReport);
  Application.Run;
end.
