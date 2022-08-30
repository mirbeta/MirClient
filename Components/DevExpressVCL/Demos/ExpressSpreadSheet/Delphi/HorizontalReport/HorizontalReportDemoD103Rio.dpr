program HorizontalReportDemoD103Rio;

uses
  Forms,
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\Common\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  ReportDesignerBaseForm in '..\Common\ReportDesignerBaseForm.pas' {frmReportDesignerBase},
  ReportPreviewUnit in '..\Common\ReportPreviewUnit.pas' {frmPreview},
  HorizontalReportDemoMain in 'HorizontalReportDemoMain.pas' {TfrmHorizontalReport};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmHorizontalReport, frmHorizontalReport);
  Application.Run;
end.
