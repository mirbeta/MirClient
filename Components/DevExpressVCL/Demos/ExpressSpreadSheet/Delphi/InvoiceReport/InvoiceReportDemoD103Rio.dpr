program InvoiceReportDemoD103Rio;

uses
  Forms,
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  ReportPreviewUnit in '..\Common\ReportPreviewUnit.pas' {frmPreview},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  ReportDesignerBaseForm in '..\Common\ReportDesignerBaseForm.pas' {frmReportDesignerBase},
  BaseForm in '..\Common\BaseForm.pas' {fmBaseForm},
  InvoiceReportDemoMain in 'InvoiceReportDemoMain.pas' {frmInvoiceReport};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmInvoiceReport, frmInvoiceReport);
  Application.Run;
end.
