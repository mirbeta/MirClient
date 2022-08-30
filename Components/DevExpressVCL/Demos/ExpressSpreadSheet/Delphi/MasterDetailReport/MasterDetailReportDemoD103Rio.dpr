program MasterDetailReportDemoD103Rio;

uses
  Forms,
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  ReportPreviewUnit in '..\Common\ReportPreviewUnit.pas' {frmPreview},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  ReportDesignerBaseForm in '..\Common\ReportDesignerBaseForm.pas' {frmReportDesignerBase},
  BaseForm in '..\Common\BaseForm.pas' {fmBaseForm},
  MasterDetailReportDemoMain in 'MasterDetailReportDemoMain.pas' {frmMasterDetail};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMasterDetail, frmMasterDetail);
  Application.Run;
end.
