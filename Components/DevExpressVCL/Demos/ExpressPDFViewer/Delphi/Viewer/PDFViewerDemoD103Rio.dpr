program PDFViewerDemoD103Rio;

uses
  Forms,
  uPDFViewer in 'uPDFViewer.pas' {frmPDFViewer},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  dxProgressDialog in '..\Common\dxProgressDialog.pas' {frmProgress},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPDFViewer, frmPDFViewer);
  Application.Run;
end.
