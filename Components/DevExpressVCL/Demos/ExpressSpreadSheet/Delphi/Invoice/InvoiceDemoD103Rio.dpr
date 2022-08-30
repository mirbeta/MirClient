program InvoiceDemoD103Rio;

uses
  Forms,
  InvoiceDemoMain in 'InvoiceDemoMain.pas' {frmInvoice},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\Common\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmInvoice, frmInvoice);
  Application.Run;
end.
