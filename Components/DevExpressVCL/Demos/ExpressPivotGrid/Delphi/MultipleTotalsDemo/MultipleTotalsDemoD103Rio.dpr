program MultipleTotalsDemoD103Rio;

uses
  Forms,
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {frmDemoBasicMain},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicDM in '..\Common\DemoBasicDM.pas' {dmOrders: TDataModule},
  MultipleTotalsMain in 'MultipleTotalsMain.pas' {frmMultipleTotals},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TdmOrders, dmOrders);
  Application.CreateForm(TfrmMultipleTotals, frmMultipleTotals);
  Application.Run;
end.
