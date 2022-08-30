program MasterDetailChartDemoD103Rio;

uses
  Forms,
  MasterDetailChartDemoMain in 'MasterDetailChartDemoMain.pas' {frmMain},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid Master-Detail Chart Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
