program ViewChartStackedDemoD103Rio;

uses
  Forms,
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  ViewChartStackedDemoMain in 'ViewChartStackedDemoMain.pas' {frmMain},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid ViewChart Stacked Diagrams Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
