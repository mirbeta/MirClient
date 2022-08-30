program FilterDemoD103Rio;

uses
  Forms,
  FilterDemoMain in 'FilterDemoMain.pas' {frmMain},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid Filter Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
