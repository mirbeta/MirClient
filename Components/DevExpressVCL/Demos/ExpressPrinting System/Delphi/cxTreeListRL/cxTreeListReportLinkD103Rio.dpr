program cxTreeListReportLinkD103Rio;

uses
  Forms,
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  cxTreeListRLMain in 'cxTreeListRLMain.pas' {cxTreeListRLMainForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'Report Link Demo - ExpressQuantumTreeList';
  Application.CreateForm(TcxTreeListRLMainForm, cxTreeListRLMainForm);
  Application.Run;
end.
