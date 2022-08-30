program EQGridReportLinkD103Rio;

uses
  Forms,
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  EQGridRLMain in 'EQGridRLMain.pas' {EQGridRLMainForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'Report Link Demo - ExpressQuantumGrid';
  Application.CreateForm(TEQGridRLMainForm, EQGridRLMainForm);
  Application.Run;
end.
