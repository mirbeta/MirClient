program LayoutControlReportLinkD103Rio;

uses
  Forms,
  LayoutControlRLMain in 'LayoutControlRLMain.pas' {LayoutControlMainForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoDM in 'DemoDM.pas' {dmDemo: TDataModule};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Title := 'Report Link Demo - ExpressLayoutControl';
  Application.Initialize;
  Application.CreateForm(TLayoutControlMainForm, LayoutControlMainForm);
  Application.CreateForm(TdmDemo, dmDemo);
  Application.Run;
end.
