program ServerModeADOQueryDemoD103Rio;

uses
  Forms,
  ServerModeDemoData in 'ServerModeDemoData.pas' {ServerModeDemoDataDM},
  ServerModeDemoConnection in 'ServerModeDemoConnection.pas' {ServerModeDemoConnectionForm},
  ServerModeDemoMain in 'ServerModeDemoMain.pas' {ServerModeDemoMainForm},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TServerModeDemoConnectionForm, ServerModeDemoConnectionForm);
  Application.CreateForm(TServerModeDemoDataDM, ServerModeDemoDataDM);
  Application.CreateForm(TServerModeDemoMainForm, ServerModeDemoMainForm);
  Application.Run;
end.
