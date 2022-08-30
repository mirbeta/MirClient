program UnboundExternalDemoD103Rio;

uses
  Forms,
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {frmDemoBasicMain},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  UnboundExternalMain in 'UnboundExternalMain.pas' {frmUnboundExternal},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TfrmUnboundExternal, frmUnboundExternal);
  Application.Run;
end.
