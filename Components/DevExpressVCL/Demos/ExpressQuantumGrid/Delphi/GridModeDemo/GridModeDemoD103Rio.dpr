program GridModeDemoD103Rio;

uses
  Forms,
  GridModeDemoMain in 'GridModeDemoMain.pas' {GridModeDemoMainForm},
  GridModeDemoData in 'GridModeDemoData.pas' {GridModeDemoDataDM: TDataModule},
  GridModeDemoTerminate in 'GridModeDemoTerminate.pas' {GridModeDemoTerminateForm},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid GridModeDemoD103Rio';
  Application.CreateForm(TGridModeDemoDataDM, GridModeDemoDataDM);
  Application.CreateForm(TGridModeDemoMainForm, GridModeDemoMainForm);
  Application.Run;
end.
