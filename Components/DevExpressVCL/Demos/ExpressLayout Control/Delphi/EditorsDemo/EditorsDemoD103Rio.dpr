program EditorsDemoD103Rio;

uses
  Forms,
  DemoDM in '..\common\DemoDM.pas' {dmDemo: TDataModule},
  BasicDemoMain in '..\common\BasicDemoMain.pas' {frmBasicDemoMain},
  AboutDemoForm in '..\common\AboutDemoForm.pas' {frmAboutDemoForm},
  EditorsDemoMain in 'EditorsDemoMain.pas' {frmEditorsDemoMain};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TdmDemo, dmDemo);
  Application.CreateForm(TfrmEditorsDemoMain, frmEditorsDemoMain);
  Application.Run;
end.
