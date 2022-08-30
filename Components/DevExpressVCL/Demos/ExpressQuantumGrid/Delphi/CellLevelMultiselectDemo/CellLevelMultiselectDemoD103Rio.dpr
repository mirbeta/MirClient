program CellLevelMultiselectDemoD103Rio;

uses
  Forms,
  CellLevelMultiselectDemoMain in 'CellLevelMultiselectDemoMain.pas' {CellLevelMultiselectDemoMainForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid CellLevelMultiselect Demo';
  Application.CreateForm(TCellLevelMultiselectDemoMainForm, CellLevelMultiselectDemoMainForm);
  Application.Run;
end.
