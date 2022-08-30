program ColumnsMultiEditorsDemoD103Rio;

uses
  Forms,
  ColumnsMultiEditorsDemoDS in 'ColumnsMultiEditorsDemoDS.pas',
  ColumnsMultiEditorsDemoMain in 'ColumnsMultiEditorsDemoMain.pas' {ColumnsMultiEditorsDemoMainForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid ColumnsMultiEditors Demo';
  Application.CreateForm(TColumnsMultiEditorsDemoMainForm, ColumnsMultiEditorsDemoMainForm);
  Application.Run;
end.
