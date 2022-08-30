program EditorsMaskDemoD103Rio;

uses
  Forms,
  EditorsMaskDemoMain in 'EditorsMaskDemoMain.pas' {EditorsMaskDemoMainForm},
  EditorsMaskDemoData in 'EditorsMaskDemoData.pas' {EditorsMaskDemoMainDM: TDataModule},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  DemoUtils in '..\DemoUtils.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressEditors MaskDemo';
  Application.CreateForm(TEditorsMaskDemoMainDM, EditorsMaskDemoMainDM);
  Application.CreateForm(TEditorsMaskDemoMainForm, EditorsMaskDemoMainForm);
  Application.Run;
end.
