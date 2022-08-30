program EditorsInPlaceValidationDemoD103Rio;

uses
  Forms,
  EditorsInPlaceValidationDemoMain in 'EditorsInPlaceValidationDemoMain.pas' {EditorsInPlaceValidationDemoMainForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid EditorsInPlaceValidation Demo';
  Application.CreateForm(TEditorsInPlaceValidationDemoMainForm, EditorsInPlaceValidationDemoMainForm);
  Application.Run;
end.
