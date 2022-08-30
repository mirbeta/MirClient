program EditorsInPlaceValidationDemoD103Rio;

uses
  Forms,
  EditorsInPlaceValidationDemoMain in 'EditorsInPlaceValidationDemoMain.pas' {EditorsInPlaceValidationDemoMainForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas',
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumTreeList EditorsInPlaceValidation Demo';
  Application.HelpFile := '..\..\Help\ExpressQuantumTreeList.hlp';
  Application.CreateForm(TEditorsInPlaceValidationDemoMainForm, EditorsInPlaceValidationDemoMainForm);
  Application.Run;
end.
