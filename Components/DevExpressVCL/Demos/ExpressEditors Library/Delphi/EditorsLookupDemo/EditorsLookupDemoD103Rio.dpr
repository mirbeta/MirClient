program EditorsLookupDemoD103Rio;

uses
  Forms,
  EditorsLookupDemoMain in 'EditorsLookupDemoMain.pas' {EditorsLookupDemoMainForm},
  EditorsLookupDemoData in 'EditorsLookupDemoData.pas' {EditorsLookupDemoDataDM: TDataModule},
  EditorsLookupDemoNewUser in 'EditorsLookupDemoNewUser.pas' {EditorsLookupDemoNewUserForm},
  DemoUtils in '..\DemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressEditors LookupDemo';
  Application.CreateForm(TEditorsLookupDemoDataDM, EditorsLookupDemoDataDM);
  Application.CreateForm(TEditorsLookupDemoMainForm, EditorsLookupDemoMainForm);
  Application.CreateForm(TEditorsLookupDemoNewUserForm, EditorsLookupDemoNewUserForm);
  Application.Run;
end.
