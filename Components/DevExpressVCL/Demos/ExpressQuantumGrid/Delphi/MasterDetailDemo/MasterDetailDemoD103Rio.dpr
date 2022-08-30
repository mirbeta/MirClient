program MasterDetailDemoD103Rio;

uses
  Forms,
  MasterDetailDemoMain in 'MasterDetailDemoMain.pas' {MasterDetailDemoMainForm},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  DemoUtils in '..\DemoUtils.pas',
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  FilmsDemoData in '..\Common\FilmsDemoData.pas' {FilmsDemoDM: TDataModule};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid Master Detail Demo';
  Application.CreateForm(TFilmsDemoDM, FilmsDemoDM);
  Application.CreateForm(TMasterDetailDemoMainForm, MasterDetailDemoMainForm);
  Application.Run;
end.
