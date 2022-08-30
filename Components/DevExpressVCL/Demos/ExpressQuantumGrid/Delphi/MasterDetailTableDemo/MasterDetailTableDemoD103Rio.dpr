program MasterDetailTableDemoD103Rio;

uses
  Forms,
  MasterDetailTableDemoMain in 'MasterDetailTableDemoMain.pas' {MasterDetailTableDemoMainForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  DemoUtils in '..\DemoUtils.pas',
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  FilmsDemoData in '..\Common\FilmsDemoData.pas' {FilmsDemoDM: TDataModule};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid Master Detail Table Demo';
  Application.CreateForm(TFilmsDemoDM, FilmsDemoDM);
  Application.CreateForm(TMasterDetailTableDemoMainForm, MasterDetailTableDemoMainForm);
  Application.Run;
end.
