program MasterDetailMultiDemoD103Rio;

uses
  Forms,
  MasterDetailMultiDemoMain in 'MasterDetailMultiDemoMain.pas' {MasterDetailMultiDemoMainForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  DemoUtils in '..\DemoUtils.pas',
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  FilmsDemoData in '..\Common\FilmsDemoData.pas' {FilmsDemoDM: TDataModule};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid MasterDetailMulti Demo';
  Application.CreateForm(TFilmsDemoDM, FilmsDemoDM);
  Application.CreateForm(TMasterDetailMultiDemoMainForm, MasterDetailMultiDemoMainForm);
  Application.Run;
end.
