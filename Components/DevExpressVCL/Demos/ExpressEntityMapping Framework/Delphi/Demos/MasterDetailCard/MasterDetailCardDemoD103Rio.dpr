program MasterDetailCardDemoD103Rio;

uses
  Forms,
  MasterDetailCardDemoMain in 'MasterDetailCardDemoMain.pas' {MasterDetailCardDemoMainForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  DemoUtils in '..\DemoUtils.pas',
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  FilmsDemoData in '..\Common\FilmsDemoData.pas' {FilmsDemoDM: TDataModule},
  Films.Entities in '..\Common\Films.Entities.pas',
  Films.Linq in '..\Common\Films.Linq.pas';

  {$R *.res}
  {$R WindowsXP.res}

  {$R WindowsXP.res}

begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid Master Detail Card Demo';
  Application.CreateForm(TFilmsDemoDM, FilmsDemoDM);
  Application.CreateForm(TMasterDetailCardDemoMainForm, MasterDetailCardDemoMainForm);
  Application.Run;
end.
