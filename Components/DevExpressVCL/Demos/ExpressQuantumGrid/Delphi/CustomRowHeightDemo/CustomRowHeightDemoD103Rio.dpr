program CustomRowHeightDemoD103Rio;

uses
  Forms,
  CustomRowHeightDemoMain in 'CustomRowHeightDemoMain.pas' {CustomRowHeightDemoMainForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  FilmsDemoData in '..\Common\FilmsDemoData.pas' {FilmsDemoDM: TDataModule};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid CustomRowHeight Demo';
  Application.CreateForm(TFilmsDemoDM, FilmsDemoDM);
  Application.CreateForm(TCustomRowHeightDemoMainForm, CustomRowHeightDemoMainForm);
  Application.Run;
end.
