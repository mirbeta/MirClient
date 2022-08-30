program CustomDrawCardViewDemoD103Rio;

{$R 'CustomDrawCardViewDemoImages.res' 'CustomDrawCardViewDemoImages.rc'}

uses
  Forms,
  CustomDrawCardViewDemoMain in 'CustomDrawCardViewDemoMain.pas' {CustomDrawCardViewDemoMainForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  DemoUtils in '..\DemoUtils.pas',
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  FilmsDemoData in '..\Common\FilmsDemoData.pas' {FilmsDemoDM: TDataModule};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid CustomDrawCardView Demo';
  Application.CreateForm(TFilmsDemoDM, FilmsDemoDM);
  Application.CreateForm(TCustomDrawCardViewDemoMainForm, CustomDrawCardViewDemoMainForm);
  Application.Run;
end.
