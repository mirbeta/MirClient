program DragDropDemoD103Rio;

uses
  Forms,
  DragDropDemoDictionaries in 'DragDropDemoDictionaries.pas' {DragDropDemoDictionariesForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  DemoUtils in '..\DemoUtils.pas',
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  FilmsDemoData in '..\Common\FilmsDemoData.pas' {FilmsDemoDM: TDataModule},
  DragDropDemoMain in 'DragDropDemoMain.pas' {DragDropDemoMainForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid DragDropDemoD103Rio';
  Application.CreateForm(TFilmsDemoDM, FilmsDemoDM);
  Application.CreateForm(TDragDropDemoMainForm, DragDropDemoMainForm);
  Application.CreateForm(TDragDropDemoDictionariesForm, DragDropDemoDictionariesForm);
  Application.Run;
end.
