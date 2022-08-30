program RowsMultiEditorsDemoD103Rio;



uses
  Forms,
  RowsMultiEditorsDemoMain in 'RowsMultiEditorsDemoMain.pas' {RowsMultiEditorsDemoMainForm},
  DemoBasicAbout in '..\Common\DemoBasicAbout.pas' {DemoBasicAboutForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  RowsMultiEditorsDemoData in 'RowsMultiEditorsDemoData.pas' {RowsMultiEditorsDemoDataDM: TDataModule},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressVerticalGrid RowsMultiEditors Demo';
  Application.CreateForm(TRowsMultiEditorsDemoMainForm, RowsMultiEditorsDemoMainForm);
  Application.CreateForm(TRowsMultiEditorsDemoDataDM, RowsMultiEditorsDemoDataDM);
  Application.Run;
end.
