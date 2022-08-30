program DragDropDemoD103Rio;

uses
  Forms,
  DragDropDemoMain in 'DragDropDemoMain.pas' {DragDropDemoMainForm},
  DragDropDemoData in 'DragDropDemoData.pas' {DragDropDemoDataDM: TDataModule},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DragDropDemoDictionary in 'DragDropDemoDictionary.pas' {DragDropDemoDictionaryForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas',
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumTreeList DragDropDemoD103Rio';
  Application.HelpFile := '..\..\Help\ExpressQuantumTreeList.hlp';
  Application.CreateForm(TDragDropDemoMainForm, DragDropDemoMainForm);
  Application.CreateForm(TDragDropDemoDataDM, DragDropDemoDataDM);
  Application.CreateForm(TDragDropDemoDictionaryForm, DragDropDemoDictionaryForm);
  Application.Run;
end.
