program SummaryMultiDemoD103Rio;

uses
  Forms,
  SummaryMultiDemoMain in 'SummaryMultiDemoMain.pas' {SummaryMultiDemoMainForm},
  SummaryMultiDemoData in 'SummaryMultiDemoData.pas' {SummaryMultiDemoDataDM: TDataModule},
  CarsData in '..\Common\CarsData.pas' {dmCars},
  CarsDataForGrid in '..\Common\CarsDataForGrid.pas' {dmGridCars},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  DemoUtils in '..\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid SummaryMultiDemoD103Rio';
  Application.CreateForm(TdmGridCars, dmGridCars);
  Application.CreateForm(TSummaryMultiDemoDataDM, SummaryMultiDemoDataDM);
  Application.CreateForm(TSummaryMultiDemoMainForm, SummaryMultiDemoMainForm);
  Application.Run;
end.
