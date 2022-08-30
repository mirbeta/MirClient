program GridMenuViewsDemoD103Rio;

uses
  Forms,
  CarsData in '..\Common\CarsData.pas' {dmCars},
  CarsDataForGrid in '..\Common\CarsDataForGrid.pas' {dmGridCars},
  GridMenuViewsDemoMain in 'GridMenuViewsDemoMain.pas' {GridMenuViewsDemoMainForm},
  GridMenuViewsDemoData in 'GridMenuViewsDemoData.pas' {GridMenuViewsDemoDataDM: TDataModule},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid GridMenuViewsDemoD103Rio';
  Application.CreateForm(TdmGridCars, dmGridCars);
  Application.CreateForm(TGridMenuViewsDemoDataDM, GridMenuViewsDemoDataDM);
  Application.CreateForm(TGridMenuViewsDemoMainForm, GridMenuViewsDemoMainForm);
  Application.Run;
end.
