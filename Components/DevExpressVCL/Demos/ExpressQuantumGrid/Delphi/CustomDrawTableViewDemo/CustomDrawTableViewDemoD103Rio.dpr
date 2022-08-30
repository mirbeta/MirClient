program CustomDrawTableViewDemoD103Rio;

{$R 'CustomDrawTableViewDemoImages.res' 'CustomDrawTableViewDemoImages.rc'}

uses
  Forms,
  CarsData in '..\Common\CarsData.pas' {dmCars},
  CarsDataForGrid in '..\Common\CarsDataForGrid.pas' {dmGridCars},
  CustomDrawTableViewDemoMain in 'CustomDrawTableViewDemoMain.pas' {CustomDrawTableViewDemoMainForm},
  CustomDrawTableViewDemoData in 'CustomDrawTableViewDemoData.pas' {CustomDrawTableViewDemoMainDM: TDataModule},
  CustomDrawTableViewDemoStylesEditor in 'CustomDrawTableViewDemoStylesEditor.pas' {CustomDrawTableViewDemoStylesEditorForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  DemoUtils in '..\DemoUtils.pas',
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid CustomDrawTableView Demo';
  Application.CreateForm(TdmGridCars, dmGridCars);
  Application.CreateForm(TCustomDrawTableViewDemoMainDM, CustomDrawTableViewDemoMainDM);
  Application.CreateForm(TCustomDrawTableViewDemoMainForm, CustomDrawTableViewDemoMainForm);
  Application.CreateForm(TCustomDrawTableViewDemoStylesEditorForm, CustomDrawTableViewDemoStylesEditorForm);
  Application.Run;
end.
