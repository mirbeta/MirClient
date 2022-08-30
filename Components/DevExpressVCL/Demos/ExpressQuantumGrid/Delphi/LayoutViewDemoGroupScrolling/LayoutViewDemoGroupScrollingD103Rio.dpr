program LayoutViewDemoGroupScrollingD103Rio;

uses
  Forms,
  CarsData in '..\Common\CarsData.pas' {dmCars},
  CarsDataForGrid in '..\Common\CarsDataForGrid.pas' {dmGridCars},
  LayoutViewDemoGroupScrollingMain in 'LayoutViewDemoGroupScrollingMain.pas' {frmMain},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid LayoutView Demo';
  Application.CreateForm(TdmGridCars, dmGridCars);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
