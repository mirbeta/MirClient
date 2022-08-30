program WinExplorerViewDemoD103Rio;

uses
  Forms,
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  CarsData in '..\Common\CarsData.pas' {dmCars},
  CarsDataForGrid in '..\Common\CarsDataForGrid.pas' {dmGridCars},
  WinExplorerViewDemoMain in 'WinExplorerViewDemoMain.pas' {frmMain};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid WinExplorer View Demo';
  Application.CreateForm(TdmGridCars, dmGridCars);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
