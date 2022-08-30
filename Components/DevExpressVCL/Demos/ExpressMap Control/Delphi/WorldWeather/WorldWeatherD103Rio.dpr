program WorldWeatherD103Rio;

uses
  Forms,
  WorldWeatherDemoAddCityDialog in 'WorldWeatherDemoAddCityDialog.pas' {WorldWeatherDemoAddCityDialogForm},
  BasicDemoMain in '..\Common\BasicDemoMain.pas' {frmBasicDemoMain},
  WorldWeatherDemoMain in 'WorldWeatherDemoMain.pas' {WorldWeatherDemoMainForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  LoadingSplash in 'LoadingSplash.pas' {frmLoading},
  WorldWeatherChangeVisibilityDialog in 'WorldWeatherChangeVisibilityDialog.pas' {WorldWeatherChangeVisibilityDialogForm},
  OpenWeatherMapService in 'OpenWeatherMapService.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  cxSetSplashVisibility(True, 'Weather info');
  Application.CreateForm(TWorldWeatherDemoMainForm, WorldWeatherDemoMainForm);
  cxSetSplashVisibility(False);
  Application.CreateForm(TWorldWeatherDemoAddCityDialogForm, WorldWeatherDemoAddCityDialogForm);
  Application.CreateForm(TWorldWeatherChangeVisibilityDialogForm, WorldWeatherChangeVisibilityDialogForm);
  Application.Run;
end.
