program CarDashboardD103Rio;

uses
  Forms,
  uCarDashboard in 'uCarDashboard.pas' {frmCarDashboard};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TfrmCarDashboard, frmCarDashboard);
  Application.Run;
end.
