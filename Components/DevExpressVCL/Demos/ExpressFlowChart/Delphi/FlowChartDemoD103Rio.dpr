program FlowChartDemoD103Rio;

uses
  Forms,
  main in 'main.pas' {MainForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
