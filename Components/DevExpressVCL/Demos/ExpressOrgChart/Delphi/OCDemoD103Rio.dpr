program OCDemoD103Rio;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Options in 'Options.pas' {OptionsForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.Run;
end.
