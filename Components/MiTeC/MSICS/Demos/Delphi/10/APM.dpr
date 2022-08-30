program APM;

uses
  Forms,
  Main in 'Main.pas' {wndMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Power Properties';
  Application.CreateForm(TwndMain, wndMain);
  Application.Run;
end.
