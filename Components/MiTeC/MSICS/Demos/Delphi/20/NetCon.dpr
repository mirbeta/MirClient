program NetCon;

uses
  Forms,
  Main in 'Main.pas' {wndMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Single Process Monitor';
  Application.CreateForm(TwndMain, wndMain);
  Application.Run;
end.
