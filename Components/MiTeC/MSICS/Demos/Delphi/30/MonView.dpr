program MonView;

uses
  Forms,
  Main in 'Main.pas' {wndMain};

{$R *.res}
{$R PM.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Monitor View';
  Application.CreateForm(TwndMain, wndMain);
  Application.Run;
end.
