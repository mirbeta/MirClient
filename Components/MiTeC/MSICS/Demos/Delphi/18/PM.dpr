program PM;

uses
  Forms,
  Main in 'Main.pas' {wndMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Process Monitor';
  Application.CreateForm(TwndMain, wndMain);
  Application.Run;
end.
