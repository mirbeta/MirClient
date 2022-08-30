program APM;

uses
  Interfaces,
  Forms,
  Main;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Power Properties';
  Application.CreateForm(TwndMain, wndMain);
  Application.Run;
end.
