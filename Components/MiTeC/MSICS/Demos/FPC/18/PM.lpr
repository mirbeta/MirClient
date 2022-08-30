program PM;

uses
  Interfaces,
  Forms,
  Main;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TwndMain, wndMain);
  Application.Run;
end.
