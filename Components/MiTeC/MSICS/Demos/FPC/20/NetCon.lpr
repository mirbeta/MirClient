program NetCon;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  Main;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Single Process Monitor';
  Application.CreateForm(TwndMain, wndMain);
  Application.Run;
end.
