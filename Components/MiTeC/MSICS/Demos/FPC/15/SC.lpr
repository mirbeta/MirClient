program SC;

{$MODE Delphi}

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
