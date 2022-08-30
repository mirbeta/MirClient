program Handles;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Main;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TwndMain, wndMain);
  Application.Run;
end.
