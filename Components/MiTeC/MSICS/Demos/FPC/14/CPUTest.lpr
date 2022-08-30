program CPUTest;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  Main;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Twnd_cputest_Main, wnd_cputest_Main);
  Application.Run;
end.
