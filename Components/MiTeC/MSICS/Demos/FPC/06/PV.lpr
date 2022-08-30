program PV;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  Main,
  PrcDetails,
  WinDetails,
  SvcDetails,
  SMDetails;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Process Viewer';
  Application.CreateForm(Twnd_Main, wnd_Main);
  Application.Run;
end.

