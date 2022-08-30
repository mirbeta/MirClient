program DeviceMonitor;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  Main;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Device Monitor';
  Application.CreateForm(Twnd_dm_Main, wnd_dm_Main);
  Application.Run;
end.
