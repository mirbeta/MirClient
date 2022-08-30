program DeviceBrowser;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  Main,
  ResourcesDlg,
  DetailDlg;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Device Browser';
  Application.CreateForm(Twnd_db_Main, wnd_db_Main);
  Application.Run;
end.
