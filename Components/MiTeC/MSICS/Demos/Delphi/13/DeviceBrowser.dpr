program DeviceBrowser;

uses
  Forms,
  Main in 'Main.pas' {wnd_db_Main},
  ResourcesDlg in 'ResourcesDlg.pas' {dlg_db_Resources},
  DetailDlg in 'DetailDlg.pas' {dlg_db_Detail};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Device Browser';
  Application.CreateForm(Twnd_db_Main, wnd_db_Main);
  Application.Run;
end.
