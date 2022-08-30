program WMIExp;

uses
  Forms,
  Main in 'Main.pas' {wnd_wmie_Main},
  Viewer in 'Viewer.pas' {mdi_wmie_Viewer},
  LoginDlg in 'LoginDlg.pas' {dlg_wmie_Login};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'MiTeC WMI Explorer';
  Application.CreateForm(Twnd_wmie_Main, wnd_wmie_Main);
  Application.Run;
end.
