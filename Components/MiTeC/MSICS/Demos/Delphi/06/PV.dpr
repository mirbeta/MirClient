program PV;

uses
  Forms,
  Main in 'Main.pas' {wnd_Main},
  PrcDetails in 'PrcDetails.pas' {dlg_PrcDetails},
  WinDetails in 'WinDetails.pas' {dlg_WinDetails},
  SvcDetails in 'SvcDetails.pas' {dlg_SvcDetails},
  SMDetails in 'SMDetails.pas' {dlg_SMDetails};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Process Viewer';
  Application.CreateForm(Twnd_Main, wnd_Main);
  Application.Run;
end.

