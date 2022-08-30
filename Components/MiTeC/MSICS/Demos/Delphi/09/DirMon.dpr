program DirMon;

uses
  Forms,
  Main in 'Main.pas' {wnd_dm_Main};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Directory Monitor';
  Application.CreateForm(Twnd_dm_Main, wnd_dm_Main);
  Application.Run;
end.
