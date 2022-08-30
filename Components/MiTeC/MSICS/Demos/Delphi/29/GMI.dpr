program GMI;

uses
  Forms,
  Main in 'Main.pas' {wnd_gai_Main};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Get Machine Info';
  Application.CreateForm(Twnd_gai_Main, wnd_gai_Main);
  Application.Run;
end.
