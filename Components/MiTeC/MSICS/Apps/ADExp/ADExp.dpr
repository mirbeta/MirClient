program ADExp;

uses
  Forms,
  Main in 'Main.pas' {wnd_ade_Main};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'AD Explorer';
  Application.CreateForm(Twnd_ade_Main, wnd_ade_Main);
  Application.Run;
end.
