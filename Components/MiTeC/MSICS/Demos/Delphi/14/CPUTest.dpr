program CPUTest;

uses
  Forms,
  Main in 'Main.pas' {wnd_cputest_Main};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(Twnd_cputest_Main, wnd_cputest_Main);
  Application.Run;
end.
