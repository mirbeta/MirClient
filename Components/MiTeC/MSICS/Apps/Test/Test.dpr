program Test;

uses
  Forms,
  Main in 'Main.pas' {wnd_msicstest_Main};

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.Title := 'MSICS Test';
  Application.CreateForm(Twnd_msicstest_Main, wnd_msicstest_Main);
  Application.Run;
end.
