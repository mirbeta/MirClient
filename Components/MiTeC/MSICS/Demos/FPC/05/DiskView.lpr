program DiskView;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  Main;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Storage devices';
  Application.CreateForm(Twnd_dv_Main, wnd_dv_Main);
  Application.Run;
end.
