program CertInfo;

uses
  Forms,
  Main in 'Main.pas' {wndMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Certificate Information';
  Application.CreateForm(TwndMain, wndMain);
  Application.Run;
end.
