program VIEditor;

uses
  Forms,
  Main in 'Main.pas' {wnd_vie_Main};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Version Info Editor';
  Application.CreateForm(Twnd_vie_Main, wnd_vie_Main);
  Application.Run;
end.
