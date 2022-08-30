program Handles;

uses
  Forms,
  Main in 'Main.pas' {wndMain: TLMForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TwndMain, wndMain);
  Application.Run;
end.
