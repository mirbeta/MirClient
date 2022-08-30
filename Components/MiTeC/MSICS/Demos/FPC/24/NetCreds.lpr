program NetCreds;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Network Credentials';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
