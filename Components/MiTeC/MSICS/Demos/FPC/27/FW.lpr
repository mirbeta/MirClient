program FW;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Main in 'Main.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Windows Firewall';
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
