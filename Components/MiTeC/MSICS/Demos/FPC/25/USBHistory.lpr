program USBHistory;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Main in 'Main.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'USB History';
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
