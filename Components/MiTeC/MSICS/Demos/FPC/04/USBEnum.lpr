program USBEnum;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  Main;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'USB Enumerator';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
