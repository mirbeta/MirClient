program BT;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  Main;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Bluetooth Devices';
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
