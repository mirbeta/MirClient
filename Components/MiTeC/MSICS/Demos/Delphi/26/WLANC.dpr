program WLANC;

uses
  Forms,
  Main in 'Main.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Known Wi-Fi Networks';
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
