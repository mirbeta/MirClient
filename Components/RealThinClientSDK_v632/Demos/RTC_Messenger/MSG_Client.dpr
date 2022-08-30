program MSG_Client;

{$include rtcDefs.inc}

uses
{$ifdef rtcDeploy}
  FastMM4,
  FastMove,
{$endif}
  Forms,
  Client_ChatForm in 'Client_ChatForm.pas' {ChatForm},
  Client_MainForm in 'Client_MainForm.pas' {Form1};

{$R *.res}
{$R knock.res}
{$R door.res}
{$R click.res}
{$R chimeup.res}
{$R doorbell.res}
{$R eingang.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
