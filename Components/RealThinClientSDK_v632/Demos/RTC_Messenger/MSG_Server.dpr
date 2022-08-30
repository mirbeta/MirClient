program MSG_Server;

{$include rtcDefs.inc}

uses
{$ifdef rtcDeploy}
  FastMM4,
  FastMove,
{$endif}
  Forms,
  Server_MainForm in 'Server_MainForm.pas' {ServerMain},
  rtcMessengerProvider in '..\DataProviders\rtcMessengerProvider.pas' {Messenger_Provider: TDataModule},
  rtcMessenger in '..\DataProviders\rtcMessenger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServerMain, ServerMain);
  Application.Run;
end.
