program RTCWebForum;

{$include rtcDefs.inc}

uses
{$ifdef rtcDeploy}
  FastMM4,
  FastMove,
{$endif}
  Forms,
  uSrv in '..\uSrv.pas' {frmServer},
  HTTP_Module in '..\HTTP_Module.pas' {HTTP_Server: TDataModule},
  rtcForumProvider in '..\rtcForumProvider.pas' {ForumProviderDM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmServer, frmServer);
  Application.OnException := frmServer.AppException;;
  Application.Run;
end.
