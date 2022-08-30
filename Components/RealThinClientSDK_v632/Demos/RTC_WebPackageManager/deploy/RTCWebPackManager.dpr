program RTCWebPackManager;

{$include rtcDefs.inc}

uses
{$ifdef rtcDeploy}
  FastMM4,
  FastMove,
{$endif}
  Forms,
  uSrv in '..\uSrv.pas' {frmServer},
  HTTP_Module in '..\HTTP_Module.pas' {HTTP_Server: TDataModule},
  uFileProvider in '..\uFileProvider.pas' {FileDM: TDataModule},
  uPageProvider in '..\uPageProvider.pas' {PageDM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmServer, frmServer);
  Application.OnException := frmServer.AppException;;
  Application.Run;
end.
