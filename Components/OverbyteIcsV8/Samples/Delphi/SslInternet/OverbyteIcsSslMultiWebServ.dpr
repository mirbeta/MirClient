program OverbyteIcsSslMultiWebServ;

uses
  Forms,
  OverbyteIcsSslMultiWebServ1 in 'OverbyteIcsSslMultiWebServ1.pas' {WeblServerForm},
  OverbyteIcsWebAppServerSessionData in '..\WebDemos\OverbyteIcsWebAppServerSessionData.pas',
  OverbyteIcsWebAppServerLogin in '..\WebDemos\OverbyteIcsWebAppServerLogin.pas',
  OverbyteIcsWebAppServerHelloWorld in '..\WebDemos\OverbyteIcsWebAppServerHelloWorld.pas',
  OverbyteIcsWebAppServerUrlDefs in '..\WebDemos\OverbyteIcsWebAppServerUrlDefs.pas',
  OverbyteIcsWebAppServerHttpHandlerBase in '..\WebDemos\OverbyteIcsWebAppServerHttpHandlerBase.pas',
  OverbyteIcsWebAppServerDataModule in '..\WebDemos\OverbyteIcsWebAppServerDataModule.pas' {WebAppSrvDataModule: TDataModule},
  OverbyteIcsWebAppServerCounter in '..\WebDemos\OverbyteIcsWebAppServerCounter.pas',
  OverbyteIcsWebAppServerHomePage in '..\WebDemos\OverbyteIcsWebAppServerHomePage.pas',
  OverbyteIcsWebAppServerConfig in '..\WebDemos\OverbyteIcsWebAppServerConfig.pas',
  OverbyteIcsWebAppServerCounterView in '..\WebDemos\OverbyteIcsWebAppServerCounterView.pas',
  OverbyteIcsWebAppServerHead in '..\WebDemos\OverbyteIcsWebAppServerHead.pas',
  OverbyteIcsWebAppServerMailer in '..\WebDemos\OverbyteIcsWebAppServerMailer.pas',
  OverbyteIcsWebAppServerUploads in 'OverbyteIcsWebAppServerUploads.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TWeblServerForm, WeblServerForm);
  Application.CreateForm(TWebAppSrvDataModule, WebAppSrvDataModule);
  Application.Run;
end.
