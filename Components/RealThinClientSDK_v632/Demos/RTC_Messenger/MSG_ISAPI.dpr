{ @html(<b>)
  ISAPI Project Template
  @html(</b>)
  - Copyright 2004-2013 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  @exclude }
library MSG_ISAPI;

{$include rtcDefs.inc}

uses
{$ifdef rtcDeploy}
  FastMM4,
  FastMove,
{$endif}
  ActiveX,
  ComObj,
  rtcISAPIApp,
  ISAPI_Module in 'ISAPI_Module.pas' {ISAPIModule: TDataModule},
  rtcMessengerProvider in '..\DataProviders\rtcMessengerProvider.pas' {Messenger_Provider: TDataModule},
  rtcMessenger in '..\DataProviders\rtcMessenger.pas';

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.CreateForm(TISAPIModule, ISAPIModule);
  Application.Run;
end.
