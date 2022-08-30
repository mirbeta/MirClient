{ @html(<b>)
  ISAPI Project Template
  @html(</b>)
  - Copyright 2004-2013 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  @exclude }
library FileISAPI;

{$include rtcDefs.inc}

uses
{$ifdef rtcDeploy}
  FastMM4,
  FastMove,
{$endif}
  ActiveX,
  ComObj,
  rtcISAPIApp,
  ISAPI_Module in 'ISAPI_Module.pas' {ISAPI_Server: TDataModule},
  rtcFileProvider in '..\DataProviders\rtcFileProvider.pas' {File_Provider: TDataModule};

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.CreateForm(TISAPI_Server, ISAPI_Server);
  Application.Run;
end.
