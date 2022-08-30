{ @html(<b>)
  ISAPI Project Template
  @html(</b>)
  - Copyright 2004-2013 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  @exclude }
library AppISAPI;

{$include rtcDefs.inc}

uses
{$ifdef RtcDeploy}
  FastMM4,
  FastMove,
{$endif}
  ActiveX,
  ComObj,
  rtcISAPIApp,
  Isapi_Module in 'Isapi_Module.pas' {MyISAPI_Module: TDataModule},
  AppServer_Module in 'AppServer_Module.pas' {AppSrv_Module: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMyISAPI_Module, MyISAPI_Module);
  Application.Run;
end.
