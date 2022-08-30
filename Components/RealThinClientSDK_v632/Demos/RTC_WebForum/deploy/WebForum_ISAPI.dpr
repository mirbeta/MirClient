{ @html(<b>)
  Web Forum ISAPI Project
  @html(</b>)
  - Copyright 2004-2013 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  @exclude }
library WebForum_ISAPI;

{$include rtcDefs.inc}

uses
{$ifdef rtcDeploy}
  FastMM4,
  FastMove,
{$endif}
  ActiveX,
  Forms,
  ComObj,
  rtcISAPIApp,
  ISAPI_Module in '..\ISAPI_Module.pas' {ISAPI_Server: TDataModule},
  rtcForumProvider in '..\rtcForumProvider.pas' {Forum_Provider: TDataModule};

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
