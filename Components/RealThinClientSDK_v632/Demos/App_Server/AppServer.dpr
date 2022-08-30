program AppServer;

//{$APPTYPE CONSOLE}

{$include rtcDeploy.inc}
{$include rtcDefs.inc}

{$MINSTACKSIZE 4000}
{$MAXSTACKSIZE 70000}

uses
{$IFDEF RtcDeploy}
  {$IFNDEF IDE_2006up}
  FastMM4,
  FastMove,
  {$ENDIF}
{$ENDIF}
  rtcTypes,
  Forms,
  Server_Form in 'Server_Form.pas' {Form1},
  AppServer_Module in 'AppServer_Module.pas' {AppSrv_Module: TDataModule};

{$R *.res}

begin
  {$IFDEF IDE_2006up}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
