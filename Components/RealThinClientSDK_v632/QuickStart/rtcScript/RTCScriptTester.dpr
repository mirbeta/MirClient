program RTCScriptTester;

{$include rtcDefs.inc}

uses
{$ifdef RtcDeploy}
  FastMM4,
  FastMove,
{$endif}
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
