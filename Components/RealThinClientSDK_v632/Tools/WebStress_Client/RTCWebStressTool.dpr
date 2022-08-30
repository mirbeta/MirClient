program RTCWebStressTool;

{$include rtcDefs.inc}

uses
  // ScaleMM2,
  {$IFDEF Deploy}
  FastMM4,
  FastMove,
  {$ENDIF}
  Forms,
  WebStressTool_Unit in 'WebStressTool_Unit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
