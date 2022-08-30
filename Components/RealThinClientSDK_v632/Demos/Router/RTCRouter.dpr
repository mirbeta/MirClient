program RTCRouter;

{$include rtcDefs.inc}

uses
  {$ifdef rtcDeploy}
  FastMM4,
  {$endif}
  Forms,
  Unit1 in 'Unit1.pas' {RtcRouterMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcRouterMainForm, RtcRouterMainForm);
  Application.Run;
end.
