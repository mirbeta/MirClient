program RTCLoadBalancer2;

{$include rtcDefs.inc}

uses
  {$ifdef rtcDeploy}
  FastMM4,
  {$endif}
  Forms,
  Unit1 in 'Unit1.pas' {RtcLoadBalancerMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcLoadBalancerMainForm, RtcLoadBalancerMainForm);
  Application.Run;
end.
