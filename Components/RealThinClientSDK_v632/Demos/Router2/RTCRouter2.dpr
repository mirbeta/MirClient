program RTCRouter2;

{$include rtcDefs.inc}

uses
  {$ifdef rtcDeploy}
  FastMM4,
  {$endif}
  Forms,
  Unit1 in 'Unit1.pas' {RtcRouter2MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcRouter2MainForm, RtcRouter2MainForm);
  Application.Run;
end.
