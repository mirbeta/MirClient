program File_Client;

{$include rtcDefs.inc}

{$MAXSTACKSIZE 128000}

uses
{$ifdef rtcDeploy}
  {$IFNDEF IDE_2006up}
  FastMM4,
  FastMove,
  {$ENDIF}
{$endif}
  Forms,
  Client_Form in 'Client_Form.pas' {RtcFileClient};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcFileClient, RtcFileClient);
  Application.Run;
end.
