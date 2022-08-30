{
  @html(<b>)
  RTC Gateway components Registration
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  RealThinClient Gateway components are being
  registered to Delphi component palette.

  @exclude
}
unit rtcRegisterGate;

{$INCLUDE rtcDefs.inc}

interface

// This procedure is being called by Delphi to register the components.
procedure Register;

implementation

uses
  Classes,
  rtcTypes,

  rtcGateConst,
  rtcGateCli,
  rtcGateSrv;

procedure Register;
  begin
  RegisterComponents('RTC Gate',[TRtcGateway,
                                 TRtcHttpGateClient,
                                 TRtcGateClientLink]);
  end;

end.
