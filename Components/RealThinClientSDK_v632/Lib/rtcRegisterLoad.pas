{
  @html(<b>)
  Router and Load Balancer Component Registration
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  RealThinClient Router and Load Balancer components
  are being registered to the Delphi component palette.

  @exclude
}
unit rtcRegisterLoad;

{$INCLUDE rtcDefs.inc}

interface

// This procedure is being called by Delphi to register the components.
procedure Register;

implementation

uses
  Classes,

  rtcTypes,
  rtcDataRoute,
  rtcLoadBalance;

procedure Register;
  begin
  RegisterComponents('RTC Server',[TRtcDataRouter,
                                   TRtcLoadBalancer]);
  end;

initialization
end.
