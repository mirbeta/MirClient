{
  @html(<b>)
  Data-Link Component Registration
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  RealThinClient Data-Link components are being
  registered to Delphi component palette.
  
  @exclude
}
unit rtcRegisterLink;

{$INCLUDE rtcDefs.inc}

interface

// This procedure is being called by Delphi to register the components.
procedure Register;

implementation

uses
  Classes,

  rtcTypes,
  rtcLink,
  rtcDataCli,
  rtcDataSrv;

procedure Register;
  begin
  RegisterComponents('RTC Server',[TRtcDataServerLink,
                                   TRtcDualDataServerLink,
                                   TRtcDataProvider,
                                   TRtcLinkedModule]);

  RegisterComponents('RTC Client',[TRtcDataClientLink,
                                   TRtcDualDataClientLink,
                                   TRtcDataRequest]);
  end;

initialization
end.
