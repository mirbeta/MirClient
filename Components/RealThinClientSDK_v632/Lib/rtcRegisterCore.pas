{
  @html(<b>)
  Core Component Registration
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  RealThinClient Core components are being
  registered to Delphi component palette.

  @exclude
}
unit rtcRegisterCore;

{$INCLUDE rtcDefs.inc}

interface

// This procedure is being called by Delphi to register the components.
procedure Register;

implementation

uses
  Classes,

  rtcTypes,
  rtcLog,
  rtcSyncObjs,
  rtcThrPool;

procedure Register;
  begin
  RegisterComponents('RTC Server',[TRtcQuickJob]);
  end;

initialization
end.
