{
  @html(<b>)
  HTTP Component Registration
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  RealThinClient HTTP components are being registered to Delphi component palette.
  
  @exclude
}
unit rtcRegisterHttp;

{$INCLUDE rtcDefs.inc}

interface

// This procedure is being called by Delphi to register the components.
procedure Register;

implementation

uses
  Classes,

  rtcTypes,

  rtcHttpSrv,
  rtcHttpCli;

procedure Register;
  begin
  RegisterComponents('RTC Server',[TRtcHttpServer]);

  RegisterComponents('RTC Client',[TRtcHttpClient]);
  end;

initialization
end.
