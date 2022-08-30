{
  @html(<b>)
  Remote Functions & Scripting Component Registration
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  RTC Remote Functions & Scripting Components are
  being registered to the Delphi component palette.

  @exclude
}
unit rtcRegisterFun;

{$INCLUDE rtcDefs.inc}

interface

// This procedure is being called by Delphi to register the components.
procedure Register;

implementation

uses
  Classes,

  rtcTypes,
  rtcScript,
  rtcCliModule,
  rtcSrvModule,
  rtcFunction;

procedure Register;
  begin
  RegisterComponents('RTC Server',[TRtcScriptEngine,
                                   TRtcServerModule,
                                   TRtcFunctionGroup,
                                   TRtcFunction]);

  RegisterComponents('RTC Client',[TRtcClientModule,
                                   TRtcResult]);
  end;

initialization
end.
