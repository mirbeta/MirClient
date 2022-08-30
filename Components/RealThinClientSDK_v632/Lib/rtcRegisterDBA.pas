{
  @html(<b>)
  DB-Aware components Registration
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  RealThinClient DB-Aware components are being
  registered to Delphi component palette.
  
  @exclude
}
unit rtcRegisterDBA;

{$INCLUDE rtcDefs.inc}

interface

// This procedure is being called by Delphi to register the components.
procedure Register;

implementation

uses
  Classes,
  rtcTypes,
  rtcDB;

procedure Register;
  begin
  RegisterComponents('RTC DBA',[TRtcMemDataSet,
                                TRtcDataSetMonitor]);
  end;

end.
