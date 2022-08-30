{
  ActiveX support
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}
unit rtcActiveX;

{$include rtcDefs.inc}

interface

uses
  Classes, ActiveX,
  rtcTypes, rtcLog, rtcThrPool;

type
  TRtcActiveX=class(TRtcThreadCallback)
    procedure AfterThreadStart; override;
    { Called from inside each Thread, before it will be stopped/destroyed }
    procedure BeforeThreadStop; override;
    { Callled after all threads have been stopped.
      This is the method from which you should destroy the object by calling "Free" }
    procedure DestroyCallback; override;
    end;

implementation

{ TRtcActiveX }

procedure TRtcActiveX.AfterThreadStart;
  begin
  CoInitialize(nil);
  end;

procedure TRtcActiveX.BeforeThreadStop;
  begin
  CoUninitialize;
  end;

procedure TRtcActiveX.DestroyCallback;
  begin
  Free;
  end;

initialization
{$IFDEF RTC_DEBUG} Log('rtcActiveX Initializing ...','DEBUG');{$ENDIF}

AddThreadCallback( TRtcActiveX.Create );

{$IFDEF RTC_DEBUG} Log('rtcActiveX Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcActiveX Finalized.','DEBUG');{$ENDIF}
end.
