{
  "RTC VCL - Thread.Synchronize for GUI Applications on Windows"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
   
   Use this unit anywhere in your iOS Project to make RTC connections
   work on iOS very similar to the way they are working on Windows.
   
  @exclude
}
unit rtcVCL_GUI;

interface

uses
  ExtCtrls, rtcTypes;

const
  RTC_FMXTIMER_INTERVAL=50;

implementation

{$include rtcFMX_GUI.inc}

end.
