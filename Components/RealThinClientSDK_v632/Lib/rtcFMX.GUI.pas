{
  "RTC FMX - Thread.Synchronize for GUI Applications on Mac OSX and Windows"
   - Copyright (c) 2004-2014 by Danijel Tkalcec (http://www.realthinclient.com)

   Use this unit anywhere in your Mac OSX Project to make RTC connections
   work on iOS very similar to the way they are working on Windows.

  @exclude
}
unit rtcFMX.GUI;

interface

uses
  FMX.Types, rtcTypes;

const
  RTC_FMXTIMER_INTERVAL=50;

implementation

{$include rtcFMX_GUI.inc}

end.
