{ If you want to debug the RTC SDK (including loading and unloading of units),
  declare the RTC_DEBUG compiler directive. }

{.$DEFINE RTC_DEBUG}



{ You want to use Windows Timers when compiling your code for Windows instead
  of using a cross-platform rtcTimer implementation which uses background thread?
  By declaring the "RTC_useWindowsTimers" compiler directive, the old TRtcTimer implementation
  which was using Windows Timers will be used when compiling your code for Windows (32bit and 64bit),
  while the new cross-platform implementation (using background threads) is used for other platforms. }

{.$DEFINE RTC_useWindowsTimers}



{ You want to remove Asynchronous WinSock API (Windows) support to reduce your applications size?
  Then you should declare the RTC_noAsynSock compiler directive in your app.
  NOTE: This only applies to applications compiled for Windows.
  On all other platforms, this compiler directive is ignored.

  If RTC_noAsynSock is declared, but RTC_noSynSock is NOT declared,
  blocking WinSock API will be used instead of the asynchronous WinSock API.

  WARNING: If you want to use Third-Party Encryption components for SSL support,
     you should NOT remove Asynchronous WinSock API support from your application,
     because Third-Party encryption components are working only in combination with
     the Asynchronous WinSock API on Windows. If you remove the Asynchronous WinSock API
     support from your Application, you will be making the "CryptPlugin" property on the
     TRtcHttpClient component useless. On Windows, you will still be able to get SSL support
     by using WinInet or WinHTTP APIs directly, but NOT by using third-party components with
     the CryptPlugin property. SSL support would then be handled by the WinInet or WinHTTP API
     directly and the CryptPlugin component would simply be ignored and not used. }

{.$DEFINE RTC_noAsynSock}



{ You want to remove Synchronous Sockets API (cross-platform) support to reduce your applications size?
  Then you should declare the RTC_noSynSock compiler directive in your app.
  NOTE: This applies to ALL applications compiled for Mac and Linux, as well as
  Windows applications compiled with the RTC_noAsynSock compiler directive declared. }

{.$DEFINE RTC_noSynSock}



{ You want to remove blocking WinInet API (Windows) support to reduce your applications size?
  Then you should declare the RTC_noWinInet compiler directive in your app.
  NOTE: This only applies to applications compiled for Windows.
  On all other platforms, this compiler directive is ignored.
  WARNING: If you disable WinInet and WinHTTP support, but are NOT using the CryptPlugin
     property in combination with third-party components for SSL support, you will NOT
     have support for SSL encryption in your Application. }

{.$DEFINE RTC_noWinInet}



{ You want to remove WinHTTP API (Windows) support to reduce your applications size?
  Then you should declare the RTC_noWinHTTP compiler directive in your app.
  NOTE: This only applies to applications compiled for Windows.
  On all other platforms, this compiler directive is ignored.
    WARNING: If you disable WinInet and WinHTTP support, but are NOT using the CryptPlugin
     property in combination with third-party components for SSL support, you will NOT
     have support for SSL encryption in your Application. }

{.$DEFINE RTC_noWinHTTP}



{ When "UseGUIDs" is defined, CoCreateGUID() API function is used
  to create Session IDs. Otherwise, a short string counter is used.
  For security reasons, use the GDI option (keep the directive defined): }

{$DEFINE UseGUIDs}



{ If you want to use the "TimeoutsOfAPI" property to set low-level API
  timeout parameters, make sure the RTC_USESETTIMEOUTS is declared. }

{$DEFINE RTC_UseSetTimeouts}



{ By default, PHP5.0 is used.
  If you are using PHP 4, undeclare this compiler directive: }

{$DEFINE PHP5} // PHP 5.0.x - seems to work with 5.0.3



{ If you are using a newer PHP 5 version, declare the one you are using }

{.$DEFINE PHP510} // PHP 5.1.x - not tested
{.$DEFINE PHP520} // PHP 5.2.x - seems to work with 5.2.17
{.$DEFINE PHP530} // PHP 5.3.x - not tested



{ You want to reduce traffic when working with XML-RPC, and the clients/servers
   you are working with understand that data enclosed in <value></value> is
   implied to be of type String? Then define this compiler directive: }

{.$DEFINE RTC_XMLRPC_VALUESTRING}



{ If you need all your new clients and servers to stay compatible with all
  older RTC clients and servers, even if this means disabling some new features
  from the latest RTC SDK, you simply need to uncomment this compiler directive: }

{.$DEFINE OldRtcCompatibilityMode}



{ RTC SDK versions up to 2.04 are expecting all Field names received with
  remote function calls to be in UpperCase. This is not the case with newer
  RTC SDK versions, which allow working with field names using mixed case,
  without internaly converting all field names to uppercase.

  If you are still using Clients or Servers compiled with older RTC SDK
  versions, you have to undeclare (comment out) the below declaration,
  to make sure all Field Names will be converted to UpperCase when sent
  out, so that older clients and servers won't have problems with the
  client/server compiled with THIS RTC SDK version.

  To send all Field Names in UpperCase and keep your new application
  compatible with apps compiled with all older RTC SDK versions,
  declare the compiler directive below (remove the dot): }

{.$DEFINE RtcUpperCaseFieldNames}

{$IFDEF OldRtcCompatibilityMode}
  {$DEFINE RtcUpperCaseFieldNames}
{$ENDIF}



{ RTC Clients compiled with RTC SDK versions *before* 2.27 do NOT send a
  control sum when using RTC functions with encryption, which makes it
  hard for the Server to check if the control key received from the client
  is a valid control key. Starting with RTC SDK 2.27, RTC Clients will be
  sending a control sum value along with each control key, which RTC Servers
  can use to check if the control key is valid and make sure at least the
  end of the request data was decrypted properly.

  But, since this control sum value is not being sent by RTC Clients compiled
  with RTC SDK versions older than 2.27, those clients would NOT work with
  a RTC Server compiled with the latest RTC SDK version if the Server would
  expect this control sum value to be sent by the Client.

  If you have RTC Clients compiled with RTC SDK 2.26 or older,
  you need to *uncomment* the below compiler directive, so that your RTC
  Server (when compiled with the laterst RTC SDK version) does NOT check
  for the control sum value, so it can work with older clients.
  To keep your Server backward-compatible, remove the dot: }

{.$DEFINE RtcDoNotCheckCryptControlSums}

{$IFDEF OldRtcCompatibilityMode}
  {$DEFINE RtcDoNotCheckCryptControlSums}
{$ENDIF}



{ All "AnsiString" references have been replaced with "RtcString" in the entire RTC SDK, and
  the "RtcString" type is now mapped to the "String" type by default instead of "AnsiString".

  If your existing code depends on the "AnsiString" type when compiled with Delphi 2009 and later,
  or if you want to use the "WideString" type with FPC or Delphi 6-2007 instead of AnsiString,
  you can *force* the RTC SDK to change the mapping of the "RtcString" type ...

  Declare the "FORCE_ANSI_STRING" compiler directive if you want the
  "RtcString" type to be mapped to the "AnsiString" type instead of "String".

  Or ...

  Declare the "FORCE_WIDE_STRING" compiler directive if you want the
  "RtcString" type to be mapped to the "WideString" type instead of "String".

  If both compiler directives ("FORCE_ANSI_STRING" and "FORCE_WIDE_STRING") are declared,
  "FORCE_ANSI_STRING" compiler directive will be used and "FORCE_WIDE_STRING" will be ignored. }

{.$DEFINE FORCE_ANSI_STRING} // set type RtcString = AnsiString ?
{.$DEFINE FORCE_WIDE_STRING} // set type RtcString = WideString ?

{$IFDEF OldRtcCompatibilityMode}
  {$DEFINE FORCE_ANSI_STRING}
{$ENDIF}


{ COMPRESS has to be declared if you want to allow the use of ZLib
  for automated compression when using RTC remote functions. }

{$DEFINE COMPRESS}



{ Declare the compiler directive "RTC_USEZLIB" (below) if you want to use the "ZLib.pas"
  unit from Delphi (or FPC) for compression and decompression instead of using the default
  RTC implementation which is based on a modified FastZLib version and uses zlib OBJ files
  on Windows and the "libz.dylib" library on Mac OSX and iOS platforms. }

{.$DEFINE RTC_USEZLIB}


// Quick Fix for the C++ Builder XE3 problem with the rtcDB.pas unit
{$DEFINE RTC_DSNOPS}


{***********************************************************
 !!! Here come the options which you should NEVER change !!!
************************************************************}


{ VER80  => Delphi 1 }
{ VER90  => Delphi 2 }
{ VER93  => Bcb 1    }
{ VER100 => Delphi 3 }
{ VER110 => Bcb 3    }

{ VER120 => Delphi 4 }
{ VER125 => Bcb 4    }
{ VER130 => Delphi 5 }
{ VER140 => Delphi 6 }
{ VER150 => Delphi 7 }
{ VER160 => Delphi 8 }
{ VER170 => Delphi / BDS 2005 }
{ VER180 => Delphi / BDS 2006 }
{ VER190 => Delphi / RAD 2007 }
{ VER200 => Delphi / RAD 2009 }
{ VER210 => Delphi / RAD 2010 }
{ VER220 => Delphi / RAD XE }
{ VER230 => Delphi / RAD XE2 }
{ VER240 => Delphi / RAD XE3 }
{ VER250 => Delphi / RAD XE4 }

{$V+}{$B-}{$X+}{$T-}{$P+}{$H+}{$J-}

{$IFNDEF FPC}
  {$DEFINE IDE_6up}
  {$DEFINE IDE_7up}
  {$DEFINE IDE_2005up}
  {$DEFINE IDE_2006up}
  {$DEFINE IDE_2007up}
  {$DEFINE IDE_2009up}
  {$DEFINE IDE_2010up}
  {$DEFINE IDE_XEup}
  {$DEFINE IDE_XE2up}
  {$DEFINE IDE_XE3up}
  {$DEFINE IDE_XE4up}
  {$DEFINE IDE_XE5up}
{$ENDIF}

{$IFDEF VER120}
  {$UNDEF IDE_6up}
  {$UNDEF IDE_7up}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER125}
  {$UNDEF IDE_6up}
  {$UNDEF IDE_7up}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER130}
  {$UNDEF IDE_6up}
  {$UNDEF IDE_7up}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER135}
  {$UNDEF IDE_6up}
  {$UNDEF IDE_7up}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER140}
  {$UNDEF IDE_7up}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER145}
  {$UNDEF IDE_7up}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER150}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER160}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER170}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER180}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER190}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER200}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER210}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER220}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER230}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER240}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF VER250}
  {$UNDEF IDE_XE5up}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$DEFINE WINDOWS}
{$ENDIF}
{$IFDEF WIN32}
  {$DEFINE WINDOWS}
  {$DEFINE CPUX86}
{$ENDIF}
{$IFDEF WIN64}
  {$DEFINE WINDOWS}
  {$DEFINE CPUX64}
{$ENDIF}

{$IFDEF IDE_XE4up}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$IFDEF UNICODE}
  {$IFDEF IDE_2009up}
    {$DEFINE UNICODE_2009up}
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$mode delphi}
  {$PACKRECORDS 1}
  {$HINTS OFF}
  {$DEFINE RTC_NOISAPI}
  {$DEFINE RTC_NOINTF}
  {$DEFINE FPC_POSIX}
{$ELSE}
  {$IFDEF POSIX}
    {$DEFINE FPC_POSIX}
  {$ENDIF}
  {$IFDEF MACOS}
    {$IFDEF IOS}
      {$DEFINE MACIOS}
    {$ELSE}
      {$DEFINE MACOSX}
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF WIN32} // The IDE (RAD Studio) currently only works on 32-bit Windows
    {$DEFINE RTC_NOINTF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF WINDOWS} // Asynchronous Sockets, WinInet, WinHTTP and ISAPI available only on Windows
  {$DEFINE RTC_noAsynSock}
  {$DEFINE RTC_noWinInet}
  {$DEFINE RTC_noWinHttp}
  {$DEFINE RTC_NOISAPI}
{$ENDIF}

{$DEFINE DESKTOP}
{$IFDEF ANDROID}
  {$UNDEF DESKTOP}
{$ENDIF}
{$IFDEF MACIOS}
  {$UNDEF DESKTOP}
{$ENDIF}

{$IFDEF BCB5}
  {$DEFINE RTC_NOINTF}
{$ENDIF}

{$IFDEF CPUX64}
  {$DEFINE RTC_NOASM}
  // Intel 64-bit
{$ELSE}
  {$IFDEF CPUX86}
    // Intel 32-bit
    {$DEFINE CPUX32}
  {$ELSE}
    {$IFDEF CPU386}
      // Intel 32-bit
      {$DEFINE CPUX32} // For backwards compatibility
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE RTC_NOASM}
    {.$DEFINE RTC_USEZLIB}
{$ENDIF}

{$IFDEF NEXTGEN}
  {$DEFINE RTC_NOASM}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE IDE_2}
  {$DEFINE IDE_1}
  {$DEFINE IDE_0}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE IDE_2}
  {$DEFINE IDE_1}
  {$DEFINE IDE_0}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE IDE_2}
  {$DEFINE IDE_1}
{$ENDIF}
{$IFDEF VER135}
  {$DEFINE IDE_2}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE IDE_2}
{$ENDIF}

{$IFNDEF IDE_1}
  {$IFNDEF FPC}
    {$WARN SYMBOL_PLATFORM OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF IDE_2}
  {$IFNDEF FPC}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$ENDIF}
{$ENDIF}

{$include rtcDeploy.inc}

{$UNDEF RTC_BYTESTRING}
{$UNDEF RTC_WIDESTRING}
{$IFDEF FORCE_ANSI_STRING}
  {$DEFINE RTC_BYTESTRING}
{$ELSE}
  {$IFNDEF UNICODE}
    {$IFDEF FORCE_WIDE_STRING}
      {$DEFINE RTC_WIDESTRING}
    {$ELSE}
      {$DEFINE RTC_BYTESTRING}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$UNDEF FPC_WIDESTRING}
{$IFDEF FPC}
  {$IFNDEF RTC_BYTESTRING}
    {$DEFINE FPC_WIDESTRING}
  {$ENDIF}
{$ENDIF}
