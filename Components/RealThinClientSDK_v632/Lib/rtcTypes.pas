{
  Base Types
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
}
unit rtcTypes;

{$INCLUDE rtcDefs.inc}

interface

const
  // Version of the RTC SDK components
  RTCSDK_VERSION='v6.32';

const
{$IFDEF WINDOWS}
  // System-specific Folder delimiter
  FOLDER_DELIMITER='\';
{$ELSE}
  // System-specific Folder delimiter
  FOLDER_DELIMITER='/';
{$ENDIF}
  
  // @exclude
  pidWin32        = $0001;
  // @exclude
  pidWin64        = $0002;
  // @exclude
  pidOSX32        = $0004;
  // @exclude
  pidiOSSimulator = $0008;
  // @exclude
  pidAndroid      = $0010;
  // @exclude
  pidLinux32      = $0020;
  // @exclude
  pidiOSDevice    = $0040;

  // @exclude
  pidWindows = pidWin32 or pidWin64;
  // @exclude
  pidDesktop = pidWindows or pidOSX32;
  // @exclude
  pidMobile  = pidiOSSimulator or pidiOSDevice or pidAndroid;
  // @exclude
  pidAll     = pidDesktop or pidMobile;

type
{$IFDEF AUTOREFCOUNT}
  // Array of bytes
  RtcByteArray = array of byte; // TArray<Byte>;
{$ELSE}
  // Array of bytes
  RtcByteArray = array of byte;
{$ENDIF}

{$IFDEF UNICODE}
  // @exclude
  RtcBinWideChar = Word;
  // Unicode String
  RtcWideString = String;
  // Unicode character
  RtcWideChar = Char;
{$ELSE}
  // @exclude
  RtcBinWideChar = Word;
  // Unicode String
  RtcWideString = WideString;
  // Unicode character
  RtcWideChar = WideChar;
{$ENDIF}

{$IFDEF NEXTGEN}
  // @exclude
  RtcBinChar = Word;
  // 8-bit character String (NOT UNICODE!)
  RtcString = String;
  // 8-bit character (NOT UNICODE!)
  RtcChar = Char;
  // @exclude
  RtcPtrAnsiChar = MarshaledAString;
  // @exclude
  RtcPtrWideChar = MarshaledString;
{$ELSE}
  // @exclude
  RtcPtrAnsiChar = PAnsiChar;
  // @exclude
  RtcPtrWideChar = PWideChar;
  {$IFDEF RTC_BYTESTRING}
    // @exclude
    RtcBinChar = Byte;
    {$IFDEF UNICODE}
      // 8-bit character String (NOT UNICODE!)
      RtcString = AnsiString;
      // 8-bit character (NOT UNICODE!)
      RtcChar = AnsiChar;
    {$ELSE}
      // 8-bit character String (NOT UNICODE!)
      RtcString = String;
      // 8-bit character (NOT UNICODE!)
      RtcChar = Char;
    {$ENDIF}
  {$ELSE}
    // @exclude
    RtcBinChar = Word;
    // 8-bit character String (NOT UNICODE!)
    RtcString = RtcWideString;
    // 8-bit character (NOT UNICODE!)
    RtcChar = RtcWideChar;
  {$ENDIF}
{$ENDIF}

{$IFDEF CPUX64}
  // @exclude
  RtcIntPtr = NativeUInt; // uint64;
  {$IFDEF FPC}
    // Thread ID
    RtcThrID = TThreadID;
  {$ELSE}
    // Thread ID
    RtcThrID = longword;
  {$ENDIF}
{$ELSE}{$IFDEF CPUX32}
  // @exclude
  RtcIntPtr = longword;
  {$IFDEF FPC}
    // Thread ID
    RtcThrID = TThreadID;
  {$ELSE}
    // Thread ID
    RtcThrID = longword;
  {$ENDIF}
{$ELSE}{$IFDEF CPUARM}
  // @exclude
  RtcIntPtr = NativeUInt;
  // Thread ID
  RtcThrID = NativeUInt;
{$ELSE}{$IFDEF FPC}
  // @exclude
  RtcIntPtr = uint64;
  // Thread ID
  RtcThrID = TThreadID;
{$ELSE}
  {$message error 'rtcTypes: Unknown POINTER Type'}
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}

const
  // @exclude
  SizeOf_Char = SizeOf(RtcChar);
  // @exclude
  SizeOf_WideChar = SizeOf(RtcWideChar);

var
  // @exclude
  RtcEmptyByteArray:RtcByteArray;

// FreeAndNil() replacement for the NextGen compiler
procedure RtcFreeAndNil(var obj);

implementation

procedure RtcFreeAndNil(var obj);
  var
    Temp:TObject absolute obj;
    i:RtcIntPtr absolute obj;
  begin
  if i=0 then Exit;
  {$IFDEF AUTOREFCOUNT}
    Temp.DisposeOf;
    i := 0;
  {$ELSE}
    Temp.Free;
    i := 0;
  {$ENDIF}
  end;

(*
// Implementation from the SysUtils unit ...
procedure RtcFreeAndNil(var obj);
{$IF not Defined(AUTOREFCOUNT)}
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;
{$ELSE}
begin
  TObject(Obj) := nil;
end;
{$ENDIF}
*)

initialization
SetLength(RtcEmptyByteArray,0);
end.