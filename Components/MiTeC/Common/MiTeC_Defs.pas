{*******************************************************}
{               MiTeC Type Definitions                  }
{                                                       }
{           Copyright (c) 2013-2018 Michal Mutl         }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Defs;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows;
     {$ELSE}
     Windows;
     {$ENDIF}

type
  {$IFNDEF BDS3PLUS}
  PCardinal = ^Cardinal;
  {$ENDIF}
  NTSTATUS = LONGINT;
  PPointer = ^Pointer;
  PVOID = Pointer;
  USHORT = Word;
  LONG = Integer;
  TLUID = Int64;
  PLUID = ^TLUID;
  PBOOLEAN = ^Boolean;
  CCHAR = Byte;
  HANDLE = THandle;

  {$if not defined(RAD5PLUS) and not defined(FPC)}
  ULONGLONG = UInt64;
  TBytes = array of Byte;
  {$ifend}

  {$IFNDEF RADPLUS}
  PPVOID = ^PVOID;
  {$ENDIF}

  {$IFNDEF NATIVEINT}
  PNativeUInt = ^NativeUInt;
  NativeUInt = LongWord;
  NativeInt = LongInt;
  UINT_PTR = NativeUInt;
  INT_PTR = NativeInt;
  WPARAM = UINT_PTR;
  LPARAM = INT_PTR;
  LRESULT = INT_PTR;
  LONG_PTR = NativeInt;
  ULONG_PTR = NativeUInt;
  PULONG_PTR = ^ULONG_PTR;
  DWORD_PTR = ULONG_PTR;
  HHOOK = type UINT_PTR;
  HANDLE_PTR = type NativeUInt;
  SIZE_T = ULONG_PTR;
  SSIZE_T = LONG_PTR;
  ULONG64 = UInt64;
  PULONG64 = ^ULONG64;
  {$ENDIF}

  {$IFDEF FPC}
  SSIZE_T = LONG_PTR;
  {$ENDIF}

  LPBYTE = PBYTE;

  GUID = TGUID;
  LPGUID = ^GUID;
  CLSID = TGUID;

  LPVOID = Pointer;
  LPCVOID = Pointer;
  LPLPVOID = ^LPVOID;

  PSIZE_T = ^SIZE_T;
  PSSIZE_T = ^SSIZE_T;

  LPINT = PINT;

  PVOID64 = Pointer;

  LPLPSTR = ^LPSTR;
  LPLPCSTR = ^LPCSTR;
  LPLPCWSTR = ^LPCWSTR;
  LPLPWSTR = ^LPWSTR;
  LPLPCTSTR = ^LPCTSTR;

  LPFILETIME = PFILETIME;

  KAFFINITY = ULONG_PTR;

implementation

end.
