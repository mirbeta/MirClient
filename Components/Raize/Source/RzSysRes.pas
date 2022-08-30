{===============================================================================
  RzSysRes Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Description
  ------------------------------------------------------------------------------
  This is an interface unit to the RSRC32.DLL which provides a thunking layer to
  the Free System Resource values under Win32, specifically Win95 and Win98.
  Note that this unit is only valid under Win32 and that the RSRC32.DLL is
  not available on WinNT systems.


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * No changes.
===============================================================================}

{$I RzComps.inc}

unit RzSysRes;

interface

type
  {$Z4}
  TFreeSystemResources = ( gfsr_SystemResources, gfsr_GDIResources, gfsr_UserResources );


function GetFreeSystemResources( ResType: TFreeSystemResources ): Integer;

implementation

uses
  SysUtils,
  Windows;

type
  TFcnGetFreeSysRes = function( ResType: TFreeSystemResources ): Integer; stdcall;


var
  SysResModule: THandle = 0;
  FcnGetFreeSysRes: TFcnGetFreeSysRes = nil;


function GetFreeSystemResources( ResType: TFreeSystemResources ): Integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
  begin
    { Thunking down to 16-bit is only valid on Win95/Win98 }
    if SysResModule = 0 then
      SysResModule := LoadLibrary( 'RSRC32.DLL' );

    if @FcnGetFreeSysRes = nil then
      @FcnGetFreeSysRes := GetProcAddress( SysResModule,
                                           '_MyGetFreeSystemResources32@4' );

    if @FcnGetFreeSysRes <> nil then
      Result := FcnGetFreeSysRes( ResType )
    else
      Result := 0;
  end
  else
    Result := 0;
end;



initialization

finalization
  if SysResModule <> 0 then
    FreeLibrary( SysResModule );        { Be sure to release library when done }

end.
