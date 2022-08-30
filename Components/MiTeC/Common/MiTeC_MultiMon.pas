{*******************************************************}
{                MiTeC Common Routines                  }
{             Win32 Multi-monitor support               }
{                                                       }
{         Copyright (c) 1997-2019 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_MultiMon;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, WinApi.MultiMon
     {$else}
     Windows, SysUtils, MultiMon
     {$ENDIF}
     ;

const
  PHYSICAL_MONITOR_DESCRIPTION_SIZE = 128;
  EDD_GET_DEVICE_INTERFACE_NAME = $00000001;

  WM_DPICHANGED = 736; // 0x02E0

type
  {$IFNDEF RAD17PLUS}
  TDWordFiller = record
  {$IFDEF CPUX64}
    Filler: array[1..4] of Byte;
  {$ENDIF}
  end;

  TWMDpi = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    YDpi: Word;
    XDpi: Word;
    WParamFiller: TDWordFiller;
    ScaledRect: PRECT;
    Result: LRESULT;
  end;
  {$ENDIF}

  tagMONITORINFOEX = record
    cbSize: DWORD;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: DWORD;
    szDevice: array[0..CCHDEVICENAME - 1] of Char;
  end;
  MONITORINFOEX = tagMONITORINFOEX;
  LPMONITORINFOEX = ^tagMONITORINFOEX;
  PMonitorInfoEx = ^tagMONITORINFOEX;
  TMonitorInfoEx = tagMONITORINFOEX;

  _PHYSICAL_MONITOR = record
    hPhysicalMonitor: THandle;
    szPhysicalMonitorDescription: array[0..PHYSICAL_MONITOR_DESCRIPTION_SIZE-1] of WideChar;
  end;
  PHYSICAL_MONITOR = _PHYSICAL_MONITOR;
  LPPHYSICAL_MONITOR = ^_PHYSICAL_MONITOR;
  TPhysicalMonitor = _PHYSICAL_MONITOR;
  PPhysicalMonitor = LPPHYSICAL_MONITOR;

  TGetNumberOfPhysicalMonitorsFromHMONITOR = function(MonitorHandlke: HMONITOR; var pdwNumberOfPhysicalMonitors: Cardinal): BOOL; stdcall;
  TGetPhysicalMonitorsFromHMONITOR = function(MonitorHandle: HMONITOR; dwPhysicalMonitorArraySize: Cardinal; pPhysicalMonitorArray: PPhysicalMonitor): BOOL; stdcall;
  TDestroyPhysicalMonitor = function(MonitorHandle: HMONITOR): BOOL; stdcall;
  TDestroyPhysicalMonitors = function(MonitorHandle: HMONITOR; pPhysicalMonitorArray: PPhysicalMonitor): BOOL; stdcall;

var
  GetNumberOfPhysicalMonitorsFromHMONITOR: TGetNumberOfPhysicalMonitorsFromHMONITOR = nil;
  GetPhysicalMonitorsFromHMONITOR: TGetPhysicalMonitorsFromHMONITOR = nil;
  DestroyPhysicalMonitor: TDestroyPhysicalMonitor = nil;
  DestroyPhysicalMonitors: TDestroyPhysicalMonitors = nil;

function GetMonitorInfo(hMonitor: HMONITOR; lpMonitorInfo: PMonitorInfo): Boolean; stdcall;
function GetPixelsPerInch(hMonitor: HMONITOR): Integer;

implementation

uses MiTeC_Windows;

function GetMonitorInfo; external user32 name {$IFDEF UNICODE}'GetMonitorInfoW'{$ELSE}'GetMonitorInfoA'{$ENDIF};

function GetPixelsPerInch(hMonitor: HMONITOR): Integer;
var
  Ydpi: Cardinal;
  Xdpi: Cardinal;
  DC: HDC;
begin
  if Assigned(GetDpiForMonitor) then begin
    if GetDpiForMonitor(hMonitor, MDT_EFFECTIVE_DPI, Ydpi, Xdpi)=S_OK then
      Result:=Ydpi
    else
      Result := 0;
  end else begin
    DC:=GetDC(0);
    Result:=GetDeviceCaps(DC,LOGPIXELSY);
    ReleaseDC(0,DC);
  end;
end;

var
  Dxva2Dll: THandle;
initialization
  Dxva2Dll:=GetModuleHandle('Dxva2.dll');
  if Dxva2Dll=0 then
    Dxva2Dll:=LoadLibrary('Dxva2.dll');
  if Dxva2Dll<>0 then begin
    GetNumberOfPhysicalMonitorsFromHMONITOR:=GetProcAddress(Dxva2Dll,'GetNumberOfPhysicalMonitorsFromHMONITOR');
    GetPhysicalMonitorsFromHMONITOR:=GetProcAddress(Dxva2Dll,'GetPhysicalMonitorsFromHMONITOR');
    DestroyPhysicalMonitor:=GetProcAddress(Dxva2Dll,'DestroyPhysicalMonitor');
    DestroyPhysicalMonitors:=GetProcAddress(Dxva2Dll,'DestroyPhysicalMonitors');
  end;
end.
