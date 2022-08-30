{***********************************************************************}
{ DWM API import                                                        }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2009 - 2011                                    }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}
unit AdvDWM;

{$HPPEMIT ''}
{$HPPEMIT '#include "dwmapi.h"'}
{$HPPEMIT ''}

{$I TMSDEFS.INC}
{$T-}

interface

uses
  Windows;

const

  WM_DWMSENDICONICTHUMBNAIL = $323;
  {$EXTERNALSYM WM_DWMSENDICONICTHUMBNAIL}
  DWMWA_FORCE_ICONIC_REPRESENTATION = 7;
  {$EXTERNALSYM DWMWA_FORCE_ICONIC_REPRESENTATION}
  DWMWA_HAS_ICONIC_BITMAP = 10;
  {$EXTERNALSYM DWMWA_HAS_ICONIC_BITMAP}
  DWMWA_DISALLOW_PEEK = 11;
  {$EXTERNALSYM DWMWA_DISALLOW_PEEK}

  MSGFLT_ADD    = 1;
  {$EXTERNALSYM MSGFLT_ADD}

  MSGFLT_REMOVE = 2;
  {$EXTERNALSYM MSGFLT_REMOVE}


type
  _DWMMARGINS = record
    cxLeftWidth: Integer;
    cxRightWidth: Integer;
    cyTopHeight: Integer;
    cyBottomHeight: Integer;
  end {_DWMMARGINS};

  {$EXTERNALSYM _DWMMARGINS}

  DWMMARGINS = _DWMMARGINS;

  {$EXTERNALSYM DWMMARGINS}

  PDWMMARGINS = ^_DWMMARGINS;

  {$EXTERNALSYM PDWMMARGINS}

  TDWMMargins = DWMMARGINS;

  {$EXTERNALSYM TDWMMARGINS}



{$EXTERNALSYM DwmDefWindowProc}
function DwmDefWindowProc(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM;
  var plResult: LRESULT): BOOL;

{$EXTERNALSYM DwmExtendFrameIntoClientArea}
function DwmExtendFrameIntoClientArea(hWnd: HWND; const pMarInset: TDWMMargins): HResult;

{$EXTERNALSYM DwmIsCompositionEnabled}
function DwmIsCompositionEnabled(out pfEnabled: BOOL): HResult;

{$EXTERNALSYM DwmCompositionEnabled}
function DwmCompositionEnabled: Boolean;

{$EXTERNALSYM DwmSetWindowAttribute}
function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HResult; stdcall;

{$EXTERNALSYM DwmSetIconicThumbnail}
function DwmSetIconicThumbnail(hwnd: HWND; hbmp: HBITMAP; dwSITFlags: DWORD): HResult; stdcall;

{$EXTERNALSYM ChangeWindowMessageFilter}
function ChangeWindowMessageFilter(mes: integer; dwFlag: dword): Bool;

{$EXTERNALSYM PrintWindow}
function PrintWindow(hwnd: HWND; hdcBlt:  HDC; nFlags: UINT) : BOOL; stdcall;

implementation

uses
  SysUtils;

const
  ModName = 'DWMAPI.DLL';

var
  hDWMAPI: HMODULE;
  HUser32 : HMODULE;

var
  _DwmDefWindowProc: function(hWnd: HWND; msg: UINT; wParam: WPARAM;
    lParam: LPARAM; var plResult: LRESULT): BOOL; stdcall;

  _DwmExtendFrameIntoClientArea: function(hWnd: HWND;
    const pMarInset: TDWMMargins): HResult; stdcall;

  _DwmIsCompositionEnabled: function(out pfEnabled: BOOL): HResult; stdcall;

  _DwmSetWindowAttribute : function (hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer;
   cbAttribute: DWORD): HResult; stdcall;
  _DwmSetIconicThumbnail : function (hwnd: HWND; hbmp: HBITMAP; dwSITFlags: DWORD): HResult; stdcall;

  _ChangeWindowMessageFilter: function (mes: integer; dwFlag: dword): Bool; stdcall;
  _PrintWindow : function(hwnd : HWND; hdcBlt :  HDC; nFlags : UINT) : BOOL; stdcall;

//------------------------------------------------------------------------------

procedure InitDwmApi; //inline;
begin
  if hDWMAPI = 0 then
    hDWMAPI := LoadLibrary(ModName);
end;

//------------------------------------------------------------------------------

function DwmDefWindowProc(hWnd: HWND; msg: UINT; wParam: WPARAM;
  lParam: LPARAM; var plResult: LRESULT): BOOL;
begin
  if Assigned(_DwmDefWindowProc) then
    Result := _DwmDefWindowProc(hWnd, msg, wParam, lParam, plResult)
  else
  begin
    InitDwmApi;
    Result := False;
    if hDWMAPI > 0 then
    begin
      _DwmDefWindowProc := GetProcAddress(hDWMAPI, 'DwmDefWindowProc');
      if Assigned(_DwmDefWindowProc) then
        Result := _DwmDefWindowProc(hWnd, msg, wParam, lParam, plResult);
    end;
  end;
end;

//------------------------------------------------------------------------------

function DwmExtendFrameIntoClientArea(hWnd: HWND; const pMarInset: TDWMMargins): HResult;
begin
  if Assigned(_DwmExtendFrameIntoClientArea) then
    Result := _DwmExtendFrameIntoClientArea(hWnd, pMarInset)
  else
  begin
    InitDwmApi;
    Result := E_NOTIMPL;
    if hDWMAPI > 0 then
    begin
      _DwmExtendFrameIntoClientArea := GetProcAddress(hDWMAPI, 'DwmExtendFrameIntoClientArea');
      if Assigned(_DwmExtendFrameIntoClientArea) then
        Result := _DwmExtendFrameIntoClientArea(hWnd, pMarInset);
    end;
  end;
end;

//------------------------------------------------------------------------------

function DwmCompositionEnabled: Boolean;
var
  LEnabled: BOOL;
begin
  Result := (Win32MajorVersion >= 6) and (DwmIsCompositionEnabled(LEnabled) = S_OK) and LEnabled;
end;

//------------------------------------------------------------------------------

function DwmIsCompositionEnabled(out pfEnabled: BOOL): HResult;
begin
  if Assigned(_DwmIsCompositionEnabled) then
    Result := _DwmIsCompositionEnabled(pfEnabled)
  else
  begin
    InitDwmApi;
    Result := E_NOTIMPL;
    if hDWMAPI > 0 then
    begin
      _DwmIsCompositionEnabled := GetProcAddress(hDWMAPI, 'DwmIsCompositionEnabled');
      if Assigned(_DwmIsCompositionEnabled) then
        Result := _DwmIsCompositionEnabled(pfEnabled);
    end;
  end;
end;

//------------------------------------------------------------------------------

function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HResult; stdcall;
begin
  if Assigned(_DwmSetWindowAttribute) then
    Result := _DwmSetWindowAttribute(hwnd, dwAttribute, pvAttribute, cbAttribute)
  else
  begin
    InitDwmApi;
    Result := E_NOTIMPL;
    if hDWMAPI > 0 then
    begin
      _DwmSetWindowAttribute := GetProcAddress(HDwmApi, 'DwmSetWindowAttribute');
      if Assigned(_DwmSetWindowAttribute) then
        Result := _DwmSetWindowAttribute(hwnd, dwAttribute, pvAttribute, cbAttribute);
    end;
  end;
end;

//------------------------------------------------------------------------------

function DwmSetIconicThumbnail(hwnd: HWND; hbmp: HBITMAP; dwSITFlags: DWORD): HResult; stdcall;
begin
  if Assigned(_DwmSetIconicThumbnail) then
    Result := _DwmSetIconicThumbnail(hwnd, hbmp, dwSITFlags)
  else
  begin
    InitDwmApi;
    Result := E_NOTIMPL;
    if hDWMAPI > 0 then
    begin
      _DwmSetIconicThumbnail := GetProcAddress(HDwmApi, 'DwmSetIconicThumbnail');
      if Assigned(_DwmSetIconicThumbnail) then
        Result := _DwmSetIconicThumbnail(hwnd, hbmp, dwSITFlags);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure InitUser32;
begin
  if (Win32MajorVersion >= 7) or ((Win32MajorVersion = 6) and (Win32MinorVersion >=1)) then
    if (HUser32 = 0) then
      HUser32 :=  LoadLibrary('User32.dll');
end;

//------------------------------------------------------------------------------

function ChangeWindowMessageFilter(mes: integer; dwFlag: dword): Bool;
begin
  if Assigned(_ChangeWindowMessageFilter) then
    Result := _ChangeWindowMessageFilter(mes, dwFlag)
  else
  begin
    InitUser32;
    Result := False;
    if HUser32 <> 0 then
    begin
      _ChangeWindowMessageFilter := GetProcAddress(hUser32, 'ChangeWindowMessageFilter');
      if Assigned(_ChangeWindowMessageFilter) then
        Result := _ChangeWindowMessageFilter(mes, dwFlag);
    end;
  end;
end;

//------------------------------------------------------------------------------

function PrintWindow(hwnd: HWND; hdcBlt:  HDC; nFlags: UINT) : BOOL; stdcall;
begin
  if Assigned(_PrintWindow) then
    Result := _PrintWindow(hwnd, hdcBlt, nFlags)
  else
  begin
    InitUser32;
    Result := False;
    if HUser32 <> 0 then
    begin
      _PrintWindow := GetProcAddress(hUser32, 'PrintWindow');
      if Assigned(_PrintWindow) then
        Result := _PrintWindow(hwnd, hdcBlt, nFlags)
    end;
  end;
end;

//------------------------------------------------------------------------------

initialization

finalization
  if hDWMAPI > 0 then
  begin
    FreeLibrary(hDWMAPI);
    hDWMAPI := 0;

  end;
  if HUser32 <> 0 then
  begin
    FreeLibrary(HUser32);
    HUser32 := 0;
  end;
end.
