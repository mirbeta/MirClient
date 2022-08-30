{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxDWMApi;

{$I cxVer.inc}
{$ALIGN 1}
{$MINENUMSIZE 4}

interface

uses
  Windows, dxUxTheme;

const
  {$EXTERNALSYM WM_DWMCOMPOSITIONCHANGED}
  WM_DWMCOMPOSITIONCHANGED       = $0031E;
  {$EXTERNALSYM WM_DWMNCRENDERINGCHANGED}
  WM_DWMNCRENDERINGCHANGED       = $0031F;
  {$EXTERNALSYM WM_DWMCOLORIZATIONCOLORCHANGED}
  WM_DWMCOLORIZATIONCOLORCHANGED = $00320;
  {$EXTERNALSYM WM_DWMWINDOWMAXIMIZEDCHANGE}
  WM_DWMWINDOWMAXIMIZEDCHANGE    = $00321;
  {$EXTERNALSYM WM_DWMSENDICONICTHUMBNAIL}
  WM_DWMSENDICONICTHUMBNAIL         = $0323;
  {$EXTERNALSYM WM_DWMSENDICONICLIVEPREVIEWBITMAP}
  WM_DWMSENDICONICLIVEPREVIEWBITMAP = $0326;

  {$EXTERNALSYM DWM_EC_DISABLECOMPOSITION}
  DWM_EC_DISABLECOMPOSITION      = 0;
  {$EXTERNALSYM DWM_EC_ENABLECOMPOSITION}
  DWM_EC_ENABLECOMPOSITION       = 1;

  // Blur behind data structures
  {$EXTERNALSYM DWM_BB_ENABLE}
  DWM_BB_ENABLE                  = $00000001; // fEnable has been specified
  {$EXTERNALSYM DWM_BB_BLURREGION}
  DWM_BB_BLURREGION              = $00000002; // hRgnBlur has been specified}
  {$EXTERNALSYM DWM_BB_TRANSITIONONMAXIMIZED}
  DWM_BB_TRANSITIONONMAXIMIZED   = $00000004; // fTransitionOnMaximized has been specified

  // Window attributes
  {$EXTERNALSYM DWMWA_NCRENDERING_ENABLED}
  DWMWA_NCRENDERING_ENABLED         = 1; // [get] Is non-client rendering enabled/disabled
  {$EXTERNALSYM DWMWA_NCRENDERING_POLICY}
  DWMWA_NCRENDERING_POLICY          = 2; // [set] Non-client rendering policy
  {$EXTERNALSYM DWMWA_TRANSITIONS_FORCEDISABLED}
  DWMWA_TRANSITIONS_FORCEDISABLED   = 3; // [set] Potentially enable/forcibly disable transitions
  {$EXTERNALSYM DWMWA_ALLOW_NCPAINT}
  DWMWA_ALLOW_NCPAINT               = 4; // [set] Allow contents rendered in the non-client area to be visible on the DWM-drawn frame.
  {$EXTERNALSYM DWMWA_CAPTION_BUTTON_BOUNDS}
  DWMWA_CAPTION_BUTTON_BOUNDS       = 5; // [get] Bounds of the caption button area in window-relative space.
  {$EXTERNALSYM DWMWA_NONCLIENT_RTL_LAYOUT}
  DWMWA_NONCLIENT_RTL_LAYOUT        = 6; // [set] Is non-client content RTL mirrored
  {$EXTERNALSYM DWMWA_FORCE_ICONIC_REPRESENTATION}
  DWMWA_FORCE_ICONIC_REPRESENTATION = 7; // [set] Force this window to display iconic thumbnails.
  {$EXTERNALSYM DWMWA_FLIP3D_POLICY}
  DWMWA_FLIP3D_POLICY               = 8; // [set] Designates how Flip3D will treat the window.
  {$EXTERNALSYM DWMWA_EXTENDED_FRAME_BOUNDS}
  DWMWA_EXTENDED_FRAME_BOUNDS       = 9; // [get] Gets the extended frame bounds rectangle in screen space
  {$EXTERNALSYM DWM_SIT_DISPLAYFRAME }
  DWM_SIT_DISPLAYFRAME = $00000001;      // Display a window frame around the provided bitmap
  {$EXTERNALSYM DWMWA_HAS_ICONIC_BITMAP}
  DWMWA_HAS_ICONIC_BITMAP			      = 10;// The window can provide a bitmap for use by DWM as an iconic thumbnail or peek representation
  {$EXTERNALSYM DWMWA_DISALLOW_PEEK}
  DWMWA_DISALLOW_PEEK				        = 11;// Do not show peek preview for the window
  {$EXTERNALSYM DWMWA_EXCLUDED_FROM_PEEK}
  DWMWA_EXCLUDED_FROM_PEEK          = 12;// Prevents a window from fading to a glass sheet when the peek preview of another window is shown
  {$EXTERNALSYM DWMWA_LAST}
  DWMWA_LAST                        = 13;

type
  _DWM_BLURBEHIND = record
    dwFlags: DWORD;
    fEnable: Bool;
    hRgnBlur: HRGN;
    fTransitionOnMaximized: Bool;
  end;
  DWM_BLURBEHIND = _DWM_BLURBEHIND;
  PDWM_BLURBEHIND = ^_DWM_BLURBEHIND;

  // Non-client rendering policy attribute values
  TDWMNCRENDERINGPOLICY = (
    DWMNCRP_USEWINDOWSTYLE, // Enable/disable non-client rendering based on window style
    DWMNCRP_DISABLED,       // Disabled non-client rendering; window style is ignored
    DWMNCRP_ENABLED,        // Enabled non-client rendering; window style is ignored
    DWMNCRP_LAST);

var
  DwmDefWindowProc: function (wnd: HWND; msg: UINT; wParam: WPARAM;
    lParam: LPARAM; plResult: LRESULT): HRESULT; stdcall;
  DwmEnableBlurBehindWindow: function (wnd: HWND;
    pBlurBehind: PDWM_BLURBEHIND): HRESULT; stdcall;
  DwmEnableComposition: function (uCompositionAction: Boolean): HRESULT; stdcall;
  DwmEnableMMCSS: function (fEnableMMCSS: Boolean): HRESULT; stdcall;
  DwmExtendFrameIntoClientArea: function (wnd: HWND;
    pMarInset: PdxMargins): HRESULT; stdcall;
  DwmGetColorizationColor: function (out pcrColorization: DWORD;
    out pfOpaqueBlend: BOOL): HRESULT; stdcall;
  DwmGetWindowAttribute: function(hwnd: HWND; dwAttribute: DWORD;
    pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;
  DwmIsCompositionEnabled: function(out pfEnabled: BOOL): HRESULT; stdcall;
  DwmSetWindowAttribute: function(hwnd: HWND; dwAttribute: DWORD;
    pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;
  DwmSetIconicThumbnail: function(hwnd: HWND; hbmp: HBITMAP; dwSITFlags: DWORD): HRESULT;
  DwmSetIconicLivePreviewBitmap: function(hwnd: HWND; hbmp: HBITMAP; var pptClient: TPoint; dwSITFlags: DWORD): HRESULT;
  DwmInvalidateIconicBitmaps: function(hwnd: HWND): HRESULT;

function IsDwmPresent: Boolean;
function IsCompositionEnabled: Boolean;

implementation

uses
  SysUtils;

var
  dwmapiDLL: THandle;
  dwmPresent: Boolean;

function IsDwmPresent: Boolean;
begin
  Result := dwmPresent;
end;

function IsCompositionEnabled: Boolean;
var
  B: BOOL;
begin
  Result := IsDwmPresent;
  if Result then
  begin
    DwmIsCompositionEnabled(B);
    Result := B;
  end;
end;

function InitDWM: Boolean;
begin
  Result := False;
  if Win32MajorVersion < 6 then Exit;
  dwmapiDLL := LoadLibrary('dwmapi.dll');
  if dwmapiDLL <> 0 then
  begin
    DwmDefWindowProc := GetProcAddress(dwmapiDLL, 'DwmDefWindowProc');
    DwmEnableBlurBehindWindow := GetProcAddress(dwmapiDLL, 'DwmEnableBlurBehindWindow');
    DwmEnableComposition := GetProcAddress(dwmapiDLL, 'DwmEnableComposition');
    DwmEnableMMCSS := GetProcAddress(dwmapiDLL, 'DwmEnableMMCSS');
    DwmExtendFrameIntoClientArea := GetProcAddress(dwmapiDLL, 'DwmExtendFrameIntoClientArea');
    DwmGetColorizationColor := GetProcAddress(dwmapiDLL, 'DwmGetColorizationColor');
    DwmGetWindowAttribute := GetProcAddress(dwmapiDLL, 'DwmGetWindowAttribute');
    DwmIsCompositionEnabled := GetProcAddress(dwmapiDLL, 'DwmIsCompositionEnabled');
    DwmSetWindowAttribute := GetProcAddress(dwmapiDLL, 'DwmSetWindowAttribute');
    DwmSetIconicThumbnail := GetProcAddress(dwmapiDLL, 'DwmSetIconicThumbnail');
    DwmSetIconicLivePreviewBitmap := GetProcAddress(dwmapiDLL, 'DwmSetIconicLivePreviewBitmap');
    DwmInvalidateIconicBitmaps := GetProcAddress(dwmapiDLL, 'DwmInvalidateIconicBitmaps');
    Result := True;
  end;
end;

initialization
  dwmPresent := InitDWM;

finalization
  if dwmapiDLL <> 0 then
    FreeLibrary(dwmapiDLL);

end.
