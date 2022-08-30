{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressXPThemeManager                                    }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSXPTHEMEMANAGER AND ALL         }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxUxTheme;

{$I cxVer.inc}

interface

uses
  Types, Windows, Classes, CommCtrl;

const
  dxMaxIntListCount = 10;

 {$EXTERNALSYM BPBF_COMPATIBLEBITMAP}
  BPBF_COMPATIBLEBITMAP = 0; // Compatible bitmap
 {$EXTERNALSYM BPBF_DIB}
  BPBF_DIB              = 1; // Device-independent bitmap
  {$EXTERNALSYM BPBF_TOPDOWNDIB}
  BPBF_TOPDOWNDIB       = 2; // Top-down device-independent bitmap
  {$EXTERNALSYM BPBF_TOPDOWNMONODIB}
  BPBF_TOPDOWNMONODIB   = 3; // Top-down monochrome device-independent bitmap
  {$EXTERNALSYM BPBF_COMPOSITED}
  BPBF_COMPOSITED = BPBF_TOPDOWNDIB;

type
  TdxTheme = THandle;

  TdxThemeSize = Integer;

   TdxMargins = packed record
    cxLeftWidth: Integer;
    cxRightWidth: Integer;
    cyTopHeight: Integer;
    cyBottomHeight: Integer;
  end;

  PdxMargins = ^TdxMargins;

  TdxIntList = packed record
    iValueCount: Integer;
    iValues: array[0..dxMaxIntListCount - 1] of Integer;
  end;
  PdxIntList = ^TdxIntList;

  TdxPropertyOrigin = (PO_STATE, PO_PART, PO_CLASS, PO_GLOBAL, PO_NOTFOUND);

// vista extension
  TdxPaintBuffer = THandle;

  TdxDTTOpts = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    crText: COLORREF;
    crBorder: COLORREF;
    crShadow: COLORREF;
    iTextShadowType: Integer;
    ptShadowOffset: TPoint;
    iBorderSize: Integer;
    iFontPropId: Integer;
    iColorPropId: Integer;
    iStateId: Integer;
    fApplyOverlay: BOOL;
    iGlowSize: Integer;
    pfnDrawTextCallback: DWORD;
    lParam: LPARAM;
  end;
  PdxDTTOpts = ^TdxDTTOpts;

function OpenThemeData(hWnd: HWND; pszClassList: PWideChar): TdxTheme;
function CloseThemeData(hTheme: TdxTheme): HRESULT;

// functions for basic drawing support
function DrawThemeBackground(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pRect: PRect; pClipRect: PRect = nil): HRESULT; overload;
function DrawThemeBackground(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const pRect: TRect; pClipRect: PRect = nil): HRESULT; overload;
function DrawThemeBackground(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const pRect: TRect; const pClipRect: TRect): HRESULT; overload;

function DrawThemeText(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pszText: PWideChar; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD;
  pRect: PRect): HRESULT; overload;
function DrawThemeText(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pszText: string; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD;
  const pRect: TRect): HRESULT; overload;

function DrawThemeTextEx(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const pszText: WideString; iCharCount: Integer; dwTextFlags: DWORD;
  const pRect: TRect; const pOptions: TdxDTTOpts): HRESULT;

function GetThemeBackgroundContentRect(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const BoundingRect: TRect; out ContentRect: TRect): HRESULT;

function GetThemeBackgroundExtent(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pContentRect: PRect;
  out ExtentRect: TRect): HRESULT;

function GetThemePartSize(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  prc: PRect; eSize: TdxThemeSize; psz: PSize): HRESULT; overload;
function GetThemePartSize(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const prc: TRect; eSize: TdxThemeSize; out psz: TSize): HRESULT; overload;
function GetThemePartSize(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  eSize: TdxThemeSize; out psz: TSize): HRESULT; overload;

function GetThemeTextExtent(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pszTextL: PWideChar; iCharCount: Integer; dwTextFlags: DWORD;
  pBoundingRect: PRect; out ExtentRect: TRect): HRESULT;
function GetThemeTextMetrics(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  out tm: TEXTMETRIC): HRESULT;
function GetThemeBackgroundRegion(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pRect: PRect; out Region: HRGN): HRESULT;
function HitTestThemeBackground(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  dwOptions: DWORD; pRect: PRect;
  hRgn: HRGN; ptTest: TPoint; out wHitTestCode: WORD): HRESULT;

function DrawThemeEdge(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pDestRect: PRect; uEdge, uFlags: UINT; pContentRect: PRect): HRESULT; overload;
function DrawThemeEdge(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const pDestRect: TRect; uEdge, uFlags: UINT; out pContentRect: TRect): HRESULT; overload;
function DrawThemeEdge(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const pDestRect: TRect; uEdge, uFlags: UINT): HRESULT; overload;

function DrawThemeIcon(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pRect: PRect; himl: HIMAGELIST; iImageIndex: Integer): HRESULT;
function IsThemePartDefined(hTheme: TdxTheme; iPartId, iStateId: Integer): BOOL;
function IsThemeBackgroundPartiallyTransparent(hTheme: TdxTheme;
  iPartId, iStateId: Integer): BOOL;
function DrawThemeParentBackground(hWnd: HWND; DC: HDC; const prc: TRect): HRESULT;

// lower-level theme information services
function GetThemeColor(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out Color: TColorRef): HRESULT;
function GetThemeMetric(hTheme: TdxTheme; DC: HDC; iPartId, iStateId, iPropId: Integer;
  out iVal: Integer): HRESULT;
function GetThemeString(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  pszBuff: PWideChar; cchMaxBuffChars: Integer): HRESULT;
function GetThemeBool(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out fVal{???}: BOOL): HRESULT; // TODO
function GetThemeInt(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out iVal: Integer): HRESULT;
function GetThemeEnumValue(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out iVal: Integer): HRESULT;
function GetThemePosition(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out Point: TPoint): HRESULT;
function GetThemeFont(hTheme: TdxTheme; DC: HDC; iPartId, iStateId, iPropId: Integer;
  out Font: LOGFONTW): HRESULT;
function GetThemeRect(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out Rect: TRect): HRESULT;
function GetThemeMargins(hTheme: TdxTheme; DC: HDC; iPartId, iStateId, iPropId: Integer;
  prc: PRect; out Margins: TdxMargins): HRESULT;
function GetThemeIntList(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out IntList: TdxIntList): HRESULT;
function GetThemePropertyOrigin(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out Origin: TdxPropertyOrigin): HRESULT;
function SetWindowTheme(hWnd: HWND; pszSubAppName, pszSubIdList:
  PWideChar): HRESULT;
function GetThemeFilename(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  pszThemeFileName: PWideChar; cchMaxBuffChars: Integer): HRESULT;
function GetThemeSysColor(hTheme: TdxTheme; iColorId: Integer): COLORREF;
function GetThemeSysColorBrush(hTheme: TdxTheme; iColorId: Integer): HBRUSH;
function GetThemeSysBool(hTheme: TdxTheme; iBoolId: Integer): BOOL;
function GetThemeSysSize(hTheme: TdxTheme; iSizeId: Integer): Integer;
function GetThemeSysFont(hTheme: TdxTheme; iFontId: Integer; out lf: LOGFONTW): HRESULT;
function GetThemeSysString(hTheme: TdxTheme; iStringId: Integer; pszStringBuff:
  PWideChar; cchMaxStringChars: Integer): HRESULT;
function GetThemeSysInt(hTheme: TdxTheme; iIntId: Integer; var iValue: Integer): HRESULT; // TODO var -> out ???

function IsThemeActive: BOOL;
function IsAppThemed: BOOL;
function GetWindowTheme(hWnd: HWND): TdxTheme;
function EnableThemeDialogTexture(hWnd: HWND; dwFlags: DWORD): HRESULT;
function IsThemeDialogTextureEnabled(hWnd: HWND): BOOL;
function GetThemeAppProperties: DWORD;
procedure SetThemeAppProperties(dwFlags: DWORD);
function GetCurrentThemeName(pszThemeFileName: PWideChar;
  cchMaxNameChars: Integer; pszColorBuff: PWideChar;
  cchMaxColorChars: Integer; pszSizeBuff: PWideChar;
  cchMaxSizeChars: Integer): HRESULT;
function GetThemeDocumentationProperty(
  pszThemeName, pszPropertyName, pszValueBuff: PWideChar;
  cchMaxValChars: Integer): HRESULT;

//vista extension
type
  TdxBPPaintParams = packed record
    cbSize: DWORD;
    dwFlags: DWORD; // BPPF_ flags
    prcExclude: PRect;
    pBlendFunction: Pointer;
  end;
  PdxBPPaintParams = ^TdxBPPaintParams;

function BufferedPaintInit: HRESULT;
function BufferedPaintUnInit: HRESULT;
function BeginBufferedPaint(hdcTarget: HDC; prcTarget: PRect;
  dwFormat: DWORD; pPaintParams: PdxBPPaintParams; out dc: HDC): TdxPaintBuffer;
function EndBufferedPaint(hBufferedPaint: TdxPaintBuffer; fUpdateTarget: BOOL): HRESULT;
function BufferedPaintClear(hBufferedPaint: TdxPaintBuffer; const R: TRect): HRESULT;
function BufferedPaintSetAlpha(hBufferedPaint: TdxPaintBuffer; prc: PRect; alpha: Byte): HRESULT;

function IsThemeLibraryLoaded: Boolean;

function dxBeginPanningFeedback(AWindow: THandle): Boolean;
function dxUpdatePanningFeedback(AWindow: THandle; AOverpan: TPoint; AIsInertia: Boolean): Boolean;
function dxEndPanningFeedback(AWindow: THandle; AAnimateBack: Boolean): Boolean;

implementation

uses
  ComCtrls;

const
  ThemeLibraryName = 'UxTheme.dll';

type
  TCloseThemeData = function(hTheme: TdxTheme): HRESULT; stdcall;
  TDrawThemeBackground = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
    pRect: PRect; pClipRect: PRect = nil): HRESULT; stdcall;
  TDrawThemeEdge = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
    pDestRect: PRect; uEdge, uFlags: UINT; pContentRect: PRect): HRESULT; stdcall;
  TDrawThemeIcon = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
    pRect: PRect; himl: HIMAGELIST; iImageIndex: Integer): HRESULT; stdcall;
  TDrawThemeParentBackground = function(hWnd: HWND; DC: HDC; const prc: TRect): HRESULT; stdcall;
  TDrawThemeText = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
    pszText: PWideChar; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD;
    pRect: PRect): HRESULT; stdcall;
  TDrawThemeTextEx = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
    const pszText: WideString; iCharCount: Integer; dwTextFlags: DWORD;
    pRect: PRect; const pOptions: TdxDTTOpts): HRESULT; stdcall;
  TEnableThemeDialogTexture = function(hWnd: HWND; dwFlags: DWORD): HRESULT; stdcall;
  TGetCurrentThemeName = function(pszThemeFileName: PWideChar; cchMaxNameChars: Integer;
    pszColorBuff: PWideChar; cchMaxColorChars: Integer; pszSizeBuff: PWideChar;
    cchMaxSizeChars: Integer): HRESULT; stdcall;
  TGetThemeAppProperties = function: DWORD; stdcall;
  TGetThemeBackgroundContentRect = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
    const BoundingRect: TRect; out ContentRect: TRect): HRESULT; stdcall;
  TGetThemeBackgroundRegion = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
    pRect: PRect; out Region: HRGN): HRESULT; stdcall;
  TGetThemeBackgroundExtent = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
    pContentRect: PRect; out ExtentRect: TRect): HRESULT; stdcall;
  TGetThemeBool = function(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
    out fVal: BOOL): HRESULT; stdcall;
  TGetThemeColor = function(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
    out Color: TColorRef): HRESULT; stdcall;
  TGetThemeDocumentationProperty = function(pszThemeName, pszPropertyName, pszValueBuff: PWideChar;
    cchMaxValChars: Integer): HRESULT; stdcall;
  TGetThemeEnumValue = function(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
    out iVal: Integer): HRESULT; stdcall;
  TGetThemeFilename = function(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
    pszThemeFileName: PWideChar; cchMaxBuffChars: Integer): HRESULT; stdcall;
  TGetThemeFont = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId, iPropId: Integer;
    out pFont: LOGFONTW): HRESULT; stdcall;
  TGetThemeInt = function(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
    out iVal: Integer): HRESULT; stdcall;
  TGetThemeIntList = function(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
    out IntList: TdxIntList): HRESULT; stdcall;
  TGetThemeMargins = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId, iPropId: Integer;
    prc: PRect; out Margins: TdxMargins): HRESULT; stdcall;
  TGetThemeMetric = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId, iPropId: Integer;
    out iVal: Integer): HRESULT; stdcall;
  TGetThemePartSize = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
    prc: PRect; eSize: TdxThemeSize; psz: PSize): HRESULT; stdcall;
  TGetThemePosition = function(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
    out Point: TPoint): HRESULT; stdcall;
  TGetThemePropertyOrigin = function(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
    out Origin: TdxPropertyOrigin): HRESULT; stdcall;
  TGetThemeRect = function(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
    out Rect: TRect): HRESULT; stdcall;
  TGetThemeString = function(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
    pszBuff: PWideChar; cchMaxBuffChars: Integer): HRESULT; stdcall;
  TGetThemeSysBool = function(hTheme: TdxTheme; iBoolId: Integer): BOOL; stdcall;
  TGetThemeSysColor = function(hTheme: TdxTheme; iColorId: Integer): COLORREF; stdcall;
  TGetThemeSysColorBrush = function(hTheme: TdxTheme; iColorId: Integer): HBRUSH; stdcall;
  TGetThemeSysFont = function(hTheme: TdxTheme; iFontId: Integer; out lf: LOGFONTW): HRESULT; stdcall;
  TGetThemeSysInt = function(hTheme: TdxTheme; iIntId: Integer; var iValue: Integer): HRESULT; stdcall;
  TGetThemeSysSize = function(hTheme: TdxTheme; iSizeId: Integer): Integer; stdcall;
  TGetThemeSysString = function(hTheme: TdxTheme; iStringId: Integer; pszStringBuff: PWideChar;
    cchMaxStringChars: Integer): HRESULT; stdcall;
  TGetThemeTextExtent = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
    pszTextL: PWideChar; iCharCount: Integer; dwTextFlags: DWORD;
    pBoundingRect: PRect; out ExtentRect: TRect): HRESULT; stdcall;
  TGetThemeTextMetrics = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
    out tm: TEXTMETRIC): HRESULT; stdcall;
  TGetWindowTheme = function(hWnd: HWND): TdxTheme;stdcall;
  THitTestThemeBackground = function(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
    dwOptions: DWORD; pRect : PRect; hRgn: HRGN; ptTest: TPoint;
    out wHitTestCode: WORD): HRESULT; stdcall;
  TIsAppThemed = function: BOOL; stdcall;
  TIsThemeActive = function: BOOL; stdcall;
  TIsThemePartDefined = function(hTheme: TdxTheme; iPartId, iStateId: Integer): BOOL; stdcall;
  TIsThemeBackgroundPartiallyTransparent = function(hTheme: TdxTheme;
    iPartId, iStateId: Integer): BOOL; stdcall;
  TIsThemeDialogTextureEnabled = function(hWnd: HWND): BOOL; stdcall;
  TOpenThemeData = function(hWnd: HWND; pszClassList: PWideChar): TdxTheme; stdcall;
  TSetThemeAppProperties = procedure(dwFlags: DWORD); stdcall;
  TSetWindowTheme = function(hWnd: HWND; pszSubAppName, pszSubIdList: PWideChar): HRESULT; stdcall;
  //vista extension
  TBufferedPaintInit = function: HRESULT; stdcall;
  TBufferedPaintUnInit = function: HRESULT; stdcall;
  TBeginBufferedPaint = function(hdcTarget: HDC; prcTarget: PRect;
    dwFormat: DWORD; pPaintParams: PdxBPPaintParams; var dc: HDC): TdxPaintBuffer; stdcall;
  TEndBufferedPaint = function(hBufferedPaint: TdxPaintBuffer; fUpdateTarget: BOOL): HRESULT; stdcall;
  TBufferedPaintClear = function(hBufferedPaint: TdxPaintBuffer; const R: TRect): HRESULT; stdcall;
  TBufferedPaintSetAlpha = function(hBufferedPaint: TdxPaintBuffer; prc: PRect; alpha: Byte): HRESULT; stdcall;

  TThemeLibraryMethodPointersKeeper = record
    FCloseThemeData: TCloseThemeData;
    FDrawThemeBackground: TDrawThemeBackground;
    FDrawThemeEdge: TDrawThemeEdge;
    FDrawThemeIcon: TDrawThemeIcon;
    FDrawThemeParentBackground: TDrawThemeParentBackground;
    FDrawThemeText: TDrawThemeText;
    FDrawThemeTextEx: TDrawThemeTextEx;
    FEnableThemeDialogTexture: TEnableThemeDialogTexture;
    FGetCurrentThemeName: TGetCurrentThemeName;
    FGetThemeAppProperties: TGetThemeAppProperties;
    FGetThemeBackgroundContentRect: TGetThemeBackgroundContentRect;
    FGetThemeBackgroundRegion: TGetThemeBackgroundRegion;
    FGetThemeBackgroundExtent: TGetThemeBackgroundExtent;
    FGetThemeBool: TGetThemeBool;
    FGetThemeColor: TGetThemeColor;
    FGetThemeDocumentationProperty: TGetThemeDocumentationProperty;
    FGetThemeEnumValue: TGetThemeEnumValue;
    FGetThemeFilename: TGetThemeFilename;
    FGetThemeFont: TGetThemeFont;
    FGetThemeInt: TGetThemeInt;
    FGetThemeIntList: TGetThemeIntList;
    FGetThemeMargins: TGetThemeMargins;
    FGetThemeMetric: TGetThemeMetric;
    FGetThemePartSize: TGetThemePartSize;
    FGetThemePosition: TGetThemePosition;
    FGetThemePropertyOrigin: TGetThemePropertyOrigin;
    FGetThemeRect: TGetThemeRect;
    FGetThemeString: TGetThemeString;
    FGetThemeSysBool: TGetThemeSysBool;
    FGetThemeSysColor: TGetThemeSysColor;
    FGetThemeSysColorBrush: TGetThemeSysColorBrush;
    FGetThemeSysFont: TGetThemeSysFont;
    FGetThemeSysInt: TGetThemeSysInt;
    FGetThemeSysSize: TGetThemeSysSize;
    FGetThemeSysString: TGetThemeSysString;
    FGetThemeTextExtent: TGetThemeTextExtent;
    FGetThemeTextMetrics: TGetThemeTextMetrics;
    FGetWindowTheme: TGetWindowTheme;
    FHitTestThemeBackground: THitTestThemeBackground;
    FIsAppThemed: TIsAppThemed;
    FIsThemeActive: TIsThemeActive;
    FIsThemePartDefined: TIsThemePartDefined;
    FIsThemeBackgroundPartiallyTransparent: TIsThemeBackgroundPartiallyTransparent;
    FIsThemeDialogTextureEnabled: TIsThemeDialogTextureEnabled;
    FOpenThemeData: TOpenThemeData;
    FSetThemeAppProperties: TSetThemeAppProperties;
    FSetWindowTheme: TSetWindowTheme;
    //vista extension
    FBeginBufferedPaint: TBeginBufferedPaint;
    FBufferedPaintClear: TBufferedPaintClear;
    FBufferedPaintInit: TBufferedPaintInit;
    FBufferedPaintSetAlpha: TBufferedPaintSetAlpha;
    FBufferedPaintUnInit: TBufferedPaintUnInit;
    FEndBufferedPaint: TEndBufferedPaint;
  end;

var
  FThemeLibrary: HMODULE = 0;
  FThemeLibraryMethodPointersKeeper: TThemeLibraryMethodPointersKeeper;
  //touch support
  FBeginPanningFeedback: function(hwnd: HWND): BOOL; stdcall;
  FUpdatePanningFeedback: function(hwnd: HWND; lTotalOverpanOffsetX: Integer;
    lTotalOverpanOffsetY: Integer; fInInertia: BOOL): BOOL; stdcall;
  FEndPanningFeedback: function(hwnd: HWND; fAnimateBack: BOOL): BOOL; stdcall;

function OpenThemeData(hWnd: HWND; pszClassList: PWideChar): TdxTheme;
begin
  Result := FThemeLibraryMethodPointersKeeper.FOpenThemeData(hWnd, pszClassList);
end;

function CloseThemeData(hTheme: TdxTheme): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FCloseThemeData(hTheme);
end;

function DrawThemeBackground(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pRect: PRect; pClipRect: PRect = nil
): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FDrawThemeBackground(hTheme, DC,
    iPartId, iStateId, pRect, pClipRect);
end;
function DrawThemeBackground(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const pRect: TRect; pClipRect: PRect = nil): HRESULT;
begin
  Result := DrawThemeBackground(hTheme, DC, iPartId, iStateId, @pRect, pClipRect);
end;

function DrawThemeBackground(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const pRect: TRect; const pClipRect: TRect): HRESULT;
begin
  Result := DrawThemeBackground(hTheme, DC, iPartId, iStateId, @pRect, @pClipRect);
end;

function DrawThemeText(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pszText: PWideChar;
  iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD;
  pRect: PRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FDrawThemeText(hTheme, DC, iPartId, iStateId,
    pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect);
end;

function DrawThemeText(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pszText: string; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD;
  const pRect: TRect): HRESULT;
begin
  Result := DrawThemeText(hTheme, DC, iPartId, iStateId,
    PWideChar(WideString(pszText)), iCharCount, dwTextFlags, dwTextFlags2, @pRect);
end;

//vista extension
function DrawThemeTextEx(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const pszText: WideString; iCharCount: Integer; dwTextFlags: DWORD;
  const pRect: TRect; const pOptions: TdxDTTOpts): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FDrawThemeTextEx(hTheme, DC, iPartId, iStateId,
    PWideChar(pszText), iCharCount, dwTextFlags, @pRect, pOptions);
end;

function GetThemeBackgroundContentRect(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const BoundingRect: TRect; out ContentRect: TRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeBackgroundContentRect(hTheme,
    DC, iPartId, iStateId, BoundingRect, ContentRect);
end;

function GetThemeBackgroundExtent(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pContentRect: PRect;
  out ExtentRect: TRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeBackgroundExtent(hTheme, DC,
    iPartId, iStateId, pContentRect, ExtentRect);
end;

function GetThemePartSize(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  prc: PRect; eSize: TdxThemeSize;
  psz: PSize): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemePartSize(hTheme, DC, iPartId,
    iStateId, prc, eSize, psz);
end;

function GetThemePartSize(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const prc: TRect; eSize: TdxThemeSize; out psz: TSize): HRESULT;
begin
  Result := GetThemePartSize(hTheme, DC, iPartId, iStateId, @prc, eSize, @psz);
end;

function GetThemePartSize(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  eSize: TdxThemeSize; out psz: TSize): HRESULT;
begin
  Result := GetThemePartSize(hTheme, DC, iPartId, iStateId, nil, eSize, @psz);
end;

function GetThemeTextExtent(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pszTextL: PWideChar; iCharCount: Integer; dwTextFlags: DWORD;
  pBoundingRect: PRect; out ExtentRect: TRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeTextExtent(hTheme, DC, iPartId,
    iStateId, pszTextL, iCharCount, dwTextFlags, pBoundingRect, ExtentRect);
end;

function GetThemeTextMetrics(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  out tm: TEXTMETRIC): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeTextMetrics(hTheme, DC, iPartId,
    iStateId, tm);
end;

function GetThemeBackgroundRegion(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pRect: PRect; out Region: HRGN): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeBackgroundRegion(hTheme,
    DC, iPartId, iStateId, pRect, Region);
end;

function HitTestThemeBackground(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  dwOptions: DWORD; pRect: PRect;
  hRgn: HRGN; ptTest: TPoint; out wHitTestCode: WORD): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FHitTestThemeBackground(hTheme, DC,
    iPartId, iStateId, dwOptions, pRect, hRgn, ptTest, wHitTestCode);
end;

function DrawThemeEdge(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pDestRect: PRect;
  uEdge, uFlags: UINT; pContentRect: PRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FDrawThemeEdge(hTheme, DC, iPartId, iStateId,
    pDestRect, uEdge, uFlags, pContentRect);
end;

function DrawThemeEdge(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const pDestRect: TRect; uEdge, uFlags: UINT; out pContentRect: TRect): HRESULT;
begin
  Result := DrawThemeEdge(hTheme, DC, iPartId, iStateId, @pDestRect, uEdge, uFlags, @pContentRect);
end;

function DrawThemeEdge(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  const pDestRect: TRect; uEdge, uFlags: UINT): HRESULT;
var
  R: TRect;
begin
  Result := DrawThemeEdge(hTheme, DC, iPartId, iStateId, pDestRect, uEdge, uFlags, R);
end;

function DrawThemeIcon(hTheme: TdxTheme; DC: HDC; iPartId, iStateId: Integer;
  pRect: PRect; himl: HIMAGELIST; iImageIndex: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FDrawThemeIcon(hTheme, DC, iPartId, iStateId,
    pRect, himl, iImageIndex);
end;

function IsThemePartDefined(hTheme: TdxTheme; iPartId: Integer; iStateId: Integer): BOOL;
begin
  Result := FThemeLibraryMethodPointersKeeper.FIsThemePartDefined(hTheme, iPartId, iStateId);
end;

function IsThemeBackgroundPartiallyTransparent(hTheme: TdxTheme;
  iPartId: Integer; iStateId: Integer): BOOL;
begin
  Result := FThemeLibraryMethodPointersKeeper.FIsThemeBackgroundPartiallyTransparent(hTheme,
    iPartId, iStateId);
end;

function DrawThemeParentBackground(hWnd: HWND; DC: HDC; const prc: TRect): HRESULT;
var
  AFontHandle: HFONT;
begin
  AFontHandle := GetCurrentObject(DC, OBJ_FONT);
  Result := FThemeLibraryMethodPointersKeeper.FDrawThemeParentBackground(hWnd, DC, prc);
  SelectObject(DC, AFontHandle);
end;

function GetThemeColor(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out Color: TColorRef): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeColor(hTheme, iPartId, iStateId, iPropId, Color);
end;

function GetThemeMetric(hTheme: TdxTheme; DC: HDC; iPartId, iStateId, iPropId: Integer;
  out iVal: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeMetric(hTheme, DC, iPartId,
    iStateId, iPropId, iVal);
end;

function GetThemeString(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  pszBuff: PWideChar; cchMaxBuffChars: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeString(hTheme, iPartId, iStateId,
    iPropId, pszBuff, cchMaxBuffChars);
end;

function GetThemeBool(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out fVal: BOOL): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeBool(hTheme, iPartId, iStateId,
    iPropId, fVal);
end;

function GetThemeInt(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out iVal: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeInt(hTheme, iPartId, iStateId,
    iPropId, iVal);
end;

function GetThemeEnumValue(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out iVal: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeEnumValue(hTheme, iPartId,
    iStateId, iPropId, iVal);
end;

function GetThemePosition(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out Point: TPoint): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemePosition(hTheme, iPartId, iStateId,
    iPropId, Point);
end;

function GetThemeFont(hTheme: TdxTheme; DC: HDC; iPartId, iStateId, iPropId: Integer;
  out Font: LOGFONTW): HRESULT;
var
  S: array [0 .. 1000] of Char;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeFont(hTheme, DC, iPartId, iStateId,
    iPropId, Font);
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, Result, LOCALE_USER_DEFAULT, S, 1001, nil);
end;

function GetThemeRect(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out Rect: TRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeRect(hTheme, iPartId, iStateId,
    iPropId, Rect);
end;

function GetThemeMargins(hTheme: TdxTheme; DC: HDC; iPartId, iStateId, iPropId: Integer;
  prc: PRect; out Margins: TdxMargins): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeMargins(hTheme, DC, iPartId,
    iStateId, iPropId, prc, Margins);
end;

function GetThemeIntList(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out IntList: TdxIntList): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeIntList(hTheme, iPartId, iStateId,
    iPropId, IntList);
end;

function GetThemePropertyOrigin(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  out Origin: TdxPropertyOrigin): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemePropertyOrigin(hTheme, iPartId,
    iStateId, iPropId, Origin);
end;

function SetWindowTheme(hWnd: HWND; pszSubAppName, pszSubIdList:
  PWideChar): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FSetWindowTheme(hWnd, pszSubAppName, pszSubIdList);
end;

function GetThemeFilename(hTheme: TdxTheme; iPartId, iStateId, iPropId: Integer;
  pszThemeFileName: PWideChar; cchMaxBuffChars: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeFilename(hTheme, iPartId,
    iStateId, iPropId, pszThemeFileName, cchMaxBuffChars);
end;

function GetThemeSysColor(hTheme: TdxTheme; iColorId: Integer): COLORREF;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysColor(hTheme, iColorId);
end;

function GetThemeSysColorBrush(hTheme: TdxTheme; iColorId: Integer): HBRUSH;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysColorBrush(hTheme, iColorId);
end;

function GetThemeSysBool(hTheme: TdxTheme; iBoolId: Integer): BOOL;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysBool(hTheme, iBoolId);
end;

function GetThemeSysSize(hTheme: TdxTheme; iSizeId: Integer): Integer;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysSize(hTheme, iSizeId);
end;

function GetThemeSysFont(hTheme: TdxTheme; iFontId: Integer;
  out lf: LOGFONTW): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysFont(hTheme, iFontId, lf);
end;

function GetThemeSysString(hTheme: TdxTheme; iStringId: Integer; pszStringBuff:
  PWideChar; cchMaxStringChars: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysString(hTheme, iStringId,
    pszStringBuff, cchMaxStringChars);
end;

function GetThemeSysInt(hTheme: TdxTheme; iIntId: Integer; var iValue: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysInt(hTheme, iIntId, iValue);
end;

function IsThemeActive: BOOL;
begin
  if @FThemeLibraryMethodPointersKeeper.FIsThemeActive = nil then
    Result := False
  else
    Result := FThemeLibraryMethodPointersKeeper.FIsThemeActive;
end;

function IsAppThemed: BOOL;
begin
  Result := FThemeLibraryMethodPointersKeeper.FIsAppThemed;
end;

function GetWindowTheme(hWnd: HWND): TdxTheme;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetWindowTheme(hWnd);
end;

function EnableThemeDialogTexture(hWnd: HWND; dwFlags: DWORD): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FEnableThemeDialogTexture(hWnd, dwFlags);
end;

function IsThemeDialogTextureEnabled(hWnd: HWND): BOOL;
begin
  Result := FThemeLibraryMethodPointersKeeper.FIsThemeDialogTextureEnabled(hWnd);
end;

function GetThemeAppProperties: DWORD;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeAppProperties;
end;

procedure SetThemeAppProperties(dwFlags: DWORD);
begin
  FThemeLibraryMethodPointersKeeper.FSetThemeAppProperties(dwFlags);
end;

function GetCurrentThemeName(pszThemeFileName: PWideChar;
  cchMaxNameChars: Integer; pszColorBuff: PWideChar;
  cchMaxColorChars: Integer; pszSizeBuff: PWideChar;
  cchMaxSizeChars: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetCurrentThemeName(pszThemeFileName,
    cchMaxNameChars, pszColorBuff, cchMaxColorChars, pszSizeBuff, cchMaxSizeChars);
end;

function GetThemeDocumentationProperty(
  pszThemeName, pszPropertyName, pszValueBuff: PWideChar;
  cchMaxValChars: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeDocumentationProperty(pszThemeName,
    pszPropertyName, pszValueBuff, cchMaxValChars);
end;

//vista extension

function BufferedPaintInit: HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FBufferedPaintInit;
end;

function BufferedPaintUnInit: HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FBufferedPaintUnInit;
end;

function BeginBufferedPaint(hdcTarget: HDC; prcTarget: PRect;
  dwFormat: DWORD; pPaintParams: PdxBPPaintParams; out dc: HDC): TdxPaintBuffer;
begin
  Result := FThemeLibraryMethodPointersKeeper.FBeginBufferedPaint(hdcTarget, prcTarget, dwFormat, pPaintParams, dc);
end;

function EndBufferedPaint(hBufferedPaint: TdxPaintBuffer; fUpdateTarget: BOOL): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FEndBufferedPaint(hBufferedPaint, fUpdateTarget);
end;

function BufferedPaintClear(hBufferedPaint: TdxPaintBuffer; const R: TRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FBufferedPaintClear(hBufferedPaint, R);
end;

function BufferedPaintSetAlpha(hBufferedPaint: TdxPaintBuffer; prc: PRect; alpha: Byte): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FBufferedPaintSetAlpha(hBufferedPaint, prc, alpha);
end;

function IsThemeLibraryLoaded: Boolean;
begin
  Result := FThemeLibrary <> 0;
end;

function dxBeginPanningFeedback(AWindow: THandle): Boolean;
begin
  if Assigned(FBeginPanningFeedback) then
    Result := FBeginPanningFeedback(AWindow)
  else
    Result := False;
end;

function dxUpdatePanningFeedback(AWindow: THandle; AOverpan: TPoint; AIsInertia: Boolean): Boolean;
begin
  if Assigned(FUpdatePanningFeedback) then
    Result := FUpdatePanningFeedback(AWindow, AOverpan.X, AOverpan.Y, AIsInertia)
  else
    Result := False;
end;

function dxEndPanningFeedback(AWindow: THandle; AAnimateBack: Boolean): Boolean;
begin
  if Assigned(FEndPanningFeedback) then
    Result := FEndPanningFeedback(AWindow, AAnimateBack)
  else
    Result := False;
end;

procedure SetMethodPointers;
begin
  with FThemeLibraryMethodPointersKeeper do
  begin
    @FCloseThemeData := GetProcAddress(FThemeLibrary, 'CloseThemeData');
    @FDrawThemeBackground := GetProcAddress(FThemeLibrary, 'DrawThemeBackground');
    @FDrawThemeEdge := GetProcAddress(FThemeLibrary, 'DrawThemeEdge');
    @FDrawThemeIcon := GetProcAddress(FThemeLibrary, 'DrawThemeIcon');
    @FDrawThemeParentBackground := GetProcAddress(FThemeLibrary, 'DrawThemeParentBackground');
    @FDrawThemeText := GetProcAddress(FThemeLibrary, 'DrawThemeText');
    @FDrawThemeTextEx := GetProcAddress(FThemeLibrary, 'DrawThemeTextEx');
    @FEnableThemeDialogTexture := GetProcAddress(FThemeLibrary, 'EnableThemeDialogTexture');
    @FGetCurrentThemeName := GetProcAddress(FThemeLibrary, 'GetCurrentThemeName');
    @FGetThemeAppProperties := GetProcAddress(FThemeLibrary, 'GetThemeAppProperties');
    @FGetThemeBackgroundContentRect := GetProcAddress(FThemeLibrary, 'GetThemeBackgroundContentRect');
    @FGetThemeBackgroundRegion := GetProcAddress(FThemeLibrary, 'GetThemeBackgroundRegion');
    @FGetThemeBackgroundExtent := GetProcAddress(FThemeLibrary, 'GetThemeBackgroundExtent');
    @FGetThemeBool := GetProcAddress(FThemeLibrary, 'GetThemeBool');
    @FGetThemeColor := GetProcAddress(FThemeLibrary, 'GetThemeColor');
    @FGetThemeDocumentationProperty := GetProcAddress(FThemeLibrary, 'GetThemeDocumentationProperty');
    @FGetThemeEnumValue := GetProcAddress(FThemeLibrary, 'GetThemeEnumValue');
    @FGetThemeFilename := GetProcAddress(FThemeLibrary, 'GetThemeFilename');
    @FGetThemeFont := GetProcAddress(FThemeLibrary, 'GetThemeFont');
    @FGetThemeInt := GetProcAddress(FThemeLibrary, 'GetThemeInt');
    @FGetThemeIntList := GetProcAddress(FThemeLibrary, 'GetThemeIntList');
    @FGetThemeMargins := GetProcAddress(FThemeLibrary, 'GetThemeMargins');
    @FGetThemeMetric := GetProcAddress(FThemeLibrary, 'GetThemeMetric');
    @FGetThemePartSize := GetProcAddress(FThemeLibrary, 'GetThemePartSize');
    @FGetThemePosition := GetProcAddress(FThemeLibrary, 'GetThemePosition');
    @FGetThemePropertyOrigin := GetProcAddress(FThemeLibrary, 'GetThemePropertyOrigin');
    @FGetThemeRect := GetProcAddress(FThemeLibrary, 'GetThemeRect');
    @FGetThemeString := GetProcAddress(FThemeLibrary, 'GetThemeString');
    @FGetThemeSysBool := GetProcAddress(FThemeLibrary, 'GetThemeSysBool');
    @FGetThemeSysColor := GetProcAddress(FThemeLibrary, 'GetThemeSysColor');
    @FGetThemeSysColorBrush := GetProcAddress(FThemeLibrary, 'GetThemeSysColorBrush');
    @FGetThemeSysFont := GetProcAddress(FThemeLibrary, 'GetThemeSysFont');
    @FGetThemeSysInt := GetProcAddress(FThemeLibrary, 'GetThemeSysInt');
    @FGetThemeSysSize := GetProcAddress(FThemeLibrary, 'GetThemeSysSize');
    @FGetThemeSysString := GetProcAddress(FThemeLibrary, 'GetThemeSysString');
    @FGetThemeTextExtent := GetProcAddress(FThemeLibrary, 'GetThemeTextExtent');
    @FGetThemeTextMetrics := GetProcAddress(FThemeLibrary, 'GetThemeTextMetrics');
    @FGetWindowTheme := GetProcAddress(FThemeLibrary, 'GetWindowTheme');
    @FHitTestThemeBackground := GetProcAddress(FThemeLibrary, 'HitTestThemeBackground');
    @FIsAppThemed := GetProcAddress(FThemeLibrary, 'IsAppThemed');
    @FIsThemeActive := GetProcAddress(FThemeLibrary, 'IsThemeActive');
    @FIsThemePartDefined := GetProcAddress(FThemeLibrary, 'IsThemePartDefined');
    @FIsThemeBackgroundPartiallyTransparent := GetProcAddress(FThemeLibrary, 'IsThemeBackgroundPartiallyTransparent');
    @FIsThemeDialogTextureEnabled := GetProcAddress(FThemeLibrary, 'IsThemeDialogTextureEnabled');
    @FOpenThemeData := GetProcAddress(FThemeLibrary, 'OpenThemeData');
    @FSetThemeAppProperties := GetProcAddress(FThemeLibrary, 'SetThemeAppProperties');
    @FSetWindowTheme := GetProcAddress(FThemeLibrary, 'SetWindowTheme');
    //vista extension
    @FBeginBufferedPaint := GetProcAddress(FThemeLibrary, 'BeginBufferedPaint');
    @FBufferedPaintClear := GetProcAddress(FThemeLibrary, 'BufferedPaintClear');
    @FBufferedPaintInit := GetProcAddress(FThemeLibrary, 'BufferedPaintInit');
    @FBufferedPaintSetAlpha := GetProcAddress(FThemeLibrary, 'BufferedPaintSetAlpha');
    @FBufferedPaintUnInit := GetProcAddress(FThemeLibrary, 'BufferedPaintUnInit');
    @FEndBufferedPaint := GetProcAddress(FThemeLibrary, 'EndBufferedPaint');
  end;
  @FBeginPanningFeedback := GetProcAddress(FThemeLibrary, 'BeginPanningFeedback');
  @FUpdatePanningFeedback := GetProcAddress(FThemeLibrary, 'UpdatePanningFeedback');
  @FEndPanningFeedback := GetProcAddress(FThemeLibrary, 'EndPanningFeedback');
end;

initialization
  InitCommonControls;
  FThemeLibrary := LoadLibrary(ThemeLibraryName);
  if FThemeLibrary <> 0 then
    SetMethodPointers;

finalization
  if FThemeLibrary <> 0 then
    FreeLibrary(FThemeLibrary);

end.
