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

unit cxDrawTextUtils;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Controls, Types, Classes, SysUtils, Graphics;

const
  CXTO_LEFT                    = $00000000;
  CXTO_CENTER_HORIZONTALLY     = $00000001;
  CXTO_RIGHT                   = $00000002;
  CXTO_JUSTIFY_HORIZONTALLY    = $00000003;
  CXTO_DISTRIBUTE_HORIZONTALLY = $00000004;

  CXTO_TOP                     = $00000000;
  CXTO_CENTER_VERTICALLY       = $00000010;
  CXTO_BOTTOM                  = $00000020;
  CXTO_DISTRIBUTE_VERTICALLY   = $00000030;

  CXTO_WORDBREAK               = $00000100;
  CXTO_SINGLELINE              = $00000200;
  CXTO_EXPANDTABS              = $00000400;
  CXTO_END_ELLIPSIS            = $00000800;

  CXTO_PATTERNEDTEXT           = $00001000;
  CXTO_EXTERNALLEADING         = $00002000;
  CXTO_EDITCONTROL             = $00004000;
  CXTO_NOCLIP                  = $00008000;

  CXTO_AUTOINDENTS             = $00010000;
  CXTO_CHARBREAK               = $00020000;
  CXTO_PREVENT_LEFT_EXCEED     = $00040000;
  CXTO_PREVENT_TOP_EXCEED      = $00080000;

  CXTO_CALCRECT                = $00100000;
  CXTO_CALCROWCOUNT            = $00200000;

  CXTO_NOPREFIX                = $00000000;
  CXTO_HIDEPREFIX              = $01000000;
  CXTO_EXCELSTYLE              = $02000000;

  CXTO_RTLREADING              = $04000000;

  CXTO_DEFAULT_FORMAT          = CXTO_LEFT or CXTO_TOP or CXTO_SINGLELINE;

  cxTextSpace                  = 2;
  TcxCacheStaticRowsCount      = 32;
  cxMinVisuallyVisibleTextHeight = 6;

type
  TCaptionChar = Char;
  TcxCaptionChar = TCaptionChar;
  PcxCaptionChar = PChar;
  TcxTextOutFormat = DWORD;
  TcxTextAlignX = (taLeft, taCenterX, taRight, taJustifyX, taDistributeX);
  TcxTextAlignY = (taTop, taCenterY, taBottom, taDistributeY);
  TcxVerticalTextOutDirection = (vtdTopToBottom, vtdBottomToTop);

  PcxTextParams = ^TcxTextParams;
  TcxTextParams = packed record
    RowHeight: Integer;
    tmExternalLeading: Integer;
    FullRowHeight: Integer;
    EndEllipsisWidth: Integer;
    Bold: Boolean;
    BreakChar: WideChar;
    TextAlignX: TcxTextAlignX;
    TextAlignY: TcxTextAlignY;
    WordBreak: Boolean;
    SingleLine: Boolean;
    ExpandTabs: Boolean;
    EndEllipsis: Boolean;
    ExternalLeading: Boolean;
    EditControl: Boolean;
    ExcelStyle: Boolean;
    NoClip: Boolean;
    AutoIndents: Boolean;
    PatternedText: Boolean;
    PreventLeftExceed: Boolean;
    PreventTopExceed: Boolean;
    CharBreak: Boolean;
    CalcRowCount: Boolean;
    CalcRect: Boolean;
    MaxCharWidth: Integer;
    CharSet: Byte;
    OnePixel: Integer;
    HidePrefix: Boolean;
    RTLReading: Boolean;
  end;

  TcxTextRow = record
    Text: PChar;
    TextLength: Integer;
    TextExtents: TSize;
    BreakCount: Integer;
    TextOriginX: Integer;
    TextOriginY: Integer;
    StartOffset: Integer;
  end;
  PcxTextRow = ^TcxTextRow;

  TCanvasHandle = HDC;

  TcxDynamicTextRows = array of TcxTextRow;

  TcxCalcTextExtentsProc = function (AHandle: TCanvasHandle; AText: PChar;
    ATextLength: Integer; AExpandTabs: Boolean; AData: Pointer): TSize;

  TcxTextRows = record
    Count: Integer;
    Text: string;
    StaticRows: array[0..TcxCacheStaticRowsCount - 1] of TcxTextRow;
    DynamicRows: TcxDynamicTextRows;
  end;

function cxCalcTextParams(AHandle: TCanvasHandle; AFormat: DWORD;
  const ALineSpacingFactor: Double = 1.0): TcxTextParams; overload;

function cxCalcTextParams(ACanvas: TCanvas; AFormat: DWORD;
  const ALineSpacingFactor: Double = 1.0): TcxTextParams; overload;

function cxCalcTextExtents(AHandle: TCanvasHandle; AText: PChar;
  ATextLength: Integer; AExpandTabs: Boolean): TSize; overload;

function cxCalcTextExtentsEx(AHandle: TCanvasHandle; AText: PChar;
  ATextLength: Integer; AExpandTabs: Boolean; AData: Pointer): TSize; overload;

procedure cxCalcTextRowExtents(AHandle: TCanvasHandle; ATextRow: PcxTextRow;
  const ATextParams: TcxTextParams); overload; inline;

procedure cxCalcTextRowExtents(AHandle: TCanvasHandle; ATextRow: PcxTextRow;
  const ATextParams: TcxTextParams; AUserData: Pointer;
  ACalcTextExtentsProc: TcxCalcTextExtentsProc); overload; inline;

function cxGetLongestTextRowWidth(const ATextRows: TcxTextRows; ARowCount: Integer): Integer;

procedure cxPlaceTextRows(AHandle: TCanvasHandle; const R: TRect;
  var ATextParams: TcxTextParams; const ATextRows: TcxTextRows; ARowCount: Integer);

procedure cxResetTextRows(var ATextRows: TcxTextRows); inline;

function cxGetTextRow(const ATextRows: TcxTextRows; AIndex: Integer): PcxTextRow; inline;

function cxGetTextRowCount(const ATextRows: TcxTextRows): Integer; inline;

procedure cxPrepareEndEllipsis(AHandle: TCanvasHandle; ATextRow: PcxTextRow;
  const ATextParams: TcxTextParams; ACalcTextExtentsProc: TcxCalcTextExtentsProc;
  var AWidth: Integer; AUserData: Pointer = nil); overload;

procedure cxPrepareEndEllipsis(AHandle: TCanvasHandle; ATextRow: PcxTextRow;
  const ATextParams: TcxTextParams; ACalcTextExtentsProc: TcxCalcTextExtentsProc;
  const ABounds: TRect; var AWidth: Integer; AUserData: Pointer = nil); overload;

function cxMakeFormat(ATextAlignX: TcxTextAlignX; ATextAlignY: TcxTextAlignY): DWORD;

function cxPrepareRect(const R: TRect; const ATextParams: TcxTextParams;
  ALeftIndent, ARightIndent: Integer): TRect;

function cxGetIsWordDelimeter(ACodePage: DWORD; AChar: WideChar): Boolean;
function cxGetNextWordBreak(ACodePage: DWORD; AStart, AEnd: PChar): PChar;

procedure cxTextRowsOutHighlight(AHandle: TCanvasHandle; const ABounds: TRect;
  const ATextParams: TcxTextParams; const ATextRows: TcxTextRows; ARowCount,
  ASelStart, ASelLength: Integer; ASelBkgColor, ASelTextColor: TColor;
  AForceEndEllipsis: Boolean);

function cxMakeTextRows(AHandle: TCanvasHandle;
  AText: PChar; ATextLength: Integer;
  const R: TRect; const ATextParams: TcxTextParams; var ATextRows: TcxTextRows;
  out ACount: Integer; AMaxLineCount: Integer; AUserData: Pointer;
  ACalcTextExtentsProc: TcxCalcTextExtentsProc): Boolean; overload;

function cxMakeTextRows(AHandle: TCanvasHandle;
  AText: PChar; ATextLength: Integer;
  const R: TRect; const ATextParams: TcxTextParams; var ATextRows: TcxTextRows;
  out ACount: Integer; AMaxLineCount: Integer = 0): Boolean; overload; inline;

function cxMakeTextRows(ACanvas: TCanvas;
  AText: PChar; ATextLength: Integer;
  const R: TRect; const ATextParams: TcxTextParams; var ATextRows: TcxTextRows;
  out ACount: Integer; AMaxLineCount: Integer = 0): Boolean; overload; inline;

function cxTextOut(AHandle: TCanvasHandle; const AText: string; var R: TRect;
  AFormat: TcxTextOutFormat; ASelStart, ASelLength: Integer; AFont: TFont;
  ASelBkgColor, ASelTextColor: TColor; AMaxLineCount: Integer = 0;
  ALeftIndent: Integer = 0; ARightIndent: Integer = 0;
  ATextColor: TColor = clDefault; const ALineSpacingFactor: Double = 1.0): Integer; overload;

function cxTextOut(AHandle: TCanvasHandle; const AText: string; var R: TRect;
  AFormat: TcxTextOutFormat = CXTO_DEFAULT_FORMAT; AFont: TFont = nil;
  AMaxLineCount: Integer = 0; ALeftIndent: Integer = 0; ARightIndent: Integer = 0;
  ATextColor: TColor = clDefault; const ALineSpacingFactor: Double = 1.0): Integer; overload; inline;

function cxTextOut(ACanvas: TCanvas; const AText: string; var R: TRect;
  AFormat: TcxTextOutFormat; ASelStart, ASelLength: Integer; AFont: TFont;
  ASelBkgColor, ASelTextColor: TColor; AMaxLineCount: Integer = 0; ALeftIndent: Integer = 0;
  ARightIndent: Integer = 0; ATextColor: TColor = clDefault;
  const ALineSpacingFactor: Double = 1.0): Integer; overload; inline;

function cxTextOut(ACanvas: TCanvas; const AText: string; var R: TRect;
  AFormat: TcxTextOutFormat = CXTO_DEFAULT_FORMAT; AFont: TFont = nil;
  AMaxLineCount: Integer = 0; ALeftIndent: Integer = 0; ARightIndent: Integer = 0;
  ATextColor: TColor = clDefault; const ALineSpacingFactor: Double = 1.0): Integer; overload; inline;

procedure cxRotatedTextOut(AHandle: TCanvasHandle; const ABounds: TRect; const AText: string; AFont: TFont;
  AAlignHorz: TcxTextAlignX = taCenterX; AAlignVert: TcxTextAlignY = taCenterY; AWordBreak: Boolean = True;
  ALeftExceed: Boolean = True; ARightExceed: Boolean = True; ADirection: TcxVerticalTextOutDirection = vtdBottomToTop;
  AFontSize: Integer = 0); overload;

implementation

uses
  Math, dxTypeHelpers, dxCoreGraphics, dxCore;

type
  TCanvasAccess = class(TControlCanvas);

  TFullWidthUnicode = record
    Start, Finish: WideChar;
  end;

const
  CXTO_VERT_ALIGN_OFFSET = 4;
  CXTO_HORZ_ALIGN_MASK   = CXTO_CENTER_HORIZONTALLY or CXTO_RIGHT or CXTO_JUSTIFY_HORIZONTALLY or CXTO_DISTRIBUTE_HORIZONTALLY;
  CXTO_VERT_ALIGN_MASK   = CXTO_CENTER_VERTICALLY or CXTO_BOTTOM or CXTO_DISTRIBUTE_VERTICALLY;
  MaxSymbolsOnTabbedLine = 4096;

  Tab = #9;
  LF = #10;
  CR = #13;
  Space = #32;

  cxEndEllipsisChars: PChar = '...';
  cxEndEllipsisCharsLength = 3;

type
  TcxTextRowFormat = record
    Align: TcxTextAlignX;
    BreakByWords: Boolean;
    BreakByChars: Boolean;
    CalcRect: Boolean;
    EditControl: Boolean;
    EndEllipsis: Boolean;
    ExpandTabs: Boolean;
    ExcelStyle: Boolean;
    SingleLine: Boolean;
    Special: Boolean;
  end;

var
  FillPatterns: array[Boolean] of HBRUSH;

//work with cache
procedure cxResetTextRows(var ATextRows: TcxTextRows);
begin
  ATextRows.Count := 0;
  SetLength(ATextRows.DynamicRows, 0);
  ATextRows.DynamicRows := nil;
  //don't clear ATextRows.Text here
end;

procedure ValidateTextRows(var ATextRows: TcxTextRows; ACount: Integer);
var
  AIndex: Integer;
begin
  AIndex := ACount - 1;
  ATextRows.Count := ACount;
  Dec(ACount, TcxCacheStaticRowsCount);
  if ACount > Length(ATextRows.DynamicRows) then
    SetLength(ATextRows.DynamicRows, ACount + 4);
  if AIndex >= 0 then
    FillChar(cxGetTextRow(ATextRows, AIndex)^, SizeOf(TcxTextRow), 0);
end;

function cxGetTextRow(const ATextRows: TcxTextRows; AIndex: Integer): PcxTextRow;
begin
  if AIndex < TcxCacheStaticRowsCount then
    Result := @ATextRows.StaticRows[AIndex]
  else
  begin
    Dec(AIndex, TcxCacheStaticRowsCount);
    Result := @ATextRows.DynamicRows[AIndex];
  end;
end;

function cxGetTextRowCount(const ATextRows: TcxTextRows): Integer;
begin
  Result := ATextRows.Count;
end;

function cxMakeFormat(ATextAlignX: TcxTextAlignX; ATextAlignY: TcxTextAlignY): DWORD;
begin
  Result := Byte(ATextAlignX) or (Byte(ATextAlignY) shl CXTO_VERT_ALIGN_OFFSET);
end;

function cxGetTextExtentExPoint(AHandle: TCanvasHandle; AText: PChar;
  ATextLength, AWidth: Integer; ACharNumber, ADX: PInteger): Boolean;
var
  ASize: TSize;
begin
  Result := GetTextExtentExPointW(AHandle, AText, ATextLength, AWidth, ACharNumber, ADX, ASize)
end;

function cxGetTabbedTextExtentW(hDC: HDC; lpString: PChar;
  nCount, nTabPositions: Integer; var lpnTabStopPositions): TSIZE;
var
  R: TRect;
  ACharSize: TSize;
  ATextExtent: DWORD;
begin
  ATextExtent := GetTabbedTextExtentW(hDC, lpString, nCount, nTabPositions, lpnTabStopPositions);
  Result.cy := HiWord(ATextExtent);
  Result.cx := LoWord(ATextExtent);
  // todo: B150889 if Result.cx > MaxWord result is incorrect!
  GetTextExtentPoint32W(hDC, 'W', 1, ACharSize);
  if ACharSize.cx * nCount > MaxWord then
  begin
     R.Empty;
     DrawTextW(hDC, lpString, nCount, R, DT_SINGLELINE or DT_NOPREFIX or DT_CALCRECT or DT_EXPANDTABS);
     if (R.Right - R.Left) > MaxWord then
       Inc(Result.cx, (R.Right - R.Left) div MaxWord * MaxWord);
  end;
end;

function cxTabbedTextOutW(hDC: HDC; X, Y: Integer; lpString: PChar;
  nCount, nTabPositions: Integer; var lpnTabStopPositions; nTabOrigin: Integer): Longint;
begin
  if nCount > MaxSymbolsOnTabbedLine then
    nCount := MaxSymbolsOnTabbedLine;
  Result := TabbedTextOutW(hDC, X, Y, lpString, nCount, nTabPositions, lpnTabStopPositions, nTabOrigin);
end;

function cxCalcTextExtents(AHandle: TCanvasHandle; AText: PChar;
  ATextLength: Integer; AExpandTabs: Boolean): TSize;
var
  R: TRect;
begin
  if AExpandTabs then
  begin
    if ATextLength < MaxSymbolsOnTabbedLine then
      Result := cxGetTabbedTextExtentW(AHandle, AText, ATextLength, 0, Result)
    else
    begin
      R.Empty;
      Result.cy := DrawTextW(AHandle, AText, ATextLength, R,
        DT_SINGLELINE or DT_NOPREFIX or DT_CALCRECT or DT_EXPANDTABS);
      Result.cx := R.Right - R.Left;
      if (ATextLength > MaxSymbolsOnTabbedLine) and (Result.cx = 0) then
      begin
        R.Empty;
        Result.cy := DrawTextW(AHandle, AText, MaxSymbolsOnTabbedLine, R,
          DT_SINGLELINE or DT_NOPREFIX or DT_CALCRECT or DT_EXPANDTABS);
        Result.cx := R.Right - R.Left;
      end;
    end;
  end
  else
    GetTextExtentPoint32W(AHandle, AText, ATextLength, Result);
end;

function IntersectClipRect(AHandle: TCanvasHandle; const R: TRect): HRGN;
begin
  Result := CreateRectRgn(0, 0, 0, 0);
  if GetClipRgn(AHandle, Result) <> 1 then
  begin
    DeleteObject(Result);
    Result := 0;
  end;
  with R do
    Windows.IntersectClipRect(AHandle, Left, Top, Right, Bottom);
end;

procedure RestoreClipRgn(AHandle: TCanvasHandle; var ARgn: HRGN);
begin
  SelectClipRgn(AHandle, ARgn);
  if ARgn <> 0 then
  begin
    DeleteObject(ARgn);
    ARgn := 0
  end;
end;

function cxCalcTextExtentsEx(AHandle: TCanvasHandle; AText: PChar;
  ATextLength: Integer; AExpandTabs: Boolean; AData: Pointer): TSize;
begin
  Result := cxCalcTextExtents(AHandle, AText, ATextLength, AExpandTabs);
end;

procedure cxCalcTextRowExtents(AHandle: TCanvasHandle; ATextRow: PcxTextRow;
  const ATextParams: TcxTextParams);
begin
  cxCalcTextRowExtents(AHandle, ATextRow, ATextParams, nil, @cxCalcTextExtentsEx);
end;

procedure cxCalcTextRowExtents(AHandle: TCanvasHandle; ATextRow: PcxTextRow;
  const ATextParams: TcxTextParams; AUserData: Pointer;
  ACalcTextExtentsProc: TcxCalcTextExtentsProc); overload; inline;
begin
  with ATextRow^ do
    TextExtents := ACalcTextExtentsProc(AHandle, Text, TextLength,
      ATextParams.ExpandTabs, AUserData);
end;

function cxCalcTextParams(AHandle: TCanvasHandle; AFormat: TcxTextOutFormat; const ALineSpacingFactor: Double = 1.0): TcxTextParams;
var
  ATextMetricW: TTextMetricW;
  ATextMetricA: TTextMetricA;
  R: TRect;
begin
  FillChar(Result, SizeOf(Result), 0);
  with Result do
  begin
    if GetTextMetricsW(AHandle, ATextMetricW) then
    begin
      Bold := ATextMetricW.tmWeight >= FW_BOLD;
      BreakChar := ATextMetricW.tmBreakChar;
      MaxCharWidth := ATextMetricW.tmMaxCharWidth;
      RowHeight := ATextMetricW.tmHeight;
      CharSet := ATextMetricW.tmCharSet;
      if ExternalLeading then
        tmExternalLeading := ATextMetricW.tmExternalLeading;
    end
    else
    begin
      GetTextMetricsA(AHandle, ATextMetricA);
      Bold := ATextMetricA.tmWeight >= FW_BOLD;
      BreakChar := WideChar(ATextMetricA.tmBreakChar);
      MaxCharWidth := ATextMetricA.tmMaxCharWidth;
      RowHeight := ATextMetricA.tmHeight;
      CharSet := ATextMetricA.tmCharSet;
      if ExternalLeading then
        tmExternalLeading := ATextMetricA.tmExternalLeading;
    end;
    TextAlignX := TcxTextAlignX(AFormat and CXTO_HORZ_ALIGN_MASK);
    TextAlignY := TcxTextAlignY(AFormat and CXTO_VERT_ALIGN_MASK shr CXTO_VERT_ALIGN_OFFSET);
    AutoIndents := AFormat and CXTO_AUTOINDENTS <> 0;
    CalcRect := AFormat and CXTO_CALCRECT <> 0;
    CalcRowCount := AFormat and CXTO_CALCROWCOUNT <> 0;
    CharBreak := (AFormat and CXTO_CHARBREAK <> 0) or SysLocale.FarEast; //SysLocale.FarEast ??
    EditControl := AFormat and CXTO_EDITCONTROL <> 0;
    EndEllipsis := AFormat and CXTO_END_ELLIPSIS <> 0;
    ExcelStyle := AFormat and CXTO_EXCELSTYLE <> 0;
    ExpandTabs := AFormat and CXTO_EXPANDTABS <> 0;
    ExternalLeading := AFormat and CXTO_EXTERNALLEADING <> 0;
    NoClip := AFormat and CXTO_NOCLIP <> 0;
    PatternedText := AFormat and CXTO_PATTERNEDTEXT <> 0;
    PreventLeftExceed := AFormat and CXTO_PREVENT_LEFT_EXCEED <> 0;
    PreventTopExceed := AFormat and CXTO_PREVENT_TOP_EXCEED <> 0;
    SingleLine := AFormat and CXTO_SINGLELINE <> 0;
    WordBreak := AFormat and CXTO_WORDBREAK <> 0;
    HidePrefix := AFormat and CXTO_HIDEPREFIX <> 0;
    RTLReading := AFormat and CXTO_RTLREADING <> 0;
    R := Rect(0, 0, 1, 1);
    DPtoLP(AHandle, R, 2);
    OnePixel := R.Right - R.Left;
    FullRowHeight := Round((RowHeight + tmExternalLeading) * ALineSpacingFactor);

    if PatternedText then
    begin
    {.$IFNDEF CXTEST}
      R := Rect(0, 0, cxMinVisuallyVisibleTextHeight, cxMinVisuallyVisibleTextHeight);
      DPtoLP(AHandle, R, 2);
      PatternedText := RowHeight < R.Bottom - R.Top;
    {.$ENDIF}
    end;

    if EndEllipsis then
      EndEllipsisWidth := cxCalcTextExtents(AHandle, cxEndEllipsisChars, cxEndEllipsisCharsLength, False).cX
    else
      EndEllipsisWidth := 0;
  end;
end;

function cxCalcTextParams(ACanvas: TCanvas; AFormat: DWORD;
  const ALineSpacingFactor: Double = 1.0): TcxTextParams;
begin
  TCanvasAccess(ACanvas).RequiredState([csHandleValid, csFontValid]);
  Result := cxCalcTextParams(ACanvas.Handle, AFormat, ALineSpacingFactor);
end;

function IsFarEastLineBreak(C: WideChar): Boolean;
const
  ASCIILatin1EndBreak: array[0..150] of Byte = (
         1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
      1, 0, 0, 0, 0, 0, 0, 1);
  GeneralPunctuationEndBreak: array[0..32] of Byte = (
               1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0,
      0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 1, 1);
  CJKSymbolEndBreak: array[0..29] of Byte = (
         1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1,
      0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1);
  CNS11643SmallVariantsEndBreak: array[0..46] of Byte = (
      1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
      1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1);
  FullWidthHalfWidthVariantsEndBreak: array[0..158] of Byte = (
         1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0,
      0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1);

begin
  case Ord(C) shr 8 of
    $0000:
      Result := (C >= #$0021) and (C <= #$00B7) and Boolean(ASCIILatin1EndBreak[Ord(C) - $0021]);
    $0002:
      Result := (C = #$02C7) or (C = #$02C9);
    $0020:
      Result := (C >= #$2013) and (C <= #$2033) and Boolean(GeneralPunctuationEndBreak[Ord(C) - $2013]);
    $0021:
      Result := C = #$2103;
    $0022:
      Result := C = #$2236;
    $0025:
      Result := C = #$2574;
    $0030:
      begin
        if (C >= #$3001) and (C <= #$301E) then
          Result := Boolean(CJKSymbolEndBreak[Ord(C) - $3001])
        else
          Result := (C = #$309B) or (C = #$309C);
      end;
    $00FE:
      Result := (C >= #$FE30) and (C <= #$FE5E) and Boolean(CNS11643SmallVariantsEndBreak[Ord(C) - $FE30]);
    $00FF:
      begin
        if (C >= #$FF01) and (C <= #$FF9F) then
          Result := Boolean(FullWidthHalfWidthVariantsEndBreak[Ord(C) - $FF01])
        else
          Result := C >= #$FFE0;
      end
    else
      Result := False;
  end;
end;

function IsFarEastFullWidthCharacter(ACodePage: DWORD; AChar: WideChar): Boolean;
const
  NUM_FULLWIDTH_UNICODES = 4;
  FullWidthUnicodes: array[0..NUM_FULLWIDTH_UNICODES - 1] of TFullWidthUnicode = (
   (Start: #$4E00; Finish: #$9FFF), // CJK_UNIFIED_IDOGRAPHS
   (Start: #$3040; Finish: #$309F), // HIRAGANA
   (Start: #$30A0; Finish: #$30FF), // KATAKANA
   (Start: #$AC00; Finish: #$D7A3)  // HANGUL
  );

var
  I: Integer;
begin
  if AChar < #$0080 then
  begin
    Result := False;
    Exit;
  end;
  for I := 0 to NUM_FULLWIDTH_UNICODES - 1 do
    if (AChar >= FullWidthUnicodes[I].Start) and (AChar <= FullWidthUnicodes[I].Finish) then
    begin
      Result := True;
      Exit;
    end;
  Result := WideCharToMultiByte(ACodePage, 0, @AChar, 1, nil, 0, nil, nil) > 1;
end;

function cxGetIsWordDelimeter(ACodePage: DWORD; AChar: WideChar): Boolean;
begin
  Result := (AChar = CR) or (AChar = LF) or (AChar = Tab) or (AChar = Space) or
    IsFarEastFullWidthCharacter(ACodePage, AChar) or IsFarEastLineBreak(AChar);
end;

function cxGetNextWordBreak(ACodePage: DWORD; AStart, AEnd: PChar): PChar;
var
  ANonWhite: Integer;
begin
  //todo: break by "-", see behavior of the Notepad in Windows 7
  ANonWhite := 1;
  while AStart < AEnd do
    case AStart^ of
      CR, LF:
        Break;
      Tab, Space:
        begin
          Result := AStart + ANonWhite;
          Exit;
        end;
      else
      begin
        if IsFarEastFullWidthCharacter(ACodePage, AStart^) then
        begin
          if ANonWhite = 0 then
            Result := AStart
          else
            if (AStart + 1 <> AEnd) and IsFarEastLineBreak((AStart + 1)^) then
              Result := AStart + 1 + 1
            else
              Result := AStart + 1;
          Exit;
        end;
        Inc(AStart);
        ANonWhite := 0;
      end;
    end;
  Result := AStart;
end;

function BreakAWord(AHandle: TCanvasHandle; AText: PChar;
  ATextLength, AWidth: Integer; AExpandTabs: Boolean): PChar;
var
  ALow, AHigh, ANew: Integer;
begin
  ALow := 0;
  AHigh := ATextLength;
  while (AHigh - ALow) > 1 do
  begin
    ANew := ALow + (AHigh - ALow) div 2;
    if cxCalcTextExtents(AHandle, AText, ANew, AExpandTabs).cx > AWidth then
      AHigh := ANew
    else
      ALow := ANew;
  end;
  if (ALow = 0) and (ATextLength > 0) then
    ALow := 1;
  Result := AText + ALow;
end;

function AdjustWhiteSpaces(ANextLine: PChar; var ACount: Integer; AAlign: TcxTextAlignX): PChar;
begin
  case AAlign of
    taLeft:
      if (ANextLine^ = Space) or (ANextLine^ = Tab) then
        Inc(ANextLine);
    taRight:
      if ((ANextLine - 1)^ = Space) or ((ANextLine - 1)^ = Tab) then
        Dec(ACount);
    taCenterX:
      begin
        if ((ANextLine - 1)^ = Space) or ((ANextLine - 1)^ = Tab) then
          Dec(ACount);
        if (ANextLine^ = Space) or (ANextLine^ = Tab) then
          Inc(ANextLine);
      end;
  end;
  Result := ANextLine;
end;

function FastFindLineEnd(AText, AEnd: PChar): PChar;
begin
  while AText < AEnd do
    if (AText^ = CR) or (AText^ = LF) then
      Break
    else
      Inc(AText);
  Result := AText;
end;

function GetLineBreak(AHandle: TCanvasHandle; ALineStart: PChar;
  ACount, AMaxWidth: Integer; const ATextRowFormat: TcxTextRowFormat;
  var ATextRow: PcxTextRow; ACalcTextExtentsProc: TcxCalcTextExtentsProc;
  AUserData: Pointer): PChar;
var
  AExtent, ANewExtent: Integer;
  AText, AEnd, P, ALineEnd: PChar;
  AAdjustWhiteSpaces: Boolean;
begin
  AExtent := 0;
  AAdjustWhiteSpaces := False;
  AText := ALineStart;
  AEnd := ALineStart + ACount;
  P := AText;
  ATextRow.Text := AText;
  ALineEnd := AEnd;
  while AText < AEnd do
  begin
    if {#AI: not ATextRowFormat.CalcRect and} ATextRowFormat.BreakByWords then
      P := cxGetNextWordBreak(CP_ACP, AText, AEnd)
    else
      P := FastFindLineEnd(AText, AEnd);
    ALineEnd := P;
    ANewExtent := ACalcTextExtentsProc(AHandle, ALineStart, P - ALineStart,
      ATextRowFormat.ExpandTabs, AUserData).cx;
    if ATextRowFormat.BreakByWords and (ANewExtent > AMaxWidth) then
    begin
      // Are there more than one word in this line and not a special case?
      if (AText <> ALineStart) and not ATextRowFormat.Special then
      begin
        ALineEnd := AText;
        P := AText;
        AAdjustWhiteSpaces := True;
      end
      else
      begin
        //One word is longer than the maximum width permissible.
        //See if we are allowed to break that single word.
        if ATextRowFormat.BreakByChars then
        begin
          P := BreakAWord(AHandle, AText, P - AText, AMaxWidth - AExtent, ATextRowFormat.ExpandTabs);
          ALineEnd := P;
          //Note: Since we broke in the middle of a word, no need to adjust for white spaces.
        end
        else
        begin
          AAdjustWhiteSpaces := True;
          // Check if we need to end this line with ellipsis
          if ATextRowFormat.EndEllipsis then
            // If there are CR/LF at the end, skip them.
            if P < AEnd then
            begin
              if P^ = CR then
              begin
                Inc(P);
                if (P < AEnd) and (P^ = LF) then
                  Inc(P);
                AAdjustWhiteSpaces := False;
              end;
              if P^ = LF then
              begin
                Inc(P);
                if (P < AEnd) and (P^ = CR) then
                  Inc(P);
                AAdjustWhiteSpaces := False;
              end;
            end;
        end;
      end;
      Break;
    end
    else
      // Don't do this if already at the end of the string.
      if P < AEnd then
      begin
        if P^ = CR then
        begin
          Inc(P);
          if (P < AEnd) and (P^ = LF) then
            Inc(P);
          AAdjustWhiteSpaces := False;
          Break;
        end;
        if P^ = LF then
        begin
          Inc(P);
          if (P < AEnd) and (P^ = CR) then
            Inc(P);
          AAdjustWhiteSpaces := False;
          Break;
        end;
      end;
    // Point at the beginning of the next word.
    AText := P;
    AExtent := ANewExtent;
  end;
  // Calculate the length of current line.
  ATextRow.TextLength := ALineEnd - ALineStart;
  // Adjust the line length and P to take care of spaces.
  if AAdjustWhiteSpaces and (P < AEnd) then
  begin
    P := AdjustWhiteSpaces(P, ATextRow.TextLength, ATextRowFormat.Align);
    if ATextRowFormat.ExcelStyle then
      while (P < AEnd) and (P^ = Space) do
        Inc(P);
  end;

  if (ATextRowFormat.Align = taDistributeX) or
    (AAdjustWhiteSpaces and (ATextRowFormat.Align = taJustifyX)) then
  begin
    AEnd := P - 1;
    while (ATextRow.TextLength > 0) and ((AEnd^ = Space) or (AEnd^ = Tab)) do
    begin
      Dec(AEnd);
      Dec(ATextRow.TextLength);
    end;

    AText := ATextRow.Text;
    while AText < AEnd do
    begin
      if (AText^ = Space) or (AText^ = Tab) then
        Inc(ATextRow.BreakCount);
      Inc(AText);
    end;
  end;
  // return the begining of next line;
  Result := P;
end;

procedure MakeTextRowFormat(const ATextParams: TcxTextParams; out ATextRowFormat: TcxTextRowFormat);
begin
  with ATextRowFormat do
  begin
    BreakByWords := not ATextParams.SingleLine and
     (ATextParams.WordBreak or (ATextParams.TextAlignX in [taJustifyX, taDistributeX]));
    CalcRect := ATextParams.CalcRect;
    EditControl := ATextParams.EditControl;
    EndEllipsis := ATextParams.EndEllipsis;
    ExcelStyle := ATextParams.ExcelStyle;
    ExpandTabs := ATextParams.ExpandTabs;
    Align := ATextParams.TextAlignX;
    SingleLine := ATextParams.SingleLine;
    BreakByChars := BreakByWords and (ATextParams.CharBreak or EditControl);
  end;
end;

function cxMakeTextRows(AHandle: TCanvasHandle; AText: PChar;
  ATextLength: Integer; const R: TRect; const ATextParams: TcxTextParams;
  var ATextRows: TcxTextRows; out ACount: Integer; AMaxLineCount: Integer = 0): Boolean; overload;
begin
  Result := cxMakeTextRows(AHandle, AText, ATextLength, R, ATextParams,
    ATextRows, ACount, AMaxLineCount, nil, @cxCalcTextExtentsEx);
end;

function cxMakeTextRows(AHandle: TCanvasHandle; AText: PChar; ATextLength: Integer;
  const R: TRect; const ATextParams: TcxTextParams; var ATextRows: TcxTextRows;
  out ACount: Integer; AMaxLineCount: Integer; AUserData: Pointer;
  ACalcTextExtentsProc: TcxCalcTextExtentsProc): Boolean; overload;

  function CheckIsLastRow(ATotalHeight, AHeight: Integer): Boolean;
  begin
    if ATextParams.SingleLine then
      Result := True
    else
      if (ATextParams.TextAlignY = taTop) and not (ATextParams.CalcRect or ATextParams.CalcRowCount) then
      begin
        if ATextParams.EditControl and not ATextParams.NoClip then
          Result := ATotalHeight + ATextParams.FullRowHeight > AHeight
        else
          Result := ATotalHeight > AHeight
      end
      else
        Result := (AMaxLineCount > 0) and (ACount = AMaxLineCount);
  end;

var
  P, ATextEnd, ANextLine: PChar;
  ATextRow: PcxTextRow;
  AIsLastRow, ARectIsSmall, APreSpecial, ACalculate: Boolean;
  ATotalHeight, H, W, AOffset: Integer;
  ATextRowFormat: TcxTextRowFormat;
begin
  cxResetTextRows(ATextRows);
  ARectIsSmall := False;
  ACount := 0;
  if ATextLength > 0 then
  begin
    if ATextParams.CalcRect and ATextParams.SingleLine then
    begin
      ACount := 1;
      ValidateTextRows(ATextRows, ACount);
      ATextRow := cxGetTextRow(ATextRows, 0);
      ATextRow.Text := AText;
      ATextRow.TextLength := ATextLength;
      cxCalcTextRowExtents(AHandle, ATextRow, ATextParams, AUserData, ACalcTextExtentsProc);
    end
    else
    begin
      P := AText;
      AOffset := 0;
      ATotalHeight := 0;
      W := R.Right - R.Left;
      ACalculate := ATextParams.CalcRect or ATextParams.CalcRowCount;
      H := R.Bottom - R.Top;
      ATextEnd := AText + ATextLength;
      AIsLastRow := False;
      MakeTextRowFormat(ATextParams, ATextRowFormat);
      APreSpecial := not ATextParams.NoClip and ATextParams.EndEllipsis and
        not ACalculate and not (ATextParams.TextAlignX in [taJustifyX, taDistributeX]);
      while (P < ATextEnd) and not AIsLastRow do
      begin
        Inc(ACount);
        ValidateTextRows(ATextRows, ACount);
        Inc(ATotalHeight, ATextParams.FullRowHeight);
        AIsLastRow := CheckIsLastRow(ATotalHeight, H);
        ATextRow := cxGetTextRow(ATextRows, ACount - 1);
        ATextRowFormat.Special := AIsLastRow and APreSpecial;
        ANextLine := GetLineBreak(AHandle, P, ATextLength, W, ATextRowFormat, ATextRow, ACalcTextExtentsProc, AUserData);
        Dec(ATextLength, ANextLine - P);
        P := ANextLine;
        ATextRow.StartOffset := AOffset;
        cxCalcTextRowExtents(AHandle, ATextRow, ATextParams, AUserData, ACalcTextExtentsProc);
        AOffset := P - AText;
        if not AIsLastRow then
          AIsLastRow := ATextLength = 0;
        if (AMaxLineCount > 0) and (ACount = AMaxLineCount) then
          Break;
      end;
      if not ACalculate and (ACount > 0) then
      begin
        if ATextRowFormat.SingleLine then
          ARectIsSmall := ATextRow.TextExtents.cx > W
        else
          ARectIsSmall := ATextLength > 0;
      end;
    end;
  end;
  Result := not ARectIsSmall;
end;

function cxMakeTextRows(ACanvas: TCanvas; AText: PChar; ATextLength: Integer; const R: TRect;
  const ATextParams: TcxTextParams; var ATextRows: TcxTextRows; out ACount: Integer; AMaxLineCount: Integer = 0): Boolean; overload;
begin
  Result := cxMakeTextRows(ACanvas.Handle, AText, ATextLength, R, ATextParams, ATextRows, ACount, AMaxLineCount);
end;

procedure cxPlaceTextRows(AHandle: TCanvasHandle; const R: TRect;
  var ATextParams: TcxTextParams; const ATextRows: TcxTextRows; ARowCount: Integer);

  procedure CalcExtraAndTopRowOffset(out AExtra, ATopRowOffset: Integer);
  var
    H: Integer;
  begin
    AExtra := 0;
    with ATextParams do
    begin
      if (ARowCount > 1) and (TextAlignY = taDistributeY) then
      begin
        H := R.Bottom - R.Top;
        Dec(H, RowHeight);
        if H / (ARowCount - 1) > RowHeight then
        begin
          FullRowHeight := H div (ARowCount - 1);
          AExtra := H mod (ARowCount - 1);
        end;
      end;

      case TextAlignY of
        taCenterY:
          ATopRowOffset := R.Top + (R.Bottom - R.Top - ARowCount * FullRowHeight) div 2;
        taBottom:
          ATopRowOffset := R.Bottom - ARowCount * FullRowHeight + tmExternalLeading;
      else
        ATopRowOffset := R.Top;
      end;

      if PreventTopExceed and (ATopRowOffset < R.Top) then
        ATopRowOffset := R.Top;
    end;
  end;

  procedure PlaceRows(AExtra, ATopRowOffset: Integer);
  var
    I: Integer;
    ARow: PcxTextRow;
  begin
    for I := 0 to ARowCount - 1 do
    begin
      ARow := cxGetTextRow(ATextRows, I);
      // Horizontally
      case ATextParams.TextAlignX of
        taCenterX:
          if ATextParams.SingleLine or not ATextParams.WordBreak then
            ARow.TextOriginX := R.Right - (ARow.TextExtents.cx + (R.Right - R.Left - ARow.TextExtents.cx) div 2)
          else
            ARow.TextOriginX := R.Left + (R.Right - R.Left - ARow.TextExtents.cx) div 2;
        taRight:
          ARow.TextOriginX := R.Right - ARow.TextExtents.cx;
      else
        ARow.TextOriginX := R.Left;
      end;
      if ATextParams.PreventLeftExceed and (ARow.TextOriginX < R.Left) then
        ARow.TextOriginX := R.Left;

      // Vertically
      ARow.TextOriginY := ATopRowOffset;
      Inc(ATopRowOffset, ATextParams.FullRowHeight);
      if AExtra > 0 then
      begin
        Inc(ATopRowOffset);
        Dec(AExtra);
      end;
    end;
  end;

var
  Extra, TopRowOffset: Integer;
begin
  CalcExtraAndTopRowOffset(Extra, TopRowOffset);
  PlaceRows(Extra, TopRowOffset);
end;

function cxPrepareRect(const R: TRect; const ATextParams: TcxTextParams; ALeftIndent, ARightIndent: Integer): TRect;
begin
  Result := R;
  with Result, ATextParams do
  begin
    if AutoIndents then
      InflateRect(Result, -cxTextSpace * OnePixel, -cxTextSpace * OnePixel);
    Inc(Left, ALeftIndent * OnePixel);
    Dec(Right, ARightIndent * OnePixel);
  end;
end;

function cxUnprepareRect(const R: TRect; const ATextParams: TcxTextParams; ALeftIndent: Integer = 0; ARightIndent: Integer = 0): TRect;
begin
  Result := R;
  with Result, ATextParams do
  begin
    Dec(Left, ALeftIndent * OnePixel);
    Inc(Right, ARightIndent * OnePixel);
    if AutoIndents then
      InflateRect(Result, cxTextSpace * OnePixel, cxTextSpace * OnePixel);
  end;
end;

function IsMetaFile(AHandle: TCanvasHandle): Boolean;
begin
  Result := GetObjectType(AHandle) in [OBJ_METAFILE, OBJ_METADC, OBJ_ENHMETAFILE, OBJ_ENHMETADC];
end;

procedure cxPrepareEndEllipsis(AHandle: TCanvasHandle; ATextRow: PcxTextRow;
  const ATextParams: TcxTextParams; ACalcTextExtentsProc: TcxCalcTextExtentsProc;
  var AWidth: Integer; AUserData: Pointer = nil);
var
  ACharNumber: Integer;
begin
  Dec(AWidth, ATextParams.EndEllipsisWidth);
  if AWidth < 0 then
    AWidth := 0;

  if not cxGetTextExtentExPoint(AHandle, ATextRow.Text, ATextRow.TextLength, AWidth, @ACharNumber, nil) then
    ACharNumber := 0;
  if (ACharNumber = 0) and (ATextParams.TextAlignX = taLeft) then
    ACharNumber := 1;
  ATextRow.TextLength := ACharNumber;
  ATextRow.TextExtents := ACalcTextExtentsProc(AHandle,
    ATextRow.Text, ATextRow.TextLength, ATextParams.ExpandTabs, AUserData);
end;

procedure cxPrepareEndEllipsis(AHandle: TCanvasHandle; ATextRow: PcxTextRow;
  const ATextParams: TcxTextParams; ACalcTextExtentsProc: TcxCalcTextExtentsProc;
  const ABounds: TRect; var AWidth: Integer; AUserData: Pointer = nil); overload;
var
  ACharNumber: Integer;
begin
  Dec(AWidth, ATextParams.EndEllipsisWidth);
  if AWidth < 0 then
    AWidth := 0;

  if not cxGetTextExtentExPoint(AHandle, ATextRow.Text, ATextRow.TextLength, AWidth, @ACharNumber, nil) then
    ACharNumber := 0;

  if (ACharNumber = 0) and (
     (ATextParams.RTLReading and (ATextParams.TextAlignX = taRight) or
     (not ATextParams.RTLReading and (ATextParams.TextAlignX = taLeft)))) then
    ACharNumber := 1;

  ATextRow.TextLength := ACharNumber;
  ATextRow.TextExtents := ACalcTextExtentsProc(AHandle, ATextRow.Text, ATextRow.TextLength, ATextParams.ExpandTabs, AUserData);

  if ATextParams.RTLReading then
  begin
    case ATextParams.TextAlignX of
      taLeft:
        Inc(ATextRow.TextOriginX, ATextParams.EndEllipsisWidth);
      taCenterX:
        ATextRow.TextOriginX := ABounds.Left + ATextParams.EndEllipsisWidth +
          (ABounds.Width - ATextRow.TextExtents.cx - ATextParams.EndEllipsisWidth) div 2;
      else
        ATextRow.TextOriginX := ABounds.Right - ATextRow.TextExtents.cx;
    end;
  end
  else
  begin
    case ATextParams.TextAlignX of
      taRight:
        ATextRow.TextOriginX := ABounds.Right - (ATextRow.TextExtents.cx + ATextParams.EndEllipsisWidth);
      taCenterX:
        ATextRow.TextOriginX := ABounds.Left + (ABounds.Width - ATextRow.TextExtents.cx - ATextParams.EndEllipsisWidth) div 2;
    end;
  end
end;

{$WARNINGS OFF}
procedure cxTextRowsOutHighlight(AHandle: TCanvasHandle; const ABounds: TRect;
  const ATextParams: TcxTextParams; const ATextRows: TcxTextRows; ARowCount,
  ASelStart, ASelLength: Integer; ASelBkgColor, ASelTextColor: TColor;
  AForceEndEllipsis: Boolean);

  procedure OutTextRowAsPattern(ATextRow: PcxTextRow; R: TRect);
  var
    ABkColor: COLORREF;
  begin
    InflateRect(R, 0, -ATextParams.OnePixel);
    with R do
    begin
      if Bottom <= Top then
        Bottom := Top + ATextParams.OnePixel;
      Left := ATextRow.TextOriginX;
      if Right > Left + ATextRow.TextExtents.cX then
        Right := Left + ATextRow.TextExtents.cX;
    end;

    ABkColor := SetBkColor(AHandle, ColorToRGB(clWindow));
    FillRect(AHandle, R, FillPatterns[ATextParams.Bold]);
    SetBkColor(AHandle, ABkColor);
  end;

  procedure OutTextRow(ATextRow: PcxTextRow; const R: TRect);
  var
    Stub, X: Integer;
  begin
    X := ATextRow.TextOriginX;
    if ATextParams.ExpandTabs then
    begin
      Stub := 0;
      if ATextParams.RTLReading then
        Inc(X, ATextRow.TextExtents.cx);
      cxTabbedTextOutW(AHandle, X, ATextRow.TextOriginY, ATextRow.Text, ATextRow.TextLength, 0, Stub, X);
    end
    else
      TextOutW(AHandle, X, ATextRow.TextOriginY, ATextRow.Text, ATextRow.TextLength);
  end;

  procedure PrepareEndEllipsis(ATextRow: PcxTextRow; const ABounds: TRect; var AWidth: Integer);
  begin
    cxPrepareEndEllipsis(AHandle, ATextRow, ATextParams, @cxCalcTextExtentsEx, ABounds, AWidth);
  end;

  procedure OutEndEllipsis(ATextRow: PcxTextRow; R: TRect);
  const
    ClipTexts: array[Boolean] of UINT = (0, ETO_CLIPPED);
  var
    fuOptions: UINT;
  begin
    if ATextParams.RTLReading then
    begin
      case ATextParams.TextAlignX of
        taLeft:
          R.Right := R.Left + ATextParams.EndEllipsisWidth;
        else
          R.Right := ATextRow.TextOriginX;
      end;
      fuOptions := ClipTexts[not ATextParams.NoClip and (R.Right - ATextParams.EndEllipsisWidth < R.Left)] or ETO_RTLREADING;
      R.Left := Max(R.Left, R.Right - ATextParams.EndEllipsisWidth);
    end
    else
    begin
      R.Left := ATextRow.TextOriginX + ATextRow.TextExtents.cx;
      fuOptions := ClipTexts[not ATextParams.NoClip and (R.Left + ATextParams.EndEllipsisWidth > R.Right)];
      R.Right := Min(R.Left + ATextParams.EndEllipsisWidth, R.Right);
    end;
    if R.Left < R.Right then
    begin
      ExtTextOutW(AHandle, R.Left, ATextRow.TextOriginY, fuOptions,
        @R, cxEndEllipsisChars, cxEndEllipsisCharsLength, nil);
    end;
  end;

  function GetSubstringWidth(AText: PChar; ATextLength, ASubstringLength: Integer): Integer;
  var
    ACharExtents: array of Integer;
  begin
    if ASubstringLength = 0 then
      Result := 0
    else
    begin
      SetLength(ACharExtents, ATextLength);
      cxGetTextExtentExPoint(AHandle, AText, ATextLength, 0, nil, @ACharExtents[0]);
      Result := ACharExtents[ASubstringLength - 1];
    end;
  end;

var
  ARowRect, AHighlightRect: TRect;
  AWidth, I, F, L, ABreakExtra, APrevBkMode, APrevTextAlign: Integer;
  ASaveTextColor: TColor;
  ANeedClip, ANeedEndEllipsis, AIsMetafile, AHasSelTextColor: Boolean;
  ATextRow: PcxTextRow;
  AHighlightStart, AHighlightEnd: Integer;
  ARgn, AHighlightRgn, ASaveClipRgn: HRGN;
  ABrush: HBRUSH;
  AOrigin: TPoint;
begin
  ASelBkgColor := ColorToRGB(ASelBkgColor);
  AHasSelTextColor := (ASelTextColor <> clNone) and (ASelTextColor <> clDefault);
  if AHasSelTextColor then
    ASelTextColor := ColorToRGB(ASelTextColor);
  AWidth := ABounds.Right - ABounds.Left;
  APrevBkMode := SetBkMode(AHandle, Windows.TRANSPARENT);
  APrevTextAlign := GetTextAlign(AHandle);
  if ATextParams.RTLReading then
    SetTextAlign(AHandle, APrevTextAlign or TA_RTLREADING or TA_LEFT);
  if (ASelLength > 0) and (ASelBkgColor <> clNone) and (ASelBkgColor <> clDefault) then
    ABrush := CreateSolidBrush(ASelBkgColor)
  else
    ABrush := 0;
  AIsMetafile := IsMetaFile(AHandle);
  ARowRect := ABounds;
  GetWindowOrgEx(AHandle, AOrigin);
  for I := 0 to ARowCount - 1 do
  begin
    ATextRow := cxGetTextRow(ATextRows, I);
    with ATextRow^ do
    begin
      if TextLength <> 0 then
      begin
        ARowRect.Top := TextOriginY;
        ARowRect.Bottom := ARowRect.Top + ATextParams.FullRowHeight;
        if {AIsMetafile or RectVisible(AHandle, ARowRect)}true then
        begin
          if ATextParams.PatternedText then
            OutTextRowAsPattern(ATextRow, ARowRect)
          else
          begin
            ANeedEndEllipsis := ATextParams.EndEllipsis and (I = ARowCount - 1) and
              ((TextExtents.cx > AWidth) or AForceEndEllipsis);
            ABreakExtra := 0;
            if (ATextParams.TextAlignX in [taJustifyX, taDistributeX]) and not ANeedEndEllipsis then
            begin
              ABreakExtra := AWidth - TextExtents.cX;
              if (BreakCount <> 0) and (ABreakExtra > 0) then
                SetTextJustification(AHandle, ABreakExtra, BreakCount);
            end;

            ANeedClip := not ATextParams.NoClip and
              ((TextExtents.cX > AWidth) or (ARowRect.Top < ABounds.Top) or (ARowRect.Bottom > ABounds.Bottom));
            if ANeedClip then
            begin
              if ARowRect.Top < ABounds.Top then ARowRect.Top := ABounds.Top;
              if ARowRect.Bottom > ABounds.Bottom then ARowRect.Bottom := ABounds.Bottom;
              ARgn := IntersectClipRect(AHandle, ARowRect);
            end;

            if ANeedEndEllipsis then
              PrepareEndEllipsis(ATextRow, ABounds, AWidth);

            if (ASelLength > 0) and not AIsMetafile then
            begin
              if not ((ASelStart >= StartOffset + TextLength) or
                (ASelStart + ASelLength <= StartOffset)) then
              begin
                F := Max(ASelStart, StartOffset);
                L := Min(ASelStart + ASelLength, StartOffset + TextLength);
                Dec(F, StartOffset);
                Dec(L, StartOffset);
                if L > F then
                begin
                  if ATextParams.RTLReading then
                  begin
                    AHighlightRect.InitSize(TextOriginX, ARowRect.Top, TextExtents.cx, ARowRect.Height);
                    AHighlightRgn := TdxRTLReadingCharacterPlacementHelper.GetSelectionRangeRegion(AHandle, Text, TextLength,
                      AHighlightRect, F, L - F, 0, ATextParams.RTLReading);
                    if AHighlightRgn <> 0 then
                    begin
                      TdxGdiClippingHelper.SaveClipping(AHandle, ASaveClipRgn);
                      OffsetRgn(AHighlightRgn, -AOrigin.X, -AOrigin.Y);
                      ExtSelectClipRgn(AHandle, AHighlightRgn, RGN_AND);
                      if ABrush <> 0 then
                      begin
                        OffsetRgn(AHighlightRgn, AOrigin.X, AOrigin.Y);
                        FillRgn(AHandle, AHighlightRgn, ABrush);
                        OffsetRgn(AHighlightRgn, -AOrigin.X, -AOrigin.Y);
                      end;
                      if AHasSelTextColor then
                        ASaveTextColor := SetTextColor(AHandle, ASelTextColor);
                      OutTextRow(ATextRow, ARowRect);
                      if AHasSelTextColor then
                        SetTextColor(AHandle, ASaveTextColor);
                      TdxGdiClippingHelper.RestoreClipping(AHandle, ASaveClipRgn);
                      ExtSelectClipRgn(AHandle, AHighlightRgn, RGN_DIFF);
                      DeleteObject(AHighlightRgn);
                    end
                    else
                      ANeedEndEllipsis := False;
                  end
                  else
                  begin
                    AHighlightStart := GetSubstringWidth(Text, TextLength, F);
                    AHighlightEnd := GetSubstringWidth(Text, TextLength, L);
                    begin
                      AHighlightRect.Left := TextOriginX + AHighlightStart;
                      AHighlightRect.Right := TextOriginX + AHighlightEnd;
                    end;
                    AHighlightRect.Top := ARowRect.Top;
                    AHighlightRect.Bottom := ARowRect.Bottom;
                    if not IsRectEmpty(AHighlightRect) then
                    begin
                      ASaveClipRgn := IntersectClipRect(AHandle, AHighlightRect);
                      if ABrush <> 0 then
                        FillRect(AHandle, AHighlightRect, ABrush);
                      if AHasSelTextColor then
                        ASaveTextColor := SetTextColor(AHandle, ASelTextColor);
                      OutTextRow(ATextRow, ARowRect);
                      if AHasSelTextColor then
                        SetTextColor(AHandle, ASaveTextColor);
                      RestoreClipRgn(AHandle, ASaveClipRgn);
                      with AHighlightRect do
                        ExcludeClipRect(AHandle, Left, Top, Right, Bottom);
                    end
                    else
                      ANeedEndEllipsis := False;
                  end;
                end;
                if ANeedEndEllipsis and (ASelStart + ASelLength >= StartOffset + TextLength) then
                begin
                  ANeedEndEllipsis := False;
                  ASaveTextColor := SetTextColor(AHandle, ASelTextColor);
                  OutEndEllipsis(ATextRow, ARowRect);
                  SetTextColor(AHandle, ASaveTextColor);
                end;
              end
            end;
            OutTextRow(ATextRow, ARowRect);

            if ANeedEndEllipsis then
              OutEndEllipsis(ATextRow, ARowRect);

            if ANeedClip then
              RestoreClipRgn(AHandle, ARgn);
            if ABreakExtra > 0 then
              SetTextJustification(AHandle, 0, 0);
          end;
        end;
      end
      else
        if AForceEndEllipsis and ATextParams.EndEllipsis and (I = ARowCount - 1) then
        begin
          ARowRect.Top := TextOriginY;
          ARowRect.Bottom := ARowRect.Top + ATextParams.FullRowHeight;
          PrepareEndEllipsis(ATextRow, ABounds, AWidth);
          OutEndEllipsis(ATextRow, ARowRect);
        end;
    end;
  end;

  if ABrush <> 0 then
    DeleteObject(ABrush);
  SetBkMode(AHandle, APrevBkMode);
  if ATextParams.RTLReading then
    SetTextAlign(AHandle, APrevTextAlign);
end;
{$WARNINGS ON}

function cxGetLongestTextRowWidth(const ATextRows: TcxTextRows; ARowCount: Integer): Integer;
var
  I, W: Integer;
begin
  if ARowCount > cxGetTextRowCount(ATextRows) then
    ARowCount := cxGetTextRowCount(ATextRows);
  Result := 0;
  for I := 0 to ARowCount - 1 do
  begin
    W := cxGetTextRow(ATextRows, I).TextExtents.cx;
    if W > Result then Result := W;
  end;
end;

function CanProcessText(const ATextParams: TcxTextParams; const ATextRect: TRect): Boolean;
begin
  if ATextParams.CalcRect then
    Result := (ATextRect.Right - ATextRect.Left) > 0
  else
    Result := ((ATextRect.Right - ATextRect.Left) > 0) and ((ATextRect.Bottom - ATextRect.Top) > 0);
end;

function cxTextOut(AHandle: TCanvasHandle; const AText: string; var R: TRect;
  AFormat: TcxTextOutFormat; ASelStart, ASelLength: Integer; AFont: TFont;
  ASelBkgColor, ASelTextColor: TColor; AMaxLineCount: Integer = 0;
  ALeftIndent: Integer = 0; ARightIndent: Integer = 0;
  ATextColor: TColor = clDefault; const ALineSpacingFactor: Double = 1.0): Integer;
var
  APrevFont: HFONT;
  APrevFontColor: COLORREF;
  ATextHeight, ARowCount, ATextLength: Integer;
  ATextParams: TcxTextParams;
  ATextRect: TRect;
  ATextRows: TcxTextRows;
  AForceEndEllipsis: Boolean;
  ATextPtr: PChar;
  AHidePrefixStr: string;
begin
  Result := 0;
  ATextLength := Length(AText);
  if ATextLength = 0 then Exit;
  APrevFont := GetCurrentObject(AHandle, OBJ_FONT);
  APrevFontColor := GetTextColor(AHandle);
  if AFont <> nil then
  begin
    APrevFont := SelectObject(AHandle, AFont.Handle);
    SetTextColor(AHandle, ColorToRGB(AFont.Color));
  end;
  if ATextColor <> clDefault then
    SetTextColor(AHandle, ColorToRGB(ATextColor));

  ATextParams := cxCalcTextParams(AHandle, AFormat, ALineSpacingFactor);
  ATextRect := cxPrepareRect(R, ATextParams, ALeftIndent, ARightIndent);
  ATextHeight := 0;

  if CanProcessText(ATextParams, ATextRect) then
  begin
    if ATextParams.HidePrefix then
    begin
      AHidePrefixStr := RemoveAccelChars(AText, False);
      ATextLength := Length(AHidePrefixStr);
      if ATextLength = 0 then Exit;
      ATextPtr := PChar(AHidePrefixStr);
    end
    else
      ATextPtr := PChar(AText);

    AForceEndEllipsis := not cxMakeTextRows(AHandle, ATextPtr, ATextLength,
      ATextRect, ATextParams, ATextRows, ARowCount, AMaxLineCount);
    if ARowCount <> 0 then
    try
      if ATextParams.CalcRect then
      begin
        if (AMaxLineCount > 0) and (AMaxLineCount < ARowCount) then
          ARowCount := AMaxLineCount;
        ATextRect.Right := ATextRect.Left + cxGetLongestTextRowWidth(ATextRows, ARowCount);
        if not ATextParams.SingleLine then
        begin
          cxResetTextRows(ATextRows);
          cxMakeTextRows(AHandle, ATextPtr, ATextLength, ATextRect, ATextParams, ATextRows, ARowCount, AMaxLineCount);
        end;
        cxPlaceTextRows(AHandle, ATextRect, ATextParams, ATextRows, ARowCount);
        ATextRect.Bottom := cxGetTextRow(ATextRows, ARowCount - 1).TextOriginY + ATextParams.RowHeight;
        R := cxUnprepareRect(ATextRect, ATextParams, ALeftIndent, ARightIndent);
      end
      else
      begin
        cxPlaceTextRows(AHandle, ATextRect, ATextParams, ATextRows, ARowCount);
        if (ASelStart < 0) or (ASelStart >= ATextLength) then
          ASelLength := 0
        else
          if (ASelLength + ASelStart) > ATextLength then
            ASelLength := ATextLength - ASelStart;
        cxTextRowsOutHighlight(AHandle, ATextRect, ATextParams, ATextRows,
          ARowCount, ASelStart, ASelLength, ASelBkgColor, ASelTextColor, AForceEndEllipsis);
      end;
      ATextHeight := cxGetTextRow(ATextRows, ARowCount - 1).TextOriginY + ATextParams.RowHeight - ATextRect.Top;
    finally
      cxResetTextRows(ATextRows);
    end;
  end;
  if ATextParams.CalcRowCount or (ATextHeight = 0) then
    Result := ARowCount
  else
    Result := ATextHeight;

  SelectObject(AHandle, APrevFont);
  SetTextColor(AHandle, APrevFontColor);
end;

function cxTextOut(AHandle: TCanvasHandle; const AText: string; var R: TRect;
  AFormat: TcxTextOutFormat = CXTO_DEFAULT_FORMAT; AFont: TFont = nil;
  AMaxLineCount: Integer = 0; ALeftIndent: Integer = 0; ARightIndent: Integer = 0;
  ATextColor: TColor = clDefault; const ALineSpacingFactor: Double = 1.0): Integer;
begin
  Result := cxTextOut(AHandle, AText, R, AFormat, 0, 0, AFont, clDefault, clDefault,
    AMaxLineCount, ALeftIndent, ARightIndent, ATextColor, ALineSpacingFactor);
end;

function cxTextOut(ACanvas: TCanvas; const AText: string; var R: TRect;
  AFormat: TcxTextOutFormat; ASelStart, ASelLength: Integer; AFont: TFont;
  ASelBkgColor, ASelTextColor: TColor; AMaxLineCount: Integer = 0; ALeftIndent: Integer = 0;
  ARightIndent: Integer = 0; ATextColor: TColor = clDefault;
  const ALineSpacingFactor: Double = 1.0): Integer;
begin
  if ACanvas.TextFlags and ETO_RTLREADING <> 0 then
    AFormat := AFormat or CXTO_RTLREADING;
  Result := cxTextOut(ACanvas.Handle, AText, R, AFormat, ASelStart, ASelLength,
    AFont, ASelBkgColor, ASelTextColor, AMaxLineCount, ALeftIndent, ARightIndent,
    ATextColor, ALineSpacingFactor);
end;

function cxTextOut(ACanvas: TCanvas; const AText: string; var R: TRect;
  AFormat: TcxTextOutFormat = CXTO_DEFAULT_FORMAT; AFont: TFont = nil;
  AMaxLineCount: Integer = 0; ALeftIndent: Integer = 0; ARightIndent: Integer = 0;
  ATextColor: TColor = clDefault; const ALineSpacingFactor: Double = 1.0): Integer;
begin
  Result := cxTextOut(ACanvas, AText, R, AFormat, 0, 0, AFont, clDefault,
    clDefault, AMaxLineCount, ALeftIndent, ARightIndent, ATextColor, ALineSpacingFactor);
end;

procedure cxRotatedTextOut(AHandle: TCanvasHandle; const ABounds: TRect; const AText: string; AFont: TFont;
  AAlignHorz: TcxTextAlignX = taCenterX; AAlignVert: TcxTextAlignY = taCenterY; AWordBreak: Boolean = True;
  ALeftExceed: Boolean = True; ARightExceed: Boolean = True; ADirection: TcxVerticalTextOutDirection = vtdBottomToTop;
  AFontSize: Integer = 0);

const
  Angles: array[TcxVerticalTextOutDirection] of Integer = (-900, 900);
  Flags: array[TcxVerticalTextOutDirection] of Integer =
    (TA_LEFT or TA_BOTTOM or TA_NOUPDATECP, TA_LEFT or TA_TOP or TA_NOUPDATECP);

  procedure AddRow(AList: TList; AFirstChar: PChar; ACount, AWidth: Integer);
  begin
    AList.Add(AFirstChar);
    AList.Add(Pointer(ACount));
    AList.Add(Pointer(AWidth));
  end;

  function CreateRotatedFont: HFONT;
  var
    ALogFontW: TLogFontW;
    ALogFontA: TLogFontA;
  begin
    if IsWinNT then
    begin
      FillChar(ALogFontW, SizeOf(ALogFontW), 0);
      GetObject(AFont.Handle, SizeOf(TLogFontW), @ALogFontW);
      if AFontSize <> 0 then
        ALogFontW.lfHeight := -MulDiv(AFontSize, GetDeviceCaps(AHandle, LOGPIXELSY), 72);
      ALogFontW.lfEscapement := Angles[ADirection];
      ALogFontW.lfOutPrecision := OUT_TT_ONLY_PRECIS;
      Result := CreateFontIndirectW(ALogFontW);
    end
    else
    begin
      FillChar(ALogFontA, SizeOf(ALogFontA), 0);
      GetObject(AFont.Handle, SizeOf(TLogFontA), @ALogFontA);
      if AFontSize <> 0 then
        ALogFontA.lfHeight := -MulDiv(AFontSize, GetDeviceCaps(AHandle, LOGPIXELSY), 72);
      ALogFontA.lfEscapement := Angles[ADirection];
      ALogFontA.lfOutPrecision := OUT_TT_ONLY_PRECIS;
      Result := CreateFontIndirectA(ALogFontA);
    end;
  end;

  function TextSize(ACurrentChar: PChar; ACharCount: Integer): TSize;
  begin
    GetTextExtentPoint32W(AHandle, ACurrentChar, ACharCount, Result);
  end;

  function TextWidth(const AFirstChar: PChar; ACharCount: Integer): Integer;
  var
    ASize: TSize;
  begin
    GetTextExtentPoint32W(AHandle, AFirstChar, ACharCount, ASize);
    Result := ASize.cx;
  end;

  function MakeRow(var AFirstChar: PChar; ALastChar: PChar;
    ACharCount, ARowCharCount: Integer; ATextRows: TList): Integer;
  begin
    // make break and move first point to current point
    AddRow(ATextRows, AFirstChar, ARowCharCount, TextWidth(AFirstChar, ARowCharCount));
    if ALastChar^ = ' ' then
    begin
      Dec(ACharCount);
      Inc(ALastChar);
    end;
    AFirstChar := ALastChar;
    Result := ACharCount - ARowCharCount;
  end;

  procedure CalculateWordWrappedTextRows(AFirstChar: PChar;
    ACharCount, ATextWidth: Integer; ATextRows: TList);
  var
    ACurrentChar, APrevBreakChar: PChar;
    AIsBreakChar, AHasPrevBreak: Boolean;
    APos, APrevBreakPos, AWidth: Integer;
  begin
    while ACharCount > 0 do
    begin
      ACurrentChar := AFirstChar;
      APos := 0;
      AHasPrevBreak := False;
      AWidth := 0;
      APrevBreakPos := 0;
      APrevBreakChar := ' ';
      while APos < ACharCount do
      begin
        Inc(ACurrentChar);
        Inc(APos);
        AIsBreakChar := (ACurrentChar^ = Space) or (ACurrentChar^ = CR) or (ACurrentChar^ = LF);
        if AIsBreakChar or (APos = ACharCount) then
        begin
          AWidth := TextWidth(AFirstChar, APos);
          if AWidth < ATextWidth then
          begin
            AHasPrevBreak := AIsBreakChar;
            APrevBreakPos := APos;
            APrevBreakChar := ACurrentChar;
          end;
        end;
        if (AIsBreakChar and ((AWidth > ATextWidth) or (ACurrentChar^ = CR) or (ACurrentChar^ = LF))) or (APos = ACharCount) then
        begin
          if AHasPrevBreak and (AWidth > ATextWidth) then
          begin
            APos := APrevBreakPos;
            ACurrentChar := APrevBreakChar;
          end;
          ACharCount := MakeRow(AFirstChar, ACurrentChar, ACharCount, APos, ATextRows);
          Break;
        end;
      end;
    end;
  end;

  function ProcessHorizontalAlignment(const ATextBounds: TRect;
    AWidth: Integer): Integer;
  var
    ARightPos, ALeftPos: Integer;
  begin
    if ADirection = vtdBottomToTop then
    begin
      ARightPos := ATextBounds.Top + AWidth;
      ALeftPos := ATextBounds.Bottom;
    end
    else
    begin
      ALeftPos := ATextBounds.Top;
      ARightPos := ATextBounds.Bottom - AWidth;
    end;
    if AAlignHorz = taLeft then
      Result := ALeftPos
    else
      if AAlignHorz = taRight then
        Result := ARightPos
      else
      begin
        if ADirection = vtdBottomToTop then
          Result := (ATextBounds.Top + ATextBounds.Bottom + AWidth) div 2
        else
          Result := (ATextBounds.Top + ATextBounds.Bottom - AWidth) div 2
      end;
    if AWidth > (ATextBounds.Bottom - ATextBounds.Top) then
    begin
      if ARightExceed then
        Result := ARightPos
      else
        if ALeftExceed then
          Result := ALeftPos;
    end;
  end;

  function ProcessVerticalAlignment(const ATextBounds: TRect;
    ARowHeight, ARowCount: Integer): Integer;
  begin
    if ADirection = vtdBottomToTop then
    begin
      // align by horizontally
      if AAlignVert = taBottom then
      begin
        Result := ATextBounds.Right - ARowHeight * ARowCount;
        if Result < ATextBounds.Left then
          Result := ATextBounds.Left;
      end
      else
        if AAlignVert = taCenterY then
          Result := (ATextBounds.Left + ATextBounds.Right - ARowHeight * ARowCount) div 2
        else
          Result := ATextBounds.Left;
      if Result < ATextBounds.Left then
        Result := ATextBounds.Left;
    end
    else
    begin
      if AAlignVert = taTop then
        Result := ATextBounds.Right - ARowHeight
      else
        if AAlignVert = taCenterY then
          Result := (ATextBounds.Left - ARowHeight + ATextBounds.Right + ARowHeight * (ARowCount - 1)) div 2
        else
          Result := ATextBounds.Left + ARowHeight * (ARowCount - 1);
      if Result > (ATextBounds.Right - ARowHeight) then
        Result := ATextBounds.Right - ARowHeight;
    end;
  end;

  function GetRowData(ATextRows: TList; ARowIndex: Integer): Pointer;
  begin
    Result := ATextRows.List[ARowIndex];
  end;

  procedure PlaceTextRows(ATextRows: TList; ATextBounds: TRect);
  var
    ASize: TSize;
    I, ALeft, ATop, ARowCount, AWidth: Integer;
  begin
    GetTextExtentPoint32W(AHandle, dxMeasurePattern, 2, ASize);
    ARowCount := ATextRows.Count div 3;
    ALeft := ProcessVerticalAlignment(ATextBounds, ASize.cy, ARowCount);
    for I := 0 to ARowCount - 1 do
    begin
     // align by vertically
      AWidth := Integer(GetRowData(ATextRows, I * 3 + 2));
      ATop := ProcessHorizontalAlignment(ATextBounds, AWidth);
      // out text row
      ExtTextOutW(AHandle, ALeft, ATop, 0{ETO_CLIPPED}, @ATextBounds,
        PChar(GetRowData(ATextRows, I * 3)), Integer(GetRowData(ATextRows, I * 3 + 1)), nil);
      // offset place and check visibility
      if ADirection = vtdBottomToTop then
        Inc(ALeft, ASize.cy)
      else
        Dec(ALeft, ASize.cy);
      if (ALeft < ATextBounds.Left) or (ALeft > ATextBounds.Right) then
        Break;
    end;
  end;

var
  ATextRows: TList;
  AFontHandle: HFONT;
  ATextBounds: TRect;
  ACharCount, AMode, AWidth: Integer;
begin
  ACharCount := Length(AText);
  if ACharCount = 0 then Exit;
  ATextBounds := ABounds;
  InflateRect(ATextBounds, -2, -2);
  AWidth := ATextBounds.Bottom - ATextBounds.Top;
  AFontHandle := SelectObject(AHandle, CreateRotatedFont);
  AMode := SetBkMode(AHandle, Windows.TRANSPARENT);
  ATextRows := TList.Create;
  try
    ATextRows.Capacity := Length(AText) * 3;
    SetTextAlign(AHandle, Flags[ADirection]);
    if not AWordBreak then
      AddRow(ATextRows, @AText[1], ACharCount, TextWidth(@AText[1], ACharCount))
    else
      CalculateWordWrappedTextRows(@AText[1], ACharCount, AWidth, ATextRows);
    PlaceTextRows(ATextRows, ATextBounds);
  finally
    SetBkMode(AHandle, AMode);
    AFontHandle := SelectObject(AHandle, AFontHandle);
    DeleteObject(AFontHandle);
    ATextRows.Free;
  end;
end;

procedure CreateFillPatterns;
const
  BoldPatternBits: array[0..7] of Word = ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000);
  StandardPatternBits: array[0..7] of Word = ($5555, $AAAA, $5555, $AAAA, $5555, $AAAA, $5555, $AAAA);

  function CreateFillPattern(AIsBold: Boolean): HBRUSH;
  var
    ABits:  Pointer ;
    ABitmap: HBITMAP;
  begin
    if AIsBold then
      ABits := @BoldPatternBits
    else
      ABits := @StandardPatternBits;
    ABitmap := CreateBitmap(8, 8, 1, 1, ABits);
    try
      Result := CreatePatternBrush(ABitmap);
    finally
      DeleteObject(ABitmap);
    end;
  end;

begin
  FillPatterns[False] := CreateFillPattern(False);
  FillPatterns[True] := CreateFillPattern(True);
end;

procedure DestroyFillPatterns;
begin
  if FillPatterns[False] <> 0 then DeleteObject(FillPatterns[False]);
  if FillPatterns[True] <> 0 then DeleteObject(FillPatterns[True]);
end;

initialization
  CreateFillPatterns;

finalization
  DestroyFillPatterns;

end.
