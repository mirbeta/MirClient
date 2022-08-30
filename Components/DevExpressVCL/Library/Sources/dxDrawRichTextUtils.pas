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

unit dxDrawRichTextUtils;

{$I cxVer.inc}

interface

uses
  Windows, Controls, Messages, Types, Classes, Math, SysUtils, Graphics, RichEdit, ComCtrls, cxGeometry;

const
  {$EXTERNALSYM EM_GETZOOM}
  EM_GETZOOM = WM_USER + 224;
  {$EXTERNALSYM EM_SETZOOM}
  EM_SETZOOM = WM_USER + 225;

type

  { TdxRichTextHelper }

  TdxRichTextHelper = class
  strict private
    FRichEdit: TRichEdit;
    FScaleFactor: TdxScaleFactor;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CalculateTextHeight(ACanvas: TCanvas; var ARect: TRect); virtual;
    procedure DrawText(ACanvas: TCanvas; const ARect: TRect); virtual;
    procedure Init(ACanvas: TCanvas; const AText: string; AScaleFactor: TdxScaleFactor); virtual;
  end;

procedure dxDrawRichEdit(ADC: HDC; const ARect: TRect; ARichHandle: HWND;
  AMinCharIndex, AMaxCharIndex: Integer; ACalculateHeight: Boolean; out AHeight: Integer); overload;
procedure dxDrawRichEdit(ADC: HDC; ARect: TRect; ARichHandle: HWND; AScaleFactor: TdxScaleFactor;
  AMinCharIndex, AMaxCharIndex: Integer; ACalculateHeight: Boolean; out AHeight: Integer); overload;
procedure dxDrawRichEdit(ACanvas: TCanvas; const ARect: TRect; ARichEdit: TRichEdit;
  AMinCharIndex, AMaxCharIndex: Integer; ACalculateHeight: Boolean; out AHeight: Integer); overload;

function dxIsRichText(const AText: string): Boolean;
function dxPixelsToTwips(AValue: Integer): Integer;
function dxTwipsToPixels(AValue: Integer): Integer;

procedure dxRichLoadFromString(ALines: TStrings; const S: string);
implementation

uses
  cxClasses, cxGraphics, cxControls, dxDPIAwareUtils;

const
  TwipsPerInch = 1440;

procedure dxDrawRichEdit(ADC: HDC; const ARect: TRect; ARichHandle: HWND;
  AMinCharIndex, AMaxCharIndex: Integer; ACalculateHeight: Boolean; out AHeight: Integer);
begin
  dxDrawRichEdit(ADC, ARect, ARichHandle, dxDefaultScaleFactor, AMinCharIndex, AMaxCharIndex, ACalculateHeight, AHeight);
end;

procedure dxInternalDrawRichEdit(ADC: HDC; ARect: TRect; ARichHandle: HWND; AZoomFactor: TdxScaleFactor;
  AMinCharIndex, AMaxCharIndex: Integer; ACalculateHeight: Boolean; out AHeight: Integer);
type
  TState = record
    Assigned: Boolean;
    MapMode: Integer;
    ViewPortExt: TSize;
    ViewPortOrg: TPoint;
    WindowExt: TSize;
  end;

  procedure PrepareCanvas(out AState: TState);
  const
    CanvasSize = TwipsPerInch;
  begin
    AState.Assigned := AZoomFactor.Assigned;
    if AState.Assigned then
    begin
      AState.MapMode := SetMapMode(ADC, MM_ANISOTROPIC);
      SetWindowExtEx(ADC, CanvasSize, CanvasSize, @AState.WindowExt);
      SetViewportExtEx(ADC, AZoomFactor.Apply(CanvasSize), AZoomFactor.Apply(CanvasSize), @AState.ViewPortExt);
      SetViewportOrgEx(ADC, ARect.Left, ARect.Top, @AState.ViewPortOrg);
      ARect := cxRectSetNullOrigin(ARect);
    end;
  end;

  procedure RestoreCanvas(const AState: TState);
  begin
    if AState.Assigned then
    begin
      SetViewportOrgEx(ADC, AState.ViewPortOrg.X, AState.ViewPortOrg.Y, nil);
      SetViewportExtEx(ADC, AState.ViewPortExt.cx, AState.ViewPortExt.cy, nil);
      SetWindowExtEx(ADC, AState.WindowExt.cx, AState.WindowExt.cy, nil);
      SetMapMode(ADC, AState.MapMode);
    end;
  end;

  function PrepareRect(const R: TRect): TRect;
  begin
    Result.Top := dxPixelsToTwips(R.Top);
    Result.Left := dxPixelsToTwips(R.Left);
    Result.Right := Result.Left + dxPixelsToTwips(R.Right - R.Left);
    if ACalculateHeight then
      Result.Bottom := Result.Top + TwipsPerInch
    else
      Result.Bottom := Result.Top + dxPixelsToTwips(R.Bottom - R.Top);
  end;

var
  AFormatRange: TFormatRange;
  AStartIndex: Integer;
  AState: TState;
begin
  PrepareCanvas(AState);
  try
    SendMessage(ARichHandle, EM_FORMATRANGE, 0, 0);
    try
      AHeight := 0;
      ZeroMemory(@AFormatRange, SizeOf(AFormatRange));
      AFormatRange.hdc := ADC;
      AFormatRange.hdcTarget := ADC;
      AFormatRange.chrg.cpMin := AMinCharIndex;
      AFormatRange.chrg.cpMax := AMaxCharIndex;
      repeat
        AFormatRange.rc := PrepareRect(ARect);
        AFormatRange.rcPage := cxRectSetNullOrigin(AFormatRange.rc);
        AStartIndex := AFormatRange.chrg.cpMin;
        AFormatRange.chrg.cpMin := cxSendStructMessage(ARichHandle, EM_FORMATRANGE, WPARAM(not ACalculateHeight), AFormatRange);
        if AFormatRange.chrg.cpMin <= AStartIndex then
          Break;
        if ACalculateHeight then
          Inc(AHeight, cxRectHeight(AFormatRange.rc));
      until not ACalculateHeight;

      if ACalculateHeight then
        AHeight := MulDiv(AHeight, AZoomFactor.Apply(GetDeviceCaps(ADC, LOGPIXELSY)), TwipsPerInch);
    finally
      SendMessage(ARichHandle, EM_FORMATRANGE, 0, 0);
    end;
  finally
    RestoreCanvas(AState);
  end;
end;

procedure dxDrawRichEdit(ADC: HDC; ARect: TRect; ARichHandle: HWND; AScaleFactor: TdxScaleFactor;
  AMinCharIndex, AMaxCharIndex: Integer; ACalculateHeight: Boolean; out AHeight: Integer);
var
  AZoomFactor: TdxScaleFactor;
  N, D: Integer;
begin
  AZoomFactor := TdxScaleFactor.Create;
  try
    if Boolean(SendMessage(ARichHandle, EM_GETZOOM, WPARAM(@N), LPARAM(@D))) and (D > 0) and (N <> D) then
      AZoomFactor.Assign(N, D)
    else
      AZoomFactor.Assign(AScaleFactor.TargetDPI, dxSystemScaleFactor.TargetDPI);
    dxInternalDrawRichEdit(ADC, ARect, ARichHandle, AZoomFactor, AMinCharIndex, AMaxCharIndex,
      ACalculateHeight, AHeight);
  finally
    AZoomFactor.Free;
  end;
end;

procedure dxDrawRichEdit(ACanvas: TCanvas; const ARect: TRect; ARichEdit: TRichEdit;
  AMinCharIndex, AMaxCharIndex: Integer; ACalculateHeight: Boolean; out AHeight: Integer);
begin
  dxDrawRichEdit(ACanvas.Handle, ARect, ARichEdit.Handle, AMinCharIndex, AMaxCharIndex, ACalculateHeight, AHeight);
end;

function dxIsRichText(const AText: string): Boolean;
const
  ARichPrefix = '{\rtf';
begin
  Result := Copy(AText, 1, Length(ARichPrefix)) = ARichPrefix;
end;

function dxPixelsToTwips(AValue: Integer): Integer;
begin
  Result:= MulDiv(AValue, TwipsPerInch, cxGetPixelsPerInch.cy);
end;

function dxTwipsToPixels(AValue: Integer): Integer;
begin
  Result:= MulDiv(AValue, cxGetPixelsPerInch.cy, TwipsPerInch);
end;

procedure dxRichLoadFromString(ALines: TStrings; const S: string);
var
  AStream: TStringStream;
  AEncoding: TEncoding;
begin
  if dxIsRichText(S) then
    AEncoding := TEncoding.Default
  else
    AEncoding := TEncoding.Unicode;

  AStream := TStringStream.Create(S, AEncoding);
  try
    ALines.LoadFromStream(AStream, AEncoding);
  finally
    AStream.Free;
  end;
end;

{ TdxRichTextHelper }

constructor TdxRichTextHelper.Create;
begin
  inherited Create;
  FRichEdit := TRichEdit.Create(nil);
  FRichEdit.ParentWindow := cxMessageWindow.Handle;
  FScaleFactor := TdxScaleFactor.Create;
  SendMessage(FRichEdit.Handle, EM_SETEVENTMASK, 0, 0);
end;

destructor TdxRichTextHelper.Destroy;
begin
  FreeAndNil(FScaleFactor);
  FreeAndNil(FRichEdit);
  inherited Destroy;
end;

procedure TdxRichTextHelper.CalculateTextHeight(ACanvas: TCanvas; var ARect: TRect);
var
  AHeight: Integer;
begin
  ACanvas.Lock;
  try
    dxDrawRichEdit(ACanvas.Handle, ARect, FRichEdit.Handle, FScaleFactor, 0, -1, True, AHeight);
    ARect.Bottom := ARect.Top + AHeight;
  finally
    ACanvas.Unlock;
  end;
end;

procedure TdxRichTextHelper.DrawText(ACanvas: TCanvas; const ARect: TRect);
var
  AHeight: Integer;
begin
  ACanvas.Lock;
  try
    dxDrawRichEdit(ACanvas.Handle, ARect, FRichEdit.Handle, FScaleFactor, 0, -1, False, AHeight);
  finally
    ACanvas.Unlock;
  end;
end;

procedure TdxRichTextHelper.Init(ACanvas: TCanvas; const AText: string; AScaleFactor: TdxScaleFactor);
begin
  FRichEdit.HandleNeeded;
  FScaleFactor.Assign(AScaleFactor.TargetDPI, dxSystemScaleFactor.TargetDPI);
  SetWindowLong(FRichEdit.Handle, GWL_EXSTYLE, GetWindowLong(FRichEdit.Handle, GWL_EXSTYLE) or WS_EX_TRANSPARENT);
  FRichEdit.DefAttributes.Assign(ACanvas.Font);
  FRichEdit.DefAttributes.Color := ACanvas.Font.Color;
  dxRichLoadFromString(FRichEdit.Lines, AText);
end;

end.
