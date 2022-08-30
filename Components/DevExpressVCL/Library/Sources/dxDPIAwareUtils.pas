{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
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

unit dxDPIAwareUtils;

{$I cxVer.inc}

{$ALIGN ON}
{$MINENUMSIZE 4}

interface

uses
  Types, Windows, Forms, MultiMon, Classes, Controls, Graphics, cxGeometry, dxCore;

const
  dxDefaultDPI = USER_DEFAULT_SCREEN_DPI;
  dxMaxDPI = 480;
  dxMinDPI = 30;

  dxDefaultDPIValues: array[0..7] of Integer = (96, 120, 144, 168, 192, 216, 240, 288);

type
  TdxProcessDpiAwareness = (pdaUnaware, pdaSystemDpiAware, pdaPerMonitorDpiAware, pdaPerMonitorDpiAwareV2);

  { IdxSourceSize }

  IdxSourceSize = interface
  ['{F8A5DDDA-FE77-45F1-8A75-203767ED5982}']
    function GetSourceSize: TSize;
  end;

  { IdxSourceDPI }

  IdxSourceDPI = interface
  ['{F722512C-50D3-4266-AA1C-399C3301BB6A}']
    function GetSourceDPI: Integer;
  end;

  { TdxScaleFactorHelper }

  TdxScaleFactorHelper = class helper for TdxScaleFactor
  public
    function TargetDPI: Integer;
  end;

procedure dxAssignFont(ATargetFont, ASourceFont: TFont; ATargetScaleFactor, ASourceScaleFactor: TdxScaleFactor);
function dxGetFontHeightForDefaultDPI(AFontSize: Integer): Integer;
function dxCheckDPIValue(AValue: Integer): Integer;

function dxAdjustWindowRectEx(var R: TRect; Style, ExStyle: Cardinal; Menu: Boolean; AScaleFactor: TdxScaleFactor): Boolean;
function dxGetCurrentDPI(AComponent: TComponent): Integer;
function dxGetCurrentScaleFactor(AComponent: TComponent; out M, D: Integer): Boolean;
function dxGetFormDPI(AForm: TCustomForm): Integer;
function dxGetDesktopDPI: Integer;
function dxGetMonitorDPI(const AMonitor: TMonitor): Integer; overload;
function dxGetMonitorDPI(const AScreenPoint: TPoint): Integer; overload;
function dxGetTargetDPI(AComponent: TComponent): Integer;
function dxGetScaleFactor(AOwner: TObject): TdxScaleFactor;
function dxGetScaleFactorForCanvas(const ACanvas: TCanvas): TdxScaleFactor;
function dxGetScaleFactorForInterface(const AOwner: IUnknown): TdxScaleFactor;
function dxGetSystemDPI: Integer;
function dxGetSystemMetrics(AIndex: Integer; AScaleFactor: TdxScaleFactor = nil): Integer;
function dxTryGetScaleFactorForControl(AControl: TControl; out AScaleFactor: TdxScaleFactor): Boolean;

function dxDefaultScaleFactor: TdxScaleFactor;
function dxDesktopScaleFactor: TdxScaleFactor;
function dxSystemScaleFactor: TdxScaleFactor;

function dxIsProcessDPIAware: Boolean;
function dxIsProcessPerMonitorV2Aware: Boolean;
function dxGetProcessDpiAwareness: TdxProcessDpiAwareness; overload;
function dxGetProcessDpiAwareness(AProcess: THandle): TdxProcessDpiAwareness; overload; deprecated;
function dxGetProcessDpiAwareness(AProcess: THandle; out AValue: TdxProcessDpiAwareness): Boolean; overload;
function dxSetProcessDpiAwareness(AValue: TdxProcessDpiAwareness): Boolean;
implementation

uses
  Registry, SysUtils, Math;

const
  Shcore = 'Shcore.dll';

  DPI_AWARENESS_CONTEXT_INVALID = 0;

type
  TControlAccess = class(TControl);
  TCustomFormAccess = class(TCustomForm);

  TProcessDpiAwareness = (
    PROCESS_DPI_UNAWARE = 0,
    PROCESS_SYSTEM_DPI_AWARE = 1,
    PROCESS_PER_MONITOR_DPI_AWARE = 2
  );

  TDpiAwarenessContext = type THandle;

  TAdjustWindowRectExForDPI = function (var lpRect: TRect; dwStyle: DWORD; bMenu: BOOL; dwExStyle: DWORD; DPI: UINT): BOOL; stdcall;
  TGetDpiForMonitorFunc = function (Monitor: HMONITOR; dpiType: Cardinal; out dpiX: UINT; out dpiY: UINT): HRESULT; stdcall;
  TGetProcessDpiAwarenessFunc = function (Process: THandle; out value: TProcessDpiAwareness): HRESULT; stdcall;
  TGetSystemMetricsForDpiFunc = function (Index, DPI: Integer): Integer; stdcall;
  TGetThreadDpiAwarenessContext = function : TDpiAwarenessContext; stdcall;
  TSetProcessDpiAwarenessFunc = function (value: TProcessDpiAwareness): HRESULT; stdcall;
  TSetThreadDpiAwarenessContext = function (Value: TDpiAwarenessContext): TDpiAwarenessContext; stdcall;

type
  { TdxSystemBasedScaleFactor }

  TdxSystemBasedScaleFactor = class(TdxScaleFactor)
  public
    procedure AfterConstruction; override;
    procedure Update; virtual; abstract;
  end;

  { TdxDesktopScaleFactor }

  TdxDesktopScaleFactor = class(TdxSystemBasedScaleFactor)
  public
    procedure Update; override;
  end;

  { TdxSystemScaleFactor }

  TdxSystemScaleFactor = class(TdxSystemBasedScaleFactor)
  public
    procedure Update; override;
  end;

var
  FUnitIsFinalized: Boolean = False;
  FDefaultScaleFactor: TdxScaleFactor = nil;
  FDesktopScaleFactor: TdxSystemBasedScaleFactor = nil;
  FSystemScaleFactor: TdxSystemBasedScaleFactor = nil;

  FAdjustWindowRectExForDpi: TAdjustWindowRectExForDPI = nil;
  FGetDpiForMonitor: TGetDpiForMonitorFunc;
  FGetProcessDpiAwareness: TGetProcessDpiAwarenessFunc;
  FGetSystemMetricsForDpi: TGetSystemMetricsForDpiFunc;
  FGetThreadDpiAwarenessContext: TGetThreadDpiAwarenessContext;
  FSetProcessDpiAwareness: TSetProcessDpiAwarenessFunc;
  FSetThreadDpiAwarenessContext: TSetThreadDpiAwarenessContext;
  FShellCoreLibHandle: THandle;

procedure dxAssignFont(ATargetFont, ASourceFont: TFont; ATargetScaleFactor, ASourceScaleFactor: TdxScaleFactor);
begin
  ATargetFont.Assign(ASourceFont);
  ATargetFont.Height := ATargetScaleFactor.Apply(ASourceFont.Height, ASourceScaleFactor);
end;

function dxGetFontHeightForDefaultDPI(AFontSize: Integer): Integer;
begin
  Result := -MulDiv(AFontSize, dxDefaultDPI, 72);
end;

function dxCheckDPIValue(AValue: Integer): Integer;
begin
  Result := Min(Max(AValue, dxMinDPI), dxMaxDPI);
end;

function dxAdjustWindowRectEx(var R: TRect; Style, ExStyle: Cardinal; Menu: Boolean; AScaleFactor: TdxScaleFactor): Boolean;
begin
  if Assigned(FAdjustWindowRectExForDpi) then
    Result := FAdjustWindowRectExForDpi(R, Style, Menu, ExStyle, AScaleFactor.TargetDPI)
  else
  begin
    Result := AdjustWindowRectEx(R, Style, Menu, ExStyle);
    if not AScaleFactor.Equals(dxSystemScaleFactor) then
    begin
      Inc(R.Top, GetSystemMetrics(SM_CYCAPTION));
      R := AScaleFactor.Apply(R, dxSystemScaleFactor);
      if AScaleFactor.TargetDPI > dxSystemScaleFactor.TargetDPI then
        R := cxRectInflate(R, -1);
      Dec(R.Top, dxGetSystemMetrics(SM_CYCAPTION, AScaleFactor));
    end;
  end;
end;

function dxGetCurrentDPI(AComponent: TComponent): Integer;
var
  M, D: Integer;
begin
  Result := dxDefaultDPI;
  if dxGetCurrentScaleFactor(AComponent, M, D) then
    Result := MulDiv(Result, M, D);
end;

function dxGetFormDPI(AForm: TCustomForm): Integer;
begin
{$IFDEF DELPHIBERLIN}
  Result := TCustomFormAccess(AForm).FCurrentPPI;
  if Result = 0 then
{$ENDIF}
    Result := TCustomFormAccess(AForm).PixelsPerInch;
end;

function dxGetCurrentScaleFactor(AComponent: TComponent; out M, D: Integer): Boolean;
var
  AIntf: IdxScaleFactor;
begin
  Result := True;
  if AComponent is TCustomForm then
  begin
    M := dxGetFormDPI(TCustomFormAccess(AComponent));
    D := dxDefaultDPI;
  end
  else

  if Supports(AComponent, IdxScaleFactor, AIntf) then
  begin
    M := AIntf.Value.Numerator;
    D := AIntf.Value.Denominator;
  end
  else

  if AComponent is TControl then
    Result := dxGetCurrentScaleFactor(TControl(AComponent).Parent, M, D)
  else

  if AComponent is TDataModule then
  begin
    M := dxGetSystemDPI;
    D := dxDefaultDPI;
  end
  else
    Result := False;
end;

function dxGetDesktopDPI: Integer;
const
  SAppliedDPI = 'AppliedDPI';
var
  ARegistry: TRegistry;
begin
  ARegistry := TRegistry.Create;
  try
    ARegistry.RootKey := HKEY_CURRENT_USER;
    if ARegistry.OpenKeyReadOnly('Control Panel\Desktop\WindowMetrics') and ARegistry.ValueExists(SAppliedDPI) then
      Result := ARegistry.ReadInteger(SAppliedDPI)
    else
      Result := dxGetSystemDPI;
  finally
    ARegistry.Free;
  end;
end;

function dxGetSystemDPI: Integer;
var
  DC: Integer;
begin
  DC := GetDC(0);
  try
    // #AI: don't use cached value
    Result := GetDeviceCaps(DC, LOGPIXELSY);
  finally
    ReleaseDC(0, DC);
  end;
end;

function dxGetMonitorDPI(const AMonitor: TMonitor): Integer;
{$IFNDEF DELPHI10SEATTLE}
var
  X, Y: Cardinal;
{$ENDIF}
begin
{$IFDEF DELPHI10SEATTLE}
  if AMonitor <> nil then
    Result := AMonitor.PixelsPerInch
{$ELSE}
  if (AMonitor <> nil) and Assigned(FGetDpiForMonitor) and (FGetDpiForMonitor(AMonitor.Handle, 0, X, Y) = S_OK) then
    Result := X
{$ENDIF}
  else
    Result := dxGetSystemDPI;
end;

function dxGetMonitorDPI(const AScreenPoint: TPoint): Integer;
begin
  Result := dxGetMonitorDPI(Screen.MonitorFromPoint(AScreenPoint));
end;

function dxGetTargetDPI(AComponent: TComponent): Integer;
begin
  if IsWin8OrLater then
  begin
    while AComponent <> nil do
    begin
      if AComponent is TControl then
        Exit(dxGetMonitorDPI(TControl(AComponent).ClientToScreen(cxRectCenter(TControl(AComponent).ClientRect))));
      AComponent := AComponent.Owner;
    end;
  end;
  Result := dxGetSystemDPI;
end;

function dxGetScaleFactor(AOwner: TObject): TdxScaleFactor;
var
  AScaleFactor: IdxScaleFactor;
begin
  if Supports(AOwner, IdxScaleFactor, AScaleFactor) then
    Result := AScaleFactor.Value
  else
    Result := dxSystemScaleFactor;
end;

function dxGetScaleFactorForCanvas(const ACanvas: TCanvas): TdxScaleFactor;
var
  AScaleFactor: TdxScaleFactor;
begin
  if ACanvas is TControlCanvas then
  begin
    if dxTryGetScaleFactorForControl(TControlCanvas(ACanvas).Control, AScaleFactor) then
      Exit(AScaleFactor);
  end;
  Result := dxDefaultScaleFactor;
end;

function dxGetScaleFactorForInterface(const AOwner: IUnknown): TdxScaleFactor;
var
  AScaleFactor: IdxScaleFactor;
begin
  if Supports(AOwner, IdxScaleFactor, AScaleFactor) then
    Result := AScaleFactor.Value
  else
    Result := dxSystemScaleFactor;
end;

function dxGetSystemMetrics(AIndex: Integer; AScaleFactor: TdxScaleFactor = nil): Integer;
begin
  if AScaleFactor = nil then
    Result := GetSystemMetrics(AIndex)
  else
    if Assigned(FGetSystemMetricsForDpi) then
      Result := FGetSystemMetricsForDpi(AIndex, AScaleFactor.Apply(dxDefaultDPI))
    else
      Result := AScaleFactor.Apply(GetSystemMetrics(AIndex), dxDesktopScaleFactor);
end;

function dxTryGetScaleFactorForControl(AControl: TControl; out AScaleFactor: TdxScaleFactor): Boolean;
var
  AIntf: IdxScaleFactor;
begin
  while AControl <> nil do
  begin
    if Supports(AControl, IdxScaleFactor, AIntf) then
    begin
      AScaleFactor := AIntf.Value;
      Exit(True);
    end;
    AControl := AControl.Parent;
  end;
  Result := False;
end;

function dxDefaultScaleFactor: TdxScaleFactor;
begin
  if (FDefaultScaleFactor = nil) and not FUnitIsFinalized then
    FDefaultScaleFactor := TdxScaleFactor.Create;
  Result := FDefaultScaleFactor;
end;

function dxDesktopScaleFactor: TdxScaleFactor;
begin
  if (FDesktopScaleFactor = nil) and not FUnitIsFinalized then
    FDesktopScaleFactor := TdxDesktopScaleFactor.Create;
  Result := FDesktopScaleFactor;
end;

function dxSystemScaleFactor: TdxScaleFactor;
begin
  if (FSystemScaleFactor = nil) and not FUnitIsFinalized then
    FSystemScaleFactor := TdxSystemScaleFactor.Create;
  Result := FSystemScaleFactor;
end;

function dxGetProcessDpiAwareness: TdxProcessDpiAwareness;
const
  DPI_AWARENESS_CONTEXT_UNAWARE = 16;
  DPI_AWARENESS_CONTEXT_SYSTEM_AWARE = 17;
  DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE = 18;
  DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2 = 34;
begin
  if Assigned(FGetThreadDpiAwarenessContext) then
    case FGetThreadDpiAwarenessContext of
      DPI_AWARENESS_CONTEXT_UNAWARE:
        Exit(pdaUnaware);
      DPI_AWARENESS_CONTEXT_SYSTEM_AWARE:
        Exit(pdaSystemDpiAware);
      DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE:
        Exit(pdaPerMonitorDpiAware);
      DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2:
        Exit(pdaPerMonitorDpiAwareV2);
    end;

  if not dxGetProcessDpiAwareness(GetCurrentProcess, Result) then
  begin
    if dxIsProcessDPIAware then
      Result := pdaSystemDpiAware
    else
      Result := pdaUnaware;
  end;
end;

function dxGetProcessDpiAwareness(AProcess: THandle): TdxProcessDpiAwareness;
begin
  if not dxGetProcessDpiAwareness(AProcess, Result) then
    Result := pdaUnaware;
end;

function dxGetProcessDpiAwareness(AProcess: THandle; out AValue: TdxProcessDpiAwareness): Boolean;
var
  AProcessDpiAwareness: TProcessDpiAwareness;
begin
  Result := Assigned(FGetProcessDpiAwareness) and Succeeded(FGetProcessDpiAwareness(AProcess, AProcessDpiAwareness));
  if Result then
    AValue := TdxProcessDpiAwareness(AProcessDpiAwareness);
end;

function dxIsProcessDPIAware: Boolean;
begin
{$WARNINGS OFF}
  Result := IsWinVistaOrLater and IsProcessDPIAware;
{$WARNINGS ON}
end;

function dxIsProcessPerMonitorV2Aware: Boolean;
begin
  Result := IsWin10OrLater and (dxGetProcessDpiAwareness = pdaPerMonitorDpiAwareV2);
end;

function dxSetProcessDpiAwareness(AValue: TdxProcessDpiAwareness): Boolean;
const
  ThreadDpiAwarenessContextMap: array[TdxProcessDpiAwareness] of TDpiAwarenessContext = (
    TDpiAwarenessContext(-1),
    TDpiAwarenessContext(-2),
    TDpiAwarenessContext(-3),
    TDpiAwarenessContext(-4)
  );
begin
  if Assigned(FSetThreadDpiAwarenessContext) then
    Result := FSetThreadDpiAwarenessContext(ThreadDpiAwarenessContextMap[AValue]) <> DPI_AWARENESS_CONTEXT_INVALID
  else
    if Assigned(FSetProcessDpiAwareness) then
      Result := (AValue <= pdaPerMonitorDpiAware) and Succeeded(FSetProcessDpiAwareness(TProcessDpiAwareness(AValue)))
    else
      Result := IsWinVistaOrLater and (AValue <> pdaUnaware) and SetProcessDPIAware;

  if Result then
  begin
    if FDesktopScaleFactor <> nil then
      FDesktopScaleFactor.Update;
    if FSystemScaleFactor <> nil then
      FSystemScaleFactor.Update;
  end;
end;

{ TdxSystemBasedScaleFactor }

procedure TdxSystemBasedScaleFactor.AfterConstruction;
begin
  inherited;
  Update;
end;

{ TdxDesktopScaleFactor }

procedure TdxDesktopScaleFactor.Update;
begin
  if dxIsProcessDPIAware then
    Assign(dxGetDesktopDPI, dxDefaultDPI)
  else
    Assign(dxGetSystemDPI, dxDefaultDPI);
end;

{ TdxSystemScaleFactor }

procedure TdxSystemScaleFactor.Update;
begin
  Assign(dxGetSystemDPI, dxDefaultDPI);
end;

{ TdxScaleFactorHelper }

function TdxScaleFactorHelper.TargetDPI: Integer;
begin
  Result := Apply(dxDefaultDPI);
end;

procedure InitializeUnit;
var
  AUserLibHandle: THandle;
begin
  FShellCoreLibHandle := LoadLibrary(Shcore);
  if FShellCoreLibHandle > 32 then
  begin
    @FGetDpiForMonitor := GetProcAddress(FShellCoreLibHandle, 'GetDpiForMonitor');
    @FGetProcessDpiAwareness := GetProcAddress(FShellCoreLibHandle, 'GetProcessDpiAwareness');
    @FSetProcessDpiAwareness := GetProcAddress(FShellCoreLibHandle, 'SetProcessDpiAwareness');
  end;

  AUserLibHandle := GetModuleHandle(user32);
  if AUserLibHandle > 32 then
  begin
    @FGetSystemMetricsForDpi := GetProcAddress(AUserLibHandle, 'GetSystemMetricsForDpi');
    @FGetThreadDpiAwarenessContext := GetProcAddress(AUserLibHandle, 'GetThreadDpiAwarenessContext');
    @FSetThreadDpiAwarenessContext := GetProcAddress(AUserLibHandle, 'SetThreadDpiAwarenessContext');
    @FAdjustWindowRectExForDpi := GetProcAddress(AUserLibHandle, 'AdjustWindowRectExForDpi');
  end;
end;

procedure FinalizeUnit;
begin
  FUnitIsFinalized := True;
  @FGetDpiForMonitor := nil;
  @FGetProcessDpiAwareness := nil;
  @FGetSystemMetricsForDpi := nil;
  @FGetThreadDpiAwarenessContext := nil;
  @FSetProcessDpiAwareness := nil;
  @FSetThreadDpiAwarenessContext := nil;
  @FAdjustWindowRectExForDpi := nil;
  if FShellCoreLibHandle > 32 then
  begin
    FreeLibrary(FShellCoreLibHandle);
    FShellCoreLibHandle := 0;
  end;
end;

initialization
  dxUnitsLoader.AddUnit(@InitializeUnit, @FinalizeUnit);

finalization
  dxUnitsLoader.RemoveUnit(@FinalizeUnit);
  FreeAndNil(FDesktopScaleFactor);
  FreeAndNil(FDefaultScaleFactor);
  FreeAndNil(FSystemScaleFactor);
end.
