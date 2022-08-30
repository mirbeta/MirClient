{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCommonLibrary                                     }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCOMMONLIBRARY AND ALL          }
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

unit dxHooks;

{$I cxVer.inc}
{$D-}

interface

uses
  Types, Windows, SysUtils, Classes, Contnrs, Forms, dxCore;

type
  TdxHookType = (htKeyboard, htMouse, htWndProc, htGetMessage, htCBT, htWndProcRet);
  TdxHookProc = procedure (ACode: Integer; AWParam: WPARAM; ALParam: LPARAM; var AHookResult: LRESULT);

procedure dxSetHook(AHookType: TdxHookType; AHookProc: TdxHookProc);
procedure dxReleaseHook(AHookProc: TdxHookProc);

implementation

type
  TdxSystemHook = class
  private
    FHandle: HHOOK;
    FHookType: TdxHookType;
    FHooks: TList;
    procedure SetSystemHook;
    procedure ReleaseSystemHook;
    function ProcessHookProcs(ACode: Integer; AWParam: WPARAM; ALParam: LPARAM): LRESULT;
  protected
    function HasHookProc(AHookProc: TdxHookProc): Boolean;
    procedure SetHookProc(AHookProc: TdxHookProc);
    procedure ReleaseHookProc(AHookProc: TdxHookProc);
  public
    constructor Create(AHookType: TdxHookType);
    destructor Destroy; override;
  end;

  TdxSystemHooks = array[TdxHookType] of TdxSystemHook;

  TdxHookController = class
  private
    FSystemHooks: TdxSystemHooks;
    procedure CreateSystemHooks;
    procedure DestroySystemHooks;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetHook(AHookType: TdxHookType; AHookProc: TdxHookProc);
    procedure ReleaseHook(AHookProc: TdxHookProc);
  end;

var
  FdxHookController: TdxHookController;

function dxHookController: TdxHookController;
begin
  if FdxHookController = nil then
    FdxHookController := TdxHookController.Create;
  Result := FdxHookController;
end;

function dxSystemKeyboardHook(Code: Integer; wParam: WParam; lParam: LParam): LRESULT; stdcall;
begin
  Result := dxHookController.FSystemHooks[htKeyboard].ProcessHookProcs(Code, wParam, lParam);
end;

function dxSystemMouseHook(Code: Integer; wParam: WParam; lParam: LParam): LRESULT; stdcall;
begin
  Result := dxHookController.FSystemHooks[htMouse].ProcessHookProcs(Code, wParam, lParam);
end;

function dxSystemWndProcHook(Code: Integer; wParam: WParam; lParam: LParam): LRESULT; stdcall;
begin
  Result := dxHookController.FSystemHooks[htWndProc].ProcessHookProcs(Code, wParam, lParam);
end;

function dxSystemWndProcRetHook(Code: Integer; wParam: WParam; lParam: LParam): LRESULT; stdcall;
begin
  Result := dxHookController.FSystemHooks[htWndProcRet].ProcessHookProcs(Code, wParam, lParam);
end;

function dxSystemGetMessageHook(Code: Integer; wParam: WParam; lParam: LParam): LRESULT; stdcall;
begin
  Result := dxHookController.FSystemHooks[htGetMessage].ProcessHookProcs(Code, wParam, lParam);
end;

function dxSystemCBTHook(Code: Integer; wParam: WParam; lParam: LParam): LRESULT; stdcall;
begin
  Result := dxHookController.FSystemHooks[htCBT].ProcessHookProcs(Code, wParam, lParam);
end;

type
  TdxSystemHookParams = record
    Proc: TFNHookProc;
    ID: Byte;
  end;

  TdxSystemHooksData = array[TdxHookType] of TdxSystemHookParams;

const
  HooksData: TdxSystemHooksData = (
    (Proc: dxSystemKeyboardHook;   ID: WH_KEYBOARD),      // htKeyboard
    (Proc: dxSystemMouseHook;      ID: WH_MOUSE),         // htMouse
    (Proc: dxSystemWndProcHook;    ID: WH_CALLWNDPROC),   // htWndProc
    (Proc: dxSystemGetMessageHook; ID: WH_GETMESSAGE),    // htGetMessage
    (Proc: dxSystemCBTHook;        ID: WH_CBT),           // htWCBT
    (Proc: dxSystemWndProcRetHook; ID: WH_CALLWNDPROCRET) // htWndProcRet
  );

{ TdxSystemHook }

constructor TdxSystemHook.Create(AHookType: TdxHookType);
begin
  inherited Create;
  FHookType := AHookType;
  FHooks := TList.Create;
end;

destructor TdxSystemHook.Destroy;
begin
  Assert((FHooks.Count = 0) and (FHandle = 0));
  FreeAndNil(FHooks);
  inherited Destroy;
end;

function TdxSystemHook.HasHookProc(AHookProc: TdxHookProc): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FHooks.Count - 1 do
    if FHooks[I] = @AHookProc then
    begin
      Result := True;
      Break;
    end;
end;

procedure TdxSystemHook.SetHookProc(AHookProc: TdxHookProc);
begin
  Assert(not HasHookProc(AHookProc));
  if FHooks.Count = 0 then
    SetSystemHook;
  FHooks.Add(@AHookProc);
end;

procedure TdxSystemHook.ReleaseHookProc(AHookProc: TdxHookProc);
begin
  if (FHooks.Remove(@AHookProc) >= 0) and (FHooks.Count = 0) then
    ReleaseSystemHook;
end;

procedure TdxSystemHook.SetSystemHook;
begin
  Assert(FHandle = 0);
  FHandle := SetWindowsHookEx(HooksData[FHookType].ID, HooksData[FHookType].Proc, 0, GetCurrentThreadId);
end;

procedure TdxSystemHook.ReleaseSystemHook;
begin
  Assert(FHandle <> 0);
  UnhookWindowsHookEx(FHandle);
  FHandle := 0;
end;

function TdxSystemHook.ProcessHookProcs(ACode: Integer; AWParam: WPARAM; ALParam: LPARAM): LRESULT;
var
  I, AHookResult: LRESULT;
begin
  AHookResult := 0;
  for I := FHooks.Count - 1 downto 0 do
    try
      TdxHookProc(FHooks.List[I])(ACode, AWParam, ALParam, AHookResult);
    except
      Application.HandleException(Application);
    end;

  Result := CallNextHookEx(FHandle, ACode, AWParam, ALParam);
  if AHookResult <> 0 then
    Result := AHookResult;
end;

{ TdxHookController }

constructor TdxHookController.Create;
begin
  inherited Create;
  CreateSystemHooks;
end;

destructor TdxHookController.Destroy;
begin
  DestroySystemHooks;
  inherited Destroy;
end;

procedure TdxHookController.SetHook(AHookType: TdxHookType; AHookProc: TdxHookProc);
begin
  FSystemHooks[AHookType].SetHookProc(AHookProc);
end;

procedure TdxHookController.ReleaseHook(AHookProc: TdxHookProc);
var
  I: TdxHookType;
begin
  for I := Low(TdxHookType) to High(TdxHookType) do
    FSystemHooks[I].ReleaseHookProc(AHookProc);
end;

procedure TdxHookController.CreateSystemHooks;
var
  I: TdxHookType;
begin
  for I := Low(TdxHookType) to High(TdxHookType) do
    FSystemHooks[I] := TdxSystemHook.Create(I);
end;

procedure TdxHookController.DestroySystemHooks;
var
  I: TdxHookType;
begin
  for I := Low(TdxHookType) to High(TdxHookType) do
    FreeAndNil(FSystemHooks[I]);
end;

procedure dxSetHook(AHookType: TdxHookType; AHookProc: TdxHookProc);
begin
  dxHookController.SetHook(AHookType, AHookProc);
end;

procedure dxReleaseHook(AHookProc: TdxHookProc);
begin
  dxHookController.ReleaseHook(AHookProc);
end;

initialization

finalization
  FreeAndNil(FdxHookController);

end.
