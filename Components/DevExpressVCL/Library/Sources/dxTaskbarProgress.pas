{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit dxTaskbarProgress;

{$I cxVer.inc}

interface

uses
  Types, Windows, Classes, Forms, SysUtils, ShlObj, Messages,
  CommCtrl, ComObj, ActiveX, AppEvnts, dxCore, cxClasses, cxProgressBar;


type
  TdxTaskbarProgressState = (tbpsNoProgress, tbpsIndeterminate, tbpsNormal, tbpsError, tbpsPaused);

  TdxTaskbarCustomProgress = class(TcxCustomComponent)
  private
    FActive: Boolean;
    FLinkedComponent: TComponent;
    FIsLinkedComponentChangeLocked: Boolean;
    FListener: TcxProgressBarListener;
    FLoadedActive: Boolean;
    FLoadedLinkedComponent: TComponent;
    FPosition: Int64;
    FState: TdxTaskbarProgressState;
    FTotal: Int64;
    function IsTotalStored: Boolean;
    function GetProgressBar: TcxCustomProgressBar;
    function GetProgressBarProperties: TcxCustomProgressBarProperties;
    procedure SetActive(AValue: Boolean);
    procedure SetLinkedComponent(AValue: TComponent);
    procedure SetPosition(AValue: Int64);
    procedure SetState(AValue: TdxTaskbarProgressState);
    procedure SetTotal(AValue: Int64);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CheckUnique;
    procedure CreateListener;
    procedure DoLinkedComponentChanged(ASender: TObject);
    function IsActive: Boolean;
    function IsComponentLinked: Boolean;
    procedure Refresh;
    procedure RefreshPosition;
    procedure RefreshState;
    procedure RemoveListener;
    procedure Reset;
    procedure SynchronizeAndRefresh;
    procedure SynchronizeLinkedComponent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Active: Boolean read FActive write SetActive default False;
    property LinkedComponent: TComponent read FLinkedComponent write SetLinkedComponent;
    property Position: Int64 read FPosition write SetPosition;
    property State: TdxTaskbarProgressState read FState write SetState default tbpsNormal;
    property Total: Int64 read FTotal write SetTotal stored IsTotalStored;
  end;

  TdxTaskbarProgress = class(TdxTaskbarCustomProgress)
  published
    property Active;
    property LinkedComponent;
    property Position;
    property State;
    property Total;
  end;

implementation

uses
  Math;

const
  TaskbarProgressState: array [TdxTaskbarProgressState] of Integer = (TBPF_NOPROGRESS, TBPF_INDETERMINATE,
    TBPF_NORMAL, TBPF_ERROR, TBPF_PAUSED);
  dxTaskButtonCreatedMessage = 'TaskbarButtonCreated';

type
  TdxChangeFilterStruct = record
    cbSize: DWORD;
    ExtStatus: DWORD;
  end;
  PdxChangeFilterStruct = ^TdxChangeFilterStruct;

var
  FTaskbar: ITaskbarList;
  FTaskbarProgress: ITaskbarList3;
  dxTaskButtonCreatedMessageID: Cardinal;
  User32lib: NativeUInt;
  dxChangeWindowMessageFilterEx: function (hwnd: HWND; message: UINT;
    action: DWORD; pChangeFilterStruct: PdxChangeFilterStruct): BOOL; stdcall;

function GetMainWindow: HWND;
begin
  if Application.MainFormOnTaskbar then
  begin
    if Assigned(Application.MainForm) and Application.MainForm.HandleAllocated then
      Result := Application.MainForm.Handle
    else
      Result := 0;
  end
  else
    Result := Application.Handle;
end;

procedure UnlockTaskButtonCreatedMessage(AWnd: HWND);
const
  MSGFLT_ALLOW = 1;
begin
  if @dxChangeWindowMessageFilterEx = nil then
    Exit;

  if not dxChangeWindowMessageFilterEx(AWnd, dxTaskButtonCreatedMessageID, MSGFLT_ALLOW, nil) then
    ;// raise EdxException.Create('ChangeWindowMessageFilterEx: ' + SysErrorMessage(GetLastError))
end;

type
  TdxTaskbarProgressManager = class
  private
    FAppEvents: TApplicationEvents;
    FMainWindow: HWND;
    FIsTaskbarButtonCreated: Boolean;
    FItems: TcxObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function AppHook(var Message: TMessage): Boolean;
    procedure CheckUnique(AItem: TdxTaskbarCustomProgress);
    procedure DoAppMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure Initialize;
    procedure RefreshItems;
    procedure RegisterItem(AItem: TdxTaskbarCustomProgress);
    procedure UnRegisterItem(AItem: TdxTaskbarCustomProgress);
  end;

{ TdxTaskbarProgressManager }

constructor TdxTaskbarProgressManager.Create;
begin
  inherited Create;
  FItems := TcxObjectList.Create(False);
end;

destructor TdxTaskbarProgressManager.Destroy;
begin
  if (FTaskbarProgress <> nil) and (Application <> nil) then
    Application.UnhookMainWindow(AppHook);
  FreeAndNil(FAppEvents);
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxTaskbarProgressManager.AppHook(var Message: TMessage): Boolean;
begin
  Result := False;
  if (Application.MainForm <> nil) and (Application.MainForm.HandleAllocated) and
    (GetMainWindow <> FMainWindow) then
  begin
    FMainWindow := GetMainWindow;
    UnlockTaskButtonCreatedMessage(FMainWindow);
    FIsTaskbarButtonCreated := False;
    FAppEvents.OnMessage := DoAppMessage;
  end;
end;

procedure TdxTaskbarProgressManager.CheckUnique(
  AItem: TdxTaskbarCustomProgress);
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    if TdxTaskbarCustomProgress(FItems[I]).Active and
      (FItems[I] <> AItem) then
      TdxTaskbarCustomProgress(FItems[I]).Active := False;
end;

procedure TdxTaskbarProgressManager.DoAppMessage(var Msg: tagMSG;
  var Handled: Boolean);
begin
  if (Msg.message = dxTaskButtonCreatedMessageID) and
    (Msg.hwnd = GetMainWindow) then
  begin
    FAppEvents.OnMessage := nil;
    FIsTaskbarButtonCreated := True;
    RefreshItems;
  end;
end;

procedure TdxTaskbarProgressManager.Initialize;
begin
  FAppEvents := TApplicationEvents.Create(nil);
  Application.HookMainWindow(AppHook);
end;

procedure TdxTaskbarProgressManager.RefreshItems;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    if TdxTaskbarCustomProgress(FItems[I]).Active then
      TdxTaskbarCustomProgress(FItems[I]).Refresh;
end;

procedure TdxTaskbarProgressManager.RegisterItem(
  AItem: TdxTaskbarCustomProgress);
begin
  FItems.Add(AItem);
end;

procedure TdxTaskbarProgressManager.UnRegisterItem(
  AItem: TdxTaskbarCustomProgress);
begin
  FItems.Remove(AItem);
end;

var
  FTaskbarProgressManager: TdxTaskbarProgressManager;

function TaskbarProgressManager: TdxTaskbarProgressManager;
begin
  if FTaskbarProgressManager = nil then
    FTaskbarProgressManager := TdxTaskbarProgressManager.Create;
  Result := FTaskbarProgressManager;
end;

procedure UpdateTaskbarProgressState(AFlags: Integer);
begin
  if TaskbarProgressManager.FIsTaskbarButtonCreated then
    FTaskbarProgress.SetProgressState(GetMainWindow, AFlags);
end;

procedure UpdateTaskbarProgressPosition(ACompleted, ATotal: UInt64);
begin
  if TaskbarProgressManager.FIsTaskbarButtonCreated then
    FTaskbarProgress.SetProgressValue(GetMainWindow, ACompleted, ATotal);
end;

{ TdxTaskbarCustomProgress }

constructor TdxTaskbarCustomProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTotal := 100;
  FState := tbpsNormal;
  TaskbarProgressManager.RegisterItem(Self);
end;

destructor TdxTaskbarCustomProgress.Destroy;
begin
  TaskbarProgressManager.UnRegisterItem(Self);
  LinkedComponent := nil;
  inherited Destroy;
end;

procedure TdxTaskbarCustomProgress.Loaded;
begin
  inherited;
  LinkedComponent := FLoadedLinkedComponent;
  Active := FLoadedActive;
end;

procedure TdxTaskbarCustomProgress.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = LinkedComponent) then
    LinkedComponent := nil;
end;

procedure TdxTaskbarCustomProgress.CheckUnique;
begin
  if csDesigning in ComponentState then Exit;
  TaskbarProgressManager.CheckUnique(Self);
end;

procedure TdxTaskbarCustomProgress.CreateListener;
begin
  FListener := TcxProgressBarListener.Create(GetProgressBar);
  FListener.OnChanged := DoLinkedComponentChanged;
  GetProgressBar.RegisterListener(FListener);
end;

procedure TdxTaskbarCustomProgress.DoLinkedComponentChanged(ASender: TObject);
begin
  if FIsLinkedComponentChangeLocked then Exit;
  FPosition := Round(GetProgressBar.Position - GetProgressBarProperties.Min);
  if FState <> tbpsNoProgress then
    if GetProgressBarProperties.Marquee then
      FState := tbpsIndeterminate
    else
      if FState = tbpsIndeterminate then
        FState := tbpsNormal;
  FTotal := Round(GetProgressBarProperties.Max - GetProgressBarProperties.Min);
  Refresh;
end;

function TdxTaskbarCustomProgress.IsActive: Boolean;
begin
  Result := not (csDesigning in ComponentState) and FActive;
end;

function TdxTaskbarCustomProgress.IsComponentLinked: Boolean;
begin
  Result := LinkedComponent <> nil;
end;

procedure TdxTaskbarCustomProgress.SynchronizeAndRefresh;
begin
  if IsComponentLinked then
    SynchronizeLinkedComponent;
  Refresh;
end;

procedure TdxTaskbarCustomProgress.SynchronizeLinkedComponent;
begin
  FIsLinkedComponentChangeLocked := True;
  try
    if FState <> tbpsNoProgress then
      GetProgressBarProperties.Marquee := FState = tbpsIndeterminate;
    GetProgressBarProperties.Max := GetProgressBarProperties.Min + FTotal;
    GetProgressBar.EditValue := FPosition + GetProgressBarProperties.Min;
  finally
    FIsLinkedComponentChangeLocked := False;
  end;
end;

procedure TdxTaskbarCustomProgress.Refresh;
begin
  RefreshState;
  RefreshPosition;
end;

procedure TdxTaskbarCustomProgress.RefreshPosition;
begin
  if IsActive and not (State in [tbpsIndeterminate, tbpsNoProgress]) then
    UpdateTaskbarProgressPosition(FPosition, FTotal);
end;

procedure TdxTaskbarCustomProgress.RefreshState;
begin
  if IsActive then
    UpdateTaskbarProgressState(TaskbarProgressState[FState]);
end;

procedure TdxTaskbarCustomProgress.RemoveListener;
begin
  GetProgressBar.UnRegisterListener(FListener);
  FreeAndNil(FListener);
end;

procedure TdxTaskbarCustomProgress.Reset;
begin
  UpdateTaskbarProgressState(TaskbarProgressState[tbpsNoProgress]);
end;

function TdxTaskbarCustomProgress.IsTotalStored: Boolean;
begin
  Result := FTotal <> 100;
end;

function TdxTaskbarCustomProgress.GetProgressBar: TcxCustomProgressBar;
begin
  Result := LinkedComponent as TcxCustomProgressBar;
end;

function TdxTaskbarCustomProgress.GetProgressBarProperties: TcxCustomProgressBarProperties;
begin
  Result := GetProgressBar.ActiveProperties;
end;

procedure TdxTaskbarCustomProgress.SetActive(AValue: Boolean);
begin
  if csLoading in ComponentState then
    FLoadedActive := AValue
  else
    if AValue <> Active then
      if AValue then
      begin
        FActive := True;
        CheckUnique;
        Refresh;
      end
      else
      begin
        Reset;
        FActive := False;
      end;
end;

procedure TdxTaskbarCustomProgress.SetLinkedComponent(AValue: TComponent);
begin
  if csLoading in ComponentState then
    FLoadedLinkedComponent := AValue
  else
    if (AValue <> nil) and not (AValue is TcxCustomProgressBar) then
      AValue := nil;
    if FLinkedComponent <> AValue then
    begin
      if FLinkedComponent <> nil then
      begin
        RemoveListener;
        FLinkedComponent.RemoveFreeNotification(Self);
      end;
      FLinkedComponent := AValue;
      if FLinkedComponent <> nil then
      begin
        FLinkedComponent.FreeNotification(Self);
        CreateListener;
      end;
      if IsComponentLinked then
        DoLinkedComponentChanged(FLinkedComponent);
    end;
end;

procedure TdxTaskbarCustomProgress.SetPosition(AValue: Int64);
begin
  if FPosition <> AValue then
  begin
    FPosition := Max(AValue, 0);
    SynchronizeAndRefresh;
  end;
end;

procedure TdxTaskbarCustomProgress.SetState(
  AValue: TdxTaskbarProgressState);
begin
  if FState <> AValue then
  begin
    FState := AValue;
    SynchronizeAndRefresh;
  end;
end;

procedure TdxTaskbarCustomProgress.SetTotal(AValue: Int64);
begin
  if FTotal <> AValue then
  begin
    FTotal := Max(AValue, 0);
    SynchronizeAndRefresh;
  end;
end;

procedure RegisterAssistants;
begin
  CoInitialize(nil);
  FTaskbar := CreateComObject(CLSID_TaskbarList) as ITaskbarList;
  if FTaskbar.QueryInterface(IID_ITaskbarList3, FTaskbarProgress) <> S_OK then
    FTaskbarProgress := nil
  else
  begin
    dxTaskButtonCreatedMessageID := RegisterWindowMessage(dxTaskButtonCreatedMessage);
    TaskbarProgressManager.Initialize;
  end;
  User32lib := GetModuleHandle(user32);
  if User32lib <> 0 then
    dxChangeWindowMessageFilterEx := GetProcAddress(User32lib, 'ChangeWindowMessageFilterEx');
end;

procedure UnregisterAssistants;
begin
  FreeAndNil(FTaskbarProgressManager);
  FTaskbarProgress := nil;
  FTaskbar := nil;
  CoUninitialize;
end;

initialization
  dxUnitsLoader.AddUnit(@RegisterAssistants, @UnregisterAssistants);

finalization
  dxUnitsLoader.RemoveUnit(@UnregisterAssistants);

end.
