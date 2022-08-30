{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSPopupMan;

interface

{$I cxVer.inc}

uses
  Classes, Controls, Windows, Menus,
  dxCoreClasses, cxGeometry, cxClasses, dxHooks, dxBase, dxPSSngltn;

type
  TdxPSPopupMenuBuilderClass = class of TAbstractdxPSPopupMenuBuilder;

  { TAbstractdxPSPopupMenuBuilder }

  TAbstractdxPSPopupMenuBuilder = class(TObject)
  protected
    function BuildPopup(const AControl: TControl;
      const APopupMenu: TPopupMenu): TComponent; virtual; abstract;
    class function CanShowPopup(const APopupMenu: TPopupMenu): Boolean; virtual;
    procedure FreePopup(var APopupMenu: TComponent); virtual; abstract;
    procedure InvokePopup(X, Y: Integer; AControl: TControl; APopupMenu: TComponent); virtual; abstract;
    class function RequireProcessDoPopup: Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  { TdxStandardPSPopupMenuBuilder }

  TdxStandardPSPopupMenuBuilder = class(TAbstractdxPSPopupMenuBuilder)
  protected
    function BuildPopup(const AControl: TControl;
      const APopupMenu: TPopupMenu): TComponent; override;
    procedure FreePopup(var APopupMenu: TComponent); override;
    procedure InvokePopup(X, Y: Integer; AControl: TControl; APopupMenu: TComponent); override;
  end;

  { TdxPSPopupMenuController }

  TdxPSPopupMenuController = class(TBasedxPSSingleton)
  private
    FBuilders: TdxClassList;
    FControls: TList;
    FNexus: TcxFreeNotificator;
    function GetActiveBuilder: TdxPSPopupMenuBuilderClass;
    function GetBuilder(Index: Integer): TdxPSPopupMenuBuilderClass;
    function GetBuilderCount: Integer;
    function GetControl(Index: Integer): TControl;
    function GetControlCount: Integer;
    procedure FreeAndNilControls;
  protected
    procedure FinalizeInstance; override;
    procedure InitializeInstance; override;

    function IndexOfControl(AControl: TControl): Integer;
    function TryShowPopup(AControl: TControl; Pt: TPoint): Boolean;
    procedure FreeNotification(Sender: TComponent);
  public
    class function Instance: TdxPSPopupMenuController; reintroduce; overload;

    procedure ShowPopup(const X, Y: Integer; const AControl: TControl; const APopupMenu: TPopupMenu);
    procedure ShowPopupAtMousePos(const AControl: TControl; const APopupMenu: TPopupMenu);

    procedure RegisterBuilder(ABuilder: TdxPSPopupMenuBuilderClass);
    procedure UnregisterBuilder(ABuilder: TdxPSPopupMenuBuilderClass);

    procedure RegisterControl(AControl: TControl);
    procedure UnregisterControl(AControl: TControl);

    property ActiveBuilder: TdxPSPopupMenuBuilderClass read GetActiveBuilder;
    property BuilderCount: Integer read GetBuilderCount;
    property Builders[Index: Integer]: TdxPSPopupMenuBuilderClass read GetBuilder;
    property ControlCount: Integer read GetControlCount;
    property Controls[Index: Integer]: TControl read GetControl;
  end;

function dxPSPopupMenuController: TdxPSPopupMenuController;

implementation

uses
  Types, SysUtils, Messages, Forms, dxPSGlbl, dxPSUtl, cxControls;

function dxPSPopupMenuController: TdxPSPopupMenuController;
begin
  Result := TdxPSPopupMenuController.Instance;
end;

procedure EatWMContextMenu(Wnd: HWND);
var
  Msg: TMsg;
begin
  PeekMessage(Msg, Wnd, WM_CONTEXTMENU, WM_CONTEXTMENU, PM_REMOVE);
end;

procedure dxPSPopupManKeyboardHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);

  function IsProcessKey(AKey, AKeyData: Longint; AKeyPressed: Boolean): Boolean;
  begin
    Result :=
      ((AKey = VK_F10) and (GetAsyncKeyState(VK_SHIFT) < 0)) or
      ((AKey = VK_APPS) and ((AKeyData shr 29) and 1 <> 1));
  end;

 function GetVCLControl(Wnd: HWND): TWinControl;
 begin
   repeat
     Result := FindControl(Wnd);
     if Result <> nil then Exit;
     Wnd := GetParent(Wnd);
   until Wnd = 0;
 end;

const
  KbdMessages: array[Boolean] of Cardinal = (WM_KEYUP, WM_KEYDOWN);
var
  KeyPressed: Boolean;
  Wnd: HWND;
  Control: TWinControl;
begin
  KeyPressed := (lParam shr 31) and 1 <> 1;
  if (ACode >= 0) and IsProcessKey(wParam, lParam, KeyPressed) then
  begin
    Wnd := GetFocus;
    Control := GetVCLControl(Wnd);
    if Control <> nil then
    begin
      if dxPSPopupMenuController.IndexOfControl(Control) > -1 then
      begin
        EatWMContextMenu(Wnd);
        dxPSPopupMenuController.TryShowPopup(Control, Control.ClientToScreen(cxNullPoint));
        AHookResult := 1;
      end;
    end;
  end;
end;

procedure dxPSPopupManMouseHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);

  function FindControl(Wnd: HWND; const Pt: TPoint): TControl;

    function IsRegisteredControl(AControl: TControl; const Pt: TPoint): Boolean;
    begin
      Result := PtInRect(AControl.ClientRect, AControl.ScreenToClient(Pt)) and
        (dxPSPopupMenuController.IndexOfControl(AControl) > -1);
    end;

    function FindInChildren(AControl: TWinControl; const Pt: TPoint): TControl;
    var
      I: Integer;
    begin
      for I := 0 to AControl.ControlCount - 1 do
      begin
        Result := AControl.Controls[I];
        if IsRegisteredControl(Result, Pt) then Exit;
      end;
      Result := nil;
    end;

    function FindInParents(AControl: TWinControl; const Pt: TPoint): TWinControl;
    begin
      Result := AControl;
      while Result <> nil do
      begin
        if IsRegisteredControl(Result, Pt) then Exit;
        Result := Result.Parent;
      end;
    end;

  var
    TargetControl: TWinControl;
  begin
    Result := nil;
    TargetControl := Controls.FindControl(Wnd);
    if TargetControl = nil then Exit;
    Result := FindInChildren(TargetControl, Pt);
    if Result = nil then
    begin
      Result := TargetControl;
      if dxPSPopupMenuController.IndexOfControl(Result) > -1 then Exit;
      Result := FindInParents(TargetControl, Pt);
    end;
  end;

var
  MouseHookStruct: TMouseHookStruct;
  Control: TControl;
begin
  if (ACode >= 0) and ((wParam = WM_RBUTTONUP) or (wParam = WM_RBUTTONDOWN)) then
  begin
    MouseHookStruct := PMouseHookStruct(lParam)^;
    with MouseHookStruct do
    begin
      Control := FindControl(HWND, Pt);
      if Control <> nil then
      begin
        if wParam = WM_RBUTTONUP then
          dxPSPopupMenuController.TryShowPopup(Control, Pt);
        AHookResult := 1;
      end;
    end;
  end;
end;

{ TdxPSPopupMenuController }

class function TdxPSPopupMenuController.Instance: TdxPSPopupMenuController;
begin
  Result := inherited Instance as TdxPSPopupMenuController;
end;

procedure TdxPSPopupMenuController.ShowPopup(
  const X, Y: Integer; const AControl: TControl; const APopupMenu: TPopupMenu);
var
  APoint: TPoint;
  APopupMenuBuilder: TAbstractdxPSPopupMenuBuilder;
  APopupMenuComponent: TComponent;
begin
  if Assigned(APopupMenu) then
  begin
    if Assigned(AControl) then
      dxPSUtl.Control_SendCancelMode(AControl, nil);
    if ActiveBuilder.RequireProcessDoPopup then
      dxPSUtl.PopupMenu_DoPopup(APopupMenu);
    APopupMenuBuilder := ActiveBuilder.Create;
    try
      APopupMenuComponent := APopupMenuBuilder.BuildPopup(AControl, APopupMenu);
      if Assigned(APopupMenuComponent) then
      try
        try
          if Assigned(AControl) then
            APoint := AControl.ClientToScreen(Point(0, AControl.Height))
          else
            APoint := Point(X, Y);

          APopupMenuBuilder.InvokePopup(APoint.X, APoint.Y, AControl, APopupMenuComponent);
        except
          Application.HandleException(Self);
        end;
      finally
        APopupMenuBuilder.FreePopup(APopupMenuComponent);
      end;
    finally
      APopupMenuBuilder.Free;
    end;
  end
end;

procedure TdxPSPopupMenuController.ShowPopupAtMousePos(
  const AControl: TControl; const APopupMenu: TPopupMenu);
begin
  with GetMouseCursorPos do
    ShowPopup(X, Y, AControl, APopupMenu);
end;

procedure TdxPSPopupMenuController.RegisterBuilder(ABuilder: TdxPSPopupMenuBuilderClass);
begin
  FBuilders.Add(ABuilder);
end;

procedure TdxPSPopupMenuController.UnregisterBuilder(ABuilder: TdxPSPopupMenuBuilderClass);
begin
  FBuilders.Remove(ABuilder);
end;

procedure TdxPSPopupMenuController.RegisterControl(AControl: TControl);
begin
  if (AControl <> nil) and (IndexOfControl(AControl) = -1) then
  begin
    FControls.Add(AControl);
    FNexus.AddSender(AControl);
  end;
end;

procedure TdxPSPopupMenuController.UnregisterControl(AControl: TControl);
begin
  FControls.Remove(AControl);
  FNexus.RemoveSender(AControl);
end;

procedure TdxPSPopupMenuController.FinalizeInstance;
begin
  dxReleaseHook(dxPSPopupManMouseHook);
  dxReleaseHook(dxPSPopupManKeyboardHook);
  FreeAndNil(FBuilders);
  FreeAndNilControls;
  FreeAndNil(FNexus);
  inherited;
end;

procedure TdxPSPopupMenuController.InitializeInstance;
begin
  inherited;
  FBuilders := TdxClassList.Create;
  FControls := TList.Create;
  dxSetHook(htKeyboard, dxPSPopupManKeyboardHook);
  dxSetHook(htMouse, dxPSPopupManMouseHook);
  FNexus := TcxFreeNotificator.Create(nil);
  FNexus.OnFreeNotification := FreeNotification;
end;

function TdxPSPopupMenuController.IndexOfControl(AControl: TControl): Integer;
begin
  Result := FControls.IndexOf(AControl);
end;

procedure TdxPSPopupMenuController.FreeNotification(Sender: TComponent);
begin
  if Sender is TControl then
    UnregisterControl(TControl(Sender));
end;

function TdxPSPopupMenuController.TryShowPopup(AControl: TControl; Pt: TPoint): Boolean;

  function GetPopupMenu(var AControl: TControl): TPopupMenu;
  begin
    if AControl <> nil then
    begin
      Result := dxPSUtl.Control_GetPopupMenu(AControl);
      while (Result = nil) and (AControl.Parent <> nil) do
      begin
        AControl := AControl.Parent;
        Result := dxPSUtl.Control_GetPopupMenu(AControl);
      end;
    end
    else
      Result := nil;
  end;

  function CheckPopupMenu(AControl: TControl; const Pt: TPoint): TPopupMenu;
  begin
    Result := GetPopupMenu(AControl);
    if (Result <> nil) and not Result.AutoPopup then
      Result := nil;

    if (Result <> nil) and (Pt.X >= 0) and
      not PtInRect(AControl.ClientRect, AControl.ScreenToClient(Pt)) then
      Result := nil;
  end;

var
  PopupMenu: TPopupMenu;
begin
  PopupMenu := CheckPopupMenu(AControl, Pt);
  Result := (PopupMenu <> nil) and ActiveBuilder.CanShowPopup(PopupMenu);
  if Result then
  begin
    dxPSUtl.Control_DoContextPopup(AControl, AControl.ScreenToClient(Pt), Result);
    if not Result then
    begin
      Result := True;
      if Pt.X < 0 then
        Pt := AControl.ClientToScreen(cxNullPoint);
      ShowPopup(Pt.X, Pt.Y, AControl, PopupMenu);
    end;
  end;
end;

function TdxPSPopupMenuController.GetActiveBuilder: TdxPSPopupMenuBuilderClass;
begin
  if BuilderCount <> 0 then
    Result := Builders[BuilderCount - 1]
  else
    Result := TdxStandardPSPopupMenuBuilder;
end;

function TdxPSPopupMenuController.GetBuilder(Index: Integer): TdxPSPopupMenuBuilderClass;
begin
  Result := TdxPSPopupMenuBuilderClass(FBuilders[Index]);
end;

function TdxPSPopupMenuController.GetBuilderCount: Integer;
begin
  Result := FBuilders.Count;
end;

function TdxPSPopupMenuController.GetControl(Index: Integer): TControl;
begin
  Result := TControl(FControls[Index]);
end;

function TdxPSPopupMenuController.GetControlCount: Integer;
begin
  Result := FControls.Count;
end;

procedure TdxPSPopupMenuController.FreeAndNilControls;
begin
  while ControlCount > 0 do
    UnregisterControl(Controls[ControlCount - 1]);
  FreeAndNil(FControls);
end;

{ TAbstractdxPSPopupMenuBuilder }

constructor TAbstractdxPSPopupMenuBuilder.Create;
begin
  inherited Create;
end;

class function TAbstractdxPSPopupMenuBuilder.CanShowPopup(const APopupMenu: TPopupMenu): Boolean;
begin
  Result := True;
end;

class function TAbstractdxPSPopupMenuBuilder.RequireProcessDoPopup: Boolean;
begin
  Result := False;
end;

{ TdxStandardPSPopupMenuBuilder }

function TdxStandardPSPopupMenuBuilder.BuildPopup(const AControl: TControl;
  const APopupMenu: TPopupMenu): TComponent;
begin
  Result := APopupMenu;
  TPopupMenu(Result).PopupComponent := AControl;
end;

procedure TdxStandardPSPopupMenuBuilder.FreePopup(var APopupMenu: TComponent);
begin
end;

procedure TdxStandardPSPopupMenuBuilder.InvokePopup(
  X, Y: Integer; AControl: TControl; APopupMenu: TComponent);
var
  APopupMenuHeight: Integer;
begin
  if Assigned(AControl) then
  begin
    APopupMenuHeight := GetPopupMenuHeight(TPopupMenu(APopupMenu));
    if Y + APopupMenuHeight > GetDesktopWorkArea(Point(X, Y)).Bottom then
       Dec(Y, APopupMenuHeight + 2 + AControl.Height);
  end;
  TPopupMenu(APopupMenu).Popup(X, Y);
end;

initialization
  dxPSPopupMenuController.RegisterBuilder(TdxStandardPSPopupMenuBuilder);

end.
