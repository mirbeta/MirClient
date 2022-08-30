{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid Utils                                 }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridPopupMenu;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Types, SysUtils, Classes, Controls, cxGrid, cxGridCustomView,
  cxGridUIHelper, cxGridUICustomTableHelper, cxGridUITableHelper,
  cxGridUIBandedTableHelper, cxGridUICardHelper, cxGridCustomPopupMenu;

type
  { TcxGridPopupMenu }

  TcxGridPopupMenu = class(TcxCustomGridPopupMenu)
  strict private
    FAlwaysFireOnPopup: Boolean;
    FGrid: TcxGrid;
    FGridDefaultPopupMenu: TcxGridDefaultPopupMenu;
    FGridOperationHelper: TcxGridOperationHelper;
    FHitGridView: TcxCustomGridView;
    FHitPoint: TPoint;
    FHitTest: TcxCustomGridHitTest;
    FHitType: TcxGridViewHitType;
    FMouseUpNotification: TcxCustomGridNotification;
    FUseBuiltInPopupMenus: Boolean;

    FOnClick: TcxGridPopupMenuItemClickProc;
    FOnPopup: TcxGridBeforePopupProc;

    procedure CreateBuiltInPopupMenus;
    procedure FreeBuiltInPopupMenus;
    function GetPopupMenus: TcxPopupMenuInfos;
    procedure SetUseBuiltInPopupMenus(const Value: Boolean);
    procedure SetGrid(const AValue: TcxGrid);
    procedure SetPopupMenus(const AValue: TcxPopupMenuInfos);
    function TryPopupAsIcxPopupMenuIntf(APopupMenu: TComponent; AHitTest: TcxCustomGridHitTest; APoint: TPoint): Boolean;
    function TryPopupAsIDoPopupIntf(APopupMenu: TComponent; AHitTest: TcxCustomGridHitTest; APoint: TPoint): Boolean;
    function TryPopupAsVCLPopupMenu(APopupMenu: TComponent; AHitTest: TcxCustomGridHitTest; APoint: TPoint): Boolean;
    procedure SetHitTest(const Value: TcxCustomGridHitTest);
  protected
    function DoOnPopup(ASenderMenu: TComponent; AHitTest: TcxCustomGridHitTest; X,Y: Integer): Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function DoPopup(AAPPSKeyPressed: Boolean): Boolean; virtual;
    function GetPopupMenuInfo(const AMenu: TComponent; const AView: TcxCustomGridView; SearchDefault: Boolean = True): TcxPopupMenuInfo; override;
    property GridOperationHelper: TcxGridOperationHelper read FGridOperationHelper;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckShortCuts(var Message: TWMKeyDown): Boolean; override;
    function FindPopupMenuInfo(const AView: TcxCustomGridView;
      AHitType: TcxGridViewHitType; AHitTest: TcxCustomGridHitTest): TcxPopupMenuInfo; override;
    function GetBuiltInPopupMenuByClass(const AMenuClass: TcxPopupMenuClass): TComponent;

    procedure RegisterPopupMenu(const AMenu: TComponent; AHitTypes: TcxGridViewHitTypes;
      ADoPopupProc: TcxGridOnPopupProc; const AView: TcxCustomGridView); overload; virtual;
    procedure RegisterPopupMenu(const AMenu: TComponent; AHitTypes: TcxGridViewHitTypes;
      ADoPopupProc: TcxGridOnPopupProc; AViewClass: TcxCustomGridViewClass); overload; virtual;
    procedure UnregisterPopupMenu(const AMenu: TComponent; const AGridView: TcxCustomGridView;
      AHitTypes: TcxGridViewHitTypes = []); overload; virtual;
    procedure UnregisterPopupMenu(const AMenu: TComponent; AGridViewClass: TcxCustomGridViewClass;
      AHitTypes: TcxGridViewHitTypes = []); overload; virtual;

    property BuiltInPopupMenus: TcxGridDefaultPopupMenu read FGridDefaultPopupMenu;
    property HitGridView: TcxCustomGridView read FHitGridView;
    property HitPoint: TPoint read FHitPoint;
    property HitTest: TcxCustomGridHitTest read FHitTest write SetHitTest;
    property HitType: TcxGridViewHitType read FHitType;
  published
    property Grid: TcxGrid read FGrid write SetGrid;
    property PopupMenus: TcxPopupMenuInfos read GetPopupMenus write SetPopupMenus;
    property UseBuiltInPopupMenus: Boolean read FUseBuiltInPopupMenus write SetUseBuiltInPopupMenus default True;
    property OnMenuItemClick: TcxGridPopupMenuItemClickProc read FOnClick write FOnClick;
    property OnPopup: TcxGridBeforePopupProc read FOnPopup write FOnPopup;
    property AlwaysFireOnPopup: Boolean read FAlwaysFireOnPopup write FAlwaysFireOnPopup default False;
  end;

var
  ActiveGridPopupMenu: TcxGridPopupMenu;

implementation

uses
  Forms, cxGridLevel, Dialogs, cxGridStdPopupMenu, Menus, cxGridCustomTableView, cxControls;

type
  TcxGridContextPopupNotification = class(TcxCustomGridNotification)
  private
    FGridPopupMenu: TcxGridPopupMenu;
  protected
    procedure Notify(AKind: TcxGridNotificationKind; AData: TObject; var AHandled: Boolean); override;
    function NotificationKinds: TcxGridNotificationKinds; override;
  end;

  PWMKeyDown = ^TWMKeyDown;
  TcxCustomGridCellViewInfoAccess = class(TcxCustomGridCellViewInfo);
  TcxCustomGridViewInfoAccess = class(TcxCustomGridViewInfo);

procedure TcxGridContextPopupNotification.Notify(AKind: TcxGridNotificationKind;
  AData: TObject; var AHandled: Boolean);
begin
  case (AKind) of
    gnkContextMenu:
      begin
        Application.ProcessMessages;
        AHandled := FGridPopupMenu.DoPopup(Boolean(AData));
      end;
    gnkKeyDown:
      begin
        FGridPopupMenu.CheckShortCuts(PWMKeyDown(AData)^);
      end;
  end;
end;

function TcxGridContextPopupNotification.NotificationKinds: TcxGridNotificationKinds;
begin
  Result := [gnkContextMenu, gnkKeyDown];
end;

{TcxGridPopupMenu}

constructor TcxGridPopupMenu.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysFireOnPopup := False;
  FGridDefaultPopupMenu := TcxGridDefaultPopupMenu.Create(Self);
  FUseBuiltInPopupMenus := True;
  CreateBuiltInPopupMenus;
  FGridOperationHelper := TcxGridOperationHelper.Create(Self);
  FMouseUpNotification := TcxGridContextPopupNotification.Create;
  TcxGridContextPopupNotification(FMouseUpNotification).FGridPopupMenu := self;
end;

destructor TcxGridPopupMenu.Destroy;
begin
  FreeAndNil(FHitTest);
  FreeBuiltInPopupMenus;
  FreeAndNil(FGridOperationHelper);
  FreeAndNil(FGridDefaultPopupMenu);
  if Grid <> nil then
    Grid.UnregisterNotification(FMouseUpNotification);
  FreeAndNil(FMouseUpNotification);
  inherited Destroy;
end;

function TcxGridPopupMenu.CheckShortCuts(var Message: TWMKeyDown): Boolean;
begin
  Result := inherited CheckShortCuts(Message);
  if not Result then
    Result := FGridDefaultPopupMenu.CheckShortCuts(Message);
end;

function TcxGridPopupMenu.GetBuiltInPopupMenuByClass(const AMenuClass: TcxPopupMenuClass): TComponent;
var
  I: Integer;
begin
  for I := 0 to FGridDefaultPopupMenu.Count - 1 do
  begin
    if Assigned(FGridDefaultPopupMenu[i].PopupMenu) and (FGridDefaultPopupMenu[I].PopupMenu.ClassType = AMenuClass) then
      Exit(FGridDefaultPopupMenu[i].PopupMenu);
  end;
  Result := nil;
end;

function TcxGridPopupMenu.FindPopupMenuInfo(const AView: TcxCustomGridView;
  AHitType: TcxGridViewHitType; AHitTest: TcxCustomGridHitTest): TcxPopupMenuInfo;
begin
  Result := inherited FindPopupMenuInfo(AView, AHitType, AHitTest);
  if Result = nil then
    Result := FGridDefaultPopupMenu.FindPopupMenuInfo(AView, AHitType, AHitTest);
end;

function TcxGridPopupMenu.GetPopupMenuInfo(const AMenu: TComponent;
  const AView: TcxCustomGridView; SearchDefault: Boolean = True): TcxPopupMenuInfo;
var
  I : Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    If (AMenu = MenuInfos[I].PopupMenu) and (AView = MenuInfos[I].GridView) then
    begin
      Result := MenuInfos[I];
      Result.Index := I;
      Break;
    end;

  if SearchDefault and not Assigned(Result) then
    Result := FGridDefaultPopupMenu.GetPopupMenuInfo(AMenu, TcxCustomGridViewClass(AView.ClassType));
end;

procedure TcxGridPopupMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Grid) then
    FGrid := nil;
end;

procedure TcxGridPopupMenu.RegisterPopupMenu(const AMenu: TComponent;
  AHitTypes: TcxGridViewHitTypes; ADoPopupProc: TcxGridOnPopupProc; const AView: TcxCustomGridView);
begin
  if Assigned(AView) then
    AdjustMenuInfo(GetPopupMenuInfo(AMenu, AView), AMenu, AHitTypes, ADoPopupProc, nil, AView);
end;

procedure TcxGridPopupMenu.RegisterPopupMenu(const AMenu: TComponent;
  AHitTypes: TcxGridViewHitTypes; ADoPopupProc: TcxGridOnPopupProc; AViewClass: TcxCustomGridViewClass);
begin
  FGridDefaultPopupMenu.RegisterPopupMenu(AMenu, AHitTypes, ADoPopupProc, AViewClass);
end;

procedure TcxGridPopupMenu.UnregisterPopupMenu(const AMenu: TComponent;
  const AGridView: TcxCustomGridView; AHitTypes: TcxGridViewHitTypes);
var
  AMenuInfo: TcxPopupMenuInfo;
begin
  AMenuInfo := GetPopupMenuInfo(AMenu, AGridView, false);
  If (AMenuInfo <> nil) then
    If AMenuInfo.HitTypes = AHitTypes then
      AMenuInfo.Free
    else
      AMenuInfo.HitTypes := AMenuInfo.HitTypes - AHitTypes;
end;

procedure TcxGridPopupMenu.UnregisterPopupMenu(const AMenu: TComponent;
  AGridViewClass: TcxCustomGridViewClass; AHitTypes: TcxGridViewHitTypes = []);
begin
  FGridDefaultPopupMenu.UnRegisterPopupMenu(AMenu, AGridViewClass, AHitTypes);
end;

function TcxGridPopupMenu.DoOnPopup(ASenderMenu: TComponent;
  AHitTest: TcxCustomGridHitTest; X,
  Y: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnPopup) then
    FOnPopup(ASenderMenu, AHitTest, X, Y, Result);
end;

function TcxGridPopupMenu.DoPopup(AAPPSKeyPressed: Boolean): Boolean;

  function DoAutoPopup(AMenuInfo:TcxPopupMenuInfo; AHitTest: TcxCustomGridHitTest;
    APoint: TPoint): Boolean;
  begin
    Result := Assigned(AMenuInfo.PopupMenu);
    if Result then
      Result := TryPopupAsIDoPopupIntf(AMenuInfo.PopupMenu, AHitTest, APoint) or
        TryPopupAsIcxPopupMenuIntf(AMenuInfo.PopupMenu, AHitTest, APoint) or
        TryPopupAsVCLPopupMenu(AMenuInfo.PopupMenu, AHitTest, APoint);
  end;

  function DoPopupByAPPSKey(var AHitCode: Integer; var APoint: TPoint): Boolean;
  var
    AViewInfo: TcxCustomGridCellViewInfo;
    ABounds: TRect;
  begin
    Result := FHitGridView is TcxCustomGridTableView;
    if not Result then Exit;
    with TcxCustomGridTableView(FHitGridView).Controller do
    begin
      if (FocusedRecord <> nil) then
      begin
        if not FocusedRecord.Visible then
          MakeFocusedRecordVisible;
        if (FocusedItem <> nil) and (FocusedItem.FocusedCellViewInfo <> nil) then
        begin
          ABounds := FocusedItem.FocusedCellViewInfo.ContentBounds;
          AViewInfo := FocusedItem.FocusedCellViewInfo;
        end
        else
        begin
          ABounds := FocusedRecord.ViewInfo.ContentBounds;
          if ABounds.Left < 0 then
             ABounds.Left := 0;
          AViewInfo := FocusedRecord.ViewInfo;
        end;
        with ABounds do
          APoint := Point(Left, Bottom);
        HitTest := TcxCustomGridCellViewInfoAccess(AViewInfo).GetHitTestClass.Instance(APoint);
        TcxCustomGridCellViewInfoAccess(AViewInfo).InitHitTest(FHitTest);
      end
      else
      begin
        APoint := Point(0,0);
        HitTest := TcxGridViewNoneHitTest.Instance(APoint);
        TcxCustomGridViewInfoAccess(FHitGridView.Site.ViewInfo).InitHitTest(FHitTest);
      end;
      AHitCode := FHitTest.HitTestCode;
      FHitPoint := APoint;
      APoint := FHitGridView.Site.ClientToScreen(APoint);
    end;
  end;

  function InternalPopup(AHitType: TcxGridViewHitType; APoint: TPoint;
    AHitTest: TcxCustomGridHitTest): Boolean;
  var
    AMenuInfo: TcxPopupMenuInfo;
  begin
    Result := False;
    if FHitGridView = nil then Exit;
    AMenuInfo := FindPopupMenuInfo(FHitGridView.PatternGridView, AHitType, AHitTest);
    if AMenuInfo = nil then
      Exit;
    if Assigned(AMenuInfo.OnPopup) then
    begin
      if Assigned(AMenuInfo.PopupMenu) or FAlwaysFireOnPopup then
      begin
        if DoOnPopup(AMenuInfo.PopupMenu, AHitTest, APoint.X, APoint.Y) then
          AMenuInfo.OnPopup(AMenuInfo.PopupMenu, AHitTest, APoint.X, APoint.Y);
        Result := True;
      end;
    end
    else
      Result := DoAutoPopup(AMenuInfo, AHitTest, APoint);
  end;

var
  AHitCode: Integer;
  APoint: TPoint;
begin
  Result := True;
  FHitGridView := Grid.FocusedView;
  ActiveGridPopupMenu := Self;
  AHitCode := 0;
  if AAPPSKeyPressed then
    Result := DoPopupByAPPSKey(AHitCode, APoint)
  else
  begin
    GetCursorPos(APoint);
    FHitPoint := Grid.ScreenToClient(APoint);
    HitTest := Grid.ViewInfo.GetHitTest(FHitPoint.X, FHitPoint.Y);
    AHitCode := FHitTest.HitTestCode;
    if (FHitGridView = nil) or
      not PtInRect(FHitGridView.Site.ClientRect, FHitGridView.Site.ScreenToClient(APoint)) then
      AHitCode := AHitCode + cxhtGridBase;
  end;
  if Result then
  begin
    FHitType := GetHitTypeByHitCode(AHitCode);
    Result := InternalPopup(FHitType, APoint, FHitTest);
  end;
end;

procedure TcxGridPopupMenu.CreateBuiltInPopupMenus;
begin
  BuiltInPopupMenuList.RegisterItemsTo(FGridDefaultPopupMenu);
end;

procedure TcxGridPopupMenu.FreeBuiltInPopupMenus;
var
  I: Integer;
begin
  for I := FGridDefaultPopupMenu.Count - 1 downto 0 do
    if FGridDefaultPopupMenu[I].IsBuiltIn then
      FGridDefaultPopupMenu[I].Free;
end;

function TcxGridPopupMenu.GetPopupMenus: TcxPopupMenuInfos;
begin
  Result := PopupMenuInfos;
end;

procedure TcxGridPopupMenu.SetUseBuiltInPopupMenus(const Value: Boolean);
begin
  if FUseBuiltInPopupMenus <> Value then
    if Value then
      CreateBuiltInPopupMenus
    else
      FreeBuiltInPopupMenus;
  FUseBuiltInPopupMenus := Value;
end;

procedure TcxGridPopupMenu.SetGrid(const AValue: TcxGrid);
begin
  if FGrid <> AValue then
  begin
    FGridOperationHelper.Grid := AValue;
    if (FGrid <> nil) and not (csDestroying in FGrid.ComponentState) then
    begin
      FGrid.UnregisterNotification(FMouseUpNotification);
      FGrid.RemoveFreeNotification(self);
      ClearMenuInfosGridView;
    end;
    FGrid := AValue;
    if FGrid <> nil then
    begin
      FGrid.FreeNotification(self);
      FGrid.RegisterNotification(FMouseUpNotification);
    end;
  end;
end;

procedure TcxGridPopupMenu.SetHitTest(const Value: TcxCustomGridHitTest);
begin
  FreeAndNil(FHitTest);
  FHitTest := Value.Clone;
end;

procedure TcxGridPopupMenu.SetPopupMenus(const AValue: TcxPopupMenuInfos);
begin
  PopupMenuInfos.Assign(AValue);
end;

function TcxGridPopupMenu.TryPopupAsIcxPopupMenuIntf(
  APopupMenu: TComponent; AHitTest: TcxCustomGridHitTest; APoint: TPoint): Boolean;
var
  AIcxPopupMenuIntf: IcxPopupMenu;
begin
  Result := False;
  if APopupMenu = nil then
    Exit;
  Result := Supports(APopupMenu, IcxPopupMenu, AIcxPopupMenuIntf);
  if Result and DoOnPopup(APopupMenu, AHitTest, APoint.X, APoint.Y) then
    AIcxPopupMenuIntf.Popup(APoint.X, APoint.Y);
end;

function TcxGridPopupMenu.TryPopupAsIDoPopupIntf(APopupMenu: TComponent;
  AHitTest: TcxCustomGridHitTest; APoint: TPoint): Boolean;
var
  AIDoPopupIntf: IDoPopup;
begin
  Result := False;
  if APopupMenu = nil then
    Exit;
  Result := Supports(APopupMenu, IDoPopup, AIDoPopupIntf);
  if Result and DoOnPopup(APopupMenu, AHitTest, APoint.X, APoint.Y) then
    TcxGridOnPopupProc(AIDoPopupIntf.GetPopupHandler)(APopupMenu, AHitTest, APoint.X, APoint.Y);
end;

function TcxGridPopupMenu.TryPopupAsVCLPopupMenu(APopupMenu: TComponent;
  AHitTest: TcxCustomGridHitTest; APoint: TPoint): Boolean;
begin
  Result := False;
  if APopupMenu = nil then
    Exit;
  Result := (APopupMenu is TPopupMenu) or (APopupMenu.InheritsFrom(TPopupMenu));
  if Result and DoOnPopup(APopupMenu, AHitTest, APoint.X, APoint.Y) then
  begin
    TPopupMenu(APopupMenu).PopupComponent := FHitGridView;
    TPopupMenu(APopupMenu).Popup(APoint.X, APoint.Y);
  end;
end;

end.
