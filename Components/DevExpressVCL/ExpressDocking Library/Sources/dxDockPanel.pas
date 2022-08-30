{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDocking                                           }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDOCKING AND ALL ACCOMPANYING   }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxDockPanel;

{$I cxVer.inc}

interface

uses
  Menus, Windows, Graphics, Classes, Controls, ExtCtrls, Messages, Forms,
  dxCore, dxDockConsts, dxDockControl, cxControls, cxPC, cxGraphics, cxLookAndFeels;

type

  { TdxDockPanelTabControlController }

  TdxDockPanelTabControlController = class(TdxDockingTabControlController)
  protected
    function IsDockable(AControl: TdxCustomDockControl): Boolean; override;
    procedure DoUndock(const APoint: TPoint; ADockControl: TdxCustomDockControl); override;
  end;

  { TdxDockPanelTabControlProperties }

  TdxDockPanelTabControlProperties = class(TdxDockingTabControlProperties);

  { TdxDockPanel }

  TdxDockPanel = class(TdxCustomDockControl, IcxTabControl, IcxControlComponentState)
  strict private
    FShowSingleTab: Boolean;
    FTabControlSuggestedBounds: TRect;
    FTabControlViewInfo: TdxDockingTabControlViewInfo;
    FTabsController: TdxDockPanelTabControlController;
    FTabsProperties: TdxDockPanelTabControlProperties;

    FOnCustomDrawTab: TdxDockingTabControlPropertiesDrawTabEvent;
    FOnCustomDrawTabEx: TdxDockingTabControlPropertiesDrawTabExEvent;

    function GetTabRect: TRect;
    function GetTabsRect: TRect;
    procedure SetShowSingleTab(const Value: Boolean);
    procedure SetTabsProperties(AValue: TdxDockPanelTabControlProperties);

    procedure HandlerDrawTab(AProperties: TcxCustomTabControlProperties; ATab: TcxTab; var ADefaultDraw: Boolean);
    procedure HandlerDrawTabEx(AProperties: TcxCustomTabControlProperties; ATab: TcxTab; AFont: TFont);

    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
  protected
    // IcxTabControl
    function CanDrawParentBackground: Boolean;
    function GetBoundsRect: TRect;
    function GetCanvas: TcxCanvas;
    function GetColor: TColor;
    function GetControl: TWinControl;
    function GetController: TcxCustomTabControlController;
    function GetDragAndDropObject: TcxDragAndDropObject;
    function GetDragAndDropState: TcxDragAndDropState;
    function GetFont: TFont;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetPainter: TcxPCCustomPainter;
    function GetProperties: TcxCustomTabControlProperties;
    function GetViewInfo: TcxCustomTabControlViewInfo;
    function IsEnabled: Boolean;
    function IsFocused: Boolean;
    function IsParentBackground: Boolean;
    procedure InvalidateRect(const R: TRect; AEraseBackground: Boolean);
    procedure RequestLayout;
    procedure SetModified;

    function GetClientRect: TRect; override;
    function IsDockPanel: Boolean; override;
    procedure Changed; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure MouseLeave; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateCaption; override;
    procedure ValidateInsert(AComponent: TComponent); override;

    // Activation
    procedure CheckActiveDockControl; override;
    procedure DoActivate; override;
    // Docking
    procedure UpdateControlDockZones(AControl: TdxCustomDockControl; AZoneWidth: Integer); override;
    // Site layout
    procedure CreateLayout(AControl: TdxCustomDockControl; AType: TdxDockingType; Index: Integer); override;
    // SideContainer site
    function CanAcceptSideContainerItems(AContainer: TdxSideContainerDockSite): Boolean; override;
    // TabContainer site
    procedure AssignTabContainerSiteProperties(ASite: TdxTabContainerDockSite); override;
    function CanAcceptTabContainerItems(AContainer: TdxTabContainerDockSite): Boolean; override;
    // Painting
    procedure CalculateNC(var ARect: TRect); override;
    procedure InvalidateCaptionArea; override;
    procedure NCPaint(ACanvas: TcxCanvas); override;
    function NeedInvalidateCaptionArea: Boolean; override;
    function HasBorder: Boolean; override;
    function HasCaption: Boolean; override;
    function HasTabs: Boolean; override;

    property TabControlViewInfo: TdxDockingTabControlViewInfo read FTabControlViewInfo;
    property TabRect: TRect read GetTabRect;
    property TabsController: TdxDockPanelTabControlController read FTabsController;
    property TabsRect: TRect read GetTabsRect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CanActive: Boolean; override;
    function CanAutoHide: Boolean; override;
    function CanDock: Boolean; override;
    function CanDockHost(AControl: TdxCustomDockControl; AType: TdxDockingType): Boolean; override;
    function CanMaximize: Boolean; override;
  published
    property AllowClosing;
    property AllowDock;
    property AllowDockClients;
    property AllowFloating;
    property AutoHide;
    property Caption;
    property CaptionButtons;
    property CustomCaptionButtons;
    property Dockable;
    property ImageIndex;
    property ShowCaption;
    property ShowSingleTab: Boolean read FShowSingleTab write SetShowSingleTab default False;
    property TabsProperties: TdxDockPanelTabControlProperties read FTabsProperties write SetTabsProperties;

    property OnActivate;
    property OnAutoHideChanged;
    property OnAutoHideChanging;
    property OnCanResize;
    property OnClose;
    property OnCloseQuery;
    property OnCreateFloatSite;
    property OnCreateSideContainer;
    property OnCreateTabContainer;
    property OnCustomDrawDockingSelection;
    property OnCustomDrawResizingSelection;
    property OnCustomDrawTab: TdxDockingTabControlPropertiesDrawTabEvent read FOnCustomDrawTab write FOnCustomDrawTab;
    property OnCustomDrawTabEx: TdxDockingTabControlPropertiesDrawTabExEvent read FOnCustomDrawTabEx write FOnCustomDrawTabEx;
    property OnDestroy;
    property OnDock;
    property OnDocking;
    property OnEndDocking;
    property OnEndResizing;
    property OnGetAutoHidePosition;
    property OnLayoutChanged;
    property OnMakeFloating;
    property OnResize;
    property OnResizing;
    property OnRestoreDockPosition;
    property OnStartDocking;
    property OnStartResizing;
    property OnStoreDockPosition;
    property OnUnDock;
    property OnUpdateDockZones;
    property OnUpdateResizeZones;
  end;

implementation

uses
  Types, SysUtils, dxDockZones, cxGeometry;

type
  TdxCustomDockControlAccess = class(TdxCustomDockControl);
  TdxDockingControllerAccess = class(TdxDockingController);

{ TdxDockPanelTabControlController }

function TdxDockPanelTabControlController.IsDockable(AControl: TdxCustomDockControl): Boolean;
begin
  Result := AControl <> nil;
end;

procedure TdxDockPanelTabControlController.DoUndock(
  const APoint: TPoint; ADockControl: TdxCustomDockControl);
var
  ADockControlAccess: TdxCustomDockControlAccess;
begin
  if ADockControl.FloatDockSite <> nil then
  begin
    ADockControlAccess := TdxCustomDockControlAccess(ADockControl);
    ADockControlAccess.RestoreDockPosition(ADockControlAccess.ScreenToWindow(APoint));
  end
  else
    ADockControl.MakeFloating;
end;

{ TdxDockPanel }

constructor TdxDockPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTabsProperties := TdxDockPanelTabControlProperties.Create(Self);
  FTabsProperties.OnDrawTabEx := HandlerDrawTabEx;
  FTabsProperties.OnDrawTab := HandlerDrawTab;
  FTabsController := TdxDockPanelTabControlController.Create(Self);
  FTabControlViewInfo := TdxDockingTabControlViewInfo.Create(Self);
  TabsController.AddTab(Self, True);
  ShowSingleTab := False;
  UseDoubleBuffer := True;
  SetBounds(0, 0, 185, 140);
end;

destructor TdxDockPanel.Destroy;
begin
  FreeAndNil(FTabControlViewInfo);
  FreeAndNil(FTabsController);
  FreeAndNil(FTabsProperties);
  inherited Destroy;
end;

function TdxDockPanel.CanActive: Boolean;
begin
  Result := True;
end;

function TdxDockPanel.CanAutoHide: Boolean;
begin
  Result := IsLoading or ((AutoHideHostSite <> nil) and ((AutoHideControl = nil) or (AutoHideControl = Self)));
end;

function TdxDockPanel.CanDock: Boolean;
begin
  Result := True;
end;

function TdxDockPanel.CanDockHost(AControl: TdxCustomDockControl; AType: TdxDockingType): Boolean;
var
  ACanDockHost: Boolean;
begin
  if Container <> nil then
  begin
    ACanDockHost := Container.CanContainerDockHost(AType);
    if Container is TdxTabContainerDockSite then
    begin
      if doSideContainerCanInTabContainer in ControllerOptions then
        ACanDockHost := ACanDockHost or (AType in [dtLeft, dtRight, dtTop, dtBottom]);
    end;
    if Container is TdxSideContainerDockSite then
    begin
      if doTabContainerCanInSideContainer in ControllerOptions then
        ACanDockHost := ACanDockHost or (AType in [dtClient]);
      if doSideContainerCanInSideContainer in ControllerOptions then
        ACanDockHost := ACanDockHost or (AType in [dtLeft, dtRight, dtTop, dtBottom]);
    end;
  end
  else
    ACanDockHost := (AType in [dtClient, dtLeft, dtRight, dtTop, dtBottom]);

  Result := inherited CanDockHost(AControl, AType) and ACanDockHost;
end;

function TdxDockPanel.CanMaximize: Boolean;
begin
  Result := not AutoHide and (SideContainer <> nil) and (SideContainer.ValidChildCount > 1);
end;

procedure TdxDockPanel.CheckActiveDockControl;
begin
  if Active and not IsDesigning and CanFocusEx and not dxDockingController.IsDockControlFocusedEx(Self) then
    DoActivate;
end;

procedure TdxDockPanel.DoActivate;
var
  AParentForm: TCustomForm;
begin
  AParentForm := Forms.GetParentForm(Self);
  if (AParentForm <> nil) then
  begin
    AParentForm.ActiveControl := Self;
    if AParentForm.ActiveControl = Self then
      SelectFirst;
  end
  else
    SetFocus;
end;

// IcxTabControl
function TdxDockPanel.CanDrawParentBackground: Boolean;
begin
  Result := False;
end;

function TdxDockPanel.GetBoundsRect: TRect;
begin
  Result := FTabControlSuggestedBounds;
end;

function TdxDockPanel.GetCanvas: TcxCanvas;
begin
  Result := Canvas;
end;

function TdxDockPanel.GetColor: TColor;
begin
  Result := Color;
end;

function TdxDockPanel.GetControl: TWinControl;
begin
  Result := Self;
end;

function TdxDockPanel.GetController: TcxCustomTabControlController;
begin
  Result := TabsController;
end;

function TdxDockPanel.GetDragAndDropObject: TcxDragAndDropObject;
begin
  Result := nil;
end;

function TdxDockPanel.GetDragAndDropState: TcxDragAndDropState;
begin
  Result := ddsNone;
end;

function TdxDockPanel.GetFont: TFont;
begin
  Result := Font;
end;

function TdxDockPanel.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := Controller.LookAndFeel(ParentForm);
end;

function TdxDockPanel.GetPainter: TcxPCCustomPainter;
begin
  Result := TabControlViewInfo.Painter;
end;

function TdxDockPanel.GetProperties: TcxCustomTabControlProperties;
begin
  Result := TabsProperties;
end;

function TdxDockPanel.GetViewInfo: TcxCustomTabControlViewInfo;
begin
  Result := FTabControlViewInfo;
end;

procedure TdxDockPanel.InvalidateRect(const R: TRect; AEraseBackground: Boolean);
begin
  cxRedrawWindow(Handle, RDW_FRAME or RDW_INVALIDATE);
end;

function TdxDockPanel.IsEnabled: Boolean;
begin
  Result := Enabled;
end;

function TdxDockPanel.IsFocused: Boolean;
begin
  Result := Focused;
end;

function TdxDockPanel.IsParentBackground: Boolean;
begin
  Result := False;
end;

procedure TdxDockPanel.RequestLayout;
begin
  NCChanged(True);
end;

procedure TdxDockPanel.SetModified;
begin
  Modified;
end;

procedure TdxDockPanel.Changed;
begin
  inherited Changed;
  TabsController.TabInfoChanged(Self);
end;

procedure TdxDockPanel.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  TabsProperties.ChangeScale(M, D);
end;

procedure TdxDockPanel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('TabPosition', TabsProperties.ReadOldTabsPosition, nil, False);
end;

procedure TdxDockPanel.MouseLeave;
begin
  inherited MouseLeave;
  TabsController.MouseLeave;
end;

procedure TdxDockPanel.UpdateCaption;
begin
  inherited UpdateCaption;
  if not IsDestroying then
    TabsController.TabInfoChanged(Self);
end;

function TdxDockPanel.GetClientRect: TRect;
begin
  if HandleAllocated then
    Result := inherited GetClientRect
  else
    Result := cxEmptyRect;

  if (disLoading in TdxDockingControllerAccess(Controller).FInternalState) and
    TdxDockingControllerAccess(Controller).IsUpdateNCLocked and
    ((Height = 0) or (Width = 0) or (cxRectWidth(Result) = 0) or (cxRectHeight(Result) = 0)) then
    Result := Rect(0, 0, OriginalWidth, OriginalHeight);
end;

procedure TdxDockPanel.SetParent(AParent: TWinControl);
var
  ALeft, ATop: Integer;
  APrevParent: TWinControl;
  I: TdxDockingType;
begin
  if (UpdateLayoutLock = 0) and not IsDestroying and ((AParent = nil) or not (csLoading in AParent.ComponentState)) then
  begin
    if IsDesigning and (AParent <> nil) then
      ValidateContainer(AParent);
    if AParent is TdxCustomDockControl then
      for I := Low(TdxDockingType) to High(TdxDockingType) do
      begin
        if TdxCustomDockControl(AParent).CanDockHost(Self, I) then
        begin
          DockTo(AParent as TdxCustomDockControl, I, -1);
          Exit;
        end;
      end;
    // else Make Float
    APrevParent := Parent;
    ALeft := Left;
    ATop := Top;
    if AParent = nil then
      AParent := APrevParent;
    if AParent <> nil then
    begin
      ALeft := AParent.ClientOrigin.X + ALeft;
      ATop := AParent.ClientOrigin.Y + ATop;
    end;
    MakeFloating(ALeft, ATop);
  end
  else
    if (AParent <> nil) and not ((AParent is TdxCustomDockControl) or
       (AutoHide and (AParent is TdxDockSiteAutoHideContainer)))
    then
      raise EdxException.CreateFmt(sdxInvalidParent, [ClassName])
    else
      inherited SetParent(AParent);
end;

procedure TdxDockPanel.ValidateInsert(AComponent: TComponent);
begin
  if AComponent is TdxCustomDockControl then
    raise EdxException.CreateFmt(sdxInvalidPanelChild, [AComponent.ClassName]);
end;

function TdxDockPanel.IsDockPanel: Boolean;
begin
  Result := True;
end;

procedure TdxDockPanel.UpdateControlDockZones(AControl: TdxCustomDockControl; AZoneWidth: Integer);
begin
  if not (doUseCaptionAreaToClientDocking in ControllerOptions) then
  begin
    if (TabContainer <> nil) and TdxTabContainerZone.ValidateDockZone(Self, AControl) then
      DockZones.Insert(0, TdxTabContainerZone.Create(TabContainer))
    else
      if TdxDockPanelClientZone.ValidateDockZone(Self, AControl) then
        DockZones.Insert(0, TdxDockPanelClientZone.Create(Self));
  end;
  inherited;
  if doUseCaptionAreaToClientDocking in ControllerOptions then
  begin
    if (TabContainer <> nil) and TdxTabContainerCaptionZone.ValidateDockZone(Self, AControl) then
      DockZones.Insert(0, TdxTabContainerCaptionZone.Create(TabContainer))
    else
      if TdxDockPanelCaptionClientZone.ValidateDockZone(Self, AControl) then
        DockZones.Insert(0, TdxDockPanelCaptionClientZone.Create(Self));
  end;
end;

procedure TdxDockPanel.CreateLayout(AControl: TdxCustomDockControl; AType: TdxDockingType; Index: Integer);
begin
  if (Container <> nil) and Container.CanContainerDockHost(AType) then
    CreateContainerLayout(Container, AControl, AType, DockIndex)
  else
    case AType of
      dtClient:
        CreateTabContainerLayout(AControl, AType, Index);
      dtLeft, dtRight, dtTop, dtBottom:
        CreateSideContainerLayout(AControl, AType, Index);
      else
        Assert(False, Format(sdxInternalErrorCreateLayout, [TdxContainerDockSite.ClassName]));
    end;
end;

function TdxDockPanel.CanAcceptSideContainerItems(AContainer: TdxSideContainerDockSite): Boolean;
begin
  Result := True;
end;

procedure TdxDockPanel.AssignTabContainerSiteProperties(ASite: TdxTabContainerDockSite);
begin
  inherited AssignTabContainerSiteProperties(ASite);
  if ShowSingleTab then
    ASite.TabsProperties.Assign(TabsProperties);
end;

function TdxDockPanel.CanAcceptTabContainerItems(AContainer: TdxTabContainerDockSite): Boolean;
begin
  Result := True;
end;

procedure TdxDockPanel.CalculateNC(var ARect: TRect);
begin
  inherited CalculateNC(ARect);
  FTabControlSuggestedBounds := ARect;
  TabsController.RefreshImageList;
  TabControlViewInfo.Calculate;
  if HasTabs then
    ARect := TabControlViewInfo.PageClientBounds;
end;

procedure TdxDockPanel.InvalidateCaptionArea;
begin
  if HasTabs then
    InvalidateNC(True)
  else
    inherited InvalidateCaptionArea;
end;

procedure TdxDockPanel.NCPaint(ACanvas: TcxCanvas);
begin
  inherited NCPaint(ACanvas);
  if HasTabs then
    TabControlViewInfo.Draw(ACanvas);
end;

function TdxDockPanel.NeedInvalidateCaptionArea: Boolean;
begin
  Result := inherited NeedInvalidateCaptionArea or HasTabs;
end;

function TdxDockPanel.HasBorder: Boolean;
begin
  Result := (FloatDockSite = nil) and
    (AutoHide or
      ((TabContainer = nil) or ((TabContainer.ValidChildCount < 2) and not TabContainer.AutoHide)) and
      ((ParentDockControl = nil) or TdxCustomDockControlAccess(ParentDockControl).CanUndock(Self))
    );
end;

function TdxDockPanel.HasCaption: Boolean;
begin
  Result := ShowCaption and HasBorder;
end;

function TdxDockPanel.HasTabs: Boolean;
begin
  Result := ShowSingleTab and ((TabContainer = nil) or (TabContainer.ValidChildCount = 1));
end;

function TdxDockPanel.GetTabRect: TRect;
begin
  Result := TabControlViewInfo.TabBounds[0];
end;

function TdxDockPanel.GetTabsRect: TRect;
begin
  Result := TabControlViewInfo.TabsAreaBounds;
end;

procedure TdxDockPanel.SetShowSingleTab(const Value: Boolean);
begin
  if FShowSingleTab <> Value then
  begin
    FShowSingleTab := Value;
    NCChanged;
  end;
end;

procedure TdxDockPanel.SetTabsProperties(AValue: TdxDockPanelTabControlProperties);
begin
  TabsProperties.Assign(AValue);
end;

procedure TdxDockPanel.HandlerDrawTab(AProperties: TcxCustomTabControlProperties; ATab: TcxTab; var ADefaultDraw: Boolean);
begin
  if Assigned(OnCustomDrawTab) then
    OnCustomDrawTab(Self, TabsProperties, ATab, ADefaultDraw);
end;

procedure TdxDockPanel.HandlerDrawTabEx(
  AProperties: TcxCustomTabControlProperties; ATab: TcxTab; AFont: TFont);
begin
  if Assigned(OnCustomDrawTabEx) then
    OnCustomDrawTabEx(Self, TabsProperties, ATab, AFont);
end;

procedure TdxDockPanel.CMHintShow(var Message: TCMHintShow);
begin
  if not (HasTabs and TabsController.HasHintableObject) then
    inherited;
end;

procedure TdxDockPanel.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  if HasTabs and (Message.Result = 0) then
    TabsController.DoMouseDblClick(Message);
end;

procedure TdxDockPanel.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if HasTabs and (Message.Result = 0) then
    TabsController.DoMouseDown(Message);
end;

procedure TdxDockPanel.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  if HasTabs and (Message.Result = 0) then
    TabsController.DoMouseUp(Message);
end;

procedure TdxDockPanel.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
  if HasTabs and (Message.Result = 0) then
    TabsController.DoMouseMove(Message);
end;

initialization
  RegisterClasses([TdxDockPanel]);
end.
