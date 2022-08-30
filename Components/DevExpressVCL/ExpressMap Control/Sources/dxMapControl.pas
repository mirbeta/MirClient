{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
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

unit dxMapControl;

interface

{$I cxVer.inc}

uses
  SysUtils, Graphics, Classes, Types, RTLConsts, Forms, Math, Windows, Controls,
  Generics.Defaults, Generics.Collections, DateUtils,
  cxClasses, cxControls, cxGeometry, dxAnimation, dxCore, dxCoreClasses, cxLookAndFeels,
  dxCoreGraphics, cxGraphics, cxLookAndFeelPainters, dxGdiPlusClasses, dxTypeHelpers, dxScreenTip,
  dxMapControlTypes, dxMapControlViewInfo, dxMapLayer, dxMapImageTileLayer,
  dxCustomMapItemLayer, dxMapItemLayer, dxMapItemFileLayer, dxMapItem,
  dxMapControlInformationProvider;

const
  dxMapControlInertialScrollingDecelerationRatio = 1500;
  dxMapControlDefaultZoomPaddingFactor = 0.15;

type
  TdxCustomMapControl = class;

  TdxMapItemSelectMode = (mismNone, mismSingle, mismMultiple, mismExtended);

  TdxMapControlNavigationPanelStyle = class(TcxOwnedPersistent)
  private
    FColor: TdxAlphaColor;
    FCoordinateFont: TFont;
    FCoordinateTextColor: TColor;
    FElementColor: TdxAlphaColor;
    FElementHotColor: TdxAlphaColor;
    FElementPressedColor: TdxAlphaColor;
    FMapControl: TdxCustomMapControl;
    FScaleFont: TFont;
    FScaleTextColor: TColor;
    procedure SetColor(Value: TdxAlphaColor);
    procedure SetCoordinateFont(Value: TFont);
    procedure SetCoordinateTextColor(Value: TColor);
    procedure SetElementColor(Value: TdxAlphaColor);
    procedure SetElementHotColor(Value: TdxAlphaColor);
    procedure SetElementPressedColor(Value: TdxAlphaColor);
    procedure SetScaleFont(Value: TFont);
    procedure SetScaleTextColor(Value: TColor);
  protected
    procedure Changed;
    procedure ChangeScale(M, D: Integer);
    procedure DoAssign(Source: TPersistent); override;
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property Color: TdxAlphaColor read FColor write SetColor default dxacDefault;
    property CoordinateFont: TFont read FCoordinateFont write SetCoordinateFont;
    property CoordinateTextColor: TColor read FCoordinateTextColor write SetCoordinateTextColor default clDefault;
    property ElementColor: TdxAlphaColor read FElementColor write SetElementColor default dxacDefault;
    property ElementHotColor: TdxAlphaColor read FElementHotColor write SetElementHotColor default dxacDefault;
    property ElementPressedColor: TdxAlphaColor read FElementPressedColor write SetElementPressedColor default dxacDefault;
    property ScaleFont: TFont read FScaleFont write SetScaleFont;
    property ScaleTextColor: TColor read FScaleTextColor write SetScaleTextColor default clDefault;
  end;

  TdxMapControlNavigationPanel = class(TcxOwnedPersistent)
  private
    FHeight: Integer;
    FMapControl: TdxCustomMapControl;
    FShowCoordinates: Boolean;
    FShowKilometersScale: Boolean;
    FShowMilesScale: Boolean;
    FShowScrollButtons: Boolean;
    FShowZoomTrackBar: Boolean;
    FStyle: TdxMapControlNavigationPanelStyle;
    FVisible: Boolean;
    FXCoordinateDisplayMask: string;
    FYCoordinateDisplayMask: string;
    procedure SetHeight(AValue: Integer);
    procedure SetShowCoordinates(AValue: Boolean);
    procedure SetShowKilometersScale(AValue: Boolean);
    procedure SetShowMilesScale(AValue: Boolean);
    procedure SetShowScrollButtons(AValue: Boolean);
    procedure SetShowZoomTrackBar(AValue: Boolean);
    procedure SetStyle(AValue: TdxMapControlNavigationPanelStyle);
    procedure SetVisible(AValue: Boolean);
    procedure SetXCoordinateDisplayMask(const AValue: string);
    procedure SetYCoordinateDisplayMask(const AValue: string);
  protected
    procedure Changed;
    procedure ChangeScale(M, D: Integer);
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property Height: Integer read FHeight write SetHeight default 80;
    property ShowCoordinates: Boolean read FShowCoordinates write SetShowCoordinates default True;
    property ShowKilometersScale: Boolean read FShowKilometersScale write SetShowKilometersScale default True;
    property ShowMilesScale: Boolean read FShowMilesScale write SetShowMilesScale default True;
    property ShowScrollButtons: Boolean read FShowScrollButtons write SetShowScrollButtons default True;
    property ShowZoomTrackBar: Boolean read FShowZoomTrackBar write SetShowZoomTrackBar default True;
    property Style: TdxMapControlNavigationPanelStyle read FStyle write SetStyle;
    property Visible: Boolean read FVisible write SetVisible default True;
    property XCoordinateDisplayMask: string read FXCoordinateDisplayMask write SetXCoordinateDisplayMask;
    property YCoordinateDisplayMask: string read FYCoordinateDisplayMask write SetYCoordinateDisplayMask;
  end;

  TdxMapControlOptionsBehavior = class(TcxOwnedPersistent)
  private
    FAnimation: Boolean;
    FInertialScrolling: Boolean;
    FMapControl: TdxCustomMapControl;
    FMapItemSelectMode: TdxMapItemSelectMode;
    FScrolling: Boolean;
    FZooming: Boolean;
    procedure Changed;
    procedure SetAnimation(const Value: Boolean);
    procedure SetMapItemSelectMode(const Value: TdxMapItemSelectMode);
    procedure SetScrolling(const Value: Boolean);
    procedure SetZooming(const Value: Boolean);
  protected
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Animation: Boolean read FAnimation write SetAnimation default True;
    property InertialScrolling: Boolean read FInertialScrolling write FInertialScrolling default True;
    property MapItemSelectMode: TdxMapItemSelectMode read FMapItemSelectMode write SetMapItemSelectMode default mismExtended;
    property Scrolling: Boolean read FScrolling write SetScrolling default True;
    property Zooming: Boolean read FZooming write SetZooming default True;
  end;

  TdxMapControlAnimationController = class
  private
    FMapControl: TdxCustomMapControl;
    FIsAsyncMode: Boolean;
    FZoomAnimation: TdxAnimationTransition;
    FZoomAnimationAnchorPoint: TdxPointDouble;
    FZoomAnimationStartLevel: Double;
    FZoomLevel: Double;
    FZoomPositionScale: Double;
    FNewLevel: Double;
    FAnchorPoint: TdxPointDouble;
    FAnimated: Boolean;
    procedure DoZoom(AZoomLevel: Double);
    procedure DoZoomAnimation(Sender: TdxAnimationTransition; var APosition: Integer;
      var AFinished: Boolean);
    procedure DoZoomAnimationTerminate(Sender: TObject);
  public
    constructor Create(AOwner: TdxCustomMapControl); virtual;
    destructor Destroy; override;
    procedure StartZoomAnimation;
    procedure Zoom(ANewZoomValue: Double; const AAnchorPoint: TdxPointDouble;
      AAnimated: Boolean = True; AIsAsyncMode: Boolean = False);
    property ZoomLevel: Double read FZoomLevel;
  end;

  TdxMapControlSelectionController = class
  private
    FMapControl: TdxCustomMapControl;
    FSelection: TdxFastObjectList;
    procedure AddSelection(AItem: TdxMapItem);
    function GetCount: Integer;
    function GetItems(Index: Integer): TdxMapItem;
  public
    constructor Create(AOwner: TdxCustomMapControl); virtual;
    destructor Destroy; override;
    procedure ClearSelection;
    procedure Deselect(AItem: TdxMapItem);
    procedure Select(AItem: TdxMapItem; AShift: TShiftState);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxMapItem read GetItems;
  end;

  TdxMapControlDragAndDropObject = class(TcxDragAndDropObject)
  protected
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function ProcessKeyDown(AKey: Word; AShiftState: TShiftState): Boolean; override;
  end;

  TdxMapControlInertialScrollingController = class
  protected
  {$REGION 'private types'}
    type
      TInertiaCalculator = class
      private
        FCapacity: Integer;
        FItems: TList<TdxPointDouble>;
        function GetCount: Integer;
      public
        constructor Create; virtual;
        destructor Destroy; override;
        procedure AddItem(const AItem: TdxPointDouble);
        procedure Clear;
        function GetValue: TdxPointDouble;
        property Count: Integer read GetCount;
      end;

      TSpeedCalculator = class
      private
        FLastPos: TdxPointDouble;
        FLastTime: TTime;
      public
        function CheckSpeedValue(out AValue: TdxPointDouble): Boolean;
        procedure Prepare;
      end;
  {$ENDREGION}
  private
    FDeceleration: TdxPointDouble;
    FInertiaCalculator: TInertiaCalculator;
    FInertiaTime: Double;
    FMapControl: TdxCustomMapControl;
    FSpeed: TdxPointDouble;
    FSpeedCalculator: TSpeedCalculator;
    FStartCenterPoint: TdxMapControlGeoPoint;
    FStartInertiaTime: TTime;
    FTimer: TcxTimer;
    procedure InertiaTimer(Sender: TObject);
    procedure TrackSpeedTimer(Sender: TObject);
  public
    constructor Create(AOwner: TdxCustomMapControl); virtual;
    destructor Destroy; override;
    procedure CheckSpeedValue;
    procedure StartTrackSpeed;
    procedure StopInertia;
    procedure StopTrackSpeed;
  end;

  TdxMapControlCenterPointChangingEvent = procedure(Sender: TObject; var ANewCenterPoint: TdxMapControlGeoPoint; var AAllow: Boolean) of object;
  TdxMapControlSelectionChangingEvent = procedure(Sender: TObject; AItem: TdxMapItem; var AAllow: Boolean) of object;
  TdxMapControlZoomLevelChangingEvent = procedure(Sender: TObject; ANewZoomLevel: Double; var AAllow: Boolean) of object;

  TdxMapControlDragAction = (mcdaNone, mcdaScroll, mcdaSelect, mcdaZoom);

  TdxCustomMapControl = class(TcxControl, IdxScreenTipProvider)
  private
    FAnimationController: TdxMapControlAnimationController;
    FCenterPoint: TdxMapControlGeoLocation;
    FColor: TColor;
    FController: TdxMapControlController;
    FDragMode: TdxMapControlDragAction;
    FIsViewInfoDirty: Boolean;
    FIsViewPortValid: Boolean;
    FLayers: TdxMapLayers;
    FLoadedZoomLevel: Double;
    FLockCount: Integer;
    FMapItemLayerSelectionController: TdxMapControlSelectionController;
    FNavigationPanel: TdxMapControlNavigationPanel;
    FOptionsBehavior: TdxMapControlOptionsBehavior;
    FPainter: TdxMapControlPainter;
    FSelectedRegionBounds: TRect;
    FShift: TShiftState;
    FStartCenterPoint: TdxMapControlGeoPoint;
    FViewInfo: TdxMapControlViewInfo;
    FZoomLevel: Double;
    FOnCenterPointChanged: TNotifyEvent;
    FOnCenterPointChanging: TdxMapControlCenterPointChangingEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnSelectionChanging: TdxMapControlSelectionChangingEvent;
    FOnZoomLevelChanged: TNotifyEvent;
    FOnZoomLevelChanging: TdxMapControlZoomLevelChangingEvent;
    FInformationProviders: TdxMapControlInformationProviders;
    FInertiaController: TdxMapControlInertialScrollingController;
    function AllowCenterPointChanges(var AValue: TdxMapControlGeoPoint): Boolean;
    function AllowZoomLevelChanges(ALevel: Double): Boolean;
    procedure CenterPointChanged(ASender: TObject);
    procedure CheckZoomLevel(var ALevel: Double);
    procedure DoCenterPointChanged;
    procedure DoZoomLevelChanged;
    function GetActualCenterPoint: TdxMapControlGeoPoint;
    function GetActualZoomLevel: Double;
    function GetBaseLayer: TdxMapLayer;
    function GetHitTest: TdxMapControlHitTest;
    function GetSelectedMapItemCount: Integer;
    function GetSelectedMapItems(Index: Integer): TdxMapItem;
    function IsZoomLevelStored: Boolean;
    procedure SetCenterPoint(AValue: TdxMapControlGeoLocation);
    procedure SetColor(const Value: TColor);
    procedure SetInformationProviders(const Value: TdxMapControlInformationProviders);
    procedure SetLayers(const Value: TdxMapLayers);
    procedure SetNavigationPanel(AValue: TdxMapControlNavigationPanel);
    procedure SetOptionsBehavior(AValue: TdxMapControlOptionsBehavior);
    procedure SetZoomLevel(Value: Double);
    procedure UpdateSelectedRegion(const R: TRect);
  protected
    procedure BoundsChanged; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure DoPaint; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    procedure InitControl; override;
    function IsDoubleBufferedNeeded: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override; // to do
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;

    // IdxScreenTipProvider
    function IdxScreenTipProvider.GetAction = GetMapAction;
    function GetMapAction: TBasicAction;
    function GetScreenTip: TdxScreenTip;
    function GetShortCut: string;

    function AllowSelectionChanging(AItem: TdxMapItem): Boolean;
    procedure CalculateZoomAndCenterPointForRegion(const ARect: TdxRectDouble;
      const APaddingFactor: Double; out AZoomLevel: Double; out ACenterPoint: TdxMapControlGeoPoint);
    function CanAnimate: Boolean;
    function CanInertialScrolling: Boolean;
    function CanScroll: Boolean;
    function CanSelect: Boolean;
    function CanZoom: Boolean;
    procedure Changed;
    procedure CheckChanges;
    procedure CheckViewPort;
    procedure DoSelectionChanged;
    function GetKilometersScale(ACenterPoint: TdxMapControlGeoPoint; AScreenDistance: Double): Double;
    function GetMaxZoomLevel: Integer;
    procedure InvalidateViewPort;
    procedure LayersChanged(Sender: TObject; AItem: TcxComponentCollectionItem;
      AAction: TcxComponentCollectionNotification); virtual;
    function ScreenPointToGeoPoint(APoint: TdxPointDouble): TdxMapControlGeoPoint;
    procedure ScrollMap(X, Y: Integer);
    procedure SelectMapItemsInRegion(const ARect: TRect);
    procedure UpdateViewPort;
    procedure Zoom(AZoomLevel: Double; const AAnchorPoint: TdxPointDouble; AAnimated: Boolean;
      AIsAsyncMode: Boolean = False); overload;
    procedure ZoomToRegion(const ARect: TRect); overload;
    procedure ZoomToRegion(const ARect: TdxRectDouble; const APaddingFactor: Double = dxMapControlDefaultZoomPaddingFactor); overload;

    function CreateAnimationController: TdxMapControlAnimationController; virtual;
    function CreateController: TdxMapControlController; virtual;
    function CreateMapItemsLayerSelectionController: TdxMapControlSelectionController; virtual;
    function CreateNavigationPanelOptions: TdxMapControlNavigationPanel; virtual;
    function CreateOptionsBehavior: TdxMapControlOptionsBehavior; virtual;
    procedure CreateSubclasses;
    procedure DestroySubclasses;
    function CreatePainter: TdxMapControlPainter; virtual;
    function CreateViewInfo: TdxMapControlViewInfo; virtual;
    procedure DoSetCenterPoint(const AValue: TdxMapControlGeoPoint; AIsUserAction: Boolean = False);

    property Controller: TdxMapControlController read FController;
    property Painter: TdxMapControlPainter read FPainter;
    property ViewInfo: TdxMapControlViewInfo read FViewInfo;

    property SelectedRegionBounds: TRect read FSelectedRegionBounds;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    // IdxLocalizerListener
    procedure TranslationChanged; override;

    function AddImageTileLayer: TdxMapImageTileLayer;
    function AddItemFileLayer: TdxMapItemFileLayer;
    function AddItemLayer: TdxMapItemLayer;
    procedure BeginUpdate;
    procedure ClearSelection;
    procedure Deselect(AMapItem: TdxMapItem);
    procedure EndUpdate;
    function IsLocked: Boolean;
    procedure Select(AMapItem: TdxMapItem; AShiftState: TShiftState = [];
      ACheckSelectionMode: Boolean = True);
    procedure Zoom(AZoomLevel: Double; AAnimated: Boolean); overload;
    procedure ZoomAsync(AZoomLevel: Double);
    procedure ZoomIn;
    procedure ZoomOut;
    procedure ZoomToFitLayerItems(ALayers: TdxCustomMapItemLayerList;
      const APaddingFactor: Double = dxMapControlDefaultZoomPaddingFactor);
    procedure ZoomToFitItems(AItems: TdxMapItemList; const APaddingFactor: Double = dxMapControlDefaultZoomPaddingFactor);
    procedure ZoomToGeoRect(const ARect: TdxMapControlGeoRect); overload;
    procedure ZoomToGeoRect(const ARect: TdxMapControlGeoRect; const APaddingFactor: Double); overload;

    property ActualCenterPoint: TdxMapControlGeoPoint read GetActualCenterPoint;
    property ActualZoomLevel: Double read GetActualZoomLevel;
    property CenterPoint: TdxMapControlGeoLocation read FCenterPoint write SetCenterPoint;
    property Color: TColor read FColor write SetColor default clDefault;
    property HitTest: TdxMapControlHitTest read GetHitTest;
    property InformationProviders: TdxMapControlInformationProviders read FInformationProviders write SetInformationProviders;
    property Layers: TdxMapLayers read FLayers write SetLayers;
    property NavigationPanel: TdxMapControlNavigationPanel read FNavigationPanel write SetNavigationPanel;
    property OptionsBehavior: TdxMapControlOptionsBehavior read FOptionsBehavior write SetOptionsBehavior;
    property SelectedMapItemCount: Integer read GetSelectedMapItemCount;
    property SelectedMapItems[Index: Integer]: TdxMapItem read GetSelectedMapItems;
    property ZoomLevel: Double read FZoomLevel write SetZoomLevel stored IsZoomLevelStored;
    property OnCenterPointChanged: TNotifyEvent read FOnCenterPointChanged write FOnCenterPointChanged;
    property OnCenterPointChanging: TdxMapControlCenterPointChangingEvent read FOnCenterPointChanging write FOnCenterPointChanging;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnSelectionChanging: TdxMapControlSelectionChangingEvent read FOnSelectionChanging write FOnSelectionChanging;
    property OnZoomLevelChanged: TNotifyEvent read FOnZoomLevelChanged write FOnZoomLevelChanged;
    property OnZoomLevelChanging: TdxMapControlZoomLevelChangingEvent read FOnZoomLevelChanging write FOnZoomLevelChanging;
  end;

  TdxMapControl = class(TdxCustomMapControl)
  published
    property Align;
    property Anchors;
    property BorderStyle default cxcbsDefault;
    property CenterPoint;
    property Color;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property InformationProviders;
    property Layers;
    property LookAndFeel;
    property NavigationPanel;
    property OptionsBehavior;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property ZoomLevel;
    property OnCanResize;
    property OnCenterPointChanged;
    property OnCenterPointChanging;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnSelectionChanged;
    property OnSelectionChanging;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnZoomLevelChanged;
    property OnZoomLevelChanging;
  end;

implementation

uses
  dxDPIAwareUtils;

type
  TdxMapLayersAccess = class(TdxMapLayers);

const
  dxMapControlMaxZoomLevel = 20;
  dxMapControlZoomAnimationInterval = 500;
  dxMapControlZoomAnimationFrameCount = 100;

{ TdxMapControlNavigationPanelOptions }

constructor TdxMapControlNavigationPanelStyle.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FMapControl := (AOwner as TdxMapControlNavigationPanel).FMapControl;
  FColor := dxacDefault;
  FCoordinateFont := TFont.Create;
  FCoordinateFont.Height := dxGetFontHeightForDefaultDPI(16);
  FCoordinateFont.OnChange := FontChanged;
  FCoordinateTextColor := clDefault;
  FElementColor := dxacDefault;
  FElementHotColor := dxacDefault;
  FElementPressedColor := dxacDefault;
  FScaleFont := TFont.Create;
  FScaleFont.Height := dxGetFontHeightForDefaultDPI(12);
  FScaleFont.OnChange := FontChanged;
  FScaleTextColor := clDefault;
end;

destructor TdxMapControlNavigationPanelStyle.Destroy;
begin
  FreeAndNil(FScaleFont);
  FreeAndNil(FCoordinateFont);
  inherited;
end;

procedure TdxMapControlNavigationPanelStyle.Changed;
begin
  FMapControl.Changed;
end;

procedure TdxMapControlNavigationPanelStyle.ChangeScale(M, D: Integer);
begin
  CoordinateFont.Height := MulDiv(CoordinateFont.Height, M, D);
  ScaleFont.Height := MulDiv(ScaleFont.Height, M, D);
end;

procedure TdxMapControlNavigationPanelStyle.DoAssign(Source: TPersistent);
var
  ANavigationPanelStyle: TdxMapControlNavigationPanelStyle;
begin
  inherited;
  if Source is TdxMapControlNavigationPanelStyle then
  begin
    FMapControl.BeginUpdate;
    try
      ANavigationPanelStyle := TdxMapControlNavigationPanelStyle(Source);
      FColor := ANavigationPanelStyle.Color;
      CoordinateFont := ANavigationPanelStyle.CoordinateFont;
      FCoordinateTextColor := ANavigationPanelStyle.CoordinateTextColor;
      FElementColor := ANavigationPanelStyle.ElementColor;
      FElementHotColor := ANavigationPanelStyle.ElementHotColor;
      FElementPressedColor := ANavigationPanelStyle.ElementPressedColor;
      ScaleFont := ANavigationPanelStyle.ScaleFont;
      ScaleTextColor := ANavigationPanelStyle.ScaleTextColor;
    finally
      FMapControl.EndUpdate;
    end;
  end;
end;

procedure TdxMapControlNavigationPanelStyle.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TdxMapControlNavigationPanelStyle.SetColor(Value: TdxAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FMapControl.Invalidate;
  end;
end;

procedure TdxMapControlNavigationPanelStyle.SetCoordinateFont(Value: TFont);
begin
  FCoordinateFont.Assign(Value);
end;

procedure TdxMapControlNavigationPanelStyle.SetCoordinateTextColor(Value: TColor);
begin
  if FCoordinateTextColor <> Value then
  begin
    FCoordinateTextColor := Value;
    FMapControl.Invalidate;
  end;
end;

procedure TdxMapControlNavigationPanelStyle.SetElementColor(Value: TdxAlphaColor);
begin
  if FElementColor <> Value then
  begin
    FElementColor := Value;
    FMapControl.Invalidate;
  end;
end;

procedure TdxMapControlNavigationPanelStyle.SetElementHotColor(Value: TdxAlphaColor);
begin
  if FElementHotColor <> Value then
  begin
    FElementHotColor := Value;
    FMapControl.Invalidate;
  end;
end;

procedure TdxMapControlNavigationPanelStyle.SetElementPressedColor(Value: TdxAlphaColor);
begin
  if FElementPressedColor <> Value then
  begin
    FElementPressedColor := Value;
    FMapControl.Invalidate;
  end;
end;

procedure TdxMapControlNavigationPanelStyle.SetScaleFont(Value: TFont);
begin
  FScaleFont.Assign(Value);
end;

procedure TdxMapControlNavigationPanelStyle.SetScaleTextColor(Value: TColor);
begin
  if FScaleTextColor <> Value then
  begin
    FScaleTextColor := Value;
    FMapControl.Invalidate;
  end;
end;

{ TdxMapControlNavigationPanelOptions }

constructor TdxMapControlNavigationPanel.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FMapControl := AOwner as TdxCustomMapControl;
  FHeight := 80;
  FStyle := TdxMapControlNavigationPanelStyle.Create(Self);
  FShowCoordinates := True;
  FShowKilometersScale := True;
  FShowMilesScale := True;
  FShowScrollButtons := True;
  FShowZoomTrackBar := True;
  FVisible := True;
end;

destructor TdxMapControlNavigationPanel.Destroy;
begin
  FreeAndNil(FStyle);
  inherited Destroy;
end;

procedure TdxMapControlNavigationPanel.Changed;
begin
  FMapControl.Changed;
end;

procedure TdxMapControlNavigationPanel.ChangeScale(M, D: Integer);
begin
  Height := MulDiv(Height, M, D);
  Style.ChangeScale(M, D);
end;

procedure TdxMapControlNavigationPanel.DoAssign(Source: TPersistent);
var
  ANavigationPanelOptions: TdxMapControlNavigationPanel;
begin
  inherited;
  if Source is TdxMapControlNavigationPanel then
  begin
    ANavigationPanelOptions := TdxMapControlNavigationPanel(Source);
    FMapControl.BeginUpdate;
    try
      FHeight := ANavigationPanelOptions.Height;
      FShowCoordinates := ANavigationPanelOptions.ShowCoordinates;
      FShowKilometersScale := ANavigationPanelOptions.ShowKilometersScale;
      FShowMilesScale := ANavigationPanelOptions.ShowMilesScale ;
      FShowScrollButtons := ANavigationPanelOptions.ShowScrollButtons;
      FShowZoomTrackBar := ANavigationPanelOptions.ShowZoomTrackBar;
      FVisible := ANavigationPanelOptions.Visible;
      Style := ANavigationPanelOptions.Style;
    finally
      FMapControl.EndUpdate;
    end;
  end;
end;

procedure TdxMapControlNavigationPanel.SetHeight(AValue: Integer);
begin
  if FHeight <> AValue then
  begin
    FHeight := AValue;
    Changed;
  end;
end;

procedure TdxMapControlNavigationPanel.SetShowCoordinates(AValue: Boolean);
begin
  if FShowCoordinates <> AValue then
  begin
    FShowCoordinates := AValue;
    Changed;
  end;
end;

procedure TdxMapControlNavigationPanel.SetShowKilometersScale(AValue: Boolean);
begin
  if FShowKilometersScale <> AValue then
  begin
    FShowKilometersScale := AValue;
    Changed;
  end;
end;

procedure TdxMapControlNavigationPanel.SetShowMilesScale(AValue: Boolean);
begin
  if FShowMilesScale <> AValue then
  begin
    FShowMilesScale := AValue;
    Changed;
  end;
end;

procedure TdxMapControlNavigationPanel.SetShowScrollButtons(AValue: Boolean);
begin
  if FShowScrollButtons <> AValue then
  begin
    FShowScrollButtons := AValue;
    Changed;
  end;
end;

procedure TdxMapControlNavigationPanel.SetShowZoomTrackBar(AValue: Boolean);
begin
  if FShowZoomTrackBar <> AValue then
  begin
    FShowZoomTrackBar := AValue;
    Changed;
  end;
end;

procedure TdxMapControlNavigationPanel.SetStyle(AValue: TdxMapControlNavigationPanelStyle);
begin
  FStyle.Assign(AValue);
end;

procedure TdxMapControlNavigationPanel.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed;
  end;
end;

procedure TdxMapControlNavigationPanel.SetXCoordinateDisplayMask(const AValue: string);
begin
  if FXCoordinateDisplayMask <> AValue then
  begin
    FXCoordinateDisplayMask := AValue;
    Changed;
  end;
end;

procedure TdxMapControlNavigationPanel.SetYCoordinateDisplayMask(const AValue: string);
begin
  if FYCoordinateDisplayMask <> AValue then
  begin
    FYCoordinateDisplayMask := AValue;
    Changed;
  end;
end;

{ TdxMapControlOptionsBehavior }

constructor TdxMapControlOptionsBehavior.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FMapControl := AOwner as TdxCustomMapControl;
  FAnimation := True;
  FScrolling := True;
  FMapItemSelectMode := mismExtended;
  FZooming := True;
  FInertialScrolling := True;
end;

procedure TdxMapControlOptionsBehavior.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxMapControlOptionsBehavior then
  begin
    FAnimation := TdxMapControlOptionsBehavior(Source).Animation;
    FInertialScrolling := TdxMapControlOptionsBehavior(Source).InertialScrolling;
    FMapItemSelectMode := TdxMapControlOptionsBehavior(Source).MapItemSelectMode;
    FScrolling := TdxMapControlOptionsBehavior(Source).Scrolling;
    FZooming := TdxMapControlOptionsBehavior(Source).Zooming;
    Changed;
  end;
end;

procedure TdxMapControlOptionsBehavior.Changed;
begin
  FMapControl.Changed;
end;

procedure TdxMapControlOptionsBehavior.SetAnimation(const Value: Boolean);
begin
  FAnimation := Value;
end;

procedure TdxMapControlOptionsBehavior.SetMapItemSelectMode(const Value: TdxMapItemSelectMode);
begin
  if Value <> FMapItemSelectMode then
  begin
    FMapItemSelectMode := Value;
    FMapControl.ClearSelection;
  end;
end;

procedure TdxMapControlOptionsBehavior.SetScrolling(const Value: Boolean);
begin
  if FScrolling <> Value then
  begin
    FScrolling := Value;
    Changed;
  end;
end;

procedure TdxMapControlOptionsBehavior.SetZooming(const Value: Boolean);
begin
  if FZooming <> Value then
  begin
    FZooming := Value;
    Changed;
  end;
end;

{ TdxMapControlAnimationController }

constructor TdxMapControlAnimationController.Create(AOwner: TdxCustomMapControl);
begin
  inherited Create;
  FMapControl := AOwner;
  FZoomLevel := 1;
end;

destructor TdxMapControlAnimationController.Destroy;
begin
  FreeAndNil(FZoomAnimation);
  inherited;
end;

procedure TdxMapControlAnimationController.StartZoomAnimation;
begin
  FZoomAnimationAnchorPoint := FAnchorPoint;
  FZoomAnimationStartLevel := FZoomLevel;
  FZoomPositionScale := (FNewLevel - FZoomLevel) / dxMapControlZoomAnimationFrameCount;
  FreeAndNil(FZoomAnimation);
  FZoomAnimation := TdxAnimationTransition.Create(dxMapControlZoomAnimationInterval,
    ateAccelerateDecelerate, dxMapControlZoomAnimationFrameCount);
  FZoomAnimation.FreeOnTerminate := False;
  FZoomAnimation.OnAnimate := DoZoomAnimation;
  if FIsAsyncMode then
  begin
    FZoomAnimation.OnTerminate := DoZoomAnimationTerminate;
    FZoomAnimation.Resume;
  end
  else
    FZoomAnimation.ImmediateAnimation;
end;

procedure TdxMapControlAnimationController.Zoom(ANewZoomValue: Double; const AAnchorPoint: TdxPointDouble;
  AAnimated: Boolean = True; AIsAsyncMode: Boolean = False);
begin
  FNewLevel := ANewZoomValue;
  FAnchorPoint := AAnchorPoint;
  FAnimated := AAnimated;
  FIsAsyncMode := AIsAsyncMode;
  if FAnimated then
    StartZoomAnimation
  else
    DoZoom(FNewLevel);
end;

procedure TdxMapControlAnimationController.DoZoom(AZoomLevel: Double);

  procedure UpdateZoomLevel;
  begin
    FZoomLevel := AZoomLevel;
    FMapControl.InvalidateViewPort;
  end;

var
  ANewCenterPoint: TdxMapControlGeoPoint;
begin
  FMapControl.CheckZoomLevel(AZoomLevel);
  if FZoomLevel <> AZoomLevel then
  begin
    FMapControl.BeginUpdate;
    try
      if FMapControl.GetBaseLayer <> nil then
        ANewCenterPoint := FMapControl.GetBaseLayer.MoveAndZoom(FMapControl.CenterPoint.GeoPoint,
          FAnchorPoint, FZoomLevel, AZoomLevel)
      else
        ANewCenterPoint := FMapControl.CenterPoint.GeoPoint;
      UpdateZoomLevel;
      FMapControl.DoSetCenterPoint(ANewCenterPoint, True);
      FMapControl.CheckViewPort;
    finally
      FMapControl.EndUpdate;
    end;
    if not FIsAsyncMode and not FMapControl.IsLocked then
      FMapControl.Update;
  end;
end;

procedure TdxMapControlAnimationController.DoZoomAnimation(Sender: TdxAnimationTransition; var APosition: Integer;
  var AFinished: Boolean);
begin
  DoZoom(FZoomAnimationStartLevel + APosition * FZoomPositionScale);
end;

procedure TdxMapControlAnimationController.DoZoomAnimationTerminate(Sender: TObject);
begin
  FMapControl.DoZoomLevelChanged;
end;

{ TdxCustomMapItemLayerSelectionController }

constructor TdxMapControlSelectionController.Create(AOwner: TdxCustomMapControl);
begin
  inherited Create;
  FMapControl := AOwner;
  FSelection := TdxFastObjectList.Create(False);
end;

destructor TdxMapControlSelectionController.Destroy;
begin
  FreeAndNil(FSelection);
  inherited;
end;

procedure TdxMapControlSelectionController.ClearSelection;
var
  I: Integer;
begin
  for I := FSelection.Count - 1 downto 0 do
    Deselect(FSelection[I] as TdxMapItem);
end;

procedure TdxMapControlSelectionController.Deselect(AItem: TdxMapItem);
begin
  if (FSelection.IndexOf(AItem) <> -1) and not FMapControl.AllowSelectionChanging(AItem) then
    Exit;
  AItem.ViewInfo.State := mcesNormal;
  FSelection.Remove(AItem);
  FMapControl.DoSelectionChanged;
end;

procedure TdxMapControlSelectionController.Select(AItem: TdxMapItem; AShift: TShiftState);
begin
  if ssCtrl in AShift then
    if FSelection.IndexOf(AItem) = -1 then
      AddSelection(AItem)
    else
      Deselect(AItem)
  else
    if FSelection.IndexOf(AItem) = -1 then
    begin
      ClearSelection;
      AddSelection(AItem);
    end;
end;

procedure TdxMapControlSelectionController.AddSelection(AItem: TdxMapItem);
begin
  if not FMapControl.AllowSelectionChanging(AItem) then
    Exit;
  AItem.ViewInfo.State := mcesSelected;
  FSelection.Add(AItem);
  FMapControl.DoSelectionChanged;
end;

function TdxMapControlSelectionController.GetCount: Integer;
begin
  Result := FSelection.Count;
end;

function TdxMapControlSelectionController.GetItems(Index: Integer): TdxMapItem;
begin
  Result := FSelection[Index] as TdxMapItem;
end;

{ TdxMapControlDragAndDropObject }

function TdxMapControlDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := crSizeAll
  else
    Result := inherited GetDragAndDropCursor(Accepted);
end;

function TdxMapControlDragAndDropObject.ProcessKeyDown(AKey: Word; AShiftState: TShiftState): Boolean;
begin
  Result := AKey in [VK_SHIFT, VK_CONTROL];
end;

{ TdxMapControlInertialScrollingController }

constructor TdxMapControlInertialScrollingController.Create(AOwner: TdxCustomMapControl);
begin
  inherited Create;
  FMapControl := AOwner;
  FInertiaCalculator := TInertiaCalculator.Create;
  FSpeedCalculator := TSpeedCalculator.Create;
  FTimer := TcxTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 10;
end;

destructor TdxMapControlInertialScrollingController.Destroy;
begin
  FreeAndNil(FTimer);
  FreeAndNil(FSpeedCalculator);
  FreeAndNil(FInertiaCalculator);
  inherited Destroy;
end;

procedure TdxMapControlInertialScrollingController.CheckSpeedValue;
var
  ASpeedValue: TdxPointDouble;
begin
  FTimer.Enabled := False;
  if FSpeedCalculator.CheckSpeedValue(ASpeedValue) then
    FInertiaCalculator.AddItem(ASpeedValue);
  FTimer.Enabled := True;
end;

procedure TdxMapControlInertialScrollingController.StartTrackSpeed;
begin
  FTimer.Interval := 10;
  FInertiaCalculator.Clear;
  FSpeedCalculator.Prepare;
  FTimer.OnTimer := TrackSpeedTimer;
  FTimer.Enabled := True;
end;

procedure TdxMapControlInertialScrollingController.StopInertia;
begin
  FTimer.Enabled := False;
end;

procedure TdxMapControlInertialScrollingController.StopTrackSpeed;
var
  ASpeed: Double;
begin
  FTimer.Enabled := False;
  FSpeed := FInertiaCalculator.GetValue;
  ASpeed := Sqrt(Sqr(FSpeed.X) + Sqr(FSpeed.Y));
  if ASpeed > 0 then
  begin
    FDeceleration.X := - dxMapControlInertialScrollingDecelerationRatio * FSpeed.X / ASpeed;
    FDeceleration.Y := - dxMapControlInertialScrollingDecelerationRatio * FSpeed.Y / ASpeed;
    FInertiaTime := ASpeed / dxMapControlInertialScrollingDecelerationRatio;
    FStartCenterPoint := FMapControl.CenterPoint.GeoPoint;
    FTimer.Interval := 10;
    FTimer.OnTimer := InertiaTimer;
    FStartInertiaTime := Time;
    FTimer.Enabled := True;
  end;
end;

procedure TdxMapControlInertialScrollingController.InertiaTimer(Sender: TObject);
var
  ATimeDelta: Double;
  AOffset: TPoint;
  ANewGeoPoint: TdxMapControlGeoPoint;
begin
  FTimer.Enabled := False;
  ATimeDelta := MilliSecondsBetween(Time, FStartInertiaTime) / 1000;
  if ATimeDelta < FInertiaTime then
  begin
    AOffset.X := -Round(FSpeed.X * ATimeDelta + FDeceleration.X * Sqr(ATimeDelta) / 2);
    AOffset.Y := -Round(FSpeed.Y * ATimeDelta + FDeceleration.Y * Sqr(ATimeDelta) / 2);
    ANewGeoPoint := FMapControl.GetBaseLayer.Move(FStartCenterPoint, AOffset);
    FMapControl.DoSetCenterPoint(ANewGeoPoint, True);
    FTimer.Enabled := True;
  end;
end;

procedure TdxMapControlInertialScrollingController.TrackSpeedTimer(Sender: TObject);
begin
  CheckSpeedValue;
end;

{ TdxMapControlInertialScrollingController.TInertiaCalculator }

constructor TdxMapControlInertialScrollingController.TInertiaCalculator.Create;
begin
  inherited Create;
  FItems := TList<TdxPointDouble>.Create;
  FCapacity := 5;
end;

destructor TdxMapControlInertialScrollingController.TInertiaCalculator.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxMapControlInertialScrollingController.TInertiaCalculator.AddItem(const AItem: TdxPointDouble);
begin
  FItems.Add(AItem);
  if Count > FCapacity then
    FItems.Delete(0);
end;

procedure TdxMapControlInertialScrollingController.TInertiaCalculator.Clear;
begin
  FItems.Clear;
end;

function TdxMapControlInertialScrollingController.TInertiaCalculator.GetValue: TdxPointDouble;
var
  AValue: TdxPointDouble;
begin
  Result := dxNullPointDouble;
  for AValue in FItems do
  begin
    Result.X := Result.X + AValue.X;
    Result.Y := Result.Y + AValue.Y;
  end;
  if Count > 0 then
  begin
    Result.X := Result.X / Count;
    Result.Y := Result.Y / Count;
  end;
end;

function TdxMapControlInertialScrollingController.TInertiaCalculator.GetCount: Integer;
begin
  Result := FItems.Count;
end;

{ TdxMapControlInertialScrollingController.TSpeedCalculator }

function TdxMapControlInertialScrollingController.TSpeedCalculator.CheckSpeedValue(out AValue: TdxPointDouble): Boolean;
var
  ANewPos: TdxPointDouble;
  ADelta: Double;
  ACurrentTime: TTime;
begin
  AValue := dxNullPointDouble;
  ACurrentTime := Time;
  ADelta := MilliSecondsBetween(ACurrentTime, FLastTime) / 1000;
  Result := ADelta > 0;
  if Result then
  begin
    ANewPos := dxPointDouble(GetMouseCursorPos);
    AValue.X := (ANewPos.X - FLastPos.X) / ADelta;
    AValue.Y := (ANewPos.Y - FLastPos.Y) / ADelta;
    FLastPos := ANewPos;
  end;
  FLastTime := ACurrentTime;
end;

procedure TdxMapControlInertialScrollingController.TSpeedCalculator.Prepare;
begin
  FLastTime := Time;
  FLastPos :=  dxPointDouble(GetMouseCursorPos);
end;

{ TdxMapControl }

constructor TdxCustomMapControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsViewInfoDirty := True;
  FZoomLevel := 1;
  FColor := clDefault;
  FCenterPoint := TdxMapControlGeoLocation.Create(Self);
  FCenterPoint.OnChanged := CenterPointChanged;
  CreateSubclasses;
  BorderStyle := cxcbsDefault;
  Width := 400;
  Height := 200;
  Keys := [kArrows];
end;

destructor TdxCustomMapControl.Destroy;
begin
  DestroySubclasses;
  FreeAndNil(FCenterPoint);
  inherited;
end;

function TdxCustomMapControl.AddImageTileLayer: TdxMapImageTileLayer;
begin
  Result := Layers.Add(TdxMapImageTileLayer) as TdxMapImageTileLayer;
end;

function TdxCustomMapControl.AddItemFileLayer: TdxMapItemFileLayer;
begin
  Result := Layers.Add(TdxMapItemFileLayer) as TdxMapItemFileLayer;
end;

function TdxCustomMapControl.AddItemLayer: TdxMapItemLayer;
begin
  Result := Layers.Add(TdxMapItemLayer) as TdxMapItemLayer;
end;

function TdxCustomMapControl.AllowCenterPointChanges(var AValue: TdxMapControlGeoPoint): Boolean;
begin
  Result := True;
  if Assigned(FOnCenterPointChanging) then
    FOnCenterPointChanging(Self, AValue, Result);
end;

procedure TdxCustomMapControl.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxCustomMapControl.ClearSelection;
begin
  FMapItemLayerSelectionController.ClearSelection;
end;

procedure TdxCustomMapControl.Deselect(AMapItem: TdxMapItem);
begin
  FMapItemLayerSelectionController.Deselect(AMapItem);
end;

procedure TdxCustomMapControl.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Changed;
end;

function TdxCustomMapControl.IsLocked: Boolean;
begin
  Result := (FLockCount > 0) or IsLoading or IsDestroying or not HandleAllocated;
end;

procedure TdxCustomMapControl.Select(AMapItem: TdxMapItem; AShiftState: TShiftState = [];
  ACheckSelectionMode: Boolean = True);
begin
  if not ACheckSelectionMode or CanSelect then
  begin
    if ACheckSelectionMode and (OptionsBehavior.MapItemSelectMode = mismSingle) then
      AShiftState := [];
    FMapItemLayerSelectionController.Select(AMapItem, AShiftState);
  end;
end;

procedure TdxCustomMapControl.Zoom(AZoomLevel: Double; AAnimated: Boolean);
begin
  Zoom(AZoomLevel, dxPointDouble(Width / 2, Height / 2), AAnimated);
end;

procedure TdxCustomMapControl.ZoomAsync(AZoomLevel: Double);
begin
  Zoom(AZoomLevel, dxPointDouble(Width / 2, Height / 2), True, True);
end;

procedure TdxCustomMapControl.ZoomIn;
begin
  ZoomLevel := ZoomLevel + 1;
end;

procedure TdxCustomMapControl.ZoomOut;
begin
  ZoomLevel := ZoomLevel - 1;
end;

procedure TdxCustomMapControl.ZoomToFitItems(AItems: TdxMapItemList;
  const APaddingFactor: Double = dxMapControlDefaultZoomPaddingFactor);
var
  ARect: TdxRectDouble;
begin
  if TdxMapItemsZoomHelper.CalculateMapBounds(AItems, ARect) then
    ZoomToRegion(ARect, APaddingFactor);
end;

procedure TdxCustomMapControl.ZoomToFitLayerItems(ALayers: TdxCustomMapItemLayerList;
  const APaddingFactor: Double = dxMapControlDefaultZoomPaddingFactor);
var
  I, J: Integer;
  AItems: TdxMapItemList;
begin
  AItems := TdxMapItemList.Create;
  try
    for I := 0 to ALayers.Count - 1 do
      if ALayers[I].IsActuallyVisible then
        for J := 0 to ALayers[I].MapItems.Count - 1 do
          AItems.Add(ALayers[I].MapItems[J]);
    ZoomToFitItems(AItems, APaddingFactor);
  finally
    AItems.Free;
  end;
end;

procedure TdxCustomMapControl.ZoomToGeoRect(const ARect: TdxMapControlGeoRect);
begin
  if (GetBaseLayer <> nil) and ARect.IsValid then
    BeginUpdate;
    try
      ZoomToGeoRect(ARect, 0);
      ZoomLevel := Trunc(ZoomLevel);
    finally
      EndUpdate;
    end;
end;

procedure TdxCustomMapControl.ZoomToGeoRect(const ARect: TdxMapControlGeoRect; const APaddingFactor: Double);
var
  ALayer: TdxMapLayer;
begin
  ALayer := GetBaseLayer;
  if (ALayer <> nil) and ARect.IsValid then
    ZoomToRegion(dxRectDouble(ALayer.GeoPointToMapUnit(ARect.TopLeft),
      ALayer.GeoPointToMapUnit(ARect.BottomRight)), APaddingFactor);
end;

procedure TdxCustomMapControl.BoundsChanged;
begin
  inherited BoundsChanged;
  UpdateViewPort;
  Changed;
end;

procedure TdxCustomMapControl.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  BeginUpdate;
  try
    NavigationPanel.ChangeScale(M, D);
    TdxMapLayersAccess(Layers).ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

function TdxCustomMapControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result and (WheelDelta <> 0) and CanZoom and (DragAndDropState = ddsNone) then
  begin
    Zoom(ZoomLevel + WheelDelta / WHEEL_DELTA, dxPointDouble(ScreenToClient(MousePos)), CanAnimate, True);
    Result := True;
   end;
end;

procedure TdxCustomMapControl.DoPaint;
begin
  inherited DoPaint;
  ViewInfo.Paint(Canvas);
end;

procedure TdxCustomMapControl.DragAndDrop(const P: TPoint; var Accepted: Boolean);
var
  AOffset: TPoint;
  ASelectedRegion: TRect;
  ANewGeoPoint: TdxMapControlGeoPoint;
begin
  Accepted := (FDragMode = mcdaScroll) and CanScroll or (FDragMode in [mcdaSelect, mcdaZoom]);
  inherited DragAndDrop(P, Accepted);
  if Accepted then
  begin
    AOffset := Point(MouseDownPos.X - P.X, MouseDownPos.Y - P.Y);
    case FDragMode of
      mcdaScroll:
      begin
        if GetBaseLayer <> nil then
        begin
          ANewGeoPoint := GetBaseLayer.Move(FStartCenterPoint, AOffset);
          DoSetCenterPoint(ANewGeoPoint, True);
          Update;
          if CanInertialScrolling then
            FInertiaController.CheckSpeedValue;
        end;
      end
    else
      cxRectIntersect(ASelectedRegion, cxPointsBox([MouseDownPos, P]), ClientBounds);
      UpdateSelectedRegion(ASelectedRegion);
    end;
  end;
end;

procedure TdxCustomMapControl.EndDragAndDrop(Accepted: Boolean);
var
  ASelectedRegion: TRect;
  ADragMode: TdxMapControlDragAction;
begin
  inherited;
  ASelectedRegion := FSelectedRegionBounds;
  ADragMode := FDragMode;
  UpdateSelectedRegion(cxEmptyRect);
  FDragMode := mcdaNone;
  if ADragMode = mcdaSelect then
    SelectMapItemsInRegion(ASelectedRegion)
  else
    if ADragMode = mcdaZoom then
      ZoomToRegion(ASelectedRegion)
    else // ADragMode = mcdaScroll
      if (GetBaseLayer <> nil) and CanInertialScrolling then
        FInertiaController.StopTrackSpeed;
end;

function TdxCustomMapControl.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := TdxMapControlDragAndDropObject;
end;

procedure TdxCustomMapControl.InitControl;
begin
  inherited InitControl;
  CheckViewPort;
  CheckChanges;
end;

function TdxCustomMapControl.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

procedure TdxCustomMapControl.KeyDown(var Key: Word; Shift: TShiftState);
const
  AStep = 50;

  procedure ScrollMapByKeyboard;
  begin
    if GetKeyState(VK_LEFT) < 0 then
      ScrollMap(-AStep, 0);
    if GetKeyState(VK_RIGHT) < 0 then
      ScrollMap(AStep, 0);
    if GetKeyState(VK_UP) < 0 then
      ScrollMap(0, -AStep);
    if GetKeyState(VK_DOWN) < 0 then
      ScrollMap(0, AStep);
  end;

begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN:
      if CanScroll then
        ScrollMapByKeyboard;
    VK_ADD:
      if CanZoom then
        ZoomIn;
    VK_SUBTRACT:
      if CanZoom then
        ZoomOut;
  end;
end;

procedure TdxCustomMapControl.Loaded;
begin
  inherited Loaded;
  UpdateViewPort;
  ZoomLevel := FLoadedZoomLevel;
  CheckChanges;
end;

procedure TdxCustomMapControl.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  ViewInfo.ClearCache;
  Changed;
end;

procedure TdxCustomMapControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FInertiaController.StopInertia;
  FShift := Shift;
  inherited;
  FController.MouseDown(Button, Shift, X, Y);
end;

procedure TdxCustomMapControl.MouseLeave(AControl: TControl);
begin
  inherited;
  FController.MouseLeave;
end;

procedure TdxCustomMapControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  FController.MouseMove(Shift, X, Y);
end;

procedure TdxCustomMapControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FController.MouseUp(Button, Shift, X, Y);
end;

function TdxCustomMapControl.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := not Controller.HitTest.HitAtNavigationPanel and (GetBaseLayer <> nil);
  if Result then
  begin
    if (FOptionsBehavior.MapItemSelectMode = mismExtended) and
      (FShift * [ssShift, ssCtrl] = [ssShift]) then
      FDragMode := mcdaSelect
    else
      if CanZoom and (FShift * [ssShift, ssCtrl] = [ssShift, ssCtrl]) then
        FDragMode := mcdaZoom
      else
        if CanScroll then
        begin
          FStartCenterPoint := CenterPoint.GeoPoint;
          FDragMode := mcdaScroll;
          if CanInertialScrolling then
            FInertiaController.StartTrackSpeed;
        end
        else
          Result := False;
  end;
end;

// IdxScreenTipProvider
function TdxCustomMapControl.GetMapAction: TBasicAction;
begin
  Result := nil;
end;

function TdxCustomMapControl.GetScreenTip: TdxScreenTip;
begin
  Result := Controller.GetScreenTip;
end;

function TdxCustomMapControl.GetShortCut: string;
begin
  Result := '';
end;

function TdxCustomMapControl.AllowSelectionChanging(AItem: TdxMapItem): Boolean;
begin
  Result := True;
  if Assigned(FOnSelectionChanging) then
    FOnSelectionChanging(Self, AItem, Result);
end;

procedure TdxCustomMapControl.CalculateZoomAndCenterPointForRegion(const ARect: TdxRectDouble;
  const APaddingFactor: Double; out AZoomLevel: Double; out ACenterPoint: TdxMapControlGeoPoint);
var
  ALayer: TdxMapLayer;
  ARectInPixels: TdxRectDouble;
  AViewPortInPixels: TSize;
  AIsAdjustByHeight: Boolean;
  AZoomDelta: Double;
begin
  AZoomLevel := ZoomLevel;
  ACenterPoint := ActualCenterPoint;
  ALayer := GetBaseLayer;
  if (ALayer <> nil) and (APaddingFactor < 1) and (APaddingFactor >= 0) then
  begin
    if not ARect.IsEmpty then
    begin
      ARectInPixels.TopLeft := ALayer.MapUnitToScreenPoint(ARect.TopLeft);
      ARectInPixels.BottomRight := ALayer.MapUnitToScreenPoint(ARect.BottomRight);
      AViewPortInPixels := ALayer.ViewportInPixels;
      AViewPortInPixels.cx := AViewPortInPixels.cx;
      AViewPortInPixels.cy := AViewPortInPixels.cy;
      AIsAdjustByHeight := ARectInPixels.Height / AViewPortInPixels.cy > ARectInPixels.Width / AViewPortInPixels.cx;
      if AIsAdjustByHeight then
        AZoomDelta := Log2(AViewPortInPixels.cy * (1 - APaddingFactor) / ARectInPixels.Height)
      else
        AZoomDelta := Log2(AViewPortInPixels.cx * (1 - APaddingFactor) / ARectInPixels.Width);
    end
    else
      AZoomDelta := 0;
    ACenterPoint := ALayer.ScreenPointToGeoPoint(ALayer.MapUnitToScreenPoint(
      dxPointDouble(ARect.Left + ARect.Width / 2, ARect.Top + ARect.Height / 2)));
    AZoomLevel := AZoomLevel + AZoomDelta;
  end;
end;

function TdxCustomMapControl.CanAnimate: Boolean;
begin
  Result := not IsDesigning and FOptionsBehavior.Animation;
end;

function TdxCustomMapControl.CanInertialScrolling: Boolean;
begin
  Result := FOptionsBehavior.InertialScrolling;
end;

function TdxCustomMapControl.CanScroll: Boolean;
begin
  Result := FOptionsBehavior.Scrolling;
end;

function TdxCustomMapControl.CanSelect: Boolean;
begin
  Result := FOptionsBehavior.MapItemSelectMode <> mismNone;
end;

function TdxCustomMapControl.CanZoom: Boolean;
begin
  Result := FOptionsBehavior.Zooming;
end;

procedure TdxCustomMapControl.Changed;
begin
  if IsLocked then
  begin
    FIsViewInfoDirty := True;
    Exit;
  end;
  FViewInfo.Calculate;
  Invalidate;
  FIsViewInfoDirty := False;
end;

procedure TdxCustomMapControl.CheckChanges;
begin
  if FIsViewInfoDirty then
    Changed;
end;

procedure TdxCustomMapControl.CheckViewPort;
begin
  if not FIsViewPortValid then
    UpdateViewPort;
end;

procedure TdxCustomMapControl.DoSelectionChanged;
begin
  if not IsDestroying then
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self);
end;

function TdxCustomMapControl.GetKilometersScale(
  ACenterPoint: TdxMapControlGeoPoint; AScreenDistance: Double): Double;
begin
  if GetBaseLayer <> nil then
    Result := GetBaseLayer.GetKilometersScale(ACenterPoint, AScreenDistance)
  else
    Result := 0;
end;

function TdxCustomMapControl.GetSelectedMapItemCount: Integer;
begin
  Result := FMapItemLayerSelectionController.Count;
end;

function TdxCustomMapControl.GetSelectedMapItems(Index: Integer): TdxMapItem;
begin
  Result := FMapItemLayerSelectionController.Items[Index];
end;

function TdxCustomMapControl.GetMaxZoomLevel: Integer;
begin
  Result := dxMapControlMaxZoomLevel;
end;

procedure TdxCustomMapControl.InvalidateViewPort;
begin
  FIsViewPortValid := False;
end;

procedure TdxCustomMapControl.LayersChanged(Sender: TObject; AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
begin
  Changed;
end;

function TdxCustomMapControl.ScreenPointToGeoPoint(
  APoint: TdxPointDouble): TdxMapControlGeoPoint;
begin
  if GetBaseLayer <> nil then
    Result := GetBaseLayer.ScreenPointToGeoPoint(APoint)
  else
    Result := dxMapControlGeoPoint(0, 0);
end;

procedure TdxCustomMapControl.ScrollMap(X, Y: Integer);
var
  ANewGeoPoint: TdxMapControlGeoPoint;
begin
  if GetBaseLayer <> nil then
  begin
    ANewGeoPoint := GetBaseLayer.Move(CenterPoint.GeoPoint, cxPoint(X, Y));
    DoSetCenterPoint(ANewGeoPoint, True);
  end;
end;

procedure TdxCustomMapControl.SelectMapItemsInRegion(const ARect: TRect);
var
  I: Integer;
  AMapItems: TList;
begin
  ClearSelection;
  AMapItems := TList.Create;
  try
    for I := 0 to Layers.Count - 1 do
    begin
      if Layers[I] is TdxCustomMapItemLayer then
        TdxCustomMapItemLayer(Layers[I]).GetItemsInRegion(ARect, AMapItems);
    end;
    for I := 0 to AMapItems.Count -1 do
      Select(TdxMapItem(AMapItems[I]), [ssCtrl]);
  finally
    AMapItems.Free;
  end;
end;

procedure TdxCustomMapControl.UpdateViewPort;
begin
  if not IsLoading then
  begin
    Layers.UpdateViewPort;
    FIsViewPortValid := True;
  end;
end;

procedure TdxCustomMapControl.Zoom(AZoomLevel: Double;
  const AAnchorPoint: TdxPointDouble; AAnimated: Boolean; AIsAsyncMode: Boolean = False);
var
  AReallyAnimated: Boolean;
begin
  if not AllowZoomLevelChanges(AZoomLevel) then
    Exit;
  CheckZoomLevel(AZoomLevel);
  if FZoomLevel <> AZoomLevel then
  begin
    FInertiaController.StopInertia;
    FZoomLevel := AZoomLevel;
    AReallyAnimated := AAnimated and not IsLocked;
    FAnimationController.Zoom(AZoomLevel, AAnchorPoint, AReallyAnimated, AIsAsyncMode);
    if not AIsAsyncMode or not AReallyAnimated then
      DoZoomLevelChanged;
  end;
end;

procedure TdxCustomMapControl.ZoomToRegion(const ARect: TRect);
var
  ALayer: TdxMapLayer;
  AMapRect: TdxRectDouble;
  AZoomLevel: Double;
  ACenterPoint: TdxMapControlGeoPoint;
begin
  ALayer := GetBaseLayer;
  if (ALayer <> nil) and not ARect.IsEmpty then
  begin
    AMapRect := dxRectDouble(ALayer.ScreenPointToMapUnit(dxPointDouble(ARect.TopLeft)),
      ALayer.ScreenPointToMapUnit(dxPointDouble(ARect.BottomRight)));
    CalculateZoomAndCenterPointForRegion(AMapRect, 0, AZoomLevel, ACenterPoint);
    DoSetCenterPoint(ACenterPoint, True);
    ZoomLevel := Trunc(AZoomLevel);
  end;
end;

procedure TdxCustomMapControl.ZoomToRegion(const ARect: TdxRectDouble;
  const APaddingFactor: Double = dxMapControlDefaultZoomPaddingFactor);
var
  AZoomLevel: Double;
  ACenterPoint: TdxMapControlGeoPoint;
begin
  CalculateZoomAndCenterPointForRegion(ARect, APaddingFactor, AZoomLevel, ACenterPoint);
  DoSetCenterPoint(ACenterPoint);
  ZoomLevel := AZoomLevel;
end;

function TdxCustomMapControl.CreateAnimationController: TdxMapControlAnimationController;
begin
  Result := TdxMapControlAnimationController.Create(Self);
end;

function TdxCustomMapControl.CreateController: TdxMapControlController;
begin
  Result := TdxMapControlController.Create(Self);
end;

function TdxCustomMapControl.CreateMapItemsLayerSelectionController: TdxMapControlSelectionController;
begin
  Result := TdxMapControlSelectionController.Create(Self);
end;

function TdxCustomMapControl.CreateNavigationPanelOptions: TdxMapControlNavigationPanel;
begin
  Result := TdxMapControlNavigationPanel.Create(Self);
end;

function TdxCustomMapControl.CreateOptionsBehavior: TdxMapControlOptionsBehavior;
begin
  Result := TdxMapControlOptionsBehavior.Create(Self);
end;

procedure TdxCustomMapControl.CreateSubclasses;
begin
  FController := CreateController;
  FPainter := CreatePainter;
  FViewInfo := CreateViewInfo;
  FMapItemLayerSelectionController := CreateMapItemsLayerSelectionController;
  FNavigationPanel := CreateNavigationPanelOptions;
  FAnimationController := CreateAnimationController;
  FOptionsBehavior := CreateOptionsBehavior;
  FLayers := TdxMapLayers.Create(Self, TdxMapLayer);
  FLayers.OnChange := LayersChanged;
  FInformationProviders := TdxMapControlInformationProviders.Create(Self, TdxMapControlInformationProvider);
  FInertiaController := TdxMapControlInertialScrollingController.Create(Self);
  //FInformationProviders.OnChange := InformationProvidersChanged;
end;

function TdxCustomMapControl.CreatePainter: TdxMapControlPainter;
begin
  Result := TdxMapControlPainter.Create(Self);
end;

procedure TdxCustomMapControl.DestroySubclasses;
begin
  FreeAndNil(FInertiaController);
  FreeAndNil(FInformationProviders);
  FreeAndNil(FLayers);
  FreeAndNil(FOptionsBehavior);
  FreeAndNil(FAnimationController);
  FreeAndNil(FNavigationPanel);
  FreeAndNil(FMapItemLayerSelectionController);
  FreeAndNil(FViewInfo);
  FreeAndNil(FPainter);
  FreeAndNil(FController);
end;

function TdxCustomMapControl.CreateViewInfo: TdxMapControlViewInfo;
begin
  Result := TdxMapControlViewInfo.Create(Self);
end;

function TdxCustomMapControl.AllowZoomLevelChanges(ALevel: Double): Boolean;
begin
  Result := True;
  if Assigned(FOnZoomLevelChanging) then
    FOnZoomLevelChanging(Self, ALevel, Result);
end;

procedure TdxCustomMapControl.CenterPointChanged(ASender: TObject);
begin
  UpdateViewPort;
  Changed;
end;

procedure TdxCustomMapControl.CheckZoomLevel(var ALevel: Double);
begin
  ALevel := Min(Max(ALevel, 1), dxMapControlMaxZoomLevel);
end;

procedure TdxCustomMapControl.DoCenterPointChanged;
begin
  dxCallNotify(FOnCenterPointChanged, Self);
end;

procedure TdxCustomMapControl.DoSetCenterPoint(const AValue: TdxMapControlGeoPoint; AIsUserAction: Boolean = False);

  procedure InternalSetCenterPoint(const AValue: TdxMapControlGeoPoint);
  begin
    FCenterPoint.GeoPoint := AValue;
  end;

var
  ANewGeoPoint: TdxMapControlGeoPoint;
begin
  if not dxMapControlGeoPointIsEqual(FCenterPoint.GeoPoint, AValue) then
    if AIsUserAction then
    begin
      ANewGeoPoint := AValue;
      if AllowCenterPointChanges(ANewGeoPoint) and
        not dxMapControlGeoPointIsEqual(FCenterPoint.GeoPoint, ANewGeoPoint) then
      begin
        InternalSetCenterPoint(ANewGeoPoint);
        DoCenterPointChanged;
      end;
    end
    else
      InternalSetCenterPoint(AValue);
end;

procedure TdxCustomMapControl.DoZoomLevelChanged;
begin
  dxCallNotify(FOnZoomLevelChanged, Self);
end;

function TdxCustomMapControl.GetActualCenterPoint: TdxMapControlGeoPoint;
begin
  Result := FCenterPoint.GeoPoint;
end;

function TdxCustomMapControl.GetActualZoomLevel: Double;
begin
  Result := FAnimationController.ZoomLevel;
end;

function TdxCustomMapControl.GetBaseLayer: TdxMapLayer;
begin
  Result := Layers.GetBaseLayer;
end;

procedure TdxCustomMapControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to Layers.Count - 1 do
    if Layers[I].Owner = Root then
      Proc(Layers[I]);
  for I := 0 to InformationProviders.Count - 1 do
    if InformationProviders[I].Owner = Root then
      Proc(InformationProviders[I]);
end;

procedure TdxCustomMapControl.TranslationChanged;
begin
  Changed;
end;

function TdxCustomMapControl.GetHitTest: TdxMapControlHitTest;
begin
  Result := Controller.HitTest;
end;

function TdxCustomMapControl.IsZoomLevelStored: Boolean;
begin
  Result := FZoomLevel <> 1;
end;

procedure TdxCustomMapControl.SetCenterPoint(AValue: TdxMapControlGeoLocation);
begin
  FCenterPoint.Assign(AValue);
end;

procedure TdxCustomMapControl.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TdxCustomMapControl.SetInformationProviders(const Value: TdxMapControlInformationProviders);
begin
  FInformationProviders.Assign(Value);
end;

procedure TdxCustomMapControl.SetLayers(const Value: TdxMapLayers);
begin
  FLayers.Assign(Value);
end;

procedure TdxCustomMapControl.SetNavigationPanel(AValue: TdxMapControlNavigationPanel);
begin
  FNavigationPanel.Assign(AValue);
end;

procedure TdxCustomMapControl.SetOptionsBehavior(AValue: TdxMapControlOptionsBehavior);
begin
  FOptionsBehavior.Assign(AValue);
end;

procedure TdxCustomMapControl.SetZoomLevel(Value: Double);
begin
  if IsLoading then
    FLoadedZoomLevel := Value
  else
    Zoom(Value, CanAnimate);
end;

procedure TdxCustomMapControl.UpdateSelectedRegion(const R: TRect);
begin
  cxInvalidateRect(Handle, FSelectedRegionBounds);
  FSelectedRegionBounds := R;
  cxInvalidateRect(Handle, R);
end;

end.
