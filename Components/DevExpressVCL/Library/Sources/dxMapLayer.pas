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

unit dxMapLayer;

interface

{$I cxVer.inc}

uses
  Types, Classes, SysUtils, Math, Controls,
  dxCoreClasses, cxGraphics, cxGeometry, cxClasses,
  dxMapControlProjections, dxMapUnitConverter, dxMapControlTypes,
  dxMapControlElementViewInfo, dxMapControlViewInfo;

const
  dxMapControlDefaultMapSize = 512;

type
  TdxMapLayers = class;
  TdxMapLayer = class;

  TdxMapLayerViewInfo = class(TdxMapControlElementViewInfo)
  private
    FLayer: TdxMapLayer;
  protected
    property Layer: TdxMapLayer read FLayer;
  public
    constructor Create(AOwner: TdxMapLayer); reintroduce; virtual;
  end;

  TdxMapLayer = class(TcxComponentCollectionItem)
  private
    FRenderScale: TdxPointDouble;
    FRenderOffset: TdxPointDouble;
    FUnitConverter: TdxMapUnitConverter;
    FViewInfo: TdxMapLayerViewInfo;
    FViewPort: TdxRectDouble;
    FVisible: Boolean;
    procedure CalculateRenderParameters;
    function GetCenterPoint: TdxMapControlGeoPoint;
    function GetScaleFactor: TdxScaleFactor;
    function GetViewportInPixels: TSize;
    function GetZoomLevel: Double;
    procedure SetViewPort(const Value: TdxRectDouble);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure DoActivate; virtual;
    procedure DoAssign(Source: TPersistent); virtual;
    procedure DoDeactivate; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    procedure InvalidateViewPort;
    procedure SetCollection(AValue: TcxComponentCollection); override;

    function CalculateViewport(AZoomLevel: Double; ACenterPoint: TdxMapControlGeoPoint;
      AViewportInPixels: TSize): TdxRectDouble;
    function CreateViewInfo: TdxMapLayerViewInfo; virtual;
    function GetMapProjection: TdxMapControlCustomProjection; virtual; abstract;
    function GetMapSizeInPixels(AZoomLevel: Double): TdxSizeDouble; virtual; abstract;
    function GetMapUnitConverter: TdxMapUnitConverter;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Invalidate;
    procedure InvalidateRect(const R: TRect);
    function IsActuallyVisible: Boolean; virtual;
    function Move(ACenterPoint: TdxMapControlGeoPoint; AOffset: TPoint): TdxMapControlGeoPoint; virtual;
    function MoveAndZoom(ACenterPoint: TdxMapControlGeoPoint; AAnchorPoint: TdxPointDouble;
      ACurrentZoomLevel, ANewZoomLevel: Double): TdxMapControlGeoPoint; virtual;
    procedure UpdateViewport; virtual;

    function GetKilometersScale(const ACenterPoint: TdxMapControlGeoPoint; AScreenDistance: Double): Double;
    function GeoPointToMapUnit(const APoint: TdxMapControlGeoPoint; AShouldNormalize: Boolean = True): TdxPointDouble;
    function GeoPointToScreenPoint(const APoint: TdxMapControlGeoPoint): TdxPointDouble;
    function GeoToKilometersSize(const AAnchorPoint: TdxMapControlGeoPoint; const ASize: TdxSizeDouble): TdxSizeDouble;
    function KilometersToGeoSize(const AAnchorPoint: TdxMapControlGeoPoint; const ASize: TdxSizeDouble): TdxSizeDouble;
    function MapUnitToScreenPoint(const APoint: TdxPointDouble): TdxPointDouble;
    function ScreenPointToGeoPoint(const APoint: TdxPointDouble): TdxMapControlGeoPoint;
    function ScreenPointToMapUnit(const APoint: TdxPointDouble): TdxPointDouble;

    property CenterPoint: TdxMapControlGeoPoint read GetCenterPoint;
    property RenderScale: TdxPointDouble read FRenderScale;
    property RenderOffset: TdxPointDouble read FRenderOffset;
    property ViewInfo: TdxMapLayerViewInfo read FViewInfo;
    property ViewPort: TdxRectDouble read FViewPort write SetViewPort;
    property ViewportInPixels: TSize read GetViewportInPixels;
    property ZoomLevel: Double read GetZoomLevel;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
  end;
  TdxMapLayerClass = class of TdxMapLayer;

  TdxMapLayersViewInfo = class(TdxMapControlElementViewInfo)
  private
    FLayers: TdxMapLayers;
    function GetItems(Index: Integer): TdxMapLayerViewInfo;
  protected
    procedure AddVisibleElements; override;
    function GetHitTestIndex: Integer; override;
  public
    constructor Create(AOwner: TdxMapLayers); reintroduce; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property Items[Index: Integer]: TdxMapLayerViewInfo read GetItems;
  end;

  TdxMapLayers = class(TcxComponentCollection)
  private
    FViewInfo: TdxMapLayersViewInfo;
    function GetItem(Index: Integer): TdxMapLayer;
    procedure SetItem(Index: Integer; Value: TdxMapLayer);
  protected
    procedure ChangeScale(M, D: Integer);
    function GetItemPrefixName: string; override;
    procedure SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1); override;
  public
    constructor Create(AParentComponent: TComponent; AItemClass: TcxComponentCollectionItemClass); override;
    destructor Destroy; override;
    function Add(AItemClass: TdxMapLayerClass): TdxMapLayer;
    procedure Assign(Source: TPersistent); override;
    function GetBaseLayer: TdxMapLayer;
    procedure UpdateViewPort;
    property ViewInfo: TdxMapLayersViewInfo read FViewInfo;
    property Items[Index: Integer]: TdxMapLayer read GetItem write SetItem; default;
  end;

implementation

uses
  dxMapControl, dxDPIAwareUtils;

type
  TdxMapControlAccess = class(TdxCustomMapControl);

{ TdxMapLayerViewInfo }

constructor TdxMapLayerViewInfo.Create(AOwner: TdxMapLayer);
begin
  inherited Create;
  FLayer := AOwner;
end;

{ TdxMapLayer }

constructor TdxMapLayer.Create(AOwner: TComponent);
begin
  inherited;
  FViewInfo := CreateViewInfo;
  FVisible := True;
end;

destructor TdxMapLayer.Destroy;
begin
  FreeAndNil(FViewInfo);
  FreeAndNil(FUnitConverter);
  inherited;
end;

procedure TdxMapLayer.Assign(Source: TPersistent);
begin
  if Source is TdxMapLayer then
  begin
    Collection.BeginUpdate;
    try
      DoAssign(Source);
    finally
      Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TdxMapLayer.DoActivate;
begin

end;

procedure TdxMapLayer.DoAssign(Source: TPersistent);
begin
  Visible := TdxMapLayer(Source).Visible;
end;

procedure TdxMapLayer.DoDeactivate;
begin

end;

procedure TdxMapLayer.ChangeScale(M, D: Integer);
begin
  InvalidateViewPort
end;

procedure TdxMapLayer.Invalidate;
begin
  (GetParentComponent as TdxCustomMapControl).Invalidate;
end;

procedure TdxMapLayer.InvalidateRect(const R: TRect);
begin
  (GetParentComponent as TdxCustomMapControl).InvalidateRect(R, True);
end;

function TdxMapLayer.IsActuallyVisible: Boolean;
begin
  Result := Visible and not (GetParentComponent as TdxCustomMapControl).IsDesigning and
    (GetMapProjection <> nil);
end;

function TdxMapLayer.Move(ACenterPoint: TdxMapControlGeoPoint;
  AOffset: TPoint): TdxMapControlGeoPoint;
var
  AMapPoint: TdxPointDouble;
begin
  AMapPoint := MapUnitToScreenPoint(GeoPointToMapUnit(ACenterPoint, False));
  Result := ScreenPointToGeoPoint(dxPointDouble(AMapPoint.X + AOffset.X, AMapPoint.Y + AOffset.Y));
end;

function TdxMapLayer.MoveAndZoom(
  ACenterPoint: TdxMapControlGeoPoint; AAnchorPoint: TdxPointDouble;
  ACurrentZoomLevel, ANewZoomLevel: Double): TdxMapControlGeoPoint;
var
  AMapSize, ANewMapSize: TdxSizeDouble;
  AFactorWidth, AFactorHeight: Double;
  ACenter, AAnchor, ANewCenter: TdxPointDouble;
begin
  AMapSize := GetMapSizeInPixels(ACurrentZoomLevel);
  ANewMapSize := GetMapSizeInPixels(ANewZoomLevel);
  AFactorWidth := ANewMapSize.Width / AMapSize.Width;
  AFactorHeight := ANewMapSize.Height / AMapSize.Height;
  ACenter := GetMapUnitConverter.GeoPointToMapUnit(ACenterPoint, GetMapProjection);
  AAnchor := GetMapUnitConverter.ScreenPointToMapUnit(AAnchorPoint, ViewPort, ViewportInPixels);
  ANewCenter := dxPointDouble(AAnchor.X + (ACenter.X - AAnchor.X) / AFactorWidth,
    AAnchor.Y + (ACenter.Y - AAnchor.Y) / AFactorHeight);
  Result := GetMapUnitConverter.MapUnitToGeoPoint(ANewCenter, GetMapProjection);
end;

procedure TdxMapLayer.UpdateViewport;
begin
  if cxSizeIsEmpty(ViewportInPixels) then
    ViewPort := dxNullRectDouble
  else
    ViewPort := CalculateViewport(ZoomLevel, CenterPoint, ViewportInPixels);
end;

function TdxMapLayer.GetKilometersScale(
  const ACenterPoint: TdxMapControlGeoPoint; AScreenDistance: Double): Double;
var
  AScreenCenterPoint: TdxPointDouble;
  AHemisphereMultiplier: Double;
  AScaleGeoPoint: TdxMapControlGeoPoint;
begin
  AScreenCenterPoint := GeoPointToScreenPoint(ACenterPoint);
  AHemisphereMultiplier := IfThen(ACenterPoint.Longitude >= 0, 1.0, -1.0);
  AScaleGeoPoint := ScreenPointToGeoPoint(
    dxPointDouble(AScreenCenterPoint.X - AHemisphereMultiplier * AScreenDistance, AScreenCenterPoint.Y));
  Result := GeoToKilometersSize(ACenterPoint, dxSizeDouble(Abs(AScaleGeoPoint.Longitude - ACenterPoint.Longitude), 0.0)).Width;
end;

function TdxMapLayer.GeoPointToMapUnit(
  const APoint: TdxMapControlGeoPoint; AShouldNormalize: Boolean = True): TdxPointDouble;
begin
  Result := GetMapUnitConverter.GeoPointToMapUnit(APoint, GetMapProjection, AShouldNormalize);
end;

function TdxMapLayer.GeoPointToScreenPoint(const APoint: TdxMapControlGeoPoint): TdxPointDouble;
begin
  Result := MapUnitToScreenPoint(GeoPointToMapUnit(APoint));
end;

function TdxMapLayer.GeoToKilometersSize(const AAnchorPoint: TdxMapControlGeoPoint;
  const ASize: TdxSizeDouble): TdxSizeDouble;
begin
  Result := GetMapProjection.GeoToKilometersSize(AAnchorPoint, ASize);
end;

function TdxMapLayer.KilometersToGeoSize(const AAnchorPoint: TdxMapControlGeoPoint;
  const ASize: TdxSizeDouble): TdxSizeDouble;
begin
  Result := GetMapProjection.KilometersToGeoSize(AAnchorPoint, ASize);
end;

function TdxMapLayer.MapUnitToScreenPoint(const APoint: TdxPointDouble): TdxPointDouble;
begin
  Result := dxPointDoubleOffset(dxPointDoubleScale(APoint, FRenderScale), FRenderOffset, False);
end;

function TdxMapLayer.ScreenPointToGeoPoint(const APoint: TdxPointDouble): TdxMapControlGeoPoint;
begin
  Result := GetMapUnitConverter.MapUnitToGeoPoint(ScreenPointToMapUnit(APoint), GetMapProjection);
end;

function TdxMapLayer.ScreenPointToMapUnit(const APoint: TdxPointDouble): TdxPointDouble;
begin
  Result := GetMapUnitConverter.ScreenPointToMapUnit(APoint, ViewPort, ViewportInPixels);
end;

function TdxMapLayer.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxCustomMapControl).Layers;
end;

procedure TdxMapLayer.InvalidateViewPort;
begin
  if GetParentComponent <> nil then
    TdxMapControlAccess(GetParentComponent).UpdateViewport;
end;

function TdxMapLayer.CalculateViewport(AZoomLevel: Double; ACenterPoint: TdxMapControlGeoPoint;
  AViewportInPixels: TSize): TdxRectDouble;
var
  AMapSizeInPixels: TdxSizeDouble;
  AWidth, AHeight: Double;
  ACenter: TdxPointDouble;
begin
  AMapSizeInPixels := GetMapSizeInPixels(AZoomLevel);
  AWidth := AViewportInPixels.cx / AMapSizeInPixels.Width;
  AHeight := AViewportInPixels.cy / AMapSizeInPixels.Height;
  ACenter := GetMapUnitConverter.GeoPointToMapUnit(ACenterPoint, GetMapProjection);
  Result := dxRectDouble(ACenter.X - AWidth * 0.5, ACenter.Y - AHeight * 0.5, AWidth, AHeight);
end;

function TdxMapLayer.CreateViewInfo: TdxMapLayerViewInfo;
begin
  Result := TdxMapLayerViewInfo.Create(Self);
end;

function TdxMapLayer.GetMapUnitConverter: TdxMapUnitConverter;
begin
  if FUnitConverter = nil then
    FUnitConverter := TdxMapUnitConverter.Create;
  Result := FUnitConverter;
end;

procedure TdxMapLayer.CalculateRenderParameters;
begin
  if (ViewPort.Width = 0) or (Viewport.Height = 0) then
    FRenderScale := dxNullPointDouble
  else
    FRenderScale := dxPointDouble(ViewportInPixels.cx / (Viewport.Width),
      ViewportInPixels.cy / (Viewport.Height));
  FRenderOffset := dxPointDoubleScale(Viewport.TopLeft, FRenderScale);
end;

function TdxMapLayer.GetCenterPoint: TdxMapControlGeoPoint;
begin
  Result := (GetParentComponent as TdxCustomMapControl).ActualCenterPoint;
end;

function TdxMapLayer.GetScaleFactor: TdxScaleFactor;
begin
  Result := TdxMapControlAccess(GetParentComponent).ScaleFactor;
end;

function TdxMapLayer.GetViewportInPixels: TSize;
begin
  Result := cxSize((GetParentComponent as TdxCustomMapControl).Bounds);
end;

function TdxMapLayer.GetZoomLevel: Double;
begin
  Result := (GetParentComponent as TdxCustomMapControl).ActualZoomLevel;
end;

procedure TdxMapLayer.SetCollection(AValue: TcxComponentCollection);
var
  ANumerator, ADenominator: Integer;
begin
  inherited;
  if (Collection <> nil) and dxGetCurrentScaleFactor(Collection.ParentComponent, ANumerator, ADenominator) then
    ChangeScale(ANumerator, ADenominator);
end;

procedure TdxMapLayer.SetViewPort(
  const Value: TdxRectDouble);
begin
  if not dxRectDoubleIsEqual(FViewPort, Value) then
  begin
    FViewPort := Value;
    CalculateRenderParameters;
  end;
end;

procedure TdxMapLayer.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    InvalidateViewPort;
    Changed(False);
  end;
end;

{ TdxMapLayersViewInfo }

constructor TdxMapLayersViewInfo.Create(AOwner: TdxMapLayers);
begin
  inherited Create;
  FLayers := AOwner;
end;

procedure TdxMapLayersViewInfo.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  TdxCustomMapControl(FLayers.ParentComponent).ClearSelection;
end;

procedure TdxMapLayersViewInfo.AddVisibleElements;
var
  I: Integer;
begin
  for I := 0 to FLayers.Count - 1 do
    if FLayers[I].IsActuallyVisible then
      Add(FLayers[I].ViewInfo);
end;

function TdxMapLayersViewInfo.GetHitTestIndex: Integer;
begin
  Result := mchtLayer;
end;

function TdxMapLayersViewInfo.GetItems(
  Index: Integer): TdxMapLayerViewInfo;
begin
  Result := inherited Items[Index] as TdxMapLayerViewInfo;
end;

{ TdxMapLayers }

constructor TdxMapLayers.Create(AParentComponent: TComponent;
  AItemClass: TcxComponentCollectionItemClass);
begin
  inherited Create(AParentComponent, AItemClass);
  FViewInfo := TdxMapLayersViewInfo.Create(Self);
end;

destructor TdxMapLayers.Destroy;
begin
  FreeAndNil(FViewInfo);
  inherited;
end;

function TdxMapLayers.Add(AItemClass: TdxMapLayerClass): TdxMapLayer;
begin
  Result := AItemClass.Create(ParentComponent.Owner);
  Result.SetParentComponent(ParentComponent);
  SetItemName(Result);
  Result.InvalidateViewPort;
end;

procedure TdxMapLayers.Assign(Source: TPersistent);
var
  I: Integer;
  AItem: TdxMapLayer;
begin
  if Source is TdxMapLayers then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TdxMapLayers(Source).Count - 1 do
      begin
        AItem := TdxMapLayers(Source).Items[I];
        Add(TdxMapLayerClass(AItem.ClassType)).Assign(AItem);
      end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TdxMapLayers.GetBaseLayer: TdxMapLayer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].IsActuallyVisible then
    begin
      Result := Items[I];
      Break;
    end;
end;

procedure TdxMapLayers.UpdateViewPort;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].IsActuallyVisible then
      Items[I].UpdateViewport;
end;

procedure TdxMapLayers.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ChangeScale(M, D);
end;

function TdxMapLayers.GetItemPrefixName: string;
begin
  Result := 'TdxMap';
end;

procedure TdxMapLayers.SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1);
begin
  if csDesigning in ParentComponent.ComponentState then
    inherited SetItemName(AItem, ABaseIndex);
end;

function TdxMapLayers.GetItem(Index: Integer): TdxMapLayer;
begin
  Result := inherited GetItem(Index) as TdxMapLayer;
end;

procedure TdxMapLayers.SetItem(Index: Integer; Value: TdxMapLayer);
begin
  inherited SetItem(Index, Value);
end;

end.
