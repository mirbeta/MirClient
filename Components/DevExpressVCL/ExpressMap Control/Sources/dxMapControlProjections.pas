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

unit dxMapControlProjections;

interface

{$I cxVer.inc}

uses
  SysUtils, Math, Classes,
  dxCore, dxCoreClasses, cxGeometry, cxClasses,
  dxMapControlTypes;

type
  TdxMapControlProjectionCalculator = class
  protected
    function MaxLatitudeInternal: Double; virtual; abstract;
    function MaxLongitudeInternal: Double; virtual; abstract;
    function MinLatitudeInternal: Double; virtual; abstract;
    function MinLongitudeInternal: Double; virtual; abstract;

    function GetGeoPointLatitude(AMapUnitY, AOffsetY, AScaleY: Double): Double; virtual;
    function GetGeoPointLongitude(AMapUnitX, AOffsetX, AScaleX: Double): Double; virtual;
    function GetMapUnitX(ALongitude, AOffsetX, AScaleX: Double): Double; virtual;
    function GetMapUnitY(ALatitude, AOffsetY, AScaleY: Double): Double; virtual;

    function GetGeoPoint(AMapUnit: TdxPointDouble; AOffset, AScale: TdxPointDouble): TdxMapControlGeoPoint; virtual;
    function GetMapUnit(AGeoPoint: TdxMapControlGeoPoint; AOffset, AScale: TdxPointDouble): TdxPointDouble; virtual;
  end;

  TdxMapControlCustomProjection = class(TcxInterfacedPersistent)
  private
    FChanged: Boolean;
    FLockCount: Integer;
    FOffsetX, FOffsetY: Double;
    FProjectionCalculator: TdxMapControlProjectionCalculator;
    FScaleX, FScaleY: Double;
    FOnChanged: TNotifyEvent;
    function IsOffsetXStored: Boolean;
    function IsOffsetYStored: Boolean;
    function IsScaleXStored: Boolean;
    function IsScaleYStored: Boolean;
    procedure SetOffsetX(const Value: Double);
    procedure SetOffsetY(const Value: Double);
    procedure SetScaleX(const Value: Double);
    procedure SetScaleY(const Value: Double);
  protected
    procedure BeginUpdate;
    procedure Changed;
    function CreateProjectionCalculator: TdxMapControlProjectionCalculator; virtual;
    procedure EndUpdate;
    function GetDefaultOffsetX: Double; virtual;
    function GetDefaultOffsetY: Double; virtual;
    function GetDefaultScaleX: Double; virtual;
    function GetDefaultScaleY: Double; virtual;
    function InternalGeoPointToMapUnit(AGeoPoint: TdxMapControlGeoPoint): TdxPointDouble; virtual;
    function InternalMapUnitToGeoPoint(AMapUnit: TdxPointDouble): TdxMapControlGeoPoint; virtual;

    property OffsetX: Double read FOffsetX write SetOffsetX stored IsOffsetXStored;
    property OffsetY: Double read FOffsetY write SetOffsetY stored IsOffsetYStored;
    property ScaleX: Double read FScaleX write SetScaleX stored IsScaleXStored;
    property ScaleY: Double read FScaleY write SetScaleY stored IsScaleYStored;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function MapUnitToGeoPoint(AMapUnit: TdxPointDouble): TdxMapControlGeoPoint; virtual;
    function GeoPointToMapUnit(AGeoPoint: TdxMapControlGeoPoint): TdxPointDouble; virtual;
    function GeoToKilometersSize(AAnchorPoint: TdxMapControlGeoPoint; ASize: TdxSizeDouble): TdxSizeDouble; virtual;
    function KilometersToGeoSize(AAnchorPoint: TdxMapControlGeoPoint; ASize: TdxSizeDouble): TdxSizeDouble; virtual;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TdxMapControlCustomProjectionClass = class of TdxMapControlCustomProjection;

  TdxMapControlSphericalMercatorProjectionCalculator = class(TdxMapControlProjectionCalculator)
  protected
    function MaxLatitudeInternal: Double; override;
    function MaxLongitudeInternal: Double; override;
    function MinLatitudeInternal: Double; override;
    function MinLongitudeInternal: Double; override;

    function GetGeoPointLatitude(AMapUnitY, AOffsetY, AScaleY: Double): Double; override;
    function GetGeoPointLongitude(AMapUnitX, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitX(ALongitude, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitY(ALatitude, AOffsetY, AScaleY: Double): Double; override;
  end;

  TdxMapControlSphericalMercatorProjection = class(TdxMapControlCustomProjection)
  protected
    function CreateProjectionCalculator: TdxMapControlProjectionCalculator; override;
  end;

  TdxMapControlEllipticalMercatorProjectionCalculator = class(TdxMapControlProjectionCalculator)
  private
    FPiHalf: Double;
    FRadiusMajor: Double;
    FRadiusMinor: Double;
    FRadiusRatio: Double;
    FEccentricity: Double;
    FEccentricityHalf: Double;
  protected
    function MaxLatitudeInternal: Double; override;
    function MaxLongitudeInternal: Double; override;
    function MinLatitudeInternal: Double; override;
    function MinLongitudeInternal: Double; override;

    function GetGeoPointLatitude(AMapUnitY, AOffsetY, AScaleY: Double): Double; override;
    function GetGeoPointLongitude(AMapUnitX, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitX(ALongitude, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitY(ALatitude, AOffsetY, AScaleY: Double): Double; override;
  public
    constructor Create;
  end;

  TdxMapControlEllipticalMercatorProjection = class(TdxMapControlCustomProjection)
  protected
    function CreateProjectionCalculator: TdxMapControlProjectionCalculator; override;
  end;

  TdxMapControlMillerProjectionCalculator = class(TdxMapControlProjectionCalculator)
  private
    FScaleFactor: Double;
  protected
    function MaxLatitudeInternal: Double; override;
    function MaxLongitudeInternal: Double; override;
    function MinLatitudeInternal: Double; override;
    function MinLongitudeInternal: Double; override;

    function GetGeoPointLatitude(AMapUnitY, AOffsetY, AScaleY: Double): Double; override;
    function GetGeoPointLongitude(AMapUnitX, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitX(ALongitude, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitY(ALatitude, AOffsetY, AScaleY: Double): Double; override;
  public
    constructor Create;
  end;

  TdxMapControlMillerProjection = class(TdxMapControlCustomProjection)
  protected
    function CreateProjectionCalculator: TdxMapControlProjectionCalculator; override;
    function GetDefaultScaleY: Double; override;
  end;

  TdxMapControlEquirectangularProjectionCalculator = class(TdxMapControlProjectionCalculator)
  protected
    function MaxLatitudeInternal: Double; override;
    function MaxLongitudeInternal: Double; override;
    function MinLatitudeInternal: Double; override;
    function MinLongitudeInternal: Double; override;

    function GetGeoPointLatitude(AMapUnitY, AOffsetY, AScaleY: Double): Double; override;
    function GetGeoPointLongitude(AMapUnitX, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitX(ALongitude, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitY(ALatitude, AOffsetY, AScaleY: Double): Double; override;
  end;

  TdxMapControlEquirectangularProjection = class(TdxMapControlCustomProjection)
  protected
    function CreateProjectionCalculator: TdxMapControlProjectionCalculator; override;
    function GetDefaultScaleY: Double; override;
  end;

  TdxMapControlEqualAreaProjectionCalculator = class(TdxMapControlProjectionCalculator)
  protected
    function MaxLatitudeInternal: Double; override;
    function MaxLongitudeInternal: Double; override;
    function MinLatitudeInternal: Double; override;
    function MinLongitudeInternal: Double; override;

    function GetGeoPointLatitude(AMapUnitY, AOffsetY, AScaleY: Double): Double; override;
    function GetGeoPointLongitude(AMapUnitX, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitX(ALongitude, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitY(ALatitude, AOffsetY, AScaleY: Double): Double; override;
  end;

  TdxMapControlEqualAreaProjection = class(TdxMapControlCustomProjection)
  protected
    function CreateProjectionCalculator: TdxMapControlProjectionCalculator; override;
    function GetDefaultScaleY: Double; override;
  end;

  TdxMapControlLambertCylindricalEqualAreaProjectionCalculator = class(TdxMapControlProjectionCalculator)
  private const
    MeridianOffset = 0;
  protected
    function MaxLatitudeInternal: Double; override;
    function MaxLongitudeInternal: Double; override;
    function MinLatitudeInternal: Double; override;
    function MinLongitudeInternal: Double; override;

    function GetGeoPointLatitude(AMapUnitY, AOffsetY, AScaleY: Double): Double; override;
    function GetGeoPointLongitude(AMapUnitX, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitX(ALongitude, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitY(ALatitude, AOffsetY, AScaleY: Double): Double; override;
  end;

  TdxMapControlLambertCylindricalEqualAreaProjection = class(TdxMapControlCustomProjection)
  protected
    function CreateProjectionCalculator: TdxMapControlProjectionCalculator; override;
    function GetDefaultScaleY: Double; override;
  end;

  TdxMapControlBraunStereographicProjectionCalculator = class(TdxMapControlProjectionCalculator)
  private const
    Radius = 1;
  protected
    function MaxLatitudeInternal: Double; override;
    function MaxLongitudeInternal: Double; override;
    function MinLatitudeInternal: Double; override;
    function MinLongitudeInternal: Double; override;

    function GetGeoPointLatitude(AMapUnitY, AOffsetY, AScaleY: Double): Double; override;
    function GetGeoPointLongitude(AMapUnitX, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitX(ALongitude, AOffsetX, AScaleX: Double): Double; override;
    function GetMapUnitY(ALatitude, AOffsetY, AScaleY: Double): Double; override;
  end;

  TdxMapControlBraunStereographicProjection = class(TdxMapControlCustomProjection)
  protected
    function CreateProjectionCalculator: TdxMapControlProjectionCalculator; override;
    function GetDefaultScaleY: Double; override;
  end;

  TdxMapControlKavrayskiyProjectionCalculator = class(TdxMapControlProjectionCalculator)
  private const
    MeridianOffset = 0;
  protected
    function MaxLatitudeInternal: Double; override;
    function MaxLongitudeInternal: Double; override;
    function MinLatitudeInternal: Double; override;
    function MinLongitudeInternal: Double; override;

    function GetGeoPoint(AMapUnit: TdxPointDouble; AOffset, AScale: TdxPointDouble): TdxMapControlGeoPoint; override;
    function GetMapUnit(AGeoPoint: TdxMapControlGeoPoint; AOffset, AScale: TdxPointDouble): TdxPointDouble; override;
  end;

  TdxMapControlKavrayskiyProjection = class(TdxMapControlCustomProjection)
  protected
    function CreateProjectionCalculator: TdxMapControlProjectionCalculator; override;
    function GetDefaultScaleY: Double; override;
  end;

  TdxMapControlSinusoidalProjectionCalculator = class(TdxMapControlProjectionCalculator)
  private const
    MeridianOffset = 0;
    k = 1;
  protected
    function MaxLatitudeInternal: Double; override;
    function MaxLongitudeInternal: Double; override;
    function MinLatitudeInternal: Double; override;
    function MinLongitudeInternal: Double; override;

    function GetGeoPoint(AMapUnit: TdxPointDouble; AOffset, AScale: TdxPointDouble): TdxMapControlGeoPoint; override;
    function GetMapUnit(AGeoPoint: TdxMapControlGeoPoint; AOffset, AScale: TdxPointDouble): TdxPointDouble; override;
  end;

  TdxMapControlSinusoidalProjection = class(TdxMapControlCustomProjection)
  protected
    function CreateProjectionCalculator: TdxMapControlProjectionCalculator; override;
    function GetDefaultScaleY: Double; override;
  end;

  TdxMapControlEPSG4326ProjectionCalculator = class(TdxMapControlProjectionCalculator)
  protected
    function MaxLatitudeInternal: Double; override;
    function MaxLongitudeInternal: Double; override;
    function MinLatitudeInternal: Double; override;
    function MinLongitudeInternal: Double; override;

    function GetGeoPoint(AMapUnit: TdxPointDouble; AOffset, AScale: TdxPointDouble): TdxMapControlGeoPoint; override;
    function GetMapUnit(AGeoPoint: TdxMapControlGeoPoint; AOffset, AScale: TdxPointDouble): TdxPointDouble; override;
  end;

  TdxMapControlEPSG4326Projection = class(TdxMapControlCustomProjection)
  protected
    function CreateProjectionCalculator: TdxMapControlProjectionCalculator; override;
  end;

function dxRegisteredMapProjections: TcxRegisteredClasses;

implementation

const
  Ratio: Double = 111.2;

var
  FRegisteredProjections: TcxRegisteredClasses;

function dxRegisteredMapProjections: TcxRegisteredClasses;
begin
  if FRegisteredProjections = nil then
    FRegisteredProjections := TcxRegisteredClasses.Create;
  Result := FRegisteredProjections;
end;

{ TdxMapControlProjectionCalculator }

function TdxMapControlProjectionCalculator.GetGeoPoint(AMapUnit, AOffset,
  AScale: TdxPointDouble): TdxMapControlGeoPoint;
var
  ALatitude, ALongitude: Double;
begin
  ALatitude := GetGeoPointLatitude(AMapUnit.Y, AOffset.Y, AScale.Y);
  ALongitude := GetGeoPointLongitude(AMapUnit.X, AOffset.X, AScale.X);
  Result := dxMapControlGeoPoint(ALatitude, ALongitude);
end;

function TdxMapControlProjectionCalculator.GetGeoPointLatitude(AMapUnitY, AOffsetY, AScaleY: Double): Double;
begin
  Result := 0;
  dxAbstractError;
end;

function TdxMapControlProjectionCalculator.GetGeoPointLongitude(AMapUnitX, AOffsetX, AScaleX: Double): Double;
begin
  Result := 0;
  dxAbstractError;
end;

function TdxMapControlProjectionCalculator.GetMapUnit(AGeoPoint: TdxMapControlGeoPoint; AOffset,
  AScale: TdxPointDouble): TdxPointDouble;
var
  AMapUnitX, AMapUnitY: Double;
begin
  AMapUnitX := GetMapUnitX(AGeoPoint.Longitude, AOffset.X, AScale.X);
  AMapUnitY := GetMapUnitY(AGeoPoint.Latitude, AOffset.Y, AScale.Y);
  Result := dxPointDouble(AMapUnitX, AMapUnitY);
end;

function TdxMapControlProjectionCalculator.GetMapUnitX(ALongitude, AOffsetX, AScaleX: Double): Double;
begin
  Result := 0;
  dxAbstractError;
end;

function TdxMapControlProjectionCalculator.GetMapUnitY(ALatitude, AOffsetY, AScaleY: Double): Double;
begin
  Result := 0;
  dxAbstractError;
end;

{ TdxMapControlCustomProjection }

constructor TdxMapControlCustomProjection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FOffsetX := GetDefaultOffsetX;
  FOffsetY := GetDefaultOffsetY;
  FScaleX := GetDefaultScaleX;
  FScaleY := GetDefaultScaleY;
  FProjectionCalculator := CreateProjectionCalculator;
end;

destructor TdxMapControlCustomProjection.Destroy;
begin
  FreeAndNil(FProjectionCalculator);
  inherited;
end;

procedure TdxMapControlCustomProjection.Assign(Source: TPersistent);
begin
  if Source is TdxMapControlCustomProjection then
  begin
    BeginUpdate;
    try
      OffsetX := TdxMapControlCustomProjection(Source).OffsetX;
      OffsetY := TdxMapControlCustomProjection(Source).OffsetY;
      ScaleX := TdxMapControlCustomProjection(Source).ScaleX;
      ScaleY := TdxMapControlCustomProjection(Source).ScaleY;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TdxMapControlCustomProjection.MapUnitToGeoPoint(
  AMapUnit: TdxPointDouble): TdxMapControlGeoPoint;
begin
  Result := InternalMapUnitToGeoPoint(AMapUnit);
end;

function TdxMapControlCustomProjection.GeoPointToMapUnit(
  AGeoPoint: TdxMapControlGeoPoint): TdxPointDouble;
begin
  Result := InternalGeoPointToMapUnit(AGeoPoint);
end;

function TdxMapControlCustomProjection.GeoToKilometersSize(
  AAnchorPoint: TdxMapControlGeoPoint;
  ASize: TdxSizeDouble): TdxSizeDouble;
begin
  Result := dxSizeDouble(ASize.Width * Ratio * Cos(DegToRad(AAnchorPoint.Latitude)),
    ASize.Height * Ratio);
end;

function TdxMapControlCustomProjection.KilometersToGeoSize(
  AAnchorPoint: TdxMapControlGeoPoint;
  ASize: TdxSizeDouble): TdxSizeDouble;
begin
  Result := dxSizeDouble(ASize.Width / Ratio / Cos(DegToRad(AAnchorPoint.Latitude)),
    ASize.Height / Ratio);
end;

procedure TdxMapControlCustomProjection.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxMapControlCustomProjection.Changed;
begin
  if FLockCount > 0 then
    FChanged := True
  else
  begin
    if Assigned(FOnChanged) then
      FOnChanged(Self);
    FChanged := False;
  end;
end;

function TdxMapControlCustomProjection.CreateProjectionCalculator: TdxMapControlProjectionCalculator;
begin
  Result := nil;
end;

procedure TdxMapControlCustomProjection.EndUpdate;
begin
  Dec(FLockCount);
  if (FLockCount = 0) and FChanged then
    Changed;
end;

function TdxMapControlCustomProjection.GetDefaultOffsetX: Double;
begin
  Result := 0.5;
end;

function TdxMapControlCustomProjection.GetDefaultOffsetY: Double;
begin
  Result := 0.5;
end;

function TdxMapControlCustomProjection.GetDefaultScaleX: Double;
begin
  Result := 0.5 / Pi;
end;

function TdxMapControlCustomProjection.GetDefaultScaleY: Double;
begin
  Result := -0.5 / Pi;
end;

function TdxMapControlCustomProjection.InternalGeoPointToMapUnit(
  AGeoPoint: TdxMapControlGeoPoint): TdxPointDouble;
begin
  if FProjectionCalculator <> nil then
    Result := FProjectionCalculator.GetMapUnit(AGeoPoint, dxPointDouble(OffsetX, OffsetY), dxPointDouble(ScaleX, ScaleY))
  else
    Result := dxPointDouble(0, 0);
end;

function TdxMapControlCustomProjection.InternalMapUnitToGeoPoint(
  AMapUnit: TdxPointDouble): TdxMapControlGeoPoint;
begin
  if FProjectionCalculator <> nil then
    Result := FProjectionCalculator.GetGeoPoint(AMapUnit, dxPointDouble(OffsetX, OffsetY), dxPointDouble(ScaleX, ScaleY))
  else
    Result := dxMapControlGeoPoint(0, 0);
end;

function TdxMapControlCustomProjection.IsOffsetXStored: Boolean;
begin
  Result := not SameValue(OffsetX, GetDefaultOffsetX);
end;

function TdxMapControlCustomProjection.IsOffsetYStored: Boolean;
begin
  Result := not SameValue(OffsetY, GetDefaultOffsetY);
end;

function TdxMapControlCustomProjection.IsScaleXStored: Boolean;
begin
  Result := not SameValue(ScaleX, GetDefaultScaleX);
end;

function TdxMapControlCustomProjection.IsScaleYStored: Boolean;
begin
  Result := not SameValue(ScaleY, GetDefaultScaleY);
end;

procedure TdxMapControlCustomProjection.SetOffsetX(const Value: Double);
begin
  if FOffsetX <> Value then
  begin
    FOffsetX := Value;
    Changed;
  end;
end;

procedure TdxMapControlCustomProjection.SetOffsetY(const Value: Double);
begin
  if FOffsetY <> Value then
  begin
    FOffsetY := Value;
    Changed;
  end;
end;

procedure TdxMapControlCustomProjection.SetScaleX(const Value: Double);
begin
  if FScaleX <> Value then
  begin
    FScaleX := Value;
    Changed;
  end;
end;

procedure TdxMapControlCustomProjection.SetScaleY(const Value: Double);
begin
  if FScaleY <> Value then
  begin
    FScaleY := Value;
    Changed;
  end;
end;

{ TdxMapControlSphericalMercatorProjection }

function TdxMapControlSphericalMercatorProjection.CreateProjectionCalculator: TdxMapControlProjectionCalculator;
begin
  Result := TdxMapControlSphericalMercatorProjectionCalculator.Create;
end;

{ TdxMapControlSphericalMercatorProjectionCalculator }

function TdxMapControlSphericalMercatorProjectionCalculator.GetGeoPointLatitude(
  AMapUnitY, AOffsetY, AScaleY: Double): Double;
begin
  Result := RadToDeg(ArcTan(Sinh((AMapUnitY - AOffsetY) / AScaleY)));
end;

function TdxMapControlSphericalMercatorProjectionCalculator.GetGeoPointLongitude(
  AMapUnitX, AOffsetX, AScaleX: Double): Double;
begin
  Result := RadToDeg(AMapUnitX - AOffsetX) / AScaleX;
end;

function TdxMapControlSphericalMercatorProjectionCalculator.GetMapUnitX(
  ALongitude, AOffsetX, AScaleX: Double): Double;
begin
  Result := DegToRad(ALongitude) * AScaleX + AOffsetX;
end;

function TdxMapControlSphericalMercatorProjectionCalculator.GetMapUnitY(
  ALatitude, AOffsetY, AScaleY: Double): Double;
var
  ALatInRadian, ASinLat: Double;
begin
  ALatInRadian := DegToRad(Min(MaxLatitudeInternal, Max(MinLatitudeInternal, ALatitude)));
  ASinLat := Sin(ALatInRadian);
  Result := 0.5 * Ln((1.0 + ASinLat) / (1.0 - ASinLat)) * AScaleY + AOffsetY;
end;

function TdxMapControlSphericalMercatorProjectionCalculator.MaxLatitudeInternal: Double;
begin
  Result := 85.05112878;
end;

function TdxMapControlSphericalMercatorProjectionCalculator.MaxLongitudeInternal: Double;
begin
  Result := 180.0;
end;

function TdxMapControlSphericalMercatorProjectionCalculator.MinLatitudeInternal: Double;
begin
  Result := -85.05112878;
end;

function TdxMapControlSphericalMercatorProjectionCalculator.MinLongitudeInternal: Double;
begin
  Result := -180.0;
end;

{ TdxMapControlEllipticalMercatorProjection }

function TdxMapControlEllipticalMercatorProjection.CreateProjectionCalculator: TdxMapControlProjectionCalculator;
begin
  Result := TdxMapControlEllipticalMercatorProjectionCalculator.Create;
end;

{ TdxMapControlEllipticalMercatorProjectionCalculator }

constructor TdxMapControlEllipticalMercatorProjectionCalculator.Create;
begin
  inherited Create;
  FPiHalf := PI / 2.0;
  FRadiusMajor := 6378137.0;
  FRadiusMinor := 6356752.3142;
  FRadiusRatio := FRadiusMinor / FRadiusMajor;
  FEccentricity := Sqrt(1.0 - Sqr(FRadiusRatio));
  FEccentricityHalf := FEccentricity / 2.0;
end;

function TdxMapControlEllipticalMercatorProjectionCalculator.GetGeoPointLatitude(
  AMapUnitY, AOffsetY, AScaleY: Double): Double;
var
  ATs, APhi, ADphi, ACon: Double;
  I: Integer;
begin
  ATs := Exp(-(AMapUnitY - AOffsetY) / AScaleY);
  APhi := FPiHalf - 2 * ArcTan(ATs);
  for I := 0 to 14 do
  begin
      ACon := FEccentricity * Sin(APhi);
      ADphi := FPiHalf - 2 * ArcTan(ATs * Power((1.0 - ACon) / (1.0 + ACon), FEccentricityHalf)) - APhi;
      APhi := APhi + ADphi;
    if Abs(ADphi) < 0.000000001 then Break;
  end;
  Result := RadToDeg(APhi);
end;

function TdxMapControlEllipticalMercatorProjectionCalculator.GetGeoPointLongitude(
  AMapUnitX, AOffsetX, AScaleX: Double): Double;
begin
  Result := RadToDeg((AMapUnitX - AOffsetX) / AScaleX);
end;

function TdxMapControlEllipticalMercatorProjectionCalculator.GetMapUnitX(
  ALongitude, AOffsetX, AScaleX: Double): Double;
var
  ALonInRadian: Double;
begin
  ALonInRadian := DegToRad(ALongitude);
  Result := ALonInRadian * AScaleX + AOffsetX;
end;

function TdxMapControlEllipticalMercatorProjectionCalculator.GetMapUnitY(
  ALatitude, AOffsetY, AScaleY: Double): Double;
var
  ALatInRadian, ACon, Ats: Double;
begin
  ALatInRadian := DegToRad(Math.Min(89.0, Math.Max(ALatitude, -89.0)));
  ACon := FEccentricity * Sin(ALatInRadian);
  ACon := Power(((1.0 - ACon) / (1.0 + ACon)), FEccentricityHalf);
  Ats := Tan(0.5 * ((PI * 0.5) + ALatInRadian)) * ACon;
  Result := Min(1.0, Max(0.0, Ln(Ats) * AScaleY + AOffsetY));
end;

function TdxMapControlEllipticalMercatorProjectionCalculator.MaxLatitudeInternal: Double;
begin
  Result := 85.08405905;
end;

function TdxMapControlEllipticalMercatorProjectionCalculator.MaxLongitudeInternal: Double;
begin
  Result := 180.0;
end;

function TdxMapControlEllipticalMercatorProjectionCalculator.MinLatitudeInternal: Double;
begin
  Result := -85.08405905;
end;

function TdxMapControlEllipticalMercatorProjectionCalculator.MinLongitudeInternal: Double;
begin
  Result := -180.0;
end;

{ TdxMapControlMillerProjection }

function TdxMapControlMillerProjection.CreateProjectionCalculator: TdxMapControlProjectionCalculator;
begin
  Result := TdxMapControlMillerProjectionCalculator.Create;
end;

function TdxMapControlMillerProjection.GetDefaultScaleY: Double;
begin
  Result := inherited GetDefaultScaleY * 0.8;
end;

{ TdxMapControlMillerProjectionCalculator }

constructor TdxMapControlMillerProjectionCalculator.Create;
begin
  inherited Create;
  FScaleFactor := 2.30341254337639 / PI;
end;

function TdxMapControlMillerProjectionCalculator.GetGeoPointLatitude(AMapUnitY,
  AOffsetY, AScaleY: Double): Double;
var
  Y, ALat: Double;
begin
  Y := ((AMapUnitY - AOffsetY) / AScaleY) * FScaleFactor;
  ALat := 1.25 * ArcTan(Sinh(0.8 * Y));
  Result := Min(MaxLatitudeInternal, Max(MinLatitudeInternal, RadToDeg(ALat)));
end;

function TdxMapControlMillerProjectionCalculator.GetGeoPointLongitude(AMapUnitX,
  AOffsetX, AScaleX: Double): Double;
begin
  Result := RadToDeg((AMapUnitX - AOffsetX) / AScaleX);
end;

function TdxMapControlMillerProjectionCalculator.GetMapUnitX(ALongitude,
  AOffsetX, AScaleX: Double): Double;
var
  ALonInRadian: Double;
begin
  ALonInRadian := DegToRad(ALongitude);
  Result := ALonInRadian * AScaleX + AOffsetX;
end;

function TdxMapControlMillerProjectionCalculator.GetMapUnitY(ALatitude,
  AOffsetY, AScaleY: Double): Double;
var
  ALatInRadian, Y: Double;
begin
  ALatInRadian := DegToRad(Math.Min(MaxLatitudeInternal, Max(ALatitude, MinLatitudeInternal)));
  Y := 1.25 * Ln(Math.Tan((PI / 4.0) + (0.4 * ALatInRadian)));
  Result := Min(1.0, Math.Max(0.0, (Y / FScaleFactor) * AScaleY + AOffsetY));
end;

function TdxMapControlMillerProjectionCalculator.MaxLatitudeInternal: Double;
begin
  Result := 90.0;
end;

function TdxMapControlMillerProjectionCalculator.MaxLongitudeInternal: Double;
begin
  Result := 180.0;
end;

function TdxMapControlMillerProjectionCalculator.MinLatitudeInternal: Double;
begin
  Result := -90.0;
end;

function TdxMapControlMillerProjectionCalculator.MinLongitudeInternal: Double;
begin
  Result := -180.0;
end;

{ TdxMapControlEquirectangularProjection }

function TdxMapControlEquirectangularProjection.CreateProjectionCalculator: TdxMapControlProjectionCalculator;
begin
  Result := TdxMapControlEquirectangularProjectionCalculator.Create;
end;

function TdxMapControlEquirectangularProjection.GetDefaultScaleY: Double;
begin
  Result := inherited GetDefaultScaleY / 2;
end;

{ TdxMapControlEquirectangularProjectionCalculator }

function TdxMapControlEquirectangularProjectionCalculator.GetGeoPointLatitude(
  AMapUnitY, AOffsetY, AScaleY: Double): Double;
begin
  Result := RadToDeg(AMapUnitY - AOffsetY) / (2 * AScaleY);
end;

function TdxMapControlEquirectangularProjectionCalculator.GetGeoPointLongitude(
  AMapUnitX, AOffsetX, AScaleX: Double): Double;
begin
  Result := RadToDeg(AMapUnitX - AOffsetX) / AScaleX;
end;

function TdxMapControlEquirectangularProjectionCalculator.GetMapUnitX(
  ALongitude, AOffsetX, AScaleX: Double): Double;
var
  ALonInRadian: Double;
begin
  ALonInRadian := DegToRad(ALongitude);
  Result := ALonInRadian * AScaleX + AOffsetX;
end;

function TdxMapControlEquirectangularProjectionCalculator.GetMapUnitY(ALatitude,
  AOffsetY, AScaleY: Double): Double;
var
  ALatInRadian: Double;
begin
  ALatInRadian := DegToRad(Min(MaxLatitudeInternal, Max(MinLatitudeInternal, ALatitude)));
  Result := 2 * ALatInRadian * AScaleY + AOffsetY;
end;

function TdxMapControlEquirectangularProjectionCalculator.MaxLatitudeInternal: Double;
begin
  Result := 90.0;
end;

function TdxMapControlEquirectangularProjectionCalculator.MaxLongitudeInternal: Double;
begin
  Result := 180.0;
end;

function TdxMapControlEquirectangularProjectionCalculator.MinLatitudeInternal: Double;
begin
  Result := -90.0;
end;

function TdxMapControlEquirectangularProjectionCalculator.MinLongitudeInternal: Double;
begin
  Result := -180.0;
end;

{ TdxMapControlEqualAreaProjection }

function TdxMapControlEqualAreaProjection.CreateProjectionCalculator: TdxMapControlProjectionCalculator;
begin
  Result := TdxMapControlEqualAreaProjectionCalculator.Create;
end;

function TdxMapControlEqualAreaProjection.GetDefaultScaleY: Double;
begin
  Result := inherited GetDefaultScaleY / PI * 2;
end;

{ TdxMapControlEqualAreaProjectionCalculator }

function TdxMapControlEqualAreaProjectionCalculator.GetGeoPointLatitude(
  AMapUnitY, AOffsetY, AScaleY: Double): Double;
var
  Y: Double;
begin
 Y := EnsureRange(((AMapUnitY - AOffsetY) / AScaleY) / PI, -1, 1);
 Result := RadToDeg(ArcSin(Y));
end;

function TdxMapControlEqualAreaProjectionCalculator.GetGeoPointLongitude(
  AMapUnitX, AOffsetX, AScaleX: Double): Double;
begin
  Result := RadToDeg((AMapUnitX - AOffsetX) / AScaleX);
end;

function TdxMapControlEqualAreaProjectionCalculator.GetMapUnitX(ALongitude,
  AOffsetX, AScaleX: Double): Double;
begin
  Result := DegToRad(ALongitude) * AScaleX + AOffsetX;
end;

function TdxMapControlEqualAreaProjectionCalculator.GetMapUnitY(ALatitude,
  AOffsetY, AScaleY: Double): Double;
begin
  Result := Sin(DegToRad(EnsureRange(ALatitude, MinLatitudeInternal, MaxLatitudeInternal))) * PI * AScaleY + AOffsetY;
end;

function TdxMapControlEqualAreaProjectionCalculator.MaxLatitudeInternal: Double;
begin
  Result := 90.0;
end;

function TdxMapControlEqualAreaProjectionCalculator.MaxLongitudeInternal: Double;
begin
  Result := 180.0;
end;

function TdxMapControlEqualAreaProjectionCalculator.MinLatitudeInternal: Double;
begin
  Result := -90.0;
end;

function TdxMapControlEqualAreaProjectionCalculator.MinLongitudeInternal: Double;
begin
  Result := -180.0;
end;

{ TdxMapControlLambertCylindricalEqualAreaProjectionCalculator }

function TdxMapControlLambertCylindricalEqualAreaProjectionCalculator.GetGeoPointLatitude(AMapUnitY, AOffsetY,
  AScaleY: Double): Double;
begin
  Result := RadToDeg(ArcSin(EnsureRange((AMapUnitY - AOffsetY) / AScaleY / PI, -1, 1)));
end;

function TdxMapControlLambertCylindricalEqualAreaProjectionCalculator.GetGeoPointLongitude(AMapUnitX, AOffsetX,
  AScaleX: Double): Double;
begin
  Result := RadToDeg((AMapUnitX - AOffsetX) / AScaleX);
end;

function TdxMapControlLambertCylindricalEqualAreaProjectionCalculator.GetMapUnitX(ALongitude, AOffsetX,
  AScaleX: Double): Double;
begin
  Result := DegToRad(ALongitude) * AScaleX + AOffsetX;
end;

function TdxMapControlLambertCylindricalEqualAreaProjectionCalculator.GetMapUnitY(ALatitude, AOffsetY,
  AScaleY: Double): Double;
begin
  Result := Sin(DegToRad(EnsureRange(ALatitude, MinLatitudeInternal, MaxLatitudeInternal))) * AScaleY * PI + AOffsetY;
end;

function TdxMapControlLambertCylindricalEqualAreaProjectionCalculator.MaxLatitudeInternal: Double;
begin
  Result := 90;
end;

function TdxMapControlLambertCylindricalEqualAreaProjectionCalculator.MaxLongitudeInternal: Double;
begin
  Result := 180;
end;

function TdxMapControlLambertCylindricalEqualAreaProjectionCalculator.MinLatitudeInternal: Double;
begin
  Result := -90;
end;

function TdxMapControlLambertCylindricalEqualAreaProjectionCalculator.MinLongitudeInternal: Double;
begin
  Result := -180;
end;

{ TdxMapControlLambertCylindricalEqualAreaProjection }

function TdxMapControlLambertCylindricalEqualAreaProjection.CreateProjectionCalculator: TdxMapControlProjectionCalculator;
begin
  Result := TdxMapControlLambertCylindricalEqualAreaProjectionCalculator.Create;
end;

function TdxMapControlLambertCylindricalEqualAreaProjection.GetDefaultScaleY: Double;
begin
  Result := inherited GetDefaultScaleY / PI;
end;

{ TdxMapControlBraunStereographicProjection }

function TdxMapControlBraunStereographicProjection.CreateProjectionCalculator: TdxMapControlProjectionCalculator;
begin
  Result := TdxMapControlBraunStereographicProjectionCalculator.Create;
end;

function TdxMapControlBraunStereographicProjection.GetDefaultScaleY: Double;
begin
  Result := inherited GetDefaultScaleY / PI * 2;
end;

{ TdxMapControlKavrayskiyProjectionCalculator }

function TdxMapControlKavrayskiyProjectionCalculator.GetGeoPoint(AMapUnit, AOffset, AScale: TdxPointDouble): TdxMapControlGeoPoint;
var
  ALatitude, ALongitude: Double;
  ADenom: Double;
  x, y: Double;
begin
  x := (AMapUnit.X - AOffset.X) / AScale.X / Sqrt(3) * PI;
  y := (AMapUnit.Y - AOffset.Y) / AScale.Y / 2;
  ADenom := PI * PI / 3 - y * y;
  ALatitude := EnsureRange(RadToDeg(y), MinLatitudeInternal, MaxLatitudeInternal);
  if ADenom <= 0 then
    x := PI * Sign(x)
  else
    x := x / Sqrt(ADenom);
  ALongitude := EnsureRange(RadToDeg(x), MinLongitudeInternal, MaxLongitudeInternal);
  Result := dxMapControlGeoPoint(ALatitude, ALongitude);
end;

function TdxMapControlKavrayskiyProjectionCalculator.GetMapUnit(AGeoPoint: TdxMapControlGeoPoint; AOffset, AScale: TdxPointDouble): TdxPointDouble;
var
  ALatitude, ALongitude: Double;
  AMapUnitX, AMapUnitY: Double;
begin
  ALongitude := DegToRad(AGeoPoint.Longitude);
  ALatitude := DegToRad(EnsureRange(AGeoPoint.Latitude, MinLatitudeInternal, MaxLatitudeInternal));

  AMapUnitX := ALongitude * Sqrt((PI * PI) / 3 - ALatitude * ALatitude) * AScale.X * Sqrt(3) / PI + AOffset.X;
  AMapUnitY := ALatitude * AScale.Y * 2 + AOffset.Y;

  Result := dxPointDouble(AMapUnitX, AMapUnitY);
end;

function TdxMapControlKavrayskiyProjectionCalculator.MaxLatitudeInternal: Double;
begin
  Result := 90;
end;

function TdxMapControlKavrayskiyProjectionCalculator.MaxLongitudeInternal: Double;
begin
  Result := 180;
end;

function TdxMapControlKavrayskiyProjectionCalculator.MinLatitudeInternal: Double;
begin
  Result := -90;
end;

function TdxMapControlKavrayskiyProjectionCalculator.MinLongitudeInternal: Double;
begin
  Result := -180;
end;

{ TdxMapControlKavrayskiyProjection }

function TdxMapControlKavrayskiyProjection.CreateProjectionCalculator: TdxMapControlProjectionCalculator;
begin
  Result := TdxMapControlKavrayskiyProjectionCalculator.Create;
end;

function TdxMapControlKavrayskiyProjection.GetDefaultScaleY: Double;
begin
  Result := inherited GetDefaultScaleY / 2;
end;

{ TdxMapControlSinusoidalProjection }

function TdxMapControlSinusoidalProjection.CreateProjectionCalculator: TdxMapControlProjectionCalculator;
begin
  Result := TdxMapControlSinusoidalProjectionCalculator.Create;
end;

function TdxMapControlSinusoidalProjection.GetDefaultScaleY: Double;
begin
  Result := inherited GetDefaultScaleY / 2;
end;

{ TdxMapControlEPSG4326Projection }

function TdxMapControlEPSG4326Projection.CreateProjectionCalculator: TdxMapControlProjectionCalculator;
begin
  Result := TdxMapControlEPSG4326ProjectionCalculator.Create;
end;

{ TdxMapControlSinusoidalProjectionCalculator }

function TdxMapControlSinusoidalProjectionCalculator.MaxLatitudeInternal: Double;
begin
  Result := 90;
end;

function TdxMapControlSinusoidalProjectionCalculator.MaxLongitudeInternal: Double;
begin
  Result := 180;
end;

function TdxMapControlSinusoidalProjectionCalculator.MinLatitudeInternal: Double;
begin
  Result := -90;
end;

function TdxMapControlSinusoidalProjectionCalculator.MinLongitudeInternal: Double;
begin
  Result := -180;
end;

function TdxMapControlSinusoidalProjectionCalculator.GetGeoPoint(AMapUnit, AOffset, AScale: TdxPointDouble): TdxMapControlGeoPoint;
var
  ALatitude, ALongitude: Double;
  ANormY: Double;
  x, y, cosy: Double;
begin
  ANormY := EnsureRange(AMapUnit.Y, 0.25, 0.75);
  y := (ANormY - AOffset.Y) / AScale.Y / 2;
  ALatitude := EnsureRange(RadToDeg(y / k), MinLatitudeInternal, MaxLatitudeInternal);
  x := (AMapUnit.X - AOffset.X) / AScale.X;
  cosy := Cos(y);
  if Abs(x - cosy) > Math.MinDouble then
    ALongitude := EnsureRange(RadToDeg(x / cosy / k), MinLongitudeInternal, MaxLongitudeInternal)
  else
    ALongitude := IfThen(y > 0, MaxLongitudeInternal, MinLongitudeInternal);
  Result := dxMapControlGeoPoint(ALatitude, ALongitude);
end;

function TdxMapControlSinusoidalProjectionCalculator.GetMapUnit(AGeoPoint: TdxMapControlGeoPoint; AOffset, AScale: TdxPointDouble): TdxPointDouble;
var
  ALatitude, ALongitude: Double;
  AMapUnitX, AMapUnitY: Double;
begin
  ALongitude := DegToRad(AGeoPoint.Longitude);
  ALatitude := DegToRad(EnsureRange(AGeoPoint.Latitude, MinLatitudeInternal, MaxLatitudeInternal));

  AMapUnitX := ALongitude * Cos(ALatitude) * AScale.X + AOffset.X;
  AMapUnitY := ALatitude * AScale.Y * 2 + AOffset.Y;

  Result := dxPointDouble(AMapUnitX, AMapUnitY);
end;

{ TdxMapControlBraunStereographicProjectionCalculator }

function TdxMapControlBraunStereographicProjectionCalculator.GetGeoPointLatitude(AMapUnitY, AOffsetY,
  AScaleY: Double): Double;
begin
  Result := EnsureRange(RadToDeg(ArcTan((AMapUnitY - AOffsetY) / AScaleY / Radius / PI) * 2),
    MinLatitudeInternal, MaxLatitudeInternal);
end;

function TdxMapControlBraunStereographicProjectionCalculator.GetGeoPointLongitude(AMapUnitX, AOffsetX,
  AScaleX: Double): Double;
begin
  Result := RadToDeg((AMapUnitX - AOffsetX) / AScaleX / Radius);
end;

function TdxMapControlBraunStereographicProjectionCalculator.GetMapUnitX(ALongitude, AOffsetX, AScaleX: Double): Double;
begin
  Result := Radius * DegToRad(ALongitude) * AScaleX + AOffsetX;
end;

function TdxMapControlBraunStereographicProjectionCalculator.GetMapUnitY(ALatitude, AOffsetY, AScaleY: Double): Double;
begin
  Result := Radius * Tan(DegToRad(EnsureRange(ALatitude, MinLatitudeInternal, MaxLatitudeInternal)) / 2) * AScaleY * PI + AOffsetY;
end;

function TdxMapControlBraunStereographicProjectionCalculator.MaxLatitudeInternal: Double;
begin
  Result := 90;
end;

function TdxMapControlBraunStereographicProjectionCalculator.MaxLongitudeInternal: Double;
begin
  Result := 180;
end;

function TdxMapControlBraunStereographicProjectionCalculator.MinLatitudeInternal: Double;
begin
  Result := -90;
end;

function TdxMapControlBraunStereographicProjectionCalculator.MinLongitudeInternal: Double;
begin
  Result := -180;
end;

{ TdxMapControlEPSG4326ProjectionCalculator }

function TdxMapControlEPSG4326ProjectionCalculator.GetGeoPoint(AMapUnit, AOffset,
  AScale: TdxPointDouble): TdxMapControlGeoPoint;
begin
  Result := dxMapControlGeoPoint(EnsureRange((1 - AMapUnit.Y) * 180 - 90, -90, 90),
    EnsureRange(AMapUnit.X * 360 - 180, -180, 180));
end;

function TdxMapControlEPSG4326ProjectionCalculator.GetMapUnit(AGeoPoint: TdxMapControlGeoPoint; AOffset,
  AScale: TdxPointDouble): TdxPointDouble;
begin
  Result := dxPointDouble((AGeoPoint.Longitude + 180) / 360,
    1 - (EnsureRange(AGeoPoint.Latitude, MinLatitudeInternal, MaxLatitudeInternal) + 90) / 180);
end;

function TdxMapControlEPSG4326ProjectionCalculator.MaxLatitudeInternal: Double;
begin
  Result := 90;
end;

function TdxMapControlEPSG4326ProjectionCalculator.MaxLongitudeInternal: Double;
begin
  Result := 180;
end;

function TdxMapControlEPSG4326ProjectionCalculator.MinLatitudeInternal: Double;
begin
  Result := -90;
end;

function TdxMapControlEPSG4326ProjectionCalculator.MinLongitudeInternal: Double;
begin
  Result := -180;
end;

initialization
  dxRegisteredMapProjections.Register(TdxMapControlSphericalMercatorProjection, 'SphericalMercatorProjection|Spherical Mercator');
  dxRegisteredMapProjections.Register(TdxMapControlEllipticalMercatorProjection, 'EllipticalMercatorProjection|Elliptical Mercator');
  dxRegisteredMapProjections.Register(TdxMapControlMillerProjection, 'MillerProjection|Miller Cylindrical');
  dxRegisteredMapProjections.Register(TdxMapControlEquirectangularProjection, 'EquirectangularProjection|Equirectangular');
  dxRegisteredMapProjections.Register(TdxMapControlEqualAreaProjection, 'EqualAreaProjection|Equal Area');
  dxRegisteredMapProjections.Register(TdxMapControlLambertCylindricalEqualAreaProjection, 'LambertCylindricalEqualAreaProjection|Lambert Cylindrical Equal Area');
  dxRegisteredMapProjections.Register(TdxMapControlBraunStereographicProjection, 'BraunStereographicProjection|Braun Stereographic');
  dxRegisteredMapProjections.Register(TdxMapControlKavrayskiyProjection, 'KavrayskiyProjection|Kavrayskiy VII');
  dxRegisteredMapProjections.Register(TdxMapControlSinusoidalProjection, 'SinusoidalProjection|Sinusoidal');
  dxRegisteredMapProjections.Register(TdxMapControlEPSG4326Projection, 'EPSG4326Projection|EPSG:4326');

finalization
  FreeAndNil(FRegisteredProjections);

end.
