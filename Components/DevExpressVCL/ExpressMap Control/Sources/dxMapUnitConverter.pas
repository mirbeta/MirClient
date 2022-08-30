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

unit dxMapUnitConverter;

interface

{$I cxVer.inc}

uses
  SysUtils, Math, Types,
  dxCoreClasses, cxGeometry,
  dxMapControlTypes, dxMapControlProjections;

type
  TdxMapUnitConverter = class
  public
    function ScreenPointToMapUnit(APoint: TdxPointDouble; AViewport: TdxRectDouble; AViewportInPixels: TSize): TdxPointDouble;
    function MapUnitToScreenPoint(AMapUnit: TdxPointDouble; AViewport: TdxRectDouble; AViewportInPixels: TSize): TdxPointDouble;
    function GeoPointToMapUnit(const APoint: TdxMapControlGeoPoint;
      AProjection: TdxMapControlCustomProjection; AShouldNormalize: Boolean = True): TdxPointDouble;
    function MapUnitToGeoPoint(AUnit: TdxPointDouble;
      AProjection: TdxMapControlCustomProjection; AShouldNormalize: Boolean = True): TdxMapControlGeoPoint;
  end;

function dxMapGeoPointNormalize(const APoint: TdxMapControlGeoPoint): TdxMapControlGeoPoint;{$IFDEF DELPHI15} inline;{$ENDIF}
function dxMapPointNormalize(const APoint: TdxPointDouble): TdxPointDouble; inline;

implementation

function NormalizeRadian(AAngleRadian: Double): Double;{$IFDEF DELPHI15} inline;{$ENDIF}
begin
  Result := ArcTan2(Sin(AAngleRadian), Cos(AAngleRadian));
end;

function NormalizeDegree(AAngleDegree: Double): Double;{$IFDEF DELPHI15} inline;{$ENDIF}
begin
  Result := RadToDeg(NormalizeRadian(DegToRad(AAngleDegree)));
end;

function dxMapGeoPointNormalize(const APoint: TdxMapControlGeoPoint): TdxMapControlGeoPoint;
begin
  Result := dxMapControlGeoPoint(Max(-90, Min(APoint.Latitude, 90)),
    NormalizeDegree(APoint.Longitude));
end;

function dxMapPointNormalize(const APoint: TdxPointDouble): TdxPointDouble;
begin
  Result := dxPointDouble(Max(0, Min(APoint.X, 1.0)), Math.Max(0, Min(APoint.Y, 1.0)));
end;

{ TdxMapUnitConverter }

function TdxMapUnitConverter.GeoPointToMapUnit(
  const APoint: TdxMapControlGeoPoint; AProjection: TdxMapControlCustomProjection;
  AShouldNormalize: Boolean = True): TdxPointDouble;
var
  AGeoPoint: TdxMapControlGeoPoint;
begin
  if AShouldNormalize then
    AGeoPoint := dxMapGeoPointNormalize(APoint)
  else
    AGeoPoint := APoint;
  Result := AProjection.GeoPointToMapUnit(AGeoPoint);
end;

function TdxMapUnitConverter.MapUnitToGeoPoint(
  AUnit: TdxPointDouble; AProjection: TdxMapControlCustomProjection;
  AShouldNormalize: Boolean = True): TdxMapControlGeoPoint;
var
  AMapPoint: TdxPointDouble;
begin
  if AShouldNormalize then
    AMapPoint := dxMapPointNormalize(AUnit)
  else
    AMapPoint := AUnit;
  Result := AProjection.MapUnitToGeoPoint(AMapPoint);
end;

function TdxMapUnitConverter.MapUnitToScreenPoint(
  AMapUnit: TdxPointDouble; AViewport: TdxRectDouble;
  AViewportInPixels: TSize): TdxPointDouble;
var
  AUnitFactorX, AUnitFactorY: Double;
begin
  if (AViewport.Width = 0) or (AViewport.Height = 0) then
    Result := dxNullPointDouble
  else
  begin
    AUnitFactorX := AViewportInPixels.cx / AViewport.Width;
    AUnitFactorY := AViewportInPixels.cy / AViewport.Height;
    Result := dxPointDouble((AMapUnit.X - AViewport.Left) * AUnitFactorX,
      (AMapUnit.Y - AViewport.Top) * AUnitFactorY);
  end;
end;

function TdxMapUnitConverter.ScreenPointToMapUnit(
  APoint: TdxPointDouble; AViewport: TdxRectDouble;
  AViewportInPixels: TSize): TdxPointDouble;
var
  APixelFactorX, APixelFactorY: Double;
begin
  if (AViewportInPixels.cx = 0) or (AViewportInPixels.cy = 0) then
    Result := dxNullPointDouble
  else
  begin
    APixelFactorX := AViewport.Width / AViewportInPixels.cx;
    APixelFactorY := AViewport.Height / AViewportInPixels.cy;
    Result.X := Max(Min(APoint.X * APixelFactorX + AViewport.Left, 1.0), 0);
    Result.Y := Max(Min(APoint.Y * APixelFactorY + AViewport.Top, 1.0), 0);
  end;
end;

end.
