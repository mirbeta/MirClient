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

unit dxMapControlTypes;

interface

{$I cxVer.inc}

uses
  Math, Classes, Types, SysUtils,
  dxCoreClasses, dxCore, cxGeometry;

const
  TempScaleFactor = 2;

type
  TdxMapControlTileIndex = record
    X: Integer;
    Y: Integer;
    Level: Integer;
  end;

  TdxMapControlTileRange = record
    Max: TdxMapControlTileIndex;
    Min: TdxMapControlTileIndex;
  end;

  TdxMapControlGeoPoint = record
    Latitude: Double;
    Longitude: Double;
  public
    function AsString: string;
    function IsEqual(const AGeoPoint: TdxMapControlGeoPoint): Boolean; inline;
    function IsValid: Boolean; inline;
  end;

  TdxMapControlGeoPoints = array of TdxMapControlGeoPoint;

  TdxMapControlGeoRect = record
  public
    function AsString: string;
    function IsEqual(const AGeoRect: TdxMapControlGeoRect): Boolean;
    function IsValid: Boolean;
  case Integer of
    0: (Top, Left, Bottom, Right: Double);
    1: (TopLeft, BottomRight: TdxMapControlGeoPoint);
    2: (NorthLatitude, WestLongitude, SouthLatitude, EastLongitude: Double);
  end;

  TdxMapControlGeoLocation = class(TcxOwnedPersistent)
  private
    FGeoPoint: TdxMapControlGeoPoint;
    FOnChanged: TNotifyEvent;
    function GetLatitude: Double;
    function GetLongitude: Double;
    procedure SetGeoPoint(const Value: TdxMapControlGeoPoint);
    procedure SetLatitude(const Value: Double);
    procedure SetLongitude(const Value: Double);
  protected
    procedure Changed;
    procedure DoAssign(Source: TPersistent); override;
  public
    property GeoPoint: TdxMapControlGeoPoint read FGeoPoint write SetGeoPoint;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Longitude: Double read GetLongitude write SetLongitude;
    property Latitude: Double read GetLatitude write SetLatitude;
  end;

  TdxMapControlGeoRectPersistent = class(TcxGeometryObject)
  private
    FGeoRect: TdxMapControlGeoRect;
    function GetEastLongitude: Double;
    function GetNorthLatitude: Double;
    function GetSouthLatitude: Double;
    function GetWestLongitude: Double;
    procedure SetEastLongitude(const Value: Double);
    procedure SetGeoRect(const Value: TdxMapControlGeoRect);
    procedure SetNorthLatitude(const Value: Double);
    procedure SetSouthLatitude(const Value: Double);
    procedure SetWestLongitude(const Value: Double);
  public
    property GeoRect: TdxMapControlGeoRect read FGeoRect write SetGeoRect;
  published
    property NorthLatitude: Double read GetNorthLatitude write SetNorthLatitude;
    property WestLongitude: Double read GetWestLongitude write SetWestLongitude;
    property SouthLatitude: Double read GetSouthLatitude write SetSouthLatitude;
    property EastLongitude: Double read GetEastLongitude write SetEastLongitude;
  end;

const
  dxMapControlInvalidTileIndex: TdxMapControlTileIndex = (X: -1; Y: -1; Level: -1);
  dxMapControlInvalidLatitude = NaN;
  {$NODEFINE dxMapControlInvalidLatitude}
  {$HPPEMIT    'static const Extended dxMapControlInvalidLatitude = NaN;'}
  dxMapControlInvalidLongitude = NaN;
  {$NODEFINE dxMapControlInvalidLongitude}
  {$HPPEMIT    'static const Extended dxMapControlInvalidLongitude = NaN;'}
  dxMapControlInvalidGeoPoint: TdxMapControlGeoPoint = (Latitude: dxMapControlInvalidLatitude; Longitude: dxMapControlInvalidLongitude);
  dxMapControlInvalidGeoRect: TdxMapControlGeoRect = (
    NorthLatitude: dxMapControlInvalidLatitude; WestLongitude: dxMapControlInvalidLongitude;
    SouthLatitude: dxMapControlInvalidLatitude; EastLongitude: dxMapControlInvalidLongitude);

function dxMapControlTileRangeIsEqual(const ARange1, ARange2: TdxMapControlTileRange): Boolean; inline;
function dxMapControlTileIndex(X, Y, ALevel: Integer): TdxMapControlTileIndex; inline;
function dxMapControlTileIndexIsEqual(const AIndex1, AIndex2: TdxMapControlTileIndex): Boolean; inline;
function dxMapControlTileRange(const AMin, AMax: TdxMapControlTileIndex): TdxMapControlTileRange; inline;
function dxMapControlGeoPoint(ALatitude, ALongitude: Double): TdxMapControlGeoPoint; inline;
function dxMapControlGeoPointIsEqual(const AGeoPoint1, AGeoPoint2: TdxMapControlGeoPoint): Boolean; inline;
function dxMapControlGeoRect(ANorthLatitude, AWestLongitude, ASouthLatitude, AEastLongitude: Double): TdxMapControlGeoRect; inline;

implementation

function dxMapControlTileRangeIsEqual(const ARange1, ARange2: TdxMapControlTileRange): Boolean;
begin
  Result := dxMapControlTileIndexIsEqual(ARange1.Max, ARange2.Max) and
    dxMapControlTileIndexIsEqual(ARange1.Min, ARange2.Min);
end;

function dxMapControlTileIndex(X, Y, ALevel: Integer): TdxMapControlTileIndex;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Level := ALevel;
end;

function dxMapControlTileIndexIsEqual(const AIndex1, AIndex2: TdxMapControlTileIndex): Boolean;
begin
  Result := (AIndex1.X = AIndex2.X) and
    (AIndex1.Y = AIndex2.Y) and (AIndex1.Level = AIndex2.Level);
end;

function dxMapControlTileRange(const AMin, AMax: TdxMapControlTileIndex): TdxMapControlTileRange;
begin
  Result.Max := AMax;
  Result.Min := AMin;
end;

function dxMapControlGeoPoint(ALatitude, ALongitude: Double): TdxMapControlGeoPoint;
begin
  Result.Latitude := ALatitude;
  Result.Longitude := ALongitude;
end;

function dxMapControlGeoPointIsEqual(const AGeoPoint1, AGeoPoint2: TdxMapControlGeoPoint): Boolean;
begin
  Result := SameValue(AGeoPoint1.Latitude, AGeoPoint2.Latitude) and
   SameValue(AGeoPoint1.Longitude, AGeoPoint2.Longitude);
end;

function dxMapControlGeoRect(ANorthLatitude, AWestLongitude, ASouthLatitude, AEastLongitude: Double): TdxMapControlGeoRect;
begin
  Result.NorthLatitude := ANorthLatitude;
  Result.WestLongitude := AWestLongitude;
  Result.SouthLatitude := ASouthLatitude;
  Result.EastLongitude := AEastLongitude;
end;

{ TdxMapControlGeoPoint }

function TdxMapControlGeoPoint.AsString: string;
begin
  Result := Format('%s,%s', [dxFloatToStr(Latitude), dxFloatToStr(Longitude)]);
end;

function TdxMapControlGeoPoint.IsEqual(const AGeoPoint: TdxMapControlGeoPoint): Boolean;
begin
  Result := dxMapControlGeoPointIsEqual(Self, AGeoPoint);
end;

function TdxMapControlGeoPoint.IsValid: Boolean;
begin
  Result := not IsNan(Latitude) and not IsNan(Longitude) and InRange(Latitude, -90, 90) and
    InRange(Longitude, -180, 180);
end;

{ TdxMapControlGeoRect }

function TdxMapControlGeoRect.AsString: string;
begin
  Result := dxMapControlGeoPoint(SouthLatitude, WestLongitude).AsString + ',' +
    dxMapControlGeoPoint(NorthLatitude, EastLongitude).AsString;
end;

function TdxMapControlGeoRect.IsEqual(const AGeoRect: TdxMapControlGeoRect): Boolean;
begin
  Result := TopLeft.IsEqual(AGeoRect.TopLeft) and
    BottomRight.IsEqual(AGeoRect.BottomRight);
end;

function TdxMapControlGeoRect.IsValid: Boolean;
begin
  Result := TopLeft.IsValid and BottomRight.IsValid and
    (TopLeft.Latitude > BottomRight.Latitude) and
    (TopLeft.Longitude < BottomRight.Longitude);
end;

  { TdxMapControlGeoLocation }

procedure TdxMapControlGeoLocation.Changed;
begin
  dxCallNotify(FOnChanged, Self);
end;

procedure TdxMapControlGeoLocation.DoAssign(Source: TPersistent);
begin
  inherited;
  if Source is TdxMapControlGeoLocation then
    GeoPoint := TdxMapControlGeoLocation(Source).GeoPoint;
end;

function TdxMapControlGeoLocation.GetLatitude: Double;
begin
  Result := FGeoPoint.Latitude;
end;

function TdxMapControlGeoLocation.GetLongitude: Double;
begin
  Result := FGeoPoint.Longitude;
end;

procedure TdxMapControlGeoLocation.SetGeoPoint(const Value: TdxMapControlGeoPoint);
begin
  if not dxMapControlGeoPointIsEqual(FGeoPoint, Value) then
  begin
    FGeoPoint := Value;
    Changed;
  end;
end;

procedure TdxMapControlGeoLocation.SetLatitude(const Value: Double);
begin
  if FGeoPoint.Latitude <> Value then
  begin
    FGeoPoint.Latitude := Value;
    Changed;
  end;
end;

procedure TdxMapControlGeoLocation.SetLongitude(const Value: Double);
begin
  if FGeoPoint.Longitude <> Value then
  begin
    FGeoPoint.Longitude := Value;
    Changed;
  end;
end;

{ TdxMapControlGeoRectPersistent }

function TdxMapControlGeoRectPersistent.GetEastLongitude: Double;
begin
  Result := FGeoRect.EastLongitude;
end;

function TdxMapControlGeoRectPersistent.GetNorthLatitude: Double;
begin
  Result := FGeoRect.NorthLatitude;
end;

function TdxMapControlGeoRectPersistent.GetSouthLatitude: Double;
begin
  Result := FGeoRect.SouthLatitude;
end;

function TdxMapControlGeoRectPersistent.GetWestLongitude: Double;
begin
  Result := FGeoRect.WestLongitude;
end;

procedure TdxMapControlGeoRectPersistent.SetEastLongitude(const Value: Double);
begin
  if not SameValue(FGeoRect.EastLongitude, Value) then
  begin
    FGeoRect.EastLongitude := Value;
    DoChange;
  end;
end;

procedure TdxMapControlGeoRectPersistent.SetGeoRect(const Value: TdxMapControlGeoRect);
begin
  if not FGeoRect.IsEqual(Value) then
  begin
    FGeoRect := Value;
    DoChange;
  end;
end;

procedure TdxMapControlGeoRectPersistent.SetNorthLatitude(const Value: Double);
begin
  if not SameValue(FGeoRect.NorthLatitude, Value) then
  begin
    FGeoRect.NorthLatitude := Value;
    DoChange;
  end;
end;

procedure TdxMapControlGeoRectPersistent.SetSouthLatitude(const Value: Double);
begin
  if not SameValue(FGeoRect.SouthLatitude, Value) then
  begin
    FGeoRect.SouthLatitude := Value;
    DoChange;
  end;
end;

procedure TdxMapControlGeoRectPersistent.SetWestLongitude(const Value: Double);
begin
  if not SameValue(FGeoRect.WestLongitude, Value) then
  begin
    FGeoRect.WestLongitude := Value;
    DoChange;
  end;
end;

end.
