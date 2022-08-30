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

unit dxMapControlKmlFileLoader;

interface

{$I cxVer.inc}

uses
  Types, Classes, Graphics, SysUtils, Math, RTLConsts,
  cxGraphics, cxGeometry, dxCore, dxCoreClasses, dxGDIPlusClasses, dxCoreGraphics,
  dxMapControlTypes, dxMapLayer, dxCustomMapItemLayer, dxMapItem,
  dxMapItemStyle, dxKmlElements, dxMapControlHttpRequest, dxMapItemFileLayer;

type
  TdxMapControlKmlFileLoader = class;

  TdxMapControlCustomKmlGeometryConverter = class
  private
    FLoader: TdxMapControlKmlFileLoader;
  protected
    procedure ConvertGeometryCoordinatesToGeoPoints(AGeometry: TdxKmlCoordinatesGeometry;
      AGeoPoints: TdxMapControlGeoPointCollection);
    procedure ConvertKmlStyleToMapStyle(AKmlStyle: TdxKmlStyle; AStyle: TdxMapItemStyle);
    function GetIconHotSpotInFractions(AImageDimension: Integer; AValue: Double; AUnit: TdxKmlUnits): Double;
    procedure GetRealColorValueAndAlpha(AColorStyle: TdxKmlColorStyle;
      out AColor: TColor; out AAlpha: Byte);
    class function KmlElementClass: TdxKmlElementClass; virtual;
    property Loader: TdxMapControlKmlFileLoader read FLoader;
  public
    constructor Create(ALoader: TdxMapControlKmlFileLoader);
    procedure Convert(APlacemark: TdxKmlPlacemark; AGeometry: TdxKmlGeometry;
      AStyle, AStyleHot: TdxKmlStyle); virtual; abstract;
  end;
  TdxMapControlCustomKmlGeometryConverterClass = class of TdxMapControlCustomKmlGeometryConverter;

  TdxMapControlKmlMultiGeometryConverter = class(TdxMapControlCustomKmlGeometryConverter)
  private
    FNestedLevel: Integer;
    FPath: TdxMapPath;
    procedure AddToPath(APolygon: TdxKmlPolygon);
  protected
    class function KmlElementClass: TdxKmlElementClass; override;
  public
    procedure Convert(APlacemark: TdxKmlPlacemark; AGeometry: TdxKmlGeometry;
      AStyle, AStyleHot: TdxKmlStyle); override;
  end;

  TdxMapControlKmlPointConverter = class(TdxMapControlCustomKmlGeometryConverter)
  protected
    class function KmlElementClass: TdxKmlElementClass; override;
  public
    procedure Convert(APlacemark: TdxKmlPlacemark; AGeometry: TdxKmlGeometry;
      AStyle, AStyleHot: TdxKmlStyle); override;
  end;

  TdxMapControlKmlLineStringConverter = class(TdxMapControlCustomKmlGeometryConverter)
  protected
    class function KmlElementClass: TdxKmlElementClass; override;
  public
    procedure Convert(APlacemark: TdxKmlPlacemark; AGeometry: TdxKmlGeometry;
      AStyle, AStyleHot: TdxKmlStyle); override;
  end;

  TdxMapControlKmlLinearRingConverter = class(TdxMapControlKmlLineStringConverter)
  protected
    class function KmlElementClass: TdxKmlElementClass; override;
  public
    procedure Convert(APlacemark: TdxKmlPlacemark; AGeometry: TdxKmlGeometry;
      AStyle, AStyleHot: TdxKmlStyle); override;
  end;

  TdxMapControlKmlPolygonConverter = class(TdxMapControlCustomKmlGeometryConverter)
  protected
    class function KmlElementClass: TdxKmlElementClass; override;
  public
    procedure Convert(APlacemark: TdxKmlPlacemark; AGeometry: TdxKmlGeometry;
      AStyle, AStyleHot: TdxKmlStyle); override;
  end;

  TdxMapControlKmlFileLoader = class
  private
    FMapItems: TdxMapItems;
    FOwner: TdxMapItemFileLayer;
  protected
    function GetIcon(const AIconHRef: string): TdxSmartGlyph;
    property MapItems: TdxMapItems read FMapItems;
  public
    constructor Create(AOwner: TdxMapItemFileLayer);
    procedure LoadFromStream(AFileStream: TStream);
  end;

implementation

type
  TdxMapControlKmlElementConverterFactory = class
  private
    FItems: TdxFastList;
    FConvertersCache: TdxFastObjectList;
    procedure RegisterConverter(AConverterClass: TdxMapControlCustomKmlGeometryConverterClass);
  public
    constructor Create;
    destructor Destroy; override;
    function GetConverter(AKmlGeometry: TdxKmlGeometry): TdxMapControlCustomKmlGeometryConverter;
    procedure InitializeConverters(ALoader: TdxMapControlKmlFileLoader);
    procedure FinalizeConverters;
    procedure RegisterConverters(AConverterClasses: array of TdxMapControlCustomKmlGeometryConverterClass);
  end;

{ TdxMapControlKmlElementConverterFactory }

constructor TdxMapControlKmlElementConverterFactory.Create;
begin
  inherited Create;
  FItems := TdxFastList.Create;
end;

destructor TdxMapControlKmlElementConverterFactory.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxMapControlKmlElementConverterFactory.GetConverter(
  AKmlGeometry: TdxKmlGeometry): TdxMapControlCustomKmlGeometryConverter;
var
  I: Integer;
begin
  Result := nil;
  if AKmlGeometry <> nil then
    for I := 0 to FConvertersCache.Count - 1 do
      if TdxMapControlCustomKmlGeometryConverter(FConvertersCache[I]).KmlElementClass = AKmlGeometry.ClassType then
      begin
        Result := TdxMapControlCustomKmlGeometryConverter(FConvertersCache[I]);
        Break;
      end;
end;

procedure TdxMapControlKmlElementConverterFactory.InitializeConverters(
  ALoader: TdxMapControlKmlFileLoader);
var
  I: Integer;
begin
  FConvertersCache := TdxFastObjectList.Create;
  for I := 0 to FItems.Count - 1 do
    FConvertersCache.Add(TdxMapControlCustomKmlGeometryConverterClass(FItems[I]).Create(ALoader));
end;

procedure TdxMapControlKmlElementConverterFactory.FinalizeConverters;
begin
  FreeAndNil(FConvertersCache);
end;

procedure TdxMapControlKmlElementConverterFactory.RegisterConverters(
  AConverterClasses: array of TdxMapControlCustomKmlGeometryConverterClass);
var
  I: Integer;
begin
  for I := Low(AConverterClasses) to High(AConverterClasses) do
    RegisterConverter(AConverterClasses[I]);
end;

procedure TdxMapControlKmlElementConverterFactory.RegisterConverter(
  AConverterClass: TdxMapControlCustomKmlGeometryConverterClass);
begin
  FItems.Add(AConverterClass);
end;

var
  dxMapControlWebIconCache: TStringList;
  FKmlElementConverterFactory: TdxMapControlKmlElementConverterFactory;

function dxMapControlKmlElementConverters: TdxMapControlKmlElementConverterFactory;
begin
  if FKmlElementConverterFactory = nil then
    FKmlElementConverterFactory := TdxMapControlKmlElementConverterFactory.Create;
  Result := FKmlElementConverterFactory;
end;

{ TdxMapControlCustomKmlGeometryConverter }

constructor TdxMapControlCustomKmlGeometryConverter.Create(
  ALoader: TdxMapControlKmlFileLoader);
begin
  inherited Create;
  FLoader := ALoader;
end;

procedure TdxMapControlCustomKmlGeometryConverter.ConvertGeometryCoordinatesToGeoPoints(
  AGeometry: TdxKmlCoordinatesGeometry;
  AGeoPoints: TdxMapControlGeoPointCollection);
var
  I: Integer;
begin
  AGeoPoints.BeginUpdate;
  try
    for I := 0 to AGeometry.CoordinatesCount - 1 do
    begin
      with AGeoPoints.Add do
      begin
        Longitude := AGeometry.Coordinates[I].Longitude;
        Latitude := AGeometry.Coordinates[I].Latitude;
      end;
    end;
  finally
    AGeoPoints.EndUpdate;
  end;
end;

procedure TdxMapControlCustomKmlGeometryConverter.ConvertKmlStyleToMapStyle(
  AKmlStyle: TdxKmlStyle; AStyle: TdxMapItemStyle);
var
  APolyStyle: TdxKmlPolyStyle;
  ALineStyle: TdxKmlLineStyle;
  AColor: TColor;
  AAlpha: Byte;
begin
  if AKmlStyle <> nil then
  begin
    ALineStyle := AKmlStyle.LineStyle;
    if ALineStyle <> nil then
    begin
      GetRealColorValueAndAlpha(ALineStyle, AColor, AAlpha);
      AStyle.BorderColor := dxColorToAlphaColor(AColor, AAlpha);
      AStyle.BorderWidth := Ceil(ALineStyle.Width);
    end;
    APolyStyle := AKmlStyle.PolyStyle;
    if APolyStyle <> nil then
    begin
      GetRealColorValueAndAlpha(APolyStyle, AColor, AAlpha);
      if not APolyStyle.Outline then
        AStyle.BorderWidth := 0;
      if not APolyStyle.Fill then
        AAlpha := 0;
      AStyle.Color := dxColorToAlphaColor(AColor, AAlpha);
    end;
  end;
end;

function TdxMapControlCustomKmlGeometryConverter.GetIconHotSpotInFractions(
  AImageDimension: Integer; AValue: Double; AUnit: TdxKmlUnits): Double;
begin
  case AUnit of
    kmluPixels:
      Result := AValue / AImageDimension;
    kmluInsetPixels:
      Result := 1 - AValue / AImageDimension;
  else //kmluFraction
    Result := AValue;
  end;
end;

procedure TdxMapControlCustomKmlGeometryConverter.GetRealColorValueAndAlpha(
  AColorStyle: TdxKmlColorStyle; out AColor: TColor; out AAlpha: Byte);
var
  AColorValue: Byte;
  AColorMask: Cardinal;
  I: Integer;
  AColorValueShift: Integer;
begin
  AColor := dxAlphaColorToColor(AColorStyle.Color, AAlpha);
  if AColorStyle.ColorMode = kcmRandom then
  begin
    Randomize;
    for I := 0 to 2 do
    begin
      AColorValueShift := I * 8;
      AColorValue := Byte(AColor shr AColorValueShift);
      if AColorValue = $FF then
      begin
        AColorValue := Random($FF);
        AColorMask := ($FF shl AColorValueShift) xor $FFFFFF;
        AColor := AColor and AColorMask or (AColorValue shl AColorValueShift);
      end;
    end;
  end;
end;

class function TdxMapControlCustomKmlGeometryConverter.KmlElementClass: TdxKmlElementClass;
begin
  Result := nil;
end;

{ TdxMapControlKmlMultiGeometryConverter }

procedure TdxMapControlKmlMultiGeometryConverter.Convert(
  APlacemark: TdxKmlPlacemark; AGeometry: TdxKmlGeometry; AStyle,
  AStyleHot: TdxKmlStyle);
var
  I: Integer;
  AConverter: TdxMapControlCustomKmlGeometryConverter;
  AGeometries: TdxFastObjectList;
begin
  Inc(FNestedLevel);
  AGeometries := TdxKmlMultiGeometry(AGeometry).Geometries;
  for I := 0 to AGeometries.Count - 1 do
  begin
    AConverter := dxMapControlKmlElementConverters.GetConverter(TdxKmlGeometry(AGeometries[I]));
    if AConverter <> nil then
    begin
      if AConverter is TdxMapControlKmlPolygonConverter then
      begin
        if FPath = nil then
        begin
          FPath := Loader.MapItems.Add(TdxMapPath) as TdxMapPath;
          ConvertKmlStyleToMapStyle(AStyle, FPath.Style);
        end;
        AddToPath(TdxKmlPolygon(AGeometries[I]));
      end
      else
        AConverter.Convert(APlacemark, TdxKmlGeometry(AGeometries[I]), AStyle, AStyleHot);
    end;
  end;
  Dec(FNestedLevel);
  if FNestedLevel = 0 then
    FPath := nil;
end;

procedure TdxMapControlKmlMultiGeometryConverter.AddToPath(APolygon: TdxKmlPolygon);
var
  AMapPath: TdxMapPath;
  ASegment: TdxMapPathSegment;
begin
  AMapPath := FPath;
  if (APolygon.InnerBoundaryIs <> nil) and (APolygon.InnerBoundaryIs.LinearRing <> nil) then
  begin
    ASegment := AMapPath.Segments.Add;
    ConvertGeometryCoordinatesToGeoPoints(APolygon.InnerBoundaryIs.LinearRing, ASegment.GeoPoints);
  end;
  if (APolygon.OuterBoundaryIs <> nil) and (APolygon.OuterBoundaryIs.LinearRing <> nil) then
  begin
    ASegment := AMapPath.Segments.Add;
    ConvertGeometryCoordinatesToGeoPoints(APolygon.OuterBoundaryIs.LinearRing, ASegment.GeoPoints);
  end;
end;

class function TdxMapControlKmlMultiGeometryConverter.KmlElementClass: TdxKmlElementClass;
begin
  Result := TdxKmlMultiGeometry;
end;

{ TdxMapControlKmlPointConverter }

procedure TdxMapControlKmlPointConverter.Convert(APlacemark: TdxKmlPlacemark;
  AGeometry: TdxKmlGeometry; AStyle, AStyleHot: TdxKmlStyle);
var
  APoint: TdxKmlPoint;
  ACustomMapItem: TdxMapCustomElement;
  AIconStyle: TdxKmlIconStyle;
  AHotSpot: TdxKmlHotSpot;
  AImageWidth, AImageHeight: Integer;
  ALabelStyle: TdxKmlLabelStyle;
  AColor: TColor;
  AAlpha: Byte;
begin
  APoint := TdxKmlPoint(AGeometry);
  if APoint.CoordinatesCount = 0 then
    Exit;
  ACustomMapItem := Loader.MapItems.Add(TdxMapCustomElement) as TdxMapCustomElement;
  ACustomMapItem.Location.Longitude := APoint.Coordinates[0].Longitude;
  ACustomMapItem.Location.Latitude := APoint.Coordinates[0].Latitude;
  ACustomMapItem.Text := APlacemark.Name;
  if AStyle <> nil then
  begin
    AIconStyle := AStyle.IconStyle;
    if AIconStyle <> nil then
    begin
      if AIconStyle.Icon <> nil then
      begin
        ACustomMapItem.Image := Loader.GetIcon(AIconStyle.Icon.HRef);
        if not SameValue(AIconStyle.Scale, 1) then
          ACustomMapItem.Image.Resize(Round(ACustomMapItem.Image.Width * AIconStyle.Scale),
            Round(ACustomMapItem.Image.Height * AIconStyle.Scale));
        AHotSpot := AIconStyle.HotSpot;
        AImageWidth := ACustomMapItem.Image.Width;
        AImageHeight := ACustomMapItem.Image.Height;
        if (AHotSpot <> nil) and (ACustomMapItem.Image <> nil) and
          (AImageWidth <> 0) and (AImageHeight <> 0) then
          ACustomMapItem.ImageOrigin.Point := dxPointDouble(GetIconHotSpotInFractions(AImageWidth, AHotSpot.X, AHotSpot.XUnits),
            1 - GetIconHotSpotInFractions(AImageHeight, AHotSpot.Y, AHotSpot.YUnits));
      end;
    end;
    ALabelStyle := AStyle.LabelStyle;
    if ALabelStyle <> nil then
    begin
      GetRealColorValueAndAlpha(ALabelStyle, AColor, AAlpha);
      ACustomMapItem.Style.Font.Color := AColor;
    end;
  end;
end;

class function TdxMapControlKmlPointConverter.KmlElementClass: TdxKmlElementClass;
begin
  Result := TdxKmlPoint;
end;

{ TdxMapControlKmlLineStringConverter }

procedure TdxMapControlKmlLineStringConverter.Convert(
  APlacemark: TdxKmlPlacemark; AGeometry: TdxKmlGeometry; AStyle,
  AStyleHot: TdxKmlStyle);
var
  APolylineMapItem: TdxMapPolyline;
begin
  APolylineMapItem := Loader.MapItems.Add(TdxMapPolyline) as TdxMapPolyline;
  ConvertGeometryCoordinatesToGeoPoints(TdxKmlCoordinatesGeometry(AGeometry), APolylineMapItem.GeoPoints);
  ConvertKmlStyleToMapStyle(AStyle, APolylineMapItem.Style);
end;

class function TdxMapControlKmlLineStringConverter.KmlElementClass: TdxKmlElementClass;
begin
  Result := TdxKmlLineString;
end;

{ TdxMapControlKmlLinearRingConverter }

procedure TdxMapControlKmlLinearRingConverter.Convert(
  APlacemark: TdxKmlPlacemark; AGeometry: TdxKmlGeometry; AStyle,
  AStyleHot: TdxKmlStyle);
begin
  inherited;
end;

class function TdxMapControlKmlLinearRingConverter.KmlElementClass: TdxKmlElementClass;
begin
  Result := TdxKmlLinearRing;
end;

{ TdxMapControlKmlPolygonConverter }

procedure TdxMapControlKmlPolygonConverter.Convert(APlacemark: TdxKmlPlacemark;
  AGeometry: TdxKmlGeometry; AStyle, AStyleHot: TdxKmlStyle);
var
  APolygon: TdxKmlPolygon;
  AMapPath: TdxMapPath;
  ASegment: TdxMapPathSegment;
begin
  APolygon := TdxKmlPolygon(AGeometry);
  AMapPath := Loader.MapItems.Add(TdxMapPath) as TdxMapPath;
  if (APolygon.InnerBoundaryIs <> nil) and (APolygon.InnerBoundaryIs.LinearRing <> nil) then
  begin
    ASegment := AMapPath.Segments.Add;
    ConvertGeometryCoordinatesToGeoPoints(APolygon.InnerBoundaryIs.LinearRing, ASegment.GeoPoints);
  end;
  if (APolygon.OuterBoundaryIs <> nil) and (APolygon.OuterBoundaryIs.LinearRing <> nil) then
  begin
    ASegment := AMapPath.Segments.Add;
    ConvertGeometryCoordinatesToGeoPoints(APolygon.OuterBoundaryIs.LinearRing, ASegment.GeoPoints);
  end;
  ConvertKmlStyleToMapStyle(AStyle, AMapPath.Style);
end;

class function TdxMapControlKmlPolygonConverter.KmlElementClass: TdxKmlElementClass;
begin
  Result := TdxKmlPolygon;
end;

{ TdxMapControlKmlFileLoader }

constructor TdxMapControlKmlFileLoader.Create(
  AOwner: TdxMapItemFileLayer);
begin
  inherited Create;
  FOwner := AOwner;
  FMapItems := FOwner.MapItems;
end;

procedure TdxMapControlKmlFileLoader.LoadFromStream(AFileStream: TStream);
var
  AKmlRoot: TdxKmlRoot;
  I: Integer;
  APlacemark: TdxKmlPlacemark;
  AKmlStyle, AKmlStyleHot: TdxKmlStyle;
  AConverter: TdxMapControlCustomKmlGeometryConverter;
begin
  AKmlRoot := TdxKmlRoot.Create;
  try
    AKmlRoot.Parse(AFileStream);
    MapItems.BeginUpdate;
    dxMapControlKmlElementConverters.InitializeConverters(Self);
    dxMapControlWebIconCache := TStringList.Create;
    try
      for I := 0 to AKmlRoot.Placemarks.Count - 1 do
      begin
        APlacemark := AKmlRoot.Placemarks[I] as TdxKmlPlacemark;
        AKmlRoot.ResolveElementStyles(APlacemark, AKmlStyle, AKmlStyleHot);
        AConverter := dxMapControlKmlElementConverters.GetConverter(APlacemark.Geometry);
        if AConverter <> nil then
          AConverter.Convert(APlacemark, APlacemark.Geometry, AKmlStyle, AKmlStyleHot);
      end;
    finally
      for I := 0 to dxMapControlWebIconCache.Count - 1 do
        dxMapControlWebIconCache.Objects[I].Free;
      FreeAndNil(dxMapControlWebIconCache);
      dxMapControlKmlElementConverters.FinalizeConverters;
      MapItems.EndUpdate;
    end;
  finally
    AKmlRoot.Free;
  end;
end;

function TdxMapControlKmlFileLoader.GetIcon(const AIconHRef: string): TdxSmartGlyph;
var
  AStream: TStream;
  AIconIndex: Integer;
begin
  Result := nil;
  AIconIndex := dxMapControlWebIconCache.IndexOf(AIconHRef);
  if AIconIndex <> -1 then
    Result := dxMapControlWebIconCache.Objects[AIconIndex] as TdxSmartGlyph
  else
  begin
    AStream := TMemoryStream.Create;
    try
      if GetContent(AIconHRef, AStream) then
      begin
        AStream.Position := 0;
        Result := TdxSmartGlyph.CreateFromStream(AStream);
        dxMapControlWebIconCache.AddObject(AIconHRef, Result);
      end;
    finally
      AStream.Free;
    end;
  end;
end;

initialization
  dxMapControlKmlElementConverters.RegisterConverters([TdxMapControlKmlPointConverter,
    TdxMapControlKmlLineStringConverter, TdxMapControlKmlLinearRingConverter,
    TdxMapControlKmlPolygonConverter, TdxMapControlKmlMultiGeometryConverter]);

finalization
  FreeAndNil(FKmlElementConverterFactory);

end.
