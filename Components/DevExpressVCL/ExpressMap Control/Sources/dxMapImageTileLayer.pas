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

unit dxMapImageTileLayer;

interface

{$I cxVer.inc}

uses
  Windows, Types, Classes, SysUtils, Graphics, Math, RTLConsts,
  cxGraphics, cxGeometry, dxCoreClasses, dxGDIPlusClasses,
  dxMapControlTypes, dxMapUnitConverter, dxMapControlProjections,
  dxMapControlMultiScaleTile, dxMapLayer, dxMapControlImageTileProvider;

type
  TdxMapImageTileLayer = class;

  TdxMapImageTileLayerViewInfo = class(TdxMapLayerViewInfo)
  private
    function GetLayer: TdxMapImageTileLayer;
  protected
    procedure InitializeVisibleElements; override;
    property Layer: TdxMapImageTileLayer read GetLayer;
  public
    procedure CalculateBounds; override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxMapImageTileLayer = class(TdxMapLayer)
  private
    FAlphaBlendValue: Byte;
    FProvider: TdxMapControlImageTileProvider;
    FProviderClass: TdxMapControlImageTileProviderClass;
    FMultiScaleImage: TdxMapControlMultiScaleImage;
    function GetIsReady: Boolean;
    function GetProviderClass: TdxMapControlImageTileProviderClass;
    function GetProviderClassName: string;
    procedure SetAlphaBlendValue(Value: Byte);
    procedure SetProvider(const Value: TdxMapControlImageTileProvider);
    procedure SetProviderClass(const Value: TdxMapControlImageTileProviderClass);
    procedure SetProviderClassName(const Value: string);
    procedure UpdateMultiScaleImage;
  protected
    function CreateViewInfo: TdxMapLayerViewInfo; override;
    procedure DoAssign(Source: TPersistent); override;
    function GetMapProjection: TdxMapControlCustomProjection; override;
    function GetMapSizeInPixels(AZoomLevel: Double): TdxSizeDouble; override;
    procedure Paint(ACanvas: TcxCanvas);
    procedure ProviderPropertiesChanged(ASender: TObject);
    procedure PrepareForRendering;
    procedure UpdateTileSource(ATileSource: TdxMapControlMultiScaleTileSource);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IsReady: Boolean read GetIsReady;
    property ProviderClass: TdxMapControlImageTileProviderClass read GetProviderClass write SetProviderClass;
  published
    property AlphaBlendValue: Byte read FAlphaBlendValue write SetAlphaBlendValue default 255;
    property ProviderClassName: string read GetProviderClassName write SetProviderClassName;
    property Provider: TdxMapControlImageTileProvider read FProvider write SetProvider;
  end;

implementation

{ TdxMapImageTileLayerViewInfo }

procedure TdxMapImageTileLayerViewInfo.CalculateBounds;
begin
  inherited CalculateBounds;
  Layer.PrepareForRendering;
end;

procedure TdxMapImageTileLayerViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Layer.Paint(ACanvas);
end;

procedure TdxMapImageTileLayerViewInfo.InitializeVisibleElements;
begin
  Layer.UpdateMultiScaleImage;
  inherited InitializeVisibleElements;
end;

function TdxMapImageTileLayerViewInfo.GetLayer: TdxMapImageTileLayer;
begin
  Result := inherited Layer as TdxMapImageTileLayer;
end;

{ TdxMapImageTileLayer }

constructor TdxMapImageTileLayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlphaBlendValue := 255;
  FMultiScaleImage := TdxMapControlMultiScaleImage.Create(Self);
end;

destructor TdxMapImageTileLayer.Destroy;
begin
  FreeAndNil(FProvider);
  FreeAndNil(FMultiScaleImage);
  inherited;
end;

function TdxMapImageTileLayer.CreateViewInfo: TdxMapLayerViewInfo;
begin
  Result := TdxMapImageTileLayerViewInfo.Create(Self);
end;

procedure TdxMapImageTileLayer.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxMapImageTileLayer then
  begin
    AlphaBlendValue := TdxMapImageTileLayer(Source).AlphaBlendValue;
    ProviderClassName := TdxMapImageTileLayer(Source).ProviderClassName;
    Provider := TdxMapImageTileLayer(Source).Provider;
  end;
end;

function TdxMapImageTileLayer.GetMapProjection: TdxMapControlCustomProjection;
begin
  if Provider <> nil then
    Result := Provider.Projection
  else
    Result := nil;
end;

function TdxMapImageTileLayer.GetMapSizeInPixels(AZoomLevel: Double): TdxSizeDouble;
var
  ASize: Double;
begin
  if Provider <> nil then
    Result := Provider.GetMapSizeInPixels(AZoomLevel)
  else
  begin
    if AZoomLevel < 1 then
      ASize := dxMapControlDefaultMapSize * AZoomLevel
    else
      ASize := dxMapControlDefaultMapSize / 2 * Power(2, AZoomLevel);
    Result := dxSizeDouble(ASize, ASize);
  end;
end;

function TdxMapImageTileLayer.GetIsReady: Boolean;
begin
  Result := FMultiScaleImage.IsReady;
end;

function TdxMapImageTileLayer.GetProviderClass: TdxMapControlImageTileProviderClass;
begin
  Result := FProviderClass;
end;

function TdxMapImageTileLayer.GetProviderClassName: string;
begin
  if FProvider <> nil then
    Result := FProvider.ClassName
  else
    Result := '';
end;

procedure TdxMapImageTileLayer.SetAlphaBlendValue(Value: Byte);
begin
  if FAlphaBlendValue <> Value then
  begin
    FAlphaBlendValue := Value;
    Invalidate;
  end;
end;

procedure TdxMapImageTileLayer.Paint(ACanvas: TcxCanvas);
begin
  FMultiScaleImage.Paint(ACanvas);
end;

procedure TdxMapImageTileLayer.ProviderPropertiesChanged(ASender: TObject);
var
  ATileSource: TdxMapControlMultiScaleTileSource;
begin
  ATileSource := (ASender as TdxMapControlImageTileProvider).TileSource;
  UpdateTileSource(ATileSource);
  InvalidateViewPort;
  Changed(False);
end;

procedure TdxMapImageTileLayer.PrepareForRendering;
begin
  if IsActuallyVisible then
  begin
    Provider.PrepareData;
    FMultiScaleImage.UpdateTileParams;
    Provider.StartLoadTiles;
  end;
end;

procedure TdxMapImageTileLayer.UpdateTileSource(ATileSource: TdxMapControlMultiScaleTileSource);
begin
  FMultiScaleImage.Source := ATileSource;
end;

procedure TdxMapImageTileLayer.SetProvider(
  const Value: TdxMapControlImageTileProvider);
begin
  FProvider.Assign(Value);
end;

procedure TdxMapImageTileLayer.SetProviderClass(
  const Value: TdxMapControlImageTileProviderClass);
begin
  if FProviderClass <> Value then
  begin
    UpdateTileSource(nil);
    FreeAndNil(FProvider);
    FProviderClass := Value;
    if FProviderClass <> nil then
    begin
      FProvider := FProviderClass.Create(Self);
      InvalidateViewPort;
      UpdateMultiScaleImage;
      UpdateTileSource(FProvider.TileSource);
      FProvider.OnChanged := ProviderPropertiesChanged;
    end;
    Changed(False);
  end;
end;

procedure TdxMapImageTileLayer.SetProviderClassName(
  const Value: string);
begin
  ProviderClass := TdxMapControlImageTileProviderClass(dxMapControlImageryDataProviders.FindByClassName(Value));
end;

procedure TdxMapImageTileLayer.UpdateMultiScaleImage;
begin
  FMultiScaleImage.UpdateViewPort(ViewportInPixels, Viewport);
end;

end.
