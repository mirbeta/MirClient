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

unit dxMapControlBingMapImageryDataProvider;

interface

{$I cxVer.inc}

uses
  Math, SysUtils, Classes,
  cxGeometry,
  dxMapControlMultiScaleTile, dxMapControlBingImageryServiceInfo, dxMapLayer,
  dxMapControlTypes, dxMapControlCacheOptions,
  dxMapControlImageTileProvider, dxBingMapRESTService, dxBingMapRESTServiceStrs, dxBingMapImageryDataService;

const
  dxMapControlBingMapDefaultMaxParallelConnectionCount = 30;

type
  TdxMapControlBingMapImageryDataProvider = class;

  TdxMapControlBingMapTileSource = class(TdxMapControlMultiScaleTileSource)
  private
    FBingMapProvider: TdxMapControlBingMapImageryDataProvider;
    FServiceInfo: TdxMapControlBingImageryServiceInfo;
    function GetQuadKey(AZoomLevel, ATilePositionX, ATilePositionY: Integer): string;
  protected
    function TilePrefix: string; override;
  public
    constructor Create(ABingMapProvider: TdxMapControlBingMapImageryDataProvider;
      AInfo: TdxMapControlBingImageryServiceInfo); reintroduce;
    function GetTileByZoomLevel(AZoomLevel, ATilePositionX, ATilePositionY: Integer): string; override;
  end;

  TdxBingMapKind = (bmkRoad, bmkArea, bmkHybrid);

  TdxMapControlBingMapImageryDataProvider = class(TdxMapControlImageTileProvider)
  private
    FKind: TdxBingMapKind;
    FServiceInfo: TdxMapControlBingImageryServiceInfo;
    FImageryDataService: TdxBingMapImageryDataService;
    FIsImageryDataDirty: Boolean;
    procedure RequestImageryData;
    function GetBingKey: string;
    procedure SetBingKey(const Value: string);
    procedure SetKind(const Value: TdxBingMapKind);
  protected
    function CreateTileSource: TdxMapControlMultiScaleTileSource; override;
    procedure DoAssign(Source: TPersistent); override;
    function GetDefaultMaxParallelConnectionCount: Integer; override;
    procedure Update; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function GetMapSizeInPixels(AZoomLevel: Double): TdxSizeDouble; override;
    function IsDataValid: Boolean;
    procedure PrepareData; override;
 published
    property BingKey: string read GetBingKey write SetBingKey;
    property CacheOptions;
    property Kind: TdxBingMapKind read FKind write SetKind default bmkRoad;
    property MaxParallelConnectionCount;
  end;

implementation

uses
  dxMapImageTileLayer;

function GetBingBitmapKindPrefix(AMapKind: TdxBingMapKind): string;
begin
  case AMapKind of
    bmkRoad: Result := 'r';
    bmkArea: Result := 'a';
  else//  bmkHybrid
    Result := 'h'
  end;
end;

function GetBingMapKind(AMapKind: TdxBingMapKind): string;
begin
  case AMapKind of
    bmkRoad: Result := 'Road';
    bmkArea: Result := 'Aerial';
  else//  bmkHybrid
    Result := 'AerialWithLabels'
  end;
end;

{ TdxMapControlBingMapImageryDataProvider }

constructor TdxMapControlBingMapImageryDataProvider.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FServiceInfo := TdxMapControlBingImageryServiceInfo.Create;
  FImageryDataService := TdxBingMapImageryDataService.Create;
  FImageryDataService.OnResponse := FServiceInfo.DoOnResponse;
  UpdateTileSource;
end;

destructor TdxMapControlBingMapImageryDataProvider.Destroy;
begin
  FreeAndNil(FImageryDataService);
  FreeAndNil(FServiceInfo);
  inherited Destroy;
end;

function TdxMapControlBingMapImageryDataProvider.GetMapSizeInPixels(AZoomLevel: Double): TdxSizeDouble;
var
  AActualTileSize: TdxSizeDouble;
begin
  AActualTileSize := dxSizeDouble(ScaleFactor.ApplyF(dxBingMapDefaultTileSize), ScaleFactor.ApplyF(dxBingMapDefaultTileSize));
  if AZoomLevel < 1.0 then
    Result := dxSizeDouble(AZoomLevel * AActualTileSize.Width * 2, AZoomLevel * AActualTileSize.Height * 2)
  else
    Result := dxSizeDouble(Power(2.0, AZoomLevel) * AActualTileSize.Width, Power(2.0, AZoomLevel) * AActualTileSize.Height);
end;

function TdxMapControlBingMapImageryDataProvider.IsDataValid: Boolean;
begin
  Result := FServiceInfo.IsValid and not FIsImageryDataDirty;
end;

procedure TdxMapControlBingMapImageryDataProvider.PrepareData;
begin
  RequestImageryData;
end;

function TdxMapControlBingMapImageryDataProvider.CreateTileSource: TdxMapControlMultiScaleTileSource;
begin
  Result := TdxMapControlBingMapTileSource.Create(Self, FServiceInfo);
end;

procedure TdxMapControlBingMapImageryDataProvider.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxMapControlBingMapImageryDataProvider then
  begin
    Kind :=  TdxMapControlBingMapImageryDataProvider(Source).Kind;
    BingKey := TdxMapControlBingMapImageryDataProvider(Source).BingKey;
  end;
end;

function TdxMapControlBingMapImageryDataProvider.GetDefaultMaxParallelConnectionCount: Integer;
begin
  Result := dxMapControlBingMapDefaultMaxParallelConnectionCount;
end;

procedure TdxMapControlBingMapImageryDataProvider.Update;
begin
  FIsImageryDataDirty := True;
  inherited Update;
end;

procedure TdxMapControlBingMapImageryDataProvider.RequestImageryData;
begin
  if not IsDataValid then
  begin
    FImageryDataService.RequestImageryData(GetBingMapKind(FKind));
    FIsImageryDataDirty := False;
  end;
end;

function TdxMapControlBingMapImageryDataProvider.GetBingKey: string;
begin
  Result := FImageryDataService.BingKey;
end;

procedure TdxMapControlBingMapImageryDataProvider.SetBingKey(const Value: string);
begin
  if BingKey <> Value then
  begin
    FImageryDataService.BingKey := Value;
    Update;
  end;
end;

procedure TdxMapControlBingMapImageryDataProvider.SetKind(const Value: TdxBingMapKind);
begin
  if FKind <> Value then
  begin
    if TileSource <> nil then
      TileSource.ClearCache;
    FKind := Value;
    Update;
  end;
end;

{ TdxMapControlBingMapTileSource }

constructor TdxMapControlBingMapTileSource.Create(ABingMapProvider: TdxMapControlBingMapImageryDataProvider;
  AInfo: TdxMapControlBingImageryServiceInfo);
begin
  inherited Create(ABingMapProvider, AInfo.ImageWidth, AInfo.ImageHeight, AInfo.TileWidth, AInfo.TileHeight);
  FBingMapProvider := ABingMapProvider;
  FServiceInfo := AInfo;
end;

function TdxMapControlBingMapTileSource.GetTileByZoomLevel(AZoomLevel, ATilePositionX, ATilePositionY: Integer): string;
begin
  if FBingMapProvider.IsDataValid then
  begin
    Result :=  FServiceInfo.ImageUrlTemplate;
    Result := StringReplace(Result, '{culture}', dxBingMapGetCulture, [rfIgnoreCase]);
    Result := StringReplace(Result, '{subdomain}',
      FServiceInfo.Subdomain[GetSubdomainIndex(FServiceInfo.SubdomainCount)], [rfIgnoreCase]);
    Result := StringReplace(Result, '{quadkey}', GetQuadKey(AZoomLevel, ATilePositionX, ATilePositionY), [rfIgnoreCase]);
  end
  else
    Result := '';
end;

function TdxMapControlBingMapTileSource.TilePrefix: string;
begin
  Result := 'bing_' + GetBingBitmapKindPrefix(FBingMapProvider.Kind);
end;

function TdxMapControlBingMapTileSource.GetQuadKey(AZoomLevel, ATilePositionX, ATilePositionY: Integer): string;
var
  I: Integer;
  ADigitCount: Integer;
  AMask: Integer;
  ADigit: Integer;
begin
  Result := '';
  ADigitCount := Round(AZoomLevel);
  for I := ADigitCount - 1 downto 0 do
  begin
    ADigit := 0;
    AMask := 1 shl (I and 31);
    if (AMask and ATilePositionX) <> 0 then
      Inc(ADigit);
    if (AMask and ATilePositionY) <> 0 then
      Inc(ADigit, 2);
    Result := Result + IntToStr(ADigit);
  end;
end;

initialization
  dxMapControlImageryDataProviders.Register(TdxMapControlBingMapImageryDataProvider, 'BingMapImageryDataProvider');

finalization

end.
