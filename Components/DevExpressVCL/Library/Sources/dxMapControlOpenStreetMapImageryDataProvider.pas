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

unit dxMapControlOpenStreetMapImageryDataProvider;

interface

{$I cxVer.inc}

uses
  Math, SysUtils, Classes, IdHTTP,
  cxGeometry,
  dxMapControlMultiScaleTile, dxMapLayer,
  dxMapControlTypes, dxMapControlCacheOptions, dxMapControlImageTileProvider;

const
  dxMapControlOpenStreetMapDefaultMaxParallelConnectionCount = 2;

type
  TdxMapControlOpenStreetMapTileSource = class(TdxMapControlMultiScaleTileSource)
  private
    FUrlTemplate: string;
    FSubdomains: TStrings;
  protected
    function TilePrefix: string; override;
  public
    constructor Create(AOwner: TPersistent; AMaxImageSize: Int64; const AUrlTemplate: string; ASubdomains: TStrings); reintroduce;
    function GetTileByZoomLevel(AZoomLevel, ATilePositionX, ATilePositionY: Integer): string; override;
  end;

  TdxMapControlOpenStreetMapImageryDataProvider = class(TdxMapControlImageTileProvider)
  private
    FSubdomains: TStrings;
    FUrlTemplate: string;
    function CalculateTotalImageSize(AZoomLevel: Double): Double;
    procedure SetUrlTemplate(const Value: string);
    procedure SetSubdomains(const Value: TStrings);
  protected
    function CreateTileSource: TdxMapControlMultiScaleTileSource; override;
    procedure DoAssign(Source: TPersistent); override;
    function GetDefaultMaxParallelConnectionCount: Integer; override;
    procedure Update; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function GetMapSizeInPixels(AZoomLevel: Double): TdxSizeDouble; override;
  published
    property CacheOptions;
    property MaxParallelConnectionCount;
    property Subdomains: TStrings read FSubdomains write SetSubdomains;
    property UrlTemplate: string read FUrlTemplate write SetUrlTemplate;
  end;

implementation

const
  OpenStreetDefaultTileUriTemplate = 'http://[subdomain].tile.openstreetmap.org/[z]/[x]/[y].png';
  OpenStreetTileSize = 256;
  OpenStreetMaxZoomLevel = 19;

{ TdxMapControlOpenStreetMapTileSource }

constructor TdxMapControlOpenStreetMapTileSource.Create(AOwner: TPersistent;
  AMaxImageSize: Int64; const AUrlTemplate: string; ASubdomains: TStrings);
begin
  inherited Create(AOwner, AMaxImageSize, AMaxImageSize,  OpenStreetTileSize, OpenStreetTileSize);
  FUrlTemplate := AUrlTemplate;
  FSubdomains := ASubdomains;
end;

function TdxMapControlOpenStreetMapTileSource.TilePrefix: string;
begin
  Result := 'openstreet';
end;

function TdxMapControlOpenStreetMapTileSource.GetTileByZoomLevel(AZoomLevel,
  ATilePositionX, ATilePositionY: Integer): string;
var
  ASubdomain: string;
begin
  if FSubdomains.Count > 0 then
    ASubdomain := FSubdomains[GetSubdomainIndex(FSubdomains.Count)]
  else
    ASubdomain := '';
  Result := StringReplace(FUrlTemplate, '[subdomain]', ASubdomain, []);
  Result := StringReplace(Result, '[z]', '%d', []);
  Result := StringReplace(Result, '[x]', '%d', []);
  Result := StringReplace(Result, '[y]', '%d', []);
  Result := Format(Result, [AZoomLevel, ATilePositionX, ATilePositionY]);
end;

{ TdxMapControlOpenStreetMapImageryDataProvider }

constructor TdxMapControlOpenStreetMapImageryDataProvider.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FSubdomains := TStringList.Create;
  FUrlTemplate := OpenStreetDefaultTileUriTemplate;
  UpdateTileSource;
  FSubdomains.Add('a');
  FSubdomains.Add('b');
  FSubdomains.Add('c');
end;

destructor TdxMapControlOpenStreetMapImageryDataProvider.Destroy;
begin
  FSubdomains.Free;
  inherited;
end;

function TdxMapControlOpenStreetMapImageryDataProvider.GetMapSizeInPixels(AZoomLevel: Double): TdxSizeDouble;
var
  AImageSize: Double;
begin
  AImageSize := ScaleFactor.ApplyF(CalculateTotalImageSize(AZoomLevel));
  Result := dxSizeDouble(AImageSize, AImageSize);
end;

function TdxMapControlOpenStreetMapImageryDataProvider.CreateTileSource: TdxMapControlMultiScaleTileSource;
var
  AMaxImageSize: Int64;
begin
  AMaxImageSize := Round(CalculateTotalImageSize(OpenStreetMaxZoomLevel));
  Result := TdxMapControlOpenStreetMapTileSource.Create(Self, AMaxImageSize, FUrlTemplate, Subdomains);
end;

procedure TdxMapControlOpenStreetMapImageryDataProvider.DoAssign(Source: TPersistent);
var
  AOpenStreetMapImageryDataProvider: TdxMapControlOpenStreetMapImageryDataProvider;
begin
  inherited DoAssign(Source);
  if Source is TdxMapControlOpenStreetMapImageryDataProvider then
  begin
    AOpenStreetMapImageryDataProvider := TdxMapControlOpenStreetMapImageryDataProvider(Source);
    Subdomains := AOpenStreetMapImageryDataProvider.Subdomains;
    UrlTemplate := AOpenStreetMapImageryDataProvider.UrlTemplate;
  end;
end;

function TdxMapControlOpenStreetMapImageryDataProvider.GetDefaultMaxParallelConnectionCount: Integer;
begin
  Result := dxMapControlOpenStreetMapDefaultMaxParallelConnectionCount;
end;

procedure TdxMapControlOpenStreetMapImageryDataProvider.Update;
begin
  UpdateTileSource;
  inherited Update;
end;

function TdxMapControlOpenStreetMapImageryDataProvider.CalculateTotalImageSize(AZoomLevel: Double): Double;
begin
  if AZoomLevel < 1.0 then
    Result := AZoomLevel * OpenStreetTileSize
  else
    Result := Power(2.0, AZoomLevel) * OpenStreetTileSize;
end;

procedure TdxMapControlOpenStreetMapImageryDataProvider.SetSubdomains(const Value: TStrings);
begin
  FSubdomains.Assign(Value);
end;

procedure TdxMapControlOpenStreetMapImageryDataProvider.SetUrlTemplate(
  const Value: string);
begin
  if FUrlTemplate <> Value then
  begin
    FUrlTemplate := Value;
    Update;
  end;
end;

initialization
  dxMapControlImageryDataProviders.Register(TdxMapControlOpenStreetMapImageryDataProvider, 'OpenStreetMapImageryDataProvider');

finalization

end.
