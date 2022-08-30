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

unit dxMapControlImageTileProvider;

interface

{$I cxVer.inc}

uses
  Types, Classes, SysUtils, Graphics, Math,
  dxCoreClasses, cxGraphics, cxClasses, cxGeometry,
  dxMapControlCacheOptions, dxMapControlMultiScaleTile, dxMapControlProjections;

type
  TdxMapControlImageTileProvider = class(TcxOwnedPersistent)
  private
    FCacheOptions: TdxMapControlCacheOptions;
    FMaxParallelConnectionCount: Integer;
    FProjection: TdxMapControlCustomProjection;
    FTileSource: TdxMapControlMultiScaleTileSource;
    FOnChanged: TNotifyEvent;
    function IsMaxParallelConnectionCountStored: Boolean;
    function GetScaleFactor: TdxScaleFactor;
    procedure SetCacheOptions(const Value: TdxMapControlCacheOptions);
    procedure SetMaxParallelConnectionCount(AValue: Integer);
    procedure SetProjection(const Value: TdxMapControlCustomProjection);
  protected
    procedure CacheOptionsChanged(Sender: TObject);
    function CreateProjection: TdxMapControlCustomProjection; virtual;
    function CreateTileSource: TdxMapControlMultiScaleTileSource; virtual;
    procedure DoAssign(Source: TPersistent); override;
    function GetDefaultMaxParallelConnectionCount: Integer; virtual; abstract;
    procedure ProjectionChanged(Sender: TObject);
    procedure Update; virtual;
    procedure UpdateTileSource; virtual;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function GetMapSizeInPixels(AZoomLevel: Double): TdxSizeDouble; virtual; abstract;
    procedure PrepareData; virtual;
    procedure StartLoadTiles;

    property CacheOptions: TdxMapControlCacheOptions read FCacheOptions write SetCacheOptions;
    property MaxParallelConnectionCount: Integer read FMaxParallelConnectionCount
      write SetMaxParallelConnectionCount stored IsMaxParallelConnectionCountStored;
    property Projection: TdxMapControlCustomProjection read FProjection write SetProjection;
    property TileSource: TdxMapControlMultiScaleTileSource read FTileSource;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;
  TdxMapControlImageTileProviderClass = class of TdxMapControlImageTileProvider;

  function dxMapControlImageryDataProviders: TcxRegisteredClasses;

implementation

uses
  dxMapImageTileLayer;

type
  TdxMapImageTileLayerAccess = class(TdxMapImageTileLayer);

var
  FRegisteredImageryDataProviders: TcxRegisteredClasses;

function dxMapControlImageryDataProviders: TcxRegisteredClasses;
begin
  if FRegisteredImageryDataProviders = nil then
    FRegisteredImageryDataProviders := TcxRegisteredClasses.Create;
  Result := FRegisteredImageryDataProviders;
end;

{ TdxMapControlMapDataProvider }

constructor TdxMapControlImageTileProvider.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FCacheOptions := TdxMapControlCacheOptions.Create(Self);
  FCacheOptions.OnChanged := CacheOptionsChanged;
  FMaxParallelConnectionCount := GetDefaultMaxParallelConnectionCount;
  FProjection := CreateProjection;
  FProjection.OnChanged := ProjectionChanged;
end;

destructor TdxMapControlImageTileProvider.Destroy;
begin
  FreeAndNil(FProjection);
  FreeAndNil(FTileSource);
  FreeAndNil(FCacheOptions);
  inherited;
end;

procedure TdxMapControlImageTileProvider.PrepareData;
begin
  // do nothing
end;

procedure TdxMapControlImageTileProvider.StartLoadTiles;
begin
  if FTileSource <> nil then
    FTileSource.StartLoadTiles;
end;

procedure TdxMapControlImageTileProvider.CacheOptionsChanged(Sender: TObject);
begin
  if FTileSource <> nil then
    TileSource.UpdateCacheOptions;
end;

function TdxMapControlImageTileProvider.CreateProjection: TdxMapControlCustomProjection;
begin
  Result := TdxMapControlSphericalMercatorProjection.Create(nil);
end;

function TdxMapControlImageTileProvider.CreateTileSource: TdxMapControlMultiScaleTileSource;
begin
  Result := nil;
end;

procedure TdxMapControlImageTileProvider.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxMapControlImageTileProvider then
  begin
    CacheOptions := TdxMapControlImageTileProvider(Source).CacheOptions;
    MaxParallelConnectionCount := TdxMapControlImageTileProvider(Source).MaxParallelConnectionCount;
  end;
end;

procedure TdxMapControlImageTileProvider.ProjectionChanged(Sender: TObject);
begin
  Update;
end;

procedure TdxMapControlImageTileProvider.Update;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TdxMapControlImageTileProvider.UpdateTileSource;
begin
  FreeAndNil(FTileSource);
  if not (csDesigning in (Owner as TComponent).ComponentState) then
    FTileSource := CreateTileSource;
end;

function TdxMapControlImageTileProvider.IsMaxParallelConnectionCountStored: Boolean;
begin
  Result := FMaxParallelConnectionCount <> GetDefaultMaxParallelConnectionCount;
end;

function TdxMapControlImageTileProvider.GetScaleFactor: TdxScaleFactor;
begin
  Result := TdxMapImageTileLayerAccess(Owner).ScaleFactor;
end;

procedure TdxMapControlImageTileProvider.SetCacheOptions(
  const Value: TdxMapControlCacheOptions);
begin
  FCacheOptions.Assign(Value);
end;

procedure TdxMapControlImageTileProvider.SetMaxParallelConnectionCount(
  AValue: Integer);
begin
  AValue := Max(1, Min(AValue, 30));
  if FMaxParallelConnectionCount <> AValue then
  begin
    FMaxParallelConnectionCount := AValue;
    if TileSource <> nil then
      TileSource.UpdateConnectionSettings;
  end;
end;

procedure TdxMapControlImageTileProvider.SetProjection(
  const Value: TdxMapControlCustomProjection);
begin
  FProjection.Assign(Value);
end;

initialization

finalization
  FreeAndNil(FRegisteredImageryDataProviders);

end.
