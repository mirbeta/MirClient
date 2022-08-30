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

unit dxMapControlMultiScaleTile;

interface

{$I cxVer.inc}

uses
  Windows, SysUtils, Classes, Types, Math, RTLConsts, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, cxGeometry, dxGdiPlusClasses, cxClasses, cxGraphics,
  dxMapControlTypes, dxMapLayer, dxMapControlCacheOptions,
  dxMapControlDiskCache, dxMapUnitConverter, dxMapControlHttpRequest;

type
  TdxMapControlMultiScaleImage = class;
  TdxMapControlMultiScaleViewport = class;
  TdxMapControlMultiScaleTile = class;
  TdxMapControlMultiScaleTileSource = class;
  TdxMapControlTileImageSource = class;
  TdxMapControlTileImageLoadingTask = class;
  TdxMapControlBitmapTile = class;

  TdxMapControlTileImageSourceLoadingQueueManager = class
  private
    FMaxParallelDownloads: Integer;
    FTaskDestroyingLock: TRTLCriticalSection;
    FTileSource: TdxMapControlMultiScaleTileSource;
    FTileImageSourcesQueue: TThreadList;
    FThreads: TdxFastObjectList;
    procedure CreateThreads;
    procedure RecreateThreads;
    procedure SetMaxParallelDownloads(const Value: Integer);
    procedure TerminateThreads;
  protected
    function CreateTileImageLoadingTask(ATileSource: TdxMapControlTileImageSource;
      AUri: string): TdxMapControlTileImageLoadingTask;
    procedure DestroyTileImageLoadingTask(var ATask: TdxMapControlTileImageLoadingTask);
    procedure ImageLoaded(ASource: TdxMapControlTileImageSource);
    procedure LockTaskDestroying;
    procedure StartLoadTiles;
    procedure StopThreads;
    function TaskExists(ASource: TdxMapControlTileImageSource): Boolean;
    procedure UnlockTaskDestroying;
    procedure WakeThreads;
    property TileImageSourcesQueue: TThreadList read FTileImageSourcesQueue;
  public
    constructor Create(ATileSource: TdxMapControlMultiScaleTileSource; AMaxParallelDownloads: Integer);
    destructor Destroy; override;
    procedure Clear;
    function DelayedDestroying(ASource: TdxMapControlTileImageSource): Boolean;
    procedure Push(ATask: TdxMapControlTileImageLoadingTask);
    property MaxParallelDownloads: Integer read FMaxParallelDownloads write SetMaxParallelDownloads;
  end;

  TdxMapControlTileImageSources = class(TcxDoublyLinkedObjectList)
  public
    function AddSource(const AIndex: TdxMapControlTileIndex; ATileSize: TSize): TdxMapControlTileImageSource;
    procedure MakeLast(AImageTileSource: TdxMapControlTileImageSource);
  end;

  TdxMapControlTileImageSourceStorage = class
  private
    FTimeSortedTileSources: TdxMapControlTileImageSources;
    FTileImageSources: TDictionary<TdxMapControlTileIndex, TdxMapControlTileImageSource>;
  protected
    procedure CheckMemoryLimit(ALimit: Int64; AExtraImageSources: TList<TdxMapControlTileImageSource>);
    function CreateTileImageSource(const AIndex: TdxMapControlTileIndex; ATileSize: TSize): TdxMapControlTileImageSource;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Extract(ASource: TdxMapControlTileImageSource): TdxMapControlTileImageSource;
    procedure Remove(ASource: TdxMapControlTileImageSource);
    function TryGetValue(const AKey: TdxMapControlTileIndex;
      out AValue: TdxMapControlTileImageSource): Boolean;
  end;

  TdxMapControlMultiScaleTileSource = class
  private
    FCurrentSubdomainIndex: Integer;
    FDiskCache: TdxMapControlDiskCache;
    FImageHeight: Int64;
    FImageWidth: Int64;
    FLoadingQueueManager: TdxMapControlTileImageSourceLoadingQueueManager;
    FMaxZoomLevel: Integer;
    FOwner: TPersistent;
    FTileCountRatio: Double;
    FTileSize: TSize;
    FTileSourceStorage: TdxMapControlTileImageSourceStorage;
    procedure CheckMemoryLimit;
    function GetCacheOptions: TdxMapControlCacheOptions;
    function GetMaxParallelConnections: Integer;
    function GetScaleFactor: TdxScaleFactor;
    procedure UpdateDiskCache;
  protected
    function GetSubdomainIndex(ASubdomainCount: Integer): Integer;
    procedure GetTileImageSource(AIndex: TdxMapControlTileIndex; ABitmapTile: TdxMapControlBitmapTile);
    function GetTileLayers(ATileLevel, ATilePositionX, ATilePositionY: Integer): string; virtual;
    function PeekTileImageSource(AIndex: TdxMapControlTileIndex): TdxMapControlTileImageSource;
    function TilePrefix: string; virtual;

    property CacheOptions: TdxMapControlCacheOptions read GetCacheOptions;
    property LoadingQueueManager: TdxMapControlTileImageSourceLoadingQueueManager read FLoadingQueueManager;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AOwner: TPersistent; AImageWidth: Int64; AImageHeight: Int64; ATileWidth: Integer;
      ATileHeight: Integer); virtual;
    destructor Destroy; override;
    procedure ClearCache;
    function GetTileByZoomLevel(AZoomLevel, ATilePositionX, ATilePositionY: Integer): string; virtual; abstract;
    procedure PushToDisk(ATileSource: TdxMapControlTileImageSource);
    procedure StartLoadTiles;
    procedure UpdateCacheOptions;
    procedure UpdateConnectionSettings;

    property MaxZoomLevel: Integer read FMaxZoomLevel;
    property Owner: TPersistent read FOwner;
    property TileCountRatio: Double read FTileCountRatio;
    property TileSize: TSize read FTileSize;
  end;

  TdxMapControlTileStatus = (mptsNotReady, mptsReady, mptsLoading, mptsFailed);

  TdxMapControlTileImageSource = class(TcxDoublyLinkedObject)
  private
    FBitmapTiles: TdxFastObjectList;
    FIndex: TdxMapControlTileIndex;
    FNeedToBeDestoying: Boolean;
    FSize: TSize;
    FSource: TdxSmartImage;
    FStatus: TdxMapControlTileStatus;
    FStatusLocker: TRTLCriticalSection;
    FTask: TdxMapControlTileImageLoadingTask;
    FOnImageLoaded: TNotifyEvent;
  protected
    procedure AddLink(ABitmapTile: TdxMapControlBitmapTile);
    function CreateImage(AStream: TStream): Boolean;
    procedure RaiseImageCanceled;
    procedure RaiseImageFailed;
    procedure RaiseImageLoaded;
    procedure RemoveLink(ABitmapTile: TdxMapControlBitmapTile);
    procedure RemoveLinks;
    procedure SetStatusSafe(AStatus: TdxMapControlTileStatus);
    property Task: TdxMapControlTileImageLoadingTask read FTask write FTask;
  public
    constructor Create(ATileIndex: TdxMapControlTileIndex; AExpectedTileSize: TSize);
    destructor Destroy; override;
    function GetMemoryUsage: Integer; virtual;
    function IsLinked: Boolean;

    property Index: TdxMapControlTileIndex read FIndex;
    property Status: TdxMapControlTileStatus read FStatus;
    property Source: TdxSmartImage read FSource;
    property OnImageLoaded: TNotifyEvent read FOnImageLoaded write FOnImageLoaded;
  end;

  TdxMapControlTileImageLoadingTask = class
  private
    FLoadingQueueManager: TdxMapControlTileImageSourceLoadingQueueManager;
    FTileImageSource: TdxMapControlTileImageSource;
    FUri: string;
  protected
    function DoLoad: Boolean; virtual; abstract;
    procedure RaiseImageFailed;
    procedure RaiseImageLoaded; virtual;

    property LoadingQueueManager: TdxMapControlTileImageSourceLoadingQueueManager read
      FLoadingQueueManager write FLoadingQueueManager;
    property TileImageSource: TdxMapControlTileImageSource read FTileImageSource;
    property Uri: string read FUri;
  public
    constructor Create(ATileImageSource: TdxMapControlTileImageSource; const AUri: string);
    destructor Destroy; override;
    function Load: Boolean;
  end;

  TdxMapControlTileImageRemoteLoadingTask = class(TdxMapControlTileImageLoadingTask)
  private
    procedure DoRequest(ASender: TObject; ARequestMode: TdxMapControlHttpRequestMode;
      ACount: Int64; var AIsCancelled: Boolean);
  protected
    function DoLoad: Boolean; override;
    procedure RaiseImageLoaded; override;
  end;

  TdxMapControlTileImageDiskLoadingTask = class(TdxMapControlTileImageLoadingTask)
  protected
    function DoLoad: Boolean; override;
  end;

  TdxMapControlMultiScaleImage = class
  private
    FLayer: TdxMapLayer;
    FRenderItems: TdxFastObjectList;
    FViewport: TdxMapControlMultiScaleViewport;
    function GetIsReady: Boolean;
    function GetSource: TdxMapControlMultiScaleTileSource;
    procedure SetSource(const Value: TdxMapControlMultiScaleTileSource);
  protected
    function CreateTile(AViewport: TdxMapControlMultiScaleViewport;
      X, Y: Integer): TdxMapControlMultiScaleTile;
    function GetLayerAlphaBlendValue: Byte;
    procedure InvalidateRect(const ARect: TRect);
    property Viewport: TdxMapControlMultiScaleViewport read FViewport;
  public
    constructor Create(ALayer: TdxMapLayer);
    destructor Destroy; override;
    procedure UpdateTileParams;
    procedure UpdateViewPort(const AViewPortSizeInPixels: TSize; const AViewPortBounds: TdxRectDouble);
    procedure Paint(ACanvas: TcxCanvas);

    property IsReady: Boolean read GetIsReady;
    property Layer: TdxMapLayer read FLayer;
    property RenderItems: TdxFastObjectList read FRenderItems;
    property Source: TdxMapControlMultiScaleTileSource read GetSource write SetSource;
  end;

  TdxMapControlTilesDimension = array of array of TdxMapControlMultiScaleTile;

  TdxMapControlMultiScaleViewport = class
  private
    FTileSource: TdxMapControlMultiScaleTileSource;
    FScale: TdxPointDouble;
    FOffset: TPoint;
    FViewSize: TSize;
    FTileRange: TdxMapControlTileRange;
    FArrangedTiles: TSize;
    FZoomLevelTileCount: Integer;
    FZoomLevel: Integer;
    FTiles: TdxMapControlTilesDimension;
    FTileFactory: TdxMapControlMultiScaleImage;
    FInflateCount: Integer;
    FBounds: TdxRectDouble;
    function GetAspectRatio: Double;
    function GetInflatedTiles: TSize;
    function GetIsReady: Boolean;
    function GetSourceTileSize: TSize;
    function GetTiles(X, Y: Integer): TdxMapControlMultiScaleTile;
    procedure SetTileSource(Value: TdxMapControlMultiScaleTileSource);
  protected
    procedure ConfirmTiles(ASize: TSize);
    procedure InvalidateAllTiles;
    procedure ProjectRange(ARange: TdxMapControlTileRange);
    procedure Recalculate;
    procedure SetBounds(const ARect: TdxRectDouble);
  public
    constructor Create(ATileFactory: TdxMapControlMultiScaleImage; AInflateCount: Integer);
    destructor Destroy; override;
    procedure InvalidateRect(const ARect: TRect);
    procedure Update(const ANewSizeInPixels: TSize; const ANewBounds: TdxRectDouble);

    property AspectRatio: Double read GetAspectRatio;
    property InflatedTiles: TSize read GetInflatedTiles;
    property IsReady: Boolean read GetIsReady;
    property MultiScaleTileImage: TdxMapControlMultiScaleImage read FTileFactory;
    property Offset: TPoint read FOffset;
    property Scale: TdxPointDouble read FScale;
    property SourceTileSize: TSize read GetSourceTileSize;
    property TileRange: TdxMapControlTileRange read FTileRange;
    property Tiles[X, Y: Integer]: TdxMapControlMultiScaleTile read GetTiles;
    property TileSource: TdxMapControlMultiScaleTileSource read FTileSource write SetTileSource;
  end;

  TdxMapControlBitmapTile = class
  private
    FImage: TdxSmartImage;
    FClipImageRect: TRect;
    FBounds: TRect;
    FOpacity: Single;
    FOwningTile: TdxMapControlMultiScaleTile;
    FPosition: TPoint;
    FSize: TSize;
    FSource: TdxMapControlTileImageSource;
    procedure SetSource(const Value: TdxMapControlTileImageSource);
    procedure SourceImageLoaded(ASender: TObject);
    procedure UpdateImage(AImage: TdxSmartImage);
  public
    constructor Create(ATile: TdxMapControlMultiScaleTile);
    destructor Destroy; override;
    procedure CalculateBounds;
    function IsValid: Boolean;

    property ClipImageRect: TRect read FClipImageRect write FClipImageRect;
    property Bounds: TRect read FBounds;
    property Image: TdxSmartImage read FImage;
    property Position: TPoint read FPosition write FPosition;
    property Opacity: Single read FOpacity write FOpacity;
    property OwningTile: TdxMapControlMultiScaleTile read FOwningTile;
    property Source: TdxMapControlTileImageSource read FSource write SetSource;
    property Size: TSize read FSize write FSize;
  end;

  TdxMapControlTileMaskAction = procedure (X, Y: Integer; var ADone: Boolean) of object;

  TdxMapControlMultiScaleTile = class
  private
    FFace: TdxMapControlBitmapTile;
    FLocation: TPoint;
    FMask: array [0..1, 0..1] of TdxMapControlBitmapTile;
    FMaskSize: TSize;
    FRenderStretchFactor: TdxPointDouble;
    FIndex: TdxMapControlTileIndex;
    FX, FY: Integer;
    FViewPort: TdxMapControlMultiScaleViewport;

    procedure AddMaskToRenderItems(X, Y: Integer; var ADone: Boolean);
    procedure CalculateMaskBitmapBounds(X, Y: Integer; var ADone: Boolean);
    procedure CreateMaskBitmap(X, Y: Integer; var ADone: Boolean);
    procedure DestroyMaskBitmap(X, Y: Integer; var ADone: Boolean);
    procedure InvalidateMask(X, Y: Integer; var ADone: Boolean);
    procedure UpdateMaskBitmap(X, Y: Integer; var ADone: Boolean);

    function AddTileBitmapToRenderItems(ATileBitmap: TdxMapControlBitmapTile): Boolean;
    procedure CalculateTileBitmapBounds;
    procedure CreateTileBitmaps;
    procedure EnumerateMaskBitmaps(AAction: TdxMapControlTileMaskAction);
    function GetDownwardTileIndex(X, Y: Integer): TdxMapControlTileIndex;
    function GetIsReady: Boolean;
    function GetOverlayPieceClip(const AOffset: TdxPointDouble; AScale: Double): TRect;
    function GetSourceTileSize: TSize;
    function GetUpwardTileIndex(X, Y, ALevel: Integer; out AOffset: TdxPointDouble; out AScale: Double): TdxMapControlTileIndex;
    function LookupCache(ATile: TdxMapControlBitmapTile; AIndex: TdxMapControlTileIndex): Boolean;
    procedure SetIndex(const Value: TdxMapControlTileIndex);
    procedure UpdateBitmaps;
    procedure UpdateRenderParams(AScaleFactor: TdxPointDouble);
    function GetTileImageSize: TSize;
  protected
    function CreateTileBitmap: TdxMapControlBitmapTile; virtual;
    procedure Invalidate;
    procedure InvalidateRect(const ARect: TRect);
    function IsTileLoaded(ATile: TdxMapControlBitmapTile): Boolean;
    procedure TileSourceLoaded(ABitmapTile: TdxMapControlBitmapTile);

    property TileImageSize: TSize read GetTileImageSize;
  public
    constructor Create(AViewport: TdxMapControlMultiScaleViewport; X, Y: Integer); virtual;
    destructor Destroy; override;
    procedure CreateRenderItems;

    property Index: TdxMapControlTileIndex read FIndex write SetIndex;
    property IsReady: Boolean read GetIsReady;
    property Location: TPoint read FLocation;
    property RenderStretchFactor: TdxPointDouble read FRenderStretchFactor;
    property X: Integer read FX;
    property Y: Integer read FY;
    property SourceTileSize: TSize read GetSourceTileSize;
    property Viewport: TdxMapControlMultiScaleViewport read FViewPort;
  end;

implementation

uses
  dxMapControl, dxMapControlImageTileProvider, dxMapImageTileLayer;

const
  OverlayFadeTime: Double = 1.0;
  BytesPerPixel = 4;
  PreseveLevel = 1;

type
  TdxMapControlImageTileProviderAccess = class(TdxMapControlImageTileProvider);
  TdxMapLayerAccess = class(TdxMapLayer);

{ TdxMapControlMultiScaleTile }

constructor TdxMapControlMultiScaleTile.Create(AViewport: TdxMapControlMultiScaleViewport; X, Y: Integer);
begin
  inherited Create;
  FViewport := AViewport;
  FX := X;
  FY := Y;
  FIndex := dxMapControlInvalidTileIndex;
  FLocation := cxPoint(X * SourceTileSize.cx, Y * SourceTileSize.cy);
  FRenderStretchFactor := dxPointDouble(1.0, 1.0);
  CreateTileBitmaps;
end;

destructor TdxMapControlMultiScaleTile.Destroy;
begin
  FreeAndNil(FFace);
  EnumerateMaskBitmaps(DestroyMaskBitmap);
  inherited;
end;

procedure TdxMapControlMultiScaleTile.CreateRenderItems;
begin
  if not AddTileBitmapToRenderItems(FFace) then
    EnumerateMaskBitmaps(AddMaskToRenderItems);
end;

function TdxMapControlMultiScaleTile.CreateTileBitmap: TdxMapControlBitmapTile;
begin
  Result := TdxMapControlBitmapTile.Create(Self);
end;

procedure TdxMapControlMultiScaleTile.Invalidate;
begin
  FFace.Source := nil;
  EnumerateMaskBitmaps(InvalidateMask);
end;

procedure TdxMapControlMultiScaleTile.InvalidateRect(const ARect: TRect);
begin
  Viewport.InvalidateRect(ARect);
end;

function TdxMapControlMultiScaleTile.IsTileLoaded(ATile: TdxMapControlBitmapTile): Boolean;
begin
  Result := (ATile <> nil) and ATile.IsValid;
end;

procedure TdxMapControlMultiScaleTile.TileSourceLoaded(ABitmapTile: TdxMapControlBitmapTile);
begin
  InvalidateRect(ABitmapTile.Bounds);
end;

procedure TdxMapControlMultiScaleTile.AddMaskToRenderItems(X, Y: Integer;
  var ADone: Boolean);
begin
  AddTileBitmapToRenderItems(FMask[X, Y]);
end;

procedure TdxMapControlMultiScaleTile.CalculateMaskBitmapBounds(X, Y: Integer; var ADone: Boolean);
begin
  FMask[X, Y].Position := cxPointOffset(FFace.Position,  X * FMaskSize.cx, Y * FMaskSize.cy);
  FMask[X, Y].Size := FMaskSize;
  FMask[X, Y].CalculateBounds;
end;

procedure TdxMapControlMultiScaleTile.CreateMaskBitmap(X, Y: Integer; var ADone: Boolean);
begin
  FMask[X, Y] := CreateTileBitmap;
 // FMask[X, Y].Size := SourceTileSize;
end;

procedure TdxMapControlMultiScaleTile.DestroyMaskBitmap(X, Y: Integer; var ADone: Boolean);
begin
  FreeAndNil(FMask[X, Y]);
end;

procedure TdxMapControlMultiScaleTile.InvalidateMask(X, Y: Integer;
  var ADone: Boolean);
begin
  FMask[X, Y].Source := nil;
end;

procedure TdxMapControlMultiScaleTile.UpdateMaskBitmap(X, Y: Integer; var ADone: Boolean);
var
  ATile: TdxMapControlBitmapTile;
  ADownwardTileIndex, AUpwardTileIndex: TdxMapControlTileIndex;
  ALevel: Integer;
  AScale: Double;
  AOffset: TdxPointDouble;
begin
  ATile := FMask[X, Y];
  if ATile.Source = nil then
  begin
    ADownwardTileIndex := GetDownwardTileIndex(X, Y);
    if LookupCache(ATile, ADownwardTileIndex) then
      ATile.ClipImageRect := cxRect(TileImageSize)
    else
      if FIndex.Level > 0 then
        for ALevel := FIndex.Level - 1 downto 0 do
        begin
          AUpwardTileIndex := GetUpwardTileIndex(X, Y, ALevel, AOffset, AScale);
          if LookupCache(ATile, AUpwardTileIndex) then
          begin
            ATile.ClipImageRect := GetOverlayPieceClip(AOffset, AScale);
            Break;
          end;
        end
      else
        ATile.Source := nil;
  end;
end;

function TdxMapControlMultiScaleTile.AddTileBitmapToRenderItems(ATileBitmap: TdxMapControlBitmapTile): Boolean;
begin
  Result := IsTileLoaded(ATileBitmap);
  if Result then
    Viewport.MultiScaleTileImage.RenderItems.Add(ATileBitmap);
end;

procedure TdxMapControlMultiScaleTile.CalculateTileBitmapBounds;
begin
  FFace.Position := Location;
  FFace.Size := SourceTileSize;
  FFace.CalculateBounds;
  FMaskSize := cxSize(FFace.Size.cx div 2, FFace.Size.cy div 2);
  EnumerateMaskBitmaps(CalculateMaskBitmapBounds);
end;

procedure TdxMapControlMultiScaleTile.CreateTileBitmaps;
begin
  FFace := CreateTileBitmap;
  EnumerateMaskBitmaps(CreateMaskBitmap);
end;

procedure TdxMapControlMultiScaleTile.EnumerateMaskBitmaps(AAction: TdxMapControlTileMaskAction);
var
  I, J: Integer;
  ADone: Boolean;
begin
  ADone := False;
  for I := 0 to 1 do
    for J := 0 to 1 do
    begin
      AAction(I, J, ADone);
      if ADone then
        Break;
    end;
end;

function TdxMapControlMultiScaleTile.GetDownwardTileIndex(X, Y: Integer): TdxMapControlTileIndex;
begin
  Result := dxMapControlTileIndex(FIndex.X * 2 + X, FIndex.Y * 2 + Y, FIndex.Level + 1);
end;

function TdxMapControlMultiScaleTile.GetIsReady: Boolean;
begin
  Result := dxMapControlTileIndexIsEqual(FIndex, dxMapControlInvalidTileIndex) or FFace.IsValid;
end;

function TdxMapControlMultiScaleTile.GetOverlayPieceClip(const AOffset: TdxPointDouble; AScale: Double): TRect;
begin
  Result := cxRectBounds(Ceil(AOffset.X * TileImageSize.cx),
    Ceil(AOffset.Y * TileImageSize.cy),
    Ceil(TileImageSize.cx / AScale),
    Ceil(TileImageSize.cy / AScale));
end;

function TdxMapControlMultiScaleTile.GetSourceTileSize: TSize;
begin
  Result := FViewPort.SourceTileSize;
end;

function TdxMapControlMultiScaleTile.GetTileImageSize: TSize;
begin
  Result := SourceTileSize;
end;

function TdxMapControlMultiScaleTile.GetUpwardTileIndex(X, Y, ALevel: Integer;
  out AOffset: TdxPointDouble; out AScale: Double): TdxMapControlTileIndex;
var
  ADistance, APieceX, APieceY, AUpX, AUpY: Integer;
begin
  ADistance := Round(Power(2, FIndex.Level + 1 - ALevel));
  APieceX := (FIndex.X * 2) + X;
  APieceY := (FIndex.Y * 2) + Y;
  AUpX := Floor(APieceX / ADistance);
  AUpY := Floor(APieceY / ADistance);
  AOffset := dxPointDouble((APieceX / ADistance) - AUpX, (APieceY / ADistance) - AUpY);
  AScale := ADistance;
  Result := dxMapControlTileIndex(AUpX, AUpY, ALevel);
end;

function TdxMapControlMultiScaleTile.LookupCache(ATile: TdxMapControlBitmapTile;
  AIndex: TdxMapControlTileIndex): Boolean;
var
  ASource: TdxMapControlTileImageSource;
begin
  ASource := Viewport.TileSource.PeekTileImageSource(AIndex);
  Result := ASource <> nil;
  if Result then
    ATile.Source := ASource;
end;

procedure TdxMapControlMultiScaleTile.SetIndex(
  const Value: TdxMapControlTileIndex);
begin
  if not dxMapControlTileIndexIsEqual(FIndex, Value) then
  begin
    FIndex := Value;
    Invalidate;
  end;
end;

procedure TdxMapControlMultiScaleTile.UpdateBitmaps;
begin
  if dxMapControlTileIndexIsEqual(FIndex, dxMapControlInvalidTileIndex) then
    Invalidate
  else
  begin
    FFace.ClipImageRect := cxRect(TileImageSize);
    if FFace.Source = nil then
      Viewport.TileSource.GetTileImageSource(FIndex, FFace);
    if (FFace.Source = nil) or (FFace.Source.Status <> mptsReady) or (FFace.Source.Source = nil) then
      EnumerateMaskBitmaps(UpdateMaskBitmap);
  end;
end;

procedure TdxMapControlMultiScaleTile.UpdateRenderParams(AScaleFactor: TdxPointDouble);
begin
  CalculateTileBitmapBounds;
  FRenderStretchFactor := AScaleFactor;
  UpdateBitmaps;
end;

{ TdxMapControlMultiScaleViewport }

constructor TdxMapControlMultiScaleViewport.Create(
  ATileFactory: TdxMapControlMultiScaleImage; AInflateCount: Integer);
begin
  inherited Create;
  FTileFactory := ATileFactory;
  FInflateCount := AInflateCount;
  FBounds := dxRectDouble(0, 0, 1, 1);
end;

destructor TdxMapControlMultiScaleViewport.Destroy;
var
  X, Y: Integer;
begin
  for X := 0 to High(FTiles) do
    for Y := 0 to High(FTiles[X]) do
      FTiles[X, Y].Free;
  inherited;
end;

procedure TdxMapControlMultiScaleViewport.ConfirmTiles(ASize: TSize);
var
  AInflatedSize: TSize;
  ATiles: TdxMapControlTilesDimension;
  X, Y: Integer;
begin
  if (Length(FTiles) = 0) or not cxSizeIsEqual(FArrangedTiles, ASize) then
  begin
    AInflatedSize := cxSize(ASize.cx + FInflateCount * 2, ASize.cy + FInflateCount * 2);
    SetLength(ATiles, AInflatedSize.cx, AInflatedSize.cy);
    if (FArrangedTiles.cx <> 0) and (FArrangedTiles.cy <> 0) then
      for Y := 0 to InflatedTiles.cy - 1 do
        for X := 0 to InflatedTiles.cx - 1 do
          if (X >= AInflatedSize.cx) or (Y >= AInflatedSize.cy) then
            FreeAndNil(FTiles[X, Y])
          else
            ATiles[X, Y] := FTiles[X, Y];
    for Y := 0 to AInflatedSize.cy - 1 do
      for X := 0 to AInflatedSize.cx - 1 do
        if ATiles[X, Y] = nil then
           ATiles[X, Y] := MultiScaleTileImage.CreateTile(Self, X - FInflateCount, Y - FInflateCount);
    FTiles := ATiles;
    FArrangedTiles := ASize;
  end;
end;

procedure TdxMapControlMultiScaleViewport.InvalidateAllTiles;
var
  X, Y: Integer;
begin
  if Length(FTiles) > 0 then
    for Y := 0 to InflatedTiles.cy - 1 do
      for X := 0 to InflatedTiles.cx - 1 do
        FTiles[X, Y].Invalidate;
end;

procedure TdxMapControlMultiScaleViewport.ProjectRange(ARange: TdxMapControlTileRange);
var
  AInflatedSize: TSize;
  X, Y: Integer;
  AProjX, AProjY: Integer;
begin
  if (Length(FTiles) > 0) and not dxMapControlTileRangeIsEqual(ARange, FTileRange) then
  begin
    AInflatedSize := InflatedTiles;
    for Y := 0 to AInflatedSize.cy - 1 do
      for X := 0 to AInflatedSize.cx - 1 do
      begin
        AProjX := ARange.Min.X + X - FInflateCount;
        AProjY :=  ARange.Min.Y + Y - FInflateCount;
        if (AProjX >= FZoomLevelTileCount) or (AProjY >= FZoomLevelTileCount) or (AProjX < 0) or (AProjY < 0) then
          FTiles[X, Y].Index := dxMapControlInvalidTileIndex
        else
          FTiles[X, Y].Index := dxMapControlTileIndex(AProjX, AProjY, ARange.Min.Level);
      end;
    FTileRange := ARange;
  end;
end;

procedure TdxMapControlMultiScaleViewport.Recalculate;
var
  AMax, AMin: TdxMapControlTileIndex;
  AContentSize: TdxSizeDouble;
  AContentOrigin: TdxPointDouble;
  AScaleFactor: TdxScaleFactor;
begin

  if (FViewSize.cx = 0) or (FBounds.Width = 0) or (FTileSource = nil) or cxSizeIsEmpty(SourceTileSize) then
    Exit;

  AScaleFactor := TdxMapLayerAccess(FTileFactory.Layer).ScaleFactor;
  ConfirmTiles(cxSize(Ceil(FViewSize.cx / AScaleFactor.Apply(SourceTileSize.cx)),
    Ceil(FViewSize.cy / AScaleFactor.Apply(SourceTileSize.cy))));

  AContentSize := dxSizeDouble(FBounds.Width, FBounds.Width * AspectRatio);
  AContentOrigin := FBounds.TopLeft;
  FZoomLevel := Min(Max(Floor(Log2(FArrangedTiles.cx / AContentSize.Width)), 1), FTileSource.MaxZoomLevel);
  FZoomLevelTileCount := Round(Power(2.0, FZoomLevel));
  AMin := dxMapControlTileIndex(Max(Floor(AContentOrigin.X * FZoomLevelTileCount), 0),
    Max(Floor(AContentOrigin.Y * FZoomLevelTileCount), 0), FZoomLevel);

  AMax := dxMapControlTileIndex(Min(Ceil((AContentOrigin.X + AContentSize.Width) * FZoomLevelTileCount),
    FZoomLevelTileCount), Min(Ceil((AContentOrigin.Y + AContentSize.Height) * FZoomLevelTileCount),
    Round(FZoomLevelTileCount * FTileSource.TileCountRatio)), FZoomLevel);

  ProjectRange(dxMapControlTileRange(AMin, AMax));
  FScale := dxPointDouble(FViewSize.cx / (AContentSize.Width * SourceTileSize.cx) / FZoomLevelTileCount,
    FViewSize.cy / (AContentSize.Height * SourceTileSize.cy) / FZoomLevelTileCount);
  FOffset := cxPoint(FTileRange.Min.X * SourceTileSize.cx -
    Round(AContentOrigin.X * SourceTileSize.cx * FZoomLevelTileCount), FTileRange.Min.Y * SourceTileSize.cy -
    Round(AContentOrigin.Y * SourceTileSize.cy * FZoomLevelTileCount));
end;

procedure TdxMapControlMultiScaleViewport.SetBounds(const ARect: TdxRectDouble);
begin
  if not dxRectDoubleIsEqual(FBounds, ARect) then
  begin
    FBounds := ARect;
    Recalculate;
  end;
end;

procedure TdxMapControlMultiScaleViewport.InvalidateRect(const ARect: TRect);
begin
  FTileFactory.InvalidateRect(ARect);
end;

procedure TdxMapControlMultiScaleViewport.Update(const ANewSizeInPixels: TSize; const ANewBounds: TdxRectDouble);
begin
  if not dxRectDoubleIsEqual(FBounds, ANewBounds) or
    not cxSizeIsEqual(FViewSize, ANewSizeInPixels) then
  begin
    FViewSize := ANewSizeInPixels;
    FBounds := ANewBounds;
    Recalculate;
  end;
end;

function TdxMapControlMultiScaleViewport.GetAspectRatio: Double;
begin
  if FViewSize.cx <> 0 then
    Result := FViewSize.cy / FViewSize.cx
  else
    Result := 0;
end;

function TdxMapControlMultiScaleViewport.GetInflatedTiles: TSize;
begin
  if cxSizeIsEmpty(FArrangedTiles) then
    Result := cxNullSize
  else
    Result := cxSize(FArrangedTiles.cx + (FInflateCount shl 1), FArrangedTiles.cy + (FInflateCount shl 1));
end;

function TdxMapControlMultiScaleViewport.GetIsReady: Boolean;
var
  X, Y: Integer;
begin
  Result := True;
  for X := 0 to InflatedTiles.cx - 1 do
    for Y := 0 to InflatedTiles.cy - 1 do
    begin
      Result := Result and FTiles[X, Y].IsReady;
      if not Result then
        Break;
    end;
end;

function TdxMapControlMultiScaleViewport.GetSourceTileSize: TSize;
begin
  Result := FTileSource.TileSize;
end;

function TdxMapControlMultiScaleViewport.GetTiles(X,
  Y: Integer): TdxMapControlMultiScaleTile;
begin
  Result := FTiles[X, Y];
end;

procedure TdxMapControlMultiScaleViewport.SetTileSource(Value: TdxMapControlMultiScaleTileSource);
begin
  if FTileSource <> Value then
  begin
    FTileSource := Value;
    Recalculate;
    InvalidateAllTiles;
  end;
end;

{ TdxMapControlMultiScaleImage }

constructor TdxMapControlMultiScaleImage.Create(ALayer: TdxMapLayer);
begin
  inherited Create;
  FLayer := ALayer;
  FRenderItems := TdxFastObjectList.Create(False);
  FViewport := TdxMapControlMultiScaleViewport.Create(Self, 1);
end;

function TdxMapControlMultiScaleImage.CreateTile(AViewport: TdxMapControlMultiScaleViewport;
  X, Y: Integer): TdxMapControlMultiScaleTile;
begin
  Result := TdxMapControlMultiScaleTile.Create(AViewport, X, Y);
end;

function TdxMapControlMultiScaleImage.GetLayerAlphaBlendValue: Byte;
begin
  Result := (Layer as TdxMapImageTileLayer).AlphaBlendValue;
end;

destructor TdxMapControlMultiScaleImage.Destroy;
begin
  FreeAndNil(FViewport);
  FreeAndNil(FRenderItems);
  inherited;
end;

procedure TdxMapControlMultiScaleImage.UpdateTileParams;
var
  X, Y, I: Integer;
  ATile: TdxMapControlMultiScaleTile;
  AVisibleTiles: TList;
begin
  AVisibleTiles := TList.Create;
  try
    for Y := 0 to Viewport.InflatedTiles.cy - 1 do
      for X := 0 to Viewport.InflatedTiles.cx - 1 do
      begin
        ATile := Viewport.Tiles[X, Y];
        if dxMapControlTileIndexIsEqual(ATile.Index, dxMapControlInvalidTileIndex) then
          Continue;
        if (X < 1) or (Y < 1) or (X > Viewport.InflatedTiles.cx - 2) or (Y > Viewport.InflatedTiles.cy - 2) then
          ATile.UpdateRenderParams(Viewport.Scale)
        else
          AVisibleTiles.Add(ATile);
      end;
    for I := 0 to AVisibleTiles.Count - 1 do
      TdxMapControlMultiScaleTile(AVisibleTiles[I]).UpdateRenderParams(Viewport.Scale);
  finally
    AVisibleTiles.Free;
  end;
end;

procedure TdxMapControlMultiScaleImage.UpdateViewPort(
  const AViewPortSizeInPixels: TSize; const AViewPortBounds: TdxRectDouble);
begin
  Viewport.Update(AViewPortSizeInPixels, AViewPortBounds);
end;

procedure TdxMapControlMultiScaleImage.Paint(ACanvas: TcxCanvas);

  procedure DrawTileBitmaps(AGpCanvas: TdxGPCanvas);
  var
    I: Integer;
    ABitmapTile: TdxMapControlBitmapTile;
    ADestRect, ASourceRect: TRect;
    AAlphaValue: Byte;
  begin
    AAlphaValue := GetLayerAlphaBlendValue;
    for I := 0 to RenderItems.Count - 1 do
    begin
      ABitmapTile := RenderItems[I] as TdxMapControlBitmapTile;
      ASourceRect := ABitmapTile.ClipImageRect;
      ADestRect := ABitmapTile.Bounds;
      if dxGpIsRectVisible(AGpCanvas.Handle, ADestRect) then
        AGpCanvas.Draw(ABitmapTile.Image, ADestRect, ASourceRect, AAlphaValue);
    end;
  end;

  procedure CreateRenderItems;
  var
    X, Y: Integer;
    ATile: TdxMapControlMultiScaleTile;
  begin
    RenderItems.Clear;
    for Y := 0 to Viewport.InflatedTiles.cy - 1 do
      for X := 0 to Viewport.InflatedTiles.cx - 1 do
      begin
        ATile := Viewport.Tiles[X, Y];
        if dxMapControlTileIndexIsEqual(ATile.Index, dxMapControlInvalidTileIndex) then
          Continue;
        ATile.CreateRenderItems;
      end;
  end;

var
  AGPCanvas: TdxGPCanvas;
begin
  CreateRenderItems;
  AGPCanvas := TdxGPCanvas.Create(ACanvas.Handle);
  try
    AGPCanvas.SmoothingMode := smAntiAlias;
    DrawTileBitmaps(AGPCanvas);
  finally
    AGPCanvas.Free;
  end;
end;

procedure TdxMapControlMultiScaleImage.InvalidateRect(const ARect: TRect);
begin
  Layer.InvalidateRect(ARect);
end;

function TdxMapControlMultiScaleImage.GetIsReady: Boolean;
begin
  Result := FViewport.IsReady;
end;

function TdxMapControlMultiScaleImage.GetSource: TdxMapControlMultiScaleTileSource;
begin
  Result := FViewport.TileSource;
end;

procedure TdxMapControlMultiScaleImage.SetSource(
  const Value: TdxMapControlMultiScaleTileSource);
begin
  FViewport.TileSource := Value;
end;

{ TdxMapControlBitmapTile }

procedure TdxMapControlBitmapTile.CalculateBounds;
var
  APosition: TPoint;
  AScale: TdxPointDouble;
begin
  APosition := cxPointOffset(Position, FOwningTile.Viewport.Offset);
  AScale := FOwningTile.Viewport.Scale;
  FBounds := cxRectBounds(Ceil(APosition.X * AScale.X),
    Ceil(APosition.Y * AScale.Y),
    Ceil(Size.cx * AScale.X),
    Ceil(Size.cy * AScale.Y));
end;

constructor TdxMapControlBitmapTile.Create(ATile: TdxMapControlMultiScaleTile);
begin
  FClipImageRect := cxNullRect;
  FOwningTile := ATile;
  FOpacity := 1.0;
end;

destructor TdxMapControlBitmapTile.Destroy;
begin
  Source := nil;
  inherited;
end;

function TdxMapControlBitmapTile.IsValid: Boolean;
begin
  Result := (FImage <> nil);// and not FImage.Empty;
end;

procedure TdxMapControlBitmapTile.SetSource(
  const Value: TdxMapControlTileImageSource);
var
  APreviousSource: TdxMapControlTileImageSource;
  AImage: TdxSmartImage;
begin
  if FSource <> Value then
  begin
    APreviousSource := FSource;
    FSource := Value;
    AImage := nil;
    if APreviousSource <> nil then
    begin
      APreviousSource.OnImageLoaded := nil;
      APreviousSource.RemoveLink(Self);
    end;
    if FSource <> nil then
    begin
      FSource.OnImageLoaded := SourceImageLoaded;
      FSource.AddLink(Self);
      if FSource.Status = mptsReady then
        AImage := FSource.Source;
    end;
    UpdateImage(AImage);
  end;
end;

procedure TdxMapControlBitmapTile.SourceImageLoaded(ASender: TObject);
begin
  UpdateImage((ASender as TdxMapControlTileImageSource).Source);
  FOwningTile.TileSourceLoaded(Self);
end;

procedure TdxMapControlBitmapTile.UpdateImage(AImage: TdxSmartImage);
begin
  FImage := AImage;
end;

{ TdxMapControlMultiScaleTileSource }

constructor TdxMapControlMultiScaleTileSource.Create(AOwner: TPersistent; AImageWidth: Int64; AImageHeight: Int64;
  ATileWidth: Integer; ATileHeight: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FImageWidth := AImageWidth;
  FImageHeight := AImageHeight;
  FTileSize := cxSize(ATileWidth, ATileHeight);

  FTileSourceStorage := TdxMapControlTileImageSourceStorage.Create;
  FTileCountRatio := FImageHeight / (FImageWidth * (FTileSize.cx / FTileSize.cy));
  FMaxZoomLevel := Round(Log2(FImageWidth / FTileSize.cx));
  FLoadingQueueManager := TdxMapControlTileImageSourceLoadingQueueManager.Create(Self, GetMaxParallelConnections);
  UpdateDiskCache;
end;

destructor TdxMapControlMultiScaleTileSource.Destroy;
begin
  FreeAndNil(FLoadingQueueManager);
  FreeAndNil(FTileSourceStorage);
  FreeAndNil(FDiskCache);
  inherited;
end;

procedure TdxMapControlMultiScaleTileSource.ClearCache;
begin
  LoadingQueueManager.Clear;
  FTileSourceStorage.Clear;
end;

procedure TdxMapControlMultiScaleTileSource.PushToDisk(ATileSource: TdxMapControlTileImageSource);
begin
  if FDiskCache <> nil then
    FDiskCache.Push(ATileSource.Source, ATileSource.Index, TilePrefix);
end;

procedure TdxMapControlMultiScaleTileSource.StartLoadTiles;
begin
  FLoadingQueueManager.StartLoadTiles;
end;

procedure TdxMapControlMultiScaleTileSource.UpdateCacheOptions;
begin
  UpdateDiskCache;
  CheckMemoryLimit;
end;

procedure TdxMapControlMultiScaleTileSource.UpdateConnectionSettings;
begin
  LoadingQueueManager.MaxParallelDownloads := GetMaxParallelConnections;
end;

function TdxMapControlMultiScaleTileSource.GetCacheOptions: TdxMapControlCacheOptions;
begin
  Result := (FOwner as TdxMapControlImageTileProvider).CacheOptions;
end;

function TdxMapControlMultiScaleTileSource.GetMaxParallelConnections: Integer;
begin
  Result := (FOwner as TdxMapControlImageTileProvider).MaxParallelConnectionCount;
end;

function TdxMapControlMultiScaleTileSource.GetScaleFactor: TdxScaleFactor;
begin
  Result := TdxMapControlImageTileProviderAccess(FOwner).ScaleFactor;
end;

procedure TdxMapControlMultiScaleTileSource.UpdateDiskCache;
var
  ACacheOptions: TdxMapControlCacheOptions;
begin
  ACacheOptions := CacheOptions;
  if FDiskCache = nil then
  begin
    if ACacheOptions.DiskFolder <> '' then
      FDiskCache := TdxMapControlDiskCache.Create(ACacheOptions.DiskFolder, ACacheOptions.DiskExpireTime,
        ACacheOptions.DiskLimit)
  end
  else
    if ACacheOptions.DiskFolder = '' then
      FreeAndNil(FDiskCache)
    else
      if not FDiskCache.LocatedAt(ACacheOptions.DiskFolder) then
      begin
        FreeAndNil(FDiskCache);
        FDiskCache := TdxMapControlDiskCache.Create(ACacheOptions.DiskFolder, ACacheOptions.DiskExpireTime,
          ACacheOptions.DiskLimit)
      end
      else
      begin
        FDiskCache.ExpireTime := CacheOptions.DiskExpireTime;
        FDiskCache.DiskLimit := CacheOptions.DiskLimit;
      end;
end;

function TdxMapControlMultiScaleTileSource.GetSubdomainIndex(ASubdomainCount: Integer): Integer;
begin
  FCurrentSubdomainIndex := IfThen(FCurrentSubdomainIndex + 2 > ASubdomainCount, 0, FCurrentSubdomainIndex + 1);
  Result := FCurrentSubdomainIndex;
end;

procedure TdxMapControlMultiScaleTileSource.GetTileImageSource(
  AIndex: TdxMapControlTileIndex; ABitmapTile: TdxMapControlBitmapTile);

  procedure AddLoadingTask(ATileImageSource: TdxMapControlTileImageSource);
  var
    ATileLoadingTask: TdxMapControlTileImageLoadingTask;
    AUri: string;
  begin
    ATileLoadingTask := nil;
    if (FDiskCache <> nil) and
      FDiskCache.TryRetrieveFileName(AIndex, TilePrefix, AUri) then
      ATileLoadingTask := LoadingQueueManager.CreateTileImageLoadingTask(ATileImageSource, AUri);
    if ATileLoadingTask = nil then
    begin
      AUri := GetTileLayers(AIndex.Level, AIndex.X, AIndex.Y);
      if AUri <> '' then
        ATileLoadingTask := LoadingQueueManager.CreateTileImageLoadingTask(ATileImageSource, AUri);
    end;
    if ATileLoadingTask <> nil then
      LoadingQueueManager.Push(ATileLoadingTask);
  end;

var
  ATileImageSource: TdxMapControlTileImageSource;
begin
  if not dxMapControlTileIndexIsEqual(AIndex, dxMapControlInvalidTileIndex) then
  begin
    if FTileSourceStorage.TryGetValue(AIndex, ATileImageSource) then
    begin
      ABitmapTile.Source := ATileImageSource;
      if (ATileImageSource.Status = mptsNotReady) and
        not LoadingQueueManager.TaskExists(ATileImageSource) then
        AddLoadingTask(ATileImageSource);
    end
    else
    begin
      ATileImageSource := FTileSourceStorage.CreateTileImageSource(AIndex, TileSize);
      ABitmapTile.Source := ATileImageSource;
      CheckMemoryLimit;
      if ABitmapTile.Source <> nil then
        AddLoadingTask(ABitmapTile.Source);
    end;
  end;
end;

function TdxMapControlMultiScaleTileSource.GetTileLayers(ATileLevel, ATilePositionX, ATilePositionY: Integer): string;
begin
  if (ATileLevel > 0) and (ATileLevel <= FMaxZoomLevel) then
    Result := GetTileByZoomLevel(ATileLevel, ATilePositionX, ATilePositionY)
  else
    Result := '';
end;

function TdxMapControlMultiScaleTileSource.PeekTileImageSource(AIndex: TdxMapControlTileIndex): TdxMapControlTileImageSource;
var
  ASource: TdxMapControlTileImageSource;
begin
  FTileSourceStorage.TryGetValue(AIndex, ASource);
  if (ASource <> nil) and (ASource.Status = mptsReady) and (ASource.Source <> nil) then
    Result := ASource
  else
    Result := nil;
end;

function TdxMapControlMultiScaleTileSource.TilePrefix: string;
begin
  Result := '';
end;

procedure TdxMapControlMultiScaleTileSource.CheckMemoryLimit;
var
  I: Integer;
  AExtraTileImageSources: TList<TdxMapControlTileImageSource>;
begin
  AExtraTileImageSources := TList<TdxMapControlTileImageSource>.Create;
  try
    FTileSourceStorage.CheckMemoryLimit(CacheOptions.MemoryLimit shl 20, AExtraTileImageSources);
    for I := 0 to AExtraTileImageSources.Count - 1 do
      if not LoadingQueueManager.DelayedDestroying(AExtraTileImageSources[I]) then
        AExtraTileImageSources[I].Free;
  finally
    AExtraTileImageSources.Free;
  end;
end;

{ TdxMapControlTileImageSource }

constructor TdxMapControlTileImageSource.Create(
  ATileIndex: TdxMapControlTileIndex; AExpectedTileSize: TSize);
begin
  inherited Create;
  FSize := AExpectedTileSize;
  FBitmapTiles := TdxFastObjectList.Create(False);
  FStatus := mptsNotReady;
  FSource := nil;
  FIndex := ATileIndex;
  InitializeCriticalSection(FStatusLocker);
end;

destructor TdxMapControlTileImageSource.Destroy;
begin
  RemoveLinks;
  FreeAndNil(FSource);
  DeleteCriticalSection(FStatusLocker);
  dxTestCheck(FBitmapTiles.Count = 0, 'TdxMapControlTileImageSource.Destroy: FBitmapTiles.Count <> 0');
  FreeAndNil(FBitmapTiles);
  inherited;
end;

function TdxMapControlTileImageSource.GetMemoryUsage: Integer;
begin
  Result := FSize.cx * FSize.cy * BytesPerPixel;
end;

function TdxMapControlTileImageSource.IsLinked: Boolean;
begin
  Result := FBitmapTiles.Count > 0;
end;

procedure TdxMapControlTileImageSource.RaiseImageLoaded;
begin
  SetStatusSafe(mptsReady);
  if Assigned(FOnImageLoaded) then
    FOnImageLoaded(Self);
end;

procedure TdxMapControlTileImageSource.RemoveLink(
  ABitmapTile: TdxMapControlBitmapTile);
begin
  FBitmapTiles.Remove(ABitmapTile);
end;

procedure TdxMapControlTileImageSource.RemoveLinks;
var
  I: Integer;
begin
  for I := FBitmapTiles.Count - 1 downto 0 do
  begin
    dxTestCheck((FBitmapTiles[I] as TdxMapControlBitmapTile).Source = Self,
      'TdxMapControlTileImageSource.Destroy: FBitmapTiles[I].Source <> Self');
    (FBitmapTiles[I] as TdxMapControlBitmapTile).Source := nil;
  end;
end;

procedure TdxMapControlTileImageSource.RaiseImageFailed;
begin
  SetStatusSafe(mptsFailed);
end;

procedure TdxMapControlTileImageSource.AddLink(
  ABitmapTile: TdxMapControlBitmapTile);
begin
  FBitmapTiles.Add(ABitmapTile);
end;

function TdxMapControlTileImageSource.CreateImage(AStream: TStream): Boolean;
begin
  Result := True;
  AStream.Position := 0;
  FSource := TdxSmartImage.Create;
  FSource.LoadFromStream(AStream);
  try
    FSize := Source.Size;
  except
    Result := False;
    FSource.Clear;
  end;
end;

procedure TdxMapControlTileImageSource.RaiseImageCanceled;
begin
  SetStatusSafe(mptsNotReady);
end;

procedure TdxMapControlTileImageSource.SetStatusSafe(AStatus: TdxMapControlTileStatus);
begin
  EnterCriticalSection(FStatusLocker);
  try
    FStatus := AStatus;
  finally
    LeaveCriticalSection(FStatusLocker);
  end;
end;

type
  TdxMapControTileImageLoadingThread = class(TcxThread)
  private
    FTask: TdxMapControlTileImageLoadingTask;
    FOwner: TdxMapControlTileImageSourceLoadingQueueManager;
    procedure DestroyTask;
    procedure ImageLoaded;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TdxMapControlTileImageSourceLoadingQueueManager);
  end;

{ TdxMapControlRemoteLoadImageThread }

constructor TdxMapControTileImageLoadingThread.Create(AOwner: TdxMapControlTileImageSourceLoadingQueueManager);
begin
  inherited Create(True, True);
  FOwner := AOwner;
end;

procedure TdxMapControTileImageLoadingThread.DestroyTask;
begin
  FOwner.DestroyTileImageLoadingTask(FTask);
end;

procedure TdxMapControTileImageLoadingThread.Execute;
var
  AList: TList;
  I: Integer;
begin
  repeat
    CheckForPause;
    try
      AList := FOwner.TileImageSourcesQueue.LockList;
      try
        FTask := nil;
        for I := AList.Count - 1 downto 0 do
        begin
          FTask := TdxMapControlTileImageLoadingTask(AList.Extract(AList[I]));
          if FTask.TileImageSource.IsLinked then
          begin
            FTask.TileImageSource.SetStatusSafe(mptsLoading);
            Break;
          end
          else
            DestroyTask;
        end;
        if FTask = nil then
          Pause;
      finally
        FOwner.TileImageSourcesQueue.UnlockList;
      end;
      if FTask <> nil then
      try
        if FTask.Load then
          Synchronize(ImageLoaded);
      finally
        DestroyTask;
      end;
    except
      HandleException;
    end;
  until Terminated;
end;

procedure TdxMapControTileImageLoadingThread.ImageLoaded;
begin
  if not FTask.TileImageSource.FNeedToBeDestoying then
    FTask.RaiseImageLoaded;
end;

{ TdxMapControlTileImageLoadingTask }

constructor TdxMapControlTileImageLoadingTask.Create(
  ATileImageSource: TdxMapControlTileImageSource; const AUri: string);
begin
  inherited Create;
  FTileImageSource := ATileImageSource;
  FTileImageSource.Task := Self;
  FUri := AUri;
end;

destructor TdxMapControlTileImageLoadingTask.Destroy;
begin
  if FTileImageSource.FNeedToBeDestoying then
    FreeAndNil(FTileImageSource)
  else
    FTileImageSource.Task := nil;
  inherited;
end;

function TdxMapControlTileImageLoadingTask.Load: Boolean;
begin
  Result := DoLoad;
end;

procedure TdxMapControlTileImageLoadingTask.RaiseImageFailed;
begin
  FTileImageSource.RaiseImageFailed;
end;

procedure TdxMapControlTileImageLoadingTask.RaiseImageLoaded;
begin
  FTileImageSource.RaiseImageLoaded;
end;

{ TdxMapControlRemoteImageSource }

function TdxMapControlTileImageRemoteLoadingTask.DoLoad: Boolean;
var
  AContent: TMemoryStream;
begin
  AContent := TMemoryStream.Create;
  try
    with TdxMapControlHttpRequest.Create do
    try
      OnRequest := DoRequest;
      Get(Uri, AContent);
      Result := not HasErrors and not IsCancelled;
      if HasErrors then
        RaiseImageFailed
      else
        if IsCancelled then
          TileImageSource.RaiseImageCanceled
        else
          Result := TileImageSource.CreateImage(AContent);
    finally
      Free;
    end;
  finally
    AContent.Free;
  end;
end;

procedure TdxMapControlTileImageRemoteLoadingTask.RaiseImageLoaded;
begin
  LoadingQueueManager.ImageLoaded(FTileImageSource);
  inherited RaiseImageLoaded;
end;

procedure TdxMapControlTileImageRemoteLoadingTask.DoRequest(ASender: TObject;
  ARequestMode: TdxMapControlHttpRequestMode; ACount: Int64; var AIsCancelled: Boolean);
begin
  AIsCancelled := not TileImageSource.IsLinked or TileImageSource.FNeedToBeDestoying;
end;

{ TdxMapControlDiskImageSource }

function TdxMapControlTileImageDiskLoadingTask.DoLoad: Boolean;
var
  AContent: TStream;
begin
  Result := FileExists(Uri);
  if Result then
  begin
    AContent := TFileStream.Create(Uri, fmOpenRead);
    try
      TileImageSource.CreateImage(AContent);
    finally
      AContent.Free;
    end;
  end
  else
    RaiseImageFailed;
end;

{ TdxMapControlTileImageSourceLoadingQueueManager }

constructor TdxMapControlTileImageSourceLoadingQueueManager.Create(
  ATileSource: TdxMapControlMultiScaleTileSource; AMaxParallelDownloads: Integer);
begin
  inherited Create;
  FTileImageSourcesQueue := TThreadList.Create;
  FMaxParallelDownloads := AMaxParallelDownloads;
  FTileSource := ATileSource;
  InitializeCriticalSection(FTaskDestroyingLock);
  CreateThreads;
end;

destructor TdxMapControlTileImageSourceLoadingQueueManager.Destroy;
var
  I: Integer;
  AList: TList;
begin
  TerminateThreads;
  DeleteCriticalSection(FTaskDestroyingLock);
  AList := FTileImageSourcesQueue.LockList;
  try
    for I := 0 to AList.Count - 1 do
      TdxMapControlTileImageLoadingTask(AList[I]).Free;
  finally
    FTileImageSourcesQueue.UnlockList;
  end;
  FreeAndNil(FTileImageSourcesQueue);
  inherited;
end;

procedure TdxMapControlTileImageSourceLoadingQueueManager.Clear;
var
  I: Integer;
  AList: TList;
  ATask: TdxMapControlTileImageLoadingTask;
begin
  AList := FTileImageSourcesQueue.LockList;
  try
    for I := AList.Count - 1 downto 0 do
    begin
      ATask := TdxMapControlTileImageLoadingTask(AList[I]);
      AList.Delete(I);
      ATask.Free;
    end;
  finally
    FTileImageSourcesQueue.UnlockList;
  end;
  StopThreads;
end;

function TdxMapControlTileImageSourceLoadingQueueManager.CreateTileImageLoadingTask(
  ATileSource: TdxMapControlTileImageSource;
  AUri: string): TdxMapControlTileImageLoadingTask;
begin
  if Pos('http', AUri) = 1 then
    Result := TdxMapControlTileImageRemoteLoadingTask.Create(ATileSource, AUri)
  else
    Result := TdxMapControlTileImageDiskLoadingTask.Create(ATileSource, AUri);
  Result.LoadingQueueManager := Self;
end;

procedure TdxMapControlTileImageSourceLoadingQueueManager.DestroyTileImageLoadingTask(
  var ATask: TdxMapControlTileImageLoadingTask);
begin
  LockTaskDestroying;
  try
    FreeAndNil(ATask);
  finally
    UnlockTaskDestroying;
  end;
end;

procedure TdxMapControlTileImageSourceLoadingQueueManager.ImageLoaded(ASource: TdxMapControlTileImageSource);
begin
  FTileSource.PushToDisk(ASource);
end;

procedure TdxMapControlTileImageSourceLoadingQueueManager.LockTaskDestroying;
begin
  EnterCriticalSection(FTaskDestroyingLock);
end;

function TdxMapControlTileImageSourceLoadingQueueManager.DelayedDestroying(
  ASource: TdxMapControlTileImageSource): Boolean;
begin
  LockTaskDestroying;
  try
    Result := ASource.Task <> nil;
    if Result then
    begin
      ASource.RemoveLinks;
      ASource.FNeedToBeDestoying := True;
    end;
  finally
    UnlockTaskDestroying;
  end;
end;

procedure TdxMapControlTileImageSourceLoadingQueueManager.Push(
  ATask: TdxMapControlTileImageLoadingTask);
begin
  FTileImageSourcesQueue.Add(ATask);
end;

procedure TdxMapControlTileImageSourceLoadingQueueManager.StartLoadTiles;
begin
  WakeThreads;
end;

procedure TdxMapControlTileImageSourceLoadingQueueManager.StopThreads;

  function ThreadsStopped: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to FThreads.Count - 1 do
    begin
      Result := TdxMapControTileImageLoadingThread(FThreads[I]).Paused;
      if not Result then
        Break;
    end;
  end;

begin
  while not ThreadsStopped do
    CheckSynchronize;
end;

function TdxMapControlTileImageSourceLoadingQueueManager.TaskExists(
  ASource: TdxMapControlTileImageSource): Boolean;
begin
  LockTaskDestroying;
  try
    Result := ASource.Task <> nil;
  finally
    UnlockTaskDestroying;
  end;
end;

procedure TdxMapControlTileImageSourceLoadingQueueManager.UnlockTaskDestroying;
begin
  LeaveCriticalSection(FTaskDestroyingLock);
end;

procedure TdxMapControlTileImageSourceLoadingQueueManager.WakeThreads;
var
  I: Integer;
begin
  FTileImageSourcesQueue.LockList;
  try
    for I := 0 to FThreads.Count - 1 do
      TdxMapControTileImageLoadingThread(FThreads[I]).Unpause;
  finally
    FTileImageSourcesQueue.UnlockList;
  end;
end;

procedure TdxMapControlTileImageSourceLoadingQueueManager.CreateThreads;
var
  I: Integer;
  AThread: TdxMapControTileImageLoadingThread;
begin
  FThreads := TdxFastObjectList.Create(False, FMaxParallelDownloads);
  for I := 0 to FMaxParallelDownloads - 1 do
  begin
    AThread := TdxMapControTileImageLoadingThread.Create(Self);
    AThread.Priority := tpIdle;
    AThread.Start;
    FThreads.Add(AThread);
  end;
end;

procedure TdxMapControlTileImageSourceLoadingQueueManager.RecreateThreads;
begin
  TerminateThreads;
  CreateThreads;
  StartLoadTiles;
end;

procedure TdxMapControlTileImageSourceLoadingQueueManager.SetMaxParallelDownloads(
  const Value: Integer);
begin
  if FMaxParallelDownloads <> Value then
  begin
    FMaxParallelDownloads := Value;
    RecreateThreads;
  end;
end;

procedure TdxMapControlTileImageSourceLoadingQueueManager.TerminateThreads;
var
  I: Integer;
  AThread: TdxMapControTileImageLoadingThread;
begin
  for I := 0 to FThreads.Count - 1 do
  begin
    AThread := FThreads[I] as TdxMapControTileImageLoadingThread;
    AThread.Terminate;
  end;
  WakeThreads;
  for I := 0 to FThreads.Count - 1 do
  begin
    AThread := FThreads[I] as TdxMapControTileImageLoadingThread;
    AThread.WaitFor;
    FreeAndNil(AThread);
  end;
  FreeAndNil(FThreads);
end;

{ TdxMapControlTileImageSources }

function TdxMapControlTileImageSources.AddSource(
  const AIndex: TdxMapControlTileIndex; ATileSize: TSize): TdxMapControlTileImageSource;
begin
  Result := TdxMapControlTileImageSource.Create(AIndex, ATileSize);
  DoAdd(Result);
end;

procedure TdxMapControlTileImageSources.MakeLast(
  AImageTileSource: TdxMapControlTileImageSource);
begin
  if FLast <> AImageTileSource then
  begin
    Extract(AImageTileSource);
    InsertAfter(FLast, AImageTileSource);
  end;
end;

{ TdxMapControlTileImageSourceStorage }

constructor TdxMapControlTileImageSourceStorage.Create;
begin
  inherited Create;
  FTimeSortedTileSources := TdxMapControlTileImageSources.Create;
  FTileImageSources := TDictionary<TdxMapControlTileIndex, TdxMapControlTileImageSource>.Create;
end;

function TdxMapControlTileImageSourceStorage.CreateTileImageSource(
  const AIndex: TdxMapControlTileIndex; ATileSize: TSize): TdxMapControlTileImageSource;
begin
  Result := FTimeSortedTileSources.AddSource(AIndex, ATileSize);
  FTileImageSources.Add(Result.Index, Result);
end;

destructor TdxMapControlTileImageSourceStorage.Destroy;
begin
  FreeAndNil(FTileImageSources);
  FreeAndNil(FTimeSortedTileSources);
  inherited;
end;

procedure TdxMapControlTileImageSourceStorage.Clear;
begin
  FTileImageSources.Clear;
  FTimeSortedTileSources.Clear;
end;

function TdxMapControlTileImageSourceStorage.Extract(
  ASource: TdxMapControlTileImageSource): TdxMapControlTileImageSource;
begin
  FTimeSortedTileSources.Extract(ASource);
{$IFDEF DELPHIXE2}
  Result := FTileImageSources.ExtractPair(ASource.Index).Value;
{$ELSE}
  if FTileImageSources.TryGetValue(ASource.Index, Result) then
    FTileImageSources.ExtractPair(ASource.Index)
  else
    Result := nil;
{$ENDIF}
end;

procedure TdxMapControlTileImageSourceStorage.Remove(
  ASource: TdxMapControlTileImageSource);
begin
  FTileImageSources.Remove(ASource.Index);
  FTimeSortedTileSources.Delete(ASource);
end;

function TdxMapControlTileImageSourceStorage.TryGetValue(const AKey: TdxMapControlTileIndex;
  out AValue: TdxMapControlTileImageSource): Boolean;
begin
  Result := FTileImageSources.TryGetValue(AKey, AValue);
  if Result then
    FTimeSortedTileSources.MakeLast(AValue);
end;

procedure TdxMapControlTileImageSourceStorage.CheckMemoryLimit(ALimit: Int64;
  AExtraImageSources: TList<TdxMapControlTileImageSource>);
var
  I: Integer;
  AItem, AItemFirst: TdxMapControlTileImageSource;
  AItemMemory: Integer;
  AOverhead: Integer;
  AMemoryUsage: Int64;
begin
  if FTimeSortedTileSources.Last = nil then
     Exit;
  AMemoryUsage := 0;
  AItemFirst := nil;
  AItem := FTimeSortedTileSources.Last as TdxMapControlTileImageSource;
  while AItem <> nil do
  begin
    Inc(AMemoryUsage, AItem.GetMemoryUsage);
    AItemFirst := AItem;
    AItem := AItem.Prev as TdxMapControlTileImageSource;
  end;
  if AMemoryUsage > ALimit then
  begin
    AOverhead := AMemoryUsage - ALimit;
    AItem := AItemFirst;
    while (AItem <> nil) and (AOverhead > 0) do
    begin
      AItemMemory := AItem.GetMemoryUsage;
      if (AItemMemory > 0) and (AItem.Index.Level > PreseveLevel) then
      begin
        Dec(AOverhead, AItemMemory);
        AExtraImageSources.Add(AItem);
      end;
      AItem := AItem.Next as TdxMapControlTileImageSource;
    end;
    for I := 0 to AExtraImageSources.Count - 1 do
      Extract(AExtraImageSources[I]);
  end;
end;

end.
