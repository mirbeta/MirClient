{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDocking                                           }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDOCKING AND ALL ACCOMPANYING   }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxDockStyleVS2005;

{$I cxVer.inc}

interface

uses
  Types, Windows, Classes, SysUtils, Graphics, Controls,
  dxCore, cxClasses, cxControls, cxGraphics, dxDockControl, cxGeometry, dxSkinsCore;

type
  TdxDockingTargetSelectorZoneKind = (tszLeft, tszRight, tszTop, tszBottom, tszClient,
    tszDockSiteLeft, tszDockSiteRight, tszDockSiteTop, tszDockSiteBottom);

  TdxDockingCustomTargetSelector = class;

  { TdxDockingTargetSelectorZoneViewInfo }

  TdxDockingTargetSelectorZoneViewInfo = class
  strict private
    FBounds: TRect;
    FKind: TdxDockingTargetSelectorZoneKind;
    FOwner: TdxDockingCustomTargetSelector;
    FZone: TdxZone;

    function GetActive: Boolean;
    function GetEnabled: Boolean;
  public
    constructor Create(AOwner: TdxDockingCustomTargetSelector;
      AKind: TdxDockingTargetSelectorZoneKind; AZone: TdxZone); virtual;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    //
    property Active: Boolean read GetActive;
    property Bounds: TRect read FBounds write FBounds;
    property Enabled: Boolean read GetEnabled;
    property Kind: TdxDockingTargetSelectorZoneKind read FKind;
    property Owner: TdxDockingCustomTargetSelector read FOwner;
    property Zone: TdxZone read FZone;
  end;

  { TdxDockingTargetSelectorCustomPainter }

  TdxDockingTargetSelectorCustomPainter = class
  strict private
    FScaleFactor: TdxScaleFactor;
  public
    procedure Initialize(AScaleFactor: TdxScaleFactor); virtual;
    // Client Target Selector
    procedure DrawClientTargetSelectorBackground(ACanvas: TcxCanvas; const R: TRect); virtual; abstract;
    function GetClientTargetSelectorContentOffsets: TRect; virtual; abstract;
    // Zones
    procedure DrawZone(ACanvas: TcxCanvas; const R: TRect; AActive: Boolean;
      AEnabled: Boolean; AKind: TdxDockingTargetSelectorZoneKind); virtual; abstract;
    function GetZoneSize(AKind: TdxDockingTargetSelectorZoneKind): TSize; virtual; abstract;
    //
    property ScaleFactor: TdxScaleFactor read FScaleFactor;
  end;

  { TdxDockingTargetSelectorPainter }

  TdxDockingTargetSelectorPainter = class(TdxDockingTargetSelectorCustomPainter)
  strict private const
    StatesCount = 3;
  strict private
    FBackgroundClient: TdxSkinImage;
    FBackgroundDockSite: TdxSkinImage;
    FSelectors: array[TdxDockingTargetSelectorZoneKind] of TdxSkinImage;

    function LoadTexture(AResourceName: string): TdxSkinImage;
    procedure ResourcesAlloc;
    procedure ResourcesRelease;
  public
    destructor Destroy; override;
    procedure Initialize(AScaleFactor: TdxScaleFactor); override;
    // Client Target Selector
    procedure DrawClientTargetSelectorBackground(ACanvas: TcxCanvas; const R: TRect); override;
    function GetClientTargetSelectorContentOffsets: TRect; override;
    // Zones
    procedure DrawZone(ACanvas: TcxCanvas; const R: TRect; AActive, AEnabled: Boolean; AKind: TdxDockingTargetSelectorZoneKind); override;
    function GetZoneSize(AKind: TdxDockingTargetSelectorZoneKind): TSize; override;
  end;

  { TdxDockingCustomTargetSelector }

  TdxDockingCustomTargetSelectorClass = class of TdxDockingCustomTargetSelector;
  TdxDockingCustomTargetSelector = class(TcxLayeredDragImage)
  strict private
    FActiveTargetZone: TdxDockingTargetSelectorZoneViewInfo;
    FPainter: TdxDockingTargetSelectorCustomPainter;
    FDockControl: TdxCustomDockControl;
    FTargetZones: TcxObjectList;

    function GetScaleFactor: TdxScaleFactor;
    function GetTargetZone: TdxZone;
    function GetTargetZoneSize(AKind: TdxDockingTargetSelectorZoneKind): TSize;
    function GetTargetZoneViewInfo(Index: Integer): TdxDockingTargetSelectorZoneViewInfo;
    function GetTargetZoneViewInfoCount: Integer;
    procedure SetActiveTargetZone(AValue: TdxDockingTargetSelectorZoneViewInfo);
    procedure SetDockControl(AValue: TdxCustomDockControl);
  protected
    function AddZoneViewInfo(AKind: TdxDockingTargetSelectorZoneKind): TdxDockingTargetSelectorZoneViewInfo;
    procedure Calculate; virtual;
    procedure CalculateTargetZones; virtual; abstract;
    function CreatePainter: TdxDockingTargetSelectorCustomPainter; virtual;
    procedure DrawContent(ACanvas: TcxCanvas); virtual;
    //
    property TargetZones: TcxObjectList read FTargetZones;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    procedure CalculateHitTest(const AScreenPoint: TPoint); virtual;
    function GetTargetZoneAtPoint(const APoint: TPoint): TdxDockingTargetSelectorZoneViewInfo;
    procedure Update; override;
    //
    property ActiveTargetZone: TdxDockingTargetSelectorZoneViewInfo read FActiveTargetZone write SetActiveTargetZone;
    property DockControl: TdxCustomDockControl read FDockControl write SetDockControl;
    property Painter: TdxDockingTargetSelectorCustomPainter read FPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property TargetZone: TdxZone read GetTargetZone;
    property TargetZoneSize[AKind: TdxDockingTargetSelectorZoneKind]: TSize read GetTargetZoneSize;
    property TargetZoneViewInfo[Index: Integer]: TdxDockingTargetSelectorZoneViewInfo read GetTargetZoneViewInfo;
    property TargetZoneViewInfoCount: Integer read GetTargetZoneViewInfoCount;
  end;

  { TdxDockingControllerVS2005Helper }

  TdxDockingControllerVS2005Helper = class(TdxDockingControllerHelper)
  strict private
    FTargetSelectors: array[TdxDockingType] of TdxDockingCustomTargetSelector;

    function GetTargetSelector(AType: TdxDockingType): TdxDockingCustomTargetSelector;
  protected
    function CheckTargetDockControl(const APoint: TPoint;
      AControl, ATargetDockControl: TdxCustomDockControl): Boolean; virtual;
    function CreateTargetSelector(AType: TdxDockingType): TdxDockingCustomTargetSelector; virtual;
    function GetDockingTargetControl(AControl: TdxCustomDockControl; const APoint: TPoint): TdxCustomDockControl; virtual;
    function GetDockSite(AControl: TWinControl): TdxCustomDockSite; virtual;
    function GetTargetZone(const APoint: TPoint): TdxZone; virtual;
    procedure SetDockingParams(AControl: TdxCustomDockControl; AZone: TdxZone; const APoint: TPoint);
    procedure UpdateTargetSelectors(ATargetDockControl, ATargetDockSite: TdxCustomDockControl); virtual;
  public
    procedure DockingFinish(AControl: TdxCustomDockControl; const APoint: TPoint); override;
    procedure DockingMove(AControl: TdxCustomDockControl; const APoint: TPoint); override;
    procedure DockingStart(AControl: TdxCustomDockControl; const APoint: TPoint); override;
    //
    property TargetSelector[AType: TdxDockingType]: TdxDockingCustomTargetSelector read GetTargetSelector;
  end;

  { TdxDockingClientTargetSelector }

  TdxDockingClientTargetSelector = class(TdxDockingCustomTargetSelector)
  strict private
    function CalculateClientRect: TRect;
    function CalculateHorizontalRowMaxSize: Integer;
    function CalculateVerticalRowMaxSize: Integer;
  protected
    procedure CalculateClientZone; virtual;
    procedure CalculateHorizontalZones; virtual;
    procedure CalculateTargetZones; override;
    procedure CalculateVerticalZones; virtual;
    procedure DrawContent(ACanvas: TcxCanvas); override;
  end;

  { TdxDockingParentTargetSelector }

  TdxDockingParentTargetSelector = class(TdxDockingCustomTargetSelector)
  protected
    procedure CalculateTargetZones; override;
    function GetZoneKind: TdxDockingTargetSelectorZoneKind; virtual; abstract;
    procedure PlaceTargetSelector(const ATargetWindowRect: TRect); virtual; abstract;
  public
    property ZoneKind: TdxDockingTargetSelectorZoneKind read GetZoneKind;
  end;

  { TdxDockingLeftTargetSelector }

  TdxDockingLeftTargetSelector = class(TdxDockingParentTargetSelector)
  protected
    function GetZoneKind: TdxDockingTargetSelectorZoneKind; override;
    procedure PlaceTargetSelector(const ATargetWindowRect: TRect); override;
  end;

  { TdxDockingRightTargetSelector }

  TdxDockingRightTargetSelector = class(TdxDockingParentTargetSelector)
  protected
    function GetZoneKind: TdxDockingTargetSelectorZoneKind; override;
    procedure PlaceTargetSelector(const ATargetWindowRect: TRect); override;
  end;

  { TdxDockingTopTargetSelector }

  TdxDockingTopTargetSelector = class(TdxDockingParentTargetSelector)
  protected
    function GetZoneKind: TdxDockingTargetSelectorZoneKind; override;
    procedure PlaceTargetSelector(const ATargetWindowRect: TRect); override;
  end;

  { TdxDockingBottomTargetSelector }

  TdxDockingBottomTargetSelector = class(TdxDockingParentTargetSelector)
  protected
    function GetZoneKind: TdxDockingTargetSelectorZoneKind; override;
    procedure PlaceTargetSelector(const ATargetWindowRect: TRect); override;
  end;

implementation

uses
  Math, dxDockZones, dxDPIAwareUtils, dxGDIPlusClasses, dxGDIPlusAPI;

{$R dxDockStyleVS2005.res}

type
  TdxCustomDockControlAccess = class(TdxCustomDockControl);

{ TdxDockingTargetSelectorZoneViewInfo }

constructor TdxDockingTargetSelectorZoneViewInfo.Create(
  AOwner: TdxDockingCustomTargetSelector;
  AKind: TdxDockingTargetSelectorZoneKind; AZone: TdxZone);
begin
  inherited Create;
  FOwner := AOwner;
  FKind := AKind;
  FZone := AZone;
end;

procedure TdxDockingTargetSelectorZoneViewInfo.Draw(ACanvas: TcxCanvas);
begin
  Owner.Painter.DrawZone(ACanvas, Bounds, Active, Enabled, Kind);
end;

function TdxDockingTargetSelectorZoneViewInfo.GetActive: Boolean;
begin
  Result := Self = Owner.ActiveTargetZone;
end;

function TdxDockingTargetSelectorZoneViewInfo.GetEnabled: Boolean;
begin
  Result := Zone <> nil;
end;

{ TdxDockingTargetSelectorCustomPainter }

procedure TdxDockingTargetSelectorCustomPainter.Initialize(AScaleFactor: TdxScaleFactor);
begin
  FScaleFactor := AScaleFactor;
end;

{ TdxDockingTargetSelectorPainter }

destructor TdxDockingTargetSelectorPainter.Destroy;
begin
  ResourcesRelease;
  inherited Destroy;
end;

procedure TdxDockingTargetSelectorPainter.DrawClientTargetSelectorBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
  try
    dxGPPaintCanvas.InterpolationMode := imNearestNeighbor;
    dxGPPaintCanvas.PixelOffsetMode := PixelOffsetModeHalf;
    FBackgroundClient.DrawEx(dxGPPaintCanvas, R);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxDockingTargetSelectorPainter.DrawZone(ACanvas: TcxCanvas;
  const R: TRect; AActive, AEnabled: Boolean; AKind: TdxDockingTargetSelectorZoneKind);
const
  StatesMap: array[Boolean, Boolean] of Integer = ((2, 2), (0, 1));
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
  try
    dxGPPaintCanvas.InterpolationMode := imBicubic;
    dxGPPaintCanvas.PixelOffsetMode := PixelOffsetModeHalf;
    if AKind in [tszDockSiteLeft, tszDockSiteRight, tszDockSiteTop, tszDockSiteBottom] then
      FBackgroundDockSite.DrawEx(dxGPPaintCanvas, R);
    FSelectors[AKind].DrawEx(dxGPPaintCanvas, R, ScaleFactor, StatesMap[AEnabled, AActive]);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

function TdxDockingTargetSelectorPainter.GetClientTargetSelectorContentOffsets: TRect;
begin
  Result := ScaleFactor.Apply(cxRect(1, 1, 0, 0));
end;

function TdxDockingTargetSelectorPainter.GetZoneSize(AKind: TdxDockingTargetSelectorZoneKind): TSize;
begin
  Result := ScaleFactor.Apply(cxSize(FBackgroundDockSite.Texture.Width, FBackgroundDockSite.Texture.Height));
end;

procedure TdxDockingTargetSelectorPainter.Initialize(AScaleFactor: TdxScaleFactor);
begin
  ResourcesRelease;
  inherited Initialize(AScaleFactor);
  ResourcesAlloc;
end;

function TdxDockingTargetSelectorPainter.LoadTexture(AResourceName: string): TdxSkinImage;
var
  ABitmap: TBitmap;
  AScaledResName: string;
begin
  ABitmap := TBitmap.Create;
  try
    ABitmap.TransparentColor := clFuchsia;
    ABitmap.Transparent := True;

    if ScaleFactor.Assigned then
    begin
      AScaledResName := AResourceName + '_' + IntToStr(ScaleFactor.Apply(100));
      if FindResource(HInstance, PChar(AScaledResName), RT_BITMAP) <> 0 then
        AResourceName := AScaledResName;
    end;

    ABitmap.LoadFromResourceName(HInstance, AResourceName);
    ABitmap.PixelFormat := pf24bit;

    Result := TdxSkinImage.Create(nil);
    Result.Texture.SetBitmap(ABitmap);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxDockingTargetSelectorPainter.ResourcesAlloc;
const
  SelectorResNames: array[TdxDockingTargetSelectorZoneKind] of string = (
    'DXDOCKINGVS2005LEFT',
    'DXDOCKINGVS2005RIGHT',
    'DXDOCKINGVS2005TOP',
    'DXDOCKINGVS2005BOTTOM',
    'DXDOCKINGVS2005CLIENT',
    'DXDOCKINGVS2005LEFT',
    'DXDOCKINGVS2005RIGHT',
    'DXDOCKINGVS2005TOP',
    'DXDOCKINGVS2005BOTTOM'
  );
var
  AZoneKind: TdxDockingTargetSelectorZoneKind;
begin
  FBackgroundClient := LoadTexture('DXDOCKINGVS2005DOCKCLIENT');
  FBackgroundDockSite := LoadTexture('DXDOCKINGVS2005DOCKSITE');
  FBackgroundDockSite.Margins.All := 2;
  for AZoneKind := Low(FSelectors) to High(FSelectors) do
  begin
    FSelectors[AZoneKind] := LoadTexture(SelectorResNames[AZoneKind]);
    FSelectors[AZoneKind].ImageLayout := ilVertical;
    FSelectors[AZoneKind].ImageCount := 3;
    FSelectors[AZoneKind].Stretch := smNoResize;
  end;
end;

procedure TdxDockingTargetSelectorPainter.ResourcesRelease;
var
  AZoneKind: TdxDockingTargetSelectorZoneKind;
begin
  FreeAndNil(FBackgroundDockSite);
  FreeAndNil(FBackgroundClient);
  for AZoneKind := Low(FSelectors) to High(FSelectors) do
    FreeAndNil(FSelectors[AZoneKind]);
end;

{ TdxDockingCustomTargetSelector }

constructor TdxDockingCustomTargetSelector.Create;
begin
  inherited Create;
  FPainter := CreatePainter;
  FTargetZones := TcxObjectList.Create;
end;

destructor TdxDockingCustomTargetSelector.Destroy;
begin
  FreeAndNil(FTargetZones);
  FreeAndNil(FPainter);
  inherited Destroy;
end;

function TdxDockingCustomTargetSelector.AddZoneViewInfo(
  AKind: TdxDockingTargetSelectorZoneKind): TdxDockingTargetSelectorZoneViewInfo;

  function FindTargetZone(AOwnerControl: TdxCustomDockControl): TdxZone;
  const
    DockingTypeMap: array[TdxDockingTargetSelectorZoneKind] of TdxDockingType =
      (dtLeft, dtRight, dtTop, dtBottom, dtClient, dtLeft, dtRight, dtTop, dtBottom);
  begin
    Result := TdxCustomDockControlAccess(DockControl).DockZones.FindZone(AOwnerControl, DockingTypeMap[AKind]);
  end;

var
  AZone: TdxZone;
begin
  if DockControl.TabContainer <> nil then
    AZone := FindTargetZone(DockControl.TabContainer)
  else
    AZone := nil;

  if AZone = nil then
    AZone := FindTargetZone(DockControl);

  if (AZone <> nil) and not AZone.CanDock(dxDockingController.DockingDockControl) then
    AZone := nil;
  Result := TdxDockingTargetSelectorZoneViewInfo.Create(Self, AKind, AZone);
  TargetZones.Add(Result);
end;

procedure TdxDockingCustomTargetSelector.Calculate;
begin
  TargetZones.Clear;
  Painter.Initialize(ScaleFactor);
  CalculateTargetZones;
  Invalidate;
  Update;
end;

procedure TdxDockingCustomTargetSelector.CalculateHitTest(const AScreenPoint: TPoint);
begin
  if Visible then
    ActiveTargetZone := GetTargetZoneAtPoint(ScreenToClient(AScreenPoint))
  else
    ActiveTargetZone := nil;
end;

function TdxDockingCustomTargetSelector.CreatePainter: TdxDockingTargetSelectorCustomPainter;
begin
  Result := TdxDockingTargetSelectorPainter.Create;
end;

procedure TdxDockingCustomTargetSelector.DrawContent(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to TargetZoneViewInfoCount - 1 do
    TargetZoneViewInfo[I].Draw(ACanvas);
end;

procedure TdxDockingCustomTargetSelector.Update;
begin
  Image.Clear;
  DrawContent(Image.cxCanvas);
  inherited;
end;

function TdxDockingCustomTargetSelector.GetScaleFactor: TdxScaleFactor;
begin
  if DockControl <> nil then
    Result := TdxCustomDockControlAccess(DockControl).ScaleFactor
  else
    Result := dxSystemScaleFactor;
end;

function TdxDockingCustomTargetSelector.GetTargetZone: TdxZone;
begin
  if ActiveTargetZone <> nil then
    Result := ActiveTargetZone.Zone
  else
    Result := nil;
end;

function TdxDockingCustomTargetSelector.GetTargetZoneAtPoint(const APoint: TPoint): TdxDockingTargetSelectorZoneViewInfo;
var
  AViewInfo: TdxDockingTargetSelectorZoneViewInfo;
  I: Integer;
begin
  for I := TargetZoneViewInfoCount - 1 downto 0 do
  begin
    AViewInfo := TargetZoneViewInfo[I];
    if AViewInfo.Enabled and PtInRect(AViewInfo.Bounds, APoint) then
      Exit(AViewInfo);
  end;
  Result := nil;
end;

function TdxDockingCustomTargetSelector.GetTargetZoneSize(AKind: TdxDockingTargetSelectorZoneKind): TSize;
begin
  if Painter <> nil then
    Result := Painter.GetZoneSize(AKind)
  else
    Result := cxNullSize;
end;

function TdxDockingCustomTargetSelector.GetTargetZoneViewInfo(Index: Integer): TdxDockingTargetSelectorZoneViewInfo;
begin
  Result := TdxDockingTargetSelectorZoneViewInfo(TargetZones[Index]);
end;

function TdxDockingCustomTargetSelector.GetTargetZoneViewInfoCount: Integer;
begin
  Result := TargetZones.Count;
end;

procedure TdxDockingCustomTargetSelector.SetActiveTargetZone(AValue: TdxDockingTargetSelectorZoneViewInfo);
begin
  if AValue <> FActiveTargetZone then
  begin
    FActiveTargetZone := AValue;
    Invalidate;
  end;
end;

procedure TdxDockingCustomTargetSelector.SetDockControl(AValue: TdxCustomDockControl);
begin
  if AValue <> DockControl then
  begin
    FActiveTargetZone := nil;
    FDockControl := AValue;
    if DockControl = nil then
      Hide
    else
    begin
      Calculate;
      Show;
    end;
  end;
end;

{ TdxDockingClientTargetSelector }

procedure TdxDockingClientTargetSelector.CalculateTargetZones;
begin
  CalculateClientZone;
  CalculateHorizontalZones;
  CalculateVerticalZones;
  Init(CalculateClientRect, cxNullPoint);
  MoveTo(cxRectCenter(cxGetWindowRect(DockControl), Width, Height).TopLeft);
  TransparentColorValue := clFuchsia;
  TransparentColor := True;
end;

function TdxDockingClientTargetSelector.CalculateClientRect: TRect;
var
  I: Integer;
begin
  Result := cxNullRect;
  for I := 0 to TargetZoneViewInfoCount - 1 do
    Result := cxRectUnion(Result, TargetZoneViewInfo[I].Bounds);
  Result := cxRectInflate(Result, Painter.GetClientTargetSelectorContentOffsets);
end;

function TdxDockingClientTargetSelector.CalculateHorizontalRowMaxSize: Integer;
begin
  Result := Max(TargetZoneSize[tszLeft].cy, TargetZoneSize[tszClient].cy);
  Result := Max(TargetZoneSize[tszRight].cy, Result);
end;

function TdxDockingClientTargetSelector.CalculateVerticalRowMaxSize: Integer;
begin
  Result := Max(TargetZoneSize[tszTop].cx, TargetZoneSize[tszClient].cx);
  Result := Max(TargetZoneSize[tszBottom].cx, Result);
end;

procedure TdxDockingClientTargetSelector.CalculateClientZone;
var
  ARect: TRect;
begin
  ARect := cxRectBounds(TargetZoneSize[tszLeft].cx, TargetZoneSize[tszTop].cy,
    CalculateVerticalRowMaxSize, CalculateHorizontalRowMaxSize);
  ARect := cxRectOffset(ARect, Painter.GetClientTargetSelectorContentOffsets.TopLeft);
  AddZoneViewInfo(tszClient).Bounds := cxRectCenter(ARect, TargetZoneSize[tszClient]);
end;

procedure TdxDockingClientTargetSelector.CalculateHorizontalZones;

  function PlaceZone(const R: TRect; const AZoneSize: TSize): TRect;
  begin
    Result := cxRectCenterVertically(cxRectSetWidth(R, AZoneSize.cx), AZoneSize.cy);
  end;

var
  ARect: TRect;
  AZoneViewInfo: TdxDockingTargetSelectorZoneViewInfo;
begin
  ARect := cxRectBounds(0, TargetZoneSize[tszTop].cy, 0, CalculateHorizontalRowMaxSize);
  ARect := cxRectOffset(ARect, Painter.GetClientTargetSelectorContentOffsets.TopLeft);

  AZoneViewInfo := AddZoneViewInfo(tszLeft);
  AZoneViewInfo.Bounds := PlaceZone(ARect, TargetZoneSize[tszLeft]);
  ARect.Left := AZoneViewInfo.Bounds.Right + CalculateVerticalRowMaxSize;

  AZoneViewInfo := AddZoneViewInfo(tszRight);
  AZoneViewInfo.Bounds := PlaceZone(ARect, TargetZoneSize[tszRight]);
end;

procedure TdxDockingClientTargetSelector.CalculateVerticalZones;

  function PlaceZone(const R: TRect; const AZoneSize: TSize): TRect;
  begin
    Result := cxRectCenterHorizontally(cxRectSetHeight(R, AZoneSize.cy), AZoneSize.cx);
  end;

var
  ARect: TRect;
  AZoneViewInfo: TdxDockingTargetSelectorZoneViewInfo;
begin
  ARect := cxRectBounds(TargetZoneSize[tszLeft].cx, 0, CalculateVerticalRowMaxSize, 0);
  ARect := cxRectOffset(ARect, Painter.GetClientTargetSelectorContentOffsets.TopLeft);

  AZoneViewInfo := AddZoneViewInfo(tszTop);
  AZoneViewInfo.Bounds := PlaceZone(ARect, TargetZoneSize[tszTop]);
  ARect.Top := AZoneViewInfo.Bounds.Bottom + CalculateHorizontalRowMaxSize;

  AZoneViewInfo := AddZoneViewInfo(tszBottom);
  AZoneViewInfo.Bounds := PlaceZone(ARect, TargetZoneSize[tszBottom]);
end;

procedure TdxDockingClientTargetSelector.DrawContent(ACanvas: TcxCanvas);
begin
  Painter.DrawClientTargetSelectorBackground(ACanvas, ClientRect);
  inherited;
end;

{ TdxDockingParentTargetSelector }

procedure TdxDockingParentTargetSelector.CalculateTargetZones;
var
  AZoneViewInfo: TdxDockingTargetSelectorZoneViewInfo;
begin
  AZoneViewInfo := AddZoneViewInfo(ZoneKind);
  AZoneViewInfo.Bounds := cxRect(TargetZoneSize[ZoneKind]);
  Init(AZoneViewInfo.Bounds, cxNullPoint);
  PlaceTargetSelector(cxGetWindowRect(DockControl));
end;

{ TdxDockingLeftTargetSelector }

function TdxDockingLeftTargetSelector.GetZoneKind: TdxDockingTargetSelectorZoneKind;
begin
  Result := tszDockSiteLeft;
end;

procedure TdxDockingLeftTargetSelector.PlaceTargetSelector(const ATargetWindowRect: TRect);
begin
  MoveTo(Point(ATargetWindowRect.Left, cxRectCenterVertically(ATargetWindowRect, Height).Top));
end;

{ TdxDockingRightTargetSelector }

function TdxDockingRightTargetSelector.GetZoneKind: TdxDockingTargetSelectorZoneKind;
begin
  Result := tszDockSiteRight;
end;

procedure TdxDockingRightTargetSelector.PlaceTargetSelector(const ATargetWindowRect: TRect);
begin
  MoveTo(Point(ATargetWindowRect.Right - Width, cxRectCenterVertically(ATargetWindowRect, Height).Top));
end;

{ TdxDockingTopTargetSelector }

function TdxDockingTopTargetSelector.GetZoneKind: TdxDockingTargetSelectorZoneKind;
begin
  Result := tszDockSiteTop;
end;

procedure TdxDockingTopTargetSelector.PlaceTargetSelector(const ATargetWindowRect: TRect);
begin
  MoveTo(Point(cxRectCenterHorizontally(ATargetWindowRect, Width).Left, ATargetWindowRect.Top));
end;

{ TdxDockingBottomTargetSelector }

function TdxDockingBottomTargetSelector.GetZoneKind: TdxDockingTargetSelectorZoneKind;
begin
  Result := tszDockSiteBottom;
end;

procedure TdxDockingBottomTargetSelector.PlaceTargetSelector(const ATargetWindowRect: TRect);
begin
  MoveTo(Point(cxRectCenterHorizontally(ATargetWindowRect, Width).Left, ATargetWindowRect.Bottom - Height));
end;

{ TdxDockingControllerVS2005Helper }

function TdxDockingControllerVS2005Helper.CreateTargetSelector(AType: TdxDockingType): TdxDockingCustomTargetSelector;
const
  TargetSelectorsClasses: array[TdxDockingType] of TdxDockingCustomTargetSelectorClass = (
    TdxDockingClientTargetSelector, TdxDockingLeftTargetSelector,
    TdxDockingTopTargetSelector, TdxDockingRightTargetSelector,
    TdxDockingBottomTargetSelector
  );
begin
  Result := TargetSelectorsClasses[AType].Create;
end;

function TdxDockingControllerVS2005Helper.CheckTargetDockControl(
  const APoint: TPoint; AControl, ATargetDockControl: TdxCustomDockControl): Boolean;

  function IsTargetDockControlFullyVisible: Boolean;
  var
    R: TRect;
  begin
    if ATargetDockControl <> nil then
    begin
      R := ATargetDockControl.ClientRect;
      if cxRectIsEmpty(R) then
        Result := (ATargetDockControl is TdxDockSite) and TdxDockSite(ATargetDockControl).AutoSize
      else
        Result := ATargetDockControl = GetDockingTargetControl(AControl, ATargetDockControl.ClientToScreen(cxRectCenter(R)));
    end
    else
      Result := False;
  end;

begin
  if ATargetDockControl <> AControl then
    Result := IsTargetDockControlFullyVisible
  else
    Result := AControl.TabContainer <> nil;
end;

procedure TdxDockingControllerVS2005Helper.DockingStart(AControl: TdxCustomDockControl; const APoint: TPoint);
var
  ADockingType: TdxDockingType;
begin
  inherited DockingStart(AControl, APoint);
  for ADockingType := Low(TdxDockingType) to High(TdxDockingType) do
    FTargetSelectors[ADockingType] := CreateTargetSelector(ADockingType);
end;

procedure TdxDockingControllerVS2005Helper.DockingMove(AControl: TdxCustomDockControl; const APoint: TPoint);

  function GetTabContainerZone(ATargetDockControl: TdxCustomDockControl; out AZone: TdxZone): Boolean;
  begin
    if ATargetDockControl <> nil then
      AZone := ATargetDockControl.GetDockZoneAtPos(AControl, APoint)
    else
      AZone := nil;

    Result := AZone is TdxTabContainerTabZone;
  end;

var
  ATargetDockControl: TdxCustomDockControl;
  ATargetDockSite: TdxCustomDockControl;
  ATargetZone: TdxZone;
begin
  ATargetDockControl := GetDockingTargetControl(AControl, APoint);
  ATargetDockSite := GetDockSite(ATargetDockControl);
  if GetTabContainerZone(ATargetDockControl, ATargetZone) then
  begin
    SetDockingParams(AControl, ATargetZone, APoint);
    UpdateTargetSelectors(nil, nil);
  end
  else
  begin
    if not CheckTargetDockControl(APoint, AControl, ATargetDockControl) then
      ATargetDockControl := nil;
    UpdateTargetSelectors(ATargetDockControl, ATargetDockSite);
    SetDockingParams(AControl, GetTargetZone(APoint), APoint);
  end;
end;

procedure TdxDockingControllerVS2005Helper.DockingFinish(AControl: TdxCustomDockControl; const APoint: TPoint);
var
  AType: TdxDockingType;
begin
  inherited DockingFinish(AControl, APoint);
  for AType := Low(TdxDockingType) to High(TdxDockingType) do
    FreeAndNil(FTargetSelectors[AType]);
end;

procedure TdxDockingControllerVS2005Helper.UpdateTargetSelectors(ATargetDockControl, ATargetDockSite: TdxCustomDockControl);
var
  ADockingType: TdxDockingType;
begin
  for ADockingType := Low(TdxDockingType) to High(TdxDockingType) do
  begin
    if ADockingType <> dtClient then
      TargetSelector[ADockingType].DockControl := ATargetDockSite
    else
      TargetSelector[ADockingType].DockControl := ATargetDockControl;
  end;
end;

function TdxDockingControllerVS2005Helper.GetDockingTargetControl(
  AControl: TdxCustomDockControl; const APoint: TPoint): TdxCustomDockControl;
var
  AStyles: Cardinal;
begin
  if (AControl.FloatDockSite <> nil) and AControl.FloatFormVisible then
  begin
    AStyles := dxSetWindowStyle(AControl.FloatForm.Handle, WS_VISIBLE, soSubtract);
    Result := TdxCustomDockControlAccess(AControl).GetDockingTargetControlAtPos(APoint);
    dxSetWindowStyle(AControl.FloatForm.Handle, AStyles, soSet);
  end
  else
    Result := TdxCustomDockControlAccess(AControl).GetDockingTargetControlAtPos(APoint);
end;

function TdxDockingControllerVS2005Helper.GetDockSite(AControl: TWinControl): TdxCustomDockSite;
begin
  Result := nil;
  while AControl <> nil do
  begin
    if AControl is TdxDockSite then
    begin
      Result := TdxDockSite(AControl);
      Break;
    end;
    AControl := AControl.Parent;
  end;
end;

function TdxDockingControllerVS2005Helper.GetTargetSelector(
  AType: TdxDockingType): TdxDockingCustomTargetSelector;
begin
  Result := FTargetSelectors[AType];
end;

function TdxDockingControllerVS2005Helper.GetTargetZone(const APoint: TPoint): TdxZone;
var
  ADockingType: TdxDockingType;
begin
  for ADockingType := Low(ADockingType) to High(ADockingType) do
    TargetSelector[ADockingType].CalculateHitTest(APoint);
  for ADockingType := Low(ADockingType) to High(ADockingType) do
  begin
    Result := TargetSelector[ADockingType].TargetZone;
    if Result <> nil then Break;
  end;
end;

procedure TdxDockingControllerVS2005Helper.SetDockingParams(
  AControl: TdxCustomDockControl; AZone: TdxZone; const APoint: TPoint);
begin
  if AZone <> nil then
  begin
    if not CanDocking(AControl, AZone.Owner, AZone, APoint) then
      AZone := nil;
  end;
  TdxCustomDockControlAccess(AControl).SetDockingParams(AZone, APoint);
end;

end.
