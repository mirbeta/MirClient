{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
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

unit cxSchedulerRangeControlClientProperties;

{$I cxVer.inc}

interface

uses
  Windows, Types, Graphics, Math, Variants, Classes, SysUtils, DateUtils,
  cxGraphics, dxCoreClasses, cxClasses, cxGeometry,
  cxScheduler, cxSchedulerStorage, cxSchedulerCustomControls, cxSchedulerUtils,
  dxRangeControl, dxTypeHelpers, Generics.Collections, Generics.Defaults;

type
  TcxSchedulerCustomRangeControlClientProperties = class;
  TcxSchedulerRangeControlClientStyle = class;

  TcxSchedulerRangeControlContentElementViewInfo = class(TdxRangeControlDateTimeHeaderClientContentElementViewInfo)
  private
    const
      ThumbnailDefaultRowHeight = 7;
      ContentOffset = 1;
      ThumbnailRowIndent = 1;
  private
    FEventColors: TList<TColor>;
    FIsEnoughSpaceForText: Boolean;
    FIsThumbnailMode: Boolean;
    FThumbnailRowHeight: Integer;
    function GetProperties: TcxSchedulerCustomRangeControlClientProperties;
  public
    procedure CalculateBounds(const ABounds: TRect); override;
    procedure Paint(ACanvas: TcxCanvas); override;
    property Properties: TcxSchedulerCustomRangeControlClientProperties read GetProperties;
  end;

  TcxSchedulerRangeControlContentViewInfo = class(TdxRangeControlDateTimeHeaderClientContentViewInfo)
  private
    FFont: TFont;
    function GetStyle: TcxSchedulerRangeControlClientStyle;
  protected
    function CreateContentElementViewInfo: TdxRangeControlDateTimeHeaderClientContentElementViewInfo; override;
    function GetFont: TFont; virtual;
    function GetFontColor: TColor; virtual;
    property Style: TcxSchedulerRangeControlClientStyle read GetStyle;
  public
    constructor Create(AViewInfo: TdxCustomRangeControlViewInfo); override;
    destructor Destroy; override;
    procedure CalculateBounds(const ABounds: TRect); override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TcxSchedulerRangeControlClientViewInfo = class(TdxRangeControlDateTimeHeaderClientViewInfo)
  private
    function GetProperties: TcxSchedulerCustomRangeControlClientProperties;
  protected
    function CreateContentInfo: TdxRangeControlContentViewInfo; override;
  public
    property Properties: TcxSchedulerCustomRangeControlClientProperties read GetProperties;
  end;

  TcxSchedulerRangeControlDataDisplayType = (ddtAuto, ddtThumbnail, ddtNumber);

  TcxSchedulerRangeControlIntervalEventInfo = class(TcxDoublyLinkedObject)
  private
    FColors: TList<TColor>;
    FInterval: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AValue: TColor);
    property Colors: TList<TColor> read FColors;
    property Interval: TDateTime read FInterval write FInterval;
  end;

  TcxSchedulerRangeControlIntervalEventInfos = class(TcxDoublyLinkedObjectList)
  public
    function AddInfo: TcxSchedulerRangeControlIntervalEventInfo;
    procedure MakeLast(AInfo: TcxSchedulerRangeControlIntervalEventInfo);
  end;

  TcxSchedulerRangeControlEventsCache = class
  private
    FTimeSortedEventInfos: TcxSchedulerRangeControlIntervalEventInfos;
    FEventInfos: TDictionary<TDateTime, TcxSchedulerRangeControlIntervalEventInfo>;
    FOwner: TcxSchedulerCustomRangeControlClientProperties;
    function GetStorage: TcxCustomSchedulerStorage;
  protected
    function AddEvents(const AMin, AMax: TDateTime): TList<TColor>;
    property Storage: TcxCustomSchedulerStorage read GetStorage;
  public
    constructor Create(AOwner: TcxSchedulerCustomRangeControlClientProperties);
    destructor Destroy; override;
    procedure Clear;
    function GetEvents(const AMin, AMax: TDateTime): TList<TColor>;
  end;

  TcxSchedulerRangeControlClientStyle = class(TdxRangeControlDateTimeHeaderClientStyle)
  private
    FEventCounterFont: TFont;
    FEventCounterTextColor: TColor;
    procedure FontChanged(Sender: TObject);
    procedure SetEventCounterFont(Value: TFont);
    procedure SetEventCounterTextColor(Value: TColor);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property EventCounterFont: TFont read FEventCounterFont write SetEventCounterFont;
    property EventCounterTextColor: TColor read FEventCounterTextColor write SetEventCounterTextColor default clDefault;
  end;

  TcxSchedulerRangeControlAutoAdjustment = (aaClient, aaRangeControl);
  TcxSchedulerRangeControlAutoAdjustments = set of TcxSchedulerRangeControlAutoAdjustment;

  TcxSchedulerRangeControlAutoAdjustingInfo = record
    PrimaryScale: TdxRangeControlDateTimeScale;
    Scales: TdxRangeControlDateTimeScaleList;
    MinRangeValue: TDateTime;
    MaxRangeValue: TDateTime;
  end;

  TcxSchedulerRangeControlAutoAdjustingEvent = procedure (Sender: TObject;
    var AInfo: TcxSchedulerRangeControlAutoAdjustingInfo) of object;

  TcxSchedulerCustomRangeControlClientProperties = class(TdxRangeControlCustomDateTimeHeaderClientProperties)
  private
    FAutoAdjustedScaleList: TdxRangeControlDateTimeScaleList;
    FAllowChangeActiveView: Boolean;
    FAutoAdjustedPrimaryScale: TdxRangeControlDateTimeScale;
    FAutoAdjustMode: Boolean;
    FDataDisplayType: TcxSchedulerRangeControlDataDisplayType;
    FEventsCache: TcxSchedulerRangeControlEventsCache;
    FOnAutoAdjustRangeControlSettings: TcxSchedulerRangeControlAutoAdjustingEvent;
    FThumbnailHeight: Integer;
    function GetAutoAdjustments: TcxSchedulerRangeControlAutoAdjustments;
    function GetScheduler: TcxScheduler;
    procedure SetAllowChangeActiveView(Value: Boolean);
    procedure SetAutoAdjustMode(Value: Boolean);
    procedure SetAutoAdjustments(Value: TcxSchedulerRangeControlAutoAdjustments);
    procedure SetDataDisplayType(Value: TcxSchedulerRangeControlDataDisplayType);
    procedure SetThumbnailHeight(Value: Integer);
  protected
    procedure AdjustPrimaryScale(AScale: TdxRangeControlDateTimeScale);
    function CreateStyle: TdxRangeControlCustomClientStyle; override;
    function CreateViewInfo(AViewInfo: TdxCustomRangeControlViewInfo): TdxRangeControlCustomClientViewInfo; override;
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;
    procedure DoUpdateVisibleScales(AValues: TdxRangeControlDateTimeScaleList); override;
    function GetActualPrimaryScale: TdxRangeControlDateTimeScale; override;
    procedure PrimaryScaleChanged; override;
    procedure StorageChanged;

    property Scheduler: TcxScheduler read GetScheduler;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure DoAutoAdjustRangeControlSettings(AInfo: TcxSchedulerRangeControlAutoAdjustingInfo);
    property ActualPrimaryScale;
    property AutoAdjustments: TcxSchedulerRangeControlAutoAdjustments read GetAutoAdjustments write SetAutoAdjustments default [aaClient, aaRangeControl];
    property DataDisplayType: TcxSchedulerRangeControlDataDisplayType read FDataDisplayType write SetDataDisplayType default ddtAuto;
    property ThumbnailHeight: Integer read FThumbnailHeight write SetThumbnailHeight default 0;
    property OnAutoAdjustRangeControlSettings: TcxSchedulerRangeControlAutoAdjustingEvent read FOnAutoAdjustRangeControlSettings write FOnAutoAdjustRangeControlSettings;
  end;

  TcxSchedulerRangeControlClientProperties  = class(TcxSchedulerCustomRangeControlClientProperties)
  published
    property AutoAdjustments;
    property AutoFormatScaleCaptions;
    property PrimaryScale stored False;
    property DataDisplayType;
    property MaxValue;
    property MinValue;
    property ScaleIntervalMinWidth default 30;
    property Scales;
    property Style;
    property ThumbnailHeight;
    property OnAutoAdjustRangeControlSettings;
  end;

implementation

uses
  dxGdiPlusClasses, dxGdiPlusApi, dxCoreGraphics;

type
  TcxSchedulerAccess = class(TcxScheduler);
  TdxCustomRangeControlViewInfoAccess = class(TdxCustomRangeControlViewInfo);

function DateTimeInRange(ADateTime: TDateTime; AStartDateTime, AEndDateTime: TDateTime; aInclusive: Boolean = True): Boolean;
begin
{$IFDEF DELPHIXE}
  Result := DateUtils.DateTimeInRange(ADateTime, AStartDateTime, AEndDateTime, aInclusive);
{$ELSE}
  if aInclusive then
    Result := (AStartDateTime <= ADateTime) and (ADateTime <= AEndDateTime)
  else
    Result := (AStartDateTime < ADateTime) and (ADateTime < AEndDateTime);
{$ENDIF}
end;

procedure TcxSchedulerRangeControlContentElementViewInfo.CalculateBounds(const ABounds: TRect);
var
  AThumbnailsHeight: Integer;
  ATextSize: TSize;
begin
  inherited CalculateBounds(ABounds);
  FEventColors := Properties.FEventsCache.GetEvents(MinDate, MaxDate);
  if FEventColors <> nil then
  begin
    FThumbnailRowHeight := IfThen(Properties.ThumbnailHeight <> 0, Properties.ThumbnailHeight,
      ScaleFactor.Apply(ThumbnailDefaultRowHeight));
    AThumbnailsHeight := FEventColors.Count * (FThumbnailRowHeight + ScaleFactor.Apply(ThumbnailRowIndent)) +
      ScaleFactor.Apply(ThumbnailRowIndent);
    FIsThumbnailMode := (Properties.DataDisplayType = ddtThumbnail) or (Properties.DataDisplayType = ddtAuto) and
      (AThumbnailsHeight <= (Bounds.Height - 2 * ScaleFactor.Apply(ContentOffset)));
    if not FIsThumbnailMode then
    begin
      ATextSize := cxTextSize((ViewInfo.ClientInfo.Content as TcxSchedulerRangeControlContentViewInfo).GetFont,
        IntToStr(FEventColors.Count));
      FIsEnoughSpaceForText := (ATextSize.cx <= (cxRectWidth(Bounds) - ScaleFactor.Apply(4))) and
        (ATextSize.cy <= (cxRectHeight(Bounds) - ScaleFactor.Apply(4)));
    end;
  end;
end;

function TcxSchedulerRangeControlContentElementViewInfo.GetProperties: TcxSchedulerCustomRangeControlClientProperties;
begin
  Result := ViewInfo.ClientProperties as TcxSchedulerCustomRangeControlClientProperties;
end;

procedure TcxSchedulerRangeControlContentElementViewInfo.Paint(ACanvas: TcxCanvas);
var
  I: Integer;
  AAppointmentColor: TColor;
  ARect: TRect;
begin
  if FEventColors = nil then
    Exit;
  if FIsThumbnailMode or not FIsEnoughSpaceForText then
  begin
    ARect := cxRectInflate(Bounds, -ScaleFactor.Apply(ContentOffset), -ScaleFactor.Apply(ContentOffset));
    ARect.Top := ARect.Bottom;
    for I := 0 to FEventColors.Count - 1 do
    begin
      ARect.Bottom := ARect.Top - ScaleFactor.Apply(1);
      ARect.Top := ARect.Bottom - FThumbnailRowHeight;
      if ARect.Top > Bounds.Top + ScaleFactor.Apply(ContentOffset) then
      begin
        if FEventColors[I] = clDefault then
          AAppointmentColor := Painter.GetRangeControlDefaultElementColor
        else
          AAppointmentColor := FEventColors[I];
        ACanvas.FillRect(ARect, AAppointmentColor);
      end;
    end;
  end
  else
    ACanvas.DrawTexT(IntToStr(FEventColors.Count), Bounds, cxAlignVCenter or cxAlignHCenter);
end;

{ TcxSchedulerRangeControlContentViewInfo }

constructor TcxSchedulerRangeControlContentViewInfo.Create(AViewInfo: TdxCustomRangeControlViewInfo);
begin
  inherited Create(AViewInfo);
  FFont := TFont.Create;
end;

destructor TcxSchedulerRangeControlContentViewInfo.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TcxSchedulerRangeControlContentViewInfo.CalculateBounds(const ABounds: TRect);
begin
  FFont.Assign(Style.EventCounterFont);
  FFont.Color := GetFontColor;
  inherited CalculateBounds(ABounds);
end;

function TcxSchedulerRangeControlContentViewInfo.CreateContentElementViewInfo: TdxRangeControlDateTimeHeaderClientContentElementViewInfo;
begin
  Result := TcxSchedulerRangeControlContentElementViewInfo.Create(ViewInfo);
end;

function TcxSchedulerRangeControlContentViewInfo.GetFont: TFont;
begin
  Result := FFont;
end;

function TcxSchedulerRangeControlContentViewInfo.GetFontColor: TColor;
begin
  Result := Style.EventCounterTextColor;
  if Result = clDefault then
    Result := Painter.GetRangeControlElementForeColor;
end;

function TcxSchedulerRangeControlContentViewInfo.GetStyle: TcxSchedulerRangeControlClientStyle;
begin
  Result := ViewInfo.ClientProperties.Style as TcxSchedulerRangeControlClientStyle;
end;

procedure TcxSchedulerRangeControlContentViewInfo.Paint(ACanvas: TcxCanvas);
begin
  ACanvas.Font := GetFont;
  inherited Paint(ACanvas);
end;

{ TcxSchedulerRangeControlClientViewInfo }

function TcxSchedulerRangeControlClientViewInfo.CreateContentInfo: TdxRangeControlContentViewInfo;
begin
  Result := TcxSchedulerRangeControlContentViewInfo.Create(ViewInfo);
end;

function TcxSchedulerRangeControlClientViewInfo.GetProperties: TcxSchedulerCustomRangeControlClientProperties;
begin
  Result := Control.ClientProperties as TcxSchedulerCustomRangeControlClientProperties;
end;

{ TdxRangeControlDateTimeHeaderClientProperties }

procedure TcxSchedulerCustomRangeControlClientProperties.ChangeScale(M, D: Integer);
begin
  inherited;
  ThumbnailHeight := MulDiv(ThumbnailHeight, M, D);
end;

constructor TcxSchedulerCustomRangeControlClientProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FAutoAdjustedPrimaryScale := Scales.Day;
  FAllowChangeActiveView := True;
  FAutoAdjustMode := True;
  FEventsCache := TcxSchedulerRangeControlEventsCache.Create(Self);
  FAutoAdjustedScaleList := TdxRangeControlDateTimeScaleList.Create;
end;

destructor TcxSchedulerCustomRangeControlClientProperties.Destroy;
begin
  FreeAndNil(FAutoAdjustedScaleList);
  FreeAndNil(FEventsCache);
  inherited Destroy;
end;

function TcxSchedulerCustomRangeControlClientProperties.CreateViewInfo(AViewInfo: TdxCustomRangeControlViewInfo): TdxRangeControlCustomClientViewInfo;
begin
  Result := TcxSchedulerRangeControlClientViewInfo.Create(AViewInfo);
end;

procedure TcxSchedulerCustomRangeControlClientProperties.DoAssign(Source: TPersistent);
var
  ASource: TcxSchedulerCustomRangeControlClientProperties;
begin
  inherited DoAssign(Source);
  if Source is TcxSchedulerCustomRangeControlClientProperties then
  begin
    ASource := TcxSchedulerCustomRangeControlClientProperties(Source);
    AutoAdjustments := ASource.AutoAdjustments;
    DataDisplayType := ASource.DataDisplayType;
    ThumbnailHeight := ASource.ThumbnailHeight;
  end;
end;

procedure TcxSchedulerCustomRangeControlClientProperties.DoUpdateVisibleScales(AValues: TdxRangeControlDateTimeScaleList);
begin
  if FAutoAdjustMode then
    AValues.AddRange(FAutoAdjustedScaleList)
  else
    inherited DoUpdateVisibleScales(AValues);
end;

procedure TcxSchedulerCustomRangeControlClientProperties.DoAutoAdjustRangeControlSettings(
  AInfo: TcxSchedulerRangeControlAutoAdjustingInfo);
begin
  if Assigned(FOnAutoAdjustRangeControlSettings) then
    FOnAutoAdjustRangeControlSettings(Self, AInfo);
  BeginUpdate;
  try
    AdjustPrimaryScale(AInfo.PrimaryScale);
    FAutoAdjustedScaleList.Clear;
    FAutoAdjustedScaleList.AddRange(AInfo.Scales);
    UpdateVisibleScales;
    MinValue := AInfo.MinRangeValue;
    MaxValue := AInfo.MaxRangeValue;
  finally
    EndUpdate;
  end;
end;

function TcxSchedulerCustomRangeControlClientProperties.GetScheduler: TcxScheduler;
begin
  Result := RangeControl.Client as TcxScheduler;
end;

procedure TcxSchedulerCustomRangeControlClientProperties.PrimaryScaleChanged;
begin
  FEventsCache.Clear;
  inherited PrimaryScaleChanged;
end;

procedure TcxSchedulerCustomRangeControlClientProperties.SetAllowChangeActiveView(Value: Boolean);
begin
  if FAllowChangeActiveView <> Value then
  begin
    FAllowChangeActiveView := Value;
    Changed;
    if FAllowChangeActiveView and (Scheduler <> nil) then
      TcxSchedulerAccess(Scheduler).SelectedRangeChanged;
  end;
end;

procedure TcxSchedulerCustomRangeControlClientProperties.SetAutoAdjustments(Value: TcxSchedulerRangeControlAutoAdjustments);
begin
  SetAutoAdjustMode(aaRangeControl in Value);
  SetAllowChangeActiveView(aaClient in Value);
end;

procedure TcxSchedulerCustomRangeControlClientProperties.SetAutoAdjustMode(Value: Boolean);
begin
  if FAutoAdjustMode <> Value then
  begin
    BeginUpdate;
    try
      FAutoAdjustMode := Value;
      if FAutoAdjustMode then
      begin
        if Scheduler <> nil then
          TcxSchedulerAccess(Scheduler).SynchronizeRangeControl;
      end
      else
      begin
        if PrimaryScale <> FAutoAdjustedPrimaryScale then
          PrimaryScaleChanged;
        UpdateVisibleScales;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcxSchedulerCustomRangeControlClientProperties.SetDataDisplayType(Value: TcxSchedulerRangeControlDataDisplayType);
begin
  if FDataDisplayType <> Value then
  begin
    FDataDisplayType := Value;
    Changed;
  end;
end;

procedure TcxSchedulerCustomRangeControlClientProperties.SetThumbnailHeight(Value: Integer);
begin
  Value := Max(0, Value);
  if FThumbnailHeight <> Value then
  begin
    FThumbnailHeight := Value;
    Changed;
  end;
end;

procedure TcxSchedulerCustomRangeControlClientProperties.StorageChanged;
begin
  FEventsCache.Clear;
  RangeControl.ContentChanged;
end;

procedure TcxSchedulerCustomRangeControlClientProperties.AdjustPrimaryScale(AScale: TdxRangeControlDateTimeScale);
begin
  if AScale = nil then
    AScale := Scales.Day;
  if FAutoAdjustedPrimaryScale <> AScale then
  begin
    FAutoAdjustedPrimaryScale := AScale;
    PrimaryScaleChanged;
  end;
end;

function TcxSchedulerCustomRangeControlClientProperties.CreateStyle: TdxRangeControlCustomClientStyle;
begin
  Result := TcxSchedulerRangeControlClientStyle.Create(Self);
end;

function TcxSchedulerCustomRangeControlClientProperties.GetActualPrimaryScale: TdxRangeControlDateTimeScale;
begin
  if FAutoAdjustMode then
    Result := FAutoAdjustedPrimaryScale
  else
    Result := inherited GetActualPrimaryScale;
end;

function TcxSchedulerCustomRangeControlClientProperties.GetAutoAdjustments: TcxSchedulerRangeControlAutoAdjustments;
begin
  Result := [];
  if FAutoAdjustMode then
    Include(Result, aaRangeControl);
  if FAllowChangeActiveView then
    Include(Result, aaClient);
end;

{ TcxSchedulerRangeControlIntervalEventInfo }

constructor TcxSchedulerRangeControlIntervalEventInfo.Create;
begin
  inherited Create;
end;

destructor TcxSchedulerRangeControlIntervalEventInfo.Destroy;
begin
  FreeAndNil(FColors);
  inherited Destroy;
end;

procedure TcxSchedulerRangeControlIntervalEventInfo.Add(AValue: TColor);
begin
  if FColors = nil then
    FColors := TList<TColor>.Create;
  FColors.Add(AValue);
end;

{ TcxSchedulerRangeControlEventsCache }

constructor TcxSchedulerRangeControlEventsCache.Create(AOwner: TcxSchedulerCustomRangeControlClientProperties);
begin
  inherited Create;
  FOwner := AOwner;
  FTimeSortedEventInfos := TcxSchedulerRangeControlIntervalEventInfos.Create;
  FEventInfos := TDictionary<TDateTime, TcxSchedulerRangeControlIntervalEventInfo>.Create;
end;

destructor TcxSchedulerRangeControlEventsCache.Destroy;
begin
  FreeAndNil(FEventInfos);
  FreeAndNil(FTimeSortedEventInfos);
  inherited Destroy;
end;

function TcxSchedulerRangeControlEventsCache.AddEvents(const AMin, AMax: TDateTime): TList<TColor>;
const
  MaxCachedIntervalCount = 2000;
  BufferIntervalCount = 100;
var
  AEvents: TcxSchedulerCachedEventList;
  I: Integer;
  AEvent: TcxSchedulerControlEvent;
  AInfo: TcxSchedulerRangeControlIntervalEventInfo;
  AResourceIDs: Variant;
  AVisibleResourceCount: Integer;
begin
  if Storage <> nil then
  begin
    if FEventInfos.Count >= MaxCachedIntervalCount then
      for I := 0 to BufferIntervalCount - 1 do
      begin
        AInfo := FTimeSortedEventInfos.First as TcxSchedulerRangeControlIntervalEventInfo;
        FEventInfos.Remove(AInfo.Interval);
        FTimeSortedEventInfos.Delete(AInfo);
      end;
    AInfo := FTimeSortedEventInfos.AddInfo;
    AInfo.Interval := AMin;
    AEvents := TcxSchedulerCachedEventList.Create;
    try
      if Storage.ResourceCount > 0 then
      begin
        AVisibleResourceCount := Storage.Resources.ResourceItems.VisibleResourceCount;
        AResourceIDs := VarArrayCreate([0, AVisibleResourceCount], varVariant);
        for I := 0 to AVisibleResourceCount - 1 do
          AResourceIDs[I] := Storage.Resources.ResourceItems.VisibleResources[I].ResourceID;
        AResourceIDs[AVisibleResourceCount] := Null;
        Storage.GetEvents(AEvents, AMin, AMax, AResourceIDs);
      end
      else
        Storage.GetEvents(AEvents, AMin, AMax);
      for I := 0 to AEvents.Count - 1 do
      begin
        AEvent := AEvents.Items[I];
        if (AEvent.Start = AMin) or (AEvent.Finish = AMax) or
          DateTimeInRange(AEvent.Start, AMin,  AMax, False) or
          DateTimeInRange(AEvent.Finish, AMin,  AMax, False) or
          DateTimeInRange(AMin, AEvent.Start,  AEvent.Finish, False) then
          AInfo.Add(AEvent.LabelColor);
      end;
    finally
      AEvents.Free;
    end;
    Result := AInfo.Colors;
    FEventInfos.Add(AMin, AInfo);
  end
  else
    Result := nil;
end;

procedure TcxSchedulerRangeControlEventsCache.Clear;
begin
  FEventInfos.Clear;
  FTimeSortedEventInfos.Clear;
end;

function TcxSchedulerRangeControlEventsCache.GetEvents(const AMin, AMax: TDateTime): TList<TColor>;
var
  AInfo: TcxSchedulerRangeControlIntervalEventInfo;
begin
  Result := nil;
  if Storage <> nil then
    if FEventInfos.TryGetValue(AMin, AInfo) then
    begin
      Result := AInfo.Colors;
      FTimeSortedEventInfos.MakeLast(AInfo);
    end
    else
      Result := AddEvents(AMin, AMax);
end;

function TcxSchedulerRangeControlEventsCache.GetStorage: TcxCustomSchedulerStorage;
begin
  if FOwner.Scheduler <> nil then
    Result := FOwner.Scheduler.Storage
  else
    Result := nil;
end;

function TcxSchedulerRangeControlIntervalEventInfos.AddInfo: TcxSchedulerRangeControlIntervalEventInfo;
begin
  Result := TcxSchedulerRangeControlIntervalEventInfo.Create;
  DoAdd(Result);
end;

procedure TcxSchedulerRangeControlIntervalEventInfos.MakeLast(AInfo: TcxSchedulerRangeControlIntervalEventInfo);
begin
  if FLast <> AInfo then
  begin
    Extract(AInfo);
    InsertAfter(FLast, AInfo);
  end;
end;

{ TcxSchedulerRangeControlClientStyle }

procedure TcxSchedulerRangeControlClientStyle.ChangeScale(M, D: Integer);
begin
  inherited;
  EventCounterFont.Height := MulDiv(EventCounterFont.Height, M, D)
end;

constructor TcxSchedulerRangeControlClientStyle.Create(AOwner: TPersistent);
begin
  inherited;
  FEventCounterTextColor := clDefault;
  FEventCounterFont := TFont.Create;
  FEventCounterFont.Size := 26;
  FEventCounterFont.OnChange := FontChanged;
end;

destructor TcxSchedulerRangeControlClientStyle.Destroy;
begin
  FreeAndNil(FEventCounterFont);
  inherited Destroy;
end;

procedure TcxSchedulerRangeControlClientStyle.DoAssign(Source: TPersistent);
var
  ASource: TcxSchedulerRangeControlClientStyle;
begin
  inherited;
  if Source is TcxSchedulerRangeControlClientStyle then
  begin
    ASource := TcxSchedulerRangeControlClientStyle(Source);
    EventCounterTextColor := ASource.EventCounterTextColor;
    EventCounterFont := ASource.EventCounterFont;
  end;
end;

procedure TcxSchedulerRangeControlClientStyle.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TcxSchedulerRangeControlClientStyle.SetEventCounterFont(Value: TFont);
begin
  FEventCounterFont.Assign(Value);
end;

procedure TcxSchedulerRangeControlClientStyle.SetEventCounterTextColor(Value: TColor);
begin
  if FEventCounterTextColor <> Value then
  begin
    FEventCounterTextColor := Value;
    Changed;
  end;
end;

initialization
  dxRangeControlClients.Register(TcxSchedulerRangeControlClientProperties, 'Scheduler');

finalization
  dxRangeControlClients.Unregister(TcxSchedulerRangeControlClientProperties);

end.
