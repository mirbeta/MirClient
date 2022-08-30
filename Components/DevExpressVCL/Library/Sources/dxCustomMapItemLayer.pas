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

unit dxCustomMapItemLayer;

interface

{$I cxVer.inc}

uses
  Types, Classes, Math, SysUtils, RtlConsts,
  cxGraphics, cxGeometry, cxClasses, dxCoreClasses, dxGDIPlusClasses, cxLookAndFeelPainters,
  Generics.Defaults, Generics.Collections,
  dxMapControlTypes, dxMapLayer, dxMapControlElementViewInfo,
  dxMapControlProjections, dxMapItem, dxMapItemStyle;

type
  TdxCustomMapItemLayer = class;

  TdxMapItemLayerViewInfo = class(TdxMapLayerViewInfo)
  private
    FLastNonPointerItemIndex: Integer;
    FSelectedElements: TdxFastObjectList;
    FSelectedPointers: TdxFastObjectList;
    FSelectedTitles: TdxFastObjectList;
    FItemTitles: TdxFastObjectList;
  protected
    procedure AddVisibleElements; override;
  public
    constructor Create(AOwner: TdxMapLayer); override;
    destructor Destroy; override;
    procedure CalculateSize; override;
    procedure ClearCache; override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxCustomMapItemLayerGetItemHintEvent = procedure (Sender: TdxCustomMapItemLayer;
    Item: TdxMapItem; var AHint: string) of object;

  TdxCustomMapItemLayer = class(TdxMapLayer)
  private
    FAllowHotTrack: Boolean;
    FMapItems: TdxMapItems;
    FProjection: TdxMapControlCustomProjection;
    FProjectionClass: TdxMapControlCustomProjectionClass;
    FInitialMapSize: TcxSize;
    FItemHint: string;
    FItemStyles: TdxMapItemStylesHelper;
    FItemTitleOptions: TdxMapItemTitleOptions;
    FOnGetItemHint: TdxCustomMapItemLayerGetItemHintEvent;
    function GetProjectionClassName: string;
    function GetProjectionClass: TdxMapControlCustomProjectionClass;
    procedure InitialMapSizeChanged(ASender: TObject);
    function IsInitialMapSizeStored: Boolean;
    procedure ItemStyleChanged(ASender: TObject);
    procedure ItemTitleOptionsChanged(ASender: TObject);
    procedure SetInitialMapSize(AValue: TcxSize);
    procedure SetItemTitleOptions(const Value: TdxMapItemTitleOptions);
    procedure SetMapItems(const Value: TdxMapItems);
    procedure SetProjection(const Value: TdxMapControlCustomProjection);
    procedure SetProjectionClassName(const Value: string);
    procedure SetProjectionClass(const Value: TdxMapControlCustomProjectionClass);
    function GetItemStyle(Index: TdxMapControlElementState): TdxMapItemStyle;
    procedure SetItemStyle(const Index: TdxMapControlElementState; const Value: TdxMapItemStyle);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function CreateViewInfo: TdxMapLayerViewInfo; override;
    procedure DoAssign(Source: TPersistent); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetMapProjection: TdxMapControlCustomProjection; override;
    function GetMapSizeInPixels(AZoomLevel: Double): TdxSizeDouble; override;
    procedure MapItemsChanged(Sender: TObject; AItem: TcxComponentCollectionItem;
      AAction: TcxComponentCollectionNotification); virtual;
    procedure ProjectionChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddItem(AMapItemClass: TdxMapItemClass): TdxMapItem;

    function GetItemHint(AItem: TdxMapItem): string;
    procedure GetItemsInRegion(const ARect: TRect; AItems: TList);
    function GetStyle(AState: TdxMapControlElementState): TdxMapItemStyle;

    property AllowHotTrack: Boolean read FAllowHotTrack write FAllowHotTrack default True;
    property InitialMapSize: TcxSize read FInitialMapSize write SetInitialMapSize stored IsInitialMapSizeStored;
    property ItemHint: string read FItemHint write FItemHint;
    property ItemStyle: TdxMapItemStyle index mcesNormal read GetItemStyle write SetItemStyle;
    property ItemStyleHot: TdxMapItemStyle index mcesHot read GetItemStyle write SetItemStyle;
    property ItemStyleSelected: TdxMapItemStyle index mcesSelected read GetItemStyle write SetItemStyle;
    property MapItems: TdxMapItems read FMapItems write SetMapItems;
    property ItemTitleOptions: TdxMapItemTitleOptions read FItemTitleOptions write SetItemTitleOptions;
    property ProjectionClass: TdxMapControlCustomProjectionClass read GetProjectionClass write SetProjectionClass;
    property OnGetItemHint: TdxCustomMapItemLayerGetItemHintEvent read FOnGetItemHint write FOnGetItemHint;
  published
    property ProjectionClassName: string read GetProjectionClassName write SetProjectionClassName;
    property Projection: TdxMapControlCustomProjection read FProjection write SetProjection;
  end;

  TdxCustomMapItemLayerList = class(TList<TdxCustomMapItemLayer>);

implementation

uses
  dxMapControl;

type
  TdxMapItemStyleAccess = class(TdxMapItemStyle);
  TdxMapItemsAccess = class(TdxMapItems);

{ TdxMapControlItemsLayerViewInfo }

constructor TdxMapItemLayerViewInfo.Create(AOwner: TdxMapLayer);
begin
  inherited Create(AOwner);
  FSelectedElements := TdxFastObjectList.Create(False);
  FSelectedPointers := TdxFastObjectList.Create(False);
  FSelectedTitles := TdxFastObjectList.Create(False);
  FItemTitles := TdxFastObjectList.Create(False);
end;

destructor TdxMapItemLayerViewInfo.Destroy;
begin
  FreeAndNil(FItemTitles);
  FreeAndNil(FSelectedTitles);
  FreeAndNil(FSelectedPointers);
  FreeAndNil(FSelectedElements);
  inherited;
end;

procedure TdxMapItemLayerViewInfo.CalculateSize;
begin
// do nothing
end;

procedure TdxMapItemLayerViewInfo.Paint(ACanvas: TcxCanvas);
var
  I: Integer;
  AItemHot, APointerHot, ATitleHot: TdxMapControlElementViewInfo;
  ATitle: TdxMapItemTextViewInfo;
begin
  FSelectedElements.Clear;
  FSelectedPointers.Clear;
  FSelectedTitles.Clear;
  AItemHot := nil;
  ATitleHot := nil;
  APointerHot := nil;
  for I := 0 to FLastNonPointerItemIndex do
  begin
    if Items[I].State = mcesSelected then
      FSelectedElements.Add(Items[I])
    else
      if Items[I].State = mcesHot then
        AItemHot := Items[I]
      else
        Items[I].Paint(ACanvas);
  end;
  for I := 0 to FSelectedElements.Count - 1 do
    (FSelectedElements[I] as TdxMapControlElementViewInfo).Paint(ACanvas);
  if AItemHot <> nil then
    AItemHot.Paint(ACanvas);

  for I := 0 to FItemTitles.Count - 1 do
  begin
    ATitle := FItemTitles[I] as TdxMapItemTextViewInfo;
    if ATitle.State = mcesSelected then
      FSelectedTitles.Add(ATitle)
    else
      if ATitle.State = mcesHot then
        ATitleHot := ATitle
      else
        ATitle.Paint(ACanvas);
  end;
  for I := 0 to FSelectedTitles.Count - 1 do
    (FSelectedTitles[I] as TdxMapItemTextViewInfo).Paint(ACanvas);
  if ATitleHot <> nil then
    ATitleHot.Paint(ACanvas);

  for I := FLastNonPointerItemIndex + 1 to Count - 1 do
  begin
    if Items[I].State = mcesSelected then
      FSelectedPointers.Add(Items[I])
    else
      if Items[I].State = mcesHot then
        APointerHot := Items[I]
      else
        Items[I].Paint(ACanvas);
  end;
  for I := 0 to FSelectedPointers.Count - 1 do
    (FSelectedPointers[I] as TdxMapControlElementViewInfo).Paint(ACanvas);
  if APointerHot <> nil then
    APointerHot.Paint(ACanvas);
end;

procedure TdxMapItemLayerViewInfo.AddVisibleElements;
var
  I: Integer;
  ALayer: TdxCustomMapItemLayer;
  AMapItemViewInfo: TdxMapItemViewInfo;
begin
  ALayer := Layer as TdxCustomMapItemLayer;
  FLastNonPointerItemIndex := -1;
  FItemTitles.Clear;
  for I := 0 to ALayer.MapItems.Count - 1 do
  begin
    AMapItemViewInfo := ALayer.MapItems[I].ViewInfo;
    if AMapItemViewInfo.CanBeVisible then
    begin
      if ALayer.MapItems[I].IsPointer then
        Add(AMapItemViewInfo)
      else
      begin
        Inc(FLastNonPointerItemIndex);
        Insert(FLastNonPointerItemIndex , AMapItemViewInfo);
      end;
      if AMapItemViewInfo.HasTitle then
        FItemTitles.Add(AMapItemViewInfo.Title);
    end;
  end;
end;

procedure TdxMapItemLayerViewInfo.ClearCache;
var
  ALayer: TdxCustomMapItemLayer;
  I: Integer;
begin
  ALayer := Layer as TdxCustomMapItemLayer;
  for I := 0 to ALayer.MapItems.Count - 1 do
    ALayer.MapItems[I].ViewInfo.ClearCache;
end;

{ TdxCustomMapItemLayer }

constructor TdxCustomMapItemLayer.Create;
begin
  inherited;
  FAllowHotTrack := True;
  FMapItems := TdxMapItems.Create(Self, TdxMapItem);
  FMapItems.OnChange := MapItemsChanged;
  FInitialMapSize := TcxSize.Create(Self, dxMapControlDefaultMapSize, dxMapControlDefaultMapSize);
  FInitialMapSize.OnChange := InitialMapSizeChanged;
  FItemStyles := TdxMapItemStylesHelper.Create(Self);
  FItemStyles.OnChanged := ItemStyleChanged;
  FItemTitleOptions := TdxMapItemTitleOptions.Create(Self);
  FItemTitleOptions.OnChanged := ItemTitleOptionsChanged;
  ProjectionClass := TdxMapControlSphericalMercatorProjection;
end;

destructor TdxCustomMapItemLayer.Destroy;
begin
  FreeAndNil(FProjection);
  FreeAndNil(FItemTitleOptions);
  FreeAndNil(FItemStyles);
  FreeAndNil(FInitialMapSize);
  FreeAndNil(FMapItems);
  inherited;
end;

procedure TdxCustomMapItemLayer.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxCustomMapItemLayer then
  begin
    MapItems := TdxCustomMapItemLayer(Source).MapItems;
    AllowHotTrack := TdxCustomMapItemLayer(Source).AllowHotTrack;
    InitialMapSize := TdxCustomMapItemLayer(Source).InitialMapSize;
    ItemStyle := TdxCustomMapItemLayer(Source).ItemStyle;
    ItemStyleHot := TdxCustomMapItemLayer(Source).ItemStyleHot;
    ItemStyleSelected := TdxCustomMapItemLayer(Source).ItemStyleSelected;
    ItemTitleOptions := TdxCustomMapItemLayer(Source).ItemTitleOptions;
    ProjectionClassName := TdxCustomMapItemLayer(Source).ProjectionClassName;
    Projection := TdxCustomMapItemLayer(Source).Projection;
  end;
end;

function TdxCustomMapItemLayer.AddItem(AMapItemClass: TdxMapItemClass): TdxMapItem;
begin
  Result := FMapItems.Add(AMapItemClass);
end;

function TdxCustomMapItemLayer.GetItemHint(AItem: TdxMapItem): string;
begin
  Result := ItemHint;
  if Assigned(FOnGetItemHint) then
    FOnGetItemHint(Self, AItem, Result);
end;

procedure TdxCustomMapItemLayer.GetItemsInRegion(const ARect: TRect; AItems: TList);
var
  I: Integer;
begin
  for I := 0 to ViewInfo.Count - 1 do
  begin
    if (ViewInfo[I] as TdxMapItemViewInfo).IsIntersect(ARect) then
      AItems.Add(TdxMapItemViewInfo(ViewInfo[I]).Item);
  end;
end;

function TdxCustomMapItemLayer.GetItemStyle(
  Index: TdxMapControlElementState): TdxMapItemStyle;
begin
  Result := GetStyle(Index);
end;

function TdxCustomMapItemLayer.GetStyle(AState: TdxMapControlElementState): TdxMapItemStyle;
begin
  Result := FItemStyles[AState];
end;

procedure TdxCustomMapItemLayer.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  InitialMapSize.ChangeScale(M, D);
  TdxMapItemsAccess(MapItems).ChangeScale(M, D);
end;

function TdxCustomMapItemLayer.CreateViewInfo: TdxMapLayerViewInfo;
begin
  Result := TdxMapItemLayerViewInfo.Create(Self);
end;

procedure TdxCustomMapItemLayer.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to MapItems.Count - 1 do
    if MapItems[I].Owner = Root then Proc(MapItems[I]);
end;

function TdxCustomMapItemLayer.GetMapProjection: TdxMapControlCustomProjection;
begin
  Result := FProjection;
end;

function TdxCustomMapItemLayer.GetMapSizeInPixels(
  AZoomLevel: Double): TdxSizeDouble;
var
  ACoeff, ALevel: Double;
begin
  if AZoomLevel < 1.0 then
    Result := dxSizeDouble(AZoomLevel * InitialMapSize.Width, AZoomLevel * InitialMapSize.Height)
  else
  begin
    ALevel := Max(0.0, AZoomLevel - 1.0);
    ACoeff := Power(2.0, ALevel);
    Result := dxSizeDouble(ACoeff * InitialMapSize.Width, ACoeff * InitialMapSize.Height);
  end;
end;

procedure TdxCustomMapItemLayer.MapItemsChanged(Sender: TObject;
  AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification);
begin
  Changed(False);
end;

procedure TdxCustomMapItemLayer.ProjectionChanged(Sender: TObject);
begin
  ViewInfo.ClearCache;
  InvalidateViewPort;
  Changed(False);
end;

function TdxCustomMapItemLayer.GetProjectionClass: TdxMapControlCustomProjectionClass;
begin
  Result := FProjectionClass;
end;

function TdxCustomMapItemLayer.GetProjectionClassName: string;
begin
  if FProjection <> nil then
    Result := FProjection.ClassName
  else
    Result := '';
end;

procedure TdxCustomMapItemLayer.InitialMapSizeChanged(ASender: TObject);
begin
  InvalidateViewPort;
  Changed(False);
end;

function TdxCustomMapItemLayer.IsInitialMapSizeStored: Boolean;
begin
  Result := not FInitialMapSize.IsEqual(cxSize(dxMapControlDefaultMapSize, dxMapControlDefaultMapSize));
end;

procedure TdxCustomMapItemLayer.ItemStyleChanged(ASender: TObject);
var
  I: Integer;
begin
  for I := 0 to MapItems.Count - 1 do
    MapItems[I].InvalidateStylesInfo;
  Changed(False);
end;

procedure TdxCustomMapItemLayer.ItemTitleOptionsChanged(ASender: TObject);
var
  I: Integer;
begin
  for I := 0 to MapItems.Count - 1 do
    MapItems[I].InvalidateTitleInfo;
  Changed(False);
end;

procedure TdxCustomMapItemLayer.SetInitialMapSize(AValue: TcxSize);
begin
  FInitialMapSize.Assign(AValue);
end;

procedure TdxCustomMapItemLayer.SetItemStyle(const Index: TdxMapControlElementState;
  const Value: TdxMapItemStyle);
begin
  FItemStyles[Index].Assign(Value);
end;

procedure TdxCustomMapItemLayer.SetItemTitleOptions(
  const Value: TdxMapItemTitleOptions);
begin
  FItemTitleOptions.Assign(Value);
end;

procedure TdxCustomMapItemLayer.SetMapItems(
  const Value: TdxMapItems);
begin
  FMapItems.Assign(Value);
end;

procedure TdxCustomMapItemLayer.SetProjection(
  const Value: TdxMapControlCustomProjection);
begin
  FProjection.Assign(Value);
end;

procedure TdxCustomMapItemLayer.SetProjectionClass(
  const Value: TdxMapControlCustomProjectionClass);
begin
  if FProjectionClass <> Value then
  begin
    FreeAndNil(FProjection);
    FProjectionClass := Value;
    if FProjectionClass <> nil then
    begin
      FProjection := FProjectionClass.Create(nil);
      FProjection.OnChanged := ProjectionChanged;
    end;
    ProjectionChanged(FProjection);
  end;
end;

procedure TdxCustomMapItemLayer.SetProjectionClassName(const Value: string);
begin
  ProjectionClass := TdxMapControlCustomProjectionClass(dxRegisteredMapProjections.FindByClassName(Value));
end;

end.
