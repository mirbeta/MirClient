{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxGridLayoutViewCarouselMode;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Classes, Windows, Controls, Forms, cxGeometry, cxGraphics, dxGDIPlusClasses,
  cxGridCustomView, cxGridCustomTableView, cxGridCustomLayoutView, cxGridLayoutView,
  dxAnimation;

type
  TcxGridLayoutViewCarouselModeRecordViewInfo = class;
  TcxGridLayoutViewCarouselModeRecordsViewInfo = class;
  TcxGridLayoutViewControllerCarouselHelper = class;
  TcxGridLayoutViewInfoCarouselModeCalculator = class;

  { TcxGridLayoutViewCarouselModeRecordPainter }

  TcxGridLayoutViewCarouselModeRecordPainter = class(TcxGridLayoutViewRecordPainter)
  private
    function GetViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo;
    function IsPaintBitmap: Boolean;
  protected
    procedure Paint; override;
    function CanDrawBackground: Boolean; override;
    function CanDrawExpandButton: Boolean; override;

    property ViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo read GetViewInfo;
  end;

  { TcxGridLayoutViewCarouselModeRecordsPainter }

  TcxGridLayoutViewCarouselModeRecordsPainter = class(TcxCustomGridRecordsPainter)
  private
    function GetViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo;
  protected
    procedure Paint; override;

    property ViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo read GetViewInfo;
  end;

  { TcxGridLayoutViewCarouselModeRecordViewInfoCacheItem }

  TcxGridLayoutViewCarouselModeRecordViewInfoCacheItem = class(TcxGridLayoutViewRecordViewInfoCacheItem)
  private
    FBitmap: TdxGPImage;
    FSelected: Boolean;
    FIsSelectedAssigned: Boolean;
    procedure SetBitmap(const Value: TdxGPImage);
    procedure SetSelected(const Value: Boolean);
  public
    constructor Create(AOwner: TcxCustomGridViewInfoCache; AIndex: Integer); override;
    destructor Destroy; override;

    procedure UnassignValues(AKeepMaster: Boolean); override;

    property Bitmap: TdxGPImage read FBitmap write SetBitmap;
    property Selected: Boolean read FSelected write SetSelected;
    property IsSelectedAssigned: Boolean read FIsSelectedAssigned;
  end;

  { TcxGridLayoutViewCarouselModeRecordViewInfo }

  TcxGridLayoutViewCarouselModeRecordViewInfo = class(TcxGridLayoutViewRecordViewInfo)
  private
    FAlpha: Integer;
    FAngle: Double;
    FCalculatedBounds: TRect;
    FIsInternalPainting: Boolean;
    function GetCacheItem: TcxGridLayoutViewCarouselModeRecordViewInfoCacheItem;
    function GetCachedBitmap: TdxGPImage;
    function GetCachedSelected: Boolean;
    function GetCarouselIndex: Integer;
    function GetRecordsViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo;
    procedure SetCachedSelected(const Value: Boolean);
  protected
    function GetBackgroundBounds: TRect; override;
    function GetEditViewDataBounds(AItem: TcxGridLayoutViewItemViewInfo): TRect; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetPixelScrollSize: Integer; override;
    function GetVisible: Boolean; override;

    function CanGetChildrenHitTest: Boolean; override;
    function CanShowGroupScrollBars: Boolean; override;
    procedure CheckCachedBitmap;
    procedure DrawCachedBitmap;
    function HasItemFocusRect(AItem: TcxGridLayoutViewItemViewInfo): Boolean; override;
    function IsExpanded: Boolean; override;
    function IsGridViewInfoRightToLeftConverted: Boolean; override;
    function IsTopRecord: Boolean;

    property Angle: Double read FAngle write FAngle;
    property Alpha: Integer read FAlpha write FAlpha;
    property CacheItem: TcxGridLayoutViewCarouselModeRecordViewInfoCacheItem read GetCacheItem;
    property CalculatedBounds: TRect read FCalculatedBounds write FCalculatedBounds;
    property CachedBitmap: TdxGPImage read GetCachedBitmap;
    property CachedSelected: Boolean read GetCachedSelected write SetCachedSelected;
    property CarouselIndex: Integer read GetCarouselIndex;
    property IsInternalPainting: Boolean read FIsInternalPainting;
  public
    function HasPoint(const P: TPoint): Boolean; override;

    property RecordsViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo read GetRecordsViewInfo;
  end;

  { TcxGridLayoutViewCarouselModeRecordsViewInfo }

  TcxTouchPointData = packed record
    X, Y, Z: Double;
    ASignZ: Integer;
  end;

  TcxGridLayoutViewCarouselModeRecordsViewInfo = class(TcxGridLayoutViewRecordsViewInfo)
  private
    FIsRadiusCalculating: Boolean;
    FIsPitchAngleCalculated: Boolean;
    FOffsetAngle: Double;
    FPitchAngle: Extended;
    FRadius: Double;
    function GetAdjustedContentSize: TSize;
    function GetRotatedContentSize: TSize;
    function GetCalculator: TcxGridLayoutViewInfoCarouselModeCalculator;
    function GetController: TcxGridLayoutViewController;
    function GetControllerHelper: TcxGridLayoutViewControllerCarouselHelper; inline;
    function GetItem(Index: Integer): TcxGridLayoutViewCarouselModeRecordViewInfo;
    function GetOptions: TcxGridLayoutViewCarouselMode;
    function GetPitchAngle: Extended;
    function GetRollAngle: Extended;
    function PointToTouchPointData(const APoint: TPoint; ASignZ: Integer): TcxTouchPointData;
    procedure SetOffsetAngle(const Value: Double);
  protected
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; override;
    procedure CalculateVisibleCount; override;
    procedure CalculateVisibleRecordsBound(out AFirstRecordIndex, ALastRecordIndex: Integer); override;
    function GetCalculatorClass: TcxGridCustomLayoutRecordsViewInfoBasedCalculatorClass; override;
    function GetCarouselViewInfos: TList;
    function GetItemViewInfoClass: TcxGridCustomLayoutRecordViewInfoClass; override;
    function GetContentCenter: TPoint;
    function GetPainterClass: TcxCustomGridRecordsPainterClass; override;
    function GetRecordSize: TSize;
    function GetRecordViewInfoByRecordIndex(ARecordIndex: Integer): TcxGridLayoutViewCarouselModeRecordViewInfo;
    function GetScrollableAreaBoundsForEdit: TRect; override;
    procedure InternalSetOffsetAngle(const Value: Double);
    function IsAutoPitchAngle: Boolean;
    function IsCarouselMode: Boolean; override;
    function IsRecordVisible(ARecord: TcxGridLayoutViewRecord): Boolean;

    function CalculateAnimationInterval(Angle: Double): Cardinal;
    procedure CalculateRadius;
    function CalculateTouchScrollingAngle(const APrevPoint, APoint: TcxTouchPointData): Double;
    procedure EndTouchScrolling;
    procedure Recalculate;

    function CreateGPCanvas(ACanvas: TcxCanvas): TdxGPCanvas;

    property Calculator: TcxGridLayoutViewInfoCarouselModeCalculator read GetCalculator;
    property Controller: TcxGridLayoutViewController read GetController;
    property ControllerHelper: TcxGridLayoutViewControllerCarouselHelper read GetControllerHelper;
    property IsRadiusCalculating: Boolean read FIsRadiusCalculating;
    property Items[Index: Integer]: TcxGridLayoutViewCarouselModeRecordViewInfo read GetItem; default;
    property OffsetAngle: Double read FOffsetAngle write SetOffsetAngle;
    property Options: TcxGridLayoutViewCarouselMode read GetOptions;
    property PitchAngle: Extended read GetPitchAngle;
    property Radius: Double read FRadius;
    property RollAngle: Extended read GetRollAngle;
  public
    function CanOffset(AItemCountDelta: Integer): Boolean; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    procedure OffsetRecords(AItemCountDelta, APixelScrollRecordOffsetDelta: Integer); override;
  end;

  { TcxGridLayoutViewInfoCarouselModeCalculator }

  TcxGridLayoutViewInfoCarouselModeCalculator = class(TcxGridLayoutViewInfoHorizontalCalculator)
  private
    function CalculateAlpha(AViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo): Integer;
    function CalculateAngle(AViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo): Double;
    function CalculateRecordBounds(const AContentBounds: TRect; AViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo): TRect;
    function CalculateScale(AViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo): Double;
    function GetOptions: TcxGridLayoutViewCarouselMode;
    function GetInterpolatedValue(AViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo;
      ABeginValue, AEndValue: Double; AUseRecordScale: Boolean): Double;
    function GetRecordsViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo;
  protected
    procedure BeforeCalculate; override;
    procedure DoCalculate(const AContentBounds: TRect); override;

    property Options: TcxGridLayoutViewCarouselMode read GetOptions;
  public
    property RecordsViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo read GetRecordsViewInfo;
  end;

  { TcxGridCarouselModeAnimationTransition }

  TcxGridCarouselModeAnimationTransition = class(TdxAnimationTransition)
  private
    FAngle: Double;
    FIsStop: Boolean;
    FControllerHelper: TcxGridLayoutViewControllerCarouselHelper;
    FScale: Double;
    FStartAngle: Double;
    function GetRecordsViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo;
  protected
    procedure DoAnimate; override;
    function NeedSkipCards: Boolean;
    function PositionToAngle(APosition: Integer): Double; virtual;

    property Angle: Double read FAngle;
    property ControllerHelper: TcxGridLayoutViewControllerCarouselHelper read FControllerHelper;
    property IsStop: Boolean read FIsStop write FIsStop;
    property RecordsViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo read GetRecordsViewInfo;
    property Scale: Double read FScale;
    property StartAngle: Double read FStartAngle write FStartAngle;
  public
    constructor Create(AControllerHelper: TcxGridLayoutViewControllerCarouselHelper; Angle: Double;
      ATime: Cardinal; AEffect: TdxAnimationTransitionEffect); reintroduce; virtual;
    destructor Destroy; override;
  end;

  { TcxGridLayoutViewControllerCarouselHelper }

  TcxKineticDataPoint = packed record
    Angle: Double;
    Time: Cardinal;
  end;

  TcxKineticDataPoints = array of TcxKineticDataPoint;

  TcxGridLayoutViewControllerCarouselHelper = class(TcxGridLayoutViewControllerHorizontalHelper)
  private
    FAnimationTransition: TcxGridCarouselModeAnimationTransition;
    FIsInternalTopRecordIndexChanging: Boolean;
    FKineticDataPoints: TcxKineticDataPoints;
    FPrevStartRecordIndex: Integer;
    FTouchPoint: TcxTouchPointData;
    procedure AddKineticDataPoint(Angle: Double);
    function GetInternalTopRecordIndex: Integer;
    function GetRecordsViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo;
    procedure SetInternalTopRecordIndex(Value: Integer);
    procedure SetTouchPoint(const Value: TcxTouchPointData);
  protected
    function GetPageRecordCount(AVisibleRecordCount: Integer): Integer; override;
    function IsForegroundRecord(AIndex: Integer): Boolean;
    procedure UpdatePrevStartRecordIndex;

    procedure BeginTouchScrolling(Shift: TShiftState; const P: TPoint);
    function IsTouchScrolling(Shift: TShiftState): Boolean;
    procedure EndTouchScrolling;
    procedure TouchScrolling(const P: TPoint);

    procedure StartAnimation(AEffect: TdxAnimationTransitionEffect; Angle: Double; ATime: Integer);
    procedure StartCarouselAnimation(AItemCountDelta: Integer);
    procedure StartKinematicAnimation;
    procedure StopAnimation;

    property AnimationTransition: TcxGridCarouselModeAnimationTransition read FAnimationTransition;
    property InternalTopRecordIndex: Integer read GetInternalTopRecordIndex write SetInternalTopRecordIndex;
    property IsInternalTopRecordIndexChanging: Boolean read FIsInternalTopRecordIndexChanging;
    property PrevStartRecordIndex: Integer read FPrevStartRecordIndex write FPrevStartRecordIndex;
    property RecordsViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo read GetRecordsViewInfo;
    property TouchPoint: TcxTouchPointData read FTouchPoint write SetTouchPoint;
    property KineticDataPoints: TcxKineticDataPoints read FKineticDataPoints;
  public
    constructor Create(AController: TcxGridCustomLayoutViewController); override;
    destructor Destroy; override;
    function GetScrollDelta: Integer; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

implementation

uses
  Math, dxCoreClasses, dxGDIPlusAPI, dxTouch, cxControls;

const
  dxHalfOfPi = Pi / 2;
  cxOneAndHalfOfPi = 1.5 * Pi;
  cxInvalidTouchPointData: TcxTouchPointData = (X: -1; Y: -1; Z: -1; ASignZ: 0);
  cxAnimationLength = 1000;

type
  TcxCustomGridTablePainterAccess = class(TcxCustomGridTablePainter);
  TcxGridLayoutViewControllerAccess = class(TcxGridLayoutViewController);


function cxTouchPointDataEqual(const P1, P2: TcxTouchPointData): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y) and (P1.Z = P2.Z);
end;

function CheckValueOnZero(const Value: Extended): Boolean;
begin
  Result := Abs(Value) < 0.0000001;
end;

function TransformAngle(Angle: Double): Double;
begin
  if (Angle >= cxOneAndHalfOfPi) or (Angle <= -dxHalfOfPi) then
    Result := 1.0
  else
    Result := Abs(Angle - dxHalfOfPi) / Pi;
end;

function GetInterpolatedLineValue(ABeginValue, AEndValue, Angle: Double): Double;
begin
  Result := ABeginValue + TransformAngle(Angle) *
    (AEndValue - ABeginValue);
end;

function GetInterpolatedCubicValue(ABeginValue, AEndValue, Angle: Double): Double;
begin
  Result := ABeginValue + Sqrt(Sqrt(TransformAngle(Angle))) *
    (AEndValue - ABeginValue);
end;

function GetInternalInterpolatedValue(ABeginValue, AEndValue, ABoundAngle, Angle: Double;
  AUseCubicInterpolatedFunction: Boolean): Double;
var
  ACalculatedAngle: Double;
begin
  if (Angle >= cxOneAndHalfOfPi) or (Angle <= -dxHalfOfPi) then
    Result := AEndValue
  else
  begin
    if (Abs(Angle - dxHalfOfPi) < ABoundAngle) then
    begin
      ACalculatedAngle := dxHalfOfPi;
      if Angle < dxHalfOfPi then
        ABoundAngle := -ABoundAngle;
      ACalculatedAngle := ACalculatedAngle + ABoundAngle;
    end
    else
      ACalculatedAngle := Angle;
    if AUseCubicInterpolatedFunction then
      Result := GetInterpolatedCubicValue(ABeginValue, AEndValue, ACalculatedAngle)
    else
      Result := GetInterpolatedLineValue(ABeginValue, AEndValue, ACalculatedAngle);
  end;
end;

function cxGridLayoutViewCompareByAngle(AItem1, AItem2: Pointer): Integer;
var
  Angle1, Angle2: Double;
begin
  Angle1 := RadToDeg(TcxGridLayoutViewCarouselModeRecordViewInfo(AItem1).Angle - dxHalfOfPi);
  Angle2 := RadToDeg(TcxGridLayoutViewCarouselModeRecordViewInfo(AItem2).Angle - dxHalfOfPi);

  Result := Abs(Trunc(Angle2)) - Abs(Trunc(Angle1));
end;

function cxGridLayoutViewCompareByCarouselIndex(AItem1,
  AItem2: Pointer): Integer;
begin
  Result := Abs(TcxGridLayoutViewCarouselModeRecordViewInfo(AItem2).CarouselIndex) -
    Abs(TcxGridLayoutViewCarouselModeRecordViewInfo(AItem1).CarouselIndex);
end;

{ TcxGridLayoutViewCarouselModeRecordPainter }

procedure TcxGridLayoutViewCarouselModeRecordPainter.Paint;
begin
  if IsPaintBitmap then
    ViewInfo.DrawCachedBitmap
  else
    inherited Paint;
end;

function TcxGridLayoutViewCarouselModeRecordPainter.CanDrawBackground: Boolean;
begin
  Result := not IsPaintBitmap;
end;

function TcxGridLayoutViewCarouselModeRecordPainter.CanDrawExpandButton: Boolean;
begin
  Result := not IsPaintBitmap;
end;

function TcxGridLayoutViewCarouselModeRecordPainter.GetViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo;
begin
  Result := TcxGridLayoutViewCarouselModeRecordViewInfo(inherited ViewInfo);
end;

function TcxGridLayoutViewCarouselModeRecordPainter.IsPaintBitmap: Boolean;
begin
  Result := not ViewInfo.IsInternalPainting and
    (not ViewInfo.IsTopRecord or (ViewInfo.RecordsViewInfo.OffsetAngle <> 0));
end;

{ TcxGridLayoutViewCarouselModeRecordsPainter }

procedure TcxGridLayoutViewCarouselModeRecordsPainter.Paint;
var
  I: Integer;
  AList: TList;
begin
  TcxCustomGridTablePainterAccess(ViewInfo.GridView.Painter).DrawBackground;
  AList := ViewInfo.GetCarouselViewInfos;
  try
    for I := 0 to AList.Count - 1 do
    begin
      TcxGridLayoutViewCarouselModeRecordViewInfo(AList[I]).CheckCachedBitmap;
      TcxGridLayoutViewCarouselModeRecordViewInfo(AList[I]).Paint(Canvas);
    end;
  finally
    AList.Free;
  end;
end;

function TcxGridLayoutViewCarouselModeRecordsPainter.GetViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo;
begin
  Result := TcxGridLayoutViewCarouselModeRecordsViewInfo(inherited ViewInfo);
end;

{ TcxGridLayoutViewCarouselModeRecordViewInfoCacheItem }

constructor TcxGridLayoutViewCarouselModeRecordViewInfoCacheItem.Create(AOwner: TcxCustomGridViewInfoCache; AIndex: Integer);
begin
  inherited Create(AOwner, AIndex);
  FBitmap := TdxGPImage.Create;
end;

destructor TcxGridLayoutViewCarouselModeRecordViewInfoCacheItem.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TcxGridLayoutViewCarouselModeRecordViewInfoCacheItem.UnassignValues(AKeepMaster: Boolean);
begin
  inherited UnassignValues(AKeepMaster);
  FBitmap.Clear;
end;

procedure TcxGridLayoutViewCarouselModeRecordViewInfoCacheItem.SetBitmap(
  const Value: TdxGPImage);
begin
  FBitmap.Assign(Value);
end;

procedure TcxGridLayoutViewCarouselModeRecordViewInfoCacheItem.SetSelected(
  const Value: Boolean);
begin
  FSelected := Value;
  FIsSelectedAssigned := True;
end;

{ TcxGridLayoutViewCarouselModeRecordViewInfo }

function TcxGridLayoutViewCarouselModeRecordViewInfo.HasPoint(const P: TPoint): Boolean;
begin
  Result := PtInRect(CalculatedBounds, P);
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.GetBackgroundBounds: TRect;
begin
  Result := CalculatedBounds;
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.GetEditViewDataBounds(AItem: TcxGridLayoutViewItemViewInfo): TRect;
begin
  if not AItem.Editing or (IsTopRecord and (RecordsViewInfo.OffsetAngle = 0)) then
    Result := inherited GetEditViewDataBounds(AItem)
  else
    Result := cxInvalidRect;
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridLayoutViewCarouselModeRecordPainter;
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.GetPixelScrollSize: Integer;
begin
  Result := Trunc(RecordsViewInfo.Options.DeltaAngle * RecordsViewInfo.Radius);
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.GetVisible: Boolean;
begin
  Result := RecordsViewInfo.IsRecordVisible(GridRecord);
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.CanGetChildrenHitTest: Boolean;
begin
  Result := IsTopRecord and (RecordsViewInfo.OffsetAngle = 0);
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.CanShowGroupScrollBars: Boolean;
begin
  Result := inherited CanShowGroupScrollBars and IsTopRecord and (RecordsViewInfo.OffsetAngle = 0);
end;

procedure TcxGridLayoutViewCarouselModeRecordViewInfo.CheckCachedBitmap;
var
  ABitmap: TcxBitmap;
begin
  if not IsVisibleForPainting or (not CachedBitmap.Empty and (Selected = CachedSelected) and
      cxRectIsEqual(CachedBitmap.ClientRect, cxRectOffset(Bounds, Bounds.TopLeft, False))) then
    Exit;
  FIsInternalPainting := True;
  try
    ABitmap := TcxBitmap.CreateSize(Bounds);
    try
      ABitmap.cxCanvas.UseRightToLeftAlignment := Canvas.UseRightToLeftAlignment;
      ABitmap.Canvas.TextFlags := Canvas.Canvas.TextFlags;
      ABitmap.cxCanvas.WindowOrg := Bounds.TopLeft;
      try
        Paint(ABitmap.cxCanvas);
      finally
        ABitmap.cxCanvas.WindowOrg := cxNullPoint;
      end;
      CachedBitmap.Assign(ABitmap);
    finally
      ABitmap.Free;
    end;
    CachedSelected := Selected;
  finally
    FIsInternalPainting := False;
  end;
end;

procedure TcxGridLayoutViewCarouselModeRecordViewInfo.DrawCachedBitmap;
var
  ACanvas: TdxGPCanvas;
begin
  ACanvas := RecordsViewInfo.CreateGPCanvas(Canvas);
  try
    ACanvas.Draw(CachedBitmap, CalculatedBounds, CachedBitmap.ClientRect, Alpha);
  finally
    ACanvas.Free;
  end;
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.HasItemFocusRect(AItem: TcxGridLayoutViewItemViewInfo): Boolean;
begin
  Result := IsTopRecord and (RecordsViewInfo.OffsetAngle = 0);
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.IsExpanded: Boolean;
begin
  Result := inherited IsExpanded or RecordsViewInfo.IsRadiusCalculating;
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.IsTopRecord: Boolean;
begin
  Result := CarouselIndex = 0;
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.GetCacheItem: TcxGridLayoutViewCarouselModeRecordViewInfoCacheItem;
begin
  Result := TcxGridLayoutViewCarouselModeRecordViewInfoCacheItem(inherited CacheItem);
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.GetCachedBitmap: TdxGPImage;
begin
  Result := CacheItem.Bitmap;
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.GetCachedSelected: Boolean;
begin
  Result := CacheItem.Selected;
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.GetCarouselIndex: Integer;
begin
  Result := GridRecord.Index - RecordsViewInfo.TopRecordIndex;
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.GetRecordsViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo;
begin
  Result := TcxGridLayoutViewCarouselModeRecordsViewInfo(inherited RecordsViewInfo);
end;

function TcxGridLayoutViewCarouselModeRecordViewInfo.IsGridViewInfoRightToLeftConverted: Boolean;
begin
  Result := UseRightToLeftAlignment;
end;

procedure TcxGridLayoutViewCarouselModeRecordViewInfo.SetCachedSelected(
  const Value: Boolean);
begin
  CacheItem.Selected := Value;
end;

{ TcxGridLayoutViewCarouselModeRecordsViewInfo }

function TcxGridLayoutViewCarouselModeRecordsViewInfo.CanOffset(AItemCountDelta: Integer): Boolean;
begin
  Result := True;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  AList: TList;
  I: Integer;
begin
  Result := nil;
  AList := GetCarouselViewInfos;
  try
    for I := AList.Count - 1 downto 0 do
    begin
      Result := TcxGridLayoutViewCarouselModeRecordViewInfo(AList[I]).GetHitTest(P);
      if Result <> nil then
        Break;
    end;
  finally
    AList.Free;
  end;
end;

procedure TcxGridLayoutViewCarouselModeRecordsViewInfo.OffsetRecords(AItemCountDelta, APixelScrollRecordOffsetDelta: Integer);
begin
  if (AItemCountDelta <> 0) and not ControllerHelper.IsInternalTopRecordIndexChanging then
  begin
    if AItemCountDelta < TopRecordIndex - ViewData.RecordCount then
      AItemCountDelta := TopRecordIndex - ViewData.RecordCount;
    if TopRecordIndex - AItemCountDelta < 0 then
      AItemCountDelta := TopRecordIndex;
    ControllerHelper.StartCarouselAnimation(AItemCountDelta);
  end;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := False;
end;

procedure TcxGridLayoutViewCarouselModeRecordsViewInfo.CalculateVisibleCount;
var
  AMin, AMax: Integer;
begin
  FVisibleCount := IfThen(MaxCount > 0, 1, 0);
  CalculateVisibleRecordsBound(AMin, AMax);
  FPartVisibleCount := AMax - AMin;
end;

procedure TcxGridLayoutViewCarouselModeRecordsViewInfo.CalculateVisibleRecordsBound(
  out AFirstRecordIndex, ALastRecordIndex: Integer);
var
  ACount: Integer;
  ATopRecordIndex: Integer;
begin
  ACount := Options.RecordCount;
  ATopRecordIndex := TopRecordIndex - Trunc(OffsetAngle / Options.DeltaAngle);
  AFirstRecordIndex := Max(0, ATopRecordIndex - (ACount - 1) div 2);
  ALastRecordIndex := Min(ViewData.RecordCount - 1, ATopRecordIndex + ACount -  1 - (ACount - 1) div 2);
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetCalculatorClass: TcxGridCustomLayoutRecordsViewInfoBasedCalculatorClass;
begin
  Result := TcxGridLayoutViewInfoCarouselModeCalculator;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetCarouselViewInfos: TList;
var
  I: Integer;
begin
  Result := TList.Create;
  for I := 0 to Count - 1 do
    if Items[I].Calculated and IsRecordVisible(Items[I].GridRecord) then
      Result.Add(Items[I]);
  Result.Sort(cxGridLayoutViewCompareByAngle);
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetItemViewInfoClass: TcxGridCustomLayoutRecordViewInfoClass;
begin
  Result := TcxGridLayoutViewCarouselModeRecordViewInfo;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetContentCenter: TPoint;
begin
  Result := cxRectCenter(ContentBounds);
  Inc(Result.Y, Max(0, (GetRecordSize.cy - cxRectHeight(ContentBounds))) div 2);
  Dec(Result.Y, TcxGridLayoutViewControllerAccess(Controller).ContentScrollBarPosition);
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetPainterClass: TcxCustomGridRecordsPainterClass;
begin
  Result := TcxGridLayoutViewCarouselModeRecordsPainter;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetRecordSize: TSize;
begin
  if (ViewData.RecordCount > 0) and (FirstRecordIndex >= 0) then
    with GetRecordViewInfoByRecordIndex(FirstRecordIndex) do
      Result := cxSize(CalculateWidth, CalculateHeight)
  else
    Result := cxNullSize;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetRecordViewInfoByRecordIndex(
  ARecordIndex: Integer): TcxGridLayoutViewCarouselModeRecordViewInfo;
var
  ARecord: TcxCustomGridRecord;
begin
  ARecord := ViewData.Records[ARecordIndex];
  Result := TcxGridLayoutViewCarouselModeRecordViewInfo(ARecord.ViewInfo);
  if Result = nil then
  begin
    Result := TcxGridLayoutViewCarouselModeRecordViewInfo(CreateRecordViewInfo(ARecord));
    AddRecordViewInfo(Result);
  end;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetScrollableAreaBoundsForEdit: TRect;
var
  I: Integer;
begin
  Result := inherited GetScrollableAreaBoundsForEdit;
  if OffsetAngle = 0 then
    for I := 0 to Count - 1 do
      if Items[I].IsTopRecord then
      begin
        Result := Items[I].Bounds;
        Break;
      end;
end;

procedure TcxGridLayoutViewCarouselModeRecordsViewInfo.InternalSetOffsetAngle(const Value: Double);
begin
  FOffsetAngle := Value;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.IsAutoPitchAngle: Boolean;
begin
  Result := Options.AutoPitchAngle;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.IsCarouselMode: Boolean;
begin
  Result := True;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.IsRecordVisible(ARecord: TcxGridLayoutViewRecord): Boolean;
var
  AMax, AMin: Integer;
begin
  CalculateVisibleRecordsBound(AMin, AMax);
  Result := (ARecord.Index <= AMax) and (ARecord.Index >= AMin);
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.CalculateAnimationInterval(Angle: Double): Cardinal;
begin
  Result := Options.AnimationInterval;
  if Abs(Angle) < Options.DeltaAngle then
    Result := Trunc(Result * Abs(Angle) / Options.DeltaAngle)
  else
    if Abs(Angle) > Options.DeltaAngle then
      Result := Result + Trunc((Abs(Angle) / Options.DeltaAngle - 1) * Options.AnimationInterval / 4);
  Result := Min(Result, 2 * Options.AnimationInterval);
end;

procedure TcxGridLayoutViewCarouselModeRecordsViewInfo.CalculateRadius;
var
  ASize: TSize;
  ARollAngle, APitchAngle: Double;
  AWidth, AHeight: Double;
begin
  if not GridViewInfo.CalculateDown then
    Exit;
  if MaxCount = 0 then
  begin
    FRadius := 0;
    Exit;
  end;
  FRadius := Options.Radius;
  if Radius > 0 then
    Exit;
  FIsRadiusCalculating := True;
  try
    ASize := GetAdjustedContentSize;
    AWidth := ASize.cx / 2;
    AHeight := ASize.cy / 2;

    ARollAngle := RollAngle;
    APitchAngle := PitchAngle;

    if Sin(ARollAngle) = 0 then
      FRadius := AWidth
    else
      if Cos(ARollAngle) = 0 then
        FRadius := AHeight
      else
        FRadius := Min(Abs(AWidth / Cos(ARollAngle)), Abs(AHeight / Sin(ARollAngle)));

    if Cos(APitchAngle) <> 0 then
      FRadius := Min(FRadius, Abs(Min(AHeight, AWidth) / Cos(APitchAngle)));
  finally
    FIsRadiusCalculating := False;
  end;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.CalculateTouchScrollingAngle(const APrevPoint, APoint: TcxTouchPointData): Double;

  function CalculateVectorLength(const P: TcxTouchPointData): Double;
  begin
    with P do
      Result := Sqrt(X * X + Y * Y + Z * Z);
  end;

var
  ASum, ALenA, ALenB: Double;
  ASign: Integer;
begin
  ASum := APrevPoint.X * APoint.X + APrevPoint.Y * APoint.Y + APrevPoint.Z * APoint.Z;
  ALenA := CalculateVectorLength(APrevPoint);
  ALenB := CalculateVectorLength(APoint);

  if CheckValueOnZero(Cos(PitchAngle)) then
    ASign := Sign(APoint.X - APrevPoint.X)
  else
    ASign := Sign(APrevPoint.X * APoint.Y - APrevPoint.Y * APoint.X);

  if ASum / (ALenA * ALenB) = 1 then
    Result := 0
  else
    Result := ArcCos(ASum / (ALenA * ALenB)) * ASign;
end;

procedure TcxGridLayoutViewCarouselModeRecordsViewInfo.EndTouchScrolling;
begin
  ControllerHelper.StartKinematicAnimation;
end;

procedure TcxGridLayoutViewCarouselModeRecordsViewInfo.Recalculate;
begin
  MainCalculate;
  GridView.ViewChanged;
  GridView.Site.Update;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.CreateGPCanvas(ACanvas: TcxCanvas): TdxGPCanvas;
begin
  Result := TdxGPCanvas.Create(ACanvas.Handle);
  Result.InterpolationMode := Options.InterpolationMode;
  Result.SmoothingMode := smAntiAlias;
end;

procedure TcxGridLayoutViewCarouselModeRecordsViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetAdjustedContentSize: TSize;
begin
  Result := GetRecordSize;
  Result.cx := Trunc(cxRectWidth(ContentBounds) - Result.cx - Options.BackgroundRecordEndScale / 100 * Result.cx);
  Result.cy := Trunc(cxRectHeight(ContentBounds) - Result.cy - Options.BackgroundRecordEndScale / 100 * Result.cy);
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetRotatedContentSize: TSize;
var
  ASize: TSize;
  ARollAngle: Extended;
begin
  ASize := GetAdjustedContentSize;
  ARollAngle := RollAngle;
  if ARollAngle > Pi then
    ARollAngle := ARollAngle - Pi;
  if ARollAngle > dxHalfOfPi then
    ARollAngle := Pi - ARollAngle;
  Result.cx := Trunc(ASize.cx * Cos(ARollAngle) + ASize.cy * Sin(ARollAngle));
  Result.cy := Trunc(ASize.cy * Cos(ARollAngle) - ASize.cx * Sin(ARollAngle));
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetCalculator: TcxGridLayoutViewInfoCarouselModeCalculator;
begin
  Result := TcxGridLayoutViewInfoCarouselModeCalculator(inherited Calculator);
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetController: TcxGridLayoutViewController;
begin
  Result := TcxGridLayoutViewController(inherited Controller);
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetControllerHelper: TcxGridLayoutViewControllerCarouselHelper;
begin
  Result := TcxGridLayoutViewControllerCarouselHelper(inherited ControllerHelper);
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetItem(Index: Integer): TcxGridLayoutViewCarouselModeRecordViewInfo;
begin
  Result := TcxGridLayoutViewCarouselModeRecordViewInfo(inherited Items[Index]);
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetOptions: TcxGridLayoutViewCarouselMode;
begin
  Result := GridView.OptionsView.CarouselMode;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetPitchAngle: Extended;
var
  ASize: TSize;
begin
  if not FIsPitchAngleCalculated then
  begin
    if IsAutoPitchAngle then
    begin
      ASize := GetRotatedContentSize;
      if Options.Radius > 0 then
      begin
        ASize.cx := Min(ASize.cx, Options.Radius * 2);
        ASize.cy := Min(ASize.cy, Options.Radius * 2);
      end;
      if ASize.cy = 0 then
        FPitchAngle := dxHalfOfPi
      else
        FPitchAngle := ArcTan(ASize.cx / ASize.cy);

      if Options.PitchAngle > 90 then
        FPitchAngle := Pi - FPitchAngle
    end
    else
      FPitchAngle := DegToRad(Options.PitchAngle);
    FIsPitchAngleCalculated := True;
  end;
  Result := FPitchAngle;
  if GridViewInfo.UseRightToLeftAlignment then
    Result := Pi - Result;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.GetRollAngle: Extended;
begin
  Result := DegToRad(Options.RollAngle);
  if GridViewInfo.UseRightToLeftAlignment then
    Result := Pi - Result;
end;

function TcxGridLayoutViewCarouselModeRecordsViewInfo.PointToTouchPointData(
  const APoint: TPoint; ASignZ: Integer): TcxTouchPointData;

  function GetTrackingCarouselIndex(out AIndex: Integer): Boolean;
  var
    AList: TList;
    AViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo;
    I: Integer;
    R: TRect;
  begin
    Result := False;
    AList := GetCarouselViewInfos;
    try
      for I := AList.Count - 1 downto 0 do
      begin
        AViewInfo := TcxGridLayoutViewCarouselModeRecordViewInfo(AList[I]);
        R := cxRectInflate(AViewInfo.CalculatedBounds, Mouse.DragThreshold, Mouse.DragThreshold);
        Result := PtInRect(R, APoint);
        if Result then
        begin
          AIndex := AViewInfo.CarouselIndex;
          Break;
        end;
      end;
    finally
      AList.Free;
    end;
  end;

var
  P: TPoint;
  Ax, Ay, Az: Double;
  AIsForegroundRecordTracking: Boolean;
  ATrackingCarouselIndex: Integer;
begin
  P := cxPointOffset(APoint, GetContentCenter, False);
  Ax := P.X * Cos(RollAngle) + P.Y * Sin(RollAngle);
  Ay := P.Y * Cos(RollAngle) - P.X * Sin(RollAngle);
  if Abs(Ax) > Radius then
    Az := 0
  else
    Az := Sqrt(Radius * Radius - Ax * Ax);

  if Az <> 0 then
  begin
    if (ASignZ = 0) or ((Radius - Abs(Ax)) < Radius / 5) then
    begin
      AIsForegroundRecordTracking := GetTrackingCarouselIndex(ATrackingCarouselIndex) and (Abs(ATrackingCarouselIndex) <= Options.RecordCount div 4);
      if (Cos(PitchAngle) > 0) <>
          ((AIsForegroundRecordTracking and (Cos(PitchAngle) < 0)) or (not AIsForegroundRecordTracking and (Ay > 0))) then
        Az := -Az;
    end
    else
      Az := ASignZ * Az;
  end;
  Result.ASignZ := Sign(Az);
  Result.X := Ax;
  Result.Y := Cos(PitchAngle) * Ay + Sin(PitchAngle) * Az;
  Result.Z := - Sin(PitchAngle) * Ay + Cos(PitchAngle) * Az;
end;

procedure TcxGridLayoutViewCarouselModeRecordsViewInfo.SetOffsetAngle(
  const Value: Double);
var
  ADelta: Integer;
  ACanCheckTopRecordIndex: Boolean;
begin
  if OffsetAngle <> Value then
  begin
    ADelta := Trunc(Value / Options.DeltaAngle);
    ACanCheckTopRecordIndex := ControllerHelper.AnimationTransition = nil;
    if ACanCheckTopRecordIndex and (Abs(Value - Options.DeltaAngle * ADelta) > Options.DeltaAngle / 2) then
      Inc(ADelta, Sign(Value));
    if (ControllerHelper.InternalTopRecordIndex - ADelta >= 0) and
      (ControllerHelper.InternalTopRecordIndex - ADelta < ViewData.RecordCount) then
    begin
      InternalSetOffsetAngle(Value);
      if ACanCheckTopRecordIndex then
      begin
        if ADelta <> 0 then
        begin
          FOffsetAngle := FOffsetAngle - Options.DeltaAngle * ADelta;
          ControllerHelper.InternalTopRecordIndex := ControllerHelper.InternalTopRecordIndex - ADelta;
        end;
        ControllerHelper.UpdatePrevStartRecordIndex;
      end;
      Recalculate;
    end;
  end;
end;

{ TcxGridLayoutViewInfoCarouselModeCalculator }

procedure TcxGridLayoutViewInfoCarouselModeCalculator.BeforeCalculate;
begin
  inherited BeforeCalculate;
  RecordsViewInfo.CalculateRadius;
end;

procedure TcxGridLayoutViewInfoCarouselModeCalculator.DoCalculate(const AContentBounds: TRect);
var
  I: Integer;
  ARecordViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo;
  AMinIndex, AMaxIndex: Integer;
begin
  if RecordsViewInfo.GridViewInfo.CalculateDown then
  begin
    RecordsViewInfo.CalculateVisibleRecordsBound(AMinIndex, AMaxIndex);
    for I := AMinIndex to AMaxIndex do
    begin
      ARecordViewInfo := RecordsViewInfo.GetRecordViewInfoByRecordIndex(I);
      ARecordViewInfo.Angle := CalculateAngle(ARecordViewInfo);
      ARecordViewInfo.Alpha := CalculateAlpha(ARecordViewInfo);
      ARecordViewInfo.CalculatedBounds := CalculateRecordBounds(AContentBounds, ARecordViewInfo);
      ARecordViewInfo.CalculationPosition := ARecordViewInfo.CalculatedBounds.TopLeft;
    end;
  end;

  RecordsViewInfo.CalculateVisibleCount;
end;

function TcxGridLayoutViewInfoCarouselModeCalculator.CalculateAlpha(AViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo): Integer;
begin
  Result := Trunc(GetInterpolatedValue(AViewInfo, 255, Options.BackgroundRecordAlphaLevel, False));
end;

function TcxGridLayoutViewInfoCarouselModeCalculator.CalculateAngle(AViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo): Double;
begin
  Result := dxHalfOfPi + AViewInfo.CarouselIndex * Options.DeltaAngle + RecordsViewInfo.OffsetAngle;
end;

function TcxGridLayoutViewInfoCarouselModeCalculator.CalculateRecordBounds(const AContentBounds: TRect;
  AViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo): TRect;
var
  AScale: Double;
  P: TPoint;
  xAxis, yAxis: TdxPointF;
  AOrigin: TPoint;
  Angle: Double;
begin
  AOrigin := RecordsViewInfo.GetContentCenter;

  Angle := AViewInfo.Angle;
  xAxis := dxPointF(Cos(RecordsViewInfo.RollAngle), Sin(RecordsViewInfo.RollAngle));
  yAxis := dxPointF(-xAxis.Y, xAxis.X);

  Result.Left := Trunc(RecordsViewInfo.Radius * Cos(Angle) + 0.5);
  Result.Top := -Trunc(RecordsViewInfo.Radius * Sin(Angle) *
    Cos(RecordsViewInfo.PitchAngle) + 0.5);

  AScale := CalculateScale(AViewInfo);
  Result := cxRectSetWidth(Result, Trunc(AViewInfo.Width * AScale + 0.5));
  Result := cxRectSetHeight(Result, Trunc(AViewInfo.Height * AScale + 0.5));

  P := Result.TopLeft;

  Result := cxRectSetLeft(Result, Trunc(AOrigin.X + P.X * xAxis.X + P.Y *
    yAxis.X));
  Result := cxRectSetTop(Result, Trunc(AOrigin.Y + P.X * xAxis.Y + P.Y *
    yAxis.Y));
  Result := cxRectOffset(Result, -Trunc(cxRectWidth(Result) / 2 + 0.5),
    -Trunc(cxRectHeight(Result) / 2 + 0.5));
end;

function TcxGridLayoutViewInfoCarouselModeCalculator.CalculateScale(AViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo): Double;
begin
  Result := GetInterpolatedValue(AViewInfo, 1, 1, True)
end;

function TcxGridLayoutViewInfoCarouselModeCalculator.GetOptions: TcxGridLayoutViewCarouselMode;
begin
  Result := RecordsViewInfo.Options;
end;

function TcxGridLayoutViewInfoCarouselModeCalculator.GetInterpolatedValue(
  AViewInfo: TcxGridLayoutViewCarouselModeRecordViewInfo;
  ABeginValue, AEndValue: Double; AUseRecordScale: Boolean): Double;

  function GetScale(const AValue: Cardinal): Double;
  begin
    Result := IfThen(AUseRecordScale, AValue / 100, 1);
  end;

var
  P1, P2: TdxPointF;
  Angle, ABoundAngle: Double;
begin
  ABoundAngle := Options.DeltaAngle;
  Angle := AViewInfo.Angle;
  Angle := dxHalfOfPi - Abs(Angle - dxHalfOfPi);
  if (Abs(Angle - dxHalfOfPi) < ABoundAngle) and
    RecordsViewInfo.ControllerHelper.IsForegroundRecord(AViewInfo.GridRecord.Index) then
  begin
    P1 := dxPointF(dxHalfOfPi - ABoundAngle,
      GetInternalInterpolatedValue(GetScale(Options.BackgroundRecordStartScale) * ABeginValue,
        GetScale(Options.BackgroundRecordEndScale) * AEndValue, ABoundAngle, dxHalfOfPi - ABoundAngle, not AUseRecordScale));
    P2 := dxPointF(dxHalfOfPi, ABeginValue);
    Result := P1.Y + (Angle - P1.X) * (P2.Y - P1.Y) / (P2.X - P1.X);
  end
  else
    Result := GetInternalInterpolatedValue(GetScale(Options.BackgroundRecordStartScale) * ABeginValue,
      GetScale(Options.BackgroundRecordEndScale) * AEndValue, ABoundAngle, Angle, not AUseRecordScale);
end;

function TcxGridLayoutViewInfoCarouselModeCalculator.GetRecordsViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo;
begin
  Result := TcxGridLayoutViewCarouselModeRecordsViewInfo(inherited RecordsViewInfo);
end;

{ TcxGridCarouselModeAnimationTransition }

constructor TcxGridCarouselModeAnimationTransition.Create(
  AControllerHelper: TcxGridLayoutViewControllerCarouselHelper; Angle: Double;
  ATime: Cardinal; AEffect: TdxAnimationTransitionEffect);
begin
  inherited Create(ATime, AEffect, cxAnimationLength);
  FControllerHelper := AControllerHelper;
  FAngle := Angle;
  if NeedSkipCards then
    Angle := Sign(Angle) * 2 * Pi;
  FScale := Angle / cxAnimationLength;
  FStartAngle := RecordsViewInfo.OffsetAngle;
end;

destructor TcxGridCarouselModeAnimationTransition.Destroy;
begin
  FControllerHelper.FAnimationTransition := nil;
  inherited Destroy;
end;

procedure TcxGridCarouselModeAnimationTransition.DoAnimate;
begin
  if not Finished then
    RecordsViewInfo.OffsetAngle := PositionToAngle(Position)
  else
    if not IsStop then
    begin
      ControllerHelper.UpdatePrevStartRecordIndex;
      RecordsViewInfo.OffsetAngle := 0;
    end;
end;

function TcxGridCarouselModeAnimationTransition.NeedSkipCards: Boolean;
begin
  Result := Abs(Angle) > 2 * Pi;
end;

function TcxGridCarouselModeAnimationTransition.PositionToAngle(APosition: Integer): Double;
begin
  if NeedSkipCards and (APosition > cxAnimationLength div 2) then
    Result := StartAngle + Angle - (cxAnimationLength - APosition) * FScale
  else
    Result := StartAngle + APosition * FScale;
end;

function TcxGridCarouselModeAnimationTransition.GetRecordsViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo;
begin
  Result := FControllerHelper.RecordsViewInfo;
end;

{ TcxGridLayoutViewControllerCarouselHelper }

constructor TcxGridLayoutViewControllerCarouselHelper.Create(AController: TcxGridCustomLayoutViewController);
begin
  inherited Create(AController);
  FTouchPoint := cxInvalidTouchPointData;
end;

destructor TcxGridLayoutViewControllerCarouselHelper.Destroy;
begin
  SetLength(FKineticDataPoints, 0);
  FreeAndNil(FAnimationTransition);
  inherited Destroy;
end;

function TcxGridLayoutViewControllerCarouselHelper.GetScrollDelta: Integer;
begin
  Result := 1;
end;

procedure TcxGridLayoutViewControllerCarouselHelper.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Controller.Site.MouseCapture then
    BeginTouchScrolling(Shift, Point(X, Y));
end;

procedure TcxGridLayoutViewControllerCarouselHelper.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if IsTouchScrolling(Shift) then
    TouchScrolling(Point(X, Y))
  else
    EndTouchScrolling;
end;

procedure TcxGridLayoutViewControllerCarouselHelper.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  EndTouchScrolling;
end;

function TcxGridLayoutViewControllerCarouselHelper.GetPageRecordCount(AVisibleRecordCount: Integer): Integer;
begin
  Result := Trunc(RecordsViewInfo.Options.RecordCount / 2 + 0.5);
end;

function TcxGridLayoutViewControllerCarouselHelper.IsForegroundRecord(AIndex: Integer): Boolean;
begin
  Result := (AIndex = InternalTopRecordIndex) or (AIndex = PrevStartRecordIndex);
end;

procedure TcxGridLayoutViewControllerCarouselHelper.UpdatePrevStartRecordIndex;
begin
  if RecordsViewInfo.OffsetAngle = 0 then
    FPrevStartRecordIndex := InternalTopRecordIndex
  else
    FPrevStartRecordIndex := InternalTopRecordIndex - Sign(RecordsViewInfo.OffsetAngle);
end;

procedure TcxGridLayoutViewControllerCarouselHelper.BeginTouchScrolling(Shift: TShiftState; const P: TPoint);
begin
  if IsTouchScrolling(Shift) and
    (Length(FKineticDataPoints) = 0) then
  begin
    FTouchPoint := RecordsViewInfo.PointToTouchPointData(P, 0);
    AddKineticDataPoint(0);
  end;
end;

function TcxGridLayoutViewControllerCarouselHelper.IsTouchScrolling(Shift: TShiftState): Boolean;
begin
  Result := (ssLeft in Shift) or dxIsTouchEvent(Shift);
end;

procedure TcxGridLayoutViewControllerCarouselHelper.EndTouchScrolling;
begin
  if Length(FKineticDataPoints) > 1 then
    RecordsViewInfo.EndTouchScrolling;
  FTouchPoint := cxInvalidTouchPointData;
  SetLength(FKineticDataPoints, 0);
end;

procedure TcxGridLayoutViewControllerCarouselHelper.TouchScrolling(const P: TPoint);
begin
  if not cxTouchPointDataEqual(TouchPoint, cxInvalidTouchPointData) then
    TouchPoint := RecordsViewInfo.PointToTouchPointData(P, TouchPoint.ASignZ);
end;

procedure TcxGridLayoutViewControllerCarouselHelper.StartAnimation(AEffect: TdxAnimationTransitionEffect;
  Angle: Double; ATime: Integer);
begin
  StopAnimation;
  FAnimationTransition := TcxGridCarouselModeAnimationTransition.Create(Self, Angle, ATime, AEffect);
  FAnimationTransition.Resume;
end;

procedure TcxGridLayoutViewControllerCarouselHelper.StartCarouselAnimation(AItemCountDelta: Integer);
var
  Angle: Double;
  ATime: Cardinal;
begin
  if AItemCountDelta = 0 then
    Exit;
  Angle := RecordsViewInfo.OffsetAngle + AItemCountDelta * RecordsViewInfo.Options.DeltaAngle;
  ATime := RecordsViewInfo.CalculateAnimationInterval(Angle);
  RecordsViewInfo.InternalSetOffsetAngle(Angle);
  if AnimationTransition = nil then
    PrevStartRecordIndex := InternalTopRecordIndex - AItemCountDelta;
  StartAnimation(ateTanh, -Angle, ATime);
end;

procedure TcxGridLayoutViewControllerCarouselHelper.StartKinematicAnimation;
const
  ADecelerationTime = 30;
  AMinSpeedValue = 0.0001;
  ADeceleration = 0.8;

  function CalculateSpeed: Double;
  var
    I: Integer;
    ATime: Cardinal;
  begin
    Result := 0;
    if Length(KineticDataPoints) > 1 then
      for I := 1 to Length(KineticDataPoints) - 1 do
      begin
        ATime := KineticDataPoints[I].Time - KineticDataPoints[I - 1].Time + 1;
        Result := 0.5 * Result + KineticDataPoints[I].Angle / ATime;
      end;
    Result := Result / Length(KineticDataPoints) * dxTimeToTickCount(1) / 1000;
  end;

var
  Angle: Double;
  ASpeed: Double;
  ATime: Cardinal;
  ADelta: Integer;
begin
  ASpeed := CalculateSpeed;
  if ASpeed = 0 then
    Exit;
  ATime := 0;
  Angle := RecordsViewInfo.OffsetAngle;
  while Abs(ASpeed) > AMinSpeedValue do
  begin
    Angle := Angle + ASpeed * ADecelerationTime;
    ASpeed := ASpeed * ADeceleration;
    Inc(ATime, ADecelerationTime);
  end;

  ADelta := Trunc(Angle / RecordsViewInfo.Options.DeltaAngle + 0.5);
  ADelta := Max(ADelta, -InternalTopRecordIndex);
  ADelta := Min(ADelta, RecordsViewInfo.ViewData.RecordCount - 1 - InternalTopRecordIndex);

  Angle := - ADelta * RecordsViewInfo.Options.DeltaAngle - RecordsViewInfo.OffsetAngle;

  if Abs(Angle) < RecordsViewInfo.Options.DeltaAngle then
    ATime := RecordsViewInfo.CalculateAnimationInterval(Angle);

  PrevStartRecordIndex := InternalTopRecordIndex;
  InternalTopRecordIndex := InternalTopRecordIndex + ADelta;
  RecordsViewInfo.InternalSetOffsetAngle(-Angle);
  StartAnimation(ateAccelerateDecelerate, Angle, ATime);
end;

procedure TcxGridLayoutViewControllerCarouselHelper.StopAnimation;
begin
  if FAnimationTransition <> nil then
  begin
    FAnimationTransition.IsStop := True;
    FAnimationTransition.Suspend(True);
  end;
end;

procedure TcxGridLayoutViewControllerCarouselHelper.AddKineticDataPoint(Angle: Double);
begin
  if Length(FKineticDataPoints) >= 10 then
    Move(FKineticDataPoints[1], FKineticDataPoints[0], Length(FKineticDataPoints) - 1)
  else
    SetLength(FKineticDataPoints, Length(FKineticDataPoints) + 1);
  FKineticDataPoints[Length(FKineticDataPoints) - 1].Angle := Angle;
  FKineticDataPoints[Length(FKineticDataPoints) - 1].Time := GetTickCount;
end;

function TcxGridLayoutViewControllerCarouselHelper.GetInternalTopRecordIndex: Integer;
begin
  Result := TcxGridLayoutViewControllerAccess(Controller).InternalTopRecordIndex;
end;

function TcxGridLayoutViewControllerCarouselHelper.GetRecordsViewInfo: TcxGridLayoutViewCarouselModeRecordsViewInfo;
begin
  Result := TcxGridLayoutViewCarouselModeRecordsViewInfo(ViewInfo.RecordsViewInfo);
end;

procedure TcxGridLayoutViewControllerCarouselHelper.SetInternalTopRecordIndex(Value: Integer);
begin
  FIsInternalTopRecordIndexChanging := True;
  try
    TcxGridLayoutViewControllerAccess(Controller).InternalTopRecordIndex := Value;
  finally
    FIsInternalTopRecordIndexChanging := False;
  end;
end;

procedure TcxGridLayoutViewControllerCarouselHelper.SetTouchPoint(const Value: TcxTouchPointData);
var
  Angle: Double;
begin
  if not cxTouchPointDataEqual(TouchPoint, Value)  then
  begin
    Angle := RecordsViewInfo.CalculateTouchScrollingAngle(FTouchPoint, Value);
    if Angle <> 0 then
    begin
      AddKineticDataPoint(Angle);
      StopAnimation;
      RecordsViewInfo.OffsetAngle := RecordsViewInfo.OffsetAngle - Angle;
      FTouchPoint := Value;
    end;
  end;
end;

end.
