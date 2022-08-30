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

unit dxMapControlViewInfo;

interface

{$I cxVer.inc}

uses
  SysUtils, Graphics, Classes, Types, RTLConsts, Forms, Math, Windows, Controls,
  dxCore, dxCoreGraphics, dxCoreClasses, cxClasses, cxControls, cxGeometry,
  cxGraphics, dxGdiPlusClasses, cxLookAndFeelPainters, dxScreenTip, dxCustomHint,
  dxMapControlTypes, dxMapControlElementViewInfo;

const
  // hit test constants
  mchtNone                      = 0;
  mchtNavigationPanel           = 1;
  mchtScrollButtons             = 2;
  mchtZoomTrackBar              = 3;
  mchtZoomTrackBarZoomInButton  = 4;
  mchtZoomTrackBarZoomOutButton = 5;
  mchtZoomTrackBarThumb         = 6;
  mchtZoomTrackBarScale         = 7;
  mchtMapItem                   = 8;
  mchtLayer                     = 9;

type
  TdxMapControlViewInfo = class;
  TdxMapControlPainter = class;
  TdxMapControlController = class;
  TdxMapControlHitTest = class;
  TdxMapControlZoomTrackBarViewInfo = class;

  TdxMapControlUIElementViewInfo = class(TdxMapControlElementViewInfo)
  private
    FPainter: TdxMapControlPainter;
    FViewInfo: TdxMapControlViewInfo;
    function GetController: TdxMapControlController;
    function GetScaleFactor: TdxScaleFactor;
  protected
    procedure DoElementDestroying; override;
    function GetIsVisible: Boolean; override;
    procedure Invalidate; override;
    function IsCapture: Boolean; override;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AViewInfo: TdxMapControlViewInfo); reintroduce; virtual;

    property Controller: TdxMapControlController read GetController;
    property Painter: TdxMapControlPainter read FPainter;
    property ViewInfo: TdxMapControlViewInfo read FViewInfo;
  end;

  TdxMapControlErrorPanelViewInfo = class(TdxMapControlUIElementViewInfo)

  end;

  TdxMapControlCoordinatesViewInfo = class(TdxMapControlUIElementViewInfo)
  private
    FXPattern: string;
    FYPattern: string;
    FText: string;
    function GetFont: TFont;
    function GetGeoPointFromPos(const APoint: TPoint): TdxMapControlGeoPoint;
    procedure SetText(const Value: string);
  protected
    function GeoPointToText(const AGeoPoint: TdxMapControlGeoPoint): string; virtual;
    function ParsePattern(const APattern: string): string; virtual;
    property Text: string read FText write SetText;
  public
    function DoCalculateSize: TSize; override;
    procedure Initialize; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint(ACanvas: TcxCanvas); override;

    property Font: TFont read GetFont;
  end;

  TdxMapControlLegendViewInfo = class(TdxMapControlUIElementViewInfo)
  public
    function DoCalculateSize: TSize; override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxMapControlScrollButtonsViewInfo = class(TdxMapControlUIElementViewInfo)
  private
    FCenter: TPoint;
    FScaleDelta: TPoint;
    FScrollTimer: TcxTimer;
    procedure ScrollMap;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure StartScrollTimer;
    procedure StopScrollTimer;
    procedure UpdateScaleDelta(X, Y: Integer);
  protected
    function GetHitTestIndex: Integer; override;
    procedure Invalidate; override;
  public
    constructor Create(AViewInfo: TdxMapControlViewInfo); override;
    destructor Destroy; override;
    procedure CalculateBounds; override;
    function DoCalculateSize: TSize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxMapControlScaleViewInfo = class(TdxMapControlUIElementViewInfo)
  private
    FIsKilometersVisible: Boolean;
    FIsMilesVisible: Boolean;
    FKilometersTextBounds: TRect;
    FKilometersTextSize: TSize;
    FKilometersLineWidth: Integer;
    FMetersMeasure: string;
    FMetersText: string;
    FMilesText: string;
    FMilesTextBounds: TRect;
    FMilesTextSize: TSize;
    FMilesLineWidth: Integer;
    FMilesMeasure: string;
    FScaleBounds: TRect;
    FScaleSize: TSize;
    procedure CalculateScaleLineLength(AKilometersScale: Double);
    function GetFont: TFont;
  protected
    function GetMaxScaleLineWidth: Integer; virtual;
  public
    procedure CalculateBounds; override;

    function DoCalculateSize: TSize; override;
    function IsKilometersVisible: Boolean;
    function IsMilesVisible: Boolean;
    procedure Paint(ACanvas: TcxCanvas); override;

    property Font: TFont read GetFont;
    property KilometersLineWidth: Integer read FKilometersLineWidth;
    property KilometersTextBounds: TRect read FKilometersTextBounds;
    property MetersText: string read FMetersText;
    property MilesTextBounds: TRect read FMilesTextBounds;
    property MilesLineWidth: Integer read FMilesLineWidth;
    property MilesText: string read FMilesText;
    property ScaleBounds: TRect read FScaleBounds;
  end;

  TdxMapControlZoomTrackBarPartViewInfo = class(TdxMapControlUIElementViewInfo)
  private
    function GetTrackBarViewInfo: TdxMapControlZoomTrackBarViewInfo;
  protected
    property TrackBarViewInfo: TdxMapControlZoomTrackBarViewInfo read GetTrackBarViewInfo;
  end;

  TdxMapControlZoomTrackBarButtonViewInfo = class(TdxMapControlZoomTrackBarPartViewInfo)
  private
    FDrawBounds: TRect;
  protected
    function GetOffsets: TRect; virtual;
    function IsZoomOut: Boolean; virtual;

    property DrawBounds: TRect read FDrawBounds;
  public
    procedure CalculateBounds; override;
    procedure Click; override;
    function DoCalculateSize: TSize; override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxMapControlZoomTrackBarZoomInButtonViewInfo = class(TdxMapControlZoomTrackBarButtonViewInfo)
  protected
    function GetHitTestIndex: Integer; override;
  end;

  TdxMapControlZoomTrackBarZoomOutButtonViewInfo = class(TdxMapControlZoomTrackBarButtonViewInfo)
  protected
    function GetHitTestIndex: Integer; override;
    function IsZoomOut: Boolean; override;
  end;

  TdxMapControlZoomTrackBarThumbViewInfo = class(TdxMapControlZoomTrackBarPartViewInfo)
  private
    FDrawBounds: TRect;
  protected
    function GetHitTestIndex: Integer; override;

    property DrawBounds: TRect read FDrawBounds;
  public
    procedure CalculateBounds; override;
    function DoCalculateSize: TSize; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxMapControlZoomTrackBarScaleViewInfo = class(TdxMapControlZoomTrackBarPartViewInfo)
  private
    FDrawBounds: TRect;
  protected
    function GetHitTestIndex: Integer; override;

    property DrawBounds: TRect read FDrawBounds;
  public
    procedure CalculateBounds; override;
    function DoCalculateSize: TSize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxMapControlZoomTrackBarViewInfo = class(TdxMapControlUIElementViewInfo)
  private
    FPixelsPerScaleInterval: Integer;
    FZoomTrackBarScaleViewInfo: TdxMapControlZoomTrackBarScaleViewInfo;
    FZoomTrackBarThumbViewInfo: TdxMapControlZoomTrackBarThumbViewInfo;
    FZoomTrackBarZoomInButtonViewInfo: TdxMapControlZoomTrackBarZoomInButtonViewInfo;
    FZoomTrackBarZoomOutButtonViewInfo: TdxMapControlZoomTrackBarZoomOutButtonViewInfo;
  protected
    procedure AddVisibleElements; override;
    function GetHitTestIndex: Integer; override;
    function GetZoomButtonOffset: Integer; virtual;
    procedure SetZoomLevelByThumbPosition(APosition: Integer);

    property PixelsPerScaleInterval: Integer read FPixelsPerScaleInterval;
  public
    constructor Create(AViewInfo: TdxMapControlViewInfo); override;
    destructor Destroy; override;
    procedure CalculateBounds; override;
    function DoCalculateSize: TSize; override;
    procedure Paint(ACanvas: TcxCanvas); override;

    property ZoomTrackBarScaleViewInfo: TdxMapControlZoomTrackBarScaleViewInfo read FZoomTrackBarScaleViewInfo;
    property ZoomTrackBarThumbViewInfo: TdxMapControlZoomTrackBarThumbViewInfo read FZoomTrackBarThumbViewInfo;
    property ZoomTrackBarZoomInButtonViewInfo: TdxMapControlZoomTrackBarZoomInButtonViewInfo read FZoomTrackBarZoomInButtonViewInfo;
    property ZoomTrackBarZoomOutButtonViewInfo: TdxMapControlZoomTrackBarZoomOutButtonViewInfo read FZoomTrackBarZoomOutButtonViewInfo;
  end;

  TdxMapControlNavigationPanelViewInfo = class(TdxMapControlUIElementViewInfo)
  private
    FCoordinatesViewInfo: TdxMapControlCoordinatesViewInfo;
    FScaleViewInfo: TdxMapControlScaleViewInfo;
    FScrollButtonsViewInfo: TdxMapControlScrollButtonsViewInfo;
    FZoomTrackBarViewInfo: TdxMapControlZoomTrackBarViewInfo;
  protected
    procedure AddVisibleElements; override;
    function GetContentOffsets: TRect; virtual;
    function GetHitTestIndex: Integer; override;
  public
    constructor Create(AViewInfo: TdxMapControlViewInfo); override;
    destructor Destroy; override;
    procedure CalculateBounds; override;
    function DoCalculateSize: TSize; override;
    procedure Paint(ACanvas: TcxCanvas); override;

    property CoordinatesViewInfo: TdxMapControlCoordinatesViewInfo read FCoordinatesViewInfo;
    property ScaleViewInfo: TdxMapControlScaleViewInfo read FScaleViewInfo;
    property ScrollButtonsViewInfo: TdxMapControlScrollButtonsViewInfo read FScrollButtonsViewInfo;
    property ZoomTrackBarViewInfo: TdxMapControlZoomTrackBarViewInfo read FZoomTrackBarViewInfo;
  end;

  TdxMapControlSelectedRegionViewInfo = class(TdxMapControlUIElementViewInfo)
  public
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxMapControlViewInfo = class
  private
    FBounds: TRect;
    FMapControl: TcxControl;
    FNavigationPanelViewInfo: TdxMapControlNavigationPanelViewInfo;
    FPainter: TdxMapControlPainter;
    FSelectedRegionViewInfo: TdxMapControlSelectedRegionViewInfo;
    FVisibleElements: TdxFastObjectList;
    function GetController: TdxMapControlController;
    function GetScaleFactor: TdxScaleFactor;
  protected
    procedure AddVisibleElements; virtual;
    procedure CalculateElementBounds; virtual;
    procedure CalculateElements; virtual;
    procedure CalculateElementSizes; virtual;
    procedure CreateElements; virtual;
    procedure InitializeElements; virtual;
    procedure Invalidate(const ABounds: TRect);
    function IsAnyLayerVisible: Boolean;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AMapControl: TcxControl); virtual;
    destructor Destroy; override;
    procedure Add(AElement: TdxMapControlElementViewInfo);
    procedure Calculate; virtual;
    procedure CalculateHitTest(AHitTest: TdxMapControlHitTest);
    procedure ClearCache; virtual;
    procedure DrawElements(ACanvas: TcxCanvas); virtual;
    procedure Paint(ACanvas: TcxCanvas); virtual;

    function IsCoordinatesVisible: Boolean;
    function IsElementVisible(AElement: TdxMapControlElementViewInfo): Boolean;
    function IsKilometersScaleVisible: Boolean;
    function IsMilesScaleVisible: Boolean;
    function IsNavigationPanelVisible: Boolean;
    function IsScaleVisible: Boolean;
    function IsScrollButtonsVisible: Boolean;
    function IsZoomTrackBarVisible: Boolean;

    property Bounds: TRect read FBounds;
    property Controller: TdxMapControlController read GetController;
    property MapControl: TcxControl read FMapControl;
    property NavigationPanelViewInfo: TdxMapControlNavigationPanelViewInfo read FNavigationPanelViewInfo;
    property Painter: TdxMapControlPainter read FPainter;
  end;

  TdxMapControlHitTest = class
  private
    FController: TdxMapControlController;
    FHitObject: TdxMapControlElementViewInfo;
    FFlags: Int64;
    FHitPoint: TPoint;
    function GetBitState(AIndex: Integer): Boolean;
  public
    constructor Create(AOwner: TdxMapControlController);
    procedure Clear;
    procedure Recalculate;
    procedure SetBitState(AIndex: Integer; AValue: Boolean);
    procedure Calculate(const APoint: TPoint); virtual;

    property HitAtNavigationPanel: Boolean index mchtNavigationPanel read GetBitState;
    property HitAtScrollButtons: Boolean index mchtScrollButtons read GetBitState;
    property HitAtZoomTrackBar: Boolean index mchtZoomTrackBar read GetBitState;
    property HitAtZoomTrackBarThumb: Boolean index mchtZoomTrackBarThumb read GetBitState;
    property HitObject: TdxMapControlElementViewInfo read FHitObject write FHitObject;
    property HitPoint: TPoint read FHitPoint;
  end;

  TdxMapControlHintHelper = class(TcxControlHintHelper)
  private
    FController: TdxMapControlController;
  protected
    procedure CorrectHintWindowRect(var ARect: TRect); override;
    function GetOwnerControl: TcxControl; override;
    function IsHintWindowVisible: Boolean;
    property Controller: TdxMapControlController read FController;
  public
    constructor Create(AController: TdxMapControlController);
  end;

  TdxMapControlController = class
  private
    FHintElement: TdxMapControlElementViewInfo;
    FHintHelper: TdxMapControlHintHelper;
    FHitTest: TdxMapControlHitTest;
    FHotElement: TdxMapControlElementViewInfo;
    FMapControl: TcxControl;
    FPressedElement: TdxMapControlElementViewInfo;
    FShowHintTimer: TcxTimer;
    procedure CheckHotElement(AShift: TShiftState; const APoint: TPoint);
    function GetViewInfo: TdxMapControlViewInfo;
    procedure SetHotElement(AValue: TdxMapControlElementViewInfo);
    procedure SetPressedElement(AValue: TdxMapControlElementViewInfo);
    procedure ShowHintTimerExpired(Sender: TObject);
  protected
    procedure CheckHint; virtual;
    function CreateHintHelper: TdxMapControlHintHelper; virtual;
    function CreateHitTestController: TdxMapControlHitTest; virtual;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseLeave; virtual;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    property HotElement: TdxMapControlElementViewInfo read FHotElement write SetHotElement;
    property PressedElement: TdxMapControlElementViewInfo read FPressedElement write SetPressedElement;
  public
    constructor Create(AMapControl: TcxControl);
    destructor Destroy; override;

    procedure ElementDestroying(AElement: TdxMapControlElementViewInfo);
    function GetScreenTip: TdxScreenTip;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseLeave;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    property HitTest: TdxMapControlHitTest read FHitTest;
    property ViewInfo: TdxMapControlViewInfo read GetViewInfo;
  end;

  TdxMapControlPainter = class
  private
    FMapControl: TcxControl;
    function GetElementColorByState(AState: TdxMapControlElementState): TdxAlphaColor;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetScaleFactor: TdxScaleFactor;
    function GetViewInfo: TdxMapControlViewInfo;
  protected
    procedure DrawCoordinatesInfo(ACanvas: TcxCanvas; AViewInfo: TdxMapControlCoordinatesViewInfo); virtual;
    procedure DrawNavigationPanel(ACanvas: TcxCanvas;
      AViewInfo: TdxMapControlNavigationPanelViewInfo); virtual;
    procedure DrawScale(ACanvas: TcxCanvas;
      AViewInfo: TdxMapControlScaleViewInfo); virtual;
    procedure DrawScrollButtons(ACanvas: TcxCanvas;
      AViewInfo: TdxMapControlScrollButtonsViewInfo); virtual;
    procedure DrawSelectedRegion(ACanvas: TcxCanvas;
      AViewInfo: TdxMapControlSelectedRegionViewInfo); virtual;
    procedure DrawZoomTrackBarBackground(ACanvas: TcxCanvas;
      AViewInfo: TdxMapControlZoomTrackBarViewInfo); virtual;
    procedure DrawZoomTrackBarZoomButtonBackround(ACanvas: TcxCanvas;
      AViewInfo: TdxMapControlZoomTrackBarButtonViewInfo); virtual;
    procedure DrawZoomTrackBarZoomButtonIcon(ACanvas: TcxCanvas;
      AViewInfo: TdxMapControlZoomTrackBarButtonViewInfo); virtual;
    procedure DrawZoomTrackBarScale(ACanvas: TcxCanvas;
      AViewInfo: TdxMapControlZoomTrackBarScaleViewInfo); virtual;
    procedure DrawZoomTrackBarThumbnail(ACanvas: TcxCanvas;
      AViewInfo: TdxMapControlZoomTrackBarThumbViewInfo); virtual;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AMapControl: TcxControl); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;

    property ViewInfo: TdxMapControlViewInfo read GetViewInfo;
  end;


implementation

uses
  StrUtils, dxMapControl, dxMapLayer, dxMapControlStrs;

type
  TdxMapControlGeoCoordinatePatternType = (cptCardinalPoint, cptDegree, cptMinute, cptSecond, cptPrecisionDegree,
    cptPrecisionMinute, cptPrecisionSecond);

const
  SDegreeSymbol: string = #$00B0;
  SMinuteSymbol = '''';
  SSecondSymbol = '''''';
  SCardinalPointPattern = 'CP';
  SDegreePattern = 'D';
  SMinutePattern = 'M';
  SSecondPattern = 'S';
  dxLongitudeStr: array [Boolean] of TcxResourceStringID = (@sdxMapControlWest, @sdxMapControlEast);
  dxLatitudeStr: array [Boolean] of TcxResourceStringID = (@sdxMapControlSouth, @sdxMapControlNorth);

type
  TdxCustomMapControlAccess = class(TdxCustomMapControl);

function GetMapControl(AMapControl: TcxControl): TdxCustomMapControlAccess;
begin
  Result := TdxCustomMapControlAccess(AMapControl);
end;

{ TdxMapControlUIElementViewInfo }

constructor TdxMapControlUIElementViewInfo.Create(AViewInfo: TdxMapControlViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
  FPainter := AViewInfo.Painter;
end;

function TdxMapControlUIElementViewInfo.GetController: TdxMapControlController;
begin
  Result := FViewInfo.Controller;
end;

function TdxMapControlUIElementViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := ViewInfo.ScaleFactor;
end;

procedure TdxMapControlUIElementViewInfo.DoElementDestroying;
begin
  Controller.ElementDestroying(Self);
end;

function TdxMapControlUIElementViewInfo.GetIsVisible: Boolean;
begin
  Result := FViewInfo.IsElementVisible(Self);
end;

procedure TdxMapControlUIElementViewInfo.Invalidate;
begin
  FViewInfo.Invalidate(Bounds);
end;

function TdxMapControlUIElementViewInfo.IsCapture: Boolean;
begin
  Result := Controller.PressedElement = Self;
end;

function TdxMapControlCoordinatesViewInfo.DoCalculateSize: TSize;
var
  ASizeOfDirectionText: TSize;
  ANorthTextSize, AWestTextSize, ASouthTextSize, AEastTextSize: TSize;
  ANorthYeastTextSize, ANorthWestTextSize, ASouthWestTextSize, ASouthYeastTextSize: TSize;
  AMaxSizeGeoPoint: TdxMapControlGeoPoint;
begin
  if (FXPattern = '') and (FYPattern = '') then
  begin
    ANorthTextSize := cxTextSize(Font, (cxGetResourceString(dxLatitudeStr[True])));
    AWestTextSize := cxTextSize(Font, (cxGetResourceString(dxLongitudeStr[False])));
    ASouthTextSize := cxTextSize(Font, (cxGetResourceString(dxLatitudeStr[False])));
    AEastTextSize := cxTextSize(Font, (cxGetResourceString(dxLongitudeStr[True])));
    ASizeOfDirectionText.cx := Max(AEastTextSize.cx, AWestTextSize.cx) + Max(ANorthTextSize.cx, ASouthTextSize.cx);
    ASizeOfDirectionText.cy := Max(Max(AEastTextSize.cy, AWestTextSize.cy), Max(ANorthTextSize.cy, ASouthTextSize.cy));
    Result := cxTextSize(Font, '00.0' + SDegreeSymbol + '    000.0' + SDegreeSymbol);
    Result.cx := Result.cx + ASizeOfDirectionText.cx;
    Result.cy := Max(Result.cy, ASizeOfDirectionText.cy);
  end
  else
  begin
    AMaxSizeGeoPoint := dxMapControlGeoPoint(89.725, 179.725);
    ANorthYeastTextSize := cxTextSize(Font, GeoPointToText(AMaxSizeGeoPoint));
    AMaxSizeGeoPoint := dxMapControlGeoPoint(89.725, -179.725);
    ANorthWestTextSize := cxTextSize(Font, GeoPointToText(AMaxSizeGeoPoint));
    AMaxSizeGeoPoint := dxMapControlGeoPoint(-89.725, -179.725);
    ASouthWestTextSize := cxTextSize(Font, GeoPointToText(AMaxSizeGeoPoint));
    AMaxSizeGeoPoint := dxMapControlGeoPoint(-89.725, 179.725);
    ASouthYeastTextSize := cxTextSize(Font, GeoPointToText(AMaxSizeGeoPoint));
    Result.cx := Max(Max(Max(ANorthYeastTextSize.cx, ANorthWestTextSize.cx), ASouthWestTextSize.cx), ASouthYeastTextSize.cx);
    Result.cy := Max(Max(Max(ANorthYeastTextSize.cy, ANorthWestTextSize.cy), ASouthWestTextSize.cy), ASouthYeastTextSize.cy);
  end;
end;

procedure TdxMapControlCoordinatesViewInfo.Initialize;
var
  APattern: string;
  AGeoPoint: TdxMapControlGeoPoint;
begin
  inherited Initialize;
  APattern := GetMapControl(ViewInfo.MapControl).NavigationPanel.XCoordinateDisplayMask;
  if APattern <> '' then
    FXPattern := ParsePattern(APattern)
  else
    FXPattern := '';
  APattern := GetMapControl(ViewInfo.MapControl).NavigationPanel.YCoordinateDisplayMask;
  if APattern <> '' then
    FYPattern := ParsePattern(APattern)
  else
    FYPattern := '';
  AGeoPoint := GetGeoPointFromPos(GetMapControl(ViewInfo.MapControl).ScreenToClient(GetMouseCursorPos));
  FText := GeoPointToText(AGeoPoint);
end;

procedure TdxMapControlCoordinatesViewInfo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AGeoPoint: TdxMapControlGeoPoint;
begin
  AGeoPoint := GetGeoPointFromPos(Point(X, Y));
  Text := GeoPointToText(AGeoPoint);
end;

procedure TdxMapControlCoordinatesViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Painter.DrawCoordinatesInfo(ACanvas, Self);
end;

function TdxMapControlCoordinatesViewInfo.GeoPointToText(const AGeoPoint: TdxMapControlGeoPoint): string;

  function GetMinutes(const ADegree: Double): Double;
  begin
    Result := (ADegree - Trunc(ADegree)) * 60;
  end;

  function GetSeconds(const ADegree: Double): Double;
  var
    AMinutes: Double;
  begin
    AMinutes := GetMinutes(ADegree);
    Result := (AMinutes - Trunc(AMinutes)) * 60;
  end;

var
  AValue, AMinutes, ASeconds, ADegree: Double;
  ADirectionName: string;
  AXCoordinateText, AYCoordinateText: string;
begin
  AValue := AGeoPoint.Longitude;
  ADegree := Abs(AValue);
  ADirectionName := cxGetResourceString(dxLongitudeStr[AValue > 0]);
  if FXPattern <> '' then
  begin
    AMinutes := GetMinutes(ADegree);
    ASeconds := GetSeconds(ADegree);
    AXCoordinateText := Format(FXPattern, [ADirectionName,
      Trunc(ADegree), Trunc(AMinutes), Trunc(ASeconds), ADegree, AMinutes, ASeconds]);
  end
  else
    AXCoordinateText := Format('%.1f' + SDegreeSymbol + '%s', [ADegree, ADirectionName]);

  AValue := AGeoPoint.Latitude;
  ADegree := Abs(AValue);
  ADirectionName := cxGetResourceString(dxLatitudeStr[AValue > 0]);
  if FYPattern <> '' then
  begin
    AMinutes := GetMinutes(ADegree);
    ASeconds := GetSeconds(ADegree);
    AYCoordinateText := Format(FYPattern, [ADirectionName,
      Trunc(ADegree), Trunc(AMinutes), Trunc(ASeconds), ADegree, AMinutes, ASeconds]);
  end
  else
    AYCoordinateText := Format('%.1f' + SDegreeSymbol + '%s', [ADegree, ADirectionName]);

  Result := AYCoordinateText + '    ' + AXCoordinateText;
end;

function TdxMapControlCoordinatesViewInfo.GetFont: TFont;
begin
  Result := GetMapControl(ViewInfo.MapControl).NavigationPanel.Style.CoordinateFont;
end;

function TdxMapControlCoordinatesViewInfo.GetGeoPointFromPos(const APoint: TPoint): TdxMapControlGeoPoint;
begin
   Result := GetMapControl(ViewInfo.MapControl).ScreenPointToGeoPoint(dxPointDouble(APoint));
end;

function TdxMapControlCoordinatesViewInfo.ParsePattern(const APattern: string): string;

  function GetArgumentNumberByType(AType: TdxMapControlGeoCoordinatePatternType): Integer;
  begin
    Result := Ord(AType);
  end;

  function HasSubPattern(const APattern: string; AStartSearchPos: Integer; out ASubPatternPos: Integer): Boolean;
  begin
    ASubPatternPos := PosEx('{', APattern, AStartSearchPos);
    Result := ASubPatternPos <> 0;
  end;

var
  AStartPos: Integer;
  AEndPos: Integer;
  ASubPattern: string;
  APatternType: TdxMapControlGeoCoordinatePatternType;
begin
  AEndPos := 0;
  Result := '';
  if Pos('{', APattern) = 0 then
    Result := APattern
  else
  begin
    while HasSubPattern(APattern, AEndPos + 1, AStartPos)  do
    begin
      if AStartPos > (AEndPos + 1) then
        Result := Result + Copy(APattern, AEndPos + 1, AStartPos - AEndPos - 1);
      AEndPos := PosEx('}', APattern, AStartPos + 1);
      if AEndPos <> 0 then
      begin
        ASubPattern := Trim(Copy(APattern, AStartPos + 1, AEndPos - AStartPos - 1));
        if SameText(ASubPattern, SCardinalPointPattern) then
          Result := Result + '%' + IntToStr(GetArgumentNumberByType(cptCardinalPoint)) + ':s'
        else
          if CharInSet(ASubPattern[1], [SDegreePattern, SMinutePattern, SSecondPattern]) then
          begin
            if Length(ASubPattern) = 1 then
            begin
              case ASubPattern[1] of
                SDegreePattern: APatternType := cptDegree;
                SMinutePattern: APatternType := cptMinute
              else // SSecondPattern
                APatternType := cptSecond;
              end;
              Result := Result + '%' + IntToStr(GetArgumentNumberByType(APatternType)) + ':d';
            end
            else
              if (Length(ASubPattern) = 3) and (ASubPattern[2] = ':') and CharInSet(ASubPattern[3], ['1'..'9']) then
              begin
                case ASubPattern[1] of
                  SDegreePattern: APatternType := cptPrecisionDegree;
                  SMinutePattern: APatternType := cptPrecisionMinute
                else // SSecondPattern
                  APatternType := cptPrecisionSecond;
                end;
                Result := Result + '%' + IntToStr(GetArgumentNumberByType(APatternType)) + ':.' + ASubPattern[3] + 'f';
              end;
          end;
      end
      else
        Exit;
    end;
    if AEndPos < Length(APattern) then
      Result := Result + Copy(APattern, AEndPos + 1, Length(APattern) - AEndPos);
  end;
end;

procedure TdxMapControlCoordinatesViewInfo.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    if IsVisible then
      Invalidate;
  end;
end;

{ TdxMapControlLegendViewInfo }

function TdxMapControlLegendViewInfo.DoCalculateSize: TSize;
begin
  Result := cxSize(0, 0);
end;

procedure TdxMapControlLegendViewInfo.Paint(ACanvas: TcxCanvas);
begin
end;

{ TdxMapControlScrollButtonsViewInfo }

constructor TdxMapControlScrollButtonsViewInfo.Create(AViewInfo: TdxMapControlViewInfo);
begin
  inherited;
  FScrollTimer := TcxTimer.Create(nil);
  FScrollTimer.Enabled := False;
  FScrollTimer.OnTimer := ScrollTimerHandler;
  FScrollTimer.Interval := 200;
end;

destructor TdxMapControlScrollButtonsViewInfo.Destroy;
begin
  FreeAndNil(FScrollTimer);
  inherited;
end;

procedure TdxMapControlScrollButtonsViewInfo.CalculateBounds;
begin
  FCenter := cxRectCenter(Bounds);
end;

function TdxMapControlScrollButtonsViewInfo.DoCalculateSize: TSize;
begin
  Result := ScaleFactor.Apply(cxSize(58, 58));
end;

procedure TdxMapControlScrollButtonsViewInfo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateScaleDelta(X, Y);
  ScrollMap;
  StartScrollTimer;
end;

procedure TdxMapControlScrollButtonsViewInfo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if IsCapture then
    UpdateScaleDelta(X, Y);
end;

procedure TdxMapControlScrollButtonsViewInfo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  StopScrollTimer;
end;

procedure TdxMapControlScrollButtonsViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Painter.DrawScrollButtons(ACanvas, Self);
end;

procedure TdxMapControlScrollButtonsViewInfo.UpdateScaleDelta(X, Y: Integer);
begin
  FScaleDelta := cxPoint(X - FCenter.X, Y - FCenter.Y);
end;

function TdxMapControlScrollButtonsViewInfo.GetHitTestIndex: Integer;
begin
  Result := mchtScrollButtons;
end;

procedure TdxMapControlScrollButtonsViewInfo.Invalidate;
begin
  FViewInfo.Invalidate(cxRectInflate(Bounds, ScaleFactor.Apply(1), ScaleFactor.Apply(1)));
end;

procedure TdxMapControlScrollButtonsViewInfo.ScrollMap;
begin
  GetMapControl(ViewInfo.MapControl).ScrollMap(FScaleDelta.X, FScaleDelta.Y);
end;

procedure TdxMapControlScrollButtonsViewInfo.ScrollTimerHandler(Sender: TObject);
begin
  FScrollTimer.Enabled := False;
  ScrollMap;
  FScrollTimer.Enabled := True;
end;

procedure TdxMapControlScrollButtonsViewInfo.StartScrollTimer;
begin
  FScrollTimer.Enabled := True;
end;

procedure TdxMapControlScrollButtonsViewInfo.StopScrollTimer;
begin
  FScrollTimer.Enabled := False;
end;

procedure TdxMapControlScaleViewInfo.CalculateBounds;
var
  ARect: TRect;
begin
  FKilometersTextBounds := cxNullRect;
  FMilesTextBounds := cxNullRect;
  ARect := Bounds;
  if IsKilometersVisible then
  begin
    FKilometersTextBounds := ARect;
    FKilometersTextBounds.Bottom := FKilometersTextBounds.Top + FKilometersTextSize.cy;
    FKilometersTextBounds.Right := FKilometersTextBounds.Left + FKilometersLineWidth;
    ARect.Top := FKilometersTextBounds.Bottom;
  end
  else
  begin
    FMilesTextBounds := Bounds;
    FMilesTextBounds.Bottom := FMilesTextBounds.Top + FMilesTextSize.cy;
    FMilesTextBounds.Right := FMilesTextBounds.Left + FMilesLineWidth;
    ARect.Top := FMilesTextBounds.Bottom;
  end;
  FScaleBounds := ARect;
  FScaleBounds.Bottom := FScaleBounds.Top + FScaleSize.cy;
  FScaleBounds.Right := FScaleBounds.Left + FScaleSize.cx;
  if IsMilesVisible and IsKilometersVisible then
  begin
    FMilesTextBounds := Bounds;
    FMilesTextBounds.Top := FMilesTextBounds.Bottom - FMilesTextSize.cy;
    FMilesTextBounds.Right := FMilesTextBounds.Left + FMilesLineWidth;
  end;
end;

function TdxMapControlScaleViewInfo.DoCalculateSize: TSize;
begin
  FIsKilometersVisible := ViewInfo.IsKilometersScaleVisible;
  FIsMilesVisible := ViewInfo.IsMilesScaleVisible;
  CalculateScaleLineLength(GetMapControl(ViewInfo.MapControl).GetKilometersScale(
    GetMapControl(ViewInfo.MapControl).ActualCenterPoint, GetMaxScaleLineWidth));
  if IsKilometersVisible then
  begin
    FKilometersTextSize := cxTextSize(Font, FMetersText + ' ' + FMetersMeasure);
    if FKilometersTextSize.cx < FKilometersLineWidth then
      FMetersText := FMetersText + ' ' + FMetersMeasure;
  end
  else
    FKilometersTextSize := cxNullSize;
  if IsMilesVisible then
  begin
    FMilesTextSize := cxTextSize(Font, FMilesText + ' ' + FMilesMeasure);
    if FMilesTextSize.cx < FMilesLineWidth then
      FMilesText := FMilesText + ' ' + FMilesMeasure;
  end
  else
    FMilesTextSize := cxNullSize;
  FScaleSize.cx := Max(IfThen(IsKilometersVisible, FKilometersLineWidth),
    IfThen(IsMilesVisible, FMilesLineWidth));
  if IsKilometersVisible and IsMilesVisible then
    FScaleSize.cy := ScaleFactor.Apply(15) // Top mark + scale + bottom mark
  else
    FScaleSize.cy := ScaleFactor.Apply(9); // Top mark + scale
  Result.cx := Max(Max(FMilesTextSize.cx, FKilometersTextSize.cx), GetMaxScaleLineWidth);
  Result.cy := FKilometersTextSize.cy + FMilesTextSize.cy + FScaleSize.cy;
end;

function TdxMapControlScaleViewInfo.IsKilometersVisible: Boolean;
begin
  Result := FIsKilometersVisible and (FMetersText <> '');
end;

function TdxMapControlScaleViewInfo.IsMilesVisible: Boolean;
begin
   Result := FIsMilesVisible;
end;

procedure TdxMapControlScaleViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Painter.DrawScale(ACanvas, Self);
end;

function TdxMapControlScaleViewInfo.GetFont: TFont;
begin
  Result := GetMapControl(ViewInfo.MapControl).NavigationPanel.Style.ScaleFont;
end;

function TdxMapControlScaleViewInfo.GetMaxScaleLineWidth: Integer;
begin
  Result := ScaleFactor.Apply(130);
end;

procedure TdxMapControlScaleViewInfo.CalculateScaleLineLength(AKilometersScale: Double);
const
  ScaleValuesCount = 23;
  KilometersScaleRanges: array [0..ScaleValuesCount - 1] of Double = (10000.0, 5000.0, 2500.0, 1000.0,
    500.0, 250.0, 200.0, 100.0, 50.0, 25.0, 20.0, 10.0, 5.0, 2.5, 1.0, 0.5, 0.25,
    0.2, 0.1, 0.05, 0.025, 0.010, 0.005);
  MilesScaleRanges: array [0..ScaleValuesCount - 1] of Double =       (10000.0, 5000.0, 2500.0, 1000.0,
    500.0, 250.0, 200.0, 100.0, 50.0, 25.0, 20.0, 10.0, 5.0, 2.5, 1.0, 0.5, 0.25,
    0.2, 0.1, 0.05, 0.025, 0.010, 0.005);
  FootsScaleRanges: array [0..13] of Double = (5000.0, 2500.0, 1000.0, 500.0, 250.0,
    200.0, 100.0, 50.0, 25.0, 20.0, 10.0, 5.0, 2.0, 1.0);

var
  I: Integer;
  AScaleRange: Double;
  AMeasure: string;
  AMileScale, AFootsScale: Double;
begin
  for I := 0 to ScaleValuesCount - 1 do
  begin
    AScaleRange := KilometersScaleRanges[I];
    if AScaleRange <= AKilometersScale then
    begin
      FKilometersLineWidth := Round(AScaleRange / AKilometersScale * GetMaxScaleLineWidth);
      if AScaleRange >= 1 then
        AMeasure := cxGetResourceString(@sdxMapControlKilometers)
      else
      begin
        AMeasure := cxGetResourceString(@sdxMapControlMeters);
        AScaleRange := AScaleRange * 1000.0;
      end;
      FMetersMeasure := AMeasure;
      FMetersText := Format('%d', [Trunc(AScaleRange)]);
      Break;
    end;
    FMetersText := '';
  end;
  AMileScale := AKilometersScale * 0.621371192;
  if AMileScale >= 1 then
    for I := 0 to ScaleValuesCount - 1 do
    begin
      AScaleRange := MilesScaleRanges[I];
      if AScaleRange <= AMileScale then
      begin
        FMilesLineWidth := Round(AScaleRange / AMileScale * GetMaxScaleLineWidth);
        FMilesMeasure := cxGetResourceString(@sdxMapControlMiles);
        FMilesText := Format('%d', [Trunc(AScaleRange)]);
        Break;
      end;
    end
  else
  begin
    AFootsScale := AMileScale * 5280.0;
    for I := 0 to High(FootsScaleRanges) do
    begin
      AScaleRange := FootsScaleRanges[I];
      if AScaleRange <= AFootsScale then
      begin
        FMilesLineWidth := Round(AScaleRange / AFootsScale * GetMaxScaleLineWidth);
        FMilesMeasure := cxGetResourceString(@sdxMapControlFeet);
        FMilesText := Format('%d', [Trunc(AScaleRange)]);
        Break;
      end;
    end
  end;
end;

{ TdxMapControlZoomTrackBarPartViewInfo }

function TdxMapControlZoomTrackBarPartViewInfo.GetTrackBarViewInfo: TdxMapControlZoomTrackBarViewInfo;
begin
  Result := ViewInfo.NavigationPanelViewInfo.ZoomTrackBarViewInfo;
end;

{ TdxMapControlZoomTrackBarButtonViewInfo }

procedure TdxMapControlZoomTrackBarButtonViewInfo.CalculateBounds;
begin
  FDrawBounds := cxRectContent(Bounds, GetOffsets);
  inherited CalculateBounds;
end;

procedure TdxMapControlZoomTrackBarButtonViewInfo.Click;
var
  ANewZoomLevel: Double;
  AMapControl: TdxCustomMapControlAccess;
begin
  AMapControl := GetMapControl(ViewInfo.MapControl);
  if IsZoomOut then
    ANewZoomLevel := AMapControl.ZoomLevel - 1
  else
    ANewZoomLevel := AMapControl.ZoomLevel + 1;
  if AMapControl.CanAnimate then
    AMapControl.ZoomAsync(ANewZoomLevel)
  else
    AMapControl.Zoom(ANewZoomLevel, False);
end;

function TdxMapControlZoomTrackBarButtonViewInfo.DoCalculateSize: TSize;
begin
  Result := ScaleFactor.Apply(cxSize(33, 33));
end;

procedure TdxMapControlZoomTrackBarButtonViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Painter.DrawZoomTrackBarZoomButtonBackround(ACanvas, Self);
  Painter.DrawZoomTrackBarZoomButtonIcon(ACanvas, Self);
end;

function TdxMapControlZoomTrackBarButtonViewInfo.IsZoomOut: Boolean;
begin
  Result := False;
end;

function TdxMapControlZoomTrackBarButtonViewInfo.GetOffsets: TRect;
begin
  Result := ScaleFactor.Apply(cxRect(2, 2, 2, 2));
end;

function TdxMapControlZoomTrackBarZoomInButtonViewInfo.GetHitTestIndex: Integer;
begin
  Result := mchtZoomTrackBarZoomInButton;
end;

{ TdxMapControlZoomTrackBarZoomOutButtonViewInfo }

function TdxMapControlZoomTrackBarZoomOutButtonViewInfo.IsZoomOut: Boolean;
begin
  Result := True;
end;

function TdxMapControlZoomTrackBarZoomOutButtonViewInfo.GetHitTestIndex: Integer;
begin
  Result := mchtZoomTrackBarZoomOutButton;
end;

{ TdxMapControlZoomTrackBarThumbViewInfo }

procedure TdxMapControlZoomTrackBarThumbViewInfo.CalculateBounds;
begin
  FDrawBounds := cxRectInflate(Bounds, ScaleFactor.Apply(-2), 0);
  inherited CalculateBounds;
end;

function TdxMapControlZoomTrackBarThumbViewInfo.DoCalculateSize: TSize;
begin
  Result := ScaleFactor.Apply(cxSize(11, 20));
end;

procedure TdxMapControlZoomTrackBarThumbViewInfo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if IsCapture then
    TrackBarViewInfo.SetZoomLevelByThumbPosition(X);
end;

procedure TdxMapControlZoomTrackBarThumbViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Painter.DrawZoomTrackBarThumbnail(ACanvas, Self);
end;

function TdxMapControlZoomTrackBarThumbViewInfo.GetHitTestIndex: Integer;
begin
  Result := mchtZoomTrackBarThumb;
end;

{ TdxMapControlZoomTrackBarScaleViewInfo }

procedure TdxMapControlZoomTrackBarScaleViewInfo.CalculateBounds;
begin
  FDrawBounds := cxRectCenterVertically(Bounds, ScaleFactor.Apply(3));
  inherited CalculateBounds;
end;

function TdxMapControlZoomTrackBarScaleViewInfo.DoCalculateSize: TSize;
begin
  Result := ScaleFactor.Apply(cxSize(140, 20));
end;

procedure TdxMapControlZoomTrackBarScaleViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Painter.DrawZoomTrackBarScale(ACanvas, Self);
end;

function TdxMapControlZoomTrackBarScaleViewInfo.GetHitTestIndex: Integer;
begin
  Result := mchtZoomTrackBarScale;
end;

procedure TdxMapControlZoomTrackBarScaleViewInfo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TrackBarViewInfo.SetZoomLevelByThumbPosition(X);
end;

{ TdxMapControlZoomTrackBarViewInfo }

constructor TdxMapControlZoomTrackBarViewInfo.Create(AViewInfo: TdxMapControlViewInfo);
begin
  inherited Create(AViewInfo);
  FZoomTrackBarScaleViewInfo := TdxMapControlZoomTrackBarScaleViewInfo.Create(AViewInfo);
  FZoomTrackBarThumbViewInfo := TdxMapControlZoomTrackBarThumbViewInfo.Create(AViewInfo);
  FZoomTrackBarZoomInButtonViewInfo := TdxMapControlZoomTrackBarZoomInButtonViewInfo.Create(AViewInfo);
  FZoomTrackBarZoomOutButtonViewInfo := TdxMapControlZoomTrackBarZoomOutButtonViewInfo.Create(AViewInfo);
end;

destructor TdxMapControlZoomTrackBarViewInfo.Destroy;
begin
  FreeAndNil(FZoomTrackBarZoomOutButtonViewInfo);
  FreeAndNil(FZoomTrackBarZoomInButtonViewInfo);
  FreeAndNil(FZoomTrackBarThumbViewInfo);
  FreeAndNil(FZoomTrackBarScaleViewInfo);
  inherited;
end;

procedure TdxMapControlZoomTrackBarViewInfo.CalculateBounds;
var
  AElementBounds: TRect;
begin
  AElementBounds := Bounds;
  AElementBounds.Right := AElementBounds.Left + FZoomTrackBarZoomOutButtonViewInfo.Size.cx;
  FZoomTrackBarZoomOutButtonViewInfo.Bounds := AElementBounds;

  AElementBounds := Bounds;
  AElementBounds.Left := AElementBounds.Right - FZoomTrackBarZoomInButtonViewInfo.Size.cx;
  FZoomTrackBarZoomInButtonViewInfo.Bounds := AElementBounds;

  AElementBounds := Bounds;
  AElementBounds.Left := FZoomTrackBarZoomOutButtonViewInfo.Bounds.Right + GetZoomButtonOffset;
  AElementBounds.Right := FZoomTrackBarZoomInButtonViewInfo.Bounds.Left - GetZoomButtonOffset;
  FZoomTrackBarScaleViewInfo.Bounds := AElementBounds;

  FPixelsPerScaleInterval := FZoomTrackBarScaleViewInfo.Size.cx div GetMapControl(ViewInfo.MapControl).GetMaxZoomLevel;
  AElementBounds := Bounds;
  AElementBounds := cxRectCenterVertically(AElementBounds, FZoomTrackBarThumbViewInfo.Size.cy);
  AElementBounds.Left := FZoomTrackBarScaleViewInfo.Bounds.Left +
    Round(FPixelsPerScaleInterval * (GetMapControl(ViewInfo.MapControl).ZoomLevel - 1)) +
    FPixelsPerScaleInterval div 2 - FZoomTrackBarThumbViewInfo.Size.cx div 2;
  AElementBounds.Right := AElementBounds.Left + FZoomTrackBarThumbViewInfo.Size.cx;
  FZoomTrackBarThumbViewInfo.Bounds := AElementBounds;

  inherited CalculateBounds;
end;

function TdxMapControlZoomTrackBarViewInfo.DoCalculateSize: TSize;
var
  AZoomTrackBarLength: Integer;
begin
  AZoomTrackBarLength := FZoomTrackBarZoomInButtonViewInfo.Size.cx * 2 + FZoomTrackBarScaleViewInfo.Size.cx +
    GetZoomButtonOffset * 2;
  Result := cxSize(AZoomTrackBarLength, FZoomTrackBarZoomInButtonViewInfo.Size.cy);
end;

procedure TdxMapControlZoomTrackBarViewInfo.AddVisibleElements;
begin
  Add(FZoomTrackBarZoomInButtonViewInfo);
  Add(FZoomTrackBarZoomOutButtonViewInfo);
  Add(FZoomTrackBarScaleViewInfo);
  Add(FZoomTrackBarThumbViewInfo);
end;

procedure TdxMapControlZoomTrackBarViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Painter.DrawZoomTrackBarBackground(ACanvas, Self);
  FZoomTrackBarThumbViewInfo.Paint(ACanvas);
  FZoomTrackBarScaleViewInfo.Paint(ACanvas);
  FZoomTrackBarZoomInButtonViewInfo.Paint(ACanvas);
  FZoomTrackBarZoomOutButtonViewInfo.Paint(ACanvas);
end;

function TdxMapControlZoomTrackBarViewInfo.GetHitTestIndex: Integer;
begin
  Result := mchtZoomTrackBar;
end;

function TdxMapControlZoomTrackBarViewInfo.GetZoomButtonOffset: Integer;
begin
  Result := ScaleFactor.Apply(5);
end;

procedure TdxMapControlZoomTrackBarViewInfo.SetZoomLevelByThumbPosition(APosition: Integer);
var
  ANewZoomLevel: Integer;
begin
  ANewZoomLevel := Round((APosition - ZoomTrackBarScaleViewInfo.Bounds.Left) / PixelsPerScaleInterval);
  if GetMapControl(ViewInfo.MapControl).CanAnimate then
    GetMapControl(ViewInfo.MapControl).ZoomAsync(ANewZoomLevel)
  else
    GetMapControl(ViewInfo.MapControl).Zoom(ANewZoomLevel, False);
end;

{ TdxMapControlNavigationPanelViewInfo }

constructor TdxMapControlNavigationPanelViewInfo.Create(
  AViewInfo: TdxMapControlViewInfo);
begin
  inherited Create(AViewInfo);
  FCoordinatesViewInfo := TdxMapControlCoordinatesViewInfo.Create(AViewInfo);
  FScaleViewInfo := TdxMapControlScaleViewInfo.Create(AViewInfo);
  FScrollButtonsViewInfo := TdxMapControlScrollButtonsViewInfo.Create(AViewInfo);
  FZoomTrackBarViewInfo := TdxMapControlZoomTrackBarViewInfo.Create(AViewInfo);
end;

destructor TdxMapControlNavigationPanelViewInfo.Destroy;
begin
  FreeAndNil(FZoomTrackBarViewInfo);
  FreeAndNil(FScrollButtonsViewInfo);
  FreeAndNil(FScaleViewInfo);
  FreeAndNil(FCoordinatesViewInfo);
  inherited Destroy;
end;

procedure TdxMapControlNavigationPanelViewInfo.CalculateBounds;
var
  AAvailableBounds, AElementBounds: TRect;
  AOffset: Integer;
begin
  AOffset := ScaleFactor.Apply(15);
  AAvailableBounds := cxRectContent(Bounds, GetContentOffsets);
  if ViewInfo.IsCoordinatesVisible then
    if not FCoordinatesViewInfo.IsEnoughSpace(AAvailableBounds) then
      Remove(FCoordinatesViewInfo)
    else
    begin
      AElementBounds := AAvailableBounds;
      AElementBounds.Left := AElementBounds.Right - FCoordinatesViewInfo.Size.cx;
      FCoordinatesViewInfo.Bounds := cxRectCenterVertically(AElementBounds, FCoordinatesViewInfo.Size.cy);
      AAvailableBounds.Right := AElementBounds.Left - AOffset;
    end;
  if ViewInfo.IsZoomTrackBarVisible then
    if not FZoomTrackBarViewInfo.IsEnoughSpace(AAvailableBounds) then
      Remove(FZoomTrackBarViewInfo)
    else
    begin
      AElementBounds := AAvailableBounds;
      AElementBounds.Right := AElementBounds.Left + FZoomTrackBarViewInfo.Size.cx;
      FZoomTrackBarViewInfo.Bounds := cxRectCenterVertically(AElementBounds, FZoomTrackBarViewInfo.Size.cy);
      AAvailableBounds.Left := AElementBounds.Right + AOffset;
    end;
  if ViewInfo.IsScrollButtonsVisible then
    if not FScrollButtonsViewInfo.IsEnoughSpace(AAvailableBounds) then
      Remove(FScrollButtonsViewInfo)
    else
    begin
      AElementBounds := cxRectCenterVertically(AAvailableBounds, FScrollButtonsViewInfo.Size.cy);
      if ViewInfo.IsElementVisible(FZoomTrackBarViewInfo) then
      begin
        AElementBounds.Left := FZoomTrackBarViewInfo.Bounds.Left;
        AElementBounds.Right := AElementBounds.Left + FScrollButtonsViewInfo.Size.cx;
        FZoomTrackBarViewInfo.Bounds := cxRectOffsetHorz(FZoomTrackBarViewInfo.Bounds,
          FScrollButtonsViewInfo.Size.cx + AOffset);
        AAvailableBounds.Left := FZoomTrackBarViewInfo.Bounds.Right + AOffset;
      end
      else
      begin
        AElementBounds.Right := AElementBounds.Left + FScrollButtonsViewInfo.Size.cx;
        AAvailableBounds.Left := AElementBounds.Right + AOffset;
      end;
      FScrollButtonsViewInfo.Bounds := AElementBounds;
    end;
  if ViewInfo.IsScaleVisible then
    if not FScaleViewInfo.IsEnoughSpace(AAvailableBounds) then
      Remove(FScaleViewInfo)
    else
    begin
      AElementBounds := cxRectCenterVertically(AAvailableBounds, FScaleViewInfo.Size.cy);
      if ViewInfo.IsElementVisible(FCoordinatesViewInfo) then
      begin
        AElementBounds.Right := FCoordinatesViewInfo.Bounds.Right;
        AElementBounds.Left := AElementBounds.Right - FScaleViewInfo.Size.cx;
        FCoordinatesViewInfo.Bounds := cxRectOffsetHorz(FCoordinatesViewInfo.Bounds, -FScaleViewInfo.Size.cx - AOffset);
        AAvailableBounds.Right := FCoordinatesViewInfo.Bounds.Left;
      end
      else
      begin
        AElementBounds.Left := AElementBounds.Right - FScaleViewInfo.Size.cx;
        AAvailableBounds.Right := AElementBounds.Left;
      end;
      FScaleViewInfo.Bounds := AElementBounds;
    end;
  inherited CalculateBounds;
end;

function TdxMapControlNavigationPanelViewInfo.DoCalculateSize: TSize;
begin
  Result := cxSize(cxRectWidth(GetMapControl(ViewInfo.MapControl).Bounds),
    GetMapControl(ViewInfo.MapControl).NavigationPanel.Height);
end;

procedure TdxMapControlNavigationPanelViewInfo.AddVisibleElements;
begin
  if ViewInfo.IsScrollButtonsVisible then
    Add(FScrollButtonsViewInfo);
  if ViewInfo.IsZoomTrackBarVisible then
    Add(FZoomTrackBarViewInfo);
  if ViewInfo.IsScaleVisible then
    Add(FScaleViewInfo);
  if ViewInfo.IsCoordinatesVisible then
    Add(FCoordinatesViewInfo);
end;

procedure TdxMapControlNavigationPanelViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Painter.DrawNavigationPanel(ACanvas, Self);
  inherited Paint(ACanvas);
end;

function TdxMapControlNavigationPanelViewInfo.GetContentOffsets: TRect;
begin
  Result := ScaleFactor.Apply(cxRect(15, 5, 30, 5));
end;

function TdxMapControlNavigationPanelViewInfo.GetHitTestIndex: Integer;
begin
  Result := mchtNavigationPanel;
end;

{ TdxMapControlSelectedRegionViewInfo }

procedure TdxMapControlSelectedRegionViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Bounds := GetMapControl(ViewInfo.MapControl).SelectedRegionBounds;
  if not IsRectEmpty(Bounds) then
    Painter.DrawSelectedRegion(ACanvas, Self);
end;

{ TdxMapControlViewInfo }

constructor TdxMapControlViewInfo.Create(AMapControl: TcxControl);
begin
  inherited Create;
  FMapControl := AMapControl;
  FPainter := GetMapControl(FMapControl).Painter;
  FVisibleElements := TdxFastObjectList.Create(False);
  CreateElements;
end;

destructor TdxMapControlViewInfo.Destroy;
begin
  FreeAndNil(FSelectedRegionViewInfo);
  FreeAndNil(FNavigationPanelViewInfo);
  FreeAndNil(FVisibleElements);
  inherited Destroy;
end;

procedure TdxMapControlViewInfo.Add(AElement: TdxMapControlElementViewInfo);
begin
  FVisibleElements.Add(AElement);
end;

procedure TdxMapControlViewInfo.CalculateHitTest(AHitTest: TdxMapControlHitTest);
var
  I: Integer;
begin
  for I := FVisibleElements.Count - 1 downto 0 do
    if (FVisibleElements[I] as TdxMapControlElementViewInfo).GetHitTest(AHitTest) then
      Break;
end;

procedure TdxMapControlViewInfo.Calculate;
begin
  FBounds := GetMapControl(FMapControl).Bounds;
  FVisibleElements.Clear;
  AddVisibleElements;
  CalculateElements;
  if GetMapControl(FMapControl).DragAndDropState = ddsNone then
    Controller.CheckHotElement(KeyboardStateToShiftState,
      GetMapControl(FMapControl).ScreenToClient(cxControls.GetMouseCursorPos));
end;

procedure TdxMapControlViewInfo.ClearCache;
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TdxMapControlElementViewInfo).ClearCache;
end;

procedure TdxMapControlViewInfo.DrawElements(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TdxMapControlElementViewInfo).Paint(ACanvas);
end;

procedure TdxMapControlViewInfo.Paint(ACanvas: TcxCanvas);
begin
  FPainter.DrawBackground(ACanvas);
  DrawElements(ACanvas);
end;

function TdxMapControlViewInfo.IsCoordinatesVisible: Boolean;
begin
  Result := GetMapControl(FMapControl).NavigationPanel.ShowCoordinates;
end;

function TdxMapControlViewInfo.IsElementVisible(AElement: TdxMapControlElementViewInfo): Boolean;
var
  I: Integer;
begin
  Result := FVisibleElements.IndexOf(AElement) <> -1;
  if not Result then
    for I := 0 to FVisibleElements.Count - 1 do
    begin
      Result := (FVisibleElements[I] as TdxMapControlElementViewInfo).IsElementVisible(AElement);
      if Result then
        Break;
    end;
end;

function TdxMapControlViewInfo.IsKilometersScaleVisible: Boolean;
begin
  Result := GetMapControl(FMapControl).NavigationPanel.ShowKilometersScale and IsAnyLayerVisible;
end;

function TdxMapControlViewInfo.IsMilesScaleVisible: Boolean;
begin
  Result := GetMapControl(FMapControl).NavigationPanel.ShowMilesScale and IsAnyLayerVisible;
end;

function TdxMapControlViewInfo.IsNavigationPanelVisible: Boolean;
begin
  Result := GetMapControl(FMapControl).NavigationPanel.Visible;
end;

function TdxMapControlViewInfo.IsScaleVisible: Boolean;
begin
  Result := IsMilesScaleVisible or IsKilometersScaleVisible;
end;

function TdxMapControlViewInfo.IsScrollButtonsVisible: Boolean;
begin
  Result := GetMapControl(FMapControl).NavigationPanel.ShowScrollButtons and
    GetMapControl(FMapControl).CanScroll;
end;

function TdxMapControlViewInfo.IsZoomTrackBarVisible: Boolean;
begin
  Result := GetMapControl(FMapControl).NavigationPanel.ShowZoomTrackBar and
    GetMapControl(FMapControl).CanZoom;
end;

procedure TdxMapControlViewInfo.AddVisibleElements;
begin
  if not cxRectIsEmpty(Bounds) then
  begin
    if GetMapControl(MapControl).Layers.Count > 0 then
      Add(GetMapControl(MapControl).Layers.ViewInfo);
    Add(FSelectedRegionViewInfo);
    if IsNavigationPanelVisible then
      Add(FNavigationPanelViewInfo);
  end;
end;

procedure TdxMapControlViewInfo.CalculateElementBounds;
var
  I: Integer;
  AElementBounds: TRect;
begin
  AElementBounds := Bounds;
  AElementBounds.Top := AElementBounds.Bottom - FNavigationPanelViewInfo.Size.cy;
  FNavigationPanelViewInfo.Bounds := AElementBounds;
 // GetMapControl(MapControl).Layers.ViewInfo.Bounds := Bounds; //# move to InitializeElements
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TdxMapControlElementViewInfo).CalculateBounds;
end;

procedure TdxMapControlViewInfo.CalculateElements;
begin
  InitializeElements;
  CalculateElementSizes;
  CalculateElementBounds;
end;

procedure TdxMapControlViewInfo.CalculateElementSizes;
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TdxMapControlElementViewInfo).CalculateSize;
end;

procedure TdxMapControlViewInfo.CreateElements;
begin
  FNavigationPanelViewInfo := TdxMapControlNavigationPanelViewInfo.Create(Self);
  FSelectedRegionViewInfo := TdxMapControlSelectedRegionViewInfo.Create(Self);
end;

procedure TdxMapControlViewInfo.InitializeElements;
var
  I: Integer;
begin
  GetMapControl(MapControl).Layers.ViewInfo.Bounds := Bounds;
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TdxMapControlElementViewInfo).Initialize;
end;

procedure TdxMapControlViewInfo.Invalidate(const ABounds: TRect);
begin
  MapControl.InvalidateRect(ABounds, True);
end;

function TdxMapControlViewInfo.IsAnyLayerVisible: Boolean;
begin
  Result := GetMapControl(MapControl).Layers.ViewInfo.Count > 0;
end;

function TdxMapControlViewInfo.GetController: TdxMapControlController;
begin
  Result := GetMapControl(MapControl).Controller;
end;

function TdxMapControlViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := GetMapControl(MapControl).ScaleFactor;
end;

{ TdxMapControlHitTest }

constructor TdxMapControlHitTest.Create(AOwner: TdxMapControlController);
begin
  inherited Create;
  FController := AOwner;
end;

procedure TdxMapControlHitTest.Clear;
begin
  FFlags := 0;
  FHitObject := nil;
end;

procedure TdxMapControlHitTest.Recalculate;
begin
  Clear;
  FController.ViewInfo.CalculateHitTest(Self);
end;

procedure TdxMapControlHitTest.Calculate(const APoint: TPoint);
begin
  FHitPoint := APoint;
  Recalculate;
end;

function TdxMapControlHitTest.GetBitState(AIndex: Integer): Boolean;
begin
  Result := (FFlags and (1 shl AIndex)) <> 0;
end;

procedure TdxMapControlHitTest.SetBitState(AIndex: Integer; AValue: Boolean);
begin
  if AValue then
    FFlags := FFlags or (1 shl AIndex)
  else
    FFlags := FFlags and not (1 shl AIndex);
end;

{ TdxMapControlHintHelper }

constructor TdxMapControlHintHelper.Create(AController: TdxMapControlController);
begin
  inherited Create;
  FController := AController
end;

procedure TdxMapControlHintHelper.CorrectHintWindowRect(var ARect: TRect);
begin
  inherited;
  ARect := cxRectSetOrigin(ARect, GetMouseCursorPos);
  OffsetRect(ARect, 0, cxGetCursorSize.cy);
end;

function TdxMapControlHintHelper.GetOwnerControl: TcxControl;
begin
  Result := FController.FMapControl;
end;

function TdxMapControlHintHelper.IsHintWindowVisible: Boolean;
begin
  Result := (HintWindow <> nil) and HintWindow.HandleAllocated and
    IsWindowVisible(HintWindow.Handle);
end;

{ TdxMapControlController }

constructor TdxMapControlController.Create(AMapControl: TcxControl);
begin
  inherited Create;
  FMapControl := AMapControl;
  FHintHelper := CreateHintHelper;
  FHitTest := CreateHitTestController;
  FShowHintTimer := TcxTimer.Create(nil);
  FShowHintTimer.OnTimer := ShowHintTimerExpired;
end;

destructor TdxMapControlController.Destroy;
begin
  FreeAndNil(FShowHintTimer);
  FreeAndNil(FHitTest);
  FreeAndNil(FHintHelper);
  inherited Destroy;
end;

procedure TdxMapControlController.ElementDestroying(AElement: TdxMapControlElementViewInfo);
begin
  if HitTest.HitObject = AElement then
    HitTest.Clear;
  if FHintHelper.HintableObject = AElement then
  begin
    FHintElement := nil;
    FHintHelper.CancelHint;
  end;
  if FPressedElement = AElement then
    FPressedElement := nil;
  if FHotElement = AElement then
    FHotElement := nil;
end;

function TdxMapControlController.GetScreenTip: TdxScreenTip;
begin
  if FHintElement <> nil then
    Result := FHintElement.GetScreenTip
  else
    Result := nil;
end;

procedure TdxMapControlController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DoMouseDown(Button, Shift, X, Y);
end;

procedure TdxMapControlController.MouseLeave;
begin
  DoMouseLeave;
end;

procedure TdxMapControlController.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  DoMouseMove(Shift, X, Y);
end;

procedure TdxMapControlController.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DoMouseUp(Button, Shift, X, Y);
end;

procedure TdxMapControlController.CheckHint;
var
  APrevHintElement: TdxMapControlElementViewInfo;
  AWasVisible: Boolean;
begin
  APrevHintElement := FHintElement;
  FHintElement := HotElement;
  if FHintElement <> APrevHintElement then
  begin
    FShowHintTimer.Enabled := False;
    AWasVisible := FHintHelper.IsHintWindowVisible;
    FHintHelper.ResetLastHintElement;
    if (FHintElement <> nil) and ((FHintElement.GetHint <> '') or (FHintElement.GetScreenTip <> nil)) then
    begin
      if AWasVisible then
        FShowHintTimer.Interval := Application.HintShortPause
      else
        FShowHintTimer.Interval := Application.HintPause;
      FShowHintTimer.Enabled := True;
    end;
  end;
end;

function TdxMapControlController.CreateHintHelper: TdxMapControlHintHelper;
begin
  Result := TdxMapControlHintHelper.Create(Self);
end;

function TdxMapControlController.CreateHitTestController: TdxMapControlHitTest;
begin
  Result := TdxMapControlHitTest.Create(Self);
end;

procedure TdxMapControlController.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FHintHelper.MouseDown;
  FHitTest.Calculate(cxPoint(X, Y));
  PressedElement := FHitTest.HitObject;
  if PressedElement <> nil then
    PressedElement.MouseDown(Button, Shift, X, Y);
end;

procedure TdxMapControlController.DoMouseLeave;
begin
  FHintHelper.MouseLeave;
  CheckHotElement([], cxInvisiblePoint);
end;

procedure TdxMapControlController.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if GetMapControl(FMapControl).DragAndDropState <> ddsNone then
    Exit;
  CheckHotElement(Shift, cxPoint(X, Y));
  ViewInfo.NavigationPanelViewInfo.CoordinatesViewInfo.MouseMove(Shift, X, Y);
  if PressedElement <> nil then
    PressedElement.MouseMove(Shift, X, Y)
  else
    if HotElement <> nil then
      HotElement.MouseMove(Shift, X, Y);
  CheckHint;
end;

procedure TdxMapControlController.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  APreviousPressedElement: TdxMapControlElementViewInfo;
begin
  FHitTest.Calculate(cxPoint(X, Y));
  APreviousPressedElement := PressedElement;
  if PressedElement <> nil then
  begin
    PressedElement.MouseUp(Button, Shift, X, Y);
    PressedElement := nil;
  end;
  if (HotElement <> nil) and (HotElement = APreviousPressedElement) then
    HotElement.Click;
end;

procedure TdxMapControlController.CheckHotElement(AShift: TShiftState; const APoint: TPoint);
begin
  FHitTest.Calculate(APoint);
  HotElement := FHitTest.HitObject;
end;

function TdxMapControlController.GetViewInfo: TdxMapControlViewInfo;
begin
  Result := GetMapControl(FMapControl).ViewInfo as TdxMapControlViewInfo;
end;

procedure TdxMapControlController.SetHotElement(AValue: TdxMapControlElementViewInfo);
begin
  if FHotElement <> AValue then
  begin
    if (FHotElement <> nil) and
      (FHotElement.State <> mcesSelected) then
      if FHotElement = FPressedElement then
        FHotElement.State := mcesPressed
      else
        FHotElement.State := mcesNormal;
    FHotElement := AValue;
    if (FHotElement <> nil) and
      (FHotElement.State <> mcesSelected) then
      if FHotElement = FPressedElement then
        FHotElement.State := mcesPressed
      else
        FHotElement.State := mcesHot;
  end;
end;

procedure TdxMapControlController.SetPressedElement(AValue: TdxMapControlElementViewInfo);
begin
  if FPressedElement <> AValue then
  begin
    if (FPressedElement <> nil) and
      (FPressedElement.State <> mcesSelected) then
      if FPressedElement = FHotElement then
        FPressedElement.State := mcesHot
      else
        FPressedElement.State := mcesNormal;
    FPressedElement := AValue;
    if (FPressedElement <> nil) and
      (FPressedElement.State <> mcesSelected) then
      FPressedElement.State := mcesPressed;
  end;
end;

procedure TdxMapControlController.ShowHintTimerExpired(Sender: TObject);
begin
  FShowHintTimer.Enabled := False;
  if (FHintElement <> nil) and ((FHintElement.GetHint <> '') or (FHintElement.GetScreenTip <> nil)) then
    FHintHelper.ShowHint(FHintElement.Bounds, FHintElement.Bounds, FHintElement.GetHint, False, FHintElement);
end;

{ TdxMapControlPainter }

constructor TdxMapControlPainter.Create(AMapControl: TcxControl);
begin
  inherited Create;
  FMapControl := AMapControl;
end;

procedure TdxMapControlPainter.DrawBackground(ACanvas: TcxCanvas);
var
  AColor: TColor;
begin
  AColor := (FMapControl as TdxCustomMapControl).Color;
  if AColor = clDefault then
    AColor := LookAndFeelPainter.MapControlBackgroundColor;
  ACanvas.FillRect(ViewInfo.Bounds, AColor);
end;

procedure TdxMapControlPainter.DrawCoordinatesInfo(ACanvas: TcxCanvas; AViewInfo: TdxMapControlCoordinatesViewInfo);
var
  AStyle: TdxMapControlNavigationPanelStyle;
begin
  ACanvas.Font := AViewInfo.Font;
  AStyle := GetMapControl(ViewInfo.MapControl).NavigationPanel.Style;
  if AStyle.CoordinateTextColor = clDefault then
    ACanvas.Font.Color := dxAlphaColorToColor(GetElementColorByState(mcesNormal))
  else
    ACanvas.Font.Color := AStyle.CoordinateTextColor;
  ACanvas.DrawText(AViewInfo.Text, AViewInfo.Bounds, DT_CENTER);
end;

procedure TdxMapControlPainter.DrawNavigationPanel(ACanvas: TcxCanvas; AViewInfo: TdxMapControlNavigationPanelViewInfo);
var
  AColor: TdxAlphaColor;
begin
  with TdxGPCanvas.Create(ACanvas.Handle) do
  try
    AColor := GetMapControl(FMapControl).NavigationPanel.Style.Color;
    if AColor = dxacDefault then
      AColor := LookAndFeelPainter.MapControlPanelBackColor;
    Rectangle(AViewInfo.Bounds, AColor,  AColor);
  finally
    Free;
  end;
end;

procedure TdxMapControlPainter.DrawScale(ACanvas: TcxCanvas; AViewInfo: TdxMapControlScaleViewInfo);
var
  AGraduationMarkThickness: Integer;
  AScaleThickness: Integer;
  AScaleLineRect: TRect;
  AStartGraduationMark, AKilometersGraduationMark, AMilesGraduationMark: TRect;
  AColor: TColor;
  AStyle: TdxMapControlNavigationPanelStyle;
begin
  AGraduationMarkThickness := ScaleFactor.Apply(2);
  AScaleThickness := ScaleFactor.Apply(3);
  ACanvas.Font := AViewInfo.Font;
  AStyle := GetMapControl(ViewInfo.MapControl).NavigationPanel.Style;
  if AStyle.ScaleTextColor = clDefault then
    ACanvas.Font.Color := dxAlphaColorToColor(GetElementColorByState(mcesNormal))
  else
    ACanvas.Font.Color := AStyle.ScaleTextColor;
  AColor := dxAlphaColorToColor(GetElementColorByState(mcesNormal));
  if AViewInfo.IsKilometersVisible then
    ACanvas.DrawText(AViewInfo.MetersText, AViewInfo.KilometersTextBounds, DT_CENTER or DT_VCENTER);
  if AViewInfo.IsMilesVisible then
    ACanvas.DrawText(AViewInfo.MilesText, AViewInfo.MilesTextBounds, DT_CENTER or DT_VCENTER);

  AScaleLineRect := AViewInfo.ScaleBounds;
  if AViewInfo.IsKilometersVisible and AViewInfo.IsMilesVisible then
    AScaleLineRect := cxRectCenterVertically(AScaleLineRect, AScaleThickness)
  else
    AScaleLineRect.Top := AScaleLineRect.Bottom - AScaleThickness;
  ACanvas.FillRect(AScaleLineRect, AColor);

  AStartGraduationMark := AViewInfo.ScaleBounds;
  AStartGraduationMark.Right := AStartGraduationMark.Left + AGraduationMarkThickness;
  ACanvas.FillRect(AStartGraduationMark, AColor);

  if AViewInfo.IsKilometersVisible then
  begin
    AKilometersGraduationMark := cxRectOffsetHorz(
      AStartGraduationMark, AViewInfo.KilometersLineWidth - AGraduationMarkThickness);
    AKilometersGraduationMark.Bottom := AScaleLineRect.Top;
    ACanvas.FillRect(AKilometersGraduationMark, AColor);
  end;

  if AViewInfo.IsMilesVisible then
  begin
    AMilesGraduationMark := cxRectOffsetHorz(AStartGraduationMark,
      AViewInfo.MilesLineWidth - AGraduationMarkThickness);
    if AViewInfo.IsKilometersVisible then
      AMilesGraduationMark.Top := AScaleLineRect.Bottom
    else
      AMilesGraduationMark.Bottom := AScaleLineRect.Top;
    ACanvas.FillRect(AMilesGraduationMark, AColor);
  end;
end;

procedure TdxMapControlPainter.DrawScrollButtons(ACanvas: TcxCanvas; AViewInfo: TdxMapControlScrollButtonsViewInfo);
var
  AArrowDistance: Integer;
  AColor: TdxAlphaColor;
  AColor1: TColor;
  AArrowBounds: TRect;
begin
  AArrowDistance := ScaleFactor.Apply(15);
  AColor := GetElementColorByState(AViewInfo.State);
  with TdxGPCanvas.Create(ACanvas.Handle) do
  try
    SmoothingMode := smAntiAlias;
    Ellipse(AViewInfo.Bounds, AColor, 0, 3);
  finally
    Free;
  end;

  with cxLookAndFeelPaintersManager.GetPainter(lfsFlat) do
  begin
    AArrowBounds := cxRectCenter(AViewInfo.Bounds, ViewInfo.ScaleFactor.Apply(20), ViewInfo.ScaleFactor.Apply(20));
    AArrowBounds := cxRectOffsetHorz(AArrowBounds, 1);
    AColor1 := dxAlphaColorToColor(AColor);
    DrawArrow(ACanvas, cxRectOffsetVert(AArrowBounds, -AArrowDistance), adUp, AColor1);
    DrawArrow(ACanvas, cxRectOffsetVert(AArrowBounds, AArrowDistance), adDown, AColor1);
    DrawArrow(ACanvas, cxRectOffsetHorz(AArrowBounds, -AArrowDistance), adLeft, AColor1);
    DrawArrow(ACanvas, cxRectOffsetHorz(AArrowBounds, AArrowDistance), adRight, AColor1);
  end;
end;

procedure TdxMapControlPainter.DrawSelectedRegion(ACanvas: TcxCanvas; AViewInfo: TdxMapControlSelectedRegionViewInfo);
var
  AGpGrahpics: TdxGPGraphics;
begin
  AGpGrahpics := dxGpBeginPaint(ACanvas.Handle, AViewInfo.Bounds);
  try
    AGpGrahpics.Rectangle(AViewInfo.Bounds, LookAndFeelPainter.MapControlSelectedRegionBorderColor,
      LookAndFeelPainter.MapControlSelectedRegionBackgroundColor);
  finally
    dxGpEndPaint(AGpGrahpics);
  end;
end;

procedure TdxMapControlPainter.DrawZoomTrackBarBackground(ACanvas: TcxCanvas;
  AViewInfo: TdxMapControlZoomTrackBarViewInfo);
begin
end;

procedure TdxMapControlPainter.DrawZoomTrackBarZoomButtonBackround(
  ACanvas: TcxCanvas; AViewInfo: TdxMapControlZoomTrackBarButtonViewInfo);
begin
  with TdxGPCanvas.Create(ACanvas.Handle) do
  try
    SmoothingMode := smAntiAlias;
    Ellipse(AViewInfo.DrawBounds, GetElementColorByState(AViewInfo.State), 0, 2);
  finally
    Free;
  end;
end;

procedure TdxMapControlPainter.DrawZoomTrackBarZoomButtonIcon(
  ACanvas: TcxCanvas; AViewInfo: TdxMapControlZoomTrackBarButtonViewInfo);
var
  ABounds: TRect;
  AColor: TdxAlphaColor;
begin
  ABounds := cxRectCenter(AViewInfo.Bounds, ScaleFactor.Apply(13), ScaleFactor.Apply(3));
  AColor := GetElementColorByState(AViewInfo.State);
  with TdxGPCanvas.Create(ACanvas.Handle) do
  try
    SmoothingMode := smAntiAlias;
    if AViewInfo.IsZoomOut then
      Rectangle(ABounds, AColor, AColor, ScaleFactor.Apply(1))
    else
    begin
      Rectangle(ABounds, AColor, AColor, ScaleFactor.Apply(1));
      ABounds := cxRectCenter(AViewInfo.Bounds, ScaleFactor.Apply(3), ScaleFactor.Apply(13));
      Rectangle(ABounds, AColor, AColor, ScaleFactor.Apply(1));
    end;
  finally
    Free;
  end;
end;

procedure TdxMapControlPainter.DrawZoomTrackBarScale(ACanvas: TcxCanvas;
  AViewInfo: TdxMapControlZoomTrackBarScaleViewInfo);
var
  AColor: TdxAlphaColor;
  AGpCanvas: TdxGPGraphics;
begin
  AGpCanvas := dxGpBeginPaint(ACanvas.Handle, AViewInfo.DrawBounds);
  try
    AColor := GetElementColorByState(mcesNormal);
    AGpCanvas.Rectangle(AViewInfo.DrawBounds, AColor, AColor);
  finally
    dxGpEndPaint(AGpCanvas);
  end;
end;

procedure TdxMapControlPainter.DrawZoomTrackBarThumbnail(ACanvas: TcxCanvas;
  AViewInfo: TdxMapControlZoomTrackBarThumbViewInfo);
var
  AColor: TdxAlphaColor;
  AGpCanvas: TdxGPGraphics;
begin
  AGpCanvas := dxGpBeginPaint(ACanvas.Handle, AViewInfo.DrawBounds);
  try
    AColor := GetElementColorByState(AViewInfo.State);
    AGpCanvas.Rectangle(AViewInfo.DrawBounds, AColor, AColor);
  finally
    dxGpEndPaint(AGpCanvas);
  end;
  ACanvas.ExcludeClipRect(AViewInfo.Bounds);
end;

function TdxMapControlPainter.GetElementColorByState(AState: TdxMapControlElementState): TdxAlphaColor;
var
  AStyle: TdxMapControlNavigationPanelStyle;
begin
  AStyle := GetMapControl(ViewInfo.MapControl).NavigationPanel.Style;
  case AState of
    mcesPressed:
      Result := AStyle.ElementPressedColor;
    mcesHot:
      Result := AStyle.ElementHotColor;
  else // mcesNormal
    Result := AStyle.ElementColor;
  end;
  if Result = dxacDefault then
    case AState of
      mcesPressed:
        Result := LookAndFeelPainter.MapControlPanelPressedTextColor;
      mcesHot:
        Result := LookAndFeelPainter.MapControlPanelHotTrackedTextColor;
    else // mcesNormal
      Result := LookAndFeelPainter.MapControlPanelTextColor;
    end;
end;

function TdxMapControlPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := GetMapControl(FMapControl).LookAndFeelPainter;
end;

function TdxMapControlPainter.GetScaleFactor: TdxScaleFactor;
begin
  Result := ViewInfo.ScaleFactor;
end;

function TdxMapControlPainter.GetViewInfo: TdxMapControlViewInfo;
begin
  Result := GetMapControl(FMapControl).ViewInfo as TdxMapControlViewInfo;
end;

end.
