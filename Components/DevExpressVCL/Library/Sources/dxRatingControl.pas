{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit dxRatingControl;

{$I cxVer.inc}

interface

uses
  Types, Variants, Windows, Classes, Controls, Forms, Graphics, Messages, SysUtils,
  dxCore, cxClasses, cxContainer, cxControls, cxEdit, cxExtEditConsts, cxLookAndFeelPainters,
  cxFilterControlUtils, cxGraphics, cxTextEdit, cxVariants, dxCoreClasses, dxGDIPlusClasses;

const
  dxRatingControlDefaultItemCount = 5;

type
  TdxCustomRatingControlProperties = class;
  TdxCustomRatingControlViewInfo = class;

  TdxRatingControlFillPrecision = (rcfpFull, rcfpHalf, rcfpExact);
  TdxRatingControlIndicatorStates = set of TdxRatingControlIndicatorState;

  { TdxRatingControlStyle }

  TdxRatingControlStyle = class(TcxEditStyle)
  protected
    function DefaultBorderStyle: TcxContainerBorderStyle; override;
    function DefaultHotTrack: Boolean; override;
    function DefaultTransparentBorder: Boolean; override;
  end;

  { TdxCustomRatingControlIndicatorViewInfo }

  TdxCustomRatingControlIndicatorViewInfo = class
  private
    FBounds: TRect;
    FCheckedRect: TRect;
    FClientRect: TRect;
    FHoverRect: TRect;
    FIndex: Integer;
    FRatingControlViewInfo: TdxCustomRatingControlViewInfo;
    FStates: TdxRatingControlIndicatorStates;
    FUncheckedRect: TRect;

    function GetFractionalPartHeight(AValue: Double): Integer;
    function GetFractionalPartWidth(AValue: Double): Integer;
    function GetPainter: TcxCustomLookAndFeelPainter;
  protected
    procedure CalculateCheckedRect;
    procedure CalculateHoverRect;
    procedure CalculateStates;
    procedure CalculateUncheckedRect;
    procedure DrawIndicatorRect(ACanvas: TcxCanvas; AState: TdxRatingControlIndicatorState); virtual;
    function CheckHit(X, Y: Integer): Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;

    property Bounds: TRect read FBounds;
    property RatingControlViewInfo: TdxCustomRatingControlViewInfo read FRatingControlViewInfo;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
  public
    constructor Create(ARatingControlViewInfo: TdxCustomRatingControlViewInfo); virtual;

    procedure CalculateBounds(ALeft, ATop: Integer);
    procedure CalculateRects;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    procedure Offset(DX: Integer; DY: Integer); virtual;

    property Index: Integer read FIndex write FIndex;
  end;

  TdxCustomRatingControlIndicatorViewInfos = class(TdxFastObjectList)
  private
    function GetItem(AIndex: Integer): TdxCustomRatingControlIndicatorViewInfo;
    procedure SetItem(AIndex: Integer; AValue: TdxCustomRatingControlIndicatorViewInfo);
  public
    property Items[Index: Integer]: TdxCustomRatingControlIndicatorViewInfo read GetItem write SetItem; default;
  end;

  { TdxCustomRatingControlViewInfo }

  TdxCustomRatingControlViewInfo = class(TcxCustomTextEditViewInfo)
  private
    FAllowHover: Boolean;
    FCheckedGlyph: TdxSmartImage;
    FFillPrecision: TdxRatingControlFillPrecision;
    FFocusRect: TRect;
    FGlyph: TdxSmartImage;
    FHoverRating: Double;
    FHoverGlyph: TdxSmartImage;
    FIndicators: TdxCustomRatingControlIndicatorViewInfos;
    FItemCount: Integer;
    FOrientation: TdxOrientation;
    FRating: Double;
    FRatingLastValue: Double;
    FReverseDirection: Boolean;
    FShowCaption: Boolean;
    FStep: Double;
    FStopHover: Boolean;
    FTextSize: TSize;

    procedure CreateIndicators;
    procedure DestroyIndicators;
    procedure RecreateIndicators;

    //Size
    function GetIndicatorSize: TSize;
    function GetIndicatorsHeight: Integer;
    function GetIndicatorsWidth: Integer;
    function GetRequiredHeight: Integer;
    function GetRequiredWidth: Integer;
    function GetReverseDirection: Boolean;
    procedure SetCheckedGlyph(const Value: TdxSmartImage);
    procedure SetGlyph(const Value: TdxSmartImage);
    procedure SetHoverGlyph(const Value: TdxSmartImage);
    procedure SetRating(const Value: Double);
  protected
    procedure CalculateFocusRect; virtual;
    procedure CalculateHoverRating(APoint: TPoint); virtual;
    procedure CalculateIndicatorsBounds; virtual;
    procedure CalculateIndicatorRects; virtual;
    procedure CalculateTextParams(ACanvas: TcxCanvas); virtual;
    function GetRatingByMousePos(X, Y: Integer): Double; virtual;
    function IsCaptionVisible: Boolean; virtual;
    function NeedRecreateIndicators: Boolean; virtual;

    //Draw
    procedure DrawFocusRect(ACanvas: TcxCanvas); virtual;
    procedure DrawIndicator(ACanvas: TcxCanvas; AIndex: Integer); virtual;
    procedure DrawIndicators(ACanvas: TcxCanvas); virtual;
    procedure InternalPaint(ACanvas: TcxCanvas); override;

    //Indents
    function GetIndicatorIndent: Integer; virtual;
    function GetBottomIndent: Integer; virtual;
    function GetLeftIndent: Integer; virtual;
    function GetRightIndent: Integer; virtual;
    function GetTextIndent: Integer; virtual;
    function GetTopIndent: Integer; virtual;

    property AllowHover: Boolean read FAllowHover write FAllowHover;
    property CheckedGlyph: TdxSmartImage read FCheckedGlyph write SetCheckedGlyph;
    property FillPrecision: TdxRatingControlFillPrecision read FFillPrecision write FFillPrecision;
    property FocusRect: TRect read FFocusRect write FFocusRect;
    property Glyph: TdxSmartImage read FGlyph write SetGlyph;
    property HoverRating: Double read FHoverRating write FHoverRating;
    property HoverGlyph: TdxSmartImage read FHoverGlyph write SetHoverGlyph;
    property ItemCount: Integer read FItemCount write FItemCount;
    property Indicators: TdxCustomRatingControlIndicatorViewInfos read FIndicators;
    property IndicatorSize: TSize read GetIndicatorSize;
    property Orientation: TdxOrientation read FOrientation write FOrientation;
    property Rating: Double read FRating write SetRating;
    property RatingLastValue: Double read FRatingLastValue write FRatingLastValue;
    property RequiredHeight: Integer read GetRequiredHeight;
    property RequiredWidth: Integer read GetRequiredWidth;
    property ReverseDirection: Boolean read GetReverseDirection write FReverseDirection;
    property ShowCaption: Boolean read FShowCaption write FShowCaption;
    property StopHover: Boolean read FStopHover write FStopHover;
    property Step: Double read FStep write FStep;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TObject); override;

    procedure Calculate(ACanvas: TcxCanvas; APoint: TPoint); virtual;
    procedure Offset(DX: Integer; DY: Integer); override;
    function Repaint(AControl: TWinControl; const AInnerEditRect: TRect;
      AViewInfo: TcxContainerViewInfo = nil): Boolean; override;
  end;

  { TdxCustomRatingControlViewData }

  TdxCustomRatingControlViewData = class(TcxCustomEditViewData)
  private
    function GetProperties: TdxCustomRatingControlProperties;
  protected
    procedure CalculateViewInfoByProperties(AViewInfo: TcxCustomEditViewInfo); virtual;
    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas;
      AIsInplace: Boolean; AEditSizeProperties: TcxEditSizeProperties;
      var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize; override;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect;
      const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
      AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    procedure CalculateButtonsViewInfo(ACanvas: TcxCanvas; const ABounds: TRect;
      const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
      AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: Variant; AViewInfo: TcxCustomEditViewInfo); override;

    property Properties: TdxCustomRatingControlProperties read GetProperties;
  end;

  { TdxCustomRatingControlProperties }

  TdxCustomRatingControlProperties = class(TcxCustomEditProperties)
  strict private
    FAllowHover: Boolean;
    FCheckedGlyph: TdxSmartGlyph;
    FFillPrecision: TdxRatingControlFillPrecision;
    FGlyph: TdxSmartGlyph;
    FHoverGlyph: TdxSmartGlyph;
    FItemCount: Integer;
    FOrientation: TdxOrientation;
    FReverseDirection: Boolean;
    FShowCaption: Boolean;
    FShowEndEllipsis: Boolean;
    FStep: Double;

    function CreateGlyph: TdxSmartGlyph;
    function GetStepStored: Boolean;
    procedure SetAllowHover(AValue: Boolean);
    procedure SetCheckedGlyph(AValue: TdxSmartGlyph);
    procedure SetFillPrecision(AValue: TdxRatingControlFillPrecision);
    procedure SetGlyph(AValue: TdxSmartGlyph);
    procedure SetHoverGlyph(AValue: TdxSmartGlyph);
    procedure SetItemCount(AValue: Integer);
    procedure SetOrientation(AValue: TdxOrientation);
    procedure SetReverseDirection(AValue: Boolean);
    procedure SetShowCaption(AValue: Boolean);
    procedure SetShowEndEllipsis(AValue: Boolean);
    procedure SetStep(AValue: Double);
  protected
    function CanValidate: Boolean; override;
    function ChangeEditValueByStep(const AEditValue: Variant; AIsInc: Boolean): Variant; virtual;
    function CorrectRatingByRange(AValue: Double): Double; virtual;
    procedure CorrectViewInfoRating(AViewInfo: TdxCustomRatingControlViewInfo);
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    function GetCorrectedRating(ARating: Double): Double;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function HasDisplayValue: Boolean; override;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default False; //move to published part if need suddenly
    property ShowEndEllipsis: Boolean read FShowEndEllipsis write SetShowEndEllipsis default False; //move to published part if need suddenly
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure DoPrepareDisplayValue(const AEditValue: Variant; var DisplayValue: Variant; AEditFocused: Boolean); virtual;
    function CanCompareEditValue: Boolean; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: Variant; AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    class function GetStyleClass: TcxCustomEditStyleClass; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function IsResetEditClass: Boolean; override;
    procedure PrepareDisplayValue(const AEditValue: Variant; var DisplayValue: Variant; AEditFocused: Boolean); override;

    property AllowHover: Boolean read FAllowHover write SetAllowHover default True;
    property CheckedGlyph: TdxSmartGlyph read FCheckedGlyph write SetCheckedGlyph;
    property FillPrecision: TdxRatingControlFillPrecision read FFillPrecision write SetFillPrecision default rcfpFull;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property HoverGlyph: TdxSmartGlyph read FHoverGlyph write SetHoverGlyph;
    property ItemCount: Integer read FItemCount write SetItemCount default dxRatingControlDefaultItemCount;
    property Orientation: TdxOrientation read FOrientation write SetOrientation default orHorizontal;
    property ReverseDirection: Boolean read FReverseDirection write SetReverseDirection default False;
    property Step: Double read FStep write SetStep stored GetStepStored;
  end;

  { TdxRatingControlProperties }

  TdxRatingControlProperties = class(TdxCustomRatingControlProperties)
  published
    property AllowHover;
    property AssignedValues;
    property ClearKey;
    property CheckedGlyph;
    property FillPrecision;
    property Glyph;
    property HoverGlyph;
    property ImmediatePost;
    property ItemCount;
    property Orientation;
    property ReadOnly;
    property ReverseDirection;
    property Step;
    property OnChange;
    property OnEditValueChanged;
  end;

  { TdxCustomRatingControl }

  TdxCustomRatingControl = class(TcxCustomEdit)
  strict private
    function GetProperties: TdxCustomRatingControlProperties;
    function GetRating: Double;
    function GetRatingStored: Boolean;
    function GetStyle: TdxRatingControlStyle;
    function GetActiveProperties: TdxCustomRatingControlProperties;
    function GetViewInfo: TdxCustomRatingControlViewInfo;
    procedure SetProperties(Value: TdxCustomRatingControlProperties);
    procedure SetRating(AValue: Double);
    procedure SetStyle(AValue: TdxRatingControlStyle);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    function CanAutoWidth: Boolean; override;
    function DefaultParentColor: Boolean; override;
    function FadingCanFadeBackground: Boolean; override;
    function GetDisplayText: string; override;
    procedure Initialize; override;
    procedure InternalSetRating(AValue: Double; ANeedResetEqualsValue: Boolean = True); virtual;
    function GetEditStateColorKind: TcxEditStateColorKind; override;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure SynchronizeDisplayValue; override;
    procedure TextChanged; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;Y: Integer); override;
    function WantNavigationKeys: Boolean; override;

    property ViewInfo: TdxCustomRatingControlViewInfo read GetViewInfo;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;

    property ActiveProperties: TdxCustomRatingControlProperties read GetActiveProperties;
    property Properties: TdxCustomRatingControlProperties read GetProperties write SetProperties;
    property Rating: Double read GetRating write SetRating stored GetRatingStored;
    property Style: TdxRatingControlStyle read GetStyle write SetStyle;
  end;

  { TdxRatingControl }

  TdxRatingControl = class(TdxCustomRatingControl)
  strict private
    function GetActiveProperties: TdxRatingControlProperties;
    function GetProperties: TdxRatingControlProperties;
    procedure SetProperties(Value: TdxRatingControlProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TdxRatingControlProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TdxRatingControlProperties read GetProperties write SetProperties;
    property Rating;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Math, cxEditConsts, cxDrawTextUtils, cxEditUtils, cxExtEditUtils,
  cxSpinEdit, dxThemeConsts, dxThemeManager, dxUxTheme, cxGeometry, dxOffice11,
  cxLookAndFeels, dxCoreGraphics, RTLConsts;

const
  DoubleResolution = 1E-10;

type
{ TdxFilterRatingControlHelper }

  TdxFilterRatingControlHelper = class(TcxFilterSpinEditHelper)
  public
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
  end;

function AdjustRatingByStep(ARating: Double; AStep: Double; ARoundingUp: Boolean = True): Double;
var
  AStepCount: Double;
begin
  Result := ARating;
  AStepCount := Result / AStep;
  if CompareValue(Round(AStepCount), AStepCount, DoubleResolution) <> 0 then
    if ARoundingUp then
      Result := Ceil(AStepCount) * AStep
    else
      Result := Floor(AStepCount) * AStep;
  Result := RoundTo(Result, -10);
end;

{ TdxFilterRatingControlHelper }

class procedure TdxFilterRatingControlHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
var
  ASpinProperties: TcxCustomSpinEditProperties;
  ARatingProperties: TdxCustomRatingControlProperties;
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  ASpinProperties := TcxCustomSpinEditProperties(AProperties);
  ARatingProperties := TdxCustomRatingControlProperties(AEditProperties);
  ASpinProperties.Buttons.Add;
  ASpinProperties.Buttons.Add;
  ASpinProperties.MinValue := ARatingProperties.MinValue;
  ASpinProperties.MaxValue := ARatingProperties.MaxValue;
end;

{ TdxRatingControlStyle }

function TdxRatingControlStyle.DefaultBorderStyle: TcxContainerBorderStyle;
begin
  if IsBaseStyle then
    Result := cbsNone
  else
    Result := inherited DefaultBorderStyle;
end;

function TdxRatingControlStyle.DefaultHotTrack: Boolean;
begin
  Result := False;
end;

function TdxRatingControlStyle.DefaultTransparentBorder: Boolean;
begin
  Result := False;
end;

{ TdxCustomRatingControlIndicatorViewInfo }

constructor TdxCustomRatingControlIndicatorViewInfo.Create(
  ARatingControlViewInfo: TdxCustomRatingControlViewInfo);
begin
  inherited Create;
  FRatingControlViewInfo := ARatingControlViewInfo;
end;

procedure TdxCustomRatingControlIndicatorViewInfo.CalculateRects;
begin
  CalculateStates;
  CalculateHoverRect;
  CalculateCheckedRect;
  CalculateUncheckedRect;
end;

procedure TdxCustomRatingControlIndicatorViewInfo.Draw(ACanvas: TcxCanvas);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(Bounds);
    if rcisUnchecked in FStates then
      DrawIndicatorRect(ACanvas, rcisUnchecked);
    if rcisChecked in FStates then
      DrawIndicatorRect(ACanvas, rcisChecked);
    if rcisHover in FStates then
      DrawIndicatorRect(ACanvas, rcisHover);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxCustomRatingControlIndicatorViewInfo.Offset(DX: Integer; DY: Integer);
begin
  OffsetRect(FBounds, DX, DY);
end;

procedure TdxCustomRatingControlIndicatorViewInfo.CalculateBounds(ALeft, ATop: Integer);
begin
  FClientRect := Rect(0, 0, GetWidth, GetHeight);
  FBounds := cxRectOffset(FClientRect, ALeft, ATop);
end;

procedure TdxCustomRatingControlIndicatorViewInfo.CalculateCheckedRect;
var
  AFractionalPartSize: Integer;
begin
  if not (rcisChecked in FStates) then
  begin
    FCheckedRect := cxEmptyRect;
    Exit;
  end;
  FCheckedRect := FClientRect;
  if rcisUnchecked in FStates then
  begin
    if RatingControlViewInfo.Orientation = orHorizontal then
    begin
      AFractionalPartSize := GetFractionalPartWidth(RatingControlViewInfo.Rating);
      if not RatingControlViewInfo.ReverseDirection then
        FCheckedRect.Right := FCheckedRect.Left + AFractionalPartSize
      else
        FCheckedRect.Left := FCheckedRect.Right - AFractionalPartSize;
    end
    else
    begin
      AFractionalPartSize := GetFractionalPartHeight(RatingControlViewInfo.Rating);
      if not RatingControlViewInfo.ReverseDirection then
        FCheckedRect.Top := FCheckedRect.Bottom - AFractionalPartSize
      else
        FCheckedRect.Bottom := FCheckedRect.Top + AFractionalPartSize;
    end;
  end;
  if rcisHover in FStates then
    if RatingControlViewInfo.Orientation = orHorizontal then
      if not RatingControlViewInfo.ReverseDirection then
        FCheckedRect.Left := Max(FHoverRect.Right, FCheckedRect.Left)
      else
        FCheckedRect.Right := Min(FHoverRect.Left, FCheckedRect.Right)
    else
      if not RatingControlViewInfo.ReverseDirection then
        FCheckedRect.Bottom := Min(FHoverRect.Top, FCheckedRect.Bottom)
      else
        FCheckedRect.Top := Max(FHoverRect.Bottom, FCheckedRect.Top);
end;

procedure TdxCustomRatingControlIndicatorViewInfo.CalculateHoverRect;
var
  AFractionalPartSize: Integer;
begin
  if not (rcisHover in FStates) then
  begin
    FHoverRect := cxEmptyRect;
    Exit;
  end;
  FHoverRect := FClientRect;
  if FStates - [rcisChecked, rcisUnchecked] <> FStates then
  begin
    if RatingControlViewInfo.Orientation = orHorizontal then
    begin
      AFractionalPartSize := GetFractionalPartWidth(RatingControlViewInfo.HoverRating);
      if not RatingControlViewInfo.ReverseDirection then
        FHoverRect.Right := FHoverRect.Left + AFractionalPartSize
      else
        FHoverRect.Left := FHoverRect.Right - AFractionalPartSize;
    end
    else
    begin
      AFractionalPartSize := GetFractionalPartHeight(RatingControlViewInfo.HoverRating);
      if not RatingControlViewInfo.ReverseDirection then
        FHoverRect.Top := FHoverRect.Bottom - AFractionalPartSize
      else
        FHoverRect.Bottom := FHoverRect.Top + AFractionalPartSize;
    end
  end;
end;

procedure TdxCustomRatingControlIndicatorViewInfo.CalculateStates;
var
  AHoverRating, ARating: Double;
begin
  FStates := [];
  ARating := RatingControlViewInfo.Rating;
  if CompareValue(ARating, Index + 1, DoubleResolution) >= 0 then
    FStates := [rcisChecked]
  else
    if (Floor(ARating) = Index) and (CompareValue(Round(ARating), ARating, DoubleResolution) <> 0) then
      FStates := [rcisChecked, rcisUnchecked]
    else
      FStates := [rcisUnchecked];

  AHoverRating := RatingControlViewInfo.HoverRating;
  if CompareValue(AHoverRating, Index + 1, DoubleResolution) >= 0 then
    FStates := [rcisHover]
  else
    if (Floor(AHoverRating) = Index) and (CompareValue(Round(AHoverRating), AHoverRating, DoubleResolution) <> 0) then
    begin
      FStates := FStates + [rcisHover];
      if CompareValue(AHoverRating, ARating, DoubleResolution) >= 0 then
        FStates := FStates - [rcisChecked];
    end;
end;

procedure TdxCustomRatingControlIndicatorViewInfo.CalculateUncheckedRect;
var
  ANeighborRect: TRect;
begin
  if not (rcisUnchecked in FStates) then
  begin
    FUncheckedRect := cxEmptyRect;
    Exit;
  end;
  FUncheckedRect := FClientRect;
  if FStates - [rcisChecked, rcisHover] <> FStates then
  begin
    if rcisChecked in FStates then
      ANeighborRect := FCheckedRect
    else
      ANeighborRect := FHoverRect;
    if RatingControlViewInfo.Orientation = orHorizontal then
      if not RatingControlViewInfo.ReverseDirection then
        FUncheckedRect.Left := ANeighborRect.Right
      else
        FUncheckedRect.Right := ANeighborRect.Left
    else
      if not RatingControlViewInfo.ReverseDirection then
        FUncheckedRect.Bottom := ANeighborRect.Top
      else
        FUncheckedRect.Top := ANeighborRect.Bottom;
  end;
end;

procedure TdxCustomRatingControlIndicatorViewInfo.DrawIndicatorRect(ACanvas: TcxCanvas; AState: TdxRatingControlIndicatorState);
var
  AGlyph: TdxSmartImage;
  ARect: TRect;
begin
  case AState of
    rcisChecked:
      begin
        AGlyph := RatingControlViewInfo.CheckedGlyph;
        ARect := FCheckedRect;
      end;
    rcisUnchecked:
      begin
        AGlyph := RatingControlViewInfo.Glyph;
        ARect := FUncheckedRect;
      end
  else //rcisHover
    begin
      AGlyph := RatingControlViewInfo.HoverGlyph;
      ARect := FHoverRect;
    end;
  end;

  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(cxRectOffset(ARect, Bounds.Left, Bounds.Top));
    if AGlyph.Empty then
    begin
      Painter.DrawRatingControlScaledIndicator(ACanvas,
        cxRectCenter(Bounds, Painter.GetRatingControlScaledIndicatorSize(RatingControlViewInfo.ScaleFactor)),
        AState, RatingControlViewInfo.ScaleFactor);
    end
    else
      cxDrawImage(ACanvas,
        cxRectCenter(Bounds, dxGetImageSize(AGlyph, RatingControlViewInfo.ScaleFactor)), AGlyph, ifmNormal,
        Painter.GetRatingControlIndicatorColorPalette(AState), RatingControlViewInfo.ScaleFactor);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TdxCustomRatingControlIndicatorViewInfo.CheckHit(X, Y: Integer): Boolean;
begin
  Result := PtInRect(Bounds, Point(X, Y));
end;

function TdxCustomRatingControlIndicatorViewInfo.GetHeight: Integer;
begin
  Result := RatingControlViewInfo.IndicatorSize.cy;
end;

function TdxCustomRatingControlIndicatorViewInfo.GetWidth: Integer;
begin
  Result := RatingControlViewInfo.IndicatorSize.cx;
end;

function TdxCustomRatingControlIndicatorViewInfo.GetFractionalPartHeight(AValue: Double): Integer;
begin
  Result := Round(GetHeight * Frac(AValue));
end;

function TdxCustomRatingControlIndicatorViewInfo.GetFractionalPartWidth(AValue: Double): Integer;
begin
  Result := Round(GetWidth * Frac(AValue));
end;

function TdxCustomRatingControlIndicatorViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := RatingControlViewInfo.Painter;
end;

{ TdxCustomRatingControlIndicatorViewInfos }

function TdxCustomRatingControlIndicatorViewInfos.GetItem(AIndex: Integer): TdxCustomRatingControlIndicatorViewInfo;
begin
  Result := TdxCustomRatingControlIndicatorViewInfo(inherited Items[AIndex]);
end;

procedure TdxCustomRatingControlIndicatorViewInfos.SetItem(AIndex: Integer; AValue: TdxCustomRatingControlIndicatorViewInfo);
begin
  inherited Items[AIndex] := AValue;
end;

{ TdxCustomRatingControlViewInfo }

constructor TdxCustomRatingControlViewInfo.Create;
begin
  inherited Create;
  FIndicators := TdxCustomRatingControlIndicatorViewInfos.Create;
  FGlyph := TdxSmartImage.Create;
  FCheckedGlyph := TdxSmartImage.Create;
  FHoverGlyph := TdxSmartImage.Create;
end;

destructor TdxCustomRatingControlViewInfo.Destroy;
begin
  FreeAndNil(FHoverGlyph);
  FreeAndNil(FCheckedGlyph);
  FreeAndNil(FGlyph);
  FreeAndNil(FIndicators);
  inherited Destroy;
end;

procedure TdxCustomRatingControlViewInfo.Assign(Source: TObject);
var
  ASourceViewInfo: TdxCustomRatingControlViewInfo;
begin
  inherited Assign(Source);
  if Source is TdxCustomRatingControlViewInfo then
  begin
    ASourceViewInfo := TdxCustomRatingControlViewInfo(Source);
    AllowHover := ASourceViewInfo.AllowHover;
    CheckedGlyph := ASourceViewInfo.CheckedGlyph;
    HoverGlyph := ASourceViewInfo.HoverGlyph;
    Glyph := ASourceViewInfo.Glyph;
    ReverseDirection := ASourceViewInfo.ReverseDirection;
    FillPrecision := ASourceViewInfo.FillPrecision;
    ItemCount := ASourceViewInfo.ItemCount;
    Orientation := ASourceViewInfo.Orientation;
    Rating := ASourceViewInfo.Rating;
    RatingLastValue := ASourceViewInfo.RatingLastValue;
    ShowCaption := ASourceViewInfo.ShowCaption;
    Step := ASourceViewInfo.Step;
    HoverRating := ASourceViewInfo.HoverRating;
    UseRightToLeftAlignment := ASourceViewInfo.UseRightToLeftAlignment;
  end;
end;

procedure TdxCustomRatingControlViewInfo.Calculate(ACanvas: TcxCanvas; APoint: TPoint);
begin
  CalculateTextParams(ACanvas);
  CalculateFocusRect;
  if NeedRecreateIndicators then
    RecreateIndicators;
  CalculateIndicatorsBounds;
  CalculateHoverRating(APoint);
  CalculateIndicatorRects;
end;

procedure TdxCustomRatingControlViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  inherited Offset(DX, DY);
  for I := 0 to Indicators.Count - 1 do
    Indicators[I].Offset(DX, DY);
end;

function TdxCustomRatingControlViewInfo.Repaint(AControl: TWinControl;
  const AInnerEditRect: TRect; AViewInfo: TcxContainerViewInfo = nil): Boolean;
var
  ARatingControlViewInfo: TdxCustomRatingControlViewInfo;
begin
  Result := AControl.HandleAllocated;
  if not Result then
    Exit;
  Result := inherited Repaint(AControl, AInnerEditRect, AViewInfo);
  ARatingControlViewInfo := AViewInfo as TdxCustomRatingControlViewInfo;
  if (ARatingControlViewInfo = nil) or
    (CompareValue(HoverRating, ARatingControlViewInfo.HoverRating, DoubleResolution) <> 0) then
  begin
    cxRedrawWindow(AControl.Handle, InnerEditRect, False);
    Result := True;
  end;
end;

procedure TdxCustomRatingControlViewInfo.CalculateFocusRect;
begin
  if not Focused or IsInplace then
    FocusRect := cxEmptyRect
  else
    if ShowCaption then
      FocusRect := cxRectInflate(TextRect, 1, 1)
    else
      FocusRect := cxRectInflate(InnerEditRect, -1, -1);
end;

procedure TdxCustomRatingControlViewInfo.CalculateHoverRating(APoint: TPoint);
begin
  if not IsDesigning and AllowHover then
  begin
    HoverRating := GetRatingByMousePos(APoint.X, APoint.Y);
    if StopHover then
      if (CompareValue(HoverRating, Rating, DoubleResolution) = 0) or
        (CompareValue(HoverRating, RatingLastValue, DoubleResolution) = 0) then
        HoverRating := 0
      else
        StopHover := False;
  end
  else
    HoverRating := 0;
end;

procedure TdxCustomRatingControlViewInfo.CalculateIndicatorsBounds;
var
  I, AIndex: Integer;
  APosX, APosY: Integer;
  AIndicator: TdxCustomRatingControlIndicatorViewInfo;
  ARect: TRect;
  AWidth, AHeight: Integer;
begin
  AWidth := Min(RequiredWidth, cxRectWidth(InnerEditRect));
  AHeight := Min(RequiredHeight, cxRectHeight(InnerEditRect));
  ARect := cxRectCenter(InnerEditRect, AWidth, AHeight);
  if not IsInplace then
    ARect := cxRectInflate(ARect, - GetLeftIndent, - GetTopIndent, - GetRightIndent, - GetBottomIndent);
  if IsCaptionVisible and not IsRectEmpty(TextRect) then
  begin
    ARect.Left := ARect.Left + cxRectWidth(TextRect) + GetTextIndent;
    if GetIndicatorsHeight < cxRectHeight(ARect) then
      ARect := cxRectCenterVertically(ARect, GetIndicatorsHeight);
  end;
  APosX := ARect.Left;
  APosY := ARect.Top;
  for I := 0 to ItemCount - 1 do
  begin
    if not ReverseDirection and (Orientation = orHorizontal) or
      ReverseDirection and (Orientation = orVertical) then
      AIndex := I
    else
      AIndex := ItemCount - I - 1;
    AIndicator := Indicators[AIndex];
    AIndicator.CalculateBounds(APosX, APosY);
    if Orientation = orHorizontal then
      APosX := APosX + GetIndicatorIndent + IndicatorSize.cx
    else
      APosY := APosY + GetIndicatorIndent + IndicatorSize.cy;
  end;
end;

procedure TdxCustomRatingControlViewInfo.CalculateIndicatorRects;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    Indicators[I].CalculateRects;
end;

procedure TdxCustomRatingControlViewInfo.CalculateTextParams(ACanvas: TcxCanvas);
var
  ARect: TRect;
begin
  if IsCaptionVisible then
  begin
    FTextSize.cx := ACanvas.TextWidth(Text);
    FTextSize.cy := ACanvas.TextHeight(Text);
    ARect := InnerEditRect;
    if RequiredWidth <= cxRectWidth(ARect) then
    begin
      ARect := cxRectCenterHorizontally(ARect, RequiredWidth);
      ARect := cxRectInflate(ARect, - GetLeftIndent, 0, - GetRightIndent, 0);
      ARect.Right := ARect.Left + FTextSize.cx;
    end
    else
    begin
      ARect := cxRectInflate(ARect, - GetLeftIndent, 0, - GetRightIndent, 0);
      ARect.Right := ARect.Right - GetIndicatorsWidth - GetTextIndent;
    end;
    if RequiredHeight <= cxRectHeight(ARect) then
    begin
      ARect := cxRectCenterVertically(ARect, RequiredHeight);
      ARect := cxRectInflate(ARect, 0, - GetTopIndent, 0, - GetBottomIndent);
      ARect := cxRectCenterVertically(ARect, FTextSize.cy);
    end
    else
    begin
      if cxRectHeight(ARect) > FTextSize.cy then
        ARect := cxRectCenterVertically(ARect, FTextSize.cy);
      Inc(ARect.Top, GetTopIndent);
    end;
    if (ARect.Right < ARect.Left) or (ARect.Bottom < ARect.Top)  then
      ARect := cxEmptyRect;
    TextRect := ARect;
  end
  else
    TextRect := cxEmptyRect;
  TextOutData.Initialized := False;
end;

function TdxCustomRatingControlViewInfo.GetRatingByMousePos(X, Y: Integer): Double;

  function CalculateRatingByIndicator(AIndicator: TdxCustomRatingControlIndicatorViewInfo; X, Y: Integer): Double;
  var
    AFractionalValue: Double;
  begin
    if Orientation = orHorizontal then
      if not ReverseDirection then
        AFractionalValue := (X - AIndicator.Bounds.Left) / AIndicator.GetWidth
      else
        AFractionalValue := (AIndicator.Bounds.Right - X) / AIndicator.GetWidth
    else
      if not ReverseDirection then
        AFractionalValue := (AIndicator.Bounds.Bottom - Y) / AIndicator.GetHeight
      else
        AFractionalValue := (Y - AIndicator.Bounds.Top) / AIndicator.GetHeight;
    Result := AdjustRatingByStep(AIndicator.Index + AFractionalValue, Step);
  end;

  function GetIndicatorBoundsWithIndent(AIndicator: TdxCustomRatingControlIndicatorViewInfo): TRect;
  begin
    Result := AIndicator.Bounds;
    if Orientation = orHorizontal then
      if not ReverseDirection then
        Result := cxRectInflate(Result, 0, 0, GetIndicatorIndent, 0)
      else
        Result := cxRectInflate(Result, GetIndicatorIndent, 0, 0, 0)
    else
      if not ReverseDirection then
        Result := cxRectInflate(Result, 0, GetIndicatorIndent, 0, 0)
      else
        Result := cxRectInflate(Result, 0, 0, 0, GetIndicatorIndent);
  end;

var
  I: Integer;
  ABoundsWithIndent: TRect;
  AIndicator: TdxCustomRatingControlIndicatorViewInfo;
begin
  Result := 0;
  for I := 0 to ItemCount - 1 do
  begin
    AIndicator := Indicators[I];
    ABoundsWithIndent := GetIndicatorBoundsWithIndent(AIndicator);
    if PtInRect(ABoundsWithIndent, Point(X, Y)) then
    begin
      if AIndicator.CheckHit(X, Y) then
        Result := CalculateRatingByIndicator(AIndicator, X, Y)
      else
        Result := I + 1;
      Exit;
    end;
  end;
end;

function TdxCustomRatingControlViewInfo.IsCaptionVisible: Boolean;
begin
  Result := not IsInplace and ShowCaption and (Text <> '');
end;

function TdxCustomRatingControlViewInfo.NeedRecreateIndicators: Boolean;
begin
  Result := ItemCount <> FIndicators.Count;
end;

procedure TdxCustomRatingControlViewInfo.DrawFocusRect(ACanvas: TcxCanvas);
begin
  if not cxRectIsEmpty(FocusRect) then
    ACanvas.DrawFocusRect(FocusRect);
end;

procedure TdxCustomRatingControlViewInfo.DrawIndicator(ACanvas: TcxCanvas; AIndex: Integer);
begin
  Indicators[AIndex].Draw(ACanvas);
end;

procedure TdxCustomRatingControlViewInfo.DrawIndicators(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(InnerEditRect);
    for I := 0 to ItemCount - 1 do
      DrawIndicator(ACanvas, I);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxCustomRatingControlViewInfo.InternalPaint(ACanvas: TcxCanvas);
var
  ABufferBitmap: TcxBitmap32;
  ABufferCanvas: TcxCanvas;
begin
  ABufferBitmap := TcxBitmap32.CreateSize(Bounds);
  try
    ABufferCanvas := ABufferBitmap.cxCanvas;
    cxBitBlt(ABufferCanvas.Handle, ACanvas.Handle, ABufferBitmap.ClientRect, Bounds.TopLeft, SRCCOPY);
    ABufferCanvas.WindowOrg := Bounds.TopLeft;
    DrawEditBackground(ABufferCanvas, Bounds, cxNullRect, False);
    DrawText(ABufferCanvas);
    DrawFocusRect(ABufferCanvas);
    DrawCustomEditValidationMark(ABufferCanvas);
    DrawIndicators(ABufferCanvas);
    ABufferCanvas.WindowOrg := cxNullPoint;
    cxBitBlt(ACanvas.Handle, ABufferCanvas.Handle, Bounds, cxNullPoint, SRCCOPY);
  finally
    ABufferBitmap.Free;
  end;
end;

function TdxCustomRatingControlViewInfo.GetIndicatorIndent: Integer;
begin
  Result := 2;
end;

function TdxCustomRatingControlViewInfo.GetBottomIndent: Integer;
begin
  Result := 2;
end;

function TdxCustomRatingControlViewInfo.GetLeftIndent: Integer;
begin
  Result := 2
end;

function TdxCustomRatingControlViewInfo.GetRightIndent: Integer;
begin
  Result := 2;
end;

function TdxCustomRatingControlViewInfo.GetTextIndent: Integer;
begin
  Result := 5;
end;

function TdxCustomRatingControlViewInfo.GetTopIndent: Integer;
begin
  Result := 2;
end;

procedure TdxCustomRatingControlViewInfo.CreateIndicators;
var
  I: Integer;
  AIndicator: TdxCustomRatingControlIndicatorViewInfo;
begin
  for I := 0 to ItemCount - 1 do
  begin
    AIndicator := TdxCustomRatingControlIndicatorViewInfo.Create(Self);
    AIndicator.Index := I;
    FIndicators.Add(AIndicator);
  end;
end;

procedure TdxCustomRatingControlViewInfo.DestroyIndicators;
begin
  FIndicators.Clear;
end;

procedure TdxCustomRatingControlViewInfo.RecreateIndicators;
begin
  DestroyIndicators;
  CreateIndicators;
end;

function TdxCustomRatingControlViewInfo.GetIndicatorSize: TSize;
begin
  Result := dxGetImageSize(Glyph, ScaleFactor);
  Result := cxSizeMax(Result, dxGetImageSize(HoverGlyph, ScaleFactor));
  Result := cxSizeMax(Result, dxGetImageSize(CheckedGlyph, ScaleFactor));
  if (Painter <> nil) and (Glyph.Empty or HoverGlyph.Empty or CheckedGlyph.Empty) then
    Result := cxSizeMax(Result, Painter.GetRatingControlScaledIndicatorSize(ScaleFactor));
end;

function TdxCustomRatingControlViewInfo.GetIndicatorsHeight: Integer;
begin
  if Orientation = orHorizontal then
    Result := IndicatorSize.cy
  else
    Result := ItemCount * IndicatorSize.cy + (ItemCount - 1) * GetIndicatorIndent;
end;

function TdxCustomRatingControlViewInfo.GetIndicatorsWidth: Integer;
begin
  if Orientation = orHorizontal then
    Result := ItemCount * IndicatorSize.cx + (ItemCount - 1) * GetIndicatorIndent
  else
    Result := IndicatorSize.cx;
end;

function TdxCustomRatingControlViewInfo.GetRequiredHeight: Integer;
begin
  Result := GetIndicatorsHeight;
  if IsCaptionVisible then
    Result := Max(Result, FTextSize.cy);
  if not IsInplace then
    Inc(Result, GetBottomIndent + GetTopIndent);
end;

function TdxCustomRatingControlViewInfo.GetRequiredWidth: Integer;
begin
  Result := GetIndicatorsWidth;
  if not IsInplace then
    Inc(Result, GetLeftIndent + GetRightIndent);
  if IsCaptionVisible then
    Result := Result + FTextSize.cx + GetTextIndent;
end;

function TdxCustomRatingControlViewInfo.GetReverseDirection: Boolean;
begin
  Result := FReverseDirection;
  if UseRightToLeftAlignment and (Orientation = orHorizontal) then
    Result := not Result;
end;

procedure TdxCustomRatingControlViewInfo.SetCheckedGlyph(const Value: TdxSmartImage);
begin
  FCheckedGlyph.Assign(Value);
end;

procedure TdxCustomRatingControlViewInfo.SetGlyph(const Value: TdxSmartImage);
begin
  FGlyph.Assign(Value);
end;

procedure TdxCustomRatingControlViewInfo.SetHoverGlyph(const Value: TdxSmartImage);
begin
  FHoverGlyph.Assign(Value);
end;

procedure TdxCustomRatingControlViewInfo.SetRating(const Value: Double);
begin
  if FRating <> Value then
  begin
    RatingLastValue := FRating;
    FRating := Value;
  end;
end;

{ TdxCustomRatingControlViewData }

procedure TdxCustomRatingControlViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  ARatingControlViewInfo: TdxCustomRatingControlViewInfo;
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  AdjustCanvasFont(ACanvas.Canvas, Style.GetVisibleFont, 0);
  CalculateViewInfoByProperties(AViewInfo);
  ARatingControlViewInfo := TdxCustomRatingControlViewInfo(AViewInfo);
  ARatingControlViewInfo.Calculate(ACanvas, P);
end;

procedure TdxCustomRatingControlViewData.CalculateButtonsViewInfo(
  ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
  AIsMouseEvent: Boolean);
begin
end;

procedure TdxCustomRatingControlViewData.EditValueToDrawValue(
  const AEditValue: Variant; AViewInfo: TcxCustomEditViewInfo);
var
  ADisplayValue: TcxEditValue;
begin
  Properties.PrepareDisplayValue(AEditValue, ADisplayValue, InternalFocused);
  TdxCustomRatingControlViewInfo(AViewInfo).Rating := ADisplayValue;
end;

procedure TdxCustomRatingControlViewData.CalculateViewInfoByProperties(
  AViewInfo: TcxCustomEditViewInfo);
var
  ARatingControlViewInfo: TdxCustomRatingControlViewInfo;
begin
  ARatingControlViewInfo := TdxCustomRatingControlViewInfo(AViewInfo);
  ARatingControlViewInfo.AllowHover := Properties.AllowHover;
  ARatingControlViewInfo.CheckedGlyph := Properties.CheckedGlyph;
  ARatingControlViewInfo.ReverseDirection := Properties.ReverseDirection;
  ARatingControlViewInfo.FillPrecision := Properties.FillPrecision;
  ARatingControlViewInfo.Glyph := Properties.Glyph;
  ARatingControlViewInfo.HoverGlyph := Properties.HoverGlyph;
  ARatingControlViewInfo.ItemCount := Properties.ItemCount;
  ARatingControlViewInfo.Orientation := Properties.Orientation;
  ARatingControlViewInfo.ShowCaption := Properties.ShowCaption;
  if Properties.ShowEndEllipsis then
    ARatingControlViewInfo.DrawTextFlags := ARatingControlViewInfo.DrawTextFlags or CXTO_END_ELLIPSIS
  else
    ARatingControlViewInfo.DrawTextFlags := ARatingControlViewInfo.DrawTextFlags and not CXTO_END_ELLIPSIS;
  ARatingControlViewInfo.Step := Properties.Step;
  if Edit <> nil then
  begin
    ARatingControlViewInfo.Text := TdxRatingControl(Edit).Caption;
    ARatingControlViewInfo.UseRightToLeftAlignment := TdxRatingControl(Edit).UseRightToLeftAlignment;
  end;
end;

function TdxCustomRatingControlViewData.InternalGetEditConstantPartSize(ACanvas: TcxCanvas;
  AIsInplace: Boolean; AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize;
  AViewInfo: TcxCustomEditViewInfo): TSize;
var
  ARatingControlViewInfo: TdxCustomRatingControlViewInfo;
begin
  ACanvas.SaveState;
  try
    Result := inherited InternalGetEditConstantPartSize(ACanvas,
      AIsInplace, AEditSizeProperties, MinContentSize, AViewInfo);
    ARatingControlViewInfo := TdxCustomRatingControlViewInfo(AViewInfo);
    Result.cx := Result.cx + ARatingControlViewInfo.RequiredWidth;
    Result.cy := Result.cy + ARatingControlViewInfo.RequiredHeight;
  finally
    ACanvas.RestoreState;
  end;
end;

function TdxCustomRatingControlViewData.GetProperties: TdxCustomRatingControlProperties;
begin
  Result := TdxCustomRatingControlProperties(FProperties);
end;

{ TdxCustomRatingControlProperties }

constructor TdxCustomRatingControlProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAllowHover := True;
  FCheckedGlyph := CreateGlyph;
  FGlyph := CreateGlyph;
  FHoverGlyph := CreateGlyph;
  FItemCount := dxRatingControlDefaultItemCount;
  FStep := 1;
end;

destructor TdxCustomRatingControlProperties.Destroy;
begin
  FreeAndNil(FHoverGlyph);
  FreeAndNil(FGlyph);
  FreeAndNil(FCheckedGlyph);
  inherited Destroy;
end;

procedure TdxCustomRatingControlProperties.DoPrepareDisplayValue(
  const AEditValue: Variant; var DisplayValue: Variant; AEditFocused: Boolean);
var
  AValue: Double;
  ACode: Integer;
begin
  LockUpdate(True);
  try
    DisplayValue := 0;
    if VarIsStr(AEditValue) then
    begin
      Val(VarToStr(AEditValue), AValue, ACode);
      if ACode = 0 then
        DisplayValue := AValue;
    end
    else
      if VarIsNumericEx(AEditValue) or VarIsDate(AEditValue) then
        DisplayValue := AEditValue;
  finally
    LockUpdate(False);
  end;
end;

function TdxCustomRatingControlProperties.CanCompareEditValue: Boolean;
begin
  Result := True;
end;

class function TdxCustomRatingControlProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxRatingControl;
end;

function TdxCustomRatingControlProperties.GetDisplayText(const AEditValue: Variant;
  AFullText: Boolean = False; AIsInplace: Boolean = True): string;
var
  ADisplayValue: TcxEditValue;
begin
  PrepareDisplayValue(AEditValue, ADisplayValue, False);
  Result := ADisplayValue;
end;

class function TdxCustomRatingControlProperties.GetStyleClass: TcxCustomEditStyleClass;
begin
  Result := TdxRatingControlStyle;
end;

function TdxCustomRatingControlProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := [esoAlwaysHotTrack, esoFiltering, esoSorting, esoEditing, esoTransparency];
end;

function TdxCustomRatingControlProperties.CreateGlyph: TdxSmartGlyph;
begin
  Result := TdxSmartGlyph.Create;
  Result.OnChange := ChangeHandler;
end;

function TdxCustomRatingControlProperties.GetStepStored: Boolean;
begin
  Result := Step <> 1;
end;

function TdxCustomRatingControlProperties.CanValidate: Boolean;
begin
  Result := True;
end;

function TdxCustomRatingControlProperties.ChangeEditValueByStep(const AEditValue: Variant; AIsInc: Boolean): Variant;
var
  ADisplayValue: Variant;
begin
  DoPrepareDisplayValue(AEditValue, ADisplayValue, False);
  Result := AdjustRatingByStep(ADisplayValue, Step, AIsInc);
  Result := CorrectRatingByRange(Result);
  if CompareValue(Result, ADisplayValue, DoubleResolution) <> 0 then
    Exit;
  if AIsInc then
    Result := Result + Step
  else
    Result := Result - Step;
  Result := CorrectRatingByRange(Result);
end;

function TdxCustomRatingControlProperties.CorrectRatingByRange(AValue: Double): Double;
begin
  Result := Math.Min(Math.Max(AValue, 0), ItemCount);
end;

procedure TdxCustomRatingControlProperties.CorrectViewInfoRating(AViewInfo: TdxCustomRatingControlViewInfo);
begin
  AViewInfo.Rating := GetCorrectedRating(AViewInfo.Rating);
end;

procedure TdxCustomRatingControlProperties.DoAssign(AProperties: TcxCustomEditProperties);
var
  ASourceProperties: TdxCustomRatingControlProperties;
begin
  inherited DoAssign(AProperties);
  if AProperties is TdxCustomRatingControlProperties then
  begin
    ASourceProperties := TdxCustomRatingControlProperties(AProperties);
    AllowHover := ASourceProperties.AllowHover;
    CheckedGlyph := ASourceProperties.CheckedGlyph;
    Glyph := ASourceProperties.Glyph;
    HoverGlyph := ASourceProperties.HoverGlyph;
    ReverseDirection := ASourceProperties.ReverseDirection;
    FillPrecision := ASourceProperties.FillPrecision;
    ItemCount := ASourceProperties.ItemCount;
    Orientation := ASourceProperties.Orientation;
    ShowCaption := ASourceProperties.ShowCaption;
    ShowEndEllipsis := ASourceProperties.ShowEndEllipsis;
    Step := ASourceProperties.Step;
  end;
end;

function TdxCustomRatingControlProperties.GetCorrectedRating(ARating: Double): Double;
begin
  Result := CorrectRatingByRange(ARating);
  case FillPrecision of
    rcfpFull:
      Result := Trunc(Result);
    rcfpHalf:
      Result := AdjustRatingByStep(Result, 0.5, False);
  end;
end;

class function TdxCustomRatingControlProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxCustomRatingControlViewData;
end;

function TdxCustomRatingControlProperties.HasDisplayValue: Boolean;
begin
  Result := True;
end;

class function TdxCustomRatingControlProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxCustomRatingControlViewInfo;
end;

function TdxCustomRatingControlProperties.IsResetEditClass: Boolean;
begin
  Result := True;
end;

procedure TdxCustomRatingControlProperties.SetAllowHover(AValue: Boolean);
begin
  if FAllowHover <> AValue then
  begin
    FAllowHover := AValue;
    Changed;
  end;
end;

procedure TdxCustomRatingControlProperties.PrepareDisplayValue(
  const AEditValue: Variant; var DisplayValue: Variant; AEditFocused: Boolean);
begin
  DoPrepareDisplayValue(AEditValue, DisplayValue, AEditFocused);
  DisplayValue := GetCorrectedRating(DisplayValue);
end;

procedure TdxCustomRatingControlProperties.SetCheckedGlyph(AValue: TdxSmartGlyph);
begin
  FCheckedGlyph.Assign(AValue);
end;

procedure TdxCustomRatingControlProperties.SetFillPrecision(AValue: TdxRatingControlFillPrecision);
begin
  if FFillPrecision <> AValue then
  begin
    FFillPrecision := AValue;
    Changed;
  end;
end;

procedure TdxCustomRatingControlProperties.SetGlyph(AValue: TdxSmartGlyph);
begin
  FGlyph.Assign(AValue);
end;

procedure TdxCustomRatingControlProperties.SetHoverGlyph(AValue: TdxSmartGlyph);
begin
  FHoverGlyph.Assign(AValue);
end;

procedure TdxCustomRatingControlProperties.SetItemCount(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FItemCount <> AValue then
  begin
    FItemCount := AValue;
    Changed;
  end;
end;

procedure TdxCustomRatingControlProperties.SetOrientation(AValue: TdxOrientation);
begin
  if FOrientation <> AValue then
  begin
    FOrientation := AValue;
    Changed;
  end;
end;

procedure TdxCustomRatingControlProperties.SetReverseDirection(AValue: Boolean);
begin
  if FReverseDirection <> AValue then
  begin
    FReverseDirection := AValue;
    Changed;
  end;
end;

procedure TdxCustomRatingControlProperties.SetShowCaption(AValue: Boolean);
begin
  if FShowCaption <> AValue then
  begin
    FShowCaption := AValue;
    Changed;
  end;
end;

procedure TdxCustomRatingControlProperties.SetShowEndEllipsis(AValue: Boolean);
begin
  if FShowEndEllipsis <> AValue then
  begin
    FShowEndEllipsis := AValue;
    Changed;
  end;
end;

procedure TdxCustomRatingControlProperties.SetStep(AValue: Double);
begin
  if AValue > 1 then
    AValue := 1
  else
    if AValue < 0.01 then
      AValue := 0.01;
  if FStep <> AValue then
  begin
    FStep := AValue;
    Changed;
  end;
end;

{ TdxCustomRatingControl }

class function TdxCustomRatingControl.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxCustomRatingControlProperties;
end;

function TdxCustomRatingControl.DefaultParentColor: Boolean;
begin
  Result := True;
end;

procedure TdxCustomRatingControl.Initialize;
begin
  inherited Initialize;
  ControlStyle := ControlStyle - [csDoubleClicks, csCaptureMouse, csClickEvents];
  Width := 87;
  Height := 20;
end;

procedure TdxCustomRatingControl.InternalSetRating(AValue: Double; ANeedResetEqualsValue: Boolean = True);
begin
  BeginUserAction;
  try
    if DoEditing then
    begin
      LockChangeEvents(True);
      try
        if (CompareValue(AValue, Rating, DoubleResolution) = 0) and ANeedResetEqualsValue then
          AValue := 0;
        EditValue := AValue;
        ActiveProperties.Changed;
        ModifiedAfterEnter := True;
        if ActiveProperties.ImmediatePost and CanPostEditValue and ValidateEdit then
          InternalPostEditValue;
      finally
        LockChangeEvents(False);
      end;
    end;
  finally
    EndUserAction;
  end;
end;

procedure TdxCustomRatingControl.KeyDown(var Key: Word; Shift: TShiftState);
var
  AIsInc: Boolean;
  AValue: Variant;
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_LEFT, VK_DOWN, VK_RIGHT, VK_UP:
    begin
      AIsInc := not ActiveProperties.ReverseDirection and (Key in [VK_RIGHT, VK_UP]) or
        ActiveProperties.ReverseDirection and (Key in [VK_LEFT, VK_DOWN]);
      AValue := ActiveProperties.ChangeEditValueByStep(EditValue, AIsInc);
      InternalSetRating(AValue, False);
    end;
  end;
end;

procedure TdxCustomRatingControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ARating: Double;
begin
  if Button = mbLeft then
  begin
    ARating := ViewInfo.GetRatingByMousePos(X, Y);
    if not IsZero(ARating, DoubleResolution) then
      InternalSetRating(ARating);
    ViewInfo.StopHover := True;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

function TdxCustomRatingControl.WantNavigationKeys: Boolean;
begin
  Result := True;
end;

function TdxCustomRatingControl.FadingCanFadeBackground: Boolean;
begin
  Result := False;
end;

function TdxCustomRatingControl.GetDisplayText: string;
begin
  Result := FloatToStr(Rating);
end;

function TdxCustomRatingControl.CanAutoWidth: Boolean;
begin
  Result := True;
end;

function TdxCustomRatingControl.GetEditStateColorKind: TcxEditStateColorKind;
begin
  Result := cxEditStateColorKindMap[Enabled];
end;

procedure TdxCustomRatingControl.PropertiesChanged(Sender: TObject);
begin
  SynchronizeDisplayValue;
  inherited PropertiesChanged(Sender);
end;

procedure TdxCustomRatingControl.SynchronizeDisplayValue;
var
  ADisplayValue: TcxEditValue;
  APreviewRating: Double;
begin
  APreviewRating := ViewInfo.Rating;
  ActiveProperties.PrepareDisplayValue(EditValue, ADisplayValue, Focused);
  ViewInfo.Rating := ADisplayValue;
  ActiveProperties.CorrectViewInfoRating(ViewInfo);
  if CompareValue(ViewInfo.Rating, APreviewRating) <> 0 then
    DoChange;
  ShortRefreshContainer(False);
end;

procedure TdxCustomRatingControl.TextChanged;
begin
  inherited TextChanged;
  ShortRefreshContainer(False);
end;

function TdxCustomRatingControl.GetProperties: TdxCustomRatingControlProperties;
begin
  Result := TdxCustomRatingControlProperties(inherited Properties);
end;

function TdxCustomRatingControl.GetRating: Double;
begin
  Result := ViewInfo.Rating;
end;

function TdxCustomRatingControl.GetRatingStored: Boolean;
begin
  Result := not IsZero(Rating, DoubleResolution);
end;

function TdxCustomRatingControl.GetStyle: TdxRatingControlStyle;
begin
  Result := TdxRatingControlStyle(FStyles.Style);
end;

function TdxCustomRatingControl.GetActiveProperties: TdxCustomRatingControlProperties;
begin
  Result := TdxCustomRatingControlProperties(InternalGetActiveProperties);
end;

function TdxCustomRatingControl.GetViewInfo: TdxCustomRatingControlViewInfo;
begin
  Result := TdxCustomRatingControlViewInfo(inherited ViewInfo);
end;

procedure TdxCustomRatingControl.SetProperties(Value: TdxCustomRatingControlProperties);
begin
  inherited Properties := Value;
end;

procedure TdxCustomRatingControl.SetRating(AValue: Double);
begin
  if not IsLoading then
    AValue := ActiveProperties.GetCorrectedRating(AValue);
  if CompareValue(AValue, Rating, DoubleResolution) <> 0 then
    EditValue := AValue;
end;

procedure TdxCustomRatingControl.SetStyle(AValue: TdxRatingControlStyle);
begin
  FStyles.Style := AValue;
end;

procedure TdxCustomRatingControl.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := Message.Result or DLGC_WANTARROWS;
  if IsInplace then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

{ TdxRatingControl }

class function TdxRatingControl.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxRatingControlProperties;
end;

function TdxRatingControl.GetActiveProperties: TdxRatingControlProperties;
begin
  Result := TdxRatingControlProperties(InternalGetActiveProperties);
end;

function TdxRatingControl.GetProperties: TdxRatingControlProperties;
begin
  Result := TdxRatingControlProperties(inherited Properties);
end;

procedure TdxRatingControl.SetProperties(Value: TdxRatingControlProperties);
begin
  inherited Properties := Value;
end;

initialization
  GetRegisteredEditProperties.Register(TdxRatingControlProperties, scxSEditRepositoryRatingControlItem);
  FilterEditsController.Register(TdxRatingControlProperties, TdxFilterRatingControlHelper);

finalization
  FilterEditsController.Unregister(TdxRatingControlProperties, TdxFilterRatingControlHelper);
  GetRegisteredEditProperties.Unregister(TdxRatingControlProperties);

end.
