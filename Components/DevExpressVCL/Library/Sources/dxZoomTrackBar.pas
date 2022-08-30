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

unit dxZoomTrackBar;

{$I cxVer.inc}

interface

uses
  Types, Windows, Classes, Controls, Forms, Graphics, Messages, SysUtils, Math,
  dxCoreClasses, cxEdit, cxContainer, cxGeometry, cxTrackBar, cxExtEditConsts, cxGraphics;

type

  { TdxZoomTrackBarRangeOptions }

  TdxZoomTrackBarRangeOptions = class(TcxOwnedPersistent)
  private
    FFrequency: Integer;
    FLineSize: TcxNaturalNumber;
    FPageSize: TcxNaturalNumber;
    FOnChanged: TNotifyEvent;
    procedure DoChanged;
    procedure SetFrequency(Value: Integer);
    procedure SetLineSize(Value: TcxNaturalNumber);
    procedure SetPageSize(Value: TcxNaturalNumber);
  protected
    procedure DoAssign(Source: TPersistent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Frequency: Integer read FFrequency write SetFrequency default 10;
    property LineSize: TcxNaturalNumber read FLineSize write SetLineSize
      default 10;
    property PageSize: TcxNaturalNumber read FPageSize write SetPageSize
      default 20;
  end;

  TdxCustomZoomTrackBarProperties = class;

  { TdxCustomZoomTrackBarViewData }

  TdxCustomZoomTrackBarViewData = class(TcxCustomTrackBarViewData)
  private
    function GetCenterPositionMaxDiff: Integer;
    function GetMinCenterPositionDiff: Integer;
    function GetProperties: TdxCustomZoomTrackBarProperties;
  protected
    procedure CalculateTrackBarViewInfoProperties(AViewInfo: TcxCustomEditViewInfo); override;
    procedure CalculatePixelsPerPositionStep(AViewInfo: TcxCustomTrackBarViewInfo); override;
  public
    property Properties: TdxCustomZoomTrackBarProperties read GetProperties;
  end;

  { TdxCustomZoomTrackBarViewInfo }

  TdxCustomZoomTrackBarViewInfo = class(TcxCustomTrackBarViewInfo)
  protected
    procedure PopulateMajorTicks; override;
    procedure PopulateMinorTicks; override;
    function IsTickText(ATickValue: Integer): Boolean; override;
    procedure PaintTrackBar(ACanvas: TcxCanvas); override;

    function GetCalculatedPosition(APosition: Integer; AIsInc: Boolean;
      AIsLineSize: Boolean; ANeedRound: Boolean = False): Integer; override;
    function GetPositionAfterJump(X, Y: Integer): Integer; override;
  public
    FrequencyLeft: Integer;
    FrequencyRight: Integer;

    PixelsPerPositionFirstRange: Double;
    PixelsPerPositionSecondRange: Double;

    function GetTrackRectSize: Integer;
    function GetOffset(APosition: Integer): Integer; override;

    property TrackRectSize: Integer read GetTrackRectSize;
  end;

  { TdxCustomZoomTrackBarProperties }

  TdxCustomZoomTrackBarProperties = class(TcxCustomTrackBarProperties)
  strict private
    FFirstRange: TdxZoomTrackBarRangeOptions;
    FSecondRange: TdxZoomTrackBarRangeOptions;
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    procedure SetFirstRange(Value: TdxZoomTrackBarRangeOptions);
    procedure SetSecondRange(Value: TdxZoomTrackBarRangeOptions);
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    procedure RangePropertiesChanged(Sender: TObject);
    procedure SetMax(Value: Integer); override;
    procedure SetMin(Value: Integer); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    property FirstRange: TdxZoomTrackBarRangeOptions read FFirstRange write SetFirstRange;
    property Max default 500;
    property Min default 10;
    property SecondRange: TdxZoomTrackBarRangeOptions read FSecondRange write SetSecondRange;
    property ShowChangeButtons default True;
    property ShowTicks default False;
    property ThumbStep default cxtsJump;
    property ThumbHeight default 20;
    property ThumbWidth default 10;
    property TrackSize default 10;
  end;

  { TdxZoomTrackBarProperties }

  TdxZoomTrackBarProperties = class(TdxCustomZoomTrackBarProperties)
  published
    property AutoSize;
    property BorderWidth;
    property ClearKey;
    property FirstRange;
    property Max;
    property Min;
    property Orientation;
    property ReverseDirection;
    property SecondRange;
    property SelectionColor;
    property SelectionEnd;
    property SelectionStart;
    property ShowChangeButtons;
    property ShowPositionHint;
    property ShowTicks;
    property ShowTrack;
    property TextOrientation;
    property ThumbColor;
    property ThumbHeight;
    property ThumbHighlightColor;
    property ThumbStep;
    property ThumbType;
    property ThumbWidth;
    property TickColor;
    property TickMarks;
    property TickSize;
    property TickType;
    property TrackColor;
    property TrackSize;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnChange;
    property OnGetPositionHint;
    property OnDrawThumb;
    property OnGetTickLabel;
    property OnGetThumbRect;
    property OnValidate;
  end;

  { TcxZoomTrackBarController }

  TdxZoomTrackBarController = class(TcxTrackBarController)
  private
    FCanThumbJumpToCenter: Boolean;
  protected
    function GetPositionAfterSliding(X: Integer; Y: Integer): Integer; override;
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  end;

  { TdxCustomZoomTrackBar }

  TdxCustomZoomTrackBar = class(TcxCustomTrackBar)
  private
    function GetActiveProperties: TdxCustomZoomTrackBarProperties;
    function GetProperties: TdxCustomZoomTrackBarProperties;
    function GetViewInfo: TdxCustomZoomTrackBarViewInfo;
    procedure SetProperties(Value: TdxCustomZoomTrackBarProperties);
  protected
    function CreateController: TcxTrackBarController; override;
    property ViewInfo: TdxCustomZoomTrackBarViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TdxCustomZoomTrackBarProperties read GetActiveProperties;
    property Position default 100;
    property Properties: TdxCustomZoomTrackBarProperties read GetProperties write SetProperties;
  end;

  { TdxZoomTrackBar }

  TdxZoomTrackBar = class(TdxCustomZoomTrackBar)
  strict private
    function GetActiveProperties: TdxZoomTrackBarProperties;
    function GetProperties: TdxZoomTrackBarProperties;
    procedure SetProperties(Value: TdxZoomTrackBarProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TdxZoomTrackBarProperties read GetActiveProperties;
  published
    property Anchors;
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
    property Position;
    property Properties: TdxZoomTrackBarProperties read GetProperties write SetProperties;
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
    property OnEditing;
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

const
  CenterPosition = 100;
  OffsetForJumpToCenter = 5;
  OffsetForJumpToCenterRenewal = 10;

{ TdxZoomTrackBarRangeOptions }

constructor TdxZoomTrackBarRangeOptions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFrequency := 10;
  FLineSize := 10;
  FPageSize := 20;
end;

procedure TdxZoomTrackBarRangeOptions.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxZoomTrackBarRangeOptions then
  begin
    Frequency := TdxZoomTrackBarRangeOptions(Source).Frequency;
    LineSize := TdxZoomTrackBarRangeOptions(Source).LineSize;
    PageSize := TdxZoomTrackBarRangeOptions(Source).PageSize;
  end;
end;

procedure TdxZoomTrackBarRangeOptions.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TdxZoomTrackBarRangeOptions.SetFrequency(Value: Integer);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Math.Max(0, Value);
    DoChanged;
  end;
end;
procedure TdxZoomTrackBarRangeOptions.SetLineSize(Value: TcxNaturalNumber);
begin
  if Value <> FLineSize then
  begin
    FLineSize := Value;
    DoChanged;
  end;
end;

procedure TdxZoomTrackBarRangeOptions.SetPageSize(Value: TcxNaturalNumber);
begin
  if Value <> FPageSize then
  begin
    FPageSize := Value;
    DoChanged;
  end;
end;

{ TdxCustomZoomTrackBarProperties }

constructor TdxCustomZoomTrackBarProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFirstRange := TdxZoomTrackBarRangeOptions.Create(Self);
  FSecondRange := TdxZoomTrackBarRangeOptions.Create(Self);
  FFirstRange.OnChanged := RangePropertiesChanged;
  FSecondRange.OnChanged := RangePropertiesChanged;
  FMin := 10;
  FMax := 500;
  FShowChangeButtons := True;
  FShowTicks := False;
  FThumbHeight := 20;
  FThumbWidth := 10;
  FTrackSize := 10;
  ThumbStep := cxtsJump;
end;

destructor TdxCustomZoomTrackBarProperties.Destroy;
begin
  FreeAndNil(FFirstRange);
  FreeAndNil(FSecondRange);
  inherited Destroy;
end;

class function TdxCustomZoomTrackBarProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxCustomZoomTrackBarViewData;
end;

class function TdxCustomZoomTrackBarProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxCustomZoomTrackBarViewInfo;
end;

procedure TdxCustomZoomTrackBarProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited DoAssign(AProperties);
  if AProperties is TdxCustomZoomTrackBarProperties then
  begin
    FirstRange := TdxCustomZoomTrackBarProperties(AProperties).FirstRange;
    SecondRange := TdxCustomZoomTrackBarProperties(AProperties).SecondRange;
  end;
end;

procedure TdxCustomZoomTrackBarProperties.SetFirstRange(Value: TdxZoomTrackBarRangeOptions);
begin
  FFirstRange.Assign(Value);
end;

procedure TdxCustomZoomTrackBarProperties.SetSecondRange(Value: TdxZoomTrackBarRangeOptions);
begin
  FSecondRange.Assign(Value);
end;

procedure TdxCustomZoomTrackBarProperties.RangePropertiesChanged(Sender: TObject);
begin
  Changed;
end;

procedure TdxCustomZoomTrackBarProperties.SetMax(Value: Integer);
begin
  Value := Math.Max(Value, CenterPosition);
  if FMax <> Value then
  begin
    FMax := Value;
    Changed;
  end;
end;

procedure TdxCustomZoomTrackBarProperties.SetMin(Value: Integer);
begin
  Value := Math.Min(Value, CenterPosition);
  if FMin <> Value then
  begin
    FMin := Value;
    Changed;
  end;
end;

{ TdxCustomZoomTrackBarViewData }

procedure TdxCustomZoomTrackBarViewData.CalculateTrackBarViewInfoProperties(AViewInfo: TcxCustomEditViewInfo);
begin
  TdxCustomZoomTrackBarViewInfo(AViewInfo).FrequencyLeft := Properties.FirstRange.FFrequency;
  TdxCustomZoomTrackBarViewInfo(AViewInfo).FrequencyRight := Properties.SecondRange.FFrequency;
  inherited CalculateTrackBarViewInfoProperties(AViewInfo);
end;

procedure TdxCustomZoomTrackBarViewData.CalculatePixelsPerPositionStep(AViewInfo: TcxCustomTrackBarViewInfo);
var
  APixelsPerPositionFirstRange, APixelsPerPositionSecondRange: Double;
  ATrackRect: TRect;
begin
  inherited CalculatePixelsPerPositionStep(AViewInfo);
  ATrackRect := TdxCustomZoomTrackBarViewInfo(AViewInfo).TrackRect;
  APixelsPerPositionFirstRange := (cxRectWidth(ATrackRect) - AViewInfo.ThumbSize) / (2 * GetMinCenterPositionDiff);
  APixelsPerPositionSecondRange := (cxRectWidth(ATrackRect) - AViewInfo.ThumbSize) / (2 * GetCenterPositionMaxDiff);
  TdxCustomZoomTrackBarViewInfo(AViewInfo).PixelsPerPositionFirstRange := APixelsPerPositionFirstRange;
  TdxCustomZoomTrackBarViewInfo(AViewInfo).PixelsPerPositionSecondRange := APixelsPerPositionSecondRange;
end;

function TdxCustomZoomTrackBarViewData.GetCenterPositionMaxDiff: Integer;
begin
  Result := Max(Properties.Max - CenterPosition, 1);
end;

function TdxCustomZoomTrackBarViewData.GetMinCenterPositionDiff: Integer;
begin
  Result := Max(CenterPosition - Properties.Min, 1);
end;

function TdxCustomZoomTrackBarViewData.GetProperties: TdxCustomZoomTrackBarProperties;
begin
  Result := TdxCustomZoomTrackBarProperties(FProperties);
end;

{ TdxCustomZoomTrackBarViewInfo }

function TdxCustomZoomTrackBarViewInfo.GetTrackRectSize: Integer;
begin
  Result := cxRectWidth(TrackRect);
end;

function TdxCustomZoomTrackBarViewInfo.GetOffset(APosition: Integer): Integer;
begin
  Result := ThumbSize div 2 + Round(PixelsPerPositionFirstRange * (Math.Min(APosition, CenterPosition) - Min)) +
    Round(PixelsPerPositionSecondRange * Math.Max(0, APosition - CenterPosition));
end;

procedure TdxCustomZoomTrackBarViewInfo.PopulateMajorTicks;
begin
  inherited;
  MajorTicks.Add(Pointer(CenterPosition));
end;

procedure TdxCustomZoomTrackBarViewInfo.PopulateMinorTicks;
begin
  MinorTicks.Clear;
  AddMinorTicks(Min, CenterPosition, FrequencyLeft);
  AddMinorTicks(CenterPosition, Max, FrequencyRight);
end;

function TdxCustomZoomTrackBarViewInfo.IsTickText(ATickValue: Integer): Boolean;
begin
  Result := inherited IsTickText(ATickValue) or (ATickValue = CenterPosition);
end;

procedure TdxCustomZoomTrackBarViewInfo.PaintTrackBar(ACanvas: TcxCanvas);
begin
  inherited PaintTrackBar(ACanvas);
  if not TrackBarProperties.ShowTicks then
    DrawTickAsLine(ACanvas, CenterPosition, GetCoordinate(CenterPosition));
end;

function TdxCustomZoomTrackBarViewInfo.GetCalculatedPosition(APosition: Integer; AIsInc: Boolean; AIsLineSize: Boolean; ANeedRound: Boolean = False): Integer;
var
  AFirstStep, ASecondStep, AStep: TcxNaturalNumber;
  AEditProperties: TdxCustomZoomTrackBarProperties;
begin
  AEditProperties := TdxCustomZoomTrackBarProperties(EditProperties);
  if ReverseDirection then
    AIsInc := not AIsInc;
  if AIsLineSize then
  begin
    AFirstStep := AEditProperties.FirstRange.LineSize;
    ASecondStep := AEditProperties.SecondRange.LineSize;
  end
  else
  begin
    AFirstStep := AEditProperties.FirstRange.PageSize;
    ASecondStep := AEditProperties.SecondRange.PageSize;
  end;
  if APosition < CenterPosition then
    if (APosition > CenterPosition - AFirstStep) and AIsInc then
      AStep := CenterPosition - APosition
    else
      AStep := AFirstStep
  else
    if APosition > CenterPosition then
      if (APosition < CenterPosition + ASecondStep) and not AIsInc then
        AStep := APosition - CenterPosition
      else
        AStep := ASecondStep
    else
      if AIsInc then
        AStep := ASecondStep
      else
        AStep := AFirstStep;
  if AIsInc then
    Result := APosition + AStep
  else
    Result := APosition - AStep;
  if ANeedRound and (Result <> CenterPosition) then
    if AIsInc then
      Result := Floor((Result - CenterPosition) / AStep) * AStep + CenterPosition
    else
      Result := Ceil((Result - CenterPosition) / AStep) * AStep + CenterPosition;
end;

function TdxCustomZoomTrackBarViewInfo.GetPositionAfterJump(X, Y: Integer): Integer;

  function IsTopLeftRange: Boolean;
  begin
    Result := GetCurrentMousePos(X, Y) < TrackRect.Left + cxRectWidth(TrackRect) div 2;
  end;

  function GetMouseOffset: Integer;
  begin
    if IsTopLeftRange then
      Result := GetCurrentMousePos(X, Y) - TrackRect.Left - cxRectWidth(VisibleThumbRect) div 2
    else
      Result := GetCurrentMousePos(X, Y) - TrackRect.Left  - cxRectWidth(TrackRect) div 2;
  end;

var
  AEditProperties: TdxCustomZoomTrackBarProperties;
begin
  AEditProperties := TdxCustomZoomTrackBarProperties(EditProperties);
  inherited GetPositionAfterJump(X, Y);
  if ReverseDirection then
    if IsTopLeftRange then
      Result := AEditProperties.Max - Trunc(GetMouseOffset / PixelsPerPositionSecondRange)
    else
      Result := CenterPosition - Trunc(GetMouseOffset / PixelsPerPositionFirstRange)
  else
    if IsTopLeftRange then
      Result := Trunc(GetMouseOffset / PixelsPerPositionFirstRange) + AEditProperties.Min
    else
      Result := Trunc(GetMouseOffset / PixelsPerPositionSecondRange) + CenterPosition
end;

{ TcxZoomTrackBarController }

function TdxZoomTrackBarController.GetPositionAfterSliding(X, Y: Integer): Integer;

  function NeedJumpToCenter(AMouseDelta, AInitialThumbOffCentering: Integer): Boolean;
  begin
    Result := (Abs(AMouseDelta - AInitialThumbOffCentering) < OffsetForJumpToCenter) and (FCanThumbJumpToCenter or (Abs(AInitialThumbOffCentering) > OffsetForJumpToCenter));
  end;

var
  AInitialThumbOffCentering, AMouseDeltaRemainder: Integer;
  AViewInfo: TdxCustomZoomTrackBarViewInfo;
begin
  AViewInfo := TdxCustomZoomTrackBarViewInfo(ViewInfo);
  if StoredPosition <= CenterPosition then
  begin
    AInitialThumbOffCentering := Trunc((CenterPosition - StoredPosition) * AViewInfo.PixelsPerPositionFirstRange);
    if AViewInfo.GetMouseDelta(X, Y, GetMouseDownPos) <= AInitialThumbOffCentering then
      Result := StoredPosition + Round(AViewInfo.GetMouseDelta(X, Y, GetMouseDownPos) / AViewInfo.PixelsPerPositionFirstRange)
    else
    begin
      AMouseDeltaRemainder := AViewInfo.GetMouseDelta(X, Y, GetMouseDownPos) - AInitialThumbOffCentering;
      Result := StoredPosition + Round(AInitialThumbOffCentering / AViewInfo.PixelsPerPositionFirstRange + AMouseDeltaRemainder / AViewInfo.PixelsPerPositionSecondRange);
    end;
  end
  else
  begin
    AInitialThumbOffCentering := Trunc((CenterPosition - StoredPosition) * AViewInfo.PixelsPerPositionSecondRange);
    if AViewInfo.GetMouseDelta(X, Y, GetMouseDownPos) >= AInitialThumbOffCentering then
      Result := StoredPosition + Round(AViewInfo.GetMouseDelta(X, Y, GetMouseDownPos) / AViewInfo.PixelsPerPositionSecondRange)
    else
    begin
      AMouseDeltaRemainder := AViewInfo.GetMouseDelta(X, Y, GetMouseDownPos) - AInitialThumbOffCentering;
      Result := StoredPosition + Round(AMouseDeltaRemainder / AViewInfo.PixelsPerPositionFirstRange + AInitialThumbOffCentering / AViewInfo.PixelsPerPositionSecondRange);
    end;
  end;
  if Abs(AViewInfo.GetMouseDelta(X, Y, GetMouseDownPos)) > OffsetForJumpToCenterRenewal then
    FCanThumbJumpToCenter := True;
  if NeedJumpToCenter(AViewInfo.GetMouseDelta(X, Y, GetMouseDownPos), AInitialThumbOffCentering) then
    Result := CenterPosition;
end;

procedure TdxZoomTrackBarController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FCanThumbJumpToCenter := False;
end;

{ TdxCustomZoomTrackBar }

constructor TdxCustomZoomTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InternalSetEditValue(100, True);
end;

class function TdxCustomZoomTrackBar.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxCustomZoomTrackBarProperties;
end;

function TdxCustomZoomTrackBar.CreateController: TcxTrackBarController;
begin
  Result := TdxZoomTrackBarController.Create(Self);
end;

function TdxCustomZoomTrackBar.GetActiveProperties: TdxCustomZoomTrackBarProperties;
begin
  Result := TdxCustomZoomTrackBarProperties(InternalGetActiveProperties);
end;

function TdxCustomZoomTrackBar.GetProperties: TdxCustomZoomTrackBarProperties;
begin
  Result := TdxCustomZoomTrackBarProperties(inherited Properties);
end;

function TdxCustomZoomTrackBar.GetViewInfo: TdxCustomZoomTrackBarViewInfo;
begin
  Result := TdxCustomZoomTrackBarViewInfo(FViewInfo);
end;

procedure TdxCustomZoomTrackBar.SetProperties(Value: TdxCustomZoomTrackBarProperties);
begin
  Properties.Assign(Value);
end;

{ TdxZoomTrackBar }

class function TdxZoomTrackBar.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxZoomTrackBarProperties;
end;

function TdxZoomTrackBar.GetActiveProperties: TdxZoomTrackBarProperties;
begin
  Result := TdxZoomTrackBarProperties(InternalGetActiveProperties);
end;

function TdxZoomTrackBar.GetProperties: TdxZoomTrackBarProperties;
begin
  Result := TdxZoomTrackBarProperties(inherited Properties);
end;

procedure TdxZoomTrackBar.SetProperties(Value: TdxZoomTrackBarProperties);
begin
  Properties.Assign(Value);
end;

end.
