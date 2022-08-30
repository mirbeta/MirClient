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

unit dxRangeTrackBar;

{$I cxVer.inc}

interface

uses
  Types, Variants,
  Windows, Classes, Controls, Forms, Graphics, Messages, SysUtils, StrUtils,
  dxCore, dxCoreClasses, cxClasses, cxContainer, cxControls,
  cxEdit, cxExtEditConsts, cxExtEditUtils, cxLookAndFeelPainters, cxGeometry, dxThemeConsts,
  cxFilterControlUtils, cxGraphics, cxLookAndFeels, cxTextEdit, cxVariants, Math, dxCustomHint, cxTrackBar;

type
  TdxCustomRangeTrackBar = class;
  TdxCustomRangeTrackBarProperties = class;

  TdxRangeTrackBarEditValueFormat = (revfInt64, revfString);

  TdxRangeTrackBarGetCustomHintEvent = procedure(Sender: TObject; const AMinPosition, AMaxPosition: Integer;
    var AHintText: string; var ACanShow, AIsHintMultiLine: Boolean) of object;

  { TdxTrackBarRange }

  TdxTrackBarRange = class(TcxOwnedPersistent)
  strict private
    function GetTrackBar: TdxCustomRangeTrackBar;
    function GetMax: Integer;
    function GetMin: Integer;
    procedure SetMin(AValue: Integer);
    procedure SetMax(AValue: Integer);
  protected
    procedure DoSetMax(AValue: Integer; AIsUserAction: Boolean);
    procedure DoSetMin(AValue: Integer; AIsUserAction: Boolean);
    procedure DoAssign(Source: TPersistent); override;
    property TrackBar: TdxCustomRangeTrackBar read GetTrackBar;
  published
    property Max: Integer read GetMax write SetMax;
    property Min: Integer read GetMin write SetMin;
  end;

  { TdxCustomRangeTrackBarViewInfo }

  TdxCustomRangeTrackBarViewInfo = class(TcxCustomTrackBarViewInfo)
  private
    FLeftThumb: TcxTrackBarThumbViewInfo;
    FLeftThumbWidth: Integer;
    FRightThumbWidth: Integer;
    FRightThumb: TcxTrackBarThumbViewInfo;

    function GetRangeTrackBarProperties: TdxCustomRangeTrackBarProperties;
  protected
    procedure AddThumbs; override;
    procedure DoGetPositionHint(var AHintText: string; var ACanShow: Boolean; var AIsHintMultiLine: Boolean); override;
    procedure DrawCustomThumb(ACanvas: TcxCanvas); override;
    function DrawingThumbRectToRealThumbRect(ACanvas: TcxCanvas; const AThumbRect: TRect): TRect; override;
    procedure DrawThumb(ACanvas: TcxCanvas); override;
    procedure DrawLeftThumb(ACanvas: TcxCanvas; const R: TRect; AState: Integer);
    procedure DrawRightThumb(ACanvas: TcxCanvas; const R: TRect; AState: Integer);
    function GetDefaultPositionHintText: string; override;
    function GetMaxThumb: TcxTrackBarThumbViewInfo;
    function GetMinThumb: TcxTrackBarThumbViewInfo;
    function IsPtToTheRightFromThumb(AThumb: TcxTrackBarThumbViewInfo; X, Y: Integer): Boolean; override;
    function IsPtToTheLeftFromThumb(AThumb: TcxTrackBarThumbViewInfo; X, Y: Integer): Boolean; override;
    function IsTickText(ATickValue: Integer): Boolean; override;
    procedure UpdateSelection;
    procedure UpdateTrackBarState; override;
    procedure UpdateValue(AValue: TcxEditValue); override;

    property LeftThumb: TcxTrackBarThumbViewInfo read FLeftThumb;
    property RangeTrackBarProperties: TdxCustomRangeTrackBarProperties read GetRangeTrackBarProperties;
    property RightThumb: TcxTrackBarThumbViewInfo read FRightThumb;
  public
    procedure Assign(Source: TObject); override;
  end;

  { TdxRangeTrackBarLeftThumbFadingHelper }

  TdxRangeTrackBarLeftThumbFadingHelper = class(TcxTrackBarThumbFadingHelper)
  protected
    procedure DrawThumb(ACanvas: TcxCanvas; const R: TRect; AState: Integer); override;
  end;

  { TdxRangeTrackBarRightThumbFadingHelper }

  TdxRangeTrackBarRightThumbFadingHelper = class(TcxTrackBarThumbFadingHelper)
  strict private
    function GetViewInfo: TdxCustomRangeTrackBarViewInfo;
  protected
    procedure DrawThumb(ACanvas: TcxCanvas; const R: TRect; AState: Integer); override;
    function GetThumbRect: TRect; override;
    property ViewInfo: TdxCustomRangeTrackBarViewInfo read GetViewInfo;
  end;

  { TdxRangeTrackBarLeftThumbViewInfo }

  TdxRangeTrackBarLeftThumbViewInfo = class(TcxTrackBarThumbViewInfo)
  protected
    function CreateFadingHelper: TcxTrackBarThumbFadingHelper; override;
  end;

  { TdxRangeTrackBarRightThumbViewInfo }

  TdxRangeTrackBarRightThumbViewInfo = class(TcxTrackBarThumbViewInfo)
  protected
    function CreateFadingHelper: TcxTrackBarThumbFadingHelper; override;
  end;

  { TdxRangeTrackBarViewData }

  TdxRangeTrackBarViewData = class(TcxCustomTrackBarViewData)
  strict private
    function GetProperties: TdxCustomRangeTrackBarProperties;
  protected
    procedure CalculateThumbSize(AViewInfo: TcxCustomTrackBarViewInfo); override;
    procedure CalculateThumbRect(ACanvas: TcxCanvas; AViewInfo: TcxCustomTrackBarViewInfo); override;
    procedure UpdateSelection(AViewInfo: TcxCustomTrackBarViewInfo); override;
    procedure UpdateValue(AViewInfo: TcxCustomTrackBarViewInfo; AValue: TcxEditValue); override;
  public
    property Properties: TdxCustomRangeTrackBarProperties read GetProperties;
  end;

  { TdxCustomRangeTrackBarProperties }

  TdxCustomRangeTrackBarProperties = class(TcxCustomTrackBarProperties)
  strict private
    FEditValueFormat: TdxRangeTrackBarEditValueFormat;
    FDelimiter: Char;
    FShowSelection: Boolean;

    FOnDrawMaxThumb: TcxDrawThumbEvent;
    FOnGetPositionHint: TdxRangeTrackBarGetCustomHintEvent;

    function IsDelimiterStored: Boolean;
    function GetOnDrawMinThumb: TcxDrawThumbEvent;
    procedure SetEditValueFormat(const Value: TdxRangeTrackBarEditValueFormat);
    procedure SetDelimiter(const Value: Char);
    procedure SetShowSelection(const Value: Boolean);
    procedure SetOnDrawMinThumb(const Value: TcxDrawThumbEvent);
  protected
    procedure CheckRange(var AValue: Integer);
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    procedure DoDrawRangeMinThumb(Sender: TObject; ACanvas: TcxCanvas; const ARect: TRect);
    procedure DoDrawRangeMaxThumb(Sender: TObject; ACanvas: TcxCanvas; const ARect: TRect);
    function EditValueToRange(const AEditValue: TcxEditValue): Int64; overload;
    function EditValueToRange(AEditValueFormat: TdxRangeTrackBarEditValueFormat; ADelimiter: Char;
      const AEditValue: TcxEditValue): Int64; overload;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    property EditValueFormat: TdxRangeTrackBarEditValueFormat read FEditValueFormat write SetEditValueFormat default revfInt64;
    property Delimiter: Char read FDelimiter write SetDelimiter stored IsDelimiterStored;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection default True;
    property OnDrawRangeMaxThumb: TcxDrawThumbEvent read FOnDrawMaxThumb write FOnDrawMaxThumb;
    property OnDrawRangeMinThumb: TcxDrawThumbEvent read GetOnDrawMinThumb write SetOnDrawMinThumb;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure CalculateEditValueByRange(AMin, AMax: Integer; out AValue: Variant);
    procedure CalculateRangeByEditValue(AValue: Variant; AEditValueFormat: TdxRangeTrackBarEditValueFormat;
      ADelimiter: Char; out AMin, AMax: Integer); overload;
    procedure CalculateRangeByEditValue(AValue: Variant; out AMin, AMax: Integer); overload;
    class function GetContainerClass: TcxContainerClass; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;
    property OnGetPositionHint: TdxRangeTrackBarGetCustomHintEvent read FOnGetPositionHint write FOnGetPositionHint;
  end;

  { TdxRangeTrackBarController }

  TdxRangeTrackBarController = class(TcxTrackBarController)
  strict private
    function GetPosition(AThumb: TcxTrackBarThumbViewInfo): Integer;
    function GetViewInfo: TdxCustomRangeTrackBarViewInfo;
    function GetRange: TdxTrackBarRange;
    procedure SetPosition(AValue: Integer; AThumb: TcxTrackBarThumbViewInfo);
  protected
    procedure UpdatePos(AValue: Integer; AThumb: TcxTrackBarThumbViewInfo); override;
  public
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    property ViewInfo: TdxCustomRangeTrackBarViewInfo read GetViewInfo;
    property Range: TdxTrackBarRange read GetRange;
  end;

  { TdxCustomRangeTrackBar }

  TdxCustomRangeTrackBar = class(TcxCustomTrackBar)
  strict private
    FDelimiter: Char;
    FEditValueFormat: TdxRangeTrackBarEditValueFormat;
    FRange: TdxTrackBarRange;

    function GetActiveProperties: TdxCustomRangeTrackBarProperties;
    function GetProperties: TdxCustomRangeTrackBarProperties;
    procedure SetProperties(const Value: TdxCustomRangeTrackBarProperties);
    procedure SetRange(AValue: TdxTrackBarRange);
  protected
    procedure CheckRange(var AValue: Integer);
    function CreateController: TcxTrackBarController; override;
    procedure Initialize; override;
    function GetDisplayText: string; override;
    function GetPosition: Integer; override;
    function GetRangeValue: Int64;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure SetPosition(Value: Integer); override;
    procedure SetRangeValue(AValue: Int64; AIsUserAction: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;

    property ActiveProperties: TdxCustomRangeTrackBarProperties read GetActiveProperties;
    property Properties: TdxCustomRangeTrackBarProperties read GetProperties write SetProperties;
    property Range: TdxTrackBarRange read FRange write SetRange;
  end;

  { TdxRangeTrackBarProperties }

  TdxRangeTrackBarProperties = class(TdxCustomRangeTrackBarProperties)
  published
    property AutoSize;
    property BorderWidth;
    property ClearKey;
    property Delimiter;
    property EditValueFormat;
    property Frequency;
    property LineSize;
    property Max;
    property Min;
    property Orientation;
    property PageSize;
    property ReverseDirection;
    property SelectionColor;
    property ShowPositionHint;
    property ShowSelection;
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
    property OnDrawRangeMaxThumb;
    property OnDrawRangeMinThumb;
    property OnGetTickLabel;
    property OnGetThumbRect;
    property OnValidate;
  end;

  { TdxRangeTrackBar }

  TdxRangeTrackBar = class(TdxCustomRangeTrackBar)
  strict private
    function GetActiveProperties: TdxRangeTrackBarProperties;
    function GetProperties: TdxRangeTrackBarProperties;
    procedure SetProperties(const Value: TdxRangeTrackBarProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TdxRangeTrackBarProperties read GetActiveProperties;
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
    property Properties: TdxRangeTrackBarProperties read GetProperties write SetProperties;
    property Range;
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
  DefaultRangeTrackBarValueDelimiter = ';';

function GetRange(AMin, AMax: Integer): Int64;
var
  AMaxUint, AMinUint: Cardinal;
begin
  AMaxUint := AMax;
  AMinUint := AMin;
  Result := AMaxUint;
  Result := Result shl 32 or AMinUint;
end;

function GetMinRange(ARange: Int64): Integer;
begin
  Result := ARange and $FFFFFFFF;
end;

function GetMaxRange(ARange: Int64): Integer;
begin
  Result := ARange shr 32;
end;

{ TdxCustomRangeTrackBar }

constructor TdxCustomRangeTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDelimiter := DefaultRangeTrackBarValueDelimiter;
  FRange := TdxTrackBarRange.Create(Self);
end;

destructor TdxCustomRangeTrackBar.Destroy;
begin
  FreeAndNil(FRange);
  inherited;
end;

class function TdxCustomRangeTrackBar.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxCustomRangeTrackBarProperties;
end;

procedure TdxCustomRangeTrackBar.CheckRange(var AValue: Integer);
begin
  ActiveProperties.CheckRange(AValue);
end;

function TdxCustomRangeTrackBar.CreateController: TcxTrackBarController;
begin
  Result := TdxRangeTrackBarController.Create(Self);
end;

procedure TdxCustomRangeTrackBar.Initialize;
begin
  inherited Initialize;
  FEditValue := Null;
end;

function TdxCustomRangeTrackBar.GetDisplayText: string;
begin
  if VarIsNull(EditValue) then
    Result := ''
  else
    Result := EditValue;
end;

function TdxCustomRangeTrackBar.GetPosition: Integer;
begin
  Result := Range.Min;
end;

function TdxCustomRangeTrackBar.GetRangeValue: Int64;
begin
  Result := ActiveProperties.EditValueToRange(EditValue);
end;

procedure TdxCustomRangeTrackBar.PropertiesChanged(Sender: TObject);
var
  AEditValue: TcxEditValue;
  AMin, AMax: Integer;
begin
  if not IsDBEdit and not VarIsNull(EditValue) then
  begin
    ActiveProperties.CalculateRangeByEditValue(EditValue, FEditValueFormat, FDelimiter, AMin, AMax);
    FEditValueFormat := ActiveProperties.EditValueFormat;
    FDelimiter := ActiveProperties.Delimiter;
    CheckRange(AMin);
    CheckRange(AMax);
    ActiveProperties.CalculateEditValueByRange(AMin, AMax, AEditValue);
    EditValue := AEditValue;
  end;
  inherited PropertiesChanged(Sender);
end;

procedure TdxCustomRangeTrackBar.SetPosition(Value: Integer);
begin
  Range.Min := Value;
end;

procedure TdxCustomRangeTrackBar.SetRangeValue(AValue: Int64; AIsUserAction: Boolean);

  procedure DoSetRangeValue(AValue: Int64);
  var
    AEditValue: TcxEditValue;
  begin
    ActiveProperties.CalculateEditValueByRange(GetMinRange(AValue), GetMaxRange(AValue), AEditValue);
    FEditValueFormat := ActiveProperties.EditValueFormat;
    InternalEditValue := AEditValue;
  end;

begin
  if not AIsUserAction then
    DoSetRangeValue(AValue)
  else
  begin
    BeginUserAction;
    try
      if DoEditing then
      begin
        DoSetRangeValue(AValue);
     //   ActiveProperties.Changed;
        ModifiedAfterEnter := True;
        if ValidateEdit then
          InternalPostEditValue;
      end;
    finally
      EndUserAction;
    end;
  end;
end;

function TdxCustomRangeTrackBar.GetActiveProperties: TdxCustomRangeTrackBarProperties;
begin
  Result := inherited ActiveProperties as TdxCustomRangeTrackBarProperties;
end;

function TdxCustomRangeTrackBar.GetProperties: TdxCustomRangeTrackBarProperties;
begin
  Result := inherited Properties as TdxCustomRangeTrackBarProperties;
end;

procedure TdxCustomRangeTrackBar.SetProperties(
  const Value: TdxCustomRangeTrackBarProperties);
begin
  inherited Properties := Value;
end;

procedure TdxCustomRangeTrackBar.SetRange(AValue: TdxTrackBarRange);
begin
  FRange.Assign(AValue);
end;

{ TdxTrackBarRange }

procedure TdxTrackBarRange.DoSetMax(AValue: Integer; AIsUserAction: Boolean);
begin
  TrackBar.CheckRange(AValue);
  AValue := Math.Max(AValue, Min);
  if Max <> AValue then
    TrackBar.SetRangeValue(GetRange(Min, AValue), AIsUserAction);
end;

procedure TdxTrackBarRange.DoSetMin(AValue: Integer; AIsUserAction: Boolean);
begin
  TrackBar.CheckRange(AValue);
  AValue := Math.Min(AValue, Max);
  if Min <> AValue then
    TrackBar.SetRangeValue(GetRange(AValue, Max), AIsUserAction);
end;

procedure TdxTrackBarRange.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxTrackBarRange then
  begin
    Min := TdxTrackBarRange(Source).Min;
    Max := TdxTrackBarRange(Source).Max;
  end;
end;

function TdxTrackBarRange.GetMax: Integer;
begin
  Result := GetMaxRange(TrackBar.GetRangeValue);
  TrackBar.CheckRange(Result);
  Result := Math.Max(Result, Min);
end;

function TdxTrackBarRange.GetMin: Integer;
begin
  Result := GetMinRange(TrackBar.GetRangeValue);
  TrackBar.CheckRange(Result);
end;

function TdxTrackBarRange.GetTrackBar: TdxCustomRangeTrackBar;
begin
  Result := Owner as TdxCustomRangeTrackBar;
end;

procedure TdxTrackBarRange.SetMax(AValue: Integer);
begin
  DoSetMax(AValue, False);
end;

procedure TdxTrackBarRange.SetMin(AValue: Integer);
begin
  DoSetMin(AValue, False);
end;

{ TdxCustomRangeTrackBarProperties }

constructor TdxCustomRangeTrackBarProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDelimiter := DefaultRangeTrackBarValueDelimiter;
  FShowSelection := True;
end;

procedure TdxCustomRangeTrackBarProperties.CalculateEditValueByRange(AMin, AMax: Integer; out AValue: Variant);
begin
  if FEditValueFormat = revfInt64 then
    AValue := GetRange(AMin, AMax)
  else
    AValue := Format('%d%s%d', [AMin, Delimiter, AMax]);
end;

procedure TdxCustomRangeTrackBarProperties.CalculateRangeByEditValue(
  AValue: Variant; AEditValueFormat: TdxRangeTrackBarEditValueFormat;
  ADelimiter: Char; out AMin, AMax: Integer);
var
  ARange: Int64;
begin
  ARange := EditValueToRange(AEditValueFormat, ADelimiter, AValue);
  AMin := GetMinRange(ARange);
  AMax := GetMaxRange(ARange);
end;

procedure TdxCustomRangeTrackBarProperties.CalculateRangeByEditValue(
  AValue: Variant; out AMin, AMax: Integer);
begin
  CalculateRangeByEditValue(AValue, FEditValueFormat, FDelimiter, AMin, AMax);
end;

class function TdxCustomRangeTrackBarProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxRangeTrackBar;
end;

function TdxCustomRangeTrackBarProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := inherited GetSupportedOperations - [esoFiltering];
end;

procedure TdxCustomRangeTrackBarProperties.CheckRange(var AValue: Integer);
begin
  AValue := EnsureRange(AValue, Min, Max);
end;

procedure TdxCustomRangeTrackBarProperties.DoAssign(
  AProperties: TcxCustomEditProperties);
var
  ASourceRangeTrackBarProperties: TdxCustomRangeTrackBarProperties;
begin
  inherited DoAssign(AProperties);
  if AProperties is TdxCustomRangeTrackBarProperties then
  begin
    ASourceRangeTrackBarProperties := TdxCustomRangeTrackBarProperties(AProperties);
    EditValueFormat := ASourceRangeTrackBarProperties.EditValueFormat;
    ShowSelection := ASourceRangeTrackBarProperties.ShowSelection;
    OnDrawRangeMaxThumb := ASourceRangeTrackBarProperties.OnDrawRangeMaxThumb;
  end;
end;

procedure TdxCustomRangeTrackBarProperties.DoDrawRangeMinThumb(
  Sender: TObject; ACanvas: TcxCanvas; const ARect: TRect);
begin
  if Assigned(OnDrawRangeMinThumb) then
    OnDrawRangeMinThumb(Self, ACanvas, ARect);
end;

procedure TdxCustomRangeTrackBarProperties.DoDrawRangeMaxThumb(
  Sender: TObject; ACanvas: TcxCanvas; const ARect: TRect);
begin
  if Assigned(OnDrawRangeMaxThumb) then
    OnDrawRangeMaxThumb(Self, ACanvas, ARect);
end;

function TdxCustomRangeTrackBarProperties.EditValueToRange(const AEditValue: TcxEditValue): Int64;
begin
  Result := EditValueToRange(FEditValueFormat, FDelimiter, AEditValue);
end;

function TdxCustomRangeTrackBarProperties.EditValueToRange(AEditValueFormat: TdxRangeTrackBarEditValueFormat;
  ADelimiter: Char; const AEditValue: TcxEditValue): Int64;
var
  S: string;
  AMin, AMax: Integer;
  ASeparatorPos: Integer;
  ARange: Int64;
begin
  Result := GetRange(Min, Min);
  if not IsVarEmpty(AEditValue) then
    if AEditValueFormat = revfInt64 then
    begin
      if VarIsNumeric(AEditValue) then
        Result := VarAsType(AEditValue, varInt64)
      else
        if VarIsStr(AEditValue) and TryStrToInt64(AEditValue, ARange) then
          Result := ARange;
    end
    else
    begin
      if VarIsStr(AEditValue) then
      begin
        S := VarToStr(AEditValue);
        ASeparatorPos := Pos(ADelimiter, S);
        if ASeparatorPos <> 0 then
        begin
          AMin := StrToInt(Trim(Copy(S, 1, ASeparatorPos - 1)));
          AMax := StrToInt(Trim(Copy(S, ASeparatorPos + 1, Length(S))));
          Result := GetRange(AMin, AMax);
        end;
      end;
    end;
end;

class function TdxCustomRangeTrackBarProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxRangeTrackBarViewData;
end;

class function TdxCustomRangeTrackBarProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxCustomRangeTrackBarViewInfo;
end;

procedure TdxCustomRangeTrackBarProperties.PrepareDisplayValue(
  const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue;
  AEditFocused: Boolean);
var
  ARange: Int64;
  AMin, AMax: Integer;
begin
  LockUpdate(True);
  try
    ARange := EditValueToRange(AEditValue);
    AMin := GetMinRange(ARange);
    CheckRange(AMin);
    AMax := GetMaxRange(ARange);
    CheckRange(AMax);
    AMax := Math.Max(AMax, AMin);
    DisplayValue := GetRange(AMin, AMax);
  finally
    LockUpdate(False);
  end;
end;

function TdxCustomRangeTrackBarProperties.IsDelimiterStored: Boolean;
begin
  Result := FDelimiter <> DefaultRangeTrackBarValueDelimiter;
end;

function TdxCustomRangeTrackBarProperties.GetOnDrawMinThumb: TcxDrawThumbEvent;
begin
  Result := OnDrawThumb;
end;

procedure TdxCustomRangeTrackBarProperties.SetEditValueFormat(
  const Value: TdxRangeTrackBarEditValueFormat);
begin
  if FEditValueFormat <> Value then
  begin
    FEditValueFormat := Value;
    Changed;
  end;
end;

procedure TdxCustomRangeTrackBarProperties.SetDelimiter(const Value: Char);
begin
  if Value <> FDelimiter then
  begin
    FDelimiter := Value;
    Changed;
  end;
end;

procedure TdxCustomRangeTrackBarProperties.SetShowSelection(
  const Value: Boolean);
begin
  if Value <> FShowSelection then
  begin
    FShowSelection := Value;
    Changed;
  end;
end;

procedure TdxCustomRangeTrackBarProperties.SetOnDrawMinThumb(
  const Value: TcxDrawThumbEvent);
begin
  OnDrawThumb := Value;
end;

{ TdxRangeTrackBar }

function TdxRangeTrackBar.GetActiveProperties: TdxRangeTrackBarProperties;
begin
  Result := inherited ActiveProperties as TdxRangeTrackBarProperties;
end;

class function TdxRangeTrackBar.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxRangeTrackBarProperties;
end;

function TdxRangeTrackBar.GetProperties: TdxRangeTrackBarProperties;
begin
  Result := inherited Properties as TdxRangeTrackBarProperties;
end;

procedure TdxRangeTrackBar.SetProperties(
  const Value: TdxRangeTrackBarProperties);
begin
  inherited Properties := Value;
end;

{ TdxCustomRangeTrackBarViewInfo }

procedure TdxCustomRangeTrackBarViewInfo.Assign(Source: TObject);
begin
  inherited Assign(Source);
  if Source is TdxCustomRangeTrackBarViewInfo then
    RightThumb.Position := (Source as TdxCustomRangeTrackBarViewInfo).RightThumb.Position;
end;

procedure TdxCustomRangeTrackBarViewInfo.AddThumbs;
begin
  FLeftThumb := TdxRangeTrackBarLeftThumbViewInfo.Create(Self);
  AddThumb(FLeftThumb);
  FRightThumb := TdxRangeTrackBarRightThumbViewInfo.Create(Self);
  AddThumb(FRightThumb);
end;

procedure TdxCustomRangeTrackBarViewInfo.DoGetPositionHint(
  var AHintText: string; var ACanShow, AIsHintMultiLine: Boolean);
begin
  if Assigned(RangeTrackBarProperties.OnGetPositionHint) then
    RangeTrackBarProperties.OnGetPositionHint(Edit, GetMinThumb.Position, GetMaxThumb.Position,
      AHintText, ACanShow, AIsHintMultiLine);
end;

procedure TdxCustomRangeTrackBarViewInfo.DrawCustomThumb(
  ACanvas: TcxCanvas);
begin
  (TrackBarProperties as TdxCustomRangeTrackBarProperties).DoDrawRangeMinThumb(
    Self, ACanvas, GetRealRect(GetMinThumb.FullRect));
  (TrackBarProperties as TdxCustomRangeTrackBarProperties).DoDrawRangeMaxThumb(
    Self, ACanvas, GetRealRect(GetMaxThumb.FullRect));
end;

function TdxCustomRangeTrackBarViewInfo.DrawingThumbRectToRealThumbRect(
  ACanvas: TcxCanvas; const AThumbRect: TRect): TRect;
begin
  Result := AThumbRect;
end;

procedure TdxCustomRangeTrackBarViewInfo.DrawLeftThumb(ACanvas: TcxCanvas;
  const R: TRect; AState: Integer);
begin
  LookAndFeel.Painter.DrawRangeTrackBarScaledLeftThumb(ACanvas, R,
    TrackBarStateToButtonStates[AState], Orientation = tboHorizontal,
    TrackBarTicksToTicksAlign[TickMarks], GetThumbRealColor(AState), ScaleFactor);
end;

procedure TdxCustomRangeTrackBarViewInfo.DrawRightThumb(
  ACanvas: TcxCanvas; const R: TRect; AState: Integer);
begin
  LookAndFeel.Painter.DrawRangeTrackBarScaledRightThumb(ACanvas, R,
    TrackBarStateToButtonStates[AState], Orientation = tboHorizontal,
    TrackBarTicksToTicksAlign[TickMarks], GetThumbRealColor(AState), ScaleFactor);
end;

function TdxCustomRangeTrackBarViewInfo.GetDefaultPositionHintText: string;
begin
  Result := Format('%d - %d', [GetMinThumb.Position, GetMaxThumb.Position]);
end;

function TdxCustomRangeTrackBarViewInfo.GetMaxThumb: TcxTrackBarThumbViewInfo;
begin
  if ReverseDirection then
    Result := LeftThumb
  else
    Result := RightThumb;
end;

function TdxCustomRangeTrackBarViewInfo.GetMinThumb: TcxTrackBarThumbViewInfo;
begin
  if ReverseDirection then
    Result := RightThumb
  else
    Result := LeftThumb;
end;

procedure TdxCustomRangeTrackBarViewInfo.DrawThumb(ACanvas: TcxCanvas);
begin
  if not FRightThumb.FadingHelper.DrawImage(ACanvas.Handle, GetRealRect(FRightThumb.FullRect)) then
    DrawRightThumb(ACanvas, GetRealRect(FRightThumb.FullRect), FRightThumb.State);
  if not FLeftThumb.FadingHelper.DrawImage(ACanvas.Handle, GetThumbRect) then
    DrawLeftThumb(ACanvas, GetThumbRect, FLeftThumb.State);
end;

function TdxCustomRangeTrackBarViewInfo.IsPtToTheLeftFromThumb(
  AThumb: TcxTrackBarThumbViewInfo; X, Y: Integer): Boolean;
begin
  if AThumb = FRightThumb then
    Result := GetCurrentMousePos(X, Y) < FRightThumb.Bounds.Left
  else
    Result := inherited IsPtToTheLeftFromThumb(AThumb, X, Y);
end;

procedure TdxCustomRangeTrackBarViewInfo.UpdateSelection;
begin
  SelectionStart := GetMinThumb.Position;
  SelectionEnd := GetMaxThumb.Position;
end;

function TdxCustomRangeTrackBarViewInfo.IsPtToTheRightFromThumb(
  AThumb: TcxTrackBarThumbViewInfo; X, Y: Integer): Boolean;
begin
  if AThumb = FRightThumb then
    Result := GetCurrentMousePos(X, Y) > FRightThumb.Bounds.Right
  else
    Result := inherited IsPtToTheRightFromThumb(AThumb, X, Y);
end;

function TdxCustomRangeTrackBarViewInfo.IsTickText(
  ATickValue: Integer): Boolean;
begin
  Result := inherited IsTickText(ATickValue) or (ATickValue = GetMaxThumb.Position) or
    (ATickValue = GetMinThumb.Position);
end;

procedure TdxCustomRangeTrackBarViewInfo.UpdateTrackBarState;
begin
  if not Enabled then
  begin
    FLeftThumb.State := TUS_DISABLED;
    FRightThumb.State := TUS_DISABLED;
  end
  else
  begin
    FRightThumb.State := TUS_NORMAL;
    FLeftThumb.State := TUS_NORMAL;
    if tbmpSliding in MouseStates then
    begin
      if PressedThumb <> nil then
        PressedThumb.State := TUS_PRESSED;
    end
    else
      if tbmpUnderThumb in MouseStates then
        if HotThumb <> nil then
          HotThumb.State := TUS_HOT;
  end;
end;

procedure TdxCustomRangeTrackBarViewInfo.UpdateValue(
  AValue: TcxEditValue);
begin
  GetMinThumb.Position := GetMinRange(AValue);
  GetMaxThumb.Position := GetMaxRange(AValue);
end;

function TdxCustomRangeTrackBarViewInfo.GetRangeTrackBarProperties: TdxCustomRangeTrackBarProperties;
begin
  Result := TrackBarProperties as TdxCustomRangeTrackBarProperties;
end;

{ TdxRangeTrackBarLeftThumbFadingHelper }

procedure TdxRangeTrackBarLeftThumbFadingHelper.DrawThumb(ACanvas: TcxCanvas;
  const R: TRect; AState: Integer);
begin
  (ViewInfo as TdxCustomRangeTrackBarViewInfo).DrawLeftThumb(ACanvas, R, AState);
end;

{ TdxRangeTrackBarRightThumbFadingHelper }

procedure TdxRangeTrackBarRightThumbFadingHelper.DrawThumb(ACanvas: TcxCanvas;
  const R: TRect; AState: Integer);
begin
  ViewInfo.DrawRightThumb(ACanvas, R, AState);
end;

function TdxRangeTrackBarRightThumbFadingHelper.GetThumbRect: TRect;
begin
  Result := ViewInfo.GetRealRect(ViewInfo.RightThumb.FullRect);
end;

function TdxRangeTrackBarRightThumbFadingHelper.GetViewInfo: TdxCustomRangeTrackBarViewInfo;
begin
  Result := inherited ViewInfo as TdxCustomRangeTrackBarViewInfo;
end;

{ TdxRangeTrackBarLeftThumbViewInfo }

function TdxRangeTrackBarLeftThumbViewInfo.CreateFadingHelper: TcxTrackBarThumbFadingHelper;
begin
  Result := TdxRangeTrackBarLeftThumbFadingHelper.Create(ViewInfo);
end;

{ TdxRangeTrackBarRightThumbViewInfo }

function TdxRangeTrackBarRightThumbViewInfo.CreateFadingHelper: TcxTrackBarThumbFadingHelper;
begin
  Result := TdxRangeTrackBarRightThumbFadingHelper.Create(ViewInfo);
end;

{ TdxRangeTrackBarController }

procedure TdxRangeTrackBarController.KeyDown(var Key: Word; Shift: TShiftState);
var
  ANewPosition: Integer;
  ACurrentThumb, ANextThumb: TcxTrackBarThumbViewInfo;
  ADeltaPos: Integer;
  AIsRangeMoving, AIsLeftThumbMoving: Boolean;
begin
  AIsRangeMoving := [ssShift, ssCtrl] * Shift = [ssShift, ssCtrl];
  AIsLeftThumbMoving := ssShift in Shift;
  if AIsRangeMoving and (Key in [VK_PRIOR, VK_HOME, VK_LEFT, VK_UP]) or
    not AIsRangeMoving and AIsLeftThumbMoving then
  begin
    ACurrentThumb := ViewInfo.LeftThumb;
    ANextThumb := ViewInfo.RightThumb;
  end
  else
  begin
    ACurrentThumb := ViewInfo.RightThumb;
    ANextThumb := ViewInfo.LeftThumb;
  end;

  ADeltaPos := ANextThumb.Position - ACurrentThumb.Position;
  ANewPosition := GetPositionAfterKeyDown(ACurrentThumb, Key, Shift);
  if ANewPosition <> ACurrentThumb.Position then
  begin
    SetPosition(ANewPosition, ACurrentThumb);
    if AIsRangeMoving then
       SetPosition(ANewPosition + ADeltaPos, ANextThumb);
  end;
end;

procedure TdxRangeTrackBarController.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ANewPosition: Integer;
  AThumb: TcxTrackBarThumbViewInfo;
begin
  ViewInfo.HotThumb := ViewInfo.GetThumbFromPos(cxPoint(X, Y));
  ViewInfo.PressedThumb := ViewInfo.HotThumb;
  if (Button = mbLeft) and
    PtInRect(ViewInfo.GetTrackBounds, Point(X, Y)) then
  begin
    if ViewInfo.PressedThumb <> nil then
    begin
      Include(ViewInfo.MouseStates, tbmpSliding);
        StoredPosition := GetPosition(ViewInfo.PressedThumb);
    end
    else
    begin
      ANewPosition := ViewInfo.GetNewPosition(X, Y);
      if ActiveProperties.ThumbStep = cxtsJump then
      begin
        Include(ViewInfo.MouseStates, tbmpSliding);
        StoredPosition := ANewPosition;
      end;
      AThumb := ViewInfo.GetNearestThumb(X, Y);
      UpdatePos(ANewPosition, AThumb);
      if ActiveProperties.ThumbStep = cxtsJump then
      begin
        ViewInfo.HotThumb := AThumb;
        ViewInfo.PressedThumb := AThumb;
      end;
    end;
  end;
end;

procedure TdxRangeTrackBarController.UpdatePos(AValue: Integer; AThumb: TcxTrackBarThumbViewInfo);
begin
  SetPosition(AValue, AThumb);
  ViewInfo.ProcessPositionHint;
end;

function TdxRangeTrackBarController.GetPosition(
  AThumb: TcxTrackBarThumbViewInfo): Integer;
begin
  if AThumb = ViewInfo.GetMinThumb then
    Result := Range.Min
  else
    Result := Range.Max;
end;

function TdxRangeTrackBarController.GetRange: TdxTrackBarRange;
begin
  Result := (Trackbar as TdxCustomRangeTrackBar).Range;
end;

function TdxRangeTrackBarController.GetViewInfo: TdxCustomRangeTrackBarViewInfo;
begin
  Result := inherited ViewInfo as TdxCustomRangeTrackBarViewInfo;
end;

procedure TdxRangeTrackBarController.SetPosition(AValue: Integer;
  AThumb: TcxTrackBarThumbViewInfo);
begin
  if AThumb = ViewInfo.GetMinThumb then
    Range.DoSetMin(AValue, True)
  else
    Range.DoSetMax(AValue, True);
end;

{ TdxRangeTrackBarViewData }

procedure TdxRangeTrackBarViewData.CalculateThumbSize(
  AViewInfo: TcxCustomTrackBarViewInfo);
var
  ARangeTrackBarViewInfo: TdxCustomRangeTrackBarViewInfo;
  ASize: TSize;
begin
  inherited CalculateThumbSize(AViewInfo);
  ARangeTrackBarViewInfo := AViewInfo as TdxCustomRangeTrackBarViewInfo;
  if AViewInfo.UseSkins then
  begin
    ASize := AViewInfo.Painter.RangeTrackBarScaledLeftThumbSize(
      AViewInfo.Orientation = tboHorizontal, TrackBarTicksToTicksAlign[AViewInfo.TickMarks], ScaleFactor);
    ARangeTrackBarViewInfo.FLeftThumbWidth := ASize.cx;
    ASize := AViewInfo.Painter.RangeTrackBarScaledRightThumbSize(
      AViewInfo.Orientation = tboHorizontal, TrackBarTicksToTicksAlign[AViewInfo.TickMarks], ScaleFactor);
    ARangeTrackBarViewInfo.FRightThumbWidth := ASize.cx;
    AViewInfo.ThumbSize := ARangeTrackBarViewInfo.FLeftThumbWidth + ARangeTrackBarViewInfo.FRightThumbWidth - 1;
    AViewInfo.ThumbLargeSize := ASize.cy;
  end
  else
  begin
    ARangeTrackBarViewInfo.FLeftThumbWidth := ThumbHalfSize(AViewInfo) + 1;
    ARangeTrackBarViewInfo.FRightThumbWidth := ARangeTrackBarViewInfo.FLeftThumbWidth;
  end;
end;

procedure TdxRangeTrackBarViewData.CalculateThumbRect(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomTrackBarViewInfo);
var
  ARangeViewInfo: TdxCustomRangeTrackBarViewInfo;
  R: TRect;
begin
  ARangeViewInfo := AViewInfo as TdxCustomRangeTrackBarViewInfo;

  R.Right := ARangeViewInfo.GetCoordinate(ARangeViewInfo.LeftThumb.Position) + 1;
  R.Left := R.Right - ARangeViewInfo.FLeftThumbWidth;
  R.Top := ARangeViewInfo.TrackRect.Top +
    (ARangeViewInfo.TrackSize - ARangeViewInfo.ThumbLargeSize) div 2;
  R.Bottom := R.Top + ARangeViewInfo.ThumbLargeSize;
  ARangeViewInfo.LeftThumb.Bounds := ARangeViewInfo.DrawingThumbRectToRealThumbRect(ACanvas, R);
  ARangeViewInfo.LeftThumb.FullRect := R;

  ARangeViewInfo.VisibleThumbRect := ARangeViewInfo.LeftThumb.Bounds;
  ARangeViewInfo.ThumbRect := ARangeViewInfo.LeftThumb.FullRect;

  R.Left := ARangeViewInfo.GetCoordinate(ARangeViewInfo.RightThumb.Position);
  R.Right := R.Left + ARangeViewInfo.FRightThumbWidth;
  R.Top := ARangeViewInfo.TrackRect.Top +
    (ARangeViewInfo.TrackSize - ARangeViewInfo.ThumbLargeSize) div 2;
  R.Bottom := R.Top + ARangeViewInfo.ThumbLargeSize;
  ARangeViewInfo.RightThumb.Bounds := ARangeViewInfo.DrawingThumbRectToRealThumbRect(ACanvas, R);
  ARangeViewInfo.RightThumb.FullRect := R;
end;

procedure TdxRangeTrackBarViewData.UpdateSelection(
  AViewInfo: TcxCustomTrackBarViewInfo);
var
  ARangeTrackBarViewInfo: TdxCustomRangeTrackBarViewInfo;
begin
  ARangeTrackBarViewInfo := AViewInfo as TdxCustomRangeTrackBarViewInfo;
  ARangeTrackBarViewInfo.ShowSelection := Properties.ShowSelection;
  ARangeTrackBarViewInfo.UpdateSelection;
end;

procedure TdxRangeTrackBarViewData.UpdateValue(
  AViewInfo: TcxCustomTrackBarViewInfo; AValue: TcxEditValue);
begin
  AViewInfo.ReverseDirection := IsReverseDirection;
  inherited UpdateValue(AViewInfo, AValue);
end;

function TdxRangeTrackBarViewData.GetProperties: TdxCustomRangeTrackBarProperties;
begin
  Result := inherited Properties as TdxCustomRangeTrackBarProperties;
end;

initialization
  GetRegisteredEditProperties.Register(TdxRangeTrackBarProperties, scxSEditRepositoryRangeTrackBarItem);

finalization
  GetRegisteredEditProperties.Unregister(TdxRangeTrackBarProperties);

end.
