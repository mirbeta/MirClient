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

unit dxToggleSwitch;

{$I cxVer.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics,
  cxGraphics, dxGDIPlusClasses, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxClasses, cxContainer,
  cxEditConsts, cxEdit, cxCheckBox;

type
  TdxCustomToggleSwitchViewData = class;
  TdxCustomToggleSwitchViewInfo = class;

  { TdxToggleSwitchThumbFadingHelper }

  TdxToggleSwitchThumbFadingHelper = class(TcxCustomCheckControlFadingHelper)
  private
    function GetToggleSwitchViewInfo: TdxCustomToggleSwitchViewInfo;
  protected
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap); override;
  public
    procedure Invalidate; override;
  end;

  TdxToggleSwitchStateIndicatorPosition = (sipOutside, sipInside);
  TdxToggleSwitchStateIndicatorKind = (sikNone, sikText, sikGlyph);

  { TdxToggleSwitchStateIndicator }

  TdxToggleSwitchStateIndicator = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;

    FOffGlyph: TdxSmartGlyph;
    FOffText: string;
    FOnGlyph: TdxSmartGlyph;
    FOnText: string;
    FPosition: TdxToggleSwitchStateIndicatorPosition;
    FKind: TdxToggleSwitchStateIndicatorKind;
  private
    procedure GlyphChangeHandler(Sender: TObject);

    function IsOffTextStored: Boolean;
    function IsOnTextStored: Boolean;

    procedure SetKind(AValue: TdxToggleSwitchStateIndicatorKind);
    procedure SetOffGlyph(AValue: TdxSmartGlyph);
    procedure SetOffText(const AValue: string);
    procedure SetOnGlyph(AValue: TdxSmartGlyph);
    procedure SetOnText(const AValue: string);
    procedure SetPosition(AValue: TdxToggleSwitchStateIndicatorPosition);

    procedure Changed;
  protected
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Kind: TdxToggleSwitchStateIndicatorKind read FKind write SetKind default sikNone;
    property OffGlyph: TdxSmartGlyph read fOffGlyph write SetOffGlyph;
    property OffText: string read FOffText write SetOffText stored IsOffTextStored;
    property OnGlyph: TdxSmartGlyph read FOnGlyph write SetOnGlyph;
    property OnText: string read FOnText write SetOnText stored IsOnTextStored;
    property Position: TdxToggleSwitchStateIndicatorPosition read FPosition write SetPosition default sipOutside;
  end;

  { TcxCustomToggleSwitchProperties }

  TdxCustomToggleSwitchProperties = class(TcxCustomCheckBoxProperties)
  private
    FSmoothToggle: Boolean;
    FStateText: TdxToggleSwitchStateIndicator;

    procedure SetSmoothToggle(AValue: Boolean);
    procedure SetStateText(AValue: TdxToggleSwitchStateIndicator);
  protected
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    procedure StateTextChangedHandler(Sender: TObject);
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;

    property SmoothToggle: Boolean read FSmoothToggle write SetSmoothToggle default True;
    property StateIndicator: TdxToggleSwitchStateIndicator read FStateText write SetStateText;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    class function GetContainerClass: TcxContainerClass; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;

    property Alignment default taRightJustify;
  end;

  { TcxCustomToggleSwitchViewData }

  TdxCustomToggleSwitchViewData = class(TcxCustomCheckBoxViewData)
  private
    function GetDefaultHeight: Integer;
    function GetDefaultWidth: Integer;
    function GetProperties: TdxCustomToggleSwitchProperties;
    function CalculateIndicatorRect(AViewInfo: TdxCustomToggleSwitchViewInfo;
      AIndicatoSize: TSize; AAlign: TLeftRight): TRect;
  protected
    function CalculateCheckBoxRect(AViewInfo: TcxCustomCheckBoxViewInfo): TRect; override;
    function CalculateCheckBoxState(AViewInfo: TcxCustomCheckBoxViewInfo; const P: TPoint;
      AButton: TcxMouseButton; AShift: TShiftState): TcxEditCheckState; override;
    procedure CalculateFocusRect(AViewInfo: TcxCustomCheckBoxViewInfo); override;
    function CalculateTextRect(AViewInfo: TcxCustomCheckBoxViewInfo): TRect; override;
    function GetTextGap: Integer; override;
    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
      AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize;
      AViewInfo: TcxCustomEditViewInfo): TSize; override;

    function CalculateOffIndicatorRect(AViewInfo: TdxCustomToggleSwitchViewInfo): TRect; virtual;
    function CalculateOnIndicatorRect(AViewInfo: TdxCustomToggleSwitchViewInfo): TRect; virtual;
    procedure CalculateStateTextFont(AViewInfo: TdxCustomToggleSwitchViewInfo); virtual;
    procedure CalculateStateIndicatorRectSizes(AViewInfo: TdxCustomToggleSwitchViewInfo); virtual;
    procedure CalculateStateIndicatorSizes(AViewInfo: TdxCustomToggleSwitchViewInfo); virtual;
    function CalculateThumbBounds(AViewInfo: TdxCustomToggleSwitchViewInfo): TRect; virtual;
    function CalculateToggleSwitchSize(AViewInfo: TdxCustomToggleSwitchViewInfo): TSize; virtual;
    function GetStateIndicatorGap(AViewInfo: TdxCustomToggleSwitchViewInfo): Integer; virtual;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect;
      const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
      AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    function GetBorderExtent: TRect; override;

    property Properties: TdxCustomToggleSwitchProperties read GetProperties;
  end;

  { TdxToggleSwitchViewInfo }

  TdxCustomToggleSwitchViewInfo = class(TcxCustomCheckBoxViewInfo)
  private
    FThumbBounds: TRect;
    FThumbCapture: Boolean;
    FThumbDelta: Integer;
    FToggleSwitchBounds: TRect;

    FActiveStateTextFont: TFont;
    FNormalStateTextFont: TFont;
    FOffGlyph: TdxSmartGlyph;
    FOffText: string;
    FOffIndicatorRect: TRect;
    FOnGlyph: TdxSmartGlyph;
    FOnText: string;
    FOnIndicatorRect: TRect;
    FStateIndicatorPosition: TdxToggleSwitchStateIndicatorPosition;
    FStateIndicatorKind: TdxToggleSwitchStateIndicatorKind;

    function GetThumbDelta: Integer;
    function GetThumbMaxDelta: Integer;
    function GetThumbState: TcxButtonState;
    function GetThumbStepAnimation: Integer;
  protected
    OffIndicatorRectSize: TSize;
    OffIndicatorSize: TSize;
    OnIndicatorRectSize: TSize;
    OnIndicatorSize: TSize;

    function GetLeftIndicatorWidth: Integer;
    function GetRightIndicatorWidth: Integer;

    procedure InternalPaint(ACanvas: TcxCanvas); override;
    function GetCheckControlFadingHelperClass: TcxCheckControlFadingHelperClass; override;

    procedure DrawStateGlyph(ACanvas: TcxCanvas); virtual;
    procedure DrawStateText(ACanvas: TcxCanvas); virtual;
    procedure DrawThumb(ACanvas: TcxCanvas; AClientRect: TRect; AState: TcxButtonState); virtual;
    function NeedShowOffIndicator: Boolean; virtual;
    function NeedShowOnIndicator: Boolean; virtual;

    property ActiveStateTextFont: TFont read FActiveStateTextFont;
    property NormalStateTextFont: TFont read FNormalStateTextFont;
    property OffText: string read FOffText write FOffText;
    property OffIndicatorRect: TRect read FOffIndicatorRect write FOffIndicatorRect;
    property OnText: string read FOnText write FOnText;
    property OnIndicatorRect: TRect read FOnIndicatorRect write FOnIndicatorRect;
    property StateIndicatorPosition: TdxToggleSwitchStateIndicatorPosition read FStateIndicatorPosition write FStateIndicatorPosition;
    property StateIndicatorKind: TdxToggleSwitchStateIndicatorKind read FStateIndicatorKind write FStateIndicatorKind;
    property ThumbState: TcxButtonState read GetThumbState;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Offset(DX: Integer; DY: Integer); override;

    property ThumbBounds: TRect read FThumbBounds write FThumbBounds;
    property ThumbCapture: Boolean read FThumbCapture write FThumbCapture;
    property ThumbDelta: Integer read GetThumbDelta write FThumbDelta;
    property ThumbMaxDelta: Integer read GetThumbMaxDelta;
    property ThumbStepAnimation: Integer read GetThumbStepAnimation;
    property ToggleSwitchBounds: TRect read FToggleSwitchBounds write FToggleSwitchBounds;
  end;

  { TdxCustomToggleSwitch }

  TdxCustomToggleSwitch = class(TcxCustomCheckBox)
  private
    FPrevMousePos: Integer;
    FThumbDragged: Boolean;
    FTimer: TcxTimer;

    function GetActiveProperties: TdxCustomToggleSwitchProperties;
    function GetProperties: TdxCustomToggleSwitchProperties;
    function GetViewInfo: TdxCustomToggleSwitchViewInfo;
    procedure SetProperties(Value: TdxCustomToggleSwitchProperties);
  protected
    procedure DoProcessEventsOnViewInfoChanging; override;
    procedure Initialize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;

    procedure StartAnimation;
    procedure StopAnimation;
    procedure TimerHandler(Sender: TObject);
    function GetShadowBounds: TRect; override;

    property ActiveProperties: TdxCustomToggleSwitchProperties read GetActiveProperties;
    property Properties: TdxCustomToggleSwitchProperties read GetProperties write SetProperties;
    property ViewInfo: TdxCustomToggleSwitchViewInfo read GetViewInfo;
  public
    destructor Destroy; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
  end;


  { TdxToggleSwitchProperties }

  TdxToggleSwitchProperties = class(TdxCustomToggleSwitchProperties)
  published
    property Alignment;
    property AssignedValues;
    property ClearKey;
    property DisplayChecked;
    property DisplayUnchecked;
    property DisplayGrayed;
    property FullFocusRect;
    property ImmediatePost;
    property MultiLine;
    property ReadOnly;
    property ShowEndEllipsis;
    property StateIndicator;
    property UseAlignmentWhenInplace;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property ValueChecked;
    property ValueGrayed;
    property ValueUnchecked;
    property OnChange;
    property OnEditValueChanged;
    property OnValidate;
  end;

  { TdxToggleSwitch }

  TdxToggleSwitch = class(TdxCustomToggleSwitch)
  private
    function GetActiveProperties: TdxToggleSwitchProperties;
    function GetProperties: TdxToggleSwitchProperties;
    procedure SetProperties(Value: TdxToggleSwitchProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TdxToggleSwitchProperties read GetActiveProperties;
  published
    property Action;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Checked;
    property Constraints;
    property Enabled;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TdxToggleSwitchProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnEditing;
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
  end;

implementation

uses
  Types, Variants, Math,
  dxCore, cxGeometry, cxFilterControlUtils;

const
  dxToggleSwitchDefaultHeight: Integer = 21;
  dxToggleSwitchDefaultWidth: Integer = 70;
  dxToggleSwitchInplaceHeight: Integer = 15;
  dxToggleSwitchTextOffset: Integer = 7;
  dxDefaultToggleSwitchInsideStateIndicatorOffset: Integer = 3;

{ TdxToggleSwitchThumbFadingHelper }

procedure TdxToggleSwitchThumbFadingHelper.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

  function PrepareImage(AState: TcxButtonState): TcxBitmap32;
  begin
    Result := TcxBitmap32.CreateSize(GetToggleSwitchViewInfo.ThumbBounds, True);
    GetToggleSwitchViewInfo.DrawThumb(Result.cxCanvas, Result.ClientRect, AState);
  end;

begin
  AFadeInImage := PrepareImage(cxbsHot);
  AFadeOutImage := PrepareImage(cxbsNormal);
end;

procedure TdxToggleSwitchThumbFadingHelper.Invalidate;
begin
  Invalidate(GetToggleSwitchViewInfo.ThumbBounds, False);
end;

function TdxToggleSwitchThumbFadingHelper.GetToggleSwitchViewInfo: TdxCustomToggleSwitchViewInfo;
begin
  Result := TdxCustomToggleSwitchViewInfo(ViewInfo);
end;

{ TdxCustomToggleSwitch }

destructor TdxCustomToggleSwitch.Destroy;
begin
  if not IsInplace then
    StopAnimation;
  inherited;
end;

procedure TdxCustomToggleSwitch.DoProcessEventsOnViewInfoChanging;

  function NeedToggle: Boolean;
  begin
    Result := (FThumbDragged and (ViewInfo.ThumbDelta > Round(ViewInfo.ThumbMaxDelta / 2))) or
      (not FThumbDragged and (ViewInfo.CheckBoxState = ecsHot));
  end;

begin
  if (IsInplace or Focused) and(ViewInfo.CheckBoxLastState = ecsPressed) and (ViewInfo.CheckBoxState <> ecsPressed) and
    not ActiveProperties.ReadOnly then
  begin
    if not IsInplace and ActiveProperties.SmoothToggle then
    begin
      if (EditValue = Null) then
        Toggle
      else
        if NeedToggle then
        begin
          ViewInfo.ThumbDelta := ViewInfo.ThumbMaxDelta - ViewInfo.ThumbDelta;
          Toggle;
        end;
      FThumbDragged := False;
      StartAnimation;
    end
    else
      if ViewInfo.CheckBoxState = ecsHot then
        Toggle;
  end;
end;

procedure TdxCustomToggleSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not IsInplace and (Button = mbLeft) then
    StopAnimation;
  inherited MouseDown(Button, Shift, X, Y);
  ViewInfo.ThumbCapture := Focused and not ActiveProperties.ReadOnly and (Button = mbLeft) and cxRectPtIn(ViewInfo.ThumbBounds, X, Y);
  if ViewInfo.ThumbCapture then
    FPrevMousePos := X;
end;

procedure TdxCustomToggleSwitch.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not IsInplace and ActiveProperties.SmoothToggle and ViewInfo.ThumbCapture and (Abs(X - FPrevMousePos) > 0) then
  begin
    if State = cbsUnchecked then
      ViewInfo.ThumbDelta := ViewInfo.ThumbDelta + (X - FPrevMousePos)
    else
      ViewInfo.ThumbDelta := ViewInfo.ThumbDelta - (X - FPrevMousePos);

    Invalidate;
    FPrevMousePos := X;
    FThumbDragged := True;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TdxCustomToggleSwitch.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ViewInfo.ThumbCapture := False;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TdxCustomToggleSwitch.StartAnimation;
begin
  FTimer := TcxTimer.Create(nil);
  FTimer.Interval := 10;
  FTimer.OnTimer := TimerHandler;
end;

procedure TdxCustomToggleSwitch.StopAnimation;
begin
  FreeAndNil(FTimer);
  ShortRefreshContainer(False);
end;

procedure TdxCustomToggleSwitch.TimerHandler(Sender: TObject);
begin
  if ViewInfo.ThumbDelta < ViewInfo.ThumbStepAnimation then
  begin
    ViewInfo.ThumbDelta := 0;
    StopAnimation;
  end
  else
  begin
    ViewInfo.ThumbDelta := ViewInfo.ThumbDelta - ViewInfo.ThumbStepAnimation;
    ShortRefreshContainer(False);
  end;
end;

function TdxCustomToggleSwitch.GetActiveProperties: TdxCustomToggleSwitchProperties;
begin
  Result := TdxCustomToggleSwitchProperties(InternalGetActiveProperties);
end;

function TdxCustomToggleSwitch.GetProperties: TdxCustomToggleSwitchProperties;
begin
  Result := TdxCustomToggleSwitchProperties(inherited Properties);
end;

class function TdxCustomToggleSwitch.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxCustomToggleSwitchProperties;
end;

function TdxCustomToggleSwitch.GetShadowBounds: TRect;
begin
  Result := GetControlRect(Self);
end;

function TdxCustomToggleSwitch.GetViewInfo: TdxCustomToggleSwitchViewInfo;
begin
  Result := TdxCustomToggleSwitchViewInfo(FViewInfo);
end;

procedure TdxCustomToggleSwitch.Initialize;
begin
  inherited;
  Width := 161;
end;

procedure TdxCustomToggleSwitch.SetProperties(Value: TdxCustomToggleSwitchProperties);
begin
  Properties.Assign(Value);
end;

{ TdxToggleSwitchStateIndicator }

constructor TdxToggleSwitchStateIndicator.Create;
begin
  inherited;
  FOffGlyph := TdxSmartGlyph.Create;
  FOffGlyph.OnChange := GlyphChangeHandler;
  FOnGlyph := TdxSmartGlyph.Create;
  FOnGlyph.OnChange := GlyphChangeHandler;
  FOffText := cxGetResourceString(@sdxDefaultToggleSwitchOffText);
  FOnText := cxGetResourceString(@sdxDefaultToggleSwitchOnText);
end;

destructor TdxToggleSwitchStateIndicator.Destroy;
begin
  FreeAndNil(FOnGlyph);
  FreeAndNil(FOffGlyph);
  inherited;
end;

procedure TdxToggleSwitchStateIndicator.Assign(Source: TPersistent);
var
  AToggleSwitchStateIndicator: TdxToggleSwitchStateIndicator;
begin
  if Source is TdxToggleSwitchStateIndicator then
  begin
    AToggleSwitchStateIndicator := TdxToggleSwitchStateIndicator(Source);
    OffGlyph := AToggleSwitchStateIndicator.OffGlyph;
    OnGlyph := AToggleSwitchStateIndicator.OnGlyph;
    OffText := AToggleSwitchStateIndicator.OffText;
    OnText := AToggleSwitchStateIndicator.OnText;
    Position := AToggleSwitchStateIndicator.Position;
    Kind := AToggleSwitchStateIndicator.Kind;
  end
  else
    inherited;
end;

procedure TdxToggleSwitchStateIndicator.Changed;
begin
  dxCallNotify(FOnChanged, Self);
end;

procedure TdxToggleSwitchStateIndicator.GlyphChangeHandler(Sender: TObject);
begin
  Changed;
end;

function TdxToggleSwitchStateIndicator.IsOffTextStored: Boolean;
begin
  Result := FOffText <> cxGetResourceString(@sdxDefaultToggleSwitchOffText);
end;

function TdxToggleSwitchStateIndicator.IsOnTextStored: Boolean;
begin
  Result := FOnText <> cxGetResourceString(@sdxDefaultToggleSwitchOnText);
end;

procedure TdxToggleSwitchStateIndicator.SetKind(AValue: TdxToggleSwitchStateIndicatorKind);
begin
  if FKind <> AValue then
  begin
    FKind := AValue;
    Changed;
  end;
end;

procedure TdxToggleSwitchStateIndicator.SetOffGlyph(AValue: TdxSmartGlyph);
begin
  FOffGlyph.Assign(AValue);
  Changed;
end;

procedure TdxToggleSwitchStateIndicator.SetOffText(const AValue: string);
begin
  if FOffText <> AValue then
  begin
    FOffText := AValue;
    Changed;
  end;
end;

procedure TdxToggleSwitchStateIndicator.SetOnGlyph(AValue: TdxSmartGlyph);
begin
  FOnGlyph.Assign(AValue);
  Changed;
end;

procedure TdxToggleSwitchStateIndicator.SetOnText(const AValue: string);
begin
  if FOnText <> AValue then
  begin
    FOnText := AValue;
    Changed;
  end;
end;

procedure TdxToggleSwitchStateIndicator.SetPosition(AValue: TdxToggleSwitchStateIndicatorPosition);
begin
  if FPosition <> AValue then
  begin
    FPosition := AValue;
    Changed;
  end;
end;

{ TcxCustomToggleSwitchProperties }

constructor TdxCustomToggleSwitchProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FStateText := TdxToggleSwitchStateIndicator.Create;
  FSmoothToggle := True;
  FAlignment.Horz := taRightJustify;
  FStateText.OnChanged := StateTextChangedHandler;
end;

destructor TdxCustomToggleSwitchProperties.Destroy;
begin
  FreeAndNil(FStateText);
  inherited;
end;

procedure TdxCustomToggleSwitchProperties.DoAssign(AProperties: TcxCustomEditProperties);
var
  AToggleSwitchProperties: TdxCustomToggleSwitchProperties;
begin
  if AProperties is TdxCustomToggleSwitchProperties then
  begin
    AToggleSwitchProperties := TdxCustomToggleSwitchProperties(AProperties);
    SmoothToggle := AToggleSwitchProperties.SmoothToggle;
    StateIndicator := AToggleSwitchProperties.StateIndicator;
  end;
  inherited;
end;

class function TdxCustomToggleSwitchProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxToggleSwitch;
end;

class function TdxCustomToggleSwitchProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxCustomToggleSwitchViewInfo;
end;

class function TdxCustomToggleSwitchProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxCustomToggleSwitchViewData;
end;

procedure TdxCustomToggleSwitchProperties.SetSmoothToggle(AValue: Boolean);
begin
  if AValue <> FSmoothToggle then
  begin
    FSmoothToggle := AValue;
    Changed;
  end;
end;

procedure TdxCustomToggleSwitchProperties.SetStateText(AValue: TdxToggleSwitchStateIndicator);
begin
  FStateText.Assign(AValue);
  Changed;
end;

procedure TdxCustomToggleSwitchProperties.StateTextChangedHandler(Sender: TObject);
begin
  Changed;
end;

{ TdxCustomToggleSwitchViewData }

procedure TdxCustomToggleSwitchViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
  AIsMouseEvent: Boolean);
var
  AToggleSwitchViewInfo: TdxCustomToggleSwitchViewInfo;
  AProperties: TdxCustomToggleSwitchProperties;
begin
  inherited;
  AProperties := TdxCustomToggleSwitchProperties(Properties);
  AToggleSwitchViewInfo := TdxCustomToggleSwitchViewInfo(AViewInfo);
  AToggleSwitchViewInfo.OffText := AProperties.StateIndicator.OffText;
  AToggleSwitchViewInfo.OnText := AProperties.StateIndicator.OnText;
  AToggleSwitchViewInfo.FOffGlyph := AProperties.StateIndicator.OffGlyph;
  AToggleSwitchViewInfo.FOnGlyph := AProperties.StateIndicator.OnGlyph;
  AToggleSwitchViewInfo.StateIndicatorPosition := AProperties.StateIndicator.Position;
  AToggleSwitchViewInfo.StateIndicatorKind := AProperties.StateIndicator.Kind;
  AToggleSwitchViewInfo.CheckBoxSize := CalculateToggleSwitchSize(AToggleSwitchViewInfo);
  CalculateStateTextFont(AToggleSwitchViewInfo);
  CalculateStateIndicatorSizes(AToggleSwitchViewInfo);
  CalculateStateIndicatorRectSizes(AToggleSwitchViewInfo);
  AToggleSwitchViewInfo.CheckBoxRect := CalculateCheckBoxRect(AToggleSwitchViewInfo);
  AToggleSwitchViewInfo.TextRect := CalculateTextRect(AToggleSwitchViewInfo);
  AToggleSwitchViewInfo.ToggleSwitchBounds := AToggleSwitchViewInfo.CheckBoxRect;
  AToggleSwitchViewInfo.ThumbBounds := CalculateThumbBounds(AToggleSwitchViewInfo);
  if UseRightToLeftAlignment then
    AToggleSwitchViewInfo.ThumbBounds := TdxRightToLeftLayoutConverter.ConvertRect(AToggleSwitchViewInfo.ThumbBounds,
      AToggleSwitchViewInfo.ToggleSwitchBounds);
  CalculateFocusRect(AToggleSwitchViewInfo);
  if UseRightToLeftAlignment then
  begin
    AToggleSwitchViewInfo.OffIndicatorRect := CalculateIndicatorRect(AToggleSwitchViewInfo,
      AToggleSwitchViewInfo.OffIndicatorRectSize, taRightJustify);
    AToggleSwitchViewInfo.OnIndicatorRect := CalculateIndicatorRect(AToggleSwitchViewInfo,
      AToggleSwitchViewInfo.OnIndicatorRectSize, taLeftJustify);
  end
  else
  begin
    AToggleSwitchViewInfo.OffIndicatorRect := CalculateIndicatorRect(AToggleSwitchViewInfo,
      AToggleSwitchViewInfo.OffIndicatorRectSize, taLeftJustify);
    AToggleSwitchViewInfo.OnIndicatorRect := CalculateIndicatorRect(AToggleSwitchViewInfo,
      AToggleSwitchViewInfo.OnIndicatorRectSize, taRightJustify);
  end;
end;

function TdxCustomToggleSwitchViewData.GetBorderExtent: TRect;
begin
  if IsInplace then
    Result := inherited GetBorderExtent
  else
    Result := cxNullRect;
end;

function TdxCustomToggleSwitchViewData.CalculateCheckBoxRect(AViewInfo: TcxCustomCheckBoxViewInfo): TRect;
var
  AToggleSwitchViewInfo: TdxCustomToggleSwitchViewInfo;
begin
  Result := inherited CalculateCheckBoxRect(AViewInfo);
  if not IsInplace then
    Result := cxRectSetWidth(Result, AViewInfo.CheckBoxSize.cx);

  AToggleSwitchViewInfo := TdxCustomToggleSwitchViewInfo(AViewInfo);
  if (AToggleSwitchViewInfo.StateIndicatorKind <> sikNone) and (AToggleSwitchViewInfo.StateIndicatorPosition = sipOutside) then
  begin
    case AToggleSwitchViewInfo.Alignment of
      taLeftJustify:
        if AToggleSwitchViewInfo.GetLeftIndicatorWidth > 0 then
          Result := cxRectSetLeft(Result,
            Result.Left + AToggleSwitchViewInfo.GetLeftIndicatorWidth + GetStateIndicatorGap(AToggleSwitchViewInfo));
      taRightJustify:
        if AToggleSwitchViewInfo.GetRightIndicatorWidth > 0 then
          Result := cxRectSetRight(Result,
            Result.Right - AToggleSwitchViewInfo.GetRightIndicatorWidth - GetStateIndicatorGap(AToggleSwitchViewInfo));
    end;
  end;
end;

function TdxCustomToggleSwitchViewData.CalculateCheckBoxState(AViewInfo: TcxCustomCheckBoxViewInfo; const P: TPoint;
  AButton: TcxMouseButton; AShift: TShiftState): TcxEditCheckState;
begin
  Result := inherited CalculateCheckBoxState(AViewInfo, P, AButton, AShift);
  if TdxCustomToggleSwitchViewInfo(AViewInfo).ThumbCapture then
    Result := ecsPressed;
end;

procedure TdxCustomToggleSwitchViewData.CalculateFocusRect(AViewInfo: TcxCustomCheckBoxViewInfo);
begin
  inherited;
  if not Properties.FullFocusRect then
    Inc(AViewInfo.FocusRect.Bottom)
  else
    InflateRect(AViewInfo.FocusRect, 0, -1);
end;

function TdxCustomToggleSwitchViewData.CalculateTextRect(AViewInfo: TcxCustomCheckBoxViewInfo): TRect;
var
  AToggleSwitchViewInfo: TdxCustomToggleSwitchViewInfo;
begin
  Result := inherited CalculateTextRect(AViewInfo);
  AToggleSwitchViewInfo := TdxCustomToggleSwitchViewInfo(AViewInfo);
  if (cxRectWidth(Result) > 0) and (AToggleSwitchViewInfo.StateIndicatorKind <> sikNone) and
    (AToggleSwitchViewInfo.StateIndicatorPosition = sipOutside) then
      case AViewInfo.Alignment of
        taLeftJustify:
          if AToggleSwitchViewInfo.GetRightIndicatorWidth > 0 then
            Result.Left := Result.Left + AToggleSwitchViewInfo.GetRightIndicatorWidth + GetStateIndicatorGap(AToggleSwitchViewInfo);
        taRightJustify:
          if AToggleSwitchViewInfo.GetLeftIndicatorWidth > 0 then
            Result.Right := Result.Right - AToggleSwitchViewInfo.GetLeftIndicatorWidth - GetStateIndicatorGap(AToggleSwitchViewInfo);
      end;
end;

function TdxCustomToggleSwitchViewData.GetTextGap: Integer;
begin
  Result := ScaleFactor.Apply(dxToggleSwitchTextOffset)
end;

function TdxCustomToggleSwitchViewData.InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
  AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize;

  function IsCaptionVisible: Boolean;
  begin
    Result := (TcxCustomCheckBoxViewInfo(AViewInfo).Alignment <> taCenter) and
      not IsInplace and (TcxCustomCheckBoxViewInfo(AViewInfo).Text <> '');
  end;

var
  AExtend: TRect;
  AToggleSwitchViewInfo: TdxCustomToggleSwitchViewInfo;
  ASize1: TSize;
begin
  MinContentSize := cxNullSize;
  AToggleSwitchViewInfo := TdxCustomToggleSwitchViewInfo(AViewInfo);
  if AIsInplace then
    ASize1 := ScaleFactor.Apply(cxSize(dxToggleSwitchDefaultWidth, dxToggleSwitchInplaceHeight))
  else
    ASize1 := AToggleSwitchViewInfo.CheckBoxSize;

  if cxRectIsEmpty(AViewInfo.Bounds) or cxRectIsEmpty(AViewInfo.ClientRect) then
  begin
    AExtend := GetClientExtent(ACanvas, AViewInfo);
    Result := cxSize(AExtend.Left + AExtend.Right, AExtend.Top + AExtend.Bottom);
  end
  else
  begin
    Result.cx := cxRectWidth(AViewInfo.Bounds) - cxRectWidth(AViewInfo.ClientRect);
    Result.cy := cxRectHeight(AViewInfo.Bounds) - cxRectHeight(AViewInfo.ClientRect);
  end;

  MinContentSize.cy := ASize1.cy;
  if (AToggleSwitchViewInfo.StateIndicatorKind <> sikNone) and (AToggleSwitchViewInfo.StateIndicatorPosition = sipOutside) then
  begin
    if AToggleSwitchViewInfo.OffIndicatorSize.cx > 0 then
      Inc(Result.cx, AToggleSwitchViewInfo.OffIndicatorSize.cx + GetStateIndicatorGap(AToggleSwitchViewInfo));
    if AToggleSwitchViewInfo.OnIndicatorSize.cx > 0 then
      Inc(Result.cx, AToggleSwitchViewInfo.OnIndicatorSize.cx + GetStateIndicatorGap(AToggleSwitchViewInfo));

    MinContentSize.cy := Max(MinContentSize.cy, Max(AToggleSwitchViewInfo.OnIndicatorSize.cy, AToggleSwitchViewInfo.OffIndicatorSize.cy));
  end;
  Inc(Result.cx, ASize1.cx + 2 * GetContentOffset);
end;

function TdxCustomToggleSwitchViewData.CalculateIndicatorRect(AViewInfo: TdxCustomToggleSwitchViewInfo;
  AIndicatoSize: TSize; AAlign: TLeftRight): TRect;
begin
  if AViewInfo.StateIndicatorKind <> sikNone then
  begin
    case AViewInfo.StateIndicatorPosition of
      sipInside:
        begin
          Result := cxRectCenterVertically(AViewInfo.CheckBoxRect, AIndicatoSize.cy);
          Result := cxRectSetWidth(Result, AIndicatoSize.cx);
          if AAlign = taLeftJustify then
            Result := cxRectSetRight(Result, AViewInfo.CheckBoxRect.Right - GetStateIndicatorGap(AViewInfo))
          else
            Result := cxRectSetLeft(Result, AViewInfo.CheckBoxRect.Left + GetStateIndicatorGap(AViewInfo));
        end;
      sipOutside:
        begin
          Result := cxRectCenterVertically(AViewInfo.ClientRect, AIndicatoSize.cy);
          Result := cxRectSetWidth(Result, AIndicatoSize.cx);
          if AAlign = taLeftJustify then
            Result := cxRectSetRight(Result, AViewInfo.CheckBoxRect.Left - GetStateIndicatorGap(AViewInfo))
          else
            Result := cxRectSetLeft(Result, AViewInfo.CheckBoxRect.Right + GetStateIndicatorGap(AViewInfo));
        end;
    end;
    if cxRectWidth(Result) <= 0 then
      Result := cxNullRect;
  end
  else
    Result := cxNullRect;
end;

function TdxCustomToggleSwitchViewData.CalculateOffIndicatorRect(AViewInfo: TdxCustomToggleSwitchViewInfo): TRect;
begin
  Result := CalculateIndicatorRect(AViewInfo, AViewInfo.OffIndicatorRectSize, taLeftJustify);
end;

function TdxCustomToggleSwitchViewData.CalculateOnIndicatorRect(AViewInfo: TdxCustomToggleSwitchViewInfo): TRect;
begin
  Result := CalculateIndicatorRect(AViewInfo, AViewInfo.OnIndicatorRectSize, taRightJustify);
end;

procedure TdxCustomToggleSwitchViewData.CalculateStateTextFont(AViewInfo: TdxCustomToggleSwitchViewInfo);
begin
  AViewInfo.NormalStateTextFont.Assign(AViewInfo.Font);
  AViewInfo.ActiveStateTextFont.Assign(AViewInfo.Font);
end;

procedure TdxCustomToggleSwitchViewData.CalculateStateIndicatorRectSizes(AViewInfo: TdxCustomToggleSwitchViewInfo);
var
  ATextGap: Integer;
  AFactorX: Single;
  AStateIndicatorWidth: Integer;
  AStateIndicatorMaxSizeY: Integer;
  AStateIndicatorPlaceWidth: Integer;
begin
  if (AViewInfo.OffIndicatorSize.cx = 0) and (AViewInfo.OnIndicatorSize.cx = 0) then
  begin
    AViewInfo.OffIndicatorRectSize := cxNullSize;
    AViewInfo.OnIndicatorRectSize := cxNullSize;
  end
  else
  begin
    AStateIndicatorMaxSizeY := 0;
    AFactorX := 0;
    if AViewInfo.IsTextEnabled and (AViewInfo.Text <> '') then
      ATextGap := GetTextGap
    else
      ATextGap := 0;

    AStateIndicatorWidth := AViewInfo.OffIndicatorSize.cx + AViewInfo.OnIndicatorSize.cx;
    case AViewInfo.StateIndicatorPosition of
      sipInside:
        begin
          AFactorX := Min(1, (AViewInfo.CheckBoxSize.cx - 3 * GetStateIndicatorGap(AViewInfo)) / AStateIndicatorWidth);
          AStateIndicatorMaxSizeY := AViewInfo.CheckBoxSize.cy;
        end;
      sipOutside:
        begin
          AStateIndicatorPlaceWidth := (cxRectWidth(AViewInfo.ClientRect) - AViewInfo.CheckBoxSize.cx - ATextGap);
          if AViewInfo.OffIndicatorSize.cx > 0 then
            AStateIndicatorPlaceWidth := AStateIndicatorPlaceWidth - GetStateIndicatorGap(AViewInfo);
          if AViewInfo.OnIndicatorSize.cx > 0 then
            AStateIndicatorPlaceWidth := AStateIndicatorPlaceWidth - GetStateIndicatorGap(AViewInfo);

          AFactorX := Min(1, AStateIndicatorPlaceWidth / AStateIndicatorWidth);
          AStateIndicatorMaxSizeY := cxRectHeight(AViewInfo.ClientRect);
        end;
    end;
    AViewInfo.OffIndicatorRectSize.cx := Trunc(AViewInfo.OffIndicatorSize.cx * AFactorX);
    AViewInfo.OnIndicatorRectSize.cx := Trunc(AViewInfo.OnIndicatorSize.cx * AFactorX);
    AViewInfo.OffIndicatorRectSize.cy := Min(AViewInfo.OffIndicatorSize.cy, AStateIndicatorMaxSizeY);
    AViewInfo.OnIndicatorRectSize.cy := Min(AViewInfo.OnIndicatorSize.cy, AStateIndicatorMaxSizeY);
  end;
end;

procedure TdxCustomToggleSwitchViewData.CalculateStateIndicatorSizes(AViewInfo: TdxCustomToggleSwitchViewInfo);
begin
  case AViewInfo.StateIndicatorKind of
    sikText:
      begin
        AViewInfo.OffIndicatorSize.cx := Max(cxTextWidth(AViewInfo.NormalStateTextFont, AViewInfo.OffText),
          cxTextWidth(AViewInfo.ActiveStateTextFont, AViewInfo.OffText));
        AViewInfo.OnIndicatorSize.cx := Max(cxTextWidth(AViewInfo.NormalStateTextFont, AViewInfo.OnText),
          cxTextWidth(AViewInfo.ActiveStateTextFont, AViewInfo.OnText));
        AViewInfo.OffIndicatorSize.cy := Max(cxTextHeight(AViewInfo.NormalStateTextFont),
          cxTextHeight(AViewInfo.ActiveStateTextFont));
        AViewInfo.OnIndicatorSize.cy := Max(cxTextHeight(AViewInfo.NormalStateTextFont),
          cxTextHeight(AViewInfo.ActiveStateTextFont));
      end;
    sikGlyph:
      begin
        AViewInfo.OffIndicatorSize := dxGetImageSize(AViewInfo.FOffGlyph, ScaleFactor);
        AViewInfo.OnIndicatorSize := dxGetImageSize(AViewInfo.FOnGlyph, ScaleFactor);
      end;
    sikNone:
      begin
        AViewInfo.OffIndicatorSize := cxNullSize;
        AViewInfo.OnIndicatorSize := cxNullSize;
      end;
  end;
end;

function TdxCustomToggleSwitchViewData.CalculateThumbBounds(AViewInfo: TdxCustomToggleSwitchViewInfo): TRect;
var
  ASize: TSize;
  ABounds: TRect;
begin
  ASize.cy := AViewInfo.CheckBoxSize.cy;
  ASize.cx := Round(AViewInfo.CheckBoxSize.cx * AViewInfo.Painter.GetToggleSwitchThumbPercentsWidth / 100);
  ABounds := AViewInfo.ToggleSwitchBounds;
  Result := cxRectCenter(ABounds, ASize);
  case AViewInfo.State of
    cbsUnchecked:
      Result := cxRectSetLeft(Result, ABounds.Left + AViewInfo.ThumbDelta);
    cbsChecked:
      Result := cxRectSetLeft(Result, ABounds.Right - ASize.cx - AViewInfo.ThumbDelta);
    else
      Result := cxRectSetLeft(Result, ABounds.Left + (cxRectWidth(ABounds) - ASize.cx) div 2);
  end;
end;

function TdxCustomToggleSwitchViewData.CalculateToggleSwitchSize(AViewInfo: TdxCustomToggleSwitchViewInfo): TSize;
var
  AWidth, AHeight: Integer;
  AProportion: Single;
begin
  AWidth := GetDefaultWidth;
  AHeight := GetDefaultHeight;
  if IsInplace then
  begin
    AWidth := Min(cxRectWidth(AViewInfo.ClientRect), AWidth);
    AHeight := Min(cxRectHeight(AViewInfo.ClientRect), AHeight);
    AProportion := Min(AWidth / GetDefaultWidth, AHeight / GetDefaultHeight);
    AWidth := Round(GetDefaultWidth * AProportion);
    AHeight := Round(GetDefaultHeight * AProportion);
  end;
  Result := cxSize(AWidth, AHeight);
end;

function TdxCustomToggleSwitchViewData.GetStateIndicatorGap(AViewInfo: TdxCustomToggleSwitchViewInfo): Integer;
begin
  case AViewInfo.StateIndicatorPosition of
    sipOutside:
      Result := GetTextGap;
    sipInside:
      Result := ScaleFactor.Apply(dxDefaultToggleSwitchInsideStateIndicatorOffset);
  else
    Result := 0;
  end;
end;

function TdxCustomToggleSwitchViewData.GetDefaultHeight: Integer;
begin
  Result := ScaleFactor.Apply(dxToggleSwitchDefaultHeight);
end;

function TdxCustomToggleSwitchViewData.GetDefaultWidth: Integer;
begin
  Result := ScaleFactor.Apply(dxToggleSwitchDefaultWidth);
end;

function TdxCustomToggleSwitchViewData.GetProperties: TdxCustomToggleSwitchProperties;
begin
  Result := TdxCustomToggleSwitchProperties(FProperties);
end;

{ TdxCustomToggleSwitchViewInfo }

constructor TdxCustomToggleSwitchViewInfo.Create;
begin
  inherited;
  FNormalStateTextFont := TFont.Create;
  FActiveStateTextFont := TFont.Create;
end;

destructor TdxCustomToggleSwitchViewInfo.Destroy;
begin
  FreeAndNil(FActiveStateTextFont);
  FreeAndNil(FNormalStateTextFont);
  inherited;
end;

procedure TdxCustomToggleSwitchViewInfo.Offset(DX, DY: Integer);
begin
  inherited;
  OffsetRect(FToggleSwitchBounds, DX, DY);
  OffsetRect(FThumbBounds, DX, DY);
  OffsetRect(FOffIndicatorRect, DX, DY);
  OffsetRect(FOnIndicatorRect, DX, DY);
end;

function TdxCustomToggleSwitchViewInfo.GetLeftIndicatorWidth: Integer;
begin
  if UseRightToLeftAlignment then
    Result := OnIndicatorRectSize.cx
  else
    Result := OffIndicatorRectSize.cx;
end;

function TdxCustomToggleSwitchViewInfo.GetRightIndicatorWidth: Integer;
begin
  if UseRightToLeftAlignment then
    Result := OffIndicatorRectSize.cx
  else
    Result := OnIndicatorRectSize.cx;
end;

procedure TdxCustomToggleSwitchViewInfo.InternalPaint(ACanvas: TcxCanvas);
begin
  DrawEditBackground(ACanvas, Bounds, cxNullRect, False);
  DrawCustomEditValidationMark(ACanvas);
  if Alignment <> taCenter then
    DrawCheckBoxText(ACanvas, Text, Font, TextColor, TextRect, DrawTextFlags, IsTextEnabled);
  Painter.DrawScaledToggleSwitch(ACanvas, ToggleSwitchBounds, ThumbState, ThumbBounds, ScaleFactor);
  case StateIndicatorKind of
    sikText:
      DrawStateText(ACanvas);
    sikGlyph:
      DrawStateGlyph(ACanvas);
  end;
  if not FadingHelper.DrawImage(ACanvas.Handle, ThumbBounds) then
    DrawThumb(ACanvas, ThumbBounds, ThumbState);
  if not IsRectEmpty(FocusRect) then
    ACanvas.DrawFocusRect(FocusRect);
end;

function TdxCustomToggleSwitchViewInfo.NeedShowOffIndicator: Boolean;
begin
  Result := (ThumbDelta <> 0) or (State <> cbsChecked) or (StateIndicatorPosition <> sipInside);
end;

function TdxCustomToggleSwitchViewInfo.NeedShowOnIndicator: Boolean;
begin
  Result := (ThumbDelta <> 0) or (State <> cbsUnchecked) or (StateIndicatorPosition <> sipInside);
end;

function TdxCustomToggleSwitchViewInfo.GetCheckControlFadingHelperClass: TcxCheckControlFadingHelperClass;
begin
  Result := TdxToggleSwitchThumbFadingHelper;
end;

procedure TdxCustomToggleSwitchViewInfo.DrawStateGlyph(ACanvas: TcxCanvas);

  function GetDrawingRect(const ARect: TRect; const ASize: TSize): TRect;
  begin
    Result := ARect;
    Result := cxRectSetLeft(Result, Result.Left + (cxRectWidth(Result) - ASize.cx) div 2);
    Result := cxRectSetTop(Result, Result.Top + (cxRectHeight(Result) - ASize.cy) div 2);
    Result := cxRectSetSize(Result, ASize);
  end;

  procedure InternalDrawGlyph(AGlyph: TdxSmartGlyph; const AIndicatorRect: TRect; const AIndicatorSize: TSize);
  begin
    if not cxRectIsNull(AIndicatorRect) then
    begin
      ACanvas.SaveClipRegion;
      try
        ACanvas.IntersectClipRect(AIndicatorRect);
        AGlyph.StretchDraw(ACanvas.Handle, GetDrawingRect(AIndicatorRect, AIndicatorSize), MaxByte, Painter.GetToggleSwitchColorPalette);
      finally
        ACanvas.RestoreClipRegion;
      end;
    end;
  end;

begin
  if NeedShowOffIndicator then
    InternalDrawGlyph(FOffGlyph, OffIndicatorRect, OffIndicatorSize);
  if NeedShowOnIndicator then
    InternalDrawGlyph(FOnGlyph, OnIndicatorRect, OnIndicatorSize);
end;

procedure TdxCustomToggleSwitchViewInfo.DrawStateText(ACanvas: TcxCanvas);
begin
  if not cxRectIsNull(OffIndicatorRect) and NeedShowOffIndicator then
    if State = cbsUnchecked then
      Painter.DrawToggleSwitchStateIndicator(ACanvas, OffIndicatorRect, OffText, ActiveStateTextFont)
    else
      Painter.DrawToggleSwitchStateIndicator(ACanvas, OffIndicatorRect, OffText, NormalStateTextFont);

  if not cxRectIsNull(OnIndicatorRect) and NeedShowOnIndicator then
    if State = cbsChecked then
      Painter.DrawToggleSwitchStateIndicator(ACanvas, OnIndicatorRect, OnText, ActiveStateTextFont)
    else
      Painter.DrawToggleSwitchStateIndicator(ACanvas, OnIndicatorRect, OnText, NormalStateTextFont);
end;

procedure TdxCustomToggleSwitchViewInfo.DrawThumb(ACanvas: TcxCanvas; AClientRect: TRect; AState: TcxButtonState);
begin
  Painter.DrawScaledToggleSwitchThumb(ACanvas, AClientRect, AState, ScaleFactor);
end;

function TdxCustomToggleSwitchViewInfo.GetThumbDelta: Integer;
begin
  Result := Min(Max(0, FThumbDelta), ThumbMaxDelta);
end;

function TdxCustomToggleSwitchViewInfo.GetThumbMaxDelta: Integer;
begin
  Result := cxRectWidth(ToggleSwitchBounds) - cxRectWidth(ThumbBounds);
end;

function TdxCustomToggleSwitchViewInfo.GetThumbState: TcxButtonState;
begin
  case CheckBoxState of
    ecsHot: Result := cxbsHot;
    ecsPressed: Result := cxbsPressed;
    ecsDisabled: Result := cxbsDisabled;
  else
    Result := cxbsNormal;
  end;
end;

function TdxCustomToggleSwitchViewInfo.GetThumbStepAnimation: Integer;
begin
  Result := ThumbMaxDelta div 8;
end;

{ TdxToggleSwitch }

class function TdxToggleSwitch.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxToggleSwitchProperties;
end;

function TdxToggleSwitch.GetActiveProperties: TdxToggleSwitchProperties;
begin
  Result := TdxToggleSwitchProperties(InternalGetActiveProperties);
end;

function TdxToggleSwitch.GetProperties: TdxToggleSwitchProperties;
begin
  Result := TdxToggleSwitchProperties(inherited Properties);
end;

procedure TdxToggleSwitch.SetProperties(Value: TdxToggleSwitchProperties);
begin
  Properties.Assign(Value);
end;

initialization
  GetRegisteredEditProperties.Register(TdxToggleSwitchProperties, scxSEditRepositoryToggleSwitchItem);
  FilterEditsController.Register(TdxToggleSwitchProperties, TcxFilterCheckBoxHelper);

finalization
  FilterEditsController.Unregister(TdxToggleSwitchProperties, TcxFilterCheckBoxHelper);

end.
