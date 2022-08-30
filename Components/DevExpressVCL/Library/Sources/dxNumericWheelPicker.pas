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

unit dxNumericWheelPicker;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Graphics, Classes, cxGraphics, dxGDIPlusClasses, cxLookAndFeelPainters, cxEdit, cxContainer, cxDataStorage, cxSpinEdit,
  cxFilterControlUtils, dxWheelPicker;

type
  TdxCustomNumericWheelPickerProperties = class;

  { TdxCustomNumericWheelPickerItemViewInfo }

  TdxCustomNumericWheelPickerItemViewInfo = class(TdxCustomWheelPickerItemViewInfo)
  strict private
    FFont: TFont;
    FTextRect: TRect;
    FTextSize: TSize;

    function GetTextSize: TSize;
    function HasText: Boolean;
  private
    FText: string;
  protected
    procedure Calculate(const ABounds: TRect); override;
    procedure DrawContent(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState); override;
    procedure Offset(DX: Integer; DY: Integer); override;

    procedure CalculateTextParameters; virtual;
    procedure CalculateTextRects; virtual;
    procedure DrawText(AGPCanvas: TdxGPCanvas); virtual;
    function GetTextColor: TColor; virtual;
    procedure InternalDrawContent(AGPCanvas: TdxGPCanvas); virtual;

    property TextRect: TRect read FTextRect;
  public
    constructor Create(AWheelPickerViewInfo: TdxCustomWheelPickerViewInfo); override;
    destructor Destroy; override;
  end;

  { TdxCustomNumericWheelPickerWheelViewInfo }

  TdxCustomNumericWheelPickerWheelViewInfo = class(TdxCustomWheelPickerWheelViewInfo)
  strict private
    FCanContinueTyping: Boolean;
    FTypingString: string;

    function GetValue: Integer;
    procedure SetValue(AValue: Integer);

    function GetMinValue: Integer;
    function GetMaxValue: Integer;
    procedure CalculateCyclic(const ABounds: TRect);
    procedure ResetTypingString;
  protected
    FFirstItemValue: Integer;

    class function GetWheelPickerItemViewInfoClass: TdxCustomWheelPickerItemViewInfoClass; override;
    procedure Calculate(const ABounds: TRect); override;
    procedure ChangeSelectedState; override;
    procedure InitializeItem(AItem: TdxCustomWheelPickerItemViewInfo; AIndex: Integer); override;

    procedure KeyDown(AKey: Word); override;
    procedure MouseWheel(ADelta: Integer); override;

    function GetItemText(AItemIndex: Integer): string; virtual;
    function CanTyping(AValue: Integer): Boolean; virtual;
    function NeedProccessKey(AKey: Word): Boolean; virtual;

    property CanContinueTyping: Boolean read FCanContinueTyping;
    property MaxValue: Integer read GetMaxValue;
    property MinValue: Integer read GetMinValue;
    property TypingString: string read FTypingString;
    property Value: Integer read GetValue write SetValue;
  end;

  { TdxCustomNumericWheelPickerViewInfo}

  TdxCustomNumericWheelPickerViewInfo = class(TdxCustomWheelPickerViewInfo)
  protected
    FMaxValue: Double;
    FMinValue: Double;
    FItemIndex: Integer;

    FFontColor: TColor;
    FFontSize: Integer;
    FIsSmallItemSize: Boolean;

    class function GetWheelPickerWheelViewInfoClass: TdxCustomWheelPickerWheelViewInfoClass; override;

    function GetNullValueItemIndex: Integer;
    function GetInternalValue: TcxEditValue; override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure SetInternalValue(AValue: Variant); override;
    procedure SynchronizeWheelIndexes; override;
  public
    procedure Assign(Source: TObject); override;
  end;

  { TdxCustomNumericWheelPickerViewData }

  TdxCustomNumericWheelPickerViewData = class(TdxCustomWheelPickerViewData)
  strict private
    function GetFontSize(AViewInfo: TdxCustomNumericWheelPickerViewInfo): Integer;
    function GetRealMaxValue: Integer;
    function GetRealMinValue: Integer;
  protected
    FMaxTextSize: TSize;
    FWheelWidth: Integer;

    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
      AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize; override;
    function GetOptimalRowHeight: Integer; override;
    function GetOptimalWidth: Integer; override;
    function GetWheelCount: Integer; override;
    function GetWheelWidth(AIndex: Integer): Integer; override;
    procedure CalculateTextSize(AViewInfo: TdxCustomWheelPickerViewInfo); override;

    procedure CalculateWheels(AViewInfo: TdxCustomWheelPickerViewInfo); override;
    procedure InitializeWheel(AWheelViewInfo: TdxCustomWheelPickerWheelViewInfo; AIndex: Integer); override;

    function GetMaxText: string; virtual;
    procedure CalculateWheelWidth(AViewInfo: TdxCustomWheelPickerViewInfo); virtual;

    property MaxTextSize: TSize read FMaxTextSize;
  end;

  { TdxCustomNumericWheelPickerProperties}

  TdxCustomNumericWheelPickerProperties = class(TdxCustomWheelPickerProperties)
  protected
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function CanValidate: Boolean; override;
  public
    class function GetContainerClass: TcxContainerClass; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;

    function GetSupportedOperations: TcxEditSupportedOperations; override;
    function GetDisplayText(const AEditValue: Variant; AFullText, AIsInplace: Boolean): string; override;
    procedure PrepareDisplayValue(const AEditValue: Variant; var DisplayValue: Variant; AEditFocused: Boolean); override;
  end;

  { TdxCustomNumericWheelPicker }

  TdxCustomNumericWheelPicker = class(TdxCustomWheelPicker)
  protected
    function GetEditStateColorKind: TcxEditStateColorKind; override;
    function GetDisplayText: string; override;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure TextChanged; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxNumericWheelPickerProperties }

  TdxNumericWheelPickerProperties = class(TdxCustomNumericWheelPickerProperties)
  strict private
    function GetMax: Integer;
    function GetMin: Integer;
    procedure SetMax(AValue: Integer);
    procedure SetMin(AValue: Integer);
  public
    constructor Create(AOwner: TPersistent); override;

    class function GetContainerClass: TcxContainerClass; override;
  published
    property Cyclic default False;
    property ImmediatePost;
    property LineCount;
    property Max: Integer read GetMax write SetMax default 10;
    property Min: Integer read GetMin write SetMin default 0;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnChange;
    property OnEditValueChanged;
    property OnValidate;
  end;

  { TdxNumericWheelPicker }

  TdxNumericWheelPicker = class(TdxCustomNumericWheelPicker)
  strict private
    function GetActiveProperties: TdxNumericWheelPickerProperties;
    function GetProperties: TdxNumericWheelPickerProperties;
    procedure SetProperties(AValue: TdxNumericWheelPickerProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;

    property ActiveProperties: TdxNumericWheelPickerProperties read GetActiveProperties;
  published
    property Properties: TdxNumericWheelPickerProperties read GetProperties write SetProperties;

    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property EditValue;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop default True;
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

function dxGPGetTextSize(AGpCanvas: TdxGPGraphics; const AText: string; AFont: TFont): TSize; overload;
function dxGPGetTextSize(const AText: string; AFont: TFont): TSize; overload;

implementation

uses
  SysUtils, Math, Variants, Windows, dxCore, cxVariants, cxGeometry, dxCoreGraphics, cxEditConsts,
  cxControls;

const
  dxWheelPickerFontSizeFactor: Double = 1.5;

type
  TcxCustomEditPropertiesAccess = class(TcxCustomEditProperties);
  TdxCustomWheelPickerPropertiesAccess = class(TdxCustomWheelPickerProperties);
  TdxCustomWheelPickerViewInfoAccess = class(TdxCustomWheelPickerViewInfo);
  TFontAccess = class(TFont);

function dxNumericWheelPickerGetNullValue(AMinValue, AMaxValue: Double): Variant;
begin
  if InRange(0, AMinValue, AMaxValue) then
    Result := 0
  else
    Result := AMinValue;
end;

function dxGPGetTextSize(AGpCanvas: TdxGPGraphics; const AText: string; AFont: TFont): TSize; overload;
var
  ARect: TRect;
begin
  dxGPGetTextRect(AGpCanvas, AText, AFont, False, Rect(0, 0, cxMaxRectSize, cxMaxRectSize), ARect);
  Result := cxRectSize(ARect);
end;

function dxGPGetTextSize(const AText: string; AFont: TFont): TSize;
begin
  dxGPPaintCanvas.BeginPaint(cxScreenCanvas.Handle, cxNullRect);
  try
    Result := dxGPGetTextSize(dxGPPaintCanvas, AText, AFont);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

{ TdxCustomNumericWheelPickerItemViewInfo }

constructor TdxCustomNumericWheelPickerItemViewInfo.Create(AWheelPickerViewInfo: TdxCustomWheelPickerViewInfo);
begin
  inherited;
  FFont := TFont.Create;
end;

destructor TdxCustomNumericWheelPickerItemViewInfo.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

procedure TdxCustomNumericWheelPickerItemViewInfo.CalculateTextParameters;
var
  AViewInfo: TdxCustomNumericWheelPickerViewInfo;
begin
  AViewInfo := WheelPickerViewInfo as TdxCustomNumericWheelPickerViewInfo;
  FFont.Assign(AViewInfo.Font);
  FFont.Size := AViewInfo.FFontSize;
  if AViewInfo.FIsNullEditValue and IsItemSelected then
    FFont.Style := [fsItalic];

  if not FIsRecalculate then
    FTextSize := GetTextSize;
end;

procedure TdxCustomNumericWheelPickerItemViewInfo.DrawText(AGPCanvas: TdxGPCanvas);

  function GetTextColorAlpha: Byte;
  begin
    if (IsItemSelected or (WheelViewInfo as TdxCustomNumericWheelPickerWheelViewInfo).IsSelected) and
      not TdxCustomNumericWheelPickerViewInfo(WheelPickerViewInfo).FIsNullEditValue then
      Result := 255
    else
      Result := 130;
  end;

begin
  if HasText then
    dxGPDrawText(AGPCanvas, FText, FTextRect, FFont, dxColorToAlphaColor(GetTextColor, GetTextColorAlpha));
end;

function TdxCustomNumericWheelPickerItemViewInfo.GetTextColor: TColor;
begin
  if WheelPickerViewInfo.IsInplace and (IsItemSelected or (WheelViewInfo as TdxCustomNumericWheelPickerWheelViewInfo).IsSelected) then
    Result := clBtnText
  else
    Result := WheelPickerViewInfo.TextColor;
end;

procedure TdxCustomNumericWheelPickerItemViewInfo.InternalDrawContent(AGPCanvas: TdxGPCanvas);
begin
  DrawText(AGPCanvas);
end;

procedure TdxCustomNumericWheelPickerItemViewInfo.Calculate(const ABounds: TRect);
begin
  inherited Calculate(ABounds);
  CalculateTextParameters;
  CalculateTextRects;
end;

procedure TdxCustomNumericWheelPickerItemViewInfo.CalculateTextRects;
begin
  FTextRect := cxRectCenter(ContentBounds, FTextSize);
end;

procedure TdxCustomNumericWheelPickerItemViewInfo.DrawContent(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState);
begin
  inherited DrawContent(ACanvas, ABounds, AState);
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, ContentBounds);
  try
    InternalDrawContent(dxGPPaintCanvas);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

function TdxCustomNumericWheelPickerItemViewInfo.GetTextSize: TSize;
begin
  if HasText then
    Result := dxGPGetTextSize(FText, FFont)
  else
    Result := cxNullSize;
end;

function TdxCustomNumericWheelPickerItemViewInfo.HasText: Boolean;
begin
  Result := FText <> '';
end;

procedure TdxCustomNumericWheelPickerItemViewInfo.Offset(DX, DY: Integer);
begin
  inherited Offset(DX, DY);
  FTextRect := cxRectOffset(FTextRect, DX, DY);
end;

{ TdxCustomNumericWheelPickerWheelViewInfo }

class function TdxCustomNumericWheelPickerWheelViewInfo.GetWheelPickerItemViewInfoClass: TdxCustomWheelPickerItemViewInfoClass;
begin
  Result := TdxCustomNumericWheelPickerItemViewInfo;
end;

procedure TdxCustomNumericWheelPickerWheelViewInfo.Calculate(const ABounds: TRect);
begin
  CalculateCyclic(ABounds);
  inherited;
end;

procedure TdxCustomNumericWheelPickerWheelViewInfo.ChangeSelectedState;
begin
  ResetTypingString;
  inherited ChangeSelectedState;
end;

procedure TdxCustomNumericWheelPickerWheelViewInfo.InitializeItem(AItem: TdxCustomWheelPickerItemViewInfo; AIndex: Integer);
begin
  inherited InitializeItem(AItem, AIndex);
  (AItem as TdxCustomNumericWheelPickerItemViewInfo).FText := GetItemText(AIndex);
end;

procedure TdxCustomNumericWheelPickerWheelViewInfo.KeyDown(AKey: Word);
var
  ACurrentValue: Integer;
begin
  inherited KeyDown(AKey);
  if NeedProccessKey(AKey) then
  begin
    FTypingString := FTypingString + GetCharFromKeyCode(AKey);
    if FTypingString <> '-' then
    begin
      ACurrentValue := Max(StrToInt(TypingString), GetMinValue);
      if InRange(ACurrentValue, GetMinValue, GetMaxValue) then
        Value := ACurrentValue;
      if not CanTyping(ACurrentValue) then
      begin
        ResetTypingString;
        FCanContinueTyping := False;
      end;
    end;
  end
  else
    ResetTypingString;
end;

procedure TdxCustomNumericWheelPickerWheelViewInfo.MouseWheel(ADelta: Integer);
begin
  ResetTypingString;
  inherited MouseWheel(ADelta);
end;

function TdxCustomNumericWheelPickerWheelViewInfo.GetItemText(AItemIndex: Integer): string;
begin
  Result := IntToStr(FFirstItemValue + AItemIndex);
end;

function TdxCustomNumericWheelPickerWheelViewInfo.CanTyping(AValue: Integer): Boolean;
var
  AMaxValue: Double;
begin
  AMaxValue := GetMaxValue;
  Result := (Length(TypingString) <> Max(Length(FloatToStr(AMaxValue)), Length(FloatToStr(GetMinValue)))) and
    (Abs(AValue) <= Abs(AMaxValue));
end;

function TdxCustomNumericWheelPickerWheelViewInfo.NeedProccessKey(AKey: Word): Boolean;
begin
  Result := IsNumericChar(GetCharFromKeyCode(AKey), ntInteger);
end;

function TdxCustomNumericWheelPickerWheelViewInfo.GetMinValue: Integer;
begin
  Result := FFirstItemValue;
end;

function TdxCustomNumericWheelPickerWheelViewInfo.GetMaxValue: Integer;
begin
  Result := FFirstItemValue + FWheelItemCount - 1;
end;

function TdxCustomNumericWheelPickerWheelViewInfo.GetValue: Integer;
begin
  Result := ItemIndex + FFirstItemValue;
end;

procedure TdxCustomNumericWheelPickerWheelViewInfo.SetValue(AValue: Integer);
begin
  ItemIndex := AValue - FFirstItemValue;
end;

procedure TdxCustomNumericWheelPickerWheelViewInfo.CalculateCyclic(const ABounds: TRect);
begin
  FCyclic := FCyclic and (FWheelItemCount * RowHeight > cxRectHeight(ABounds));
end;

procedure TdxCustomNumericWheelPickerWheelViewInfo.ResetTypingString;
begin
  FTypingString := '';
  FCanContinueTyping := True;
end;

{ TdxCustomNumericWheelPickerViewInfo }

procedure TdxCustomNumericWheelPickerViewInfo.Assign(Source: TObject);
var
  AViewInfo: TdxCustomNumericWheelPickerViewInfo;
begin
  if Source is TdxCustomNumericWheelPickerViewInfo then
  begin
    AViewInfo := TdxCustomNumericWheelPickerViewInfo(Source);
    FItemIndex := AViewInfo.FItemIndex;
    FMaxValue := AViewInfo.FMaxValue;
    FMinValue := AViewInfo.FMinValue;
    FFontColor := AViewInfo.FFontColor;
    FFontSize := AViewInfo.FFontSize;
  end;
  inherited Assign(Source);
end;

class function TdxCustomNumericWheelPickerViewInfo.GetWheelPickerWheelViewInfoClass: TdxCustomWheelPickerWheelViewInfoClass;
begin
  Result := TdxCustomNumericWheelPickerWheelViewInfo;
end;

function TdxCustomNumericWheelPickerViewInfo.GetNullValueItemIndex: Integer;
begin
  Result := 0;
  if (FWheels.Count > 0) then
    Result := dxNumericWheelPickerGetNullValue(FMinValue, FMaxValue);
end;

function TdxCustomNumericWheelPickerViewInfo.GetInternalValue: TcxEditValue;
begin
  Result := '';
  if FWheels.Count > 0 then
    Result := IntToStr((FWheels[0] as TdxCustomNumericWheelPickerWheelViewInfo).Value);
end;

procedure TdxCustomNumericWheelPickerViewInfo.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  inherited KeyDown(AKey, AShift);
  if (SelectedWheel <> nil) and  not TdxCustomNumericWheelPickerWheelViewInfo(SelectedWheel).CanContinueTyping then
    SelectNextWheel(1);
end;

procedure TdxCustomNumericWheelPickerViewInfo.SetInternalValue(AValue: Variant);
begin
  inherited;
  if FIsNullEditValue then
    FItemIndex := GetNullValueItemIndex
  else
    FItemIndex := AValue;
end;

procedure TdxCustomNumericWheelPickerViewInfo.SynchronizeWheelIndexes;
var
  AValue: Integer;
begin
  if not FIsNullEditValue then
    AValue := FItemIndex
  else
    AValue := dxNumericWheelPickerGetNullValue(FMinValue, FMaxValue);
  if FWheels.Count > 0 then
    (FWheels[0] as TdxCustomNumericWheelPickerWheelViewInfo).Value := AValue;
end;

{ TdxCustomNumericWheelPickerViewData }

function TdxCustomNumericWheelPickerViewData.GetMaxText: string;
var
  AMaxValue, AMinValue: string;
  AProperties: TdxCustomNumericWheelPickerProperties;
begin
  AProperties := Properties as TdxCustomNumericWheelPickerProperties;
  AMaxValue := FloatToStr(AProperties.MaxValue);
  AMinValue := FloatToStr(AProperties.MinValue);
  if Length(AMaxValue) > Length(AMinValue) then
    Result := AMaxValue
  else
    Result := AMinValue;
end;

procedure TdxCustomNumericWheelPickerViewData.CalculateWheelWidth(AViewInfo: TdxCustomWheelPickerViewInfo);
begin
  FWheelWidth := FMaxTextSize.cx + 2 * ScaleFactor.Apply(dxWheelPickerItemContentMargin)
end;

procedure TdxCustomNumericWheelPickerViewData.CalculateTextSize(AViewInfo: TdxCustomWheelPickerViewInfo);
var
  AFont: TFont;
begin
  inherited;
  (AViewInfo as TdxCustomNumericWheelPickerViewInfo).FFontSize := GetFontSize(AViewInfo as TdxCustomNumericWheelPickerViewInfo);
  AFont := TFont.Create;
  try
    AFont.Assign(Style.Font);
    AFont.Size := (AViewInfo as TdxCustomNumericWheelPickerViewInfo).FFontSize;

    FMaxTextSize := dxGPGetTextSize(GetMaxText, Style.Font);

    FMaxTextSize.cx := dxGPGetTextSize(GetMaxText, AFont).cx;
  finally
    AFont.Free;
  end;
end;

procedure TdxCustomNumericWheelPickerViewData.CalculateWheels(AViewInfo: TdxCustomWheelPickerViewInfo);
var
  AWheelPickerViewInfo: TdxCustomNumericWheelPickerViewInfo;
begin
  AWheelPickerViewInfo := AViewInfo as TdxCustomNumericWheelPickerViewInfo;
  AWheelPickerViewInfo.FIsSmallItemSize := AWheelPickerViewInfo.FRowHeight < GetOptimalRowHeight;
  AWheelPickerViewInfo.FMaxValue := GetRealMaxValue;
  AWheelPickerViewInfo.FMinValue := GetRealMinValue;
  CalculateWheelWidth(AViewInfo);
  AWheelPickerViewInfo.FFontSize := GetFontSize(AWheelPickerViewInfo);
  inherited;
end;

procedure TdxCustomNumericWheelPickerViewData.InitializeWheel(AWheelViewInfo: TdxCustomWheelPickerWheelViewInfo; AIndex: Integer);
var
  AViewInfo: TdxCustomNumericWheelPickerWheelViewInfo;
begin
  inherited;
  AViewInfo := AWheelViewInfo as TdxCustomNumericWheelPickerWheelViewInfo;
  AViewInfo.FCyclic := TdxCustomWheelPickerPropertiesAccess(Properties).Cyclic;
  AViewInfo.FFirstItemValue := Round(GetRealMinValue);
  AViewInfo.FWheelItemCount := Max(Round(GetRealMaxValue) - AViewInfo.FFirstItemValue, 0) + 1;
end;

function TdxCustomNumericWheelPickerViewData.InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
  AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: tagSIZE; AViewInfo: TcxCustomEditViewInfo): tagSIZE;
begin
  CalculateTextSize(AViewInfo as TdxCustomWheelPickerViewInfo);
  CalculateWheelWidth(AViewInfo as TdxCustomNumericWheelPickerViewInfo);
  Result := inherited InternalGetEditConstantPartSize(ACanvas, AIsInplace, AEditSizeProperties, MinContentSize, AViewInfo);
end;

function TdxCustomNumericWheelPickerViewData.GetOptimalRowHeight: Integer;
begin
  Result := Round((2 + dxWheelPickerFontSizeFactor) * FMaxTextSize.cy);
end;

function TdxCustomNumericWheelPickerViewData.GetOptimalWidth: Integer;
var
  AWheelCount: Integer;
begin
  AWheelCount := GetWheelCount;
  Result := FWheelWidth * AWheelCount + GetWheelIndent * (AWheelCount + 1) + 2 * ScaleFactor.Apply(dxWheelPickerItemContentMargin);
end;

function TdxCustomNumericWheelPickerViewData.GetWheelCount: Integer;
begin
  Result := 1;
end;

function TdxCustomNumericWheelPickerViewData.GetWheelWidth(AIndex: Integer): Integer;
begin
  Result := FWheelWidth;
end;

function TdxCustomNumericWheelPickerViewData.GetFontSize(AViewInfo: TdxCustomNumericWheelPickerViewInfo): Integer;
begin
  if AViewInfo.FIsSmallItemSize then
    Result := Style.Font.Size
  else
    Result := Trunc(Style.Font.Size * dxWheelPickerFontSizeFactor);
end;

function TdxCustomNumericWheelPickerViewData.GetRealMaxValue: Integer;
begin
  if TcxCustomEditPropertiesAccess(Properties).MinValue > TcxCustomEditPropertiesAccess(Properties).MaxValue then
    Result := Round(TcxCustomEditPropertiesAccess(Properties).MinValue)
  else
    Result := Round(TcxCustomEditPropertiesAccess(Properties).MaxValue);
end;

function TdxCustomNumericWheelPickerViewData.GetRealMinValue: Integer;
begin
  Result := Round(TcxCustomEditPropertiesAccess(Properties).MinValue);
end;

{ TdxCustomNumericWheelPickerProperties }

class function TdxCustomNumericWheelPickerProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxCustomNumericWheelPickerViewData;
end;

function TdxCustomNumericWheelPickerProperties.CanValidate: Boolean;
begin
  Result := True;
end;

class function TdxCustomNumericWheelPickerProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxCustomNumericWheelPicker;
end;

class function TdxCustomNumericWheelPickerProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxCustomNumericWheelPickerViewInfo;
end;

function TdxCustomNumericWheelPickerProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := [esoEditing, esoFiltering, esoSorting];
end;

function TdxCustomNumericWheelPickerProperties.GetDisplayText(const AEditValue: Variant; AFullText, AIsInplace: Boolean): string;
var
  ADisplayValue: TcxEditValue;
begin
  PrepareDisplayValue(AEditValue, ADisplayValue, False);
  if VarIsNull(ADisplayValue) then
    Result := ''
  else
    Result := IntToStr(ADisplayValue);
end;

procedure TdxCustomNumericWheelPickerProperties.PrepareDisplayValue(
  const AEditValue: Variant; var DisplayValue: Variant; AEditFocused: Boolean);
var
  AValue: Variant;
begin
  if VarIsNumericEx(AEditValue) or VarIsStr(AEditValue) then
  begin
    if MinValue > MaxValue then
      AValue := MinValue
    else
      AValue := Min(Max(AEditValue, MinValue), MaxValue);

    DisplayValue := VarAsType(AValue, varInteger);
  end
  else
    if VarIsNull(AEditValue) then
      DisplayValue := Null;
end;

{ TdxCustomNumericWheelPicker }

class function TdxCustomNumericWheelPicker.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxCustomNumericWheelPickerProperties;
end;

function TdxCustomNumericWheelPicker.GetEditStateColorKind: TcxEditStateColorKind;
begin
  Result := cxEditStateColorKindMap[Enabled];
end;

function TdxCustomNumericWheelPicker.GetDisplayText: string;
var
  ADisplayValue: TcxEditValue;
begin
  PrepareDisplayValue(EditValue, ADisplayValue, False);
  if VarIsNull(ADisplayValue) then
    Result := dxNumericWheelPickerGetNullValue(
      TcxCustomEditPropertiesAccess(Properties).MinValue,
      TcxCustomEditPropertiesAccess(Properties).MaxValue)
  else
    Result := ADisplayValue;
end;

procedure TdxCustomNumericWheelPicker.PropertiesChanged(Sender: TObject);
var
  ADisplayValue: TcxEditValue;
begin
  inherited;
  ActiveProperties.PrepareDisplayValue(EditValue, ADisplayValue, False);
  InternalSetSelectedValue(ADisplayValue);
end;

procedure TdxCustomNumericWheelPicker.TextChanged;
begin
  inherited;
  ShortRefreshContainer(False);
end;

{ TdxNumericWheelPickerProperties }

constructor TdxNumericWheelPickerProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FillMinMaxValues(0, 10);
end;

class function TdxNumericWheelPickerProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxNumericWheelPicker;
end;

function TdxNumericWheelPickerProperties.GetMax: Integer;
begin
  Result := Trunc(MaxValue);
end;

function TdxNumericWheelPickerProperties.GetMin: Integer;
begin
  Result := Trunc(MinValue);
end;

procedure TdxNumericWheelPickerProperties.SetMax(AValue: Integer);
begin
  MaxValue := AValue;
end;

procedure TdxNumericWheelPickerProperties.SetMin(AValue: Integer);
begin
  MinValue := AValue;
end;

{ TdxNumericWheelPicker }

class function TdxNumericWheelPicker.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxNumericWheelPickerProperties;
end;

function TdxNumericWheelPicker.GetActiveProperties: TdxNumericWheelPickerProperties;
begin
  Result := InternalGetActiveProperties as TdxNumericWheelPickerProperties;
end;

function TdxNumericWheelPicker.GetProperties: TdxNumericWheelPickerProperties;
begin
  Result := inherited Properties as TdxNumericWheelPickerProperties;
end;

procedure TdxNumericWheelPicker.SetProperties(AValue: TdxNumericWheelPickerProperties);
begin
  Properties.Assign(AValue);
end;

initialization
  GetRegisteredEditProperties.Register(TdxNumericWheelPickerProperties, scxSEditRepositoryNumericWheelPickerItem);
  FilterEditsController.Register(TdxNumericWheelPickerProperties, TcxFilterSpinEditHelper);

finalization
  FilterEditsController.Unregister(TdxNumericWheelPickerProperties, TcxFilterSpinEditHelper);
  GetRegisteredEditProperties.Unregister(TdxNumericWheelPickerProperties);

end.
