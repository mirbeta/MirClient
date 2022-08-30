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

unit dxDateTimeWheelPicker;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Variants, Classes, Graphics, Messages, Controls,
  cxGeometry, cxGraphics, dxGDIPlusClasses, dxCoreGraphics, cxLookAndFeelPainters, cxFormats, cxContainer, cxEdit,
  cxEditConsts, cxFilterControlUtils, cxDateUtils, dxWheelPicker, dxNumericWheelPicker;

type
  TdxCustomDateTimeWheelPicker = class;
  TdxDateTimeWheelPickerProperties = class;

  TdxDateTimePickerWheel = (pwYear, pwMonth, pwDay, pwHour, pwMinute, pwSecond);
  TdxDateTimePickerWheels = set of TdxDateTimePickerWheel;

  { TdxDateTimeWheelPickerItemViewInfo }

  TdxDateTimeWheelPickerItemViewInfo = class(TdxCustomNumericWheelPickerItemViewInfo)
  strict private
    FLowerFont: TFont;
    FLowerText: string;
    FLowerTextRect: TRect;
    FLowerTextSize: TSize;

    function GetLowerTextSize: TSize;
    function HasLowerText: Boolean;
  protected
    procedure CalculateTextParameters; override;
    procedure CalculateTextRects; override;
    procedure DrawText(AGPCanvas: TdxGPCanvas); override;
  public
    constructor Create(AWheelPickerViewInfo: TdxCustomWheelPickerViewInfo); override;
    destructor Destroy; override;
  end;

  { TdxDateTimeWheelPickerWheelViewInfo }

  TdxDateTimeWheelPickerWheelViewInfo = class(TdxCustomNumericWheelPickerWheelViewInfo)
  protected
    FKind: TcxDateTimeFormatItemKind;

    class function GetWheelPickerItemViewInfoClass: TdxCustomWheelPickerItemViewInfoClass; override;
    function GetItemText(AItemIndex: Integer): string; override;
    function CanTyping(AValue: Integer): Boolean; override;
    function NeedProccessKey(AKey: Word): Boolean; override;

    function GetItemLowerText(const ABounds: TRect; AIndex: Integer): string;
    procedure SetValueByDateTime(ADateTime: TcxDateTime; AUse24HourFormat: Boolean);
    procedure SynhronizeDateTime(var ADateTime: TcxDateTime; var ATimeSuffix: Integer);
  end;

  { TdxDateTimeWheelPickerViewInfo }

  TdxDateTimeWheelPickerViewInfo = class(TdxCustomNumericWheelPickerViewInfo)
  strict private
    procedure SetDateInValidRangeByDay;
    procedure SetDateInValidRangeByMonth;
    procedure SetDateInValidRangeByYear;
  protected
    FDateTime: TcxDateTime; // InternalValue
    FMaxDate: TcxDateTime;
    FMinDate: TcxDateTime;
    FUse24HourFormat: Boolean;

    class function GetWheelPickerWheelViewInfoClass: TdxCustomWheelPickerWheelViewInfoClass; override;

    function GetInternalValue: TcxEditValue; override;
    procedure SetInternalValue(AValue: Variant); override;
    procedure SynchronizeWheelIndexes; override;

    function GetDaysPerMonth: Integer;
  public
    procedure Assign(Source: TObject); override;
  end;

  { TdxDateTimeWheelPickerViewData }

  TdxDateTimeWheelPickerViewData = class(TdxCustomNumericWheelPickerViewData)
  strict private const
    TimeFormatItemKindToDateTimePickerWheel: array[dtikYear..dtikSec] of TdxDateTimePickerWheel =
      (pwYear, pwMonth, pwDay, pwHour, pwMinute, pwSecond);
  strict private
    FMaxSmallTextWidth: Integer;
    FMonthNameMaxWidth: Integer;
    FMonthWheelWidth: Integer;

    function GetMaxDate: TcxDateTime;
    function GetMinDate: TcxDateTime;
    function GetMonthCount: Integer;
    function GetProperties: TdxDateTimeWheelPickerProperties;
    function GetRightToLeftConvertedWheelKind(AWheelKind: TcxDateTimeFormatItemKind): TcxDateTimeFormatItemKind;
    function GetWheelKind(AIndex: Integer): TcxDateTimeFormatItemKind;
    function GetYearCount: Integer;
    procedure InitializeDayWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
    procedure InitializeHourWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
    procedure InitializeMinuteWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
    procedure InitializeMonthWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
    procedure InitializeSecondWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
    procedure InitializeTimeSuffixWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
    procedure InitializeYearWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
  protected
    function GetMaxText: string; override;
    function GetOptimalWidth: Integer; override;
    function GetWheelCount: Integer; override;
    function GetWheelWidth(AIndex: Integer): Integer; override;
    procedure CalculateTextSize(AViewInfo: TdxCustomWheelPickerViewInfo); override;
    procedure CalculateWheelWidth(AViewInfo: TdxCustomWheelPickerViewInfo); override;
    procedure InitializeWheel(AWheelViewInfo: TdxCustomWheelPickerWheelViewInfo; AIndex: Integer); override;

    property MaxDate: TcxDateTime read GetMaxDate;
    property MinDate: TcxDateTime read GetMinDate;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
      Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;

    property Properties: TdxDateTimeWheelPickerProperties read GetProperties;
  end;

  { TdxDateTimeWheelPickerPropertiesValues }

  TcxDateTimeWheelPickerPropertiesValues = class(TcxCustomEditPropertiesValues)
  private
    function GetMaxDate: Boolean;
    function GetMinDate: Boolean;
    function IsMaxDateStored: Boolean;
    function IsMinDateStored: Boolean;
    procedure SetMaxDate(Value: Boolean);
    procedure SetMinDate(Value: Boolean);
  published
    property MaxDate: Boolean read GetMaxDate write SetMaxDate stored IsMaxDateStored;
    property MinDate: Boolean read GetMinDate write SetMinDate stored IsMinDateStored;
  end;

  { TdxDateTimeWheelPickerProperties }

  TdxDateTimeWheelPickerProperties = class(TdxCustomNumericWheelPickerProperties)
  private
    FUse24HourFormat: Boolean;
    FWheels: TdxDateTimePickerWheels;

    function GetAssignedValues: TcxDateTimeWheelPickerPropertiesValues;
    function GetMaxDate: TDateTime;
    function GetMinDate: TDateTime;
    procedure SetAssignedValues(AValue: TcxDateTimeWheelPickerPropertiesValues);
    procedure SetMaxDate(AValue: TDateTime);
    procedure SetMinDate(AValue: TDateTime);
    procedure SetUse24HourFormat(AValue: Boolean);
    procedure SetWheels(AValue: TdxDateTimePickerWheels);
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;

    function CorrectDateByRange(AValue: Variant): Variant; virtual;
    function GetValidMaxDate: TDateTime; virtual;
    function GetValidMinDate: TDateTime; virtual;
    function IsWheelsStored: Boolean; virtual;
  public
    constructor Create(AOwner: TPersistent); override;

    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: Variant; AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    procedure PrepareDisplayValue(const AEditValue: Variant; var DisplayValue: Variant; AEditFocused: Boolean); override;
  published
    property AssignedValues: TcxDateTimeWheelPickerPropertiesValues read GetAssignedValues write SetAssignedValues;
    property ClearKey;
    property ImmediatePost;
    property MaxDate: TDateTime read GetMaxDate write SetMaxDate;
    property MinDate: TDateTime read GetMinDate write SetMinDate;
    property Use24HourFormat: Boolean read FUse24HourFormat write SetUse24HourFormat default True;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property Wheels: TdxDateTimePickerWheels read FWheels write SetWheels stored IsWheelsStored;
    property LineCount;
    property OnChange;
    property OnEditValueChanged;
    property OnValidate;
  end;

  { TdxCustomDateTimeWheelPicker }

  TdxCustomDateTimeWheelPicker = class(TdxCustomNumericWheelPicker)
  private
    function GetActiveProperties: TdxDateTimeWheelPickerProperties;
    function GetProperties: TdxDateTimeWheelPickerProperties;
    procedure SetProperties(AValue: TdxDateTimeWheelPickerProperties);
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  protected
    function GetDateTime: TDateTime; virtual;
    function GetDisplayText: string; override;
    procedure SetDateTime(AValue: TDateTime);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;

    property ActiveProperties: TdxDateTimeWheelPickerProperties read GetActiveProperties;
    property DateTime: TDateTime read GetDateTime write SetDateTime stored False;
    property Properties: TdxDateTimeWheelPickerProperties read GetProperties write SetProperties;
  end;

  { TdxDateTimeWheelPicker }

  TdxDateTimeWheelPicker = class(TdxCustomDateTimeWheelPicker)
  private
    function GetActiveProperties: TdxDateTimeWheelPickerProperties;
    function GetProperties: TdxDateTimeWheelPickerProperties;
    procedure SetProperties(AValue: TdxDateTimeWheelPickerProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;

    property ActiveProperties: TdxDateTimeWheelPickerProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DateTime;
    property EditValue;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TdxDateTimeWheelPickerProperties read GetProperties write SetProperties;
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

implementation

uses
  Windows, Math, DateUtils,
  dxCore, cxVariants, dxGDIPlusAPI, cxDrawTextUtils, cxControls, cxCalendar;

type
  TdxCustomWheelPickerViewInfoAccess = class(TdxCustomWheelPickerViewInfo);

{ TdxDateTimeWheelPickerItemViewInfo }

constructor TdxDateTimeWheelPickerItemViewInfo.Create(AWheelPickerViewInfo: TdxCustomWheelPickerViewInfo);
begin
  inherited;
  FLowerFont := TFont.Create;
end;

destructor TdxDateTimeWheelPickerItemViewInfo.Destroy;
begin
  FreeAndNil(FLowerFont);
  inherited;
end;

procedure TdxDateTimeWheelPickerItemViewInfo.CalculateTextParameters;
var
  AViewInfo: TdxDateTimeWheelPickerViewInfo;
begin
  inherited CalculateTextParameters;
  AViewInfo := WheelPickerViewInfo as TdxDateTimeWheelPickerViewInfo;
  if not AViewInfo.FIsSmallItemSize then
  begin
    FLowerFont.Assign(AViewInfo.Font);
    FLowerText := (WheelViewInfo as TdxDateTimeWheelPickerWheelViewInfo).GetItemLowerText(Bounds, FIndex);
  end
  else
    FLowerText := '';
  if not FIsRecalculate then
    FLowerTextSize := GetLowerTextSize;
  if AViewInfo.FIsNullEditValue then
    FLowerFont.Style := [fsItalic];
end;

procedure TdxDateTimeWheelPickerItemViewInfo.CalculateTextRects;
begin
  inherited CalculateTextRects;
  FLowerTextRect := cxRectCenterHorizontally(ContentBounds, FLowerTextSize.cx);
  FLowerTextRect := cxRectSetHeight(FLowerTextRect, FLowerTextSize.cy);
  FLowerTextRect := cxRectSetTop(FLowerTextRect, TextRect.Bottom);
end;

procedure TdxDateTimeWheelPickerItemViewInfo.DrawText(AGPCanvas: TdxGPCanvas);

  function GetTextColorAlpha: Byte;
  var
    AFullSize: Integer;
    AOffset: Integer;
  begin
    if TdxDateTimeWheelPickerViewInfo(WheelPickerViewInfo).FIsNullEditValue then
      Result := 130
    else
    begin
      AFullSize := cxRectHeight(TdxDateTimeWheelPickerWheelViewInfo(WheelViewInfo).FCentralRowRect);
      AOffset := Abs(Bounds.Top - TdxDateTimeWheelPickerWheelViewInfo(WheelViewInfo).FCentralRowRect.Top);
      Result := Trunc((AFullSize - AOffset) / AFullSize * 255);
    end;
  end;

const
  ADefaultAlignment: array[Boolean] of TAlignment = (taLeftJustify, taRightJustify);
var
  ARTLReading: Boolean;
begin
  inherited DrawText(AGPCanvas);
  if HasLowerText then
  begin
    ARTLReading := WheelViewInfo.WheelPickerViewInfo.UseRightToLeftReading;
    dxGPDrawText(dxGPPaintCanvas, FLowerText, FLowerTextRect, FLowerFont, dxColorToAlphaColor(GetTextColor, GetTextColorAlpha),
      ADefaultAlignment[ARTLReading], taVerticalCenter, False, TextRenderingHintSystemDefault, StringTrimmingNone,
      ARTLReading);
  end;
end;

function TdxDateTimeWheelPickerItemViewInfo.GetLowerTextSize: TSize;
begin
  if HasLowerText then
    Result := dxGPGetTextSize(FLowerText, FLowerFont)
  else
    Result := cxNullSize;
end;

function TdxDateTimeWheelPickerItemViewInfo.HasLowerText: Boolean;
begin
  Result := FLowerText <> '';
end;

{ TdxDateTimeWheelPickerWheelViewInfo }

function TdxDateTimeWheelPickerWheelViewInfo.GetItemLowerText(const ABounds: TRect; AIndex: Integer): string;
var
  ADateTime: TcxDateTime;
begin
  if TdxDateTimeWheelPickerViewInfo(WheelPickerViewInfo).FIsNullEditValue then
    ADateTime := cxGetLocalCalendar.FromDateTime(Today)
  else
    ADateTime := (WheelPickerViewInfo as TdxDateTimeWheelPickerViewInfo).FDateTime;

  case FKind of
    dtikMin:
      ADateTime.Minutes := AIndex + FFirstItemValue;
    dtikDay:
      ADateTime.Day := AIndex + FFirstItemValue;
    dtikSec:
      ADateTime.Seconds := AIndex + FFirstItemValue;
    dtikHour:
      ADateTime.Hours := AIndex + FFirstItemValue;
    dtikMonth:
      ADateTime.Month := AIndex + FFirstItemValue;
  end;

  if cxRectIntersect(ABounds, GetCentralRowRect) and cxGetLocalCalendar.IsValidDay(ADateTime.Year, ADateTime.Month, ADateTime.Day) then
    case FKind of
      dtikMin:
        Result := cxGetResourceString(@sdxDateTimeWheelPickerMinutes);
      dtikDay:
        Result := FormatDateTime('dddd', cxGetLocalCalendar.GetWeekDay(ADateTime) + 1);
      dtikSec:
        Result := cxGetResourceString(@sdxDateTimeWheelPickerSeconds);
      dtikHour:
        Result := cxGetResourceString(@sdxDateTimeWheelPickerHours);
      dtikMonth:
        Result := cxGetLocalMonthName(ADateTime.Year, ADateTime.Month, cxGetLocalCalendar);
    end
  else
    Result := '';
end;

procedure TdxDateTimeWheelPickerWheelViewInfo.SetValueByDateTime(ADateTime: TcxDateTime; AUse24HourFormat: Boolean);

  procedure SetDisplayHour;
  begin
    if AUse24HourFormat or (ADateTime.Hours < 12) then
      Value := ADateTime.Hours
    else
      Value := ADateTime.Hours - 12;
  end;

  procedure SetDisplayTimeSuffix;
  begin
    if ADateTime.Hours < 12 then
      Value := 0
    else
      Value := 1;
  end;

begin
  if TdxDateTimeWheelPickerViewInfo(WheelPickerViewInfo).FIsNullEditValue then
    ADateTime := cxGetLocalCalendar.FromDateTime(Today);

  case FKind of
    dtikSec:
      Value := ADateTime.Seconds;
    dtikMin:
      Value := ADateTime.Minutes;
    dtikHour:
      SetDisplayHour;
    dtikDay:
      Value := ADateTime.Day;
    dtikMonth:
      Value := ADateTime.Month;
    dtikYear:
      Value := ADateTime.Year;
    dtikTimeSuffix:
      SetDisplayTimeSuffix;
  end;
end;

procedure TdxDateTimeWheelPickerWheelViewInfo.SynhronizeDateTime(var ADateTime: TcxDateTime; var ATimeSuffix: Integer);
begin
  case FKind of
    dtikSec:
      ADateTime.Seconds := Value;
    dtikMin:
      ADateTime.Minutes := Value;
    dtikHour:
      ADateTime.Hours := Value;
    dtikDay:
      ADateTime.Day := Value;
    dtikMonth:
      ADateTime.Month := Value;
    dtikYear:
      ADateTime.Year := Value;
    dtikTimeSuffix:
      ATimeSuffix := Value;
  end;
end;

class function TdxDateTimeWheelPickerWheelViewInfo.GetWheelPickerItemViewInfoClass: TdxCustomWheelPickerItemViewInfoClass;
begin
  Result := TdxDateTimeWheelPickerItemViewInfo;
end;

function TdxDateTimeWheelPickerWheelViewInfo.GetItemText(AItemIndex: Integer): string;

  procedure CalculateHoursText(out AText: string);
  begin
    if not TdxDateTimeWheelPickerViewInfo(WheelPickerViewInfo).FUse24HourFormat then
    begin
      if AItemIndex = 0 then
        Result := '12'
      else
        Result := Format('%.2d', [AItemIndex]);
    end
    else
      Result := Format('%.2d', [AItemIndex]);
  end;

const
  ItemIndexToSuffix: array[0..1] of string = ('AM', 'PM');
var
  AValue: Integer;
begin
  AValue := AItemIndex + FFirstItemValue;
  case FKind of
    dtikMin, dtikDay, dtikSec:
      Result := Format('%.2d', [AValue]);
    dtikHour:
      CalculateHoursText(Result);
    dtikYear:
      Result := Format('%.4d', [AValue]);
    dtikTimeSuffix:
      Result := ItemIndexToSuffix[AValue];
    dtikMonth:
      if TdxDateTimeWheelPickerViewInfo(WheelPickerViewInfo).FIsSmallItemSize then
        Result := GetItemLowerText(Bounds, AItemIndex)
      else
        Result := Format('%.2d', [AValue]);
  end;
end;

function TdxDateTimeWheelPickerWheelViewInfo.CanTyping(AValue: Integer): Boolean;
var
  AWheel: TdxDateTimeWheelPickerWheelViewInfo;
  ATypingStringLength: Integer;
begin
  ATypingStringLength := Length(TypingString);
  AWheel := TdxCustomWheelPickerViewInfoAccess(WheelPickerViewInfo).SelectedWheel as TdxDateTimeWheelPickerWheelViewInfo;
  Result :=
    (AValue * 10 <= MaxValue) and (ATypingStringLength <> 4) and
    ((ATypingStringLength <> 1) or (AWheel.FKind <> dtikTimeSuffix)) and
    ((ATypingStringLength <> 2) or (AWheel.FKind = dtikYear));
end;

function TdxDateTimeWheelPickerWheelViewInfo.NeedProccessKey(AKey: Word): Boolean;
begin
  Result := dxCharIsNumeric(GetCharFromKeyCode(AKey));
end;

{ TdxDateTimeWheelPickerViewInfo }

procedure TdxDateTimeWheelPickerViewInfo.Assign(Source: TObject);
var
  AViewInfo: TdxDateTimeWheelPickerViewInfo;
begin
  if Source is TdxDateTimeWheelPickerViewInfo then
  begin
    AViewInfo := TdxDateTimeWheelPickerViewInfo(Source);
    FDateTime := AViewInfo.FDateTime;
    FMaxDate := AViewInfo.FMaxDate;
    FMinDate := AViewInfo.FMinDate;
    FUse24HourFormat := AViewInfo.FUse24HourFormat;
  end;
  inherited Assign(Source);
end;

function TdxDateTimeWheelPickerViewInfo.GetDaysPerMonth: Integer;
begin
  Result := DaysPerMonth(FDateTime.Year, FDateTime.Month);
end;

class function TdxDateTimeWheelPickerViewInfo.GetWheelPickerWheelViewInfoClass: TdxCustomWheelPickerWheelViewInfoClass;
begin
  Result := TdxDateTimeWheelPickerWheelViewInfo;
end;

function TdxDateTimeWheelPickerViewInfo.GetInternalValue: TcxEditValue;

  procedure AdjustValue(ATimeSuffix: Integer);
  begin
    if FDateTime.Day = 0 then
      FDateTime.Day := FMinDate.Day;
    if FDateTime.Month = 0 then
      FDateTime.Month := FMinDate.Month;
    if FDateTime.Year = 0 then
      FDateTime.Year := FMinDate.Year;
    if SelectedWheel <> nil then
      case (SelectedWheel as TdxDateTimeWheelPickerWheelViewInfo).FKind of
        dtikYear:
          SetDateInValidRangeByYear;
        dtikMonth:
          SetDateInValidRangeByMonth;
        dtikDay:
          SetDateInValidRangeByDay;
      end;
    FDateTime.Month := Max(1, Min(FDateTime.Month, 12));
    FDateTime.Day := Max(1, Min(FDateTime.Day, DaysPerMonth(FDateTime.Year, FDateTime.Month)));
    if not FUse24HourFormat and (ATimeSuffix = 1) then
      FDateTime.Hours := FDateTime.Hours + 12;
  end;

var
  ATimeSuffix: Integer;
  I: Integer;
begin
  for I := 0 to FWheels.Count - 1 do
    (FWheels[I] as TdxDateTimeWheelPickerWheelViewInfo).SynhronizeDateTime(FDateTime, ATimeSuffix);

  AdjustValue(ATimeSuffix);
  Result := cxGetLocalCalendar.ToDateTime(FDateTime);
end;

procedure TdxDateTimeWheelPickerViewInfo.SetInternalValue(AValue: Variant);
begin
  inherited;
  if not FIsNullEditValue then
    FDateTime := cxGetLocalCalendar.FromDateTime(AValue);
end;

procedure TdxDateTimeWheelPickerViewInfo.SynchronizeWheelIndexes;
var
  I: Integer;
begin
  for I := 0 to FWheels.Count - 1 do
    (FWheels[I] as TdxDateTimeWheelPickerWheelViewInfo).SetValueByDateTime(FDateTime, FUse24HourFormat);
end;

procedure TdxDateTimeWheelPickerViewInfo.SetDateInValidRangeByDay;
begin
  if (FDateTime.Year = FMaxDate.Year) and (FDateTime.Month = FMaxDate.Month) and (FDateTime.Day > FMaxDate.Day) then
    FDateTime.Month := Min(FDateTime.Month, FMaxDate.Month - 1)
  else
    if (FDateTime.Year = FMinDate.Year) and (FDateTime.Month = FMinDate.Month) and (FDateTime.Day < FMinDate.Day) then
      FDateTime.Month := Max(FDateTime.Month, FMinDate.Month + 1);
end;

procedure TdxDateTimeWheelPickerViewInfo.SetDateInValidRangeByMonth;
begin
  if FDateTime.Year = FMaxDate.Year then
  begin
    if FDateTime.Month = FMaxDate.Month then
      FDateTime.Day := Min(FDateTime.Day, FMaxDate.Day)
    else
      if FDateTime.Month > FMaxDate.Month then
        FDateTime.Year := Min(FDateTime.Year, FMaxDate.Year - 1);
  end
  else
    if FDateTime.Year = FMinDate.Year then
    begin
      if FDateTime.Month = FMinDate.Month then
        FDateTime.Day := Max(FDateTime.Day, FMinDate.Day)
      else
        if FDateTime.Month < FMinDate.Month then
          FDateTime.Year := Max(FDateTime.Year, FMinDate.Year + 1);
    end;
end;

procedure TdxDateTimeWheelPickerViewInfo.SetDateInValidRangeByYear;
begin
  if FDateTime.Year = FMaxDate.Year then
  begin
    if FDateTime.Month = FMaxDate.Month then
      FDateTime.Day := Min(FDateTime.Day, FMaxDate.Day)
    else
      if FDateTime.Month > FMaxDate.Month then
        FDateTime.Month := Min(FDateTime.Month, FMaxDate.Month);
  end
  else
    if FDateTime.Year = FMinDate.Year then
    begin
      if FDateTime.Month = FMinDate.Month then
        FDateTime.Day := Max(FDateTime.Day, FMinDate.Day)
      else
        if FDateTime.Month < FMinDate.Month then
          FDateTime.Month := Max(FDateTime.Month, FMinDate.Month)
    end;
  SetDateInValidRangeByMonth;
end;

{ TdxDateTimeWheelPickerViewData }

procedure TdxDateTimeWheelPickerViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  ADateTimeWheelPickerViewInfo: TdxDateTimeWheelPickerViewInfo;
begin
  ADateTimeWheelPickerViewInfo := AViewInfo as TdxDateTimeWheelPickerViewInfo;
  ADateTimeWheelPickerViewInfo.FUse24HourFormat := Properties.Use24HourFormat;
  ADateTimeWheelPickerViewInfo.FMinDate := MinDate;
  ADateTimeWheelPickerViewInfo.FMaxDate := MaxDate;
  inherited;
end;

procedure TdxDateTimeWheelPickerViewData.CalculateTextSize(AViewInfo: TdxCustomWheelPickerViewInfo);

  function GetMonthNameMaxWidth: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to 12 do
      Result := Max(Result, dxGPGetTextSize(dxFormatSettings.LongMonthNames[I], Style.Font).cx);
  end;

  function GetDayNameMaxWidth: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to 7 do
      Result := Max(Result, dxGPGetTextSize(dxFormatSettings.LongDayNames[I], Style.Font).cx);
  end;

begin
  inherited;
  FMaxTextSize.cx := Max(FMaxTextSize.cx, GetDayNameMaxWidth);
  FMaxSmallTextWidth := dxGPGetTextSize(GetMaxText, Style.Font).cx;
  FMonthNameMaxWidth := GetMonthNameMaxWidth;
end;

procedure TdxDateTimeWheelPickerViewData.InitializeWheel(AWheelViewInfo: TdxCustomWheelPickerWheelViewInfo; AIndex: Integer);
var
  ADateTimeWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo;
begin
  inherited;
  ADateTimeWheelViewInfo := AWheelViewInfo as TdxDateTimeWheelPickerWheelViewInfo;
  ADateTimeWheelViewInfo.FKind := GetWheelKind(AIndex);
  ADateTimeWheelViewInfo.FCyclic := True;
  ADateTimeWheelViewInfo.FFirstItemValue := 0;
  case ADateTimeWheelViewInfo.FKind of
    dtikTimeSuffix:
      InitializeTimeSuffixWheel(ADateTimeWheelViewInfo);
    dtikSec:
      InitializeSecondWheel(ADateTimeWheelViewInfo);
    dtikMin:
      InitializeMinuteWheel(ADateTimeWheelViewInfo);
    dtikHour:
      InitializeHourWheel(ADateTimeWheelViewInfo);
    dtikDay:
      InitializeDayWheel(ADateTimeWheelViewInfo);
    dtikMonth:
      InitializeMonthWheel(ADateTimeWheelViewInfo);
    dtikYear:
      InitializeYearWheel(ADateTimeWheelViewInfo);
  end;
end;

function TdxDateTimeWheelPickerViewData.GetOptimalWidth: Integer;
begin
  Result := (FWheelWidth + GetWheelIndent) * (GetWheelCount - 1) + (FMonthWheelWidth + GetWheelIndent) + GetWheelIndent
end;

function TdxDateTimeWheelPickerViewData.GetWheelCount: Integer;
var
  I: TdxDateTimePickerWheel;
begin
  Result := 0;
  for I := pwYear to pwSecond do
    if I in Properties.Wheels then
      Inc(Result);
  if not Properties.Use24HourFormat and (pwHour in Properties.Wheels) then
    Inc(Result);
end;

function TdxDateTimeWheelPickerViewData.GetWheelWidth(AIndex: Integer): Integer;
begin
  if GetWheelKind(AIndex) = dtikMonth then
    Result := FMonthWheelWidth
  else
    Result := FWheelWidth;
end;

procedure TdxDateTimeWheelPickerViewData.CalculateWheelWidth(AViewInfo: TdxCustomWheelPickerViewInfo);
begin
  if (AViewInfo as TdxDateTimeWheelPickerViewInfo).FIsSmallItemSize then
  begin
    FWheelWidth := FMaxSmallTextWidth + 2 * dxWheelPickerItemContentMargin;
    FMonthWheelWidth := FMonthNameMaxWidth + 2 * dxWheelPickerItemContentMargin;
  end
  else
  begin
    FWheelWidth := Max(FWheelWidth, FMaxTextSize.cx);
    FWheelWidth := Max(FWheelWidth, FMonthNameMaxWidth);
    FWheelWidth := FWheelWidth + 2 * dxWheelPickerItemContentMargin;
    FMonthWheelWidth := FWheelWidth;
  end;
end;

function TdxDateTimeWheelPickerViewData.GetMaxDate: TcxDateTime;
begin
  Result := cxGetLocalCalendar.FromDateTime(Properties.GetValidMaxDate);
end;

function TdxDateTimeWheelPickerViewData.GetMinDate: TcxDateTime;
begin
  Result := cxGetLocalCalendar.FromDateTime(Properties.GetValidMinDate);
end;

function TdxDateTimeWheelPickerViewData.GetProperties: TdxDateTimeWheelPickerProperties;
begin
  Result := FProperties as TdxDateTimeWheelPickerProperties;
end;

function TdxDateTimeWheelPickerViewData.GetRightToLeftConvertedWheelKind(
  AWheelKind: TcxDateTimeFormatItemKind): TcxDateTimeFormatItemKind;

  procedure SwapWheelKinds(var AWheelKind: TcxDateTimeFormatItemKind;
    const ASwapWheelKind1, ASwapWheelKind2: TcxDateTimeFormatItemKind);
  begin
    if AWheelKind = ASwapWheelKind1 then
      AWheelKind := ASwapWheelKind2
    else
      if AWheelKind = ASwapWheelKind2 then
        AWheelKind := ASwapWheelKind1;
  end;

var
  AIsDayWheelVisible, AIsMonthWheelVisible, AIsYearWheelVisible: Boolean;
begin
  Result := AWheelKind;

  AIsDayWheelVisible := TimeFormatItemKindToDateTimePickerWheel[dtikDay] in Properties.Wheels;
  AIsMonthWheelVisible := TimeFormatItemKindToDateTimePickerWheel[dtikMonth] in Properties.Wheels;
  AIsYearWheelVisible := TimeFormatItemKindToDateTimePickerWheel[dtikYear] in Properties.Wheels;

  if AIsDayWheelVisible and AIsYearWheelVisible then
    SwapWheelKinds(Result, dtikDay, dtikYear)
  else
    if AIsMonthWheelVisible then
    begin
      if AIsDayWheelVisible then
        SwapWheelKinds(Result, dtikDay, dtikMonth)
      else
        if AIsYearWheelVisible then
          SwapWheelKinds(Result, dtikMonth, dtikYear);
    end;
end;

function TdxDateTimeWheelPickerViewData.GetWheelKind(AIndex: Integer): TcxDateTimeFormatItemKind;
var
  ACounter: Integer;
  I: Integer;
  AKind: TcxDateTimeFormatItemKind;
begin
  ACounter := 0;
  Result := dtikTimeSuffix;
  for I := 0 to cxFormatController.DateTimeFormatInfo.ItemCount - 1 do
  begin
    AKind := cxFormatController.DateTimeFormatInfo.Items[I].Kind;
    if (AKind in [dtikYear..dtikSec]) and (TimeFormatItemKindToDateTimePickerWheel[AKind] in Properties.Wheels) then
    begin
      if ACounter = AIndex then
      begin
        Result := AKind;
        Break;
      end
      else
        Inc(ACounter);
    end;
  end;

  if UseRightToLeftAlignment then
    Result := GetRightToLeftConvertedWheelKind(Result);
end;

function TdxDateTimeWheelPickerViewData.GetMonthCount: Integer;
begin
  if GetYearCount > 1 then
    Result := 12
  else
    Result := MaxDate.Month - MinDate.Month + 1;
end;

function TdxDateTimeWheelPickerViewData.GetYearCount: Integer;
begin
  Result := Min(dxMaxYear, MaxDate.Year) - Max(dxMinYear, MinDate.Year) + 1;
end;

procedure TdxDateTimeWheelPickerViewData.InitializeDayWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
var
  ADayCount: Integer;
  AViewInfo: TdxDateTimeWheelPickerViewInfo;
begin
  AViewInfo := AWheelViewInfo.WheelPickerViewInfo as TdxDateTimeWheelPickerViewInfo;
  if AViewInfo.FIsNullEditValue then
    ADayCount := DaysPerMonth(cxGetLocalCalendar.FromDateTime(Today).Year, cxGetLocalCalendar.FromDateTime(Today).Month)
  else
    ADayCount := AViewInfo.GetDaysPerMonth;

  if GetMonthCount > 1 then
  begin
    AWheelViewInfo.FWheelItemCount := ADayCount;
    AWheelViewInfo.FFirstItemValue := 1;
  end
  else
  begin
    AWheelViewInfo.FWheelItemCount := Min(ADayCount, MaxDate.Day) - MinDate.Day + 1;
    AWheelViewInfo.FFirstItemValue := MinDate.Day;
  end;
end;

procedure TdxDateTimeWheelPickerViewData.InitializeHourWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
begin
  if Properties.Use24HourFormat then
    AWheelViewInfo.FWheelItemCount := 24
  else
    AWheelViewInfo.FWheelItemCount := 12;
end;

procedure TdxDateTimeWheelPickerViewData.InitializeMinuteWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
begin
  AWheelViewInfo.FWheelItemCount := MinsPerHour;
  AWheelViewInfo.FFirstItemValue := 0;
end;

procedure TdxDateTimeWheelPickerViewData.InitializeMonthWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
begin
  AWheelViewInfo.FWheelItemCount := GetMonthCount;
  if GetYearCount > 1 then
    AWheelViewInfo.FFirstItemValue := 1
  else
    AWheelViewInfo.FFirstItemValue := MinDate.Month;
end;

procedure TdxDateTimeWheelPickerViewData.InitializeSecondWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
begin
  AWheelViewInfo.FWheelItemCount := SecsPerMin;
  AWheelViewInfo.FFirstItemValue := 0;
end;

procedure TdxDateTimeWheelPickerViewData.InitializeTimeSuffixWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
begin
  AWheelViewInfo.FWheelItemCount := 2;
end;

procedure TdxDateTimeWheelPickerViewData.InitializeYearWheel(AWheelViewInfo: TdxDateTimeWheelPickerWheelViewInfo);
begin
  AWheelViewInfo.FWheelItemCount := GetYearCount;
  AWheelViewInfo.FFirstItemValue := Max(dxMinYear, MinDate.Year);
end;

function TdxDateTimeWheelPickerViewData.GetMaxText: string;
begin
  Result := '0000';
end;

{ TcxDateTimeWheelPickerPropertiesValues }

function TcxDateTimeWheelPickerPropertiesValues.GetMaxDate: Boolean;
begin
  Result := MaxValue;
end;

function TcxDateTimeWheelPickerPropertiesValues.GetMinDate: Boolean;
begin
  Result := MinValue;
end;

function TcxDateTimeWheelPickerPropertiesValues.IsMaxDateStored: Boolean;
begin
  Result := IsMaxValueStored;
end;

function TcxDateTimeWheelPickerPropertiesValues.IsMinDateStored: Boolean;
begin
  Result := IsMinValueStored;
end;

procedure TcxDateTimeWheelPickerPropertiesValues.SetMaxDate(Value: Boolean);
begin
  MaxValue := Value;
end;

procedure TcxDateTimeWheelPickerPropertiesValues.SetMinDate(Value: Boolean);
begin
  MinValue := Value;
end;

{ TdxDateTimeWheelPickerProperties }

constructor TdxDateTimeWheelPickerProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FWheels := [pwYear, pwMonth, pwDay];
  FUse24HourFormat := True;
end;

class function TdxDateTimeWheelPickerProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxDateTimeWheelPicker;
end;

function TdxDateTimeWheelPickerProperties.GetDisplayText(const AEditValue: Variant; AFullText, AIsInplace: Boolean): string;
var
  ADisplayValue: TcxEditValue;
begin
  PrepareDisplayValue(AEditValue, ADisplayValue, False);
  if VarIsNull(ADisplayValue) then
    Result := ''
  else
    Result := DateToStr(ADisplayValue);
end;

class function TdxDateTimeWheelPickerProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxDateTimeWheelPickerViewInfo;
end;

procedure TdxDateTimeWheelPickerProperties.PrepareDisplayValue(const AEditValue: Variant;
  var DisplayValue: Variant; AEditFocused: Boolean);
var
  ADateTime: TDateTime;
begin
  if VarIsStr(AEditValue) then
  begin
    if TextToDateEx(AEditValue, ADateTime) then
      DisplayValue := ADateTime
    else
      DisplayValue := NullDate;
  end
  else
    if VarIsDate(AEditValue) then
      DisplayValue := AEditValue
    else
      if VarIsNumericEx(AEditValue) then
        DisplayValue := VarToDateTime(AEditValue)
      else
        if VarIsNull(AEditValue) then
          DisplayValue := Null
        else
          DisplayValue := NullDate;

  DisplayValue := CorrectDateByRange(DisplayValue);
end;

procedure TdxDateTimeWheelPickerProperties.DoAssign(AProperties: TcxCustomEditProperties);
var
  ADateTimeWheelPickerProperties: TdxDateTimeWheelPickerProperties;
begin
  inherited;
  if AProperties is TdxDateTimeWheelPickerProperties then
  begin
    ADateTimeWheelPickerProperties := TdxDateTimeWheelPickerProperties(AProperties);
    Wheels := ADateTimeWheelPickerProperties.Wheels;
    Use24HourFormat := ADateTimeWheelPickerProperties.Use24HourFormat;
    AssignedValues.MaxDate := False;
    if AssignedValues.MaxDate then
      MaxDate := ADateTimeWheelPickerProperties.MaxDate;
    AssignedValues.MinDate := False;
    if AssignedValues.MinDate then
      MinDate := ADateTimeWheelPickerProperties.MinDate;
  end;
end;

class function TdxDateTimeWheelPickerProperties.GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass;
begin
  Result := TcxDateTimeWheelPickerPropertiesValues;
end;

class function TdxDateTimeWheelPickerProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxDateTimeWheelPickerViewData;
end;

function TdxDateTimeWheelPickerProperties.CorrectDateByRange(AValue: Variant): Variant;
var
  ATime: TTime;
begin
  Result := AValue;
  if Result <> Null then
  begin
    if VarIsDate(Result) or VarIsNumeric(Result) then
    begin
      ATime := dxTimeOf(Result);
      Result := dxDateOf(Result);
      Result := Max(Min(Result, GetValidMaxDate), GetValidMinDate);
      if Result >= 0 then
        Result := Result + ATime
      else
        Result := Result - ATime;
    end
    else
      Result := NullDate;

    Result := VarToDateTime(Result);
  end;
end;

function TdxDateTimeWheelPickerProperties.GetValidMaxDate: TDateTime;
begin
  if AssignedValues.MaxDate and (MaxDate <> NullDate) and (MinDate < MaxDate) then
    Result := MaxDate
  else
    Result := MaxDateTime;
end;

function TdxDateTimeWheelPickerProperties.GetValidMinDate: TDateTime;
begin
  if AssignedValues.MinDate and (MinDate <> NullDate) and (MinDate < MaxDate)  then
    Result := MinDate
  else
    Result := MinDateTime;
end;

function TdxDateTimeWheelPickerProperties.IsWheelsStored: Boolean;
begin
  Result := FWheels <> [pwYear, pwMonth, pwDay];
end;

function TdxDateTimeWheelPickerProperties.GetAssignedValues: TcxDateTimeWheelPickerPropertiesValues;
begin
  Result := FAssignedValues as TcxDateTimeWheelPickerPropertiesValues;
end;

function TdxDateTimeWheelPickerProperties.GetMaxDate: TDateTime;
begin
  Result := dxDateOf(inherited MaxValue);
end;

function TdxDateTimeWheelPickerProperties.GetMinDate: TDateTime;
begin
  Result := dxDateOf(inherited MinValue);
end;

procedure TdxDateTimeWheelPickerProperties.SetAssignedValues(AValue: TcxDateTimeWheelPickerPropertiesValues);
begin
  FAssignedValues.Assign(AValue);
end;

procedure TdxDateTimeWheelPickerProperties.SetMaxDate(AValue: TDateTime);
begin
  inherited MaxValue := AValue;
end;

procedure TdxDateTimeWheelPickerProperties.SetMinDate(AValue: TDateTime);
begin
  inherited MinValue := AValue;
end;

procedure TdxDateTimeWheelPickerProperties.SetUse24HourFormat(AValue: Boolean);
begin
  if AValue <> FUse24HourFormat then
  begin
    FUse24HourFormat := AValue;
    Changed;
  end;
end;

procedure TdxDateTimeWheelPickerProperties.SetWheels(AValue: TdxDateTimePickerWheels);
begin
  if AValue <> FWheels then
  begin
    FWheels := AValue;
    Changed;
  end;
end;

{ TdxCustomDateTimeWheelPicker }

class function TdxCustomDateTimeWheelPicker.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxDateTimeWheelPickerProperties;
end;

function TdxCustomDateTimeWheelPicker.GetDateTime: TDateTime;
begin
  if VarIsNull(EditValue) then
    Result := NullDate
  else
    if VarIsNumericEx(EditValue) or VarIsDate(EditValue) then
      Result := EditValue
    else
      if VarIsStr(EditValue) then
        Result := StrToDateTime(EditValue)
      else
        Result := NullDate;
end;

function TdxCustomDateTimeWheelPicker.GetDisplayText: string;
begin
  Result := ActiveProperties.GetDisplayText(EditValue);
end;

procedure TdxCustomDateTimeWheelPicker.SetDateTime(AValue: TDateTime);
begin
  InternalEditValue := AValue;
end;

function TdxCustomDateTimeWheelPicker.GetActiveProperties: TdxDateTimeWheelPickerProperties;
begin
  Result := InternalGetActiveProperties as TdxDateTimeWheelPickerProperties;
end;

function TdxCustomDateTimeWheelPicker.GetProperties: TdxDateTimeWheelPickerProperties;
begin
  Result := inherited Properties as TdxDateTimeWheelPickerProperties;
end;

procedure TdxCustomDateTimeWheelPicker.SetProperties(AValue: TdxDateTimeWheelPickerProperties);
begin
  Properties.Assign(AValue);
end;

procedure TdxCustomDateTimeWheelPicker.CMBiDiModeChanged(var Message: TMessage);
begin
  TdxCustomWheelPickerViewInfoAccess(ViewInfo).FIsDirty := True;
  inherited;
end;

{ TdxDateTimeWheelPicker }

class function TdxDateTimeWheelPicker.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxDateTimeWheelPickerProperties;
end;

function TdxDateTimeWheelPicker.GetActiveProperties: TdxDateTimeWheelPickerProperties;
begin
  Result := InternalGetActiveProperties as TdxDateTimeWheelPickerProperties;
end;

function TdxDateTimeWheelPicker.GetProperties: TdxDateTimeWheelPickerProperties;
begin
  Result := inherited Properties as TdxDateTimeWheelPickerProperties;
end;

procedure TdxDateTimeWheelPicker.SetProperties(AValue: TdxDateTimeWheelPickerProperties);
begin
  Properties.Assign(AValue);
end;

initialization
  GetRegisteredEditProperties.Register(TdxDateTimeWheelPickerProperties, scxSEditRepositoryDateTimeWheelPickerItem);
  FilterEditsController.Register(TdxDateTimeWheelPickerProperties, TcxFilterDateEditHelper);

finalization
  FilterEditsController.Unregister(TdxDateTimeWheelPickerProperties, TcxFilterDateEditHelper);
  GetRegisteredEditProperties.Unregister(TdxDateTimeWheelPickerProperties);

end.
