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

unit cxTimeEdit;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Classes, Controls, SysUtils,
  dxCore, cxClasses, cxContainer, cxDataUtils, cxDateUtils, cxDataStorage,
  cxEdit, cxFormats, cxMaskEdit, cxSpinEdit, cxVariants, cxFilterControlUtils;

type
  TcxTimeEditZoneKind = (tzHour, tzMin, tzSec, tzTimeSuffix);

  TcxTimeEditZoneInfo = record
    Kind: TcxTimeEditZoneKind;
    Start, Length: Integer;
    TimeSuffixKind: TcxTimeSuffixKind;
    Use24HourFormat: Boolean;
  end;

  { TcxTimeEditMaskMode }

  TcxTimeEditMaskMode = class(TcxMaskEditStandardMode)
  protected
    function GetBlank(APos: Integer): Char; override;
  end;

  { TcxCustomTimeEditProperties }

  TcxCustomTimeEdit = class;
  TcxTimeEditTimeFormat = (tfHourMinSec, tfHourMin, tfHour);

  TcxCustomTimeEditProperties = class(TcxCustomSpinEditProperties)
  private
    FAutoCorrectHours: Boolean;
    FShowDate: Boolean;
    FTimeFormat: TcxTimeEditTimeFormat;
    FUse24HourFormat: Boolean;
    FUseTimeFormatWhenUnfocused: Boolean;
    procedure SetAutoCorrectHours(Value: Boolean);
    procedure SetUse24HourFormat(Value: Boolean);
    procedure SetShowDate(Value: Boolean);
    procedure SetTimeFormat(Value: TcxTimeEditTimeFormat);
    procedure SetUseTimeFormatWhenUnfocused(Value: Boolean);
  protected
    function DefaultFocusedDisplayValue: TcxEditValue; override;
    function ExtendValueUpToBound: Boolean; override;
    procedure FormatChanged; override;
    function GetDisplayFormatOptions: TcxEditDisplayFormatOptions; override;
    function GetModeClass(AMaskKind: TcxEditMaskKind): TcxMaskEditCustomModeClass; override;
    procedure GetTimeZoneInfo(APos: Integer;
      out AInfo: TcxTimeEditZoneInfo); virtual;
    function IsDisplayValueNumeric: Boolean; override;
    function IsEditValueNumeric: Boolean; override;
    function PrepareValue(const AValue: TcxEditValue): Variant; override;
    function PreserveSelection: Boolean; override;
    function GetEditingPlace(APos: Integer): TcxTimeEditZoneKind;
    function GetTimePartLength(AKind: TcxTimeEditZoneKind): Integer; virtual;
    function GetTimePartPos(AKind: TcxTimeEditZoneKind): Integer; virtual;
    function GetTimeSuffixKind: TcxTimeSuffixKind; virtual;
    procedure UpdateEditMask;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function IsDisplayValueValid(var DisplayValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    function IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    procedure DoPrepareDisplayValue(const AEditValue: TcxEditValue; var ADisplayValue: TcxEditValue;
      AEditFocused: Boolean); override;
    procedure ValidateDisplayValue(var ADisplayValue: TcxEditValue;
      var AErrorText: TCaption; var AError: Boolean;
      AEdit: TcxCustomEdit); override;
    // !!!
    property AutoCorrectHours: Boolean read FAutoCorrectHours
      write SetAutoCorrectHours default True;
    property ShowDate: Boolean read FShowDate write SetShowDate default False;
    property TimeFormat: TcxTimeEditTimeFormat read FTimeFormat write SetTimeFormat default tfHourMinSec;
    property Use24HourFormat: Boolean read FUse24HourFormat write SetUse24HourFormat default True;
    property UseTimeFormatWhenUnfocused: Boolean read FUseTimeFormatWhenUnfocused
      write SetUseTimeFormatWhenUnfocused default True;
  end;

  { TcxTimeEditProperties }

  TcxTimeEditProperties = class(TcxCustomTimeEditProperties)
  published
    property Alignment;
    property AssignedValues;
    property AutoCorrectHours;
    property AutoSelect;
    property BeepOnError;
    property Circular;
    property ClearKey;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property ImmediatePost;
    property Increment;
    property LargeIncrement;
    property Nullstring;
    property ReadOnly;
    property ShowDate;
    property SpinButtons;
    property TimeFormat;
    property UseCtrlIncrement;
    property UseLeftAlignmentOnEditing;
    property UseNullString;
    property Use24HourFormat;
    property UseTimeFormatWhenUnfocused;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnChange;
    property OnEditValueChanged;
    property OnValidate;
  end;

  { TcxCustomTimeEdit }

  TcxCustomTimeEdit = class(TcxCustomSpinEdit)
  private
    FSavedDate: TDate;
    function GetProperties: TcxCustomTimeEditProperties;
    function GetActiveProperties: TcxCustomTimeEditProperties;
    function GetTime: TTime;
    procedure SetProperties(Value: TcxCustomTimeEditProperties);
    procedure SetTime(Value: TTime);
  protected
    function GetIncrement(AButton: TcxSpinEditButton): Double; override;
    function GetValue: Variant; override;
    function IncrementValueToStr(const AValue: TcxEditValue): string; override;
    procedure Initialize; override;
    function InternalGetEditingValue: TcxEditValue; override;
    function InternalGetText: string; override;
    procedure InternalSetDisplayValue(const Value: TcxEditValue); override;
    procedure InternalSetEditValue(const Value: TcxEditValue;
      AValidateEditValue: Boolean); override;
    function InternalSetText(const Value: string): Boolean; override;
    function IsValidChar(AChar: Char): Boolean; override;
    function IsCharValidForPos(var AChar: Char; APos: Integer): Boolean; override;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure SetValue(const Value: Variant); override;
    procedure UpdateTextFormatting; override;
    function EditingPlace: TcxTimeEditZoneKind;
  public
    procedure Clear; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function Increment(AButton: TcxSpinEditButton): Boolean; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue;
      out EditValue: TcxEditValue; AEditFocused: Boolean); override;
    property ActiveProperties: TcxCustomTimeEditProperties read GetActiveProperties;
    property Properties: TcxCustomTimeEditProperties read GetProperties
      write SetProperties;
    property Time: TTime read GetTime write SetTime stored False;
  end;

  { TcxTimeEdit }

  TcxTimeEdit = class(TcxCustomTimeEdit)
  private
    function GetActiveProperties: TcxTimeEditProperties;
    function GetProperties: TcxTimeEditProperties;
    procedure SetProperties(Value: TcxTimeEditProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxTimeEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DragMode;
    property EditValue;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxTimeEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Time;
    property Visible;
    property DragCursor;
    property DragKind;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
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
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  { TcxFilterTimeEditHelper }

  TcxFilterTimeEditHelper = class(TcxFilterSpinEditHelper)
  protected
    class procedure InitializeEdit(AEdit: TcxCustomEdit;
      AEditProperties: TcxCustomEditProperties); override;
  public
    class function GetFilterDataType(AValueTypeClass: TcxValueTypeClass): TcxFilterDataType; override;
    class function GetFilterEditClass: TcxCustomEditClass; override;
  end;

var
  cxTimeEditFormats: array [TcxTimeEditTimeFormat, Boolean, 0..1] of string = (
    (('hh:nn:ss ampm', '00:00:00 LL;1;0'), ('hh:nn:ss', '00:00:00;1;0')),
    (('hh:nn ampm', '00:00 LL;1;0'), ('hh:nn', '00:00;1;0')),
    (('hh ampm', '00 LL;1;0'), ('hh', '00;1;0'))
  );

function IsCharValidForTimeEdit(ATimeEdit: TcxCustomMaskEdit; var AChar: Char;
  APos: Integer; const ATimeZoneInfo: TcxTimeEditZoneInfo): Boolean;
implementation

uses
  StdCtrls, DateUtils, Forms, Dialogs, Math,
  dxCoreClasses, cxControls, cxEditConsts, cxTextEdit;

type

  { TcxTimeEditFormatListener }

  TcxTimeEditFormatListener = class(TcxInterfacedPersistent, IcxFormatControllerListener)
  protected
    procedure FormatChanged;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  end;

var
  cxTimeEditHalfDayIncrement,
  cxTimeEditOneHourIncrement,
  cxTimeEditOneMinIncrement,
  cxTimeEditOneSecIncrement: Double;
  FTimeEditFormatListener: TcxTimeEditFormatListener;

function EqualChars(C1, C2: Char): Boolean;
begin
  Result := UpperCase(C1) = UpperCase(C2);
end;

function GetFirstDifferenceIndex(const S1, S2: string): Integer;
var
  ACount, I: Integer;
begin
  Result := -1;
  ACount := Min(Length(S1), Length(S2));
  for I := 1 to ACount do
    if not EqualChars(S1[I], S2[I]) then
    begin
      Result := I;
      Break;
    end;
  if (Result = -1) and (Length(S1) <> Length(S2)) then
    Result := ACount + 1;
end;

procedure GetTimeSuffixes(ATimeSuffixKind: TcxTimeSuffixKind;
  out ATimeSuffix1, ATimeSuffix2: string);
begin
  case ATimeSuffixKind of
    tskAP:
      begin
        ATimeSuffix1 := 'A';
        ATimeSuffix2 := 'P';
      end;
    tskAMPM:
      begin
        ATimeSuffix1 := 'AM';
        ATimeSuffix2 := 'PM';
      end;
    tskAMPMString:
      begin
        ATimeSuffix1 := dxFormatSettings.TimeAMString;
        ATimeSuffix2 := dxFormatSettings.TimePMString;
      end;
  end;
end;

function IsCharValidForTimeEdit(ATimeEdit: TcxCustomMaskEdit;
  var AChar: Char; APos: Integer;
  const ATimeZoneInfo: TcxTimeEditZoneInfo): Boolean;

  procedure GetTimeValueItemValueRange(out AMinValue, AMaxValue: Integer);
  begin
    AMinValue := 0;
    if ATimeZoneInfo.Kind = tzHour then
      if not ATimeZoneInfo.Use24HourFormat then
      begin
        AMinValue := 1;
        AMaxValue := 12;
      end
      else
        AMaxValue := 23
    else
      AMaxValue := 59;
  end;

  procedure SetEditText(const AText: string; APos: Integer);
  var
    ASelStart: Integer;
    S: string;
  begin
    S := ATimeEdit.Text;
    Delete(S, APos, Length(AText));
    Insert(AText, S, APos);
    ASelStart := ATimeEdit.SelStart;
    TCustomEdit(ATimeEdit.InnerControl).Text := S;
    ATimeEdit.SelStart := ASelStart;
  end;

  function CheckTimeSuffix: Boolean;
  var
    AFirstDifferenceIndex: Integer;
    ATimeSuffix, ATimeSuffix1, ATimeSuffix2: string;
  begin
    GetTimeSuffixes(ATimeZoneInfo.TimeSuffixKind, ATimeSuffix1, ATimeSuffix2);
    AFirstDifferenceIndex := ATimeZoneInfo.Start +
      GetFirstDifferenceIndex(ATimeSuffix1, ATimeSuffix2) - 1;
    if APos < AFirstDifferenceIndex then
    begin
      Result := EqualChars(AChar, ATimeSuffix1[APos - ATimeZoneInfo.Start + 1]);
      if Result then
        AChar := ATimeSuffix1[APos - ATimeZoneInfo.Start + 1];
    end
    else
      if APos > AFirstDifferenceIndex then
      begin
        Result := EqualChars(AChar, ATimeEdit.Text[APos]);
        if Result then
          AChar := ATimeEdit.Text[APos];
      end
      else
      begin
        Result := True;
        if EqualChars(AChar, ATimeSuffix1[APos - ATimeZoneInfo.Start + 1]) then
          ATimeSuffix := ATimeSuffix1
        else if EqualChars(AChar, ATimeSuffix2[APos - ATimeZoneInfo.Start + 1]) then
          ATimeSuffix := ATimeSuffix2
        else
          Result := False;
        if Result then
        begin
          AChar := #0;
          SetEditText(ATimeSuffix, ATimeZoneInfo.Start);
          ATimeEdit.SelStart := ATimeZoneInfo.Start + ATimeZoneInfo.Length;
        end;
      end;
  end;

  function CheckTimeValueItemFirstDigit(ADigit: Char): Boolean;
  var
    AMaxTimeZoneValue, AMinTimeZoneValue: Integer;
  begin
    Result := True;
    GetTimeValueItemValueRange(AMinTimeZoneValue, AMaxTimeZoneValue);
    if ADigit = '0' then
    begin
      SetEditText('0' + ATimeEdit.Text[APos], APos);
      ATimeEdit.SelStart := ATimeEdit.SelStart + 1;
      AChar := #0;
    end
    else
      if ADigit <= IntToStr(AMaxTimeZoneValue)[1]  then
      begin
        SetEditText(ADigit + '0', APos);
        ATimeEdit.SelStart := ATimeEdit.SelStart + 1;
        AChar := #0;
      end
      else
      begin
        SetEditText('0', APos);
        ATimeEdit.SelStart := ATimeEdit.SelStart + 1;
      end
  end;

  function CheckTimeValueItemSecondDigit(ADigit: Char): Boolean;
  begin
    Result := ADigit <> '0';
    if Result then
    begin
      SetEditText('0', APos - 1);
      Result := True;
    end;
  end;

  function CheckTimeValueItem: Boolean;
  var
    AMaxTimeZoneValue, AMinTimeZoneValue: Integer;
    S: string;
  begin
    S := Copy(ATimeEdit.Text, ATimeZoneInfo.Start, ATimeZoneInfo.Length);
    S[APos - ATimeZoneInfo.Start + 1] := AChar;
    GetTimeValueItemValueRange(AMinTimeZoneValue, AMaxTimeZoneValue);
    Result := (StrToInt(S) >= AMinTimeZoneValue) and (StrToInt(S) <= AMaxTimeZoneValue);
    if not Result then
      if APos = ATimeZoneInfo.Start then
        Result := CheckTimeValueItemFirstDigit(AChar)
      else
        Result := CheckTimeValueItemSecondDigit(AChar);
  end;

begin
  if ATimeZoneInfo.Kind = tzTimeSuffix then
    Result := CheckTimeSuffix
  else
    Result := CheckTimeValueItem;
end;

function IsSpace(AChar: Char): Boolean;
begin
  Result :=  AChar = ' ';
end;

procedure PrepareTimeEditMasks;
var
  AMask: string;
  ATimeFormat: TcxTimeEditTimeFormat;
  I: Integer;
  S: string;
begin
  S := FormatDateTime('hh:mm:ss AMPM', 1.5);
  AMask := '';
  for I := 9 to Length(S) do
    if IsSpace(S[I]) then
      AMask := AMask + ' '
    else
      AMask := AMask + 'c';
  AMask := TrimRight(AMask);
  for ATimeFormat := tfHourMinSec to tfHour do
  begin
    S := cxTimeEditFormats[ATimeFormat, False, 1];
    S := Copy(S, 1, (3 - Integer(ATimeFormat)) * 3 - 1);
    S := S + AMask + ';1;0';
    cxTimeEditFormats[ATimeFormat, False, 1] := S;
  end;
end;

{ TcxTimeEditFormatListener }

constructor TcxTimeEditFormatListener.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  cxFormatController.AddListener(Self);
  PrepareTimeEditMasks;
end;

destructor TcxTimeEditFormatListener.Destroy;
begin
  cxFormatController.RemoveListener(Self);
  inherited Destroy;
end;

procedure TcxTimeEditFormatListener.FormatChanged;
begin
  PrepareTimeEditMasks;
end;

{ TcxTimeEditMaskMode }

function TcxTimeEditMaskMode.GetBlank(APos: Integer): Char;
begin
  if TcxCustomTimeEditProperties(Properties).GetEditingPlace(APos) = tzTimeSuffix then
    Result := #0
  else
    Result := '0';
end;

{ TcxCustomTimeEditProperties }

constructor TcxCustomTimeEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FTimeFormat := tfHourMinSec;
  ValueType := vtFloat;
  FAutoCorrectHours := True;
  FUse24HourFormat := True;
  FUseTimeFormatWhenUnfocused := True;
  UpdateEditMask;
  IgnoreMaskBlank := True;
  Increment := 1;
  MinValue := 0;
  MaxValue := 24 * 60 * 60 - 1;
  AlwaysShowBlanksAndLiterals := True;
end;

procedure TcxCustomTimeEditProperties.Assign(Source: TPersistent);
begin
  if Source is TcxCustomTimeEditProperties then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      with Source as TcxCustomTimeEditProperties do
      begin
        Self.AutoCorrectHours := AutoCorrectHours;
        Self.ShowDate := ShowDate;
        Self.TimeFormat := TimeFormat;
        Self.Use24HourFormat := Use24HourFormat;
        Self.UseTimeFormatWhenUnfocused := UseTimeFormatWhenUnfocused;
      end
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

class function TcxCustomTimeEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxTimeEdit;
end;

function TcxCustomTimeEditProperties.IsDisplayValueValid(var DisplayValue: TcxEditValue;
  AEditFocused: Boolean): Boolean;
var
  AText: string;
  ADateTime: TDateTime;
begin
  AText := VarToStr(DisplayValue);
  Result := True;
  try
    ADateTime := StrToDateTime(AText);
    DisplayValue := FormatDateTime(cxTimeEditFormats[TimeFormat, FUse24HourFormat, 0], ADateTime);
    DisplayValue := TrimRight(DisplayValue);
  except
    Result := False;
  end;
end;

function TcxCustomTimeEditProperties.IsEditValueValid(var EditValue: TcxEditValue;
  AEditFocused: Boolean): Boolean;
begin
  if VarIsStr(EditValue) then
    Result := IsDisplayValueValid(EditValue, AEditFocused)
  else
    Result := VarIsNull(EditValue) or VarIsDate(EditValue) or VarIsNumericEx(EditValue);
end;

procedure TcxCustomTimeEditProperties.DoPrepareDisplayValue(const AEditValue: TcxEditValue;
  var ADisplayValue: TcxEditValue; AEditFocused: Boolean);

  function InternalFormatDateTime(AValue: TcxEditValue): string;
  var
    ADateFormat, ATimeFormat: string;
  begin
    if AEditFocused or FUseTimeFormatWhenUnfocused then
      ATimeFormat := cxTimeEditFormats[TimeFormat, Use24HourFormat, 0]
    else
      ATimeFormat := cxFormatController.LocalFormatSettings.LongTimeFormat;

    if AEditFocused or (AValue = 0) or not ShowDate or (dxDateOf(AValue) = 0) then
      ADateFormat := ''
    else
      ADateFormat := cxFormatController.LocalFormatSettings.ShortDateFormat;

    Result := Trim(FormatDateTime(ADateFormat + ' ' + ATimeFormat, AValue));
  end;

begin
  if not VarIsDate(AEditValue) and not VarIsNumericEx(AEditValue) and not VarIsStr(AEditValue) and not VarIsNull(AEditValue) then
    raise EConvertError.CreateFmt(cxGetResourceString(@cxSEditTimeConvertError), [])
  else
    try
      if VarIsNull(AEditValue) then
      begin
        if not AEditFocused then
          ADisplayValue := ''
        else
          ADisplayValue := InternalFormatDateTime(0)
      end
      else
        if VarIsStr(AEditValue) then
          ADisplayValue := InternalFormatDateTime(StrToDateTime(AEditValue))
        else
          ADisplayValue := InternalFormatDateTime(AEditValue);
    except
      ADisplayValue := InternalFormatDateTime(SysUtils.Now);
    end;
end;

procedure TcxCustomTimeEditProperties.ValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean; AEdit: TcxCustomEdit);
var
  AIsUserErrorDisplayValue: Boolean;
begin
  if AErrorText = '' then
    AErrorText := cxGetResourceString(@cxSEditTimeConvertError);
  DoValidate(ADisplayValue, AErrorText, AError, AEdit, AIsUserErrorDisplayValue);
end;

function TcxCustomTimeEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  Result := GetValueEditorEditValueSource(AEditFocused);
end;

function TcxCustomTimeEditProperties.DefaultFocusedDisplayValue: TcxEditValue;
begin
  Result := FormatDateTime(cxTimeEditFormats[TimeFormat, FUse24HourFormat, 0], 0);
  Result := TrimRight(Result);
end;

function TcxCustomTimeEditProperties.ExtendValueUpToBound: Boolean;
begin
  Result := False;
end;

procedure TcxCustomTimeEditProperties.FormatChanged;
begin
  BeginUpdate;
  try
    UpdateEditMask;
  finally
    EndUpdate;
  end;
end;

function TcxCustomTimeEditProperties.GetDisplayFormatOptions: TcxEditDisplayFormatOptions;
begin
  Result := [dfoSupports, dfoNoCurrencyValue];
end;

function TcxCustomTimeEditProperties.GetModeClass(
  AMaskKind: TcxEditMaskKind): TcxMaskEditCustomModeClass;
begin
  Result := TcxTimeEditMaskMode;
end;

procedure TcxCustomTimeEditProperties.GetTimeZoneInfo(APos: Integer;
  out AInfo: TcxTimeEditZoneInfo);
begin
  AInfo.Kind := GetEditingPlace(APos);
  AInfo.Start := GetTimePartPos(AInfo.Kind);
  AInfo.Length := GetTimePartLength(AInfo.Kind);
  AInfo.TimeSuffixKind := GetTimeSuffixKind;
  AInfo.Use24HourFormat := Use24HourFormat;
end;

function TcxCustomTimeEditProperties.GetTimePartLength(AKind: TcxTimeEditZoneKind): Integer;
begin
  Result := 2;
end;

function TcxCustomTimeEditProperties.GetTimePartPos(AKind: TcxTimeEditZoneKind): Integer;
begin
  if AKind = tzTimeSuffix then
    Result := (3 - Integer(TimeFormat)) * 3 + 1
  else
    Result := Integer(AKind) * 3 + 1;
end;

function TcxCustomTimeEditProperties.GetTimeSuffixKind: TcxTimeSuffixKind;
begin
  Result := tskAMPMString;
end;

function TcxCustomTimeEditProperties.IsDisplayValueNumeric: Boolean;
begin
  Result := False;
end;

function TcxCustomTimeEditProperties.IsEditValueNumeric: Boolean;
begin
  Result := False;
end;

function TcxCustomTimeEditProperties.PrepareValue(const AValue: TcxEditValue): Variant;
begin
  Result := 0;
end;

function TcxCustomTimeEditProperties.PreserveSelection: Boolean;
begin
  Result := True;
end;

function TcxCustomTimeEditProperties.GetEditingPlace(
  APos: Integer): TcxTimeEditZoneKind;
var
  ATimeStringLength: Integer;
  ATimeZoneChar: Char;
  S: string;
begin
  ATimeStringLength := (3 - Integer(TimeFormat)) * 3 - 1;
  if (MaxLength > 0) and (APos > MaxLength) then
    APos := MaxLength;
  if (APos > ATimeStringLength) and not Use24HourFormat then
  begin
    Result := tzTimeSuffix;
    Exit;
  end;
  S := UpperCase(cxTimeEditFormats[TimeFormat, Use24HourFormat, 0]);
  if APos > Length(S) then
    APos := Length(S);
  ATimeZoneChar := S[APos];
  if not ((ATimeZoneChar = 'H') or (ATimeZoneChar = 'N') or
    (ATimeZoneChar = 'S')) then
  begin
    Inc(APos);
    if APos > Length(S) then
      APos := Length(S);
    ATimeZoneChar := S[APos];
  end;

  case ATimeZoneChar of
    'H':
      Result := tzHour;
    'N':
      Result := tzMin;
    else
      Result := tzSec;
  end;
end;

procedure TcxCustomTimeEditProperties.UpdateEditMask;
begin
  EditMask := cxTimeEditFormats[FTimeFormat, FUse24HourFormat, 1];
end;

procedure TcxCustomTimeEditProperties.SetAutoCorrectHours(Value: Boolean);
begin
  if Value <> FAutoCorrectHours then
    FAutoCorrectHours := Value;
end;

procedure TcxCustomTimeEditProperties.SetUse24HourFormat(Value: Boolean);
begin
  if Value <> FUse24HourFormat then
  begin
    FUse24HourFormat := Value;
    UpdateEditMask;
    Changed;
  end;
end;

procedure TcxCustomTimeEditProperties.SetShowDate(Value: Boolean);
begin
  if Value <> FShowDate then
  begin
    FShowDate := Value;
    Changed;
  end;
end;

procedure TcxCustomTimeEditProperties.SetTimeFormat(Value: TcxTimeEditTimeFormat);
begin
  if FTimeFormat <> Value then
  begin
    FTimeFormat := Value;
    UpdateEditMask;
    Changed;
  end;
end;

procedure TcxCustomTimeEditProperties.SetUseTimeFormatWhenUnfocused(
  Value: Boolean);
begin
  if FUseTimeFormatWhenUnfocused <> Value then
  begin
    FUseTimeFormatWhenUnfocused := Value;
    Changed;
  end;
end;

{ TcxCustomTimeEdit }

procedure TcxCustomTimeEdit.Clear;
begin
  Time := 0;
end;

class function TcxCustomTimeEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomTimeEditProperties;
end;

function TcxCustomTimeEdit.Increment(AButton: TcxSpinEditButton): Boolean;
var
  ACursorPosition: Integer;
  ASelStart, ASelLength: Integer;
begin
  ACursorPosition := 0;
  ASelLength := 0;
  if HandleAllocated then
  begin
    ASelStart := SelStart;
    if EditingPlace = tzTimeSuffix then
    begin
      ACursorPosition := (3 - Integer(ActiveProperties.TimeFormat)) * 3;
      while IsSpace(DisplayText[ACursorPosition]) do
        Inc(ACursorPosition);
      if IsPM(TDateTime(Time)) then
        ASelLength := Length(dxFormatSettings.TimeAMString)
      else
        ASelLength := Length(dxFormatSettings.TimePMString);
      Dec(ACursorPosition);
    end else
    begin
      ASelLength := 1;
      case ASelStart of
        0..1:
          ACursorPosition := 1;
        2..4:
          ACursorPosition := 4;
        else
          ACursorPosition := 7;
      end;
    end;
  end;
  Result := inherited Increment(AButton);
  if HandleAllocated then
    SetSelection(ACursorPosition, ASelLength);
end;

procedure TcxCustomTimeEdit.PrepareEditValue(const ADisplayValue: TcxEditValue;
  out EditValue: TcxEditValue; AEditFocused: Boolean);

  function GetCorrectTimeSuffix(const ASuffix: string): string;
  var
    APos: Integer;
  begin
    APos := Pos(dxFormatSettings.TimeAMString, ASuffix);
    if APos <> 0 then
      Result := dxFormatSettings.TimeAMString
    else
      Result := dxFormatSettings.TimePMString;
  end;

var
  ATimeStringLength: Integer;
  AText: string;
  AValue: TDateTime;
begin
  AText := ADisplayValue;
  ATimeStringLength := (3 - Integer(ActiveProperties.TimeFormat)) * 3 - 1;
  if not ActiveProperties.Use24HourFormat then
    AText := Copy(AText, 1, ATimeStringLength);
  if Length(AText) = 2 then
    AText := AText + ':00'; // delphi bug
  if not ActiveProperties.Use24HourFormat then
    AText := AText + GetCorrectTimeSuffix(
      Copy(ADisplayValue, ATimeStringLength + 1, Length(ADisplayValue) - ATimeStringLength));

  if (AText = '') or not TryStrToTime(AText, AValue) then
    AValue := 0;
  AValue := FSavedDate + AValue * cxSign(FSavedDate);

  EditValue := AValue;
end;

function TcxCustomTimeEdit.GetIncrement(AButton: TcxSpinEditButton): Double;
var
  AEditingPlace: TcxTimeEditZoneKind;
  AIsForwardDirection: Boolean;
  AMultiplier: Double;
begin
  Result := 0;
  AEditingPlace := EditingPlace;

  AMultiplier := ActiveProperties.GetIncrement(AButton);
  case AEditingPlace of
    tzTimeSuffix:
      begin
        AIsForwardDirection := AButton in [sebForward, sebFastForward];
        if AIsForwardDirection then
        begin
          Result := cxTimeEditHalfDayIncrement;
          if Value >= cxTimeEditHalfDayIncrement then
            Result := -Result;
        end
        else
        begin
          Result := -cxTimeEditHalfDayIncrement;
          if Value < cxTimeEditHalfDayIncrement then
            Result := -Result;
        end;
      end;
    tzHour: Result := cxTimeEditOneHourIncrement * AMultiplier;
    tzMin: Result := cxTimeEditOneMinIncrement * AMultiplier;
    tzSec: Result := cxTimeEditOneSecIncrement * AMultiplier;
  end;
end;

function TcxCustomTimeEdit.GetValue: Variant;
var
  AHours, ATimeSuffixLength, AValue: Integer;
  S: string;
begin
  S := Text;
  AHours := StrToInt(Copy(S, 1, 2));
  if not ActiveProperties.Use24HourFormat then
    AHours := AHours mod 12;
  AValue := AHours * 60 * 60;
  if ActiveProperties.TimeFormat <> tfHour then
    AValue := AValue + StrToInt(Copy(S, 4, 2)) * 60;
  if ActiveProperties.TimeFormat = tfHourMinSec then
    AValue := AValue + StrToInt(Copy(S, 7, 2));
  ATimeSuffixLength := Length(dxFormatSettings.TimePMString);
  if not ActiveProperties.Use24HourFormat and InternalCompareString(Copy(S,
      Length(S) - ATimeSuffixLength + 1, ATimeSuffixLength), dxFormatSettings.TimePMString, True) then
    Inc(AValue, 12 * 60 * 60);
  Result := AValue;
end;

function TcxCustomTimeEdit.IncrementValueToStr(const AValue: TcxEditValue): string;
var
  ATime: TDateTime;
begin
  ATime := AValue;
  Result := inherited IncrementValueToStr(ATime * MSecsPerSec / MSecsPerDay);
end;

procedure TcxCustomTimeEdit.Initialize;
begin
  inherited Initialize;
  ControlStyle := ControlStyle - [csSetCaption];
end;

function TcxCustomTimeEdit.InternalGetEditingValue: TcxEditValue;
begin
  PrepareEditValue(Text, Result, True);
end;

function TcxCustomTimeEdit.InternalGetText: string;
begin
  Result := DisplayText;
end;

procedure TcxCustomTimeEdit.InternalSetDisplayValue(const Value: TcxEditValue);
begin
  if ActiveProperties.Use24HourFormat or (VarToStr(Value) <> '') then
    inherited InternalSetDisplayValue(Value)
  else
    InnerTextEdit.EditValue := Value;
end;

procedure TcxCustomTimeEdit.InternalSetEditValue(const Value: TcxEditValue;
  AValidateEditValue: Boolean);
var
  AValue: TcxEditValue;
  AIsEditValueValid: Boolean;
begin
  AValue := Value;
  AIsEditValueValid := not AValidateEditValue or ActiveProperties.IsEditValueValid(AValue, InternalFocused);
  if VarIsNull(Value) or not AIsEditValueValid then
    FSavedDate := 0
  else
    if VarIsStr(AValue) then
      FSavedDate := TDate(dxDateOf(StrToDateTime(Value)))
    else
      FSavedDate := TDate(dxDateOf(Value));
  inherited InternalSetEditValue(Value, AValidateEditValue);
end;

function TcxCustomTimeEdit.InternalSetText(const Value: string): Boolean;
begin
  Result := SetDisplayText(Value);
end;

function TcxCustomTimeEdit.IsValidChar(AChar: Char): Boolean;
begin
  Result := True;
end;

function TcxCustomTimeEdit.IsCharValidForPos(var AChar: Char;
  APos: Integer): Boolean;
var
  ATimeZoneInfo: TcxTimeEditZoneInfo;
begin
  ActiveProperties.GetTimeZoneInfo(APos, ATimeZoneInfo);
  Result := IsCharValidForTimeEdit(Self, AChar, APos, ATimeZoneInfo);
end;

procedure TcxCustomTimeEdit.PropertiesChanged(Sender: TObject);
begin
  if not Focused then
    DataBinding.UpdateDisplayValue;
  inherited PropertiesChanged(Sender);
end;

procedure TcxCustomTimeEdit.SetValue(const Value: Variant);
var
  ATime: TDateTime;
begin
  ATime := Value;
  Time := ATime * MSecsPerSec / MSecsPerDay;
end;

procedure TcxCustomTimeEdit.UpdateTextFormatting;
begin
end;

function TcxCustomTimeEdit.EditingPlace: TcxTimeEditZoneKind;
begin
  Result := ActiveProperties.GetEditingPlace(SelStart + 1);
end;

function TcxCustomTimeEdit.GetProperties: TcxCustomTimeEditProperties;
begin
  Result := TcxCustomTimeEditProperties(inherited Properties);
end;

function TcxCustomTimeEdit.GetActiveProperties: TcxCustomTimeEditProperties;
begin
  Result := TcxCustomTimeEditProperties(InternalGetActiveProperties);
end;

function TcxCustomTimeEdit.GetTime: TTime;
var
  AValue: TcxEditValue;
begin
  Result := TTime(SysUtils.Now);
  try
    if Focused then
      PrepareEditValue(DisplayText, AValue, True)
    else
      AValue := EditValue;

    if VarIsNull(AValue) then
      Result := NullDate
    else
      if VarIsStr(AValue) then
        Result := TTime(StrToDateTime(AValue))
      else
        Result := TTime(VarAsType(AValue, varDate));
  finally
    Result := Abs(Frac(Result));
  end;
end;

procedure TcxCustomTimeEdit.SetProperties(Value: TcxCustomTimeEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomTimeEdit.SetTime(Value: TTime);
begin
  InternalEditValue := VarToDateTime(
    Abs(Frac(Value)) * cxSign(FSavedDate) + FSavedDate);
end;

{ TcxTimeEdit }

class function TcxTimeEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxTimeEditProperties;
end;

function TcxTimeEdit.GetActiveProperties: TcxTimeEditProperties;
begin
  Result := TcxTimeEditProperties(InternalGetActiveProperties);
end;

function TcxTimeEdit.GetProperties: TcxTimeEditProperties;
begin
  Result := TcxTimeEditProperties(inherited Properties);
end;

procedure TcxTimeEdit.SetProperties(Value: TcxTimeEditProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterTimeEditHelper }

class procedure TcxFilterTimeEditHelper.InitializeEdit(AEdit: TcxCustomEdit;
  AEditProperties: TcxCustomEditProperties);
begin
  inherited InitializeEdit(AEdit, AEditProperties);
  TcxTimeEdit(AEdit).Time := 0;
end;

class function TcxFilterTimeEditHelper.GetFilterDataType(AValueTypeClass: TcxValueTypeClass): TcxFilterDataType;
begin
  Result := fdtTime;
end;

class function TcxFilterTimeEditHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := TcxTimeEdit;
end;

initialization
  cxTimeEditHalfDayIncrement := 12 * 60 * 60;
  cxTimeEditOneHourIncrement := 60 * 60;
  cxTimeEditOneMinIncrement := 60;
  cxTimeEditOneSecIncrement := 1;
  FTimeEditFormatListener := TcxTimeEditFormatListener.Create(nil);

  GetRegisteredEditProperties.Register(TcxTimeEditProperties, scxSEditRepositoryTimeItem);
  FilterEditsController.Register(TcxTimeEditProperties, TcxFilterTimeEditHelper);

finalization
  FilterEditsController.Unregister(TcxTimeEditProperties, TcxFilterTimeEditHelper);
  FreeAndNil(FTimeEditFormatListener);

end.
