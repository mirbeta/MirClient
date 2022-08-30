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

unit cxCurrencyEdit;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Types, SysUtils, Classes, Controls, Clipbrd, cxContainer,
  cxDataStorage, cxDataUtils, cxEdit, cxTextEdit, cxFilterControlUtils;

type
  { TcxCurrencyEditPropertiesValues }

  TcxCurrencyEditPropertiesValues = class(TcxTextEditPropertiesValues)
  private
    FDecimalPlaces: Boolean;
    procedure SetDecimalPlaces(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property DecimalPlaces: Boolean read FDecimalPlaces write SetDecimalPlaces
      stored False;
  end;

  { TcxCustomCurrencyEditProperties }

  TcxCustomCurrencyEditProperties = class(TcxCustomTextEditProperties)
  private
    FDecimalPlaces: Integer;
    FFormatChanging: Boolean;
    FNullable: Boolean;
    FUseThousandSeparator: Boolean;
    function GetAssignedValues: TcxCurrencyEditPropertiesValues;
    function GetDecimalPlaces: Integer;
    function IsDecimalPlacesStored: Boolean;
    procedure SetAssignedValues(Value: TcxCurrencyEditPropertiesValues);
    procedure SetDecimalPlaces(Value: Integer);
    procedure SetNullable(Value: Boolean);
    procedure SetUseThousandSeparator(const Value: Boolean);
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass; override;
    function GetDefaultDisplayFormat: string; override;
    function GetDisplayFormatOptions: TcxEditDisplayFormatOptions; override;
    function HasDigitGrouping(AIsDisplayValueSynchronizing: Boolean): Boolean; override;
    function IsEditValueNumeric: Boolean; override;
    property AssignedValues: TcxCurrencyEditPropertiesValues read GetAssignedValues write SetAssignedValues;
  public
    constructor Create(AOwner: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    function IsDisplayValueValid(var ADisplayValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    procedure DoPrepareDisplayValue(const AEditValue: TcxEditValue; var ADisplayValue: TcxEditValue;
      AEditFocused: Boolean); override;
    procedure ValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
      var AError: Boolean; AEdit: TcxCustomEdit); override;
    // !!!
    property DecimalPlaces: Integer read GetDecimalPlaces write SetDecimalPlaces stored IsDecimalPlacesStored;
    property Nullable: Boolean read FNullable write SetNullable default True;
    property ValidateOnEnter default True;
    property UseThousandSeparator: Boolean read FUseThousandSeparator write SetUseThousandSeparator default False;
  end;

  { TcxCurrencyEditProperties }

  TcxCurrencyEditProperties = class(TcxCustomCurrencyEditProperties)
  published
    property Alignment;
    property AssignedValues;
    property AutoSelect;
    property ClearKey;
    property DecimalPlaces;
    property DisplayFormat;
    property EchoMode;
    property EditFormat;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property Nullable;
    property Nullstring; //lowercase because define Nullstring in CBuilder
    property PasswordChar;
    property ReadOnly;
    property UseDisplayFormatWhenEditing;
    property UseLeftAlignmentOnEditing;
    property UseNullString;
    property UseThousandSeparator;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnChange;
    property OnEditValueChanged;
    property OnValidate;
  end;

  { TcxCustomCurrencyEdit }

  TcxCustomCurrencyEdit = class(TcxCustomTextEdit)
  private
    function GetProperties: TcxCustomCurrencyEditProperties;
    function GetActiveProperties: TcxCustomCurrencyEditProperties;
    function GetValue: Double;
    procedure SetProperties(Value: TcxCustomCurrencyEditProperties);
    procedure SetValue(Value: Double);
  protected
    procedure CheckEditorValueBounds; override;
    procedure Initialize; override;
    function InternalGetEditingValue: TcxEditValue; override;
    function IsValidChar(Key: Char): Boolean; override;
    procedure KeyPress(var Key: Char); override;
    procedure PropertiesChanged(Sender: TObject); override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure PasteFromClipboard; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue;
      out EditValue: TcxEditValue; AEditFocused: Boolean); override;
    property ActiveProperties: TcxCustomCurrencyEditProperties read GetActiveProperties;
    property Properties: TcxCustomCurrencyEditProperties read GetProperties
      write SetProperties;
    property Value: Double read GetValue write SetValue stored False;
  end;

  { TcxCurrencyEdit }

  TcxCurrencyEdit = class(TcxCustomCurrencyEdit)
  private
    function GetActiveProperties: TcxCurrencyEditProperties;
    function GetProperties: TcxCurrencyEditProperties;
    procedure SetProperties(Value: TcxCurrencyEditProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCurrencyEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditValue;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxCurrencyEditProperties read GetProperties
      write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property TextHint;
    property Value;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property BiDiMode;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

  { TcxFilterCurrencyEditHelper }

  TcxFilterCurrencyEditHelper = class(TcxFilterTextEditHelper)
  public
    class function GetFilterEditClass: TcxCustomEditClass; override;
    class function GetSupportedFilterOperators(
      AProperties: TcxCustomEditProperties;
      AValueTypeClass: TcxValueTypeClass;
      AExtendedSet: Boolean = False): TcxFilterControlOperators; override;
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
  end;

implementation

uses
  Variants, cxEditConsts, cxFormats, cxVariants, cxControls, cxClasses, dxCore, StrUtils;

{ TcxCurrencyEditPropertiesValues }

procedure TcxCurrencyEditPropertiesValues.Assign(Source: TPersistent);
begin
  if Source is TcxCurrencyEditPropertiesValues then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      DecimalPlaces := TcxCurrencyEditPropertiesValues(Source).DecimalPlaces;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxCurrencyEditPropertiesValues.RestoreDefaults;
begin
  BeginUpdate;
  try
    inherited RestoreDefaults;
    DecimalPlaces := False;
  finally
    EndUpdate;
  end;
end;

procedure TcxCurrencyEditPropertiesValues.SetDecimalPlaces(Value: Boolean);
begin
  if Value <> FDecimalPlaces then
  begin
    FDecimalPlaces := Value;
    Changed;
  end;
end;

{ TcxCustomCurrencyEditProperties }

constructor TcxCustomCurrencyEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  Nullable := True;
  ValidateOnEnter := True;
end;

function TcxCustomCurrencyEditProperties.GetAssignedValues: TcxCurrencyEditPropertiesValues;
begin
  Result := TcxCurrencyEditPropertiesValues(FAssignedValues);
end;

function TcxCustomCurrencyEditProperties.GetDecimalPlaces: Integer;
begin
  if AssignedValues.DecimalPlaces then
    Result := FDecimalPlaces
  else
    Result := dxFormatSettings.CurrencyDecimals;
end;

function TcxCustomCurrencyEditProperties.IsDecimalPlacesStored: Boolean;
begin
  Result := AssignedValues.DecimalPlaces;
end;

procedure TcxCustomCurrencyEditProperties.SetAssignedValues(
  Value: TcxCurrencyEditPropertiesValues);
begin
  FAssignedValues.Assign(Value);
end;

procedure TcxCustomCurrencyEditProperties.SetDecimalPlaces(Value: Integer);
begin
  if AssignedValues.DecimalPlaces and (Value = FDecimalPlaces) then
    Exit;
  AssignedValues.FDecimalPlaces := True;
  FDecimalPlaces := Value;
  Changed;
end;

procedure TcxCustomCurrencyEditProperties.SetNullable(Value: Boolean);
begin
  if FNullable <> Value then
  begin
    FNullable := Value;
    Changed;
  end;
end;

procedure TcxCustomCurrencyEditProperties.SetUseThousandSeparator(
  const Value: Boolean);
begin
  if FUseThousandSeparator <> Value then
  begin
    FUseThousandSeparator := Value;
    Changed;
  end;
end;

procedure TcxCustomCurrencyEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomCurrencyEditProperties then
    with TcxCustomCurrencyEditProperties(AProperties) do
    begin
      Self.AssignedValues.DecimalPlaces := False;
      if AssignedValues.DecimalPlaces then
        Self.DecimalPlaces := DecimalPlaces;
      Self.Nullable := Nullable;
      Self.UseThousandSeparator := UseThousandSeparator;
    end;
end;

class function TcxCustomCurrencyEditProperties.GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass;
begin
  Result := TcxCurrencyEditPropertiesValues;
end;

function TcxCustomCurrencyEditProperties.GetDefaultDisplayFormat: string;
begin
  Result := cxFormatController.CurrencyFormat;
end;

function TcxCustomCurrencyEditProperties.GetDisplayFormatOptions: TcxEditDisplayFormatOptions;
begin
  Result := [dfoSupports, dfoNoCurrencyValue];
end;

function TcxCustomCurrencyEditProperties.HasDigitGrouping(
  AIsDisplayValueSynchronizing: Boolean): Boolean;
begin
  Result := not AIsDisplayValueSynchronizing and UseThousandSeparator;
end;

function TcxCustomCurrencyEditProperties.IsEditValueNumeric: Boolean;
begin
  Result := True;
end;

class function TcxCustomCurrencyEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxCurrencyEdit;
end;

function TcxCustomCurrencyEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  Result := evsValue;
  if not AEditFocused and (IDefaultValuesProvider <> nil) and
    IDefaultValuesProvider.IsOnGetTextAssigned then
      Result := evsText;
end;

function TcxCustomCurrencyEditProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := inherited GetSupportedOperations;
  Exclude(Result, esoEditingAutoHeight);
end;

function TcxCustomCurrencyEditProperties.IsDisplayValueValid(
  var ADisplayValue: TcxEditValue; AEditFocused: Boolean): Boolean;
var
  C, AIsOnGetTextAssigned: Boolean;
  D: Double;
  S: string;
begin
  Result := not AEditFocused or (InternalGetEditFormat(C, AIsOnGetTextAssigned) <> '') or AIsOnGetTextAssigned;
  if not Result then
  begin
    S := Trim(VarToStr(ADisplayValue));
    Result := StrToFloatEx(S, D);
    if Result then
      ADisplayValue := S;
  end;
end;

procedure TcxCustomCurrencyEditProperties.DoPrepareDisplayValue(
  const AEditValue: TcxEditValue; var ADisplayValue: TcxEditValue;
  AEditFocused: Boolean);
var
  AValue: TcxEditValue;
begin
  AValue := AEditValue;
  if VarIsSoftNull(AValue) and not AEditFocused and not Nullable then
    AValue := 0;
  try
    inherited DoPrepareDisplayValue(AValue, ADisplayValue, AEditFocused);
  except
    on EConvertError do
      if AEditFocused then
        ADisplayValue := ''
      else
        ADisplayValue := AEditValue;
    on EVariantError do
      if AEditFocused then
        ADisplayValue := ''
      else
        ADisplayValue := AEditValue;
  end;
end;

procedure TcxCustomCurrencyEditProperties.ValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean; AEdit: TcxCustomEdit);
var
  AValue: Double;
begin
  AError := not StrToFloatEx(VarToStr(ADisplayValue), AValue);
  inherited ValidateDisplayValue(ADisplayValue, AErrorText, AError, AEdit);
end;

{ TcxCustomCurrencyEdit }

function TcxCustomCurrencyEdit.GetProperties: TcxCustomCurrencyEditProperties;
begin
  Result := TcxCustomCurrencyEditProperties(inherited Properties);
end;

function TcxCustomCurrencyEdit.GetActiveProperties: TcxCustomCurrencyEditProperties;
begin
  Result := TcxCustomCurrencyEditProperties(InternalGetActiveProperties);
end;

function TcxCustomCurrencyEdit.GetValue: Double;
var
  V: Variant;
begin
  if Focused and not IsEditValidated and ModifiedAfterEnter then
  begin
    V := InternalGetEditingValue;
    if VarIsNumericEx(V) then
      Result := V
    else
      Result := 0.0;
  end
  else
    if VarIsNull(EditValue) or (VarIsStr(EditValue) and (StrToCurrDef(EditValue, 0) = 0)) then
      Result := 0.00
    else
      Result := EditValue;
end;

procedure TcxCustomCurrencyEdit.SetProperties(
  Value: TcxCustomCurrencyEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomCurrencyEdit.SetValue(Value: Double);
begin
  with ActiveProperties do
    if IsValueBoundDefined(evbMin) and (Value < MinValue) then
      Value := MinValue
    else
      if IsValueBoundDefined(evbMax) and (Value > MaxValue) then
        Value := MaxValue;
  InternalEditValue := Value;
end;

procedure TcxCustomCurrencyEdit.CheckEditorValueBounds;
begin
  BeginUserAction;
  try
    if ActiveProperties.IsValueBoundDefined(evbMin) and (Value < ActiveProperties.MinValue) then
      Value := ActiveProperties.MinValue
    else
      if ActiveProperties.IsValueBoundDefined(evbMax) and (Value > ActiveProperties.MaxValue) then
        Value := ActiveProperties.MaxValue;
  finally
    EndUserAction;
  end;
end;

procedure TcxCustomCurrencyEdit.Initialize;
begin
  inherited Initialize;
  ControlStyle := ControlStyle - [csSetCaption];
end;

function TcxCustomCurrencyEdit.InternalGetEditingValue: TcxEditValue;
begin
  PrepareEditValue(Text, Result, True);
end;

function TcxCustomCurrencyEdit.IsValidChar(Key: Char): Boolean;

  function IsValidNumber(const S: string): Boolean;
  var
    ADecPos, AStartPos: Integer;
    V: Double;
  begin
    Result := False;
    ADecPos := Pos(dxFormatSettings.DecimalSeparator, S);
    if ADecPos > 0 then
    begin
      AStartPos := Pos('E', UpperCase(S));
      if AStartPos > ADecPos then
        ADecPos := AStartPos - ADecPos - 1
      else
        ADecPos := Length(S) - ADecPos;
      if ADecPos > ActiveProperties.DecimalPlaces then
        Exit;
    end;
    Result := StrToFloatEx(S, V);
  end;

var
  AEndPos, AStartPos: Integer;
  AIsCurrency, AIsOnGetTextAssigned: Boolean;
  S: string;
begin
  Result := False;
  if not IsNumericChar(Key, ntExponent) and
    not (ActiveProperties.UseThousandSeparator and (Key = dxFormatSettings.ThousandSeparator)) then
      Exit;
  if ((ActiveProperties.InternalGetEditFormat(AIsCurrency, AIsOnGetTextAssigned, Self) <> '') or
    AIsOnGetTextAssigned) and not IsValidNumber(Text) then
  begin
    Result := True;
    Exit;
  end;
  S := Text;
  AStartPos := SelStart;
  AEndPos := SelStart + SelLength;
  Delete(S, SelStart + 1, AEndPos - AStartPos);
  if (Key = '-') and (S = '') then
  begin
    Result := True;
    Exit;
  end;
  Insert(Key, S, AStartPos + 1);
  Result := IsValidNumber(S);
end;

procedure TcxCustomCurrencyEdit.KeyPress(var Key: Char);
begin
  if not (ActiveProperties.UseThousandSeparator and (Key = dxFormatSettings.ThousandSeparator)) and
    dxCharInSet(Key, ['.', ',']) then
    Key := dxFormatSettings.DecimalSeparator;
  inherited KeyPress(Key);
end;

procedure TcxCustomCurrencyEdit.PropertiesChanged(Sender: TObject);
begin
  if (Sender <> nil) and ActiveProperties.FFormatChanging then
    Exit;
  if not Focused then
    DataBinding.UpdateDisplayValue;
  inherited PropertiesChanged(Sender);
end;

class function TcxCustomCurrencyEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomCurrencyEditProperties;
end;

procedure TcxCustomCurrencyEdit.PasteFromClipboard;
var
  AEditFormat: string;
begin
  if CanModify then
  begin
    inherited;
    if not VarIsNull(EditingValue) then
    begin
      AEditFormat := '0.' + DupeString('0', ActiveProperties.DecimalPlaces) + ';';
      EditingText := FormatFloat(AEditFormat, EditingValue);
    end;
  end;
end;

procedure TcxCustomCurrencyEdit.PrepareEditValue(
  const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue;
  AEditFocused: Boolean);
var
  V: Double;
begin
  if (ADisplayValue = '') or not StrToFloatEx(ADisplayValue, V) then
    if ActiveProperties.Nullable then
      EditValue := Null
    else
      EditValue := 0.00
  else
    EditValue := V
end;

{ TcxCurrencyEdit }

class function TcxCurrencyEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCurrencyEditProperties;
end;

function TcxCurrencyEdit.GetActiveProperties: TcxCurrencyEditProperties;
begin
  Result := TcxCurrencyEditProperties(InternalGetActiveProperties);
end;

function TcxCurrencyEdit.GetProperties: TcxCurrencyEditProperties;
begin
  Result := TcxCurrencyEditProperties(inherited Properties);
end;

procedure TcxCurrencyEdit.SetProperties(Value: TcxCurrencyEditProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterCurrencyEditHelper }

class function TcxFilterCurrencyEditHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := TcxCurrencyEdit;
end;

class function TcxFilterCurrencyEditHelper.GetSupportedFilterOperators(
  AProperties: TcxCustomEditProperties;
  AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  Result := [fcoEqual, fcoNotEqual, fcoLess, fcoLessEqual, fcoGreater,
    fcoGreaterEqual, fcoBlanks, fcoNonBlanks];
  if AExtendedSet then
    Result := Result + [fcoBetween, fcoNotBetween, fcoInList, fcoNotInList];
end;

class procedure TcxFilterCurrencyEditHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  with TcxCustomCurrencyEditProperties(AProperties) do
  begin
    MinValue := 0;
    MaxValue := 0;
    Nullable := True;
    Nullstring := '';
    UseDisplayFormatWhenEditing := False;
  end;
end;

initialization
  GetRegisteredEditProperties.Register(TcxCurrencyEditProperties, scxSEditRepositoryCurrencyItem);
  FilterEditsController.Register(TcxCurrencyEditProperties, TcxFilterCurrencyEditHelper);

finalization
  FilterEditsController.Unregister(TcxCurrencyEditProperties, TcxFilterCurrencyEditHelper);

end.
