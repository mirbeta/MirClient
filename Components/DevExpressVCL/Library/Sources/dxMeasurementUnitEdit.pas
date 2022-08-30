{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxMeasurementUnitEdit;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, SysUtils, Controls, Variants, dxCore, cxSpinEdit, cxEdit;

type

  TdxMeasurementUnitEditIncrementValueEvent = procedure(Sender: TObject; AButton: TcxSpinEditButton;
    var AValue: Variant; var AHandled: Boolean) of object;

  { TdxMeasurementUnitEditProperties }

  TdxMeasurementUnitEditProperties = class(TcxCustomSpinEditProperties)
  private
    FOnIncrementValue: TdxMeasurementUnitEditIncrementValueEvent;
  protected
    function CheckValueBounds(const Value: Variant): Variant; override;
    function DoGetNewValue(Sender: TObject; AButton: TcxSpinEditButton; var AValue: Variant): Boolean; virtual;
    function IsDisplayValueNumeric: Boolean; override;
    function PrepareValue(const AValue: Variant): Variant; override;
  public
    function IsEditValueValid(var AEditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    procedure ValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean;
      AEdit: TcxCustomEdit); override;
  published
    property ValidateOnEnter;
    property ValidationOptions;

    property OnChange;
    property OnIncrementValue: TdxMeasurementUnitEditIncrementValueEvent read FOnIncrementValue write FOnIncrementValue;
    property OnEditValueChanged;
    property OnValidate;
  end;

  { TdxMeasurementUnitEdit }

  TdxMeasurementUnitEdit = class(TcxCustomSpinEdit)
  private
    function GetActiveProperties: TdxMeasurementUnitEditProperties;
    function GetProperties: TdxMeasurementUnitEditProperties;
    procedure SetProperties(const Value: TdxMeasurementUnitEditProperties);
  protected
    procedure CheckEditorValueBounds; override;
    function GetNewValue(AButton: TcxSpinEditButton; out AValue: Variant): Boolean; override;
    function GetValue: Variant; override;
    function IsValidChar(AChar: Char): Boolean; override;
    procedure SetValue(const Value: Variant); override;
    procedure Initialize; override;
    property Value;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue; AEditFocused: Boolean); override;
    property ActiveProperties: TdxMeasurementUnitEditProperties read GetActiveProperties;
  published
    property Properties: TdxMeasurementUnitEditProperties read GetProperties write SetProperties;

    property ParentColor;
    property TabOrder;
  end;

  { TdxMeasurementUnitEditHelper }

  TdxMeasurementUnitEditHelper = class
  private
    FDescription: string;
    FMaxPrecision: Integer;
    FMinValue: Currency;
    FMaxValue: Currency;
    FIncrement: Currency;
    function IsValidValue(AValue: Extended): Boolean;
    function TryGetValue(AText: string; out AValue: Currency): Boolean;
  public
    constructor Create(const ADescription: string; AIncrement: Currency; AMaxPrecision: Integer;
      AMinValue, AMaxValue: Currency);
    function IncrementValue(AButton: TcxSpinEditButton; var AText: Variant): Boolean;
    function GetValueFromText(const AText: string; ACorrectRange: Boolean = True): Variant;
    function GetTextFromValue(const AValue: Variant): string;
    function CorrectRange(const AValue: Variant): Variant;

    property Description: string read FDescription write FDescription;
    property MaxPrecision: Integer read FMaxPrecision write FMaxPrecision;
    property MaxValue: Currency read FMaxValue write FMaxValue;
    property MinValue: Currency read FMinValue write FMinValue;
    property Increment: Currency read FIncrement write FIncrement;
  end;

implementation

uses
  Math, Windows;

{ TdxMeasurementUnitEditProperties }

function TdxMeasurementUnitEditProperties.IsEditValueValid(var AEditValue: TcxEditValue; AEditFocused: Boolean): Boolean;
begin
  Result := True;
end;

procedure TdxMeasurementUnitEditProperties.ValidateDisplayValue(var ADisplayValue: TcxEditValue;
  var AErrorText: TCaption; var AError: Boolean; AEdit: TcxCustomEdit);
var
  AIsUserErrorDisplayValue: Boolean;
begin
  DoValidate(ADisplayValue, AErrorText, AError, AEdit, AIsUserErrorDisplayValue);
end;

function TdxMeasurementUnitEditProperties.CheckValueBounds(const Value: Variant): Variant;
begin
  Result := Value;
end;

function TdxMeasurementUnitEditProperties.DoGetNewValue(Sender: TObject;
  AButton: TcxSpinEditButton; var AValue: Variant): Boolean;
begin
  Result := False;
  if Assigned(FOnIncrementValue) then
    FOnIncrementValue(Sender, AButton, AValue, Result);
end;

function TdxMeasurementUnitEditProperties.IsDisplayValueNumeric: Boolean;
begin
  Result := False;
end;

function TdxMeasurementUnitEditProperties.PrepareValue(const AValue: Variant): Variant;
begin
  Result := AValue;
end;

{ TdxMeasurementUnitEdit }

class function TdxMeasurementUnitEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxMeasurementUnitEditProperties;
end;

procedure TdxMeasurementUnitEdit.PrepareEditValue(const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue;
  AEditFocused: Boolean);
begin
  EditValue := ADisplayValue;
end;

procedure TdxMeasurementUnitEdit.CheckEditorValueBounds;
begin
end;

function TdxMeasurementUnitEdit.GetNewValue(AButton: TcxSpinEditButton; out AValue: Variant): Boolean;
begin
  AValue := DisplayValue;
  Result := ActiveProperties.DoGetNewValue(Self, AButton, AValue)
end;

function TdxMeasurementUnitEdit.GetValue: Variant;
begin
  Result := InternalEditValue;
end;

procedure TdxMeasurementUnitEdit.Initialize;
begin
  inherited Initialize;
  InternalEditValue := Null;
end;

function TdxMeasurementUnitEdit.IsValidChar(AChar: Char): Boolean;
begin
  Result := (Ord(AChar) >= 32) or (AChar = Char(#8));
end;

procedure TdxMeasurementUnitEdit.SetValue(const Value: Variant);
begin
  InternalEditValue := Value;
end;

function TdxMeasurementUnitEdit.GetActiveProperties: TdxMeasurementUnitEditProperties;
begin
  Result := TdxMeasurementUnitEditProperties(inherited ActiveProperties);
end;

function TdxMeasurementUnitEdit.GetProperties: TdxMeasurementUnitEditProperties;
begin
  Result := TdxMeasurementUnitEditProperties(inherited Properties);
end;

procedure TdxMeasurementUnitEdit.SetProperties(const Value: TdxMeasurementUnitEditProperties);
begin
  inherited Properties := Value;
end;

{ TdxMeasurementUnitEditHelper }

function TdxMeasurementUnitEditHelper.CorrectRange(const AValue: Variant): Variant;
begin
  Result := AValue;
  if not VarIsNull(AValue) then
    if AValue > MaxValue then
      Result := MaxValue
    else
      if AValue < MinValue then
        Result := MinValue;
end;

constructor TdxMeasurementUnitEditHelper.Create(const ADescription: string; AIncrement: Currency;
  AMaxPrecision: Integer; AMinValue, AMaxValue: Currency);
begin
  inherited Create;
  FDescription := ADescription;
  FIncrement := AIncrement;
  FMaxPrecision := AMaxPrecision;
  FMaxValue := AMaxValue;
  FMinValue := AMinValue;
end;

function TdxMeasurementUnitEditHelper.GetTextFromValue(const AValue: Variant): string;

  function GetFormatString: string;
  var
    I: Integer;
    ADecimalFormat: string;
  begin
    ADecimalFormat := '';
    for I := 0 to MaxPrecision - 1 do
      if ADecimalFormat = '' then
        ADecimalFormat := '.#'
      else
        ADecimalFormat := ADecimalFormat + '#';
    Result := '0' + ADecimalFormat;
  end;

begin
  Result := '';
  if not VarIsNull(AValue) then
    if Description = '' then
      Result := FormatFloat(GetFormatString, AValue)
    else
      Result := FormatFloat(GetFormatString, AValue) + Description;
end;

function TdxMeasurementUnitEditHelper.GetValueFromText(const AText: string; ACorrectRange: Boolean = True): Variant;
var
  AValue: Currency;
begin
  if TryGetValue(AText, AValue) and (not ACorrectRange or (CorrectRange(AValue) = AValue)) then
    Result := AValue
  else
    Result := Null;
end;

function TdxMeasurementUnitEditHelper.IncrementValue(AButton: TcxSpinEditButton; var AText: Variant): Boolean;

  function GetIncrement(const AValue: Currency): Currency;
  var
    AIncrement: Currency;
  begin
    if AButton = sebBackward then
      AIncrement := -Increment
    else
        AIncrement := Increment;
    Result := RoundTo(Trunc((AValue + AIncrement) / Increment) * Increment - AValue, -MaxPrecision);
    if (Abs(Result) > Increment) and not SameValue(Abs(Result), Increment) then
      Result := RoundTo(Result - AIncrement, -MaxPrecision);
  end;

var
  AValue: Variant;
  ANewValue: Currency;
begin
  if Increment = 0 then
    Exit(True);
  AValue := GetValueFromText(VarToStr(AText));
  if VarIsNull(AValue) then
    AValue := CorrectRange(0);
  ANewValue := AValue + GetIncrement(AValue);
  if ANewValue > MaxValue then
    ANewValue := MaxValue
  else
    if ANewValue < MinValue then
      ANewValue := MinValue;
  AText := GetTextFromValue(ANewValue);
  Result := True;
end;

function TdxMeasurementUnitEditHelper.IsValidValue(AValue: Extended): Boolean;
var
  AMinValue, AMaxValue: Extended;
begin
  AMinValue := MinCurrency;
  AMaxValue := MaxCurrency;
  Result := (AValue > AMinValue) and (AValue < AMaxValue);
end;

function TdxMeasurementUnitEditHelper.TryGetValue(AText: string; out AValue: Currency): Boolean;

  procedure PrepareValue;
  var
    I: Integer;
  begin
    AText := Trim(AText);
    for I := 1 to Length(AText) do
      if not CharInSet(AText[I], ['0'..'9']) and not CharInSet(AText[I], [FormatSettings.DecimalSeparator, '-']) then
      begin
        Insert(' ', AText, I);
        Exit;
      end;
  end;

var
  ASpaceAt: Integer;
  AType: string;
  E: Extended;
begin
  PrepareValue;
  ASpaceAt := Pos(' ', AText);
  if ASpaceAt > 0 then
  begin
    Result := TryStrToFloat(Copy(AText, 1, ASpaceAt - 1), E) and IsValidValue(E);
    if Result then
    begin
      AType := Trim(Copy(AText, ASpaceAt + 1, MaxInt));
      Result := (AType <> '') and AnsiSameText(AType, Trim(Description));
    end;
  end
  else
    Result := TryStrToFloat(AText, E) and IsValidValue(E);
  if Result then
  begin
    AValue := E;
    RoundTo(AValue, -MaxPrecision);
  end;
end;

end.
