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

unit dxRichEdit.Utils.DXUnit;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, SysUtils;

type

  { TdxUnitType }

  TdxUnitType = (
    Pixel = 1,
    Point = 2,
    Pica = 3,
    Inch = 4,
    Mm = 5,
    Cm = 6,
    Percentage = 7,
    Em = 8,
    Ex = 9,
    Deg = 10,
    Fd = 11,
    Emu = 12
  );

  { TdxUnitBase }

  TdxUnitBase = class abstract
  strict private
    FValue: Single;
    FType: TdxUnitType;
  protected
    function Parse(const AValue: string; AMinValue: Single; AMaxValue: Single): TdxUnitBase; virtual; abstract;
    function ObtainValue(AValue: Single; AType: TdxUnitType): Single;
    procedure ValidateValueRange(AValue: Single; AMinValue: Single; AMaxValue: Single);
  public
    constructor Create(AValue: Single; AType: TdxUnitType); overload;
    constructor Create(AValue: Single; AType: TdxUnitType; AMinValue: Single; AMaxValue: Single); overload;
    constructor Create(AValue: Integer; AType: TdxUnitType; AMinValue: Single; AMaxValue: Single); overload;
    constructor Create(const AValue: string; AMinValue: Single; AMaxValue: Single); overload;

    property Value: Single read FValue;
    property &Type: TdxUnitType read FType;
  end;

  { TdxUnit }

  TdxUnit = class(TdxUnitBase)
  protected
    function Parse(const AValue: string; AMinValue: Single; AMaxValue: Single): TdxUnitBase; override;
  public
    constructor Create; overload;
    constructor Create(AValue: Single); overload;
    constructor Create(AValue: Integer); overload;
    constructor Create(AValue: Integer; AMinValue: Single; AMaxValue: Single); overload;
    constructor Create(AValue: Single; AType: TdxUnitType); overload;
    constructor Create(AValue: Single; AType: TdxUnitType; AMinValue: Single; AMaxValue: Single); overload;
    constructor Create(const AValue: string); overload;
  end;

  { TdxRotationUnit }

  TdxRotationUnit = class(TdxUnitBase)
  protected
    function Parse(const AValue: string; AMinValue: Single; AMaxValue: Single): TdxUnitBase; override;
  public
    constructor Create; overload;
    constructor Create(const AValue: string); overload;
    constructor Create(AValue: Single; AType: TdxUnitType; AMinValue: Single; AMaxValue: Single); overload;
  end;

  { TdxVmlUnit }

  TdxVmlUnit = class(TdxUnitBase)
  protected
    function Parse(const AValue: string; AMinValue: Single; AMaxValue: Single): TdxUnitBase; override;
  public
    constructor Create; overload;
    constructor Create(const AValue: string); overload;
    constructor Create(AValue: Single; AType: TdxUnitType); overload;
    constructor Create(AValue: Single; AType: TdxUnitType; AMinValue: Single; AMaxValue: Single); overload;
    function ToString: string; override;
    function GetSuffix: string;
  end;

  { TdxStringUnitValueParser }

  TdxStringUnitValueParser = class
  strict private
    class function GetIndex(const AValue: string; const AStrings: array of string): Integer; static;
  public
    function GetUnit(const AInputValue: string): TdxUnit; overload;
    function GetUnit(const AInputValue: string; AMinValue, AMaxValue: Single): TdxUnit; overload;
    function GetRotationUnitType(const AInputValue: string; AMinValue, AMaxValue: Single): TdxRotationUnit;
    function GetVmlUnitType(const AInputValue: string; AMinValue, AMaxValue: Single): TdxVmlUnit;
    class function GetTypeFromString(const AValue: string): TdxUnitType; static;
    class function GetRotationUnitTypeFromString(const AValue: string): TdxUnitType; static;
    class function GetVmlUnitTypeFromString(const AValue: string): TdxUnitType; static;
  end;

  { TdxValueInfo }

  TdxValueInfo = record
  private
    FUnit: string;
    FValue: Single;
    FIsValidNumber: Boolean;
    class function GetEmpty: TdxValueInfo; static;
    function GetIsEmpty: Boolean;
    procedure Clear;
  public
    constructor Create(const AUnit: string); overload;
    constructor Create(AValue: Single; const AUnit: string); overload;

    class operator Equal(const A, B: TdxValueInfo): Boolean;
    class property Empty: TdxValueInfo read GetEmpty;
    property &Unit: string read FUnit;
    property Value: Single read FValue;
    property IsValidNumber: Boolean read FIsValidNumber;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  { TdxStringValueParser }

  TdxStringValueParser = class
  strict private
    type
      TSplitResult = record
      public
        Value: string;
        &Unit: string;
        constructor Create(const AValue: string; const AUnit: string);
      end;
  public
    class function TryParse(const AInputString: string): TdxValueInfo; static;
    class function SplitUnitFromValue(const AInputString: string): TSplitResult; static;
    class function Parse(ASplitResult: TSplitResult): TdxValueInfo; static;
  end;

implementation

uses
  dxCore, dxCultureInfo,
  dxRichEdit.Utils.Exceptions,
  dxStringHelper,
  dxRichEdit.Utils.NumberParser;

{ TdxUnitBase }

constructor TdxUnitBase.Create(const AValue: string; AMinValue: Single; AMaxValue: Single);
var
  AUnit: TdxUnitBase;
begin
  inherited Create;
  AUnit := Parse(AValue, AMinValue, AMaxValue);
  if AUnit <> nil then
  begin
    FValue := AUnit.Value;
    FType := AUnit.&Type;
    AUnit.Free;
  end;
end;

constructor TdxUnitBase.Create(AValue: Integer; AType: TdxUnitType; AMinValue: Single; AMaxValue: Single);
begin
  inherited Create;
  ValidateValueRange(AValue, AMinValue, AMaxValue);
  FValue := AValue;
  FType := AType;
end;

constructor TdxUnitBase.Create(AValue: Single; AType: TdxUnitType; AMinValue: Single; AMaxValue: Single);
begin
  inherited Create;
  ValidateValueRange(AValue, AMinValue, AMaxValue);
  FValue := ObtainValue(AValue, AType);
  FType := AType;
end;

constructor TdxUnitBase.Create(AValue: Single; AType: TdxUnitType);
begin
  inherited Create;
  FValue := ObtainValue(AValue, AType);
  FType := AType;
end;

function TdxUnitBase.ObtainValue(AValue: Single; AType: TdxUnitType): Single;
begin
  if (AType = TdxUnitType.Pixel) or (AType = TdxUnitType.Emu) then
    AValue := Int(AValue);
  Result := AValue;
end;

procedure TdxUnitBase.ValidateValueRange(AValue: Single; AMinValue: Single; AMaxValue: Single);
begin
  if (AValue < AMinValue) or (AValue > AMaxValue) then
    TdxRichEditExceptions.ThrowInternalException;
end;

{ TdxUnit }

constructor TdxUnit.Create(const AValue: string);
begin
  inherited Create(AValue, -32768.0, 32767.0);
end;

constructor TdxUnit.Create(AValue: Single; AType: TdxUnitType; AMinValue: Single; AMaxValue: Single);
begin
  inherited Create(AValue, AType, AMinValue, AMaxValue);
end;

constructor TdxUnit.Create(AValue: Single; AType: TdxUnitType);
begin
  inherited Create(AValue, AType, -32768.0, 32767.0);
end;

constructor TdxUnit.Create(AValue: Integer; AMinValue: Single; AMaxValue: Single);
begin
  inherited Create(AValue, TdxUnitType.Pixel, AMinValue, AMaxValue);
end;

constructor TdxUnit.Create(AValue: Integer);
begin
  Create(AValue, -32768, $7FF);
end;

constructor TdxUnit.Create(AValue: Single);
begin
  inherited Create(AValue, TdxUnitType.Pixel, -32768.0, 32767.0);
end;

constructor TdxUnit.Create;
begin
  inherited Create(0, TdxUnitType.Pixel);
end;

function TdxUnit.Parse(const AValue: string; AMinValue: Single; AMaxValue: Single): TdxUnitBase;
var
  AParser: TdxStringUnitValueParser;
begin
  AParser := TdxStringUnitValueParser.Create;
  try
    Result := AParser.GetUnit(AValue, AMinValue, AMaxValue);
  finally
    AParser.Free;
  end;
end;

{ TdxRotationUnit }

constructor TdxRotationUnit.Create(AValue: Single; AType: TdxUnitType; AMinValue: Single; AMaxValue: Single);
begin
  inherited Create(AValue, AType, AMinValue, AMaxValue);
end;

constructor TdxRotationUnit.Create(const AValue: string);
begin
  inherited Create(AValue, MinInt, MaxInt);
end;

constructor TdxRotationUnit.Create;
begin
  inherited Create(0, TdxUnitType.Deg);
end;

function TdxRotationUnit.Parse(const AValue: string; AMinValue: Single; AMaxValue: Single): TdxUnitBase;
var
  AParser: TdxStringUnitValueParser;
begin
  AParser := TdxStringUnitValueParser.Create;
  try
    Result := AParser.GetRotationUnitType(AValue, AMinValue, AMaxValue);
  finally
    AParser.Free;
  end;
end;

{ TdxVmlUnit }

constructor TdxVmlUnit.Create(AValue: Single; AType: TdxUnitType; AMinValue: Single; AMaxValue: Single);
begin
  inherited Create(AValue, AType, AMinValue, AMaxValue);
end;

constructor TdxVmlUnit.Create(AValue: Single; AType: TdxUnitType);
begin
  inherited Create(AValue, AType, MinInt, MaxInt);
end;

constructor TdxVmlUnit.Create(const AValue: string);
begin
  inherited Create(AValue, MinInt, MaxInt);
end;

constructor TdxVmlUnit.Create;
begin
  inherited Create(0, TdxUnitType.Emu);
end;

function TdxVmlUnit.Parse(const AValue: string; AMinValue: Single; AMaxValue: Single): TdxUnitBase;
var
  AParser: TdxStringUnitValueParser;
begin
  AParser := TdxStringUnitValueParser.Create;
  try
    Result := AParser.GetVmlUnitType(AValue, AMinValue, AMaxValue);
  finally
    AParser.Free;
  end;
end;

function TdxVmlUnit.ToString: string;
var
  ASb: TStringBuilder;
begin
  ASb := TStringBuilder.Create;
  if (&Type = TdxUnitType.Emu) or (&Type = TdxUnitType.Pixel) then
    ASb.Append(Trunc(Value))
  else
    ASb.Append(FloatToStr(Value));
  ASb.Append(GetSuffix);
  Result := ASb.ToString;
end;

function TdxVmlUnit.GetSuffix: string;
begin
  case &Type of
    TdxUnitType.Cm:
      Result := 'cm';
    TdxUnitType.Mm:
      Result := 'mm';
    TdxUnitType.Inch:
      Result := 'in';
    TdxUnitType.Point:
      Result := 'pt';
    TdxUnitType.Pica:
      Result := 'pc';
    TdxUnitType.Pixel:
      Result := 'px';
    else
      Result := '';
  end;
end;

{ TdxStringUnitValueParser }

function TdxStringUnitValueParser.GetUnit(const AInputValue: string): TdxUnit;
begin
  Result := GetUnit(AInputValue, -32768.0, 32767.0);
end;

function TdxStringUnitValueParser.GetUnit(const AInputValue: string; AMinValue, AMaxValue: Single): TdxUnit;
var
  AValueInfo: TdxValueInfo;
begin
  AValueInfo := TdxStringValueParser.TryParse(AInputValue);
  if AValueInfo.IsValidNumber then
    Result := TdxUnit.Create(AValueInfo.Value, GetTypeFromString(AValueInfo.&Unit), AMinValue, AMaxValue)
  else
  begin
    Result := TdxUnit.Create;
  end;
end;

class function TdxStringUnitValueParser.GetIndex(const AValue: string; const AStrings: array of string): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(AStrings) - 1 do
    if AStrings[I] = AValue then
      Exit(I);
  Result := -1;
end;

function TdxStringUnitValueParser.GetRotationUnitType(const AInputValue: string; AMinValue, AMaxValue: Single): TdxRotationUnit;
var
  AValueInfo: TdxValueInfo;
begin
  AValueInfo := TdxStringValueParser.TryParse(AInputValue);
  if AValueInfo.IsValidNumber then
    Result := TdxRotationUnit.Create(AValueInfo.Value, GetRotationUnitTypeFromString(AValueInfo.&Unit), AMinValue, AMaxValue)
  else
  begin
    Result := TdxRotationUnit.Create;
  end;
end;

function TdxStringUnitValueParser.GetVmlUnitType(const AInputValue: string; AMinValue, AMaxValue: Single): TdxVmlUnit;
var
  AValueInfo: TdxValueInfo;
begin
  AValueInfo := TdxStringValueParser.TryParse(AInputValue);
  if AValueInfo.IsValidNumber then
    Result := TdxVmlUnit.Create(AValueInfo.Value, GetVmlUnitTypeFromString(AValueInfo.&Unit), AMinValue, AMaxValue)
  else
  begin
    Result := TdxVmlUnit.Create;
  end;
end;

class function TdxStringUnitValueParser.GetTypeFromString(const AValue: string): TdxUnitType;
begin
  if AValue = '' then
    Exit(TdxUnitType.Pixel);
  case GetIndex(UpperCase(AValue), ['PX', 'PT', '%', 'PC', 'IN', 'MM', 'CM', 'EM', 'EX']) of
    0:
      Result := TdxUnitType.Pixel;
    1:
      Result := TdxUnitType.Point;
    2:
      Result := TdxUnitType.Percentage;
    3:
      Result := TdxUnitType.Pica;
    4:
      Result := TdxUnitType.Inch;
    5:
      Result := TdxUnitType.Mm;
    6:
      Result := TdxUnitType.Cm;
    7:
      Result := TdxUnitType.Em;
    8:
      Result := TdxUnitType.Ex;
    else
    begin
      TdxRichEditExceptions.ThrowArgumentException('UnitType', AValue);
      Result := TdxUnitType.Pixel;
    end;
  end;
end;

class function TdxStringUnitValueParser.GetRotationUnitTypeFromString(const AValue: string): TdxUnitType;
begin
  if AValue = '' then
    Exit(TdxUnitType.Deg);
  if UpperCase(AValue) = 'FD' then
    Result := TdxUnitType.Fd
  else
  begin
    TdxRichEditExceptions.ThrowArgumentException('UnitType', AValue);
    Result := TdxUnitType.Deg;
  end;
end;

class function TdxStringUnitValueParser.GetVmlUnitTypeFromString(const AValue: string): TdxUnitType;
begin
  if AValue = '' then
    Exit(TdxUnitType.Emu);
  case GetIndex(UpperCase(AValue), ['PX', 'PT', 'PC', 'IN', 'MM', 'CM']) of
    0:
      Result := TdxUnitType.Pixel;
    1:
      Result := TdxUnitType.Point;
    2:
      Result := TdxUnitType.Pica;
    3:
      Result := TdxUnitType.Inch;
    4:
      Result := TdxUnitType.Mm;
    5:
      Result := TdxUnitType.Cm;
    else
    begin
      TdxRichEditExceptions.ThrowArgumentException('UnitType', AValue);
      Result := TdxUnitType.Emu;
    end;
  end;
end;

{ TdxValueInfo }

constructor TdxValueInfo.Create(AValue: Single; const AUnit: string);
begin
  FIsValidNumber := True;
  FUnit := AUnit;
  FValue := AValue;
end;

constructor TdxValueInfo.Create(const AUnit: string);
begin
  FIsValidNumber := False;
  FUnit := AUnit;
  FValue := 0;
end;

procedure TdxValueInfo.Clear;
begin
  FUnit := '';
  FValue := 0;
  FIsValidNumber := False;
end;

class operator TdxValueInfo.Equal(const A, B: TdxValueInfo): Boolean;
begin
  Result := AnsiSameText(A.&Unit, B.&Unit) and (A.Value = B.Value) and (A.IsValidNumber = B.IsValidNumber);
end;

class function TdxValueInfo.GetEmpty: TdxValueInfo;
begin
  Result.Clear;
end;

function TdxValueInfo.GetIsEmpty: Boolean;
begin
  Result := (FUnit = '') and (FValue = 0) and (FIsValidNumber = False);
end;

{ TdxStringValueParser }

class function TdxStringValueParser.TryParse(const AInputString: string): TdxValueInfo;
var
  ASplitResult: TSplitResult;
begin
  if AInputString = '' then
    Exit(TdxValueInfo.Empty);
  ASplitResult := SplitUnitFromValue(AInputString);
  Result := Parse(ASplitResult);
end;

class function TdxStringValueParser.SplitUnitFromValue(const AInputString: string): TSplitResult;
var
  AValue, AUnit: string;
  APos: Integer;
begin
  AValue := '';
  AUnit := AInputString;

  APos := TdxStringHelper.LastIndexOfAny(AInputString, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']);
  if APos <> -1 then
  begin
    AValue := TdxStringHelper.Substring(AInputString, 0, APos + 1);
    AUnit := TdxStringHelper.Substring(AInputString, APos + 1);
  end;
  Result := TSplitResult.Create(AValue, AUnit);
end;

class function TdxStringValueParser.Parse(ASplitResult: TSplitResult): TdxValueInfo;
var
  AValue: Double;
begin
  if TdxNumber.TryParse(ASplitResult.Value, TdxNumberStyles.Float, AValue) then
    Result := TdxValueInfo.Create(AValue, ASplitResult.&Unit)
  else
    Result := TdxValueInfo.Create(ASplitResult.&Unit);
end;

{ TdxStringValueParser.TdxSplitResult }

constructor TdxStringValueParser.TSplitResult.Create(const AValue, AUnit: string);
begin
  Value := AValue;
  &Unit := AUnit;
end;

end.
