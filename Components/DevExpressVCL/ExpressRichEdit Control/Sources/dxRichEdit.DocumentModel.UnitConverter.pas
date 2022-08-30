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

unit dxRichEdit.DocumentModel.UnitConverter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, dxCore, dxCoreClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.Strs,
  dxRichEdit.Utils.DXUnit,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter;

type

  { TdxUnitPrecisions }

  TdxUnitPrecisions = record
  private
    FDefaultPrecisions: array [TdxRichEditDocumentUnit] of Integer;
    function GetUnitPrecision(Index: TdxRichEditDocumentUnit): Integer;
    class function GetDefaultPrecisions: TdxUnitPrecisions; static;
  public
    constructor Create(const ADefaultPrecisions: array of integer);
    property UnitPrecisions[Index: TdxRichEditDocumentUnit]: Integer read GetUnitPrecision; default;
    class property DefaultPrecisions: TdxUnitPrecisions read GetDefaultPrecisions;
  end;

  { TdxUnitCaptionDictionary }

  TdxUnitCaptionDictionary = class
  private
    const DefaultCaptions: array [TdxRichEditDocumentUnit] of Pointer = (nil, @sdxRichEditCaptionUnitInches,
      @sdxRichEditCaptionUnitMillimeters, @sdxRichEditCaptionUnitCentimeters, @sdxRichEditCaptionUnitPoints);
  private
    class function GetUnitCaption(Index: TdxRichEditDocumentUnit): string; static;
  public
    class property UnitCaption[Index: TdxRichEditDocumentUnit]: string read GetUnitCaption; default;
  end;

  { TdxUnitAbbreviationDictionary }

  TdxUnitAbbreviationDictionary = class
  private
    const DefaultAbbreviations: array [TdxRichEditDocumentUnit] of Pointer = (nil, @sdxRichEditUnitsInches,
      @sdxRichEditUnitsMillimeters, @sdxRichEditUnitsCentimeters, @sdxRichEditUnitsPoints);
  private
    class function GetUnitAbbreviation(Index: TdxRichEditDocumentUnit): string; static;
  public
    class function IsValidUnit(AValue: TdxRichEditDocumentUnit): Boolean;
    class property UnitAbbreviation[Index: TdxRichEditDocumentUnit]: string read GetUnitAbbreviation; default;
  end;

  { TdxUIUnit }

  TdxUIUnit = record
  private
    FIsValueInPercent: Boolean;
    FUnitType: TdxRichEditDocumentUnit;
    FUnitPrecisions: TdxUnitPrecisions;
    FValue: Single;
    FStringValue: string;
    FIsNegative: Boolean;
    procedure Init(const AValue: string; AType: TdxRichEditDocumentUnit; const AUnitPrecisions: TdxUnitPrecisions);
    procedure SetIsValueInPercent(const Value: Boolean);
    procedure SetUnitType(const Value: TdxRichEditDocumentUnit);
    procedure SetValue(const Value: Single);

    class function GetDecimalSeparator: Char; static;
    class function GetTextAbbreviation(AUnitType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): string; static;
    class function GetUnitTypeAndStringValue(const AText: string; ADefaultUnitType: TdxRichEditDocumentUnit;
      out AType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): string; static;
    class function TryParseFloatValue(const ASourceStringValue: string; out AResultStringValue: string;
      out AIsNegative: Boolean): Boolean; static;
    class function GetValidType(AType: TdxRichEditDocumentUnit): TdxRichEditDocumentUnit; static;
    function GetOperationValidStringValue: string;

    constructor Create(const AValue: string; AType: TdxRichEditDocumentUnit; AIsNegative: Boolean); overload;
  public
    constructor Create(const AValue: string; ADefaultUnitType: TdxRichEditDocumentUnit; const AUnitPrecisions: TdxUnitPrecisions;
      AIsValueInPercent: Boolean = False); overload;
    constructor Create(AValue: Single; AType: TdxRichEditDocumentUnit); overload;
    constructor Create(AValue: Single; AType: TdxRichEditDocumentUnit; const AUnitPrecisions: TdxUnitPrecisions); overload;
    class function TryParse(const AText: string; ADefaultUnitType: TdxRichEditDocumentUnit; out AResult: TdxUIUnit;
      AIsValueInPercent: Boolean = False): Boolean; static;

    function CreateZeroStringValue(AMaxDigitsAfterDecimalSeparator: Integer): string;
    function GetRightPart(const AStringValueParts: TArray<string>; AMaxDigitsAfterDecimalSeparator: Integer): string;
    function GetValueOfDesiredLength(const AValue: string; ADesiredLength: Integer): string;
    function GetValidValueString(const AStringValue: string; AMaxDigitsAfterDecimalSeparator: Integer): string;
    function IsIntegerValue(const AValue: string): Boolean;
    function ToString: string;
    function TrimBeginInsignificantZeros(const AStringValue: string): string;
    function TrimEndInsignificantZeros(const AStringValue: string): string;
    function TrimInsignificantZeros(const AStringValue: string): string;

    class property DecimalSeparator: Char read GetDecimalSeparator;
    property IsNegative: Boolean read FIsNegative write FIsNegative;
    property IsValueInPercent: Boolean read FIsValueInPercent write SetIsValueInPercent;
    property OperationValidStringValue: string read GetOperationValidStringValue;
    property Value: Single read FValue write SetValue;
    property UnitType: TdxRichEditDocumentUnit read FUnitType write SetUnitType;
    property UnitPrecisions: TdxUnitPrecisions read FUnitPrecisions;
  end;

  { TdxUIUnitConverter }

  TdxUIUnitConverter = record
  strict private
    FUnitPrecisions: TdxUnitPrecisions;
  public
    constructor Create(const AUnitPrecisions: TdxUnitPrecisions);
    function CreateUIUnit(const AText: string; ADefaultUnitType: TdxRichEditDocumentUnit): TdxUIUnit; overload;
    function CreateUIUnit(const AText: string; ADefaultUnitType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): TdxUIUnit; overload;
    function ToUIUnit(AValue: Integer; AType: TdxRichEditDocumentUnit): TdxUIUnit; overload;
    function ToUIUnit(AValue: Integer; AType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): TdxUIUnit; overload;
    function ToUIUnitF(AValue: Single; AType: TdxRichEditDocumentUnit): TdxUIUnit; overload;
    function ToUIUnitF(AValue: Single; AType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): TdxUIUnit; overload;
    function ToInt(AValue: Double): Integer;
    function ToTwipsUnit(const AValue: TdxUIUnit): Integer; overload;
    function ToTwipsUnit(const AValue: TdxUIUnit; AIsValueInPercent: Boolean): Integer; overload;
    function ToTwipsUnitF(const AValue: TdxUIUnit): Single; overload;
    function ToTwipsUnitF(const AValue: TdxUIUnit; AIsValueInPercent: Boolean): Single; overload;
  end;

  { TdxDocumentModelUnitConverter }

  TdxDocumentModelUnitConverter = class(TdxDpiSupport, IdxDocumentModelUnitConverter)
  public
    function ToUIUnit(AValue: Integer; AType: TdxRichEditDocumentUnit): TdxUIUnit; overload;
    function ToUIUnit(AValue: Integer; AType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): TdxUIUnit; overload;
    function ToUIUnitF(AValue: Single; AType: TdxRichEditDocumentUnit): TdxUIUnit; overload;
    function ToUIUnitF(AValue: Single; AType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): TdxUIUnit; overload;
    function ToInt(AValue: Double): Integer;
    function ToTwipsUnit(const AValue: TdxUIUnit): Integer; overload;
    function ToTwipsUnit(const AValue: TdxUIUnit; AIsValueInPercent: Boolean): Integer; overload;
    function ToTwipsUnitF(const AValue: TdxUIUnit): Single; overload;
    function ToTwipsUnitF(const AValue: TdxUIUnit; AIsValueInPercent: Boolean): Single; overload;

    function CreateConverterToLayoutUnits(const AUnit: TdxDocumentLayoutUnit; const ADpi: Single): TdxDocumentModelUnitToLayoutUnitConverter; overload; virtual; abstract;
    function CreateConverterToLayoutUnits(const AUnit: TdxDocumentLayoutUnit): TdxDocumentModelUnitToLayoutUnitConverter; overload;
    function TwipsToModelUnits(const AValue: Integer): Integer; overload; virtual; abstract;
    function TwipsToModelUnits(const AValue: TSize): TSize; overload; virtual; abstract;
    function MillimetersToModelUnitsF(const AValue: Single): Single; virtual; abstract;
    function PointsToModelUnits(const AValue: Integer): Integer; virtual; abstract;
    function PointsToModelUnitsF(const AValue: Single): Single; virtual; abstract;
    function PixelsToModelUnits(const AValue: Integer; const ADpi: Single): Integer; overload; virtual; abstract;
    function PixelsToModelUnits(const AValue: Integer): Integer; overload;
    function PixelsToModelUnits(const AValue: TSize; const ADpiX, ADpiY: Single): TSize; overload; virtual; abstract;
    function PixelsToModelUnits(const AValue: TSize): TSize; overload;
    function EmuToModelUnits(const AValue: Integer): Integer; virtual; abstract;
    function EmuToModelUnitsL(const AValue: Int64): Int64; virtual; abstract;
    function EmuToModelUnitsF(const AValue: Integer): Single; virtual; abstract;
    function ModelUnitsToEmu(const AValue: Integer): Integer; virtual; abstract;
    function ModelUnitsToEmuL(const AValue: Int64): Int64; virtual; abstract;
    function ModelUnitsToEmuF(const AValue: Single): Integer; virtual; abstract;
    function HundredthsOfInchToModelUnits(const AValue: Integer): Integer; overload; virtual; abstract;
    function HundredthsOfInchToModelUnits(const AValue: TSize): TSize; overload; virtual; abstract;
    function HundredthsOfMillimeterToModelUnits(const AValue: Integer): Integer; overload; virtual; abstract;
    function HundredthsOfMillimeterToModelUnits(const AValue: TSize): TSize; overload; virtual; abstract;
    function HundredthsOfMillimeterToModelUnitsRound(const AValue: Integer): Integer; virtual; abstract;
    function CentimetersToModelUnitsF(const AValue: Single): Single; virtual; abstract;
    function InchesToModelUnitsF(const AValue: Single): Single; virtual; abstract;
    function PicasToModelUnitsF(const AValue: Single): Single; virtual; abstract;
    function DocumentsToModelUnits(const AValue: Integer): Integer; overload; virtual; abstract;
    function DocumentsToModelUnits(const AValue: TSize): TSize; overload; virtual; abstract;
    function DocumentsToModelUnitsF(const AValue: Single): Single; virtual; abstract;
    function AdjAngleToModelUnits(const AValue: Integer): Integer;
    function ModelUnitsToAdjAngle(const AValue: Integer): Integer;
    function DegreeToModelUnits(const AValue: Single): Integer;
    function FDToModelUnits(const AValue: Integer): Integer; virtual; abstract;
    function ModelUnitsToTwips(const AValue: Integer): Integer; overload; virtual; abstract;
    function ModelUnitsToTwipsF(const AValue: Single): Single; virtual; abstract;
    function ModelUnitsToTwips(const AValue: TSize): TSize; overload; virtual; abstract;
    function ModelUnitsToHundredthsOfMillimeter(const AValue: TSize): TSize; virtual; abstract;
    function ModelUnitsToPointsF(const AValue: Single): Single; virtual; abstract;
    function ModelUnitsToPointsFRound(const AValue: Single): Single; virtual; abstract;
    function ModelUnitsToPixels(const AValue: Integer; const ADpi: Single): Integer; virtual; abstract;
    function ModelUnitsToPixelsF(const AValue: Single; const ADpi: Single): Single; virtual; abstract;
    function ModelUnitsToCentimetersF(const AValue: Single): Single; virtual; abstract;
    function ModelUnitsToInchesF(const AValue: Single): Single; virtual; abstract;
    function ModelUnitsToMillimetersF(const AValue: Single): Single; virtual; abstract;
    function ModelUnitsToDocumentsF(const AValue: Single): Single; virtual; abstract;
    function ModelUnitsToHundredthsOfInch(const AValue: Integer): Integer; overload; virtual; abstract;
    function ModelUnitsToHundredthsOfInch(const AValue: TSize): TSize; overload; virtual; abstract;
    function ModelUnitsToDegree(const AValue: Integer): Integer;
    function ModelUnitsToDegreeF(const AValue: Integer): Single;
    function ModelUnitsToRadians(const AValue: Integer): Single;
    function ModelUnitsToFD(const AValue: Integer): Integer; virtual; abstract;
  end;

  { TdxUnitConverter }

  TdxUnitConverter = class abstract
  strict private
    FUnitConverter: TdxDocumentModelUnitConverter;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitConverter);
    function ToUnits(AValue: Single): Single; virtual; abstract;
    function FromUnits(AValue: Single): Single; virtual; abstract;

    property Converter: TdxDocumentModelUnitConverter read FUnitConverter;
  end;

  { TdxUnitsConverter }

  TdxUnitsConverter = record
  strict private
    FUnitConverter: TdxDocumentModelUnitConverter;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitConverter);
    function ValueUnitToModelUnitsF(const AInfo: TdxValueInfo): Single;
  end;

  { TdxCentimetersToModelUnitsConverter }

  TdxCentimetersToModelUnitsConverter = class(TdxUnitConverter)
  public
    function ToUnits(AValue: Single): Single; override;
    function FromUnits(AValue: Single): Single; override;
  end;

  { TdxDocumentsToModelUnitsConverter }

  TdxDocumentsToModelUnitsConverter = class(TdxUnitConverter)
  public
    function ToUnits(AValue: Single): Single; override;
    function FromUnits(AValue: Single): Single; override;
  end;

  { TdxInchesToModelUnitsConverter }

  TdxInchesToModelUnitsConverter = class(TdxUnitConverter)
  public
    function ToUnits(AValue: Single): Single; override;
    function FromUnits(AValue: Single): Single; override;
  end;

  { TdxMillimetersToModelUnitsConverter }

  TdxMillimetersToModelUnitsConverter = class(TdxUnitConverter)
  public
    function ToUnits(AValue: Single): Single; override;
    function FromUnits(AValue: Single): Single; override;
  end;

  { TdxPointsToModelUnitsConverter }

  TdxPointsToModelUnitsConverter = class(TdxUnitConverter)
  public
    function ToUnits(AValue: Single): Single; override;
    function FromUnits(AValue: Single): Single; override;
  end;

implementation

uses
  Math, Character, SysUtils, StrUtils, RegularExpressions,

  dxRichEdit.Utils.Graphics,
  dxMeasurementUnits,
  dxStringHelper,
  dxRichEdit.DocumentModel.Core;

{ TdxDocumentModelUnitConverter }

function TdxDocumentModelUnitConverter.ToUIUnit(AValue: Integer; AType: TdxRichEditDocumentUnit): TdxUIUnit;
begin
  Result := ToUIUnit(AValue, AType, False);
end;

function TdxDocumentModelUnitConverter.ToUIUnit(AValue: Integer; AType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): TdxUIUnit;
begin
  Result := ToUIUnitF(AValue, AType, AIsValueInPercent);
end;

function TdxDocumentModelUnitConverter.ToUIUnitF(AValue: Single; AType: TdxRichEditDocumentUnit): TdxUIUnit;
begin
  Result := ToUIUnitF(AValue, AType, False);
end;

function TdxDocumentModelUnitConverter.ToUIUnitF(AValue: Single; AType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): TdxUIUnit;
var
  AResult: TdxUIUnit;
begin
  if AIsValueInPercent then
  begin
    AResult := TdxUIUnit.Create(AValue, AType);
    AResult.IsValueInPercent := True;
    Exit(AResult);
  end;
  case AType of
    TdxRichEditDocumentUnit.Centimeter:
      Exit(TdxUIUnit.Create(TwipsToCentimetersF(AValue), TdxRichEditDocumentUnit.Centimeter));
    TdxRichEditDocumentUnit.Inch:
      Exit(TdxUIUnit.Create(TwipsToInchesF(AValue), TdxRichEditDocumentUnit.Inch));
    TdxRichEditDocumentUnit.Millimeter:
      Exit(TdxUIUnit.Create(TwipsToMillimetersF(AValue), TdxRichEditDocumentUnit.Millimeter));
    TdxRichEditDocumentUnit.Point:
      Exit(TdxUIUnit.Create(TwipsToPointsF(AValue), TdxRichEditDocumentUnit.Point));
  end;
end;

function TdxDocumentModelUnitConverter.ToInt(AValue: Double): Integer;
begin
  if AValue > MaxInt  then
    Result := MaxInt
  else
    if AValue < MinInt then
      Result := MinInt
    else
      Result := Trunc(AValue);
end;

function TdxDocumentModelUnitConverter.ToTwipsUnit(const AValue: TdxUIUnit): Integer;
begin
  Result := ToTwipsUnit(AValue, False);
end;

function TdxDocumentModelUnitConverter.ToTwipsUnit(const AValue: TdxUIUnit; AIsValueInPercent: Boolean): Integer;
var
  AResult: Single;
begin
  AResult := ToTwipsUnitF(AValue, AIsValueInPercent);

  if AResult < 0 then
    Result := ToInt(Floor(AResult))
  else
    Result := ToInt(Ceil(AResult));
end;

function TdxDocumentModelUnitConverter.ToTwipsUnitF(const AValue: TdxUIUnit): Single;
begin
  Result := ToTwipsUnitF(AValue, False);
end;

function TdxDocumentModelUnitConverter.ToTwipsUnitF(const AValue: TdxUIUnit; AIsValueInPercent: Boolean): Single;
begin
  if AIsValueInPercent then
    Exit(AValue.Value);

  case AValue.UnitType of
    TdxRichEditDocumentUnit.Centimeter:
      Result := CentimetersToTwipsF(AValue.Value);
    TdxRichEditDocumentUnit.Inch:
      Result := InchesToTwipsF(AValue.Value);
    TdxRichEditDocumentUnit.Millimeter:
      Result := MillimetersToTwipsF(AValue.Value);
    TdxRichEditDocumentUnit.Point:
      Result := PointsToTwipsF(AValue.Value);
    else
      Result := 0;
  end;
end;

function TdxDocumentModelUnitConverter.AdjAngleToModelUnits(const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentModelUnitConverter.ModelUnitsToAdjAngle(const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentModelUnitConverter.CreateConverterToLayoutUnits(
  const AUnit: TdxDocumentLayoutUnit): TdxDocumentModelUnitToLayoutUnitConverter;
begin
  Result := CreateConverterToLayoutUnits(AUnit, TdxCustomDocumentModel.Dpi);
end;

function TdxDocumentModelUnitConverter.DegreeToModelUnits(const AValue: Single): Integer;
begin
  Result := Round(AValue * 60000);
end;

function TdxDocumentModelUnitConverter.ModelUnitsToDegree(const AValue: Integer): Integer;
begin
  Result := Trunc(AValue / 60000);
end;

function TdxDocumentModelUnitConverter.ModelUnitsToDegreeF(const AValue: Integer): Single;
begin
  Result := AValue / 60000;
end;

function TdxDocumentModelUnitConverter.ModelUnitsToRadians(const AValue: Integer): Single;
begin
  Result := PI * (AValue / 60000) / 180;
end;

function TdxDocumentModelUnitConverter.PixelsToModelUnits(const AValue: Integer): Integer;
begin
  Result := PixelsToModelUnits(AValue, ScreenDpi);
end;

function TdxDocumentModelUnitConverter.PixelsToModelUnits(const AValue: TSize): TSize;
begin
  Result := PixelsToModelUnits(AValue, ScreenDpiX, ScreenDpiY);
end;

{ TdxUnitConverter }

constructor TdxUnitConverter.Create(AUnitConverter: TdxDocumentModelUnitConverter);
begin
  inherited Create;
  FUnitConverter := AUnitConverter;
end;

{ TdxUnitsConverter }

constructor TdxUnitsConverter.Create(AUnitConverter: TdxDocumentModelUnitConverter);
begin
  Assert(AUnitConverter <> nil, 'unitConverter');
  FUnitConverter := AUnitConverter;
end;

function TdxUnitsConverter.ValueUnitToModelUnitsF(const AInfo: TdxValueInfo): Single;
var
  AUnit: string;
begin
  if AInfo.&Unit = '' then
    Exit(AInfo.Value);
  AUnit := LowerCase(AInfo.&Unit);
  if AUnit = 'km' then
    Result := FUnitConverter.CentimetersToModelUnitsF(AInfo.Value * 100000)
  else
  if AUnit = 'm' then
    Result := FUnitConverter.CentimetersToModelUnitsF(AInfo.Value * 100)
  else
  if AUnit = 'cm' then
    Result := FUnitConverter.CentimetersToModelUnitsF(AInfo.Value)
  else
  if AUnit = 'mm' then
    Result := FUnitConverter.MillimetersToModelUnitsF(AInfo.Value)
  else
  if AUnit = 'in' then
    Result := FUnitConverter.InchesToModelUnitsF(AInfo.Value)
  else
  if AUnit = 'pt' then
    Result := FUnitConverter.PointsToModelUnitsF(AInfo.Value)
  else
  if AUnit = 'pc' then
    Result := FUnitConverter.PicasToModelUnitsF(AInfo.Value)
  else
  if AUnit = 'inch' then
    Result := FUnitConverter.InchesToModelUnitsF(AInfo.Value)
  else
  if AUnit = 'ft' then
    Result := FUnitConverter.InchesToModelUnitsF(AInfo.Value * 12)
  else
  if (AUnit = 'mi') or (AUnit = '%') then
    Result := AInfo.Value / 100
  else
    Result := AInfo.Value;
end;

{ TdxCentimetersToModelUnitsConverter }

function TdxCentimetersToModelUnitsConverter.ToUnits(AValue: Single): Single;
begin
  Result := Converter.CentimetersToModelUnitsF(AValue);
end;

function TdxCentimetersToModelUnitsConverter.FromUnits(AValue: Single): Single;
begin
  Result := Converter.ModelUnitsToCentimetersF(AValue);
end;

{ TdxDocumentsToModelUnitsConverter }

function TdxDocumentsToModelUnitsConverter.ToUnits(AValue: Single): Single;
begin
  Result := Converter.DocumentsToModelUnitsF(AValue);
end;

function TdxDocumentsToModelUnitsConverter.FromUnits(AValue: Single): Single;
begin
  Result := Converter.ModelUnitsToDocumentsF(AValue);
end;

{ TdxInchesToModelUnitsConverter }

function TdxInchesToModelUnitsConverter.ToUnits(AValue: Single): Single;
begin
  Result := Converter.InchesToModelUnitsF(AValue);
end;

function TdxInchesToModelUnitsConverter.FromUnits(AValue: Single): Single;
begin
  Result := Converter.ModelUnitsToInchesF(AValue);
end;

{ TdxMillimetersToModelUnitsConverter }

function TdxMillimetersToModelUnitsConverter.ToUnits(AValue: Single): Single;
begin
  Result := Converter.MillimetersToModelUnitsF(AValue);
end;

function TdxMillimetersToModelUnitsConverter.FromUnits(AValue: Single): Single;
begin
  Result := Converter.ModelUnitsToMillimetersF(AValue);
end;

{ TdxPointsToModelUnitsConverter }

function TdxPointsToModelUnitsConverter.ToUnits(AValue: Single): Single;
begin
  Result := Converter.PointsToModelUnitsF(AValue);
end;

function TdxPointsToModelUnitsConverter.FromUnits(AValue: Single): Single;
begin
  Result := Converter.ModelUnitsToPointsF(AValue);
end;

{ TdxUIUnit }

constructor TdxUIUnit.Create(AValue: Single; AType: TdxRichEditDocumentUnit; const AUnitPrecisions: TdxUnitPrecisions);
var
  ARoundedValue: Double;
begin
  FValue := AValue;
  FUnitType := GetValidType(AType);
  FUnitPrecisions := AUnitPrecisions;
  ARoundedValue := RoundTo(AValue, -(UnitPrecisions[UnitType] + 1));
  FStringValue := FloatToStr(Abs(ARoundedValue));
  FIsNegative := AValue < 0;
  FIsValueInPercent := False;
end;

constructor TdxUIUnit.Create(const AValue: string; AType: TdxRichEditDocumentUnit; AIsNegative: Boolean);
begin
  FStringValue := AValue;
  FUnitType := GetValidType(AType);
  FIsNegative := AIsNegative;
  FIsValueInPercent := false;
end;

function TdxUIUnit.CreateZeroStringValue(AMaxDigitsAfterDecimalSeparator: Integer): string;
var
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    SB.Append('0');
    if AMaxDigitsAfterDecimalSeparator > 0 then
    begin
      SB.Append(DecimalSeparator);
      SB.Append('0', AMaxDigitsAfterDecimalSeparator);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

constructor TdxUIUnit.Create(AValue: Single; AType: TdxRichEditDocumentUnit);
begin
  Create(AValue, AType, TdxUnitPrecisions.DefaultPrecisions);
end;

constructor TdxUIUnit.Create(const AValue: string; ADefaultUnitType: TdxRichEditDocumentUnit;
  const AUnitPrecisions: TdxUnitPrecisions; AIsValueInPercent: Boolean);
var
  AUnitType: TdxRichEditDocumentUnit;
  AStringValue: string;
begin
  AUnitType := ADefaultUnitType;
  AStringValue := GetUnitTypeAndStringValue(AValue, ADefaultUnitType, AUnitType, AIsValueInPercent);
  Init(AStringValue, AUnitType, AUnitPrecisions);
  FIsValueInPercent := AIsValueInPercent;
end;

class function TdxUIUnit.GetDecimalSeparator: Char;
begin
  Result := FormatSettings.DecimalSeparator;
end;

function TdxUIUnit.GetOperationValidStringValue: string;
begin
  Result := GetValidValueString(FStringValue, UnitPrecisions[UnitType]);
end;

function TdxUIUnit.GetRightPart(const AStringValueParts: TArray<string>; AMaxDigitsAfterDecimalSeparator: Integer): string;
begin
  if Length(AStringValueParts) = 2 then
    Result := AStringValueParts[1]
  else
    Result := '';
end;

class function TdxUIUnit.GetTextAbbreviation(AUnitType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): string;
begin
  if AIsValueInPercent then
    Result := cxGetResourceString(@sdxRichEditUnitsPercent)
  else
    Result := TdxUnitAbbreviationDictionary.UnitAbbreviation[AUnitType];
end;

class function TdxUIUnit.GetUnitTypeAndStringValue(const AText: string; ADefaultUnitType: TdxRichEditDocumentUnit;
  out AType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): string;
var
  AStringValue: string;
  AUnitType: TdxRichEditDocumentUnit;
  ATestAbbreviationText: string;
  AIndex: Integer;
begin
  AType := ADefaultUnitType;
  AStringValue := AText;
  for AUnitType := Low(TdxRichEditDocumentUnit) to High(TdxRichEditDocumentUnit) do
    if TdxUnitAbbreviationDictionary.IsValidUnit(AUnitType) then
    begin
      ATestAbbreviationText := Trim(GetTextAbbreviation(AUnitType, AIsValueInPercent));
      AIndex := TdxStringHelper.LastIndexOf(AText, ATestAbbreviationText);
      if AIndex = -1 then
        Continue;
      if AIndex = Length(AText) - Length(ATestAbbreviationText) then
      begin
        AStringValue := TdxStringHelper.Substring(AText, 0, AIndex);
        AType := AUnitType;
        Break;
      end;
    end;
  Result := AStringValue;
end;

class function TdxUIUnit.GetValidType(AType: TdxRichEditDocumentUnit): TdxRichEditDocumentUnit;
begin
  if AType = TdxRichEditDocumentUnit.Document then
    Result := TdxRichEditDocumentUnit.Inch
  else
    Result := AType;
end;

function TdxUIUnit.GetValidValueString(const AStringValue: string; AMaxDigitsAfterDecimalSeparator: Integer): string;
var
  AStringValueParts: TArray<string>;
  APartCount: Integer;
  ALeftPart, ARightPart: string;
begin
  if AStringValue = '' then
    Exit(CreateZeroStringValue(AMaxDigitsAfterDecimalSeparator));
  AStringValueParts := TdxStringHelper.Split(AStringValue, [DecimalSeparator]);
  APartCount := Length(AStringValueParts);
  if APartCount > 2 then
    Exit(CreateZeroStringValue(AMaxDigitsAfterDecimalSeparator));
  ALeftPart := IfThen(AStringValueParts[0] = '', '0', AStringValueParts[0]);
  ARightPart := GetRightPart(AStringValueParts, AMaxDigitsAfterDecimalSeparator);
  if not IsIntegerValue(ALeftPart) or ((ARightPart <> '') and not IsIntegerValue(ARightPart)) then
    Exit(CreateZeroStringValue(AMaxDigitsAfterDecimalSeparator));
  ARightPart := GetValueOfDesiredLength(ARightPart, AMaxDigitsAfterDecimalSeparator);
  if AMaxDigitsAfterDecimalSeparator < 1 then
    Result := ALeftPart
  else
    Result := ALeftPart + DecimalSeparator + ARightPart;
end;

function TdxUIUnit.GetValueOfDesiredLength(const AValue: string; ADesiredLength: Integer): string;
var
  AZeroCount: Integer;
begin
  if Length(AValue) > ADesiredLength then
    Result := TdxStringHelper.Substring(AValue, 0, ADesiredLength)
  else
  begin
    AZeroCount := ADesiredLength - Length(AValue);
    Result := AValue + StringOfChar('0', AZeroCount);
  end;
end;

procedure TdxUIUnit.Init(const AValue: string; AType: TdxRichEditDocumentUnit;
  const AUnitPrecisions: TdxUnitPrecisions);
begin
  FUnitType := GetValidType(AType);
  FUnitPrecisions := AUnitPrecisions;
  TryParseFloatValue(AValue, FStringValue, FIsNegative);
  FStringValue := GetValidValueString(FStringValue, UnitPrecisions[UnitType] + 1);
  FStringValue := TrimInsignificantZeros(FStringValue);
  TryStrToFloat(FStringValue, FValue);
  if IsNegative then
    FValue := -FValue;
  FIsValueInPercent := False;
end;

function TdxUIUnit.IsIntegerValue(const AValue: string): Boolean;
var
  C: Char;
begin
  for C in AValue do
    if (C < '0') or (C > '9') then
      Exit(False);
  Result := True;
end;

procedure TdxUIUnit.SetIsValueInPercent(const Value: Boolean);
begin
  FIsValueInPercent := Value;
end;

procedure TdxUIUnit.SetUnitType(const Value: TdxRichEditDocumentUnit);
begin
  FUnitType := Value;
end;

procedure TdxUIUnit.SetValue(const Value: Single);
begin
  FValue := Value;
end;

function TdxUIUnit.ToString: string;
var
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    if IsNegative then
      SB.Append('-');
    SB.Append(TrimInsignificantZeros(FStringValue));
    SB.Append(GetTextAbbreviation(UnitType, IsValueInPercent));
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TdxUIUnit.TrimBeginInsignificantZeros(const AStringValue: string): string;
var
  ACount, APosition: Integer;
  C: Char;
begin
  ACount := Length(AStringValue);
  APosition := 0;
  for C in AStringValue do
  begin
    if C <> '0' then
      Break;
    Inc(APosition);
  end;
  if (APosition = ACount) or (APosition = 0) then
    Result := AStringValue
  else
  begin
    if not {$IFDEF DELPHIXE4}AStringValue[APosition - 1].IsDigit{$ELSE}TCharacter.IsDigit(AStringValue[APosition - 1]){$ENDIF} then
      Dec(APosition);
    Result := TdxStringHelper.Substring(AStringValue, APosition, ACount - APosition);
  end;
end;

function TdxUIUnit.TrimEndInsignificantZeros(const AStringValue: string): string;
var
  ANewValue: string;
  ALength: Integer;
begin
  if TdxStringHelper.IndexOf(AStringValue, DecimalSeparator) = -1 then
    Exit(AStringValue);
  ANewValue := TdxStringHelper.TrimEnd(AStringValue, ['0']);
  ALength := Length(ANewValue);
  if ANewValue[ALength] = DecimalSeparator then
    Result := TdxStringHelper.Substring(AStringValue, 0, ALength - 1)
  else
    Result := ANewValue;
end;

function TdxUIUnit.TrimInsignificantZeros(const AStringValue: string): string;
begin
  Result := TrimEndInsignificantZeros(AStringValue);
  Result := TrimBeginInsignificantZeros(Result);
end;

class function TdxUIUnit.TryParse(const AText: string; ADefaultUnitType: TdxRichEditDocumentUnit; out AResult: TdxUIUnit;
  AIsValueInPercent: Boolean): Boolean;
var
  AUnitType: TdxRichEditDocumentUnit;
  AStringValue, AResultValue: string;
  AIsNegative, AIsValid: Boolean;
begin
  AUnitType := ADefaultUnitType;
  AStringValue := GetUnitTypeAndStringValue(AText, ADefaultUnitType, AUnitType, AIsValueInPercent);
  AIsNegative := False;
  AResultValue := '';
  AIsValid := TryParseFloatValue(AStringValue, AResultValue, AIsNegative);
  if not AIsValid then
    Result := False
  else
  begin
    AResult := TdxUIUnit.Create(AResultValue, AUnitType, AIsNegative);
    AResult.IsValueInPercent := AIsValueInPercent;
    Result := True;
  end;
end;

class function TdxUIUnit.TryParseFloatValue(const ASourceStringValue: string; out AResultStringValue: string;
  out AIsNegative: Boolean): Boolean;
var
  APattern, ADecimalSeparatorPattern: string;
  AMatch: TMatch;
  AGroups: TGroupCollection;
  ASignGroup, AValueGroup: TGroup;
begin
  AIsNegative := False;
  AResultStringValue := '0';
  ADecimalSeparatorPattern := DecimalSeparator;
  APattern := Format('^(?<sign>\+|-)?(?<value>\d*%s?(\d+)?)$', [ADecimalSeparatorPattern]);
  AMatch := TRegEx.Match(Trim(ASourceStringValue), APattern);
  if not AMatch.Success then
    Exit(False);
  AGroups := AMatch.Groups;
  ASignGroup := AGroups['sign'];
  AValueGroup := AGroups['value'];
  AIsNegative := ASignGroup.Success and (ASignGroup.Value = '-');
  AResultStringValue := Trim(AValueGroup.Value);
  if (Length(AResultStringValue) = 0) or ((Length(AResultStringValue) = 1) and (AResultStringValue[1] = DecimalSeparator)) then
    AIsNegative := False;
  Result := True;
end;

{ TdxUIUnitConverter }

constructor TdxUIUnitConverter.Create(const AUnitPrecisions: TdxUnitPrecisions);
begin
  FUnitPrecisions := AUnitPrecisions;
end;

function TdxUIUnitConverter.ToUIUnit(AValue: Integer; AType: TdxRichEditDocumentUnit): TdxUIUnit;
begin
  Result := ToUIUnit(AValue, AType, False);
end;

function TdxUIUnitConverter.ToUIUnit(AValue: Integer; AType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): TdxUIUnit;
begin
  Result := ToUIUnitF(AValue, AType, AIsValueInPercent);
end;

function TdxUIUnitConverter.ToUIUnitF(AValue: Single; AType: TdxRichEditDocumentUnit): TdxUIUnit;
begin
  Result := ToUIUnitF(AValue, AType, False);
end;

function TdxUIUnitConverter.ToUIUnitF(AValue: Single; AType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): TdxUIUnit;
begin
  if AIsValueInPercent then
  begin
    Result := TdxUIUnit.Create(AValue, AType, FUnitPrecisions);
    Result.IsValueInPercent := True;
  end
  else
    case AType of
      TdxRichEditDocumentUnit.Inch:
        Result := TdxUIUnit.Create(TwipsToInchesF(AValue), TdxRichEditDocumentUnit.Inch, FUnitPrecisions);
      TdxRichEditDocumentUnit.Millimeter:
        Result := TdxUIUnit.Create(TwipsToMillimetersF(AValue), TdxRichEditDocumentUnit.Millimeter, FUnitPrecisions);
      TdxRichEditDocumentUnit.Centimeter:
        Result := TdxUIUnit.Create(TwipsToCentimetersF(AValue), TdxRichEditDocumentUnit.Centimeter, FUnitPrecisions);
      TdxRichEditDocumentUnit.Point:
        Result := TdxUIUnit.Create(TwipsToPointsF(AValue), TdxRichEditDocumentUnit.Point, FUnitPrecisions);
      else
        Result := Default(TdxUIUnit);
    end;
end;

function TdxUIUnitConverter.ToInt(AValue: Double): Integer;
begin
  if AValue > MaxInt then
    Result := MaxInt
  else
    if AValue < MinInt then
      Result := MinInt
    else
      Result := Trunc(AValue);
end;

function TdxUIUnitConverter.ToTwipsUnit(const AValue: TdxUIUnit): Integer;
begin
  Result := ToTwipsUnit(AValue, False);
end;

function TdxUIUnitConverter.ToTwipsUnit(const AValue: TdxUIUnit; AIsValueInPercent: Boolean): Integer;
var
  AResult: Single;
begin
  AResult := ToTwipsUnitF(AValue, AIsValueInPercent);

  if AResult < 0 then
    Exit(ToInt(Math.Floor(AResult)));
  Result := ToInt(Math.Ceil(AResult));
end;

function TdxUIUnitConverter.ToTwipsUnitF(const AValue: TdxUIUnit): Single;
begin
  Result := ToTwipsUnitF(AValue, False);
end;

function TdxUIUnitConverter.ToTwipsUnitF(const AValue: TdxUIUnit; AIsValueInPercent: Boolean): Single;
begin
  if AIsValueInPercent then
    Result := AValue.Value
  else
    case AValue.UnitType of
      TdxRichEditDocumentUnit.Centimeter:
        Result := CentimetersToTwipsF(AValue.Value);
      TdxRichEditDocumentUnit.Inch:
        Result := InchesToTwipsF(AValue.Value);
      TdxRichEditDocumentUnit.Millimeter:
        Result := MillimetersToTwipsF(AValue.Value);
      TdxRichEditDocumentUnit.Point:
        Result := PointsToTwipsF(AValue.Value);
      else
        Result := 0;
    end;
end;

function TdxUIUnitConverter.CreateUIUnit(const AText: string; ADefaultUnitType: TdxRichEditDocumentUnit): TdxUIUnit;
begin
  Result := CreateUIUnit(AText, ADefaultUnitType, False);
end;

function TdxUIUnitConverter.CreateUIUnit(const AText: string; ADefaultUnitType: TdxRichEditDocumentUnit; AIsValueInPercent: Boolean): TdxUIUnit;
begin
  Result := TdxUIUnit.Create(AText, ADefaultUnitType, FUnitPrecisions, AIsValueInPercent);
end;


{ TdxUnitPrecisions }

constructor TdxUnitPrecisions.Create(const ADefaultPrecisions: array of integer);
var
  I: TdxRichEditDocumentUnit;
begin
  Assert(Length(FDefaultPrecisions) = Length(ADefaultPrecisions));
  for I := Low(FDefaultPrecisions) to High(FDefaultPrecisions) do
   FDefaultPrecisions[I] := ADefaultPrecisions[Ord(I)];
end;

class function TdxUnitPrecisions.GetDefaultPrecisions: TdxUnitPrecisions;
begin
  Result := TdxUnitPrecisions.Create([0, 1, 0, 1, 0]);
end;

function TdxUnitPrecisions.GetUnitPrecision(Index: TdxRichEditDocumentUnit): Integer;
begin
  Result := FDefaultPrecisions[Index];
end;

{ TdxUnitCaptionDictionary }

class function TdxUnitCaptionDictionary.GetUnitCaption(Index: TdxRichEditDocumentUnit): string;
begin
  Result := cxGetResourceString(DefaultCaptions[Index]);
end;

{ TdxUnitAbbreviationDictionary }

class function TdxUnitAbbreviationDictionary.GetUnitAbbreviation(Index: TdxRichEditDocumentUnit): string;
begin
  Result := cxGetResourceString(DefaultAbbreviations[Index]);
end;

class function TdxUnitAbbreviationDictionary.IsValidUnit(AValue: TdxRichEditDocumentUnit): Boolean;
begin
  Result := DefaultAbbreviations[AValue] <> nil;
end;

end.
