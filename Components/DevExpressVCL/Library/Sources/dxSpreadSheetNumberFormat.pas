{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetNumberFormat;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Character, Classes, Graphics, Variants, SysUtils, Generics.Collections, Generics.Defaults,
  dxCore, dxCoreGraphics, dxCultureInfo,
  dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetNumberFormatCore;

type
  EdxSpreadSheetNumberFormatError = class(EdxException);

  TdxSpreadSheetNumberFormatCategory = (nfcGeneral, nfcNumber, nfcCurrency, nfcAccounting,
    nfcDate, nfcTime, nfcPercentage, nfcFraction, nfcScientific, nfcText, nfcCustom);

  { TdxSpreadSheetNumberFormatResult }

  TdxSpreadSheetNumberFormatResult = record
    Color: TColor;
    IsError: Boolean;
    IsText: TdxDefaultBoolean;
    Text: string;

    procedure Reset;
  end;

  { TdxSpreadSheetCustomNumberFormat }

  TdxSpreadSheetCustomNumberFormat = class abstract
  protected
    function GetValueType: TdxNumberFormatType; virtual; abstract;
  public
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; var AResult: TdxSpreadSheetNumberFormatResult); overload; virtual; abstract;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType; ADateTimeSystem: TdxSpreadSheetDateTimeSystem;
      const AFormatSettings: TFormatSettings; var AResult: TdxSpreadSheetNumberFormatResult); overload;
    function Format(const AValue: Variant; const AFormatSettings: TFormatSettings): TdxSpreadSheetNumberFormatResult; overload;

    function IsDateTime: Boolean;
    function IsNumeric: Boolean;
    function IsText: Boolean;
    function IsTime: Boolean; virtual; abstract;

    property ValueType: TdxNumberFormatType read GetValueType;
  end;

  { TdxSpreadSheetNumberFormat }

  TdxSpreadSheetNumberFormat = class(TdxSpreadSheetCustomNumberFormat)
  strict private
    FFormat: TdxNumberFormat;
  protected
    function GetValueType: TdxNumberFormatType; override;
  public
    constructor Create(const AFormatCode: string; AFormatCodeId: Integer = -1);
    destructor Destroy; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; var AResult: TdxSpreadSheetNumberFormatResult); override;
    function IsTime: Boolean; override;
  end;

  { TdxSpreadSheetPredefinedNumberFormat }

  TdxSpreadSheetPredefinedNumberFormat = class(TdxSpreadSheetNumberFormat);

  { TdxSpreadSheetCurrencyFormatHelper }

  TdxSpreadSheetCurrencyFormatHelper = class
  public
    class function GetFormat(ASettings: TdxSpreadSheetFormatSettings; const APattern: string): string;
    class function GetNegativeFormat(ACurrency: string; const APattern: string;
      AFormatCode: Byte; AIncludeCurrencyIntoMarkChar: Boolean = False): string;
    class function GetPositiveFormat(ACurrency: string; const APattern: string;
      AFormatCode: Byte; AIncludeCurrencyIntoMarkChar: Boolean = False): string;
    class function IsCurrencySymbolFollowsValue(ANegFormatCode: Integer): Boolean;
  end;

  { TdxSpreadSheetNumberFormatSplitter }

  TdxSpreadSheetNumberFormatSplitter = class
  public
    class procedure Split(const AFormatCode: string; AList: TStringList);
  end;

  { TdxSpreadSheetDateTimeFormatHelper }

  TdxSpreadSheetDateTimeFormatHelper = class
  public
    class function GetDefaultFormat(const AValue: TDateTime): string;
  end;

  { TdxSpreadSheetDisplayFormatConverter }

  TdxSpreadSheetDisplayFormatConverter = class
  strict private
    class procedure CorrectDigitsPlaces(AData: TStringBuilder);
    class function IsDigitPlaceHolder(const C: Char): Boolean; inline;
  public
    class function ConvertDateTimeDisplayFormat(const S: string; const AValue: TDateTime): string;
    class function ConvertFloatValueDisplayFormat(const S: string): string;
    class function ConvertFloatValueDisplayFormatSection(const S: string): string;
  end;

function dxGetDataTypeByVariantValue(const AValue: Variant): TdxSpreadSheetCellDataType;
function dxGetFormattedResult(const AValue: Variant; const AFormatCode: string): TdxSpreadSheetNumberFormatResult; overload;
function dxGetFormattedResult(const AValue: Variant; const AFormatCode: string;
  AFormatSettings: TdxSpreadSheetCustomFormatSettings): TdxSpreadSheetNumberFormatResult; overload;
function dxIsNumericType(AValueType: TdxSpreadSheetCellDataType): Boolean;
implementation

uses
  dxSpreadSheetCoreStrs, dxSpreadSheetUtils, dxSpreadSheetNumberFormatParser, cxFormats, StrUtils, dxStringHelper;

function dxGetDataTypeByVariantValue(const AValue: Variant): TdxSpreadSheetCellDataType;
begin
  case VarType(AValue) of
    varBoolean:
      Result := cdtBoolean;
    varCurrency:
      Result := cdtCurrency;
    varSingle, varDouble:
      Result := cdtFloat;
    varDate:
      Result := cdtDateTime;
    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varUInt64:
      Result := cdtInteger;
  else
    Result := cdtString;
  end;
end;

function dxGetFormattedResult(const AValue: Variant; const AFormatCode: string): TdxSpreadSheetNumberFormatResult;
var
  AFormatSettings: TdxSpreadSheetCustomFormatSettings;
begin
  AFormatSettings := TdxSpreadSheetCustomFormatSettings.Create;
  try
    Result := dxGetFormattedResult(AValue, AFormatCode, AFormatSettings);
  finally
    AFormatSettings.Free;
  end;
end;

function dxGetFormattedResult(const AValue: Variant; const AFormatCode: string;
  AFormatSettings: TdxSpreadSheetCustomFormatSettings): TdxSpreadSheetNumberFormatResult;
var
  AFormat: TdxSpreadSheetNumberFormat;
begin
  AFormat := TdxSpreadSheetNumberFormat.Create(AFormatCode);
  try
    AFormat.Format(AValue, dxGetDataTypeByVariantValue(AValue), AFormatSettings, Result);
  finally
    AFormat.Free;
  end;
end;

function dxIsNumericType(AValueType: TdxSpreadSheetCellDataType): Boolean;
begin
  Result := AValueType in [cdtCurrency, cdtFloat, cdtDateTime, cdtInteger];
end;

{ TdxSpreadSheetNumberFormatResult }

procedure TdxSpreadSheetNumberFormatResult.Reset;
begin
  Color := clDefault;
  IsError := False;
  IsText := bDefault;
  Text := EmptyStr;
end;

{ TdxSpreadSheetCustomNumberFormat }

function TdxSpreadSheetCustomNumberFormat.Format(
  const AValue: Variant; const AFormatSettings: TFormatSettings): TdxSpreadSheetNumberFormatResult;
begin
  Format(AValue, dxGetDataTypeByVariantValue(AValue), dts1900, AFormatSettings, Result);
end;

function TdxSpreadSheetCustomNumberFormat.IsDateTime: Boolean;
begin
  Result := ValueType = nftDateTime;
end;

function TdxSpreadSheetCustomNumberFormat.IsNumeric: Boolean;
begin
  Result := ValueType = nftNumeric;
end;

function TdxSpreadSheetCustomNumberFormat.IsText: Boolean;
begin
  Result := ValueType = nftText;
end;

procedure TdxSpreadSheetCustomNumberFormat.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType; ADateTimeSystem: TdxSpreadSheetDateTimeSystem;
  const AFormatSettings: TFormatSettings; var AResult: TdxSpreadSheetNumberFormatResult);
var
  AFormat: TdxSpreadSheetCustomFormatSettings;
begin
  AFormat := TdxSpreadSheetCustomFormatSettings.Create;
  try
    AFormat.Data := AFormatSettings;
    AFormat.DateTimeSystem := ADateTimeSystem;
    Format(AValue, AValueType, AFormat, AResult);
  finally
    AFormat.Free;
  end;
end;

{ TdxSpreadSheetNumberFormat }

constructor TdxSpreadSheetNumberFormat.Create(const AFormatCode: string; AFormatCodeId: Integer);
begin
  inherited Create;
  FFormat := TdxNumberFormatParser.Parse(AFormatCode, AFormatCodeId);
  if FFormat = nil then
    FFormat := TdxGeneralNumberFormat.Create;
end;

destructor TdxSpreadSheetNumberFormat.Destroy;
begin
  FreeAndNil(FFormat);
  inherited Destroy;
end;

procedure TdxSpreadSheetNumberFormat.Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; var AResult: TdxSpreadSheetNumberFormatResult);
var
  AResultBuffer: TdxNumberFormatResult;
begin
  AResult.Reset;
  try
    AResultBuffer := TdxNumberFormatResult.Create;
    try
      if AValueType = cdtFormula then
        AValueType := dxGetDataTypeByVariantValue(AValue);

      FFormat.Format(AValue, AValueType, AFormatSettings, AResultBuffer);

      AResult.Color := AResultBuffer.Color;
      AResult.IsError := AResultBuffer.IsError;
      AResult.IsText := AResultBuffer.IsText;
      AResult.Text := AResultBuffer.ToString;
      if IsText then
        AResult.IsText := bTrue;
    finally
      AResultBuffer.Free;
    end;
  except
    AResult.IsError := True;
  end;
end;

function TdxSpreadSheetNumberFormat.GetValueType: TdxNumberFormatType;
begin
  Result := FFormat.ValueType;
end;

function TdxSpreadSheetNumberFormat.IsTime: Boolean;
begin
  Result := FFormat.IsTimeFormat;
end;

{ TdxSpreadSheetCurrencyFormatHelper }

class function TdxSpreadSheetCurrencyFormatHelper.GetFormat(
  ASettings: TdxSpreadSheetFormatSettings; const APattern: string): string;
begin
  Result :=
    GetPositiveFormat(ASettings.CurrencyFormat, APattern, ASettings.Data.CurrencyFormat, True) + ';[Red]' +
    GetNegativeFormat(ASettings.CurrencyFormat, APattern, ASettings.Data.NegCurrFormat, True);
end;

class function TdxSpreadSheetCurrencyFormatHelper.GetNegativeFormat(
  ACurrency: string; const APattern: string; AFormatCode: Byte; AIncludeCurrencyIntoMarkChar: Boolean): string;

  function Make(const APrefix, ACurrency, APattern, ASuffix: string): string;
  begin
    if APattern[1] = '*' then
      Result := ACurrency + Copy(APattern, 1, 2) + APrefix + Copy(APattern, 3, MaxInt) + ASuffix
    else
      Result := APrefix + ACurrency + APattern + ASuffix;
  end;

begin
  if AIncludeCurrencyIntoMarkChar and (ACurrency <> '') then
    ACurrency := dxStringMarkChar + ACurrency + dxStringMarkChar;

  case AFormatCode of
     0:
       Result := Make('(', ACurrency, APattern, ')');
     1:
       Result := Make('-', ACurrency, APattern, '');
     2:
       Result := ACurrency + '-' + APattern;
     3:
       Result := ACurrency + APattern + '-';
     4:
       Result := '(' + APattern + ACurrency + ')';
     5:
       Result := '-' + APattern + ACurrency;
     6:
       Result := APattern + '-' + ACurrency;
     7:
       Result := APattern + ACurrency + '-';
     8:
       Result := '-' + APattern + ' ' + ACurrency;
     9:
       Result := '-' + ACurrency + ' ' + APattern;
    10:
       Result := APattern + ' ' + ACurrency + '-';
    11:
       Result := ACurrency + ' ' + APattern + '-';
    12:
       Result := ACurrency + ' ' + '-' + APattern;
    13:
       Result := APattern + '-' + ' ' + ACurrency;
    14:
       Result := Make('(', ACurrency + ' ', APattern, ')');
  else
       Result := '(' + APattern + ' ' + ACurrency + ')';
  end;
end;

class function TdxSpreadSheetCurrencyFormatHelper.GetPositiveFormat(
  ACurrency: string; const APattern: string; AFormatCode: Byte; AIncludeCurrencyIntoMarkChar: Boolean): string;
begin
  if AIncludeCurrencyIntoMarkChar and (ACurrency <> '') then
    ACurrency := dxStringMarkChar + ACurrency + dxStringMarkChar;

  case AFormatCode of
    0:
      Result := ACurrency + APattern;
    1:
      Result := APattern + ACurrency;
    2:
      Result := ACurrency + ' ' + APattern;
  else
    Result := APattern + ' ' + ACurrency;
  end
end;

class function TdxSpreadSheetCurrencyFormatHelper.IsCurrencySymbolFollowsValue(ANegFormatCode: Integer): Boolean;
begin
  Result := not (ANegFormatCode in [0..3, 9, 11, 12, 14]);
end;

{ TdxSpreadSheetNumberFormatSplitter }

class procedure TdxSpreadSheetNumberFormatSplitter.Split(const AFormatCode: string; AList: TStringList);
var
  AChar: Char;
  AHasBackSlash: Boolean;
  AHasQuotationMark: Boolean;
  ATemp: string;
  I: Integer;
begin
  if AFormatCode <> '' then
  begin
    AHasBackSlash := False;
    AHasQuotationMark := False;
    for I := 1 to Length(AFormatCode) do
    begin
      AChar := AFormatCode[I];
      case AChar of
        '"':
          if AHasBackSlash then
            AHasBackSlash := False
          else
            AHasQuotationMark := not AHasQuotationMark;

        '\':
          if not AHasQuotationMark then
            AHasBackSlash := not AHasBackSlash;

        ';':
          if AHasBackSlash then
            AHasBackSlash := False
          else
            if not AHasQuotationMark then
            begin
              AList.Add(ATemp);
              ATemp := EmptyStr;
              Continue;
            end;
      else
        AHasBackSlash := False;
      end;
      ATemp := ATemp + AChar;
    end;
    AList.Add(ATemp);
  end;
end;

{ TdxSpreadSheetDateTimeFormatHelper }

class function TdxSpreadSheetDateTimeFormatHelper.GetDefaultFormat(const AValue: TDateTime): string;
begin
  if Trunc(AValue) = 0 then
    Result := dxFormatSettings.ShortTimeFormat
  else
    if Frac(AValue) = 0 then
      Result := dxFormatSettings.ShortDateFormat
    else
      Result := 'C';

  Result := TdxSpreadSheetDisplayFormatConverter.ConvertDateTimeDisplayFormat(Result, AValue);
end;

{ TdxSpreadSheetDisplayFormatConverter }

class function TdxSpreadSheetDisplayFormatConverter.ConvertDateTimeDisplayFormat(
  const S: string; const AValue: TDateTime): string;

  procedure ProcessSequence(var ASequenceSize, AIndex: Integer; ALength: Integer);
  var
    AChar: Char;
  begin
    ASequenceSize := 1;
    AChar := S[AIndex];
    Inc(AIndex);
    while (AIndex <= ALength) and (S[AIndex] = AChar) do
    begin
      Inc(ASequenceSize);
      Inc(AIndex);
    end;
  end;

var
  ABuilder: TStringBuilder;
  ACount: Integer;
  AIndex: Integer;
  ALength: Integer;
begin
  Result := StringReplace(S, 'AMPM', 'AM/PM', [rfReplaceAll, rfIgnoreCase]);
  ALength := Length(Result);
  ABuilder := TdxStringBuilderManager.Get(ALength);
  try
    AIndex := 1;
    while AIndex <= ALength do
    begin
      case Result[AIndex] of
        'd', 'D':
          begin
            ProcessSequence(ACount, AIndex, ALength);
            case ACount of
              1..4:
                ABuilder.Append(DupeString('d', ACount));
              5:
                ABuilder.Append(ConvertDateTimeDisplayFormat(dxFormatSettings.ShortDateFormat, AValue));
            else
              ABuilder.Append(ConvertDateTimeDisplayFormat(dxFormatSettings.LongDateFormat, AValue));
            end;
          end;

        'C':
          begin
            ProcessSequence(ACount, AIndex, ALength);
            ABuilder.Append(ConvertDateTimeDisplayFormat(dxFormatSettings.ShortDateFormat, AValue));
            if Frac(AValue) > 0 then
            begin
              ABuilder.Append(' ');
              ABuilder.Append(ConvertDateTimeDisplayFormat(dxFormatSettings.LongTimeFormat, AValue));
            end;
          end;

        'N', 'n':
          begin
            ABuilder.Append('m');
            Inc(AIndex);
          end;

        'Z', 'z':
          begin
            ProcessSequence(ACount, AIndex, ALength);
            if (ABuilder.Length > 0) and not dxCharIsNumeric(ABuilder.Chars[ABuilder.Length - 1]) then
              ABuilder.Length := ABuilder.Length - 1;
            ABuilder.Append('.');
            ABuilder.Append(DupeString('0', ACount));
          end;

        'T', 't':
          begin
            ProcessSequence(ACount, AIndex, ALength);
            if ACount = 1 then
              ABuilder.Append('h:mm')
            else
              ABuilder.Append('h:mm:ss');
          end;

      else
        begin
          ABuilder.Append(Result[AIndex]);
          Inc(AIndex);
        end;
      end;
    end;
    Result := ABuilder.ToString;
  finally
    TdxStringBuilderManager.Release(ABuilder);
  end;
end;

class function TdxSpreadSheetDisplayFormatConverter.ConvertFloatValueDisplayFormat(const S: string): string;
var
  AList: TStringList;
  AResult: TStringBuilder;
  I: Integer;
begin
  AList := TStringList.Create;
  try
    TdxSpreadSheetNumberFormatSplitter.Split(S, AList);

    AResult := TdxStringBuilderManager.Get(Length(S));
    try
      for I := 0 to AList.Count - 1 do
      begin
        if AResult.Length > 0 then
          AResult.Append(';');
        AResult.Append(ConvertFloatValueDisplayFormatSection(AList[I]));
      end;
      Result := AResult.ToString;
    finally
      TdxStringBuilderManager.Release(AResult);
    end;
  finally
    AList.Free;
  end;
end;

class function TdxSpreadSheetDisplayFormatConverter.ConvertFloatValueDisplayFormatSection(const S: string): string;
var
  ABuilder: TStringBuilder;
  AIndex, I: Integer;
  ALength: Integer;
begin
  AIndex := 1;
  ALength := Length(S);
  ABuilder := TdxStringBuilderManager.Get(ALength);
  try
    while AIndex <= ALength do
    begin
      case S[AIndex] of
        '%':
          ABuilder.Append('\%');

        '#', '0', ' ', 'E', 'e', '-', '+', '.':
          ABuilder.Append(S[AIndex]);

        '"', '''':
          begin
            I := PosEx(S[AIndex], S, AIndex + 1);
            if I = 0 then
              I := AIndex;
            ABuilder.Append('"');
            ABuilder.Append(Copy(S, AIndex + 1, I - AIndex - 1));
            ABuilder.Append('"');
            AIndex := I;
          end;

        ',':
          if (ABuilder.Length > 0) and (ABuilder[ABuilder.Length - 1] = '.') then
          begin
            ABuilder[ABuilder.Length - 1] := ',';
            CorrectDigitsPlaces(ABuilder);
            ABuilder.Append('.');
          end
          else
          begin
            ABuilder.Append(',');
            Inc(AIndex);
            while (AIndex <= ALength) and IsDigitPlaceHolder(S[AIndex]) do
            begin
              ABuilder.Append(S[AIndex]);
              Inc(AIndex);
            end;
            CorrectDigitsPlaces(ABuilder);
            Continue;
          end;

      else
        begin
          I := AIndex + 1;
          while (I <= ALength) and not dxCharInSet(S[I], ['0', '#', '''', '"']) do
          begin
            if dxCharInSet(S[I], [',', '.']) then
            begin
              if (I < ALength) and IsDigitPlaceHolder(S[I + 1]) then
                Break;
            end;
            Inc(I);
          end;

          if I - AIndex = 1 then
          begin
            ABuilder.Append('\');
            ABuilder.Append(S[AIndex]);
          end
          else
          begin
            ABuilder.Append('"');
            ABuilder.Append(Copy(S, AIndex, I - AIndex));
            ABuilder.Append('"');
          end;

          AIndex := I;
          Continue;
        end;
      end;
      Inc(AIndex);
    end;
    Result := ABuilder.ToString;
  finally
    TdxStringBuilderManager.Release(ABuilder);
  end;
end;

class procedure TdxSpreadSheetDisplayFormatConverter.CorrectDigitsPlaces(AData: TStringBuilder);
var
  ADigitCount: Integer;
  AIndex: Integer;
begin
  AIndex := AData.Length - 1;

  ADigitCount := 0;
  while (AIndex >= 0) and IsDigitPlaceHolder(AData[AIndex]) do
  begin
    Inc(ADigitCount);
    Dec(AIndex);
  end;

  if (AIndex >= 0) and (AData[AIndex] = ',') then
  begin
    while (AIndex > 0) and (ADigitCount < 3) and IsDigitPlaceHolder(AData[AIndex - 1]) do
    begin
      AData[AIndex] := AData[AIndex - 1];
      AData[AIndex - 1] := ',';
      Inc(ADigitCount);
      Dec(AIndex);
    end;
    while ADigitCount < 3 do
    begin
      AData.Insert(AIndex + 1, '#');
      Inc(ADigitCount);
    end;
    if (AIndex = 0) or not IsDigitPlaceHolder(AData[AIndex - 1]) then
      AData.Insert(AIndex, '#');
  end;
end;

class function TdxSpreadSheetDisplayFormatConverter.IsDigitPlaceHolder(const C: Char): Boolean;
begin
  Result := dxCharInSet(C, ['0', '#']);
end;

end.

