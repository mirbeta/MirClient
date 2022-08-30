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

unit dxSpreadSheetNumberFormatParser;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Character, Generics.Collections, Generics.Defaults, Graphics, Variants, SysUtils, Math, StrUtils,
  dxCore, dxCoreGraphics, dxCultureInfo, cxGraphics, dxSpreadSheetNumberFormatCore;

type

  { TdxNumberFormatDesignatorMap }

  TdxNumberFormatDesignatorMap = class(TDictionary<Char, Integer>);

  { TdxNumberFormatLocalizer }

  TdxNumberFormatLocalizer = class
  strict private
    FChars: TDictionary<Integer, Char>;
    FCulture: TdxCultureInfo;
    FDesignators: TdxNumberFormatDesignatorMap;
    FGeneralDesignator: string;
  protected
    class procedure GenerateInvariant(ADesignators: TdxNumberFormatDesignatorMap);
  public
    constructor Create(ACulture: TdxCultureInfo);
    destructor Destroy; override;
    procedure AddSeparator(ASymbol: Char; ADesignator: Integer);
    procedure GenerateDesignators(ACulture: TdxCultureInfo);

    property Chars: TDictionary<Integer, Char> read FChars;
    property Culture: TdxCultureInfo read FCulture;
    property Designators: TdxNumberFormatDesignatorMap read FDesignators;
    property GeneralDesignator: string read FGeneralDesignator;
  end;

  TdxNumberFormatParser = class
  strict private type
    TdxNumberFormatDesignatorCheckResult = (crSucceed, crError, crContinue);
    TdxNumberFormatDesignatorCheckMethod = reference to function (AChar: Char): TdxNumberFormatDesignatorCheckResult;
    TdxNumberFormatDesignatorParseMethod = procedure of object;
    TdxNumberFormatDesignatorParseSeparatorMethod = function (ADesignator: Integer): Integer of object;

  strict private
    FFormatString: string;
    FLocalizer: TdxNumberFormatLocalizer;

    FParseMethods1: TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>;
    FParseMethods2: TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>;
    FParseMethods3: TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>;
    FParseMethods4: TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>;
    FParseMethods5: TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>;
    FParseMethods6: TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>;
    FParseMethods7: TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>;

  {$REGION 'Internal State'}
    FCurrentIndex: Integer;
    FCurrentSymbol: Char;
    FDesignator: Integer;
    FDesignatorParser: TdxNumberFormatDesignatorParseMethod;
    FElements: TdxNumberFormatElementList;
    FErrorState: Boolean;
    FFormats: TdxSimpleNumberFormatList;
    FLocale: TdxNumberFormatElement;
    FPart: TdxSimpleNumberFormat;

    //numeric
    FDecimalCount: Integer;
    FDecimalSeparatorIndex: Integer;
    FDisplayFactor: Integer;
    FDividendCount: Integer;
    FGrouping: Boolean;
    FIntegerCount: Integer;
    FIsDecimal: Boolean;
    FPercentCount: Integer;
    FPreFractionIndex: Integer;

    //dateTime
    FElapsed: Boolean;
    FElementLength: Integer;
    FHasMilliseconds: Boolean;

    //exp
    FExpCount: Integer;
    FExpIndex: Integer;
    FExplicitSign: Boolean;

    //frac
    FDivisor: Integer;
    FDivisorCount: Integer;
    FDivisorPow: Integer;
    FFractionSeparatorIndex: Integer;
  {$ENDREGION}

    procedure PopulateParseMethods1;
    procedure PopulateParseMethods2;
    procedure PopulateParseMethods3;
    procedure PopulateParseMethods4;
    procedure PopulateParseMethods5;
    procedure PopulateParseMethods6;
    procedure PopulateParseMethods7;
  protected
    function ParseCore: TdxNumberFormat;
    function ParseSeparator(ADesignator: Integer; ASeparators: array of Integer): Integer;
    function ParseSeparator1(ADesignator: Integer): Integer;
    procedure ParseSeparator2(ADesignator: Integer);
    function ParseSeparator3(ADesignator: Integer): Integer;
    function ParseSeparator6(ADesignator: Integer): Integer;
    function ParseSeparator7(ADesignator: Integer): Integer;
    procedure ParseGeneral;
    procedure ParseNumericExponent(AIntegerCount, ADecimalSeparatorIndex, ADecimalCount: Integer; AGrouping: Boolean);
    procedure ParseNumericFraction;
    procedure ParseText;

    procedure BeforeDigit;
    function CheckIsGeneral: Boolean;
    procedure PrepareGrouping;

    function ProcessDesignator(AChar: Char;
      AMethods: TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>;
      ASeparatorParser: TdxNumberFormatDesignatorParseSeparatorMethod;
      ACheckDesignator: TdxNumberFormatDesignatorCheckMethod = nil): Boolean;
    procedure ProcessSequence(
      AMethods: TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>;
      ASeparatorParser: TdxNumberFormatDesignatorParseSeparatorMethod;
      ACheckDesignator: TdxNumberFormatDesignatorCheckMethod = nil);

    function GetDateTimeBlockLength: Integer; overload;
    function GetDateTimeBlockLength(const AFormatString: string; ACurrentIndex: Integer): Integer; overload;

    function TryGetConditionString: string;
    function TryParseColor(const S: string): TdxNumberFormatElement;
    function TryParseCondition: Integer;
    function TryParseDateTimeCondition(var AElapsed: Boolean): Integer;
    function TryParseElapsed(const S: string): TdxNumberFormatElement;
    function TryParseExpr(const S: string): TdxNumberFormatElement;
    function TryParseLocale(const S: string): TdxNumberFormatElement;

    function OnAmPmCore: Boolean;
    function OnDayOfWeekCore: Boolean;
    function OnMillisecondCore: Boolean;

    procedure OnAmPmOrDayOfWeek;
    procedure OnAmPmOrMonth;
    procedure OnAmPmOrYear;
    procedure OnAsterisk;
    procedure OnAt;
    procedure OnAt6;
    procedure OnBackslash;
    procedure OnBracket;
    procedure OnBracket2;
    procedure OnBracket3;
    procedure OnDateOrTimeSeparator;
    procedure OnDateSeparator;
    procedure OnDateTimeSymbol;
    procedure OnDay;
    procedure OnDayOfWeekOrDefault;
    procedure OnDayOfWeekOrDefault2;
    procedure OnDayOfWeekOrDefault3;
    procedure OnDecimalSeparator;
    procedure OnDefault;
    procedure OnDefaultDateSeparator;
    procedure OnDigitEmpty;
    procedure OnDigitEmpty4;
    procedure OnDigitEmpty5;
    procedure OnDigitSpace;
    procedure OnDigitSpace4;
    procedure OnDigitSpace5;
    procedure OnDigitZero;
    procedure OnDigitZero4;
    procedure OnDigitZero5;
    procedure OnEndOfPart;
    procedure OnEndOfPart2;
    procedure OnEndOfPart3;
    procedure OnEndOfPart4;
    procedure OnEndOfPart5;
    procedure OnEndOfPart6;
    procedure OnEndOfPart7;
    procedure OnError;
    procedure OnESymbol;
    procedure OnESymbol2;
    procedure OnESymbol3;
    procedure OnExponent;
    procedure OnFractionSeparator;
    procedure OnGeneral;
    procedure OnGeneral2;
    procedure OnGeneral3;
    procedure OnGeneral7;
    procedure OnGeneralCore;
    procedure OnGeneralOrDateTime;
    procedure OnGeneralOrDateTime7;
    procedure OnGeneralOrDay;
    procedure OnGeneralOrInvariantYear;
    procedure OnGeneralOrJapaneseEra;
    procedure OnGeneralOrSecond;
    procedure OnGroupSeparator;
    procedure OnHour;
    procedure OnInvariantYear;
    procedure OnJapaneseEra;
    procedure OnMillisecond;
    procedure OnMinute;
    procedure OnMonth;
    procedure OnMonthOrMinute;
    procedure OnNumericSymbol;
    procedure OnPercent;
    procedure OnQuote;
    procedure OnSecond;
    procedure OnThaiYear;
    procedure OnTimeSeparator;
    procedure OnUnderline;
    procedure OnYear;

    property FormatString: string read FFormatString;
    property Localizer: TdxNumberFormatLocalizer read FLocalizer;
  public
    constructor Create;
    destructor Destroy; override;
    class function Parse(const S: string; ID: Integer = -1): TdxNumberFormat;
  end;

implementation

uses
  dxSpreadSheetTypes;

{ TdxNumberFormatLocalizer }

constructor TdxNumberFormatLocalizer.Create(ACulture: TdxCultureInfo);

  procedure CheckAddChar(ADesignator, AFlag: Integer; AKey: Char);
  begin
    if ADesignator and AFlag > 0 then
      FChars.Add(AFlag, AKey);
  end;

var
  ADesignator: Integer;
  AKey: Char;
  APrevCount: Integer;
begin
  FCulture := ACulture;
  FChars := TDictionary<Integer, Char>.Create;
  FDesignators := TdxNumberFormatDesignatorMap.Create;

  GenerateDesignators(ACulture);

  for AKey in FDesignators.Keys do
  begin
    APrevCount := FChars.Count;
    ADesignator := FDesignators[AKey];
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.AmPm, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.Year, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.InvariantYear, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.Month, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.Minute, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.DateSeparator, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.DecimalSeparator, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.FractionOrDateSeparator, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.GroupSeparator, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.TimeSeparator, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.Day, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.Second, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.General, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.JapaneseEra, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.DayOfWeek, AKey);
    CheckAddChar(ADesignator, TdxNumberFormatDesignator.Hour, AKey);
    if FChars.Count = APrevCount then
      FChars.Add(ADesignator, AKey);
  end;

  FGeneralDesignator := 'General';
end;

destructor TdxNumberFormatLocalizer.Destroy;
begin
  FreeAndNil(FDesignators);
  FreeAndNil(FChars);
  inherited Destroy;
end;

procedure TdxNumberFormatLocalizer.GenerateDesignators(ACulture: TdxCultureInfo);
var
  ADateSeparator: Char;
  ADecimalSeparator: Char;
  AGroupSeparator: Char;
  ATimeSeparator: Char;
begin
  ADecimalSeparator := ACulture.FormatSettings.DecimalSeparator;
  AGroupSeparator := ACulture.FormatSettings.ThousandSeparator;
  ADateSeparator := ACulture.FormatSettings.DateSeparator;

  if AGroupSeparator = ADecimalSeparator then
  begin
    if ADecimalSeparator = '.' then
      AGroupSeparator := ',';
    if ADecimalSeparator = ',' then
      AGroupSeparator := '.';
  end;

  ATimeSeparator := ACulture.FormatSettings.TimeSeparator;
  if dxIsWhiteSpace(AGroupSeparator) then
    AGroupSeparator := ' ';

  AddSeparator(ADecimalSeparator, TdxNumberFormatDesignator.DecimalSeparator);
  AddSeparator(ADateSeparator, TdxNumberFormatDesignator.DateSeparator);
  AddSeparator(AGroupSeparator, TdxNumberFormatDesignator.GroupSeparator);
  AddSeparator(ATimeSeparator, TdxNumberFormatDesignator.TimeSeparator);
  AddSeparator('/', TdxNumberFormatDesignator.FractionOrDateSeparator);

  FDesignators.Add('a', TdxNumberFormatDesignator.AmPm);
  FDesignators.Add('*', TdxNumberFormatDesignator.Asterisk);
  FDesignators.Add('@', TdxNumberFormatDesignator.At);
  FDesignators.Add('\', TdxNumberFormatDesignator.Backslash);
  FDesignators.Add('[', TdxNumberFormatDesignator.Bracket);
  FDesignators.Add('#', TdxNumberFormatDesignator.DigitEmpty);
  FDesignators.Add('?', TdxNumberFormatDesignator.DigitSpace);
  FDesignators.Add('0', TdxNumberFormatDesignator.DigitZero);
  FDesignators.Add(';', TdxNumberFormatDesignator.EndOfPart);
  FDesignators.Add('E', TdxNumberFormatDesignator.Exponent);
  FDesignators.Add('e', TdxNumberFormatDesignator.InvariantYear);
  FDesignators.Add('%', TdxNumberFormatDesignator.Percent);
  FDesignators.Add('"', TdxNumberFormatDesignator.Quote);
  FDesignators.Add('b', TdxNumberFormatDesignator.ThaiYear);
  FDesignators.Add('_', TdxNumberFormatDesignator.Underline);
  GenerateInvariant(FDesignators);
end;

procedure TdxNumberFormatLocalizer.AddSeparator(ASymbol: Char; ADesignator: Integer);
begin
  if FDesignators.ContainsKey(ASymbol) then
    FDesignators[ASymbol] := FDesignators[ASymbol] or ADesignator
  else
    FDesignators.Add(ASymbol, ADesignator);
end;

class procedure TdxNumberFormatLocalizer.GenerateInvariant(ADesignators: TdxNumberFormatDesignatorMap);
begin
  ADesignators['a'] := ADesignators['a'] or TdxNumberFormatDesignator.DayOfWeek;
  ADesignators.Add('d', TdxNumberFormatDesignator.Day);
  ADesignators.Add('h', TdxNumberFormatDesignator.Hour);
  ADesignators.Add('m', TdxNumberFormatDesignator.Minute or TdxNumberFormatDesignator.Month);
  ADesignators.Add('s', TdxNumberFormatDesignator.Second);
  ADesignators.Add('y', TdxNumberFormatDesignator.Year);
  ADesignators.Add('g', TdxNumberFormatDesignator.JapaneseEra or TdxNumberFormatDesignator.General);
end;

{ TdxNumberFormatParser }

constructor TdxNumberFormatParser.Create;
begin
  FFormats := TdxSimpleNumberFormatList.Create;
  FElements := TdxNumberFormatElementList.Create;
  FLocalizer := TdxNumberFormatLocalizer.Create(TdxCultureInfo.InvariantCulture);

  PopulateParseMethods1;
  PopulateParseMethods2;
  PopulateParseMethods3;
  PopulateParseMethods4;
  PopulateParseMethods5;
  PopulateParseMethods6;
  PopulateParseMethods7;

  FDecimalSeparatorIndex := -1;
  FExpIndex := -1;
  FExplicitSign := False;
  FFractionSeparatorIndex := -1;
  FPreFractionIndex := -1;
end;

destructor TdxNumberFormatParser.Destroy;
begin
  FreeAndNil(FParseMethods1);
  FreeAndNil(FParseMethods2);
  FreeAndNil(FParseMethods3);
  FreeAndNil(FParseMethods4);
  FreeAndNil(FParseMethods5);
  FreeAndNil(FParseMethods6);
  FreeAndNil(FParseMethods7);
  FreeAndNil(FLocalizer);
  FreeAndNil(FElements);
  FreeAndNil(FFormats);
  inherited Destroy;
end;

class function TdxNumberFormatParser.Parse(const S: string; ID: Integer): TdxNumberFormat;
begin
  if S = '' then
    Exit(TdxGeneralNumberFormat.Create);
  if ID = 14 then
    Exit(TdxDateTimeSystemShortDateNumberFormat.Create(nil));

  with TdxNumberFormatParser.Create do
  try
    FFormatString := S;
    Result := ParseCore;
  finally
    Free;
  end;
end;

function TdxNumberFormatParser.ParseCore: TdxNumberFormat;
var
  AIndex: Integer;
begin
  FFormatString := FFormatString + ';';
  FCurrentIndex := 1;
  while FCurrentIndex <= Length(FormatString) do
  begin
    FCurrentSymbol := FormatString[FCurrentIndex];
    ProcessDesignator(FCurrentSymbol, FParseMethods1, ParseSeparator1);
    Inc(FCurrentIndex);

    if FErrorState then
      Exit(nil);
    if FPart = nil then
      Continue;

    FFormats.Add(FPart);
    FPart := nil;
    FElements.Clear;

    if FFormats.Count = 3 then
    begin
      AIndex := FCurrentIndex;
      if FCurrentIndex > Length(FormatString) then
        Break;

      ParseText;
      if FErrorState then
      begin
        FErrorState := False;
        FElements.Clear;
        FCurrentIndex := AIndex;
        ParseGeneral;
        if FErrorState then
          Exit(nil);
      end;
      if FCurrentIndex < Length(FormatString) - 1 then // formats.Count > 4
        Exit(nil);
      if (FPart.ValueType <> nftGeneral) or (FPart.List.Count > 0) then
        FFormats.Add(FPart);
      Break;
    end;
  end;

  if FFormats.Count = 1 then
    Result := FFormats.Extract(FFormats.First)
  else
    Result := TdxConditionalNumberFormat.Create(FFormats);
end;

procedure TdxNumberFormatParser.ParseGeneral;
begin
  ProcessSequence(FParseMethods7, ParseSeparator7);
end;

procedure TdxNumberFormatParser.ParseNumericExponent(
  AIntegerCount, ADecimalSeparatorIndex, ADecimalCount: Integer; AGrouping: Boolean);
begin
  Inc(FCurrentIndex);
  if FCurrentIndex >= Length(FormatString) then
    Exit;

  FCurrentSymbol := FormatString[FCurrentIndex];
  if FCurrentSymbol = '+' then
    FExplicitSign := True
  else
  begin
    FExplicitSign := False;
    if FCurrentSymbol <> '-' then
      Exit;
  end;

  FExpIndex := FElements.Count;
  FExpCount := 0;

  ProcessSequence(FParseMethods4, ParseSeparator3);
end;

procedure TdxNumberFormatParser.ParseNumericFraction;
begin
  FDisplayFactor := -1;
  FDivisorCount := 0;
  FDivisor := 0;
  FDivisorPow := 10000;

  Inc(FCurrentIndex);
  FFractionSeparatorIndex := FElements.Count;
  ProcessSequence(FParseMethods5, ParseSeparator3,
    function (AChar: Char): TdxNumberFormatDesignatorCheckResult
    begin
      Result := crSucceed;
      if dxCharIsNumeric(AChar) then
      begin
        if FDivisorPow <= 0 then
          Exit(crError);
        Inc(FDivisor, (Ord(AChar) - Ord('0')) * FDivisorPow);
        FDivisorPow := FDivisorPow div 10;
        FDisplayFactor := FElements.Count;
        Result := crContinue;
      end;
    end);
end;

procedure TdxNumberFormatParser.ParseText;
begin
  ProcessSequence(FParseMethods6, ParseSeparator6,
    function (AChar: Char): TdxNumberFormatDesignatorCheckResult
    begin
      if dxCharIsNumeric(AChar) then
        Result := crError
      else
        Result := crSucceed;
    end);
end;

function TdxNumberFormatParser.ParseSeparator(ADesignator: Integer; ASeparators: array of Integer): Integer;
var
  I: Integer;
begin
  for I := Low(ASeparators) to High(ASeparators) do
  begin
    if ADesignator and ASeparators[I] <> 0 then
      Exit(ASeparators[I]);
  end;
  Result := TdxNumberFormatDesignator.Default;
end;

function TdxNumberFormatParser.ParseSeparator1(ADesignator: Integer): Integer;
begin
  Result := ParseSeparator(ADesignator, [
    TdxNumberFormatDesignator.DateSeparator,
    TdxNumberFormatDesignator.TimeSeparator,
    TdxNumberFormatDesignator.DecimalSeparator,
    TdxNumberFormatDesignator.GroupSeparator,
    TdxNumberFormatDesignator.FractionOrDateSeparator]);
end;

procedure TdxNumberFormatParser.ParseSeparator2(ADesignator: Integer);
begin
  if (ADesignator and TdxNumberFormatDesignator.DecimalSeparator <> 0) and OnMillisecondCore then
    Exit;

  if ADesignator and TdxNumberFormatDesignator.DateSeparator <> 0 then
  begin
    if ADesignator and TdxNumberFormatDesignator.TimeSeparator <> 0 then
      OnDateOrTimeSeparator
    else
      OnDateSeparator;
  end
  else

  if ADesignator and TdxNumberFormatDesignator.FractionOrDateSeparator <> 0 then
    OnDefaultDateSeparator()
  else

  if ADesignator and TdxNumberFormatDesignator.TimeSeparator <> 0 then
    OnTimeSeparator()
  else

  if ADesignator and TdxNumberFormatDesignator.GroupSeparator <> 0 then
    OnDefault()
  else
    OnError();
end;

function TdxNumberFormatParser.ParseSeparator3(ADesignator: Integer): Integer;
begin
  Result := ParseSeparator(ADesignator, [
    TdxNumberFormatDesignator.DecimalSeparator,
    TdxNumberFormatDesignator.GroupSeparator,
    TdxNumberFormatDesignator.FractionOrDateSeparator,
    TdxNumberFormatDesignator.DateSeparator,
    TdxNumberFormatDesignator.TimeSeparator]);
end;

function TdxNumberFormatParser.ParseSeparator6(ADesignator: Integer): Integer;
begin
  Result := ParseSeparator(ADesignator, [
    TdxNumberFormatDesignator.FractionOrDateSeparator,
    TdxNumberFormatDesignator.DecimalSeparator,
    TdxNumberFormatDesignator.GroupSeparator,
    TdxNumberFormatDesignator.DateSeparator,
    TdxNumberFormatDesignator.TimeSeparator]);
end;

function TdxNumberFormatParser.ParseSeparator7(ADesignator: Integer): Integer;
begin
  Result := TdxNumberFormatDesignator.Default;
end;

procedure TdxNumberFormatParser.BeforeDigit;
begin
  if FElements.Count > 0 then
  begin
    if FElements.Last.IsDigit then
    begin
      if FDisplayFactor = 1 then
        FGrouping := True;
    end
    else
    begin
      FPreFractionIndex := FElements.Count - 1;
      Inc(FIntegerCount, FDividendCount);
      FDividendCount := 0;
    end;
  end;

  FDisplayFactor := 0;
  if FIsDecimal then
    Inc(FDecimalCount)
  else
    Inc(FDividendCount);
end;

function TdxNumberFormatParser.CheckIsGeneral: Boolean;
begin
  Result := SameText(Copy(FormatString, FCurrentIndex, Length(Localizer.GeneralDesignator)), Localizer.GeneralDesignator);
end;

procedure TdxNumberFormatParser.PrepareGrouping;
var
  APrevIntegerCount: Integer;
  I: Integer;
begin
  APrevIntegerCount := FIntegerCount;
  if FGrouping and (FIntegerCount < 4) then
  begin
    for I := 0 to FElements.Count - 1 do
    begin
      if FElements[I].IsDigit then
        Break;
    end;
    while FIntegerCount < 4 do
    begin
      FElements.Insert(I, TdxNumberFormatElementDigitEmpty.Create);
      Inc(FIntegerCount);
    end;
    if FDecimalSeparatorIndex >= 0 then
      FDecimalSeparatorIndex := FDecimalSeparatorIndex + FIntegerCount - APrevIntegerCount;
  end;
end;

function TdxNumberFormatParser.ProcessDesignator(AChar: Char;
  AMethods: TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>;
  ASeparatorParser: TdxNumberFormatDesignatorParseSeparatorMethod;
  ACheckDesignator: TdxNumberFormatDesignatorCheckMethod = nil): Boolean;
begin
  if not Localizer.Designators.TryGetValue(dxLowerCase(AChar), FDesignator) then
  begin
    if Assigned(ACheckDesignator) then
      case ACheckDesignator(AChar) of
        crContinue:
          Exit(True);
        crError:
          begin
            FErrorState := True;
            FreeAndNil(FPart);
            Exit(False);
          end;
      end;

    FDesignator := TdxNumberFormatDesignator.Default;
  end;
  if not AMethods.TryGetValue(FDesignator, FDesignatorParser) then
  begin
    FDesignator := ASeparatorParser(FDesignator);
    FDesignatorParser := AMethods[FDesignator];
  end;
  FDesignatorParser();
  Result := True;
end;

procedure TdxNumberFormatParser.ProcessSequence(
  AMethods: TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>;
  ASeparatorParser: TdxNumberFormatDesignatorParseSeparatorMethod;
  ACheckDesignator: TdxNumberFormatDesignatorCheckMethod = nil);
begin
  while FCurrentIndex <= Length(FormatString) do
  begin
    FCurrentSymbol := FormatString[FCurrentIndex];
    if not ProcessDesignator(FCurrentSymbol, AMethods, ASeparatorParser, ACheckDesignator) or FErrorState then
    begin
      FreeAndNil(FPart);
      Exit;
    end;
    if FCurrentSymbol = ';' then
      Exit;
    Inc(FCurrentIndex);
  end;
  if FPart = nil then
    FErrorState := True;
end;

function TdxNumberFormatParser.GetDateTimeBlockLength: Integer;
begin
  Result := GetDateTimeBlockLength(FormatString, FCurrentIndex);
  Inc(FCurrentIndex, Result - 1);
end;

function TdxNumberFormatParser.GetDateTimeBlockLength(const AFormatString: string; ACurrentIndex: Integer): Integer;
var
  AStartIndex: Integer;
begin
  AStartIndex := ACurrentIndex;
  FCurrentSymbol := dxLowerCase(FCurrentSymbol);
  while ACurrentIndex <= Length(AFormatString) do
  begin
    if dxLowerCase(AFormatString[ACurrentIndex]) <> FCurrentSymbol then
    begin
      Dec(ACurrentIndex);
      Exit(ACurrentIndex - AStartIndex + 1);
    end;
    Inc(ACurrentIndex);
  end;
  Result := Length(AFormatString) - AStartIndex + 1;
end;

function TdxNumberFormatParser.TryGetConditionString: string;
var
  ACloseBracketIndex: Integer;
  AIndex: Integer;
begin
  AIndex := FCurrentIndex + 1;
  ACloseBracketIndex := PosEx(']', FormatString, FCurrentIndex);
  if ACloseBracketIndex <= 0 then
    Result := ''
  else
    Result := Copy(FormatString, AIndex, ACloseBracketIndex - AIndex);
end;

function TdxNumberFormatParser.TryParseColor(const S: string): TdxNumberFormatElement;
const
  ColorPrefix = 'Color';
var
  AColor: TColor;
  AIndex: Integer;
begin
  Result := nil;
  if cxColorByName(S, AColor) then
    Result := TdxNumberFormatElementColor.Create(AColor)
  else
    if SameText(ColorPrefix, Copy(S, 1, Length(ColorPrefix))) then
    begin
      AIndex := StrToIntDef(Copy(S, Length(ColorPrefix) + 1, MaxInt), 0);
      if InRange(AIndex - 1, Low(dxExcelStandardColors), High(dxExcelStandardColors)) then
        Result := TdxNumberFormatElementColor.Create(dxExcelStandardColors[AIndex - 1]);
    end
end;

function TdxNumberFormatParser.TryParseCondition: Integer;
var
  AElement: TdxNumberFormatElement;
  AString: string;
begin
  AString := TryGetConditionString;
  if AString = '' then
  begin
    OnDefault;
    Exit(1);
  end;

  AElement := TryParseColor(AString);
  if AElement <> nil then
    FElements.Insert(0, AElement)
  else
  begin
    AElement := TryParseLocale(AString);
    if AElement = nil then
      AElement := TryParseExpr(AString);
    if AElement = nil then
      Exit(-1);
    FElements.Add(AElement);
  end;
  Result := Length(AString) + 2;
  Inc(FCurrentIndex, Result - 1);
end;

function TdxNumberFormatParser.TryParseDateTimeCondition(var AElapsed: Boolean): Integer;
var
  AElement: TdxNumberFormatElement;
  AElementString: string;
begin
  FLocale := nil;

  AElementString := TryGetConditionString;
  if AElementString = '' then
  begin
    OnDefault;
    Exit(1);
  end;

  AElement := TryParseColor(AElementString);
  if AElement = nil then
  begin
    AElement := TryParseLocale(AElementString);
    if AElement = nil then
    begin
      AElement := TryParseElapsed(AElementString);
      if AElement = nil then
      begin
        AElement := TryParseExpr(AElementString);
        if AElement = nil then
          Exit(-1)
        else
          FElements.Add(AElement);
      end
      else
      begin
        if AElapsed then
          Exit(-1);
        AElapsed := True;
        FElements.Add(AElement);
      end;
    end
    else
    begin
      if FLocale <> nil then
        Exit(-1)
      else
        if AElement is TdxNumberFormatElementDisplayLocale then
          FLocale := TdxNumberFormatElementDisplayLocale(AElement)
        else
          FLocale := nil;

      FElements.Add(AElement);
    end;
  end
  else
    FElements.Insert(0, AElement);

  Inc(FCurrentIndex, Length(AElementString) + 1);
  Result := Length(AElementString) + 2;
end;

function TdxNumberFormatParser.TryParseElapsed(const S: string): TdxNumberFormatElement;
var
  ABlockLength: Integer;
begin
  Result := nil;
  FCurrentSymbol := dxLowerCase(S[1]);
  if (FCurrentSymbol = 'h') or (FCurrentSymbol = 'm') or (FCurrentSymbol = 's') then
  begin
    ABlockLength := GetDateTimeBlockLength(S, 1);
    if ABlockLength = Length(S) then
      case FCurrentSymbol of
        'h': Result := TdxNumberFormatElementHours.Create(ABlockLength, True, False);
        'm': Result := TdxNumberFormatElementMinutes.Create(ABlockLength, True);
        's': Result := TdxNumberFormatElementSeconds.Create(ABlockLength, True);
      end;
  end;
end;

function TdxNumberFormatParser.TryParseExpr(const S: string): TdxNumberFormatElement;
begin
  Result := TdxNumberFormatElementExprCondition.Create(S);
end;

function TdxNumberFormatParser.TryParseLocale(const S: string): TdxNumberFormatElement;
var
  ADashIndex: Integer;
begin
  if (S = '') or (S[1] <> '$') then
    Exit(nil);

  ADashIndex := Pos('-', S);
  if ADashIndex > 0 then
    Result := TdxNumberFormatElementDisplayLocale.Create(StrToIntDef('$' + Copy(S, ADashIndex + 1, MaxInt), -1), Copy(S, 2, ADashIndex - 2))
  else
    Result := TdxNumberFormatElementDisplayLocale.Create(-1, Copy(S, 2, MaxInt));
end;

function TdxNumberFormatParser.OnAmPmCore: Boolean;
var
  AElement: TdxNumberFormatElement;
  I: Integer;
  S: string;
begin
  if (FCurrentIndex + 5 <= Length(FormatString)) and SameText(Copy(FormatString, FCurrentIndex, 5), 'am/pm') then
  begin
    if FElapsed then
    begin
      FErrorState := True;
      Exit(True);
    end;
    Inc(FCurrentIndex, 4);
    FElements.Add(TdxNumberFormatElementAmPm.Create);
  end
  else

  if FCurrentIndex + 3 <= Length(FormatString) then
  begin
    S := Copy(FormatString, FCurrentIndex, 3);
    if not SameText(S, 'a/p') then
      Exit(False);

    if FElapsed then
    begin
      FErrorState := True;
      Exit(True);
    end;
    Inc(FCurrentIndex, 2);
    FElements.Add(TdxNumberFormatElementAmPm.CreateEx(dxIsLowerCase(S[1]), dxIsLowerCase(S[2])));
  end
  else
    Exit(False);

  for I := FElements.Count - 2 downto 0 do
  begin
    AElement := FElements[I];
    if AElement is TdxNumberFormatElementTimeBase then
    begin
      if AElement is TdxNumberFormatElementHours then
      begin
        TdxNumberFormatElementHours(AElement).Is12HourTime := True;
        Break;
      end;
    end
    else
      if AElement is TdxNumberFormatElementDateBase then
        Break;
  end;
  Result := True;
end;

procedure TdxNumberFormatParser.OnAmPmOrDayOfWeek;
begin
  if not OnAmPmCore then
    OnDayOfWeekOrDefault2;
end;

procedure TdxNumberFormatParser.OnAmPmOrMonth;
begin
  if not OnAmPmCore then
    OnMonth;
end;

procedure TdxNumberFormatParser.OnAmPmOrYear;
begin
  if not OnAmPmCore then
    OnYear;
end;

procedure TdxNumberFormatParser.OnAsterisk;
begin
  Inc(FCurrentIndex);
  FElements.Add(TdxNumberFormatElementAsterisk.Create(FormatString[FCurrentIndex]));
end;

procedure TdxNumberFormatParser.OnAt;
begin
  ParseText;
end;

procedure TdxNumberFormatParser.OnAt6;
begin
  FElements.Add(TdxNumberFormatElementTextContent.Create);
end;

procedure TdxNumberFormatParser.OnBackslash;
begin
  Inc(FCurrentIndex);
  FCurrentSymbol := FormatString[FCurrentIndex];
  OnDefault;
end;

procedure TdxNumberFormatParser.OnBracket;
var
  ACloseBracketIndex: Integer;
  ACount: Integer;
  AElement: TdxNumberFormatElement;
  AElementString: string;
  AIndex: Integer;
begin
  AIndex := FCurrentIndex + 1;
  ACloseBracketIndex := PosEx(']', FormatString, FCurrentIndex);
  if ACloseBracketIndex < 0 then
  begin
    OnDefault;
    Exit;
  end;

  AElementString := Copy(FormatString, AIndex, ACloseBracketIndex - AIndex);

  AElement := TryParseColor(AElementString);
  if AElement = nil then
  begin
    FLocale := TryParseLocale(AElementString);
    AElement := FLocale;
    if AElement = nil then
    begin
      ACount := FElements.Count;
      OnDateTimeSymbol;
      if FPart = nil then
      begin
        FErrorState := False;
        FCurrentIndex := AIndex - 1;
        FElements.DeleteRange(ACount, FElements.Count - ACount);
        AElement := TryParseExpr(AElementString);
        if AElement = nil then
        begin
          FErrorState := True;
          Exit;
        end;
      end
      else
        Exit;
    end;
  end;
  FElements.Add(AElement);
  FCurrentIndex := ACloseBracketIndex;
end;

procedure TdxNumberFormatParser.OnBracket2;
begin
  TryParseDateTimeCondition(FElapsed);
end;

procedure TdxNumberFormatParser.OnBracket3;
begin
  if TryParseCondition < 0 then
    FErrorState := True;
end;

procedure TdxNumberFormatParser.OnDateOrTimeSeparator;
begin
  if (FElements.Count > 0) and (FElements.Last is TdxNumberFormatElementTimeBase) then
    OnTimeSeparator
  else
    OnDateSeparator;
end;

procedure TdxNumberFormatParser.OnDateSeparator;
begin
  FElements.Add(TdxNumberFormatElementDateSeparator.Create);
end;

procedure TdxNumberFormatParser.OnDateTimeSymbol;
begin
  FElapsed := False;
  FHasMilliseconds := False;
  FElementLength := -1;
  while FCurrentIndex <= Length(FormatString) do
  begin
    FCurrentSymbol := FormatString[FCurrentIndex];
    if not FLocalizer.Designators.TryGetValue(dxLowerCase(FCurrentSymbol), FDesignator) then
      FDesignator := TdxNumberFormatDesignator.Default;
    if not FParseMethods2.TryGetValue(FDesignator, FDesignatorParser) then
      ParseSeparator2(FDesignator)
    else
      FDesignatorParser();

    if FErrorState then
    begin
      FreeAndNil(FPart);
      Exit;
    end;
    if FCurrentSymbol = ';' then
      Exit;
    Inc(FCurrentIndex);
  end;
  if FPart = nil then
    FErrorState := True;
end;

procedure TdxNumberFormatParser.OnDay;
begin
  FElements.Add(TdxNumberFormatElementDay.Create(Min(GetDateTimeBlockLength, 4)));
end;

function TdxNumberFormatParser.OnDayOfWeekCore: Boolean;
var
  AElementLength: Integer;
  AIndex: Integer;
begin
  AIndex := FCurrentIndex;
  AElementLength := GetDateTimeBlockLength();
  Result := AElementLength >= 3;
  if Result then
    FElements.Add(TdxNumberFormatElementDayOfWeek.Create(IfThen(AElementLength > 3, 4, 3)))
  else
    FCurrentIndex := AIndex;
end;

procedure TdxNumberFormatParser.OnDayOfWeekOrDefault;
begin
  if OnDayOfWeekCore then
  begin
    Inc(FCurrentIndex);
    OnDateTimeSymbol;
  end
  else
    OnDefault;
end;

procedure TdxNumberFormatParser.OnDayOfWeekOrDefault2;
begin
  if not OnDayOfWeekCore then
    OnDefault;
end;

procedure TdxNumberFormatParser.OnDayOfWeekOrDefault3;
begin
  if OnDayOfWeekCore then
  begin
    FElements.Delete(FElements.Count - 1);
    OnError;
  end
  else
    OnDefault;
end;

procedure TdxNumberFormatParser.OnDecimalSeparator;
begin
  FIsDecimal := True;
  if FDecimalSeparatorIndex < 0 then
    FDecimalSeparatorIndex := FElements.Count;
  FElements.Add(TdxNumberFormatElementDecimalSeparator.Create);
end;

procedure TdxNumberFormatParser.OnDefault;
begin
  FElements.Add(TdxNumberFormatElementBackslashedText.Create(FCurrentSymbol));
end;

procedure TdxNumberFormatParser.OnDefaultDateSeparator;
begin
  FElements.Add(TdxNumberFormatElementDefaultDateSeparator.Create);
end;

procedure TdxNumberFormatParser.OnDigitEmpty;
begin
  BeforeDigit;
  FElements.Add(TdxNumberFormatElementDigitEmpty.Create);
end;

procedure TdxNumberFormatParser.OnDigitEmpty4;
begin
  Inc(FExpCount);
  FElements.Add(TdxNumberFormatElementDigitEmpty.Create);
end;

procedure TdxNumberFormatParser.OnDigitEmpty5;
begin
  Inc(FDivisorCount);
  FElements.Add(TdxNumberFormatElementDigitEmpty.Create);
  FDisplayFactor := FElements.Count;
end;

procedure TdxNumberFormatParser.OnDigitSpace;
begin
  BeforeDigit;
  FElements.Add(TdxNumberFormatElementDigitSpace.Create);
end;

procedure TdxNumberFormatParser.OnDigitSpace4;
begin
  Inc(FExpCount);
  FElements.Add(TdxNumberFormatElementDigitSpace.Create);
end;

procedure TdxNumberFormatParser.OnDigitSpace5;
begin
  Inc(FDivisorCount);
  FElements.Add(TdxNumberFormatElementDigitSpace.Create);
  FDisplayFactor := FElements.Count;
end;

procedure TdxNumberFormatParser.OnDigitZero;
begin
  BeforeDigit;
  FElements.Add(TdxNumberFormatElementDigitZero.Create);
end;

procedure TdxNumberFormatParser.OnDigitZero4;
begin
  Inc(FExpCount);
  FElements.Add(TdxNumberFormatElementDigitZero.Create);
end;

procedure TdxNumberFormatParser.OnDigitZero5;
begin
  if FDivisor > 0 then
    FDivisorPow := FDivisorPow div 10
  else
  begin
    Inc(FDivisorCount);
    FElements.Add(TdxNumberFormatElementDigitZero.Create);
  end;
  FDisplayFactor := FElements.Count;
end;

procedure TdxNumberFormatParser.OnEndOfPart;
begin
  FPart := TdxSimpleNumericNumberFormat.Create(FElements, 0, 0, 0, 0, -1, False, FFormats.Count = 1);
end;

procedure TdxNumberFormatParser.OnEndOfPart2;

  function CreatePart(ALocale: TdxNumberFormatElementDisplayLocale): TdxSimpleNumberFormat;
  begin
    if ALocale <> nil then
    begin
      if ALocale.LocaleId = TdxDateTimeNumberFormat.SystemLongDate then
        Exit(TdxDateTimeSystemLongDateNumberFormat.Create(FElements));
      if ALocale.LocaleId = TdxDateTimeNumberFormat.SystemLongTime then
        Exit(TdxDateTimeSystemLongTimeNumberFormat.Create(FElements));
    end;
    Result := TdxDateTimeNumberFormat.Create(FElements, ALocale, FHasMilliseconds);
  end;

begin
  FPart := CreatePart(FLocale as TdxNumberFormatElementDisplayLocale);
end;

procedure TdxNumberFormatParser.OnEndOfPart3;
begin
  Inc(FIntegerCount, FDividendCount);
  PrepareGrouping;
  FPart := TdxSimpleNumericNumberFormat.Create(FElements, FPercentCount, FIntegerCount,
    FDecimalCount, FDisplayFactor, FDecimalSeparatorIndex, FGrouping, FFormats.Count = 1);
end;

procedure TdxNumberFormatParser.OnEndOfPart4;
begin
  FPart := TdxNumericNumberFormatExponent.Create(FElements, FIntegerCount, FDecimalCount,
    FDecimalSeparatorIndex, FExpIndex, FExpCount, FExplicitSign, FGrouping, FFormats.Count = 1);
end;

procedure TdxNumberFormatParser.OnEndOfPart5;
begin
  if FDivisor > 0 then
  begin
    FDivisor := FDivisor div (FDivisorPow * 10);
    FPart := TdxExplicitNumericFractionNumberFormat.Create(FElements, FPercentCount, FIntegerCount,
      FPreFractionIndex, FFractionSeparatorIndex, FDisplayFactor, FDividendCount, FDivisor, FGrouping, FFormats.Count = 1);
  end
  else
    if FDivisorCount = 0 then
      FErrorState := True
    else
      FPart := TdxNumericFractionNumberFormat.Create(FElements, FPercentCount, FIntegerCount, FPreFractionIndex,
        FFractionSeparatorIndex, FDisplayFactor, FDividendCount, FDivisorCount, FGrouping, FFormats.Count = 1);
end;

procedure TdxNumberFormatParser.OnEndOfPart6;
begin
  if FCurrentIndex < Length(FormatString) then
    FErrorState := True
  else
    FPart := TdxTextNumberFormat.Create(FElements);
end;

procedure TdxNumberFormatParser.OnEndOfPart7;
begin
  if FFractionSeparatorIndex < 0 then
    FErrorState := True;
  FPart := TdxGeneralNumberFormat.Create(FElements);
end;

procedure TdxNumberFormatParser.OnError;
begin
  FErrorState := True;
end;

procedure TdxNumberFormatParser.OnESymbol;
begin
  if dxIsLowerCase(FCurrentSymbol) then
    OnDateTimeSymbol
  else
    OnError;
end;

procedure TdxNumberFormatParser.OnESymbol2;
begin
  if dxIsLowerCase(FCurrentSymbol) then
    OnInvariantYear
  else
    OnError;
end;

procedure TdxNumberFormatParser.OnESymbol3;
begin
  if dxIsLowerCase(FCurrentSymbol) then
    OnError
  else
    OnExponent;
end;

procedure TdxNumberFormatParser.OnExponent;
begin
  Inc(FIntegerCount, FDividendCount);
  PrepareGrouping;
  ParseNumericExponent(FIntegerCount, FDecimalSeparatorIndex, FDecimalCount, FGrouping);
end;

procedure TdxNumberFormatParser.OnFractionSeparator;
begin
  if FIsDecimal or (FDividendCount <= 0) then
  begin
    FErrorState := True;
    Exit;
  end;

  if FIntegerCount > 0 then
    PrepareGrouping
  else
    if FGrouping then
    begin
      FErrorState := True;
      Exit;
    end;

  ParseNumericFraction;
end;

procedure TdxNumberFormatParser.OnGeneral;
begin
  ParseGeneral;
end;

procedure TdxNumberFormatParser.OnGeneral2;
begin
  if CheckIsGeneral then
    FErrorState := True
  else
    OnDefault;
end;

procedure TdxNumberFormatParser.OnGeneral3;
begin
  if CheckIsGeneral then
    FErrorState := True
  else
    OnDefault;
end;

procedure TdxNumberFormatParser.OnGeneral7;
begin
  if CheckIsGeneral then
    OnGeneralCore
  else
    OnDefault;
end;

procedure TdxNumberFormatParser.OnGeneralCore;
begin
  if FFractionSeparatorIndex >= 0 then
    FErrorState := True
  else
  begin
    Inc(FCurrentIndex, Length(Localizer.GeneralDesignator) - 1);
    FElements.Add(TdxNumberFormatElementGeneral.Create);
    FFractionSeparatorIndex := FElements.Count;
  end;
end;

procedure TdxNumberFormatParser.OnGeneralOrDateTime;
begin
  if CheckIsGeneral then
    OnGeneral
  else
    OnDateTimeSymbol;
end;

procedure TdxNumberFormatParser.OnGeneralOrDateTime7;
begin
  if CheckIsGeneral then
    OnGeneralCore
  else
    FErrorState := True;
end;

procedure TdxNumberFormatParser.OnGeneralOrDay;
begin
  if CheckIsGeneral then
    FErrorState := True
  else
    OnDay
end;

procedure TdxNumberFormatParser.OnGeneralOrInvariantYear;
begin
  if CheckIsGeneral then
    FErrorState := True
  else
    OnInvariantYear;
end;

procedure TdxNumberFormatParser.OnGeneralOrJapaneseEra;
begin
  if CheckIsGeneral then
    FErrorState := True
  else
    OnJapaneseEra;
end;

procedure TdxNumberFormatParser.OnGeneralOrSecond;
begin
  if CheckIsGeneral then
    FErrorState := True
  else
    OnSecond;
end;

procedure TdxNumberFormatParser.OnGroupSeparator;
begin
  if (FElements.Count > 0) and FElements.Last.IsDigit then
    Inc(FDisplayFactor)
  else
    OnDefault;
end;

procedure TdxNumberFormatParser.OnHour;

  function Is12HourTime: Boolean;
  var
    AElement: TdxNumberFormatElement;
    I: Integer;
  begin
    Result := False;
    for I := FElements.Count - 1 downto 0 do
    begin
      AElement := FElements[I];
      if AElement is TdxNumberFormatElementDateBase then
      begin
        if AElement is TdxNumberFormatElementAmPm then
          Exit(True);
        if (AElement is TdxNumberFormatElementHours) or not (AElement is TdxNumberFormatElementTimeBase) then
          Break;
      end;
    end;
  end;

begin
  FElements.Add(TdxNumberFormatElementHours.Create(IfThen(GetDateTimeBlockLength > 1, 2, 1), False, Is12HourTime));
end;

procedure TdxNumberFormatParser.OnInvariantYear;
begin
  FElements.Add(TdxNumberFormatElementInvariantYear.Create(Min(GetDateTimeBlockLength, 2)));
end;

procedure TdxNumberFormatParser.OnJapaneseEra;
begin
  FElements.Add(TdxNumberFormatElementJapaneseEra.Create(Min(GetDateTimeBlockLength, 3)));
end;

procedure TdxNumberFormatParser.OnMillisecond;
begin
  if not OnMillisecondCore then
    OnDefault;
end;

function TdxNumberFormatParser.OnMillisecondCore: Boolean;
var
  AElementLength: Integer;
begin
  AElementLength := FCurrentIndex;

  Inc(FCurrentIndex);
  while FCurrentIndex <= Length(FormatString) do
  begin
    if FormatString[FCurrentIndex] = '0' then
      Inc(FCurrentIndex)
    else
      Break;
  end;
  Dec(FCurrentIndex);

  AElementLength := FCurrentIndex - AElementLength;
  if AElementLength = 0 then
    Result := False
  else
  begin
    if AElementLength > 3 then
      FErrorState := True
    else
    begin
      FHasMilliseconds := True;
      FElements.Add(TdxNumberFormatElementMilliseconds.Create(AElementLength, False));
    end;
    Result := True;
  end;
end;

procedure TdxNumberFormatParser.OnMinute;
begin
  FElements.Add(TdxNumberFormatElementMinutes.Create(Min(GetDateTimeBlockLength, 2), False));
end;

procedure TdxNumberFormatParser.OnMonth;
var
  ALength: Integer;
begin
  ALength := GetDateTimeBlockLength;
  FElements.Add(TdxNumberFormatElementMonth.Create(IfThen(ALength > 5, 4, ALength)));
end;

procedure TdxNumberFormatParser.OnMonthOrMinute;

  function CheckIsMinute: Boolean;
  var
    AElement: TdxNumberFormatElement;
    I: Integer;
  begin
    Result := False;
    for I := FElements.Count - 1 downto 0 do
    begin
      AElement := FElements[I];
      if AElement is TdxNumberFormatElementTimeBase then
        Exit(True);
      if (AElement is TdxNumberFormatElementDateBase) and not (AElement is TdxNumberFormatElementAmPm) then
        Exit(False);
    end;
  end;

var
  ABlockLength: Integer;
begin
  ABlockLength := GetDateTimeBlockLength;
  if ABlockLength > 2 then
    FElements.Add(TdxNumberFormatElementMonth.Create(IfThen(ABlockLength > 5, 4, ABlockLength)))
  else
    if CheckIsMinute then
      FElements.Add(TdxNumberFormatElementMinutes.Create(ABlockLength, False))
    else
      FElements.Add(TdxNumberFormatElementMonth.Create(ABlockLength));
end;

procedure TdxNumberFormatParser.OnNumericSymbol;
begin
  FDecimalCount := 0;
  FDecimalSeparatorIndex := -1;
  FDisplayFactor := 0;
  FDividendCount := 0;
  FGrouping := False;
  FIntegerCount := 0;
  FIsDecimal := False;
  FPercentCount := 0;
  FPreFractionIndex := -1;

  ProcessSequence(FParseMethods3, ParseSeparator3);
end;

procedure TdxNumberFormatParser.OnPercent;
begin
  Inc(FPercentCount);
  FElements.Add(TdxNumberFormatElementPercent.Create);
end;

procedure TdxNumberFormatParser.OnQuote;
var
  AIndex: Integer;
begin
  Inc(FCurrentIndex);
  AIndex := FCurrentIndex;
  while FCurrentIndex <= Length(FFormatString) do
  begin
    FCurrentSymbol := FormatString[FCurrentIndex];
    if FCurrentSymbol = '"' then
    begin
      FElements.Add(TdxNumberFormatElementQuotedText.Create(Copy(FormatString, AIndex, FCurrentIndex - AIndex)));
      Break;
    end;
    Inc(FCurrentIndex);
  end;
end;

procedure TdxNumberFormatParser.OnSecond;
var
  AElement: TdxNumberFormatElement;
  I: Integer;
begin
  // try convert month to minute
  for I := FElements.Count - 1 downto 0 do
  begin
    AElement := FElements[I];
    if AElement is TdxNumberFormatElementDateBase then
    begin
      if AElement is TdxNumberFormatElementAmPm then
        Continue;
      if AElement is TdxNumberFormatElementMonth then
        FElements[I] := TdxNumberFormatElementMinutes.Create(TdxNumberFormatElementMonth(AElement).Count, False);
      Break;
    end;
  end;
  FElements.Add(TdxNumberFormatElementSeconds.Create(IfThen(GetDateTimeBlockLength > 1, 2, 1), False));
end;

procedure TdxNumberFormatParser.OnThaiYear;
var
  AElementLength: Integer;
begin
  AElementLength := GetDateTimeBlockLength;
  Inc(FCurrentIndex);
  if FCurrentIndex <= Length(FormatString) then
  begin
    if dxCharInSet(FormatString[FCurrentIndex], ['1', '2']) then
    begin
      FErrorState := True;
      Exit;
    end;
  end;
  Dec(FCurrentIndex);
  FElements.Add(TdxNumberFormatElementThaiYear.Create(IfThen(AElementLength > 2, 4, 2)));
end;

procedure TdxNumberFormatParser.OnTimeSeparator;
begin
  FElements.Add(TdxNumberFormatElementTimeSeparator.Create);
end;

procedure TdxNumberFormatParser.OnUnderline;
begin
  Inc(FCurrentIndex);
  FElements.Add(TdxNumberFormatElementUnderline.Create(FormatString[FCurrentIndex]));
end;

procedure TdxNumberFormatParser.OnYear;
begin
  FElements.Add(TdxNumberFormatElementYear.Create(IfThen(GetDateTimeBlockLength > 2, 4, 2)));
end;

procedure TdxNumberFormatParser.PopulateParseMethods1;
begin
  FParseMethods1 := TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>.Create;
  FParseMethods1.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek, OnDateTimeSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month, OnDateTimeSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Year, OnDateTimeSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek or TdxNumberFormatDesignator.Month, FParseMethods1[(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month)]);
  FParseMethods1.Add(TdxNumberFormatDesignator.Asterisk, OnAsterisk);
  FParseMethods1.Add(TdxNumberFormatDesignator.At, OnAt);
  FParseMethods1.Add(TdxNumberFormatDesignator.Backslash, OnBackslash);
  FParseMethods1.Add(TdxNumberFormatDesignator.Bracket, OnBracket);
  FParseMethods1.Add(TdxNumberFormatDesignator.DateSeparator, OnError);
  FParseMethods1.Add(TdxNumberFormatDesignator.Day, OnDateTimeSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.DayOfWeek, OnDayOfWeekOrDefault);
  FParseMethods1.Add(TdxNumberFormatDesignator.Default, OnDefault);
  FParseMethods1.Add(TdxNumberFormatDesignator.DecimalSeparator, OnNumericSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.DigitEmpty, OnNumericSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.DigitSpace, OnNumericSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.DigitZero, OnNumericSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.EndOfPart, OnEndOfPart);
  FParseMethods1.Add(TdxNumberFormatDesignator.Exponent, OnESymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.FractionOrDateSeparator, OnError);
  FParseMethods1.Add(TdxNumberFormatDesignator.GroupSeparator, OnDefault);
  FParseMethods1.Add(TdxNumberFormatDesignator.General, OnGeneral);
  FParseMethods1.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day, OnGeneralOrDateTime);
  FParseMethods1.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day or TdxNumberFormatDesignator.JapaneseEra, FParseMethods1[(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day)]);
  FParseMethods1.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Second, OnGeneralOrDateTime);
  FParseMethods1.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.InvariantYear, OnGeneralOrDateTime);
  FParseMethods1.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.JapaneseEra, OnGeneralOrDateTime);
  FParseMethods1.Add(TdxNumberFormatDesignator.Hour, OnDateTimeSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.Hour or TdxNumberFormatDesignator.JapaneseEra, FParseMethods1[TdxNumberFormatDesignator.Hour]);
  FParseMethods1.Add(TdxNumberFormatDesignator.InvariantYear, OnESymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.JapaneseEra, OnDateTimeSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.Minute or TdxNumberFormatDesignator.Month, OnDateTimeSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.Minute, OnDateTimeSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.Month, OnDateTimeSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.Percent, OnNumericSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.Quote, OnQuote);
  FParseMethods1.Add(TdxNumberFormatDesignator.Second, OnDateTimeSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.ThaiYear, OnDateTimeSymbol);
  FParseMethods1.Add(TdxNumberFormatDesignator.TimeSeparator, OnDefault);
  FParseMethods1.Add(TdxNumberFormatDesignator.Underline, OnUnderline);
  FParseMethods1.Add(TdxNumberFormatDesignator.Year, OnDateTimeSymbol);
end;

procedure TdxNumberFormatParser.PopulateParseMethods2;
begin
  FParseMethods2 := TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>.Create;
  FParseMethods2.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek, OnAmPmOrDayOfWeek);
  FParseMethods2.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month, OnAmPmOrMonth);
  FParseMethods2.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Year, OnAmPmOrYear);
  FParseMethods2.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek or TdxNumberFormatDesignator.Month, FParseMethods2[(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month)]);
  FParseMethods2.Add(TdxNumberFormatDesignator.Asterisk, OnAsterisk);
  FParseMethods2.Add(TdxNumberFormatDesignator.At, OnError);
  FParseMethods2.Add(TdxNumberFormatDesignator.Backslash, OnBackslash);
  FParseMethods2.Add(TdxNumberFormatDesignator.Bracket, OnBracket2);
  FParseMethods2.Add(TdxNumberFormatDesignator.DateSeparator, OnDateSeparator);
  FParseMethods2.Add(TdxNumberFormatDesignator.Day, OnDay);
  FParseMethods2.Add(TdxNumberFormatDesignator.DayOfWeek, OnDayOfWeekOrDefault2);
  FParseMethods2.Add(TdxNumberFormatDesignator.Default, OnDefault);
  FParseMethods2.Add(TdxNumberFormatDesignator.DecimalSeparator, OnMillisecond);
  FParseMethods2.Add(TdxNumberFormatDesignator.DigitEmpty, OnError);
  FParseMethods2.Add(TdxNumberFormatDesignator.DigitSpace, OnError);
  FParseMethods2.Add(TdxNumberFormatDesignator.DigitZero, OnError);
  FParseMethods2.Add(TdxNumberFormatDesignator.EndOfPart, OnEndOfPart2);
  FParseMethods2.Add(TdxNumberFormatDesignator.Exponent, OnESymbol2);
  FParseMethods2.Add(TdxNumberFormatDesignator.FractionOrDateSeparator, OnDefaultDateSeparator);
  FParseMethods2.Add(TdxNumberFormatDesignator.GroupSeparator, OnDefault);
  FParseMethods2.Add(TdxNumberFormatDesignator.General, OnGeneral2);
  FParseMethods2.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day, OnGeneralOrDay);
  FParseMethods2.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day or TdxNumberFormatDesignator.JapaneseEra, FParseMethods2[(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day)]);
  FParseMethods2.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Second, OnGeneralOrSecond);
  FParseMethods2.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.InvariantYear, OnGeneralOrInvariantYear);
  FParseMethods2.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.JapaneseEra, OnGeneralOrJapaneseEra);
  FParseMethods2.Add(TdxNumberFormatDesignator.Hour, OnHour);
  FParseMethods2.Add(TdxNumberFormatDesignator.Hour or TdxNumberFormatDesignator.JapaneseEra, FParseMethods2[TdxNumberFormatDesignator.Hour]);
  FParseMethods2.Add(TdxNumberFormatDesignator.InvariantYear, OnESymbol2);
  FParseMethods2.Add(TdxNumberFormatDesignator.JapaneseEra, OnJapaneseEra);
  FParseMethods2.Add(TdxNumberFormatDesignator.Minute or TdxNumberFormatDesignator.Month, OnMonthOrMinute);
  FParseMethods2.Add(TdxNumberFormatDesignator.Minute, OnMinute);
  FParseMethods2.Add(TdxNumberFormatDesignator.Month, OnMonth);
  FParseMethods2.Add(TdxNumberFormatDesignator.Percent, OnError);
  FParseMethods2.Add(TdxNumberFormatDesignator.Quote, OnQuote);
  FParseMethods2.Add(TdxNumberFormatDesignator.Second, OnSecond);
  FParseMethods2.Add(TdxNumberFormatDesignator.ThaiYear, OnThaiYear);
  FParseMethods2.Add(TdxNumberFormatDesignator.TimeSeparator, OnTimeSeparator);
  FParseMethods2.Add(TdxNumberFormatDesignator.Underline, OnUnderline);
  FParseMethods2.Add(TdxNumberFormatDesignator.Year, OnYear);
end;

procedure TdxNumberFormatParser.PopulateParseMethods3;
begin
  FParseMethods3 := TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>.Create;
  FParseMethods3.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Year, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek or TdxNumberFormatDesignator.Month, FParseMethods3[(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month)]);
  FParseMethods3.Add(TdxNumberFormatDesignator.Asterisk, OnAsterisk);
  FParseMethods3.Add(TdxNumberFormatDesignator.At, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.Backslash, OnBackslash);
  FParseMethods3.Add(TdxNumberFormatDesignator.Bracket, OnBracket3);
  FParseMethods3.Add(TdxNumberFormatDesignator.DateSeparator, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.Day, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.DayOfWeek, OnDayOfWeekOrDefault3);
  FParseMethods3.Add(TdxNumberFormatDesignator.Default, OnDefault);
  FParseMethods3.Add(TdxNumberFormatDesignator.DecimalSeparator, OnDecimalSeparator);
  FParseMethods3.Add(TdxNumberFormatDesignator.DigitEmpty, OnDigitEmpty);
  FParseMethods3.Add(TdxNumberFormatDesignator.DigitSpace, OnDigitSpace);
  FParseMethods3.Add(TdxNumberFormatDesignator.DigitZero, OnDigitZero);
  FParseMethods3.Add(TdxNumberFormatDesignator.EndOfPart, OnEndOfPart3);
  FParseMethods3.Add(TdxNumberFormatDesignator.Exponent, OnESymbol3);
  FParseMethods3.Add(TdxNumberFormatDesignator.FractionOrDateSeparator, OnFractionSeparator);
  FParseMethods3.Add(TdxNumberFormatDesignator.GroupSeparator, OnGroupSeparator);
  FParseMethods3.Add(TdxNumberFormatDesignator.General, OnGeneral3);
  FParseMethods3.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day or TdxNumberFormatDesignator.JapaneseEra, FParseMethods3[(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day)]);
  FParseMethods3.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Second, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.InvariantYear, OnESymbol3);
  FParseMethods3.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.JapaneseEra, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.Hour, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.Hour or TdxNumberFormatDesignator.JapaneseEra, FParseMethods3[TdxNumberFormatDesignator.Hour]);
  FParseMethods3.Add(TdxNumberFormatDesignator.InvariantYear, OnESymbol3);
  FParseMethods3.Add(TdxNumberFormatDesignator.JapaneseEra, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.Minute or TdxNumberFormatDesignator.Month, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.Minute, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.Month, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.Percent, OnPercent);
  FParseMethods3.Add(TdxNumberFormatDesignator.Quote, OnQuote);
  FParseMethods3.Add(TdxNumberFormatDesignator.Second, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.ThaiYear, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.TimeSeparator, OnError);
  FParseMethods3.Add(TdxNumberFormatDesignator.Underline, OnUnderline);
  FParseMethods3.Add(TdxNumberFormatDesignator.Year, OnError);
end;

procedure TdxNumberFormatParser.PopulateParseMethods4;
begin
  FParseMethods4 := TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>.Create;
  FParseMethods4.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Year, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek or TdxNumberFormatDesignator.Month, FParseMethods4[(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month)]);
  FParseMethods4.Add(TdxNumberFormatDesignator.Asterisk, OnAsterisk);
  FParseMethods4.Add(TdxNumberFormatDesignator.At, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.Backslash, OnBackslash);
  FParseMethods4.Add(TdxNumberFormatDesignator.Bracket, OnBracket3);
  FParseMethods4.Add(TdxNumberFormatDesignator.DateSeparator, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.Day, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.DayOfWeek, OnDayOfWeekOrDefault3);
  FParseMethods4.Add(TdxNumberFormatDesignator.Default, OnDefault);
  FParseMethods4.Add(TdxNumberFormatDesignator.DecimalSeparator, OnDecimalSeparator);
  FParseMethods4.Add(TdxNumberFormatDesignator.DigitEmpty, OnDigitEmpty4);
  FParseMethods4.Add(TdxNumberFormatDesignator.DigitSpace, OnDigitSpace4);
  FParseMethods4.Add(TdxNumberFormatDesignator.DigitZero, OnDigitZero4);
  FParseMethods4.Add(TdxNumberFormatDesignator.EndOfPart, OnEndOfPart4);
  FParseMethods4.Add(TdxNumberFormatDesignator.Exponent, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.FractionOrDateSeparator, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.GroupSeparator, OnDefault);
  FParseMethods4.Add(TdxNumberFormatDesignator.General, OnGeneral3);
  FParseMethods4.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day or TdxNumberFormatDesignator.JapaneseEra, FParseMethods4[(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day)]);
  FParseMethods4.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Second, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.InvariantYear, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.JapaneseEra, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.Hour, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.Hour or TdxNumberFormatDesignator.JapaneseEra, FParseMethods4[TdxNumberFormatDesignator.Hour]);
  FParseMethods4.Add(TdxNumberFormatDesignator.InvariantYear, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.JapaneseEra, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.Minute or TdxNumberFormatDesignator.Month, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.Minute, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.Month, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.Percent, OnDefault);
  FParseMethods4.Add(TdxNumberFormatDesignator.Quote, OnQuote);
  FParseMethods4.Add(TdxNumberFormatDesignator.Second, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.ThaiYear, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.TimeSeparator, OnError);
  FParseMethods4.Add(TdxNumberFormatDesignator.Underline, OnUnderline);
  FParseMethods4.Add(TdxNumberFormatDesignator.Year, OnError);
end;
procedure TdxNumberFormatParser.PopulateParseMethods5;
begin
  FParseMethods5 := TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>.Create;
  FParseMethods5.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Year, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek or TdxNumberFormatDesignator.Month, FParseMethods5[(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month)]);
  FParseMethods5.Add(TdxNumberFormatDesignator.Asterisk, OnAsterisk);
  FParseMethods5.Add(TdxNumberFormatDesignator.At, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.Backslash, OnBackslash);
  FParseMethods5.Add(TdxNumberFormatDesignator.Bracket, OnBracket3);
  FParseMethods5.Add(TdxNumberFormatDesignator.DateSeparator, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.Day, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.DayOfWeek, OnDayOfWeekOrDefault3);
  FParseMethods5.Add(TdxNumberFormatDesignator.Default, OnDefault);
  FParseMethods5.Add(TdxNumberFormatDesignator.DecimalSeparator, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.DigitEmpty, OnDigitEmpty5);
  FParseMethods5.Add(TdxNumberFormatDesignator.DigitSpace, OnDigitSpace5);
  FParseMethods5.Add(TdxNumberFormatDesignator.DigitZero, OnDigitZero5);
  FParseMethods5.Add(TdxNumberFormatDesignator.EndOfPart, OnEndOfPart5);
  FParseMethods5.Add(TdxNumberFormatDesignator.Exponent, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.FractionOrDateSeparator, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.GroupSeparator, OnDefault);
  FParseMethods5.Add(TdxNumberFormatDesignator.General, OnGeneral3);
  FParseMethods5.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day or TdxNumberFormatDesignator.JapaneseEra, FParseMethods5[(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day)]);
  FParseMethods5.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Second, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.InvariantYear, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.JapaneseEra, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.Hour, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.Hour or TdxNumberFormatDesignator.JapaneseEra, FParseMethods5[TdxNumberFormatDesignator.Hour]);
  FParseMethods5.Add(TdxNumberFormatDesignator.InvariantYear, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.JapaneseEra, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.Minute or TdxNumberFormatDesignator.Month, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.Minute, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.Month, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.Percent, OnPercent);
  FParseMethods5.Add(TdxNumberFormatDesignator.Quote, OnQuote);
  FParseMethods5.Add(TdxNumberFormatDesignator.Second, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.ThaiYear, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.TimeSeparator, OnError);
  FParseMethods5.Add(TdxNumberFormatDesignator.Underline, OnUnderline);
  FParseMethods5.Add(TdxNumberFormatDesignator.Year, OnError);
end;

procedure TdxNumberFormatParser.PopulateParseMethods6;
begin
  FParseMethods6 := TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>.Create;
  FParseMethods6.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Year, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek or TdxNumberFormatDesignator.Month, FParseMethods6[(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month)]);
  FParseMethods6.Add(TdxNumberFormatDesignator.Asterisk, OnAsterisk);
  FParseMethods6.Add(TdxNumberFormatDesignator.At, OnAt6);
  FParseMethods6.Add(TdxNumberFormatDesignator.Backslash, OnBackslash);
  FParseMethods6.Add(TdxNumberFormatDesignator.Bracket, OnBracket3);
  FParseMethods6.Add(TdxNumberFormatDesignator.DateSeparator, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.Day, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.DayOfWeek, OnDayOfWeekOrDefault3);
  FParseMethods6.Add(TdxNumberFormatDesignator.Default, OnDefault);
  FParseMethods6.Add(TdxNumberFormatDesignator.DecimalSeparator, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.DigitEmpty, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.DigitSpace, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.DigitZero, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.EndOfPart, OnEndOfPart6);
  FParseMethods6.Add(TdxNumberFormatDesignator.Exponent, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.FractionOrDateSeparator, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.GroupSeparator, OnDefault);
  FParseMethods6.Add(TdxNumberFormatDesignator.General, OnGeneral3);
  FParseMethods6.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day or TdxNumberFormatDesignator.JapaneseEra, FParseMethods6[(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day)]);
  FParseMethods6.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Second, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.InvariantYear, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.JapaneseEra, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.Hour, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.Hour or TdxNumberFormatDesignator.JapaneseEra, FParseMethods6[TdxNumberFormatDesignator.Hour]);
  FParseMethods6.Add(TdxNumberFormatDesignator.InvariantYear, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.JapaneseEra, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.Minute or TdxNumberFormatDesignator.Month, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.Minute, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.Month, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.Percent, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.Quote, OnQuote);
  FParseMethods6.Add(TdxNumberFormatDesignator.Second, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.ThaiYear, OnError);
  FParseMethods6.Add(TdxNumberFormatDesignator.TimeSeparator, OnDefault);
  FParseMethods6.Add(TdxNumberFormatDesignator.Underline, OnUnderline);
  FParseMethods6.Add(TdxNumberFormatDesignator.Year, OnError);
end;

procedure TdxNumberFormatParser.PopulateParseMethods7;
begin
  FParseMethods7 := TDictionary<Integer, TdxNumberFormatDesignatorParseMethod>.Create;
  FParseMethods7.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Year, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.DayOfWeek or TdxNumberFormatDesignator.Month, FParseMethods7[(TdxNumberFormatDesignator.AmPm or TdxNumberFormatDesignator.Month)]);
  FParseMethods7.Add(TdxNumberFormatDesignator.Asterisk, OnAsterisk);
  FParseMethods7.Add(TdxNumberFormatDesignator.At, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.Backslash, OnBackslash);
  FParseMethods7.Add(TdxNumberFormatDesignator.Bracket, OnBracket3);
  FParseMethods7.Add(TdxNumberFormatDesignator.DateSeparator, OnDefault);
  FParseMethods7.Add(TdxNumberFormatDesignator.Day, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.DayOfWeek, OnDayOfWeekOrDefault3);
  FParseMethods7.Add(TdxNumberFormatDesignator.Default, OnDefault);
  FParseMethods7.Add(TdxNumberFormatDesignator.DecimalSeparator, OnDefault);
  FParseMethods7.Add(TdxNumberFormatDesignator.DigitEmpty, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.DigitSpace, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.DigitZero, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.EndOfPart, OnEndOfPart7);
  FParseMethods7.Add(TdxNumberFormatDesignator.Exponent, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.FractionOrDateSeparator, OnDefault);
  FParseMethods7.Add(TdxNumberFormatDesignator.GroupSeparator, OnDefault);
  FParseMethods7.Add(TdxNumberFormatDesignator.General, OnGeneral7);
  FParseMethods7.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day, OnGeneralOrDateTime7);
  FParseMethods7.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day or TdxNumberFormatDesignator.JapaneseEra, FParseMethods7[(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Day)]);
  FParseMethods7.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.Second, OnGeneralOrDateTime7);
  FParseMethods7.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.InvariantYear, OnGeneralOrDateTime7);
  FParseMethods7.Add(TdxNumberFormatDesignator.General or TdxNumberFormatDesignator.JapaneseEra, OnGeneralOrDateTime7);
  FParseMethods7.Add(TdxNumberFormatDesignator.Hour, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.Hour or TdxNumberFormatDesignator.JapaneseEra, FParseMethods7[TdxNumberFormatDesignator.Hour]);
  FParseMethods7.Add(TdxNumberFormatDesignator.InvariantYear, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.JapaneseEra, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.Minute or TdxNumberFormatDesignator.Month, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.Minute, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.Month, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.Percent, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.Quote, OnQuote);
  FParseMethods7.Add(TdxNumberFormatDesignator.Second, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.ThaiYear, OnError);
  FParseMethods7.Add(TdxNumberFormatDesignator.TimeSeparator, OnDefault);
  FParseMethods7.Add(TdxNumberFormatDesignator.Underline, OnUnderline);
  FParseMethods7.Add(TdxNumberFormatDesignator.Year, OnError);
end;

end.
