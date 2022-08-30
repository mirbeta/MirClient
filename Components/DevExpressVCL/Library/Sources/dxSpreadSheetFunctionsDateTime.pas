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

unit dxSpreadSheetFunctionsDateTime;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  SysUtils, Variants, Math, DateUtils, cxDateUtils, dxCore,
  dxSpreadSheetTypes, dxSpreadSheetUtils, dxSpreadSheetCoreFormulas;

procedure fnDate(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnDateValue(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnDay(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnDays(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnDays360(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnEDate(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnEOMonth(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnHour(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnIsoWeekNum(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMinute(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMonth(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNetworkDays(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNetworkDays_Intl(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNow(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSecond(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnTime(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnTimeValue(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnToday(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnWeekDay(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnWeekNum(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnWorkDay(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnWorkDay_Intl(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnYear(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnYearFrac(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

procedure fpiDate(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiDateValue(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiDay(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiDays(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiDays360(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiEDate(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiEOMonth(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiHour(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIsoWeekNum(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMinute(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMonth(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNetworkDays(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNetworkDays_Intl(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNow(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSecond(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiTime(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiTimeValue(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiToday(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiWeekDay(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiWeekNum(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiWorkDay(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiWorkDay_Intl(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiYear(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiYearFrac(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);

implementation

uses
  cxVariants, Classes, Generics.Defaults, Generics.Collections,
  dxSpreadSheetCoreFormulasTokens, dxSpreadSheetCoreFormulasHelpers, dxSpreadSheetFunctions, dxSpreadSheetFunctionsStrs;

type
  TdxTokenAccess = class(TdxSpreadSheetFormulaToken);

const
  dxMondaysWeek: array [1..7] of Integer =    (7, 1, 2, 3, 4, 5, 6);
  dxMondays0Week: array [1..7] of Integer =   (6, 0, 1, 2, 3, 4, 5);
  dxTuesdaysWeek: array [1..7] of Integer =   (6, 7, 1, 2, 3, 4, 5);
  dxWednesdaysWeek: array [1..7] of Integer = (5, 6, 7, 1, 2, 3, 4);
  dxThursdaysWeek: array [1..7] of Integer =  (4, 5, 6, 7, 1, 2, 3);
  dxFridaysWeek: array [1..7] of Integer =    (3, 4, 5, 6, 7, 1, 2);
  dxSaturdaysWeek: array [1..7] of Integer =  (2, 3, 4, 5, 6, 7, 1);

function dxIsNonWorkDay(ADate: TDateTime; const AWeekEndType: Integer): Boolean;
const
  AWeekEnds: array[1..14, 1..2] of Integer = (
    (6, 7), (7, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 6),    //  1..7
    (7, 7), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6) );  // 11..17
var
  ADay, ADay1, ADay2: Integer;
begin
  ADay := DayOfTheWeek(ADate);
  if AWeekEndType <= 7 then
  begin
    ADay1 := AWeekEnds[AWeekEndType, 1];
    ADay2 := AWeekEnds[AWeekEndType, 2];
  end
  else
  begin
    ADay1 := AWeekEnds[AWeekEndType - 3, 1];
    ADay2 := AWeekEnds[AWeekEndType - 3, 2];
  end;
  Result := (ADay = ADay1) or (ADay = ADay2);
end;

type
  TdxExtractHolidaysHelper = class
  public
    DateList: TList<TDate>;
    DateTimeSystem: TdxSpreadSheetDateTimeSystem;
    constructor Create(AList: TList<TDate>; ADateTimeSystem: TdxSpreadSheetDateTimeSystem);
  end;

constructor TdxExtractHolidaysHelper.Create(AList: TList<TDate>; ADateTimeSystem: TdxSpreadSheetDateTimeSystem);
begin
  DateList := AList;
  DateTimeSystem := ADateTimeSystem;
end;

function cbExtractHolidays(const AValue: Variant; ACanConvertStrToNumber: Boolean;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode; AData, AInfo: Pointer): Boolean;

  procedure AddDateInList(ADate: TDateTime);
  var
    AList: TList<TDate>;
  begin
    AList := TList<TDate>(TdxExtractHolidaysHelper(AData).DateList);
    if AList.IndexOf(ADate) < 0 then
      AList.Add(ADate);
  end;

var
  ADate: TDateTime;
begin
  if dxIsNumberOrDateTime(AValue) then
    AddDateInList(dxDateTimeToRealDateTime(AValue, TdxExtractHolidaysHelper(AData).DateTimeSystem))
  else
    if dxConvertToXLSDate(AValue, ADate) then
      AddDateInList(ADate)
    else
      if not(VarIsEmpty(AValue) or VarIsNull(AValue)) then
        AErrorCode := ecValue;
  Result := AErrorCode = ecNone;
end;

function dxCreateHolidaysList(Sender: TdxSpreadSheetFormulaResult; AToken: TdxSpreadSheetFormulaToken): TList<TDate>;
var
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AHelper: TdxExtractHolidaysHelper;
begin
  Result := TList<TDate>.Create;
  AErrorCode := ecNone;
  AHelper := TdxExtractHolidaysHelper.Create(Result, Sender.FormatSettings.DateTimeSystem);
  try
    TdxTokenAccess(AToken).ForEach(cbExtractHolidays, AHelper, AErrorCode);
  finally
    FreeAndNil(AHelper);
  end;
  if AErrorCode <> ecNone then
    Sender.SetError(AErrorCode)
  else
    Result.Sort;
end;

function dxIsSpreadSheetLeapYear(AYear: Word): Boolean;
begin
  Result := IsLeapYear(AYear) or (AYear = 1900);
end;

function dxGetDays360(const AStart, AEnd: Variant; AMethod: Boolean): Integer;

  function GetGeneralMonthIndex(const AYear, AMonth: Word): Word;
  begin
    Result := (AYear - 1900) * 12 + AMonth;
  end;

var
  AStartYear, AStartMonth, AStartDay: Word;
  AEndYear, AEndMonth, AEndDay: Word;
  ADate: TDate;
begin
  if (AStart <= 2) and (AEnd <= 2) then
  begin
    Result := Trunc(AEnd) - Trunc(AStart);
    Exit;
  end;
  DecodeDate(AStart, AStartYear, AStartMonth, AStartDay);
  DecodeDate(AEnd, AEndYear, AEndMonth, AEndDay);
  if AMethod then
  begin
    AStartDay := Min(AStartDay, 30);
    AEndDay := Min(AEndDay, 30);
  end
  else
  begin
    if (AStartDay = 31) or ((AStartMonth = 2) and (AStartDay = DaysPerMonth(AStartYear, AStartMonth))) then
      AStartDay := 30;
    if AEndDay = 31 then
    begin
      ADate := AEnd + ValueIncr[AStartDay < 30];
      DecodeDate(ADate, AEndYear, AEndMonth, AEndDay);
    end;
  end;
  Result := (GetGeneralMonthIndex(AEndYear, AEndMonth) - GetGeneralMonthIndex(AStartYear, AStartMonth)) * 30 +
    AEndDay - AStartDay;
  Inc(Result, Integer(AStart = 0));
end;

function dxGetIsoWeekNum(ADate: TDate): Word;

  function GetFirstISOYearMonday(AYear: Word): TDate;
  var
    ANewYear: TDate;
    ANewYearWeekDay: Word;
  begin
    ANewYear := EncodeDate(AYear, 1, 1);
    ANewYearWeekDay := DayOfTheWeek(ANewYear);
    if ANewYearWeekDay <= 4 then
      Result := ANewYear - ANewYearWeekDay + 1
    else
      Result := ANewYear + 8 - ANewYearWeekDay;
  end;

var
  AFirstISOYearMonday: TDate;
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(ADate, AYear, AMonth, ADay);
  AFirstISOYearMonday := GetFirstISOYearMonday(AYear + 1);
  if ADate < AFirstISOYearMonday then
    AFirstISOYearMonday := GetFirstISOYearMonday(AYear);
  if ADate < AFirstISOYearMonday then
    AFirstISOYearMonday := GetFirstISOYearMonday(AYear - 1);
  Result := Trunc((ADate - AFirstISOYearMonday) / 7 + 1);
end;

function dxGetWeekDay(ADate: TDate; AType: Integer): Integer;  inline;
var
  AWeekDay: Word;
begin
  Result := -1;
  AWeekDay := DayOfWeek(ADate);
  case AType of
    1, 17:
      Result := AWeekDay;

    2, 11:
      Result := dxMondaysWeek[AWeekDay];

    3:
      Result := dxMondays0Week[AWeekDay];

    12:
      Result := dxTuesdaysWeek[AWeekDay];

    13:
      Result := dxWednesdaysWeek[AWeekDay];

    14:
      Result := dxThursdaysWeek[AWeekDay];

    15:
      Result := dxFridaysWeek[AWeekDay];

    16:
      Result := dxSaturdaysWeek[AWeekDay];
  end;
end;

function dxGetWeekNum(ADate: TDate; AType: Integer): Integer;

  function GetFirstDayOfFirstWeekOfYear: TDate;
  var
    ANewYear: TDate;
    ANewYearWeekDay: Word;
  begin
    ANewYear := EncodeDate(YearOf(ADate), 1, 1);
    ANewYearWeekDay := DayOfWeek(ANewYear);
    case AType of
      2, 11:
        Result := ANewYear - dxMondaysWeek[ANewYearWeekDay] + 1;

      12:
        Result := ANewYear - dxTuesdaysWeek[ANewYearWeekDay] + 1;

      13:
        Result := ANewYear - dxWednesdaysWeek[ANewYearWeekDay] + 1;

      14:
        Result := ANewYear - dxThursdaysWeek[ANewYearWeekDay] + 1;

      15:
        Result := ANewYear - dxFridaysWeek[ANewYearWeekDay] + 1;

      16:
        Result := ANewYear - dxSaturdaysWeek[ANewYearWeekDay] + 1;
    else
        Result := ANewYear - ANewYearWeekDay + 1;
    end;
  end;

begin
  if AType = 21 then
     Result := dxGetIsoWeekNum(ADate)
  else
  begin
    Result := -1;
    if AType in [1, 2, 11, 12, 13, 14, 15, 16, 17] then
      Result := Trunc((ADate - GetFirstDayOfFirstWeekOfYear) / 7 + 1);
  end;
end;

function dxGetXLSDate(AYear, AMonth, ADay: Variant): TDateTime; inline;
begin
  Result := 2;
  if AYear < 1900 then
    AYear := AYear + 1900;
  Result := IncDay(IncMonth(IncYear(Result, AYear - 1900), AMonth - 1), ADay - 1);
end;

function dxGetYearFrac(AStart, AEnd: TDate; ABasis: Integer): Real;

  procedure CheckExchange(var AStartDate, AEndDate: TDate);
  var
    ADate: TDate;
  begin
    if AStartDate < AEndDate then
      Exit;
    ADate := AStartDate;
    AStartDate := AEndDate;
    AEndDate := ADate;
  end;

  function GetAverageDaysPerYears(AYearStart, AYearEnd: Word): Real;
  var
    I: Word;
  begin
    Result := 0;
    for I := AYearStart to AYearEnd do
      Result := Result + DaysPerYear[dxIsSpreadSheetLeapYear(I)];
    Result := Result / (AYearEnd - AYearStart + 1);
  end;

  function GetDaysPerYears(AStartDate, AEndDate: TDate): Real;

    function IsPeriodHasLeapDay: Boolean;
    var
      AYearStart, AYearEnd: Word;
    begin
      AYearStart := YearOf(AStartDate);
      AYearEnd := YearOf(AEndDate);
      Result := dxIsSpreadSheetLeapYear(AYearStart) and dxIsSpreadSheetLeapYear(AYearEnd);
      if not Result then
        if dxIsSpreadSheetLeapYear(AYearStart) then
          Result := (EncodeDate(AYearStart, 3, 1) > AStartDate) and (AEndDate > (EncodeDate(AYearStart, 2, 28)))
        else
          if dxIsSpreadSheetLeapYear(AYearEnd) then
            Result := (EncodeDate(AYearEnd, 3, 1) > AStartDate) and (AEndDate > (EncodeDate(AYearEnd, 2, 28)))
    end;

  begin
    if IncYear(AStartDate, 1) >= AEndDate then
      Result := DaysPerYear[IsPeriodHasLeapDay]
    else
      Result := GetAverageDaysPerYears(YearOf(AStartDate), YearOf(AEndDate));
  end;

const
  ADaysPerYear: array [Boolean] of Word = (360, 365);
begin
  CheckExchange(AStart, AEnd);
  Result := -1;
  case ABasis of
    0, 4:
      Result := dxGetDays360(AStart, AEnd, ABasis = 4) / 360;
    1:
      Result := (AEnd - AStart) / GetDaysPerYears(AStart, AEnd);
    2, 3:
      Result := (AEnd - AStart) / ADaysPerYear[ABasis = 3];
  end;
end;

procedure fnDate(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AYear, AMonth, ADay: Variant;
  ADate: Variant;
  ADateTimeSystem: TdxSpreadSheetDateTimeSystem;
begin
  if Sender.ExtractNumericParameter(AYear, AParams) and Sender.ExtractNumericParameter(AMonth, AParams, 1) and
     Sender.ExtractNumericParameter(ADay, AParams, 2) then
  begin
    ADate := dxGetXLSDate(Trunc(AYear), Trunc(AMonth), Trunc(ADay));
    ADateTimeSystem := Sender.FormatSettings.DateTimeSystem;
    if (ADate < 1) or ((ADateTimeSystem = dts1904) and (ADate < 1462)) then
      Sender.SetError(ecNum)
    else
    begin
      if ADateTimeSystem = dts1904 then
        ADate := dxRealDateTimeToDateTime(ADate, ADateTimeSystem);
      Sender.AddValue(ADate);
    end;
  end;
end;

procedure fnDateValue(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
  ADateTimeSystem: TdxSpreadSheetDateTimeSystem;
begin
  if Sender.ExtractDateTimeOnlyParameter(AParameter, AParams) then
  begin
    ADateTimeSystem := Sender.FormatSettings.DateTimeSystem;
    AParameter := Trunc(AParameter);
    if (AParameter <= 1) or ((ADateTimeSystem = dts1904) and (AParameter < 1462)) then
      Sender.SetError(ecValue)
    else
    if AParameter < 61 then
    begin
      Sender.AddValue(AParameter - 1);
    end
    else
      Sender.AddValue(dxRealDateTimeToDateTime(Trunc(AParameter), ADateTimeSystem));
  end;
end;

procedure fnDay(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
  ADateTimeSystem: TdxSpreadSheetDateTimeSystem;
  ACanConvertStrToNumber: Boolean;
  ADate: TDateTime;
begin
  if Sender.ExtractParameter(AParameter, ACanConvertStrToNumber, AParams) then
  begin
    ADateTimeSystem := Sender.FormatSettings.DateTimeSystem;
    if Sender.ConvertToNumeric(AParameter, ACanConvertStrToNumber, False) then
    begin
      if ADateTimeSystem <> dts1904 then
      begin
        if InRange(AParameter, 0, 60) then
        begin
          if AParameter < 32 then
            Sender.AddValue(AParameter)
          else
            Sender.AddValue(AParameter - 31);
          Exit;
        end;
      end;
      AParameter := dxDateTimeToRealDateTime(AParameter, ADateTimeSystem);
      Sender.AddValue(DayOf(AParameter));
    end
    else
    if not dxConvertToXLSDate(AParameter, ADate) or ((ADateTimeSystem = dts1904) and (ADate < 1462)) then
      Sender.SetError(ecValue)
    else
    begin
      Sender.SetError(ecNone);
      Sender.AddValue(DayOf(ADate));
    end;
  end;
end;

procedure fnDays(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AEnd, AStart: Variant;
begin
  if Sender.ExtractDateTimeParameter(AEnd, AParams) and Sender.ExtractDateTimeParameter(AStart, AParams.Next) then
    Sender.AddValue(Trunc(AEnd) - Trunc(AStart));
end;

procedure fnDays360(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

   function ExtractParameter(var AParameter: Variant; AParam: TdxSpreadSheetFormulaToken): Boolean;
   const
     AInteger: array[Boolean] of Integer = (1, 2);
   begin
     Result := Sender.ExtractDateTimeParameterWithoutBoolean(AParameter, AParam);
     if not Result then
     begin
       Sender.SetError(ecNone);
       Result := Sender.ExtractParameter(AParameter, AParam) and (VarType(AParameter) = varBoolean);
       if Result then
       begin
         AParameter := dxDateTimeToRealDateTime(AInteger[AParameter = True], Sender.FormatSettings.DateTimeSystem);
         if Sender.FormatSettings.DateTimeSystem = dts1904 then
           AParameter := AParameter - 1;
       end
       else
         if Sender.Validate then
           Sender.SetError(ecValue);
     end;
   end;

var
  AStart, AEnd, AMethod: Variant;
begin
  if ExtractParameter(AStart, AParams) and ExtractParameter(AEnd, AParams.Next) and
     Sender.ExtractNumericParameterDef(AMethod, False, False, AParams, 2) then
    Sender.AddValue(dxGetDays360(Trunc(Integer(AStart)), Trunc(Integer(AEnd)), AMethod <> 0));
end;

function dxExtractStartDateAndMonthCount(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  out ADate, AMonthCount: Variant): Boolean;
begin
  Result := False;
  if dxSpreadSheetExtractedArgumentsAreNotNull(Sender, AParams, ADate, AMonthCount) then
    Result := Sender.ExtractDateTimeParameterWithoutBoolean(ADate, AParams) and
      Sender.ExtractNumericParameterWithoutBoolean(AMonthCount, AParams.Next) and Sender.Validate;
end;

procedure fnEDate(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ADate, AMonthCount: Variant;
begin
  if dxExtractStartDateAndMonthCount(Sender, AParams, ADate, AMonthCount) then
  begin
    ADate := IncMonth(Trunc(ADate), Trunc(AMonthCount));
    Sender.AddValue(dxRealDateTimeToDateTime(ADate, Sender.FormatSettings.DateTimeSystem));
  end;
end;

procedure fnEOMonth(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ADate, AMonthCount: Variant;
  AYear, AMonth, ADay: Word;
begin
  if dxExtractStartDateAndMonthCount(Sender, AParams, ADate, AMonthCount) then
  begin
    ADate := Trunc(ADate);
    AMonthCount := Trunc(AMonthCount);
    ADate := IncMonth(ADate, AMonthCount);
    DecodeDate(ADate, AYear, AMonth, ADay);
    ADate := EncodeDate(AYear, AMonth, DaysPerMonth(AYear, AMonth));
    Sender.AddValue(dxRealDateTimeToDateTime(ADate, Sender.FormatSettings.DateTimeSystem));
  end;
end;

procedure fnHour(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractDateTimeParameter(AParameter, AParams) then
    if AParameter < 0 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(HourOf(AParameter));
end;

procedure fnIsoWeekNum(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractDateTimeParameter(AParameter, AParams) then
    Sender.AddValue(dxGetIsoWeekNum(Trunc(AParameter)));
end;

procedure fnMinute(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractDateTimeParameter(AParameter, AParams) then
    if AParameter < 0 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(MinuteOf(AParameter));
end;

procedure fnMonth(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractDateTimeParameter(AParameter, AParams) then
    if AParameter < 0 then
      Sender.SetError(ecNUM)
    else
      if AParameter <= 2 then
        Sender.AddValue(1)
      else
        Sender.AddValue(MonthOf(dxDateTimeToRealDateTime(AParameter, Sender.FormatSettings.DateTimeSystem)));
end;

procedure dxExtractWeekEnd(Sender: TdxSpreadSheetFormulaResult;
  const AParams: TdxSpreadSheetFormulaToken; AIntl: Boolean; var AWeekEnd: Variant; var AWeekEndDaysCount: Integer);
var
  AWeekEndInt, I: Integer;
  AWeekEndStr: string;
begin
  if not AIntl or (Sender.GetParamsCount(AParams) < 3) then
  begin
    AWeekEnd := 1;
    AWeekEndDaysCount := 2;
  end
  else
  if Sender.ExtractParameterDef(AWeekEnd, 1, 1, AParams, 2) then
  begin
    if VarIsStr(AWeekEnd) then
    begin
      AWeekEndStr := VarToStr(AWeekEnd);
      if (Length(AWeekEndStr) <> 7) then
        Sender.SetError(ecValue)
      else
      begin
        AWeekEndDaysCount := 0;
        for I := 1 to 7 do
        begin
          if (AWeekEndStr[I] <> '1') and (AWeekEndStr[I] <> '0') then
            Sender.SetError(ecValue)
          else
            if AWeekEndStr[I] = '1' then
              Inc(AWeekEndDaysCount);
          if not Sender.Validate then
            Break;
        end;
      end;
    end
    else
    begin
      AWeekEndInt := Trunc(AWeekEnd);
      if not(AWeekEndInt in [1, 2, 3, 4, 5, 6, 7, 11, 12, 13, 14, 15, 16, 17]) then
        Sender.SetError(ecNUM)
      else
        if AWeekEndInt < 11 then
          AWeekEndDaysCount := 2
        else
          AWeekEndDaysCount := 1;
      AWeekEnd := AWeekEndInt;
    end;
  end;
end;

function dxExtractHolidays(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AIntl: Boolean): TList<TDate>;
begin
  Result := nil;
  if AIntl then
  begin
    if Sender.GetParamsCount(AParams) = 4 then
      Result := dxCreateHolidaysList(Sender, AParams.Next.Next.Next.FirstChild);
  end
  else
    if Sender.GetParamsCount(AParams) = 3 then
      Result := dxCreateHolidaysList(Sender, AParams.Next.Next.FirstChild);
end;

function dxDateInRange(const ADate, AStart, AEnd: TDate; AForward: Boolean): Boolean;
begin
  if AForward then
    Result := (ADate >= AStart) and (ADate <= AEnd)
  else
    Result := (ADate >= AEnd) and (ADate <= AStart);
end;

procedure dxCalculateNetworkDays(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken; AIntl: Boolean);
var
  ASign: Integer;

  function ProcessOtherDays(AStart, ACount: Integer; const AWeekEnd: Variant): Integer;
  var
    I: Integer;
    AWeekEndString: string;
  begin
    Result := 0;
    if ACount = 0 then
      Exit;
    AWeekEndString := '';
    if VarIsStr(AWeekEnd) then
      AWeekEndString :=  VarToStr(AWeekEnd);
    for I := 0 to ACount - 1 do
      if AWeekEndString <> '' then
      begin
        if AWeekEndString[DayOfTheWeek(AStart + ASign * I)] = '1' then
          Inc(Result);
      end
      else
      if dxIsNonWorkDay(AStart + ASign * I, AWeekEnd) then
        Inc(Result);
  end;

var
  AStart, AFinish: Variant;
  AWeekEnd: Variant;
  AHolidays: TList<TDate>;
  AResult, AFullWeeks, AOtherDays, AWeekEndDaysCount, I: Integer;
  AHoliday: TDate;
  AIsIsNonWorkDay: Boolean;
begin
  if not Sender.ExtractDateTimeParameterWithoutBoolean(AStart, AParams) then
    Exit;
  if AStart < 0 then
  begin
    Sender.SetError(ecNUM);
    Exit;
  end;
  if not Sender.ExtractDateTimeParameterWithoutBoolean(AFinish, AParams.Next) then
    Exit;
  if AFinish < 0 then
    Sender.SetError(ecNUM);
  if Sender.Validate then
  begin
    dxExtractWeekEnd(Sender, AParams, AIntl, AWeekEnd, AWeekEndDaysCount);
    if VarIsStr(AWeekEnd) and (VarToStr(AWeekEnd) = '1111111') then
      AResult := 0
    else
    begin
      if not Sender.Validate then
        Exit;
      AStart := Trunc(AStart);
      AFinish := Trunc(AFinish);
      ASign := 1;
      if AFinish < AStart then
        ASign := -1;
      AResult := AFinish - AStart + ASign;
      AFullWeeks := Abs(AResult div 7);
      AOtherDays := Abs(AResult) - AFullWeeks * 7;
      AResult := AResult - ASign * AFullWeeks * AWeekEndDaysCount -
        ASign * ProcessOtherDays(AStart + ASign * AFullWeeks * 7, AOtherDays, AWeekEnd);
      if AResult <> 0 then
      begin
        AHolidays := dxExtractHolidays(Sender, AParams, AIntl);
        try
          if Sender.Validate and (AHolidays <> nil) then
            for I := 0 to AHolidays.Count - 1 do
            begin
              if ASign = 1 then
                AHoliday := AHolidays[I]
              else
                AHoliday := AHolidays[AHolidays.Count - 1 - I];
              if VarIsStr(AWeekEnd) then
                AIsIsNonWorkDay := VarToStr(AWeekEnd)[DayOfTheWeek(AHoliday)] = '1'
              else
                AIsIsNonWorkDay := dxIsNonWorkDay(AHoliday, AWeekEnd);
              if not AIsIsNonWorkDay and dxDateInRange(AHoliday, AStart, AFinish, ASign = 1) then
                Dec(AResult, ASign);
            end;
        finally
          FreeAndNil(AHolidays);
        end;
      end;
    end;
    if Sender.Validate then
      Sender.AddValue(AResult);
  end;
end;

procedure fnNetworkDays(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCalculateNetworkDays(Sender, AParams, False);
end;

procedure fnNetworkDays_Intl(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCalculateNetworkDays(Sender, AParams, True);
end;

procedure fnNow(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  Sender.AddValue(dxRealDateTimeToDateTime(Now, Sender.FormatSettings.DateTimeSystem));
end;

procedure fnSecond(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractDateTimeParameter(AParameter, AParams) then
    if AParameter < 0 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(SecondOf(AParameter));
end;

procedure fnTime(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AHour, AMinute, ASecond: Variant;
  AResult: Variant;
begin
  if Sender.ExtractNumericParameter(AHour, AParams) and Sender.ExtractNumericParameter(AMinute, AParams.Next) and
     Sender.ExtractNumericParameter(ASecond, AParams, 2) then
  begin
    AResult := AHour * 60 * 60 + AMinute * 60 + ASecond;
    if AResult < 0  then
      Sender.SetError(ecNUM);
    Sender.AddValue(Frac(AResult / 24 / 60 / 60));
  end;
end;

procedure fnTimeValue(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AParameter: Variant;
begin
  if Sender.ExtractDateTimeOnlyParameter(AParameter, AParams) then
    Sender.AddValue(Frac(AParameter));
end;

procedure fnToday(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  Sender.AddValue(dxRealDateTimeToDateTime(Date, Sender.FormatSettings.DateTimeSystem));
end;

procedure fnWeekDay(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AType: Variant;
  AIntType: Integer;
  AParameter: Variant;
  ADateTimeSystem: TdxSpreadSheetDateTimeSystem;
  ACanConvertStrToNumber: Boolean;
  ADate: TDateTime;
begin
  if not Sender.ExtractParameter(AParameter, ACanConvertStrToNumber, AParams) then
    Exit;

  ADateTimeSystem := Sender.FormatSettings.DateTimeSystem;
  if Sender.ConvertToNumeric(AParameter, ACanConvertStrToNumber, False) then
    AParameter := dxDateTimeToRealDateTime(AParameter, ADateTimeSystem)
  else
  if not dxConvertToXLSDate(AParameter, ADate) or ((ADateTimeSystem = dts1904) and (ADate < 1462)) then
    Sender.SetError(ecValue)
  else
  begin
    Sender.SetError(ecNone);
    AParameter := ADate;
    if (ADateTimeSystem <> dts1904) and (ADate <= 60) then
      AParameter := AParameter - 1;
  end;

  if Sender.Validate and Sender.ExtractNumericParameterDef(AType, 1, 0, AParams, 1) then
  begin
    ADate := Trunc(AParameter);
    AIntType := Trunc(AType);
    if AIntType in [1, 2, 3, 11, 12, 13, 14, 15, 16, 17] then
      Sender.AddValue(dxGetWeekDay(ADate, AIntType))
    else
      Sender.SetError(ecNUM);
  end;
end;

procedure fnWeekNum(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AType: Variant;
  AIntType: Integer;
  AParameter: Variant;
  ADateTimeSystem: TdxSpreadSheetDateTimeSystem;
  ACanConvertStrToNumber: Boolean;
  ADate: TDateTime;
begin
  if not Sender.ExtractParameter(AParameter, ACanConvertStrToNumber, AParams) then
    Exit;

  ADateTimeSystem := Sender.FormatSettings.DateTimeSystem;
  if Sender.ConvertToNumeric(AParameter, ACanConvertStrToNumber, True) then
    AParameter := dxDateTimeToRealDateTime(AParameter, ADateTimeSystem)
  else
  if not dxConvertToXLSDate(AParameter, ADate) or ((ADateTimeSystem = dts1904) and (ADate < 1462)) then
    Sender.SetError(ecValue)
  else
  begin
    Sender.SetError(ecNone);
    AParameter := ADate;
    if (ADateTimeSystem <> dts1904) and (ADate <= 60) then
      AParameter := AParameter - 1;
  end;

  if Sender.Validate and Sender.ExtractNumericParameterDefWithoutBoolean(AType, 1, AParams.Next) then
  begin
    ADate := Trunc(AParameter);
    AIntType := Trunc(AType);
    if AIntType in [1, 2, 11, 12, 13, 14, 15, 16, 17, 21] then
      Sender.AddValue(dxGetWeekNum(ADate, AIntType))
    else
      Sender.SetError(ecNUM);
  end;
end;

procedure dxCalculateWorkDay(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken; AIntl: Boolean);
var
  ASign: Integer;

  function ProcessOtherDays(AStart, ACount: Integer; const AWeekEnd: Variant): Integer;
  var
    I: Integer;
    AWeekEndString: string;
  begin
    Result := ASign * ACount;
    if Result = 0 then
      Exit;
    I := 1;
    AWeekEndString := '';
    if VarIsStr(AWeekEnd) then
      AWeekEndString :=  VarToStr(AWeekEnd);
    repeat
      if AWeekEndString <> '' then
      begin
        if AWeekEndString[DayOfTheWeek(AStart + ASign * I)] = '1' then
        begin
          Inc(Result, ASign);
          Inc(ACount);
        end;
      end
      else
      if dxIsNonWorkDay(AStart + ASign * I, AWeekEnd) then
      begin
        Inc(Result, ASign);
        Inc(ACount);
      end;
      Inc(I);
    until I > ACount;
  end;

  procedure AddResult(AValue: TDateTime);
  begin
    if Sender.Validate then
      if AValue > 0 then
        Sender.AddValue(AValue)
      else
        Sender.SetError(ecNUM);
  end;

var
  AStart, ADays: Variant;
  ACount: Integer;
  AHolidays: TList<TDate>;
  AResult, AFullWeeks, AOtherDays, AWeekEndDaysCount, I: Integer;
  AHoliday: TDate;
  AWeekEnd: Variant;
  AIsIsNonWorkDay: Boolean;
begin
  if dxExtractStartDateAndMonthCount(Sender, AParams, AStart, ADays) then
  begin
    if AStart < 0 then
    begin
      Sender.SetError(ecNUM);
      Exit;
    end;
    dxExtractWeekEnd(Sender, AParams, AIntl, AWeekEnd, AWeekEndDaysCount);
    if VarIsStr(AWeekEnd) and (VarToStr(AWeekEnd) = '1111111') then
      Sender.SetError(ecValue);
    if not Sender.Validate then
      Exit;
    ACount := Trunc(ADays);
    if (ADays < 0) and (Trunc(ADays) <> ADays) then
      Dec(ACount);
    ASign := 1;
    if ACount < 0 then
      ASign := -1;
    AResult := Trunc(AStart);
    if ACount <> 0 then
    begin
      AHolidays := dxExtractHolidays(Sender, AParams, AIntl);
      try
        AFullWeeks := Abs(ACount div 7);
        AOtherDays := Abs(ACount) - AFullWeeks * 7;
        AResult := AResult + ASign * AFullWeeks * 7;
        if AResult > 0 then
        begin
          Inc(AResult, ProcessOtherDays(AResult, AFullWeeks * AWeekEndDaysCount, AWeekEnd));
          if AResult > 0 then
          begin
            Inc(AResult, ProcessOtherDays(AResult, AOtherDays, AWeekEnd));
            if (AResult > 0) and (AHolidays <> nil) then
              for I := 0 to AHolidays.Count - 1 do
              begin
                if ASign = 1 then
                  AHoliday := AHolidays[I]
                else
                  AHoliday := AHolidays[AHolidays.Count - 1 - I];
                if VarIsStr(AWeekEnd) then
                  AIsIsNonWorkDay := VarToStr(AWeekEnd)[DayOfTheWeek(AHoliday)] = '1'
                else
                  AIsIsNonWorkDay := dxIsNonWorkDay(AHoliday, AWeekEnd);
                if not AIsIsNonWorkDay and dxDateInRange(AHoliday, AStart + ASign, AResult, ASign = 1) then
                begin
                  Inc(AResult, ProcessOtherDays(AResult, 1, AWeekEnd));
                  if AResult < 1 then Break;
                end;
              end;
          end;
        end;
        AddResult(dxRealDateTimeToDateTime(AResult, Sender.FormatSettings.DateTimeSystem));
      finally
        FreeAndNil(AHolidays);
      end;
    end
    else
      AddResult(dxRealDateTimeToDateTime(AResult, Sender.FormatSettings.DateTimeSystem));
  end;
end;

procedure fnWorkDay(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCalculateWorkDay(Sender, AParams, False);
end;

procedure fnWorkDay_Intl(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCalculateWorkDay(Sender, AParams, True);
end;

procedure fnYear(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
const
  AFirstYear: array[Boolean] of Integer = (1900, 1904);
var
  AParameter: Variant;
begin
  if Sender.ExtractDateTimeParameter(AParameter, AParams) then
    if AParameter < 0 then
      Sender.SetError(ecNUM)
    else
      if AParameter <= 2 then
        Sender.AddValue(AFirstYear[Sender.FormatSettings.DateTimeSystem = dts1904])
      else
       Sender.AddValue(YearOf(AParameter));
end;

procedure fnYearFrac(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AStart, AEnd, ABasis: Variant;
begin
  if Sender.ExtractDateTimeParameterWithoutBoolean(AStart, AParams) and Sender.ExtractDateTimeParameterWithoutBoolean(AEnd, AParams.Next) and
     Sender.ExtractNumericParameterDefWithoutBoolean(ABasis, 0, AParams, 2)  then
  begin
    AStart := Trunc(AStart);
    AEnd := Trunc(AEnd);
    ABasis := Trunc(ABasis);
    if (ABasis < 0) or (ABasis > 4) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetYearFrac(AStart, AEnd, ABasis));
  end;
end;

procedure fpiDate(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiDateValue(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiDay(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiDays(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiDays360(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredValue;
end;

procedure fpiEDate(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiEOMonth(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiHour(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiIsoWeekNum(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiMinute(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiMonth(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiNetworkDays(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredArray;
end;

procedure fpiNetworkDays_Intl(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredValue;
  AParamKind[3] := fpkNonRequiredArray;
end;

procedure fpiNow(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(0, AParamCount, AParamKind);
end;

procedure fpiSecond(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiTime(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiTimeValue(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiToday(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(0, AParamCount, AParamKind);
end;

procedure fpiWeekDay(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkNonRequiredValue;
end;

procedure fpiWeekNum(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkNonRequiredValue;
end;

procedure fpiWorkDay(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredArray;
end;

procedure fpiWorkDay_Intl(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredValue;
  AParamKind[3] := fpkNonRequiredArray;
end;

procedure fpiYear(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiYearFrac(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredValue;
end;

{ RegisterFunctions }

procedure RegisterFunctions(ARepository: TdxSpreadSheetFunctionsRepository);
begin
  ARepository.Add(@sfnDate, fnDate, fpiDate, frkValue, 65, ftDateTime, @sfnDateDescription);
  ARepository.Add(@sfnDateValue, fnDateValue, fpiDateValue, frkValue, 140, ftDateTime, @sfnDateValueDescription);
  ARepository.Add(@sfnDay, fnDay, fpiDay, frkValue, 67, ftDateTime, @sfnDayDescription);
  ARepository.Add(@sfnDays, fnDays, fpiDays, frkValue, 255, ftDateTime, @sfnDaysDescription, 1);
  ARepository.Add(@sfnDays360, fnDays360, fpiDays360, frkValue, 220, ftDateTime, @sfnDays360Description);
  ARepository.Add(@sfnEDate, fnEDate, fpiEDate, frkValue, 255, ftDateTime, @sfnEDateDescription);
  ARepository.Add(@sfnEOMonth, fnEOMonth, fpiEOMonth, frkValue, 255, ftDateTime, @sfnEOMonthDescription);
  ARepository.Add(@sfnHour, fnHour, fpiHour, frkValue, 71, ftDateTime, @sfnHourDescription);
  ARepository.Add(@sfnIsoWeekNum, fnIsoWeekNum, fpiIsoWeekNum, frkValue, 255, ftDateTime, @sfnIsoWeekNumDescription, 1);
  ARepository.Add(@sfnMinute, fnMinute, fpiMinute, frkValue, 72, ftDateTime, @sfnMinuteDescription);
  ARepository.Add(@sfnMonth, fnMonth, fpiMonth, frkValue, 68, ftDateTime, @sfnMonthDescription);
  ARepository.Add(@sfnNetworkDays, fnNetworkDays, fpiNetworkDays, frkValue, 255, ftDateTime, @sfnNetworkDaysDescription);
  ARepository.Add(@sfnNetworkDays_Intl, fnNetworkDays_Intl, fpiNetworkDays_Intl, frkValue, 255, ftDateTime, @sfnNetworkDays_IntlDescription);
  ARepository.Add(@sfnNow, fnNow, fpiNow, frkValue, 74, ftDateTime, @sfnNowDescription);
  ARepository.Add(@sfnSecond, fnSecond, fpiSecond, frkValue, 73, ftDateTime, @sfnSecondDescription);
  ARepository.Add(@sfnTime, fnTime, fpiTime, frkValue, 66, ftDateTime, @sfnTimeDescription);
  ARepository.Add(@sfnTimeValue, fnTimeValue, fpiTimeValue, frkValue, 141, ftDateTime, @sfnTimeValueDescription);
  ARepository.Add(@sfnToday, fnToday, fpiToday, frkValue, 221, ftDateTime, @sfnTodayDescription);
  ARepository.Add(@sfnWeekDay, fnWeekDay, fpiWeekDay, frkValue, 70, ftDateTime, @sfnWeekDayDescription);
  ARepository.Add(@sfnWeekNum, fnWeekNum, fpiWeekNum, frkValue, 255, ftDateTime, @sfnWeekNumDescription);
  ARepository.Add(@sfnWorkDay, fnWorkDay, fpiWorkDay, frkValue, 255, ftDateTime, @sfnWorkDayDescription);
  ARepository.Add(@sfnWorkDay_Intl, fnWorkDay_Intl, fpiWorkDay_Intl, frkValue, 255, ftDateTime, @sfnWorkDay_IntlDescription);
  ARepository.Add(@sfnYear, fnYear, fpiYear, frkValue, 69, ftDateTime, @sfnYearDescription);
  ARepository.Add(@sfnYearFrac, fnYearFrac, fpiYearFrac, frkValue, 255, ftDateTime, @sfnYearFracDescription);
end;

initialization
  RegisterFunctions(dxSpreadSheetFunctionsRepository);
end.
