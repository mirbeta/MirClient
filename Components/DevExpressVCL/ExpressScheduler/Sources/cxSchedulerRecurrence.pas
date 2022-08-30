{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxSchedulerRecurrence;

{$I cxVer.inc}

interface

uses
  Types, Variants, DateUtils, Classes, SysUtils, cxDateUtils, cxSchedulerUtils, cxFormats;

type
  TcxDayType = (cxdtDay, cxdtEveryDay, cxdtWeekDay, cxdtWeekEndDay, cxdtSunday,
    cxdtMonday, cxdtTuesday, cxdtWednesday, cxdtThursday, cxdtFriday, cxdtSaturday);

  TcxRecurrence = (cxreDaily, cxreWeekly, cxreMonthly, cxreYearly);

  TcxRecurrenceValidStatus = (rvsValid, rvsReplaceOccurrenceDate, rvsInvalidPattern,
    rvsInvalidDuration);

  TcxSchedulerCustomRecurrenceInfo = class;

  { TcxSchedulerRecurrenceCalculator }

  TcxSchedulerRecurrenceCalculator = class
  private
    FOwner: TObject;
  protected
    FActualStart: TDateTime;
    FDate: Integer;
    FDay: Word;
    FDayNumber: Integer;
    FDayType: TcxDayType;
    FDismissDate: TDateTime;
    FFinishDate: TDateTime;
    FIndex: Integer;
    FMonth: Word;
    FOccurDays: TDays;
    FOccurrenceFinish: TDateTime;
    FOccurrenceStart: TDateTime;
    FPeriodicity: Integer;
    FRecurCount: Integer;
    FRecurrence: TcxRecurrence;
    FStartOfWeek: Integer;
    FVisibleFinish: TDateTime;
    FVisibleStart: TDateTime;
    FWeekStart: Integer;
    FWorkDays: TDays;
    FYear: Word;
    FYearPeriodicity: Integer;

    procedure CalcFirstDate;
    procedure CalcNextDate;
    function GetCalcForReminders: Boolean; virtual;
    function GetCertainDay(const ADate, ANumber: Integer; ADayType: TcxDayType): Integer;
    function GetDayForMonth: Integer; virtual;
    function GetDuration: TDateTime; virtual; abstract;
    function GetDayFromOccurDays(const APrevDate, APeriodicity: Integer): Integer;
    function GetRecurrenceInfo: TcxSchedulerCustomRecurrenceInfo; virtual; abstract;
    function GetSomeDay(const ADate, ANumber: Integer; AWeekEnd: Boolean): Integer;
    function GetStart: TDateTime; virtual; abstract;
    function GetTimeBias: Double; virtual; abstract;
    procedure InitTimes; virtual;
    function IsValidOccurrence: Boolean;

    property CalcForReminders: Boolean read GetCalcForReminders;
    property Owner: TObject read FOwner;
    property RecurrenceInfo: TcxSchedulerCustomRecurrenceInfo read GetRecurrenceInfo;
  public
    constructor Create(AOwner: TObject; const AStart, AFinish: TDateTime);
    procedure CalcOccurrence(AIndex: Integer);
    function GetNextOccurrence: Boolean; virtual;

    property Index: Integer read FIndex;
    property OccurrenceFinish: TDateTime read FOccurrenceFinish;
    property OccurrenceStart: TDateTime read FOccurrenceStart;
    property StartOfWeek: Integer read FStartOfWeek;
    property VisibleFinish: TDateTime read FVisibleFinish;
    property WorkDays: TDays read FWorkDays write FWorkDays;
  end;

  TcxSchedulerRecurrenceCalculatorClass = class of TcxSchedulerRecurrenceCalculator;

  TcxSchedulerCustomRecurrenceInfoData = packed record
    Count: Integer;
    DayNumber: Integer;
    DayType: TcxDayType;
    Finish: TDateTime;
    OccurDays: TDays;
    Periodicity: Integer;
    Recurrence: TcxRecurrence;
    Start: TDateTime;
    YearPeriodicity: Integer;
    Version: Byte;
    DismissDate: Integer;
  end;

  { TcxSchedulerCustomRecurrenceInfo }

  TcxSchedulerCustomRecurrenceInfo = class(TPersistent)
  private
    FDisplayTimeBias: Double;
    FOwner: TObject;
    function GetCount: Integer;
    function GetDayNumber: Integer;
    function GetDayType: TcxDayType;
    function GetDismissDate: TDateTime;

    function GetIsInfinity: Boolean;
    function GetOccurDays: TDays;
    function GetOriginalStart: TDateTime;
    function GetPeriodicity: Integer;
    function GetRecurrence: TcxRecurrence;

    function GetYearPeriodicity: Integer;
    procedure SetCount(const AValue: Integer);
    procedure SetDayNumber(const AValue: Integer);
    procedure SetDayType(const AValue: TcxDayType);
    procedure SetDismissDate(const AValue: TDateTime);

    procedure SetOccurDays(const AValue: TDays);
    procedure SetOriginalStart(const AValue: TDateTime);
    procedure SetPeriodicity(const AValue: Integer);
    procedure SetRecurrence(const AValue: TcxRecurrence);

    procedure SetYearPeriodicity(const AValue: Integer);
  protected
    function GetFinish: TDateTime; virtual; abstract;
    function GetStart: TDateTime; virtual; abstract;
    procedure SetFinish(AValue: TDateTime); virtual; abstract;
    procedure SetStart(const AValue: TDateTime); virtual; abstract;

    procedure AssignDefaultValues; virtual;
    function GetData: TcxSchedulerCustomRecurrenceInfoData; virtual; abstract;
    function GetCalculatorClass: TcxSchedulerRecurrenceCalculatorClass; virtual;
    function GetOwner: TPersistent; override;

    function GetValue(var AValue: AnsiString): Boolean; virtual; abstract;
    procedure SetDataItem(AOffset: Pointer; ASize: Integer; const AValue); virtual; abstract;
    procedure SetValue(const AValue: AnsiString); virtual; abstract;
    // validate
    function GetDailyPatternStatus: TcxRecurrenceValidStatus;
    function GetMonthlyPatternStatus: TcxRecurrenceValidStatus;
    function GetWeeklyPatternStatus: TcxRecurrenceValidStatus;
    function GetYearlyPatternStatus: TcxRecurrenceValidStatus;

    property Owner: TObject read FOwner;
  public
    constructor Create(AOwner: TObject); virtual;
    procedure Assign(Source: TPersistent); override;
    function GetEndDate: TDateTime;

    property Count: Integer read GetCount write SetCount;
    property DayNumber: Integer read GetDayNumber write SetDayNumber;
    property DayType: TcxDayType read GetDayType write SetDayType;
    property DismissDate: TDateTime read GetDismissDate write SetDismissDate;
    property DisplayTimeBias: Double read FDisplayTimeBias write FDisplayTimeBias;
    property Finish: TDateTime read GetFinish write SetFinish;
    property IsInfinity: Boolean read GetIsInfinity;
    property OccurDays: TDays read GetOccurDays write SetOccurDays;
    property OriginalStart: TDateTime read GetOriginalStart write SetOriginalStart;
    property Periodicity: Integer read GetPeriodicity write SetPeriodicity;
    property Recurrence: TcxRecurrence read GetRecurrence write SetRecurrence;
    property Start: TDateTime read GetStart write SetStart;
    property YearPeriodicity: Integer read GetYearPeriodicity write SetYearPeriodicity;
  end;

const
  DefInfoData: TcxSchedulerCustomRecurrenceInfoData =
    (Count: 10; DayNumber: 1; DayType: cxdtDay;  Finish: -1;
     OccurDays: []; Periodicity: 1; Recurrence: cxreDaily;
     Start: 0; YearPeriodicity: 1; Version: 1; DismissDate: 0);

function cxGetCustomRecurrenceDescriptionString(ARecurrenceInfo: TcxSchedulerCustomRecurrenceInfo): string;
function cxRecurrenceInfoDataToString(const AData: TcxSchedulerCustomRecurrenceInfoData): AnsiString;
function cxStringToRecurrenceInfoData(const S: AnsiString): TcxSchedulerCustomRecurrenceInfoData;

implementation

uses
  cxSchedulerStorage;

const
  PatternValidStatus: array[Boolean] of TcxRecurrenceValidStatus =
    (rvsInvalidPattern, rvsValid);

  cxRecurrenceInfoDataVersion = 1;

function cxGetCustomRecurrenceDescriptionString(
  ARecurrenceInfo: TcxSchedulerCustomRecurrenceInfo): string;
const
  Weeks: array[1..5] of string = ('first', 'second', 'third', 'fourth', 'last');
  Days: array[cxdtEveryDay..cxdtWeekEndDay] of string = ('day', 'weekday', 'weekend day');
  EveryDays: array[Boolean] of string = ('every %d days', 'every day');
  EveryMonths1: array[Boolean] of string = ('day %d of every %d months', 'day %d of every month');
  EveryMonths2: array[Boolean] of string = ('the %s %s of every %d months', 'the %s %s of every month');

  procedure GetDateParts(out ADayStr, AWeekStr: string);
  begin
    with ARecurrenceInfo do
    begin
      if DayNumber in [1..5] then AWeekStr := Weeks[DayNumber] else AWeekStr := '';
      if DayType in [cxdtEveryDay..cxdtWeekEndDay] then
        ADayStr := Days[DayType]
      else
        ADayStr := dxFormatSettings.LongDayNames[Ord(DayType) - Ord(cxdtSunday) + 1];
    end;
  end;

  function OccurDaysToString: string;
  var
    ADay: TDay;
    ACount, ASaveCount: Integer;
  begin
    Result := '';
    ACount := 0;
    for ADay := Low(TDay) to High(TDay) do
      if ADay in ARecurrenceInfo.OccurDays then Inc(ACount);
    ASaveCount := ACount;
    for ADay := Low(TDay) to High(TDay) do
      if ADay in ARecurrenceInfo.OccurDays then
        if ASaveCount = 1 then
        begin
          Result := dxFormatSettings.LongDayNames[Ord(ADay) + 1];
          Exit;
        end
        else
        begin
          Dec(ACount);
          if ACount > 1 then
            Result := Result + dxFormatSettings.LongDayNames[Ord(ADay) + 1] + ', '
          else if ACount = 1 then
            Result := Result + dxFormatSettings.LongDayNames[Ord(ADay) + 1] + ' '
          else
            Result := Result + 'and ' + dxFormatSettings.LongDayNames[Ord(ADay) + 1];
        end;
  end;

var
  ADayStr, AWeekStr, AMonthStr: string;
begin
  Result := '';
  case ARecurrenceInfo.Recurrence of
    cxreDaily:
      if ARecurrenceInfo.DayType = cxdtWeekDay then
        Result := 'every weekday'
      else
        Result := Format(EveryDays[ARecurrenceInfo.Periodicity = 1], [ARecurrenceInfo.Periodicity]);
    cxreWeekly:
      if ARecurrenceInfo.Periodicity = 1 then
        Result := 'every ' + OccurDaysToString
      else
        Result := Format('every %d weeks on %s', [ARecurrenceInfo.Periodicity, OccurDaysToString]);
    cxreMonthly:
      if ARecurrenceInfo.DayType = cxdtDay then
        Result := Format(EveryMonths1[ARecurrenceInfo.Periodicity = 1], [ARecurrenceInfo.DayNumber, ARecurrenceInfo.Periodicity])
      else
      begin
        GetDateParts(ADayStr, AWeekStr);
        Result := Format(EveryMonths2[ARecurrenceInfo.Periodicity = 1], [AWeekStr, ADayStr, ARecurrenceInfo.Periodicity]);
      end;
    cxreYearly:
      begin
        if ARecurrenceInfo.Periodicity in [1..12] then
          AMonthStr := dxFormatSettings.LongMonthNames[ARecurrenceInfo.Periodicity]
        else
          AMonthStr := '';
        if ARecurrenceInfo.DayType = cxdtDay then
          Result := Format('every %s %d', [AMonthStr, ARecurrenceInfo.DayNumber])
        else
        begin
          GetDateParts(ADayStr, AWeekStr);
          Result := Format('the %s %s of %s', [AWeekStr, ADayStr, AMonthStr]);
        end;
      end;
  end;
end;

function cxRecurrenceInfoDataToString(const AData: TcxSchedulerCustomRecurrenceInfoData): AnsiString;
begin
  SetLength(Result, SizeOf(AData));
  Move(AData, Result[1], SizeOf(AData));
end;

function cxStringToRecurrenceInfoData(const S: AnsiString): TcxSchedulerCustomRecurrenceInfoData;
begin
  if Length(S) = SizeOf(TcxSchedulerCustomRecurrenceInfoData) then
    Move(S[1], Result, SizeOf(TcxSchedulerCustomRecurrenceInfoData))
  else
    Result := DefInfoData;
end;

{ TcxSchedulerOccurrenceCalculator }

constructor TcxSchedulerRecurrenceCalculator.Create(AOwner: TObject;
  const AStart, AFinish: TDateTime);
begin
  inherited Create;
  FOwner := AOwner;
  FWorkDays := DateTimeHelper.WorkDays;
  FStartOfWeek := DateTimeHelper.StartOfWeek;
  FVisibleStart := AStart - RecurrenceInfo.DisplayTimeBias;
  FVisibleFinish := AFinish - RecurrenceInfo.DisplayTimeBias;
  FRecurCount := RecurrenceInfo.Count;
  InitTimes;
  FDayNumber := RecurrenceInfo.DayNumber;
  FDayType := RecurrenceInfo.DayType;
  FDismissDate := RecurrenceInfo.DismissDate;
  FOccurDays := RecurrenceInfo.OccurDays;
  FPeriodicity := RecurrenceInfo.Periodicity;
  FRecurrence := RecurrenceInfo.Recurrence;
  FYearPeriodicity := RecurrenceInfo.YearPeriodicity;
end;

procedure TcxSchedulerRecurrenceCalculator.CalcOccurrence(AIndex: Integer);
begin
  InitTimes;
  FFinishDate := cxMaxDate;
  FRecurCount := -1;
  while (AIndex > 0) and GetNextOccurrence do
    Dec(AIndex);
end;

function TcxSchedulerRecurrenceCalculator.GetNextOccurrence: Boolean;
begin
  FDate := Trunc(FOccurrenceStart);
  case FRecurrence of
    cxreWeekly:
      begin
        FWeekStart := FDate - (DayOfWeek(FDate) - 1) + StartOfWeek;
        if FWeekStart > FDate then Dec(FWeekStart, 7);
      end;
    cxreMonthly:
      DecodeDate(FDate, FYear, FMonth, FDay);
    cxreYearly:
      begin
        DecodeDate(FDate, FYear, FMonth, FDay);
        FMonth := FPeriodicity;
      end;
  end;
  repeat
    //DELPHI8! check Trunc()
    if (FIndex = - 1) then
      CalcFirstDate
    else
      CalcNextDate;
    if FDate = InvalidDate then
      Break;
    FOccurrenceFinish := FDate + (FOccurrenceFinish - dxDateOf(FOccurrenceStart));
    FOccurrenceStart := FDate + dxTimeOf(FOccurrenceStart);
    Inc(FIndex);
  until (not CalcForReminders and (dxDateOf(FOccurrenceStart) >= dxDateOf(FVisibleStart))) or
    (CalcForReminders and (dxDateOf(FOccurrenceStart) >= dxDateOf(FActualStart)));
  Result := IsValidOccurrence;
end;

procedure TcxSchedulerRecurrenceCalculator.CalcFirstDate;
begin
  //DELPHI8! check Trunc()
  FDate := Trunc(FActualStart);
  case FRecurrence of
    cxreDaily:
      if FDayType in [cxdtWeekDay, cxdtWeekEndDay] then
        FDate := GetSomeDay(FDate, 1, FDayType = cxdtWeekEndDay);
    cxreWeekly:
      begin
        FWeekStart := FDate - (DayOfWeek(FDate) - 1) + StartOfWeek;
        if FWeekStart > FDate then Dec(FWeekStart, 7);
        FDate := GetDayFromOccurDays(FDate - 1, 1);
      end;
    cxreMonthly:
      begin
        DecodeDate(FDate, FYear, FMonth, FDay);
        FDate := GetDayForMonth;
      end;
    cxreYearly:
      begin
        DecodeDate(FDate, FYear, FMonth, FDay);
        FMonth := FPeriodicity;
        FDate := GetDayForMonth;
        if FDate < dxDateOf(FOccurrenceStart) then
        begin
          Inc(FYear);
          FDate := GetDayForMonth;
        end;
      end;
  end;
end;

procedure TcxSchedulerRecurrenceCalculator.CalcNextDate;
var
  ADay: Word;
begin
  case FRecurrence of
    cxreDaily:
      if FDayType = cxdtEveryDay then
        Inc(FDate, FPeriodicity)
      else
        FDate := GetSomeDay(FDate + 1, 1, FDayType = cxdtWeekEndDay);
    cxreWeekly:
      FDate := GetDayFromOccurDays(FDate, FPeriodicity);
    cxreMonthly:
    begin
      IncAMonth(FYear, FMonth, ADay, FPeriodicity);
      FDate := GetDayForMonth;
    end;
    cxreYearly:
    begin
      Inc(FYear, FYearPeriodicity);
      FDate := GetDayForMonth;
    end;
  end;
end;

function TcxSchedulerRecurrenceCalculator.GetCalcForReminders: Boolean;
begin
  Result := False;
end;

function TcxSchedulerRecurrenceCalculator.GetCertainDay(
  const ADate, ANumber: Integer; ADayType: TcxDayType): Integer;
var
  AYear, AMonth, ADay: Word;
  AOffset: Integer;
begin
  DecodeDate(ADate, AYear, AMonth, ADay);
  AOffset := Ord(ADayType) - Ord(cxdtSunDay) - (DayOfWeek(ADate - ADay + 1) - 1);
  if AOffset < 0 then Inc(AOffset, 7);
  Inc(AOffset, (ANumber - 1) * 7);
  if AOffset > DaysInAMonth(AYear, AMonth) - 1 then Dec(AOffset, 7);
  Result := ADate - ADay + 1 + AOffset;
end;

function TcxSchedulerRecurrenceCalculator.GetDayForMonth: Integer;

  function GetADay(const ADate: Integer; ACondition: Boolean): Integer;
  begin
    if ACondition then
      Result := ADate + DaysInAMonth(FYear, FMonth)
    else
      Result := ADate + FDayNumber;
  end;

begin
  Result := Trunc(EncodeDate(FYear, FMonth, 1)) - 1;
  case FDayType of
    cxdtDay:
      Result := GetADay(Result, FDayNumber > DaysInAMonth(FYear, FMonth));
    cxdtEveryDay:
      Result := GetADay(Result, FDayNumber = 5);
    cxdtWeekDay, cxdtWeekEndDay:
      Result := GetSomeDay(Result + 1, FDayNumber, FDayType = cxdtWeekEndDay);
    cxdtSunday..cxdtSaturday:
      Result := GetCertainDay(Result + 1, FDayNumber, FDayType);
  end;
end;

function TcxSchedulerRecurrenceCalculator.GetDayFromOccurDays(
  const APrevDate, APeriodicity: Integer): Integer;
var
  ADelta: Integer;
  ADay: TDay;
begin
  Result := InvalidDate;
  if FOccurDays = [] then
    Exit;
  repeat
    for ADelta := 0 to 6 do
    begin
      ADay := TDay(StartOfWeek + ADelta - 7 * Byte(StartOfWeek + ADelta > 6));
      if ADay in FOccurDays then
      begin
        Result := FWeekStart + ADelta;
        if Result > APrevDate then Exit;
      end;
    end;
    Inc(FWeekStart, 7 * APeriodicity);
  until False;
end;

function TcxSchedulerRecurrenceCalculator.GetSomeDay(
  const ADate, ANumber: Integer; AWeekEnd: Boolean): Integer;
var
  AYear, AMonth, ADay: Word;
  I: Integer;
begin
  if ANumber = 5 then
  begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    Result := ADate - ADay + DaysInAMonth(AYear, AMonth);
    while not (AWeekEnd xor (TDay(DayOfWeek(Result) - 1) in WorkDays)) do
      Dec(Result);
  end
  else
  begin
    Result := ADate;
    I := 1;
    repeat
      while not (AWeekEnd xor (TDay(DayOfWeek(Result) - 1) in WorkDays)) do
        Inc(Result);
      if ANumber = I then Break;
      Inc(Result);
      Inc(I);
    until False;
  end;
end;

procedure TcxSchedulerRecurrenceCalculator.InitTimes;
begin
  if FRecurCount = 0 then
    FFinishDate := RecurrenceInfo.Finish
  else
  begin
    if CalcForReminders and (FRecurCount > 0) then
      FFinishDate := RecurrenceInfo.GetEndDate
    else
      FFinishDate := VisibleFinish;
  end;
  FOccurrenceStart := GetStart - RecurrenceInfo.DisplayTimeBias + GetTimeBias;
  FOccurrenceFinish := FOccurrenceStart + GetDuration;
end;

function TcxSchedulerRecurrenceCalculator.IsValidOccurrence: Boolean;
begin
  Result := (dxDateOf(FOccurrenceStart) <= FVisibleFinish) and
    (((FRecurCount > 0) and (FIndex < FRecurCount)) or
    ((FRecurCount <= 0) and ((dxDateOf(FOccurrenceStart) <= FFinishDate) or
    (CalcForReminders and (dxDateOf(FOccurrenceStart) <= FFinishDate)))));
end;

{ TcxSchedulerCustomRecurrenceInfo }

constructor TcxSchedulerCustomRecurrenceInfo.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TcxSchedulerCustomRecurrenceInfo.Assign(Source: TPersistent);
var
  S: AnsiString;
begin
  if Source is TcxSchedulerCustomRecurrenceInfo then
  begin
    if TcxSchedulerCustomRecurrenceInfo(Source).GetValue(S) then
      SetValue(S)
    else
      SetValue('');
  end
  else
    inherited Assign(Source);
end;

function TcxSchedulerCustomRecurrenceInfo.GetEndDate: TDateTime;
begin
  if Count > 0 then
    with GetCalculatorClass.Create(Owner, 0, cxMaxDate) do
    try
      CalcOccurrence(Self.Count);
      Result := dxDateOf(OccurrenceStart);
    finally
      Free;
    end
  else
    if Count = 0 then
      Result := Finish
    else
      Result := NullDate;
end;

procedure TcxSchedulerCustomRecurrenceInfo.AssignDefaultValues;
begin
  Count := -1;
  DayNumber := 1;
  DayType := cxdtDay;
  Finish := -1;
  OccurDays := [TDay(DayOfWeek(Start) - 1)];
  Periodicity := 1;
  Recurrence := cxreWeekly;
  YearPeriodicity := 1;
end;

function TcxSchedulerCustomRecurrenceInfo.GetCalculatorClass: TcxSchedulerRecurrenceCalculatorClass;
begin
  Result := TcxSchedulerRecurrenceCalculator;
end;

function TcxSchedulerCustomRecurrenceInfo.GetOwner: TPersistent;
begin
  Result := TPersistent(Owner);
end;

function TcxSchedulerCustomRecurrenceInfo.GetDailyPatternStatus: TcxRecurrenceValidStatus;
begin
  if DayType = cxdtEveryDay then
    Result := PatternValidStatus[Periodicity > 0]
  else
    Result := PatternValidStatus[(DayType = cxdtWeekDay) and (Periodicity = 1)]
end;

function TcxSchedulerCustomRecurrenceInfo.GetMonthlyPatternStatus: TcxRecurrenceValidStatus;
begin
  if DayType = cxdtDay then
  begin
    Result := PatternValidStatus[(Periodicity > 0) and (DayNumber in [1..31])];
    if (Result = rvsValid) and (DayNumber in [29..31]) then
      Result := rvsReplaceOccurrenceDate;
  end
  else
    Result := PatternValidStatus[Periodicity > 0];
end;

function TcxSchedulerCustomRecurrenceInfo.GetWeeklyPatternStatus: TcxRecurrenceValidStatus;
begin
  Result := PatternValidStatus[(Periodicity > 0) and (OccurDays <> [])]
end;

function TcxSchedulerCustomRecurrenceInfo.GetYearlyPatternStatus: TcxRecurrenceValidStatus;
begin
  if DayType = cxdtDay then
    Result := PatternValidStatus[(Periodicity in [1..12]) and
      (DayNumber >=1) and (DayNumber <= DaysPerMonth(2000, Periodicity))]
  else
    Result := PatternValidStatus[(Periodicity in [1..12])];
end;

function TcxSchedulerCustomRecurrenceInfo.GetCount: Integer;
begin
  Result := GetData.Count;
end;

function TcxSchedulerCustomRecurrenceInfo.GetDayNumber: Integer;
begin
  Result := GetData.DayNumber;
end;

function TcxSchedulerCustomRecurrenceInfo.GetDayType: TcxDayType;
begin
  Result := GetData.DayType;
end;

function TcxSchedulerCustomRecurrenceInfo.GetDismissDate: TDateTime;
begin
  Result := GetData.DismissDate;
end;

function TcxSchedulerCustomRecurrenceInfo.GetIsInfinity: Boolean;
begin
  with GetData do
    Result := (Count <= 0) and (Finish = -1);
end;

function TcxSchedulerCustomRecurrenceInfo.GetOccurDays: TDays;
begin
  Result := GetData.OccurDays;
end;

function TcxSchedulerCustomRecurrenceInfo.GetOriginalStart: TDateTime;
begin
  Result := dxDateOf(GetData.Start);
end;

function TcxSchedulerCustomRecurrenceInfo.GetPeriodicity: Integer;
begin
  Result := GetData.Periodicity;
end;

function TcxSchedulerCustomRecurrenceInfo.GetRecurrence: TcxRecurrence;
begin
  Result := GetData.Recurrence;
end;

function TcxSchedulerCustomRecurrenceInfo.GetYearPeriodicity: Integer;
begin
  Result := GetData.YearPeriodicity;
end;

procedure TcxSchedulerCustomRecurrenceInfo.SetCount(
  const AValue: Integer);
begin
  SetDataItem(@DefInfoData.Count, SizeOf(Integer), AValue);
end;

procedure TcxSchedulerCustomRecurrenceInfo.SetDayNumber(
  const AValue: Integer);
begin
  SetDataItem(@DefInfoData.DayNumber, SizeOf(Integer), AValue);
end;

procedure TcxSchedulerCustomRecurrenceInfo.SetDayType(
  const AValue: TcxDayType);
begin
  SetDataItem(@DefInfoData.DayType, SizeOf(TcxDayType), AValue);
end;

procedure TcxSchedulerCustomRecurrenceInfo.SetDismissDate(
  const AValue: TDateTime);
var
  ADate: Integer;
begin
  ADate := Trunc(AValue);
  SetDataItem(@DefInfoData.DismissDate, SizeOf(Integer), ADate);
end;

procedure TcxSchedulerCustomRecurrenceInfo.SetOccurDays(
  const AValue: TDays);
begin
  SetDataItem(@DefInfoData.OccurDays, SizeOf(TDays), AValue);
end;

procedure TcxSchedulerCustomRecurrenceInfo.SetOriginalStart(const AValue: TDateTime);
var
  AVersion: Byte;
begin
  AVersion := cxRecurrenceInfoDataVersion;
  SetDataItem(@DefInfoData.Version, SizeOf(Byte), AVersion);
  SetDataItem(@DefInfoData.Start, SizeOf(TDateTime), AValue);
end;

procedure TcxSchedulerCustomRecurrenceInfo.SetPeriodicity(
  const AValue: Integer);
begin
  SetDataItem(@DefInfoData.Periodicity, SizeOf(Integer), AValue);
end;

procedure TcxSchedulerCustomRecurrenceInfo.SetRecurrence(
  const AValue: TcxRecurrence);
begin
  SetDataItem(@DefInfoData.Recurrence, SizeOf(TcxRecurrence), AValue);
end;

procedure TcxSchedulerCustomRecurrenceInfo.SetYearPeriodicity(
  const AValue: Integer);
begin
  SetDataItem(@DefInfoData.YearPeriodicity, SizeOf(Integer), AValue);
end;

end.
