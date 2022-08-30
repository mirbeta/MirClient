{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit dxDateRanges;

{$I cxVer.inc}

interface

uses
  Classes;

type
  TdxDateRanges = class;

  TdxInitDateRangesEvent = procedure(Sender: TObject; ADateRanges: TdxDateRanges) of object;

  TdxDateTimeFilter = (dtfRelativeDays, dtfRelativeDayPeriods, dtfRelativeWeeks, dtfRelativeMonths, dtfRelativeYears,
    dtfPastFuture, dtfMonths, dtfYears);
  TdxDateTimeFilters = set of TdxDateTimeFilter;
  TdxDateTimeGrouping = (dtgDefault, dtgByDateAndTime, dtgRelativeToToday, dtgByHour, dtgByDate, dtgByMonth, dtgByYear);

  { IdxDateTimeHandling }
  // for internal use
  IdxDateTimeHandling = interface
  ['{FBFE2378-48D4-4D6C-B93A-9A430D2C6FD6}']
    function GetDateFormat: string;
    function GetHourFormat: string;
    function GetMonthFormat: string;
    function GetYearFormat: string;
  end;

  { IdxFilteringDateTimeHandling }
  // for internal use
  IdxFilteringDateTimeHandling = interface
  ['{0A6FF65F-FE22-4D0B-A576-1B8A7850C18F}']
    function GetFilters: TdxDateTimeFilters;
  end;

  { IdxGroupingDateTimeHandling }
  // for internal use
  IdxGroupingDateTimeHandling = interface
  ['{8BFD824B-3C6C-4A20-9726-22BF3A0D0F2B}']
    function GetGrouping: TdxDateTimeGrouping;
  end;

  { TdxCustomDateRange }

  TdxCustomDateRange = class
  private
    FContainer: TdxDateRanges;
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
  public
    destructor Destroy; override;

    function Contains(const ADate: TDateTime): Boolean; virtual; abstract;
    function GetDisplayText(const ADate: TDateTime): string; virtual; abstract;
    function GetRangeValue(const ADate: TDateTime; AIgnoreTime: Boolean): Variant; virtual;
    function GetSortingValue(const ADate: TDateTime): Variant; virtual;
    function GetValue(const ADate: TDateTime): Variant; virtual; abstract;
    function NeedsSortingByTime: Boolean; virtual;
    function NeedsTime: Boolean; virtual;

    property Container: TdxDateRanges read FContainer;
    property Index: Integer read GetIndex write SetIndex;
  end;
  TdxCustomDateRangeClass = class of TdxCustomDateRange;

  { TdxHourRange }

  TdxHourRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
    function NeedsSortingByTime: Boolean; override;
  end;

  { TdxDayRange }

  TdxDayRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxMonthRange }

  TdxMonthRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetRangeValue(const ADate: TDateTime; AIgnoreTime: Boolean): Variant; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxYearRange }

  TdxYearRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetRangeValue(const ADate: TDateTime; AIgnoreTime: Boolean): Variant; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxYesterdayRange }

  TdxYesterdayRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxTodayRange }

  TdxTodayRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxTomorrowRange }

  TdxTomorrowRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxLastWeekRange }

  TdxLastWeekRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxThisWeekRange }

  TdxThisWeekRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxNextWeekRange }

  TdxNextWeekRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxLastMonthRange }

  TdxLastMonthRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxThisMonthRange }

  TdxThisMonthRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxNextMonthRange }

  TdxNextMonthRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxLastYearRange }

  TdxLastYearRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxThisYearRange }

  TdxThisYearRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxNextYearRange }

  TdxNextYearRange = class(TdxCustomDateRange)
  public
    function Contains(const ADate: TDateTime): Boolean; override;
    function GetDisplayText(const ADate: TDateTime): string; override;
    function GetValue(const ADate: TDateTime): Variant; override;
  end;

  { TdxDateRanges }

  TdxDateRanges = class
  private
    FDateTimeHandling: TObject;
    FItems: TList;
    FStartOfThisWeek: TDateTime;
    FThisDay: Word;
    FThisMonth: Word;
    FThisMonthNumber: Integer;
    FThisYear: Word;
    FToday: TDateTime;
    function GetCount: Integer;
    function GetIDateTimeHandling: IdxDateTimeHandling;
    function GetItem(Index: Integer): TdxCustomDateRange;
  protected
    procedure AddItem(AItem: TdxCustomDateRange);
    procedure RemoveItem(AItem: TdxCustomDateRange);
    function GetItemIndex(AItem: TdxCustomDateRange): Integer;
    procedure SetItemIndex(AItem: TdxCustomDateRange; AValue: Integer);

    procedure InitConsts; virtual;

    property IDateTimeHandling: IdxDateTimeHandling read GetIDateTimeHandling;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(ARange: TdxCustomDateRange); overload;
    procedure Add(ARangeClass: TdxCustomDateRangeClass); overload;
    procedure Clear;
    function GetRange(const ADate: TDateTime): TdxCustomDateRange; overload;
    function GetRange(ARangeClass: TdxCustomDateRangeClass): TdxCustomDateRange; overload;
    procedure Init(ADateTimeHandling: TObject); virtual;
    function IsEmpty: Boolean;
    function NeedSortingByTime: Boolean;
    function NeedTime: Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxCustomDateRange read GetItem; default;

    property DateTimeHandling: TObject read FDateTimeHandling;
    property StartOfThisWeek: TDateTime read FStartOfThisWeek;
    property ThisDay: Word read FThisDay;
    property ThisMonth: Word read FThisMonth;
    property ThisMonthNumber: Integer read FThisMonthNumber;
    property ThisYear: Word read FThisYear;
    property Today: TDateTime read FToday;
  end;
  TdxDateRangesClass = class of TdxDateRanges;

  { TdxFilteringDateRanges }

  TdxFilteringDateRanges = class(TdxDateRanges)
  private
    function GetIFilteringDateTimeHandling: IdxFilteringDateTimeHandling;
  protected
    function GetMonthRange: TdxCustomDateRangeClass; virtual;
    function GetYearRange: TdxCustomDateRangeClass; virtual;

    property IFilteringDateTimeHandling: IdxFilteringDateTimeHandling read GetIFilteringDateTimeHandling;
  public
    procedure Init(ADateTimeHandling: TObject); override;
  end;
  TdxFilteringDateRangesClass = class of TdxFilteringDateRanges;

  { TdxGroupingDateRanges }

  TdxGroupingDateRanges = class(TdxDateRanges)
  private
    function GetIGroupingDateTimeHandling: IdxGroupingDateTimeHandling;
  protected
    function GetDayRangeClass: TdxCustomDateRangeClass; virtual;
    function GetHourRangeClass: TdxCustomDateRangeClass; virtual;
    function GetLastMonthRangeClass: TdxCustomDateRangeClass; virtual;
    function GetLastWeekRangeClass: TdxCustomDateRangeClass; virtual;
    function GetLastYearRangeClass: TdxCustomDateRangeClass; virtual;
    function GetMonthRangeClass: TdxCustomDateRangeClass; virtual;
    function GetNextMonthRangeClass: TdxCustomDateRangeClass; virtual;
    function GetNextWeekRangeClass: TdxCustomDateRangeClass; virtual;
    function GetNextYearRangeClass: TdxCustomDateRangeClass; virtual;
    function GetThisMonthRangeClass: TdxCustomDateRangeClass; virtual;
    function GetThisWeekRangeClass: TdxCustomDateRangeClass; virtual;
    function GetThisYearRangeClass: TdxCustomDateRangeClass; virtual;
    function GetTodayRangeClass: TdxCustomDateRangeClass; virtual;
    function GetTomorrowRangeClass: TdxCustomDateRangeClass; virtual;
    function GetYearRangeClass: TdxCustomDateRangeClass; virtual;
    function GetYesterdayRangeClass: TdxCustomDateRangeClass; virtual;

    property IGroupingDateTimeHandling: IdxGroupingDateTimeHandling read GetIGroupingDateTimeHandling;
  public
    procedure Init(ADateTimeHandling: TObject); override;
  end;

  function dxContainDateTimeAbsoluteFilters(AFilters: TdxDateTimeFilters): Boolean;
  function dxContainDateTimeRelativeFilters(AFilters: TdxDateTimeFilters): Boolean;

implementation

uses
  SysUtils, DateUtils, Types, cxVariants, cxDateUtils;

const
  dxYesterday = 'Yesterday';
  dxToday = 'Today';
  dxTomorrow = 'Tomorrow';
  dxLastWeek = 'Last week';
  dxThisWeek = 'This week';
  dxNextWeek = 'Next week';
  dxLastMonth = 'Last month';
  dxThisMonth = 'This month';
  dxNextMonth = 'Next month';
  dxLastYear = 'Last year';
  dxThisYear = 'This year';
  dxNextYear = 'Next year';

  dxDateTimeRelativeFilters = [dtfRelativeDays..dtfPastFuture];
  dxDateTimeAbsoluteFilters = [dtfMonths, dtfYears];

function dxContainDateTimeAbsoluteFilters(AFilters: TdxDateTimeFilters): Boolean;
begin
  Result := AFilters * dxDateTimeAbsoluteFilters <> [];
end;

function dxContainDateTimeRelativeFilters(AFilters: TdxDateTimeFilters): Boolean;
begin
  Result := AFilters * dxDateTimeRelativeFilters <> [];
end;

{ TdxCustomDateRange }

destructor TdxCustomDateRange.Destroy;
begin
  if FContainer <> nil then
    FContainer.RemoveItem(Self);
  inherited;
end;

function TdxCustomDateRange.GetIndex: Integer;
begin
  if FContainer = nil then
    Result := -1
  else
    Result := FContainer.GetItemIndex(Self);
end;

procedure TdxCustomDateRange.SetIndex(Value: Integer);
begin
  if FContainer <> nil then
    FContainer.SetItemIndex(Self, Value);
end;

function TdxCustomDateRange.GetRangeValue(const ADate: TDateTime; AIgnoreTime: Boolean): Variant;
begin
  Result := GetValue(ADate);
end;

function TdxCustomDateRange.GetSortingValue(const ADate: TDateTime): Variant;
begin
  if NeedsSortingByTime then
    Result := dxTimeOf(ADate)
  else
    Result := GetValue(ADate);
end;

function TdxCustomDateRange.NeedsSortingByTime: Boolean;
begin
  Result := False;
end;

function TdxCustomDateRange.NeedsTime: Boolean;
begin
  Result := NeedsSortingByTime;
end;

{ TdxHourRange }

function TdxHourRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := True;
end;

function TdxHourRange.GetDisplayText(const ADate: TDateTime): string;
begin
  if Container.IDateTimeHandling.GetHourFormat = '' then
    Result := ''
  else
    Result := FormatDateTime(Container.IDateTimeHandling.GetHourFormat, GetValue(ADate));
end;

function TdxHourRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := EncodeTime(HourOf(ADate), 0, 0, 0);
end;

function TdxHourRange.NeedsSortingByTime: Boolean;
begin
  Result := True;
end;

{ TdxDayRange }

function TdxDayRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := True;
end;

function TdxDayRange.GetDisplayText(const ADate: TDateTime): string;
begin
  if Container.IDateTimeHandling.GetDateFormat = '' then
    Result := ''
  else
    Result := FormatDateTime(Container.IDateTimeHandling.GetDateFormat, ADate);
end;

function TdxDayRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := ADate;
end;

{ TdxMonthRange }

function TdxMonthRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := True;
end;

function TdxMonthRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := FormatDateTime(Container.IDateTimeHandling.GetMonthFormat, ADate);
end;

function TdxMonthRange.GetRangeValue(const ADate: TDateTime; AIgnoreTime: Boolean): Variant;
begin
  Result := VarBetweenArrayCreate(dxGetStartDateOfMonth(ADate), dxGetEndDateOfMonth(ADate, AIgnoreTime));
end;

function TdxMonthRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := dxGetStartDateOfMonth(ADate);
end;

{ TdxYearRange }

function TdxYearRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := True;
end;

function TdxYearRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := FormatDateTime(Container.IDateTimeHandling.GetYearFormat, ADate);
end;

function TdxYearRange.GetRangeValue(const ADate: TDateTime; AIgnoreTime: Boolean): Variant;
begin
  Result := VarBetweenArrayCreate(dxGetStartDateOfYear(ADate), dxGetEndDateOfYear(ADate, AIgnoreTime));
end;

function TdxYearRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := dxGetStartDateOfYear(ADate);
end;

{ TdxYesterdayRange }

function TdxYesterdayRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := ADate = Container.Today - 1;
end;

function TdxYesterdayRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := dxYesterday;
end;

function TdxYesterdayRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := ADate;
end;

{ TdxTodayRange }

function TdxTodayRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := ADate = Container.Today;
end;

function TdxTodayRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := dxToday;
end;

function TdxTodayRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := ADate;
end;

{ TdxTomorrowRange }

function TdxTomorrowRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := ADate = Container.Today + 1;
end;

function TdxTomorrowRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := dxTomorrow;
end;

function TdxTomorrowRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := ADate;
end;

{ TdxLastWeekRange }

function TdxLastWeekRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := dxGetStartDateOfWeek(ADate) = Container.StartOfThisWeek - 7;
end;

function TdxLastWeekRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := dxLastWeek;
end;

function TdxLastWeekRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := dxGetStartDateOfWeek(ADate) + 0.1;
end;

{ TdxThisWeekRange }

function TdxThisWeekRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := dxGetStartDateOfWeek(ADate) = Container.StartOfThisWeek;
end;

function TdxThisWeekRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := dxThisWeek;
end;

function TdxThisWeekRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := dxGetStartDateOfWeek(ADate) + 0.1;
end;

{ TdxNextWeekRange }

function TdxNextWeekRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := dxGetStartDateOfWeek(ADate) = Container.StartOfThisWeek + 7;
end;

function TdxNextWeekRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := dxNextWeek;
end;

function TdxNextWeekRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := dxGetStartDateOfWeek(ADate) + 0.1;
end;

{ TdxLastMonthRange }

function TdxLastMonthRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := dxGetMonthNumber(ADate) = Container.ThisMonthNumber - 1;
end;

function TdxLastMonthRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := dxLastMonth;
end;

function TdxLastMonthRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := dxGetStartDateOfMonth(ADate) + 0.2;
end;

{ TdxThisMonthRange }

function TdxThisMonthRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := dxGetMonthNumber(ADate) = Container.ThisMonthNumber;
end;

function TdxThisMonthRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := dxThisMonth;
end;

function TdxThisMonthRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := dxGetStartDateOfMonth(ADate) + 0.2;
end;

{ TdxNextMonthRange }

function TdxNextMonthRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := dxGetMonthNumber(ADate) = Container.ThisMonthNumber + 1;
end;

function TdxNextMonthRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := dxNextMonth;
end;

function TdxNextMonthRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := dxGetStartDateOfMonth(ADate) + 0.2;
end;

{ TdxLastYearRange }

function TdxLastYearRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := dxGetDateElement(ADate, deYear) = Container.ThisYear - 1;
end;

function TdxLastYearRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := dxLastYear;
end;

function TdxLastYearRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := dxGetStartDateOfYear(ADate) + 0.3;
end;

{ TdxThisYearRange }

function TdxThisYearRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := dxGetDateElement(ADate, deYear) = Container.ThisYear;
end;

function TdxThisYearRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := dxThisYear;
end;

function TdxThisYearRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := dxGetStartDateOfYear(ADate) + 0.3;
end;

{ TdxNextYearRange }

function TdxNextYearRange.Contains(const ADate: TDateTime): Boolean;
begin
  Result := dxGetDateElement(ADate, deYear) = Container.ThisYear + 1;
end;

function TdxNextYearRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := dxNextYear;
end;

function TdxNextYearRange.GetValue(const ADate: TDateTime): Variant;
begin
  Result := dxGetStartDateOfYear(ADate) + 0.3;
end;

{ TdxDateRanges }

constructor TdxDateRanges.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TdxDateRanges.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TdxDateRanges.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxDateRanges.GetIDateTimeHandling: IdxDateTimeHandling;
begin
  Supports(DateTimeHandling, IdxDateTimeHandling, Result);
end;

function TdxDateRanges.GetItem(Index: Integer): TdxCustomDateRange;
begin
  Result := TdxCustomDateRange(FItems[Index]);
end;

procedure TdxDateRanges.AddItem(AItem: TdxCustomDateRange);
begin
  AItem.FContainer := Self;
  FItems.Add(AItem);
end;

procedure TdxDateRanges.RemoveItem(AItem: TdxCustomDateRange);
begin
  FItems.Remove(AItem);
  AItem.FContainer := nil;
end;

function TdxDateRanges.GetItemIndex(AItem: TdxCustomDateRange): Integer;
begin
  Result := FItems.IndexOf(AItem);
end;

procedure TdxDateRanges.SetItemIndex(AItem: TdxCustomDateRange; AValue: Integer);
begin
  FItems.Move(GetItemIndex(AItem), AValue);
end;

procedure TdxDateRanges.InitConsts;
begin
  FToday := Date;
  FStartOfThisWeek := dxGetStartDateOfWeek(Today);
  DecodeDate(Today, FThisYear, FThisMonth, FThisDay);
  FThisMonthNumber := dxGetMonthNumber(Today);
end;

procedure TdxDateRanges.Add(ARange: TdxCustomDateRange);
begin
  AddItem(ARange);
end;

procedure TdxDateRanges.Add(ARangeClass: TdxCustomDateRangeClass);
begin
  Add(ARangeClass.Create);
end;

procedure TdxDateRanges.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I].Free;
end;

function TdxDateRanges.GetRange(const ADate: TDateTime): TdxCustomDateRange;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Contains(ADate) then Exit;
  end;
  Result := nil;
end;

function TdxDateRanges.GetRange(ARangeClass: TdxCustomDateRangeClass): TdxCustomDateRange;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result is ARangeClass then Exit;
  end;
  Result := nil;
end;

procedure TdxDateRanges.Init(ADateTimeHandling: TObject);
begin
  FDateTimeHandling := ADateTimeHandling;
  InitConsts;
  Clear;
end;

function TdxDateRanges.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TdxDateRanges.NeedSortingByTime: Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].NeedsSortingByTime;
    if Result then Exit;
  end;
  Result := False;
end;

function TdxDateRanges.NeedTime: Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].NeedsTime;
    if Result then Exit;
  end;
  Result := False;
end;

{ TdxFilteringDateRanges }

procedure TdxFilteringDateRanges.Init(ADateTimeHandling: TObject);
begin
  inherited Init(ADateTimeHandling);
  if dtfYears in IFilteringDateTimeHandling.GetFilters then
    Add(GetYearRange);
  if dtfMonths in IFilteringDateTimeHandling.GetFilters then
    Add(GetMonthRange);
end;

function TdxFilteringDateRanges.GetMonthRange: TdxCustomDateRangeClass;
begin
  Result := TdxMonthRange;
end;

function TdxFilteringDateRanges.GetYearRange: TdxCustomDateRangeClass;
begin
  Result := TdxYearRange;
end;

function TdxFilteringDateRanges.GetIFilteringDateTimeHandling: IdxFilteringDateTimeHandling;
begin
  Supports(DateTimeHandling, IdxFilteringDateTimeHandling, Result);
end;

{ TdxGroupingDateRanges }

procedure TdxGroupingDateRanges.Init(ADateTimeHandling: TObject);
begin
  inherited Init(ADateTimeHandling);
  case IGroupingDateTimeHandling.GetGrouping of
    dtgRelativeToToday:
      begin
        Add(GetYesterdayRangeClass);
        Add(GetTodayRangeClass);
        Add(GetTomorrowRangeClass);

        Add(GetLastWeekRangeClass);
        Add(GetThisWeekRangeClass);
        Add(GetNextWeekRangeClass);

        Add(GetLastMonthRangeClass);
        Add(GetThisMonthRangeClass);
        Add(GetNextMonthRangeClass);

        Add(GetLastYearRangeClass);
        Add(GetThisYearRangeClass);
        Add(GetNextYearRangeClass);
      end;
    dtgByHour:
      Add(GetHourRangeClass);
    dtgByDate:
      Add(GetDayRangeClass);
    dtgByMonth:
      Add(GetMonthRangeClass);
    dtgByYear:
      Add(GetYearRangeClass);
  end;
end;

function TdxGroupingDateRanges.GetDayRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxDayRange;
end;

function TdxGroupingDateRanges.GetHourRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxHourRange;
end;

function TdxGroupingDateRanges.GetLastMonthRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxLastMonthRange;
end;

function TdxGroupingDateRanges.GetLastWeekRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxLastWeekRange;
end;

function TdxGroupingDateRanges.GetLastYearRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxLastYearRange;
end;

function TdxGroupingDateRanges.GetMonthRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxMonthRange;
end;

function TdxGroupingDateRanges.GetNextMonthRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxNextMonthRange;
end;

function TdxGroupingDateRanges.GetNextWeekRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxNextWeekRange;
end;

function TdxGroupingDateRanges.GetNextYearRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxNextYearRange;
end;

function TdxGroupingDateRanges.GetThisMonthRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxThisMonthRange;
end;

function TdxGroupingDateRanges.GetThisWeekRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxThisWeekRange;
end;

function TdxGroupingDateRanges.GetThisYearRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxThisYearRange;
end;

function TdxGroupingDateRanges.GetTodayRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxTodayRange;
end;

function TdxGroupingDateRanges.GetTomorrowRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxTomorrowRange;
end;

function TdxGroupingDateRanges.GetYearRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxYearRange;
end;

function TdxGroupingDateRanges.GetYesterdayRangeClass: TdxCustomDateRangeClass;
begin
  Result := TdxYesterdayRange;
end;

function TdxGroupingDateRanges.GetIGroupingDateTimeHandling: IdxGroupingDateTimeHandling;
begin
  Supports(DateTimeHandling, IdxGroupingDateTimeHandling, Result);
end;

end.
