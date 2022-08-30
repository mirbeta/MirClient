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

unit cxDateUtils;

{$I cxVer.inc}

interface

uses
  Variants, SysUtils, Windows, Controls, Classes, Graphics, Registry,
  dxCore, dxCoreClasses;

type
  TcxDateElement = (deYear, deMonth, deDay);
  TcxFirstWeekOfYear = (fwySystem, fwyJan1, fwyFirstFullWeek, fwyFirstFourDays);

  TDay = (dSunday, dMonday, dTuesday, dWednesday, dThursday, dFriday, dSaturday);
  TDays = set of TDay;
  TdxDayOfWeek = 0..6;       //0 = FirstDayOfWeek .. 6 = LastDayOfWeek
  TdxDayOfWeekSystem = 0..6; //0 = Monday..6 = Sunday
  TdxDayOfWeekVCL = 1..7;    //1 = Sunday..7 = Satuday

const
  DATE_YEARMONTH  = $00000008;  // use year month picture
  {$EXTERNALSYM DATE_YEARMONTH}
  DATE_LTRREADING = $00000010;  // add marks for left to right reading order layout
  {$EXTERNALSYM DATE_LTRREADING}
  DATE_RTLREADING = $00000020;  // add marks for right to left reading order layout
  {$EXTERNALSYM DATE_RTLREADING}

  NullDate = -700000;
  InvalidDate = NullDate + 1;
  SmartTextToDateFunc: function(const AText: string; var ADate: TDateTime): Boolean = nil;

type
  TcxCALID = DWORD;    { Calendar ID. }
  {$EXTERNALSYM TcxCALID}

  TcxCalendarAlgorithmType = (catUnknown, catSolarCalendar, catLunarCalendar,
    catLunarSolarCalendar);

  TcxDateEditSmartInput = (deiToday, deiYesterday, deiTomorrow,
    deiSunday, deiMonday, deiTuesday, deiWednesday, deiThursday, deiFriday, deiSaturday,
    deiFirst, deiSecond, deiThird, deiFourth, deiFifth, deiSixth, deiSeventh,
    deiBOM, deiEOM, deiNow);

  TcxDateTime = record
    Era: Integer;
    Year: Cardinal;
    Month: Cardinal;
    Day: Cardinal;
    Hours: Byte;
    Minutes: Byte;
    Seconds: Byte;
    Milliseconds: Word;
  end;

  TcxDate = record
    Era: Integer;
    Year: Cardinal;
    Month: Cardinal;
    Day: Cardinal;
  end;

  TcxTime = record
    Hours: Cardinal;
    Minutes: Cardinal;
    Seconds: Cardinal;
    Miliseconds: Cardinal;
  end;

  TcxEra = class(TPersistent)
  private
    FEra: Integer;
    FMaxEraYear: Integer;
    FMinEraYear: Integer;
    FStartDate: TDateTime;
    FYearOffset: Integer;
  public
    constructor Create(AEra: Integer; AStartDate: TDateTime;
      AYearOffset, AMinEraYear, AMaxEraYear: Integer);
    procedure Assign(Source: TPersistent); override;

    property Era: Integer read FEra write FEra;
    property MaxEraYear: Integer read FMaxEraYear write FMaxEraYear;
    property MinEraYear: Integer read FMinEraYear write FMinEraYear;
    property StartDate: TDateTime read FStartDate write FStartDate;
    property YearOffset: Integer read FYearOffset write FYearOffset;
  end;

  TcxEras = class(TdxFastObjectList)
  private
    function GetItem(AIndex: Integer): TcxEra;
    procedure SetItem(AIndex: Integer; AValue: TcxEra);
  public
    property Items[Index: Integer]: TcxEra read GetItem write SetItem; default;
  end;

  { TcxCustomCalendarTable }

  TcxCustomCalendarTable = class
  protected
    FEras: TcxEras;
    procedure AdjustYear(var AYear, AEra: Integer); overload; virtual;
    procedure AdjustYear(var AYear, AEra: Integer; AMonth, ADay: Integer); overload; virtual;
    function GetCalendarAlgorithmType: TcxCalendarAlgorithmType; virtual; abstract;
    function GetCalendarID: TcxCALID; virtual; abstract;
    function GetDefaultEra: TcxEra; virtual; abstract;
    function GetMaxSupportedDate: TDateTime; virtual; abstract;
    function GetMinSupportedDate: TDateTime; virtual; abstract;
    procedure CheckDateTime(var ADateTime: TDateTime); virtual;
    function GetMaxSupportedYear: Integer; virtual; abstract;
    function GetMinSupportedYear: Integer; virtual; abstract;
    function IsNotValid(ADate: TcxDateTime; out AResult: TDateTime): Boolean;
    procedure YearToGregorianYear(var AYear: Cardinal; AEra: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function AddDays(ADate: TcxDateTime; ACountDays: Integer): TDateTime; overload; virtual;

    function AddMonths(ADate: TDateTime; ACountMonths: Integer): TDateTime; overload; virtual;
    function AddMonths(ADate: TcxDateTime; ACountMonths: Integer): TDateTime; overload; virtual;

    function AddYears(ADate: TDateTime; ACountYears: Integer): TDateTime; overload; virtual;
    function AddYears(ADate: TcxDateTime; ACountYears: Integer): TDateTime; overload; virtual;

    function AddWeeks(ADate: TDateTime; ACountWeeks: Integer): TDateTime; overload; virtual;
    function AddWeeks(ADate: TcxDateTime; ACountWeeks: Integer): TDateTime; overload; virtual;

    function FromDateTime(ADate: TDateTime): TcxDateTime; overload; virtual; abstract;
    function FromDateTime(AYear, AMonth, ADay: Cardinal): TcxDateTime; overload; virtual;
    function FromDateTime(AYear, AMonth, ADay: Cardinal; AHours, AMinutes, ASeconds: Byte;
      AMilliseconds: Word): TcxDateTime; overload; virtual;
    function GetDayOfYear(ADate: TDateTime): Cardinal; overload; virtual;
    function GetDayOfYear(ADate: TcxDateTime): Cardinal; overload; virtual;
    function GetDaysInMonth(AYear, AMonth: Cardinal): Cardinal; overload; virtual;
    function GetDaysInMonth(AEra: Integer; AYear, AMonth: Cardinal): Cardinal; overload; virtual; abstract;
    function GetDaysInYear(AYear: Cardinal): Cardinal; overload; virtual;
    function GetDaysInYear(AEra: Integer; AYear: Cardinal): Cardinal; overload; virtual; abstract;
    function GetEra(AYear: Integer): Integer; overload; virtual;
    function GetEra(AYear, AMonth, ADay: Integer): Integer; overload; virtual;
    function GetFirstDayOfWeek(ADate: TDateTime): TDateTime; overload; virtual;
    function GetFirstDayOfWeek(ADate: TDateTime; AStartDayOfWeek: TDay): TDateTime; overload; virtual;
    function GetFirstDayOfWeek(ADate: TcxDateTime): TcxDateTime; overload; virtual;
    function GetFirstDayOfWeek(ADate: TcxDateTime; AStartDayOfWeek: TDay): TcxDateTime; overload; virtual;
    function GetFirstDayOfMonth(const ADate: TDateTime): TDateTime; overload; virtual;
    function GetFirstDayOfMonth(const ADate: TcxDateTime): TcxDateTime; overload; virtual;
    function GetFirstDayOfYear(const ADate: TDateTime): TDateTime; overload; virtual;
    function GetFirstDayOfYear(const ADate: TcxDateTime): TcxDateTime; overload; virtual;
    function GetFirstWeekDay: Byte; virtual; abstract;
    function GetFullWeeksInYear(AYear: Cardinal): Cardinal; virtual; abstract;
    function GetLastDayOfMonth(const ADate: TDateTime): TDateTime; overload; virtual;
    function GetLastDayOfMonth(const ADate: TcxDateTime): TcxDateTime; overload; virtual;
    function GetLastDayOfYear(const ADate: TDateTime): TDateTime; overload; virtual;
    function GetLastDayOfYear(const ADate: TcxDateTime): TcxDateTime; overload; virtual;
    function GetMonthsInYear(AYear: Cardinal): Cardinal; overload; virtual;
    function GetMonthsInYear(AEra: Integer; AYear: Cardinal): Cardinal; overload; virtual; abstract;
    function GetYear(ADate: TDateTime): Cardinal; overload; virtual;
    function GetYear(ADate: TcxDate): Cardinal; overload; virtual;
    function GetYear(ADate: TcxDateTime): Cardinal; overload; virtual;
    function GetWeekDay(ADate: TDateTime): Byte; overload; virtual;
    function GetWeekDay(ADate: TcxDateTime): Byte; overload; virtual;
    function GetWeekNumber(ADate: TDateTime; AStartOfWeek: TDay;
      AFirstWeekOfYear: TcxFirstWeekOfYear): Cardinal; overload; virtual;
    function GetWeekNumber(ADate: TcxDateTime; AStartOfWeek: TDay;
      AFirstWeekOfYear: TcxFirstWeekOfYear): Cardinal; overload; virtual; abstract;
    function IsLeapDay(AYear, AMonth, ADay: Cardinal): Boolean; overload; virtual;
    function IsLeapDay(AEra: Integer; AYear, AMonth, ADay: Cardinal): Boolean; overload; virtual; abstract;
    function IsLeapMonth(AYear, AMonth: Cardinal): Boolean; overload; virtual;
    function IsLeapMonth(AEra: Integer; AYear, AMonth: Cardinal): Boolean; overload; virtual; abstract;
    function IsLeapYear(AYear: Cardinal): Boolean; overload; virtual;
    function IsLeapYear(AEra: Integer; Year: Cardinal): Boolean; overload; virtual; abstract;

    function IsValidYear(AYear: Cardinal): Boolean; overload; virtual;
    function IsValidYear(AEra: Integer; AYear: Cardinal): Boolean; overload; virtual;
    function IsValidMonth(AYear, AMonth: Cardinal): Boolean; overload; virtual;
    function IsValidMonth(AEra: Integer; AYear, AMonth: Cardinal): Boolean; overload; virtual;
    function IsValidDay(AYear, AMonth, ADay: Cardinal): Boolean; overload; virtual;
    function IsValidDay(AEra: Integer; AYear, AMonth, ADay: Cardinal): Boolean; overload; virtual;
    function IsValidDate(ADate: TDateTime): Boolean; virtual;

    function ToDateTime(ADate: TcxDate): TDateTime; overload; virtual;
    function ToDateTime(AYear, AMonth, ADay: Cardinal): TDateTime; overload; virtual;
    function ToDateTime(AYear, AMonth, ADay: Cardinal; AHours, AMinutes, ASeconds: Byte;
      AMilliseconds: Word): TDateTime; overload; virtual;
    function ToDateTime(ADateTime: TcxDateTime): TDateTime; overload; virtual; abstract;

    function GetDayNumber(const S: string): Integer; virtual;
    function GetMonthNumber(AYear: Integer; const S: string): Integer; virtual;
    function GetYearNumber(const S: string): Integer; virtual;

    property AlgorithmType: TcxCalendarAlgorithmType read GetCalendarAlgorithmType;
    property CalendarID: TcxCALID read GetCalendarID;
    property DefaultEra: TcxEra read GetDefaultEra;
    property Eras: TcxEras read FEras;
    property MaxSupportedDate: TDateTime read GetMaxSupportedDate;
    property MinSupportedDate: TDateTime read GetMinSupportedDate;
  end;

  TcxCustomCalendarTableClass = class of TcxCustomCalendarTable;

  { TdxTZIField }

  TcxTZIField = packed record
    Bias: LongInt;
    StandardBias: LongInt;
    DaylightBias: LongInt;
    StandardDate: TSystemTime;
    DaylightDate: TSystemTime;
  end;

  TcxDSTInfo = packed record
    Year: Word;
    DaylightDate: TDateTime;
    StandardDate: TDateTime;
  end;

  { TcxTimeZoneInformation }

  TcxTimeZoneInformation = record
    Display: Widestring;
    StandardName: Widestring;
    DaylightName: Widestring;
    Index: Integer;
    MapId: string;
    TZI: TcxTZIField;
    DynamicDST: array of TcxDSTInfo; // need for DST optimization
  end;

  TdxTimeZoneHelper = record
  strict private
    class var
      FCurrentTimeZone: Integer;
      FTimeZoneInformations: array of TcxTimeZoneInformation;
    // time zone info
    class function CheckTimeIndex(const AIndex: Integer): Integer; static;
    class function cxTZInfoToTZInfo(const AInfo: TcxTimeZoneInformation): TTimeZoneInformation; static;
    class function TZInfoTocxTZInfo(const AInfo: TTimeZoneInformation): TcxTimeZoneInformation; static;
    class procedure ReadTimeZoneInfoFromRegistry(
      ARegistry: TRegistry; out AInfo: TcxTimeZoneInformation); static;
  private
    class procedure InitTimeZoneInformation; static;
    class procedure DoneTimeZoneInformation; static;
  public
      //internal time zone
    class function AddInternalTimeZone(const ATimeZone: TcxTimeZoneInformation): Integer; static;
    // time zone conversion
    class function AddDSTInfo(var ATimeZoneInfo: TcxTimeZoneInformation;
      const AYear: Integer; const AStandardDate, ADaylightDate: TSystemTime): Integer; overload; static;
    class function AddDSTInfo(var ATimeZoneInfo: TcxTimeZoneInformation;
      const AYear: Integer; const AStandardDate, ADaylightDate: TDateTime): Integer; overload; static;
    class function AddDSTInfo(const ATimeZone: Integer;
      const AYear: Integer; const AStandardDate, ADaylightDate: TDateTime): Integer; overload; static;
    class function CalculateTransitionDate(const AYear: Word;
      const ADSTDateTime: TSystemTime): TDateTime; static;
    class function ConvertToGlobalTime(
      const ADateTime: TDateTime; ATimeZone: Integer = -1): TDateTime; static;
    class function ConvertToLocalTime(
      const ADateTime: TDateTime; ATimeZone: Integer = -1): TDateTime; static;
    class function CurrentTimeZone: Integer; static;
    class function CurrentTimeZoneBias: Double; static;
    class function IsDaylightDateTime(ATimeZone: Integer; ADateTime: TDateTime): Boolean; static;
    class function TimeZoneBias(AIndex: Integer): TDateTime; static;
    class function TimeZoneBiasDelta(AIndex: Integer): TDateTime; static;
    class function TimeZoneCount: Integer; static;
    class function TimeZoneDaylightBias(ADateTime: TDateTime; ATimeZone: Integer): Integer; static;
    class function TimeZoneInfo(AIndex: Integer): TcxTimeZoneInformation; static;
  end;

procedure dxDecMonth(var AYear, AMonth: Word);
procedure dxIncMonth(var AYear, AMonth: Word); overload;
procedure dxChangeMonth(var AYear, AMonth: Word; Delta: Integer);

function IsLeapYear(AYear: Integer): Boolean;
function DaysPerMonth(AYear, AMonth: Integer): Integer;
function CheckDay(AYear, AMonth, ADay: Integer): Integer;

function dxTimeOf(const AValue: TDateTime): TDateTime;
function dxDateOf(const AValue: TDateTime): TDateTime;

function cxIsDateValid(ADate: Double): Boolean;

function dxGetDateElement(ADate: TDateTime; AElement: TcxDateElement): Integer;
function dxGetStartDateOfMonth(const ADate: TDateTime): TDateTime;
function dxGetEndDateOfMonth(const ADate: TDateTime; AIgnoreTime: Boolean): TDateTime;
function dxGetStartDateOfYear(const ADate: TDateTime): TDateTime;
function dxGetEndDateOfYear(const ADate: TDateTime; AIgnoreTime: Boolean): TDateTime;
function dxGetStartOfWeek: TDay;
function dxGetAssignedStartOfWeek: Boolean;
procedure dxResetAssignedStartOfWeek;
procedure dxSetStartOfWeek(AValue: TDay);

function dxGetLocalFirstWeekOfYear: TcxFirstWeekOfYear;
function dxGetLocalStartOfWeek: TDay;
function dxGetWeekNumber(ADate: TDateTime; AStartOfWeek: TDay; AFirstWeekOfYear: TcxFirstWeekOfYear): Integer;
function dxGetMonthNumber(const ADate: TDateTime): Integer;

function dxDayOfWeek(const AValue: TDateTime): TDay;
function dxDayOfWeekOffset(const AValue: TDateTime): TdxDayOfWeek; overload;
function dxDayOfWeekOffset(const AValue: TDateTime; AStartOfWeek: TDay): TdxDayOfWeek; overload;

function dxDayOfWeekFromVCL(const AValue: TdxDayOfWeekVCL): TDay;
function dxDayOfWeekFromSystem(const AValue: TdxDayOfWeekSystem): TDay;
function dxDayOfWeekToVCL(AValue: TDay): TdxDayOfWeekVCL;
function dxDayOfWeekToSystem(AValue: TDay): TdxDayOfWeekSystem;

function dxGetDayOfWeek(ADayFrom: TDay; ADaysCount: Integer): TDay;
function dxGetStartDateOfWeek(const AValue: TDateTime): TDateTime; overload;
function dxGetStartDateOfWeek(const AValue: TDateTime; AStartOfWeek: TDay): TDateTime; overload;

var
  dxMinYear: Integer = 100;
  dxMaxYear: Integer = 9999;


function cxGetCalendarInfo(Locale: LCID; Calendar: CALID;
  CalendType: CALTYPE; lpCalData: PChar; cchData: Integer;
  lpValue: PDWORD): Integer;
function cxGetCalendar(ACalendType: CALTYPE): TcxCustomCalendarTable;
function cxGetCalendarID(Locale: LCID): TcxCALID;
function cxGetDefaultCalendar(Locale: LCID): TcxCustomCalendarTable;
function cxGetLocalCalendarID: TcxCALID;
function cxGetLocalCalendar: TcxCustomCalendarTable;

function cxDateToLocalFormatStr(ADate: TDateTime): string;
function cxDateToStr(ADate: TDateTime): string; overload;
function cxDateToStr(ADate: TDateTime; const AFormat: TFormatSettings): string; overload;
function cxDayNumberToLocalFormatStr(ADate: TDateTime): string; overload;
function cxDayNumberToLocalFormatStr(ADay: Integer; ACalendar: TcxCustomCalendarTable = nil): string; overload;

// local settings
function cxGetLocalDateSeparator: Char;
function cxGetLocalFormatSettings: TFormatSettings;
function cxGetLocalLongDateFormat: string;
function cxGetLocalLongTimeFormat: string;
function cxGetLocalShortDateFormat: string;
function cxGetLocalTimeAMString: string;
function cxGetLocalTimePMString: string;
function cxGetLocalTimeSeparator: Char;

function cxGetLocalMonthName(ADate: TDateTime; ACalendar: TcxCustomCalendarTable): string; overload;
function cxGetLocalMonthName(AYear, AMonth: Integer; ACalendar: TcxCustomCalendarTable): string; overload;
function cxGetLocalMonthYear(ADate: TDateTime; ACalendar: TcxCustomCalendarTable = nil): string;
function cxGetLocalYear(ADate: TDateTime; ACalendar: TcxCustomCalendarTable  = nil): string;

function cxGetDayOfWeekName(ADay: TDay; AFontCharset: TFontCharset): string;
function cxLocalFormatStrToDate(const ADateStr: string): TDateTime;
function cxStrToDate(const ADateStr: string;
  ACalendar: TcxCustomCalendarTable = nil): TcxDateTime; overload;
function cxStrToDate(const ADateStr: string; const AFormat: TFormatSettings;
  ACalendar: TcxCustomCalendarTable = nil): TcxDateTime; overload;
function cxStrToDate(const ADateString: string; const AFormat: TFormatSettings;
  ACALTYPE: CALTYPE): TDate; overload;
function cxTimeToStr(ATime: TDateTime): string; overload;
function cxTimeToStr(ATime: TDateTime; const ATimeFormat: string): string; overload;
function cxTimeToStr(ATime: TDateTime; const AFormatSettings: TFormatSettings): string; overload;

procedure AddDateRegExprMaskSmartInput(var AMask: string; ACanEnterTime: Boolean);

function dxGetCalendarDateElement(ADate: TDateTime; AElement: TcxDateElement; ACalendar: TcxCustomCalendarTable): Integer;

function cxGetDateFormat(ADate: TDateTime; out AFormatDate: string; AFlags: Integer; const AFormat: string = ''): Boolean;
function DateToLongDateStr(ADate: TDateTime): string;

function cxDateTimeToText(ADate: TDateTime; AFourDigitYearNeeded: Boolean = False; AUseDelphiDateTimeFormats: Boolean = False): string;
function cxTextToDateTime(const AText: string; AUseDelphiDateTimeFormats: Boolean = False): TDateTime;
function DateTimeToText(ADate: TDateTime; AFourDigitYearNeeded: Boolean = False): string;
function DateTimeToTextEx(const ADate: TDateTime; AIsMasked: Boolean;
  AIsDateTimeEdit: Boolean = False; AFourDigitYearNeeded: Boolean = False): string;
function TextToDateEx(AText: string; var ADate: TDateTime; const AFormatString: string = ''): Boolean;
function SmartTextToDate(const AText: string; var ADate: TDateTime): Boolean;
function cxStrToDateTime(S: string; AUseOleDateFormat: Boolean; out ADate: TDateTime): Boolean;

var
  cxUseSingleCharWeekNames: Boolean = True;
  scxDateEditSmartInput: array [TcxDateEditSmartInput] of string;
  DefaultTimeZoneInfo: TcxTimeZoneInformation;

implementation

uses
  RTLConsts, DateUtils, cxLibraryStrs, cxClasses, dxCalendarUtils, cxFormats,
  cxGraphics, StrUtils;

const
  dxDayConvertTableSystem: array[TdxDayOfWeekSystem] of TDay = (dMonday, dTuesday, dWednesday, dThursday, dFriday, dSaturday, dSunday);
  dxDayConvertTableVCL: array[TdxDayOfWeekVCL] of TDay = (dSunday, dMonday, dTuesday, dWednesday, dThursday, dFriday, dSaturday);

var
  FAssignedStartOfWeek: Boolean;
  FCalendarList: TdxFastObjectList;
  FStartOfWeek: TDay;

type
  TcxDateOrder = (doMDY, doDMY, doYMD);
  TcxMonthView = (mvName, mvDigital, mvNone);
  TcxYearView = (yvFourDigitals, yvTwoDigitals, yvNone);

function GetDateOrder(const AFormatInfo: TcxDateTimeFormatInfo): TcxDateOrder; overload;
var
  I: Integer;
begin
  Result := doMDY;
  for I := 0 to AFormatInfo.ItemCount - 1 do
  begin
    case AFormatInfo.Items[I].Kind of
      dtikYear:
        begin
          Result := doYMD;
          Break;
        end;
      dtikMonth:
        begin
          Result := doMDY;
          Break;
        end;
      dtikDay:
        begin
          Result := doDMY;
          Break;
        end;
    end;
  end;
end;

function GetDateOrder(const ADateFormat: string): TcxDateOrder; overload;
var
  I: Integer;
begin
  Result := doMDY;
  I := 1;
  while I <= Length(ADateFormat) do
  begin
    case Chr(Ord(ADateFormat[I]) and $DF) of
      'E': Result := doYMD;
      'Y': Result := doYMD;
      'M': Result := doMDY;
      'D': Result := doDMY;
    else
      Inc(I);
      Continue;
    end;
    Exit;
  end;
end;

function GetCountOfChar(const S: string; Ch: Char; var APos: Integer): Integer;
begin
  Result := APos;
  while (APos <= Length(S)) and (S[APos] = Ch)do
    Inc(APos);
  Result := APos - Result;
end;

function GetMonthView(const ADateFormat: string; var APos: Integer): TcxMonthView;
var
  ACount: Integer;
begin
  ACount := GetCountOfChar(AnsiLowerCase(ADateFormat), 'm', APos);
  if ACount = 4 then
    Result := mvName
  else
    if ACount = 0 then
      Result := mvNone
    else
      Result := mvDigital;
end;

function cxDateToStrByFormat(const ADate: TDateTime; const ADateFormat: string; const ADateSeparator: Char): string;

  function AddZeros(const S: string; ALength: Integer): string;
  begin
    Result := S;
    if ALength <= Length(S) then
      Exit;
    Result := StringOfChar('0', ALength - Length(Result)) + Result;
  end;

  function GetYearView(const ADateFormat: string; var APos: Integer): TcxYearView;
  var
    ACount: Integer;
  begin
    ACount := GetCountOfChar(AnsiLowerCase(ADateFormat), 'y', APos);
    if ACount = 4 then
      Result := yvFourDigitals
    else
      if ACount = 0 then
        Result := yvNone
      else
        Result := yvTwoDigitals;
  end;

  function MonthToStr(AMonth: Integer; AView: TcxMonthView): string;
  begin
    case AView of
      mvName:
        Result := dxFormatSettings.LongMonthNames[AMonth];
      mvDigital:
        Result := AddZeros(IntToStr(AMonth), 2);
      else
        Result := '';
    end;
  end;

  function YearToStr(AYear: Integer; AView: TcxYearView): string;
  begin
    if AView = yvNone then
    begin
      Result := '';
      Exit;
    end;
    Result := IntToStr(AYear);
    if Length(Result) > 4 then
      Result := Copy(Result, Length(Result) - 3, 4);
    Result := AddZeros(Result, 4);
    if AView = yvTwoDigitals then
      Result := Copy(Result, Length(Result) - 1, 2);
  end;

  function FindNextAllowChar(const S: string; var APos: Integer): Boolean;
  const
    AAllowChars = ['d', 'm', 'y', '"', ''''];
  begin
    while (APos <= Length(S)) and not dxCharInSet(AnsiLowerCase(S[APos])[1], AAllowChars) do
      Inc(APos);
    Result := (APos <= Length(S)) and dxCharInSet(AnsiLowerCase(S[APos])[1], AAllowChars);
  end;

  procedure AddPart(var ADateString: string; const APart: string; const ASeparator: string);
  begin
    if (Length(ADateString) > 0) or (ASeparator <> '') then
      if ADateSeparator <> '' then
        ADateString := ADateString + ADateSeparator
      else
        ADateString := ADateString + ASeparator;
    ADateString := ADateString + APart;
  end;

var
  AQuote: Char;
  ASystemDate: TSystemTime;
  APos: Integer;
  ACurrentSeparator: string;
  ACountChar: Integer;
  ADayOfWeek: Integer;
begin
  Result := '';
  DateTimeToSystemTime(ADate, ASystemDate);
  ADayOfWeek := ASystemDate.wDayOfWeek;
  Inc(ADayOfWeek);
  if ADayOfWeek > 7 then
    Dec(ADayOfWeek, 7);
  APos := 1;
  ACurrentSeparator := '';
  FindNextAllowChar(ADateFormat, APos);
  while APos <= Length(ADateFormat) do
  begin
    case AnsiLowerCase(ADateFormat[APos])[1] of
      '''', '"':
        begin
          AQuote := ADateFormat[APos];
          Inc(APos);
          while (APos <= Length(ADateFormat)) and (ADateFormat[APos] <> AQuote) do
          begin
            ACurrentSeparator := ACurrentSeparator + ADateFormat[APos];
            Inc(APos);
          end;
          Inc(APos);
        end;
      'd':
        begin
          ACountChar := GetCountOfChar(AnsiLowerCase(ADateFormat), 'd', APos);
          if ACountChar = 3 then
            AddPart(Result, dxFormatSettings.ShortDayNames[ADayOfWeek], ACurrentSeparator)
          else
            if ACountChar = 4 then
              AddPart(Result, dxFormatSettings.LongDayNames[ADayOfWeek], ACurrentSeparator)
            else
              if ACountChar = 2 then
                AddPart(Result, AddZeros(IntToStr(ASystemDate.wDay), 2), ACurrentSeparator)
              else
                AddPart(Result, IntToStr(ASystemDate.wDay), ACurrentSeparator);
          ACurrentSeparator := '';
        end;
      'y':
        begin
          AddPart(Result, YearToStr(ASystemDate.wYear, GetYearView(ADateFormat, APos)), ACurrentSeparator);
          ACurrentSeparator := '';
        end;
      'm':
        begin
          AddPart(Result, MonthToStr(ASystemDate.wMonth, GetMonthView(ADateFormat, APos)), ACurrentSeparator);
          ACurrentSeparator := '';
        end;
      'e':
        begin
          FindNextAllowChar(ADateFormat, APos);
          ACurrentSeparator := '';
        end;
      else
        begin
          ACurrentSeparator := ACurrentSeparator + ADateFormat[APos];
          Inc(APos);
        end;
      end;
  end;
end;

procedure ScanBlanks(const S: string; var APos: Integer);
var
  I: Integer;
begin
  I := APos;
  while (I <= Length(S)) and (S[I] = ' ') do Inc(I);
  APos := I;
end;

function cxDateToLocalFormatStr(ADate: TDateTime): string;
begin
  Result := cxDateTimeToText(ADate);
end;

function cxDateToStr(ADate: TDateTime): string;
begin
  Result := cxDateToStrByFormat(ADate, dxFormatSettings.ShortDateFormat, dxFormatSettings.DateSeparator);
end;

function cxDateToStr(ADate: TDateTime; const AFormat: TFormatSettings): string;
begin
  Result := cxDateToStrByFormat(ADate, AFormat.ShortDateFormat, AFormat.DateSeparator);
end;

function cxDayNumberToLocalFormatStr(ADate: TDateTime): string;
var
  AOldFormatShortDate: string;
begin
  if not cxGetDateFormat(ADate, Result, 0, 'd') then
  begin
    AOldFormatShortDate := dxFormatSettings.ShortDateFormat;
    dxFormatSettings.ShortDateFormat := 'd';
    try
      Result := DateToStr(ADate);
    finally
      dxFormatSettings.ShortDateFormat := AOldFormatShortDate;
    end;
  end;
end;

function cxDayNumberToLocalFormatStr(ADay: Integer; ACalendar: TcxCustomCalendarTable = nil): string;
var
  ADate: TcxDate;
begin
  if ACalendar = nil then
    ACalendar := cxGetLocalCalendar;
  with ADate do
  begin
    Year := ACalendar.GetMinSupportedYear;
    Month := 1;
    Day := ADay;
  end;
  Result := cxDayNumberToLocalFormatStr(ACalendar.ToDateTime(ADate));
end;

function cxGetLocalDateSeparator: Char;
var
  S: string;
begin
  S := dxGetLocaleInfo(GetThreadLocale, LOCALE_SDATE, '');
  if Length(S) > 0 then
    Result := S[1]
  else
    Result := #0;
end;

function cxGetLocalFormatSettings: TFormatSettings;
begin
  Result.DateSeparator := cxGetLocalDateSeparator;
  Result.ShortDateFormat := cxGetLocalShortDateFormat;
  Result.LongDateFormat := cxGetLocalLongDateFormat;

  Result.TimeSeparator := cxGetLocalTimeSeparator;
  Result.LongTimeFormat := cxGetLocalLongTimeFormat;
  Result.TimeAMString := cxGetLocalTimeAMString;
  Result.TimePMString := cxGetLocalTimePMString;

  Result.ListSeparator := dxFormatSettings.ListSeparator;
end;

function cxGetLocalLongDateFormat: string;
begin
  Result := dxGetLocaleInfo(GetThreadLocale, LOCALE_SLONGDATE, '');
end;

function cxGetLocalLongTimeFormat: string;
begin
  Result := dxGetLocaleInfo(GetThreadLocale, LOCALE_STIMEFORMAT);
end;

function cxGetLocalMonthName(ADate: TDateTime; ACalendar: TcxCustomCalendarTable): string;
var
  AFormat: string;
  AConvertDate: TcxDateTime;
begin
  AConvertDate := ACalendar.FromDateTime(ADate);
  AConvertDate.Day := 1;
  AFormat := 'MMMM';
  if (cxIsGregorianCalendar(ACalendar) and cxFormatController.UseDelphiDateTimeFormats) or
      not cxGetDateFormat(ACalendar.ToDateTime(AConvertDate), Result,
      0, AFormat) then
    Result := dxFormatSettings.LongMonthNames[AConvertDate.Month];
end;

function cxGetLocalMonthName(AYear, AMonth: Integer; ACalendar: TcxCustomCalendarTable): string;
var
  ADate: TcxDate;
begin
  if cxIsGregorianCalendar(ACalendar) and cxFormatController.UseDelphiDateTimeFormats then
  begin
    Result := dxFormatSettings.LongMonthNames[AMonth];
  end
  else
  begin
    ADate.Year := AYear;
    ADate.Month := AMonth;
    ADate.Day := 1;
    if ACalendar.IsValidMonth(ADate.Year, ADate.Month) then
      Result := cxGetLocalMonthName(ACalendar.ToDateTime(ADate), ACalendar)
    else
      Result := '';
  end;
end;

function cxGetLocalMonthYear(ADate: TDateTime; ACalendar: TcxCustomCalendarTable = nil): string;
const
  ADefaultFormat = 'MMMM yyyy';
var
  AFormat: string;
  AConvertDate: TcxDateTime;
  ABuf: array [0..255] of Char;
begin
  if cxIsGregorianCalendar(ACalendar) and cxFormatController.UseDelphiDateTimeFormats then
  begin
    Result := cxDateToStrByFormat(ADate, ADefaultFormat, ' ');
    Exit;
  end;
  if ACalendar = nil then
    ACalendar := cxGetLocalCalendar;
  AConvertDate := ACalendar.FromDateTime(ADate);
  AConvertDate.Day := 1;
  if cxGetCalendarInfo(GetThreadLocale, ACalendar.GetCalendarID, CAL_SYEARMONTH, ABuf, Length(ABuf), nil) > 0 then
    AFormat := ABuf
  else
    AFormat := ADefaultFormat;
  if not cxGetDateFormat(ACalendar.ToDateTime(AConvertDate), Result, 0, AFormat) then
    Result := cxGetLocalMonthName(AConvertDate.Year, AConvertDate.Month, ACalendar) + ' ' +
      cxGetLocalYear(ADate, ACalendar);
end;

function cxGetLocalShortDateFormat: string;
begin
  Result := dxGetLocaleInfo(GetThreadLocale, LOCALE_SSHORTDATE);
end;

function cxGetLocalTimeAMString: string;
begin
  Result := dxGetLocaleInfo(GetThreadLocale, LOCALE_S1159);
end;

function cxGetLocalTimePMString: string;
begin
  Result := dxGetLocaleInfo(GetThreadLocale, LOCALE_S2359);
end;

function cxGetLocalTimeSeparator: Char;
var
  S: string;
begin
  S := dxGetLocaleInfo(GetThreadLocale, LOCALE_STIME);
  if Length(S) > 0 then
    Result := S[1]
  else
    Result := #0;
end;

function cxGetLocalYear(ADate: TDateTime; ACalendar: TcxCustomCalendarTable = nil): string;
var
  AFormat: string;
  AConvertDate: TcxDateTime;
begin
  if ACalendar = nil then
    ACalendar := cxGetLocalCalendar;
  AConvertDate := ACalendar.FromDateTime(ADate);
  AConvertDate.Day := 1;
  AFormat := 'yyyy';
  if not cxGetDateFormat(ACalendar.ToDateTime(AConvertDate), Result, 0, AFormat) then
    Result := IntToStr(AConvertDate.Year);
end;

function cxGetDayOfWeekName(ADay: TDay; AFontCharset: TFontCharset): string;
const
  cxDayNameLCType: array [Boolean, TDay] of Cardinal =
    ((LOCALE_SABBREVDAYNAME7, LOCALE_SABBREVDAYNAME1, LOCALE_SABBREVDAYNAME2, LOCALE_SABBREVDAYNAME3,
    LOCALE_SABBREVDAYNAME4, LOCALE_SABBREVDAYNAME5, LOCALE_SABBREVDAYNAME6),
    (CAL_SSHORTESTDAYNAME7, CAL_SSHORTESTDAYNAME1, CAL_SSHORTESTDAYNAME2, CAL_SSHORTESTDAYNAME3,
    CAL_SSHORTESTDAYNAME4, CAL_SSHORTESTDAYNAME5, CAL_SSHORTESTDAYNAME6));
begin
  if cxFormatController.LocalIsGregorianCalendar and cxFormatController.UseDelphiDateTimeFormats then
    Result := dxFormatSettings.ShortDayNames[dxDayOfWeekToVCL(ADay)]
  else
    Result := dxGetLocaleInfo(GetThreadLocale, cxDayNameLCType[IsWinVistaOrLater, ADay]);
  if cxUseSingleCharWeekNames then
    if cxGetWritingDirection(AFontCharset, Result) = coRightToLeft then
      Result := AnsiLastChar(Result)
    else
    begin
      Result := WideString(Result)[1];
    end;
end;

function cxLocalFormatStrToDate(const ADateStr: string): TDateTime;
var
  D: TcxDateTime;
  ACalendar: TcxCustomCalendarTable;
begin
  case cxGetLocalCalendarID of
    CAL_JAPAN, CAL_TAIWAN, CAL_KOREA, CAL_HIJRI, CAL_THAI, CAL_HEBREW:
      begin
        ACalendar := cxGetLocalCalendar;
        try
          D := cxStrToDate(ADateStr, ACalendar);
          Result := ACalendar.ToDateTime(D);
        finally
          FreeAndNil(ACalendar);
        end;
      end;
    else
      TextToDateEx(ADateStr, Result);
  end;
end;

procedure ScanToNumber(const S: string; var APos: Integer);
begin
  while (APos <= Length(S)) and not dxCharInSet(S[APos], ['0'..'9']) do
  begin
    if dxCharInSet(S[APos], LeadBytes) then
      APos := NextCharIndex(S, APos)
    else
      Inc(APos);
  end;
end;

procedure ScanNumber(const S: string; var APos: Integer);
begin
  while (APos <= Length(S)) and dxCharInSet(S[APos], ['0'..'9']) do
  begin
    if dxCharInSet(S[APos], LeadBytes) then
      APos := NextCharIndex(S, APos)
    else
      Inc(APos);
  end;
end;

function ScanPart(const S: string; var APos: Integer; AEndScan: TdxAnsiCharSet): string;
begin
  Result := '';
  while (APos <= Length(S)) and not dxCharInSet(S[APos], AEndScan) do
  begin
    Result := Result + S[APos];
    Inc(APos);
  end;
end;

function cxStrToDate(const ADateStr: string;
  ACalendar: TcxCustomCalendarTable = nil): TcxDateTime;
begin
  Result := cxStrToDate(ADateStr, cxFormatController.LocalFormatSettings, ACalendar);
end;

function cxStrToDate(const ADateStr: string; const AFormat: TFormatSettings;
  ACalendar: TcxCustomCalendarTable = nil): TcxDateTime; overload;
var
  APart1, APart2, APart3: string;
  H, M, S, MS: Word;
  ATime: TTime;
  APos: Integer;
  AEraName : string;
  AEraYearOffset: Integer;

{$IFNDEF DELPHI19}
  function GetEraYearOffset(const Name: string): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := Low(EraNames) to High(EraNames) do
    begin
      if EraNames[I] = '' then Break;
      if AnsiStrPos(PChar(EraNames[I]), PChar(Name)) <> nil then
      begin
        Result := EraYearOffsets[I];
        Exit;
      end;
    end;
  end;
{$ENDIF}

  function NeedMoreScanMonthStr: Boolean;
  begin
    ScanBlanks(ADateStr, APos);
    Result := (AFormat.DateSeparator = ' ') and
      ((APos < Length(ADateStr)) and not dxCharInSet(ADateStr[APos], ['0'..'9']));
  end;

begin
  APos := 1;
  AEraYearOffset := 0;
  if (AFormat.ShortDateFormat <> '') and (AFormat.ShortDateFormat[1] = 'g') then  // skip over prefix text
  begin
    ScanToNumber(ADateStr, APos);
    AEraName := Trim(Copy(ADateStr, 1, APos-1));
  {$IFDEF DELPHI19}
    AEraYearOffset := FormatSettings.GetEraYearOffset(AEraName);
  {$ELSE}
    AEraYearOffset := GetEraYearOffset(AEraName);
  {$ENDIF}
  end
  else
  {$IFDEF DELPHI19}
    if (AnsiPos('e', FormatSettings.ShortDateFormat) > 0) and
      (High(FormatSettings.EraInfo)>=0) then
      AEraYearOffset := FormatSettings.EraInfo[High(FormatSettings.EraInfo)].EraOffset;
  {$ELSE}
    if AnsiPos('e', AFormat.ShortDateFormat) > 0 then
      AEraYearOffset := EraYearOffsets[1];
  {$ENDIF}
  APart1 := ScanPart(ADateStr, APos, [AFormat.DateSeparator]);
  Inc(APos);
  APart2 := ScanPart(ADateStr, APos, [AFormat.DateSeparator]);
  Inc(APos);
  APart3 := ScanPart(ADateStr, APos, [' ']);
  Result.Era := -1;
  if ACalendar = nil then
    ACalendar := cxGetLocalCalendar;
  case GetDateOrder(AFormat.ShortDateFormat) of
    doMDY:
      begin
        if NeedMoreScanMonthStr then
        begin
          APart1 := APart1 + ' ' + APart2;
          APart2 := APart3;
          APart3 := ScanPart(ADateStr, APos, [' ']);
        end;
        Result.Year := ACalendar.GetYearNumber(APart3);
        Result.Month := ACalendar.GetMonthNumber(Result.Year, APart1);
        Result.Day := ACalendar.GetDayNumber(APart2);
      end;
    doDMY:
      begin
        if NeedMoreScanMonthStr then
        begin
          APart2 := APart2 + ' ' + APart3;
          APart3 := ScanPart(ADateStr, APos, [' ']);
        end;
        Result.Year := ACalendar.GetYearNumber(APart3);
        Result.Month := ACalendar.GetMonthNumber(Result.Year, APart2);
        Result.Day := ACalendar.GetDayNumber(APart1);
      end;
    doYMD:
      begin
        if NeedMoreScanMonthStr then
        begin
          APart2 := APart2 + ' ' + APart3;
          APart3 := ScanPart(ADateStr, APos, [' ']);
        end;
        Result.Year := ACalendar.GetYearNumber(APart1);
        Result.Month := ACalendar.GetMonthNumber(Result.Year, APart2);
        Result.Day := ACalendar.GetDayNumber(APart3);
      end;
  end;
  Result.Era := ACalendar.GetEra(AEraYearOffset + 1);
  H := 0;
  M := 0;
  S := 0;
  MS := 0;
  if APos < Length(ADateStr) then
  begin
    ATime := StrToTime(Copy(ADateStr, APos, Length(ADateStr) - APos + 1));
    DecodeTime(ATime, H, M, S, MS);
  end;
  with Result do
  begin
    Hours := H;
    Minutes := M;
    Seconds := S;
    Milliseconds := MS;
  end;
end;

function cxStrToDate(const ADateString: string; const AFormat: TFormatSettings;
  ACALTYPE: CALTYPE): TDate;
var
  ACalendar: TcxCustomCalendarTable;
  ADate: TcxDateTime;
begin
  ACalendar := cxGetCalendar(ACALTYPE);
  ADate := cxStrToDate(ADateString, AFormat, ACalendar);
  Result := ACalendar.ToDateTime(ADate);
end;

function cxTimeToStr(ATime: TDateTime): string;
begin
  Result := cxTimeToStr(ATime, cxFormatController.LocalFormatSettings);
end;

function cxTimeToStr(ATime: TDateTime; const ATimeFormat: string): string;
var
  AFormatSettings: TFormatSettings;
begin
  AFormatSettings := cxFormatController.LocalFormatSettings;
  with AFormatSettings do
  begin
    LongTimeFormat := ATimeFormat;
    TimeSeparator := dxFormatSettings.TimeSeparator;
  end;
  Result := cxTimeToStr(ATime, AFormatSettings);
end;

function cxTimeToStr(ATime: TDateTime; const AFormatSettings: TFormatSettings): string;
begin
  DateTimeToString(Result, AFormatSettings.LongTimeFormat, dxTimeOf(ATime));
end;

function cxGetCalendarInfo(Locale: LCID; Calendar: CALID;
  CalendType: CALTYPE; lpCalData: PChar; cchData: Integer;
  lpValue: PDWORD): Integer;
var
  AKernelDLL : Integer;
  AGetCalendarInfo: function (Locale: LCID; Calendar: CALID;
    CalendType: CALTYPE; lpCalData : PChar;
    cchData: Integer; lpValue: lpDWord): Integer; stdcall;
begin
  Result:= 0;
  AKernelDLL:= GetModuleHandle(kernel32);
  if AKernelDLL <> 0 then
  begin
    AGetCalendarInfo := GetProcAddress(AKernelDll, 'GetCalendarInfoW');
    if Assigned(AGetCalendarInfo) then
      Result:= AGetCalendarInfo(Locale, Calendar, CalendType, lpCalData, cchData, lpValue);
  end;
end;

function FindCalendarTable(ACalendarID: CALID): TcxCustomCalendarTable;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FCalendarList.Count - 1 do
    if TcxCustomCalendarTable(FCalendarList[I]).CalendarID = ACalendarID then
    begin
      Result := TcxCustomCalendarTable(FCalendarList[I]);
      Break;
    end;
end;

function cxGetCalendar(ACalendType: CALTYPE): TcxCustomCalendarTable;
begin
  Result := FindCalendarTable(ACalendType);
  if Result = nil then
    Result := cxGetCalendarClass(ACalendType).Create;
end;

function cxGetCalendarID(Locale: LCID): TcxCALID;
begin
  GetLocaleInfo(Locale, LOCALE_ICALENDARTYPE or CAL_RETURN_NUMBER,
    @Result, SizeOf(Result));
end;

function cxGetDefaultCalendar(Locale: LCID): TcxCustomCalendarTable;
begin
  Result := cxGetCalendar(cxGetCalendarID(Locale));
end;

function cxGetLocalCalendarID: TcxCALID;
begin
  Result := cxGetCalendarID(GetThreadLocale);
end;

function cxGetLocalCalendar: TcxCustomCalendarTable;
begin
  Result := cxGetCalendar(cxGetLocalCalendarID);
end;

procedure CorrectTextForDateTimeConversion(var AText: string;
  AOleConversion: Boolean; ADateFormatInfo: TcxDateTimeFormatInfo = nil;
  ATimeFormatInfo: TcxDateTimeFormatInfo = nil);

  procedure InternalStringReplace(var S: WideString; ASubStr: WideString);
  begin
    S := StringReplace(S, ASubStr, GetCharString(' ', Length(ASubStr)),
      [rfIgnoreCase, rfReplaceAll]);
  end;

  procedure GetSpecialStrings(AList: TStringList);
  var
    I: Integer;
  begin
    if AOleConversion then
    begin
      AList.Add(cxFormatController.LocalFormatSettings.DateSeparator);
      AList.Add(cxFormatController.LocalFormatSettings.TimeSeparator);
      AList.Add(cxFormatController.LocalFormatSettings.TimeAMString);
      AList.Add(cxFormatController.LocalFormatSettings.TimePMString);
    end
    else
    begin
      AList.Add(dxFormatSettings.DateSeparator);
      AList.Add(dxFormatSettings.TimeSeparator);
      AList.Add(dxFormatSettings.TimeAMString);
      AList.Add(dxFormatSettings.TimePMString);
    end;
    for I := 0 to AList.Count - 1 do
      AList[I] := AnsiUpperCase(Trim(AList[I]));
  end;

  procedure RemoveStringsThatInFormatInfo(var S: WideString;
    const ADateTimeFormatInfo: TcxDateTimeFormatInfo);
  var
    ASpecialStrings: TStringList;
    ASubStr: string;
    I: Integer;
  begin
    ASpecialStrings := TStringList.Create;
    try
      GetSpecialStrings(ASpecialStrings);
      for I := 0 to ADateTimeFormatInfo.ItemCount - 1 do
      begin
        if ADateTimeFormatInfo.Items[I].Kind = dtikString then
        begin
          ASubStr := AnsiUpperCase(Trim(ADateTimeFormatInfo.Items[I].Data));
          if (ASubStr <> '') and (ASpecialStrings.IndexOf(ASubStr) = -1) then
            InternalStringReplace(S, ASubStr);
        end;
      end;
    finally
      ASpecialStrings.Free;
    end;
  end;

  function GetOleDataOrder: TcxDateOrder;
  begin
    if AOleConversion then
      Result := GetDateOrder(cxFormatController.LocalFormatSettings.ShortDateFormat)
    else
      Result := GetDateOrder(cxFormatController.DateFormatInfo);
  end;

  function GetOleDataSeparator: string;
  var
    I: Integer;
  begin
    if AOleConversion or not cxFormatController.DateFormatInfo.DefinedItems[dtikDateSeparator] then
      Result := cxFormatController.LocalFormatSettings.DateSeparator
    else
    begin
      Result := '';
      for I := 0 to cxFormatController.DateFormatInfo.ItemCount - 1 do
        if cxFormatController.DateFormatInfo.Items[I].Kind = dtikDateSeparator then
        begin
          Result := cxFormatController.DateFormatInfo.Items[I].Data;
          Break;
        end;
    end;
  end;

  procedure CheckStringByDateFormatInfo(var S: WideString;
    const ADateTimeFormatInfo: TcxDateTimeFormatInfo);
  const
    AFormatMap: array[TcxDateOrder] of string = ('%2:s%0:s%1:s%0:s%3:s %4:s',
      '%1:s%0:s%2:s%0:s%3:s %4:s', '%3:s%0:s%2:s%0:s%1:s %4:s');
  var
    ABeginPos, APos: Integer;
    ADay: string;
    AMonth: string;
    AYear: string;
    AOleDateOrder: TcxDateOrder;
    AOleDateSeparator: string;
    AFormatDateOrder: TcxDateOrder;
    I: Integer;
  begin
    RemoveStringsThatInFormatInfo(S, ADateTimeFormatInfo);
    AOleDateOrder := GetOleDataOrder;
    AOleDateSeparator := GetOleDataSeparator;
    AFormatDateOrder := GetDateOrder(ADateTimeFormatInfo);
    if (AFormatDateOrder <> AOleDateOrder) or
      not ADateTimeFormatInfo.DefinedItems[dtikYear] or
      not ADateTimeFormatInfo.DefinedItems[dtikMonth] or
      not ADateTimeFormatInfo.DefinedItems[dtikDay] then
    begin
      APos := 1;
      for I := 0 to ADateTimeFormatInfo.ItemCount - 1 do
      begin
        case ADateTimeFormatInfo.Items[I].Kind of
          dtikMonth:
            AMonth := ScanPart(S, APos, [' ', cxFormatController.LocalFormatSettings.DateSeparator,
              dxFormatSettings.DateSeparator]);
          dtikDay:
            begin
              ABeginPos := APos;
              ScanNumber(S, APos);
              ADay := Copy(S, ABeginPos, APos - ABeginPos);
            end;
          dtikYear:
            begin
              ABeginPos := APos;
              ScanNumber(S, APos);
              AYear := Copy(S, ABeginPos, APos - ABeginPos);
            end;
          dtikDateSeparator:
            Inc(APos);
          dtikString:
            APos := APos + Length(ADateTimeFormatInfo.Items[I].Data);
        else
          Break;
        end;
      end;
      if ADay = '' then
        ADay := '1';
      if AMonth = '' then
        AMonth := '1';
      S := Format(AFormatMap[AOleDateOrder], [AOleDateSeparator, ADay, AMonth, AYear, Copy(S, APos, Length(S) - APos + 1)]);
    end;
  end;

  procedure RemoveUnnecessarySpaces(var S: WideString);
  var
    I: Integer;
  begin
    S := Trim(S);
    I := 2;
    while I < Length(S) - 1 do
      if (S[I] <= ' ') and (S[I + 1] <= ' ') then
        Delete(S, I, 1)
      else
        Inc(I);
  end;

var
  S: WideString;
begin
  S := AText;
  if ADateFormatInfo = nil then
    RemoveStringsThatInFormatInfo(S, cxFormatController.DateFormatInfo)
  else
    CheckStringByDateFormatInfo(S, ADateFormatInfo);
  if ATimeFormatInfo = nil then
    RemoveStringsThatInFormatInfo(S, cxFormatController.TimeFormatInfo)
  else
    RemoveStringsThatInFormatInfo(S, ATimeFormatInfo);
  RemoveUnnecessarySpaces(S);
  if AOleConversion then
    InternalStringReplace(S, cxFormatController.LocalFormatSettings.DateSeparator);
  AText := S;
end;

procedure InitSmartInputConsts;
begin
  scxDateEditSmartInput[deiToday] := cxGetResourceString(@cxSDateToday);
  scxDateEditSmartInput[deiYesterday] := cxGetResourceString(@cxSDateYesterday);
  scxDateEditSmartInput[deiTomorrow] := cxGetResourceString(@cxSDateTomorrow);
  scxDateEditSmartInput[deiSunday] := cxGetResourceString(@cxSDateSunday);
  scxDateEditSmartInput[deiMonday] := cxGetResourceString(@cxSDateMonday);
  scxDateEditSmartInput[deiTuesday] := cxGetResourceString(@cxSDateTuesday);
  scxDateEditSmartInput[deiWednesday] := cxGetResourceString(@cxSDateWednesday);
  scxDateEditSmartInput[deiThursday] := cxGetResourceString(@cxSDateThursday);
  scxDateEditSmartInput[deiFriday] := cxGetResourceString(@cxSDateFriday);
  scxDateEditSmartInput[deiSaturday] := cxGetResourceString(@cxSDateSaturday);
  scxDateEditSmartInput[deiFirst] := cxGetResourceString(@cxSDateFirst);
  scxDateEditSmartInput[deiSecond] := cxGetResourceString(@cxSDateSecond);
  scxDateEditSmartInput[deiThird] := cxGetResourceString(@cxSDateThird);
  scxDateEditSmartInput[deiFourth] := cxGetResourceString(@cxSDateFourth);
  scxDateEditSmartInput[deiFifth] := cxGetResourceString(@cxSDateFifth);
  scxDateEditSmartInput[deiSixth] := cxGetResourceString(@cxSDateSixth);
  scxDateEditSmartInput[deiSeventh] := cxGetResourceString(@cxSDateSeventh);
  scxDateEditSmartInput[deiBOM] := cxGetResourceString(@cxSDateBOM);
  scxDateEditSmartInput[deiEOM] := cxGetResourceString(@cxSDateEOM);
  scxDateEditSmartInput[deiNow] := cxGetResourceString(@cxSDateNow);
end;

procedure AddDateRegExprMaskSmartInput(var AMask: string; ACanEnterTime: Boolean);

  procedure AddString(var AMask: string; const S: string);
  var
    I: Integer;
  begin
    I := 1;
    while I <= Length(S) do
      if S[I] = '''' then
      begin
        AMask := AMask + '\''';
        Inc(I);
      end
      else
      begin
        AMask := AMask + '''';
        repeat
          AMask := AMask + S[I];
          Inc(I);
        until (I > Length(S)) or (S[I] = '''');
        AMask := AMask + '''';
      end;
  end;

var
  I: TcxDateEditSmartInput;
begin
  InitSmartInputConsts;
  AMask := '(' + AMask + ')|(';
  I := Low(TcxDateEditSmartInput);
  if not ACanEnterTime and (I = deiNow) then
    Inc(I);
  AddString(AMask, scxDateEditSmartInput[I]);
  while I < High(TcxDateEditSmartInput) do
  begin
    Inc(I);
    if not(not ACanEnterTime and (I = deiNow)) then
    begin
      AMask := AMask + '|';
      AddString(AMask, scxDateEditSmartInput[I]);
    end;
  end;
  AMask := AMask + ')((\+|-)\d(\d(\d\d?)?)?)?';
end;

function dxGetCalendarDateElement(ADate: TDateTime; AElement: TcxDateElement;
  ACalendar: TcxCustomCalendarTable): Integer;
var
  ACalendarDate: TcxDateTime;
  AYear, AMonth, ADay: Word;
begin
  if ACalendar = nil then
    DecodeDate(ADate, AYear, AMonth, ADay)
  else
  begin
    ACalendarDate := ACalendar.FromDateTime(ADate);
    with ACalendarDate do
    begin
      AYear := Year;
      AMonth := Month;
      ADay := Day;
    end;
  end;
  case AElement of
    deYear:
      Result := AYear;
    deMonth:
      Result := AMonth;
    else
      Result := ADay;
  end;
end;

{!!! TODO: adapt to .net}
function cxGetDateFormat(ADate: TDateTime; out AFormatDate: string; AFlags: Integer; const AFormat: string = ''): Boolean;
var
  L: Integer;
  P: PChar;
  ASystemDate: TSystemTime;
  Buffer: array[0..255] of Char;
begin
  if cxFormatController.LocalIsGregorianCalendar and cxFormatController.UseDelphiDateTimeFormats then
  begin
    Result := True;
    if Length(AFormat) = 0 then
      AFormatDate := cxDateToStrByFormat(ADate, dxFormatSettings.LongDateFormat, #0)
    else
      AFormatDate := cxDateToStrByFormat(ADate, AFormat, #0);
  end
  else
  begin
    DateTimeToSystemTime(ADate, ASystemDate);
    if Length(AFormat) = 0 then P := nil else P := PChar(AFormat);
    L := GetDateFormat(GetThreadLocale, AFlags, @ASystemDate, P, Buffer, Length(Buffer));
    Result := L > 0;
    if Result then SetString(AFormatDate, Buffer, L - 1) else AFormatDate := '';
  end;
end;

function DateToLongDateStr(ADate: TDateTime): string;
begin
  if not cxGetDateFormat(ADate, Result, DATE_LONGDATE) then
    Result := FormatDateTime('dddddd', Date);
end;

function SICompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  S1, S2: string;
begin
  S1 := List[Index1];
  S2 := List[Index2];
  if Length(S1) > Length(S2) then
    Result := -1
  else
    if Length(S1) < Length(S2) then
      Result := 1
    else
      Result := -AnsiCompareText(S1, S2);
end;

function SmartTextToDate(const AText: string; var ADate: TDateTime): Boolean;

  function GetSmartInputKind(const AText: string;
    var Kind: TcxDateEditSmartInput): Boolean;
  var
    I: TcxDateEditSmartInput;
    J: Integer;
    S: string;
  begin
    Result := False;
    with TStringList.Create do
    try
      for I := Low(TcxDateEditSmartInput) to High(TcxDateEditSmartInput) do
        AddObject(scxDateEditSmartInput[I], TObject(I));
      CustomSort(SICompare);
      for J := 0 to Count - 1 do
      begin
        S := Strings[J];
        if AnsiCompareText(S, Copy(AText, 1, Length(S))) = 0 then
        begin
          Kind := TcxDateEditSmartInput(Objects[J]);
          Result := True;
          Break;
        end;
      end;
    finally
      Free;
    end;
  end;

var
  I: TcxDateEditSmartInput;
  L, Delta: Integer;
  S: string;
  Y, M, D: Word;

begin
  InitSmartInputConsts;
  Result := False;
  S := Trim(AText);
  if GetSmartInputKind(S, I) then
  begin
    case I of
      deiToday:
        ADate := Date;
      deiYesterday:
        ADate := Date - 1;
      deiTomorrow:
        ADate := Date + 1;
      deiSunday, deiMonday, deiTuesday, deiWednesday, deiThursday, deiFriday, deiSaturday:
        begin
          ADate := Date;
          Delta := Integer(I) - Integer(deiSunday) + 1 - DayOfWeek(ADate);
          if Delta >= 0 then
            ADate := ADate + Delta
          else
            ADate := ADate + 7 + Delta;
        end;
      deiFirst..deiSeventh:
        begin
          ADate := Date;
          Delta := dxDayOfWeekOffset(ADate) - (Integer(I) - Integer(deiFirst));
          ADate := ADate - Delta;
        end;
      deiBOM:
        begin
          DecodeDate(Date, Y, M, D);
          ADate := EncodeDate(Y, M, 1);
        end;
      deiEOM:
        begin
          DecodeDate(Date, Y, M, D);
          ADate := EncodeDate(Y, M, MonthDays[IsLeapYear(Y), M]);
        end;
      deiNow:
        ADate := Now;
    end;
    L := Length(scxDateEditSmartInput[I]);
    S := Trim(Copy(AText, L + 1, Length(AText)));
    Result := Length(S) = 0;
    if not Result then
    begin
      if (S[1] = '+') or (S[1] = '-') then
      begin
        if S[1] = '+' then Delta := 1 else Delta := -1;
        S := Trim(Copy(S, 2, Length(S)));
        try
          ADate := ADate + Delta * StrToInt(S);
          Result := True;
        except
          on EConvertError do;
        end;
      end;
    end;
  end;
  if not Result and Assigned(SmartTextToDateFunc) then
    Result := SmartTextToDateFunc(AText, ADate);
end;

function TextToDateEx(AText: string; var ADate: TDateTime; const AFormatString: string = ''): Boolean;
var
  AFormatInfo: TcxDateTimeFormatInfo;
begin
  if AFormatString <> '' then
  begin
    AFormatInfo := TcxDateTimeFormatInfo.Create;
    try
      GetDateTimeFormatInfo(AFormatString, AFormatInfo);
      CorrectTextForDateTimeConversion(AText, True, AFormatInfo, AFormatInfo);
    finally
      AFormatInfo.free;
    end;
    ADate := VarToDateTime(AText);
  end
  else
    ADate := cxTextToDateTime(AText, cxFormatController.UseDelphiDateTimeFormats);

  Result := ADate <> NullDate;
end;

function cxDateTimeToText(ADate: TDateTime; AFourDigitYearNeeded: Boolean = False; AUseDelphiDateTimeFormats: Boolean = False): string;

  function GetDateTimeFormat: string;
  var
    I: Integer;
    S: string;
  begin
    if AUseDelphiDateTimeFormats then
    begin
      Result := dxFormatSettings.ShortDateFormat;
      if dxTimeOf(ADate) <> 0 then
        Result := Result + ' ' + dxFormatSettings.LongTimeFormat;
    end
    else
      Result := cxFormatController.LocalFormatSettings.ShortDateFormat;
    if AFourDigitYearNeeded then
    begin
      S := LowerCase(Result);
      if (Pos('yyy', S) = 0) and (Pos('yy', S) > 0) then
      begin
        I := Pos('yy', S);
        Insert(Result[I], Result, I + 2);
        Insert(Result[I], Result, I + 3);
      end;
    end;
  end;

var
  SystemTime: TSystemTime;
  PS: array[0..100] of Char;
begin
  if ADate = NullDate then
    Result := ''
  else
    if AUseDelphiDateTimeFormats then
      DateTimeToString(Result, GetDateTimeFormat, ADate)
    else
    begin
      DateTimeToSystemTime(ADate, SystemTime);
      if GetDateFormat(GetThreadLocale, 0, @SystemTime,
        PChar(GetDateTimeFormat), PS, 100) <> 0 then
      begin
        Result := PS;
        if dxTimeOf(ADate) <> 0 then
        begin
          GetTimeFormat(GetThreadLocale, 0, @SystemTime, nil, PS, 100);
          Result := Result + ' ' + PS;
        end;
      end
      else
        try
          Result := VarFromDateTime(ADate);
        except
          on EVariantError do
            Result := '';
        end;
    end;
end;

function cxTextToDateTime(const AText: string; AUseDelphiDateTimeFormats: Boolean = False): TDateTime;
var
  ADay, AMonth, AYear: Word;
  S: string;
begin
  try
    S := Trim(AText);
    if S = '' then
      Result := NullDate
    else
    begin
      if cxFormatController.LocalIsGregorianCalendar then
      begin
        // Smart Date
        if not SmartTextToDate(S, Result) then
        begin
          CorrectTextForDateTimeConversion(S, not AUseDelphiDateTimeFormats);
          if AUseDelphiDateTimeFormats then
            Result := StrToDateTime(S)
          else
            Result := VarToDateTime(S);
        end;
        DecodeDate(Result, AYear, AMonth, ADay);
        if (Result >= MaxInt) or (Result <= MinInt) or (ADay <= 0) or (AYear > dxMaxYear) then
          Result := NullDate;
      end
      else
        Result := cxLocalFormatStrToDate(S);
    end;
  except
    Result := NullDate;
  end;
end;

function DateTimeToText(ADate: TDateTime; AFourDigitYearNeeded: Boolean = False): string;
begin
  Result := cxDateTimeToText(ADate, AFourDigitYearNeeded, cxFormatController.UseDelphiDateTimeFormats);
end;

function DateTimeToTextEx(const ADate: TDateTime; AIsMasked: Boolean;
  AIsDateTimeEdit: Boolean = False; AFourDigitYearNeeded: Boolean = False): string;
begin
  if ADate = NullDate then
    Result := ''
  else
  begin
    if cxFormatController.LocalIsGregorianCalendar then
    begin
      if AIsMasked then
      begin
        if AIsDateTimeEdit then
          Result := FormatDateTime(cxFormatController.MaskedDateTimeEditFormat, ADate)
        else
          Result := FormatDateTime(cxFormatController.MaskedDateEditFormat, dxDateOf(ADate));
      end
      else
        Result := DateTimeToText(ADate, AFourDigitYearNeeded);
    end
    else
      Result := cxDateToLocalFormatStr(ADate);
  end;
end;

function cxStrToDateTime(S: string; AUseOleDateFormat: Boolean;
  out ADate: TDateTime): Boolean;
begin
  Result := False;
  ADate := NullDate;
  try
    if cxFormatController.LocalIsGregorianCalendar then
    begin
      CorrectTextForDateTimeConversion(S, AUseOleDateFormat);
      if AUseOleDateFormat then
        ADate := VarToDateTime(S)
      else
        ADate := StrToDateTime(S);
    end
    else
      ADate := cxLocalFormatStrToDate(S);
    Result := True;
  except
    on Exception(*EConvertError*) do
      ADate := NullDate;
  end;
end;

{ TcxEra }

constructor TcxEra.Create(AEra: Integer; AStartDate: TDateTime;
  AYearOffset, AMinEraYear, AMaxEraYear: Integer);
begin
  Era := AEra;
  StartDate := AStartDate;
  YearOffset := AYearOffset;
  MinEraYear := AMinEraYear;
  MaxEraYear := AMaxEraYear;
end;

procedure TcxEra.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxEra then
    with Source as TcxEra do
    begin
      Self.Era := Era;
      Self.FMaxEraYear := MaxEraYear;
      Self.FMinEraYear := MinEraYear;
      Self.FStartDate := StartDate;
      Self.FYearOffset := YearOffset;
    end;
end;

{ TcxEras }

function TcxEras.GetItem(AIndex: Integer): TcxEra;
begin
  Result := TcxEra(inherited Items[AIndex]);
end;

procedure TcxEras.SetItem(AIndex: Integer; AValue: TcxEra);
begin
  TcxEra(inherited Items[AIndex]).Assign(AValue);
end;

{ TcxCustomCalendarTable }

constructor TcxCustomCalendarTable.Create;
begin
  FEras := TcxEras.Create;
  FCalendarList.Add(Self);
end;

destructor TcxCustomCalendarTable.Destroy;
begin
  if Assigned(FCalendarList) then
    FCalendarList.Extract(Self);
  FEras.Clear;
  FreeAndNil(FEras);
  inherited Destroy;
end;

procedure TcxCustomCalendarTable.AdjustYear(var AYear, AEra: Integer);
begin
  AdjustYear(AYear, AEra, 1, 1);
end;

procedure TcxCustomCalendarTable.AdjustYear(var AYear, AEra: Integer; AMonth, ADay: Integer);
var
  ACurrentYear: Cardinal;
begin
  ACurrentYear := AYear;
  YearToGregorianYear(ACurrentYear, AEra);
  AEra := GetEra(ACurrentYear, AMonth, ADay);
  if AEra > 0 then
    AYear := Integer(ACurrentYear) - Eras[AEra].YearOffset;
end;

procedure TcxCustomCalendarTable.CheckDateTime(var ADateTime: TDateTime);
begin
  if ADateTime < MinSupportedDate then
    ADateTime := MinSupportedDate;
  if ADateTime > MaxSupportedDate then
    ADateTime := MaxSupportedDate;
end;

function TcxCustomCalendarTable.IsNotValid(ADate: TcxDateTime; out AResult: TDateTime): Boolean;
begin
  with ADate do
    Result := not IsValidDay(Era, Year, Month, Day);
  if Result then
    AResult := MinSupportedDate;
end;

procedure TcxCustomCalendarTable.YearToGregorianYear(var AYear: Cardinal;
  AEra: Integer);
begin
  if AEra = -1 then
    AYear := Integer(AYear) + DefaultEra.YearOffset
  else
    if (AEra >= 0) and (AEra < Eras.Count) then
      AYear := Integer(AYear) + Eras[AEra].YearOffset;
end;

function TcxCustomCalendarTable.AddDays(ADate: TcxDateTime;
  ACountDays: Integer): TDateTime;
begin
  Result := ToDateTime(ADate) + ACountDays;
  CheckDateTime(Result);
end;

function TcxCustomCalendarTable.AddMonths(ADate: TDateTime;
  ACountMonths: Integer): TDateTime;
begin
  Result := AddMonths(FromDateTime(ADate), ACountMonths);
end;

function TcxCustomCalendarTable.AddMonths(ADate: TcxDateTime;
  ACountMonths: Integer): TDateTime;
var
  ASwap: Integer;
  ACurrentMonth: Integer;
  ACurrentYear: Integer;
  ACurrentEra: Integer;
begin
  if IsNotValid(ADate, Result) then
    Exit;
  ACurrentEra := ADate.Era;
  ACurrentMonth := ADate.Month;
  ACurrentYear := ADate.Year;
  Inc(ACurrentMonth, ACountMonths);
  if ACurrentMonth > Integer(GetMonthsInYear(ACurrentEra, ACurrentYear)) then
    ASwap := -1
  else
    ASwap := 1;
  while (ACurrentMonth > Integer(GetMonthsInYear(ACurrentEra, ACurrentYear))) or (ACurrentMonth <= 0) do
  begin
    if ASwap > 0 then
      Inc(ACurrentMonth, ASwap * Integer(GetMonthsInYear(ACurrentEra, ACurrentYear - 1)))
    else
      Inc(ACurrentMonth, ASwap * Integer(GetMonthsInYear(ACurrentEra, ACurrentYear)));
    Inc(ACurrentYear, -ASwap);
  end;
  if not IsValidDay(ACurrentEra, ACurrentYear, ACurrentMonth, ADate.Day) then
    ADate.Day := GetDaysInMonth(ACurrentEra, ACurrentYear, ACurrentMonth);
  AdjustYear(ACurrentYear, ACurrentEra, ACurrentMonth, ADate.Day);
  ADate.Era := ACurrentEra;
  ADate.Year := ACurrentYear;
  ADate.Month := ACurrentMonth;
  if IsNotValid(ADate, Result) then
    Exit;
  Result := ToDateTime(ADate);
  CheckDateTime(Result);
end;

function TcxCustomCalendarTable.AddWeeks(ADate: TDateTime;
  ACountWeeks: Integer): TDateTime;
begin
  Result := AddWeeks(FromDateTime(ADate), ACountWeeks);
end;

function TcxCustomCalendarTable.AddWeeks(ADate: TcxDateTime;
  ACountWeeks: Integer): TDateTime;
begin
  Result := AddDays(ADate, ACountWeeks * 7);
  CheckDateTime(Result);
end;

function TcxCustomCalendarTable.AddYears(ADate: TDateTime;
  ACountYears: Integer): TDateTime;
begin
  Result := AddYears(FromDateTime(ADate), ACountYears);
end;

function TcxCustomCalendarTable.AddYears(ADate: TcxDateTime;
  ACountYears: Integer): TDateTime;
var
  ACurrentYaer: Integer;
  ACurrentEra: Integer;
begin
  if IsNotValid(ADate, Result) then
    Exit;
  ACurrentYaer := Integer(ADate.Year) + ACountYears;
  ACurrentEra := ADate.Era;
  AdjustYear(ACurrentYaer, ACurrentEra);
  if not IsValidYear(ACurrentEra, ACurrentYaer) then
  begin
    Result := MinSupportedDate;
    Exit;
  end;
  if not IsValidMonth(ACurrentEra, ACurrentYaer, ADate.Month) then
    ADate.Month := GetMonthsInYear(ACurrentEra, ACurrentYaer);
  if not IsValidDay(ACurrentEra, ACurrentYaer, ADate.Month, ADate.Day) then
    ADate.Day := GetDaysInMonth(ACurrentEra, ACurrentYaer, ADate.Month);
  AdjustYear(ACurrentYaer, ACurrentEra, ADate.Month, ADate.Day);
  ADate.Year := ACurrentYaer;
  ADate.Era := ACurrentEra;
  Result := ToDateTime(ADate);
  CheckDateTime(Result);
end;

function TcxCustomCalendarTable.FromDateTime(AYear, AMonth,
  ADay: Cardinal): TcxDateTime;
begin
  Result := FromDateTime(AYear, AMonth, ADay, 0, 0, 0, 0);
end;

function TcxCustomCalendarTable.FromDateTime(AYear, AMonth,
  ADay: Cardinal; AHours, AMinutes, ASeconds: Byte;
  AMilliseconds: Word): TcxDateTime;
begin
  Result := FromDateTime(EncodeDateTime(AYear, AMonth, ADay, AHours, AMinutes, ASeconds, AMilliseconds));
end;

function TcxCustomCalendarTable.GetDayOfYear(ADate: TDateTime): Cardinal;
begin
  Result := GetDayOfYear(FromDateTime(ADate));
end;

function TcxCustomCalendarTable.GetDayOfYear(ADate: TcxDateTime): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to ADate.Month - 1 do
    Result := Result + GetDaysInMonth(ADate.Year, I);
  Inc(Result, ADate.Day);
end;

function TcxCustomCalendarTable.GetDaysInMonth(AYear, AMonth: Cardinal): Cardinal;
begin
  Result := GetDaysInMonth(-1, AYear, AMonth);
end;

function TcxCustomCalendarTable.GetDaysInYear(AYear: Cardinal): Cardinal;
begin
  Result := GetDaysInYear(-1, AYear);
end;

function TcxCustomCalendarTable.GetEra(AYear: Integer): Integer;
begin
  Result := GetEra(AYear, 1, 1);
end;

function TcxCustomCalendarTable.GetEra(AYear, AMonth, ADay: Integer): Integer;
var
  I: Integer;
  ADate: TDateTime;
begin
  Result := -1;
  if Eras.Count = 0 then
    Exit;
  ADate := EncodeDate(AYear, AMonth, ADay);
  for I := 0 to Eras.Count - 1 do
    with Eras[I] do
      if (ADate >= StartDate) then
        Result := I;
end;

function TcxCustomCalendarTable.GetFirstDayOfWeek(ADate: TDateTime): TDateTime;
begin
  Result := ToDateTime(GetFirstDayOfWeek(FromDateTime(ADate)));
end;

function TcxCustomCalendarTable.GetFirstDayOfWeek(ADate: TDateTime; AStartDayOfWeek: TDay): TDateTime;
var
  ADays: Integer;
begin
  ADays := Byte(AStartDayOfWeek) - GetWeekDay(ADate);
  if ADays > 0 then
    Dec(ADays, 7);
  Result := ADate + ADays;
end;

function TcxCustomCalendarTable.GetFirstDayOfWeek(
  ADate: TcxDateTime): TcxDateTime;
var
  I: Integer;
begin
  I := GetFirstWeekDay - GetWeekDay(ADate);
  if I > 0 then
    Dec(I, 7);
  Result := FromDateTime(AddDays(ADate, I));
end;

function TcxCustomCalendarTable.GetFirstDayOfWeek(ADate: TcxDateTime; AStartDayOfWeek: TDay): TcxDateTime;
begin
  Result := FromDateTime(GetFirstDayOfWeek(ToDateTime(ADate), AStartDayOfWeek));
end;

function TcxCustomCalendarTable.GetFirstDayOfMonth(const ADate: TDateTime): TDateTime;
begin
  Result := ToDateTime(GetFirstDayOfMonth(FromDateTime(ADate)));
end;

function TcxCustomCalendarTable.GetFirstDayOfMonth(const ADate: TcxDateTime): TcxDateTime;
begin
  Result := ADate;
  Result.Day := 1;
end;

function TcxCustomCalendarTable.GetFirstDayOfYear(const ADate: TDateTime): TDateTime;
begin
  Result := ToDateTime(GetFirstDayOfYear(FromDateTime(ADate)));
end;

function TcxCustomCalendarTable.GetFirstDayOfYear(const ADate: TcxDateTime): TcxDateTime;
begin
  Result := ADate;
  Result.Month := 1;
  Result.Day := 1;
end;

function TcxCustomCalendarTable.GetLastDayOfMonth(const ADate: TcxDateTime): TcxDateTime;
begin
  Result := ADate;
  Result.Day := GetDaysInMonth(ADate.Era, ADate.Year, ADate.Month);
end;

function TcxCustomCalendarTable.GetLastDayOfMonth(const ADate: TDateTime): TDateTime;
begin
  Result := ToDateTime(GetLastDayOfMonth(FromDateTime(ADate)));
end;

function TcxCustomCalendarTable.GetLastDayOfYear(const ADate: TcxDateTime): TcxDateTime;
begin
  Result := ADate;
  Result.Month := GetMonthsInYear(ADate.Era, ADate.Year);
  Result.Day := GetDaysInMonth(ADate.Era, ADate.Year, Result.Month);
end;

function TcxCustomCalendarTable.GetLastDayOfYear(const ADate: TDateTime): TDateTime;
begin
  Result := ToDateTime(GetLastDayOfYear(FromDateTime(ADate)));
end;

function TcxCustomCalendarTable.GetMonthsInYear(AYear: Cardinal): Cardinal;
begin
  Result := GetMonthsInYear(-1, AYear);
end;

function TcxCustomCalendarTable.GetYear(ADate: TDateTime): Cardinal;
begin
  Result := FromDateTime(ADate).Year;
end;

function TcxCustomCalendarTable.GetYear(ADate: TcxDate): Cardinal;
begin
  Result := ADate.Year;
end;

function TcxCustomCalendarTable.GetYear(ADate: TcxDateTime): Cardinal;
begin
  Result := ADate.Year;
end;

function TcxCustomCalendarTable.GetWeekDay(ADate: TDateTime): Byte;
begin
  Result := DayOfWeek(ADate) - 1;
end;

function TcxCustomCalendarTable.GetWeekDay(ADate: TcxDateTime): Byte;
begin
  Result := GetWeekDay(ToDateTime(ADate));
end;

function TcxCustomCalendarTable.GetWeekNumber(ADate: TDateTime; AStartOfWeek: TDay;
  AFirstWeekOfYear: TcxFirstWeekOfYear): Cardinal;
begin
  Result := GetWeekNumber(FromDateTime(ADate), AStartOfWeek, AFirstWeekOfYear);
end;

function TcxCustomCalendarTable.IsLeapDay(AYear, AMonth, ADay: Cardinal): Boolean;
begin
  Result := IsLeapDay(-1, AYear, AMonth, ADay);
end;

function TcxCustomCalendarTable.IsLeapMonth(AYear, AMonth: Cardinal): Boolean;
begin
  Result := IsLeapMonth(-1, AYear, AMonth);
end;

function TcxCustomCalendarTable.IsLeapYear(AYear: Cardinal): Boolean;
begin
  Result := IsLeapYear(-1, AYear);
end;

function TcxCustomCalendarTable.IsValidDay(AYear, AMonth,
  ADay: Cardinal): Boolean;
begin
  Result := IsValidDay(-1, AYear, AMonth, ADay);
end;

function TcxCustomCalendarTable.IsValidDay(AEra: Integer;AYear, AMonth,
  ADay: Cardinal): Boolean;
begin
  Result := IsValidMonth(AEra, AYear, AMonth) and
    (ADay > 0) and (ADay <= GetDaysInMonth(AEra, AYear, AMonth));
end;

function TcxCustomCalendarTable.IsValidDate(ADate: TDateTime): Boolean;
var
  AConvertDate: TcxDateTime;
begin
  AConvertDate := FromDateTime(ADate);
  with AConvertDate do
    Result := IsValidDay(Year, Month, Day);
end;

function TcxCustomCalendarTable.IsValidMonth(AYear,
  AMonth: Cardinal): Boolean;
begin
  Result := IsValidMonth(-1, AYear, AMonth);
end;

function TcxCustomCalendarTable.IsValidMonth(AEra: Integer; AYear,
  AMonth: Cardinal): Boolean;
begin
  Result := IsValidYear(AEra, AYear) and
    (AMonth > 0) and (AMonth <= GetMonthsInYear(AEra, AYear));
end;

function TcxCustomCalendarTable.IsValidYear(AYear: Cardinal): Boolean;
begin
  Result := IsValidYear(-1, AYear);
end;

function TcxCustomCalendarTable.IsValidYear(AEra: Integer; AYear: Cardinal): Boolean;
begin
  Result := (Integer(AYear) >= GetMinSupportedYear) and
    (Integer(AYear) <= GetMaxSupportedYear);
end;

function TcxCustomCalendarTable.ToDateTime(ADate: TcxDate): TDateTime;
var
  ADateTime: TcxDateTime;
begin
  with ADateTime do
  begin
    Year := ADate.Year;
    Month := ADate.Month;
    Day := ADate.Day;
    Hours := 0;
    Minutes := 0;
    Seconds := 0;
    Milliseconds := 0;
  end;
  Result := ToDateTime(ADateTime);
end;

function TcxCustomCalendarTable.ToDateTime(AYear, AMonth,
  ADay: Cardinal): TDateTime;
begin
  Result := ToDateTime(AYear, AMonth, ADay, 0, 0, 0, 0);
end;

function TcxCustomCalendarTable.ToDateTime(AYear, AMonth, ADay: Cardinal;
  AHours, AMinutes, ASeconds: Byte; AMilliseconds: Word): TDateTime;
var
  ADateTime: TcxDateTime;
begin
  with ADateTime do
  begin
    Era := -1;
    Year := AYear;
    Month := AMonth;
    Day := ADay;
    Hours := AHours;
    Minutes := AMinutes;
    Seconds := ASeconds;
    Milliseconds := AMilliseconds;
  end;
  Result := ToDateTime(ADateTime);
end;

function TcxCustomCalendarTable.GetDayNumber(const S: string): Integer;
begin
  Result := StrToInt(S);
end;

function TcxCustomCalendarTable.GetMonthNumber(AYear: Integer; const S: string): Integer;
var
  I: Integer;
begin
  for I := 1 to 12 do
  begin
    if (AnsiCompareText(S, dxFormatSettings.LongMonthNames[I]) = 0) or
      (AnsiCompareText(S, dxFormatSettings.ShortMonthNames[I]) = 0) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := StrToInt(S);
end;

function TcxCustomCalendarTable.GetYearNumber(const S: string): Integer;
var
  ATwoDigitYearMax: Integer;
  ARightDigitYear: Integer;
  AAge: Integer;
begin
  Result := StrToInt(S);
  if Length(S) <= 2 then
  begin
    cxGetCalendarInfo(GetThreadLocale, GetCalendarID, CAL_ITWODIGITYEARMAX or
      CAL_RETURN_NUMBER, nil, 0, @ATwoDigitYearMax);
    AAge := ATwoDigitYearMax div 100;
    ARightDigitYear := ATwoDigitYearMax - AAge * 100;
    if Result <= ARightDigitYear then
      Result := Result + AAge * 100
    else
      Result := Result + (AAge - 1) * 100
  end;
end;

{TdxTimeZoneHelper}

class function TdxTimeZoneHelper.AddInternalTimeZone(
  const ATimeZone: TcxTimeZoneInformation): Integer;
var
  ALength: Integer;
begin
  ALength := Length(FTimeZoneInformations);
  Inc(ALength);
  SetLength(FTimeZoneInformations, ALength);
  FTimeZoneInformations[ALength - 1] := ATimeZone;
  Result := ALength - 1;
end;

class function TdxTimeZoneHelper.AddDSTInfo(var ATimeZoneInfo: TcxTimeZoneInformation;
 const AYear: Integer; const AStandardDate, ADaylightDate: TSystemTime): Integer;
begin
  if AStandardDate.wMonth <> 0 then
    Result := AddDSTInfo(ATimeZoneInfo, AYear,
      CalculateTransitionDate(AYear, AStandardDate), CalculateTransitionDate(AYear, ADaylightDate))
  else
    Result := AddDSTInfo(ATimeZoneInfo, AYear, 0, 0);
end;

class function TdxTimeZoneHelper.AddDSTInfo(var ATimeZoneInfo: TcxTimeZoneInformation;
  const AYear: Integer; const AStandardDate, ADaylightDate: TDateTime): Integer;
begin
  Result := Length(ATimeZoneInfo.DynamicDST);
  SetLength(ATimeZoneInfo.DynamicDST, Result + 1);
  with ATimeZoneInfo.DynamicDST[Result] do
  begin
    Year := AYear;
    StandardDate := AStandardDate;
    DaylightDate := ADaylightDate;
  end;
end;

class function TdxTimeZoneHelper.AddDSTInfo(const ATimeZone: Integer;
  const AYear: Integer; const AStandardDate, ADaylightDate: TDateTime): Integer;
begin
  Result := AddDSTInfo(FTimeZoneInformations[CheckTimeIndex(ATimeZone)],
    AYear, AStandardDate, ADaylightDate);
end;

class function TdxTimeZoneHelper.CalculateTransitionDate(const AYear: Word;
  const ADSTDateTime: TSystemTime): TDateTime;
var
  ADay: Integer;
  ADaysInMonth: Word;
begin
  if ADSTDateTime.wYear = AYear then
    Result := SystemTimeToDateTime(ADSTDateTime)
  else
  begin
    if (ADSTDateTime.wDay >= 1) and (ADSTDateTime.wDay <= 4) then
    begin
      ADay := DayOfWeek(StartOfAMonth(AYear, ADSTDateTime.wMonth));
      ADay := 1 + (ADSTDateTime.wDayOfWeek + 1) - ADay;
      if ADay <= 0 then
        Inc(ADay, 7);
      ADay := ADay + 7 * (ADSTDateTime.wDay - 1);
    end
    else
    begin
      ADaysInMonth := DaysInMonth(EncodeDate(AYear, ADSTDateTime.wMonth, 1));
      ADay  := DayOfWeek(EncodeDate(AYear, ADSTDateTime.wMonth, ADaysInMonth));
      ADay  := ADaysInMonth + (ADSTDateTime.wDayOfWeek + 1 - ADay);
      if ADay > ADaysInMonth then
        Dec(ADay, 7);
    end;
    Result := EncodeDate(AYear, ADSTDateTime.wMonth, ADay) + EncodeTime(ADSTDateTime.wHour,
      ADSTDateTime.wMinute, ADSTDateTime.wSecond, ADSTDateTime.wMilliseconds);
  end;
end;

class function TdxTimeZoneHelper.ConvertToGlobalTime(
  const ADateTime: TDateTime; ATimeZone: Integer = -1): TDateTime;
begin
  Result := ADateTime + TimeZoneBias(CheckTimeIndex(ATimeZone));
end;

class function TdxTimeZoneHelper.ConvertToLocalTime(
  const ADateTime: TDateTime; ATimeZone: Integer = -1): TDateTime;
begin
  Result := ADateTime - TimeZoneBias(CheckTimeIndex(ATimeZone));
end;

class function TdxTimeZoneHelper.CurrentTimeZone: Integer;
begin
  Result := FCurrentTimeZone;
end;

class function TdxTimeZoneHelper.CurrentTimeZoneBias: Double;
begin
  Result := TimeZoneBias(CurrentTimeZone);
end;

class function TdxTimeZoneHelper.CheckTimeIndex(
  const AIndex: Integer): Integer;
begin
  Result := AIndex;
  if AIndex = -1 then
    Result := CurrentTimeZone;
end;

class function TdxTimeZoneHelper.cxTZInfoToTZInfo(
  const AInfo: TcxTimeZoneInformation): TTimeZoneInformation;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Bias := AInfo.TZI.Bias;
  Result.StandardBias := AInfo.TZI.StandardBias;
  Result.DaylightBias := AInfo.TZI.DaylightBias;
  Result.StandardDate := AInfo.TZI.StandardDate;
  Result.DaylightDate := AInfo.TZI.DaylightDate;
  Move(AInfo.StandardName[1], Result.StandardName, Length(AInfo.StandardName) shl 1);
  Move(AInfo.DaylightName[1], Result.DaylightName, Length(AInfo.DaylightName) shl 1);
end;

class function TdxTimeZoneHelper.TZInfoTocxTZInfo(
  const AInfo: TTimeZoneInformation): TcxTimeZoneInformation;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.TZI.Bias := AInfo.Bias;
  Result.TZI.StandardBias := AInfo.StandardBias;
  Result.TZI.DaylightBias := AInfo.DaylightBias;
  Result.TZI.StandardDate := AInfo.StandardDate;
  Result.TZI.DaylightDate := AInfo.DaylightDate;
  Result.StandardName := AInfo.StandardName;
  Result.DaylightName := AInfo.DaylightName;
end;

class function TdxTimeZoneHelper.IsDaylightDateTime(ATimeZone: Integer;
  ADateTime: TDateTime): Boolean;
var
  AYear: Word;
  AIndex: Integer;
  AInfo: ^TcxTimeZoneInformation;
begin
  ATimeZone := CheckTimeIndex(ATimeZone);
  if (ATimeZone >= 0) and (ATimeZone < Length(FTimeZoneInformations)) then
    AInfo := @FTimeZoneInformations[ATimeZone]
  else
    AInfo := @DefaultTimeZoneInfo;
  AYear := YearOf(ADateTime);
  Result := (AInfo^.TZI.StandardDate.wMonth <> 0) and not (ADateTime = NullDate) and (AYear > 0);
  if Result then
  begin
    ADateTime := Trunc(ADateTime);
    AIndex := Length(AInfo.DynamicDST) - 1;
    while (AIndex >= 0) and (AInfo^.DynamicDST[AIndex].Year <> AYear) do
      Dec(AIndex);
    if AIndex < 0 then
      AIndex := AddDSTInfo(AInfo^, AYear, AInfo^.TZI.StandardDate, AInfo^.TZI.DaylightDate);
    Result := (ADateTime >= Trunc(AInfo^.DynamicDST[AIndex].DaylightDate)) and
      (ADateTime < Trunc(AInfo^.DynamicDST[AIndex].StandardDate));
  end;
end;

class function TdxTimeZoneHelper.TimeZoneCount: Integer;
begin
  Result := Length(FTimeZoneInformations);
end;

class function TdxTimeZoneHelper.TimeZoneBias(AIndex: Integer): TDateTime;
const
  MinuteToTime = (SecsPerMin * MSecsPerSec) / MSecsPerDay;
begin
  if AIndex = -1 then
    AIndex := CurrentTimeZone;
  Result := TimeZoneInfo(AIndex).TZI.Bias * MinuteToTime;
end;

class function TdxTimeZoneHelper.TimeZoneBiasDelta(AIndex: Integer): TDateTime;
begin
  Result := TimeZoneBias(AIndex) - CurrentTimeZoneBias;
end;

class function TdxTimeZoneHelper.TimeZoneDaylightBias(ADateTime: TDateTime;
  ATimeZone: Integer): Integer;
begin
  Result := 0;
  if ATimeZone < 0 then
    ATimeZone := CurrentTimeZone;
  with TimeZoneInfo(CheckTimeIndex(ATimeZone)).TZI do
    if DaylightDate.wMonth <> 0 then
    begin
      if IsDaylightDateTime(ATimeZone, ADateTime) then
        Inc(Result, DaylightBias)
      else
        Inc(Result, StandardBias);
    end;
end;

class function TdxTimeZoneHelper.TimeZoneInfo(AIndex: Integer): TcxTimeZoneInformation;
begin
  if (AIndex >= 0) and (AIndex < Length(FTimeZoneInformations)) then
    Result := FTimeZoneInformations[AIndex]
  else
    Result := DefaultTimeZoneInfo;
end;

type
  TdxDynamicTimeZoneInformation = packed record
    Bias: Longint;
    StandardName: array[0..31] of WCHAR;
    StandardDate: TSystemTime;
    StandardBias: Longint;
    DaylightName: array[0..31] of WCHAR;
    DaylightDate: TSystemTime;
    DaylightBias: Longint;
    TimeZoneKeyName: array[0..127] of WCHAR;
    DynamicDaylightTimeDisabled: Boolean;
  end;
  PdxDynamicTimeZoneInformation = TdxDynamicTimeZoneInformation;

  TdxGetDynamicTZIProc = function(var AInfo: TdxDynamicTimeZoneInformation): DWORD; stdcall;

function dxGetDynamicTimeZoneInformation(var AInfo: TdxDynamicTimeZoneInformation): Boolean;
var
  AKernel: TdxNativeInt;
  AProc: TdxGetDynamicTZIProc;
begin
  FillChar(AInfo, 0, SizeOf(TdxDynamicTimeZoneInformation));
  AKernel := LoadLibrary(kernel32);
  AProc := GetProcAddress(AKernel, PChar('GetDynamicTimeZoneInformation'));
  Result := Assigned(AProc) and (AProc(AInfo) <> DWORD($FFFFFFFF));
  FreeLibrary(AKernel);
end;

class procedure TdxTimeZoneHelper.ReadTimeZoneInfoFromRegistry(
  ARegistry: TRegistry; out AInfo: TcxTimeZoneInformation);
type
  TcxDynamicDST = packed record
    Bias: LongInt;
    StandardBias: LongInt;
    DaylightBias: LongInt;
    StandardDate: TSystemTime;
    DaylightDate: TSystemTime;
  end;
var
  APath: string;
  ARegDataInfo: TRegDataInfo;
  ADSTInfo: TcxDynamicDST;
  I, AFirstEntry, ALastEntry: Integer;
begin
  if ARegistry.ValueExists('Display') then
    AInfo.Display := ARegistry.ReadString('Display');
  if ARegistry.ValueExists('Std') then
    AInfo.StandardName := ARegistry.ReadString('Std');
  if ARegistry.ValueExists('Dlt') then
    AInfo.DaylightName := ARegistry.ReadString('Dlt');
  if ARegistry.ValueExists('MapID') then
    AInfo.MapID := ARegistry.ReadString('MapID');
  if ARegistry.ValueExists('Index') then
    AInfo.Index := ARegistry.ReadInteger('Index');
  if ARegistry.ValueExists('TZI') then
    ARegistry.ReadBinaryData('TZI', AInfo.TZI, SizeOf(AInfo.TZI));
  if not ARegistry.HasSubKeys then Exit;
  APath := ARegistry.CurrentPath + '\' + 'Dynamic DST';
  ARegistry.CloseKey;
  if ARegistry.OpenKeyReadOnly(APath) then
  begin
    if ARegistry.ValueExists('FirstEntry') then
      AFirstEntry := ARegistry.ReadInteger('FirstEntry')
    else
      AFirstEntry := MaxInt;
    if ARegistry.ValueExists('LastEntry') then
      ALastEntry := ARegistry.ReadInteger('LastEntry')
    else
      ALastEntry := -1;
    for I := AFirstEntry to ALastEntry do
      if ARegistry.GetDataInfo(IntToStr(I), ARegDataInfo) and
        (ARegDataInfo.RegData = rdBinary) and (ARegDataInfo.DataSize >= SizeOf(ADSTInfo)) and
        (ARegistry.ReadBinaryData(IntToStr(I), ADSTInfo, SizeOf(ADSTInfo)) = SizeOf(ADSTInfo)) then
          AddDSTInfo(AInfo, I, ADSTInfo.StandardDate, ADSTInfo.DaylightDate);
  end;
end;

class procedure TdxTimeZoneHelper.InitTimeZoneInformation;
var
  I: Integer;
  ARegistry: TRegistry;
  ASubKeys: TStringList;
  ACurrentZoneKeyName: string;
  ADynamicZoneInfo: TdxDynamicTimeZoneInformation;
  ATimeZoneInfo, ACurTimeZoneInfo: TTimeZoneInformation;
const
  AKeys: array[Boolean] of string =
    ('SOFTWARE\Microsoft\Windows\CurrentVersion\Time Zones',
     'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones');
begin
  ARegistry := TRegistry.Create(KEY_READ);
  try
    GetTimeZoneInformation(ACurTimeZoneInfo);
    DefaultTimeZoneInfo := TZInfoTocxTZInfo(ACurTimeZoneInfo);
    if dxGetDynamicTimeZoneInformation(ADynamicZoneInfo) then
      ACurrentZoneKeyName := ADynamicZoneInfo.TimeZoneKeyName
    else
      ACurrentZoneKeyName := '';
    ARegistry.RootKey := HKEY_LOCAL_MACHINE;
    if ARegistry.OpenKeyReadOnly(AKeys[IsWinNT]) and ARegistry.HasSubKeys then
    begin
      ASubKeys := TStringList.Create;
      try
        ARegistry.GetKeyNames(ASubKeys);
        ARegistry.CloseKey;
        SetLength(FTimeZoneInformations, ASubKeys.Count);
        for I := 0 to ASubKeys.Count - 1 do
          if ARegistry.OpenKeyReadOnly(AKeys[IsWinNT] + '\' + ASubKeys[I]) then
          begin
            ReadTimeZoneInfoFromRegistry(ARegistry, FTimeZoneInformations[I]);
            ARegistry.CloseKey;
            ATimeZoneInfo := cxTZInfoToTZInfo(FTimeZoneInformations[I]);
            if ((ACurrentZoneKeyName = '') and CompareMem(@ATimeZoneInfo, @ACurTimeZoneInfo, SizeOf(ATimeZoneInfo.Bias) +
              SizeOf(ATimeZoneInfo.StandardName))) or SameText(ACurrentZoneKeyName, ASubKeys[I]) then
              FCurrentTimeZone := I;
          end;
      finally
        ASubKeys.Free;
      end;
    end;
  finally
    ARegistry.Free;
  end;
end;

class procedure TdxTimeZoneHelper.DoneTimeZoneInformation;
begin
  SetLength(FTimeZoneInformations, 0);
end;

////////////////////////////////////////////////////////////////////////////////
procedure dxDecMonth(var AYear, AMonth: Word);
begin
  dxChangeMonth(AYear, AMonth, -1);
end;

procedure dxIncMonth(var AYear, AMonth: Word);
begin
  dxChangeMonth(AYear, AMonth, 1);
end;

procedure dxChangeMonth(var AYear, AMonth: Word; Delta: Integer);
var
  AIntMonth: Integer;
begin
  AIntMonth := AMonth;
  Inc(AYear, Delta div 12);
  Inc(AIntMonth, Delta mod 12);
  if AIntMonth < 1 then
  begin
    Dec(AYear);
    Inc(AIntMonth, 12);
  end;
  if AIntMonth > 12 then
  begin
    Inc(AYear);
    Dec(AIntMonth, 12);
  end;
  AMonth := AIntMonth;
end;

function IsLeapYear(AYear: Integer): Boolean;
begin
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

function DaysPerMonth(AYear, AMonth: Integer): Integer;
const
  ADaysInMonth: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := ADaysInMonth[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then Inc(Result);
end;

function CheckDay(AYear, AMonth, ADay: Integer): Integer;
begin
  if ADay < 1 then
    Result := 1
  else
    if ADay > DaysPerMonth(AYear, AMonth) then
      Result := DaysPerMonth(AYear, AMonth)
    else
      Result := ADay;
end;

function dxTimeOf(const AValue: TDateTime): TDateTime;
begin
  if AValue = NullDate then
    Result := 0
  else
    Result := DateTimeToTimeStamp(AValue).Time / MSecsPerDay;
end;

function dxDateOf(const AValue: TDateTime): TDateTime;
begin
  if AValue = NullDate then
    Result := NullDate
  else
    Result := DateTimeToTimeStamp(AValue).Date - DateDelta;
end;

function cxIsDateValid(ADate: Double): Boolean;
begin
  Result := (ADate = NullDate) or
    ((ADate >= MinDateTime) and (ADate <= MaxDateTime));
end;

function dxGetDateElement(ADate: TDateTime; AElement: TcxDateElement): Integer;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(ADate, AYear, AMonth, ADay);
  case AElement of
    deYear:
      Result := AYear;
    deMonth:
      Result := AMonth;
    else
      Result := ADay;
  end;
end;

function dxGetStartDateOfMonth(const ADate: TDateTime): TDateTime;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(ADate, AYear, AMonth, ADay);
  Result := EncodeDate(AYear, AMonth, 1);
end;

function dxGetEndDateOfMonth(const ADate: TDateTime; AIgnoreTime: Boolean): TDateTime;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(ADate, AYear, AMonth, ADay);
  Result := EncodeDate(AYear, AMonth, MonthDays[IsLeapYear(AYear), AMonth]);
  if not AIgnoreTime then
    Result := Result + EncodeTime(23, 59, 59, 999);
end;

function dxGetStartDateOfYear(const ADate: TDateTime): TDateTime;
begin
  Result := EncodeDate(dxGetDateElement(ADate, deYear), 1, 1);
end;

function dxGetEndDateOfYear(const ADate: TDateTime; AIgnoreTime: Boolean): TDateTime;
begin
  Result := EncodeDate(dxGetDateElement(ADate, deYear), 12, 31);
  if not AIgnoreTime then
    Result := Result + EncodeTime(23, 59, 59, 999);
end;

function dxGetStartOfWeek: TDay;
begin
  if FAssignedStartOfWeek then
    Result := FStartOfWeek
  else
    Result := dxGetLocalStartOfWeek;
end;

function dxGetAssignedStartOfWeek: Boolean;
begin
  Result := FAssignedStartOfWeek;
end;

procedure dxResetAssignedStartOfWeek;
begin
  FAssignedStartOfWeek := False;
end;

procedure dxSetStartOfWeek(AValue: TDay);
begin
  FStartOfWeek := AValue;
  FAssignedStartOfWeek := True;
end;

function dxGetLocalFirstWeekOfYear: TcxFirstWeekOfYear;
begin
  Result := TcxFirstWeekOfYear(StrToInt(dxGetLocaleInfo(GetThreadLocale, LOCALE_IFIRSTWEEKOFYEAR, '0')[1]) + 1);
end;

function dxGetLocalStartOfWeek: TDay;
begin
  Result := dxDayOfWeekFromSystem(StrToInt(dxGetLocaleInfo(GetThreadLocale, LOCALE_IFIRSTDAYOFWEEK, '0')));
end;

function dxGetWeekNumber(ADate: TDateTime; AStartOfWeek: TDay;
  AFirstWeekOfYear: TcxFirstWeekOfYear): Integer;

  function GetYearFirstWeekDate(AYear: Word): TDateTime;
  var
    AYearStart: TDateTime;
  begin
    if TryEncodeDate(AYear, 1, 1, AYearStart) then
    begin
      case AFirstWeekOfYear of
        fwyJan1:
          Result := dxGetStartDateOfWeek(AYearStart);
        fwyFirstFourDays:
          Result := dxGetStartDateOfWeek(AYearStart + 3);
        else //fwyFirstFullWeek
          Result := dxGetStartDateOfWeek(AYearStart + 6);
      end;
    end
    else
      Result := InvalidDate;
  end;

var
  AYear, AMonth, ADay: Word;
  AStart: TDateTime;
begin
  Result := -1;
  if AFirstWeekOfYear = fwySystem then
    AFirstWeekOfYear := dxGetLocalFirstWeekOfYear;
  DecodeDate(ADate, AYear, AMonth, ADay);
  if AMonth = 12 then
  begin
    AStart := GetYearFirstWeekDate(AYear + 1);
    if (AStart <> InvalidDate) and (ADate >= AStart) then
      Result := 1;
  end;
  if Result < 0 then
  begin
    AStart := GetYearFirstWeekDate(AYear);
    if (AMonth = 1) and (ADate < AStart) then
      AStart := GetYearFirstWeekDate(AYear - 1);
    Result := Trunc(Trunc(ADate) - AStart) div 7 + 1;
  end;
end;

function dxGetMonthNumber(const ADate: TDateTime): Integer;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(ADate, AYear, AMonth, ADay);
  Result := (AYear - 1) * 12 + AMonth;
end;

function dxDayOfWeek(const AValue: TDateTime): TDay;
begin
  Result := dxDayOfWeekFromVCL(DayOfWeek(AValue));
end;

function dxDayOfWeekOffset(const AValue: TDateTime): TdxDayOfWeek;
begin
  Result := dxDayOfWeekOffset(AValue, dxGetStartOfWeek);
end;

function dxDayOfWeekOffset(const AValue: TDateTime; AStartOfWeek: TDay): TdxDayOfWeek;
var
  AResult: Integer;
begin
  AResult := Ord(dxDayOfWeek(AValue)) - Ord(AStartOfWeek);
  if AResult < 0 then
    Inc(AResult, 7);
  Result := AResult;
end;

function dxDayOfWeekFromVCL(const AValue: TdxDayOfWeekVCL): TDay;
begin
  Result := dxDayConvertTableVCL[AValue];
end;

function dxDayOfWeekFromSystem(const AValue: TdxDayOfWeekSystem): TDay;
begin
  Result := dxDayConvertTableSystem[AValue];
end;

function dxDayOfWeekToVCL(AValue: TDay): TdxDayOfWeekVCL;
begin
  Result := Ord(AValue) + 1;
end;

function dxDayOfWeekToSystem(AValue: TDay): TdxDayOfWeekSystem;
var
  AResult: Integer;
begin
  AResult := Ord(AValue) - 1;
  if AResult = -1 then
    AResult := 6;
  Result := AResult;
end;

function dxGetDayOfWeek(ADayFrom: TDay; ADaysCount: Integer): TDay;
var
  AResult: Integer;
begin
  AResult := Ord(ADayFrom) + ADaysCount mod 7;
  if AResult > 6 then
    Dec(AResult, 7);
  Result := TDay(AResult);
end;

function dxGetStartDateOfWeek(const AValue: TDateTime): TDateTime;
begin
  Result := dxDateOf(AValue) - dxDayOfWeekOffset(AValue);
end;

function dxGetStartDateOfWeek(const AValue: TDateTime; AStartOfWeek: TDay): TDateTime;
begin
  Result := dxDateOf(AValue) - dxDayOfWeekOffset(AValue, AStartOfWeek);
end;

procedure InitializeUtils;
begin
  TdxTimeZoneHelper.InitTimeZoneInformation;
end;

procedure FinalizeUtils;
begin
  TdxTimeZoneHelper.DoneTimeZoneInformation;
end;

initialization
  FStartOfWeek := dxGetLocalStartOfWeek;
  FCalendarList := TdxFastObjectList.Create;
  dxUnitsLoader.AddUnit(@InitializeUtils, @FinalizeUtils);

finalization
  dxUnitsLoader.RemoveUnit(@FinalizeUtils);
  FreeAndNil(FCalendarList);
end.
