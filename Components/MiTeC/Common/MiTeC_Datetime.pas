{*******************************************************}
{               MiTeC Common Routines                   }
{                 Datetime routines                     }
{                                                       }
{                                                       }
{       Copyright (c) 1997-2019 Michal Mutl             }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Datetime;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}


{$IFDEF FPC}
type
  TValueRelationship = -1..1;

const
  LessThanValue = Low(TValueRelationship);
  EqualsValue = 0;
  GreaterThanValue = High(TValueRelationship);
{$ENDIF}

type
  TLanguage = (lngEn, lngCz);

  TRegistryTimeZoneInfo = record
    Bias: Longint;
    StandardBias: Longint;
    DaylightBias: Longint;
    StandardDate: TSystemTime;
    DaylightDate: TSystemTime;
  end;

const
  DaysInMonths: array [1..12] of Integer =
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

  MinutesPerDay     = 60 * 24;
  SecondsPerMinute  = 60;
  SecondsPerHour    = 3600;
  SecondsPerDay     = MinutesPerDay * 60;
  MsecsPerMinute    = 60 * 1000;
  MsecsPerHour      = 60 * MsecsPerMinute;
  DaysPerYear       = 365.2422454;          // Solar Year
  DaysPerMonth      = DaysPerYear / 12;
  DateTimeBaseDay   = -693593;              //  1/1/0001
  EncodeDateMaxYear = 9999;
  SolarDifference   = 1.7882454;            //  Difference of Julian Calendar to Solar Calendar at 1/1/10000
  DateTimeMaxDay    = 2958466;              //  12/31/EncodeDateMaxYear + 1;
  FileTimeBase      = -109205.0;
  FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; // 100 nSek per Day

  tzAEST = 'AUS Eastern Standard Time';
  tzJST = 'Tokyo Standard Time';
  tzCET = 'Central Europe Standard Time';
  tzGMT = 'GMT Standard Time';
  tzEST = 'Eastern Standard Time';
  tzPST = 'Pacific Standard Time';

procedure ResetMemory(out P; Size: Longint);

function SecondsToTime(const ASeconds: Int64): TDateTime;
function UTCToDateTime(UTC: Cardinal; ConvertTimeToLocal: Boolean = False): TDateTime;
function UTCToDateTimeEx(UTC: int64; CorrectToLocal: Boolean = True): TDateTime;
function Int64ToDateTime(UTC: Int64; ConvertTimeToLocal: Boolean = False): TDateTime;
function DSTDate2Date(dstDate: TSystemTime; year: word): TDateTime;
function FileTimeToDateTimeStr(FileTime: TFileTime): string;
function FileTimeToDateTime(FT: FILETIME; ConvertTimeToLocal: Boolean = False): TDateTime;
function ParseDate(YYYYMMDD: string): TDatetime;
function EasterSunday(const Year: Word): TDateTime;
function GetWorkDays(ADT: TDatetime; ACount: Integer): Integer;
function DateToStrDef(ADT: TDateTime; ADef: string = ''): string;
function DateTimeToStrDef(ADT: TDateTime; ADef: string = ''; AValidDateLimit: TDatetime = 0): string;
function DateTimeToFileTime(dt: TDateTime): FILETIME;
function IncWorkDays(ADT: TDateTime; ADays: Cardinal): TDateTime;
function DecWorkDays(ADT: TDateTime; ADays: Cardinal): TDateTime;
function UTCToSystemTime(UTC : TDateTime) : TDateTime;
function GetDaylightBias: Integer;
function GetDaylightName: string;
function GetCurrentTimezoneKeyName: string;
procedure GetTimeZoneList(AList: TStringList);
function GetTimeZone(AKeyName: string; out AData: TTimeZoneInformation; AYear: Word = 0): string;
function LocalDateTimeToTZ(AValue: TDateTime; ATZ: TTimeZoneInformation): TDateTime;

function GetDateTimeForBiasSystemTime(ADateTime: TSystemTime; AGivenYear: Integer): TDateTime;
function GetBiasForDate(ADateTime: TDateTime): Integer;
function LocalDateTimeToUTC(Local: TDateTime): TDateTime;
function UTCToLocalDateTime(UTC: TDateTime): TDateTime;
function RFC822DateToDateTime(RFC822DateTime: string): TDateTime;

function CompareSysTime(st1, st2: TSystemTime): integer;

function GreaterOrEquals(ANow,AThen: TDateTime): Boolean;
function LessOrEquals(ANow,AThen: TDateTime): Boolean;
function Less(ANow,AThen: TDateTime): Boolean;
function Greater(ANow,AThen: TDateTime): Boolean;

function IsTimeInRange(ATime: TDateTime; AStartTime, AEndTime: TDateTime; AInclusive: Boolean = True): Boolean;
function IsDateTimeInRange(ADateTime: TDateTime; AStartDateTime, AEndDateTime: TDateTime; aInclusive: Boolean = True): Boolean;
function IsDateInRange(ADate: TDateTime; AStartDate, AEndDate: TDateTime; AInclusive: Boolean = True): Boolean;

function SecondsBetweenSgn(ANow,AThen: TDateTime): Integer;
function MinutesBetweenSgn(ANow,AThen: TDateTime): Integer;
function HoursBetweenSgn(ANow,AThen: TDateTime): Integer;
function DaysBetweenSgn(ANow,AThen: TDateTime): Integer;
function WeeksBetweenSgn(ANow,AThen: TDateTime): Integer;

function FormatTicks(AValue :Int64; AWholeSecondsOnly: Boolean = True) :string;
function FormatSeconds(AValue :Int64; AShort: Boolean = True) :string;
function FormatMilliseconds(AValue :Int64; AWholeSecondsOnly: Boolean = True; AShort: Boolean = True) :string;

const
{$IFDEF CZLNG}
  Zodiac :array[0..12] of record
                           Sign :string;
                           FromDOY, ToDOY :integer;
                           Ruler,Traits :string;
                         end = (
         (Sign:'Aries - Beran';FromDOY:81;ToDOY:111;Ruler:'Mars';Traits:'Stateènost, ráznost a netrpìlivost'),
         (Sign:'Taurus - Býk';FromDOY:112;ToDOY:142;Ruler:'Venuše';Traits:'Odhodlanost, vøelost a tvrdohlavost'),
         (Sign:'Gemini - Blíøenci';FromDOY:143;ToDOY:173;Ruler:'Merkur';Traits:'Èilost, všestrannost a nestálost'),
         (Sign:'Cancer - Rak';FromDOY:174;ToDOY:204;Ruler:'Mesíc';Traits:'Vynalézavost, citlivost a sebeovládání'),
         (Sign:'Leo - Lev';FromDOY:205;ToDOY:235;Ruler:'Slunce';Traits:'Síla, tvùrèí talent a sebevìdomí'),
         (Sign:'Virgo - Panna';FromDOY:236;ToDOY:266;Ruler:'Merkur';Traits:'Skromnost, praktiènost a povznesenost'),
         (Sign:'Libra - Váhy';FromDOY:267;ToDOY:296;Ruler:'Venuše';Traits:'Idealismus, romantika a lehkomyslnost'),
         (Sign:'Scorpio - Štír';FromDOY:297;ToDOY:326;Ruler:'Pluto a Mars';Traits:'Vášnivost, cílevìdomost a žárlivost'),
         (Sign:'Sagittarius - Støelec';FromDOY:327;ToDOY:356;Ruler:'Jupiter';Traits:'Optimismus, aktivita a neklid'),
         (Sign:'Capricorn - Kozoroh';FromDOY:357;ToDOY:366;Ruler:'Saturn';Traits:'Opatrnost, ctižádost a pesimismus'),
         (Sign:'Capricorn - Kozoroh';FromDOY:1;ToDOY:20;Ruler:'Saturn';Traits:'Opatrnost, ctižádost a pesimismus'),
         (Sign:'Aquarius - Vodnáø';FromDOY:21;ToDOY:50;Ruler:'Uran a Saturn';Traits:'Nezávislost, svéráznost a tvrdohlavost'),
         (Sign:'Pisces - Ryby';FromDOY:51;ToDOY:80;Ruler:'Neptun a Jupiter';Traits:'Starostlivost, intuitivnost a neurèitost')
         );
{$ENDIF}

    ZodiacEn :array[0..12] of record
                           Sign :string;
                           FromDOY, ToDOY :integer;
                           Ruler,Traits :string;
                         end = (
         (Sign:'Aries';FromDOY:81;ToDOY:111;Ruler:'Mars';Traits:'Bravery, energy and impatience '),
         (Sign:'Taurus';FromDOY:112;ToDOY:142;Ruler:'Venus';Traits:'Resoluteness, warmth of feeling and stubborness'),
         (Sign:'Gemini';FromDOY:143;ToDOY:173;Ruler:'Mercury';Traits:'Agility, versatility and inconstancy'),
         (Sign:'Cancer';FromDOY:174;ToDOY:204;Ruler:'Moon';Traits:'Inventibility, sensitivity and self-possession'),
         (Sign:'Leo';FromDOY:205;ToDOY:235;Ruler:'Sun';Traits:'Strength, creativity and self-confidence'),
         (Sign:'Virgo';FromDOY:236;ToDOY:266;Ruler:'Mercury';Traits:'Modesty, practicality and loftiness'),
         (Sign:'Libra';FromDOY:267;ToDOY:296;Ruler:'Venus';Traits:'Idealist, romantic and improvidence'),
         (Sign:'Scorpio';FromDOY:297;ToDOY:326;Ruler:'Pluto and Mars';Traits:'Passion, resolution and jealousy'),
         (Sign:'Sagittarius';FromDOY:327;ToDOY:356;Ruler:'Jupiter';Traits:'Optimistic, active and unquiet'),
         (Sign:'Capricorn';FromDOY:357;ToDOY:366;Ruler:'Saturn';Traits:'Prudent, ambitiousness and pesimistic'),
         (Sign:'Capricorn';FromDOY:1;ToDOY:20;Ruler:'Saturn';Traits:'Prudent, ambitiousness and pesimistic'),
         (Sign:'Aquarius';FromDOY:21;ToDOY:50;Ruler:'Uran and Saturn';Traits:'Independent, individual and stubborn'),
         (Sign:'Pisces';FromDOY:51;ToDOY:80;Ruler:'Neptun and Jupiter';Traits:'Careful, intuitive and casual')
         );

type
  TTzSpecificLocalTimeToSystemTime =  function (lpTimeZoneInformation: PTimeZoneInformation; var pLocalTime, pUniversalTime: TSystemTime): BOOL; stdcall;
  TSystemTimeToTzSpecificLocalTime = function (lpTimeZoneInformation: PTimeZoneInformation; var pUniversalTime, pLocalTime: TSystemTime): BOOL; stdcall;
var
  TzSpecificLocalTimeToSystemTime:  TTzSpecificLocalTimeToSystemTime = nil;
  SystemTimeToTzSpecificLocalTime:  TSystemTimeToTzSpecificLocalTime = nil;


implementation

uses {$IFDEF RAD9PLUS}
     System.Types, System.DateUtils, System.Timespan, System.Win.Registry
     {$ELSE}
     Types, DateUtils, Registry
     {$ENDIF}
     ;

procedure ResetMemory(out P; Size: Longint);
begin
  if Size>0 then begin
    Byte(P):=0;
    FillChar(P,Size,0);
  end;
end;

function UTCToSystemTime(UTC : TDateTime) : TDateTime;
var
  TimeZoneInf: _TIME_ZONE_INFORMATION;
  UTCTime,LocalTime: TSystemTime;
begin
  if GetTimeZoneInformation(TimeZoneInf)<$FFFFFFFF then begin
    DatetimetoSystemTime(UTC,UTCTime);
    if SystemTimeToTzSpecificLocalTime(@TimeZoneInf,UTCTime,LocalTime) then begin
      result:=SystemTimeToDateTime(LocalTime);
    end else
      result:=UTC;
  end else
    result:=UTC;
end;

function GetDateTimeForBiasSystemTime(ADateTime: TSystemTime; AGivenYear: Integer): TDateTime;
var
  Year, Month, Day: Word;
  Hour, Minute, Second, MilliSecond: Word;
begin
  with ADateTime do begin
    wYear:=AGivenYear;
    while not TryEncodeDayOfWeekInMonth(wYear,wMonth,wDay,wDayOfWeek,Result) do
      Dec(wDay);
    DecodeDateTime(Result,Year,Month,Day,Hour,Minute,Second,MilliSecond);
    Result:=EncodeDateTime(Year,Month,Day,wHour,wMinute,wSecond,wMilliseconds);
  end;
end;

function GetBiasForDate(ADateTime: TDateTime): Integer;
var
  tzi: TIME_ZONE_INFORMATION;
  st,dt: TDateTime;
  s: string;
begin
  s:=GetCurrentTimezoneKeyName;
  GetTimeZone(s,tzi,YearOf(ADatetime));
  st:=GetDateTimeForBiasSystemTime(tzi.StandardDate,YearOf(ADateTime));
  dt:=GetDateTimeForBiasSystemTime(tzi.DaylightDate,YearOf(ADateTime));
  if tzi.StandardDate.wMonth=0 then
    Result:=tzi.Bias
  else if tzi.StandardDate.wMonth>tzi.DaylightDate.wMonth then
    if (ADateTime<st) and (ADateTime >= dt) then
      Result:=tzi.Bias+tzi.DaylightBias
    else
      Result:=tzi.Bias+tzi.StandardBias
  else if (ADateTime >= st) and (ADateTime<dt) then
    Result:=tzi.Bias+tzi.StandardBias
  else
    Result:=tzi.Bias+tzi.DaylightBias;
end;

function UTCToLocalDateTime(UTC: TDateTime): TDateTime;
var
  sUTC,sLocal: TSystemTime;
begin
  DateTimeToSystemTime(UTC,sUTC);
  if SystemTimeToTzSpecificLocalTime(nil,sUTC,sLocal) then
    Result:=SystemTimeToDateTime(sLocal)
  else
   Result:=IncMinute(UTC,-GetBiasForDate(UTC));
end;

function LocalDateTimeToUTC(Local: TDateTime): TDateTime;
var
  sUTC,sLocal: TSystemTime;
begin
  Result:=0;
  if Assigned(TzSpecificLocalTimeToSystemTime) then begin
    DateTimeToSystemTime(Local,sLocal);
    if TzSpecificLocalTimeToSystemTime(nil,sLocal,sUTC) then
      Result:=SystemTimeToDateTime(sUTC);
  end;
  if Result=0 then
    Result:=IncMinute(Local,GetBiasForDate(Local));
end;

function CompareSysTime(st1, st2: TSystemTime): integer;
begin
  if st1.wYear<st2.wYear then
    Result:=-1
  else
    if st1.wYear>st2.wYear then
      Result:=1
    else
      if st1.wMonth<st2.wMonth then
        Result:=-1
      else
        if st1.wMonth>st2.wMonth then
          Result:=1
        else
          if st1.wDayOfWeek<st2.wDayOfWeek then
            Result:=-1
          else
            if st1.wDayOfWeek>st2.wDayOfWeek then
              Result:=1
            else
              if st1.wDay<st2.wDay then
                Result:=-1
              else
                if st1.wDay>st2.wDay then
                  Result:=1
                else
                  if st1.wHour<st2.wHour then
                    Result:=-1
                  else
                    if st1.wHour>st2.wHour then
                      Result:=1
                    else
                      if st1.wMinute<st2.wMinute then
                        Result:=-1
                      else
                        if st1.wMinute>st2.wMinute then
                          Result:=1
                         else
                           if st1.wSecond<st2.wSecond then
                             Result:=-1
                           else
                             if st1.wSecond>st2.wSecond then
                               Result:=1
                             else
                               if st1.wMilliseconds<st2.wMilliseconds then
                                 Result:=-1
                               else
                                 if st1.wMilliseconds>st2.wMilliseconds then
                                   Result:=1
                                 else
                                   Result:=0;
end;

function GetDaylightBias: Integer;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  GetTimeZoneInformation(TimeZoneInfo);
  Result:=TimeZoneInfo.DaylightBias;
end;

function GetDaylightName: string;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  GetTimeZoneInformation(TimeZoneInfo);
  Result:=TimeZoneInfo.DaylightName;
end;

function GetCurrentTimezoneKeyName: string;
const
  rkTZKN = {HKEY_LOCAL_MACHINE\}'SYSTEM\CurrentControlSet\Control\TimeZoneInformation';
  rvTZKN = 'TimeZoneKeyName';
begin
  Result:='';
  with TRegistry.Create do begin
    Rootkey:=HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(rkTZKN) then begin
      if ValueExists(rvTZKN) then
        Result:=ReadString(rvTZKN);
      CloseKey;
    end;
  end;
end;

procedure GetTimeZoneList(AList: TStringList);
var
  i: Integer;
begin
  AList.Clear;
  with TRegistry.Create do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if not OpenKeyReadOnly('SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones\') then
        Exit;
      GetKeyNames(AList);
      CloseKey;
      for i:=0 to AList.Count-1 do
        if OpenKeyReadOnly(Format('SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones\%s',[AList[i]])) then begin
          AList[i]:=AList[i]+'='+ReadString('Display');
          CloseKey;
        end;
    finally
      Free;
    end;
end;

function GetTimeZone(AKeyName: string; out AData: TTimeZoneInformation; AYear: Word = 0): string;
var
  s: string;
  rtz: TRegistryTimeZoneInfo;
begin
  Result:='';
  ResetMemory(AData,SizeOf(AData));
  if AYear=0 then
    AYear:=YearOf(Date);
  with TRegistry.Create do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if not OpenKeyReadOnly(Format('SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones\%s',[AKeyName])) then
        Exit;
      Result:=ReadString('Display');
      s:=ReadString('Std');
      StringToWideChar(s,@(AData.StandardName),Length(s)+1);
      s:=ReadString('Dlt');
      StringToWideChar(s,@(AData.DaylightName),Length(s)+1);
      ReadBinaryData('TZI',rtz,SizeOf(rtz));
      AData.Bias:=rtz.Bias;
      AData.StandardDate:=rtz.StandardDate;
      AData.StandardBias:=rtz.StandardBias;
      AData.DaylightDate:=rtz.DaylightDate;
      AData.DaylightBias:=rtz.DaylightBias;
      CloseKey;
      if not OpenKeyReadOnly(Format('SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones\%s\Dynamic DST',[AKeyName])) then
        Exit;
      if not ValueExists(IntToStr(AYear)) then
        Exit;
      ReadBinaryData(IntToStr(AYear),rtz,SizeOf(rtz));
      AData.Bias:=rtz.Bias;
      AData.StandardDate:=rtz.StandardDate;
      AData.StandardBias:=rtz.StandardBias;
      AData.DaylightDate:=rtz.DaylightDate;
      AData.DaylightBias:=rtz.DaylightBias;
      CloseKey;
    finally
      Free;
    end;
end;

function LocalDateTimeToTZ(AValue: TDateTime; ATZ: TTimeZoneInformation): TDateTime;
var
  sLocal,sUTC: TSystemTime;
begin
  Result:=0;
  AValue:=LocalDatetimeToUTC(AValue);
  if Assigned(SystemTimeToTzSpecificLocalTime) then begin
    DateTimeToSystemTime(AValue,sUTC);
    if SystemTimeToTzSpecificLocalTime(@ATZ,sUTC,sLocal) then
      Result:=SystemTimeToDateTime(sLocal);
  end;
end;

function IncWorkDays(ADT: TDateTime; ADays: Cardinal): TDateTime;
var
  i: Cardinal;
begin
  Result:=ADT;
  i:=0;
  while i<ADays do begin
    Result:=Result+1;
    if DayOfWeek(Result) in [2..6] then
      Inc(i);
  end;
end;

function DecWorkDays(ADT: TDateTime; ADays: Cardinal): TDateTime;
var
  i: Cardinal;
begin
  Result:=ADT;
  i:=0;
  while i<ADays do begin
    Result:=Result-1;
    if DayOfWeek(Result) in [2..6] then
      Inc(i);
  end;
end;

function FileTimeToDateTimeStr(FileTime: TFileTime): string;
begin
  Result:=DateTimeToStr(FileTimeToDateTime(FileTime));
end;

function FileTimeToDateTime(FT: FILETIME; ConvertTimeToLocal: Boolean = False): TDateTime;
var
  SysFTime: TSystemTime;
begin
  try
    FileTimeToSystemTime(FT,SysFTime);
    Result:=SystemTimeTodateTime(SysFTime);
    if ConvertTimeToLocal then
      Result:=UTCToSystemTime(Result);
  except
    Result:=0;
  end;
end;

function ParseDate(YYYYMMDD: string): TDatetime;
var
  y,m,d: Word;
begin
  y:=StrToInt(Copy(YYYYMMDD,1,4));
  m:=StrToInt(Copy(YYYYMMDD,5,2));
  d:=StrToInt(Copy(YYYYMMDD,7,2));
  Result:=EncodeDate(y,m,d);
end;

function UTCToDateTime(UTC: Cardinal; ConvertTimeToLocal: Boolean = False): TDateTime;
var
  d: LARGE_INTEGER;
  ft: FILETIME;
begin
  d.QuadPart:=365*24*60*60;
  d.QuadPart:=((1970-1601)*d.QuadPart+UTC+89*24*60*60+3600)*10000000;
  ft.dwLowDateTime:=d.LowPart;
  ft.dwHighDateTime:=d.HighPart;
  Result:=FiletimeToDateTime(ft,ConvertTimeToLocal);
end;

function UTCToDateTimeEx(UTC: int64; CorrectToLocal: Boolean = True): TDateTime;
var
  d: LARGE_INTEGER;
  ft: FILETIME;
begin
  d.QuadPart:=365*24*60*60;
  d.QuadPart:=((1970-1601)*d.QuadPart+89*24*60*60)*10000000+UTC*10;
  ft.dwLowDateTime:=d.LowPart;
  ft.dwHighDateTime:=d.HighPart;
  try
    Result:=FiletimeToDateTime(ft,CorrectToLocal);
    if YearOf(Result)>YearOf(Now) then
      Result:=IncYear(Result,-1970+1601);
  except
    Result:=0;
  end;
end;


function Int64ToDateTime(UTC: Int64; ConvertTimeToLocal: Boolean = False): TDateTime;
var
  d: LARGE_INTEGER;
  ft: FILETIME;
begin
  d.QuadPart:=UTC;
  ft.dwLowDateTime:=d.LowPart;
  ft.dwHighDateTime:=d.HighPart;
  Result:=FiletimeToDateTime(ft);
  if ConvertTimeToLocal then
    Result:=UTCToLocalDateTime(Result);
end;

function DSTDate2Date(dstDate: TSystemTime; year: word): TDateTime;
begin
  Result:=0;
  if dstDate.wMonth=0 then
    Exit;
  repeat
    if dstDate.wYear=0 then
      try
        Result:=EncodeDayOfWeekInMonth(year,dstDate.wMonth,dstDate.wDay,dstDate.wDayOfWeek)+
                EncodeTime(dstDate.wHour,dstDate.wMinute,dstDate.wSecond,dstDate.wMilliseconds);
      except
        Dec(dstDate.wDay);
      end
    else
      Result:=SystemTimeToDateTime(dstDate);
  until Result>0;
end;

function EasterSunday(const Year: Word): TDateTime;
var
  C, I, J, H, G, L: Integer;
  D, M: Word;
begin
  G:=Year mod 19;
  C:=Year div 100;
  H:=(C-C div 4-(8*C+13) div 25+19*G+15) mod 30;
  I:=H-(H div 28)*(1-(H div 28)*(29 div (H+1))*((21-G) div 11));
  J:=(Year+Year div 4+I+2-C+C div 4) mod 7;
  L:=I-J;
  M:=3+(L+40) div 44;
  D:=L+28-31*(M div 4);
  Result:=EncodeDate(Year,M,D);
end;

function GetWorkDays(ADT: TDatetime; ACount: Integer): Integer;
var
  sd: TDateTime;
begin
  Result:=0;
  sd:=ADT;
  repeat
    if DayOfWeek(ADT) in [2..6] then
      Inc(Result);
    if ACount<0 then
      ADT:=ADT-1
    else
      ADT:=ADT+1;
  until Result=abs(ACount);
  Result:=Round(abs(int(ADT)-int(sd)));
end;

function DateToStrDef(ADT: TDateTime; ADef: string = ''): string;
begin
  if ADT<=0 then
    Result:=ADef
  else
    Result:=DateToStr(ADT);
end;

function DateTimeToStrDef(ADT: TDateTime; ADef: string = ''; AValidDateLimit: TDatetime = 0): string;
begin
  if ADT<=AValidDateLimit then
    Result:=ADef
  else
    Result:=DateTimeToStr(ADT);
end;

function DateTimeToFileTime(dt: TDateTime): FILETIME;
var
  st: SYSTEMTIME;
begin
  DateTimeToSystemTime(dt,st);
  SystemTimeToFileTime(st,Result);
end;

function RFC822DateToDateTime(RFC822DateTime: string): TDateTime;
const
  RFC822ConvertDateTimeConvertError = '"%s" is not valid RFC822 datetime';

  DayArray: array[0..6] of string = ('Mon','Tue','Wed','Thu','Fri','Sat','Sun');
  MonthArray: array[0..11] of string = ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
  ZoneArray: array[0..14] of string = ('UT','GMT','EST','EDT','CST','CDT','MST','MDT','PST','PDT','Z','A','M','N','Y');
var
  lString: string;
  lDayName: string;
  lMonthName: string;
  I: Integer;
  lProceed: Boolean;
  lDay: Integer;
  lMonth: Integer;
  lYear: Integer;
  lTmp: Integer;
  lHours: Integer;
  lMinutes: Integer;
  lSeconds: Integer;
  lTimeZone: TTimeZoneInformation;
  lLocalStringTime: TDateTime;
  lTimeZoneName: string;
  lTimeZoneIndex: Integer;
  lLocalDiffHours: Integer;
  lLocalDiffMinutes: Integer;
  lAddLocalDiff: Boolean;
begin
  RFC822DateTime:=StringReplace(RFC822DateTime,'GTM','GMT',[rfIgnorecase]);
  lTimeZoneIndex:=-1;
  lAddLocalDiff:=False;
  lMonth:=-1;
  lTmp:=0;
  lString:=RFC822DateTime;
  lProceed:=False;
  if Pos(',',lString)>0 then
  begin
    lDayName:=Copy(lString,1,3);
    for i:=0 to Length(DayArray)-1 do begin
      if lDayName=DayArray[i] then begin
        lProceed:=True;
        Break;
      end;
    end;
    Delete(lString,1,5);
  end;
  if lProceed then begin
    if not TryStrToInt(Copy(lString,1,2), lDay) then
      lProceed:=False;
  end;
  if lProceed then begin
    lMonthName:=Copy(lString,4,3);
    lProceed:=False;
    for i:=0 to Length(MonthArray)-1 do begin
      if lMonthName=MonthArray[I] then begin
        lProceed:=True;
        lMonth:=Succ(I);
        Break;
      end;
    end;
  end;
  if lProceed then begin
    if not TryStrToInt(Copy(lString,8,4), lYear) then begin
      // might be only 2 characters long
      if not TryStrToInt(Copy(lString,8,2), lYear) then
        lProceed:=False
      else
        lTmp:=2;
    end else
      lTmp:=4;
  end;
  if lProceed then begin
    lTmp:=8+Succ(lTmp);
    if not TryStrToInt(Copy(lString,lTmp,2),lHours) then
      lProceed:=False;
  end;
  if lProceed then begin
    Inc(lTmp,3);
    if not TryStrToInt(Copy(lString, lTmp, 2), lMinutes) then
      lProceed:=False;
  end;
  if lProceed then begin
    Inc(lTmp,3);
    if not TryStrToInt(Copy(lString,lTmp,2),lSeconds) then
      // Just proceed, seconds are optional.
      lSeconds:=0;
  end;
  if lProceed then begin
    Inc(lTmp,3); // Start of TimeZone
    lTimeZoneName:=Copy(lString,lTmp,3); // e.g. "GMT"
    if (Copy(lTimeZoneName,1,1)='-') or
       (Copy(lTimeZoneName,1,1)='+') or
       (Length(lTimeZoneName) = 0) then
      // Assume UTC
      lTimeZoneIndex:=0
    else begin
      lProceed:=False;
      if Length(lTimeZoneName)=3 then begin
        for i:=0 to Length(ZoneArray)-1 do begin
          if ZoneArray[i]=lTimeZoneName then  begin
            lTimeZoneIndex:=i;
            lProceed:=True;
            Break;
          end;
        end;
      end;
      if not lProceed then begin
        // Try the ones with only 2 letters
        for i:=0 to Length(ZoneArray)-1 do begin
          if ZoneArray[i]=Copy(lTimeZoneName,1,2) then begin
            lTimeZoneIndex:=i;
            lProceed:=True;
            Break;
          end;
        end;
      end;
      if not lProceed then begin
        // Try the ones with only 1 letter
        for i:=0 to Length(ZoneArray)-1 do begin
          if ZoneArray[i]=lTimeZoneName[1] then begin
            lTimeZoneIndex:=i;
            lProceed:=True;
            Break;
          end;
        end;
      end;
      Inc(lTmp,Length(ZoneArray[lTimeZoneIndex])); // Begin of+/ -
    end;
  end;
  if lProceed then begin
    // Get local differential hours
    lAddLocalDiff:=Copy(lString,lTmp,1)='+';
    Inc(lTmp,1); // Begin of local diff hours
    if lTmp<Length(lString) then begin
      // Has local differential hours
      if not TryStrToInt(Copy(lString,lTmp,2),lLocalDiffHours) then
        lProceed:=False
    end else begin
      // No local diff time
      lLocalDiffHours:=-1;
      lLocalDiffMinutes:=-1;
    end;
  end;
  if (lProceed) and (lLocalDiffHours<>-1) then begin
    // Get local differential minutes
    Inc(lTmp,2); // Begin of local diff minutes
    if not TryStrToInt(Copy(lString,lTmp,2),lLocalDiffMinutes) then
      lProceed:=False
  end;
  if lProceed then begin
    // Create current local time of string as TDateTime
    lLocalStringTime:=EncodeDate(lYear,lMonth,lDay)+EncodeTime(lHours,lMinutes,lSeconds,0);
    case lTimeZoneIndex of
      0,1,10: lTmp:=0; // UT, GMT, Z
      2: lTmp:=5; // EST,-5
      3: lTmp:=4; // EDT,-4
      4: lTmp:=6; // CST,-6
      5: lTmp:=5; // CDT,-5
      6: lTmp:=7; // MST,-7
      7: lTmp:=6; // MDT,-6
      8: lTmp:=8; // PST,-8
      9: lTmp:=7; // PDT,-7
      11: lTmp:=1; // A,-1
      12: lTmp:=12; // M,-12
      13: lTmp:=-1; // N,+1
      14: lTmp:=-12; // Y,+12
    end;
    // Calculate the UTC-Time of the given string
    lLocalStringTime:=lLocalStringTime+(lTmp*OneHour);
    if lLocalDiffHours<>-1 then begin
      if lAddLocalDiff then
        lLocalStringTime:=lLocalStringTime-(lLocalDiffHours*OneHour)-(lLocalDiffMinutes*OneMinute)
      else
        lLocalStringTime:=lLocalStringTime+(lLocalDiffHours*OneHour)+(lLocalDiffMinutes*OneMinute);
    end;
    // Now calculate the time in local format
    if GetTimeZoneInformation(lTimeZone)=TIME_ZONE_ID_DAYLIGHT then begin
      Result:=lLocalStringTime-((lTimeZone.Bias+lTimeZone.DaylightBias)*OneMinute);
    end else begin
      Result:=lLocalStringTime-((lTimeZone.Bias+lTimeZone.StandardBias)*OneMinute);
    end;
  end else begin
    raise EConvertError.Create(Format(RFC822ConvertDateTimeConvertError,[RFC822DateTime]));
  end;
end;

function GreaterOrEquals(ANow,AThen: TDateTime): Boolean;
begin
  Result:=(CompareDateTime(ANow,AThen)<>LessThanValue);
end;

function LessOrEquals(ANow,AThen: TDateTime): Boolean;
begin
  Result:=(CompareDateTime(ANow,AThen)<>GreaterThanValue);
end;

function Less(ANow,AThen: TDateTime): Boolean;
begin
  Result:=(CompareDateTime(ANow,AThen)=LessThanValue);
end;

function Greater(ANow,AThen: TDateTime): Boolean;
begin
  Result:=(CompareDateTime(ANow,AThen)=GreaterThanValue);
end;

function IsTimeInRange(ATime: TDateTime; AStartTime, AEndTime: TDateTime; AInclusive: Boolean = True): Boolean;
var
  LTime, LStartTime, LEndTime: TDateTime;
begin
  LTime:=TimeOf(ATime);
  LStartTime:=TimeOf(AStartTime);
  LEndTime:=TimeOF(AEndTime);

  if CompareTime(LEndTime,LStartTime)=LessThanValue then
    if AInclusive then
      Result:=(CompareTime(LStartTime,LTime)<>GreaterThanValue) or (CompareTime(LTime,LEndTime)<>GreaterThanValue)
    else
      Result:=(CompareTime(LStartTime,LTime)=LessThanValue) or (CompareTime(LTime,LEndTime)=LessThanValue)
  else
    if AInclusive then
      Result:=(CompareTime(LStartTime,LTime)<>GreaterThanValue) and (CompareTime(LTime,LEndTime)<>GreaterThanValue)
    else
      Result:=(CompareTime(LStartTime,LTime)=LessThanValue) and (CompareTime(LTime,LEndTime)=LessThanValue);
end;

function IsDateTimeInRange(ADateTime: TDateTime; AStartDateTime, AEndDateTime: TDateTime; aInclusive: Boolean = True): Boolean;
begin
  if aInclusive then
    Result:=(CompareDateTime(AStartDateTime,ADateTime)<>GreaterThanValue) and (CompareDateTime(ADateTime,AEndDateTime)<>GreaterThanValue)
  else
    Result:=(CompareDateTime(AStartDateTime,ADateTime)=LessThanValue) and (CompareDateTime(ADateTime,AEndDateTime)=LessThanValue);
end;

function IsDateInRange(ADate: TDateTime; AStartDate, AEndDate: TDateTime; AInclusive: Boolean = True): Boolean;
begin
  if AInclusive then
    Result:=(CompareDate(DateOf(AStartDate),DateOf(ADate))<>GreaterThanValue) and (CompareDate(DateOf(ADate),DateOf(AEndDate))<>GreaterThanValue)
 else
    Result:=(CompareDate(DateOf(AStartDate),DateOf(ADate))=LessThanValue) and (CompareDate(DateOf(ADate),DateOf(AEndDate))=LessThanValue);
end;

function SecondsBetweenSgn(ANow,AThen: TDateTime): Integer;
begin
  Result:=CompareDatetime(AThen,ANow)*SecondsBetween(ANow,AThen);
end;

function MinutesBetweenSgn(ANow,AThen: TDateTime): Integer;
begin
  Result:=CompareDatetime(AThen,ANow)*MinutesBetween(ANow,AThen);
end;

function HoursBetweenSgn(ANow,AThen: TDateTime): Integer;
begin
  Result:=CompareDatetime(AThen,ANow)*HoursBetween(ANow,AThen);
end;

function DaysBetweenSgn(ANow,AThen: TDateTime): Integer;
begin
  Result:=CompareDatetime(AThen,ANow)*DaysBetween(ANow,AThen);
end;

function WeeksBetweenSgn(ANow,AThen: TDateTime): Integer;
begin
  Result:=CompareDatetime(AThen,ANow)*WeeksBetween(ANow,AThen);
end;

function SecondsToTime(const ASeconds: Int64): TDatetime;
const
  SecPerDay = 86400;
  SecPerHour = 3600;
  SecPerMinute = 60;
var
  ms, ss, mm, hh, dd: Cardinal;
begin
  dd:=ASeconds div SecPerDay;
  hh:=(ASeconds mod SecPerDay) div SecPerHour;
  mm:=((ASeconds mod SecPerDay) mod SecPerHour) div SecPerMinute;
  ss:=((ASeconds mod SecPerDay) mod SecPerHour) mod SecPerMinute;
  ms:=0;
  Result:=dd+EncodeTime(hh,mm,ss,ms);
end;

function FormatTicks(AValue :Int64; AWholeSecondsOnly: Boolean = True) :string;
{$IFDEF RAD9PLUSxxx}
var
  ts: TTimeSpan;
begin
  ts:=TTimeSpan.Create(AValue);
  Result:=Format('%2.2d:%2.2d:%2.2d',[ts.Hours,ts.Minutes,ts.Seconds]);
  if not AWholeSecondsOnly then
    Result:=Result+Format('.%3.3d',[ts.Milliseconds]);
  if ts.Days>0 then
    Result:=Format('%3.3d %s',[ts.Days,Result]);
{$ELSE}
const
  TicksPerMillisecond = 10000;
  TicksPerSecond = 1000 * Int64(TicksPerMillisecond);
var
  dt: TDateTime;
  ms,d: Integer;
begin
  dt:=SecondsToTime(Round(AValue/TicksPerSecond));
  d:=DaysBetween(0,dt);
  ms:=Integer((AValue div TicksPerMillisecond) mod 1000);
  Result:=FormatDateTime('hh:mm:ss',dt);
  if d>0 then
    Result:=Format('%3.3d %s',[d,Result]);
  if not AWholeSecondsOnly then
    Result:=Result+Format('.%3.3d',[ms]);
{$ENDIF}
end;

function FormatSeconds(AValue :Int64; AShort: Boolean = True) :string;
{$IFDEF RAD9PLUSxxx}
var
  ts: TTimeSpan;
begin
  ts:=TTimeSpan.Create(0,0,0,AValue);
  if AShort and (ts.Days=0) then
    Result:=Format('%2.2d:%2.2d:%2.2d',[ts.Hours,ts.Minutes,ts.Seconds])
  else
    Result:=Format('%3.3d %2.2d:%2.2d:%2.2d',[ts.Days,ts.Hours,ts.Minutes,ts.Seconds]);
{$ELSE}
var
  dt: TDateTime;
  d: integer;
begin
  dt:=SecondsToTime(AValue);
  d:=DaysBetween(0,dt);
  if AShort and (d=0) then
    Result:=FormatDateTime('hh:mm:ss',dt)
  else
    Result:=Format('%3.3d %s',[d,FormatDateTime('hh:mm:ss',dt)]);
{$ENDIF}
end;

function FormatMilliseconds(AValue :Int64; AWholeSecondsOnly: Boolean = True; AShort: Boolean = True) :string;
{$IFDEF RAD9PLUSxxx}
var
  ts: TTimeSpan;
begin
  ts:=TTimeSpan.Create(0,0,0,0,AValue);
  if AShort and (ts.Days=0) then
    Result:=Format('%2.2d:%2.2d:%2.2d',[ts.Hours,ts.Minutes,ts.Seconds])
  else
    Result:=Format('%3.3d %2.2d:%2.2d:%2.2d',[ts.Days,ts.Hours,ts.Minutes,ts.Seconds]);
  if not AWholeSecondsOnly then
    Result:=Result+Format('.%3.3d',[ts.Milliseconds]);
{$ELSE}
var
  dt: TDateTime;
  ms,d: integer;
begin
  dt:=SecondsToTime(Round(AValue/1000));
  d:=DaysBetween(0,dt);
  ms:=AValue mod 1000;
  if AShort and (d=0) then
    Result:=FormatDateTime('hh:mm:ss',dt)
  else
    Result:=Format('%3.3d %s',[d,FormatDateTime('hh:mm:ss',dt)]);
  if not AWholeSecondsOnly then
    Result:=Result+Format('.%3.3d',[ms]);
{$ENDIF}
end;

initialization
  TzSpecificLocalTimeToSystemTime:=TTzSpecificLocalTimeToSystemTime(GetProcAddress(GetModuleHandle('Kernel32.dll'),'TzSpecificLocalTimeToSystemTime'));
  SystemTimeToTzSpecificLocalTime:=TSystemTimeToTzSpecificLocalTime(GetProcAddress(GetModuleHandle('Kernel32.dll'),'SystemTimeToTzSpecificLocalTime'));
end.
