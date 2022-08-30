{***********************************************************************}
{ TDBPlanner component                                                  }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 1999-2015                                      }
{            Email: info@tmssoftware.com                                }
{            Web: http://www.tmssoftware.com                            }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The complete     }
{ source code remains property of the author and may not be distributed,}
{ published, given or sold in any form as such. No parts of the source  }
{ code can be included in any other component or application without    }
{ written authorization of the author.                                  }
{***********************************************************************}

{$I TMSDEFS.INC}

unit PlanRecurr;

interface
uses
  Classes, Windows, SysUtils, Dialogs, PlanUtil, DateUtils;

const
  INFINITE_SPAN = 100;  

type
  TDateItem = class(TCollectionItem)
  private
    FStartTime: TDateTime;
    FEndDate: TDateTime;
  published
    property StartDate: TDateTime read FStartTime write FStartTime;
    property EndDate: TDateTime read FEndDate write FEndDate;
  end;

  TDateItems = class(TCollection)
  private
    function GetItem(Index: Integer): TDateItem;
    procedure SetItem(Index: Integer; const Value: TDateItem);
  public
    constructor Create;
    function Add: TDateItem;
    property Items[Index: Integer]: TDateItem read GetItem write SetItem; default;
  published
  end;

  TRecurrencyFrequency = (rfHourly,rfDaily,rfWeekly,rfMonthly,rfYearly,frNone);

  TIntList = class(TList)
  private
    FIndex: Integer;
    procedure SetInteger(Index: Integer; Value: Integer);
    function GetInteger(Index: Integer):Integer;
    function GetStrValue: string;
    procedure SetStrValue(const Value: string);
  public
    constructor Create;
    property Items[index: Integer]: Integer read GetInteger write SetInteger; default;
    procedure Add(Value: Integer);
    procedure Insert(Index,Value: Integer);
    procedure Delete(Index: Integer);
    property StrValue: string read GetStrValue write SetStrValue;
    function HasValue(Value: Integer): Boolean;
    property Index: Integer read FIndex write FIndex;
  end;

  TDaySet = set of byte;
  TDayArray = array[1..7] of byte;

  TRecurrencyHandler = class(TObject)
  private
    FRecurrency: string;
    FEndTime: TDateTime;
    FStartTime: TDateTime;
    FCount: Integer;
    FDates: TDateItems;
    FExDates: TDateItems;
    FFreq: TRecurrencyFrequency;
    FRCount: Integer;
    FUntil: TDateTime;
    FInterval: Integer;
    FMonthList: TIntList;
    FDayList: TIntList;
    FHourList: TIntList;
    FDaySet: TDaySet;
    FDayNum: TDayArray;
    FTimeSpan: TDateTime;
    function GetDayNum(Index: Integer): byte;
    procedure SetDayNum(Index: Integer; const Value: byte);
  protected
    function ApplyFreq(ADate: TDateTime; Freq:TRecurrencyFrequency; Interval: Integer): TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse;
    procedure Generate;
    function Compose: string;
    function IsRecurrent: Boolean;
    function NextDate(var AStartDate, AEndDate: TDateTime): Boolean;
    function RecurrentMinDate: TDateTime;
    function RecurrentMaxDate: TDateTime;
    property Days: TDaySet read FDaySet write FDaySet;
    property ExDates: TDateItems read FExDates;
    property Dates: TDateItems read FDates;
    function DatesAsStrings: TStrings;
    property DayNum[Index: Integer]: byte read GetDayNum write SetDayNum;
    property Months: TIntList read FMonthList;
    
    property Recurrency: string read FRecurrency write FRecurrency;
    property StartTime: TDateTime read FStartTime write FStartTime;
    property EndTime: TDateTime read FEndTime write FEndTime;

    property Frequency: TRecurrencyFrequency read FFreq write FFreq;

    property RepeatCount: Integer read FRCount write FRCount;
    property RepeatUntil: TDateTime read FUntil write FUntil;
    property Interval: Integer read FInterval write FInterval;

    property TimeSpan: TDateTime read FTimeSpan write FTimeSpan;
  end;

function NextDayOfWeek(ADate: TDateTime;dw: Integer): TDateTime;

function WeekDayInMonth(ADate: TDateTime): integer;

implementation

{$IFNDEF DELPHI6_LVL}
const
  HoursPerDay = 24;
  MonthsPerYear = 12;

function IncHour(AValue: TDateTime; ANumberOfHours: Integer): TDateTime;
begin
  Result := ((AValue * HoursPerDay) + ANumberOfHours) / HoursPerDay;
end;

function IncYear(AValue: TDateTime; ANumberOfYears: Integer): TDateTime;
begin
  Result := IncMonth(AValue, ANumberOfYears * MonthsPerYear);
end;
{$ENDIF}

function IntToZStr(i,l: Integer):string;
var
  Res: string;
begin
  Res := IntToStr(i);
  while Length(Res)<l do
    Res := '0' + Res;

  Result := Res;
end;

function IsoToDateTime(s: string):TDateTime;
var
  da,mo,ye,ho,mi,se: Word;
  err: Integer;
begin
  Val(Copy(s,1,4),ye,err);
  Val(Copy(s,5,2),mo,err);
  Val(Copy(s,7,2),da,err);
  Val(Copy(s,10,2),ho,err);
  Val(Copy(s,12,2),mi,err);
  Val(Copy(s,14,2),se,err);
  Result := EncodeDate(ye,mo,da) + EncodeTime(ho,mi,se,0);
end;

function DateTimeToIso(dt: TDateTime):string;
var
  da,mo,ye,ho,mi,se,ms:word;
begin
  DecodeDate(dt,ye,mo,da);
  DecodeTime(dt,ho,mi,se,ms);
  Result := IntToStr(ye) + IntToZStr(mo,2) + IntToZStr(da,2) + 'T' +
            IntToZStr(ho,2) + IntToZStr(mi,2) + IntToZStr(se,2);
end;

function VarPos(su,s: string; var vp: Integer): integer;
begin
  vp := pos(su,s);
  Result := vp;
end;

function NextDayOfWeek(ADate: TDateTime;dw: Integer): TDateTime;
var
  cdw: Integer;
begin
  cdw := DayOfWeek(ADate);
  cdw := dw - cdw;
  while (cdw < 1) do inc(cdw,7);
  Result := ADate + cdw;
end;

function WeekDayInMonth(ADate: TDateTime): integer;
var
  dw: Word;
  da,mo,ye:word;
  dt: TDateTime;
  res: Word;
begin
  DecodeDate(ADate,ye,mo,da);
  dt := EncodeDate(ye,mo,1);
  dw := DayOfWeek(ADate);

  res := 0;

  while (Int(dt) <= int(ADate)) do
  begin
    if dw = DayOfWeek(dt) then
      inc(res);
    dt := dt + 1;
  end;

  Result := res;
end;


{ TRecurrencyHandler }

function TRecurrencyHandler.ApplyFreq(ADate: TDateTime;
  Freq: TRecurrencyFrequency; Interval: Integer): TDateTime;

var
  da,mo,ye,mi,ho,se,se1,da2,mo2,ye2,dim: word;
  flg,wkbnd: boolean;

begin
  Result := ADate;

  flg := false;
  // special case, 5th weekday in month not occurring in every month
  wkbnd := (FDayNum[DayOfWeek(ADate)] = 5);

  if (FHourList.Count > 0) then
  begin
    DecodeTime(ADate,ho,mi,se,se1);

    if (FHourList.Index < FHourList.Count) then
    begin
      ho := FHourList.Items[FHourList.Index];
      Result := EncodeTime(ho,mi,se,se1) + Int(ADate);

      FHourList.Index := FHourList.Index + 1;
      if FHourList.Index <= FHourList.Count then
        Exit;
    end
    else
    begin
      FHourList.Index := 0;
      ho := FHourList.Items[FHourList.Index];
      ADate := EncodeTime(ho,mi,se,se1) + Int(ADate);
      FHourList.Index := FHourList.Index + 1;
    end;
  end;

  // create set of allowed days .. & select from set

  if (FDayList.Count > 0) then
  begin
    DecodeDate(ADate,ye,mo,da);

    mo2 := mo;
    while (mo = mo2) or (Freq in [rfWeekly, rfDaily]) do
    begin
      ADate := ADate + 1;

      DecodeDate(ADate,ye2,mo2,da);
      // date is in set of allowed days
      if (DayOfWeek(ADate) in FDaySet) and ((mo = mo2) or (Freq in [rfWeekly,rfDaily])) then
      begin
        // no day of the month specifier ...
        if FDayNum[DayOfWeek(ADate)] = 0 then
        begin
          Result := ADate;
          if (Freq = rfWeekly) and (DayOfWeek(ADate) = 7) and (Interval > 1) then
          begin
            Result := Result + ((Interval - 1) * 7);
          end;
          Exit;
        end
        else
        begin
          if FDayNum[DayOfWeek(ADate)] = WeekDayInMonth(ADate) then
          begin
            Result := ADate;
            if (Freq = rfWeekly) and (Interval > 1) then
            begin
              Result := Result + ((Interval - 1) * 7);
            end;

            Exit;
          end;
        end;
      end
      else
      begin
        if (Freq = rfWeekly) and (DayOfWeek(ADate) = 7) and (Interval > 1) then
        begin
          ADate := ADate + ((Interval - 1) * 7);
        end;

        if (mo <> mo2) and not (Freq in [rfWeekly, rfDaily])  then
        begin
          ADate := EncodeDate(ye,mo,1) + Frac(ADate);
          Break;
        end;
      end;
    end;
  end;


  if (FMonthList.Count > 0) then
  begin
    DecodeDate(ADate,ye,mo,da);

    if (FMonthList.Index < FMonthList.Count) then
    begin
      mo := FMonthList.Items[FMonthList.Index];

      dim := PlanUtil.DaysInMonth(mo,ye);

      if (da <= dim) then
      begin
        if Freq = rfDaily then
        begin
          if Interval > 1 then
            ADate := ADate + Interval
          else
            ADate := ADate + 1;

          Result := ADate;

          flg := not (da = dim);
        end
        else
        begin
          FMonthList.Index := FMonthList.Index + 1;
          flg := true;

          //if FMonthList.Index <= FMonthList.Count then
          //  Exit;

          if (FDayList.Count > 0) then
          begin
            ADate := Result;
            DecodeDate(ADate,ye,mo,da);
            mo2 := mo;
            while (mo = mo2) do
            begin
              DecodeDate(ADate,ye2,mo2,da);
              // date is in set of allowed days
              if (DayOfWeek(ADate) in FDaySet) and (mo = mo2) then
              begin
                // no day of the month specifier ...
                if FDayNum[DayOfWeek(ADate)] = 0 then
                begin
                  Result := ADate;
                  if (Freq = rfWeekly) and (DayOfWeek(ADate) = 7) and (Interval > 1) then
                  begin
                    Result := Result + ((Interval - 1) * 7);
                  end;

                  Exit;
                end
                else
                begin
                  if FDayNum[DayOfWeek(ADate)] = WeekDayInMonth(ADate) then
                  begin
                    Result := ADate;
                    if (Freq = rfWeekly) and (Interval > 1) then
                      Result := Result + ((Interval - 1) * 7);
                    Exit;
                  end;
                end;
              end;

              ADate := ADate + 1;
            end;
          end;
        end;
      end
      else
      begin
        if (FMonthList.Index = FMonthList.Count - 1) and (da = 31) then
          Result := 0
        else
          Result := ADate;
      end;

      if not flg then
        FMonthList.Index := FMonthList.Index + 1;

      if FMonthList.Index <= FMonthList.Count then
        Exit;
    end
    else
    begin
      FMonthList.Index := 0;
      mo := FMonthList.Items[FMonthList.Index];
      ADate := Encodedate(ye,mo,da) + Frac(ADate);
      FMonthList.Index := FMonthList.Index + 1;
    end;
  end;

  Result := ADate;
  case Freq of
  rfHourly: Result := IncHour(ADate, Interval);
  rfDaily: Result := ADate + Interval;
  rfWeekly: Result := ADate + 7 * Interval ;
  rfMonthly:
    begin
      ADate := IncMonth(ADate, Interval);

      DecodeDate(ADate,ye,mo,da);
      DecodeDate(StartTime,ye2,mo2,da2);
      if (da <> da2) then
      begin
        if da2 <= DaysInMonth(ADate) then
          da := da2
        else
          da := DaysInMonth(ADate);
        ADate := EncodeDate(ye,mo,da) + Frac(ADate);
      end;
      Result := ADate;
    end;
  rfYearly: Result := IncYear(ADate, Interval);
  end;

  // after change autocorrect for allowed days
  if (FDayList.Count > 0) then
  begin
    ADate := Int(Result) + Frac(ADate);

    DecodeDate(ADate,ye,mo,da);

    // result date not in allowed dates
    if (DayOfWeek(ADate) in FDaySet) then
    begin
      if FDayNum[DayOfWeek(ADate)] = 0 then
      begin
        Result := ADate;
        Exit;
      end
      else
      begin
        if FDayNum[DayOfWeek(ADate)] = WeekDayInMonth(ADate) then
        begin
          Result := ADate;
          Exit;
        end;
      end;
    end;

    mo2 := mo;
    DecodeDate(ADate,ye2,mo2,da);
    ADate := EncodeDate(ye2,mo2,1) + Frac(ADate);

    while (mo = mo2) or wkbnd do
    begin
      DecodeDate(ADate,ye2,mo2,da);
      if (DayOfWeek(ADate) in FDaySet) and ((mo = mo2) or wkbnd) then
      begin
        if FDayNum[DayOfWeek(ADate)] = 0 then
        begin
          Result := ADate;
          Exit;
        end
        else
        begin
          if FDayNum[DayOfWeek(ADate)] = WeekDayInMonth(ADate) then
          begin
            Result := ADate;
            Exit;
          end;
        end;
      end;

      ADate := ADate + 1;
    end;
  end;
end;

constructor TRecurrencyHandler.Create;
begin
  inherited Create;
  FDates := TDateItems.Create;
  FExDates := TDateItems.Create;
  FMonthList := TIntList.Create;
  FDayList := TIntList.Create;
  FHourList := TIntList.Create;
  FTimeSpan := 0;
end;

destructor TRecurrencyHandler.Destroy;
begin
  FDates.Free;
  FExDates.Free;
  FMonthList.Free;
  FDayList.Free;
  FHourList.Free;
  inherited Destroy;
end;

function TRecurrencyHandler.GetDayNum(Index: Integer): byte;
begin
  Result := FDayNum[Index];
end;

procedure TRecurrencyHandler.SetDayNum(Index: Integer; const Value: byte);
begin
  FDayNum[Index] := Value;
end;

procedure TRecurrencyHandler.Parse;
var
  rule,part,srule,command,sd,ed: string;
  vp,i,err: Integer;
begin
  // RRULE=;;#EXRULE=;;#EXDATES=

  // default recurrency values
  FFreq := frNone;
  FRCount := 0;
  FUntil := 0;
  FInterval := 1;
  FDayList.Clear;
  FMonthList.Clear;
  FHourList.Clear;
  
  for i := 1 to 7 do
    FDayNum[i] := 0;

  rule := UpperCase(Recurrency) + ';';

  srule := '';
  for i := 1 to length(rule) do
    if rule[i] <> ' ' then
      srule := srule + rule[i];

  rule := srule;

  // extract RRULE part
  if (VarPos('#',rule,vp) > 0) then
  begin
    rule := copy(rule, 1,vp - 1) + ';';
  end;

  if (pos('RRULE:',rule) > 0) then
    delete(rule,1, 6);

  while (VarPos(';',rule,vp) > 0) do
  begin
    part := copy(rule,1,vp-1);
    delete(rule,1,vp);

    //outputdebugstring(pchar(part));

    varpos('=',part,vp);
    if vp > 0 then
      command := copy(part,1,vp - 1)
    else
      command := '';

    if command = 'FREQ' then
    begin
      if pos('HOURLY',part) > 0 then
        FFreq := rfHourly
      else
        if pos('DAILY',part) > 0 then
          FFreq := rfDaily
        else
          if pos('WEEKLY',part) > 0 then
            FFreq := rfWeekly
          else
            if pos('MONTHLY',part) > 0 then
              FFreq := rfMonthly
            else
              if pos('YEARLY',part) > 0 then
                FFreq := rfYearly;
    end;

    if command = 'COUNT' then
    begin
      delete(part,1,6);
      val(part, FRCount, err);
    end;

    if command = 'UNTIL' then
    begin
      delete(part,1,6);
      FUntil := IsoToDateTime(part);
    end;

    if command = 'INTERVAL' then
    begin
      delete(part,1,9);
      val(part,FInterval,err);
      if err <> 0 then
        FInterval := 1;
    end;

    if command = 'BYDAY' then
    begin
      delete(part,1,6);
      part := trim(part);
      part := StringReplace(part,'SU','1',[rfReplaceAll]);
      part := StringReplace(part,'MO','2',[rfReplaceAll]);
      part := StringReplace(part,'TU','3',[rfReplaceAll]);
      part := StringReplace(part,'WE','4',[rfReplaceAll]);
      part := StringReplace(part,'TH','5',[rfReplaceAll]);
      part := StringReplace(part,'FR','6',[rfReplaceAll]);
      part := StringReplace(part,'SA','7',[rfReplaceAll]);

      FDayList.StrValue := part;
      FDayList.Index := 0;

      FDaySet := [];

      for i := 1 to FDayList.Count do
      begin
        case (FDayList.Items[i - 1] mod 10) of
        1: FDaySet := FDaySet + [1];
        2: FDaySet := FDaySet + [2];
        3: FDaySet := FDaySet + [3];
        4: FDaySet := FDaySet + [4];
        5: FDaySet := FDaySet + [5];
        6: FDaySet := FDaySet + [6];
        7: FDaySet := FDaySet + [7];
        end;

        FDayNum[(FDayList.Items[i - 1] mod 10)] := (FDayList.Items[i - 1] div 10);
      end;
    end;

    if command = 'BYHOUR' then
    begin
      delete(part,1,7);
      FHourList.StrValue := part;
      FHourList.Index := 0;
    end;

    if command = 'BYMONTH' then
    begin
      delete(part,1,8);
      FMonthList.StrValue := part;
      FMonthList.Index := 0;
    end;
  end;

  if (FDayList.Count > 0) and (FMonthList.Count > 0) then
    FMonthList.Index := 1;


  rule := srule;

  if (VarPos('#EXDATES:',rule,vp) > 0) then
  begin
    rule := copy(rule, vp + 9, length(rule)) + ',';

    while (VarPos(',',rule,vp) > 0) do
    begin
      part := copy(rule,1,vp-1);
      delete(rule,1,vp);

      if (VarPos('/', part, vp) > 0) then
      begin
        sd := copy(part,1, vp -1);
        ed := copy(part, vp + 1, length(part));

        with FExDates.Add do
        begin
          StartDate := IsoToDateTime(sd);
          EndDate := IsoToDateTime(ed);
        end;
      end;
    end;
  end
  else
    FExDates.Clear;
end;

procedure TRecurrencyHandler.Generate;
var
  i,j: Integer;
  tdelta,nextdate,prevdate: TDateTime;
  allday: boolean;
  sdc,edc: TDateTime;
  da,mo,ye: word;
begin
  FCount := 0;
  FDates.Clear;
  FExDates.Clear;

  with FDates.Add do
  begin
    StartDate := StartTime;
    EndDate := EndTime;
  end;

  if Recurrency = '' then
    Exit;

  FDaySet := [1,2,3,4,5,6,7];

  tdelta := EndTime - StartTime;

  Parse;

  if (FRCount = 0) and (FUntil = 0) then
  begin
    if FTimeSpan = 0 then
      FUntil := IncYear(StartTime,INFINITE_SPAN)
    else
      FUntil := FTimeSpan;
  end;

  // initialize FMonthlist.Index
  if (FMonthList.Count > 0) then
  begin
    decodedate(starttime,ye,mo,da);
    for i := 0 to FMonthList.Count - 1 do
    begin
      if mo = FMonthList.Items[i] then
      begin
        FMonthList.Index := i;
        break;
      end;
    end;
  end;

  if (FRCount > 0) then
  begin
    nextdate := StartTime;
    prevdate := StartTime;
    for i := 1 to FRCount - 1 do
    begin
      nextdate := ApplyFreq(nextdate, FFreq, FInterval);

      if (nextdate > prevdate) then
        with FDates.Add do
        begin
          StartDate := nextdate;
          EndDate := nextdate + tdelta;
          prevdate := nextdate;
        end;

      if (FTimeSpan <> 0) then
      begin
        if (FTimeSpan < nextdate) then
          break;
      end;
    end;
  end
  else
  begin
    nextdate := StartTime;
    prevdate := StartTime;

    while (nextdate < FUntil) and (nextdate <> 0) do
    begin
      nextdate := ApplyFreq(nextdate, FFreq, FInterval);

      if (int(nextdate) <= int(FUntil)) and (nextdate <> 0) then
      begin
        if (FDates.Count = 0) or ((FDates.Count > 0) and (FDates.Items[FDates.Count - 1].StartDate <> nextdate)) then

        if nextdate > prevdate then
        with FDates.Add do
        begin
          StartDate := nextdate;
          EndDate := nextdate + tdelta;
          prevdate := nextdate;
        end;
      end;
    end;
  end;

  for i := 1 to ExDates.Count  do
  begin
    j := 0;
    while j < FDates.Count do
    begin
      allday := (Frac(ExDates[i - 1].EndDate) = 0) and (Frac(ExDates[i - 1].StartDate) = 0);

      if allday then
      begin
        sdc := Int(FDates.Items[j].StartDate);
        edc := Int(FDates.Items[j].EndDate);
      end
      else
      begin
        sdc := FDates.Items[j].StartDate;
        edc := FDates.Items[j].EndDate;
      end;

      if (sdc >= ExDates[i - 1].StartDate) and
         (edc <= ExDates[i - 1].EndDate) then
      begin
        FDates.Items[j].Free;
      end
      else
        inc(j);
    end;
  end;

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar(recurrency));
  for i :=1 to FDates.Count do
  begin
    outputdebugstring(pchar(datetostr(fdates.items[i - 1].StartDate) + ':' + datetostr(fdates.items[i - 1].EndDate)));
  end;
  {$ENDIF}

end;

function TRecurrencyHandler.IsRecurrent: Boolean;
begin
  Result := FDates.Count > 1;
end;

function TRecurrencyHandler.NextDate(var AStartDate, AEndDate: TDateTime): Boolean;
begin
  if FCount < FDates.Count then
  begin
    AStartDate := FDates.Items[FCount].StartDate;
    AEndDate := FDates.Items[FCount].EndDate;
  end;
  inc(FCount);
  Result := FCount <= FDates.Count;
end;

function TRecurrencyHandler.RecurrentMaxDate: TDateTime;
begin
  if FDates.Count > 0 then
    Result := FDates[FDates.Count - 1].EndDate
  else
    Result := EndTime;
end;

function TRecurrencyHandler.RecurrentMinDate: TDateTime;
begin
  Result := StartTime;
end;

function TRecurrencyHandler.Compose: string;
var
  res,daystr,mstr,dn: string;
  i: integer;
begin
  res := 'RRULE:FREQ=';
  case Integer(FFreq) of
  0: res := res + 'HOURLY';
  1: res := res + 'DAILY';
  2: res := res + 'WEEKLY';
  3: res := res + 'MONTHLY';
  4: res := res + 'YEARLY';
  end;

  if RepeatCount > 0 then
    res := res + ';COUNT='+inttostr(RepeatCount);

  if RepeatUntil > 0 then
    res := res + ';UNTIL='+DateTimeToIso(RepeatUntil);

  if Interval > 1 then
    res := res + ';INTERVAL='+IntToStr(Interval);

  if FMonthList.Count > 0 then
  begin
    res := res + ';BYMONTH=';

    for i := 1 to FMonthList.Count do
      mstr := mstr + ',' + IntToStr(FMonthList.Items[i - 1]);
    delete(mstr,1,1);
    res := res + mstr;
  end;

  if Days <> [] then
  begin
    res := res + ';BYDAY=';

    daystr := '';

    if 2 in Days then
    begin
      if (DayNum[2] > 0) then
        dn := inttostr(DayNum[2]) else dn := '';
      if daystr = '' then daystr := daystr + dn + 'MO' else daystr := daystr + ',' + dn + 'MO';
    end;

    if 3 in Days then
    begin
      if (DayNum[3] > 0) then
        dn := inttostr(DayNum[3]) else dn := '';
      if daystr = '' then daystr := daystr + dn + 'TU' else daystr := daystr + ',' + dn + 'TU';
    end;

    if 4 in Days then
    begin
      if (DayNum[4] > 0) then
        dn := inttostr(DayNum[4]) else dn := '';

      if daystr = '' then daystr := daystr + dn + 'WE' else daystr := daystr + ',' + dn + 'WE';
    end;

    if 5 in Days then
    begin
      if (DayNum[5] > 0) then
        dn := inttostr(DayNum[5]) else dn := '';

      if daystr = '' then daystr := daystr + dn + 'TH' else daystr := daystr + ',' + dn + 'TH';
    end;

    if 6 in Days then
    begin
      if (DayNum[6] > 0) then
        dn := inttostr(DayNum[6]) else dn := '';

      if daystr = '' then daystr := daystr + dn + 'FR' else daystr := daystr + ',' + dn + 'FR';
    end;

    if 7 in Days then
    begin
      if (DayNum[7] > 0) then
        dn := inttostr(DayNum[7]) else dn := '';

      if daystr = '' then daystr := daystr + dn + 'SA' else daystr := daystr + ',' + dn + 'SA';
    end;

    if 1 in Days then
    begin
      if (DayNum[1] > 0) then
        dn := inttostr(DayNum[1]) else dn := '';

      if daystr = '' then daystr := daystr + dn + 'SU' else daystr := daystr + ',' + dn + 'SU';
    end;

    res := res + daystr;
  end;

  if FExDates.Count > 0 then
  begin
    res := res + '#EXDATES:';

    for i := 1 to FExDates.Count do
    begin
      if i = 1 then
        res := res + DateTimeToIso(FExDates.Items[i - 1].StartDate) + '/' + DateTimeToIso(FExDates.Items[i - 1].EndDate)
      else
        res := res + ',' + DateTimeToIso(FExDates.Items[i - 1].StartDate) + '/' + DateTimeToIso(FExDates.Items[i - 1].EndDate)
    end;
  end;

  Result := res;
end;

function TRecurrencyHandler.DatesAsStrings: TStrings;
var
  i: Integer;

  function WeekDayStr(ADate: TDateTime): string;
  var
    day: integer;
  begin
    day := DayOfWeek(ADate);
    Result := '';
    case day of
    1: Result := 'SU';
    2: Result := 'MO';
    3: Result := 'TU';
    4: Result := 'WE';
    5: Result := 'TH';
    6: Result := 'FR';
    7: Result := 'SA';
    end;
  end;

begin
  Result := TStringList.Create;

  for i := 1 to Dates.Count do
  begin
    Result.Add(WeekDayStr(Dates[i - 1].StartDate)+' '+ FormatDateTime('dd/mm/yyyy hh:nn',Dates[i - 1].StartDate)+ ' - ' +
      FormatDateTime('dd/mm/yyyy hh:nn',Dates[i - 1].EndDate));
  end;
end;

{ TDateItems }

function TDateItems.Add: TDateItem;
begin
  Result := TDateItem(inherited Add);
end;

constructor TDateItems.Create;
begin
  inherited Create(TDateItem);
end;

function TDateItems.GetItem(Index: Integer): TDateItem;
begin
  Result := TDateItem(inherited Items[Index]);
end;

procedure TDateItems.SetItem(Index: Integer; const Value: TDateItem);
begin
  inherited Items[Index] := Value;
end;


{ TIntList }

constructor TIntList.Create;
begin
  inherited Create;
end;

procedure TIntList.SetInteger(Index:Integer;Value:Integer);
begin
  inherited Items[Index] := Pointer(Value);
end;

function TIntList.GetInteger(Index: Integer): Integer;
begin
  Result := Integer(inherited Items[Index]);
end;

procedure TIntList.Add(Value: Integer);
begin
  inherited Add(Pointer(Value));
end;

procedure TIntList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

function TIntList.GetStrValue: string;
var
  i: integer;
begin
  for i := 1 to Count do
    if i = 1 then
      Result:= IntToStr(Items[i - 1])
    else
      Result := Result + ',' + IntToStr(Items[i - 1]);
end;

procedure TIntList.SetStrValue(const Value: string);
var
  sl:TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  sl.CommaText := Value;
  Clear;
  for i := 1 to sl.Count do
   Add(StrToInt(sl.Strings[i - 1]));
  sl.Free;
end;

procedure TIntList.Insert(Index, Value: Integer);
begin
  inherited Insert(Index, Pointer(Value));
end;


function TIntList.HasValue(Value: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Count do
  begin
    if Items[i - 1] = Value then
    begin
      Result := true;
      Break;
    end;
  end;
end;

end.
