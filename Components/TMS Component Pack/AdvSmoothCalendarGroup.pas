{**************************************************************************}
{ TAdvSmoothCalendarGroup component                                        }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2015                                                       }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvSmoothCalendarGroup;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, Graphics, Dialogs, SysUtils, Contnrs, Messages, Controls, AdvStyleIF, GDIPFill,AdvGDIP,
  AdvSmoothCalendar, Math
{$IFDEF DELPHI7_LVL}
  , DateUtils
{$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Issue with parameters passed to OnSelectMultiDate event
  // v1.1.0.0 : New : Windows 8, Office 2013 styles added
  // v1.1.0.1 : Fixed : Issue with footer visible and current day click
  // v1.1.0.2 : Fixed: Issue with Disjunct selection
  // v1.2.0.0 : New : Windows 10, Office 2016 styles added
  // v1.2.0.1 : Fixed : Issue with OnDblClick event

type
  TAdvSmoothCalGroupMonthChangedEvent = procedure(Sender: TObject; AMonth, APrevMonth: Integer) of object;

  TAdvSmoothCalendarGroup = class;

  TAdvSmoothCalGroup = class(TAdvSmoothCalendar)
  private
    FGroup: TAdvSmoothCalendarGroup;
    FOnChangeCalendarMonth: TAdvSmoothCalGroupMonthChangedEvent;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure SetFocusDate(const Value: TDateTime); override;
  published
    property OnChangeCalendarMonth: TAdvSmoothCalGroupMonthChangedEvent read FOnChangeCalendarMonth write FOnChangeCalendarMonth;
  end;

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothCalendarGroupCustomizeEvent = procedure(Sender: TObject; ACalendar: TAdvSmoothCalendar; AMonth, AYear: Integer) of object;

  TAdvSmoothCalendarGroupArrowEvent = procedure(Sender: TObject; var Allow: Boolean) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothCalendarGroup = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    ResetMultiSelect: Boolean;
    cal: TAdvSmoothCalGroup;
    FMetroStyle: Boolean;
    FStartDate, FEndDate: TDateTime;
    FBlockSelection, FBlockEvent: Boolean;
    FDisjunctArray: TDisjunctDateTimeArray;
    FUpdateCount: Integer;
    FStyle: TTMSStyle;
    FDesignTime: Boolean;
    FCalendarDisplayList: TObjectList;
    FRows: Integer;
    FColumns: Integer;
    FYear: Integer;
    FMonth: Integer;
    FFill: TGDIPFill;
    FOnCustomizeCalendar: TAdvSmoothCalendarGroupCustomizeEvent;
    FOnDateFill: TAdvSmoothCalendarDateFillEvent;
    FMultiSelect: Boolean;
    FDisjunctDaySelect: Boolean;
    FOnSelectDate: TAdvSmoothCalendarDateSelectedEvent;
    FOnSelectMultiDate: TAdvSmoothCalendarMultiDateSelectedEvent;
    FOnSelectDisjunctDate: TAdvSmoothCalendarDisjunctDateSelectedEvent;
    FOnArrowLeftClick: TAdvSmoothCalendarGroupArrowEvent;
    FOnArrowRightClick: TAdvSmoothCalendarGroupArrowEvent;
    FArrowLeft: Boolean;
    FArrowRight: Boolean;
    FDate: TDateTime;
    FOnDateHint: TAdvSmoothCalendarDateHintEvent;
    FOnDateStatusClick: TAdvSmoothCalendarStatusClickEvent;
    FOnDateText: TAdvSmoothCalendarDateTextEvent;
    FOnDateStatus: TAdvSmoothCalendarGetDateStatusEvent;
    FOnGetHeaderText: TAdvSmoothCalendarGetTextEvent;
    FOnGetFooterText: TAdvSmoothCalendarGetTextEvent;
    FShowFocus: Boolean;
    FStatusAppearance: TGDIPStatus;
    FDateAppearance: TAdvSmoothCalendarDateAppearance;
    FMaxDate: TDate;
    FMinDate: TDate;
    FHeader: TAdvSmoothCalendarHeader;
    FFooter: TAdvSmoothCalendarFooter;
    FStatusCursor: TCursor;
    FFocusColor: TColor;
    FShowCurrentDate: Boolean;
    FSingleFillSelection: Boolean;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetColumns(const Value: Integer);
    procedure SetRows(const Value: Integer);
    procedure SetMonth(const Value: Integer);
    procedure SetYear(const Value: Integer);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetDisjunctDaySelect(const Value: Boolean);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetArrowLeft(const Value: Boolean);
    procedure SetArrowRight(const Value: Boolean);
    procedure SetDate(const Value: TDateTime);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetEndDate(const Value: TDateTime);
    procedure SetStartDate(const Value: TDateTime);
    procedure SetDateAppearance(const Value: TAdvSmoothCalendarDateAppearance);
    procedure SetStatusAppearance(const Value: TGDIPStatus);
    function DoStoreMaxDate: Boolean;
    function DoStoreMinDate: Boolean;
    procedure SetMaxDate(const Value: TDate);
    procedure SetMinDate(const Value: TDate);
    procedure SetFooter(const Value: TAdvSmoothCalendarFooter);
    procedure SetHeader(const Value: TAdvSmoothCalendarHeader);
    function GetVersionNr: Integer;
    function GetVersion: String;
    procedure SetVersion(const Value: String);
    procedure SetFocusColor(const Value: TColor);
    procedure SetShowCurrentDate(const Value: Boolean);
    procedure SetSingleFillSelection(const Value: Boolean);
  protected
    procedure DoEnter; override;
    procedure AppearanceChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure Changed;
    procedure UpdateCalendars;
    procedure CreateCalendars;
    procedure ArrowLeftClick(Sender: TObject);
    procedure ArrowRightClick(Sender: TObject);
    procedure CalendarMonthChanged(Sender: TObject; AMonth, APrevMonth: Integer);
    procedure CalendarDateStatus(Sender: TObject; Date: TDateTime; var StatusMessage: String;
    Fill: TGDIPStatus; var OffsetX: integer; var OffsetY: integer);
    procedure CalendarDateStatusClick(Sender: TObject; StatusMessage: String; Date: TDateTime);
    procedure CalendarGetDateText(Sender: TObject; Date: TDateTime; AFont: TFont; var AText:String);
    procedure CalendarGetHeaderText(Sender: TObject; var AText: String);
    procedure CalendarGetFooterText(Sender: TObject; var AText: String);
    procedure CalendarDateHint(Sender: TObject; Date: TDateTime; var hint: String);
    procedure CalendarDateModeChanged(Sender: TObject; Mode, ModeTo: TAdvSmoothCalendarDateMode; var allow: Boolean);
    procedure CalendarDateFill(Sender: TObject; AFill: TGDIPFill; AFont: TFont; Date: TDateTime; DateKind: TAdvSmoothCalendarDateKind);
    procedure CalendarSelectDate(Sender: TObject; Mode: TAdvSmoothCalendarDateMode; Date: TDateTime);
    procedure CalendarSelectMultiDate(Sender: TObject; Mode: TAdvSmoothCalendarDateMode; StartDate, EndDate: TDateTime);
    procedure CalendarSelectDisjunctDate(Sender: TObject; Mode: TAdvSmoothCalendarDateMode; ADisjunctDates: TDisjunctDateTimeArray);
    procedure CalendarDblClick(Sender: TObject);
  public
    function GetDisjunctDays: TDisjunctDateTimeArray;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Resize; override;
    procedure Loaded; override;
    property Calendars: TObjectList read FCalendarDisplayList write FCalendarDisplayList;
    procedure Paint; override;
    procedure NextMonth;
    procedure PreviousMonth;
    procedure NextYear;
    procedure PreviousYear;
    procedure WholeYear(AYear: Integer = - 1);
    procedure BeginUpdate;
    procedure EndUpdate;
    property DisjunctDates: TDisjunctDateTimeArray read GetDisjunctDays;
    function GetCalendarByDate(ADate: TDate): TAdvSmoothCalendar;
    procedure SelectDisjunctDates(ADisjunctDates: array of TDateTime);
    property Date: TDateTime read FDate write SetDate;
    property StartDate: TDateTime read FStartDate write SetStartDate;
    property EndDate: TDateTime read FEndDate write SetEndDate;
  published
    property SingleFillSelection: Boolean read FSingleFillSelection write SetSingleFillSelection default True;
    property Header: TAdvSmoothCalendarHeader read FHeader write SetHeader;
    property Footer: TAdvSmoothCalendarFooter read FFooter write SetFooter;
    property ArrowLeft: Boolean read FArrowLeft write SetArrowLeft default True;
    property ArrowRight: Boolean read FArrowRight write SetArrowRight default True;
    property OnArrowLeftClick: TAdvSmoothCalendarGroupArrowEvent read FOnArrowLeftClick write FOnArrowLeftClick;
    property OnArrowRightClick: TAdvSmoothCalendarGroupArrowEvent read FOnArrowRightClick write FOnArrowRightClick;
    property OnSelectDate: TAdvSmoothCalendarDateSelectedEvent read FOnSelectDate write FOnSelectDate;
    property OnSelectMultiDate: TAdvSmoothCalendarMultiDateSelectedEvent read FOnSelectMultiDate write FOnSelectMultiDate;
    property OnSelectDisjunctDate: TAdvSmoothCalendarDisjunctDateSelectedEvent read FOnSelectDisjunctDate write FOnSelectDisjunctDate;
    property OnDateFill: TAdvSmoothCalendarDateFillEvent read FOnDateFill write FOnDateFill;
    property OnCustomizeCalendar: TAdvSmoothCalendarGroupCustomizeEvent read FOnCustomizeCalendar write FOnCustomizeCalendar;
    property Fill: TGDIPFill read FFill write SetFill;
    property Month: Integer read FMonth write SetMonth;
    property Year: Integer read FYear write SetYear;
    property Columns: Integer read FColumns write SetColumns default 3;
    property Rows: Integer read FRows write SetRows default 1;
    property TabOrder;
    property TabStop default true;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property DisjunctDaySelect: Boolean read FDisjunctDaySelect write SetDisjunctDaySelect default False;
    property Visible;
    property ShowHint;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default True;
    property ShowCurrentDate: Boolean read FShowCurrentDate write SetShowCurrentDate default true;

    property FocusColor: TColor read FFocusColor write SetFocusColor default clBlack;

    property Version: String read GetVersion write SetVersion;

    property MaxDate: TDate read FMaxDate write SetMaxDate stored DoStoreMaxDate;
    property MinDate: TDate read FMinDate write SetMinDate stored DoStoreMinDate;
    property StatusCursor: TCursor read FStatusCursor write FStatusCursor default crHandPoint;

    property DateAppearance: TAdvSmoothCalendarDateAppearance read FDateAppearance write SetDateAppearance;
    property StatusAppearance: TGDIPStatus read FStatusAppearance write SetStatusAppearance;

    property OnGetHeaderText: TAdvSmoothCalendarGetTextEvent read FOnGetHeaderText write FOnGetHeaderText;
    property OnGetFooterText: TAdvSmoothCalendarGetTextEvent read FOnGetFooterText write FOnGetFooterText;
    property OnDateHint: TAdvSmoothCalendarDateHintEvent read FOnDateHint write FOnDateHint;
    property OnGetDateText: TAdvSmoothCalendarDateTextEvent read FOnDateText write FOnDateText;
    property OnDateStatus: TAdvSmoothCalendarGetDateStatusEvent read FOnDateStatus write FOnDateStatus;
    property OnDateStatusClick: TAdvSmoothCalendarStatusClickEvent read FOnDateStatusClick write FOnDateStatusClick;

    property Align;
    property Anchors;
    property Constraints;
    property DragMode;
    property DragCursor;
    property DragKind;

    {$IFDEF DELPHI2006_LVL}
    property OnCanResize;
    property OnConstrainedResize;
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property PopupMenu;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property OnClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
  end;

implementation

{$IFNDEF DELPHI7_LVL}

const
  CS_DROPSHADOW = $00020000;

function DayOf(d: TDateTime): word;
var
  LYear, LMonth: Word;
begin
  DecodeDate(d, LYear, LMonth, Result);
end;

function MonthOf(d: TDateTime): word;
var
  LYear, LDay: Word;
begin
  DecodeDate(d, LYear, Result, LDay);
end;

function YearOf(d: TDateTime): word;
var
  LMonth, LDay: Word;
begin
  DecodeDate(d, Result, LMonth, LDay);
end;

function getWeekNumber(today: Tdatetime): integer; const
  Fiddle : array[1..7] of Byte = (6,7,8,9,10,4,5);
var
  present, startOfYear: Tdatetime;
  firstDayOfYear, weekNumber, numberOfDays: integer;
  year, month, day: word;
begin
  present:= trunc(today); //truncate to remove hours, mins and secs
  decodeDate(present, year, month, day); //decode to find year
  startOfYear:= encodeDate(year, 1, 1);  //encode 1st Jan of the year

  //find what day of week 1st Jan is, then add days according to rule
  firstDayOfYear:= Fiddle[dayOfWeek(startOfYear)];

  //calc number of days since beginning of year + additional according to rule
  numberOfDays:= trunc(present - startOfYear) + firstDayOfYear;

  //calc number of weeks
  weekNumber:= trunc(numberOfDays / 7);

  //create datecode string
  result := weekNumber;

  if weekNumber = 0 then //recursive call for year begin/end...
    //see if previous year end was week 52 or 53
    result := getWeekNumber(encodeDate(year - 1, 12, 31))
  else
    if weekNumber = 53 then
    //if 31st December less than Thursday then must be week 01 of next year
    if dayOfWeek(encodeDate(year, 12, 31)) < 5 then
    begin
      result:= 1;
    end;
end;

function WeekOf(d: TDateTime): word;
begin
  Result := getWeekNumber(d);
end;

function CompareDateTime(d1,d2: TDateTime): integer; begin  if Abs(d1 - d2) < (1 / (60 * 24 * 60 * 1000)) then
    Result := 0
  else if d1 < d2 then
    Result := -1
  else
    Result := +1;
end;

function CompareDate(d1,d2: TDateTime): integer; begin
  if Trunc(d1) = Trunc(d2) then
    Result := 0
  else if d1 < d2 then
    Result := -1
  else
    Result := 1;
end;

function DaysInAMonth(const AYear, AMonth: Word): Word;
begin
  Result := MonthDays[(AMonth = 2) and IsLeapYear(AYear), AMonth];
end;

function DaysInMonth(d: TDateTime): word; var
  LYear, LMonth, LDay: Word;
begin
  DecodeDate(d, LYear, LMonth, LDay);
  Result := DaysInAMonth(LYear, LMonth);
end;

function DaysPerWeek: word;
begin
  Result := 7;
end;
{$ENDIF}

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

{ TAdvSmoothCalendarGroup }

procedure TAdvSmoothCalendarGroup.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothCalendarGroup.ArrowLeftClick(Sender: TObject);
var
  allow: Boolean;
begin
  allow := True;
  if Assigned(OnArrowLeftClick) then
    OnArrowLeftClick(Self, allow);

  if Allow then
    PreviousMonth;
end;

procedure TAdvSmoothCalendarGroup.ArrowRightClick(Sender: TObject);
var
  allow: Boolean;
begin
  allow := True;
  if Assigned(OnArrowRightClick) then
    OnArrowRightClick(Self, allow);

  if Allow then
    NextMonth;
end;

procedure TAdvSmoothCalendarGroup.Assign(Source: TPersistent);
begin
  inherited;

end;

procedure TAdvSmoothCalendarGroup.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TAdvSmoothCalendarGroup.CalendarDateFill(Sender: TObject;
  AFill: TGDIPFill; AFont: TFont; Date: TDateTime;
  DateKind: TAdvSmoothCalendarDateKind);
begin
  if Assigned(OnDateFill) then
    OnDateFill(Sender, AFill, AFont,Date, DateKind);
end;

procedure TAdvSmoothCalendarGroup.CalendarDateHint(Sender: TObject;
  Date: TDateTime; var hint: String);
begin
  if Assigned(OnDateHint) then
    OnDateHint(Sender, Date, Hint);
end;

procedure TAdvSmoothCalendarGroup.CalendarDateModeChanged(Sender: TObject; Mode,
  ModeTo: TAdvSmoothCalendarDateMode; var allow: Boolean);
begin
  Allow := False;
end;

procedure TAdvSmoothCalendarGroup.CalendarDateStatus(Sender: TObject;
  Date: TDateTime; var StatusMessage: String; Fill: TGDIPStatus; var OffsetX,
  OffsetY: integer);
begin
  if Assigned(OnDateStatus) then
    OnDateStatus(Sender, Date, StatusMessage, Fill, OffsetX, OffsetY);
end;

procedure TAdvSmoothCalendarGroup.CalendarDateStatusClick(Sender: TObject;
  StatusMessage: String; Date: TDateTime);
begin
  if Assigned(OnDateStatusClick) then
    OnDateStatusClick(Sender, StatusMessage, Date);
end;

procedure TAdvSmoothCalendarGroup.CalendarDblClick(Sender: TObject);
begin
  if Assigned(OnDblClick) then
    OnDblClick(Self);
end;

procedure TAdvSmoothCalendarGroup.CalendarGetDateText(Sender: TObject;
  Date: TDateTime; AFont: TFont; var AText: String);
begin
  if Assigned(OnGetDateText) then
    OnGetDateText(Sender, Date, AFont, AText);
end;

procedure TAdvSmoothCalendarGroup.CalendarGetFooterText(Sender: TObject;
  var AText: String);
begin
  if Assigned(OnGetFooterText) then
    OnGetFooterText(Sender, AText);
end;

procedure TAdvSmoothCalendarGroup.CalendarGetHeaderText(Sender: TObject;
  var AText: String);
begin
  if Assigned(OnGetHeaderText) then
    OnGetHeaderText(Sender, AText);
end;

procedure TAdvSmoothCalendarGroup.CalendarMonthChanged(Sender: TObject;
  AMonth, APrevMonth: Integer);
var
  cl: TAdvSmoothCalGroup;
var
  I: Integer;
begin
  if FBlockSelection then
    Exit;

  FBlockSelection := True;
  for I := 0 to FCalendarDisplayList.Count - 1 do
  begin
    cl := TAdvSmoothCalGroup(FCalendarDisplayList[I]);
    if Assigned(cl) and (cl.Month = AMonth) then
    begin
      if AMonth > APrevMonth then
        cl.FocusDate := EncodeDate(cl.Year, cl.Month, 1)
      else
        cl.FocusDate := EncodeDate(cl.Year, cl.Month, DaysInMonth(cl.MinDate));

      cl.SetFocus;
    end;
  end;
  FBlockSelection := False;
end;

procedure TAdvSmoothCalendarGroup.CalendarSelectDate(Sender: TObject;
  Mode: TAdvSmoothCalendarDateMode; Date: TDateTime);
var
  I: Integer;
  cl: TAdvSmoothCalGroup;
begin
  if FBlockSelection then
    Exit;

  FDate := Date;
  (Sender as TAdvSmoothCalGroup).FocusDate := Date;
  (Sender as TAdvSmoothCalGroup).Repaint;
  cal := (Sender as TAdvSmoothCalGroup);

  FBlockSelection := True;
  FStartDate := FDate;
  FEndDate := FDate;
  DisjunctDates.Clear;

  for I := 0 to FCalendarDisplayList.Count - 1 do
  begin
    cl := TAdvSmoothCalGroup(FCalendarDisplayList[I]);
    if Assigned(cl) then
    begin
      if (cl <> Sender) then
      begin
        cl.SelectedDateRange := drSingledate;
        cl.FocusDate := -1;
        cl.Selectedd := -1;
        cl.HoveredDate := -1;
        cl.Startd := -1;
        cl.Stopd := -1;
        cl.DisjunctDates.Clear;
        cl.Invalidate;
      end;
    end;
  end;

  if not FBlockEvent then
  begin
    if Assigned(OnSelectDate) then
      OnSelectDate(Self, Mode, Date);
  end;
  FBlockSelection := False;
end;

procedure TAdvSmoothCalendarGroup.CalendarSelectDisjunctDate(Sender: TObject;
  Mode: TAdvSmoothCalendarDateMode; ADisjunctDates: TDisjunctDateTimeArray);
var
  I: Integer;
  d: TDate;
  dt: TDisjunctDateTimeItem;
begin

  if DisjunctDates.Count = 0 then
  begin
    dt := DisjunctDates.Add;
    dt.DateTime := Date;
    dt.Calendar := (Sender as TAdvSmoothCalendar);
  end;

  (Sender as TAdvSmoothCalGroup).FocusDate := (Sender as TAdvSmoothCalGroup).StartDate;
  (Sender as TAdvSmoothCalGroup).Repaint;

  for I := DisjunctDates.Count - 1 downto 0 do
  begin
    if DisjunctDates[I].Calendar = Sender then
      DisjunctDates.Delete(I);
  end;

  for I := 0 to ADisjunctDates.Count - 1 do
  begin
    d := ADisjunctDates[i].DateTime;
    dt := FDisjunctArray.Add;
    dt.DateTime := d;
    dt.Calendar := Sender as TAdvSmoothCalendar;
  end;

  if Assigned(OnSelectDisjunctDate) then
    OnSelectDisjunctDate(Sender, Mode, DisjunctDates);
end;

procedure TAdvSmoothCalendarGroup.CalendarSelectMultiDate(Sender: TObject;
  Mode: TAdvSmoothCalendarDateMode; StartDate, EndDate: TDateTime);
var
  i, it, K: Integer;
  cals: TAdvSmoothCalGroup;
  cl: TAdvSmoothCalGroup;
begin
  FBlockSelection := True;

  for I := 0 to FCalendarDisplayList.Count - 1 do
  begin
    cl := TAdvSmoothCalGroup(FCalendarDisplayList[I]);
    if Assigned(cl) and (cl <> Sender) and (cl <> cal) then
    begin
      cl.DisjunctDates.Clear;
      cl.SelectedDateRange := drSingledate;
      cl.startd := -1;
      cl.stopd := -1;

      cl.Invalidate;
    end;
  end;

  cals := (Sender as TAdvSmoothCalGroup);
  if (cals <> cal) then
  begin
    i := FCalendarDisplayList.IndexOf(cals);
    it := FCalendarDisplayList.IndexOf(cal);
    if (i >= 0) and  (i <= FCalendarDisplayList.Count - 1) and (it >= 0) and (it <= FCalendarDisplayList.Count - 1) then
    begin
      if i < it then
      begin
        for K := i to it do
        begin
          cl := TAdvSmoothCalGroup(FCalendarDisplayList[K]);
          if Assigned(cl) then
          begin
            cl.DisjunctDates.Clear;
            cl.SelectedDateRange := drMultiDates;

            if K < it then
              cl.Stopd := cl.MaxDate
            else
            begin
              if CompareDateTime(cl.StopD, cl.startd) < 0 then
                cl.Stopd := cl.Startd;
            end;

            if K > i then
              cl.Startd := cl.MinDate
            else
              cl.Startd := StartDate;

            cl.FocusDate := cl.StartDate;

            if K = I then
              FStartDate := cl.Startd
            else if K = it then
              FEndDate := cl.EndDate;

            cl.Invalidate;
          end;
        end;
      end
      else
      begin
        for K := it to i do
        begin
          cl := TAdvSmoothCalGroup(FCalendarDisplayList[K]);
          if Assigned(cl) then
          begin
            cl.DisjunctDates.Clear;
            cl.SelectedDateRange := drMultiDates;

            if K > it then
              cl.startd := cl.MinDate
            else
            begin
              if CompareDateTime(cl.stopd, cl.startd) < 0 then
                cl.stopd := cl.startd;
            end;

            if K < i then
              cl.stopd := cl.MaxDate
            else
              cl.stopd := EndDate;

            cl.FocusDate := cl.EndDate;

            if K = it then
              FStartDate := cl.Startd
            else if K = i then
              FEndDate := cl.EndDate;

            cl.Invalidate;
          end;
        end;
      end;
    end;
  end
  else
  begin
    FStartDate := StartDate;
    FEndDate := EndDate;
  end;

  if Assigned(OnSelectMultiDate) then
    OnSelectMultiDate(Sender, Mode, FStartDate, FEndDate);

  FBlockSelection := False;
end;

procedure TAdvSmoothCalendarGroup.Changed;
begin
  CreateCalendars;
end;

constructor TAdvSmoothCalendarGroup.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  DoubleBuffered := True;

  FHeader := TAdvSmoothCalendarHeader.Create(nil);
  FFooter := TAdvSmoothCalendarFooter.Create(nil);
  FDateAppearance := TAdvSmoothCalendarDateAppearance.Create(nil);
  FStatusAppearance := TGDIPStatus.Create;

  FSingleFillSelection := True;

  if FDesigntime then
  begin
    FFooter.Visible := False;
    FStatusAppearance.Fill.Color := clRed;
    FStatusAppearance.Fill.GradientType := gtSolid;
    FStatusAppearance.Fill.BorderColor := clGray;
    FStatusAppearance.Font.Color := clWhite;
  end;

  TabStop := True;
  FShowFocus := True;
  FShowCurrentDate := True;

  FFocusColor := clBlack;
  FStatusCursor := crHandPoint;

  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;

  FMinDate := 0;
  FMaxDate := 0;
  FStartDate := -1;
  FEndDate := -1;
  FYear := YearOf(Now);
  FMonth := MonthOf(Now);
  FColumns := 3;
  FRows := 1;
  FArrowLeft := True;
  FArrowRight := True;
  FDisjunctDaySelect := False;
  FMultiSelect := False;

  FDisjunctArray := TDisjunctDateTimeArray.Create(TDisjunctDateTimeItem);

  Width := 530;
  Height := 200;

  FCalendarDisplayList := TObjectList.Create;
  FStyle := tsOffice2010Blue;

  CreateCalendars;

  if FDesignTime then
    SetComponentStyle(tsOffice2010Blue);

  FHeader.OnChange := AppearanceChanged;
  FFooter.OnChange := AppearanceChanged;
  FDateAppearance.OnChange := AppearanceChanged;
  FStatusAppearance.OnChange := AppearanceChanged;
end;

procedure TAdvSmoothCalendarGroup.CreateCalendars;
var
  c, r, j: Integer;
  cm, cy: Integer;
  cl: TAdvSmoothCalGroup;
begin
  FCalendarDisplayList.Clear;
  if (Columns <= 0) and (Rows <= 0) then
    Exit;

  if FUpdateCount > 0 then
    Exit;

  cm := Month;
  cy := Year;

  for r := 0 to Rows - 1 do
  begin
    for c := 0 to Columns - 1 do
    begin
      cl := TAdvSmoothCalGroup.Create(Self);
      cl.FocusColor := FocusColor;
      cl.Transparent := True;
      cl.FGroup := Self;
      cl.SingleFillSelection := SingleFillSelection;
      cl.StatusAppearance.Assign(StatusAppearance);
      cl.DateAppearance.Assign(DateAppearance);
      cl.Header.Assign(Header);
      cl.Footer.Assign(Footer);
      cl.StatusCursor := StatusCursor;
      cl.ShowHint := ShowHint;
      cl.Header.UpDownVisible := False;
      cl.TabStop := TabStop;
      cl.ArrowLeftClick := ArrowLeftClick;
      cl.ArrowRightClick := ArrowRightClick;
      cl.DisableInteraction := True;
      cl.KeyBoardDateModeToggle := False;
      cl.OnSelectDisjunctDate := CalendarSelectDisjunctDate;
      cl.DisjunctDaySelect := DisjunctDaySelect;
      cl.OnChangeCalendarMonth := CalendarMonthChanged;
      cl.Footer.Visible := False;
      cl.OnDateModeChanged := CalendarDateModeChanged;
      cl.OnDateHint := CalendarDateHint;
      cl.OnDateStatus := CalendarDateStatus;
      cl.OnDateStatusClick := CalendarDateStatusClick;
      cl.OnGetDateText := CalendarGetDateText;
      cl.OnGetHeaderText := CalendarGetHeaderText;
      cl.OnGetFooterText := CalendarGetFooterText;
      cl.OnDateFill := CalendarDateFill;
      cl.OnSelectDate := CalendarSelectDate;
      cl.OnSelectMultiDate := CalendarSelectMultiDate;
      cl.OnDblClick := CalendarDblClick;
      cl.Fill.BorderColor := clNone;
      cl.ShowCurrentDate := ShowCurrentDate;
      cl.Header.ArrowsVisible := True;
      cl.ArrowLeftVisible := False;
      cl.ArrowRightVisible := False;
      cl.AllowToggle := False;
      cl.MultiSelect := MultiSelect;
      cl.ShowFocus := ShowFocus;
      if (r = 0) then
      begin
        if c = 0 then
          cl.ArrowLeftVisible := ArrowLeft;
        if c = Columns - 1 then
          cl.ArrowRightVisible := ArrowRight;
      end;

      cl.Month := cm;
      cl.OriginalMonth := cm;
      cl.Year := cy;
      cl.Parent := Self;
      cl.MinDate := EncodeDate(cy, cm, 1);
      cl.MaxDate := EncodeDate(cy, cm, DaysInMonth(cl.MinDate));
      cl.FocusDate := cl.MinDate;
      if (MinDate <> 0) and (MaxDate <> 0) then
      begin
        cl.MinDate := Max(Mindate, cl.MinDate);
        cl.MaxDate := Min(MaxDate, cl.MaxDate);
      end;

      if (MultiSelect and DisjunctDaySelect) and (DisjunctDates.Count > 0) then
      begin
        for J := 0 to DisjunctDates.Count - 1 do
          if (CompareDate(DisjunctDates[J].DateTime, cl.MinDate) >= 0) and (CompareDate(DisjunctDates[J].DateTime, cl.MaxDate) <= 0) then
            cl.DisjunctDates.Add.DateTime := DisjunctDates[J].DateTime;

        cl.SelectedDateRange := drSingledate;
      end
      else if MultiSelect then
      begin
        cl.Startd := StartDate;
        cl.Stopd := EndDate;

        if cl.Startd <> cl.Stopd then
          cl.SelectedDateRange := drMultiDates
        else
          cl.SelectedDateRange := drSingledate;
      end;

      cl.Selectedd := Date;


      FCalendarDisplayList.Add(cl);

      Inc(cm);
      if cm > 12 then
      begin
        cm := 1;
        Inc(cy);
      end;
    end;
  end;

  UpdateCalendars;
end;

destructor TAdvSmoothCalendarGroup.Destroy;
begin
  FHeader.Free;
  FFooter.Free;
  FDateAppearance.Free;
  FStatusAppearance.Free;
  FDisjunctArray.Free;
  FFill.Free;
  FCalendarDisplayList.Free;
  inherited;
end;

procedure TAdvSmoothCalendarGroup.DoEnter;
begin
  inherited;
//  if FCalendarDisplayList.Count > 0 then
//    TAdvSmoothCalGroup(FCalendarDisplayList[0]).SetFocus;
end;

function TAdvSmoothCalendarGroup.DoStoreMaxDate: Boolean;
begin
  Result := FMaxDate <> 0.0;
end;

function TAdvSmoothCalendarGroup.DoStoreMinDate: Boolean;
begin
  Result := FMinDate <> 0.0;
end;

function TAdvSmoothCalendarGroup.GetCalendarByDate(
  ADate: TDate): TAdvSmoothCalendar;
var
  I: Integer;
  cl: TAdvSmoothCalGroup;
begin
  Result := nil;
  for I := 0 to FCalendarDisplayList.Count - 1 do
  begin
    cl := TAdvSmoothCalGroup(FCalendarDisplayList[I]);
    if (CompareDate(ADate, cl.MinDate) >= 0) and (CompareDate(ADate, cl.MaxDate) <= 0) then
    begin
      Result := cl;
      Break;
    end;
  end;
end;

function TAdvSmoothCalendarGroup.GetDisjunctDays: TDisjunctDateTimeArray;
begin
  Result := FDisjunctArray;
end;

function TAdvSmoothCalendarGroup.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothCalendarGroup.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothCalendarGroup.Loaded;
begin
  inherited;
  CreateCalendars;
end;

procedure TAdvSmoothCalendarGroup.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed;
end;

procedure TAdvSmoothCalendarGroup.FillChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvSmoothCalendarGroup.NextMonth;
var
  m: Integer;
begin
  BeginUpdate;
  m := Month;
  Inc(m);
  if m > 12 then
  begin
    m := 1;
    Year := Year + 1;
  end;
  Month := m;
  EndUpdate;
end;

procedure TAdvSmoothCalendarGroup.NextYear;
begin
  Year := Year + 1;
end;

procedure TAdvSmoothCalendarGroup.Paint;
var
  g: TGPGraphics;
begin
  inherited;
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  Fill.Fill(g, MakeRect(0, 0, Width - 1, Height - 1));
  g.Free;
end;

procedure TAdvSmoothCalendarGroup.PreviousMonth;
var
  m: Integer;
begin
  BeginUpdate;
  m := Month;
  Dec(m);
  if m < 1 then
  begin
    m := 12;
    Year := Year - 1;
  end;

  Month := m;
  EndUpdate;
end;

procedure TAdvSmoothCalendarGroup.PreviousYear;
begin
  Year := Year - 1;
end;

procedure TAdvSmoothCalendarGroup.Resize;
begin
  inherited;
  UpdateCalendars;
end;

procedure TAdvSmoothCalendarGroup.SelectDisjunctDates(
  ADisjunctDates: array of TDateTime);
var
  I: Integer;
  d: TDate;
  dt: TDisjunctDateTimeItem;
begin
  if MultiSelect and DisjunctDaySelect then
  begin
    FDate := -1;
    FDisjunctArray.Clear;
    for I := 0 to Length(ADisjunctDates) - 1 do
    begin
      d := int(ADisjunctDates[i]);
      dt := FDisjunctArray.Add;
      dt.DateTime := d;
      dt.Calendar := GetCalendarByDate(d);
    end;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetArrowLeft(const Value: Boolean);
begin
  FArrowLeft := Value;
  Changed;
end;

procedure TAdvSmoothCalendarGroup.SetArrowRight(const Value: Boolean);
begin
  FArrowRight := Value;
  Changed;
end;

procedure TAdvSmoothCalendarGroup.SetColorTones(ATones: TColorTones);
begin
  FMetroStyle := True;
  Fill.Color := ATones.Background.BrushColor;
  Fill.ColorTo := ATones.Background.BrushColor;
  Fill.BorderColor := ATones.Background.BorderColor;

  Header.Fill.Color := ATones.Selected.BrushColor;
  Header.Fill.ColorTo := ATones.Selected.BrushColor;
  Header.Font.Color := ATones.Selected.TextColor;
  Header.Fill.BorderColor := clNone;
  Header.ArrowColor := ATones.Selected.TextColor;

  Footer.Fill.Color := ATones.Selected.BrushColor;
  Footer.Fill.ColorTo := ATones.Selected.BrushColor;
  Footer.Font.Color := ATones.Selected.TextColor;
  Footer.Fill.BorderColor := ATones.Selected.BorderColor;

  DateAppearance.DayOfWeekFill.Color := ATones.Selected.BrushColor;
  DateAppearance.DayOfWeekFill.ColorTo := ATones.Selected.BrushColor;
  DateAppearance.DayOfWeekFill.BorderColor := clNone;
  DateAppearance.DayOfWeekFill.GradientType := gtSolid;
  DateAppearance.DayOfWeekFill.GradientMirrorType := gtNone;
  DateAppearance.DayOfWeekFill.ColorTo := clNone;
  DateAppearance.DayOfWeekFill.ColorMirror := clNone;
  DateAppearance.DayOfWeekFill.ColorMirrorTo := clNone;
  DateAppearance.DayOfWeekFont.Color := ATones.Selected.TextColor;
  DateAppearance.DayOfWeekFont.Name := GetMetroFont;

  DateAppearance.DateFill.Color := ATones.Background.BrushColor;
  DateAppearance.DateFill.ColorTo := ATones.Background.BrushColor;
  DateAppearance.DateFill.ColorMirror := ATones.Background.BrushColor;
  DateAppearance.DateFill.ColorMirrorTo := ATones.Background.BrushColor;
  DateAppearance.DateFill.BorderColor := ATones.BackGround.BorderColor;
  DateAppearance.DateFill.GradientType := gtSolid;
  DateAppearance.DateFill.GradientMirrorType := gtNone;
  DateAppearance.DateFill.ColorTo := clNone;
  DateAppearance.DateFill.ColorMirror := clNone;
  DateAppearance.DateFill.ColorMirrorTo := clNone;
  DateAppearancE.DateFont.Color := ATones.Background.TextColor;
  DateAppearancE.DateFont.Name := GetMetroFont;

  DateAppearance.MonthDateFont.Color := ATones.Background.TextColor;
  DateAppearance.YearDateFont.Color := ATones.Background.TextColor;
  DateAppearance.YearDateFont.Name := GetMetroFont;

  DateAppearance.WeekendFill.Color := ATones.Background.BrushColor;
  DateAppearance.WeekendFill.ColorTo := ATones.Background.BrushColor;
  DateAppearance.WeekendFill.ColorMirror := ATones.Background.BrushColor;
  DateAppearance.WeekendFill.ColorMirrorTo := ATones.Background.BrushColor;
  DateAppearance.WeekendFill.BorderColor := ATones.Background.BorderColor;
  DateAppearance.WeekendFill.GradientType := gtSolid;
  DateAppearance.WeekendFill.ColorTo := clNone;
  DateAppearance.WeekendFill.ColorMirror := clNone;
  DateAppearance.WeekendFill.ColorMirrorTo := clNone;
  DateAppearance.WeekendFill.GradientMirrorType := gtNone;
  DateAppearance.WeekendFont.Color := ATones.Background.TextColor;
  DateAppearance.WeekendFont.Name := GetMetroFont;

  DateAppearance.SelectedDateFill.Color := ATones.Selected.BrushColor;
  DateAppearance.SelectedDateFill.ColorTo := ATones.Selected.BrushColor;
  DateAppearance.SelectedDateFill.ColorMirror := ATones.Selected.BrushColor;
  DateAppearance.SelectedDateFill.ColorMirrorTo := ATones.Selected.BrushColor;
  DateAppearance.SelectedDateFill.BorderColor := ATones.Selected.BorderColor;
  DateAppearance.SelectedDateFill.GradientType := gtSolid;
  DateAppearance.SelectedDateFill.ColorTo := clNone;
  DateAppearance.SelectedDateFill.ColorMirror := clNone;
  DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
  DateAppearance.SelectedDateFill.GradientMirrorType := gtNone;
  DateAppearance.SelectedDateFont.Color := ATones.Selected.TextColor;
  DateAppearance.SelectedDateFont.Name := GetMetroFont;

  DateAppearance.HoverDateFill.Color :=  ATones.Hover.BrushColor;
  DateAppearance.HoverDateFill.ColorTo :=  ATones.Hover.BrushColor;
  DateAppearance.HoverDateFill.ColorMirror :=  ATones.Hover.BrushColor;
  DateAppearance.HoverDateFill.ColorMirrorTo :=  ATones.Hover.BrushColor;
  DateAppearance.HoverDateFill.BorderColor :=   ATones.Hover.BorderColor;
  DateAppearance.HoverDateFill.GradientType :=   gtSolid;
  DateAppearance.HoverDateFill.ColorTo := clNone;
  DateAppearance.HoverDateFill.ColorMirror := clNone;
  DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
  DateAppearance.HoverDateFill.GradientMirrorType := gtNone;
  DateAppearance.HoverDateFont.Color :=  ATones.Hover.TextColor;

  DateAppearance.CurrentDateFill.Color := ATones.Foreground.BrushColor;
  DateAppearance.CurrentDateFill.ColorTo := ATones.Foreground.BrushColor;
  DateAppearance.CurrentDateFill.ColorMirror := ATones.Foreground.BrushColor;
  DateAppearance.CurrentDateFill.ColorMirrorTo := ATones.Foreground.BrushColor;
  DateAppearance.CurrentDateFill.BorderColor := ATones.Foreground.BorderColor;
  DateAppearance.CurrentDateFill.GradientType := gtSolid;
  DateAppearance.CurrentDateFill.ColorTo := clNone;
  DateAppearance.CurrentDateFill.ColorMirror := clNone;
  DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
  DateAppearance.CurrentDateFill.GradientMirrorType := gtNone;
  DateAppearance.CurrentDateFont.Color := ATones.Foreground.TextColor;

  DateAppearance.DisabledDateFill.Color := ATones.Disabled.BrushColor;
  DateAppearance.DisabledDateFill.ColorTo := ATones.Disabled.BrushColor;
  DateAppearance.DisabledDateFill.ColorMirror := ATones.Disabled.BrushColor;
  DateAppearance.DisabledDateFill.ColorMirrorTo := ATones.Disabled.BrushColor;
  DateAppearance.DisabledDateFont.Color := ATones.Disabled.TextColor;
  DateAppearance.DisabledDateFill.GradientType := gtSolid;
  DateAppearance.DisabledDateFill.ColorTo := clNone;
  DateAppearance.DisabledDateFill.ColorMirror := clNone;
  DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
  DateAppearance.DisabledDateFill.GradientMirrorType := gtNone;
  DateAppearance.DisabledDateFill.BorderColor := ATones.Disabled.BorderColor;

  DateAppearance.WeekNumbers.Fill.Color := ATones.Background.BrushColor;
  DateAppearance.WeekNumbers.Fill.ColorTo := ATones.Background.BrushColor;
  DateAppearance.WeekNumbers.Fill.BorderColor := ATones.ForeGround.BorderColor;
  DateAppearance.WeekNumbers.Fill.GradientType := gtSolid;
  DateAppearance.WeekNumbers.Fill.ColorTo := clNone;
  DateAppearance.WeekNumbers.Fill.ColorMirror := clNone;
  DateAppearance.WeekNumbers.Fill.ColorMirrorTo := clNone;

  Changed;
end;

procedure TAdvSmoothCalendarGroup.SetComponentStyle(AStyle: TTMSStyle);
begin
  FMetroStyle := False;

  DateAppearance.CurrentDateFont.Color := clBlack;
  DateAppearance.SelectedDateFont.Color := clBlack;
  DateAppearance.DisabledDateFont.Color := clGray;
  DateAppearance.HoverDateFont.Color := clBlack;
  DateAppearance.WeekendFont.Color := clBlack;
  DateAppearance.DayOfWeekFont.Color := clBlack;
  DateAppearance.WeekendFont.Color := clBlack;
  DateAppearance.DateFont.Color := clBlack;
  DateAppearance.YearDateFont.Color := clBlack;
  DateAppearance.MonthDateFont.Color := clBlack;

  StatusAppearance.Fill.RoundingType:= rtboth;
  StatusAppearance.Glow := true;

  // TODO : do color settings here
 case AStyle of
    tsOffice2003Blue:
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
        Fill.BorderColor := clNone;

        Header.Fill.Color := $D68759;
        Header.Fill.ColorTo := $933803;
        Header.Font.Color := clWhite;
        Header.Fill.BorderColor := $962D00;

        Footer.Fill.Color := $D68759;
        Footer.Fill.ColorTo := $933803;
        Footer.Font.Color := clWhite;
        Footer.Fill.BorderColor := $962D00;

        DateAppearance.DayOfWeekFill.Color := RGB(196, 218, 250);
        DateAppearance.DayOfWeekFill.ColorTo := RGB(215, 232, 253);
        DateAppearance.DayOfWeekFill.BorderColor := Fill.BorderColor;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := $EEDBC8;
        DateAppearance.DateFill.ColorTo := $F6DDC9;
        DateAppearance.DateFill.ColorMirror := $EDD4C0;
        DateAppearance.DateFill.ColorMirrorTo := $F7E1D0;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := $EEDBC8;
        DateAppearance.WeekendFill.ColorTo := $F6DDC9;
        DateAppearance.WeekendFill.ColorMirror := $EDD4C0;
        DateAppearance.WeekendFill.ColorMirrorTo := $F7E1D0;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $EBFDFF;
        DateAppearance.HoverDateFill.ColorTo := $ACECFF;
        DateAppearance.HoverDateFill.ColorMirror := $59DAFF;
        DateAppearance.HoverDateFill.ColorMirrorTo := $A4E9FF;
        DateAppearance.HoverDateFill.BorderColor :=  $99CEDB;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;

        DateAppearance.SelectedDateFill.Color := $AAD9FF;
        DateAppearance.SelectedDateFill.ColorTo := $6EBBFF;
        DateAppearance.SelectedDateFill.ColorMirror := $42AEFE;
        DateAppearance.SelectedDateFill.ColorMirrorTo := $7AE1FE;
        DateAppearance.SelectedDateFill.BorderColor := $42AEFE;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $00F2F2F2;
        DateAppearance.DisabledDateFill.ColorTo := $00B6B6B6;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clGray;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $76AFF1;
        DateAppearance.CurrentDateFill.ColorTo := $4190F3;
        DateAppearance.CurrentDateFill.ColorMirror := $0E72F1;
        DateAppearance.CurrentDateFill.ColorMirrorTo := $4C9FFD;
        DateAppearance.CurrentDateFill.BorderColor := $45667B;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;

      end;
    tsOffice2003Silver:
      begin
        Fill.Color := $00E6D8D8;
        Fill.ColorTo := $00E6D8D8;
        Fill.BorderColor := clNone;

        Header.Fill.Color := $BDA4A5;
        Header.Fill.ColorTo := $957475;
        Header.Font.Color := clWhite;
        Header.Fill.BorderColor := $947C7C;

        Footer.Fill.Color := $BDA4A5;
        Footer.Fill.ColorTo := $957475;
        Footer.Font.Color := clWhite;
        Footer.Fill.BorderColor := $947C7C;

        DateAppearance.DayOfWeekFill.Color := RGB(222, 222, 235);
        DateAppearance.DayOfWeekFill.ColorTo := RGB(239, 239, 244);
        DateAppearance.DayOfWeekFill.BorderColor := Fill.BorderColor;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := $E6E9E2;
        DateAppearance.DateFill.ColorTo := $00E6D8D8;
        DateAppearance.DateFill.ColorMirror := $C8B2B3;
        DateAppearance.DateFill.ColorMirrorTo := $E6E9E2;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := $E6E9E2;
        DateAppearance.WeekendFill.ColorTo := $00E6D8D8;
        DateAppearance.WeekendFill.ColorMirror := $C8B2B3;
        DateAppearance.WeekendFill.ColorMirrorTo := $E6E9E2;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $EBFDFF;
        DateAppearance.HoverDateFill.ColorTo := $ACECFF;
        DateAppearance.HoverDateFill.ColorMirror := $59DAFF;
        DateAppearance.HoverDateFill.ColorMirrorTo := $A4E9FF;
        DateAppearance.HoverDateFill.BorderColor :=  $99CEDB;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $00F2F2F2;
        DateAppearance.DisabledDateFill.ColorTo := $00B6B6B6;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clGray;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.SelectedDateFill.Color := $AAD9FF;
        DateAppearance.SelectedDateFill.ColorTo := $6EBBFF;
        DateAppearance.SelectedDateFill.ColorMirror := $42AEFE;
        DateAppearance.SelectedDateFill.ColorMirrorTo := $7AE1FE;
        DateAppearance.SelectedDateFill.BorderColor := $42AEFE;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $76AFF1;
        DateAppearance.CurrentDateFill.ColorTo := $4190F3;
        DateAppearance.CurrentDateFill.ColorMirror := $0E72F1;
        DateAppearance.CurrentDateFill.ColorMirrorTo := $4C9FFD;
        DateAppearance.CurrentDateFill.BorderColor := $45667B;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Olive:
      begin
        Fill.Color := RGB(225, 234, 185);
        Fill.ColorTo := RGB(225, 234, 185);
        Fill.BorderColor := clNone;

        Header.Fill.Color := $82C0AF;
        Header.Fill.ColorTo := $447A63;
        Header.Font.Color := clWhite;
        Header.Fill.BorderColor := $588060;

        Footer.Fill.Color := $82C0AF;
        Footer.Fill.ColorTo := $447A63;
        Footer.Font.Color := clWhite;
        Footer.Fill.BorderColor := $588060;

        DateAppearance.DayOfWeekFill.Color := $CFF0EA;
        DateAppearance.DayOfWeekFill.ColorTo := RGB(225, 234, 185);
        DateAppearance.DayOfWeekFill.BorderColor := Fill.BorderColor;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := $CFF0EA;
        DateAppearance.DateFill.ColorTo := $CFF0EA;
        DateAppearance.DateFill.ColorMirror := $8CC0B1;
        DateAppearance.DateFill.ColorMirrorTo := $CFF0EA;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := $CFF0EA;
        DateAppearance.WeekendFill.ColorTo := $CFF0EA;
        DateAppearance.WeekendFill.ColorMirror := $8CC0B1;
        DateAppearance.WeekendFill.ColorMirrorTo := $CFF0EA;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $00F2F2F2;
        DateAppearance.DisabledDateFill.ColorTo := $00B6B6B6;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clGray;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $EBFDFF;
        DateAppearance.HoverDateFill.ColorTo := $ACECFF;
        DateAppearance.HoverDateFill.ColorMirror := $59DAFF;
        DateAppearance.HoverDateFill.ColorMirrorTo := $A4E9FF;
        DateAppearance.HoverDateFill.BorderColor :=  $99CEDB;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;

        DateAppearance.SelectedDateFill.Color := $AAD9FF;
        DateAppearance.SelectedDateFill.ColorTo := $6EBBFF;
        DateAppearance.SelectedDateFill.ColorMirror := $42AEFE;
        DateAppearance.SelectedDateFill.ColorMirrorTo := $7AE1FE;
        DateAppearance.SelectedDateFill.BorderColor := $42AEFE;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $76AFF1;
        DateAppearance.CurrentDateFill.ColorTo := $4190F3;
        DateAppearance.CurrentDateFill.ColorMirror := $0E72F1;
        DateAppearance.CurrentDateFill.ColorMirrorTo := $4C9FFD;
        DateAppearance.CurrentDateFill.BorderColor := $45667B;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Classic:
      begin
        Fill.Color := $00F2F2F2;
        Fill.ColorTo := $00F2F2F2;
        Fill.BorderColor := clNone;

        Header.Fill.Color := $808080;
        Header.Fill.ColorTo := $808080;
        Header.Font.Color := clWhite;
        Header.Fill.BorderColor := $808080;

        Footer.Fill.Color := $808080;
        Footer.Fill.ColorTo := $808080;
        Footer.Font.Color := clWhite;
        Footer.Fill.BorderColor := $808080;

        DateAppearance.DayOfWeekFill.Color := RGB(230, 227, 223);
        DateAppearance.DayOfWeekFill.ColorTo := RGB(214, 210, 202);
        DateAppearance.DayOfWeekFill.BorderColor := Fill.BorderColor;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := clWhite;
        DateAppearance.DateFill.ColorTo := $C9D1D5;
        DateAppearance.DateFill.ColorMirror := clNone;
        DateAppearance.DateFill.ColorMirrorTo := clNone;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := clWhite;
        DateAppearance.WeekendFill.ColorTo := $C9D1D5;
        DateAppearance.WeekendFill.ColorMirror := clNone;
        DateAppearance.WeekendFill.ColorMirrorTo := clNone;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $D2BDB6;
        DateAppearance.HoverDateFill.ColorTo := $D2BDB6;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor := $808080;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $D8D5D4;
        DateAppearance.DisabledDateFill.ColorTo := $D8D5D4;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clGray;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.SelectedDateFill.Color := $B59285;
        DateAppearance.SelectedDateFill.ColorTo := $B59285;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := $808080;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $D8D5D4;
        DateAppearance.CurrentDateFill.ColorTo := $D8D5D4;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := $808080;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Luna:
      begin
        Fill.Color := $00F3E5DA;
        Fill.ColorTo := $00F0DED0;
        Fill.BorderColor := clNone;

        Header.Fill.Color := $FFEFE3;
        Header.Fill.ColorTo := $FFD2AF;
        Header.Font.Color := $723708;
        Header.Fill.BorderColor := $00FFD2AF;

        Footer.Fill.Color := $FFEFE3;
        Footer.Fill.ColorTo := $FFD2AF;
        Footer.Font.Color := $723708;
        Footer.Fill.BorderColor := $00FFD2AF;

        DateAppearance.DayOfWeekFill.Color := Fill.Color;
        DateAppearance.DayOfWeekFill.ColorTo := Fill.ColorTo;
        DateAppearance.DayOfWeekFill.BorderColor := Fill.BorderColor;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := $FFEFE3;
        DateAppearance.DateFill.ColorTo := $FFDDC4;
        DateAppearance.DateFill.ColorMirror := $FFD1AD;
        DateAppearance.DateFill.ColorMirrorTo := $FFDBC0;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := $FFEFE3;
        DateAppearance.WeekendFill.ColorTo := $FFDDC4;
        DateAppearance.WeekendFill.ColorMirror := $FFD1AD;
        DateAppearance.WeekendFill.ColorMirrorTo := $FFDBC0;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $EBFDFF;
        DateAppearance.HoverDateFill.ColorTo := $ACECFF;
        DateAppearance.HoverDateFill.ColorMirror := $59DAFF;
        DateAppearance.HoverDateFill.ColorMirrorTo := $A4E9FF;
        DateAppearance.HoverDateFill.BorderColor :=  $99CEDB;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $00F2F2F2;
        DateAppearance.DisabledDateFill.ColorTo := $00B6B6B6;
        DateAppearance.DisabledDateFill.ColorMirror := $00B6B6B6;
        DateAppearance.DisabledDateFill.ColorMirrorTo := $00F2F2F2;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.SelectedDateFill.Color := $AAD9FF;
        DateAppearance.SelectedDateFill.ColorTo := $6EBBFF;
        DateAppearance.SelectedDateFill.ColorMirror := $42AEFE;
        DateAppearance.SelectedDateFill.ColorMirrorTo := $7AE1FE;
        DateAppearance.SelectedDateFill.BorderColor := $42AEFE;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $76AFF1;
        DateAppearance.CurrentDateFill.ColorTo := $4190F3;
        DateAppearance.CurrentDateFill.ColorMirror := $0E72F1;
        DateAppearance.CurrentDateFill.ColorMirrorTo := $4C9FFD;
        DateAppearance.CurrentDateFill.BorderColor := $45667B;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;

      end;
    tsOffice2007Obsidian:
      begin
        Fill.Color := $5C534C;
        Fill.ColorTo := $5C534C;
        Fill.BorderColor := clNone;

        Header.Fill.Color := $F2F1F0;
        Header.Fill.ColorTo := $C9C2BD;
        Header.Font.Color := $433C37;
        Header.Fill.BorderColor := $5C534C;

        Footer.Fill.Color := $F2F1F0;
        Footer.Fill.ColorTo := $C9C2BD;
        Footer.Font.Color := $433C37;
        Footer.Fill.BorderColor := $5C534C;
//
        DateAppearance.DayOfWeekFill.Color := RGB(215, 219, 224);
        DateAppearance.DayOfWeekFill.ColorTo := RGB(194, 199, 207);
        DateAppearance.DayOfWeekFill.BorderColor := Fill.BorderColor;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := $F9F8F8;
        DateAppearance.DateFill.ColorTo := $E4E2DF;
        DateAppearance.DateFill.ColorMirror := $D1CBC7;
        DateAppearance.DateFill.ColorMirrorTo := $E2DEDB;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := $F9F8F8;
        DateAppearance.WeekendFill.ColorTo := $E4E2DF;
        DateAppearance.WeekendFill.ColorMirror := $D1CBC7;
        DateAppearance.WeekendFill.ColorMirrorTo := $E2DEDB;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $EBFDFF;
        DateAppearance.HoverDateFill.ColorTo := $ACECFF;
        DateAppearance.HoverDateFill.ColorMirror := $59DAFF;
        DateAppearance.HoverDateFill.ColorMirrorTo := $A4E9FF;
        DateAppearance.HoverDateFill.BorderColor :=  $99CEDB;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $00F2F2F2;
        DateAppearance.DisabledDateFill.ColorTo := $00B6B6B6;
        DateAppearance.DisabledDateFill.ColorMirror := $00B6B6B6;
        DateAppearance.DisabledDateFill.ColorMirrorTo := $00F2F2F2;
        DateAppearance.DisabledDateFont.Color := clGray;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.SelectedDateFill.Color := $AAD9FF;
        DateAppearance.SelectedDateFill.ColorTo := $6EBBFF;
        DateAppearance.SelectedDateFill.ColorMirror := $42AEFE;
        DateAppearance.SelectedDateFill.ColorMirrorTo := $7AE1FE;
        DateAppearance.SelectedDateFill.BorderColor := $42AEFE;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $76AFF1;
        DateAppearance.CurrentDateFill.ColorTo := $4190F3;
        DateAppearance.CurrentDateFill.ColorMirror := $0E72F1;
        DateAppearance.CurrentDateFill.ColorMirrorTo := $4C9FFD;
        DateAppearance.CurrentDateFill.BorderColor := $45667B;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
      end;
    tsWindowsXP:
      begin
        Fill.Color := $00B6B6B6;
        Fill.ColorTo := $00B6B6B6;

        Header.Fill.Color := clBtnFace;
        Header.Fill.ColorTo := clBtnFace;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := clBlack;

        Footer.Fill.Color := clBtnFace;
        Footer.Fill.ColorTo := clBtnFace;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := clBlack;

        DateAppearance.DayOfWeekFill.Color := clBtnFace;
        DateAppearance.DayOfWeekFill.ColorTo := clBtnFace;
        DateAppearance.DayOfWeekFill.BorderColor := Fill.BorderColor;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := clBtnFace;//clWhite;
        DateAppearance.DateFill.ColorTo := clBtnFace;//$B9D8DC;
        DateAppearance.DateFill.ColorMirror := clNone;
        DateAppearance.DateFill.ColorMirrorTo := clNone;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := clBtnFace;//clWhite;
        DateAppearance.WeekendFill.ColorTo := clBtnFace;//$B9D8DC;
        DateAppearance.WeekendFill.ColorMirror := clNone;
        DateAppearance.WeekendFill.ColorMirrorTo := clNone;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $00B6B6B6;
        DateAppearance.DisabledDateFill.ColorTo := $00B6B6B6;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clGray;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $EFD3C6;
        DateAppearance.HoverDateFill.ColorTo := $EFD3C6;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  clHighlight;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;

        DateAppearance.SelectedDateFill.Color := clInactiveCaption;
        DateAppearance.SelectedDateFill.ColorTo := clInactiveCaption;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := clHighLight;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $B9D8DC;
        DateAppearance.CurrentDateFill.ColorTo := $B9D8DC;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := clBlack;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
      end;
    tsWhidbey:
      begin
        Fill.Color := $F5F9FA;
        Fill.ColorTo := $F5F9FA;
        Fill.BorderColor := clNone;

        Header.Fill.Color := $EBEEEF;
        Header.Fill.ColorTo := $7E9898;
        Header.Font.Color := clWhite;
        Header.Fill.BorderColor := $962D00;

        Footer.Fill.Color := $EBEEEF;
        Footer.Fill.ColorTo := $7E9898;
        Footer.Font.Color := clWhite;
        Footer.Fill.BorderColor := $962D00;

        DateAppearance.DayOfWeekFill.Color := RGB(232, 230, 210);
        DateAppearance.DayOfWeekFill.ColorTo := RGB(224, 221, 197);
        DateAppearance.DayOfWeekFill.BorderColor := Fill.BorderColor;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := clWhite;
        DateAppearance.DateFill.ColorTo := $DFEDF0;
        DateAppearance.DateFill.ColorMirror := $DFEDF0;
        DateAppearance.DateFill.ColorMirrorTo := $DFEDF0;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := clWhite;
        DateAppearance.WeekendFill.ColorTo := $DFEDF0;
        DateAppearance.WeekendFill.ColorMirror := $DFEDF0;
        DateAppearance.WeekendFill.ColorMirrorTo := $DFEDF0;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $00F2F2F2;
        DateAppearance.DisabledDateFill.ColorTo := $00B6B6B6;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clGray;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $EBFDFF;
        DateAppearance.HoverDateFill.ColorTo := $ACECFF;
        DateAppearance.HoverDateFill.ColorMirror := $59DAFF;
        DateAppearance.HoverDateFill.ColorMirrorTo := $A4E9FF;
        DateAppearance.HoverDateFill.BorderColor :=  $99CEDB;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;

        DateAppearance.SelectedDateFill.Color := $AAD9FF;
        DateAppearance.SelectedDateFill.ColorTo := $6EBBFF;
        DateAppearance.SelectedDateFill.ColorMirror := $42AEFE;
        DateAppearance.SelectedDateFill.ColorMirrorTo := $7AE1FE;
        DateAppearance.SelectedDateFill.BorderColor := $42AEFE;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $76AFF1;
        DateAppearance.CurrentDateFill.ColorTo := $4190F3;
        DateAppearance.CurrentDateFill.ColorMirror := $0E72F1;
        DateAppearance.CurrentDateFill.ColorMirrorTo := $4C9FFD;
        DateAppearance.CurrentDateFill.BorderColor := $45667B;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        Fill.Color := RGB(241, 244, 248);
        Fill.ColorTo := RGB(227, 232, 240);
        Fill.BorderColor := clNone;

        Header.Fill.Color := $F8F7F6;
        Header.Fill.ColorTo := $E8E0DB;
        Header.Font.Color := $8B4215;
        Header.Fill.BorderColor := $74706F;

        Footer.Fill.Color := $F8F7F6;
        Footer.Fill.ColorTo := $E8E0DB;
        Footer.Font.Color := $8B4215;
        Footer.Fill.BorderColor := $74706F;

        DateAppearance.DayOfWeekFill.Color := RGB(241, 244, 248);
        DateAppearance.DayOfWeekFill.ColorTo := RGB(227, 232, 240);
        DateAppearance.DayOfWeekFill.BorderColor := Fill.BorderColor;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := $F9F8F8;
        DateAppearance.DateFill.ColorTo := $E4E2DF;
        DateAppearance.DateFill.ColorMirror := $D1CBC7;
        DateAppearance.DateFill.ColorMirrorTo := $E2DEDB;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := $F9F8F8;
        DateAppearance.WeekendFill.ColorTo := $E4E2DF;
        DateAppearance.WeekendFill.ColorMirror := $D1CBC7;
        DateAppearance.WeekendFill.ColorMirrorTo := $E2DEDB;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $EBFDFF;
        DateAppearance.HoverDateFill.ColorTo := $ACECFF;
        DateAppearance.HoverDateFill.ColorMirror := $59DAFF;
        DateAppearance.HoverDateFill.ColorMirrorTo := $A4E9FF;
        DateAppearance.HoverDateFill.BorderColor :=  $99CEDB;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;

        DateAppearance.SelectedDateFill.Color := $AAD9FF;
        DateAppearance.SelectedDateFill.ColorTo := $6EBBFF;
        DateAppearance.SelectedDateFill.ColorMirror := $42AEFE;
        DateAppearance.SelectedDateFill.ColorMirrorTo := $7AE1FE;
        DateAppearance.SelectedDateFill.BorderColor := $42AEFE;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $00F2F2F2;
        DateAppearance.DisabledDateFill.ColorTo := $00B6B6B6;
        DateAppearance.DisabledDateFill.ColorMirror := $00B6B6B6;
        DateAppearance.DisabledDateFill.ColorMirrorTo := $00F2F2F2;
        DateAppearance.DisabledDateFont.Color := clGray;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $76AFF1;
        DateAppearance.CurrentDateFill.ColorTo := $4190F3;
        DateAppearance.CurrentDateFill.ColorMirror := $0E72F1;
        DateAppearance.CurrentDateFill.ColorMirrorTo := $4C9FFD;
        DateAppearance.CurrentDateFill.BorderColor := $45667B;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
      end;
      tsWindowsVista:
      begin
        Fill.Color := RGB(255, 255, 255);
        Fill.ColorTo := RGB(255, 255, 255);
        Fill.BorderColor := RGB(151, 151, 151);

        Header.Fill.Color := $FFFFFF;
        Header.Fill.ColorTo := $FFFFFF;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := clNone;

        Footer.Fill.Color := $FFFFFF;
        Footer.Fill.ColorTo := $FFFFFF;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := clNone;

        DateAppearance.DayOfWeekFill.Color := $FFFFFF;
        DateAppearance.DayOfWeekFill.ColorTo := $FFFFFF;
        DateAppearance.DayOfWeekFill.BorderColor := Fill.BorderColor;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := $FFFFFF;
        DateAppearance.DateFill.ColorTo := $FFFFFF;
        DateAppearance.DateFill.ColorMirror := $FFFFFF;
        DateAppearance.DateFill.ColorMirrorTo := $FFFFFF;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := $FFFFFF;
        DateAppearance.WeekendFill.ColorTo := $FFFFFF;
        DateAppearance.WeekendFill.ColorMirror := $FFFFFF;
        DateAppearance.WeekendFill.ColorMirrorTo := $FFFFFF;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $FFFFFF;
        DateAppearance.DisabledDateFill.ColorTo := $FFFFFF;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := $646464;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $FFFDF9;
        DateAppearance.HoverDateFill.ColorTo := $FFFAF0;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  $FCF2DA;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;
        DateAppearance.HoverDateFont.Color :=  $CC6600;


        DateAppearance.SelectedDateFill.Color := $FEF9F0;
        DateAppearance.SelectedDateFill.ColorTo := $FDF0D7;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := $FEDF9A;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $FEF9F0;
        DateAppearance.CurrentDateFill.ColorTo := $FDF0D7;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := $FEDF9AB;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
      end;
      tsWindows7:
      begin
        Fill.Color := RGB(255, 255, 255);
        Fill.ColorTo := RGB(255, 255, 255);
        Fill.BorderColor := RGB(151, 151, 151);

        Header.Fill.Color := $FFFFFF;
        Header.Fill.ColorTo := $FFFFFF;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := clNone;

        Footer.Fill.Color := $FFFFFF;
        Footer.Fill.ColorTo := $FFFFFF;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := clNone;

        DateAppearance.DayOfWeekFill.Color := $FFFFFF;
        DateAppearance.DayOfWeekFill.ColorTo := $FFFFFF;
        DateAppearance.DayOfWeekFill.BorderColor := Fill.BorderColor;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := $FFFFFF;
        DateAppearance.DateFill.ColorTo := $FFFFFF;
        DateAppearance.DateFill.ColorMirror := $FFFFFF;
        DateAppearance.DateFill.ColorMirrorTo := $FFFFFF;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := $FFFFFF;
        DateAppearance.WeekendFill.ColorTo := $FFFFFF;
        DateAppearance.WeekendFill.ColorMirror := $FFFFFF;
        DateAppearance.WeekendFill.ColorMirrorTo := $FFFFFF;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $FFFFFF;
        DateAppearance.DisabledDateFill.ColorTo := $FFFFFF;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := $646464;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $FDFBFA;
        DateAppearance.HoverDateFill.ColorTo := $FDF3EB;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  $FBD6B8;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;
        DateAppearance.HoverDateFont.Color :=  $CC6600;


        DateAppearance.SelectedDateFill.Color := $FCEBDC;
        DateAppearance.SelectedDateFill.ColorTo := $FCDBC1;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := $CEA27D;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $FEF9F0;
        DateAppearance.CurrentDateFill.ColorTo := $FCDBC1;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := $CEA27D;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
      end;
      tsTerminal:
      begin
        Fill.Color := clBtnFace;
        Fill.ColorTo := clBtnFace;
        Fill.BorderColor := clGray;

        Header.Fill.Color := clBtnFace;
        Header.Fill.ColorTo := clBtnFace;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := clNone;

        Footer.Fill.Color := clBtnFace;
        Footer.Fill.ColorTo := clBtnFace;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := clNone;

        DateAppearance.DayOfWeekFill.Color := clBtnFace;
        DateAppearance.DayOfWeekFill.ColorTo := clBtnFace;
        DateAppearance.DayOfWeekFill.BorderColor := Fill.BorderColor;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := clBtnFace;
        DateAppearance.DateFill.ColorTo := clBtnFace;
        DateAppearance.DateFill.ColorMirror := clNone;
        DateAppearance.DateFill.ColorMirrorTo := clNone;
        DateAppearance.DateFill.BorderColor := clNone;


        DateAppearance.WeekendFill.Color := clBtnFace;
        DateAppearance.WeekendFill.ColorTo := clBtnFace;
        DateAppearance.WeekendFill.ColorMirror := clNone;
        DateAppearance.WeekendFill.ColorMirrorTo := clNone;
        DateAppearance.WeekendFill.BorderColor := clNone;


        DateAppearance.DisabledDateFill.Color := clBtnFace;
        DateAppearance.DisabledDateFill.ColorTo := clBtnFace;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clGray;


        DateAppearance.HoverDateFill.Color := clSilver;
        DateAppearance.HoverDateFill.ColorTo := clSilver;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  clGray;
        DateAppearance.HoverDateFont.Color :=  clBlack;


        DateAppearance.SelectedDateFill.Color := clHighLight;
        DateAppearance.SelectedDateFill.ColorTo := clHighLight;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := clGray;
        DateAppearance.SelectedDateFont.Color := clWhite;

        DateAppearance.CurrentDateFill.Color := clBtnFace;
        DateAppearance.CurrentDateFill.ColorTo := clBtnFace;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := clHighLight;

      end;
      tsOffice2010Blue:
      begin
        Fill.Color := $EDDBCD;
        Fill.ColorTo := clNone;
        Fill.BorderColor := clNone;

        Header.Fill.Color := $EDDBCD;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := $5B391E;
        Header.Fill.BorderColor := $EEDDCF;

        Footer.Fill.Color := $EDDBCD;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := $5B391E;
        Footer.Fill.BorderColor := $EEDDCF;

        DateAppearance.DayOfWeekFill.Color := $EDDBCD;
        DateAppearance.DayOfWeekFill.ColorTo := clNone;
        DateAppearance.DayOfWeekFill.BorderColor := $FFC69A;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := $EDDBCD;
        DateAppearance.DateFill.ColorTo := clNone;
        DateAppearance.DateFill.ColorMirror := clNone;
        DateAppearance.DateFill.ColorMirrorTo := clNone;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := $EDDBCD;
        DateAppearance.WeekendFill.ColorTo := clNone;
        DateAppearance.WeekendFill.ColorMirror := clNone;
        DateAppearance.WeekendFill.ColorMirrorTo := clNone;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $DEC1A9;
        DateAppearance.DisabledDateFill.ColorTo := clNone;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clWhite;//$C0C0C0;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $FFEBDB;
        DateAppearance.HoverDateFill.ColorTo := clNone;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  clNone;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;
        DateAppearance.HoverDateFont.Color := $5B391E;


        DateAppearance.SelectedDateFill.Color := $DEC1A9;
        DateAppearance.SelectedDateFill.ColorTo := clNone;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := clNone;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $EDDBCD;
        DateAppearance.CurrentDateFill.ColorTo := clNone;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := $055CC9;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;


      end;

      tsOffice2010Silver:
      begin
        Fill.Color := $EDE9E5;
        Fill.ColorTo := clNone;
        Fill.BorderColor := clNone;

        Header.Fill.Color := $EDE9E5;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := $5B391E;
        Header.Fill.BorderColor := $EEDDCF;

        Footer.Fill.Color := $EDE9E5;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := $5B391E;
        Footer.Fill.BorderColor := $EEDDCF;

        DateAppearance.DayOfWeekFill.Color := $EDE9E5;
        DateAppearance.DayOfWeekFill.ColorTo := clNone;
        DateAppearance.DayOfWeekFill.BorderColor := $FFC69A;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := $EDE9E5;
        DateAppearance.DateFill.ColorTo := clNone;
        DateAppearance.DateFill.ColorMirror := clNone;
        DateAppearance.DateFill.ColorMirrorTo := clNone;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := $EDE9E5;
        DateAppearance.WeekendFill.ColorTo := clNone;
        DateAppearance.WeekendFill.ColorMirror := clNone;
        DateAppearance.WeekendFill.ColorMirrorTo := clNone;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $DEC1A9;
        DateAppearance.DisabledDateFill.ColorTo := clNone;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clWhite;//$C0C0C0;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $EEDDCF;
        DateAppearance.HoverDateFill.ColorTo := clNone;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  clNone;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;
        DateAppearance.HoverDateFont.Color := $5B391E;


        DateAppearance.SelectedDateFill.Color := $DEC1A9;
        DateAppearance.SelectedDateFill.ColorTo := clNone;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := clNone;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $EDE9E5;
        DateAppearance.CurrentDateFill.ColorTo := clNone;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := $055CC9;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
      end;
      tsOffice2010Black:
      begin
        Fill.Color := $828282;
        Fill.ColorTo := clNone;
        Fill.BorderColor := clNone;

        Header.Fill.Color := $828282;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := $5B391E;
        Header.Fill.BorderColor := $EEDDCF;

        Footer.Fill.Color := $828282;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := $5B391E;
        Footer.Fill.BorderColor := $EEDDCF;

        DateAppearance.DayOfWeekFill.Color :=  $828282;
        DateAppearance.DayOfWeekFill.ColorTo := clNone;
        DateAppearance.DayOfWeekFill.BorderColor := $FFC69A;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := $828282;
        DateAppearance.DateFill.ColorTo := clNone;
        DateAppearance.DateFill.ColorMirror := clNone;
        DateAppearance.DateFill.ColorMirrorTo := clNone;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := $828282;
        DateAppearance.WeekendFill.ColorTo := clNone;
        DateAppearance.WeekendFill.ColorMirror := clNone;
        DateAppearance.WeekendFill.ColorMirrorTo := clNone;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $A3A3A3;
        DateAppearance.DisabledDateFill.ColorTo := clNone;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clWhite;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $EEDDCF;
        DateAppearance.HoverDateFill.ColorTo := clNone;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  clNone;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;
        DateAppearance.HoverDateFont.Color := $5B391E;


        DateAppearance.SelectedDateFill.Color := $DEC1A9;
        DateAppearance.SelectedDateFill.ColorTo := clNone;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := clNone;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $828282;
        DateAppearance.CurrentDateFill.ColorTo := clNone;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := $055CC9;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
      end;
       tsWindows8, tsWindows10:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $DCDBDA;

        Header.Fill.Color := clWhite;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := clNone;

        Footer.Fill.Color := clWhite;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := $DCDBDA;

        DateAppearance.DayOfWeekFill.Color :=  clWhite;
        DateAppearance.DayOfWeekFill.ColorTo := clNone;
        DateAppearance.DayOfWeekFill.BorderColor := $DCDBDA;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := clWhite;
        DateAppearance.DateFill.ColorTo := clNone;
        DateAppearance.DateFill.ColorMirror := clNone;
        DateAppearance.DateFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := clWhite;
        DateAppearance.WeekendFill.ColorTo := clNone;
        DateAppearance.WeekendFill.ColorMirror := clNone;
        DateAppearance.WeekendFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $00F2F2F2;
        DateAppearance.DisabledDateFill.ColorTo := clNone;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clWhite;
//        DateAppearance.DisabledDateFill.BorderColor := $588060;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $F7EFE8;
        DateAppearance.HoverDateFill.ColorTo := clNone;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  $F9CEA4;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;
        DateAppearance.HoverDateFont.Color := $5B391E;


        DateAppearance.SelectedDateFill.Color := $F7E0C9;
        DateAppearance.SelectedDateFill.ColorTo := clNone;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := $E4A262;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $DAA026;
        DateAppearance.CurrentDateFill.ColorTo := clNone;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := $DAA026;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
        DAteAppearance.CurrentDateFont.Color := clwhite;
      end;

    tsOffice2013White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $D4D4D4;

        Header.Fill.Color := clWhite;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := clNone;

        Footer.Fill.Color := clWhite;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := $D4D4D4;

        DateAppearance.DayOfWeekFill.Color :=  clWhite;
        DateAppearance.DayOfWeekFill.ColorTo := clNone;
        DateAppearance.DayOfWeekFill.BorderColor := $D4D4D4;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := clWhite;
        DateAppearance.DateFill.ColorTo := clNone;
        DateAppearance.DateFill.ColorMirror := clNone;
        DateAppearance.DateFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := clWhite;
        DateAppearance.WeekendFill.ColorTo := clNone;
        DateAppearance.WeekendFill.ColorMirror := clNone;
        DateAppearance.WeekendFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $00F2F2F2;
        DateAppearance.DisabledDateFill.ColorTo := clNone;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clWhite;
//        DateAppearance.DisabledDateFill.BorderColor := $588060;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $F7EFE8;
        DateAppearance.HoverDateFill.ColorTo := clNone;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  $F9CEA4;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;
        DateAppearance.HoverDateFont.Color := $5B391E;


        DateAppearance.SelectedDateFill.Color := $F7E0C9;
        DateAppearance.SelectedDateFill.ColorTo := clNone;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := $E4A262;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $FF9933;
        DateAppearance.CurrentDateFill.ColorTo := clNone;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := $FF9933;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
        DAteAppearance.CurrentDateFont.Color := clwhite;
      end;

    tsOffice2013LightGray:
       begin
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $C6C6C6;

        Header.Fill.Color := clWhite;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := clNone;

        Footer.Fill.Color := clWhite;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := $C6C6C6;

        DateAppearance.DayOfWeekFill.Color :=  clWhite;
        DateAppearance.DayOfWeekFill.ColorTo := clNone;
        DateAppearance.DayOfWeekFill.BorderColor := $C6C6C6;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := clWhite;
        DateAppearance.DateFill.ColorTo := clNone;
        DateAppearance.DateFill.ColorMirror := clNone;
        DateAppearance.DateFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := clWhite;
        DateAppearance.WeekendFill.ColorTo := clNone;
        DateAppearance.WeekendFill.ColorMirror := clNone;
        DateAppearance.WeekendFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $00F2F2F2;
        DateAppearance.DisabledDateFill.ColorTo := clNone;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clWhite;
//        DateAppearance.DisabledDateFill.BorderColor := $588060;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $F7EFE8;
        DateAppearance.HoverDateFill.ColorTo := clNone;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  $F9CEA4;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;
        DateAppearance.HoverDateFont.Color := $5B391E;


        DateAppearance.SelectedDateFill.Color := $F7E0C9;
        DateAppearance.SelectedDateFill.ColorTo := clNone;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := $E4A262;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $FF9933;
        DateAppearance.CurrentDateFill.ColorTo := clNone;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := $FF9933;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
        DAteAppearance.CurrentDateFont.Color := clwhite;
      end;

    tsOffice2013Gray:
       begin
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $ABABAB;

        Header.Fill.Color := clWhite;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := clNone;

        Footer.Fill.Color := clWhite;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := $ABABAB;

        DateAppearance.DayOfWeekFill.Color :=  clWhite;
        DateAppearance.DayOfWeekFill.ColorTo := clNone;
        DateAppearance.DayOfWeekFill.BorderColor := $ABABAB;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := clWhite;
        DateAppearance.DateFill.ColorTo := clNone;
        DateAppearance.DateFill.ColorMirror := clNone;
        DateAppearance.DateFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := clWhite;
        DateAppearance.WeekendFill.ColorTo := clNone;
        DateAppearance.WeekendFill.ColorMirror := clNone;
        DateAppearance.WeekendFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $00F2F2F2;
        DateAppearance.DisabledDateFill.ColorTo := clNone;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := clWhite;
//        DateAppearance.DisabledDateFill.BorderColor := $588060;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $F7EFE8;
        DateAppearance.HoverDateFill.ColorTo := clNone;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  $F9CEA4;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;
        DateAppearance.HoverDateFont.Color := $5B391E;


        DateAppearance.SelectedDateFill.Color := $F7E0C9;
        DateAppearance.SelectedDateFill.ColorTo := clNone;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := $E4A262;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $FF9933;
        DateAppearance.CurrentDateFill.ColorTo := clNone;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := $FF9933;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
        DAteAppearance.CurrentDateFont.Color := clwhite;
      end;
  tsOffice2016White:
      begin
        StatusAppearance.Fill.RoundingType:= rtnone;
        StatusAppearance.Glow := false;

        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        Fill.BorderColor := clNone; //$D4D4D4;

        Header.Fill.Color := $F0F0F0;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := $444444;
        Header.Fill.BorderColor := clNone;

        Footer.Fill.Color := $F0F0F0;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := $444444;
        Footer.Fill.BorderColor := clNone;

        DateAppearance.DayOfWeekFill.Color :=  clWhite;
        DateAppearance.DayOfWeekFill.ColorTo := clNone;
        DateAppearance.DayOfWeekFill.BorderColor := clWhite;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := clWhite;
        DateAppearance.DateFill.ColorTo := clNone;
        DateAppearance.DateFill.ColorMirror := clNone;
        DateAppearance.DateFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := clWhite;
        DateAppearance.WeekendFill.ColorTo := clNone;
        DateAppearance.WeekendFill.ColorMirror := clNone;
        DateAppearance.WeekendFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := clWhite;
        DateAppearance.DisabledDateFill.ColorTo := clNone;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := $666666;
//        DateAppearance.DisabledDateFill.BorderColor := $588060;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $FAF2E6;
        DateAppearance.HoverDateFill.ColorTo := clNone;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  $FAF2E6;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;
        DateAppearance.HoverDateFont.Color := $5B391E;


        DateAppearance.SelectedDateFill.Color := $F7E6CD;
        DateAppearance.SelectedDateFill.ColorTo := clNone;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := $F7E6CD;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;

        DateAppearance.CurrentDateFill.Color := $C67200;
        DateAppearance.CurrentDateFill.ColorTo := clNone;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := $C67200;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
        DAteAppearance.CurrentDateFont.Color := clwhite;
      end;

    tsOffice2016Gray:
       begin

        StatusAppearance.Fill.RoundingType:= rtnone;
        StatusAppearance.Glow := false;

        Fill.Color := $6A6A6A;
        Fill.ColorTo := clNone;
        Fill.BorderColor := clNone;

        Header.Fill.Color := $585858;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := clWhite;
        Header.Fill.BorderColor := clNone;

        Footer.Fill.Color := $585858;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := clWhite;
        Footer.Fill.BorderColor := clNone;

        DateAppearance.DayOfWeekFill.Color :=  $6A6A6A;
        DateAppearance.DayOfWeekFill.ColorTo := clNone;
        DateAppearance.DayOfWeekFill.BorderColor := $6A6A6A;

        DateAppearance.DayOfWeekFont.Color := clWhite;
        DateAppearance.WeekNumbers.Font.Color := clWhite;
        DateAppearance.DateFont.Color := clWhite;
        DateAppearance.WeekendFont.Color := clWhite;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := $6A6A6A;
        DateAppearance.DateFill.ColorTo := clNone;
        DateAppearance.DateFill.ColorMirror := clNone;
        DateAppearance.DateFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := $6A6A6A;
        DateAppearance.WeekendFill.ColorTo := clNone;
        DateAppearance.WeekendFill.ColorMirror := clNone;
        DateAppearance.WeekendFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $6A6A6A;
        DateAppearance.DisabledDateFill.ColorTo := clNone;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := $F0F0F0;
//        DateAppearance.DisabledDateFill.BorderColor := $588060;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $B06700;
        DateAppearance.HoverDateFill.ColorTo := clNone;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  $B06700;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;
        DateAppearance.HoverDateFont.Color := clWhite;


        DateAppearance.SelectedDateFill.Color := $B2B2B2;
        DateAppearance.SelectedDateFill.ColorTo := clNone;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := $B2B2B2;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;
        DateAppearance.SelectedDateFont.Color := clBlack;

        DateAppearance.CurrentDateFill.Color := $C67200;
        DateAppearance.CurrentDateFill.ColorTo := clNone;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := $C67200;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
        DAteAppearance.CurrentDateFont.Color := clwhite;
      end;

    tsOffice2016Black:
       begin
        StatusAppearance.Fill.RoundingType:= rtnone;
        StatusAppearance.Glow := false;

        Fill.Color := $252525;
        Fill.ColorTo := clNone;
        Fill.BorderColor := clNone;

        Header.Fill.Color := $383838;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := clWhite;
        Header.Fill.BorderColor := clNone;

        Footer.Fill.Color := $383838;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := clWhite;
        Footer.Fill.BorderColor := clNone;

        DateAppearance.DayOfWeekFont.Color := clWhite;
        DateAppearance.WeekNumbers.Font.Color := clWhite;
        DateAppearance.DateFont.Color := clWhite;
        DateAppearance.WeekendFont.Color := clWhite;

        DateAppearance.DayOfWeekFill.Color :=  $252525;
        DateAppearance.DayOfWeekFill.ColorTo := clNone;
        DateAppearance.DayOfWeekFill.BorderColor := $252525;

        DateAppearance.WeekNumbers.Fill.Color := DateAppearance.DayOfWeekFill.Color;
        DateAppearance.WeekNumbers.Fill.ColorTo := DateAppearance.DayOfWeekFill.ColorTo;
        DateAppearance.WeekNumbers.Fill.BorderColor := DateAppearance.DayOfWeekFill.BorderColor;

        DateAppearance.DateFill.Color := $252525;
        DateAppearance.DateFill.ColorTo := clNone;
        DateAppearance.DateFill.ColorMirror := clNone;
        DateAppearance.DateFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.DateFill.BorderColor := clNone;
        DateAppearance.DateFill.GradientMirrorType := gtVertical;

        DateAppearance.WeekendFill.Color := $252525;
        DateAppearance.WeekendFill.ColorTo := clNone;
        DateAppearance.WeekendFill.ColorMirror := clNone;
        DateAppearance.WeekendFill.ColorMirrorTo := clNone;
//        DateAppearance.DateFill.BorderColor := $8CC0B1;
        DateAppearance.WeekendFill.BorderColor := clNone;
        DateAppearance.WeekendFill.GradientMirrorType := gtVertical;

        DateAppearance.DisabledDateFill.Color := $252525;
        DateAppearance.DisabledDateFill.ColorTo := clNone;
        DateAppearance.DisabledDateFill.ColorMirror := clNone;
        DateAppearance.DisabledDateFill.ColorMirrorTo := clNone;
        DateAppearance.DisabledDateFont.Color := $F0F0F0;
//        DateAppearance.DisabledDateFill.BorderColor := $588060;
        DateAppearance.DisabledDateFill.GradientMirrorType := gtVertical;

        DateAppearance.HoverDateFill.Color := $B06700;
        DateAppearance.HoverDateFill.ColorTo := clNone;
        DateAppearance.HoverDateFill.ColorMirror := clNone;
        DateAppearance.HoverDateFill.ColorMirrorTo := clNone;
        DateAppearance.HoverDateFill.BorderColor :=  $B06700;
        DateAppearance.HoverDateFill.GradientMirrorType := gtVertical;
        DateAppearance.HoverDateFont.Color := clWhite;


        DateAppearance.SelectedDateFill.Color := $424242;
        DateAppearance.SelectedDateFill.ColorTo := clNone;
        DateAppearance.SelectedDateFill.ColorMirror := clNone;
        DateAppearance.SelectedDateFill.ColorMirrorTo := clNone;
        DateAppearance.SelectedDateFill.BorderColor := $E424242;
        DateAppearance.SelectedDateFill.GradientMirrorType := gtVertical;
        DateAppearance.SelectedDateFont.Color:= $F1F1F1;

        DateAppearance.CurrentDateFill.Color := $C67200;
        DateAppearance.CurrentDateFill.ColorTo := clNone;
        DateAppearance.CurrentDateFill.ColorMirror := clNone;
        DateAppearance.CurrentDateFill.ColorMirrorTo := clNone;
        DateAppearance.CurrentDateFill.BorderColor := $C67200;
        DateAppearance.CurrentDateFill.GradientMirrorType := gtVertical;
        DAteAppearance.CurrentDateFont.Color := clwhite;
      end;

  end;


  Header.ArrowColor := Header.Font.Color;
  DateAppearance.DateBeforeFill.Color := clNone;
  DateAppearance.DateBeforeFont.Color := clSilver;
  DateAppearance.DateAfterFont.Color := clSilver;
  DateAppearance.DateAfterFill.Color := clNone;

   case AStyle of
     tsOffice2010Blue:
     begin
   DateAppearance.DateBeforeFill.Color := $EDDBCD;
   DateAppearance.DateBeforeFill.ColorTo := clNone;
   DateAppearance.DateBeforeFont.Color := $AD815E;
   DateAppearance.DateAfterFont.Color := $AD815E;
   DateAppearance.DateAfterFill.Color := $EDDBCD;
   DateAppearance.DateAfterFill.ColorTo := clNone;

     end;
     tsOffice2010Silver:
     begin
   DateAppearance.DateBeforeFill.Color := $EDE9E5;
   DateAppearance.DateBeforeFill.ColorTo := clNone;
   DateAppearance.DateBeforeFont.Color := $AD815E;
   DateAppearance.DateAfterFont.Color := $AD815E;
   DateAppearance.DateAfterFill.Color := $EDE9E5;
   DateAppearance.DateAfterFill.ColorTo := clNone;

     end;
     tsOffice2010Black:
     begin
   DateAppearance.DateBeforeFill.Color := $828282;
   DateAppearance.DateBeforeFill.ColorTo := clNone;
   DateAppearance.DateBeforeFont.Color := $AD815E;
   DateAppearance.DateAfterFont.Color := $AD815E;
   DateAppearance.DateAfterFill.Color := $828282;
   DateAppearance.DateAfterFill.ColorTo := clNone;

     end;
   end;

  Changed;

end;

procedure TAdvSmoothCalendarGroup.SetColumns(const Value: Integer);
begin
  if FColumns <> Value then
  begin
    FColumns := Max(1, Value);
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetDate(const Value: TDateTime);
begin
  if FDate <> Value then
  begin
    DisjunctDates.Clear;
    FDate := Value;
    FStartDate := Value;
    FEndDate := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetDateAppearance(
  const Value: TAdvSmoothCalendarDateAppearance);
begin
  if FDateAppearance <> Value then
  begin
    FDateAppearance := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetDisjunctDaySelect(const Value: Boolean);
begin
  if FDisjunctDaySelect <> Value then
  begin
    FDisjunctDaySelect := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetEndDate(const Value: TDateTime);
begin
  if FEndDate <> Value then
  begin
    FEndDate := Value;
    FDate := -1;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetFocusColor(const Value: TColor);
begin
  if FFocusColor <> Value then
  begin
    FFocusColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetFooter(
  const Value: TAdvSmoothCalendarFooter);
begin
  if FFooter <> Value then
  begin
    FFooter.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetHeader(
  const Value: TAdvSmoothCalendarHeader);
begin
  if FHeader <> Value then
  begin
    FHeader := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetMaxDate(const Value: TDate);
begin
  if FMaxDate <> Value then
  begin
    FMaxDate := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetMinDate(const Value: TDate);
begin
  if FMinDate <> Value then
  begin
    FMinDate := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetMonth(const Value: Integer);
var
  val: Integer;
begin
  if FMonth <> Value then
  begin
    val := Max(1, Min(Value, 12));
    FMonth := val;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetMultiSelect(const Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetRows(const Value: Integer);
begin
  if FRows <> Value then
  begin
    FRows := Max(1, Value);
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetShowCurrentDate(const Value: Boolean);
begin
  if FShowCurrentDate <> Value then
  begin
    FShowCurrentDate := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetSingleFillSelection(const Value: Boolean);
begin
  if FSingleFillSelection <> Value then
  begin
    FSingleFillSelection := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetStartDate(const Value: TDateTime);
begin
  if FStartDate <> Value then
  begin
    DisjunctDates.Clear;
    FDate := -1;
    FStartDate := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetStatusAppearance(const Value: TGDIPStatus);
begin
  if FStatusAppearance <> Value then
  begin
    FStatusAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.SetVersion(const Value: String);
begin

end;

procedure TAdvSmoothCalendarGroup.SetYear(const Value: Integer);
begin
  if FYear <> Value then
  begin
    FYear := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalendarGroup.UpdateCalendars;
var
  I: Integer;
  w, h: Single;
  c, r: Integer;
  cl: TAdvSmoothCalGroup;
  rt: TGPRectF;
begin
  if (Columns <= 0) and (Rows <= 0) then
    Exit;

  if FUpdateCount > 0 then
    Exit;

  rt := MakeRect(1, 1, Width - 2, Height - 2);

  w := rt.Width / Columns;
  h := rt.Height / Rows;

  I := 0;
  for r := 0 to Rows - 1 do
  begin
    for c := 0 to Columns - 1 do
    begin
      if (I >= 0) and (I <= FCalendarDisplayList.Count - 1) then
      begin
        cl := TAdvSmoothCalGroup(FCalendarDisplayList[I]);
        cl.SetBounds(Round(rt.X + (c * w)), Round(rt.Y + (r * h)), Round(w), Round(h));

        if Assigned(OnCustomizeCalendar) then
          OnCustomizeCalendar(Self, cl, cl.Month, cl.Year);
      end;

      Inc(I);
    end;
  end;
end;

procedure TAdvSmoothCalendarGroup.WholeYear(AYear: Integer = -1);
begin
  BeginUpdate;
  if AYear <> -1 then
    Year := AYear;
  Month := 1;
  Columns := 4;
  Rows := 3;
  EndUpdate;
end;

procedure TAdvSmoothCalendarGroup.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TAdvSmoothCalendarGroup.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TAdvSmoothCalendarGroup.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  {$IFNDEF DELPHI_UNICODE}
  dbl: boolean;
  {$ENDIF}
  p: TPoint;
  i: integer;
begin
  if Assigned(Parent) {and (Fill.ShadowOffset > 0) ?} then
  begin
    DC := Message.DC;
    if DC <> 0 then
    begin
      {$IFNDEF DELPHI_UNICODE}
      dbl := Parent.DoubleBuffered;
      Parent.DoubleBuffered := false;
      {$ENDIF}
      i := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(DC, p.x, p.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(DC, nil);
      RestoreDC(DC, i);
      {$IFNDEF DELPHI_UNICODE}
      Parent.DoubleBuffered := dbl;
      {$ENDIF}
    end;
  end;

  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

{ TAdvSmoothCalGroup }

procedure TAdvSmoothCalGroup.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FGroup) then
  begin
    if (Key = VK_SHIFT) and FGroup.ResetMultiSelect then
    begin
      FGroup.ResetMultiSelect := False;
      FGroup.cal := Self;
      FGroup.FBlockEvent := True;
      FGroup.CalendarSelectDate(Self, dmDay, FocusDate);
      FGroup.FBlockEvent := False;
      Startd := FocusDate;
    end;
  end;
  inherited;
end;

procedure TAdvSmoothCalGroup.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Assigned(Fgroup) and ((Key = VK_LEFT) or (Key = VK_DOWN) or (Key = VK_RIGHT) or (Key = VK_UP)) then
    FGroup.ResetMultiSelect := True;
end;

procedure TAdvSmoothCalGroup.SetFocusDate(const Value: TDateTime);
var
  val: Integer;
begin
  val := MonthOf(Value);
  if MonthOf(Value) = OriginalMonth then
    inherited
  else
  begin
    if Assigned(OnChangeCalendarMonth) then
      OnChangeCalendarMonth(Self, val, OriginalMonth);
  end;
end;

end.

