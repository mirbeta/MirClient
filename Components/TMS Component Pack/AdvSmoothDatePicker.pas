{***********************************************************************}
{ TAdvSmoothDatePicker component                                        }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2013 - 2015                                    }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

{$I TMSDEFS.INC}

unit AdvSmoothDatePicker;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, AdvSmoothEditButton, AdvSmoothCalendar, AdvStyleIF, GDIPFill
  {$IFDEF DELPHI6_LVL}
  ,Variants
  {$ENDIF}
  ;


const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // Version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed : issue with dropdown button click & dateselect
  // v1.0.0.2 : Improved : behaviour with click outside calendar to hide calendar
  //          : Improved : default control width
  // v1.0.0.3 : Fix with default weekend fill
  // v1.0.1.0 : New Exposed Width and Height of Calendar
  //          : Fix with hiding dropdown calendar when in Month or Year mode
  // v1.5.0.0 : New Properties MaxDate, MinDate, Animation, ShowCurrentDate, ShowHint, StatusAppearance
  //          : New : Calendar Day Status Indicator
  //          : New : Exposed events OnDateFill, OnDateStatus, OnDateHint
  //          : Fixed : Issue with Resizing Calendar and first dropdown.
  // v1.5.0.1 : Fixed : Memory leak
  // v1.5.0.2 : Fixed : Issue with initialization of the Text property
  // v1.5.0.3 : Fixed : Access violation when initializing events
  // v1.5.0.4 : Fixed : issue with editing date
  //          : Fixed : issue with ReadOnly property
  // v1.5.1.0 : New : Exposed events OnMonthChanged, OnYearChanged, OnYearRangeChanged, OnCurrentDayClicked, OnDateModeChanged
  // v1.5.1.1 : Improved : Alt-Down key to show calendar.
  // v1.5.2.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.5.3.0 : New : Built-in support for reduced color set for use with terminal servers
  //          : Fixed : issue with EmptyText property
  //          : Fixed : issue with SetComponentStyle overriding values
  // v1.5.3.1 : Fixed : issue with datepicker change called during load
  // v1.5.3.2 : Fixed : issue with setfocus when disabled
  //          : Fixed : issue with ctl3D in AdvSmoothEditButton.pas
  // v1.5.4.0 : New : Property Format, to control the formatting of the date.
  //          : Fixed : issue with use on forms with WS_EX_APPWINDOW style
  // v1.5.4.1 : Fixed : Issue with owner when creating at runtime with nil
  // v1.5.4.2 : Fixed : Issue with date initialization when text is cleared
  // v2.0.0.0 : New : Database aware version of the TAdvSmoothDatePicker
  // v2.0.0.1 : Fixed : Issue with setting Date := 0;
  // v2.0.0.2 : Fixed : Fix for use with scripter studio
  // v2.0.0.3 : Fixed : Issue with reading date in OnChange after text change
  // v2.0.0.4 : Fixed : Issue in DB with setting nil dates
  // v2.0.0.5 : Fixed : Issue with clearing date in calendar
  // v2.0.0.6 : Fixed : Issue with flickering when setting Enabled property
  // v2.0.1.0 : New : Built-in support for Office 2010 colors
  // v2.0.1.1 : Fixed : Issue in DB version calling OnSelectDate event
  // v2.0.1.2 : Fixed : Issue with deleting text at runtime
  // v2.0.1.3 : Fixed : Issue with accessing dataset when destroying
  // v2.0.1.4 : Fixed : Issue with parent
  // v2.0.1.5 : Fixed : Issue with button size and position
  // v2.0.1.6 : Fixed : Issue with VK_F4 to show Calendar
  // v2.0.1.7 : Fixed : Issue with stay on top
  // v2.0.1.8 : Improved : Time value of inserted date is saved.
  // v2.0.2.0 : New : Published new events
  // v2.1.0.0 : New : Metro Style support
  //          : Fixed : Issue with updown in header
  // v2.1.0.1 : Fixed : Issue with mix between VarToDateTime and StrToDate
  // v2.1.0.2 : Fixed : Issue with entering invalid dates loop
  // v2.1.5.0 : New : Mask editing support
  // v2.2.0.0 : New : Windows 8, Office 2013 styles added
  // v2.2.0.1 : Improved : AllowNumericNullValue in DBAdvSmoothDatePicker
  // v2.2.0.2 : Improved : Exception handling during debugging
  // v2.2.0.3 : Fixed : Issue with leaving focus after programmatically setting date
  // v2.3.0.0 : New : Windows 10, Office 2016 styles added

type
  TAdvSmoothDatePicker = class;

  TAdvSmoothDatePickerCalendar = class(TPersistent)
  private
    FOwner: TAdvsmoothDatePicker;
    FHeader: TAdvSmoothCalendarHeader;
    FDateAppearance: TAdvSmoothCalendarDateAppearance;
    FFooter: TAdvSmoothCalendarFooter;
    FFill: TGDIPFill;
    FWidth: integer;
    FHeight: integer;
    FMaxDate: TDate;
    FAnimation: Boolean;
    FMinDate: TDate;
    FShowCurrentDate: Boolean;
    FStatusAppearance: TGDIPStatus;
    FShowHint: Boolean;
  protected
    procedure DateAppearanceChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure FooterChanged(Sender: TObject);
    procedure HeaderChanged(Sender: TObject);
    procedure WidthChanged(Sender: TObject);
    procedure HeightChanged(Sender: TObject);
    procedure ShowCurrentDateChanged(Sender: TObject);
    procedure StatusAppearanceChanged(Sender: TObject);
    procedure AnimationChanged(Sender: TObject);
    procedure MaxDateChanged(Sender: TObject);
    procedure MinDateChanged(Sender: TObject);
    procedure ShowHintChanged(Sender: TObject);
    procedure Update;
  public
    constructor Create(AOwner: TAdvSmoothDatePicker);
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  published
    property Fill: TGDIPFill read FFill write FFill;
    property MinDate: TDate read FMinDate write FMinDate;
    property MaxDate: TDate read FMaxDate write FMaxDate;
    property Animation: Boolean read FAnimation write FAnimation;
    property ShowCurrentDate: Boolean read FShowCurrentDate write FShowCurrentDate;
    property DateAppearance: TAdvSmoothCalendarDateAppearance read FDateAppearance write FDateAppearance;
    property StatusAppearance: TGDIPStatus read FStatusAppearance write FStatusAppearance;
    property Footer: TAdvSmoothCalendarFooter read FFooter write FFooter;
    property Header: TAdvSmoothCalendarHeader read FHeader write FHeader;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property ShowHint: Boolean read FShowHint write FShowHint;
  end;

  TDropDownForm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothDatePicker = class(TAdvSmoothEditBtn, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FTimer: TTimer;
    FTime: TDateTime;
    FDesignTime: Boolean;
    FCal: TAdvSmoothCalendar;
    FDeactivating: boolean;
    FIgnoreSelect: boolean;
    CalParent : TDropDownForm;
    CancelThisBtnClick : Boolean;
    FHideCalendarAfterSelection: boolean;
    FOnDaySelect: TAdvSmoothCalendarDateSelectedEvent;
    FCalendar: TAdvSmoothDatePickerCalendar;
    FOnDateFill: TAdvSmoothCalendarDateFillEvent;
    FOnDateStatus: TAdvSmoothCalendarGetDateStatusEvent;
    FOnDateHint: TAdvSmoothCalendarDateHintEvent;
    FOnYearRangeChanged: TAdvSmoothCalendarYearRangeChangedEvent;
    FOnYearChanged: TAdvSmoothCalendarYearChangedEvent;
    FOnCurrentDayClick: TAdvSmoothCalendarCurrentDayClickEvent;
    FOnDateModeChanged: TAdvSmoothCalendarDateModeChangedEvent;
    FOnMonthChanged: TAdvSmoothCalendarMonthChangedEvent;
    FFormat: String;
    FOnGetYearName: TAdvSmoothCalendarGetYearNameEvent;
    FOnGetWeekDayName: TAdvSmoothCalendarGetWeekDayNameEvent;
    FOnGetHeaderText: TAdvSmoothCalendarGetTextEvent;
    FOnMonthFill: TAdvSmoothCalendarNormalDateFillEvent;
    FOnGetMonthName: TAdvSmoothCalendarGetMonthNameEvent;
    FOnGetFooterText: TAdvSmoothCalendarGetTextEvent;
    FOnYearFill: TAdvSmoothCalendarNormalDateFillEvent;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure HideParent;
    procedure InitEvents;
    function GetParentEx: TWinControl;
    procedure SetParentEx(const Value: TWinControl);
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
    procedure SetCalendar(const Value: TAdvSmoothDatePickerCalendar);
    procedure SetFormat(const Value: String);
    { Private declarations }
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    function IsDropDownVisible: Boolean; override;
    function GetVersionNr: Integer; override;
    { Protected declarations }
    procedure BtnClick(Sender: TObject); override;
    procedure CalParentDeactivate(Sender: TObject);
    procedure CalendarDateFill(Sender: TObject; AFill: TGDIPFill; AFont: TFont; Date: TDateTime; DateKind: TAdvSmoothCalendarDateKind);
    procedure CalendarDateHint(Sender: TObject; Date: TDateTime; var hint: String);
    procedure CalendarDateStatus(Sender: TObject; Date: TDateTime; var StatusMessage: String;
      Fill: TGDIPStatus; var OffsetX: integer; var OffsetY: integer);
    procedure CalendarGetHeaderText(Sender: TObject; var AText: String);
    procedure CalendarGetFooterText(Sender: TObject; var AText: String);

    procedure CalendarGetWeekDayName(Sender: TObject; WeekDay: Integer; var WeekDayName: String);
    procedure CalendarGetMonthName(Sender: TObject; Month: Integer; var MonthName: String);
    procedure CalendarGetYearName(Sender: TObject; Year: Integer; var YearName: String);
    procedure CalendarMonthFill(Sender: TObject; AFill: TGDIPFill; AFont: TFont; Date: TDateTime; DateKind: TAdvSmoothCalendarDateKind);
    procedure CalendarYearFill(Sender: TObject; AFill: TGDIPFill; AFont: TFont; Date: TDateTime; DateKind: TAdvSmoothCalendarDateKind);

    procedure CalendarCurrentDateClick(Sender: TObject; var allow: Boolean);
    procedure CalendarChangeMonth(Sender: TObject; Month: integer);
    procedure CalendarChangeYear(Sender: TObject; Year: integer);
    procedure CalendarChangeYearRange(Sender: TObject; YearFrom, YearTo: integer);
    procedure CalendarChangeMode(Sender: TObject; Mode, ModeTo: TAdvSmoothCalendarDateMode; var allow: Boolean);
    procedure CalendarDaySelect(Sender: TObject; Mode: TAdvSmoothCalendarDateMode; Date: TDateTime); virtual;
    procedure CalendarKeyPress(Sender: TObject; var Key: Char);
    procedure CalendarKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CalendarKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure Loaded; override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CreateWnd; override;
    procedure TimerEvent(Sender: TObject);
    property Cal: TAdvSmoothCalendar read FCal write FCal;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure CancelBtnClick;
    destructor Destroy; override;
    procedure DropDown; virtual;
    property Parent: TWinControl read GetParentEx write SetParentEx;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
  published
    { Published declarations }
    property TabOrder;
    property TabStop default true;
    property HideCalendarAfterSelection : boolean read FHideCalendarAfterSelection
      write FHideCalendarAfterSelection;
    property OnSelectDate: TAdvSmoothCalendarDateSelectedEvent read FOnDaySelect write FOnDaySelect;
    property OnDateFill: TAdvSmoothCalendarDateFillEvent read FOnDateFill write FOnDateFill;
    property OnDateStatus: TAdvSmoothCalendarGetDateStatusEvent read FOnDateStatus write FOnDateStatus;
    property OnDateHint: TAdvSmoothCalendarDateHintEvent read FOnDateHint write FOnDateHint;
    property OnDateModeChanged: TAdvSmoothCalendarDateModeChangedEvent read FOnDateModeChanged write FOnDateModeChanged;
    property OnMonthChanged: TAdvSmoothCalendarMonthChangedEvent read FOnMonthChanged write FOnMonthChanged;
    property OnYearChanged: TAdvSmoothCalendarYearChangedEvent read FOnYearChanged write FOnYearChanged;
    property OnYearRangeChanged: TAdvSmoothCalendarYearRangeChangedEvent read FOnYearRangeChanged write FOnYearRangeChanged;
    property OnCurrentDayClick: TAdvSmoothCalendarCurrentDayClickEvent read FOnCurrentDayClick write FOnCurrentDayClick;
    property OnGetHeaderText: TAdvSmoothCalendarGetTextEvent read FOnGetHeaderText write FOnGetHeaderText;
    property OnGetFooterText: TAdvSmoothCalendarGetTextEvent read FOnGetFooterText write FOnGetFooterText;
    property OnGetWeekDayName: TAdvSmoothCalendarGetWeekDayNameEvent read FOnGetWeekDayName write FOnGetWeekDayName;
    property OnGetMonthName: TAdvSmoothCalendarGetMonthNameEvent read FOnGetMonthName write FOnGetMonthName;
    property OnGetYearName: TAdvSmoothCalendarGetYearNameEvent read FOnGetYearName write FOnGetYearName;

    property OnMonthFill: TAdvSmoothCalendarNormalDateFillEvent read FOnMonthFill write FOnMonthFill;
    property OnYearFill: TAdvSmoothCalendarNormalDateFillEvent read FOnYearFill write FOnYearFill;

    property Format: String read FFormat write SetFormat;

    property Calendar: TAdvSmoothDatePickerCalendar read FCalendar write SetCalendar;
    property Date: TDateTime read GetDate write SetDate;
  end;

implementation

{$I DELPHIXE.INC}

function GetDateFromString(v: Variant): TDateTime;
begin
  Result := Now;
  try
    if not VarIsNull(v) and not VarIsEmpty(v) then
    begin
      try
        Result := StrToDate(v)
      except
        Result := VarToDateTime(v)
      end;
    end
  except
    Result := Now;
  end;
end;

{ TAdvSmoothDatePicker }

procedure TAdvSmoothDatePicker.DropDown;
var
  CalPos: TPoint;
  r: TRect;
  dt: TDateTime;
  d,m,y: Word;
  TopMostWindow: TControl;

  function Min(a,b: Integer): Integer;
  begin
    if (a > b) then
      Result := b
    else
      Result := a;
  end;

  function CheckDate(dt: TDateTime): TDateTime;
  begin
    Result := dt;
  end;

  function GetParentWnd: HWnd;
  var
    Last, P: HWnd;
  begin
    Result := 0;
    if Owner <> nil then
    begin
      P := GetParent((Owner as TWinControl).Handle);
      Last := P;
      while P <> 0 do
      begin
        Last := P;
        P := GetParent(P);
      end;
      Result := Last;
    end;
  end;

begin
 TopMostWindow := Parent;
 while (TopMostWindow <> nil) and (TopMostWindow.Parent <> nil) do
   TopMostWindow := TopMostWindow.Parent;

 if (TopMostWindow is TForm) then
 begin
    if (TopMostWindow as TForm).FormStyle = fsStayOnTop then
      CalParent.FormStyle := fsStayOnTop;
  end
  else
    CalParent.FormStyle := fsStayOnTop;

  CalPos.x := -2;
  CalPos.y := Height - 3;
  CalPos := ClientToScreen(CalPos);

  {$IFNDEF TMSDOTNET}
  SystemParametersInfo(SPI_GETWORKAREA, 0,@r,0); //account for taskbar...
  {$ENDIF}
  {$IFDEF TMSDOTNET}
  SystemParametersInfo(SPI_GETWORKAREA, 0,r,0); //account for taskbar...
  {$ENDIF}

  Cal.Footer.Assign(Calendar.Footer);
  Cal.DateAppearance.Assign(Calendar.DateAppearance);
  Cal.StatusAppearance.Assign(Calendar.StatusAppearance);
  Cal.Header.Assign(Calendar.Header);
  Cal.Fill.Assign(Calendar.Fill);
  Cal.Width := Calendar.Width;
  Cal.Height := Calendar.Height;
  Cal.MaxDate := Calendar.MaxDate;
  Cal.MinDate := Calendar.MinDate;
  Cal.ShowCurrentDate := Calendar.ShowCurrentDate;
  Cal.Animation := Calendar.Animation;
  Cal.ShowHint := Calendar.ShowHint;

  if (CalPos.y + FCal.Height > r.Bottom) then
    CalPos.Y := CalPos.Y - FCal.Height - Height + 3;

  if (CalPos.x + FCal.Width > r.right) then
    CalPos.x := CalPos.x - (FCal.Width - Width);

  // Set calendar date
  try
    FIgnoreSelect := true;
    if (Text = '') then
      dt := Now
    else
      dt := Trunc(GetDateFromString(Text));
    FIgnoreSelect := false;
  except
    on Exception do
    begin
      Text := 'exception';
      Exit;
    end;
  end;

  FCal.SelectedDate := dt;

  CalParent.Width := 0;
  CalParent.Height := 0;

  DecodeDate(FCal.SelectedDate, y, m, d);

  FCal.Year := y;
  FCal.Month := m;

  CalParent.Show;

  CalParent.Left := CalPos.x;
  CalParent.Top := CalPos.y;
  CalParent.Width := FCal.Width;
  CalParent.Height := FCal.Height;

  FCal.SetFocus;
  SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);
end;

procedure TAdvSmoothDatePicker.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothDatePicker.SetCalendar(
  const Value: TAdvSmoothDatePickerCalendar);
begin
  if FCalendar <> value then
  begin
    FCalendar := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDatePicker.SetColorTones(ATones: TColorTones);
begin
  cal.SetColorTones(ATones);
  Calendar.Footer.Assign(Cal.Footer);
  Calendar.DateAppearance.AssignVisuals(Cal.DateAppearance);
  Calendar.StatusAppearance.Assign(Cal.StatusAppearance);
  Calendar.Header.Assign(Cal.Header);
  Calendar.Fill.Assign(Cal.Fill);
  Calendar.Animation := Cal.Animation;
  Calendar.MaxDate := Cal.MaxDate;
  Calendar.MinDate := Cal.MinDate;
  Calendar.ShowCurrentDate := Cal.ShowCurrentDate;
  Calendar.ShowHint := Cal.ShowHint;
end;

procedure TAdvSmoothDatePicker.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  case AStyle of
    tsOffice2003Blue: Cal.SetComponentStyle(tsOffice2003Blue);
    tsOffice2003Olive: Cal.SetComponentStyle(tsOffice2003Olive);
    tsOffice2003Silver: Cal.SetComponentStyle(tsOffice2003Silver);
    tsOffice2003Classic: Cal.SetComponentStyle(tsOffice2003Classic);
    tsOffice2007Luna: Cal.SetComponentStyle(tsOffice2007Luna);
    tsOffice2007Obsidian: Cal.SetComponentStyle(tsOffice2007Obsidian);
    tsOffice2007Silver: Cal.SetComponentStyle(tsOffice2007Silver);
    tsWindowsXP: Cal.SetComponentStyle(tsWindowsXP);
    tsWhidbey: Cal.SetComponentStyle(tsWhidbey);
    tsWindowsVista: Cal.SetComponentStyle(tsWindowsVista);
    tsWindows7: Cal.SetComponentStyle(tsWindows7);
    tsTerminal: Cal.SetComponentStyle(tsTerminal);
    tsOffice2010Blue: Cal.SetComponentStyle(tsOffice2010Blue);
    tsOffice2010Silver: Cal.SetComponentStyle(tsOffice2010Silver);
    tsWindows8: Cal.SetComponentStyle(tsWindows8);
    tsOffice2013White: Cal.SetComponentStyle(tsOffice2013White);
    tsOffice2013LightGray: Cal.SetComponentStyle(tsOffice2013LightGray);
    tsOffice2013Gray: Cal.SetComponentStyle(tsOffice2013Gray);
    tsWindows10: Cal.SetComponentStyle(tsWindows10);
    tsOffice2016White: Cal.SetComponentStyle(tsOffice2016White);
    tsOffice2016Gray: Cal.SetComponentStyle(tsOffice2016Gray);
    tsOffice2016Black: Cal.SetComponentStyle(tsOffice2016Black);
  end;

  Calendar.Footer.Assign(Cal.Footer);
  Calendar.DateAppearance.AssignVisuals(Cal.DateAppearance);
  Calendar.StatusAppearance.Assign(Cal.StatusAppearance);
  Calendar.Header.Assign(Cal.Header);
  Calendar.Fill.Assign(Cal.Fill);
  Calendar.Animation := Cal.Animation;
  Calendar.MaxDate := Cal.MaxDate;
  Calendar.MinDate := Cal.MinDate;
  Calendar.ShowCurrentDate := Cal.ShowCurrentDate;
  Calendar.ShowHint := Cal.ShowHint;
end;

procedure TAdvSmoothDatePicker.BtnClick(Sender: TObject);
begin
  CancelThisBtnClick := False;

  inherited;

  if CancelThisBtnClick then
    Exit;

  if FDeactivating then
  begin
    FDeactivating := false;
    Exit;
  end;

  if Assigned(CalParent) then
  begin
    if CalParent.Visible then
    begin
      FDeactivating := true;
      CalParent.Hide;
      Exit;
    end
    else
      DropDown;
  end
  else
    DropDown;
end;

procedure TAdvSmoothDatePicker.CancelBtnClick;
begin
  CancelThisBtnClick := True;
end;

constructor TAdvSmoothDatePicker.Create(AOwner: TComponent);
begin
  inherited;
  Text := '';
  CalParent := TDropDownForm.CreateNew(Self,0);
  CalParent.BorderStyle := bsNone;

  CalParent.Width := 0;
  CalParent.Height := 0;

  FCal := TAdvSmoothCalendar.Create(Self);
  FCal.Parent := CalParent;
  FCal.KeyBoardDateModeToggle := false;
  FCal.MultiSelect := false;
  FCal.Name := self.Name +'cal_';
  FCal.TabStop := true;
  FCal.ShowFocus := true;
  FIgnoreSelect := false;

  FCalendar := TAdvSmoothDatePickerCalendar.Create(Self);
  CalParent.OnDeactivate := CalParentDeactivate;
  Width := 108;
  FHideCalendarAfterSelection := True;
  Button.Glyph.Handle := LoadBitmap(0, MakeIntResource(OBM_COMBO));
  Button.FocusControl := nil;
  ButtonStyle := bsDropDown;
  Date := Round(Now);

  TabStop := true;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);

  FTimer := TTimer.Create(self);
  FTimer.Enabled := false;
  FTimer.OnTimer := TimerEvent;
  FTimer.Interval := 100;
end;

procedure TAdvSmoothDatePicker.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

destructor TAdvSmoothDatePicker.Destroy;
begin
  FTimer.Free;
  FCalendar.Free;
  FCal.Free;
  CalParent.Free;
  inherited;
end;

procedure TAdvSmoothDatePicker.TimerEvent(Sender: TObject);
begin
  FDeactivating := false;
  FTimer.Enabled :=false;
end;

procedure TAdvSmoothDatePicker.HideParent;
begin
  if (csDesigning in ComponentState) then
    Exit;

  FDeactivating := false;
  if CalParent.HandleAllocated then
  begin
    CalParent.Hide;
    try
      if Canfocus and enabled then
        SetFocus;
    except
    end;
  end;
end;

procedure TAdvSmoothDatePicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  s:string;
  ss: integer;
  ssl: integer;
  dfus: boolean;
begin
  inherited;
  if (key = VK_DOWN) and (ssAlt in Shift) then
  begin
    if CalParent.Visible then
      HideParent
    else
      BtnClick(Self);
    Exit;
  end;

//  if (key = VK_F4) and not (ssAlt in Shift) and not (ssCtrl in Shift) then
//  begin
//    if CalParent.Visible then
//      HideParent
//    else
//      BtnClick(Self);
//  end;

  if FFormat <> '' then
    dfus := pos('M',Uppercase(FFormat)) < pos('D',Uppercase(FFormat))
  else
    dfus := pos('M',Uppercase(ShortDateFormat)) < pos('D',Uppercase(ShortDateFormat));

  case key of
  VK_DOWN:
    begin
      s := Text;
      ss := SelStart;
      ssl := SelStart;
      if (ss > pos(DateSeparator,s)) then
      begin
        ss := ss - pos(DateSeparator,s);
        Delete(s,1,pos(DateSeparator,s));
        if (ss > pos(DateSeparator,s)) then
        begin
          FCal.SelectedDate := Date;
          Date := FCal.SelectedDate;
        end
        else
        begin
          FCal.SelectedDate := Date;

          if dfus then
            Date := Date -1
          else
          begin
            Date := FCal.SelectedDate;
          end;
        end;
      end
      else
      begin
        if dfus then
        begin
          Date := FCal.SelectedDate;
        end
        else
          Date := Date - 1;
      end;

      SelStart := ssl;

    end;
  VK_UP:
    begin
      s := Text;
      ss := SelStart;
      ssl := SelStart;
      if (ss > pos(DateSeparator,s)) then
      begin
        ss := ss - pos(DateSeparator,s);
        Delete(s,1,pos(DateSeparator,s));
        if (ss > pos(DateSeparator,s)) then
        begin
          FCal.SelectedDate := Date;
          Date := FCal.SelectedDate;
        end
        else
        begin
          FCal.SelectedDate := Date;
          if dfus then
            Date := Date + 1
          else
          begin
            Date := FCal.SelectedDate;
          end;
        end;
      end
      else
      begin
        if dfus then
        begin
          Date := FCal.SelectedDate;
        end
        else
          Date := Date + 1;
      end;

      SelStart := ssl;
    end;
  end;
end;

procedure TAdvSmoothDatePicker.InitEvents;
begin
  FCal.OnKeyPress := CalendarKeypress;
  FCal.OnKeyUp := CalendarKeyUp;
  FCal.OnKeyDown := CalendarKeyDown;
  FCal.OnSelectDate := CalendarDaySelect;
  FCal.OnDateFill := CalendarDateFill;
  FCal.OnDateHint := CalendarDateHint;
  FCal.OnDateStatus := CalendarDateStatus;
  FCal.OnDateModeChanged := CalendarChangeMode;
  FCal.OnMonthChanged := CalendarChangeMonth;
  FCal.OnYearChanged := CalendarChangeYear;
  FCal.OnYearRangeChanged := CalendarChangeYearRange;
  FCal.OnCurrentDayClick := CalendarCurrentDateClick;
  FCal.OnGetHeaderText := CalendarGetHeaderText;
  FCal.OnGetFooterText := CalendarGetFooterText;
  FCal.OnGetWeekDayName := CalendarGetWeekDayName;
  FCal.OnGetMonthName := CalendarGetMonthName;
  FCal.OnGetYearName := CalendarGetYearName;
  FCal.OnMonthFill := CalendarMonthFill;
  FCal.OnYearFill := CalendarYearFill;
end;

function TAdvSmoothDatePicker.IsDropDownVisible: Boolean;
begin
  Result := false;
  if Assigned(CalParent) then
    Result := CalParent.Visible;
end;

procedure TAdvSmoothDatePicker.Loaded;
begin
  inherited;
  InitEvents;
  Button.Enabled := not ReadOnly;
end;

procedure TAdvSmoothDatePicker.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothDatePicker.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothDatePicker.CalendarChangeMode(Sender: TObject; Mode,
  ModeTo: TAdvSmoothCalendarDateMode; var allow: Boolean);
begin
  if Assigned(FOnDateModeChanged) then
    FOnDateModeChanged(Self, Mode, ModeTo, Allow);
end;

procedure TAdvSmoothDatePicker.CalendarChangeMonth(Sender: TObject;
  Month: integer);
begin
  if Assigned(FOnMonthChanged) then
    FOnMonthChanged(Self, Month);
end;

procedure TAdvSmoothDatePicker.CalendarChangeYear(Sender: TObject;
  Year: integer);
begin
  if Assigned(FOnYearChanged) then
    FOnYearChanged(Self, Year);
end;

procedure TAdvSmoothDatePicker.CalendarChangeYearRange(Sender: TObject;
  YearFrom, YearTo: integer);
begin
  if Assigned(FOnYearRangeChanged) then
    FOnYearRangeChanged(Self, YearFrom, YearTo);
end;

procedure TAdvSmoothDatePicker.CalendarCurrentDateClick(Sender: TObject;
  var allow: Boolean);
begin
  if Assigned(FOnCurrentDayClick) then
    FOnCurrentDayClick(Self, Allow);
  Calendar.Update;
end;

procedure TAdvSmoothDatePicker.CalendarDateFill(Sender: TObject;
  AFill: TGDIPFill; AFont: TFont; Date: TDateTime;
  DateKind: TAdvSmoothCalendarDateKind);
begin
  if Assigned(FOnDateFill) then
    FOnDateFill(Self, AFill, AFont, Date, DateKind);
end;

procedure TAdvSmoothDatePicker.CalendarDateHint(Sender: TObject;
  Date: TDateTime; var hint: String);
begin
  if Assigned(FOnDateHint) then
    FOnDateHint(Self, Date, Hint);
end;

procedure TAdvSmoothDatePicker.CalendarDaySelect(Sender: TObject; Mode: TAdvSmoothCalendarDateMode; Date: TDateTime);
begin
  if FIgnoreSelect then
    Exit;

  if Mode = dmDay then
  begin
    if FFormat <> '' then
      Text := FormatDateTime(FFormat, Date)
    else
      Text := DateToStr(Date);

    if FHideCalendarAfterSelection then
    begin
      HideParent;
    end;

    if Assigned(FOnDaySelect) then
      FOnDaySelect(Self,Mode, Date);
  end;
end;

procedure TAdvSmoothDatePicker.CalendarGetFooterText(Sender: TObject;
  var AText: String);
begin
  if Assigned(OnGetFooterText) then
    OnGetFooterText(Sender, AText);
end;

procedure TAdvSmoothDatePicker.CalendarGetHeaderText(Sender: TObject;
  var AText: String);
begin
  if Assigned(OnGetHeaderText) then
    OnGetHeaderText(Sender, AText);
end;

procedure TAdvSmoothDatePicker.CalendarGetMonthName(Sender: TObject; Month: Integer; var MonthName: String);
begin
  if Assigned(OnGetMonthName) then
    OnGetMonthName(Sender, Month, MonthName);
end;

procedure TAdvSmoothDatePicker.CalendarGetWeekDayName(Sender: TObject; WeekDay: Integer; var WeekDayName: String);
begin
  if Assigned(OnGetWeekDayName) then
    OnGetWeekDayName(Sender, WeekDay, WeekDayName);
end;

procedure TAdvSmoothDatePicker.CalendarGetYearName(Sender: TObject; Year: Integer; var YearName: String);
begin
  if Assigned(OnGetYearName) then
    OnGetYearName(Sender, Year, YearName);
end;

procedure TAdvSmoothDatePicker.CalendarDateStatus(Sender: TObject;
  Date: TDateTime; var StatusMessage: String; Fill: TGDIPStatus; var OffsetX,
  OffsetY: integer);
begin
  if Assigned(FOnDateStatus) then
    FOnDateStatus(Self, Date, StatusMessage, Fill, OffsetX, OffsetY);
end;

procedure TAdvSmoothDatePicker.CalendarKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F4 then
    HideParent;

  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end;

procedure TAdvSmoothDatePicker.CalendarKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, Shift);
end;

procedure TAdvSmoothDatePicker.CalendarMonthFill(Sender: TObject; AFill: TGDIPFill; AFont: TFont; Date: TDateTime; DateKind: TAdvSmoothCalendarDateKind);
begin
  if Assigned(OnMonthFill) then
    OnMonthFill(Sender, AFill, AFont, Date, DateKind);
end;

procedure TAdvSmoothDatePicker.CalendarYearFill(Sender: TObject; AFill: TGDIPFill; AFont: TFont; Date: TDateTime; DateKind: TAdvSmoothCalendarDateKind);
begin
  if Assigned(OnYearFill) then
    OnYearFill(Sender, AFill, AFont, Date, DateKind);
end;

procedure TAdvSmoothDatePicker.CalendarKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);

  if (Key = char(VK_RETURN)) or (Key = char(VK_SPACE)) then
    CalendarDaySelect(Sender, dmDay, FCal.SelectedDate);

  if Key = #27 then
    HideParent;
end;

procedure TAdvSmoothDatePicker.CalParentDeactivate(Sender: TObject);
begin
  FDeactivating := true;
  (Sender as TForm).Hide;
  FTimer.Enabled := true;
end;

procedure TAdvSmoothDatePicker.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end;

procedure TAdvSmoothDatePicker.WMSetFocus(var Message: TWMSetFocus);
begin
  if EditorEnabled then
    inherited
  else
    Button.SetFocus;
end;

procedure TAdvSmoothDatePicker.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothDatePicker.Change;
var
  dt: TDateTime;
begin
// try to extract the date
  try
    if (Text = '') then
    begin
      FIgnoreSelect := true;
      Cal.SelectedDate := 0;
      FIgnoreSelect := false;
      if (not AllowNumericNullValue) and (EmptyText = '') then
      begin
        if FFormat <> '' then
          Text := FormatDateTime(FFormat, Now)
        else
          Text := DateToStr(Now);
      end;
    end
    else
    begin
      TryStrToDate(Text,dt);
      FIgnoreSelect := true;
      try
        Cal.SelectedDate := dt;
      finally
        FIgnoreSelect := false;
      end;
    end;
  except
  end;

  if not (csLoading in ComponentState) then
    inherited;
end;

procedure TAdvSmoothDatePicker.CreateWnd;
begin
  inherited;
  InitEvents;
end;

function TAdvSmoothDatePicker.GetParentEx: TWinControl;
begin
  Result := inherited Parent;
end;

function TAdvSmoothDatePicker.GetThemeID: String;
begin
  Result := ClassName;
end;

procedure TAdvSmoothDatePicker.SetParentEx(const Value: TWinControl);
begin
  inherited Parent := Value;
  InitEvents;
end;

function TAdvSmoothDatePicker.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothDatePicker.GetDate: TDateTime;
begin
  if Text <> '' then
    Result := Int(FCal.SelectedDate) + Frac(FTime)
  else
    Result := Frac(FTime);
end;

procedure TAdvSmoothDatePicker.SetDate(const Value: TDateTime);
begin
  if FCal.SelectedDate <> Value then
    FCal.SelectedDate := Value;

  FTime := Frac(Value);
  if Value = 0 then
    Text := ''
  else
  begin
    if FFormat <> '' then
      Text := FormatDateTime(FFormat, Value)
    else
      Text := DateToStr(Value);
  end;
end;

procedure TAdvSmoothDatePicker.SetFormat(const Value: String);
begin
  if FFormat <> value then
  begin
    FFormat := Value;
    Date := Date;
    Changed;
  end;
end;

function TAdvSmoothDatePicker.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

{ TAdvSmoothDatePickerCalendar }

procedure TAdvSmoothDatePickerCalendar.AnimationChanged(Sender: TObject);
begin
  FOwner.Fcal.Animation := Animation;
end;

procedure TAdvSmoothDatePickerCalendar.Assign(Source: TPersistent);
begin
  Fill.Assign((Source as TAdvSmoothDatePickerCalendar).Fill);
  DateAppearance.Assign((Source as TAdvSmoothDatePickerCalendar).DateAppearance);
  Footer.Assign((Source as TAdvSmoothDatePickerCalendar).Footer);
  Header.Assign((Source as TAdvSmoothDatePickerCalendar).Header);
  Width := (Source as TAdvSmoothDatePickerCalendar).Width;
  Height := (Source as TAdvSmoothDatePickerCalendar).Height;
  Animation := (Source as TadvSmoothDatePickerCalendar).Animation;
  ShowCurrentDate := (Source as TAdvSmoothDatePickerCalendar).ShowCurrentDate;
  MaxDate := (Source as TAdvSmoothDatePickerCalendar).MaxDate;
  MinDate := (Source as TAdvSmoothDatePickerCalendar).MinDate;
  StatusAppearance.Assign((source as TAdvSmoothDatePickerCalendar).StatusAppearance);
  ShowHint := (Source as TAdvSmoothDatePickerCalendar).ShowHint;
end;

constructor TAdvSmoothDatePickerCalendar.Create(AOwner: TAdvSmoothDatePicker);
begin
  FOwner := AOwner;
  FFill := TGDIPFill.Create;
  FHeader := TAdvSmoothCalendarHeader.Create(FOwner.FCal);
  FFooter := TAdvSmoothCalendarFooter.Create(Fowner.FCal);
  FDateAppearance := TAdvSmoothCalendarDateAppearance.Create(FOwner.FCal);
  FWidth := FOwner.FCal.Width;
  FHeight := FOwner.FCal.Height;
  FMaxDate := FOwner.Fcal.MaxDate;
  FMinDate := FOwner.Fcal.MinDate;
  FAnimation := FOwner.Fcal.Animation;
  FShowCurrentDate := FOwner.Fcal.ShowCurrentDate;
  FStatusAppearance := TGDIPStatus.Create;
  FShowHint := FOwner.Fcal.ShowHint;
end;

procedure TAdvSmoothDatePickerCalendar.DateAppearanceChanged(Sender: TObject);
begin
  FOwner.FCal.DateAppearance.Assign(DateAppearance);
end;

destructor TAdvSmoothDatePickerCalendar.Destroy;
begin
  FFill.Free;
  FHeader.Free;
  FFooter.Free;
  FDateAppearance.Free;
  FStatusAppearance.Free;
  inherited;
end;

procedure TAdvSmoothDatePickerCalendar.FillChanged(Sender: TObject);
begin
  FOwner.FCal.Fill.Assign(Fill);
end;

procedure TAdvSmoothDatePickerCalendar.FooterChanged(Sender: TObject);
begin
  FOwner.FCal.Footer.Assign(Footer);
end;

procedure TAdvSmoothDatePickerCalendar.HeaderChanged(Sender: TObject);
begin
  FOwner.FCal.Header.Assign(Header);
end;

procedure TAdvSmoothDatePickerCalendar.HeightChanged(Sender: TObject);
begin
  FOwner.FCal.Height := Height;
end;

procedure TAdvSmoothDatePickerCalendar.MaxDateChanged(Sender: TObject);
begin
  FOwner.FCal.MaxDate := MaxDate;
end;

procedure TAdvSmoothDatePickerCalendar.MinDateChanged(Sender: TObject);
begin
  FOwner.FCal.MinDate := MinDate;
end;

procedure TAdvSmoothDatePickerCalendar.ShowCurrentDateChanged(Sender: TObject);
begin
  FOwner.FCal.ShowCurrentDate := ShowCurrentDate;
end;

procedure TAdvSmoothDatePickerCalendar.ShowHintChanged(Sender: TObject);
begin
  FOwner.FCal.ShowHint := ShowHint;
end;

procedure TAdvSmoothDatePickerCalendar.StatusAppearanceChanged(Sender: TObject);
begin
  FOwner.Fcal.StatusAppearance.Assign(StatusAppearance);
end;

procedure TAdvSmoothDatePickerCalendar.Update;
begin
  FOwner.FCal.Footer.Assign(FOwner.Calendar.Footer);
  FOwner.FCal.DateAppearance.Assign(FOwner.Calendar.DateAppearance);
  FOwner.FCal.StatusAppearance.Assign(FOwner.Calendar.StatusAppearance);
  FOwner.FCal.Header.Assign(FOwner.Calendar.Header);
  FOwner.FCal.Fill.Assign(FOwner.Calendar.Fill);
  FOwner.FCal.Width := FOwner.Calendar.Width;
  FOwner.FCal.Height := FOwner.Calendar.Height;
  FOwner.FCal.MaxDate := FOwner.Calendar.MaxDate;
  FOwner.FCal.MinDate := FOwner.Calendar.MinDate;
  FOwner.FCal.ShowCurrentDate := FOwner.Calendar.ShowCurrentDate;
  FOwner.FCal.Animation := FOwner.Calendar.Animation;
  FOwner.FCal.ShowHint := FOwner.Calendar.ShowHint;
end;

procedure TAdvSmoothDatePickerCalendar.WidthChanged(Sender: TObject);
begin
  FOwner.FCal.Width := Width;
end;

{ TDropDownForm }

procedure TDropDownForm.CreateParams(var Params: TCreateParams);
var
  f: TCustomForm;
begin
  inherited CreateParams(Params);

  if Assigned(Owner) then
    f := GetParentForm(Owner as TWinControl)
  else
    f := nil;

  if Assigned(f) then
    Params.WndParent := f.Handle;

end;

end.
