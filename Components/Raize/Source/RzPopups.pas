{===============================================================================
  RzPopups Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzCalendar
    Calendar panel that displays dates in month view (Office-style)

  TRzTimePicker
    Custom panel that displays a clock and allows a user to select a time using
    left and right mouse buttons.

  TRzCalculator
    Custom panel that displays a simple calculator with standard arithmetic
    operations.


  Modification History
  ------------------------------------------------------------------------------
  6.1.10 (05 Sep 2014)
    * Fixed parsing issue in TRzDateTimeEdit and TRzDBDateTimeEdit when a short
      date format of YMD order is defined for the current locale in Windows.
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * Fixed issue in popup controls (TRzDateTimeEdit, TRzColorEdit) where if
      the popup control is on a container that has its origin shifted above the
      origin of the parent form (and AutoScroll is True) the vertical scroll
      bar of the form gets shifted when the popup is invoked.
    * Modified TRzTimePicker to use HintWindowClass instead of THintWindow
      directly to display custom hint.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Fixed display positioning of separator lines on the TRzCalendar.
    * TRzTimePicker now displays correct themed radio buttons for AM and PM.
    * Made necessary modifications to TRzCalendar, TRzTimePicker, and
      TRzCalculator to fully support VCL Styles introduced in RAD Studio XE2.
  ------------------------------------------------------------------------------
  5.5    (06 Mar 2011)
    * The TRzCalendar control has been updated such that it will hide the focus
      rectangle until the user navigates on the form using the keyboard. The
      control honors the Windows system setting that affects this behavior.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Fixed issue where pressing the keypad decimal key would not insert the
      DecimalSeparator character (based on user locale settings) in the
      TRzCalculator if the DecimalSeparator was something other than a period.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Fixed border display problem in TRzCalendar, TRzTimePicker, and
      TRzCalculator when running under older versions of Delphi.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * Fixed border display problem in TRzCalendar, TRzTimePicker, and
      TRzCalculator.
    * Fixed button positioning issue in the TRzCalculator.
    * Changed default button colors of TRzCalculator to clBtnFace.
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzCalendar, TRzTimePicker, and TRzCalculator controls.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added the new TRzCalculator component.  This TRzCustomPanel descendant
      provides an interface to perform simply math calculations. Integer and
      floating point numbers are supported.  The calculator is designed to allow
      for continuous calculations.  (For example, 10 + 20 + 30 - 25 =)
      Button colors are customizable based on functionality through the
      CalculatorColors property. The control is also fully theme aware.
  ------------------------------------------------------------------------------
  4.3    (13 Sep 2007)
    * Updated TRzPopupPanel.Popup to handle situations where MonitorFromPoint
      returns nil.
    * Fixed issue in TRzCalendar that would cause a conversion exception to be
      raised when changing the Year spinner to year 1 or 0.
    * The TRzCalendar now monitors the current date, and when the date changes,
      the display is updated to reflect the new value for "Today".
    * The TRzCalendar.OnGetDayFormat event now only fires for days that are
      visible in the calendar. For example, if ceFillDays is removed from the
      Elements set, then the OnGetDayFormat event is not raised for the fill
      dates as it was in previous versions.
    * The Today and Clear buttons are now swapped when running under
      right-to-left (RTL) systems.
    * Abbreviations for day names when running on Hebrew systems has been
      updated to reflect that all Hebrew day names have the same prefix.
  ------------------------------------------------------------------------------
  4.1.2  (17 Apr 2007)
    * Added overloaded version of TRzPopupPanel.Popup to allow custom
      positioning of the popup on the control.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Changed the mechanism by which users change the month and year in the
      TRzCalendar control. The user changes the month by clicking on the month
      name and selecting a new month from the popup menu that is displayed. To
      change the year, the user clicks on the year, which results in a spinner
      being displayed allowing the user to quickly change the year.
    * Added new MinDate and MaxDate properties to TRzCalendar to allow for
      specifying a range of valid dates that can be selected by the user.
    * Added the new OnGetDayFormat event to the TRzCalendar. This event allows
      the individual days of the calendar display to be colored and styled
      differently than the calendar's default formatting.  This new event should
      be used in place of the OnGetBoldDays event.  The OnGetBoldDays event is
      still available, but the new OnGetDayFormat event is much more flexible
      and powerful.
    * Adjusted the drawing of Days of the Week header. In the new approach, if
      the width of the display is wide enough, the full short name (based on
      locale) is displayed. If the display is not wide enough, then the short
      names are shortened to 1 or 2 characters to accomodate the size. Also, the
      font size is reduced slightly for the day of week short names.
    * Fixed problem where bold-days bitmasks were not correctly applied to the
      previous and next months in a the current view.
    * Added new OnToday and OnClear events to the TRzCalendar. These events
      occur when the corresponding button is click, or the Today or Clear method
      is called, respectively.
    * Surfaced FrameController and FrameControllerNotifications properties in
      TRzCalendar and TRzTimePicker.
    * Updated the display of the TRzCalendar and TRzTimePicker when used with
      Windows XP Themes.
    * Added ThemeAware property to TRzCalendar and TRzTimePicker.
    * Fixed display problem in TRzCalendar and TRzTimePicker that would occur
      when used within a TRzDateTimeEdit and the FocusColor property was set to
      a different value from the Color property.
  ------------------------------------------------------------------------------
  4.0.1  (07 Jan 2006)
    * Fixed problem where radio buttons in TRzTimePicker for selecting AM and PM
      were not visible.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed problem where clicking the Clear button in the Calendar when used
      by a TRzDateTimeEdit would not set the underlying database field to NULL.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Added ShowDays property to TRzCalendar. This allows the days in the
      calendar to be hidden, which is useful if one only requires month and year
      selection.
    * The TRzTimePicker has been modified so that the hour hand is displayed in
      its true time position. That is, in-between hours as the minutes change.
      This change makes the TRzTimePicker suitable for displaying a clock and
      not just for selecting times.
    * As the user moves the minute hand of the clock the hour will also change
      as the user moves the minute hand past the top of the hour.
    * Added new ShowTime property to TRzTimePicker. When set to False, the
      time in digital format is no longer visible at the top of the control.
    * Added new RestrictMinutesBy property to control the amount of minutes
      the time is altered when the Control key is pressed as the user changes
      the minute hand. The hint that is displayed is also updated to reflect
      any changes to the FRestrictMinutesBy value.
    * Added new ButtonColor and ButtonFontColor to both the TRzCalendar and the
      TRzTimePicker controls. These properties control the appearance of the
      buttons and header area that appear in these controls.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem where double-clicking on a TRzCalendar would cause an
      exception if the control was hidden as a result of code executed in the
      event handler.
    * Fixed issue where Month arrows would not inc/dec month values correctly
      when running under an RTL system.
    * Added OnViewDateChange event to TRzCalendar, which fires when the user
      changes the month (i.e. view) of the calendar.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Enhanced StrToDateEx procedure to accept 8-digit strings (e.g. MMDDYYYY)
      without separators and correctly convert it into an actual date.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Fixed problem where TRzCalendar.OnDblClick would never get fired.
    * Added OnGetWeekNumber event to TRzCalendar. Handle this event to implement
      a customized week numbering scheme.
    * Added Format property to TRzTimePicker. This property controls the format
      of the time that is displayed at the top of the control.
    * Set default values for TRzCalendar.CalendarColors and
      TRzTimePicker.ClockFaceColors.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Made change to TRzPopup.Popup method so that when a popup window is
      displayed by a control, the CPU utilization no longer hits 100%.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * Fixed problem where StrToDateEx would try to interpret the year in a date
      specifying a month name (e.g. April 11, 2003) as a month/year combintation
      without a date-separator. This lead to the entire date being considered
      invalid, when in fact it was a valid date.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Fixed problem where popups were not being displayed on the appropriate
      monitor when running under a system supporting multiple monitors.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Added keyboard support for TRzCalendar and TRzTimePicker.
    * HowToUseHint is positioned at bottom of TRzTimePicker. Also, a timer is
      used to remove the hint if the mouse is moved outside the clock face and
      the hint is still visible, which can happen if the user moves the mouse
      quickly.
    * Fixed display of Time header in TRzTimePicker so that it completely fills
      client region when other border styles are used.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzPopups;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Menus,
  StdCtrls,
  ExtCtrls,
  RzCommon,
  RzPanel;

type
  TRzCalendar = class;

  {=========================================}
  {== TRzCalendarColors Class Declaration ==}
  {=========================================}

  TRzCalendarColors = class( TPersistent )
  private
    FDays: TColor;
    FFillDays: TColor;
    FDaysOfWeek: TColor;
    FLines: TColor;
    FSelectedDateBack: TColor;
    FSelectedDateFore: TColor;
    FTodaysDateFrame: TColor;

    FCalendar: TRzCalendar;
  protected
    { Property Access Methods }
    function GetColor( Index: Integer ): TColor; virtual;
    procedure SetColor( Index: Integer; Value: TColor ); virtual;
  public
    constructor Create( ACalendar: TRzCalendar );
    destructor Destroy; override;

    procedure Assign( Source: TPersistent ); override;

    { Property Declarations }
    property Calendar: TRzCalendar
      read FCalendar;

    property Colors[ Index: Integer ]: TColor
      read GetColor
      write SetColor;
  published
    property Days: TColor
      index 0
      read GetColor
      write SetColor
      default clWindowText;

    property FillDays: TColor
      index 1
      read GetColor
      write SetColor
      default clBtnShadow;

    property DaysOfWeek: TColor
      index 2
      read GetColor
      write SetColor
      default clWindowText;

    property Lines: TColor
      index 3
      read GetColor
      write SetColor
      default clBtnShadow;

    property SelectedDateBack: TColor
      index 4
      read GetColor
      write SetColor
      default clHighlight;

    property SelectedDateFore: TColor
      index 5
      read GetColor
      write SetColor
      default clHighlightText;

    property TodaysDateFrame: TColor
      index 6
      read GetColor
      write SetColor
      default clMaroon;
  end;


  {===========================================}
  {== TRzCalendar Support Types Declaration ==}
  {===========================================}

  TRzCalendarElement = ( ceYear, ceMonth, ceArrows, ceWeekNumbers, ceDaysOfWeek,
                         ceFillDays, ceTodayButton, ceClearButton );
  TRzCalendarElements = set of TRzCalendarElement;

  TRzCalendarArea = ( caYear, caMonth, caLeftArrow, caRightArrow, caWeekNumbers,
                      caDays, caDaysOfWeek, caFillDays,
                      caTodayButton, caClearButton );
  TRzCalendarAreas = array[ TRzCalendarArea ] of TRect;

  TRzFirstDayOfWeek = ( fdowMonday, fdowTuesday, fdowWednesday, fdowThursday, fdowFriday,
                        fdowSaturday, fdowSunday, fdowLocale );

  TRzGetBoldDaysEvent = procedure( Sender: TObject; Year, Month: Word;
                                   var Bitmask: Cardinal ) of object;
  TRzGetDayFormatEvent = procedure( Sender: TObject; DayDate: TDateTime;
                                    Year, Month, Day: Word;
                                    var DayColor, DayFontColor: TColor;
                                    var DayFontStyle: TFontStyles ) of object;

  TRzGetWeekNumberEvent = procedure( Sender: TObject; WeekDate: TDateTime; var WeekNumber: Integer ) of object;

  TRzViewDateChangeEvent = procedure( Sender: TObject; ViewDate: TDateTime ) of object;

  TRzInvalidDateEvent = procedure( Sender: TObject; var KeepFocused, KeepInvalidText: Boolean;
                                   var NewDate: TDateTime ) of object;
  TRzInvalidTimeEvent = procedure( Sender: TObject; var KeepFocused, KeepInvalidText: Boolean;
                                   var NewTime: TDateTime ) of object;

  TRzCalendarTodayEvent = procedure( Sender: TObject; var AllowChange: Boolean ) of object;
  TRzCalendarClearEvent = procedure( Sender: TObject; var AllowClear: Boolean ) of object;

  ERzCalendarError = class( Exception );

  {===================================}
  {== TRzCalendar Class Declaration ==}
  {===================================}

  TRzCalendar = class( TRzCustomPanel )
  private
    FFirstDay: Byte;
    FFirstDayOfWeek: TRzFirstDayOfWeek;
    FElements: TRzCalendarElements;
    FPressedArea: TRzCalendarArea;
    FOverArea: TRzCalendarArea;
    FShowDays: Boolean;

    FIsPopup: Boolean;
    FMouseOverRect: TRect;
    FCharSize: TPoint;
    FCellSize: TPoint;

    FDate: TDateTime;
    FMinDate: TDateTime;
    FMaxDate: TDateTime;

    FTodayTimer: TTimer;
    FToday: TDateTime;
    FViewDate: TDateTime;
    FFirstDateInGrid: TDateTime;
    FForceUpdate: Boolean;
    FClearClicked: Boolean;
    FIgnoreClick: Boolean;

    FButtonColor: TColor;
    FButtonFontColor: TColor;
    FCaptionClearBtn: TCaption;
    FClearBtnWidth: Integer;
    FCaptionTodayBtn: TCaption;
    FTodayBtnWidth: Integer;

    FCalendarColors: TRzCalendarColors;
    FCounter: Integer;
    FMonthPopupMenu: TPopupMenu;
    FYearSpinner: TWinControl;
    FYearSpinnerVisible: Boolean;
    FThemeAware: Boolean;

    FOnChange: TNotifyEvent;
    FOnGetBoldDays: TRzGetBoldDaysEvent;
    FOnGetDayFormat: TRzGetDayFormatEvent;
    FOnGetWeekNumber: TRzGetWeekNumberEvent;
    FOnViewDateChange: TRzViewDateChangeEvent;
    FOnToday: TRzCalendarTodayEvent;
    FOnClear: TRzCalendarClearEvent;

    function StoreMinDate: Boolean;
    function StoreMaxDate: Boolean;

    // Message Handling Methods
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CMDialogChar( var Msg: TCMDialogChar ); message cm_DialogChar;
    procedure WMEraseBkgnd( var Msg: TMessage ); message wm_EraseBkgnd;
    procedure WMNCHitTest( var Msg: TWMNCHitTest ); message wm_NCHitTest;
    procedure WMTimer( var Msg: TMessage ); message wm_Timer;
    procedure WMGetDlgCode( var Msg: TWMGetDlgCode ); message wm_GetDlgCode;

    // Internal Event Handlers
    procedure TodayTimerExpired( Sender: TObject );
  protected
    procedure CreateHandle; override;
    procedure CreateWnd; override;

    function ShowFocus: Boolean;

    procedure Paint; override;
    function GetButtonFontColor: TColor;

    function GetMaxShortDayNameLength: Integer;
    procedure CalcAreas( var Areas: TRzCalendarAreas );
    procedure AdjustClientRect( var Rect: TRect ); override;
    procedure AdjustForFont;
    procedure CalcFontSize;
    procedure ConstrainedResize( var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer ); override;

    procedure SetDate( Value: TDateTime ); virtual;
    function DateInRange( Value: TDateTime ): Integer;
    function ViewMonthInRange( Value: TDateTime ): Boolean;
    procedure SetRange( MinValue, MaxValue: TDate );

    procedure CreateMonthPopupMenu;
    procedure DisplayMonthPopupMenu( X, Y: Integer );
    procedure MonthPopupClickHandler( Sender: TObject );
    procedure HideYearSpinner;
    procedure ShowYearSpinner;
    procedure YearSpinnerDecrementHandler( Sender: TObject; Amount: Integer );
    procedure YearSpinnerIncrementHandler( Sender: TObject; Amount: Integer );

    procedure UpdateHighlight( X, Y: Integer );
    function InternalHitTest( R: TRect; P: TPoint ): TDateTime;

    procedure StartTimer;
    procedure TimerExpired;
    procedure StopTimer;

    // Event Dispatch Methods
    procedure Changed; dynamic;
    procedure Click; override;
    procedure DblClick; override;
    procedure GetBoldDays( Year, Month: Word; var Bitmask: Cardinal ); dynamic;
    function GetDayFormat( DayDate: TDateTime; Year, Month, Day: Word;
                           var DayColor, DayFontColor: TColor;
                           var DayFontStyle: TFontStyles ): Boolean; dynamic;

    procedure ViewDateChange( ViewDate: TDateTime ); dynamic;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;

    function CanClear: Boolean; dynamic;
    function CanGoToToday: Boolean; dynamic;

    { Property Access Methods }
    procedure SetButtonColor( Value: TColor ); virtual;
    procedure SetButtonFontColor( Value: TColor ); virtual;
    procedure SetCalendarColors( Value: TRzCalendarColors ); virtual;
    procedure SetCaptionClearBtn( const Value: TCaption ); virtual;
    procedure SetCaptionTodayBtn( const Value: TCaption ); virtual;
    procedure SetDateProperty( Value: TDateTime ); virtual;
    procedure SetMinDate( Value: TDateTime ); virtual;
    procedure SetMaxDate( Value: TDateTime ); virtual;
    procedure SetFirstDayOfWeek( Value: TRzFirstDayOfWeek ); virtual;
    procedure SetElements( Value: TRzCalendarElements ); virtual;
    procedure SetOverArea( Value: TRzCalendarArea ); virtual;
    procedure SetPressedArea( Value: TRzCalendarArea ); virtual;
    procedure SetShowDays( Value: Boolean ); virtual;
    procedure SetViewDate( Value: TDateTime ); virtual;
    procedure SetThemeAware( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function CanAutoSize( var NewWidth, NewHeight: Integer ): Boolean; override;

    function DaysToBitmask( Days: array of Byte ): Cardinal;
    function HitTest( X, Y: Integer ): TDateTime;

    function IsClear: Boolean;
    procedure Clear;
    procedure Today;

    property ViewDate: TDateTime
      read FViewDate
      write SetViewDate;

    property IsPopup: Boolean
      write FIsPopup;

    property ClearClicked: Boolean
      read FClearClicked;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property ButtonColor: TColor
      read FButtonColor
      write SetButtonColor
      default clBtnFace;

    property ButtonFontColor: TColor
      read FButtonFontColor
      write SetButtonFontColor
      default clWindowText;

    property CalendarColors: TRzCalendarColors
      read FCalendarColors
      write SetCalendarColors;

    property CaptionClearBtn: TCaption
      read FCaptionClearBtn
      write SetCaptionClearBtn;

    property CaptionTodayBtn: TCaption
      read FCaptionTodayBtn
      write SetCaptionTodayBtn;

    property Date: TDateTime
      read FDate
      write SetDateProperty;

    property MinDate: TDateTime
      read FMinDate
      write SetMinDate
      stored StoreMinDate;

    property MaxDate: TDateTime
      read FMaxDate
      write SetMaxDate
      stored StoreMaxDate;

    property FirstDayOfWeek: TRzFirstDayOfWeek
      read FFirstDayOfWeek
      write SetFirstDayOfWeek
      default fdowLocale;

    property Elements: TRzCalendarElements
      read FElements
      write SetElements
      default [ ceYear, ceMonth, ceArrows, ceFillDays, ceDaysOfWeek,
                ceTodayButton, ceClearButton ];

    property ShowDays: Boolean
      read FShowDays
      write SetShowDays
      default True;

    property ThemeAware: Boolean
      read FThemeAware
      write SetThemeAware
      default True;

    property OnChange: TNotifyEvent
      read FOnChange
      write FOnChange;

    property OnGetBoldDays: TRzGetBoldDaysEvent
      read FOnGetBoldDays
      write FOnGetBoldDays;

    property OnGetDayFormat: TRzGetDayFormatEvent
      read FOnGetDayFormat
      write FOnGetDayFormat;

    property OnGetWeekNumber: TRzGetWeekNumberEvent
      read FOnGetWeekNumber
      write FOnGetWeekNumber;

    property OnViewDateChange: TRzViewDateChangeEvent
      read FOnViewDateChange
      write FOnViewDateChange;

    property OnToday: TRzCalendarTodayEvent
      read FOnToday
      write FOnToday;

    property OnClear: TRzCalendarClearEvent
      read FOnClear
      write FOnClear;

    { Inherited Properties & Events }
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize default True;
    property BevelWidth;
    property BiDiMode;
    property BorderInner;
    property BorderOuter default fsStatus;
    property BorderSides;
    property BorderColor;
    property BorderHighlight;
    property BorderShadow;
    property BorderWidth;
    property Color default clWindow;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatColor;
    property FlatColorAdjustment default 0;
    property Font;
    property FrameController;
    property FrameControllerNotifications;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;


  {==========================================}
  {== TRzClockFaceColors Class Declaration ==}
  {==========================================}

  TRzTimePicker = class;

  TRzClockFaceColors = class( TPersistent )
  private
    FFace: TColor;
    FHands: TColor;
    FNumbers: TColor;
    FHourTicks: TColor;
    FMinuteTicks: TColor;

    FTimePicker: TRzTimePicker;
  protected
    { Property Access Methods }
    function GetColor( Index: Integer ): TColor; virtual;
    procedure SetColor( Index: Integer; Value: TColor ); virtual;
  public
    constructor Create( ATimePicker: TRzTimePicker );
    destructor Destroy; override;

    procedure Assign( Source: TPersistent ); override;

    { Property Declarations }
    property TimePicker: TRzTimePicker
      read FTimePicker;

    property Colors[ Index: Integer ]: TColor
      read GetColor
      write SetColor;
  published
    { Property Declarations }
    property Face: TColor
      index 0
      read GetColor
      write SetColor
      default clBtnFace;

    property Hands: TColor
      index 1
      read GetColor
      write SetColor
      default clWindowText;

    property Numbers: TColor
      index 2
      read GetColor
      write SetColor
      default clWindowText;

    property HourTicks: TColor
      index 3
      read GetColor
      write SetColor
      default clBtnShadow;

    property MinuteTicks: TColor
      index 4
      read GetColor
      write SetColor
      default clWindowText;
  end;


  {=====================================}
  {== TRzTimePicker Class Declaration ==}
  {=====================================}

  TRzTimePicker = class( TRzCustomPanel )
  private
    FTime: TTime;
    FTimeIsPM: Boolean;
    FRestrictMinutes: Boolean;
    FRestrictMinutesBy: Integer;
    FForceUpdate: Boolean;
    FIsPopup: Boolean;

    FTimeRect: TRect;
    FClockRect: TRect;
    FRadius: Integer;
    FClockCenter: TPoint;
    FAMRect: TRect;
    FPMRect: TRect;
    FSetRect: TRect;
    FCharSize: TPoint;

    FMouseOverAM: Boolean;
    FMouseOverPM: Boolean;
    FMouseOverSet: Boolean;
    FMouseOverClock: Boolean;
    FPressingLeft: Boolean;
    FPressingRight: Boolean;

    FFormat: string;
    FCaptionAM: TCaption;
    FCaptionPM: TCaption;
    FCaptionSet: TCaption;

    FButtonColor: TColor;
    FButtonFontColor: TColor;
    FClockFaceColors: TRzClockFaceColors;
    FShowSetButton: Boolean;
    FShowTime: Boolean;
    FShowHowToUseHint: Boolean;
    FHowToUseMsg: string;
    FHintWnd: THintWindow;
    FTimer: TTimer;
    FLastMinute: Integer;
    FThemeAware: Boolean;

    FOnChange: TNotifyEvent;
    FOnSetBtnClick: TNotifyEvent;
    FOnSetTime: TNotifyEvent;

    // Internal Event Handlers
    procedure CheckHintWindowHandler( Sender: TObject );

    // Message Handling Methods
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CMDialogChar( var Msg: TCMDialogChar ); message cm_DialogChar;
    procedure WMGetDlgCode( var Msg: TWMGetDlgCode ); message wm_GetDlgCode;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
  protected
    procedure Paint; override;
    function GetButtonFontColor: TColor;
    procedure DrawClock( Bounds: TRect; CenterPoint: TPoint; Radius: Integer ); virtual;

    procedure CalcRects;
    procedure AdjustClientRect( var Rect: TRect ); override;
    procedure AdjustForFont;
    procedure CalcFontSize;
    procedure ConstrainedResize( var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer ); override;

    function NormalizedArcTan( const Y, X: Extended ): Extended;
    function GetHourFromXY( X, Y: Integer ): Integer;
    function GetMinuteFromXY( X, Y: Integer; Restrict: Boolean ): Integer;

    function CalcHintRect( MaxWidth: Integer; const HintStr: string; HintWnd: THintWindow ): TRect;
    procedure DoHint( X, Y: Integer );
    procedure ReleaseHintWindow;

    procedure ChangeToAM;
    procedure ChangeToPM;

    // Event Dispatch Methods
    procedure Changed; dynamic;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure SetBtnClick; dynamic;

    // Property Access Methods
    procedure SetButtonColor( Value: TColor ); virtual;
    procedure SetButtonFontColor( Value: TColor ); virtual;
    procedure SetCaptionAM( const Value: TCaption ); virtual;
    procedure SetCaptionPM( const Value: TCaption ); virtual;
    procedure SetCaptionSet( const Value: TCaption ); virtual;
    procedure SetClockFaceColors( Value: TRzClockFaceColors ); virtual;
    procedure SetFormat( const Value: string ); virtual;
    procedure SetHour( Value: Integer );
    procedure SetMinutes( Value: Integer );
    procedure SetTime( Value: TTime ); virtual;
    procedure SetShowSetButton( Value: Boolean ); virtual;
    procedure SetShowTime( Value: Boolean ); virtual;
    procedure SetThemeAware( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function CanAutoSize( var NewWidth, NewHeight: Integer ): Boolean; override;

    procedure AdjustHour( DeltaHours: Int64 );
    procedure AdjustMinute( DeltaMinutes: Int64 );

    function IsClear: Boolean;
    procedure Clear;

    property IsPopup: Boolean
      write FIsPopup;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property ButtonColor: TColor
      read FButtonColor
      write SetButtonColor
      default clBtnFace;

    property ButtonFontColor: TColor
      read FButtonFontColor
      write SetButtonFontColor
      default clWindowText;

    property CaptionAM: TCaption
      read FCaptionAM
      write SetCaptionAM;

    property CaptionPM: TCaption
      read FCaptionPM
      write SetCaptionPM;

    property CaptionSet: TCaption
      read FCaptionSet
      write SetCaptionSet;

    property ClockFaceColors: TRzClockFaceColors
      read FClockFaceColors
      write SetClockFaceColors;

    property Format: string
      read FFormat
      write SetFormat;

    property HowToUseMsg: string
      read FHowToUseMsg
      write FHowToUseMsg;

    property RestrictMinutesBy: Integer
      read FRestrictMinutesBy
      write FRestrictMinutesBy
      default 5;

    property RestrictMinutes: Boolean
      read FRestrictMinutes
      write FRestrictMinutes
      default False;

    property ShowHowToUseHint: Boolean
      read FShowHowToUseHint
      write FShowHowToUseHint
      default True;

    property ShowSetButton: Boolean
      read FShowSetButton
      write SetShowSetButton
      default False;

    property ShowTime: Boolean
      read FShowTime
      write SetShowTime
      default True;
                 
    property Time: TTime
      read FTime
      write SetTime;

    property ThemeAware: Boolean
      read FThemeAware
      write SetThemeAware
      default True;

    property OnChange: TNotifyEvent
      read FOnChange
      write FOnChange;

    property OnSetBtnClick: TNotifyEvent
      read FOnSetBtnClick
      write FOnSetBtnClick;

    property OnSetTime: TNotifyEvent
      read FOnSetTime
      write FOnSetTime;

    { Inherited Properties & Events }
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize default True;
    property BevelWidth;
    property BiDiMode;
    property BorderInner;
    property BorderOuter default fsLowered;
    property BorderSides;
    property BorderColor;
    property BorderHighlight;
    property BorderShadow;
    property BorderWidth;
    property Color default clWindow;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatColor;
    property FlatColorAdjustment default 0;
    property Font;
    property FrameController;
    property FrameControllerNotifications;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;


  TRzCalculator = class;

  {===========================================}
  {== TRzCalculatorColors Class Declaration ==}
  {===========================================}

  TRzCalculatorColors = class( TPersistent )
  private
    FNumberFont: TColor;
    FNumberBtns: TColor;
    FOperatorFont: TColor;
    FOperatorBtns: TColor;
    FCommandFont: TColor;
    FCommandBtns: TColor;
    FDisplayFont: TColor;
    FDisplay: TColor;

    FCalculator: TRzCalculator;
  protected
    { Property Access Methods }
    function GetColor( Index: Integer ): TColor; virtual;
    procedure SetColor( Index: Integer; Value: TColor ); virtual;
  public
    constructor Create( ACalculator: TRzCalculator );
    destructor Destroy; override;

    procedure Assign( Source: TPersistent ); override;

    { Property Declarations }
    property Calculator: TRzCalculator
      read FCalculator;

    property Colors[ Index: Integer ]: TColor
      read GetColor
      write SetColor;
  published
    property NumberFont: TColor
      index 0
      read GetColor
      write SetColor
      default clWindowText;

    property NumberBtns: TColor
      index 1
      read GetColor
      write SetColor
      default clBtnFace;

    property OperatorFont: TColor
      index 2
      read GetColor
      write SetColor
      default clWindowText;

    property OperatorBtns: TColor
      index 3
      read GetColor
      write SetColor
      default clBtnFace;

    property CommandFont: TColor
      index 4
      read GetColor
      write SetColor
      default clWindowText;

    property CommandBtns: TColor
      index 5
      read GetColor
      write SetColor
      default clBtnFace;

    property DisplayFont: TColor
      index 6
      read GetColor
      write SetColor
      default clWindowText;

    property Display: TColor
      index 7
      read GetColor
      write SetColor
      default clWindow;
  end;


  {=====================================}
  {== TRzCalculator Class Declaration ==}
  {=====================================}

  TRzCalculatorArea = ( ccBtn0, ccBtn1, ccBtn2, ccBtn3, ccBtn4, ccBtn5, ccBtn6,
                        ccBtn7, ccBtn8, ccBtn9, ccDecimal, ccAdd, ccSubtract,
                        ccMultiply, ccDivide, ccClear, ccEqual, ccSet, ccValue );
  TRzCalculatorAreas = array[ TRzCalculatorArea ] of TRect;

  TRzCalculatorOperation = (calcNone, calcAdd, calcSubtract, calcMultiply, calcDivide );

  TRzCalculatorUpdateDisplay = ( udNumber, udResult );

  TRzCalculator = class( TRzCustomPanel )
  private
    FPressedArea: TRzCalculatorArea;
    FOverArea: TRzCalculatorArea;

    FIsPopup: Boolean;
    FMouseOverRect: TRect;
    FCharSize: TPoint;

    FNumber: Extended;
    FDecimalPlaces: Integer;
    FCurrentOperation: TRzCalculatorOperation;
    FResult: Extended;
    FFirstDigit: Boolean;
    FFirstNumber: Boolean;
    FShowResult: Boolean;
    FErrorOccurred: Boolean;

    FBoldButtons: Boolean;
    FCalculatorColors: TRzCalculatorColors;
    FThemeAware: Boolean;

    FOnChange: TNotifyEvent;
    FOnSetBtnClick: TNotifyEvent;

    function GetIntResult: Integer;
    procedure SetIntResult( Value: Integer );

    // Message Handling Methods
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure WMEraseBkgnd( var Msg: TMessage ); message wm_EraseBkgnd;
    procedure WMNCHitTest( var Msg: TWMNCHitTest ); message wm_NCHitTest;
    procedure WMGetDlgCode( var Msg: TWMGetDlgCode ); message wm_GetDlgCode;
  protected
    procedure CreateHandle; override;

    procedure DrawDisplayBox( Bounds: TRect; FrameColor: TColor ); virtual;
    function GetButtonColor( Area: TRzCalculatorArea ): TColor;
    function GetButtonFontColor( Area: TRzCalculatorArea ): TColor;
    procedure DrawCalcButton( Bounds: TRect; Area: TRzCalculatorArea ); virtual;
    procedure Paint; override;

    procedure CalcAreas( var Areas: TRzCalculatorAreas );
    procedure AdjustClientRect( var Rect: TRect ); override;
    procedure AdjustForFont;
    procedure CalcFontSize;
    procedure ConstrainedResize( var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer ); override;

    procedure PrepareForUserInput;
    procedure PrepareForOperator;
    procedure UpdateDisplay( Update: TRzCalculatorUpdateDisplay );
    procedure ShowError;
    procedure AppendDigit( N: Integer );
    procedure DecimalPressed;
    procedure OperationPressed( Operation: TRzCalculatorOperation );
    procedure EqualPressed;
    procedure CalculateNewResult;

    procedure SetResult( Value: Extended ); virtual;

    // Event Dispatch Methods
    procedure Changed; dynamic;
    procedure Click; override;

    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure SetBtnClick; dynamic;

    // Property Access Methods
    procedure SetBoldButtons( Value: Boolean ); virtual;
    procedure SetCalculatorColors( Value: TRzCalculatorColors ); virtual;
    procedure SetOverArea( Value: TRzCalculatorArea ); virtual;
    procedure SetPressedArea( Value: TRzCalculatorArea ); virtual;
    procedure SetThemeAware( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function CanAutoSize( var NewWidth, NewHeight: Integer ): Boolean; override;

    function HitTest( X, Y: Integer ): TRzCalculatorArea;

    procedure Clear;

    property IsPopup: Boolean
      write FIsPopup;

    property IntResult: Integer
      read GetIntResult
      write SetIntResult;

  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property BoldButtons: Boolean
      read FBoldButtons
      write SetBoldButtons
      default False;

    property CalculatorColors: TRzCalculatorColors
      read FCalculatorColors
      write SetCalculatorColors;

    property Result: Extended
      read FResult
      write SetResult;

    property ThemeAware: Boolean
      read FThemeAware
      write SetThemeAware
      default True;

    property OnChange: TNotifyEvent
      read FOnChange
      write FOnChange;

    property OnSetBtnClick: TNotifyEvent
      read FOnSetBtnClick
      write FOnSetBtnClick;

    { Inherited Properties & Events }
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize default True;
    property BevelWidth;
    property BiDiMode;
    property BorderInner;
    property BorderOuter default fsStatus;
    property BorderSides;
    property BorderColor;
    property BorderHighlight;
    property BorderShadow;
    property BorderWidth;
    property Color default clWindow;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatColor;
    property FlatColorAdjustment default 0;
    property Font;
    property FrameController;
    property FrameControllerNotifications;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;



  {======================================}
  {== TRzCustomPopup Class Declaration ==}
  {======================================}

  TRzCustomPopup = class( TRzCustomPanel )
  private
    FPopupControl: TControl;
  protected
    procedure AdjustClientRect( var Rect: TRect ); override;
    procedure AlignControls( AControl: TControl; var Rect: TRect ); override;

    procedure Paint; override;
  public
    constructor Create( AOwner: TComponent ); override;

    property PopupControl: TControl
      read FPopupControl;
  published
    property BevelWidth;
    property BorderInner;
    property BorderOuter;
    property BorderColor;
    property BorderHighlight;
    property BorderShadow;
    property BorderWidth;
    property FlatColor;
    property FlatColorAdjustment default 0;
  end;


  {=====================================}
  {== TRzPopupPanel Class Declaration ==}
  {=====================================}

  TRzPopupPanel = class( TRzCustomPopup )
  private
    FPopup: TWinControl;

    FOnClose: TNotifyEvent;
    FOnPopup: TNotifyEvent;
  protected
    procedure DestroyWnd; override;

    // Event Dispatch Methods
    procedure DoClose; dynamic;
    procedure DoPopup; dynamic;

    { Property Access Methods }
    function GetActive: Boolean;
  public
    constructor Create( AOwner: TComponent ); override;

    procedure Close( Sender: TObject = nil );
    function Popup( PopupControl: TControl ): Boolean; overload;
    function Popup( PopupControl: TControl; X, Y: Integer ): Boolean; overload;

    property Active: Boolean
      read GetActive;
  published
    property OnClose: TNotifyEvent
      read FOnClose
      write FOnClose;

    property OnPopup: TNotifyEvent
      read FOnPopup
      write FOnPopup;

    { Inherited Properties & Events }
    property Alignment default taRightJustify;
    property AutoSize default True;
    property BiDiMode;
    property ParentBiDiMode;
    property Color default clBtnFace;
    property Ctl3D;
    property Enabled;
    property Font;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;

    property OnContextPopup;
  end;


function StrToDateEx( const S: string; var DateTime: TDateTime;
                      TwoDigitYearConverter: TRzTwoDigitYearConverter = ycStandard ): Boolean;
function StrToTimeEx( const S: string; var DateTime: TDateTime; const ParsingFormat: string = '' ): Boolean;


resourcestring
  sRzHowToSelectTime = 'Left-click to set Hour'#13'Right-click to set Minute'#13'    Ctrl key restricts to %d minutes';
  sRzCaptionAM       = 'AM';
  sRzCaptionPM       = 'PM';
  sRzCaptionSet      = 'Set';
  sRzCaptionClearBtn = 'Clear';
  sRzCaptionTodayBtn = 'Today';

  sRzDateMax = 'Date exceeds maximum of %s';
  sRzDateMin = 'Date is less than minimum of %s';
  sRzDateRange = 'Date is beyond valid date range';


implementation

uses
  {&RAS}
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  Character,
  Themes,
  DateUtils,
  MultiMon,
  Math,
  RzSpnEdt,
  RzCommonBitmaps;


{=====================}
{== Support Methods ==}
{=====================}

function SimplifyFormat( const Format, ValidChars: string ): string;
var
  I: Integer;
  C: Char;
  S: string;
begin
  Result := '';
  S := UpperCase( Format );
  for I := 1 to Length( S ) do
  begin
    C := S[ I ];
    if ( Pos( C, ValidChars ) > 0 ) and ( Pos( C, Result ) = 0 ) then
    begin
      Result := Result + C;
    end;
  end;
end;


function ParseToken( var S: string; var Token: string ): Integer;
var
  I, Len: Integer;

  function CharType( C: Char ): Integer;
  begin
    if not IsCharAlphaNumeric( C ) then
      Result := 0
    else if IsCharAlpha( C ) then
      Result := -1
    else
      Result := 1;
  end;

begin
  Token := '';
  Result := 0;
  Len := Length( S );
  if Len > 0 then
  begin
    I := 1;
    while ( I <= Len ) and ( CharType( S[ I ] ) = 0 ) do
      Inc( I );
    Dec( Len, I - 1 );
    S := Copy( S, I, Len );
    if Len > 0 then
    begin
      Token := S;
      I := 2;
      Result := CharType( S[ 1 ] );
      while ( I <= Len ) and ( Result = CharType( S[ I ] ) ) do
        Inc( I );
      SetLength( Token, I - 1 );
      S := Copy( S, I, Len - I + 1 );
    end;
  end;
end; {= ParseToken =}



function StrToDateEx( const S: string; var DateTime: TDateTime;
                      TwoDigitYearConverter: TRzTwoDigitYearConverter = ycStandard ): Boolean;
var
  Token: Integer;
  TokenStr, T1, T2, T3, WorkStr, Format, SFormat: String;
  I, K, L, P1, P2: Integer;
  D, M, Y: Word;
  HasAllNumbers, FoundDay, FoundMonth, FoundYear: Boolean;

  function AllNumbers( const S: string ): Boolean;
  var
    C: Char;
    I: Integer;
  begin
    for I := 1 to Length( S ) do
    begin
      C := S[ I ];
      {$IFDEF VCL180_OR_HIGHER}
      if not C.IsNumber then
      begin
        Result := False;
        Exit;
      end;
      {$ELSE}
      if not TCharacter.IsNumber( C ) then
      begin
        Result := False;
        Exit;
      end;
      {$ENDIF}
    end;
    Result := True;
  end;

  function MonthPassiveName( LCID: Cardinal; Month: Integer ): string;
  var
    M: TSystemTime;
    Buffer: array[ 0..255 ] of Char;
  begin
    M.wDay := 1;
    M.wMonth := Month;
    M.wYear := 2000;
    Result := 'dd MMMM';
    {$IFDEF UNICODE}
    GetDateFormatW( LCID, 0, @M, PChar( Result ), Buffer, 256 );
    {$ELSE}
    GetDateFormatA( LCID, 0, @M, PChar( Result ), Buffer, 256 );
    {$ENDIF}
    Result := Copy( StrPas( Buffer ), 4, 255 );
  end;

  function IsMonthName( const Name: string ): Integer;
  begin
    for Result := 1 to 12 do
    begin
      if AnsiUpperCase( FormatSettings.ShortMonthNames[ Result ] ) = Name then
        Exit;
      if AnsiUpperCase( FormatSettings.LongMonthNames[ Result ] ) = Name then
        Exit;
      if AnsiUpperCase( MonthPassiveName( LOCALE_USER_DEFAULT, Result ) ) = Name then
        Exit;
      if AnsiUpperCase( MonthPassiveName( $409, Result ) ) = Name then
        Exit;
    end;
    Result := -1;
  end;


  procedure EvaluateTokenValue( K: Integer );
  var
    DateElement: Char;
  begin
    // Get the next format character
    DateElement := Format[ 1 ];

    // But check the token value to see if it looks like a specific date element
    if ( K > 31 ) and not FoundYear then
      DateElement := 'Y'
    else if ( K > 12 ) and ( DateElement <> 'Y' ) and not FoundDay then
      DateElement := 'D';

    case DateElement of
     'D':
     begin
       D := K;
       FoundDay := True;
     end;

     'M':
     begin
       M := K;
       FoundMonth := True;
     end;

     else
     begin
       Y := K;
       FoundYear := True;
     end;
    end;
    RemoveChar( Format, DateElement );
  end;


begin {= StrToDateEx =}
  if Trim( S ) = '' then
  begin
    DateTime := 0;
    Result := True;
  end
  else
  begin
    FoundDay := False;
    FoundMonth := False;
    FoundYear := False;
    Format := SimplifyFormat( FormatSettings.ShortDateFormat, 'DMY' );
    SFormat := Format;
    DecodeDate( SysUtils.Date, Y, M, D );
    WorkStr := AnsiUpperCase( S );
    HasAllNumbers := AllNumbers( Trim( S ) );
    repeat
      Token := ParseToken( WorkStr, TokenStr );
      while Token < 0 do
      begin
        K := IsMonthName( TokenStr );
        if K > 0 then
        begin
          M := K;
          FoundMonth := True;
          RemoveChar( Format, 'M' );
        end;
        Token := ParseToken( WorkStr, TokenStr );
      end;
      if ( TokenStr > '' ) and ( Format > '' ) then
      begin
        L := Length( TokenStr );

        if ( L in [ 3, 4, 5, 6, 8 ] ) and
           ( Pos( FormatSettings.DateSeparator, S ) = 0 ) and
           ( HasAllNumbers ) and
           ( Format <> 'Y' ) and
           ( Format[ 1 ] <> 'Y' ) then
        begin
          // This code handles the user entering dates without using the "/" symbol and the date element order does
          // not start with the year
          case L of
            3:
            begin
              P1 := 2;
              P2 := 0;
            end;

            4:
            begin
              P1 := 3;
              P2 := 0;
            end;

            5:
            begin
              P1 := 2;
              P2 := 4;
            end;

            else // L = 6 or 8
            begin
              P1 := 3;
              P2 := 5;
            end;
          end;

          T1 := Copy( TokenStr, 1, P1 - 1 );
          T2 := Copy( TokenStr, P1, 2 );
          if P2 > 0 then
          begin
            if L = 6 then
              T3 := Copy( TokenStr, P2, 2 )
            else
              T3 := Copy( TokenStr, P2, 4 );
          end
          else
            T3 := '';

          K := StrToInt( T1 );
          EvaluateTokenValue( K );

          if Format <> '' then
          begin
            K := StrToInt( T2 );
            EvaluateTokenValue( K );
          end;

          if ( T3 <> '' ) and ( Format <> '' ) then
          begin
            K := StrToInt( T3 );
            EvaluateTokenValue( K );
          end;
        end
        else if ( L in [ 6, 8 ] ) and
           ( Pos( FormatSettings.DateSeparator, S ) = 0 ) and
           ( HasAllNumbers ) and
           ( Format[ 1 ] = 'Y' ) then
        begin
          // This code handles the user entering dates without using the "/" symbol and the date element order
          // starts with the year (e.g. YMD)
          if L = 6 then
          begin
            P1 := 5;
            P2 := 0;
          end
          else // L = 8
          begin
            P1 := 5;
            P2 := 7;
          end;

          T1 := Copy( TokenStr, 1, P1 - 1 );
          T2 := Copy( TokenStr, P1, 2 );
          if ( P2 > 0 ) and ( L = 8 ) then
            T3 := Copy( TokenStr, P2, 2 )
          else
            T3 := '';

          K := StrToInt( T1 );
          EvaluateTokenValue( K );

          if Format <> '' then
          begin
            K := StrToInt( T2 );
            EvaluateTokenValue( K );
          end;

          if ( T3 <> '' ) and ( Format <> '' ) then
          begin
            K := StrToInt( T3 );
            EvaluateTokenValue( K );
          end;
        end
        else
        begin
          K := StrToInt( TokenStr );
          EvaluateTokenValue( K );
        end;
      end;
    until ( Format = '' ) or ( WorkStr = '' );

    if ( M > 12 ) and ( D <= 12 ) then
      Swap( M, D );

    if Y < 100 then
    begin
      // Short year format workaround
      Format := '';
      for I := 1 to Length( SFormat ) do
      begin
        case SFormat[ I ] of
         'D':
           Format := Format + FormatSettings.DateSeparator + IntToStr( D );

         'M':
           Format := Format + FormatSettings.DateSeparator + IntToStr( M );

         'Y':
           Format := Format + FormatSettings.DateSeparator + IntToStr( Y );
        end;
      end;
      Delete( Format, 1, 1 );
      try
        DateTime := StrToDate( Format );

        if ( TwoDigitYearConverter = ycPastNotFuture ) and ( DateTime > Today ) then
          DateTime := IncYear( DateTime, -100 );

        Result := True;
      except
        DateTime := 0;
        Result := False;
      end;
    end
    else // Y >= 100
    begin
      try
        DateTime := EncodeDate( Y, M, D );
        Result := True;
      except
        DateTime := 0;
        Result := False;
      end;
    end;
  end;
end; {= StrToDateEx =}




function StrToTimeEx( const S: string; var DateTime: TDateTime; const ParsingFormat: string = '' ): Boolean;
var
  Token: Integer;
  TokenStr, T1, T2, WorkStr, Format, SFormat: String;
  I, K, AMPM, L, P: Integer;
  H, M, Sec, Z: Word;
  AutoCorrected: Boolean;

  function IsAMPMName( const Token: string ): Integer;
  begin
    Result := 0;
    if ( Token = FormatSettings.TimeAMString ) or ( Token = 'AM' ) or ( Token = 'A' ) then
      Result := -1
    else if ( Token = FormatSettings.TimePMString ) or ( Token = 'PM' ) or ( Token = 'P' ) then
      Result :=  1;
  end;

begin {= StrToTimeEx =}
  if Trim( S ) = '' then
  begin
    DateTime := 0;
    Result := True;
  end
  else
  begin
    if ParsingFormat = '' then
      Format := SimplifyFormat( FormatSettings.LongTimeFormat, 'HMNSZ' )
    else
      Format := SimplifyFormat( ParsingFormat, 'HMNSZ' );

    SFormat := Format;
    H := 0;
    M := 0;
    Sec := 0;
    Z := 0;
    AMPM := 0;
    WorkStr := AnsiUpperCase( S );
    for I := 1 to 4 do
    begin
      Token := ParseToken( WorkStr, TokenStr );
      while Token < 0 do
      begin
        if AMPM = 0 then
        begin
          K := IsAMPMName( TokenStr );
          if K <> 0 then
            AMPM := K;
        end;
        Token := ParseToken( WorkStr, TokenStr );
      end;
      if ( TokenStr > '' ) and ( Format > '' ) then
      begin
        L := Length( TokenStr );
        if ( L > 2 ) and ( L <= 4 ) then
        begin
          // This code handles the user entering times without using the ":" symbol
          if ( L = 3 ) and ( Format[ 1 ] = 'H' ) then
            P := 2
          else
            P := 3;

          T1 := Copy( TokenStr, 1, P - 1 );
          T2 := Copy( TokenStr, P, 2 );

          K := StrToInt( T1 );
          case Format[ 1 ] of
           'H':
             H := K;

           'M','N':
             M := K;

           'S':
             Sec := K;

           else
             Z := K;
          end;
          Delete( Format, 1, 1 );

          K := StrToInt( T2 );
          case Format[ 1 ] of
           'H':
             H := K;

           'M','N':
             M := K;

           'S':
             Sec := K;

           else
             Z := K;
          end;
          Delete( Format, 1, 1 );

        end
        else
        begin
          K := StrToInt( TokenStr );
          case Format[ 1 ] of
           'H':
             H := K;

           'M','N':
             M := K;

           'S':
             Sec := K;

           else
             Z := K;
          end;
          Delete( Format, 1, 1 );
        end;
      end;
    end;

    if ( H < 12 ) and ( AMPM > 0 ) then
      Inc( H, 12 )
    else if ( H = 12 ) and ( AMPM < 0 ) then
      H := 0;

    AutoCorrected := False;
    if H >= 24 then
    begin
      H := H mod 24;
      AutoCorrected := True;
    end;
    if M >= 60 then
    begin
      M := 0;
      AutoCorrected := True;
    end;
    if Sec >= 60 then
    begin
      Sec := 0;
      AutoCorrected := True;
    end;
    while Z > 999 do
    begin
      Z := Z div 10;
      AutoCorrected := True;
    end;

    try
      DateTime := EncodeTime( H, M, Sec, Z );
      Result := not AutoCorrected;
    except
      DateTime := 0;
      Result := False;
    end;
  end;
end; {= StrToTimeEx =}


procedure DrawButton( Canvas: TCanvas; Bounds: TRect; const Caption: string;
                      ButtonColor, ButtonFontColor: TColor;
                      Flat, Down, ThemeAware: Boolean );
var
  ElementDetails: TThemedElementDetails;
  {$IFDEF VCL160_OR_HIGHER}
  B: TBitmap;
  R: TRect;
  {$ENDIF}
begin
  if ThemeAware and ActiveStyleServicesEnabled then
  begin
    if Down then
      ElementDetails := ActiveStyleServices.GetElementDetails( tbPushButtonPressed )
    else
      ElementDetails := ActiveStyleServices.GetElementDetails( tbPushButtonNormal );

    if UsingSystemStyle then
    begin
      ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, Bounds );
    end
    else // VCL Styles
    begin
      {$IFDEF VCL160_OR_HIGHER}
      B := TBitmap.Create;
      try
        B.Width := Bounds.Right - Bounds.Left;
        B.Height := Bounds.Bottom - Bounds.Top;

        R := Bounds;
        OffsetRect( R, -Bounds.Left, -Bounds.Top );

        B.Canvas.CopyRect( R, Canvas, Bounds );

        StyleServices.DrawElement( B.Canvas.Handle, ElementDetails, R );

        Canvas.Draw( Bounds.Left, Bounds.Top, B );
      finally
        B.Free;
      end;
      {$ENDIF}
    end;
    Canvas.Brush.Style := bsClear;
  end
  else
  begin
    if ButtonColor = clBtnFace then
    begin
      if Down then
        Bounds := DrawButtonBorder( Canvas, Bounds, True )
      else
      begin
        if Flat then
          Bounds := DrawBorder( Canvas, Bounds, fsFlat )
        else
          Bounds := DrawCtl3DBorder( Canvas, Bounds, False );
      end;
    end
    else
    begin
      if Down then
        Bounds := DrawColorButtonBorder( Canvas, Bounds, ButtonColor, True )
      else
      begin
        if Flat then
          Bounds := DrawColorBorder( Canvas, Bounds, ButtonColor, fsFlat )
        else
          Bounds := DrawColorButtonBorder( Canvas, Bounds, ButtonColor, False );
      end;
    end;
    Canvas.Brush.Color := ButtonColor;
    Canvas.FillRect( Bounds );

    if Down then
    begin
      Inc( Bounds.Left, 2 );
      Inc( Bounds.Top, 2 );
    end;
  end;

  Canvas.Font.Color := ButtonFontColor;
  DrawStringCentered( Canvas, Caption, Bounds );

  Canvas.Brush.Style := bsSolid;
end; {= DrawButton =}



{======================================}
{== TRzYearSpinner Class Declaration ==}
{======================================}

type
  TRzYearSpinner = class( TRzSpinner )
  private
    FCalendar: TRzCalendar;

    // Message Handling Methods
    procedure CMCancelMode( var Msg: TCMCancelMode ); message cm_CancelMode;
    procedure CMShowingChanged( var Msg: TMessage ); message cm_ShowingChanged;
    procedure WMKillFocus( var Msg: TMessage ); message wm_KillFocus;
    procedure CNKeyDown( var Msg: TWMKeyDown ); message cn_KeyDown;
  protected
    procedure CreateParams( var Params: TCreateParams ); override;

    // Event Dispatch Methods
    procedure KeyPress( var Key: Char ); override;
  public
    constructor Create( AOwner: TComponent ); override;
    property Font;
  end;


{===========================}
{== TRzYearSpinner Method ==}
{===========================}

constructor TRzYearSpinner.Create( AOwner: TComponent );
begin
  inherited;
  TabStop := False;
  Visible := False;
  Min := 0;
  Max := 9999;
  ShowFocusRect := False;
end;


procedure TRzYearSpinner.CreateParams( var Params: TCreateParams );
begin
  inherited;
  Params.WindowClass.Style := CS_SAVEBITS;
end;


procedure TRzYearSpinner.KeyPress( var Key: Char );
begin
  case Key of
    #9: Key := #0;

    #27:
    begin
      FCalendar.HideYearSpinner;
      Key := #0;
    end;

    #13:
    begin
      FCalendar.HideYearSpinner;
      Key := #0;
    end;
  end;

  if Key <> #0 then
    inherited;
end;


procedure TRzYearSpinner.CNKeyDown( var Msg: TWMKeyDown );
begin
  case Msg.CharCode of
    67:
    begin
      // Check to see if user presses Ctrl+C and stop it from triggering main menu Copy
      Msg.CharCode := 0;
    end;

    vk_Escape:
    begin
      FCalendar.HideYearSpinner;
      Msg.CharCode := 0;
    end;

    else
      inherited;
  end;
end;


procedure TRzYearSpinner.CMCancelMode( var Msg: TCMCancelMode );
begin
  // cm_CancelMode is sent when user clicks somewhere in same application
  if Msg.Sender <> Self then
    FCalendar.HideYearSpinner;
end;


procedure TRzYearSpinner.WMKillFocus( var Msg: TMessage );
begin
  // wm_KillFocus is sent went user switches to another application or window
  inherited;
  FCalendar.HideYearSpinner;
end;


procedure TRzYearSpinner.CMShowingChanged( var Msg: TMessage );
begin
  // Ignore showing using the Visible property
end;



{===============================}
{== TRzCalendarColors Methods ==}
{===============================}

constructor TRzCalendarColors.Create( ACalendar: TRzCalendar );
begin
  inherited Create;
  FCalendar := ACalendar;

  FDays := clWindowText;
  FFillDays := clBtnShadow;
  FDaysOfWeek := clWindowText;
  FLines := clBtnShadow;
  FSelectedDateBack := clHighlight;
  FSelectedDateFore := clHighlightText;
  FTodaysDateFrame := clMaroon;
end;


destructor TRzCalendarColors.Destroy;
begin
  FCalendar := nil;
  inherited;
end;


procedure TRzCalendarColors.Assign( Source: TPersistent );
begin
  if Source is TRzCalendarColors then
  begin
    Days := TRzCalendarColors( Source ).Days;
    FillDays := TRzCalendarColors( Source ).FillDays;
    DaysOfWeek := TRzCalendarColors( Source ).DaysOfWeek;
    Lines := TRzCalendarColors( Source ).Lines;
    SelectedDateBack := TRzCalendarColors( Source ).SelectedDateBack;
    SelectedDateFore := TRzCalendarColors( Source ).SelectedDateFore;
    TodaysDateFrame := TRzCalendarColors( Source ).TodaysDateFrame;
  end
  else
    inherited;
end;


function TRzCalendarColors.GetColor( Index: Integer ): TColor;
begin
  case Index of
    0: Result := FDays;
    1: Result := FFillDays;
    2: Result := FDaysOfWeek;
    3: Result := FLines;
    4: Result := FSelectedDateBack;
    5: Result := FSelectedDateFore;
    6: Result := FTodaysDateFrame;
    else
      Result := clNone;
  end;
end;


procedure TRzCalendarColors.SetColor( Index: Integer; Value: TColor );
begin
  case Index of
    0: FDays := Value;
    1: FFillDays := Value;
    2: FDaysOfWeek := Value;
    3: FLines := Value;
    4: FSelectedDateBack := Value;
    5: FSelectedDateFore := Value;
    6: FTodaysDateFrame := Value;
  end;

  if FCalendar <> nil then
    FCalendar.Invalidate;
end;



{&RT}
{=========================}
{== TRzCalendar Methods ==}
{=========================}

constructor TRzCalendar.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ( ControlStyle - [ csAcceptsControls, csNoStdEvents, csSetCaption ] ) +
                  [ csReflector ];

  FElements := [ ceYear, ceMonth, ceArrows, ceFillDays, ceDaysOfWeek,
                 ceTodayButton, ceClearButton ];
  FShowDays := True;
  if csDesigning in ComponentState then
  begin
    FDate := 1;
    Clear;
  end
  else
    FDate := SysUtils.Date;

  FForceUpdate := False;
  FFirstDayOfWeek := fdowLocale;

  FCalendarColors := TRzCalendarColors.Create( Self );
  FButtonColor := clBtnFace;
  FButtonFontColor := clWindowText;
  FThemeAware := True;

  {&RCI}

  Width := 147;
  Height := 159;

  Color := clWindow;
  FlatColorAdjustment := 0;
  BorderOuter := fsLowered;
  TabStop := True;
  AutoSize := True;
  DoubleBuffered := True;
  AdjustForFont;

  CreateMonthPopupMenu;

  FYearSpinner := TRzYearSpinner.Create( Self );
  FYearSpinner.Parent := Self;
  TRzYearSpinner( FYearSpinner ).FCalendar := Self;
  TRzYearSpinner( FYearSpinner ).OnDecrement := YearSpinnerDecrementHandler;
  TRzYearSpinner( FYearSpinner ).OnIncrement := YearSpinnerIncrementHandler;
  FYearSpinnerVisible := False;

  FToday := SysUtils.Date;
  FTodayTimer := TTimer.Create( Self );
  FTodayTimer.Enabled := False;
  FTodayTimer.Interval := 1000;
  FTodayTimer.OnTimer := TodayTimerExpired;
end;


procedure TRzCalendar.CreateHandle;
begin
  inherited;
  FPressedArea := caFillDays;
end;


procedure TRzCalendar.CreateWnd;
begin
  inherited;

  if RunningAtLeast( win2000 ) then
    Perform( wm_ChangeUIState, MakeWParam( UIS_INITIALIZE, UISF_HIDEACCEL or UISF_HIDEFOCUS ), 0 );

  SetFirstDayOfWeek( FFirstDayOfWeek );
  FTodayTimer.Enabled := True;
end;


procedure TRzCalendar.CreateMonthPopupMenu;
var
  I: Integer;
  Item: TMenuItem;
begin
  FMonthPopupMenu := TPopupMenu.Create( Self );

  for I := 1 to 12 do
  begin
    Item := TMenuItem.Create( Self );
    Item.Caption := FormatSettings.LongMonthNames[ I ];
    Item.Tag := I;
    Item.OnClick := MonthPopupClickHandler;
    FMonthPopupMenu.Items.Add( Item );
  end;
end;


destructor TRzCalendar.Destroy;
begin
  FCalendarColors.Free;
  inherited;
end;


procedure TRzCalendar.Paint;
var
  S, S2, F: string;
  R, WR, SelRect, DaysRect, DayR: TRect;
  I, J, WN, MaxLen, SaveFontSize: Integer;
  DrawDate: TDateTime;
  Year, Month, Day, ViewMonth, ViewYear: Word;
  DayColor, DayFontColor, TempC, C: TColor;
  DayFontStyle: TFontStyles;
  CustomDayFormat: Boolean;
  PMBoldDays, NMBoldDays, CMBoldDays, BoldDaysMask: Cardinal;
  ArrowFont: TFont;
  ButtonsVisible, DrawFlat: Boolean;
  Areas: TRzCalendarAreas;
  {$IFDEF VCL160_OR_HIGHER}
  Details: TThemedElementDetails;
  B: TBitmap;
  {$ENDIF}


  procedure DrawHorizontalLine( ALeft, ARight, AHeight: Integer );
  begin
    if UsingSystemStyle then
    begin
      Canvas.Pen.Color := FCalendarColors.Lines;
    end
    else // VCL Styles
    begin
      Canvas.Pen.Color := ActiveStyleFontColor( sfEditBoxTextDisabled );
    end;
    Canvas.MoveTo( ALeft, AHeight );
    Canvas.LineTo( ARight, AHeight );
  end;


  procedure DrawVerticalLine( ATop, ABottom, ALeft: Integer );
  begin
    if UsingSystemStyle then
    begin
      Canvas.Pen.Color := FCalendarColors.Lines;
    end
    else // VCL Styles
    begin
      Canvas.Pen.Color := ActiveStyleFontColor( sfEditBoxTextDisabled );
    end;
    Canvas.MoveTo( ALeft, ATop );
    Canvas.LineTo( ALeft, ABottom );
  end;

begin {= TRzCalendar.Paint =}
  if UsingSystemStyle then
  begin
    if FThemeAware and ActiveStyleServicesEnabled then
    begin
      C := GetXPThemeColor( xptcEditBorder );
      R := Rect( 0, 0, Width, Height );
      R := DrawBox( Canvas, R, C );
      Canvas.Brush.Color := Color;
      Canvas.FillRect( R );
    end
    else
      inherited;
  end
  else // VCL Styles
  begin
    {$IFDEF VCL160_OR_HIGHER}
    R := Rect( 0, 0, Width, Height );
    Details := StyleServices.GetElementDetails( teEditTextNormal );

    B := TBitmap.Create;
    try
      B.Width := Width;
      B.Height := Height;
        StyleServices.DrawElement( B.Canvas.Handle, Details, R );
      Canvas.Draw( 0, 0, B );
    finally
      B.Free;
    end;
    {$ENDIF}
  end;

  DrawFlat := BorderOuter in [ fsFlat, fsFlatBold, fsFlatRounded ];

  CalcAreas( Areas );

  // Draw Month and Year Bar

  if ( [ ceMonth, ceYear ] * FElements ) <> [ ] then
  begin

    if FShowDays then
    begin
      R := Areas[ caYear ];

      if UsingSystemStyle then
      begin
        if FThemeAware and ActiveStyleServicesEnabled then
        begin
          GetGradientPanelColors( gcsMSOffice, TempC, C );
        end
        else
        begin
          if not DrawFlat then
            R := DrawBorder( Canvas, R, fsPopup );
          C := FButtonColor;
        end;
      end
      else // VCL Styles
      begin
        C := ActiveStyleColor( scGenericGradientBase );
      end;

      Canvas.Brush.Color := C;
      Canvas.FillRect( R );
    end;

    SetBkMode( Canvas.Handle, Windows.Transparent );
    if ceArrows in FElements then
    begin
      ArrowFont := TFont.Create;
      try
        ArrowFont.Name := 'Marlett';
        ArrowFont.Size := Font.Size + 4;
        Canvas.Font.Assign( ArrowFont );
      finally
        ArrowFont.Free;
      end;

      Canvas.Font.Color := GetButtonFontColor;

      DrawStringCentered( Canvas, '3', Areas[ caLeftArrow ] );   // Draw Left Arrow
      DrawStringCentered( Canvas, '4', Areas[ caRightArrow ] );  // Draw Right Arrow
    end;
    // Draw month and year
    Canvas.Font.Assign( Font );
    Canvas.Font.Color := GetButtonFontColor {FButtonFontColor};
    F := 'mmmm';
    if ceYear in FElements then
      F := F + ' yyyy';
    DrawStringCentered( Canvas, FormatDateTime( F, ViewDate ), Areas[ caYear ] );
  end;

  if FShowDays then
  begin
    ButtonsVisible := False;

    // Draw Today button

    if ceTodayButton in FElements then
    begin
      if FCaptionTodayBtn <> '' then
        S := FCaptionTodayBtn
      else
        S := sRzCaptionTodayBtn;
      DrawButton( Canvas, Areas[ caTodayButton ], S,
                  FButtonColor, GetButtonFontColor, DrawFlat,
                  ( FPressedArea = caTodayButton ) and ( FOverArea = caTodayButton ),
                  FThemeAware );
      ButtonsVisible := True;
    end;

    // Draw Clear Button

    if ceClearButton in FElements then
    begin
      if FCaptionClearBtn <> '' then
        S := FCaptionClearBtn
      else
        S := sRzCaptionClearBtn;
      DrawButton( Canvas, Areas[ caClearButton ], S,
                  FButtonColor, GetButtonFontColor, DrawFlat,
                  ( FPressedArea = caClearButton ) and ( FOverArea = caClearButton ),
                  FThemeAware );
      ButtonsVisible := True;
    end;

    // Draw Day of Week Headings

    if ceDaysOfWeek in FElements then
    begin
      if UsingSystemStyle then
      begin
        Canvas.Brush.Color := Color;
        Canvas.Font.Color := FCalendarColors.DaysOfWeek;
      end
      else // VCL Styles
      begin
        Canvas.Brush.Color := ActiveStyleColor( scEdit );
        Canvas.Font.Color := ActiveStyleFontColor( sfEditBoxTextNormal );
      end;

      SaveFontSize := Canvas.Font.Size;
      Canvas.Font.Size := SaveFontSize - 1;

      R := Bounds( Areas[ caDaysOfWeek ].Left, Areas[ caDaysOfWeek ].Top - 1,
                   FCellSize.X - 2, FCellSize.Y );

      MaxLen := GetMaxShortDayNameLength;
      for I := 0 to 6 do
      begin
        J := FFirstDay + I + 2;
        if J > 7 then
          Dec( J, 7 );
        S := FormatSettings.ShortDayNames[ J ];

        if MaxLen > ( R.Right - R.Left ) then
        begin
          if SysLocale.DefaultLCID <> $040D then
            S2 := Copy( S, 1, 2 )
          else // Hebrew - Day name abbreviations handled differently
          begin
            if Length( S ) >= 5 then
              S2 := S[ 5 ]
            else
              S2 := S[ 1 ];
          end;

          if Canvas.TextWidth( S2 ) > ( R.Right - R.Left ) then
            DrawString( Canvas, S2[ 1 ], R, dt_VCenter or dt_SingleLine or dt_Right or dt_NoClip )
          else
            DrawString( Canvas, S2, R, dt_VCenter or dt_SingleLine or dt_Right or dt_NoClip )
        end
        else
          DrawString( Canvas, S, R, dt_VCenter or dt_SingleLine or dt_Right or dt_NoClip );

        OffsetRect( R, FCellSize.X, 0 );
      end;

      Canvas.Font.Size := SaveFontSize;

      if ceWeekNumbers in FElements then
      begin
        DrawHorizontalLine( Areas[ caWeekNumbers ].Right - 5, Areas[ caDaysOfWeek ].Right, R.Bottom );
      end
      else
        DrawHorizontalLine( Areas[ caDaysOfWeek ].Left, Areas[ caDaysOfWeek ].Right, R.Bottom );
    end;

    // Draw Days

    DaysRect := Areas[ caDays ];

    CMBoldDays := 0;  // Current Month
    PMBoldDays := 0;  // Previous Month
    NMBoldDays := 0;  // Next Month

    DecodeDate( FViewDate, ViewYear, ViewMonth, Day );

    GetBoldDays( ViewYear, ViewMonth, CMBoldDays );
    if ceFillDays in FElements then
    begin
      DecodeDate( IncMonth( FViewDate, -1 ), Year, Month, Day );
      GetBoldDays( Year, Month, PMBoldDays );
      DecodeDate( IncMonth( FViewDate, 1 ), Year, Month, Day );
      GetBoldDays( Year, Month, NMBoldDays );
    end;

    if ceWeekNumbers in FElements then
    begin
      Canvas.Pen.Color := FCalendarColors.DaysOfWeek;
      DrawVerticalLine( Areas[ caWeekNumbers ].Top, Areas[ caWeekNumbers ].Bottom, Areas[ caWeekNumbers ].Right - 5 );
    end;

    R := Bounds( DaysRect.Left, DaysRect.Top - 1, FCellSize.X - 2, FCellSize.Y );

    WR := Areas[ caWeekNumbers ];
    Inc( WR.Top, 1 );
    WR.Bottom := WR.Top + FCellSize.Y;
    Dec( WR.Right, 5 );

    DrawDate := FFirstDateInGrid;
    for I := 0 to 5 do
    begin
      for J := 0 to 6 do
      begin
        DecodeDate( DrawDate, Year, Month, Day );

        if UsingSystemStyle then
        begin
          DayColor := Color;
        end
        else // VCL Styles
        begin
          DayColor := ActiveStyleColor( scEdit );
        end;
        DayFontStyle := [];

        if ( Month = ViewMonth ) and ( Year = ViewYear ) then
        begin
          if UsingSystemStyle then
            DayFontColor := FCalendarColors.Days
          else
          begin
            DayFontColor := ActiveStyleFontColor( sfEditBoxTextNormal );
          end;
          BoldDaysMask := CMBoldDays;
        end
        else
        begin
          if UsingSystemStyle then
            DayFontColor := FCalendarColors.FillDays
          else
          begin
            DayFontColor := ActiveStyleFontColor( sfEditBoxTextDisabled );
          end;

          if EncodeDate( Year, Month, 1 ) > EncodeDate( ViewYear, ViewMonth, 1 ) then
            BoldDaysMask := NMBoldDays
          else
            BoldDaysMask := PMBoldDays;
        end;

        if ( ( ceFillDays in FElements ) or ( Month = ViewMonth ) ) then
        begin
          // Determine Day Color
          CustomDayFormat := GetDayFormat( DrawDate, Year, Month, Day,
                                           DayColor, DayFontColor, DayFontStyle );
          Canvas.Font.Color := DayFontColor;
          Canvas.Font.Style := DayFontStyle;
          Canvas.Brush.Color := DayColor;

          if ColorToRGB( DayColor ) <> ColorToRGB( Color )  then
          begin
            DayR := R;
            Inc( DayR.Right, 2 );

            Canvas.FillRect( DayR );
          end;

          if not CustomDayFormat then
          begin
            // if not using a CustomDayFormat, then go ahead and apply BoldDaysMask
            if ( BoldDaysMask and ( $1 shl ( Day - 1 ) ) ) = 0 then
              Canvas.Font.Style := Canvas.Font.Style - [ fsBold ]
            else
              Canvas.Font.Style := Canvas.Font.Style + [ fsBold ];
          end;

          if DrawDate = Trunc( FDate ) then
          begin
            // Highlight selected Date
            SelRect := R;
            Inc( SelRect.Right, 2 );
            Canvas.Font.Color := FCalendarColors.SelectedDateFore;
            Canvas.Brush.Color := FCalendarColors.SelectedDateBack;
            FillRect( Canvas.Handle, SelRect, Canvas.Brush.Handle );

            if ShowFocus and Focused then
              Canvas.DrawFocusRect( SelRect );
          end;

          DrawString( Canvas, IntToStr( Day ), R, dt_VCenter or dt_SingleLine or dt_Right or dt_NoClip );

          if DrawDate = Trunc( SysUtils.Date ) then
          begin
            // Highlight Today's date
            Canvas.Pen.Color := FCalendarColors.TodaysDateFrame;
            Canvas.Brush.Style := bsClear;
            Canvas.Rectangle( R.Left, R.Top, R.Right + 2, R.Bottom );
            Canvas.Brush.Style := bsSolid;
          end;
        end;
        OffsetRect( R, FCellSize.X, 0 );
        DrawDate := DrawDate + 1;
      end;

      if ceWeekNumbers in FElements then
      begin
        if UsingSystemStyle then
        begin
          Canvas.Brush.Color := Color;
          Canvas.Font.Color := FCalendarColors.DaysOfWeek;
        end
        else // VCL Styles
        begin
          Canvas.Brush.Color := ActiveStyleColor( scEdit );
          Canvas.Font.Color := ActiveStyleFontColor( sfEditBoxTextNormal );
        end;

        if Assigned( FOnGetWeekNumber ) then
        begin
          WN := WeekOf( DrawDate - 6 );
          FOnGetWeekNumber( Self, DrawDate, WN );
          DrawStringCentered( Canvas, IntToStr( WN ), WR );
        end
        else
          DrawStringCentered( Canvas, IntToStr( WeekOf( DrawDate - 6 ) ), WR );
        OffsetRect( WR, 0, FCellSize.Y );
      end;

      OffsetRect( R, DaysRect.Left - R.Left, FCellSize.Y );
    end;

    if ButtonsVisible then
    begin
      if ceWeekNumbers in FElements then
      begin
        DrawHorizontalLine( Areas[ caWeekNumbers ].Right - 5, DaysRect.Right, DaysRect.Bottom - 1 );
      end
      else
        DrawHorizontalLine( DaysRect.Left, DaysRect.Right, DaysRect.Bottom - 1 );
    end;
  end;

end; {= TRzCalendar.Paint =}


function TRzCalendar.GetButtonFontColor: TColor;
begin
  if UsingSystemStyle then
  begin
    Result := FButtonFontColor;
  end
  else // VCL Styles
  begin
    Result := ActiveStyleFontColor( sfButtonTextNormal );
  end;
end;


function TRzCalendar.ShowFocus: Boolean;
begin
  Result := ( Perform( wm_QueryUIState, 0, 0 ) and UISF_HIDEFOCUS ) = 0;
end;


function TRzCalendar.GetMaxShortDayNameLength: Integer;
var
  I, W: Integer;
begin
  Result := 0;
  for I := 1 to 7 do
  begin
    W := Canvas.TextWidth( FormatSettings.ShortDayNames[ I ] );
    if W > Result then
      Result := W;
  end;
end;


procedure TRzCalendar.CalcAreas( var Areas: TRzCalendarAreas );
var
  CR, R, Temp: TRect;
  I, J, W1, W2, X: Integer;
  F: string;
  OldFontStyle: TFontStyles;
begin
  CR := ClientRect;
  AdjustClientRect( CR );

  FillChar( Areas, SizeOf( Areas ), 0 );
  if ( [ ceYear, ceMonth ] * FElements ) <> [ ] then
  begin
    // If ceYear or ceMonth in FElements...
    Areas[ caYear ] := CR;
    Areas[ caYear ].Bottom := CR.Top + FCharSize.Y + 5;

    // Calc rectangle for Month name
    Areas[ caMonth ] := Areas[ caYear ];
    F := 'mmmm';
    if ceYear in FElements then
      F := F + ' yyyy';

    OldFontStyle := Canvas.Font.Style;
    Canvas.Font.Style := Font.Style;

    W1 := Canvas.TextWidth( FormatDateTime( F, ViewDate ) );
    W2 := Canvas.TextWidth( FormatDateTime( 'yyyy', ViewDate ) );

    Canvas.Font.Style := OldFontStyle;

    X := Areas[ caMonth ].Left +
         ( Areas[ caMonth ].Right - Areas[ caMonth ].Left ) div 2 -
         ( W1 div 2 ) + W1 - W2;

    Areas[ caMonth ].Right := X;

    if ceArrows in FElements then
    begin
      Areas[ caLeftArrow ] := Areas[ caYear ];
      Areas[ caLeftArrow ].Right := Areas[ caLeftArrow ].Left + FCharSize.Y + 5;
      Areas[ caRightArrow ] := Areas[ caYear ];
      Areas[ caRightArrow ].Left := Areas[ caRightArrow ].Right - FCharSize.Y - 5;
    end;
    CR.Top := Areas[ caYear ].Bottom;
  end;

  if FShowDays then
  begin
    Inc( CR.Left, FCharSize.X * 2 - 1 );
    Dec( CR.Right, FCharSize.X * 2 + 1 );
    if ( [ ceTodayButton, ceClearButton ] * FElements ) <> [ ] then
    begin
      // If ceTodayButton or ceClearButton in FOption...
      R := CR;
      R.Top := R.Bottom - ( FCharSize.Y + 15 + 3 );
      if ceTodayButton in FElements then
      begin
        Areas[ caTodayButton ] := R;
        if ceClearButton in FElements then
        begin
          Areas[ caTodayButton ].Right := ( R.Right + R.Left - FTodayBtnWidth - FClearBtnWidth - 5 ) div 2 + FTodayBtnWidth;
          R.Left := Areas[ caTodayButton ].Right + 5;
        end;
        Areas[ caTodayButton ] := CenterRect( Areas[ caTodayButton ], FTodayBtnWidth + 17, FCharSize.Y + 7 );
      end;
      if ceClearButton in FElements then
        Areas[ caClearButton ] := CenterRect( R, FClearBtnWidth + 17, FCharSize.Y + 7 );
      CR.Bottom := R.Top + 1;
      if (ceTodayButton in FElements) and (ceClearButton in FElements) and UseRightToLeftAlignment then
      begin
        Temp := Areas[ caTodayButton ];
        Areas[ caTodayButton ] := Areas[ caClearButton ];
        Areas[ caClearButton ] := Temp;
      end;
    end;

    if ceWeekNumbers in FElements then
    begin
      Areas[ caWeekNumbers ] := CR;
      Areas[ caWeekNumbers ].Right := Cr.Left + FCharSize.X * 2 + 10;
      Inc( CR.Left, FCharSize.X * 2 + 10 );
    end;

    I := 6;
    J := 1;
    if ceDaysOfWeek in FElements then
    begin
      Inc( I );
      Inc( J );
    end;
    FCellSize.X := ( CR.Right - CR.Left ) div 7;
    FCellSize.Y := ( CR.Bottom - CR.Top - J ) div I;
    if ceDaysOfWeek in FElements then
    begin
      Areas[ caDaysOfWeek ] := CR;
      Areas[ caDaysOfWeek ].Bottom := Areas[ caDaysOfWeek ].Top + FCellSize.Y;
      Areas[ caWeekNumbers ].Top := Areas[ caDaysOfWeek ].Top + FCellSize.Y;
      CR.Top := Areas[ caDaysOfWeek ].Bottom + 2;
    end;
  end;
  Areas[ caDays ] := CR;
end; {= TRzCalendar.CalcAreas =}


procedure TRzCalendar.AdjustClientRect( var Rect: TRect );
begin
  inherited;
  FixClientRect( Rect, False );
end;


procedure TRzCalendar.AdjustForFont;
begin
  CalcFontSize;
  AdjustSize;
  Invalidate;
end;


procedure TRzCalendar.CalcFontSize;
var
  Canvas: TCanvas;
  DC: HDC;
begin
  Canvas := TCanvas.Create;
  DC := GetDC( 0 );
  try
    Canvas.Handle := DC;
    Canvas.Font.Assign( Font );
    FCharSize.X := Canvas.TextWidth( '0' );
    FCharSize.Y := Canvas.TextHeight( '0' );

    if FCaptionTodayBtn <> '' then
      FTodayBtnWidth := Canvas.TextWidth( FCaptionTodayBtn )
    else
      FTodayBtnWidth := Canvas.TextWidth( sRzCaptionTodayBtn );

    if FCaptionClearBtn <> '' then
      FClearBtnWidth := Canvas.TextWidth( FCaptionClearBtn )
    else
      FClearBtnWidth := Canvas.TextWidth( sRzCaptionClearBtn );
  finally
    Canvas.Handle := 0;
    Canvas.Free;
    ReleaseDC( 0, DC );
  end;
end; {= TRzCalendar.CalcFontSize =}


procedure TRzCalendar.ConstrainedResize( var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer );
begin
  CanAutoSize( MinWidth, MinHeight );
  inherited;
end;


function TRzCalendar.CanAutoSize( var NewWidth, NewHeight: Integer ): Boolean;
var
  W: Integer;
begin
  NewHeight := 4;
  if ( [ ceYear, ceMonth ] * FElements ) <> [ ] then
    Inc( NewHeight, FCharSize.Y + 5 );

  if FShowDays then
  begin
    Inc( NewHeight, ( FCharSize.Y + 2 ) * 6 + 1 );

    if ceDaysOfWeek in FElements then
      Inc( NewHeight, ( FCharSize.Y + 2 ) + 1 );

    NewWidth := ( FCharSize.X * 2 + 5 ) * 7;

    if ceWeekNumbers in FElements then
      NewWidth := NewWidth + FCharSize.X * 2 + 5;

    if ( [ ceTodayButton, ceClearButton ] * FElements ) <> [ ] then
    begin
      Inc( NewHeight, FCharSize.Y + 15 + 2 );

      W := 34;
      if ceTodayButton in FElements then
      begin
        if ceClearButton in FElements then
          W := W + FTodayBtnWidth + 5 + FClearBtnWidth
        else
          W := W + FTodayBtnWidth;
      end
      else if ceClearButton in FElements then
        W := W + FClearBtnWidth;

      if W > NewWidth then
        NewWidth := W;
    end;
    Inc( NewWidth, FCharSize.X * 4 );
  end;
  Result := True;
end; {= TRzCalendar.CanAutoSize =}


procedure TRzCalendar.StartTimer;
begin
  FCounter := -1;
  TimerExpired;
  SetTimer( Handle, 0, 50, nil );
end;


procedure TRzCalendar.TimerExpired;

  function ScrollMonth( var Counter: Integer ): Boolean;
  begin
    Result := False;
    Inc( Counter );
    if Counter < 20 then
    begin
      // For the first second ( 20 * 50 ms ), scroll month every 250 ms
      if ( Counter mod 5 ) <> 0 then
        Exit;
    end
    else if Counter < 200 then
    begin
      // For the next 9 seconds ( (200 - 20) * 50 ms ), scroll month every 100 ms
      if ( Counter mod 2 ) <> 0 then
        Exit;
    end;
    Result := True;
  end;

begin {= TRzCalendar.TimerExpired =}
  case FPressedArea of
    caLeftArrow:
    begin
      if ScrollMonth( FCounter ) and ( FOverArea = FPressedArea ) then
      begin
        if UseRightToLeftAlignment then
          SetViewDate( IncMonth( FViewDate, 1 ) )
        else
          SetViewDate( IncMonth( FViewDate, -1 ) );
      end;
    end;

    caRightArrow:
    begin
      if ScrollMonth( FCounter ) and ( FOverArea = FPressedArea ) then
      begin
        if UseRightToLeftAlignment then
          SetViewDate( IncMonth( FViewDate, -1 ) )
        else
          SetViewDate( IncMonth( FViewDate, 1 ) );
      end;
    end;
  end;
end; {= TRzCalendar.TimerExpired =}


procedure TRzCalendar.StopTimer;
begin
  KillTimer( Handle, 0 );
end;


procedure TRzCalendar.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  Areas: TRzCalendarAreas;
begin
  inherited;

  if FYearSpinnerVisible then
  begin
    // This takes care of hiding the year spinner when used in a TRzDateTimeEdit
    HideYearSpinner;
    Exit;
  end;

  if not ( csDesigning in ComponentState ) and not FIsPopup and IsWindowVisible( Handle ) then
    SetFocus;

  if Button = mbLeft then
  begin
    if not PtInRect( ClientRect, Point( X, Y ) ) then
      SetOverArea( caFillDays );

    if FOverArea in [ caTodayButton, caClearButton, caDays ] then
    begin
      CalcAreas( Areas );
      FMouseOverRect := Areas[ FOverArea ];
    end
    else
      FMouseOverRect := Rect( 0, 0, 0, 0 );

    FIgnoreClick := False;

    SetPressedArea( FOverArea );
    if FPressedArea in [ caLeftArrow, caRightArrow ] then
      StartTimer
    else if FPressedArea = caYear then
      ShowYearSpinner
    else if FPressedArea = caMonth then
      DisplayMonthPopupMenu( X, Y )
    else
      UpdateHighlight( X, Y );
  end;
end; {= TRzCalendar.MouseDown =}


procedure TRzCalendar.DisplayMonthPopupMenu( X, Y: Integer );
var
  P: TPoint;
  Year, Month, Day: Word;
  I: Integer;
begin
  P := Point( X, Y );
  P := ClientToScreen( P );
  DecodeDate( FViewDate, Year, Month, Day );
  for I := 0 to 11 do
    FMonthPopupMenu.Items[ I ].Checked := I = ( Month - 1 );
  FMonthPopupMenu.Popup( P.X, P.Y );
end;


procedure TRzCalendar.MonthPopupClickHandler( Sender: TObject );
var
  Item: TMenuItem;
  Year, Month, Day: Word;
begin
  Item := Sender as TMenuItem;
  // Item.Tag represents the month number
  DecodeDate( FViewDate, Year, Month, Day );
  SetViewDate( IncMonth( FViewDate, - ( Month - Item.Tag ) ) );
end;


procedure TRzCalendar.HideYearSpinner;
begin
  if FYearSpinnerVisible then
  begin
    FYearSpinnerVisible := False;
    SetWindowPos( FYearSpinner.Handle, 0, 0, 0, 0, 0,
                  swp_NoActivate or swp_NoZOrder or swp_NoMove or swp_NoSize or
                  swp_HideWindow );
    SetFocus;
  end;
end;


procedure TRzCalendar.ShowYearSpinner;
var
  R: TRect;
  Areas: TRzCalendarAreas;
  Year, Month, Day: Word;
  MaxYear, MaxMonth, MaxDay: Word;
  MinYear, MinMonth, MinDay: Word;
  W: Integer;
  OldFontStyle: TFontStyles;
begin
  DecodeDate( FViewDate, Year, Month, Day );
  TRzYearSpinner( FYearSpinner ).Value := Year;

  if FMaxDate <> 0.0 then
  begin
    DecodeDate( FMaxDate, MaxYear, MaxMonth, MaxDay );
    TRzYearSpinner( FYearSpinner ).Max := MaxYear;
    TRzYearSpinner( FYearSpinner ).CheckRange := True;
  end;

  if FMinDate <> 0.0 then
  begin
    DecodeDate( FMinDate, MinYear, MinMonth, MinDay );
    TRzYearSpinner( FYearSpinner ).Min := MinYear;
    TRzYearSpinner( FYearSpinner ).CheckRange := True;
  end;

  CalcAreas( Areas );
  R := Areas[ caYear ];
  InflateRect( R, 0, 1 );

  OldFontStyle := Canvas.Font.Style;
  Canvas.Font.Style := Font.Style;

  R.Left := Areas[ caMonth ].Right -
            TRzYearSpinner( FYearSpinner ).ButtonWidth -
            Canvas.TextWidth( ' ' );

  W := Canvas.TextWidth( FormatDateTime( 'yyyy', FViewDate ) );
  R.Right := R.Left + W + 2 * Canvas.TextWidth( ' ' ) +
             2 * TRzYearSpinner( FYearSpinner ).ButtonWidth;

  Canvas.Font.Style := OldFontStyle;

  SetWindowPos( FYearSpinner.Handle, 0,
                R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
                swp_NoActivate or swp_ShowWindow );
  Windows.SetFocus( FYearSpinner.Handle );
  FYearSpinnerVisible := True;
end;


procedure TRzCalendar.YearSpinnerDecrementHandler( Sender: TObject; Amount: Integer );
var
  NewViewDate: TDateTime;
  Y, M, D, MinYear, MinMonth, MinDay: Word;
begin
  if FYearSpinnerVisible then
  begin
    DecodeDate( FViewDate, Y, M, D );
    if Y - Amount <= 0 then
      Exit;
    NewViewDate := IncYear( FViewDate, -Amount );

    if ( FMinDate <> 0.0 ) and ( NewViewDate < FMinDate ) then
    begin
      DecodeDate( FMinDate, MinYear, MinMonth, MinDay );
      NewViewDate := EncodeDate( MinYear, MinMonth, 1 );
    end;
    SetViewDate( NewViewDate );
  end;
end;


procedure TRzCalendar.YearSpinnerIncrementHandler( Sender: TObject; Amount: Integer );
var
  NewViewDate: TDateTime;
  MaxYear, MaxMonth, MaxDay: Word;
begin
  if FYearSpinnerVisible then
  begin
    NewViewDate := IncYear( FViewDate, Amount );
    if ( FMaxDate <> 0.0 ) and ( NewViewDate > FMaxDate ) then
    begin
      DecodeDate( FMaxDate, MaxYear, MaxMonth, MaxDay );
      NewViewDate := EncodeDate( MaxYear, MaxMonth, 1 );
    end;
    SetViewDate( NewViewDate );
  end;
end;


procedure TRzCalendar.UpdateHighlight( X, Y: Integer );
var
  HighlightDate: TDateTime;
  DoChange: Boolean;
begin
  if ( FPressedArea = caDays ) and ( FOverArea = caDays ) then
  begin
    HighlightDate := InternalHitTest( FMouseOverRect, Point( X, Y ) );

    if ( HighlightDate <> 0 ) and ( DateInRange( HighlightDate ) = 0 ) then
    begin
      DoChange := Trunc( FDate ) <> HighlightDate;

      FDate := HighlightDate;
      try
        if DoChange then
          Changed;
      finally
        Invalidate;
      end;
    end
    else
      FIgnoreClick := True;
  end;
end;


function TRzCalendar.InternalHitTest( R: TRect; P: TPoint ): TDateTime;
var
  Row, Col: Integer;
  CellRect: TRect;
  Year, Month, Day, ViewMonth: Word;
  CellDate: TDateTime;
begin
  Result := 0;
  if PtInRect( R, P ) then
  begin
    DecodeDate( FViewDate, Year, ViewMonth, Day );
    CellRect := Bounds( R.Left, R.Top - 1, FCellSize.X, FCellSize.Y );

    CellDate := FFirstDateInGrid;
    for Row := 0 to 5 do
    begin
      for Col := 0 to 6 do
      begin
        DecodeDate( CellDate, Year, Month, Day );
        if ( ceFillDays in FElements ) or ( Month = ViewMonth ) then
        begin
          if PtInRect( CellRect, P ) then
          begin
            Result := CellDate;
            Exit;
          end;
        end;
        OffsetRect( CellRect, FCellSize.X, 0 );
        CellDate := CellDate + 1;
      end;
      OffsetRect( CellRect, R.Left - CellRect.Left, FCellSize.Y );
    end;
  end;
end; {= TRzCalendar.InternalHitTest =}


function TRzCalendar.HitTest( X, Y: Integer ): TDateTime;
var
  Areas: TRzCalendarAreas;
begin
  CalcAreas( Areas );
  Result := InternalHitTest( Areas[ caDays ], Point( X, Y ) );
end;


procedure TRzCalendar.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  if Button = mbLeft then
  begin
    SetPressedArea( caFillDays );
    StopTimer;
  end;
  inherited;
end;


procedure TRzCalendar.Changed;
begin
  if Assigned( FOnChange ) then
    FOnChange( Self );
end;


procedure TRzCalendar.Click;
var
  Year, ViewYear, Month, ViewMonth, Day, ViewDay: Word;
begin
  {&RV}
  if ( FPressedArea = FOverArea ) and ( FPressedArea in [ caDays, caTodayButton, caClearButton ] ) then
  begin
    case FPressedArea of
      caClearButton:
      begin
        FClearClicked := True;
        Clear;
      end;

      caTodayButton:
        Today;

      caDays:
      begin
        if not FIgnoreClick then
        begin
          // This only needs to be called if the user picked a "filled" date
          // I should be able to determine when this happens by comparing the
          // FViewDate month and the FDate month.  If they are different then I
          // need to adjust the view month.
          DecodeDate( FDate, Year, Month, Day );
          DecodeDate( FViewDate, ViewYear, ViewMonth, ViewDay );
          if ( Year <> ViewYear ) or ( Month <> ViewMonth ) then
            SetViewDate( FDate );
        end;
      end;
    end;
    if not FIgnoreClick then
      inherited;
  end;
end;


procedure TRzCalendar.DblClick;
begin
  SetPressedArea( FOverArea );
  if FPressedArea in [ caDays, caTodayButton, caClearButton ] then
    inherited;
end;


function TRzCalendar.DaysToBitmask( Days: array of Byte ): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := Low( Days ) to High( Days ) do
  begin
    if Days[ I ] in [ 1..31 ] then
      Result := Result or ( $00000001 shl ( Days[ I ] - 1 ) );
  end;
end;


procedure TRzCalendar.GetBoldDays( Year, Month: Word; var Bitmask: Cardinal );
begin
  if Assigned( FOnGetBoldDays ) then
    FOnGetBoldDays( Self, Year, Month, Bitmask );
end;


function TRzCalendar.GetDayFormat( DayDate: TDateTime; Year, Month, Day: Word;
                                   var DayColor, DayFontColor: TColor;
                                   var DayFontStyle: TFontStyles ): Boolean;
begin
  Result := False;
  if Assigned( FOnGetDayFormat ) then
  begin
    Result := True;
    FOnGetDayFormat( Self, DayDate, Year, Month, Day,
                     DayColor, DayFontColor, DayFontStyle );
  end;
end;


procedure TRzCalendar.DoEnter;
begin
  inherited;
  Invalidate;
end;


procedure TRzCalendar.DoExit;
begin
  inherited;
  Invalidate;
end;


procedure TRzCalendar.KeyDown( var Key: Word; Shift: TShiftState );
var
  D, M, Y: Word;
  BaseDate: TDateTime;
begin
  inherited;

  if Shift = [ ] then
  begin
    if IsClear then
      BaseDate := SysUtils.Date
    else
      BaseDate := FDate;

    case Key of
      vk_Up:
        SetDate( BaseDate - 7 );

      vk_Down:
        SetDate( BaseDate + 7 );

      vk_Left:
        SetDate( BaseDate - 1 );

      vk_Right:
        SetDate( BaseDate + 1 );

      vk_Home:
      begin
        DecodeDate( BaseDate, Y, M, D );
        SetDate( EncodeDate( Y, M, 1 ) );
      end;

      vk_End:
      begin
        DecodeDate( IncMonth( BaseDate, 1 ), Y, M, D );
        SetDate( EncodeDate( Y, M, 1 ) - 1 );
      end;
    end;

    if ceMonth in FElements then
    begin
      case Key of
        vk_Prior:
          SetDate( IncMonth( BaseDate, -1 ) );

        vk_Next:
          SetDate( IncMonth( BaseDate, 1 ) );
      end;
    end;

    if Key = vk_Return then
      inherited Click;
  end;
end; {= TRzCalendar.KeyDown =}


procedure TRzCalendar.SetButtonColor( Value: TColor );
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    Invalidate;
  end;
end;


procedure TRzCalendar.SetButtonFontColor( Value: TColor );
begin
  if FButtonFontColor <> Value then
  begin
    FButtonFontColor := Value;
    Invalidate;
  end;
end;


procedure TRzCalendar.SetCalendarColors( Value: TRzCalendarColors );
begin
  FCalendarColors.Assign( Value );
end;


procedure TRzCalendar.SetCaptionClearBtn( const Value: TCaption );
begin
  if FCaptionClearBtn <> Value then
  begin
    FCaptionClearBtn := Value;
    AdjustForFont;
  end;
end;


procedure TRzCalendar.SetCaptionTodayBtn( const Value: TCaption );
begin
  if FCaptionTodayBtn <> Value then
  begin
    FCaptionTodayBtn := Value;
    AdjustForFont;
  end;
end;


function TRzCalendar.IsClear: Boolean;
begin
  Result := Trunc( FDate ) = 0;
end;


function TRzCalendar.CanClear: Boolean;
begin
  Result := True;
  if Assigned( FOnClear ) then
    FOnClear( Self, Result );
end;



procedure TRzCalendar.Clear;
begin
  if CanClear then
  begin
    FForceUpdate := True;
    try
      SetDate( 0 );
    finally
      FForceUpdate := False;
    end;
  end;
end;


function TRzCalendar.CanGoToToday: Boolean;
begin
  Result := True;
  if Assigned( FOnToday ) then
    FOnToday( Self, Result );
end;


procedure TRzCalendar.Today;
begin
  if CanGoToToday then
  begin
    FForceUpdate := True;
    try
      SetDate( SysUtils.Date );
    finally
      FForceUpdate := False;
    end;
  end;
end;



procedure TRzCalendar.TodayTimerExpired( Sender: TObject );
var
  D: TDateTime;
begin
  D := SysUtils.Date;
  if Trunc( FToday ) <> Trunc( D ) then
  begin
    // Today's date changed, so repaint the display.
    FToday := D;
    Invalidate;
  end;
end;


function TRzCalendar.DateInRange( Value: TDateTime ): Integer;
begin
  Result := 0;
  if ( FMaxDate <> 0.0 ) and ( Value > FMaxDate ) then
  begin
    Result := 1;
    Exit;
  end;

  if ( FMinDate <> 0.0 ) and ( Value < FMinDate ) then
  begin
    Result := -1;
    Exit;
  end;
end;


function TRzCalendar.ViewMonthInRange( Value: TDateTime ): Boolean;
var
  MaxYear, MaxMonth, MaxDay: Word;
  MinYear, MinMonth, MinDay: Word;
  MinMonthYear, MaxMonthYear: TDateTime;
begin
  Result := True;

  if FMaxDate <> 0.0 then
  begin
    DecodeDate( FMaxDate, MaxYear, MaxMonth, MaxDay );
    MaxMonthYear := EncodeDate( MaxYear, MaxMonth, 1 );
    if Value > MaxMonthYear then
    begin
      Result := False;
      Exit;
    end;
  end;

  if FMinDate <> 0.0 then
  begin
    DecodeDate( FMinDate, MinYear, MinMonth, MinDay );
    MinMonthYear := EncodeDate( MinYear, MinMonth, 1 );
    if Value < MinMonthYear then
    begin
      Result := False;
      Exit;
    end;
  end;
end; {= TRzCalendar.ViewMonthInRange =}


procedure TRzCalendar.SetDateProperty( Value: TDateTime );
var
  RangeCheck: Integer;
begin
  RangeCheck := DateInRange( Value );
  if RangeCheck > 0 then
    raise ERzCalendarError.CreateFmt( SRzDateMax, [ DateToStr( FMaxDate ) ] )
  else if RangeCheck < 0 then
    raise ERzCalendarError.CreateFmt( SRzDateMin, [ DateToStr( FMinDate ) ] );

  SetDate( Value );
end;


procedure TRzCalendar.SetDate( Value: TDateTime );
begin
  if FForceUpdate or
     ( ( FDate <> Trunc( Value ) ) and  ( DateInRange( Value ) = 0 ) ) then
  begin
    // Remove any Time from Value
    FDate := Trunc( Value );

    if FDate = 0 then
      SetViewDate( SysUtils.Date )
    else
      SetViewDate( FDate );

    try
      Changed;
    finally
      Invalidate;
    end;
  end;
end; {= TRzCalendar.SetDate =}


function TRzCalendar.StoreMinDate: Boolean;
begin
  Result := FMinDate <> 0.0;
end;


procedure TRzCalendar.SetMinDate( Value: TDateTime );
var
  NewMinDate: TDateTime;
begin
  NewMinDate := Trunc( Value );
  if ( NewMinDate <> 0.0 ) and ( FMaxDate <> 0.0 ) and ( NewMinDate > FMaxDate ) then
    raise ERzCalendarError.CreateFmt( SRzDateMax, [ DateToStr( FMaxDate ) ] );

  if FMinDate <> NewMinDate then
  begin
    SetRange( NewMinDate, FMaxDate );
    FMinDate := NewMinDate;
  end;
end;


function TRzCalendar.StoreMaxDate: Boolean;
begin
  Result := FMaxDate <> 0.0;
end;


procedure TRzCalendar.SetMaxDate( Value: TDateTime );
var
  NewMaxDate: TDateTime;
begin
  NewMaxDate := Trunc( Value );
  if ( NewMaxDate <> 0.0 ) and ( FMinDate <> 0.0 ) and ( NewMaxDate < FMinDate ) then
    raise ERzCalendarError.CreateFmt( SRzDateMin, [ DateToStr( FMinDate ) ] );

  if FMaxDate <> NewMaxDate then
  begin
    SetRange( FMinDate, NewMaxDate );
    FMaxDate := NewMaxDate;
  end;
end;


procedure TRzCalendar.SetRange( MinValue, MaxValue: TDate );
var
  TruncDate, TruncMin, TruncMax: Int64;
begin
  // Ignore range checking if current date is empty
  if FDate = 0.0 then
    Exit;

  TruncMin := Trunc( MinValue );
  TruncMax := Trunc( MaxValue );
  TruncDate := Trunc( FDate );

  if TruncMin <> 0 then
  begin
    if TruncDate < TruncMin then
      SetDate( MinValue );
  end;

  if TruncMax <> 0 then
  begin
    if TruncDate > TruncMax then
      SetDate( MaxValue );
  end;
end;


procedure TRzCalendar.SetFirstDayOfWeek( Value: TRzFirstDayOfWeek );
var
  A: array[ 0..1 ] of Char;
begin
  if HandleAllocated then
  begin
    if Value = fdowLocale then
    begin
      GetLocaleInfo( locale_User_Default, locale_IFirstDayOfWeek, A, SizeOf( A ) );
      FFirstDay := Ord( A[ 0 ] ) - Ord( '0' );
    end
    else
      FFirstDay := Ord( Value );
  end;
  FFirstDayOfWeek := Value;
  FForceUpdate := True;
  try
    SetDate( FDate );  // This will rebuild the calendar days
  finally
    FForceUpdate := False;
  end;
end;


procedure TRzCalendar.SetElements( Value: TRzCalendarElements );
begin
  if FElements <> Value then
  begin
    FElements := Value;
    AdjustForFont;
  end;
end;


procedure TRzCalendar.SetOverArea( Value: TRzCalendarArea );
var
  NeedToInvalidate: Boolean;
begin
  if FOverArea <> Value then
  begin
    NeedToInvalidate := FOverArea = FPressedArea;
    FOverArea := Value;
    NeedToInvalidate := NeedToInvalidate or ( FOverArea = FPressedArea );
    if NeedToInvalidate and not IsRectEmpty( FMouseOverRect ) then
      InvalidateRect( Handle, @FMouseOverRect, False );
  end;
end;


procedure TRzCalendar.SetPressedArea( Value: TRzCalendarArea );
begin
  if FPressedArea <> Value then
  begin
    FPressedArea := Value;
    if not IsRectEmpty( FMouseOverRect ) then
      InvalidateRect( Handle, @FMouseOverRect, False );
  end;
end;


procedure TRzCalendar.SetShowDays( Value: Boolean );
begin
  if FShowDays <> Value then
  begin
    FShowDays := Value;
    AdjustForFont;
  end;
end;


procedure TRzCalendar.ViewDateChange( ViewDate: TDateTime );
begin
  if Assigned( FOnViewDateChange ) then
    FOnViewDateChange( Self, ViewDate );
end;


procedure TRzCalendar.SetViewDate( Value: TDateTime );
var
  Year, Month, Day: Word;
  NewViewDate, PrevMonthLastDay: TDateTime;
begin
  DecodeDate( Value, Year, Month, Day );
  NewViewDate := EncodeDate( Year, Month, 1 );

  if FForceUpdate or
     ( ( FViewDate <> NewViewDate ) and ViewMonthInRange( NewViewDate ) ) then
  begin
    FViewDate := NewViewDate;
    ViewDateChange( FViewDate );

    PrevMonthLastDay := FViewDate - 1;
    if Year > 0 then
    begin
      DecodeDate( PrevMonthLastDay, Year, Month, Day );

      // Day will be 31 for Jan, Mar, May Jul, Aug, Oct, Dec
      // Day will be 30 for Apr, Jun, Sep, Nov
      // Day will be 28 for Feb (29 on leap years)

      Day := Day - ( ( DayOfWeek( PrevMonthLastDay ) + 5 - FFirstDay ) mod 7 );
    end;
    FFirstDateInGrid := EncodeDate( Year, Month, Day );
    Invalidate;
  end;
end;


procedure TRzCalendar.SetThemeAware( Value: Boolean );
begin
  if FThemeAware <> Value then
  begin
    FThemeAware := Value;
    Invalidate;
  end;
end;


procedure TRzCalendar.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  AdjustForFont;
end;


procedure TRzCalendar.CMDialogChar( var Msg: TCMDialogChar );
begin
  if Enabled then
  begin
    if IsAccel( Msg.CharCode, FCaptionClearBtn ) then
    begin
      Clear;
      Msg.Result := 1;
    end
    else if IsAccel( Msg.CharCode, FCaptionTodayBtn ) then
    begin
      Today;
      Msg.Result := 1;
    end;
  end;
end;


procedure TRzCalendar.WMEraseBkgnd( var Msg: TMessage );
begin
  Msg.Result := 1;
end;


procedure TRzCalendar.WMNCHitTest( var Msg: TWMNCHitTest );
var
  P: TPoint;
  Area: TRzCalendarArea;
  Areas: TRzCalendarAreas;
begin
  inherited;
  CalcAreas( Areas );
  P := ScreenToClient( Point( Msg.XPos, Msg.YPos ) );
  for Area := High( Area ) downto Low( Area ) do
  begin
    if PtInRect( Areas[ Area ], P ) then
    begin
      SetOverArea( Area );
      Exit;
    end;
  end;
  SetOverArea( caFillDays );
end;


procedure TRzCalendar.WMTimer( var Msg: TMessage );
begin
  TimerExpired;
end;


procedure TRzCalendar.WMGetDlgCode( var Msg: TWMGetDlgCode );
begin
  Msg.Result := dlgc_WantArrows;
end;



{===========================}
{== TRzTimePicker Methods ==}
{===========================}

constructor TRzTimePicker.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ( ControlStyle - [ csAcceptsControls, csNoStdEvents, csSetCaption ] ) + [ csReflector ];

  FIsPopup := False;
  FTime := SysUtils.Time;
  Clear;

  FForceUpdate := False;

  FClockFaceColors := TRzClockFaceColors.Create( Self );

  FThemeAware := True;

  Color := clWindow;
  FButtonColor := clBtnFace;
  FButtonFontColor := clWindowText;
  FlatColorAdjustment := 0;
  BorderOuter := fsLowered;
  TabStop := True;
  AutoSize := True;
  DoubleBuffered := True;
  AdjustForFont;
  {&RCI}

  FShowSetButton := False;

  Width := 160;
  Height := 190;

  FRestrictMinutes := False;
  FRestrictMinutesBy := 5;
  FShowHowToUseHint := True;
  FShowTime := True;

  FTimer := TTimer.Create( Self );
  FTimer.OnTimer := CheckHintWindowHandler;
  FTimer.Interval := 1000;
  FTimer.Enabled := False;

  // If this is True, then control flickers like crazy during mouse move
  ParentBackground := False;
end;


destructor TRzTimePicker.Destroy;
begin
  FClockFaceColors.Free;
  inherited;
end;


procedure TRzTimePicker.Paint;
var
  S, FormatStr: string;
  DrawFlat: Boolean;
  R: TRect;
  C, TempC: TColor;
  {$IFDEF VCL160_OR_HIGHER}
  Details: TThemedElementDetails;
  B: TBitmap;
  {$ENDIF}

  procedure DrawLine( ALeft, ARight, AHeight: Integer );
  begin
    if UsingSystemStyle then
    begin
      Canvas.Pen.Color := FlatColor;
    end
    else // VCL Styles
    begin
      Canvas.Pen.Color := ActiveStyleFontColor( sfEditBoxTextDisabled );
    end;
    Canvas.MoveTo( ALeft, AHeight );
    Canvas.LineTo( ARight, AHeight );
  end;


  function CheckColor( Value: TColor ): TColor;
  begin
    // Need to check color against TransparentColor and WinMaskColor
    if ColorToRGB( Value ) = ColorToRGB( clGray ) then
    begin
      Result := ColorToRGB( Value ) + 1;
    end
    else
      Result := Value;
  end;


  procedure DisplayRadioButton( Bounds: TRect; const Caption: string; Checked: Boolean );
  var
    W, H, Offset: Integer;
    R: TRect;
    Bmp: TBitmap;
    Details: TThemedElementDetails;
  begin
    Bmp := TBitmap.Create;
    try
      if UsingSystemStyle then
      begin
        W := 13;
        H := 13;
      end
      else // VCL Styles
      begin
        // For now, hard-code to 14 because there is no way to get the true
        // size of the radio button image from the style.
        W := 14;
        H := 14;
      end;

      R := Rect( 0, 0, W, H );
      Bmp.Width := W;
      Bmp.Height := H;


      if UsingSystemStyle then
      begin
        Bmp.Canvas.Brush.Color := Color;
        Bmp.Canvas.FillRect( R );

        if FThemeAware and ActiveStyleServicesEnabled then
        begin
          if Checked then
            Details := ActiveStyleServices.GetElementDetails( tbRadioButtonCheckedNormal )
          else
            Details := ActiveStyleServices.GetElementDetails( tbRadioButtonUncheckedNormal );
          ActiveStyleServices.DrawElement( Bmp.Canvas.Handle, Details, R );
        end
        else
        begin
          DrawRadioButton( Bmp.Canvas, R, Checked, bdsNormal, False, htsFrame,
                           FlatColor, clHighlight, clWindow, clWindow, clBtnFace,
                           clNone, clNone, Color, False, Color, False, False, clWindow );
        end;
      end
      else // VCL Styles
      begin
        Bmp.Canvas.Brush.Color := ActiveStyleColor( scEdit );
        Bmp.Canvas.FillRect( R );

        if Checked then
          Details := ActiveStyleServices.GetElementDetails( tbRadioButtonCheckedNormal )
        else
          Details := ActiveStyleServices.GetElementDetails( tbRadioButtonUncheckedNormal );
        ActiveStyleServices.DrawElement( Bmp.Canvas.Handle, Details, R );
      end;

      Offset := ( Bounds.Bottom - Bounds.Top - H ) div 2;
      R := Classes.Bounds( Bounds.Left, Bounds.Top + Offset, W, H );
      Canvas.Draw( R.Left, R.Top, Bmp );
    finally
      Bmp.Free;
    end;

    if UsingSystemStyle then
    begin
      Canvas.Font.Color := Self.Font.Color;
      Canvas.Brush.Color := Color;
    end
    else // VCL Styles
    begin
      Canvas.Brush.Color := ActiveStyleColor( scEdit );
      Canvas.Font.Color := ActiveStyleFontColor( sfEditBoxTextNormal );
    end;
    Inc( Bounds.Left, 15 );
    DrawStringCentered( Canvas, Caption, Bounds );
  end; {= DisplayRadioButton =}


begin {= TRzTimePicker.Paint =}
  if UsingSystemStyle then
  begin
    if FThemeAware and ActiveStyleServicesEnabled then
    begin
      C := GetXPThemeColor( xptcEditBorder );
      R := Rect( 0, 0, Width, Height );
      R := DrawBox( Canvas, R, C );
      Canvas.Brush.Color := Color;
      Canvas.FillRect( R );
    end
    else
      inherited;
  end
  else // VCL Styles
  begin
    {$IFDEF VCL160_OR_HIGHER}
    R := Rect( 0, 0, Width, Height );
    Details := StyleServices.GetElementDetails( teEditTextNormal );

    B := TBitmap.Create;
    try
      B.Width := Width;
      B.Height := Height;
        StyleServices.DrawElement( B.Canvas.Handle, Details, R );
      Canvas.Draw( 0, 0, B );
    finally
      B.Free;
    end;
    {$ENDIF}
  end;


  CalcRects;

  Canvas.Font := Self.Font;

  DrawFlat := BorderOuter in [ fsFlat, fsFlatBold, fsFlatRounded ];

  // Draw the Time
  if FShowTime then
  begin
    R := FTimeRect;

    if UsingSystemStyle then
    begin
      if FThemeAware and ActiveStyleServicesEnabled then
      begin
        GetGradientPanelColors( gcsMSOffice, TempC, C );
      end
      else
      begin
        if not DrawFlat then
          R := DrawBorder( Canvas, R, fsPopup );
        C := FButtonColor;
      end;
    end
    else // VCL Styles
    begin
      C := ActiveStyleColor( scGenericGradientBase );
    end;

    Canvas.Brush.Color := C;
    Canvas.FillRect( R );


    Canvas.Brush.Style := bsClear;
    if FFormat = '' then
      FormatStr := 'h:mm ampm'
    else
      FormatStr := FFormat;

    Canvas.Font.Color := GetButtonFontColor;
    DrawStringCentered( Canvas, FormatDateTime( FormatStr, FTime ), FTimeRect );
    Canvas.Brush.Style := bsSolid;
  end; { if FShowTime }

  // Draw the Clock
  DrawClock( FClockRect, FClockCenter, FRadius );
  Canvas.Font := Self.Font;                                // Reset font in case DrawClock changed it

  // Draw Separator Line
  DrawLine( 5, Width - 5, FAMRect.Top - 5 );

  // Draw AM Button
  if FCaptionAM <> '' then
    S := FCaptionAM
  else
    S := sRzCaptionAM;
  DisplayRadioButton( FAMRect, S, not FTimeIsPM );

  // Draw PM Button
  if FCaptionPM <> '' then
    S := FCaptionPM
  else
    S := sRzCaptionPM;
  DisplayRadioButton( FPMRect, S, FTimeIsPM );

  // Draw Set Radio Button
  if FShowSetButton then
  begin
    if FCaptionSet <> '' then
      S := FCaptionSet
    else
      S := sRzCaptionSet;
    DrawButton( Canvas, FSetRect, S, FButtonColor, GetButtonFontColor, DrawFlat,
                FPressingLeft and FMouseOverSet, FThemeAware );
  end;
end; {= TRzTimePicker.Paint =}


function TRzTimePicker.GetButtonFontColor: TColor;
begin
  if UsingSystemStyle then
  begin
    Result := FButtonFontColor;
  end
  else // VCL Styles
  begin
    Result := ActiveStyleFontColor( sfButtonTextNormal );
  end;
end;


procedure TRzTimePicker.DrawClock( Bounds: TRect; CenterPoint: TPoint; Radius: Integer );
var
  I, X, Y, R, K, HX, HY, HL, Offset: Integer;
  Hour, Min, Sec, MSec: Word;
  OldTextAlign: Cardinal;
  S: string;
  P1, P2, P3, P4: TPoint;
  MinuteTickColor: TColor;

  procedure GetHoursXY( Hours: Integer; Radius: Integer; var X, Y: Integer );
  var
    Angle: Extended;
  begin
    Angle := ( Hours * Pi / 6 ) - ( Pi / 2 );

    X := Round( Radius * Cos( Angle ) );
    Y := Round( Radius * Sin( Angle ) );
  end;


  procedure GetHoursMinutesXY( Hours, Minutes: Integer; Radius: Integer; var X, Y: Integer );
  var
    Angle: Extended;
  begin
    Angle := ( Hours * Pi / 6 ) + ( Minutes * Pi / 360 ) - ( Pi / 2 );

    X := Round( Radius * Cos( Angle ) );
    Y := Round( Radius * Sin( Angle ) );
  end;


  procedure GetMinutesXY( Minutes: Integer; Radius: Integer; var X, Y: Integer );
  var
    Angle: Extended;
  begin
    Angle := ( Minutes * Pi / 30 ) - ( Pi / 2 );

    X := Round( Radius * Cos( Angle ) );
    Y := Round( Radius * Sin( Angle ) );
  end;


  procedure GetAngleXY( const Angle: Extended; Radius: Integer; var X, Y: Integer );
  begin
    X := Round( Radius * Cos( Angle ) );
    Y := Round( Radius * Sin( Angle ) );
  end;

begin {= TRzTimePicker.DrawClock =}
  if UsingSystemStyle then
  begin
    if Focused then
      Canvas.Pen.Color := DarkerColor( FClockFaceColors.Face, 40 )
    else
      Canvas.Pen.Color := FClockFaceColors.Face;

    Canvas.Brush.Color := FClockFaceColors.Face;
  end
  else // VCL Styles
  begin
    if Focused then
      Canvas.Pen.Color := ActiveStyleColor( scGenericGradientEnd )
    else
      Canvas.Pen.Color := ActiveStyleColor( scWindow );

    Canvas.Brush.Color := ActiveStyleColor( scWindow );
  end;


  Canvas.Font.Style := [ fsBold ];
  Canvas.Font.Height := -Round( 0.20 * Radius );
  Canvas.Ellipse( Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom );

  R := Round( 0.75 * Radius );


  // Draw Numbers

  OldTextAlign := GetTextAlign( Canvas.Handle );
  SetTextAlign( Canvas.Handle, ta_Center );
  Offset := Canvas.TextHeight( '0' ) div 2;


  for I := 1 to 12 do
  begin
    S := IntToStr( I );
    GetHoursXY( I, R, X, Y );
    if UsingSystemStyle then
      Canvas.Font.Color := FClockFaceColors.Numbers
    else
    begin
      Canvas.Font.Color := ActiveStyleFontColor( sfButtonTextNormal );
    end;
    Canvas.TextOut( CenterPoint.X + X, CenterPoint.Y + Y - Offset, S );
  end;

  // Draw Ticks
  if UsingSystemStyle then
  begin
    Canvas.Brush.Color := FClockFaceColors.HourTicks;
    Canvas.Pen.Color := FClockFaceColors.HourTicks;
    MinuteTickColor := FClockFaceColors.MinuteTicks;
  end
  else // VCL Styles
  begin
    Canvas.Brush.Color := ActiveStyleColor( scGenericGradientBase );
    Canvas.Pen.Color := ActiveStyleColor( scGenericGradientBase );
    MinuteTickColor := ActiveStyleFontColor( sfEditBoxTextDisabled );
  end;


  R := Round( 0.92 * Radius );
  K := Round( 0.02 * Radius );
  for I := 1 to 60 do
  begin
    GetMinutesXY( I, R, X, Y );

    if I mod 5 = 0 then
      Canvas.Rectangle( CenterPoint.X + X - K, CenterPoint.Y + Y - K, CenterPoint.X + X + K + 1, CenterPoint.Y + Y + K + 1 )
    else
    begin
      Canvas.Pixels[ CenterPoint.X + X, CenterPoint.Y + Y ] := MinuteTickColor;
    end;
  end;


  DecodeTime( FTime, Hour, Min, Sec, MSec );
  if Hour > 12 then
    Dec( Hour, 12 );

  // Draw the Big Hand
  P1 := CenterPoint;

  R := Round( 0.80 * Radius );
  GetMinutesXY( Min, R, X, Y );
  P3 := Point( CenterPoint.X + X, CenterPoint.Y + Y );

  R := Round( 0.25 * Radius );
  GetMinutesXY( Min, R, HX, HY );

  HL := Round( 0.04 * Radius );
  GetAngleXY( Min * Pi / 30, HL, X, Y );
  P4 := Point( CenterPoint.X + HX + X, CenterPoint.Y + HY + Y );
  P2 := Point( CenterPoint.X + HX - X, CenterPoint.Y + HY - Y );

  if UsingSystemStyle then
  begin
    Canvas.Brush.Color := FClockFaceColors.Hands;
    Canvas.Pen.Color := FClockFaceColors.Hands;
  end
  else // VCL Styles
  begin
    Canvas.Brush.Color := ActiveStyleFontColor( sfEditBoxTextNormal );
    Canvas.Pen.Color := ActiveStyleFontColor( sfEditBoxTextNormal );
  end;

  Canvas.Polygon( [ P1, P2, P3, P4 ] );

  // Draw the Little Hand
  P1 := CenterPoint;

  R := Round( 0.50 * Radius );
  GetHoursMinutesXY( Hour, Min, R, X, Y );
  P3 := Point( CenterPoint.X + X, CenterPoint.Y + Y );

  R := Round( 0.15 * Radius );
  GetHoursMinutesXY( Hour, Min, R, HX, HY );

  HL := Round( 0.04 * Radius );
  GetAngleXY( ( Hour * Pi / 6 ) + ( Min * Pi / 360 ), HL, X, Y );
  P4 := Point( CenterPoint.X + HX + X, CenterPoint.Y + HY + Y );
  P2 := Point( CenterPoint.X + HX - X, CenterPoint.Y + HY - Y );

  Canvas.Polygon( [ P1, P2, P3, P4 ] );


  SetTextAlign( Canvas.Handle, OldTextAlign );
end; {= TRzTimePicker.DrawClock =}


procedure TRzTimePicker.CalcRects;
var
  Margin, W, H, FontHeight: Integer;
  S: string;
  CR: TRect;
begin
  CR := ClientRect;
  AdjustClientRect( CR );

  Canvas.Font := Font;

  FontHeight := GetMinFontHeight( Font ) + 4;
  Margin := 5;
  FTimeRect := CR;
  if FShowTime then
    FTimeRect.Bottom := FontHeight
  else
    FTimeRect.Bottom := FTimeRect.Top;;

  if FCaptionAM <> '' then
    S := FCaptionAM
  else
    S := sRzCaptionAM;
  FAMRect := Bounds( Margin, Height - Margin - FontHeight, Canvas.TextWidth( S ) + 15, FontHeight );

  if FCaptionPM <> '' then
    S := FCaptionPM
  else
    S := sRzCaptionPM;
  FPMRect := Bounds( FAMRect.Right + Margin, Height - Margin - FontHeight, Canvas.TextWidth( S ) + 15, FontHeight );

  if FCaptionSet <> '' then
    S := FCaptionSet
  else
    S := sRzCaptionSet;
  FSetRect := Rect( FPMRect.Right + ( Width - Margin - FPMRect.Right ) div 3, Height - Margin - FontHeight, Width - Margin, FPMRect.Bottom );

  W := Width - 10;
  H := FAMRect.Top - 5 - FTimeRect.Bottom - 5;
  FClockCenter.X := 5 + ( W div 2 );
  FClockCenter.Y := FTimeRect.Bottom + 3 + ( H div 2 );

  FRadius := Min( W div 2, H div 2 );
  FClockRect := Rect( FClockCenter.X - FRadius, FClockCenter.Y - FRadius, FClockCenter.X + FRadius, FClockCenter.Y + FRadius );
end;


procedure TRzTimePicker.AdjustClientRect( var Rect: TRect );
begin
  inherited;
  FixClientRect( Rect, False );
end;


procedure TRzTimePicker.AdjustForFont;
begin
  CalcFontSize;
  AdjustSize;
  Invalidate;
end;


procedure TRzTimePicker.CalcFontSize;
var
  Canvas: TCanvas;
  DC: HDC;
begin
  Canvas := TCanvas.Create;
  DC := GetDC( 0 );
  try
    Canvas.Handle := DC;
    Canvas.Font.Assign( Font );
    FCharSize.X := Canvas.TextWidth( '0' );
    FCharSize.Y := Canvas.TextHeight( '0' );
  finally
    Canvas.Handle := 0;
    Canvas.Free;
    ReleaseDC( 0, DC );
  end;
end; {= TRzTimePicker.CalcFontSize =}


procedure TRzTimePicker.ConstrainedResize( var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer );
begin
  CanAutoSize( MinWidth, MinHeight );
  inherited;
end;


function TRzTimePicker.CanAutoSize( var NewWidth, NewHeight: Integer ): Boolean;
var
  W1, W2: Integer;
begin
  NewHeight := FCharSize.Y * 10 + ( FCharSize.Y + 2 ) * 4;

  W1 := ( FCharSize.X + 6 ) * 13;
  if FCaptionSet = '' then
    W2 := FCharSize.X * 12 + 2 * FCharSize.X * 6
  else
    W2 := FCharSize.X * 12 + 2 * FCharSize.X * ( Length( FCaptionSet ) + 2 );

  NewWidth := Max( W1, W2 );

  Result := True;
end;


function TRzTimePicker.NormalizedArcTan( const Y, X: Extended ): Extended;
begin
  Result := ArcTan( Y / X );
  if ( X > 0 ) and ( Y < 0 ) then
    Result := Result + 2 * Pi
  else if ( X < 0 ) then
    Result := Result + Pi;
end;


function TRzTimePicker.GetHourFromXY( X, Y: Integer ): Integer;
var
  I, DX, DY: Integer;
  Angle: Extended;
begin
  Result := 12;
  DX := X - FClockCenter.X;
  DY := Y - FClockCenter.Y;
  if DX <> 0 then
  begin
    Angle := NormalizedArcTan( DY, DX );

    Angle := Angle + ( Pi / 3 );
    if Angle > 2 * Pi then
      Angle := Angle - 2 * Pi;

    for I := 1 to 12 do
    begin
      if Angle < ( I * ( Pi / 6 ) - ( Pi / 12 ) ) then
      begin
        Result := I;
        Break;
      end;
    end;

  end
  else if DY > 0 then
    Result := 6
  else
    Result := 12;
end;


function TRzTimePicker.GetMinuteFromXY( X, Y: Integer; Restrict: Boolean ): Integer;
var
  I, DX, DY: Integer;
  Angle: Extended;
begin
  Result := 0;
  DX := X - FClockCenter.X;
  DY := Y - FClockCenter.Y;
  if DX <> 0 then
  begin
    Angle := NormalizedArcTan( DY, DX );

    Angle := Angle + ( Pi / 2 );
    if Angle > 2 * Pi then
      Angle := Angle - 2 * Pi;

    for I := 1 to 60 do
    begin
      if Angle < ( I * ( Pi / 30 ) - ( Pi / 60 ) ) then
      begin
        Result := I - 1;
        Break;
      end;
    end;

    if FRestrictMinutes or Restrict then
    begin
      Result := Round( Result / 60 * ( 60 div FRestrictMinutesBy ) ) * FRestrictMinutesBy;
      if Result = 60 then
        Result := 0;
    end;
  end
  else if DY > 0 then
    Result := 30
  else
    Result := 0;
end;


procedure TRzTimePicker.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;
  {&RV}

  if not ( csDesigning in ComponentState ) and not FIsPopup then
    SetFocus;

  if Button = mbLeft then
  begin
    FPressingLeft := True;
    if PtInRect( FAMRect, Point( X, Y ) ) then
    begin
      FMouseOverAM := True;
    end
    else if PtInRect( FPMRect, Point( X, Y ) ) then
    begin
      FMouseOverPM := True;

    end
    else if PtInRect( FSetRect, Point( X, Y ) ) then
    begin
      FMouseOverSet := True;

    end
    else if PtInRect( FClockRect, Point( X, Y ) ) then
    begin
      // Select Hours
      FMouseOverClock := True;
      SetHour( GetHourFromXY( X, Y ) );
    end;
    Invalidate;
  end
  else if Button = mbRight then
  begin
    FPressingRight := True;
    if PtInRect( FClockRect, Point( X, Y ) ) then
    begin
      FMouseOverClock := True;
      FLastMinute := GetMinuteFromXY( X, Y, ssCtrl in Shift );
      SetMinutes( FLastMinute );
    end;

  end;
end; {= TRzTimePicker.MouseDown =}


procedure TRzTimePicker.MouseMove( Shift: TShiftState; X, Y: Integer );
var
  M: Integer;
begin
  if ShowHowToUseHint then
    DoHint( X, Y );

  inherited;

  if FPressingLeft then
  begin
    if FMouseOverClock and PtInRect( FClockRect, Point( X, Y ) ) then
      SetHour( GetHourFromXY( X, Y ) );
  end
  else if FPressingRight then
  begin
    if FMouseOverClock and PtInRect( FClockRect, Point( X, Y ) ) then
    begin
      M := GetMinuteFromXY( X, Y, ssCtrl in Shift );
      if ( M = 0 ) and ( FLastMinute > 30 ) then
        AdjustHour( 1 )
      else if ( M > 30  ) and ( FLastMinute < 30 ) then
        AdjustHour( -1 );
      FLastMinute := M;
      SetMinutes( M );
    end;
  end;

end;


procedure TRzTimePicker.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;

  if ( Button = mbLeft ) and FPressingLeft then
  begin
    if FMouseOverAM and PtInRect( FAMRect, Point( X, Y ) ) then
    begin
      ChangeToAM;
    end
    else if FMouseOverPM and PtInRect( FPMRect, Point( X, Y ) ) then
    begin
      ChangeToPM;
    end
    else if FMouseOverSet and PtInRect( FSetRect, Point( X, Y ) ) then
    begin
      // Probably generate an event here so that popup can close
      SetBtnClick;
    end
    else if FMouseOverClock and PtInRect( FClockRect, Point( X, Y ) ) then
    begin
      SetHour( GetHourFromXY( X, Y ) );
      if Assigned( FOnSetTime ) then
        FOnSetTime( Self );
    end;
  end
  else if ( Button = mbRight ) and FPressingRight then
  begin
    if FMouseOverClock and PtInRect( FClockRect, Point( X, Y ) ) then
    begin
      SetMinutes( GetMinuteFromXY( X, Y, ssCtrl in Shift ) );
      if Assigned( FOnSetTime ) then
        FOnSetTime( Self );
    end;
  end;

  FPressingLeft := False;
  FMouseOverAM := False;
  FMouseOverPM := False;
  FMouseOverSet := False;
  FMouseOverClock := False;

  Invalidate;
end; {= TRzTimePicker.MouseUp =}


procedure TRzTimePicker.CMMouseEnter( var Msg: TMessage );
var
  P: TPoint;
begin
  inherited;

  GetCursorPos( P );
  P := ScreenToClient( P );
  if FShowHowToUseHint then
    DoHint( P.X, P.Y );
end;


procedure TRzTimePicker.CMMouseLeave( var Msg: TMessage );
begin
  inherited;
  ReleaseHintWindow;
end;


procedure TRzTimePicker.AdjustHour( DeltaHours: Int64 );
var
  T: TTime;
begin
  T := Date + FTime;
  T := IncHour( T, DeltaHours );
  SetTime( T );
end;


procedure TRzTimePicker.AdjustMinute( DeltaMinutes: Int64 );
var
  T: TTime;
begin
  T := Date + FTime;
  T := IncMinute( T, DeltaMinutes );
  SetTime( T );
end;


procedure TRzTimePicker.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;

  case Key of
    vk_Prior:
      AdjustHour( 1 );
    vk_Next:
      AdjustHour( -1 );
    vk_Up:
      AdjustMinute( 1 );
    vk_Down:
      AdjustMinute( -1 );

    vk_Return:
    begin
      if FShowSetButton then
        SetBtnClick;
    end;
  end;
end;


procedure TRzTimePicker.DoEnter;
begin
  inherited;
  Invalidate;
end;


procedure TRzTimePicker.DoExit;
begin
  inherited;
  Invalidate;
end;


procedure TRzTimePicker.SetBtnClick;
begin
  if Assigned( FOnSetBtnClick ) then
    FOnSetBtnClick( Self );
end;


function TRzTimePicker.CalcHintRect( MaxWidth: Integer; const HintStr: string; HintWnd: THintWindow ): TRect;
begin
  Result := HintWnd.CalcHintRect( Screen.Width, HintStr, nil );
end;


procedure TRzTimePicker.DoHint( X, Y: Integer );
var
  R, WinRect: TRect;
  P: TPoint;
  HintStr: string;
begin
  if not ( csDesigning in ComponentState ) and PtInRect( FClockRect, Point( X, Y ) ) and ForegroundTask then
  begin
    Canvas.Font := Font;
    if not Assigned( FHintWnd ) then
    begin
      FHintWnd := HintWindowClass.Create( Self );
      FHintWnd.Color := Application.HintColor;
    end;

    if FHowToUseMsg = '' then
      HintStr := SysUtils.Format( sRzHowToSelectTime, [ FRestrictMinutesBy ] )
    else
      HintStr := FHowToUseMsg;
    R := CalcHintRect( Screen.Width, HintStr, FHintWnd );

    P := ClientToScreen( Point( 0, Height ) );
    OffsetRect( R, P.X, P.Y );

    GetWindowRect( FHintWnd.Handle, WinRect );

    if not IsWindowVisible( FHintWnd.Handle ) or not ( ( R.Left = WinRect.Left ) and ( R.Top = WinRect.Top ) ) then
    begin
      FHintWnd.ActivateHint( R, HintStr );
      FTimer.Enabled := True;
    end;
  end
  else
  begin
    FTimer.Enabled := False;
    ReleaseHintWindow;
    Repaint;
  end;
end;


procedure TRzTimePicker.CheckHintWindowHandler( Sender: TObject );
var
  P: TPoint;
begin
  GetCursorPos( P );
  P := ScreenToClient( P );
  if not PtInRect( FClockRect, P ) then
  begin
    FTimer.Enabled := False;
    ReleaseHintWindow;
    Repaint;
  end;
end;


procedure TRzTimePicker.ReleaseHintWindow;
begin
  if Assigned( FHintWnd ) then
    FHintWnd.ReleaseHandle;
end;


procedure TRzTimePicker.Changed;
begin
  if Assigned( FOnChange ) then
    FOnChange( Self );
end;


procedure TRzTimePicker.ChangeToAM;
var
  Hour, Min, Sec, MSec: Word;
begin
  if FTimeIsPM then
  begin
    FTimeIsPM := False;
    DecodeTime( FTime, Hour, Min, Sec, MSec );
    SetHour( Hour - 12 );
  end;
end;


procedure TRzTimePicker.ChangeToPM;
var
  Hour, Min, Sec, MSec: Word;
begin
  if not FTimeIsPM then
  begin
    FTimeIsPM := True;
    DecodeTime( FTime, Hour, Min, Sec, MSec );
    SetHour( Hour + 12 );
  end;
end;


procedure TRzTimePicker.SetButtonColor( Value: TColor );
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    Invalidate;
  end;
end;


procedure TRzTimePicker.SetButtonFontColor( Value: TColor );
begin
  if FButtonFontColor <> Value then
  begin
    FButtonFontColor := Value;
    Invalidate;
  end;
end;


procedure TRzTimePicker.SetCaptionAM( const Value: TCaption );
begin
  if FCaptionAM <> Value then
  begin
    FCaptionAM := Value;
    AdjustForFont;
  end;
end;


procedure TRzTimePicker.SetCaptionPM( const Value: TCaption );
begin
  if FCaptionPM <> Value then
  begin
    FCaptionPM := Value;
    AdjustForFont;
  end;
end;


procedure TRzTimePicker.SetCaptionSet( const Value: TCaption );
begin
  if FCaptionSet <> Value then
  begin
    FCaptionSet := Value;
    AdjustForFont;
  end;
end;


procedure TRzTimePicker.SetClockFaceColors( Value: TRzClockFaceColors );
begin
  FClockFaceColors.Assign( Value );
end;


function TRzTimePicker.IsClear: Boolean;
begin
  Result := Trunc( FTime ) = 0;
end;


procedure TRzTimePicker.Clear;
begin
  SetTime( 0 );
end;


procedure TRzTimePicker.SetFormat( const Value: string );
begin
  if FFormat <> Value then
  begin
    FFormat := Value;
    Invalidate;
  end;
end;


procedure TRzTimePicker.SetHour( Value: Integer );
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime( FTime, Hour, Min, Sec, MSec );
  if FTimeIsPM and ( Value < 12 ) then
    Value := Value + 12
  else if not FTimeIsPM and ( Value = 12 ) then
    Value := 0;
  SetTime( EncodeTime( Value, Min, 0, 0 ) );
end;


procedure TRzTimePicker.SetMinutes( Value: Integer );
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime( FTime, Hour, Min, Sec, MSec );
  SetTime( EncodeTime( Hour, Value, 0, 0 ) );
end;


procedure TRzTimePicker.SetTime( Value: TTime );
var
  Hour, Min, Sec, MSec: Word;
begin
  if ( FTime <> Frac( Value ) ) or FForceUpdate then
  begin
    DecodeTime( Value, Hour, Min, Sec, MSec );
    FTime := EncodeTime( Hour, Min, 0, 0 );

    FTimeIsPM := Hour >= 12;

    try
      Changed;
    finally
      Invalidate;
    end;
  end;
end; {= TRzTimePicker.SetTime =}


procedure TRzTimePicker.SetShowSetButton( Value: Boolean );
begin
  if FShowSetButton <> Value then
  begin
    FShowSetButton := Value;
    Invalidate;
  end;
end;


procedure TRzTimePicker.SetShowTime( Value: Boolean );
begin
  if FShowTime <> Value then
  begin
    FShowTime := Value;
    Invalidate;
  end;
end;


procedure TRzTimePicker.SetThemeAware( Value: Boolean );
begin
  if FThemeAware <> Value then
  begin
    FThemeAware := Value;
    Invalidate;
  end;
end;


procedure TRzTimePicker.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  AdjustForFont;
end;


procedure TRzTimePicker.CMDialogChar( var Msg: TCMDialogChar );
begin
  if Enabled then
  begin
    if IsAccel( Msg.CharCode, FCaptionAM ) then
    begin
      ChangeToAM;
      Msg.Result := 1;
    end
    else if IsAccel( Msg.CharCode, FCaptionPM ) then
    begin
      ChangeToPM;
      Msg.Result := 1;
    end
    else if IsAccel( Msg.CharCode, FCaptionSet ) then
    begin
      SetBtnClick;
      Msg.Result := 1;
    end;
  end;
end;


procedure TRzTimePicker.WMGetDlgCode( var Msg: TWMGetDlgCode );
begin
  Msg.Result := dlgc_WantArrows;
end;





{=================================}
{== TRzCalculatorColors Methods ==}
{=================================}

constructor TRzCalculatorColors.Create( ACalculator: TRzCalculator );
begin
  inherited Create;
  FCalculator := ACalculator;

  FNumberFont := clWindowText;
  FNumberBtns := clBtnFace;
  FOperatorFont := clWindowText;
  FOperatorBtns := clBtnFace;
  FCommandFont := clWindowText;
  FCommandBtns := clBtnFace;
  FDisplayFont := clWindowText;
  FDisplay := clWindow;
end;


destructor TRzCalculatorColors.Destroy;
begin
  FCalculator := nil;
  inherited;
end;


procedure TRzCalculatorColors.Assign( Source: TPersistent );
begin
  if Source is TRzCalculatorColors then
  begin
    NumberFont := TRzCalculatorColors( Source ).NumberFont;
    NumberBtns := TRzCalculatorColors( Source ).NumberBtns;
    OperatorFont := TRzCalculatorColors( Source ).OperatorFont;
    OperatorBtns := TRzCalculatorColors( Source ).OperatorBtns;
    CommandFont := TRzCalculatorColors( Source ).CommandFont;
    CommandBtns := TRzCalculatorColors( Source ).CommandBtns;
    DisplayFont := TRzCalculatorColors( Source ).DisplayFont;
    Display := TRzCalculatorColors( Source ).Display;
  end
  else
    inherited;
end;


function TRzCalculatorColors.GetColor( Index: Integer ): TColor;
begin
  case Index of
    0: Result := FNumberFont;
    1: Result := FNumberBtns;
    2: Result := FOperatorFont;
    3: Result := FOperatorBtns;
    4: Result := FCommandFont;
    5: Result := FCommandBtns;
    6: Result := FDisplayFont;
    7: Result := FDisplay;
    else
      Result := clNone;
  end;
end;


procedure TRzCalculatorColors.SetColor( Index: Integer; Value: TColor );
begin
  case Index of
    0: FNumberFont := Value;
    1: FNumberBtns := Value;
    2: FOperatorFont := Value;
    3: FOperatorBtns := Value;
    4: FCommandFont := Value;
    5: FCommandBtns := Value;
    6: FDisplayFont := Value;
    7: FDisplay := Value;
  end;

  if FCalculator <> nil then
    FCalculator.Invalidate;
end;


{===========================}
{== TRzCalculator Methods ==}
{===========================}

constructor TRzCalculator.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle -
                  [ csAcceptsControls, csNoStdEvents, csSetCaption, csDoubleClicks ] +
                  [ csReflector ];

  FBoldButtons := False;
  FCalculatorColors := TRzCalculatorColors.Create( Self );
  FThemeAware := True;

  {&RCI}

  Width := 147;
  Height := 159;

  Color := clWindow;
  FlatColorAdjustment := 0;
  BorderOuter := fsLowered;
  TabStop := True;
  AutoSize := True;
  DoubleBuffered := True;
  AdjustForFont;

  Clear;
end;


procedure TRzCalculator.CreateHandle;
begin
  inherited;
  FPressedArea := ccValue;
end;



destructor TRzCalculator.Destroy;
begin
  FCalculatorColors.Free;
  inherited;
end;


procedure TRzCalculator.DrawDisplayBox( Bounds: TRect; FrameColor: TColor );
var
  R: TRect;
  C: TColor;
  Offset: Integer;
  N: Extended;
  S: string;
begin
  R := DrawBox( Canvas, Bounds, FrameColor );

  if not FErrorOccurred then
  begin
    if FShowResult then
      N := FResult
    else
      N := FNumber;

    // Use FloatToStrF instead of FormatFloat to force trailing zeros after
    // the decimal point to appear in the formatted string.
    if FDecimalPlaces > 1 then
      S := FloatToStrF( N, ffFixed, 15, FDecimalPlaces - 1 )
    else
      S := FloatToStr( N );

    if Pos( FormatSettings.DecimalSeparator, S ) = 0 then
      S := S + FormatSettings.DecimalSeparator;

  end
  else // Error has occurred, show an E in the display
    S := 'E';

  SetTextAlign( Canvas.Handle, ta_Right );

  if UsingSystemStyle then
  begin
    Canvas.Brush.Color := FCalculatorColors.Display;
    Canvas.Font.Color := FCalculatorColors.DisplayFont;
  end
  else
  begin
    if Focused then
      Canvas.Brush.Color := LighterColor( ActiveStyleColor( scEdit ), 40 )
    else
      Canvas.Brush.Color := ActiveStyleColor( scEdit );
    Canvas.Font.Color := ActiveStyleFontColor( sfEditBoxTextNormal );
  end;

  Canvas.Brush.Style := bsClear;
  Offset := ( ( R.Bottom - R.Top ) - FCharSize.Y ) div 2;

  Canvas.TextRect( R, R.Right - 2, R.Top + Offset, S );
  Canvas.Brush.Style := bsSolid;

  if Focused then
  begin
    if UsingSystemStyle then
      C := LighterColor( FrameColor, 40 )
    else // VCL Styles
      C := ActiveStyleColor( scGenericGradientBase );
    DrawBox( Canvas, R, C );
  end;
end;


function TRzCalculator.GetButtonColor( Area: TRzCalculatorArea ): TColor;
begin
  Result := clBtnFace;
  case Area of
    ccBtn0..ccBtn9, ccDecimal:
      Result := FCalculatorColors.NumberBtns;

    ccAdd, ccSubtract, ccMultiply, ccDivide:
      Result := FCalculatorColors.OperatorBtns;

    ccClear, ccEqual, ccSet:
      Result := FCalculatorColors.CommandBtns;
  end;
end;


function TRzCalculator.GetButtonFontColor( Area: TRzCalculatorArea ): TColor;
begin
  if UsingSystemStyle then
  begin
    Result := clWindowText;
    case Area of
      ccBtn0..ccBtn9, ccDecimal:
        Result := FCalculatorColors.NumberFont;

      ccAdd, ccSubtract, ccMultiply, ccDivide:
        Result := FCalculatorColors.OperatorFont;

      ccClear, ccEqual, ccSet:
        Result := FCalculatorColors.CommandFont;
    end;
  end
  else // VCL Styles
  begin
    Result := ActiveStyleFontColor( sfButtonTextNormal );
  end;

end;


procedure TRzCalculator.DrawCalcButton( Bounds: TRect; Area: TRzCalculatorArea );
const
  Captions: array[ TRzCalculatorArea ] of string = ( '0', '1', '2', '3', '4',
                                                     '5', '6', '7', '8', '9',
                                                     '.', '+', '-', '*', #247,
                                                     'C', '=', '', '' );
var
  R: TRect;
  DrawFlat: Boolean;
  ArrowFont: TFont;
begin
  DrawFlat := BorderOuter in [ fsFlat, fsFlatBold, fsFlatRounded ];

  R := Bounds;

//  if FThemeAware and ActiveStyleServicesEnabled then
//    InflateRect( R, 1, 1 );

  DrawButton( Canvas, R, Captions[ Area ],
              GetButtonColor( Area ), GetButtonFontColor( Area ),
              DrawFlat, ( FPressedArea = Area ) and ( FOverArea = Area ),
              FThemeAware );

  if Area = ccSet then
  begin
    SetBkMode( Canvas.Handle, Windows.Transparent );
    ArrowFont := TFont.Create;
    try
      ArrowFont.Name := 'Marlett';
      ArrowFont.Size := Font.Size + 4;
      Canvas.Font.Assign( ArrowFont );
      Canvas.Font.Color := GetButtonFontColor( ccSet );
    finally
      ArrowFont.Free;
    end;
    DrawStringCentered( Canvas, '5', R );   // Draw Up Arrow

    Canvas.Font.Assign( Font );
    Canvas.Font.Color := GetButtonFontColor( ccSet );
    if FBoldButtons then
      Canvas.Font.Style := [ fsBold ];
  end;

  Canvas.Brush.Style := bsSolid;
end;


procedure TRzCalculator.Paint;
var
  R: TRect;
  FrameColor: TColor;
  A: TRzCalculatorArea;
  Areas: TRzCalculatorAreas;
  {$IFDEF VCL160_OR_HIGHER}
  Details: TThemedElementDetails;
  B: TBitmap;
  {$ENDIF}
begin
  if UsingSystemStyle then
  begin
    if FThemeAware and ActiveStyleServicesEnabled then
    begin
      FrameColor := GetXPThemeColor( xptcEditBorder );
      R := Rect( 0, 0, Width, Height );
      R := DrawBox( Canvas, R, FrameColor );
      Canvas.Brush.Color := Color;
      Canvas.FillRect( R );
    end
    else
    begin
      inherited;
      FrameColor := FlatColor;
    end;
  end
  else // VCL Styles
  begin
    {$IFDEF VCL160_OR_HIGHER}
    R := Rect( 0, 0, Width, Height );
    Details := StyleServices.GetElementDetails( teEditTextNormal );

    B := TBitmap.Create;
    try
      B.Width := Width;
      B.Height := Height;
        StyleServices.DrawElement( B.Canvas.Handle, Details, R );
      Canvas.Draw( 0, 0, B );
    finally
      B.Free;
    end;

    FrameColor := ActiveStyleFontColor( sfEditBoxTextDisabled );

    {$ELSE}

    // To eliminate warnings in earlier versions of Delphi -- this code will not be called
    FrameColor := clNone;
    {$ENDIF}
  end;

  CalcAreas( Areas );

  Canvas.Font.Assign( Font );
  if FBoldButtons then
    Canvas.Font.Style := [ fsBold ];

  for A := Low( Areas ) to High( Areas ) do
  begin
    R := Areas[ A ];
    if ( A <> ccSet ) or ( ( A = ccSet ) and FIsPopup ) then
    begin
      if A <> ccValue then
        DrawCalcButton( R, A )
      else
        DrawDisplayBox( R, FrameColor );
    end;
  end;
  Canvas.Brush.Style := bsSolid;
end; {= TRzCalculator.Paint =}



procedure TRzCalculator.CalcAreas( var Areas: TRzCalculatorAreas );
var
  CR: TRect;
  B, G: Integer;
  C1, C2, C3, C4, C5, Offset: Integer;
begin
  CR := ClientRect;
  AdjustClientRect( CR );

  FillChar( Areas, SizeOf( Areas ), 0 );

  G := 4;
  B := ( CR.Right - CR.Left - ( 6 * G ) ) div 5 + 1;

  C1 := G;
  C2 := B + 2 * G;
  C3 := 2 * B + 3 * G;
  C4 := 3 * B + 4 * G;
  C5 := 4 * B + 5 * G;

  case BorderOuter of
    fsGroove, fsBump, fsLowered, fsButtonDown, fsRaised, fsButtonUp, fsFlatBold, fsFlatRounded:
      Offset := 2;

  else
    Offset := 0;
  end;
  Inc( C1, Offset );
  Inc( C2, Offset );
  Inc( C3, Offset );
  Inc( C4, Offset );
  Inc( C5, Offset );


  // Row 1
  if FIsPopup then
  begin
    Areas[ ccValue ] := Rect( C1, C1, C4 + B, C1 + B );
    Areas[ ccSet ] := Rect( C5, C1, C5 + B, C1 + B );
  end
  else
  begin
    Areas[ ccValue ] := Rect( C1, C1, C5 + B, C1 + B );
    Areas[ ccSet ] := Rect( 0, 0, 0, 0 );
  end;

  // Row 2
  Areas[ ccBtn7 ] := Rect( C1, C2, C1 + B, C2 + B );
  Areas[ ccBtn8 ] := Rect( C2, C2, C2 + B, C2 + B );
  Areas[ ccBtn9 ] := Rect( C3, C2, C3 + B, C2 + B );
  Areas[ ccDivide ] := Rect( C4, C2, C4 + B, C2 + B );
  Areas[ ccClear ] := Rect( C5, C2, C5 + B, C3 + B );

  // Row 3
  Areas[ ccBtn4 ] := Rect( C1, C3, C1 + B, C3 + B );
  Areas[ ccBtn5 ] := Rect( C2, C3, C2 + B, C3 + B );
  Areas[ ccBtn6 ] := Rect( C3, C3, C3 + B, C3 + B );
  Areas[ ccMultiply ] := Rect( C4, C3, C4 + B, C3 + B );

  // Row 4
  Areas[ ccBtn1 ] := Rect( C1, C4, C1 + B, C4 + B );
  Areas[ ccBtn2 ] := Rect( C2, C4, C2 + B, C4 + B );
  Areas[ ccBtn3 ] := Rect( C3, C4, C3 + B, C4 + B );
  Areas[ ccSubtract ] := Rect( C4, C4, C4 + B, C4 + B );
  Areas[ ccEqual ] := Rect( C5, C4, C5 + B, C5 + B );

  // Row 5
  Areas[ ccBtn0 ] := Rect( C1, C5, C2 + B, C5 + B );
  Areas[ ccDecimal ] := Rect( C3, C5, C3 + B, C5 + B );
  Areas[ ccAdd ] := Rect( C4, C5, C4 + B, C5 + B );

end; {= TRzCalculator.CalcAreas =}


procedure TRzCalculator.AdjustClientRect( var Rect: TRect );
begin
  inherited;
  FixClientRect( Rect, False );
end;


procedure TRzCalculator.AdjustForFont;
begin
  CalcFontSize;
  AdjustSize;
  Invalidate;
end;


procedure TRzCalculator.CalcFontSize;
var
  Canvas: TCanvas;
  DC: HDC;
begin
  Canvas := TCanvas.Create;
  DC := GetDC( 0 );
  try
    Canvas.Handle := DC;
    Canvas.Font.Assign( Font );
    FCharSize.X := Canvas.TextWidth( '0' );
    FCharSize.Y := Canvas.TextHeight( '0' );
  finally
    Canvas.Handle := 0;
    Canvas.Free;
    ReleaseDC( 0, DC );
  end;
end; {= TRzCalculator.CalcFontSize =}


procedure TRzCalculator.ConstrainedResize( var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer );
begin
  CanAutoSize( MinWidth, MinHeight );
  inherited;
end;


function TRzCalculator.CanAutoSize( var NewWidth, NewHeight: Integer ): Boolean;
begin
  NewHeight := 5 * ( FCharSize.Y + 12 ) + 6 * 4;
  NewWidth := NewHeight;
  Result := True;
end; {= TRzCalculator.CanAutoSize =}



procedure TRzCalculator.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  Areas: TRzCalculatorAreas;
begin
  inherited;

  if not ( csDesigning in ComponentState ) and not FIsPopup and IsWindowVisible( Handle ) then
    SetFocus;

  if Button = mbLeft then
  begin
    if not PtInRect( ClientRect, Point( X, Y ) ) then
      SetOverArea( ccValue );

    if FOverArea in [ ccBtn0..ccSet ] then
    begin
      CalcAreas( Areas );
      FMouseOverRect := Areas[ FOverArea ];
    end
    else
      FMouseOverRect := Rect( 0, 0, 0, 0 );

    SetPressedArea( FOverArea );
  end;
end; {= TRzCalculator.MouseDown =}



function TRzCalculator.HitTest( X, Y: Integer ): TRzCalculatorArea;
var
  A: TRzCalculatorArea;
  P: TPoint;
  Areas: TRzCalculatorAreas;
begin
  Result := ccValue;

  CalcAreas( Areas );
  P := Point( X, Y );
  for A := Low( TRzCalculatorArea ) to High( TRzCalculatorArea ) do
  begin
    if PtInRect( Areas[ A ], P ) then
    begin
      Result := A;
      Break;
    end;
  end;
end;


procedure TRzCalculator.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  if Button = mbLeft then
    SetPressedArea( ccValue );
  inherited;
end;


procedure TRzCalculator.Changed;
begin
  if Assigned( FOnChange ) then
    FOnChange( Self );
end;


procedure TRzCalculator.SetBtnClick;
begin
  if not FIsPopup then
    Exit;

  if Assigned( FOnSetBtnClick ) then
    FOnSetBtnClick( Self );
end;


procedure TRzCalculator.UpdateDisplay( Update: TRzCalculatorUpdateDisplay );
begin
  FShowResult := Update = udResult;
  Invalidate;
end;


procedure TRzCalculator.AppendDigit( N: Integer );

  function PositionalValue( Places: Integer ): Extended;
  var
    I: Integer;
  begin
    Result := 1.0;
    for I := 1 to Places do
      Result := Result / 10.0;
  end;

begin {= TRzCalculator.AppendDigit =}
  if FErrorOccurred then
    Exit;

  if FFirstDigit then
  begin
    FNumber := N;
    UpdateDisplay( udNumber );
    FFirstDigit := False;
  end
  else // Add to the existing number
  begin
    if FDecimalPlaces >= 1 then           // User is entering decimals now
    begin
      FNumber := Int( Abs( FNumber ) ) + Frac( Abs( FNumber ) ) +
                ( PositionalValue( FDecimalPlaces ) * N );
      Inc( FDecimalPlaces );
      UpdateDisplay( udNumber );
    end
    else
    begin
      // So far a whole number is being entered.  Add new digits to the right
      // of the number by shifting number one place (* 10) and adding the new
      // digit in the 'ones' place
      try
        FNumber := ( Abs( FNumber ) * 10 ) + N;
        UpdateDisplay( udNumber );
      except
        on EOverflow do
          ShowError;
      end;
    end;
  end;
end; {= TRzCalculator.AppendDigit =}


procedure TRzCalculator.DecimalPressed;
begin
  if FErrorOccurred then
    Exit;

  if FDecimalPlaces <= 1 then
  begin
    if FFirstDigit then
    begin
      FNumber := 0.0;
      UpdateDisplay( udNumber );
    end;
    FDecimalPlaces := 1;
    FFirstDigit := False;
  end;
end;


procedure TRzCalculator.Click;
begin
  {&RV}
  if ( FPressedArea = FOverArea ) and ( FPressedArea <> ccValue ) then
  begin
    case FPressedArea of
      ccBtn0..ccBtn9:
        AppendDigit( Ord( FPressedArea ) );

      ccDecimal:
        DecimalPressed;

      ccAdd:
        OperationPressed( calcAdd );

      ccSubtract:
        OperationPressed( calcSubtract );

      ccMultiply:
        OperationPressed( calcMultiply );

      ccDivide:
        OperationPressed( calcDivide );

      ccClear:
        Clear;

      ccEqual:
        EqualPressed;

      ccSet:
      begin
        if not FFirstNumber then
          CalculateNewResult; // Perform any outstanding calculation
        SetBtnClick;
      end;
    end;
    Repaint;
    inherited;
  end;
end;


procedure TRzCalculator.KeyDown( var Key: Word; Shift: TShiftState );
var
  C: Char;
begin
  inherited;

  if Key in [ vk_NumPad0..vk_NumPad9 ] then
  begin
    AppendDigit( Key - vk_NumPad0 );
  end
  else if Key = vk_Return then
  begin
    if FIsPopup then
    begin
      if not FFirstNumber then
        CalculateNewResult; // Perform any outstanding calculation
      SetBtnClick;
    end
    else
      EqualPressed;
  end
  else if Key = vk_Decimal then
  begin
    DecimalPressed;
  end
  else
  begin
    C := Char( MapVirtualKey( Key, 2 {MAPVK_VK_TO_CHAR} ) );
    case C of
      '0'..'9':
      begin
        if ( ssShift in Shift ) and ( C = '8' ) then
          OperationPressed( calcMultiply )
        else
          AppendDigit( Ord( Key ) - $30 );
      end;
      '+':
        OperationPressed( calcAdd );
      '-':
        OperationPressed( calcSubtract );
      '*':
        OperationPressed( calcMultiply );
      '/':
        OperationPressed( calcDivide );
      '=':
      begin
        if ssShift in Shift then
          OperationPressed( calcAdd )
        else
          EqualPressed;
      end;
      'C', 'c': Clear;

      else
      begin
        if C = FormatSettings.DecimalSeparator then
          DecimalPressed;
      end;

    end;
  end;
end;


procedure TRzCalculator.DoEnter;
begin
  inherited;
  Invalidate;
end;


procedure TRzCalculator.DoExit;
begin
  inherited;
  Invalidate;
end;


function TRzCalculator.GetIntResult: Integer;
begin
  Result := Round( FResult );
end;


procedure TRzCalculator.SetIntResult( Value: Integer );
begin
  SetResult( Value );
end;


procedure TRzCalculator.SetBoldButtons( Value: Boolean );
begin
  if FBoldButtons <> Value then
  begin
    FBoldButtons := Value;
    Invalidate;
  end;
end;

procedure TRzCalculator.SetCalculatorColors( Value: TRzCalculatorColors );
begin
  FCalculatorColors.Assign( Value );
end;


procedure TRzCalculator.Clear;
begin
  FResult := 0.0;
  FNumber := 0.0;
  FCurrentOperation := calcNone;
  FFirstDigit := True;
  FFirstNumber := True;
  FDecimalPlaces := 0;
  FErrorOccurred := False;

  UpdateDisplay( udResult );
end;


procedure TRzCalculator.ShowError;
begin
  FErrorOccurred := True;
  Invalidate;
end;


procedure TRzCalculator.PrepareForUserInput;
begin
  FFirstDigit := True;
  FDecimalPlaces := 0;
end;


procedure TRzCalculator.PrepareForOperator;
begin
  if FFirstNumber then
  begin
    FFirstNumber := False;
    if FCurrentOperation = calcNone then
    begin
      FResult := FNumber;
    end
  end
  else
    CalculateNewResult;
end;


procedure TRzCalculator.OperationPressed( Operation: TRzCalculatorOperation );
begin
  if FErrorOccurred then
    Exit;

  PrepareForOperator;
  FCurrentOperation := Operation;
  PrepareForUserInput;
end;


procedure TRzCalculator.EqualPressed;
begin
  if FErrorOccurred then
    Exit;

  CalculateNewResult;
  PrepareForUserInput;
  FFirstNumber := True;
  Changed;
end;


procedure TRzCalculator.CalculateNewResult;
var
  TempResult: Extended;
begin
  try
    case FCurrentOperation of
      calcAdd:
      begin
        TempResult := FResult + FNumber;
        FResult := TempResult;
        UpdateDisplay( udResult );
      end;

      calcSubtract:
      begin
        TempResult := FResult - FNumber;
        FResult := TempResult;
        UpdateDisplay( udResult );
      end;

      calcDivide:
      begin
        if IsZero( FNumber ) then
        begin
          ShowError;
        end
        else
        begin
          TempResult := FResult / FNumber;
          FResult := TempResult;
          UpdateDisplay( udResult );
        end;
      end;

      calcMultiply:
      begin
        TempResult := FResult * FNumber;
        FResult := TempResult;
        UpdateDisplay( udResult );
      end;

      calcNone:
      begin
        // User has entered a number followed by pressing the equal button.
        FResult := FNumber;
        PrepareForUserInput;
        FFirstNumber := True;
      end;
    end; { case FCurrentOperation }

  except
    on EIntOverflow do
      ShowError;
    on EOverflow do
      ShowError;
    on EZeroDivide do
      ShowError;
    on EMathError do
      ShowError;
  end;
end; {= TRzCalculator.CalculateNewResult =}


procedure TRzCalculator.SetResult( Value: Extended );
begin
  if FResult <> Value then
  begin
    FResult := Value;
    FNumber := FResult;
    FCurrentOperation := calcNone;
    FFirstDigit := True;
    FFirstNumber := True;
    FDecimalPlaces := 0;
    FErrorOccurred := False;

    UpdateDisplay( udResult );
  end;
end;


procedure TRzCalculator.SetOverArea( Value: TRzCalculatorArea );
var
  NeedToInvalidate: Boolean;
begin
  if FOverArea <> Value then
  begin
    NeedToInvalidate := FOverArea = FPressedArea;
    FOverArea := Value;
    NeedToInvalidate := NeedToInvalidate or ( FOverArea = FPressedArea );
    if NeedToInvalidate and not IsRectEmpty( FMouseOverRect ) then
      InvalidateRect( Handle, @FMouseOverRect, False );
  end;
end;


procedure TRzCalculator.SetPressedArea( Value: TRzCalculatorArea );
begin
  if FPressedArea <> Value then
  begin
    FPressedArea := Value;
    if not IsRectEmpty( FMouseOverRect ) then
      InvalidateRect( Handle, @FMouseOverRect, False );
  end;
end;




procedure TRzCalculator.SetThemeAware( Value: Boolean );
begin
  if FThemeAware <> Value then
  begin
    FThemeAware := Value;
    Invalidate;
  end;
end;


procedure TRzCalculator.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  AdjustForFont;
end;


procedure TRzCalculator.WMEraseBkgnd( var Msg: TMessage );
begin
  Msg.Result := 1;
end;


procedure TRzCalculator.WMNCHitTest( var Msg: TWMNCHitTest );
var
  P: TPoint;
  Area: TRzCalculatorArea;
  Areas: TRzCalculatorAreas;
begin
  inherited;
  CalcAreas( Areas );
  P := ScreenToClient( Point( Msg.XPos, Msg.YPos ) );
  for Area := High( Area ) downto Low( Area ) do
  begin
    if PtInRect( Areas[ Area ], P ) then
    begin
      SetOverArea( Area );
      Exit;
    end;
  end;
  SetOverArea( ccValue );
end;


procedure TRzCalculator.WMGetDlgCode( var Msg: TWMGetDlgCode );
begin
  // Even though there is no keyboard focus to change within the control,
  // arrow keys are trapped here so they do not take the focus away in case
  // the user does not have NumLock turned on and they press an arrow key.
  Msg.Result := dlgc_WantArrows;
end;


{================================}
{== TRzClockFaceColors Methods ==}
{================================}

constructor TRzClockFaceColors.Create( ATimePicker: TRzTimePicker );
begin
  inherited Create;
  FTimePicker := ATimePicker;

  FFace := clBtnFace;
  FHands := clWindowText;
  FNumbers := clWindowText;
  FHourTicks := clBtnShadow;
  FMinuteTicks := clWindowText;
end;


destructor TRzClockFaceColors.Destroy;
begin
  FTimePicker := nil;
  inherited;
end;


procedure TRzClockFaceColors.Assign( Source: TPersistent );
begin
  if Source is TRzClockFaceColors then
  begin
    Face := TRzClockFaceColors( Source ).Face;
    Hands := TRzClockFaceColors( Source ).Hands;
    Numbers := TRzClockFaceColors( Source ).Numbers;
    HourTicks := TRzClockFaceColors( Source ).HourTicks;
    MinuteTicks := TRzClockFaceColors( Source ).MinuteTicks;
  end
  else
    inherited;
end;


function TRzClockFaceColors.GetColor( Index: Integer ): TColor;
begin
  case Index of
    0: Result := FFace;
    1: Result := FHands;
    2: Result := FNumbers;
    3: Result := FHourTicks;
    4: Result := FMinuteTicks;
    else
      Result := clNone;
  end;
end;


procedure TRzClockFaceColors.SetColor( Index: Integer; Value: TColor );
begin
  case Index of
    0: FFace := Value;
    1: FHands := Value;
    2: FNumbers := Value;
    3: FHourTicks := Value;
    4: FMinuteTicks := Value;
  end;

  if FTimePicker <> nil then
    FTimePicker.Invalidate;
end;



{============================}
{== TRzCustomPopup Methods ==}
{============================}

constructor TRzCustomPopup.Create( AOwner: TComponent );
begin
  inherited;

  BorderWidth := 2;
  BorderOuter := fsPopup;
  FlatColorAdjustment := 0;
  Visible := False;
end;


procedure TRzCustomPopup.AdjustClientRect( var Rect: TRect );
begin
  inherited;
  FixClientRect( Rect, False );
end;


procedure TRzCustomPopup.AlignControls( AControl: TControl; var Rect: TRect );
begin
  AdjustClientRect( Rect );
  inherited;
end;


procedure TRzCustomPopup.Paint;
var
  ElementDetails: TThemedElementDetails;
begin
  if ActiveStyleServicesEnabled then
  begin
    ElementDetails := ActiveStyleServices.GetElementDetails( teEditTextDisabled );
    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, ClientRect );
  end
  else
    inherited;
end;


{======================}
{== TRzPopup Methods ==}
{======================}

const
  cm_KillPopup = wm_User + $2023;

type
  TRzPopup = class( TRzCustomPopup )
  private
    FPopupPanel: TRzPopupPanel;
    FTarget: TWinControl;
    FCancel: Boolean;

    function ContainsWindow( Wnd: HWnd ): Boolean;
    procedure ReparentControls( OldParent, NewParent: TWinControl );
  protected
    procedure DoClose( Sender: TObject );
    procedure Cancel;

    procedure CreateParams( var Params: TCreateParams ); override;
    procedure WndProc( var Msg: TMessage ); override;
  public
    constructor Create( AOwner: TComponent ); override;
                         
    function Popup( PopupControl: TControl; Alignment: TAlignment;
                    PopupX: Integer = -1; PopupY: Integer = -1 ): Boolean; 
  end;


constructor TRzPopup.Create( AOwner: TComponent );
begin
  inherited;
  Visible := False;

  FPopupPanel := TRzPopupPanel( AOwner );

  Parent := GetParentForm( FPopupPanel );
  Color := FPopupPanel.Color;
  Font := FPopupPanel.Font;
  Width := FPopupPanel.BorderWidth;
  Ctl3D := False;
  Hint := FPopupPanel.Hint;
  ShowHint := FPopupPanel.ShowHint;
end;


procedure TRzPopup.CreateParams( var Params: TCreateParams );
begin
  inherited;
  Params.Style := ws_Popup;
end;


type
  TApplicationAccess = class( TApplication );


function TRzPopup.Popup( PopupControl: TControl; Alignment: TAlignment;
                         PopupX: Integer = -1; PopupY: Integer = -1 ): Boolean;
var
  PopupControlBounds: TRect;
  X, Y, W, H: Integer;
  Msg: TMsg;
  M: TMonitor;

  procedure SetTarget;
  var
    I, P: Integer;
    C: TControl;
    F: TCustomForm;
  begin
    ReparentControls( FPopupPanel, Self );
    FTarget := nil;

    for I := 0 to ControlCount - 1 do
    begin
      C := Controls[ I ];
      if ( C is TWinControl ) and C.Enabled and C.Visible then
      begin
        FTarget := TWinControl( C );
        Break;
      end;
    end;

    // If the popup control in on a container that has its origin shifted above the top of the
    // form (and AutoScroll is True), the vertical scroll bar gets shifted.  The following code
    // records the previous position of the vertical scroll bar and resets it after the SetFocus call.

    F := GetParentForm( FPopupPanel );
    P := 0;
    if ( F <> nil ) and ( F is TForm ) and TForm( F ).VertScrollBar.Visible then
      P := TForm( F ).VertScrollBar.Position;

    Windows.SetFocus( FPopupPanel.Handle );

    if P <> 0 then
      TForm( F ).VertScrollBar.Position := P;
  end;


  procedure Hide;
  begin
    SetWindowPos( Handle, 0, 0, 0, 0, 0, swp_HideWindow or swp_NoZOrder or swp_NoMove or swp_NoSize or swp_NoActivate );
    ReparentControls( Self, FPopupPanel );
    Parent := nil;
  end;


  procedure MessageLoop;
  begin
    try
      repeat
        if PeekMessage( Msg, 0, 0, 0, pm_NoRemove ) then
        begin
          case Msg.message of
            wm_NCLButtonDown, wm_NCRButtonDown, wm_NCMButtonDown,
            wm_LButtonDown, wm_LButtonDblClk,
            wm_RButtonDown, wm_RButtonDblClk,
            wm_MButtonDown, wm_MButtonDblClk:
            begin
              if not ContainsWindow( Msg.hwnd ) then
              begin
                PeekMessage( Msg, 0, 0, 0, pm_Remove );
                Break;
              end;
            end;

            wm_KeyFirst..wm_KeyLast:
            begin
              PeekMessage( Msg, 0, 0, 0, pm_Remove );
              SendMessage( Handle, Msg.Message, Msg.WParam, Msg.LParam );
              Continue;
            end;

            wm_KillFocus, cm_KillPopup:
              Exit;

            cm_Deactivate, wm_ActivateApp:
              Break;
          end;
          Application.HandleMessage;
        end
        else
          TApplicationAccess( Application ).Idle( Msg );
      until Application.Terminated;
    finally
      Hide;
    end;
    Cancel;
  end; {= MessageLoop =}


begin {= TRzPopup.Popup =}

  FPopupControl := PopupControl;

  if FPopupControl is TWinControl then
  begin
    GetWindowRect( TWinControl( FPopupControl ).Handle, PopupControlBounds );
  end
  else with FPopupControl.Parent do
  begin
    PopupControlBounds := FPopupControl.BoundsRect;
    PopupControlBounds.TopLeft := ClientToScreen( PopupControlBounds.TopLeft );
    PopupControlBounds.BottomRight := ClientToScreen( PopupControlBounds.BottomRight );
  end;

  SetTarget;

  W := FPopupPanel.BoundsRect.Right - FPopupPanel.BoundsRect.Left;
  H := FPopupPanel.BoundsRect.Bottom - FPopupPanel.BoundsRect.Top;

  if ( PopupX = -1 ) and ( PopupY = -1 ) then
  begin
    Y := PopupControlBounds.Bottom;

    case Alignment of
     taLeftJustify:
       X := PopupControlBounds.Left;

     taRightJustify:
       X := PopupControlBounds.Right - W;

     else
       X := ( PopupControlBounds.Left + PopupControlBounds.Right - W ) div 2;
    end;
  end
  else
  begin
    // Popup in a specific location (X,Y)
    X := PopupControlBounds.Left + PopupX;
    Y := PopupControlBounds.Top + PopupY;
  end;

  M := Screen.MonitorFromPoint( Point( X, Y ) );

  if M <> nil then
  begin
    if ( X + W ) > M.WorkareaRect.Right then
      X := M.WorkareaRect.Right - W;
    if ( Y + H ) > M.WorkareaRect.Bottom then
      Y := PopupControlBounds.Top - H;

    if X < M.WorkareaRect.Left then
      X := M.WorkareaRect.Left;
    if Y < M.WorkareaRect.Top then
      Y := M.WorkareaRect.Top;
  end
  else // Monitor is nil, use Screen object instead
  begin
    if ( X + W ) > Screen.WorkareaRect.Right then
      X := Screen.WorkareaRect.Right - W;
    if ( Y + H ) > Screen.WorkareaRect.Bottom then
      Y := PopupControlBounds.Top - H;

    if X < Screen.WorkareaRect.Left then
      X := Screen.WorkareaRect.Left;
    if Y < Screen.WorkareaRect.Top then
      Y := Screen.WorkareaRect.Top;
  end;

  SetBounds( X, Y, W, H );
  SetWindowPos( Handle, hwnd_Top, X, Y, W, H, swp_NoActivate or swp_ShowWindow );

  Visible := True;
  FCancel := True;
  MessageLoop;
  Result := not FCancel;
end; {= TRzPopup.Popup =}


procedure TRzPopup.DoClose( Sender: TObject );
begin
  FCancel := False;
  Cancel;
end;


procedure TRzPopup.Cancel;
begin
  PostMessage( Handle, cm_KillPopup, 0, 0 );
end;


procedure TRzPopup.ReparentControls( OldParent, NewParent: TWinControl );
var
  I: Integer;
begin
  for I := OldParent.ControlCount - 1 downto 0 do
    OldParent.Controls[ I ].Parent := NewParent;
end;



function TRzPopup.ContainsWindow( Wnd: HWnd ): Boolean;
begin
  while ( Wnd <> 0 ) and ( Wnd <> Handle ) do
    Wnd := GetParent( Wnd );

  Result := Wnd = Handle;
end;


procedure TRzPopup.WndProc( var Msg: TMessage );
begin
  case Msg.Msg of
    wm_NCActivate:
      Msg.Result := 0;

    wm_MouseActivate:
      Msg.Result := ma_NoActivate;

    wm_SysCommand:
      Cancel;

    wm_KeyFirst..wm_KeyLast:
    begin
      if Msg.WParam = vk_Escape then
        Cancel;

      if FTarget <> nil then
        TRzPopup( FTarget ).WndProc( Msg )
      else
        inherited;
    end;

    cm_KillPopup:
      Free;

    else
      inherited;
  end;
end; {= TRzPopup.WndProc =}


{===========================}
{== TRzPopupPanel Methods ==}
{===========================}

constructor TRzPopupPanel.Create( AOwner: TComponent );
begin
  inherited;
  Alignment := taRightJustify;
  Width := 100;
  Height := 100;
  ControlStyle := ControlStyle + [ csAcceptsControls ];
  Color := clBtnFace;
  ParentColor := False;
  AutoSize := True;
end;


procedure TRzPopupPanel.DestroyWnd;
begin
  Close( Self );
  inherited;
end;


procedure TRzPopupPanel.Close;
begin
  if FPopup <> nil then
    TRzPopup( FPopup ).DoClose( Self );
end;


function TRzPopupPanel.Popup( PopupControl: TControl ): Boolean;
begin
  Result := Popup( PopupControl, -1, -1 );
end;


function TRzPopupPanel.Popup( PopupControl: TControl; X, Y: Integer ): Boolean;
var
  F: TCustomForm;
  C: TWinControl;
  A: TAlignment;
begin
  Result := False;
  if not Active then
  begin
    FPopupControl := PopupControl;
    F := GetParentForm( Self );
    C := nil;
    if F <> nil then
      C := F.ActiveControl;

    if F <> nil then
      F.DisableAutoRange;

    try
      DoPopup;
      Handle;
      RecreateWnd;
      Handle;

      A := Alignment;
      if UseRightToLeftAlignment then
        ChangeBiDiModeAlignment( A );

      FPopup := TRzPopup.Create( Self );
      TRzPopup( FPopup ).BorderOuter := BorderOuter;
      TRzPopup( FPopup ).FlatColor := FlatColor;
      Result := TRzPopup( FPopup ).Popup( FPopupControl, A, X, Y );

      DoClose;
    finally
      if F <> nil then
        F.EnableAutoRange;
      if Application.Active then
      begin
        if C <> nil then
          C.SetFocus;
      end;
      FPopupControl := nil;
      FPopup := nil;
    end;
  end;
end;


procedure TRzPopupPanel.DoClose;
begin
  if Assigned( FOnClose ) then
    FOnClose( Self );
end;


procedure TRzPopupPanel.DoPopup;
begin
  if Assigned( FOnPopup ) then
    FOnPopup( Self );
end;


function TRzPopupPanel.GetActive: Boolean;
begin
  Result := FPopup <> nil;
end;


{&RUIF}
end.

