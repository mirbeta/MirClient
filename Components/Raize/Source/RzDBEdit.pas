{===============================================================================
  RzDBEdit Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBEdit
    Data-Aware TRzEdit

  TRzDBNumericEdit
    Data-Aware TRzNumericEdit

  TRzDBExpandEdit
    Data-Aware TRzExpandEdit

  TRzDBDateTimeEdit
    Data-Aware TRzDateTimeEdit

  TRzDBMemo
    Data-Aware TRzMemo

  TRzDBRichEdit
    Data-Aware TRzRichEdit


  Modification History
  ------------------------------------------------------------------------------
  6.1.11 (08 Apr 2015)
    * Fixed issue in TRzDBEdit and descendants where registered style hook was 
      not correctly unregistered when the hook was no longer needed.
    * Added UseFormatToParse property to TRzDateTimeEdit. Set this property to
      True in situations where the Time to be parsed contains more elements
      than are contained in the LongTimeFormat string. For example, if the Time
      needed to be entered must have 'hh:nn:ss.zzz' and the LongTimeFormat 
      string is set to 'hh:nn', then set the UseFormatToParse to True and set
      the Format property to the desired format string.
  ------------------------------------------------------------------------------
  6.1.9  (21 Jun 2014)
    * Fixed issue in TRzDateTimeEdit where MinDate and MaxDate values were not
      honored when special keys were used to change the date.
  ------------------------------------------------------------------------------
  6.1.3  (01 May 2013)
    * Fixed issue where TRzDBDateTimeEdit Date property was always streamed out
      to form file as a result of changes to the VCL streaming criteria.
    * Fixed encoding issue in TRzDBRichEdit that could occur when PlainText is
      set to True and the control's window handle needed to be recreated.
  ------------------------------------------------------------------------------
  6.1.2  (22 Feb 2013)
    * Fixed issue where a TRzDBEDit control or descendant that is connected to
      a right-justified data field, such as a numeric value, the control's
      OnExit event handler would get called because the underlying control's
      window handle was recreated to set the alignment.
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * PopupWidth and PopupHeight properties for TRzDBDateTimeEdit are no longer
      persisted into the DFM file when their values are zero.
    * Added AsDateTime runtime property to TRzDBDateTimeEdit to aid in type
      conversions between TDate and TTime values and TDateTime.
    * Added PopupAlignment property to TRzDBDateTimeEdit and TRzNumericEdit,
      which specifies which side of the control the popup is aligned.
    * Added PopupWidth and PopupHeight properties to TRzDBNumericEdit, which
      control the size of the calculator if displayed.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Added new TwoDigitYearConverter property to TDBRzDateTimeEdit, which
      controls how 2-digit years are converted to 4-digit years. The default
      value is ycStandard, which means that 2-digit years are converted using
      the TwoDigitYearCenturyWindow value.  The ycPastNotFuture value means that
      a date with a 2-digit year is interpreted as a date in the past rather
      than the future.
      For example, consider entering '3/15/20' (in the U.S.). With the
      ycStandard converter value, the date gets converted to '3/15/2020'. But
      with the ycPastNotFuture converter, the converted date is '3/15/1920'.
      Entry of 4-digit years are that unaffected by this property.
    * Added PopupHeight and PopupWidth properties to TRzDBDateTimeEdit. When both
      of these properties are set to non-zero values, the popup control (i.e.
      Calendar or TimePicker) is sized to the PopupHeight and PopupWidth values
      instead of the default auto size values.
    * Made necessary modifications to TRzDBEdit, TRzDBNumericEdit,
      TRzDBExpandEdit, TRzDBDateTimeEdit, TRzDBMemo, and TRzDBRichEdit to
      fully support VCL Styles introduced in RAD Studio XE2.
    * Made necessary modifications to TRzDBEdit and descendants to support
      64-bit development.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Fixed issue in TRzDBNumericEdit where pressing the keypad decimal key
      would not insert the DecimalSeparator character (based on user locale
      settings) if the DecimalSeparator was something other than a period and
      the calculator was dropped-down.
    * Fixed issue in TRzDBNumericEdit where setting the Value property to a
      decimal value at design-time would truncate the decimal portion when the
      form was loaded if DisplayFormat used a decimal based format such as
      ',0.0;(,0.0)'.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Changed the TRzDBNumericEdit.IntValue property to be of type Int64.
    * Fixed issue where TRzDBDateTimeEdit and TRzDBNumericEdit would trap the
      Alt+F4 key combination and prevent the application from closing.
    * Fixed issue where TRzDBEdit would allow text modification even if the
      associated TDataSource had AutoEdit set to False.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * Fixed issue in TRzDBNumericEdit where pressing the Del key on the numeric
      keypad would cause a beep to sound if the user's locale was set to use a
      different character than the period "." for the decimal point. This error
      was a side effect of the new functionality added in 5.1.2 that handles
      translating the Del key on the keypad into the locale specific decimal
      separator.
    * Fixed issue where assigning a negative value to a TRzDBNumericEdit with a
      non default DisplayFormat *and* AllowScientificNotation set to False would
      result in the Value of the control being set to zero.
    * Redefined how the TRzNumericEdit determines if a number is negative.
    * Fixed issue where OnDateTimeChange event would get raised twice when the
      user would press a key to change the date or time.
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzDBEdit, TRzDBNumericEdit, TRzDBExpandEdit, and TRzDBDateTimeEdit
      controls.
  ------------------------------------------------------------------------------
  5.1.2  (11 Jun 2009)
    * Added new AllowScientificNotation property to TRzDBNumericEdit. When set
      to True (the default), the control allows entering "E" and "e" to specify
      a number in scientific notation. Setting this property to False, prevents
      the control from accepting "E" and "e" during data-entry and parsing.
    * The TRzDBNumericEdit has also been enhanced to handle presses of the Del
      key on the numeric keypad by inserting the DecimalSeparator into the edit
      area. This change allows the numeric keypad to be used to enter floating
      point numbers even if the user locale's numeric settings uses a symbol
      other than a period for the decimal separator.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Updated the parsing code used by TRzDBDateTimeEdit so that an invalid
      month value results in the OnInvalidDate event being fired instead of
      simply resetting the month value to 1.
    * Updated parsing code used by TRzDBDateTimeEdit so that invalid minute
      values extract the first two digits to represent the minutes and not just
      the first digit, which results in a more accurate resolution of time
      values.
    * Added new OnInvalidTime event to the TRzDBDateTimeEdit, which fires when
      the entered time string contains invalid values for one of the time
      portions. The NewTime parameter to the event handler contains the auto-
      corrected Time value that will be used if the value is not modified in the
      event handler.
    * Fixed problem of the TextHint property not getting displayed when running
      under Vista.
    * Surfaced FlatButtons and FlatButtonColor properties in TRzDBNumericEdit.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added new TextHint and TextHintVisibleOnFocus properties to TRzEdit and
      descendants (e.g. TRzDBNumericEdit, TRzDBDateTimeEdit). The TextHint
      property allows a developer to specify a prompt that appears inside the
      edit area of the control, when the control is empty. The text prompt
      appears grayed. By default, the prompt is automatically hidden when the
      control receives focus. To keep the prompt visible until the user enters a
      value, set the TextHintVisibleOnFocus property to True.
      NOTES:
        - Please see the comments in RzEdit.pas for more details on TextHint
          and TextHintVisibleOnFocus.
    * The TRzDBNumericEdit control now has a CalculatorVisible property. When
      this property is set to True, a drop down button becomes visible, and when
      clicked, a popup Calculator (an instance of the new TRzCalculator) is
      displayed.  The calculator allows the user to perform simple numeric
      calculations and then move the result into the Value property of the
      TRzNumericEdit.  The calculator supports both Integer and floating point
      operations.
      NOTE:
        Please note that if the TRzNumericEdit specifies a DisplayFormat that
        does not specify a decimal placeholder and the calculator produces a
        floating-point result, the result will be rounded according to
        DisplayFormat property.
    * The drop down calendar and time picker displayed by the TRzDBDateTimeEdit
      control now uses the same font size as the edit control itself. Therefore,
      if a larger font is used for the edit field text, the calendar and time
      picker will also be displayed using the larger font.
    * Fixed problem in TRzDBDateTimeEdit and TRzDBNumericEdit where changing the
      DropButtonVisible property to False would not be honored and the button
      would reappear if the field was edited or if the ReadOnly property of the
      connected field would change.
    * Changed the way popups for TRzDBDateTimeEdit are parented when displayed.
    * Fixed issue in TRzNumericEdit where using a DisplayFormat that used
      scientific notation with a lower case 'e' would not get parsed correctly.
    * Fixed problem where TRzDBRichEdit would not honor DisabledColor value when
      disabled.
    * The TRzDBExpandEdit now expands to the left on right-to-left systems.
  ------------------------------------------------------------------------------
  4.3.2  (04 Nov 2007)
    * A blank TRzDBDateTimeEdit no longer automatically sets its Date property
      to the current date when the calendar is dropped down.
  ------------------------------------------------------------------------------
  4.3.1  (21 Sep 2007)
    * Fixed issue where displaying a TRzDBEdit that was not connected to a
      DataField would result in an Access Violation.
  ------------------------------------------------------------------------------
  4.3    (13 Sep 2007)
    * TRzDBDateTimeEdit now handle the F4 key to drop down their respective
      panels.
    * Updated TRzDBEdit.Alignment property to allow the taCenter setting. This
      allows TField.Alignment settings of taCenter to be honored when connected
      to a TRzDBEdit.
  ------------------------------------------------------------------------------
  4.2.1  (07 Jun 2007)
    * Fixed problem in TRzDBDateTimeEdit where certain time values were being
      mis-interpreted as date values and were thus raising exceptions.
  ------------------------------------------------------------------------------
  4.2    (29 May 2007)
    * Added an OnInvalidDate event to TRzDateTimeEdit. This event is fired when
      the user enters an invalid date value (e.g. 4/55/2007) and the control
      loses keyboard focus. In previous versions, the control would display the
      "0" date, which is "12/30/1899". Starting with this version, the default
      behavior in this situation is to blank out the date, which is essentially
      the same as before except the 12/30/1899 date is not displayed, which is
      much more noticeable to users.
      However, the new OnInvalidDate event gives developers much more control
      over what happens in this situation.  Specifically, OnInvalidDate event
      handlers are passed 3 variable parameters:  KeepFocused, KeepInvalidText,
      and NewDate. Set KeepFocused to True in the event handler to keep the
      focus on the control. What appears in the control depends on the other two
      parameters. Set KeepInvalidText to True to have the control continue to
      display the invalid text the user entered. When KeepInvalidText is True,
      it is implied that focus is to be kept on the control. If KeepInvalidText
      is False, then the NewDate parameter controls what appears to replace the
      invalid date. By default NewDate is 0, which means the date gets blanked
      out. Setting NewDate to some other value causes the invalid date to be
      changed to the date specified, regardless of whether focus stays on the
      control or not. The OnInvalidDate event handler can also be used to
      display a message box (or some other indicator) to the user that an
      invalid date has been entered.
    * Fixed problem where plus and minus keys would change the date/time of a
      TRzDateTimeEdit even when the control's ReadOnly property was set to True.
    * Fixed problem where TRzDBNumericEdit would accept alphabetic characters
      when IntegersOnly was set to False.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Added new MinDate and MaxDate properties to TRzDBDateTimeEdit to allow for
      specifying a range of valid dates that can be selected by the user.
    * Added the new OnGetDayFormat event to the TRzDBDateTimeEdit. This event
      allows the individual days of the calendar display to be colored and
      styled differently than the calendar's default formatting.  This new event
      should be used in place of the OnGetBoldDays event.  The OnGetBoldDays
      event is still available, but the new OnGetDayFormat event is much more
      flexible and powerful.
    * Adjusted the drawing of Days of the Week header in the drop down calendar
      of the TRzDBDateTimeEdit. In the new approach, the headers are either all
      drawn showing their short name (based on locale), or they are all drawn
      showing the first two characters of their short name. Also, the font size
      is reduced slightly for the day of week short names.
    * Fixed problem where bold-days bitmasks in TRzDBDateTimeEdit were not
      correctly applied to the previous and next months in a the current view.
    * Surfaced the ReadOnlyColorOnFocus property in TRzDBEdit, TRzDBNumericEdit,
      TRzDBDateTimeEdit controls. The functionality was added in 4.0.2, but was
      not surfaced.
    * Added BeepOnInvalidKey property to TRzDBEdit, TRzDBNumericEdit, and
      TRzDBDateTimeEdit.
    * Updated TRzDBNumericEdit to allow free form floating point numbers
      including scientific notation. Free form entry is only allowed when the
      IntegersOnly property is set to False.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Surfaced OnMouseWheel event in TRzDBMemo and TRzDBRichEdit.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * Added ReadOnlyColorOnFocus property to TRzDBEdit and descendants, and
      TRzDBMemo, and TRzDBRichEdit. This property determines if the
      ReadOnlyColor or FocusColor is used to color the control when the control
      has the focus and has its ReadOnly property set to True. By default, a
      focused control will use the FocusColor even if the control is read-only.
      Setting ReadOnlyColorOnFocus to True will cause a focused read-only
      control to be colored using ReadOnlyColor.
  ------------------------------------------------------------------------------
  4.0.1  (07 Jan 2006)
    * Changed the preference order between FocusColor and ReadOnlyColor
      properties in TRzDBEdit, TRzDBMemo, and TRzDBRichEdit. In the previous
      ordering, a ReadOnly control would be displayed in ReadOnlyColor even when
      the control had the focus. Now, a ReadOnly control that has the input
      focus is displayed in FocusColor.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed problem where clicking the Clear button in the Calendar when used by
      a TRzDBDateTimeEdit would not set the underlying database field to NULL.
    * Fixed issue where editing a NULL date field with a TRzDBDateTimeEdit and
      then cancelling the edit of the dataset resulted in 12/31/1899 appearing
      in the edit area.
    * Added GetRtfData method to TRzDBRichEdit, which returns a string
      containing the RTF encoded data for the rich edit control.
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzDBMemo and
      TRzDBRichEdit to account for changes introduced in Borland Developer
      Studio 2006.
    * Fixed custom framing display problem that would occur in TRzDBEdit and
      descendants when FrameVisible was set to True and changes were made to
      control's appearance within calls to LockWindowUpdate.
    * Added ReadOnlyColor property to TRzDBEdit and descendant classes. The new
      property has also been added to TRzDBMemo and TRzDBRichEdit.  This color
      property is used to change the color of the control when the ReadOnly
      property is set to True.
    * When the ReadOnly property for a TRzDBDateTimeEdit is set to True, the
      drop down button is hidden.
    * Added new FrameControllerNotifications property to TRzDBEdit and
      descendant classes (e.g. TRzDBDateTimeEdit).
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Fixed problem where TRzDBMemo and TRzDBRichEdit would not honor the user's
      mouse wheel lines settings when scrolling using the mouse wheel.
    * Added new PopupButtonColor and PopupButtonFontColor properties to
      TRzDBDateTimeEdit control, which provide access to the popup TRzCalendar
      or TRzTimerPicker ButtonColor and ButtonFontColor properties.
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Fixed problem in TRzDBDateTimeEdit where setting the Date property to a
      TDateTime value of 0 would clear the edit area instead of setting the date
      to 12/30/1899.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem in TRzNumericEdit where initial value would not be formatted
      correctly if DisplayFormat was not the default *and* AllowBlank was set
      to True.
    * Addd OnViewDateChange event to TRzDateTimeEdit, which fires when the
      month (i.e. view) of the drop-down calendar is changed by the user.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Fixed problem in TRzDBDateTimeEdit where using Minus key or Down Arrow key
      to change time value would result in an incorrect time value if the change
      would cause the time to be earlier than midnight (i.e. 12:00 am).
    * Enhanced TRzDateTimeEdit to allow entry of unformatted 8-digit date
      strings and correctly convert it to an actual date.
    * Fixed problem where changing ParentColor to True in a control using Custom
      Framing did not reset internal color fields used to manage the color of
      the control at various states.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Added OnGetWeekNumber event to TRzDBDateTimeEdit. Handle this event to
      implement a customized week numbering scheme.
    * Added OnRangeError event to TRzNumericEdit. This event is generated when
      the user leaves the field and the value entered by the user exceeds the
      bounds defined by the Min and Max properties.
    * Fixed problem where Line property in TRzDBMemo and TRzDBRichEdit would not
      return the correct value when text was selected.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Fixed problem where user could not enter a positive number into a
      TRzDBNumericEdit if the first character in the selection was a '-' sign.
    * Fixed problem where taking a screen capture of a TRzDBEdit when
      PasswordChar was <> #0 caused the real text to appear in the capture.
    * Fixed display problems when edit controls were placed on a TDBCtrlGrid.
    * Added OnDateTimeChange to TRzDBDateTimeEdit. This event fires whenever the
      date/time value in the edit field changes.
    * Fixed problem where selected text at beginning of a TRzDBNumericEdit that
      contained a '+' or '-' sign could not be replaced by pressing the '+' or
      '-' keys.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * Fixed problem where using the TimePicker to set the time in a
      TRzDBDateTimeEdit's Time to 12:00 AM did not enter 12:00 AM into the edit
      field.
    * Fixed problem where a digit could be entered to the left of a sign symbol
      in TRzDBNumericEdit.
    * Fixed problem where drop-down TimePicker was closing in response to the
      wrong event.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * Fixed problem with enter 12:00 am into TRzDBDateTimeEdit and leaving the
      field changed the time to 12:00 PM.
    * Clear method now correctly clears the TRzDBDateTimeEdit and it internal
      datetime value and sets the corresponding database field to NULL.
    * Modified TRzDBNumericEdit.EvaluteText so that string conversion is avoided
      if text just contains a leading minus sign or open paren indicating a
      negative number. This change prevents the beep from sounded if the text
      value cannot be converted.
    * Fixed problem in TRzDBEdit.WMPaint where Msg.DC was being passed to
      SendMessage as a WParam and was not casted to a WParam (i.e. Longint).
    * Fixed problem where leaving a TRzDBDateTimeEdit (with EditType=etTime)
      after deleting selected time value caused 12:00 AM (or appropriately
      formatted time) to appear in control.
    * Fixed problem where clearing the date/time value from a TRzDBDateTimeEdit
      did not write NULL to the database field.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Fixed problem where empty TRzDBNumericEdit would not write NULL to
      database field even if AllowBlank was set to True.
    * Fixed problem where format of TRzDBDateTimeEdit would change when control
      received the focus.
    * Fixed problem where OnExit event for TRzDBEdit was getting fired twice.
    * Fixed problem where focus would leave TRzDBDateTimeEdit if accelerator
      pressed and the key was a valid date or time value key (e.g. 1, 2, 3...).
    * Surfaced DropButtonVisible in TRzDBDateTimeEdit.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Added StoreColor and StoreFocusColor methods so that if control is
      disabled at design-time the Color and FocusColor properties are not
      streamed with the disabled color value.
    * Added additional hot keys to TRzDBDateTimeEdit for changing Day, Month,
      Year, and Hour, Minute.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * TRzDBEdit now descends from TRzCustomEdit instead of from TDBEdit. This
      change was needed so that we can have more control over the painting of
      the data-aware version. The TDBEdit component handles the wm_Paint message
      and does its own painting of the control.
    * Renamed FrameFlat property to FrameHotTrack.
    * Renamed FrameFocusStyle property to FrameHotStyle.
    * Removed FrameFlatStyle property.
    * Published inherited FocusColor and DisabledColor properties.
    * Published inherited FramingPreference property.

    << TRzDBNumericEdit >>
    * Added AllowBlank and BlankValue to TRzDBNumericEdit.
===============================================================================}

{$I RzComps.inc}

// Range checking must be turned off so that TRzDBEdit can operate correctly
// in DBCtrlGrid
{$R-}     

unit RzDBEdit;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  RzCommon,
  StdCtrls,
  Mask,
  RzEdit,
  DBCtrls,
  RzPopups,
  DB,
  DBCGrids,
  ExtCtrls;

type
  TRzDBEdit = class;

  {-----------------------------------------------------------------------------
    TRzPaintEdit is a simple panel descendant that knows how to look like a
    TRzDBEdit component. This is necessary for the TRzDBEdit component to appear
    correctly when used in a TDBCtrlGrid. The problem is that the TDBCtrlGrid
    uses a technique that relies on the control being replicated painting itself
    using a shared device context. Since the standard edit control does not do
    this, the TRzPaintEdit component is used for replicated instances of a
    TRzDBEdit.
  -----------------------------------------------------------------------------}

  TRzPaintEdit = class( TCustomPanel )
  private
    FEditControl: TRzDBEdit;
  protected
    procedure Paint; override;
  public
    constructor Create( AOwner: TComponent ); override;
  end;


  {=================================}
  {== TRzDBEdit Class Declaration ==}
  {=================================}

  TRzDBEdit = class( TRzCustomEdit )
  private
    FFocused: Boolean;
    FPaintControl: TRzPaintEdit;
    FDataLink: TFieldDataLink;
    FAlignment: TAlignment;

    { Internal Event Handlers }
    procedure ActiveChangeHandler( Sender: TObject );
    procedure DataChangeHandler( Sender: TObject );
    procedure EditingChangeHandler( Sender: TObject );
    procedure UpdateDataHandler( Sender: TObject );

    { Message Handling Methods }
    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure WMCut( var Msg: TMessage ); message wm_Cut;
    procedure WMPaste( var Msg: TMessage ); message wm_Paste;
    procedure WMClear( var Msg: TMessage ); message wm_Clear;
    procedure WMUndo( var Msg: TMessage ); message wm_undo;
    procedure CMGetDataLink( var Msg: TMessage ); message cm_GetDataLink;
  protected
    FOverControl: Boolean;

    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure RepaintFrame; override;

    function GetRightJustifiedText: string; override;
    procedure AdjustEditRect; virtual;
    function GetEditRect: TRect; override;

    procedure ResetMaxLength;
    function EditCanModify: Boolean; override;
    procedure Reset; override;

    procedure ActiveChanged; virtual;
    procedure DataChanged; virtual;
    procedure EditingChanged; virtual;
    procedure UpdateData; virtual;

    function GetDisplayString: string; virtual;

    { Event Dispatch Methods }
    procedure Change; override;

    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyPress( var Key: Char ); override;

    { Property Access Methods }
    function GetDataField: string; virtual;
    procedure SetDataField( const Value: string ); virtual;
    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource( Value: TDataSource ); virtual;
    function GetField: TField; virtual;
    procedure SetFocused( Value: Boolean ); virtual;
    function ReadOnlyValue: Boolean; override;
    function GetReadOnly: Boolean;
    procedure SetReadOnly( Value: Boolean );

    procedure SetFrameVisible( Value: Boolean ); override;

    // Give Descendants Access to the DataLink
    property DataLink: TFieldDataLink
      read FDataLink;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function ExecuteAction( Action: TBasicAction ): Boolean; override;
    function UpdateAction( Action: TBasicAction ): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;

    property Field: TField
      read GetField;

    property Text;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property DataSource: TDataSource
      read GetDataSource
      write SetDataSource;

    property DataField: string
      read GetDataField
      write SetDataField;

    property ReadOnly: Boolean
      read GetReadOnly
      write SetReadOnly
      default False;

    { Inherited Properties & Events }
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BeepOnInvalidKey;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FocusColor;
    property FrameColor;
    property FrameControllerNotifications;
    property FrameController;
    property FrameHotColor;
    property FrameHotTrack;
    property FrameHotStyle;
    property FrameSides;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property ShowHint;
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    property TextHint;
    property TextHintVisibleOnFocus;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;



  {========================================}
  {== TRzDBNumericEdit Class Declaration ==}
  {========================================}

  TRzDBNumericEdit = class( TRzDBEdit )
  {$IFDEF VCL160_OR_HIGHER}
  strict private
    class constructor Create;
    class destructor Destroy;
  {$ENDIF}
  private
    FAllowBlank: Boolean;
    FAllowScientificNotation: Boolean;
    FBlankValue: Extended;
    FCheckRange: Boolean;
    FIntegersOnly: Boolean;
    FMin: Extended;
    FMax: Extended;
    FDisplayFormat: string;
    FFieldValue: Extended;
    FModified: Boolean;
    FCalculatorBoldButtons: Boolean;
    FCalculatorColors: TRzCalculatorColors;
    FDecimalPressed: Boolean;

    FPopupAlignment: TAlignment;
    FPopupHeight: Integer;
    FPopupWidth: Integer;

    FOnRangeError: TRzRangeErrorEvent;

    function GetCalculatorVisible: Boolean;
    procedure SetCalculatorVisible( Value: Boolean );

    // Message Handling Methods
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
  protected
    procedure CreateWnd; override;
    procedure CreateParams( var Params: TCreateParams ); override;

    function IsValidChar( Key: Char ): Boolean; virtual;
    function FormatText( const Value: Extended ): string; virtual;
    function EvaluateText: Extended; virtual;

    procedure DisplayCalculator; virtual;
    function GetDisplayString: string; override;

    procedure DataChanged; override;
    procedure UpdateData; override;

    // Event Dispatch Methods
    procedure CloseUp; override;
    procedure DropDown; override;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyPress( var Key: Char ); override;
    procedure RangeError( EnteredValue, AdjustedValue: Extended; var AutoCorrect: Boolean ); dynamic;

    // Property Access Methods
    procedure SetIntegersOnly( Value: Boolean ); virtual;

    procedure SetMin( const Value: Extended ); virtual;
    procedure SetMax( const Value: Extended ); virtual;

    function GetIntValue: Int64; virtual;
    procedure SetIntValue( Value: Int64 ); virtual;
    function GetValue: Extended; virtual;
    function CheckValue( const Value: Extended; var KeepFocusOnEdit: Boolean ): Extended; virtual;
    procedure SetValue( const Value: Extended ); virtual;
    procedure SetDisplayFormat( FormatString: string ); virtual;
    procedure SetCalculatorColors( Value: TRzCalculatorColors ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    property IntValue: Int64
      read GetIntValue
      write SetIntValue;

    property Modified: Boolean
      read FModified;

   published
    property AllowBlank: Boolean
      read FAllowBlank
      write FAllowBlank
      default True;

    property AllowScientificNotation: Boolean
      read FAllowScientificNotation
      write FAllowScientificNotation
      default True;

    property BlankValue: Extended
      read FBlankValue
      write FBlankValue;

    property CalculatorVisible: Boolean
      read GetCalculatorVisible
      write SetCalculatorVisible
      default False;

    property CalculatorBoldButtons: Boolean
      read FCalculatorBoldButtons
      write FCalculatorBoldButtons
      default False;

    property CalculatorColors: TRzCalculatorColors
      read FCalculatorColors
      write SetCalculatorColors;

    property CheckRange: Boolean
      read FCheckRange
      write FCheckRange
      default False;

    property IntegersOnly: Boolean
      read FIntegersOnly
      write SetIntegersOnly
      default True;

    property Max: Extended
      read FMax
      write SetMax;

    property Min: Extended
      read FMin
      write SetMin;

    property DisplayFormat: string
      read FDisplayFormat
      write SetDisplayFormat;

    property PopupAlignment: TAlignment
      read FPopupAlignment
      write FPopupAlignment
      default taRightJustify;

    property PopupHeight: Integer
      read FPopupHeight
      write FPopupHeight
      default 0;

    property PopupWidth: Integer
      read FPopupWidth
      write FPopupWidth
      default 0;

    property Value: Extended
      read GetValue
      write SetValue;

    property OnRangeError: TRzRangeErrorEvent
      read FOnRangeError
      write FOnRangeError;

    { Inherited Properties & Events }
    property Alignment default taRightJustify;
    property FlatButtonColor;
    property FlatButtons;
  end;


  {=======================================}
  {== TRzDBExpandEdit Class Declaration ==}
  {=======================================}

  TRzDBExpandEdit = class( TRzDBEdit )
  private
    FExpandedWidth: Integer;
    FExpanded: Boolean;
    FOrigWidth: Integer;
    FExpandOn: TExpandOnType;

    { Message Handling Methods }
    procedure WMSetFocus( var Msg: TWMSetFocus  ); message wm_SetFocus;
    procedure WMKillFocus( var Msg: TWMKillFocus ); message wm_KillFocus;
    procedure WMRButtonUp( var Msg: TWMRButtonUp ); message wm_RButtonUp;
  protected
    { Property Access Methods }
    procedure SetExpandedWidth( Value: Integer ); virtual;
    procedure SetExpandOn( Value: TExpandOnType ); virtual;
  public
   constructor Create( AOwner: TComponent ); override;
  published
   property ExpandedWidth: Integer
     read FExpandedWidth
     write SetExpandedWidth;

   property ExpandOn: TExpandOnType
     read FExpandOn
     write SetExpandOn
     default etNone;
  end;


  {=========================================}
  {== TRzDBDateTimeEdit Class Declaration ==}
  {=========================================}

  TRzDBDateTimeEdit = class( TRzDBEdit )
  {$IFDEF VCL160_OR_HIGHER}
  strict private
    class constructor Create;
    class destructor Destroy;
  {$ENDIF}
  private
    FEditType: TRzDTEditType;
    FLastDateTime: TDateTime;
    FDateTime: TDateTime;
    FMinDate: TDateTime;
    FMaxDate: TDateTime;
    FFormat: string;
    FUseFormatToParse: Boolean;
    FUpdating: Boolean;
    FTimeHasBeenSet: Boolean;
    FSettingTime: Boolean;
    FTimePicked: Boolean;
    FDateHasBeenSet: Boolean;
    FSettingDate: Boolean;
    FDatePicked: Boolean;
    FValidating: Boolean;
    FTwoDigitYearConverter: TRzTwoDigitYearConverter;

    FCalendarElements: TRzCalendarElements;
    FCalendarColors: TRzCalendarColors;
    FCaptionClearBtn: string;
    FCaptionTodayBtn: string;
    FFirstDayOfWeek: TRzFirstDayOfWeek;

    FClockFaceColors: TRzClockFaceColors;
    FCaptionAM: string;
    FCaptionPM: string;
    FCaptionSet: string;
    FRestrictMinutes: Boolean;
    FShowHowToUseHint: Boolean;
    FHowToUseMsg: string;

    FPopupAlignment: TAlignment;
    FPopupButtonColor: TColor;
    FPopupButtonFontColor: TColor;
    FPopupHeight: Integer;
    FPopupWidth: Integer;

    FOnGetBoldDays: TRzGetBoldDaysEvent;
    FOnGetDayFormat: TRzGetDayFormatEvent;
    FOnDateTimeChange: TRzDateTimeChangeEvent;
    FOnGetWeekNumber: TRzGetWeekNumberEvent;
    FOnViewDateChange: TRzViewDateChangeEvent;
    FOnInvalidDate: TRzInvalidDateEvent;
    FOnInvalidTime: TRzInvalidTimeEvent;

    function StoreMinDate: Boolean;
    function StoreMaxDate: Boolean;
    procedure CheckDateTimeChange;

    { Message Handling Methods }
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure WMGetDlgCode( var Msg: TWMGetDlgCode ); message wm_GetDlgCode;
  protected
    function CanEditData: Boolean;
    procedure SetDateTime;
    procedure UpdateText;
    function DateInRange( Value: TDateTime ): Integer;
    procedure SetRange( MinValue, MaxValue: TDate );

    procedure DisplayCalendar; virtual;
    procedure DisplayTimePicker; virtual;

    procedure DataChanged; override;
    procedure UpdateData; override;

    { Event Dispatch Methods }
    procedure Change; override;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyPress( var Key: Char ); override;
    procedure CloseUp; override;
    procedure DropDown; override;
    procedure DateTimeChange; dynamic;

    procedure InvalidDate( var KeepFocused, KeepInvalidText: Boolean;
                           var NewDate: TDateTime ); dynamic;
    procedure InvalidTime( var KeepFocused, KeepInvalidText: Boolean;
                           var NewTime: TDateTime ); dynamic;

    { Property Access Methods }
    function GetDate: TDate; virtual;
    procedure SetDate( Value: TDate ); virtual;
    function GetAsDateTime: TDateTime; virtual;
    procedure SetAsDateTime( Value: TDateTime ); virtual;
    function StoreDate: Boolean; virtual;
    procedure SetMinDate( Value: TDateTime ); virtual;
    procedure SetMaxDate( Value: TDateTime ); virtual;
    procedure SetEditType( Value: TRzDTEditType ); virtual;
    procedure SetFormat( const Value: string ); virtual;
    function GetTime: TTime; virtual;
    procedure SetTime( Value: TTime ); virtual;
    function StoreTime: Boolean; virtual;
    procedure SetClockFaceColors( Value: TRzClockFaceColors ); virtual;
    procedure SetCalendarColors( Value: TRzCalendarColors ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure Clear; override;

    function DaysToBitmask( Days: array of Byte ): Cardinal;

    procedure AdjustYear( DeltaYears: Integer );
    procedure AdjustMonth( DeltaMonths: Integer );
    procedure AdjustDay( DeltaDays: Integer );
    procedure AdjustHour( DeltaHours: Int64 );
    procedure AdjustMinute( DeltaMinutes: Int64 );

    function ReformatDateTime: Boolean;

    property AsDateTime: TDateTime
      read GetAsDateTime
      write SetAsDateTime
      stored False;
  published
    property CalendarColors: TRzCalendarColors
      read FCalendarColors
      write SetCalendarColors;

    property CalendarElements: TRzCalendarElements
      read FCalendarElements
      write FCalendarElements
      default [ ceYear, ceMonth, ceArrows, ceFillDays, ceDaysOfWeek, ceTodayButton, ceClearButton ];

    property CaptionTodayBtn: string
      read FCaptionTodayBtn
      write FCaptionTodayBtn;

    property CaptionClearBtn: string
      read FCaptionClearBtn
      write FCaptionClearBtn;

    property CaptionAM: string
      read FCaptionAM
      write FCaptionAM;

    property CaptionPM: string
      read FCaptionPM
      write FCaptionPM;

    property CaptionSet: string
      read FCaptionSet
      write FCaptionSet;

    property TwoDigitYearConverter: TRzTwoDigitYearConverter
      read FTwoDigitYearConverter
      write FTwoDigitYearConverter
      default ycStandard;

    property ClockFaceColors: TRzClockFaceColors
      read FClockFaceColors
      write SetClockFaceColors;

    property Date: TDate
      read GetDate
      write SetDate
      stored StoreDate;

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
      write FFirstDayOfWeek
      default fdowLocale;

    property HowToUseMsg: string
      read FHowToUseMsg
      write FHowToUseMsg;

    property RestrictMinutes: Boolean
      read FRestrictMinutes
      write FRestrictMinutes
      default False;

    property ShowHowToUseHint: Boolean
      read FShowHowToUseHint
      write FShowHowToUseHint
      default True;

    property Time: TTime
      read GetTime
      write SetTime
      stored StoreTime;

    property EditType: TRzDTEditType
      read FEditType
      write SetEditType
      nodefault;

    property Format: string
      read FFormat
      write SetFormat;

    property UseFormatToParse: Boolean
      read FUseFormatToParse
      write FUseFormatToParse
      default False;

    property PopupButtonColor: TColor
      read FPopupButtonColor
      write FPopupButtonColor
      default clBtnFace;

    property PopupButtonFontColor: TColor
      read FPopupButtonFontColor
      write FPopupButtonFontColor
      default clWindowText;

    property PopupHeight: Integer
      read FPopupHeight
      write FPopupHeight
      default 0;

    property PopupWidth: Integer
      read FPopupWidth
      write FPopupWidth
      default 0;

    property OnGetBoldDays: TRzGetBoldDaysEvent
      read FOnGetBoldDays
      write FOnGetBoldDays;

    property OnGetDayFormat: TRzGetDayFormatEvent
      read FOnGetDayFormat
      write FOnGetDayFormat;

    property OnDateTimeChange: TRzDateTimeChangeEvent
      read FOnDateTimeChange
      write FOnDateTimeChange;

    property OnGetWeekNumber: TRzGetWeekNumberEvent
      read FOnGetWeekNumber
      write FOnGetWeekNumber;

    property OnViewDateChange: TRzViewDateChangeEvent
      read FOnViewDateChange
      write FOnViewDateChange;

    property OnInvalidDate: TRzInvalidDateEvent
      read FOnInvalidDate
      write FOnInvalidDate;

    property OnInvalidTime: TRzInvalidTimeEvent
      read FOnInvalidTime
      write FOnInvalidTime;

    // Inherited Properties & Events
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BeepOnInvalidKey;
    property BorderStyle;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property DisabledColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropButtonVisible default True;
    property Enabled;
    property FlatButtonColor;
    property FlatButtons;
    property Font;
    property FocusColor;
    property FrameColor;
    property FrameControllerNotifications;
    property FrameController;
    property FrameHotColor;
    property FrameHotTrack;
    property FrameHotStyle;
    property FrameSides;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property ShowHint;
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;



  TRzDBMemo = class;

  {---------------------------------------------------------------------------------------------------------------------
    TRzPaintMemo is a simple panel descendant that knows how to look like a TRzDBMemo component. This is necessary for
    the TRzDBMemo component to appear correctly when used in a TDBCtrlGrid. The problem is that the TDBCtrlGrid uses a
    technique that relies on the control being replicated painting itself using a shared device context. Since the
    standard Memo control does not do this, the TRzPaintMemo component is used for replicated instances of a TRzDBMemo.
  ---------------------------------------------------------------------------------------------------------------------}
  TRzPaintMemo = class( TRzPaintEdit )
  private
    FEditControl: TRzDBMemo;
  protected
    procedure Paint; override;
  public
    constructor Create( AOwner: TComponent ); override;
  end;


  {=================================}
  {== TRzDBMemo Class Declaration ==}
  {=================================}

  TRzDBMemo = class( TDBMemo )
  private
    FAboutInfo: TRzAboutInfo;
    FUpdatingColor: Boolean;
    FDisabledColor: TColor;
    FReadOnlyColor: TColor;
    FReadOnlyColorOnFocus: Boolean;
    FFocusColor: TColor;
    FNormalColor: TColor;
    FFrameColor: TColor;
    FFrameController: TRzFrameController;
    FFrameControllerNotifications: TRzFrameControllerNotifications;
    FFrameHotColor: TColor;
    FFrameHotTrack: Boolean;
    FFrameHotStyle: TFrameStyle;
    FFrameSides: TSides;
    FFrameStyle: TFrameStyle;
    FFrameVisible: Boolean;
    FFramingPreference: TFramingPreference;
    FTabOnEnter: Boolean;
    FPaintControl: TRzPaintMemo;

    FOnLineColChange: TLineColChangeEvent;
    FOnClipboardChange: TClipboardChangeEvent;

    procedure ReadOldFrameFlatProp( Reader: TReader );
    procedure ReadOldFrameFocusStyleProp( Reader: TReader );

    { Message Handling Methods }
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure WMNCPaint( var Msg: TWMNCPaint ); message wm_NCPaint;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
  protected
    FCanvas: TCanvas;
    FOverControl: Boolean;

    procedure CreateWnd; override;
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;
    procedure RepaintFrame; virtual;

    { Event Dispatch Methods }
    procedure Change; override;
    procedure Click; override;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyUp( var Key: Word; Shift: TShiftState ); override;
    procedure KeyPress( var Key: Char ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure LineColChange; dynamic;
    procedure ClipboardChange; dynamic;

    function DoMouseWheel( Shift: TShiftState; WheelDelta: Integer;
                           MousePos: TPoint ): Boolean; override;

    { Property Access Methods }
    function GetColumn: Integer; virtual;
    procedure SetColumn( Value: Integer ); virtual;
    function GetLine: Integer; virtual;
    procedure SetLine( Value: Integer ); virtual;
    function StoreColor: Boolean;
    function StoreFocusColor: Boolean;
    function StoreDisabledColor: Boolean;
    function StoreReadOnlyColor: Boolean;
    function StoreReadOnlyColorOnFocus: Boolean;
    function StoreParentColor: Boolean;
    function StoreFlatButtonColor: Boolean;
    function StoreFlatButtons: Boolean;
    function StoreFrameColor: Boolean;
    function StoreFrameHotColor: Boolean;
    function StoreFrameHotTrack: Boolean;
    function StoreFrameHotStyle: Boolean;
    function StoreFrameSides: Boolean;
    function StoreFrameStyle: Boolean;
    function StoreFrameVisible: Boolean;
    function StoreFramingPreference: Boolean;
    procedure SetDisabledColor( Value: TColor ); virtual;
    procedure SetFocusColor( Value: TColor ); virtual;
    procedure SetFrameColor( Value: TColor ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetFrameHotColor( Value: TColor ); virtual;
    procedure SetFrameHotTrack( Value: Boolean ); virtual;
    procedure SetFrameHotStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameSides( Value: TSides ); virtual;
    procedure SetFrameStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameVisible( Value: Boolean ); virtual;
    procedure SetFramingPreference( Value: TFramingPreference ); virtual;
    function GetReadOnly: Boolean;
    procedure SetReadOnly( Value: Boolean );
    procedure SetReadOnlyColor( Value: TColor ); virtual;

    { Property Declarations }
    property Canvas: TCanvas
      read FCanvas;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UseThemes: Boolean; virtual;
    procedure JumpTo( ALine, ACol: Integer );

    { Property Declarations }
    property Column: Integer
      read GetColumn
      write SetColumn;

    property Line: Integer
      read GetLine
      write SetLine;
  published
    { Property Declarations }
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Color
      stored StoreColor
      default clWindow;

    property DisabledColor: TColor
      read FDisabledColor
      write SetDisabledColor
      stored StoreDisabledColor
      default clBtnFace;

    property FocusColor: TColor
      read FFocusColor
      write SetFocusColor
      stored StoreFocusColor
      default clWindow;

    property FrameColor: TColor
      read FFrameColor
      write SetFrameColor
      stored StoreFrameColor
      default clBtnShadow;

    property FrameControllerNotifications: TRzFrameControllerNotifications
      read FFrameControllerNotifications
      write FFrameControllerNotifications
      default fccAll;

    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property FrameHotColor: TColor
      read FFrameHotColor
      write SetFrameHotColor
      stored StoreFrameHotColor
      default clBtnShadow;

    property FrameHotStyle: TFrameStyle
      read FFrameHotStyle
      write SetFrameHotStyle
      stored StoreFrameHotStyle
      default fsFlatBold;

    property FrameHotTrack: Boolean
      read FFrameHotTrack
      write SetFrameHotTrack
      stored StoreFrameHotTrack
      default False;

    property FrameSides: TSides
      read FFrameSides
      write SetFrameSides
      stored StoreFrameSides
      default sdAllSides;

    property FrameStyle: TFrameStyle
      read FFrameStyle
      write SetFrameStyle
      stored StoreFrameStyle
      default fsFlat;

    property FrameVisible: Boolean
      read FFrameVisible
      write SetFrameVisible
      stored StoreFrameVisible
      default False;

    property FramingPreference: TFramingPreference
      read FFramingPreference
      write SetFramingPreference
      stored StoreFramingPreference
      default fpXPThemes;

    property ReadOnly: Boolean
      read GetReadOnly
      write SetReadOnly
      default False;

    property ReadOnlyColor: TColor
      read FReadOnlyColor
      write SetReadOnlyColor
      stored StoreReadOnlyColor
      default clInfoBk;

    property ReadOnlyColorOnFocus: Boolean
      read FReadOnlyColorOnFocus
      write FReadOnlyColorOnFocus
      stored StoreReadOnlyColorOnFocus
      default False;

    property TabOnEnter: Boolean
      read FTabOnEnter
      write FTabOnEnter
      default False;

    property OnLineColChange: TLineColChangeEvent
      read FOnLineColChange
      write FOnLineColChange;

    property OnClipboardChange: TClipboardChangeEvent
      read FOnClipboardChange
      write FOnClipboardChange;

    { Inherited Properties & Events }
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end;


  {=====================================}
  {== TRzDBRichEdit Class Declaration ==}
  {=====================================}

  TRzDBRichEdit = class( TDBRichEdit )
  {$IFDEF VCL160_OR_HIGHER}
  strict private
    class constructor Create;
    class destructor Destroy;
  {$ENDIF}
  private
    FAboutInfo: TRzAboutInfo;
    FUpdatingColor: Boolean;
    FDisabledColor: TColor;
    FReadOnlyColor: TColor;
    FReadOnlyColorOnFocus: Boolean;
    FFocusColor: TColor;
    FNormalColor: TColor;
    FFrameColor: TColor;
    FFrameController: TRzFrameController;
    FFrameControllerNotifications: TRzFrameControllerNotifications;
    FFrameHotColor: TColor;
    FFrameHotTrack: Boolean;
    FFrameHotStyle: TFrameStyle;
    FFrameSides: TSides;
    FFrameStyle: TFrameStyle;
    FFrameVisible: Boolean;
    FFramingPreference: TFramingPreference;
    FTabOnEnter: Boolean;

    FOnLineColChange: TLineColChangeEvent;
    FOnClipboardChange: TClipboardChangeEvent;

    procedure ReadOldFrameFlatProp( Reader: TReader );
    procedure ReadOldFrameFocusStyleProp( Reader: TReader );

    { Message Handling Methods }
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure WMNCPaint( var Msg: TWMNCPaint ); message wm_NCPaint;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
  protected
    FCanvas: TCanvas;
    FOverControl: Boolean;

    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;
    procedure RepaintFrame; virtual;

    { Event Dispatch Methods }
    procedure Change; override;
    procedure Click; override;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyUp( var Key: Word; Shift: TShiftState ); override;
    procedure KeyPress( var Key: Char ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure SelectionChange; override;
    procedure LineColChange; dynamic;
    procedure ClipboardChange; dynamic;

    function DoMouseWheel( Shift: TShiftState; WheelDelta: Integer;
                           MousePos: TPoint ): Boolean; override;

    { Property Access Methods }
    function GetColumn: Integer; virtual;
    procedure SetColumn( Value: Integer ); virtual;
    function GetLine: Integer; virtual;
    procedure SetLine( Value: Integer ); virtual;
    function StoreColor: Boolean;
    function StoreFocusColor: Boolean;
    function StoreDisabledColor: Boolean;
    function StoreReadOnlyColor: Boolean;
    function StoreReadOnlyColorOnFocus: Boolean;
    function StoreParentColor: Boolean;
    function StoreFlatButtonColor: Boolean;
    function StoreFlatButtons: Boolean;
    function StoreFrameColor: Boolean;
    function StoreFrameHotColor: Boolean;
    function StoreFrameHotTrack: Boolean;
    function StoreFrameHotStyle: Boolean;
    function StoreFrameSides: Boolean;
    function StoreFrameStyle: Boolean;
    function StoreFrameVisible: Boolean;
    function StoreFramingPreference: Boolean;
    procedure SetDisabledColor( Value: TColor ); virtual;
    procedure SetFocusColor( Value: TColor ); virtual;
    procedure SetFrameColor( Value: TColor ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetFrameHotColor( Value: TColor ); virtual;
    procedure SetFrameHotTrack( Value: Boolean ); virtual;
    procedure SetFrameHotStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameSides( Value: TSides ); virtual;
    procedure SetFrameStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameVisible( Value: Boolean ); virtual;
    procedure SetFramingPreference( Value: TFramingPreference ); virtual;
    function GetReadOnly: Boolean;
    procedure SetReadOnly( Value: Boolean );
    procedure SetReadOnlyColor( Value: TColor ); virtual;

    { Property Declarations }
    property Canvas: TCanvas
      read FCanvas;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UseThemes: Boolean; virtual;
    procedure JumpTo( ALine, ACol: Integer );

    function GetRtfData: string;

    { Property Declarations }
    property Column: Integer
      read GetColumn
      write SetColumn;

    property Line: Integer
      read GetLine
      write SetLine;
  published
    { Property Declarations }
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Color
      stored StoreColor
      default clWindow;

    property DisabledColor: TColor
      read FDisabledColor
      write SetDisabledColor
      stored StoreDisabledColor
      default clBtnFace;

    property FocusColor: TColor
      read FFocusColor
      write SetFocusColor
      stored StoreFocusColor
      default clWindow;

    property FrameColor: TColor
      read FFrameColor
      write SetFrameColor
      stored StoreFrameColor
      default clBtnShadow;

    property FrameControllerNotifications: TRzFrameControllerNotifications
      read FFrameControllerNotifications
      write FFrameControllerNotifications
      default fccAll;

    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property FrameHotColor: TColor
      read FFrameHotColor
      write SetFrameHotColor
      stored StoreFrameHotColor
      default clBtnShadow;

    property FrameHotStyle: TFrameStyle
      read FFrameHotStyle
      write SetFrameHotStyle
      stored StoreFrameHotStyle
      default fsFlatBold;

    property FrameHotTrack: Boolean
      read FFrameHotTrack
      write SetFrameHotTrack
      stored StoreFrameHotTrack
      default False;

    property FrameSides: TSides
      read FFrameSides
      write SetFrameSides
      stored StoreFrameSides
      default sdAllSides;

    property FrameStyle: TFrameStyle
      read FFrameStyle
      write SetFrameStyle
      stored StoreFrameStyle
      default fsFlat;

    property FrameVisible: Boolean
      read FFrameVisible
      write SetFrameVisible
      stored StoreFrameVisible
      default False;

    property FramingPreference: TFramingPreference
      read FFramingPreference
      write SetFramingPreference
      stored StoreFramingPreference
      default fpXPThemes;

    property ReadOnly: Boolean
      read GetReadOnly
      write SetReadOnly
      default False;

    property ReadOnlyColor: TColor
      read FReadOnlyColor
      write SetReadOnlyColor
      stored StoreReadOnlyColor
      default clInfoBk;

    property ReadOnlyColorOnFocus: Boolean
      read FReadOnlyColorOnFocus
      write FReadOnlyColorOnFocus
      stored StoreReadOnlyColorOnFocus
      default False;

    property TabOnEnter: Boolean
      read FTabOnEnter
      write FTabOnEnter
      default False;

    property OnLineColChange: TLineColChangeEvent
      read FOnLineColChange
      write FOnLineColChange;

    property OnClipboardChange: TClipboardChangeEvent
      read FOnClipboardChange
      write FOnClipboardChange;

    { Inherited Properties & Events }
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end;


implementation

uses
  {&RAS}
  DateUtils,
  Themes,
  UxTheme,
  Clipbrd,
  TypInfo,
  ComCtrls,
  RzGrafx,
  RzPanel;

{&RT}
{==========================}
{== TRzPaintEdit Methods ==}
{==========================}

constructor TRzPaintEdit.Create( AOwner: TComponent );
begin
  inherited;
  FEditControl := TRzDBEdit( AOwner );
  Alignment := taLeftJustify;
  BevelOuter := bvNone;
end;


procedure TRzPaintEdit.Paint;
var
  X, Y: Integer;
  R: TRect;
  FrameColor: TColor;
  FrameSides: TSides;
  FrameStyle: TFrameStyle;
  FrameVisible: Boolean;
begin
  inherited;
  Canvas.Font := FEditControl.Font;
  Canvas.Brush.Color := FEditControl.Color;
  Canvas.FillRect( ClientRect );

  FrameColor := FEditControl.FrameColor;
  FrameSides := FEditControl.FrameSides;
  FrameStyle := FEditControl.FrameStyle;
  FrameVisible := FEditControl.FrameVisible;


  { Draw Border }
  if FrameVisible then
  begin
    R := ClientRect;
    if FEditControl.Color = clWindow then
    begin
      if FrameStyle = fsFlat then
        DrawSides( Canvas, R, FrameColor, FrameColor, FrameSides )
      else
        DrawBorderSides( Canvas, R, FrameStyle, FrameSides );
    end
    else
    begin
      if FrameStyle = fsFlat then
        DrawSides( Canvas, R, FrameColor, FrameColor, FrameSides )
      else
        DrawColorBorderSides( Canvas, R, FEditControl.Color, FrameStyle, FrameSides );
    end;
  end;

  // We just check if FrameVisible is True (above) and do not worry about whether Themes are
  // being used because when replicating the edit control, there is no way to overwrite the
  // box that gets drawn.


  { Draw Text }
  R := FEditControl.GetEditRect;
  if FrameVisible then
    InflateRect( R, -2, -2 );

  SetTextAlign( Canvas.Handle, SetTextAlignments[ Alignment ] );
  case Alignment of
    taLeftJustify:
    begin
      if FrameVisible then
        X := R.Left
      else
        X := R.Left + 1;
    end;

    taRightJustify:
    begin
      if FrameVisible then
        X := R.Right - 1
      else
        X := R.Right - 2;
    end;

    taCenter:
      X := R.Left + ( R.Right - R.Left ) div 2;
  else
    X := 0;
  end;
  Y := R.Top + ( R.Bottom - R.Top - Canvas.TextHeight( 'Pp' ) ) div 2 - 1;

  if UseThemes then
    Canvas.Brush.Style := bsClear;
  Canvas.TextRect( R, X, Y, Caption );
  if UseThemes then
  Canvas.Brush.Style := bsSolid;
end; {= TRzPaintEdit.Paint; =}



{=======================}
{== TRzDBEdit Methods ==}
{=======================}

constructor TRzDBEdit.Create( AOwner: TComponent );
begin
  inherited;
  inherited ReadOnly := True;

  //  ControlStyle := ControlStyle + [csReplicatable];
  ControlStyle := ControlStyle + [ csReplicatable, csSetCaption ];

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChangeHandler;
  FDataLink.OnEditingChange := EditingChangeHandler;
  FDataLink.OnUpdateData := UpdateDataHandler;
  FDataLink.OnActiveChange := ActiveChangeHandler;

  {&RCI}
  FPaintControl := TRzPaintEdit.Create( Self );
  FPaintControl.Parent := Self;
  FPaintControl.Visible := False;

end;


destructor TRzDBEdit.Destroy;
begin
  FPaintControl.Free;
  FPaintControl := nil;

  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;


procedure TRzDBEdit.Loaded;
begin
  inherited;
  ResetMaxLength;
  if csDesigning in ComponentState then
    DataChanged;
end;


procedure TRzDBEdit.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( FDataLink <> nil ) and ( AComponent = DataSource ) then
    SetDataSource( nil );
end;


function TRzDBEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment( Self, Field );
end;


procedure TRzDBEdit.Change;
begin
  FDataLink.Modified;
  inherited;
end;


procedure TRzDBEdit.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  if ( Key = vk_Delete ) or ( ( Key = vk_Insert ) and ( ssShift in Shift ) ) then
    FDataLink.Edit;
end;


procedure TRzDBEdit.KeyPress( var Key: Char );
begin
  inherited;

  if ( Key >= #32 ) and ( FDataLink.Field <> nil ) and
     not FDataLink.Field.IsValidChar( Key ) then
  begin
    InvalidKeyPressed;
    Key := #0;
  end;

  case Key of
    ^H, ^V, ^X, #32..High(Char):
    begin
      if not FDataLink.Edit then
        Key := #0;
    end;

    #27: // Escape Key
    begin
      FDataLink.Reset;
      SelectAll;
      Key := #0;
    end;
  end;
end;


function TRzDBEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;


procedure TRzDBEdit.Reset;
begin
  inherited;
  FDataLink.Reset;
  SelectAll;
end;


function TRzDBEdit.GetRightJustifiedText: string;
begin
  Result := Text;
end;


function TRzDBEdit.GetEditRect: TRect;
begin
  Result := ClientRect;
end;


procedure TRzDBEdit.AdjustEditRect;
begin
end;


procedure TRzDBEdit.RepaintFrame;
begin
  if Parent is TDBCtrlPanel then
    Invalidate
  else
    InvalidateWindowFrame( Handle, ClientRect );
end;


function TRzDBEdit.GetDisplayString: string;
begin
  if Field <> nil then
  begin
    Result := Field.DisplayText;
    if PasswordChar <> #0 then
      FillChar( Result[ 1 ], Length( Result ), PasswordChar );
  end
  else
    Result := '';
end;


procedure TRzDBEdit.WMPaint( var Msg: TWMPaint );
var
  S: string;
begin
  if Field <> nil then
    Alignment := Field.Alignment
  else
    Alignment := taLeftJustify;

  if csPaintCopy in ControlState then
  begin
    S := GetDisplayString;

    FPaintControl.SetBounds( BoundsRect.Left, BoundsRect.Top,
                             BoundsRect.Right - BoundsRect.Left,
                             BoundsRect.Bottom - BoundsRect.Top );

    if Field <> nil then
      FPaintControl.Alignment := Field.Alignment;

    SendTextMessage( FPaintControl.Handle, wm_SetText, 0, S );
    SendMessage( FPaintControl.Handle, wm_Paint, WParam( Msg.DC ), 0 );
  end
  else
  begin
    FPaintControl.SetBounds( 0, 0, 0, 0 );
    AdjustEditRect;
    inherited;

    if FrameVisible and not UseThemes and ( Parent is TDBCtrlPanel ) then
       DrawFrame( FCanvas, Width, Height, FrameStyle, Color, FrameColor, FrameSides );
  end;
end; {= TRzDBEdit.WMPaint =}


function TRzDBEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;


function TRzDBEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;


procedure TRzDBEdit.SetDataField( const Value: string );
begin
  if not ( csDesigning in ComponentState ) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
end;


function TRzDBEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;


procedure TRzDBEdit.SetDataSource( Value: TDataSource );
begin
  if not ( FDataLink.DataSourceFixed and ( csLoading in ComponentState ) ) then
    FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification( Self );
end;


procedure TRzDBEdit.ResetMaxLength;
var
  F: TField;
begin
  if ( MaxLength > 0 ) and Assigned( DataSource ) and Assigned( DataSource.DataSet ) then
  begin
    F := DataSource.DataSet.FindField( DataField );
    if Assigned( F ) and ( F.DataType in [ ftString, ftWideString ] ) and ( F.Size = MaxLength ) then
      MaxLength := 0;
  end;
end;


function TRzDBEdit.ReadOnlyValue: Boolean;
begin
  Result := GetReadOnly;
end;


function TRzDBEdit.GetReadOnly: Boolean;
begin
  if FDataLink <> nil then
    Result := FDataLink.ReadOnly
  else
    Result := False;
end;


procedure TRzDBEdit.SetReadOnly( Value: Boolean );
begin
  FDataLink.ReadOnly := Value;
  ReadOnlyChanged;
end;


procedure TRzDBEdit.SetFocused( Value: Boolean );
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if ( Alignment <> taLeftJustify) and not IsMasked then
      Invalidate;
    FDataLink.Reset;
  end;
end;


procedure TRzDBEdit.ActiveChanged;
begin
  ResetMaxLength;
end;


procedure TRzDBEdit.ActiveChangeHandler( Sender: TObject );
begin
  ActiveChanged;
end;


procedure TRzDBEdit.DataChanged;
begin
  if FDataLink.Field <> nil then
  begin
    if FAlignment <> FDataLink.Field.Alignment then
    begin
      EditText := '';  {forces update}
      FAlignment := FDataLink.Field.Alignment;
    end;
    EditMask := FDataLink.Field.EditMask;
    if not ( csDesigning in ComponentState ) then
    begin
      if ( FDataLink.Field.DataType in [ ftString, ftWideString ] ) and ( MaxLength = 0 ) then
        MaxLength := FDataLink.Field.Size;
    end;
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      EditText := FDataLink.Field.DisplayText;
      if FDataLink.Editing {and FDataLink.FModified} then   // FModified is private
        Modified := True;
    end;
  end
  else
  begin
    FAlignment := taLeftJustify;
    EditMask := '';
    if csDesigning in ComponentState then
      EditText := Name
    else
      EditText := '';
  end;
end; {= TRzDBEdit.DataChanged =}


procedure TRzDBEdit.DataChangeHandler( Sender: TObject );
begin
  DataChanged;
end;


procedure TRzDBEdit.EditingChanged;
begin
  inherited ReadOnly := not FDataLink.Editing;
end;


procedure TRzDBEdit.EditingChangeHandler( Sender: TObject );
begin
  EditingChanged;
end;


procedure TRzDBEdit.UpdateData;
begin
  ValidateEdit;
  FDataLink.Field.Text := Text;
end;


procedure TRzDBEdit.UpdateDataHandler( Sender: TObject );
begin
  UpdateData;
end;


procedure TRzDBEdit.WMUndo( var Msg: TMessage );
begin
  if FDataLink.Edit then
    inherited;
end;


procedure TRzDBEdit.WMCut( var Msg: TMessage );
begin
  if FDataLink.Edit then
    inherited;
end;


procedure TRzDBEdit.WMPaste( var Msg: TMessage );
begin
  if FDataLink.Edit then
    inherited;
end;


procedure TRzDBEdit.WMClear( var Msg: TMessage );
begin
  if FDataLink.Edit then
    inherited;
end;


procedure TRzDBEdit.CMEnter( var Msg: TCMEnter );
begin
  SetFocused( True );
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
end;


procedure TRzDBEdit.CMExit( var Msg: TCMExit );
begin
  // Replace call to inherited with just a call to UpdateFrame. This change
  // eliminates the extra OnExit event that was being generated and still
  // allows the FocusColor property to work correctly.
  UpdateFrame( False, False );

  try
    if FDataLink.Editing then
      FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused( False );
  CheckCursor;

  if not ( csRecreating in ControlState ) then
    DoExit;
end;


procedure TRzDBEdit.CMGetDataLink( var Msg: TMessage );
begin
  Msg.Result := Integer( FDataLink );
end;


function TRzDBEdit.ExecuteAction( Action: TBasicAction ): Boolean;
begin
  Result := inherited ExecuteAction( Action ) or ( FDataLink <> nil ) and FDataLink.ExecuteAction( Action );
end;


function TRzDBEdit.UpdateAction( Action: TBasicAction ): Boolean;
begin
  Result := inherited UpdateAction( Action ) or ( FDataLink <> nil ) and FDataLink.UpdateAction( Action );
end;


procedure TRzDBEdit.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    if Parent is TDBCtrlPanel then
    begin
      ParentCtl3D := not FFrameVisible;
      Ctl3D := not FFrameVisible;
      Invalidate;
    end
    else
    begin
      if FFrameVisible then
        Ctl3D := True;
      RecreateWnd;
    end;
  end;
end;



{==============================}
{== TRzDBNumericEdit Methods ==}
{==============================}

{$IFDEF VCL160_OR_HIGHER}
class constructor TRzDBNumericEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook( TRzDBNumericEdit, TRzEditStyleHook );
end;


class destructor TRzDBNumericEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook( TRzDBNumericEdit, TRzEditStyleHook );
end;
{$ENDIF}


constructor TRzDBNumericEdit.Create( AOwner: TComponent );
begin
  inherited;

  // Pass nil since this component is not a TRzCalculator
  FCalculatorColors := TRzCalculatorColors.Create( nil );
  FCalculatorBoldButtons := False;

  Height := 21;
  Width := 65;
  FAllowBlank := True;
  FAllowScientificNotation := True;
  FBlankValue := 0;
  FIntegersOnly := True;
  FCheckRange := False;
  FMin := 0;
  FMax := 0;
  Alignment := taRightJustify;
  FDisplayFormat := ',0;(,0)';
  FFieldValue := 0.0;
  SetValue( FFieldValue );
  Text := FormatText( FFieldValue );

  FPopupWidth := 0;
  FPopupHeight := 0;
  FPopupAlignment := taRightJustify;
end;


procedure TRzDBNumericEdit.CreateParams( var Params: TCreateParams );
begin
  inherited;
end;


procedure TRzDBNumericEdit.CreateWnd;
begin
  inherited;

  SetValue( Value );
  Text := FormatText( Value );
end;


destructor TRzDBNumericEdit.Destroy;
begin
  FCalculatorColors.Free;
  inherited;
end;


procedure TRzDBNumericEdit.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  FDecimalPressed := Key = vk_Decimal;
end;


procedure TRzDBNumericEdit.KeyPress( var Key: Char );
begin
  if FDecimalPressed then
  begin
    FDecimalPressed := False;
    Key := FormatSettings.DecimalSeparator;
  end;

  inherited;

  // If Key is cleared by inherited KeyPress, then exit
  if Key = #0 then
    Exit;

  if not IsValidChar( Key ) then
  begin
    Key := #0;
    InvalidKeyPressed;
  end;
end;


function TRzDBNumericEdit.IsValidChar( Key: Char ): Boolean;
begin
  if FIntegersOnly then
  begin
    Result := CharInSet( Key, [ '+', '-', '0'..'9', '(', ')' ] )
              or
              ( ( Key < #32 ) and ( Key <> Chr( vk_Return ) ) );
  end
  else if FAllowScientificNotation then
  begin
    Result := CharInSet( Key, [ '0'..'9', 'E', 'e', '+', '-', AnsiChar( FormatSettings.DecimalSeparator ), '(', ')' ] )
              or
              ( ( Key < #32 ) and ( Key <> Chr( vk_Return ) ) );
  end
  else
  begin
    Result := CharInSet( Key, [ '0'..'9', '+', '-', AnsiChar( FormatSettings.DecimalSeparator ), '(', ')' ] )
              or
              ( ( Key < #32 ) and ( Key <> Chr( vk_Return ) ) );
  end;

  if Result then
  begin
    if Key = FormatSettings.DecimalSeparator then
    begin
      if SelLength = 0 then
        Result := Pos( FormatSettings.DecimalSeparator, Text ) = 0
      else
      begin
        Result := Pos( FormatSettings.DecimalSeparator, Text ) = 0;
        if not Result then
          Result := Pos( FormatSettings.DecimalSeparator, SelText ) <> 0;
      end;
    end
    else if FIntegersOnly then
    begin
      if ( Key = '+' ) or ( Key = '-' ) then
      begin
        if ( SelStart = 0 ) and ( Pos( '+', Text ) = 0 ) and ( Pos( '-', Text ) = 0 ) then
        begin
          // Cursor is at beginning and there is no + or - currently in the text
          Result := True;
        end
        else if SelLength = Length( Text ) then
        begin
          // All text in the edit field is selected. Enter a + or - is acceptable.
          Result := True;
        end
        else if ( SelStart = 0 ) and ( SelLength > 0 ) and
                ( ( Pos( '+', SelText ) > 0 ) or ( Pos( '-', SelText ) > 0 ) ) then
        begin
          // Cursor is at beginning and there is a selection at the beginning.  If the selection contains a + or - then
          // allow the new + or - to replace the old one.
          Result := True;
        end
        else
          Result := False;
      end
      else
      begin
        // Check if a digit is being entered at the beginning, but the text already has a sign symbol at the beginning.
        if ( SelStart = 0 ) and ( SelLength = 0 ) and ( ( Pos( '+', Text ) = 1 ) or ( Pos( '-', Text ) = 1 ) ) then
          Result := False;
      end;
    end;
  end;
end;


function TRzDBNumericEdit.GetCalculatorVisible: Boolean;
begin
  Result := DropButtonVisible;
end;


procedure TRzDBNumericEdit.SetCalculatorVisible( Value: Boolean );
begin
  DropButtonVisible := Value;
end;


procedure TRzDBNumericEdit.SetIntegersOnly( Value: Boolean );
begin
  if FIntegersOnly <> Value then
  begin
    FIntegersOnly := Value;
    if FIntegersOnly then
    begin
      SetValue( Round( GetValue ) );
    end;
  end;
end;


procedure TRzDBNumericEdit.SetMin( const Value: Extended );
begin
  if FMin <> Value then
  begin
    FMin := Value;
    if FMax < FMin then
      FMax := FMin;
    Invalidate;
  end;
end;


procedure TRzDBNumericEdit.SetMax( const Value: Extended );
begin
  if FMax <> Value then
  begin
    FMax := Value;
    if FMin > FMax then
      FMin := FMax;
    Invalidate;
  end;
end;


function TRzDBNumericEdit.GetIntValue: Int64;
begin
  Result := Round( GetValue );
end;


procedure TRzDBNumericEdit.SetIntValue( Value: Int64 );
begin
  SetValue( Value );
end;


function TRzDBNumericEdit.GetValue: Extended;
begin
  if Field = nil then
  begin
    Result := FMin;
    Exit;
  end;

  try
    if Text = '' then
    begin
      if FAllowBlank then
        Result := FBlankValue
      else
      begin
        Text := FormatText( FMin );
        Result := EvaluateText;
      end;
    end
    else
    begin
      Result := EvaluateText;
    end;
  except
    Result := FMin;
  end;
end;


procedure TRzDBNumericEdit.RangeError( EnteredValue, AdjustedValue: Extended; var AutoCorrect: Boolean );
begin
  if Assigned( FOnRangeError ) then
    FOnRangeError( Self, EnteredValue, AdjustedValue, AutoCorrect );
end;


function TRzDBNumericEdit.CheckValue( const Value: Extended; var KeepFocusOnEdit: Boolean ): Extended;
var
  AutoCorrect: Boolean;
begin
  Result := Value;
  if ( FMax <> FMin ) or FCheckRange then
  begin
    AutoCorrect := True;
    if Value < FMin then
    begin
      RangeError( Value, FMin, AutoCorrect );
      if AutoCorrect then
        Result := FMin
      else
        KeepFocusOnEdit := True;
    end
    else if Value > FMax then
    begin
      RangeError( Value, FMax, AutoCorrect );
      if AutoCorrect then
        Result := FMax
      else
        KeepFocusOnEdit := True;
    end;
  end;

  FModified := ( Result <> FFieldValue );
  FFieldValue := Result;
end;


procedure TRzDBNumericEdit.SetValue( const Value: Extended );
begin
  if ( Field <> nil ) and ( Value <> EvaluateText ) then
    Text := FormatText( Value );
end;


procedure TRzDBNumericEdit.DataChanged;
begin
  if Field <> nil then
  begin
    if FAllowBlank and Field.IsNull then
      Text := ''
    else
    begin
      Value := DataLink.Field.AsFloat;
      Text := FormatText( Value );
    end;
  end
  else
  begin
    if csDesigning in ComponentState then
      EditText := Name
    else
      EditText := '';
  end;
end;


procedure TRzDBNumericEdit.UpdateData;
begin
  if DataLink.Editing then
  begin
    if FAllowBlank and ( Text = '' ) then
      DataLink.Field.Clear
    else
      DataLink.Field.AsFloat := Value;
  end;
end;


procedure TRzDBNumericEdit.CMEnter( var Msg: TCMEnter );
begin
  if Field <> nil then
  begin
    FModified := False;
    FFieldValue := EvaluateText;
    if not FAllowBlank then
      Text := FormatText( FFieldValue );
  end;
  inherited;
end;


procedure TRzDBNumericEdit.CMExit( var Msg: TCMExit );
var
  N: Extended;
  MustSet, KeepFocusOnEdit: Boolean;
begin
  if Field <> nil then
  begin
    if FAllowBlank and ( Text = '' ) then
    begin
      inherited;
      Exit;
    end;

    MustSet := False;
    try
      KeepFocusOnEdit := False;
      N := CheckValue( EvaluateText, KeepFocusOnEdit );
      if KeepFocusOnEdit then
      begin
        SetFocus;
        Exit;
      end;
    except
      N := FMin;
      MustSet := True;
    end;
    if MustSet then
      SetValue( N );
    Text := FormatText( N );
  end;
  inherited;
end;


procedure TRzDBNumericEdit.SetDisplayFormat( FormatString: string );
begin
  if FDisplayFormat <> FormatString then
  begin
    FDisplayFormat := FormatString;
    SetValue( Value );
    if not ( csLoading in ComponentState ) then
      Text := FormatText( Value );
  end;
end;


procedure TRzDBNumericEdit.SetCalculatorColors( Value: TRzCalculatorColors );
begin
  FCalculatorColors.Assign( Value );
end;


function TRzDBNumericEdit.FormatText( const Value: Extended ): string;
begin
  if Field <> nil then
    Result := FormatFloat( FDisplayFormat, Value )
  else
    Result := '';
end;


function TRzDBNumericEdit.GetDisplayString: string;
begin
  if Field <> nil then
    Result := FormatFloat( FDisplayFormat, Field.AsFloat )
  else
    Result := '';
end;


function TRzDBNumericEdit.EvaluateText: Extended;
var
  S, NumberStr: string;
  C: Char;
  I: Byte;
  IsNeg, IncludeChar: Boolean;
  PosMinus, PosOpenParen, PosCloseParen: Integer;
begin
  NumberStr := Trim( Text );
  if Length( NumberStr ) > 0 then
  begin
    // First determine if the number is to be intrpreted as negative
    // That is, minus sign that does not follow E in scientific notation
    // or open-closed parentheses.

    PosMinus := Pos( '-', NumberStr );
    PosOpenParen := Pos( '(', NumberStr );
    PosCloseParen := Pos( ')', NumberStr );

    IsNeg := ( PosMinus = 1 ) or
             ( ( PosOpenParen = 1 ) and ( PosCloseParen = Length( NumberStr ) ) );


    // Filter out non-numeric characters
    S := '';

    for I := 1 to Length( NumberStr ) do
    begin
      C := NumberStr[ I ];
      if FAllowScientificNotation then
        IncludeChar := CharInSet( C, [ '0'..'9', 'E', 'e', '-', AnsiChar( FormatSettings.DecimalSeparator ) ] )
      else
        IncludeChar := CharInSet( C, [ '0'..'9', '-', AnsiChar( FormatSettings.DecimalSeparator ) ] );

      if IncludeChar then
      begin
        if FAllowScientificNotation and ( C = '-' ) then
        begin
          if ( I > 1 ) and CharInSet( NumberStr[ I - 1 ], [ 'E', 'e' ] ) then
            S := S + C;
        end
        else
          S := S + C;
      end;
    end;

    if S <> '' then
    begin
      if IsNeg and ( FirstNonWhitespaceChar( S ) <> '-' ) then
        S := '-' + S;
      Result := StrToFloatDef( S, 0.0 );
    end
    else
      Result := 0;
  end
  else
    Result := 0;
end;


procedure TRzDBNumericEdit.CloseUp;
begin
  if not ReadOnly and ( Field <> nil ) and Field.CanModify and ( DataSource <> nil ) then
  begin
    try
      inherited;
    except
    end;
  end;
end;


procedure TRzDBNumericEdit.DropDown;
var
  OldOnExitHandler: TNotifyEvent;
  OldOnEnterHandler: TNotifyEvent;
begin
  if not ReadOnly and ( Field <> nil ) and Field.CanModify and ( DataSource <> nil ) then
  begin
    try
      inherited;
      OldOnEnterHandler := OnEnter;
      OnEnter := nil;
      OldOnExitHandler := OnExit;
      OnExit := nil;
      try
        DisplayCalculator;
      finally
        OnEnter := OldOnEnterHandler;
        OnExit := OldOnExitHandler;
      end;
    except
    end;
  end;
end;


procedure TRzDBNumericEdit.DisplayCalculator;
var
  PopupPanel: TRzPopupPanel;
  Calculator: TRzCalculator;
  SaveAutoSelect: Boolean;
begin
  SaveAutoSelect := AutoSelect;
  AutoSelect := False;
  try
    if Self.Parent <> nil then
    begin
      PopupPanel := TRzPopupPanel.Create( Self );
      try
        Calculator := TRzCalculator.Create( PopupPanel );
        Calculator.Parent := PopupPanel;

        if ( FPopupWidth <> 0 ) and ( FPopupHeight <> 0 ) then
        begin
          Calculator.AutoSize := False;
          Calculator.Width := FPopupWidth;
          Calculator.Height := FPopupHeight;
        end;

        PopupPanel.Parent := Self.Parent;
        PopupPanel.Font.Name := Font.Name;
        PopupPanel.Font.Color := Font.Color;
        PopupPanel.Font.Size := Font.Size;
        PopupPanel.Alignment := FPopupAlignment;

        Calculator.IsPopup := True;
        Calculator.ThemeAware := UseThemes;
        Calculator.Color := Color;

        Calculator.Handle;
        Calculator.Result := GetValue;

        if FrameVisible and not UseThemes and ( FrameStyle = fsFlat ) or ( FrameStyle = fsFlatBold ) then
        begin
          Calculator.BorderOuter := fsFlat;
          Calculator.FlatColor := FrameColor;
        end;
        Calculator.Visible := True;
        Calculator.CalculatorColors := FCalculatorColors;
        Calculator.BoldButtons := FCalculatorBoldButtons;
        Calculator.OnSetBtnClick := PopupPanel.Close;

        if PopupPanel.Popup( Self ) then
        begin
          SetValue( Calculator.Result );
        end;
      finally
        PopupPanel.Free;
      end;
    end;
  finally
    AutoSelect := SaveAutoSelect;
  end;
end; {= TRzDBNumericEdit.DisplayCalculator =}
{=============================}
{== TRzDBExpandEdit Methods ==}
{=============================}

constructor TRzDBExpandEdit.Create( AOwner: TComponent );
begin
  inherited;

  FExpandOn := etNone;
  FExpanded := False;
  FExpandedWidth := 0;
  FOrigWidth := Width;
  {&RCI}
end;


procedure TRzDBExpandEdit.WMSetFocus( var Msg: TWMSetFocus );
begin
  if ( FExpandOn = etFocus ) and not FExpanded and ( FExpandedWidth > 0 ) then
  begin
    BringToFront;
    FExpanded := True;
    FOrigWidth := Width;
    Width := FExpandedWidth;
    if UseRightToLeftAlignment then
    begin
      // On RTL systems, need to update the Left position to give the appearance
      // that the control is expanding to the left.
      Left := Left - ( FExpandedWidth - FOrigWidth );
    end;
    if AutoSelect then
    begin
      SelLength := 0;
      SelectAll;
    end;
  end;
  inherited;
end;


procedure TRzDBExpandEdit.WMRButtonUp( var Msg: TWMRButtonUp );
begin
  if ( FExpandOn = etMouseButton2Click ) and ( FExpandedWidth > 0 ) then
  begin
    if not FExpanded then
    begin
      BringToFront;
      FExpanded := True;
      FOrigWidth := Width;
      Width := FExpandedWidth;
      SetFocus;
      if AutoSelect then
      begin
        SelLength := 0;
        SelectAll;
      end;
    end
    else
    begin
      Width := FOrigWidth;
      FExpanded := False;
    end;
  end
  else
    inherited;
end;


procedure TRzDBExpandEdit.WMKillFocus( var Msg: TWMKillFocus );
begin
  if ( FExpandOn <> etNone ) and ( FExpandedWidth > 0 ) and FExpanded then
  begin
    Width := FOrigWidth;
    if UseRightToLeftAlignment then
      Left := Left + ( FExpandedWidth - FOrigWidth );
  end;
  FExpanded := False;
  inherited;
end;


procedure TRzDBExpandEdit.SetExpandedWidth( Value: Integer );
begin
  {&RV}
  if FExpandedWidth <> Value then
  begin
    FExpandedWidth := Value;
    Repaint;
  end;
end;

procedure TRzDBExpandEdit.SetExpandOn( Value: TExpandOnType );
begin
  if FExpandOn <> Value then
  begin
    FExpandOn := Value;
    Repaint;
  end;
end;


{===============================}
{== TRzDBDateTimeEdit Methods ==}
{===============================}

{$IFDEF VCL160_OR_HIGHER}
class constructor TRzDBDateTimeEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook( TRzDBDateTimeEdit, TRzEditStyleHook );
end;


class destructor TRzDBDateTimeEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook( TRzDBDateTimeEdit, TRzEditStyleHook );
end;
{$ENDIF}


constructor TRzDBDateTimeEdit.Create( AOwner: TComponent );
begin
  inherited;

  DropButtonVisible := True;
  SetEditType( etDate );
  FCalendarElements := [ ceYear, ceMonth, ceArrows, ceFillDays, ceDaysOfWeek, ceTodayButton, ceClearButton ];
  FFirstDayOfWeek := fdowLocale;
  FTwoDigitYearConverter := ycStandard;

  FUseFormatToParse := False;
  FRestrictMinutes := False;
  FShowHowToUseHint := True;

  // Pass nil since this component is not a TRzTimePicker
  FClockFaceColors := TRzClockFaceColors.Create( nil );
  // Pass nil since this component is not a TRzCalendar
  FCalendarColors := TRzCalendarColors.Create( nil );

  FPopupAlignment := taRightJustify;
  FPopupWidth := 0;
  FPopupHeight := 0;
  FPopupButtonColor := clBtnFace;
  FPopupButtonFontColor := clWindowText;
end;


destructor TRzDBDateTimeEdit.Destroy;
begin
  FClockFaceColors.Free;
  FCalendarColors.Free;
  inherited;
end;


function TRzDBDateTimeEdit.CanEditData: Boolean;
begin
  if not ReadOnly and ( Field <> nil ) and Field.CanModify and ( DataSource <> nil ) then
  begin
    DataSource.Edit;
    Result := DataSource.State in dsEditModes;
  end
  else
    Result := False;
end;


procedure TRzDBDateTimeEdit.SetDateTime;
var
  DT: TDateTime;
begin
  if Modified then
  begin
    try
      Modified := False;
      if FEditType = etTime then
      begin
        if not FUseFormatToParse then
          StrToTimeEx( Text, DT )
        else
          StrToTimeEx( Text, DT, FFormat );
        FDateTime := DT;
        FTimeHasBeenSet := True;
      end
      else // FEditType = etDate
      begin
        StrToDateEx( Text, DT, FTwoDigitYearConverter );
        FDateTime := DT;
        FDateHasBeenSet := True;
      end;
      CheckDateTimeChange;
    except
      // Catch all exceptions during this conversion
    end;
  end;
end;


procedure TRzDBDateTimeEdit.CheckDateTimeChange;
begin
  if FEditType = etTime then
  begin
    if Frac( FDateTime ) <> Frac( FLastDateTime ) then
    begin
      DateTimeChange;
      FLastDateTime := FDateTime;
    end;
  end
  else
  begin
    if Trunc( FDateTime ) <> Trunc( FLastDateTime ) then
    begin
      DateTimeChange;
      FLastDateTime := FDateTime;
    end;
  end;
end;


procedure TRzDBDateTimeEdit.DateTimeChange;
begin
  if Assigned( FOnDateTimeChange ) then
    FOnDateTimeChange( Self, FDateTime );
end;


procedure TRzDBDateTimeEdit.CMEnter( var Msg: TCMEnter );
begin
  inherited;
  UpdateText;
end;


procedure TRzDBDateTimeEdit.CMExit( var Msg: TCMExit );
var
  Success: Boolean;
begin
  Success := True;
  if not FValidating and not ( csDestroying in ComponentState ) then
  begin
    FValidating := True;
    try
      try
        Success := ReformatDateTime;
      except
        // Must display error message first, then set focus back to control
        Application.MessageBox( PChar( sRzDateRange ),
                                PChar( Application.Title ),
                                MB_OK + MB_ICONSTOP );
        SetFocus;
      end;
    finally
      FValidating := False;
    end;
  end;

  if Success then
    inherited;
end;


procedure TRzDBDateTimeEdit.InvalidDate( var KeepFocused, KeepInvalidText: Boolean;
                                         var NewDate: TDateTime );
begin
  if Assigned( FOnInvalidDate ) then
    FOnInvalidDate( Self, KeepFocused, KeepInvalidText, NewDate );
end;


procedure TRzDBDateTimeEdit.InvalidTime( var KeepFocused, KeepInvalidText: Boolean;
                                       var NewTime: TDateTime );
begin
  if Assigned( FOnInvalidTime ) then
    FOnInvalidTime( Self, KeepFocused, KeepInvalidText, NewTime );
end;


procedure TRzDBDateTimeEdit.UpdateText;
var
  TempFormat: string;
begin
  TempFormat := FFormat;
  if TempFormat = '' then
  begin
    if FEditType = etTime then
      TempFormat := 't'
    else
      TempFormat := 'ddddd';
  end;

  FUpdating := True;
  try
    if FEditType = etTime then
    begin
      if not FSettingTime and
         ( ( ( FDateTime = 0 ) and not FTimeHasBeenSet and ( Field <> nil ) and Field.IsNull ) or
           ( ( FDateTime = 0 ) and FTimeHasBeenSet and ( Text = '' ) and not FTimePicked ) ) then
      begin
        Text := '';
      end
      else
        Text := FormatDateTime( TempFormat, FDateTime );
    end
    else // FEditType = etDate
    begin
      if not FSettingDate and
         ( ( ( FDateTime = 0 ) and not FDateHasBeenSet ) or
           ( ( FDateTime = 0 ) and FDateHasBeenSet and ( Text = '' ) and not FDatePicked ) ) then
        Text := ''
      else
        Text := FormatDateTime( TempFormat, FDateTime );
    end;
  finally
    FUpdating := False;
  end;
  Modified := False;
end; {= TRzDBDateTimeEdit.UpdateText =}


procedure TRzDBDateTimeEdit.CloseUp;
begin
  if not ReadOnly and ( Field <> nil ) and Field.CanModify and ( DataSource <> nil ) then
  begin
    try
      inherited;
    except
    end;
  end;
end;


procedure TRzDBDateTimeEdit.DropDown;
var
  OldOnExitHandler: TNotifyEvent;
  OldOnEnterHandler: TNotifyEvent;
begin
  if not ReadOnly and ( Field <> nil ) and Field.CanModify and ( DataSource <> nil ) then
  begin
    try
      inherited;
      OldOnEnterHandler := OnEnter;
      OnEnter := nil;
      OldOnExitHandler := OnExit;
      OnExit := nil;
      try
        if FEditType = etDate then
          DisplayCalendar
        else
          DisplayTimePicker;
      finally
        OnEnter := OldOnEnterHandler;
        OnExit := OldOnExitHandler;
      end;
    except
    end;
  end;
end;


function TRzDBDateTimeEdit.DaysToBitmask( Days: array of Byte ): Cardinal;
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


procedure TRzDBDateTimeEdit.DisplayCalendar;
var
  PopupPanel: TRzPopupPanel;
  Calendar: TRzCalendar;
  SaveAutoSelect: Boolean;
  DT: TDateTime;
  RangeCheck: Integer;
begin
  SaveAutoSelect := AutoSelect;
  AutoSelect := False;
  try
    if Self.Parent <> nil then
    begin
      PopupPanel := TRzPopupPanel.Create( Self );
      try
        Calendar := TRzCalendar.Create( PopupPanel );
        Calendar.Parent := PopupPanel;

        if ( FPopupWidth <> 0 ) and ( FPopupHeight <> 0 ) then
        begin
          Calendar.AutoSize := False;
          Calendar.Width := FPopupWidth;
          Calendar.Height := FPopupHeight;
        end;

        PopupPanel.Parent := Self.Parent;
        PopupPanel.Font.Name := Font.Name;
        PopupPanel.Font.Color := Font.Color;
        PopupPanel.Font.Size := Font.Size;
        PopupPanel.Alignment := FPopupAlignment;

        Calendar.IsPopup := True;
        Calendar.ThemeAware := UseThemes;
        Calendar.Color := Color;
        Calendar.ButtonColor := FPopupButtonColor;
        Calendar.ButtonFontColor := FPopupButtonFontColor;
        Calendar.Elements := FCalendarElements;
        Calendar.FirstDayOfWeek := FFirstDayOfWeek;
        Calendar.CaptionClearBtn := FCaptionClearBtn;
        Calendar.CaptionTodayBtn := FCaptionTodayBtn;

        Calendar.OnGetBoldDays := FOnGetBoldDays;
        Calendar.OnGetDayFormat := FOnGetDayFormat;
        Calendar.OnGetWeekNumber := FOnGetWeekNumber;
        Calendar.OnViewDateChange := FOnViewDateChange;
        Calendar.Handle;

        Calendar.MinDate := FMinDate;
        Calendar.MaxDate := FMaxDate;

        DT := GetDate;
        if DT = 0 then
        begin
          Calendar.Date := SysUtils.Date;
        end
        else
        begin
          RangeCheck := DateInRange( DT );
          if RangeCheck > 0 then
            Calendar.Date := FMaxDate
          else if RangeCheck < 0 then
            Calendar.Date := FMinDate
          else
            Calendar.Date := DT;
        end;

        if FrameVisible and not UseThemes and ( FrameStyle = fsFlat ) or ( FrameStyle = fsFlatBold ) then
        begin
          Calendar.BorderOuter := fsFlat;
          Calendar.FlatColor := FrameColor;
        end;
        Calendar.Visible := True;
        Calendar.CalendarColors := FCalendarColors;
        Calendar.OnClick := PopupPanel.Close;

        // Setting FValidating to True before popping up calendar prevents the
        // exceptions during reformatting if the current date is out of range
        // when the focus is changed to the popup calendar.
        FValidating := True;
        try
          if PopupPanel.Popup( Self ) then
          begin
            FDatePicked := True;
            if Calendar.ClearClicked then
              Clear
            else
              SetDate( Calendar.Date );
            FDatePicked := False;
          end;
        finally
          FValidating := False;
        end;
      finally
        PopupPanel.Free;
      end;
    end;
  finally
    AutoSelect := SaveAutoSelect;
  end;
end; {= TRzDBDateTimeEdit.DisplayCalendar =}


procedure TRzDBDateTimeEdit.DisplayTimePicker;
var
  PopupPanel: TRzPopupPanel;
  TimePicker: TRzTimePicker;
  SaveAutoSelect: Boolean;
begin
  SaveAutoSelect := AutoSelect;
  AutoSelect := False;
  try
    if Self.Parent <> nil then
    begin
      PopupPanel := TRzPopupPanel.Create( Self );
      try
        TimePicker := TRzTimePicker.Create( PopupPanel );
        TimePicker.Parent := PopupPanel;

        if ( FPopupWidth <> 0 ) and ( FPopupHeight <> 0 ) then
        begin
          TimePicker.AutoSize := False;
          TimePicker.Width := FPopupWidth;
          TimePicker.Height := FPopupHeight;
        end;

        PopupPanel.Parent := Self.Parent;
        PopupPanel.Font.Name := Font.Name;
        PopupPanel.Font.Color := Font.Color;
        PopupPanel.Font.Size := Font.Size;
        PopupPanel.Alignment := FPopupAlignment;

        TimePicker.IsPopup := True;
        TimePicker.ThemeAware := UseThemes;
        TimePicker.Color := Color;
        TimePicker.ButtonColor := FPopupButtonColor;
        TimePicker.ButtonFontColor := FPopupButtonFontColor;
        TimePicker.CaptionAM := FCaptionAM;
        TimePicker.CaptionPM := FCaptionPM;
        TimePicker.CaptionSet := FCaptionSet;
        TimePicker.Handle;
        TimePicker.Time := GetTime;
        if FrameVisible and not UseThemes and ( FrameStyle = fsFlat ) or ( FrameStyle = fsFlatBold ) then
        begin
          TimePicker.BorderOuter := fsFlat;
          TimePicker.FlatColor := FrameColor;
        end;
        TimePicker.Visible := True;
        TimePicker.ClockFaceColors := FClockFaceColors;
        TimePicker.Format := FFormat;
        TimePicker.ShowSetButton := True;
        TimePicker.ShowHowToUseHint := FShowHowToUseHint;
        TimePicker.HowToUseMsg := FHowToUseMsg;
        TimePicker.RestrictMinutes := FRestrictMinutes;
        TimePicker.OnSetBtnClick := PopupPanel.Close;

        if PopupPanel.Popup( Self ) then
        begin
          FTimePicked := True;
          SetTime( TimePicker.Time );
          FTimePicked := False;
        end;
      finally
        PopupPanel.Free;
      end;
    end;
  finally
    AutoSelect := SaveAutoSelect;
  end;
end; {= TRzDBDateTimeEdit.DisplayTimePicker =}


procedure TRzDBDateTimeEdit.Change;
begin
  if not FUpdating then
  begin
    SetDateTime;
  end;
  inherited;
end;


procedure TRzDBDateTimeEdit.AdjustYear( DeltaYears: Integer );
var
  DT: TDateTime;
begin
  if not CanEditData then
    Exit;

  DT := FDateTime;
  if DT = 0 then
    DT := SysUtils.Date;
  DT := IncYear( DT, DeltaYears );

  if ( ( Trunc( MinDate ) = 0 ) and ( Trunc( MaxDate ) = 0 ) ) or
     ( ( Trunc( DT ) <= Trunc( MaxDate ) ) and ( Trunc( DT ) >= Trunc( MinDate ) ) ) then
  begin
    FDateTime := DT;
    CheckDateTimeChange;
    UpdateText;
  end;
end;


procedure TRzDBDateTimeEdit.AdjustMonth( DeltaMonths: Integer );
var
  DT: TDateTime;
begin
  if not CanEditData then
    Exit;

  DT := FDateTime;
  if DT = 0 then
    DT := SysUtils.Date;
  DT := IncMonth( DT, DeltaMonths );

  if ( ( Trunc( MinDate ) = 0 ) and ( Trunc( MaxDate ) = 0 ) ) or
     ( ( Trunc( DT ) <= Trunc( MaxDate ) ) and ( Trunc( DT ) >= Trunc( MinDate ) ) ) then
  begin
    FDateTime := DT;
    CheckDateTimeChange;
    UpdateText;
  end;
end;


procedure TRzDBDateTimeEdit.AdjustDay( DeltaDays: Integer );
var
  DT: TDateTime;
begin
  if not CanEditData then
    Exit;

  DT := FDateTime;
  if DT = 0 then
    DT := SysUtils.Date;
  DT := IncDay( DT, DeltaDays );

  if ( ( Trunc( MinDate ) = 0 ) and ( Trunc( MaxDate ) = 0 ) ) or
     ( ( Trunc( DT ) <= Trunc( MaxDate ) ) and ( Trunc( DT ) >= Trunc( MinDate ) ) ) then
  begin
    FDateTime := DT;
    CheckDateTimeChange;
    UpdateText;
  end;
end;


procedure TRzDBDateTimeEdit.AdjustHour( DeltaHours: Int64 );
begin
  if not CanEditData then
    Exit;
  if FDateTime = 0 then
    FDateTime := SysUtils.Now;
  FDateTime := IncHour( FDateTime, DeltaHours );
  if FDateTime < 0 then
  begin
    // Time has been changed to be earlier than 12:00 am.
    FDateTime := IncHour( FDateTime, 24 );
  end;
  CheckDateTimeChange;
  UpdateText;
end;


procedure TRzDBDateTimeEdit.AdjustMinute( DeltaMinutes: Int64 );
begin
  if not CanEditData then
    Exit;
  if FDateTime = 0 then
    FDateTime := SysUtils.Now;
  FDateTime := IncMinute( FDateTime, DeltaMinutes );
  if FDateTime < 0 then
  begin
    // Time has been changed to be earlier than 12:00 am.
    FDateTime := IncHour( FDateTime, 24 );
  end;
  CheckDateTimeChange;
  UpdateText;
end;


procedure TRzDBDateTimeEdit.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;

  if FEditType = etDate then
  begin
    case Key of
      vk_Prior:
      begin
        if ssShift in Shift then
          AdjustYear( 1 )
        else
          AdjustMonth( 1 );
      end;

      vk_Next:
      begin
        if ssShift in Shift then
          AdjustYear( -1 )
        else
          AdjustMonth( -1 );
      end;

      vk_Up:
        AdjustDay( 1 );
      vk_Down:
        AdjustDay( -1 );
    end;
  end
  else // FEditType = etTime
  begin
    case Key of
      vk_Prior:
        AdjustHour( 1 );
      vk_Next:
        AdjustHour( -1 );
      vk_Up:
        AdjustMinute( 1 );
      vk_Down:
        AdjustMinute( -1 );
    end;
  end;
end;


procedure TRzDBDateTimeEdit.KeyPress( var Key: Char );
begin
  if ReadOnly and CharInSet( Key, [ '+', '=', '-', '_' ] ) then
    Key := #0
  else if ( FEditType = etTime ) and not CharInSet( Key, [ #8, #13, #27 ] ) and IsCharAlpha( Key ) and
     not CharInSet( Upcase( Key ), [ 'A', 'P', 'M' ] ) then
  begin
    Key := #0;
    InvalidKeyPressed;
  end
  else if FEditType = etDate then
  begin
    if Key <> FormatSettings.DateSeparator then
    begin
      case Key of
        '+', '=':
        begin
          AdjustDay( 1 );
          Key := #0;
        end;

        '-', '_':
        begin
          AdjustDay( -1 );
          Key := #0;
        end;

        else
          inherited;
      end;
    end
    else
      inherited;
  end
  else // FEdit = etTime
  begin
    if Key <> FormatSettings.TimeSeparator then
    begin
      case Key of
        '+', '=':
        begin
          AdjustHour( 1 );
          Key := #0;
        end;

        '-', '_':
        begin
          AdjustHour( -1 );
          Key := #0;
        end;

        else
          inherited;
      end;
    end
    else
      inherited;
  end;
end;


function TRzDBDateTimeEdit.ReformatDateTime: Boolean;
var
  RangeCheck: Integer;
  DT, NewDate, NewTime: TDateTime;
  KeepFocused, KeepInvalidText: Boolean;
  OriginalText, Fmt: string;
begin
  Result := True;
  OriginalText := Text;
  SetDateTime;

  if FEditType = etDate then
  begin
    KeepFocused := False;
    KeepInvalidText := False;
    NewDate := 0;
    if not StrToDateEx( Text, DT ) then
    begin
      InvalidDate( KeepFocused, KeepInvalidText, NewDate );

      FDateTime := NewDate;

      if KeepInvalidText or KeepFocused then
      begin
        Result := False; // Do not let CMExit call inherited
        SetFocus;
        if KeepInvalidText then
          Text := OriginalText
        else if FDateTime = 0.0 then
          Text := '';
      end;

      FDateHasBeenSet := False;
    end;

    if GetDate <> 0.0 then
    begin
      RangeCheck := DateInRange( GetDate );
      if RangeCheck > 0 then
        raise ERzCalendarError.CreateFmt( SRzDateMax, [ DateToStr( FMaxDate ) ] )
      else if RangeCheck < 0 then
        raise ERzCalendarError.CreateFmt( SRzDateMin, [ DateToStr( FMinDate ) ] );
    end;

    if not ( KeepInvalidText or KeepFocused ) then
      UpdateText;
  end
  else // FEditType = etTime
  begin
    KeepFocused := False;
    KeepInvalidText := False;
    NewTime := 0;
    if FUseFormatToParse then
      Fmt := FFormat
    else
      Fmt := '';
    if not StrToTimeEx( Text, DT, Fmt ) then
    begin
      // If StrToTimeEx returns False, either there was a format exception or the time was auto-corrected
      NewTime := DT;
      InvalidTime( KeepFocused, KeepInvalidText, NewTime );

      FDateTime := NewTime;

      if KeepInvalidText or KeepFocused then
      begin
        Result := False; // Do not let CMExit call inherited
        SetFocus;
        if KeepInvalidText then
          Text := OriginalText
        else if FDateTime = 0.0 then
          Text := '';
      end;

      FTimeHasBeenSet := False;
    end;

    if not ( KeepInvalidText or KeepFocused ) then
      UpdateText;
  end;
end;


function TRzDBDateTimeEdit.DateInRange( Value: TDateTime ): Integer;
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


function TRzDBDateTimeEdit.GetDate: TDate;
begin
  Result := Trunc( FDateTime );
end;


procedure TRzDBDateTimeEdit.SetDate( Value: TDate );
var
  RangeCheck: Integer;
begin
  if CanEditData then
  begin
    FSettingDate := True;
    try
      RangeCheck := DateInRange( Value );
      if RangeCheck > 0 then
        raise ERzCalendarError.CreateFmt( SRzDateMax, [ DateToStr( FMaxDate ) ] )
      else if RangeCheck < 0 then
        raise ERzCalendarError.CreateFmt( SRzDateMin, [ DateToStr( FMinDate ) ] );

      FDateTime := Value;
      FDateHasBeenSet := True;
      SetEditType( etDate );
      CheckDateTimeChange;
      UpdateText;
    finally
      FSettingDate := False;
    end;
  end;
end;


function TRzDBDateTimeEdit.GetAsDateTime: TDateTime;
begin
  if FEditType = etDate then
    Result := Date
  else
    Result := Time;
end;


procedure TRzDBDateTimeEdit.SetAsDateTime( Value: TDateTime );
begin
  if FEditType = etDate then
    SetDate( Value )
  else
    SetTime( Value );
end;



function TRzDBDateTimeEdit.StoreDate: Boolean;
begin
  Result := ( FEditType = etDate ) and ( Date <> 0.0 );
end;


function TRzDBDateTimeEdit.StoreMinDate: Boolean;
begin
  Result := FMinDate <> 0.0;
end;


procedure TRzDBDateTimeEdit.SetMinDate( Value: TDateTime );
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


function TRzDBDateTimeEdit.StoreMaxDate: Boolean;
begin
  Result := FMaxDate <> 0.0;
end;


procedure TRzDBDateTimeEdit.SetMaxDate( Value: TDateTime );
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


procedure TRzDBDateTimeEdit.SetRange( MinValue, MaxValue: TDate );
var
  TruncDate, TruncMin, TruncMax: Int64;
begin
  // Ignore range checking if EditType is etTime, or if current date is empty
  if ( FEditType <> etDate ) or ( GetDate = 0.0 ) then
    Exit;

  TruncMin := Trunc( MinValue );
  TruncMax := Trunc( MaxValue );
  TruncDate := Trunc( FDateTime );

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


procedure TRzDBDateTimeEdit.SetEditType( Value: TRzDTEditType );
begin
  if FEditType <> Value then
  begin
    FEditType := Value;
    ReformatDateTime;
  end;
end;


procedure TRzDBDateTimeEdit.SetFormat( const Value: string );
begin
  if FFormat <> Value then
  begin
    SetDateTime;
    FFormat := Value;
    UpdateText;
  end;
end;


procedure TRzDBDateTimeEdit.SetClockFaceColors( Value: TRzClockFaceColors );
begin
  FClockFaceColors.Assign( Value );
end;


procedure TRzDBDateTimeEdit.SetCalendarColors( Value: TRzCalendarColors );
begin
  FCalendarColors.Assign( Value );
end;


function TRzDBDateTimeEdit.GetTime: TTime;
begin
  Result := Frac( FDateTime );
end;


procedure TRzDBDateTimeEdit.SetTime( Value: TTime );
begin
  if CanEditData then
  begin
    FSettingTime := True;
    try
      FDateTime := Value;
      FTimeHasBeenSet := True;
      SetEditType( etTime );
      CheckDateTimeChange;
      UpdateText;
    finally
      FSettingTime := False;
    end;
  end;
end;


function TRzDBDateTimeEdit.StoreTime: Boolean;
begin
  Result := ( FEditType = etTime ) and ( Time <> 0.0 );
end;


procedure TRzDBDateTimeEdit.Clear;
begin
  if CanEditData then
  begin
    inherited;
    FDateTime := 0;
    CheckDateTimeChange;
    FTimeHasBeenSet := False;
    FDateHasBeenSet := False;
    UpdateText;
    if Field <> nil then
      Field.Clear;
  end;
end;



procedure TRzDBDateTimeEdit.DataChanged;
begin
  if Field <> nil then
  begin
    FDateTime := DataLink.Field.AsDateTime;
    FDateHasBeenSet := False;
    FTimeHasBeenSet := False;
    UpdateText;
  end
  else
  begin
    if csDesigning in ComponentState then
      EditText := Name
    else
      EditText := '';
  end;
end;


procedure TRzDBDateTimeEdit.UpdateData;
var
  DT: TDateTime;
begin
  if DataLink.Editing then
  begin
    DT := DataLink.Field.AsDateTime;
    if FEditType = etDate then
      ReplaceDate( DT, FDateTime )
    else
      ReplaceTime( DT, FDateTime );
    if ( DT = 0 ) and ( Text = '' ) then
      DataLink.Field.Clear
    else
      DataLink.Field.AsDateTime := DT;
  end;
end;


procedure TRzDBDateTimeEdit.WMGetDlgCode( var Msg: TWMGetDlgCode );
begin
  Msg.Result := dlgc_WantChars + dlgc_WantArrows;
end;



{==========================}
{== TRzPaintMemo Methods ==}
{==========================}

constructor TRzPaintMemo.Create( AOwner: TComponent );
begin
  inherited;
  FEditControl := TRzDBMemo( AOwner );
end;


procedure TRzPaintMemo.Paint;
var
  R: TRect;
  FrameColor: TColor;
  FrameSides: TSides;
  FrameStyle: TFrameStyle;
  FrameVisible: Boolean;
begin
  Canvas.Font := FEditControl.Font;
  Canvas.Brush.Color := FEditControl.Color;
  Canvas.FillRect( ClientRect );

  FrameColor := FEditControl.FrameColor;
  FrameSides := FEditControl.FrameSides;
  FrameStyle := FEditControl.FrameStyle;
  FrameVisible := FEditControl.FrameVisible;

  { Draw Border }
  if FrameVisible then
  begin
    R := ClientRect;
    if FEditControl.Color = clWindow then
    begin
      if FrameStyle = fsFlat then
        DrawSides( Canvas, R, FrameColor, FrameColor, FrameSides )
      else
        DrawBorderSides( Canvas, R, FrameStyle, FrameSides );
    end
    else
    begin
      if FrameStyle = fsFlat then
        DrawSides( Canvas, R, FrameColor, FrameColor, FrameSides )
      else
        DrawColorBorderSides( Canvas, R, FEditControl.Color, FrameStyle, FrameSides );
    end;
  end;

  { Draw Text }
  R := FEditControl.ClientRect;
  if FrameVisible then
    InflateRect( R, -1, -2 )
  else
    InflateRect( R, -1, -1 );
  Inc( R.Left );
  Dec( R.Right, 4 );

  DrawString( Canvas, Caption, R, dt_EditControl or dt_WordBreak or
                                  dt_ExpandTabs or DrawTextAlignments[ Alignment ] );

end; {= TRzPaintMemo.Paint; =}


{=======================}
{== TRzDBMemo Methods ==}
{=======================}

constructor TRzDBMemo.Create( AOwner: TComponent );
begin
  inherited;

  ControlStyle := ControlStyle - [ csSetCaption ];

  FCanvas := TControlCanvas.Create;
  TControlCanvas( FCanvas ).Control := Self;

  FDisabledColor := clBtnFace;
  FReadOnlyColor := clInfoBk;
  FFocusColor := clWindow;
  FNormalColor := clWindow;
  FFrameColor := clBtnShadow;
  FFrameController := nil;
  FFrameControllerNotifications := fccAll;
  FFrameHotColor := clBtnShadow;
  FFrameHotTrack := False;
  FFrameHotStyle := fsFlatBold;
  FFrameSides := sdAllSides;
  FFrameStyle := fsFlat;
  FFrameVisible := False;
  FFramingPreference := fpXPThemes;
  FTabOnEnter := False;

  FPaintControl := TRzPaintMemo.Create( Self );
  FPaintControl.Parent := Self;
  FPaintControl.Visible := False;
end;


procedure TRzDBMemo.CreateWnd;
begin
  inherited;
  LineColChange;
  ClipboardChange;
  {&RCI}
end;


destructor TRzDBMemo.Destroy;
begin
  FPaintControl.Free;
  FPaintControl := nil;

  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  inherited;
end;


procedure TRzDBMemo.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzDBMemo.ReadOldFrameFlatProp( Reader: TReader );
begin
  FFrameHotTrack := Reader.ReadBoolean;
  if FFrameHotTrack then
  begin
    // If the FrameFlat property is stored, then init the FrameHotStyle property and the FrameStyle property.
    // These may be overridden when the rest of the stream is read in. However, we need to re-init them here
    // because the default values of fsStatus and fsLowered have changed in RC3.
    FFrameStyle := fsStatus;
    FFrameHotStyle := fsLowered;
  end;
end;


procedure TRzDBMemo.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzDBMemo.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzDBMemo.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


procedure TRzDBMemo.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  if not FUpdatingColor then
  begin
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


function TRzDBMemo.StoreColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpColor in FFrameControllerNotifications ) ) )
            and
            Enabled;
end;


function TRzDBMemo.StoreFocusColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpFocusColor in FFrameControllerNotifications ) ) )
            and
            ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzDBMemo.StoreDisabledColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpDisabledColor in FFrameControllerNotifications ) );
end;


function TRzDBMemo.StoreReadOnlyColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpReadOnlyColor in FFrameControllerNotifications ) );
end;


function TRzDBMemo.StoreReadOnlyColorOnFocus: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpReadOnlyColorOnFocus in FFrameControllerNotifications ) );
end;


function TRzDBMemo.StoreParentColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpParentColor in FFrameControllerNotifications ) );
end;


function TRzDBMemo.StoreFlatButtonColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtonColor in FFrameControllerNotifications ) );
end;


function TRzDBMemo.StoreFlatButtons: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtons in FFrameControllerNotifications ) );
end;


function TRzDBMemo.StoreFrameColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameColor in FFrameControllerNotifications ) );
end;


function TRzDBMemo.StoreFrameHotColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotColor in FFrameControllerNotifications ) );
end;


function TRzDBMemo.StoreFrameHotTrack: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotTrack in FFrameControllerNotifications ) );
end;


function TRzDBMemo.StoreFrameHotStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotStyle in FFrameControllerNotifications ) );
end;


function TRzDBMemo.StoreFrameSides: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameSides in FFrameControllerNotifications ) );
end;


function TRzDBMemo.StoreFrameStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameStyle in FFrameControllerNotifications ) );
end;


function TRzDBMemo.StoreFrameVisible: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameVisible in FFrameControllerNotifications ) );
end;


function TRzDBMemo.StoreFramingPreference: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFramingPreference in FFrameControllerNotifications ) );
end;


procedure TRzDBMemo.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzDBMemo.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzDBMemo.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBMemo.SetFrameController( Value: TRzFrameController );
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FFrameController := Value;
  if Value <> nil then
  begin
    Value.AddControl( Self );
    Value.FreeNotification( Self );
  end;
end;


procedure TRzDBMemo.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBMemo.SetFrameHotTrack( Value: Boolean );
begin
  if FFrameHotTrack <> Value then
  begin
    FFrameHotTrack := Value;
    if FFrameHotTrack then
    begin
      FrameVisible := True;
      if not ( csLoading in ComponentState ) then
        FFrameSides := sdAllSides;
    end;
    RepaintFrame;
    Invalidate;
  end;
end;


procedure TRzDBMemo.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBMemo.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBMemo.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBMemo.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    if Parent is TDBCtrlPanel then
    begin
      ParentCtl3D := not FFrameVisible;
      Ctl3D := not FFrameVisible;
      Invalidate;
    end
    else
    begin
      if FFrameVisible then
        Ctl3D := True;
      RecreateWnd;
    end;
  end;
end;


procedure TRzDBMemo.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      RepaintFrame;
  end;
end;


function TRzDBMemo.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;


procedure TRzDBMemo.SetReadOnly( Value: Boolean );
begin
  if ReadOnly <> Value then
  begin
    inherited ReadOnly := Value;
    UpdateColors;
  end;
end;


procedure TRzDBMemo.SetReadOnlyColor( Value: TColor );
begin
  FReadOnlyColor := Value;
  if ReadOnly then
    UpdateColors;
end;


procedure TRzDBMemo.JumpTo( ALine, ACol: Integer );
begin
  Line := ALine;
  if Line = ALine then
    Column := ACol;
end;


function TRzDBMemo.GetColumn: Integer;
var
  CaretPos: TPoint;
  P, LinePos, CharPos: Integer;
begin
  Windows.GetCaretPos( CaretPos );

  P := SendMessage( Handle, em_CharFromPos, 0, MakeLParam( Word( CaretPos.X ), Word( CaretPos.Y ) ) );
  if P <> -1 then
  begin
    CharPos := LoWord( P );
    LinePos := HiWord( P );
    Result := CharPos - SendMessage( Handle, em_LineIndex, LinePos, 0 ) + 1;
  end
  else
    Result := 1;
end;


procedure TRzDBMemo.SetColumn( Value: Integer );
var
  P, Len: Integer;
begin
  P := SendMessage( Handle, em_LineIndex, Line - 1, 0 );
  Len := SendMessage( Handle, em_LineLength, P, 0 );
  if Value <= Len + 1 then
  begin
    P := P + Value - 1;
    SendMessage( Handle, em_SetSel, P, P );
    LineColChange;
  end;
end;


function TRzDBMemo.GetLine: Integer;
var
  CaretPos: TPoint;
  P: Integer;
begin
  Windows.GetCaretPos( CaretPos );

  P := SendMessage( Handle, em_CharFromPos, 0, MakeLParam( Word( CaretPos.X ), Word( CaretPos.Y ) ) );
  if P <> -1 then
    Result := HiWord( P ) + 1
  else
    Result := 1;
end;


procedure TRzDBMemo.SetLine( Value: Integer );
var
  P, L: Integer;
begin
  P := SendMessage( Handle, em_LineIndex, Value - 1, 0 );

                       { Position Cursor to correct line number }
  SendMessage( Handle, em_SetSel, P, P );

  if Line = Value then
  begin
                            { Move selected line to top of window }
    L := SendMessage( Handle, em_GetFirstVisibleLine, 0, 0 );
    SendMessage( Handle, em_LineScroll, 0, Value - L - 1 );
    LineColChange;
  end;
end;


procedure TRzDBMemo.Change;
begin
  inherited;
  LineColChange;
end;


procedure TRzDBMemo.Click;
begin
  {&RV}
  inherited;
  LineColChange;
end;


procedure TRzDBMemo.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  LineColChange;
end;


procedure TRzDBMemo.KeyUp( var Key: Word; Shift: TShiftState );
begin
  inherited;
  LineColChange;
end;


procedure TRzDBMemo.KeyPress( var Key: Char );
begin
  if FTabOnEnter and ( Ord( Key ) = vk_Return ) then
  begin
    Key := #0;
    PostMessage( Handle, wm_KeyDown, vk_Tab, 0 );
  end
  else
    inherited;
end;


procedure TRzDBMemo.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;
  ClipboardChange;
end;


procedure TRzDBMemo.LineColChange;
begin
  if Assigned( FOnLineColChange ) then
    FOnLineColChange( Self, Line, Column );
  ClipboardChange;
end;


procedure TRzDBMemo.ClipboardChange;
begin
  if Assigned( FOnClipboardChange ) then
    FOnClipboardChange( Self, SelLength <> 0, Clipboard.HasFormat( cf_Text ) );
end;



function TRzDBMemo.DoMouseWheel( Shift: TShiftState; WheelDelta: Integer;
                                 MousePos: TPoint ): Boolean;
var
  I, Movement, Command: Integer;
begin
  Result := not Assigned( OnMouseWheel );

  if Result then
  begin
    if Mouse.WheelScrollLines = $FFFF then
      Movement := WheelDelta div WHEEL_DELTA
    else
      Movement := ( WheelDelta div WHEEL_DELTA ) * Mouse.WheelScrollLines;

    if Mouse.WheelScrollLines = $FFFF then
    begin
      if Movement > 0 then
        Command := SB_PAGEUP
      else
        Command := SB_PAGEDOWN;
    end
    else if Movement > 0 then
      Command := SB_LINEUP
    else
      Command := SB_LINEDOWN;

    for I := 1 to Abs( Movement ) do
    begin
      if HIWORD( SendMessage( Handle, EM_SCROLL, Command, 0 ) ) = 0 then
        Break;
    end;
  end
  else
    Result := inherited DoMouseWheel( Shift, WheelDelta, MousePos );
end; {= TRzDBMemo.DoMouseWheel =}


procedure TRzDBMemo.RepaintFrame;
begin
  if Parent is TDBCtrlPanel then
    Invalidate
  else
    InvalidateWindowFrame( Handle, ClientRect );
end;


function TRzDBMemo.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzDBMemo.WMNCPaint( var Msg: TWMNCPaint );
var
  DC: HDC;
begin
  inherited;                       { Must call inherited so scroll bar show up }

  if FFrameVisible and not UseThemes then
  begin
    DC := GetWindowDC( Handle );
    FCanvas.Handle := DC;
    try
      if FFrameHotTrack and ( Focused or FOverControl ) then
        DrawFrame( FCanvas, Width, Height, FFrameHotStyle, Color, FFrameHotColor, FFrameSides )
      else
        DrawFrame( FCanvas, Width, Height, FFrameStyle, Color, FFrameColor, FFrameSides );
    finally
      FCanvas.Handle := 0;
      ReleaseDC( Handle, DC );
    end;
    Msg.Result := 0;
  end;
end; {= TRzDBMemo.WMNCPaint =}


procedure TRzDBMemo.CMParentColorChanged( var Msg: TMessage );
begin
  inherited;

  if ParentColor then
  begin
    // If ParentColor set to True, must reset FNormalColor and FFocusColor
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;

  if FrameVisible then
    RepaintFrame;
end;


procedure TRzDBMemo.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  UpdateColors;
end;


procedure TRzDBMemo.WMPaint( var Msg: TWMPaint );
var
  S: string;
begin
  if csPaintCopy in ControlState then
  begin
    if Field <> nil then
    begin
      if Field.IsBlob then
      begin
        if AutoDisplay then
          S := AdjustLineBreaks( Field.AsString )
        else
          S := Format('(%s)', [ Field.DisplayLabel ] );
      end
      else
        S := Field.DisplayText;
    end;

    FPaintControl.SetBounds( BoundsRect.Left, BoundsRect.Top,
                             BoundsRect.Right - BoundsRect.Left,
                             BoundsRect.Bottom - BoundsRect.Top );
    if Field <> nil then
      FPaintControl.Alignment := Field.Alignment;

    SendTextMessage( FPaintControl.Handle, wm_SetText, 0, S );
    SendMessage( FPaintControl.Handle, wm_Paint, WParam( Msg.DC ), 0 );
  end
  else
  begin
    FPaintControl.SetBounds( 0, 0, 0, 0 );
    inherited;

    if FFrameVisible and not UseThemes and ( Parent is TDBCtrlPanel ) then
      DrawFrame( FCanvas, Width, Height, FFrameStyle, Color, FFrameColor, FFrameSides );
  end;
end; {= TRzDBMemo.WMPaint =}


procedure TRzDBMemo.UpdateColors;
begin
  if csLoading in ComponentState then
    Exit;

  FUpdatingColor := True;
  try
    if not Enabled then
      Color := FDisabledColor
    else if Focused then
    begin
      if ReadOnly and FReadOnlyColorOnFocus then
        Color := FReadOnlyColor
      else
        Color := FFocusColor;
    end
    else if ReadOnly then
      Color := FReadOnlyColor
    else
      Color := FNormalColor;
  finally
    FUpdatingColor := False;
  end;
end;


procedure TRzDBMemo.UpdateFrame( ViaMouse, InFocus: Boolean );
begin
  if ViaMouse then
    FOverControl := InFocus;

  if FFrameHotTrack then
    RepaintFrame;

  UpdateColors;
end;


procedure TRzDBMemo.CMEnter( var Msg: TCMEnter );
begin
  UpdateFrame( False, True );
  inherited;
end;

procedure TRzDBMemo.CMExit( var Msg: TCMExit );
begin
  inherited;
  UpdateFrame( False, False );
end;


procedure TRzDBMemo.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  UpdateFrame( True, True );
end;


procedure TRzDBMemo.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  UpdateFrame( True, False );
end;

procedure TRzDBMemo.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


{===========================}
{== TRzDBRichEdit Methods ==}
{===========================}

{$IFDEF VCL160_OR_HIGHER}
class constructor TRzDBRichEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook( TRzDBRichEdit, TRichEditStyleHook );
end;


class destructor TRzDBRichEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook( TRzDBRichEdit, TRichEditStyleHook );
end;
{$ENDIF}


constructor TRzDBRichEdit.Create( AOwner: TComponent );
begin
  inherited;

  ControlStyle := ControlStyle - [ csSetCaption ];

  FCanvas := TControlCanvas.Create;
  TControlCanvas( FCanvas ).Control := Self;

  FDisabledColor := clBtnFace;
  FReadOnlyColor := clInfoBk;
  FFocusColor := clWindow;
  FNormalColor := clWindow;
  FFrameColor := clBtnShadow;
  FFrameController := nil;
  FFrameControllerNotifications := fccAll;
  FFrameHotColor := clBtnShadow;
  FFrameHotTrack := False;
  FFrameHotStyle := fsFlatBold;
  FFrameSides := sdAllSides;
  FFrameStyle := fsFlat;
  FFrameVisible := False;
  FFramingPreference := fpXPThemes;
  FTabOnEnter := False;
end;

procedure TRzDBRichEdit.CreateWnd;
begin
  inherited;
  LineColChange;
  ClipboardChange;
  {&RCI}
end;


destructor TRzDBRichEdit.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  inherited;
end;


procedure TRzDBRichEdit.DestroyWnd;
begin
  // Data-Aware rich edit does not need to stream contents when recreating the window
  ControlState := ControlState - [ csRecreating ];
  inherited DestroyWnd;
end;


procedure TRzDBRichEdit.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzDBRichEdit.ReadOldFrameFlatProp( Reader: TReader );
begin
  FFrameHotTrack := Reader.ReadBoolean;
  if FFrameHotTrack then
  begin
    // If the FrameFlat property is stored, then init the FrameHotStyle property and the FrameStyle property.
    // These may be overridden when the rest of the stream is read in. However, we need to re-init them here
    // because the default values of fsStatus and fsLowered have changed in RC3.
    FFrameStyle := fsStatus;
    FFrameHotStyle := fsLowered;
  end;
end;


procedure TRzDBRichEdit.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzDBRichEdit.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzDBRichEdit.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


procedure TRzDBRichEdit.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  if not FUpdatingColor then
  begin
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


function TRzDBRichEdit.StoreColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpColor in FFrameControllerNotifications ) ) )
            and
            Enabled;
end;


function TRzDBRichEdit.StoreFocusColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpFocusColor in FFrameControllerNotifications ) ) )
            and
            ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzDBRichEdit.StoreDisabledColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpDisabledColor in FFrameControllerNotifications ) );
end;


function TRzDBRichEdit.StoreReadOnlyColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpReadOnlyColor in FFrameControllerNotifications ) );
end;


function TRzDBRichEdit.StoreReadOnlyColorOnFocus: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpReadOnlyColorOnFocus in FFrameControllerNotifications ) );
end;


function TRzDBRichEdit.StoreParentColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpParentColor in FFrameControllerNotifications ) );
end;


function TRzDBRichEdit.StoreFlatButtonColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtonColor in FFrameControllerNotifications ) );
end;


function TRzDBRichEdit.StoreFlatButtons: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtons in FFrameControllerNotifications ) );
end;


function TRzDBRichEdit.StoreFrameColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameColor in FFrameControllerNotifications ) );
end;


function TRzDBRichEdit.StoreFrameHotColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotColor in FFrameControllerNotifications ) );
end;


function TRzDBRichEdit.StoreFrameHotTrack: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotTrack in FFrameControllerNotifications ) );
end;


function TRzDBRichEdit.StoreFrameHotStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotStyle in FFrameControllerNotifications ) );
end;


function TRzDBRichEdit.StoreFrameSides: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameSides in FFrameControllerNotifications ) );
end;


function TRzDBRichEdit.StoreFrameStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameStyle in FFrameControllerNotifications ) );
end;


function TRzDBRichEdit.StoreFrameVisible: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameVisible in FFrameControllerNotifications ) );
end;


function TRzDBRichEdit.StoreFramingPreference: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFramingPreference in FFrameControllerNotifications ) );
end;


procedure TRzDBRichEdit.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzDBRichEdit.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzDBRichEdit.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBRichEdit.SetFrameController( Value: TRzFrameController );
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FFrameController := Value;
  if Value <> nil then
  begin
    Value.AddControl( Self );
    Value.FreeNotification( Self );
  end;
end;


procedure TRzDBRichEdit.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBRichEdit.SetFrameHotTrack( Value: Boolean );
begin
  if FFrameHotTrack <> Value then
  begin
    FFrameHotTrack := Value;
    if FFrameHotTrack then
    begin
      FrameVisible := True;
      if not ( csLoading in ComponentState ) then
        FFrameSides := sdAllSides;
    end;
    RepaintFrame;
    Invalidate;
  end;
end;


procedure TRzDBRichEdit.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBRichEdit.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBRichEdit.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBRichEdit.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    if FFrameVisible then
      Ctl3D := True;
    RecreateWnd;
  end;
end;


procedure TRzDBRichEdit.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      RepaintFrame;
  end;
end;


function TRzDBRichEdit.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;


procedure TRzDBRichEdit.SetReadOnly( Value: Boolean );
begin
  if ReadOnly <> Value then
  begin
    inherited ReadOnly := Value;
    UpdateColors;
  end;
end;


procedure TRzDBRichEdit.SetReadOnlyColor( Value: TColor );
begin
  FReadOnlyColor := Value;
  if ReadOnly then
    UpdateColors;
end;


procedure TRzDBRichEdit.JumpTo( ALine, ACol: Integer );
begin
  Line := ALine;
  if Line = ALine then
    Column := ACol;
end;


function TRzDBRichEdit.GetColumn: Integer;
var
  CaretPos: TPoint;
  P, LinePos, CharPos: Integer;
begin
  Windows.GetCaretPos( CaretPos );

  P := SendMessage( Handle, em_CharFromPos, 0, MakeLParam( Word( CaretPos.X ), Word( CaretPos.Y ) ) );
  if P <> -1 then
  begin
    CharPos := LoWord( P );
    LinePos := HiWord( P );
    Result := CharPos - SendMessage( Handle, em_LineIndex, LinePos, 0 ) + 1;
  end
  else
    Result := 1;
end;


procedure TRzDBRichEdit.SetColumn( Value: Integer );
var
  P, Len: Integer;
begin
  P := SendMessage( Handle, em_LineIndex, Line - 1, 0 );
  Len := SendMessage( Handle, em_LineLength, P, 0 );
  if Value <= Len + 1 then
  begin
    P := P + Value - 1;
    SendMessage( Handle, em_SetSel, P, P );
    LineColChange;
  end;
end;


function TRzDBRichEdit.GetLine: Integer;
var
  CaretPos: TPoint;
  P: Integer;
begin
  Windows.GetCaretPos( CaretPos );

  P := SendMessage( Handle, em_CharFromPos, 0, MakeLParam( Word( CaretPos.X ), Word( CaretPos.Y ) ) );
  if P <> -1 then
    Result := HiWord( P ) + 1
  else
    Result := 1;
end;


procedure TRzDBRichEdit.SetLine( Value: Integer );
var
  P, L: Integer;
begin
  P := SendMessage( Handle, em_LineIndex, Value - 1, 0 );

                       { Position Cursor to correct line number }
  SendMessage( Handle, em_SetSel, P, P );

  if Line = Value then
  begin
                            { Move selected line to top of window }
    L := SendMessage( Handle, em_GetFirstVisibleLine, 0, 0 );
    SendMessage( Handle, em_LineScroll, 0, Value - L - 1 );
    LineColChange;
  end;
end;


procedure TRzDBRichEdit.Change;
begin
  inherited;
  LineColChange;
end;


procedure TRzDBRichEdit.Click;
begin
  {&RV}
  inherited;
  LineColChange;
end;


procedure TRzDBRichEdit.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  LineColChange;
end;


procedure TRzDBRichEdit.KeyUp( var Key: Word; Shift: TShiftState );
begin
  inherited;
  LineColChange;
end;


procedure TRzDBRichEdit.KeyPress( var Key: Char );
begin
  if FTabOnEnter and ( Ord( Key ) = vk_Return ) then
  begin
    Key := #0;
    PostMessage( Handle, wm_KeyDown, vk_Tab, 0 );
  end
  else
    inherited;
end;


procedure TRzDBRichEdit.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;
  ClipboardChange;
end;


procedure TRzDBRichEdit.SelectionChange;
begin
  inherited;
  ClipboardChange;
end;


procedure TRzDBRichEdit.LineColChange;
begin
  if Assigned( FOnLineColChange ) then
    FOnLineColChange( Self, Line, Column );
  ClipboardChange;
end;


procedure TRzDBRichEdit.ClipboardChange;
begin
  if Assigned( FOnClipboardChange ) then
    FOnClipboardChange( Self, SelLength <> 0, Clipboard.HasFormat( cf_Text ) );
end;


function TRzDBRichEdit.DoMouseWheel( Shift: TShiftState; WheelDelta: Integer;
                                     MousePos: TPoint ): Boolean;
var
  I, Movement, Command: Integer;
begin
  Result := not Assigned( OnMouseWheel );

  if Result then
  begin
    if Mouse.WheelScrollLines = $FFFF then
      Movement := WheelDelta div WHEEL_DELTA
    else
      Movement := ( WheelDelta div WHEEL_DELTA ) * Mouse.WheelScrollLines;

    if Mouse.WheelScrollLines = $FFFF then
    begin
      if Movement > 0 then
        Command := SB_PAGEUP
      else
        Command := SB_PAGEDOWN;
    end
    else if Movement > 0 then
      Command := SB_LINEUP
    else
      Command := SB_LINEDOWN;

    for I := 1 to Abs( Movement ) do
    begin
      if HIWORD( SendMessage( Handle, EM_SCROLL, Command, 0 ) ) = 0 then
        Break;
    end;
  end
  else
    Result := inherited DoMouseWheel( Shift, WheelDelta, MousePos );
end; {= TRzDBRichEdit.DoMouseWheel =}


procedure TRzDBRichEdit.RepaintFrame;
begin
  InvalidateWindowFrame( Handle, ClientRect );
end;


function TRzDBRichEdit.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzDBRichEdit.CMEnabledChanged( var Msg: TMessage );
var
  Form: TCustomForm;
  L: Longint;
begin
  // Do NOT call inherited in this method.  The inherited version uses the EnableWindow API function to change
  // the state of the control.  Unfortunately, for the RichEdit this function does not honor color changes.  The
  // work-around is to set the ws_Disabled style for the control instead.

  if not Enabled and ( Parent <> nil ) then
  begin
    Form := GetParentForm( Self );
    if Form <> nil then
      Form.DefocusControl( Self, False );
  end;

  if HandleAllocated and not ( csDesigning in ComponentState ) then
  begin
    if Enabled then
      EnableWindow( Handle, Enabled )
    else
    begin
      L := GetWindowLong( Handle, gwl_Style );
      L := L or ws_Disabled;
      SetWindowLong( Handle, gwl_Style, L );
    end;
  end;

  UpdateColors;
end;


procedure TRzDBRichEdit.WMNCPaint( var Msg: TWMNCPaint );
var
  DC: HDC;
begin
  inherited;                       { Must call inherited so scroll bar show up }

  if FFrameVisible and not UseThemes then
  begin
    DC := GetWindowDC( Handle );
    FCanvas.Handle := DC;
    try
      if FFrameHotTrack and ( Focused or FOverControl ) then
        DrawFrame( FCanvas, Width, Height, FFrameHotStyle, Color, FFrameHotColor, FFrameSides )
      else
        DrawFrame( FCanvas, Width, Height, FFrameStyle, Color, FFrameColor, FFrameSides );
    finally
      FCanvas.Handle := 0;
      ReleaseDC( Handle, DC );
    end;
    Msg.Result := 0;
  end;
end; {= TRzDBRichEdit.WMNCPaint =}


procedure TRzDBRichEdit.CMParentColorChanged( var Msg: TMessage );
begin
  inherited;

  if ParentColor then
  begin
    // If ParentColor set to True, must reset FNormalColor and FFocusColor
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;

  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


procedure TRzDBRichEdit.UpdateColors;
begin
  if csLoading in ComponentState then
    Exit;

  FUpdatingColor := True;
  try
    if not Enabled then
      Color := FDisabledColor
    else if Focused then
    begin
      if ReadOnly and FReadOnlyColorOnFocus then
        Color := FReadOnlyColor
      else
        Color := FFocusColor;
    end
    else if ReadOnly then
      Color := FReadOnlyColor
    else
      Color := FNormalColor;
  finally
    FUpdatingColor := False;
  end;
end;


procedure TRzDBRichEdit.UpdateFrame( ViaMouse, InFocus: Boolean );
begin
  if ViaMouse then
    FOverControl := InFocus;

  if FFrameHotTrack then
    RepaintFrame;

  UpdateColors;
end;


procedure TRzDBRichEdit.CMEnter( var Msg: TCMEnter );
begin
  UpdateFrame( False, True );
  inherited;
end;

procedure TRzDBRichEdit.CMExit( var Msg: TCMExit );
begin
  inherited;
  UpdateFrame( False, False );
end;


procedure TRzDBRichEdit.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  UpdateFrame( True, True );
end;


procedure TRzDBRichEdit.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  UpdateFrame( True, False );
end;


procedure TRzDBRichEdit.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


function TRzDBRichEdit.GetRtfData: string;
var
  memStream: TMemoryStream;
  strList: TStringList;
begin
  Result := '';

  memStream := TMemoryStream.Create;
  strList := TStringList.Create;
  try
    Lines.SaveToStream( memStream );
    memStream.Position := 0;
    strList.LoadFromStream( memStream );
    Result := strList.Text;
  finally
    memStream.Free;
    strList.Free;
  end;
end;


{&RUIF}

end.




