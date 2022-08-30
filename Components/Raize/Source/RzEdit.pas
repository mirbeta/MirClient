{===============================================================================
  RzEdit Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzCustomEdit
    Base class for all Raize edit-style components--supports Custom Framing and
    true text alignment.

  TRzEdit
    Published version of TRzCustomEdit.

  TRzNumericEdit
    Edit control for entering numeric data (integers and floating-point numbers).

  TRzMaskEdit
    TMaskEdit descendant--adds Custom Framing support.

  TRzExpandEdit
    Edit field can expand to a larger size on enter, or via mouse click.

  TRzColorEdit
    Edit field that allows user to pick a color from a popup TRzColorPicker.

  TRzDateTimeEdit
    Edit field for entering dates and times.  Allows user to pick date from
    popup TRzCalendar.

  TRzMemo
    Enhanced TMemo--supports Custom Framing, Line & Column properties and events.

  TRzRichEdit
    Enhanced TRichEdit--supports Custom Framing, Line & Column properties and
    events.

  TRzHotKeyEdit
    Enhanced THotKey--supports Custom Framing.

  Modification History
  ------------------------------------------------------------------------------
  6.2.1  (01 Sep 2015)
    * Fixed issue in TRzColorEdit, TRzDateTimeEdit, and TRzNumericEdit where
      the drop down arrow would not display new Windows 10 style if FlatButtons
      was set to True and the program was running under Windows 10.
  ------------------------------------------------------------------------------
  6.1.11 (08 Apr 2015)
    * Fixed issue in TRzEdit and descendants where registered style hook was not 
      correctly unregistered when the hook was no longer needed.
    * Added UseFormatToParse property to TRzDateTimeEdit. Set this property to
      True in situations where the Time to be parsed contains more elements
      than are contained in the LongTimeFormat string. For example, if the Time
      needed to be entered must have 'hh:nn:ss.zzz' and the LongTimeFormat string
      is set to 'hh:nn', then set the UseFormatToParse to True and set the
      Format property to the desired format string.
  ------------------------------------------------------------------------------
  6.1.9  (21 Jun 2014)
    * Fixed issue in TRzDateTimeEdit where special keys would not change the
      current date if the MinDate and MaxDate were both set to zero (the
      default value).
    * Removed empty try..except block from TRzDateTimeEdit.DropDown method.
  ------------------------------------------------------------------------------
  6.1.6  (15 Feb 2014)
    * Fixed issue in TRzDateTimeEdit where MinDate and MaxDate values were not
      honored when special keys were used to change the date.
  ------------------------------------------------------------------------------
  6.1.3  (01 May 2013)
    * Fixed issue where TRzDateTimeEdit Date property was always streamed out
      to form file as a result of changes to the VCL streaming criteria.
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * The TRzDateTimeEdit now supports setting the Text property to indirectly
      set the Date or Time properties.
    * Fixed issue in TRzNumericEdit that resulted in the numeric value being
      converted to zero when the DisplayFormat string was set to contain quoted
      text *and* in that quoted text the DecimalSeparator symbol was present.
    * Added AsDateTime runtime property to TRzDateTimeEdit to aid in type
      conversions between TDate and TTime values and TDateTime.
    * PopupWidth and PopupHeight properties for TRzColorEdit and TRzDateTimeEdit
      are no longer persisted into the DFM file when their values are zero.
    * Added PopupAlignment property to TRzDateTimeEdit, TRzColorEdit, and
      TRzNumericEdit, which specifies which side of the control the popup is
      aligned.
    * Added PopupWidth and PopupHeight properties to TRzNumericEdit, which
      control the size of the calculator if displayed.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Fixed issue where the TextHint property would not display correctly in
      TRzEdit (and descendants) if the FrameVisible property was set to True.
    * Added new TwoDigitYearConverter property to TRzDateTimeEdit, which
      controls how 2-digit years are converted to 4-digit years. The default
      value is ycStandard, which means that 2-digit years are converted using
      the TwoDigitYearCenturyWindow value.  The ycPastNotFuture value means that
      a date with a 2-digit year is interpreted as a date in the past rather
      than the future.
      For example, consider entering '3/15/20' (in the U.S.). With the
      ycStandard converter value, the date gets converted to '3/15/2020'. But
      with the ycPastNotFuture converter, the converted date is '3/15/1920'.
      Entry of 4-digit years are that unaffected by this property.
    * Added PopupHeight and PopupWidth properties to TRzDateTimeEdit and
      TRzColorEdit. When both of these properties are set to non-zero values,
      the popup control (i.e. Calendar, TimePicker, or ColorPicker) is sized to
      the PopupHeight and PopupWidth values instead of the default auto sized
      values.
    * Made necessary modifications to TRzEdit, TRzNumericEdit, TRzMaskEdit,
      TRzExpandEdit, TRzColorEdit, TRzDateTimeEdit, TRzMemo, and TRzRichEdit to
      fully support VCL Styles introduced in RAD Studio XE2.
    * Made necessary modifications to TRzMemo to support 64-bit development.
  ------------------------------------------------------------------------------
  5.5    (06 Mar 2011)
    * Added PopupButtonColor and PopupButtonFontColor properties to the
      TRzColorEdit control.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Fixed issue in TRzNumericEdit where pressing the keypad decimal key would
      not insert the DecimalSeparator character (based on user locale settings)
      if the DecimalSeparator was something other than a period and the
      calculator was dropped-down.
    * Fixed issue in TRzNumericEdit where setting the Value property to a
      decimal value at design-time would truncate the decimal portion when the
      form was loaded if DisplayFormat used a decimal based format such as
      ',0.0;(,0.0)'.
    * Updated the display of the drop-down button for TRzDateTimeEdit,
      TRzNumericEdit, and TRzColorEdit when running under Windows Vista and
      Windows 7.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Changed the TRzNumericEdit.IntValue property to be of type Int64.
    * Fixed issue where TRzDateTimeEdit and TRzNumericEdit would trap the
      Alt+F4 key combination and prevent the application from closing.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * Fixed issue where assigning a negative value to a TRzNumericEdit with a
      non default DisplayFormat *and* AllowScientificNotation set to False would
      result in the Value of the control being set to zero.
    * Redefined how the TRzNumericEdit determines if a number is negative.
    * Fixed issue where OnDateTimeChange event would get raised twice when the
      user would press a key to change the date or time.
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzEdit, TRzNumericEdit, TRzMaskEdit, TRzExpandEdit, TRzColorEdit, and
      TRzDateTimeEdit controls.
  ------------------------------------------------------------------------------
  5.1.2  (11 Jun 2009)
    * Added new AllowScientificNotation property to TRzNumericEdit. When set to
      True (the default), the control allows entering "E" and "e" to specify a
      number in scientific notation. Setting this property to False, prevents
      the control from accepting "E" and "e" during data-entry and parsing.
    * The TRzNumericEdit has also been enhanced to handle presses of the Del key
      on the numeric keypad by inserting the DecimalSeparator into the edit
      area. This change allows the numeric keypad to be used to enter floating
      point numbers even if the user locale's numeric settings uses a symbol
      other than a period for the decimal separator.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Updated the parsing code used by TRzDateTimeEdit so that an invalid month
      value results in the OnInvalidDate event being fired instead of simply
      resetting the month value to 1.
    * Updated parsing code used by TRzDateTimeEdit so that invalid minute values
      extract the first two digits to represent the minutes and not just the
      first digit. This results in a more accurate resolution of time values.
    * Added new OnInvalidTime event to the TRzDateTimeEdit, which fires when the
      entered time string contains invalid values for one of the time portions.
      The NewTime parameter to the event handler contains the auto-corrected
      Time value that will be used if the value is not modified in the event
      handler.
    * Fixed problem of the TextHint property not getting displayed when running
      under Vista.
    * Surfaced FlatButtons and FlatButtonColor properties in TRzNumericEdit.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added new TextHint and TextHintVisibleOnFocus properties to TRzEdit and
      descendants (e.g. TRzNumericEdit, TRzDateTimeEdit). The TextHint property
      allows a developer to specify a prompt that appears inside the edit area
      of the control, when the control is empty. The text prompt appears grayed.
      By default, the prompt is automatically hidden when the control receives
      focus. To keep the prompt visible until the user enters some text, set the
      TextHintVisibleOnFocus property to True.
      NOTES:
        - TextHint is only applicable under WinXP or Vista, and only when
          XP/Vista themes are in use.
        - The TextHintVisibleOnFocus is only applicable under Vista.
        - RAD Studio 2009 supports TextHint for TEdit, but not the
          TextHintVisibleOnFocus property. The TRzEdit and descendants utilize
          the inherited TextHint property where possible, and add the
          TextHintVisibleOnFocus property where necessary.
    * The TRzNumericEdit control now has a CalculatorVisible property. When this
      property is set to True, a drop down button becomes visible, and when
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
    * Fixed problem in TRzDateTimeEdit, TRzColorEdit, and TRzNumericEdit where
      changing the DropButtonVisible property to False, and then toggling the
      ReadOnly property would cause the drop button to reappear.
    * The drop down calendar and time picker displayed by the TRzDateTimeEdit
      control now uses the same font size as the edit control itself. Therefore,
      if a larger font is used for the edit field text, the calendar and time
      picker will also be displayed using the larger font.
    * The TRzExpandEdit now expands to the left on right-to-left systems.
  ------------------------------------------------------------------------------
    * Changed the way popups for TRzDateTimeEdit and TRzColorEdit are parented
      when displayed.
    * Fixed issue in TRzNumericEdit where using a DisplayFormat that used
      scientific notation with a lower case 'e' would not get parsed correctly.
  ------------------------------------------------------------------------------
  4.3.2  (04 Nov 2007)
    * Reverted drawing code for TRzDateTimeEdit and TRzColorEdit for drawing the
      drop down buttons when Vista/XP themes are in use.
    * A blank TRzDateTimeEdit no longer automatically sets its Date property to
      the current date when the calendar is dropped down.
  ------------------------------------------------------------------------------
  4.3    (13 Sep 2007)
    * TRzDateTimeEdit and TRzColorEdit now handle the F4 key to drop down their
      respective panels.
    * Updated TRzEdit.Alignment property to allow the taCenter setting.
    * Fixed issue where the dropdown button in TRzDateTimeEdit and TRzColorEdit
      would get drawn themed even if the FramingPreference was set to
      fpCustomFraming.
    * Fixed issue where buttons in the dropped down Calendar and TimePicker
      (in TRzDateTimeEdit) would get drawn themed even if the FramingPreference
      for the edit was set to fpCustomFraming.
  ------------------------------------------------------------------------------
  4.2.1  (07 Jun 2007)
    * Fixed problem in TRzDateTimeEdit where certain time values were being
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
    * Fixed problem where TRzNumericEdit would accept alphabetic characters when
      IntegersOnly was set to False.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Added new MinDate and MaxDate properties to TRzDateTimeEdit to allow for
      specifying a range of valid dates that can be selected by the user.
    * Added the new OnGetDayFormat event to the TRzDateTimeEdit. This event
      allows the individual days of the calendar display to be colored and
      styled differently than the calendar's default formatting.  This new event
      should be used in place of the OnGetBoldDays event.  The OnGetBoldDays
      event is still available, but the new OnGetDayFormat event is much more
      flexible and powerful.
    * Adjusted the drawing of Days of the Week header in the drop down calendar
      of the TRzDateTimeEdit. In the new approach, the headers are either all
      drawn showing their short name (based on locale), or they are all drawn
      showing the first two characters of their short name. Also, the font size
      is reduced slightly for the day of week short names.
    * Fixed problem where bold-days bitmasks in TRzDateTimeEdit were not
      correctly applied to the previous and next months in a the current view.
    * Fixed issue where changing the TRzRichEdit.Line property would not cause
      the view to scroll to the new line position.
    * Fixed problem in TRzColorEdit, TRzDateTimeEdit, and descendants that
      resulted in the drop down arrow button looking distorted on Windows 98
      systems.
    * Added BeepOnInvalidKey property to TRzNumericEdit and TRzDateTimeEdit.
    * Updated TRzNumericEdit to allow free form floating point numbers including
      scientific notation. Free form entry is only allowed when the
      IntegersOnly property is set to False.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Surface OnMouseWheel event in TRzMemo and TRzRichEdit.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * Added ReadOnlyColorOnFocus property to TRzEdit and descendants, and
      TRzMemo, and TRzRichEdit. This property determines if the ReadOnlyColor
      or FocusColor is used to color the control when the control has the focus
      and has its ReadOnly property set to True. By default, a focused control
      will use the FocusColor even if the control is read-only. Setting
      ReadOnlyColorOnFocus to True will cause a focused read-only control to
      be colored using ReadOnlyColor.
  ------------------------------------------------------------------------------
  4.0.1  (07 Jan 2006)
    * Changed the preference order between FocusColor and ReadOnlyColor
      properties in TRzEdit, TRzMemo, and TRzRichEdit. In the previous ordering,
      a ReadOnly control would be displayed in ReadOnlyColor even when the
      control had the focus. Now, a ReadOnly control that has the input focus is
      displayed in FocusColor.
    * When the Custom color item is selected in the TRzColorEdit color picker,
      the initial color displayed in resulting the Color Dialog is the currently
      SelectedColor instead of the previous custom color value.  This change
      makes it much easier for users to adjust selected colors.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed problem where clicking the Clear button in the Calendar when used
      by a TRzDateTimeEdit would cause 12/30/1899 to appear in the edit area.
    * Added GetRtfData method to TRzRichEdit, which returns a string containing
      the RTF encoded data for the rich edit control.
    * Fixed problem in custom edits (e.g. TRzDateTimeEdit, TRzColorEdit) where
      drop-down button would be drawn with a disabled background when the
      control's Enabled property was toggled from False to True in certain
      circumstances.
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzCustomEdit, TRzMemo,
      TRzRichEdit, and TRzHotKeyEdit to account for changes introduced in
      Borland Developer Studio 2006.
    * Fixed custom framing display problem that would occur in TRzEdit and
      descendants when FrameVisible was set to True and changes were made to
      control's appearance within calls to LockWindowUpdate.
    * Added UseRightToLeftLayout to TRzCustomEdit. This method is used by 
      descendant classes to correctly determine appropriate layouts (including
      embedded button positions) when running under RTL systems.
    * Added ReadOnlyColor property to TRzEdit and descendant classes. The new
      property has also been added to TRzMemo and TRzRichEdit.  This color
      property is used to change the color of the control when the ReadOnly
      property is set to True.
    * For edit controls that display a drop down button (e.g. TRzColorEdit,
      TRzDateTimeEdit), when the ReadOnly property is set to True, the drop down
      button is hidden.
    * Added new FrameControllerNotifications property to TRzEdit and all edit
      descendant classes (e.g. TRzDateTimeEdit). In addition, this property has
      been added to the TRzMemo, TRzRichEdit, and TRzHotKeyEdit classes.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Fixed problem where TRzMemo and TRzRichEdit would not honor the user's
      mouse wheel lines settings when scrolling using the mouse wheel.
    * Added new PopupButtonColor and PopupButtonFontColor properties to
      TRzDateTimeEdit control, which provide access to the popup TRzCalendar
      or TRzTimerPicker ButtonColor and ButtonFontColor properties.
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Fixed problem in TRzDateTimeEdit where setting the Date property to a
      TDateTime value of 0 would clear the edit area instead of setting the date
      to 12/30/1899.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem where drop down button of TRzDateTimeEdit and TRzColorEdit
      was not drawn using DisabledColor when the control was disabled and
      FlatButtons was set to True.
    * Fixed problem in TRzNumericEdit where initial value would not be formatted
      correctly if DisplayFormat was not the default *and* AllowBlank was set
      to True.
    * Addd OnViewDateChange event to TRzDateTimeEdit, which fires when the
      month (i.e. view) of the drop-down calendar is changed by the user.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Fixed problem in TRzDateTimeEdit where using Minus key or Down Arrow key
      to change time value would result in an incorrect time value if the change
      would cause the time to be earlier than midnight (i.e. 12:00 am).
    * Fixed problem where jumping to a line in a TRzMemo that was out of view
      would not cause the memo to scroll so that the cursor position was
      visible.
    * Enhanced TRzDateTimeEdit to allow entry of unformatted 8-digit date
      strings and correctly convert it to an actual date.
    * Fixed problem where changing ParentColor to True in a control using Custom
      Framing did not reset internal color fields used to manage the color of
      the control at various states.
    * Fixed problem with Line and Column properties of TRzRichEdit. The same
      technique used by the TRzMemo causes an access violation when applied to
      a rich edit control. 
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Added OnGetWeekNumber event to TRzDateTimeEdit. Handle this event to
      implement a customized week numbering scheme.
    * Added OnRangeError event to TRzNumericEdit. This event is generated when
      the user leaves the field and the value entered by the user exceeds the
      bounds defined by the Min and Max properties.
    * Fixed problem where Line property in TRzMemo and TRzRichEdit would not
      return the correct value when text was selected.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Fixed problem where user could not enter a positive number into a
      TRzNumericEdit if the first character in the selection was a '-' sign.
    * Fixed problem where csCaptureMouse ControlStyle was causing problems if
      the edit was not allowed to get the focus via some user defined OnExit
      event handler for some other control.
    * Fixed problem where TRzRichEdit would not honor DisabledColor value when
      disabled.
    * Moved FFrameVisible instance field to protected section for access in
      TRzDBEdit.
    * Fixed problem where user could still display the calendar or time picker
      when the TRzDateTimeEdit.ReadOnly property was set to True.
    * TRzDateTimeEdit no longer generates the OnExit event when the calendar or
      time picker are displayed.
    * Added OnDateTimeChange to TRzDateTimeEdit. This event fires whenever the
      date/time value in the edit field changes.
    * Fixed problem where selected text at beginning of a TRzNumericEdit that
      contained a '+' or '-' sign could not be replaced by pressing the '+' or
      '-' keys.
    * Fixed problem where OnChange event of TRzNumericEdit would fire during
      program load under special circumstannce--e.g. current regional settings
      are different than those used by developer.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * Fixed problem where using the TimePicker to set the time in a
      TRzDateTimeEdit's Time to 12:00 AM did not enter 12:00 AM into the edit
      field.
    * Fixed problem where a digit could be entered to the left of a sign symbol
      in TRzNumericEdit.
    * Modified TRzCustomEdit.KeyDown to prevent the vk_Down key from being
      further processed by descendant components after the DoDropDown virtual
      method has been invoked. This change fixed the problem where using
      Alt+Down to display the TimePicker for a TRzDateTimeEdit and setting the
      desired time pressing Enter would not always set the time correctly.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * Fixed problem with enter 12:00 am into TRzDateTimeEdit and leaving the
      field changed the time to 12:00 PM.
    * Clear method now correctly clears the TRzDateTimeEdit and it internal
      datetime value.
    * Modified TRzDBNumericEdit.EvaluteText so that string conversion is avoided
      if text just contains a leading minus sign or open paren indicating a
      negative number. This change prevents the beep from sounded if the text
      value cannot be converted.
    * Fixed problem where leaving a TRzDateTimeEdit (with EditType=etTime) after
      deleting selected time value caused 12:00 AM (or appropriately formatted
      time) to appear in control.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Fixed problem where format of TRzDateTimeEdit would change when control
      received the focus.
    * Fixed problem where focus would leave TRzDateTimeEdit if accelerator
      pressed and the key was a valid date or time value key (e.g. 1, 2, 3...).
    * Added OnCloseUp event to go with OnDropDown event.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Fixed problem where clicking on TRzColorEdit in Delphi 5 caused the IDE to
      place control into "move" mode.
    * Added StoreColor and StoreFocusColor methods so that if control is
      disabled at design-time the Color and FocusColor properties are not
      streamed with the disabled color value.
    * Added additional hot keys to TRzDateTimeEdit for changing Day, Month,
      Year, and Hour, Minute.
    * Fixed problem where OnKeyDown event was not getting generated.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    << TRzCustomEdit and TRzEdit >>
    * Simplified the TRzCustomEdit.CreateParams method. In previous versions, we
      had a check for Win95 and NT 4, and under these conditions, the
      es_MultiLine style was used to support right justification. However, this
      style introduced many other side-effects. Fortunately, it is no longer
      necessary to include this style to get right-justification and therefore
      it has been removed.
    * Add FocusColor and DisabledColor properties.
    * Renamed FrameFlat property to FrameHotTrack.
    * Renamed FrameFocusStyle property to FrameHotStyle.
    * Removed FrameFlatStyle property.
    * Fixed problem where the MaskEdit property editor was being used for
      editing the Text property of the TRzEdit and descendants.

    << TRzMaskEdit >>
    * Added an OnValidateError event.  This event fires when a validation error
      occurs in edit mask. This event allows a developer to specify a custom
      error message instead of the default exception message defined in the VCL.

    << TRzNumericEdit >>
    * Added AllowBlank and BlankValue properties. Setting AllowBlank to True
      prevents the control from automatically ensuring that the value is always
      a non-blank valid numeric value. This property is most useful when
      connecting the data-aware version to a field that can have NULL values.

    * TRzColorEdit component added.
    * TRzDateTimeEdit component added.
    * TRzHotKeyEdit component added.
===============================================================================}

{$I RzComps.inc}

unit RzEdit;

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
  Buttons,
  ComCtrls,
  Forms,
  Dialogs,
  RzCommon,
  StdCtrls,
  Menus,
  Mask,
  RzPopups;

type
  TRzValidateErrorEvent = procedure ( Sender: TObject; var ErrorMsg: string ) of object;

  {=====================================}
  {== TRzCustomEdit Class Declaration ==}
  {=====================================}

  TRzCustomEdit = class( TCustomMaskEdit )
  {$IFDEF VCL160_OR_HIGHER}
  strict private
    class constructor Create;
    class destructor Destroy;
  {$ENDIF}
  private
    FAlignment: TAlignment;
    FTabOnEnter: Boolean;
    FBeepOnInvalidKey: Boolean;
    FUpdatingColor: Boolean;
    FDisabledColor: TColor;
    FFocusColor: TColor;
    FNormalColor: TColor;
    FFrameColor: TColor;
    FFrameHotColor: TColor;
    FFrameController: TRzFrameController;
    FFrameControllerNotifications: TRzFrameControllerNotifications;
    FFrameHotTrack: Boolean;
    FFrameHotStyle: TFrameStyle;
    FFrameSides: TSides;
    FFrameStyle: TFrameStyle;
    FFramingPreference: TFramingPreference;
    FReadOnlyColor: TColor;
    FReadOnlyColorOnFocus: Boolean;
    FTextHintVisibleOnFocus: Boolean;

    FOnValidateError: TRzValidateErrorEvent;

    // Combo-Edit functionality
    FDropButtonVisible: Boolean;
    FShowDropButton: Boolean;
    FMouseOverButton: Boolean;
    FFlatButtons: Boolean;
    FFlatButtonColor: TColor;
    FDroppedDown: Boolean;
    FButtonState: TButtonState;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;

    procedure ReadOldFrameFlatProp( Reader: TReader );
    procedure ReadOldFrameFocusStyleProp( Reader: TReader );

    // Property Access Methods
    procedure SetTextHintVisibleOnFocus( Value: Boolean );

    // Message Handling Methods
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
//!! add    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure WMNCPaint( var Msg: TWMNCPaint ); message wm_NCPaint;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
    procedure WMNCHitTest( var Msg: TMessage ); message wm_NCHitTest;
    procedure WMNCCalcSize( var Msg: TWMNCCalcSize ); message wm_NCCalcSize;
    procedure WMKillFocus( var Msg: TMessage ); message wm_KillFocus;
    procedure WMLButtonDown( var Msg: TMessage ); message wm_LButtonDown;
    procedure WMLButtonUp( var Msg: TMessage ); message wm_LButtonUp;
    procedure WMLButtonDblClk( var Msg: TMessage ); message wm_LButtonDblClk;
    procedure WMRButtonDown( var Msg: TMessage ); message wm_RButtonDown;
    procedure WMSetCursor( var Msg: TWMSetCursor ); message wm_SetCursor;
  protected
    FAboutInfo: TRzAboutInfo;
    FCanvas: TCanvas;
    FInControl: Boolean;
    FOverControl: Boolean;
    FFrameVisible: Boolean;

    procedure CreateParams( var Params: TCreateParams ); override;
    procedure CreateWnd; override;
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure DoSetTextHint( const Value: string ); override;
    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;
    procedure RepaintFrame; virtual;

    function GetEditRect: TRect; virtual;
//!!add    procedure SetEditRect; virtual;
    function GetRightJustifiedText: string; virtual;

    procedure ValidateError; override;

    procedure HideButton; virtual;
    procedure ShowButton; virtual;

    procedure RepaintButton;
    procedure DrawButton( Canvas: TCanvas; var R: TRect ); virtual;
    procedure MouseCancel;
    procedure DoDropDown;
    procedure InvalidKeyPressed;

    // Event Dispatch Methods
    procedure KeyPress( var Key: Char ); override;
    procedure CloseUp; dynamic;
    procedure DropDown; dynamic;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;

    // Property Access Methods
    procedure SetAlignment( Value: TAlignment ); virtual;
    procedure SetButtonState( Value: TButtonState ); virtual;
    procedure SetDropButtonVisible( Value: Boolean ); virtual;
    procedure SetShowDropButton( Value: Boolean ); virtual;
    procedure SetFlatButtons( Value: Boolean ); virtual;
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

    function ReadOnlyValue: Boolean; virtual;
    procedure ReadOnlyChanged; virtual;
    function GetReadOnly: Boolean;
    procedure SetReadOnly( Value: Boolean );
    procedure SetReadOnlyColor( Value: TColor ); virtual;

    // Property Declarations
    property Canvas: TCanvas
      read FCanvas;

    property Alignment: TAlignment
      read FAlignment
      write SetAlignment
      default taLeftJustify;

    property BeepOnInvalidKey: Boolean
      read FBeepOnInvalidKey
      write FBeepOnInvalidKey
      default True;

    property ButtonState: TButtonState
      read FButtonState
      write SetButtonState;

    property DropButtonVisible: Boolean
      read FDropButtonVisible
      write SetDropButtonVisible
      default False;

    // ShowDropButton is used internally to manage the display of the
    // drop button between user preferences (DropButtonVisible property) and
    // ReadOnly property changes (i.e. Button is hidden when ReadOnly is True).
    property ShowDropButton: Boolean
      read FShowDropButton
      write SetShowDropButton
      default False;

    property Color
      stored StoreColor
      default clWindow;

    property FlatButtonColor: TColor
      read FFlatButtonColor
      write FFlatButtonColor
      stored StoreFlatButtonColor
      default clBtnFace;

    property FlatButtons: Boolean
      read FFlatButtons
      write SetFlatButtons
      stored StoreFlatButtons
      default False;

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

    property TextHintVisibleOnFocus: Boolean
      read FTextHintVisibleOnFocus
      write SetTextHintVisibleOnFocus
      default False;

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

    property OnCloseUp: TNotifyEvent
      read FOnCloseUp
      write FOnCloseUp;

    property OnDropDown: TNotifyEvent
      read FOnDropDown
      write FOnDropDown;

    property OnValidateError: TRzValidateErrorEvent
      read FOnValidateError
      write FOnValidateError;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UseThemes: Boolean; virtual;
    function GetControlsAlignment: TAlignment; override;
    function UseRightToLeftLayout: Boolean;
  end;

  {$IFDEF VCL160_OR_HIGHER}
  TRzEditStyleHook = class( TEditStyleHook )
  strict private
    procedure WMNCCalcSize( var Msg: TWMNCCalcSize ); message wm_NCCalcSize;
  strict protected
    procedure PaintNC( Canvas: TCanvas ); override;
  public
    constructor Create( AControl: TWinControl ); override;
  end;
  {$ENDIF}


  {===============================}
  {== TRzEdit Class Declaration ==}
  {===============================}

  TRzEdit = class( TRzCustomEdit )
  {$IFDEF VCL160_OR_HIGHER}
  strict private
    class constructor Create;
    class destructor Destroy;
  {$ENDIF}
  protected
    function GetText: TCaption;
    procedure SetText( const Value: TCaption );
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    // Redefine the Text property to change the type of the inherited property (in TCustomMaskEdit) of TMaskedText
    // back to TCaption. This is so that the regular Caption editor is used for editing the TRzEdit.Text property.
    property Text: TCaption
      read GetText
      write SetText;

    // Inherited Properties & Events
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
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
    property ReadOnly;
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


  {======================================}
  {== TRzNumericEdit Class Declaration ==}
  {======================================}

  TRzRangeErrorEvent = procedure( Sender: TObject; EnteredValue, AdjustedValue: Extended;
                                  var AutoCorrect: Boolean ) of object;

  TRzNumericEdit = class( TRzEdit )
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
    procedure WMPaste( var Msg: TWMPaste ); message wm_Paste;
    procedure WMCut( var Msg: TWMCut ); message wm_Cut;
  protected
    FLoading: Boolean;
    procedure Loaded; override;
    procedure CreateParams( var Params: TCreateParams ); override;

    function IsValidChar( Key: Char ): Boolean; virtual;
    function FormatText( const Value: Extended ): string; virtual;

    function CleanupQuotedText( const S: string ): string;
    function EvaluateText: Extended; virtual;

    procedure DisplayCalculator; virtual;


    // Event Dispatch Methods
    procedure DropDown; override;
    procedure Change; override;
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
      default False;

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

    // Inherited Properties & Events
    property Alignment default taRightJustify;
    property BeepOnInvalidKey;
    property FlatButtonColor;
    property FlatButtons;
    property Text stored False;
    property TextHint;
    property TextHintVisibleOnFocus;
  end;

  {===================================}
  {== TRzMaskEdit Class Declaration ==}
  {===================================}

  TRzMaskEdit = class( TRzCustomEdit )
  protected
    function GetRightJustifiedText: string; override;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    // Inherited Properties & Events
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
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
    property EditMask;
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
    property ParentCtl3D;
    property ParentDoubleBuffered;
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
    property Text;
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
    property OnValidateError;
  end;

  {=====================================}
  {== TRzExpandEdit Class Declaration ==}
  {=====================================}

  TRzExpandEdit = class( TRzEdit )
  private
    FExpandedWidth: Integer;
    FExpanded: Boolean;
    FOrigWidth: Integer;
    FExpandOn: TExpandOnType;

    // Message Handling Methods
    procedure WMSetFocus( var Msg: TWMSetFocus  ); message wm_SetFocus;
    procedure WMKillFocus( var Msg: TWMKillFocus ); message wm_KillFocus;
    procedure WMRButtonUp( var Msg: TWMRButtonUp ); message wm_RButtonUp;
  protected
    // Property Access Methods
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


  {====================================}
  {== TRzColorEdit Class Declaration ==}
  {====================================}

  TRzColorEdit = class( TRzCustomEdit )
  {$IFDEF VCL160_OR_HIGHER}
  strict private
    class constructor Create;
    class destructor Destroy;
  {$ENDIF}
  private
    FCustomColor: TColor;
    FDefaultColor: TColor;
    FColorDlgOptions: TColorDialogOptions;
    FCustomColors: TRzCustomColors;
    FCustomColorCaption: string;
    FNoColorCaption: string;
    FDefaultColorCaption: string;

    FShowCustomColor: Boolean;
    FShowNoColor: Boolean;
    FShowDefaultColor: Boolean;
    FShowSystemColors: Boolean;
    FSelectedColor: TColor;
    FShowColorHints: Boolean;

    FPopupAlignment: TAlignment;
    FPopupButtonColor: TColor;
    FPopupButtonFontColor: TColor;
    FPopupHeight: Integer;
    FPopupWidth: Integer;


    // Message Handling Methods
    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
    procedure WMSetFocus( var Msg: TMessage ); message wm_SetFocus;
    procedure WMNCHitTest( var Msg: TMessage ); message wm_NCHitTest;
    procedure WMKillFocus( var Msg: TMessage ); message wm_KillFocus;
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure DisplayColorPicker; virtual;

    // Event Dispatch Methods
    procedure DropDown; override;

    // Property Access Methods
    procedure SetCustomColors( Value: TRzCustomColors ); virtual;
    procedure SetSelectedColor( Value: TColor ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;

    procedure WndProc( var Msg: TMessage ); override;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property CustomColorCaption: string
      read FCustomColorCaption
      write FCustomColorCaption;

    property CustomColor: TColor
      read FCustomColor
      write FCustomColor
      default clWhite;

    property CustomColors: TRzCustomColors
      read FCustomColors
      write SetCustomColors;

    property ColorDlgOptions: TColorDialogOptions
      read FColorDlgOptions
      write FColorDlgOptions
      default [ cdFullOpen ];

    property DefaultColor: TColor
      read FDefaultColor
      write FDefaultColor
      default clHighlight;

    property DefaultColorCaption: string
      read FDefaultColorCaption
      write FDefaultColorCaption;

    property ShowColorHints: Boolean
      read FShowColorHints
      write FShowColorHints
      default True;

    property NoColorCaption: string
      read FNoColorCaption
      write FNoColorCaption;

    property PopupAlignment: TAlignment
      read FPopupAlignment
      write FPopupAlignment
      default taRightJustify;

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

    property SelectedColor: TColor
      read FSelectedColor
      write SetSelectedColor
      default clNone;

    property ShowNoColor: Boolean
      read FShowNoColor
      write FShowNoColor
      default False;

    property ShowCustomColor: Boolean
      read FShowCustomColor
      write FShowCustomColor
      default False;

    property ShowDefaultColor: Boolean
      read FShowDefaultColor
      write FShowDefaultColor
      default False;

    property ShowSystemColors: Boolean
      read FShowSystemColors
      write FShowSystemColors
      default False;

    // Inherited Properties & Events
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DisabledColor;
    property DoubleBuffered;
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
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property ShowHint;
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
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


  {=======================================}
  {== TRzDateTimeEdit Class Declaration ==}
  {=======================================}

  TRzDTEditType  = ( etDate, etTime );

  TRzDateTimeChangeEvent = procedure( Sender: TObject; DateTime: TDateTime ) of object;

  TRzDateTimeEdit = class( TRzCustomEdit )
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

    // Message Handling Methods
    procedure WMGetDlgCode( var Msg: TWMGetDlgCode ); message wm_GetDlgCode;
  protected
    procedure SetDateTime;
    procedure UpdateText;
    function DateInRange( Value: TDateTime ): Integer;
    procedure SetRange( MinValue, MaxValue: TDate );

    procedure DisplayCalendar; virtual;
    procedure DisplayTimePicker; virtual;

    // Event Dispatch Methods
    procedure Change; override;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyPress( var Key: Char ); override;
    procedure DropDown; override;
    procedure DateTimeChange; dynamic;

    procedure DoEnter; override;
    procedure DoExit; override;
    procedure InvalidDate( var KeepFocused, KeepInvalidText: Boolean;
                           var NewDate: TDateTime ); dynamic;
    procedure InvalidTime( var KeepFocused, KeepInvalidText: Boolean;
                           var NewTime: TDateTime ); dynamic;

    // Property Access Methods
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

    function GetText: TCaption;
    procedure SetText( const Value: TCaption );

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

    procedure ReformatDateTime;
    property Text: TCaption
      read GetText
      write SetText;

    property AsDateTime: TDateTime
      read GetAsDateTime
      write SetAsDateTime
      stored False;

  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

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

    property PopupAlignment: TAlignment
      read FPopupAlignment
      write FPopupAlignment
      default taRightJustify;

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
    property DoubleBuffered;
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
    property ParentDoubleBuffered;
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
    property TextHint;
    property TextHintVisibleOnFocus;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
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


  {===============================}
  {== TRzMemo Class Declaration ==}
  {===============================}

  TLineColChangeEvent = procedure ( Sender: TObject; Line, Column: Integer ) of object;
  TClipboardChangeEvent = procedure ( Sender: TObject; HasSelection, HasText: Boolean ) of object;

  TRzMemo = class( TMemo )
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

    // Message Handling Methods
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
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;
    procedure RepaintFrame; virtual;

    // Event Dispatch Methods
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

    // Property Access Methods
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

    // Property Declarations
    property Canvas: TCanvas
      read FCanvas;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UseThemes: Boolean; virtual;
    procedure JumpTo( ALine, ACol: Integer );

    // Property Declarations
    property Column: Integer
      read GetColumn
      write SetColumn;

    property Line: Integer
      read GetLine
      write SetLine;
  published
    // Property Declarations
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

    // Inherited Properties & Events
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end;



  {===================================}
  {== TRzRichEdit Class Declaration ==}
  {===================================}

  TRzRichEdit = class( TRichEdit )
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

    // Message Handling Methods
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
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;
    procedure RepaintFrame; virtual;

    // Event Dispatch Methods
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

    // Property Access Methods
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

    // Property Declarations
    property Canvas: TCanvas
      read FCanvas;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UseThemes: Boolean; virtual;
    procedure JumpTo( ALine, ACol: Integer );

    function GetRtfData: string;

    // Property Declarations
    property Column: Integer
      read GetColumn
      write SetColumn;

    property Line: Integer
      read GetLine
      write SetLine;
  published
    // Property Declarations
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

    // Inherited Properties & Events
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end;


  TRzHotKeyEdit = class( THotKey )
  private
    FAboutInfo: TRzAboutInfo;
    FUpdatingColor: Boolean;
    FDisabledColor: TColor;
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

    procedure ReadOldFrameFlatProp( Reader: TReader );
    procedure ReadOldFrameFocusStyleProp( Reader: TReader );

    // Message Handling Methods
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
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;
    procedure RepaintFrame; virtual;

    // Property Access Methods
    function StoreColor: Boolean;
    function StoreFocusColor: Boolean;
    function StoreDisabledColor: Boolean;
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

    // Property Declarations
    property Canvas: TCanvas
      read FCanvas;

  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UseThemes: Boolean; virtual;
  published
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
  end;


implementation

uses
  {&RAS}
  Variants,
  DateUtils,
  StrUtils,
  Themes,
  Registry,
  TypInfo,
  RzPanel,
  RzBorder,
  CommCtrl,
  Clipbrd,
  RzGrafx;

{&RT}
{===========================}
{== TRzCustomEdit Methods ==}
{===========================}

{$IFDEF VCL160_OR_HIGHER}
class constructor TRzCustomEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook( TRzCustomEdit, TRzEditStyleHook );
end;


class destructor TRzCustomEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook( TRzCustomEdit, TRzEditStyleHook );
end;
{$ENDIF}


constructor TRzCustomEdit.Create( AOwner: TComponent );
begin
  inherited;

  ControlStyle := ControlStyle + [ csCaptureMouse ] - [ csSetCaption ];

  FCanvas := TControlCanvas.Create;
  TControlCanvas( FCanvas ).Control := Self;

  FDisabledColor := clBtnFace;
  FReadOnlyColor := clInfoBk;
  FReadOnlyColorOnFocus := False;
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

  FTextHintVisibleOnFocus := False;

  FBeepOnInvalidKey := True;
  FTabOnEnter := False;
  FFlatButtonColor := clBtnFace;
  {&RCI}
end;


procedure TRzCustomEdit.CreateParams( var Params: TCreateParams );
begin
  inherited;

  Params.Style := Params.Style or ws_ClipChildren;  // This is needed for ComboEdit functionality

  case Alignment of
    taLeftJustify:
      Params.Style := ( Params.Style or es_Left ) and not es_Multiline;

    taRightJustify:
      Params.Style := ( Params.Style or es_Right ) and not es_Multiline;

    taCenter:
      Params.Style := ( Params.Style or es_Center ) and not es_Multiline;
  end;
end;


procedure TRzCustomEdit.CreateWnd;
begin
  inherited;
//!!add  SetEditRect;
end;


destructor TRzCustomEdit.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  inherited;
end;


procedure TRzCustomEdit.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzCustomEdit.ReadOldFrameFlatProp( Reader: TReader );
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


procedure TRzCustomEdit.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzCustomEdit.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzCustomEdit.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


// UseRightToLeftLayout is specifically designed for data-aware descendants.
// Determines control *layout* from BiDi settings, not from field data type
function TRzCustomEdit.UseRightToLeftLayout: Boolean;
begin
  Result := inherited UseRightToLeftAlignment;
end;


function TRzCustomEdit.GetControlsAlignment: TAlignment;
begin
  Result := FAlignment;
end;


procedure TRzCustomEdit.SetAlignment( Value: TAlignment );
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;


procedure TRzCustomEdit.ValidateError;
var
  Msg: string;
begin
  if Assigned( FOnValidateError ) then
  begin
    Msg := '';
    FOnValidateError( Self, Msg );
    if Msg <> '' then
    begin
      raise EDBEditError.Create( Msg );
    end
    else
      inherited;
  end
  else
    inherited;
end;


procedure TRzCustomEdit.CMColorChanged( var Msg: TMessage );
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


function TRzCustomEdit.StoreColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpColor in FFrameControllerNotifications ) ) )
            and
            Enabled;
end;


function TRzCustomEdit.StoreFocusColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpFocusColor in FFrameControllerNotifications ) ) )
            and
            ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzCustomEdit.StoreDisabledColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpDisabledColor in FFrameControllerNotifications ) );
end;


function TRzCustomEdit.StoreReadOnlyColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpReadOnlyColor in FFrameControllerNotifications ) );
end;


function TRzCustomEdit.StoreReadOnlyColorOnFocus: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpReadOnlyColorOnFocus in FFrameControllerNotifications ) );
end;


function TRzCustomEdit.StoreParentColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpParentColor in FFrameControllerNotifications ) );
end;


function TRzCustomEdit.StoreFlatButtonColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtonColor in FFrameControllerNotifications ) );
end;


function TRzCustomEdit.StoreFlatButtons: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtons in FFrameControllerNotifications ) );
end;


function TRzCustomEdit.StoreFrameColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameColor in FFrameControllerNotifications ) );
end;


function TRzCustomEdit.StoreFrameHotColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotColor in FFrameControllerNotifications ) );
end;


function TRzCustomEdit.StoreFrameHotTrack: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotTrack in FFrameControllerNotifications ) );
end;


function TRzCustomEdit.StoreFrameHotStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotStyle in FFrameControllerNotifications ) );
end;


function TRzCustomEdit.StoreFrameSides: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameSides in FFrameControllerNotifications ) );
end;


function TRzCustomEdit.StoreFrameStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameStyle in FFrameControllerNotifications ) );
end;


function TRzCustomEdit.StoreFrameVisible: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameVisible in FFrameControllerNotifications ) );
end;


function TRzCustomEdit.StoreFramingPreference: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFramingPreference in FFrameControllerNotifications ) );
end;


procedure TRzCustomEdit.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzCustomEdit.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzCustomEdit.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomEdit.SetFrameController( Value: TRzFrameController );
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


procedure TRzCustomEdit.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomEdit.SetFrameHotTrack( Value: Boolean );
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


procedure TRzCustomEdit.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomEdit.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomEdit.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomEdit.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    if FFrameVisible then
      Ctl3D := True;
    RecreateWnd;
  end;
end;


procedure TRzCustomEdit.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      RepaintFrame;
  end;
end;


function TRzCustomEdit.ReadOnlyValue: Boolean;
begin
  Result := GetReadOnly;
end;


procedure TRzCustomEdit.ReadOnlyChanged;
begin
  if ReadOnlyValue then
    HideButton
  else if FDropButtonVisible then
    ShowButton;
  UpdateColors;
  RepaintButton;
end;


function TRzCustomEdit.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;


procedure TRzCustomEdit.SetReadOnly( Value: Boolean );
begin
  if ReadOnly <> Value then
  begin
    inherited ReadOnly := Value;
    ReadOnlyChanged;
  end;
end;


procedure TRzCustomEdit.SetReadOnlyColor( Value: TColor );
begin
  FReadOnlyColor := Value;
  if ReadOnlyValue then
    UpdateColors;
end;


procedure TRzCustomEdit.DoSetTextHint( const Value: string );
{$IFNDEF UNICODE}
var
  WS: WideString;
{$ENDIF}
begin
  if RunningAtLeast( winXP ) and ActiveStyleServicesEnabled and HandleAllocated then
  begin
    {$IFNDEF UNICODE}
    WS := Value;
    SendMessage( Handle, EM_SETCUEBANNER, Ord( FTextHintVisibleOnFocus ), LParam( PWideChar( WS ) ) );
    {$ELSE}
    SendTextMessage( Handle, EM_SETCUEBANNER, Ord( FTextHintVisibleOnFocus ), Value );
    {$ENDIF}
  end;
end;


procedure TRzCustomEdit.SetTextHintVisibleOnFocus( Value: Boolean );
begin
  if FTextHintVisibleOnFocus <> Value then
  begin
    FTextHintVisibleOnFocus := Value;
    if not ( csLoading in ComponentState ) then
      DoSetTextHint( TextHint );
  end;
end;


procedure TRzCustomEdit.InvalidKeyPressed;
begin
  if FBeepOnInvalidKey then
    SysUtils.Beep;
end;


procedure TRzCustomEdit.KeyPress( var Key: Char );
begin
  if FTabOnEnter and ( Ord( Key ) = vk_Return ) then
  begin
    Key := #0;
    PostMessage( Handle, wm_KeyDown, vk_Tab, 0 );
  end
  else if ( Alignment = taRightJustify ) and
          ( Win32MajorVersion = 4 ) and ( Win32MinorVersion = 0 ) and
          ( Ord( Key ) = vk_Return ) then
  begin
    // Enter key causes new line to be entered under NT 4
    Key := #0;
  end
  else
    inherited;
end;



procedure TRzCustomEdit.CMEnter( var Msg: TCMEnter );
begin
//!!add  SetEditRect;
  if ( Win32MajorVersion = 4 ) and ( Win32MinorVersion = 0 ) and AutoSelect and not ( csLButtonDown in ControlState ) then
    SelectAll;

  UpdateFrame( False, True );
  inherited;
end;


procedure TRzCustomEdit.CMExit( var Msg: TCMExit );
begin
  inherited;
  UpdateFrame( False, False );
//!!add  SetEditRect;
end;


function TRzCustomEdit.GetEditRect: TRect;
begin
  Result := ClientRect;
(*!!add
  if FShowDropButton then
  begin
    if not UseRightToLeftLayout then
      Dec( Result.Right, GetSystemMetrics( sm_CxVScroll ) )
    else
      Inc( Result.Left, GetSystemMetrics( sm_CxVScroll ) );
  end;
  *)
end;


(*!!add
procedure TRzCustomEdit.SetEditRect;
begin
  if ReadOnlyValue {and FHideButtonsOnReadOnly} then
  begin
    SendMessage( Handle, em_SetMargins, ec_LeftMargin, 0 );
    SendMessage( Handle, em_SetMargins, ec_RightMargin, 0 );
    Exit;
  end;

  if not UseRightToLeftAlignment then
  begin
    SendMessage( Handle, em_SetMargins, ec_LeftMargin, 0 );
    SendMessage( Handle, em_SetMargins, ec_RightMargin, GetSystemMetrics( sm_CxVScroll ) );
  end
  else
  begin
    SendMessage( Handle, em_SetMargins, ec_LeftMargin, GetSystemMetrics( sm_CxVScroll ) );
    SendMessage( Handle, em_SetMargins, ec_RightMargin, 0 );
  end;
end;
*)


function TRzCustomEdit.GetRightJustifiedText: string;
begin
  Result := Text;
end;


procedure TRzCustomEdit.RepaintFrame;
begin
  InvalidateWindowFrame( Handle, ClientRect );
end;


function TRzCustomEdit.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzCustomEdit.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  UpdateColors;
  RepaintButton;
//!!add  SetEditRect;
end;


(*!!add
procedure TRzCustomEdit.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  SetEditRect;
end;
*)


procedure TRzCustomEdit.WMNCPaint( var Msg: TWMNCPaint );
var
  DC: HDC;
  R, BtnRect: TRect;
begin
  inherited;                       { Must call inherited so scroll bar show up }

  DC := GetWindowDC( Handle );
  FCanvas.Handle := DC;
  try
    if FFrameVisible and not UseThemes then
    begin
      if FFrameHotTrack and ( FInControl or FOverControl ) then
        DrawFrame( FCanvas, Width, Height, FFrameHotStyle, Color, FFrameHotColor, FFrameSides )
      else
        DrawFrame( FCanvas, Width, Height, FFrameStyle, Color, FFrameColor, FFrameSides );

      Msg.Result := 0;
    end;

    DoSetTextHint( TextHint );

    // This is needed for Combo-Edit functionality
    if FShowDropButton then
    begin
      GetWindowRect( Handle, BtnRect );
      OffsetRect( BtnRect, -BtnRect.Left, -BtnRect.Top );

      if BorderStyle = bsSingle then
      begin
        if ActiveStyleServicesEnabled then
          InflateRect( BtnRect, -1, -1 )
        else
          InflateRect( BtnRect, -2, -2 );
        IntersectClipRect( FCanvas.Handle, BtnRect.Left, BtnRect.Top, BtnRect.Right, BtnRect.Bottom );
      end;

      // Use UseRightToLeftLayout to prevent calling DB descendants overrides as
      // they (incorrectly) determine layout from data type
      if not UseRightToLeftLayout then
        BtnRect.Left := BtnRect.Right - GetSystemMetrics( sm_CxVScroll )
      else
        BtnRect.Right := BtnRect.Left + GetSystemMetrics( sm_CxVScroll );
      R := BtnRect;

      DrawButton( FCanvas, R );
      ExcludeClipRect( FCanvas.Handle, R.Left, R.Top, R.Right, R.Bottom );

      FCanvas.FillRect( BtnRect );
      Msg.Result := 0;
    end;

  finally
    FCanvas.Handle := 0;
    ReleaseDC( Handle, DC );
  end;
end; {= TRzCustomEdit.WMNCPaint =}



procedure TRzCustomEdit.CMParentColorChanged( var Msg: TMessage );
begin
  inherited;

  if ParentColor then
  begin
    // If ParentColor set to True, must reset FNormalColor and FFocusColor
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;

  if FrameVisible and not UseThemes then
    RepaintFrame;
end;


procedure TRzCustomEdit.UpdateColors;
begin
  if csLoading in ComponentState then
    Exit;

  FUpdatingColor := True;
  try
    if not Enabled then
      Color := FDisabledColor
    else if Focused then
    begin
      if ReadOnlyValue and FReadOnlyColorOnFocus then
        Color := FReadOnlyColor
      else
        Color := FFocusColor;
    end
    else if ReadOnlyValue then
      Color := FReadOnlyColor
    else
      Color := FNormalColor;
  finally
    FUpdatingColor := False;
  end;
end;


procedure TRzCustomEdit.UpdateFrame( ViaMouse, InFocus: Boolean );
var
  PaintIt: Boolean;
  R: TRect;
begin
  if ViaMouse then
    FOverControl := InFocus
  else
    FInControl := InFocus;

  if FShowDropButton then
  begin
    PaintIt := FFlatButtons or FFrameHotTrack or ActiveStyleServicesEnabled;

    if PaintIt and ( FButtonState <> bsDown ) then
    begin
      R := ClientRect;
      if not FFrameHotTrack then
      begin
        if not UseRightToLeftLayout then
          R.Left := R.Right - GetSystemMetrics( sm_CxVScroll ) - 2
        else
          R.Right := R.Left + GetSystemMetrics( sm_CxVScroll ) + 2;
      end;
      RedrawWindow( Handle, @R, 0, rdw_Invalidate or rdw_Frame or rdw_NoErase );
    end;
  end
  else // Normal edit field
  begin
    if FFrameHotTrack then
      RepaintFrame;
  end;

  UpdateColors;
end; {= TRzCustomEdit.UpdateFrame =}


procedure TRzCustomEdit.CMMouseEnter( var Msg: TMessage );
begin
  {&RV}
  if csDesigning in ComponentState then
    Exit;

  inherited;

  UpdateFrame( True, True );
end;


procedure TRzCustomEdit.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  UpdateFrame( True, False );
end;


procedure TRzCustomEdit.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


procedure TRzCustomEdit.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  if FShowDropButton and
     ( ( ( Key = vk_Down ) and ( Shift = [ ssAlt ] ) ) or
       ( ( Key = vk_F4 ) and ( Shift = [] ) ) ) then
  begin
    DoDropDown;
    Key := 0;
  end;
end;


procedure TRzCustomEdit.MouseCancel;
begin
  if not FDroppedDown then
    ButtonState := bsUp;

  if GetCapture = Handle then
    ReleaseCapture;
end;


procedure TRzCustomEdit.CloseUp;
begin
  if Assigned( FOnCloseUp ) then
    FOnCloseUp( Self );
end;


procedure TRzCustomEdit.DropDown;
begin
  if Assigned( FOnDropDown ) then
    FOnDropDown( Self );
end;


procedure TRzCustomEdit.DoDropDown;
begin
  if FShowDropButton then
  begin
    FDroppedDown := True;
    try
      SetCapture( Handle );

      ButtonState := bsDown;
      DropDown;
      CloseUp;
      ButtonState := bsUp;
    finally
      FDroppedDown := False;
    end;
  end;
end;


procedure TRzCustomEdit.SetButtonState( Value: TButtonState );
begin
  if FShowDropButton and ( FButtonState <> Value ) then
  begin
    FButtonState := Value;
    RepaintButton;
  end;
end;


procedure TRzCustomEdit.SetDropButtonVisible( Value: Boolean );
begin
  if FDropButtonVisible <> Value then
  begin
    FDropButtonVisible := Value;
    ShowDropButton := FDropButtonVisible;
  end;
end;


procedure TRzCustomEdit.SetShowDropButton( Value: Boolean );
begin
  if FShowDropButton <> Value then
  begin
    FShowDropButton := Value;
    RecreateWnd;
  end;
end;


procedure TRzCustomEdit.SetFlatButtons( Value: Boolean );
begin
  if FFlatButtons <> Value then
  begin
    FFlatButtons := Value;
    RepaintButton;
  end;
end;


procedure TRzCustomEdit.WMNCHitTest( var Msg: TMessage );
begin
  inherited;

  if Msg.Result = Windows.HTNOWHERE then
  begin
    FMouseOverButton := True;
    Msg.Result := HTCLIENT;
  end
  else
    FMouseOverButton := False;
end;


procedure TRzCustomEdit.WMNCCalcSize( var Msg: TWMNCCalcSize );
var
  W: Integer;
begin
  if FShowDropButton then
  begin
    W := GetSystemMetrics( sm_CxVScroll );
    if ActiveStyleServicesEnabled then
      Dec( W );
    if not UseRightToLeftLayout then
      Dec( Msg.CalcSize_Params^.rgrc[ 0 ].Right, W )
    else
      Inc( Msg.CalcSize_Params^.rgrc[ 0 ].Left, W );
  end;
  inherited;
end;


procedure TRzCustomEdit.HideButton;
begin
  // Only hide the drop button if the DropButton is allowed to be visible
  if FDropButtonVisible then
    ShowDropButton := False;
end;


procedure TRzCustomEdit.ShowButton;
begin
  // Only show the drop button if the DropButton is allowed to be visible
  if FDropButtonVisible then
    ShowDropButton := True;
end;


procedure TRzCustomEdit.RepaintButton;
begin
  if HandleAllocated then
    SendMessage( Handle, wm_NCPaint, 0, 0 );
end;


procedure TRzCustomEdit.DrawButton( Canvas: TCanvas; var R: TRect );
var
  Flags: Cardinal;
  TempRect: TRect;
  ElementDetails: TThemedElementDetails;

  procedure DrawThemedDropDownButton;
  begin
    if not Enabled then
      ElementDetails := ActiveStyleServices.GetElementDetails( tcDropDownButtonDisabled )
    else if FButtonState = bsDown then
      ElementDetails := ActiveStyleServices.GetElementDetails( tcDropDownButtonPressed )
    else if FInControl or FOverControl then
      ElementDetails := ActiveStyleServices.GetElementDetails( tcDropDownButtonHot )
    else
      ElementDetails := ActiveStyleServices.GetElementDetails( tcDropDownButtonNormal );

    if UsingSystemStyle then
    begin
      if RunningAtLeast( winVista ) then
      begin
        Dec( R.Top );
        Inc( R.Bottom );
        Inc( R.Right );
      end;
    end
    else // VCL Styles
    begin
      Inc( R.Left );
    end;
    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );
  end;

begin {= TRzCustomEdit.DrawButton =}
  if ActiveStyleServicesEnabled then
  begin
    if FFlatButtons then
    begin
      if not ( FInControl or FOverControl ) then
      begin
        if UsingSystemStyle then
          Canvas.Brush.Color := Color
        else
          Canvas.Brush.Color := ActiveStyleColor( scEdit );
        Canvas.FillRect( R );

        DrawDropDownArrow( Canvas, R, uiWindowsXP, False, Enabled );
      end
      else
      begin
        DrawThemedDropDownButton;
      end;
    end
    else
    begin
      DrawThemedDropDownButton;
    end;
  end
  else // No Themes
  begin
    Canvas.Brush.Color := clBtnFace;
    Flags := dfcs_ScrollComboBox;
    if FButtonState = bsDown then
      Flags := Flags or dfcs_Pushed or dfcs_Flat;

    if FFlatButtons then
    begin
      if not ( FInControl or FOverControl ) then
      begin
        if not Enabled then
          Canvas.Brush.Color := FDisabledColor
        else if Focused then
        begin
          if ReadOnlyValue and FReadOnlyColorOnFocus then
            Canvas.Brush.Color := FReadOnlyColor
          else
            Canvas.Brush.Color := FFocusColor;
        end
        else if ReadOnlyValue then
          Canvas.Brush.Color := FReadOnlyColor
        else
          Canvas.Brush.Color := FNormalColor;
        Canvas.FillRect( R );

        DrawDropDownArrow( Canvas, R, uiWindows95, False, Enabled );
      end
      else
      begin
        Canvas.Brush.Color := FFlatButtonColor;

        if FFlatButtonColor = clBtnFace then
        begin
          if FButtonState = bsDown then
            TempRect := DrawBevel( Canvas, R, clBtnShadow, clBtnHighlight, 1, sdAllSides )
          else
            TempRect := DrawBevel( Canvas, R, clBtnHighlight, clBtnShadow, 1, sdAllSides );
        end
        else
        begin
          if FButtonState = bsDown then
            TempRect := DrawColorBorder( Canvas, R, FFlatButtonColor, fsStatus )
          else
            TempRect := DrawColorBorder( Canvas, R, FFlatButtonColor, fsPopup );
        end;

        FCanvas.FillRect( TempRect );
        DrawDropDownArrow( Canvas, TempRect, uiWindows95, FButtonState = bsDown, Enabled );
      end;

    end
    else
    begin
      if not Enabled then
        Flags := Flags or dfcs_Inactive;
      DrawFrameControl( Canvas.Handle, R, dfc_Scroll, Flags );
    end;
  end;
end;


procedure TRzCustomEdit.WMKillFocus( var Msg: TMessage );
begin
  inherited;
  MouseCancel;
end;


procedure TRzCustomEdit.WMLButtonDown( var Msg: TMessage );
begin
  if FMouseOverButton then
  begin
    if not Focused then
      SetFocus;
    DoDropDown;
    Msg.Result := 0;
  end
  else
  begin
    inherited;
    if not Focused then
      MouseCancel;
  end;
end;


procedure TRzCustomEdit.WMLButtonUp( var Msg: TMessage );
begin
  MouseCancel;
  inherited;
end;


procedure TRzCustomEdit.WMLButtonDblClk( var Msg: TMessage );
begin
  if FMouseOverButton then
    WMLButtonDown( Msg )
  else
    inherited;
end;


procedure TRzCustomEdit.WMRButtonDown( var Msg: TMessage );
begin
  if FMouseOverButton then
    Msg.Result := 0
  else
    inherited;
end;



procedure TRzCustomEdit.WMSetCursor( var Msg: TWMSetCursor );
begin
  if FMouseOverButton then
    Msg.HitTest := Windows.HTCAPTION;  (*was HTNOWHERE*)

  inherited;
end;


{$IFDEF VCL160_OR_HIGHER}

{==============================}
{== TRzEditStyleHook Methods ==}
{==============================}

constructor TRzEditStyleHook.Create( AControl: TWinControl );
begin
  inherited;
end;


procedure TRzEditStyleHook.WMNCCalcSize( var Msg: TWMNCCalcSize );
var
  W: Integer;
begin
  if ( Control is TRzCustomEdit ) then
  begin

    if TRzCustomEdit( Control ).ShowDropButton then
    begin
      W := GetSystemMetrics( sm_CxVScroll );
      if ActiveStyleServicesEnabled then
        Dec( W );
      if not TRzCustomEdit( Control ).UseRightToLeftLayout then
        Dec( Msg.CalcSize_Params^.rgrc[ 0 ].Right, W )
      else
        Inc( Msg.CalcSize_Params^.rgrc[ 0 ].Left, W );
    end;

    InflateRect( Msg.CalcSize_Params^.rgrc[ 0 ], -2, -2 );
    Handled := True;
  end;
end;


procedure TRzEditStyleHook.PaintNC( Canvas: TCanvas );
var
  Details: TThemedElementDetails;
  R, BtnRect: TRect;
  BtnWidth: Integer;
begin
  if StyleServices.Available then
  begin
    Details := StyleServices.GetElementDetails( teEditBorderNoScrollNormal );
    R := Rect( 0, 0, Control.Width, Control.Height );
    InflateRect( R, -2, -2 );

    BtnWidth := GetSystemMetrics( sm_CxVScroll );
    if TRzCustomEdit( Control ).ShowDropButton then
    begin
      Dec( R.Right, BtnWidth );
    end;
    ExcludeClipRect( Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom );

    StyleServices.DrawElement( Canvas.Handle, Details, Rect( 0, 0, Control.Width, Control.Height ) );

    if TRzCustomEdit( Control ).ShowDropButton then
    begin
      BtnRect := Rect( 0, 0, Control.Width, Control.Height );

      InflateRect( BtnRect, -2, -2 );
      IntersectClipRect( Canvas.Handle, BtnRect.Left, BtnRect.Top, BtnRect.Right, BtnRect.Bottom );

      // Use UseRightToLeftLayout to prevent calling DB descendants overrides as
      // they (incorrectly) determine layout from data type
      if not TRzCustomEdit( Control ).UseRightToLeftLayout then
        BtnRect.Left := BtnRect.Right - BtnWidth
      else
        BtnRect.Right := BtnRect.Left + BtnWidth;
      R := BtnRect;

      TRzCustomEdit( Control ).DrawButton( Canvas, R );
    end;
  end;
end;

{$ENDIF}


{============================}
{== TRzNumericEdit Methods ==}
{============================}

{$IFDEF VCL160_OR_HIGHER}
class constructor TRzNumericEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook( TRzNumericEdit, TRzEditStyleHook );
end;


class destructor TRzNumericEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook( TRzNumericEdit, TRzEditStyleHook );
end;
{$ENDIF}

constructor TRzNumericEdit.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle - [ csSetCaption ];

  // Pass nil since this component is not a TRzCalculator
  FCalculatorColors := TRzCalculatorColors.Create( nil );
  FCalculatorBoldButtons := False;

  Height := 21;
  Width := 65;
  FAllowBlank := False;
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


procedure TRzNumericEdit.CreateParams( var Params: TCreateParams );
begin
  inherited;
end;


destructor TRzNumericEdit.Destroy;
begin
  FCalculatorColors.Free;
  inherited;
end;


procedure TRzNumericEdit.Loaded;
begin
  inherited;
  FLoading := True;
  FFieldValue := EvaluateText;
  Text := FormatText( FFieldValue );
  FLoading := False;
end;


procedure TRzNumericEdit.Change;
begin
  if not ( csLoading in ComponentState ) and not FLoading then
    inherited;
end;


procedure TRzNumericEdit.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  FDecimalPressed := Key = vk_Decimal;
end;


procedure TRzNumericEdit.KeyPress( var Key: Char );
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


function TRzNumericEdit.IsValidChar( Key: Char ): Boolean;
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


function TRzNumericEdit.GetCalculatorVisible: Boolean;
begin
  Result := DropButtonVisible;
end;


procedure TRzNumericEdit.SetCalculatorVisible( Value: Boolean );
begin
  DropButtonVisible := Value;
end;


procedure TRzNumericEdit.SetIntegersOnly( Value: Boolean );
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


procedure TRzNumericEdit.SetMin( const Value: Extended );
begin
  if FMin <> Value then
  begin
    FMin := Value;
    if FMax < FMin then
      FMax := FMin;
    Invalidate;
  end;
end;


procedure TRzNumericEdit.SetMax( const Value: Extended );
begin
  if FMax <> Value then
  begin
    FMax := Value;
    if FMin > FMax then
      FMin := FMax;
    Invalidate;
  end;
end;


function TRzNumericEdit.GetIntValue: Int64;
begin
  Result := Round( GetValue );
end;


procedure TRzNumericEdit.SetIntValue( Value: Int64 );
begin
  SetValue( Value );
end;


function TRzNumericEdit.GetValue: Extended;
begin
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


procedure TRzNumericEdit.RangeError( EnteredValue, AdjustedValue: Extended; var AutoCorrect: Boolean );
begin
  if Assigned( FOnRangeError ) then
    FOnRangeError( Self, EnteredValue, AdjustedValue, AutoCorrect );
end;


function TRzNumericEdit.CheckValue( const Value: Extended; var KeepFocusOnEdit: Boolean ): Extended;
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


procedure TRzNumericEdit.SetValue( const Value: Extended );
begin
  if Value <> EvaluateText then
    Text := FormatText( Value );
end;


procedure TRzNumericEdit.WMPaste( var Msg: TWMPaste );
begin
  if ReadOnly then
    Exit;
  inherited;
end;


procedure TRzNumericEdit.WMCut( var Msg: TWMPaste );
begin
  if ReadOnly then
    Exit;
  inherited;
end;


procedure TRzNumericEdit.CMEnter( var Msg: TCMEnter );
begin
  FModified := False;
  FFieldValue := EvaluateText;
  if not FAllowBlank then
    Text := FormatText( FFieldValue );

  inherited;
end;


procedure TRzNumericEdit.CMExit( var Msg: TCMExit );
var
  N: Extended;
  MustSet, KeepFocusOnEdit: Boolean;
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

  inherited;
end;


procedure TRzNumericEdit.SetDisplayFormat( FormatString: string );
begin
  if FDisplayFormat <> FormatString then
  begin
    FDisplayFormat := FormatString;
    SetValue( Value );
    if not ( csLoading in ComponentState ) then
      Text := FormatText( Value );
  end;
end;


procedure TRzNumericEdit.SetCalculatorColors( Value: TRzCalculatorColors );
begin
  FCalculatorColors.Assign( Value );
end;


function TRzNumericEdit.FormatText( const Value: Extended ): string;
begin
  Result := FormatFloat( FDisplayFormat, Value );
end;



function TRzNumericEdit.CleanupQuotedText( const S: string ): string;
type
  TQuoteState = ( qsOutside, qsInside, qsStart, qsEnd );
var
  I: Byte;
  C: Char;
  QStr, CleanedUpQStr: string;
  QuoteState: TQuoteState;
begin
  // This method looks at each quoted string segment contained in the DisplayFormat string.
  // For each segment, all instances of the DecimalSeparator are replaced with a space
  // to ensure that the EvaluateText method works correctly. Having extra decimal separators
  // in the quoted text sections cause the StrToFloat function to fail.

  Result := S;

  if ( FDisplayFormat = '' ) or
     ( Pos( FormatSettings.DecimalSeparator, FDisplayFormat ) = 0 ) or
     ( ( Pos( Char( 34 ), FDisplayFormat ) = 0 ) and ( Pos( Char( 39 ), FDisplayFormat ) = 0 ) ) then
  begin
    // Exit this method if the DisplayFormat property is not used,
    // the DisplayFormat does not contain a DecimalSeparator,
    // the DisplayFormat does not contain either a double or single quote
    Exit;
  end;

  QuoteState := qsOutside;
  QStr := '';
  for I := 1 to Length( FDisplayFormat ) do
  begin
    C := FDisplayFormat[ I ];
    if CharInSet( C, [ Char( 34 ), Char( 39 ) ] ) then
    begin
      // If C is a double or single quote, then modify QuoteState appropriately
      case QuoteState of
        qsOutside: QuoteState := qsStart;
        qsInside:  QuoteState := qsEnd;
        qsStart:   QuoteState := qsEnd;
        qsEnd:     QuoteState := qsStart;
      end;
    end
    else
    begin
      // For other characters, the state will change when transitioning in and out of a quoted string
      case QuoteState of
        qsStart: QuoteState := qsInside;
        qsEnd:   QuoteState := qsOutside;
      end;
    end;


    if QuoteState = qsInside then
      QStr := QStr + C;


    if QuoteState = qsEnd then
    begin
      CleanedUpQStr := AnsiReplaceStr( QStr, FormatSettings.DecimalSeparator, ' ' );
      Result := AnsiReplaceStr( Result, QStr, CleanedUpQStr );
      QStr := '';
    end;
  end;
end;


function TRzNumericEdit.EvaluateText: Extended;
var
  S, NumberStr: string;
  C: Char;
  I: Byte;
  IsNeg, IncludeChar: Boolean;
  PosMinus, PosOpenParen, PosCloseParen: Integer;
begin
  NumberStr := CleanupQuotedText( Trim( Text ) );
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


procedure TRzNumericEdit.DropDown;
var
  OldOnExitHandler: TNotifyEvent;
  OldOnEnterHandler: TNotifyEvent;
begin
  if not ReadOnly then
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


procedure TRzNumericEdit.DisplayCalculator;
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

        if FFrameVisible and not UseThemes and ( FFrameStyle = fsFlat ) or ( FFrameStyle = fsFlatBold ) then
        begin
          Calculator.BorderOuter := fsFlat;
          Calculator.FlatColor := FFrameColor;
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
end; {= TRzNumericEdit.DisplayCalculator =}



{=====================}
{== TRzEdit Methods ==}
{=====================}

{$IFDEF VCL160_OR_HIGHER}
class constructor TRzEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook( TRzEdit, TRzEditStyleHook );
end;


class destructor TRzEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook( TRzEdit, TRzEditStyleHook );
end;
{$ENDIF}


function TRzEdit.GetText: TCaption;
begin
  Result := inherited Text;
end;


procedure TRzEdit.SetText( const Value: TCaption );
begin
  inherited Text := Value;
end;


{=========================}
{== TRzMaskEdit Methods ==}
{=========================}

function TRzMaskEdit.GetRightJustifiedText: string;
begin
  Result := EditText;
end;


{===========================}
{== TRzExpandEdit Methods ==}
{===========================}

constructor TRzExpandEdit.Create( AOwner: TComponent );
begin
  inherited;

  FExpandOn := etNone;
  FExpanded := False;
  FExpandedWidth := 0;
  FOrigWidth := Width;
  {&RCI}
end;


procedure TRzExpandEdit.WMSetFocus( var Msg: TWMSetFocus );
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


procedure TRzExpandEdit.WMRButtonUp( var Msg: TWMRButtonUp );
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


procedure TRzExpandEdit.WMKillFocus( var Msg: TWMKillFocus );
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


procedure TRzExpandEdit.SetExpandedWidth( Value: Integer );
begin
  {&RV}
  if FExpandedWidth <> Value then
  begin
    FExpandedWidth := Value;
    Repaint;
  end;
end;

procedure TRzExpandEdit.SetExpandOn( Value: TExpandOnType );
begin
  if FExpandOn <> Value then
  begin
    FExpandOn := Value;
    Repaint;
  end;
end;



{==========================}
{== TRzColorEdit Methods ==}
{==========================}

{$IFDEF VCL160_OR_HIGHER}
class constructor TRzColorEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook( TRzColorEdit, TRzEditStyleHook );
end;


class destructor TRzColorEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook( TRzColorEdit, TRzEditStyleHook );
end;
{$ENDIF}


constructor TRzColorEdit.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle - [ csDoubleClicks ];

  FDropButtonVisible := True;
  FShowDropButton := True;

  ParentColor := False;

  FSelectedColor := clNone;
  FShowNoColor := False;
  FShowDefaultColor := False;
  FShowSystemColors := False;
  FShowCustomColor := False;
  FColorDlgOptions := [ cdFullOpen ];
  FCustomColor := clWhite;
  FDefaultColor := clHighlight;
  FShowColorHints := True;

  FPopupAlignment := taRightJustify;
  FPopupWidth := 0;
  FPopupHeight := 0;
  FPopupButtonColor := clBtnFace;
  FPopupButtonFontColor := clWindowText;
end;


procedure TRzColorEdit.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FCustomColors ) then
    FCustomColors := nil;
end;


procedure TRzColorEdit.DropDown;
begin
  inherited;
  if not ReadOnly then
    DisplayColorPicker;
end;


procedure TRzColorEdit.DisplayColorPicker;
var
  PopupPanel: TRzPopupPanel;
  ColorPicker: TRzColorPicker;
begin
  if Self.Parent <> nil then
  begin
    PopupPanel := TRzPopupPanel.Create( Self );
    try
      ColorPicker := TRzColorPicker.Create( PopupPanel );
      ColorPicker.Parent := PopupPanel;

      if ( FPopupWidth <> 0 ) and ( FPopupHeight <> 0 ) then
      begin
        ColorPicker.AutoSize := False;
        ColorPicker.Width := FPopupWidth;
        ColorPicker.Height := FPopupHeight;
      end;


      PopupPanel.Parent := Self.Parent;
      PopupPanel.Font.Name := Font.Name;
      PopupPanel.Alignment := FPopupAlignment;

      ColorPicker.IsPopup := True;
      ColorPicker.ButtonColor := FPopupButtonColor;
      ColorPicker.ButtonFontColor := FPopupButtonFontColor;
      ColorPicker.NoColorCaption := FNoColorCaption;
      ColorPicker.ShowNoColor := FShowNoColor;
      ColorPicker.CustomColorCaption := FCustomColorCaption;
      ColorPicker.DefaultColorCaption := FDefaultColorCaption;
      ColorPicker.ShowCustomColor := FShowCustomColor;
      ColorPicker.ShowDefaultColor := FShowDefaultColor;
      ColorPicker.ShowSystemColors := FShowSystemColors;
      ColorPicker.ShowColorHints := FShowColorHints;
      ColorPicker.CustomColor := FCustomColor;
      ColorPicker.DefaultColor := FDefaultColor;
      ColorPicker.SelectedColor := FSelectedColor;
      ColorPicker.CustomColors := FCustomColors;
      ColorPicker.ColorDlgOptions := FColorDlgOptions;
      ColorPicker.Handle;

      if FFrameVisible and not UseThemes and ( FFrameStyle = fsFlat ) or ( FFrameStyle = fsFlatBold ) then
      begin
        ColorPicker.BorderOuter := fsFlat;
        ColorPicker.FlatColor := FFrameColor;
      end;
      ColorPicker.Visible := True;
      ColorPicker.OnClick := PopupPanel.Close;

      if PopupPanel.Popup( Self ) then
      begin
        SelectedColor := ColorPicker.SelectedColor;
        FCustomColor := ColorPicker.CustomColor;
        SetCustomColors( ColorPicker.CustomColors );
        Change;
      end;
    finally
      PopupPanel.Free;
    end;
  end;
end;


procedure TRzColorEdit.SetSelectedColor( Value: TColor );
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    Invalidate;
  end;
end;


procedure TRzColorEdit.SetCustomColors( Value: TRzCustomColors );
begin
  if FCustomColors <> Value then
  begin
    FCustomColors := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


procedure TRzColorEdit.WndProc( var Msg: TMessage );
var
  KeyMsg: TWMKey;
  ShiftState: TShiftState;
begin
  case Msg.Msg of
    wm_RButtonDown, wm_KeyFirst..wm_KeyLast:
    begin
      if Msg.Msg = wm_SysKeyDown then
      begin
        KeyMsg := TWMKey( Msg );
        ShiftState := KeyDataToShiftState( KeyMsg.KeyData );
        if ( KeyMsg.CharCode = vk_Down ) and ( ShiftState = [ ssAlt ] ) then
          inherited;
      end
      else if ( TWMKey( Msg ).CharCode = vk_F4 ) then
      begin
        KeyMsg := TWMKey( Msg );
        ShiftState := KeyDataToShiftState( KeyMsg.KeyData );
        if ( ShiftState = [] ) or ( ssAlt in ShiftState ) then
          inherited;
      end;
      // Eat all other messages
    end;

    else
    begin
      inherited;
    end;
  end;

  if ( Msg.Msg = wm_LButtonDown ) and not ( csDesigning in ComponentState ) then
  begin
    HideCaret( Handle );
    ReleaseCapture;
  end;

end;


procedure TRzColorEdit.WMKillFocus( var Msg: TMessage );
begin
  inherited;
  if not FDroppedDown then
    Invalidate;
end;


procedure TRzColorEdit.WMPaint( var Msg: TWMPaint );
var
  R: TRect;
  PS: TPaintStruct;
  DC: HDC;
  OldPenStyle: TPenStyle;
  OldBrushStyle: TBrushStyle;
begin
  // Do not call inherited.  Default handler draws client area as an edit field.

  DC := Msg.DC;
  if Msg.DC = 0 then
    DC := BeginPaint( Handle, PS );

  try
    FCanvas.Handle := DC;
    try
      FCanvas.Font := Font;
      FCanvas.Brush := Brush;
      Windows.GetClientRect( Handle, R );
      InflateRect( R, -2, -2 );

      if SelectedColor = clNone then
      begin
        OldPenStyle := FCanvas.Pen.Style;
        OldBrushStyle := FCanvas.Brush.Style;
        if Focused or FDroppedDown then
        begin
          FCanvas.Pen.Color := ActiveStyleSystemColor( clBtnShadow );
          FCanvas.Brush.Color := ActiveStyleSystemColor( clBtnShadow );
        end
        else
        begin
          FCanvas.Pen.Color := ActiveStyleSystemColor( clBtnFace );
          FCanvas.Brush.Color := ActiveStyleSystemColor( clBtnFace );
        end;
        FCanvas.Brush.Style := bsBDiagonal;
        FCanvas.Rectangle( R );
        FCanvas.Brush.Style := OldBrushStyle;
        FCanvas.Pen.Style := OldPenStyle;
      end
      else
      begin
        DrawBox( FCanvas, R, ActiveStyleSystemColor( clBtnShadow ) );
        FCanvas.Brush.Color := SelectedColor;
        // GDI functions used directly b/c FCanvas methods are not painting focus rect correctly
        FillRect( DC, R, FCanvas.Brush.Handle );
      end;

      if Focused or FDroppedDown then
      begin
        // GDI functions used directly b/c FCanvas methods are not painting focus rect correctly
        DrawFocusRect( DC, R );
      end;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    if Msg.DC = 0 then
      ReleaseDC( Handle, DC );
  end;
end; {= TRzColorEdit.WMPaint =}


procedure TRzColorEdit.WMNCHitTest( var Msg: TMessage );
begin
  inherited;
  // Treat the entire edit field as if mouse is over the button
  FMouseOverButton := True;
end;


procedure TRzColorEdit.WMSetFocus( var Msg: TMessage );
begin
  inherited;
  HideCaret( Handle );                      // Do not allow caret to become visible
  Invalidate;
end;



{=============================}
{== TRzDateTimeEdit Methods ==}
{=============================}

{$IFDEF VCL160_OR_HIGHER}
class constructor TRzDateTimeEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook( TRzDateTimeEdit, TRzEditStyleHook );
end;


class destructor TRzDateTimeEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook( TRzDateTimeEdit, TRzEditStyleHook );
end;
{$ENDIF}


constructor TRzDateTimeEdit.Create( AOwner: TComponent );
begin
  inherited;

  FDropButtonVisible := True;
  FShowDropButton := True;

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


destructor TRzDateTimeEdit.Destroy;
begin
  FClockFaceColors.Free;
  FCalendarColors.Free;
  inherited;
end;


procedure TRzDateTimeEdit.SetDateTime;
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


procedure TRzDateTimeEdit.CheckDateTimeChange;
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


procedure TRzDateTimeEdit.DateTimeChange;
begin
  if Assigned( FOnDateTimeChange ) then
    FOnDateTimeChange( Self, FDateTime );
end;


procedure TRzDateTimeEdit.DoEnter;
begin
  inherited;
  UpdateText;
end;


procedure TRzDateTimeEdit.DoExit;
begin
  inherited;
  if not FValidating and not ( csDestroying in ComponentState ) then
  begin
    FValidating := True;
    try
      try
        ReformatDateTime;
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
end;


procedure TRzDateTimeEdit.InvalidDate( var KeepFocused, KeepInvalidText: Boolean;
                                       var NewDate: TDateTime );
begin
  if Assigned( FOnInvalidDate ) then
    FOnInvalidDate( Self, KeepFocused, KeepInvalidText, NewDate );
end;


procedure TRzDateTimeEdit.InvalidTime( var KeepFocused, KeepInvalidText: Boolean;
                                       var NewTime: TDateTime );
begin
  if Assigned( FOnInvalidTime ) then
    FOnInvalidTime( Self, KeepFocused, KeepInvalidText, NewTime );
end;


procedure TRzDateTimeEdit.UpdateText;
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
         ( ( ( FDateTime = 0 ) and not FTimeHasBeenSet ) or
           ( ( FDateTime = 0 ) and FTimeHasBeenSet and ( Text = '' ) and not FTimePicked ) ) then
      begin
        inherited Text := '';
      end
      else
        inherited Text := FormatDateTime( TempFormat, FDateTime );
    end
    else // FEditType = etDate
    begin
      if not FSettingDate and
         ( ( ( FDateTime = 0 ) and not FDateHasBeenSet ) or
           ( ( FDateTime = 0 ) and FDateHasBeenSet and ( Text = '' ) and not FDatePicked ) ) then
        inherited Text := ''
      else
        inherited Text := FormatDateTime( TempFormat, FDateTime );
    end;
  finally
    FUpdating := False;
  end;
  Modified := False;
end; {= TRzDateTimeEdit.UpdateText =}


procedure TRzDateTimeEdit.DropDown;
var
  OldOnExitHandler: TNotifyEvent;
  OldOnEnterHandler: TNotifyEvent;
begin
  if not ReadOnly then
  begin
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
  end;
end;


function TRzDateTimeEdit.DaysToBitmask( Days: array of Byte ): Cardinal;
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


procedure TRzDateTimeEdit.DisplayCalendar;
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

        if FFrameVisible and not UseThemes and ( FFrameStyle = fsFlat ) or ( FFrameStyle = fsFlatBold ) then
        begin
          Calendar.BorderOuter := fsFlat;
          Calendar.FlatColor := FFrameColor;
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
end; {= TRzDateTimeEdit.DisplayCalendar =}


procedure TRzDateTimeEdit.DisplayTimePicker;
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
        if FFrameVisible and not UseThemes and ( FFrameStyle = fsFlat ) or ( FFrameStyle = fsFlatBold ) then
        begin
          TimePicker.BorderOuter := fsFlat;
          TimePicker.FlatColor := FFrameColor;
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
end; {= TRzDateTimeEdit.DisplayTimePicker =}


procedure TRzDateTimeEdit.Change;
begin
  if not FUpdating then
  begin
    SetDateTime;
  end;
  inherited;
end;


procedure TRzDateTimeEdit.AdjustYear( DeltaYears: Integer );
var
  DT: TDateTime;
begin
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


procedure TRzDateTimeEdit.AdjustMonth( DeltaMonths: Integer );
var
  DT: TDateTime;
begin
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


procedure TRzDateTimeEdit.AdjustDay( DeltaDays: Integer );
var
  DT: TDateTime;
begin
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


procedure TRzDateTimeEdit.AdjustHour( DeltaHours: Int64 );
begin
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


procedure TRzDateTimeEdit.AdjustMinute( DeltaMinutes: Int64 );
begin
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


procedure TRzDateTimeEdit.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;

  if ReadOnly then
    Exit;

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


procedure TRzDateTimeEdit.KeyPress( var Key: Char );
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


procedure TRzDateTimeEdit.ReformatDateTime;
var
  RangeCheck: Integer;
  DT, NewDate, NewTime: TDateTime;
  KeepFocused, KeepInvalidText: Boolean;
  OriginalText, Fmt: string;
begin
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
        SetFocus;
        if KeepInvalidText then
          inherited Text := OriginalText
        else if FDateTime = 0.0 then
          inherited Text := '';
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
        SetFocus;
        if KeepInvalidText then
          inherited Text := OriginalText
        else if FDateTime = 0.0 then
          inherited Text := '';
      end;

      FTimeHasBeenSet := False;
    end;

    if not ( KeepInvalidText or KeepFocused ) then
      UpdateText;
  end;
end;


function TRzDateTimeEdit.GetText: TCaption;
begin
  Result := inherited Text;
end;


procedure TRzDateTimeEdit.SetText( const Value: TCaption );
begin
  inherited Text := Value;
  Modified := True;
  ReformatDateTime;
end;


function TRzDateTimeEdit.DateInRange( Value: TDateTime ): Integer;
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


function TRzDateTimeEdit.GetDate: TDate;
begin
  Result := Trunc( FDateTime );
end;


procedure TRzDateTimeEdit.SetDate( Value: TDate );
var
  RangeCheck: Integer;
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


function TRzDateTimeEdit.GetAsDateTime: TDateTime;
begin
  if FEditType = etDate then
    Result := Date
  else
    Result := Time;
end;


procedure TRzDateTimeEdit.SetAsDateTime( Value: TDateTime );
begin
  if FEditType = etDate then
    SetDate( Value )
  else
    SetTime( Value );
end;


function TRzDateTimeEdit.StoreDate: Boolean;
begin
  Result := ( FEditType = etDate ) and ( Date <> 0.0 );
end;


function TRzDateTimeEdit.StoreMinDate: Boolean;
begin
  Result := FMinDate <> 0.0;
end;


procedure TRzDateTimeEdit.SetMinDate( Value: TDateTime );
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


function TRzDateTimeEdit.StoreMaxDate: Boolean;
begin
  Result := FMaxDate <> 0.0;
end;


procedure TRzDateTimeEdit.SetMaxDate( Value: TDateTime );
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


procedure TRzDateTimeEdit.SetRange( MinValue, MaxValue: TDate );
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


procedure TRzDateTimeEdit.SetEditType( Value: TRzDTEditType );
begin
  if FEditType <> Value then
  begin
    FEditType := Value;
    ReformatDateTime;
  end;
end;


procedure TRzDateTimeEdit.SetFormat( const Value: string );
begin
  if FFormat <> Value then
  begin
    SetDateTime;
    FFormat := Value;
    UpdateText;
  end;
end;


procedure TRzDateTimeEdit.SetClockFaceColors( Value: TRzClockFaceColors );
begin
  FClockFaceColors.Assign( Value );
end;


procedure TRzDateTimeEdit.SetCalendarColors( Value: TRzCalendarColors );
begin
  FCalendarColors.Assign( Value );
end;


function TRzDateTimeEdit.GetTime: TTime;
begin
  Result := Frac( FDateTime );
end;


procedure TRzDateTimeEdit.SetTime( Value: TTime );
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


function TRzDateTimeEdit.StoreTime: Boolean;
begin
  Result := ( FEditType = etTime ) and ( Time <> 0.0 );
end;


procedure TRzDateTimeEdit.Clear;
begin
  inherited;
  FDateTime := 0;
  CheckDateTimeChange;
  FTimeHasBeenSet := False;
  FDateHasBeenSet := False;
  UpdateText;
end;



procedure TRzDateTimeEdit.WMGetDlgCode( var Msg: TWMGetDlgCode );
begin
  Msg.Result := dlgc_WantChars + dlgc_WantArrows;
end;



{=====================}
{== TRzMemo Methods ==}
{=====================}

constructor TRzMemo.Create( AOwner: TComponent );
begin
  inherited;

  ControlStyle := ControlStyle - [ csSetCaption ];

  FCanvas := TControlCanvas.Create;
  TControlCanvas( FCanvas ).Control := Self;

  FDisabledColor := clBtnFace;
  FReadOnlyColor := clInfoBk;
  FReadOnlyColorOnFocus := False;
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


procedure TRzMemo.CreateWnd;
begin
  inherited;
  LineColChange;
  ClipboardChange;
  {&RCI}
end;


destructor TRzMemo.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  inherited;
end;


procedure TRzMemo.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzMemo.ReadOldFrameFlatProp( Reader: TReader );
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


procedure TRzMemo.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzMemo.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzMemo.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


procedure TRzMemo.CMColorChanged( var Msg: TMessage );
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


function TRzMemo.StoreColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpColor in FFrameControllerNotifications ) ) )
            and
            Enabled;
end;


function TRzMemo.StoreFocusColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpFocusColor in FFrameControllerNotifications ) ) )
            and
            ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzMemo.StoreDisabledColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpDisabledColor in FFrameControllerNotifications ) );
end;


function TRzMemo.StoreReadOnlyColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpReadOnlyColor in FFrameControllerNotifications ) );
end;


function TRzMemo.StoreReadOnlyColorOnFocus: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpReadOnlyColorOnFocus in FFrameControllerNotifications ) );
end;


function TRzMemo.StoreParentColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpParentColor in FFrameControllerNotifications ) );
end;


function TRzMemo.StoreFlatButtonColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtonColor in FFrameControllerNotifications ) );
end;


function TRzMemo.StoreFlatButtons: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtons in FFrameControllerNotifications ) );
end;


function TRzMemo.StoreFrameColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameColor in FFrameControllerNotifications ) );
end;


function TRzMemo.StoreFrameHotColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotColor in FFrameControllerNotifications ) );
end;


function TRzMemo.StoreFrameHotTrack: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotTrack in FFrameControllerNotifications ) );
end;


function TRzMemo.StoreFrameHotStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotStyle in FFrameControllerNotifications ) );
end;


function TRzMemo.StoreFrameSides: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameSides in FFrameControllerNotifications ) );
end;


function TRzMemo.StoreFrameStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameStyle in FFrameControllerNotifications ) );
end;


function TRzMemo.StoreFrameVisible: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameVisible in FFrameControllerNotifications ) );
end;


function TRzMemo.StoreFramingPreference: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFramingPreference in FFrameControllerNotifications ) );
end;


procedure TRzMemo.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzMemo.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzMemo.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzMemo.SetFrameController( Value: TRzFrameController );
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


procedure TRzMemo.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzMemo.SetFrameHotTrack( Value: Boolean );
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


procedure TRzMemo.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzMemo.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    RepaintFrame;
  end;
end;


procedure TRzMemo.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzMemo.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    if FFrameVisible then
      Ctl3D := True;
    RecreateWnd;
  end;
end;


procedure TRzMemo.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      RepaintFrame;
  end;
end;


function TRzMemo.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;


procedure TRzMemo.SetReadOnly( Value: Boolean );
begin
  if ReadOnly <> Value then
  begin
    inherited ReadOnly := Value;
    UpdateColors;
  end;
end;


procedure TRzMemo.SetReadOnlyColor( Value: TColor );
begin
  FReadOnlyColor := Value;
  if ReadOnly then
    UpdateColors;
end;


procedure TRzMemo.JumpTo( ALine, ACol: Integer );
begin
  Line := ALine;
  if Line = ALine then
    Column := ACol;
end;


function TRzMemo.GetColumn: Integer;
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


procedure TRzMemo.SetColumn( Value: Integer );
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


function TRzMemo.GetLine: Integer;
var
  CaretPos: TPoint;
  P: Integer;
begin
  if SelLength = 0 then
  begin
    Result := SendMessage( Handle, em_LineFromChar, SelStart, 0 ) + 1;
  end
  else
  begin
    Windows.GetCaretPos( CaretPos );

    P := SendMessage( Handle, em_CharFromPos, 0, MakeLParam( Word( CaretPos.X ), Word( CaretPos.Y ) ) );
    if P <> -1 then
      Result := HiWord( P ) + 1
    else
      Result := 1;
  end;
end;


procedure TRzMemo.SetLine( Value: Integer );
var
  P, L: Integer;
begin
  P := SendMessage( Handle, em_LineIndex, Value - 1, 0 );

  SendMessage( Handle, em_SetSel, P, P );                  // Position Cursor to correct line number

  if Line = Value then
  begin
    // Move selected line to top of window
    L := SendMessage( Handle, em_GetFirstVisibleLine, 0, 0 );
    SendMessage( Handle, em_LineScroll, 0, Value - L - 1 );
    LineColChange;
  end;
end;


procedure TRzMemo.Change;
begin
  inherited;
  LineColChange;
  {&RV}
end;


procedure TRzMemo.Click;
begin
  inherited;
  LineColChange;
end;


procedure TRzMemo.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  LineColChange;
end;


procedure TRzMemo.KeyUp( var Key: Word; Shift: TShiftState );
begin
  inherited;
  LineColChange;
end;


procedure TRzMemo.KeyPress( var Key: Char );
begin
  if FTabOnEnter and ( Ord( Key ) = vk_Return ) then
  begin
    Key := #0;
    PostMessage( Handle, wm_KeyDown, vk_Tab, 0 );
  end
  else
    inherited;
end;


procedure TRzMemo.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;
  ClipboardChange;
end;


procedure TRzMemo.LineColChange;
begin
  if Assigned( FOnLineColChange ) then
    FOnLineColChange( Self, Line, Column );
  ClipboardChange;
end;


procedure TRzMemo.ClipboardChange;
begin
  if Assigned( FOnClipboardChange ) then
    FOnClipboardChange( Self, SelLength <> 0, Clipboard.HasFormat( cf_Text ) );
end;


function TRzMemo.DoMouseWheel( Shift: TShiftState; WheelDelta: Integer;
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
end; {= TRzMemo.DoMouseWheel =}


procedure TRzMemo.RepaintFrame;
begin
  InvalidateWindowFrame( Handle, ClientRect );
end;


function TRzMemo.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzMemo.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  UpdateColors;
end;


procedure TRzMemo.WMNCPaint( var Msg: TWMNCPaint );
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
end; {= TRzMemo.WMNCPaint =}


procedure TRzMemo.CMParentColorChanged( var Msg: TMessage );
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


procedure TRzMemo.UpdateColors;
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


procedure TRzMemo.UpdateFrame( ViaMouse, InFocus: Boolean );
begin
  if ViaMouse then
    FOverControl := InFocus;

  if FFrameHotTrack then
    RepaintFrame;

  UpdateColors;
end;


procedure TRzMemo.CMEnter( var Msg: TCMEnter );
begin
  UpdateFrame( False, True );
  inherited;
end;


procedure TRzMemo.CMExit( var Msg: TCMExit );
begin
  inherited;
  UpdateFrame( False, False );
end;


procedure TRzMemo.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  UpdateFrame( True, True );
end;


procedure TRzMemo.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  UpdateFrame( True, False );
end;


procedure TRzMemo.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


{=========================}
{== TRzRichEdit Methods ==}
{=========================}

{$IFDEF VCL160_OR_HIGHER}
class constructor TRzRichEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook( TRzRichEdit, TRichEditStyleHook );
end;


class destructor TRzRichEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook( TRzRichEdit, TRichEditStyleHook );
end;
{$ENDIF}

constructor TRzRichEdit.Create( AOwner: TComponent );
begin
  inherited;

  ControlStyle := ControlStyle - [ csSetCaption ];

  FCanvas := TControlCanvas.Create;
  TControlCanvas( FCanvas ).Control := Self;

  FDisabledColor := clBtnFace;
  FReadOnlyColor := clInfoBk;
  FReadOnlyColorOnFocus := False;
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


procedure TRzRichEdit.CreateWnd;
begin
  inherited;
  LineColChange;
  ClipboardChange;
  {&RCI}
end;


destructor TRzRichEdit.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  inherited;
end;


procedure TRzRichEdit.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzRichEdit.ReadOldFrameFlatProp( Reader: TReader );
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


procedure TRzRichEdit.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzRichEdit.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzRichEdit.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


procedure TRzRichEdit.CMColorChanged( var Msg: TMessage );
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


function TRzRichEdit.StoreColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpColor in FFrameControllerNotifications ) ) )
            and
            Enabled;
end;


function TRzRichEdit.StoreFocusColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpFocusColor in FFrameControllerNotifications ) ) )
            and
            ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzRichEdit.StoreDisabledColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpDisabledColor in FFrameControllerNotifications ) );
end;


function TRzRichEdit.StoreReadOnlyColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpReadOnlyColor in FFrameControllerNotifications ) );
end;


function TRzRichEdit.StoreReadOnlyColorOnFocus: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpReadOnlyColorOnFocus in FFrameControllerNotifications ) );
end;


function TRzRichEdit.StoreParentColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpParentColor in FFrameControllerNotifications ) );
end;


function TRzRichEdit.StoreFlatButtonColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtonColor in FFrameControllerNotifications ) );
end;


function TRzRichEdit.StoreFlatButtons: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtons in FFrameControllerNotifications ) );
end;


function TRzRichEdit.StoreFrameColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameColor in FFrameControllerNotifications ) );
end;


function TRzRichEdit.StoreFrameHotColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotColor in FFrameControllerNotifications ) );
end;


function TRzRichEdit.StoreFrameHotTrack: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotTrack in FFrameControllerNotifications ) );
end;


function TRzRichEdit.StoreFrameHotStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotStyle in FFrameControllerNotifications ) );
end;


function TRzRichEdit.StoreFrameSides: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameSides in FFrameControllerNotifications ) );
end;


function TRzRichEdit.StoreFrameStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameStyle in FFrameControllerNotifications ) );
end;


function TRzRichEdit.StoreFrameVisible: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameVisible in FFrameControllerNotifications ) );
end;


function TRzRichEdit.StoreFramingPreference: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFramingPreference in FFrameControllerNotifications ) );
end;


procedure TRzRichEdit.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzRichEdit.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzRichEdit.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzRichEdit.SetFrameController( Value: TRzFrameController );
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


procedure TRzRichEdit.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzRichEdit.SetFrameHotTrack( Value: Boolean );
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


procedure TRzRichEdit.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzRichEdit.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    RepaintFrame;
  end;
end;


procedure TRzRichEdit.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzRichEdit.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    if FFrameVisible then
      Ctl3D := True;
    RecreateWnd;
  end;
end;


procedure TRzRichEdit.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      RepaintFrame;
  end;
end;


function TRzRichEdit.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;


procedure TRzRichEdit.SetReadOnly( Value: Boolean );
begin
  if ReadOnly <> Value then
  begin
    inherited ReadOnly := Value;
    UpdateColors;
  end;
end;


procedure TRzRichEdit.SetReadOnlyColor( Value: TColor );
begin
  FReadOnlyColor := Value;
  if ReadOnly then
    UpdateColors;
end;


procedure TRzRichEdit.JumpTo( ALine, ACol: Integer );
begin
  Line := ALine;
  if Line = ALine then
    Column := ACol;
end;


function TRzRichEdit.GetColumn: Integer;
begin
  // The same technique used in the TRzMemo to get the correct caret position
  // regardless of the selection anchor does not work with the rich edit
  // common control.  Sending the em_CharFromPos message to a rich edit results
  // in an access violation.

  Result := CaretPos.X + 1;
end;


procedure TRzRichEdit.SetColumn( Value: Integer );
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


function TRzRichEdit.GetLine: Integer;
begin
  // The same technique used in the TRzMemo to get the correct caret position
  // regardless of the selection anchor does not work with the rich edit
  // common control.  Sending the em_CharFromPos message to a rich edit results
  // in an access violation.

  Result := CaretPos.Y + 1;
end;


procedure TRzRichEdit.SetLine( Value: Integer );
var
  P, L: Integer;
begin
  P := SendMessage( Handle, em_LineIndex, Value - 1, 0 );

  SendMessage( Handle, em_SetSel, P, P );                  // Position Cursor to correct line number

  if Line = Value then
  begin
    // Move selected line to top of window
    L := SendMessage( Handle, em_GetFirstVisibleLine, 0, 0 );
    SendMessage( Handle, em_LineScroll, 0, Value - L - 1 );
    LineColChange;
  end;
end;


procedure TRzRichEdit.Change;
begin
  inherited;
  LineColChange;
  {&RV}
end;


procedure TRzRichEdit.Click;
begin
  inherited;
  LineColChange;
end;


procedure TRzRichEdit.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  LineColChange;
end;


procedure TRzRichEdit.KeyUp( var Key: Word; Shift: TShiftState );
begin
  inherited;
  LineColChange;
end;


procedure TRzRichEdit.KeyPress( var Key: Char );
begin
  if FTabOnEnter and ( Ord( Key ) = vk_Return ) then
  begin
    Key := #0;
    PostMessage( Handle, wm_KeyDown, vk_Tab, 0 );
  end
  else
    inherited;
end;


procedure TRzRichEdit.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;
  ClipboardChange;
end;


procedure TRzRichEdit.SelectionChange;
begin
  inherited;
  ClipboardChange;
end;


procedure TRzRichEdit.LineColChange;
begin
  if Assigned( FOnLineColChange ) then
    FOnLineColChange( Self, Line, Column );
  ClipboardChange;
end;


procedure TRzRichEdit.ClipboardChange;
begin
  if Assigned( FOnClipboardChange ) then
    FOnClipboardChange( Self, SelLength <> 0, Clipboard.HasFormat( cf_Text ) );
end;


function TRzRichEdit.DoMouseWheel( Shift: TShiftState; WheelDelta: Integer;
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
end; {= TRzRichEdit.DoMouseWheel =}


procedure TRzRichEdit.RepaintFrame;
begin
  InvalidateWindowFrame( Handle, ClientRect );
end;


function TRzRichEdit.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzRichEdit.CMEnabledChanged( var Msg: TMessage );
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


procedure TRzRichEdit.WMNCPaint( var Msg: TWMNCPaint );
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
end; {= TRzRichEdit.WMNCPaint =}


procedure TRzRichEdit.CMParentColorChanged( var Msg: TMessage );
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


procedure TRzRichEdit.UpdateColors;
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


procedure TRzRichEdit.UpdateFrame( ViaMouse, InFocus: Boolean );
begin
  if ViaMouse then
    FOverControl := InFocus;

  if FFrameHotTrack then
    RepaintFrame;

  UpdateColors;
end;


procedure TRzRichEdit.CMEnter( var Msg: TCMEnter );
begin
  UpdateFrame( False, True );
  inherited;
end;


procedure TRzRichEdit.CMExit( var Msg: TCMExit );
begin
  inherited;
  UpdateFrame( False, False );
end;


procedure TRzRichEdit.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  UpdateFrame( True, True );
end;


procedure TRzRichEdit.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  UpdateFrame( True, False );
end;


procedure TRzRichEdit.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


function TRzRichEdit.GetRtfData: string;
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


{===========================}
{== TRzHotKeyEdit Methods ==}
{===========================}

constructor TRzHotKeyEdit.Create( AOwner: TComponent );
begin
  inherited;

  Height := 21;
  FCanvas := TControlCanvas.Create;
  TControlCanvas( FCanvas ).Control := Self;

  FUpdatingColor := False;
  FDisabledColor := clBtnFace;
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
  {&RCI}
end;


destructor TRzHotKeyEdit.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  inherited;
end;


procedure TRzHotKeyEdit.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzHotKeyEdit.ReadOldFrameFlatProp( Reader: TReader );
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


procedure TRzHotKeyEdit.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzHotKeyEdit.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzHotKeyEdit.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


procedure TRzHotKeyEdit.CMColorChanged( var Msg: TMessage );
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


function TRzHotKeyEdit.StoreColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpColor in FFrameControllerNotifications ) ) )
            and
            Enabled;
end;


function TRzHotKeyEdit.StoreFocusColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpFocusColor in FFrameControllerNotifications ) ) )
            and
            ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzHotKeyEdit.StoreDisabledColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpDisabledColor in FFrameControllerNotifications ) );
end;


function TRzHotKeyEdit.StoreParentColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpParentColor in FFrameControllerNotifications ) );
end;


function TRzHotKeyEdit.StoreFlatButtonColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtonColor in FFrameControllerNotifications ) );
end;


function TRzHotKeyEdit.StoreFlatButtons: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtons in FFrameControllerNotifications ) );
end;


function TRzHotKeyEdit.StoreFrameColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameColor in FFrameControllerNotifications ) );
end;


function TRzHotKeyEdit.StoreFrameHotColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotColor in FFrameControllerNotifications ) );
end;


function TRzHotKeyEdit.StoreFrameHotTrack: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotTrack in FFrameControllerNotifications ) );
end;


function TRzHotKeyEdit.StoreFrameHotStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotStyle in FFrameControllerNotifications ) );
end;


function TRzHotKeyEdit.StoreFrameSides: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameSides in FFrameControllerNotifications ) );
end;


function TRzHotKeyEdit.StoreFrameStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameStyle in FFrameControllerNotifications ) );
end;


function TRzHotKeyEdit.StoreFrameVisible: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameVisible in FFrameControllerNotifications ) );
end;


function TRzHotKeyEdit.StoreFramingPreference: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFramingPreference in FFrameControllerNotifications ) );
end;


procedure TRzHotKeyEdit.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzHotKeyEdit.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzHotKeyEdit.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzHotKeyEdit.SetFrameController( Value: TRzFrameController );
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


procedure TRzHotKeyEdit.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzHotKeyEdit.SetFrameHotTrack( Value: Boolean );
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


procedure TRzHotKeyEdit.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzHotKeyEdit.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    RepaintFrame;
  end;
end;


procedure TRzHotKeyEdit.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzHotKeyEdit.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    if FFrameVisible then
      Ctl3D := True;
    RecreateWnd;
  end;
end;


procedure TRzHotKeyEdit.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      RepaintFrame;
  end;
end;


procedure TRzHotKeyEdit.CMEnter( var Msg: TCMEnter );
begin
  UpdateFrame( False, True );
  inherited;
end;


procedure TRzHotKeyEdit.CMExit( var Msg: TCMExit );
begin
  inherited;
  UpdateFrame( False, False );
end;


procedure TRzHotKeyEdit.RepaintFrame;
begin
  InvalidateWindowFrame( Handle, ClientRect );
end;


function TRzHotKeyEdit.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzHotKeyEdit.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  UpdateColors;
end;


procedure TRzHotKeyEdit.WMNCPaint( var Msg: TWMNCPaint );
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
end; {= TRzHotKeyEdit.WMNCPaint =}


procedure TRzHotKeyEdit.CMParentColorChanged( var Msg: TMessage );
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


procedure TRzHotKeyEdit.UpdateColors;
begin
  if csLoading in ComponentState then
    Exit;

  FUpdatingColor := True;
  try
    if not Enabled then
      Color := FDisabledColor
    else if Focused then
      Color := FFocusColor
    else
      Color := FNormalColor;
  finally
    FUpdatingColor := False;
  end;
end;


procedure TRzHotKeyEdit.UpdateFrame( ViaMouse, InFocus: Boolean );
begin
  if ViaMouse then
    FOverControl := InFocus;

  if FFrameHotTrack then
    RepaintFrame;

  UpdateColors;
end;


procedure TRzHotKeyEdit.CMMouseEnter( var Msg: TMessage );
begin
  {&RV}
  if csDesigning in ComponentState then
    Exit;

  inherited;

  UpdateFrame( True, True );
end;


procedure TRzHotKeyEdit.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  UpdateFrame( True, False );
end;


procedure TRzHotKeyEdit.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


{&RUIF}
end.




