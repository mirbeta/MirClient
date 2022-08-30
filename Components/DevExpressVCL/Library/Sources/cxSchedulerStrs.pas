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

unit cxSchedulerStrs;

{$I cxVer.inc}

interface

resourcestring
  scxUntitledEvent = 'Untitled event';

  scxVertical   = 'Vertical';
  scxHorizontal = 'Horizontal';
  scxTimeGrid   = 'TimeGrid';

  scxMinute  = 'Minute';
  scxMinutes = 'Minutes';
  scxHour    = 'Hour';
  scxHours   = 'Hours';
  scxOneDay  = 'One day';

  // Navigation buttons
  scxNextAppointment = 'Next appointment';
  scxPrevAppointment = 'Previous appointment';

  // dialogs constants
  scxDeleteRecurringEventDescription = 'is a recurring event. Do you want to delete only this occurrence or the series?';
  scxEditRecurringEventDescription   = 'is a recurring event. Do you want to open only this occurrence or the series?';

  scxGoToDateDialogCaption     = 'Go To Date';
  scxDeleteTypeDialogCaption   = 'Confirm Delete';
  scxDeleteTypeOccurrenceLabel = 'Delete this occurrence';
  scxDeleteTypeSeriesLabel     = 'Delete the series';
  scxEditTypeDialogCaption     = 'Open Recurring Item';
  scxEditTypeOccurrenceLabel   = 'Open this occurrence';
  scxEditTypeSeriesLabel       = 'Open the series';

  scxExitConfirmation  = 'Do you want to save changes?';
  scxDeleteConfirmation= 'This item has been changed. Are you sure you want to delete it?';
  scxWrongTimeBounds   = 'The end date you entered occurs before the start date.';
  scxWrongPattern      = 'The recurrence pattern is not valid.';
  scxReplaceOccurrenceDate = 'Some months have fewer than %s days. For these months, the occurrence will fall on the last day of the month.';
  scxInvalidRecurrenceDuration = 'The duration of the event must be shorter than how frequently it occurs. ' +
    'Shorten the duration, or change the recurrence pattern in the Recurrence dialog box.';
  scxConfirmLostExceptions = 'Any exceptions associated with this recurring event will be lost. Is this OK?';
  scxInvalidNumber      = 'You must enter a valid number.';
  scxShedulerEditorFormNotRegistered = 'There is no registered editor form';
  scxNoAvailableFreeTime = 'No free time is available.';
  scxCannotRescheduleOccurrence = 'Cannot reschedule an occurrence of the recurring appointment "%s" if it skips over a later occurrence of the same appointment.';
  scxTwoOccurrencesPerDay = 'Two occurrences of "%s" cannot occur on the same day.';

  scxEvent             = 'Event';
  scxUntitled          = 'Untitled';

  scxNoneEvent         = 'simple event';
  scxRecurrenceEvent   = 'recurrence event';
  scxExceptionEvent    = 'exception event';
  scxOccurenceEvent    = 'occurrence event';

  scxAdd               = '&Add';
  scxEdit              = '&Edit';
  scxDelete            = '&Delete';
  scxRecurrence        = '&Recurrence';
  scxActionRecurrence  = 'Recurrence';

  scxDate              = '&Date:';
  scxShowIn            = '&Show in:';
  scxAgendaCalendar    = 'Agenda Calendar';
  scxDayCalendar       = 'Day Calendar';
  scxWeekCalendar      = 'Week Calendar';
  scxMonthCalendar     = 'Month Calendar';
  scxWorkWeekCalendar  = 'Work Week Calendar';

  scxContinueFrom      = 'From';
  scxContinueTo        = 'To';
  scxEventsConflict    = 'Conflicts with another event in your schedule.';
  scxResource          = 'Resource';
  scxSubject           = '&Subject:';
  scxLocation          = '&Location:';
  scxLabelAs           = 'Label As:';
  scxLabel             = 'La&bel:';
  scxStartTime         = 'S&tart time:';
  scxEndTime           = '&End time:';
  scxAllDayEvent       = '&All day event';
  scxRecurrenceLabel   = 'Recurrence:';

  scxReminder          = 'Reminder:';
  scxShowTimeAs        = 'Sho&w time as:';
  scxShowAs            = 'Show As:';

  scxSuffixMinute      = 'minute';
  scxSuffixMinutes     = 'minutes';
  scxSuffixHour        = 'hour';
  scxSuffixHours       = 'hours';
  scxSuffixDay         = 'day';
  scxSuffixDays        = 'days';
  scxSuffixWeek        = 'week';
  scxSuffixWeeks       = 'weeks';

  scxBusy              = 'Busy';
  scxFree              = 'Free';
  scxTentative         = 'Tentative';
  scxOutOfOffice       = 'Out of office';

  scxRecurrenceCaption         = 'Event recurrence';
  scxRecurrenceHolidayCaption  = 'Holiday recurrence';

  scxEventTime         = ' Event time ';
  scxRecurrencePattern = ' Recurrence pattern ';
  scxRangeOfRecurrence = ' Range of recurrence ';
  scxStart             = '&Start:';
  scxStart1            = 'S&tart:';
  scxEnd               = '&End:';
  scxDuration          = 'D&uration:';
  // Pattern
  scxDaily             = '&Daily';
  scxWeekly            = '&Weekly';
  scxQuarterly         = '&Quarterly';
  scxMonthly           = '&Monthly';
  scxYearly            = '&Yearly';
  // daily
  scxEvery             = 'E&very';
  scxEveryWeekDay      = 'Every wee&kday';
  scxDays              = 'day(s)';
  // weekly
  scxWeeksOn           = 'week(s) on:';
  scxRecurEvery        = 'Rec&ur every';
  //monthly
  scxOfEvery           = 'of every';
  scxMonths            = 'month(s)';
  // yearly
  scxThe               = 'T&he';
  scxOf                = 'of';

  // Modern Style Hint
  scxModernStyleHintStart        = 'Start:';
  scxModernStyleHintEnd          = 'End:';
  scxModernStyleHintLocation     = 'Location:';
  scxModernStyleHintReminder     = 'Reminder:';
  scxModernStyleHintComplete     = 'Complete:';
  scxModernStyleHintReminderNone = 'None';


  // Task links
  scxTaskComplete                   = 'Task co&mplete:';
  scxTaskStatus                     = 'Task status';
  scxTaskDependencyEditorCaption    = 'Task Dependency';
  scxTaskWrongTimeBounds            = 'A new date must be entered within the period of %s - %s.';
  scxFinishToFinishLong   = 'Finish-to-Finish (FF)';
  scxFinishToStartLong    = 'Finish-to-Start (FS)';
  scxFrom                 = 'From:';
  scxStartToFinishLong    = 'Start-to-Finish (SF)';
  scxStartToStartLong     = 'Start-to-Start (SS)';
  scxTo                   = 'To:';
  scxType                 = '&Type:';

  // other
  scxFirst             = 'first';
  scxSecond            = 'second';
  scxThird             = 'third';
  scxFourth            = 'fourth';
  scxLast              = 'last';
  scxDay               = 'D&ay';
  scxDay1              = 'day';
  scxWeekday           = 'weekday';
  scxWeekendday        = 'weekend day';
  scxNoEndDate         = '&No end date';
  scxEndAfter          = 'End a&fter:';
  scxEndBy             = 'End &by:';
  scxOccurences        = 'occurrences';

  // buttons
  scxAdd1              = 'Add';
  scxAdd1Hint          = 'Add (Ins)';
  scxEditDotted        = 'Edit...';
  scxApply             = '&Apply';
  scxFindAvailableTime = 'Find available time';
  scxOk                = '&OK';
  scxSaveAndClose      = 'Save && Close';
  scxSaveAndCloseHint  = 'Save & Close';
  scxSave              = 'Save';
  scxCancel            = '&Cancel';
  scxClose             = '&Close';
  scxActionClose       = 'Close';
  scxDown              = '&Down';
  scxDelete1           = 'Delete';
  scxDelete1Hint       = 'Delete (Del)';
  scxEdit1             = 'Edit';
  scxImport            = '&Import';
  scxExport            = '&Export';
  scxImportHint        = 'Import';
  scxExportHint        = 'Export';
  scxRemoveRecur       = '&Remove recurrence';
  scxSelectAll         = 'Select &all';
  scxSelectNone        = 'Select &none';
  scxUp                = '&Up';
  scxAppointment       = 'Appointment';
  scxActions           = 'Actions';
  scxOptions           = 'Options';
  //
  scxResourceLayoutCaption = 'Resources layout editor';

  // popup menu resources
  scxpmNewEvent          = '&New Event';
  scxpmNewAllDayEvent    = 'New All Day &Event';
  scxpmNewRecurringEvent = 'New &Recurring Event';
  scxpmToday             = 'T&oday';
  scxpmGotoThisDay       = 'Go to This &Day';
  scxpmGoToDate          = 'Go &to Date...';
  scxpmResourcesLayout   = 'Resources layout editor...';

  // for event
  scxpmOpen              = '&Open';
  scxpmEditSeries        = 'Edit Se&ries';
  scxpmShowTimeAs        = 'S&how Time As';
  scxpmDelete            = '&Delete';
  scxpmFree              = '&Free';
  scxpmTentative         = '&Tentative';
  scxpmBusy              = '&Busy';
  scxpmOutOfOffice       = '&Out of Office';
  scxpmLabel             = '&Label';

  // event label captions
  scxEventLabelNone      = 'None';
  scxEventLabel0         = 'Important';
  scxEventLabel1         = 'Business';
  scxEventLabel2         = 'Personal';
  scxEventLabel3         = 'Vacation';
  scxEventLabel4         = 'Must Attend';
  scxEventLabel5         = 'Travel Required';
  scxEventLabel6         = 'Needs Preparation';
  scxEventLabel7         = 'Birthday';
  scxEventLabel8         = 'Anniversary';
  scxEventLabel9         = 'Phone Call';

  // for time ruler menu items
  scxpmTimeZone          = 'Chan&ge Time Zone';
  scxpm60Minutes         = '6&0 Minutes';
  scxpm30Minutes         = '&30 Minutes';
  scxpm15Minutes         = '&15 Minutes';
  scxpm10Minutes         = '10 &Minutes';
  scxpm6Minutes          = '&6 Minutes';
  scxpm5Minutes          = '&5 Minutes';

  // for year view scale menu items
  scxpmFullYear          = '&Full Year';
  scxpmHalfYear          = '&Half-Year';
  scxpmQuarter           = '&Quarter';

  // year view scales
  scxFullYear            = 'Full Year';
  scxHalfYear            = 'Half-Year';
  scxQuarter             = 'Quarter';
  scxHalfYearShort       = 'H';
  scxQuarterShort        = 'Q';

  //navigator hints
  scxFirstButtonHint     = 'First Resource';
  scxPrevPageButtonHint  = 'Previous Page';
  scxPrevButtonHint      = 'Previous Resource';
  scxNextButtonHint      = 'Next Resource';
  scxNextPageButtonHint  = 'Next Page';
  scxLastButtonHint      = 'Last Resource';
  scxShowMoreResourcesButtonHint  = 'Show More Resources';
  scxShowFewerResourcesButtonHint = 'Show Fewer Resources';

  //for reminder
  scxrCaptionReminder  = '1 Reminder';
  scxrCaptionReminders = '%d Reminders';
  scxrDismissButton    = '&Dismiss';
  scxrDismissAllButton = 'Dismiss &All';
  scxrDueIn            = 'Due in';
  scxrOpenItemButton   = '&Open Item';
  scxrSnoozeButton     = '&Snooze';
  scxrSubject          = 'Subject';
  scxrSnoozeLabel      = '&Snooze';
  scxrSelected         = '%d reminders are selected';
  scxrStartTime        = 'Start time: %s';

  // time
  scxTime0m     = '0 minutes';
  scxTime5m     = '5 minutes';
  scxTime10m    = '10 minutes';
  scxTime15m    = '15 minutes';
  scxTime20m    = '20 minutes';
  scxTime30m    = '30 minutes';
  scxTime1h     = '1 hour';
  scxTime2h     = '2 hours';
  scxTime3h     = '3 hours';
  scxTime4h     = '4 hours';
  scxTime5h     = '5 hours';
  scxTime6h     = '6 hours';
  scxTime7h     = '7 hours';
  scxTime8h     = '8 hours';
  scxTime9h     = '9 hours';
  scxTime10h    = '10 hours';
  scxTime11h    = '11 hours';
  scxTime12h    = '12 hours';
  scxTime18h    = '18 hours';
  scxTime1d     = '1 day';
  scxTime2d     = '2 days';
  scxTime3d     = '3 days';
  scxTime4d     = '4 days';
  scxTime1w     = '1 week';
  scxTime2w     = '2 weeks';
  // advance time
  scxAdvance0h  = '0 hours before start';
  scxAdvance5m  = '5 minutes before start';
  scxAdvance10m = '10 minutes before start';
  scxAdvance15m = '15 minutes before start';

  // for export

  secxSetDateRangeCaption = 'Set Date Range';
  secxSetDateRangeText = 'Export and create individual occurrences of appointments or ' +
    'tasks that occur between:';
  secxSetDateRangeAnd = 'and';
  secxTrue = 'TRUE';
  secxFalse = 'FALSE';
  secxExportStorageInvalid = 'Storage not assigned';

  // card field names

  secxYes          = 'Yes';
  secxNo           = 'No';
  secxSubject      = 'Subject';
  secxLocation     = 'Location';
  secxDescription  = 'Description';
  secxAllDay       = 'All day';
  secxStart        = 'Start';
  secxFinish       = 'Finish';
  secxState        = 'State';
  secxReminder     = 'Reminder';
  secxTimeRange    = '%s to %s';

  // table fields

  secxStartDate          = 'StartDate';
  secxStartTime          = 'StartTime';
  secxEndDate            = 'EndDate';
  secxEndTime            = 'EndTime';
  secxAlldayevent        = 'Alldayevent';
  secxReminderonoff      = 'Reminderonoff';
  secxReminderDate       = 'ReminderDate';
  secxReminderTime       = 'ReminderTime';
  secxCategories         = 'Categories';
  secxShowtimeas         = 'Showtimeas';

  // storage
  scxRequiredFieldsNeeded = 'The following required fields'#13#10'%sare not assigned!';
  scxInvalidFieldName = 'Invalid field name';
  scxInvalidCustomField = 'Invalid custom field';

  // Event fields
  scxAllDayEventField = 'All Day Event';
  scxIDField = 'ID';
  scxActualFinishField = 'Actual Finish';
  scxActualStartField = 'Actual Start';
  scxCaptionField = 'Caption';
  scxEmptyDayCaption = 'There are no events';
  scxEnabledField = 'Enabled';
  scxEventTypeField = 'Type';
  scxFinishField = 'Finish';
  scxLabelField = 'Label';
  scxLocationField = 'Location';
  scxMessageField = 'Message';
  scxParentIDField = 'ParentID';
  scxGroupIDField = 'GroupID';
  scxRecurrenceField = 'Recurrence Pattern';
  scxRecurrenceIndexField = 'Recurrence Index';
  scxReminderDateField = 'ReminderDate';
  scxReminderField = 'Reminder';
  scxReminderMinutesBeforeStartField = 'Reminder Minutes Before Start';
  scxResourceField = 'Resource';
  scxStartField = 'Start';
  scxStateField = 'State';
  scxTaskCompleteField = 'Task Complete';
  scxTaskIndexField = 'Task Index';
  scxTaskLinksField = 'Task Links';
  scxTaskStatusField = 'Task Status';

  // status
  scxNotStarted = 'Not Started';
  scxInProgress = 'In Progress';
  scxComplete   = 'Complete';
  scxWaiting    = 'Waiting';
  scxDeferred   = 'Deferred';

  // Event task relations

  scxFinishToStart  = 'Finish-To-Start';
  scxStartToStart   = 'Start-To-Start';
  scxFinishToFinish = 'Finish-To-Finish';
  scxStartToFinish  = 'Start-To-Finish ';

  scxFinishToStartShort  = 'FS';
  scxStartToStartShort   = 'SS';
  scxFinishToFinishShort = 'FF';
  scxStartToFinishShort  = 'SF';

  scxGanttEventHint = 'Task: %s'#13#10'Complete: %d %%'#13#10'Start: %s'#13#10'Finish: %s';
  scxLinkHint = 'Task Link: %s (%s)'#13#10'From: %s'#13#10'To: %s';

  //

  scxCompleteDisplayFormat = '0 %';

  scxNone      = 'Simple Event';
  scxPattern   = 'Recurrence Pattern';
  scxOccurrence= 'Simple Occurrence';
  scxException = 'Exception Occurrence';
  scxCustom    = 'Custom Occurrence';

  // Holidays

  scxHolidaysEditorCaption                = 'Holidays editor';
  scxLocationsGroupBox                    = 'Locations';
  scxHolidaysGroupBox                     = 'Holidays';
  scxAddedHolidaysGroupBox                = 'Added holidays';
  scxLocationName                         = 'Name:';
  scxHolidaysLocationEditorCaption        = 'Location editor';
  scxHolidayName                          = 'Name:';
  scxHolidayDate                          = 'Date:';
  scxHolidaysLocationHolidayEditorCaption = 'Holiday editor';
  scxOutlookFormatMismatch                = 'Holiday format mismatch';
  scxHolidayDisplayFormat                 = '%s (%s)';
  scxAddedHolidayDisplayFormat            = '%s - %s (%s)';

const
  sRangeNames: array[0..4] of Pointer =
    (@scxFirst, @scxSecond, @scxThird, @scxFourth, @scxLast);
  sDayNames: array [0..9] of string =
    (scxDay, scxWeekday, scxWeekendday, '', '', '', '', '', '', '');
  sEventLabelCaptions: array[0..10] of Pointer = (
    @scxEventLabelNone, @scxEventLabel0, @scxEventLabel1, @scxEventLabel2,
    @scxEventLabel3, @scxEventLabel4, @scxEventLabel5, @scxEventLabel6,
    @scxEventLabel7, @scxEventLabel8, @scxEventLabel9);
  sEventRelations: array[0..3] of Pointer = (
    @scxFinishToStart, @scxStartToStart, @scxFinishToFinish, @scxStartToFinish);
  sEventRelationsShort: array[0..3] of Pointer = (
    @scxFinishToStartShort, @scxStartToStartShort, @scxFinishToFinishShort,
    @scxStartToFinishShort);
  sEventTaskStatus: array[0..4] of Pointer =
    (@scxNotStarted, @scxInProgress, @scxComplete, @scxWaiting, @scxDeferred);

procedure cxSchedulerInitStrings;

implementation

uses
  SysUtils, dxCore, cxFormats;

procedure cxSchedulerInitStrings;
var
  I: Integer;
begin
  for I := 1 to 7 do
    sDayNames[2 + I] := dxFormatSettings.LongDayNames[I];
end;

procedure AddcxSchedulerResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAddress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAddress);
  end;

begin
  InternalAdd('scxUntitledEvent', @scxUntitledEvent);
  InternalAdd('scxVertical', @scxVertical);
  InternalAdd('scxHorizontal', @scxHorizontal);
  InternalAdd('scxTimeGrid', @scxTimeGrid);
  InternalAdd('scxMinute', @scxMinute);
  InternalAdd('scxMinutes', @scxMinutes);
  InternalAdd('scxHour', @scxHour);
  InternalAdd('scxHours', @scxHours);
  InternalAdd('scxOneDay', @scxOneDay);
  InternalAdd('scxNextAppointment', @scxNextAppointment);
  InternalAdd('scxPrevAppointment', @scxPrevAppointment);
  InternalAdd('scxDeleteRecurringEventDescription', @scxDeleteRecurringEventDescription);
  InternalAdd('scxEditRecurringEventDescription', @scxEditRecurringEventDescription);
  InternalAdd('scxGoToDateDialogCaption', @scxGoToDateDialogCaption);
  InternalAdd('scxDeleteTypeDialogCaption', @scxDeleteTypeDialogCaption);
  InternalAdd('scxDeleteTypeOccurrenceLabel', @scxDeleteTypeOccurrenceLabel);
  InternalAdd('scxDeleteTypeSeriesLabel', @scxDeleteTypeSeriesLabel);
  InternalAdd('scxEditTypeDialogCaption', @scxEditTypeDialogCaption);
  InternalAdd('scxEditTypeOccurrenceLabel', @scxEditTypeOccurrenceLabel);
  InternalAdd('scxEditTypeSeriesLabel', @scxEditTypeSeriesLabel);
  InternalAdd('scxExitConfirmation', @scxExitConfirmation);
  InternalAdd('scxDeleteConfirmation', @scxDeleteConfirmation);
  InternalAdd('scxWrongTimeBounds', @scxWrongTimeBounds);
  InternalAdd('scxWrongPattern', @scxWrongPattern);
  InternalAdd('scxReplaceOccurrenceDate', @scxReplaceOccurrenceDate);
  InternalAdd('scxInvalidRecurrenceDuration', @scxInvalidRecurrenceDuration);
  InternalAdd('scxConfirmLostExceptions', @scxConfirmLostExceptions);
  InternalAdd('scxInvalidNumber', @scxInvalidNumber);
  InternalAdd('scxShedulerEditorFormNotRegistered', @scxShedulerEditorFormNotRegistered);
  InternalAdd('scxNoAvailableFreeTime', @scxNoAvailableFreeTime);
  InternalAdd('scxCannotRescheduleOccurrence', @scxCannotRescheduleOccurrence);
  InternalAdd('scxTwoOccurrencesPerDay', @scxTwoOccurrencesPerDay);
  InternalAdd('scxEvent', @scxEvent);
  InternalAdd('scxUntitled', @scxUntitled);
  InternalAdd('scxNoneEvent', @scxNoneEvent);
  InternalAdd('scxRecurrenceEvent', @scxRecurrenceEvent);
  InternalAdd('scxExceptionEvent', @scxExceptionEvent);
  InternalAdd('scxOccurenceEvent', @scxOccurenceEvent);
  InternalAdd('scxAdd', @scxAdd);
  InternalAdd('scxEdit', @scxEdit);
  InternalAdd('scxDelete', @scxDelete);
  InternalAdd('scxRecurrence', @scxRecurrence);
  InternalAdd('scxActionRecurrence', @scxActionRecurrence);
  InternalAdd('scxDate', @scxDate);
  InternalAdd('scxShowIn', @scxShowIn);
  InternalAdd('scxAgendaCalendar', @scxAgendaCalendar);
  InternalAdd('scxDayCalendar', @scxDayCalendar);
  InternalAdd('scxWeekCalendar', @scxWeekCalendar);
  InternalAdd('scxMonthCalendar', @scxMonthCalendar);
  InternalAdd('scxWorkWeekCalendar', @scxWorkWeekCalendar);
  InternalAdd('scxEventsConflict', @scxEventsConflict);
  InternalAdd('scxResource', @scxResource);
  InternalAdd('scxSubject', @scxSubject);
  InternalAdd('scxLocation', @scxLocation);
  InternalAdd('scxLabelAs', @scxLabelAs);
  InternalAdd('scxLabel', @scxLabel);
  InternalAdd('scxStartTime', @scxStartTime);
  InternalAdd('scxEndTime', @scxEndTime);
  InternalAdd('scxAllDayEvent', @scxAllDayEvent);
  InternalAdd('scxRecurrenceLabel', @scxRecurrenceLabel);
  InternalAdd('scxReminder', @scxReminder);
  InternalAdd('scxShowTimeAs', @scxShowTimeAs);
  InternalAdd('scxShowAs', @scxShowAs);
  InternalAdd('scxSuffixMinute', @scxSuffixMinute);
  InternalAdd('scxSuffixMinutes', @scxSuffixMinutes);
  InternalAdd('scxSuffixHour', @scxSuffixHour);
  InternalAdd('scxSuffixHours', @scxSuffixHours);
  InternalAdd('scxSuffixDay', @scxSuffixDay);
  InternalAdd('scxSuffixDays', @scxSuffixDays);
  InternalAdd('scxSuffixWeek', @scxSuffixWeek);
  InternalAdd('scxSuffixWeeks', @scxSuffixWeeks);
  InternalAdd('scxBusy', @scxBusy);
  InternalAdd('scxFree', @scxFree);
  InternalAdd('scxTentative', @scxTentative);
  InternalAdd('scxOutOfOffice', @scxOutOfOffice);
  InternalAdd('scxRecurrenceCaption', @scxRecurrenceCaption);
  InternalAdd('scxRecurrenceHolidayCaption', @scxRecurrenceHolidayCaption);
  InternalAdd('scxEventTime', @scxEventTime);
  InternalAdd('scxRecurrencePattern', @scxRecurrencePattern);
  InternalAdd('scxRangeOfRecurrence', @scxRangeOfRecurrence);
  InternalAdd('scxContinueFrom', @scxContinueFrom);
  InternalAdd('scxContinueTo', @scxContinueTo);
  InternalAdd('scxStart', @scxStart);
  InternalAdd('scxStart1', @scxStart1);
  InternalAdd('scxEnd', @scxEnd);
  InternalAdd('scxDuration', @scxDuration);
  InternalAdd('scxDaily', @scxDaily);
  InternalAdd('scxWeekly', @scxWeekly);
  InternalAdd('scxQuarterly', @scxQuarterly);
  InternalAdd('scxMonthly', @scxMonthly);
  InternalAdd('scxYearly', @scxYearly);
  InternalAdd('scxEvery', @scxEvery);
  InternalAdd('scxEveryWeekDay', @scxEveryWeekDay);
  InternalAdd('scxDays', @scxDays);
  InternalAdd('scxWeeksOn', @scxWeeksOn);
  InternalAdd('scxRecurEvery', @scxRecurEvery);
  InternalAdd('scxOfEvery', @scxOfEvery);
  InternalAdd('scxMonths', @scxMonths);
  InternalAdd('scxThe', @scxThe);
  InternalAdd('scxOf', @scxOf);
  InternalAdd('scxModernStyleHintStart', @scxModernStyleHintStart);
  InternalAdd('scxModernStyleHintEnd', @scxModernStyleHintEnd);
  InternalAdd('scxModernStyleHintLocation', @scxModernStyleHintLocation);
  InternalAdd('scxModernStyleHintReminder', @scxModernStyleHintReminder);
  InternalAdd('scxModernStyleHintComplete', @scxModernStyleHintComplete);
  InternalAdd('scxModernStyleHintReminderNone', @scxModernStyleHintReminderNone);
  InternalAdd('scxTaskComplete', @scxTaskComplete);
  InternalAdd('scxTaskStatus', @scxTaskStatus);
  InternalAdd('scxTaskDependencyEditorCaption', @scxTaskDependencyEditorCaption);
  InternalAdd('scxTaskWrongTimeBounds', @scxTaskWrongTimeBounds);
  InternalAdd('scxFinishToFinishLong', @scxFinishToFinishLong);
  InternalAdd('scxFinishToStartLong', @scxFinishToStartLong);
  InternalAdd('scxFrom', @scxFrom);
  InternalAdd('scxStartToFinishLong', @scxStartToFinishLong);
  InternalAdd('scxStartToStartLong', @scxStartToStartLong);
  InternalAdd('scxTo', @scxTo);
  InternalAdd('scxType', @scxType);
  InternalAdd('scxFirst', @scxFirst);
  InternalAdd('scxSecond', @scxSecond);
  InternalAdd('scxThird', @scxThird);
  InternalAdd('scxFourth', @scxFourth);
  InternalAdd('scxLast', @scxLast);
  InternalAdd('scxDay', @scxDay);
  InternalAdd('scxDay1', @scxDay1);
  InternalAdd('scxWeekday', @scxWeekday);
  InternalAdd('scxWeekendday', @scxWeekendday);
  InternalAdd('scxNoEndDate', @scxNoEndDate);
  InternalAdd('scxEndAfter', @scxEndAfter);
  InternalAdd('scxEndBy', @scxEndBy);
  InternalAdd('scxOccurences', @scxOccurences);
  InternalAdd('scxAdd1', @scxAdd1);
  InternalAdd('scxAdd1Hint', @scxAdd1Hint);
  InternalAdd('scxEditDotted', @scxEditDotted);
  InternalAdd('scxApply', @scxApply);
  InternalAdd('scxFindAvailableTime', @scxFindAvailableTime);
  InternalAdd('scxOk', @scxOk);
  InternalAdd('scxSaveAndClose', @scxSaveAndClose);
  InternalAdd('scxSaveAndCloseHint', @scxSaveAndCloseHint);
  InternalAdd('scxSave', @scxSave);
  InternalAdd('scxCancel', @scxCancel);
  InternalAdd('scxClose', @scxClose);
  InternalAdd('scxActionClose', @scxActionClose);
  InternalAdd('scxDown', @scxDown);
  InternalAdd('scxDelete1', @scxDelete1);
  InternalAdd('scxDelete1Hint', @scxDelete1Hint);
  InternalAdd('scxEdit1', @scxEdit1);
  InternalAdd('scxImport', @scxImport);
  InternalAdd('scxExport', @scxExport);
  InternalAdd('scxImportHint', @scxImportHint);
  InternalAdd('scxExportHint', @scxExportHint);
  InternalAdd('scxRemoveRecur', @scxRemoveRecur);
  InternalAdd('scxSelectAll', @scxSelectAll);
  InternalAdd('scxSelectNone', @scxSelectNone);
  InternalAdd('scxUp', @scxUp);
  InternalAdd('scxResourceLayoutCaption', @scxResourceLayoutCaption);
  InternalAdd('scxpmNewEvent', @scxpmNewEvent);
  InternalAdd('scxpmNewAllDayEvent', @scxpmNewAllDayEvent);
  InternalAdd('scxpmNewRecurringEvent', @scxpmNewRecurringEvent);
  InternalAdd('scxpmToday', @scxpmToday);
  InternalAdd('scxpmGotoThisDay', @scxpmGotoThisDay);
  InternalAdd('scxpmGoToDate', @scxpmGoToDate);
  InternalAdd('scxpmResourcesLayout', @scxpmResourcesLayout);
  InternalAdd('scxpmOpen', @scxpmOpen);
  InternalAdd('scxpmEditSeries', @scxpmEditSeries);
  InternalAdd('scxpmShowTimeAs', @scxpmShowTimeAs);
  InternalAdd('scxpmDelete', @scxpmDelete);
  InternalAdd('scxpmFree', @scxpmFree);
  InternalAdd('scxpmTentative', @scxpmTentative);
  InternalAdd('scxpmBusy', @scxpmBusy);
  InternalAdd('scxpmOutOfOffice', @scxpmOutOfOffice);
  InternalAdd('scxpmLabel', @scxpmLabel);
  InternalAdd('scxEventLabelNone', @scxEventLabelNone);
  InternalAdd('scxEventLabel0', @scxEventLabel0);
  InternalAdd('scxEventLabel1', @scxEventLabel1);
  InternalAdd('scxEventLabel2', @scxEventLabel2);
  InternalAdd('scxEventLabel3', @scxEventLabel3);
  InternalAdd('scxEventLabel4', @scxEventLabel4);
  InternalAdd('scxEventLabel5', @scxEventLabel5);
  InternalAdd('scxEventLabel6', @scxEventLabel6);
  InternalAdd('scxEventLabel7', @scxEventLabel7);
  InternalAdd('scxEventLabel8', @scxEventLabel8);
  InternalAdd('scxEventLabel9', @scxEventLabel9);
  InternalAdd('scxpmTimeZone', @scxpmTimeZone);
  InternalAdd('scxpm60Minutes', @scxpm60Minutes);
  InternalAdd('scxpm30Minutes', @scxpm30Minutes);
  InternalAdd('scxpm15Minutes', @scxpm15Minutes);
  InternalAdd('scxpm10Minutes', @scxpm10Minutes);
  InternalAdd('scxpm6Minutes', @scxpm6Minutes);
  InternalAdd('scxpm5Minutes', @scxpm5Minutes);
  InternalAdd('scxpmFullYear', @scxpmFullYear);
  InternalAdd('scxpmHalfYear', @scxpmHalfYear);
  InternalAdd('scxpmQuarter', @scxpmQuarter);
  InternalAdd('scxFullYear', @scxFullYear);
  InternalAdd('scxHalfYear', @scxHalfYear);
  InternalAdd('scxQuarter', @scxQuarter);
  InternalAdd('scxHalfYearShort', @scxHalfYearShort);
  InternalAdd('scxQuarterShort', @scxQuarterShort);
  InternalAdd('scxFirstButtonHint', @scxFirstButtonHint);
  InternalAdd('scxPrevPageButtonHint', @scxPrevPageButtonHint);
  InternalAdd('scxPrevButtonHint', @scxPrevButtonHint);
  InternalAdd('scxNextButtonHint', @scxNextButtonHint);
  InternalAdd('scxNextPageButtonHint', @scxNextPageButtonHint);
  InternalAdd('scxLastButtonHint', @scxLastButtonHint);
  InternalAdd('scxShowMoreResourcesButtonHint', @scxShowMoreResourcesButtonHint);
  InternalAdd('scxShowFewerResourcesButtonHint', @scxShowFewerResourcesButtonHint);
  InternalAdd('scxrCaptionReminder', @scxrCaptionReminder);
  InternalAdd('scxrCaptionReminders', @scxrCaptionReminders);
  InternalAdd('scxrDismissButton', @scxrDismissButton);
  InternalAdd('scxrDismissAllButton', @scxrDismissAllButton);
  InternalAdd('scxrDueIn', @scxrDueIn);
  InternalAdd('scxrOpenItemButton', @scxrOpenItemButton);
  InternalAdd('scxrSnoozeButton', @scxrSnoozeButton);
  InternalAdd('scxrSubject', @scxrSubject);
  InternalAdd('scxrSnoozeLabel', @scxrSnoozeLabel);
  InternalAdd('scxrSelected', @scxrSelected);
  InternalAdd('scxrStartTime', @scxrStartTime);
  InternalAdd('scxTime0m', @scxTime0m);
  InternalAdd('scxTime5m', @scxTime5m);
  InternalAdd('scxTime10m', @scxTime10m);
  InternalAdd('scxTime15m', @scxTime15m);
  InternalAdd('scxTime20m', @scxTime20m);
  InternalAdd('scxTime30m', @scxTime30m);
  InternalAdd('scxTime1h', @scxTime1h);
  InternalAdd('scxTime2h', @scxTime2h);
  InternalAdd('scxTime3h', @scxTime3h);
  InternalAdd('scxTime4h', @scxTime4h);
  InternalAdd('scxTime5h', @scxTime5h);
  InternalAdd('scxTime6h', @scxTime6h);
  InternalAdd('scxTime7h', @scxTime7h);
  InternalAdd('scxTime8h', @scxTime8h);
  InternalAdd('scxTime9h', @scxTime9h);
  InternalAdd('scxTime10h', @scxTime10h);
  InternalAdd('scxTime11h', @scxTime11h);
  InternalAdd('scxTime12h', @scxTime12h);
  InternalAdd('scxTime18h', @scxTime18h);
  InternalAdd('scxTime1d', @scxTime1d);
  InternalAdd('scxTime2d', @scxTime2d);
  InternalAdd('scxTime3d', @scxTime3d);
  InternalAdd('scxTime4d', @scxTime4d);
  InternalAdd('scxTime1w', @scxTime1w);
  InternalAdd('scxTime2w', @scxTime2w);
  InternalAdd('scxAdvance0h', @scxAdvance0h);
  InternalAdd('scxAdvance5m', @scxAdvance5m);
  InternalAdd('scxAdvance10m', @scxAdvance10m);
  InternalAdd('scxAdvance15m', @scxAdvance15m);
  InternalAdd('secxExportStorageInvalid', @secxExportStorageInvalid);
  InternalAdd('secxYes', @secxYes);
  InternalAdd('secxNo', @secxNo);
  InternalAdd('secxSubject', @secxSubject);
  InternalAdd('secxLocation', @secxLocation);
  InternalAdd('secxDescription', @secxDescription);
  InternalAdd('secxAllDay', @secxAllDay);
  InternalAdd('secxStart', @secxStart);
  InternalAdd('secxFinish', @secxFinish);
  InternalAdd('secxState', @secxState);
  InternalAdd('secxReminder', @secxReminder);
  InternalAdd('secxTimeRange', @secxTimeRange);
  InternalAdd('secxStartDate', @secxStartDate);
  InternalAdd('secxStartTime', @secxStartTime);
  InternalAdd('secxEndDate', @secxEndDate);
  InternalAdd('secxEndTime', @secxEndTime);
  InternalAdd('secxAlldayevent', @secxAlldayevent);
  InternalAdd('secxReminderonoff', @secxReminderonoff);
  InternalAdd('secxReminderDate', @secxReminderDate);
  InternalAdd('secxReminderTime', @secxReminderTime);

  InternalAdd('secxTrue', @secxTrue);
  InternalAdd('secxFalse', @secxFalse);
  InternalAdd('secxSetDateRangeCaption', @secxSetDateRangeCaption);
  InternalAdd('secxSetDateRangeText', @secxSetDateRangeText);
  InternalAdd('secxSetDateRangeAnd', @secxSetDateRangeAnd);

  InternalAdd('secxCategories', @secxCategories);
  InternalAdd('secxShowtimeas', @secxShowtimeas);
  InternalAdd('scxRequiredFieldsNeeded', @scxRequiredFieldsNeeded);
  InternalAdd('scxInvalidFieldName', @scxInvalidFieldName);
  InternalAdd('scxInvalidCustomField', @scxInvalidCustomField);
  InternalAdd('scxAllDayEventField', @scxAllDayEventField);
  InternalAdd('scxIDField', @scxIDField);
  InternalAdd('scxActualFinishField', @scxActualFinishField);
  InternalAdd('scxActualStartField', @scxActualStartField);
  InternalAdd('scxCaptionField', @scxCaptionField);
  InternalAdd('scxEmptyDayCaption', @scxEmptyDayCaption);
  InternalAdd('scxEnabledField', @scxEnabledField);
  InternalAdd('scxEventTypeField', @scxEventTypeField);
  InternalAdd('scxFinishField', @scxFinishField);
  InternalAdd('scxLabelField', @scxLabelField);
  InternalAdd('scxLocationField', @scxLocationField);
  InternalAdd('scxMessageField', @scxMessageField);
  InternalAdd('scxParentIDField', @scxParentIDField);
  InternalAdd('scxRecurrenceField', @scxRecurrenceField);
  InternalAdd('scxRecurrenceIndexField', @scxRecurrenceIndexField);
  InternalAdd('scxReminderDateField', @scxReminderDateField);
  InternalAdd('scxReminderField', @scxReminderField);
  InternalAdd('scxReminderMinutesBeforeStartField', @scxReminderMinutesBeforeStartField);
  InternalAdd('scxResourceField', @scxResourceField);
  InternalAdd('scxStartField', @scxStartField);
  InternalAdd('scxStateField', @scxStateField);
  InternalAdd('scxTaskCompleteField', @scxTaskCompleteField);
  InternalAdd('scxTaskIndexField', @scxTaskIndexField);
  InternalAdd('scxTaskLinksField', @scxTaskLinksField);
  InternalAdd('scxTaskStatusField', @scxTaskStatusField);
  InternalAdd('scxNotStarted', @scxNotStarted);
  InternalAdd('scxInProgress', @scxInProgress);
  InternalAdd('scxComplete', @scxComplete);
  InternalAdd('scxWaiting', @scxWaiting);
  InternalAdd('scxDeferred', @scxDeferred);
  InternalAdd('scxFinishToStart', @scxFinishToStart);
  InternalAdd('scxStartToStart', @scxStartToStart);
  InternalAdd('scxFinishToFinish', @scxFinishToFinish);
  InternalAdd('scxStartToFinish', @scxStartToFinish);
  InternalAdd('scxFinishToStartShort', @scxFinishToStartShort);
  InternalAdd('scxStartToStartShort', @scxStartToStartShort);
  InternalAdd('scxFinishToFinishShort', @scxFinishToFinishShort);
  InternalAdd('scxStartToFinishShort', @scxStartToFinishShort);
  InternalAdd('scxGanttEventHint', @scxGanttEventHint);
  InternalAdd('scxLinkHint', @scxLinkHint);
  InternalAdd('scxCompleteDisplayFormat', @scxCompleteDisplayFormat);
  InternalAdd('scxNone', @scxNone);
  InternalAdd('scxPattern', @scxPattern);
  InternalAdd('scxOccurrence', @scxOccurrence);
  InternalAdd('scxException', @scxException);
  InternalAdd('scxCustom', @scxCustom);
  InternalAdd('scxHolidaysEditorCaption', @scxHolidaysEditorCaption);
  InternalAdd('scxLocationsGroupBox', @scxLocationsGroupBox);
  InternalAdd('scxHolidaysGroupBox', @scxHolidaysGroupBox);
  InternalAdd('scxAddedHolidaysGroupBox', @scxAddedHolidaysGroupBox);
  InternalAdd('scxLocationName', @scxLocationName);
  InternalAdd('scxHolidaysLocationEditorCaption', @scxHolidaysLocationEditorCaption);
  InternalAdd('scxHolidayName', @scxHolidayName);
  InternalAdd('scxHolidayDate', @scxHolidayDate);
  InternalAdd('scxHolidaysLocationHolidayEditorCaption', @scxHolidaysLocationHolidayEditorCaption);
  InternalAdd('scxOutlookFormatMismatch', @scxOutlookFormatMismatch);
  InternalAdd('scxHolidayDisplayFormat', @scxHolidayDisplayFormat);
  InternalAdd('scxAddedHolidayDisplayFormat', @scxAddedHolidayDisplayFormat);
  InternalAdd('scxGroupIDField', @scxGroupIDField);
  InternalAdd('scxAppointment', @scxAppointment);
  InternalAdd('scxActions', @scxActions);
  InternalAdd('scxOptions', @scxOptions);
end;

initialization
  cxSchedulerInitStrings;
  dxResourceStringsRepository.RegisterProduct('ExpressScheduler', @AddcxSchedulerResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressScheduler');

end.
