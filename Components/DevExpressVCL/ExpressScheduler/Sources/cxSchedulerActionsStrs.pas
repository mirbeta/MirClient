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

unit cxSchedulerActionsStrs;

{$I cxVer.inc}

interface

resourcestring
  // New Event
  sdxSchedulerActionNewEventCaption = 'New Event';
  sdxSchedulerActionNewEventHint = 'New Event';
  sdxSchedulerActionNewRecurringEventCaption = 'New Recurring Event';
  sdxSchedulerActionNewRecurringEventHint = 'New Recurring Event';
  // Navigation
  sdxSchedulerActionGoBackwardCaption = 'Go Backward';
  sdxSchedulerActionGoBackwardHint = 'Go Backward';
  sdxSchedulerActionGoForwardCaption = 'Go Forward';
  sdxSchedulerActionGoForwardHint = 'Go Forward';
  sdxSchedulerActionGoToTodayCaption = 'Go to Today';
  sdxSchedulerActionGoToTodayHint = 'Go to Today';
  sdxSchedulerActionGoToDateCaption = 'Go to Date';
  sdxSchedulerActionGoToDateHint = 'Go to Date';
  sdxSchedulerActionNextSevenDaysCaption = 'Next 7 Days';
  sdxSchedulerActionNextSevenDaysHint = 'Next 7 Days';
  // Arrange
  sdxSchedulerActionAgendaViewCaption = 'Agenda';
  sdxSchedulerActionAgendaViewHint = 'Agenda';
  sdxSchedulerActionDayViewCaption = 'Day';
  sdxSchedulerActionDayViewHint = 'Day';
  sdxSchedulerActionWorkWeekViewCaption = 'Work Week';
  sdxSchedulerActionWorkWeekViewHint = 'Work Week';
  sdxSchedulerActionWeekViewCaption = 'Week';
  sdxSchedulerActionWeekViewHint = 'Week';
  sdxSchedulerActionMonthViewCaption = 'Month';
  sdxSchedulerActionMonthViewHint = 'Month';
  sdxSchedulerActionTimeGridViewCaption = 'Timeline';
  sdxSchedulerActionTimeGridViewHint = 'Timeline';
  sdxSchedulerActionYearViewCaption = 'Year';
  sdxSchedulerActionYearViewHint = 'Year';
  sdxSchedulerActionGanttViewCaption = 'Gantt View';
  sdxSchedulerActionGanttViewHint = 'Gantt View';
  // Group By
  sdxSchedulerActionGroupByNoneCaption = 'Group By None';
  sdxSchedulerActionGroupByNoneHint = 'Group By None';
  sdxSchedulerActionGroupByDateCaption = 'Group By Date';
  sdxSchedulerActionGroupByDateHint = 'Group By Date';
  sdxSchedulerActionGroupByResourceCaption = 'Group By Resource';
  sdxSchedulerActionGroupByResourceHint = 'Group By Resource';
  // Time Scale
  sdxSchedulerActionTimeScale60MinutesCaption = '60 Minutes';
  sdxSchedulerActionTimeScale60MinutesHint = '60 Minutes';
  sdxSchedulerActionTimeScale30MinutesCaption = '30 Minutes';
  sdxSchedulerActionTimeScale30MinutesHint = '30 Minutes';
  sdxSchedulerActionTimeScale15MinutesCaption = '15 Minutes';
  sdxSchedulerActionTimeScale15MinutesHint = '15 Minutes';
  sdxSchedulerActionTimeScale10MinutesCaption = '10 Minutes';
  sdxSchedulerActionTimeScale10MinutesHint = '10 Minutes';
  sdxSchedulerActionTimeScale6MinutesCaption = '6 Minutes';
  sdxSchedulerActionTimeScale6MinutesHint = '6 Minutes';
  sdxSchedulerActionTimeScale5MinutesCaption = '5 Minutes';
  sdxSchedulerActionTimeScale5MinutesHint = '5 Minutes';
  // Layout
  sdxSchedulerActionCompressWeekendsCaption = 'Compress Weekends';
  sdxSchedulerActionCompressWeekendsHint = 'Compress Weekends';
  sdxSchedulerActionWorkTimeOnlyCaption = 'Working Hours';
  sdxSchedulerActionWorkTimeOnlyHint = 'Working Hours';
  sdxSchedulerActionSnapEventsToTimeSlotsCaption = 'Snap Events To Time Slots';
  sdxSchedulerActionSnapEventsToTimeSlotsHint = 'Snap Events To Time Slots';
  sdxSchedulerActionDateNavigatorCaption = 'Date Navigator';
  sdxSchedulerActionDateNavigatorHint = 'Date Navigator';
  sdxSchedulerActionResourcesLayoutEditorCaption = 'Resources Layout Editor';
  sdxSchedulerActionResourcesLayoutEditorHint = 'Resources Layout Editor';
  // Print
  sdxSchedulerActionPageSetupCaption = 'Page Setup';
  sdxSchedulerActionPageSetupHint = 'Page Setup';
  sdxSchedulerActionPrintCaption = '&Print';
  sdxSchedulerActionPrintHint = 'Print';
  sdxSchedulerActionPrintPreviewCaption = 'Print Pre&view';
  sdxSchedulerActionPrintPreviewHint = 'Print Preview';

implementation

uses
  dxCore;

procedure AddSchedulerActionsResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  // New Event
  AProduct.Add('sdxSchedulerActionNewEventCaption', @sdxSchedulerActionNewEventCaption);
  AProduct.Add('sdxSchedulerActionNewEventHint', @sdxSchedulerActionNewEventHint);
  AProduct.Add('sdxSchedulerActionNewRecurringEventCaption', @sdxSchedulerActionNewRecurringEventCaption);
  AProduct.Add('sdxSchedulerActionNewRecurringEventHint', @sdxSchedulerActionNewRecurringEventHint);
  // Navigation
  AProduct.Add('sdxSchedulerActionGoBackwardCaption', @sdxSchedulerActionGoBackwardCaption);
  AProduct.Add('sdxSchedulerActionGoBackwardHint', @sdxSchedulerActionGoBackwardHint);
  AProduct.Add('sdxSchedulerActionGoForwardCaption', @sdxSchedulerActionGoForwardCaption);
  AProduct.Add('sdxSchedulerActionGoForwardHint', @sdxSchedulerActionGoForwardHint);
  AProduct.Add('sdxSchedulerActionGoToTodayCaption', @sdxSchedulerActionGoToTodayCaption);
  AProduct.Add('sdxSchedulerActionGoToTodayHint', @sdxSchedulerActionGoToTodayHint);
  AProduct.Add('sdxSchedulerActionGoToDateCaption', @sdxSchedulerActionGoToDateCaption);
  AProduct.Add('sdxSchedulerActionGoToDateHint', @sdxSchedulerActionGoToDateHint);
  AProduct.Add('sdxSchedulerActionNextSevenDaysCaption', @sdxSchedulerActionNextSevenDaysCaption);
  AProduct.Add('sdxSchedulerActionNextSevenDaysHint', @sdxSchedulerActionNextSevenDaysHint);
  // Arrange
  AProduct.Add('sdxSchedulerActionAgendaViewCaption', @sdxSchedulerActionAgendaViewCaption);
  AProduct.Add('sdxSchedulerActionAgendaViewHint', @sdxSchedulerActionAgendaViewHint);
  AProduct.Add('sdxSchedulerActionDayViewCaption', @sdxSchedulerActionDayViewCaption);
  AProduct.Add('sdxSchedulerActionDayViewHint', @sdxSchedulerActionDayViewHint);
  AProduct.Add('sdxSchedulerActionWorkWeekViewCaption', @sdxSchedulerActionWorkWeekViewCaption);
  AProduct.Add('sdxSchedulerActionWorkWeekViewHint', @sdxSchedulerActionWorkWeekViewHint);
  AProduct.Add('sdxSchedulerActionWeekViewCaption', @sdxSchedulerActionWeekViewCaption);
  AProduct.Add('sdxSchedulerActionWeekViewHint', @sdxSchedulerActionWeekViewHint);
  AProduct.Add('sdxSchedulerActionMonthViewCaption', @sdxSchedulerActionMonthViewCaption);
  AProduct.Add('sdxSchedulerActionMonthViewHint', @sdxSchedulerActionMonthViewHint);
  AProduct.Add('sdxSchedulerActionTimeGridViewCaption', @sdxSchedulerActionTimeGridViewCaption);
  AProduct.Add('sdxSchedulerActionTimeGridViewHint', @sdxSchedulerActionTimeGridViewHint);
  AProduct.Add('sdxSchedulerActionYearViewCaption', @sdxSchedulerActionYearViewCaption);
  AProduct.Add('sdxSchedulerActionYearViewHint', @sdxSchedulerActionYearViewHint);
  AProduct.Add('sdxSchedulerActionGanttViewCaption', @sdxSchedulerActionGanttViewCaption);
  AProduct.Add('sdxSchedulerActionGanttViewHint', @sdxSchedulerActionGanttViewHint);
  // Group By
  AProduct.Add('sdxSchedulerActionGroupByNoneCaption', @sdxSchedulerActionGroupByNoneCaption);
  AProduct.Add('sdxSchedulerActionGroupByNoneHint', @sdxSchedulerActionGroupByNoneHint);
  AProduct.Add('sdxSchedulerActionGroupByDateCaption', @sdxSchedulerActionGroupByDateCaption);
  AProduct.Add('sdxSchedulerActionGroupByDateHint', @sdxSchedulerActionGroupByDateHint);
  AProduct.Add('sdxSchedulerActionGroupByResourceCaption', @sdxSchedulerActionGroupByResourceCaption);
  AProduct.Add('sdxSchedulerActionGroupByResourceHint', @sdxSchedulerActionGroupByResourceHint);
  // Time Scale
  AProduct.Add('sdxSchedulerActionTimeScale60MinutesCaption', @sdxSchedulerActionTimeScale60MinutesCaption);
  AProduct.Add('sdxSchedulerActionTimeScale60MinutesHint', @sdxSchedulerActionTimeScale60MinutesHint);
  AProduct.Add('sdxSchedulerActionTimeScale30MinutesCaption', @sdxSchedulerActionTimeScale30MinutesCaption);
  AProduct.Add('sdxSchedulerActionTimeScale30MinutesHint', @sdxSchedulerActionTimeScale30MinutesHint);
  AProduct.Add('sdxSchedulerActionTimeScale15MinutesCaption', @sdxSchedulerActionTimeScale15MinutesCaption);
  AProduct.Add('sdxSchedulerActionTimeScale15MinutesHint', @sdxSchedulerActionTimeScale15MinutesHint);
  AProduct.Add('sdxSchedulerActionTimeScale10MinutesCaption', @sdxSchedulerActionTimeScale10MinutesCaption);
  AProduct.Add('sdxSchedulerActionTimeScale10MinutesHint', @sdxSchedulerActionTimeScale10MinutesHint);
  AProduct.Add('sdxSchedulerActionTimeScale6MinutesCaption', @sdxSchedulerActionTimeScale6MinutesCaption);
  AProduct.Add('sdxSchedulerActionTimeScale6MinutesHint', @sdxSchedulerActionTimeScale6MinutesHint);
  AProduct.Add('sdxSchedulerActionTimeScale5MinutesCaption', @sdxSchedulerActionTimeScale5MinutesCaption);
  AProduct.Add('sdxSchedulerActionTimeScale5MinutesHint', @sdxSchedulerActionTimeScale5MinutesHint);
  // Layout
  AProduct.Add('sdxSchedulerActionCompressWeekendsCaption', @sdxSchedulerActionCompressWeekendsCaption);
  AProduct.Add('sdxSchedulerActionCompressWeekendsHint', @sdxSchedulerActionCompressWeekendsHint);
  AProduct.Add('sdxSchedulerActionWorkTimeOnlyCaption', @sdxSchedulerActionWorkTimeOnlyCaption);
  AProduct.Add('sdxSchedulerActionWorkTimeOnlyHint', @sdxSchedulerActionWorkTimeOnlyHint);
  AProduct.Add('sdxSchedulerActionSnapEventsToTimeSlotsCaption', @sdxSchedulerActionSnapEventsToTimeSlotsCaption);
  AProduct.Add('sdxSchedulerActionSnapEventsToTimeSlotsHint', @sdxSchedulerActionSnapEventsToTimeSlotsHint);
  AProduct.Add('sdxSchedulerActionDateNavigatorCaption', @sdxSchedulerActionDateNavigatorCaption);
  AProduct.Add('sdxSchedulerActionDateNavigatorHint', @sdxSchedulerActionDateNavigatorHint);
  AProduct.Add('sdxSchedulerActionResourcesLayoutEditorCaption', @sdxSchedulerActionResourcesLayoutEditorCaption);
  AProduct.Add('sdxSchedulerActionResourcesLayoutEditorHint', @sdxSchedulerActionResourcesLayoutEditorHint);
  // Print
  AProduct.Add('sdxSchedulerActionPageSetupCaption', @sdxSchedulerActionPageSetupCaption);
  AProduct.Add('sdxSchedulerActionPageSetupHint', @sdxSchedulerActionPageSetupHint);
  AProduct.Add('sdxSchedulerActionPrintCaption', @sdxSchedulerActionPrintCaption);
  AProduct.Add('sdxSchedulerActionPrintHint', @sdxSchedulerActionPrintHint);
  AProduct.Add('sdxSchedulerActionPrintPreviewCaption', @sdxSchedulerActionPrintPreviewCaption);
  AProduct.Add('sdxSchedulerActionPrintPreviewHint', @sdxSchedulerActionPrintPreviewHint);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressScheduler', @AddSchedulerActionsResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressScheduler', @AddSchedulerActionsResourceStringNames);

end.
