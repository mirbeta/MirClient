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

unit cxSchedulerUIGeneratorScheme;

{$I cxVer.inc}

interface

const
  sdxSchedulerActionsCategoryName = 'DevExpress ExpressScheduler';

const
  sdxSchedulerTabFile = 'File';
  sdxSchedulerBarFilePrint = 'Print';

  sdxSchedulerTabHome = 'Home';
  sdxSchedulerBarHomeEvent = 'Event';
  sdxSchedulerBarHomeNavigation = 'Navigation';
  sdxSchedulerBarHomeArrange = 'Arrange';
  sdxSchedulerBarHomeGroupBy = 'Group By';

  sdxSchedulerTabView = 'View';
  sdxSchedulerBarViewTimeScale = 'Time Scale';
  sdxSchedulerSubItemViewTimeScaleTimeScale = 'Time Scale';
  sdxSchedulerBarViewLayout = 'Layout';

procedure RegisterSchedulerUIGeneratorScheme;

implementation

uses
  dxUIGenerator, cxScheduler, cxSchedulerActions;

procedure RegisterCategoryEvent(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSchedulerNewEvent);
  ACategory.Add(TdxSchedulerNewRecurringEvent);
end;

procedure RegisterCategoryNavigation(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSchedulerGoBackward);
  ACategory.Add(TdxSchedulerGoForward);

  ACategory.Add(TdxSchedulerGoToToday, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);
  ACategory.Add(TdxSchedulerGoToDate);

  ACategory.Add(TdxSchedulerNextSevenDays, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);
end;

procedure RegisterCategoryArrange(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSchedulerDayView);
  ACategory.Add(TdxSchedulerWorkWeekView);
  ACategory.Add(TdxSchedulerWeekView);
  ACategory.Add(TdxSchedulerMonthView);
  ACategory.Add(TdxSchedulerTimeGridView);
  ACategory.Add(TdxSchedulerYearView);
  ACategory.Add(TdxSchedulerGanttView);
  ACategory.Add(TdxSchedulerAgendaView);
end;

procedure RegisterCategoryGroupBy(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSchedulerGroupByNone);
  ACategory.Add(TdxSchedulerGroupByDate);
  ACategory.Add(TdxSchedulerGroupByResource);
end;

procedure RegisterCategoryTimeScale(ACategory: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategory.Add('Scheduling\SwitchTimeScalesTo.png', sdxSchedulerSubItemViewTimeScaleTimeScale);
  ACommand.Add(TdxSchedulerTimeScale60Minutes);
  ACommand.Add(TdxSchedulerTimeScale30Minutes);
  ACommand.Add(TdxSchedulerTimeScale15Minutes);
  ACommand.Add(TdxSchedulerTimeScale10Minutes);
  ACommand.Add(TdxSchedulerTimeScale6Minutes);
  ACommand.Add(TdxSchedulerTimeScale5Minutes);
end;

procedure RegisterCategoryLayout(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSchedulerCompressWeekends);
  ACategory.Add(TdxSchedulerWorkTimeOnly);
  ACategory.Add(TdxSchedulerSnapEventsToTimeSlots);

  ACategory.Add(TdxSchedulerDateNavigator, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);
  ACategory.Add(TdxSchedulerResourcesLayoutEditor);
end;

procedure RegisterSchedulerUIGeneratorScheme;
var
  AComponent: TdxUIGeneratorComponentInfo;
begin
  AComponent := TdxUIGenerator.RegisterComponent(TcxScheduler, sdxSchedulerActionsCategoryName);

  RegisterCategoryEvent(AComponent.Add(sdxSchedulerTabHome, sdxSchedulerBarHomeEvent, 'Scheduling\Appointment_16x16.png', 0));
  RegisterCategoryNavigation(AComponent.Add(sdxSchedulerTabHome, sdxSchedulerBarHomeNavigation, 'Scheduling\GoToDate_16x16.png', 0));
  RegisterCategoryArrange(AComponent.Add(sdxSchedulerTabHome, sdxSchedulerBarHomeArrange, 'Scheduling\FullWeekView_16x16.png', 0));
  RegisterCategoryGroupBy(AComponent.Add(sdxSchedulerTabHome, sdxSchedulerBarHomeGroupBy, 'Scheduling\GroupByDate_16x16.png', 0));

  RegisterCategoryTimeScale(AComponent.Add(sdxSchedulerTabView, sdxSchedulerBarViewTimeScale, 'Scheduling\SwitchTimeScalesTo_16x16.png', 1));
  RegisterCategoryLayout(AComponent.Add(sdxSchedulerTabView, sdxSchedulerBarViewLayout, 'Scheduling\ResourcesLayoutEditor_16x16.png', 1));
end;

end.
