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

unit cxSchedulerActions;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, dxActions, dxPrinting, cxScheduler, cxSchedulerCustomControls;

type

  { TdxSchedulerAction }

  TdxSchedulerAction = class(TdxCustomAction)
  strict private
    function GetControl: TcxScheduler;
  protected
    procedure SetControl(Value: TcxScheduler); reintroduce;
    //
    procedure DoExecute; virtual;
    procedure DoUpdateState; override;
    function IsEnabled: Boolean; virtual;
    procedure DoResetState; override;
    procedure UpdateControl(Target: TObject); override;
    //
    property Control: TcxScheduler read GetControl write SetControl;
  public
    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
  end;

  { TdxSchedulerCustomNewEventAction }

  TdxSchedulerCustomNewEventAction = class(TdxSchedulerAction)
  protected
    FIsRecurringEvent: Boolean;

    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  end;

  { TdxSchedulerNewEvent }

  TdxSchedulerNewEvent = class(TdxSchedulerCustomNewEventAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerNewRecurringEvent }

  TdxSchedulerNewRecurringEvent = class(TdxSchedulerCustomNewEventAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerGoBackward }

  TdxSchedulerGoBackward = class(TdxSchedulerAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerGoForward }

  TdxSchedulerGoForward = class(TdxSchedulerAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerGoToToday }

  TdxSchedulerGoToToday = class(TdxSchedulerAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerGoToDate }

  TdxSchedulerGoToDate = class(TdxSchedulerAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerNextSevenDays }

  TdxSchedulerNextSevenDays = class(TdxSchedulerAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerCustomViewAction }

  TdxSchedulerCustomViewAction = class(TdxSchedulerAction)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSchedulerAgendaView }

  TdxSchedulerAgendaView = class(TdxSchedulerCustomViewAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerDayView }

  TdxSchedulerDayView = class(TdxSchedulerCustomViewAction)
  strict private
    function IsWorkWeekActive: Boolean;
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerWorkWeekView }

  TdxSchedulerWorkWeekView = class(TdxSchedulerCustomViewAction)
  strict private
    function IsWorkWeekActive: Boolean;
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerWeekView }

  TdxSchedulerWeekView = class(TdxSchedulerCustomViewAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerMonthView }

  TdxSchedulerMonthView = class(TdxSchedulerCustomViewAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerTimeGridView }

  TdxSchedulerTimeGridView = class(TdxSchedulerCustomViewAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerYearView }

  TdxSchedulerYearView = class(TdxSchedulerCustomViewAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerGanttView }

  TdxSchedulerGanttView = class(TdxSchedulerCustomViewAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerCustomGroupByAction }

  TdxSchedulerCustomGroupByAction = class(TdxSchedulerAction)
  protected
    FGroupingKind: TcxSchedulerGroupingKind;

    procedure DoExecute; override;
    procedure DoUpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSchedulerGroupByNone }

  TdxSchedulerGroupByNone = class(TdxSchedulerCustomGroupByAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerGroupByDate }

  TdxSchedulerGroupByDate = class(TdxSchedulerCustomGroupByAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerGroupByResource }

  TdxSchedulerGroupByResource = class(TdxSchedulerCustomGroupByAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerCustomTimeScaleAction }

  TdxSchedulerCustomTimeScaleAction = class(TdxSchedulerAction)
  protected
    FTimeScaleStep: Integer;

    procedure DoExecute; override;
    procedure DoUpdateState; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSchedulerTimeScale60Minutes }

  TdxSchedulerTimeScale60Minutes = class(TdxSchedulerCustomTimeScaleAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerTimeScale30Minutes }

  TdxSchedulerTimeScale30Minutes = class(TdxSchedulerCustomTimeScaleAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerTimeScale15Minutes }

  TdxSchedulerTimeScale15Minutes = class(TdxSchedulerCustomTimeScaleAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerTimeScale10Minutes }

  TdxSchedulerTimeScale10Minutes = class(TdxSchedulerCustomTimeScaleAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerTimeScale6Minutes }

  TdxSchedulerTimeScale6Minutes = class(TdxSchedulerCustomTimeScaleAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerTimeScale5Minutes }

  TdxSchedulerTimeScale5Minutes = class(TdxSchedulerCustomTimeScaleAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerCompressWeekends }

  TdxSchedulerCompressWeekends = class(TdxSchedulerAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSchedulerWorkTimeOnly }

  TdxSchedulerWorkTimeOnly = class(TdxSchedulerAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSchedulerSnapEventsToTimeSlots }

  TdxSchedulerSnapEventsToTimeSlots = class(TdxSchedulerAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSchedulerDateNavigator }

  TdxSchedulerDateNavigator = class(TdxSchedulerAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSchedulerResourcesLayoutEditor }

  TdxSchedulerResourcesLayoutEditor = class(TdxSchedulerAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerShowPageSetupForm }

  TdxSchedulerShowPageSetupForm = class(TdxCustomShowPageSetupFormAction)
  protected
    function GetControlClass: TWinControlClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerShowPrintForm }

  TdxSchedulerShowPrintForm = class(TdxCustomShowPrintFormAction)
  protected
    function GetControlClass: TWinControlClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSchedulerShowPrintPreviewForm }

  TdxSchedulerShowPrintPreviewForm = class(TdxCustomShowPrintPreviewFormAction)
  protected
    function GetControlClass: TWinControlClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  cxDateUtils, cxSchedulerActionsStrs, cxSchedulerDialogs, cxSchedulerStorage, cxSchedulerDayView, cxSchedulerWeekView,
  cxSchedulerTimeGridView, cxSchedulerGanttView, cxSchedulerYearView, cxSchedulerAgendaView,
  cxSchedulerCustomResourceView;

type
  TcxCustomSchedulerStorageAccess = class(TcxCustomSchedulerStorage);

{ TdxSchedulerAction }

procedure TdxSchedulerAction.ExecuteTarget(Target: TObject);
begin
  UpdateControl(Target);
  if Control <> nil then
    DoExecute;
end;

function TdxSchedulerAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (inherited HandlesTarget(Target) or (Target is TcxScheduler)) and
    (not NeedControlFocus or TcxScheduler(Target).Focused);
end;

procedure TdxSchedulerAction.SetControl(Value: TcxScheduler);
begin
  inherited SetControl(Value);
end;

procedure TdxSchedulerAction.DoExecute;
begin
  //do nothing
end;

procedure TdxSchedulerAction.DoUpdateState;
begin
  Enabled := IsEnabled;
end;

function TdxSchedulerAction.IsEnabled: Boolean;
begin
  Result := Control.CanFocusEx and (Control.Storage <> nil);
end;

procedure TdxSchedulerAction.DoResetState;
begin
  Enabled := True;
  Checked := False;
end;

procedure TdxSchedulerAction.UpdateControl(Target: TObject);
begin
  if Target is TcxScheduler then
    Control := TcxScheduler(Target);
end;

function TdxSchedulerAction.GetControl: TcxScheduler;
begin
  Result := TcxScheduler(inherited Control);
end;

{ TdxSchedulerCustomNewEventAction }

procedure TdxSchedulerCustomNewEventAction.DoExecute;
begin
  Control.CreateEventUsingDialog(False, FIsRecurringEvent);
end;

function TdxSchedulerCustomNewEventAction.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and Control.EventOperations.Creating and Control.EventOperations.DialogEditing and
    Control.EventOperations.DialogShowing;
end;

{ TdxSchedulerNewEvent }

constructor TdxSchedulerNewEvent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionNewEventCaption;
  FDefaultHintResString := @sdxSchedulerActionNewEventHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\Appointment.png';
  FIsRecurringEvent := False;
end;

{ TdxSchedulerNewRecurringEvent }

constructor TdxSchedulerNewRecurringEvent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionNewRecurringEventCaption;
  FDefaultHintResString := @sdxSchedulerActionNewRecurringEventHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\RecurringAppointment.png';
  FIsRecurringEvent := True;
end;

{ TdxSchedulerGoBackward }

constructor TdxSchedulerGoBackward.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionGoBackwardCaption;
  FDefaultHintResString := @sdxSchedulerActionGoBackwardHint;
  FDefaultImageNameInIconLibrary := 'Navigation\Backward.png';
end;

procedure TdxSchedulerGoBackward.DoExecute;
begin
  Control.GoToDate(Control.SelStart - 1);
end;

function TdxSchedulerGoBackward.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (Control.CurrentView is TcxSchedulerDayView) and (Control.SelectedDays.Count = 1);
end;

{ TdxSchedulerGoForward }

constructor TdxSchedulerGoForward.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionGoForwardCaption;
  FDefaultHintResString := @sdxSchedulerActionGoForwardHint;
  FDefaultImageNameInIconLibrary := 'Navigation\Forward.png';
end;

procedure TdxSchedulerGoForward.DoExecute;
begin
  Control.GoToDate(Control.SelStart + 1);
end;

function TdxSchedulerGoForward.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (Control.CurrentView is TcxSchedulerDayView) and (Control.SelectedDays.Count = 1);
end;

{ TdxSchedulerGoToToday }

constructor TdxSchedulerGoToToday.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionGoToTodayCaption;
  FDefaultHintResString := @sdxSchedulerActionGoToTodayHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\Today.png';
end;

procedure TdxSchedulerGoToToday.DoExecute;
begin
  Control.GoToDate(Date);
end;

{ TdxSchedulerGoToDate }

constructor TdxSchedulerGoToDate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionGoToDateCaption;
  FDefaultHintResString := @sdxSchedulerActionGoToDateHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\GoToDate.png';
end;

procedure TdxSchedulerGoToDate.DoExecute;
var
  ADate: TDateTime;
  AViewMode: TcxSchedulerViewMode;
begin
  ADate := dxDateOf(Control.SelStart);
  if cxShowGoToDateDialog(Control, Control.DialogsLookAndFeel, ADate, AViewMode) then
    Control.GoToDate(ADate, AViewMode);
end;

{ TdxSchedulerNextSevenDays }

constructor TdxSchedulerNextSevenDays.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionNextSevenDaysCaption;
  FDefaultHintResString := @sdxSchedulerActionNextSevenDaysHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\NextSevenDays.png';
end;

procedure TdxSchedulerNextSevenDays.DoExecute;
begin
  Control.GoToDate(Date, vmDay);
  Control.SelectDays(Date, Date + 6, True);
end;

{ TdxSchedulerCustomViewAction }

constructor TdxSchedulerCustomViewAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
end;

{ TdxSchedulerAgendaView }

constructor TdxSchedulerAgendaView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionAgendaViewCaption;
  FDefaultHintResString := @sdxSchedulerActionAgendaViewHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\AgendaView.png';
end;

procedure TdxSchedulerAgendaView.DoExecute;
begin
  Control.GoToDate(Trunc(Control.SelStart), vmAgenda);
  Control.LayoutChanged;
end;

procedure TdxSchedulerAgendaView.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := Control.CurrentView is TcxSchedulerAgendaView;
end;

{ TdxSchedulerDayView }

constructor TdxSchedulerDayView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionDayViewCaption;
  FDefaultHintResString := @sdxSchedulerActionDayViewHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\DayView.png';
end;

procedure TdxSchedulerDayView.DoExecute;
begin
  Control.GoToDate(Trunc(Control.SelStart), vmDay);
  Control.LayoutChanged;
end;

procedure TdxSchedulerDayView.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := (Control.CurrentView is TcxSchedulerDayView) and not IsWorkWeekActive;
end;

function TdxSchedulerDayView.IsWorkWeekActive: Boolean;
var
  ADays: TDays;
  I: Integer;
begin
  Result := Control.SelectedDays.Count < 7;
  if Result then
  begin
    ADays := [];
    for I := 0 to Control.SelectedDays.Count - 1 do
      ADays := ADays + [TDay(DayOfWeek(Control.SelectedDays[I]) - 1)];
    Result := (ADays * Control.OptionsView.WorkDays) = Control.OptionsView.WorkDays;
  end;
end;

{ TdxSchedulerWorkWeekView }

constructor TdxSchedulerWorkWeekView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionWorkWeekViewCaption;
  FDefaultHintResString := @sdxSchedulerActionWorkWeekViewHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\WorkWeekView.png';
end;

procedure TdxSchedulerWorkWeekView.DoExecute;
begin
  Control.GoToDate(Trunc(Control.SelStart), vmWorkWeek);
  Control.LayoutChanged;
end;

procedure TdxSchedulerWorkWeekView.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := (Control.CurrentView is TcxSchedulerDayView) and IsWorkWeekActive;
end;

function TdxSchedulerWorkWeekView.IsWorkWeekActive: Boolean;
var
  ADays: TDays;
  I: Integer;
begin
  Result := Control.SelectedDays.Count < 7;
  if Result then
  begin
    ADays := [];
    for I := 0 to Control.SelectedDays.Count - 1 do
      ADays := ADays + [TDay(DayOfWeek(Control.SelectedDays[I]) - 1)];
    Result := (ADays * Control.OptionsView.WorkDays) = Control.OptionsView.WorkDays;
  end;
end;

{ TdxSchedulerWeekView }

constructor TdxSchedulerWeekView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionWeekViewCaption;
  FDefaultHintResString := @sdxSchedulerActionWeekViewHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\WeekView.png';
end;

procedure TdxSchedulerWeekView.DoExecute;
begin
  Control.GoToDate(Trunc(Control.SelStart), vmWeek);
  Control.LayoutChanged;
end;

procedure TdxSchedulerWeekView.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := Control.CurrentView is TcxSchedulerWeekView;
end;

{ TdxSchedulerMonthView }

constructor TdxSchedulerMonthView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionMonthViewCaption;
  FDefaultHintResString := @sdxSchedulerActionMonthViewHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\MonthView.png';
end;

procedure TdxSchedulerMonthView.DoExecute;
begin
  Control.GoToDate(Trunc(Control.SelStart), vmMonth);
  Control.LayoutChanged;
end;

procedure TdxSchedulerMonthView.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := Control.CurrentView is TcxSchedulerWeeksView;
end;

{ TdxSchedulerTimeGridView }

constructor TdxSchedulerTimeGridView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionTimeGridViewCaption;
  FDefaultHintResString := @sdxSchedulerActionTimeGridViewHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\TimeLineView.png';
end;

procedure TdxSchedulerTimeGridView.DoExecute;
begin
  Control.SelectedDays.Clear;
  Control.SelectedDays.Add(Trunc(Control.SelStart));
  Control.ViewTimeGrid.Active := True;
  Control.LayoutChanged;
end;

procedure TdxSchedulerTimeGridView.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := (Control.CurrentView is TcxSchedulerTimeGridView) and not (Control.CurrentView is TcxSchedulerGanttView);
end;

{ TdxSchedulerYearView }

constructor TdxSchedulerYearView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionYearViewCaption;
  FDefaultHintResString := @sdxSchedulerActionYearViewHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\YearView.png';
end;

procedure TdxSchedulerYearView.DoExecute;
begin
  Control.ViewYear.Active := True;
  Control.LayoutChanged;
end;

procedure TdxSchedulerYearView.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := Control.CurrentView is TcxSchedulerYearView;
end;

{ TdxSchedulerGanttView }

constructor TdxSchedulerGanttView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionGanttViewCaption;
  FDefaultHintResString := @sdxSchedulerActionGanttViewHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\GanttView.png';
end;

procedure TdxSchedulerGanttView.DoExecute;
begin
  Control.ViewGantt.Active := True;
  Control.ViewGantt.VisibleStart := Control.SelStart;
  Control.LayoutChanged;
end;

procedure TdxSchedulerGanttView.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := Control.CurrentView is TcxSchedulerGanttView;
end;

{ TdxSchedulerCustomGroupByAction }

constructor TdxSchedulerCustomGroupByAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
end;

procedure TdxSchedulerCustomGroupByAction.DoExecute;
begin
  if Checked then
    Control.OptionsView.GroupingKind := FGroupingKind
  else
    Control.OptionsView.GroupingKind := gkDefault;
end;

procedure TdxSchedulerCustomGroupByAction.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := Control.OptionsView.GroupingKind = FGroupingKind;
end;

{ TdxSchedulerGroupByNone }

constructor TdxSchedulerGroupByNone.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionGroupByNoneCaption;
  FDefaultHintResString := @sdxSchedulerActionGroupByNoneHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\GroupByNone.png';
  FGroupingKind := gkNone;
end;

{ TdxSchedulerGroupByDate }

constructor TdxSchedulerGroupByDate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionGroupByDateCaption;
  FDefaultHintResString := @sdxSchedulerActionGroupByDateHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\GroupByDate.png';
  FGroupingKind := gkByDate;
end;

{ TdxSchedulerGroupByResource }

constructor TdxSchedulerGroupByResource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionGroupByResourceCaption;
  FDefaultHintResString := @sdxSchedulerActionGroupByResourceHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\GroupByResource.png';
  FGroupingKind := gkByResource;
end;

{ TdxSchedulerCustomTimeScaleAction }

constructor TdxSchedulerCustomTimeScaleAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
end;

procedure TdxSchedulerCustomTimeScaleAction.DoExecute;
var
  AIntf: IcxSchedulerViewTimeScaleStep;
begin
  if Supports(Control.CurrentView, IcxSchedulerViewTimeScaleStep, AIntf) then
    AIntf.SetTimeScaleStep(FTimeScaleStep);
end;

procedure TdxSchedulerCustomTimeScaleAction.DoUpdateState;
var
  AChecked: Boolean;
  AIntf: IcxSchedulerViewTimeScaleStep;
begin
  inherited DoUpdateState;
  AChecked := Enabled;
  if Supports(Control.CurrentView, IcxSchedulerViewTimeScaleStep, AIntf) then
    AChecked := AChecked and (AIntf.GetTimeScaleStep = FTimeScaleStep);
  Checked := AChecked;
end;

function TdxSchedulerCustomTimeScaleAction.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and Supports(Control.CurrentView, IcxSchedulerViewTimeScaleStep);
end;

{ TdxSchedulerTimeScale60Minutes }

constructor TdxSchedulerTimeScale60Minutes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionTimeScale60MinutesCaption;
  FDefaultHintResString := @sdxSchedulerActionTimeScale60MinutesHint;
  FDefaultImageNameInIconLibrary := '';
  FTimeScaleStep := 60;
end;

{ TdxSchedulerTimeScale30Minutes }

constructor TdxSchedulerTimeScale30Minutes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionTimeScale30MinutesCaption;
  FDefaultHintResString := @sdxSchedulerActionTimeScale30MinutesHint;
  FDefaultImageNameInIconLibrary := '';
  FTimeScaleStep := 30;
end;

{ TdxSchedulerTimeScale15Minutes }

constructor TdxSchedulerTimeScale15Minutes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionTimeScale15MinutesCaption;
  FDefaultHintResString := @sdxSchedulerActionTimeScale15MinutesHint;
  FDefaultImageNameInIconLibrary := '';
  FTimeScaleStep := 15;
end;

{ TdxSchedulerTimeScale10Minutes }

constructor TdxSchedulerTimeScale10Minutes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionTimeScale10MinutesCaption;
  FDefaultHintResString := @sdxSchedulerActionTimeScale10MinutesHint;
  FDefaultImageNameInIconLibrary := '';
  FTimeScaleStep := 10;
end;

{ TdxSchedulerTimeScale6Minutes }

constructor TdxSchedulerTimeScale6Minutes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionTimeScale6MinutesCaption;
  FDefaultHintResString := @sdxSchedulerActionTimeScale6MinutesHint;
  FDefaultImageNameInIconLibrary := '';
  FTimeScaleStep := 6;
end;

{ TdxSchedulerTimeScale5Minutes }

constructor TdxSchedulerTimeScale5Minutes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionTimeScale5MinutesCaption;
  FDefaultHintResString := @sdxSchedulerActionTimeScale5MinutesHint;
  FDefaultImageNameInIconLibrary := '';
  FTimeScaleStep := 5;
end;

{ TdxSchedulerCompressWeekends }

constructor TdxSchedulerCompressWeekends.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSchedulerActionCompressWeekendsCaption;
  FDefaultHintResString := @sdxSchedulerActionCompressWeekendsHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\WeekEnd.png';
end;

procedure TdxSchedulerCompressWeekends.DoExecute;
begin
  TcxSchedulerWeekView(Control.CurrentView).CompressWeekEnd := Checked;
end;

procedure TdxSchedulerCompressWeekends.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := Enabled and TcxSchedulerWeekView(Control.CurrentView).CompressWeekEnd;
end;

function TdxSchedulerCompressWeekends.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (Control.CurrentView is TcxSchedulerCustomWeekView);
end;

{ TdxSchedulerWorkTimeOnly }

constructor TdxSchedulerWorkTimeOnly.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSchedulerActionWorkTimeOnlyCaption;
  FDefaultHintResString := @sdxSchedulerActionWorkTimeOnlyHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\ShowWorkTimeOnly.png';
end;

procedure TdxSchedulerWorkTimeOnly.DoExecute;
begin
  if Control.CurrentView is TcxSchedulerDayView then
    TcxSchedulerDayView(Control.CurrentView).WorkTimeOnly := Checked
  else
    if Control.CurrentView is TcxSchedulerTimeGridView then
      TcxSchedulerTimeGridView(Control.CurrentView).WorkTimeOnly := Checked;
end;

procedure TdxSchedulerWorkTimeOnly.DoUpdateState;
var
  AChecked: Boolean;
begin
  inherited DoUpdateState;
  AChecked := Enabled;
  if Control.CurrentView is TcxSchedulerDayView then
    AChecked := AChecked and TcxSchedulerDayView(Control.CurrentView).WorkTimeOnly
  else
    if Control.CurrentView is TcxSchedulerTimeGridView then
      AChecked := AChecked and TcxSchedulerTimeGridView(Control.CurrentView).WorkTimeOnly;
  Checked := AChecked;
end;

function TdxSchedulerWorkTimeOnly.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ((Control.CurrentView is TcxSchedulerDayView) or
    (Control.CurrentView is TcxSchedulerTimeGridView));
end;

{ TdxSchedulerSnapEventsToTimeSlots }

constructor TdxSchedulerSnapEventsToTimeSlots.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSchedulerActionSnapEventsToTimeSlotsCaption;
  FDefaultHintResString := @sdxSchedulerActionSnapEventsToTimeSlotsHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\SnapToCells.png';
end;

procedure TdxSchedulerSnapEventsToTimeSlots.DoExecute;
begin
  TcxSchedulerTimeGridView(Control.CurrentView).SnapEventsToTimeSlots := Checked;
end;

procedure TdxSchedulerSnapEventsToTimeSlots.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := Enabled and TcxSchedulerTimeGridView(Control.CurrentView).SnapEventsToTimeSlots;
end;

function TdxSchedulerSnapEventsToTimeSlots.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (Control.CurrentView is TcxSchedulerTimeGridView);
end;

{ TdxSchedulerDateNavigator }

constructor TdxSchedulerDateNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSchedulerActionDateNavigatorCaption;
  FDefaultHintResString := @sdxSchedulerActionDateNavigatorHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\Calendar.png';
end;

procedure TdxSchedulerDateNavigator.DoExecute;
begin
  Control.DateNavigator.Visible := Checked;
end;

procedure TdxSchedulerDateNavigator.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := Control.DateNavigator.Visible;
end;

{ TdxSchedulerResourcesLayoutEditor }

constructor TdxSchedulerResourcesLayoutEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionResourcesLayoutEditorCaption;
  FDefaultHintResString := @sdxSchedulerActionResourcesLayoutEditorHint;
  FDefaultImageNameInIconLibrary := 'Scheduling\ResourcesLayoutEditor.png';
end;

procedure TdxSchedulerResourcesLayoutEditor.DoExecute;
begin
  cxShowResourcesLayoutEditor(Control.Storage, Control.DialogsLookAndFeel);
end;

function TdxSchedulerResourcesLayoutEditor.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (Control.Storage.ResourceCount > 0);
end;

{ TdxSchedulerShowPageSetupForm }

constructor TdxSchedulerShowPageSetupForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionPageSetupCaption;
  FDefaultHintResString := @sdxSchedulerActionPageSetupHint;
  FDefaultImageNameInIconLibrary := 'Setup\PageSetup.png';
end;

function TdxSchedulerShowPageSetupForm.GetControlClass: TWinControlClass;
begin
  Result := TcxScheduler;
end;

{ TdxSchedulerShowPrintForm }

constructor TdxSchedulerShowPrintForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionPrintCaption;
  FDefaultHintResString := @sdxSchedulerActionPrintHint;
  FDefaultImageNameInIconLibrary := 'Print\PrintDialog.png';
end;

function TdxSchedulerShowPrintForm.GetControlClass: TWinControlClass;
begin
  Result := TcxScheduler;
end;

{ TdxSchedulerShowPrintPreviewForm }

constructor TdxSchedulerShowPrintPreviewForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSchedulerActionPrintPreviewCaption;
  FDefaultHintResString := @sdxSchedulerActionPrintPreviewHint;
  FDefaultImageNameInIconLibrary := 'Print\Preview.png';
end;

function TdxSchedulerShowPrintPreviewForm.GetControlClass: TWinControlClass;
begin
  Result := TcxScheduler;
end;

end.
