unit AgendaViewDemoMain;

interface

{$I cxVer.inc}

uses
{$IFDEF EXPRESSTREELIST}
  cxSchedulerTreeListBrowser,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Variants,
  Graphics, Controls, Forms, Dialogs, DemoBasicMain, cxLookAndFeels, ActnList,
  ImgList, Menus, StdCtrls, ComCtrls, cxClasses, cxStyles, cxGraphics, cxEdit, cxScheduler,
  cxSchedulerCustomControls, cxSchedulerCustomResourceView,
  cxSchedulerDayView, cxSchedulerDateNavigator, cxSchedulerStorage,
  cxControls, cxContainer, cxTextEdit, cxMemo, cxRichEdit, ExtCtrls, Buttons,
  cxSchedulerWeekView, cxSchedulerTimeGridView, cxSchedulerUtils,
  cxSchedulerYearView, cxSchedulerHolidays, cxSchedulerAgendaView,
  cxLookAndFeelPainters, cxButtons, cxDateUtils, dxCalendarUtils, cxSchedulerRecurrence,
  cxSchedulerRibbonStyleEventEditor, cxSchedulerGanttView;

type
  TAgendaViewDemoMainForm = class(TDemoBasicMainForm)
    SchedulerStorage: TcxSchedulerStorage;
    AgendaOptions1: TMenuItem;
    DayHeaderOrientation2: TMenuItem;
    DisplayMode1: TMenuItem;
    Showlocation2: TMenuItem;
    Showresources2: TMenuItem;
    Showtimeasclock2: TMenuItem;
    Horizontal1: TMenuItem;
    Vertical1: TMenuItem;
    AllDays1: TMenuItem;
    SelectedDays1: TMenuItem;
    SelectedNonEmptyDays1: TMenuItem;
    procedure Horizontal1Click(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  AgendaViewDemoMainForm: TAgendaViewDemoMainForm;

implementation

{$R *.dfm}

constructor TAgendaViewDemoMainForm.Create(AOwner: TComponent);
var
  ADate: TDateTime;
begin
  inherited Create(AOwner);
  miDay.Visible := False;
  miWorkWeek.Visible := False;
  miWeek.Visible := False;
  miMonth.Visible := False;
  miTimeGrid.Visible := False;
  miYear.Visible := False;
  miAgenda.Visible := False;
  miGotoDate.Visible := False;
  Options1.Visible := False;
  miEventsOpt.Visible := False;
  Resources1.Visible := False;
  ADate := EncodeDate(2016, 11, 30);
  SchedulerStorage.LoadFromFile('..\..\Data\AgendaViewData.bin');
  Scheduler.SelectDays(ADate, ADate, True);
end;

procedure TAgendaViewDemoMainForm.Horizontal1Click(Sender: TObject);
const
  AOrientation: array[Boolean] of TcxSchedulerAgendaViewDayHeaderOrientation = (dhoHorizontal, dhoVertical);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  Scheduler.ViewAgenda.DayHeaderOrientation := AOrientation[Vertical1.Checked];
end;

end.
