//---------------------------------------------------------------------------
#include "..\cxDemosBCB.inc"

#include <vcl.h>
#pragma hdrstop

#include "GanttViewDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxScheduler"
#pragma link "cxSchedulerCustomControls"
#pragma link "cxSchedulerCustomResourceView"
#pragma link "cxSchedulerDateNavigator"
#pragma link "cxSchedulerDayView"
#pragma link "cxStyles"
#pragma link "DemoBasicMain"
#pragma link "cxSchedulerStorage"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxScheduler"
#pragma link "cxSchedulerCustomControls"
#pragma link "cxSchedulerCustomResourceView"
#pragma link "cxSchedulerDateNavigator"
#pragma link "cxSchedulerDayView"
#pragma link "cxSchedulerStorage"
#pragma link "cxStyles"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxScheduler"
#pragma link "cxSchedulerCustomControls"
#pragma link "cxSchedulerCustomResourceView"
#pragma link "cxSchedulerDateNavigator"
#pragma link "cxSchedulerDayView"
#pragma link "cxSchedulerAgendaView"
#pragma link "cxSchedulerStorage"
#pragma link "cxStyles"
#pragma link "DemoBasicMain"
#pragma link "cxSchedulerTimeGridView"
#pragma link "cxSchedulerUtils"
#pragma link "cxSchedulerWeekView"
#pragma link "cxSchedulerYearView"
#pragma link "cxSchedulerGanttView"
#pragma link "cxSchedulerHolidays"
#pragma link "cxDateUtils"
#pragma link "dxCalendarUtils"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TGanttViewDemoMainForm *GanttViewDemoMainForm;

//---------------------------------------------------------------------------
__fastcall TGanttViewDemoMainForm::TGanttViewDemoMainForm(TComponent* Owner)
        : TDemoBasicMainForm(Owner)
{
  TDateTime ADate;

  miDay->Visible = False;
  miWorkweek->Visible = False;
  miWeek->Visible = False;
  miMonth->Visible = False;
  miTimeGrid->Visible = False;
  miYear->Visible = False;
  miAgenda->Visible = False;
  miGotoDate->Visible = False;
  Options1->Visible = False;
  miEventsOpt->Visible = False;
  Resources1->Visible = False;

  ADate = EncodeDate(2011, 8, 1);
  SchedulerStorage->LoadFromFile("..\\..\\Data\\cxSchedulerGanttViewData.bin");
  Scheduler->SelectDays(ADate, ADate, True);
}

void __fastcall TGanttViewDemoMainForm::miShowAsProgressClick(TObject *Sender)
{
  if (miShowAsProgress->Checked)
    Scheduler->ViewGantt->EventsStyle = esProgress;
  else
    Scheduler->ViewGantt->EventsStyle = esDefault;
  miShowTotalProgress->Enabled = Scheduler->ViewGantt->EventsStyle == esProgress;
}

void __fastcall TGanttViewDemoMainForm::miShowTotalProgressClick(TObject *Sender)
{
  Scheduler->ViewGantt->ShowTotalProgressLine = miShowTotalProgress->Checked;
}

void __fastcall TGanttViewDemoMainForm::miShowExpandButtonsClick(TObject *Sender)
{
  Scheduler->ViewGantt->ShowExpandButtons = miShowExpandButtons->Checked;
}

void __fastcall TGanttViewDemoMainForm::miHotTrackClick(TObject *Sender)
{
  Scheduler->OptionsBehavior->HotTrack = miHotTrack->Checked;
}
void __fastcall TGanttViewDemoMainForm::SchedulerViewGanttGetMinorUnitDisplayText(TcxSchedulerTimeGridView *Sender,
          const TDateTime AStart, const TDateTime AFinish,
          TcxSchedulerTimeGridScaleTextType ATextType, String &AText)
{
  AText = cxGetDayOfWeekName(dxDayOfWeek(AFinish), Scheduler->Font->Charset);
}
//---------------------------------------------------------------------------

