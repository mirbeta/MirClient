//---------------------------------------------------------------------------
#include "..\cxDemosBCB.inc"

#include <vcl.h>
#pragma hdrstop

#include "AgendaViewDemoMain.h"
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
#pragma link "cxSchedulerHolidays"
#pragma link "cxDateUtils"
#pragma link "dxCalendarUtils"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TAgendaViewDemoMainForm *AgendaViewDemoMainForm;

//---------------------------------------------------------------------------
__fastcall TAgendaViewDemoMainForm::TAgendaViewDemoMainForm(TComponent* Owner)
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

  ADate = EncodeDate(2016, 11, 30);
  SchedulerStorage->LoadFromFile("..\\..\\Data\\AgendaViewData.bin");
  Scheduler->SelectDays(ADate, ADate, True);
}

void __fastcall TAgendaViewDemoMainForm::Horizontal1Click(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = True;
  Scheduler->ViewAgenda->DayHeaderOrientation = (TcxSchedulerAgendaViewDayHeaderOrientation)(((TMenuItem*)Sender)->Tag);
}


//---------------------------------------------------------------------------

