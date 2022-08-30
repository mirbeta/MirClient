//---------------------------------------------------------------------------
#include "..\cxDemosBCB.inc"

#include <vcl.h>
#pragma hdrstop

#include "UnboundDemoMain.h"
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
#pragma link "cxButtons"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TUnboundDemoMainForm *UnboundDemoMainForm;

//---------------------------------------------------------------------------
__fastcall TUnboundDemoMainForm::TUnboundDemoMainForm(TComponent* Owner)
        : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TUnboundDemoMainForm::btnGenerate1Click(TObject *Sender)
{
  GenerateRandomEvents(500, true);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundDemoMainForm::btnDelete1Click(TObject *Sender)
{
  SchedulerUnboundStorage->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TUnboundDemoMainForm::SchedulerInitEventImages(
      TcxCustomScheduler *Sender, TcxSchedulerControlEvent *AEvent,
      TcxSchedulerEventImages *AImages)
{
  Variant AValue = AEvent->GetCustomFieldValueByName("IconIndex");
  if ((VarIsNull(AValue) || VarIsEmpty(AValue)) || ((int)AValue == -1)) return;
  AImages->Add(AValue, False);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  Scheduler->SelectDays(Date(), Date(), True);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundDemoMainForm::OnNewEvent(TcxSchedulerEvent *AEvent, int AIndex)
{
  AEvent->SetCustomFieldValueByName("IconIndex", AIndex);
  if ((random(2) == 1) || ((int)AEvent->Start >= (int)Now())) AEvent->Reminder = true;
}
//---------------------------------------------------------------------------

void __fastcall TUnboundDemoMainForm::SchedulerUnboundStorageRemindersOpenEvent(
  TcxSchedulerReminders *Sender, TcxSchedulerControlEvent *AEvent)
{
  Scheduler->EditEventUsingDialog(AEvent, true, false);
}

//---------------------------------------------------------------------------


