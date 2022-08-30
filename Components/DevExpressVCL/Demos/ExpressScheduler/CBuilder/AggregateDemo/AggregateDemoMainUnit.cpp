//---------------------------------------------------------------------------
#include "..\cxDemosBCB.inc"

#include <vcl.h>
#pragma hdrstop

#include "AggregateDemoMainUnit.h"
#include "SelectStorageUnit.h"
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
#pragma link "cxSchedulerDBStorage"
#pragma link "cxSchedulerStorage"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxScheduler"
#pragma link "cxSchedulerCustomControls"
#pragma link "cxSchedulerCustomResourceView"
#pragma link "cxSchedulerDateNavigator"
#pragma link "cxSchedulerDayView"
#pragma link "cxSchedulerDBStorage"
#pragma link "cxSchedulerStorage"
#pragma link "cxStyles"
#pragma link "DemoBasicMain"
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxSchedulerAggregateStorage"
#pragma link "cxSchedulerGanttView"
#pragma link "cxSchedulerHolidays"
#pragma link "cxSchedulerTimeGridView"
#pragma link "cxSchedulerUtils"
#pragma link "cxSchedulerWeekView"
#pragma link "cxSchedulerYearView"
#pragma link "cxSchedulerAgendaView"
#pragma link "cxLookAndFeels"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TAggregateDemoMainForm *AggregateDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TAggregateDemoMainForm::TAggregateDemoMainForm(TComponent* Owner)
		: TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TAggregateDemoMainForm::FormCreate(TObject *Sender)
{
  mdEvents->LoadFromBinaryFile("..\\..\\Data\\cxSchedulerEvents.dat");
  mdEvents->Open();
  TDateTime ADate = Date();
  TDemoBasicMainForm::FormCreate(Sender);
  Scheduler->GoToDate(ADate);
  ADate = ADate + Scheduler->OptionsView->WorkStart;
  Scheduler->SelectTime(ADate, ADate, NULL);
  SchedulerDBStorage->Clear();
  cxButton1->LookAndFeel = Scheduler->LookAndFeel;
  cxButton2->LookAndFeel = Scheduler->LookAndFeel;
}
//---------------------------------------------------------------------------


void __fastcall TAggregateDemoMainForm::cxButton1Click(TObject *Sender)
{
	GenerateRandomEvents(500, false, SchedulerDBStorage, EventLabelColors[3]);
}

//---------------------------------------------------------------------------

void __fastcall TAggregateDemoMainForm::SchedulerAggregateStorageEventInserting(TcxSchedulerAggregateStorage *Sender, TcxSchedulerEvent *AEvent,
  TcxCustomSchedulerStorage *&AStorage)
{
  TSelectStorage * AEditor = new TSelectStorage(NULL);
  if (AEditor->ShowModal() == mrOk)
  {
	if (AEditor->rbDBStorage->Checked) AStorage = SchedulerDBStorage;
	else AStorage = SchedulerStorage;
  }
  AEditor->Free();
}
//---------------------------------------------------------------------------

void __fastcall TAggregateDemoMainForm::cxButton2Click(TObject *Sender)
{
  GenerateRandomEvents(500, false, SchedulerStorage, EventLabelColors[1]);
}
//---------------------------------------------------------------------------

