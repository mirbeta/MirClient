//---------------------------------------------------------------------------
#include "..\cxDemosBCB.inc"

#include <vcl.h>
#pragma hdrstop

#include "DBDemoMainUnit.h"
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
#pragma link "cxSchedulerAgendaView"
#pragma link "cxSchedulerDBStorage"
#pragma link "cxSchedulerStorage"
#pragma link "cxStyles"
#pragma link "DemoBasicMain"
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxGroupBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxSchedulerGanttView"
#pragma link "cxSchedulerHolidays"
#pragma link "cxSchedulerTimeGridView"
#pragma link "cxSchedulerUtils"
#pragma link "cxSchedulerWeekView"
#pragma link "cxSchedulerYearView"
#pragma link "cxLookAndFeels"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TDBDemoMainForm *DBDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TDBDemoMainForm::TDBDemoMainForm(TComponent* Owner)
        : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TDBDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  mdEvents->LoadFromBinaryFile("..\\..\\Data\\cxSchedulerEvents.dat");
  mdEvents->Open();
  Scheduler->GoToDate(EncodeDate(2016, 11, 30));
  FEventCount = SchedulerDBStorage->EventCount;
  FMaxID = 0;
  for (int i = 0; i < SchedulerDBStorage->EventCount; i++)
	FMaxID = Max(FMaxID, SchedulerDBStorage->Events[i]->ID);
}
//---------------------------------------------------------------------------

void __fastcall TDBDemoMainForm::chDataModeClick(TObject *Sender)
{
  SchedulerDBStorage->SmartRefresh = cxCheckBox1->Checked;
//
}
//---------------------------------------------------------------------------

void __fastcall TDBDemoMainForm::cxButton1Click(TObject *Sender)
{
  MaxRandomPeriod = 720;
  GenerateRandomEvents(5000, false);
  MaxRandomPeriod = 60;
}
//---------------------------------------------------------------------------
