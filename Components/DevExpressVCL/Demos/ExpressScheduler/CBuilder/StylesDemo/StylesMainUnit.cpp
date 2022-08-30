//---------------------------------------------------------------------------
#include "..\cxDemosBCB.inc"

#include <vcl.h>
#pragma hdrstop

#include "StylesMainUnit.h"
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
#pragma link "cxSchedulerTimeGridView"
#pragma link "cxSchedulerUtils"
#pragma link "cxSchedulerWeekView"
#pragma link "cxSchedulerYearView"
#pragma link "cxSchedulerAgendaView"
#pragma link "cxClasses"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxSchedulerGanttView"
#pragma link "cxSchedulerHolidays"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TStylesMainForm *StylesMainForm;
//---------------------------------------------------------------------------
__fastcall TStylesMainForm::TStylesMainForm(TComponent* Owner)
        : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

TcxLookAndFeelKind __fastcall TStylesMainForm::GetDefaultLookAndFeelKind()
{
  return(lfUltraFlat);
}

TcxStyle *GetStyle(TObject *Sender, TcxStyle *AAssignedStyle)
{
  if (((TMenuItem*)Sender)->Checked)
    return AAssignedStyle;
  else
    return NULL;
}

void __fastcall TStylesMainForm::miStylesItemClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  switch (((TMenuItem*)Sender)->Tag) {
    case 0: // Events
      Scheduler->Styles->Event = GetStyle(Sender, stEvents);
      break;
    case 1: // Headers
      Scheduler->Styles->DayHeader = GetStyle(Sender, stHeaders);
      break;
    case 2: // Content
      Scheduler->Styles->Content = GetStyle(Sender, stContent);
      break;
    case 3: // Content selection
      Scheduler->Styles->Selection = GetStyle(Sender, stContentSelection);
      break;
    case 4: // Resources
      Scheduler->Styles->ResourceHeader = GetStyle(Sender, stResources);
      break;
    case 5: // Group separator
      Scheduler->Styles->GroupSeparator = GetStyle(Sender, stGroupSeparator);
      break;
    case 6: // DayView container
      Scheduler->ViewDay->Styles->HeaderContainer = GetStyle(Sender, stContainer);
      break;
    case 7: // DayView time ruler
      Scheduler->ViewDay->Styles->TimeRuler = GetStyle(Sender, stTimeRuler);
      break;
    case 8: // date navigator header
      Scheduler->DateNavigator->Styles->Header = GetStyle(Sender, stHeaders);
      break;
    case 9: // date navigator background
      Scheduler->DateNavigator->Styles->Background = GetStyle(Sender, stBackground);
      break;
    case 10: // date navigator content
      Scheduler->DateNavigator->Styles->Content = GetStyle(Sender, stDateContent);

  }
}
//---------------------------------------------------------------------------
void __fastcall TStylesMainForm::miSplitterClick(TObject *Sender)
{
  ((TMenuItem*)miSplitter)->Checked = !((TMenuItem*)miSplitter)->Checked;
  if (((TMenuItem*)miSplitter)->Checked){
    Scheduler->OptionsView->VertSplitterWidth = 50;
    Scheduler->Styles->VertSplitter = stVertSplitter;
  }
  else {
    Scheduler->OptionsView->VertSplitterWidth = cxDefaultSplitterWidth;
    Scheduler->Styles->VertSplitter = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TStylesMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  mdEvents->LoadFromBinaryFile("..\\..\\Data\\cxSchedulerEvents.dat");
  mdEvents->Open();
  Scheduler->GoToDate(EncodeDate(2016, 11, 30));
  SchedulerDBStorage->BeginUpdate();
  try {
    for (int I = 0; I < SchedulerDBStorage->EventCount; I++){
     SchedulerDBStorage->Events[I]->ResourceID = GetRandomResourceID();
      }
  }
  __finally {
    SchedulerDBStorage->EndUpdate();
  }
}
//---------------------------------------------------------------------------

