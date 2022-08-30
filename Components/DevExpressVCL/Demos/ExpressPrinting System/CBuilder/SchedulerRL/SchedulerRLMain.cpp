//---------------------------------------------------------------------------
#include "cxDemosBCB.inc"

#include <vcl.h>
#pragma hdrstop

#include "cxDemosBCB.inc"
#include "SchedulerRLMain.h"
#include "SysUtils.hpp"
#include "cxSchedulerUtils.hpp"
#include "cxSchedulerDialogs.hpp"
#include "cxExportSchedulerLink.hpp"
#include "cxSchedulerOutlookExchange.hpp"

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
#pragma link "cxSchedulerStorage"
#pragma link "cxStyles"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxScheduler"
#pragma link "cxSchedulerCustomControls"
#pragma link "cxSchedulerCustomResourceView"
#pragma link "cxSchedulerDateNavigator"
#pragma link "cxSchedulerOutlookExchange"
#pragma link "cxSchedulerDayView"
#pragma link "cxStyles"
#pragma link "cxSchedulerStorage"
#pragma link "dxPSCore"
#pragma link "dxPScxCommon"
#pragma link "dxPScxSchedulerLnk"
#pragma link "cxDrawTextUtils"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxSchedulerGanttView"
#pragma link "cxSchedulerHolidays"
#pragma link "cxSchedulerTimeGridView"
#pragma link "cxSchedulerUtils"
#pragma link "cxSchedulerWeekView"
#pragma link "cxSchedulerYearView"
#pragma link "dxBkgnd"
#pragma link "dxPrnDev"
#pragma link "dxPrnPg"
#pragma link "dxPSCompsProvider"
#pragma link "dxPScxEditorProducers"
#pragma link "dxPScxExtEditorProducers"
#pragma link "dxPScxPageControlProducer"
#pragma link "dxPSEdgePatterns"
#pragma link "dxPSEngn"
#pragma link "dxPSFillPatterns"
#pragma link "dxPSGlbl"
#pragma link "dxPSPDFExport"
#pragma link "dxPSPDFExportCore"
#pragma link "dxPSPrVwStd"
#pragma link "dxPSUtl"
#pragma link "dxWrap"
#pragma link "DemoBasicMain"
#pragma resource "*.dfm"
TSchedulerRLMainForm *SchedulerRLMainForm;

const String Captions[23] =
   {"Men's Track and Field",
    "500 meter Men's Speed Skating",
    "100 Meter Individual Medley",
    "Women's Basketball Finals",
    "Women's Cycling 20mi",
    "Men's Gymnastics Finals",
    "Women's Hockey Finals",
    "Women's High Dive Finals",
    "Karate Finals",
    "Kayak Level IV Finals",
    "Lacrosse Semi-finals",
    "Polo Finals",
    "Women's Soccer Finals",
    "Surfing:  Freestyle Finals",
    "Swimming: 100 m Finals",
    "Track & Field:  Women's Finals",
    "Archery Finals",
    "Men's Hockey Finals",
    "Men's Downhill Giant Solom",
    "Men's Ski Jump Finals",
    "Women's Downhill Ski Finals",
    "Men's Relay",
    "Baseball Quarter-finals"};

//---------------------------------------------------------------------------
__fastcall TSchedulerRLMainForm::TSchedulerRLMainForm(TComponent* Owner)
        : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------


void __fastcall TSchedulerRLMainForm::FormCreate(TObject* Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  Scheduler->SelectDays(Date() - 1, Date() + 1, True);
  Height = 550;
  Width = 700;
}
//---------------------------------------------------------------------------

void TSchedulerRLMainForm::GenerateRandomEvents(int ACount, bool ARandomResource)
{
  TDateTime ADate;
  TcxSchedulerEvent *AEvent;
  TcxCustomSchedulerStorage *AStorage;

  AStorage = Scheduler->Storage;
  if (AStorage == NULL) return;
  AStorage->BeginUpdate();
  try {
    Randomize();
    for (int I = 0; I < (int)(ACount/5 * 4); I++){
      AEvent = AStorage->createEvent();
      ADate = GetRandomDate();
      AEvent->Start = ADate;
      AEvent->Finish = ADate + (double)(random(180) * MinuteToTime);
      AEvent->State = GetRandomState();
      AEvent->LabelColor = EventLabelColors[random(11)];
      AEvent->Caption = GetRandomCaption();
      if (ARandomResource)
        AEvent->ResourceID = GetRandomResourceID();
    }
    for (int I = 0; I < (int)(ACount/5); I++){
      AEvent = AStorage->createEvent();
      ADate = GetRandomDate();
      AEvent->Start = ADate;
      AEvent->Finish = ADate + 1;
      AEvent->AllDayEvent = true;
      AEvent->LabelColor = EventLabelColors[random(11)];
      AEvent->Caption = GetRandomCaption();
      if (ARandomResource)
        AEvent->ResourceID = GetRandomResourceID();
    }
  }
  __finally {
    AStorage->EndUpdate();
  }
}

//---------------------------------------------------------------------------

String TSchedulerRLMainForm::GetRandomCaption()
{
  return Captions[random(sizeof(Captions)/sizeof(Captions[0]))];
}

//---------------------------------------------------------------------------

TDateTime TSchedulerRLMainForm::GetRandomDate()
{
  TDateTime ADate = Date() + random(60) - 30 + Scheduler->OptionsView->WorkStart;
  if ((int)(ADate) == (int)Date())
    ADate = ADate + (double)(random(10) * HourToTime);
  else
    ADate = ADate + (double)(random(24) * HourToTime);
  return ADate;
}

//---------------------------------------------------------------------------

TColor TSchedulerRLMainForm::GetRandomLabelColor()
{
  return EventLabelColors[random(11)];
}

//---------------------------------------------------------------------------

Variant TSchedulerRLMainForm::GetRandomResourceID()
{
  if ((Scheduler->Storage == NULL) || (Scheduler->Storage->ResourceCount == 0))
    return Null;
  else
    if (Scheduler->Storage->ResourceCount == 1)
      return Scheduler->Storage->ResourceIDs[0];
    else
      return Scheduler->Storage->ResourceIDs[random(Scheduler->Storage->ResourceCount)];
}

//---------------------------------------------------------------------------

int TSchedulerRLMainForm::GetRandomState()
{
  return random(4);
}

void __fastcall TSchedulerRLMainForm::tbnGenerateClick(TObject *Sender)
{
  GenerateRandomEvents(100, false);
}
//---------------------------------------------------------------------------


