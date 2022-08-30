//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "..\cxDemosBCB.inc"
#include "RentUnit.h"
#include "ResourceMainUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDateNavigator"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxListBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxScheduler"
#pragma link "cxSchedulerCustomControls"
#pragma link "cxSchedulerCustomResourceView"
#pragma link "cxSchedulerDateNavigator"
#pragma link "cxSchedulerDayView"
#pragma link "cxSchedulerStorage"
#pragma link "cxStyles"
#pragma link "cxTextEdit"
#pragma resource "*.dfm"

int __fastcall CompareEvents(TcxSchedulerEvent *AEvent1, TcxSchedulerEvent *AEvent2)
{
  if (AEvent1->Start == AEvent2->Start)
    return 0;
  else
    if (AEvent1->Start < AEvent2->Start) 
      return -1;
    else
      return 1;
}

//---------------------------------------------------------------------------
__fastcall TfrmRentCar::TfrmRentCar(TComponent* Owner)
        : TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::AddPeriodTime(TDateTime AStart, TDateTime AFinish)
{
  if (AStart == AFinish) return;
  FPeriods->Add(TimeToDateTime(AStart));
  FPeriods->Add(TimeToDateTime(AFinish));
  lbxPeriod->Items->Add(TimeToStr(AStart)  + '-' + TimeToStr(AFinish));
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::AddRent(TDateTime AStart, TDateTime AFinish)
{
  if (AStart >= AFinish) return;
  TcxSchedulerEvent *AEvent = Storage()->createEvent();
  AEvent->Start = AStart;
  AEvent->Finish = AFinish;
  AEvent->Caption = edtUserName->Text;
  AEvent->ResourceID = GetIndex(lvCars);
}
//---------------------------------------------------------------------------

TDateTime __fastcall TfrmRentCar::DateTimeToTime(TDateTime ATime)
{
  return DateNavigator->Date + EncodeTime((double)ATime / 60, (int)ATime % 60, 0, 0);
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::FillTimeList()
{
  TDateTime AFinish;
  lbxPeriod->Items->BeginUpdate();
  FPeriods->Clear();
  try {
    lbxPeriod->Items->Clear();
    TDateTime AStart = EncodeTime(0, 0, 0, 0);
    if (FDayEvents->Count == 0)
      AddPeriodTime(AStart, EncodeTime(23, 59, 0, 0));
    else
      for (int I = 0; I <= FDayEvents->Count; I++) {
        if (I > 0)
          AStart = FDayEvents->Items[I - 1]->Finish;
        if (I < FDayEvents->Count)
          AFinish = FDayEvents->Items[I]->Start;
        else
          AFinish = EncodeTime(23, 59, 0, 0);
        AddPeriodTime(AStart, AFinish);
      }
  }
  __finally {
    lbxPeriod->Items->EndUpdate();
  }
}

//---------------------------------------------------------------------------
bool __fastcall TfrmRentCar::Intersect(TDateTime AStart1, TDateTime AFinish1,
  TDateTime AStart2, TDateTime AFinish2)
{
  return (AStart1 < AFinish2) && (AStart2 < AFinish1);
}

//---------------------------------------------------------------------------
bool __fastcall TfrmRentCar::IntersectTime(TDateTime &AStart1,
  TDateTime &AFinish1, TDateTime &AStart2, TDateTime &AFinish2)
{
  if (AStart2 > AStart1)
    AStart1 = AStart2;
  if (AFinish2 < AFinish1)
    AFinish1 = AFinish2;
  return AFinish1 > AStart1;
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::ProcessSelectItem(int AIndex)
{
  FDays->Clear();
  Storage()->GetEvents(FEventsList, DateNavigator->RealFirstDate,
    DateNavigator->RealLastDate, AIndex);
  for (int I = (int)DateNavigator->RealFirstDate; I <= (int)DateNavigator->RealLastDate; I++)
    for (int J = 0; J < FEventsList->Count; J++)
      if (FEventsList->Items[J]->IsDayEvent(I)) {
        FDays->Add(I);
        break;
      }
  DateNavigator->LayoutChanged();
  DateNavigatorSelectionChanged(DateNavigator, DateNavigator->Date, DateNavigator->Date);
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::RentCar()
{
  TDateTime AStart1, AFinish1;
  TDateTime AStart = TimeScheduler->SelStart;
  TDateTime AFinish = TimeScheduler->SelFinish;
  try {
    for (int I = 0; I <= FDayEvents->Count; I++) {
      if (I == 0)
        AStart1 = (int)AStart;
      if (I == FDayEvents->Count)
        AFinish1 = (int)AStart + (double)EncodeTime(23, 59, 0, 0);
      else
        AFinish1 = FDayEvents->Items[I]->Start;
      if ((AStart1 < AFinish1) && IntersectTime(AStart1, AFinish1, AStart, AFinish))
        AddRent(AStart1, AFinish1);
      if ((FDayEvents->Count > 0) && (I < FDayEvents->Count))
        AStart1 = FDayEvents->Items[I]->Finish;
    }
  }
  __finally {
    Storage()->PostEvents();
    ProcessSelectItem(GetIndex(lvCars));
    TimeScheduler->Refresh();
  }
}

//---------------------------------------------------------------------------
TcxSchedulerStorage* __fastcall TfrmRentCar::Storage()
{
  return ResourceDemoMainForm->Storage;
}

//---------------------------------------------------------------------------
TDateTime __fastcall TfrmRentCar::TimeToDateTime(TDateTime ATime)
{
  WORD H, M, S, MS;
  DecodeTime(ATime, H, M, S, MS);
  return H * 60 + M;
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::FormCreate(TObject* Sender)
{
  FPeriods = new TcxSchedulerDateList;
  FEventsList = new TcxSchedulerFilteredEventList;
  FDays = new TcxSchedulerDateList;
  FDayEvents = new TcxSchedulerEventList;
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::FormDestroy(TObject* Sender)
{
  delete FPeriods;
  delete FEventsList;
  delete FDays;
  delete FDayEvents;
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::FormShow(TObject* Sender)
{
  Storage()->FullRefresh();
  TcxCustomScheduler* AScheduler = ResourceDemoMainForm->Scheduler;
  DateNavigator->Date = (int)AScheduler->SelStart;
  if (AScheduler->SelResource == NULL)
    SetIndex(lvCars, 0);
  else
    SetIndex(lvCars, AScheduler->SelResource->ID);
  ProcessSelectItem(GetIndex(lvCars));
  TimeScheduler->SelectDays(DateNavigator->Date, DateNavigator->Date, true);
  TimeScheduler->SelectTime(AScheduler->SelStart, AScheduler->SelFinish, NULL);
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::TimeSchedulerCustomDrawContent(TObject* Sender,
  TcxCanvas* ACanvas, TcxSchedulerContentCellViewInfo* AViewInfo, bool &ADone)
{
  for (int I = 0; I < FDayEvents->Count; I++)
    if (Intersect(AViewInfo->TimeStart, AViewInfo->TimeFinish,
      FDayEvents->Items[I]->Start, FDayEvents->Items[I]->Finish)) {
      ACanvas->Brush->Color = clBtnShadow;
      ACanvas->FillRect(AViewInfo->Bounds, NULL, false);
      ACanvas->Font->Color = clBtnText;
      ACanvas->DrawTexT("Busy time", AViewInfo->Bounds, cxAlignCenter, true);
      AViewInfo->Transparent = true;
    }
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::DateNavigatorCustomDrawDayNumber(TObject* Sender,
  TcxCanvas* ACanvas, TcxSchedulerDateNavigatorDayNumberViewInfo* AViewInfo,
  bool &ADone)
{
  if (FDays->IndexOf(AViewInfo->Date) != -1)
    ACanvas->Font = cxBoldStyle->Font;
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::DateNavigatorCustomDrawContent(TObject* Sender,
  TcxCanvas* ACanvas, TcxSchedulerDateNavigatorMonthContentViewInfo* AViewInfo,
  bool &ADone)
{
  ResourceDemoMainForm->DrawDateNavigatorContent(ACanvas, AViewInfo, ADone);
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::lvCarsSelectItem(TObject* Sender, TListItem* Item,
  bool Selected)
{
  if (!Selected) return;
  ProcessSelectItem(Item->Index);
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::lbxPeriodClick(TObject* Sender)
{
  int AIndex = lbxPeriod->ItemIndex;
  if (AIndex != -1) {
    TimeScheduler->SelectTime(DateTimeToTime(FPeriods->Items[AIndex * 2]),
      DateTimeToTime((double)FPeriods->Items[AIndex * 2 + 1]) + (double)HourToTime, NULL);
  }
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::TimeSchedulerKeyDown(TObject* Sender, WORD &Key,
  TShiftState Shift)
{
  if (Key != VK_TAB) return;
  TWinControl* AControl = FindNextControl(TimeScheduler,
    !Shift.Contains(ssCtrl), true, true);
  if (AControl != NULL)
    AControl->SetFocus();
}

//---------------------------------------------------------------------------
void __fastcall TfrmRentCar::btnRentClick(TObject* Sender)
{
  RentCar();
}
//---------------------------------------------------------------------------

void __fastcall TfrmRentCar::DateNavigatorSelectionChanged(TObject *Sender,
      const TDateTime AStart, const TDateTime AFinish)
{
  FDayEvents->Clear();
  for (int I = 0; I < FEventsList->Count; I++)
    if (FEventsList->Items[I]->IsDayEvent(AStart))
      FDayEvents->Add(FEventsList->Items[I]);
  FDayEvents->Sort(CompareEvents);
  FillTimeList();
  TimeScheduler->SelectDays(AStart, AFinish, true);
  lbxPeriod->ItemIndex = 0;
  lbxPeriodClick(lbxPeriod);
}
//---------------------------------------------------------------------------

