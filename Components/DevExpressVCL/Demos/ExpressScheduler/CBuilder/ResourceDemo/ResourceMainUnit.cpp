//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "..\cxDemosBCB.inc"
#include "ResourceMainUnit.h"
#include "RentUnit.h"
#include "CancelReservationUnit.h"
#include "cxSchedulerUtils.hpp"
#include "cxSchedulerStorage.hpp"
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
#pragma link "cxSchedulerAgendaView"
#pragma link "cxStyles"
#pragma link "DemoBasicMain"
#pragma link "cxLookAndFeels"
#pragma link "cxSchedulerStorage"
#pragma link "cxSchedulerUtils"
#pragma link "cxSchedulerTimeGridView"
#pragma link "cxSchedulerWeekView"
#pragma link "cxSchedulerYearView"
#pragma resource "*.dfm"
TResourceDemoMainForm *ResourceDemoMainForm;

bool IsChanged;

const int ContentColors[4] =
  {12317183, 16574923, 12381397, 12701439};

int __fastcall GetIndex(TListView *AListView)
{
  if (AListView->Selected != NULL)
    return AListView->Selected->Index;
  else
    return -1;
}

void __fastcall SetIndex(TListView *AListView, int AIndex)
{
  if (AIndex != -1)
    AListView->Selected = AListView->Items->Item[AIndex];
  else
    AListView->Selected = NULL;
}

//---------------------------------------------------------------------------
__fastcall TResourceDemoMainForm::TResourceDemoMainForm(TComponent* Owner)
        : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

int __fastcall scxTaskCompare(TcxSchedulerEvent *AEvent1, TcxSchedulerEvent *AEvent2)
{
  int Res = AnsiCompareText(AEvent1->Caption, AEvent2->Caption);
  if (Res == 0)
    Res = AnsiCompareText(AEvent1->GetResourceItem()->Name,
      AEvent2->GetResourceItem()->Name);
  if (Res == 0)
    if (AEvent1->Start < AEvent2->Start)
      Res = -1;
    else
      if (AEvent1->Start > AEvent2->Start)
        Res = 1;
  return Res;
}
//---------------------------------------------------------------------------


void __fastcall TResourceDemoMainForm::CreateRecurrenceEvent(
  int AResourceID, String ACaption, TDays AOccurDays)
{
  TcxSchedulerEvent *AEvent = Storage->createEvent();
  AEvent->ResourceID = AResourceID;
  AEvent->Caption = ACaption;
  AEvent->Duration = EncodeTime(0, 45, 0, 0);
  AEvent->MoveTo(Date() + (TDateTime)((8 + AResourceID) * (double)HourToTime));
  AEvent->EventType = etPattern;
  AEvent->LabelColor = 0x51B0F7;
  AEvent->RecurrenceInfo->Count = -1;
  AEvent->RecurrenceInfo->Recurrence = cxreWeekly;
  AEvent->RecurrenceInfo->Periodicity = 1;
  AEvent->RecurrenceInfo->OccurDays = AOccurDays;
  AEvent->RecurrenceInfo->DayType = cxdtDay;
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::DrawDateNavigatorContent(TcxCanvas *ACanvas,
    TcxSchedulerDateNavigatorMonthContentViewInfo *AViewInfo, bool &ADone)
{
  Graphics::TBitmap* ABitmap;
  if (!miUseColorScheme->Checked) return;
  switch (AViewInfo->Month) {
    case 3:
    case 4:
    case 5:
      ABitmap = cxSpringStyle->Bitmap;
      break;
    case 6:
    case 7:
    case 8:
      ABitmap = cxSummerStyle->Bitmap;
      break;
    case 9:
    case 10:
    case 11:
      ABitmap = cxAutumnStyle->Bitmap;
      break;
  default:
    ABitmap = cxWinterStyle->Bitmap;
  }
  ACanvas->Draw(AViewInfo->Bounds.Left, AViewInfo->Bounds.Top, ABitmap);
  ADone = true;
  AViewInfo->Transparent = true;
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::FillTaskGrid()
{
  TGridRect ASelRect;
  int I;
  TcxSchedulerEvent *AEvent;
  if (TaskEvents == NULL) return;
  TaskEvents->Clear();
  for (I = 0; I < Storage->EventCount; I++)
	if (Storage->Events[I]->EventType == Cxschedulerstorage::etNone)
      TaskEvents->Add(Storage->Events[I]);
  TaskEvents->Sort(scxTaskCompare);
  TaskGrid->RowCount = 2;
  if (TaskEvents->Count + 1 > 2)
    TaskGrid->RowCount = TaskEvents->Count + 1;
  TaskGrid->Rows[1]->Clear();
  ASelRect.Top = 1;
  ASelRect.Bottom = 1;
  ASelRect.Left = 0;
  ASelRect.Right = 3;
  for (I = 0; I < TaskEvents->Count; I++) {
    AEvent = TaskEvents->Items[I];
    TaskGrid->Cells[0][I + 1] = AEvent->Caption;
    TaskGrid->Cells[1][I + 1] = AEvent->GetResourceItem()->Name;
    TaskGrid->Cells[2][I + 1] = DateTimeToStr(AEvent->Start);
    TaskGrid->Cells[3][I + 1] = DateTimeToStr(AEvent->Finish);
    if ((Scheduler->SelectedEventCount > 0) && (Scheduler->SelectedEvents[0]->Source == AEvent)) {
      ASelRect.Top = I + 1;
      ASelRect.Bottom = I + 1;
    }
  }
  TaskGrid->Selection = ASelRect;
}
//---------------------------------------------------------------------------

TColor __fastcall  TResourceDemoMainForm::GetResourceColor(
  TcxSchedulerResourceViewInfo *AResource)
{
  return (TColor) ContentColors[AResource->ResourceItem->ID];
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::SelectEvents(int AStartIndex)
{
  Scheduler->BeginUpdate();
  try {
    Scheduler->UnselectEvents();
    for (int I = 0; I < Scheduler->VisibleEventCount; I++)
      if (Scheduler->VisibleEvents[I]->Index >= AStartIndex)
        Scheduler->SelectEvent(Scheduler->VisibleEvents[I], TShiftState()<<ssCtrl<<ssShift);
  }
  __finally {
    Scheduler->EndUpdate();
    Scheduler->CurrentView->Refresh();
  }
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::SyncVisibility(TDateTime ADate, int AIndex)
{
  if ((GetIndex(lvCarsBar) != 0) && (AIndex != -1))
    SetIndex(lvCarsBar, AIndex + 1);
  if (ADate != (TDateTime)NullDate) {
    TcxSchedulerDateList *ADays = Scheduler->SelectedDays;
    if (!((ADate >= ADays->Items[0]) && (ADate <= ADays->Items[ADays->Count - 1])))
      Scheduler->SelectDays(ADate, ADate + ADays->Count - 1, True);
  }
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::FormCreate(TObject* Sender)
{
  miEventsOpt->Visible = false;
  TaskEvents = new TcxSchedulerEventList;
  TDemoBasicMainForm::FormCreate(Sender);
  WindowState = wsMaximized;
  lvCarsBar->Items->Item[0]->Selected = true;
  Storage->BeginUpdate();
  for (int I = 0; I < 4; I++) {
    CreateRecurrenceEvent(I, "Maintenance", TDays()<<dMonday);
    CreateRecurrenceEvent(I, "Car wash", TDays()<<dWednesday<<dSaturday);
  }
  Storage->EndUpdate();
  miUseColorSchemeClick(miUseColorScheme);
  TaskGrid->Cells[0][0] = "Customer";
  TaskGrid->Cells[1][0] = "Model";
  TaskGrid->Cells[2][0] = "Start rent";
  TaskGrid->Cells[3][0] = "Finish rent";
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::lvCarsBarSelectItem(
  TObject* Sender, TListItem* Item, bool Selected)
{
  if (!Selected) return;
  Storage->BeginUpdate();
  try {
    for (int I = 0; I < Storage->ResourceCount; I++){
      TcxSchedulerStorageResourceItem *AResource = Storage->Resources->ResourceItems->Items[I];
      AResource->Visible = ((Item->Index == 0) || (AResource->ID == (Item->Index - 1)));
    }
  }
  __finally {
      Storage->EndUpdate();
      Scheduler->FullRefresh();
  }
}
//---------------------------------------------------------------------------
void __fastcall TResourceDemoMainForm::OpenSaveClick(TObject* Sender)
{
  if ((((TMenuItem*)Sender)->Tag == 0) && dlgOpen->Execute())
    Storage->LoadFromFile(dlgOpen->FileName);
  if ((((TMenuItem*)Sender)->Tag == 1) && SaveDialog->Execute())
      Storage->SaveToFile(SaveDialog->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::RentClick(TObject* Sender)
{
  if (((TComponent*)Sender)->Tag == 0) {
    TfrmRentCar* RentCar = new TfrmRentCar(NULL);
    try {
      int ACount = Storage->EventCount;
      if (RentCar->ShowModal() == mrOk) {
        SyncVisibility(RentCar->DateNavigator->Date, GetIndex(RentCar->lvCars));
        SelectEvents(ACount);
      }
    }
    __finally {
      delete RentCar;
    }
  }
  else {
    TfrmCancelReservation* frmCancel = new TfrmCancelReservation(NULL);
    try {
      if (frmCancel->ShowModal() == mrOk)
        SyncVisibility(frmCancel->Date, frmCancel->Index);
    }
    __finally {
      delete frmCancel;
    }
  FillTaskGrid();
 }
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::SchedulerLayoutChanged(TObject* Sender)
{
  IsChanged = true;
  FillTaskGrid();
  IsChanged = false;
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::SchedulerDateNavigatorCustomDrawContent(TObject* Sender,
    TcxCanvas* ACanvas, TcxSchedulerDateNavigatorMonthContentViewInfo* AViewInfo, bool &ADone)
{
  DrawDateNavigatorContent(ACanvas, AViewInfo, ADone);
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::SchedulerViewDayCustomDrawContainer(TObject* Sender,
    TcxCanvas* ACanvas, TcxSchedulerContainerCellViewInfo* AViewInfo, bool &ADone)
{
  if (!AViewInfo->Selected && miUseColorScheme->Checked && (AViewInfo->Resource != NULL))
    ACanvas->Brush->Color = (TColor)dxGetMiddleRGB((TColor)GetResourceColor(AViewInfo->Resource), clBlack, 80);
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::SchedulerCustomDrawContent(TObject* Sender, TcxCanvas *ACanvas,
    TcxSchedulerContentCellViewInfo* AViewInfo, bool &ADone)
{
  TcxViewParams AParams;
  if (miUseColorScheme->Checked && (AViewInfo->Resource != NULL)) {
    AParams = AViewInfo->TimeLineParams;
    AParams.Color = (TColor)dxGetMiddleRGB((TColor)GetResourceColor(AViewInfo->Resource), clBlack, 80);
    AViewInfo->TimeLineParams = AParams;
  }
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::SchedulerStylesGetContentStyle(TObject* Sender,
    TcxSchedulerStorageResourceItem* AResource, const TDateTime ADateTime, TcxStyle* &AStyle)
{
  if (miUseColorScheme->Checked && (AResource != NULL)) {
    AStyle = cxContentStyle;
    AStyle->Color = (TColor)ContentColors[AResource->ID];
  }
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::miUseColorSchemeClick(TObject* Sender)
{
  miUseColorScheme->Checked = !miUseColorScheme->Checked;
  if (miUseColorScheme->Checked)
    Scheduler->DateNavigator->Styles->Selection = cxSelectionStyle;
  else
    Scheduler->DateNavigator->Styles->Selection = NULL;
  Scheduler->LayoutChanged();
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::SchedulerEventPopupMenuPopup(
  TcxSchedulerEventPopupMenu *Sender, TPopupMenu *ABuiltInMenu, bool &AHandled)
{
  if ((Sender->Event != NULL) && Sender->Event->IsRecurring())
    AHandled = true;
  else
    Sender->GetMenuItem(epmiDelete, -1)->Caption = "Cancel reservation";
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::SchedulerContentPopupMenuPopup(
  TcxSchedulerContentPopupMenu *Sender, TPopupMenu *ABuiltInMenu, bool &AHandled)
{
  TMenuItem *AItem;
  ABuiltInMenu->Images = imgGlyph;
  AItem = NewItem("Rent a car", scNone, False, True, RentClick, 0, "");
  AItem->ImageIndex = 0;
  ABuiltInMenu->Items->Insert(0, AItem);
  AItem = NewItem("Cancel reservation", scNone, False, True, RentClick, 0, "");
  AItem->ImageIndex = 1;
  AItem->Tag = 1;
  ABuiltInMenu->Items->Insert(1, AItem);
  ABuiltInMenu->Items->Insert(2, NewLine());
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::SchedulerBeforeDeleting(TcxCustomScheduler *Sender,
    TcxSchedulerControlEvent *AEvent, bool &Allow)
{
  Allow = !AEvent->IsRecurring();
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::SchedulerSelectionChanged(TObject* Sender)
{
  IsChanged = true;
  FillTaskGrid();
  IsChanged = false;
}
//---------------------------------------------------------------------------

void __fastcall TResourceDemoMainForm::TaskGridSelectCell(TObject* Sender,
  int ACol, int ARow, bool &CanSelect)
{
  int I;
  TcxSchedulerEvent *AEvent;
  CanSelect = ARow > 0;
  if (!IsChanged) {
    if ((ARow > 0) && (ARow <= TaskEvents->Count)) {
      AEvent = TaskEvents->Items[ARow - 1];
      SyncVisibility((int)AEvent->Start, AEvent->GetResourceItem()->ID);
      Scheduler->UnselectEvents();
      for (I = 0; I < Scheduler->VisibleEventCount; I++)
        if (Scheduler->VisibleEvents[I]->Source == AEvent)
          Scheduler->SelectEvent(Scheduler->VisibleEvents[I], TShiftState()<<ssShift);
    }
  }
}
//---------------------------------------------------------------------------



