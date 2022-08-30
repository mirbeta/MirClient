//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "..\cxDemosBCB.inc"
#include "CancelReservationUnit.h"
#include "ResourceMainUnit.h"
#include "cxSchedulerStorage.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxLabel"
#pragma link "cxListBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxSchedulerStorage"
#pragma resource "*.dfm"

int __fastcall EventsCompare(TcxSchedulerEvent* AEvent1, TcxSchedulerEvent* AEvent2)
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
__fastcall TfrmCancelReservation::TfrmCancelReservation(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TfrmCancelReservation::CheckButtonEnabled()
{
  btnCancelReserv->Enabled = (lbxEvents->SelCount > 0);
}
//---------------------------------------------------------------------------

void __fastcall TfrmCancelReservation::FillCustomersList()
{
  int I;
  TStringList* AList;
  AList = new TStringList;
  try{
    for (I = 0; I < Storage()->EventCount; I++)
	  if (Storage()->Events[I]->EventType == Cxschedulerstorage::etNone)
        AList->Add(Storage()->Events[I]->Caption);
    AList->Sort();
    lbxCustomers->Items->BeginUpdate();
    try{
      lbxCustomers->Items->Clear();
      for (I = 0; I < AList->Count; I++)
        if ((I == 0) || (AList->Strings[I - 1] != AList->Strings[I]))
          lbxCustomers->Items->Add(AList->Strings[I]);
    }
    __finally{
      lbxCustomers->Items->EndUpdate();
      if (lbxCustomers->Count > 0)
        lbxCustomers->ItemIndex = 0;
      FillEventsList(lbxCustomers->ItemIndex);
    }
  }
  __finally{
      delete AList;
  }
  CheckButtonEnabled();
}
//---------------------------------------------------------------------------

void __fastcall TfrmCancelReservation::FillEventsList(int ACustomerIndex)
{
  TcxSchedulerEvent* AEvent;
  TStrings* AItems = lbxEvents->Items;
  AItems->BeginUpdate();
  TcxSchedulerEventList* AList = new TcxSchedulerEventList;
  try{
    AItems->Clear();
    if (ACustomerIndex >= 0)
      for (int I = 0; I < Storage()->EventCount; I++) {
        AEvent = Storage()->Events[I];
		if ((AEvent->Caption == lbxCustomers->Items->Strings[ACustomerIndex]) && (AEvent->EventType == Cxschedulerstorage::etNone))
          AList->Add(AEvent);
      }
      AList->Sort(EventsCompare);
      for (int I = 0; I < AList->Count; I++) {
        AEvent = AList->Items[I];
        lbxEvents->AddItem(DateTimeToStr(AEvent->Start) + " - " + DateTimeToStr(AEvent->Finish), AEvent);
      }
  }
  __finally{
    delete AList;
    AItems->EndUpdate();
    if (lbxEvents->Count > 0)
      lbxEvents->ItemIndex = 0;
  }
  CheckButtonEnabled();
}
//---------------------------------------------------------------------------

TcxCustomSchedulerStorage* __fastcall TfrmCancelReservation::Storage()
{
  return ResourceDemoMainForm->Storage;
}
//---------------------------------------------------------------------------

void __fastcall TfrmCancelReservation::FormCreate(TObject* Sender)
{
  Index = -1;
  Date = (TDateTime)NullDate;
  FillCustomersList();
}
//---------------------------------------------------------------------------

void __fastcall TfrmCancelReservation::lbxCustomersClick(TObject* Sender)
{
  FillEventsList(lbxCustomers->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TfrmCancelReservation::lbxEventsClick(TObject* Sender)
{
  CheckButtonEnabled();
}
//---------------------------------------------------------------------------

void __fastcall TfrmCancelReservation::btnCancelReservClick(TObject* Sender)
{
  Storage()->BeginUpdate();
  try{
    for (int I = 0; I < lbxEvents->Count; I++)
      if (lbxEvents->Selected[I]){
        TcxSchedulerEvent *AEvent = (TcxSchedulerEvent*)lbxEvents->Items->Objects[I];
        Index = AEvent->ResourceID;
        Date = AEvent->Start;
        AEvent->Delete();
      }
  }
  __finally{
    Storage()->EndUpdate();
  }
}
//---------------------------------------------------------------------------










