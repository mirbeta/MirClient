//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SampleDockingListBox.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TSampleDockingListBoxFrame *SampleDockingListBoxFrame;
//---------------------------------------------------------------------------
__fastcall TSampleDockingListBoxFrame::TSampleDockingListBoxFrame(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TSampleDockingListBoxFrame::actAddExecute(TObject *Sender)
{
  ListBox->Items->Add(Edit->Text);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingListBoxFrame::actDeleteExecute(
      TObject *Sender)
{
  ListBox->Items->Delete(ListBox->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingListBoxFrame::actClearExecute(
      TObject *Sender)
{
  ListBox->Items->Clear();
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingListBoxFrame::actAddUpdate(TObject *Sender)
{
  ((TCustomAction*)Sender)->Enabled = Edit->Text != "";
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingListBoxFrame::actDeleteUpdate(
      TObject *Sender)
{
  ((TCustomAction*)Sender)->Enabled = ListBox->ItemIndex != -1;
}
//---------------------------------------------------------------------------

