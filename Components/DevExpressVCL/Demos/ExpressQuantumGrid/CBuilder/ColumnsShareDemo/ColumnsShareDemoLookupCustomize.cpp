//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ColumnsShareDemoLookupCustomize.h"
#include "ColumnsShareDemoMain.h"
#include "cxDBLookupComboBox.hpp"
#include "ColumnsShareDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "cxListBox"
#pragma link "cxGraphics"
#pragma link "cxLabel"
#pragma link "cxLookAndFeels"
#pragma link "cxPC"
#pragma resource "*.dfm"
TColumnsShareDemoLookupCustomizeForm *ColumnsShareDemoLookupCustomizeForm;
//---------------------------------------------------------------------------
__fastcall TColumnsShareDemoLookupCustomizeForm::TColumnsShareDemoLookupCustomizeForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TColumnsShareDemoLookupCustomizeForm::CancelEditing()
{
  TcxCustomGridTableController* AController = ((TcxCustomGridTableController*)ColumnsShareDemoMainForm->
    Grid->FocusedView->Controller);
  if (AController->IsEditing)
    AController->EditingController->HideEdit(false);
}

void __fastcall TColumnsShareDemoLookupCustomizeForm::CustomizeListBoxes()
{
  lbUnlinkedColumns->Clear();
  lbListColumns->Clear();
  for(int i = 0; i < ColumnsShareDemoMainDM->tblUsers->FieldCount; i++)
    lbUnlinkedColumns->Items->Add(ColumnsShareDemoMainDM->tblUsers->Fields->Fields[i]->FieldName);
   TcxLookupDBGridColumns* Columns = ColumnsShareDemoMainForm->eriLookupComboUsers->Properties->ListColumns;
   int AIndex;
   for(int i = 0; i < Columns->Count; i++) {
     AIndex = lbUnlinkedColumns->Items->IndexOf(Columns->Items[i]->FieldName);
     if (AIndex != -1) {
       lbUnlinkedColumns->ItemIndex = AIndex;
       lbMoveItem(lbUnlinkedColumns, lbListColumns);
     }
    }
  if (lbListColumns->Items->Count != 0) lbListColumns->ItemIndex = 0;
  if (lbUnlinkedColumns->Items->Count != 0) lbUnlinkedColumns->ItemIndex = 0;
}

void __fastcall TColumnsShareDemoLookupCustomizeForm::CustomizeParams()
{
  TcxLookupComboBoxProperties* cbProperties = ColumnsShareDemoMainForm->eriLookupComboUsers->Properties;
  chbDropDownAutoSize->Checked = cbProperties->DropDownAutoSize;
  chbImmediateDropDown->Checked = cbProperties->ImmediateDropDown;
  chbIncrementalFilltering->Checked = cbProperties->IncrementalFiltering;
  cbDropDownListStyle->ItemIndex = (int)cbProperties->DropDownListStyle;
  seDropDownRows->Value = cbProperties->DropDownRows;
  seListFieldIndex->Value = cbProperties->ListFieldIndex;
  chbHeaders->Checked = cbProperties->ListOptions->ShowHeader;
}

void __fastcall TColumnsShareDemoLookupCustomizeForm::lbMoveItem(TcxListBox* ASourceListBox, TcxListBox* ADestinationListBox)
{
  ADestinationListBox->Items->Add(ASourceListBox->Items->Strings[ASourceListBox->ItemIndex]);
  lbDeleteSelection(ASourceListBox);
  if(ADestinationListBox->Items->Count == 1)
    ADestinationListBox->ItemIndex = 0;
}

void __fastcall TColumnsShareDemoLookupCustomizeForm::lbDeleteSelection(TcxListBox* AListBox)
{
  int AIndex = AListBox->ItemIndex;
  if(AIndex == AListBox->Items->Count - 1)
     AIndex--;
  AListBox->Items->Delete(AListBox->ItemIndex);
  if(AListBox->Items->Count)
    AListBox->ItemIndex = AIndex;
}

void __fastcall TColumnsShareDemoLookupCustomizeForm::btnAddClick(
      TObject *Sender)
{
  CancelEditing();
  FEditProperty->BeginUpdate();
  try{
    FEditProperty->ListColumns->Add()->FieldName = lbUnlinkedColumns->Items->Strings[lbUnlinkedColumns->ItemIndex];
  }
  __finally{
    FEditProperty->EndUpdate(true);
    ColumnsShareDemoMainForm->Grid->FocusedView->LayoutChanged(true);
  };
  lbMoveItem(lbUnlinkedColumns, lbListColumns);
  if (lbUnlinkedColumns->Items->Count == 0)
    btnAdd->Enabled = false;
  btnDelete->Enabled = true;
  seListFieldIndex->Properties->MaxValue = lbListColumns->Items->Count - 1;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoLookupCustomizeForm::btnDeleteClick(
      TObject *Sender)
{
  CancelEditing();
  FEditProperty->BeginUpdate();
  try{
    FEditProperty->ListColumns->Items[lbListColumns->ItemIndex]->Free();
  }
  __finally{
    FEditProperty->EndUpdate(true);
    ColumnsShareDemoMainForm->Grid->FocusedView->LayoutChanged(true);
  }
  lbMoveItem(lbListColumns, lbUnlinkedColumns);
  if(lbListColumns->Items->Count == 0)
    btnDelete->Enabled = false;
  btnAdd->Enabled = true;
  seListFieldIndex->Properties->MaxValue = lbListColumns->Items->Count - 1;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoLookupCustomizeForm::FormCreate(
      TObject *Sender)
{
  FEditProperty = ColumnsShareDemoMainForm->eriLookupComboUsers->Properties;
  CustomizeListBoxes();
  CustomizeParams();
  TcxCustomGridTableController* AController =
    (TcxCustomGridTableController*)ColumnsShareDemoMainForm->Grid->FocusedView->Controller;
  if(AController->IsEditing)
    AController->EditingController->HideEdit(false);
  tsLookupProperties->Enabled = true;
  btnAdd->Enabled = lbUnlinkedColumns->Items->Count != 0;
  btnDelete->Enabled = lbListColumns->Items->Count != 0;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoLookupCustomizeForm::btnCloseClick(
      TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoLookupCustomizeForm::lbUnlinkedColumnsDblClick(
      TObject *Sender)
{
  CancelEditing();
  FEditProperty->BeginUpdate();
  try {
    FEditProperty->ListColumns->Add()->FieldName = lbUnlinkedColumns->Items->Strings[lbUnlinkedColumns->ItemIndex];
  }
  __finally {
    FEditProperty->EndUpdate(true);
  }
  lbMoveItem(lbUnlinkedColumns, lbListColumns);
  if(lbUnlinkedColumns->Items->Count == 0)
    btnAdd->Enabled = false;
  btnDelete->Enabled = true;
  seListFieldIndex->Properties->MaxValue = lbListColumns->Items->Count - 1;
}
//---------------------------------------------------------------------------


void __fastcall TColumnsShareDemoLookupCustomizeForm::lbUnlinkedColumnsKeyPress(
      TObject *Sender, char &Key)
{
  if (Key == 13) btnAddClick(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoLookupCustomizeForm::lbListColumnsKeyPress(
      TObject *Sender, char &Key)
{
  if (Key == 13) btnDeleteClick(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoLookupCustomizeForm::chbDropDownAutoSizeClick(
      TObject *Sender)
{
  CancelEditing();
  FEditProperty->DropDownAutoSize = chbDropDownAutoSize->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoLookupCustomizeForm::chbImmediateDropDownClick(
      TObject *Sender)
{
  CancelEditing();
  FEditProperty->ImmediateDropDown = chbImmediateDropDown->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoLookupCustomizeForm::chbIncrementalFillteringClick(
      TObject *Sender)
{
  CancelEditing();
  FEditProperty->IncrementalFiltering = chbIncrementalFilltering->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoLookupCustomizeForm::chbHeadersClick(
      TObject *Sender)
{
  CancelEditing();
  FEditProperty->ListOptions->ShowHeader = chbHeaders->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoLookupCustomizeForm::cbDropDownListStylePropertiesChange(
      TObject *Sender)
{
  CancelEditing();
  FEditProperty->DropDownListStyle =
    (TcxEditDropDownListStyle)cbDropDownListStyle->ItemIndex;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoLookupCustomizeForm::seDropDownRowsPropertiesChange(
      TObject *Sender)
{
  CancelEditing();
  FEditProperty->DropDownRows = seDropDownRows->Value;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoLookupCustomizeForm::seListFieldIndexPropertiesChange(
      TObject *Sender)
{
  CancelEditing();
  FEditProperty->ListFieldIndex = seListFieldIndex->Value;
}
//---------------------------------------------------------------------------

