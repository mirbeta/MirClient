//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SummaryFooterDemoEditSummary.h"
#include "SummaryFooterDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxImageComboBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxTextEdit"
#pragma link "cxListBox"
#pragma link "cxGraphics"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TSummaryFooterDemoEditSummaryForm *SummaryFooterDemoEditSummaryForm;
//---------------------------------------------------------------------------
__fastcall TSummaryFooterDemoEditSummaryForm::TSummaryFooterDemoEditSummaryForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------;
void __fastcall TSummaryFooterDemoEditSummaryForm::FormCreate(TObject *Sender)
{
	//
};
//---------------------------------------------------------------------------
void __fastcall TSummaryFooterDemoEditSummaryForm::btnAddClick(TObject *Sender)
{
  TcxGridDBTableSummaryItem* AFooterSummaryItem = ((TcxGridDBTableSummaryItem*)GetFooterSummaryItems()->Add());
  try {
    try {
      AFooterSummaryItem->BeginUpdate();
      if(cbFooterSummaryColumn->ItemIndex != -1);
        AFooterSummaryItem->Column =
          (TcxGridDBColumn*)GetFocusedView()->VisibleColumns[cbFooterSummaryColumn->ItemIndex];
      TcxGridDBColumn* AColumn = NULL;
      if(cbCalculatedColumn->ItemIndex != -1)
        AColumn =
          (TcxGridDBColumn*)GetFocusedView()->VisibleColumns[cbCalculatedColumn->ItemIndex];
      if(AColumn != NULL)
        AFooterSummaryItem->FieldName = AColumn->DataBinding->FieldName;
      AFooterSummaryItem->Kind = (TcxSummaryKind)((int)cbSummaryKind->EditValue);
      cbFooterSummaryColumn->ItemIndex = 0;
      cbSummaryKind->EditValue = 0;
    }
    __finally {
      AFooterSummaryItem->EndUpdate();
    }
  }
  catch(...) {
    ::MessageDlg("Invalid Data", mtError, TMsgDlgButtons()<<mbOK, 0);
    GetFooterSummaryItems()->Items[GetFooterSummaryItems()->Count - 1]->Free();
  }
  UpdateSummaryList(false);
}
//---------------------------------------------------------------------------

inline TcxDataFooterSummaryItems*  TSummaryFooterDemoEditSummaryForm::GetFooterSummaryItems()
{
  return SummaryFooterDemoMainForm->Grid->FocusedView->
    DataController->Summary->FooterSummaryItems;
}
//---------------------------------------------------------------------------

inline TcxGridDBTableView* TSummaryFooterDemoEditSummaryForm::GetFocusedView()
{
  return ((TcxGridDBTableView*)SummaryFooterDemoMainForm->Grid->FocusedView);  
}
//---------------------------------------------------------------------------

void TSummaryFooterDemoEditSummaryForm::UpdateSummaryList(bool AFirstItemActive)
{
  lbSummaries->Clear();
  for(int i = 0; i <= GetFooterSummaryItems()->Count - 1; i++)
    if(((TcxGridTableSummaryItem*)GetFooterSummaryItems()->Items[i])->Column != NULL)
      lbSummaries->Items->Add(
        ((TcxGridTableSummaryItem*)GetFooterSummaryItems()->Items[i])->Column->Caption);
    else
      lbSummaries->Items->Add("Unassigned");
  if(AFirstItemActive)
    lbSummaries->ItemIndex = 0;
  else
    lbSummaries->ItemIndex = lbSummaries->Items->Count - 1;
  btnDelete->Enabled = lbSummaries->Items->Count > 0;
}
//---------------------------------------------------------------------------

void __fastcall TSummaryFooterDemoEditSummaryForm::FormShow(
      TObject *Sender)
{
  cbCalculatedColumn->Properties->Items->Clear();
  cbFooterSummaryColumn->Properties->Items->Clear();
  for(int i = 0; i <= GetFocusedView()->VisibleColumnCount - 1; i++) {
    cbCalculatedColumn->Properties->Items->Add(GetFocusedView()->VisibleColumns[i]->Caption);
    cbFooterSummaryColumn->Properties->Items->Add(GetFocusedView()->VisibleColumns[i]->Caption);
  }
  if(GetFocusedView()->VisibleColumnCount > 0)
    cbFooterSummaryColumn->ItemIndex = 0;
  else
    btnAdd->Enabled = false;
  UpdateSummaryList(true);
}
//---------------------------------------------------------------------------

void TSummaryFooterDemoEditSummaryForm::UpdateSummaryKinds(TField* AField)
{
  TcxFieldTypes NumberFieldTypes;
  NumberFieldTypes = NumberFieldTypes<<ftSmallint<<ftInteger<<ftWord<<ftFloat<<
    ftCurrency<<ftBCD<<ftDate<<ftTime<<TFieldType::ftDateTime<<ftAutoInc;
  TcxFieldTypes TimeFieldTypes;
  TimeFieldTypes = TimeFieldTypes<<ftDate<<ftTime<<TFieldType::ftDateTime;
  cbSummaryKind->Properties->Items->Clear();
  if(AField != NULL) {
    AddSummaryKind(skNone);
    AddSummaryKind(skCount);
    if(NumberFieldTypes.Contains(AField->DataType)) {
      AddSummaryKind(skMax);
      AddSummaryKind(skMin);
      if(!TimeFieldTypes.Contains(AField->DataType)) {
        AddSummaryKind(skSum);
        AddSummaryKind(skAverage);
      }
    }
  }
  if(cbSummaryKind->Properties->Items->Count == 0)
    cbSummaryKind->Enabled = False;
  else {
    cbSummaryKind->Enabled = True;
    cbSummaryKind->EditValue = 0;
  }
}
//---------------------------------------------------------------------------

void TSummaryFooterDemoEditSummaryForm::AddSummaryKind(TcxSummaryKind AKind)
{
  TcxImageComboBoxItem* AItem = (TcxImageComboBoxItem*)cbSummaryKind->Properties->Items->Add();
  switch(AKind) {
    case skNone:
      SetItemProperties("None", -1, 0, AItem);
      break;
    case skSum:
      SetItemProperties("Sum", 6, 1, AItem);
      break;
    case skMin:
      SetItemProperties("Min", 5, 2, AItem);
      break;
    case skMax:
      SetItemProperties("Max", 4, 3, AItem);
      break;
    case skCount:
      SetItemProperties("Count", 3, 4, AItem);
      break;
    case skAverage:
      SetItemProperties("Average", 7, 5, AItem);
      break;
  };
}
//---------------------------------------------------------------------------

void TSummaryFooterDemoEditSummaryForm::SetItemProperties(AnsiString ADescription,
        int AImageIndex, int AValue, TcxImageComboBoxItem* AItem)
{
  AItem->Description = ADescription;
  AItem->ImageIndex = AImageIndex;
  AItem->Value = AValue;
}
//---------------------------------------------------------------------------


void __fastcall TSummaryFooterDemoEditSummaryForm::cbCalculatedColumnPropertiesChange(
      TObject *Sender)
{
  if(cbCalculatedColumn->ItemIndex != -1) {
    TcxGridDBColumn* AColumn =
      (TcxGridDBColumn*)GetFocusedView()->VisibleColumns[cbCalculatedColumn->ItemIndex];
    UpdateSummaryKinds(AColumn->DataBinding->Field);
  }
}
//---------------------------------------------------------------------------

void __fastcall TSummaryFooterDemoEditSummaryForm::cbFooterSummaryColumnPropertiesChange(
      TObject &Sender)
{
  cbCalculatedColumn->ItemIndex = cbFooterSummaryColumn->ItemIndex;
}
//---------------------------------------------------------------------------

void __fastcall TSummaryFooterDemoEditSummaryForm::btnDeleteClick(
      TObject *Sender)
{
  TcxGridDBTableSummaryItem* AItem =
    (TcxGridDBTableSummaryItem*)GetFooterSummaryItems()->Items[lbSummaries->ItemIndex];
  if(AItem->FieldName != "")
    for(int i = 0; i < GetFocusedView()->VisibleColumnCount - 1; i++) {
      TcxGridDBColumn* AColumn = (TcxGridDBColumn*)GetFocusedView()->VisibleColumns[i];
      if(AColumn->DataBinding->FieldName == AItem->FieldName) {
        cbCalculatedColumn->ItemIndex = AColumn->VisibleIndex;
        break;
      }
    }
  if(AItem->Column != NULL)
    cbFooterSummaryColumn->ItemIndex = AItem->Column->VisibleIndex;
  cbSummaryKind->EditValue = (int)AItem->Kind;;
  GetFooterSummaryItems()->Items[lbSummaries->ItemIndex]->Free();
  UpdateSummaryList(true);
}
//---------------------------------------------------------------------------

