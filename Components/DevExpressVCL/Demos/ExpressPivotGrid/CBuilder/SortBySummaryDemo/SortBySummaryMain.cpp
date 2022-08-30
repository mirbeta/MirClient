//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SortBySummaryMain.h"
//---------------------------------------------------------------------------
#include "DemoBasicDM.h"
#pragma package(smart_init)
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxCustomPivotGrid"
#pragma link "cxDBPivotGrid"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma resource "*.dfm"
TfrmSortBySummary *frmSortBySummary;
//---------------------------------------------------------------------------
__fastcall TfrmSortBySummary::TfrmSortBySummary(TComponent* Owner)
        : TfrmDemoBasicMain(Owner)
{
}

void __fastcall TfrmSortBySummary::FormCreate(TObject* Sender)
{
  TfrmDemoBasicMain::FormCreate(Sender); 
  SortFieldChanged(NULL);
  pgfCompanyName->SortBySummaryInfo->Conditions->Add(pgfPaymentType, "AmEx");

  PivotGrid()->ApplyBestFit();
}

TcxCustomPivotGrid* __fastcall TfrmSortBySummary::PivotGrid()
{
  return DBPivotGrid;
}

TcxPivotGridField* __fastcall TfrmSortBySummary::CurrentField()
{
  TcxPivotGridField* AField = PivotGrid()->GetFieldByName(cbxSortField->Text);
  return AField;
}

//---------------------------------------------------------------------------

void __fastcall TfrmSortBySummary::SortFieldChanged(TObject *Sender)
{
  FLocked = true;
  // sync settings with selected field
  if (CurrentField()->SortBySummaryInfo->Field == NULL) {
    cbxSortByField->ItemIndex = 0;
  }
  else {
    cbxSortByField->Text = CurrentField()->SortBySummaryInfo->Field->Caption;
  }
  speTopCount->Value = CurrentField()->TopValueCount;
  speTopCount->Properties->MaxValue = CurrentField()->GroupValueList->Count;
  cbxTopValuesShowOthers->Checked = CurrentField()->TopValueShowOthers;
  FLocked = false;
}
//---------------------------------------------------------------------------

void __fastcall TfrmSortBySummary::SortByChanged(TObject *Sender)
{
  if (FLocked) return;
  CurrentField()->SortBySummaryInfo->Field = PivotGrid()->GetFieldByName(cbxSortByField->Text);
}
//---------------------------------------------------------------------------

void __fastcall TfrmSortBySummary::speTopCountPropertiesChange(
      TObject *Sender)
{
  if (FLocked) return;
  CurrentField()->TopValueCount = speTopCount->Value;
}
//---------------------------------------------------------------------------


void __fastcall TfrmSortBySummary::cbxTopValuesShowOthersPropertiesChange(
      TObject *Sender)
{
  if (FLocked) return;
  CurrentField()->TopValueShowOthers = cbxTopValuesShowOthers->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfrmSortBySummary::GetGroupHeaderStyle(
      TcxCustomPivotGrid *Sender, TcxPivotGridViewDataItem *AItem,
      TcxStyle *&AStyle)
{
  if (AItem->GroupItem->RecordIndex == cxPivotGridOthersRecordIndex) {
    AStyle = dmOrders->stBoldBlackFont;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmSortBySummary::rgSortOrderClick(TObject *Sender)
{
  DBPivotGrid->OptionsBehavior->SortBySummaryDefaultOrder = (TcxDataSortOrder)rgSortOrder->ItemIndex;
}
//---------------------------------------------------------------------------

void __fastcall TfrmSortBySummary::DBPivotGridLayoutChanged(TObject *Sender)
{
  SortFieldChanged(NULL);
}
//---------------------------------------------------------------------------

