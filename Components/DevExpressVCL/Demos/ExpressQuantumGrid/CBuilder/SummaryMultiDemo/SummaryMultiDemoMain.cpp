//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "SummaryMultiDemoMain.h"
#include "SummaryMultiDemoData.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxGridCustomPopupMenu"
#pragma link "cxGridPopupMenu"
#pragma link "cxLookAndFeels"
#pragma link "cxBlobEdit"
#pragma link "cxCalc"
#pragma link "cxCalendar"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxImageComboBox"
#pragma link "cxSpinEdit"
#pragma link "cxGridCardView"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TSummaryMultiDemoMainForm *SummaryMultiDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TSummaryMultiDemoMainForm::TSummaryMultiDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void TSummaryMultiDemoMainForm::UpdateMenu()
{
  miSelectedRecordsOnly->Checked = tvOrders->DataController->Summary->Options.Contains(soSelectedRecords);
  miIgnoreNullValues->Checked = tvOrders->DataController->Summary->Options.Contains(soNullIgnore);
  miUseOnAfterSummaryEvent->Checked = tvOrders->DataController->Summary->OnAfterSummary != NULL;
  miGroupFooters->Items[(int)tvOrders->OptionsView->GroupFooters]->Checked = true;
  miMultipleSummariesInFooter->Checked = tvOrders->OptionsView->FooterMultiSummaries;
  miMultipleSummariesInGroupFooters->Checked = tvOrders->OptionsView->GroupFooterMultiSummaries;
  miMultiSelect->Checked = tvOrders->OptionsSelection->MultiSelect;
}
//---------------------------------------------------------------------------

void __fastcall TSummaryMultiDemoMainForm::FormShow(TObject *Sender)
{
  tvOrders->DataController->Groups->ChangeExpanding(0, true, false);
  tvOrders->DataController->GotoFirst();
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSummaryMultiDemoMainForm::miSelectedRecordsOnlyClick(
      TObject *Sender)
{
  TcxDataSummary* ASummary = tvOrders->DataController->Summary;
  if (ASummary->Options.Contains(soSelectedRecords))
    ASummary->Options = ASummary->Options >> soSelectedRecords;
  else
    ASummary->Options = ASummary->Options << soSelectedRecords;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSummaryMultiDemoMainForm::miIgnoreNullValuesClick(
      TObject *Sender)
{
  TcxDataSummary* ASummary = tvOrders->DataController->Summary;
  if (ASummary->Options.Contains(soNullIgnore))
    ASummary->Options = ASummary->Options >> soNullIgnore;
  else
    ASummary->Options = ASummary->Options << soNullIgnore;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSummaryMultiDemoMainForm::miMultiSelectClick(
      TObject *Sender)
{
  tvOrders->OptionsSelection->MultiSelect = !tvOrders->OptionsSelection->MultiSelect;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSummaryMultiDemoMainForm::miUseOnAfterSummaryEventClick(TObject *Sender)
{
  TcxDataSummary* ASummary = tvOrders->DataController->Summary;
  ASummary->BeginUpdate();
  try {
    if (ASummary->OnAfterSummary == NULL)
      ASummary->OnAfterSummary = tvOrdersDataControllerSummaryAfterSummary;
    else
      ASummary->OnAfterSummary = NULL;
  }
  __finally {
    ASummary->EndUpdate();
  }
  UpdateMenu();
}

//---------------------------------------------------------------------------

Variant TSummaryMultiDemoMainForm::CalculateFooterSummaryValue(int AIndex, TcxSummaryKind AKind)
{
  TcxDataControllerGroups* AGroups  = tvOrders->DataController->Groups;
  Variant Result;
  Variant AValue;

  if (AKind == skCount)
    Result = AGroups->ChildCount[-1];
  else {
    Result = VarAsType(Result, varNull);
    for (int i = 0; i < AGroups->ChildCount[-1]; i++) {
      AValue = tvOrders->DataController->Summary->GroupSummaryValues[AGroups->ChildDataGroupIndex[-1][i]][AIndex];
      if (!VarIsNull(AValue))
        if (VarIsNull(Result))
          Result = AValue;
        else
          switch (AKind) {
            case skMin:
              if (AValue < Result)
                Result = AValue;
              break;
            case skMax:
              if (AValue > Result)
                Result = AValue;
              break;
            case skSum:
            case skAverage:
              Result += AValue;
              break;
          };
    }
    if ((AKind == skAverage) && !VarIsNull(Result) && (AGroups->ChildCount[-1] != 0))
      Result /= AGroups->ChildCount[-1];
  }
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TSummaryMultiDemoMainForm::tvOrdersDataControllerSummaryAfterSummary(
  TcxDataSummary* ASender)
{  // calculate footer summaries using the group footer summary values, not actual data values
  if (tvOrders->GroupedColumnCount == 0) return;
  TcxDataFooterSummaryItems* AFooterSummaryItems = ASender->FooterSummaryItems;
  TcxDataGroupSummaryItems* AGroupSummaryItems = ASender->GroupSummaryItems[0];
  for (int i = 0; i < AFooterSummaryItems->Count; i++) {
    TcxDataSummaryItem* AGroupSummaryItem = AGroupSummaryItems->GetDataItem(
      ((TcxGridColumn*)AFooterSummaryItems->Items[i]->ItemLink)->Index, spFooter, false, skNone);
    if (AGroupSummaryItem != NULL) {
      Variant AValue = CalculateFooterSummaryValue(AGroupSummaryItem->Index, AFooterSummaryItems->Items[i]->Kind);
      if (!VarIsNull(AValue))
        ASender->FooterSummaryValues[i] = VarAsType(AValue, VarType(ASender->FooterSummaryValues[i]));
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TSummaryMultiDemoMainForm::tvOrdersDataControllerSummaryDefaultGroupSummaryItemsSummary(
      TcxDataSummaryItems *ASender, const TcxSummaryEventArguments &Arguments,
      TcxSummaryEventOutArguments &OutArguments)
{  // calculate the number of orders with the PaymentAmount > $300,000
  TcxDataSummaryItem* AItem = Arguments.SummaryItem;
  if ((AItem->ItemLink == tvOrdersProductID) &&
    (AItem->Kind == skCount) && (AItem->Position == spGroup)) {
    Variant AValue = tvOrders->DataController->Values[Arguments.RecordIndex][tvOrdersPaymentAmount->Index];
    if ((!VarIsNull(AValue)) && (VarAsType(AValue, varInteger) <= 300000))
      OutArguments.CountValue--;
  }
}
//---------------------------------------------------------------------------

void __fastcall TSummaryMultiDemoMainForm::miGroupFootersClick(
      TObject *Sender)
{
  tvOrders->OptionsView->GroupFooters = (TcxGridGroupFootersMode)((TMenuItem*)Sender)->MenuIndex;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSummaryMultiDemoMainForm::miMultipleSummariesInFooterClick(
      TObject *Sender)
{
  tvOrders->OptionsView->FooterMultiSummaries = !tvOrders->OptionsView->FooterMultiSummaries;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSummaryMultiDemoMainForm::miMultipleSummariesInGroupFootersClick(
      TObject *Sender)
{
  tvOrders->OptionsView->GroupFooterMultiSummaries = !tvOrders->OptionsView->GroupFooterMultiSummaries;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSummaryMultiDemoMainForm::tvOrdersStylesGetFooterSummaryStyle(
      TcxGridTableView *AView, TcxCustomGridRow *ARow,
      TcxGridColumn *AColumn, int AFooterGroupLevel,
      TcxDataSummaryItem *ASummaryItem, TcxStyle *&AStyle)
{
  switch (ASummaryItem->Kind) {
    case skMin:
      AStyle = SummaryMultiDemoDataDM->styleRed;
      break;
    case skMax:
      AStyle = SummaryMultiDemoDataDM->styleBlue;
      break;
  }
}
//---------------------------------------------------------------------------

void __fastcall TSummaryMultiDemoMainForm::tvOrdersStylesGetGroupSummaryStyle(
      TcxGridTableView *Sender, TcxGridGroupRow *ARow,
      TcxGridColumn *AColumn, TcxDataSummaryItem *ASummaryItem,
      TcxStyle *&AStyle)
{
  if (ASummaryItem != NULL)
    tvOrdersStylesGetFooterSummaryStyle(Sender, ARow, AColumn, ARow->Level, ASummaryItem, AStyle);
}
//---------------------------------------------------------------------------

