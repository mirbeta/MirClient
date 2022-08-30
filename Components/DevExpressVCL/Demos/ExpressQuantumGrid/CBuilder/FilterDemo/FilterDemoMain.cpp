//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "FilterDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxGridCardView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
        : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  cdsCompanies->Open();
  cdsCountries->Open();
  cdsProducts->Open();

  GenerateData();
  UpdateMenuValues();

  TableViewProduct->DataBinding->AddToFilter(NULL, foLike, "Express*", "", true);
  TableViewOrderDate->DataBinding->AddToFilter(NULL, foThisMonth, VarAsType(0, varNull), "", true);
  TableView->DataController->Filter->Active = true;
}
//---------------------------------------------------------------------------

void TfrmMain::GenerateData()
{
  const int OrdersPerCustomer = 25;
  const int DayRange = 3 * 365 / 2;
  const int MaxQuantity = 50;

  TDateTime ADate = Date();
  TableView->DataController->RecordCount = cdsCompanies->RecordCount * OrdersPerCustomer;

  TableView->BeginUpdate();
  try {
    cdsCompanies->First();
    while (!cdsCompanies->Eof)
    {
      AnsiString ACompanyName = cdsCompaniesCOMPANYNAME->AsString;
      int ACountryID = cdsCompaniesCOUNTRYID->AsInteger;

      for (int i = 0; i < OrdersPerCustomer; i++)
      {
        int ARecordIndex = (cdsCompanies->RecNo - 1) * OrdersPerCustomer + i;
        TcxCustomGridRow* ARow = TableView->ViewData->Rows[ARecordIndex];

        ARow->Values[TableViewCompany->Index] = ACompanyName;
        ARow->Values[TableViewCountry->Index] = ACountryID;
        ARow->Values[TableViewOrderDate->Index] =
          ADate + (int)(random(DayRange) - DayRange / 2) + (double)random(3599) / 3600;
        ARow->Values[TableViewProduct->Index] = GetProductName(1 + random(cdsProducts->RecordCount));
        ARow->Values[TableViewQuantity->Index] = 1 + random(MaxQuantity);
      }
      
      cdsCompanies->Next();
    }
    TableViewOrderDate->SortOrder = soAscending;
  }
  __finally {
    TableView->EndUpdate();
  }

  TableView->BeginUpdate();
  try {
    for(int i = 0; i < TableView->ViewData->RowCount; i++)
      TableView->ViewData->Rows[i]->Values[TableViewOrderID->Index] = 1 + i;
  }
  __finally {
    TableView->EndUpdate();
  }
}
//---------------------------------------------------------------------------

AnsiString TfrmMain::GetProductName(int AID)
{
  if (cdsProducts->Locate("ID", AID, TLocateOptions()))
    return cdsProductsName->AsString;
  else
    return "";
}
//---------------------------------------------------------------------------

void TfrmMain::UpdateMenuValues()
{
  miColumnFilterPopupMultiSelect->Checked = TableView->Filtering->ColumnPopup->MultiSelect;
  miApplyMultiSelectChanges->Enabled = TableView->Filtering->ColumnPopup->MultiSelect;
  if (TableView->Filtering->ColumnPopup->ApplyMultiSelectChanges == fpacImmediately)
    miApplyMultiSelectChangesImmediately->Checked = true;
  else
    miApplyMultiSelectChangesOnButtonClick->Checked = true;

  miColumnFilterPopupFilteredList->Checked = TableView->Filtering->ColumnFilteredItemsList;

  miFilterRow->Checked = TableView->FilterRow->Visible;
  miApplyFilterRowChanges->Enabled = TableView->FilterRow->Visible;
  if (TableView->FilterRow->ApplyChanges == fracOnCellExit)
    miApplyFilterRowChangesOnCellExit->Checked = true;
  else
    miApplyFilterRowChangesImmediately->Checked = true;

  for (int i = 0; i < miDateTimeFilters->Count; i++)
    miDateTimeFilters->Items[i]->Checked = TableView->DateTimeHandling->Filters.Contains((TcxGridDateTimeFilter)i);
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miColumnFilterPopupMultiSelectClick(TObject *Sender)
{
  TableView->Filtering->ColumnPopup->MultiSelect = !TableView->Filtering->ColumnPopup->MultiSelect;
  UpdateMenuValues();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miApplyMultiSelectChangesClick(TObject *Sender)
{
  TableView->Filtering->ColumnPopup->ApplyMultiSelectChanges =
    (TcxGridItemFilterPopupApplyChangesMode)((TMenuItem*)Sender)->MenuIndex;
  UpdateMenuValues();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miColumnFilterPopupFilteredListClick(
      TObject *Sender)
{
  TableView->Filtering->ColumnFilteredItemsList = !TableView->Filtering->ColumnFilteredItemsList;
  UpdateMenuValues();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miFilterRowClick(TObject *Sender)
{
  TableView->FilterRow->Visible = !TableView->FilterRow->Visible;
  UpdateMenuValues();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miApplyFilterRowChangesClick(TObject *Sender)
{
  TableView->FilterRow->ApplyChanges =
    (TcxGridFilterRowApplyChangesMode)((TMenuItem*)Sender)->MenuIndex;
  UpdateMenuValues();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miDateTimeFilterClick(TObject *Sender)
{
  TcxGridDateTimeFilter AFilter = (TcxGridDateTimeFilter)((TMenuItem*)Sender)->MenuIndex;
  TcxGridDateTimeFilters AFilters = TableView->DateTimeHandling->Filters;
  if (AFilters.Contains(AFilter))
    AFilters >> AFilter;
  else
    AFilters << AFilter;
  TableView->DateTimeHandling->Filters = AFilters;
  UpdateMenuValues();
}

void __fastcall TfrmMain::miAllowOperatorCustomizationClick(TObject *Sender)
{
  TableView->FilterRow->OperatorCustomization = !TableView->FilterRow->OperatorCustomization;
  UpdateMenuValues();
}
//---------------------------------------------------------------------------


