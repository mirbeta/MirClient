//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MultipleTotalsMain.h"
//---------------------------------------------------------------------------
#include "DemoBasicDM.h"
#pragma package(smart_init)
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxCustomPivotGrid"
#pragma link "cxDBPivotGrid"
#pragma resource "*.dfm"
TfrmMultipleTotals *frmMultipleTotals;
//---------------------------------------------------------------------------
__fastcall TfrmMultipleTotals::TfrmMultipleTotals(TComponent* Owner)
        : TfrmDemoBasicMain(Owner)
{
}

void __fastcall TfrmMultipleTotals::FormCreate(TObject* Sender)
{
  TfrmDemoBasicMain::FormCreate(Sender); 
  PivotGrid()->ApplyBestFit();
}

TcxCustomPivotGrid* __fastcall TfrmMultipleTotals::PivotGrid()
{
  return DBPivotGrid;
}

//---------------------------------------------------------------------------

double VarToDoubleEx(Variant AValue)
{
  if (!VarIsNull(AValue)) {
    return ((double)AValue);
  }
  else {
    return 0;
  }
}

void __fastcall TfrmMultipleTotals::CalculateCustomSummary(
      TcxPivotGridField *Sender, TcxPivotGridCrossCellSummary *ASummary)
{
  TcxPivotGridGroupItem *ARow;
  TcxPivotGridGroupItem *AColumn;
  if (pgfCompanyName->Area == faRow) {
    ARow = ASummary->Owner->Row;
    AColumn = ASummary->Owner->Column;
  }
  else {
    ARow = ASummary->Owner->Column;
    AColumn = ASummary->Owner->Row;
  }
  if (AColumn->Parent == NULL){
    ASummary->Custom = 100;
  }
  else{
    double ASum = VarToDoubleEx(ASummary->Sum);
    double AGrandTotalValue = VarToDoubleEx(ARow->GetCellByCrossItem(
      AColumn->Parent)->GetSummaryByField(Sender, stSum));
    if (AGrandTotalValue != 0) {
      ASummary->Custom = (ASum / AGrandTotalValue * 100);
    }
    else {
      ASummary->Custom = NULL;
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmMultipleTotals::PivotGridStylesGetGroupHeaderStyle(
      TcxCustomPivotGrid *Sender, TcxPivotGridViewDataItem *AItem,
      TcxStyle *&AStyle)
{
   if (AItem->IsTotal || !AItem->Expanded & (AItem->Field == pgfCompanyName)) {
     AStyle = dmOrders->stBoldBlueFont;
   }
}
//---------------------------------------------------------------------------

void __fastcall TfrmMultipleTotals::PivotGridStylesGetContentStyle(
      TcxCustomPivotGrid *Sender, TcxPivotGridDataCellViewInfo *ACell,
      TcxStyle *&AStyle)
{
  if (ACell->SummaryType == stCustom) {
    AStyle = dmOrders->stBoldRedFont;
  }
}
//---------------------------------------------------------------------------

