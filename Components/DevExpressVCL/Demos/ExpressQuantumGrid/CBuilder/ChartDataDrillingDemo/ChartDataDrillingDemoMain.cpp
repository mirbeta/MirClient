//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ChartDataDrillingDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridChartView"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBChartView"
#pragma link "cxGridDBTableView"
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
  String APath = ExtractFilePath(Application->ExeName) + "..\\..\\Data\\";
  tblCustomers->LoadFromFile(APath + "Customers.xml");
  tblProducts->LoadFromFile(APath + "Products.xml");
  tblOrders->LoadFromFile(APath + "Orders.xml");
};
//---------------------------------------------------------------------------
TcxGridDBColumn* TfrmMain::GetColumnByChartItem(TcxGridChartItem* AChartItem)
{
  TcxGridDBChartItemDataBinding* ADataBinding = (TcxGridDBChartItemDataBinding*)AChartItem->DataBinding;
  return TableView->GetColumnByFieldName(ADataBinding->FieldName);
}
//---------------------------------------------------------------------------

void TfrmMain::ShowTableActiveGroup()
{
  int ADataGroupIndex;

  // expand group rows so that the group with the active chart data is made visible
  ActiveDataGroups.Length = ChartView->ActiveDataLevel;
  TableView->BeginUpdate();
  try {
    TableView->DataController->Groups->FullCollapse();
    ADataGroupIndex = -1;
    for (int i = 0; i < ChartView->ActiveDataLevel; i++) {
      ADataGroupIndex = TableView->DataController->Groups->GetDataGroupIndexByGroupValue(
        ADataGroupIndex, ChartView->VisibleDataGroups[i]->ActiveValue);
      ActiveDataGroups[i] = ADataGroupIndex;

      int AGroupRowIndex = TableView->DataController->DataControllerInfo->DataGroups->Items[ADataGroupIndex]->RowIndex;
      TableView->DataController->Groups->ChangeExpanding(AGroupRowIndex, true, false);
      TableView->Controller->FocusedRowIndex = AGroupRowIndex;
    }
  }
  __finally {
    TableView->EndUpdate();
  }
  // select rows with the data used for the active chart
  TableView->BeginUpdate();
  try {
    if (ADataGroupIndex == -1)
      TableView->Controller->SelectAll();
    else {
      TableView->Controller->ClearSelection();
      for (int i = TableView->Controller->FocusedRowIndex + 1; i < TableView->ViewData->RowCount; i++) {
        TcxCustomGridRow* ARow = TableView->ViewData->Rows[i];
        if (ARow->Level == ChartView->ActiveDataLevel) {
          ARow->Focused = true;
          ARow->Selected = true;
        }
        else
          break;
      }
    }
  }  
  __finally {
    TableView->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void TfrmMain::SynchronizeTableWithChart()
{
  UpdateTableGroupingAndColumnVisibility();
  ShowTableActiveGroup();
}
//---------------------------------------------------------------------------

void TfrmMain::UpdateTableGroupingAndColumnVisibility()
{
  TableView->BeginUpdate();
  try {
    TableView->Controller->ClearGrouping();
    for (int i = 0; i < ChartView->VisibleDataGroupCount; i++)
      GetColumnByChartItem(ChartView->VisibleDataGroups[i])->GroupIndex = i;

    for (int i = 0; i < ChartView->DataGroupCount; i++)
      GetColumnByChartItem(ChartView->DataGroups[i])->Visible = ChartView->DataGroups[i]->Visible;
    for (int i = 0; i < ChartView->SeriesCount; i++)
      GetColumnByChartItem(ChartView->Series[i])->Visible = ChartView->Series[i]->Visible;
  }    
  __finally {
    TableView->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::GridActiveTabChanged(TcxCustomGrid *Sender,
      TcxGridLevel *ALevel)
{
  if (ALevel == GridLevelTable)
    SynchronizeTableWithChart();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::TableViewStylesGetGroupStyle(
      TcxGridTableView *Sender, TcxCustomGridRecord *ARecord, int ALevel,
      TcxStyle *&AStyle)
{
  if (ARecord == NULL) return;
  int ADataGroupIndex = TableView->DataController->Groups->DataGroupIndexByRowIndex[ARecord->Index];
  for (int i = 0; i < ActiveDataGroups.Length; i++)
    if (ADataGroupIndex == ActiveDataGroups[i]) {
      AStyle = styleActiveGroup;
      break;
    };
}
//---------------------------------------------------------------------------

