//--------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DataSetsMain.h"
//---------------------------------------------------------------------------
#include "cxGraphics.hpp"

#pragma package(smart_init)
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxCustomPivotGrid"
#pragma link "cxDBPivotGrid"
#pragma link "cxClasses"
#pragma link "cxCustomData"
#pragma link "cxStyles"
#pragma link "dxmdaset"
#pragma link "dxSparkline"
#pragma resource "*.dfm"
TfrmDataSets *frmDataSets;

//---------------------------------------------------------------------------
__fastcall TfrmDataSets::TfrmDataSets(TComponent* Owner): TfrmDemoBasicMain(Owner)
{
//
}
//---------------------------------------------------------------------------
void __fastcall TfrmDataSets::FormCreate(TObject *Sender)
{
  TfrmDemoBasicMain::FormCreate(Sender);
  PivotGrid()->ViewData->FocusedCell = cxNullPoint;
  SparklineTemplate = new TdxSparklineProperties(this);
  SparklineTemplate->Assign(pgfPaymentAmount->Properties);
  PivotGrid()->FullRefresh();
}
//---------------------------------------------------------------------------
void __fastcall TfrmDataSets::FormDestroy(TObject *Sender)
{
  SparklineTemplate->Free();
}
//---------------------------------------------------------------------------
void __fastcall TfrmDataSets::pgfPaymentAmountCalculateCustomSummary(
  TcxPivotGridField* Sender, TcxPivotGridCrossCellSummary* ASummary)
{
  if (SparklineTemplate != NULL)
  {
	TcxPivotGridRecords* ARecords = ASummary->Owner->Records;
    SparklineTemplate->DataController->RecordCount = ARecords->Count;
	for (int i = 0; i < ARecords->Count; i++)
	{
	  SparklineTemplate->DataController->Values[i][SparklineTemplate->Series->Items[0]->DataIndex] =
		PivotGrid()->DataController->Values[ARecords->Items[i]][pgfPaymentAmount->Index];
	}
    ASummary->Custom = SparklineTemplate->DataValue;
  } 
}
//---------------------------------------------------------------------------
TcxCustomPivotGrid* __fastcall TfrmDataSets::PivotGrid()
{
  return DBPivotGrid;
}
//---------------------------------------------------------------------------
void __fastcall TfrmDataSets::GridActiveTabChanged(TcxCustomGrid *Sender,
	  TcxGridLevel *ALevel)
{
  if ((ALevel->Tag != 2) && PivotGridSummaryDataSet->SynchronizeData)
	PivotGridDataSetDataChanged(NULL);

  PivotGridDrillDownDataSet->SynchronizeData = ALevel->Tag == 2;
  PivotGridSummaryDataSet->SynchronizeData = !PivotGridDrillDownDataSet->SynchronizeData;
}
//---------------------------------------------------------------------------
void __fastcall TfrmDataSets::PivotGridDataSetCreateField(TcxPivotGridCustomDataSet *Sender,
  TcxPivotGridField *APivotGridField, TField *ADataSetField)
{
  ADataSetField->Size = ((TcxDBPivotGridField*)APivotGridField)->DataBinding->DBField->Size;
}
//---------------------------------------------------------------------------
void __fastcall TfrmDataSets::PivotGridDataSetDataChanged(
	  TObject *Sender)
{
  if (HandleAllocated())
	PostMessage(Handle, CM_UPDATEGRIDVIEW, Grid->ActiveLevel->Tag, 0);
}
//---------------------------------------------------------------------------
void TfrmDataSets::CreateFooterSummaryCell(TcxGridColumn *AGridColumn)
{
  if (AGridColumn != NULL) {
	AGridColumn->Summary->FooterKind = skSum;
    AGridColumn->Summary->FooterFormat = ",.00";
  }
}
//---------------------------------------------------------------------------
TcxGridColumn* TfrmDataSets::ColumnByCaption(TcxGridTableView *ATableView, AnsiString ACaption)
{
  int I;
  for (I = 0; I < ATableView->ColumnCount; I++)
  {
	if (ATableView->Columns[I]->Caption == ACaption)
	{
	  return ATableView->Columns[I];
	}
  }
  return NULL;
}
//---------------------------------------------------------------------------
void TfrmDataSets::CreateFooterSummary(TcxGridTableView *ATableView)
{
  int I;
  for (I = 0; I < PivotGrid()->FieldCount; I++)
  {
	if ((PivotGrid()->Fields[I]->Area == faData) /*&& !PivotGrid()->Fields[I]->DataBinding->ValueTypeClass->IsString*/)
 	  CreateFooterSummaryCell(ColumnByCaption(ATableView, PivotGrid()->Fields[I]->Caption));
  }
}
//---------------------------------------------------------------------------
void TfrmDataSets::UpdateSummaryChartView()
{
  int I;
  SummaryChartView->BeginUpdate();
  try
  {
	SummaryChartView->ClearSeries();
	SummaryChartView->ClearDataGroups();
	for (I = 0; I < PivotGridSummaryDataSet->FieldCount; I++)
	{
	  switch (TcxPivotGridFieldArea(PivotGridSummaryDataSet->Fields->Fields[I]->Tag - 1))
	  {
		case faColumn:
                case faRow: 
                  SummaryChartView->CreateDataGroup()->DataBinding->FieldName = PivotGridSummaryDataSet->Fields->Fields[I]->FieldName; break;
		case faData: SummaryChartView->CreateSeries()->DataBinding->FieldName = PivotGridSummaryDataSet->Fields->Fields[I]->FieldName;	break;
	  }
	};
  }
  __finally
  {
	SummaryChartView->EndUpdate();
  }
}
//---------------------------------------------------------------------------
void TfrmDataSets::UpdateSummaryTableView()
{
  SummaryTableView->BeginUpdate();
  try
  {
	SummaryTableView->ClearItems();
	SummaryTableView->DataController->CreateAllItems();
	CreateFooterSummary(SummaryTableView);
  }
  __finally
  {
	SummaryTableView->EndUpdate();
  }

  if (HandleAllocated())
	PostMessage(Handle, CM_APPLYBESTFIT, Integer(SummaryTableView), 0);
}
//---------------------------------------------------------------------------
void TfrmDataSets::UpdateDrillDownTableView()
{
  DrillDownTableView->BeginUpdate();
  try
  {
	DrillDownTableView->ClearItems();
	DrillDownTableView->DataController->CreateAllItems();
	CreateFooterSummary(DrillDownTableView);
  }
  __finally
  {
	DrillDownTableView->EndUpdate();
  }

  if (HandleAllocated())
	PostMessage(Handle, CM_APPLYBESTFIT, Integer(DrillDownTableView), 0);
}
//---------------------------------------------------------------------------
void __fastcall TfrmDataSets::CMApplyBestFit(Messages::TMessage &Message)
{
  if (((TObject*)(Message.WParam))->InheritsFrom(__classid(TcxGridDBTableView)))
	((TcxGridDBTableView*)Message.WParam)->ApplyBestFit();
}
//---------------------------------------------------------------------------
void __fastcall TfrmDataSets::CMUpdateGridView(Messages::TMessage &Message)
{
  switch (Message.WParam)
  {
	case 0: UpdateSummaryTableView(); break;
	case 1: UpdateSummaryChartView(); break;
	case 2: UpdateDrillDownTableView(); break;
  }
}
//---------------------------------------------------------------------------
