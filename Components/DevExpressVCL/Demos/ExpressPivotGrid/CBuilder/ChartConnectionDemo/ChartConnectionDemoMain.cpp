//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ChartConnectionDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxCustomPivotGrid"
#pragma link "cxDBPivotGrid"
#pragma link "cxClasses"
#pragma link "cxCustomData"
#pragma link "cxGraphics"
#pragma link "cxStyles"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGrid"
#pragma link "cxGridChartView"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxPivotGridChartConnection"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxNavigator"
#pragma resource "*.dfm"
TfrmChartConnection *frmChartConnection;
//---------------------------------------------------------------------------
__fastcall TfrmChartConnection::TfrmChartConnection(TComponent* Owner)
        : TfrmDemoBasicMain(Owner)
{   
}

void __fastcall TfrmChartConnection::FormCreate(TObject* Sender)
{
  TfrmDemoBasicMain::FormCreate(Sender);  
  PivotGrid()->ApplyBestFit();
  InitializeDiagramList();
}

TcxCustomPivotGrid* __fastcall TfrmChartConnection::PivotGrid()
{
  return DBPivotGrid;
}

void __fastcall TfrmChartConnection::InitializeDiagramList()
{
  TcxGridChartDiagram *ADiagram;
  TMenuItem *AMenuItem;
  miChartType->Clear();
  for (int I = 0; I < cxGridChartView->DiagramCount; I++)
  {
	ADiagram = cxGridChartView->Diagrams[I];
	AMenuItem = new TMenuItem(miChartType);
	AMenuItem->AutoCheck = True;
	AMenuItem->RadioItem = True;
	AMenuItem->Checked = ADiagram == cxGridChartView->ActiveDiagram;
	AMenuItem->OnClick = miChartColumDiagramClick;
	AMenuItem->Caption = ADiagram->DisplayText;
	AMenuItem->Tag = I;
	miChartType->Add(AMenuItem);
  }
}

//---------------------------------------------------------------------------
void __fastcall TfrmChartConnection::pgfPaymentTypeGetGroupImageIndex(
	  TcxPivotGridField *Sender, const TcxPivotGridViewDataItem *AItem,
	  int &AImageIndex, TAlignment &AImageAlignHorz,
	  TcxAlignmentVert &AImageAlignVert)
{
  AnsiString Card = ((TcxPivotGridViewDataItem *)AItem)->Value;
  if (SameText(Card, "Cash")) AImageIndex = 0;
  else if (SameText(Card, "AmEx")) AImageIndex = 1;
	   else if (SameText(Card, "Master")) AImageIndex = 2;
			else if (SameText(Card, "Visa")) AImageIndex = 3;
}
//---------------------------------------------------------------------------
void __fastcall TfrmChartConnection::miVisibleCellsClick(TObject *Sender)
{
  cxPivotGridChartConnection->SourceData = (TcxPivotGridChartViewSourceData)((TMenuItem*)Sender)->Tag;
}
void __fastcall TfrmChartConnection::miSourceColumnClick(TObject *Sender)
{
  cxPivotGridChartConnection->SourceForCategories = TcxPivotGridChartViewSourceForCategories(((TMenuItem*)Sender)->Tag);

}
void __fastcall TfrmChartConnection::miChartColumDiagramClick(TObject *Sender)
{
  cxGridChartView->ActiveDiagram = cxGridChartView->Diagrams[((TMenuItem*)Sender)->Tag];
}

void __fastcall TfrmChartConnection::cxGridChartViewActiveDiagramChanged(TcxGridChartView *Sender,
	TcxGridChartDiagram *ADiagram)
{
	InitializeDiagramList();
}


