//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "InvoiceReportDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseForm"
#pragma link "ReportPreviewUnit"
#pragma link "ReportDesignerBaseForm"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxSplitter"
#pragma link "cxTextEdit"
#pragma link "dxSpreadSheetCore"
#pragma link "dxSpreadSheetReportDesigner"
#pragma link "cxFilterControl"
#pragma link "dxmdaset"
#pragma link "ReportPreviewUnit"
#pragma link "dxSpreadSheetTypes"
#pragma resource "*.dfm"

TfrmInvoiceReport *frmInvoiceReport;
//---------------------------------------------------------------------------
__fastcall TfrmInvoiceReport::TfrmInvoiceReport(TComponent* Owner)
	: TfrmReportDesignerBase(Owner)
{
}
//---------------------------------------------------------------------------


void TfrmInvoiceReport::Initialize()
{
  ReportDesigner->LoadFromFile("..\\..\\Data\\InvoiceTemplate.xlsx");
  LoadDataset(mdsInvoice, "..\\..\\Data\\Invoices.mds");
  TfrmReportDesignerBase::Initialize();
}

void __fastcall TfrmInvoiceReport::ReportDesignerAfterBuild(TObject *Sender)
{
  for (int i = 0; i < Preview->ssResult->SheetCount; i++) {
    TdxSpreadSheetTableView *ASheet = (TdxSpreadSheetTableView*)(Preview->ssResult->Sheets[i]);
	TdxSpreadSheetCell *ACell = ASheet->Cells[15][1];
	if ((ACell != NULL) & (ACell->DataType == cdtInteger)) {
	  ASheet->Caption = "Order ID " + ACell->AsString;
	}
  }
}
