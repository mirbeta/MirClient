//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MasterDetailReportDemoMain.h"
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
#pragma resource "*.dfm"
TfrmMasterDetail *frmMasterDetail; 
//---------------------------------------------------------------------------
__fastcall TfrmMasterDetail::TfrmMasterDetail(TComponent* Owner)
	: TfrmReportDesignerBase(Owner)
{
}
//---------------------------------------------------------------------------


void TfrmMasterDetail::Initialize()
{

  ReportDesigner->LoadFromFile("..\\..\\Data\\MasterDetailTemplate.xlsx");
  LoadDataset(mdsMaster, "..\\..\\Data\\Suppliers.mds");
  LoadDataset(mdsDetailLevel0, "..\\..\\Data\\Products.mds");
  LoadDataset(mdsDetailLevel1, "..\\..\\Data\\OrderReports.mds");
  TfrmReportDesignerBase::Initialize();
}

void __fastcall  TfrmMasterDetail::mdsDetailLevel1CalcFields(TDataSet *DataSet)
{
  mdsDetailLevel1->FieldByName("SubTotal")->Value =
    mdsDetailLevel1->FieldByName("Quantity")->Value * mdsDetailLevel1->FieldByName("UnitPrice")->Value;
}

void __fastcall TfrmMasterDetail::ReportDesignerAfterBuild(TObject *Sender)
{
  for (int i = 0; i < Preview->ssResult->SheetCount; i++) {
	TdxSpreadSheetTableView *ASheet = (TdxSpreadSheetTableView*)(Preview->ssResult->Sheets[i]);
	TdxSpreadSheetCell *ACell = ASheet->Cells[4][1];
	if ((ACell != NULL) & (ACell->AsString != "")) {
	  ASheet->Caption = ACell->AsString;
	}
  }
}
