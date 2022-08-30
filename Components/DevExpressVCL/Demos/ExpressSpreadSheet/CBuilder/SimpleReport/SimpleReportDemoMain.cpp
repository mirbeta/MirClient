//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SimpleReportDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseForm"
#pragma link "ReportDesignerBaseForm"
#pragma link "ReportPreviewUnit"
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
#pragma link "ReportDesignerBaseForm"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TfrmSimpleReport *frmSimpleReport;

__fastcall TfrmSimpleReport::TfrmSimpleReport(TComponent* Owner)
	: TfrmReportDesignerBase(Owner)
{
}

//---------------------------------------------------------------------------

void TfrmSimpleReport::Initialize()
{
  LoadDataset(mdsOrders, "..\\..\\Data\\OrderDetails.mds");
  ReportDesigner->LoadFromFile("..\\..\\Data\\SimpleReportTemplate.xlsx");
  TfrmReportDesignerBase::Initialize();
}

