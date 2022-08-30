//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "HorizontalReportDemoMain.h"
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
#pragma resource "*.dfm"
TfrmHorizontalReport *frmHorizontalReport;
//---------------------------------------------------------------------------
__fastcall TfrmHorizontalReport::TfrmHorizontalReport(TComponent* Owner)
	: TfrmReportDesignerBase(Owner)
{
}
//---------------------------------------------------------------------------


void TfrmHorizontalReport::Initialize()
{
  LoadDataset(mdsEmployees, "..\\..\\Data\\Employees.mds");
  ReportDesigner->LoadFromFile("..\\..\\Data\\HorizontalReportTemplate.xlsx");
  ReportDesigner->Options->Orientation = roHorizontal;
  TfrmReportDesignerBase::Initialize();
}
