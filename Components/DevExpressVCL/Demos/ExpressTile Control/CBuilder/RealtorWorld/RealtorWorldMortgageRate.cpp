//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RealtorWorldMortgageRate.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "RealtorWorldBaseFrame"
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxCalendar"
#pragma link "cxFilter"
#pragma link "cxGrid"
#pragma link "cxGridChartView"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBChartView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxGroupBox"
#pragma link "cxSplitter"
#pragma link "cxStyles"
#pragma link "cxNavigator"
#pragma link "dxDateRanges"
#pragma resource "*.dfm"
TfrmMortgageRate *frmMortgageRate;
//---------------------------------------------------------------------------
__fastcall TfrmMortgageRate::TfrmMortgageRate(TComponent* Owner)
	: TfrmBase(Owner)
{
  cxGrid1DBTableView1->DataController->Groups->FullExpand();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMortgageRate::cxSplitter1BeforeClose(TObject *Sender, bool &AllowClose)

{
  AllowClose = false;
}
//---------------------------------------------------------------------------
