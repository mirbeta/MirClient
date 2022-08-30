//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RealtorWorldResearch.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "RealtorWorldBaseFrame"
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxCustomData"
#pragma link "cxCustomPivotGrid"
#pragma link "cxDBPivotGrid"
#pragma link "cxEdit"
#pragma link "cxGroupBox"
#pragma link "cxImage"
#pragma link "cxSplitter"
#pragma link "cxStyles"
#pragma link "dxGDIPlusClasses"
#pragma link "cxDBData"
#pragma link "cxGrid"
#pragma link "cxGridChartView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBChartView"
#pragma link "cxGridLevel"
#pragma resource "*.dfm"
TfrmResearch *frmResearch;
//---------------------------------------------------------------------------
__fastcall TfrmResearch::TfrmResearch(TComponent* Owner)
	: TfrmBase(Owner)
{
  InitializeFrame();
  cxgChart->BeginUpdate();
  try
  {
	cvChart->DataGroups[0]->ActiveValue = "For Sale";
	cvChart->DataGroups[1]->ActiveValue = "Not Seasonally Adjus";
	cvChart->ActiveDataLevel = 2;
  }
  __finally
  {
	cxgChart->EndUpdate();
  };
}
//---------------------------------------------------------------------------
void __fastcall TfrmResearch::cxSplitter1BeforeClose(TObject *Sender, bool &AllowClose)

{
  AllowClose = false;
}
//---------------------------------------------------------------------------
void __fastcall TfrmResearch::InitializeFrame()
{
  pgResearch->BeginUpdate();
  __try
  {
	for (int I = 0; I < pgResearch->FieldCount; I++)
	{
	  pgResearch->Fields[I]->ExpandAll();
    };
	//pgfRegion->Filter->Values->Add(Null);
  }
  __finally
  {
	pgResearch->EndUpdate();
  };
  pgfRegion->ApplyBestFit();
}
