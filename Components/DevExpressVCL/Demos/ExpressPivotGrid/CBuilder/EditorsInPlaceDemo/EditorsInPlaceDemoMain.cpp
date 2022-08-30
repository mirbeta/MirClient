//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsInPlaceDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxCustomPivotGrid"
#pragma link "cxDBPivotGrid"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeels"
#pragma link "cxProgressBar"
#pragma link "cxStyles"
#pragma link "DemoBasicMain"
#pragma resource "*.dfm"
TfrmEditorsInPlace *frmEditorsInPlace;
//---------------------------------------------------------------------------
__fastcall TfrmEditorsInPlace::TfrmEditorsInPlace(TComponent* Owner)
	: TfrmDemoBasicMain(Owner)
{
}
//---------------------------------------------------------------------------
TcxCustomPivotGrid* __fastcall TfrmEditorsInPlace::PivotGrid()
{
  return DBPivotGrid;
}

//---------------------------------------------------------------------------
