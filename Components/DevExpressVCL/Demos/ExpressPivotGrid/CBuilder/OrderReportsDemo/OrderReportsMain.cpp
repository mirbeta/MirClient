//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "OrderReportsMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxCustomPivotGrid"
#pragma link "cxDBPivotGrid"
#pragma resource "*.dfm"
TfrmOrderReport *frmOrderReport;
//---------------------------------------------------------------------------
__fastcall TfrmOrderReport::TfrmOrderReport(TComponent* Owner)
        : TfrmDemoBasicMain(Owner)
{
}

void __fastcall TfrmOrderReport::FormCreate(TObject* Sender)
{
  TfrmDemoBasicMain::FormCreate(Sender); 
  PivotGrid()->ApplyBestFit();
}

TcxCustomPivotGrid* __fastcall TfrmOrderReport::PivotGrid()
{
  return DBPivotGrid;
}

//---------------------------------------------------------------------------
void __fastcall TfrmOrderReport::pgfPaymentTypeGetGroupImageIndex(
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

