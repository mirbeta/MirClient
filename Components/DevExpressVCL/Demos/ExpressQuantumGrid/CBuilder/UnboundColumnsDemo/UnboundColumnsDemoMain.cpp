//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "UnboundColumnsDemoMain.h"
#include "UnboundColumnsDemoData.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxGridCardView"
#pragma link "cxGridDBCardView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "cxLookAndFeels"
#pragma link "cxGridBandedTableView"
#pragma link "cxGridDBBandedTableView"
#pragma link "cxBlobEdit"
#pragma link "cxCalendar"
#pragma link "cxDataStorage"
#pragma resource "*.dfm"
TUnboundColumnsDemoMainForm *UnboundColumnsDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TUnboundColumnsDemoMainForm::TUnboundColumnsDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
  GenerateUnboundData();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundColumnsDemoMainForm::BandedTableViewStylesGetContentStyle(
      TcxCustomGridTableView *Sender, TcxCustomGridRecord *ARecord,
      TcxCustomGridTableItem *AItem, TcxStyle *&AStyle)
{
  if (dynamic_cast<TcxGridDataRow*>(ARecord) != 0 && !ARecord->Selected &&
    !VarIsNull(ARecord->Values[BandedTableViewSelected->Index]) &&
    (int)ARecord->Values[BandedTableViewSelected->Index])
      AStyle = styChecked;
}
//---------------------------------------------------------------------------

void TUnboundColumnsDemoMainForm::GenerateUnboundData()
{
  randomize();
  BandedTableView->BeginUpdate();
  try{
    for (int I = 0; I < BandedTableView->DataController->RecordCount; I++){
      BandedTableView->DataController->SetValue(
        I, BandedTableViewSelected->Index, div(random(100),2).rem == 1);
      BandedTableView->DataController->SetValue(
        I, BandedTableViewSupportRequests->Index, 1 + random(20));
      BandedTableView->DataController->SetValue(I,
        BandedTableViewLastSupportRequest->Index,
          EncodeDate(2000 + random(4), 1 + random(11), 1 + random(27)));
      BandedTableView->DataController->SetValue(
        I, BandedTableViewComments->Index, "Put your comments here...");
    }
  }
  __finally{
    BandedTableView->EndUpdate();
  }
}
//---------------------------------------------------------------------------


