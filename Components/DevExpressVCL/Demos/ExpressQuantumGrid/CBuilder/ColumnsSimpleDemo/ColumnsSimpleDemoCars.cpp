//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ColumnsSimpleDemoCars.h"
#include "ColumnsSimpleDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridBandedTableView"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBBandedTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxBlobEdit"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImage"
#pragma link "cxImageComboBox"
#pragma link "cxMemo"
#pragma link "cxRadioGroup"
#pragma resource "*.dfm"
TColumnSimpleDemoCarsForm *ColumnSimpleDemoCarsForm;
//---------------------------------------------------------------------------
__fastcall TColumnSimpleDemoCarsForm::TColumnSimpleDemoCarsForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TColumnSimpleDemoCarsForm::bvCarsTopRecordIndexChanged(
      TObject *Sender)
{
  bvCars->Controller->FocusedRecordIndex = bvCars->Controller->TopRecordIndex;
}
//---------------------------------------------------------------------------

