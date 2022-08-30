//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ColumnsSimpleDemoCities.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
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
#pragma link "cxLookAndFeelPainters"
#pragma link "cxStyles"
#pragma link "cxDataStorage"
#pragma resource "*.dfm"
TColumnsSimpleDemoCitiesForm *ColumnsSimpleDemoCitiesForm;
//---------------------------------------------------------------------------
__fastcall TColumnsSimpleDemoCitiesForm::TColumnsSimpleDemoCitiesForm(TComponent* Owner)
  : TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TColumnsSimpleDemoCitiesForm::tvCitiesKeyDown(
      TObject *Sender, WORD &Key, TShiftState Shift)
{
  if ((Key == VK_RETURN) && (!tvCities->Controller->IsEditing))
    ModalResult = mrOk;
}

//---------------------------------------------------------------------------
Variant __fastcall TColumnsSimpleDemoCitiesForm::GetValue()
{
  return(tvCities->Controller->FocusedRecord->Values[0]);
}

//---------------------------------------------------------------------------
void __fastcall TColumnsSimpleDemoCitiesForm::btnAddClick(TObject *Sender)
{
  tvCities->DataController->Insert();
  tvCities->DataController->Edit();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoCitiesForm::btnDeleteClick(
      TObject *Sender)
{
  tvCities->Controller->DeleteSelection();
}
//---------------------------------------------------------------------------
