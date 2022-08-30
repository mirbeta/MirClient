//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsInPlaceDemoCities.h"
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
#pragma resource "*.dfm"
TEditorsInPlaceDemoCitiesForm *EditorsInPlaceDemoCitiesForm;
//---------------------------------------------------------------------------
__fastcall TEditorsInPlaceDemoCitiesForm::TEditorsInPlaceDemoCitiesForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCitiesForm::tvCitiesKeyDown(TObject *Sender, Word &Key,
    TShiftState Shift)
{
  if ((Key == VK_RETURN) && !tvCities->Controller->IsEditing)
    ModalResult = mrOk;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCitiesForm::btnAddClick(TObject *Sender)
{
  tvCities->DataController->Insert();
  tvCities->DataController->Edit();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCitiesForm::btnDeleteClick(TObject *Sender)
{
  tvCities->Controller->DeleteSelection();
}
//---------------------------------------------------------------------------

Variant TEditorsInPlaceDemoCitiesForm::GetValue()
{
  return tvCities->Controller->FocusedRecord->Values[0];
}
//---------------------------------------------------------------------------

