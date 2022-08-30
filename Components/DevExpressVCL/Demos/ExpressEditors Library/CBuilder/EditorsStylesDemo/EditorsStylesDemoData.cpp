//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "EditorsStylesDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TEditorsStylesDemoDataDM *EditorsStylesDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TEditorsStylesDemoDataDM::TEditorsStylesDemoDataDM(TComponent* Owner)
        : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TEditorsStylesDemoDataDM::tblUsersCalcFields(TDataSet *DataSet)
{
  tblUsersUserName->Value = tblUsersFNAME->Value + ' ' + tblUsersLNAME->Value;
}
//---------------------------------------------------------------------------

