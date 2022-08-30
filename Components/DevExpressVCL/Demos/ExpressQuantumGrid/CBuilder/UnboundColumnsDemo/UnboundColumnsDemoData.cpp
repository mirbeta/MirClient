//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "UnboundColumnsDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxDBEditRepository"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
#pragma link "cxGridCardView"
#pragma resource "*.dfm"
TUnboundColumnsDemoDataDM *UnboundColumnsDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TUnboundColumnsDemoDataDM::TUnboundColumnsDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TUnboundColumnsDemoDataDM::DataModuleCreate(TObject *Sender)
{
    tblCustomers->LoadFromFile(ExtractFilePath(Application->ExeName) + "..\\..\\Data\\Customer.xml");
};
//---------------------------------------------------------------------------
