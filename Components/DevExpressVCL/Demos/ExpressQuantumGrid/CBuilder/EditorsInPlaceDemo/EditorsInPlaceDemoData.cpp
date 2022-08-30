//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsInPlaceDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxGridCardView"
#pragma resource "*.dfm"
TEditorsInPlaceDemoDataDM *EditorsInPlaceDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TEditorsInPlaceDemoDataDM::TEditorsInPlaceDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TEditorsInPlaceDemoDataDM::DataModuleCreate(TObject *Sender)
{
   String APath = ExtractFilePath(Application->ExeName) + "..\\..\\Data\\";
   tblCities->LoadFromFile(APath + "Cities.xml");
   tblCustomers->LoadFromFile(APath + "Customers.xml");
   tblOrders->LoadFromFile(APath + "Orders.xml");
}

