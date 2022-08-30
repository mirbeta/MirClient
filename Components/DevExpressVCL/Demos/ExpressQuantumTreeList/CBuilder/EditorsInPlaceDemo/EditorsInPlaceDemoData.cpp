//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsInPlaceDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxTL"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TEditorsInPlaceDemoDataDM *EditorsInPlaceDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TEditorsInPlaceDemoDataDM::TEditorsInPlaceDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoDataDM::mdOrdersCalcFields(TDataSet *DataSet)
{
  mdCustomers->Open();
  mdOrdersCustomerEmail->AsString = mdCustomers->Lookup("ID",
    mdOrdersCustomerID->Value, "email");
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoDataDM::DataModuleCreate(TObject *Sender)
{
  mdCustomers->LoadFromBinaryFile("..\\..\\Data\\Customers.dat");
  mdOrders->LoadFromBinaryFile("..\\..\\Data\\Orders.dat");
  mdCustomers->Open();
  mdOrders->Open();
}
//---------------------------------------------------------------------------

